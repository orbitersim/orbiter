//
// MLOptPal.cpp
//
// Optimal palette generation
//
// Uses the Gervautz-Purgathofer octree method. For a detail explanation of this algorithm
// and code which uses it to display hi or true color bitmaps well on a 256-color display,
// see the August 1996 issue of Microsoft Systems Journal.
//
// Copyright (c) 1998-1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------
#include <windows.h>

#include <assert.h>
#include <ddraw.h>
#include "Debug.h"
#include "MLOptPal.h"

#define MAX_CHILDREN            8               // Octree node has 8 children (by definition)

// An OptimalPaletteNode is node in the octree representing the palette information
// so far.
//
// Each node maintains a weighted average of the color information under it so far.
//
class OptimalPaletteNode
{
public:
    BOOL m_bLeaf;
    LONG m_nTotalPixels;
    LONG m_nRed;
    LONG m_nGreen;
    LONG m_nBlue;
    
    OptimalPaletteNode *m_pChildren[MAX_CHILDREN];
    OptimalPaletteNode *m_pNext;
};

// OptimalPalette::OptimalPalette
//
// Create an empty tree. nSignificant bits sets the max color resolution by indicating
// how many bits of the source colors, at most, will be used to index into the tree.
// Colors that differ in only the last (8 - nSignificantBits) in all three components
// will be represented by the same OptimalPaletteNode.
//
OptimalPalette::OptimalPalette(
    int nSignificantBits)
{
    assert(nSignificantBits <= 8);

    m_pRoot = NULL;
    m_nSignificantBits = nSignificantBits;
    ZeroMemory(m_pReducibleList, sizeof(m_pReducibleList));
    m_nLeafNodes = 0;
}

OptimalPalette::~OptimalPalette()
{
    FreeTree(m_pRoot);
}

// OptimalPalette::Clear
//
// Free all tree memory, reset the reducible lists, and clear the leaf node count.
//
void OptimalPalette::Clear()
{
    FreeTree(m_pRoot);
    m_pRoot = NULL;
    ZeroMemory(m_pReducibleList, sizeof(m_pReducibleList));
    m_nLeafNodes = 0;
}

// OptimalPalette::AddPixel
//
// Add the pixel's color to the tree, and reduce the tree size to keep the number of 
// leaf nodes within the maximum size of the final 8-bit palette.
//
void OptimalPalette::AddPixel(
    PALETTEENTRY pal)
{
    int idxColor;

    BYTE bRed = pal.peRed;
    BYTE bGreen = pal.peGreen;
    BYTE bBlue = pal.peBlue;

    OptimalPaletteNode **ppNode = &m_pRoot;
    OptimalPaletteNode *pNode;

    // Walk the tree down to the correct leaf, filling in nodes as we go.
    //    
    for (idxColor = 0; idxColor < m_nSignificantBits; idxColor++)
    {
        // Create a new node at this level if needed. If this is not a leaf node 
        // then add to the level's reducible list.
        //
        if ((pNode = *ppNode) == NULL)
        {
            pNode = *ppNode = new OptimalPaletteNode;
    
            ZeroMemory(pNode, sizeof(OptimalPaletteNode));
            pNode->m_bLeaf = FALSE;

            pNode->m_pNext = m_pReducibleList[idxColor];
            m_pReducibleList[idxColor] = pNode;
        }

        // We have the node; pick the correct child based on the color bits.
        // 1 of 8 combinations is picked based on the R, G, and B bits at this 
        // level.
        //
        BYTE bIndex = 
            ((bRed >> 7) & 0x01) | ((bBlue >> 6) & 0x02) | ((bGreen >> 5) & 0x04);
        bRed <<= 1;
        bBlue <<= 1;
        bGreen <<= 1;

        ppNode = &pNode->m_pChildren[bIndex];
    }    

    // We've reached a leaf node, which we might have to create.
    //
    if ((pNode = *ppNode) == NULL)
    {
        pNode = *ppNode = new OptimalPaletteNode;

        ZeroMemory(pNode, sizeof(OptimalPaletteNode));
        pNode->m_bLeaf = TRUE;

        ++m_nLeafNodes;
    }

    pNode->m_nTotalPixels++;
    pNode->m_nRed += pal.peRed;
    pNode->m_nGreen += pal.peGreen;
    pNode->m_nBlue += pal.peBlue;

    // If we just added a leaf node, we may have caused the count to go over
    // the max (but only by one). If so, reduce by combining the most recently
    // added lowest level non-leaf node as the sum of its children.
    //
    if (m_nLeafNodes > MAX_PALETTE_ENTRIES)
    {
        for (idxColor = m_nSignificantBits - 1; idxColor >= 0; idxColor--)
        {
            if (m_pReducibleList[idxColor])
            {
                break;
            }
        }

        assert(idxColor != -1);

        // Pull this node off; it will become a leaf and thus no longer reducible
        //
        pNode = m_pReducibleList[idxColor];
        m_pReducibleList[idxColor] = pNode->m_pNext;

        pNode->m_bLeaf = TRUE;

        // Combine the children
        //
        for (int idxChild = 0; idxChild < MAX_CHILDREN; idxChild++)
        {
            OptimalPaletteNode *pChild = pNode->m_pChildren[idxChild];
            if (pChild == NULL)
            {
                continue;
            }
                    
            assert(pChild->m_bLeaf);

            pNode->m_nTotalPixels += pChild->m_nTotalPixels;
            pNode->m_nRed += pChild->m_nRed;
            pNode->m_nGreen += pChild->m_nGreen;
            pNode->m_nBlue += pChild->m_nBlue;   

            pNode->m_pChildren[idxChild] = NULL;
            delete pChild;

            -- m_nLeafNodes;
        }
    }

    assert(m_nLeafNodes <= MAX_PALETTE_ENTRIES);
}

// OptimalPalette::GetPalette
//
// Just call the recursive routine to fill in the palette entries.
//
void OptimalPalette::GeneratePalette(
    PALETTEENTRY pal[MAX_PALETTE_ENTRIES])
{
    UINT idx = 0;

    ZeroMemory(pal, sizeof(pal));
    GetPaletteRecursive(m_pRoot, pal, &idx);
}

// OptimalPalette::GetPaletteRecursive
//
// Recursively walk the octree and build the palette. We don't do this often
// so the overhead of recursion is ok. (Also the total size of the tree is
// bounded to be fairly small by the restriction of 256 leaf nodes).
//
// The palette index in the generated palette is saved in the leaf nodes. This
// allows us to build a translation from any given palette into the optimal
// palette.
//
void OptimalPalette::GetPaletteRecursive(
    OptimalPaletteNode *pNode,
    PALETTEENTRY pal[MAX_PALETTE_ENTRIES],
    UINT *pIdx)
{
    if (pNode->m_bLeaf)
    {
        assert((*pIdx) < MAX_PALETTE_ENTRIES);

        PALETTEENTRY *pPal = &pal[*pIdx];
        (*pIdx)++;

        assert(pNode->m_nTotalPixels);
        pPal->peRed =   (BYTE)(pNode->m_nRed / pNode->m_nTotalPixels);
        pPal->peGreen = (BYTE)(pNode->m_nGreen / pNode->m_nTotalPixels);
        pPal->peBlue =  (BYTE)(pNode->m_nBlue / pNode->m_nTotalPixels);
      
        return;
    }

    for (int idxChild = 0; idxChild < MAX_CHILDREN; idxChild++)
    {
        if (pNode->m_pChildren[idxChild])
        {
            GetPaletteRecursive(pNode->m_pChildren[idxChild], pal, pIdx);
        }
    }
}

// OptimalPalette::FreeTree
//
// Free all tree memory recursively.
//
void OptimalPalette::FreeTree(
    OptimalPaletteNode *pNode)
{
    if (pNode == NULL)
    {
        return;
    }

    for (int idxChild = 0; idxChild < MAX_CHILDREN; idxChild++)
    {
       FreeTree(pNode->m_pChildren[idxChild]);
    }

    delete pNode;
}

