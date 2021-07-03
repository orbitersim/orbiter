//
// MLOptPal.h
//
// Optimal palette generation
//
// Copyright (c) 1998-1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------

#ifndef _MLOptPal_
#define _MLOptPal_

#define MAX_PALETTE_ENTRIES     256             // Palette has up to 256 entries and
#define MAX_COLOR_COMP_BITS     8               // each component (R,G,B) has an 8 bit representation

class OptimalPaletteNode;
class OptimalPalette
{
public:
    OptimalPalette(int nSigificantBits);
    ~OptimalPalette();

    // Clear discards any color information and starts a fresh optimal palette.
    //
    void Clear();

    // AddPixel is used to add one pixel from the set of pixels to generate the 
    // palette for. It should be called once per pixel per bitmap, not once per
    // color entry per bitmap, as the palette is weighted for the number of times
    // a color occurs.
    //
    void AddPixel(PALETTEENTRY pal);    

    // GetPalette retrieves a palette based on the color information provided so far.
    // 
    void GeneratePalette(PALETTEENTRY pal[MAX_PALETTE_ENTRIES]);

private:
    OptimalPaletteNode *m_pRoot;                // Octree root
    OptimalPaletteNode *m_pReducibleList[MAX_COLOR_COMP_BITS];   
                                                // Reducible list for non-leaf nodes
    int                 m_nSignificantBits;     // How many color bits to pay attention to?
    int                 m_nLeafNodes;           // How many leaf nodes?

private:
    void FreeTree(OptimalPaletteNode* pRoot);   // Delete all memory allocated in the tree
    void GetPaletteRecursive(OptimalPaletteNode *pRoot,
                             PALETTEENTRY pal[256],
                             UINT *pIdx);       // Recursively fill in the palette
};

#endif // _MLOptPal_
