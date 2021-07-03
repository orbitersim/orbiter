//
// CGround
// Copyright (c) 1999 Microsoft Corporation. All rights reserved.
//

#include "pch.hpp"


//////////////////////////////////////////////////////////////////////////////
// Types /////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////

#pragma pack(4)
typedef struct GROUND_VERTEX
{
    D3DXVECTOR3 m_vecPos;
    D3DCOLOR    m_dwDiffuse;
    D3DXVECTOR2 m_vecTex;
} GROUND_VERTEX;

//////////////////////////////////////////////////////////////////////////////
// Globals ///////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////

extern LPDIRECT3D7       g_pD3D;
extern LPDIRECT3DDEVICE7 g_pD3DDevice;
extern D3DDEVICEDESC7    g_D3DDeviceDesc;


//////////////////////////////////////////////////////////////////////////////
// CGround implementation ///////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////

CGround::CGround()
{
    m_uIndices  = 0;
    m_uVertices = 0;

    m_pwIndices   = NULL;
    m_pvbVertices = NULL;
}


CGround::~CGround()
{
    if(m_pwIndices)
        delete [] m_pwIndices;

    if(m_pvbVertices)
        m_pvbVertices->Release();
}


HRESULT
CGround::Initialize(float fWidth, float fHeight, float fTile, DWORD dwColor)
{
    HRESULT hr;

    // Calculate number of vertices and indices
    UINT uDivs = 8;

    m_uVertices = (uDivs + 1) * (uDivs + 1);
    m_uIndices  = (uDivs * uDivs) * 6;


    // Create indices
    m_pwIndices = new WORD[m_uIndices];

    if(!m_pwIndices)
        return E_OUTOFMEMORY;


    // Fill in indicies
    WORD *pwIndex = m_pwIndices;

    for(UINT uZ = 0; uZ < uDivs; uZ++)
    {
        for(UINT uX = 0; uX < uDivs; uX++)
        {
            UINT uVertex = uX + uZ * (uDivs + 1);

            pwIndex[0] = uVertex + 0;
            pwIndex[1] = uVertex + 1;
            pwIndex[2] = uVertex + (uDivs + 1);
            pwIndex[3] = uVertex + (uDivs + 1);
            pwIndex[4] = uVertex + 1;
            pwIndex[5] = uVertex + (uDivs + 2);

            pwIndex += 6;
            uVertex++;
        }
    }

    // Save all the data
    m_fWidth = fWidth; 
    m_fHeight = fHeight; 
    m_fTile = fTile; 
    m_dwColor = dwColor;

    return S_OK;
}




HRESULT 
CGround::CreateSurfaces()
{
    HRESULT hr;

    if(!m_pvbVertices)
    {
        // Create vertices
        GROUND_VERTEX *pVertices;
        D3DVERTEXBUFFERDESC vbdesc;

        vbdesc.dwSize = sizeof(vbdesc);
        vbdesc.dwCaps = 0;
        vbdesc.dwFVF = D3DFVF_XYZ | D3DFVF_DIFFUSE | D3DFVF_TEX1 | D3DFVF_TEXCOORDSIZE2(0);
        vbdesc.dwNumVertices = m_uVertices;

        if(!(g_D3DDeviceDesc.dwDevCaps & D3DDEVCAPS_HWTRANSFORMANDLIGHT) ||
            IsEqualGUID(g_D3DDeviceDesc.deviceGUID, IID_IDirect3DRefDevice))
        {
            vbdesc.dwCaps |= D3DVBCAPS_SYSTEMMEMORY;
        }
    
        if(FAILED(hr = g_pD3D->CreateVertexBuffer(&vbdesc, &m_pvbVertices, 0)))
            return hr;

        if(SUCCEEDED(m_pvbVertices->Lock(DDLOCK_WAIT | DDLOCK_WRITEONLY, (void **) &pVertices, NULL)))
        {

            // Fill in vertices
            UINT uDivs = 8;
            UINT uX, uZ;

            float fXInc = m_fWidth  / (float) uDivs;
            float fZInc = m_fHeight / (float) uDivs;

            float fSInc = m_fTile / (float) uDivs;
            float fTInc = m_fTile / (float) uDivs;

            float fZ = m_fHeight * -0.5f;
            float fT = 0.0f;

            for(uZ = 0; uZ <= uDivs; uZ++)
            {
                float fX = m_fWidth  * -0.5f;
                float fS = 0.0f;

                for(uX = 0; uX <= uDivs; uX++)
                {
                    GROUND_VERTEX *pVertex;

                    pVertex = &pVertices[uX + uZ * (uDivs + 1)];
                    pVertex->m_vecPos = D3DXVECTOR3(fX, 0.0f, fZ);
                    pVertex->m_dwDiffuse = m_dwColor;
                    pVertex->m_vecTex = D3DXVECTOR2(fS, fT);

                    fX += fXInc;
                    fS += fSInc;
                }

                fZ += fZInc;
                fT += fTInc;
            }

            m_pvbVertices->Unlock();
            m_pvbVertices->Optimize(g_pD3DDevice, 0);
        }
    }

    return S_OK;
}


HRESULT 
CGround::RestoreSurfaces()
{
    HRESULT hr;

    if(FAILED(hr = ReleaseSurfaces()))
        return hr;
    if(FAILED(hr = CreateSurfaces()))
        return hr;

    return S_OK;
}


HRESULT 
CGround::ReleaseSurfaces()
{
    if(m_pvbVertices)
    {
        m_pvbVertices->Release();
        m_pvbVertices = NULL;
    }

    return S_OK;
}



HRESULT
CGround::Draw()
{
    g_pD3DDevice->DrawIndexedPrimitiveVB(D3DPT_TRIANGLELIST, m_pvbVertices, 0, m_uVertices, m_pwIndices, m_uIndices, D3DDP_WAIT);
    return S_OK;
}


