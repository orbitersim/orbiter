//
// CTentacle
// Copyright (c) 1999 Microsoft Corporation. All rights reserved.
//

#include "pch.hpp"


//////////////////////////////////////////////////////////////////////////////
// Types /////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////

#pragma pack(4)
typedef struct TENTACLE_VERTEX
{
    D3DXVECTOR3 m_vecPos;
    D3DVALUE    m_fBeta;
    D3DXVECTOR3 m_vecNorm;
    D3DXVECTOR2 m_vecTex;
} TENTACLE_VERTEX;



//////////////////////////////////////////////////////////////////////////////
// Globals ///////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////

extern LPDIRECT3D7       g_pD3D;
extern LPDIRECT3DDEVICE7 g_pD3DDevice;
extern D3DDEVICEDESC7    g_D3DDeviceDesc;



//////////////////////////////////////////////////////////////////////////////
// CTentacle implementation ///////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////

CTentacle::CTentacle()
{
    m_fRadius0 = 0.0f;
    m_fRadius1 = 0.0f;
    m_fLength  = 0.0f;

    m_uRings = 0;
    m_uSides = 0;

    m_pChild   = NULL;
    m_pSibling = NULL;

    m_fStretch = 1.0f;
    m_vecTwist = D3DXVECTOR3(0.0f, 0.0f, 0.0f);
    m_bChanged = TRUE;

    m_uIndices  = 0;
    m_uVertices = 0;

    m_pwIndices   = NULL;
    m_pvbVertices = NULL;
}


CTentacle::~CTentacle()
{
    if(m_pwIndices)
        delete [] m_pwIndices;

    if(m_pvbVertices)
        m_pvbVertices->Release();

    if(m_pSibling)
        delete m_pSibling;

    if(m_pChild)
        delete m_pChild;
}


HRESULT
CTentacle::Initialize(float fRadius0, float fRadius1, float fLength, float fTex0, float fTex1, UINT uRings, UINT uSides)
{
    HRESULT hr;

    if(uRings < 2 || uSides < 3)
        return E_FAIL;

    m_fRadius0 = fRadius0;
    m_fRadius1 = fRadius1;
    m_fLength  = fLength;
    m_fTex0    = fTex0;
    m_fTex1    = fTex1;
    
    m_uRings = uRings;
    m_uSides = uSides;

    m_fStretch = 1.0f;
    m_vecTwist = D3DXVECTOR3(0.0f, 0.0f, 0.0f);
    m_bChanged = TRUE;

    m_uIndices  = (uRings - 1) * uSides * 6;
    m_uVertices = uRings * (uSides + 1);


    // Create indices
    if(!(m_pwIndices = new WORD[m_uIndices]))
        return E_OUTOFMEMORY;

    UINT r, s;
    WORD *pw = m_pwIndices;

    for(r = 1; r < m_uRings; r++)
    {
        UINT r0 = (r - 1) * (m_uSides + 1);
        UINT r1 = r * (m_uSides + 1);

        for(s = 0; s < m_uSides; s++)
        {
            UINT s0 = s;
            UINT s1 = s + 1;

            pw[0] = r1 + s0;
            pw[1] = r0 + s0;
            pw[2] = r1 + s1;

            pw[3] = pw[2];
            pw[4] = pw[1];
            pw[5] = r0 + s1;

            pw += 6;
        }
    }

    return S_OK;
}


HRESULT
CTentacle::AddChild(CTentacle *pChild)
{
    pChild->m_pSibling = m_pChild;
    m_pChild = pChild;
    return S_OK;
}


void
CTentacle::Stretch(float f)
{
    m_fStretch += f;
    m_bChanged = TRUE;
}


void
CTentacle::Twist(float x, float y, float z)
{
    m_vecTwist.x += x;
    m_vecTwist.y += y;
    m_vecTwist.z += z;
    m_bChanged = TRUE;
}



HRESULT 
CTentacle::CreateSurfaces()
{
    HRESULT hr;

    // Create vertices
    if(!m_pvbVertices)
    {
        TENTACLE_VERTEX *pVertex;
        D3DVERTEXBUFFERDESC vbdesc;

        vbdesc.dwSize = sizeof(vbdesc);
        vbdesc.dwCaps = D3DVBCAPS_WRITEONLY;
        vbdesc.dwFVF = D3DFVF_NORMAL | D3DFVF_XYZB1 | D3DFVF_TEX1 | D3DFVF_TEXCOORDSIZE2(0);
        vbdesc.dwNumVertices = m_uVertices;

        if(!(g_D3DDeviceDesc.dwDevCaps & D3DDEVCAPS_HWTRANSFORMANDLIGHT) ||
            IsEqualGUID(g_D3DDeviceDesc.deviceGUID, IID_IDirect3DRefDevice))
        {
            vbdesc.dwCaps |= D3DVBCAPS_SYSTEMMEMORY;
        }
    
        if(FAILED(hr = g_pD3D->CreateVertexBuffer(&vbdesc, &m_pvbVertices, 0)))
            return hr;

        if(SUCCEEDED(m_pvbVertices->Lock(DDLOCK_WAIT | DDLOCK_WRITEONLY, (void **) &pVertex, NULL)))
        {
            for(UINT r = 0; r < m_uRings; r++)
            {
                float fr = (float) r / (float) (m_uRings - 1);
                float fBetaL = cosf(fr * D3DX_PI * 0.5f);
                float fBetaC = (cosf(fr * D3DX_PI) + 1.0f) * 0.5f;

                for(UINT s = 0; s <= m_uSides; s++)
                {
                    float fs = (float) s / (float) m_uSides;
                    float fCos = cosf(fs * (D3DX_PI * 2.0f));
                    float fSin = sinf(fs * (D3DX_PI * 2.0f));
                    float fRadius = m_fRadius1 + fBetaL * (m_fRadius0 - m_fRadius1);

                    pVertex->m_vecPos  = D3DXVECTOR3(fCos * fRadius, fr * m_fLength, fSin * fRadius);
                    pVertex->m_fBeta   = fBetaC;
                    pVertex->m_vecNorm = D3DXVECTOR3(fCos, 0.0f, fSin);
                    pVertex->m_vecTex  = D3DXVECTOR2(fs, m_fTex0 + fr * (m_fTex1 - m_fTex0));
                    pVertex++;
                }
            }

            m_pvbVertices->Unlock();
            m_pvbVertices->Optimize(g_pD3DDevice, 0);
        }
    }


    if(m_pChild && FAILED(hr = m_pChild->CreateSurfaces()))
        return hr;
    if(m_pSibling && FAILED(hr = m_pSibling->CreateSurfaces()))
        return hr;

    return S_OK;
}


HRESULT 
CTentacle::RestoreSurfaces()
{
    HRESULT hr;

    if(FAILED(hr = ReleaseSurfaces()))
        return hr;
    if(FAILED(hr = CreateSurfaces()))
        return hr;

    return S_OK;
}


HRESULT 
CTentacle::ReleaseSurfaces()
{
    HRESULT hr;

    if(m_pvbVertices)
    {
        m_pvbVertices->Release();
        m_pvbVertices = NULL;
    }

    if(m_pChild && FAILED(hr = m_pChild->ReleaseSurfaces()))
        return hr;
    if(m_pSibling && FAILED(hr = m_pSibling->ReleaseSurfaces()))
        return hr;

    return S_OK;
}



HRESULT
CTentacle::Draw(D3DXMATRIX *pmat, D3DPRIMITIVETYPE primType, BOOL bScale)
{
    D3DXMATRIX mat, matT;

    // Calculate local matrix
    if(m_bChanged)
    {
        D3DXMatrixIdentity(&m_mat);

        float fStretchSqrt = 1.0f / sqrtf(fabsf(m_fStretch));
        D3DXMatrixScaling(&matT, fStretchSqrt, m_fStretch, fStretchSqrt);
        D3DXMatrixMultiply(&m_mat, &m_mat, &matT);

        D3DXMatrixTranslation(&matT, 0.0f, m_fLength * -0.5f, 0.0f);
        D3DXMatrixMultiply(&m_mat, &m_mat, &matT);

        D3DXMatrixRotationY(&matT, m_vecTwist.y);
        D3DXMatrixMultiply(&m_mat, &m_mat, &matT);

        D3DXMatrixRotationX(&matT, m_vecTwist.x);
        D3DXMatrixMultiply(&m_mat, &m_mat, &matT);

        D3DXMatrixRotationZ(&matT, m_vecTwist.z);
        D3DXMatrixMultiply(&m_mat, &m_mat, &matT);

        D3DXMatrixTranslation(&matT, 0.0f, m_fLength * 0.5f, 0.0f);
        D3DXMatrixMultiply(&m_mat, &m_mat, &matT);

        m_bChanged = FALSE;
    }


    // Calculate world1 matrix
    D3DXMatrixMultiply(&mat, &m_mat, pmat);


    // Draw self
    BOOL bStretch = (m_fStretch != 1.0f);

    if(bStretch != bScale)
        g_pD3DDevice->SetRenderState(D3DRENDERSTATE_NORMALIZENORMALS, TRUE);

    g_pD3DDevice->SetRenderState(D3DRENDERSTATE_VERTEXBLEND, D3DVBLEND_1WEIGHT);
    g_pD3DDevice->SetTransform(D3DTRANSFORMSTATE_WORLD, *pmat);
    g_pD3DDevice->SetTransform(D3DTRANSFORMSTATE_WORLD1, mat);

    g_pD3DDevice->DrawIndexedPrimitiveVB(primType, m_pvbVertices, 0, m_uVertices, m_pwIndices, m_uIndices, D3DDP_WAIT);


    // Draw children
    D3DXMatrixTranslation(&matT, 0.0f, m_fLength, 0.0f);
    D3DXMatrixMultiply(&matT, &matT, &mat);

    if(m_pChild)
        m_pChild->Draw(&matT, primType, bScale | bStretch);


    if(bStretch != bScale)
        g_pD3DDevice->SetRenderState(D3DRENDERSTATE_NORMALIZENORMALS, FALSE);


    // Draw siblings
    if(m_pSibling)
        m_pSibling->Draw(pmat, primType, bScale);


    return S_OK;
}




//////////////////////////////////////////////////////////////////////////////
// CGround ///////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////

#pragma pack(4)
typedef struct GROUND_VERTEX
{
    D3DXVECTOR3 m_vecPos;
    D3DCOLOR    m_dwDiffuse;
    D3DXVECTOR2 m_vecTex;
} GROUND_VERTEX;


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

    m_fWidth  = fWidth;
    m_fHeight = fHeight;
    m_fTile   = fTile;
    m_dwColor = dwColor;
    
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


