//
// CLensFlare
// Copyright (c) 1999 Microsoft Corporation. All rights reserved.
//

#include "pch.hpp"
//#define TFAN



//////////////////////////////////////////////////////////////////////////////
// Globals ///////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////

extern LPDIRECT3D7       g_pD3D;
extern LPDIRECT3DDEVICE7 g_pD3DDevice;


//////////////////////////////////////////////////////////////////////////////
// Function prototypes ///////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////

HRESULT
TransformVertex(D3DTLVERTEX* pvVertex);


//////////////////////////////////////////////////////////////////////////////
// CLensFlare implementation //////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////

CLensFlare::CLensFlare()
{
    m_fSourceRadius   = 0.0f;
    m_pSourceTexture  = NULL;

    m_uFlares         = 0;
    m_pfFlareOpacity  = NULL;
    m_pfFlareRadius   = NULL;
    m_pfFlarePosition = NULL;
    m_ppFlareTexture  = NULL;

    m_colorLight = D3DXCOLOR(0.0f, 0.0f, 0.0f, 0.0f);
    m_vecLight = D3DXVECTOR4(0.0f, -1.0f, 0.0f, 0.0f);
}


CLensFlare::~CLensFlare()
{
    if(m_pSourceTexture)
        m_pSourceTexture->Release();

    if(m_pfFlareOpacity)
        delete [] m_pfFlareOpacity;

    if(m_pfFlareRadius)
        delete [] m_pfFlareRadius;

    if(m_pfFlarePosition)
        delete [] m_pfFlarePosition;

    if(m_ppFlareTexture)
        delete [] m_ppFlareTexture;
}


HRESULT
CLensFlare::Initialize(UINT uFlares, float fDistance)
{
    if(uFlares)
    {
        m_pfFlareOpacity  = new float[uFlares];
        m_pfFlareRadius   = new float[uFlares];
        m_pfFlarePosition = new float[uFlares];
        m_ppFlareTexture  = new LPDIRECTDRAWSURFACE7[uFlares];

        if(!m_pfFlareOpacity || !m_pfFlareRadius || !m_pfFlarePosition || !m_ppFlareTexture)
        {
            delete [] m_pfFlareOpacity;
            delete [] m_pfFlareRadius;
            delete [] m_pfFlarePosition;
            delete [] m_ppFlareTexture;

            m_pfFlareOpacity  = NULL;
            m_pfFlareRadius   = NULL;
            m_pfFlarePosition = NULL;
            m_ppFlareTexture  = NULL;

            return E_OUTOFMEMORY;
        }

        memset(m_ppFlareTexture, 0x00, uFlares * sizeof(LPDIRECTDRAWSURFACE7));
    }

    m_fDistance = fDistance;
    m_uFlares = uFlares;
    return S_OK;
}



HRESULT
CLensFlare::Draw(const D3DXMATRIX &matPos)
{
    #define DIFFUSE   (D3DCOLOR) D3DRGBA(1.0f, 1.0f, 1.0f, 1.0f)
    #define SPECULAR  (D3DCOLOR) D3DRGB(0.0f, 0.0f, 0.0f)

    static D3DLVERTEX d3dVertex[] =
    {
        { 0.0f, 0.0f, 0.0f, 0, DIFFUSE, SPECULAR, 1.0f, 1.0f },
        { 0.0f, 0.0f, 0.0f, 0, DIFFUSE, SPECULAR, 0.0f, 1.0f },
        { 0.0f, 0.0f, 0.0f, 0, DIFFUSE, SPECULAR, 0.0f, 0.0f },

#ifndef TFAN
        { 0.0f, 0.0f, 0.0f, 0, DIFFUSE, SPECULAR, 1.0f, 1.0f },
        { 0.0f, 0.0f, 0.0f, 0, DIFFUSE, SPECULAR, 0.0f, 0.0f },
#endif
        { 0.0f, 0.0f, 0.0f, 0, DIFFUSE, SPECULAR, 1.0f, 0.0f }
    };


    LPDIRECTDRAWSURFACE7 pSurface;
    LPDIRECTDRAWSURFACE7 pZBuffer;


    BOOL bFlares = TRUE;
    DWORD dwLight = m_colorLight;

    HRESULT hr;



    //
    // Compute center and axis of flares
    //

    float fDot;
    D3DXVECTOR3 vecPos, vecDir, vecCenter;
    D3DXVECTOR3 vecLight, vecAxis, vecDx, vecDy;
    D3DXVECTOR3 vecX, vecY, vec, vecSx, vecSy;


    // Calculate our position and direction
    vecPos = D3DXVECTOR3(matPos.m30, matPos.m31, matPos.m32);
    vecDir = D3DXVECTOR3(0.0f, 0.0f, -1.0f);

    D3DXVec3TransformNormal(&vecDir, &vecDir, &matPos);
    D3DXVec3Normalize(&vecDir, &vecDir);

    // Figure out of light (or flare) might be visible
    vecLight = D3DXVECTOR3(-m_vecLight.x, -m_vecLight.y, -m_vecLight.z);
    D3DXVec3Normalize(&vecLight, &vecLight);

    fDot = D3DXVec3Dot(&vecLight, &vecDir);

    if(fDot <= 0.00001f)
        return S_OK;

    // Calculate the point directly in front of us, on the far clip plane
    vecCenter = vecDir * m_fDistance + vecPos;

    // Calculate position of light on the far clip plane
    vecLight = vecLight * (m_fDistance / fDot) + vecPos;

    // Compute axis which goes from light through the center of the screen
    vecAxis = vecLight - vecCenter;

    D3DXVec3Normalize(&vecDx, &vecAxis);
    D3DXVec3Cross(&vecDy, &vecDx, &vecDir);




    //
    // Figure out if light is behind something else
    //

    if(SUCCEEDED(hr = g_pD3DDevice->GetRenderTarget(&pSurface)))
    {
        DDSCAPS2 ddsc;
        ZeroMemory(&ddsc, sizeof(ddsc));
        ddsc.dwCaps = DDSCAPS_ZBUFFER;

        if(SUCCEEDED(hr = pSurface->GetAttachedSurface(&ddsc, &pZBuffer)))
        {
            D3DTLVERTEX d3dTL;

            d3dTL.sx = vecLight.x;
            d3dTL.sy = vecLight.y;
            d3dTL.sz = vecLight.z;

            if(SUCCEEDED(hr = TransformVertex(&d3dTL)))
            {
                DDSURFACEDESC2 ddsd;
                ddsd.dwSize = sizeof(ddsd);
                pSurface->GetSurfaceDesc(&ddsd);
                UINT cb = ddsd.ddpfPixelFormat.dwZBufferBitDepth >> 3;

                if(SUCCEEDED(hr = pZBuffer->Lock(NULL, &ddsd, DDLOCK_READONLY | DDLOCK_WAIT, NULL)))
                {
                    BYTE *pb = (BYTE *) ddsd.lpSurface + (UINT) d3dTL.sy * ddsd.lPitch + (UINT) d3dTL.sx * cb;
                    BYTE *pbLim = pb + cb;

                    for(; pb < pbLim; pb++)
                    {
                        if(*pb != 0xff)
                        {
                            bFlares = FALSE;
                            break;
                        }
                    }

                    pZBuffer->Unlock(NULL);
                }
            }

            pZBuffer->Release();
        }

        pSurface->Release();
    }


    //
    // Draw light source
    //

    // Figure out screen X and Y axes in model coordinates
    vecX = D3DXVECTOR3(1.0f, 0.0f, 0.0f);
    D3DXVec3TransformNormal(&vecX, &vecX, &matPos);
    D3DXVec3Normalize(&vecX, &vecX);
    D3DXVec3Cross(&vecY, &vecX, &vecDir);

    vecSx = vecX * m_fSourceRadius;
    vecSy = vecY * m_fSourceRadius;



    d3dVertex[0].x = vecLight.x + vecSx.x - vecSy.x;
    d3dVertex[0].y = vecLight.y + vecSx.y - vecSy.y;
    d3dVertex[0].z = vecLight.z + vecSx.z - vecSy.z;
    d3dVertex[0].color = dwLight;

    d3dVertex[1].x = vecLight.x - vecSx.x - vecSy.x;
    d3dVertex[1].y = vecLight.y - vecSx.y - vecSy.y;
    d3dVertex[1].z = vecLight.z - vecSx.z - vecSy.z;
    d3dVertex[1].color = dwLight;

    d3dVertex[2].x = vecLight.x - vecSx.x + vecSy.x;
    d3dVertex[2].y = vecLight.y - vecSx.y + vecSy.y;
    d3dVertex[2].z = vecLight.z - vecSx.z + vecSy.z;
    d3dVertex[2].color = dwLight;

#ifdef TFAN
    d3dVertex[3].x = vecLight.x + vecSx.x + vecSy.x;
    d3dVertex[3].y = vecLight.y + vecSx.y + vecSy.y;
    d3dVertex[3].z = vecLight.z + vecSx.z + vecSy.z;
    d3dVertex[3].color = dwLight;

    g_pD3DDevice->SetTexture(0, m_pSourceTexture);
    g_pD3DDevice->DrawPrimitive(D3DPT_TRIANGLEFAN, D3DFVF_LVERTEX, d3dVertex, 4, D3DDP_WAIT);

#else

    d3dVertex[3].x = vecLight.x + vecSx.x - vecSy.x;
    d3dVertex[3].y = vecLight.y + vecSx.y - vecSy.y;
    d3dVertex[3].z = vecLight.z + vecSx.z - vecSy.z;
    d3dVertex[3].color = dwLight;

    d3dVertex[4].x = vecLight.x - vecSx.x + vecSy.x;
    d3dVertex[4].y = vecLight.y - vecSx.y + vecSy.y;
    d3dVertex[4].z = vecLight.z - vecSx.z + vecSy.z;
    d3dVertex[4].color = dwLight;

    d3dVertex[5].x = vecLight.x + vecSx.x + vecSy.x;
    d3dVertex[5].y = vecLight.y + vecSx.y + vecSy.y;
    d3dVertex[5].z = vecLight.z + vecSx.z + vecSy.z;
    d3dVertex[5].color = dwLight;

    g_pD3DDevice->SetTexture(0, m_pSourceTexture);
    g_pD3DDevice->DrawPrimitive(D3DPT_TRIANGLELIST, D3DFVF_LVERTEX, d3dVertex, 6, D3DDP_WAIT);
#endif


    //
    // Disable Z-buffer and draw flares
    //

    if(!bFlares || !m_uFlares)
        return S_OK;

    UINT uFlare;
    DWORD dwFlareColor;


    DWORD dwZB;
    D3DXCOLOR color;

    g_pD3DDevice->GetRenderState(D3DRENDERSTATE_ZENABLE, &dwZB);
    g_pD3DDevice->SetRenderState(D3DRENDERSTATE_ZENABLE, D3DZB_FALSE);

    for(uFlare = 0; uFlare < m_uFlares; uFlare++)
    {
        vec = vecAxis * m_pfFlarePosition[uFlare] + vecCenter;

        vecSx = vecDx * m_pfFlareRadius[uFlare];
        vecSy = vecDy * m_pfFlareRadius[uFlare];

        color = m_colorLight * m_pfFlareOpacity[uFlare];
        dwFlareColor = color;

        d3dVertex[0].x = vec.x + vecSx.x - vecSy.x;
        d3dVertex[0].y = vec.y + vecSx.y - vecSy.y;
        d3dVertex[0].z = vec.z + vecSx.z - vecSy.z;
        d3dVertex[0].color = dwFlareColor;

        d3dVertex[1].x = vec.x - vecSx.x - vecSy.x;
        d3dVertex[1].y = vec.y - vecSx.y - vecSy.y;
        d3dVertex[1].z = vec.z - vecSx.z - vecSy.z;
        d3dVertex[1].color = dwFlareColor;

        d3dVertex[2].x = vec.x - vecSx.x + vecSy.x;
        d3dVertex[2].y = vec.y - vecSx.y + vecSy.y;
        d3dVertex[2].z = vec.z - vecSx.z + vecSy.z;
        d3dVertex[2].color = dwFlareColor;

#ifdef TFAN
        d3dVertex[3].x = vec.x + vecSx.x + vecSy.x;
        d3dVertex[3].y = vec.y + vecSx.y + vecSy.y;
        d3dVertex[3].z = vec.z + vecSx.z + vecSy.z;
        d3dVertex[3].color = dwFlareColor;

        g_pD3DDevice->SetTexture(0, m_ppFlareTexture[uFlare]);
        g_pD3DDevice->DrawPrimitive(D3DPT_TRIANGLEFAN, D3DFVF_LVERTEX, d3dVertex, 4, D3DDP_WAIT);
#else
        d3dVertex[3].x = vec.x + vecSx.x - vecSy.x;
        d3dVertex[3].y = vec.y + vecSx.y - vecSy.y;
        d3dVertex[3].z = vec.z + vecSx.z - vecSy.z;
        d3dVertex[3].color = dwFlareColor;

        d3dVertex[4].x = vec.x - vecSx.x + vecSy.x;
        d3dVertex[4].y = vec.y - vecSx.y + vecSy.y;
        d3dVertex[4].z = vec.z - vecSx.z + vecSy.z;
        d3dVertex[4].color = dwFlareColor;

        d3dVertex[5].x = vec.x + vecSx.x + vecSy.x;
        d3dVertex[5].y = vec.y + vecSx.y + vecSy.y;
        d3dVertex[5].z = vec.z + vecSx.z + vecSy.z;
        d3dVertex[5].color = dwFlareColor;

        g_pD3DDevice->SetTexture(0, m_ppFlareTexture[uFlare]);
        g_pD3DDevice->DrawPrimitive(D3DPT_TRIANGLELIST, D3DFVF_LVERTEX, d3dVertex, 6, D3DDP_WAIT);
#endif

    }

    g_pD3DDevice->SetRenderState(D3DRENDERSTATE_ZENABLE, dwZB);
    return S_OK;
}




//////////////////////////////////////////////////////////////////////////////
// Helper functions //////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////


//-----------------------------------------------------------------------------
//
// TransformVertex
//
// Manually transform a vertex from local 3D space to 2D screen coordinates.
//
//-----------------------------------------------------------------------------

HRESULT
TransformVertex(D3DTLVERTEX* pvVertex)
{

    // Get the width and height of the viewport. This is needed to scale the
    // transformed vertices to fit the render window.
    D3DVIEWPORT7 vp;
    g_pD3DDevice->GetViewport(&vp);

    float fClipWidth2  = (float) vp.dwWidth;
    float fClipHeight2 = (float) vp.dwHeight;
    float fClipWidth   = fClipWidth2 * 0.5f;
    float fClipHeight  = fClipHeight2 * 0.5f;

    // Get the current matrix set. This is needed for the transformation.
    D3DXMATRIX matWorld, matView, matProj, matSet;
    g_pD3DDevice->GetTransform(D3DTRANSFORMSTATE_WORLD, matWorld);
    g_pD3DDevice->GetTransform(D3DTRANSFORMSTATE_VIEW, matView);
    g_pD3DDevice->GetTransform(D3DTRANSFORMSTATE_PROJECTION, matProj);

    D3DXMatrixMultiply(&matSet, &matWorld, &matView);
    D3DXMatrixMultiply(&matSet, &matSet, &matProj);

    // Get the untransformed vertex position
    float fX = pvVertex->sx;
    float fY = pvVertex->sy;
    float fZ = pvVertex->sz;

    // Transform it through the current matrix set
    float fXp = matSet.m00*fX + matSet.m10*fY + matSet.m20*fZ + matSet.m30;
    float fYp = matSet.m01*fX + matSet.m11*fY + matSet.m21*fZ + matSet.m31;
    float fZp = matSet.m02*fX + matSet.m12*fY + matSet.m22*fZ + matSet.m32;
    float fWp = matSet.m03*fX + matSet.m13*fY + matSet.m23*fZ + matSet.m33;

    float fWpInv = 1.0f / fWp;

    // Finally, scale the vertices to screen coords. This step first
    // "flattens" the coordinates from 3D space to 2D device coordinates,
    // by dividing each coordinate by the wp value. Then, the x- and
    // y-components are transformed from device coords to screen coords.
    // Note 1: device coords range from -1 to +1 in the viewport.
    // Note 2: the sz-coordinate will be used in the z-buffer.

    pvVertex->sx  = (1.0f + (fXp * fWpInv)) * fClipWidth;
    pvVertex->sy  = (1.0f - (fYp * fWpInv)) * fClipHeight;
    pvVertex->sz  = fZp * fWpInv;
    pvVertex->rhw = fWp;

    if(pvVertex->sx < 0.0f || pvVertex->sx > fClipWidth2 ||
       pvVertex->sy < 0.0f || pvVertex->sy > fClipHeight2)
    {
       return E_FAIL;
    }

    return S_OK;
}
