//
// CParticle
// Copyright (c) 1999 Microsoft Corporation. All rights reserved.
//

#include "pch.hpp"

#define MAX_VB_PARTICLES 128


//////////////////////////////////////////////////////////////////////////////
// Types /////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////


#pragma pack(4)
typedef struct PARTICLE_VERTEX
{
    D3DXVECTOR3 m_vecPos;
    D3DCOLOR    m_dwDiffuse;
    D3DXVECTOR2 m_vecTex;
} PARTICLE_VERTEX;


//////////////////////////////////////////////////////////////////////////////
// Globals ///////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////

extern LPDIRECT3D7       g_pD3D;
extern LPDIRECT3DDEVICE7 g_pD3DDevice;
extern D3DDEVICEDESC7    g_D3DDeviceDesc;



//////////////////////////////////////////////////////////////////////////////
// CParticle implementation ///////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////

CParticle::CParticle()
{
    m_fRadius = 0.0f;

    m_uParticles = 0;
    m_uParticlesLim = 0;

    m_pParticles = NULL;
    m_pParticlesFree = NULL;

    m_uIndices = 0;
    m_uVertices = 0;

    m_pwIndices = NULL;
    m_pvbVertices = NULL;
}


CParticle::~CParticle()
{
    while(m_pParticles)
    {
        PARTICLE_LIST *pSpark = m_pParticles;
        m_pParticles = pSpark->m_pNext;
        delete pSpark;
    }

    while(m_pParticlesFree)
    {
        PARTICLE_LIST *pSpark = m_pParticlesFree;
        m_pParticlesFree = pSpark->m_pNext;
        delete pSpark;
    }

    if(m_pwIndices)
        delete [] m_pwIndices;

    ReleaseSurfaces();
}


HRESULT
CParticle::Initialize(float fRadius)
{
    HRESULT hr;

    m_fRadius = fRadius;

    m_uIndices = MAX_VB_PARTICLES * 6;
    m_uVertices = MAX_VB_PARTICLES * 4;
    m_uTriangles = 0;


    // Create indices
    m_pwIndices = new WORD[m_uIndices];

    if(!m_pwIndices)
        return E_OUTOFMEMORY;

    UINT uIndex = 0;
    UINT uVertex = 0;

    while(uIndex < MAX_VB_PARTICLES * 6)
    {
        m_pwIndices[uIndex + 0] = uVertex + 0;    
        m_pwIndices[uIndex + 1] = uVertex + 3;    
        m_pwIndices[uIndex + 2] = uVertex + 1;    

        m_pwIndices[uIndex + 3] = uVertex + 1;    
        m_pwIndices[uIndex + 4] = uVertex + 3;    
        m_pwIndices[uIndex + 5] = uVertex + 2;    

        uVertex += 4;
        uIndex += 6;
    }


    // Create particles
    m_uParticles = 0;
    m_uParticlesLim = 500;

    return S_OK;
}


HRESULT
CParticle::CreateSurfaces()
{
    HRESULT hr;

    if(!m_pvbVertices)
    {
        // Create vertices
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
    }

    return S_OK;
}


HRESULT
CParticle::RestoreSurfaces()
{
    HRESULT hr;

    if(FAILED(hr = ReleaseSurfaces()))
        return hr;

    if(FAILED(hr = CreateSurfaces()))
        return hr;

    return S_OK;
}


HRESULT
CParticle::ReleaseSurfaces()
{
    if(m_pvbVertices)
    {
        m_pvbVertices->Release();
        m_pvbVertices = NULL;
    }

    return S_OK;
}


HRESULT 
CParticle::Update(float fSecsPerFrame, UINT uEmit, const D3DXCOLOR &clrEmitColor, 
                  const D3DXCOLOR &clrFadeColor, float fEmitVel, BOOL bStatic)
{

    PARTICLE_LIST *pParticle, **ppParticle;
    static float fTime = 0.0f;
    fTime += fSecsPerFrame;


    ppParticle = &m_pParticles;

    while(*ppParticle)
    {
        pParticle = *ppParticle;


        // Calculate new position
        float fT = fTime - pParticle->m_fTime0;
        float fGravity;


        if(pParticle->m_bSpark)
        {
            fGravity = -5.0f;
            pParticle->m_fFade -= fSecsPerFrame * 2.25f;
        }
        else
        {
            fGravity = -9.8f;
            pParticle->m_fFade -= fSecsPerFrame * 0.25f;
        }

        pParticle->m_vecPos = pParticle->m_vecVel0 * fT + pParticle->m_vecPos0;
        pParticle->m_vecPos.y += (0.5f * fGravity) * (fT * fT);
        pParticle->m_vecVel.y = pParticle->m_vecVel0.y + fGravity * fT;


        if(pParticle->m_fFade < 0.0f)
            pParticle->m_fFade = 0.0f;


        // Kill old particles
        if(pParticle->m_vecPos.y < m_fRadius || 
            pParticle->m_bSpark && pParticle->m_fFade <= 0.0f)
        {
            // Emit sparks
            if(!pParticle->m_bSpark)
            {
                for(int i = 0; i < 4; i++)
                {
                    PARTICLE_LIST *pSpark;

                    if(m_pParticlesFree)
                    {
                        pSpark = m_pParticlesFree;
                        m_pParticlesFree = pSpark->m_pNext;
                    }
                    else
                    {
                        if(!(pSpark = new PARTICLE_LIST))
                            return E_OUTOFMEMORY;
                    }

                    pSpark->m_pNext = m_pParticles;
                    m_pParticles = pSpark;

                    pSpark->m_bSpark = TRUE;
                    pSpark->m_vecPos0 = pParticle->m_vecPos;
                    pSpark->m_vecPos0.y = m_fRadius;

                    float fRand1 = ((float) rand() / (float) RAND_MAX) * D3DX_PI * 2.0f;
                    float fRand2 = ((float) rand() / (float) RAND_MAX) * D3DX_PI * 0.25f;

                    pSpark->m_vecVel0.x = pParticle->m_vecVel.x * 0.25f + cosf(fRand1) * sinf(fRand2);
                    pSpark->m_vecVel0.z = pParticle->m_vecVel.z * 0.25f + sinf(fRand1) * sinf(fRand2);
                    pSpark->m_vecVel0.y = cosf(fRand2);
                    pSpark->m_vecVel0.y *= ((float) rand() / (float) RAND_MAX) * 1.5f;

                    pSpark->m_vecPos = pSpark->m_vecPos0;
                    pSpark->m_vecVel = pSpark->m_vecVel0;

                    D3DXColorLerp(&pSpark->m_clrDiffuse, &pParticle->m_clrFade, &pParticle->m_clrDiffuse, pParticle->m_fFade);
                    pSpark->m_clrFade = D3DXCOLOR(0.0f, 0.0f, 0.0f, 1.0f);
                    pSpark->m_fFade = 1.0f;
                    pSpark->m_fTime0 = fTime;
                }
            }


            // Kill particle
            *ppParticle = pParticle->m_pNext;
            pParticle->m_pNext = m_pParticlesFree;
            m_pParticlesFree = pParticle;

            if(!pParticle->m_bSpark)
                m_uParticles--;
        }
        else
        {
            ppParticle = &pParticle->m_pNext;
        }
    }



    // Emit new particles
	UINT uParticlesEmit = m_uParticles + uEmit;
    while(m_uParticles < m_uParticlesLim && m_uParticles < uParticlesEmit)
    {
        if(m_pParticlesFree)
        {
            pParticle = m_pParticlesFree;
            m_pParticlesFree = pParticle->m_pNext;
        }
        else
        {
            if(!(pParticle = new PARTICLE_LIST))
                return E_OUTOFMEMORY;
        }

        pParticle->m_pNext = m_pParticles;
        m_pParticles = pParticle;
        m_uParticles++;


        // Emit new particle
        float fRand1 = ((float) rand() / (float) RAND_MAX) * D3DX_PI * 2.0f;
        float fRand2 = ((float) rand() / (float) RAND_MAX) * D3DX_PI * 0.25f;

        pParticle->m_bSpark = FALSE;

        if(bStatic)
            pParticle->m_vecPos0 = D3DXVECTOR3(0.0f, m_fRadius, 0.0f);
        else
            pParticle->m_vecPos0 = D3DXVECTOR3(4.0f * cosf(fTime), m_fRadius, 4.0f * sinf(fTime));

        pParticle->m_vecVel0.x = cosf(fRand1) * sinf(fRand2) * 2.5f;
        pParticle->m_vecVel0.z = sinf(fRand1) * sinf(fRand2) * 2.5f;
        pParticle->m_vecVel0.y = cosf(fRand2);
        pParticle->m_vecVel0.y *= ((float) rand() / (float) RAND_MAX) * fEmitVel;

        int nRand = rand();

        pParticle->m_vecPos = pParticle->m_vecPos0;
        pParticle->m_vecVel = pParticle->m_vecVel0;

        pParticle->m_clrDiffuse = clrEmitColor;
        pParticle->m_clrFade = clrFadeColor;
        pParticle->m_fFade = 1.0f;
        pParticle->m_fTime0 = fTime;
    }


    m_uTriangles = 0;
    return S_OK;
}



HRESULT
CParticle::Draw(const D3DXMATRIX &matPos)
{
    HRESULT hr;

    D3DXVECTOR3 vecTL(-m_fRadius,  m_fRadius,  0.0f);
    D3DXVECTOR3 vecBL(-m_fRadius, -m_fRadius,  0.0f);
    D3DXVECTOR3 vecBR( m_fRadius, -m_fRadius,  0.0f);
    D3DXVECTOR3 vecTR( m_fRadius,  m_fRadius,  0.0f);

    D3DXVec3TransformNormal(&vecTL, &vecTL, &matPos);
    D3DXVec3TransformNormal(&vecBL, &vecBL, &matPos);
    D3DXVec3TransformNormal(&vecBR, &vecBR, &matPos);
    D3DXVec3TransformNormal(&vecTR, &vecTR, &matPos);





    PARTICLE_LIST *pParticle = m_pParticles;
    PARTICLE_VERTEX *pVertices;
    UINT uVertex = 0;

    if(FAILED(hr = m_pvbVertices->Lock(DDLOCK_WAIT | DDLOCK_WRITEONLY, (void **) &pVertices, NULL)))
        return hr;



    while(pParticle)
    {
        D3DXVECTOR3 vecPos(pParticle->m_vecPos);
        D3DXVECTOR3 vecVel(pParticle->m_vecVel);

        float fL2 = D3DXVec3LengthSq(&vecVel);
        UINT uSteps;
        
        #define SQ(x) ((x) * (x))

             if(fL2 < SQ(1.0f)) uSteps = 2;
        else if(fL2 < SQ(2.0f)) uSteps = 3;
        else if(fL2 < SQ(3.0f)) uSteps = 4;
        else if(fL2 < SQ(3.5f)) uSteps = 5;
        else if(fL2 < SQ(4.0f)) uSteps = 6;
        else if(fL2 < SQ(4.5f)) uSteps = 7;
        else                    uSteps = 8;


        vecVel *= -0.04f / (float) uSteps;

        D3DXCOLOR clrDiffuse;
        D3DXColorLerp(&clrDiffuse, &pParticle->m_clrFade, &pParticle->m_clrDiffuse, pParticle->m_fFade);
        DWORD dwDiffuse = (DWORD) clrDiffuse;


        for(UINT u = 0; u < uSteps; u++)
        {
            pVertices->m_vecPos = vecPos + vecTL;
            pVertices->m_dwDiffuse = dwDiffuse;
            pVertices->m_vecTex = D3DXVECTOR2(0.0f, 0.0f);
            pVertices++;

            pVertices->m_vecPos = vecPos + vecBL;
            pVertices->m_dwDiffuse = dwDiffuse;
            pVertices->m_vecTex = D3DXVECTOR2(0.0f, 1.0f);
            pVertices++;

            pVertices->m_vecPos = vecPos + vecBR;
            pVertices->m_dwDiffuse = dwDiffuse;
            pVertices->m_vecTex = D3DXVECTOR2(1.0f, 1.0f);
            pVertices++;

            pVertices->m_vecPos = vecPos + vecTR;
            pVertices->m_dwDiffuse = dwDiffuse;
            pVertices->m_vecTex = D3DXVECTOR2(1.0f, 0.0f);
            pVertices++;


            uVertex += 4;
            if(uVertex == m_uVertices)
            {
                m_pvbVertices->Unlock();

		        g_pD3DDevice->DrawIndexedPrimitiveVB(D3DPT_TRIANGLELIST, 
                    m_pvbVertices, 0, m_uVertices, m_pwIndices, m_uIndices, D3DDP_WAIT);

                if(FAILED(hr = m_pvbVertices->Lock(DDLOCK_WAIT | DDLOCK_WRITEONLY, (void **) &pVertices, NULL)))
                    return hr;

                m_uTriangles += uVertex >> 2;
                uVertex = 0;
            }

            vecPos += vecVel;
        }


        pParticle = pParticle->m_pNext;
    }


    m_pvbVertices->Unlock();

	if(uVertex)
	{
		g_pD3DDevice->DrawIndexedPrimitiveVB(D3DPT_TRIANGLELIST, 
            m_pvbVertices, 0, uVertex, m_pwIndices, (uVertex >> 1) * 3, D3DDP_WAIT);

        m_uTriangles += uVertex >> 2;
	}

        
    return S_OK;
}


HRESULT
CParticle::DrawLights()
{
    HRESULT hr;

    D3DXVECTOR3 vecTL(-1.0f,  0.0f,  1.0f);
    D3DXVECTOR3 vecBL(-1.0f,  0.0f, -1.0f);
    D3DXVECTOR3 vecBR( 1.0f,  0.0f, -1.0f);
    D3DXVECTOR3 vecTR( 1.0f,  0.0f,  1.0f);


    PARTICLE_LIST *pParticle = m_pParticles;
    PARTICLE_VERTEX *pVertices;
    UINT uVertex = 0;

    if(FAILED(hr = m_pvbVertices->Lock(DDLOCK_WAIT | DDLOCK_WRITEONLY, (void **) &pVertices, NULL)))
        return hr;


    while(pParticle)
    {
        float fY = pParticle->m_vecPos.y;

		if(fY < 1.0f)
		{	
			if(fY < 0.0f)
				fY = 0.0f;

			float fSize = fY * 0.25f + m_fRadius;

            D3DXCOLOR clrDiffuse;
            D3DXColorLerp(&clrDiffuse, &pParticle->m_clrFade, &pParticle->m_clrDiffuse, pParticle->m_fFade);
            DWORD dwDiffuse = (DWORD) (clrDiffuse * ((1.0f - fY) * 0.5f));
            D3DXVECTOR3 vecPos(pParticle->m_vecPos.x, 0.0f, pParticle->m_vecPos.z);


            pVertices->m_vecPos = vecPos + vecTR * fSize;
			pVertices->m_dwDiffuse = dwDiffuse;
            pVertices->m_vecTex = D3DXVECTOR2(0.0f, 0.0f);
            pVertices++;

            pVertices->m_vecPos = vecPos + vecBR * fSize;
			pVertices->m_dwDiffuse = dwDiffuse;
            pVertices->m_vecTex = D3DXVECTOR2(0.0f, 1.0f);
            pVertices++;

            pVertices->m_vecPos = vecPos + vecBL * fSize;
			pVertices->m_dwDiffuse = dwDiffuse;
            pVertices->m_vecTex = D3DXVECTOR2(1.0f, 1.0f);
            pVertices++;

            pVertices->m_vecPos = vecPos + vecTL * fSize;
			pVertices->m_dwDiffuse = dwDiffuse;
            pVertices->m_vecTex = D3DXVECTOR2(1.0f, 0.0f);
            pVertices++;


            uVertex += 4;
            if(uVertex == m_uVertices)
            {
                m_pvbVertices->Unlock();

		        g_pD3DDevice->DrawIndexedPrimitiveVB(D3DPT_TRIANGLELIST, 
                    m_pvbVertices, 0, m_uVertices, m_pwIndices, m_uIndices, D3DDP_WAIT);

                if(FAILED(hr = m_pvbVertices->Lock(DDLOCK_WAIT | DDLOCK_WRITEONLY, (void **) &pVertices, NULL)))
                    return hr;

                m_uTriangles += uVertex >> 2;
                uVertex = 0;
            }
		}

        pParticle = pParticle->m_pNext;
    }


    m_pvbVertices->Unlock();

	if(uVertex)
	{

		g_pD3DDevice->DrawIndexedPrimitiveVB(D3DPT_TRIANGLELIST, 
            m_pvbVertices, 0, uVertex, m_pwIndices, (uVertex >> 1) * 3, D3DDP_WAIT);

        m_uTriangles += uVertex >> 2;
	}
        
    return S_OK;
}
