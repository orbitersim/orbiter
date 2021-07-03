//
// CParticle
// Copyright (c) 1999 Microsoft Corporation. All rights reserved.
//

#pragma once
#ifndef __CParticle_HPP
#define __CParticle_HPP

#include <d3d.h>


//////////////////////////////////////////////////////////////////////////////
// Types /////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////

typedef struct PARTICLE_LIST
{
    BOOL m_bSpark;          // Spark? or real particle?

    D3DXVECTOR3 m_vecPos;   // Current position
    D3DXVECTOR3 m_vecVel;   // Current velocity

    D3DXVECTOR3 m_vecPos0;  // Initial position
    D3DXVECTOR3 m_vecVel0;  // Initial velocity
    float m_fTime0;         // Time of creation

    D3DXCOLOR m_clrDiffuse; // Initial diffuse color
    D3DXCOLOR m_clrFade;    // Faded diffuse color
    float m_fFade;          // Fade progression

    PARTICLE_LIST *m_pNext; // Next particle in list
} PARTICLE_LIST;



//////////////////////////////////////////////////////////////////////////////
// CParticle /////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////


class CParticle
{
public:
    CParticle();
   ~CParticle();
   
    HRESULT Initialize(float fRadius);

    HRESULT CreateSurfaces();
    HRESULT RestoreSurfaces();
    HRESULT ReleaseSurfaces();

    HRESULT Update(float fSecsPerFrame, UINT uEmit, const D3DXCOLOR &dwEmitColor, 
                   const D3DXCOLOR &dwFadeColor, float fEmitVel, BOOL bStatic);

    HRESULT Draw(const D3DXMATRIX &matPos);
    HRESULT DrawLights();

    UINT GetNumParticles() { return m_uParticles; }
    UINT GetNumTriangles() { UINT u = m_uTriangles; m_uTriangles = 0; return u; }


protected:
    float m_fRadius;

    UINT m_uParticles;
    UINT m_uParticlesLim;

    PARTICLE_LIST *m_pParticles;
    PARTICLE_LIST *m_pParticlesFree;


    // Geometry
    UINT m_uIndices;
    UINT m_uVertices;

    WORD *m_pwIndices;
    LPDIRECT3DVERTEXBUFFER7 m_pvbVertices;

    UINT m_uTriangles;
};


#endif __CParticle_HPP
