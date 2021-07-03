//
// CLensFlare
// Copyright (c) 1999 Microsoft Corporation. All rights reserved.
//

#pragma once
#ifndef __CLENSFLARE_HPP
#define __CLENSFLARE_HPP

#include <d3d.h>
#include <D3DXmath.h>



class CLensFlare
{
public:
    CLensFlare();
    ~CLensFlare();

    HRESULT Initialize(UINT uFlares, float fDistance);

    void SetLightColor   (const D3DXCOLOR &color);
    void SetLightPosition(const D3DXVECTOR4 &vec);

    void SetSource(float fRadius, LPDIRECTDRAWSURFACE7 pTexture);

    void SetFlare(UINT uFlare, float fRadius, float fOpacity,
                  float fPosition, LPDIRECTDRAWSURFACE7 pTexture);

    HRESULT Draw(const D3DXMATRIX &matPos);

protected:
    float  m_fDistance;
    float  m_fSourceRadius;
    LPDIRECTDRAWSURFACE7 m_pSourceTexture;

    UINT   m_uFlares;
    float *m_pfFlareOpacity;
    float *m_pfFlareRadius;
    float *m_pfFlarePosition;
    LPDIRECTDRAWSURFACE7 *m_ppFlareTexture;

    D3DXCOLOR m_colorLight;
    D3DXVECTOR4 m_vecLight;
};



//////////////////////////////////////////////////////////////////////////////
// Inline methods ////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////


inline void
CLensFlare::SetLightColor(const D3DXCOLOR &color)
{
    m_colorLight = color;
}

inline void
CLensFlare::SetLightPosition(const D3DXVECTOR4 &vec)
{
    D3DXVec4Normalize(&m_vecLight, &vec);
}

inline void
CLensFlare::SetSource(float fRadius, LPDIRECTDRAWSURFACE7 pTexture)
{
    m_fSourceRadius = fRadius;
    m_pSourceTexture = pTexture;
}

inline void
CLensFlare::SetFlare(UINT uFlare, float fRadius, float fOpacity,
                    float fPosition, LPDIRECTDRAWSURFACE7 pTexture)
{
    m_pfFlareRadius  [uFlare] = fRadius;
    m_pfFlareOpacity [uFlare] = fOpacity;
    m_pfFlarePosition[uFlare] = fPosition;
    m_ppFlareTexture [uFlare] = pTexture;
}

#endif // __CLENSFLARE_HPP