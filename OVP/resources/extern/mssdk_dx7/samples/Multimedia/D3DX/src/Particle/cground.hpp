//
// CGround
// Copyright (c) 1999 Microsoft Corporation. All rights reserved.
//

#pragma once
#ifndef __CGround_HPP
#define __CGround_HPP

#include <d3d.h>
#include <D3DXmath.h>




class CGround
{
    float m_fWidth; 
    float m_fHeight; 
    float m_fTile; 
    DWORD m_dwColor;

    UINT m_uIndices;
    UINT m_uVertices;

    WORD *m_pwIndices;
    LPDIRECT3DVERTEXBUFFER7 m_pvbVertices;

public:
    CGround();
   ~CGround();

    HRESULT Initialize(float fWidth, float fHeight, float fTile, DWORD dwColor);

    HRESULT CreateSurfaces();
    HRESULT RestoreSurfaces();
    HRESULT ReleaseSurfaces();

    HRESULT Draw();
};

#endif // __CGround_HPP
