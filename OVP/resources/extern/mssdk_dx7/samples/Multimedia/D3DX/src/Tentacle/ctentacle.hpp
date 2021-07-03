//
// CTentacle
// Copyright (c) 1999 Microsoft Corporation. All rights reserved.
//

#pragma once
#ifndef __CTENTACLE_HPP
#define __CTENTACLE_HPP

#include <d3d.h>


class CTentacle
{
public:
    CTentacle();
    ~CTentacle();

    HRESULT Initialize(float fRadius0, float fRadius1, float fLength, float fTex0, float fTex1, UINT uRings, UINT uSides);
    HRESULT AddChild(CTentacle *pChild);

    void Stretch(float f);
    void Twist(float x, float y, float z);

    HRESULT CreateSurfaces();
    HRESULT RestoreSurfaces();
    HRESULT ReleaseSurfaces();

    HRESULT Draw(D3DXMATRIX *pM, D3DPRIMITIVETYPE primType, BOOL bScale);


protected:
    // Properties
    float m_fRadius0;
    float m_fRadius1;
    float m_fLength;
    float m_fTex0;
    float m_fTex1;
    
    UINT m_uRings;
    UINT m_uSides;


    // Hierarchy
    CTentacle *m_pChild;
    CTentacle *m_pSibling;


    // Local transform
    float m_fStretch;
    D3DXVECTOR3 m_vecTwist;
    D3DXMATRIX m_mat;
    BOOL m_bChanged;


    // Geometry
    UINT  m_uIndices;
    UINT  m_uVertices;

    WORD *m_pwIndices;
    LPDIRECT3DVERTEXBUFFER7 m_pvbVertices;
};


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



#endif __CTENTACLE_HPP
