//-----------------------------------------------------------------------------
// File: envmap.h
//
// Desc: Example code showing how to use cube-maps with D3DX
//
// Copyright (c) 1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------

#ifndef __ENVMAP_H__
#define __ENVMAP_H__
#include <windows.h>
#include <stdio.h>
#include <stdlib.h>
#define D3D_OVERLOADS
#include "d3d.h"
#include "d3dx.h"
#include "resource.h"

#define FULLSCREEN_WIDTH    640
#define FULLSCREEN_HEIGHT   480

#define RELEASE(object) if (object) {object->Release();}

#define SPH_NUM  26
#define SPH_TRIS (2*(SPH_NUM*2) + (SPH_NUM-2)*(SPH_NUM*4))
#define SPH_VTXS (SPH_TRIS*3)
#define SPH_RADIUS (.35F)

typedef struct tagSPHVERTEX
{
    D3DVERTEX v;
    FLOAT   tu2;
} SPHVERTEX, *PSPHVERTEX;

const DWORD SPHVERTEX_FVF = (D3DFVF_XYZ | D3DFVF_NORMAL | D3DFVF_TEX1 | D3DFVF_TEXCOORDSIZE3(0));

class CGlobals
{
public:
    CGlobals();
    ~CGlobals();
    void                    PauseDrawing();
    void                    RestartDrawing();
    HRESULT                 InitD3DX();
    HRESULT                 InitRenderer();
    void                    InterpretError(HRESULT hr);
    HRESULT                 UnInit();
    HRESULT                 Draw();
    HRESULT                 RenderEnvironment();
    void                    InitSphVtx();
    void                    ReflectNormals();
    HRESULT                 HandleModeChanges();
    
    // Application state
    BOOL                    m_bD3DXReady;
    BOOL                    m_bActive;
    BOOL                    m_bIsFullscreen;

    HINSTANCE               m_hInst;
    HWND                    m_hwndMain;
    RECT                    m_rWindowedRect;

    LPDIRECT3DDEVICE7       m_pD3DDev;
    LPDIRECT3D7             m_pD3D;
    LPDIRECTDRAW7           m_pDD;

    ID3DXContext*           m_pD3DX;
    CHAR                    m_szPath[512];

    // Sample-Specific state
    LPDIRECTDRAWSURFACE7    m_pTex[6];
    LPDIRECTDRAWSURFACE7    m_pEnvTex;

    D3DMATERIAL7            m_Material;

    SPHVERTEX               m_SphVtx[SPH_VTXS];
}; // CGlobals

extern CGlobals g;

#endif // __ENVMAP_H__

