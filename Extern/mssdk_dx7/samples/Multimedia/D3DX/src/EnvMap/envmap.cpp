//-----------------------------------------------------------------------------
// File: envmap.cpp
//
// Desc: Example code showing how to use cube-maps with D3DX
//
// Copyright (c) 1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------

#include "envmap.h"
#include "stdio.h"
#include "d3dtypes.h"

#define NAME_OF_THE_APP "D3DX - EnvMap"

#define RELEASENULL(pObject) if (pObject) {pObject->Release(); pObject = NULL;}

CGlobals* g_p = NULL;

typedef struct
{
    D3DVALUE x,y,z;
    D3DCOLOR diffuse;
    D3DVALUE u,v;
} MYVERTEX;

#define BOX_VTXS (4*6)
MYVERTEX BoxVtx[BOX_VTXS] = {
    // Left
    { 1.0f,  1.0f,  1.0f,  0xffffffff,  0.0f, 0.0f},
    { 1.0f,  1.0f, -1.0f,  0xffffffff,  1.0f, 0.0f},
    { 1.0f, -1.0f, -1.0f,  0xffffffff,  1.0f, 1.0f},
    { 1.0f, -1.0f,  1.0f,  0xffffffff,  0.0f, 1.0f},

    // Front
    { 1.0f,  1.0f, -1.0f,  0xffffffff,  0.0f, 0.0f},
    {-1.0f,  1.0f, -1.0f,  0xffffffff,  1.0f, 0.0f},
    {-1.0f, -1.0f, -1.0f,  0xffffffff,  1.0f, 1.0f},
    { 1.0f, -1.0f, -1.0f,  0xffffffff,  0.0f, 1.0f},

    // Right
    {-1.0f,  1.0f, -1.0f,  0xffffffff,  0.0f, 0.0f},
    {-1.0f,  1.0f,  1.0f,  0xffffffff,  1.0f, 0.0f},
    {-1.0f, -1.0f,  1.0f,  0xffffffff,  1.0f, 1.0f},
    {-1.0f, -1.0f, -1.0f,  0xffffffff,  0.0f, 1.0f},

    // Back
    {-1.0f,  1.0f,  1.0f,  0xffffffff,  0.0f, 0.0f},
    { 1.0f,  1.0f,  1.0f,  0xffffffff,  1.0f, 0.0f},
    { 1.0f, -1.0f,  1.0f,  0xffffffff,  1.0f, 1.0f},
    {-1.0f, -1.0f,  1.0f,  0xffffffff,  0.0f, 1.0f},

    // Top
    {-1.0f,  1.0f, -1.0f,  0xffffffff,  0.0f, 1.0f},
    { 1.0f,  1.0f, -1.0f,  0xffffffff,  1.0f, 1.0f},
    { 1.0f,  1.0f,  1.0f,  0xffffffff,  1.0f, 0.0f},
    {-1.0f,  1.0f,  1.0f,  0xffffffff,  0.0f, 0.0f},

    // Bottom
    { 1.0f, -1.0f, -1.0f,  0xffffffff,  0.0f, 0.0f},
    {-1.0f, -1.0f, -1.0f,  0xffffffff,  1.0f, 0.0f},
    {-1.0f, -1.0f,  1.0f,  0xffffffff,  1.0f, 1.0f},
    { 1.0f, -1.0f,  1.0f,  0xffffffff,  0.0f, 1.0f},
};

D3DXMATRIX g_World, g_WorldSph, g_WorldSpin;

void CGlobals::InitSphVtx()
{
    int iSph = 0;
    float fDAngY = D3DX_PI / (float)SPH_NUM;
    float fDAngX = (2 * D3DX_PI) / (float)(SPH_NUM*2);
    float fDAngY0 = fDAngY;
    float fDAngY1 = fDAngY0 + fDAngY;

    // make middle
    int x, y;
    for (y = 0; y < (SPH_NUM-2); y++) 
    {
		float y0, y1, ang0, ang1;

        y0 = (float)cos(fDAngY0);
        y1 = (float)cos(fDAngY1);
        ang0 = (float)sin(fDAngY0);
        ang1 = (float)sin(fDAngY1);

        for (x = 0; x < (SPH_NUM*2); x++) 
        {
            float fDAngX0 = (float)x*fDAngX;
            float fDAngX1;
            if (x == (SPH_NUM*2-1))
                fDAngX1 = 0.0f;
            else
                fDAngX1 = (float)(x+1)*fDAngX;

            float x00 = ang0*(float)sin(fDAngX0);
            float x01 = ang0*(float)sin(fDAngX1);
            float x10 = ang1*(float)sin(fDAngX0);
            float x11 = ang1*(float)sin(fDAngX1);

            float z00 = ang0*(float)cos(fDAngX0);
            float z01 = ang0*(float)cos(fDAngX1);
            float z10 = ang1*(float)cos(fDAngX0);
            float z11 = ang1*(float)cos(fDAngX1);

            m_SphVtx[iSph].v.x = SPH_RADIUS*x00;  m_SphVtx[iSph].v.y = SPH_RADIUS*y0;  m_SphVtx[iSph].v.z = SPH_RADIUS*z00;
            m_SphVtx[iSph].v.nx = x00; m_SphVtx[iSph].v.ny = y0; m_SphVtx[iSph].v.nz = z00;
            iSph++;

            m_SphVtx[iSph].v.x = SPH_RADIUS*x10;  m_SphVtx[iSph].v.y = SPH_RADIUS*y1;  m_SphVtx[iSph].v.z = SPH_RADIUS*z10;
            m_SphVtx[iSph].v.nx = x10; m_SphVtx[iSph].v.ny = y1; m_SphVtx[iSph].v.nz = z10;
            iSph++;

            m_SphVtx[iSph].v.x = SPH_RADIUS*x11;  m_SphVtx[iSph].v.y = SPH_RADIUS*y1;  m_SphVtx[iSph].v.z = SPH_RADIUS*z11;
            m_SphVtx[iSph].v.nx = x11; m_SphVtx[iSph].v.ny = y1; m_SphVtx[iSph].v.nz = z11;
            iSph++;


            m_SphVtx[iSph].v.x = SPH_RADIUS*x00;  m_SphVtx[iSph].v.y = SPH_RADIUS*y0;  m_SphVtx[iSph].v.z = SPH_RADIUS*z00;
            m_SphVtx[iSph].v.nx = x00; m_SphVtx[iSph].v.ny = y0; m_SphVtx[iSph].v.nz = z00;
            iSph++;

            m_SphVtx[iSph].v.x = SPH_RADIUS*x11;  m_SphVtx[iSph].v.y = SPH_RADIUS*y1;  m_SphVtx[iSph].v.z = SPH_RADIUS*z11;
            m_SphVtx[iSph].v.nx = x11; m_SphVtx[iSph].v.ny = y1; m_SphVtx[iSph].v.nz = z11;
            iSph++;

            m_SphVtx[iSph].v.x = SPH_RADIUS*x01;  m_SphVtx[iSph].v.y = SPH_RADIUS*y0;  m_SphVtx[iSph].v.z = SPH_RADIUS*z01;
            m_SphVtx[iSph].v.nx = x01; m_SphVtx[iSph].v.ny = y0; m_SphVtx[iSph].v.nz = z01;
            iSph++;

        }
        fDAngY0 = fDAngY1;
        fDAngY1 += fDAngY;
    }

    // make top
    fDAngY1 = fDAngY;
    float y1 = (float)cos(fDAngY1);
    float ang1 = (float)sin(fDAngY1);
    for (x = 0; x < (SPH_NUM*2); x++) 
    {
        float fDAngX0 = (float)x*fDAngX;
        float fDAngX1;
        if (x == (SPH_NUM*2-1))
            fDAngX1 = 0.0f;
        else
            fDAngX1 = (float)(x+1)*fDAngX;

        float x10 = ang1*(float)sin(fDAngX0);
        float x11 = ang1*(float)sin(fDAngX1);

        float z10 = ang1*(float)cos(fDAngX0);
        float z11 = ang1*(float)cos(fDAngX1);

        m_SphVtx[iSph].v.x = 0.0f;  m_SphVtx[iSph].v.y = SPH_RADIUS;  m_SphVtx[iSph].v.z = 0.0f;
        m_SphVtx[iSph].v.nx = 0.0f; m_SphVtx[iSph].v.ny = 1.0f; m_SphVtx[iSph].v.nz = 0.0f;
        iSph++;

        m_SphVtx[iSph].v.x = SPH_RADIUS*x10;  m_SphVtx[iSph].v.y = SPH_RADIUS*y1;  m_SphVtx[iSph].v.z = SPH_RADIUS*z10;
        m_SphVtx[iSph].v.nx = x10; m_SphVtx[iSph].v.ny = y1; m_SphVtx[iSph].v.nz = z10;
        iSph++;

        m_SphVtx[iSph].v.x = SPH_RADIUS*x11;  m_SphVtx[iSph].v.y = SPH_RADIUS*y1;  m_SphVtx[iSph].v.z = SPH_RADIUS*z11;
        m_SphVtx[iSph].v.nx = x11; m_SphVtx[iSph].v.ny = y1; m_SphVtx[iSph].v.nz = z11;
        iSph++;
    }

    // make bottom
    fDAngY1 = fDAngY0;          // remember last value used, so there are no cracks
    y1 = (float)cos(fDAngY1);
    ang1 = (float)sin(fDAngY1);
    for (x = 0; x < (SPH_NUM*2); x++) 
    {
        float fDAngX0 = (float)x*fDAngX;
        float fDAngX1;
        if (x == (SPH_NUM*2-1))
            fDAngX1 = 0.0f;
        else
            fDAngX1 = (float)(x+1)*fDAngX;

        // to keep the same orientation
        float x11 = ang1*(float)sin(fDAngX0);
        float x10 = ang1*(float)sin(fDAngX1);

        float z11 = ang1*(float)cos(fDAngX0);
        float z10 = ang1*(float)cos(fDAngX1);

        m_SphVtx[iSph].v.x = 0.0f;  m_SphVtx[iSph].v.y = -SPH_RADIUS;  m_SphVtx[iSph].v.z = 0.0f;
        m_SphVtx[iSph].v.nx = 0.0f; m_SphVtx[iSph].v.ny = -1.0f; m_SphVtx[iSph].v.nz = 0.0f;
        iSph++;

        m_SphVtx[iSph].v.x = SPH_RADIUS*x10;  m_SphVtx[iSph].v.y = SPH_RADIUS*y1;  m_SphVtx[iSph].v.z = SPH_RADIUS*z10;
        m_SphVtx[iSph].v.nx = x10; m_SphVtx[iSph].v.ny = y1; m_SphVtx[iSph].v.nz = z10;
        iSph++;

        m_SphVtx[iSph].v.x = SPH_RADIUS*x11;  m_SphVtx[iSph].v.y = SPH_RADIUS*y1;  m_SphVtx[iSph].v.z = SPH_RADIUS*z11;
        m_SphVtx[iSph].v.nx = x11; m_SphVtx[iSph].v.ny = y1; m_SphVtx[iSph].v.nz = z11;
        iSph++;
    }
    if (iSph != SPH_VTXS)
    {
        MessageBox(NULL,"Sphere Vtx Count","D3DX Error",MB_OK);
    }
} // CGlobals::InitSphVtx

CGlobals::CGlobals()
{
    m_bD3DXReady        = FALSE;
    m_bIsFullscreen     = FALSE;
    m_pD3DDev           = NULL;
    m_pDD               = NULL;
    m_pD3D              = NULL;
    m_pD3DX             = NULL;
    m_bActive           = !m_bIsFullscreen;


    // Get media path from registry
    HKEY key;
    m_szPath[0] = '\0';

    if(ERROR_SUCCESS == RegOpenKeyEx(HKEY_LOCAL_MACHINE,
        "Software\\Microsoft\\DirectX", 0, KEY_READ, &key))
    {
        DWORD dwType;
        DWORD dwSize = sizeof(m_szPath);

        if(ERROR_SUCCESS == RegQueryValueEx( key, 
            "DXSDK Samples Path", NULL, &dwType, (BYTE*) m_szPath, &dwSize))
        {
            if(REG_SZ == dwType)
                strcat(m_szPath, "\\D3DX\\Media\\");
            else
                m_szPath[0] = '\0';
        }

        RegCloseKey(key);
    }

} // CGlobals::CGlobals

CGlobals::~CGlobals()
{
    g_p->UnInit();
} // CGlobals::~CGlobals

void InterpretError(HRESULT hr)
{
    char errStr[256];

    if(D3DXERR_NODIRECT3DDEVICEAVAILABLE  == hr) 
    {
        strcpy(errStr, "D3DXERR_NODIRECT3DDEVICEAVAILABLE\n\n"
                       "No suitable 3D device found.  "
                       "Try enabling the reference rasterizer.");
    }
    else
    {
        D3DXGetErrorString(hr, 256, errStr);
    }

    MessageBox(NULL, errStr, "D3DX Error", MB_OK);
} // InterpretError

void CGlobals::PauseDrawing()
{
    g_p->m_bActive = FALSE;
    if (m_bIsFullscreen)
        ShowCursor(TRUE);
} // CGlobals::PauseDrawing

void CGlobals::RestartDrawing()
{
    g_p->m_bActive = TRUE;
    if (m_bIsFullscreen)
        ShowCursor(FALSE);
} // CGlobals::RestartDrawing

HRESULT CGlobals::InitD3DX()
{
    HRESULT hr;

    if(FAILED(hr = D3DXInitialize()))
        return hr;

    // Look for fastest device which supports cubic environment mapping
    DWORD dwDevice;
    DWORD dwDeviceCount = D3DXGetDeviceCount();

    D3DX_DEVICEDESC dev;

    dev.deviceIndex = D3DX_DEFAULT;
    dev.hwLevel     = D3DX_DEFAULT;
    dev.onPrimary   = TRUE;


    for(dwDevice = 0; dwDevice < dwDeviceCount; dwDevice++)
    {
        D3DDEVICEDESC7 d3dDesc;
        D3DX_DEVICEDESC devDesc;

        if(FAILED(D3DXGetDeviceCaps(dwDevice, NULL, &d3dDesc, NULL, NULL)))
            continue;

        if(!(d3dDesc.dpcTriCaps.dwTextureCaps & D3DPTEXTURECAPS_CUBEMAP))
            continue;

        if(FAILED(D3DXGetDeviceDescription(dwDevice, &devDesc)))
            continue;

        if( D3DX_DEFAULT == dev.hwLevel || 
            dev.hwLevel > devDesc.hwLevel ||
            dev.hwLevel == devDesc.hwLevel && devDesc.onPrimary )
        {
            dev = devDesc;
        }
    }

    if(D3DX_DEFAULT == dev.hwLevel)
        return D3DXERR_NODIRECT3DDEVICEAVAILABLE;



    // D3DX Initialization
    hr = D3DXCreateContextEx(  
                dev.hwLevel,            // D3DX device
                m_bIsFullscreen ? D3DX_CONTEXT_FULLSCREEN:0, // flags
                m_hwndMain,
                NULL,                   // focusWnd
                D3DX_DEFAULT,           // colorbits
                D3DX_DEFAULT,           // alphabits
                D3DX_DEFAULT,           // numdepthbits
                0,                      // numstencilbits
                D3DX_DEFAULT,           // numbackbuffers
                m_bIsFullscreen? FULLSCREEN_WIDTH:D3DX_DEFAULT,  // width
                m_bIsFullscreen? FULLSCREEN_HEIGHT:D3DX_DEFAULT, // height
                D3DX_DEFAULT,           // refresh rate
                &m_pD3DX                // returned D3DX interface
                );

    if (FAILED(hr))
        return hr;

    m_pD3DDev = m_pD3DX->GetD3DDevice();
    if (m_pD3DDev == NULL)
        return E_FAIL;

    m_pD3D = m_pD3DX->GetD3D();
    if (m_pD3D == NULL)
        return E_FAIL;

    m_pDD = m_pD3DX->GetDD();
    if( m_pDD == NULL )
        return E_FAIL;

    m_bD3DXReady = TRUE;
    return InitRenderer();
} // CGlobals::InitD3DX

HRESULT CGlobals::InitRenderer()
{
    HRESULT hr;

    if (!m_bD3DXReady)
        return E_FAIL;

    hr = m_pD3DX->SetClearColor(D3DRGBA(1.0,1.0,0.0,0.0));
    if (FAILED(hr))
        return hr;

    hr = m_pD3DDev->SetRenderState(D3DRENDERSTATE_ZENABLE, TRUE); 
    if (FAILED(hr)) 
        return hr;
    hr = m_pD3DDev->SetRenderState(D3DRENDERSTATE_ZWRITEENABLE, TRUE); 
    if (FAILED(hr)) 
        return hr;
    hr = m_pD3DDev->SetRenderState(D3DRENDERSTATE_ZFUNC, D3DCMP_LESS);
    if (FAILED(hr)) 
        return hr;
    hr = m_pD3DDev->SetRenderState(D3DRENDERSTATE_CULLMODE, D3DCULL_CCW);
    if (FAILED(hr)) 
        return hr;
    hr = m_pD3DDev->SetRenderState(D3DRENDERSTATE_WRAP0, 0); 
    if (FAILED(hr)) 
        return hr;
    hr = m_pD3DDev->SetRenderState(D3DRENDERSTATE_TEXTUREPERSPECTIVE, TRUE); 
    if (FAILED(hr)) 
        return hr;
    hr = m_pD3DDev->SetRenderState(D3DRENDERSTATE_ANTIALIAS, D3DANTIALIAS_SORTINDEPENDENT); 
    if (FAILED(hr)) 
        return hr;
    hr = m_pD3DDev->SetRenderState(D3DRENDERSTATE_DITHERENABLE, FALSE); 
    if (FAILED(hr)) 
        return hr;
    hr = m_pD3DDev->SetRenderState(D3DRENDERSTATE_SPECULARENABLE, FALSE);
    if (FAILED(hr)) 
        return hr;
    hr = m_pD3DDev->SetRenderState(D3DRENDERSTATE_LIGHTING, FALSE);
    if (FAILED(hr)) 
        return hr;


    // Grab some textures that will form the walls and the pieces of the
    // cube map

    for (int i = 0; i < 6; i++)
    {
        char pszTex[256];
        sprintf(pszTex, "%senv%d.bmp", m_szPath, i);
        hr = D3DXCreateTextureFromFile( m_pD3DDev,
                                        0,
                                        0,  // use texture w
                                        0,  // use texture h
                                        NULL,
                                        NULL,
                                        &m_pTex[i],
                                        0,
                                        pszTex,
                                        D3DX_FT_DEFAULT);

        if (FAILED(hr))
            return hr;
    }

    // Create the cube map texture
    DWORD w = 256, h = 256;
    D3DX_SURFACEFORMAT sf = D3DX_SF_A8R8G8B8;

    hr = D3DXCreateCubeMapTexture(
                    m_pD3DDev,
                    0,                              // flags
                    DDSCAPS2_CUBEMAP_ALLFACES,      // faces
                    0,                              // colors for empty faces
                    &w,                             // width
                    &h,                             // height
                    &sf,                            // surface format
                    NULL,                           // palette
                    &m_pEnvTex,                     // resulting surface
                    NULL);                          // number of mipmap levels
    if (FAILED(hr)) 
        return hr;

    // Load the faces of the cube-map
    DDSCAPS2 ddsCaps;
    ZeroMemory(&ddsCaps, sizeof(ddsCaps));
    for (i = 0; i < 6; i++)
    {
        DWORD FacesArray[] = 
        {
            DDSCAPS2_CUBEMAP_POSITIVEX,
            DDSCAPS2_CUBEMAP_NEGATIVEX,
            DDSCAPS2_CUBEMAP_POSITIVEY, 
            DDSCAPS2_CUBEMAP_NEGATIVEY,
            DDSCAPS2_CUBEMAP_POSITIVEZ, 
            DDSCAPS2_CUBEMAP_NEGATIVEZ
        };

        // Maps file indexes to the various faces of
        // the cube-map
        DWORD FileIndex[] =
        {
            2, 0, 5, 4, 1, 3
        };

        // Find the right face
        LPDIRECTDRAWSURFACE7 pFace;
        if (i != 0)
        {
            ddsCaps.dwCaps2 = FacesArray[i];
            if (FAILED(hr = m_pEnvTex->GetAttachedSurface(&ddsCaps, &pFace)))
                return hr;
        }
        else
        {
            // Special case zero'th face; because it is the
            // same as the top-level envmap
            pFace = m_pEnvTex;
            pFace->AddRef();
        }

        char pszTex[256];
        sprintf(pszTex, "%senvf%d.bmp", m_szPath, FileIndex[i]);

        // Load it from our copy
        hr = D3DXLoadTextureFromFile(
                        m_pD3DDev,
                        pFace,                      // destination
                        D3DX_DEFAULT,               // all mip levels
                        pszTex,                     // source
                        NULL,                       // entire source
                        NULL,                       // entire destination
                        D3DX_FT_DEFAULT);       

        pFace->Release();
        if (FAILED(hr)) 
            return hr;
    }

    hr = m_pD3DDev->SetTextureStageState(0,  D3DTSS_ADDRESS, D3DTADDRESS_WRAP);
    if (FAILED(hr)) 
        return hr;
    hr = m_pD3DDev->SetTextureStageState(0,  D3DTSS_MAGFILTER, D3DTFG_LINEAR);
    if (FAILED(hr)) 
        return hr;
    hr = m_pD3DDev->SetTextureStageState(0,  D3DTSS_MINFILTER, D3DTFN_LINEAR);
    if (FAILED(hr)) 
        return hr;
    hr = m_pD3DDev->SetTextureStageState(0,  D3DTSS_MIPFILTER, D3DTFP_NONE);
    if (FAILED(hr)) 
        return hr;
    hr = m_pD3DDev->SetTextureStageState(0,  D3DTSS_TEXCOORDINDEX, 0);
    if (FAILED(hr)) 
        return hr;
    hr = m_pD3DDev->SetTextureStageState(0,  D3DTSS_COLOROP, D3DTOP_SELECTARG1);
    if (FAILED(hr)) 
        return hr;
    hr = m_pD3DDev->SetTextureStageState(0,  D3DTSS_COLORARG1, D3DTA_TEXTURE);
    if (FAILED(hr)) 
        return hr;
    hr = m_pD3DDev->SetTextureStageState(0,  D3DTSS_COLORARG2, D3DTA_DIFFUSE);
    if (FAILED(hr)) 
        return hr;
    hr = m_pD3DDev->SetTextureStageState(0,  D3DTSS_ALPHAOP, D3DTOP_SELECTARG1);
    if (FAILED(hr)) 
        return hr;
    hr = m_pD3DDev->SetTextureStageState(0,  D3DTSS_ALPHAARG1, D3DTA_TEXTURE);
    if (FAILED(hr)) 
        return hr;
    hr = m_pD3DDev->SetTextureStageState(1,  D3DTSS_COLOROP, D3DTOP_DISABLE);
    if (FAILED(hr)) 
        return hr;
    hr = m_pD3DDev->SetTexture(1, (LPDIRECTDRAWSURFACE7)NULL);
    if (FAILED(hr)) 
        return hr;
    hr = m_pD3DX->Clear(D3DCLEAR_TARGET|D3DCLEAR_ZBUFFER);
    if (FAILED(hr))
        return hr;

    memset(&m_Material,0,sizeof(D3DMATERIAL7));
    m_Material.diffuse.r = 1.0f;
    m_Material.diffuse.g = 1.0f;
    m_Material.diffuse.b = 1.0f;

    D3DVIEWPORT7 vp;
    m_pD3DDev->GetViewport(&vp);
    float fAspect = (float) vp.dwHeight / (float) vp.dwWidth;

    D3DXMATRIX proj, view;
    D3DXMatrixPerspectiveFovLH(&proj, D3DXToRadian(120.0f), fAspect, 1.0f, 20.0f);
    D3DXMatrixTranslation(&view, 0.0f, 0.0f, 5.0f);
    D3DXMatrixScaling(&g_World, 10.0f, 10.0f, 10.0f);
    g_WorldSph = g_World;
    D3DXVECTOR3 axis(0.0f, 1.0f, 0.0f);
    D3DXMatrixRotationAxis(&g_WorldSpin, &axis, D3DXToRadian(10.0f));
    D3DXMatrixMultiply(&g_World, &g_World, &g_WorldSpin);
    D3DXMatrixMultiply(&g_World, &g_World, &g_WorldSpin);
    D3DXMatrixMultiply(&g_World, &g_World, &g_WorldSpin);
    D3DXMatrixMultiply(&g_World, &g_World, &g_WorldSpin);
    D3DXMatrixMultiply(&g_World, &g_World, &g_WorldSpin);

    hr = m_pD3DDev->SetTransform(D3DTRANSFORMSTATE_PROJECTION, (D3DMATRIX *)&proj);
    if (FAILED(hr))
        return hr;

    hr = m_pD3DDev->SetTransform(D3DTRANSFORMSTATE_VIEW,       (D3DMATRIX *)&view);
    if (FAILED(hr))
        return hr;

    hr = m_pD3DDev->SetTransform(D3DTRANSFORMSTATE_WORLD,      (D3DMATRIX *)&g_World);
    if (FAILED(hr))
        return hr;

    InitSphVtx();

    return S_OK;
} // CGlobals::InitRenderer

void CGlobals::ReflectNormals()
{
    SPHVERTEX *pVIn = &m_SphVtx[0];

    D3DXMATRIX InvWorld;
    D3DXMatrixInverse(&InvWorld, NULL, &g_World);
    float enx = InvWorld.m[2][0];
    float eny = InvWorld.m[2][1];
    float enz = InvWorld.m[2][2];
    float InvNorm = 1.0f/(float)sqrt(enx*enx + eny*eny + enz*enz);
    enx *= InvNorm;
    eny *= InvNorm;
    enz *= InvNorm;

    D3DXMATRIX NormMatrix;
    D3DXMatrixTranspose(&NormMatrix, &g_World);

    for (int i = 0; i < SPH_VTXS; i++) 
    {
        D3DXVECTOR4 vin, vout;
        vin.x = pVIn->v.nx; vin.y = pVIn->v.ny; vin.z = pVIn->v.nz; vin.w = 0.0f;
        D3DXVec4Transform(&vout, &vin, &NormMatrix);
        float nx = vout.x;
        float ny = vout.y;
        float nz = vout.z;
        InvNorm = 1.0f/(float)sqrt(nx*nx + ny*ny + nz*nz);
        nx *= InvNorm;
        ny *= InvNorm;
        nz *= InvNorm;
        float NE2 = (enx*nx + eny*ny + enz*nz)*2.0f;
        float vnx = NE2*nx - enx;
        float vny = NE2*ny - eny;
        float vnz = NE2*nz - enz;

        InvNorm = 1.0f/(float)sqrt(vnx*vnx + vny*vny + vnz*vnz);
        vnx *= InvNorm;
        vny *= InvNorm;
        vnz *= InvNorm;

        // reflected normal
        pVIn->v.tu = vnx;
        pVIn->v.tv = vny;
        pVIn->tu2  = vnz;


        pVIn++;
    }
} // CGlobals::ReflectNormals

HRESULT CGlobals::UnInit()
{
    RELEASENULL(m_pD3DDev);
    RELEASENULL(m_pD3D);
    RELEASENULL(m_pDD);
    RELEASENULL(m_pD3DX);
    m_bD3DXReady = FALSE;

    D3DXUninitialize();
    return S_OK;
} // CGlobals::UnInit

HRESULT CGlobals::RenderEnvironment()
{
    HRESULT hr;

    hr = m_pD3DDev->SetRenderState(D3DRENDERSTATE_WRAP0, 0); 
    if (FAILED(hr)) 
        return hr;
    hr = m_pD3DDev->SetTransform(D3DTRANSFORMSTATE_WORLD, (D3DMATRIX *)&g_World); 
    if (FAILED(hr)) 
        return hr;

    for (int i = 0; i < 6; i++) 
    {
        hr = m_pD3DDev->SetTexture(0, m_pTex[i]); 
        if (FAILED(hr)) 
            return hr;

        hr = m_pD3DDev->DrawPrimitive(D3DPT_TRIANGLEFAN, D3DFVF_XYZ | D3DFVF_DIFFUSE | D3DFVF_TEX1,
            &BoxVtx[i*4], 4, 0); 
        if (FAILED(hr)) 
            return hr;
    }
    return D3D_OK;
} // CGlobals::RenderEnvironment

static int bSpinWorld = TRUE;

HRESULT CGlobals::Draw()
{
    HRESULT hr;

    if (!m_bD3DXReady)
    {
        return E_FAIL;
    }
    if (!m_bActive)
    {
        return S_OK;
    }

    hr = m_pD3DDev->BeginScene();
    if (SUCCEEDED(hr))
    {
        m_pD3DX->Clear(D3DCLEAR_TARGET|D3DCLEAR_ZBUFFER);

        RenderEnvironment();
        
        ReflectNormals();

        m_pD3DDev->SetRenderState(D3DRENDERSTATE_WRAP0, D3DWRAP_U | D3DWRAP_V); 
        m_pD3DDev->SetTransform(D3DTRANSFORMSTATE_WORLD, (D3DMATRIX *)&g_WorldSph); 
        m_pD3DDev->SetTexture(0, m_pEnvTex); 
        
        m_pD3DDev->DrawPrimitive(D3DPT_TRIANGLELIST, SPHVERTEX_FVF, &m_SphVtx[0], SPH_VTXS, 0x0);

        m_pD3DDev->EndScene();
    }

    hr = m_pD3DX->UpdateFrame( 0 );
    if ( hr == DDERR_SURFACELOST || hr == DDERR_SURFACEBUSY )
        hr = HandleModeChanges();

    if (bSpinWorld)
    {
        D3DXMatrixMultiply(&g_World, &g_World, &g_WorldSpin);
    }

    return hr;
} // CGlobals::Draw

HRESULT CGlobals::HandleModeChanges()
{
    HRESULT hr;
    hr = m_pDD->TestCooperativeLevel();

    if( SUCCEEDED( hr ) || DDERR_WRONGMODE == hr)
    {
        UnInit();

        if(FAILED(hr = InitD3DX()))
            return hr;
    }
    else if( hr != DDERR_EXCLUSIVEMODEALREADYSET &&
             hr != DDERR_NOEXCLUSIVEMODE )
    {
        // Busted!!
        return hr;
    }

    return S_OK;
}

LRESULT CALLBACK WndProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
    switch(uMsg)
    {
    case WM_ACTIVATEAPP:
        {
            if (!g_p)
                break;

            if (g_p->m_bIsFullscreen)
            {
                if ((BOOL)wParam)
                    g_p->RestartDrawing();
                else
                    g_p->PauseDrawing();
            }
        }
        break;
    case WM_CREATE:
        break;
    case WM_CLOSE:
        PostQuitMessage(0);
        break;
    case WM_SIZE:
        if (g_p
            && g_p->m_bD3DXReady
            && !g_p->m_bIsFullscreen
           )
        {
            HRESULT hr;

            if (wParam == SIZE_MINIMIZED)
            {
                g_p->m_bActive = FALSE;
                break;
            }
            else if (LOWORD(lParam)>0 && HIWORD(lParam)>0)
            {
                if (FAILED(hr = g_p->m_pD3DX->Resize(LOWORD(lParam),HIWORD(lParam))))
                {
                    InterpretError(hr);
                    g_p->m_bD3DXReady = FALSE;
                    PostQuitMessage(0);
                }
            }
            g_p->m_bActive = TRUE;

        }
        break;
    case WM_KEYDOWN:
        switch(wParam)
        {
        case VK_ESCAPE:
        {
            PostQuitMessage(0);
        }
        }
        break;
    case WM_COMMAND:
         if (1 == HIWORD(wParam))
         {
             switch (LOWORD(wParam))
             {
             case IDM_FULLSCREEN:
                 if (g_p && g_p->m_bD3DXReady)
                 {
                     HRESULT hr;
                     g_p->m_bIsFullscreen = ! g_p->m_bIsFullscreen;
                     g_p->m_bD3DXReady = FALSE;

                     if (g_p->m_bIsFullscreen)
                     {
                         // going to fullscreen
                         GetWindowRect(hwnd, &g_p->m_rWindowedRect);
                     }
                     ShowCursor(!(g_p->m_bIsFullscreen));
                     hr = g_p->m_pD3DX->Clear(D3DCLEAR_TARGET|D3DCLEAR_ZBUFFER);
                     if (FAILED(hr))
                     {
                         InterpretError(hr);
                         g_p->PauseDrawing();
                         PostQuitMessage(-1);
                         break;
                     }
                     g_p->UnInit();

                     if (!g_p->m_bIsFullscreen)
                     {
                         RECT& r = g_p->m_rWindowedRect;
                         SetWindowPos(hwnd, HWND_NOTOPMOST,
                                     r.left,
                                     r.top,
                                     r.right-r.left,
                                     r.bottom-r.top,
                                     SWP_NOACTIVATE);
                     }

                     hr = g_p->InitD3DX();
                     if (FAILED(hr))
                     {
                         InterpretError(hr);
                         g_p->PauseDrawing();
                         PostQuitMessage(-1);
                         break;
                     }
                 }
                 break;
             }
         }
         break;
    default:
        break;
    }

    return DefWindowProc(hwnd,uMsg,wParam,lParam);

} // WndProc

int PASCAL WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance,
                   LPSTR lpszCmdLine, int nCmdShow)
{
    HRESULT     hr;
    MSG         msg;
    WNDCLASS    wc;
    HACCEL      hAccelApp;
    HCURSOR     hcur = NULL;
    int         ret = 0;

    g_p = new CGlobals; // set up our data AFTER starting up d3dx!
    if (!g_p)
    {
        ret = -1;
        goto Exit;
    }

    // Register the window class for the main window.

    if (!hPrevInstance)
    {
        hcur = CopyCursor(LoadCursor(NULL, IDC_ARROW));

        wc.style = 0;
        wc.lpfnWndProc = (WNDPROC) WndProc;
        wc.cbClsExtra = 0;
        wc.cbWndExtra = 0;
        wc.hInstance = hInstance;
        wc.hIcon = LoadIcon(hInstance, MAKEINTRESOURCE(IDI_APP_ICON));
        wc.hCursor = hcur;
        wc.hbrBackground = (HBRUSH)GetStockObject(BLACK_BRUSH);
        wc.lpszMenuName =  NULL;
        wc.lpszClassName = NAME_OF_THE_APP;

        if (!RegisterClass(&wc))
        {
            ret = -1;
            goto Exit;
        }
    }

    // Create the window
    g_p->m_hwndMain = CreateWindow(NAME_OF_THE_APP,
                                   NAME_OF_THE_APP,
                                   WS_OVERLAPPEDWINDOW,
                                   CW_USEDEFAULT,
                                   CW_USEDEFAULT,
                                   400,
                                   400,
                                   (HWND) NULL,
                                   (HMENU) NULL,
                                   hInstance,
                                   (LPVOID) NULL);

    if (!g_p->m_hwndMain)
    {
        ret = -1;
        goto Exit;
    }


    // Hide the cursor if necessary
    if (g_p->m_bIsFullscreen)
    {
        ShowCursor(FALSE);
    }

    // Show the window
    ShowWindow(g_p->m_hwndMain, nCmdShow);
    UpdateWindow(g_p->m_hwndMain);

    hAccelApp = LoadAccelerators(hInstance, MAKEINTRESOURCE(IDR_APP_ACCELERATOR));
    if (!hAccelApp)
    {
        ret = -1;
        goto Exit;
    }

    // Initialize D3DX
    hr = g_p->InitD3DX();
    if (FAILED(hr))
    {
        InterpretError(hr);
        ret = -1;
        goto Exit;
    }

    hr = g_p->InitRenderer();
    if (FAILED(hr))
    {
        InterpretError(hr);
        ret = -1;
        goto Exit;
    }

    BOOL bGotMsg;
    PeekMessage(&msg, NULL, 0U, 0U, PM_NOREMOVE);
    while (WM_QUIT != msg.message )
    {
        bGotMsg = PeekMessage(&msg, NULL, 0U, 0U, PM_REMOVE);

        if (bGotMsg)
        {
            if (!TranslateAccelerator(g_p->m_hwndMain, hAccelApp, &msg))
            {
                TranslateMessage(&msg);
                DispatchMessage(&msg);
            }
        }
        else
        {
            if (g_p->m_bActive)
            {
                hr = g_p->Draw();
                if (FAILED(hr))
                {
                    InterpretError(hr);
                    g_p->m_bD3DXReady = FALSE;
                    PostQuitMessage(-1);
                }
            }
            else
            {
                WaitMessage();
            }
        }
    }
    delete g_p; // clean up our data BEFORE shutting down d3dx!

Exit:
    if(hcur)
        DestroyCursor(hcur);
    
    return ret;
} // WinMain
