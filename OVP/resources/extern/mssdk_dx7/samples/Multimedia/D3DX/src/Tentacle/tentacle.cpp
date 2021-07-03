//
// Tentacle Demo
// Copyright (c) 1999 Microsoft Corporation. All rights reserved.
//

#include "pch.hpp"


//////////////////////////////////////////////////////////////////////////////
// Globals ///////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////


//
// Window and context
//

const char *g_szAppName = "D3DX - Tentacle";

HINSTANCE         g_hAppInstance = NULL;
HWND              g_hwnd         = NULL;
ID3DXContext     *g_pD3DX        = NULL;
LPDIRECTDRAW7     g_pDD          = NULL;
LPDIRECT3D7       g_pD3D         = NULL;
LPDIRECT3DDEVICE7 g_pD3DDevice   = NULL;

D3DDEVICEDESC7    g_D3DDeviceDesc;



//
// Textures
//

char g_szPath[512];

enum {
    TEX_GROUND, TEX_TENTACLE,
    TEX_FLARE0, TEX_FLARE1, TEX_FLARE2, TEX_FLARE3,
    NUM_TEXTURES
};

char *g_szTexName[NUM_TEXTURES] = {
    "ground1.bmp", "tentacle.bmp",
    "flare0.bmp", "flare1.bmp", "flare2.bmp", "flare3.bmp"
};

LPDIRECTDRAWSURFACE7 g_ppTex[NUM_TEXTURES];


//
// Light source
//

D3DXCOLOR g_colorLight(1.0f, 0.95f, 0.8f, 1.0f);
D3DXVECTOR4 g_vecLight(0.1f, -1.0f, -1.5f, 0.0f);


//
// Background
//

D3DXCOLOR g_colorClear (0.4f, 0.5f, 0.8f, 1.0f);


//
// Ground plane
//

BOOL g_bDrawGround = TRUE;
CGround *g_pGround = NULL;

D3DXPLANE g_planeGround(0.0f, 1.0f, 0.0f, 0.0f);
D3DXCOLOR g_colorGround(0.17f, 0.35f, 0.7f, 0.75f);


//
// Tentacle
//

BOOL g_bWireframe      = FALSE;
BOOL g_bDrawTentacle   = TRUE;
BOOL g_bCapsTentacle   = TRUE;
BOOL g_bDrawReflection = TRUE;
BOOL g_bCapsReflection = TRUE;

enum {
    TCL_BASE,
    TCL_BASE_A, TCL_BASE_A1, TCL_ARM_A, TCL_ARM_A1, TCL_TIP_A,
    TCL_BASE_B, TCL_BASE_B1, TCL_ARM_B, TCL_TIP_B,
    NUM_TENTACLES
};

CTentacle *g_ppTentacle[NUM_TENTACLES];


//
// Lens Flare
//

BOOL g_bDrawLensFlare = TRUE;
BOOL g_bCapsLensFlare = TRUE;
CLensFlare *g_pLensFlare = NULL;


//
// User interface
//

BOOL  g_bReady           = FALSE;
BOOL  g_bActive          = TRUE;
BOOL  g_bFullScreen      = FALSE;
DWORD g_dwHwLevel        = D3DX_DEFAULT;
DWORD g_dwNumBackBuffers = 2;


float g_fSpeed        = 5.0f;
float g_fAngularSpeed = 1.0f;

BOOL g_bMouseCaptured = FALSE;
WORD g_wMouseButtons  = 0;

int g_nLastMouseX = 0;
int g_nLastMouseY = 0;

BYTE g_bKey[256];

D3DXVECTOR3 g_vecVelocity(0.0f, 0.0f, 0.0f);
D3DXVECTOR3 g_vecAngularVelocity(0.0f, 0.0f, 0.0f);

D3DXMATRIX g_matView;
D3DXMATRIX g_matPosition;
D3DXMATRIX g_matProjection;

D3DXMATRIX g_matIdentity
    (1.0f, 0.0f, 0.0f, 0.0f,
     0.0f, 1.0f, 0.0f, 0.0f,
     0.0f, 0.0f, 1.0f, 0.0f,
     0.0f, 0.0f, 0.0f, 1.0f);


//
// Timer
//

BOOL g_bDrawFramerate = TRUE;
BOOL g_bDrawHelp = FALSE;
CFrameTimer *g_pFrameTimer = NULL;






//////////////////////////////////////////////////////////////////////////////
// Implementation ////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////




// ---------------------------------------------------------------------------
//
// InterpretError
//
// This function puts up a message box containing the D3DX description of an
// HRESULT error code.
//
// ---------------------------------------------------------------------------

void InterpretError(HRESULT hr)
{
    char errStr[256];

    if(D3DXERR_CAPSNOTSUPPORTED == hr) 
    {
        strcpy(errStr, "D3DXERR_CAPSNOTSUPPORTED\n\n"
                       "This device lacks required capabilities.  "
                       "Try using the reference rasterizer.");
    }
    else
    {
        D3DXGetErrorString(hr, 256, errStr);
    }

    MessageBox(NULL, errStr, "D3DX Error", MB_OK);
}




// ---------------------------------------------------------------------------
//
// CreateScene
//
// ---------------------------------------------------------------------------

HRESULT CreateScene()
{
    HRESULT hr;

    //
    // Create ground
    // 

    if(!(g_pGround = new CGround))
        return E_OUTOFMEMORY;

    if(FAILED(hr = g_pGround->Initialize(256.0f, 256.0f, 10.0f, g_colorGround)))
        return hr;


    //
    // Create Tentacles
    //

    for(UINT i = 0; i < NUM_TENTACLES; i++)
        if(!(g_ppTentacle[i] = new CTentacle))
            return E_OUTOFMEMORY;

    // Base
    g_ppTentacle[TCL_BASE]   ->Initialize( 0.5f,   0.3f,   1.5f,   0.0f,   0.3f,  15,  12);
    g_ppTentacle[TCL_BASE]   ->Twist(D3DX_PI * 0.05f, D3DX_PI * 0.05f, D3DX_PI * 0.05f);

    g_ppTentacle[TCL_BASE]   ->AddChild(g_ppTentacle[TCL_BASE_A]);
    g_ppTentacle[TCL_BASE]   ->AddChild(g_ppTentacle[TCL_BASE_B]);


    // Arm A
    g_ppTentacle[TCL_BASE_A] ->Initialize(  0.3f,   0.3f,  0.25f,   0.3f,  0.35f,   4,  12);
    g_ppTentacle[TCL_BASE_A1]->Initialize(  0.3f,   0.3f,   0.3f,  0.35f,   0.4f,   2,  12);
    g_ppTentacle[TCL_ARM_A]  ->Initialize(  0.3f, 0.187f,  1.75f,   0.4f,   0.7f,  15,  12);
    g_ppTentacle[TCL_ARM_A1] ->Initialize(0.187f,   0.1f,  1.75f,   0.7f,  0.99f,  15,  12);
    g_ppTentacle[TCL_TIP_A]  ->Initialize(  0.1f,   0.0f,  0.25f,  0.99f,   1.0f,  10,  12);

    g_ppTentacle[TCL_BASE_A] ->Twist(0.0f, 0.0f, D3DX_PI *  0.25f);
    g_ppTentacle[TCL_ARM_A]  ->Twist(0.0f, 0.0f, D3DX_PI * -0.30f);
    g_ppTentacle[TCL_ARM_A1] ->Twist(0.0f, 0.0f, D3DX_PI * -0.30f);

    g_ppTentacle[TCL_BASE_A] ->AddChild(g_ppTentacle[TCL_BASE_A1]);
    g_ppTentacle[TCL_BASE_A1]->AddChild(g_ppTentacle[TCL_ARM_A]);
    g_ppTentacle[TCL_ARM_A]  ->AddChild(g_ppTentacle[TCL_ARM_A1]);
    g_ppTentacle[TCL_ARM_A1] ->AddChild(g_ppTentacle[TCL_TIP_A]);


    // Arm B
    g_ppTentacle[TCL_BASE_B] ->Initialize(  0.3f,   0.3f,  0.25f,   0.3f,  0.35f,   4,  12);
    g_ppTentacle[TCL_BASE_B1]->Initialize(  0.3f,   0.3f,   0.3f,  0.35f,   0.4f,   2,  12);
    g_ppTentacle[TCL_ARM_B]  ->Initialize(  0.3f, 0.075f,   3.5f,   0.4f,  0.99f,  30,  12);
    g_ppTentacle[TCL_TIP_B]  ->Initialize(0.075f,   0.0f,  0.25f,  0.99f,   1.0f,  10,  12);

    g_ppTentacle[TCL_BASE_B] ->Twist(0.0f, 0.0f, D3DX_PI * -0.25f);
    g_ppTentacle[TCL_ARM_B]  ->Twist(0.0f, 0.0f, D3DX_PI * -0.20f);

    g_ppTentacle[TCL_BASE_B] ->AddChild(g_ppTentacle[TCL_BASE_B1]);
    g_ppTentacle[TCL_BASE_B1]->AddChild(g_ppTentacle[TCL_ARM_B]);
    g_ppTentacle[TCL_ARM_B]  ->AddChild(g_ppTentacle[TCL_TIP_B]);



    //
    // Create CLensFlare
    //

    if(!(g_pLensFlare = new CLensFlare))
        return E_OUTOFMEMORY;

    if(FAILED(hr = g_pLensFlare->Initialize(6, 95.0f)))
        return hr;

    g_pLensFlare->SetLightColor(g_colorLight);
    g_pLensFlare->SetLightPosition(g_vecLight);


    //
    // Frame timer
    //

    if(!(g_pFrameTimer = new CFrameTimer()))
        return E_OUTOFMEMORY;

    g_pFrameTimer->Start(30.0f);

    
    
    //
    // Misc stuff
    //


    D3DXMatrixTranslation(&g_matView, 0.0f, 0.0f, -20.0f);
    D3DXMatrixTranslation(&g_matPosition, 0.0f, 0.0f, 20.0f);
    D3DXMatrixPerspectiveFov(&g_matProjection, D3DXToRadian(60.0f), 3.0f / 4.0f, 0.1f, 100.0f);

    memset(g_bKey, 0x00, 256);

    return S_OK;
}


// ---------------------------------------------------------------------------
//
// ReleaseScene
//
// ---------------------------------------------------------------------------

HRESULT ReleaseScene()
{
    if(g_pGround)
        delete g_pGround;

    if(g_ppTentacle[TCL_BASE])
        delete g_ppTentacle[TCL_BASE];

    if(g_pLensFlare)
        delete g_pLensFlare;

    if(g_pFrameTimer)
        delete g_pFrameTimer;

    return S_OK;
}


// ---------------------------------------------------------------------------
//
// CreateContext
//
// ---------------------------------------------------------------------------

HRESULT CreateContext()
{
    HRESULT hr;

    //
    // Initialize D3DX
    //

    if(FAILED(hr = D3DXInitialize()))
        return hr;

    if(g_bFullScreen)
    {
        hr = D3DXCreateContextEx(g_dwHwLevel, D3DX_CONTEXT_FULLSCREEN,  g_hwnd, NULL, D3DX_DEFAULT, 0,
            D3DX_DEFAULT, 0, g_dwNumBackBuffers, 640, 480, D3DX_DEFAULT, &g_pD3DX);
    }
    else
    {
        hr = D3DXCreateContextEx(g_dwHwLevel, 0, g_hwnd, NULL, D3DX_DEFAULT, 0,
            D3DX_DEFAULT, 0, g_dwNumBackBuffers, D3DX_DEFAULT, D3DX_DEFAULT, D3DX_DEFAULT, &g_pD3DX);
    }

    if(FAILED(hr))
        return hr;

    if(!(g_pDD = g_pD3DX->GetDD()))
        return E_FAIL;

    if(!(g_pD3D = g_pD3DX->GetD3D()))
        return E_FAIL;

    if(!(g_pD3DDevice = g_pD3DX->GetD3DDevice()))
        return E_FAIL;


    //
    // Grok device caps
    //

    memset(&g_D3DDeviceDesc, 0x00, sizeof(D3DDEVICEDESC7));
    g_pD3DDevice->GetCaps(&g_D3DDeviceDesc);

    g_bCapsLensFlare  = (g_D3DDeviceDesc.dpcTriCaps.dwSrcBlendCaps & D3DPBLENDCAPS_ONE) &&
                        (g_D3DDeviceDesc.dpcTriCaps.dwDestBlendCaps & D3DPBLENDCAPS_INVSRCCOLOR) &&
                        (g_D3DDeviceDesc.dpcTriCaps.dwShadeCaps & D3DPSHADECAPS_ALPHAFLATBLEND);

    g_bCapsReflection = (g_D3DDeviceDesc.dpcTriCaps.dwSrcBlendCaps & D3DPBLENDCAPS_SRCALPHA) &&
                        (g_D3DDeviceDesc.dpcTriCaps.dwDestBlendCaps & D3DPBLENDCAPS_INVSRCALPHA) &&
                        (g_D3DDeviceDesc.dpcTriCaps.dwShadeCaps & D3DPSHADECAPS_ALPHAFLATBLEND) &&
                        (g_D3DDeviceDesc.wMaxUserClipPlanes >= 1);

    g_bCapsTentacle   = (g_D3DDeviceDesc.wMaxVertexBlendMatrices >= 2);

    if(!g_bCapsTentacle)
        return D3DXERR_CAPSNOTSUPPORTED;


    //
    // Setup render states
    //

    g_pD3DX->SetClearColor(g_colorClear);

    g_pD3DDevice->SetTransform(D3DTRANSFORMSTATE_VIEW, g_matView);
    g_pD3DDevice->SetTransform(D3DTRANSFORMSTATE_WORLD, g_matIdentity);
    g_pD3DDevice->SetTransform(D3DTRANSFORMSTATE_PROJECTION, g_matProjection);

    g_pD3DDevice->LightEnable(0, TRUE);
    g_pD3DDevice->SetRenderState(D3DRENDERSTATE_AMBIENT, 0xffffffff);

    float fFogStart = 0.0f, fFogEnd = 40.0f;
    g_pD3DDevice->SetRenderState(D3DRENDERSTATE_FOGCOLOR, g_colorClear);
    g_pD3DDevice->SetRenderState(D3DRENDERSTATE_FOGTABLEMODE, D3DFOG_LINEAR);
    g_pD3DDevice->SetRenderState(D3DRENDERSTATE_FOGSTART, *((DWORD *) &fFogStart));
    g_pD3DDevice->SetRenderState(D3DRENDERSTATE_FOGEND, *((DWORD *) &fFogEnd));

    g_pD3DDevice->SetRenderState(D3DRENDERSTATE_CULLMODE, D3DCULL_CCW);

    g_pD3DDevice->SetTextureStageState(0, D3DTSS_MINFILTER, D3DTFN_LINEAR);
    g_pD3DDevice->SetTextureStageState(0, D3DTSS_MAGFILTER, D3DTFG_LINEAR);
    g_pD3DDevice->SetTextureStageState(0, D3DTSS_MIPFILTER, D3DTFP_POINT);
    g_pD3DDevice->SetTextureStageState(0, D3DTSS_COLOROP, D3DTOP_MODULATE);
    g_pD3DDevice->SetTextureStageState(0, D3DTSS_ALPHAOP, D3DTOP_MODULATE);


    //
    // Create light
    //

    D3DLIGHT7 d3dLight;
    ZeroMemory(&d3dLight, sizeof(d3dLight));

    d3dLight.dltType = D3DLIGHT_DIRECTIONAL;
    d3dLight.dcvDiffuse.r = g_colorLight.r;
    d3dLight.dcvDiffuse.g = g_colorLight.g;
    d3dLight.dcvDiffuse.b = g_colorLight.b;
    d3dLight.dcvDiffuse.a = g_colorLight.a;
    d3dLight.dcvSpecular.r = 1.0f;
    d3dLight.dcvSpecular.g = 1.0f;
    d3dLight.dcvSpecular.b = 1.0f;
    d3dLight.dcvSpecular.a = 0.0f;
    d3dLight.dvDirection.x = g_vecLight.x;
    d3dLight.dvDirection.y = g_vecLight.y;
    d3dLight.dvDirection.z = g_vecLight.z;

    g_pD3DDevice->SetLight(0, &d3dLight);


    //
    // Create material
    //

    D3DMATERIAL7 d3dMaterial;
    ZeroMemory(&d3dMaterial, sizeof(d3dMaterial));

    d3dMaterial.dcvDiffuse.r = 0.75f;
    d3dMaterial.dcvDiffuse.g = 0.75f;
    d3dMaterial.dcvDiffuse.b = 0.75f;
    d3dMaterial.dcvDiffuse.a = 0.25f;
    d3dMaterial.dcvAmbient.r = g_colorLight.r;
    d3dMaterial.dcvAmbient.g = g_colorLight.g;
    d3dMaterial.dcvAmbient.b = g_colorLight.b;
    d3dMaterial.dcvSpecular.r = 0.5f;
    d3dMaterial.dcvSpecular.g = 0.5f;
    d3dMaterial.dcvSpecular.b = 0.5f;
    d3dMaterial.dcvSpecular.b = 0.5f;
    d3dMaterial.dvPower = 10.0f;

    g_pD3DDevice->SetMaterial(&d3dMaterial);


    //
    // Get media path from registry
    //

    HKEY key;
    g_szPath[0] = '\0';

    if(ERROR_SUCCESS == RegOpenKeyEx(HKEY_LOCAL_MACHINE,
        "Software\\Microsoft\\DirectX", 0, KEY_READ, &key))
    {
        DWORD dwType;
        DWORD dwSize = sizeof(g_szPath);

        if(ERROR_SUCCESS == RegQueryValueEx( key, 
            "DXSDK Samples Path", NULL, &dwType, (BYTE*) g_szPath, &dwSize))
        {
            if(REG_SZ == dwType)
                strcat(g_szPath, "\\D3DX\\Media\\");
            else
                g_szPath[0] = '\0';
        }

        RegCloseKey(key);
    }


    //
    // Create textures
    //

    UINT i;

    for(i = 0; i < NUM_TEXTURES; i++)
    {
        D3DX_SURFACEFORMAT sf = D3DX_SF_UNKNOWN;
        char szTexName[512];

        sprintf(szTexName, "%s%s", g_szPath, g_szTexName[i]);

        if(FAILED(hr = D3DXCreateTextureFromFile(g_pD3DDevice, 0, 0, 0, &sf, NULL,
            &g_ppTex[i], NULL, szTexName, D3DX_FT_LINEAR)))
        {
            g_ppTex[i] = NULL;
        }
    }


    //
    // Create surfaces
    //

    if(g_ppTentacle[TCL_BASE] && FAILED(hr = g_ppTentacle[TCL_BASE]->CreateSurfaces()))
        return hr;

    if(g_pGround && FAILED(hr = g_pGround->CreateSurfaces()))
        return hr;


    g_pLensFlare->SetSource(17.0f, g_ppTex[TEX_FLARE0]);
    g_pLensFlare->SetFlare(0,  8.00f, 0.06f,  1.30f, g_ppTex[TEX_FLARE1]);
    g_pLensFlare->SetFlare(1, 12.00f, 0.04f,  1.00f, g_ppTex[TEX_FLARE2]);
    g_pLensFlare->SetFlare(2,  4.00f, 0.10f,  0.50f, g_ppTex[TEX_FLARE2]);
    g_pLensFlare->SetFlare(3,  8.00f, 0.08f, -0.30f, g_ppTex[TEX_FLARE2]);
    g_pLensFlare->SetFlare(4, 12.00f, 0.04f, -0.60f, g_ppTex[TEX_FLARE3]);
    g_pLensFlare->SetFlare(5, 30.00f, 0.04f, -1.00f, g_ppTex[TEX_FLARE1]);

    g_bReady = TRUE;
    return S_OK;
}




// ---------------------------------------------------------------------------
//
// RestoreContext
//
// Restores all surfaces.
//
// ---------------------------------------------------------------------------

HRESULT RestoreContext()
{
    HRESULT hr;
    
    if(FAILED(hr = g_pD3DX->RestoreSurfaces()))
        return hr;

    if(g_ppTentacle[TCL_BASE] && FAILED(hr = g_ppTentacle[TCL_BASE]->RestoreSurfaces()))
        return hr;

    if(g_pGround && FAILED(hr = g_pGround->RestoreSurfaces()))
        return hr;

    return S_OK;
}



// ---------------------------------------------------------------------------
//
// ReleaseContext
//
// ---------------------------------------------------------------------------

HRESULT ReleaseContext()
{
    g_bReady = FALSE;

    if(g_ppTentacle[TCL_BASE])
        g_ppTentacle[TCL_BASE]->ReleaseSurfaces();

    if(g_pGround)
        g_pGround->ReleaseSurfaces();

    RELEASE(g_pD3DDevice);
    RELEASE(g_pD3D);
    RELEASE(g_pDD);
    RELEASE(g_pD3DX);

    D3DXUninitialize();

    return S_OK;
}





// ---------------------------------------------------------------------------
//
// Startup
//
// Create the scene, initialize D3DX
//
// ---------------------------------------------------------------------------

HRESULT Startup()
{
    HRESULT hr;

    if(FAILED(hr = CreateScene()))
        return hr;

    if(FAILED(hr = CreateContext()))
        return hr;

    return S_OK;
}



// ---------------------------------------------------------------------------
//
// Shutdown
//
// Destroys the scene, and uninitializes D3DX
//
// ---------------------------------------------------------------------------

HRESULT Shutdown()
{
    ReleaseContext();
    ReleaseScene();

    return S_OK;
}




// ---------------------------------------------------------------------------
//
// Draw
//
// ---------------------------------------------------------------------------

HRESULT Draw()
{
    HRESULT hr;

    if(!g_bReady)
        return E_FAIL;


    if(g_bActive)
    {

        //
        // Process keyboard input
        //

        D3DXVECTOR3 vecT(0.0f, 0.0f, 0.0f);
        D3DXVECTOR3 vecR(0.0f, 0.0f, 0.0f);

        if(g_bKey['A'] || g_bKey[VK_NUMPAD1] || g_bKey[VK_LEFT])  vecT.x -= 1.0f; // Slide Left
        if(g_bKey['D'] || g_bKey[VK_NUMPAD3] || g_bKey[VK_RIGHT]) vecT.x += 1.0f; // Slide Right
        if(g_bKey[VK_DOWN])                                       vecT.y -= 1.0f; // Slide Down
        if(g_bKey[VK_UP])                                         vecT.y += 1.0f; // Slide Up
        if(g_bKey['W'])                                           vecT.z -= 2.0f; // Move Forward
        if(g_bKey['S'])                                           vecT.z += 2.0f; // Move Backward
        if(g_bKey[VK_NUMPAD8])                                    vecR.x -= 1.0f; // Pitch Down
        if(g_bKey[VK_NUMPAD2])                                    vecR.x += 1.0f; // Pitch Up
        if(g_bKey['E'] || g_bKey[VK_NUMPAD6])                     vecR.y -= 1.0f; // Turn Right
        if(g_bKey['Q'] || g_bKey[VK_NUMPAD4])                     vecR.y += 1.0f; // Turn Left
        if(g_bKey[VK_NUMPAD9])                                    vecR.z -= 2.0f; // Roll CW
        if(g_bKey[VK_NUMPAD7])                                    vecR.z += 2.0f; // Roll CCW

        g_vecVelocity = g_vecVelocity * 0.9f + vecT * 0.1f;
        g_vecAngularVelocity = g_vecAngularVelocity * 0.9f + vecR * 0.1f;



        //
        // Update position and view matricies
        //

        D3DXMATRIX matT, matR;
        D3DXQUATERNION qR;

        vecT = g_vecVelocity * g_pFrameTimer->GetSecsPerFrame() * g_fSpeed;
        vecR = g_vecAngularVelocity * g_pFrameTimer->GetSecsPerFrame() * g_fAngularSpeed;

        D3DXMatrixTranslation(&matT, vecT.x, vecT.y, vecT.z);
        D3DXMatrixMultiply(&g_matPosition, &matT, &g_matPosition);

        D3DXQuaternionRotationYawPitchRoll(&qR, vecR.y, vecR.x, vecR.z);
        D3DXMatrixRotationQuaternion(&matR, &qR);
        D3DXMatrixMultiply(&g_matPosition, &matR, &g_matPosition);

        if(g_matPosition.m31 < 1.0f)
            g_matPosition.m31 = 1.0f;

        D3DXMatrixInverse(&g_matView, NULL, &g_matPosition);
    }





    //
    // Begin Scene
    //

    if(SUCCEEDED(g_pD3DDevice->BeginScene()))
    {
        g_pD3DX->Clear(D3DCLEAR_TARGET | D3DCLEAR_ZBUFFER);


        // Draw reflection
        if(g_bCapsReflection && g_bDrawReflection && g_bCapsTentacle && g_bDrawTentacle && g_bDrawGround)
        {
            D3DXMATRIX mat;

            D3DXMatrixReflect(&mat, &g_planeGround);
            D3DXMatrixMultiply(&mat, &mat, &g_matView);

            g_pD3DDevice->SetTransform(D3DTRANSFORMSTATE_VIEW, mat);
            g_pD3DDevice->SetClipPlane(0, g_planeGround);
            g_pD3DDevice->SetTexture(0, g_ppTex[TEX_TENTACLE]);

            g_pD3DDevice->SetRenderState(D3DRENDERSTATE_CLIPPLANEENABLE, D3DCLIPPLANE0);
            g_pD3DDevice->SetRenderState(D3DRENDERSTATE_CULLMODE, D3DCULL_CW);
            g_pD3DDevice->SetRenderState(D3DRENDERSTATE_SPECULARENABLE, TRUE);
            g_pD3DDevice->SetRenderState(D3DRENDERSTATE_FOGENABLE, TRUE);
            g_pD3DDevice->SetRenderState(D3DRENDERSTATE_SHADEMODE, D3DSHADE_GOURAUD);

            g_ppTentacle[TCL_BASE]->Draw(&g_matIdentity, g_bWireframe ? D3DPT_LINELIST : D3DPT_TRIANGLELIST, FALSE);

            g_pD3DDevice->SetRenderState(D3DRENDERSTATE_SHADEMODE, D3DSHADE_FLAT);
            g_pD3DDevice->SetRenderState(D3DRENDERSTATE_VERTEXBLEND, D3DVBLEND_DISABLE);
            g_pD3DDevice->SetRenderState(D3DRENDERSTATE_CLIPPLANEENABLE, 0);
            g_pD3DDevice->SetRenderState(D3DRENDERSTATE_CULLMODE, D3DCULL_CCW);
            g_pD3DDevice->SetRenderState(D3DRENDERSTATE_SPECULARENABLE, FALSE);
            g_pD3DDevice->SetRenderState(D3DRENDERSTATE_FOGENABLE, FALSE);
        }

        g_pD3DDevice->SetTransform(D3DTRANSFORMSTATE_VIEW, g_matView);
        g_pD3DDevice->SetTransform(D3DTRANSFORMSTATE_WORLD, g_matIdentity);


        // Draw ground
        if(g_bDrawGround)
        {
            g_pD3DDevice->SetTexture(0, g_ppTex[TEX_GROUND]);
            g_pD3DDevice->SetRenderState(D3DRENDERSTATE_LIGHTING, FALSE);

            if(g_bCapsReflection)
            {
                g_pD3DDevice->SetRenderState(D3DRENDERSTATE_ALPHABLENDENABLE, TRUE);
                g_pD3DDevice->SetRenderState(D3DRENDERSTATE_SRCBLEND, D3DBLEND_SRCALPHA);
                g_pD3DDevice->SetRenderState(D3DRENDERSTATE_DESTBLEND, D3DBLEND_INVSRCALPHA);
            }

            g_pGround->Draw();

            if(g_bCapsReflection)
            {
                g_pD3DDevice->SetRenderState(D3DRENDERSTATE_ALPHABLENDENABLE, FALSE);
            }

            g_pD3DDevice->SetRenderState(D3DRENDERSTATE_LIGHTING, TRUE);
        }


        // Draw tentacle
        if(g_bCapsTentacle && g_bDrawTentacle)
        {
            g_pD3DDevice->SetTexture(0, g_ppTex[TEX_TENTACLE]);
            g_pD3DDevice->SetRenderState(D3DRENDERSTATE_SPECULARENABLE, TRUE);
            g_pD3DDevice->SetRenderState(D3DRENDERSTATE_SHADEMODE, D3DSHADE_GOURAUD);

            g_ppTentacle[TCL_BASE]->Draw(&g_matIdentity, g_bWireframe ? D3DPT_LINELIST : D3DPT_TRIANGLELIST, FALSE);

            g_pD3DDevice->SetRenderState(D3DRENDERSTATE_VERTEXBLEND, D3DVBLEND_DISABLE);
            g_pD3DDevice->SetRenderState(D3DRENDERSTATE_SHADEMODE, D3DSHADE_FLAT);
            g_pD3DDevice->SetRenderState(D3DRENDERSTATE_SPECULARENABLE, FALSE);
        }


        // Draw lens flare
        if(g_bCapsLensFlare && g_bDrawLensFlare)
        {
            g_pD3DDevice->SetTransform(D3DTRANSFORMSTATE_WORLD, g_matIdentity);

            g_pD3DDevice->SetRenderState(D3DRENDERSTATE_LIGHTING, FALSE);
            g_pD3DDevice->SetRenderState(D3DRENDERSTATE_ALPHABLENDENABLE, TRUE);
            g_pD3DDevice->SetRenderState(D3DRENDERSTATE_VERTEXBLEND, D3DVBLEND_DISABLE);
            g_pD3DDevice->SetRenderState(D3DRENDERSTATE_ZWRITEENABLE, FALSE);
            g_pD3DDevice->SetRenderState(D3DRENDERSTATE_SRCBLEND, D3DBLEND_ONE);
            g_pD3DDevice->SetRenderState(D3DRENDERSTATE_DESTBLEND, D3DBLEND_INVSRCCOLOR);

            g_pLensFlare->Draw(g_matPosition);

            g_pD3DDevice->SetRenderState(D3DRENDERSTATE_ALPHABLENDENABLE, FALSE);
            g_pD3DDevice->SetRenderState(D3DRENDERSTATE_LIGHTING, TRUE);
            g_pD3DDevice->SetRenderState(D3DRENDERSTATE_ZWRITEENABLE, TRUE);
        }


        // Draw framerate
        DWORD dwColor;
        dwColor = 0xff7fcf7f;

        if(g_bDrawFramerate)
        {
            char sz[32];
            sprintf(sz, "%d fps", (int) g_pFrameTimer->GetFramesPerSec());
            g_pD3DX->DrawDebugText(0.01f, 0.0f, dwColor, sz);

            if(!g_bDrawHelp)
            {
                g_pD3DX->DrawDebugText(0.89f, 0.0f, dwColor, "F1=help");
            }
        }


        // Draw help
        if(g_bDrawHelp)
        {
            g_pD3DX->DrawDebugText(0.01f, 0.1f, dwColor,
                "Keyboard controls:\n"
                "(NUMLOCK must be on)\n"
                "\n"
                "  Move\n"
                "  Slide\n"
                "  Turn\n"
                "  Pitch\n"
                "  Roll\n"
                "  Exit\n"
                "\n"
                "Mouse controls:\n"
                "\n"
                "  Rotate left arm\n"
                "  Rotate right arm\n"
                "  Rotate base\n"
                "  Twist left arm\n"
                "  Twist right arm\n"
                "  Twist base\n");

            g_pD3DX->DrawDebugText(0.25f, 0.1f, dwColor,
                "\n"
                "\n"
                "\n"
                "W,S\n"
                "Arrow Keys\n"
                "Numpad 4,6\n"
                "Numpad 2,8\n"
                "Numpad 7,9\n"
                "Escape\n"
                "\n"
                "\n"
                "\n"
		        "LButton\n"
		        "RButton\n"
                "LButton+RButton\n"
                "Shift+LButton\n"
                "Shift+RButton\n"
                "Shift+LButton+RButton");
        }

        // End Scene
        g_pD3DDevice->EndScene();
    }


    // Update frame
    hr = g_pD3DX->UpdateFrame(0);

    if(DDERR_SURFACELOST == hr || DDERR_SURFACEBUSY == hr)
    {
        hr = g_pDD->TestCooperativeLevel();

        if(SUCCEEDED(hr))
        {
            if(FAILED(hr = RestoreContext()))
                return hr;
        }
        else if(DDERR_WRONGMODE == hr)
        {
            if(FAILED(hr = ReleaseContext()))
                return hr;

            if(FAILED(hr = CreateContext()))
                return hr;
        }
    }

    if(g_bActive)
        g_pFrameTimer->Frame();

    return S_OK;
}




//----------------------------------------------------------------------------
//
// OnMouseMove
//
// We track mouse moves to animate the tentacle.
//
//----------------------------------------------------------------------------

LRESULT OnMouseMove(HWND hwnd, WORD fwKeys, int x, int y)
{
    if (g_bMouseCaptured)
    {
        // Build a relative rotation about X and Y based on the mouse's
        // relative motion
        int XDelta = x - g_nLastMouseX;
        int YDelta = y - g_nLastMouseY;

        if (g_bFullScreen)
        {
            g_nLastMouseX = 320;
            g_nLastMouseY = 200;

            SetCursorPos(g_nLastMouseX, g_nLastMouseY);
        }
        else
        {
            g_nLastMouseX = x;
            g_nLastMouseY = y;
        }

        // NOTE: The division by 5.0f is arbitrary and was chosen simply because
        // it seemed to give a reasonable change in angle for mouse motion.
        float fXAngle = D3DXToRadian((float) XDelta / 5.0f);
        float fYAngle = D3DXToRadian((float) YDelta / 5.0f);


        if(fwKeys & MK_CONTROL)
        {
            // Scale
            if(fwKeys & MK_LBUTTON)
            {
                g_ppTentacle[TCL_TIP_A]->Stretch(fXAngle);
            }
            else if(fwKeys & MK_RBUTTON)
            {
                g_ppTentacle[TCL_TIP_B]->Stretch(fXAngle);
            }
        }
        else if(fwKeys & MK_SHIFT)
        {
            // Rotate about Y axis
            if((fwKeys & (MK_LBUTTON | MK_RBUTTON)) == (MK_LBUTTON | MK_RBUTTON))
            {
                g_ppTentacle[TCL_BASE]->Twist(0.0f, fXAngle, 0.0f);
            }
            else if(fwKeys & MK_LBUTTON)
            {
                g_ppTentacle[TCL_ARM_A] ->Twist(0.0f, fXAngle, 0.0f);
                g_ppTentacle[TCL_ARM_A1]->Twist(0.0f, fXAngle, 0.0f);
            }
            else if(fwKeys & MK_RBUTTON)
            {
                g_ppTentacle[TCL_ARM_B]->Twist(0.0f, fXAngle, 0.0f);
            }
        }
        else
        {
            // Rotate about X and Z axes
            if((fwKeys & (MK_LBUTTON | MK_RBUTTON)) == (MK_LBUTTON | MK_RBUTTON))
            {
                g_ppTentacle[TCL_BASE]->Twist(fYAngle, 0.0f, fXAngle);
            }
            else if(fwKeys & MK_LBUTTON)
            {
                g_ppTentacle[TCL_ARM_A] ->Twist(fYAngle, 0.0f, fXAngle);
                g_ppTentacle[TCL_ARM_A1]->Twist(fYAngle, 0.0f, fXAngle);
            }
            else if(fwKeys & MK_RBUTTON)
            {
                g_ppTentacle[TCL_ARM_B]->Twist(fYAngle, 0.0f, fXAngle);
            }
        }
    }

    return 0L;
}


// ---------------------------------------------------------------------------
//
// WndProc
//
// ---------------------------------------------------------------------------

LRESULT CALLBACK WndProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{

    switch(uMsg)
    {
    case WM_CLOSE:
        PostQuitMessage(0);
        break;

    case WM_SIZE:
        if(g_bReady && !g_bFullScreen && LOWORD(lParam) > 0 && HIWORD(lParam) > 0)
        {
            HRESULT hr;

            if(FAILED(hr = g_pD3DX->Resize(LOWORD(lParam),HIWORD(lParam))))
            {
                g_bReady = FALSE;
                InterpretError(hr);
                PostQuitMessage(0);
            }

            D3DXMatrixPerspectiveFov(&g_matProjection, D3DXToRadian(60.0f), 3.0f / 4.0f, 0.1f, 100.0f);
            g_pD3DDevice->SetTransform(D3DTRANSFORMSTATE_PROJECTION, g_matProjection);
        }
        break;

    case WM_KEYUP:
        g_bKey[wParam] = 0;

        switch(wParam)
        {
        case VK_ESCAPE:
            PostQuitMessage(0);
            break;

        case VK_F1:
            g_bDrawHelp = !g_bDrawHelp;
            break;

        case VK_F2:
            g_bWireframe = !g_bWireframe;
            break;
            
        case 'F':
            g_bDrawFramerate = !g_bDrawFramerate;
            break;

        case 'G':
            g_bDrawGround = !g_bDrawGround;
            break;

        case 'R':
            g_bDrawReflection = !g_bDrawReflection;
            break;

        case 'T':
            g_bDrawTentacle = !g_bDrawTentacle;
            break;

        case 'L':
            g_bDrawLensFlare = !g_bDrawLensFlare;
            break;
        }

        break;

    case WM_KEYDOWN:
        g_bKey[wParam] = 1;
        break;

    case WM_LBUTTONDOWN:
    case WM_RBUTTONDOWN:
        if (!g_bMouseCaptured)
        {
            SetCapture(hwnd);

            g_bMouseCaptured = TRUE;
            g_wMouseButtons = (WORD) wParam;

            g_nLastMouseX = (short) LOWORD(lParam);
            g_nLastMouseY = (short) HIWORD(lParam);
        }
        return 0L;

    case WM_LBUTTONUP:
    case WM_RBUTTONUP:
        if (g_bMouseCaptured)
        {
            ReleaseCapture();
            g_bMouseCaptured = FALSE;
        }
        return 0L;

    case WM_MOUSEMOVE:
        return OnMouseMove(hwnd, (WORD)wParam, (short) LOWORD(lParam), (short) HIWORD(lParam));

    case WM_ACTIVATEAPP:
        if(!wParam && g_bActive)
        {
            g_bActive = FALSE;

            if(g_pDD)
                g_pDD->FlipToGDISurface();

            RedrawWindow(g_hwnd, NULL, NULL, RDW_FRAME);
        }
        else if(wParam && !g_bActive)
        {
            g_bActive = TRUE;
        }
        break;

    case WM_PAINT:
        if(g_bReady)
        {
            HRESULT hr;
            PAINTSTRUCT ps;

            BeginPaint(hwnd, &ps);

            if(FAILED(hr = Draw()))
            {
                g_bReady = FALSE;
                InterpretError(hr);
                PostQuitMessage(0);
            }

            EndPaint(hwnd, &ps);
            return 0L;
        }
        break;

    case WM_ERASEBKGND:
        return 0L;

    case WM_SETCURSOR:
        if(g_bFullScreen)
            SetCursor(NULL);
        break;

    default:
        break;
    }


    return DefWindowProc(hwnd, uMsg, wParam, lParam);
}




// ---------------------------------------------------------------------------
//
// WinMain
//
// ---------------------------------------------------------------------------

int PASCAL WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance,
                   LPSTR lpszCmdLine, int nCmdShow)
{
    HRESULT  hr = S_OK;
    MSG      msg;
    WNDCLASS wc;
    HCURSOR  hcur = NULL;
    int      ret = 0;



    // Parse command line
    char *psz, *pszLim;

    psz = lpszCmdLine;
    pszLim = psz + strlen(psz);

    while(psz < pszLim)
    {        
        char *pszWord;
        pszWord = psz;

        while(psz[0] != '\0' && psz[0] != ' ')
            psz++;

        psz[0] = '\0';
        psz++;


        if(!strcmp(pszWord, "fs"))
            g_bFullScreen = TRUE;
        else if(!strcmp(pszWord, "ref"))
            g_dwHwLevel = D3DX_HWLEVEL_REFERENCE;
        else if(!strcmp(pszWord, "rgb"))
            g_dwHwLevel = D3DX_HWLEVEL_2D;
        else if(!strcmp(pszWord, "hal"))
            g_dwHwLevel = D3DX_HWLEVEL_RASTER;
        else if(!strcmp(pszWord, "tnl"))
            g_dwHwLevel = D3DX_HWLEVEL_TL;
        else if(atoi(pszWord) != 0)
            g_dwNumBackBuffers = (DWORD) atoi(pszWord);


        while(psz < pszLim && psz[0] == ' ')
            psz++;
    }


    if(!g_bFullScreen)
        g_dwNumBackBuffers = 1;



    // Register the window class for the main window.
    g_hAppInstance = hInstance;

    if (!hPrevInstance)
    {
        if(!g_bFullScreen)
            hcur = CopyCursor(LoadCursor(NULL, IDC_ARROW));


        wc.style = 0;
        wc.lpfnWndProc = (WNDPROC) WndProc;
        wc.cbClsExtra = 0;
        wc.cbWndExtra = 0;
        wc.hInstance = hInstance;
        wc.hIcon = LoadIcon(hInstance, MAKEINTRESOURCE(IDI_APP_ICON));
        wc.hCursor = hcur;
        wc.hbrBackground = (HBRUSH) GetStockObject(BLACK_BRUSH);
        wc.lpszMenuName = NULL;
        wc.lpszClassName = g_szAppName;

        if(!RegisterClass(&wc))
            goto LExit;
    }

    // Create the main window.
    if(g_bFullScreen)
    {
        g_hwnd = CreateWindow(
            g_szAppName, g_szAppName, WS_POPUP, CW_USEDEFAULT, CW_USEDEFAULT,
            640, 480, (HWND) NULL, (HMENU) NULL, hInstance, (LPVOID) NULL);
    }
    else
    {
        g_hwnd = CreateWindow(
            g_szAppName, g_szAppName, WS_OVERLAPPEDWINDOW, CW_USEDEFAULT, CW_USEDEFAULT,
            640, 480, (HWND) NULL, (HMENU) NULL, hInstance, (LPVOID) NULL);
    }

    if (!g_hwnd)
        goto LExit;

    ShowWindow(g_hwnd, nCmdShow);
    UpdateWindow(g_hwnd);



    // Startup
    if(FAILED(hr = Startup()))
    {
        InterpretError(hr);
        goto LExit;
    }


    // Message loop.
    while(TRUE)
    {
        if(PeekMessage(&msg, NULL, 0U, 0U, PM_REMOVE))
        {
            if(WM_QUIT == msg.message)
                break;

            TranslateMessage(&msg);
            DispatchMessage(&msg);
        }
        else if(g_bActive)
        {
            if(FAILED(hr = Draw()))
            {
                g_bReady = FALSE;
                InterpretError(hr);
                PostQuitMessage(0);
            }
        }
        else
        {
            WaitMessage();
        }
    }



LExit:
    // Shutdown
    if(FAILED(hr = Shutdown()))
    {
        InterpretError(hr);
        return -1;
    }

    if(hcur)
        DestroyCursor(hcur);

    return ret;
}


