//
// Particle Demo
// Copyright (c) 1999 Microsoft Corporation. All rights reserved.
//

#include "pch.hpp"


//////////////////////////////////////////////////////////////////////////////
// Globals ///////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////


//
// Window and context
//

const char *g_szAppName = "D3DX - Particle";

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
    TEX_GROUND, TEX_PARTICLE,
    NUM_TEXTURES
};

char *g_szTexName[NUM_TEXTURES] = {
    "ground2.bmp", "particle.bmp",
};

LPDIRECTDRAWSURFACE7 g_ppTex[NUM_TEXTURES];


//
// Background
//

D3DXCOLOR g_colorClear (0.0f, 0.0f, 0.0f, 1.0f);


//
// Ground plane
//

BOOL g_bDrawGround = TRUE;
BOOL g_bDrawReflection = TRUE;
BOOL g_bCapsReflection = TRUE;
CGround *g_pGround = NULL;

D3DXPLANE g_planeGround(0.0f, 1.0f, 0.0f, 0.0f);
D3DXCOLOR g_colorGround(0.2f, 0.2f, 0.2f, 0.9f);



//
// Particle
//

enum {
    COLOR_WHITE, COLOR_RED, COLOR_GREEN, COLOR_BLUE,
    NUM_COLORS
};

D3DXCOLOR g_clrColor[NUM_COLORS] = 
{
    D3DXCOLOR( 1.0f,   1.0f,   1.0f,   1.0f ),
    D3DXCOLOR( 1.0f,   0.5f,   0.5f,   1.0f ),
    D3DXCOLOR( 0.5f,   1.0f,   0.5f,   1.0f ),
    D3DXCOLOR( 0.125f, 0.5f,   1.0f,   1.0f )
};

DWORD g_clrColorFade[NUM_COLORS] = 
{
    D3DXCOLOR( 1.0f,   0.25f,   0.25f,   1.0f ),
    D3DXCOLOR( 1.0f,   0.25f,   0.25f,   1.0f ),
    D3DXCOLOR( 0.25f,  0.75f,   0.25f,   1.0f ),
    D3DXCOLOR( 0.125f, 0.25f,   0.75f,   1.0f )
};


BOOL g_bDrawParticle = TRUE;
BOOL g_bCapsParticle = TRUE;
CParticle *g_pParticle = NULL;
UINT g_uEmitParticle = 10;
BOOL g_bStaticParticle = TRUE;
BOOL g_uColorParticle = COLOR_RED;



//
// User interface
//

BOOL g_bReady      = FALSE;
BOOL g_bActive     = TRUE;
BOOL g_bPause      = FALSE;
BOOL g_bFullScreen = FALSE;

DWORD g_dwHwLevel        = D3DX_DEFAULT;
DWORD g_dwNumBackBuffers = 2;

float g_fSpeed        = 5.0f;
float g_fAngularSpeed = 1.0f;
float g_fFov          = 45.0f;
float g_fTime         = 0.0f;
float g_fTimeOld      = 0.0f;

BYTE g_bKey[256];

D3DXVECTOR3 g_vecVelocity(0.0f, 0.0f, 0.0f);
D3DXVECTOR3 g_vecAngularVelocity(0.0f, 0.0f, 0.0f);

D3DXVECTOR3 g_vecPosition(-4.33925f, 2.99127f, 2.35433f);
D3DXVECTOR3 g_vecYPR(-0.45f, -1.06782f, 0.0f);

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
CFrameTimer *g_pFrameTimer = NULL;

BOOL g_bDrawHelp = FALSE;



//////////////////////////////////////////////////////////////////////////////
// Implementation ////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////


// Converts a FLOAT to a DWORD for use in SetRenderState() calls
inline DWORD F2DW( FLOAT f ) { return *((DWORD*)&f); }



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

    // Create Ground
    if(!(g_pGround = new CGround))
        return E_OUTOFMEMORY;

    if(FAILED(hr = g_pGround->Initialize(256.0f, 256.0f, 32.0f, g_colorGround)))
        return hr;


    // Create particle
    if(!(g_pParticle = new CParticle))
        return E_OUTOFMEMORY;

    if(FAILED(hr = g_pParticle->Initialize(0.03f)))
        return hr;
    

    // Frame timer
    if(!(g_pFrameTimer = new CFrameTimer()))
        return E_OUTOFMEMORY;

    g_pFrameTimer->Start(30.0f);
    

    // Misc stuff
    D3DXMatrixTranslation(&g_matView, 0.0f, 0.0f, 10.0f);
    D3DXMatrixTranslation(&g_matPosition, 0.0f, 0.0f, 0.0f);
    D3DXMatrixPerspectiveFov(&g_matProjection, D3DXToRadian(g_fFov), 3.0f / 4.0f, 0.1f, 100.0f);

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
    if(g_pFrameTimer)
        delete g_pFrameTimer;

    if(g_pGround)
        delete g_pGround;

    if(g_pParticle)
        delete g_pParticle;

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
        hr = D3DXCreateContextEx(g_dwHwLevel, D3DX_CONTEXT_FULLSCREEN, g_hwnd, NULL, 32, 0,
            D3DX_DEFAULT, 0, g_dwNumBackBuffers, 640, 480, D3DX_DEFAULT, &g_pD3DX);

        if(FAILED(hr))
        {
            hr = D3DXCreateContextEx(g_dwHwLevel, D3DX_CONTEXT_FULLSCREEN, g_hwnd, NULL, D3DX_DEFAULT, 0,
                D3DX_DEFAULT, 0, g_dwNumBackBuffers, 640, 480, D3DX_DEFAULT, &g_pD3DX);
        }
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
    // Grok caps
    //

    memset(&g_D3DDeviceDesc, 0x00, sizeof(D3DDEVICEDESC7));
    g_pD3DDevice->GetCaps(&g_D3DDeviceDesc);

    g_bCapsReflection = (g_D3DDeviceDesc.dpcTriCaps.dwSrcBlendCaps & D3DPBLENDCAPS_SRCALPHA) &&
                        (g_D3DDeviceDesc.dpcTriCaps.dwDestBlendCaps & D3DPBLENDCAPS_INVSRCALPHA) &&
                        (g_D3DDeviceDesc.dpcTriCaps.dwShadeCaps & D3DPSHADECAPS_ALPHAFLATBLEND) &&
                        (g_D3DDeviceDesc.wMaxUserClipPlanes >= 1);

    g_bCapsParticle   = (g_D3DDeviceDesc.dpcTriCaps.dwSrcBlendCaps & D3DPBLENDCAPS_ONE) &&
                        (g_D3DDeviceDesc.dpcTriCaps.dwDestBlendCaps & D3DPBLENDCAPS_ONE) &&
                        (g_D3DDeviceDesc.dpcTriCaps.dwShadeCaps & D3DPSHADECAPS_ALPHAFLATBLEND);

    if(!g_bCapsParticle)
        return D3DXERR_CAPSNOTSUPPORTED;


    //
    // Setup render states
    //

    g_pD3DX->SetClearColor(g_colorClear);

    g_pD3DDevice->SetTransform(D3DTRANSFORMSTATE_VIEW, g_matView);
    g_pD3DDevice->SetTransform(D3DTRANSFORMSTATE_PROJECTION, g_matProjection);

    float fFogStart = 0.0f, fFogEnd = 40.0f;
    g_pD3DDevice->SetRenderState(D3DRENDERSTATE_FOGCOLOR, g_colorClear);
    g_pD3DDevice->SetRenderState(D3DRENDERSTATE_FOGTABLEMODE, D3DFOG_LINEAR);
    g_pD3DDevice->SetRenderState(D3DRENDERSTATE_FOGSTART, *((DWORD *) &fFogStart));
    g_pD3DDevice->SetRenderState(D3DRENDERSTATE_FOGEND, *((DWORD *) &fFogEnd));

    g_pD3DDevice->SetTextureStageState(0, D3DTSS_MINFILTER, D3DTFN_LINEAR);
    g_pD3DDevice->SetTextureStageState(0, D3DTSS_MAGFILTER, D3DTFG_LINEAR);
    g_pD3DDevice->SetTextureStageState(0, D3DTSS_MIPFILTER, D3DTFP_POINT);
    g_pD3DDevice->SetTextureStageState(0, D3DTSS_COLOROP, D3DTOP_MODULATE);
    g_pD3DDevice->SetTextureStageState(0, D3DTSS_ALPHAOP, D3DTOP_SELECTARG1);
    g_pD3DDevice->SetTextureStageState(1, D3DTSS_COLOROP, D3DTOP_DISABLE);
    g_pD3DDevice->SetTextureStageState(1, D3DTSS_ALPHAOP, D3DTOP_DISABLE);

    g_pD3DDevice->SetRenderState(D3DRENDERSTATE_SRCBLEND, D3DBLEND_ONE);
    g_pD3DDevice->SetRenderState(D3DRENDERSTATE_DESTBLEND, D3DBLEND_ONE);
    g_pD3DDevice->SetRenderState(D3DRENDERSTATE_LIGHTING, FALSE);
    g_pD3DDevice->SetRenderState(D3DRENDERSTATE_CULLMODE, D3DCULL_CCW);
    g_pD3DDevice->SetRenderState(D3DRENDERSTATE_SHADEMODE, D3DSHADE_FLAT);






    // Get media path from registry
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



    if(g_pGround && FAILED(hr = g_pGround->CreateSurfaces()))
        return hr;

    if(g_pParticle && FAILED(hr = g_pParticle->CreateSurfaces()))
        return hr;


    g_bReady = TRUE;
    return S_OK;
}


// ---------------------------------------------------------------------------
//
// RestoreContext
//
// ---------------------------------------------------------------------------

HRESULT RestoreContext()
{
    HRESULT hr;

    if(FAILED(hr = g_pD3DX->RestoreSurfaces()))
        return hr;

    if(g_pGround && FAILED(hr = g_pGround->RestoreSurfaces()))
        return hr;

    if(g_pParticle && FAILED(hr = g_pParticle->RestoreSurfaces()))
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

    if(g_pGround)
        g_pGround->ReleaseSurfaces();

    if(g_pParticle)
        g_pParticle->ReleaseSurfaces();

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

        g_vecVelocity = g_vecVelocity * 0.9f + vecT * 0.1f;
        g_vecAngularVelocity = g_vecAngularVelocity * 0.9f + vecR * 0.1f;


        if(g_bKey[VK_ADD] && g_uEmitParticle < 10)
            g_uEmitParticle++;
        if(g_bKey[VK_SUBTRACT] && g_uEmitParticle > 0)
            g_uEmitParticle--;


        //
        // Update position and view matricies
        //

        D3DXQUATERNION qR;

        vecT = g_vecVelocity * g_pFrameTimer->GetSecsPerFrame() * g_fSpeed;
        vecR = g_vecAngularVelocity * g_pFrameTimer->GetSecsPerFrame() * g_fAngularSpeed;

        D3DXVec3TransformNormal(&vecT, &vecT, &g_matPosition);
        g_vecPosition += vecT;

        if(g_bDrawGround && g_vecPosition.y < 1.0f)
            g_vecPosition.y = 1.0f;

        g_vecYPR += vecR;

        if(g_vecYPR.x > D3DX_PI * 0.45f)
            g_vecYPR.x = D3DX_PI * 0.45f;

        if(g_vecYPR.x < D3DX_PI * -0.45f)
            g_vecYPR.x = D3DX_PI * -0.45f;

        D3DXQuaternionRotationYawPitchRoll(&qR, g_vecYPR.y, g_vecYPR.x,  g_vecYPR.z);
        D3DXMatrixAffineTransformation(&g_matPosition, 1.25f, NULL, &qR, &g_vecPosition);
        D3DXMatrixInverse(&g_matView, NULL, &g_matPosition);


        //
        // Update particle simulation
        //

        if(g_bDrawParticle && !g_bPause)
        {
            UINT uColor;
            float fVel;

            if(g_bKey[VK_SPACE])
            {
                uColor = g_uColorParticle;
                fVel = 10.0f;
            }
            else
            {
                uColor = COLOR_WHITE;
                fVel = 8.0f;
            }

            g_pParticle->Update(g_pFrameTimer->GetSecsPerFrame() * 1.0f, g_uEmitParticle, 
                g_clrColor[uColor], g_clrColorFade[uColor], fVel, g_bStaticParticle);
        }
    }




    //
    // Begin Scene
    //


    if(SUCCEEDED(g_pD3DDevice->BeginScene()))
    {
        g_pD3DX->Clear(D3DCLEAR_TARGET | D3DCLEAR_ZBUFFER);
        g_pD3DDevice->SetTransform(D3DTRANSFORMSTATE_WORLD, g_matIdentity);


        // Draw reflection
        if(g_bCapsReflection && g_bDrawReflection && g_bDrawGround)
        {
            D3DXMATRIX mat;

            D3DXMatrixReflect(&mat, &g_planeGround);
            D3DXMatrixMultiply(&mat, &mat, &g_matView);

            g_pD3DDevice->SetTransform(D3DTRANSFORMSTATE_VIEW, mat);
            g_pD3DDevice->SetClipPlane(0, g_planeGround);

            g_pD3DDevice->SetRenderState(D3DRENDERSTATE_CLIPPLANEENABLE, D3DCLIPPLANE0);
            g_pD3DDevice->SetRenderState(D3DRENDERSTATE_FOGENABLE, TRUE);


            // Draw particles
            if(g_bCapsParticle && g_bDrawParticle)
            {
                D3DXMATRIX mat2;
                D3DXMatrixInverse(&mat2, NULL, &mat);

                g_pD3DDevice->SetTexture(0, g_ppTex[TEX_PARTICLE]);
                g_pD3DDevice->SetRenderState(D3DRENDERSTATE_ZWRITEENABLE, FALSE);
                g_pD3DDevice->SetRenderState(D3DRENDERSTATE_ALPHABLENDENABLE, TRUE);

                g_pParticle->Draw(mat2);

                g_pD3DDevice->SetRenderState(D3DRENDERSTATE_ALPHABLENDENABLE, FALSE);
                g_pD3DDevice->SetRenderState(D3DRENDERSTATE_ZWRITEENABLE, TRUE);
            }

            g_pD3DDevice->SetRenderState(D3DRENDERSTATE_FOGENABLE, FALSE);
            g_pD3DDevice->SetRenderState(D3DRENDERSTATE_CLIPPLANEENABLE, 0);
        }

        g_pD3DDevice->SetTransform(D3DTRANSFORMSTATE_VIEW, g_matView);


        // Draw ground
        if(g_bDrawGround)
        {
            g_pD3DDevice->SetTexture(0, g_ppTex[TEX_GROUND]);

            if(g_bCapsReflection)
            {
                g_pD3DDevice->SetRenderState(D3DRENDERSTATE_ALPHABLENDENABLE, TRUE);
                g_pD3DDevice->SetRenderState(D3DRENDERSTATE_SRCBLEND, D3DBLEND_SRCALPHA);
                g_pD3DDevice->SetRenderState(D3DRENDERSTATE_DESTBLEND, D3DBLEND_INVSRCALPHA);
		        g_pD3DDevice->SetTextureStageState(0, D3DTSS_ALPHAOP, D3DTOP_MODULATE);
            }

            g_pGround->Draw();

            if(g_bCapsReflection)
            {
                g_pD3DDevice->SetRenderState(D3DRENDERSTATE_ALPHABLENDENABLE, FALSE);
                g_pD3DDevice->SetRenderState(D3DRENDERSTATE_SRCBLEND, D3DBLEND_ONE);
                g_pD3DDevice->SetRenderState(D3DRENDERSTATE_DESTBLEND, D3DBLEND_ONE);
		        g_pD3DDevice->SetTextureStageState(0, D3DTSS_ALPHAOP, D3DTOP_SELECTARG1);
            }
        }



        // Draw particles
        if(g_bCapsParticle && g_bDrawParticle)
        {
            g_pD3DDevice->SetTexture(0, g_ppTex[TEX_PARTICLE]);

            g_pD3DDevice->SetRenderState(D3DRENDERSTATE_ZWRITEENABLE, FALSE);
            g_pD3DDevice->SetRenderState(D3DRENDERSTATE_ALPHABLENDENABLE, TRUE);

            if(g_bDrawGround)
            {
                g_pD3DDevice->SetRenderState(D3DRENDERSTATE_ZENABLE, FALSE);

                g_pParticle->DrawLights();

                g_pD3DDevice->SetRenderState(D3DRENDERSTATE_ZENABLE, TRUE);
            }

            g_pParticle->Draw(g_matPosition);

            g_pD3DDevice->SetRenderState(D3DRENDERSTATE_ALPHABLENDENABLE, FALSE);
            g_pD3DDevice->SetRenderState(D3DRENDERSTATE_ZWRITEENABLE, TRUE);
        }


        // Draw framerate
        DWORD dwColor;
        dwColor = 0xff404040;


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
            g_pD3DX->DrawDebugText(0.01f, 0.2f, dwColor,
                "Keyboard controls:\n"
                "(NUMLOCK must be on)\n"
                "\n"
                "  Move\n"
                "  Slide\n"
                "  Turn\n"
                "  Pitch\n"
                "\n"
                "  Pause simulation\n"
                "  Mobile emitter\n"
                "  Change color\n"
                "\n"
                "  Toggle ground\n"
                "  Toggle reflection\n"
                "  Toggle framerate\n"
                "\n"
                "  Exit");

            g_pD3DX->DrawDebugText(0.30f, 0.2f, dwColor,
                "\n"
                "\n"
                "\n"
                "W,S\n"
                "Arrow Keys\n"
                "Numpad 4,6\n"
                "Numpad 2,8\n"
                "\n"
		        "Pause\n"
		        "Enter\n"
                "Space\n"
                "\n"
                "G\n"
                "R\n"
                "F\n"
                "\n"
                "Escape");
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

            D3DXMatrixPerspectiveFov(&g_matProjection, D3DXToRadian(g_fFov), 3.0f / 4.0f, 0.1f, 100.0f);
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

        case 'G':
            g_bDrawGround = !g_bDrawGround;
            break;

        case 'R':
            g_bDrawReflection = !g_bDrawReflection;
            break;
        
        case 'F':
            g_bDrawFramerate = !g_bDrawFramerate;
            break;

        case VK_RETURN:
            g_bStaticParticle = !g_bStaticParticle;
            break;

		case VK_PAUSE:
			g_bPause = !g_bPause;
			break;

        case VK_SPACE:
            if(++g_uColorParticle == NUM_COLORS)
                g_uColorParticle = COLOR_RED;
            break;
        }

        break;

    case WM_KEYDOWN:
        g_bKey[wParam] = 1;
        break;

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
            g_pFrameTimer->Start(g_pFrameTimer->GetFramesPerSec());
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
