//-----------------------------------------------------------------------------
// File: Sprite.cpp
//
// Desc: Example code showing how to use sprites with DrawPrim along with
//       D3DX. A "sprite" is loosely defined as a 2D image that you want 
//       to transfer to the rendering target.
//
// Copyright (c) 1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------

#define APP_NAME "D3DX - Sprite"

#define D3D_OVERLOADS

#define RELEASENULL(object) if (object) {object->Release();}

#include "d3dx.h"
#include "resource.h"
#include "mmsystem.h"
#include "stdio.h"

//-----------------------------------------------------------------------------
// Defines, constants, and global variables
//-----------------------------------------------------------------------------

// To allow WinMain to access the main application class, we use
// a global pointer to the application.
class CSpriteApp;

CSpriteApp* g_pSpriteApp = NULL;

//-----------------------------------------------------------------------------
// Name: CSpriteApp
// Desc: This class contains all the application specific state for
//       rendering the sample.
//-----------------------------------------------------------------------------
class CSpriteApp
{
public:
    CSpriteApp();
    ~CSpriteApp();

    HRESULT                 InitD3DX();
    HRESULT                 ReleaseD3DX();
    HRESULT                 InitRenderer();
    HRESULT                 Draw();
    HRESULT                 HandleModeChanges();
    
    void                    PauseDrawing();
    void                    RestartDrawing();

    HRESULT                 FrameMove(FLOAT fTimeKey);

    BOOL                    m_bD3DXReady;
    BOOL                    m_bActive;

    HWND                    m_hwndMain;

    LPDIRECT3DDEVICE7       m_pd3dDevice;
    LPDIRECTDRAW7           m_pddraw;
    ID3DXContext*           m_pd3dx;

private:
    // Application specific state
   
    // Animated Scaling for Image
    float m_fScaleCurrent;
    
    // Animated Rotation
    float m_fRotationCurrent;      
    
    // Animated Alpha
    DWORD m_dwAlpha;      
    
    // Animated frame count (0-59)
    DWORD m_currentFrame;         
    
    // Texture used to blt from
    IDirectDrawSurface7 *m_ptex;

    // Media path
    CHAR m_szPath[512];
    
}; // class CSpriteApp

//-----------------------------------------------------------------------------
// Name: CSpriteApp::CSpriteApp()
// Desc: Constructor for CSpriteApp; this method initializes
//       all the member values to good defaults. It is important
//       to NULL out any pointer that might get freed in the
//       destructor.
//-----------------------------------------------------------------------------

CSpriteApp::CSpriteApp()
{  
    m_bD3DXReady            = FALSE;
    m_pd3dDevice            = NULL;
    m_pd3dx                 = NULL;
    m_pddraw                = FALSE;
   
    m_bActive               = TRUE;
    
    m_fScaleCurrent         = 0.0f;
    m_fRotationCurrent      = 0.0f;
    m_dwAlpha               = 0xFF;
    m_currentFrame          = 0;
    m_ptex                  = NULL;



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
} // CSpriteApp::CSpriteApp

//-----------------------------------------------------------------------------
// Name: CSpriteApp::~CSpriteApp()
// Desc: Destructor for CSpriteApp; This is
//       a good time to free memory that we've allocated. Also, it's a good
//       time to release interfaces that we are holding references to. 
//-----------------------------------------------------------------------------
CSpriteApp::~CSpriteApp()
{
    ReleaseD3DX();
} // CSpriteApp::~CSpriteApp

//-----------------------------------------------------------------------------
// Name: ReleaseD3DX
// Desc: Release all the created objects.
//-----------------------------------------------------------------------------
HRESULT CSpriteApp::ReleaseD3DX()
{
    RELEASENULL(m_ptex);
    RELEASENULL(m_pddraw);
    RELEASENULL(m_pd3dDevice);
    RELEASENULL(m_pd3dx);
    D3DXUninitialize();
    return S_OK;
}

//-----------------------------------------------------------------------------
// Name: InterpretError
// Desc: Debugging helper that tries to give a helpful message when
//       something goes wrong
//-----------------------------------------------------------------------------
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
} // ::InterpretError

//-----------------------------------------------------------------------------
// Name: CSpriteApp::PauseDrawing()
// Desc: This method is called whenever the rendering loop is paused.
//-----------------------------------------------------------------------------
void CSpriteApp::PauseDrawing()
{
    g_pSpriteApp->m_bActive = FALSE;
    if (g_pSpriteApp->m_pddraw)
        g_pSpriteApp->m_pddraw->FlipToGDISurface();
    RedrawWindow(m_hwndMain, NULL, NULL, RDW_FRAME);
    ShowCursor(TRUE);
} // PauseDrawing

//-----------------------------------------------------------------------------
// Name: CSpriteApp::RestartDrawing()
// Desc: This method is called whenever the rendering loop is restarted after
//       a pause.
//-----------------------------------------------------------------------------
void CSpriteApp::RestartDrawing()
{
    g_pSpriteApp->m_bActive = TRUE;
    ShowCursor(FALSE);
} // RestartDrawing

//-----------------------------------------------------------------------------
// Name: CSpriteApp::InitD3DX()
// Desc: This method initializes D3DX just using defaults. Much greater control
//       is possible if explicit parameters are passed.
//-----------------------------------------------------------------------------
HRESULT CSpriteApp::InitD3DX()
{
    HRESULT hr;
    
    if( FAILED( hr = D3DXInitialize()) )
        return hr;

    // Look for fastest device which supports the desired blending for sprites
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

        if(!((d3dDesc.dpcTriCaps.dwSrcBlendCaps & D3DPBLENDCAPS_SRCALPHA) &&
             (d3dDesc.dpcTriCaps.dwDestBlendCaps & D3DPBLENDCAPS_INVSRCALPHA) &&
             (d3dDesc.dpcTriCaps.dwTextureFilterCaps & D3DPTFILTERCAPS_LINEAR) &&
             (d3dDesc.dpcTriCaps.dwTextureBlendCaps & D3DPTBLENDCAPS_MODULATE)
            ))
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

    hr = D3DXCreateContext(
            dev.hwLevel,                    // D3DX device
            0,                              // flags
            m_hwndMain,                     // Main window
            D3DX_DEFAULT,                   // colorbits
            D3DX_DEFAULT,	                // numdepthbits
            &m_pd3dx);                      // returned D3DX interface
    if (FAILED(hr))
        return hr;
    
    m_bD3DXReady = TRUE;
    return InitRenderer();
} // InitD3DX

//-----------------------------------------------------------------------------
// Name: CSpriteApp::InitRenderer()
// Desc: This function is called to initialize the application
//       state when the device changes.
//-----------------------------------------------------------------------------
HRESULT CSpriteApp::InitRenderer()
{
    HRESULT hr;
    
    if (!m_bD3DXReady)
        return E_FAIL;
    
    m_pd3dDevice = m_pd3dx->GetD3DDevice();
    if (m_pd3dDevice == NULL)
        return E_FAIL;
    
    m_pddraw = m_pd3dx->GetDD();
    if (m_pddraw == NULL)
        return E_FAIL;
    
    CHAR szTex[512];
    sprintf(szTex, "%s%s", m_szPath, "donut24.bmp");

    hr = D3DXCreateTextureFromFile(
            m_pd3dDevice,
            NULL,                   // dwFlags
            NULL,                   // auto-width
            NULL,                   // auto-height
            NULL,                   // auto-surface type
            NULL,                   // pointer to Palette
            &m_ptex,                // returned pointer to texture
            NULL,                   // returned number of mip-maps
            szTex,                  // file name for texture
            D3DX_FT_DEFAULT);       // default scaling

    if (FAILED(hr))
        return hr;
    
    // Enable dither, specular, lighting and z-buffer usage
    hr = m_pd3dDevice->SetRenderState(D3DRENDERSTATE_DITHERENABLE,      TRUE);
    if (FAILED(hr))
        return hr;
    hr = m_pd3dDevice->SetRenderState(D3DRENDERSTATE_SPECULARENABLE,    TRUE);
    if (FAILED(hr))
        return hr;
    hr = m_pd3dDevice->SetRenderState(D3DRENDERSTATE_LIGHTING,          TRUE);
    if (FAILED(hr))
        return hr;
    hr = m_pd3dDevice->SetRenderState(D3DRENDERSTATE_ZENABLE,           TRUE);
    if (FAILED(hr))
        return hr;
    
    // Enable vertices to have colors 
    hr = m_pd3dDevice->SetRenderState(D3DRENDERSTATE_COLORVERTEX,       TRUE);
    if (FAILED(hr))
        return hr;
    hr = m_pd3dDevice->SetRenderState(D3DRENDERSTATE_DIFFUSEMATERIALSOURCE,     TRUE);
    if (FAILED(hr))
        return hr;
    
    // Set the background to bright blue (red/green/blue/alpha)
    hr = m_pd3dx->SetClearColor((D3DCOLOR) D3DRGBA(0.0f, 0.0f, 1.0f, 1.0f));
    if (FAILED(hr))
        return hr;
    
    hr = m_pd3dx->Clear(D3DCLEAR_TARGET | D3DCLEAR_ZBUFFER);
    if (FAILED(hr))
        return hr;
    
    return S_OK;
} // InitRenderer

//-----------------------------------------------------------------------------
// Name: FrameMove()
// Desc: Called once per frame, the call is the entry point for animating
//       the scene.
//-----------------------------------------------------------------------------
HRESULT CSpriteApp::FrameMove(FLOAT fTimeKey)
{
    FLOAT t  = fTimeKey;
    
    // Over 10 seconds, we want to animate our image through some
    // changes smoothly; and then reverse through the same changes. 
    
    // First, figure out where we are in the 10 second time-line
    t = (float)fmod(t, 10.0f);
    
    // If T is greater then 5, then time-reverse it.
    if (t > 5.0f)
    {
        t = 10.0f - t;
    }
    
    // At this point, t should be a value that goes from 0 to 5 and then back
    // down to 0 over a period of 10 seconds.
    
    // Normalize T so that it goes from 0 to 1 and then back to 0.
    t = t / 5.0f; 
    
    // We want to scale from 0% to 200% over 5 seconds
    m_fScaleCurrent = t * 2.00f;
    
    // Similarly we want a rotation of 360 degrees at the 5 second point
    m_fRotationCurrent = t * 2 * D3DX_PI; // 360 degrees == 2 * pi
    
    // Similarly we want an alpha value of 0xFF at the 5 second point
    m_dwAlpha = (DWORD)(t * 0xFF);
   
    // Let's have our frame count do a 3 full cycles in 5 seconds
    m_currentFrame = (DWORD) (t * 3 * 60.0f);
    
    // We take the mod by 60; because our frame count should
    // be a number in the range of 0 and 59 (since we have
    // 60 frames in our donut bitmap.)
    m_currentFrame %= 60;
    
    return S_OK;
} // FrameMove

//-----------------------------------------------------------------------------
// Name: 
// Desc: 
//       
//-----------------------------------------------------------------------------
HRESULT CSpriteApp::Draw()
{
    HRESULT hr;
    
    if (!m_bD3DXReady)
        return E_FAIL;
    
    if (!m_bActive)
        return S_OK;
    
    if( SUCCEEDED( hr = m_pd3dDevice->BeginScene() ) )
    {
        m_pd3dx->Clear(D3DCLEAR_TARGET|D3DCLEAR_ZBUFFER);
    
        // We need to setup the rasterizer for rendering sprites;
        // this only needs to be done once for all the sprites that
        // are rendered; however this function does need to be called
        // again if any render state changes are made outside of the
        // bltsprite call.
        ::D3DXPrepareDeviceForSprite(m_pd3dDevice, TRUE);
        
        // Set up a SpriteFX structure so we can specify some
        // special effects
        RECT srcRect;
        
        // Our rect source is a different sub-rect every
        // frame. The source image is a grid of pictures that has
        // 10 columns and 6 rows. To find our X position, we
        // compute (m_currentFrame % 10). To find our Y position, we
        // compute (m_currentFrame / 10). 
        int iX = (m_currentFrame % 10);
        int iY = (m_currentFrame / 10);
        
        // Each frame is spaced into 64-pixels by 64 pixels cells
        srcRect.left = iX * 64;
        srcRect.top  = iY * 64;
        
        // Each sub-frame is 64 pixels wide and 48 pixels tall
        srcRect.right  = srcRect.left + 64;
        srcRect.bottom = srcRect.top  + 48;
        
        // Get our current viewport
        D3DVIEWPORT7 viewport;
        hr = m_pd3dDevice->GetViewport(&viewport);
        if (FAILED(hr))
            return hr;
        
        // Convert the viewport into a proper Rect
        RECT rectViewport;
        rectViewport.left   = viewport.dwX;
        rectViewport.top    = viewport.dwY;
        rectViewport.right  = viewport.dwX + viewport.dwWidth;
        rectViewport.bottom = viewport.dwY + viewport.dwHeight;
        
        // Our non-rotated render target should be centered in the viewport;
        D3DXVECTOR3 pointDest; 
        pointDest.x = (float)viewport.dwX + (float)viewport.dwWidth  / 2.0f;
        pointDest.y = (float)viewport.dwY + (float)viewport.dwHeight / 2.0f;
        pointDest.z = 0.0f;
        
        // Pick a mode; these are static to make it easy to
        // switch around in the debugger
        static BOOL fDraw = TRUE;
        static BOOL fMatrixHard = FALSE;
        static BOOL fMatrixEasy = FALSE;
        
        // Go ahead and do the render
        if (fDraw)
        {
            hr = ::D3DXDrawSpriteSimple(
                m_ptex,             // texture
                m_pd3dDevice,       // 3D device
                &pointDest,         // destination point (center)
                (float)m_dwAlpha/255.0f,    // alpha
                m_fScaleCurrent,    // scale
                m_fRotationCurrent, // rotation
                NULL,               // offset
                &srcRect            // src sub rect
                );     
        }
        else if (fMatrixHard)
        {
            D3DXMATRIX matScaleToSize;
            D3DXMatrixScaling(&matScaleToSize, 64.0f, 48.0f, 1.0f);
            
            D3DXMATRIX matRotate;
            D3DXMatrixRotationZ(&matRotate, m_fRotationCurrent);
            
            D3DXMATRIX matTranslate;
            D3DXMatrixTranslation(&matTranslate, (float)pointDest.x, (float)pointDest.y, 0.0f);
            
            D3DXMATRIX matScale;
            D3DXMatrixScaling(&matScale, m_fScaleCurrent, m_fScaleCurrent, 1.0f);
            
            D3DXMATRIX mat;
            mat = matScaleToSize * matRotate * matScale * matTranslate;
            
            hr = ::D3DXDrawSpriteTransform(
                m_ptex,         // texture
                m_pd3dDevice,   // 3D device
                &mat,           // matrix
                (BYTE)m_dwAlpha,// alpha
                &srcRect        // source sub-rect
                );     
        }
        else if (fMatrixEasy)
        {
            D3DXMATRIX matScale;
            D3DXMatrixScaling(&matScale, m_fScaleCurrent, m_fScaleCurrent, 1.0f);
            
            D3DXMATRIX matTransform;
            D3DXBuildSpriteTransform(&matTransform, &rectViewport, m_fRotationCurrent);
            
            D3DXMATRIX mat;
            mat = matScale * matTransform;
            
            hr = ::D3DXDrawSpriteTransform(
                m_ptex,         // texture
                m_pd3dDevice,   // 3D device
                &mat,           // matrix
                (BYTE)m_dwAlpha,// alpha
                &srcRect        // source sub-rect
                );     
        }
        
        if (FAILED(hr))
            return hr;
        
        m_pd3dDevice->EndScene();
    }
    
    hr = m_pd3dx->UpdateFrame(D3DX_DEFAULT);
    if ( hr == DDERR_SURFACELOST || hr == DDERR_SURFACEBUSY )
        hr = HandleModeChanges();

    return hr;
} // Draw

//-----------------------------------------------------------------------------
// Name: HandleWindowedModeChanges
// Desc: Handle mode changes 
//       
//-----------------------------------------------------------------------------
HRESULT CSpriteApp::HandleModeChanges()
{
    HRESULT hr;
    hr = m_pddraw->TestCooperativeLevel();

    if( SUCCEEDED( hr ) )
    {
        // This means that mode changes had taken place, surfaces
        // were lost but still we are in the original mode, so we
        // simply restore all surfaces and keep going.
        if( FAILED( m_pddraw->RestoreAllSurfaces() ) )
            return hr;

        // Load the texture back from the file
        CHAR szTex[512];
        sprintf(szTex, "%s%s", m_szPath, "donut24.bmp");

        hr = D3DXLoadTextureFromFile(
            m_pd3dDevice,
            m_ptex,                 // texture
            D3DX_DEFAULT,           // Num Mipmaps
            szTex,                  // file name for texture
            NULL,                   // srcRect
            NULL,                   // dstRect
            D3DX_FT_DEFAULT);       // default scaling

        if (FAILED(hr))
            return hr;
    
    }
    else if( hr == DDERR_WRONGMODE )
    {
        // This means that the desktop mode has changed
        // we can destroy and recreate everything back again.
        if(FAILED(hr = ReleaseD3DX()))
            return hr;
        if(FAILED(hr = InitD3DX()))
            return hr;
    }
    else if( hr == DDERR_EXCLUSIVEMODEALREADYSET )
    {
        // This means that some app took exclusive mode access
        // we need to sit in a loop till we get back to the right mode.
        do
        {
            Sleep( 500 );
        } while( DDERR_EXCLUSIVEMODEALREADYSET == 
                 (hr = m_pddraw->TestCooperativeLevel()) );
        if( SUCCEEDED( hr ) )
        {
            // This means that the exclusive mode app relinquished its 
            // control and we are back to the safe mode, so simply restore
            if( FAILED( m_pddraw->RestoreAllSurfaces() ) )
                return hr;
            // Load the texture back from the file
            CHAR szTex[512];
            sprintf(szTex, "%s%s", m_szPath, "donut24.bmp");
            
            hr = D3DXLoadTextureFromFile(
                m_pd3dDevice,
                m_ptex,                 // texture
                D3DX_DEFAULT,           // Num Mipmaps
                szTex,                  // file name for texture
                NULL,                   // srcRect
                NULL,                   // dstRect
                D3DX_FT_DEFAULT);       // default scaling
            
            if (FAILED(hr))
                return hr;
        }
        else if( DDERR_WRONGMODE == hr )
        {
            // This means that the exclusive mode app relinquished its 
            // control BUT we are back to some strange mode, so destroy
            // and recreate.
            if(FAILED(hr = ReleaseD3DX()))
                return hr;
            if(FAILED(hr = InitD3DX()))
                return hr;
        }
        else
        {
            // Busted!!
            return hr;
        }
    }
    else
    {
        // Busted!!
        return hr;
    }
    return S_OK;
}


//-----------------------------------------------------------------------------
// Name: 
// Desc: 
//       
//-----------------------------------------------------------------------------
LRESULT CALLBACK WndProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
    switch(uMsg)
    {
    case WM_CREATE:
        SetTimer(hwnd, 1, 1, NULL);
        break;
    case WM_CLOSE:
        PostQuitMessage(0);
        break;
    case WM_DESTROY:
        KillTimer(hwnd, 1);
        break;
    case WM_SIZE:
        if (g_pSpriteApp && 
                g_pSpriteApp->m_bD3DXReady && 
                LOWORD(lParam) > 0 && 
                HIWORD(lParam) > 0)
        {
            HRESULT hr;

            hr = g_pSpriteApp->m_pd3dx->Resize(LOWORD(lParam),HIWORD(lParam));

            if (FAILED(hr))
            {
                InterpretError(hr);
                g_pSpriteApp->m_bD3DXReady = FALSE;
                PostQuitMessage(0);
            }
            
        }
        break;
    case WM_TIMER:
        if (g_pSpriteApp && g_pSpriteApp->m_bActive)
        {
            FLOAT t  = (float)timeGetTime() / 1000.0f;
            HRESULT hr = g_pSpriteApp->FrameMove(t);
            if (FAILED(hr))
            {
                InterpretError(hr);
                g_pSpriteApp->m_bD3DXReady = FALSE;
                PostQuitMessage(0);
            }
        }
        break;
    case WM_KEYDOWN:
        switch (wParam)
        {
        case VK_ESCAPE:
            PostQuitMessage(0);		
        }
        break;
    default:
        break;
    }
   
    return DefWindowProc(hwnd,uMsg,wParam,lParam);
    
} // WndProc

//-----------------------------------------------------------------------------
// Name: 
// Desc: 
//       
//-----------------------------------------------------------------------------
int PASCAL WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, 
                   LPSTR lpszCmdLine, int nCmdShow) 
{
    HRESULT     hr;	
    MSG         msg; 
    WNDCLASS    wc; 
    HCURSOR     hcur = NULL;
    int         ret = 0;

    // Allocate a new instance of our SpriteApp object
    g_pSpriteApp = new CSpriteApp; // set up our data AFTER starting up d3dx!
    if (!g_pSpriteApp)
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
        wc.lpszClassName = APP_NAME; 
        
        if (!RegisterClass(&wc)) 
        {
            ret = -1;
            goto Exit;
        }
    } 
    
    // Create the main window. 
    g_pSpriteApp->m_hwndMain = CreateWindow(
            APP_NAME, 
            APP_NAME, 
            WS_OVERLAPPEDWINDOW, 
            CW_USEDEFAULT, 
            CW_USEDEFAULT, 
            300, 
            300, 
            (HWND) NULL, 
            (HMENU) NULL, 
            hInstance,
            (LPVOID) NULL); 
    
    // If the main window cannot be created, terminate 
    // the application. 
    if (!g_pSpriteApp->m_hwndMain)
    {
        ret = -1;
        goto Exit;
    }
    
    // Show the window and paint its contents. 
    ShowWindow(g_pSpriteApp->m_hwndMain, nCmdShow); 
    UpdateWindow(g_pSpriteApp->m_hwndMain); 

    // Init the D3DX portion of the SpriteApp object
    hr = g_pSpriteApp->InitD3DX();
    if (FAILED(hr))
    {
        InterpretError(hr);
        ret = -1;
        goto Exit;
    }
    
    // Start the message loop. 
    
    // Now we're ready to recieve and process Windows messages.
    BOOL bGotMsg;
    PeekMessage(&msg, NULL, 0U, 0U, PM_NOREMOVE);
    while (WM_QUIT != msg.message )
    {
        bGotMsg = PeekMessage(&msg, NULL, 0U, 0U, PM_REMOVE);
        
        if (bGotMsg)
        {
            TranslateMessage(&msg);
            DispatchMessage(&msg);
        }
        else
        {
            if (g_pSpriteApp && g_pSpriteApp->m_bActive)
            {
                hr = g_pSpriteApp->Draw();
                if (FAILED(hr))
                {
                    InterpretError(hr);
                    g_pSpriteApp->m_bD3DXReady = FALSE;
                    PostQuitMessage(0);
                }
            }
            else
            {
                WaitMessage();
            }
       }
   }
   delete g_pSpriteApp; // clean up our data BEFORE shutting down d3dx!
   
Exit:
    if(hcur)
        DestroyCursor(hcur);

    return ret;
} // WinMain

// End Of File

