//
// TrivFS
// Copyright (c) 1999 Microsoft Corporation. All rights reserved.
//

#define     APP_NAME "D3DX - TrivFS"

#define     D3D_OVERLOADS
#define     RELEASENULL(object) if (object) {object->Release();}

#include    "d3dx.h"
#include    "resource.h"

class CTrivial
{
public:
    CTrivial();
    ~CTrivial();
    HRESULT                 InitD3DX();
    HRESULT                 InitRenderer();
    HRESULT                 Draw();
    void                    PauseDrawing();
    void                    RestartDrawing();
    HRESULT                 HandleFullScreenModeChanges();
    
    BOOL                    m_bD3DXReady;
    BOOL                    m_bActive;

    HWND                    m_hwndMain;

    LPDIRECT3DDEVICE7       m_pD3DDev;
    LPDIRECTDRAW7           m_pDD;
    ID3DXContext*           m_pD3DX;

    float                   m_fAngle;
};

CTrivial* g_pTrivial;

CTrivial::CTrivial()
{  
    m_bD3DXReady            = FALSE;
    m_pD3DDev               = NULL;
    m_pD3DX                 = NULL;
    m_pDD                   = FALSE;

    m_fAngle                = 0.0f;

    m_bActive               = FALSE;
}

CTrivial::~CTrivial()
{
    RELEASENULL(m_pDD);
    RELEASENULL(m_pD3DDev);
    RELEASENULL(m_pD3DX);
}

void InterpretError(HRESULT hr)
{
    char errStr[100];
    D3DXGetErrorString(hr, 100, errStr );
    MessageBox(NULL,errStr,"D3DX Error",MB_OK);
}

void CTrivial::PauseDrawing()
{
    g_pTrivial->m_bActive = FALSE;
    if( g_pTrivial->m_pDD ) g_pTrivial->m_pDD->FlipToGDISurface();
    DrawMenuBar(m_hwndMain);
    RedrawWindow(m_hwndMain, NULL, NULL, RDW_FRAME);
    ShowCursor(TRUE);
}

void CTrivial::RestartDrawing()
{
    g_pTrivial->m_bActive = TRUE;
    ShowCursor(FALSE);
}

HRESULT CTrivial::InitD3DX()
{
    HRESULT hr;

    hr = D3DXCreateContext( D3DX_DEFAULT,       // D3DX handle
                            D3DX_CONTEXT_FULLSCREEN,   // flags
                            m_hwndMain,
                            D3DX_DEFAULT,       // colorbits
                            D3DX_DEFAULT,       // numdepthbits
                            &m_pD3DX            // returned D3DX interface
                            );

    if( FAILED(hr) )
        return hr;

    m_bD3DXReady = TRUE;
    return S_OK;
}

HRESULT CTrivial::InitRenderer()
{
    HRESULT hr;

    if( !m_bD3DXReady )
        return E_FAIL;

    m_pD3DDev = m_pD3DX->GetD3DDevice();
    if( m_pD3DDev == NULL )
        return E_FAIL;

    m_pDD = m_pD3DX->GetDD();
    if( m_pDD == NULL )
        return E_FAIL;

    hr = m_pD3DDev->SetRenderState( D3DRENDERSTATE_DITHERENABLE, TRUE );
    if ( FAILED(hr) )
        return hr;

    hr = m_pD3DDev->SetRenderState( D3DRENDERSTATE_CULLMODE, D3DCULL_NONE );
    if ( FAILED(hr) )
        return hr;

    // let's make the background non-black.
    hr = m_pD3DX->SetClearColor(D3DRGBA(0.3f,0.3f,0.3f,0));
    if( FAILED(hr) )
        return hr;

    hr = m_pD3DX->Clear(D3DCLEAR_TARGET|D3DCLEAR_ZBUFFER);
    if ( FAILED(hr) )
        return hr;

    return S_OK;
}

D3DVECTOR v[3] = { D3DVECTOR(0.0f, 1.0f, 0.0f), D3DVECTOR(1.0f, 0.0f, 0.0f), D3DVECTOR(-1.0f, 0.0f, 0.0f) };
// position data in vTriangle is filled in by rotating the above points
D3DLVERTEX vTriangle[3] = { D3DLVERTEX(D3DVECTOR(0.0f, 0.0f, 0.0f), RGB_MAKE( 0xff, 0x00, 0x00 ), 0, 0.0f, 0.0f),
                            D3DLVERTEX(D3DVECTOR(0.0f, 0.0f, 0.0f), RGB_MAKE( 0x00, 0xff, 0x00 ), 0, 0.0f, 0.0f),
                            D3DLVERTEX(D3DVECTOR(0.0f, 0.0f, 0.0f), RGB_MAKE( 0x00, 0x00, 0xff ), 0, 0.0f, 0.0f) };

HRESULT CTrivial::Draw()
{
    HRESULT hr;

    if( !m_bD3DXReady )
        return E_FAIL;

    if( !m_bActive )
        return S_OK;

    hr = m_pD3DDev->BeginScene();
    if ( SUCCEEDED(hr) )
    {
        m_pD3DX->Clear(D3DCLEAR_TARGET|D3DCLEAR_ZBUFFER);
        
        // Rotate triangle about Y axis before drawing
        float fSin = (float) sin(m_fAngle);
        float fCos = (float) cos(m_fAngle);
        for (int i = 0; i < 3; i += 1)
        {
            vTriangle[i].x = v[i].x * fCos - v[i].z * fSin;
            vTriangle[i].y = v[i].y;
            vTriangle[i].z = v[i].x * fSin + v[i].z * fCos + 2.5f;
        }
        
        m_pD3DDev->DrawPrimitive( D3DPT_TRIANGLELIST, D3DFVF_LVERTEX, vTriangle, 3, D3DDP_WAIT );
        
        m_pD3DDev->EndScene();
    }

    hr = m_pD3DX->UpdateFrame( 0 );
    if ( hr == DDERR_SURFACELOST || hr == DDERR_SURFACEBUSY )
        hr = HandleFullScreenModeChanges();

    return hr;
}

HRESULT CTrivial::HandleFullScreenModeChanges()
{
    HRESULT hr;
    hr = m_pDD->TestCooperativeLevel();

    if( SUCCEEDED( hr ) )
    {
        // This means that mode changes had taken place, surfaces
        // were lost but still we are in the original mode, so we
        // simply restore all surfaces and keep going.
        if( FAILED( m_pDD->RestoreAllSurfaces() ) )
            return hr;
    }
    else if( hr == DDERR_NOEXCLUSIVEMODE )
    {
        // This means that some app took exclusive mode access
        // we need to sit in a loop till we get back the exclusive mode.
        do
        {
            Sleep( 500 );
        } while( DDERR_NOEXCLUSIVEMODE == 
                 (hr = m_pDD->TestCooperativeLevel()) );

        if( SUCCEEDED( hr ) )
        {
            // This means that the exclusive mode app relinquished its 
            // control and we are back to the safe mode, so simply restore
            if( FAILED( m_pDD->RestoreAllSurfaces() ) )
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

LRESULT CALLBACK WndProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
    switch(uMsg)
    {
    case WM_ACTIVATEAPP:
        if( !g_pTrivial )
            break;
        if( (BOOL)wParam )
            g_pTrivial->RestartDrawing();
        else
            g_pTrivial->PauseDrawing();
        break;
    case WM_CREATE:
        SetTimer(hwnd, 1, 1, NULL );
        break;
    case WM_CLOSE:
        PostQuitMessage(0);
        break;
    case WM_DESTROY:
        KillTimer(hwnd, 1);
        break;
    case WM_TIMER:
        if ( g_pTrivial && g_pTrivial->m_bActive )
            g_pTrivial->m_fAngle += 2.0f*3.141592654f / 90.0f;
        break;
    case WM_KEYDOWN:
        switch( wParam )
        {
        case VK_ESCAPE:
            PostQuitMessage(0);
            break;
        }
        break;
    case WM_COMMAND:
        if ( 0 == HIWORD(wParam) )
        {
            switch ( LOWORD(wParam) )
            {
            case ID_FILE_EXIT:
                PostQuitMessage(0);
                return 0;
            }
        }
        break;
    case WM_ENTERMENULOOP:
        if ( g_pTrivial ) 
            g_pTrivial->PauseDrawing();
        break;
    case WM_EXITMENULOOP:
        if ( g_pTrivial ) 
            g_pTrivial->RestartDrawing();
        break;

    case WM_SETCURSOR:
        SetCursor(NULL);
        break;

    default:
        break;
    }

    return DefWindowProc(hwnd,uMsg,wParam,lParam);
}

int PASCAL WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, 
    LPSTR lpszCmdLine, int nCmdShow) 
{
    HRESULT     hr;    
    MSG         msg; 
    WNDCLASS    wc; 
    int         ret = 0;

    hr = D3DXInitialize();
    if( FAILED(hr))
    {
        InterpretError(hr);
        ret = -1;
        goto Exit;
    }

    g_pTrivial = new CTrivial; // set up our data AFTER starting up d3dx!
    if( !g_pTrivial )
    {
        ret = -1;
        goto Exit;
    }

    // Register the window class for the main window. 

    if (!hPrevInstance) 
    { 
        wc.style = 0; 
        wc.lpfnWndProc = (WNDPROC) WndProc; 
        wc.cbClsExtra = 0; 
        wc.cbWndExtra = 0; 
        wc.hInstance = hInstance; 
        wc.hIcon = LoadIcon(hInstance, MAKEINTRESOURCE(IDI_APP_ICON));
        wc.hCursor = (HCURSOR) NULL;
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

    g_pTrivial->m_hwndMain = CreateWindow(
                                APP_NAME, 
                                APP_NAME, 
                                WS_POPUP, 
                                CW_USEDEFAULT, 
                                CW_USEDEFAULT, 
                                640, 
                                480, 
                                (HWND) NULL, 
                                (HMENU) NULL, 
                                hInstance,
                                (LPVOID) NULL); 

    // If the main window cannot be created, terminate 
    // the application. 

    if (!g_pTrivial->m_hwndMain) 
    {
        ret = -1;
        goto Exit;
    }

    ShowWindow(g_pTrivial->m_hwndMain, nCmdShow);
    UpdateWindow(g_pTrivial->m_hwndMain);


    hr = g_pTrivial->InitD3DX();
    if ( FAILED(hr) )
    {
        InterpretError(hr);
        ret = -1;
        goto Exit;
    }

    hr = g_pTrivial->InitRenderer();
    if ( FAILED(hr) )
    {
        InterpretError(hr);
        ret = -1;
        goto Exit;
    }

    // Start the message loop. 

    // Now we're ready to recieve and process Windows messages.
    BOOL bGotMsg;
    PeekMessage( &msg, NULL, 0U, 0U, PM_NOREMOVE );
    while( WM_QUIT != msg.message  )
    {
        bGotMsg = PeekMessage( &msg, NULL, 0U, 0U, PM_REMOVE );

        if( bGotMsg )
        {
            TranslateMessage( &msg );
            DispatchMessage( &msg );
        }
        else
        {
            if( g_pTrivial && g_pTrivial->m_bActive )
            {
                hr = g_pTrivial->Draw();
                if( FAILED(hr) )
                {
                    InterpretError(hr);
                    g_pTrivial->m_bD3DXReady = FALSE;
                    PostQuitMessage(0);
                }
            }
            else
                WaitMessage();
        }
    }

    delete g_pTrivial; // clean up our data BEFORE shutting down d3dx!
    ret = msg.wParam;

Exit:
    hr = D3DXUninitialize();
    if( FAILED(hr) )
    {
        InterpretError(hr);
        ret = -1;
    }
    return ret;
}
