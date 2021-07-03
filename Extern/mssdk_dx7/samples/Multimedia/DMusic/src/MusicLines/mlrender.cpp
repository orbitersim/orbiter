// 
// MLRender.cpp
//
// Copyright (c) 1998-1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------

#include <windows.h>
#include <assert.h>
#include <ddraw.h>

#include "Debug.h"
#include "MusicLines.h"
#include "MLOptPal.h"
#include "MLRender.h"
#include "MLGame.h"
#include "MLBmpSrf.h"

#define BITMAP_COLORS   (256)               // Max colors available
#define BITMAP_TYPE     ((WORD)0x4D42)      // BM

#define PALETTE_SIGNIFICANT_BITS     (6)    // Bits which count as significant in building the
                                            // optimal palette

#define DWORD_ROUNDUP(x) (((x) + 3) & ~3)  

// Shake offsets. When Shake() method is called, these get added to the origin of
// all the rendered bitmaps so the image on the screen appears to shake.
//
static POINT ShakeOffsets[] =
{
    { 0, 3 },
    {-3, 0 },
    { 0,-2 },
    { 2, 0 },
    { 0, 1 },
};
const int nShakeOffsets = sizeof(ShakeOffsets) / sizeof(ShakeOffsets[0]);

// GraphicsEngine::GraphicsEngine
//
GraphicsEngine::GraphicsEngine(
    LPCSTR ClassName,
    BOOL FullScreen)
{
    m_ClassName = new char[strlen(ClassName) + 1];
    strcpy(m_ClassName, ClassName);
    m_FullScreen = FullScreen;
    m_hWnd = NULL;
    m_DDraw = NULL;        
    m_DDrawPalette = NULL;
    m_BackBuffer = NULL;
    m_Primary = NULL;
    m_Clipper = NULL;
    m_Render = TRUE;
    m_pOptimalPalette = NULL;
    m_DialogUp = FALSE;

    ZeroMemory(m_Tiles, sizeof(m_Tiles));
    ZeroMemory(m_ScoreFonts, sizeof(m_ScoreFonts));
    ZeroMemory(m_Scores, sizeof(m_Scores));

    m_Backdrop = NULL;

    m_ScoreX[0] = 128;
    m_ScoreX[1] = 448;

    m_Shake = nShakeOffsets;
}

// GraphicsEngine::~GraphicsEngine
//
GraphicsEngine::~GraphicsEngine()
{
    Leave();

    for (int PlayerNo = 0; PlayerNo < MAX_PLAYERS; PlayerNo++)
    {
        if (m_Tiles[PlayerNo])
        {
            delete m_Tiles[PlayerNo];
            m_Tiles[PlayerNo] = NULL;
        }
  
        if (m_ScoreFonts[PlayerNo])
        {
            delete m_ScoreFonts[PlayerNo];
            m_ScoreFonts[PlayerNo] = NULL;
        }
    }

    if (m_Backdrop)
    {
        delete m_Backdrop;
    }


    if (HIWORD(m_ClassName))
    {
        delete[] m_ClassName;
    }
}

// GraphicsEngine::Enter
//
// Instantiate DirectDraw and all other objects needed to start rendering frames.
// This method is called by the mainline game code at startup and by GraphicsEngine::WndProc
// on an Alt-Enter. Calling Leave() followed by Enter() releases and recreates everything
// except the window itself. This allows us to switch between windowed and full screen 
// activation without having to also release other DirectX components like DirectMusic or
// DirectSound.
//
HWND GraphicsEngine::Enter()
{
    BOOL            Success = FALSE;
    HWND            hWnd;

    // If we can't get a DirectDraw object, all else is moot.
    //
    HRESULT hr = DirectDrawCreate(NULL,               // Active display driver
                                  &m_DDraw,           // Returned pointer to interface
                                  NULL);              // No aggregation

    if (FAILED(hr))
    {
        TraceMsg("DirectDrawCreate", hr);
        return (HWND)NULL;
    }                    

    if (m_FullScreen)
    {
        hWnd = EnterFullScreen();
    }
    else
    {
        hWnd = EnterWindowed();
    }

    if (!hWnd)
    {
        return hWnd;
    }

    // If palettized, then create an optimal palette object. 
    //
    if (m_PixelFormat.dwFlags & DDPF_PALETTEINDEXED8)
    {
        m_pOptimalPalette = new OptimalPalette(PALETTE_SIGNIFICANT_BITS);
    }

    // If there were already resources loaded, then we just switched to/from windowed/fullscreen.
    // Reset resources to new ddraw and make sure they are restored
    //
    if (m_Tiles[0])
    {
        for (int PlayerNo = 0; PlayerNo < MAX_PLAYERS; PlayerNo++)
        {
            assert(m_Tiles[PlayerNo]);
            assert(m_ScoreFonts[PlayerNo]);

            Trace(0, "Player %d: m_DDraw %08lX", PlayerNo, m_DDraw);
            m_Tiles[PlayerNo]->SetDDraw(m_DDraw);
            m_ScoreFonts[PlayerNo]->SetDDraw(m_DDraw);
        }

        // Backdrop is optional
        //
        if (m_Backdrop)
        {
            m_Backdrop->SetDDraw(m_DDraw);
        }

        EndLevelLoad();
    }

    return hWnd;
}

// GraphicsEngine::Leave
//
// Clean up all resources except the main window.
//
void GraphicsEngine::Leave()
{
    if (m_Primary)
    {
        m_Primary->Release();
        m_Primary = NULL;
    }

    if (m_BackBuffer)
    {
        if (!m_FullScreen) 
        {
            m_BackBuffer->Release();
        }
        m_BackBuffer = NULL;
    }

    if (m_DDrawPalette)
    {
        m_DDrawPalette->Release();
        m_DDrawPalette = NULL;
    }

    if (m_DDraw)
    {
        if (m_FullScreen)
        {
            m_DDraw->RestoreDisplayMode();
        }

        m_DDraw->Release();
        m_DDraw = NULL;  
    }

    if (m_pOptimalPalette)
    {
        delete m_pOptimalPalette;
        m_pOptimalPalette = NULL;
    }

    // If there are resources, force them to lose their ddraw handles.
    // The actual bitmap surface objects are not deleted; they are needed
    // to restore the bitmaps if Enter() is called again. If the game
    // is unloading, they will get freed in the destructor.
    //
    if (m_Tiles[0])
    {
        for (int PlayerNo = 0; PlayerNo < MAX_PLAYERS; PlayerNo++)
        {
            assert(m_Tiles[PlayerNo]);
            assert(m_ScoreFonts[PlayerNo]);

            m_Tiles[PlayerNo]->SetDDraw(NULL);
            m_ScoreFonts[PlayerNo]->SetDDraw(NULL);
        }

        if (m_Backdrop)
        {
            m_Backdrop->SetDDraw(NULL);
        }
    }
}

// GraphicsEngine::LoadPlayerTiles
//
// Load a set of tiles for the given player.
//
// In all the helpers which load bitmaps, nothing is actually done but the allocation 
// of a matching BitmapSurface object which tracks the resource name or file name. 
// On EndLevelLoad, a palette will be built based on the loaded art, and the bitmaps
// will be restored and loaded based on that palette. This allows the addition of
// user-supplied tiles.
//
// A player tile is a 48x16 bitmap which contains:
// (0,0)  - (16,16) Player head
// (16,0) - (32,16) Player tail
// (32,0) - (48,16) Dead player
//
BOOL GraphicsEngine::LoadPlayerTiles(
    int PlayerNo, 
    LPSTR ResourceName)
{
    if (PlayerNo < 0 || PlayerNo >= MAX_PLAYERS)
    {
        Trace(0, "LoadPlayerTiles: Bad playerno");
        return FALSE;
    }

    if (m_Tiles[PlayerNo])
    {
        delete m_Tiles[PlayerNo];
    }

    assert(m_DDraw);
    m_Tiles[PlayerNo] = new BitmapSurface(m_DDraw, ResourceName, 0, 0);

    return TRUE;
}

// GraphicsEngine::LoadPlayerScoreBitmap
//
// Creates a BitmapSurface for the score digits for the given player.
//
// Bitmap is 320x32 and contains 32x32 regions of the digits 0 through 9.
//
BOOL GraphicsEngine::LoadPlayerScoreBitmap(
    int PlayerNo, 
    LPSTR ResourceName)
{
    if (PlayerNo < 0 || PlayerNo >= MAX_PLAYERS)
    {
        Trace(0, "LoadPlayerScoreBitmap: Bad playerno");
        return FALSE;
    }

    if (m_ScoreFonts[PlayerNo])
    {
        delete m_ScoreFonts[PlayerNo];
    }

    assert(m_DDraw);
    m_ScoreFonts[PlayerNo] = new BitmapSurface(m_DDraw, ResourceName, 0, 0);

    return TRUE;
}

// GraphicsEngine::LoadBackdrop
//
BOOL GraphicsEngine::LoadBackdrop(
    LPSTR ResourceName)
{
    if (m_Backdrop)
    {
        delete m_Backdrop;
    }

    assert(m_DDraw);
    m_Backdrop = new BitmapSurface(m_DDraw, ResourceName, 0, 0);

    return TRUE;
}
// GraphicsEngine::BeginLevelLoad
//
// Signifies the start of loading art.
//
BOOL GraphicsEngine::BeginLevelLoad()
{
    return TRUE;
}

// GraphicsEngine::EndLevelLoad
//
// Called after all the art has been loaded. Generates an optimal palette if one is required.
// (m_pOptimalPalette will only exist if the current graphics mode is palettized).
// Then restore all surfaces, which also makes sure the bitmap data has been loaded.
//
BOOL GraphicsEngine::EndLevelLoad()
{
    HRESULT hr;

    // If m_pOptimalPalette exists, then we are in a mode which requires such a palette
    // to be built.
    //
    if (m_pOptimalPalette)
    {
        if (m_DDrawPalette)
        {
            m_DDrawPalette->Release();
            m_DDrawPalette = NULL;
        }

        m_pOptimalPalette->Clear();

        for (int PlayerNo = 0; PlayerNo < 2; PlayerNo++)
        {
            assert(m_Tiles[PlayerNo]);
            assert(m_ScoreFonts[PlayerNo]);

            m_Tiles[PlayerNo]->AddToOptimalPalette(m_pOptimalPalette);
            m_ScoreFonts[PlayerNo]->AddToOptimalPalette(m_pOptimalPalette);
        }

        if (m_Backdrop)
        {
            m_Backdrop->AddToOptimalPalette(m_pOptimalPalette);
        }

        PALETTEENTRY pal[MAX_PALETTE_ENTRIES];

        m_pOptimalPalette->GeneratePalette(pal);

        for (int idx = 0; idx < MAX_PALETTE_ENTRIES; ++idx)
        {
            Trace(0, "Index %3d  R %03d G %03d B %03d", idx, pal[idx].peRed, pal[idx].peGreen, pal[idx].peBlue);
        }
   
        hr = m_DDraw->CreatePalette(DDPCAPS_8BIT,
                                    pal,
                                    &m_DDrawPalette,
                                    NULL);
        if (FAILED(hr))
        {
            TraceMsg("CreatePalette", hr);
            m_DDrawPalette = NULL;
            return FALSE;
        }

        // We have the palette, now select it.
        //

        HRESULT hr = m_Primary->SetPalette(m_DDrawPalette);
        if (FAILED(hr))
        {
            TraceMsg("SetPalette", hr);
            return FALSE;
        }
    }

    RestoreSurfaces();

    return TRUE;
}

// GraphicsEngine::SetPlayerScore
//
// Sets the score for the player so it can be rendered.
//
void GraphicsEngine::SetPlayerScore(
    int PlayerNo,
    int Score)
{
    if (PlayerNo < 0 || PlayerNo >= MAX_PLAYERS)
    {
        Trace(0, "SetPlayerScore: Bad playerno");
        return;
    }

    if (Score > 99) Score = 99;
    if (Score < 0)  Score = 0;

    m_Scores[PlayerNo] = Score;
}

// GraphicsEngine::Shake
//
// Resets the shake sequence to the start. The render code will use m_Shake as an
// index into ShakeOffsets and increment it once per frame until it runs off the end
// of the array. Since we clamp to a minimum of 35ms per frame (about 30FPS) the effect
// will be consistent across machines.
// 
void GraphicsEngine::Shake()
{
    m_Shake = 0;
}

// GraphicsEngine::RenderFrame
//
// Render a frame, taking care of surface-lost retries and flipping.
//
void GraphicsEngine::RenderFrame()
{
    HRESULT hr;

    if (!m_Render)
    {
        return;
    }

    for (;;)
    {
        hr = RenderToSurface();

        if (SUCCEEDED(hr))
        {
            break;
        }
        
        if (hr != DDERR_SURFACELOST)
        {
            return;
        }

        if (!RestoreSurfaces())
        {
            return;
        }
    }

    // We've successfully rendered to the backbuffer. Unfortunately the flip
    // can also suffer surface lost. 
    //
    for (;;)
    {
        hr = m_FullScreen ? FlipFullScreen() : FlipWindowed();
        if (SUCCEEDED(hr))
        {
            return;
        }

        if (hr != DDERR_SURFACELOST)
        {
            TraceMsg("FlipXxx()", hr);
            return;
        }

        if (!RestoreSurfaces())
        {
            return;
        }
    }
}

// GraphicsEngine::WndProc
//
// Hooks the wndproc from the mainline code. This allows the graphics code to trap
// any window message it wants without putting code in the main wndproc. 
//
// Currently it only cares about palette messages and Alt+Enter.
//
// Returns TRUE and sets lResult to indicate that a message was processed here and should
// not be passed on to the main or default wndproc.
//
BOOL GraphicsEngine::WndProc(
    HWND hWnd,
    UINT uMsg,
    WPARAM wParam,
    LPARAM lParam,
    LRESULT *lResult)
{
    switch (uMsg)
    {
        case WM_QUERYNEWPALETTE:
            *lResult = (LRESULT)QueryNewPalette(hWnd);
            return TRUE;

        case WM_ACTIVATEAPP:
            m_Active = wParam && (GetForegroundWindow() == m_hWnd);

            Trace(0, "m_Active now %d", m_Active);
            *lResult = 0;
            return TRUE;

        case WM_SETCURSOR:
            if (m_FullScreen && m_Active && !m_DialogUp)
            {
                SetCursor(NULL);
            }
            else
            {
	            SetCursor(LoadCursor(NULL, IDC_ARROW));
            }
            *lResult = 0;
            return TRUE;

        case WM_SYSKEYUP:
            if (wParam == VK_RETURN)
            {
                Leave();
                m_FullScreen = !m_FullScreen;
                if (!Enter())
                {
                    // If we could not restart for some reason, it's fatal
                    //
                    PostMessage(hWnd, WM_CLOSE, 0, 0);
                }                        
            }
            *lResult = 0;
            return TRUE;
    }

    return FALSE;
}

// GraphicsEngine::QueryNewPalette
//
// If we're running windowed and are about to receive focus, force our palette to be realized
// again.
//
BOOL GraphicsEngine::QueryNewPalette(
    HWND hWnd)
{
    if (hWnd != m_hWnd)
    {
        Trace(0, "GraphicsEngine::QueryNewPalette: hWnd not mine!");
        return FALSE;
    }

    if (m_Primary && m_DDrawPalette && !m_FullScreen)
    {
        for (int idx = 0; idx < 2; idx++)
        {
            HRESULT hr = m_Primary->SetPalette(m_DDrawPalette);
            if (SUCCEEDED(hr))
            {
                return TRUE;
            }

            if (hr == DDERR_SURFACELOST)
            {
                hr = m_Primary->Restore();
                if (FAILED(hr))
                {
                    TraceMsg("Lost surface, cannot restore on QueryNewPalette", hr);
                    return FALSE;
                }
            }
            else
            {
                TraceMsg("SetPalette on QueryNewPalette()", hr);
                return FALSE;
            }
        }
    }

    return TRUE;
}

// GraphicsEngine::EnterFullScreen
//
// Set up primary surface and back buffer for full screen activation
//
HWND GraphicsEngine::EnterFullScreen()
{
    // Try to create the window. For this we need a full-screen always on top popup.
    //
    if (!m_hWnd)
    {
        m_hWnd = CreateWindowEx(WS_EX_APPWINDOW, 
                                m_ClassName,   
                                m_ClassName,
                                WS_OVERLAPPEDWINDOW & ~WS_THICKFRAME, 
                                0, 0, 
                                GetSystemMetrics(SM_CXSCREEN), 
                                GetSystemMetrics(SM_CYSCREEN), 
                                GetDesktopWindow(), 
                                NULL, 
                                GetModuleHandle(NULL), 
                                NULL);
    }

    if (!m_hWnd)
    {
        Trace(0, "Main window creation failed");
        Leave();
        return m_hWnd;
    }

    ShowWindow(m_hWnd, SW_RESTORE);
    UpdateWindow(m_hWnd);
    SetFocus(m_hWnd);
    ShowCursor(FALSE);

    HRESULT hr = m_DDraw->SetCooperativeLevel(m_hWnd, DDSCL_EXCLUSIVE | DDSCL_FULLSCREEN);
    if (FAILED(hr)) 
    {
        TraceMsg("DirectDraw::SetCooperativeLevel", hr);
        Leave();
        return (HWND)NULL;
    }

    // Set display to 640x480 8 bit color
    //
    hr = m_DDraw->SetDisplayMode(GAME_WIDTH, GAME_HEIGHT, 8);
    if (FAILED(hr))
    {
        TraceMsg("DirectDraw::SetDisplayMode", hr);
        Leave();
        return (HWND)NULL;
    }

    // Create primary surface with one attached back buffer
    //
    DDSURFACEDESC ddsd;
    ddsd.dwSize             = sizeof(ddsd);
    ddsd.dwFlags            = DDSD_CAPS | DDSD_BACKBUFFERCOUNT;
    ddsd.ddsCaps.dwCaps     = DDSCAPS_PRIMARYSURFACE | DDSCAPS_FLIP | DDSCAPS_COMPLEX;
    ddsd.dwBackBufferCount  = 1;

    hr = m_DDraw->CreateSurface(&ddsd, 
                                &m_Primary,
                                NULL);
    if (FAILED(hr))
    {
        m_Primary = NULL;
        TraceMsg("Full screen CreateSurface", hr);
        Leave();
        return (HWND)NULL;
    }

    // Stash the pixel format. We know what it is, but the rest of the engine code is independent
    // of windowed or full-screen activation.
    //
    m_PixelFormat.dwSize = sizeof(m_PixelFormat);
    hr = m_Primary->GetPixelFormat(&m_PixelFormat);
    if (FAILED(hr))
    {
        TraceMsg("DirectDraw::GetPixelFormat", hr);
        Leave();
        return (HWND)NULL;
    }

    DDSCAPS ddscaps;
    ddscaps.dwCaps = DDSCAPS_BACKBUFFER;
    hr = m_Primary->GetAttachedSurface(&ddscaps, &m_BackBuffer);
    if (FAILED(hr))
    {
        m_BackBuffer = NULL;
        TraceMsg("GetAttachedSurface", hr);
        Leave();
        return (HWND)NULL;
    }

    return (HWND)m_hWnd;
}

// GraphicsEngine::EnterWindowed
//
// Set up primary surface and back buffer for windowed activation
//
HWND GraphicsEngine::EnterWindowed()
{
    RECT rcWindow;

    rcWindow.top        = 0;
    rcWindow.left       = 0;
    rcWindow.bottom     = GAME_HEIGHT;
    rcWindow.right      = GAME_WIDTH;

    AdjustWindowRectEx(&rcWindow,
                       WS_OVERLAPPEDWINDOW & ~WS_THICKFRAME,
                       FALSE,
                       WS_EX_APPWINDOW);

    if (!m_hWnd)
    {
        m_hWnd = CreateWindowEx(WS_EX_APPWINDOW,
                                m_ClassName,
                                m_ClassName,
                                WS_OVERLAPPEDWINDOW & ~WS_THICKFRAME & ~WS_MAXIMIZEBOX,
                                CW_USEDEFAULT,
                                CW_USEDEFAULT,
                                rcWindow.right - rcWindow.left,
                                rcWindow.bottom - rcWindow.top,
                                GetDesktopWindow(),
                                NULL,
                                GetModuleHandle(NULL),
                                NULL);
    }
    else
    {
        // Already exists, just reset the size
        //
        SetWindowPos(m_hWnd, 
                     HWND_NOTOPMOST,
                     0, 0,
                     rcWindow.right - rcWindow.left,
                     rcWindow.bottom - rcWindow.top,
                     SWP_FRAMECHANGED);
    }

    if (!m_hWnd)
    {
        Trace(0, "Main window creation failed");
        Leave();
        return m_hWnd;
    }

    ShowWindow(m_hWnd, SW_RESTORE);
    UpdateWindow(m_hWnd);
    SetFocus(m_hWnd);

    HRESULT hr = m_DDraw->SetCooperativeLevel(m_hWnd, DDSCL_NORMAL);
    if (FAILED(hr)) 
    {
        TraceMsg("DirectDraw::SetCooperativeLevel", hr);
        Leave();
        return (HWND)NULL;
    }

    DDSURFACEDESC ddsd;

    ddsd.dwSize             = sizeof(ddsd);
    ddsd.dwFlags            = DDSD_CAPS | DDSD_WIDTH | DDSD_HEIGHT;
    ddsd.ddsCaps.dwCaps     = DDSCAPS_OFFSCREENPLAIN;
    ddsd.dwWidth            = GAME_WIDTH;
    ddsd.dwHeight           = GAME_HEIGHT;

    hr = m_DDraw->CreateSurface(&ddsd,
                                &m_BackBuffer,
                                NULL);
    if (FAILED(hr))
    {
        m_BackBuffer = NULL;
        TraceMsg("CreateSurface(windowed backbuffer)", hr);
        Leave();
        return (HWND)NULL;
    }

    ddsd.dwSize = sizeof(ddsd);
    ddsd.dwFlags = DDSD_CAPS;
    ddsd.ddsCaps.dwCaps = DDSCAPS_PRIMARYSURFACE;

    hr = m_DDraw->CreateSurface(&ddsd,
                                &m_Primary,
                                NULL);
    if (FAILED(hr))
    {
        m_Primary = NULL;
        TraceMsg("DirectDraw::CreateSurface(primary)", hr);
        Leave();
        return (HWND)NULL;
    }

    // Get the caps of the primary surface to figure out if we're in a palettized
    // mode.
    //
    m_PixelFormat.dwSize = sizeof(m_PixelFormat);
    hr = m_Primary->GetPixelFormat(&m_PixelFormat);
    if (FAILED(hr))
    {
        TraceMsg("DirectDraw::GetPixelFormat", hr);
        Leave();
        return (HWND)NULL;
    }

    // Now set up a clipper for our window to make sure we can only write 
    // to the client area.
    //
    hr = m_DDraw->CreateClipper(0,          // Unused flags
                                &m_Clipper,
                                NULL);
    if (FAILED(hr))
    {
        m_Clipper = NULL;
        TraceMsg("DirectDraw::CreateClipper", hr);
        Leave();
        return (HWND)NULL;
    }

    hr = m_Clipper->SetHWnd(0, m_hWnd);
    if (FAILED(hr))
    {
        TraceMsg("DirectDraw::SetHwnd", hr);
        Leave();
        return (HWND)NULL;
    }

    hr = m_Primary->SetClipper(m_Clipper);
    if (FAILED(hr))
    {
        TraceMsg("DirectDraw::SetClipper", hr);
        Leave();
        return (HWND)NULL;
    }

    return m_hWnd;
}

// GraphicsEngine::RestoreSurfaces
//
// Restore all surfaces and reset their transparency. White (255,255,255) is the global
// color used for source transparency in all art.
//
BOOL GraphicsEngine::RestoreSurfaces()
{
    HRESULT hr;
    
    Trace(1, "GraphicsEngine::RestoreSurfaces");
    
    hr = m_Primary->Restore();
    if (FAILED(hr))
    {
        TraceMsg("Restore(Primary)", hr);
        return FALSE;
    }            

    // This isn't needed in the full screen case (the back buffer is attached to the
    // primary surface) but is harmless. In the windowed case, however, the back buffer
    // is just an offscreen surface that we blt to the primary surface from, so it
    // must be restored as well.
    //
    hr = m_BackBuffer->Restore();
    if (FAILED(hr))
    {
        TraceMsg("Restore(backbuffer)", hr);
        return FALSE;
    }

    for (int idx = 0; idx < MAX_PLAYERS; idx++)
    {
        if (m_Tiles[idx])
        {
            hr = m_Tiles[idx]->Restore();
            if (FAILED(hr))
            {
                TraceMsg("Restore(tile)", hr);
                return FALSE;
            }

            DDCOLORKEY ddck;
            if (m_PixelFormat.dwFlags & DDPF_PALETTEINDEXED8)
            {
                hr = m_Tiles[idx]->GetColorMatch(RGB(255,255,255), &ddck.dwColorSpaceLowValue);
                if (FAILED(hr))
                {
                    TraceMsg("GetColorMatch", hr);
                    ddck.dwColorSpaceLowValue = 255;
                }

                ddck.dwColorSpaceHighValue = ddck.dwColorSpaceLowValue;
            }
            else
            {
                ddck.dwColorSpaceLowValue = m_PixelFormat.dwRBitMask |
                                            m_PixelFormat.dwGBitMask |
                                            m_PixelFormat.dwBBitMask; 
                ddck.dwColorSpaceHighValue = ddck.dwColorSpaceLowValue;
            }

            hr = m_Tiles[idx]->GetSurface()->SetColorKey(DDCKEY_SRCBLT, &ddck);
            if (FAILED(hr))
            {
                TraceMsg("SetColorKey", hr);
                return FALSE;
            }
        }

        if (m_ScoreFonts[idx])
        {
            hr = m_ScoreFonts[idx]->Restore();
            if (FAILED(hr))
            {
                TraceMsg("Restore(tile)", hr);
                return FALSE;
            }

            DDCOLORKEY ddck;
            if (m_PixelFormat.dwFlags & DDPF_PALETTEINDEXED8)
            {
                hr = m_ScoreFonts[idx]->GetColorMatch(RGB(255,255,255), &ddck.dwColorSpaceLowValue);
                if (FAILED(hr))
                {
                    TraceMsg("GetColorMatch", hr);
                    ddck.dwColorSpaceLowValue = 255;
                }

                ddck.dwColorSpaceHighValue = ddck.dwColorSpaceLowValue;
            }
            else
            {
                ddck.dwColorSpaceLowValue = m_PixelFormat.dwRBitMask |
                                            m_PixelFormat.dwGBitMask |
                                            m_PixelFormat.dwBBitMask; 
                ddck.dwColorSpaceHighValue = ddck.dwColorSpaceLowValue;
            }

            hr = m_ScoreFonts[idx]->GetSurface()->SetColorKey(DDCKEY_SRCBLT, &ddck);
            if (FAILED(hr))
            {
                TraceMsg("SetColorKey", hr);
                return FALSE;
            }
        }
    }

    // NOTE: Backdrop does not need transparency
    //
    if (m_Backdrop)
    {
        hr = m_Backdrop->Restore();
        if (FAILED(hr))
        {
            TraceMsg("m_Backdrop->Restore()", hr);
            return FALSE;
        }
    }

    return TRUE;
}

// GraphicsEngine::RenderToSurface
//
// This function is the game-specific function which knows how to render the
// frame. Guaranteed to have a blt-fast capable surface. Doesn't have to handle
// client-screen transforms in windowed case or restore retries. Just blit
// the frame.
//
HRESULT GraphicsEngine::RenderToSurface()
{
    DDBLTFX     ddbltfx;
    RECT        rc;
    HRESULT     hr;
    int         iShake = 0;
    POINT       ptShake = {0,0};

    if (m_Shake < nShakeOffsets)
    {
        ptShake = ShakeOffsets[m_Shake++];
    }

    // First let's fill the back buffer with blackness
    //
    ZeroMemory(&ddbltfx, sizeof(ddbltfx));
    ddbltfx.dwSize = sizeof(ddbltfx);
    ddbltfx.dwFillColor = 0;

    rc.left     = 0;
    rc.right    = GAME_WIDTH;
    rc.top      = 0;
    rc.bottom   = GAME_HEIGHT;

    if (m_Backdrop)
    {
        hr = m_BackBuffer->BltFast(0, 0,
                                   m_Backdrop->GetSurface(),
                                   &rc,
                                   DDBLTFAST_NOCOLORKEY | DDBLTFAST_WAIT);                                     
    }
    else
    {
        hr = m_BackBuffer->Blt(&rc,
                               NULL,        // src surface and
                               NULL,        // rect
                               DDBLT_COLORFILL | DDBLT_WAIT,
                               &ddbltfx);
    }

    if (FAILED(hr))
    {
        TraceMsg("backbuffer blt", hr);
        return hr;
    }

    // Now walk and render the tile list
    //
    Tile *pTile = theGame->Arena();
    int yMax = ArenaY * 16;
    int xMax = ArenaX * 16;
    
    for (int cy = 0; cy < yMax; cy += 16)
    {
        for (int cx = 0; cx < xMax; cx += 16, pTile++)
        {
            if (pTile->GetContents() == Tile::Empty)
            {
                continue;            
            }

            rc.left = (int)pTile->GetContents();
            rc.right = rc.left + 16;
            rc.top = 0;
            rc.bottom = 16;

            hr = m_BackBuffer->BltFast(cx + ptShake.x, cy + SCORE_Y + ptShake.y,
                                       m_Tiles[pTile->GetPlayerNo()]->GetSurface(),
                                       &rc,
                                       DDBLTFAST_SRCCOLORKEY | DDBLTFAST_WAIT);
            if (FAILED(hr))
            {
                TraceMsg("BltFast inner render loop", hr);
                return hr;
            }
        }
    }

    // Render score digits
    //
    int Digit;
    for (int PlayerNo = 0; PlayerNo < MAX_PLAYERS; PlayerNo++)
    {
        if (!m_ScoreFonts[PlayerNo])
        {
            continue;
        }
        
        Digit = m_Scores[PlayerNo] / 10;

        rc.top = 0;
        rc.bottom = SCORE_Y;
        rc.left = Digit * SCORE_X;
        rc.right = rc.left + SCORE_X;
                
        m_BackBuffer->BltFast(m_ScoreX[PlayerNo] + ptShake.x, ptShake.y, 
                              m_ScoreFonts[PlayerNo]->GetSurface(),
                              &rc,
                              DDBLTFAST_SRCCOLORKEY | DDBLTFAST_WAIT);

        Digit = m_Scores[PlayerNo] % 10;

        rc.top = 0;
        rc.bottom = SCORE_Y;
        rc.left = Digit * SCORE_X;
        rc.right = rc.left + SCORE_X;
                
        m_BackBuffer->BltFast(m_ScoreX[PlayerNo] + SCORE_X + ptShake.x, ptShake.y,
                              m_ScoreFonts[PlayerNo]->GetSurface(),
                              &rc,
                              DDBLTFAST_SRCCOLORKEY | DDBLTFAST_WAIT);

    }        
        
    return S_OK;
}

// GraphicsEngine::FlipWindowed
//
// Do the equivalent of a flip in windowed activation. What this actually means
// is to BltFast the back buffer we just built into the primary surface owned by the window.
//
HRESULT GraphicsEngine::FlipWindowed()
{
    // Not a flip per se. We copy the backbuffer to the window
    //
    RECT ClientRect;
    RECT BackRect;
    POINT ClientStart;
    
    GetClientRect(m_hWnd, &ClientRect);

    ClientStart.x = 0;
    ClientStart.y = 0;
    ClientToScreen(m_hWnd, &ClientStart);

    long Width  = (GAME_WIDTH < ClientRect.right ? GAME_WIDTH : ClientRect.right);
    long Height = (GAME_HEIGHT < ClientRect.bottom ? GAME_HEIGHT : ClientRect.bottom);

    BackRect.top        = 0;
    BackRect.left       = 0;
    BackRect.bottom     = Height;
    BackRect.right      = Width;

    ClientRect.top      = ClientStart.y;
    ClientRect.left     = ClientStart.x;
    ClientRect.bottom   = ClientStart.y + Height;
    ClientRect.right    = ClientStart.x + Width;

    return m_Primary->Blt(&ClientRect,
                          m_BackBuffer,
                          &BackRect,
                          DDBLT_WAIT,
                          NULL);

    
}

// GraphicsEngine::FlipFullScreen
//
HRESULT GraphicsEngine::FlipFullScreen()
{
    HRESULT hr = m_Primary->Flip(NULL, DDFLIP_WAIT);
    if (FAILED(hr))
    {
        TraceMsg("Flip", hr);
    }

    return hr;
}

    
void GraphicsEngine::SetDialogUp(BOOL fDialogUp)
{
    if ((fDialogUp && m_DialogUp) || (!fDialogUp && !m_DialogUp))
    {
	    return;
    }

    m_DialogUp = fDialogUp;

    ShowCursor(m_DialogUp);
}
