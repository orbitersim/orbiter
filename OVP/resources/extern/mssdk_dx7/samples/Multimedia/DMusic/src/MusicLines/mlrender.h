//
// MLRender.h
//
// Graphics interface for MusicLines
//
// Copyright (c) 1998-1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------

#ifndef _MLRender_
#define _MLRender_

#include <ddraw.h>

// Loaded tiles are 3 16x16 (head, tail, dead)
//
#define TILES_X     (3*16)
#define TILES_Y     (16)

// Each score digit is 32x32
//
#define SCORE_X     (32)
#define SCORE_Y     (32)

// We always want to run 640x480
//
#define GAME_WIDTH  (640)
#define GAME_HEIGHT (480)

class BitmapSurface;        // Private representation of bitmaps 
class OptimalPalette;       // Optimal palette generation class

class GraphicsEngine 
{
public:
    GraphicsEngine(LPCSTR ClassName, BOOL fFullScreen);
    ~GraphicsEngine();

    // Enter creates the main window, enters graphics mode, and returns the HWND (or NULL if something went wrong).
    // The class is registered by WinMain so it has control of the WndProc and message loop.
    //
    // Leave undoes Enter and returns to Windows.
    //
    HWND Enter();
    void Leave();

    // Because we want to leave the option open for loading player bitmaps in the future, bitmaps are parsed and a palette
    // calculated at level startup time. BeginLevelLoad and EndLevelLoad wrap calls into the graphics code for all the 
    // player bitmaps (and any other bitmap resources which are added later).
    //
    BOOL BeginLevelLoad();
    BOOL EndLevelLoad();

    // LoadPlayerTiles loads a bitmap for a player. Currently these are resources attached to the executable. 
    // Bitmaps must be 16x16 256-color uncompressed BMP format.
    //
    // If supporting player-loadable bitmaps, there would be an overridden version of this function which loaded bitmaps
    // from a regular file.
    //
    BOOL LoadPlayerTiles(int PlayerNo, LPSTR ResourceName);

    // LoadPlayerScoreBitmap loads the score bitmap for a player
    // Bitmap must be 320x32, 32x32 per digit
    //
    BOOL LoadPlayerScoreBitmap(int PlayerNo, LPSTR ResourceName);
        
    // LoadBackdrop loads the background bitmap if there is one.
    // Bitmap must be 640x480
    //
    BOOL LoadBackdrop(LPSTR ResourceName);

    // RenderFrame, well..., renders the current frame. Only tiles which have changed will be repainted unless
    // some situation occurs which indicates a total repaint. A total repaint can occur because of a window message
    // from the message loop (such as app activation) or be forced by the render code on its own (if the DirectDraw
    // surface is lost).
    //
    void RenderFrame();

    // SetScore sets a player's score so it can be rendered at the top of the display. This only
    // needs to be done when the score changes.
    //
    void SetPlayerScore(int PlayerNo, int Score);

    // Shake starts a shake effect on the entire display, as if the monitor had been hit.
    //
    void Shake();

    // WndProc is called to let the graphics engine process and possibly consume a message from the main
    // WndProc.
    //
    BOOL WndProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam, LRESULT *lResult);

    // Active will return TRUE if the application is active.
    //
    inline BOOL Active() { return m_Active; }

    // Set TRUE while a dialog is up so the cursor will be visible
    //
    void SetDialogUp(BOOL fDialogUp);


private:
    BOOL                    m_FullScreen;           // Running full screen?
    LPSTR                   m_ClassName;            // Caller's class for window creation
    HINSTANCE               m_hInstance;            // Application instance
    BOOL                    m_Active;               // TRUE if app is active
    LPDIRECTDRAW            m_DDraw;                // DirectDraw interface
    LPDIRECTDRAWPALETTE     m_DDrawPalette;         // Palette for current level
    DDPIXELFORMAT           m_PixelFormat;          // Pixel format for primary surface
    HWND                    m_hWnd;                 // Window handle of main game window
    BitmapSurface*          m_Tiles[MAX_PLAYERS];   // Surfaces of tile sets
    BitmapSurface*          m_ScoreFonts[MAX_PLAYERS];   
                                                    // Surfaces of score fonts
    BitmapSurface*          m_Backdrop;             // Surface of the backdrop
    int                     m_Scores[MAX_PLAYERS];  // The scores
    int                     m_ScoreX[MAX_PLAYERS];  // Offset of score in score bar
    LPDIRECTDRAWSURFACE     m_Primary;              // Primary surface
    LPDIRECTDRAWSURFACE     m_BackBuffer;           // In full screen, attached flip buffer
                                                    // If windowed, offscreen compose buffer
    LPDIRECTDRAWCLIPPER     m_Clipper;              // In windowed case, render to window only
    BOOL                    m_Render;               // FALSE if we can't render (such as while changing modes)
    OptimalPalette*         m_pOptimalPalette;      // Optimal palette generator if we're in a palettized mode
    DWORD                   m_dwBlack;              // Color matched pixel color of black
    int                     m_Shake;                // If shaking, index into shake table
    BOOL					m_DialogUp;				// TRUE if displaying a dialog
            
private:
    HWND EnterFullScreen();
    HWND EnterWindowed();

    LPDIRECTDRAWSURFACE SurfaceFromBitmap(LPBYTE pbBitmapData, DWORD cbBitmapData, LONG cx, LONG cy);    
    HRESULT RenderToSurface();
    HRESULT FlipFullScreen();
    HRESULT FlipWindowed();
    BOOL RestoreSurfaces();
    BOOL QueryNewPalette(HWND hWnd);
};

extern GraphicsEngine *theGraphicsEngine;

#endif // _MLRender_
