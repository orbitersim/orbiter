//-----------------------------------------------------------------------------
// Name: GamePlay.cpp
//
// Desc: Code to implement the game play.
//
// Copyright (c) 1998-1999 Microsoft Corp.  All rights reserved.
//-----------------------------------------------------------------------------
#include <windows.h>
#include <stdio.h>
#include <stdlib.h>
#include <mmsystem.h>
#include <math.h>
#include "resource.h"




//-----------------------------------------------------------------------------
// Game global variables
//-----------------------------------------------------------------------------
#define NUM_ROWS      8
#define WM_GAMEOVER   (WM_USER+1)

HBITMAP g_hbmPlayerLeft,    g_hbmPlayerRight, g_hbmPlayerExploding;
HBITMAP g_hbmWeaponL1,      g_hbmWeaponL2,    g_hbmWeaponR1, g_hbmWeaponR2;
HBITMAP g_hbmBarrierLeft,   g_hbmBarrierRight;
HBITMAP g_hbmExplosion1,    g_hbmExplosion2;
HBITMAP g_hbmShieldPowerUp, g_hbmTimeStopPowerUp;
HBITMAP g_hbmBackBuffer;

struct Weapon
{
    HBITMAP hbm;
    FLOAT   fX;
    int     iVelocity;
    int     x, y, w, h;
    int     iExploding;
    int     iPointValue;
    BOOL    bVisible;
    int     iPowerUp;
};

#define NUM_WEAPONS 5
Weapon  g_Weapons[NUM_WEAPONS];

#define NUM_STARS 300
POINT g_Stars[NUM_STARS];

FLOAT   g_fSpaceshipY         = 0.0f;
int     g_iSpaceshipY         = 0;
int     g_iSpaceshipX         = 0;
BOOL    g_bSpaceshipFire      = FALSE;
BOOL    g_bSpaceshipFacesLeft = TRUE;
FLOAT   g_fSpeedScale         = 1.0f;
FLOAT   g_fFireRechargeTime   = 0.0f;
FLOAT   g_fShieldTime         = 0.0f;
FLOAT   g_fTimeStopTime       = 0.0f;
DWORD   g_dwScore             = 0L;
BOOL    g_bGameOver           = FALSE;
FLOAT   g_fLastTime;
FLOAT   g_fTimeLapse;
WORD    g_wGameWidth;
WORD    g_wGameHeight;

extern FLOAT g_fCurrentTime; // Game time in seconds




//-----------------------------------------------------------------------------
// Inline helper functions
//-----------------------------------------------------------------------------
inline HBITMAP LoadBitmapFromResource( HINSTANCE hInst, DWORD dwID )
{
    return (HBITMAP)LoadImage( hInst, MAKEINTRESOURCE(dwID),
                               IMAGE_BITMAP, 0, 0, LR_CREATEDIBSECTION );
}




//-----------------------------------------------------------------------------
// Name: InitializeGameObjects()
// Desc: Creates the bitmaps for the game. Also, initializes the weapons.
//-----------------------------------------------------------------------------
VOID InitializeGameObjects( HINSTANCE hInst, HWND hWnd )
{
    g_hbmPlayerLeft      = LoadBitmapFromResource( hInst, IDB_PLAYER_LEFT );
    g_hbmPlayerRight     = LoadBitmapFromResource( hInst, IDB_PLAYER_RIGHT );
    g_hbmPlayerExploding = LoadBitmapFromResource( hInst, IDB_PLAYER_EXPLODING );
    g_hbmBarrierLeft     = LoadBitmapFromResource( hInst, IDB_BARRIER_LEFT );
    g_hbmBarrierRight    = LoadBitmapFromResource( hInst, IDB_BARRIER_RIGHT );
    g_hbmWeaponL1        = LoadBitmapFromResource( hInst, IDB_WEAPON_L1 );
    g_hbmWeaponL2        = LoadBitmapFromResource( hInst, IDB_WEAPON_L2 );
    g_hbmWeaponR1        = LoadBitmapFromResource( hInst, IDB_WEAPON_R1 );
    g_hbmWeaponR2        = LoadBitmapFromResource( hInst, IDB_WEAPON_R2 );
    g_hbmExplosion1      = LoadBitmapFromResource( hInst, IDB_EXPLOSION1 );
    g_hbmExplosion2      = LoadBitmapFromResource( hInst, IDB_EXPLOSION2 );
    g_hbmShieldPowerUp   = LoadBitmapFromResource( hInst, IDB_SHIELD_POWERUP );
    g_hbmTimeStopPowerUp = LoadBitmapFromResource( hInst, IDB_TIMESTOP_POWERUP );

    // Get the game window's dimensions
    RECT rc;
    GetClientRect( hWnd, &rc );
    g_wGameWidth  = (WORD)(rc.right - rc.left);
    g_wGameHeight = (WORD)(rc.bottom - rc.top);
    g_iSpaceshipX = g_wGameWidth/2;


    // Create a back buffer for rendering
    HDC hDC = GetDC( hWnd );
    g_hbmBackBuffer = CreateCompatibleBitmap( hDC, g_wGameWidth, g_wGameHeight );
    ReleaseDC( hWnd, hDC );
}


    

//-----------------------------------------------------------------------------
// Name: DeleteGameObjects()
// Desc: Delete objects used by the game
//-----------------------------------------------------------------------------
VOID DeleteGameObjects()
{
    DeleteObject( g_hbmPlayerLeft );
    DeleteObject( g_hbmPlayerRight );
    DeleteObject( g_hbmPlayerExploding );
    DeleteObject( g_hbmBarrierLeft );
    DeleteObject( g_hbmBarrierRight );
    DeleteObject( g_hbmWeaponL1 );
    DeleteObject( g_hbmWeaponL2 );
    DeleteObject( g_hbmWeaponR1 );
    DeleteObject( g_hbmWeaponR2 );
    DeleteObject( g_hbmExplosion1 );
    DeleteObject( g_hbmExplosion2 );
    DeleteObject( g_hbmShieldPowerUp );
    DeleteObject( g_hbmTimeStopPowerUp );
    DeleteObject( g_hbmBackBuffer );
}




//-----------------------------------------------------------------------------
// Name: StartNewGame()
// Desc: Resets all game variables for a new game.
//-----------------------------------------------------------------------------
VOID StartNewGame()
{
    // Initialize game variables
    ZeroMemory( &g_Weapons, NUM_WEAPONS*sizeof(Weapon) );
    g_fSpaceshipY         = 0.0f;
    g_iSpaceshipY         = 0;
    g_bSpaceshipFire      = FALSE;
    g_bSpaceshipFacesLeft = TRUE;
    g_fFireRechargeTime   = 0.0f;
    g_fSpeedScale         = 1.0f;
    g_fShieldTime         = 0.0f;
    g_fTimeStopTime       = 0.0f;
    g_dwScore             = 0L;
    g_bGameOver           = FALSE;
    g_fLastTime           = timeGetTime() * 0.001f;

    // Generate some stars for the background
    srand( timeGetTime() );
    for( int i=0; i<NUM_STARS; i++ )
    {
        g_Stars[i].x = (LONG)(300.0f*rand())/RAND_MAX;
        g_Stars[i].y = (LONG)(350.0f*rand())/RAND_MAX;
    }
}




//-----------------------------------------------------------------------------
// Name: PauseGame()
// Desc: Pauses or unpauses the game
//-----------------------------------------------------------------------------
VOID PauseGame( BOOL bPause )
{
    // Sync up to the system time if unpausing the game
    if( FALSE == bPause )
    {
        g_fLastTime = timeGetTime() * 0.001f;
    }
}



    
//-----------------------------------------------------------------------------
// Name: MovePlayer()
// Desc: Move the players ship
//-----------------------------------------------------------------------------
VOID MovePlayer( BOOL bLeft, BOOL bRight, BOOL bUp, BOOL bDown, BOOL bFire )
{
    if( g_bGameOver )
        return;

    // Determine the time (in seconds) since the last frame
    g_fTimeLapse = g_fCurrentTime - g_fLastTime;
    g_fLastTime  = g_fCurrentTime;

    // Update the variables for the player's ship
    if( bLeft )   g_bSpaceshipFacesLeft = TRUE;
    if( bRight )  g_bSpaceshipFacesLeft = FALSE;
    if( bUp )     g_fSpaceshipY -= g_fTimeLapse * 100.0f;
    if( bDown )   g_fSpaceshipY += g_fTimeLapse * 100.0f;
    
    // Bounds checking for the spaceship
    if( g_fSpaceshipY < 20.0f )               g_fSpaceshipY = 20.0f;
    if( g_fSpaceshipY > g_wGameHeight-20.0f ) g_fSpaceshipY = g_wGameHeight - 20.0f;
    g_iSpaceshipY = (int)g_fSpaceshipY;

    if( bFire )
        if( g_fFireRechargeTime - g_fCurrentTime < 0.0f )
            g_bSpaceshipFire = TRUE;
}




//-----------------------------------------------------------------------------
// Name: AnimateScene()
// Desc: Update the objects in the scene.
//-----------------------------------------------------------------------------
VOID AnimateScene( HWND hWnd )
{
    // Update the weapons
    for( int i=0; i<NUM_WEAPONS; i++ )
    {
        if( g_Weapons[i].bVisible )
        {
            // Update weapons (unless time is stopped, or game is over)
            if( FALSE == g_bGameOver )
                if( g_fTimeStopTime - g_fCurrentTime < 0.0f )
                    g_Weapons[i].fX += g_fSpeedScale * g_Weapons[i].iVelocity * g_fTimeLapse;
            
            if( g_Weapons[i].iExploding )
            {
                if( g_Weapons[i].iExploding % 2 )
                    g_Weapons[i].hbm = g_hbmExplosion2;
                else
                    g_Weapons[i].hbm = g_hbmExplosion1;
                if( --g_Weapons[i].iExploding == 0 )
                    g_Weapons[i].bVisible = FALSE;
            }

            // Check if the weapon went off the screen
            if( (int)g_Weapons[i].fX > g_wGameWidth )
                g_Weapons[i].bVisible = FALSE;
            if( (int)g_Weapons[i].fX + g_Weapons[i].w < 0 )
                g_Weapons[i].bVisible = FALSE;
        }
    }

    // Insert new weapons
    for( i=0; i<NUM_WEAPONS; i++ )
    {
        if( g_Weapons[i].bVisible == FALSE )
        {
            // Select a new velocity 
            g_Weapons[i].iVelocity = 35*((timeGetTime()%7)-3);
            g_Weapons[i].iPointValue = abs((timeGetTime()%7)-3);

            if( g_Weapons[i].iVelocity )
            {
                g_Weapons[i].bVisible = TRUE;

                static int PowerUpCounter = 0;
                PowerUpCounter++;

                // Assign a bitmap, which may be a powerup, or a left- or
                // right-facing weapon
                if( 10 == PowerUpCounter )
                {
                    g_Weapons[i].iPowerUp    = 1;
                    g_Weapons[i].hbm         = g_hbmShieldPowerUp;
                    g_Weapons[i].w           = 20;
                    g_Weapons[i].iPointValue = 1000;
                }
                else if( 20 == PowerUpCounter )
                {
                    g_Weapons[i].iPowerUp    = 2;
                    g_Weapons[i].hbm         = g_hbmTimeStopPowerUp;
                    g_Weapons[i].w           = 20;
                    g_Weapons[i].iPointValue = 1000;

                    PowerUpCounter = 0; // Reset the powerup counter
                }
                else
                {
                    g_Weapons[i].iPowerUp = 0;

                    int b = timeGetTime()%2;

                    if( b == 0 )
                    {
                        if( g_Weapons[i].iVelocity < 0 )
                            g_Weapons[i].hbm      = g_hbmWeaponR1;
                        else
                            g_Weapons[i].hbm      = g_hbmWeaponL1;

                        g_Weapons[i].w            =  96;
                        g_Weapons[i].iPointValue *= 200;
                    }
                    else
                    {
                        if( g_Weapons[i].iVelocity < 0 )
                            g_Weapons[i].hbm      = g_hbmWeaponR2;
                        else
                            g_Weapons[i].hbm      = g_hbmWeaponL2;

                        g_Weapons[i].w            =  48;
                        g_Weapons[i].iPointValue *= 100;
                    }
                }

                if( g_Weapons[i].iVelocity < 0 )
                    g_Weapons[i].fX = (FLOAT)g_wGameWidth;
                else
                    g_Weapons[i].fX = (FLOAT)-g_Weapons[i].w;

                g_Weapons[i].y = (timeGetTime()%NUM_ROWS);
                g_Weapons[i].h = 20;

                for( int j=0; j<NUM_WEAPONS; j++ )
                {
                    if( i == j )
                        continue;
                    if( g_Weapons[j].bVisible == FALSE )
                        continue;
                    if( g_Weapons[i].y == g_Weapons[j].y )
                    {
                        g_Weapons[i].y+=3;
                        if( g_Weapons[i].y >= NUM_ROWS )
                            g_Weapons[i].y -= NUM_ROWS;
                        j=-1;
                    }
                }
            }

            // Only insert max of 1 new weapon per frame
            break;
        }
    }

    // Check for collision
    for( i=0; i<NUM_WEAPONS; i++ )
    {
        if( g_Weapons[i].bVisible )
        {
            int x = (int)g_Weapons[i].fX;
            int y = g_Weapons[i].y*40 + 20;
            int w = g_Weapons[i].w;
            int h = g_Weapons[i].h;

            if( ( x > g_iSpaceshipX+11 ) || ( x+w < g_iSpaceshipX-11 ) )
                continue;
            if( ( y > g_iSpaceshipY+11 ) || ( y+h < g_iSpaceshipY-11 ) )
                continue;
            if( g_Weapons[i].iExploding )
                continue;

            // Else, the player hit object
            if( g_Weapons[i].iPowerUp == 1 )
            {
                g_fShieldTime = g_fCurrentTime + 5.0f; // Shields last for 5 seconds
                g_dwScore += g_Weapons[i].iPointValue;
                g_Weapons[i].bVisible = FALSE;
            }
            if( g_Weapons[i].iPowerUp == 2 )
            {
                g_fTimeStopTime = g_fCurrentTime + 3.0f; // Stop time for 3 seconds
                g_dwScore += g_Weapons[i].iPointValue;
                g_Weapons[i].bVisible = FALSE;
            }
            else // Player hit weapon
            {
                if( g_fShieldTime - g_fCurrentTime < 0.0f )
                {
                    g_bGameOver = TRUE;
                    SendMessage( hWnd, WM_GAMEOVER, 0, 0 );
                }
                else
                {
                    g_Weapons[i].hbm        = g_hbmExplosion1;
                    g_Weapons[i].fX        += (w-48)/2; 
                    g_Weapons[i].w          = 48; 
                    g_Weapons[i].iExploding = 15;
                    g_dwScore              += g_Weapons[i].iPointValue;
                }
            }
        }
    }

    // Handle the laser beam
    if( TRUE == g_bSpaceshipFire )
    {
        int beamX = g_iSpaceshipX;
        int beamY = g_iSpaceshipY;
        g_bSpaceshipFire = FALSE;

        if( beamY%40 > 20 ) // Don't fire at barriers
        {
            g_fFireRechargeTime = g_fCurrentTime + 0.5f;

            // Blow up weapons
            for( int i=0; i<NUM_WEAPONS; i++ )
            {
                int x = (int)g_Weapons[i].fX;
                int y = g_Weapons[i].y*40 + 20;
                int w = g_Weapons[i].w;
                int h = g_Weapons[i].h;

                if( FALSE == g_Weapons[i].bVisible )
                    continue;
                if( ( beamY < y ) || ( beamY >= y+h ) )
                    continue;

                if( ( g_bSpaceshipFacesLeft && x < g_iSpaceshipX ) ||
                    ( !g_bSpaceshipFacesLeft && x > g_iSpaceshipX ) )
                {
                    g_Weapons[i].hbm = g_hbmExplosion1;
                    g_Weapons[i].fX += (w-48)/2; 
                    g_Weapons[i].w   = 48; 
                    g_Weapons[i].iExploding = 15;
                    g_dwScore += g_Weapons[i].iPointValue;
                }
            }
        }
    }

    // Update the speed factor
    g_fSpeedScale = 1.0f + (g_dwScore/10000.0f);
}




//-----------------------------------------------------------------------------
// Name: RenderScene()
// Desc: Render the scene to a backbuffer, and blt to the HWND.
//-----------------------------------------------------------------------------
VOID RenderScene( HWND hWnd )
{
    // Create the DC's for rendering the frame into a backbuffer
    HDC hdcBackBuffer = CreateCompatibleDC( NULL );
    HDC hdcBitmap     = CreateCompatibleDC( NULL );

    // Create a brush and pen
    HPEN   hPen      = (HPEN)CreatePen( PS_SOLID, 1, 0x00ffffff );
    HPEN   hOldPen   = (HPEN)SelectObject( hdcBackBuffer, hPen );
    HBRUSH hBrush    = (HBRUSH)CreateSolidBrush( 0x00000000 );
    HBRUSH hOldBrush = (HBRUSH)SelectObject( hdcBackBuffer, hBrush );

    // Fill the background
    RECT rc;
    GetClientRect( hWnd, &rc );
    SelectObject( hdcBackBuffer, g_hbmBackBuffer );
    FillRect( hdcBackBuffer, &rc, hBrush );

    // Draw stars for the background
    for( int i=0; i<NUM_STARS; i++ )
    {
        MoveToEx( hdcBackBuffer, g_Stars[i].x,   g_Stars[i].y, NULL );
        LineTo(   hdcBackBuffer, g_Stars[i].x+1, g_Stars[i].y+1 );
    }

    // Draw the barriers
    for( i=0; i<=NUM_ROWS; i++ )
    {
        SelectObject( hdcBitmap, g_hbmBarrierLeft );
        BitBlt( hdcBackBuffer, 0, i*40, 140, 20, hdcBitmap, 0, 0, SRCCOPY );
        SelectObject( hdcBitmap, g_hbmBarrierRight );
        BitBlt( hdcBackBuffer, g_wGameWidth-141, i*40, 140, 20, hdcBitmap, 0, 0,
                SRCCOPY );
    }

    // Draw the weapons
    for( i=0; i<NUM_WEAPONS; i++ )
    {
        if( g_Weapons[i].bVisible )
        {
            if( g_Weapons[i].iExploding )
            {
                if( g_Weapons[i].iExploding % 2 )
                    g_Weapons[i].hbm = g_hbmExplosion2;
                else
                    g_Weapons[i].hbm = g_hbmExplosion1;
            }

            int x = (int)g_Weapons[i].fX;
            int y = g_Weapons[i].y*40 + 20;
            int w = g_Weapons[i].w;
            int h = g_Weapons[i].h;

            SelectObject( hdcBitmap, g_Weapons[i].hbm );
            BitBlt( hdcBackBuffer, x, y, w, h, hdcBitmap, 0, 0, SRCCOPY );
        }
    }

    // Draw the laser beam
    if( g_fFireRechargeTime - g_fCurrentTime > 0.45f )
    {
        MoveToEx( hdcBackBuffer, g_iSpaceshipX, g_iSpaceshipY, NULL );

        if( g_bSpaceshipFacesLeft )
            LineTo( hdcBackBuffer, 0, g_iSpaceshipY );
        else
            LineTo( hdcBackBuffer, g_wGameWidth, g_iSpaceshipY );
    }

    // Draw the spaceship
    if( g_bGameOver )
        SelectObject( hdcBitmap, g_hbmPlayerExploding );
    else if( g_bSpaceshipFacesLeft )
        SelectObject( hdcBitmap, g_hbmPlayerLeft );
    else
        SelectObject( hdcBitmap, g_hbmPlayerRight );

    BitBlt( hdcBackBuffer, g_iSpaceshipX-16, g_iSpaceshipY-16, 32, 32,
            hdcBitmap, 0, 0, SRCCOPY );

    // Draw the shield
    if( g_fShieldTime - g_fCurrentTime > 0.0f )
        Arc( hdcBackBuffer, g_iSpaceshipX-16, g_iSpaceshipY-16,
             g_iSpaceshipX+16, g_iSpaceshipY+16, g_iSpaceshipX, g_iSpaceshipY-16,
             g_iSpaceshipX, g_iSpaceshipY-16 );

    // Output the score
    CHAR buffer[80];
    sprintf( buffer, " Score: %08ld", g_dwScore );
    SetTextColor( hdcBackBuffer, RGB(255,255,0) );
    SetBkColor( hdcBackBuffer, RGB(0,0,255) );
    ExtTextOut( hdcBackBuffer, 0, 2, ETO_OPAQUE, NULL, buffer, strlen(buffer), NULL );

    // If game is over, output "Game Over" string
    if( g_bGameOver )
    {
        HFONT hFont = CreateFont( 48, 0, 0, 0, FW_NORMAL, 0, 0, 0,
                                  ANSI_CHARSET, OUT_DEFAULT_PRECIS,
                                  CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY,
                                  VARIABLE_PITCH, "Arial" );
        HFONT hOldFont = (HFONT)SelectObject( hdcBackBuffer, hFont );

        SetTextColor( hdcBackBuffer, RGB(255,255,0) );
        SetBkMode(    hdcBackBuffer, TRANSPARENT );
        SetTextAlign( hdcBackBuffer, TA_CENTER );

        ExtTextOut(   hdcBackBuffer, g_wGameWidth/2, NUM_ROWS*20-25, 0,
                      NULL, "Game Over", 9, NULL );
        
        SelectObject( hdcBackBuffer, hOldFont );
        DeleteObject( hFont );
    }

    // Blt the backbuffer to the window
    HDC hdcWindow = GetDC( hWnd );
    BitBlt( hdcWindow, 0, 0, g_wGameWidth, g_wGameHeight, hdcBackBuffer, 0, 0,
            SRCCOPY );
    ReleaseDC( hWnd, hdcWindow );

    // Cleanup used objects
    SelectObject( hdcBackBuffer, hOldBrush );
    DeleteObject( hBrush );
    SelectObject( hdcBackBuffer, hOldPen );
    DeleteObject( hPen );
    DeleteDC( hdcBackBuffer );
    DeleteDC( hdcBitmap );
}




