//----------------------------------------------------------------------------
// File: WinMain.cpp
//
// Desc: Main application file for the Play3DSound sample. This sample shows how
//       to load a wave file and play it using a 3D DirectSound buffer.
//
// Copyright (c) 1999 Microsoft Corp. All rights reserved.
//-----------------------------------------------------------------------------
#define STRICT
#include <windows.h>
#include <commctrl.h>
#include <commdlg.h>
#include <mmreg.h>
#include <mmsystem.h>
#include <dsound.h>
#include <math.h>
#include <stdio.h>
#include "resource.h"




//-----------------------------------------------------------------------------
// Function-prototypes
//-----------------------------------------------------------------------------
extern HRESULT InitDirectSound( HWND hDlg );
extern HRESULT FreeDirectSound();
extern VOID SetParameters( FLOAT fDopplerFactor, FLOAT fRolloffFactor, 
                           FLOAT fMinDistance,   FLOAT fMaxDistance );
extern VOID SetObjectProperties( D3DVECTOR* pvPosition, D3DVECTOR* pvVelocity );
extern VOID LoadWaveFile( HWND hDlg, TCHAR* strFileName );
extern HRESULT PlayBuffer( BOOL bLooped );
extern VOID StopBuffer();

BOOL CALLBACK MainDlgProc( HWND hDlg, UINT msg, WPARAM wParam, LPARAM lParam );
VOID OnInitDialog( HWND hDlg );
VOID OnSliderChanged( HWND hDlg );
VOID OnOpenSoundFile( HWND hDlg );
HRESULT OnPlaySound( HWND hDlg );
VOID OnStopSound( HWND hDlg );
VOID OnMovementTimer( HWND hDlg );
VOID OnCommitTimer( HWND hDlg );
VOID OnEnablePlayUI( HWND hDlg, BOOL bEnable );
VOID SetStatusUI( HWND hDlg, TCHAR* strStatus );
VOID SetFileUI( HWND hDlg, TCHAR* strFileName );
VOID SetSlidersPos( HWND hDlg, LONG lDopplerSlider, LONG lRolloffSlider,
                    LONG lMinDistSlider, LONG lMaxDistSlider );




//-----------------------------------------------------------------------------
// Defines, constants, and global variables
//-----------------------------------------------------------------------------
#define ORBIT_MAX_RADIUS        5.5f
#define IDT_MOVEMENT_TIMER      1
#define IDT_DEFER_TIMER         2

extern LPDIRECTSOUND3DLISTENER g_pDSListener;

BOOL g_bDeferSettings = FALSE;




//-----------------------------------------------------------------------------
// Name: WinMain()
// Desc: Entry point for the application.  Since we use a simple dialog for 
//       user interaction we don't need to pump messages.
//-----------------------------------------------------------------------------
INT APIENTRY WinMain( HINSTANCE hInst, HINSTANCE hPrevInst, LPSTR pCmdLine, 
                      INT nCmdShow )
{
    // Init the common control dll 
    InitCommonControls();

    // Display the main dialog box.
    DialogBox( hInst, MAKEINTRESOURCE(IDD_MAIN), NULL, MainDlgProc );

    return TRUE;
}




//-----------------------------------------------------------------------------
// Name: MainDlgProc()
// Desc: Handles dialog messages
//-----------------------------------------------------------------------------
BOOL CALLBACK MainDlgProc( HWND hDlg, UINT msg, WPARAM wParam, LPARAM lParam )
{
    switch( msg ) 
    {
        case WM_INITDIALOG:
            OnInitDialog( hDlg );
            break;

        case WM_COMMAND:
            switch( LOWORD(wParam) )
            {
                case IDC_SOUNDFILE:
                    OnOpenSoundFile( hDlg );
                    break;

                case IDCANCEL:
                    EndDialog( hDlg, IDCANCEL );
                    break;

                case IDC_PLAY:
                    if( FAILED( OnPlaySound( hDlg ) ) )
                    {
                        MessageBox( hDlg, "Error playing DirectSound buffer."
                                    "Sample will now exit.", "DirectSound Sample", 
                                    MB_OK | MB_ICONERROR );
                        EndDialog( hDlg, IDABORT );
                    }
                    break;

                case IDC_STOP:
                    OnStopSound( hDlg );
                    break;

                case IDC_DEFER:
                    g_bDeferSettings = !g_bDeferSettings;
                    OnSliderChanged( hDlg );                    
                    break;

                default:
                    return FALSE; // Didn't handle message
            }
            break;

        case WM_TIMER:
            if( wParam == IDT_MOVEMENT_TIMER )
                OnMovementTimer( hDlg );
            else
                OnCommitTimer( hDlg );
            break;

        case WM_NOTIFY:
            OnSliderChanged( hDlg );
            break;

        case WM_DESTROY:
            // Cleanup everything
            KillTimer( hDlg, 1 );    
            FreeDirectSound();
            break; 

        default:
            return FALSE; // Didn't handle message
    }

    return TRUE; // Handled message
}




//-----------------------------------------------------------------------------
// Name: OnInitDialog()
// Desc: Initializes the dialogs (sets up UI controls, etc.)
//-----------------------------------------------------------------------------
VOID OnInitDialog( HWND hDlg )
{
    // Load the icon
    HINSTANCE hInst = (HINSTANCE) GetWindowLong( hDlg, GWL_HINSTANCE );
    HICON hIcon = LoadIcon( hInst, MAKEINTRESOURCE( IDR_MAINFRAME ) );

    // Init DirectSound
    if( FAILED( InitDirectSound( hDlg ) ) )
    {
        MessageBox( hDlg, "Error initializing DirectSound.  Sample will now exit.", 
                            "DirectSound Sample", MB_OK | MB_ICONERROR );
        EndDialog( hDlg, IDABORT );
        return;
    }

    // Set the icon for this dialog.
    PostMessage( hDlg, WM_SETICON, ICON_BIG,   (LPARAM) hIcon );  // Set big icon
    PostMessage( hDlg, WM_SETICON, ICON_SMALL, (LPARAM) hIcon );  // Set small icon

    // Create a timer to periodically move the 3D object around
    SetTimer( hDlg, IDT_MOVEMENT_TIMER, 0, NULL );

    // Set the UI controls
    SetFileUI( hDlg, TEXT("") );
    SetStatusUI( hDlg, TEXT("No file loaded.") );

    // Get handles to dialog items
    HWND hDopplerSlider  = GetDlgItem( hDlg, IDC_DOPPLER_SLIDER );
    HWND hRolloffSlider  = GetDlgItem( hDlg, IDC_ROLLOFF_SLIDER );
    HWND hMinDistSlider  = GetDlgItem( hDlg, IDC_MINDISTANCE_SLIDER );
    HWND hMaxDistSlider  = GetDlgItem( hDlg, IDC_MAXDISTANCE_SLIDER );
    HWND hVertSlider     = GetDlgItem( hDlg, IDC_VERTICAL_SLIDER );
    HWND hHorzSlider     = GetDlgItem( hDlg, IDC_HORIZONTAL_SLIDER );
    HWND hDeferSlider    = GetDlgItem( hDlg, IDC_DEFER_SLIDER );

    // Set the range and position of the sliders
    LONG lMaxDistOnSlider = (LONG)(ORBIT_MAX_RADIUS*200);

    PostMessage( hDopplerSlider, TBM_SETRANGEMAX, TRUE, 100L );
    PostMessage( hDopplerSlider, TBM_SETRANGEMIN, TRUE, 0L );

    PostMessage( hRolloffSlider, TBM_SETRANGEMAX, TRUE, 100L );
    PostMessage( hRolloffSlider, TBM_SETRANGEMIN, TRUE, 0L );

    PostMessage( hMinDistSlider, TBM_SETRANGEMAX, TRUE, lMaxDistOnSlider );
    PostMessage( hMinDistSlider, TBM_SETRANGEMIN, TRUE, 10L );

    PostMessage( hMaxDistSlider, TBM_SETRANGEMAX, TRUE, lMaxDistOnSlider );
    PostMessage( hMaxDistSlider, TBM_SETRANGEMIN, TRUE, 10L );

    PostMessage( hVertSlider,    TBM_SETRANGEMAX, TRUE, 100L );
    PostMessage( hVertSlider,    TBM_SETRANGEMIN, TRUE, -100L );

    PostMessage( hHorzSlider,    TBM_SETRANGEMAX, TRUE, 100L );
    PostMessage( hHorzSlider,    TBM_SETRANGEMIN, TRUE, -100L );

    PostMessage( hDeferSlider,   TBM_SETRANGEMAX, TRUE, 100L );
    PostMessage( hDeferSlider,   TBM_SETRANGEMIN, TRUE, 1L );

    // Set the position of the sliders
    SetSlidersPos( hDlg, 10, 10, 10, lMaxDistOnSlider );
    PostMessage( hDeferSlider, TBM_SETPOS, TRUE, 20 );
}




//-----------------------------------------------------------------------------
// Name: OnSliderChanged()  
// Desc: Called when the dialog's slider bars are changed by the user, or need
//       updating
//-----------------------------------------------------------------------------
VOID OnSliderChanged( HWND hDlg )
{
    TCHAR strBuffer[10];
    FLOAT fDopplerFactor;
    FLOAT fRolloffFactor;
    FLOAT fMinDistance; 
    FLOAT fMaxDistance;
    DWORD dwDeferTime;

    // Get handles to dialog items
    HWND hDopplerSlider  = GetDlgItem( hDlg, IDC_DOPPLER_SLIDER );
    HWND hRolloffSlider  = GetDlgItem( hDlg, IDC_ROLLOFF_SLIDER );
    HWND hMinDistSlider  = GetDlgItem( hDlg, IDC_MINDISTANCE_SLIDER );
    HWND hMaxDistSlider  = GetDlgItem( hDlg, IDC_MAXDISTANCE_SLIDER );
    HWND hDeferSlider    = GetDlgItem( hDlg, IDC_DEFER_SLIDER );

    // Get the position of the sliders
    fDopplerFactor = SendMessage( hDopplerSlider, TBM_GETPOS, 0, 0 ) * 0.1f;
    fRolloffFactor = SendMessage( hRolloffSlider, TBM_GETPOS, 0, 0 ) * 0.1f;
    fMinDistance   = SendMessage( hMinDistSlider, TBM_GETPOS, 0, 0 ) * 0.01f;
    fMaxDistance   = SendMessage( hMaxDistSlider, TBM_GETPOS, 0, 0 ) * 0.01f;
    dwDeferTime    = SendMessage( hDeferSlider, TBM_GETPOS, 0, 0 );

    // Set the static text boxes
    sprintf( strBuffer, TEXT("%.1f"), fDopplerFactor );
    SetWindowText( GetDlgItem( hDlg, IDC_DOPPLERFACTOR ), strBuffer );

    sprintf( strBuffer, TEXT("%.1f"), fRolloffFactor );
    SetWindowText( GetDlgItem( hDlg, IDC_ROLLOFFFACTOR ), strBuffer );

    sprintf( strBuffer, TEXT("%.1f"), fMinDistance );
    SetWindowText( GetDlgItem( hDlg, IDC_MINDISTANCE ), strBuffer );

    sprintf( strBuffer, TEXT("%.1f"), fMaxDistance );
    SetWindowText( GetDlgItem( hDlg, IDC_MAXDISTANCE ), strBuffer );

    sprintf( strBuffer, TEXT("%dHz"), dwDeferTime );
    SetWindowText( GetDlgItem( hDlg, IDC_DEFER_TEXT ), strBuffer );

    // Set the options in the DirectSound buffer
    SetParameters( fDopplerFactor, fRolloffFactor, fMinDistance, fMaxDistance );

    if( g_bDeferSettings )
    {
        // If we are deferring the settings, then set a timer to go off at 
        // a user set Hz rate  to issue a CommitDefferedSettings() call
        SetTimer( hDlg, IDT_DEFER_TIMER, 
                  1000 / dwDeferTime,  NULL );
    }
}




//-----------------------------------------------------------------------------
// Name: OnOpenSoundFile()
// Desc: Called when the user requests to open a sound file
//-----------------------------------------------------------------------------
VOID OnOpenSoundFile( HWND hDlg ) 
{
    static TCHAR strFileName[MAX_PATH] = TEXT("");
    static TCHAR strPath[MAX_PATH] = TEXT("");

    // Setup the OPENFILENAME structure
    OPENFILENAME ofn = { sizeof(OPENFILENAME), hDlg, NULL,
                         TEXT("Wave Files\0*.wav\0All Files\0*.*\0\0"), NULL,
                         0, 1, strFileName, MAX_PATH, NULL, 0, strPath,
                         TEXT("Open Sound File"),
                         OFN_FILEMUSTEXIST|OFN_HIDEREADONLY, 0, 0,
                         TEXT(".wav"), 0, NULL, NULL };

    // Get the default media path (something like C:\WINDOWS\MEDIA)
    if( '\0' == strPath[0] )
    {
        GetWindowsDirectory( strPath, MAX_PATH );
        if( strcmp( &strPath[strlen(strPath)], TEXT("\\") ) )
            strcat( strPath, TEXT("\\") );
        strcat( strPath, TEXT("MEDIA") );
    }

    StopBuffer();

    // Update the UI controls to show the sound as loading a file
    EnableWindow(  GetDlgItem( hDlg, IDC_PLAY ), FALSE);
    EnableWindow(  GetDlgItem( hDlg, IDC_STOP ), FALSE);
    SetStatusUI( hDlg, TEXT("Loading file...") );

    // Display the OpenFileName dialog. Then, try to load the specified file
    if( TRUE == GetOpenFileName( &ofn ) )
    {
        SetFileUI( hDlg, TEXT("") );
        LoadWaveFile( hDlg, strFileName );

        // Remember the path for next time
        strcpy( strPath, strFileName );
        char* strLastSlash = strrchr( strPath, '\\' );
        strLastSlash[0] = '\0';

        // Set the slider positions
        SetSlidersPos( hDlg, 10L, 10L, 10L, (LONG)(ORBIT_MAX_RADIUS*200) );
    }
    else
    {
        SetStatusUI( hDlg, TEXT("Load aborted.") );
    }
}




//-----------------------------------------------------------------------------
// Name: OnPlaySound()
// Desc: User hit the "Play" button
//-----------------------------------------------------------------------------
HRESULT OnPlaySound( HWND hDlg ) 
{
    HRESULT hr;

    // Play the buffer looped
    if( FAILED( hr = PlayBuffer( TRUE ) ) )
        return hr;

    // Update the UI controls to show the sound as playing
    OnEnablePlayUI( hDlg, FALSE );
    SetStatusUI( hDlg, TEXT("Sound playing.") );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: OnStopSound()
// Desc: User hit the "Stop" button
//-----------------------------------------------------------------------------
VOID OnStopSound( HWND hDlg ) 
{
    StopBuffer();

    // Update the UI controls to show the sound as stopped
    OnEnablePlayUI( hDlg, TRUE );
    SetStatusUI( hDlg, TEXT("Sound stopped.") );
}




//-----------------------------------------------------------------------------
// Name: OnEnablePlayUI()
// Desc: Enables or disables the Play UI controls 
//-----------------------------------------------------------------------------
VOID OnEnablePlayUI( HWND hDlg, BOOL bEnable )
{
    if( bEnable )
    {
        EnableWindow(   GetDlgItem( hDlg, IDC_PLAY ),       TRUE );
        EnableWindow(   GetDlgItem( hDlg, IDC_STOP ),       FALSE );
        SetFocus(       GetDlgItem( hDlg, IDC_PLAY ) );
    }
    else
    {
        EnableWindow(  GetDlgItem( hDlg, IDC_PLAY ),       FALSE );
        EnableWindow(  GetDlgItem( hDlg, IDC_STOP ),       TRUE );
        SetFocus(      GetDlgItem( hDlg, IDC_STOP ) );
    }
}




//-----------------------------------------------------------------------------
// Name: SetStatusUI()
// Desc: Sets the text for the status UI 
//-----------------------------------------------------------------------------
VOID SetStatusUI( HWND hDlg, TCHAR* strStatus ) 
{
    SetWindowText( GetDlgItem( hDlg, IDC_STATUS ), strStatus );
}




//-----------------------------------------------------------------------------
// Name: SetStatusUI()
// Desc: Sets the text for the status UI 
//-----------------------------------------------------------------------------
VOID SetFileUI( HWND hDlg, TCHAR* strFileName )
{
    SetWindowText( GetDlgItem( hDlg, IDC_FILENAME ), strFileName );
}




//-----------------------------------------------------------------------------
// Name: SetSlidersPos()
// Desc: Sets the slider positions
//-----------------------------------------------------------------------------
VOID SetSlidersPos( HWND hDlg, LONG lDopplerSlider, LONG lRolloffSlider,
                    LONG lMinDistSlider, LONG lMaxDistSlider )
{
    HWND hDopplerSlider  = GetDlgItem( hDlg, IDC_DOPPLER_SLIDER );
    HWND hRolloffSlider  = GetDlgItem( hDlg, IDC_ROLLOFF_SLIDER );
    HWND hMinDistSlider  = GetDlgItem( hDlg, IDC_MINDISTANCE_SLIDER );
    HWND hMaxDistSlider  = GetDlgItem( hDlg, IDC_MAXDISTANCE_SLIDER );

    PostMessage( hDopplerSlider, TBM_SETPOS, TRUE, lDopplerSlider );
    PostMessage( hRolloffSlider, TBM_SETPOS, TRUE, lRolloffSlider );
    PostMessage( hMinDistSlider, TBM_SETPOS, TRUE, lMinDistSlider );
    PostMessage( hMaxDistSlider, TBM_SETPOS, TRUE, lMaxDistSlider );
}




//-----------------------------------------------------------------------------
// Name: UpdateGrid()
// Desc: Draws a red dot in the dialog's grid bitmap at the x,y coordinate.
//-----------------------------------------------------------------------------
VOID UpdateGrid( HWND hDlg, FLOAT x, FLOAT y )
{
    static LONG s_lPixel[5] = { 0,0,0,0,0 };
    static LONG s_lX = 0;
    static LONG s_lY = 0;

    HWND hWndGrid = GetDlgItem( hDlg, IDC_RENDER_WINDOW );
    HDC  hDC  = GetDC( hWndGrid );
    RECT rc;
    
    // Replace pixels from that were overdrawn last time
    SetPixel( hDC, s_lX-1, s_lY+0, s_lPixel[0] );
    SetPixel( hDC, s_lX+0, s_lY-1, s_lPixel[1] );
    SetPixel( hDC, s_lX+0, s_lY+0, s_lPixel[2] );
    SetPixel( hDC, s_lX+0, s_lY+1, s_lPixel[3] );
    SetPixel( hDC, s_lX+1, s_lY+0, s_lPixel[4] );

    // Convert the world space x,y coordinates to pixel coordinates
    GetClientRect( hWndGrid, &rc );
    s_lX = (LONG)( ( x/ORBIT_MAX_RADIUS + 1 ) * ( rc.left + rc.right ) / 2 );
    s_lY = (LONG)( (-y/ORBIT_MAX_RADIUS + 1 ) * ( rc.top + rc.bottom ) / 2 );

    // Save the pixels before drawing the cross hair
    s_lPixel[0] = GetPixel( hDC, s_lX-1, s_lY+0 );
    s_lPixel[1] = GetPixel( hDC, s_lX+0, s_lY-1 );
    s_lPixel[2] = GetPixel( hDC, s_lX+0, s_lY+0 );
    s_lPixel[3] = GetPixel( hDC, s_lX+0, s_lY+1 );
    s_lPixel[4] = GetPixel( hDC, s_lX+1, s_lY+0 );

    // Draw a crosshair object in red pixels
    SetPixel( hDC, s_lX-1, s_lY+0, 0x000000ff );
    SetPixel( hDC, s_lX+0, s_lY-1, 0x000000ff );
    SetPixel( hDC, s_lX+0, s_lY+0, 0x000000ff );
    SetPixel( hDC, s_lX+0, s_lY+1, 0x000000ff );
    SetPixel( hDC, s_lX+1, s_lY+0, 0x000000ff );

    ReleaseDC( hWndGrid, hDC );
}




//-----------------------------------------------------------------------------
// Name: OnMovementTimer()
// Desc: Periodically updates the position of the object 
//-----------------------------------------------------------------------------
VOID OnMovementTimer( HWND hDlg ) 
{
    FLOAT fXScale;
    FLOAT fYScale;

    HWND hHorzSlider = GetDlgItem( hDlg, IDC_HORIZONTAL_SLIDER );
    HWND hVertSlider = GetDlgItem( hDlg, IDC_VERTICAL_SLIDER );

    fXScale = SendMessage( hHorzSlider, TBM_GETPOS, 0, 0 ) / 100.0f;
    fYScale = SendMessage( hVertSlider, TBM_GETPOS, 0, 0 ) / 100.0f;
    FLOAT t = timeGetTime()/1000.0f;

    // Move the sound object around the listener. The maximum radius of the
    // orbit is 27.5 units.
    D3DVECTOR vPosition;
    vPosition.x = ORBIT_MAX_RADIUS * fXScale * (FLOAT)sin(t);
    vPosition.y = 0.0f;
    vPosition.z = ORBIT_MAX_RADIUS * fYScale * (FLOAT)cos(t);

    D3DVECTOR vVelocity;
    vVelocity.x = ORBIT_MAX_RADIUS * fXScale * (FLOAT)sin(t+0.05f);
    vVelocity.y = 0.0f;
    vVelocity.z = ORBIT_MAX_RADIUS * fYScale * (FLOAT)cos(t+0.05f);

    // Show the object's position on the dialog's grid control
    UpdateGrid( hDlg, vPosition.x, vPosition.z );

    // Set the sound buffer velocity and position
    SetObjectProperties( &vPosition, &vVelocity );
}



//-----------------------------------------------------------------------------
// Name: OnCommitTimer()
// Desc: Periodically commits the settings for the 3D listener
//-----------------------------------------------------------------------------
VOID OnCommitTimer( HWND hDlg ) 
{
    if( g_bDeferSettings )
    {
        // Call the IDirectSound3DListener::CommitDeferredSettings 
        // method to execute all of the deferred commands at once.
        if ( g_pDSListener )
        {
            g_pDSListener->CommitDeferredSettings();
        }
    }
    else
    {
        // We are not defering the update, so stop this timer.
        KillTimer( hDlg, IDT_DEFER_TIMER );
    }
}
