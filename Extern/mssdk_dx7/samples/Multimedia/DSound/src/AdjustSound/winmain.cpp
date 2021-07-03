//----------------------------------------------------------------------------
// File: WinMain.cpp
//
// Desc: Main application file for the AdjustSound sample. This sample shows how
//       to load a wave file and play it using a static DirectSound buffer 
//       and adjust its focus, frequency, pan, and volume.
//
// Copyright (c) 1999 Microsoft Corp. All rights reserved.
//-----------------------------------------------------------------------------
#define STRICT
#include <windows.h>
#include <mmsystem.h>
#include <mmreg.h>
#include <dsound.h>
#include <commctrl.h>
#include <commdlg.h>
#include <dsound.h>
#include <stdio.h>
#include "resource.h"




//-----------------------------------------------------------------------------
// Function-prototypes
//-----------------------------------------------------------------------------
extern HRESULT InitDirectSound( HWND hDlg );
extern HRESULT FreeDirectSound();
extern VOID SetBufferOptions( LONG lFrequency, LONG lPan, LONG lVolume );
extern VOID LoadWaveFile( HWND hDlg, TCHAR* strFileName );
extern HRESULT PlayBuffer( HWND hDlg, BOOL bLooped, DWORD dwCreationFlags );
extern VOID StopBuffer();
extern BOOL IsSoundPlaying();

BOOL CALLBACK MainDlgProc( HWND hDlg, UINT msg,  WPARAM wParam, LPARAM lParam );
VOID OnInitDialog( HWND hDlg );
VOID OnSliderChanged( HWND hDlg );
VOID OnOpenSoundFile( HWND hDlg );
HRESULT OnPlaySound( HWND hDlg );
VOID OnTimer( HWND hDlg );
VOID OnEnablePlayUI( HWND hDlg, BOOL bEnable );
VOID SetStatusUI( HWND hDlg,TCHAR* strStatus );
VOID SetFileUI( HWND hDlg,TCHAR* strFileName );
VOID SetSlidersPos( HWND hDlg, LONG lFreqSlider, LONG lPanSlider, LONG lVolumeSlider );




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
BOOL CALLBACK MainDlgProc( HWND hDlg, UINT msg,  WPARAM wParam, LPARAM lParam )
{
    switch( msg ) 
    {
        case WM_COMMAND:
            switch( LOWORD(wParam) )
            {
                case IDCANCEL:
                    EndDialog( hDlg, IDCANCEL );
                    break;

                case IDC_SOUNDFILE:
                    OnOpenSoundFile( hDlg );
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
                    StopBuffer(); 
                    break;

                default:
                    return FALSE; // Didn't handle message
            }
            break;

        case WM_TIMER:
            OnTimer( hDlg );
            break;

        case WM_INITDIALOG:
            OnInitDialog( hDlg );
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

    // Create a timer, so we can check for when the soundbuffer is stopped
    SetTimer( hDlg, 1, 0, NULL );

    // Set the UI controls
    SetFileUI( hDlg, TEXT("") );
    SetStatusUI( hDlg, TEXT("No file loaded.") );

    // Get handles to dialog items
    HWND hFreqSlider    = GetDlgItem( hDlg, IDC_FREQUENCY_SLIDER );
    HWND hPanSlider     = GetDlgItem( hDlg, IDC_PAN_SLIDER );
    HWND hVolumeSlider  = GetDlgItem( hDlg, IDC_VOLUME_SLIDER );

    // Set the focus to normal by default
    CheckRadioButton( hDlg, IDC_FOCUS_NORMAL, IDC_FOCUS_NORMAL, IDC_FOCUS_NORMAL );

    // Set the buffer memory to default 
    CheckRadioButton( hDlg, IDC_MEMORY_DEFAULT, IDC_MEMORY_SOFTWARE, IDC_MEMORY_DEFAULT );

    // Set the range and position of the freq slider from 
    // DSBFREQUENCY_MIN and DSBFREQUENCY_MAX are DirectSound constants
    PostMessage( hFreqSlider, TBM_SETRANGEMAX, TRUE, DSBFREQUENCY_MAX );
    PostMessage( hFreqSlider, TBM_SETRANGEMIN, TRUE, DSBFREQUENCY_MIN );

    // Set the range and position of the pan slider from 
    PostMessage( hPanSlider, TBM_SETRANGEMAX, TRUE, ( 10000L/500L) );
    PostMessage( hPanSlider, TBM_SETRANGEMIN, TRUE, (-10000L/500L) );

    // Set the range and position of the volume slider 
    PostMessage( hVolumeSlider, TBM_SETRANGEMAX, TRUE, 0L );
    PostMessage( hVolumeSlider, TBM_SETRANGEMIN, TRUE, (-10000L/100L) );

    // Set the position of the sliders
    SetSlidersPos( hDlg, DSBFREQUENCY_MIN, 0, 0 );
}




//-----------------------------------------------------------------------------
// Name: OnSliderChanged()  
// Desc: Called when the dialog's slider bars are changed by the user, or need
//       updating
//-----------------------------------------------------------------------------
VOID OnSliderChanged( HWND hDlg )
{
    TCHAR strBuffer[10];

    // Get handles to dialog items
    HWND hFreqSlider   = GetDlgItem( hDlg, IDC_FREQUENCY_SLIDER );
    HWND hPanSlider    = GetDlgItem( hDlg, IDC_PAN_SLIDER );
    HWND hVolumeSlider = GetDlgItem( hDlg, IDC_VOLUME_SLIDER );

    // Get the position of the sliders
    LONG lFrequency = SendMessage( hFreqSlider,   TBM_GETPOS, 0, 0 ) * 1L;
    LONG lPan       = SendMessage( hPanSlider,    TBM_GETPOS, 0, 0 ) * 500L;
    LONG lVolume    = SendMessage( hVolumeSlider, TBM_GETPOS, 0, 0 ) * 100L;

    // Set the static text boxes
    sprintf( strBuffer, TEXT("%ld"), lFrequency );
    SetWindowText( GetDlgItem( hDlg, IDC_FREQUENCY ), strBuffer );

    sprintf( strBuffer, TEXT("%ld"), lPan );
    SetWindowText( GetDlgItem( hDlg, IDC_PAN       ), strBuffer );

    sprintf( strBuffer, TEXT("%ld"), lVolume );
    SetWindowText( GetDlgItem( hDlg, IDC_VOLUME    ), strBuffer );

    // Set the options in the DirectSound buffer
    SetBufferOptions( lFrequency, lPan, lVolume );
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
    EnableWindow( GetDlgItem( hDlg, IDC_PLAY ),         FALSE );
    EnableWindow( GetDlgItem( hDlg, IDC_STOP ),         FALSE );
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
    }
    else
    {
        SetStatusUI( hDlg, TEXT("Load aborted.") );
    }
}




//-----------------------------------------------------------------------------
// Name: OnTimer()
// Desc: When we think the sound is playing this periodically checks to see if 
//       the sound has stopped.  If it has then updates the dialog.
//-----------------------------------------------------------------------------
VOID OnTimer( HWND hDlg ) 
{
    if( IsWindowEnabled( GetDlgItem( hDlg, IDC_STOP ) ) )
    {
        // We think the sound is playing, so see if it has stopped yet.
        if( !IsSoundPlaying() ) 
        {
            // Update the UI controls to show the sound as stopped
            OnEnablePlayUI( hDlg, TRUE );
            SetStatusUI( hDlg, TEXT("Sound stopped.") );
        }
    }
}




//-----------------------------------------------------------------------------
// Name: OnPlaySound()
// Desc: User hit the "Play" button
//-----------------------------------------------------------------------------
HRESULT OnPlaySound( HWND hDlg ) 
{
    HRESULT hr;
    DWORD dwCreationFlags;

    BOOL bLooped      = ( IsDlgButtonChecked( hDlg, IDC_LOOP_CHECK )      == BST_CHECKED );
    BOOL bFocusSticky = ( IsDlgButtonChecked( hDlg, IDC_FOCUS_STICKY )    == BST_CHECKED );
    BOOL bFocusGlobal = ( IsDlgButtonChecked( hDlg, IDC_FOCUS_GLOBAL )    == BST_CHECKED );
    BOOL bMemHardware = ( IsDlgButtonChecked( hDlg, IDC_MEMORY_HARDWARE ) == BST_CHECKED );
    BOOL bMemSoftware = ( IsDlgButtonChecked( hDlg, IDC_MEMORY_SOFTWARE ) == BST_CHECKED );

    // Detrimine the creation flags to use based on the radio buttons
    dwCreationFlags = 0;
    if( bFocusGlobal )
        dwCreationFlags |= DSBCAPS_GLOBALFOCUS;

    if( bFocusSticky )
        dwCreationFlags |= DSBCAPS_STICKYFOCUS;

    if( bMemHardware )
        dwCreationFlags |= DSBCAPS_LOCHARDWARE;

    if( bMemSoftware )
        dwCreationFlags |= DSBCAPS_LOCSOFTWARE;

    if( FAILED( hr = PlayBuffer( hDlg, bLooped, dwCreationFlags ) ) )
        return hr;

    // Only if the sound buffer was created perfectly should we update the UI
    if( hr == S_OK )
    {
        // Update the UI controls to show the sound as playing
        OnEnablePlayUI( hDlg, FALSE );
        SetStatusUI( hDlg, TEXT("Sound playing.") );
    }

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: OnEnablePlayUI()
// Desc: Enables or disables the Play UI controls 
//-----------------------------------------------------------------------------
VOID OnEnablePlayUI( HWND hDlg, BOOL bEnable )
{
    if( bEnable )
    {
        EnableWindow( GetDlgItem( hDlg, IDC_LOOP_CHECK      ), TRUE );
        EnableWindow( GetDlgItem( hDlg, IDC_PLAY            ), TRUE );
        EnableWindow( GetDlgItem( hDlg, IDC_STOP            ), FALSE );

        EnableWindow( GetDlgItem( hDlg, IDC_FOCUS_NORMAL    ), TRUE );
        EnableWindow( GetDlgItem( hDlg, IDC_FOCUS_STICKY    ), TRUE );
        EnableWindow( GetDlgItem( hDlg, IDC_FOCUS_GLOBAL    ), TRUE );

        EnableWindow( GetDlgItem( hDlg, IDC_MEMORY_DEFAULT  ), TRUE );
        EnableWindow( GetDlgItem( hDlg, IDC_MEMORY_HARDWARE ), TRUE );
        EnableWindow( GetDlgItem( hDlg, IDC_MEMORY_SOFTWARE ), TRUE );

        SetFocus(     GetDlgItem( hDlg, IDC_PLAY ) );
    }
    else
    {
        EnableWindow( GetDlgItem( hDlg, IDC_LOOP_CHECK      ), FALSE );
        EnableWindow( GetDlgItem( hDlg, IDC_PLAY            ), FALSE );
        EnableWindow( GetDlgItem( hDlg, IDC_STOP            ), TRUE );

        EnableWindow( GetDlgItem( hDlg, IDC_FOCUS_NORMAL    ), FALSE );
        EnableWindow( GetDlgItem( hDlg, IDC_FOCUS_STICKY    ), FALSE );
        EnableWindow( GetDlgItem( hDlg, IDC_FOCUS_GLOBAL    ), FALSE );

        EnableWindow( GetDlgItem( hDlg, IDC_MEMORY_DEFAULT  ), FALSE );
        EnableWindow( GetDlgItem( hDlg, IDC_MEMORY_HARDWARE ), FALSE );
        EnableWindow( GetDlgItem( hDlg, IDC_MEMORY_SOFTWARE ), FALSE );

        SetFocus(     GetDlgItem( hDlg, IDC_STOP ) );
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
VOID SetSlidersPos( HWND hDlg, LONG lFreqSlider, LONG lPanSlider, LONG lVolumeSlider )
{
    HWND hFreqSlider    = GetDlgItem( hDlg, IDC_FREQUENCY_SLIDER );
    HWND hPanSlider     = GetDlgItem( hDlg, IDC_PAN_SLIDER );
    HWND hVolumeSlider  = GetDlgItem( hDlg, IDC_VOLUME_SLIDER );

    PostMessage( hFreqSlider,   TBM_SETPOS, TRUE, lFreqSlider );
    PostMessage( hPanSlider,    TBM_SETPOS, TRUE, lPanSlider );
    PostMessage( hVolumeSlider, TBM_SETPOS, TRUE, lVolumeSlider );
}
