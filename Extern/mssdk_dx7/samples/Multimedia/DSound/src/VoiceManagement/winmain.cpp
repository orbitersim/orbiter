//----------------------------------------------------------------------------
// File: WinMain.cpp
//
// Desc: Main application file for the VoiceManagement sample. 
//
// Copyright (c) 1999 Microsoft Corp. All rights reserved.
//-----------------------------------------------------------------------------
#define STRICT
#include <windows.h>
#include <commdlg.h>
#include <commctrl.h>
#include <mmreg.h>
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
extern BOOL IsBufferPlaying();
extern HRESULT PlayBuffer( DWORD dwPriority, DWORD dwPlayFlags );
extern VOID StopBuffer( BOOL bResetPosition );
extern BOOL IsSoundPlaying();

BOOL CALLBACK MainDlgProc( HWND hDlg, UINT msg, WPARAM wParam, LPARAM lParam );
VOID OnInitDialog( HWND hDlg );
VOID OnOpenSoundFile( HWND hDlg );
HRESULT OnPlaySound( HWND hDlg );
VOID OnTimer( HWND hDlg );
VOID EnablePlayUI( HWND hDlg, BOOL bShowPlayControl );
VOID EnableManagementFlags( HWND hDlg, BOOL bShowFlags );
VOID UpdateBehaviorText( HWND hDlg );
VOID SetFileUI( HWND hDlg, TCHAR* strFileName );




//-----------------------------------------------------------------------------
// Name: WinMain()
// Desc: Entry point for the application.  Since we use a simple dialog for 
//       user interaction we don't need to pump messages.
//-----------------------------------------------------------------------------
INT APIENTRY WinMain( HINSTANCE hInst, HINSTANCE hPrevInst, LPSTR pCmdLine, 
                      INT nCmdShow )
{
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
                    // The 'play' button was pressed
                    if( FAILED( OnPlaySound( hDlg ) ) )
                    {
                        MessageBox( hDlg, "Error playing DirectSound buffer."
                                    "Sample will now exit.", "DirectSound Sample", 
                                    MB_OK | MB_ICONERROR );
                        EndDialog( hDlg, IDABORT );
                    }
                    break;

                case IDC_STOP:
                    StopBuffer( TRUE ); 
                    EnablePlayUI( hDlg, TRUE );
                    break;

                case IDC_ALLOC_HARDWARE:
                case IDC_ALLOC_EITHER:
                    EnableManagementFlags( hDlg, TRUE );
                    UpdateBehaviorText( hDlg );
                    break;

                case IDC_ALLOC_SOFTWARE:
                    EnableManagementFlags( hDlg, FALSE );
                    UpdateBehaviorText( hDlg );
                    break;

                case IDC_BYTIME:
                    if( IsDlgButtonChecked( hDlg, IDC_BYTIME ) == BST_CHECKED ) 
                        CheckDlgButton( hDlg, IDC_BYDISTANCE, BST_UNCHECKED );
                    UpdateBehaviorText( hDlg );
                    break;

                case IDC_BYDISTANCE:
                    if( IsDlgButtonChecked( hDlg, IDC_BYDISTANCE ) == BST_CHECKED ) 
                        CheckDlgButton( hDlg, IDC_BYTIME, BST_UNCHECKED );
                    UpdateBehaviorText( hDlg );
                    break;
                    
                case IDC_BYPRIORTY:
                    UpdateBehaviorText( hDlg );
                    break;

                default:
                    return FALSE; // Didn't handle message
            }
            break;

        case WM_TIMER:
            OnTimer( hDlg );
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

    // Check the 'hardware' voice allocation button by default. 
    CheckRadioButton( hDlg, IDC_ALLOC_EITHER, IDC_ALLOC_SOFTWARE, IDC_ALLOC_HARDWARE );

    HWND hEditPri = GetDlgItem( hDlg, IDC_EDIT_PRIORITY );
    HWND hSpinPri = GetDlgItem( hDlg, IDC_SPIN_PRIORITY );
    SendMessage( hSpinPri, UDM_SETBUDDY, (WPARAM) hEditPri, 0 );
    SendMessage( hSpinPri, UDM_SETRANGE, 0, MAKELONG (0x7FFF, 0) );
    SendMessage( hSpinPri, UDM_SETPOS, 0, 0 );

    // Set the icon for this dialog.
    PostMessage( hDlg, WM_SETICON, ICON_BIG,   (LPARAM) hIcon );  // Set big icon
    PostMessage( hDlg, WM_SETICON, ICON_SMALL, (LPARAM) hIcon );  // Set small icon

    // Create a timer, so we can check for when the soundbuffer is stopped
    SetTimer( hDlg, 1, 0, NULL );

    // Set the UI controls
    UpdateBehaviorText( hDlg );
    SetFileUI( hDlg, TEXT("No file loaded.") );
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

    StopBuffer( TRUE );

    // Update the UI controls to show the sound as loading a file
    EnableWindow( GetDlgItem( hDlg, IDC_PLAY ), FALSE);
    EnableWindow( GetDlgItem( hDlg, IDC_STOP ), FALSE);
    SetFileUI( hDlg, TEXT("Loading file...") );

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
        SetFileUI( hDlg, TEXT("Load aborted.") );
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
            EnablePlayUI( hDlg, TRUE );
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
    DWORD   dwPriority;
    BOOL    bTranslated;
    DWORD   dwPlayFlags;
    BOOL    bLooped;
    BOOL    bAllocHW;
    BOOL    bAllocSW;
    BOOL    bAllocEither;
    BOOL    bByTime;
    BOOL    bByDistance;
    BOOL    bByPriority;

    bLooped = ( IsDlgButtonChecked( hDlg, IDC_LOOP_CHECK ) == BST_CHECKED );

    // Detrimine where the buffer would like to be allocated 
    bAllocHW     = ( IsDlgButtonChecked( hDlg, IDC_ALLOC_HARDWARE ) == BST_CHECKED );
    bAllocSW     = ( IsDlgButtonChecked( hDlg, IDC_ALLOC_SOFTWARE ) == BST_CHECKED );
    bAllocEither = ( IsDlgButtonChecked( hDlg, IDC_ALLOC_EITHER   ) == BST_CHECKED );

    if( bAllocHW || bAllocEither )
    {
        // Detrimine how the buffer should steal hardware resources (if they are not available)
        bByTime      = ( IsDlgButtonChecked( hDlg, IDC_BYTIME     ) == BST_CHECKED );
        bByDistance  = ( IsDlgButtonChecked( hDlg, IDC_BYDISTANCE ) == BST_CHECKED );
        bByPriority  = ( IsDlgButtonChecked( hDlg, IDC_BYPRIORTY  ) == BST_CHECKED );
    }
    else
    {
        // Buffers running in software are not allowed to have
        // voice management flags since they have no need to 
        // steal hardware resources.
        bByTime      = FALSE;
        bByDistance  = FALSE;
        bByPriority  = FALSE;
    }

    // Get the buffer priority
    dwPriority = GetDlgItemInt( hDlg, IDC_EDIT_PRIORITY, &bTranslated, FALSE );
    if( !bTranslated )
    {
        MessageBox( hDlg, "Please enter a buffer priority between 0 and 32767", 
                    "DirectSound Sample", MB_OK );
        return S_OK;
    }

    // Figure out the voice allocation flag from the dialog,
    // and what the user should expect based on the dialog choice
    if( bAllocSW )
        dwPlayFlags = DSBPLAY_LOCSOFTWARE;

    if( bAllocHW )
        dwPlayFlags = DSBPLAY_LOCHARDWARE;

    if( bAllocEither )
        dwPlayFlags = 0;

    // Figure out what voice management flags should be based on the dlg
    if( bByTime )
    {
        if( bByPriority )
        {
            dwPlayFlags |= DSBPLAY_TERMINATEBY_TIME | 
                           DSBPLAY_TERMINATEBY_PRIORITY;
        }
        else
        {
            dwPlayFlags |= DSBPLAY_TERMINATEBY_TIME;
        }
    }
    else if( bByDistance )
    {
        if( bByPriority )
        {
            dwPlayFlags |= DSBPLAY_TERMINATEBY_DISTANCE | 
                           DSBPLAY_TERMINATEBY_PRIORITY;
        }
        else
        {
            dwPlayFlags |= DSBPLAY_TERMINATEBY_DISTANCE;
        }
    }
    else
    {
        if( bByPriority )
        {
            dwPlayFlags |= DSBPLAY_TERMINATEBY_PRIORITY;
        }
        else
        {
            dwPlayFlags |= 0;
        }
    }


    if( bLooped )
        dwPlayFlags |= DSBPLAY_LOOPING;

    // Play the sound 
    if( FAILED( hr = PlayBuffer( dwPriority, dwPlayFlags ) ) )
    {
        if( bAllocHW )
        {
            if( hr == DSERR_CONTROLUNAVAIL || 
                hr == DSERR_INVALIDCALL )
            {
                MessageBox( hDlg, "The buffer could not be played in hardware", 
                            "DirectPlay Sample", MB_OK | MB_ICONERROR );
                return S_OK;
            }
        }

        return hr;
    }

    // Update the UI controls to show the sound as playing
    EnablePlayUI( hDlg, FALSE );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: UpdateBehaviorText()
// Desc: Figure out what the expected behavoir is based on the dialog,
//       and display it on the dialog
//-----------------------------------------------------------------------------
VOID UpdateBehaviorText( HWND hDlg )
{
    TCHAR   strExcepted[1024];
    BOOL    bAllocHW;
    BOOL    bAllocSW;
    BOOL    bAllocEither;
    BOOL    bByTime;
    BOOL    bByDistance;
    BOOL    bByPriority;

    // Detrimine where the buffer would like to be allocated 
    bAllocHW     = ( IsDlgButtonChecked( hDlg, IDC_ALLOC_HARDWARE ) == BST_CHECKED );
    bAllocSW     = ( IsDlgButtonChecked( hDlg, IDC_ALLOC_SOFTWARE ) == BST_CHECKED );
    bAllocEither = ( IsDlgButtonChecked( hDlg, IDC_ALLOC_EITHER   ) == BST_CHECKED );

    if( bAllocHW || bAllocEither )
    {
        // Detrimine how the buffer should steal hardware resources (if they are not available)
        bByTime      = ( IsDlgButtonChecked( hDlg, IDC_BYTIME     ) == BST_CHECKED );
        bByDistance  = ( IsDlgButtonChecked( hDlg, IDC_BYDISTANCE ) == BST_CHECKED );
        bByPriority  = ( IsDlgButtonChecked( hDlg, IDC_BYPRIORTY  ) == BST_CHECKED );
    }
    else
    {
        // Buffers running in software are not allowed to have
        // voice management flags since they have no need to 
        // steal hardware resources.
        bByTime      = FALSE;
        bByDistance  = FALSE;
        bByPriority  = FALSE;
    }

    // Figure what the user should expect based on the dialog choice
    if( bAllocSW )
    {
        strcpy( strExcepted, "The new sound will be played in software" );
    }

    if( bAllocHW )
    {
        strcpy( strExcepted, "The new sound will be played in hardware" );
    }

    if( bAllocEither )
    {
        strcpy( strExcepted, "The new sound will be played in hardware "
                             "if available" );
    }

    if( bByTime )
    {
        if( bByPriority )
        {
            if( bAllocEither )
            {
                strcpy( strExcepted, "The new sound will be played in hardware, "
                                     "if the the hardware has no available "
                                     "voices, and new sound has a higher priority "
                                     "than sounds currently playing in hardware "
                                     "then sound with the lowest priority will be "
                                     "terminated and the new sound will play in "
                                     "hardware. Otherwise, the new sound will play "
                                     "in software.  In event of a priority tie, "
                                     "then the buffer with the least time left to "
                                     "play will be prematurely terminated." );
            }
            else
            {
                strcat( strExcepted, ", and if the hardware has no available "
                                     "voices, the voice management buffer with "
                                     "the lowest priority as set by the "
                                     "IDirectSoundBuffer::Play priority argument "
                                     "will be prematurely terminated. In event "
                                     "of a priority tie, then the buffer with "
                                     "the least time left to play will be "
                                     "prematurely terminated." );         
            }
        }
        else
        {
            strcat( strExcepted, ", and if the hardware has no available "
                                 "voices, the voice management buffer with "
                                 "the least time left to play will be "
                                 "prematurely terminated." );                                     
        }
    }
    else if( bByDistance )
    {
        if( bByPriority )
        {
            if( bAllocEither )
            {
                strcpy( strExcepted, "The new sound will be played in hardware, "
                                     "if the the hardware has no available "
                                     "voices, and new sound has a higher priority "
                                     "than sounds currently playing in hardware "
                                     "then sound with the lowest priority will be "
                                     "terminated and the new sound will play in "
                                     "hardware. Otherwise, the new sound will play "
                                     "in software.  In event of a priority tie, "
                                     "then the buffer which is the furthest "
                                     "distance from the listener at the time "
                                     "of the Play will be prematurely terminated." );
            }
            else
            {
                strcat( strExcepted, ", and if the hardware has no available "
                                     "voices, the voice management buffer with "
                                     "the lowest priority as set by the "
                                     "IDirectSoundBuffer::Play priority argument "
                                     "will be prematurely terminated. In event "
                                     "of a priority tie, then the buffer which "
                                     "is the furthest distance from the "
                                     "listener at the time of the Play will "
                                     "be prematurely terminated." );
            }
        }
        else
        {
            strcat( strExcepted, ", and if the hardware has no available "
                                 "voices, the voice management buffer which "
                                 "is the furthest distance from the "
                                 "listener at the time of the Play will "
                                 "be prematurely terminated." );

        }
    }
    else
    {
        if( bByPriority )
        {
            if( bAllocEither )
            {
                strcpy( strExcepted, "The new sound will be played in hardware, "
                                     "if the the hardware has no available "
                                     "voices, and new sound has a higher priority "
                                     "than sounds currently playing in hardware "
                                     "then sound with the lowest priority will be "
                                     "terminated and the new sound will play in "
                                     "hardware. Otherwise, the new sound will play "
                                     "in software." );
            }
            else
            {
                strcat( strExcepted, ", and if the hardware has no available "
                                     "voices, the voice management buffer with "
                                     "the lowest priority as set by the "
                                     "IDirectSoundBuffer::Play priority argument "
                                     "will be prematurely terminated. " );
            }
        }
        else
        {
            strcat( strExcepted, ", and the buffer will not steal any "
                                 "hardware resources." );
        }
    }


    // Tell the user what to expect
    SetDlgItemText( hDlg, IDC_BEHAVIOR, strExcepted );
}






//-----------------------------------------------------------------------------
// Name: OnEnablePlayUI()
// Desc: Enables or disables the Play UI controls 
//-----------------------------------------------------------------------------
VOID EnablePlayUI( HWND hDlg, BOOL bShowPlayControl )
{
    EnableWindow( GetDlgItem( hDlg, IDC_LOOP_CHECK ),  bShowPlayControl );
    EnableWindow( GetDlgItem( hDlg, IDC_STOP ),       !bShowPlayControl );
    EnableWindow( GetDlgItem( hDlg, IDC_PLAY ),        bShowPlayControl );

    // Don't allow the voice allocation or voicemanagement flags 
    // to be changed when a sound is playing
    EnableWindow( GetDlgItem( hDlg, IDC_BYTIME         ), bShowPlayControl );
    EnableWindow( GetDlgItem( hDlg, IDC_BYDISTANCE     ), bShowPlayControl );
    EnableWindow( GetDlgItem( hDlg, IDC_BYPRIORTY      ), bShowPlayControl );
    EnableWindow( GetDlgItem( hDlg, IDC_EDIT_PRIORITY  ), bShowPlayControl );
    EnableWindow( GetDlgItem( hDlg, IDC_ALLOC_HARDWARE ), bShowPlayControl );
    EnableWindow( GetDlgItem( hDlg, IDC_ALLOC_SOFTWARE ), bShowPlayControl );
    EnableWindow( GetDlgItem( hDlg, IDC_ALLOC_EITHER   ), bShowPlayControl );

    if( bShowPlayControl )
    {
        // If the software alloc flag is checked, then don't enable
        // the voice management flags
        if( IsDlgButtonChecked( hDlg, IDC_ALLOC_SOFTWARE ) == BST_CHECKED )
            EnableManagementFlags( hDlg, FALSE );
    }

    if( bShowPlayControl )
        SetFocus( GetDlgItem( hDlg, IDC_PLAY ) );
    else
        SetFocus( GetDlgItem( hDlg, IDC_STOP ) );
}




//-----------------------------------------------------------------------------
// Name: EnableManagementFlags()
// Desc: Enable or disable the voice management flags
//-----------------------------------------------------------------------------
VOID EnableManagementFlags( HWND hDlg, BOOL bShowFlags )
{
    EnableWindow( GetDlgItem( hDlg, IDC_BYTIME        ), bShowFlags );
    EnableWindow( GetDlgItem( hDlg, IDC_BYDISTANCE    ), bShowFlags );
    EnableWindow( GetDlgItem( hDlg, IDC_BYPRIORTY     ), bShowFlags );
    EnableWindow( GetDlgItem( hDlg, IDC_EDIT_PRIORITY ), bShowFlags );
}




//-----------------------------------------------------------------------------
// Name: SetStatusUI()
// Desc: Sets the text for the status UI 
//-----------------------------------------------------------------------------
VOID SetFileUI( HWND hDlg, TCHAR* strFileName )
{
    SetWindowText( GetDlgItem( hDlg, IDC_FILENAME ), strFileName );
}


