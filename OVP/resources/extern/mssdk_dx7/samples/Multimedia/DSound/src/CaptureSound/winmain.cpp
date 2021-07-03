//----------------------------------------------------------------------------
// File: WinMain.cpp
//
// Desc: Main application file for the CaptureSound sample showing how to 
//       use DirectSound to capture sound in a wave file 
//
// Copyright (c) 1999 Microsoft Corp. All rights reserved.
//-----------------------------------------------------------------------------
#define STRICT
#include <windows.h>
#include <commdlg.h>
#include <mmreg.h>
#include <dsound.h>
#include <stdio.h>
#include "resource.h"




//-----------------------------------------------------------------------------
// Function-prototypes
//-----------------------------------------------------------------------------
extern HRESULT InitDirectSound( HWND hDlg );
extern HRESULT FreeDirectSound();

extern HRESULT RecordCapturedData();
extern HRESULT ScanAvailableOutputFormats();
extern HRESULT ScanAvailableInputFormats();
extern VOID    GetWaveFormatFromIndex( INT nIndex, WAVEFORMATEX* pwfx );
extern HRESULT InitNotifications();
extern HRESULT CreateCaptureBuffer( WAVEFORMATEX* pwfxInput );
extern VOID    CreateWaveFile( TCHAR* strFileName );
extern HRESULT StartBuffers();
extern HRESULT StopBuffers();

BOOL CALLBACK FormatsDlgProc( HWND hDlg, UINT msg, WPARAM wParam, LPARAM lParam );
HRESULT OnInitFormatsDialog( HWND hDlg );
HRESULT FillFormatListBox( HWND hListBox, BOOL* aFormatSupported );
HRESULT OnInputFormatBoxSelected( HWND hDlg );
HRESULT OnFormatsOK( HWND hDlg );

BOOL CALLBACK MainDlgProc( HWND hDlg, UINT msg, WPARAM wParam, LPARAM lParam );
HRESULT OnInitMainDialog( HWND hDlg );
HRESULT SetMainDialogText();
VOID    OnSaveSoundFile();
VOID    SetFileUI( TCHAR* strFileName );




//-----------------------------------------------------------------------------
// Defines, constants, and global variables
//-----------------------------------------------------------------------------
extern HANDLE       g_hNotificationEvents[2];
extern BOOL         g_abInputFormatSupported[16];
extern DWORD        g_dwNotifySize;
extern WAVEFORMATEX g_wfxInput;
extern LPDIRECTSOUNDCAPTUREBUFFER g_pDSBCapture;

HWND         g_hDlg         = NULL;
BOOL         g_bRecording;


//-----------------------------------------------------------------------------
// Name: WinMain()
// Desc: Entry point for the application.  Since we use a simple dialog for 
//       user interaction we don't need to pump messages.
//-----------------------------------------------------------------------------
INT APIENTRY WinMain( HINSTANCE hInst, HINSTANCE hPrevInst, LPSTR pCmdLine, 
                      INT nCmdShow )
{
    DWORD dwResult;
    MSG   msg;
    BOOL  bDone;
    
    g_hNotificationEvents[0] = CreateEvent( NULL, FALSE, FALSE, NULL );
    g_hNotificationEvents[1] = CreateEvent( NULL, FALSE, FALSE, NULL );

    // Create the main dialog box, but keep it hidden for now
    g_hDlg = CreateDialog( hInst, MAKEINTRESOURCE(IDD_MAIN), NULL, MainDlgProc );

    // Init DirectSound
    if( FAILED( InitDirectSound( g_hDlg ) ) )
    {
        MessageBox( g_hDlg, "Error initializing DirectSound.  Sample will now exit.", 
                    "DirectSound Sample", MB_OK | MB_ICONERROR );
        EndDialog( g_hDlg, IDABORT );
        return 1;
    }

    // Display the formats dialog box, and show it
    dwResult = DialogBox( hInst, MAKEINTRESOURCE(IDD_FORMATS), NULL, FormatsDlgProc );

    if( dwResult != IDOK )
    {
        // The user canceled, so stop message pump, 
        // and fall through to the cleanup code
        PostQuitMessage( 0 );
    }
    else
    {
        SetMainDialogText();
        g_bRecording = FALSE;
        ShowWindow( g_hDlg, SW_SHOW ); 
    }

    bDone = FALSE;
    while( !bDone ) 
    { 
        dwResult = MsgWaitForMultipleObjects( 2, g_hNotificationEvents, 
                                              FALSE, INFINITE, QS_ALLEVENTS );
        switch( dwResult )
        {
            case WAIT_OBJECT_0 + 0:
                // g_hNotificationEvents[0] is signaled

                // This means that DirectSound just finished playing 
                // a piece of the buffer, so we need to fill the circular 
                // buffer with new sound from the wav file

                if( FAILED( RecordCapturedData() ) )
                {
                    MessageBox( g_hDlg, "Error handling DirectSound notifications. "
                               "Sample will now exit.", "DirectSound Sample", 
                               MB_OK | MB_ICONERROR );
                    bDone = TRUE;
                }
                break;

            case WAIT_OBJECT_0 + 1:
                // g_hNotificationEvents[1] is signaled

                // This means that the stop event was signaled, so 
                // any needed update could be done here
                break;

            case WAIT_OBJECT_0 + 2:
                // Windows messages are available
                while( PeekMessage( &msg, NULL, 0, 0, PM_REMOVE ) ) 
                { 
                    if( !IsDialogMessage( g_hDlg, &msg ) )  
                    {
                        TranslateMessage( &msg ); 
                        DispatchMessage( &msg ); 
                    }

                    if( msg.message == WM_QUIT )
                        bDone = TRUE;
                }
                break;
        }
    }

    // Stop the capture and read any data that was not caught by a notification
    StopBuffers();

    // Clean up everything
    FreeDirectSound();

    CloseHandle( g_hNotificationEvents[0] );
    CloseHandle( g_hNotificationEvents[1] );

    return TRUE;
}




//-----------------------------------------------------------------------------
// Name: FormatsDlgProc()
// Desc: Handles dialog messages for formats dlg box
//-----------------------------------------------------------------------------
BOOL CALLBACK FormatsDlgProc( HWND hDlg, UINT msg, WPARAM wParam, LPARAM lParam )
{
    switch( msg ) 
    {
    case WM_COMMAND:
        switch( LOWORD(wParam) )
        {
            case IDCANCEL:
                EndDialog( hDlg, IDCANCEL );
                break;

            case IDOK:
                if( FAILED( OnFormatsOK( hDlg ) ) )
                {
                    MessageBox( hDlg, "Error scanning DirectSound formats."
                                "Sample will now exit.", "DirectSound Sample", 
                                MB_OK | MB_ICONERROR );
                    EndDialog( hDlg, IDABORT );
                }

                break;

            case IDC_FORMATS_INPUT_LISTBOX:
                OnInputFormatBoxSelected( hDlg );
                break;
            
            default:
                return FALSE; // Didn't handle message
        }   
        break;

    case WM_INITDIALOG:
        if( FAILED( OnInitFormatsDialog( hDlg ) ) )
        {
            MessageBox( hDlg, "Error scanning DirectSound formats."
                        "Sample will now exit.", "DirectSound Sample", 
                        MB_OK | MB_ICONERROR );
            EndDialog( hDlg, IDABORT );
        }
        break;

    default:
        return FALSE; // Didn't handle message
    }

    return TRUE; // Handled message
}




//-----------------------------------------------------------------------------
// Name: OnInitFormatsDialog()
// Desc: Initializes the formats dialog
//-----------------------------------------------------------------------------
HRESULT OnInitFormatsDialog( HWND hDlg )
{
    HRESULT hr;

    if( FAILED( hr = ScanAvailableInputFormats() ) )
        return hr;

    HWND hInputList = GetDlgItem( hDlg, IDC_FORMATS_INPUT_LISTBOX );
    FillFormatListBox( hInputList, g_abInputFormatSupported );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: OnInputFormatBoxSelected()
// Desc: Enables the OK button when there is a selection
//-----------------------------------------------------------------------------
HRESULT OnInputFormatBoxSelected( HWND hDlg )
{
    HWND hInputList  = GetDlgItem( hDlg, IDC_FORMATS_INPUT_LISTBOX );
    HWND hOK         = GetDlgItem( hDlg, IDOK );

    if( SendMessage( hInputList,  LB_GETCURSEL, 0, 0 ) != -1 )        
    {
        EnableWindow( hOK, TRUE );
    }
    else
    {
        EnableWindow( hOK, FALSE );
    }

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: ConvertWaveFormatToString()
// Desc: Converts a wave format to a text string
//-----------------------------------------------------------------------------
VOID ConvertWaveFormatToString( WAVEFORMATEX* pwfx, TCHAR* strFormatName )
{
    sprintf( strFormatName, 
             TEXT("%u Hz, %u-bit %s"), 
             pwfx->nSamplesPerSec, 
             pwfx->wBitsPerSample, 
             ( pwfx->nChannels == 1 ) ? TEXT("Mono") : TEXT("Stereo") );
}




//-----------------------------------------------------------------------------
// Name: FillFormatListBox()
// Desc: Initializes the main dialog
//-----------------------------------------------------------------------------
HRESULT FillFormatListBox( HWND hListBox, BOOL* aFormatSupported )
{
    TCHAR        strFormatName[255];
    WAVEFORMATEX wfx;
    DWORD        dwStringIndex;

    SendMessage( hListBox, LB_RESETCONTENT, 0, 0 );

    for( INT iIndex = 0; iIndex < 16; iIndex++ )
    {
        if( aFormatSupported[ iIndex ] )
        {
            GetWaveFormatFromIndex( iIndex, &wfx );
            ConvertWaveFormatToString( &wfx, strFormatName );

            dwStringIndex = SendMessage( hListBox, LB_ADDSTRING, 0, 
                                         (LPARAM) (LPCTSTR) strFormatName );
            SendMessage( hListBox, LB_SETITEMDATA, dwStringIndex, iIndex );
        }
    }

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: OnFormatsOK()
// Desc: Sets the capture buffer format based on what was selected
//-----------------------------------------------------------------------------
HRESULT OnFormatsOK( HWND hDlg )
{
    HRESULT       hr;
    DWORD         dwInputSelect;
    DWORD         dwInputWavIndex;
    HWND          hInputList;

    ZeroMemory( &g_wfxInput, sizeof(g_wfxInput));
    g_wfxInput.wFormatTag = WAVE_FORMAT_PCM;

    hInputList   = GetDlgItem( hDlg, IDC_FORMATS_INPUT_LISTBOX );

    dwInputSelect = SendMessage( hInputList, LB_GETCURSEL, 0, 0 );
    dwInputWavIndex = SendMessage( hInputList, LB_GETITEMDATA, dwInputSelect, 0 );

    GetWaveFormatFromIndex( dwInputWavIndex, &g_wfxInput );

    if( FAILED( hr = CreateCaptureBuffer( &g_wfxInput ) ) )
        return hr;

    EndDialog( hDlg, IDOK );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: MainDlgProc()
// Desc: Handles dialog messages
//-----------------------------------------------------------------------------
BOOL CALLBACK MainDlgProc( HWND hDlg, UINT msg, WPARAM wParam, LPARAM lParam )
{
    switch( msg ) 
    {
    case WM_COMMAND:
        switch( LOWORD(wParam) )
        {
            case IDCANCEL:
                PostQuitMessage( 0 );
                EndDialog( hDlg, IDCANCEL );
                break; 

            case IDC_SOUNDFILE:
                OnSaveSoundFile();
                break;

            case IDC_RECORD:
                if( g_bRecording )
                {
                    // Stop the capture and read any data that 
                    // was not caught by a notification
                    StopBuffers();
                    EnableWindow( GetDlgItem( g_hDlg, IDC_RECORD ), FALSE );
                }
                else
                {
                    if( FAILED( CreateCaptureBuffer( &g_wfxInput ) ) )
                    {
                        MessageBox( hDlg, "Error creating DirectSoundCapture buffer. "                            
                                    "Sample will now exit.", "DirectSound Sample", 
                                    MB_OK | MB_ICONERROR );
                        PostQuitMessage( 0 );
                        EndDialog( hDlg, IDABORT );
                    }

                    if( FAILED( StartBuffers() ) )
                    {
                        MessageBox( hDlg, "Error starting DirectSound buffer. "                            
                                    "Sample will now exit.", "DirectSound Sample", 
                                    MB_OK | MB_ICONERROR );
                        PostQuitMessage( 0 );
                        EndDialog( hDlg, IDABORT );
                    }
                }

                g_bRecording = !g_bRecording;
                break;

            default:
                return FALSE; // Didn't handle message
        }
        break;

    case WM_INITDIALOG:
        OnInitMainDialog( hDlg );
        break;

    default:
        return FALSE; // Didn't handle message
    }

    return TRUE; // Handled message
}




//-----------------------------------------------------------------------------
// Name: OnInitMainDialog()
// Desc: Initializes the main dialog
//-----------------------------------------------------------------------------
HRESULT OnInitMainDialog( HWND hDlg )
{
    // Store HWND in global
    g_hDlg = hDlg;

    // Load the icon
    HINSTANCE hInst = (HINSTANCE) GetWindowLong( g_hDlg, GWL_HINSTANCE );
    HICON hIcon = LoadIcon( hInst, MAKEINTRESOURCE( IDR_MAINFRAME ) );

    // Set the icon for this dialog.
    SendMessage( g_hDlg, WM_SETICON, ICON_BIG,   (LPARAM) hIcon );  // Set big icon
    SendMessage( g_hDlg, WM_SETICON, ICON_SMALL, (LPARAM) hIcon );  // Set small icon

    EnableWindow( GetDlgItem( g_hDlg, IDC_RECORD ), FALSE);
    SetFileUI( TEXT("No file loaded.") );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: SetMainDialogText()
// Desc: Sets the main dialog static text based on the selected format
//-----------------------------------------------------------------------------
HRESULT SetMainDialogText()
{
    WAVEFORMATEX wfxInput;
    TCHAR        strInputFormat[255];
    HWND         hInputFormatText;

    ZeroMemory( &wfxInput, sizeof(wfxInput));
    g_pDSBCapture->GetFormat( &wfxInput, sizeof(wfxInput), NULL );
    ConvertWaveFormatToString( &wfxInput, strInputFormat );   
    hInputFormatText = GetDlgItem( g_hDlg, IDC_MAIN_INPUTFORMAT_TEXT );
    SetWindowText( hInputFormatText, strInputFormat );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: OnSaveSoundFile()
// Desc: Called when the user requests to save to a sound file
//-----------------------------------------------------------------------------
VOID OnSaveSoundFile() 
{
    static TCHAR strFileName[MAX_PATH] = TEXT("");
    static TCHAR strPath[MAX_PATH] = TEXT("");

    // Setup the OPENFILENAME structure
    OPENFILENAME ofn = { sizeof(OPENFILENAME), g_hDlg, NULL,
                         TEXT("Wave Files\0*.wav\0All Files\0*.*\0\0"), NULL,
                         0, 1, strFileName, MAX_PATH, NULL, 0, strPath,
                         TEXT("Save Sound File"),
                         OFN_OVERWRITEPROMPT | OFN_PATHMUSTEXIST | OFN_HIDEREADONLY, 
                         0, 0, TEXT(".wav"), 0, NULL, NULL };

    // Get the default media path (something like C:\WINDOWS\MEDIA)
    if( '\0' == strPath[0] )
    {
        GetWindowsDirectory( strPath, MAX_PATH );
        if( strcmp( &strPath[strlen(strPath)], TEXT("\\") ) )
            strcat( strPath, TEXT("\\") );
        strcat( strPath, TEXT("MEDIA") );
    }

    if( g_bRecording )
    {
        // Stop the capture and read any data that 
        // was not caught by a notification
        StopBuffers();
        g_bRecording = FALSE;
    }

    // Update the UI controls to show the sound as loading a file
    EnableWindow( GetDlgItem( g_hDlg, IDC_RECORD ), FALSE );
    SetFileUI( TEXT("Saving file...") );

    // Display the SaveFileName dialog. Then, try to load the specified file
    if( TRUE == GetSaveFileName( &ofn ) )
    {
        SetFileUI( TEXT("") );

        CreateWaveFile( strFileName );
        EnableWindow( GetDlgItem( g_hDlg, IDC_RECORD ), TRUE );

        // Remember the path for next time
        strcpy( strPath, strFileName );
        char* strLastSlash = strrchr( strPath, '\\' );
        strLastSlash[0] = '\0';
    }
    else
    {
        SetFileUI( TEXT("Save aborted.") );
    }
}




//-----------------------------------------------------------------------------
// Name: SetStatusUI()
// Desc: Sets the text for the status UI 
//-----------------------------------------------------------------------------
VOID SetFileUI( TCHAR* strFileName )
{
    SetWindowText( GetDlgItem( g_hDlg, IDC_FILENAME ), strFileName );
}



