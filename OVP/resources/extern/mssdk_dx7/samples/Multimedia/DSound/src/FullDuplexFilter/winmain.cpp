//----------------------------------------------------------------------------
// File: WinMain.cpp
//
// Desc: Main application file for the FullDuplexFilter sample showing how to 
//       use DirectSound to implement full duplex audio and a filter.
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
extern HRESULT HandleNotification();
extern HRESULT ScanAvailableOutputFormats();
extern HRESULT ScanAvailableInputFormats();
extern VOID    GetWaveFormatFromIndex( INT nIndex, WAVEFORMATEX* pwfx );
extern HRESULT CreateOutputBuffer();
extern HRESULT SetBufferFormats( WAVEFORMATEX* pwfxInput, WAVEFORMATEX* pwfxOutput );
extern HRESULT StartBuffers();
extern HRESULT StopBuffers();

BOOL CALLBACK FormatsDlgProc( HWND hDlg, UINT msg, WPARAM wParam, LPARAM lParam );
HRESULT OnInitFormatsDialog( HWND hDlg );
HRESULT FillFormatListBox( HWND hListBox, BOOL* aFormatSupported );
HRESULT OnOutputFormatBoxSelected( HWND hDlg );
HRESULT OnInputFormatBoxSelected( HWND hDlg );
HRESULT OnFormatsOK( HWND hDlg );

BOOL CALLBACK MainDlgProc( HWND hDlg, UINT msg, WPARAM wParam, LPARAM lParam );
HRESULT OnInitMainDialog( HWND hDlg );
HRESULT SetMainDialogText( HWND hDlg );
VOID    ConvertWaveFormatToString( WAVEFORMATEX* pwfx, TCHAR* strFormatName );




//-----------------------------------------------------------------------------
// Defines, constants, and global variables
//-----------------------------------------------------------------------------
extern HANDLE g_hNotificationEvents[2];
extern BOOL g_abOutputFormatSupported[16];
extern BOOL g_abInputFormatSupported[16];
extern BOOL g_bUseFilter;

extern LPDIRECTSOUNDBUFFER g_pDSBPrimary;
extern LPDIRECTSOUNDBUFFER  g_pDSBOutput;
extern LPDIRECTSOUNDCAPTUREBUFFER g_pDSBCapture;

BOOL g_bRecording;




//-----------------------------------------------------------------------------
// Name: WinMain()
// Desc: Entry point for the application.  Since we use a simple dialog for 
//       user interaction we don't need to pump messages.
//-----------------------------------------------------------------------------
INT APIENTRY WinMain( HINSTANCE hInst, HINSTANCE hPrevInst, LPSTR pCmdLine, 
                      INT nCmdShow )
{
    DWORD   dwResult;
    MSG     msg;
    BOOL    bDone;
    HRESULT hr;
    HWND    hDlg;
    
    g_hNotificationEvents[0] = CreateEvent( NULL, FALSE, FALSE, NULL );
    g_hNotificationEvents[1] = CreateEvent( NULL, FALSE, FALSE, NULL );

    // Display the main dialog box.
    hDlg = CreateDialog( hInst, MAKEINTRESOURCE(IDD_MAIN), NULL, MainDlgProc );

    // Init DirectSound
    if( FAILED( hr = InitDirectSound( hDlg ) ) )
    {
        if( hr == DSERR_ALLOCATED )
        {
            MessageBox( hDlg, "Full duplex audio failed. "
                        "Make sure the sound card supports this operation. "
                        "The sample will now exit.", "DirectSound Sample", 
                        MB_OK | MB_ICONERROR );
            EndDialog( hDlg, IDABORT );
        }
        else
        {
            MessageBox( hDlg, "Error initializing DirectSound. "
                        "The sample will now exit.", "DirectSound Sample", 
                        MB_OK | MB_ICONERROR );
            EndDialog( hDlg, IDABORT );
        }
        return 1;
    }

    // Display the main dialog box.
    dwResult = DialogBox( hInst, MAKEINTRESOURCE(IDD_FORMATS), NULL, FormatsDlgProc );

    if( dwResult != IDOK )
    {
        // The user canceled, so stop message pump, 
        // and fall through to the cleanup code
        PostQuitMessage( 0 );
    }
    else
    {
        if( FAILED( CreateOutputBuffer() ) )
        {
            MessageBox( hDlg, "Error creating output buffer. "
                       "The sample will now exit.", "DirectSound Sample", 
                       MB_OK | MB_ICONERROR );
            EndDialog( hDlg, IDABORT );
        }

        SetMainDialogText( hDlg );

        g_bRecording = FALSE;
        g_bUseFilter = FALSE;

        ShowWindow( hDlg, SW_SHOW ); 
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
                if( FAILED( HandleNotification() ) )
                {
                    MessageBox( hDlg, "Error handling DirectSound notifications. "
                               "The sample will now exit.", "DirectSound Sample", 
                               MB_OK | MB_ICONERROR );
                    bDone = TRUE;
                }

                break;

            case WAIT_OBJECT_0 + 1:
                // g_hNotificationEvents[1] is signaled

                // This means that the stop event was signaled, so 
                // any neeeded update of could be done here
                break;

            case WAIT_OBJECT_0 + 2:
                // Messages are available
                while( PeekMessage( &msg, NULL, 0, 0, PM_REMOVE ) ) 
                { 
                    if( !IsDialogMessage( hDlg, &msg ) )  
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
    HRESULT hr;

    switch( msg ) 
    {
    case WM_COMMAND:
        switch( LOWORD(wParam) )
        {
            case IDCANCEL:
                EndDialog( hDlg, IDCANCEL );
                break;

            case IDOK:
                if( FAILED( hr = OnFormatsOK( hDlg ) ) )
                {
                    if( hr == DSERR_ALLOCATED )
                    {
                        MessageBox( hDlg, "Full duplex audio failed. "
                                    "Make sure the sound card supports this operation. "
                                    "The sample will now exit.", "DirectSound Sample", 
                                    MB_OK | MB_ICONERROR );
                    }
                    else
                    {
                        MessageBox( hDlg, "Error accepting DirectSound formats. "
                                    "The sample will now exit.", "DirectSound Sample", 
                                    MB_OK | MB_ICONERROR );
                    }
                    EndDialog( hDlg, IDABORT );
                }
                break;

            case IDC_FORMATS_INPUT_LISTBOX:
                OnInputFormatBoxSelected( hDlg );
                break;
            
            case IDC_FORMATS_OUTPUT_LISTBOX:
                if( HIWORD(wParam) == LBN_SELCHANGE )
                {
                    OnOutputFormatBoxSelected( hDlg );
                }
    
                break;

            default:
                return FALSE; // Didn't handle message
        }   
        break;

    case WM_INITDIALOG:
        if( FAILED( OnInitFormatsDialog( hDlg ) ) )
        {
            MessageBox( hDlg, "Error scanning DirectSound formats. "
                        "The sample will now exit.", "DirectSound Sample", 
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

    if( FAILED( hr = ScanAvailableOutputFormats() ) )
        return hr;

    HWND hOutputList = GetDlgItem( hDlg, IDC_FORMATS_OUTPUT_LISTBOX );
    FillFormatListBox( hOutputList, g_abOutputFormatSupported );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: OnInputFormatBoxSelected()
// Desc: When both format boxes have a selection then enable the OK button
//-----------------------------------------------------------------------------
HRESULT OnInputFormatBoxSelected( HWND hDlg )
{
    HWND hInputList  = GetDlgItem( hDlg, IDC_FORMATS_INPUT_LISTBOX );
    HWND hOutputList = GetDlgItem( hDlg, IDC_FORMATS_OUTPUT_LISTBOX );
    HWND hOK         = GetDlgItem( hDlg, IDOK );

    if( SendMessage( hInputList,  LB_GETCURSEL, 0, 0 ) != -1 &&
        SendMessage( hOutputList, LB_GETCURSEL, 0, 0 ) != -1 )
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
// Name: OnOutputFormatBoxSelected()
// Desc: When the output format is selected, then we can enumerate the
//       the availible capture formats that are supported
//-----------------------------------------------------------------------------
HRESULT OnOutputFormatBoxSelected( HWND hDlg )
{
    HRESULT      hr;
    WAVEFORMATEX wfx;
    HWND         hOutputList;
    DWORD        dwOutputSelect;
    DWORD        dwOutputWavIndex;
    HWND         hOK;

    hOK         = GetDlgItem( hDlg, IDOK );
    hOutputList = GetDlgItem( hDlg, IDC_FORMATS_OUTPUT_LISTBOX );

    EnableWindow( hOK, FALSE );

    dwOutputSelect = SendMessage( hOutputList, LB_GETCURSEL, 0, 0 );
    dwOutputWavIndex = SendMessage( hOutputList, LB_GETITEMDATA, dwOutputSelect, 0 );

    ZeroMemory( &wfx, sizeof(wfx) );
    wfx.wFormatTag = WAVE_FORMAT_PCM;

    GetWaveFormatFromIndex( dwOutputWavIndex, &wfx );

    if( FAILED( hr = g_pDSBPrimary->SetFormat( &wfx ) ) )
        return hr;

    if( FAILED( hr = ScanAvailableInputFormats() ) )
        return hr;

    HWND hInputList = GetDlgItem( hDlg, IDC_FORMATS_INPUT_LISTBOX );
    FillFormatListBox( hInputList, g_abInputFormatSupported );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: ConvertWaveFormatToString()
// Desc: Converts a format to a text string
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
// Desc: Sets the formats of the capture and sound buffer based on the UI
//-----------------------------------------------------------------------------
HRESULT OnFormatsOK( HWND hDlg )
{
    HRESULT       hr;
    DWORD         dwOutputSelect;
    DWORD         dwInputSelect;
    DWORD         dwOutputWavIndex;
    DWORD         dwInputWavIndex;
    WAVEFORMATEX  wfxInput;
    WAVEFORMATEX  wfxOutput;

    ZeroMemory( &wfxInput, sizeof(wfxInput) );
    wfxInput.wFormatTag = WAVE_FORMAT_PCM;

    ZeroMemory( &wfxOutput, sizeof(wfxOutput) );
    wfxOutput.wFormatTag = WAVE_FORMAT_PCM;

    HWND hInputList  = GetDlgItem( hDlg, IDC_FORMATS_INPUT_LISTBOX );
    HWND hOutputList = GetDlgItem( hDlg, IDC_FORMATS_OUTPUT_LISTBOX );

    dwOutputSelect   = SendMessage( hOutputList, LB_GETCURSEL, 0, 0 );
    dwOutputWavIndex = SendMessage( hOutputList, LB_GETITEMDATA, dwOutputSelect, 0 );

    dwInputSelect   = SendMessage( hInputList, LB_GETCURSEL, 0, 0 );
    dwInputWavIndex = SendMessage( hInputList, LB_GETITEMDATA, dwInputSelect, 0 );

    GetWaveFormatFromIndex( dwOutputWavIndex, &wfxOutput );
    GetWaveFormatFromIndex( dwInputWavIndex, &wfxInput );

    if( FAILED( hr = SetBufferFormats( &wfxInput, &wfxOutput ) ) )
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

            case IDC_RECORD:
                g_bRecording = !g_bRecording;

                if( !g_bRecording )
                {
                    StopBuffers();
                }
                else
                {
                    StartBuffers();
                }
                break;

            case IDC_MAIN_ENABLEFILTER_CHECK:
                g_bUseFilter = !g_bUseFilter;
                break;

            default:
                return FALSE; // Didn't handle message
        }
        break;

    case WM_ACTIVATE:
        if( LOWORD(wParam) == WA_INACTIVE )
        {
            StopBuffers();
        }
        else
        {
            if( g_bRecording )
            {
                if( FAILED( StartBuffers() ) )
                {
                    MessageBox( hDlg, "Error starting DirectSound buffers. "                            
                                "The sample will now exit.", "DirectSound Sample", 
                                MB_OK | MB_ICONERROR );
                    PostQuitMessage( 0 );
                    EndDialog( hDlg, IDABORT );
                }
            }
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
    hDlg = hDlg;

    // Load the icon
    HINSTANCE hInst = (HINSTANCE) GetWindowLong( hDlg, GWL_HINSTANCE );
    HICON hIcon = LoadIcon( hInst, MAKEINTRESOURCE( IDR_MAINFRAME ) );

    // Set the icon for this dialog.
    SendMessage( hDlg, WM_SETICON, ICON_BIG,   (LPARAM) hIcon );  // Set big icon
    SendMessage( hDlg, WM_SETICON, ICON_SMALL, (LPARAM) hIcon );  // Set small icon

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: SetMainDialogText()
// Desc: Sets the static text of the main dialog based on the formats
//-----------------------------------------------------------------------------
HRESULT SetMainDialogText( HWND hDlg )
{
    WAVEFORMATEX  wfxInput;
    WAVEFORMATEX  wfxOutputPri;
    WAVEFORMATEX  wfxOutputSec;

    ZeroMemory( &wfxInput,     sizeof(wfxInput) );
    ZeroMemory( &wfxOutputPri, sizeof(wfxOutputPri) );
    ZeroMemory( &wfxOutputSec, sizeof(wfxOutputSec) );

    g_pDSBCapture->GetFormat( &wfxInput,     sizeof(wfxInput), NULL );
    g_pDSBPrimary->GetFormat( &wfxOutputPri, sizeof(wfxOutputPri), NULL );
    g_pDSBOutput->GetFormat(  &wfxOutputSec, sizeof(wfxOutputSec), NULL );

    TCHAR strInputFormat[255];
    TCHAR strOutputSecFormat[255];
    TCHAR strOutputPriFormat[255];

    ConvertWaveFormatToString( &wfxInput, strInputFormat );
    ConvertWaveFormatToString( &wfxOutputPri, strOutputPriFormat );
    ConvertWaveFormatToString( &wfxOutputSec, strOutputSecFormat );
    
    HWND hInputFormatText     = GetDlgItem( hDlg, IDC_MAIN_INPUTFORMAT_TEXT );
    HWND hPrimaryOutputText   = GetDlgItem( hDlg, IDC_MAIN_PRIMARYFORMAT_TEXT );
    HWND hSecondaryOutputText = GetDlgItem( hDlg, IDC_MAIN_SECONDARYFORMAT_TEXT );    

    SetWindowText( hInputFormatText, strInputFormat );
    SetWindowText( hPrimaryOutputText, strOutputPriFormat );
    SetWindowText( hSecondaryOutputText, strOutputSecFormat );

    return S_OK;
}




