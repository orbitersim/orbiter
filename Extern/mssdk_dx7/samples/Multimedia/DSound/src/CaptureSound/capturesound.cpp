//-----------------------------------------------------------------------------
// File: CaptureSound.cpp
//
// Desc: DirectSound support file for sample showing how to use DirectSound 
//       to capture sound in a wave file 
//
// Copyright (c) 1999 Microsoft Corp. All rights reserved.
//-----------------------------------------------------------------------------
#define STRICT
#include <objbase.h>
#include <initguid.h>
#include <mmreg.h>
#include <dsound.h>
#include <commdlg.h>
#include <stdio.h>
#include "resource.h"
#include "WavWrite.h"




//-----------------------------------------------------------------------------
// Function-prototypes
//-----------------------------------------------------------------------------
extern VOID SetFileUI( TCHAR* strFileName );
HRESULT RecordCapturedData();
HRESULT InitNotifications();




//-----------------------------------------------------------------------------
// Defines, constants, and global variables
//-----------------------------------------------------------------------------
#define NUM_REC_NOTIFICATIONS  16
#define MAX(a,b)        ( (a) > (b) ? (a) : (b) )

#define SAFE_DELETE(p)  { if(p) { delete (p);     (p)=NULL; } }
#define SAFE_RELEASE(p) { if(p) { (p)->Release(); (p)=NULL; } }

LPDIRECTSOUNDCAPTURE       g_pDSCapture     = NULL;
LPDIRECTSOUNDCAPTUREBUFFER g_pDSBCapture    = NULL;
LPDIRECTSOUNDNOTIFY        g_pDSNotify      = NULL;

WAVEFORMATEX         g_wfxInput;
DSBPOSITIONNOTIFY    g_aPosNotify[ NUM_REC_NOTIFICATIONS + 1 ];  
HANDLE               g_hNotificationEvents[2]; 
BOOL                 g_abInputFormatSupported[16];
DWORD                g_dwCaptureBufferSize;
DWORD                g_dwNextCaptureOffset;
DWORD                g_dwNotifySize;
WAVEFORMATEX*        g_pCaptureWaveFormat;
CWaveSoundWrite*     g_pWaveSoundWrite;




//-----------------------------------------------------------------------------
// Name: InitDirectSound()
// Desc: Initilizes DirectSound
//-----------------------------------------------------------------------------
HRESULT InitDirectSound( HWND hDlg )
{
    HRESULT hr;

    ZeroMemory( &g_aPosNotify, sizeof(DSBPOSITIONNOTIFY) * 
                               (NUM_REC_NOTIFICATIONS + 1) );
    g_dwCaptureBufferSize = 0;
    g_dwNotifySize        = 0;
    g_pWaveSoundWrite     = NULL;

    // Initialize COM
    if( hr = CoInitialize(NULL) )
        return hr;

    // Create IDirectSoundCapture using the preferred capture device
    if( FAILED( hr = DirectSoundCaptureCreate( NULL, &g_pDSCapture, NULL ) ) )
        return hr;

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: FreeDirectSound()
// Desc: Releases DirectSound 
//-----------------------------------------------------------------------------
HRESULT FreeDirectSound()
{
    SAFE_DELETE( g_pCaptureWaveFormat );
    SAFE_DELETE( g_pWaveSoundWrite );

    // Release DirectSound interfaces
    SAFE_RELEASE( g_pDSNotify );
    SAFE_RELEASE( g_pDSBCapture );
    SAFE_RELEASE( g_pDSCapture ); 

    // Release COM
    CoUninitialize();

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: GetWaveFormatFromIndex()
// Desc: Returns 16 different wave formats based on nIndex
//-----------------------------------------------------------------------------
VOID GetWaveFormatFromIndex( INT nIndex, WAVEFORMATEX* pwfx )
{
    INT iSampleRate = nIndex / 4;
    INT iType = nIndex % 4;

    switch( iSampleRate )
    {
        case 0: pwfx->nSamplesPerSec =  8000; break;
        case 1: pwfx->nSamplesPerSec = 11025; break;
        case 2: pwfx->nSamplesPerSec = 22050; break;
        case 3: pwfx->nSamplesPerSec = 44100; break;
    }

    switch( iType )
    {
        case 0: pwfx->wBitsPerSample =  8; pwfx->nChannels = 1; break;
        case 1: pwfx->wBitsPerSample = 16; pwfx->nChannels = 1; break;
        case 2: pwfx->wBitsPerSample =  8; pwfx->nChannels = 2; break;
        case 3: pwfx->wBitsPerSample = 16; pwfx->nChannels = 2; break;
    }

    pwfx->nBlockAlign = pwfx->nChannels * ( pwfx->wBitsPerSample / 8 );
    pwfx->nAvgBytesPerSec = pwfx->nBlockAlign * pwfx->nSamplesPerSec;
}




//-----------------------------------------------------------------------------
// Name: ScanAvailableInputFormats()
// Desc: Tests to see if 16 different standard wave formats are supported by
//       the capture device 
//-----------------------------------------------------------------------------
HRESULT ScanAvailableInputFormats()
{
    HRESULT       hr;
    WAVEFORMATEX  wfx;
    HCURSOR       hCursor;
    DSCBUFFERDESC dscbd;
    LPDIRECTSOUNDCAPTUREBUFFER pDSCaptureBuffer = NULL;
    
    // This might take a second or two, so throw up the hourglass
    hCursor = GetCursor();
    SetCursor( LoadCursor( NULL, IDC_WAIT ) );
    
    ZeroMemory( &wfx, sizeof(wfx));
    wfx.wFormatTag = WAVE_FORMAT_PCM;

    ZeroMemory( &dscbd, sizeof(dscbd) );
    dscbd.dwSize = sizeof(dscbd);

    // Try 16 different standard format to see if they are supported
    for( INT iIndex = 0; iIndex < 16; iIndex++ )
    {
        GetWaveFormatFromIndex( iIndex, &wfx );

        // To test if a capture format is supported, try to create a 
        // new capture buffer using a specific format.  If it works
        // then the format is supported, otherwise not.
        dscbd.dwBufferBytes = wfx.nAvgBytesPerSec;
        dscbd.lpwfxFormat = &wfx;
        
        if( FAILED( hr = g_pDSCapture->CreateCaptureBuffer( &dscbd, 
                                                            &pDSCaptureBuffer, 
                                                            NULL ) ) )
            g_abInputFormatSupported[ iIndex ] = FALSE;
        else
            g_abInputFormatSupported[ iIndex ] = TRUE;

        SAFE_RELEASE( pDSCaptureBuffer );
    }

    SetCursor( hCursor );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: CreateCaptureBuffer()
// Desc: Creates a capture buffer and sets the format 
//-----------------------------------------------------------------------------
HRESULT CreateCaptureBuffer( WAVEFORMATEX* pwfxInput )
{
    HRESULT hr;
    DSCBUFFERDESC dscbd;

    SAFE_RELEASE( g_pDSNotify );
    SAFE_RELEASE( g_pDSBCapture );

    // Set the notification size
    g_dwNotifySize = MAX( 1024, pwfxInput->nAvgBytesPerSec / 8 );
    g_dwNotifySize -= g_dwNotifySize % pwfxInput->nBlockAlign;   

    // Set the buffer sizes 
    g_dwCaptureBufferSize = g_dwNotifySize * NUM_REC_NOTIFICATIONS;

    SAFE_RELEASE( g_pDSNotify );
    SAFE_RELEASE( g_pDSBCapture );

    // Create the capture buffer
    ZeroMemory( &dscbd, sizeof(dscbd) );
    dscbd.dwSize        = sizeof(dscbd);
    dscbd.dwBufferBytes = g_dwCaptureBufferSize;
    dscbd.lpwfxFormat   = pwfxInput; // Set the format during creatation

    if( FAILED( hr = g_pDSCapture->CreateCaptureBuffer( &dscbd, 
                                                        &g_pDSBCapture, 
                                                        NULL ) ) )
        return hr;

    g_dwNextCaptureOffset = 0;

    if( FAILED( InitNotifications() ) )
        return hr;

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: InitNotifications()
// Desc: Inits the notifications on the capture buffer which are handled
//       in WinMain()
//-----------------------------------------------------------------------------
HRESULT InitNotifications()
{
    HRESULT hr; 

    if( NULL == g_pDSBCapture )
        return E_FAIL;

    // Create a notification event, for when the sound stops playing
    if( FAILED( hr = g_pDSBCapture->QueryInterface( IID_IDirectSoundNotify, 
                                                    (VOID**)&g_pDSNotify ) ) )
        return hr;

    // Setup the notification positions
    for( INT i = 0; i < NUM_REC_NOTIFICATIONS; i++ )
    {
        g_aPosNotify[i].dwOffset = (g_dwNotifySize * i) + g_dwNotifySize - 1;
        g_aPosNotify[i].hEventNotify = g_hNotificationEvents[0];             
    }
    
    g_aPosNotify[i].dwOffset     = DSBPN_OFFSETSTOP;
    g_aPosNotify[i].hEventNotify = g_hNotificationEvents[1];

    // Tell DirectSound when to notify us. the notification will come in the from 
    // of signaled events that are handled in WinMain()
    if( FAILED( hr = g_pDSNotify->SetNotificationPositions( NUM_REC_NOTIFICATIONS + 1, 
                                                            g_aPosNotify ) ) )
        return hr;

    // Save the format of the capture buffer in g_pCaptureWaveFormat
    SAFE_DELETE( g_pCaptureWaveFormat );

    g_pCaptureWaveFormat = new WAVEFORMATEX;
    ZeroMemory( g_pCaptureWaveFormat, sizeof(WAVEFORMATEX) );
    g_pDSBCapture->GetFormat( g_pCaptureWaveFormat, sizeof(WAVEFORMATEX), NULL );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: CreateWaveFile()
// Desc: Creates a wave file 
//-----------------------------------------------------------------------------
VOID CreateWaveFile( TCHAR* strFileName )
{
    SAFE_DELETE( g_pWaveSoundWrite );

    g_pWaveSoundWrite = new CWaveSoundWrite();

    // Load the wave file
    if( FAILED( g_pWaveSoundWrite->Open( strFileName, g_pCaptureWaveFormat ) ) )
    {
        SetFileUI( TEXT("Can not create wave file.") );
    }
    else // The load call succeeded
    {
        // Update the UI controls to show the sound as the file is loaded
        SetFileUI( strFileName );
    }
}




//-----------------------------------------------------------------------------
// Name: StartBuffers()
// Desc: Starts the capture buffer
//-----------------------------------------------------------------------------
HRESULT StartBuffers()
{
    HRESULT hr;

    // Tell the capture buffer to start recording
    if( FAILED( hr = g_pDSBCapture->Start( DSCBSTART_LOOPING ) ) )
        return hr;
    
    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: StopBuffers()
// Desc: Stop the capture buffer
//-----------------------------------------------------------------------------
HRESULT StopBuffers()
{
    if( NULL == g_pDSBCapture )
        return S_OK;

    g_pDSBCapture->Stop();

    // Read any data that was not caught by a notification
    RecordCapturedData();

    // Close the wav file
    SAFE_DELETE( g_pWaveSoundWrite );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: RecordCapturedData()
// Desc: Copies data from the capture buffer to the output buffer 
//-----------------------------------------------------------------------------
HRESULT RecordCapturedData() 
{
    HRESULT hr;
    VOID*   pbCaptureData    = NULL;
    DWORD   dwCaptureLength;
    VOID*   pbCaptureData2   = NULL;
    DWORD   dwCaptureLength2;
    VOID*   pbPlayData       = NULL;
    UINT    dwDataWrote;
    DWORD   dwReadPos;
    DWORD   dwCapturePos;
    LONG lLockSize;

    if( NULL == g_pDSBCapture )
        return S_FALSE;
    if( NULL == g_pWaveSoundWrite )
        return S_FALSE;

    if( FAILED( hr = g_pDSBCapture->GetCurrentPosition( &dwCapturePos, &dwReadPos ) ) )
        return hr;

    lLockSize = dwReadPos - g_dwNextCaptureOffset;
    if( lLockSize < 0 )
        lLockSize += g_dwCaptureBufferSize;

    // Block align lock size so that we are always write on a boundary
    lLockSize -= (lLockSize % g_dwNotifySize);

    if( lLockSize == 0 )
        return S_FALSE;

    char szRecordMsg[200];
    sprintf( szRecordMsg, "Writing %5d bytes to file from capture buffer starting at offset %d\n", lLockSize, g_dwNextCaptureOffset );
    OutputDebugString( szRecordMsg );

    // Lock the capture buffer down
    if( FAILED( hr = g_pDSBCapture->Lock( g_dwNextCaptureOffset, lLockSize, 
                                          &pbCaptureData, &dwCaptureLength, 
                                          &pbCaptureData2, &dwCaptureLength2, 0L ) ) )
        return hr;

    // Write the data into the wav file
    if( FAILED( hr = g_pWaveSoundWrite->Write( dwCaptureLength, 
                                               (BYTE*)pbCaptureData, 
                                               &dwDataWrote ) ) )
        return hr; 

    // Unlock the capture buffer
    g_pDSBCapture->Unlock( pbCaptureData, dwCaptureLength, NULL, 0 );

    // Move the capture offset along
    g_dwNextCaptureOffset += dwCaptureLength; 
    g_dwNextCaptureOffset %= g_dwCaptureBufferSize; // Circular buffer

    if( pbCaptureData2 != NULL )
    {
        // Write the data into the wav file
        if( FAILED( hr = g_pWaveSoundWrite->Write( dwCaptureLength2, 
                                                   (BYTE*)pbCaptureData2, 
                                                   &dwDataWrote ) ) )
            return hr; 

        // Unlock the capture buffer
        g_pDSBCapture->Unlock( pbCaptureData2, dwCaptureLength2, NULL, 0 );

        // Move the capture offset along
        g_dwNextCaptureOffset += dwCaptureLength2; 
        g_dwNextCaptureOffset %= g_dwCaptureBufferSize; // Circular buffer
    }

    return S_OK;
}



