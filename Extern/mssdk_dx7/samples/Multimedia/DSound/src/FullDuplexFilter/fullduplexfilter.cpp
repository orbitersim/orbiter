//-----------------------------------------------------------------------------
// File: FullDuplexFilter.cpp
//
// Desc: DirectSound support file for sample showing how to use DirectSound 
//       to implement full duplex audio and a filter.
//
// Copyright (c) 1999 Microsoft Corp. All rights reserved.
//-----------------------------------------------------------------------------
#define STRICT
#include <objbase.h>
#include <initguid.h>
#include <commdlg.h>
#include <mmreg.h>
#include <dsound.h>
#include <stdio.h>
#include "resource.h"




//-----------------------------------------------------------------------------
// Function-prototypes
//-----------------------------------------------------------------------------
HRESULT TransformData( BYTE* pbOut, BYTE* pbIn, DWORD dwLength );
HRESULT RestoreBuffers();




//-----------------------------------------------------------------------------
// Defines, constants, and global variables
//-----------------------------------------------------------------------------
#define NUM_PLAY_NOTIFICATIONS  16
#define NUM_BUFFERS     (16)
#define MAX(a,b)        ( (a) > (b) ? (a) : (b) )

#define SAFE_DELETE(p)  { if(p) { delete (p);     (p)=NULL; } }
#define SAFE_RELEASE(p) { if(p) { (p)->Release(); (p)=NULL; } }

LPDIRECTSOUND              g_pDS            = NULL;
LPDIRECTSOUNDCAPTURE       g_pDSCapture     = NULL;
LPDIRECTSOUNDBUFFER        g_pDSBPrimary    = NULL;
LPDIRECTSOUNDBUFFER        g_pDSBOutput     = NULL;
LPDIRECTSOUNDCAPTUREBUFFER g_pDSBCapture    = NULL;
LPDIRECTSOUNDNOTIFY        g_pDSNotify      = NULL;

DSBPOSITIONNOTIFY    g_aPosNotify[ NUM_PLAY_NOTIFICATIONS + 1 ];  
HANDLE               g_hNotificationEvents[2]; 
BOOL                 g_bUseFilter;
BOOL                 g_abOutputFormatSupported[16];
BOOL                 g_abInputFormatSupported[16];
DWORD                g_dwPlayBufferSize;
DWORD                g_dwCaptureBufferSize;
DWORD                g_dwNextPlayOffset;
DWORD                g_dwNextCaptureOffset;
DWORD                g_dwNotifySize;
WAVEFORMATEX*        g_pCaptureWaveFormat;




//-----------------------------------------------------------------------------
// Name: InitDirectSound()
// Desc: Initilizes DirectSound
//-----------------------------------------------------------------------------
HRESULT InitDirectSound( HWND hDlg )
{
    HRESULT             hr;
    DSBUFFERDESC        dsbdesc;

    ZeroMemory( &g_aPosNotify, sizeof(DSBPOSITIONNOTIFY) * 
                               (NUM_PLAY_NOTIFICATIONS + 1) );
    g_dwPlayBufferSize    = 0;
    g_dwCaptureBufferSize = 0;
    g_dwNotifySize        = 0;
    g_dwNextPlayOffset    = 0;

    // Initialize COM
    if( hr = CoInitialize(NULL) )
        return hr;

    // Create IDirectSound using the preferred sound device
    if( FAILED( hr = DirectSoundCreate( NULL, &g_pDS, NULL ) ) )
        return hr;

    // Set coop level to DSSCL_PRIORITY
    if( FAILED( hr = g_pDS->SetCooperativeLevel( hDlg, DSSCL_PRIORITY ) ) )
        return hr;

    // Obtain primary buffer 
    ZeroMemory( &dsbdesc, sizeof(DSBUFFERDESC) );
    dsbdesc.dwSize  = sizeof(DSBUFFERDESC);
    dsbdesc.dwFlags = DSBCAPS_PRIMARYBUFFER;

    if( FAILED( hr = g_pDS->CreateSoundBuffer( &dsbdesc, &g_pDSBPrimary, NULL ) ) )
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
    // Release DirectSound interfaces
    SAFE_DELETE( g_pCaptureWaveFormat );
    SAFE_RELEASE( g_pDSNotify );

    SAFE_RELEASE( g_pDSBPrimary );
    SAFE_RELEASE( g_pDSBOutput );
    SAFE_RELEASE( g_pDSBCapture );

    SAFE_RELEASE( g_pDSCapture ); 
    SAFE_RELEASE( g_pDS ); 

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
// Name: ScanAvailableOutputFormats()
// Desc: Tests to see if 16 different standard wave formats are supported by
//       the playback device 
//-----------------------------------------------------------------------------
HRESULT ScanAvailableOutputFormats()
{
    WAVEFORMATEX wfx;
    WAVEFORMATEX wfxSet;
    HRESULT      hr;
    HCURSOR      hCursor;
    
    // This might take a second or two, so throw up the hourglass
    hCursor = GetCursor();
    SetCursor( LoadCursor( NULL, IDC_WAIT ) );
    
    ZeroMemory( &wfxSet, sizeof(wfxSet) );
    wfxSet.wFormatTag = WAVE_FORMAT_PCM;

    ZeroMemory( &wfx, sizeof(wfx) );
    wfx.wFormatTag = WAVE_FORMAT_PCM;
    
    // Try 16 different standard format to see if they are supported
    for( INT iIndex = 0; iIndex < 16; iIndex++ )
    {
        GetWaveFormatFromIndex( iIndex, &wfx );

        // To test if a playback format is supported, try to set the format 
        // using a specific format.  If it works then the format is 
        // supported, otherwise not.
        if( FAILED( hr = g_pDSBPrimary->SetFormat( &wfx ) ) )
        {
            g_abOutputFormatSupported[ iIndex ] = FALSE;
        }
        else
        {
            // Get the format that was just set, and see if it 
            // is actually supported since SetFormat() sometimes returns DS_OK 
            // even if the format was not supported
            if( FAILED( hr = g_pDSBPrimary->GetFormat( &wfxSet, sizeof(wfxSet), 
                                                       NULL ) ) )
                return hr;

            if( memcmp( &wfx, &wfxSet, sizeof(wfx) ) == 0 )
                g_abOutputFormatSupported[ iIndex ] = TRUE;
            else
                g_abOutputFormatSupported[ iIndex ] = FALSE;    
        }
    }

    SetCursor( hCursor );

    return S_OK;
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
    
    ZeroMemory( &wfx, sizeof(wfx) );
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
// Name: SetBufferFormats()
// Desc: Sets the buffer formats for the primary buffer, and the capture buffer
//-----------------------------------------------------------------------------
HRESULT SetBufferFormats( WAVEFORMATEX* pwfxInput, 
                          WAVEFORMATEX* pwfxOutput )
{
    HRESULT hr;
    DSCBUFFERDESC dscbd;

    // Set the format of the primary buffer 
    // to the format of the output buffer
    if( FAILED( hr = g_pDSBPrimary->SetFormat( pwfxOutput ) ) )
        return hr;

    // Set the notification size
    g_dwNotifySize = MAX( 4096, pwfxInput->nAvgBytesPerSec / 8 );
    g_dwNotifySize -= g_dwNotifySize % pwfxInput->nBlockAlign;   

    // Set the buffer sizes 
    g_dwPlayBufferSize = NUM_BUFFERS * g_dwNotifySize / 2;
    g_dwCaptureBufferSize = g_dwNotifySize * NUM_BUFFERS;

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

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: CreateOutputBuffer()
// Desc: Creates the ouptut buffer and sets up the notification positions
//       on the capture buffer
//-----------------------------------------------------------------------------
HRESULT CreateOutputBuffer()
{
    HRESULT hr; 
    WAVEFORMATEX wfxInput;

    // This sample works by creating notification events which 
    // are signaled when the capture buffer reachs specific offsets 
    // WinMain() waits for the associated event to be signaled, and
    // when it is, it calls HandleNotifications() which copy the 
    // data from the capture buffer into the output buffer
    // while optionally running it through a filter

    ZeroMemory( &wfxInput, sizeof(wfxInput) );
    g_pDSBCapture->GetFormat( &wfxInput, sizeof(wfxInput), NULL );

    // Set up the direct sound buffer, and only request the flags needed
    // since each requires some overhead and limits if the buffer can 
    // be hardware accelerated
    DSBUFFERDESC dsbd;
    ZeroMemory( &dsbd, sizeof(DSBUFFERDESC) );
    dsbd.dwSize        = sizeof(DSBUFFERDESC);
    dsbd.dwFlags       = DSBCAPS_GLOBALFOCUS;
    dsbd.dwBufferBytes = g_dwPlayBufferSize;
    dsbd.lpwfxFormat   = &wfxInput;

    // Create a DirectSound buffer
    if( FAILED( hr = g_pDS->CreateSoundBuffer( &dsbd, &g_pDSBOutput, NULL ) ) )
        return hr;

    // Create a notification event, for when the sound stops playing
    if( FAILED( hr = g_pDSBCapture->QueryInterface( IID_IDirectSoundNotify, 
                                                    (VOID**)&g_pDSNotify ) ) )
        return hr;

    // Setup the notification positions
    for( INT i = 0; i < NUM_PLAY_NOTIFICATIONS; i++ )
    {
        g_aPosNotify[i].dwOffset = (g_dwNotifySize * i) + g_dwNotifySize - 1;
        g_aPosNotify[i].hEventNotify = g_hNotificationEvents[0];             
    }
    
    g_aPosNotify[i].dwOffset     = DSBPN_OFFSETSTOP;
    g_aPosNotify[i].hEventNotify = g_hNotificationEvents[1];

    // Tell DirectSound when to notify us. the notification will come in the from 
    // of signaled events that are handled in WinMain()
    if( FAILED( hr = g_pDSNotify->SetNotificationPositions( NUM_PLAY_NOTIFICATIONS + 1, 
                                                            g_aPosNotify ) ) )
        return hr;

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: StartBuffers()
// Desc: Start the capture buffer, and the start playing the output buffer
//-----------------------------------------------------------------------------
HRESULT StartBuffers()
{
    LPVOID pbBuffer;
    DWORD dwBufferSize;
    WAVEFORMATEX wfxOutput;
    HRESULT hr;

    // Restore lost buffers
    if( FAILED( hr = RestoreBuffers() ) )
        return hr;

    // Find out where the capture buffer is right now, then write data 
    // some extra amount forward to make sure we're ahead of the write cursor
    g_pDSBCapture->GetCurrentPosition( &g_dwNextCaptureOffset, NULL );
    g_dwNextCaptureOffset -= g_dwNextCaptureOffset % g_dwNotifySize;

    g_dwNextPlayOffset = g_dwNextCaptureOffset + ( 2 * g_dwNotifySize );
    g_dwNextPlayOffset %= g_dwPlayBufferSize;  // Circular buffer

    // Tell the capture buffer to start recording
    g_pDSBCapture->Start( DSCBSTART_LOOPING );
    
    // Rewind the output buffer, fill it with silence, and play it
    g_pDSBOutput->SetCurrentPosition( g_dwNextPlayOffset );

    // Save the format of the capture buffer in g_pCaptureWaveFormat
    SAFE_DELETE( g_pCaptureWaveFormat );
    g_pCaptureWaveFormat = new WAVEFORMATEX;
    ZeroMemory( g_pCaptureWaveFormat, sizeof(WAVEFORMATEX) );
    g_pDSBCapture->GetFormat( g_pCaptureWaveFormat, sizeof(WAVEFORMATEX), NULL );

    // Get the format of the output buffer
    ZeroMemory( &wfxOutput, sizeof(wfxOutput) );
    g_pDSBOutput->GetFormat( &wfxOutput, sizeof(wfxOutput), NULL );

    // Fill the output buffer with silence at first
    // As capture data arrives, HandleNotifications() will fill
    // the output buffer with wave data.
    if( FAILED( hr = g_pDSBOutput->Lock( 0, g_dwPlayBufferSize, 
                                         &pbBuffer, &dwBufferSize, 
                                         NULL, NULL, 0 ) ) )
        return hr;
    FillMemory( (BYTE*) pbBuffer, dwBufferSize, 
                (BYTE)( wfxOutput.wBitsPerSample == 8 ? 128 : 0 ) );
    g_pDSBOutput->Unlock( pbBuffer, dwBufferSize, NULL, NULL ); 

    // Play the output buffer 
    g_pDSBOutput->Play( 0, 0, DSBPLAY_LOOPING );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: StopBuffers()
// Desc: Stop both the capture and the output buffers
//-----------------------------------------------------------------------------
HRESULT StopBuffers()
{
    if( NULL == g_pDSBCapture || 
        NULL == g_pDSBOutput )
        return S_OK;

    g_pDSBCapture->Stop();
    g_pDSBOutput->Stop();
    
    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: RestoreBuffers()
// Desc: Restore lost buffers and fill them up with sound if possible
//-----------------------------------------------------------------------------
HRESULT RestoreBuffers()
{
    HRESULT hr;

    if( NULL == g_pDSBCapture || 
        NULL == g_pDSBOutput )
        return S_OK;

    DWORD dwStatus;
    if( FAILED( hr = g_pDSBOutput->GetStatus( &dwStatus ) ) )
        return hr;

    if( dwStatus & DSBSTATUS_BUFFERLOST )
    {
        // Since the app could have just been activated, then
        // DirectSound may not be giving us control yet, so 
        // the restoring the buffer may fail.  
        // If it does, sleep until DirectSound gives us control.
        do 
        {
            hr = g_pDSBOutput->Restore();
            if( hr == DSERR_BUFFERLOST )
                Sleep( 10 );
        }
        while( hr = g_pDSBOutput->Restore() );
    }

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: HandleNotification()
// Desc: Handle the notification that tells us to copy data from the 
//       capture buffer to the output buffer 
//-----------------------------------------------------------------------------
HRESULT HandleNotification() 
{
    HRESULT hr;
    VOID* pbCaptureData    = NULL;
    VOID* pbPlayData       = NULL;
    DWORD dwCaptureLength;
    DWORD dwPlayLength;
   
    DWORD dwStatus;
    if( FAILED( hr = g_pDSBOutput->GetStatus( &dwStatus ) ) )
        return hr;

    if( dwStatus & DSBSTATUS_BUFFERLOST )
    {
        // Start capturing again, since it they have stopped
        if( FAILED( hr = StartBuffers() ) )
            return hr;

        return S_OK;
    }

    // Lock the capture buffer down
    if( FAILED( hr = g_pDSBCapture->Lock( g_dwNextCaptureOffset, g_dwNotifySize, 
                                          &pbCaptureData, &dwCaptureLength, 
                                          NULL, NULL, 0L ) ) )
        return hr;

    // Lock the output buffer down
    if( FAILED( hr = g_pDSBOutput->Lock( g_dwNextPlayOffset, g_dwNotifySize, 
                                         &pbPlayData, &dwPlayLength, 
                                         NULL, NULL, 0L ) ) )
        return hr;

    if( g_bUseFilter )
    {
        // Filter the data before coping the capture data into 
        // the playback buffer
        TransformData( (BYTE*) pbPlayData, (BYTE*) pbCaptureData, dwPlayLength );
    }
    else
    {
        // Just copy the memory from the capture buffer
        // to the playback buffer if we are not using the filter
        CopyMemory( pbPlayData, pbCaptureData, dwPlayLength );
    }

    // Unlock the play buffer
    g_pDSBOutput->Unlock( pbPlayData, dwPlayLength, NULL, 0 );

    // Unlock the capture buffer
    g_pDSBCapture->Unlock( pbCaptureData, dwCaptureLength, NULL, 0 );

    // Move the capture offset along
    g_dwNextCaptureOffset += dwCaptureLength; 
    g_dwNextCaptureOffset %= g_dwCaptureBufferSize; // Circular buffer

    // Move the playback offset along
    g_dwNextPlayOffset += dwPlayLength; 
    g_dwNextPlayOffset %= g_dwPlayBufferSize; // Circular buffer

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: TransformData()
// Desc: Applies a gargle filter to the data from pbIn and copies it to pbOut
//-----------------------------------------------------------------------------
HRESULT TransformData( BYTE* pbOut, BYTE* pbIn, DWORD dwLength )
{
    static INT s_nPhase = 0;

    INT nGargleRate = 10;
    INT nPeriod = ( g_pCaptureWaveFormat->nSamplesPerSec * 
                    g_pCaptureWaveFormat->nChannels ) / nGargleRate;

    while( dwLength )
    {
        dwLength--;

        // If m_Shape is 0 (triangle) then we multiply by a triangular waveform
        // that runs 0..nPeriod/2..0..nPeriod/2..0... else by a square one that
        // is either 0 or nPeriod/2 (same maximum as the triangle) or zero.
        //
        {
            // s_Phase is the number of samples from the start of the period.
            // We keep this running from one call to the next,
            // but if the period changes so as to make this more
            // than nPeriod then we reset to 0 with a bang, so this may cause
            // an audible click or pop 

            ++s_nPhase;
            if( s_nPhase > nPeriod )
                s_nPhase = 0;
            
            INT nModulate = s_nPhase; 
            
            // Set this to 0 to use a triangle wave
            // or 1 to use a square wave
            if( 1 ) 
            {
                // Square wave
                if( nModulate <= nPeriod / 2 )
                    nModulate = nPeriod / 2;
                else 
                    nModulate = 0;
            }
            else
            {
                // Triangle wave
                if( nModulate > nPeriod / 2 ) // handle downslope
                    nModulate = nPeriod - nModulate;  
            }
            
            if( g_pCaptureWaveFormat->wBitsPerSample == 8 )
            {
                // 8 bit sound uses 0..255 representing -128..127
                // Any overflow, even by 1, would sound very bad.
                // so we clip paranoically after modulating.
                // I think it should never clip by more than 1
                //
                INT i = *pbIn - 128;  // Sound sample, zero based
                i = ( i * nModulate * 2 ) / nPeriod; // Modulate

                // Clip
                if( i > 127 )                     
                    i = 127;
                if( i < -128 )
                    i = -128;
                
                *pbOut = (BYTE)( i + 128 ); // Reset zero offset to 128                
            } 
            else 
            {
                // 16 bit sound uses 16 bits properly (0 means 0)
                // We still clip paranoically

                WORD* psi = (WORD*) pbOut;
                INT i = *( (WORD*)pbIn );   // In a register, we might hope
                i = ( i * nModulate * 2 ) / nPeriod;         // Modulate

                // Clip 
                if( i > 32767 )
                    i = 32767;              
                if( i < -32768 )
                    i = -32768;

                *psi = (WORD)i;

                ++pbIn;     // Nudge it on another 8 bits here to get a 16 bit step
                ++pbOut;

                --dwLength; // And nudge the count too.
            } 
        }

        // Move on 8 bits to next sound sample
        ++pbIn;   
        ++pbOut;
    }
        
    return S_OK;
}
