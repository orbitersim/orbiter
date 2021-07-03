//-----------------------------------------------------------------------------
// File: PlayStreaming.cpp
//
// Desc: DirectSound support file for sample showing how to load a wave file 
//       and play it using a streaming DirectSound buffer.
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
#include "WavRead.h"




//-----------------------------------------------------------------------------
// Function-prototypes
//-----------------------------------------------------------------------------
extern VOID OnEnablePlayUI( HWND hDlg, BOOL bEnable );
extern VOID SetFileUI( HWND hDlg, TCHAR* strFileName );

HRESULT CreateStreamingBuffer();
HRESULT UpdateProgress();
HRESULT FillBuffer( BOOL bLooped );
HRESULT WriteToBuffer( BOOL bLooped, VOID* pbBuffer, DWORD dwBufferLength );
HRESULT RestoreBuffers( BOOL bLooped );




//-----------------------------------------------------------------------------
// Defines, constants, and global variables
//-----------------------------------------------------------------------------
#define NUM_PLAY_NOTIFICATIONS  16

#define SAFE_DELETE(p)  { if(p) { delete (p);     (p)=NULL; } }
#define SAFE_RELEASE(p) { if(p) { (p)->Release(); (p)=NULL; } }

LPDIRECTSOUND       g_pDS            = NULL;
LPDIRECTSOUNDBUFFER g_pDSBuffer      = NULL;
LPDIRECTSOUNDNOTIFY g_pDSNotify      = NULL;
CWaveSoundRead*     g_pWaveSoundRead = NULL;

DSBPOSITIONNOTIFY   g_aPosNotify[ NUM_PLAY_NOTIFICATIONS + 1 ];  
HANDLE              g_hNotificationEvents[2];

DWORD               g_dwBufferSize;
DWORD               g_dwNotifySize;
DWORD               g_dwNextWriteOffset;
DWORD               g_dwProgress;
DWORD               g_dwLastPos;
BOOL                g_bFoundEnd;




//-----------------------------------------------------------------------------
// Name: InitDirectSound()
// Desc: Initilizes DirectSound
//-----------------------------------------------------------------------------
HRESULT InitDirectSound( HWND hDlg )
{
    HRESULT             hr;
    LPDIRECTSOUNDBUFFER pDSBPrimary = NULL;

    ZeroMemory( &g_aPosNotify, sizeof(DSBPOSITIONNOTIFY) * 
                               (NUM_PLAY_NOTIFICATIONS + 1) );
    g_dwBufferSize      = 0;
    g_dwNotifySize      = 0;
    g_dwNextWriteOffset = 0;
    g_dwProgress        = 0;
    g_dwLastPos         = 0;
    g_bFoundEnd         = FALSE;

    // Initialize COM
    if( hr = CoInitialize(NULL) )
        return hr;

    // Create IDirectSound using the primary sound device
    if( FAILED( hr = DirectSoundCreate( NULL, &g_pDS, NULL ) ) )
        return hr;

    // Set coop level to DSSCL_PRIORITY
    if( FAILED( hr = g_pDS->SetCooperativeLevel( hDlg, DSSCL_PRIORITY ) ) )
        return hr;

    // Get the primary buffer 
    DSBUFFERDESC        dsbd;
    ZeroMemory( &dsbd, sizeof(DSBUFFERDESC) );
    dsbd.dwSize        = sizeof(DSBUFFERDESC);
    dsbd.dwFlags       = DSBCAPS_PRIMARYBUFFER;
    dsbd.dwBufferBytes = 0;
    dsbd.lpwfxFormat   = NULL;
       
    if( FAILED( hr = g_pDS->CreateSoundBuffer( &dsbd, &pDSBPrimary, NULL ) ) )
        return hr;

    // Set primary buffer format to 22kHz and 16-bit output.
    WAVEFORMATEX wfx;
    ZeroMemory( &wfx, sizeof(WAVEFORMATEX) ); 
    wfx.wFormatTag      = WAVE_FORMAT_PCM; 
    wfx.nChannels       = 2; 
    wfx.nSamplesPerSec  = 22050; 
    wfx.wBitsPerSample  = 16; 
    wfx.nBlockAlign     = wfx.wBitsPerSample / 8 * wfx.nChannels;
    wfx.nAvgBytesPerSec = wfx.nSamplesPerSec * wfx.nBlockAlign;

    if( FAILED( hr = pDSBPrimary->SetFormat(&wfx) ) )
        return hr;

    SAFE_RELEASE( pDSBPrimary );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: FreeDirectSound()
// Desc: Releases DirectSound 
//-----------------------------------------------------------------------------
HRESULT FreeDirectSound()
{
    if( NULL != g_pWaveSoundRead )
    {
        g_pWaveSoundRead->Close();
        SAFE_DELETE( g_pWaveSoundRead );
    }

    // Release DirectSound interfaces
    SAFE_RELEASE( g_pDSNotify );
    SAFE_RELEASE( g_pDSBuffer );
    SAFE_RELEASE( g_pDS ); 

    // Release COM
    CoUninitialize();

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: LoadWaveFile()
// Desc: Loads a wave file into a secondary streaming DirectSound buffer
//-----------------------------------------------------------------------------
VOID LoadWaveFile( HWND hDlg, TCHAR* strFileName )
{
    // Close and delete any existing open wave file
    if( NULL != g_pWaveSoundRead )
    {
        g_pWaveSoundRead->Close();
        SAFE_DELETE( g_pWaveSoundRead );
    }

    g_pWaveSoundRead = new CWaveSoundRead();

    // Load the wave file
    if( FAILED( g_pWaveSoundRead->Open( strFileName ) ) )
    {
        SetFileUI( hDlg, TEXT("Bad wave file.") );
    }
    else // The load call succeeded
    {
        // Start with a new sound buffer object
        SAFE_RELEASE( g_pDSNotify );
        SAFE_RELEASE( g_pDSBuffer );
        ZeroMemory( &g_aPosNotify, sizeof(DSBPOSITIONNOTIFY) );

        // Create the sound buffer object from the wave file data
        if( FAILED( CreateStreamingBuffer() ) )
        {
            SetFileUI( hDlg, TEXT("Couldn't create sound buffer.") );
        }
        else // The sound buffer was successfully created
        {
            // Update the UI controls to show the sound as the file is loaded
            OnEnablePlayUI( hDlg, TRUE );
            SetFileUI( hDlg, strFileName );
        }
    }
}




//-----------------------------------------------------------------------------
// Name: CreateStreamingBuffer()
// Desc: Creates a streaming buffer, and the notification events to handle
//       filling it as sound is played
//-----------------------------------------------------------------------------
HRESULT CreateStreamingBuffer()
{
    HRESULT hr; 

    // This samples works by dividing a 3 second streaming buffer into 
    // NUM_PLAY_NOTIFICATIONS (or 16) pieces.  it creates a notification for each
    // piece and when a notification arrives then it fills the circular streaming 
    // buffer with new wav data over the sound data which was just played

    // The size of wave data is in pWaveFileSound->m_ckIn
    DWORD nBlockAlign = (DWORD)g_pWaveSoundRead->m_pwfx->nBlockAlign;
    INT nSamplesPerSec = g_pWaveSoundRead->m_pwfx->nSamplesPerSec;

    // The g_dwNotifySize should be an integer multiple of nBlockAlign
    g_dwNotifySize = nSamplesPerSec * 3 * nBlockAlign / NUM_PLAY_NOTIFICATIONS;
    g_dwNotifySize -= g_dwNotifySize % nBlockAlign;   

    // The buffersize should approximately 3 seconds of wav data
    g_dwBufferSize  = g_dwNotifySize * NUM_PLAY_NOTIFICATIONS;
    g_bFoundEnd     = FALSE;
    g_dwProgress    = 0;
    g_dwLastPos     = 0;

    // Set up the direct sound buffer, and only request the flags needed
    // since each requires some overhead and limits if the buffer can 
    // be hardware accelerated
    DSBUFFERDESC dsbd;
    ZeroMemory( &dsbd, sizeof(DSBUFFERDESC) );
    dsbd.dwSize        = sizeof(DSBUFFERDESC);
    dsbd.dwFlags       = DSBCAPS_CTRLPOSITIONNOTIFY | DSBCAPS_GETCURRENTPOSITION2;
    dsbd.dwBufferBytes = g_dwBufferSize;
    dsbd.lpwfxFormat   = g_pWaveSoundRead->m_pwfx;

    // Create a DirectSound buffer
    if( FAILED( hr = g_pDS->CreateSoundBuffer( &dsbd, &g_pDSBuffer, NULL ) ) )
        return hr;

    // Create a notification event, for when the sound stops playing
    if( FAILED( hr = g_pDSBuffer->QueryInterface( IID_IDirectSoundNotify, 
                                                  (VOID**)&g_pDSNotify ) ) )
        return hr;

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
// Name: PlayBuffer()
// Desc: Play the DirectSound buffer
//-----------------------------------------------------------------------------
HRESULT PlayBuffer( BOOL bLooped )
{
    HRESULT hr;
    VOID*   pbBuffer = NULL;

    if( NULL == g_pDSBuffer )
        return E_FAIL;

    // Restore the buffers if they are lost
    if( FAILED( hr = RestoreBuffers( bLooped ) ) )
        return hr;

    // Fill the entire buffer with wave data
    if( FAILED( hr = FillBuffer( bLooped ) ) )
        return hr;

    // Always play with the LOOPING flag since the streaming buffer
    // wraps around before the entire WAV is played
    if( FAILED( hr = g_pDSBuffer->Play( 0, 0, DSBPLAY_LOOPING ) ) )
        return hr;

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: FillBuffer()
// Desc: Fills the DirectSound buffer with wave data
//-----------------------------------------------------------------------------
HRESULT FillBuffer( BOOL bLooped )
{
    HRESULT hr;
    VOID*   pbBuffer = NULL;
    DWORD   dwBufferLength;

    if( NULL == g_pDSBuffer )
        return E_FAIL;

    g_bFoundEnd = FALSE;
    g_dwNextWriteOffset = 0; 
    g_dwProgress = 0;
    g_dwLastPos  = 0;

    // Reset the wav file to the beginning
    g_pWaveSoundRead->Reset();
    g_pDSBuffer->SetCurrentPosition( 0 );

    // Lock the buffer down, 
    if( FAILED( hr = g_pDSBuffer->Lock( 0, g_dwBufferSize, 
                                        &pbBuffer, &dwBufferLength, 
                                        NULL, NULL, 0L ) ) )
        return hr;

    // Fill the buffer with wav data 
    if( FAILED( hr = WriteToBuffer( bLooped, pbBuffer, dwBufferLength ) ) )
        return hr;

    // Now unlock the buffer
    g_pDSBuffer->Unlock( pbBuffer, dwBufferLength, NULL, 0 );

    g_dwNextWriteOffset = dwBufferLength; 
    g_dwNextWriteOffset %= g_dwBufferSize; // Circular buffer

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: WriteToBuffer()
// Desc: Writes wave data to the streaming DirectSound buffer 
//-----------------------------------------------------------------------------
HRESULT WriteToBuffer( BOOL bLooped, VOID* pbBuffer, DWORD dwBufferLength )
{
    HRESULT hr;
    UINT nActualBytesWritten;

    if( !g_bFoundEnd )
    {
        // Fill the DirectSound buffer with WAV data
        if( FAILED( hr = g_pWaveSoundRead->Read( dwBufferLength, 
                                                 (BYTE*) pbBuffer, 
                                                 &nActualBytesWritten ) ) )           
            return hr;
    }
    else
    {
        // Fill the DirectSound buffer with silence
        FillMemory( pbBuffer, dwBufferLength, 
                    (BYTE)( g_pWaveSoundRead->m_pwfx->wBitsPerSample == 8 ? 128 : 0 ) );
        nActualBytesWritten = dwBufferLength;
    }

    // If the number of bytes written is less than the 
    // amount we requested, we have a short file.
    if( nActualBytesWritten < dwBufferLength )
    {
        if( !bLooped ) 
        {
            g_bFoundEnd = TRUE;

            // Fill in silence for the rest of the buffer.
            FillMemory( (BYTE*) pbBuffer + nActualBytesWritten, 
                        dwBufferLength - nActualBytesWritten, 
                        (BYTE)(g_pWaveSoundRead->m_pwfx->wBitsPerSample == 8 ? 128 : 0 ) );
        }
        else
        {
            // We are looping.
            UINT nWritten = nActualBytesWritten;    // From previous call above.
            while( nWritten < dwBufferLength )
            {  
                // This will keep reading in until the buffer is full. For very short files.
                if( FAILED( hr = g_pWaveSoundRead->Reset() ) )
                    return hr;

                if( FAILED( hr = g_pWaveSoundRead->Read( (UINT)dwBufferLength - nWritten,
                                                          (BYTE*)pbBuffer + nWritten,
                                                          &nActualBytesWritten ) ) )
                    return hr;

                nWritten += nActualBytesWritten;
            } 
        } 
    }

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: StopBuffer()
// Desc: Stop the DirectSound buffer
//-----------------------------------------------------------------------------
HRESULT StopBuffer() 
{
    if( NULL != g_pDSBuffer )
    {
        g_pDSBuffer->Stop();    
    }

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: HandleNotification()
// Desc: Handle the notification that tell us to put more wav data in the 
//       circular buffer
//-----------------------------------------------------------------------------
HRESULT HandleNotification( BOOL bLooped ) 
{
    HRESULT hr;
    VOID* pbBuffer  = NULL;
    DWORD dwBufferLength;

    UpdateProgress();

    // Lock the buffer down
    if( FAILED( hr = g_pDSBuffer->Lock( g_dwNextWriteOffset, g_dwNotifySize, 
                                        &pbBuffer, &dwBufferLength, NULL, NULL, 0L ) ) )
        return hr;

    // Fill the buffer with wav data 
    if( FAILED( hr = WriteToBuffer( bLooped, pbBuffer, dwBufferLength ) ) )
        return hr;

    // Now unlock the buffer
    g_pDSBuffer->Unlock( pbBuffer, dwBufferLength, NULL, 0 );
    pbBuffer = NULL;

    // If the end was found, detrimine if there's more data to play 
    // and if not, stop playing
    if( g_bFoundEnd )
    {
        // We don't want to cut off the sound before it's done playing.
        // if doneplaying is set, the next notification event will post a stop message.
        if( g_pWaveSoundRead->m_ckInRiff.cksize > g_dwNotifySize )
        {
            if( g_dwProgress >= g_pWaveSoundRead->m_ckInRiff.cksize - g_dwNotifySize )
            {
                g_pDSBuffer->Stop();
            }
        }
        else // For short files.
        {
            if( g_dwProgress >= g_pWaveSoundRead->m_ckInRiff.cksize )
            {
                g_pDSBuffer->Stop();
            }
        }
    }

    g_dwNextWriteOffset += dwBufferLength; 
    g_dwNextWriteOffset %= g_dwBufferSize; // Circular buffer

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: UpdateProgress()
// Desc: Update the progress variable to know when the entire buffer was played
//-----------------------------------------------------------------------------
HRESULT UpdateProgress()
{
    HRESULT hr;
    DWORD   dwPlayPos;
    DWORD   dwWritePos;
    DWORD   dwPlayed;

    if( FAILED( hr = g_pDSBuffer->GetCurrentPosition( &dwPlayPos, &dwWritePos ) ) )
        return hr;

    if( dwPlayPos < g_dwLastPos )
        dwPlayed = g_dwBufferSize - g_dwLastPos + dwPlayPos;
    else
        dwPlayed = dwPlayPos - g_dwLastPos;

    g_dwProgress += dwPlayed;
    g_dwLastPos = dwPlayPos;

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: RestoreBuffers()
// Desc: Restore lost buffers and fill them up with sound if possible
//-----------------------------------------------------------------------------
HRESULT RestoreBuffers( BOOL bLooped )
{
    HRESULT hr;

    if( NULL == g_pDSBuffer )
        return S_OK;

    DWORD dwStatus;
    if( FAILED( hr = g_pDSBuffer->GetStatus( &dwStatus ) ) )
        return hr;

    if( dwStatus & DSBSTATUS_BUFFERLOST )
    {
        // Since the app could have just been activated, then
        // DirectSound may not be giving us control yet, so 
        // the restoring the buffer may fail.  
        // If it does, sleep until DirectSound gives us control.
        do 
        {
            hr = g_pDSBuffer->Restore();
            if( hr == DSERR_BUFFERLOST )
                Sleep( 10 );
        }
        while( hr = g_pDSBuffer->Restore() );

        if( FAILED( hr = FillBuffer( bLooped ) ) )
            return hr;
    }

    return S_OK;
}
