//-----------------------------------------------------------------------------
// File: AdjustSound.cpp
//
// Desc: DirectSound support for how to load a wave file and play it using a 
//       static DirectSound buffer and adjust its focus, frequency,
//       pan, and volume.
//
// Copyright (c) 1999 Microsoft Corp. All rights reserved.
//-----------------------------------------------------------------------------
#define STRICT
#include <objbase.h>
#include <initguid.h>
#include <mmsystem.h>
#include <mmreg.h>
#include <dsound.h>
#include <commdlg.h>
#include "resource.h"
#include "WavRead.h"




//-----------------------------------------------------------------------------
// Function-prototypes
//-----------------------------------------------------------------------------
extern VOID OnEnablePlayUI( HWND hDlg, BOOL bEnable );
extern VOID SetStatusUI( HWND hDlg, TCHAR* strStatus );
extern VOID SetFileUI( HWND hDlg, TCHAR* strFileName );
extern VOID SetSlidersPos( HWND hDlg, LONG lFreqSlider, LONG lPanSlider, LONG lVolumeSlider );
extern VOID OnSliderChanged( HWND hDlg );
HRESULT RestoreBuffer();




//-----------------------------------------------------------------------------
// Defines, constants, and global variables
//-----------------------------------------------------------------------------
#define SAFE_DELETE(p)  { if(p) { delete (p);     (p)=NULL; } }
#define SAFE_RELEASE(p) { if(p) { (p)->Release(); (p)=NULL; } }

LPDIRECTSOUND       g_pDS       = NULL;
LPDIRECTSOUNDBUFFER g_pDSBuffer = NULL;
CWaveSoundRead*     g_pWaveSoundRead = NULL;




//-----------------------------------------------------------------------------
// Name: InitDirectSound()
// Desc: Initilizes DirectSound
//-----------------------------------------------------------------------------
HRESULT InitDirectSound( HWND hDlg )
{
    HRESULT             hr;
    LPDIRECTSOUNDBUFFER pDSBPrimary = NULL;

    // Initialize COM
    if( hr = CoInitialize( NULL ) )
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
    // Release DirectSound interfaces
    SAFE_DELETE( g_pWaveSoundRead );
    SAFE_RELEASE( g_pDSBuffer );
    SAFE_RELEASE( g_pDS ); 

    // Release COM
    CoUninitialize();

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: SetBufferOptions()
// Desc: Sets the DirectSound buffer options 
//-----------------------------------------------------------------------------
VOID SetBufferOptions( LONG lFrequency, LONG lPan, LONG lVolume )
{
    if( g_pDSBuffer )
    {
        g_pDSBuffer->SetFrequency( lFrequency );
        g_pDSBuffer->SetPan( lPan );
        g_pDSBuffer->SetVolume( lVolume );
    }
}




//-----------------------------------------------------------------------------
// Name: LoadWaveFile()
// Desc: Loads a wave file into a secondary static DirectSound buffer
//-----------------------------------------------------------------------------
VOID LoadWaveFile( HWND hDlg, TCHAR* strFileName )
{
    SAFE_DELETE( g_pWaveSoundRead );
    g_pWaveSoundRead = new CWaveSoundRead();

    // Load the wave file
    if( FAILED( g_pWaveSoundRead->Open( strFileName ) ) )
    {
        SetStatusUI( hDlg, TEXT("Bad wave file.") );
    }
    else // The load call succeeded
    {
        // Update the UI controls to show the sound as the file is loaded
        OnEnablePlayUI( hDlg, TRUE );
        SetFileUI( hDlg, strFileName );
        SetStatusUI( hDlg, TEXT("File loaded.") );

        // Get the samples per sec from the wave file
        INT nSamplesPerSec = g_pWaveSoundRead->m_pwfx->nSamplesPerSec;

        // Set the slider positions
        SetSlidersPos( hDlg, nSamplesPerSec, 0, 0 );
    }
}




//-----------------------------------------------------------------------------
// Name: CreateStaticBuffer()
// Desc: Uses data from a CWaveSoundRead class to create the sound buffer 
//-----------------------------------------------------------------------------
HRESULT CreateStaticBuffer( HWND hDlg, DWORD dwCreatationFlags )
{
    HRESULT hr; 
    BYTE*   pbWavData; // Pointer to actual wav data 
    UINT    cbWavSize; // Size of data

    // Reset the wave file to the beginning 
    g_pWaveSoundRead->Reset();

    // The size of wave data is in pWaveFileSound->m_ckIn
    INT nWaveFileSize = g_pWaveSoundRead->m_ckIn.cksize;

    // Allocate that buffer.
    pbWavData = new BYTE[ nWaveFileSize ];

    if( NULL == pbWavData )
        return E_OUTOFMEMORY;

    if( FAILED( hr = g_pWaveSoundRead->Read( nWaveFileSize, 
                                             pbWavData, 
                                             &cbWavSize ) ) )           
        return hr;

    // Set up the direct sound buffer, and only request the flags needed
    // since each requires some overhead and limits if the buffer can 
    // be hardware accelerated
    DSBUFFERDESC dsbd;
    ZeroMemory( &dsbd, sizeof(DSBUFFERDESC) );
    dsbd.dwSize        = sizeof(DSBUFFERDESC);
    dsbd.dwFlags       = DSBCAPS_CTRLPAN       | DSBCAPS_CTRLVOLUME |
                         DSBCAPS_CTRLFREQUENCY | DSBCAPS_STATIC;
    dsbd.dwBufferBytes = cbWavSize;
    dsbd.lpwfxFormat   = g_pWaveSoundRead->m_pwfx;

    // Add in the passed in creation flags 
    dsbd.dwFlags       |= dwCreatationFlags;

    // Create the static DirectSound buffer using the focus set by the UI
    if( FAILED( hr = g_pDS->CreateSoundBuffer( &dsbd, &g_pDSBuffer, NULL ) ) )
        return hr;

    VOID* pbData  = NULL;
    VOID* pbData2 = NULL;
    DWORD dwLength;
    DWORD dwLength2;

    // Make sure we have focus, and we didn't just switch in from
    // an app which had a DirectSound device
    if( FAILED( hr = RestoreBuffer() ) ) 
        return hr;

    // Lock the buffer down
    if( FAILED( hr = g_pDSBuffer->Lock( 0, dsbd.dwBufferBytes, &pbData, &dwLength, 
                                        &pbData2, &dwLength2, 0L ) ) )
        return hr;

    // Copy the memory to it.
    memcpy( pbData, pbWavData, dsbd.dwBufferBytes );

    // Unlock the buffer, we don't need it anymore.
    g_pDSBuffer->Unlock( pbData, dsbd.dwBufferBytes, NULL, 0 );
    pbData = NULL;

    // We dont need the wav file data buffer anymore, so delete it 
    SAFE_DELETE( pbWavData );

    // Set the buffer options to what the sliders are set to
    OnSliderChanged( hDlg );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: RestoreBuffer()
// Desc: Restore lost buffer and fill them up with sound if possible
//-----------------------------------------------------------------------------
HRESULT RestoreBuffer()
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
    }

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: PlayBuffer()
// Desc: User hit the "Play" button, so play the DirectSound buffer
//-----------------------------------------------------------------------------
HRESULT PlayBuffer( HWND hDlg, BOOL bLooped, DWORD dwCreationFlags )
{
    HRESULT hr;
    DWORD dwStatus;

    // Since the user can change the focus before the sound is played, we
    // need to create the sound buffer every time the play button is pressed 

    // Free any previous g_pDSBuffer buffer
    SAFE_RELEASE( g_pDSBuffer );

    // Create the sound buffer object from the wave file data
    if( FAILED( hr = CreateStaticBuffer( hDlg, dwCreationFlags ) ) )
    {
        // Not a critical failure, so just update the status
        SetStatusUI( hDlg, TEXT("Couldn't create sound buffer.") );
        return S_FALSE; 
    }

    if( NULL == g_pDSBuffer )
        return E_FAIL;

    // Now that the buffer is created, play the buffer
    if( FAILED( hr = g_pDSBuffer->GetStatus( &dwStatus ) ) )
        return hr;

    if( dwStatus & DSBSTATUS_PLAYING )
    {
        // Don't bother playing, just restart
        g_pDSBuffer->SetCurrentPosition( 0 );
    }
    else
    {
        DWORD dwLooped = bLooped ? DSBPLAY_LOOPING : 0L;

        if( FAILED( hr = g_pDSBuffer->Play( 0, 0, dwLooped ) ) )
            return hr;
    }

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: StopBuffer()
// Desc: Stop the DirectSound buffer from playing 
//-----------------------------------------------------------------------------
VOID StopBuffer() 
{
    if( NULL == g_pDSBuffer )
        return;

    g_pDSBuffer->Stop();
    g_pDSBuffer->SetCurrentPosition( 0L );    
}




//-----------------------------------------------------------------------------
// Name: IsSoundPlaying()
// Desc: Checks to see if a sound is playing and returns TRUE if it is.
//-----------------------------------------------------------------------------
BOOL IsSoundPlaying()
{
    if( g_pDSBuffer )
    {  
        DWORD dwStatus = 0;
        g_pDSBuffer->GetStatus( &dwStatus );
        return( ( dwStatus & DSBSTATUS_PLAYING ) != 0 );
    }
    else
    {
        return FALSE;
    }
}

