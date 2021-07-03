//-----------------------------------------------------------------------------
// File: VoiceManagement.cpp
//
// Desc: DirectSound support file for sample showing how to load a wave file 
//       and play it using a system RAM DirectSound buffer with
//       voice management flags
//
// Copyright (c) 1999 Microsoft Corp. All rights reserved.
//-----------------------------------------------------------------------------
#define STRICT
#include <objbase.h>
#include <initguid.h>
#include <commdlg.h>
#include <mmreg.h>
#include <dsound.h>
#include "resource.h"
#include "WavRead.h"




//-----------------------------------------------------------------------------
// Function-prototypes
//-----------------------------------------------------------------------------
extern VOID EnablePlayUI( HWND hDlg, BOOL bShowPlayControl );
extern VOID SetFileUI( HWND hDlg, TCHAR* strFileName );

VOID LoadWaveFile( HWND hDlg, TCHAR* strFileName );
HRESULT CreateSystemRAMBuffer( HWND hDlg, TCHAR* strFileName );
HRESULT FillBuffer();
HRESULT RestoreBuffers();




//-----------------------------------------------------------------------------
// Defines, constants, and global variables
//-----------------------------------------------------------------------------
#define SAFE_DELETE(p)  { if(p) { delete (p);     (p)=NULL; } }
#define SAFE_RELEASE(p) { if(p) { (p)->Release(); (p)=NULL; } }

LPDIRECTSOUND       g_pDS            = NULL;
LPDIRECTSOUNDBUFFER g_pDSBuffer      = NULL;
LPDIRECTSOUNDNOTIFY g_pDSNotify      = NULL;
CWaveSoundRead*     g_pWaveSoundRead = NULL;
DWORD               g_dwBufferBytes;



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
    DSBUFFERDESC dsbd;
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
    SAFE_DELETE( g_pWaveSoundRead );

    // Release DirectSound interfaces
    SAFE_RELEASE( g_pDSBuffer );
    SAFE_RELEASE( g_pDS ); 

    // Release COM
    CoUninitialize();

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: LoadWaveFile()
// Desc: Loads the wave file into a secondary system RAM DirectSound buffer
//-----------------------------------------------------------------------------
VOID LoadWaveFile( HWND hDlg, TCHAR* strFileName )
{
    // Create the sound buffer object from the wave file data
    if( FAILED( CreateSystemRAMBuffer( hDlg, strFileName ) ) )
    {        
        SetFileUI( hDlg, TEXT("Couldn't create sound buffer.") ); 
    }
    else // The sound buffer was successfully created
    {
        // Fill the buffer with wav data
        FillBuffer();

        // Update the UI controls to show the sound as the file is loaded
        SetFileUI( hDlg, strFileName );
        EnablePlayUI( hDlg, TRUE );
    }
}




//-----------------------------------------------------------------------------
// Name: CreateSystemRAMBuffer()
// Desc: Creates a wave file, sound buffer and notification events 
//-----------------------------------------------------------------------------
HRESULT CreateSystemRAMBuffer( HWND hDlg, TCHAR* strFileName )
{
    HRESULT hr; 

    // Free any previous globals 
    SAFE_DELETE( g_pWaveSoundRead );
    SAFE_RELEASE( g_pDSBuffer );

    // Create a new wave file class
    g_pWaveSoundRead = new CWaveSoundRead();

    // Load the wave file
    if( FAILED( g_pWaveSoundRead->Open( strFileName ) ) )
    {
        SetFileUI( hDlg, TEXT("Bad wave file.") );
    }

    // Set up the direct sound buffer
    // DSBCAPS_LOCDEFER must be specificed since this allows the 
    // buffer to at Play() time to go into either a software 
    // or hardware buffer. 
    //
    // Not specifing the DSBCAPS_STATIC flag will put the buffer in 
    // system RAM instead of sound card RAM.  This will typically 
    // be more efficent for voice management buffers since newer 
    // generation PCI audio cards all use system memory for their 
    // hardware buffers, and by creating our buffers in system RAM 
    // buffers, we're guarenteed the highest performance using the 
    // DSBCAPS_LOCDEFER flag.  Because of the download time, we 
    // don't want to use DEFER'd buffers on older ISA sound cards 
    // with their own memory
    DSBUFFERDESC dsbd;
    ZeroMemory( &dsbd, sizeof(DSBUFFERDESC) );
    dsbd.dwSize        = sizeof(DSBUFFERDESC);
    dsbd.dwFlags       = DSBCAPS_LOCDEFER;
    dsbd.dwBufferBytes = g_pWaveSoundRead->m_ckIn.cksize;
    dsbd.lpwfxFormat   = g_pWaveSoundRead->m_pwfx;

    // Create the static DirectSound buffer 
    if( FAILED( hr = g_pDS->CreateSoundBuffer( &dsbd, &g_pDSBuffer, NULL ) ) )
        return hr;

    // Remember how big the buffer is
    g_dwBufferBytes = dsbd.dwBufferBytes;

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: FillBuffer()
// Desc: Fill the DirectSound buffer with data from the wav file
//-----------------------------------------------------------------------------
HRESULT FillBuffer()
{
    HRESULT hr; 
    BYTE*   pbWavData; // Pointer to actual wav data 
    UINT    cbWavSize; // Size of data
    VOID*   pbData  = NULL;
    VOID*   pbData2 = NULL;
    DWORD   dwLength;
    DWORD   dwLength2;

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

    // Reset the file to the beginning 
    g_pWaveSoundRead->Reset();

    // Lock the buffer down
    if( FAILED( hr = g_pDSBuffer->Lock( 0, g_dwBufferBytes, &pbData, &dwLength, 
                                   &pbData2, &dwLength2, 0L ) ) )
        return hr;

    // Copy the memory to it.
    memcpy( pbData, pbWavData, g_dwBufferBytes );

    // Unlock the buffer, we don't need it anymore.
    g_pDSBuffer->Unlock( pbData, g_dwBufferBytes, NULL, 0 );
    pbData = NULL;

    // We dont need the wav file data buffer anymore, so delete it 
    SAFE_DELETE( pbWavData );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: PlayBuffer()
// Desc: User hit the "Play" button, so play the DirectSound buffer
//-----------------------------------------------------------------------------
BOOL IsBufferPlaying() 
{
    DWORD dwStatus = 0;

    if( NULL == g_pDSBuffer )
        return E_FAIL;

    g_pDSBuffer->GetStatus( &dwStatus );

    if( dwStatus & DSBSTATUS_PLAYING )
        return TRUE;
    else 
        return FALSE;
}




//-----------------------------------------------------------------------------
// Name: PlayBuffer()
// Desc: User hit the "Play" button, so play the DirectSound buffer
//-----------------------------------------------------------------------------
HRESULT PlayBuffer( DWORD dwPriority, DWORD dwPlayFlags )
{
    HRESULT hr;

    if( NULL == g_pDSBuffer )
        return E_FAIL;

    // Restore the buffers if they are lost
    if( FAILED( hr = RestoreBuffers() ) )
        return hr;

    // Play buffer 
    if( FAILED( hr = g_pDSBuffer->Play( 0, dwPriority, dwPlayFlags ) ) )
        return hr;

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: StopBuffer()
// Desc: Stop the DirectSound buffer from playing 
//-----------------------------------------------------------------------------
VOID StopBuffer( BOOL bResetPosition ) 
{
    if( NULL == g_pDSBuffer )
        return;

    g_pDSBuffer->Stop();

    if( bResetPosition )
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




//-----------------------------------------------------------------------------
// Name: RestoreBuffers()
// Desc: Restore lost buffers and fill them up with sound if possible
//-----------------------------------------------------------------------------
HRESULT RestoreBuffers()
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

        if( FAILED( hr = FillBuffer() ) )
            return hr;
    }

    return S_OK;
}

