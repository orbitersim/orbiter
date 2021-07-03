//-----------------------------------------------------------------------------
// File: Play3DSound.cpp
//
// Desc: DirectSound support for how to load a wave file and play it using a 
//       3D DirectSound buffer.
//
// Copyright (c) 1999 Microsoft Corp. All rights reserved.
//-----------------------------------------------------------------------------
#define STRICT
#include <objbase.h>
#include <initguid.h>
#include <commctrl.h>
#include <commdlg.h>
#include <mmreg.h>
#include <dsound.h>
#include "resource.h"
#include "WavRead.h"




//-----------------------------------------------------------------------------
// Function-prototypes
//-----------------------------------------------------------------------------
extern VOID OnEnablePlayUI( HWND hDlg, BOOL bEnable );
extern VOID SetStatusUI( HWND hDlg, TCHAR* strStatus );
extern VOID SetFileUI( HWND hDlg, TCHAR* strFileName );
extern VOID SetSlidersPos( LONG lDopplerSlider, LONG lRolloffSlider,
                           LONG lMinDistSlider, LONG lMaxDistSlider );

BOOL CALLBACK AlgorithmDlgProc( HWND hDlg, UINT msg, WPARAM wParam, LPARAM lParam );
VOID    LoadWaveFile( HWND hDlg, TCHAR* strFileName );
HRESULT CreateStaticBuffer( HWND hDlg, TCHAR* strFileName );
HRESULT FillBuffer();
HRESULT RestoreBuffers();




//-----------------------------------------------------------------------------
// Defines, constants, and global variables
//-----------------------------------------------------------------------------
#define SAFE_DELETE(p)  { if(p) { delete (p);     (p)=NULL; } }
#define SAFE_RELEASE(p) { if(p) { (p)->Release(); (p)=NULL; } }

LPDIRECTSOUND           g_pDS             = NULL;   // DirectSound object
LPDIRECTSOUNDBUFFER     g_pDSBuffer       = NULL;   // Secondary sound buffer
LPDIRECTSOUND3DBUFFER   g_pDS3DBuffer     = NULL;   // 3D sound buffer
LPDIRECTSOUND3DLISTENER g_pDSListener     = NULL;   // 3D listener object

DS3DBUFFER              g_dsBufferParams;           // 3D buffer properties
DS3DLISTENER            g_dsListenerParams;         // Listener properties
DWORD                   g_dwBufferBytes;
CWaveSoundRead*         g_pWaveSoundRead  = NULL;
extern BOOL             g_bDeferSettings;




//-----------------------------------------------------------------------------
// Name: InitDirectSound()
// Desc: Initilizes DirectSound
//-----------------------------------------------------------------------------
HRESULT InitDirectSound( HWND hDlg )
{
    HRESULT             hr;
    DSBUFFERDESC        dsbdesc;
    LPDIRECTSOUNDBUFFER pDSBPrimary;

    // Initialize COM
    if( hr = CoInitialize( NULL ) )
        return hr;

    // Create IDirectSound using the primary sound device
    if( FAILED( hr = DirectSoundCreate( NULL, &g_pDS, NULL ) ) )
        return hr;

    // Set coop level to DSSCL_PRIORITY
    if( FAILED( hr = g_pDS->SetCooperativeLevel( hDlg, DSSCL_PRIORITY ) ) )
        return hr;

    // Obtain primary buffer, asking it for 3D control
    ZeroMemory( &dsbdesc, sizeof(DSBUFFERDESC) );
    dsbdesc.dwSize = sizeof(DSBUFFERDESC);
    dsbdesc.dwFlags = DSBCAPS_CTRL3D | DSBCAPS_PRIMARYBUFFER;
    if( FAILED( hr = g_pDS->CreateSoundBuffer( &dsbdesc, &pDSBPrimary, NULL ) ) )
        return hr;

    if( FAILED( hr = pDSBPrimary->QueryInterface( IID_IDirectSound3DListener, 
                                                  (VOID**)&g_pDSListener ) ) )
        return hr;

    // Get listener parameters
    g_dsListenerParams.dwSize = sizeof(DS3DLISTENER);
    g_pDSListener->GetAllParameters( &g_dsListenerParams );

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

    // Release the primary buffer, since it is not need anymore
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
    SAFE_RELEASE( g_pDSListener );
    SAFE_RELEASE( g_pDS3DBuffer );
    SAFE_RELEASE( g_pDSBuffer );
    SAFE_RELEASE( g_pDS ); 

    // Release COM
    CoUninitialize();

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: SetParameters()
// Desc: 
//-----------------------------------------------------------------------------
VOID SetParameters( FLOAT fDopplerFactor, FLOAT fRolloffFactor,
                    FLOAT fMinDistance,   FLOAT fMaxDistance )
{
    // Every change to 3-D sound buffer and listener settings causes 
    // DirectSound to remix, at the expense of CPU cycles. 
    // To minimize the performance impact of changing 3-D settings, 
    // use the DS3D_DEFERRED flag in the dwApply parameter of any of 
    // the IDirectSound3DListener or IDirectSound3DBuffer methods that 
    // change 3-D settings. Then call the IDirectSound3DListener::CommitDeferredSettings 
    // method to execute all of the deferred commands at once.
    DWORD dwApplyFlag = ( g_bDeferSettings ) ? DS3D_DEFERRED : DS3D_IMMEDIATE;

    g_dsListenerParams.flDopplerFactor = fDopplerFactor;
    g_dsListenerParams.flRolloffFactor = fRolloffFactor;

    if( g_pDSListener )
        g_pDSListener->SetAllParameters( &g_dsListenerParams, dwApplyFlag );

    g_dsBufferParams.flMinDistance = fMinDistance;
    g_dsBufferParams.flMaxDistance = fMaxDistance;

    if( g_pDS3DBuffer )
        g_pDS3DBuffer->SetAllParameters( &g_dsBufferParams, dwApplyFlag );
}




//-----------------------------------------------------------------------------
// Name: SetObjectProperties()
// Desc: Sets the position and velocity on the 3D buffer
//-----------------------------------------------------------------------------
VOID SetObjectProperties( D3DVECTOR* pvPosition, D3DVECTOR* pvVelocity )
{
    // Every change to 3-D sound buffer and listener settings causes 
    // DirectSound to remix, at the expense of CPU cycles. 
    // To minimize the performance impact of changing 3-D settings, 
    // use the DS3D_DEFERRED flag in the dwApply parameter of any of 
    // the IDirectSound3DListener or IDirectSound3DBuffer methods that 
    // change 3-D settings. Then call the IDirectSound3DListener::CommitDeferredSettings 
    // method to execute all of the deferred commands at once.
    DWORD dwApplyFlag = ( g_bDeferSettings ) ? DS3D_DEFERRED : DS3D_IMMEDIATE;

    memcpy( &g_dsBufferParams.vPosition, pvPosition, sizeof(D3DVECTOR) );
    memcpy( &g_dsBufferParams.vVelocity, pvVelocity, sizeof(D3DVECTOR) );

    if( g_pDS3DBuffer )
        g_pDS3DBuffer->SetAllParameters( &g_dsBufferParams, dwApplyFlag );
}




//-----------------------------------------------------------------------------
// Name: LoadWaveFile()
// Desc: Loads the wave file into a secondary static DirectSound buffer
//-----------------------------------------------------------------------------
VOID LoadWaveFile( HWND hDlg, TCHAR* strFileName )
{
    HRESULT hr;

    // Create the sound buffer object from the wave file data
    if( FAILED( hr = CreateStaticBuffer( hDlg, strFileName ) ) )
    {        
        SetStatusUI( hDlg, TEXT("Couldn't create sound buffer.") );
        SetFileUI( hDlg, TEXT("") );
    }

    // If the sound buffer was successfully created, then fill it
    if( hr == S_OK )
    {

        // Fill the buffer with wav data
        FillBuffer();

        // Update the UI controls to show the sound as the file is loaded
        SetFileUI( hDlg, strFileName );
        OnEnablePlayUI( hDlg, TRUE );

        DSBCAPS dsbcaps;
        ZeroMemory( &dsbcaps, sizeof(DSBCAPS) );
        dsbcaps.dwSize = sizeof(DSBCAPS);

        g_pDSBuffer->GetCaps( &dsbcaps );
        if( ( dsbcaps.dwFlags & DSBCAPS_LOCHARDWARE ) != 0 )
        {       
            SetStatusUI( hDlg, TEXT("File loaded using hardware mixing.") );
        }
        else
        {
            SetStatusUI( hDlg, TEXT("File loaded using software mixing.") );
        }
    }
}




//-----------------------------------------------------------------------------
// Name: CreateStaticBuffer()
// Desc: Creates a wave file, and sound buffer 
//-----------------------------------------------------------------------------
HRESULT CreateStaticBuffer( HWND hDlg, TCHAR* strFileName )
{
    HRESULT hr; 
    int nResult;

    // Free any previous globals 
    SAFE_DELETE( g_pWaveSoundRead );
    SAFE_RELEASE( g_pDSBuffer );

    // Create a new wave file class
    g_pWaveSoundRead = new CWaveSoundRead();

    // Load the wave file
    if( FAILED( hr = g_pWaveSoundRead->Open( strFileName ) ) )
    {
        SetStatusUI( hDlg, TEXT("Bad wave file.") );
        SetFileUI( hDlg, TEXT("") );
        return S_FALSE;
    }

    // Set up the direct sound buffer, only requesting the flags we need, 
    // since each require overhead.
    DSBUFFERDESC dsbd;
    ZeroMemory( &dsbd, sizeof(DSBUFFERDESC) );
    dsbd.dwSize        = sizeof(DSBUFFERDESC);
    dsbd.dwFlags       = DSBCAPS_CTRL3D | DSBCAPS_STATIC;
    dsbd.dwBufferBytes = g_pWaveSoundRead->m_ckIn.cksize;
    dsbd.lpwfxFormat   = g_pWaveSoundRead->m_pwfx;
    
    // Set the software DirectSound3D emulation algorithm to use
    // so display the algorith dialog box.
    nResult = DialogBox( NULL, MAKEINTRESOURCE(IDD_3D_ALGORITHM), 
                         NULL, AlgorithmDlgProc );
    switch( nResult )
    {
    case -1: // User canceled dialog box
        SetStatusUI( hDlg, TEXT("Load aborted.") );
        SetFileUI( hDlg, TEXT("") );
        return S_FALSE;

    case 0: // User selected DS3DALG_NO_VIRTUALIZATION  
        dsbd.guid3DAlgorithm = DS3DALG_NO_VIRTUALIZATION;
        break;

    case 1: // User selected DS3DALG_HRTF_FULL  
        dsbd.guid3DAlgorithm = DS3DALG_HRTF_FULL;
        break;

    case 2: // User selected DS3DALG_HRTF_LIGHT
        dsbd.guid3DAlgorithm = DS3DALG_HRTF_LIGHT;
        break;
    }

    // Create the static DirectSound buffer 
    if( FAILED( hr = g_pDS->CreateSoundBuffer( &dsbd, &g_pDSBuffer, NULL ) ) )
        return hr;

    if( DS_NO_VIRTUALIZATION == hr )
    {
        MessageBox( hDlg, "The 3D virtualization algorithm requested is not supported under this "
                    "operating system.  It is available only on Win2000, and Win98 SE with WDM "
                    "drivers and beyond.  Creating buffer with no virtualization.", 
                    "DirectSound Sample", MB_OK );
    }

    // Get the 3D buffer from the secondary buffer
    if( FAILED( hr = g_pDSBuffer->QueryInterface( IID_IDirectSound3DBuffer, 
                                                  (VOID**)&g_pDS3DBuffer ) ) )
        return hr;

    g_dwBufferBytes = dsbd.dwBufferBytes;

    // Get the 3D buffer parameters
    g_dsBufferParams.dwSize = sizeof(DS3DBUFFER);
    g_pDS3DBuffer->GetAllParameters( &g_dsBufferParams );

    // Set new 3D buffer parameters
    g_dsBufferParams.dwMode = DS3DMODE_HEADRELATIVE;
    g_pDS3DBuffer->SetAllParameters( &g_dsBufferParams, DS3D_IMMEDIATE );

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
HRESULT PlayBuffer( BOOL bLooped )
{
    HRESULT hr;
    DWORD   dwStatus;

    if( NULL == g_pDSBuffer )
        return E_FAIL;

    // Restore the buffers if they are lost
    if( FAILED( hr = RestoreBuffers() ) )
        return hr;

    if( FAILED( hr = g_pDSBuffer->GetStatus( &dwStatus ) ) )
        return hr;

    if( dwStatus & DSBSTATUS_PLAYING )
    {
        // Don't bother playing, just restart
        g_pDSBuffer->SetCurrentPosition( 0 );
    }
    else
    {
        // Play buffer 
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




//-----------------------------------------------------------------------------
// Name: AlgorithmDlgProc()
// Desc: Handles dialog messages
//-----------------------------------------------------------------------------
BOOL CALLBACK AlgorithmDlgProc( HWND hDlg, UINT msg, WPARAM wParam, LPARAM lParam )
{
    // Default is DS3DALG_NO_VIRTUALIZATION for fastest performance
    static int nDefaultRadio = IDC_NO_VIRT_RADIO;

    switch( msg ) 
    {
        case WM_INITDIALOG:
            // Default is DS3DALG_NO_VIRTUALIZATION for fastest performance
            CheckRadioButton( hDlg, IDC_NO_VIRT_RADIO, IDC_LIGHT_VIRT_RADIO, nDefaultRadio );
            return TRUE; // Message handled 

        case WM_COMMAND:
            switch( LOWORD(wParam) )
            {
                case IDCANCEL:
                    EndDialog( hDlg, -1 );
                    return TRUE; // Message handled 

                case IDOK:
                    if( IsDlgButtonChecked( hDlg, IDC_NO_VIRT_RADIO )    == BST_CHECKED )
                    {
                        nDefaultRadio = IDC_NO_VIRT_RADIO;
                        EndDialog( hDlg, 0 );
                    }

                    if( IsDlgButtonChecked( hDlg, IDC_HIGH_VIRT_RADIO )  == BST_CHECKED )
                    {               
                        nDefaultRadio = IDC_HIGH_VIRT_RADIO;
                        EndDialog( hDlg, 1 );
                    }

                    if( IsDlgButtonChecked( hDlg, IDC_LIGHT_VIRT_RADIO ) == BST_CHECKED )
                    {               
                        nDefaultRadio = IDC_LIGHT_VIRT_RADIO;
                        EndDialog( hDlg, 2 );
                    }
                        
                    return TRUE; // Message handled 
            }
            break;
    }

    return FALSE; // Message not handled 
}

