/*==========================================================================
 *  Copyright (C) 1997-1999 Microsoft Corporation.
 *
 *  File:       sound.cpp
 *  Content:    DirectSound management
 *
 ***************************************************************************/

#include <dmusicc.h>
#include <dmusici.h>
#include "sound.h"
#include "dsutil3d.h"
#include "helper.h"
#include "resource.h"

extern BOOL GetSearchPath(WCHAR wszPath[MAX_PATH]);

extern IDirectMusicPerformance* g_pPerf;
extern IDirectMusicPort* g_pSWSynthPort;

LPDIRECTSOUND           g_lpds;
LPDIRECTSOUND3DLISTENER g_lp3DListener;
LPDIRECTSOUNDBUFFER     g_lpdsbDrip;

_3DSOUND                My3DSound;


/* --------------------------------------------------------

   Drip()
   This function plays a 2-D stereo sound at random times with
   random pan, to show that ambient sounds do not have
   to be created with expensive 3-D buffers.

   -------------------------------------------------------- */

void Drip( void )
{
    DWORD dwStatus;
    static DWORD dwLastTime;

    if ( timeGetTime() < ( dwLastTime + 250 ) )
        return;

    g_lpdsbDrip->GetStatus( &dwStatus );
    if ( dwStatus & DSBSTATUS_PLAYING )
        return;

    if ( ( rand() % 2 ) == 0 ) 
    {
        g_lpdsbDrip->SetPan( ( rand() % ( DSBPAN_RIGHT * 2 ) ) - DSBPAN_RIGHT );
        g_lpdsbDrip->Play( 0, 0, 0 );
    }
    dwLastTime = timeGetTime();
}


/* --------------------------------------------------------

   MoveSound()
   Move the sound source on the x- and z-axes. The negative
   screen y-axis is the positive z-axis in our imaginary 
   3-D space.

   -------------------------------------------------------- */

void MoveSound( int x, int z )
{
    D3DVALUE sx, sz;
    HRESULT hr;

    // Convert pixel location to vector.
    sx = ( D3DVALUE )x - ( WINWD / 2 );
    sz = ( D3DVALUE )-1 * ( z - ( WINHT / 2 ) );

    hr = My3DSound.lpds3db->SetPosition( sx, 0.0, sz, DS3D_IMMEDIATE );
}  // MoveSound


/* --------------------------------------------------------

   TurnListener()
   Change listener orientation

   -------------------------------------------------------- */

void TurnListener( D3DVALUE lx, D3DVALUE ly, D3DVALUE lz )
{
    HRESULT hr = g_lp3DListener->SetOrientation(
                                lx, ly, lz, 
                                0.0, 1.0, 0.0,
                                DS3D_IMMEDIATE );
}  // TurnListener()




//-----------------------------------------------------------------------------
// Name: InitDirectSound()
// Desc: Creates the DirectSound object, primary buffer, and 3D listener
//-----------------------------------------------------------------------------
HRESULT InitDirectSound( HWND hWnd )
{
    DSBUFFERDESC        dsbd;
    LPDIRECTSOUNDBUFFER lpdsbPrimary;
    WAVEFORMATEX        wfm;

    if( FAILED( DirectSoundCreate( NULL, &g_lpds, NULL ) ) )
        return E_FAIL;

    // Set cooperative level.
    if( FAILED( g_lpds->SetCooperativeLevel( hWnd, DSSCL_PRIORITY ) ) )
        return E_FAIL;

    // Create primary buffer.
    ZeroMemory( &dsbd, sizeof( DSBUFFERDESC ) );
    dsbd.dwSize  = sizeof( DSBUFFERDESC );
    dsbd.dwFlags = DSBCAPS_CTRL3D | DSBCAPS_PRIMARYBUFFER;
    
    if( FAILED( g_lpds->CreateSoundBuffer( &dsbd, &lpdsbPrimary, NULL ) ) )
        return E_FAIL;

    // Set primary buffer format.
    ZeroMemory( &wfm, sizeof( WAVEFORMATEX ) ); 
    wfm.wFormatTag      = WAVE_FORMAT_PCM; 
    wfm.nChannels       = 2; 
    wfm.nSamplesPerSec  = 44100; 
    wfm.wBitsPerSample  = 16; 
    wfm.nBlockAlign     = wfm.wBitsPerSample / 8 * wfm.nChannels;
    wfm.nAvgBytesPerSec = wfm.nSamplesPerSec * wfm.nBlockAlign;

    lpdsbPrimary->SetFormat( &wfm ); 

    // Get listener interface.
    if( FAILED( lpdsbPrimary->QueryInterface( IID_IDirectSound3DListener,
                                              (VOID**)&g_lp3DListener ) ) )
    {
        lpdsbPrimary->Release();
        return E_FAIL;
    }
    lpdsbPrimary->Release(); lpdsbPrimary = NULL;

    return S_OK;
}


//-----------------------------------------------------------------------------
// Name: Init3DMusic()
// Desc: Initialize DirectSound and DirectMusic
//-----------------------------------------------------------------------------
BOOL Init3DMusic( HWND hWnd, HINSTANCE hinst )
{
    if( FAILED( InitDirectSound( hWnd ) ) )
    {
        MessageBox( hWnd, "Could not create DSound 3D Listener!",
                    "Initialization error", MB_ICONERROR | MB_OK );
        return FALSE;
    }

    // Create 2D buffer and load sound from resource.
    char szWaveFile[_MAX_PATH];
    WCHAR wszDir[_MAX_PATH] = L".";
    GetSearchPath( wszDir );
    wcstombs(szWaveFile, wszDir, MAX_PATH);
    strcat( szWaveFile, "\\drip.wav" );

    g_lpdsbDrip = DSLoadSoundBuffer( g_lpds, szWaveFile );
    if( !g_lpdsbDrip )
    {
        MessageBox( hWnd, "Could not load sound buffer!",
                    "Initialization error", MB_ICONERROR | MB_OK );
        return FALSE;
    }

    g_lpdsbDrip->SetVolume( DSBVOLUME_MAX );

    if( FAILED( InitDirectMusic( hWnd ) ) )
    {
        // Error msg was displayed in InitDirectMusic() function
        return FALSE;
    }

    // Create 3D buffer, load sound from resource,
    // and get IDirectSound3DBuffer interface.
    My3DSound.lpdsb = DMLoad3DSoundBuffer();

    if ( My3DSound.lpdsb ) 
    {
        My3DSound.lpdsb->QueryInterface( IID_IDirectSound3DBuffer,
            ( LPVOID * )&My3DSound.lpds3db );
    }
    
    if ( ( !My3DSound.lpdsb ) | ( !My3DSound.lpds3db ) )
        return FALSE;

    // Set minimum distance. This is the distance over which
    // the volume is halved, so we don't want it too small.
    My3DSound.lpds3db->SetMinDistance( 40.0, DS3D_IMMEDIATE );

    // Load and play music into the 3D buffer
    IDirectMusicSegment* pMIDISeg = LoadSegment( "canyon.mid" );

    if( !pMIDISeg )
    {
        MessageBox( hWnd, "Could not load MIDI segment!",
                    "Initialization error", MB_ICONERROR | MB_OK );
        return FALSE;
    }

    pMIDISeg->SetRepeats((DWORD)-1);

    if ( SUCCEEDED( g_pSWSynthPort->Activate( TRUE ) ) )
    {
        if ( SUCCEEDED( g_pPerf->PlaySegment( pMIDISeg, 0, 0, NULL ) ) )
        {
            pMIDISeg->Release(); pMIDISeg = NULL;
            return TRUE;
        }
    }

    pMIDISeg->Release(); pMIDISeg = NULL;

    MessageBox( hWnd, "Could not play MIDI segment!",
                "Initialization error", MB_ICONERROR | MB_OK );

    return FALSE;
}




/* --------------------------------------------------------

   Cleanup3DMusic()
   Cleans up DirectSound and DirectMusic objects
   -------------------------------------------------------- */

void Cleanup3DMusic( void )
{
    FreeDirectMusic();
    if ( g_lp3DListener ) {g_lp3DListener->Release(); g_lp3DListener = NULL;}

    g_lpdsbDrip = NULL;
    My3DSound.lpdsb = NULL;
    My3DSound.lpds3db = NULL;

    if ( g_lpds ) {g_lpds->Release();}  // This releases buffers as well.
}
