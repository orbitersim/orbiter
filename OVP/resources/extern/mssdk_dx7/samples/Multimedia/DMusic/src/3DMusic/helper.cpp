//-----------------------------------------------------------------------------
// File: Helper.cpp
//
// Desc: DirectMusic Helper Functions
//
//
// Copyright (c) 1998-1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------
#define STRICT
#include <windows.h>
#include <windowsx.h>
#include <mmsystem.h>
#include <direct.h>
#include <dmusicc.h>
#include <dmusici.h>
#include <dsound.h>
#include "helper.h"




//-----------------------------------------------------------------------------
// Global variables for the DirectMusic sample 
//-----------------------------------------------------------------------------
extern LPDIRECTSOUND     g_lpds;
IDirectMusic*            g_pDMusic       = NULL;
IDirectMusicPerformance* g_pPerf         = NULL;
IDirectMusicPort*        g_pSWSynthPort  = NULL;
IDirectMusicLoader*      g_pLoader       = NULL;
static char              szDirectMusicMedia[] = "\\DMusic\\Media";




//-----------------------------------------------------------------------------
// Name: GetSearchPath()
// Desc: Get registry search path
//-----------------------------------------------------------------------------
BOOL GetSearchPath( WCHAR wszPath[MAX_PATH] )
{
    HKEY  hkDirectX;
    BOOL  bRet = FALSE;
    char  szPath[MAX_PATH];
    DWORD cbPath;

    // Get DirectX SDK search path from the registry
    if( RegOpenKeyEx( HKEY_LOCAL_MACHINE, "Software\\Microsoft\\DirectX",
                      0, KEY_READ, &hkDirectX ) )
    {
        return FALSE;
    }

    cbPath = sizeof(szPath);
    if( RegQueryValueEx( hkDirectX, "DXSDK Samples Path", NULL, NULL, 
                         (BYTE*)szPath, &cbPath ) == ERROR_SUCCESS )
    {
        if( cbPath + sizeof(szDirectMusicMedia) > MAX_PATH )
        {
            return FALSE;
        }

        strcat( szPath, szDirectMusicMedia );

        // DirectMusic requires the search path as a wide string
        mbstowcs( wszPath, szPath, MAX_PATH );
        bRet = TRUE;
    }

    RegCloseKey( hkDirectX );
    return bRet;
}




//-----------------------------------------------------------------------------
// Name: DMLoad3DSoundBuffer()
// Desc: Create a DirectSound 3D buffer and pass it to DirectMusic
//-----------------------------------------------------------------------------
IDirectSoundBuffer* DMLoad3DSoundBuffer()
{
    IDirectSoundBuffer* pDSB = NULL;
    DSBUFFERDESC        dsBD = {0};
    HRESULT             hr   = E_FAIL;

    if( !g_lpds || !g_pSWSynthPort )
    {
        return NULL;
    }

    WAVEFORMATEX _WaveFormat;
    ZeroMemory( &_WaveFormat, sizeof(_WaveFormat) );

    ZeroMemory( &dsBD, sizeof(dsBD) );
    dsBD.dwSize      = sizeof(dsBD);
    dsBD.dwFlags     = DSBCAPS_CTRL3D | DSBCAPS_CTRLVOLUME |
                       DSBCAPS_CTRLFREQUENCY | DSBCAPS_GETCURRENTPOSITION2 | 
                       DSBCAPS_STICKYFOCUS;
    dsBD.lpwfxFormat = &_WaveFormat;

    DWORD dwWFXSize = sizeof(_WaveFormat);
    if( FAILED( hr = g_pSWSynthPort->GetFormat( &_WaveFormat, &dwWFXSize,
                                                &dsBD.dwBufferBytes ) ) )
    {
        _WaveFormat.wFormatTag      = WAVE_FORMAT_PCM;
        _WaveFormat.nChannels       = 2;
        _WaveFormat.nSamplesPerSec  = 22050;
        _WaveFormat.wBitsPerSample  = 16;
        _WaveFormat.nBlockAlign     = _WaveFormat.nChannels *  
                                      ( _WaveFormat.wBitsPerSample / 8 );
        _WaveFormat.nAvgBytesPerSec = _WaveFormat.nSamplesPerSec * _WaveFormat.nBlockAlign;
        _WaveFormat.cbSize          = sizeof(_WaveFormat);

        dsBD.dwBufferBytes = _WaveFormat.nAvgBytesPerSec;
    }

    if( SUCCEEDED( hr = IDirectSound_CreateSoundBuffer( g_lpds, &dsBD, &pDSB,
                                                        NULL ) ) )
    {
        hr = g_pSWSynthPort->SetDirectSound( g_lpds, pDSB );
    }

    if( pDSB && FAILED(hr) )
    {
        pDSB->Release();
        pDSB = NULL;
    }

    return pDSB;
}




//-----------------------------------------------------------------------------
// Name: InitDirectMusic()
// Desc: Create and init DirectMusic performace, port, and loader objects
//-----------------------------------------------------------------------------
HRESULT InitDirectMusic( HWND hWnd )
{
    if( g_pDMusic )
        return S_FALSE;

    CoInitialize(NULL);

    if( FAILED( CoCreateInstance( CLSID_DirectMusic, NULL,
                                     CLSCTX_INPROC_SERVER, IID_IDirectMusic,
                                     (VOID**)&g_pDMusic ) ) )
    {
        MessageBox( hWnd, "Could not create DirectMusic!",
                    "Initialization Error", MB_ICONERROR|MB_OK );
        return E_FAIL;
    }

    if( FAILED( g_pDMusic->SetDirectSound( g_lpds, hWnd ) ) )
    {
        MessageBox( hWnd, "Could not set DSound to the DMusic object!",
                    "Initialization Error", MB_ICONERROR|MB_OK );
        return E_FAIL;
    }

    g_pPerf        = CreatePerformance();
    g_pSWSynthPort = CreateSWSynthPort();
    g_pLoader      = CreateLoader();

    if( NULL==g_pPerf || NULL==g_pSWSynthPort || NULL==g_pLoader )
    {
        MessageBox( hWnd, "Could not create Performance, SWSynthPort,\n"
                    "and/or Loader objects!", "Initialization Error",
                    MB_ICONERROR|MB_OK );
        return E_FAIL;
    }

    if( SUCCEEDED( g_pPerf->AddPort( g_pSWSynthPort ) ) )
    {
        if( SUCCEEDED( g_pPerf->AssignPChannelBlock( 0, g_pSWSynthPort, 1 ) ) )
            return S_OK;
    }

    MessageBox( hWnd, "Could not add performance port, and/or assign\n"
                "P channel block!", "Initialization Error", MB_ICONERROR|MB_OK );
    return E_FAIL;
}




//-----------------------------------------------------------------------------
// Name: FreeDirectMusic()
// Desc: Close and release the DirectMusic objects
//-----------------------------------------------------------------------------
HRESULT FreeDirectMusic()
{
    if( g_pDMusic )
    {
        g_pDMusic->Activate(FALSE);
    }

    if( g_pSWSynthPort )
    {
        g_pSWSynthPort->Release();
        g_pSWSynthPort = NULL;
    }

    if( g_pPerf )
    {
        // If there is any music playing, stop it.
        g_pPerf->Stop( NULL, NULL, 0, 0 );

        // CloseDown and Release the performance object.
        g_pPerf->CloseDown();
        g_pPerf->Release();
        g_pPerf = NULL;
    }

    if( g_pLoader )
    {
        g_pLoader->Release();
        g_pLoader = NULL;
    }

    if( g_pDMusic )
    {
        g_pDMusic->Release();
        g_pDMusic = NULL;
    }

    CoUninitialize();

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: CreateSWSynthPort()
// Desc: Create a software synth port object
//-----------------------------------------------------------------------------
IDirectMusicPort* CreateSWSynthPort()
{
    if( !g_pDMusic )
        return NULL;

    IDirectMusicPort* pPort = NULL;
    HRESULT           hr;
    GUID              guidPortGUID;
    DMUS_PORTPARAMS   dmos;
    DMUS_PORTCAPS     dmpc;

    // See if the default port suits our needs
    if( SUCCEEDED( g_pDMusic->GetDefaultPort( &guidPortGUID ) ) )
    {
        ZeroMemory( &dmos, sizeof(dmos) );
        dmos.dwSize          = sizeof(DMUS_PORTPARAMS);
        dmos.dwChannelGroups = 1;
        dmos.dwValidParams   = DMUS_PORTPARAMS_CHANNELGROUPS;

        if( SUCCEEDED( g_pDMusic->CreatePort( guidPortGUID, &dmos, &pPort,
                                              NULL ) ) )
        {
            ZeroMemory( &dmpc, sizeof(dmpc) );
            dmpc.dwSize = sizeof(DMUS_PORTCAPS);

            if( SUCCEEDED( pPort->GetCaps( &dmpc ) ) )
            {
                if( ( dmpc.dwClass != DMUS_PC_OUTPUTCLASS ) ||
                   !( dmpc.dwFlags & DMUS_PC_DLS ) ||
                   !( dmpc.dwFlags & DMUS_PC_DIRECTSOUND ) )
                {
                    pPort->Release();
                    pPort = NULL;
                }
            }
            else
            {
                pPort->Release();
                pPort = NULL;
            }
        }
    }

    if( NULL == pPort )
    {
        // Find a software synth output port.
        for( DWORD index = 0; ; index++ )
        {
            ZeroMemory( &dmpc, sizeof(dmpc) );
            dmpc.dwSize = sizeof(DMUS_PORTCAPS);

            hr = g_pDMusic->EnumPort( index, &dmpc );

            if( SUCCEEDED(hr) && hr != S_FALSE )
            {
                if( ( dmpc.dwClass == DMUS_PC_OUTPUTCLASS ) &&
                    ( dmpc.dwFlags & DMUS_PC_DLS ) &&
                    ( dmpc.dwFlags & DMUS_PC_DIRECTSOUND ) )
                {
                    CopyMemory( &guidPortGUID, &dmpc.guidPort, sizeof(GUID) );

                    ZeroMemory( &dmos, sizeof(dmos) );
                    dmos.dwSize          = sizeof(DMUS_PORTPARAMS);
                    dmos.dwChannelGroups = 1;
                    dmos.dwValidParams   = DMUS_PORTPARAMS_CHANNELGROUPS;

                    if( FAILED( g_pDMusic->CreatePort( guidPortGUID, &dmos,
                                                       &pPort, NULL ) ) )
                    {
                        pPort = NULL;
                    }
                    break;
                }
            }
            else
            {
                break;
            }
        }
    }

    return pPort;
}




//-----------------------------------------------------------------------------
// Name: CreatePerformance()
// Desc: Create a performance object that autodownloads instruments
//-----------------------------------------------------------------------------
IDirectMusicPerformance* CreatePerformance()
{
    if( !g_pDMusic )
        return NULL;

    IDirectMusicPerformance* pPerf = NULL;

    if( SUCCEEDED(CoCreateInstance( CLSID_DirectMusicPerformance,NULL,
                                    CLSCTX_INPROC, IID_IDirectMusicPerformance,
                                   (VOID**)&pPerf ) ) )
    {
        if( SUCCEEDED(pPerf->Init( &g_pDMusic, NULL, NULL ) ) )
        {
            // Set autodownloading to be on
            BOOL fAutoDownload = TRUE;
            if( FAILED( pPerf->SetGlobalParam( GUID_PerfAutoDownload,
                                               &fAutoDownload,
                                               sizeof(fAutoDownload) ) ) )
            {
                pPerf->Release();
                pPerf = NULL;
            }
        }
    }
    else
    {
        pPerf = NULL;
    }

    return pPerf;
}




//-----------------------------------------------------------------------------
// Name: CreateLoader()
// Desc: Create a loader object
//-----------------------------------------------------------------------------
IDirectMusicLoader* CreateLoader()
{
    IDirectMusicLoader* pLoader;

    if( FAILED( CoCreateInstance( CLSID_DirectMusicLoader, NULL, CLSCTX_INPROC,
                                  IID_IDirectMusicLoader, (VOID**)&pLoader ) ) )
    {
        pLoader = NULL;
    }
    return pLoader;
}




//-----------------------------------------------------------------------------
// Name: LoadSegment()
// Desc: Create a segment from the given file
//-----------------------------------------------------------------------------
IDirectMusicSegment* LoadSegment( LPCTSTR szName )
{
    if( !g_pLoader )
        return NULL;

    IDirectMusicSegment* pSegment = NULL;
    char                 szDir[_MAX_PATH];
    WCHAR                wszDir[_MAX_PATH];
    DMUS_OBJECTDESC      ObjDesc;

    if( !GetSearchPath( wszDir ) )
    {
        // no SDK sample path-- use working dir
#ifdef __BORLANDC__
        if( NULL == getcwd( szDir, _MAX_PATH ) )
#else
        if( NULL == _getcwd( szDir, _MAX_PATH ) )
#endif
        {
            // there was an error. Return NULL.
            return NULL;
        }
        mbstowcs( wszDir, szDir, MAX_PATH );
    }

    g_pLoader->SetSearchDirectory( GUID_DirectMusicAllTypes, wszDir, FALSE );

    ObjDesc.guidClass   = CLSID_DirectMusicSegment;
    ObjDesc.dwSize      = sizeof(DMUS_OBJECTDESC);
    ObjDesc.dwValidData = DMUS_OBJ_CLASS | DMUS_OBJ_FILENAME;
    mbstowcs( ObjDesc.wszFileName, szName, DMUS_MAX_FILENAME );

    g_pLoader->GetObject( &ObjDesc, IID_IDirectMusicSegment, (VOID**)&pSegment );

    return pSegment;
}



