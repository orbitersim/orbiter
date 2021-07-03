//-----------------------------------------------------------------------------
// File: PlayPri.cpp
//
// Desc: Plays a Primary Segment using DirectMusic
//
//
// Copyright (c) 1998-1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------


#define STRICT
#include <objbase.h>
#include <initguid.h>
#include <conio.h>
#include <direct.h>
#include <dmusicc.h>
#include <dmusici.h>
#include "PlayPri.h"

// Both Borland CBuilder3 and Watcom C++ 11 have "chdir" defined incorrectly
// So the code below changes chdir only for MSVC++.
#if	(defined(_MSC_VER))
#define chdir   _chdir
#endif

//-----------------------------------------------------------------------------
// Global variables for the DirectMusic sample 
//-----------------------------------------------------------------------------
IDirectMusicLoader*      g_pLoader      = NULL;
IDirectMusicPerformance* g_pPerformance = NULL;
IDirectMusicSegment*     g_pSegment     = NULL;




//-----------------------------------------------------------------------------
// Name: InitDirectMusic()
// Desc: Initilizes DirectMusic
//-----------------------------------------------------------------------------
HRESULT InitDirectMusic( LPSTR lpCmdLine )
{
    HRESULT hr;
    BOOL    bUseCurrentWorkingDir = FALSE;

    if( lpCmdLine[0] != 0  )  // if there are command line params
    {
        if( chdir( lpCmdLine ) != 0 )
        {
            MessageBox( NULL, "Failed to find directory.", 
                        "PlayPri", MB_ICONERROR | MB_OK );
            return E_FAIL;
        }
        bUseCurrentWorkingDir = TRUE;
    }

    // Initialize COM
    hr = CoInitialize(NULL);
    if ( FAILED(hr) )
    {
        MessageBox( NULL, "Could not initialize COM", 
                    "PlayPri", MB_ICONERROR | MB_OK );
        return hr;
	}

    // Create loader object
    hr = CoCreateInstance( CLSID_DirectMusicLoader, NULL, CLSCTX_INPROC, 
                           IID_IDirectMusicLoader, (void**)&g_pLoader );
    if ( FAILED(hr) )
    {
        MessageBox( NULL, "Could not create DMusic Loader", 
                    "PlayPri", MB_ICONERROR | MB_OK );
        return hr;
	}

    // Create performance object
    hr = CoCreateInstance( CLSID_DirectMusicPerformance, NULL, CLSCTX_INPROC, 
                           IID_IDirectMusicPerformance, (void**)&g_pPerformance );
    if ( FAILED(hr) )
    {
        MessageBox( NULL, "Could not create DMusic Performance", 
                    "PlayPri", MB_ICONERROR | MB_OK );
        return hr;
	}

    // Initialize the software synthesizer
    hr = InitializeSynth();
    if ( FAILED(hr) )
    {
		// On error, a msgbox is displayed in the InitializeSynth() function
        return hr;
	}

    // Load the segment
    hr = LoadSegment( bUseCurrentWorkingDir );
    if ( FAILED(hr) )
    {
        MessageBox( NULL, "Could not load segment", 
                    "PlayPri", MB_ICONERROR | MB_OK );
        return hr;
	}

    // set the segment to repeat many times
    g_pSegment->SetRepeats(200);

    // Play the segment and wait. The DMUS_SEGF_BEAT indicates to play on the 
    // next beat if there is a segment currently playing. The first 0 indicates 
    // to play (on the next beat from) now.
    // The final NULL means do not return an IDirectMusicSegmentState* in
    // the last parameter.
    g_pPerformance->PlaySegment( g_pSegment, DMUS_SEGF_BEAT, 0, NULL );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: InitializeSynth()
// Desc: Initialize the software synthesizer into the performance.
//-----------------------------------------------------------------------------
HRESULT InitializeSynth()
{
    HRESULT hr;

    // Calling AddPort with NULL automatically initializes the Performance
    // by calling IDirectMusicPerformance.Init(), adds a default
    // port with one channel group, and assigns PChannels 0-15 
    // to the synth port's channel group MChannels 0-15.
    // Please refer to the PlayMotf example for a more flexible, yet slightly
    // more complicated, way of doing this.
    hr = g_pPerformance->Init(NULL,NULL,NULL);
    if ( FAILED(hr) )
    {
        MessageBox( NULL, "Could not initialize performance", 
                    "PlayPri", MB_ICONERROR | MB_OK );
        return hr;
	}
    // Set autodownloading to be on.  This will cause DLS instruments to be downloaded
	// whenever a segment is played, and unloaded whenever it stops.  Please see the
	// DirectMusic documentation for more information.
	BOOL fAutoDownload = TRUE;
	g_pPerformance->SetGlobalParam(GUID_PerfAutoDownload,&fAutoDownload,sizeof(BOOL));

    hr = g_pPerformance->AddPort( NULL );

    if( FAILED(hr) )
	{
        MessageBox( NULL, "Could not add port to performance", 
                    "PlayPri", MB_ICONERROR | MB_OK );
		return hr;
	}

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: LoadSegment()
// Desc: Change the loader's current search directory and load the Sample.sgt
//       segment. The segment internally references the Sample.sty style and
//       Sample.dls downloadable sample file. When the loader loads the
//       segment, it also loads the internally referenced files from the search
//       directory. If fUseCWD is TRUE, use the current working directory.
//       Otherwise, use the path referenced by the DirectMusic/Media registry
//       key.
//-----------------------------------------------------------------------------
HRESULT LoadSegment( BOOL fUseCWD )
{
    HRESULT         hr;
    DMUS_OBJECTDESC ObjDesc; // Object descriptor for pLoader->GetObject()
    WCHAR           wszDir[_MAX_PATH];

    // Change the loader's current search directory to the
    // application's current working directory or the DirectMusic
    // registry key.
    if( fUseCWD )
    {
        char szDir[_MAX_PATH];
        // Change the loader's current search directory to the
        // application's current working directory.
#ifdef __BORLANDC__
        if( getcwd( szDir, _MAX_PATH ) == NULL )
#else
        if( _getcwd( szDir, _MAX_PATH ) == NULL )
#endif
        {
            // there was an error. Return E_FAIL.
            return E_FAIL;
        }
        MULTI_TO_WIDE( wszDir, szDir );
    }
    else if(!GetSearchPath(wszDir))
    {
        // there was an error. Return E_FAIL.
        return E_FAIL;
    }

    g_pLoader->SetSearchDirectory( GUID_DirectMusicAllTypes, wszDir, FALSE );

    // now load the segment file.
    // sections load as type Segment, as do MIDI files, for example.
    ObjDesc.guidClass = CLSID_DirectMusicSegment;
    ObjDesc.dwSize = sizeof(DMUS_OBJECTDESC);
    wcscpy( ObjDesc.wszFileName, L"Sample.sgt" );
    ObjDesc.dwValidData = DMUS_OBJ_CLASS | DMUS_OBJ_FILENAME;

    hr = g_pLoader->GetObject( &ObjDesc, IID_IDirectMusicSegment, (void**)&g_pSegment );

    return hr;
}




//-----------------------------------------------------------------------------
// Name: FreeDirectMusic()
// Desc: Releases DirectMusic
//-----------------------------------------------------------------------------
HRESULT FreeDirectMusic()
{
    // Release the Segment
    if (NULL != g_pSegment)
    {
        g_pSegment->Release();
        g_pSegment = NULL;
    }

    // If there is any music playing, stop it.
    if (NULL != g_pPerformance)
    {
        g_pPerformance->Stop( NULL, NULL, 0, 0 );

        // CloseDown and Release the performance object
        g_pPerformance->CloseDown();
        g_pPerformance->Release();
        g_pPerformance = NULL;
    }

    // Release the loader object
    if (NULL != g_pLoader)
    {
        g_pLoader->Release();
        g_pLoader = NULL;
    }    

    // Release COM
    CoUninitialize();

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: GetSearchPath()
// Desc: Finds and returns the DirectX SDK search path from the registry
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



