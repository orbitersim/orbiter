//-----------------------------------------------------------------------------
// File: PlayMotf.cpp
//
// Desc: Plays DMusic motifs
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
#include "PlayMotf.h"


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
IDirectMusicStyle*       g_pStyle       = NULL;
WCHAR                    g_awstrMotifName[9][MAX_PATH];




//-----------------------------------------------------------------------------
// Name: InitDirectMusic()
// Desc: Initilizes DirectMusic
//-----------------------------------------------------------------------------
HRESULT InitDirectMusic( LPSTR strCmdLine )
{
    HRESULT hr;
    BOOL    bUseCurrentWorkingDir = FALSE;

	// Check command line for a different directory
    if( strCmdLine[0] != 0  )
    {
        if( chdir( strCmdLine ) != 0 )
        {
            MessageBox( NULL, "Failed to find directory.", 
                        "Error", MB_ICONERROR | MB_OK );
            return E_FAIL;
        }
        bUseCurrentWorkingDir = TRUE;
    }

    // Initialize COM
    hr = CoInitialize(NULL);
    if( FAILED(hr) )
	{
	    MessageBox( NULL, "Could not initialize COM", 
                    "PlayMotf", MB_ICONERROR | MB_OK );
        return hr;
	}

    // Create loader object
    hr = CoCreateInstance( CLSID_DirectMusicLoader, NULL, CLSCTX_INPROC, 
                           IID_IDirectMusicLoader, (void**)&g_pLoader );
    if( FAILED(hr) )
	{
	    MessageBox( NULL, "Could not create DMusic Loader", 
                    "PlayMotf", MB_ICONERROR | MB_OK );
        return hr;
	}

    // Create performance object
    hr = CoCreateInstance( CLSID_DirectMusicPerformance, NULL, CLSCTX_INPROC, 
                           IID_IDirectMusicPerformance, (void**)&g_pPerformance );
    if( FAILED(hr) )
	{
	    MessageBox( NULL, "Could not create DMusic Performance", 
                    "PlayMotf", MB_ICONERROR | MB_OK );
        return hr;
	}

    // Initialize the software synthesizer
    hr = InitializeSynth();
    if( FAILED(hr) )
	{
		// On error, a MsgBox is displayed in the InitializeSynth() function
        return hr;
	}

    // Load the segment
    hr = LoadSegment( bUseCurrentWorkingDir );
    if( FAILED(hr) )
	{
	    MessageBox( NULL, "Could not load segment", 
                    "PlayMotf", MB_ICONERROR | MB_OK );
        return hr;
	}

    // set the segment to repeat many times
    g_pSegment->SetRepeats(200);

    // Get the Style from the Segment by calling the Segment's GetData() with
	// the data type GUID_StyleTrackStyle. 0xffffffff indicates to look at
	// Tracks in all TrackGroups in the segment. The first 0 indicates to
	// retrieve the Style from the first Track  in the indicated TrackGroup.
    // The second 0 indicates to retrieve the Style from the beginning of the
	// Segment, i.e. time 0 in Segment time. If this Segment was loaded from a
	// Section file, there is only one  Style and it is at time 0.
    // Note that the GetData() call with GUID_IDirectMusicStyle assumes the
    // third parameter is the address of a pointer to an IDirectMusicStyle.
    hr = g_pSegment->GetParam( GUID_IDirectMusicStyle, 0xffffffff, 0, 0, NULL,
		                       (VOID*)&g_pStyle );
    if( FAILED(hr) )
	{
	    MessageBox( NULL, "Could not get DMusic style", 
                    "PlayMotf", MB_ICONERROR | MB_OK );
        return hr;
	}

    // Play the segment and wait. The DMUS_SEGF_BEAT indicates to play on the next beat 
    // if there is a segment currently playing. The first 0 indicates to
    // play (on the next beat from) now.
    // The final NULL means do not return an IDirectMusicSegmentState* in
    // the last parameter.
    g_pPerformance->PlaySegment( g_pSegment, DMUS_SEGF_BEAT, 0, NULL );

    // Get the names of the Motifs from the Style. Styles may have
    // any number of Motifs, but for simplicity's sake only get
    // a maximum of 9 here.
    for( DWORD dwIndex = 0; dwIndex < 9; dwIndex++ )
    {
        if( FAILED( g_pStyle->EnumMotif( dwIndex, g_awstrMotifName[dwIndex] ) ) )
        {
            g_awstrMotifName[dwIndex][0] = 0;
            break;
        }
    }

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: PlayMotif()
// Desc: Play the named Motif from the Style in the Performance
//-----------------------------------------------------------------------------
HRESULT PlayMotif( WCHAR* pwstrMotifName )
{
    IDirectMusicSegment* pSeg;
    HRESULT              hr;

    // Get the Motif Segment from the Style, setting it to play once
    // through (no repeats.) Check for S_OK specifically, because
    // GetMotif() returns S_FALSE if it doesn't find the Motif.
    hr = g_pStyle->GetMotif( pwstrMotifName, &pSeg );

    if( S_OK == hr )
    {
        // Play the segment. The PSF_BEAT indicates to play on the next beat 
        // if there is a segment currently playing. PSF_SECONDARY means to
        // play the segment as a secondary segment, which plays on top of
        // the currently playing primary segment. The first 0 indicates to
        // play (on the next beat from) now.
        // The final NULL means do not return an IDirectMusicSegmentState* in
        // the last parameter.
        g_pPerformance->PlaySegment( pSeg, 
                                     DMUS_SEGF_BEAT | DMUS_SEGF_SECONDARY, 
                                     0, 
                                     NULL );
        pSeg->Release();
    }

    return hr;
}




//-----------------------------------------------------------------------------
// Name: InitializeSynth()
// Desc: Initialize the software synthesizer into the performance. This
//       function also calls IDirectMusicPerformance::Init to initialize the
//       performance and create the DirectMusic object.
//-----------------------------------------------------------------------------
HRESULT InitializeSynth()
{
    HRESULT           hr;
    IDirectMusic*     pDM;
    IDirectMusicPort* pPort = NULL;
    DMUS_PORTPARAMS   dmos;

    // Initialize the performance. Have the performance create the
    // DirectMusic object by setting pDM to NULL. It is needed to
    // create the port.
    pDM = NULL;
    hr = g_pPerformance->Init( &pDM, NULL, NULL );
    if ( FAILED(hr) )
	{
	    MessageBox( NULL, "Could not initialize Performance", 
                    "PlayMotf", MB_ICONERROR | MB_OK );
        return hr;
	}
    // Set autodownloading to be on.  This will cause DLS instruments to be downloaded
	// whenever a segment is played, and unloaded whenever it stops.  Please see the
	// DirectMusic documentation for more information.
	BOOL bAutoDownload = TRUE;
	g_pPerformance->SetGlobalParam( GUID_PerfAutoDownload, &bAutoDownload, sizeof(BOOL) );
    
    // Create the software synth port.
    // An alternate, easier method is to call
    // pPerf->AddPort(NULL), which automatically
    // creates the synth port, adds it to the
    // performance, and assigns PChannels.
    ZeroMemory( &dmos, sizeof(DMUS_PORTPARAMS) );
    dmos.dwSize          = sizeof(DMUS_PORTPARAMS);  
    dmos.dwChannelGroups = 1; // create 1 channel groups on the port
    dmos.dwValidParams   = DMUS_PORTPARAMS_CHANNELGROUPS; 

    hr = pDM->CreatePort( CLSID_DirectMusicSynth, &dmos, &pPort, NULL );
    if( FAILED(hr) )
	{
	    MessageBox( NULL, "Could not create port", 
                    "PlayMotf", MB_ICONERROR | MB_OK );
        return hr;
	}

    hr = pDM->Activate( TRUE );
    if( FAILED(hr) )
	{
	    MessageBox( NULL, "Could not activate DMusic", 
                    "PlayMotf", MB_ICONERROR | MB_OK );
        return hr;
	}

    // Succeeded in creating the port. Add the port to the Performance with
	// five groups of 16 midi channels.
    hr = g_pPerformance->AddPort( pPort );
    if( FAILED(hr) )
	{
	    MessageBox( NULL, "Could not add port", 
                    "PlayMotf", MB_ICONERROR | MB_OK );
        return hr;
	}

    // Assign a block of 16 PChannels to this port. Block 0, port pPort, and
	// group 1 means to assign PChannels 0-15 to group 1 on port pPort.
    // PChannels 0-15 correspond to the standard 16 MIDI channels.
    g_pPerformance->AssignPChannelBlock( 0, pPort, 1 );

    // Release the port since the performance now has its own reference.
    pPort->Release();

    // Release the DirectMusic object. The performance has its own reference
	// and we just needed it to call CreatePort.
    pDM->Release();

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: LoadSegment()
// Desc: Change the loader's current search directory and load the Sample.sgt
//       segment. The segment internally references the Sample.sty style and
//       Sample.dls downloadable sample file. When the loader loads the
//       segment, it also loads the internally referenced files from the search
//       directory. If bUseCWD is TRUE, use the current working directory.
//       Otherwise, use the path referenced by the DirectMusic/Media registry
//       key.
//-----------------------------------------------------------------------------
HRESULT LoadSegment( BOOL bUseCWD )
{
    WCHAR           wstrPath[MAX_PATH];
    CHAR            strPath[MAX_PATH];
    
	if( bUseCWD )
    {
        // Change the loader's current search path to the app's current
		// working directory.
#ifdef __BORLANDC__
        if( NULL == getcwd( strPath, MAX_PATH ) )
            return E_FAIL;
#else
        if( NULL == _getcwd( strPath, MAX_PATH ) )
            return E_FAIL;
#endif
    }
    else 
	{
		// Get the search path from the registry key
		if( !GetSearchPath( strPath ) )
	        return E_FAIL;
    }

    // DirectMusic requires the search path as a wide string
    mbstowcs( wstrPath, strPath, MAX_PATH );

	// Set the search directory
    g_pLoader->SetSearchDirectory( GUID_DirectMusicAllTypes, wstrPath, FALSE );

    // Load the segment file.
    // Sections load as type Segment, as do MIDI files, for example.
    DMUS_OBJECTDESC dmod;
    dmod.dwSize      = sizeof(DMUS_OBJECTDESC);
    dmod.guidClass   = CLSID_DirectMusicSegment;
    dmod.dwValidData = DMUS_OBJ_CLASS | DMUS_OBJ_FILENAME;
    wcscpy( dmod.wszFileName, L"Sample.sgt" );

    return g_pLoader->GetObject( &dmod, IID_IDirectMusicSegment,
		                         (VOID**)&g_pSegment );
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
// Desc: Finds and returns the DirectX SDK search path from the registery
//-----------------------------------------------------------------------------
BOOL GetSearchPath( CHAR strPath[MAX_PATH] )
{
    HKEY  hkey;
    DWORD cbPath = MAX_PATH;

    // Get DirectX SDK search path from the registry
    if( RegOpenKeyEx( HKEY_LOCAL_MACHINE, g_strDirectXRegKey,
                      0, KEY_READ, &hkey ) )
        return FALSE;

    if( ERROR_SUCCESS != RegQueryValueEx( hkey, g_strSamplesPath, NULL, NULL,
                                          (BYTE*)strPath, &cbPath ) )
	{
	    RegCloseKey( hkey );
		return FALSE;
	}

    if( cbPath + sizeof(g_strMediaPath) > MAX_PATH )
        return FALSE;

    strcat( strPath, g_strMediaPath );

    RegCloseKey( hkey );
    return TRUE;
}



