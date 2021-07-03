//-----------------------------------------------------------------------------
// File: WinMain.cpp
//
// Desc: Plays a Primary Segment using DirectMusic
//
//
// Copyright (c) 1998-1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------


#define STRICT
#include <windows.h>
#include "resource.h"

#include <objbase.h>
#include <initguid.h>
#include <conio.h>
#include <direct.h>
#include <stdio.h>
#include <stdlib.h>
#include <dmusicc.h>
#include <dmusici.h>
#include "EchoTool.h"
#include "Helper.h"

// Both Borland CBuilder3 and Watcom C++ 11 have "chdir" defined incorrectly
// So the code below changes chdir only for MSVC++.
#if	(defined(_MSC_VER))
#define chdir   _chdir
#endif

//-----------------------------------------------------------------------------
// Global variables for the DirectMusic sample 
//-----------------------------------------------------------------------------
IDirectMusicLoader*         g_pLoader       = NULL;
IDirectMusicPerformance*    g_pPerformance  = NULL;
CEchoTool*                  g_pEchoTool     = NULL;
IDirectMusicSegment*        g_pSegment      = NULL;




//-----------------------------------------------------------------------------
// Function: InitDirectMusic
//
// Description: 
//      Initilizes DirectMusic
//
//-----------------------------------------------------------------------------
HRESULT InitDirectMusic( LPSTR lpCmdLine )
{
    HRESULT hr;
    BOOL    bUseCurrentWorkingDir = FALSE;

    if( lpCmdLine[0] != 0  )  // if there are command line params
    {
        if( chdir( lpCmdLine ) != 0 )
        {
            MessageBox( NULL, 
                        "Failed to find directory.", 
                        "Error", 
                        MB_ICONERROR | MB_OK );
            return E_FAIL;
        }
        bUseCurrentWorkingDir = TRUE;
    }

    // Initialize COM
    hr = CoInitialize(NULL);
    if ( FAILED(hr) )
	{
        MessageBox( NULL, "Couldn't initialize COM", 
                    "EchoTool", MB_ICONERROR | MB_OK );
        return hr;
	}

    // Create loader object
    hr = CoCreateInstance( CLSID_DirectMusicLoader, NULL, CLSCTX_INPROC, 
                           IID_IDirectMusicLoader, (void**)&g_pLoader );
    if ( FAILED(hr) )
	{
        MessageBox( NULL, "Couldn't Create a DMusicLoader", 
                    "EchoTool", MB_ICONERROR | MB_OK );
        return hr;
	}

    // Create performance object
    hr = CoCreateInstance( CLSID_DirectMusicPerformance, NULL, CLSCTX_INPROC, 
                           IID_IDirectMusicPerformance, (void**)&g_pPerformance );
    if ( FAILED(hr) )
	{
        MessageBox( NULL, "Couldn't create a DMusicPerformance", 
                    "EchoTool", MB_ICONERROR | MB_OK );
        return hr;
	}

    // Create the EchoTool object
    g_pEchoTool = new CEchoTool();
    if( NULL == g_pEchoTool )
        return E_OUTOFMEMORY;

    // Start with no echoes
    g_pEchoTool->SetEchoNum(0);

    // Add the echo tool to the performance
    AddTool();

    // Initialize the software synthesizer
    hr = InitializeSynth();
    if ( FAILED(hr) )
	{
        MessageBox( NULL, "Couldn't initialize the software synthesizer", 
                    "EchoTool", MB_ICONERROR | MB_OK );
        return hr;
	}

    // Load the segment
    hr = LoadSegment( bUseCurrentWorkingDir );
    if ( FAILED(hr) )
	{
        MessageBox( NULL, "Couldn't load the segment", 
                    "EchoTool", MB_ICONERROR | MB_OK );
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
// Function: AddTool
//
// Description: 
//      Adds the EchoTool to the performance
//
//-----------------------------------------------------------------------------
HRESULT AddTool()
{
    HRESULT            hr;
    IDirectMusicGraph* pGraph;

    // First, create an IDirectMusicGraph object to hold the tool
    hr = CoCreateInstance(
            CLSID_DirectMusicGraph,
            NULL,
            CLSCTX_INPROC, 
            IID_IDirectMusicGraph,
            (void**)&pGraph
        );
    if ( FAILED(hr) )
	{
        MessageBox( NULL, "Couldn't create DMusicGraph", 
                    "EchoTool", MB_ICONERROR | MB_OK );
        return hr;
	}

    // add the tool to the graph - it will be AddRef()'d inside
    // this function. The second parameter, NULL, means to apply
    // the tool to all PChannel's. The third parameter, 0, is only
    // applicable if the second parameter is non-NULL. The fourth
    // parameter, 0, means to insert the tool at the beginning of
    // any list of tools inside the graph. Since this is the first
    // tool added to this graph, this value isn't important.
    hr = pGraph->InsertTool( (IDirectMusicTool*)g_pEchoTool, NULL, 0, 0 );
    if ( FAILED(hr) )
	{
        MessageBox( NULL, "Couldn't insert the tool to the graph", 
                    "EchoTool", MB_ICONERROR | MB_OK );
        return hr;
	}

    // add the graph to the performance. This means that all
    // segments that play through this performance will pass
    // through the tools in this graph. Graphs may be set on
    // individual segments as well.
    g_pPerformance->SetGraph( pGraph );
    pGraph->Release();

    return S_OK;
}




//-----------------------------------------------------------------------------
// Function: InitializeSynth
//
// Description: 
//      Initialize the software synthesizer into the performance.
//      This function also calls IDirectMusicPerformance::Init to
//      initialize the performance and create the DirectMusic object.
//
//-----------------------------------------------------------------------------
HRESULT InitializeSynth()
{
    HRESULT             hr;
    IDirectMusic*       pDM;
    IDirectMusicPort*   pPort = NULL;
    DMUS_PORTPARAMS     dmos;
    GUID                guidSink;

    // Initialize the performance. Have the performance create the
    // DirectMusic object by setting pDM to NULL. It is needed to
    // create the port.
    pDM = NULL;
    hr = g_pPerformance->Init( &pDM, NULL, NULL );
    if ( FAILED(hr) )
        return hr;
    // Set autodownloading to be on.  This will cause DLS instruments to be downloaded
	// whenever a segment is played, and unloaded whenever it stops.  Please see the
	// DirectMusic documentation for more information.
    BOOL fAutoDownload = TRUE;
    g_pPerformance->SetGlobalParam(GUID_PerfAutoDownload,&fAutoDownload,sizeof(BOOL));
    
    // Create the software synth port.
    ZeroMemory( &dmos, sizeof(DMUS_PORTPARAMS) );
    dmos.dwSize = sizeof(DMUS_PORTPARAMS);  
    dmos.dwChannelGroups = 5; // create 5 channel groups on the port
    dmos.dwEffectFlags = 0;
    dmos.dwValidParams = DMUS_PORTPARAMS_CHANNELGROUPS | DMUS_PORTPARAMS_EFFECTS;

    ZeroMemory( &guidSink, sizeof(GUID) );

    hr = pDM->CreatePort( CLSID_DirectMusicSynth, &dmos, &pPort, NULL );
    if ( FAILED(hr) )
        return hr;

    hr = pDM->Activate( TRUE );
    if ( FAILED(hr) )
        return hr;

    // Succeeded in creating the port. Add the port to the
    // Performance with five groups of 16 midi channels.
    hr = g_pPerformance->AddPort( pPort );
    if ( FAILED(hr) )
        return hr;

    // Assign a block of 16 PChannels to this port.
    // Block 0, port pPort, and group 1 means to assign
    // PChannels 0-15 to group 1 on port pPort.
    // PChannels 0-15 correspond to the standard 16
    // MIDI channels.
    g_pPerformance->AssignPChannelBlock( 0, pPort, 1 );

    // asign the other 4 groups
    g_pPerformance->AssignPChannelBlock( 1, pPort, 2 );
    g_pPerformance->AssignPChannelBlock( 2, pPort, 3 );
    g_pPerformance->AssignPChannelBlock( 3, pPort, 4 );
    g_pPerformance->AssignPChannelBlock( 4, pPort, 5 );

    // Release the port since the performance now has its own reference.
    pPort->Release();

    // release the DirectMusic object. The performance has its
    // own reference and we just needed it to call CreatePort.
    pDM->Release();

    return S_OK;
}




//-----------------------------------------------------------------------------
// Function: LoadSegment
//
// Description: 
//      Change the loader's current search directory and load the
//      Sample.sgt segment. The segment internally references the
//      Sample.sty style and Sample.dls downloadable sample file. 
//      When the loader loads the segment, it also loads the internally 
//      referenced files from the search directory.
//      If fUseCWD is TRUE, use the current working directory. Otherwise,
//      use the path referenced by the DirectMusic/Media registry key.
//
//-----------------------------------------------------------------------------
HRESULT LoadSegment( BOOL fUseCWD )
{
    HRESULT             hr;
    DMUS_OBJECTDESC     ObjDesc; // Object descriptor for pLoader->GetObject()
    WCHAR               wszDir[_MAX_PATH];

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

    // Now load the segment file.
    // Sections load as type Segment, as do MIDI files, for example.
    ObjDesc.guidClass = CLSID_DirectMusicSegment;
    ObjDesc.dwSize = sizeof(DMUS_OBJECTDESC);
    wcscpy( ObjDesc.wszFileName, L"Sample.sgt" );
    ObjDesc.dwValidData = DMUS_OBJ_CLASS | DMUS_OBJ_FILENAME;

    hr = g_pLoader->GetObject( &ObjDesc, IID_IDirectMusicSegment, (void**)&g_pSegment );

    return hr;
}




//-----------------------------------------------------------------------------
// Function: FreeDirectMusic
//
// Description: 
//      Releases DirectMusic
//
//-----------------------------------------------------------------------------
HRESULT FreeDirectMusic()
{
    if( g_pSegment )
    {
        // Release the Segment
        g_pSegment->Release();
        g_pSegment = NULL;
    }

    if( g_pEchoTool )
    {
        // Release the EchoTool
        g_pEchoTool->Release();
    }

    if( g_pPerformance )
    {
        // If there is any music playing, stop it.
        g_pPerformance->Stop( NULL, NULL, 0, 0 );

        // CloseDown and Release the performance object
        g_pPerformance->CloseDown();
        g_pPerformance->Release();
    }

    if( g_pLoader )
    {
        // Release the loader object
        g_pLoader->Release();
    }

    // Release COM
    CoUninitialize();

    return S_OK;
}




//-----------------------------------------------------------------------------
// Function: GetSearchPath
//
// Description: 
//      Finds and returns the DirectX SDK search path from the registery
//
//-----------------------------------------------------------------------------
BOOL GetSearchPath(WCHAR wszPath[MAX_PATH])
{
    HKEY    hkDirectX;
    BOOL    bRet = FALSE;
    char    szPath[MAX_PATH];
    DWORD   cbPath;


    // Get DirectX SDK search path from the registry
    //
    if (RegOpenKeyEx(HKEY_LOCAL_MACHINE,
                     "Software\\Microsoft\\DirectX",
                     0,                         // Reserved
                     KEY_READ,
                     &hkDirectX))
    {
        return FALSE;
    }

    cbPath = sizeof(szPath);
    if (RegQueryValueEx(hkDirectX,
                        "DXSDK Samples Path",
                        NULL,                   // Reserved
                        NULL,                   // Type: don't care
                        (LPBYTE)szPath,
                        &cbPath) == ERROR_SUCCESS)
    {
        if (cbPath + sizeof(szDirectMusicMedia) > MAX_PATH)
        {
            return FALSE;
        }

        strcat(szPath, szDirectMusicMedia);

        // DirectMusic requires the search path as a wide string
        //
        mbstowcs(wszPath, 
                 szPath,
                 MAX_PATH);
        bRet = TRUE;
    }

    RegCloseKey(hkDirectX);
    return bRet;
}

