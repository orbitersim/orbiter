// Helper.cpp : Helper functions for DirectMusic
//
// Copyright (c) 1998-1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------

#include <direct.h>
#include <dmusici.h>
#include <dmusicf.h>
#include "debug.h"
#include "DMHelper.h"

static BOOL GetSearchPath(WCHAR wszPath[MAX_PATH]);

//
// Create and return the graph object.
// It is the caller's responsibility to release the object.
//
IDirectMusicGraph* CreateGraph(void)
{
	IDirectMusicGraph* pGraph;

	if (FAILED(CoCreateInstance(
			CLSID_DirectMusicGraph,
			NULL,
			CLSCTX_INPROC, 
			IID_IDirectMusicGraph,
			(void**)&pGraph
		)))
	{
        Trace(0, "Could not create DMusicGraph");
		pGraph = NULL;
	}

	return pGraph;
}

//
// Create and return the loader object.
// It is the caller's responsibility to release the object.
//
IDirectMusicLoader* CreateLoader(void)
{
	IDirectMusicLoader* pLoader;

    WCHAR wszPath[MAX_PATH];

	if (FAILED(CoCreateInstance(
			CLSID_DirectMusicLoader,
			NULL,
			CLSCTX_INPROC, 
			IID_IDirectMusicLoader,
			(void**)&pLoader
		)))
	{
        Trace(0, "Could not create DMusicLoader");
		pLoader = NULL;
	}
	else
	{
        HRESULT hr = E_FAIL;

        if (GetSearchPath(wszPath))
        {
		    hr = pLoader->SetSearchDirectory( GUID_DirectMusicAllTypes, wszPath, FALSE );
        }
        
        if (FAILED(hr))
        {
		    hr = pLoader->SetSearchDirectory( GUID_DirectMusicAllTypes, L".", FALSE );
        }

        if (FAILED(hr))
        {
	        Trace(0, "Could not set search directory");
            pLoader->Release();
            pLoader = NULL;
        }
	}

	return pLoader;
}

//
// Create and return the performance object.
// It is the caller's responsibility to release the object.
//
IDirectMusicPerformance* CreatePerformance(void)
{
	IDirectMusicPerformance* pPerf;

	if (FAILED(CoCreateInstance(
			CLSID_DirectMusicPerformance,
			NULL,
			CLSCTX_INPROC, 
			IID_IDirectMusicPerformance,
			(void**)&pPerf
		)))
	{
        Trace(0, "Could not create DMusicPerformance");
		pPerf = NULL;
	}

	return pPerf;
}

// Create and return the composer object.
// It is the caller's responsibility to release the object.
IDirectMusicComposer* CreateComposer(void)
{
	IDirectMusicComposer* pComposer;

	if (FAILED(CoCreateInstance(
			CLSID_DirectMusicComposer,
			NULL,
			CLSCTX_INPROC, 
			IID_IDirectMusicComposer,
			(void**)&pComposer
		)))
    {
        Trace(0, "Could not create DMusicComposer");
		pComposer = NULL;
	}
	return pComposer;
}

IDirectMusicSegment* CreateSegmentFromFile(IDirectMusicLoader* pLoader, WCHAR* wszFileName)
{
	DMUS_OBJECTDESC ObjDesc; // Object descriptor for pLoader->GetObject()
	IDirectMusicSegment* pSegment = NULL;

	ObjDesc.guidClass = CLSID_DirectMusicSegment;
	ObjDesc.dwSize = sizeof(DMUS_OBJECTDESC);
	wcscpy( ObjDesc.wszFileName, wszFileName );
	ObjDesc.dwValidData = DMUS_OBJ_CLASS | DMUS_OBJ_FILENAME;
	pLoader->GetObject( &ObjDesc, IID_IDirectMusicSegment, (void**)&pSegment );
	return pSegment;
}

void RemoveTrackType(IDirectMusicSegment* pSegment, REFGUID rguid)
{
	IDirectMusicTrack* pTrack;
	// remove every track of type rguid from pSegment.
	// In GetTrack(), 0xffffffff indicates to scan all groups in the Segment,
	// and the 0 indicates to get the first track in that group.
	while( SUCCEEDED( pSegment->GetTrack( rguid, 0xffffffff, 0, &pTrack )))
	{
		if( FAILED( pSegment->RemoveTrack( pTrack )))
		{
			// this should never happen
			break;
		}
		pTrack->Release();
	}
}

IDirectMusicStyle* GetStyle(IDirectMusicLoader* pLoader, WCHAR* wszStyle)
{
	DMUS_OBJECTDESC ObjDesc; // Object descriptor for pLoader->GetObject()
	IDirectMusicStyle* pStyle = NULL;

	ObjDesc.guidClass = CLSID_DirectMusicStyle;
	ObjDesc.dwSize = sizeof(DMUS_OBJECTDESC);
	wcscpy( ObjDesc.wszFileName, wszStyle );
	ObjDesc.dwValidData = DMUS_OBJ_CLASS | DMUS_OBJ_FILENAME;
	pLoader->GetObject( &ObjDesc, IID_IDirectMusicStyle, (void**)&pStyle );
	return pStyle;
}

IDirectMusicChordMap* GetChordMap(IDirectMusicLoader* pLoader, WCHAR* wszChordMap)
{
	DMUS_OBJECTDESC ObjDesc; // Object descriptor for pLoader->GetObject()
	IDirectMusicChordMap* pChordMap = NULL;

	ObjDesc.guidClass = CLSID_DirectMusicChordMap;
	ObjDesc.dwSize = sizeof(DMUS_OBJECTDESC);
	wcscpy( ObjDesc.wszFileName, wszChordMap );
	ObjDesc.dwValidData = DMUS_OBJ_CLASS | DMUS_OBJ_FILENAME;
	pLoader->GetObject( &ObjDesc, IID_IDirectMusicChordMap, (void**)&pChordMap );
	return pChordMap;
}

IDirectMusicSegment* CreateSegmentFromTemplate(IDirectMusicLoader* pLoader, 
											  IDirectMusicSegment* pTemplate,
											  IDirectMusicComposer* pComposer,
											  WCHAR* wszStyle,
											  WCHAR* wszChordMap)
{
	IDirectMusicSegment* pSegment = NULL;
	IDirectMusicStyle* pStyle;
	IDirectMusicChordMap* pChordMap;

	pStyle = GetStyle( pLoader, wszStyle );
	pChordMap = GetChordMap( pLoader, wszChordMap );
	if( pStyle && pChordMap )
	{
		pComposer->ComposeSegmentFromTemplate(
			pStyle,
			pTemplate,
			3, // lowest activity level
			pChordMap,
			&pSegment);
	}
	RELEASE(pStyle);
	RELEASE(pChordMap);
	return pSegment;
}

IDirectMusicSegment* GetMotif(IDirectMusicLoader* pLoader, 
							 WCHAR* wszStyle,
							 WCHAR* wszMotif)
{
	IDirectMusicStyle* pStyle;
	IDirectMusicSegment* pSegment = NULL;

	pStyle = GetStyle(pLoader, wszStyle);
	if( pStyle )
	{
		pStyle->GetMotif(wszMotif, &pSegment);
		pStyle->Release();
	}
	return pSegment;
}

// Get registry search path
//
static char szDirectMusicMedia[] = "\\DMusic\\Media";

static BOOL GetSearchPath(WCHAR wszPath[MAX_PATH])
{
	HKEY	hkDirectX;
	BOOL	bRet = FALSE;
	char	szPath[MAX_PATH];
	DWORD	cbPath;


	// Get DirectX SDK search path from the registry
	//
	if (RegOpenKeyEx(HKEY_LOCAL_MACHINE,
					 "Software\\Microsoft\\DirectX",
					 0,							// Reserved
					 KEY_READ,
					 &hkDirectX))
    {
		return FALSE;
	}

	cbPath = sizeof(szPath);
	if (RegQueryValueEx(hkDirectX,
                                                "DXSDK Samples Path",
						NULL,					// Reserved
						NULL,					// Type: don't care
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
