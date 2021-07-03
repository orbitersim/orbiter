/*
**----------------------------------------------------------------------------
**
**  File:       Music.cpp
**  Purpose:    DirectMusic.
**  Notes:
**
**	Copyright (C) 1995-1999 Microsoft Corporation. All Rights Reserved.
**----------------------------------------------------------------------------
*/


#include "common.h"
#include "music.h"
#include <oleauto.h>
#include <dmusicc.h>
#include <dmusici.h>
#include <direct.h>
#include <stdio.h>

BoidMusic::BoidMusic()

{
	m_dwBeatsSinceLastMotif = 0;
	m_pSegment = NULL;
	m_pPort = NULL;
	m_pDMusic = NULL;
	m_fCollapsed = FALSE;
	m_dwIndex = 0;
	m_pBand = NULL;
	m_pStyle = NULL;
	m_pChordMap = NULL;
	long x;
	for (x = 0; x < 6; x++)
	{
		m_pTemplateSegments[x] = NULL;
		m_pPrimarySegments[x] = NULL;
		m_pMotifSegments[x] = NULL;
	}
	m_pPrimarySegments[1] = NULL;
	m_pTransitionSegment = NULL;
	m_pComposer = NULL;
	m_pLoader = NULL;
	m_pPerformance = NULL;
}

BoidMusic::~BoidMusic()

{
	long x;
	if (m_pSegment)
	{
		m_pSegment->Release();
	}
	if (m_pBand)
	{
		if(m_pPerformance)
		{
			m_pBand->Unload(m_pPerformance);
		}
		m_pBand->Release();
	}
	if (m_pGraph)
	{
		m_pGraph->Release();
	}
	if (m_pPort)
	{
		m_pPort->Release();
	}
	if (m_pDMusic)
	{
		m_pDMusic->Release();
	}
	for (x = 0; x< 6; x++)
	{
		if (m_pTemplateSegments[x])
		{
			m_pTemplateSegments[x]->Release();
		}
		if (m_pPrimarySegments[x])
		{
			m_pPrimarySegments[x]->Release();
		}
		if (m_pMotifSegments[x])
		{
			m_pMotifSegments[x]->Release();
		}
	}
	if (m_pStyle)
	{
		m_pStyle->Release();
	}
	if (m_pChordMap)
	{
		m_pChordMap->Release();
	}
	if (m_pTransitionSegment)
	{
		m_pTransitionSegment->Release();
	}
	if (m_pComposer)
	{
		m_pComposer->Release();
	}
	if (m_pLoader)
	{
		m_pLoader->Release();
	}
	if (m_pPerformance)
	{
		m_pPerformance->Release();
	}
}

BOOL BoidMusic::LoadStyle()

{
	if (m_pStyle) m_pStyle->Release();

	DMUS_OBJECTDESC ObjectDescript;
	ObjectDescript.guidClass = CLSID_DirectMusicStyle;
	wcscpy(ObjectDescript.wszFileName, L"Boids2.sty");
	ObjectDescript.dwValidData = DMUS_OBJ_CLASS | DMUS_OBJ_FILENAME ;
	ObjectDescript.dwSize = sizeof(DMUS_OBJECTDESC);
	if (SUCCEEDED(m_pLoader->GetObject(&ObjectDescript, IID_IDirectMusicStyle, (void**)&m_pStyle)))
	{
		BSTR bstr = SysAllocString(L"Default");
		m_pStyle->GetBand(bstr, &m_pBand) ;
		SysFreeString(bstr);

        return TRUE;
	}

    return FALSE;
}

BOOL BoidMusic::LoadSegment()

/*	This loads a precomposed segment.
*/

{
	if (m_pSegment) m_pSegment->Release();

	DMUS_OBJECTDESC ObjectDescript;
	ObjectDescript.guidClass = CLSID_DirectMusicSegment;
	wcscpy(ObjectDescript.wszFileName, L"BoidsD.sgt");
	ObjectDescript.dwValidData = DMUS_OBJ_CLASS | DMUS_OBJ_FILENAME ;
	ObjectDescript.dwSize = sizeof(DMUS_OBJECTDESC);
	if (SUCCEEDED(m_pLoader->GetObject(&ObjectDescript, IID_IDirectMusicSegment, (void**)&m_pSegment)))
	{

        return TRUE;
	}

    return FALSE;
}

BOOL BoidMusic::LoadDLS()

/*	Load the dls set that the band needs, but just leave it in the cache.
	Then, it will be referenced and loaded by the bands in the styles and segments.
*/

{
	DMUS_OBJECTDESC ObjectDescript;
	ObjectDescript.guidClass = CLSID_DirectMusicCollection;
	wcscpy(ObjectDescript.wszFileName, L"Boids.dls");
	ObjectDescript.dwValidData = DMUS_OBJ_CLASS | DMUS_OBJ_FILENAME ;
	ObjectDescript.dwSize = sizeof(DMUS_OBJECTDESC);
	IDirectMusicCollection* pCollect = NULL;
	if (SUCCEEDED(m_pLoader->GetObject(&ObjectDescript, IID_IDirectMusicCollection, (void**)&pCollect)))
	{
		pCollect->Release();
        return TRUE;
	}

    return FALSE;
}


BOOL BoidMusic::LoadChordMap()

/*	Load the Boids chord map.
*/

{
	if (m_pChordMap) m_pChordMap->Release();

	DMUS_OBJECTDESC ObjectDescript;
	ObjectDescript.guidClass = CLSID_DirectMusicChordMap;
	wcscpy(ObjectDescript.wszFileName, L"Boids.cdm");
	ObjectDescript.dwValidData = DMUS_OBJ_CLASS | DMUS_OBJ_FILENAME;
	ObjectDescript.dwSize = sizeof(DMUS_OBJECTDESC);
	if (SUCCEEDED(m_pLoader->GetObject(&ObjectDescript, IID_IDirectMusicChordMap, (void**)&m_pChordMap)))
	{
        return TRUE;
	}

    return FALSE;
}

BOOL BoidMusic::LoadTemplate(DWORD dwIndex, WCHAR * pwzName)

/*	Load a template and place it in the m_pTemplateSegments[] array.
*/

{ 	
	if (m_pTemplateSegments[dwIndex]) m_pTemplateSegments[dwIndex]->Release();

	DMUS_OBJECTDESC ObjectDescript;

	ObjectDescript.guidClass = CLSID_DirectMusicSegment;
	wcscpy(ObjectDescript.wszFileName, pwzName);
	ObjectDescript.dwValidData = DMUS_OBJ_CLASS | DMUS_OBJ_FILENAME;
	ObjectDescript.dwSize = sizeof(DMUS_OBJECTDESC);
	if (SUCCEEDED(m_pLoader->GetObject(&ObjectDescript, IID_IDirectMusicSegment, (void**)&m_pTemplateSegments[dwIndex])))
	{
        return TRUE;
	}

    return FALSE;
}

void BoidMusic::ComposeSegment(DWORD dwIndex)

/*	Use the composer to compose a style segment from a previously loaded
	template segment, the boids chord map and style. Place the
	segment in the m_pPrimarySegments[] array.
*/

{ 	

	if (m_pComposer && m_pStyle && m_pChordMap && m_pTemplateSegments[dwIndex])
	{
		if (m_pPrimarySegments[dwIndex])
		{
			m_pPrimarySegments[dwIndex]->Release();
			m_pPrimarySegments[dwIndex] = NULL;
		}
		m_pComposer->ComposeSegmentFromTemplate(
			m_pStyle,
			m_pTemplateSegments[dwIndex],
			1,
			m_pChordMap,
			&m_pPrimarySegments[dwIndex]);
		if (m_pPrimarySegments[dwIndex])
		{
			m_pPrimarySegments[dwIndex]->SetRepeats(100);

		}
	}
}

BOOL BoidMusic::GetMotif(DWORD dwIndex, WCHAR * pszName)

/*	Pull the named motif segment from the boids style.
	Note that we currently use the OLE BSTR string approach.
	This will probably change in the next beta.
*/

{ 
	if (m_pStyle)
	{	
		if (SUCCEEDED(m_pStyle->GetMotif(pszName, &m_pMotifSegments[dwIndex])))
        {
            return TRUE;
        }
	}

    return FALSE;
}

void BoidMusic::HandleNotifies()

/*	Check for a notify event. Since we are only waiting for
	notification of beats, we increment out beat counter. 
	The beat counter is used by the boids app to manage the
	number of motifs allowed at any point in time. This 
	keeps things from getting too overcrowded.

    Alternatively, the notifications could be used to synchronize
	graphic events.
*/

{
	DMUS_NOTIFICATION_PMSG* pMsg;
	if( m_hNotify && m_pPerformance)
	{
		WaitForSingleObject(m_hNotify, 2);
		while( S_OK == m_pPerformance->GetNotificationPMsg(&pMsg))
		{
			m_pPerformance->FreePMsg((DMUS_PMSG*)pMsg);
			m_dwBeatsSinceLastMotif++;
		}
	}
}


void BoidMusic::StartMusic()

/*	This is called by the app, once it gets its main window up.
	Activate the synth and start the music playing.
*/

{
	if (m_pDMusic)
	{
		m_pDMusic->Activate(TRUE);
	}
	if (m_pPrimarySegments[0])
	{
		m_pPerformance->PlaySegment( 
			m_pPrimarySegments[0], 0, 0, NULL);
		m_dwIndex = 0;
	}	
}

void BoidMusic::SetDistance(double fDistance)

/*	This is called from the graphics loop. As we get closer
	to the center of the flock of birds, increase the number
	of echoes for the echo tool, and also increase the
	duration scalings (makes the note durations longer.)
*/

{
	double fTemp = fDistance - 10.0;
	fTemp = 70.0 - fTemp;
	if (fTemp < 0.0) fTemp = 0.0;
	m_Tool.m_dwEcho = (DWORD)(fTemp / 15.0);
	m_Tool.m_dwStartRatio = (DWORD)(75 + fTemp);
	m_Tool.m_dwRatio = m_Tool.m_dwStartRatio;
}

void BoidMusic::Transition()

/*	Whenever the flock is about to hit a sphere, this is called.
	It chooses the next segment to go to and plays the planet motif
	leading into the next segment. Notice how it takes the start time
	of the planet motif, adds one, and uses that to start the segment,
	but on a beat boundary. This tricks the segment to start one beat
	later than the motif, creating a smooth musical transition.
*/

{
	if (!m_fCollapsed && (m_dwBeatsSinceLastMotif > 2))
	{
		static DWORD dwChoice[8] = { 0, 2, 1, 3, 0, 4, 1, 5 };
		m_dwIndex++;
		if (m_dwIndex > 8) m_dwIndex = 2;
		if (m_dwIndex > 0) 
		{
			if (m_pMotifSegments[0] && m_pPrimarySegments[dwChoice[m_dwIndex-1]]) 
			{
				IDirectMusicSegmentState *pState = NULL;
				m_pPerformance->PlaySegment(
					m_pMotifSegments[0], (DMUS_SEGF_SECONDARY | DMUS_SEGF_BEAT ), 0, &pState);
				if (pState)
				{
					MUSIC_TIME mtTime;
					pState->GetStartTime(&mtTime);
					pState->Release();
					mtTime++;
					m_pPerformance->PlaySegment( 
							m_pPrimarySegments[dwChoice[m_dwIndex-1]], DMUS_SEGF_BEAT , mtTime, NULL);
				}
			}
		}
	}
}

void BoidMusic::Migrate()

/*	When the migration force is turned off, the birds start to 
	migrate away. So, this starts the migration theme.
	Note that it plays once, and then one of the regular segments
	is queued to pick up after it.
*/

{
	if (m_pSegment) 
	{
		m_pPerformance->PlaySegment( 
					m_pSegment, DMUS_SEGF_BEAT , 0, NULL);
	}
	if (m_pPrimarySegments[2])
	{
		m_pPerformance->PlaySegment( 
					m_pPrimarySegments[2], DMUS_SEGF_QUEUE , 0, NULL);
	}
}

void BoidMusic::Collapse()

/*	When the 's' key or space bar is hit, the birds collapse
	together. Set the collapsed flag indicating the new state.
	Trigger two motifs to play. The first simply plays the
	collapse motif. The second plays a cycling motif until the
	user lifts up, calling the Expand() function. 
*/

{
	m_fCollapsed = TRUE;
	if (m_dwBeatsSinceLastMotif)
	{
		if (m_pMotifSegments[5])
		{
			m_pPerformance->PlaySegment(
				m_pMotifSegments[5], (DMUS_SEGF_SECONDARY | DMUS_SEGF_GRID), 0, NULL);
		}
		if (m_pMotifSegments[3])
		{
			m_pMotifSegments[3]->SetRepeats(20);
			m_pPerformance->PlaySegment(
				m_pMotifSegments[3], (DMUS_SEGF_SECONDARY | DMUS_SEGF_MEASURE), 0, NULL);
		}
		m_dwBeatsSinceLastMotif = 0;
	}
}

void BoidMusic::Expand()

/*	When the user releases the 's' key or space bar, the birds
	can move apart again. Notice that the repeating motif is
	turned off by calling stop on it.
*/

{
	m_fCollapsed = FALSE;
	if (m_dwBeatsSinceLastMotif)
	{
		if (m_pMotifSegments[4])
		{
			m_pPerformance->PlaySegment(
				m_pMotifSegments[4], (DMUS_SEGF_SECONDARY | DMUS_SEGF_GRID), 0, NULL);
		}
		m_dwBeatsSinceLastMotif = 0;
	}
	if (m_pMotifSegments[3])
	{
		m_pPerformance->Stop(m_pMotifSegments[3], NULL, 0, 0);
	}
}

void BoidMusic::EndMusic()

/*	Close down all the music stuff.
*/

{
	m_Tool.m_pPerformance = NULL;
	if (m_pPerformance)
	{
		m_pPerformance->Stop( NULL, NULL, 0, 0);
	}
	if (m_pSegment)
	{
		m_pSegment->Release();
		m_pSegment = NULL;
	}
	if (m_pBand)
	{
		if(m_pPerformance)
		{
			m_pBand->Unload(m_pPerformance);
		}
		m_pBand->Release();
		m_pBand = NULL;
	}
	if (m_pPort)
	{
		m_pPort->Release();
		m_pPort = NULL;
	}
	if (m_pDMusic)
	{
		m_pDMusic->Release();
		m_pDMusic = NULL;
	}
	if (m_pGraph)
	{
		m_pGraph->Release();
		m_pGraph = NULL;
	}
	long x;
	for (x = 0; x< 6; x++)
	{
		if (m_pTemplateSegments[x])
		{
			m_pTemplateSegments[x]->Release();
			m_pTemplateSegments[x] = NULL;
		}
		if (m_pPrimarySegments[x])
		{
			m_pPrimarySegments[x]->Release();
			m_pPrimarySegments[x] = NULL;
		}
		if (m_pMotifSegments[x])
		{
			m_pMotifSegments[x]->Release();
			m_pMotifSegments[x] = NULL;
		}
	}
	if (m_pStyle)
	{
		m_pStyle->Release();
		m_pStyle = NULL;
	}
	if (m_pChordMap)
	{
		m_pChordMap->Release();
		m_pChordMap = NULL;
	}
	if (m_pTransitionSegment)
	{
		m_pTransitionSegment->Release();
		m_pTransitionSegment = NULL;
	}
	if (m_pComposer)
	{
		m_pComposer->Release();
		m_pComposer = NULL;
	}
	if (m_pLoader)
	{
		m_pLoader->Release();
		m_pLoader = NULL;
	}
	if (m_pPerformance)
	{
		m_pPerformance->Stop( NULL, NULL, 0, 0);
		m_pPerformance->CloseDown();
		m_pPerformance->Release();
		m_pPerformance = NULL;
	}
	CoUninitialize();
}

static char szDirectMusicMedia[] = "\\DMusic\\Media";

BOOL BoidMusic::GetSearchPath(WCHAR wszPath[MAX_PATH])
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

/*	Tool implementation follows. Pretty much all the meat is in the
	ProcessEvent() method.
*/

CTool::CTool()

{
	m_dwRatio = 100;
	m_dwStartRatio = 100;
	m_dwEcho = 1;
	m_cRef = 0;
	m_dwDelay = 96;
	m_pPerformance = NULL;
}

CTool::~CTool()

{
}


STDMETHODIMP CTool::QueryInterface(
    const IID &iid,   
    void **ppv)       
{
	if (iid == IID_IDirectMusicTool)
	{
        *ppv = static_cast<IDirectMusicTool*>(this);
	} else
    {
        *ppv = NULL;
        return E_NOINTERFACE;
    }

    reinterpret_cast<IUnknown*>(this)->AddRef();
    return S_OK;
}

STDMETHODIMP_(ULONG) CTool::AddRef()
{
    return InterlockedIncrement(&m_cRef);
}


STDMETHODIMP_(ULONG) CTool::Release()
{
    if (!InterlockedDecrement(&m_cRef))
    {
        return 0;
    }

    return m_cRef;
}

HRESULT STDMETHODCALLTYPE CTool::ProcessPMsg(
	IDirectMusicPerformance* pPerf, 
	DMUS_PMSG* pMsg) 
{
	HRESULT hr = S_OK;
	if (pMsg->pGraph)
	{
		if (SUCCEEDED(pMsg->pGraph->StampPMsg(pMsg)))
		{
			hr = DMUS_S_REQUEUE;
		}
	}
	switch( pMsg->dwType )
	{
	case DMUS_PMSGT_NOTE:
		{
			/*	If we receive a note, first adjust the duration
				by m_dwRatio.
			*/
			DMUS_NOTE_PMSG *pNote = (DMUS_NOTE_PMSG *) pMsg;
			if (m_dwRatio < 1) m_dwRatio = 1;
			if (m_dwRatio > 400) m_dwRatio = 400;
			pNote->mtDuration = MulDiv(pNote->mtDuration,m_dwRatio,100);
			/*	Then, if it is on the PChannel of the melody,
				set it to echo the number of times established
				by m_dwEcho. Note that the echo algorithm is 
				quite simple but powerful: copy the event and play
				it on an adjacent PChannel. By playing it on a different
				PChannel, we take care of overlapping notes and
				we can have it panned differently.
			*/
			if (m_dwEcho && (pNote->dwPChannel == 7))
			{
				if (pPerf)
				{
					DWORD dwX;
					if (m_dwEcho > 3) m_dwEcho = 3;
					for (dwX = 0; dwX < m_dwEcho; dwX++)
					{
						DMUS_NOTE_PMSG * pEcho;
						if (SUCCEEDED(pPerf->AllocPMsg( sizeof(DMUS_NOTE_PMSG),
							(DMUS_PMSG**) &pEcho)))
						{
							static DWORD dwChannels[3] = { 6, 15, 0 };
							memcpy( pEcho, pNote, sizeof(DMUS_NOTE_PMSG) );
							pEcho->dwPChannel = dwChannels[dwX];
							pEcho->mtTime = pNote->mtTime + (m_dwDelay * (dwX + 1));
							pEcho->dwFlags = DMUS_PMSGF_MUSICTIME;
							if (pEcho->pGraph) pEcho->pGraph->AddRef();
							if (pEcho->pTool) pEcho->pTool->AddRef();
							pPerf->SendPMsg( (DMUS_PMSG*)pEcho) ;
						}
					}
				}
			}
		}
		break;
	case DMUS_PMSGT_CURVE:
		break;
	case DMUS_PMSGT_SYSEX:
		break;
	case DMUS_PMSGT_MIDI:
		/*	Also, if we receive program changes, volumes, or pans on the 
			PChannel of the melody, set up the echos to use the same patch,
			a diminishing volume, and assign the pans appropriately.
			This way, if a different band with different setting is loaded,
			the echoed parts will all get reset appropriately.
		*/
		{
			DMUS_MIDI_PMSG *pMidi = (DMUS_MIDI_PMSG *) pMsg;
			if (pMidi->dwPChannel == 7)
			{
				DWORD dwX;
				for (dwX = 0;dwX < 3; dwX++)
				{
					DMUS_MIDI_PMSG * pEcho;
					if (SUCCEEDED(m_pPerformance->AllocPMsg( sizeof(DMUS_MIDI_PMSG),
						(DMUS_PMSG**) &pEcho)))
					{
						static DWORD dwChannels[3] = { 6, 15, 0 };
						static BYTE bPans[3] = { 80,0,120 };
						memcpy( pEcho, pMidi, sizeof(DMUS_MIDI_PMSG) );
						pEcho->dwPChannel = dwChannels[dwX];
						pEcho->mtTime = pMsg->mtTime + 20;
						pEcho->dwFlags = DMUS_PMSGF_MUSICTIME;
						if (pMidi->bStatus == 0xB0)
						{
							if (pMidi->bByte1 == 10)
							{
								pEcho->bByte2 = bPans[dwX];
							}
							else if (pMidi->bByte1 == 7)
							{
								pEcho->bByte2 = pMidi->bByte2 - (((BYTE)dwX+1) * 20);
								if (pEcho->bByte2 > 127) pEcho->bByte2 = 10;
							}
						}
						if (pEcho->pGraph) pEcho->pGraph->AddRef();
						if (pEcho->pTool) pEcho->pTool->AddRef();
						m_pPerformance->SendPMsg( (DMUS_PMSG*)pEcho) ;
					}
				}
			}
		}
		break;
	case DMUS_PMSGT_PATCH:
		break;
	default:
		break;
	}

	return hr;
}

HRESULT STDMETHODCALLTYPE CTool::Flush(
	IDirectMusicPerformance* pPerf,
	DMUS_PMSG* pMsg,
    REFERENCE_TIME rt
	)
{
	return S_OK;
}


// IDirectMusicTool
HRESULT STDMETHODCALLTYPE CTool::Init(
		/* [in] */  IDirectMusicGraph* pGraph
	)
{
	return E_NOTIMPL;
}

HRESULT STDMETHODCALLTYPE  CTool::GetMsgDeliveryType(
	DWORD* pdwDeliveryType ) 
{
	return E_NOTIMPL;
}

HRESULT STDMETHODCALLTYPE  CTool::GetMediaTypeArraySize(DWORD* pdwNumElements ) 

{
	return E_NOTIMPL;
}

HRESULT STDMETHODCALLTYPE  CTool::GetMediaTypes(DWORD** padwMediaTypes, DWORD dwNumElements) 

{
	return E_NOTIMPL;
}

BOOL BoidMusic::LoadMusic(HWND hwnd)
{
    HRESULT hr;
       
	CoInitialize(NULL);
	hr = CoCreateInstance(
			CLSID_DirectMusicLoader,
			NULL,
			CLSCTX_INPROC, 
			IID_IDirectMusicLoader,
			(void**)&m_pLoader);
    if (FAILED(hr)) 
    {
        OutputDebugString("Couldn't create CLSID_DirectMusicLoader\n");
        return FALSE;
    }

	WCHAR dir[MAX_PATH];
	
	m_pLoader->EnableCache(GUID_DirectMusicAllTypes, TRUE);

    hr = E_FAIL;
    if (BoidMusic::GetSearchPath(dir))
    {
	    hr = m_pLoader->SetSearchDirectory(GUID_DirectMusicAllTypes, dir, FALSE);
    }
    if (FAILED(hr))
    {
	    hr = m_pLoader->SetSearchDirectory(GUID_DirectMusicAllTypes, L".", FALSE);
    }
    if (FAILED(hr))
    {
        OutputDebugString("Couldn't set DirectMusic search path");
        return FALSE;
    }

    hr = ::CoCreateInstance(
			CLSID_DirectMusicComposer,
			NULL,
			CLSCTX_INPROC, 
			IID_IDirectMusicComposer,
			(void**)&m_pComposer);
    if (FAILED(hr)) 
    {
        OutputDebugString("Couldn't create CLSID_DirectMusicComposer\n");
        return FALSE;
    }

    BOOL bResourcesOk = TRUE;

	bResourcesOk = bResourcesOk && LoadDLS();
	bResourcesOk = bResourcesOk && LoadStyle();
	bResourcesOk = bResourcesOk && LoadChordMap();
	bResourcesOk = bResourcesOk && LoadTemplate(0,L"BoidsAA.tpl");
	bResourcesOk = bResourcesOk && LoadTemplate(1,L"BoidB1.tpl");
	bResourcesOk = bResourcesOk && LoadTemplate(2,L"BoidsC1.tpl");
	bResourcesOk = bResourcesOk && LoadTemplate(3,L"BoidsCA.tpl");
	bResourcesOk = bResourcesOk && LoadTemplate(4,L"BoidsCB.tpl");

	bResourcesOk = bResourcesOk && LoadTemplate(5,L"BoidsCC.tpl");
	
	ComposeSegment(0);
	ComposeSegment(1);
	ComposeSegment(2);
	ComposeSegment(3);
	ComposeSegment(4);
	ComposeSegment(5);

	bResourcesOk = bResourcesOk && LoadSegment();
	bResourcesOk = bResourcesOk && GetMotif(0, L"planet");
	bResourcesOk = bResourcesOk && GetMotif(1, L"D motif");
	bResourcesOk = bResourcesOk && GetMotif(2, L"motif RL med");
	bResourcesOk = bResourcesOk && GetMotif(3, L"Cycle");
	bResourcesOk = bResourcesOk && GetMotif(4, L"Expand");
	bResourcesOk = bResourcesOk && GetMotif(5, L"Collapse");

    if (!bResourcesOk)
    {
        OutputDebugString("Couldn't load DirectMusic resources\n");
        return FALSE;
    }

	hr = CoCreateInstance(CLSID_DirectMusic,
						  NULL,
						  CLSCTX_INPROC_SERVER,
						  IID_IDirectMusic,
						  (LPVOID*)&m_pDMusic);
    if (FAILED(hr))
    {
        OutputDebugString("Couldn't create CLSID_DirectMusic\n");
        return FALSE;
    }

    hr = m_pDMusic->SetDirectSound(NULL, hwnd);
    if (FAILED(hr))
    {
        OutputDebugString("Could't SetDirectSound on IDirectMusic\n");
        return FALSE;
    }

	DMUS_PORTPARAMS dmos;
	DMUS_PORTCAPS dmpc;
	GUID guidSynthGUID;
	
	hr = m_pDMusic->GetDefaultPort(&guidSynthGUID);
	if (FAILED(hr))
	{
		OutputDebugString("Could't GetDefaultPort on IDirectMusic\n");
		return FALSE;
	}

	ZeroMemory(&dmos, sizeof(dmos));
	dmos.dwSize = sizeof(DMUS_PORTPARAMS);
	dmos.dwChannelGroups = 1;
	dmos.dwValidParams = DMUS_PORTPARAMS_CHANNELGROUPS;

	hr = m_pDMusic->CreatePort(guidSynthGUID,
								&dmos,
								&m_pPort,
								NULL);
	if (FAILED(hr))
	{
		OutputDebugString("Couldn't CreatePort on IDirectMusic\n");
		return FALSE;
	}

	ZeroMemory(&dmpc, sizeof(dmpc));
	dmpc.dwSize = sizeof(DMUS_PORTCAPS);

	hr = m_pPort->GetCaps(&dmpc);
	if (FAILED(hr))
	{
		OutputDebugString("Couldn't GetCaps on IDirectMusicPort\n");
		return FALSE;
	}

	if ((dmpc.dwClass != DMUS_PC_OUTPUTCLASS) || !(dmpc.dwFlags & DMUS_PC_DLS))
	{
		m_pPort->Release();
		m_pPort = NULL;
	}

	if (!m_pPort)
	{
		hr = E_FAIL;

		for (DWORD index = 0; ; index++)
		{
			ZeroMemory(&dmpc, sizeof(dmpc));
			dmpc.dwSize = sizeof(DMUS_PORTCAPS);

			hr = m_pDMusic->EnumPort(index, &dmpc);
			if(SUCCEEDED(hr) && hr != S_FALSE)
			{
				if ( (dmpc.dwClass == DMUS_PC_OUTPUTCLASS) && 
					 (dmpc.dwFlags & DMUS_PC_DLS) )
				{
					CopyMemory(&guidSynthGUID, &dmpc.guidPort, sizeof(GUID));
					
					ZeroMemory(&dmos, sizeof(dmos));
					dmos.dwSize = sizeof(DMUS_PORTPARAMS);
					dmos.dwChannelGroups = 1;
					dmos.dwValidParams = DMUS_PORTPARAMS_CHANNELGROUPS;

					hr = m_pDMusic->CreatePort(guidSynthGUID,
												&dmos,
												&m_pPort,
												NULL);
					break;
				}
			}
			else
			{
				break;
			}
		}
	}

	if (FAILED(hr))
	{
		OutputDebugString("Couldn't find or CreatePort software synthesizer\n");
		return FALSE;
	}

	hr = CoCreateInstance(CLSID_DirectMusicPerformance, 
    	        		  NULL, 
				          CLSCTX_ALL, 
				          IID_IDirectMusicPerformance,
				          (void**)&m_pPerformance);
    if (FAILED(hr))
    {
        OutputDebugString("Couldn't create CLSID_DirectMusicPerformance\n");
        return FALSE;
    }

    hr = m_pPerformance->Init(&m_pDMusic, NULL, NULL);
    if (FAILED(hr))
    {
        OutputDebugString("Couldn't init CLSID_DirectMusicPerformance\n");
        return FALSE;
    }

	m_pPerformance->AddPort(m_pPort );
	m_pPerformance->AssignPChannelBlock(0, m_pPort, 1);
	m_hNotify = CreateEvent( NULL, FALSE, FALSE, NULL );
	if( m_hNotify )
	{
		GUID guid;
		m_pPerformance->SetNotificationHandle( m_hNotify, 0 );
		guid = GUID_NOTIFICATION_MEASUREANDBEAT;
		m_pPerformance->AddNotificationType( guid );
	}
	m_Tool.m_pPerformance = m_pPerformance;
	if (m_pBand)
	{
		m_pBand->Download(m_pPerformance);
	}

	hr = ::CoCreateInstance(CLSID_DirectMusicGraph,
						    NULL,
						    CLSCTX_INPROC, 
						    IID_IDirectMusicGraph,
						    (void**)&m_pGraph);
    if (FAILED(hr))
    {
        OutputDebugString("Couldn't create CLSID_DirectMusicGraph\n");
        return FALSE;
    }

	m_pPerformance->SetGraph(m_pGraph);
	IDirectMusicTool *pTool;
	hr = m_Tool.QueryInterface(IID_IDirectMusicTool, (void**)&pTool);
	if (SUCCEEDED(hr))
	{
		m_pGraph->InsertTool( pTool, 0, NULL, 0);
	}
    else
    {
        OutputDebugString("Couldn't InsertTool on graph\n");
        return FALSE;
    }

    return TRUE;
}


