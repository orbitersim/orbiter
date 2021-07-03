//
// MLMusic.cpp
//
// Music logic for MusicLines
//
// Copyright (c) 1998-1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------
#include <windows.h>
#include "Debug.h"
#include <objbase.h>
#include <dmusici.h>

#include "MLMusic.h"
#include "DMHelper.h"

long gcMoveCount = 0;
DWORD gdwTime = 0;
DWORD gdwIntensity = 0;
#define MAX_INTENSITY 8

Music::Music()
{
	m_pPerf = NULL;
	m_pLoader = NULL;
	for( int x = 0; x < NUM_TEMPLATES; x++ )
	{
		m_apTemplate[x] = NULL;
	}
	for( x = 0; x < NUM_MAINSEGS; x++ )
	{
		m_apMainSeg[x] = NULL;
		m_pDeathSeg[x] = NULL;
		m_pEndSeg[x] = NULL;
	}
	m_pIntroSeg = NULL;
	for( x = 0; x < NUM_PLAYERS; x++ )
	{
		m_pUpSeg[x] = NULL;
		m_pDownSeg[x] = NULL;
		m_pLeftSeg[x] = NULL;
		m_pRightSeg[x] = NULL;
		m_pDirSegState[x] = NULL;
	}
	m_pWinSeg = NULL;
	m_fPlayingIntro = FALSE;
	m_cRef = 1; // set to 1 so one call to Release() will free this
}

Music::~Music()
{
	if( m_pPerf )
	{
		UnloadInstruments();
		m_pPerf->CloseDown();
	}
	RELEASE(m_pPerf);

	for( int x = 0; x < NUM_TEMPLATES; x++ )
	{
		RELEASE(m_apTemplate[x]);
	}
	for( x = 0; x < NUM_MAINSEGS; x++ )
	{
		RELEASE(m_apMainSeg[x]);
		RELEASE(m_pEndSeg[x]);
		RELEASE(m_pDeathSeg[x]);
	}
	RELEASE(m_pComposer);
	RELEASE(m_pIntroSeg);
	for( x = 0; x < NUM_PLAYERS; x++ )
	{
		RELEASE(m_pUpSeg[x]);
		RELEASE(m_pDownSeg[x]);
		RELEASE(m_pLeftSeg[x]);
		RELEASE(m_pRightSeg[x]);
		RELEASE(m_pDirSegState[x]);
	}
	RELEASE(m_pWinSeg);

	if( m_pLoader )
	{
		m_pLoader->ClearCache(GUID_DirectMusicAllTypes);
	}
	RELEASE(m_pLoader);
}

void Music::UnloadInstruments()
{
	int iCount;

	// Unload all previously downloaded instruments.
	if( m_pIntroSeg )
	{
		m_pIntroSeg->SetParam( GUID_Unload, 0xffffffff, 0, 0, (void*)m_pPerf );
	}
	for( iCount = 0; iCount < NUM_MAINSEGS; iCount++ )
	{
		if( m_apMainSeg[iCount] )
		{
			m_apMainSeg[iCount]->SetParam( GUID_Unload, 0xffffffff, 0, 0, (void*)m_pPerf );
		}
	}
}

void Music::DownloadInstruments()
{
	int iCount;

	// Download the instruments for all of the primary segments.
	// The end segments and secondary segments use the same instruments,
	// so there is no need to download those individually.
	if( m_pIntroSeg )
	{
		m_pIntroSeg->SetParam( GUID_Download, 0xffffffff, 0, 0, (void*)m_pPerf );
	}
	for( iCount = 0; iCount < NUM_MAINSEGS; iCount++ )
	{
		if( m_apMainSeg[iCount] )
		{
			m_apMainSeg[iCount]->SetParam( GUID_Download, 0xffffffff, 0, 0, (void*)m_pPerf );
		}
	}
}

BOOL Music::Initialize(HWND hWnd)
{
	if( m_pPerf )
	{
		return FALSE; // already initialized
	}
	m_pPerf = CreatePerformance();
	if (!m_pPerf)
	{
        Trace(0, "Performance was not created");
		return FALSE;
	}

	if (FAILED(m_pPerf->Init(NULL, NULL, hWnd)))
	{
        Trace(0, "Could not create initialize performance");
		m_pPerf->Release();
		return FALSE;
	}

	if( SUCCEEDED( m_pPerf->AddPort(NULL)) )
	{
		IDirectMusicGraph* pGraph = CreateGraph();
		if( pGraph )
		{
			DWORD adwPChannels[1];
			adwPChannels[0] = 15;
			pGraph->InsertTool( this, NULL, 1, 0 );
			m_pPerf->SetGraph(pGraph);
			pGraph->Release();

			m_pLoader = CreateLoader();
			if( m_pLoader )
			{
				m_pComposer = CreateComposer();
				if( m_pComposer )
				{
                    m_pIntroSeg = CreateSegmentFromFile( m_pLoader, L"IntroCR.sgt" );
					if( m_pIntroSeg )
					{
						m_pIntroSeg->SetRepeats(-1); // set repeats to infinite
					}
					m_apTemplate[0] = CreateSegmentFromFile( m_pLoader, L"CartoonRag.tpl" );
					m_apMainSeg[0] = CreateSegmentFromTemplate( m_pLoader, m_apTemplate[0],
						m_pComposer, L"CartoonRag.sty", L"CartoonRag.cdm" );
					m_apMainSeg[2] = CreateSegmentFromTemplate( m_pLoader, m_apTemplate[0],
						m_pComposer, L"CartoonRag.sty", L"CartoonRag.cdm" );

					m_apTemplate[1] = CreateSegmentFromFile( m_pLoader, L"Acidtek.tpl" );
					m_apMainSeg[1] = CreateSegmentFromTemplate( m_pLoader, m_apTemplate[1],
						m_pComposer, L"Acidtek.sty", L"Acidtek.cdm" );
					m_apMainSeg[3] = CreateSegmentFromTemplate( m_pLoader, m_apTemplate[1],
						m_pComposer, L"Acidtek.sty", L"Acidtek.cdm" );
					for( int i = 0; i < NUM_MAINSEGS; i++ )
					{
						if( m_apMainSeg[i] )
						{
							m_apMainSeg[i]->SetRepeats(-1); // set repeats to infinite
						}
					}
					
					m_apTemplate[2] = CreateSegmentFromFile( m_pLoader, L"CartoonEnd.tpl" );
					m_apTemplate[3] = CreateSegmentFromFile( m_pLoader, L"AcidEnd.tpl" );
					m_pDeathSeg[0] = GetMotif( m_pLoader, L"CartoonRag.sty", L"Explosion" );
					m_pDeathSeg[1] = GetMotif( m_pLoader, L"CartoonRag.sty", L"Explosion" );
					m_pDeathSeg[2] = GetMotif( m_pLoader, L"Acidtek.sty", L"Explosion" );
					m_pDeathSeg[3] = GetMotif( m_pLoader, L"Acidtek.sty", L"Explosion" );
					m_pEndSeg[0] = CreateSegmentFromTemplate( m_pLoader, m_apTemplate[2],
						m_pComposer, L"CartoonRag.sty", L"CartoonRag.cdm" );
					m_pEndSeg[2] = CreateSegmentFromTemplate( m_pLoader, m_apTemplate[2],
						m_pComposer, L"CartoonRag.sty", L"CartoonRag.cdm" );
					m_pEndSeg[1] = CreateSegmentFromTemplate( m_pLoader, m_apTemplate[2],
						m_pComposer, L"Acidtek.sty", L"Acidtek.cdm" );
					m_pEndSeg[3] = CreateSegmentFromTemplate( m_pLoader, m_apTemplate[2],
						m_pComposer, L"Acidtek.sty", L"Acidtek.cdm" );
				}
			} 
            else
            {
                MessageBox(NULL,
                           "SDK not properly installed.",
                           "MusicLines",
                           MB_OK | MB_ICONEXCLAMATION);

            }
		}
	}
	else
	{
		Trace(0, "Could not add port to performance");
	}

	if( m_apMainSeg[0] )
	{
		// assume if the main segment loaded that all the files were loaded.
		// Download dls instruments to the performance.
		DownloadInstruments();
		return TRUE;
	}
	return FALSE;
}

// The game logic calls:
// Play( StartLevel, 0 );			at the beginning of the level
// Play( <direction>, <player #> ); when a player makes a move
// Play( Death, <player #> );		when a player dies but there is no winner
// Play( Win, <player #> );			when a player wins by having the longest trail
// Play( EndLevel, 0 );				when both players have died and the level is over
void Music::Play( PlayEvent playEvent, int PlayerNo )
{
	DWORD dwSegF;
	IDirectMusicSegment* pSeg = NULL;
	static int siCount = NUM_MAINSEGS; // wrap around counter for main music

	if( (Up == playEvent) ||
		(Down == playEvent) ||
		(Left == playEvent) ||
		(Right == playEvent) )
	{
		if( m_fPlayingIntro )
		{
			Trace(0, "Switch from intro to real music");

			// if the intro is playing and a player moves,
			// switch to main music.
			// Cue the main music
			siCount++; // so a new main segment plays each time
			if( siCount >= NUM_MAINSEGS ) siCount = 0;
			m_pPerf->PlaySegment( m_apMainSeg[siCount], DMUS_SEGF_DEFAULT, 0, NULL );
			m_fPlayingIntro = FALSE;
		}
	}
	// Currently we're not paying attention to player number, but
	// we could code this in if desired.
	switch( playEvent )
	{
	case StartLevel:
		if( FALSE == m_fPlayingIntro )
		{
			pSeg = m_pIntroSeg;
			dwSegF = DMUS_SEGF_QUEUE;
			m_fPlayingIntro = TRUE;
		}
		gcMoveCount = 0;
		break;

	case EndLevel:
		pSeg = m_pEndSeg[siCount];
		dwSegF = DMUS_SEGF_MEASURE | DMUS_SEGF_NOINVALIDATE;
		break;

	case Death:
		pSeg = m_pDeathSeg[siCount];
		dwSegF = DMUS_SEGF_DEFAULT | DMUS_SEGF_SECONDARY;
		break;
	}
	if( pSeg )
	{
		m_pPerf->PlaySegment( pSeg, dwSegF, 0, NULL );
	}
}

/*  IUnknown */
STDMETHODIMP Music::QueryInterface(const IID &iid, void **ppv)
{
	if (iid == IID_IUnknown || iid == IID_IDirectMusicTool)
	{
		*ppv = static_cast<IDirectMusicTool*>(this);
	}
	else
	{
		*ppv = NULL;
		return E_NOINTERFACE;
	}

	reinterpret_cast<IUnknown*>(this)->AddRef();
	return S_OK;
}

STDMETHODIMP_(ULONG) Music::AddRef()
{
    return InterlockedIncrement(&m_cRef);
}

STDMETHODIMP_(ULONG) Music::Release()
{
    if( 0 == InterlockedDecrement(&m_cRef) )
    {
        delete this;
        return 0;
    }
    return m_cRef;
}

/*  IDirectMusicTool */
STDMETHODIMP Music::Init                 (THIS_ IDirectMusicGraph* pGraph)
{
	// This tool has no need to do any type of initialization.
	return E_NOTIMPL;
}

STDMETHODIMP Music::GetMsgDeliveryType   (THIS_ DWORD* pdwDeliveryType )
{
	// This tool wants messages immediately.
	// This is the default, so returning E_NOTIMPL
	// would work. The other method is to specifically
	// set *pdwDeliveryType to the delivery type, DMUS_PMSGF_IMMEDIATE,
	// DMUS_PMSGF_QUEUE, or DMUS_PMSGF_ATTIME.

	*pdwDeliveryType = DMUS_PMSGF_TOOL_IMMEDIATE;
	return S_OK;
}

STDMETHODIMP Music::GetMediaTypeArraySize(THIS_ DWORD* pdwNumElements )
{
    Trace(0,"GetMediaTypeArraySize");
	// This tool wants control changes (MIDI messages) and curves

	*pdwNumElements = 2;
	return S_OK;
}

STDMETHODIMP Music::GetMediaTypes        (THIS_ DWORD** padwMediaTypes, DWORD dwNumElements)
{
	// Fill in the array padwMediaTypes with the type of
	// messages this tool wants to process. In this case,
	// dwNumElements will be 2, since that is what this
	// tool returns from GetMediaTypeArraySize().

	if( dwNumElements == 2 )
	{
		// set the elements in the array to DMUS_PMSGT_MIDI
		// so we get control changes
		(*padwMediaTypes)[0] = DMUS_PMSGT_MIDI;
		// set DMUS_PMSGT_CURVE so we get curves
		(*padwMediaTypes)[1] = DMUS_PMSGT_CURVE;
		return S_OK;
	}
	else
	{
		// this should never happen
		return E_FAIL;
	}
}

STDMETHODIMP Music::ProcessPMsg          (THIS_ IDirectMusicPerformance* pPerf, DMUS_PMSG* pPMSG)
{
	// Since ProcessPMsg is called from inside
	// the Performance's multi-media timer thread, take as little time
	// inside this function as possible.

	// The Tool is set up to receive messages of type DMUS_PMSGT_MIDI or DMUS_PMSGT_CURVE

	// If the DMUS_PMSGF_TOOL_ATTIME flag is set, this function
	// has already operated on this PMsg and requeued it.
	if( pPMSG->dwFlags & DMUS_PMSGF_TOOL_ATTIME )
	{
		// Atomically increment the counter so the game logic knows
		// to advance the move visually.
		InterlockedIncrement(&gcMoveCount);

		// return DMUS_S_FREE so the original message is freed
		return DMUS_S_FREE;
	}
	else if( pPMSG->dwType == DMUS_PMSGT_MIDI )
	{
		DMUS_MIDI_PMSG* pMIDI = (DMUS_MIDI_PMSG*)pPMSG;

		// only process control changes of type 0x06 and value 0x7f. This was chosen
		// arbitrarily as the type of midi event to which to respond, as it isn't
		// generally used for a musical purpose. The musician authors the events into
		// the music to drive the game's rhythm.
		if(((pMIDI->bStatus & 0xf0) == 0xb0 ) &&
			(pMIDI->bByte1 == 0x06) &&
			(pMIDI->bByte2 == 0x7f))
		{
			// Requeue the message to come back at its time stamp.
			// It will then be processed by the previous block of code.
			pPMSG->dwFlags &= ~DMUS_PMSGF_TOOL_IMMEDIATE;
			pPMSG->dwFlags |= DMUS_PMSGF_TOOL_ATTIME;
			return DMUS_S_REQUEUE;
		}
	}
	else if( pPMSG->dwType == DMUS_PMSGT_CURVE )
	{
		DMUS_CURVE_PMSG* pCurve = (DMUS_CURVE_PMSG*)pPMSG;

		// only process curves of type 0x06
		if( pCurve->bCCData == 6 )
		{
			// Requeue the message to come back at its time stamp.
			// It will then be processed by the previous block of code.
			pPMSG->dwFlags &= ~DMUS_PMSGF_TOOL_IMMEDIATE;
			pPMSG->dwFlags |= DMUS_PMSGF_TOOL_ATTIME;
			return DMUS_S_REQUEUE;
		}
	}

	// stamp the message and return DMUS_S_REQUEUE so other midi messages continue to
	// the next tool
	if( pPMSG->pGraph )
	{
		if( SUCCEEDED( pPMSG->pGraph->StampPMsg( pPMSG )))
		{
			return DMUS_S_REQUEUE;
		}
	}
	// we were unable to stamp the tool, so free it
	return DMUS_S_FREE;
}

STDMETHODIMP Music::Flush                (THIS_ IDirectMusicPerformance* pPerf, DMUS_PMSG* pPMSG, REFERENCE_TIME rtTime)
{
	// this tool does not need to flush.
	return E_NOTIMPL;
}
