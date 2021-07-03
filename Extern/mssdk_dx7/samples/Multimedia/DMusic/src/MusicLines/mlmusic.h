//
// MLMusic.h
//
// Music code for MusicLines
//
// Copyright (c) 1998-1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------

#ifndef _MLMusic_
#define _MLMusic_

#include <dmusici.h>

#define NUM_TEMPLATES	4
#define NUM_MAINSEGS	4
#define NUM_PLAYERS		2

class Music : public IDirectMusicTool
{
public:
	Music();
	~Music();
	BOOL Initialize(HWND hWnd);

	enum PlayEvent
	{
		StartLevel		= 0,
		Right			= 1,
		Left			= 2,
		Up				= 3,
		Down			= 4,
		Death			= 5,
		EndLevel		= 6,
		Win				= 7
	};

	void Play( PlayEvent playEvent, int PlayerNo );

/*  IUnknown */
    virtual STDMETHODIMP            QueryInterface(const IID &iid, void **ppv);
    virtual STDMETHODIMP_(ULONG)    AddRef();
    virtual STDMETHODIMP_(ULONG)    Release();

/*  IDirectMusicTool */
STDMETHODIMP Init                 (THIS_ IDirectMusicGraph* pGraph);
STDMETHODIMP GetMsgDeliveryType   (THIS_ DWORD* pdwDeliveryType );
STDMETHODIMP GetMediaTypeArraySize(THIS_ DWORD* pdwNumElements );
STDMETHODIMP GetMediaTypes        (THIS_ DWORD** padwMediaTypes, DWORD dwNumElements);
STDMETHODIMP ProcessPMsg          (THIS_ IDirectMusicPerformance* pPerf, DMUS_PMSG* pPMSG);
STDMETHODIMP Flush                (THIS_ IDirectMusicPerformance* pPerf, DMUS_PMSG* pPMSG, REFERENCE_TIME rtTime);

protected:
	void						DownloadInstruments();
	void						UnloadInstruments();

private:
	IDirectMusicPerformance*	m_pPerf;
	IDirectMusicLoader*			m_pLoader;
	IDirectMusicComposer*		m_pComposer;
	// Primary templates
	IDirectMusicSegment*		m_apTemplate[NUM_TEMPLATES];
	// Primary segments
	IDirectMusicSegment*		m_pIntroSeg;
	IDirectMusicSegment*		m_apMainSeg[NUM_MAINSEGS];
	IDirectMusicSegment*		m_pEndSeg[NUM_MAINSEGS];
	// Secondary segments
	IDirectMusicSegment*		m_pUpSeg[NUM_PLAYERS];
	IDirectMusicSegment*		m_pDownSeg[NUM_PLAYERS];
	IDirectMusicSegment*		m_pLeftSeg[NUM_PLAYERS];
	IDirectMusicSegment*		m_pRightSeg[NUM_PLAYERS];
	IDirectMusicSegment*		m_pDeathSeg[NUM_MAINSEGS];
	IDirectMusicSegment*		m_pWinSeg;
	IDirectMusicSegmentState*	m_pDirSegState[NUM_PLAYERS];

	BOOL	m_fPlayingIntro;
	DWORD	m_dwIntensity;
	long	m_cRef;			// reference counter
};

extern Music *theMusic;

#endif // _MLMusic_

