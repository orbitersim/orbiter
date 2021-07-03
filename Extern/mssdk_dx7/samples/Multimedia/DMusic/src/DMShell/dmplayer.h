// dmplayer.h : header file
//
// Copyright (c) 1998-1999 Microsoft Corporation. All rights reserved.
/////////////////////////////////////////////////////////////////////////////
#ifndef _DMPLAYER_H
#define _DMPLAYER_H
#include <dmusici.h>
#include "scheme.h"

/////////////////////////////////////////////////////////////////////////////
//  The DMPlayer class encapsulates all of the DirectMusic interface and objects.
//  CMain calls DMPlayer to respond to incoming Shell events and user input
//  from the popup menu.
//

class CDMPlayer
{
public:
	CDMPlayer(HWND hWnd);
	virtual ~CDMPlayer();
	BOOL SelectNewScheme(WORD wSchemeNum);
	BOOL SelectOutputPort(WORD wPortIndex);
	BOOL GetSchemeName(WORD wSchemeNum, PWSTR pwzSchemeName, WORD wBufferLength);
	WORD EnumOutputPort(WORD wPortIndex, PWSTR pwzPortName, WORD wBufferLength);
	void Start();
	void Stop();
	BOOL IsPlaying();
	void PlayEvent(WPARAM wEventType, LPARAM lEventData);

protected:
	void ResetMidiStream();
	void PlayMotif(WORD wMotif);
	void PlaySegment(HWND hWnd);
	void NullMediaPointers();
	void ReleaseMediaPointers();
	BOOL InitializeSynth();
	BOOL LoadScheme(WORD wSchemeIndex);
	BOOL LoadStyle(WCHAR* pwzFileName);
	BOOL LoadDLS(WCHAR* pwzFileName);
	BOOL LoadChordMap(WCHAR* pwzFileName);
	BOOL LoadTemplate(DWORD dwIndex, WCHAR* pwzName);
	BOOL ComposeSegment(DWORD dwIndex);

	IDirectMusicLoader*			m_pLoader;
	IDirectMusicPerformance*	m_pPerformance;
	IDirectMusicStyle*			m_pStyle;
	IDirectMusicSegment*		m_pMotifs[DME_MAXEVENTNUM];
	IDirectMusicChordMap*	    m_pChordMap;
	IDirectMusicSegment*		m_pTemplates[NUM_TEMPLATES];
	IDirectMusicSegment*		m_pPrimarySegments[NUM_TEMPLATES];
								// the primary segments correspond to the templates
								// on a 1:1 basis
	IDirectMusicComposer*		m_pComposer;
	IDirectMusic*				m_pDirectMusic;
	IDirectMusicPort*			m_pPort;
	IDirectMusicBand*			m_pBand;

	HWND m_hWnd;
	CTemplateMapper m_TemplateMapper;
	BOOL m_bIsPlaying;
	BOOL m_bIsInitialized;
	MUSIC_TIME m_mtPreviousEventBeatNumber[DME_MAXEVENTRANGE];

};

#endif // _DMPLAYER_H

/////////////////////////////////////////////////////////////////////////////






