/*
**----------------------------------------------------------------------------
**
**  File:       music.h
**  Purpose:    header file for DirectMusic implementation.
**  Notes:
**
**	Copyright (C) 1995-1999 Microsoft Corporation. All Rights Reserved.
**----------------------------------------------------------------------------
*/


#ifndef MUSIC_H
#define MUSIC_H

#include <windows.h>
#include <dmusicc.h>
#include <dmusici.h>

interface IDirectMusicStyle;
interface IDirectMusicChordMap;
interface IDirectMusicPerformance;
interface IDirectMusicSegment;
interface IDirectMusicComposer;
interface IDirectMusicLoader;
interface IDirectMusicGraph;
interface IDirectMusicBand;

class CTool : public IDirectMusicTool
{
public:
	CTool();
    ~CTool();

public:
// IUnknown
    virtual STDMETHODIMP QueryInterface(const IID &iid, void **ppv);
    virtual STDMETHODIMP_(ULONG) AddRef();
    virtual STDMETHODIMP_(ULONG) Release();

// IDirectMusicTool
	HRESULT STDMETHODCALLTYPE Init(IDirectMusicGraph* pGraph) ;
	HRESULT STDMETHODCALLTYPE GetMsgDeliveryType(DWORD* pdwDeliveryType ) ;
	HRESULT STDMETHODCALLTYPE GetMediaTypeArraySize(DWORD* pdwNumElements ) ;
	HRESULT STDMETHODCALLTYPE GetMediaTypes(DWORD** padwMediaTypes, DWORD dwNumElements) ;
	HRESULT STDMETHODCALLTYPE ProcessPMsg(IDirectMusicPerformance* pPerf, DMUS_PMSG* pPMSG) ;
	HRESULT STDMETHODCALLTYPE Flush(IDirectMusicPerformance* pPerf, DMUS_PMSG* pPMSG, REFERENCE_TIME rt) ;

	long	m_cRef;
	DWORD	m_dwRatio;
	DWORD	m_dwEcho;
	DWORD	m_dwDelay;
	DWORD	m_dwStartRatio;
	IDirectMusicPerformance *	m_pPerformance;
};

class BoidMusic
{
public:
				BoidMusic();
				~BoidMusic();
	BOOL		LoadMusic(HWND hWnd);
	BOOL		LoadChordMap();
	BOOL		LoadStyle();
	BOOL		LoadSegment();
	BOOL		LoadDLS();
	BOOL		LoadTemplate(DWORD dwIndex, WCHAR * pszName);
	BOOL		GetMotif(DWORD dwIndex, WCHAR * pszName);
	void		ComposeSegment(DWORD dwIndex);
	void		EndMusic();
	void		StartMusic();
	void		Transition();
	void		Collapse();
	void		Expand();
	void		Migrate();
	void		HandleNotifies();
	void		SetDistance(double fDistance);
	BOOL		GetSearchPath(WCHAR path[MAX_PATH]);
	IDirectMusicStyle*			m_pStyle;
	IDirectMusicChordMap*	    m_pChordMap;
	IDirectMusicSegment*		m_pTemplateSegments[6];
	IDirectMusicSegment*		m_pPrimarySegments[6];
	IDirectMusicSegment*		m_pTransitionSegment;
	IDirectMusicSegment*		m_pMotifSegments[6];
	IDirectMusicSegment*		m_pSegment;
	IDirectMusicComposer*		m_pComposer;
	IDirectMusicLoader*			m_pLoader;
	IDirectMusicPerformance*	m_pPerformance;
	IDirectMusicGraph*			m_pGraph;
	IDirectMusicPort*			m_pPort;
	IDirectMusicBand*			m_pBand;
	IDirectMusic*				m_pDMusic;
	DWORD						m_dwIndex;
	HANDLE						m_hNotify;
	DWORD						m_dwLevel;
	CTool						m_Tool;
	BOOL						m_dwBeatsSinceLastMotif;
	BOOL						m_fCollapsed;
};

#endif // MUSIC_H

