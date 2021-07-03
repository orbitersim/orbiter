/*****************************************************************************
*
*  THIS CODE AND INFORMATION IS PROVIDED "AS IS" WITHOUT WARRANTY OF
*  ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING BUT NOT LIMITED
*  TO THE IMPLIED WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR
*  A PARTICULAR PURPOSE.
*
*  Copyright (C) 1993-1999 Microsoft Corporation. All Rights Reserved.
*
******************************************************************************
*
* MIDIPlyr.H
*
* Main include file for the polymessage MIDI playback app.
*
*****************************************************************************/

#ifndef _MIDIPLYR_
#define _MIDIPLYR_

#include <dmusici.h>
#include "global.h"
#include "resource.h"

/* Popup menu positions in main menu bar
*/
#define POS_FILE            0
#define POS_OPTIONS         1
#define POS_PLAYTHRU        2

/* Status bar pane indices
*/
#define SB_N_PANES          2
#define SB_PANE_STATE       0
#define SB_PANE_TFMT        1

                       
#define WM_GETPMSG			(WM_USER + 21)

typedef struct tag_MIDIPERF
{
	IDirectMusic				*pDirectMusic;
	IDirectMusicPerformance		*pDMusPerformance;
	int							iMenuPos;
	DMUS_PORTCAPS				*pPortCaps;
	IDirectMusicPort			*pDMusPort;
	IDirectMusicLoader			*pDMusLoader;
	IDirectMusicSegment			*pDMusSegment;
	IDirectMusicSegmentState	*pDMusSegmentState;
	BOOL						fSegIgnoreBSFGM;
	BOOL						fReverbOn;
    BOOL                        fDownloaded;
	UINT						uState;
	MUSIC_TIME					mtStart;
    MUSIC_TIME                  mtOffset;
    REFERENCE_TIME              rtStart;
    REFERENCE_TIME              rtOffset;
}MIDIPERF; 
typedef MIDIPERF * PMIDIPERF;

/* Globals
*/
extern  HINSTANCE       ghInst;
extern  char BCODE		gszMWndClass[];
extern  char BCODE		gszTWndClass[];
extern	MIDIPERF		MIDIPerf;
extern  char			gszUntitled[80];
extern  char			gszAppLongName[80];
extern  char			gszAppTitleMask[80];
extern  char			grgszTimeFormats[N_TIME_FORMATS][CB_TIME_FORMATS];
extern  RECT            grcTWnd;
extern  int             gnTimeFormat;

/* MainWnd.C
*/

LRESULT CALLBACK MWnd_WndProc(
    HWND                    hWnd,
    UINT                    msg,
    WPARAM                  wParam,
    LPARAM                  lParam);

/* TimeWnd.C
*/
LRESULT CALLBACK TWnd_WndProc(
    HWND                    hWnd,
    UINT                    msg,
    WPARAM                  wParam,
    LPARAM                  lParam);

/* UiUtils.C
*/
VOID  EmbossedTextOut(
     HDC                    hDC,
     int                    x,
     int                    y,
     LPSTR                  lpsz,
     UINT                   cb,
     COLORREF               crText,
     COLORREF               crShadow,
     int                    cx,
     int                    cy);   

HFONT  CreateScaledFont(
     HDC                    hDC,
     LPRECT                 lpRect,
     LPSTR                  lpszFormat,
     int                    anPosX[],
     int* nPosY);

#endif
