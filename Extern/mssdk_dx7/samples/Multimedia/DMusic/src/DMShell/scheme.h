// scheme.h : header file
//
// Copyright (c) 1998-1999 Microsoft Corporation. All rights reserved.
/////////////////////////////////////////////////////////////////////////////

#ifndef _SCHEME_H
#define _SCHEME_H
#include "events.h"

#define NUM_SCHEMES			3
#define NUM_TEMPLATES		5

// The scheme structure stores all of the filename and motif names associated with
// a musical scheme.
struct SCHEME
{
	PWSTR FriendlySchemeName;				//Human readable name for the menu
	PWSTR StyleName;						//Filename
	PWSTR DLSName;							//Filename (blank if the style uses GM.DLS)
	PWSTR ChordMapName;						//Filename
	PWSTR TemplateNames[NUM_TEMPLATES];		//Filename
	PWSTR MotifNames[DME_MAXEVENTNUM];		//Motif name contained in the style (referenced above)
};


static SCHEME Schemes[NUM_SCHEMES] = 
{
	{L"Adult Contemporary",
		L"Meshel.sty",
		L"",
		L"meeshel-v2.cdm",
		{L"Meshela.tpl", L"Meshelb.tpl", L"Meshelc.tpl", L"Mesheld.tpl",L"Meshele.tpl"},
		{
		L"app close",		//DME_WINDOWCLOSE	
		L"win max",			//DME_MAXIMIZE		
		L"win min",			//DME_MINIMIZE		
		L"win max", 		//DME_RESTORE		
		L"scroll",			//DME_SCROLL			
		L"win min", 		//DME_WINDOWMOVE		
		L"win min", 		//DME_WINDOWSIZE		
		L"menu mvt",		//DME_MENUPOPUP		
		L"menu mvt",		//DME_MENUSELECT		
		L"scroll",			//DME_KEYRETURN		
		L"",				//DME_KEYBACKSPACE	
		L"",				//DME_KEYSPACE		
		L"",				//DME_KEYGENERIC
		L"start menu",		//DME_STARTBUTTON
		L"",				//DME_MOUSELBDOWN
		L"",				//DME_MOUSELBUP
		L"",				//DME_MOUSELDBLCLK
		L"",				//DME_MOUSERBDOWN
		L"",				//DME_MOUSERBUP
		L"",				//DME_MOUSERDBLCLK
		L"app open"			//DME_APPOPEN
		}
	},
	{L"New Age",
		L"MinimalD.sty",
		L"",
		L"Minimal.cdm",
		{L"Minimal1.tpl", L"Minimal2.tpl",L"Minimal3.tpl", L"Minimal4.tpl",L"Minimal5.tpl"},
		{
		L"CloseApp",		//DME_WINDOWCLOSE	
		L"MaxWindow",		//DME_MAXIMIZE		
		L"MinWindow",		//DME_MINIMIZE		
		L"MaxWindow", 		//DME_RESTORE		
		L"MinWindow",		//DME_SCROLL			
		L"CloseApp",		//DME_WINDOWMOVE		
		L"CloseApp",		//DME_WINDOWSIZE		
		L"CloseApp",		//DME_MENUPOPUP		
		L"CloseApp",		//DME_MENUSELECT		
		L"Placeholder2",	//DME_KEYRETURN		
		L"Placeholder1",	//DME_KEYBACKSPACE	
		L"Placeholder1",	//DME_KEYSPACE		
		L"Placeholder3",	//DME_KEYGENERIC
		L"OpenApp",			//DME_STARTBUTTON
		L"",				//DME_MOUSELBDOWN
		L"",				//DME_MOUSELBUP
		L"",				//DME_MOUSELDBLCLK
		L"",				//DME_MOUSERBDOWN
		L"",				//DME_MOUSERBUP
		L"",				//DME_MOUSERDBLCLK
		L"OpenApp"			//DME_APPOPEN
		}
	},
	{L"Euro Dance",
		L"Mix.sty",
		L"shell.dls",
		L"mix.cdm",
		{L"mix1.tpl", L"mix2.tpl", L"mix3.tpl", L"mix4.tpl",L"mix5.tpl"},
		{
		L"Closeapp",		//DME_WINDOWCLOSE	
		L"Maxwindow",		//DME_MAXIMIZE		
		L"Minwindow",		//DME_MINIMIZE		
		L"Maxwindow", 		//DME_RESTORE		
		L"Scrollbar",		//DME_SCROLL			
		L"Movemenu" ,		//DME_WINDOWMOVE		
		L"Movemenu", 		//DME_WINDOWSIZE		
		L"Movemenu",		//DME_MENUPOPUP		
		L"Movemenu",		//DME_MENUSELECT		
		L"Keyboard",		//DME_KEYRETURN		
		L"Keyboard",		//DME_KEYBACKSPACE	
		L"Keyboard",		//DME_KEYSPACE		
		L"Keyboard",		//DME_KEYGENERIC
		L"Startmenu",		//DME_STARTBUTTON
		L"",				//DME_MOUSELBDOWN
		L"",				//DME_MOUSELBUP
		L"",				//DME_MOUSELDBLCLK
		L"",				//DME_MOUSERBDOWN
		L"",				//DME_MOUSERBUP
		L"",				//DME_MOUSERDBLCLK
		L"Openapp"			//DME_APPOPEN
		}
	}
};

///////////////////////////////////////////////////////////////////////////
//
//  This object stores the mapping of applications windows to templates.
//  Applications are referenced by the window handle (hWnd)
//  Templates are referenced by the template index (WORD)
//
class CTemplateIndex
{
public:
	CTemplateIndex(HWND hWnd, WORD wTemplate);
	virtual ~CTemplateIndex();
	CTemplateIndex* m_pNextIndex;
	HWND m_hWnd;
	WORD m_wTemplate;
};

class CTemplateMapper
{
public:
	CTemplateMapper();
	virtual ~CTemplateMapper();
	WORD GetTemplate(HWND hWnd);
	void DeleteWin(HWND hWnd);
	void DeleteAll();
private:
	WORD PickNewTemplate();
	CTemplateIndex* m_pFirstIndex;
};


#endif
/////////////////////////////////////////////////////////////////////////////

