// events.h : header file
//
// Copyright (c) 1998-1999 Microsoft Corporation. All rights reserved.
/////////////////////////////////////////////////////////////////////////////

#ifndef _EVENTS_H
#define _EVENTS_H


//Events played on Motifs
#define DME_WINDOWCLOSE		0	//Window and application close
#define DME_MAXIMIZE		1	//Window maximize
#define DME_MINIMIZE		2	//Window minimize
#define DME_RESTORE			3	//Application is un-minimized
#define DME_SCROLL			4	//User moves either H or V scroll bars
#define DME_WINDOWMOVE		5	//User moves a window
#define DME_WINDOWSIZE		6	//User changes size of window
#define DME_MENUPOPUP		7	//Popup menus that occur when the user
								//right clicks for a context menu
								//or when the start menu menu items are selected
#define DME_MENUSELECT		8	//User made selection from app's menu bar
#define DME_KEYRETURN		9	//Press the enter key, autorepeat will also send this message
#define DME_KEYBACKSPACE	10	//Backspace
#define DME_KEYSPACE		11	//Space bar
#define DME_KEYGENERIC		12	//Every other key that is not covered in a specific message
#define DME_STARTBUTTON		13	//User presses the start button
#define DME_MOUSELBDOWN		14	//Left Mouse button goes down
#define DME_MOUSELBUP		15	//  "    "      "   comes up
#define DME_MOUSELDBLCLK	16	//Double click of left button
#define DME_MOUSERBDOWN		17	//Right Mouse button goes down
#define DME_MOUSERBUP		18	//  "    "      "   comes up
#define DME_MOUSERDBLCLK	19	//Double click of right button
#define DME_APPOPEN			20	//Application Open


#define DME_MAXEVENTNUM		22	//Max for array sizing

//Special case events
#define DME_APPCLOSE		100	//Application closes
#define DME_APPSWITCH		101	//A new application has been selected

#define DME_MOUSELDRAG		103	//Mouse moves while left button is down
#define DME_MOUSERDRAG		104	//  "     "     "   right  "     "   "
#define DME_MAXEVENTRANGE	105 //Array size for event filter

#define NUM_PORTS			100

#define DME_REGISTEREDEVENT "DMShell Registered Event"	//Used to pass data from DLL to App
#define CLASSNAME			"DMShell Window Class"		//Arbitrary Names used to identify the window
#define WINDOWNAME			"DMShell Target Window"		// ditto
#define DMS_UIEVENT			"DMShell UI Event"		//for communication between the tray icon & target win
#define DMS_TIP				"DirectMusic Shell"


#endif // _EVENTS_H

/////////////////////////////////////////////////////////////////////////////






