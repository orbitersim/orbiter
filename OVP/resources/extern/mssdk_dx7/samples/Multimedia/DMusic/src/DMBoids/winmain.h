#ifndef WINMAIN_H
#define WINMAIN_H
/*
**-----------------------------------------------------------------------------
**  File:       WinMain.h
**  Purpose:    Sample Window code
**  Notes:
**
**  Copyright (c) 1995-1999 by Microsoft, all rights reserved
**-----------------------------------------------------------------------------
*/

/*
**-----------------------------------------------------------------------------
**  Include files
**-----------------------------------------------------------------------------
*/

#include "Common.h"



/*
**-----------------------------------------------------------------------------
**  Defines
**-----------------------------------------------------------------------------
*/

#define MIN_WIN_SIZE_X	128
#define MIN_WIN_SIZE_Y  128
#define DEF_WIN_SIZE_X	256
#define DEF_WIN_SIZE_Y  256



/*
**-----------------------------------------------------------------------------
**  Global Variables
**-----------------------------------------------------------------------------
*/

extern HINSTANCE    g_hMainInstance;
extern HWND         g_hMainWindow;
extern HACCEL       g_hMainAccel;
extern HCURSOR      g_hMainCursor;

extern LPCTSTR      g_szMainName;
extern LPCTSTR      g_szMainClass;
extern LPCTSTR      g_szMainTitle;
extern LPCTSTR      g_szPaused;

extern INT          g_nExitCode;

extern BOOL         g_fActive;
extern INT          g_nPaused;

extern INT          g_nWinWidth;
extern INT          g_nWinHeight;



/*
**-----------------------------------------------------------------------------
**  Function Prototypes
**-----------------------------------------------------------------------------
*/

// Windows routines
BOOL InitMain (void);
void FiniMain (void);

BOOL CheckPreviousApp (void);
void RunMain (void);
INT  WINAPI WinMain (HINSTANCE hInstance, HINSTANCE hPrevInstance,
                     LPSTR lpCmdLine, INT nShowCmd);
BOOL InitMainWindow (void);


/*
**-----------------------------------------------------------------------------
**  End of File
**-----------------------------------------------------------------------------
*/
#endif // End WINMAIN_H


