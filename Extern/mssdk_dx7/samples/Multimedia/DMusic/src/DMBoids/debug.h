#ifndef DEBUG_H
#define DEBUG_H
/*
**-----------------------------------------------------------------------------
**  File:       Debug.h
**  Purpose:    Sample Debug code
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

#ifdef DEBUG
// Note:  Define DEBUG_PROMPTME if you want MessageBox Error prompting
//		  This can get annoying quickly...
// #define DEBUG_PROMPTME

	// Pre and Post debug string info
	#define START_STR	TEXT ("BOIDS: ")
	#define END_STR		TEXT ("\r\n")
#endif // DEBUG

// Debug Levels
#define DEBUG_ALWAYS	0L
#define DEBUG_CRITICAL	1L
#define DEBUG_ERROR		2L
#define DEBUG_MINOR		3L
#define DEBUG_WARN		4L
#define DEBUG_DETAILS	5L


// Sample Errors
#define APPERR_GENERIC			MAKE_DDHRESULT (10001)
#define	APPERR_INVALIDPARAMS	MAKE_DDHRESULT (10002)
#define APPERR_NOTINITIALIZED	MAKE_DDHRESULT (10003)
#define APPERR_OUTOFMEMORY		MAKE_DDHRESULT (10004)
#define APPERR_NOTFOUND			MAKE_DDHRESULT (10005)



/*
**-----------------------------------------------------------------------------
**  Macros
**-----------------------------------------------------------------------------
*/

#ifdef DEBUG
    #define DPF dprintf
    #define ASSERT(x) \
        if (! (x)) \
        { \
            DPF (DEBUG_ALWAYS, TEXT("Assertion violated: %s, File = %s, Line = #%ld\n"), \
                 TEXT(#x), TEXT(__FILE__), (DWORD)__LINE__ ); \
            abort (); \
        }        

   #define REPORTERR(x) \
       ReportDDError ((x), TEXT("File = %s, Line = #%ld\n"), \
                      TEXT(__FILE__), (DWORD)__LINE__ );

   #define FATALERR(x) \
       ReportDDError ((x), TEXT("File = %s, Line = #%ld\n"), \
                      TEXT(__FILE__), (DWORD)__LINE__ ); \
       OnPause (TRUE); \
       DestroyWindow (g_hMainWindow);
#else
   #define REPORTERR(x)
   #define DPF 1 ? (void)0 : (void)
   #define ASSERT(x)
   #define FATALERR(x) \
       OnPause (TRUE); \
       DestroyWindow (g_hMainWindow);
#endif // DEBUG



/*
**-----------------------------------------------------------------------------
**  Global Variables
**-----------------------------------------------------------------------------
*/

// Debug Variables
#ifdef DEBUG
	extern DWORD g_dwDebugLevel;
#endif

extern BOOL  g_fDebug;



/*
**-----------------------------------------------------------------------------
**  Function Prototypes
**-----------------------------------------------------------------------------
*/

// Debug Routines
#ifdef DEBUG
	void __cdecl dprintf (DWORD dwDebugLevel, LPCTSTR szFormat, ...);
#endif //DEBUG

void _cdecl ReportDDError (HRESULT hResult, LPCTSTR szFormat, ...);

/*
**-----------------------------------------------------------------------------
**  End of File
**-----------------------------------------------------------------------------
*/
#endif // End DEBUG_H


