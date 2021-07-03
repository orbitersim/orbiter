/*==========================================================================
 *
 *  Copyright (C) 1995-1999 Microsoft Corporation. All Rights Reserved.
 *
 *  File:	lbprintf.h
 *  Content:	list box printf header file
 *
 ***************************************************************************/
#ifndef __LBPRINTF_INCLUDED__

#define __LBPRINTF_INCLUDED__
#ifdef __cplusplus
extern "C" {
#endif
extern void LBCreate( HWND hWnd, DWORD pos );
extern void LBSize( DWORD dwWidth, DWORD dwHeight );
extern void LBClear( void );
extern void __cdecl LBPrintf( LPSTR fmt, ... );
extern void __cdecl LBPrintfDDRC( HRESULT rc, LPSTR fmt, ... );

#ifdef __cplusplus
}
#endif

#endif
