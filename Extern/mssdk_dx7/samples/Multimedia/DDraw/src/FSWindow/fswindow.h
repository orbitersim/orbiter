//-----------------------------------------------------------------------------
// File: FSWindow.H
//
// Desc: Header file for full-screen non-GDI window update functions
//
// Copyright (c) 1998-1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------

#define USE_SURF4

#ifdef __cplusplus
	extern "C" {
#endif

// Exported function prototypes
void		FSWindow_Init(HWND hwndApp, IDirectDraw7 *dd,
                          IDirectDrawSurface7 *FrontBuffer,
                          IDirectDrawSurface7 *BackBuffer);
HWND		FSWindow_Begin(HWND hwnd,BOOL StaticContent);
void		FSWindow_End(void);
void		FSWindow_Update(void);
BOOL		FSWindow_IsActive(void);
BOOL		FSWindow_IsStatic(void);

#ifdef __cplusplus
	}
#endif

