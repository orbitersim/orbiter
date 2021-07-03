/*==========================================================================
 *
 *  Copyright (C) 1995-1999 Microsoft Corporation. All Rights Reserved.
 *
 *  File:       ddutil.cpp
 *  Content:    Routines for loading bitmap and palettes from resources
 *
 ***************************************************************************/

#ifdef __cplusplus
extern "C" {            /* Assume C declarations for C++ */
#endif	/* __cplusplus */

extern IDirectDrawPalette * DDLoadPalette2(IDirectDraw2 *pdd, LPCSTR szBitmap);
extern IDirectDrawSurface3 * DDLoadBitmap2(IDirectDraw2 *pdd, LPCSTR szBitmap, int dx, int dy);
extern HRESULT              DDReLoadBitmap2(IDirectDrawSurface3 *pdds, LPCSTR szBitmap);
extern HRESULT              DDCopyBitmap2(IDirectDrawSurface3 *pdds, HBITMAP hbm, int x, int y, int dx, int dy);
extern DWORD                DDColorMatch2(IDirectDrawSurface3 *pdds, COLORREF rgb);
extern HRESULT              DDSetColorKey2(IDirectDrawSurface3 *pdds, COLORREF rgb);

#ifdef __cplusplus
}
#endif	/* __cplusplus */
