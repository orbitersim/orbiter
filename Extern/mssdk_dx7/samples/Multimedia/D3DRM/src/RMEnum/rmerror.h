/*
 *  Copyright (C) 1995-1999 Microsoft Corporation. All Rights Reserved.
 *
 *  File: rmerror.h
 *
 *  Error reporting code for D3DRM examples.
 *
 */

#ifndef __ERROR_H__
#define __ERROR_H__

#include <ddraw.h>
#include <d3d.h>
#include <d3drmwin.h>
#include <d3drm.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Msg
 * Displays a message box containing the given formatted string.
 */
void __cdecl
Msg( LPSTR fmt, ... );

/*
 * D3DRMErrorToString
 * Returns a pointer to a string describing the given DD, D3D or D3DRM error code.
 */
char*
D3DRMErrorToString(HRESULT error);

#ifdef __cplusplus
};
#endif
#endif // __ERROR_H__

