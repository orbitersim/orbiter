//-----------------------------------------------------------------------------
// File: D3DUtil.h
//
// Desc: Helper functions and typing shortcuts for Direct3D programming.
//
// Copyright (c) 1997-1999 Microsoft Corporation. All rights reserved
//-----------------------------------------------------------------------------
#ifndef D3DUTIL_H
#define D3DUTIL_H
#include <ddraw.h>
#include <d3d.h>




//-----------------------------------------------------------------------------
// Miscellaneous helper functions
//-----------------------------------------------------------------------------
const TCHAR* D3DUtil_GetDXSDKMediaPath();

#define SAFE_DELETE(p)  { if(p) { delete (p);     (p)=NULL; } }
#define SAFE_RELEASE(p) { if(p) { (p)->Release(); (p)=NULL; } }




//-----------------------------------------------------------------------------
// Short cut functions for creating and using DX structures
//-----------------------------------------------------------------------------
VOID D3DUtil_InitDeviceDesc( D3DDEVICEDESC7& ddDevDesc );
VOID D3DUtil_InitSurfaceDesc( DDSURFACEDESC2& ddsd, DWORD dwFlags=0,
                              DWORD dwCaps=0 );
VOID D3DUtil_InitMaterial( D3DMATERIAL7& mtrl, FLOAT r=0.0f, FLOAT g=0.0f,
                           FLOAT b=0.0f, FLOAT a=1.0f );
VOID D3DUtil_InitLight( D3DLIGHT7& light, D3DLIGHTTYPE ltType,
                        FLOAT x=0.0f, FLOAT y=0.0f, FLOAT z=0.0f );




//-----------------------------------------------------------------------------
// D3D Matrix functions. For performance reasons, some functions are inline.
//-----------------------------------------------------------------------------
HRESULT D3DUtil_SetViewMatrix( D3DMATRIX& mat, D3DVECTOR& vFrom,
                               D3DVECTOR& vAt, D3DVECTOR& vUp );
HRESULT D3DUtil_SetProjectionMatrix( D3DMATRIX& mat, FLOAT fFOV = 1.570795f,
                                     FLOAT fAspect = 1.0f,
                                     FLOAT fNearPlane = 1.0f,
                                     FLOAT fFarPlane = 1000.0f );

inline VOID D3DUtil_SetIdentityMatrix( D3DMATRIX& m )
{
    m._12 = m._13 = m._14 = m._21 = m._23 = m._24 = 0.0f;
    m._31 = m._32 = m._34 = m._41 = m._42 = m._43 = 0.0f;
    m._11 = m._22 = m._33 = m._44 = 1.0f;
}

inline VOID D3DUtil_SetTranslateMatrix( D3DMATRIX& m, FLOAT tx, FLOAT ty,
                                        FLOAT tz )
{ D3DUtil_SetIdentityMatrix( m ); m._41 = tx; m._42 = ty; m._43 = tz; }

inline VOID D3DUtil_SetTranslateMatrix( D3DMATRIX& m, D3DVECTOR& v )
{ D3DUtil_SetTranslateMatrix( m, v.x, v.y, v.z ); }

inline VOID D3DUtil_SetScaleMatrix( D3DMATRIX& m, FLOAT sx, FLOAT sy,
                                    FLOAT sz )
{ D3DUtil_SetIdentityMatrix( m ); m._11 = sx; m._22 = sy; m._33 = sz; }

inline VOID SetScaleMatrix( D3DMATRIX& m, D3DVECTOR& v )
{ D3DUtil_SetScaleMatrix( m, v.x, v.y, v.z ); }

VOID    D3DUtil_SetRotateXMatrix( D3DMATRIX& mat, FLOAT fRads );
VOID    D3DUtil_SetRotateYMatrix( D3DMATRIX& mat, FLOAT fRads );
VOID    D3DUtil_SetRotateZMatrix( D3DMATRIX& mat, FLOAT fRads );
VOID    D3DUtil_SetRotationMatrix( D3DMATRIX& mat, D3DVECTOR& vDir,
                                   FLOAT fRads );




//-----------------------------------------------------------------------------
// Debug printing support
//-----------------------------------------------------------------------------

HRESULT _DbgOut( TCHAR*, DWORD, HRESULT, TCHAR* );

#if defined(DEBUG) | defined(_DEBUG)
    #define DEBUG_MSG(str)    _DbgOut( __FILE__, (DWORD)__LINE__, 0, str )
    #define DEBUG_ERR(hr,str) _DbgOut( __FILE__, (DWORD)__LINE__, hr, str )
#else
    #define DEBUG_MSG(str)    (0L)
    #define DEBUG_ERR(hr,str) (hr)
#endif




#endif // D3DUTIL_H
