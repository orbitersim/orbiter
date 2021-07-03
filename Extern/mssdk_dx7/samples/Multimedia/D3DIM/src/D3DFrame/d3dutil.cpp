//-----------------------------------------------------------------------------
// File: D3DUtil.cpp
//
// Desc: Shortcut macros and functions for using DX objects
//
//
// Copyright (c) 1997-1999 Microsoft Corporation. All rights reserved
//-----------------------------------------------------------------------------
#define D3D_OVERLOADS
#define STRICT
#include <math.h>
#include <stdio.h>
#include <tchar.h>
#include "D3DUtil.h"




//-----------------------------------------------------------------------------
// Name: D3DUtil_GetDXSDKMediaPath()
// Desc: Returns the DirectX SDK media path, as stored in the system registry
//       during the SDK install.
//-----------------------------------------------------------------------------
const TCHAR* D3DUtil_GetDXSDKMediaPath()
{
    static TCHAR strNull[2] = _T("");
    static TCHAR strPath[512];
    HKEY  key;
    DWORD type, size = 512;

    // Open the appropriate registry key
    LONG result = RegOpenKeyEx( HKEY_LOCAL_MACHINE,
                                _T("Software\\Microsoft\\DirectX"),
                                0, KEY_READ, &key );
    if( ERROR_SUCCESS != result )
        return strNull;

    result = RegQueryValueEx( key, _T("DXSDK Samples Path"), NULL,
                              &type, (BYTE*)strPath, &size );
    RegCloseKey( key );

    if( ERROR_SUCCESS != result )
        return strNull;

    lstrcat( strPath, _T("\\D3DIM\\Media\\") );

    return strPath;
}




//-----------------------------------------------------------------------------
// Name: D3DUtil_InitSurfaceDesc()
// Desc: Helper function called to build a DDSURFACEDESC2 structure,
//       typically before calling CreateSurface() or GetSurfaceDesc()
//-----------------------------------------------------------------------------
VOID D3DUtil_InitSurfaceDesc( DDSURFACEDESC2& ddsd, DWORD dwFlags,
                              DWORD dwCaps )
{
    ZeroMemory( &ddsd, sizeof(ddsd) );
    ddsd.dwSize                 = sizeof(ddsd);
    ddsd.dwFlags                = dwFlags;
    ddsd.ddsCaps.dwCaps         = dwCaps;
    ddsd.ddpfPixelFormat.dwSize = sizeof(DDPIXELFORMAT);
}




//-----------------------------------------------------------------------------
// Name: D3DUtil_InitMaterial()
// Desc: Helper function called to build a D3DMATERIAL7 structure
//-----------------------------------------------------------------------------
VOID D3DUtil_InitMaterial( D3DMATERIAL7& mtrl, FLOAT r, FLOAT g, FLOAT b,
                           FLOAT a )
{
    ZeroMemory( &mtrl, sizeof(D3DMATERIAL7) );
    mtrl.dcvDiffuse.r = mtrl.dcvAmbient.r = r;
    mtrl.dcvDiffuse.g = mtrl.dcvAmbient.g = g;
    mtrl.dcvDiffuse.b = mtrl.dcvAmbient.b = b;
    mtrl.dcvDiffuse.a = mtrl.dcvAmbient.a = a;
}




//-----------------------------------------------------------------------------
// Name: D3DUtil_InitLight()
// Desc: Initializes a D3DLIGHT7 structure
//-----------------------------------------------------------------------------
VOID D3DUtil_InitLight( D3DLIGHT7& light, D3DLIGHTTYPE ltType,
                        FLOAT x, FLOAT y, FLOAT z )
{
    ZeroMemory( &light, sizeof(D3DLIGHT7) );
    light.dltType        = ltType;
    light.dcvDiffuse.r   = 1.0f;
    light.dcvDiffuse.g   = 1.0f;
    light.dcvDiffuse.b   = 1.0f;
    light.dcvSpecular    = light.dcvDiffuse;
    light.dvPosition.x   = light.dvDirection.x = x;
    light.dvPosition.y   = light.dvDirection.y = y;
    light.dvPosition.z   = light.dvDirection.z = z;
    light.dvAttenuation0 = 1.0f;
    light.dvRange        = D3DLIGHT_RANGE_MAX;
}




//-----------------------------------------------------------------------------
// Name: D3DUtil_SetViewMatrix()
// Desc: Given an eye point, a lookat point, and an up vector, this
//       function builds a 4x4 view matrix.
//-----------------------------------------------------------------------------
HRESULT D3DUtil_SetViewMatrix( D3DMATRIX& mat, D3DVECTOR& vFrom,
                               D3DVECTOR& vAt, D3DVECTOR& vWorldUp )
{
    // Get the z basis vector, which points straight ahead. This is the
    // difference from the eyepoint to the lookat point.
    D3DVECTOR vView = vAt - vFrom;

    FLOAT fLength = Magnitude( vView );
    if( fLength < 1e-6f )
        return E_INVALIDARG;

    // Normalize the z basis vector
    vView /= fLength;

    // Get the dot product, and calculate the projection of the z basis
    // vector onto the up vector. The projection is the y basis vector.
    FLOAT fDotProduct = DotProduct( vWorldUp, vView );

    D3DVECTOR vUp = vWorldUp - fDotProduct * vView;

    // If this vector has near-zero length because the input specified a
    // bogus up vector, let's try a default up vector
    if( 1e-6f > ( fLength = Magnitude( vUp ) ) )
    {
        vUp = D3DVECTOR( 0.0f, 1.0f, 0.0f ) - vView.y * vView;

        // If we still have near-zero length, resort to a different axis.
        if( 1e-6f > ( fLength = Magnitude( vUp ) ) )
        {
            vUp = D3DVECTOR( 0.0f, 0.0f, 1.0f ) - vView.z * vView;

            if( 1e-6f > ( fLength = Magnitude( vUp ) ) )
                return E_INVALIDARG;
        }
    }

    // Normalize the y basis vector
    vUp /= fLength;

    // The x basis vector is found simply with the cross product of the y
    // and z basis vectors
    D3DVECTOR vRight = CrossProduct( vUp, vView );

    // Start building the matrix. The first three rows contains the basis
    // vectors used to rotate the view to point at the lookat point
    D3DUtil_SetIdentityMatrix( mat );
    mat._11 = vRight.x;    mat._12 = vUp.x;    mat._13 = vView.x;
    mat._21 = vRight.y;    mat._22 = vUp.y;    mat._23 = vView.y;
    mat._31 = vRight.z;    mat._32 = vUp.z;    mat._33 = vView.z;

    // Do the translation values (rotations are still about the eyepoint)
    mat._41 = - DotProduct( vFrom, vRight );
    mat._42 = - DotProduct( vFrom, vUp );
    mat._43 = - DotProduct( vFrom, vView );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: D3DUtil_SetProjectionMatrix()
// Desc: Sets the passed in 4x4 matrix to a perpsective projection matrix built
//       from the field-of-view (fov, in y), aspect ratio, near plane (D),
//       and far plane (F). Note that the projection matrix is normalized for
//       element [3][4] to be 1.0. This is performed so that W-based range fog
//       will work correctly.
//-----------------------------------------------------------------------------
HRESULT D3DUtil_SetProjectionMatrix( D3DMATRIX& mat, FLOAT fFOV, FLOAT fAspect,
                                     FLOAT fNearPlane, FLOAT fFarPlane )
{
    if( fabs(fFarPlane-fNearPlane) < 0.01f )
        return E_INVALIDARG;
    if( fabs(sin(fFOV/2)) < 0.01f )
        return E_INVALIDARG;

    FLOAT w = fAspect * ( cosf(fFOV/2)/sinf(fFOV/2) );
    FLOAT h =   1.0f  * ( cosf(fFOV/2)/sinf(fFOV/2) );
    FLOAT Q = fFarPlane / ( fFarPlane - fNearPlane );

    ZeroMemory( &mat, sizeof(D3DMATRIX) );
    mat._11 = w;
    mat._22 = h;
    mat._33 = Q;
    mat._34 = 1.0f;
    mat._43 = -Q*fNearPlane;

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: D3DUtil_SetRotateXMatrix()
// Desc: Create Rotation matrix about X axis
//-----------------------------------------------------------------------------
VOID D3DUtil_SetRotateXMatrix( D3DMATRIX& mat, FLOAT fRads )
{
    D3DUtil_SetIdentityMatrix( mat );
    mat._22 =  cosf( fRads );
    mat._23 =  sinf( fRads );
    mat._32 = -sinf( fRads );
    mat._33 =  cosf( fRads );
}




//-----------------------------------------------------------------------------
// Name: D3DUtil_SetRotateYMatrix()
// Desc: Create Rotation matrix about Y axis
//-----------------------------------------------------------------------------
VOID D3DUtil_SetRotateYMatrix( D3DMATRIX& mat, FLOAT fRads )
{
    D3DUtil_SetIdentityMatrix( mat );
    mat._11 =  cosf( fRads );
    mat._13 = -sinf( fRads );
    mat._31 =  sinf( fRads );
    mat._33 =  cosf( fRads );
}




//-----------------------------------------------------------------------------
// Name: D3DUtil_SetRotateZMatrix()
// Desc: Create Rotation matrix about Z axis
//-----------------------------------------------------------------------------
VOID D3DUtil_SetRotateZMatrix( D3DMATRIX& mat, FLOAT fRads )
{
    D3DUtil_SetIdentityMatrix( mat );
    mat._11  =  cosf( fRads );
    mat._12  =  sinf( fRads );
    mat._21  = -sinf( fRads );
    mat._22  =  cosf( fRads );
}




//-----------------------------------------------------------------------------
// Name: D3DUtil_SetRotationMatrix
// Desc: Create a Rotation matrix about vector direction
//-----------------------------------------------------------------------------
VOID D3DUtil_SetRotationMatrix( D3DMATRIX& mat, D3DVECTOR& vDir, FLOAT fRads )
{
    FLOAT     fCos = cosf( fRads );
    FLOAT     fSin = sinf( fRads );
    D3DVECTOR v    = Normalize( vDir );

    mat._11 = ( v.x * v.x ) * ( 1.0f - fCos ) + fCos;
    mat._12 = ( v.x * v.y ) * ( 1.0f - fCos ) - (v.z * fSin);
    mat._13 = ( v.x * v.z ) * ( 1.0f - fCos ) + (v.y * fSin);

    mat._21 = ( v.y * v.x ) * ( 1.0f - fCos ) + (v.z * fSin);
    mat._22 = ( v.y * v.y ) * ( 1.0f - fCos ) + fCos ;
    mat._23 = ( v.y * v.z ) * ( 1.0f - fCos ) - (v.x * fSin);

    mat._31 = ( v.z * v.x ) * ( 1.0f - fCos ) - (v.y * fSin);
    mat._32 = ( v.z * v.y ) * ( 1.0f - fCos ) + (v.x * fSin);
    mat._33 = ( v.z * v.z ) * ( 1.0f - fCos ) + fCos;

    mat._14 = mat._24 = mat._34 = 0.0f;
    mat._41 = mat._42 = mat._43 = 0.0f;
    mat._44 = 1.0f;
}




//-----------------------------------------------------------------------------
// Name: _DbgOut()
// Desc: Outputs a message to the debug stream
//-----------------------------------------------------------------------------
HRESULT _DbgOut( TCHAR* strFile, DWORD dwLine, HRESULT hr, TCHAR* strMsg )
{
    TCHAR buffer[256];
    wsprintf( buffer, _T("%s(%ld): "), strFile, dwLine );
    OutputDebugString( buffer );
    OutputDebugString( strMsg );

    if( hr )
    {
        wsprintf( buffer, _T("(hr=%08lx)\n"), hr );
        OutputDebugString( buffer );
    }

    OutputDebugString( _T("\n") );

    return hr;
}



