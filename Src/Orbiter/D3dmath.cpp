// Copyright (c) Martin Schweiger
// Licensed under the MIT License

//-----------------------------------------------------------------------------
// File: D3DMath.cpp
//
// Desc: Shortcut macros and functions for using DX objects
//
//
// Copyright (c) 1997-1998 Microsoft Corporation. All rights reserved
//-----------------------------------------------------------------------------

#define __D3DMATH_CPP
#ifndef __linux__
#define D3D_OVERLOADS
#define STRICT
#else // __linux__
// D3D_OVERLOADS and STRICT left out: d3d.h and windows.h switches
#endif // __linux__
#include <math.h>
#include <stdio.h>
#ifndef __linux__
#include "D3DMath.h"
#else // __linux__
#include "D3dmath.h"
#endif // __linux__

#ifndef __linux__
VOID VMAT_rotx (D3DMATRIX &a, double r)
#else // __linux__
VOID VMAT_rotx (oapi::FMATRIX4 &a, double r)
#endif // __linux__
{
	double sinr = sin(r), cosr = cos(r);
#ifndef __linux__
	ZeroMemory (&a, sizeof (D3DMATRIX));
	a._22 = a._33 = (FLOAT)cosr;
	a._23 = -(a._32 = (FLOAT)sinr);
	a._11 = a._44 = 1.0f;
#else // __linux__
	memset ((void*)&a, 0, sizeof (oapi::FMATRIX4)); // ZeroMemory
	a.m22 = a.m33 = (FLOAT)cosr;
	a.m23 = -(a.m32 = (FLOAT)sinr);
	a.m11 = a.m44 = 1.0f;
#endif // __linux__
}

#ifndef __linux__
VOID VMAT_roty (D3DMATRIX &a, double r)
#else // __linux__
VOID VMAT_roty (oapi::FMATRIX4 &a, double r)
#endif // __linux__
{
	double sinr = sin(r), cosr = cos(r);
#ifndef __linux__
	ZeroMemory (&a, sizeof (D3DMATRIX));
	a._11 = a._33 = (FLOAT)cosr;
	a._31 = -(a._13 = (FLOAT)sinr);
	a._22 = a._44 = 1.0f;
#else // __linux__
	memset ((void*)&a, 0, sizeof (oapi::FMATRIX4)); // ZeroMemory
	a.m11 = a.m33 = (FLOAT)cosr;
	a.m31 = -(a.m13 = (FLOAT)sinr);
	a.m22 = a.m44 = 1.0f;
#endif // __linux__
}

// Create a rotation matrix from a rotation axis and angle
#ifndef __linux__
VOID VMAT_rotation_from_axis (const D3DVECTOR &axis, D3DVALUE angle, D3DMATRIX &R)
#else // __linux__
VOID VMAT_rotation_from_axis (const oapi::FVECTOR3 &axis, float angle, oapi::FMATRIX4 &R)
#endif // __linux__
{
	// Calculate quaternion
	angle *= 0.5f;
#ifndef __linux__
	D3DVALUE w = cosf(angle), sina = sinf(angle);
	D3DVALUE x = sina * axis.x;
	D3DVALUE y = sina * axis.y;
	D3DVALUE z = sina * axis.z;
#else // __linux__
	float w = cosf(angle), sina = sinf(angle);
	float x = sina * axis.x;
	float y = sina * axis.y;
	float z = sina * axis.z;
#endif // __linux__

	// Rotation matrix
#ifndef __linux__
	D3DVALUE xx = x*x, yy = y*y, zz = z*z;
	D3DVALUE xy = x*y, xz = x*z, yz = y*z;
	D3DVALUE wx = w*x, wy = w*y, wz = w*z;

	R._11 = 1 - 2 * (yy+zz);
	R._12 =     2 * (xy+wz);
	R._13 =     2 * (xz-wy);
	R._21 =     2 * (xy-wz);
	R._22 = 1 - 2 * (xx+zz);
	R._23 =     2 * (yz+wx);
	R._31 =     2 * (xz+wy);
	R._32 =     2 * (yz-wx);
	R._33 = 1 - 2 * (xx+yy);
#else // __linux__
	float xx = x*x, yy = y*y, zz = z*z;
	float xy = x*y, xz = x*z, yz = y*z;
	float wx = w*x, wy = w*y, wz = w*z;

	R.m11 = 1 - 2 * (yy+zz);
	R.m12 =     2 * (xy+wz);
	R.m13 =     2 * (xz-wy);
	R.m21 =     2 * (xy-wz);
	R.m22 = 1 - 2 * (xx+zz);
	R.m23 =     2 * (yz+wx);
	R.m31 =     2 * (xz+wy);
	R.m32 =     2 * (yz-wx);
	R.m33 = 1 - 2 * (xx+yy);
#endif // __linux__

#ifndef __linux__
	R._14 = R._24 = R._34 = R._41 = R._42 = R._43 = 0.0f;
	R._44 = 1.0f;
#else // __linux__
	R.m14 = R.m24 = R.m34 = R.m41 = R.m42 = R.m43 = 0.0f;
	R.m44 = 1.0f;
#endif // __linux__
}

void D3DMathSetup ()
{
	VMAT_identity (Identity);
}

//-----------------------------------------------------------------------------
// Name: D3DMath_MatrixMultiply()
// Desc: Does the matrix operation: [Q] = [A] * [B].
//-----------------------------------------------------------------------------
#ifndef __linux__
VOID D3DMath_MatrixMultiply( D3DMATRIX& q, D3DMATRIX& a, D3DMATRIX& b )
#else // __linux__
VOID D3DMath_MatrixMultiply( oapi::FMATRIX4& q, oapi::FMATRIX4& a, oapi::FMATRIX4& b )
#endif // __linux__
{
    FLOAT* pA = (FLOAT*)&a;
    FLOAT* pB = (FLOAT*)&b;
    FLOAT  pM[16];

#ifndef __linux__
    ZeroMemory( pM, sizeof(D3DMATRIX) );
#else // __linux__
    memset( pM, 0, sizeof(oapi::FMATRIX4) ); // ZeroMemory
#endif // __linux__

    for( WORD i=0; i<4; i++ ) 
        for( WORD j=0; j<4; j++ ) 
            for( WORD k=0; k<4; k++ ) 
                pM[4*i+j] += pA[4*k+j] * pB[4*i+k];

#ifndef __linux__
    memcpy( &q, pM, sizeof(D3DMATRIX) );
#else // __linux__
    memcpy( (void*)&q, pM, sizeof(oapi::FMATRIX4) );
#endif // __linux__
}




//-----------------------------------------------------------------------------
// Name: D3DMath_MatrixInvert()
// Desc: Does the matrix operation: [Q] = inv[A]. Note: this function only
//       works for matrices with [0 0 0 1] for the 4th column.
//-----------------------------------------------------------------------------
#ifndef __linux__
HRESULT D3DMath_MatrixInvert( D3DMATRIX& q, D3DMATRIX& a )
#else // __linux__
int D3DMath_MatrixInvert( oapi::FMATRIX4& q, oapi::FMATRIX4& a )
#endif // __linux__
{
#ifndef __linux__
    if( fabs(a._44 - 1.0f) > .001f)
        return E_INVALIDARG;
    if( fabs(a._14) > .001f || fabs(a._24) > .001f || fabs(a._34) > .001f )
        return E_INVALIDARG;

    FLOAT fDetInv = 1.0f / ( a._11 * ( a._22 * a._33 - a._23 * a._32 ) -
                             a._12 * ( a._21 * a._33 - a._23 * a._31 ) +
                             a._13 * ( a._21 * a._32 - a._22 * a._31 ) );

    q._11 =  fDetInv * ( a._22 * a._33 - a._23 * a._32 );
    q._12 = -fDetInv * ( a._12 * a._33 - a._13 * a._32 );
    q._13 =  fDetInv * ( a._12 * a._23 - a._13 * a._22 );
    q._14 = 0.0f;

    q._21 = -fDetInv * ( a._21 * a._33 - a._23 * a._31 );
    q._22 =  fDetInv * ( a._11 * a._33 - a._13 * a._31 );
    q._23 = -fDetInv * ( a._11 * a._23 - a._13 * a._21 );
    q._24 = 0.0f;

    q._31 =  fDetInv * ( a._21 * a._32 - a._22 * a._31 );
    q._32 = -fDetInv * ( a._11 * a._32 - a._12 * a._31 );
    q._33 =  fDetInv * ( a._11 * a._22 - a._12 * a._21 );
    q._34 = 0.0f;

    q._41 = -( a._41 * q._11 + a._42 * q._21 + a._43 * q._31 );
    q._42 = -( a._41 * q._12 + a._42 * q._22 + a._43 * q._32 );
    q._43 = -( a._41 * q._13 + a._42 * q._23 + a._43 * q._33 );
    q._44 = 1.0f;
#else // __linux__
    if( fabs(a.m44 - 1.0f) > .001f)
        return -1; // E_INVALIDARG
    if( fabs(a.m14) > .001f || fabs(a.m24) > .001f || fabs(a.m34) > .001f )
        return -1; // E_INVALIDARG

    FLOAT fDetInv = 1.0f / ( a.m11 * ( a.m22 * a.m33 - a.m23 * a.m32 ) -
                             a.m12 * ( a.m21 * a.m33 - a.m23 * a.m31 ) +
                             a.m13 * ( a.m21 * a.m32 - a.m22 * a.m31 ) );

    q.m11 =  fDetInv * ( a.m22 * a.m33 - a.m23 * a.m32 );
    q.m12 = -fDetInv * ( a.m12 * a.m33 - a.m13 * a.m32 );
    q.m13 =  fDetInv * ( a.m12 * a.m23 - a.m13 * a.m22 );
    q.m14 = 0.0f;

    q.m21 = -fDetInv * ( a.m21 * a.m33 - a.m23 * a.m31 );
    q.m22 =  fDetInv * ( a.m11 * a.m33 - a.m13 * a.m31 );
    q.m23 = -fDetInv * ( a.m11 * a.m23 - a.m13 * a.m21 );
    q.m24 = 0.0f;

    q.m31 =  fDetInv * ( a.m21 * a.m32 - a.m22 * a.m31 );
    q.m32 = -fDetInv * ( a.m11 * a.m32 - a.m12 * a.m31 );
    q.m33 =  fDetInv * ( a.m11 * a.m22 - a.m12 * a.m21 );
    q.m34 = 0.0f;

    q.m41 = -( a.m41 * q.m11 + a.m42 * q.m21 + a.m43 * q.m31 );
    q.m42 = -( a.m41 * q.m12 + a.m42 * q.m22 + a.m43 * q.m32 );
    q.m43 = -( a.m41 * q.m13 + a.m42 * q.m23 + a.m43 * q.m33 );
    q.m44 = 1.0f;
#endif // __linux__

#ifndef __linux__
    return S_OK;
#else // __linux__
    return 0; // S_OK
#endif // __linux__
}




//-----------------------------------------------------------------------------
// Name: D3DMath_VectorMatrixMultiply()
// Desc: Multiplies a vector by a matrix
//-----------------------------------------------------------------------------
#ifndef __linux__
HRESULT D3DMath_VectorMatrixMultiply( D3DVECTOR& vDest, const D3DVECTOR& vSrc,
                                      const D3DMATRIX& mat)
#else // __linux__
int D3DMath_VectorMatrixMultiply( oapi::FVECTOR3& vDest, const oapi::FVECTOR3& vSrc,
                                      const oapi::FMATRIX4& mat)
#endif // __linux__
{
#ifndef __linux__
    FLOAT x = vSrc.x*mat._11 + vSrc.y*mat._21 + vSrc.z* mat._31 + mat._41;
    FLOAT y = vSrc.x*mat._12 + vSrc.y*mat._22 + vSrc.z* mat._32 + mat._42;
    FLOAT z = vSrc.x*mat._13 + vSrc.y*mat._23 + vSrc.z* mat._33 + mat._43;
    FLOAT w = vSrc.x*mat._14 + vSrc.y*mat._24 + vSrc.z* mat._34 + mat._44;
#else // __linux__
    FLOAT x = vSrc.x*mat.m11 + vSrc.y*mat.m21 + vSrc.z* mat.m31 + mat.m41;
    FLOAT y = vSrc.x*mat.m12 + vSrc.y*mat.m22 + vSrc.z* mat.m32 + mat.m42;
    FLOAT z = vSrc.x*mat.m13 + vSrc.y*mat.m23 + vSrc.z* mat.m33 + mat.m43;
    FLOAT w = vSrc.x*mat.m14 + vSrc.y*mat.m24 + vSrc.z* mat.m34 + mat.m44;
#endif // __linux__
    
    if( fabs( w ) < g_EPSILON )
#ifndef __linux__
        return E_INVALIDARG;
#else // __linux__
        return -1; // E_INVALIDARG
#endif // __linux__

    vDest.x = x/w;
    vDest.y = y/w;
    vDest.z = z/w;

#ifndef __linux__
    return S_OK;
#else // __linux__
    return 0; // S_OK
#endif // __linux__
}




//-----------------------------------------------------------------------------
// Name: D3DMath_VectorTMatrixMultiply()
// Desc: Multiplies a vector by the transpose of a matrix
//-----------------------------------------------------------------------------
#ifndef __linux__
HRESULT D3DMath_VectorTMatrixMultiply( D3DVECTOR& vDest, const D3DVECTOR& vSrc,
                                      const D3DMATRIX& mat)
#else // __linux__
int D3DMath_VectorTMatrixMultiply( oapi::FVECTOR3& vDest, const oapi::FVECTOR3& vSrc,
                                      const oapi::FMATRIX4& mat)
#endif // __linux__
{
#ifndef __linux__
    FLOAT x = vSrc.x*mat._11 + vSrc.y*mat._12 + vSrc.z* mat._13 + mat._14;
    FLOAT y = vSrc.x*mat._21 + vSrc.y*mat._22 + vSrc.z* mat._23 + mat._24;
    FLOAT z = vSrc.x*mat._31 + vSrc.y*mat._32 + vSrc.z* mat._33 + mat._34;
    FLOAT w = vSrc.x*mat._41 + vSrc.y*mat._42 + vSrc.z* mat._43 + mat._44;
#else // __linux__
    FLOAT x = vSrc.x*mat.m11 + vSrc.y*mat.m12 + vSrc.z* mat.m13 + mat.m14;
    FLOAT y = vSrc.x*mat.m21 + vSrc.y*mat.m22 + vSrc.z* mat.m23 + mat.m24;
    FLOAT z = vSrc.x*mat.m31 + vSrc.y*mat.m32 + vSrc.z* mat.m33 + mat.m34;
    FLOAT w = vSrc.x*mat.m41 + vSrc.y*mat.m42 + vSrc.z* mat.m43 + mat.m44;
#endif // __linux__
    
    if( fabs( w ) < g_EPSILON )
#ifndef __linux__
        return E_INVALIDARG;
#else // __linux__
        return -1; // E_INVALIDARG
#endif // __linux__

    vDest.x = x/w;
    vDest.y = y/w;
    vDest.z = z/w;

#ifndef __linux__
    return S_OK;
#else // __linux__
    return 0; // S_OK
#endif // __linux__
}




//-----------------------------------------------------------------------------
// Name: D3DMath_VertexMatrixMultiply()
// Desc: Multiplies a vertex by a matrix
//-----------------------------------------------------------------------------
#ifndef __linux__
HRESULT D3DMath_VertexMatrixMultiply( D3DVERTEX& vDest, const D3DVERTEX& vSrc,
                                      const D3DMATRIX& mat )
#else // __linux__
int D3DMath_VertexMatrixMultiply( NTVERTEX& vDest, const NTVERTEX& vSrc,
                                      const oapi::FMATRIX4& mat )
#endif // __linux__
{
#ifndef __linux__
    HRESULT    hr;
    D3DVECTOR* pSrcVec  = (D3DVECTOR*)&vSrc.x;
    D3DVECTOR* pDestVec = (D3DVECTOR*)&vDest.x;
#else // __linux__
    int        hr;
    oapi::FVECTOR3* pSrcVec  = (oapi::FVECTOR3*)&vSrc.x;
    oapi::FVECTOR3* pDestVec = (oapi::FVECTOR3*)&vDest.x;
#endif // __linux__

#ifndef __linux__
    if( SUCCEEDED( hr = D3DMath_VectorMatrixMultiply( *pDestVec, *pSrcVec,
                                                      mat ) ) )
#else // __linux__
    if( !( hr = D3DMath_VectorMatrixMultiply( *pDestVec, *pSrcVec,
                                                      mat ) ) )
#endif // __linux__
    {
#ifndef __linux__
        pSrcVec  = (D3DVECTOR*)&vSrc.nx;
        pDestVec = (D3DVECTOR*)&vDest.nx;
#else // __linux__
        pSrcVec  = (oapi::FVECTOR3*)&vSrc.nx;
        pDestVec = (oapi::FVECTOR3*)&vDest.nx;
#endif // __linux__
        hr = D3DMath_VectorMatrixMultiply( *pDestVec, *pSrcVec, mat );
    }
    return hr;
}




//-----------------------------------------------------------------------------
// Name: D3DMath_QuaternionFromRotation()
// Desc: Converts a normalized axis and angle to a unit quaternion.
//-----------------------------------------------------------------------------
#ifndef __linux__
VOID D3DMath_QuaternionFromRotation( FLOAT& x, FLOAT& y, FLOAT& z, FLOAT& w,
                                     D3DVECTOR& v, FLOAT fTheta )
#else // __linux__
VOID D3DMath_QuaternionFromRotation( FLOAT& x, FLOAT& y, FLOAT& z, FLOAT& w,
                                     oapi::FVECTOR3& v, FLOAT fTheta )
#endif // __linux__
{
    x = (FLOAT)sin(fTheta/2) * v.x;
    y = (FLOAT)sin(fTheta/2) * v.y;
    z = (FLOAT)sin(fTheta/2) * v.z;
    w = (FLOAT)cos(fTheta/2);
}




//-----------------------------------------------------------------------------
// Name: D3DMath_RotationFromQuaternion()
// Desc: Converts a normalized axis and angle to a unit quaternion.
//-----------------------------------------------------------------------------
#ifndef __linux__
VOID D3DMath_RotationFromQuaternion( D3DVECTOR& v, FLOAT& fTheta,
                                     FLOAT x, FLOAT y, FLOAT z, FLOAT w )
#else // __linux__
VOID D3DMath_RotationFromQuaternion( oapi::FVECTOR3& v, FLOAT& fTheta,
                                     FLOAT x, FLOAT y, FLOAT z, FLOAT w )
#endif // __linux__
                                      
{
    fTheta = (FLOAT)( acos(w) * 2 );
    v.x    = (FLOAT)( x / sin(fTheta/2) );
    v.y    = (FLOAT)( y / sin(fTheta/2) );
    v.z    = (FLOAT)( z / sin(fTheta/2) );
}




//-----------------------------------------------------------------------------
// Name: D3DMath_QuaternionFromAngles()
// Desc: Converts euler angles to a unit quaternion.
//-----------------------------------------------------------------------------
VOID D3DMath_QuaternionFromAngles( FLOAT& x, FLOAT& y, FLOAT& z, FLOAT& w,
                                   FLOAT fYaw, FLOAT fPitch, FLOAT fRoll )
                                        
{
    FLOAT fSinYaw   = (FLOAT)sin(fYaw/2);
    FLOAT fSinPitch = (FLOAT)sin(fPitch/2);
    FLOAT fSinRoll  = (FLOAT)sin(fRoll/2);
    FLOAT fCosYaw   = (FLOAT)cos(fYaw/2);
    FLOAT fCosPitch = (FLOAT)cos(fPitch/2);
    FLOAT fCosRoll  = (FLOAT)cos(fRoll/2);

    x = fSinRoll * fCosPitch * fCosYaw - fCosRoll * fSinPitch * fSinYaw;
    y = fCosRoll * fSinPitch * fCosYaw + fSinRoll * fCosPitch * fSinYaw;
    z = fCosRoll * fCosPitch * fSinYaw - fSinRoll * fSinPitch * fCosYaw;
    w = fCosRoll * fCosPitch * fCosYaw + fSinRoll * fSinPitch * fSinYaw;
}




//-----------------------------------------------------------------------------
// Name: D3DMath_MatrixFromQuaternion()
// Desc: Converts a unit quaternion into a rotation matrix.
//-----------------------------------------------------------------------------
#ifndef __linux__
VOID D3DMath_MatrixFromQuaternion( D3DMATRIX& mat, FLOAT x, FLOAT y, FLOAT z,
                                   FLOAT w )
#else // __linux__
VOID D3DMath_MatrixFromQuaternion( oapi::FMATRIX4& mat, FLOAT x, FLOAT y, FLOAT z,
                                   FLOAT w )
#endif // __linux__
{
    FLOAT xx = x*x; FLOAT yy = y*y; FLOAT zz = z*z;
    FLOAT xy = x*y; FLOAT xz = x*z; FLOAT yz = y*z;
    FLOAT wx = w*x; FLOAT wy = w*y; FLOAT wz = w*z;
    
#ifndef __linux__
    mat._11 = 1 - 2 * ( yy + zz ); 
    mat._12 =     2 * ( xy - wz );
    mat._13 =     2 * ( xz + wy );

    mat._21 =     2 * ( xy + wz );
    mat._22 = 1 - 2 * ( xx + zz );
    mat._23 =     2 * ( yz - wx );

    mat._31 =     2 * ( xz - wy );
    mat._32 =     2 * ( yz + wx );
    mat._33 = 1 - 2 * ( xx + yy );

    mat._14 = mat._24 = mat._34 = 0.0f;
    mat._41 = mat._42 = mat._43 = 0.0f;
    mat._44 = 1.0f;
#else // __linux__
    mat.m11 = 1 - 2 * ( yy + zz ); 
    mat.m12 =     2 * ( xy - wz );
    mat.m13 =     2 * ( xz + wy );

    mat.m21 =     2 * ( xy + wz );
    mat.m22 = 1 - 2 * ( xx + zz );
    mat.m23 =     2 * ( yz - wx );

    mat.m31 =     2 * ( xz - wy );
    mat.m32 =     2 * ( yz + wx );
    mat.m33 = 1 - 2 * ( xx + yy );

    mat.m14 = mat.m24 = mat.m34 = 0.0f;
    mat.m41 = mat.m42 = mat.m43 = 0.0f;
    mat.m44 = 1.0f;
#endif // __linux__
}




//-----------------------------------------------------------------------------
// Name: D3DMath_QuaternionFromMatrix()
// Desc: Converts a rotation matrix into a unit quaternion.
//-----------------------------------------------------------------------------
#ifndef __linux__
VOID D3DMath_QuaternionFromMatrix( FLOAT& x, FLOAT& y, FLOAT& z, FLOAT& w,
                                   D3DMATRIX& mat )
#else // __linux__
VOID D3DMath_QuaternionFromMatrix( FLOAT& x, FLOAT& y, FLOAT& z, FLOAT& w,
                                   oapi::FMATRIX4& mat )
#endif // __linux__
{
#ifndef __linux__
    if( mat._11 + mat._22 + mat._33 > 0.0f )
#else // __linux__
    if( mat.m11 + mat.m22 + mat.m33 > 0.0f )
#endif // __linux__
    {
#ifndef __linux__
        FLOAT s = (FLOAT)sqrt( mat._11 + mat._22 + mat._33 + mat._44 );
#else // __linux__
        FLOAT s = (FLOAT)sqrt( mat.m11 + mat.m22 + mat.m33 + mat.m44 );
#endif // __linux__

#ifndef __linux__
        x = (mat._23-mat._32) / (2*s);
        y = (mat._31-mat._13) / (2*s);
        z = (mat._12-mat._21) / (2*s);
#else // __linux__
        x = (mat.m23-mat.m32) / (2*s);
        y = (mat.m31-mat.m13) / (2*s);
        z = (mat.m12-mat.m21) / (2*s);
#endif // __linux__
        w = 0.5f * s;
    }
    else
    {


    }
    FLOAT xx = x*x; FLOAT yy = y*y; FLOAT zz = z*z;
    FLOAT xy = x*y; FLOAT xz = x*z; FLOAT yz = y*z;
    FLOAT wx = w*x; FLOAT wy = w*y; FLOAT wz = w*z;
    
#ifndef __linux__
    mat._11 = 1 - 2 * ( yy + zz ); 
    mat._12 =     2 * ( xy - wz );
    mat._13 =     2 * ( xz + wy );

    mat._21 =     2 * ( xy + wz );
    mat._22 = 1 - 2 * ( xx + zz );
    mat._23 =     2 * ( yz - wx );

    mat._31 =     2 * ( xz - wy );
    mat._32 =     2 * ( yz + wx );
    mat._33 = 1 - 2 * ( xx + yy );

    mat._14 = mat._24 = mat._34 = 0.0f;
    mat._41 = mat._42 = mat._43 = 0.0f;
    mat._44 = 1.0f;
#else // __linux__
    mat.m11 = 1 - 2 * ( yy + zz ); 
    mat.m12 =     2 * ( xy - wz );
    mat.m13 =     2 * ( xz + wy );

    mat.m21 =     2 * ( xy + wz );
    mat.m22 = 1 - 2 * ( xx + zz );
    mat.m23 =     2 * ( yz - wx );

    mat.m31 =     2 * ( xz - wy );
    mat.m32 =     2 * ( yz + wx );
    mat.m33 = 1 - 2 * ( xx + yy );

    mat.m14 = mat.m24 = mat.m34 = 0.0f;
    mat.m41 = mat.m42 = mat.m43 = 0.0f;
    mat.m44 = 1.0f;
#endif // __linux__
}




//-----------------------------------------------------------------------------
// Name: D3DMath_QuaternionMultiply()
// Desc: Mulitples two quaternions together as in {Q} = {A} * {B}.
//-----------------------------------------------------------------------------
VOID D3DMath_QuaternionMultiply( FLOAT& Qx, FLOAT& Qy, FLOAT& Qz, FLOAT& Qw,
                                  FLOAT Ax, FLOAT Ay, FLOAT Az, FLOAT Aw,
                                  FLOAT Bx, FLOAT By, FLOAT Bz, FLOAT Bw )
{
    FLOAT Dx = Bw*Ax + Bx*Aw + By*Az + Bz*Ay;
    FLOAT Dy = Bw*Ay + By*Aw + Bz*Ax + Bx*Az;
    FLOAT Dz = Bw*Az + Bz*Aw + Bx*Ay + By*Ax;
    FLOAT Dw = Bw*Aw + Bx*Ax + By*Ay + Bz*Az;

    Qx = Dx; Qy = Dy; Qz = Dz; Qw = Dw;
}




//-----------------------------------------------------------------------------
// Name: D3DMath_SlerpQuaternions()
// Desc: Compute a quaternion which is the spherical linear interpolation
//       between two other quaternions by dvFraction.
//-----------------------------------------------------------------------------
VOID D3DMath_QuaternionSlerp( FLOAT& Qx, FLOAT& Qy, FLOAT& Qz, FLOAT& Qw,
                              FLOAT Ax, FLOAT Ay, FLOAT Az, FLOAT Aw,
                              FLOAT Bx, FLOAT By, FLOAT Bz, FLOAT Bw,
                              FLOAT fAlpha )
{
    FLOAT fScale1;
    FLOAT fScale2;

    // Compute dot product, aka cos(theta):
    FLOAT fCosTheta = Ax*Bx + Ay*By + Az*Bz + Aw*Bw;

    if( fCosTheta < 0.0f )
    {
        // Flip start quaternion
        Ax = -Ax; Ay = -Ay; Ax = -Az; Aw = -Aw;
        fCosTheta = -fCosTheta;
    }

    if( fCosTheta + 1.0f > 0.05f )
    {
        // If the quaternions are close, use linear interploation
        if( 1.0f - fCosTheta < 0.05f )
        {
            fScale1 = 1.0f - fAlpha;
            fScale2 = fAlpha;
        }
        else // Otherwise, do spherical interpolation
        {
            FLOAT fTheta    = (FLOAT)acos( fCosTheta );
            FLOAT fSinTheta = (FLOAT)sin( fTheta );
            
            fScale1 = (FLOAT)sin( fTheta * (1.0f-fAlpha) ) / fSinTheta;
            fScale2 = (FLOAT)sin( fTheta * fAlpha ) / fSinTheta;
        }
    }
    else
    {
        Bx = -Ay;
        By =  Ax;
        Bz = -Aw;
        Bw =  Az;
        fScale1 = (FLOAT)sin( g_PI * (0.5f - fAlpha) );
        fScale2 = (FLOAT)sin( g_PI * fAlpha );
    }

    Qx = fScale1 * Ax + fScale2 * Bx;
    Qy = fScale1 * Ay + fScale2 * By;
    Qz = fScale1 * Az + fScale2 * Bz;
    Qw = fScale1 * Aw + fScale2 * Bw;
}



