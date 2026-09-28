//-----------------------------------------------------------------------------
// File: D3DMath.h
//
// Desc: Math functions and shortcuts for Direct3D programming.
//
//
// Copyright (C) 1997 Microsoft Corporation. All rights reserved
//-----------------------------------------------------------------------------

#ifndef D3DMATH_H
#define D3DMATH_H

#ifndef __linux__
#include <ddraw.h>
#include <d3d.h>
#else // __linux__
// ddraw.h/d3d.h left out: Direct3D 7 data types become float, oapi::FVECTOR3, oapi::FMATRIX4 and NTVERTEX
#include <cstring>
#include "DrawAPI.h"
#endif // __linux__

// ============================================================================
// Begin stuff added by MS
// ============================================================================
#include "Vecmat.h"

typedef struct {
#ifndef __linux__
	D3DVALUE x, y, z;
	D3DVALUE tu, tv;
#else // __linux__
	float x, y, z;
	float tu, tv;
#endif // __linux__
} POSTEXVERTEX;
#ifndef __linux__
const DWORD POSTEXVERTEXFLAG = D3DFVF_XYZ | D3DFVF_TEX1 | D3DFVF_TEXCOORDSIZE2(0);
#else // __linux__
// POSTEXVERTEXFLAG left out: Direct3D 7 flexible vertex format code
#endif // __linux__

#ifndef __D3DMATH_CPP
extern
#endif
#ifndef __linux__
D3DMATRIX Identity;
#else // __linux__
oapi::FMATRIX4 Identity;
#endif // __linux__

// ============================================================================
// Initialisation of the math module
void D3DMathSetup ();

// ============================================================================
// SetD3DRotation()
// Convert a logical rotation matrix into a D3D transformation matrix, taking into
// account the change in direction (counter-clockwise in the logical interface,
// clockwise in D3D)

#ifndef __linux__
inline void SetD3DRotation (D3DMATRIX &a, const Matrix &r)
#else // __linux__
inline void SetD3DRotation (oapi::FMATRIX4 &a, const Matrix &r)
#endif // __linux__
{
#ifndef __linux__
	a._11 = (FLOAT)r.m11;
	a._12 = (FLOAT)r.m12;
	a._13 = (FLOAT)r.m13;
	a._21 = (FLOAT)r.m21;
	a._22 = (FLOAT)r.m22;
	a._23 = (FLOAT)r.m23;
	a._31 = (FLOAT)r.m31;
	a._32 = (FLOAT)r.m32;
	a._33 = (FLOAT)r.m33;
#else // __linux__
	a.m11 = (FLOAT)r.m11;
	a.m12 = (FLOAT)r.m12;
	a.m13 = (FLOAT)r.m13;
	a.m21 = (FLOAT)r.m21;
	a.m22 = (FLOAT)r.m22;
	a.m23 = (FLOAT)r.m23;
	a.m31 = (FLOAT)r.m31;
	a.m32 = (FLOAT)r.m32;
	a.m33 = (FLOAT)r.m33;
#endif // __linux__
}

// ============================================================================
// SetInvD3DRotation()
// Copy the transpose of a matrix as rotation of a D3D transformation matrix

#ifndef __linux__
inline void SetInvD3DRotation (D3DMATRIX &a, const Matrix &r)
#else // __linux__
inline void SetInvD3DRotation (oapi::FMATRIX4 &a, const Matrix &r)
#endif // __linux__
{
#ifndef __linux__
	a._11 = (FLOAT)r.m11;
	a._12 = (FLOAT)r.m21;
	a._13 = (FLOAT)r.m31;
	a._21 = (FLOAT)r.m12;
	a._22 = (FLOAT)r.m22;
	a._23 = (FLOAT)r.m32;
	a._31 = (FLOAT)r.m13;
	a._32 = (FLOAT)r.m23;
	a._33 = (FLOAT)r.m33;
#else // __linux__
	a.m11 = (FLOAT)r.m11;
	a.m12 = (FLOAT)r.m21;
	a.m13 = (FLOAT)r.m31;
	a.m21 = (FLOAT)r.m12;
	a.m22 = (FLOAT)r.m22;
	a.m23 = (FLOAT)r.m32;
	a.m31 = (FLOAT)r.m13;
	a.m32 = (FLOAT)r.m23;
	a.m33 = (FLOAT)r.m33;
#endif // __linux__
}

// ============================================================================
// SetD3DTranslation()
// Assemble a logical translation vector into a D3D transformation matrix

#ifndef __linux__
inline void SetD3DTranslation (D3DMATRIX &a, const Vector &t)
#else // __linux__
inline void SetD3DTranslation (oapi::FMATRIX4 &a, const Vector &t)
#endif // __linux__
{
#ifndef __linux__
	a._41 = (FLOAT)t.x;
	a._42 = (FLOAT)t.y;
	a._43 = (FLOAT)t.z;
#else // __linux__
	a.m41 = (FLOAT)t.x;
	a.m42 = (FLOAT)t.y;
	a.m43 = (FLOAT)t.z;
#endif // __linux__
}

// ============================================================================
// End stuff added by MS
// ============================================================================


//-----------------------------------------------------------------------------
// Useful Math constants
//-----------------------------------------------------------------------------
const FLOAT g_PI       =  3.14159265358979323846f; // Pi
const FLOAT g_2_PI     =  6.28318530717958623200f; // 2 * Pi
const FLOAT g_PI_DIV_2 =  1.57079632679489655800f; // Pi / 2
const FLOAT g_PI_DIV_4 =  0.78539816339744827900f; // Pi / 4
const FLOAT g_INV_PI   =  0.31830988618379069122f; // 1 / Pi
const FLOAT g_DEGTORAD =  0.01745329251994329547f; // Degrees to Radians
const FLOAT g_RADTODEG = 57.29577951308232286465f; // Radians to Degrees
const FLOAT g_HUGE     =  1.0e+38f;                // Huge number for FLOAT
const FLOAT g_EPSILON  =  1.0e-5f;                 // Tolerance for FLOATs




//-----------------------------------------------------------------------------
// Fuzzy compares (within tolerance)
//-----------------------------------------------------------------------------
inline BOOL D3DMath_IsZero( FLOAT a, FLOAT fTol = g_EPSILON )
{ return ( a <= 0.0f ) ? ( a >= -fTol ) : ( a <= fTol ); }


//-----------------------------------------------------------------------------
// Matrix functions
//-----------------------------------------------------------------------------

// Set a to identity matrix and return the result

#ifndef __linux__
inline VOID VMAT_identity (D3DMATRIX &a)
#else // __linux__
inline VOID VMAT_identity (oapi::FMATRIX4 &a)
#endif // __linux__
{
#ifndef __linux__
	ZeroMemory (&a, sizeof (D3DMATRIX));
	a._11 = a._22 = a._33 = a._44 = 1.0f;
#else // __linux__
	memset ((void*)&a, 0, sizeof (oapi::FMATRIX4)); // ZeroMemory
	a.m11 = a.m22 = a.m33 = a.m44 = 1.0f;
#endif // __linux__
}

#ifndef __linux__
inline D3DMATRIX VMAT_identity ()
#else // __linux__
inline oapi::FMATRIX4 VMAT_identity ()
#endif // __linux__
{
#ifndef __linux__
	D3DMATRIX a;
	ZeroMemory (&a, sizeof (D3DMATRIX));
	a._11 = a._22 = a._33 = a._44 = 1.0f;
#else // __linux__
	oapi::FMATRIX4 a;
	memset ((void*)&a, 0, sizeof (oapi::FMATRIX4)); // ZeroMemory
	a.m11 = a.m22 = a.m33 = a.m44 = 1.0f;
#endif // __linux__
	return a;
}

 // Copy matrix b to a
#ifndef __linux__
inline VOID VMAT_copy (D3DMATRIX &a, const D3DMATRIX &b)
{ memcpy (&a, &b, sizeof (D3DMATRIX)); }
#else // __linux__
inline VOID VMAT_copy (oapi::FMATRIX4 &a, const oapi::FMATRIX4 &b)
{ memcpy ((void*)&a, &b, sizeof (oapi::FMATRIX4)); }
#endif // __linux__

// Set up a as mirror transformation at xz-plane
#ifndef __linux__
inline VOID VMAT_flipy (D3DMATRIX &a)
#else // __linux__
inline VOID VMAT_flipy (oapi::FMATRIX4 &a)
#endif // __linux__
{
#ifndef __linux__
	ZeroMemory (&a, sizeof (D3DMATRIX));
	a._11 = a._33 = a._44 = 1.0f;
	a._22 = -1.0f;
#else // __linux__
	memset ((void*)&a, 0, sizeof (oapi::FMATRIX4)); // ZeroMemory
	a.m11 = a.m33 = a.m44 = 1.0f;
	a.m22 = -1.0f;
#endif // __linux__
}

// Set up a as matrix for ANTICLOCKWISE rotation r around x/y/z-axis
#ifndef __linux__
VOID VMAT_rotx (D3DMATRIX &a, double r);
VOID VMAT_roty (D3DMATRIX &a, double r);
#else // __linux__
VOID VMAT_rotx (oapi::FMATRIX4 &a, double r);
VOID VMAT_roty (oapi::FMATRIX4 &a, double r);
#endif // __linux__

// Create a rotation matrix from a rotation axis and angle
#ifndef __linux__
VOID VMAT_rotation_from_axis (const D3DVECTOR &axis, D3DVALUE angle, D3DMATRIX &R);
#else // __linux__
VOID VMAT_rotation_from_axis (const oapi::FVECTOR3 &axis, float angle, oapi::FMATRIX4 &R);
#endif // __linux__

#ifndef __linux__
VOID    D3DMath_MatrixMultiply( D3DMATRIX& q, D3DMATRIX& a, D3DMATRIX& b );
HRESULT D3DMath_MatrixInvert( D3DMATRIX& q, D3DMATRIX& a );
#else // __linux__
VOID    D3DMath_MatrixMultiply( oapi::FMATRIX4& q, oapi::FMATRIX4& a, oapi::FMATRIX4& b );
int D3DMath_MatrixInvert( oapi::FMATRIX4& q, oapi::FMATRIX4& a );
#endif // __linux__


//-----------------------------------------------------------------------------
// Vector functions
//-----------------------------------------------------------------------------

#ifndef __linux__
inline void D3DMath_SetVector (D3DVECTOR &vec, float x, float y, float z)
#else // __linux__
inline void D3DMath_SetVector (oapi::FVECTOR3 &vec, float x, float y, float z)
#endif // __linux__
{ vec.x = x, vec.y = y, vec.z = z; }

#ifndef __linux__
inline void D3DMath_SetVector (D3DVECTOR &vec, double x, double y, double z)
#else // __linux__
inline void D3DMath_SetVector (oapi::FVECTOR3 &vec, double x, double y, double z)
#endif // __linux__
{ D3DMath_SetVector (vec, (float)x, (float)y, (float)z); }

#ifndef __linux__
inline D3DVECTOR D3DMath_Vector (double x, double y, double z)
{ D3DVECTOR vec; D3DMath_SetVector (vec, x, y, z); return vec; }
#else // __linux__
inline oapi::FVECTOR3 D3DMath_Vector (double x, double y, double z)
{ oapi::FVECTOR3 vec; D3DMath_SetVector (vec, x, y, z); return vec; }
#endif // __linux__

#ifndef __linux__
HRESULT D3DMath_VectorMatrixMultiply( D3DVECTOR& vDest, const D3DVECTOR& vSrc,
                                      const D3DMATRIX& mat);
HRESULT D3DMath_VertexMatrixMultiply( D3DVERTEX& vDest, const D3DVERTEX& vSrc,
                                      const D3DMATRIX& mat );
HRESULT D3DMath_VectorTMatrixMultiply( D3DVECTOR& vDest, const D3DVECTOR& vSrc,
                                      const D3DMATRIX& mat);
#else // __linux__
int D3DMath_VectorMatrixMultiply( oapi::FVECTOR3& vDest, const oapi::FVECTOR3& vSrc,
                                      const oapi::FMATRIX4& mat);
int D3DMath_VertexMatrixMultiply( NTVERTEX& vDest, const NTVERTEX& vSrc,
                                      const oapi::FMATRIX4& mat );
int D3DMath_VectorTMatrixMultiply( oapi::FVECTOR3& vDest, const oapi::FVECTOR3& vSrc,
                                      const oapi::FMATRIX4& mat);
#endif // __linux__


#ifndef __linux__
inline D3DVECTOR
D3DMath_CrossProduct (const D3DVECTOR& v1, const D3DVECTOR& v2)
#else // __linux__
inline oapi::FVECTOR3
D3DMath_CrossProduct (const oapi::FVECTOR3& v1, const oapi::FVECTOR3& v2)
#endif // __linux__
{
#ifndef __linux__
    D3DVECTOR result;
#else // __linux__
    oapi::FVECTOR3 result;
#endif // __linux__
 
    result.x = v1.y * v2.z - v1.z * v2.y;
    result.y = v1.z * v2.x - v1.x * v2.z;
    result.z = v1.x * v2.y - v1.y * v2.x;
 
    return result;
}

inline float
#ifndef __linux__
D3DMath_Length2 (const D3DVECTOR &v)
#else // __linux__
D3DMath_Length2 (const oapi::FVECTOR3 &v)
#endif // __linux__
{
	return v.x*v.x + v.y*v.y + v.z*v.z;
}

inline float
#ifndef __linux__
D3DMath_Length (const D3DVECTOR &v)
#else // __linux__
D3DMath_Length (const oapi::FVECTOR3 &v)
#endif // __linux__
{
	return (float)sqrt (D3DMath_Length2 (v));
}

inline void
#ifndef __linux__
D3DMath_Normalise (D3DVECTOR &v)
#else // __linux__
D3DMath_Normalise (oapi::FVECTOR3 &v)
#endif // __linux__
{
#ifndef __linux__
	D3DVALUE ilen = 1.0f/D3DMath_Length (v);
#else // __linux__
	float ilen = 1.0f/D3DMath_Length (v);
#endif // __linux__
	v.x *= ilen, v.y *= ilen, v.z *= ilen;
}

//-----------------------------------------------------------------------------
// Quaternion functions
//-----------------------------------------------------------------------------
#ifndef __linux__
VOID D3DMath_QuaternionFromRotation( FLOAT& x, FLOAT& y, FLOAT& z, FLOAT& w,
                                     D3DVECTOR& v, FLOAT fTheta );
VOID D3DMath_RotationFromQuaternion( D3DVECTOR& v, FLOAT& fTheta,
                                     FLOAT x, FLOAT y, FLOAT z, FLOAT w );
#else // __linux__
VOID D3DMath_QuaternionFromRotation( FLOAT& x, FLOAT& y, FLOAT& z, FLOAT& w,
                                     oapi::FVECTOR3& v, FLOAT fTheta );
VOID D3DMath_RotationFromQuaternion( oapi::FVECTOR3& v, FLOAT& fTheta,
                                     FLOAT x, FLOAT y, FLOAT z, FLOAT w );
#endif // __linux__
VOID D3DMath_QuaternionFromAngles( FLOAT& x, FLOAT& y, FLOAT& z, FLOAT& w,
                                   FLOAT fYaw, FLOAT fPitch, FLOAT fRoll );
#ifndef __linux__
VOID D3DMath_MatrixFromQuaternion( D3DMATRIX& mat, FLOAT x, FLOAT y, FLOAT z,
                                   FLOAT w );
#else // __linux__
VOID D3DMath_MatrixFromQuaternion( oapi::FMATRIX4& mat, FLOAT x, FLOAT y, FLOAT z,
                                   FLOAT w );
#endif // __linux__
#ifndef __linux__
VOID D3DMath_QuaternionFromMatrix( FLOAT &x, FLOAT &y, FLOAT &z, FLOAT &w,
                                   D3DMATRIX& mat );
#else // __linux__
VOID D3DMath_QuaternionFromMatrix( FLOAT &x, FLOAT &y, FLOAT &z, FLOAT &w,
                                   oapi::FMATRIX4& mat );
#endif // __linux__
VOID D3DMath_QuaternionMultiply( FLOAT& Qx, FLOAT& Qy, FLOAT& Qz, FLOAT& Qw,
                                 FLOAT Ax, FLOAT Ay, FLOAT Az, FLOAT Aw,
                                 FLOAT Bx, FLOAT By, FLOAT Bz, FLOAT Bw );
VOID D3DMath_QuaternionSlerp( FLOAT& Qx, FLOAT& Qy, FLOAT& Qz, FLOAT& Qw,
                              FLOAT Ax, FLOAT Ay, FLOAT Az, FLOAT Aw,
                              FLOAT Bx, FLOAT By, FLOAT Bz, FLOAT Bw,
                              FLOAT fAlpha );


#endif // D3DMATH_H

