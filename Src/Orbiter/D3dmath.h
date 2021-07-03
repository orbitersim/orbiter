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

#include <ddraw.h>
#include <d3d.h>

// ============================================================================
// Begin stuff added by MS
// ============================================================================
#include "Vecmat.h"

typedef struct {
	D3DVALUE x, y, z;
	D3DVALUE tu, tv;
} POSTEXVERTEX;
const DWORD POSTEXVERTEXFLAG = D3DFVF_XYZ | D3DFVF_TEX1 | D3DFVF_TEXCOORDSIZE2(0);

#ifndef __D3DMATH_CPP
extern
#endif
D3DMATRIX Identity;

// ============================================================================
// Initialisation of the math module
void D3DMathSetup ();

// ============================================================================
// SetD3DRotation()
// Convert a logical rotation matrix into a D3D transformation matrix, taking into
// account the change in direction (counter-clockwise in the logical interface,
// clockwise in D3D)

inline void SetD3DRotation (D3DMATRIX &a, const Matrix &r)
{
	a._11 = (FLOAT)r.m11;
	a._12 = (FLOAT)r.m12;
	a._13 = (FLOAT)r.m13;
	a._21 = (FLOAT)r.m21;
	a._22 = (FLOAT)r.m22;
	a._23 = (FLOAT)r.m23;
	a._31 = (FLOAT)r.m31;
	a._32 = (FLOAT)r.m32;
	a._33 = (FLOAT)r.m33;
}

// ============================================================================
// SetInvD3DRotation()
// Copy the transpose of a matrix as rotation of a D3D transformation matrix

inline void SetInvD3DRotation (D3DMATRIX &a, const Matrix &r)
{
	a._11 = (FLOAT)r.m11;
	a._12 = (FLOAT)r.m21;
	a._13 = (FLOAT)r.m31;
	a._21 = (FLOAT)r.m12;
	a._22 = (FLOAT)r.m22;
	a._23 = (FLOAT)r.m32;
	a._31 = (FLOAT)r.m13;
	a._32 = (FLOAT)r.m23;
	a._33 = (FLOAT)r.m33;
}

// ============================================================================
// SetD3DTranslation()
// Assemble a logical translation vector into a D3D transformation matrix

inline void SetD3DTranslation (D3DMATRIX &a, const Vector &t)
{
	a._41 = (FLOAT)t.x;
	a._42 = (FLOAT)t.y;
	a._43 = (FLOAT)t.z;
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

inline VOID VMAT_identity (D3DMATRIX &a)
{
	ZeroMemory (&a, sizeof (D3DMATRIX));
	a._11 = a._22 = a._33 = a._44 = 1.0f;
}

inline D3DMATRIX VMAT_identity ()
{
	D3DMATRIX a;
	ZeroMemory (&a, sizeof (D3DMATRIX));
	a._11 = a._22 = a._33 = a._44 = 1.0f;
	return a;
}

 // Copy matrix b to a
inline VOID VMAT_copy (D3DMATRIX &a, const D3DMATRIX &b)
{ memcpy (&a, &b, sizeof (D3DMATRIX)); }

// Set up a as mirror transformation at xz-plane
inline VOID VMAT_flipy (D3DMATRIX &a)
{
	ZeroMemory (&a, sizeof (D3DMATRIX));
	a._11 = a._33 = a._44 = 1.0f;
	a._22 = -1.0f;
}

// Set up a as matrix for ANTICLOCKWISE rotation r around x/y/z-axis
VOID VMAT_rotx (D3DMATRIX &a, double r);
VOID VMAT_roty (D3DMATRIX &a, double r);

// Create a rotation matrix from a rotation axis and angle
VOID VMAT_rotation_from_axis (const D3DVECTOR &axis, D3DVALUE angle, D3DMATRIX &R);

VOID    D3DMath_MatrixMultiply( D3DMATRIX& q, D3DMATRIX& a, D3DMATRIX& b );
HRESULT D3DMath_MatrixInvert( D3DMATRIX& q, D3DMATRIX& a );


//-----------------------------------------------------------------------------
// Vector functions
//-----------------------------------------------------------------------------

inline void D3DMath_SetVector (D3DVECTOR &vec, float x, float y, float z)
{ vec.x = x, vec.y = y, vec.z = z; }

inline void D3DMath_SetVector (D3DVECTOR &vec, double x, double y, double z)
{ D3DMath_SetVector (vec, (float)x, (float)y, (float)z); }

inline D3DVECTOR D3DMath_Vector (double x, double y, double z)
{ D3DVECTOR vec; D3DMath_SetVector (vec, x, y, z); return vec; }

HRESULT D3DMath_VectorMatrixMultiply( D3DVECTOR& vDest, const D3DVECTOR& vSrc,
                                      const D3DMATRIX& mat);
HRESULT D3DMath_VertexMatrixMultiply( D3DVERTEX& vDest, const D3DVERTEX& vSrc,
                                      const D3DMATRIX& mat );
HRESULT D3DMath_VectorTMatrixMultiply( D3DVECTOR& vDest, const D3DVECTOR& vSrc,
                                      const D3DMATRIX& mat);


inline D3DVECTOR
D3DMath_CrossProduct (const D3DVECTOR& v1, const D3DVECTOR& v2)
{
    D3DVECTOR result;
 
    result.x = v1.y * v2.z - v1.z * v2.y;
    result.y = v1.z * v2.x - v1.x * v2.z;
    result.z = v1.x * v2.y - v1.y * v2.x;
 
    return result;
}

inline float
D3DMath_Length2 (const D3DVECTOR &v)
{
	return v.x*v.x + v.y*v.y + v.z*v.z;
}

inline float
D3DMath_Length (const D3DVECTOR &v)
{
	return (float)sqrt (D3DMath_Length2 (v));
}

inline void
D3DMath_Normalise (D3DVECTOR &v)
{
	D3DVALUE ilen = 1.0f/D3DMath_Length (v);
	v.x *= ilen, v.y *= ilen, v.z *= ilen;
}

//-----------------------------------------------------------------------------
// Quaternion functions
//-----------------------------------------------------------------------------
VOID D3DMath_QuaternionFromRotation( FLOAT& x, FLOAT& y, FLOAT& z, FLOAT& w,
                                     D3DVECTOR& v, FLOAT fTheta );
VOID D3DMath_RotationFromQuaternion( D3DVECTOR& v, FLOAT& fTheta,
                                     FLOAT x, FLOAT y, FLOAT z, FLOAT w );
VOID D3DMath_QuaternionFromAngles( FLOAT& x, FLOAT& y, FLOAT& z, FLOAT& w,
                                   FLOAT fYaw, FLOAT fPitch, FLOAT fRoll );
VOID D3DMath_MatrixFromQuaternion( D3DMATRIX& mat, FLOAT x, FLOAT y, FLOAT z,
                                   FLOAT w );
VOID D3DMath_QuaternionFromMatrix( FLOAT &x, FLOAT &y, FLOAT &z, FLOAT &w,
                                   D3DMATRIX& mat );
VOID D3DMath_QuaternionMultiply( FLOAT& Qx, FLOAT& Qy, FLOAT& Qz, FLOAT& Qw,
                                 FLOAT Ax, FLOAT Ay, FLOAT Az, FLOAT Aw,
                                 FLOAT Bx, FLOAT By, FLOAT Bz, FLOAT Bw );
VOID D3DMath_QuaternionSlerp( FLOAT& Qx, FLOAT& Qy, FLOAT& Qz, FLOAT& Qw,
                              FLOAT Ax, FLOAT Ay, FLOAT Az, FLOAT Aw,
                              FLOAT Bx, FLOAT By, FLOAT Bz, FLOAT Bw,
                              FLOAT fAlpha );


#endif // D3DMATH_H

