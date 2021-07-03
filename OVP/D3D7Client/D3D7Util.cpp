// ==============================================================
//   ORBITER VISUALISATION PROJECT (OVP)
//   D3D7 Client module
//   Copyright (C) 2006-2016 Martin Schweiger
//   Dual licensed under GPL v3 and LGPL v3
// ==============================================================

// ==============================================================
// D3d7util.cpp
// Helper functions and typing shortcuts for Direct3D programming.
// ==============================================================

#define STRICT
#include "D3d7util.h"

// =======================================================================
// Some utility methods for D3D vectors and matrices
// ============================================================================

// ============================================================================
// Matrix identity

void D3DMAT_Identity (D3DMATRIX *mat)
{
	ZeroMemory (mat, sizeof (D3DMATRIX));
	mat->_11 = mat->_22 = mat->_33 = mat->_44 = 1.0f;
}

// ============================================================================
// Copy a D3DMATRIX

void D3DMAT_Copy (D3DMATRIX *tgt, const D3DMATRIX *src)
{
	 memcpy (tgt, src, sizeof (D3DMATRIX)); 
}

// ============================================================================
// Copy a rotation matrix into a D3DMATRIX

void D3DMAT_SetRotation (D3DMATRIX *mat, const MATRIX3 *rot)
{
	mat->_11 = (FLOAT)rot->m11;
	mat->_12 = (FLOAT)rot->m12;
	mat->_13 = (FLOAT)rot->m13;
	mat->_21 = (FLOAT)rot->m21;
	mat->_22 = (FLOAT)rot->m22;
	mat->_23 = (FLOAT)rot->m23;
	mat->_31 = (FLOAT)rot->m31;
	mat->_32 = (FLOAT)rot->m32;
	mat->_33 = (FLOAT)rot->m33;
}

// ============================================================================
// Copy the transpose of a matrix as rotation of a D3D transformation matrix

void D3DMAT_SetInvRotation (D3DMATRIX *mat, const MATRIX3 *rot)
{
	mat->_11 = (FLOAT)rot->m11;
	mat->_12 = (FLOAT)rot->m21;
	mat->_13 = (FLOAT)rot->m31;
	mat->_21 = (FLOAT)rot->m12;
	mat->_22 = (FLOAT)rot->m22;
	mat->_23 = (FLOAT)rot->m32;
	mat->_31 = (FLOAT)rot->m13;
	mat->_32 = (FLOAT)rot->m23;
	mat->_33 = (FLOAT)rot->m33;
}

// ============================================================================
// Define a rotation matrix from a rotation axis & rotation angle

void D3DMAT_RotationFromAxis (const D3DVECTOR &axis, D3DVALUE angle, D3DMATRIX *rot)
{
	// Calculate quaternion
	angle *= 0.5f;
	D3DVALUE w = cosf(angle), sina = sinf(angle);
	D3DVALUE x = sina * axis.x;
	D3DVALUE y = sina * axis.y;
	D3DVALUE z = sina * axis.z;

	// Rotation matrix
	D3DVALUE xx = x*x, yy = y*y, zz = z*z;
	D3DVALUE xy = x*y, xz = x*z, yz = y*z;
	D3DVALUE wx = w*x, wy = w*y, wz = w*z;

	rot->_11 = 1 - 2 * (yy+zz);
	rot->_12 =     2 * (xy+wz);
	rot->_13 =     2 * (xz-wy);
	rot->_21 =     2 * (xy-wz);
	rot->_22 = 1 - 2 * (xx+zz);
	rot->_23 =     2 * (yz+wx);
	rot->_31 =     2 * (xz+wy);
	rot->_32 =     2 * (yz-wx);
	rot->_33 = 1 - 2 * (xx+yy);

	rot->_14 = rot->_24 = rot->_34 = rot->_41 = rot->_42 = rot->_43 = 0.0f;
	rot->_44 = 1.0f;
}

// ============================================================================
// Set up a as matrix for ANTICLOCKWISE rotation r around x/y/z-axis

void D3DMAT_RotX  (D3DMATRIX *mat, double r)
{
	double sinr = sin(r), cosr = cos(r);
	ZeroMemory (mat, sizeof (D3DMATRIX));
	mat->_22 = mat->_33 = (FLOAT)cosr;
	mat->_23 = -(mat->_32 = (FLOAT)sinr);
	mat->_11 = mat->_44 = 1.0f;
}

void D3DMAT_RotY (D3DMATRIX *mat, double r)
{
	double sinr = sin(r), cosr = cos(r);
	ZeroMemory (mat, sizeof (D3DMATRIX));
	mat->_11 = mat->_33 = (FLOAT)cosr;
	mat->_31 = -(mat->_13 = (FLOAT)sinr);
	mat->_22 = mat->_44 = 1.0f;
}

// ============================================================================
// Apply a translation vector toa D3D transformation matrix

void D3DMAT_SetTranslation (D3DMATRIX *mat, const VECTOR3 *trans)
{
	mat->_41 = (FLOAT)trans->x;
	mat->_42 = (FLOAT)trans->y;
	mat->_43 = (FLOAT)trans->z;
}

// =======================================================================

void D3DMAT_MatrixMultiply (D3DMATRIX *res, const D3DMATRIX *a, const D3DMATRIX *b)
{
	// *res = *a * *b
    FLOAT* pA = (FLOAT*)a;
    FLOAT* pB = (FLOAT*)b;
    FLOAT pM[16];

    ZeroMemory (pM, sizeof (D3DMATRIX));

    for (WORD i = 0; i < 4; i++) 
        for (WORD j = 0; j < 4; j++) 
            for (WORD k = 0; k < 4; k++)
				//pM[4*i+j] += pA[4*i+k] * pB[4*k+j];
				pM[4*i+j] += pA[4*k+j] * pB[4*i+k];

    memcpy (res, pM, sizeof(D3DMATRIX));
}

// =======================================================================

bool D3DMAT_VectorMatrixMultiply (D3DVECTOR *res, const D3DVECTOR *v, const D3DMATRIX *mat)
{
    FLOAT x = v->x*mat->_11 + v->y*mat->_21 + v->z* mat->_31 + mat->_41;
    FLOAT y = v->x*mat->_12 + v->y*mat->_22 + v->z* mat->_32 + mat->_42;
    FLOAT z = v->x*mat->_13 + v->y*mat->_23 + v->z* mat->_33 + mat->_43;
    FLOAT w = v->x*mat->_14 + v->y*mat->_24 + v->z* mat->_34 + mat->_44;
    
    if (fabs (w) < 1e-5f) return false;

    res->x = x/w;
    res->y = y/w;
    res->z = z/w;
    return true;
}

// =======================================================================
// Name: D3DMath_MatrixInvert()
// Desc: Does the matrix operation: [Q] = inv[A]. Note: this function only
//       works for matrices with [0 0 0 1] for the 4th column.
// =======================================================================

HRESULT D3DMAT_MatrixInvert (D3DMATRIX *res, D3DMATRIX *a)
{
    if( fabs(a->_44 - 1.0f) > .001f)
        return E_INVALIDARG;
    if( fabs(a->_14) > .001f || fabs(a->_24) > .001f || fabs(a->_34) > .001f )
        return E_INVALIDARG;

    FLOAT fDetInv = 1.0f / ( a->_11 * ( a->_22 * a->_33 - a->_23 * a->_32 ) -
                             a->_12 * ( a->_21 * a->_33 - a->_23 * a->_31 ) +
                             a->_13 * ( a->_21 * a->_32 - a->_22 * a->_31 ) );

    res->_11 =  fDetInv * ( a->_22 * a->_33 - a->_23 * a->_32 );
    res->_12 = -fDetInv * ( a->_12 * a->_33 - a->_13 * a->_32 );
    res->_13 =  fDetInv * ( a->_12 * a->_23 - a->_13 * a->_22 );
    res->_14 = 0.0f;

    res->_21 = -fDetInv * ( a->_21 * a->_33 - a->_23 * a->_31 );
    res->_22 =  fDetInv * ( a->_11 * a->_33 - a->_13 * a->_31 );
    res->_23 = -fDetInv * ( a->_11 * a->_23 - a->_13 * a->_21 );
    res->_24 = 0.0f;

    res->_31 =  fDetInv * ( a->_21 * a->_32 - a->_22 * a->_31 );
    res->_32 = -fDetInv * ( a->_11 * a->_32 - a->_12 * a->_31 );
    res->_33 =  fDetInv * ( a->_11 * a->_22 - a->_12 * a->_21 );
    res->_34 = 0.0f;

    res->_41 = -( a->_41 * res->_11 + a->_42 * res->_21 + a->_43 * res->_31 );
    res->_42 = -( a->_41 * res->_12 + a->_42 * res->_22 + a->_43 * res->_32 );
    res->_43 = -( a->_41 * res->_13 + a->_42 * res->_23 + a->_43 * res->_33 );
    res->_44 = 1.0f;

    return S_OK;
}

// =======================================================================

VERTEX_XYZ *GetVertexXYZ (DWORD n)
{
	static DWORD nv = 1;
	static VERTEX_XYZ *v = new VERTEX_XYZ[1];
	if (n > nv) {
		delete []v;
		v = new VERTEX_XYZ[nv = n];
	}
	return v;
}

VERTEX_XYZC *GetVertexXYZC (DWORD n)
{
	static DWORD nv = 1;
	static VERTEX_XYZC *v = new VERTEX_XYZC[1];
	if (n > nv) {
		delete []v;
		v = new VERTEX_XYZC[nv = n];
	}
	return v;
}
