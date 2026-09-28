// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ====================================================================================
// File: D3d7util.h
// Desc: Helper functions and typing shortcuts for Direct3D programming.
// ====================================================================================

// ------------------------------------------------------------------------------------
// Vertex formats
// ------------------------------------------------------------------------------------

#ifndef __D3DUTIL_H
#define __D3DUTIL_H

#ifndef __linux__
#include <d3d.h>
#else // __linux__
// d3d.h left out: Direct3D 7 data types become float, DWORD, oapi::FVECTOR3 and oapi::FMATRIX4
#endif // __linux__
#include "OrbiterAPI.h"
#ifdef __linux__
#include "DrawAPI.h"
#endif // __linux__

#ifndef __linux__
struct VECTOR2D     { D3DVALUE x, y; };
#else // __linux__
struct VECTOR2D     { float x, y; };
#endif // __linux__

#ifndef __linux__
struct VERTEX_XYZ   { D3DVALUE x, y, z; };                   // transformed vertex
struct VERTEX_XYZH  { D3DVALUE x, y, z, h; };                // untransformed vertex
struct VERTEX_XYZC  { D3DVALUE x, y, z; D3DCOLOR col; };     // untransformed vertex with single colour component
struct VERTEX_XYZHC { D3DVALUE x, y, z, h; D3DCOLOR col; };  // transformed vertex with single colour component
#else // __linux__
struct VERTEX_XYZ   { float x, y, z; };                   // transformed vertex
struct VERTEX_XYZH  { float x, y, z, h; };                // untransformed vertex
struct VERTEX_XYZC  { float x, y, z; DWORD col; };     // untransformed vertex with single colour component
struct VERTEX_XYZHC { float x, y, z, h; DWORD col; };  // transformed vertex with single colour component
#endif // __linux__

// untransformed unlit vertex with two sets of texture coordinates
struct VERTEX_2TEX  {
#ifndef __linux__
	D3DVALUE x, y, z, nx, ny, nz;
	D3DVALUE tu0, tv0, tu1, tv1;
#else // __linux__
	float x, y, z, nx, ny, nz;
	float tu0, tv0, tu1, tv1;
#endif // __linux__
	inline VERTEX_2TEX() {}
#ifndef __linux__
	inline VERTEX_2TEX (D3DVECTOR p, D3DVECTOR n, D3DVALUE u0, D3DVALUE v0, D3DVALUE u1, D3DVALUE v1)
#else // __linux__
	inline VERTEX_2TEX (oapi::FVECTOR3 p, oapi::FVECTOR3 n, float u0, float v0, float u1, float v1)
#endif // __linux__
	{ x = p.x, y = p.y, z = p.z, nx = n.x, ny = n.y, nz = n.z;
  	  tu0 = u0, tv0 = v0, tu1 = u1, tv1 = v1; }
};
#ifndef __linux__
#define FVF_2TEX ( D3DFVF_XYZ | D3DFVF_NORMAL | D3DFVF_TEX2 | D3DFVF_TEXCOORDSIZE2(0) | D3DFVF_TEXCOORDSIZE2(1) )
#else // __linux__
// FVF_2TEX left out: Direct3D 7 flexible vertex format code
#endif // __linux__

// transformed lit vertex with 1 colour definition and one set of texture coordinates
struct VERTEX_TL1TEX {
#ifndef __linux__
	D3DVALUE x, y, z, rhw;
	D3DCOLOR col;
	D3DVALUE tu, tv;
#else // __linux__
	float x, y, z, rhw;
	DWORD col;
	float tu, tv;
#endif // __linux__
};
#ifndef __linux__
#define FVF_TL1TEX ( D3DFVF_XYZRHW | D3DFVF_DIFFUSE | D3DFVF_TEX1 | D3DFVF_TEXCOORDSIZE2(0) )
#else // __linux__
// FVF_TL1TEX left out: Direct3D 7 flexible vertex format code
#endif // __linux__

// transformed lit vertex with two sets of texture coordinates
struct VERTEX_TL2TEX {
#ifndef __linux__
	D3DVALUE x, y, z, rhw;
	D3DCOLOR diff, spec;
	D3DVALUE tu0, tv0, tu1, tv1;
#else // __linux__
	float x, y, z, rhw;
	DWORD diff, spec;
	float tu0, tv0, tu1, tv1;
#endif // __linux__
};
#ifndef __linux__
#define FVF_TL2TEX ( D3DFVF_XYZRHW | D3DFVF_DIFFUSE | D3DFVF_SPECULAR | D3DFVF_TEX2 | D3DFVF_TEXCOORDSIZE2(0) | D3DFVF_TEXCOORDSIZE2(1) )
#else // __linux__
// FVF_TL2TEX left out: Direct3D 7 flexible vertex format code
#endif // __linux__

VERTEX_XYZ  *GetVertexXYZ  (DWORD n);
VERTEX_XYZC *GetVertexXYZC (DWORD n);
// Return pointer to static vertex buffer of given type of at least size n

#ifndef __linux__
inline void MATRIX4toD3DMATRIX (const MATRIX4 &M, D3DMATRIX &D)
#else // __linux__
inline void MATRIX4toD3DMATRIX (const MATRIX4 &M, oapi::FMATRIX4 &D)
#endif // __linux__
{
#ifndef __linux__
	D._11 = (D3DVALUE)M.m11;  D._12 = (D3DVALUE)M.m12;  D._13 = (D3DVALUE)M.m13;  D._14 = (D3DVALUE)M.m14;
	D._21 = (D3DVALUE)M.m21;  D._22 = (D3DVALUE)M.m22;  D._23 = (D3DVALUE)M.m23;  D._24 = (D3DVALUE)M.m24;
	D._31 = (D3DVALUE)M.m31;  D._32 = (D3DVALUE)M.m32;  D._33 = (D3DVALUE)M.m33;  D._34 = (D3DVALUE)M.m34;
	D._41 = (D3DVALUE)M.m41;  D._42 = (D3DVALUE)M.m42;  D._43 = (D3DVALUE)M.m43;  D._44 = (D3DVALUE)M.m44;
#else // __linux__
	D.m11 = (float)M.m11;  D.m12 = (float)M.m12;  D.m13 = (float)M.m13;  D.m14 = (float)M.m14;
	D.m21 = (float)M.m21;  D.m22 = (float)M.m22;  D.m23 = (float)M.m23;  D.m24 = (float)M.m24;
	D.m31 = (float)M.m31;  D.m32 = (float)M.m32;  D.m33 = (float)M.m33;  D.m34 = (float)M.m34;
	D.m41 = (float)M.m41;  D.m42 = (float)M.m42;  D.m43 = (float)M.m43;  D.m44 = (float)M.m44;
#endif // __linux__
}

// ------------------------------------------------------------------------------------
// Miscellaneous helper functions
// ------------------------------------------------------------------------------------

#define SAFE_DELETE(p)  { if(p) { delete (p);     (p)=NULL; } }
#ifndef __linux__
#define SAFE_RELEASE(p) { if(p) { (p)->Release(); (p)=NULL; } }
#else // __linux__
// SAFE_RELEASE left out: COM reference counting
#endif // __linux__

#endif // !__D3DUTIL_H
