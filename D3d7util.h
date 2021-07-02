// ====================================================================================
// File: D3d7util.h
// Desc: Helper functions and typing shortcuts for Direct3D programming.
// ====================================================================================

// ------------------------------------------------------------------------------------
// Vertex formats
// ------------------------------------------------------------------------------------

#ifndef __D3DUTIL_H
#define __D3DUTIL_H

#include <d3d.h>
#include "OrbiterAPI.h"

struct VECTOR2D     { D3DVALUE x, y; };

struct VERTEX_XYZ   { D3DVALUE x, y, z; };                   // transformed vertex
struct VERTEX_XYZH  { D3DVALUE x, y, z, h; };                // untransformed vertex
struct VERTEX_XYZC  { D3DVALUE x, y, z; D3DCOLOR col; };     // untransformed vertex with single colour component
struct VERTEX_XYZHC { D3DVALUE x, y, z, h; D3DCOLOR col; };  // transformed vertex with single colour component

// untransformed unlit vertex with two sets of texture coordinates
struct VERTEX_2TEX  {
	D3DVALUE x, y, z, nx, ny, nz;
	D3DVALUE tu0, tv0, tu1, tv1;
	inline VERTEX_2TEX() {}
	inline VERTEX_2TEX (D3DVECTOR p, D3DVECTOR n, D3DVALUE u0, D3DVALUE v0, D3DVALUE u1, D3DVALUE v1)
	{ x = p.x, y = p.y, z = p.z, nx = n.x, ny = n.y, nz = n.z;
  	  tu0 = u0, tv0 = v0, tu1 = u1, tv1 = v1; }
};
#define FVF_2TEX ( D3DFVF_XYZ | D3DFVF_NORMAL | D3DFVF_TEX2 | D3DFVF_TEXCOORDSIZE2(0) | D3DFVF_TEXCOORDSIZE2(1) )

// transformed lit vertex with 1 colour definition and one set of texture coordinates
struct VERTEX_TL1TEX {
	D3DVALUE x, y, z, rhw;
	D3DCOLOR col;
	D3DVALUE tu, tv;
};
#define FVF_TL1TEX ( D3DFVF_XYZRHW | D3DFVF_DIFFUSE | D3DFVF_TEX1 | D3DFVF_TEXCOORDSIZE2(0) )

// transformed lit vertex with two sets of texture coordinates
struct VERTEX_TL2TEX {
	D3DVALUE x, y, z, rhw;
	D3DCOLOR diff, spec;
	D3DVALUE tu0, tv0, tu1, tv1;
};
#define FVF_TL2TEX ( D3DFVF_XYZRHW | D3DFVF_DIFFUSE | D3DFVF_SPECULAR | D3DFVF_TEX2 | D3DFVF_TEXCOORDSIZE2(0) | D3DFVF_TEXCOORDSIZE2(1) )

VERTEX_XYZ  *GetVertexXYZ  (DWORD n);
VERTEX_XYZC *GetVertexXYZC (DWORD n);
// Return pointer to static vertex buffer of given type of at least size n

inline void MATRIX4toD3DMATRIX (const MATRIX4 &M, D3DMATRIX &D)
{
	D._11 = (D3DVALUE)M.m11;  D._12 = (D3DVALUE)M.m12;  D._13 = (D3DVALUE)M.m13;  D._14 = (D3DVALUE)M.m14;
	D._21 = (D3DVALUE)M.m21;  D._22 = (D3DVALUE)M.m22;  D._23 = (D3DVALUE)M.m23;  D._24 = (D3DVALUE)M.m24;
	D._31 = (D3DVALUE)M.m31;  D._32 = (D3DVALUE)M.m32;  D._33 = (D3DVALUE)M.m33;  D._34 = (D3DVALUE)M.m34;
	D._41 = (D3DVALUE)M.m41;  D._42 = (D3DVALUE)M.m42;  D._43 = (D3DVALUE)M.m43;  D._44 = (D3DVALUE)M.m44;
}

// ------------------------------------------------------------------------------------
// Miscellaneous helper functions
// ------------------------------------------------------------------------------------

#define SAFE_DELETE(p)  { if(p) { delete (p);     (p)=NULL; } }
#define SAFE_RELEASE(p) { if(p) { (p)->Release(); (p)=NULL; } }

#endif // !__D3DUTIL_H