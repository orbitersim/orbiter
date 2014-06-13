// ==============================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2007 Martin Schweiger
//				 2012 Jarmo Nikkanen
// ==============================================================

#ifndef __D3DUTIL_H
#define __D3DUTIL_H

#include "OrbiterAPI.h"
#include "Log.h"
#include <d3d9.h> 
#include <d3dx9.h>

#ifdef _DEBUG

#ifndef _TRACE
#define _TRACE { LogMsg("[TRACE] %s Line:%d %s",__FILE__,__LINE__,__FUNCTION__); }
#endif

#ifndef _TRACER
#define _TRACER { LogMsg("[TRACE] %s Line:%d %s",__FILE__,__LINE__,__FUNCTION__); }
#endif

//#ifndef _TRACER
//#define _TRACER {  QueryPerformanceCounter((LARGE_INTEGER*)&qpcRef); LogMsg("[TRACE] %s Line:%d %s",__FILE__,__LINE__,__FUNCTION__); }
//#endif

#else

#ifndef _TRACE
#define _TRACE
#endif

#ifndef _TRACER
#define _TRACER
#endif

#endif



/*
#ifndef HR
#define HR(x) x;
#endif
*/

#ifndef HR
#define HR(x)                                      \
{                                                  \
	HRESULT hr = x;                                \
	if(FAILED(hr))                                 \
	{												\
		LogErr("%s Line:%d Error:%d %s",__FILE__,__LINE__,hr,(#x)); \
	}                                                \
}
#endif

/*
#ifndef __TRY
#define __TRY __try
#endif

#ifndef __EXCEPT
#define __EXCEPT(x) __except(x)
#endif
*/

#ifndef __TRY
#define __TRY
#endif

#ifndef __EXCEPT
#define __EXCEPT(x) if (false)
#endif




#define PI 3.141592653589793238462643383279

#define SURFACE(x) ((class D3D9ClientSurface *)x)

// ------------------------------------------------------------------------------------
// Vertex Declaration equal to NTVERTEX
// ------------------------------------------------------------------------------------

const D3DVERTEXELEMENT9 BAVertexDecl[] = {
	{0, 0,  D3DDECLTYPE_FLOAT3,   D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_POSITION, 0},
	{0, 12, D3DDECLTYPE_FLOAT3,   D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_NORMAL, 0},
	{0, 24, D3DDECLTYPE_FLOAT4,   D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_TEXCOORD, 0},
	{0, 40, D3DDECLTYPE_FLOAT2,   D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_TEXCOORD, 1},
	{0, 48, D3DDECLTYPE_D3DCOLOR, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_COLOR, 0},
	D3DDECL_END()
};

const D3DVERTEXELEMENT9 NTVertexDecl[] = {
	{0, 0,  D3DDECLTYPE_FLOAT3, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_POSITION, 0},
	{0, 12, D3DDECLTYPE_FLOAT3, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_NORMAL, 0},
	{0, 24, D3DDECLTYPE_FLOAT2, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_TEXCOORD, 0},
	D3DDECL_END()
};

const D3DVERTEXELEMENT9 MeshVertexDecl[] = {
	{0, 0,  D3DDECLTYPE_FLOAT3, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_POSITION, 0},
	{0, 12, D3DDECLTYPE_FLOAT3, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_NORMAL, 0},
	{0, 24, D3DDECLTYPE_FLOAT3, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_TANGENT, 0},
	{0, 36, D3DDECLTYPE_FLOAT2, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_TEXCOORD, 0},
	D3DDECL_END()
};

const D3DVERTEXELEMENT9 PatchVertexDecl[] = {
	{0, 0,  D3DDECLTYPE_FLOAT3, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_POSITION, 0},
	{0, 12, D3DDECLTYPE_FLOAT3, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_NORMAL, 0},
	{0, 24, D3DDECLTYPE_FLOAT2, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_TEXCOORD, 0},
	{0, 32, D3DDECLTYPE_FLOAT2, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_TEXCOORD, 1},
	D3DDECL_END()
};

const D3DVERTEXELEMENT9 PosTexDecl[] = {
	{0, 0,  D3DDECLTYPE_FLOAT3, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_POSITION, 0},
	{0, 12, D3DDECLTYPE_FLOAT2, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_TEXCOORD, 0},
	D3DDECL_END()
};

const D3DVERTEXELEMENT9 PosColorDecl[] = {
	{0, 0,  D3DDECLTYPE_FLOAT3, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_POSITION, 0},
	{0, 12, D3DDECLTYPE_D3DCOLOR, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_COLOR, 0},
	D3DDECL_END()
};

const D3DVERTEXELEMENT9 PositionDecl[] = {
	{0, 0,  D3DDECLTYPE_FLOAT3, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_POSITION, 0},
	D3DDECL_END()
};

const D3DVERTEXELEMENT9 Vector4Decl[] = {
	{0, 0,  D3DDECLTYPE_FLOAT4, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_POSITION, 0},
	D3DDECL_END()
};

const D3DVERTEXELEMENT9 GPUBlitDecl[] = {
	{0, 0,  D3DDECLTYPE_SHORT4, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_POSITION, 0},
	D3DDECL_END()
};

const D3DVERTEXELEMENT9 HazeVertexDecl[] = {
	{0, 0,  D3DDECLTYPE_FLOAT3, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_POSITION, 0},
	{0, 12, D3DDECLTYPE_D3DCOLOR, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_COLOR, 0},
	{0, 16, D3DDECLTYPE_FLOAT2, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_TEXCOORD, 0},
	D3DDECL_END()
};

typedef struct {
	float x;     ///< vertex x position
	float y;     ///< vertex y position
	float z;     ///< vertex z position
	float tu;    ///< vertex u texture coordinate
	float tv;    ///< vertex v texture coordinate
} SMVERTEX;

typedef struct {
	float x, y, z;
	float nx, ny, nz;
	float tx, ty, tz;
	float u, v;
} NMVERTEX;

typedef struct {
	short tx,ty;
	short sx,sy;
} GPUBLITVTX;


typedef struct {
	float x, y, z;		///< beacon position
	float dx, dy, dz;	///< light direction
	float size, angle, on, off;	///< beacon size and light cone angle
	float bright, falloff;
	DWORD color;		///< beacon color
} BAVERTEX;

typedef struct {
    int			  Type;             ///< Type of light source
    D3DXCOLOR     Diffuse;          ///< diffuse color of light
    D3DXCOLOR     Specular;         ///< specular color of light
    D3DXCOLOR     Ambient;          ///< ambient color of light
    D3DXVECTOR3   Position;         ///< position in world space
    D3DXVECTOR3   Direction;        ///< direction in world space
    D3DXVECTOR3   Attenuation;      ///< Attenuation
	D3DXVECTOR4   Param;            ///< range, falloff, theta, phi
} D3D9Light;

#define D3D9MATEX_DIFFUSE	0x01
#define D3D9MATEX_AMBIENT	0x02
#define D3D9MATEX_SPECULAR	0x04
#define D3D9MATEX_EMISSIVE	0x08
#define D3D9MATEX_REFLECT	0x10
#define D3D9MATEX_DISSOLVE	0x20
#define D3D9MATEX_FRESNEL	0x40

#define D3D9MATSIZE 104
/**
 * \brief Material structure used in D3D9Mesh. Only the upper part (23 floats) is loaded into a shadrs
 */
typedef struct {
	D3DCOLORVALUE Diffuse;        
    D3DCOLORVALUE Ambient;        
    D3DCOLORVALUE Specular;			///< Specular color, power in alpha  
    D3DCOLORVALUE Emissive;       
	D3DCOLORVALUE Reflect;			///< Color multiplier and intensity (alpha)
	D3DCOLORVALUE Fresnel;			///< Fresnel reflection
	float		  DislScale;		///< Dissolve effect scale factor 0.20 to 3.0 (typical)
	float		  DislMag;			///< Dissolve effect magnitude 0.01 to 0.1 (typical)
	// -----------------------
	SURFHANDLE	  pDissolve;		///< Pointer to Dissolve effect texture
	DWORD		  ModFlags;			///< Modification flags
} D3D9MatExt;

typedef struct {
	class D3D9Mesh *pMesh;			///< Mesh handle
	class vObject  *vObj;			///< Visual handle
	float			dist;			///< Distance to a pick point
	int				group;			///< Mesh group that was picked
	int				face;			///< Face that was picked
} D3D9Pick;

#define D3D9LRange 0
#define D3D9LFalloff 1
#define D3D9LTheta 2
#define D3D9LPhi 3

extern IDirect3DVertexDeclaration9	*pMeshVertexDecl;
extern IDirect3DVertexDeclaration9	*pHazeVertexDecl;
extern IDirect3DVertexDeclaration9	*pNTVertexDecl;
extern IDirect3DVertexDeclaration9	*pBAVertexDecl;
extern IDirect3DVertexDeclaration9	*pPosColorDecl;
extern IDirect3DVertexDeclaration9	*pPositionDecl;
extern IDirect3DVertexDeclaration9	*pPosTexDecl;
extern IDirect3DVertexDeclaration9	*pPatchVertexDecl;
extern IDirect3DVertexDeclaration9	*pGPUBlitDecl;

// -----------------------------------------------------------------------------------
// Conversion functions
// ------------------------------------------------------------------------------------

inline void D3DXCOLORSWAP(D3DXCOLOR *x)
{
	float a = x->r; x->r = x->b; x->b = a;
}

inline D3DXVECTOR3 D3DXVECTOR3f4(D3DXVECTOR4 v)
{
	return D3DXVECTOR3(v.x, v.y, v.z);
}

inline D3DCOLORVALUE D3DCOLORMULT(const D3DCOLORVALUE *a, const D3DCOLORVALUE *b)
{
	D3DCOLORVALUE c;
	c.a = a->a * b->a;
	c.r = a->r * b->r;
	c.g = a->g * b->g;
	c.b = a->b * b->b;
	return c;
}

inline void MATRIX4toD3DMATRIX (const MATRIX4 &M, D3DXMATRIX &D)
{
	D._11 = (float)M.m11;  D._12 = (float)M.m12;  D._13 = (float)M.m13;  D._14 = (float)M.m14;
	D._21 = (float)M.m21;  D._22 = (float)M.m22;  D._23 = (float)M.m23;  D._24 = (float)M.m24;
	D._31 = (float)M.m31;  D._32 = (float)M.m32;  D._33 = (float)M.m33;  D._34 = (float)M.m34;
	D._41 = (float)M.m41;  D._42 = (float)M.m42;  D._43 = (float)M.m43;  D._44 = (float)M.m44;
}

inline MATRIX4 _MATRIX4(const LPD3DXMATRIX M)
{
	MATRIX4 D;
	D.m11 = (double)M->_11;  D.m12 = (double)M->_12;  D.m13 = (double)M->_13;  D.m14 = (double)M->_14;
	D.m21 = (double)M->_21;  D.m22 = (double)M->_22;  D.m23 = (double)M->_23;  D.m24 = (double)M->_24;
	D.m31 = (double)M->_31;  D.m32 = (double)M->_32;  D.m33 = (double)M->_33;  D.m34 = (double)M->_34;
	D.m41 = (double)M->_41;  D.m42 = (double)M->_42;  D.m43 = (double)M->_43;  D.m44 = (double)M->_44;
	return D;
}

inline void D3DVEC (const VECTOR3 &v, D3DVECTOR &d3dv)
{
	d3dv.x = (float)v.x;
	d3dv.y = (float)v.y;
	d3dv.z = (float)v.z;
}

inline D3DXVECTOR3 D3DXVEC(const VECTOR3 &v)
{
	return D3DXVECTOR3(float(v.x), float(v.y), float(v.z));
}

inline D3DXVECTOR4 D3DXVEC4(const VECTOR3 &v, float w)
{
	return D3DXVECTOR4(float(v.x), float(v.y), float(v.z), w);
}

inline D3DXCOLOR _D3DXCOLOR(const VECTOR3 &v, float a = 1.0f)
{
	return D3DXCOLOR(float(v.x), float(v.y), float(v.z), a);
}

inline VECTOR3 _VD3DX(const D3DXVECTOR3 &v)
{
	return _V(double(v.x), double(v.y), double(v.z));
}

inline VECTOR4 _VD4DX(const D3DXVECTOR4 &v)
{
	return _V(double(v.x), double(v.y), double(v.z), double(v.w));
}

inline float D3DVAL (double x)
{
	return (float)x;
}

//char* _fgets(char* cbuf, int num, FILE* stream, bool keepOneSpace = false);

int fgets2(char *buf, int cmax, FILE *file, DWORD param=0);

float D3DXVec3Angle(D3DXVECTOR3 a, D3DXVECTOR3 b);
D3DXVECTOR3 Perpendicular(D3DXVECTOR3 *a);

void SurfaceLighting(D3D9Light *light, OBJHANDLE hP, OBJHANDLE hO, float ao);
void OrbitalLighting(D3D9Light *light, OBJHANDLE hP, VECTOR3 GO, float ao);

void CreateMatExt(const D3DMATERIAL9 *pIn, D3D9MatExt *pOut);
void UpdateMatExt(const D3DMATERIAL9 *pIn, D3D9MatExt *pOut);


// ------------------------------------------------------------------------------------
// D3D vector and matrix operations
// ------------------------------------------------------------------------------------

float D3DMAT_BSScaleFactor(const D3DXMATRIX *mat);
void D3DMAT_Identity (D3DXMATRIX *mat);
void D3DMAT_ZeroMatrix(D3DXMATRIX *mat);
void D3DMAT_Copy (D3DXMATRIX *tgt, const D3DXMATRIX *src);
void D3DMAT_SetRotation (D3DXMATRIX *mat, const MATRIX3 *rot);
void D3DMAT_SetInvRotation (D3DXMATRIX *mat, const MATRIX3 *rot);
void D3DMAT_RotationFromAxis (const D3DXVECTOR3 &axis, float angle, D3DXMATRIX *rot);
void D3DMAT_FromAxis(D3DXMATRIX *out, const D3DVECTOR *x, const D3DVECTOR *y, const D3DVECTOR *z);
void D3DMAT_FromAxis(D3DXMATRIX *out, const VECTOR3 *x, const VECTOR3 *y, const VECTOR3 *z);
void D3DMAT_FromAxisT(D3DXMATRIX *out, const D3DVECTOR *x, const D3DVECTOR *y, const D3DVECTOR *z);
void D3DMAT_CreateX_Billboard(const D3DXVECTOR3 *toCam, const D3DXVECTOR3 *pos, float scale, D3DXMATRIX *pOut);
void D3DMAT_CreateX_Billboard(const D3DXVECTOR3 *toCam, const D3DXVECTOR3 *pos, const D3DXVECTOR3 *dir, float size, float stretch, D3DXMATRIX *pOut);

// Set up a as matrix for ANTICLOCKWISE rotation r around x/y/z-axis
void D3DMAT_RotX (D3DXMATRIX *mat, double r);
void D3DMAT_RotY (D3DXMATRIX *mat, double r);

void D3DMAT_SetTranslation (D3DXMATRIX *mat, const VECTOR3 *trans);
bool D3DMAT_VectorMatrixMultiply (D3DXVECTOR3 *res, const D3DXVECTOR3 *v, const D3DXMATRIX *mat);
HRESULT D3DMAT_MatrixInvert (D3DXMATRIX *res, D3DXMATRIX *a);

// ------------------------------------------------------------------------------------
// Vertex formats
// ------------------------------------------------------------------------------------

struct VECTOR2D     { float x, y; };

struct VERTEX_XYZ   { float x, y, z; };                   // transformed vertex
struct VERTEX_XYZH  { float x, y, z, h; };                // untransformed vertex
struct VERTEX_XYZC  { float x, y, z; D3DCOLOR col; };     // untransformed vertex with single colour component
struct VERTEX_XYZHC { float x, y, z, h; D3DCOLOR col; };  // transformed vertex with single colour component



// untransformed lit vertex with texture coordinates
struct VERTEX_XYZ_TEX {
	float x, y, z;
	float tu, tv;
};
#define FVF_XYZ_TEX ( D3DFVF_XYZ | D3DFVF_TEX1 | D3DFVF_TEXCOORDSIZE2(0) )

// untransformed unlit vertex with two sets of texture coordinates
struct VERTEX_2TEX  {
	float x, y, z, nx, ny, nz;
	float tu0, tv0, tu1, tv1;
	inline VERTEX_2TEX() {}
	//VERTEX_2TEX ()
	//{ x = y = z = nx = ny = nz = tu0 = tv0 = tu1 = tv1 = 0.0f; }
	inline VERTEX_2TEX (D3DVECTOR p, D3DVECTOR n, float u0, float v0, float u1, float v1)
	{ x = p.x, y = p.y, z = p.z, nx = n.x, ny = n.y, nz = n.z;
  	  tu0 = u0, tv0 = v0, tu1 = u1, tv1 = v1; }
};
#define FVF_2TEX ( D3DFVF_XYZ | D3DFVF_NORMAL | D3DFVF_TEX2 | D3DFVF_TEXCOORDSIZE2(0) | D3DFVF_TEXCOORDSIZE2(1) )

// transformed lit vertex with 1 colour definition and one set of texture coordinates
struct VERTEX_TL1TEX {
	float x, y, z, rhw;
	D3DCOLOR col;
	float tu, tv;
};
#define FVF_TL1TEX ( D3DFVF_XYZRHW | D3DFVF_DIFFUSE | D3DFVF_TEX1 | D3DFVF_TEXCOORDSIZE2(0) )

// transformed lit vertex with two sets of texture coordinates
struct VERTEX_TL2TEX {
	float x, y, z, rhw;
	D3DCOLOR diff, spec;
	float tu0, tv0, tu1, tv1;
};
#define FVF_TL2TEX ( D3DFVF_XYZRHW | D3DFVF_DIFFUSE | D3DFVF_SPECULAR | D3DFVF_TEX2 | D3DFVF_TEXCOORDSIZE2(0) | D3DFVF_TEXCOORDSIZE2(1) )

VERTEX_XYZ  *GetVertexXYZ  (DWORD n);
VERTEX_XYZC *GetVertexXYZC (DWORD n);
// Return pointer to static vertex buffer of given type of at least size n

// -----------------------------------------------------------------------------------
// Resource handling
// ------------------------------------------------------------------------------------
/**
 * \brief Kind of auto_handle.
 * This simple wrapper acts like 'auto_ptr' but is for HANDLE. It is used to
 * avoid any not-closed HANDLES leaks when the block scope is left (via thrown
 * exception or return e.g.)
 */
struct AutoHandle
{
	HANDLE Handle;

	AutoHandle () {
		Handle = NULL;
	}

	~AutoHandle () {
		ForceClose();
	}

	bool IsInvalid () {
		return Handle == INVALID_HANDLE_VALUE || Handle == NULL;
	}

	void ForceClose()
	{
		if (!IsInvalid()) {
			CloseHandle(Handle);
		}
		Handle = NULL;
	}
};


/**
 * \brief Kind of auto_file.
 * This simple wrapper acts like 'auto_ptr' but is for FILE pointer. It is used
 * to avoid any not-closed FILES leaks when the block scope is left (via thrown
 * exception or return e.g.)
 */
struct AutoFile
{
	FILE* pFile;

	AutoFile () {
		pFile = NULL;
	}

	~AutoFile () {
		ForceClose();
	}

	bool IsInvalid () {
		return pFile == NULL;
	}

	void ForceClose()
	{
		if (!IsInvalid()) {
			fclose(pFile);
		}
		pFile = NULL;
	}
};


// ------------------------------------------------------------------------------------
// Miscellaneous helper functions
// ------------------------------------------------------------------------------------

#define SAFE_DELETE(p)  { if(p) { delete (p);     (p)=NULL; } }
#define SAFE_DELETEA(p)  { if(p) { delete []p;     (p)=NULL; } }
//#define SAFE_RELEASE(p) { if(p) { ULONG n=(p)->Release(); if (n>0) LogWrn("NOT RELEASED Refs=%u, File=%s Line=%u",n,__FILE__,__LINE__); (p)=NULL; } }
#define SAFE_RELEASE(p) { if(p) { (p)->Release(); (p)=NULL; } }

#endif // !__D3DUTIL_H