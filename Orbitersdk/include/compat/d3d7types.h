#pragma once

#include <windows.h>

typedef float D3DVALUE, *LPD3DVALUE;
typedef DWORD D3DCOLOR, *LPD3DCOLOR;

#define D3DVAL(val) ((float)(val))
#define D3DRGBA(r, g, b, a) \
	((((DWORD)((a) * 255)) << 24) | (((DWORD)((r) * 255)) << 16) | (((DWORD)((g) * 255)) << 8) | (DWORD)((b) * 255))
#define D3DRGB(r, g, b) (0xff000000L | (((long)((r) * 255)) << 16) | (((long)((g) * 255)) << 8) | (long)((b) * 255))
#define RGBA_MAKE(r, g, b, a) ((D3DCOLOR)(((a) << 24) | ((r) << 16) | ((g) << 8) | (b)))
#define RGB_MAKE(r, g, b) ((D3DCOLOR)(((r) << 16) | ((g) << 8) | (b)))

typedef struct _D3DVECTOR {
	union { D3DVALUE x; D3DVALUE dvX; };
	union { D3DVALUE y; D3DVALUE dvY; };
	union { D3DVALUE z; D3DVALUE dvZ; };
#ifdef __cplusplus
	_D3DVECTOR() {}
	_D3DVECTOR(D3DVALUE f) : x(f), y(f), z(f) {}
	_D3DVECTOR(D3DVALUE _x, D3DVALUE _y, D3DVALUE _z) : x(_x), y(_y), z(_z) {}
	D3DVALUE &operator[](int i) { return (&x)[i]; }
	const D3DVALUE &operator[](int i) const { return (&x)[i]; }
	_D3DVECTOR &operator+=(const _D3DVECTOR &v) { x += v.x; y += v.y; z += v.z; return *this; }
	_D3DVECTOR &operator-=(const _D3DVECTOR &v) { x -= v.x; y -= v.y; z -= v.z; return *this; }
	_D3DVECTOR &operator*=(D3DVALUE s) { x *= s; y *= s; z *= s; return *this; }
	_D3DVECTOR &operator/=(D3DVALUE s) { x /= s; y /= s; z /= s; return *this; }
	friend _D3DVECTOR operator+(const _D3DVECTOR &a, const _D3DVECTOR &b) { return _D3DVECTOR(a.x + b.x, a.y + b.y, a.z + b.z); }
	friend _D3DVECTOR operator-(const _D3DVECTOR &a, const _D3DVECTOR &b) { return _D3DVECTOR(a.x - b.x, a.y - b.y, a.z - b.z); }
	friend _D3DVECTOR operator-(const _D3DVECTOR &a) { return _D3DVECTOR(-a.x, -a.y, -a.z); }
	friend _D3DVECTOR operator*(const _D3DVECTOR &a, D3DVALUE s) { return _D3DVECTOR(a.x * s, a.y * s, a.z * s); }
	friend _D3DVECTOR operator*(D3DVALUE s, const _D3DVECTOR &a) { return _D3DVECTOR(a.x * s, a.y * s, a.z * s); }
	friend _D3DVECTOR operator/(const _D3DVECTOR &a, D3DVALUE s) { return _D3DVECTOR(a.x / s, a.y / s, a.z / s); }
	friend _D3DVECTOR operator*(const _D3DVECTOR &a, const _D3DVECTOR &b) { return _D3DVECTOR(a.x * b.x, a.y * b.y, a.z * b.z); }
	friend D3DVALUE SquareMagnitude(const _D3DVECTOR &v) { return v.x * v.x + v.y * v.y + v.z * v.z; }
	friend D3DVALUE Magnitude(const _D3DVECTOR &v) { return sqrtf(SquareMagnitude(v)); }
	friend _D3DVECTOR Normalize(const _D3DVECTOR &v) { return v / Magnitude(v); }
	friend D3DVALUE DotProduct(const _D3DVECTOR &a, const _D3DVECTOR &b) { return a.x * b.x + a.y * b.y + a.z * b.z; }
	friend _D3DVECTOR CrossProduct(const _D3DVECTOR &a, const _D3DVECTOR &b)
	{ return _D3DVECTOR(a.y * b.z - a.z * b.y, a.z * b.x - a.x * b.z, a.x * b.y - a.y * b.x); }
#endif
} D3DVECTOR, *LPD3DVECTOR;

typedef struct _D3DMATRIX {
	union {
		struct {
			D3DVALUE _11, _12, _13, _14;
			D3DVALUE _21, _22, _23, _24;
			D3DVALUE _31, _32, _33, _34;
			D3DVALUE _41, _42, _43, _44;
		};
		D3DVALUE m[4][4];
	};
#ifdef __cplusplus
	_D3DMATRIX() {}
	_D3DMATRIX(D3DVALUE a11, D3DVALUE a12, D3DVALUE a13, D3DVALUE a14,
		D3DVALUE a21, D3DVALUE a22, D3DVALUE a23, D3DVALUE a24,
		D3DVALUE a31, D3DVALUE a32, D3DVALUE a33, D3DVALUE a34,
		D3DVALUE a41, D3DVALUE a42, D3DVALUE a43, D3DVALUE a44)
		: _11(a11), _12(a12), _13(a13), _14(a14), _21(a21), _22(a22), _23(a23), _24(a24),
		  _31(a31), _32(a32), _33(a33), _34(a34), _41(a41), _42(a42), _43(a43), _44(a44) {}
	D3DVALUE &operator()(int i, int j) { return m[i][j]; }
	const D3DVALUE &operator()(int i, int j) const { return m[i][j]; }
#endif
} D3DMATRIX, *LPD3DMATRIX;

typedef struct _D3DCOLORVALUE {
	union { D3DVALUE r; D3DVALUE dvR; };
	union { D3DVALUE g; D3DVALUE dvG; };
	union { D3DVALUE b; D3DVALUE dvB; };
	union { D3DVALUE a; D3DVALUE dvA; };
} D3DCOLORVALUE, *LPD3DCOLORVALUE;

typedef struct _D3DMATERIAL7 {
	union { D3DCOLORVALUE diffuse; D3DCOLORVALUE dcvDiffuse; };
	union { D3DCOLORVALUE ambient; D3DCOLORVALUE dcvAmbient; };
	union { D3DCOLORVALUE specular; D3DCOLORVALUE dcvSpecular; };
	union { D3DCOLORVALUE emissive; D3DCOLORVALUE dcvEmissive; };
	union { D3DVALUE power; D3DVALUE dvPower; };
} D3DMATERIAL7, *LPD3DMATERIAL7;

typedef struct _D3DVERTEX {
	union { D3DVALUE x; D3DVALUE dvX; };
	union { D3DVALUE y; D3DVALUE dvY; };
	union { D3DVALUE z; D3DVALUE dvZ; };
	union { D3DVALUE nx; D3DVALUE dvNX; };
	union { D3DVALUE ny; D3DVALUE dvNY; };
	union { D3DVALUE nz; D3DVALUE dvNZ; };
	union { D3DVALUE tu; D3DVALUE dvTU; };
	union { D3DVALUE tv; D3DVALUE dvTV; };
#ifdef __cplusplus
	_D3DVERTEX() {}
	_D3DVERTEX(const D3DVECTOR &v, const D3DVECTOR &n, D3DVALUE _tu, D3DVALUE _tv)
		: x(v.x), y(v.y), z(v.z), nx(n.x), ny(n.y), nz(n.z), tu(_tu), tv(_tv) {}
#endif
} D3DVERTEX, *LPD3DVERTEX;

#define D3DFVF_RESERVED0 0x001
#define D3DFVF_POSITION_MASK 0x00E
#define D3DFVF_XYZ 0x002
#define D3DFVF_XYZRHW 0x004
#define D3DFVF_NORMAL 0x010
#define D3DFVF_DIFFUSE 0x040
#define D3DFVF_SPECULAR 0x080
#define D3DFVF_TEXCOUNT_MASK 0xf00
#define D3DFVF_TEXCOUNT_SHIFT 8
#define D3DFVF_TEX0 0x000
#define D3DFVF_TEX1 0x100
#define D3DFVF_TEX2 0x200
#define D3DFVF_TEX3 0x300
#define D3DFVF_TEX4 0x400
#define D3DFVF_TEXTUREFORMAT2 0
#define D3DFVF_TEXCOORDSIZE2(i) (D3DFVF_TEXTUREFORMAT2 << ((i) * 2 + 16))
#define D3DFVF_VERTEX (D3DFVF_XYZ | D3DFVF_NORMAL | D3DFVF_TEX1)

#ifndef D3DPT_DEFINED
#define D3DPT_DEFINED
typedef enum _D3DPRIMITIVETYPE {
	D3DPT_POINTLIST = 1,
	D3DPT_LINELIST = 2,
	D3DPT_LINESTRIP = 3,
	D3DPT_TRIANGLELIST = 4,
	D3DPT_TRIANGLESTRIP = 5,
	D3DPT_TRIANGLEFAN = 6,
	D3DPT_FORCE_DWORD = 0x7fffffff
} D3DPRIMITIVETYPE;
#endif
