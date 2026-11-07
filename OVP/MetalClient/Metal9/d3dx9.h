#pragma once

#include "d3d9.h"
#include <math.h>
#include <float.h>
#include <string.h>

#define D3DX_PI 3.141592654f
#define D3DX_DEFAULT ((UINT)-1)
#define D3DX_DEFAULT_NONPOW2 ((UINT)-2)
#define D3DX_DEFAULT_FLOAT FLT_MAX
#define D3DX_FROM_FILE ((UINT)-3)
#define D3DFMT_FROM_FILE ((D3DFORMAT)-3)
#define D3DX_SDK_VERSION 43
#define D3DXToRadian(degree) ((degree) * (D3DX_PI / 180.0f))
#define D3DXToDegree(radian) ((radian) * (180.0f / D3DX_PI))

#define D3DX_FILTER_NONE (1 << 0)
#define D3DX_FILTER_POINT (2 << 0)
#define D3DX_FILTER_LINEAR (3 << 0)
#define D3DX_FILTER_TRIANGLE (4 << 0)
#define D3DX_FILTER_BOX (5 << 0)
#define D3DX_FILTER_MIRROR (7 << 16)
#define D3DX_FILTER_DITHER (1 << 19)
#define D3DX_FILTER_SRGB (3 << 21)

#define D3DXFX_DONOTSAVESTATE (1 << 0)
#define D3DXFX_DONOTSAVESHADERSTATE (1 << 1)
#define D3DXFX_DONOTSAVESAMPLERSTATE (1 << 2)
#define D3DXFX_NOT_CLONEABLE (1 << 11)
#define D3DXFX_LARGEADDRESSAWARE (1 << 17)

#define D3DXSHADER_DEBUG (1 << 0)
#define D3DXSHADER_SKIPVALIDATION (1 << 1)
#define D3DXSHADER_SKIPOPTIMIZATION (1 << 2)
#define D3DXSHADER_PACKMATRIX_ROWMAJOR (1 << 3)
#define D3DXSHADER_PACKMATRIX_COLUMNMAJOR (1 << 4)
#define D3DXSHADER_PARTIALPRECISION (1 << 5)
#define D3DXSHADER_NO_PRESHADER (1 << 8)
#define D3DXSHADER_AVOID_FLOW_CONTROL (1 << 9)
#define D3DXSHADER_PREFER_FLOW_CONTROL (1 << 10)
#define D3DXSHADER_OPTIMIZATION_LEVEL3 (1 << 15)

typedef const char *D3DXHANDLE;
typedef D3DXHANDLE *LPD3DXHANDLE;

typedef struct _D3DXMACRO {
	const char *Name;
	const char *Definition;
} D3DXMACRO, *LPD3DXMACRO;

typedef enum _D3DXIMAGE_FILEFORMAT {
	D3DXIFF_BMP = 0,
	D3DXIFF_JPG = 1,
	D3DXIFF_TGA = 2,
	D3DXIFF_PNG = 3,
	D3DXIFF_DDS = 4,
	D3DXIFF_PPM = 5,
	D3DXIFF_DIB = 6,
	D3DXIFF_HDR = 7,
	D3DXIFF_PFM = 8,
	D3DXIFF_FORCE_DWORD = 0x7fffffff
} D3DXIMAGE_FILEFORMAT;

typedef struct _D3DXIMAGE_INFO {
	UINT Width;
	UINT Height;
	UINT Depth;
	UINT MipLevels;
	D3DFORMAT Format;
	D3DRESOURCETYPE ResourceType;
	D3DXIMAGE_FILEFORMAT ImageFileFormat;
} D3DXIMAGE_INFO;

// ----------------------------------------------------------------------------
// Math types

struct D3DXVECTOR2 {
	float x, y;
	D3DXVECTOR2() {}
	D3DXVECTOR2(const float *f) : x(f[0]), y(f[1]) {}
	D3DXVECTOR2(float fx, float fy) : x(fx), y(fy) {}
	operator float *() { return &x; }
	operator const float *() const { return &x; }
	D3DXVECTOR2 &operator+=(const D3DXVECTOR2 &v) { x += v.x; y += v.y; return *this; }
	D3DXVECTOR2 &operator-=(const D3DXVECTOR2 &v) { x -= v.x; y -= v.y; return *this; }
	D3DXVECTOR2 &operator*=(float f) { x *= f; y *= f; return *this; }
	D3DXVECTOR2 &operator/=(float f) { float i = 1.0f / f; x *= i; y *= i; return *this; }
	D3DXVECTOR2 operator+() const { return *this; }
	D3DXVECTOR2 operator-() const { return D3DXVECTOR2(-x, -y); }
	D3DXVECTOR2 operator+(const D3DXVECTOR2 &v) const { return D3DXVECTOR2(x + v.x, y + v.y); }
	D3DXVECTOR2 operator-(const D3DXVECTOR2 &v) const { return D3DXVECTOR2(x - v.x, y - v.y); }
	D3DXVECTOR2 operator*(float f) const { return D3DXVECTOR2(x * f, y * f); }
	D3DXVECTOR2 operator/(float f) const { float i = 1.0f / f; return D3DXVECTOR2(x * i, y * i); }
	friend D3DXVECTOR2 operator*(float f, const D3DXVECTOR2 &v) { return D3DXVECTOR2(f * v.x, f * v.y); }
	bool operator==(const D3DXVECTOR2 &v) const { return x == v.x && y == v.y; }
	bool operator!=(const D3DXVECTOR2 &v) const { return !(*this == v); }
};

struct D3DXVECTOR3 : public D3DVECTOR {
	D3DXVECTOR3() {}
	D3DXVECTOR3(const float *f) { x = f[0]; y = f[1]; z = f[2]; }
	D3DXVECTOR3(const D3DVECTOR &v) { x = v.x; y = v.y; z = v.z; }
	D3DXVECTOR3(float fx, float fy, float fz) { x = fx; y = fy; z = fz; }
	operator float *() { return &x; }
	operator const float *() const { return &x; }
	D3DXVECTOR3 &operator+=(const D3DXVECTOR3 &v) { x += v.x; y += v.y; z += v.z; return *this; }
	D3DXVECTOR3 &operator-=(const D3DXVECTOR3 &v) { x -= v.x; y -= v.y; z -= v.z; return *this; }
	D3DXVECTOR3 &operator*=(float f) { x *= f; y *= f; z *= f; return *this; }
	D3DXVECTOR3 &operator/=(float f) { float i = 1.0f / f; x *= i; y *= i; z *= i; return *this; }
	D3DXVECTOR3 operator+() const { return *this; }
	D3DXVECTOR3 operator-() const { return D3DXVECTOR3(-x, -y, -z); }
	D3DXVECTOR3 operator+(const D3DXVECTOR3 &v) const { return D3DXVECTOR3(x + v.x, y + v.y, z + v.z); }
	D3DXVECTOR3 operator-(const D3DXVECTOR3 &v) const { return D3DXVECTOR3(x - v.x, y - v.y, z - v.z); }
	D3DXVECTOR3 operator*(float f) const { return D3DXVECTOR3(x * f, y * f, z * f); }
	D3DXVECTOR3 operator/(float f) const { float i = 1.0f / f; return D3DXVECTOR3(x * i, y * i, z * i); }
	friend D3DXVECTOR3 operator*(float f, const D3DXVECTOR3 &v) { return D3DXVECTOR3(f * v.x, f * v.y, f * v.z); }
	bool operator==(const D3DXVECTOR3 &v) const { return x == v.x && y == v.y && z == v.z; }
	bool operator!=(const D3DXVECTOR3 &v) const { return !(*this == v); }
};

struct D3DXVECTOR4 {
	float x, y, z, w;
	D3DXVECTOR4() {}
	D3DXVECTOR4(const float *f) : x(f[0]), y(f[1]), z(f[2]), w(f[3]) {}
	D3DXVECTOR4(const D3DVECTOR &v, float fw) : x(v.x), y(v.y), z(v.z), w(fw) {}
	D3DXVECTOR4(float fx, float fy, float fz, float fw) : x(fx), y(fy), z(fz), w(fw) {}
	operator float *() { return &x; }
	operator const float *() const { return &x; }
	D3DXVECTOR4 &operator+=(const D3DXVECTOR4 &v) { x += v.x; y += v.y; z += v.z; w += v.w; return *this; }
	D3DXVECTOR4 &operator-=(const D3DXVECTOR4 &v) { x -= v.x; y -= v.y; z -= v.z; w -= v.w; return *this; }
	D3DXVECTOR4 &operator*=(float f) { x *= f; y *= f; z *= f; w *= f; return *this; }
	D3DXVECTOR4 &operator/=(float f) { float i = 1.0f / f; x *= i; y *= i; z *= i; w *= i; return *this; }
	D3DXVECTOR4 operator+() const { return *this; }
	D3DXVECTOR4 operator-() const { return D3DXVECTOR4(-x, -y, -z, -w); }
	D3DXVECTOR4 operator+(const D3DXVECTOR4 &v) const { return D3DXVECTOR4(x + v.x, y + v.y, z + v.z, w + v.w); }
	D3DXVECTOR4 operator-(const D3DXVECTOR4 &v) const { return D3DXVECTOR4(x - v.x, y - v.y, z - v.z, w - v.w); }
	D3DXVECTOR4 operator*(float f) const { return D3DXVECTOR4(x * f, y * f, z * f, w * f); }
	D3DXVECTOR4 operator/(float f) const { float i = 1.0f / f; return D3DXVECTOR4(x * i, y * i, z * i, w * i); }
	friend D3DXVECTOR4 operator*(float f, const D3DXVECTOR4 &v) { return D3DXVECTOR4(f * v.x, f * v.y, f * v.z, f * v.w); }
	bool operator==(const D3DXVECTOR4 &v) const { return x == v.x && y == v.y && z == v.z && w == v.w; }
	bool operator!=(const D3DXVECTOR4 &v) const { return !(*this == v); }
};

struct D3DXMATRIX : public D3DMATRIX {
	D3DXMATRIX() {}
	D3DXMATRIX(const float *f) { memcpy(&_11, f, sizeof(D3DMATRIX)); }
	D3DXMATRIX(const D3DMATRIX &m) { memcpy(&_11, &m, sizeof(D3DMATRIX)); }
	D3DXMATRIX(float f11, float f12, float f13, float f14, float f21, float f22, float f23, float f24,
		float f31, float f32, float f33, float f34, float f41, float f42, float f43, float f44)
	{
		_11 = f11; _12 = f12; _13 = f13; _14 = f14; _21 = f21; _22 = f22; _23 = f23; _24 = f24;
		_31 = f31; _32 = f32; _33 = f33; _34 = f34; _41 = f41; _42 = f42; _43 = f43; _44 = f44;
	}
	float &operator()(UINT r, UINT c) { return m[r][c]; }
	float operator()(UINT r, UINT c) const { return m[r][c]; }
	operator float *() { return &_11; }
	operator const float *() const { return &_11; }
	D3DXMATRIX &operator*=(const D3DXMATRIX &b);
	D3DXMATRIX &operator+=(const D3DXMATRIX &b) { for (int i = 0; i < 16; i++) (&_11)[i] += (&b._11)[i]; return *this; }
	D3DXMATRIX &operator-=(const D3DXMATRIX &b) { for (int i = 0; i < 16; i++) (&_11)[i] -= (&b._11)[i]; return *this; }
	D3DXMATRIX &operator*=(float f) { for (int i = 0; i < 16; i++) (&_11)[i] *= f; return *this; }
	D3DXMATRIX &operator/=(float f) { float inv = 1.0f / f; for (int i = 0; i < 16; i++) (&_11)[i] *= inv; return *this; }
	D3DXMATRIX operator+() const { return *this; }
	D3DXMATRIX operator-() const { D3DXMATRIX r; for (int i = 0; i < 16; i++) (&r._11)[i] = -(&_11)[i]; return r; }
	D3DXMATRIX operator*(const D3DXMATRIX &b) const;
	D3DXMATRIX operator+(const D3DXMATRIX &b) const { D3DXMATRIX r(*this); r += b; return r; }
	D3DXMATRIX operator-(const D3DXMATRIX &b) const { D3DXMATRIX r(*this); r -= b; return r; }
	D3DXMATRIX operator*(float f) const { D3DXMATRIX r(*this); r *= f; return r; }
	D3DXMATRIX operator/(float f) const { D3DXMATRIX r(*this); r /= f; return r; }
	friend D3DXMATRIX operator*(float f, const D3DXMATRIX &m) { return m * f; }
	bool operator==(const D3DXMATRIX &b) const { return memcmp(this, &b, sizeof(D3DMATRIX)) == 0; }
	bool operator!=(const D3DXMATRIX &b) const { return !(*this == b); }
};

struct D3DXCOLOR {
	float r, g, b, a;
	D3DXCOLOR() {}
	D3DXCOLOR(DWORD argb)
	{
		const float f = 1.0f / 255.0f;
		r = f * (float)(BYTE)(argb >> 16);
		g = f * (float)(BYTE)(argb >> 8);
		b = f * (float)(BYTE)(argb >> 0);
		a = f * (float)(BYTE)(argb >> 24);
	}
	D3DXCOLOR(const float *f) : r(f[0]), g(f[1]), b(f[2]), a(f[3]) {}
	D3DXCOLOR(const D3DCOLORVALUE &c) : r(c.r), g(c.g), b(c.b), a(c.a) {}
	D3DXCOLOR(float fr, float fg, float fb, float fa) : r(fr), g(fg), b(fb), a(fa) {}
	operator DWORD() const
	{
		DWORD dr = r >= 1.0f ? 0xff : r <= 0.0f ? 0x00 : (DWORD)(r * 255.0f + 0.5f);
		DWORD dg = g >= 1.0f ? 0xff : g <= 0.0f ? 0x00 : (DWORD)(g * 255.0f + 0.5f);
		DWORD db = b >= 1.0f ? 0xff : b <= 0.0f ? 0x00 : (DWORD)(b * 255.0f + 0.5f);
		DWORD da = a >= 1.0f ? 0xff : a <= 0.0f ? 0x00 : (DWORD)(a * 255.0f + 0.5f);
		return (da << 24) | (dr << 16) | (dg << 8) | db;
	}
	operator float *() { return &r; }
	operator const float *() const { return &r; }
	operator D3DCOLORVALUE *() { return (D3DCOLORVALUE *)&r; }
	operator const D3DCOLORVALUE *() const { return (const D3DCOLORVALUE *)&r; }
	operator D3DCOLORVALUE &() { return *(D3DCOLORVALUE *)&r; }
	operator const D3DCOLORVALUE &() const { return *(const D3DCOLORVALUE *)&r; }
	D3DXCOLOR &operator+=(const D3DXCOLOR &c) { r += c.r; g += c.g; b += c.b; a += c.a; return *this; }
	D3DXCOLOR &operator-=(const D3DXCOLOR &c) { r -= c.r; g -= c.g; b -= c.b; a -= c.a; return *this; }
	D3DXCOLOR &operator*=(float f) { r *= f; g *= f; b *= f; a *= f; return *this; }
	D3DXCOLOR &operator/=(float f) { float i = 1.0f / f; r *= i; g *= i; b *= i; a *= i; return *this; }
	D3DXCOLOR operator+() const { return *this; }
	D3DXCOLOR operator-() const { return D3DXCOLOR(-r, -g, -b, -a); }
	D3DXCOLOR operator+(const D3DXCOLOR &c) const { return D3DXCOLOR(r + c.r, g + c.g, b + c.b, a + c.a); }
	D3DXCOLOR operator-(const D3DXCOLOR &c) const { return D3DXCOLOR(r - c.r, g - c.g, b - c.b, a - c.a); }
	D3DXCOLOR operator*(float f) const { return D3DXCOLOR(r * f, g * f, b * f, a * f); }
	D3DXCOLOR operator/(float f) const { float i = 1.0f / f; return D3DXCOLOR(r * i, g * i, b * i, a * i); }
	friend D3DXCOLOR operator*(float f, const D3DXCOLOR &c) { return c * f; }
	bool operator==(const D3DXCOLOR &c) const { return r == c.r && g == c.g && b == c.b && a == c.a; }
	bool operator!=(const D3DXCOLOR &c) const { return !(*this == c); }
};

struct D3DXQUATERNION {
	float x, y, z, w;
	D3DXQUATERNION() {}
	D3DXQUATERNION(float fx, float fy, float fz, float fw) : x(fx), y(fy), z(fz), w(fw) {}
};

struct D3DXPLANE {
	float a, b, c, d;
	D3DXPLANE() {}
	D3DXPLANE(float fa, float fb, float fc, float fd) : a(fa), b(fb), c(fc), d(fd) {}
};

typedef D3DXVECTOR2 *LPD3DXVECTOR2;
typedef D3DXVECTOR3 *LPD3DXVECTOR3;
typedef D3DXVECTOR4 *LPD3DXVECTOR4;
typedef D3DXMATRIX *LPD3DXMATRIX;
typedef D3DXCOLOR *LPD3DXCOLOR;
typedef D3DXQUATERNION *LPD3DXQUATERNION;
typedef D3DXPLANE *LPD3DXPLANE;

// ----------------------------------------------------------------------------
// Math functions

inline float D3DXVec2Length(const D3DXVECTOR2 *v) { return sqrtf(v->x * v->x + v->y * v->y); }
inline float D3DXVec2LengthSq(const D3DXVECTOR2 *v) { return v->x * v->x + v->y * v->y; }
inline float D3DXVec2Dot(const D3DXVECTOR2 *a, const D3DXVECTOR2 *b) { return a->x * b->x + a->y * b->y; }
inline D3DXVECTOR2 *D3DXVec2Normalize(D3DXVECTOR2 *out, const D3DXVECTOR2 *v)
{
	float l = D3DXVec2Length(v);
	if (l == 0.0f) { out->x = out->y = 0.0f; return out; }
	out->x = v->x / l; out->y = v->y / l;
	return out;
}

inline float D3DXVec3Length(const D3DXVECTOR3 *v) { return sqrtf(v->x * v->x + v->y * v->y + v->z * v->z); }
inline float D3DXVec3LengthSq(const D3DXVECTOR3 *v) { return v->x * v->x + v->y * v->y + v->z * v->z; }
inline float D3DXVec3Dot(const D3DXVECTOR3 *a, const D3DXVECTOR3 *b) { return a->x * b->x + a->y * b->y + a->z * b->z; }
inline D3DXVECTOR3 *D3DXVec3Cross(D3DXVECTOR3 *out, const D3DXVECTOR3 *a, const D3DXVECTOR3 *b)
{
	D3DXVECTOR3 r(a->y * b->z - a->z * b->y, a->z * b->x - a->x * b->z, a->x * b->y - a->y * b->x);
	*out = r;
	return out;
}
inline D3DXVECTOR3 *D3DXVec3Add(D3DXVECTOR3 *out, const D3DXVECTOR3 *a, const D3DXVECTOR3 *b) { *out = *a + *b; return out; }
inline D3DXVECTOR3 *D3DXVec3Subtract(D3DXVECTOR3 *out, const D3DXVECTOR3 *a, const D3DXVECTOR3 *b) { *out = *a - *b; return out; }
inline D3DXVECTOR3 *D3DXVec3Scale(D3DXVECTOR3 *out, const D3DXVECTOR3 *v, float s) { *out = *v * s; return out; }
inline D3DXVECTOR3 *D3DXVec3Lerp(D3DXVECTOR3 *out, const D3DXVECTOR3 *a, const D3DXVECTOR3 *b, float s) { *out = *a + (*b - *a) * s; return out; }
inline D3DXVECTOR3 *D3DXVec3Normalize(D3DXVECTOR3 *out, const D3DXVECTOR3 *v)
{
	float l = D3DXVec3Length(v);
	if (l == 0.0f) { out->x = out->y = out->z = 0.0f; return out; }
	float i = 1.0f / l;
	out->x = v->x * i; out->y = v->y * i; out->z = v->z * i;
	return out;
}
inline D3DXVECTOR3 *D3DXVec3Minimize(D3DXVECTOR3 *out, const D3DXVECTOR3 *a, const D3DXVECTOR3 *b)
{
	out->x = fminf(a->x, b->x); out->y = fminf(a->y, b->y); out->z = fminf(a->z, b->z);
	return out;
}
inline D3DXVECTOR3 *D3DXVec3Maximize(D3DXVECTOR3 *out, const D3DXVECTOR3 *a, const D3DXVECTOR3 *b)
{
	out->x = fmaxf(a->x, b->x); out->y = fmaxf(a->y, b->y); out->z = fmaxf(a->z, b->z);
	return out;
}

inline float D3DXVec4Length(const D3DXVECTOR4 *v) { return sqrtf(v->x * v->x + v->y * v->y + v->z * v->z + v->w * v->w); }
inline float D3DXVec4Dot(const D3DXVECTOR4 *a, const D3DXVECTOR4 *b) { return a->x * b->x + a->y * b->y + a->z * b->z + a->w * b->w; }
inline D3DXVECTOR4 *D3DXVec4Normalize(D3DXVECTOR4 *out, const D3DXVECTOR4 *v)
{
	float l = D3DXVec4Length(v);
	if (l == 0.0f) { *out = D3DXVECTOR4(0, 0, 0, 0); return out; }
	*out = *v / l;
	return out;
}

inline D3DXVECTOR4 *D3DXVec3Transform(D3DXVECTOR4 *out, const D3DXVECTOR3 *v, const D3DXMATRIX *m)
{
	D3DXVECTOR4 r(
		v->x * m->_11 + v->y * m->_21 + v->z * m->_31 + m->_41,
		v->x * m->_12 + v->y * m->_22 + v->z * m->_32 + m->_42,
		v->x * m->_13 + v->y * m->_23 + v->z * m->_33 + m->_43,
		v->x * m->_14 + v->y * m->_24 + v->z * m->_34 + m->_44);
	*out = r;
	return out;
}

inline D3DXVECTOR3 *D3DXVec3TransformCoord(D3DXVECTOR3 *out, const D3DXVECTOR3 *v, const D3DXMATRIX *m)
{
	D3DXVECTOR4 r;
	D3DXVec3Transform(&r, v, m);
	float iw = r.w != 0.0f ? 1.0f / r.w : 1.0f;
	out->x = r.x * iw; out->y = r.y * iw; out->z = r.z * iw;
	return out;
}

inline D3DXVECTOR3 *D3DXVec3TransformNormal(D3DXVECTOR3 *out, const D3DXVECTOR3 *v, const D3DXMATRIX *m)
{
	D3DXVECTOR3 r(
		v->x * m->_11 + v->y * m->_21 + v->z * m->_31,
		v->x * m->_12 + v->y * m->_22 + v->z * m->_32,
		v->x * m->_13 + v->y * m->_23 + v->z * m->_33);
	*out = r;
	return out;
}

inline D3DXVECTOR4 *D3DXVec4Transform(D3DXVECTOR4 *out, const D3DXVECTOR4 *v, const D3DXMATRIX *m)
{
	D3DXVECTOR4 r(
		v->x * m->_11 + v->y * m->_21 + v->z * m->_31 + v->w * m->_41,
		v->x * m->_12 + v->y * m->_22 + v->z * m->_32 + v->w * m->_42,
		v->x * m->_13 + v->y * m->_23 + v->z * m->_33 + v->w * m->_43,
		v->x * m->_14 + v->y * m->_24 + v->z * m->_34 + v->w * m->_44);
	*out = r;
	return out;
}

inline D3DXVECTOR2 *D3DXVec2TransformCoord(D3DXVECTOR2 *out, const D3DXVECTOR2 *v, const D3DXMATRIX *m)
{
	float x = v->x * m->_11 + v->y * m->_21 + m->_41;
	float y = v->x * m->_12 + v->y * m->_22 + m->_42;
	float w = v->x * m->_14 + v->y * m->_24 + m->_44;
	float iw = w != 0.0f ? 1.0f / w : 1.0f;
	out->x = x * iw; out->y = y * iw;
	return out;
}

inline D3DXMATRIX *D3DXMatrixIdentity(D3DXMATRIX *out)
{
	memset(out, 0, sizeof(D3DMATRIX));
	out->_11 = out->_22 = out->_33 = out->_44 = 1.0f;
	return out;
}

inline bool D3DXMatrixIsIdentity(const D3DXMATRIX *m)
{
	D3DXMATRIX i;
	D3DXMatrixIdentity(&i);
	return memcmp(m, &i, sizeof(D3DMATRIX)) == 0;
}

inline D3DXMATRIX *D3DXMatrixMultiply(D3DXMATRIX *out, const D3DXMATRIX *a, const D3DXMATRIX *b)
{
	D3DXMATRIX r;
	for (int i = 0; i < 4; i++)
		for (int j = 0; j < 4; j++)
			r.m[i][j] = a->m[i][0] * b->m[0][j] + a->m[i][1] * b->m[1][j] + a->m[i][2] * b->m[2][j] + a->m[i][3] * b->m[3][j];
	*out = r;
	return out;
}

inline D3DXMATRIX *D3DXMatrixTranspose(D3DXMATRIX *out, const D3DXMATRIX *m)
{
	D3DXMATRIX r;
	for (int i = 0; i < 4; i++)
		for (int j = 0; j < 4; j++)
			r.m[i][j] = m->m[j][i];
	*out = r;
	return out;
}

inline D3DXMATRIX *D3DXMatrixMultiplyTranspose(D3DXMATRIX *out, const D3DXMATRIX *a, const D3DXMATRIX *b)
{
	D3DXMATRIX r;
	D3DXMatrixMultiply(&r, a, b);
	return D3DXMatrixTranspose(out, &r);
}

inline float D3DXMatrixDeterminant(const D3DXMATRIX *m)
{
	const float *a = &m->_11;
	float b0 = a[0] * a[5] - a[1] * a[4], b1 = a[0] * a[6] - a[2] * a[4], b2 = a[0] * a[7] - a[3] * a[4];
	float b3 = a[1] * a[6] - a[2] * a[5], b4 = a[1] * a[7] - a[3] * a[5], b5 = a[2] * a[7] - a[3] * a[6];
	float b6 = a[8] * a[13] - a[9] * a[12], b7 = a[8] * a[14] - a[10] * a[12], b8 = a[8] * a[15] - a[11] * a[12];
	float b9 = a[9] * a[14] - a[10] * a[13], b10 = a[9] * a[15] - a[11] * a[13], b11 = a[10] * a[15] - a[11] * a[14];
	return b0 * b11 - b1 * b10 + b2 * b9 + b3 * b8 - b4 * b7 + b5 * b6;
}

inline D3DXMATRIX *D3DXMatrixInverse(D3DXMATRIX *out, float *det, const D3DXMATRIX *m)
{
	const float *a = &m->_11;
	float b0 = a[0] * a[5] - a[1] * a[4], b1 = a[0] * a[6] - a[2] * a[4], b2 = a[0] * a[7] - a[3] * a[4];
	float b3 = a[1] * a[6] - a[2] * a[5], b4 = a[1] * a[7] - a[3] * a[5], b5 = a[2] * a[7] - a[3] * a[6];
	float b6 = a[8] * a[13] - a[9] * a[12], b7 = a[8] * a[14] - a[10] * a[12], b8 = a[8] * a[15] - a[11] * a[12];
	float b9 = a[9] * a[14] - a[10] * a[13], b10 = a[9] * a[15] - a[11] * a[13], b11 = a[10] * a[15] - a[11] * a[14];
	float d = b0 * b11 - b1 * b10 + b2 * b9 + b3 * b8 - b4 * b7 + b5 * b6;
	if (det) *det = d;
	if (d == 0.0f) return nullptr;
	float id = 1.0f / d;
	D3DXMATRIX r;
	float *o = &r._11;
	o[0] = (a[5] * b11 - a[6] * b10 + a[7] * b9) * id;
	o[1] = (-a[1] * b11 + a[2] * b10 - a[3] * b9) * id;
	o[2] = (a[13] * b5 - a[14] * b4 + a[15] * b3) * id;
	o[3] = (-a[9] * b5 + a[10] * b4 - a[11] * b3) * id;
	o[4] = (-a[4] * b11 + a[6] * b8 - a[7] * b7) * id;
	o[5] = (a[0] * b11 - a[2] * b8 + a[3] * b7) * id;
	o[6] = (-a[12] * b5 + a[14] * b2 - a[15] * b1) * id;
	o[7] = (a[8] * b5 - a[10] * b2 + a[11] * b1) * id;
	o[8] = (a[4] * b10 - a[5] * b8 + a[7] * b6) * id;
	o[9] = (-a[0] * b10 + a[1] * b8 - a[3] * b6) * id;
	o[10] = (a[12] * b4 - a[13] * b2 + a[15] * b0) * id;
	o[11] = (-a[8] * b4 + a[9] * b2 - a[11] * b0) * id;
	o[12] = (-a[4] * b9 + a[5] * b7 - a[6] * b6) * id;
	o[13] = (a[0] * b9 - a[1] * b7 + a[2] * b6) * id;
	o[14] = (-a[12] * b3 + a[13] * b1 - a[14] * b0) * id;
	o[15] = (a[8] * b3 - a[9] * b1 + a[10] * b0) * id;
	*out = r;
	return out;
}

inline D3DXMATRIX *D3DXMatrixScaling(D3DXMATRIX *out, float sx, float sy, float sz)
{
	D3DXMatrixIdentity(out);
	out->_11 = sx; out->_22 = sy; out->_33 = sz;
	return out;
}

inline D3DXMATRIX *D3DXMatrixTranslation(D3DXMATRIX *out, float x, float y, float z)
{
	D3DXMatrixIdentity(out);
	out->_41 = x; out->_42 = y; out->_43 = z;
	return out;
}

inline D3DXMATRIX *D3DXMatrixRotationX(D3DXMATRIX *out, float a)
{
	float s = sinf(a), c = cosf(a);
	D3DXMatrixIdentity(out);
	out->_22 = c; out->_23 = s; out->_32 = -s; out->_33 = c;
	return out;
}

inline D3DXMATRIX *D3DXMatrixRotationY(D3DXMATRIX *out, float a)
{
	float s = sinf(a), c = cosf(a);
	D3DXMatrixIdentity(out);
	out->_11 = c; out->_13 = -s; out->_31 = s; out->_33 = c;
	return out;
}

inline D3DXMATRIX *D3DXMatrixRotationZ(D3DXMATRIX *out, float a)
{
	float s = sinf(a), c = cosf(a);
	D3DXMatrixIdentity(out);
	out->_11 = c; out->_12 = s; out->_21 = -s; out->_22 = c;
	return out;
}

inline D3DXMATRIX *D3DXMatrixRotationAxis(D3DXMATRIX *out, const D3DXVECTOR3 *axis, float a)
{
	D3DXVECTOR3 v;
	D3DXVec3Normalize(&v, axis);
	float s = sinf(a), c = cosf(a), t = 1.0f - c;
	D3DXMatrixIdentity(out);
	out->_11 = t * v.x * v.x + c;
	out->_21 = t * v.x * v.y - s * v.z;
	out->_31 = t * v.x * v.z + s * v.y;
	out->_12 = t * v.x * v.y + s * v.z;
	out->_22 = t * v.y * v.y + c;
	out->_32 = t * v.y * v.z - s * v.x;
	out->_13 = t * v.x * v.z - s * v.y;
	out->_23 = t * v.y * v.z + s * v.x;
	out->_33 = t * v.z * v.z + c;
	return out;
}

inline D3DXMATRIX *D3DXMatrixLookAtLH(D3DXMATRIX *out, const D3DXVECTOR3 *eye, const D3DXVECTOR3 *at, const D3DXVECTOR3 *up)
{
	D3DXVECTOR3 z = *at - *eye, x, y;
	D3DXVec3Normalize(&z, &z);
	D3DXVec3Cross(&x, up, &z);
	D3DXVec3Normalize(&x, &x);
	D3DXVec3Cross(&y, &z, &x);
	*out = D3DXMATRIX(x.x, y.x, z.x, 0, x.y, y.y, z.y, 0, x.z, y.z, z.z, 0,
		-D3DXVec3Dot(&x, eye), -D3DXVec3Dot(&y, eye), -D3DXVec3Dot(&z, eye), 1);
	return out;
}

inline D3DXMATRIX *D3DXMatrixLookAtRH(D3DXMATRIX *out, const D3DXVECTOR3 *eye, const D3DXVECTOR3 *at, const D3DXVECTOR3 *up)
{
	D3DXVECTOR3 z = *eye - *at, x, y;
	D3DXVec3Normalize(&z, &z);
	D3DXVec3Cross(&x, up, &z);
	D3DXVec3Normalize(&x, &x);
	D3DXVec3Cross(&y, &z, &x);
	*out = D3DXMATRIX(x.x, y.x, z.x, 0, x.y, y.y, z.y, 0, x.z, y.z, z.z, 0,
		-D3DXVec3Dot(&x, eye), -D3DXVec3Dot(&y, eye), -D3DXVec3Dot(&z, eye), 1);
	return out;
}

inline D3DXMATRIX *D3DXMatrixPerspectiveFovLH(D3DXMATRIX *out, float fovy, float aspect, float zn, float zf)
{
	float ys = 1.0f / tanf(fovy * 0.5f), xs = ys / aspect;
	*out = D3DXMATRIX(xs, 0, 0, 0, 0, ys, 0, 0, 0, 0, zf / (zf - zn), 1, 0, 0, -zn * zf / (zf - zn), 0);
	return out;
}

inline D3DXMATRIX *D3DXMatrixPerspectiveFovRH(D3DXMATRIX *out, float fovy, float aspect, float zn, float zf)
{
	float ys = 1.0f / tanf(fovy * 0.5f), xs = ys / aspect;
	*out = D3DXMATRIX(xs, 0, 0, 0, 0, ys, 0, 0, 0, 0, zf / (zn - zf), -1, 0, 0, zn * zf / (zn - zf), 0);
	return out;
}

inline D3DXMATRIX *D3DXMatrixOrthoLH(D3DXMATRIX *out, float w, float h, float zn, float zf)
{
	*out = D3DXMATRIX(2 / w, 0, 0, 0, 0, 2 / h, 0, 0, 0, 0, 1 / (zf - zn), 0, 0, 0, zn / (zn - zf), 1);
	return out;
}

inline D3DXMATRIX *D3DXMatrixOrthoOffCenterLH(D3DXMATRIX *out, float l, float r, float b, float t, float zn, float zf)
{
	*out = D3DXMATRIX(2 / (r - l), 0, 0, 0, 0, 2 / (t - b), 0, 0, 0, 0, 1 / (zf - zn), 0,
		(l + r) / (l - r), (t + b) / (b - t), zn / (zn - zf), 1);
	return out;
}

inline D3DXMATRIX *D3DXMatrixOrthoOffCenterRH(D3DXMATRIX *out, float l, float r, float b, float t, float zn, float zf)
{
	*out = D3DXMATRIX(2 / (r - l), 0, 0, 0, 0, 2 / (t - b), 0, 0, 0, 0, 1 / (zn - zf), 0,
		(l + r) / (l - r), (t + b) / (b - t), zn / (zn - zf), 1);
	return out;
}

inline D3DXMATRIX *D3DXMatrixAffineTransformation2D(D3DXMATRIX *out, float scaling, const D3DXVECTOR2 *rotCenter, float rotation, const D3DXVECTOR2 *translation)
{
	float s = sinf(rotation * 0.5f), c = cosf(rotation * 0.5f);
	float qz = s, qw = c;
	float s2 = 2.0f * qz * qz, sc = 2.0f * qz * qw;
	D3DXMatrixIdentity(out);
	out->_11 = scaling * (1.0f - s2);
	out->_12 = scaling * sc;
	out->_21 = -scaling * sc;
	out->_22 = scaling * (1.0f - s2);
	if (rotCenter) {
		out->_41 = rotCenter->x - rotCenter->x * out->_11 - rotCenter->y * out->_21;
		out->_42 = rotCenter->y - rotCenter->x * out->_12 - rotCenter->y * out->_22;
	}
	if (translation) {
		out->_41 += translation->x;
		out->_42 += translation->y;
	}
	return out;
}

inline D3DXMATRIX *D3DXMatrixTransformation2D(D3DXMATRIX *out, const D3DXVECTOR2 *scalingCenter, float scalingRotation,
	const D3DXVECTOR2 *scaling, const D3DXVECTOR2 *rotCenter, float rotation, const D3DXVECTOR2 *translation)
{
	D3DXVECTOR2 sc = scalingCenter ? *scalingCenter : D3DXVECTOR2(0, 0);
	D3DXVECTOR2 s = scaling ? *scaling : D3DXVECTOR2(1, 1);
	D3DXVECTOR2 rc = rotCenter ? *rotCenter : D3DXVECTOR2(0, 0);
	D3DXVECTOR2 t = translation ? *translation : D3DXVECTOR2(0, 0);
	D3DXMATRIX m1, m2, m3, m4, m5, m6, m7, r;
	D3DXMatrixTranslation(&m1, -sc.x, -sc.y, 0);
	D3DXMatrixRotationZ(&m2, -scalingRotation);
	D3DXMatrixScaling(&m3, s.x, s.y, 1);
	D3DXMatrixRotationZ(&m4, scalingRotation);
	D3DXMatrixTranslation(&m5, sc.x - rc.x, sc.y - rc.y, 0);
	D3DXMatrixRotationZ(&m6, rotation);
	D3DXMatrixTranslation(&m7, rc.x + t.x, rc.y + t.y, 0);
	r = m1 * m2 * m3 * m4 * m5 * m6 * m7;
	*out = r;
	return out;
}

inline D3DXMATRIX &D3DXMATRIX::operator*=(const D3DXMATRIX &b) { D3DXMatrixMultiply(this, this, &b); return *this; }
inline D3DXMATRIX D3DXMATRIX::operator*(const D3DXMATRIX &b) const { D3DXMATRIX r; D3DXMatrixMultiply(&r, this, &b); return r; }

inline D3DXCOLOR *D3DXColorModulate(D3DXCOLOR *out, const D3DXCOLOR *a, const D3DXCOLOR *b)
{
	*out = D3DXCOLOR(a->r * b->r, a->g * b->g, a->b * b->b, a->a * b->a);
	return out;
}

inline D3DXCOLOR *D3DXColorLerp(D3DXCOLOR *out, const D3DXCOLOR *a, const D3DXCOLOR *b, float s)
{
	*out = *a + (*b - *a) * s;
	return out;
}

inline float D3DXFloat16To32(uint16_t h)
{
	uint32_t s = (h >> 15) & 1, e = (h >> 10) & 0x1f, m = h & 0x3ff, f;
	if (e == 0) {
		if (m == 0) f = s << 31;
		else {
			while (!(m & 0x400)) { m <<= 1; e--; }
			e++;
			m &= ~0x400u;
			f = (s << 31) | ((e + 112) << 23) | (m << 13);
		}
	} else if (e == 31) f = (s << 31) | 0x7f800000 | (m << 13);
	else f = (s << 31) | ((e + 112) << 23) | (m << 13);
	float r;
	memcpy(&r, &f, 4);
	return r;
}

bool D3DXIntersectTri(const D3DXVECTOR3 *p0, const D3DXVECTOR3 *p1, const D3DXVECTOR3 *p2,
	const D3DXVECTOR3 *rayPos, const D3DXVECTOR3 *rayDir, float *u, float *v, float *dist);
HRESULT D3DXComputeBoundingSphere(const D3DXVECTOR3 *firstPos, DWORD numVertices, DWORD stride, D3DXVECTOR3 *center, float *radius);

// ----------------------------------------------------------------------------
// Buffers, shaders and effects

struct ID3DXBuffer : IUnknownM9 {
	void *GetBufferPointer() { return m_data; }
	DWORD GetBufferSize() { return m_size; }
	ID3DXBuffer(const void *data, DWORD size);
	~ID3DXBuffer();
public:
	void *m_data;
	DWORD m_size;
};
typedef ID3DXBuffer *LPD3DXBUFFER;

typedef enum _D3DXREGISTER_SET { D3DXRS_BOOL, D3DXRS_INT4, D3DXRS_FLOAT4, D3DXRS_SAMPLER, D3DXRS_FORCE_DWORD = 0x7fffffff } D3DXREGISTER_SET;
typedef enum _D3DXPARAMETER_CLASS { D3DXPC_SCALAR, D3DXPC_VECTOR, D3DXPC_MATRIX_ROWS, D3DXPC_MATRIX_COLUMNS, D3DXPC_OBJECT, D3DXPC_STRUCT, D3DXPC_FORCE_DWORD = 0x7fffffff } D3DXPARAMETER_CLASS;
typedef enum _D3DXPARAMETER_TYPE {
	D3DXPT_VOID, D3DXPT_BOOL, D3DXPT_INT, D3DXPT_FLOAT, D3DXPT_STRING, D3DXPT_TEXTURE, D3DXPT_TEXTURE1D, D3DXPT_TEXTURE2D,
	D3DXPT_TEXTURE3D, D3DXPT_TEXTURECUBE, D3DXPT_SAMPLER, D3DXPT_SAMPLER1D, D3DXPT_SAMPLER2D, D3DXPT_SAMPLER3D,
	D3DXPT_SAMPLERCUBE, D3DXPT_PIXELSHADER, D3DXPT_VERTEXSHADER, D3DXPT_PIXELFRAGMENT, D3DXPT_VERTEXFRAGMENT,
	D3DXPT_UNSUPPORTED, D3DXPT_FORCE_DWORD = 0x7fffffff
} D3DXPARAMETER_TYPE;

typedef struct _D3DXCONSTANT_DESC {
	const char *Name;
	D3DXREGISTER_SET RegisterSet;
	UINT RegisterIndex;
	UINT RegisterCount;
	D3DXPARAMETER_CLASS Class;
	D3DXPARAMETER_TYPE Type;
	UINT Rows, Columns, Elements, StructMembers, Bytes;
	const void *DefaultValue;
} D3DXCONSTANT_DESC;

typedef struct _D3DXPARAMETER_DESC {
	const char *Name;
	const char *Semantic;
	D3DXPARAMETER_CLASS Class;
	D3DXPARAMETER_TYPE Type;
	UINT Rows, Columns, Elements, Annotations, StructMembers;
	DWORD Flags;
	UINT Bytes;
} D3DXPARAMETER_DESC;

namespace m9 { struct Effect; struct ConstantTable; struct Font; }

struct ID3DXConstantTable : IUnknownM9 {
	D3DXHANDLE GetConstantByName(D3DXHANDLE parent, const char *name);
	D3DXHANDLE GetConstant(D3DXHANDLE parent, UINT index);
	HRESULT GetConstantDesc(D3DXHANDLE h, D3DXCONSTANT_DESC *desc, UINT *count);
	UINT GetSamplerIndex(D3DXHANDLE h);
	HRESULT SetDefaults(IDirect3DDevice9 *dev) { return S_OK; }
	HRESULT SetValue(IDirect3DDevice9 *dev, D3DXHANDLE h, const void *data, UINT bytes);
	HRESULT SetBool(IDirect3DDevice9 *dev, D3DXHANDLE h, WINBOOL b);
	HRESULT SetInt(IDirect3DDevice9 *dev, D3DXHANDLE h, INT n);
	HRESULT SetFloat(IDirect3DDevice9 *dev, D3DXHANDLE h, float f);
	HRESULT SetFloatArray(IDirect3DDevice9 *dev, D3DXHANDLE h, const float *f, UINT count);
	HRESULT SetIntArray(IDirect3DDevice9 *dev, D3DXHANDLE h, const INT *n, UINT count);
	HRESULT SetBoolArray(IDirect3DDevice9 *dev, D3DXHANDLE h, const WINBOOL *b, UINT count);
	HRESULT SetVector(IDirect3DDevice9 *dev, D3DXHANDLE h, const D3DXVECTOR4 *v);
	HRESULT SetVectorArray(IDirect3DDevice9 *dev, D3DXHANDLE h, const D3DXVECTOR4 *v, UINT count);
	HRESULT SetMatrix(IDirect3DDevice9 *dev, D3DXHANDLE h, const D3DXMATRIX *m);
	HRESULT SetMatrixArray(IDirect3DDevice9 *dev, D3DXHANDLE h, const D3DXMATRIX *m, UINT count);
	m9::ConstantTable *Impl() const { return m_table; }
	~ID3DXConstantTable();
public:
	m9::ConstantTable *m_table = nullptr;
	friend struct m9::Device;
	friend struct m9::Shader;
	friend HRESULT D3DXCompileShaderFromFileA(const char *, const D3DXMACRO *, void *, const char *, const char *, DWORD, ID3DXBuffer **, ID3DXBuffer **, ID3DXConstantTable **);
};
typedef ID3DXConstantTable *LPD3DXCONSTANTTABLE;

struct ID3DXEffectPool : IUnknownM9 {};
typedef ID3DXEffectPool *LPD3DXEFFECTPOOL;

struct ID3DXEffect : IUnknownM9 {
	D3DXHANDLE GetParameterByName(D3DXHANDLE parent, const char *name);
	D3DXHANDLE GetParameterBySemantic(D3DXHANDLE parent, const char *semantic);
	D3DXHANDLE GetParameter(D3DXHANDLE parent, UINT index);
	D3DXHANDLE GetParameterElement(D3DXHANDLE param, UINT index);
	HRESULT GetParameterDesc(D3DXHANDLE h, D3DXPARAMETER_DESC *desc);
	D3DXHANDLE GetTechniqueByName(const char *name);
	D3DXHANDLE GetTechnique(UINT index);
	D3DXHANDLE GetCurrentTechnique();
	HRESULT SetTechnique(D3DXHANDLE h);
	HRESULT ValidateTechnique(D3DXHANDLE h);
	HRESULT FindNextValidTechnique(D3DXHANDLE h, D3DXHANDLE *next);
	HRESULT Begin(UINT *passes, DWORD flags);
	HRESULT BeginPass(UINT pass);
	HRESULT CommitChanges();
	HRESULT EndPass();
	HRESULT End();
	HRESULT OnLostDevice() { return S_OK; }
	HRESULT OnResetDevice() { return S_OK; }
	HRESULT GetDevice(IDirect3DDevice9 **dev);

	HRESULT SetValue(D3DXHANDLE h, const void *data, UINT bytes);
	HRESULT GetValue(D3DXHANDLE h, void *data, UINT bytes);
	HRESULT SetBool(D3DXHANDLE h, WINBOOL b);
	HRESULT GetBool(D3DXHANDLE h, WINBOOL *b);
	HRESULT SetBoolArray(D3DXHANDLE h, const WINBOOL *b, UINT count);
	HRESULT SetInt(D3DXHANDLE h, INT n);
	HRESULT GetInt(D3DXHANDLE h, INT *n);
	HRESULT SetIntArray(D3DXHANDLE h, const INT *n, UINT count);
	HRESULT SetFloat(D3DXHANDLE h, float f);
	HRESULT GetFloat(D3DXHANDLE h, float *f);
	HRESULT SetFloatArray(D3DXHANDLE h, const float *f, UINT count);
	HRESULT SetVector(D3DXHANDLE h, const D3DXVECTOR4 *v);
	HRESULT GetVector(D3DXHANDLE h, D3DXVECTOR4 *v);
	HRESULT SetVectorArray(D3DXHANDLE h, const D3DXVECTOR4 *v, UINT count);
	HRESULT SetMatrix(D3DXHANDLE h, const D3DXMATRIX *m);
	HRESULT GetMatrix(D3DXHANDLE h, D3DXMATRIX *m);
	HRESULT SetMatrixArray(D3DXHANDLE h, const D3DXMATRIX *m, UINT count);
	HRESULT SetMatrixTranspose(D3DXHANDLE h, const D3DXMATRIX *m);
	HRESULT SetTexture(D3DXHANDLE h, IDirect3DBaseTexture9 *tex);
	HRESULT GetTexture(D3DXHANDLE h, IDirect3DBaseTexture9 **tex);

	m9::Effect *Impl() const { return m_effect; }
	~ID3DXEffect();
public:
	m9::Effect *m_effect = nullptr;
	friend struct m9::Device;
	friend HRESULT D3DXCreateEffectFromFileA(IDirect3DDevice9 *, const char *, const D3DXMACRO *, void *, DWORD, ID3DXEffectPool *, ID3DXEffect **, ID3DXBuffer **);
};
typedef ID3DXEffect *LPD3DXEFFECT;

HRESULT D3DXCreateEffectFromFileA(IDirect3DDevice9 *dev, const char *file, const D3DXMACRO *defines, void *include,
	DWORD flags, ID3DXEffectPool *pool, ID3DXEffect **effect, ID3DXBuffer **errors);
#define D3DXCreateEffectFromFile D3DXCreateEffectFromFileA

HRESULT D3DXCompileShaderFromFileA(const char *file, const D3DXMACRO *defines, void *include, const char *function,
	const char *profile, DWORD flags, ID3DXBuffer **shader, ID3DXBuffer **errors, ID3DXConstantTable **table);
#define D3DXCompileShaderFromFile D3DXCompileShaderFromFileA

HRESULT D3DXGetShaderConstantTable(const DWORD *function, ID3DXConstantTable **table);
HRESULT D3DXDisassembleShader(const DWORD *shader, WINBOOL colorCode, const char *comments, ID3DXBuffer **out);
HRESULT D3DXDisassembleEffect(ID3DXEffect *effect, WINBOOL colorCode, ID3DXBuffer **out);
inline WINBOOL D3DXCheckVersion(UINT, UINT) { return TRUE; }

// ----------------------------------------------------------------------------
// Fonts

typedef struct _D3DXFONT_DESCA {
	INT Height;
	UINT Width;
	UINT Weight;
	UINT MipLevels;
	WINBOOL Italic;
	BYTE CharSet;
	BYTE OutputPrecision;
	BYTE Quality;
	BYTE PitchAndFamily;
	char FaceName[32];
} D3DXFONT_DESC, D3DXFONT_DESCA;

#define DT_TOP 0x00000000
#define DT_LEFT 0x00000000
#define DT_CENTER 0x00000001
#define DT_RIGHT 0x00000002
#define DT_VCENTER 0x00000004
#define DT_BOTTOM 0x00000008
#define DT_WORDBREAK 0x00000010
#define DT_SINGLELINE 0x00000020
#define DT_EXPANDTABS 0x00000040
#define DT_NOCLIP 0x00000100
#define DT_CALCRECT 0x00000400

typedef struct tagTEXTMETRICW {
	LONG tmHeight, tmAscent, tmDescent, tmInternalLeading, tmExternalLeading, tmAveCharWidth, tmMaxCharWidth;
	LONG tmWeight, tmOverhang, tmDigitizedAspectX, tmDigitizedAspectY;
	wchar_t tmFirstChar, tmLastChar, tmDefaultChar, tmBreakChar;
	BYTE tmItalic, tmUnderlined, tmStruckOut, tmPitchAndFamily, tmCharSet;
} TEXTMETRICW, TEXTMETRICA, TEXTMETRIC;

struct ID3DXSprite;

struct ID3DXFont : IUnknownM9 {
	INT DrawTextW(ID3DXSprite *sprite, const wchar_t *text, INT count, RECT *rect, DWORD format, D3DCOLOR color);
	INT DrawTextA(ID3DXSprite *sprite, const char *text, INT count, RECT *rect, DWORD format, D3DCOLOR color);
	WINBOOL GetTextMetricsW(TEXTMETRICW *tm);
	HRESULT GetDescA(D3DXFONT_DESCA *desc);
	HRESULT PreloadCharacters(UINT first, UINT last) { return S_OK; }
	HRESULT PreloadTextA(const char *, INT) { return S_OK; }
	HRESULT OnLostDevice() { return S_OK; }
	HRESULT OnResetDevice() { return S_OK; }
	m9::Font *Impl() const { return m_font; }
	~ID3DXFont();
public:
	m9::Font *m_font = nullptr;
	friend HRESULT D3DXCreateFontIndirectA(IDirect3DDevice9 *, const D3DXFONT_DESCA *, ID3DXFont **);
};
typedef ID3DXFont *LPD3DXFONT;

HRESULT D3DXCreateFontIndirectA(IDirect3DDevice9 *dev, const D3DXFONT_DESCA *desc, ID3DXFont **font);
#define D3DXCreateFontIndirect D3DXCreateFontIndirectA
HRESULT D3DXCreateFontA(IDirect3DDevice9 *dev, INT height, UINT width, UINT weight, UINT mips, WINBOOL italic,
	DWORD charset, DWORD precision, DWORD quality, DWORD pitchAndFamily, const char *face, ID3DXFont **font);
#define D3DXCreateFont D3DXCreateFontA

// ----------------------------------------------------------------------------
// Textures and surfaces

HRESULT D3DXCreateTexture(IDirect3DDevice9 *dev, UINT w, UINT h, UINT mips, DWORD usage, D3DFORMAT fmt, D3DPOOL pool, IDirect3DTexture9 **tex);
HRESULT D3DXCreateCubeTexture(IDirect3DDevice9 *dev, UINT size, UINT mips, DWORD usage, D3DFORMAT fmt, D3DPOOL pool, IDirect3DCubeTexture9 **tex);
HRESULT D3DXCreateVolumeTexture(IDirect3DDevice9 *dev, UINT w, UINT h, UINT d, UINT mips, DWORD usage, D3DFORMAT fmt, D3DPOOL pool, IDirect3DVolumeTexture9 **tex);

HRESULT D3DXGetImageInfoFromFileA(const char *file, D3DXIMAGE_INFO *info);
#define D3DXGetImageInfoFromFile D3DXGetImageInfoFromFileA
HRESULT D3DXGetImageInfoFromFileInMemory(const void *data, UINT size, D3DXIMAGE_INFO *info);

HRESULT D3DXCreateTextureFromFileA(IDirect3DDevice9 *dev, const char *file, IDirect3DTexture9 **tex);
#define D3DXCreateTextureFromFile D3DXCreateTextureFromFileA
HRESULT D3DXCreateTextureFromFileExA(IDirect3DDevice9 *dev, const char *file, UINT w, UINT h, UINT mips, DWORD usage,
	D3DFORMAT fmt, D3DPOOL pool, DWORD filter, DWORD mipFilter, D3DCOLOR colorKey, D3DXIMAGE_INFO *info, PALETTEENTRY *palette, IDirect3DTexture9 **tex);
#define D3DXCreateTextureFromFileEx D3DXCreateTextureFromFileExA
HRESULT D3DXCreateTextureFromFileInMemoryEx(IDirect3DDevice9 *dev, const void *data, UINT size, UINT w, UINT h, UINT mips, DWORD usage,
	D3DFORMAT fmt, D3DPOOL pool, DWORD filter, DWORD mipFilter, D3DCOLOR colorKey, D3DXIMAGE_INFO *info, PALETTEENTRY *palette, IDirect3DTexture9 **tex);
HRESULT D3DXCreateTextureFromFileInMemory(IDirect3DDevice9 *dev, const void *data, UINT size, IDirect3DTexture9 **tex);
HRESULT D3DXCreateCubeTextureFromFileA(IDirect3DDevice9 *dev, const char *file, IDirect3DCubeTexture9 **tex);
#define D3DXCreateCubeTextureFromFile D3DXCreateCubeTextureFromFileA

HRESULT D3DXLoadSurfaceFromSurface(IDirect3DSurface9 *dst, const PALETTEENTRY *dstPal, const RECT *dstRect,
	IDirect3DSurface9 *src, const PALETTEENTRY *srcPal, const RECT *srcRect, DWORD filter, D3DCOLOR colorKey);
HRESULT D3DXLoadSurfaceFromFileA(IDirect3DSurface9 *dst, const PALETTEENTRY *dstPal, const RECT *dstRect,
	const char *file, const RECT *srcRect, DWORD filter, D3DCOLOR colorKey, D3DXIMAGE_INFO *info);
#define D3DXLoadSurfaceFromFile D3DXLoadSurfaceFromFileA
HRESULT D3DXLoadSurfaceFromFileInMemory(IDirect3DSurface9 *dst, const PALETTEENTRY *dstPal, const RECT *dstRect,
	const void *data, UINT size, const RECT *srcRect, DWORD filter, D3DCOLOR colorKey, D3DXIMAGE_INFO *info);
HRESULT D3DXLoadSurfaceFromMemory(IDirect3DSurface9 *dst, const PALETTEENTRY *dstPal, const RECT *dstRect,
	const void *src, D3DFORMAT srcFmt, UINT srcPitch, const PALETTEENTRY *srcPal, const RECT *srcRect, DWORD filter, D3DCOLOR colorKey);

HRESULT D3DXSaveTextureToFileA(const char *file, D3DXIMAGE_FILEFORMAT fmt, IDirect3DBaseTexture9 *tex, const PALETTEENTRY *pal);
#define D3DXSaveTextureToFile D3DXSaveTextureToFileA
HRESULT D3DXSaveSurfaceToFileA(const char *file, D3DXIMAGE_FILEFORMAT fmt, IDirect3DSurface9 *surf, const PALETTEENTRY *pal, const RECT *rect);
#define D3DXSaveSurfaceToFile D3DXSaveSurfaceToFileA
HRESULT D3DXFilterTexture(IDirect3DBaseTexture9 *tex, const PALETTEENTRY *pal, UINT srcLevel, DWORD filter);
