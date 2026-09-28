// not upstream: d3d9types/d3dx9math counterparts, same layouts as D3DX (row vectors, v * M)

#ifndef __D3DXMATH_H
#define __D3DXMATH_H

#include "OrbiterPlatform.h"
#include <cmath>
#include <cstring>

typedef DWORD D3DCOLOR;

struct D3DVECTOR { float x, y, z; };

struct D3DCOLORVALUE { float r, g, b, a; };

struct D3DMATRIX {
	union {
		struct {
			float _11, _12, _13, _14;
			float _21, _22, _23, _24;
			float _31, _32, _33, _34;
			float _41, _42, _43, _44;
		};
		float m[4][4];
	};
};

struct D3DMATERIAL9 {
	D3DCOLORVALUE Diffuse;
	D3DCOLORVALUE Ambient;
	D3DCOLORVALUE Specular;
	D3DCOLORVALUE Emissive;
	float Power;
};

#define D3DCOLOR_ARGB(a,r,g,b) ((D3DCOLOR)((((a)&0xff)<<24)|(((r)&0xff)<<16)|(((g)&0xff)<<8)|((b)&0xff)))
#define D3DCOLOR_RGBA(r,g,b,a) D3DCOLOR_ARGB(a,r,g,b)
#define D3DCOLOR_XRGB(r,g,b)   D3DCOLOR_ARGB(0xff,r,g,b)
#define D3DCOLOR_COLORVALUE(r,g,b,a) D3DCOLOR_RGBA((DWORD)((r)*255.f),(DWORD)((g)*255.f),(DWORD)((b)*255.f),(DWORD)((a)*255.f))

// D3DXVECTOR2

struct D3DXVECTOR2 {
	float x, y;

	D3DXVECTOR2 () {}
	D3DXVECTOR2 (const float *pf): x(pf[0]), y(pf[1]) {}
	D3DXVECTOR2 (float fx, float fy): x(fx), y(fy) {}

	operator float* () { return &x; }
	operator const float* () const { return &x; }

	D3DXVECTOR2 &operator+= (const D3DXVECTOR2 &v) { x += v.x; y += v.y; return *this; }
	D3DXVECTOR2 &operator-= (const D3DXVECTOR2 &v) { x -= v.x; y -= v.y; return *this; }
	D3DXVECTOR2 &operator*= (float f) { x *= f; y *= f; return *this; }
	D3DXVECTOR2 &operator/= (float f) { float i = 1.0f/f; x *= i; y *= i; return *this; }

	D3DXVECTOR2 operator+ () const { return *this; }
	D3DXVECTOR2 operator- () const { return D3DXVECTOR2(-x, -y); }

	D3DXVECTOR2 operator+ (const D3DXVECTOR2 &v) const { return D3DXVECTOR2(x+v.x, y+v.y); }
	D3DXVECTOR2 operator- (const D3DXVECTOR2 &v) const { return D3DXVECTOR2(x-v.x, y-v.y); }
	D3DXVECTOR2 operator* (float f) const { return D3DXVECTOR2(x*f, y*f); }
	D3DXVECTOR2 operator/ (float f) const { float i = 1.0f/f; return D3DXVECTOR2(x*i, y*i); }
	friend D3DXVECTOR2 operator* (float f, const D3DXVECTOR2 &v) { return D3DXVECTOR2(f*v.x, f*v.y); }

	bool operator== (const D3DXVECTOR2 &v) const { return x == v.x && y == v.y; }
	bool operator!= (const D3DXVECTOR2 &v) const { return x != v.x || y != v.y; }
};

// D3DXVECTOR3

struct D3DXVECTOR3: public D3DVECTOR {
	D3DXVECTOR3 () {}
	D3DXVECTOR3 (const float *pf) { x = pf[0]; y = pf[1]; z = pf[2]; }
	D3DXVECTOR3 (const D3DVECTOR &v) { x = v.x; y = v.y; z = v.z; }
	D3DXVECTOR3 (float fx, float fy, float fz) { x = fx; y = fy; z = fz; }

	operator float* () { return &x; }
	operator const float* () const { return &x; }

	D3DXVECTOR3 &operator+= (const D3DXVECTOR3 &v) { x += v.x; y += v.y; z += v.z; return *this; }
	D3DXVECTOR3 &operator-= (const D3DXVECTOR3 &v) { x -= v.x; y -= v.y; z -= v.z; return *this; }
	D3DXVECTOR3 &operator*= (float f) { x *= f; y *= f; z *= f; return *this; }
	D3DXVECTOR3 &operator/= (float f) { float i = 1.0f/f; x *= i; y *= i; z *= i; return *this; }

	D3DXVECTOR3 operator+ () const { return *this; }
	D3DXVECTOR3 operator- () const { return D3DXVECTOR3(-x, -y, -z); }

	D3DXVECTOR3 operator+ (const D3DXVECTOR3 &v) const { return D3DXVECTOR3(x+v.x, y+v.y, z+v.z); }
	D3DXVECTOR3 operator- (const D3DXVECTOR3 &v) const { return D3DXVECTOR3(x-v.x, y-v.y, z-v.z); }
	D3DXVECTOR3 operator* (float f) const { return D3DXVECTOR3(x*f, y*f, z*f); }
	D3DXVECTOR3 operator/ (float f) const { float i = 1.0f/f; return D3DXVECTOR3(x*i, y*i, z*i); }
	friend D3DXVECTOR3 operator* (float f, const D3DXVECTOR3 &v) { return D3DXVECTOR3(f*v.x, f*v.y, f*v.z); }

	bool operator== (const D3DXVECTOR3 &v) const { return x == v.x && y == v.y && z == v.z; }
	bool operator!= (const D3DXVECTOR3 &v) const { return x != v.x || y != v.y || z != v.z; }
};

// D3DXVECTOR4

struct D3DXVECTOR4 {
	float x, y, z, w;

	D3DXVECTOR4 () {}
	D3DXVECTOR4 (const float *pf): x(pf[0]), y(pf[1]), z(pf[2]), w(pf[3]) {}
	D3DXVECTOR4 (const D3DVECTOR &xyz, float fw): x(xyz.x), y(xyz.y), z(xyz.z), w(fw) {}
	D3DXVECTOR4 (float fx, float fy, float fz, float fw): x(fx), y(fy), z(fz), w(fw) {}

	operator float* () { return &x; }
	operator const float* () const { return &x; }

	D3DXVECTOR4 &operator+= (const D3DXVECTOR4 &v) { x += v.x; y += v.y; z += v.z; w += v.w; return *this; }
	D3DXVECTOR4 &operator-= (const D3DXVECTOR4 &v) { x -= v.x; y -= v.y; z -= v.z; w -= v.w; return *this; }
	D3DXVECTOR4 &operator*= (float f) { x *= f; y *= f; z *= f; w *= f; return *this; }
	D3DXVECTOR4 &operator/= (float f) { float i = 1.0f/f; x *= i; y *= i; z *= i; w *= i; return *this; }

	D3DXVECTOR4 operator+ () const { return *this; }
	D3DXVECTOR4 operator- () const { return D3DXVECTOR4(-x, -y, -z, -w); }

	D3DXVECTOR4 operator+ (const D3DXVECTOR4 &v) const { return D3DXVECTOR4(x+v.x, y+v.y, z+v.z, w+v.w); }
	D3DXVECTOR4 operator- (const D3DXVECTOR4 &v) const { return D3DXVECTOR4(x-v.x, y-v.y, z-v.z, w-v.w); }
	D3DXVECTOR4 operator* (float f) const { return D3DXVECTOR4(x*f, y*f, z*f, w*f); }
	D3DXVECTOR4 operator/ (float f) const { float i = 1.0f/f; return D3DXVECTOR4(x*i, y*i, z*i, w*i); }
	friend D3DXVECTOR4 operator* (float f, const D3DXVECTOR4 &v) { return D3DXVECTOR4(f*v.x, f*v.y, f*v.z, f*v.w); }

	bool operator== (const D3DXVECTOR4 &v) const { return x == v.x && y == v.y && z == v.z && w == v.w; }
	bool operator!= (const D3DXVECTOR4 &v) const { return x != v.x || y != v.y || z != v.z || w != v.w; }
};

// D3DXMATRIX

struct D3DXMATRIX: public D3DMATRIX {
	D3DXMATRIX () {}
	D3DXMATRIX (const float *pf) { memcpy (&_11, pf, sizeof(D3DMATRIX)); }
	D3DXMATRIX (const D3DMATRIX &mat) { memcpy (&_11, &mat, sizeof(D3DMATRIX)); }
	D3DXMATRIX (float f11, float f12, float f13, float f14,
	            float f21, float f22, float f23, float f24,
	            float f31, float f32, float f33, float f34,
	            float f41, float f42, float f43, float f44)
	{
		_11 = f11; _12 = f12; _13 = f13; _14 = f14;
		_21 = f21; _22 = f22; _23 = f23; _24 = f24;
		_31 = f31; _32 = f32; _33 = f33; _34 = f34;
		_41 = f41; _42 = f42; _43 = f43; _44 = f44;
	}

	float &operator() (UINT row, UINT col) { return m[row][col]; }
	float operator() (UINT row, UINT col) const { return m[row][col]; }

	operator float* () { return &_11; }
	operator const float* () const { return &_11; }

	D3DXMATRIX &operator*= (const D3DXMATRIX &mat);
	D3DXMATRIX &operator+= (const D3DXMATRIX &mat) { for (int i = 0; i < 16; i++) (&_11)[i] += (&mat._11)[i]; return *this; }
	D3DXMATRIX &operator-= (const D3DXMATRIX &mat) { for (int i = 0; i < 16; i++) (&_11)[i] -= (&mat._11)[i]; return *this; }
	D3DXMATRIX &operator*= (float f) { for (int i = 0; i < 16; i++) (&_11)[i] *= f; return *this; }
	D3DXMATRIX &operator/= (float f) { float i = 1.0f/f; return *this *= i; }

	D3DXMATRIX operator+ () const { return *this; }
	D3DXMATRIX operator- () const { D3DXMATRIX r(*this); return r *= -1.0f; }

	D3DXMATRIX operator* (const D3DXMATRIX &mat) const { D3DXMATRIX r(*this); return r *= mat; }
	D3DXMATRIX operator+ (const D3DXMATRIX &mat) const { D3DXMATRIX r(*this); return r += mat; }
	D3DXMATRIX operator- (const D3DXMATRIX &mat) const { D3DXMATRIX r(*this); return r -= mat; }
	D3DXMATRIX operator* (float f) const { D3DXMATRIX r(*this); return r *= f; }
	D3DXMATRIX operator/ (float f) const { D3DXMATRIX r(*this); return r /= f; }
	friend D3DXMATRIX operator* (float f, const D3DXMATRIX &mat) { return mat * f; }

	bool operator== (const D3DXMATRIX &mat) const { return !memcmp (this, &mat, sizeof(D3DXMATRIX)); }
	bool operator!= (const D3DXMATRIX &mat) const { return memcmp (this, &mat, sizeof(D3DXMATRIX)) != 0; }
};

typedef D3DXMATRIX *LPD3DXMATRIX;
typedef D3DXVECTOR2 *LPD3DXVECTOR2;
typedef D3DXVECTOR3 *LPD3DXVECTOR3;
typedef D3DXVECTOR4 *LPD3DXVECTOR4;

// D3DXCOLOR: DWORD conversions are 0xAARRGGBB, as D3DCOLOR

struct D3DXCOLOR {
	float r, g, b, a;

	D3DXCOLOR () {}
	D3DXCOLOR (DWORD argb)
	{
		const float f = 1.0f/255.0f;
		r = f * (float)(unsigned char)(argb >> 16);
		g = f * (float)(unsigned char)(argb >>  8);
		b = f * (float)(unsigned char)(argb      );
		a = f * (float)(unsigned char)(argb >> 24);
	}
	D3DXCOLOR (const float *pf): r(pf[0]), g(pf[1]), b(pf[2]), a(pf[3]) {}
	D3DXCOLOR (const D3DCOLORVALUE &c): r(c.r), g(c.g), b(c.b), a(c.a) {}
	D3DXCOLOR (float fr, float fg, float fb, float fa): r(fr), g(fg), b(fb), a(fa) {}

	operator DWORD () const
	{
		DWORD dwR = r >= 1.0f ? 0xff : r <= 0.0f ? 0x00 : (DWORD)(r * 255.0f + 0.5f);
		DWORD dwG = g >= 1.0f ? 0xff : g <= 0.0f ? 0x00 : (DWORD)(g * 255.0f + 0.5f);
		DWORD dwB = b >= 1.0f ? 0xff : b <= 0.0f ? 0x00 : (DWORD)(b * 255.0f + 0.5f);
		DWORD dwA = a >= 1.0f ? 0xff : a <= 0.0f ? 0x00 : (DWORD)(a * 255.0f + 0.5f);
		return (dwA << 24) | (dwR << 16) | (dwG << 8) | dwB;
	}
	operator float* () { return &r; }
	operator const float* () const { return &r; }
	operator D3DCOLORVALUE* () { return (D3DCOLORVALUE*)&r; }
	operator const D3DCOLORVALUE* () const { return (const D3DCOLORVALUE*)&r; }
	operator D3DCOLORVALUE& () { return *((D3DCOLORVALUE*)&r); }
	operator const D3DCOLORVALUE& () const { return *((const D3DCOLORVALUE*)&r); }

	D3DXCOLOR &operator+= (const D3DXCOLOR &c) { r += c.r; g += c.g; b += c.b; a += c.a; return *this; }
	D3DXCOLOR &operator-= (const D3DXCOLOR &c) { r -= c.r; g -= c.g; b -= c.b; a -= c.a; return *this; }
	D3DXCOLOR &operator*= (float f) { r *= f; g *= f; b *= f; a *= f; return *this; }
	D3DXCOLOR &operator/= (float f) { float i = 1.0f/f; r *= i; g *= i; b *= i; a *= i; return *this; }

	D3DXCOLOR operator+ () const { return *this; }
	D3DXCOLOR operator- () const { return D3DXCOLOR(-r, -g, -b, -a); }

	D3DXCOLOR operator+ (const D3DXCOLOR &c) const { return D3DXCOLOR(r+c.r, g+c.g, b+c.b, a+c.a); }
	D3DXCOLOR operator- (const D3DXCOLOR &c) const { return D3DXCOLOR(r-c.r, g-c.g, b-c.b, a-c.a); }
	D3DXCOLOR operator* (float f) const { return D3DXCOLOR(r*f, g*f, b*f, a*f); }
	D3DXCOLOR operator/ (float f) const { float i = 1.0f/f; return D3DXCOLOR(r*i, g*i, b*i, a*i); }
	friend D3DXCOLOR operator* (float f, const D3DXCOLOR &c) { return D3DXCOLOR(f*c.r, f*c.g, f*c.b, f*c.a); }

	bool operator== (const D3DXCOLOR &c) const { return r == c.r && g == c.g && b == c.b && a == c.a; }
	bool operator!= (const D3DXCOLOR &c) const { return r != c.r || g != c.g || b != c.b || a != c.a; }
};

typedef D3DXCOLOR *LPD3DXCOLOR;

// Vector functions

inline float D3DXVec2Length (const D3DXVECTOR2 *pV) { return sqrtf (pV->x*pV->x + pV->y*pV->y); }
inline float D3DXVec3Length (const D3DXVECTOR3 *pV) { return sqrtf (pV->x*pV->x + pV->y*pV->y + pV->z*pV->z); }
inline float D3DXVec3Dot (const D3DXVECTOR3 *pV1, const D3DXVECTOR3 *pV2) { return pV1->x*pV2->x + pV1->y*pV2->y + pV1->z*pV2->z; }

inline D3DXVECTOR3 *D3DXVec3Cross (D3DXVECTOR3 *pOut, const D3DXVECTOR3 *pV1, const D3DXVECTOR3 *pV2)
{
	D3DXVECTOR3 v (pV1->y*pV2->z - pV1->z*pV2->y, pV1->z*pV2->x - pV1->x*pV2->z, pV1->x*pV2->y - pV1->y*pV2->x);
	*pOut = v;
	return pOut;
}

// zero length gives the zero vector
D3DXVECTOR3 *D3DXVec3Normalize (D3DXVECTOR3 *pOut, const D3DXVECTOR3 *pV);

// (x,y,z,1) * M, divided by w
D3DXVECTOR3 *D3DXVec3TransformCoord (D3DXVECTOR3 *pOut, const D3DXVECTOR3 *pV, const D3DXMATRIX *pM);

// (x,y,z,0) * M
D3DXVECTOR3 *D3DXVec3TransformNormal (D3DXVECTOR3 *pOut, const D3DXVECTOR3 *pV, const D3DXMATRIX *pM);

// (x,y,z,1) * M
D3DXVECTOR4 *D3DXVec3Transform (D3DXVECTOR4 *pOut, const D3DXVECTOR3 *pV, const D3DXMATRIX *pM);

D3DXVECTOR4 *D3DXVec4Transform (D3DXVECTOR4 *pOut, const D3DXVECTOR4 *pV, const D3DXMATRIX *pM);

// Matrix functions

D3DXMATRIX *D3DXMatrixIdentity (D3DXMATRIX *pOut);
D3DXMATRIX *D3DXMatrixMultiply (D3DXMATRIX *pOut, const D3DXMATRIX *pM1, const D3DXMATRIX *pM2);

// NULL if the matrix is singular
D3DXMATRIX *D3DXMatrixInverse (D3DXMATRIX *pOut, float *pDeterminant, const D3DXMATRIX *pM);

D3DXMATRIX *D3DXMatrixScaling (D3DXMATRIX *pOut, float sx, float sy, float sz);
D3DXMATRIX *D3DXMatrixRotationAxis (D3DXMATRIX *pOut, const D3DXVECTOR3 *pV, float Angle);
D3DXMATRIX *D3DXMatrixLookAtRH (D3DXMATRIX *pOut, const D3DXVECTOR3 *pEye, const D3DXVECTOR3 *pAt, const D3DXVECTOR3 *pUp);
D3DXMATRIX *D3DXMatrixOrthoOffCenterLH (D3DXMATRIX *pOut, float l, float r, float b, float t, float zn, float zf);
D3DXMATRIX *D3DXMatrixOrthoOffCenterRH (D3DXMATRIX *pOut, float l, float r, float b, float t, float zn, float zf);
D3DXMATRIX *D3DXMatrixTransformation2D (D3DXMATRIX *pOut, const D3DXVECTOR2 *pScalingCenter, float ScalingRotation,
	const D3DXVECTOR2 *pScaling, const D3DXVECTOR2 *pRotationCenter, float Rotation, const D3DXVECTOR2 *pTranslation);
D3DXMATRIX *D3DXMatrixAffineTransformation2D (D3DXMATRIX *pOut, float Scaling, const D3DXVECTOR2 *pRotationCenter,
	float Rotation, const D3DXVECTOR2 *pTranslation);

// Geometry functions

// ray p + t*dir against triangle (p0,p1,p2); U,V weight p1 and p2
BOOL D3DXIntersectTri (const D3DXVECTOR3 *p0, const D3DXVECTOR3 *p1, const D3DXVECTOR3 *p2,
	const D3DXVECTOR3 *pRayPos, const D3DXVECTOR3 *pRayDir, float *pU, float *pV, float *pDist);

// centre = mean of the positions, radius = largest distance from it
int D3DXComputeBoundingSphere (const D3DXVECTOR3 *pFirstPosition, DWORD NumVertices, DWORD dwStride,
	D3DXVECTOR3 *pCenter, float *pRadius);

#endif // !__D3DXMATH_H
