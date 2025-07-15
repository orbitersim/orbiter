// =================================================================================================================================
// The MIT Lisence:
//
// Copyright (C) 2013-2016 Jarmo Nikkanen
//
// Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation 
// files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, 
// modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software 
// is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
// OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
// LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR
// IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
// =================================================================================================================================

#ifndef __MATHAPI_H
#define __MATHAPI_H

#include <assert.h>
#include "OrbiterAPI.h"


#if defined(_MSC_VER) && (_MSC_VER < 1920 ) // Microsoft Visual Studio Version 2017 and lower
#include <algorithm>
#endif

#if defined (D3D9CLIENT_EXPORTS) || (Orbiter_EXPORTS)
#define __XM
#include "DirectXMath.h"
using namespace DirectX;
#endif

namespace oapi
{
	/**
	 * \brief Integer-valued 2-D vector type.
	 * \note This structure is designed to be compatible with the Windows POINT type.
	 */
	union IVECTOR2 {
		IVECTOR2() { x = y = 0; }
		IVECTOR2(long x, long y) : x(x), y(y)
		{
		}
		long data[2];  ///< vector data array
		struct {
			long x;    ///< vector x coordinate
			long y;    ///< vector y coordinate
		};
	};


	/**
	* \brief 32-bit floating point 2D vector type.
	* \note This structure is compatible with the D3DXVECTOR2 type.
	*/
	typedef union FVECTOR2
	{
		FVECTOR2()
		{
			x = y = 0.0f;
		}

		FVECTOR2(float q)
		{
			x = y = q;
		}

		FVECTOR2(float _x, float _y)
		{
			x = _x;
			y = _y;
		}

		FVECTOR2(long _x, long _y)
		{
			x = float(_x);
			y = float(_y);
		}

		FVECTOR2(DWORD _x, DWORD _y)
		{
			x = float(_x);
			y = float(_y);
		}

		FVECTOR2(int _x, int _y)
		{
			x = float(_x);
			y = float(_y);
		}

		FVECTOR2(const IVECTOR2* p)
		{
			x = float(p->x);
			y = float(p->y);
		}

		FVECTOR2(const IVECTOR2& p)
		{
			x = float(p.x);
			y = float(p.y);
		}

#ifdef __XM	
		FVECTOR2(const XMVECTOR m)
		{
			XMStoreFloat2((XMFLOAT2*)this, m);
		}

		XMVECTOR XM() const
		{
			return XMLoadFloat2((XMFLOAT2*)this);
		}

		void Load(const XMVECTOR m)
		{
			XMStoreFloat2((XMFLOAT2*)this, m);
		}
#endif
		inline float& operator[](int i)
		{
			return data[i];
		}

		inline bool operator== (const FVECTOR2& f) const
		{
			return x == f.x && y == f.y;
		}

		inline bool operator!= (const FVECTOR2& f) const
		{
			return x != f.x || y != f.y;
		}

		float data[2];
		struct {
			float x, y;
		};
	} FVECTOR2;



	/**
	* \brief 32-bit floating point 3D vector type.
	* \note This structure is compatible with the D3DXVECTOR3 type.
	*/
	typedef union FVECTOR3
	{
		FVECTOR3()
		{
			x = y = z = 0.0f;
		}

		FVECTOR3(float q)
		{
			x = y = z = q;
		}

		FVECTOR3(float _x, float _y, float _z)
		{
			x = _x;
			y = _y;
			z = _z;
		}

		FVECTOR3(const VECTOR3& v)
		{
			x = float(v.x);
			y = float(v.y);
			z = float(v.z);
		}

#ifdef __XM
		FVECTOR3(const XMVECTOR v)
		{
			XMStoreFloat3((XMFLOAT3*)this, v);
		}

		XMVECTOR XM() const
		{
			return XMLoadFloat3((XMFLOAT3*)this);
		}

		void Load(const XMVECTOR v)
		{
			XMStoreFloat3((XMFLOAT3*)this, v);
		}
#endif

		inline VECTOR3 _V() const { VECTOR3 v = { x,y,z }; return v; }

		inline float& operator[](int i)
		{
			return data[i];
		}

		inline bool operator== (const FVECTOR3& f) const
		{
			return x == f.x && y == f.y && z == f.z;
		}

		inline bool operator!= (const FVECTOR3& f) const
		{
			return x != f.x || y != f.y || z != f.z;
		}

		float data[3];
		struct { float x, y, z; };
		struct { float r, g, b; };
		FVECTOR2 xy;
	} FVECTOR3;


	/**
	* \brief 32-bit floating point 4D vector type.
	* \note This structure is compatible with the D3DXVECTOR4 type.
	*/
	typedef union FVECTOR4
	{
		DWORD dword_abgr() const
		{
			DWORD dr = DWORD((std::max)(0.0f, r) * 255.0f + 0.5f);
			DWORD dg = DWORD((std::max)(0.0f, g) * 255.0f + 0.5f);
			DWORD db = DWORD((std::max)(0.0f, b) * 255.0f + 0.5f);
			DWORD da = DWORD((std::max)(0.0f, a) * 255.0f + 0.5f);
			if (dr > 0xFF) dr = 0xFF;
			if (dg > 0xFF) dg = 0xFF;
			if (db > 0xFF) db = 0xFF;
			if (da > 0xFF) da = 0xFF;
			return (da << 24) | (db << 16) | (dg << 8) | dr;
		}

		DWORD dword_argb() const
		{
			DWORD dr = DWORD((std::max)(0.0f, r) * 255.0f + 0.5f);
			DWORD dg = DWORD((std::max)(0.0f, g) * 255.0f + 0.5f);
			DWORD db = DWORD((std::max)(0.0f, b) * 255.0f + 0.5f);
			DWORD da = DWORD((std::max)(0.0f, a) * 255.0f + 0.5f);
			if (dr > 0xFF) dr = 0xFF;
			if (dg > 0xFF) dg = 0xFF;
			if (db > 0xFF) db = 0xFF;
			if (da > 0xFF) da = 0xFF;
			return (da << 24) | (dr << 16) | (dg << 8) | db;
		}

		FVECTOR4()
		{
			r = g = b = a = 0.0f;
		}

		FVECTOR4(float q)
		{
			x = y = z = w = q;
		}

		FVECTOR4(const COLOUR4& c)
		{
			r = c.r;
			g = c.g;
			b = c.b;
			a = c.a;
		}

		FVECTOR4(DWORD abgr)
		{
			DWORD dr = (abgr & 0xFF); abgr >>= 8;
			DWORD dg = (abgr & 0xFF); abgr >>= 8;
			DWORD db = (abgr & 0xFF); abgr >>= 8;
			DWORD da = (abgr & 0xFF);
			float q = 3.92156862e-3f;
			r = float(dr) * q;
			g = float(dg) * q;
			b = float(db) * q;
			a = float(da) * q;
		}

#ifdef __XM
		FVECTOR4(const XMVECTOR v)
		{
			XMStoreFloat4((XMFLOAT4*)this, v);
		}

		XMVECTOR XM() const
		{
			return XMLoadFloat4((XMFLOAT4*)this);
		}

		void Load(const XMVECTOR v)
		{
			XMStoreFloat4((XMFLOAT4*)this, v);
		}
#endif

		FVECTOR4(const VECTOR4& v)
		{
			x = float(v.x);
			y = float(v.y);
			z = float(v.z);
			w = float(v.w);
		}

		FVECTOR4(const VECTOR3& v, float _w)
		{
			x = float(v.x);
			y = float(v.y);
			z = float(v.z);
			w = _w;
		}

		FVECTOR4(const FVECTOR3& v, float _w)
		{
			rgb = v;
			w = _w;
		}

		FVECTOR4(float _x, float _y, float _z, float _w)
		{
			x = float(_x);
			y = float(_y);
			z = float(_z);
			w = float(_w);
		}

		operator const COLOUR4() const
		{
			COLOUR4 clr = { r,g,b,a };
			return clr;
		}

		inline float& operator[](int i)
		{
			return data[i];
		}

		inline bool operator== (const FVECTOR4& f) const
		{
			return x == f.x && y == f.y && z == f.z && w == f.w;
		}

		inline bool operator!= (const FVECTOR4& f) const
		{
			return x != f.x || y != f.y || z != f.z || w != f.w;
		}

		float data[4];
		struct { float x, y, z, w; };
		struct { float r, g, b, a; };
		FVECTOR3 xyz;     //  , w; };
		FVECTOR3 rgb;    //   , a; };
	} FVECTOR4;


	typedef union DRECT
	{
		DRECT()
		{
			left = right = top = bottom = 0.0;
		}

		DRECT(double l, double t, double r, double b)
		{
			left = l; top = t; right = r; bottom = b;
		}

		DRECT(float l, float t, float r, float b)
		{
			left = double(l); top = double(t); right = double(r); bottom = double(b);
		}

		DRECT(const DRECT& x)
		{
			left = x.left;
			top = x.top;
			right = x.right;
			bottom = x.bottom;
		}

		VECTOR4 vec;

		struct {
			double left, top, right, bottom;
		};

	} DRECT;


	/**
	* \brief Float-valued 4x4 matrix.
	* \note This structure is compatible with the D3DXMATRIX.
	*/
	typedef union FMATRIX4
	{

		FMATRIX4(int i = 1) { if (i == 0) Zero(); else Ident(); }


		FMATRIX4(float f) {
			m11 = m12 = m13, m14 = m21 = m22 = m23 = m24 = m31 = m32 = m33 = m34 = m41 = m42 = m43 = m44 = f;
		}


		FMATRIX4(float m11, float m12, float m13, float m14,
			float m21, float m22, float m23, float m24,
			float m31, float m32, float m33, float m34,
			float m41, float m42, float m43, float m44) :
			m11(m11), m12(m12), m13(m13), m14(m14),
			m21(m21), m22(m22), m23(m23), m24(m24),
			m31(m31), m32(m32), m33(m33), m34(m34),
			m41(m41), m42(m42), m43(m43), m44(m44)
		{
		}

		FMATRIX4(const float* pSrc) {
			for (int i = 0; i < 16; i++) data[i] = pSrc[i];
		}

		FMATRIX4(const MATRIX4& pSrc) {
			for (int i = 0; i < 16; i++) data[i] = float(pSrc.data[i]);
		}

		FMATRIX4(const FMATRIX4& pSrc) {
			for (int i = 0; i < 16; i++) data[i] = float(pSrc.data[i]);
		}

		FMATRIX4(const FMATRIX4* pSrc) {
			for (int i = 0; i < 16; i++) data[i] = float(pSrc->data[i]);
		}

#ifdef __XM	
		FMATRIX4(const XMMATRIX m)
		{
			XMStoreFloat4x4((XMFLOAT4X4*)this, m);
		}

		XMMATRIX XM() const
		{
			return XMLoadFloat4x4((XMFLOAT4X4*)this);
		}

		void Load(const XMMATRIX m)
		{
			XMStoreFloat4x4((XMFLOAT4X4*)this, m);
		}
#endif

		void Zero()
		{
			m21 = m31 = m41 = m12 = m32 = m42 = 0.0f;
			m13 = m23 = m43 = m14 = m24 = m34 = 0.0f;
			m11 = m22 = m33 = m44 = 0.0f;
		}

		void Ident()
		{
			m21 = m31 = m41 = m12 = m32 = m42 = 0.0f;
			m13 = m23 = m43 = m14 = m24 = m34 = 0.0f;
			m11 = m22 = m33 = m44 = 1.0f;
		}

		void _swap(float& a, float& b) { float c = a; a = b; b = c; }

		void Transpose()
		{
			_swap(m12, m21); _swap(m13, m31);
			_swap(m14, m41); _swap(m23, m32);
			_swap(m24, m42); _swap(m34, m43);
		}

		float data[16];
		struct { FVECTOR4 _x, _y, _z, _p; };
		struct { float m11, m12, m13, m14, m21, m22, m23, m24, m31, m32, m33, m34, m41, m42, m43, m44; };
	} FMATRIX4;
}


	// ==============================================================================================================
	// Type casting
	//

	using namespace oapi;

	inline VECTOR3 _V(const FVECTOR3& i) { return { double(i.x), double(i.y), double(i.z) }; }
	inline VECTOR3 _V(const VECTOR4 & i) { return  { double(i.x), double(i.y), double(i.z) }; }
	inline VECTOR3 _V(const FVECTOR4 & i) { return { double(i.x), double(i.y), double(i.z) }; }

	inline VECTOR4 _V4(const FVECTOR4& i) { return { i.x, i.y, i.z, i.w }; }
	inline VECTOR4 _V4(const VECTOR3& i, double w = 0.0) { return { i.x, i.y, i.z, w }; }
	inline VECTOR4 _V4(const FVECTOR3& i, float w = 0.0f) { return { i.x, i.y, i.z, w }; }

	inline FVECTOR3 _F(const VECTOR3& i) { return FVECTOR3(float(i.x), float(i.y), float(i.z)); }
	inline FVECTOR3 _F(const VECTOR4& i) { return FVECTOR3(float(i.x), float(i.y), float(i.z)); }
	inline FVECTOR3 _F(const FVECTOR4& i) { return FVECTOR3(float(i.x), float(i.y), float(i.z)); }
	inline FVECTOR3 _F(const VECTOR3* i) { return FVECTOR3(float(i->x), float(i->y), float(i->z)); }

	inline FVECTOR4 _F4(const VECTOR4& i) { return FVECTOR4(float(i.x), float(i.y), float(i.z), float(i.w)); }
	inline FVECTOR4 _F4(const VECTOR3& i, double w = 0.0) { return FVECTOR4(float(i.x), float(i.y), float(i.z), float(w)); }
	inline FVECTOR4 _F4(const FVECTOR3& i, float w = 0.0f) { return FVECTOR4(float(i.x), float(i.y), float(i.z), float(w)); }

	inline VECTOR4  _V4(double x, double y, double z, double w) { return { x, y, z, w }; }
	inline VECTOR4  _V4(int x, int y, int z, int w) { return { double(x), double(y), double(z), double(w) }; }
	inline FVECTOR4 _F4(float x, float y, float z, float w) { return FVECTOR4(x, y, z, w); }
	inline FVECTOR4 _F4(int x, int y, int z, int w) { return FVECTOR4(float(x), float(y), float(z), float(w)); }
	inline FVECTOR3 _F(float x, float y, float z) { return FVECTOR3(x, y, z); }
	inline FVECTOR3 _F(int x, int y, int z) { return FVECTOR3(float(x), float(y), float(z)); }
	inline FVECTOR2 _F2(float x, float y) { return FVECTOR2(x, y); }

	// ==============================================================================================================
	// FVECTOR2 operators
	//
	inline FVECTOR2& operator*= (FVECTOR2&v, float f) { v.x *= f; v.y *= f; return v; }
	inline FVECTOR2& operator/= (FVECTOR2&v, float f) { v.x /= f; v.y /= f; return v; }
	inline FVECTOR2& operator+= (FVECTOR2&v, float f) { v.x += f; v.y += f; return v; }
	inline FVECTOR2& operator-= (FVECTOR2&v, float f) { v.x -= f; v.y -= f; return v; }

	inline FVECTOR2& operator*= (FVECTOR2&v, const FVECTOR2& f) { v.x *= f.x; v.y *= f.y; return v; }
	inline FVECTOR2& operator/= (FVECTOR2&v, const FVECTOR2& f) { v.x /= f.x; v.y /= f.y; return v; }
	inline FVECTOR2& operator+= (FVECTOR2&v, const FVECTOR2& f) { v.x += f.x; v.y += f.y; return v; }
	inline FVECTOR2& operator-= (FVECTOR2&v, const FVECTOR2& f) { v.x -= f.x; v.y -= f.y; return v; }

	inline FVECTOR2 operator* (const FVECTOR2&v, float f) { return _F2(v.x * f, v.y * f); }
	inline FVECTOR2 operator/ (const FVECTOR2&v, float f) { return _F2(v.x / f, v.y / f); }
	inline FVECTOR2 operator+ (const FVECTOR2&v, float f) { return _F2(v.x + f, v.y + f); }
	inline FVECTOR2 operator- (const FVECTOR2&v, float f) { return _F2(v.x - f, v.y - f); }

	inline FVECTOR2 operator* (const FVECTOR2&v, const FVECTOR2& f) { return _F2(v.x * f.x, v.y * f.y); }
	inline FVECTOR2 operator/ (const FVECTOR2&v, const FVECTOR2& f) { return _F2(v.x / f.x, v.y / f.y); }
	inline FVECTOR2 operator+ (const FVECTOR2&v, const FVECTOR2& f) { return _F2(v.x + f.x, v.y + f.y); }
	inline FVECTOR2 operator- (const FVECTOR2&v, const FVECTOR2& f) { return _F2(v.x - f.x, v.y - f.y); }

	inline FVECTOR2 operator- (const FVECTOR2&v) { return _F2(-v.x, -v.y); }


	// ==============================================================================================================
	// FVECTOR3 operators
	//
	inline FVECTOR3& operator*= (FVECTOR3&v, float f) {	v.x *= f; v.y *= f; v.z *= f; return v; }
	inline FVECTOR3& operator/= (FVECTOR3&v, float f) { v.x /= f; v.y /= f; v.z /= f; return v; }
	inline FVECTOR3& operator+= (FVECTOR3&v, float f) { v.x += f; v.y += f; v.z += f; return v; }
	inline FVECTOR3& operator-= (FVECTOR3&v, float f) { v.x -= f; v.y -= f; v.z -= f; return v; }

	inline FVECTOR3& operator*= (FVECTOR3&v, const FVECTOR3& f)	{ v.x *= f.x; v.y *= f.y; v.z *= f.z; return v;	}
	inline FVECTOR3& operator/= (FVECTOR3&v, const FVECTOR3& f) { v.x /= f.x; v.y /= f.y; v.z /= f.z; return v; }
	inline FVECTOR3& operator+= (FVECTOR3&v, const FVECTOR3& f) { v.x += f.x; v.y += f.y; v.z += f.z; return v; }
	inline FVECTOR3& operator-= (FVECTOR3&v, const FVECTOR3& f) { v.x -= f.x; v.y -= f.y; v.z -= f.z; return v; }

	inline FVECTOR3 operator* (const FVECTOR3&v, float f) { return _F(v.x * f, v.y * f, v.z * f); }
	inline FVECTOR3 operator/ (const FVECTOR3&v, float f) { return _F(v.x / f, v.y / f, v.z / f); }
	inline FVECTOR3 operator+ (const FVECTOR3&v, float f) { return _F(v.x + f, v.y + f, v.z + f); }
	inline FVECTOR3 operator- (const FVECTOR3&v, float f) { return _F(v.x - f, v.y - f, v.z - f); }

	inline FVECTOR3 operator* (const FVECTOR3&v, const FVECTOR3& f) { return _F(v.x * f.x, v.y * f.y, v.z * f.z); }
	inline FVECTOR3 operator/ (const FVECTOR3&v, const FVECTOR3& f) { return _F(v.x / f.x, v.y / f.y, v.z / f.z); }
	inline FVECTOR3 operator+ (const FVECTOR3&v, const FVECTOR3& f) { return _F(v.x + f.x, v.y + f.y, v.z + f.z); }
	inline FVECTOR3 operator- (const FVECTOR3&v, const FVECTOR3& f) { return _F(v.x - f.x, v.y - f.y, v.z - f.z); }

	inline FVECTOR3 operator- (const FVECTOR3&v) { return _F(-v.x, -v.y, -v.z); }



	// ==============================================================================================================
	// FVECTOR4 operators
	//
	inline FVECTOR4& operator*= (FVECTOR4&v, float f) { v.x *= f; v.y *= f; v.z *= f; v.w *= f; return v; }
	inline FVECTOR4& operator/= (FVECTOR4&v, float f) { v.x /= f; v.y /= f; v.z /= f; v.w /= f; return v; }
	inline FVECTOR4& operator+= (FVECTOR4&v, float f) { v.x += f; v.y += f; v.z += f; v.w += f; return v; }
	inline FVECTOR4& operator-= (FVECTOR4&v, float f) { v.x -= f; v.y -= f; v.z -= f; v.w -= f; return v; }

	inline FVECTOR4& operator*= (FVECTOR4&v, const FVECTOR4& f) { v.x *= f.x; v.y *= f.y; v.z *= f.z; v.w *= f.w; return v; }
	inline FVECTOR4& operator/= (FVECTOR4&v, const FVECTOR4& f) { v.x /= f.x; v.y /= f.y; v.z /= f.z; v.w /= f.w; return v; }
	inline FVECTOR4& operator+= (FVECTOR4&v, const FVECTOR4& f) { v.x += f.x; v.y += f.y; v.z += f.z; v.w += f.w; return v; }
	inline FVECTOR4& operator-= (FVECTOR4&v, const FVECTOR4& f) { v.x -= f.x; v.y -= f.y; v.z -= f.z; v.w -= f.w; return v; }

	inline FVECTOR4 operator* (const FVECTOR4&v, float f) { return _F4(v.x * f, v.y * f, v.z * f, v.w * f); }
	inline FVECTOR4 operator/ (const FVECTOR4&v, float f) { return _F4(v.x / f, v.y / f, v.z / f, v.w / f); }
	inline FVECTOR4 operator+ (const FVECTOR4&v, float f) { return _F4(v.x + f, v.y + f, v.z + f, v.w + f); }
	inline FVECTOR4 operator- (const FVECTOR4&v, float f) { return _F4(v.x - f, v.y - f, v.z - f, v.w - f); }

	inline FVECTOR4 operator* (const FVECTOR4&v, const FVECTOR4& f) { return _F4(v.x * f.x, v.y * f.y, v.z * f.z, v.w * f.w); }
	inline FVECTOR4 operator/ (const FVECTOR4&v, const FVECTOR4& f) { return _F4(v.x / f.x, v.y / f.y, v.z / f.z, v.w / f.w); }
	inline FVECTOR4 operator+ (const FVECTOR4&v, const FVECTOR4& f) { return _F4(v.x + f.x, v.y + f.y, v.z + f.z, v.w + f.w); }
	inline FVECTOR4 operator- (const FVECTOR4&v, const FVECTOR4& f) { return _F4(v.x - f.x, v.y - f.y, v.z - f.z, v.w - f.w); }

	inline FVECTOR4 operator- (const FVECTOR4&v) { return _F4(-v.x, -v.y, -v.z, -v.w); }

//namespace oapi
//{
	/**
	* \brief Vector Matrix multiplication
	*/
	inline FVECTOR4 mul(const FVECTOR4& V, const FMATRIX4& M)
	{
		float x = V.x * M.m11 + V.y * M.m21 + V.z * M.m31 + V.w * M.m41;
		float y = V.x * M.m12 + V.y * M.m22 + V.z * M.m32 + V.w * M.m42;
		float z = V.x * M.m13 + V.y * M.m23 + V.z * M.m33 + V.w * M.m43;
		float w = V.x * M.m14 + V.y * M.m24 + V.z * M.m34 + V.w * M.m44;
		return FVECTOR4(x, y, z, w);
	}

	inline FVECTOR4 mul(const FMATRIX4& M, const FVECTOR4& V)
	{
		float x = V.x * M.m11 + V.y * M.m12 + V.z * M.m13 + V.w * M.m14;
		float y = V.x * M.m21 + V.y * M.m22 + V.z * M.m23 + V.w * M.m24;
		float z = V.x * M.m31 + V.y * M.m32 + V.z * M.m33 + V.w * M.m34;
		float w = V.x * M.m41 + V.y * M.m42 + V.z * M.m43 + V.w * M.m44;
		return FVECTOR4(x, y, z, w);
	}

	inline FMATRIX4 transp(const FMATRIX4& M)
	{
		return FMATRIX4(M.m11, M.m21, M.m31, M.m41,
						M.m12, M.m22, M.m32, M.m42,
						M.m13, M.m23, M.m33, M.m43,
						M.m14, M.m24, M.m34, M.m44
		);
	}

	inline FVECTOR3 crossp(const FVECTOR3& a, const FVECTOR3& b)
	{
		return FVECTOR3(a.y * b.z - a.z * b.y, a.z * b.x - a.x * b.z, a.x * b.y - a.y * b.x);
	}

	inline float saturate(float x)
	{
		//sadly std::clamp produces garbage assembly on both gcc and MSVC
		//this version makes MSVC produce good code
		x = (x < 0.0f) ? 0.0f : x;
		x = (x > 1.0f) ? 1.0f : x;
		return x;
	}

	inline FVECTOR2 rcp(const FVECTOR2& v) { return _F2(1.0f / v.x, 1.0f / v.y); }
	inline FVECTOR3 rcp(const FVECTOR3& v) { return  _F(1.0f / v.x, 1.0f / v.y, 1.0f / v.z); }
	inline FVECTOR4 rcp(const FVECTOR4& v) { return _F4(1.0f / v.x, 1.0f / v.y, 1.0f / v.z, 1.0f/ v.w); }

	inline float dotp(const FVECTOR2& v, const FVECTOR2& w) { return v.x * w.x + v.y * w.y; }
	inline float dotp(const FVECTOR3& v, const FVECTOR3& w) { return v.x * w.x + v.y * w.y + v.z * w.z; }
	inline float dotp(const FVECTOR4& v, const FVECTOR4& w)	{ return v.x * w.x + v.y * w.y + v.z * w.z + v.w * w.w;	}

	inline FVECTOR2 abs(const FVECTOR2& v) { return _F2(fabs(v.x), fabs(v.y)); }
	inline FVECTOR3 abs(const FVECTOR3& v) { return  _F(fabs(v.x), fabs(v.y), fabs(v.z)); }
	inline FVECTOR4 abs(const FVECTOR4& v) { return _F4(fabs(v.x), fabs(v.y), fabs(v.z), fabs(v.w)); }

	inline FVECTOR2 saturate(const FVECTOR2& v) { return _F2(saturate(v.x), saturate(v.y)); }
	inline FVECTOR3 saturate(const FVECTOR3& v) { return  _F(saturate(v.x), saturate(v.y), saturate(v.z)); }
	inline FVECTOR4 saturate(const FVECTOR4& v) { return _F4(saturate(v.x), saturate(v.y), saturate(v.z), saturate(v.w)); }

	inline FVECTOR2 pow(const FVECTOR2& v, float p) { return _F2(pow(v.x, p), pow(v.y, p)); }
	inline FVECTOR3 pow(const FVECTOR3& v, float p) { return  _F(pow(v.x, p), pow(v.y, p), pow(v.z, p)); }
	inline FVECTOR4 pow(const FVECTOR4& v, float p) { return _F4(pow(v.x, p), pow(v.y, p), pow(v.z, p), pow(v.w, p)); }

	inline FVECTOR2 pow(const FVECTOR2& v, const FVECTOR2& w) { return _F2(pow(v.x, w.x), pow(v.y, w.y)); }
	inline FVECTOR3 pow(const FVECTOR3& v, const FVECTOR3& w) { return  _F(pow(v.x, w.x), pow(v.y, w.y), pow(v.z, w.z)); }
	inline FVECTOR4 pow(const FVECTOR4& v, const FVECTOR4& w) { return _F4(pow(v.x, w.x), pow(v.y, w.y), pow(v.z, w.z), pow(v.w, w.w)); }

	inline FVECTOR2 sqrt(const FVECTOR2& v) { return _F2(sqrt(v.x), sqrt(v.y)); }
	inline FVECTOR3 sqrt(const FVECTOR3& v) { return  _F(sqrt(v.x), sqrt(v.y), sqrt(v.z)); }
	inline FVECTOR4 sqrt(const FVECTOR4& v) { return _F4(sqrt(v.x), sqrt(v.y), sqrt(v.z), sqrt(v.w)); }

	inline FVECTOR2 exp(const FVECTOR2& v) { return _F2(exp(v.x), exp(v.y)); }
	inline FVECTOR3 exp(const FVECTOR3& v) { return  _F(exp(v.x), exp(v.y), exp(v.z)); }
	inline FVECTOR4 exp(const FVECTOR4& v) { return _F4(exp(v.x), exp(v.y), exp(v.z), exp(v.w)); }

	inline FVECTOR2 min(const FVECTOR2& v, const FVECTOR2& w) { return _F2(std::min(v.x, w.x), std::min(v.y, w.y)); }
	inline FVECTOR3 min(const FVECTOR3& v, const FVECTOR3& w) { return  _F(std::min(v.x, w.x), std::min(v.y, w.y), std::min(v.z, w.z)); }
	inline FVECTOR4 min(const FVECTOR4& v, const FVECTOR4& w) { return _F4(std::min(v.x, w.x), std::min(v.y, w.y), std::min(v.z, w.z), std::min(v.w, w.w)); }

	inline FVECTOR2 max(const FVECTOR2& v, const FVECTOR2& w) { return _F2(std::max(v.x, w.x), std::max(v.y, w.y)); }
	inline FVECTOR3 max(const FVECTOR3& v, const FVECTOR3& w) { return  _F(std::max(v.x, w.x), std::max(v.y, w.y), std::max(v.z, w.z)); }
	inline FVECTOR4 max(const FVECTOR4& v, const FVECTOR4& w) { return _F4(std::max(v.x, w.x), std::max(v.y, w.y), std::max(v.z, w.z), std::max(v.w, w.w)); }

	template <typename T> inline constexpr float ilen(const T& v) { return 1.0f / sqrt(dotp(v, v)); }
	template <typename T> inline constexpr T unit(const T& v) { return v * ilen(v); }
	template <typename T> inline constexpr void normalize(T& v) { v *= ilen(v); }
	template <typename T> inline constexpr void normalise(T& v) { v *= ilen(v); }
	template <typename T> inline constexpr float length(const T& v) { return sqrt(dotp(v,v)); }
	template <typename T> inline constexpr T lerp(const T& a, const T& b, float x) { return a + (b - a) * x; }
	template <typename T> inline constexpr T lerp(const T& a, const T& b, double x) { return a + (b - a) * x; }
	template <typename T> inline constexpr T sqr(T a) {	return a * a; }
	template <typename T> inline constexpr T hermite(T a) {	return a * a * (T(3) - T(2) * a); }
	template <typename T> inline constexpr T herp(T a, T b, float x) { return lerp(a, b, (float)hermite(x)); }
	template <typename T> inline constexpr T herp(T a, T b, double x) { return lerp(a, b, (double)hermite(x)); }

//} //namespace


static const FMATRIX4 FMATRIX_Identity = {
	1.0f, 0.0f, 0.0f, 0.0f,
	0.0f, 1.0f, 0.0f, 0.0f,
	0.0f, 0.0f, 1.0f, 0.0f,
	0.0f, 0.0f, 0.0f, 1.0f
};

static const oapi::FVECTOR4 F4_Zero = { 0.0f, 0.0f, 0.0f, 0.0f };
static const oapi::FVECTOR4 F4_One = { 1.0f, 1.0f, 1.0f, 1.0f };

#endif
