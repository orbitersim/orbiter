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

		FVECTOR2(double _x, double _y)
		{
			x = float(_x);
			y = float(_y);
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

		inline FVECTOR2 operator* (float f) const
		{
			return FVECTOR2(x * f, y * f);
		}

		inline FVECTOR2 operator* (FVECTOR2 f) const
		{
			return FVECTOR2(x * f.x, y * f.y);
		}

		inline FVECTOR2& operator*= (FVECTOR2& f)
		{
			x *= f.x; y *= f.y;
			return *this;
		}

		inline FVECTOR2& operator+= (FVECTOR2& f)
		{
			x += f.x; y += f.y;
			return *this;
		}

		inline FVECTOR2& operator-= (FVECTOR2& f)
		{
			x -= f.x; y -= f.y;
			return *this;
		}

		inline FVECTOR2& operator/= (FVECTOR2& f)
		{
			x /= f.x; y /= f.y;
			return *this;
		}

		inline FVECTOR2 operator/ (float f) const
		{
			f = 1.0f / f;
			return FVECTOR2(x * f, y * f);
		}

		inline FVECTOR2 operator+ (float f) const
		{
			return FVECTOR2(x + f, y + f);
		}

		inline FVECTOR2 operator+ (FVECTOR2& f) const
		{
			return FVECTOR2(x + f.x, y + f.y);
		}

		inline FVECTOR2 operator- (float f) const
		{
			return FVECTOR2(x - f, y - f);
		}

		inline FVECTOR2 operator+ (const FVECTOR2& f) const
		{
			return FVECTOR2(x + f.x, y + f.y);
		}

		inline FVECTOR2 operator- (const FVECTOR2& f) const
		{
			return FVECTOR2(x - f.x, y - f.y);
		}

		inline FVECTOR2& operator*= (float f)
		{
			x *= f; y *= f;
			return *this;
		}

		inline FVECTOR2& operator/= (float f)
		{
			x /= f; y /= f;
			return *this;
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

		float MaxRGB() const
		{
			return (std::max)(r, (std::max)(g, b));
		}

		float MinRGB() const
		{
			return (std::min)(r, (std::min)(g, b));
		}

		inline VECTOR3 _V() const
		{
			VECTOR3 v = { x,y,z };
			return v;
		}

		inline float& operator[](int i)
		{
			return data[i];
		}

		inline FVECTOR3& operator*= (float f)
		{
			x *= f; y *= f; z *= f;
			return *this;
		}

		inline FVECTOR3& operator*= (const FVECTOR3& f)
		{
			x *= f.x; y *= f.y; z *= f.z;
			return *this;
		}

		inline FVECTOR3& operator/= (float f)
		{
			// return *this *= (1.0f / f); // nicer?
			f = 1.0f / f;
			x *= f; y *= f; z *= f;
			return *this;
		}

		inline FVECTOR3& operator+= (float f)
		{
			x += f; y += f; z += f;
			return *this;
		}

		inline FVECTOR3& operator+= (const FVECTOR3& f)
		{
			x += f.x; y += f.y; z += f.z;
			return *this;
		}

		inline FVECTOR3& operator-= (float f)
		{
			x -= f; y -= f; z -= f;
			return *this;
		}

		inline FVECTOR3& operator-= (const FVECTOR3& f)
		{
			x -= f.x; y -= f.y; z -= f.z;
			return *this;
		}

		inline FVECTOR3 operator* (float f) const
		{
			return FVECTOR3(x * f, y * f, z * f);
		}

		inline FVECTOR3 operator* (const FVECTOR3& f) const
		{
			return FVECTOR3(x * f.x, y * f.y, z * f.z);
		}

		inline FVECTOR3 operator/ (float f) const
		{
			f = 1.0f / f;
			return FVECTOR3(x * f, y * f, z * f);
		}

		inline FVECTOR3 operator/ (const FVECTOR3& f) const
		{
			return FVECTOR3(x / f.x, y / f.y, z / f.z);
		}

		inline FVECTOR3 operator+ (float f) const
		{
			return FVECTOR3(x + f, y + f, z + f);
		}

		inline FVECTOR3 operator- (float f) const
		{
			return FVECTOR3(x - f, y - f, z - f);
		}

		inline FVECTOR3 operator+ (const FVECTOR3& f) const
		{
			return FVECTOR3(x + f.x, y + f.y, z + f.z);
		}

		inline FVECTOR3 operator- (const FVECTOR3& f) const
		{
			return FVECTOR3(x - f.x, y - f.y, z - f.z);
		}

		inline FVECTOR3 operator-() const
		{
			return FVECTOR3(-x, -y, -z);
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

		float MaxRGB() const
		{
			return (std::max)(r, (std::max)(g, b));
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

		inline FVECTOR4 operator* (float f) const
		{
			return FVECTOR4(x * f, y * f, z * f, w * f);
		}

		inline FVECTOR4& operator*= (float f)
		{
			x *= f; y *= f; z *= f; w *= f;
			return *this;
		}

		inline FVECTOR4& operator*= (const FVECTOR4& f)
		{
			x *= f.x; y *= f.y; z *= f.z; w *= f.w;
			return *this;
		}

		inline FVECTOR4& operator/= (float f)
		{
			// return *this *= (1.0f / f); // nicer?
			f = 1.0f / f;
			x *= f; y *= f; z *= f; w *= f;
			return *this;
		}

		inline FVECTOR4& operator+= (float f)
		{
			x += f; y += f; z += f; w += f;
			return *this;
		}

		inline FVECTOR4& operator+= (const FVECTOR4& f)
		{
			x += f.x; y += f.y; z += f.z; w += f.w;
			return *this;
		}

		inline FVECTOR4& operator-= (float f)
		{
			x -= f; y -= f; z -= f; w -= f;
			return *this;
		}

		inline FVECTOR4& operator-= (const FVECTOR4& f)
		{
			x -= f.x; y -= f.y; z -= f.z; w -= f.w;
			return *this;
		}

		inline FVECTOR4 operator/ (float f) const
		{
			f = 1.0f / f;
			return FVECTOR4(x * f, y * f, z * f, w * f);
		}

		inline FVECTOR4 operator+ (float f) const
		{
			return FVECTOR4(x + f, y + f, z + f, w + f);
		}

		inline FVECTOR4 operator- (float f) const
		{
			return FVECTOR4(x - f, y - f, z - f, w - f);
		}

		inline FVECTOR4 operator+ (const FVECTOR4& f) const
		{
			return FVECTOR4(x + f.x, y + f.y, z + f.z, w + f.w);
		}

		inline FVECTOR4 operator- (const FVECTOR4& f) const
		{
			return FVECTOR4(x - f.x, y - f.y, z - f.z, w - f.w);
		}

		inline FVECTOR4 operator-() const
		{
			return FVECTOR4(-x, -y, -z, -w);
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
			for (int i = 0; i < 16; i++) data[i] = 0.0;
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


	inline FVECTOR4 tmul(const FMATRIX4& M, const FVECTOR4& V)
	{
		float x = V.x * M.m11 + V.y * M.m12 + V.z * M.m13 + V.w * M.m14;
		float y = V.x * M.m21 + V.y * M.m22 + V.z * M.m23 + V.w * M.m24;
		float z = V.x * M.m31 + V.y * M.m32 + V.z * M.m33 + V.w * M.m34;
		float w = V.x * M.m41 + V.y * M.m42 + V.z * M.m43 + V.w * M.m44;
		return FVECTOR4(x, y, z, w);
	}

	inline FVECTOR2 unit(const FVECTOR2& v)
	{
		float f = 1.0f / ::sqrt(v.x * v.x + v.y * v.y);
		return FVECTOR2(v.x * f, v.y * f);
	}

	inline FVECTOR3 unit(const FVECTOR3& v)
	{
		float d = v.x * v.x + v.y * v.y + v.z * v.z;
		return d > 0 ? FVECTOR3(v.x, v.y, v.z) / ::sqrt(d) : 0.0f;
	}

	inline FVECTOR3 normalize(const FVECTOR3& v)
	{
		float d = v.x * v.x + v.y * v.y + v.z * v.z;
		return d > 0 ? FVECTOR3(v.x, v.y, v.z) / ::sqrt(d) : 0.0f;
	}

	inline float dotp(const FVECTOR2& v, const FVECTOR2& w)
	{
		return v.x * w.x + v.y * w.y;
	}

	inline float dotp(const FVECTOR3& v, const FVECTOR3& w)
	{
		return v.x * w.x + v.y * w.y + v.z * w.z;
	}

	inline float dotp(const FVECTOR4& v, const FVECTOR4& w)
	{
		return v.x * w.x + v.y * w.y + v.z * w.z + v.w * w.w;
	}

	inline float length(const FVECTOR2& v)
	{
		return ::sqrt(v.x * v.x + v.y * v.y);
	}

	inline float length(const FVECTOR3& v)
	{
		return ::sqrt(v.x * v.x + v.y * v.y + v.z * v.z);
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

	inline FVECTOR3 saturate(const FVECTOR3& v)
	{
		return FVECTOR3(saturate(v.x), saturate(v.y), saturate(v.z));
	}

	inline FVECTOR4 saturate(const FVECTOR4& v)
	{
		return FVECTOR4(saturate(v.x), saturate(v.y), saturate(v.z), saturate(v.w));
	}

	inline FVECTOR2 lerp(const FVECTOR2& a, const FVECTOR2& b, float x)
	{
		return a + (b - a) * x;
	}

	inline FVECTOR3 lerp(const FVECTOR3& a, const FVECTOR3& b, float x)
	{
		return a + (b - a) * x;
	}

	inline FVECTOR4 lerp(const FVECTOR4& a, const FVECTOR4& b, float x)
	{
		return a + (b - a) * x;
	}

	inline FVECTOR3 pow(const FVECTOR3& x, float y)
	{
		return FVECTOR3(::pow(x.x, y), ::pow(x.y, y), ::pow(x.z, y));
	}

	inline FVECTOR4 pow(const FVECTOR4& x, float y)
	{
		return FVECTOR4(::pow(x.x, y), ::pow(x.y, y), ::pow(x.z, y), ::pow(x.w, y));
	}

	inline FVECTOR3 pow(const FVECTOR3& x, const FVECTOR3& y)
	{
		return FVECTOR3(::pow(x.x, y.x), ::pow(x.y, y.y), ::pow(x.z, y.z));
	}

	inline FVECTOR4 pow(const FVECTOR4& x, const FVECTOR4& y)
	{
		return FVECTOR4(::pow(x.x, y.x), ::pow(x.y, y.y), ::pow(x.z, y.z), ::pow(x.w, y.w));
	}

	inline FVECTOR3 exp(const FVECTOR3& x)
	{
		return FVECTOR3(::exp(x.x), ::exp(x.y), ::exp(x.z));
	}

	inline FVECTOR4 exp(const FVECTOR4& x)
	{
		return FVECTOR4(::exp(x.x), ::exp(x.y), ::exp(x.z), ::exp(x.w));
	}

	inline FVECTOR3 sqrt(const FVECTOR3& x)
	{
		return FVECTOR3(::sqrt(x.x), ::sqrt(x.y), ::sqrt(x.z));
	}

	inline FVECTOR4 sqrt(const FVECTOR4& x)
	{
		return FVECTOR4(::sqrt(x.x), ::sqrt(x.y), ::sqrt(x.z), ::sqrt(x.w));
	}

	inline FVECTOR3 normalise(const FVECTOR3& v) { return normalize(v); }

} //namespace


static oapi::FMATRIX4 FMATRIX_Identity = {
	1.0f, 0.0f, 0.0f, 0.0f,
	0.0f, 1.0f, 0.0f, 0.0f,
	0.0f, 0.0f, 1.0f, 0.0f,
	0.0f, 0.0f, 0.0f, 1.0f
};

static oapi::FVECTOR4 F4_Zero = { 0.0f, 0.0f, 0.0f, 0.0f };
static oapi::FVECTOR4 F4_One = { 1.0f, 1.0f, 1.0f, 1.0f };


typedef oapi::FMATRIX4* LPFMATRIX4;
typedef oapi::FVECTOR4* LPFVECTOR4;
typedef oapi::FVECTOR3* LPFVECTOR3;
typedef oapi::FVECTOR2* LPFVECTOR2;

#endif
