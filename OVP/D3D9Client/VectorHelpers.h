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


#include "OrbiterAPI.h"
#include <math.h>


#ifndef __VECTORHELPERS_H
#define __VECTORHELPERS_H

#if defined(_MSC_VER) && (_MSC_VER >= 1900 ) // Microsoft Visual Studio Version 2015 and higher
  // exp2() and log2() are defined in <cmath>
#define  _constexpr_ constexpr

#else // older MSVC++ versions

inline double exp2(double d)
{
	return exp(d*0.69314718055994530941723212145818);
}

inline float exp2(float d)
{ 
	return exp(d*0.693147180559945f);
}

inline double log2(double d)
{ 
	return log(d*1.4426950408889634073599246810019);
}
inline float log2(float d)
{
	return log(d*1.442695040888963f);
}

inline double log1p(double d)
{ 
  return log(d+1.0);
}
// inline float log1p(float d)
// { 
// 	return log(d+1.0f);
// }
#define  _constexpr_
#endif

template <typename T> inline _constexpr_ T sign (T val)
{
	return val < T(0) ? T(-1) : T(1);
}

template <typename T> inline _constexpr_ T lerp (T a, T b, T x)
{
	return a + (b - a)*x;
}
// ...slightly faster than the above, but not available in Visual Studio 2012 (I think)?
//template <typename T> inline _constexpr_ T lerp(T v0, T v1, T t) {
//	return fma(t, v1, fma(-t, v0, v0));
//}

template <typename T> inline _constexpr_ T saturate (T val)
{
	return (val > T(1)) ? T(1)
		 : (val < T(0)) ? T(0)
		 : val;
}


	
// VECTOR3 Helpers ==================================================================
//
inline VECTOR3 &operator+= (VECTOR3 &v, double d)
{
	v.x+=d; v.y+=d;	v.z+=d;
	return v;
} 

inline VECTOR3 &operator-= (VECTOR3 &v, double d)
{
	v.x-=d; v.y-=d;	v.z-=d;
	return v;
} 

inline VECTOR3 operator+ (const VECTOR3 &v, double d)
{
	return _V(v.x+d, v.y+d, v.z+d);
}

inline VECTOR3 operator- (const VECTOR3 &v, double d)
{
	return _V(v.x-d, v.y-d, v.z-d);
}

inline VECTOR3 operator- (double d, const VECTOR3 &v)
{
	return _V(d-v.x, d-v.y, d-v.z);
}

inline VECTOR3 exp2(const VECTOR3 &v)
{
	return _V(exp2(v.x), exp2(v.y), exp2(v.z));
}

inline VECTOR3 rcp(const VECTOR3 &v)
{
	return _V(1.0/v.x, 1.0/v.y, 1.0/v.z);
}

inline VECTOR3 vmax(const VECTOR3 &v, const VECTOR3 &w)
{
	return _V(max(v.x, w.x), max(v.y, w.y), max(v.z, w.z));
}

inline VECTOR3 vmin(const VECTOR3 &v, const VECTOR3 &w)
{
	return _V(min(v.x, w.x), min(v.y, w.y), min(v.z, w.z));
}


// VECTOR4 Helpers ==================================================================
//
//
inline VECTOR4 &operator+= (VECTOR4 &v, double d)
{
	v.x+=d; v.y+=d;	v.z+=d; v.w+=d;
	return v;
} 

inline VECTOR4 &operator-= (VECTOR4 &v, double d)
{
	v.x-=d; v.y-=d;	v.z-=d; v.w-=d;
	return v;
} 

inline VECTOR4 operator+ (const VECTOR4 &v, double d)
{
	return _V(v.x+d, v.y+d, v.z+d, v.w+d);
}

inline VECTOR4 operator- (const VECTOR4 &v, double d)
{
	return _V(v.x-d, v.y-d, v.z-d, v.w-d);
}

inline VECTOR4 operator- (double d, const VECTOR4 &v)
{
	return _V(d-v.x, d-v.y, d-v.z, d-v.w);
}

inline VECTOR4 &operator*= (VECTOR4 &v, double d)
{
	v.x*=d; v.y*=d;	v.z*=d; v.w*=d;
	return v;
} 

inline VECTOR4 &operator/= (VECTOR4 &v, double d)
{
	d=1.0/d; v.x*=d; v.y*=d; v.z*=d; v.w*=d;
	return v;
} 

inline VECTOR4 operator* (const VECTOR4 &v, double d)
{
	return _V(v.x*d, v.y*d, v.z*d, v.w*d);
}

inline VECTOR4 operator/ (const VECTOR4 &v, double d)
{
	d=1.0/d; return _V(v.x*d, v.y*d, v.z*d, v.w*d);
}

inline double dotp(const VECTOR4 &v, const VECTOR4 &w)
{
	return v.x*w.x + v.y*w.y + v.z*w.z + v.w*w.w;
}

inline double length(const VECTOR4 &v)
{
	return sqrt(dotp(v,v));
}

inline VECTOR4 normalize(const VECTOR4 &v)
{
	return v*(1.0/sqrt(dotp(v,v)));
}

inline VECTOR4 exp2(const VECTOR4 &v)
{
	return _V(exp2(v.x), exp2(v.y), exp2(v.z), exp2(v.w));
}

inline VECTOR4 rcp(const VECTOR4 &v)
{
	return _V(1.0/v.x, 1.0/v.y, 1.0/v.z, 1.0/v.w);
}

inline VECTOR4 vmax(const VECTOR4 &v, const VECTOR4 &w)
{
	return _V(max(v.x, w.x), max(v.y, w.y), max(v.z, w.z), max(v.w, w.w));
}

inline VECTOR4 vmin(const VECTOR4 &v, const VECTOR4 &w)
{
	return _V(min(v.x, w.x), min(v.y, w.y), min(v.z, w.z), min(v.w, w.w));
}


// D3DXVECTOR3 Helpers ==================================================================
//
//

inline D3DXVECTOR3 exp2(const D3DXVECTOR3 &v)
{
	return D3DXVECTOR3(exp2(v.x), exp2(v.y), exp2(v.z));
}

inline D3DXVECTOR3 _D3DXVECTOR3(const VECTOR3 &v)
{
	return D3DXVECTOR3(float(v.x), float(v.y), float(v.z));
}

inline D3DXVECTOR3 _D3DXVECTOR3(double x, double y, double z)
{
	return D3DXVECTOR3(float(x), float(y), float(z));
}

inline D3DXVECTOR3 operator* (const D3DXVECTOR3 &a, const D3DXVECTOR3 &b)
{
	return D3DXVECTOR3(a.x*b.x, a.y*b.y, a.z*b.z);
}

inline D3DXVECTOR3 operator+ (const D3DXVECTOR3 &a, float d)
{
	return D3DXVECTOR3(a.x+d, a.y+d, a.z+d);
}

inline D3DXVECTOR3 &operator*= (D3DXVECTOR3 &a, const D3DXVECTOR3 &b)
{
	a.x *= b.x; a.y *= b.y; a.z *= b.z; 
	return a;
}

inline D3DXVECTOR3 &operator+= (D3DXVECTOR3 &a, float d)
{
	a.x += d; a.y += d; a.z += d; 
	return a;
}

inline D3DXVECTOR3 rcp(const D3DXVECTOR3 &v)
{
	return D3DXVECTOR3(1.0f/v.x, 1.0f/v.y, 1.0f/v.z);
}

inline D3DXVECTOR3 vmax(const D3DXVECTOR3 &v, const D3DXVECTOR3 &w)
{
	return D3DXVECTOR3(max(v.x, w.x), max(v.y, w.y), max(v.z, w.z));
}

inline D3DXVECTOR3 vmin(const D3DXVECTOR3 &v, const D3DXVECTOR3 &w)
{
	return D3DXVECTOR3(min(v.x, w.x), min(v.y, w.y), min(v.z, w.z));
}

inline D3DXVECTOR3 lerp(const D3DXVECTOR3 &v, const D3DXVECTOR3 &w, float x)
{
	return D3DXVECTOR3(v.x+(w.x-v.x)*x, v.y+(w.y-v.y)*x, v.z+(w.z-v.z)*x);
}



// D3DXVECTOR4 Helpers ==================================================================
//
//
inline D3DXVECTOR4 abs(D3DXVECTOR4 &a)
{
	return D3DXVECTOR4(abs(a.x), abs(a.y), abs(a.z), abs(a.w));
}

inline D3DXVECTOR4 sign(D3DXVECTOR4 &a)
{
	return D3DXVECTOR4(sign(a.x), sign(a.y), sign(a.z), sign(a.w));
}

inline D3DXVECTOR4 pow(float x, D3DXVECTOR4 &y)
{
	return D3DXVECTOR4(pow(x, y.x), pow(x, y.y), pow(x, y.z), pow(x, y.w));
}

inline D3DXVECTOR4 operator- (const D3DXVECTOR4 &v, float d)
{
	return D3DXVECTOR4(v.x-d, v.y-d, v.z-d, v.w-d);
}

inline D3DXVECTOR4 operator+ (const D3DXVECTOR4 &v, float d)
{
	return D3DXVECTOR4(v.x+d, v.y+d, v.z+d, v.w+d);
}

inline D3DXVECTOR4 &operator*= (D3DXVECTOR4 &v, const D3DXVECTOR4 &d)
{
	v.x*=d.x; v.y*=d.y;	v.z*=d.z; v.w*=d.w;
	return v;
}

#endif

