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

template <typename T> inline _constexpr_ T clamp(T x, T a, T b)
{
	return x > b ? b
		 : x < a ? a
		 : x;
}

template <typename T> inline _constexpr_ T ilerp(T a, T b, T x)
{
	return saturate((x - a) / (b - a));
}

template <typename T> inline _constexpr_ T sqr(T a)
{
	return a * a;
}

template <typename T> inline _constexpr_ T hermite(T a)
{
	return a * a * (T(3) - T(2)*a);
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
	return _V(std::max(v.x, w.x), std::max(v.y, w.y), std::max(v.z, w.z));
}

inline VECTOR3 vmin(const VECTOR3 &v, const VECTOR3 &w)
{
	return _V(std::min(v.x, w.x), std::min(v.y, w.y), std::min(v.z, w.z));
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
	return _V(std::max(v.x, w.x), std::max(v.y, w.y), std::max(v.z, w.z), std::max(v.w, w.w));
}

inline VECTOR4 vmin(const VECTOR4 &v, const VECTOR4 &w)
{
	return _V(std::min(v.x, w.x), std::min(v.y, w.y), std::min(v.z, w.z), std::min(v.w, w.w));
}


// FVECTOR3 Helpers ==================================================================
//
//

inline FVECTOR3 exp2(const FVECTOR3 &v)
{
	return FVECTOR3(exp2(v.x), exp2(v.y), exp2(v.z));
}

inline FVECTOR3 _FVECTOR3(const VECTOR3 &v)
{
	return FVECTOR3(float(v.x), float(v.y), float(v.z));
}

inline FVECTOR3 _FVECTOR3(double x, double y, double z)
{
	return FVECTOR3(float(x), float(y), float(z));
}

inline FVECTOR3 rcp(const FVECTOR3 &v)
{
	return FVECTOR3(1.0f/v.x, 1.0f/v.y, 1.0f/v.z);
}

inline FVECTOR3 vmax(const FVECTOR3 &v, const FVECTOR3 &w)
{
	return FVECTOR3(std::max(v.x, w.x), std::max(v.y, w.y), std::max(v.z, w.z));
}

inline FVECTOR3 vmin(const FVECTOR3 &v, const FVECTOR3 &w)
{
	return FVECTOR3(std::min(v.x, w.x), std::min(v.y, w.y), std::min(v.z, w.z));
}

inline FVECTOR3 lerp(const FVECTOR3 &v, const FVECTOR3 &w, float x)
{
	return FVECTOR3(v.x+(w.x-v.x)*x, v.y+(w.y-v.y)*x, v.z+(w.z-v.z)*x);
}



// FVECTOR4 Helpers ==================================================================
//
//
inline FVECTOR4 abs(FVECTOR4 &a)
{
	return FVECTOR4(abs(a.x), abs(a.y), abs(a.z), abs(a.w));
}

inline FVECTOR4 sign(FVECTOR4 &a)
{
	return FVECTOR4(sign(a.x), sign(a.y), sign(a.z), sign(a.w));
}

inline FVECTOR4 pow(float x, FVECTOR4 &y)
{
	return FVECTOR4(pow(x, y.x), pow(x, y.y), pow(x, y.z), pow(x, y.w));
}

#endif

