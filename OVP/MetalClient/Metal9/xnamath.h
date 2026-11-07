#pragma once
#include <simd/simd.h>
#include <stdlib.h>

typedef simd_float4 XMVECTOR;
typedef simd_float4 __m128;
struct XMFLOAT3 { float x, y, z; XMFLOAT3() {} XMFLOAT3(float a, float b, float c) : x(a), y(b), z(c) {} };
struct XMFLOAT4 { float x, y, z, w; XMFLOAT4() {} XMFLOAT4(float a, float b, float c, float d) : x(a), y(b), z(c), w(d) {} };

inline XMVECTOR XMVectorSet(float x, float y, float z, float w) { return simd_make_float4(x, y, z, w); }
inline XMVECTOR XMLoadFloat3(const XMFLOAT3 *p) { return simd_make_float4(p->x, p->y, p->z, 0.0f); }
inline XMVECTOR XMLoadFloat4(const XMFLOAT4 *p) { return simd_make_float4(p->x, p->y, p->z, p->w); }
inline void XMStoreFloat3(XMFLOAT3 *p, XMVECTOR v) { p->x = v.x; p->y = v.y; p->z = v.z; }
inline void XMStoreFloat4(XMFLOAT4 *p, XMVECTOR v) { p->x = v.x; p->y = v.y; p->z = v.z; p->w = v.w; }
inline XMVECTOR XMVectorMin(XMVECTOR a, XMVECTOR b) { return simd_min(a, b); }
inline XMVECTOR XMVectorMax(XMVECTOR a, XMVECTOR b) { return simd_max(a, b); }
inline XMVECTOR XMVector3Dot(XMVECTOR a, XMVECTOR b) { return simd_make_float4(simd_dot(a.xyz, b.xyz)); }
inline XMVECTOR XMVector3Normalize(XMVECTOR v)
{
	float l = simd_length(v.xyz);
	return l > 0.0f ? simd_make_float4(v.xyz / l, v.w / l) : simd_make_float4(0.0f);
}
inline XMVECTOR XMVectorSetW(XMVECTOR v, float w) { v.w = w; return v; }
