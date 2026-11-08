#include "M9Internal.h"
#include "M9Font.h"
#import <CoreText/CoreText.h>
#import <CoreGraphics/CoreGraphics.h>
#import <ImageIO/ImageIO.h>
#import <UniformTypeIdentifiers/UniformTypeIdentifiers.h>
#include <fstream>
#include <algorithm>
#include <cmath>
#include <list>

using namespace m9;

// ---------------------------------------------------------------------------
// Geometry helpers

bool D3DXIntersectTri(const D3DXVECTOR3 *p0, const D3DXVECTOR3 *p1, const D3DXVECTOR3 *p2,
	const D3DXVECTOR3 *rayPos, const D3DXVECTOR3 *rayDir, float *u, float *v, float *dist)
{
	D3DXVECTOR3 e1 = *p1 - *p0, e2 = *p2 - *p0, p, q, t;
	D3DXVec3Cross(&p, rayDir, &e2);
	float det = D3DXVec3Dot(&e1, &p);
	if (fabsf(det) < 1e-12f) return false;
	float inv = 1.0f / det;
	t = *rayPos - *p0;
	float uu = D3DXVec3Dot(&t, &p) * inv;
	if (uu < 0.0f || uu > 1.0f) return false;
	D3DXVec3Cross(&q, &t, &e1);
	float vv = D3DXVec3Dot(rayDir, &q) * inv;
	if (vv < 0.0f || uu + vv > 1.0f) return false;
	float d = D3DXVec3Dot(&e2, &q) * inv;
	if (d < 0.0f) return false;
	if (u) *u = uu;
	if (v) *v = vv;
	if (dist) *dist = d;
	return true;
}

HRESULT D3DXComputeBoundingSphere(const D3DXVECTOR3 *first, DWORD n, DWORD stride, D3DXVECTOR3 *center, float *radius)
{
	if (!first || !center || !radius || !n) return D3DERR_INVALIDCALL;
	const uint8_t *b = (const uint8_t *)first;
	D3DXVECTOR3 c(0, 0, 0);
	for (DWORD i = 0; i < n; i++) c += *(const D3DXVECTOR3 *)(b + (size_t)i * stride);
	c /= (float)n;
	float r2 = 0;
	for (DWORD i = 0; i < n; i++) {
		D3DXVECTOR3 d = *(const D3DXVECTOR3 *)(b + (size_t)i * stride) - c;
		r2 = std::max(r2, D3DXVec3LengthSq(&d));
	}
	*center = c;
	*radius = sqrtf(r2);
	return S_OK;
}

// ---------------------------------------------------------------------------
// Pixel formats as stored in files and in app memory

namespace {

struct PixFmt {
	UINT bytes;      // per pixel, or per block for compressed
	bool compressed;
};

bool PixInfo(D3DFORMAT f, PixFmt &pi)
{
	switch (f) {
	case D3DFMT_DXT1: pi = {8, true}; return true;
	case D3DFMT_DXT2: case D3DFMT_DXT3: case D3DFMT_DXT4: case D3DFMT_DXT5: pi = {16, true}; return true;
	case D3DFMT_A8R8G8B8: case D3DFMT_X8R8G8B8: case D3DFMT_A8B8G8R8: case D3DFMT_X8B8G8R8:
	case D3DFMT_A2R10G10B10: case D3DFMT_A2B10G10R10: case D3DFMT_G16R16: case D3DFMT_G16R16F:
	case D3DFMT_R32F: case D3DFMT_D32: case D3DFMT_D24S8: case D3DFMT_D24X8: case D3DFMT_D32F_LOCKABLE:
		pi = {4, false}; return true;
	case D3DFMT_R8G8B8: pi = {3, false}; return true;
	case D3DFMT_R5G6B5: case D3DFMT_X1R5G5B5: case D3DFMT_A1R5G5B5: case D3DFMT_A4R4G4B4: case D3DFMT_X4R4G4B4:
	case D3DFMT_A8L8: case D3DFMT_L16: case D3DFMT_R16F: case D3DFMT_D16: case D3DFMT_V8U8:
		pi = {2, false}; return true;
	case D3DFMT_L8: case D3DFMT_A8: case D3DFMT_A4L4: case D3DFMT_R3G3B2: pi = {1, false}; return true;
	case D3DFMT_A16B16G16R16: case D3DFMT_A16B16G16R16F: case D3DFMT_G32R32F: pi = {8, false}; return true;
	case D3DFMT_A32B32G32R32F: pi = {16, false}; return true;
	default: return false;
	}
}

UINT Pitch(D3DFORMAT f, UINT w)
{
	PixFmt pi;
	if (!PixInfo(f, pi)) return w * 4;
	return pi.compressed ? std::max(1u, (w + 3) / 4) * pi.bytes : w * pi.bytes;
}

UINT Rows(D3DFORMAT f, UINT h)
{
	PixFmt pi;
	if (PixInfo(f, pi) && pi.compressed) return std::max(1u, (h + 3) / 4);
	return h;
}

bool IsCompressed(D3DFORMAT f)
{
	PixFmt pi;
	return PixInfo(f, pi) && pi.compressed;
}

struct RGBA { float r, g, b, a; };

inline float U(uint32_t v, int bits) { return (float)v / (float)((1u << bits) - 1); }
inline uint32_t Q(float v, int bits)
{
	float m = (float)((1u << bits) - 1);
	v = std::min(1.0f, std::max(0.0f, v));
	return (uint32_t)lrintf(v * m);
}

uint16_t FloatToHalf(float f)
{
	uint32_t x;
	memcpy(&x, &f, 4);
	uint32_t s = (x >> 16) & 0x8000;
	int e = (int)((x >> 23) & 0xff) - 127 + 15;
	uint32_t m = x & 0x7fffff;
	if (e <= 0) {
		if (e < -10) return (uint16_t)s;
		m |= 0x800000;
		uint32_t t = 14 - e;
		return (uint16_t)(s | ((m + (1u << (t - 1))) >> t));
	}
	if (e >= 31) return (uint16_t)(s | 0x7c00 | (((x >> 23) & 0xff) == 0xff && m ? 0x200 : 0));
	uint32_t r = s | ((uint32_t)e << 10) | (m >> 13);
	if (m & 0x1000) r++;
	return (uint16_t)r;
}

bool UnpackPixel(D3DFORMAT f, const uint8_t *p, RGBA &c)
{
	uint32_t v;
	switch (f) {
	case D3DFMT_A8R8G8B8: case D3DFMT_X8R8G8B8:
		memcpy(&v, p, 4);
		c = {U((v >> 16) & 255, 8), U((v >> 8) & 255, 8), U(v & 255, 8), f == D3DFMT_X8R8G8B8 ? 1.0f : U(v >> 24, 8)};
		return true;
	case D3DFMT_A8B8G8R8: case D3DFMT_X8B8G8R8:
		memcpy(&v, p, 4);
		c = {U(v & 255, 8), U((v >> 8) & 255, 8), U((v >> 16) & 255, 8), f == D3DFMT_X8B8G8R8 ? 1.0f : U(v >> 24, 8)};
		return true;
	case D3DFMT_R8G8B8:
		c = {U(p[2], 8), U(p[1], 8), U(p[0], 8), 1.0f};
		return true;
	case D3DFMT_R5G6B5:
		v = p[0] | (p[1] << 8);
		c = {U(v >> 11, 5), U((v >> 5) & 63, 6), U(v & 31, 5), 1.0f};
		return true;
	case D3DFMT_X1R5G5B5: case D3DFMT_A1R5G5B5:
		v = p[0] | (p[1] << 8);
		c = {U((v >> 10) & 31, 5), U((v >> 5) & 31, 5), U(v & 31, 5), f == D3DFMT_X1R5G5B5 ? 1.0f : (float)(v >> 15)};
		return true;
	case D3DFMT_A4R4G4B4: case D3DFMT_X4R4G4B4:
		v = p[0] | (p[1] << 8);
		c = {U((v >> 8) & 15, 4), U((v >> 4) & 15, 4), U(v & 15, 4), f == D3DFMT_X4R4G4B4 ? 1.0f : U(v >> 12, 4)};
		return true;
	case D3DFMT_A2R10G10B10:
		memcpy(&v, p, 4);
		c = {U((v >> 20) & 1023, 10), U((v >> 10) & 1023, 10), U(v & 1023, 10), U(v >> 30, 2)};
		return true;
	case D3DFMT_A2B10G10R10:
		memcpy(&v, p, 4);
		c = {U(v & 1023, 10), U((v >> 10) & 1023, 10), U((v >> 20) & 1023, 10), U(v >> 30, 2)};
		return true;
	case D3DFMT_G16R16:
		memcpy(&v, p, 4);
		c = {U(v & 0xffff, 16), U(v >> 16, 16), 1.0f, 1.0f};
		return true;
	case D3DFMT_A16B16G16R16: {
		uint16_t s[4];
		memcpy(s, p, 8);
		c = {U(s[0], 16), U(s[1], 16), U(s[2], 16), U(s[3], 16)};
		return true;
	}
	case D3DFMT_L8: c = {U(p[0], 8), U(p[0], 8), U(p[0], 8), 1.0f}; return true;
	case D3DFMT_A8: c = {0, 0, 0, U(p[0], 8)}; return true;
	case D3DFMT_A8L8: c = {U(p[0], 8), U(p[0], 8), U(p[0], 8), U(p[1], 8)}; return true;
	case D3DFMT_A4L4: c = {U(p[0] & 15, 4), U(p[0] & 15, 4), U(p[0] & 15, 4), U(p[0] >> 4, 4)}; return true;
	case D3DFMT_L16: {
		float l = U(p[0] | (p[1] << 8), 16);
		c = {l, l, l, 1.0f};
		return true;
	}
	case D3DFMT_R16F: c = {D3DXFloat16To32(p[0] | (p[1] << 8)), 1.0f, 1.0f, 1.0f}; return true;
	case D3DFMT_G16R16F: {
		uint16_t s[2];
		memcpy(s, p, 4);
		c = {D3DXFloat16To32(s[0]), D3DXFloat16To32(s[1]), 1.0f, 1.0f};
		return true;
	}
	case D3DFMT_A16B16G16R16F: {
		uint16_t s[4];
		memcpy(s, p, 8);
		c = {D3DXFloat16To32(s[0]), D3DXFloat16To32(s[1]), D3DXFloat16To32(s[2]), D3DXFloat16To32(s[3])};
		return true;
	}
	case D3DFMT_R32F: {
		float r;
		memcpy(&r, p, 4);
		c = {r, 1.0f, 1.0f, 1.0f};
		return true;
	}
	case D3DFMT_G32R32F: {
		float r[2];
		memcpy(r, p, 8);
		c = {r[0], r[1], 1.0f, 1.0f};
		return true;
	}
	case D3DFMT_A32B32G32R32F: memcpy(&c, p, 16); return true;
	default: return false;
	}
}

bool PackPixel(D3DFORMAT f, const RGBA &c, uint8_t *p)
{
	uint32_t v;
	switch (f) {
	case D3DFMT_A8R8G8B8: case D3DFMT_X8R8G8B8:
		v = (Q(f == D3DFMT_X8R8G8B8 ? 1.0f : c.a, 8) << 24) | (Q(c.r, 8) << 16) | (Q(c.g, 8) << 8) | Q(c.b, 8);
		memcpy(p, &v, 4);
		return true;
	case D3DFMT_A8B8G8R8: case D3DFMT_X8B8G8R8:
		v = (Q(f == D3DFMT_X8B8G8R8 ? 1.0f : c.a, 8) << 24) | (Q(c.b, 8) << 16) | (Q(c.g, 8) << 8) | Q(c.r, 8);
		memcpy(p, &v, 4);
		return true;
	case D3DFMT_R8G8B8: p[0] = (uint8_t)Q(c.b, 8); p[1] = (uint8_t)Q(c.g, 8); p[2] = (uint8_t)Q(c.r, 8); return true;
	case D3DFMT_R5G6B5:
		v = (Q(c.r, 5) << 11) | (Q(c.g, 6) << 5) | Q(c.b, 5);
		p[0] = v & 255; p[1] = v >> 8;
		return true;
	case D3DFMT_X1R5G5B5: case D3DFMT_A1R5G5B5:
		v = ((f == D3DFMT_X1R5G5B5 || c.a >= 0.5f) ? 0x8000 : 0) | (Q(c.r, 5) << 10) | (Q(c.g, 5) << 5) | Q(c.b, 5);
		p[0] = v & 255; p[1] = v >> 8;
		return true;
	case D3DFMT_A4R4G4B4: case D3DFMT_X4R4G4B4:
		v = (Q(f == D3DFMT_X4R4G4B4 ? 1.0f : c.a, 4) << 12) | (Q(c.r, 4) << 8) | (Q(c.g, 4) << 4) | Q(c.b, 4);
		p[0] = v & 255; p[1] = v >> 8;
		return true;
	case D3DFMT_A2R10G10B10:
		v = (Q(c.a, 2) << 30) | (Q(c.r, 10) << 20) | (Q(c.g, 10) << 10) | Q(c.b, 10);
		memcpy(p, &v, 4);
		return true;
	case D3DFMT_A2B10G10R10:
		v = (Q(c.a, 2) << 30) | (Q(c.b, 10) << 20) | (Q(c.g, 10) << 10) | Q(c.r, 10);
		memcpy(p, &v, 4);
		return true;
	case D3DFMT_G16R16:
		v = (Q(c.g, 16) << 16) | Q(c.r, 16);
		memcpy(p, &v, 4);
		return true;
	case D3DFMT_A16B16G16R16: {
		uint16_t s[4] = {(uint16_t)Q(c.r, 16), (uint16_t)Q(c.g, 16), (uint16_t)Q(c.b, 16), (uint16_t)Q(c.a, 16)};
		memcpy(p, s, 8);
		return true;
	}
	case D3DFMT_L8: p[0] = (uint8_t)Q(c.r * 0.2126f + c.g * 0.7152f + c.b * 0.0722f, 8); return true;
	case D3DFMT_A8: p[0] = (uint8_t)Q(c.a, 8); return true;
	case D3DFMT_A8L8: p[0] = (uint8_t)Q(c.r * 0.2126f + c.g * 0.7152f + c.b * 0.0722f, 8); p[1] = (uint8_t)Q(c.a, 8); return true;
	case D3DFMT_A4L4: p[0] = (uint8_t)((Q(c.a, 4) << 4) | Q(c.r * 0.2126f + c.g * 0.7152f + c.b * 0.0722f, 4)); return true;
	case D3DFMT_L16: {
		uint16_t l = (uint16_t)Q(c.r * 0.2126f + c.g * 0.7152f + c.b * 0.0722f, 16);
		memcpy(p, &l, 2);
		return true;
	}
	case D3DFMT_R16F: { uint16_t h = FloatToHalf(c.r); memcpy(p, &h, 2); return true; }
	case D3DFMT_G16R16F: { uint16_t h[2] = {FloatToHalf(c.r), FloatToHalf(c.g)}; memcpy(p, h, 4); return true; }
	case D3DFMT_A16B16G16R16F: {
		uint16_t h[4] = {FloatToHalf(c.r), FloatToHalf(c.g), FloatToHalf(c.b), FloatToHalf(c.a)};
		memcpy(p, h, 8);
		return true;
	}
	case D3DFMT_R32F: memcpy(p, &c.r, 4); return true;
	case D3DFMT_G32R32F: memcpy(p, &c.r, 8); return true;
	case D3DFMT_A32B32G32R32F: memcpy(p, &c, 16); return true;
	default: return false;
	}
}

// ---- block compression

void Color565(uint16_t v, RGBA &c) { c = {U(v >> 11, 5), U((v >> 5) & 63, 6), U(v & 31, 5), 1.0f}; }

void DecodeColorBlock(const uint8_t *b, RGBA out[16], bool dxt1)
{
	uint16_t c0 = b[0] | (b[1] << 8), c1 = b[2] | (b[3] << 8);
	RGBA pal[4];
	Color565(c0, pal[0]);
	Color565(c1, pal[1]);
	if (!dxt1 || c0 > c1) {
		pal[2] = {(2 * pal[0].r + pal[1].r) / 3, (2 * pal[0].g + pal[1].g) / 3, (2 * pal[0].b + pal[1].b) / 3, 1.0f};
		pal[3] = {(pal[0].r + 2 * pal[1].r) / 3, (pal[0].g + 2 * pal[1].g) / 3, (pal[0].b + 2 * pal[1].b) / 3, 1.0f};
	} else {
		pal[2] = {(pal[0].r + pal[1].r) / 2, (pal[0].g + pal[1].g) / 2, (pal[0].b + pal[1].b) / 2, 1.0f};
		pal[3] = {0, 0, 0, 0};
	}
	uint32_t idx = b[4] | (b[5] << 8) | (b[6] << 16) | ((uint32_t)b[7] << 24);
	for (int i = 0; i < 16; i++) out[i] = pal[(idx >> (2 * i)) & 3];
}

void DecodeAlphaBlock(const uint8_t *b, RGBA out[16])
{
	float a[8];
	a[0] = b[0] / 255.0f;
	a[1] = b[1] / 255.0f;
	if (b[0] > b[1]) for (int i = 1; i < 7; i++) a[i + 1] = ((7 - i) * a[0] + i * a[1]) / 7;
	else {
		for (int i = 1; i < 5; i++) a[i + 1] = ((5 - i) * a[0] + i * a[1]) / 5;
		a[6] = 0;
		a[7] = 1;
	}
	uint64_t bits = 0;
	for (int i = 0; i < 6; i++) bits |= (uint64_t)b[2 + i] << (8 * i);
	for (int i = 0; i < 16; i++) out[i].a = a[(bits >> (3 * i)) & 7];
}

void DecodeBlock(D3DFORMAT f, const uint8_t *b, RGBA out[16])
{
	if (f == D3DFMT_DXT1) {
		DecodeColorBlock(b, out, true);
		return;
	}
	DecodeColorBlock(b + 8, out, false);
	if (f == D3DFMT_DXT2 || f == D3DFMT_DXT3) {
		for (int i = 0; i < 16; i++) out[i].a = ((b[i / 2] >> ((i & 1) * 4)) & 15) / 15.0f;
	} else {
		DecodeAlphaBlock(b, out);
	}
}

uint16_t To565(const RGBA &c) { return (uint16_t)((Q(c.r, 5) << 11) | (Q(c.g, 6) << 5) | Q(c.b, 5)); }

void EncodeColorBlock(const RGBA in[16], uint8_t *b, bool dxt1Alpha)
{
	bool anyTransparent = false;
	RGBA lo = {1, 1, 1, 1}, hi = {0, 0, 0, 1};
	for (int i = 0; i < 16; i++) {
		if (dxt1Alpha && in[i].a < 0.5f) { anyTransparent = true; continue; }
		lo.r = std::min(lo.r, in[i].r); lo.g = std::min(lo.g, in[i].g); lo.b = std::min(lo.b, in[i].b);
		hi.r = std::max(hi.r, in[i].r); hi.g = std::max(hi.g, in[i].g); hi.b = std::max(hi.b, in[i].b);
	}
	if (lo.r > hi.r) lo = hi = {0, 0, 0, 1};
	uint16_t c0 = To565(hi), c1 = To565(lo);
	if (anyTransparent) {
		if (c0 > c1) std::swap(c0, c1);
	} else if (c0 < c1) {
		std::swap(c0, c1);
	} else if (c0 == c1) {
		if (c1 > 0) c1--; else c0++;
	}
	RGBA pal[4];
	Color565(c0, pal[0]);
	Color565(c1, pal[1]);
	int n = anyTransparent ? 3 : 4;
	if (!anyTransparent) {
		pal[2] = {(2 * pal[0].r + pal[1].r) / 3, (2 * pal[0].g + pal[1].g) / 3, (2 * pal[0].b + pal[1].b) / 3, 1};
		pal[3] = {(pal[0].r + 2 * pal[1].r) / 3, (pal[0].g + 2 * pal[1].g) / 3, (pal[0].b + 2 * pal[1].b) / 3, 1};
	} else {
		pal[2] = {(pal[0].r + pal[1].r) / 2, (pal[0].g + pal[1].g) / 2, (pal[0].b + pal[1].b) / 2, 1};
	}
	uint32_t idx = 0;
	for (int i = 0; i < 16; i++) {
		int best = 0;
		if (anyTransparent && in[i].a < 0.5f) best = 3;
		else {
			float bd = 1e30f;
			for (int k = 0; k < n; k++) {
				float dr = in[i].r - pal[k].r, dg = in[i].g - pal[k].g, db = in[i].b - pal[k].b;
				float d = dr * dr + dg * dg + db * db;
				if (d < bd) { bd = d; best = k; }
			}
		}
		idx |= (uint32_t)best << (2 * i);
	}
	b[0] = c0 & 255; b[1] = c0 >> 8; b[2] = c1 & 255; b[3] = c1 >> 8;
	b[4] = idx & 255; b[5] = (idx >> 8) & 255; b[6] = (idx >> 16) & 255; b[7] = idx >> 24;
}

void EncodeAlphaBlock(const RGBA in[16], uint8_t *b)
{
	float lo = 1, hi = 0;
	for (int i = 0; i < 16; i++) { lo = std::min(lo, in[i].a); hi = std::max(hi, in[i].a); }
	uint8_t a0 = (uint8_t)Q(hi, 8), a1 = (uint8_t)Q(lo, 8);
	if (a0 == a1) { if (a0 < 255) a0++; else a1--; }
	float a[8];
	a[0] = a0 / 255.0f;
	a[1] = a1 / 255.0f;
	for (int i = 1; i < 7; i++) a[i + 1] = ((7 - i) * a[0] + i * a[1]) / 7;
	uint64_t bits = 0;
	for (int i = 0; i < 16; i++) {
		int best = 0;
		float bd = 1e30f;
		for (int k = 0; k < 8; k++) {
			float d = fabsf(in[i].a - a[k]);
			if (d < bd) { bd = d; best = k; }
		}
		bits |= (uint64_t)best << (3 * i);
	}
	b[0] = a0;
	b[1] = a1;
	for (int i = 0; i < 6; i++) b[2 + i] = (uint8_t)(bits >> (8 * i));
}

void EncodeBlock(D3DFORMAT f, const RGBA in[16], uint8_t *b)
{
	if (f == D3DFMT_DXT1) {
		EncodeColorBlock(in, b, true);
		return;
	}
	if (f == D3DFMT_DXT2 || f == D3DFMT_DXT3) {
		memset(b, 0, 8);
		for (int i = 0; i < 16; i++) b[i / 2] |= (uint8_t)(Q(in[i].a, 4) << ((i & 1) * 4));
	} else {
		EncodeAlphaBlock(in, b);
	}
	EncodeColorBlock(in, b + 8, false);
}

// ---- float images

struct FImage {
	UINT w = 0, h = 0;
	std::vector<RGBA> px;
	RGBA &at(UINT x, UINT y) { return px[(size_t)y * w + x]; }
	const RGBA &at(UINT x, UINT y) const { return px[(size_t)y * w + x]; }
};

bool Decode(D3DFORMAT f, const uint8_t *src, UINT pitch, UINT w, UINT h, FImage &out)
{
	out.w = w;
	out.h = h;
	out.px.assign((size_t)w * h, RGBA{0, 0, 0, 1});
	if (IsCompressed(f)) {
		PixFmt pi;
		PixInfo(f, pi);
		RGBA blk[16];
		for (UINT by = 0; by < (h + 3) / 4; by++)
			for (UINT bx = 0; bx < (w + 3) / 4; bx++) {
				DecodeBlock(f, src + (size_t)by * pitch + (size_t)bx * pi.bytes, blk);
				for (int i = 0; i < 16; i++) {
					UINT x = bx * 4 + (i & 3), y = by * 4 + i / 4;
					if (x < w && y < h) out.at(x, y) = blk[i];
				}
			}
		return true;
	}
	PixFmt pi;
	if (!PixInfo(f, pi)) return false;
	for (UINT y = 0; y < h; y++)
		for (UINT x = 0; x < w; x++)
			if (!UnpackPixel(f, src + (size_t)y * pitch + (size_t)x * pi.bytes, out.at(x, y))) return false;
	return true;
}

bool Encode(D3DFORMAT f, const FImage &in, uint8_t *dst, UINT pitch)
{
	if (IsCompressed(f)) {
		PixFmt pi;
		PixInfo(f, pi);
		RGBA blk[16];
		for (UINT by = 0; by < (in.h + 3) / 4; by++)
			for (UINT bx = 0; bx < (in.w + 3) / 4; bx++) {
				for (int i = 0; i < 16; i++) {
					UINT x = std::min(in.w - 1, bx * 4 + (i & 3)), y = std::min(in.h - 1, by * 4 + i / 4);
					blk[i] = in.at(x, y);
				}
				EncodeBlock(f, blk, dst + (size_t)by * pitch + (size_t)bx * pi.bytes);
			}
		return true;
	}
	PixFmt pi;
	if (!PixInfo(f, pi)) return false;
	for (UINT y = 0; y < in.h; y++)
		for (UINT x = 0; x < in.w; x++)
			if (!PackPixel(f, in.at(x, y), dst + (size_t)y * pitch + (size_t)x * pi.bytes)) return false;
	return true;
}

FImage Crop(const FImage &src, const RECT &r)
{
	FImage o;
	o.w = (UINT)(r.right - r.left);
	o.h = (UINT)(r.bottom - r.top);
	o.px.resize((size_t)o.w * o.h);
	for (UINT y = 0; y < o.h; y++)
		for (UINT x = 0; x < o.w; x++) o.at(x, y) = src.at(r.left + x, r.top + y);
	return o;
}

FImage Resample(const FImage &src, UINT w, UINT h, DWORD filter)
{
	if (src.w == w && src.h == h) return src;
	FImage o;
	o.w = w;
	o.h = h;
	o.px.resize((size_t)w * h);
	DWORD f = filter & 0xff;
	bool point = f == D3DX_FILTER_POINT || f == D3DX_FILTER_NONE;
	float sx = (float)src.w / w, sy = (float)src.h / h;
	if (!point && sx >= 1.0f && sy >= 1.0f) {
		for (UINT y = 0; y < h; y++)
			for (UINT x = 0; x < w; x++) {
				UINT x0 = (UINT)(x * sx), x1 = std::max(x0 + 1, std::min(src.w, (UINT)ceilf((x + 1) * sx)));
				UINT y0 = (UINT)(y * sy), y1 = std::max(y0 + 1, std::min(src.h, (UINT)ceilf((y + 1) * sy)));
				RGBA s = {0, 0, 0, 0};
				for (UINT yy = y0; yy < y1; yy++)
					for (UINT xx = x0; xx < x1; xx++) {
						const RGBA &p = src.at(xx, yy);
						s.r += p.r; s.g += p.g; s.b += p.b; s.a += p.a;
					}
				float n = (float)((x1 - x0) * (y1 - y0));
				o.at(x, y) = {s.r / n, s.g / n, s.b / n, s.a / n};
			}
		return o;
	}
	for (UINT y = 0; y < h; y++)
		for (UINT x = 0; x < w; x++) {
			float fx = (x + 0.5f) * sx - 0.5f, fy = (y + 0.5f) * sy - 0.5f;
			if (point) {
				o.at(x, y) = src.at(std::min(src.w - 1, (UINT)(x * sx)), std::min(src.h - 1, (UINT)(y * sy)));
				continue;
			}
			fx = std::max(0.0f, fx);
			fy = std::max(0.0f, fy);
			UINT x0 = std::min(src.w - 1, (UINT)fx), y0 = std::min(src.h - 1, (UINT)fy);
			UINT x1 = std::min(src.w - 1, x0 + 1), y1 = std::min(src.h - 1, y0 + 1);
			float tx = fx - x0, ty = fy - y0;
			const RGBA &a = src.at(x0, y0), &b = src.at(x1, y0), &c = src.at(x0, y1), &d = src.at(x1, y1);
			auto L = [&](float p, float q, float r, float s) { return (p * (1 - tx) + q * tx) * (1 - ty) + (r * (1 - tx) + s * tx) * ty; };
			o.at(x, y) = {L(a.r, b.r, c.r, d.r), L(a.g, b.g, c.g, d.g), L(a.b, b.b, c.b, d.b), L(a.a, b.a, c.a, d.a)};
		}
	return o;
}

void ApplyColorKey(FImage &img, D3DCOLOR key)
{
	if (!key) return;
	for (auto &p : img.px) {
		D3DCOLOR c = (Q(p.a, 8) << 24) | (Q(p.r, 8) << 16) | (Q(p.g, 8) << 8) | Q(p.b, 8);
		if (c == key) p = {0, 0, 0, 0};
	}
}

// ---- in-memory images loaded from files

struct Image {
	D3DFORMAT fmt = D3DFMT_UNKNOWN;
	UINT w = 0, h = 0, d = 1, levels = 1, faces = 1;
	bool cube = false, volume = false;
	D3DXIMAGE_FILEFORMAT ff = D3DXIFF_DDS;
	std::vector<std::vector<uint8_t>> subs; // face * levels + level; volumes hold all slices
	UINT LW(UINT l) const { return std::max(1u, w >> l); }
	UINT LH(UINT l) const { return std::max(1u, h >> l); }
	UINT LD(UINT l) const { return std::max(1u, d >> l); }
};

#pragma pack(push, 1)
struct DDSPixelFormat {
	uint32_t size, flags, fourCC, bits, r, g, b, a;
};
struct DDSHeader {
	uint32_t size, flags, height, width, pitch, depth, mips;
	uint32_t reserved1[11];
	DDSPixelFormat pf;
	uint32_t caps, caps2, caps3, caps4, reserved2;
};
struct DDSHeader10 {
	uint32_t dxgi, dim, misc, arraySize, misc2;
};
#pragma pack(pop)

D3DFORMAT FormatFromDDS(const DDSPixelFormat &pf, const uint8_t *&data, const uint8_t *end)
{
	if (pf.flags & 0x4) {
		if (pf.fourCC == MAKEFOURCC('D', 'X', '1', '0')) {
			if (data + sizeof(DDSHeader10) > end) return D3DFMT_UNKNOWN;
			DDSHeader10 h10;
			memcpy(&h10, data, sizeof(h10));
			data += sizeof(h10);
			switch (h10.dxgi) {
			case 71: case 72: return D3DFMT_DXT1;
			case 74: case 75: return D3DFMT_DXT3;
			case 77: case 78: return D3DFMT_DXT5;
			case 28: case 29: return D3DFMT_A8B8G8R8;
			case 87: case 91: return D3DFMT_A8R8G8B8;
			case 88: case 93: return D3DFMT_X8R8G8B8;
			case 10: return D3DFMT_A16B16G16R16F;
			case 11: return D3DFMT_A16B16G16R16;
			case 2: return D3DFMT_A32B32G32R32F;
			case 16: return D3DFMT_G32R32F;
			case 34: return D3DFMT_G16R16F;
			case 35: return D3DFMT_G16R16;
			case 41: return D3DFMT_R32F;
			case 54: return D3DFMT_R16F;
			case 61: return D3DFMT_L8;
			case 65: return D3DFMT_A8;
			case 24: return D3DFMT_A2B10G10R10;
			case 85: return D3DFMT_R5G6B5;
			case 86: return D3DFMT_A1R5G5B5;
			case 115: return D3DFMT_A4R4G4B4;
			default: return D3DFMT_UNKNOWN;
			}
		}
		return (D3DFORMAT)pf.fourCC;
	}
	bool alpha = (pf.flags & 0x1) != 0;
	if (pf.flags & 0x40) {
		switch (pf.bits) {
		case 32:
			if (pf.r == 0xff0000 && pf.g == 0xff00 && pf.b == 0xff) return alpha && pf.a ? D3DFMT_A8R8G8B8 : D3DFMT_X8R8G8B8;
			if (pf.r == 0xff && pf.g == 0xff00 && pf.b == 0xff0000) return alpha && pf.a ? D3DFMT_A8B8G8R8 : D3DFMT_X8B8G8R8;
			if (pf.r == 0xffff && pf.g == 0xffff0000) return D3DFMT_G16R16;
			if (pf.r == 0x3ff && pf.g == 0xffc00 && pf.b == 0x3ff00000) return D3DFMT_A2B10G10R10;
			if (pf.r == 0x3ff00000 && pf.g == 0xffc00 && pf.b == 0x3ff) return D3DFMT_A2R10G10B10;
			break;
		case 24:
			if (pf.r == 0xff0000) return D3DFMT_R8G8B8;
			break;
		case 16:
			if (pf.r == 0xf800) return D3DFMT_R5G6B5;
			if (pf.r == 0x7c00) return alpha && pf.a ? D3DFMT_A1R5G5B5 : D3DFMT_X1R5G5B5;
			if (pf.r == 0xf00) return alpha && pf.a ? D3DFMT_A4R4G4B4 : D3DFMT_X4R4G4B4;
			break;
		}
		return D3DFMT_UNKNOWN;
	}
	if (pf.flags & 0x20000) {
		if (pf.bits == 8) return D3DFMT_L8;
		if (pf.bits == 16) return alpha ? D3DFMT_A8L8 : D3DFMT_L16;
	}
	if ((pf.flags & 0x2) && pf.bits == 8) return D3DFMT_A8;
	return D3DFMT_UNKNOWN;
}

bool ParseDDS(const uint8_t *data, size_t size, Image &img, bool headerOnly)
{
	if (size < 4 + sizeof(DDSHeader) || memcmp(data, "DDS ", 4)) return false;
	DDSHeader h;
	memcpy(&h, data + 4, sizeof(h));
	const uint8_t *p = data + 4 + sizeof(h), *end = data + size;
	img.fmt = FormatFromDDS(h.pf, p, end);
	PixFmt pi;
	if (!PixInfo(img.fmt, pi)) {
		Log("unsupported DDS pixel format (fourcc %08x, %u bits)", h.pf.fourCC, h.pf.bits);
		return false;
	}
	img.w = std::max(1u, h.width);
	img.h = std::max(1u, h.height);
	img.cube = (h.caps2 & 0x200) != 0;
	img.volume = (h.caps2 & 0x200000) != 0 && h.depth > 1;
	img.d = img.volume ? h.depth : 1;
	img.faces = img.cube ? 6 : 1;
	img.levels = (h.flags & 0x20000) && h.mips ? h.mips : 1;
	img.ff = D3DXIFF_DDS;
	if (headerOnly) return true;
	img.subs.assign((size_t)img.faces * img.levels, {});
	for (UINT f = 0; f < img.faces; f++)
		for (UINT l = 0; l < img.levels; l++) {
			size_t bytes = (size_t)Pitch(img.fmt, img.LW(l)) * Rows(img.fmt, img.LH(l)) * img.LD(l);
			if (p + bytes > end) {
				if (f == 0 && l > 0) {
					img.levels = l;
					img.subs.resize(img.levels);
					return true;
				}
				return false;
			}
			img.subs[f * img.levels + l].assign(p, p + bytes);
			p += bytes;
		}
	return true;
}

bool ParseImageIO(const uint8_t *data, size_t size, Image &img, bool headerOnly)
{
	CFDataRef cd = CFDataCreateWithBytesNoCopy(nullptr, data, (CFIndex)size, kCFAllocatorNull);
	CGImageSourceRef src = CGImageSourceCreateWithData(cd, nullptr);
	CFRelease(cd);
	if (!src) return false;
	bool ok = false;
	CFStringRef type = CGImageSourceGetType(src);
	img.ff = D3DXIFF_PNG;
	if (type) {
		NSString *t = (__bridge NSString *)type;
		if ([t isEqualToString:UTTypeJPEG.identifier]) img.ff = D3DXIFF_JPG;
		else if ([t isEqualToString:UTTypeBMP.identifier]) img.ff = D3DXIFF_BMP;
		else if ([t containsString:@"tga"]) img.ff = D3DXIFF_TGA;
		else if ([t isEqualToString:UTTypePNG.identifier]) img.ff = D3DXIFF_PNG;
	}
	if (headerOnly) {
		CFDictionaryRef props = CGImageSourceCopyPropertiesAtIndex(src, 0, nullptr);
		if (props) {
			NSDictionary *d = (__bridge NSDictionary *)props;
			img.w = [d[(NSString *)kCGImagePropertyPixelWidth] unsignedIntValue];
			img.h = [d[(NSString *)kCGImagePropertyPixelHeight] unsignedIntValue];
			img.fmt = [d[(NSString *)kCGImagePropertyHasAlpha] boolValue] ? D3DFMT_A8R8G8B8 : D3DFMT_X8R8G8B8;
			ok = img.w && img.h;
			CFRelease(props);
		}
		CFRelease(src);
		return ok;
	}
	CGImageRef ci = CGImageSourceCreateImageAtIndex(src, 0, nullptr);
	CFRelease(src);
	if (!ci) return false;
	img.w = (UINT)CGImageGetWidth(ci);
	img.h = (UINT)CGImageGetHeight(ci);
	CGImageAlphaInfo ai = CGImageGetAlphaInfo(ci);
	bool alpha = !(ai == kCGImageAlphaNone || ai == kCGImageAlphaNoneSkipFirst || ai == kCGImageAlphaNoneSkipLast);
	img.fmt = alpha ? D3DFMT_A8R8G8B8 : D3DFMT_X8R8G8B8;
	img.levels = 1;
	img.subs.assign(1, std::vector<uint8_t>((size_t)img.w * img.h * 4));
	CGColorSpaceRef cs = CGColorSpaceCreateWithName(kCGColorSpaceSRGB);
	CGContextRef ctx = CGBitmapContextCreate(img.subs[0].data(), img.w, img.h, 8, img.w * 4, cs,
		kCGImageAlphaPremultipliedFirst | kCGBitmapByteOrder32Little);
	CGColorSpaceRelease(cs);
	if (ctx) {
		CGContextSetBlendMode(ctx, kCGBlendModeCopy);
		CGContextDrawImage(ctx, CGRectMake(0, 0, img.w, img.h), ci);
		CGContextRelease(ctx);
		if (alpha) {
			uint8_t *px = img.subs[0].data();
			for (size_t i = 0; i < (size_t)img.w * img.h; i++, px += 4) {
				uint8_t a = px[3];
				if (a && a < 255) {
					px[0] = (uint8_t)std::min(255, px[0] * 255 / a);
					px[1] = (uint8_t)std::min(255, px[1] * 255 / a);
					px[2] = (uint8_t)std::min(255, px[2] * 255 / a);
				}
			}
		}
		ok = true;
	}
	CGImageRelease(ci);
	return ok;
}

bool ParseImage(const void *data, size_t size, Image &img, bool headerOnly = false)
{
	if (!data || size < 4) return false;
	if (ParseDDS((const uint8_t *)data, size, img, headerOnly)) return true;
	if (!memcmp(data, "DDS ", 4)) return false;
	return ParseImageIO((const uint8_t *)data, size, img, headerOnly);
}

bool ReadFileBytes(const char *path, std::vector<uint8_t> &out)
{
	if (!path) return false;
	std::string p = path;
	for (char &c : p) if (c == '\\') c = '/';
	std::ifstream f(p, std::ios::binary | std::ios::ate);
	if (!f) return false;
	std::streamsize n = f.tellg();
	if (n <= 0) return false;
	out.resize((size_t)n);
	f.seekg(0);
	f.read((char *)out.data(), n);
	return (bool)f;
}

void FillInfo(const Image &img, D3DXIMAGE_INFO *info)
{
	if (!info) return;
	info->Width = img.w;
	info->Height = img.h;
	info->Depth = img.d;
	info->MipLevels = img.levels;
	info->Format = img.fmt;
	info->ResourceType = img.cube ? D3DRTYPE_CUBETEXTURE : img.volume ? D3DRTYPE_VOLUMETEXTURE : D3DRTYPE_TEXTURE;
	info->ImageFileFormat = img.ff;
}

// ---- reading and writing surfaces

bool ReadSub(Texture *t, UINT face, UINT level, std::vector<uint8_t> &out, UINT &pitch)
{
	UINT w = t->LevelWidth(level), h = t->LevelHeight(level);
	pitch = Pitch(t->fmt, w);
	out.assign((size_t)pitch * Rows(t->fmt, h), 0);
	if (t->IsSystem()) {
		const auto &s = t->sysmem[t->Sub(face, level)];
		memcpy(out.data(), s.data(), std::min(out.size(), s.size()));
		return true;
	}
	if (!t->tex) return false;
	t->dev->EndEncoders();
	t->Download(face, level, out.data(), pitch, nullptr);
	return true;
}

void WriteRect(Texture *t, UINT face, UINT level, const RECT &r, const uint8_t *data, UINT pitch)
{
	if (t->IsSystem()) {
		UINT dpitch = Pitch(t->fmt, t->LevelWidth(level));
		bool comp = IsCompressed(t->fmt);
		UINT rows = Rows(t->fmt, (UINT)(r.bottom - r.top));
		UINT y0 = comp ? (UINT)r.top / 4 : (UINT)r.top;
		UINT x0 = comp ? (UINT)r.left / 4 : (UINT)r.left;
		PixFmt pi;
		PixInfo(t->fmt, pi);
		UINT rowBytes = std::min(pitch, Pitch(t->fmt, (UINT)(r.right - r.left)));
		auto &s = t->sysmem[t->Sub(face, level)];
		for (UINT y = 0; y < rows; y++) {
			size_t off = (size_t)(y0 + y) * dpitch + (size_t)x0 * pi.bytes;
			if (off + rowBytes <= s.size()) memcpy(s.data() + off, data + (size_t)y * pitch, rowBytes);
		}
		return;
	}
	t->Upload(face, level, data, pitch, &r);
}

HRESULT LoadSurface(Surface *dst, const RECT *dstRect, const uint8_t *src, D3DFORMAT srcFmt, UINT srcPitch,
	UINT srcW, UINT srcH, const RECT *srcRect, DWORD filter, D3DCOLOR colorKey)
{
	Texture *t = dst->tex;
	UINT dw = t->LevelWidth(dst->level), dh = t->LevelHeight(dst->level);
	RECT dr = dstRect ? *dstRect : RECT{0, 0, (LONG)dw, (LONG)dh};
	RECT sr = srcRect ? *srcRect : RECT{0, 0, (LONG)srcW, (LONG)srcH};
	dr.right = std::min<LONG>(dr.right, dw);
	dr.bottom = std::min<LONG>(dr.bottom, dh);
	sr.right = std::min<LONG>(sr.right, srcW);
	sr.bottom = std::min<LONG>(sr.bottom, srcH);
	if (dr.right <= dr.left || dr.bottom <= dr.top || sr.right <= sr.left || sr.bottom <= sr.top) return D3DERR_INVALIDCALL;
	UINT ow = (UINT)(dr.right - dr.left), oh = (UINT)(dr.bottom - dr.top);
	bool sameSize = ow == (UINT)(sr.right - sr.left) && oh == (UINT)(sr.bottom - sr.top);
	bool comp = IsCompressed(srcFmt);

	if (srcFmt == t->fmt && sameSize && !colorKey && (!comp || (sr.left % 4 == 0 && sr.top % 4 == 0))) {
		PixFmt pi;
		PixInfo(srcFmt, pi);
		UINT rows = Rows(srcFmt, oh);
		UINT x0 = comp ? (UINT)sr.left / 4 : (UINT)sr.left, y0 = comp ? (UINT)sr.top / 4 : (UINT)sr.top;
		UINT rowBytes = Pitch(srcFmt, ow);
		std::vector<uint8_t> tmp((size_t)rowBytes * rows);
		for (UINT y = 0; y < rows; y++)
			memcpy(tmp.data() + (size_t)y * rowBytes, src + (size_t)(y0 + y) * srcPitch + (size_t)x0 * pi.bytes, rowBytes);
		WriteRect(t, dst->face, dst->level, dr, tmp.data(), rowBytes);
		return S_OK;
	}

	FImage img;
	if (!Decode(srcFmt, src, srcPitch, srcW, srcH, img)) return D3DERR_INVALIDCALL;
	img = Crop(img, sr);
	img = Resample(img, ow, oh, filter);
	ApplyColorKey(img, colorKey);

	if (IsCompressed(t->fmt) && (dr.left % 4 || dr.top % 4 || (ow % 4 && dr.right != (LONG)dw) || (oh % 4 && dr.bottom != (LONG)dh))) {
		RECT ar = {dr.left & ~3, dr.top & ~3, std::min<LONG>(dw, (dr.right + 3) & ~3), std::min<LONG>(dh, (dr.bottom + 3) & ~3)};
		std::vector<uint8_t> cur;
		UINT cpitch;
		ReadSub(t, dst->face, dst->level, cur, cpitch);
		FImage full;
		Decode(t->fmt, cur.data(), cpitch, dw, dh, full);
		for (UINT y = 0; y < oh; y++)
			for (UINT x = 0; x < ow; x++) full.at(dr.left + x, dr.top + y) = img.at(x, y);
		img = Crop(full, ar);
		dr = ar;
	}
	UINT pitch = Pitch(t->fmt, img.w);
	std::vector<uint8_t> out((size_t)pitch * Rows(t->fmt, img.h));
	if (!Encode(t->fmt, img, out.data(), pitch)) return D3DERR_INVALIDCALL;
	WriteRect(t, dst->face, dst->level, dr, out.data(), pitch);
	return S_OK;
}

UINT FullChain(UINT w, UINT h, UINT d = 1)
{
	UINT n = 1;
	while (w > 1 || h > 1 || d > 1) { w = std::max(1u, w / 2); h = std::max(1u, h / 2); d = std::max(1u, d / 2); n++; }
	return n;
}

D3DFORMAT ResolveFormat(D3DFORMAT want, const Image &img)
{
	D3DFORMAT f = (want == D3DFMT_FROM_FILE || want == D3DFMT_UNKNOWN) ? img.fmt : want;
	if (!GetFormat(f)) {
		PixFmt pi;
		bool alpha = f == D3DFMT_A8B8G8R8 || f == D3DFMT_A1R5G5B5 || f == D3DFMT_A4R4G4B4 || f == D3DFMT_A8 || f == D3DFMT_A8L8;
		if (PixInfo(f, pi) && pi.bytes > 4 && !pi.compressed) f = D3DFMT_A16B16G16R16F;
		else f = alpha ? D3DFMT_A8R8G8B8 : D3DFMT_X8R8G8B8;
		if (!GetFormat(f)) f = D3DFMT_A8R8G8B8;
	}
	return f;
}

void FillLevels(Texture *t, const Image &img, DWORD filter, DWORD mipFilter, D3DCOLOR colorKey)
{
	for (UINT face = 0; face < t->Faces(); face++) {
		UINT srcFace = std::min(face, img.faces - 1);
		FImage prev;
		bool havePrev = false;
		for (UINT l = 0; l < t->levels; l++) {
			UINT w = t->LevelWidth(l), h = t->LevelHeight(l);
			bool fromFile = l < img.levels && img.LW(l) == w && img.LH(l) == h;
			if (t->type == TT_3D) {
				if (l < img.levels && img.fmt == t->fmt && img.LW(l) == w && img.LH(l) == h) {
					const auto &s = img.subs[l];
					if (t->IsSystem()) t->sysmem[l] = s;
					else t->Upload(0, l, s.data(), Pitch(t->fmt, w), nullptr, 0, img.LD(l));
				}
				continue;
			}
			RECT full = {0, 0, (LONG)w, (LONG)h};
			if (fromFile && img.fmt == t->fmt && !colorKey) {
				WriteRect(t, face, l, full, img.subs[srcFace * img.levels + l].data(), Pitch(t->fmt, w));
				havePrev = false;
				if (l + 1 < t->levels && !(l + 1 < img.levels)) {
					Decode(img.fmt, img.subs[srcFace * img.levels + l].data(), Pitch(img.fmt, w), w, h, prev);
					havePrev = true;
				}
				continue;
			}
			FImage cur;
			if (fromFile) {
				Decode(img.fmt, img.subs[srcFace * img.levels + l].data(), Pitch(img.fmt, w), w, h, cur);
				ApplyColorKey(cur, colorKey);
			} else if (havePrev) {
				cur = Resample(prev, w, h, mipFilter == D3DX_DEFAULT ? D3DX_FILTER_BOX : mipFilter);
			} else {
				FImage base;
				Decode(img.fmt, img.subs[srcFace * img.levels].data(), Pitch(img.fmt, img.w), img.w, img.h, base);
				ApplyColorKey(base, colorKey);
				cur = Resample(base, w, h, l == 0 ? filter : mipFilter);
			}
			UINT pitch = Pitch(t->fmt, w);
			std::vector<uint8_t> out((size_t)pitch * Rows(t->fmt, h));
			Encode(t->fmt, cur, out.data(), pitch);
			WriteRect(t, face, l, full, out.data(), pitch);
			prev = std::move(cur);
			havePrev = true;
		}
	}
}

UINT PickSize(UINT want, UINT file)
{
	if (want == 0 || want == D3DX_DEFAULT || want == D3DX_DEFAULT_NONPOW2 || want == D3DX_FROM_FILE) return file;
	return want;
}

template<typename T>
HRESULT CreateFromImage(IDirect3DDevice9 *dev, const Image &img, TexType type, UINT w, UINT h, UINT mips, DWORD usage,
	D3DFORMAT fmt, D3DPOOL pool, DWORD filter, DWORD mipFilter, D3DCOLOR colorKey, D3DXIMAGE_INFO *info, T **out)
{
	if (!dev || !out) return D3DERR_INVALIDCALL;
	FillInfo(img, info);
	UINT tw = PickSize(w, img.w), th = PickSize(h, img.h);
	if (type == TT_CUBE) th = tw;
	D3DFORMAT f = ResolveFormat(fmt, img);
	if (IsCompressed(f) && ((tw % 4) || (th % 4)) && (tw > 4 || th > 4)) {
		if (f != img.fmt) f = D3DFMT_A8R8G8B8;
	}
	UINT chain = FullChain(tw, th, type == TT_3D ? img.d : 1);
	UINT levels = mips == D3DX_FROM_FILE ? std::min(img.levels, chain) : (mips == 0 || mips == D3DX_DEFAULT) ? chain : std::min(mips, chain);
	if (usage & D3DUSAGE_AUTOGENMIPMAP) levels = chain;
	Texture *t = CreateTexture(dev->Impl(), type, tw, th, type == TT_3D ? img.d : 1, levels, usage, f, pool, 1);
	if (!t) return D3DERR_OUTOFVIDEOMEMORY;
	T *iface = new T();
	t->iface = iface;
	iface->M9Attach(t);
	FillLevels(t, img, filter, mipFilter, colorKey);
	*out = iface;
	return S_OK;
}

DDSPixelFormat DDSFormatFor(D3DFORMAT f)
{
	DDSPixelFormat pf = {32, 0, 0, 0, 0, 0, 0, 0};
	switch (f) {
	case D3DFMT_A8R8G8B8: pf = {32, 0x41, 0, 32, 0xff0000, 0xff00, 0xff, 0xff000000}; break;
	case D3DFMT_X8R8G8B8: pf = {32, 0x40, 0, 32, 0xff0000, 0xff00, 0xff, 0}; break;
	case D3DFMT_A8B8G8R8: pf = {32, 0x41, 0, 32, 0xff, 0xff00, 0xff0000, 0xff000000}; break;
	case D3DFMT_X8B8G8R8: pf = {32, 0x40, 0, 32, 0xff, 0xff00, 0xff0000, 0}; break;
	case D3DFMT_R8G8B8: pf = {32, 0x40, 0, 24, 0xff0000, 0xff00, 0xff, 0}; break;
	case D3DFMT_R5G6B5: pf = {32, 0x40, 0, 16, 0xf800, 0x7e0, 0x1f, 0}; break;
	case D3DFMT_X1R5G5B5: pf = {32, 0x40, 0, 16, 0x7c00, 0x3e0, 0x1f, 0}; break;
	case D3DFMT_A1R5G5B5: pf = {32, 0x41, 0, 16, 0x7c00, 0x3e0, 0x1f, 0x8000}; break;
	case D3DFMT_A4R4G4B4: pf = {32, 0x41, 0, 16, 0xf00, 0xf0, 0xf, 0xf000}; break;
	case D3DFMT_X4R4G4B4: pf = {32, 0x40, 0, 16, 0xf00, 0xf0, 0xf, 0}; break;
	case D3DFMT_G16R16: pf = {32, 0x40, 0, 32, 0xffff, 0xffff0000, 0, 0}; break;
	case D3DFMT_A2B10G10R10: pf = {32, 0x41, 0, 32, 0x3ff, 0xffc00, 0x3ff00000, 0xc0000000}; break;
	case D3DFMT_A2R10G10B10: pf = {32, 0x41, 0, 32, 0x3ff00000, 0xffc00, 0x3ff, 0xc0000000}; break;
	case D3DFMT_L8: pf = {32, 0x20000, 0, 8, 0xff, 0, 0, 0}; break;
	case D3DFMT_L16: pf = {32, 0x20000, 0, 16, 0xffff, 0, 0, 0}; break;
	case D3DFMT_A8L8: pf = {32, 0x20001, 0, 16, 0xff, 0, 0, 0xff00}; break;
	case D3DFMT_A8: pf = {32, 0x2, 0, 8, 0, 0, 0, 0xff}; break;
	default: pf = {32, 0x4, (uint32_t)f, 0, 0, 0, 0, 0}; break;
	}
	return pf;
}

bool WriteDDS(const char *file, Texture *t)
{
	DDSHeader h = {};
	h.size = 124;
	h.flags = 0x1 | 0x2 | 0x4 | 0x1000 | (t->levels > 1 ? 0x20000 : 0) | (IsCompressed(t->fmt) ? 0x80000 : 0x8);
	h.width = t->width;
	h.height = t->height;
	h.pitch = IsCompressed(t->fmt) ? Pitch(t->fmt, t->width) * Rows(t->fmt, t->height) : Pitch(t->fmt, t->width);
	h.mips = t->levels;
	h.pf = DDSFormatFor(t->fmt);
	h.caps = 0x1000 | (t->levels > 1 ? 0x400008 : 0) | (t->type == TT_CUBE ? 0x8 : 0);
	if (t->type == TT_CUBE) h.caps2 = 0x200 | 0xfc00;
	std::string p = file;
	for (char &c : p) if (c == '\\') c = '/';
	std::ofstream f(p, std::ios::binary);
	if (!f) return false;
	f.write("DDS ", 4);
	f.write((const char *)&h, sizeof(h));
	for (UINT face = 0; face < t->Faces(); face++)
		for (UINT l = 0; l < t->levels; l++) {
			std::vector<uint8_t> data;
			UINT pitch;
			if (!ReadSub(t, face, l, data, pitch)) return false;
			f.write((const char *)data.data(), (std::streamsize)data.size());
		}
	return (bool)f;
}

bool WriteImageIO(const char *file, D3DXIMAGE_FILEFORMAT ff, D3DFORMAT fmt, const uint8_t *data, UINT pitch, UINT w, UINT h)
{
	FImage img;
	if (!Decode(fmt, data, pitch, w, h, img)) return false;
	std::vector<uint8_t> bgra((size_t)w * h * 4);
	bool alpha = ff == D3DXIFF_PNG || ff == D3DXIFF_TGA;
	for (size_t i = 0; i < img.px.size(); i++) {
		const RGBA &c = img.px[i];
		uint8_t a = alpha ? (uint8_t)Q(c.a, 8) : 255;
		bgra[i * 4 + 0] = (uint8_t)(Q(c.b, 8) * a / 255);
		bgra[i * 4 + 1] = (uint8_t)(Q(c.g, 8) * a / 255);
		bgra[i * 4 + 2] = (uint8_t)(Q(c.r, 8) * a / 255);
		bgra[i * 4 + 3] = a;
	}
	CGColorSpaceRef cs = CGColorSpaceCreateWithName(kCGColorSpaceSRGB);
	CGContextRef ctx = CGBitmapContextCreate(bgra.data(), w, h, 8, w * 4, cs,
		(alpha ? kCGImageAlphaPremultipliedFirst : kCGImageAlphaNoneSkipFirst) | kCGBitmapByteOrder32Little);
	CGColorSpaceRelease(cs);
	if (!ctx) return false;
	CGImageRef ci = CGBitmapContextCreateImage(ctx);
	CGContextRelease(ctx);
	UTType *type = UTTypePNG;
	switch (ff) {
	case D3DXIFF_JPG: type = UTTypeJPEG; break;
	case D3DXIFF_BMP: case D3DXIFF_DIB: type = UTTypeBMP; break;
	case D3DXIFF_TGA: type = [UTType typeWithIdentifier:@"com.truevision.tga-image"] ?: UTTypePNG; break;
	default: break;
	}
	std::string p = file;
	for (char &c : p) if (c == '\\') c = '/';
	NSURL *url = [NSURL fileURLWithPath:[NSString stringWithUTF8String:p.c_str()]];
	CGImageDestinationRef dst = CGImageDestinationCreateWithURL((__bridge CFURLRef)url, (__bridge CFStringRef)type.identifier, 1, nullptr);
	bool ok = false;
	if (dst) {
		CGImageDestinationAddImage(dst, ci, nullptr);
		ok = CGImageDestinationFinalize(dst);
		CFRelease(dst);
	}
	CGImageRelease(ci);
	return ok;
}

}

// ---------------------------------------------------------------------------
// D3DX texture API

HRESULT D3DXCreateTexture(IDirect3DDevice9 *dev, UINT w, UINT h, UINT mips, DWORD usage, D3DFORMAT fmt, D3DPOOL pool, IDirect3DTexture9 **tex)
{
	if (!dev) return D3DERR_INVALIDCALL;
	w = std::max(1u, w == D3DX_DEFAULT ? 256u : w);
	h = std::max(1u, h == D3DX_DEFAULT ? 256u : h);
	UINT chain = FullChain(w, h);
	UINT levels = (mips == 0 || mips == D3DX_DEFAULT) ? chain : std::min(mips, chain);
	return dev->CreateTexture(w, h, levels, usage, fmt, pool, tex, nullptr);
}

HRESULT D3DXCreateCubeTexture(IDirect3DDevice9 *dev, UINT size, UINT mips, DWORD usage, D3DFORMAT fmt, D3DPOOL pool, IDirect3DCubeTexture9 **tex)
{
	if (!dev) return D3DERR_INVALIDCALL;
	size = std::max(1u, size == D3DX_DEFAULT ? 256u : size);
	UINT chain = FullChain(size, size);
	UINT levels = (mips == 0 || mips == D3DX_DEFAULT) ? chain : std::min(mips, chain);
	return dev->CreateCubeTexture(size, levels, usage, fmt, pool, tex, nullptr);
}

HRESULT D3DXCreateVolumeTexture(IDirect3DDevice9 *dev, UINT w, UINT h, UINT d, UINT mips, DWORD usage, D3DFORMAT fmt, D3DPOOL pool, IDirect3DVolumeTexture9 **tex)
{
	if (!dev) return D3DERR_INVALIDCALL;
	UINT chain = FullChain(w, h, d);
	UINT levels = (mips == 0 || mips == D3DX_DEFAULT) ? chain : std::min(mips, chain);
	return dev->CreateVolumeTexture(w, h, d, levels, usage, fmt, pool, tex, nullptr);
}

HRESULT D3DXGetImageInfoFromFileInMemory(const void *data, UINT size, D3DXIMAGE_INFO *info)
{
	Image img;
	if (!ParseImage(data, size, img, true)) return D3DERR_INVALIDCALL;
	FillInfo(img, info);
	return S_OK;
}

HRESULT D3DXGetImageInfoFromFileA(const char *file, D3DXIMAGE_INFO *info)
{
	std::vector<uint8_t> buf;
	if (!ReadFileBytes(file, buf)) return D3DERR_NOTFOUND;
	return D3DXGetImageInfoFromFileInMemory(buf.data(), (UINT)buf.size(), info);
}

HRESULT D3DXCreateTextureFromFileInMemoryEx(IDirect3DDevice9 *dev, const void *data, UINT size, UINT w, UINT h, UINT mips, DWORD usage,
	D3DFORMAT fmt, D3DPOOL pool, DWORD filter, DWORD mipFilter, D3DCOLOR colorKey, D3DXIMAGE_INFO *info, PALETTEENTRY *, IDirect3DTexture9 **tex)
{
	Image img;
	if (!ParseImage(data, size, img)) return D3DERR_INVALIDCALL;
	if (img.cube || img.volume) {
		img.faces = 1;
		img.cube = img.volume = false;
		img.d = 1;
		img.subs.resize(img.levels);
	}
	return CreateFromImage(dev, img, TT_2D, w, h, mips, usage, fmt, pool, filter, mipFilter, colorKey, info, tex);
}

HRESULT D3DXCreateTextureFromFileExA(IDirect3DDevice9 *dev, const char *file, UINT w, UINT h, UINT mips, DWORD usage,
	D3DFORMAT fmt, D3DPOOL pool, DWORD filter, DWORD mipFilter, D3DCOLOR colorKey, D3DXIMAGE_INFO *info, PALETTEENTRY *pal, IDirect3DTexture9 **tex)
{
	std::vector<uint8_t> buf;
	if (!ReadFileBytes(file, buf)) return D3DERR_NOTFOUND;
	return D3DXCreateTextureFromFileInMemoryEx(dev, buf.data(), (UINT)buf.size(), w, h, mips, usage, fmt, pool, filter, mipFilter, colorKey, info, pal, tex);
}

HRESULT D3DXCreateTextureFromFileInMemory(IDirect3DDevice9 *dev, const void *data, UINT size, IDirect3DTexture9 **tex)
{
	return D3DXCreateTextureFromFileInMemoryEx(dev, data, size, D3DX_DEFAULT, D3DX_DEFAULT, D3DX_DEFAULT, 0, D3DFMT_UNKNOWN,
		D3DPOOL_MANAGED, D3DX_DEFAULT, D3DX_DEFAULT, 0, nullptr, nullptr, tex);
}

HRESULT D3DXCreateTextureFromFileA(IDirect3DDevice9 *dev, const char *file, IDirect3DTexture9 **tex)
{
	return D3DXCreateTextureFromFileExA(dev, file, D3DX_DEFAULT, D3DX_DEFAULT, D3DX_DEFAULT, 0, D3DFMT_UNKNOWN,
		D3DPOOL_MANAGED, D3DX_DEFAULT, D3DX_DEFAULT, 0, nullptr, nullptr, tex);
}

HRESULT D3DXCreateCubeTextureFromFileA(IDirect3DDevice9 *dev, const char *file, IDirect3DCubeTexture9 **tex)
{
	std::vector<uint8_t> buf;
	if (!ReadFileBytes(file, buf)) return D3DERR_NOTFOUND;
	Image img;
	if (!ParseImage(buf.data(), buf.size(), img) || !img.cube) return D3DERR_INVALIDCALL;
	return CreateFromImage(dev, img, TT_CUBE, D3DX_DEFAULT, D3DX_DEFAULT, D3DX_DEFAULT, 0, D3DFMT_UNKNOWN,
		D3DPOOL_MANAGED, D3DX_DEFAULT, D3DX_DEFAULT, 0, nullptr, tex);
}

HRESULT D3DXLoadSurfaceFromMemory(IDirect3DSurface9 *dst, const PALETTEENTRY *, const RECT *dstRect,
	const void *src, D3DFORMAT srcFmt, UINT srcPitch, const PALETTEENTRY *, const RECT *srcRect, DWORD filter, D3DCOLOR colorKey)
{
	Surface *d = SurfImpl(dst);
	if (!d || !src || !srcRect) return D3DERR_INVALIDCALL;
	UINT w = (UINT)srcRect->right, h = (UINT)srcRect->bottom;
	return LoadSurface(d, dstRect, (const uint8_t *)src, srcFmt, srcPitch, w, h, srcRect, filter, colorKey);
}

HRESULT D3DXLoadSurfaceFromFileInMemory(IDirect3DSurface9 *dst, const PALETTEENTRY *, const RECT *dstRect,
	const void *data, UINT size, const RECT *srcRect, DWORD filter, D3DCOLOR colorKey, D3DXIMAGE_INFO *info)
{
	Surface *d = SurfImpl(dst);
	if (!d) return D3DERR_INVALIDCALL;
	Image img;
	if (!ParseImage(data, size, img)) return D3DERR_INVALIDCALL;
	FillInfo(img, info);
	return LoadSurface(d, dstRect, img.subs[0].data(), img.fmt, Pitch(img.fmt, img.w), img.w, img.h, srcRect, filter, colorKey);
}

HRESULT D3DXLoadSurfaceFromFileA(IDirect3DSurface9 *dst, const PALETTEENTRY *pal, const RECT *dstRect,
	const char *file, const RECT *srcRect, DWORD filter, D3DCOLOR colorKey, D3DXIMAGE_INFO *info)
{
	std::vector<uint8_t> buf;
	if (!ReadFileBytes(file, buf)) return D3DERR_NOTFOUND;
	return D3DXLoadSurfaceFromFileInMemory(dst, pal, dstRect, buf.data(), (UINT)buf.size(), srcRect, filter, colorKey, info);
}

HRESULT D3DXLoadSurfaceFromSurface(IDirect3DSurface9 *dst, const PALETTEENTRY *, const RECT *dstRect,
	IDirect3DSurface9 *src, const PALETTEENTRY *, const RECT *srcRect, DWORD filter, D3DCOLOR colorKey)
{
	Surface *d = SurfImpl(dst), *s = SurfImpl(src);
	if (!d || !s) return D3DERR_INVALIDCALL;
	std::vector<uint8_t> data;
	UINT pitch;
	if (!ReadSub(s->tex, s->face, s->level, data, pitch)) return D3DERR_INVALIDCALL;
	return LoadSurface(d, dstRect, data.data(), s->tex->fmt, pitch, s->tex->LevelWidth(s->level), s->tex->LevelHeight(s->level),
		srcRect, filter, colorKey);
}

HRESULT D3DXSaveTextureToFileA(const char *file, D3DXIMAGE_FILEFORMAT ff, IDirect3DBaseTexture9 *tex, const PALETTEENTRY *)
{
	if (!file || !tex || !tex->Impl()) return D3DERR_INVALIDCALL;
	Texture *t = tex->Impl();
	if (ff == D3DXIFF_DDS) return WriteDDS(file, t) ? S_OK : E_FAIL;
	std::vector<uint8_t> data;
	UINT pitch;
	if (!ReadSub(t, 0, 0, data, pitch)) return E_FAIL;
	return WriteImageIO(file, ff, t->fmt, data.data(), pitch, t->width, t->height) ? S_OK : E_FAIL;
}

HRESULT D3DXSaveSurfaceToFileA(const char *file, D3DXIMAGE_FILEFORMAT ff, IDirect3DSurface9 *surf, const PALETTEENTRY *, const RECT *rect)
{
	Surface *s = SurfImpl(surf);
	if (!file || !s) return D3DERR_INVALIDCALL;
	Texture *t = s->tex;
	std::vector<uint8_t> data;
	UINT pitch;
	if (!ReadSub(t, s->face, s->level, data, pitch)) return E_FAIL;
	UINT w = t->LevelWidth(s->level), h = t->LevelHeight(s->level);
	RECT r = rect ? *rect : RECT{0, 0, (LONG)w, (LONG)h};
	FImage img;
	if (!Decode(t->fmt, data.data(), pitch, w, h, img)) return E_FAIL;
	img = Crop(img, r);
	D3DFORMAT outFmt = IsCompressed(t->fmt) && ff != D3DXIFF_DDS ? D3DFMT_A8R8G8B8 : t->fmt;
	if (IsCompressed(outFmt) && (img.w % 4 || img.h % 4)) outFmt = D3DFMT_A8R8G8B8;
	UINT opitch = Pitch(outFmt, img.w);
	std::vector<uint8_t> out((size_t)opitch * Rows(outFmt, img.h));
	Encode(outFmt, img, out.data(), opitch);
	if (ff != D3DXIFF_DDS) return WriteImageIO(file, ff, outFmt, out.data(), opitch, img.w, img.h) ? S_OK : E_FAIL;
	DDSHeader h2 = {};
	h2.size = 124;
	h2.flags = 0x1 | 0x2 | 0x4 | 0x1000 | (IsCompressed(outFmt) ? 0x80000 : 0x8);
	h2.width = img.w;
	h2.height = img.h;
	h2.pitch = IsCompressed(outFmt) ? (UINT)out.size() : opitch;
	h2.mips = 1;
	h2.pf = DDSFormatFor(outFmt);
	h2.caps = 0x1000;
	std::string p = file;
	for (char &c : p) if (c == '\\') c = '/';
	std::ofstream f(p, std::ios::binary);
	if (!f) return E_FAIL;
	f.write("DDS ", 4);
	f.write((const char *)&h2, sizeof(h2));
	f.write((const char *)out.data(), (std::streamsize)out.size());
	return f ? S_OK : E_FAIL;
}

HRESULT D3DXFilterTexture(IDirect3DBaseTexture9 *tex, const PALETTEENTRY *, UINT srcLevel, DWORD filter)
{
	if (!tex || !tex->Impl()) return D3DERR_INVALIDCALL;
	Texture *t = tex->Impl();
	if (srcLevel == D3DX_DEFAULT) srcLevel = 0;
	if (srcLevel + 1 >= t->levels) return S_OK;
	if (t->type == TT_3D) return S_OK;
	if (!t->IsSystem() && srcLevel == 0 && t->fi->kind == FK_PLAIN && t->tex) {
		id<MTLTexture> mt = t->tex;
		t->dev->EncodeBlit(^(id<MTLBlitCommandEncoder> b) { [b generateMipmapsForTexture:mt]; });
		return S_OK;
	}
	for (UINT face = 0; face < t->Faces(); face++) {
		std::vector<uint8_t> data;
		UINT pitch;
		if (!ReadSub(t, face, srcLevel, data, pitch)) return E_FAIL;
		FImage prev;
		if (!Decode(t->fmt, data.data(), pitch, t->LevelWidth(srcLevel), t->LevelHeight(srcLevel), prev)) return E_FAIL;
		for (UINT l = srcLevel + 1; l < t->levels; l++) {
			UINT w = t->LevelWidth(l), h = t->LevelHeight(l);
			FImage cur = Resample(prev, w, h, filter == D3DX_DEFAULT ? D3DX_FILTER_BOX : filter);
			UINT p = Pitch(t->fmt, w);
			std::vector<uint8_t> out((size_t)p * Rows(t->fmt, h));
			Encode(t->fmt, cur, out.data(), p);
			RECT r = {0, 0, (LONG)w, (LONG)h};
			WriteRect(t, face, l, r, out.data(), p);
			prev = std::move(cur);
		}
	}
	return S_OK;
}

// ---------------------------------------------------------------------------
// Fonts

namespace m9 {

struct TextEntry {
	std::u16string text;
	DWORD format;
	LONG w, h;
	id<MTLTexture> tex;
	LONG texW, texH;
};

struct Font {
	Device *dev = nullptr;
	D3DXFONT_DESCA desc = {};
	CTFontRef font = nullptr;
	float ascent = 0, descent = 0, leading = 0;
	std::list<TextEntry> cache;
	~Font() { if (font) CFRelease(font); }
};

}

static CTFontRef MakeCTFont(const char *face, int height, int width, int weight, bool italic)
{
	NSString *name = face && face[0] ? [NSString stringWithUTF8String:face] : @"Helvetica";
	CTFontRef f = CTFontCreateWithName((__bridge CFStringRef)name, 12.0, nullptr);
	if (!f) f = CTFontCreateUIFontForLanguage(kCTFontUIFontSystem, 12.0, nullptr);
	CGFloat size;
	if (height < 0) size = -height;
	else {
		CGFloat cell = CTFontGetAscent(f) + CTFontGetDescent(f);
		size = height ? 12.0 * height / cell : 12.0;
	}
	CTFontSymbolicTraits traits = 0;
	if (weight >= 600) traits |= kCTFontBoldTrait;
	if (italic) traits |= kCTFontItalicTrait;
	CGAffineTransform xf = CGAffineTransformIdentity;
	CTFontRef sized = traits ? CTFontCreateCopyWithSymbolicTraits(f, size, nullptr, traits, traits) : nullptr;
	if (!sized) sized = CTFontCreateCopyWithAttributes(f, size, nullptr, nullptr);
	CFRelease(f);
	if (width > 0) {
		UniChar ch = 'x';
		CGGlyph g;
		CGSize adv = {};
		if (CTFontGetGlyphsForCharacters(sized, &ch, &g, 1)) CTFontGetAdvancesForGlyphs(sized, kCTFontOrientationHorizontal, &g, &adv, 1);
		if (adv.width > 0) {
			xf = CGAffineTransformMakeScale(width / adv.width, 1.0);
			CTFontRef scaled = CTFontCreateCopyWithAttributes(sized, size, &xf, nullptr);
			CFRelease(sized);
			sized = scaled;
		}
	}
	return sized;
}

static CTFontRef MakeFont(const D3DXFONT_DESCA &d)
{
	return MakeCTFont(d.FaceName, d.Height, d.Width, d.Weight, d.Italic != 0);
}

void *m9::CreateFontRef(const char *face, int height, int width, int weight, bool italic)
{
	return (void *)MakeCTFont(face, height, width, weight, italic);
}

void m9::ReleaseFontRef(void *font)
{
	if (font) CFRelease((CTFontRef)font);
}

void m9::GetFontMetrics(void *font, FontMetrics *m)
{
	CTFontRef f = (CTFontRef)font;
	m->ascent = (int)ceil(CTFontGetAscent(f));
	m->descent = (int)ceil(CTFontGetDescent(f));
	m->height = m->ascent + m->descent;
	m->internalLeading = std::max(0, m->height - (int)lrint(CTFontGetSize(f)));
	m->externalLeading = (int)ceil(CTFontGetLeading(f));
	m->aveCharWidth = (int)ceil(GlyphAdvance(font, 'x'));
	m->maxCharWidth = (int)ceil(CTFontGetBoundingBox(f).size.width);
}

static bool GlyphFor(CTFontRef f, uint32_t ch, CGGlyph &g)
{
	UniChar u[2];
	CFIndex n = 1;
	if (ch >= 0x10000) {
		ch -= 0x10000;
		u[0] = (UniChar)(0xD800 + (ch >> 10));
		u[1] = (UniChar)(0xDC00 + (ch & 0x3FF));
		n = 2;
	} else {
		u[0] = (UniChar)ch;
	}
	CGGlyph gl[2] = {0, 0};
	if (!CTFontGetGlyphsForCharacters(f, u, gl, n)) return false;
	g = gl[0];
	return true;
}

float m9::GlyphAdvance(void *font, uint32_t ch)
{
	CTFontRef f = (CTFontRef)font;
	CGGlyph g;
	if (!GlyphFor(f, ch, g)) return 0.0f;
	CGSize adv = {};
	CTFontGetAdvancesForGlyphs(f, kCTFontOrientationHorizontal, &g, &adv, 1);
	return (float)adv.width;
}

void m9::DrawGlyph(void *font, uint32_t ch, uint8_t *gray, int pitch, int w, int h, float x, float baseline, bool antialias)
{
	CTFontRef f = (CTFontRef)font;
	CGGlyph g;
	if (!GlyphFor(f, ch, g)) return;
	CGColorSpaceRef cs = CGColorSpaceCreateDeviceGray();
	CGContextRef ctx = CGBitmapContextCreate(gray, w, h, 8, pitch, cs, kCGImageAlphaNone);
	CGColorSpaceRelease(cs);
	if (!ctx) return;
	CGContextSetShouldAntialias(ctx, antialias);
	CGContextSetShouldSmoothFonts(ctx, false);
	CGContextSetGrayFillColor(ctx, 1.0, 1.0);
	CGPoint p = CGPointMake(x, h - baseline);
	CTFontDrawGlyphs(f, &g, &p, 1, ctx);
	CGContextRelease(ctx);
}

uint32_t m9::AnsiToUnicode(uint8_t c)
{
	static const uint16_t cp1252[32] = {
		0x20AC, 0x0081, 0x201A, 0x0192, 0x201E, 0x2026, 0x2020, 0x2021, 0x02C6, 0x2030, 0x0160, 0x2039, 0x0152, 0x008D, 0x017D, 0x008F,
		0x0090, 0x2018, 0x2019, 0x201C, 0x201D, 0x2022, 0x2013, 0x2014, 0x02DC, 0x2122, 0x0161, 0x203A, 0x0153, 0x009D, 0x017E, 0x0178};
	return c >= 0x80 && c < 0xA0 ? cp1252[c - 0x80] : c;
}

HRESULT D3DXCreateFontIndirectA(IDirect3DDevice9 *dev, const D3DXFONT_DESCA *desc, ID3DXFont **font)
{
	if (!dev || !desc || !font) return D3DERR_INVALIDCALL;
	Font *f = new Font();
	f->dev = dev->Impl();
	f->desc = *desc;
	f->font = MakeFont(*desc);
	if (!f->font) {
		delete f;
		return E_FAIL;
	}
	f->ascent = (float)CTFontGetAscent(f->font);
	f->descent = (float)CTFontGetDescent(f->font);
	f->leading = (float)CTFontGetLeading(f->font);
	ID3DXFont *iface = new ID3DXFont();
	iface->m_font = f;
	*font = iface;
	return S_OK;
}

HRESULT D3DXCreateFontA(IDirect3DDevice9 *dev, INT height, UINT width, UINT weight, UINT mips, WINBOOL italic,
	DWORD charset, DWORD precision, DWORD quality, DWORD pitchAndFamily, const char *face, ID3DXFont **font)
{
	D3DXFONT_DESCA d = {};
	d.Height = height;
	d.Width = width;
	d.Weight = weight;
	d.MipLevels = mips;
	d.Italic = italic;
	d.CharSet = (BYTE)charset;
	d.OutputPrecision = (BYTE)precision;
	d.Quality = (BYTE)quality;
	d.PitchAndFamily = (BYTE)pitchAndFamily;
	if (face) strncpy(d.FaceName, face, sizeof(d.FaceName) - 1);
	return D3DXCreateFontIndirectA(dev, &d, font);
}

ID3DXFont::~ID3DXFont() { delete m_font; }

HRESULT ID3DXFont::GetDescA(D3DXFONT_DESCA *desc)
{
	if (!desc) return D3DERR_INVALIDCALL;
	*desc = m_font->desc;
	return S_OK;
}

WINBOOL ID3DXFont::GetTextMetricsW(TEXTMETRICW *tm)
{
	if (!tm) return FALSE;
	Font *f = m_font;
	memset(tm, 0, sizeof(*tm));
	tm->tmAscent = (LONG)ceilf(f->ascent);
	tm->tmDescent = (LONG)ceilf(f->descent);
	tm->tmHeight = tm->tmAscent + tm->tmDescent;
	tm->tmInternalLeading = std::max<LONG>(0, tm->tmHeight - (LONG)lrint(CTFontGetSize(f->font)));
	tm->tmExternalLeading = (LONG)ceilf(f->leading);
	UniChar ch = 'x';
	CGGlyph g;
	CGSize adv = {};
	if (CTFontGetGlyphsForCharacters(f->font, &ch, &g, 1)) CTFontGetAdvancesForGlyphs(f->font, kCTFontOrientationHorizontal, &g, &adv, 1);
	tm->tmAveCharWidth = (LONG)ceil(adv.width);
	tm->tmMaxCharWidth = (LONG)ceil(CTFontGetBoundingBox(f->font).size.width);
	tm->tmWeight = f->desc.Weight;
	tm->tmItalic = f->desc.Italic ? 1 : 0;
	tm->tmFirstChar = 32;
	tm->tmLastChar = 0xFFFF;
	tm->tmDefaultChar = '?';
	tm->tmBreakChar = ' ';
	tm->tmDigitizedAspectX = tm->tmDigitizedAspectY = 96;
	return TRUE;
}

static std::u16string ToU16(const wchar_t *text, INT count)
{
	std::u16string s;
	if (!text) return s;
	size_t n = count < 0 ? wcslen(text) : (size_t)count;
	for (size_t i = 0; i < n; i++) {
		uint32_t c = (uint32_t)text[i];
		if (c >= 0x10000) {
			c -= 0x10000;
			s.push_back((char16_t)(0xD800 + (c >> 10)));
			s.push_back((char16_t)(0xDC00 + (c & 0x3FF)));
		} else {
			s.push_back((char16_t)c);
		}
	}
	return s;
}

static CTFramesetterRef Framesetter(Font *f, const std::u16string &s, DWORD format)
{
	NSString *str = [NSString stringWithCharacters:(const unichar *)s.data() length:s.size()];
	if (format & DT_EXPANDTABS) str = [str stringByReplacingOccurrencesOfString:@"\t" withString:@"        "];
	CTTextAlignment align = (format & DT_CENTER) ? kCTTextAlignmentCenter : (format & DT_RIGHT) ? kCTTextAlignmentRight : kCTTextAlignmentLeft;
	CTLineBreakMode lb = (format & DT_WORDBREAK) && !(format & DT_SINGLELINE) ? kCTLineBreakByWordWrapping : kCTLineBreakByClipping;
	CTParagraphStyleSetting settings[] = {
		{kCTParagraphStyleSpecifierAlignment, sizeof(align), &align},
		{kCTParagraphStyleSpecifierLineBreakMode, sizeof(lb), &lb},
	};
	CTParagraphStyleRef ps = CTParagraphStyleCreate(settings, 2);
	NSDictionary *attrs = @{
		(NSString *)kCTFontAttributeName: (__bridge id)f->font,
		(NSString *)kCTParagraphStyleAttributeName: (__bridge id)ps,
		(NSString *)kCTForegroundColorFromContextAttributeName: @YES,
	};
	NSAttributedString *as = [[NSAttributedString alloc] initWithString:str attributes:attrs];
	CFRelease(ps);
	return CTFramesetterCreateWithAttributedString((__bridge CFAttributedStringRef)as);
}

static CGSize Measure(CTFramesetterRef fs, CGFloat width)
{
	return CTFramesetterSuggestFrameSizeWithConstraints(fs, CFRangeMake(0, 0), nullptr, CGSizeMake(width, CGFLOAT_MAX), nullptr);
}

INT ID3DXFont::DrawTextA(ID3DXSprite *sprite, const char *text, INT count, RECT *rect, DWORD format, D3DCOLOR color)
{
	if (!text) return 0;
	size_t n = count < 0 ? strlen(text) : (size_t)count;
	std::wstring w;
	NSString *s = [[NSString alloc] initWithBytes:text length:n encoding:NSUTF8StringEncoding];
	if (!s) s = [[NSString alloc] initWithBytes:text length:n encoding:NSWindowsCP1252StringEncoding];
	for (NSUInteger i = 0; i < s.length; i++) w.push_back((wchar_t)[s characterAtIndex:i]);
	return DrawTextW(sprite, w.c_str(), (INT)w.size(), rect, format, color);
}

INT ID3DXFont::DrawTextW(ID3DXSprite *, const wchar_t *text, INT count, RECT *rect, DWORD format, D3DCOLOR color)
{
	if (!rect) return 0;
	Font *f = m_font;
	@autoreleasepool {
		std::u16string s = ToU16(text, count);
		if (s.empty()) return 0;
		LONG rw = rect->right - rect->left, rh = rect->bottom - rect->top;
		bool wrap = (format & DT_WORDBREAK) && !(format & DT_SINGLELINE);
		CTFramesetterRef fs = Framesetter(f, s, format);
		CGSize sz = Measure(fs, wrap && rw > 0 ? rw : CGFLOAT_MAX);
		LONG tw = (LONG)ceil(sz.width), th = (LONG)ceil(sz.height);
		if (format & DT_SINGLELINE) th = std::max<LONG>(th, (LONG)ceilf(f->ascent + f->descent));
		if (format & DT_CALCRECT) {
			CFRelease(fs);
			if (!wrap || rw <= 0) rect->right = rect->left + tw;
			rect->bottom = rect->top + th;
			return th;
		}
		LONG bw = wrap && rw > 0 ? rw : tw;
		if (bw <= 0 || th <= 0) {
			CFRelease(fs);
			return th;
		}

		TextEntry *entry = nullptr;
		for (auto it = f->cache.begin(); it != f->cache.end(); ++it)
			if (it->text == s && it->format == (format & ~DT_NOCLIP) && it->w == bw) {
				f->cache.splice(f->cache.begin(), f->cache, it);
				entry = &f->cache.front();
				break;
			}
		if (!entry) {
			std::vector<uint8_t> px((size_t)bw * th);
			CGColorSpaceRef gray = CGColorSpaceCreateDeviceGray();
			CGContextRef ctx = CGBitmapContextCreate(px.data(), bw, th, 8, bw, gray, kCGImageAlphaNone);
			CGColorSpaceRelease(gray);
			CGContextSetGrayFillColor(ctx, 1.0, 1.0);
			CGContextSetShouldSmoothFonts(ctx, false);
			CGMutablePathRef path = CGPathCreateMutable();
			CGPathAddRect(path, nullptr, CGRectMake(0, 0, bw, th));
			CTFrameRef frame = CTFramesetterCreateFrame(fs, CFRangeMake(0, 0), path, nullptr);
			CTFrameDraw(frame, ctx);
			CFRelease(frame);
			CGPathRelease(path);
			CGContextRelease(ctx);
			MTLTextureDescriptor *td = [MTLTextureDescriptor texture2DDescriptorWithPixelFormat:MTLPixelFormatR8Unorm width:bw height:th mipmapped:NO];
			td.storageMode = MTLStorageModeShared;
			td.usage = MTLTextureUsageShaderRead;
			id<MTLTexture> t = [f->dev->mtl newTextureWithDescriptor:td];
			[t replaceRegion:MTLRegionMake2D(0, 0, bw, th) mipmapLevel:0 withBytes:px.data() bytesPerRow:bw];
			f->cache.push_front({s, format & ~DT_NOCLIP, bw, th, t, bw, th});
			entry = &f->cache.front();
			while (f->cache.size() > 256) f->cache.pop_back();
		}
		CFRelease(fs);

		LONG x = rect->left, y = rect->top;
		if (!wrap) {
			if (format & DT_CENTER) x = rect->left + (rw - bw) / 2;
			else if (format & DT_RIGHT) x = rect->right - bw;
		}
		if (format & DT_SINGLELINE) {
			if (format & DT_VCENTER) y = rect->top + (rh - th) / 2;
			else if (format & DT_BOTTOM) y = rect->bottom - th;
		}

		Device *d = f->dev;
		Surface *s0 = SurfImpl(d->rt[0]);
		if (!s0) return th;
		UINT W = s0->tex->LevelWidth(s0->level), H = s0->tex->LevelHeight(s0->level);
		id<MTLRenderCommandEncoder> enc = d->RenderEncoder();
		id<MTLRenderPipelineState> pso = d->OverlayPipeline(@"TextPS");
		if (!pso) return th;
		[enc setRenderPipelineState:pso];
		[enc setDepthStencilState:d->NoDepth()];
		[enc setCullMode:MTLCullModeNone];
		[enc setTriangleFillMode:MTLTriangleFillModeFill];
		[enc setDepthBias:0 slopeScale:0 clamp:0];
		MTLViewport v = {0, 0, (double)W, (double)H, 0, 1};
		[enc setViewport:v];
		LONG cl = 0, ct = 0, cr = (LONG)W, cb = (LONG)H;
		if (!(format & DT_NOCLIP)) {
			cl = std::max<LONG>(cl, rect->left); ct = std::max<LONG>(ct, rect->top);
			cr = std::min<LONG>(cr, rect->right); cb = std::min<LONG>(cb, rect->bottom);
		}
		if (cr <= cl || cb <= ct) return th;
		MTLScissorRect sr = {(NSUInteger)cl, (NSUInteger)ct, (NSUInteger)(cr - cl), (NSUInteger)(cb - ct)};
		[enc setScissorRect:sr];
		struct { float src[4]; float dst[4]; float depth; float pad[3]; } bp = {
			{0, 0, 1, 1},
			{(float)x / W, (float)y / H, (float)(x + entry->texW) / W, (float)(y + entry->texH) / H}, 0, {0, 0, 0}};
		[enc setVertexBytes:&bp length:sizeof(bp) atIndex:0];
		float c[4] = {((color >> 16) & 255) / 255.0f, ((color >> 8) & 255) / 255.0f, (color & 255) / 255.0f, ((color >> 24) & 255) / 255.0f};
		[enc setFragmentBytes:c length:sizeof(c) atIndex:0];
		[enc setFragmentTexture:entry->tex atIndex:0];
		[enc setFragmentSamplerState:d->blitPoint atIndex:0];
		[enc drawPrimitives:MTLPrimitiveTypeTriangleStrip vertexStart:0 vertexCount:4];
		return th;
	}
}
