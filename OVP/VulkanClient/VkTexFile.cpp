// not upstream: see VkTexFile.h

#include "VkTexFile.h"
#include "Log.h"
#include <QImage>
#include <QImageReader>
#include <QFile>
#include <algorithm>
#include <cstring>
#include <cmath>

typedef float RGBAf[4];

VkComponentMapping VkSwizzleMap (VkSwz swz)
{
	const VkComponentSwizzle R = VK_COMPONENT_SWIZZLE_R, G = VK_COMPONENT_SWIZZLE_G, B = VK_COMPONENT_SWIZZLE_B,
		A = VK_COMPONENT_SWIZZLE_A, ONE = VK_COMPONENT_SWIZZLE_ONE, ZERO = VK_COMPONENT_SWIZZLE_ZERO;
	switch (swz) {
	case SWZ_NOALPHA:  return { R, G, B, ONE };
	case SWZ_LUM:      return { R, R, R, ONE };
	case SWZ_ALPHA:    return { ZERO, ZERO, ZERO, R };
	case SWZ_LUMALPHA: return { R, R, R, G };
	default:           return { R, G, B, A };
	}
}

VkSwz VkSwizzleOf (const VkComponentMapping &m)
{
	if (m.r == VK_COMPONENT_SWIZZLE_ZERO) return SWZ_ALPHA;
	if (m.g == VK_COMPONENT_SWIZZLE_R) return (m.a == VK_COMPONENT_SWIZZLE_G) ? SWZ_LUMALPHA : SWZ_LUM;
	if (m.a == VK_COMPONENT_SWIZZLE_ONE) return SWZ_NOALPHA;
	return SWZ_NONE;
}

VkDeviceSize VkLevelSize (VkFormat fmt, UINT w, UINT h, UINT d)
{
	UINT bw;
	UINT bs = VkFormatBlockSize (fmt, &bw);
	return (VkDeviceSize)((w + bw - 1) / bw) * ((h + bw - 1) / bw) * bs * d;
}

static bool IsBC (VkFormat f)
{
	return f == VK_FORMAT_BC1_RGBA_UNORM_BLOCK || f == VK_FORMAT_BC1_RGB_UNORM_BLOCK || f == VK_FORMAT_BC2_UNORM_BLOCK || f == VK_FORMAT_BC3_UNORM_BLOCK;
}

// half floats

static float HalfToFloat (WORD h)
{
	UINT s = (h >> 15) & 1, e = (h >> 10) & 31, m = h & 1023;
	float v;
	if (e == 0) v = ldexpf ((float)m, -24);
	else if (e == 31) v = m ? NAN : INFINITY;
	else v = ldexpf ((float)(m | 1024), (int)e - 25);
	return s ? -v : v;
}

static WORD FloatToHalf (float f)
{
	UINT x;
	memcpy (&x, &f, 4);
	UINT s = (x >> 16) & 0x8000;
	int e = (int)((x >> 23) & 255) - 127 + 15;
	UINT m = x & 0x7FFFFF;
	if (((x >> 23) & 255) == 255) return (WORD)(s | 0x7C00 | (m ? 0x200 : 0));
	if (e >= 31) return (WORD)(s | 0x7C00);
	if (e <= 0) {
		if (e < -10) return (WORD)s;
		m |= 0x800000;
		return (WORD)(s | ((m >> (14 - e)) + ((m >> (13 - e)) & 1)));
	}
	UINT r = s | (e << 10) | (m >> 13);
	if (m & 0x1000) r++; // round
	return (WORD)r;
}

static inline BYTE ToByte (float v) { return (BYTE)std::clamp ((int)lroundf (v * 255.0f), 0, 255); }
static inline float Lum (const float *c) { return 0.299f * c[0] + 0.587f * c[1] + 0.114f * c[2]; }

// one pixel of an uncompressed format → RGBA as D3D samples it
static void DecodePixel (const BYTE *p, VkFormat fmt, VkSwz swz, float *c)
{
	WORD v16;
	c[0] = c[1] = c[2] = 0.0f; c[3] = 1.0f;
	switch (fmt) {
	case VK_FORMAT_B8G8R8A8_UNORM: c[0] = p[2]/255.0f; c[1] = p[1]/255.0f; c[2] = p[0]/255.0f; c[3] = p[3]/255.0f; break;
	case VK_FORMAT_R8G8B8A8_UNORM: c[0] = p[0]/255.0f; c[1] = p[1]/255.0f; c[2] = p[2]/255.0f; c[3] = p[3]/255.0f; break;
	case VK_FORMAT_R5G6B5_UNORM_PACK16:
		memcpy (&v16, p, 2);
		c[0] = (v16 >> 11)/31.0f; c[1] = ((v16 >> 5) & 63)/63.0f; c[2] = (v16 & 31)/31.0f; break;
	case VK_FORMAT_A1R5G5B5_UNORM_PACK16:
		memcpy (&v16, p, 2);
		c[3] = (float)(v16 >> 15); c[0] = ((v16 >> 10) & 31)/31.0f; c[1] = ((v16 >> 5) & 31)/31.0f; c[2] = (v16 & 31)/31.0f; break;
	case VK_FORMAT_R8_UNORM: c[0] = p[0]/255.0f; break;
	case VK_FORMAT_R8G8_UNORM: c[0] = p[0]/255.0f; c[1] = p[1]/255.0f; break;
	case VK_FORMAT_R16_UNORM: memcpy (&v16, p, 2); c[0] = v16/65535.0f; break;
	case VK_FORMAT_R16_SFLOAT: memcpy (&v16, p, 2); c[0] = HalfToFloat (v16); break;
	case VK_FORMAT_R16G16_SFLOAT: for (int i = 0; i < 2; i++) { memcpy (&v16, p + 2*i, 2); c[i] = HalfToFloat (v16); } break;
	case VK_FORMAT_R16G16B16A16_SFLOAT: for (int i = 0; i < 4; i++) { memcpy (&v16, p + 2*i, 2); c[i] = HalfToFloat (v16); } break;
	case VK_FORMAT_R16G16B16A16_UNORM: for (int i = 0; i < 4; i++) { memcpy (&v16, p + 2*i, 2); c[i] = v16/65535.0f; } break;
	case VK_FORMAT_R32_SFLOAT: memcpy (c, p, 4); break;
	case VK_FORMAT_R32G32_SFLOAT: memcpy (c, p, 8); break;
	case VK_FORMAT_R32G32B32A32_SFLOAT: memcpy (c, p, 16); break;
	default: break;
	}
	switch (swz) {
	case SWZ_NOALPHA: c[3] = 1.0f; break;
	case SWZ_LUM: c[1] = c[2] = c[0]; c[3] = 1.0f; break;
	case SWZ_ALPHA: c[3] = c[0]; c[0] = c[1] = c[2] = 0.0f; break;
	case SWZ_LUMALPHA: c[3] = c[1]; c[1] = c[2] = c[0]; break;
	default: break;
	}
}

// RGBA → one pixel of an uncompressed format
static void EncodePixel (const float *c, VkFormat fmt, VkSwz swz, BYTE *p)
{
	float v[4] = { c[0], c[1], c[2], c[3] };
	WORD v16;
	switch (swz) {
	case SWZ_NOALPHA: v[3] = 1.0f; break;
	case SWZ_LUM: v[0] = Lum (c); break;
	case SWZ_ALPHA: v[0] = c[3]; break;
	case SWZ_LUMALPHA: v[0] = Lum (c); v[1] = c[3]; break;
	default: break;
	}
	switch (fmt) {
	case VK_FORMAT_B8G8R8A8_UNORM: p[0] = ToByte (v[2]); p[1] = ToByte (v[1]); p[2] = ToByte (v[0]); p[3] = ToByte (v[3]); break;
	case VK_FORMAT_R8G8B8A8_UNORM: for (int i = 0; i < 4; i++) p[i] = ToByte (v[i]); break;
	case VK_FORMAT_R5G6B5_UNORM_PACK16:
		v16 = (WORD)((std::clamp ((int)lroundf (v[0]*31), 0, 31) << 11) | (std::clamp ((int)lroundf (v[1]*63), 0, 63) << 5) | std::clamp ((int)lroundf (v[2]*31), 0, 31));
		memcpy (p, &v16, 2); break;
	case VK_FORMAT_A1R5G5B5_UNORM_PACK16:
		v16 = (WORD)(((v[3] >= 0.5f) ? 0x8000 : 0) | (std::clamp ((int)lroundf (v[0]*31), 0, 31) << 10) | (std::clamp ((int)lroundf (v[1]*31), 0, 31) << 5) | std::clamp ((int)lroundf (v[2]*31), 0, 31));
		memcpy (p, &v16, 2); break;
	case VK_FORMAT_R8_UNORM: p[0] = ToByte (v[0]); break;
	case VK_FORMAT_R8G8_UNORM: p[0] = ToByte (v[0]); p[1] = ToByte (v[1]); break;
	case VK_FORMAT_R16_UNORM: v16 = (WORD)std::clamp ((int)lroundf (v[0]*65535), 0, 65535); memcpy (p, &v16, 2); break;
	case VK_FORMAT_R16_SFLOAT: v16 = FloatToHalf (v[0]); memcpy (p, &v16, 2); break;
	case VK_FORMAT_R16G16_SFLOAT: for (int i = 0; i < 2; i++) { v16 = FloatToHalf (v[i]); memcpy (p + 2*i, &v16, 2); } break;
	case VK_FORMAT_R16G16B16A16_SFLOAT: for (int i = 0; i < 4; i++) { v16 = FloatToHalf (v[i]); memcpy (p + 2*i, &v16, 2); } break;
	case VK_FORMAT_R16G16B16A16_UNORM: for (int i = 0; i < 4; i++) { v16 = (WORD)std::clamp ((int)lroundf (v[i]*65535), 0, 65535); memcpy (p + 2*i, &v16, 2); } break;
	case VK_FORMAT_R32_SFLOAT: memcpy (p, v, 4); break;
	case VK_FORMAT_R32G32_SFLOAT: memcpy (p, v, 8); break;
	case VK_FORMAT_R32G32B32A32_SFLOAT: memcpy (p, v, 16); break;
	default: break;
	}
}

// BC1/BC2/BC3 (DXT1/DXT3/DXT5) blocks

static void Unpack565 (WORD v, float *c)
{
	c[0] = (v >> 11) / 31.0f;
	c[1] = ((v >> 5) & 63) / 63.0f;
	c[2] = (v & 31) / 31.0f;
	c[3] = 1.0f;
}

static WORD Pack565 (const float *c)
{
	return (WORD)((std::clamp ((int)lroundf (c[0]*31), 0, 31) << 11) | (std::clamp ((int)lroundf (c[1]*63), 0, 63) << 5) | std::clamp ((int)lroundf (c[2]*31), 0, 31));
}

static void DecodeColorBlock (const BYTE *b, RGBAf *out, bool bc1)
{
	WORD c0 = (WORD)(b[0] | (b[1] << 8)), c1 = (WORD)(b[2] | (b[3] << 8));
	float col[4][4];
	Unpack565 (c0, col[0]);
	Unpack565 (c1, col[1]);
	if (!bc1 || c0 > c1) {
		for (int k = 0; k < 3; k++) {
			col[2][k] = (2.0f*col[0][k] + col[1][k]) / 3.0f;
			col[3][k] = (col[0][k] + 2.0f*col[1][k]) / 3.0f;
		}
		col[2][3] = col[3][3] = 1.0f;
	}
	else { // BC1 three colour mode: index 3 is transparent black
		for (int k = 0; k < 3; k++) { col[2][k] = (col[0][k] + col[1][k]) * 0.5f; col[3][k] = 0.0f; }
		col[2][3] = 1.0f; col[3][3] = 0.0f;
	}
	DWORD idx = b[4] | (b[5] << 8) | (b[6] << 16) | ((DWORD)b[7] << 24);
	for (int i = 0; i < 16; i++) memcpy (out[i], col[(idx >> (2*i)) & 3], 16);
}

static void DecodeBC3Alpha (const BYTE *b, RGBAf *out)
{
	float a[8];
	a[0] = b[0] / 255.0f;
	a[1] = b[1] / 255.0f;
	if (b[0] > b[1]) for (int i = 1; i < 7; i++) a[i+1] = ((7-i)*a[0] + i*a[1]) / 7.0f;
	else {
		for (int i = 1; i < 5; i++) a[i+1] = ((5-i)*a[0] + i*a[1]) / 5.0f;
		a[6] = 0.0f; a[7] = 1.0f;
	}
	uint64_t bits = 0;
	for (int i = 0; i < 6; i++) bits |= (uint64_t)b[2+i] << (8*i);
	for (int i = 0; i < 16; i++) out[i][3] = a[(bits >> (3*i)) & 7];
}

static void DecodeBlock (const BYTE *b, VkFormat fmt, RGBAf *out)
{
	if (fmt == VK_FORMAT_BC1_RGBA_UNORM_BLOCK || fmt == VK_FORMAT_BC1_RGB_UNORM_BLOCK) {
		DecodeColorBlock (b, out, true);
		if (fmt == VK_FORMAT_BC1_RGB_UNORM_BLOCK) for (int i = 0; i < 16; i++) out[i][3] = 1.0f;
	}
	else if (fmt == VK_FORMAT_BC2_UNORM_BLOCK) {
		DecodeColorBlock (b + 8, out, false);
		for (int i = 0; i < 16; i++) out[i][3] = ((b[i/2] >> (4*(i&1))) & 15) / 15.0f;
	}
	else if (fmt == VK_FORMAT_BC3_UNORM_BLOCK) {
		DecodeColorBlock (b + 8, out, false);
		DecodeBC3Alpha (b, out);
	}
}

// endpoints along the principal axis of the block's colours (range fit), indices to the nearest palette entry
static void EncodeColorBlock (const RGBAf *in, BYTE *b, bool bc1)
{
	bool transp = false;
	float mean[3] = { 0, 0, 0 };
	int n = 0;
	for (int i = 0; i < 16; i++) {
		if (bc1 && in[i][3] < 0.5f) { transp = true; continue; }
		for (int k = 0; k < 3; k++) mean[k] += in[i][k];
		n++;
	}
	if (n == 0) { // all transparent
		b[0] = b[1] = b[2] = b[3] = 0;
		b[4] = b[5] = b[6] = b[7] = 0xFF;
		return;
	}
	for (int k = 0; k < 3; k++) mean[k] /= n;
	float cov[6] = { 0, 0, 0, 0, 0, 0 };
	for (int i = 0; i < 16; i++) {
		if (bc1 && in[i][3] < 0.5f) continue;
		float d[3] = { in[i][0]-mean[0], in[i][1]-mean[1], in[i][2]-mean[2] };
		cov[0] += d[0]*d[0]; cov[1] += d[0]*d[1]; cov[2] += d[0]*d[2];
		cov[3] += d[1]*d[1]; cov[4] += d[1]*d[2]; cov[5] += d[2]*d[2];
	}
	float axis[3] = { 1, 1, 1 };
	for (int it = 0; it < 8; it++) { // power iteration
		float x = cov[0]*axis[0] + cov[1]*axis[1] + cov[2]*axis[2];
		float y = cov[1]*axis[0] + cov[3]*axis[1] + cov[4]*axis[2];
		float z = cov[2]*axis[0] + cov[4]*axis[1] + cov[5]*axis[2];
		float l = sqrtf (x*x + y*y + z*z);
		if (l < 1e-12f) break;
		axis[0] = x/l; axis[1] = y/l; axis[2] = z/l;
	}
	float tmin = 1e30f, tmax = -1e30f;
	for (int i = 0; i < 16; i++) {
		if (bc1 && in[i][3] < 0.5f) continue;
		float t = (in[i][0]-mean[0])*axis[0] + (in[i][1]-mean[1])*axis[1] + (in[i][2]-mean[2])*axis[2];
		tmin = std::min (tmin, t);
		tmax = std::max (tmax, t);
	}
	float e0[4], e1[4];
	for (int k = 0; k < 3; k++) {
		e0[k] = std::clamp (mean[k] + axis[k]*tmax, 0.0f, 1.0f);
		e1[k] = std::clamp (mean[k] + axis[k]*tmin, 0.0f, 1.0f);
	}
	WORD c0 = Pack565 (e0), c1 = Pack565 (e1);
	bool three = bc1 && transp; // three colour mode needs c0 <= c1
	if ((!three && c0 < c1) || (three && c0 > c1)) std::swap (c0, c1);
	float col[4][4];
	Unpack565 (c0, col[0]);
	Unpack565 (c1, col[1]);
	int ncol = 4;
	if (!three) {
		for (int k = 0; k < 3; k++) { col[2][k] = (2*col[0][k] + col[1][k]) / 3; col[3][k] = (col[0][k] + 2*col[1][k]) / 3; }
		if (c0 == c1) ncol = 1;
	}
	else {
		for (int k = 0; k < 3; k++) col[2][k] = (col[0][k] + col[1][k]) * 0.5f;
		ncol = 3;
	}
	DWORD idx = 0;
	for (int i = 0; i < 16; i++) {
		int best = 0;
		if (three && in[i][3] < 0.5f) best = 3;
		else {
			float bd = 1e30f;
			for (int j = 0; j < ncol; j++) {
				float d = 0;
				for (int k = 0; k < 3; k++) d += (in[i][k]-col[j][k]) * (in[i][k]-col[j][k]);
				if (d < bd) { bd = d; best = j; }
			}
		}
		idx |= (DWORD)best << (2*i);
	}
	b[0] = (BYTE)c0; b[1] = (BYTE)(c0 >> 8); b[2] = (BYTE)c1; b[3] = (BYTE)(c1 >> 8);
	b[4] = (BYTE)idx; b[5] = (BYTE)(idx >> 8); b[6] = (BYTE)(idx >> 16); b[7] = (BYTE)(idx >> 24);
}

static void EncodeBC3Alpha (const RGBAf *in, BYTE *b)
{
	float amin = 1.0f, amax = 0.0f;
	for (int i = 0; i < 16; i++) { amin = std::min (amin, in[i][3]); amax = std::max (amax, in[i][3]); }
	BYTE a0 = ToByte (amax), a1 = ToByte (amin);
	float a[8];
	a[0] = a0 / 255.0f; a[1] = a1 / 255.0f;
	int na = 2;
	if (a0 > a1) { for (int i = 1; i < 7; i++) a[i+1] = ((7-i)*a[0] + i*a[1]) / 7.0f; na = 8; }
	uint64_t bits = 0;
	for (int i = 0; i < 16; i++) {
		int best = 0;
		float bd = 1e30f;
		for (int j = 0; j < na; j++) { float d = fabsf (in[i][3] - a[j]); if (d < bd) { bd = d; best = j; } }
		bits |= (uint64_t)best << (3*i);
	}
	b[0] = a0; b[1] = a1;
	for (int i = 0; i < 6; i++) b[2+i] = (BYTE)(bits >> (8*i));
}

static void EncodeBlock (const RGBAf *in, VkFormat fmt, BYTE *b)
{
	if (fmt == VK_FORMAT_BC1_RGBA_UNORM_BLOCK || fmt == VK_FORMAT_BC1_RGB_UNORM_BLOCK) EncodeColorBlock (in, b, fmt == VK_FORMAT_BC1_RGBA_UNORM_BLOCK);
	else if (fmt == VK_FORMAT_BC2_UNORM_BLOCK) {
		for (int i = 0; i < 8; i++) {
			int lo = std::clamp ((int)lroundf (in[2*i][3]*15), 0, 15), hi = std::clamp ((int)lroundf (in[2*i+1][3]*15), 0, 15);
			b[i] = (BYTE)(lo | (hi << 4));
		}
		EncodeColorBlock (in, b + 8, false);
	}
	else if (fmt == VK_FORMAT_BC3_UNORM_BLOCK) {
		EncodeBC3Alpha (in, b);
		EncodeColorBlock (in, b + 8, false);
	}
}

// a level to/from RGBA floats
static void DecodeLevel (const BYTE *src, VkFormat fmt, VkSwz swz, UINT w, UINT h, std::vector<float> &out)
{
	out.assign ((size_t)w * h * 4, 0.0f);
	if (IsBC (fmt)) {
		UINT bs = VkFormatBlockSize (fmt);
		UINT bx = (w + 3) / 4, by = (h + 3) / 4;
		RGBAf blk[16];
		for (UINT y = 0; y < by; y++)
			for (UINT x = 0; x < bx; x++) {
				DecodeBlock (src + (y*bx + x)*bs, fmt, blk);
				for (UINT j = 0; j < 4; j++)
					for (UINT i = 0; i < 4; i++) {
						UINT px = x*4 + i, py = y*4 + j;
						if (px < w && py < h) memcpy (&out[((size_t)py*w + px)*4], blk[j*4 + i], 16);
					}
			}
		return;
	}
	UINT bs = VkFormatBlockSize (fmt);
	for (size_t i = 0; i < (size_t)w * h; i++) DecodePixel (src + i*bs, fmt, swz, &out[i*4]);
}

static void EncodeLevel (const std::vector<float> &in, UINT w, UINT h, VkFormat fmt, VkSwz swz, std::vector<BYTE> &out)
{
	out.assign (VkLevelSize (fmt, w, h), 0);
	if (IsBC (fmt)) {
		UINT bs = VkFormatBlockSize (fmt);
		UINT bx = (w + 3) / 4, by = (h + 3) / 4;
		RGBAf blk[16];
		for (UINT y = 0; y < by; y++)
			for (UINT x = 0; x < bx; x++) {
				for (UINT j = 0; j < 4; j++)
					for (UINT i = 0; i < 4; i++) {
						UINT px = std::min (x*4 + i, w - 1), py = std::min (y*4 + j, h - 1); // edge blocks repeat the border
						memcpy (blk[j*4 + i], &in[((size_t)py*w + px)*4], 16);
					}
				EncodeBlock (blk, fmt, &out[(y*bx + x)*bs]);
			}
		return;
	}
	UINT bs = VkFormatBlockSize (fmt);
	for (size_t i = 0; i < (size_t)w * h; i++) EncodePixel (&in[i*4], fmt, swz, &out[i*bs]);
}

// bilinear resampling (box average when halving)
static void Resample (const std::vector<float> &in, UINT w, UINT h, std::vector<float> &out, UINT nw, UINT nh)
{
	out.assign ((size_t)nw * nh * 4, 0.0f);
	if (nw * 2 == w && nh * 2 == h) {
		for (UINT y = 0; y < nh; y++)
			for (UINT x = 0; x < nw; x++)
				for (int k = 0; k < 4; k++)
					out[((size_t)y*nw + x)*4 + k] = 0.25f * (in[((size_t)(2*y)*w + 2*x)*4 + k] + in[((size_t)(2*y)*w + 2*x+1)*4 + k] +
						in[((size_t)(2*y+1)*w + 2*x)*4 + k] + in[((size_t)(2*y+1)*w + 2*x+1)*4 + k]);
		return;
	}
	for (UINT y = 0; y < nh; y++) {
		float fy = std::clamp ((y + 0.5f) * h / nh - 0.5f, 0.0f, (float)(h - 1));
		UINT y0 = (UINT)fy, y1 = std::min (y0 + 1, h - 1);
		float ty = fy - y0;
		for (UINT x = 0; x < nw; x++) {
			float fx = std::clamp ((x + 0.5f) * w / nw - 0.5f, 0.0f, (float)(w - 1));
			UINT x0 = (UINT)fx, x1 = std::min (x0 + 1, w - 1);
			float tx = fx - x0;
			for (int k = 0; k < 4; k++) {
				float a = in[((size_t)y0*w + x0)*4 + k] * (1-tx) + in[((size_t)y0*w + x1)*4 + k] * tx;
				float b = in[((size_t)y1*w + x0)*4 + k] * (1-tx) + in[((size_t)y1*w + x1)*4 + k] * tx;
				out[((size_t)y*nw + x)*4 + k] = a * (1-ty) + b * ty;
			}
		}
	}
}

// DDS files

#pragma pack(push, 1)
struct DDSPixelFormat { DWORD size, flags, fourCC, rgbBits, rMask, gMask, bMask, aMask; };
struct DDSHeader {
	DWORD magic, size, flags, height, width, pitch, depth, mipCount, reserved1[11];
	DDSPixelFormat pf;
	DWORD caps, caps2, caps3, caps4, reserved2;
};
struct DDSHeader10 { DWORD dxgiFormat, dimension, miscFlag, arraySize, miscFlags2; };
#pragma pack(pop)

#define FOURCC(a,b,c,d) ((DWORD)(a) | ((DWORD)(b) << 8) | ((DWORD)(c) << 16) | ((DWORD)(d) << 24))
#define DDPF_ALPHAPIXELS 0x1
#define DDPF_ALPHA 0x2
#define DDPF_FOURCC 0x4
#define DDPF_RGB 0x40
#define DDPF_LUMINANCE 0x20000
#define DDSD_MIPMAPCOUNT 0x20000
#define DDSCAPS2_CUBEMAP 0x200
#define DDSCAPS2_VOLUME 0x200000

// stored layouts that are expanded while loading
enum DDSExpand { DDS_AS_IS, DDS_RGB24, DDS_4444 };

static bool DDSFormat (const DDSHeader &h, const DDSHeader10 *h10, VkFormat *fmt, VkSwz *swz, DDSExpand *ex)
{
	const DDSPixelFormat &pf = h.pf;
	*swz = SWZ_NONE;
	*ex = DDS_AS_IS;
	*fmt = VK_FORMAT_UNDEFINED;
	if (pf.flags & DDPF_FOURCC) {
		switch (pf.fourCC) {
		case FOURCC('D','X','T','1'): *fmt = VK_FORMAT_BC1_RGBA_UNORM_BLOCK; break;
		case FOURCC('D','X','T','2'): case FOURCC('D','X','T','3'): *fmt = VK_FORMAT_BC2_UNORM_BLOCK; break;
		case FOURCC('D','X','T','4'): case FOURCC('D','X','T','5'): *fmt = VK_FORMAT_BC3_UNORM_BLOCK; break;
		case 36:  *fmt = VK_FORMAT_R16G16B16A16_UNORM; break;  // D3DFMT_A16B16G16R16
		case 111: *fmt = VK_FORMAT_R16_SFLOAT; break;          // D3DFMT_R16F
		case 112: *fmt = VK_FORMAT_R16G16_SFLOAT; break;       // D3DFMT_G16R16F
		case 113: *fmt = VK_FORMAT_R16G16B16A16_SFLOAT; break; // D3DFMT_A16B16G16R16F
		case 114: *fmt = VK_FORMAT_R32_SFLOAT; break;          // D3DFMT_R32F
		case 115: *fmt = VK_FORMAT_R32G32_SFLOAT; break;       // D3DFMT_G32R32F
		case 116: *fmt = VK_FORMAT_R32G32B32A32_SFLOAT; break; // D3DFMT_A32B32G32R32F
		case FOURCC('D','X','1','0'):
			if (!h10) return false;
			switch (h10->dxgiFormat) {
			case 71: *fmt = VK_FORMAT_BC1_RGBA_UNORM_BLOCK; break;
			case 74: *fmt = VK_FORMAT_BC2_UNORM_BLOCK; break;
			case 77: *fmt = VK_FORMAT_BC3_UNORM_BLOCK; break;
			case 28: *fmt = VK_FORMAT_R8G8B8A8_UNORM; break;
			case 87: *fmt = VK_FORMAT_B8G8R8A8_UNORM; break;
			case 88: *fmt = VK_FORMAT_B8G8R8A8_UNORM; *swz = SWZ_NOALPHA; break;
			case 10: *fmt = VK_FORMAT_R16G16B16A16_SFLOAT; break;
			case 2:  *fmt = VK_FORMAT_R32G32B32A32_SFLOAT; break;
			case 41: *fmt = VK_FORMAT_R32_SFLOAT; break;
			case 54: *fmt = VK_FORMAT_R16_SFLOAT; break;
			case 61: *fmt = VK_FORMAT_R8_UNORM; break;
			case 85: *fmt = VK_FORMAT_R5G6B5_UNORM_PACK16; break;
			}
			break;
		}
	}
	else if (pf.flags & DDPF_RGB) {
		bool alpha = (pf.flags & DDPF_ALPHAPIXELS) && pf.aMask;
		if (pf.rgbBits == 32 && pf.rMask == 0xFF0000 && pf.gMask == 0xFF00 && pf.bMask == 0xFF) *fmt = VK_FORMAT_B8G8R8A8_UNORM;
		else if (pf.rgbBits == 32 && pf.rMask == 0xFF && pf.gMask == 0xFF00 && pf.bMask == 0xFF0000) *fmt = VK_FORMAT_R8G8B8A8_UNORM;
		else if (pf.rgbBits == 24) { *fmt = VK_FORMAT_B8G8R8A8_UNORM; *ex = DDS_RGB24; }
		else if (pf.rgbBits == 16 && pf.rMask == 0xF800) *fmt = VK_FORMAT_R5G6B5_UNORM_PACK16;
		else if (pf.rgbBits == 16 && pf.rMask == 0x7C00) *fmt = VK_FORMAT_A1R5G5B5_UNORM_PACK16;
		else if (pf.rgbBits == 16 && pf.rMask == 0xF00) { *fmt = VK_FORMAT_B8G8R8A8_UNORM; *ex = DDS_4444; } // not supported as a texture format (upstream converts too)
		if (!alpha && *fmt != VK_FORMAT_R5G6B5_UNORM_PACK16) *swz = SWZ_NOALPHA;
	}
	else if (pf.flags & DDPF_LUMINANCE) {
		if (pf.rgbBits == 8) { *fmt = VK_FORMAT_R8_UNORM; *swz = SWZ_LUM; }
		else if (pf.rgbBits == 16 && pf.aMask) { *fmt = VK_FORMAT_R8G8_UNORM; *swz = SWZ_LUMALPHA; }
		else if (pf.rgbBits == 16) { *fmt = VK_FORMAT_R16_UNORM; *swz = SWZ_LUM; }
	}
	else if (pf.flags & DDPF_ALPHA) {
		if (pf.rgbBits == 8) { *fmt = VK_FORMAT_R8_UNORM; *swz = SWZ_ALPHA; }
	}
	return *fmt != VK_FORMAT_UNDEFINED;
}

static bool LoadDDS (const BYTE *buf, size_t n, VkPixels &px, VkImageInfo *info)
{
	if (n < sizeof(DDSHeader)) return false;
	DDSHeader h;
	memcpy (&h, buf, sizeof(h));
	if (h.magic != FOURCC('D','D','S',' ') || h.size != 124) return false;
	size_t ofs = sizeof(DDSHeader);
	DDSHeader10 h10;
	bool dx10 = (h.pf.flags & DDPF_FOURCC) && h.pf.fourCC == FOURCC('D','X','1','0');
	if (dx10) {
		if (n < ofs + sizeof(h10)) return false;
		memcpy (&h10, buf + ofs, sizeof(h10));
		ofs += sizeof(h10);
	}
	VkFormat fmt;
	VkSwz swz;
	DDSExpand ex;
	if (!DDSFormat (h, dx10 ? &h10 : NULL, &fmt, &swz, &ex)) {
		LogErr("DDS: pixel format not handled (flags 0x%X fourCC 0x%X bits %u)", h.pf.flags, h.pf.fourCC, h.pf.rgbBits);
		return false;
	}
	UINT levels = ((h.flags & DDSD_MIPMAPCOUNT) && h.mipCount) ? h.mipCount : 1;
	bool cube = (h.caps2 & DDSCAPS2_CUBEMAP) != 0;
	UINT depth = (h.caps2 & DDSCAPS2_VOLUME) ? std::max (1u, h.depth) : 1;
	UINT layers = cube ? 6 : 1;
	if (info) {
		info->Width = h.width; info->Height = h.height; info->Depth = depth; info->MipLevels = levels;
		info->Format = fmt; info->Swizzle = swz; info->Cube = cube; info->ImageFileFormat = VKIFF_DDS;
		return true;
	}
	px.w = h.width; px.h = h.height; px.depth = depth; px.levels = levels; px.layers = layers;
	px.fmt = fmt; px.swz = swz;
	px.data.assign (layers * levels, std::vector<BYTE>());
	for (UINT f = 0; f < layers; f++)
		for (UINT l = 0; l < levels; l++) {
			UINT w = std::max (1u, px.w >> l), hh = std::max (1u, px.h >> l), d = std::max (1u, depth >> l);
			size_t sz = (ex == DDS_RGB24) ? (size_t)w*hh*d*3 : (ex == DDS_4444) ? (size_t)w*hh*d*2 : (size_t)VkLevelSize (fmt, w, hh, d);
			if (ofs + sz > n) { LogErr("DDS: file truncated"); return false; }
			std::vector<BYTE> &out = px.Level (l, f);
			if (ex == DDS_AS_IS) out.assign (buf + ofs, buf + ofs + sz);
			else {
				out.resize ((size_t)w*hh*d*4);
				for (size_t i = 0; i < (size_t)w*hh*d; i++) {
					if (ex == DDS_RGB24) { out[i*4] = buf[ofs + i*3]; out[i*4+1] = buf[ofs + i*3+1]; out[i*4+2] = buf[ofs + i*3+2]; out[i*4+3] = 255; }
					else {
						WORD v = (WORD)(buf[ofs + i*2] | (buf[ofs + i*2+1] << 8));
						out[i*4] = (BYTE)((v & 15) * 17); out[i*4+1] = (BYTE)(((v >> 4) & 15) * 17);
						out[i*4+2] = (BYTE)(((v >> 8) & 15) * 17); out[i*4+3] = (BYTE)(((v >> 12) & 15) * 17);
					}
				}
			}
			ofs += sz;
		}
	return true;
}

static bool SaveDDS (const char *path, const VkPixels &px)
{
	DDSHeader h;
	memset (&h, 0, sizeof(h));
	h.magic = FOURCC('D','D','S',' ');
	h.size = 124;
	h.flags = 0x1 | 0x2 | 0x4 | 0x1000 | (px.levels > 1 ? DDSD_MIPMAPCOUNT : 0); // CAPS HEIGHT WIDTH PIXELFORMAT
	h.width = px.w; h.height = px.h; h.mipCount = px.levels;
	h.depth = px.depth;
	h.pf.size = 32;
	h.caps = 0x1000 | (px.levels > 1 ? 0x400008 : 0); // TEXTURE, MIPMAP|COMPLEX
	if (px.layers == 6) { h.caps |= 0x8; h.caps2 = DDSCAPS2_CUBEMAP | 0xFC00; }
	if (px.depth > 1) { h.caps2 |= DDSCAPS2_VOLUME; h.flags |= 0x800000; }
	auto rgb = [&](DWORD bits, DWORD r, DWORD g, DWORD b, DWORD a) {
		h.pf.flags = DDPF_RGB | (a ? DDPF_ALPHAPIXELS : 0); h.pf.rgbBits = bits;
		h.pf.rMask = r; h.pf.gMask = g; h.pf.bMask = b; h.pf.aMask = a;
	};
	bool na = (px.swz == SWZ_NOALPHA);
	switch (px.fmt) {
	case VK_FORMAT_BC1_RGBA_UNORM_BLOCK: case VK_FORMAT_BC1_RGB_UNORM_BLOCK: h.pf.flags = DDPF_FOURCC; h.pf.fourCC = FOURCC('D','X','T','1'); break;
	case VK_FORMAT_BC2_UNORM_BLOCK: h.pf.flags = DDPF_FOURCC; h.pf.fourCC = FOURCC('D','X','T','3'); break;
	case VK_FORMAT_BC3_UNORM_BLOCK: h.pf.flags = DDPF_FOURCC; h.pf.fourCC = FOURCC('D','X','T','5'); break;
	case VK_FORMAT_B8G8R8A8_UNORM: rgb (32, 0xFF0000, 0xFF00, 0xFF, na ? 0 : 0xFF000000); break;
	case VK_FORMAT_R8G8B8A8_UNORM: rgb (32, 0xFF, 0xFF00, 0xFF0000, na ? 0 : 0xFF000000); break;
	case VK_FORMAT_R5G6B5_UNORM_PACK16: rgb (16, 0xF800, 0x7E0, 0x1F, 0); break;
	case VK_FORMAT_A1R5G5B5_UNORM_PACK16: rgb (16, 0x7C00, 0x3E0, 0x1F, na ? 0 : 0x8000); break;
	case VK_FORMAT_R8_UNORM:
		if (px.swz == SWZ_ALPHA) { h.pf.flags = DDPF_ALPHA; h.pf.rgbBits = 8; h.pf.aMask = 0xFF; }
		else { h.pf.flags = DDPF_LUMINANCE; h.pf.rgbBits = 8; h.pf.rMask = 0xFF; }
		break;
	case VK_FORMAT_R8G8_UNORM: h.pf.flags = DDPF_LUMINANCE | DDPF_ALPHAPIXELS; h.pf.rgbBits = 16; h.pf.rMask = 0xFF; h.pf.aMask = 0xFF00; break;
	case VK_FORMAT_R16_UNORM: h.pf.flags = DDPF_LUMINANCE; h.pf.rgbBits = 16; h.pf.rMask = 0xFFFF; break;
	case VK_FORMAT_R16G16B16A16_UNORM: h.pf.flags = DDPF_FOURCC; h.pf.fourCC = 36; break;
	case VK_FORMAT_R16_SFLOAT: h.pf.flags = DDPF_FOURCC; h.pf.fourCC = 111; break;
	case VK_FORMAT_R16G16_SFLOAT: h.pf.flags = DDPF_FOURCC; h.pf.fourCC = 112; break;
	case VK_FORMAT_R16G16B16A16_SFLOAT: h.pf.flags = DDPF_FOURCC; h.pf.fourCC = 113; break;
	case VK_FORMAT_R32_SFLOAT: h.pf.flags = DDPF_FOURCC; h.pf.fourCC = 114; break;
	case VK_FORMAT_R32G32_SFLOAT: h.pf.flags = DDPF_FOURCC; h.pf.fourCC = 115; break;
	case VK_FORMAT_R32G32B32A32_SFLOAT: h.pf.flags = DDPF_FOURCC; h.pf.fourCC = 116; break;
	default: LogErr("DDS: can't write format %d", (int)px.fmt); return false;
	}
	FILE *f = fopen (path, "wb");
	if (!f) return false;
	fwrite (&h, sizeof(h), 1, f);
	for (auto &d : px.data) fwrite (d.data(), 1, d.size(), f);
	fclose (f);
	return true;
}

// other files through QImage (B8G8R8A8 in memory, as D3DX's A8R8G8B8 / X8R8G8B8)

static VkImageFileFormat FileFormat (const QByteArray &f)
{
	if (f == "jpg" || f == "jpeg") return VKIFF_JPG;
	if (f == "png") return VKIFF_PNG;
	if (f == "tga") return VKIFF_TGA;
	return VKIFF_BMP;
}

static bool LoadQImage (const QImage &src, VkPixels &px)
{
	if (src.isNull()) return false;
	QImage img = src.convertToFormat (QImage::Format_ARGB32);
	px.w = img.width(); px.h = img.height(); px.depth = 1; px.levels = 1; px.layers = 1;
	px.fmt = VK_FORMAT_B8G8R8A8_UNORM;
	px.swz = src.hasAlphaChannel() ? SWZ_NONE : SWZ_NOALPHA;
	px.data.assign (1, std::vector<BYTE>((size_t)px.w * px.h * 4));
	for (UINT y = 0; y < px.h; y++) memcpy (&px.data[0][(size_t)y * px.w * 4], img.constScanLine (y), (size_t)px.w * 4);
	return true;
}

bool VkLoadPixelsFromMemory (const BYTE *buf, size_t n, VkPixels &px)
{
	if (n >= 4 && memcmp (buf, "DDS ", 4) == 0) return LoadDDS (buf, n, px, NULL);
	QImage img;
	if (!img.loadFromData (buf, (int)n)) return false;
	return LoadQImage (img, px);
}

bool VkLoadPixels (const char *path, VkPixels &px)
{
	QFile f (QString::fromUtf8 (path));
	if (!f.open (QIODevice::ReadOnly)) return false;
	QByteArray b = f.readAll ();
	return VkLoadPixelsFromMemory ((const BYTE *)b.constData(), b.size(), px);
}

bool VkGetImageInfoFromFile (const char *path, VkImageInfo *info)
{
	QFile f (QString::fromUtf8 (path));
	if (!f.open (QIODevice::ReadOnly)) return false;
	QByteArray b = f.read (sizeof(DDSHeader) + sizeof(DDSHeader10));
	VkPixels dummy;
	if (b.size() >= 4 && memcmp (b.constData(), "DDS ", 4) == 0)
		return LoadDDS ((const BYTE *)b.constData(), b.size(), dummy, info);
	f.close ();
	QImageReader r (QString::fromUtf8 (path));
	if (!r.canRead()) return false;
	QSize s = r.size ();
	info->Width = s.width(); info->Height = s.height(); info->Depth = 1; info->MipLevels = 1;
	info->Format = VK_FORMAT_B8G8R8A8_UNORM;
	info->Swizzle = SWZ_NOALPHA;
	info->Cube = false;
	info->ImageFileFormat = FileFormat (r.format());
	return true;
}

bool VkSavePixels (const char *path, VkImageFileFormat fmt, const VkPixels &px)
{
	if (fmt == VKIFF_DDS) return SaveDDS (path, px);
	std::vector<float> rgba;
	DecodeLevel (px.Level (0).data(), px.fmt, px.swz, px.w, px.h, rgba);
	QImage img (px.w, px.h, px.swz == SWZ_NOALPHA || fmt == VKIFF_JPG || fmt == VKIFF_BMP ? QImage::Format_RGB32 : QImage::Format_ARGB32);
	for (UINT y = 0; y < px.h; y++) {
		BYTE *d = img.scanLine (y);
		for (UINT x = 0; x < px.w; x++) EncodePixel (&rgba[((size_t)y*px.w + x)*4], VK_FORMAT_B8G8R8A8_UNORM, SWZ_NONE, d + x*4);
	}
	const char *q = fmt == VKIFF_JPG ? "JPG" : fmt == VKIFF_PNG ? "PNG" : fmt == VKIFF_TGA ? "TGA" : "BMP";
	return img.save (QString::fromUtf8 (path), q);
}

// conversions

bool VkConvertPixels (const VkPixels &in, VkPixels &out, VkFormat fmt, VkSwz swz, UINT w, UINT h, UINT levels)
{
	if (in.data.empty()) return false;
	if (w == 0 || w == VKTEX_FROM_FILE) w = in.w;
	if (h == 0 || h == VKTEX_FROM_FILE) h = in.h;
	if (fmt == VK_FORMAT_UNDEFINED) { fmt = in.fmt; swz = in.swz; }
	UINT full = 1;
	for (UINT d = std::max (w, h); d > 1; d >>= 1) full++;
	if (levels == VKTEX_FROM_FILE) levels = (w == in.w && h == in.h) ? in.levels : 1;
	if (levels == 0 || levels > full) levels = full;

	if (fmt == in.fmt && swz == in.swz && w == in.w && h == in.h && levels <= in.levels) { // levels as stored
		out.w = w; out.h = h; out.depth = in.depth; out.levels = levels; out.layers = in.layers;
		out.fmt = fmt; out.swz = swz;
		out.data.assign (out.layers * levels, std::vector<BYTE>());
		for (UINT f = 0; f < in.layers; f++)
			for (UINT l = 0; l < levels; l++) out.Level (l, f) = in.Level (l, f);
		return true;
	}
	if (in.depth > 1) { LogErr("VkConvertPixels: volume textures are kept as stored"); return false; }

	out.w = w; out.h = h; out.depth = 1; out.levels = levels; out.layers = in.layers;
	out.fmt = fmt; out.swz = swz;
	out.data.assign (out.layers * levels, std::vector<BYTE>());
	std::vector<float> cur, next;
	for (UINT f = 0; f < in.layers; f++) {
		UINT lw = w, lh = h;
		for (UINT l = 0; l < levels; l++) {
			UINT iw = std::max (1u, in.w >> l), ih = std::max (1u, in.h >> l);
			if (l < in.levels && iw == lw && ih == lh) DecodeLevel (in.Level (l, f).data(), in.fmt, in.swz, lw, lh, cur); // stored level
			else if (l == 0) {
				std::vector<float> src;
				DecodeLevel (in.Level (0, f).data(), in.fmt, in.swz, in.w, in.h, src);
				Resample (src, in.w, in.h, cur, lw, lh);
			}
			else { // generated from the level above (D3DX_FILTER_BOX)
				UINT pw = std::max (1u, w >> (l-1)), ph = std::max (1u, h >> (l-1));
				Resample (next, pw, ph, cur, lw, lh);
			}
			EncodeLevel (cur, lw, lh, fmt, swz, out.Level (l, f));
			next.swap (cur);
			lw = std::max (1u, lw >> 1);
			lh = std::max (1u, lh >> 1);
		}
	}
	return true;
}

// textures

VkTex *VkCreateTexture (VkDev *dev, const VkPixels &px, VkImageUsageFlags usage)
{
	usage |= VK_IMAGE_USAGE_TRANSFER_DST_BIT;
	VkTex *t = (px.depth > 1) ? new VkTex (dev, px.fmt, px.w, px.h, px.depth, usage)
		: new VkTex (dev, px.w, px.h, px.levels, px.fmt, usage, px.layers, px.layers == 6);
	if (!t->img) { delete t; return NULL; }
	if (px.swz != SWZ_NONE) t->SetSwizzle (VkSwizzleMap (px.swz));

	// one staging buffer and one submit for all levels and faces
	VkDeviceSize total = 0;
	for (auto &d : px.data) total += (d.size() + 15) & ~15ull;
	VkBuf staging (dev, std::max (total, (VkDeviceSize)16), VK_BUFFER_USAGE_TRANSFER_SRC_BIT, true);
	std::vector<VkBufferImageCopy> regions;
	VkDeviceSize ofs = 0;
	UINT nl = (px.depth > 1) ? 1 : px.levels;
	for (UINT f = 0; f < px.layers; f++)
		for (UINT l = 0; l < nl; l++) {
			const std::vector<BYTE> &d = px.Level (l, f);
			memcpy ((BYTE *)staging.Map() + ofs, d.data(), d.size());
			VkBufferImageCopy r = {};
			r.bufferOffset = ofs;
			r.imageSubresource = { t->Aspect(), l, f, 1 };
			r.imageExtent = { std::max (1u, px.w >> l), std::max (1u, px.h >> l), std::max (1u, px.depth >> l) };
			regions.push_back (r);
			ofs += (d.size() + 15) & ~15ull;
		}
	VkCommandBuffer cmd = dev->BeginOneTime ();
	t->Transition (cmd, VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL);
	vkCmdCopyBufferToImage (cmd, staging.buf, t->img, VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, (UINT)regions.size(), regions.data());
	t->Transition (cmd, VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL);
	dev->EndOneTime (cmd);
	return t;
}

VkTex *VkCreateTextureFromFile (VkDev *dev, const char *path, UINT w, UINT h, UINT mips, VkFormat fmt, VkSwz swz,
	VkImageUsageFlags usage, VkImageInfo *info)
{
	VkPixels px, cv;
	if (!VkLoadPixels (path, px)) return NULL;
	if (info) {
		info->Width = px.w; info->Height = px.h; info->Depth = px.depth; info->MipLevels = px.levels;
		info->Format = px.fmt; info->Swizzle = px.swz; info->Cube = px.layers == 6;
		info->ImageFileFormat = VKIFF_DDS;
	}
	if (px.depth > 1) return VkCreateTexture (dev, px, usage);
	if (!VkConvertPixels (px, cv, fmt, swz, w, h, mips)) return NULL;
	return VkCreateTexture (dev, cv, usage);
}

bool VkLoadTextureLevel (VkTex *t, UINT level, UINT layer, const VkPixels &src)
{
	UINT w = std::max (1u, t->w >> level), h = std::max (1u, t->h >> level);
	VkPixels cv;
	VkSwz swz = (src.fmt == t->fmt) ? src.swz : SWZ_NONE;
	if (!VkConvertPixels (src, cv, t->fmt, swz, w, h, 1)) return false;
	const std::vector<BYTE> &d = cv.Level (0);
	t->Upload (level, layer, d.data(), d.size());
	return true;
}

// readback

bool VkReadPixels (VkDev *dev, VkTex *t, VkPixels &px, UINT levels)
{
	if (!t || !t->img) return false;
	if (dev->IsRecording ()) dev->Flush (); // the frame's commands that wrote the image
	VkTex *src = t;
	VkTex *tmp = NULL;
	if (levels == 0 || levels > t->levels) levels = t->levels;
	if (t->samples != VK_SAMPLE_COUNT_1_BIT) { // resolve first, as D3D9 did for multisampled render targets
		tmp = new VkTex (dev, t->w, t->h, 1, t->fmt, VK_IMAGE_USAGE_TRANSFER_DST_BIT | VK_IMAGE_USAGE_TRANSFER_SRC_BIT);
		VkCommandBuffer cmd = dev->BeginOneTime ();
		VkImageLayout keep = t->layout;
		t->Transition (cmd, VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL);
		tmp->Transition (cmd, VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL);
		VkImageResolve r = {};
		r.srcSubresource = { VK_IMAGE_ASPECT_COLOR_BIT, 0, 0, 1 };
		r.dstSubresource = { VK_IMAGE_ASPECT_COLOR_BIT, 0, 0, 1 };
		r.extent = { t->w, t->h, 1 };
		vkCmdResolveImage (cmd, t->img, VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL, tmp->img, VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, 1, &r);
		if (keep != VK_IMAGE_LAYOUT_UNDEFINED) t->Transition (cmd, keep);
		dev->EndOneTime (cmd);
		src = tmp;
		levels = 1;
	}
	px.w = src->w; px.h = src->h; px.depth = src->depth; px.levels = levels; px.layers = src->layers;
	px.fmt = src->fmt; px.swz = SWZ_NONE;
	px.data.assign (px.layers * levels, std::vector<BYTE>());
	VkDeviceSize total = 0;
	std::vector<VkBufferImageCopy> regions;
	for (UINT f = 0; f < px.layers; f++)
		for (UINT l = 0; l < levels; l++) {
			VkBufferImageCopy r = {};
			r.bufferOffset = total;
			r.imageSubresource = { src->Aspect(), l, f, 1 };
			r.imageExtent = { std::max (1u, px.w >> l), std::max (1u, px.h >> l), std::max (1u, px.depth >> l) };
			regions.push_back (r);
			total += (VkLevelSize (px.fmt, r.imageExtent.width, r.imageExtent.height, r.imageExtent.depth) + 15) & ~15ull;
		}
	VkBuf staging (dev, total, VK_BUFFER_USAGE_TRANSFER_DST_BIT, true, true);
	VkCommandBuffer cmd = dev->BeginOneTime ();
	VkImageLayout keep = src->layout;
	src->Transition (cmd, VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL);
	vkCmdCopyImageToBuffer (cmd, src->img, VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL, staging.buf, (UINT)regions.size(), regions.data());
	if (keep != VK_IMAGE_LAYOUT_UNDEFINED) src->Transition (cmd, keep);
	dev->EndOneTime (cmd);
	UINT i = 0;
	for (UINT f = 0; f < px.layers; f++)
		for (UINT l = 0; l < levels; l++, i++) {
			const VkExtent3D &e = regions[i].imageExtent;
			const BYTE *p = (const BYTE *)staging.Map() + regions[i].bufferOffset;
			px.Level (l, f).assign (p, p + VkLevelSize (px.fmt, e.width, e.height, e.depth));
		}
	delete tmp;
	return true;
}
