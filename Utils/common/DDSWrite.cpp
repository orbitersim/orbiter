// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#include "DDSWrite.h"
#include <algorithm>
#include <cmath>
#include <cstring>

namespace ddsw {

namespace {

struct Vec3 {
	float r, g, b;
};

inline Vec3 operator+(Vec3 a, Vec3 b) { return {a.r + b.r, a.g + b.g, a.b + b.b}; }
inline Vec3 operator-(Vec3 a, Vec3 b) { return {a.r - b.r, a.g - b.g, a.b - b.b}; }
inline Vec3 operator*(Vec3 a, float s) { return {a.r * s, a.g * s, a.b * s}; }
inline float Dot(Vec3 a, Vec3 b) { return a.r * b.r + a.g * b.g + a.b * b.b; }

uint16_t Pack565(Vec3 c)
{
	int r = std::clamp((int)lrintf(c.r * 31.0f / 255.0f), 0, 31);
	int g = std::clamp((int)lrintf(c.g * 63.0f / 255.0f), 0, 63);
	int b = std::clamp((int)lrintf(c.b * 31.0f / 255.0f), 0, 31);
	return (uint16_t)((r << 11) | (g << 5) | b);
}

Vec3 Unpack565(uint16_t c)
{
	int r = (c >> 11) & 31, g = (c >> 5) & 63, b = c & 31;
	return {(float)((r << 3) | (r >> 2)), (float)((g << 2) | (g >> 4)), (float)((b << 3) | (b >> 2))};
}

struct ColorFit {
	uint16_t c0, c1;
	uint32_t idx;
	float err;
};

void Palette(uint16_t c0, uint16_t c1, bool four, Vec3 pal[4])
{
	pal[0] = Unpack565(c0);
	pal[1] = Unpack565(c1);
	if (four) {
		pal[2] = pal[0] * (2.0f / 3.0f) + pal[1] * (1.0f / 3.0f);
		pal[3] = pal[0] * (1.0f / 3.0f) + pal[1] * (2.0f / 3.0f);
	} else {
		pal[2] = (pal[0] + pal[1]) * 0.5f;
		pal[3] = {0, 0, 0};
	}
}

ColorFit Evaluate(const Vec3 px[16], const bool opaque[16], uint16_t c0, uint16_t c1, bool four)
{
	if (four) {
		if (c0 < c1) std::swap(c0, c1);
	} else if (c0 > c1) {
		std::swap(c0, c1);
	}
	ColorFit f = {c0, c1, 0, 0.0f};
	if (four && c0 == c1) {
		Vec3 p = Unpack565(c0);
		for (int i = 0; i < 16; i++) {
			if (!opaque[i]) continue;
			Vec3 d = px[i] - p;
			f.err += Dot(d, d);
		}
		return f;
	}
	Vec3 pal[4];
	Palette(c0, c1, four, pal);
	int n = four ? 4 : 3;
	for (int i = 0; i < 16; i++) {
		int best = 3;
		if (opaque[i]) {
			float bd = 1e30f;
			for (int k = 0; k < n; k++) {
				Vec3 d = px[i] - pal[k];
				float e = Dot(d, d);
				if (e < bd) { bd = e; best = k; }
			}
			f.err += bd;
		}
		f.idx |= (uint32_t)best << (2 * i);
	}
	return f;
}

ColorFit Refine(const Vec3 px[16], const bool opaque[16], const ColorFit &fit, bool four)
{
	static const float w4[4] = {1.0f, 0.0f, 2.0f / 3.0f, 1.0f / 3.0f};
	static const float w3[3] = {1.0f, 0.0f, 0.5f};
	float aa = 0, bb = 0, ab = 0;
	Vec3 ax = {0, 0, 0}, bx = {0, 0, 0};
	for (int i = 0; i < 16; i++) {
		if (!opaque[i]) continue;
		int k = (fit.idx >> (2 * i)) & 3;
		if (!four && k == 3) continue;
		float a = four ? w4[k] : w3[k], b = 1.0f - a;
		aa += a * a; bb += b * b; ab += a * b;
		ax = ax + px[i] * a;
		bx = bx + px[i] * b;
	}
	float det = aa * bb - ab * ab;
	if (fabsf(det) < 1e-6f) return fit;
	float inv = 1.0f / det;
	Vec3 e0 = (ax * bb - bx * ab) * inv;
	Vec3 e1 = (bx * aa - ax * ab) * inv;
	auto clamp = [](Vec3 v) { return Vec3{std::clamp(v.r, 0.0f, 255.0f), std::clamp(v.g, 0.0f, 255.0f), std::clamp(v.b, 0.0f, 255.0f)}; };
	ColorFit r = Evaluate(px, opaque, Pack565(clamp(e0)), Pack565(clamp(e1)), four);
	return r.err < fit.err ? r : fit;
}

ColorFit FitMode(const Vec3 px[16], const bool opaque[16], Vec3 lo, Vec3 hi, bool four)
{
	ColorFit best = Evaluate(px, opaque, Pack565(hi), Pack565(lo), four);
	for (int it = 0; it < 2; it++) best = Refine(px, opaque, best, four);
	return best;
}

void EncodeColorBlock(const uint8_t blk[16][4], uint8_t *out, bool dxt1)
{
	Vec3 px[16];
	bool opaque[16];
	int n = 0;
	Vec3 mean = {0, 0, 0}, mn = {255, 255, 255}, mx = {0, 0, 0};
	for (int i = 0; i < 16; i++) {
		px[i] = {(float)blk[i][0], (float)blk[i][1], (float)blk[i][2]};
		opaque[i] = !dxt1 || blk[i][3] >= 128;
		if (!opaque[i]) continue;
		n++;
		mean = mean + px[i];
		mn = {std::min(mn.r, px[i].r), std::min(mn.g, px[i].g), std::min(mn.b, px[i].b)};
		mx = {std::max(mx.r, px[i].r), std::max(mx.g, px[i].g), std::max(mx.b, px[i].b)};
	}
	bool transparent = n < 16;
	ColorFit fit;
	if (!n) {
		fit = {0, 0, 0xffffffffu, 0.0f};
	} else {
		mean = mean * (1.0f / n);
		float c[6] = {0, 0, 0, 0, 0, 0};
		for (int i = 0; i < 16; i++) {
			if (!opaque[i]) continue;
			Vec3 d = px[i] - mean;
			c[0] += d.r * d.r; c[1] += d.r * d.g; c[2] += d.r * d.b;
			c[3] += d.g * d.g; c[4] += d.g * d.b; c[5] += d.b * d.b;
		}
		Vec3 axis = mx - mn;
		for (int it = 0; it < 8; it++) {
			Vec3 a = {c[0] * axis.r + c[1] * axis.g + c[2] * axis.b,
			          c[1] * axis.r + c[3] * axis.g + c[4] * axis.b,
			          c[2] * axis.r + c[4] * axis.g + c[5] * axis.b};
			float len = sqrtf(Dot(a, a));
			if (len < 1e-6f) break;
			axis = a * (1.0f / len);
		}
		float alen = sqrtf(Dot(axis, axis));
		Vec3 lo = mn, hi = mx;
		if (alen > 1e-6f) {
			axis = axis * (1.0f / alen);
			float tmin = 1e30f, tmax = -1e30f;
			for (int i = 0; i < 16; i++) {
				if (!opaque[i]) continue;
				float t = Dot(px[i] - mean, axis);
				tmin = std::min(tmin, t);
				tmax = std::max(tmax, t);
			}
			lo = mean + axis * tmin;
			hi = mean + axis * tmax;
		}
		if (transparent) {
			fit = FitMode(px, opaque, lo, hi, false);
			ColorFit box = FitMode(px, opaque, mn, mx, false);
			if (box.err < fit.err) fit = box;
		} else {
			fit = FitMode(px, opaque, lo, hi, true);
			ColorFit box = FitMode(px, opaque, mn, mx, true);
			if (box.err < fit.err) fit = box;
			if (dxt1 && fit.err > 0.0f) {
				ColorFit three = FitMode(px, opaque, lo, hi, false);
				if (three.err < fit.err) fit = three;
			}
		}
	}
	out[0] = fit.c0 & 255; out[1] = fit.c0 >> 8;
	out[2] = fit.c1 & 255; out[3] = fit.c1 >> 8;
	out[4] = fit.idx & 255; out[5] = (fit.idx >> 8) & 255;
	out[6] = (fit.idx >> 16) & 255; out[7] = fit.idx >> 24;
}

float AlphaFit(const uint8_t blk[16][4], uint8_t a0, uint8_t a1, uint64_t &bits)
{
	int pal[8];
	pal[0] = a0;
	pal[1] = a1;
	if (a0 > a1) {
		for (int i = 1; i < 7; i++) pal[i + 1] = ((7 - i) * a0 + i * a1 + 3) / 7;
	} else {
		for (int i = 1; i < 5; i++) pal[i + 1] = ((5 - i) * a0 + i * a1 + 2) / 5;
		pal[6] = 0;
		pal[7] = 255;
	}
	float err = 0;
	bits = 0;
	for (int i = 0; i < 16; i++) {
		int best = 0, bd = 1 << 30;
		for (int k = 0; k < 8; k++) {
			int d = (int)blk[i][3] - pal[k];
			if (d * d < bd) { bd = d * d; best = k; }
		}
		err += (float)bd;
		bits |= (uint64_t)best << (3 * i);
	}
	return err;
}

void EncodeAlphaBlock5(const uint8_t blk[16][4], uint8_t *out)
{
	int mn = 255, mx = 0, mn6 = 255, mx6 = 0;
	for (int i = 0; i < 16; i++) {
		int a = blk[i][3];
		mn = std::min(mn, a);
		mx = std::max(mx, a);
		if (a != 0 && a != 255) {
			mn6 = std::min(mn6, a);
			mx6 = std::max(mx6, a);
		}
	}
	uint64_t bits8, bits6 = 0;
	uint8_t a0 = (uint8_t)mx, a1 = (uint8_t)mn;
	float e8 = AlphaFit(blk, a0, a1, bits8);
	float e6 = 1e30f;
	if (mn6 > mx6) mn6 = mx6 = mn;
	if (mn6 <= mx6) e6 = AlphaFit(blk, (uint8_t)mn6, (uint8_t)mx6, bits6);
	uint64_t bits = bits8;
	if (e6 < e8) {
		a0 = (uint8_t)mn6;
		a1 = (uint8_t)mx6;
		bits = bits6;
	}
	out[0] = a0;
	out[1] = a1;
	for (int i = 0; i < 6; i++) out[2 + i] = (uint8_t)(bits >> (8 * i));
}

void EncodeAlphaBlock3(const uint8_t blk[16][4], uint8_t *out)
{
	memset(out, 0, 8);
	for (int i = 0; i < 16; i++) {
		int a = (blk[i][3] * 15 + 127) / 255;
		out[i / 2] |= (uint8_t)(a << ((i & 1) * 4));
	}
}

void EncodeLevel(const uint8_t *rgba, int w, int h, Format fmt, std::vector<uint8_t> &out)
{
	int bw = (w + 3) / 4, bh = (h + 3) / 4;
	int bytes = fmt == DXT1 ? 8 : 16;
	size_t base = out.size();
	out.resize(base + (size_t)bw * bh * bytes);
	uint8_t *dst = out.data() + base;
	uint8_t blk[16][4];
	for (int by = 0; by < bh; by++) {
		for (int bx = 0; bx < bw; bx++) {
			for (int i = 0; i < 16; i++) {
				int x = std::min(w - 1, bx * 4 + (i & 3));
				int y = std::min(h - 1, by * 4 + i / 4);
				memcpy(blk[i], rgba + ((size_t)y * w + x) * 4, 4);
			}
			switch (fmt) {
			case DXT1:
				EncodeColorBlock(blk, dst, true);
				break;
			case DXT3:
				EncodeAlphaBlock3(blk, dst);
				EncodeColorBlock(blk, dst + 8, false);
				break;
			case DXT5:
				EncodeAlphaBlock5(blk, dst);
				EncodeColorBlock(blk, dst + 8, false);
				break;
			}
			dst += bytes;
		}
	}
}

std::vector<uint8_t> Downsample(const uint8_t *src, int w, int h, int &nw, int &nh)
{
	nw = std::max(1, w / 2);
	nh = std::max(1, h / 2);
	std::vector<uint8_t> dst((size_t)nw * nh * 4);
	for (int y = 0; y < nh; y++) {
		int y0 = std::min(h - 1, y * 2), y1 = std::min(h - 1, y * 2 + 1);
		for (int x = 0; x < nw; x++) {
			int x0 = std::min(w - 1, x * 2), x1 = std::min(w - 1, x * 2 + 1);
			for (int c = 0; c < 4; c++) {
				int s = src[((size_t)y0 * w + x0) * 4 + c] + src[((size_t)y0 * w + x1) * 4 + c] +
				        src[((size_t)y1 * w + x0) * 4 + c] + src[((size_t)y1 * w + x1) * 4 + c];
				dst[((size_t)y * nw + x) * 4 + c] = (uint8_t)((s + 2) / 4);
			}
		}
	}
	return dst;
}

void Put32(std::vector<uint8_t> &v, size_t ofs, uint32_t x)
{
	v[ofs] = x & 255;
	v[ofs + 1] = (x >> 8) & 255;
	v[ofs + 2] = (x >> 16) & 255;
	v[ofs + 3] = x >> 24;
}

}

std::vector<uint8_t> Encode(const uint8_t *rgba, int width, int height, Format fmt, bool mipmaps)
{
	int levels = 1;
	if (mipmaps)
		for (int s = std::max(width, height); s > 1; s >>= 1) levels++;

	std::vector<uint8_t> out(128, 0);
	memcpy(out.data(), "DDS ", 4);
	int bytes = fmt == DXT1 ? 8 : 16;
	uint32_t linear = (uint32_t)(((width + 3) / 4) * ((height + 3) / 4) * bytes);
	uint32_t fourcc = fmt == DXT1 ? 0x31545844u : fmt == DXT3 ? 0x33545844u : 0x35545844u;
	Put32(out, 4, 124);
	Put32(out, 8, 0x1 | 0x2 | 0x4 | 0x1000 | 0x80000 | (mipmaps ? 0x20000 : 0));
	Put32(out, 12, (uint32_t)height);
	Put32(out, 16, (uint32_t)width);
	Put32(out, 20, linear);
	Put32(out, 28, mipmaps ? (uint32_t)levels : 0);
	Put32(out, 76, 32);
	Put32(out, 80, 0x4);
	Put32(out, 84, fourcc);
	Put32(out, 108, 0x1000 | (mipmaps ? 0x400008 : 0));

	EncodeLevel(rgba, width, height, fmt, out);
	std::vector<uint8_t> cur;
	const uint8_t *src = rgba;
	int w = width, h = height;
	for (int l = 1; l < levels; l++) {
		int nw, nh;
		std::vector<uint8_t> next = Downsample(src, w, h, nw, nh);
		cur.swap(next);
		src = cur.data();
		w = nw;
		h = nh;
		EncodeLevel(src, w, h, fmt, out);
	}
	return out;
}

std::vector<uint8_t> FromBottomUpBGR(const uint8_t *bgr, const uint8_t *alpha, int width, int height)
{
	std::vector<uint8_t> rgba((size_t)width * height * 4);
	for (int y = 0; y < height; y++) {
		const uint8_t *s = bgr + (size_t)(height - 1 - y) * width * 3;
		const uint8_t *a = alpha ? alpha + (size_t)(height - 1 - y) * width : nullptr;
		uint8_t *d = rgba.data() + (size_t)y * width * 4;
		for (int x = 0; x < width; x++) {
			d[x * 4 + 0] = s[x * 3 + 2];
			d[x * 4 + 1] = s[x * 3 + 1];
			d[x * 4 + 2] = s[x * 3 + 0];
			d[x * 4 + 3] = a ? a[x] : 255;
		}
	}
	return rgba;
}

}
