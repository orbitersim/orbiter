// not upstream: dxtex, Linux counterpart of the DirectX SDK DxTex.exe as pltex/plsplit call it: BMP [+ alpha BMP] -> DXT1/DXT5 .dds

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <cstdint>
#include <strings.h>
#include <algorithm>
#include <vector>
#include "dxt.h"

// fastdxt block encoding functions (dxt.cpp)
void ExtractBlock (const byte *inPtr, int width, byte *colorBlock);
void GetMinMaxColorsByBBox (const byte *colorBlock, byte *minColor, byte *maxColor);
word ColorTo565 (const byte *color);
void EmitByte (byte b, byte *&outData);
void EmitWord (word s, byte *&outData);
void EmitDoubleWord (dword i, byte *&outData);
void EmitColorIndicesFast (const byte *colorBlock, const byte *minColor, const byte *maxColor, byte *&outData);
void EmitAlphaIndicesFast (const byte *colorBlock, const byte minAlpha, const byte maxAlpha, byte *&outData);

// .dds header after the "DDS " magic: DDSURFACEDESC2 as laid out in the file
#pragma pack(push, 1)
struct DDSHEADER {
	uint32_t dwSize, dwFlags, dwHeight, dwWidth, dwLinearSize, dwDepth, dwMipMapCount, dwReserved1[11];
	struct { uint32_t dwSize, dwFlags, dwFourCC, dwRGBBitCount, dwRBitMask, dwGBitMask, dwBBitMask, dwABitMask; } ddpf;
	uint32_t dwCaps, dwCaps2, dwCaps3, dwCaps4, dwReserved2;
};
#pragma pack(pop)
static_assert (sizeof(DDSHEADER) == 124, "DDS header layout");

const uint32_t DDSD_CAPS = 0x1, DDSD_HEIGHT = 0x2, DDSD_WIDTH = 0x4, DDSD_PIXELFORMAT = 0x1000, DDSD_MIPMAPCOUNT = 0x20000, DDSD_LINEARSIZE = 0x80000;
const uint32_t DDPF_FOURCC = 0x4;
const uint32_t DDSCAPS_COMPLEX = 0x8, DDSCAPS_TEXTURE = 0x1000, DDSCAPS_MIPMAP = 0x400000;

struct Image {
	int w = 0, h = 0;
	std::vector<byte> px; // RGBA, top row first
};

// uncompressed 8/24/32-bit BMP, bottom-up or top-down, rows padded to 4 bytes
static bool ReadBMP (const char *fname, Image &img)
{
	FILE *f = fopen (fname, "rb");
	if (!f) return false;
	fseek (f, 0, SEEK_END);
	long n = ftell (f);
	fseek (f, 0, SEEK_SET);
	std::vector<byte> d(n > 0 ? n : 0);
	bool ok = n >= 54 && fread (d.data(), 1, n, f) == (size_t)n;
	fclose (f);
	if (!ok || d[0] != 'B' || d[1] != 'M') return false;
	auto u16 = [&](size_t o) { return (uint32_t)d[o] | (uint32_t)d[o+1] << 8; };
	auto u32 = [&](size_t o) { return u16(o) | u16(o+2) << 16; };
	uint32_t ofs = u32(10), hsize = u32(14), comp = u32(30), nclr = u32(46);
	int32_t w = (int32_t)u32(18), h = (int32_t)u32(22);
	int bpp = (int)u16(28);
	bool topdown = h < 0;
	if (topdown) h = -h;
	if (w <= 0 || h <= 0 || comp != 0 || (bpp != 8 && bpp != 24 && bpp != 32)) return false;
	if (bpp == 8 && !nclr) nclr = 256;
	size_t stride = ((size_t)w*bpp/8 + 3) & ~(size_t)3;
	if ((size_t)ofs + stride*h > (size_t)n || (bpp == 8 && 14 + hsize + 4*nclr > ofs)) return false;
	const byte *pal = d.data() + 14 + hsize; // BGRx entries
	img.w = w; img.h = h;
	img.px.resize ((size_t)w*h*4);
	for (int y = 0; y < h; y++) {
		const byte *row = d.data() + ofs + stride * (topdown ? y : h-1-y);
		byte *t = img.px.data() + (size_t)y*w*4;
		for (int x = 0; x < w; x++, t += 4) {
			const byte *s = (bpp == 8 ? pal + 4*(row[x] < nclr ? row[x] : 0) : row + x*(bpp/8));
			t[0] = s[2]; t[1] = s[1]; t[2] = s[0]; t[3] = 255;
		}
	}
	return true;
}

// next mip level: 2x2 box filter (edge pixels repeated for odd sizes)
static Image Half (const Image &s)
{
	Image t;
	t.w = std::max (1, s.w/2); t.h = std::max (1, s.h/2);
	t.px.resize ((size_t)t.w*t.h*4);
	for (int y = 0; y < t.h; y++) {
		int y0 = std::min (2*y, s.h-1), y1 = std::min (2*y+1, s.h-1);
		for (int x = 0; x < t.w; x++) {
			int x0 = std::min (2*x, s.w-1), x1 = std::min (2*x+1, s.w-1);
			for (int c = 0; c < 4; c++)
				t.px[((size_t)y*t.w+x)*4+c] = (byte)((s.px[((size_t)y0*s.w+x0)*4+c] + s.px[((size_t)y0*s.w+x1)*4+c] +
					s.px[((size_t)y1*s.w+x0)*4+c] + s.px[((size_t)y1*s.w+x1)*4+c] + 2) / 4);
		}
	}
	return t;
}

// DXT1 block with 1-bit alpha: colour0 <= colour1 selects 3 colours + transparent (index 3) for pixels with alpha < 128
static void EmitPunchThroughBlock (const byte *block, byte *&outData)
{
	byte minColor[4] = {255, 255, 255, 0}, maxColor[4] = {0, 0, 0, 0};
	bool opaque = false;
	for (int i = 0; i < 16; i++) {
		if (block[i*4+3] < 128) continue;
		opaque = true;
		for (int c = 0; c < 3; c++) {
			minColor[c] = std::min (minColor[c], block[i*4+c]);
			maxColor[c] = std::max (maxColor[c], block[i*4+c]);
		}
	}
	if (!opaque) minColor[0] = minColor[1] = minColor[2] = 0;
	EmitWord (ColorTo565 (minColor), outData); // min <= max per channel, so 565(min) <= 565(max)
	EmitWord (ColorTo565 (maxColor), outData);
	int col[3][3];
	for (int c = 0; c < 3; c++) {
		int mask = (c == 1 ? 0xFC : 0xF8), shift = (c == 1 ? 6 : 5);
		col[0][c] = (minColor[c] & mask) | (minColor[c] >> shift);
		col[1][c] = (maxColor[c] & mask) | (maxColor[c] >> shift);
		col[2][c] = (col[0][c] + col[1][c]) / 2;
	}
	dword result = 0;
	for (int i = 0; i < 16; i++) {
		dword idx = 3;
		if (block[i*4+3] >= 128) {
			int dmin = 1 << 30;
			for (int k = 0; k < 3; k++) {
				int d = abs (col[k][0] - block[i*4]) + abs (col[k][1] - block[i*4+1]) + abs (col[k][2] - block[i*4+2]);
				if (d < dmin) dmin = d, idx = k;
			}
		}
		result |= idx << (i*2);
	}
	EmitDoubleWord (result, outData);
}

// compress one mip level, appending the blocks to out; sizes that are not multiples of 4 repeat the edge pixels
static void Compress (const Image &img, bool dxt5, bool alpha, std::vector<byte> &out)
{
	int bw = (img.w+3)/4, bh = (img.h+3)/4, pw = bw*4;
	std::vector<byte> pad ((size_t)pw*bh*4*4);
	for (int y = 0; y < bh*4; y++)
		for (int x = 0; x < pw; x++)
			memcpy (&pad[((size_t)y*pw+x)*4], &img.px[((size_t)std::min (y, img.h-1)*img.w + std::min (x, img.w-1))*4], 4);
	size_t ofs = out.size();
	out.resize (ofs + (size_t)bw*bh*(dxt5 ? 16 : 8));
	byte *outData = out.data() + ofs;
	byte block[64], minColor[4], maxColor[4];
	for (int j = 0; j < bh; j++) {
		for (int i = 0; i < bw; i++) {
			ExtractBlock (pad.data() + ((size_t)j*4*pw + i*4)*4, pw, block);
			if (dxt5) { // alpha block: exact alpha range, 8-alpha mode
				byte amin = 255, amax = 0;
				for (int k = 0; k < 16; k++) amin = std::min (amin, block[k*4+3]), amax = std::max (amax, block[k*4+3]);
				EmitByte (amax, outData);
				EmitByte (amin, outData);
				EmitAlphaIndicesFast (block, amin, amax, outData);
			} else if (alpha) {
				bool transp = false;
				for (int k = 0; k < 16; k++) transp = transp || block[k*4+3] < 128;
				if (transp) { EmitPunchThroughBlock (block, outData); continue; }
			}
			GetMinMaxColorsByBBox (block, minColor, maxColor); // 4-colour block as fastdxt's CompressImageDXT1
			EmitWord (ColorTo565 (maxColor), outData);
			EmitWord (ColorTo565 (minColor), outData);
			EmitColorIndicesFast (block, minColor, maxColor, outData);
		}
	}
}

static int Usage ()
{
	fprintf (stderr, "Usage: dxtex <image.bmp> [-a <alpha.bmp>] [-m] DXT1|DXT5 <output.dds>\n");
	fprintf (stderr, "  -a  alpha channel from the grey level of <alpha.bmp> (DXT1: alpha < 128 is transparent)\n");
	fprintf (stderr, "  -m  generate the full mipmap chain (box filter)\n");
	return 1;
}

int main (int argc, char *argv[])
{
	const char *src = 0, *asrc = 0, *fmt = 0, *dst = 0;
	bool mipmap = false;
	for (int i = 1; i < argc; i++) {
		const char *a = argv[i];
		if (!a[0]) continue; // pltex/plsplit pass "" in place of -m
		else if (!strcmp (a, "-a") && i+1 < argc) asrc = argv[++i];
		else if (!strcmp (a, "-m")) mipmap = true;
		else if (!strcasecmp (a, "DXT1") || !strcasecmp (a, "DXT5")) fmt = a; // DxTex's other formats left out: the tools use DXT1/DXT5 only
		else if (!src) src = a;
		else if (!dst) dst = a;
		else return Usage();
	}
	if (!src || !fmt || !dst) return Usage();

	Image img, aimg;
	if (!ReadBMP (src, img)) { fprintf (stderr, "dxtex: cannot read %s (uncompressed 8/24/32-bit BMP expected)\n", src); return 1; }
	if (asrc) {
		if (!ReadBMP (asrc, aimg)) { fprintf (stderr, "dxtex: cannot read %s (uncompressed 8/24/32-bit BMP expected)\n", asrc); return 1; }
		if (aimg.w != img.w || aimg.h != img.h) { fprintf (stderr, "dxtex: %s and %s differ in size\n", src, asrc); return 1; }
		for (size_t i = 0; i < img.px.size(); i += 4)
			img.px[i+3] = (byte)((aimg.px[i] + aimg.px[i+1] + aimg.px[i+2]) / 3);
	}

	bool dxt5 = !strcasecmp (fmt, "DXT5");
	int nlevel = 1;
	if (mipmap)
		for (int s = std::max (img.w, img.h); s > 1; s >>= 1) nlevel++;
	std::vector<byte> data;
	uint32_t linsize = 0;
	Image lvl = img;
	for (int l = 0; l < nlevel; l++) {
		if (l) lvl = Half (lvl);
		Compress (lvl, dxt5, asrc != 0, data);
		if (!l) linsize = (uint32_t)data.size();
	}

	DDSHEADER hdr;
	memset (&hdr, 0, sizeof(hdr));
	hdr.dwSize = sizeof(hdr);
	hdr.dwFlags = DDSD_CAPS | DDSD_HEIGHT | DDSD_WIDTH | DDSD_PIXELFORMAT | DDSD_LINEARSIZE | (mipmap ? DDSD_MIPMAPCOUNT : 0);
	hdr.dwHeight = img.h;
	hdr.dwWidth = img.w;
	hdr.dwLinearSize = linsize;
	hdr.dwMipMapCount = (mipmap ? nlevel : 0);
	hdr.ddpf.dwSize = sizeof(hdr.ddpf);
	hdr.ddpf.dwFlags = DDPF_FOURCC;
	memcpy (&hdr.ddpf.dwFourCC, dxt5 ? "DXT5" : "DXT1", 4);
	hdr.dwCaps = DDSCAPS_TEXTURE | (mipmap ? DDSCAPS_COMPLEX | DDSCAPS_MIPMAP : 0);

	FILE *f = fopen (dst, "wb");
	if (!f) { fprintf (stderr, "dxtex: cannot write %s\n", dst); return 1; }
	bool ok = fwrite ("DDS ", 1, 4, f) == 4 && fwrite (&hdr, sizeof(hdr), 1, f) == 1 && fwrite (data.data(), 1, data.size(), f) == data.size();
	ok = (fclose (f) == 0) && ok;
	if (!ok) { fprintf (stderr, "dxtex: error writing %s\n", dst); return 1; }
	return 0;
}
