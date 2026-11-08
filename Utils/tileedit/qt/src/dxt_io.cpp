#include "dxt_io.h"
#include <cstdio>
#include <cstring>
#include <png.h>
#include "libdxt.h"

struct DDS_PIXELFORMAT {
	uint32_t dwSize;
	uint32_t dwFlags;
	uint32_t dwFourCC;
	uint32_t dwRGBBitCount;
	uint32_t dwRBitMask;
	uint32_t dwGBitMask;
	uint32_t dwBBitMask;
	uint32_t dwABitMask;
};

struct DDS_HEADER{
	uint32_t           dwSize;
	uint32_t           dwFlags;
	uint32_t           dwHeight;
	uint32_t           dwWidth;
	uint32_t           dwPitchOrLinearSize;
	uint32_t           dwDepth;
	uint32_t           dwMipMapCount;
	uint32_t           dwReserved1[11];
	DDS_PIXELFORMAT ddspf;
	uint32_t           dwCaps;
	uint32_t           dwCaps2;
	uint32_t           dwCaps3;
	uint32_t           dwCaps4;
	uint32_t           dwReserved2;
};

static_assert(sizeof(DDS_PIXELFORMAT) == 32 && sizeof(DDS_HEADER) == 124, "DDS header must match the on-disk layout");

void setdxt1header(const Image &idata, DDS_HEADER &hdr)
{
	const char fourcc[4] = { 'D', 'X', 'T', '1' };

	memset(&hdr, 0, sizeof(DDS_HEADER));
	hdr.dwSize = sizeof(DDS_HEADER);
	hdr.dwFlags = 0x00081007;
	hdr.dwHeight = idata.height;
	hdr.dwWidth = idata.width;
	hdr.dwPitchOrLinearSize = idata.height * idata.width / 2;
	hdr.ddspf.dwSize = sizeof(DDS_PIXELFORMAT);
	hdr.ddspf.dwFlags = 0x4;
	memcpy(&hdr.ddspf.dwFourCC, fourcc, 4);
	hdr.dwCaps = 0x1000;
}

void dxt1write(const char *fname, const Image &idata)
{
	const char magic[4] = { 'D', 'D', 'S', ' ' };
	int format = FORMAT_DXT1;
	uint8_t *dxt1 = new uint8_t[idata.width * idata.height * 4];

	// Need to flip RGB order for the compression engine
	uint32_t *inp = new uint32_t[idata.width * idata.height];
	const uint32_t *id = idata.data.data();
	for (int i = 0; i < idata.width*idata.height; i++)
		inp[i] = 0xff000000 | ((id[i] & 0xff) << 16) | (id[i] & 0xff00) | ((id[i] & 0xff0000) >> 16);

	int n = CompressDXT((const uint8_t*)inp, dxt1, idata.width, idata.height, format);

	DDS_HEADER hdr;
	setdxt1header(idata, hdr);
	FILE *f = fopen(fname, "wb");
	fwrite(magic, 1, 4, f);
	fwrite(&hdr, sizeof(DDS_HEADER), 1, f);
	fwrite(dxt1, n, 1, f);
	fclose(f);
	delete[]dxt1;
	delete[]inp;
}

bool pngread_tmp(const char *fname, Image &idata)
{
	bool ok;
	png_image image;
	memset(&image, 0, sizeof(png_image));
	image.version = PNG_IMAGE_VERSION;
	image.opaque = NULL;
	if ((ok = png_image_begin_read_from_file(&image, fname) != 0)) {
		image.format = PNG_FORMAT_BGRA;
		idata.width = image.width;
		idata.height = image.height;
		int n = idata.width * idata.height;
		idata.data.resize(n);
		png_image_finish_read(&image, NULL, idata.data.data(), image.width * 4, NULL);
	}
	png_image_free(&image);
	return ok;
}

void pngwrite_tmp(const char *fname, const Image &idata)
{
	int w = idata.width;
	int h = idata.height;
	int n = w*h;
	
	png_image image;
	image.opaque = NULL;
	image.version = PNG_IMAGE_VERSION;
	image.format = PNG_FORMAT_BGRA;
	image.width = w;
	image.height = h;
	image.flags = 0;
	image.colormap_entries = 0;
	png_image_write_to_file(&image, fname, 0, idata.data.data(), 0, 0);
	png_image_free(&image);
}

int dxtread_png(const char *fname, const SurfPatchMetaInfo &meta, Image &sdata)
{
	int res = 0;
	png_image image;
	memset(&image, 0, sizeof(png_image));
	image.version = PNG_IMAGE_VERSION;
	image.opaque = NULL;
	if (png_image_begin_read_from_file(&image, fname)) {
		image.format = PNG_FORMAT_BGRA;

		int w = sdata.width;
		int h = sdata.height;
		if (image.width == w && image.height == h) {

			int n = w*h;
			unsigned char *buf = new unsigned char[n * 4];
			png_image_finish_read(&image, NULL, buf, w * 4, NULL);

			if (meta.colourMatch >= 1) {
				// cumulative histogram matching
				Image sdata2;
				sdata2.width = w;
				sdata2.height = h;
				sdata2.data.resize(w*h);
				memcpy(sdata2.data.data(), buf, w*h * sizeof(uint32_t));
				switch (meta.colourMatch) {
				case 1:
					match_histogram(sdata2, sdata);
					break;
				case 2:
					match_hue_sat(sdata2, sdata);
					break;
				}
				memcpy(buf, sdata2.data.data(), w*h * sizeof(uint32_t));
			}

			int idx = 0;
			for (int ih = 0; ih < h; ih++) {
				for (int iw = 0; iw < w; iw++) {
					uint32_t v;
					if (!meta.alphaBlend || buf[idx + 3] == 0xff) {
						v = 0xff000000 | (buf[idx+2] << 16) | (buf[idx + 1] << 8) | buf[idx];
						sdata.data[iw + ih*w] = v;
					} 
					else if (buf[idx + 3]) {
						v = 0xff000000;
						uint32_t s = buf[idx + 3];
						uint32_t ch1 = (uint32_t)buf[idx+2];
						uint32_t ch2 = (sdata.data[iw + ih*w] >> 16) & 0xFF;
						uint32_t ch = (ch1 * s + ch2 * (255 - s)) / 255;
						v |= ch << 16;
						ch1 = (uint32_t)buf[idx + 1];
						ch2 = (sdata.data[iw + ih*w] >> 8) & 0xff;
						ch = (ch1 * s + ch2 * (255 - s)) / 255;
						v |= ch << 8;
						ch1 = (uint32_t)buf[idx];
						ch2 = (sdata.data[iw + ih*w] & 0xff);
						ch = (ch1 * s + ch2 * (255 - s)) / 255;
						v |= ch;
						sdata.data[iw + ih*w] = v;
					}
					idx += 4;
				}
			}
			delete[]buf;
		}
		else {
			res = -2;
		}
	}
	else {
		res = -1;
	}
	png_image_free(&image);
	return res;
}