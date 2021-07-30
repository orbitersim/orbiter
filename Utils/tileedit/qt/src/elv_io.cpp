#include <windows.h>
#include <vector>
#include <algorithm>
#define _USE_MATH_DEFINES
#include <math.h>
#include <png.h>
#include "elv_io.h"

#pragma pack(push,1)

struct ELEVFILEHEADER { // file header for patch elevation data file
	char id[4];            // ID string + version ('E','L','E',1)
	int hdrsize;           // header size (76 expected)
	int dtype;             // data format (0=flat, no data block; 8=uint8; -8=int8; 16=uint16; -16=int16)
	int xgrd, ygrd;         // data grid size (259 x 259 expected)
	int xpad, ypad;         // horizontal, vertical padding width (1, 1 expected)
	double scale;          // data scaling factor (1.0 expected)
	double offset;         // data offset (elevation = raw value * scale + offset)
	double latmin, latmax; // latitude range [rad]
	double lngmin, lngmax; // longitude range [rad]
	double emin, emax, emean; // min, max, mean elevation [m]
};

#pragma pack(pop)

// ==================================================================================

ElevData elvread(const char *fname)
{
	const int ndat = TILE_ELEVSTRIDE*TILE_ELEVSTRIDE;
	ElevData edata;
	ELEVFILEHEADER hdr;
	double scale, offset;
	int i;

	FILE *f = fopen(fname, "rb");
	if (!f)
		return edata;

	int res = fread(&hdr, sizeof(ELEVFILEHEADER), 1, f);
	if (res != 1 || strncmp(hdr.id, "ELE\01", 4))
		return edata;

	if (hdr.hdrsize != sizeof(ELEVFILEHEADER)) {
		fseek(f, hdr.hdrsize, SEEK_SET);
	}
	scale = hdr.scale;
	offset = hdr.offset;
	edata.data.resize(ndat);
	edata.width = TILE_ELEVSTRIDE;
	edata.height = TILE_ELEVSTRIDE;
	double *e = edata.data.data();

	switch (hdr.dtype) {
	case 0:
		for (i = 0; i < ndat; i++)
			e[i] = offset;
		break;
	case 8:
		for (i = 0; i < ndat; i++) {
			UINT8 v;
			fread(&v, sizeof(UINT8), 1, f);
			e[i] = (double)v * scale + offset;
		}
		break;
	case -16:
		for (i = 0; i < ndat; i++) {
			INT16 v;
			fread(&v, sizeof(INT16), 1, f);
			e[i] = (double)v * scale + offset;
		}
		break;
	}
	fclose(f);

	edata.dmin = *std::min_element(edata.data.begin(), edata.data.end());
	edata.dmax = *std::max_element(edata.data.begin(), edata.data.end());
	edata.dres = scale;

	return edata;
}

// ==================================================================================

bool elvmodread(const char *fname, ElevData &edata)
{
	const int ndat = TILE_ELEVSTRIDE*TILE_ELEVSTRIDE;
	ELEVFILEHEADER hdr;
	int i;

	FILE *f = fopen(fname, "rb");
	if (!f)
		return false;

	int res = fread(&hdr, sizeof(ELEVFILEHEADER), 1, f);
	if (res != 1 || strncmp(hdr.id, "ELE\01", 4))
		return false;

	if (hdr.hdrsize != sizeof(ELEVFILEHEADER)) {
		fseek(f, hdr.hdrsize, SEEK_SET);
	}
	double *e = edata.data.data();
	double offset = hdr.offset;
	double scale = hdr.scale;

	switch (hdr.dtype) {
	case 0:
		for (i = 0; i < ndat; i++)
			e[i] = offset;
		break;
	case 8: 
		for (i = 0; i < ndat; i++) {
			UINT8 v;
			fread(&v, sizeof(UINT8), 1, f);
			if (v != UCHAR_MAX)
				e[i] = (double)v * scale + offset;
		}
		break;
	case -16:
		for (i = 0; i < ndat; i++) {
			INT16 v;
			fread(&v, sizeof(INT16), 1, f);
			if (v != SHRT_MAX)
				e[i] = (double)v * scale + offset;
		}
		break;
	}
	fclose(f);
	edata.dmin = *std::min_element(edata.data.begin(), edata.data.end());
	edata.dmax = *std::max_element(edata.data.begin(), edata.data.end());
	if (scale < edata.dres)
		edata.dres = scale;

	return true;
}

// ==================================================================================

ElevData elvscan(const BYTE *data, int ndata)
{
	const int ndat = TILE_ELEVSTRIDE*TILE_ELEVSTRIDE;
	ElevData edata;
	ELEVFILEHEADER hdr;
	double scale, offset;
	int i;

	if (ndata < sizeof(ELEVFILEHEADER))
		return edata;

	memcpy(&hdr, data, sizeof(ELEVFILEHEADER));
	data += sizeof(ELEVFILEHEADER);
	ndata -= sizeof(ELEVFILEHEADER);

	if (strncmp(hdr.id, "ELE\01", 4) || hdr.hdrsize != sizeof(ELEVFILEHEADER))
		return edata;

	scale = hdr.scale;
	offset = hdr.offset;
	edata.data.resize(ndat);
	edata.width = TILE_ELEVSTRIDE;
	edata.height = TILE_ELEVSTRIDE;
	double *e = edata.data.data();

	switch (hdr.dtype) {
	case 0:
		for (i = 0; i < ndat; i++)
			e[i] = offset;
		break;
	case 8: {
		UINT8 *data_uint8 = (UINT8*)data;
		for (i = 0; i < ndat; i++)
			e[i] = (double)data_uint8[i] * scale + offset;
		} break;
	case -16: {
		INT16 *data_int16 = (INT16*)data;
		for (i = 0; i < ndat; i++)
			e[i] = (double)data_int16[i] * scale + offset;
		} break;
	}

	edata.dmin = *std::min_element(edata.data.begin(), edata.data.end());
	edata.dmax = *std::max_element(edata.data.begin(), edata.data.end());
	edata.dres = scale;

	return edata;
}

// ==================================================================================

bool elvmodscan(const BYTE*data, int ndata, ElevData &edata)
{
	const int ndat = TILE_ELEVSTRIDE*TILE_ELEVSTRIDE;
	ELEVFILEHEADER hdr;
	int i;

	if (ndata < sizeof(ELEVFILEHEADER))
		return false;

	memcpy(&hdr, data, sizeof(ELEVFILEHEADER));
	data += sizeof(ELEVFILEHEADER);
	ndata -= sizeof(ELEVFILEHEADER);

	if (strncmp(hdr.id, "ELE\01", 4) || hdr.hdrsize != sizeof(ELEVFILEHEADER))
		return false;

	double *e = edata.data.data();
	double offset = hdr.offset;
	double scale = hdr.scale;

	switch (hdr.dtype) {
	case 0:
		for (i = 0; i < ndat; i++)
			e[i] = offset;
		break;
	case 8: {
		UINT8 *data_uint8 = (UINT8*)data;
		for (i = 0; i < ndat; i++) {
			UINT8 v = data_uint8[i];
			if (v != UCHAR_MAX)
				e[i] = (double)v * scale + offset;
		}
		} break;
	case -16: {
		INT16 *data_int16 = (INT16*)data;
		for (i = 0; i < ndat; i++) {
			INT16 v = data_int16[i];
			if (v != SHRT_MAX)
				e[i] = (double)v * scale + offset;
		}
		} break;
	}

	edata.dmin = *std::min_element(edata.data.begin(), edata.data.end());
	edata.dmax = *std::max_element(edata.data.begin(), edata.data.end());
	if (scale < edata.dres)
		edata.dres = scale;

	return true;
}

// ==================================================================================

void elvwrite(const char *fname, const ElevData &edata, double latmin, double latmax, double lngmin, double lngmax)
{
	ELEVFILEHEADER hdr;
	strncpy(hdr.id, "ELE\01", 4);
	hdr.hdrsize = sizeof(ELEVFILEHEADER);
	hdr.xgrd = TILE_ELEVSTRIDE;
	hdr.ygrd = TILE_ELEVSTRIDE;
	hdr.xpad = 1;
	hdr.ypad = 1;
	hdr.latmin = latmin;
	hdr.latmax = latmax;
	hdr.lngmin = lngmin;
	hdr.lngmax = lngmax;
	hdr.emin = edata.dmin;
	hdr.emax = edata.dmax;

	hdr.emean = 0.0;
	for (int j = hdr.ypad; j < hdr.ygrd - hdr.ypad; j++) {
		for (int i = hdr.xpad; i < hdr.xgrd - hdr.xpad; i++) {
			hdr.emean += edata.data[j*TILE_ELEVSTRIDE + i];
		}
	}
	hdr.emean /= (hdr.xgrd - hdr.xpad * 2) * (hdr.ygrd - hdr.ypad * 2);

	hdr.scale = edata.dres;

	int imin = (int)(hdr.emin / hdr.scale);
	int imax = (int)(hdr.emax / hdr.scale);
	int shift;

	while ((imax - imin) > (1 << 16)) { // need to rescale to fit range
		hdr.scale *= 2.0;
		imin = (int)(hdr.emin / hdr.scale);
		imax = (int)(hdr.emax / hdr.scale);
	}

	if (imin == imax) {
		hdr.dtype = 0;
		shift = imin;
	}
	else if (imax - imin < 256) {
		hdr.dtype = 8;
		if (imin >= 0 && imax < 256)
			shift = 0;
		else
			shift = imin;
	}
	else {
		hdr.dtype = -16;
		if (imin >= SHRT_MIN && imax < SHRT_MAX)
			shift = 0;
		else
			shift = (imin + imax) / 2;
	}
	hdr.offset = shift * hdr.scale;

	FILE *f = fopen(fname, "wb");
	fwrite(&hdr, sizeof(ELEVFILEHEADER), 1, f);
	if (hdr.dtype == 8) {
		for (int i = 0; i < edata.data.size(); i++) {
			UINT8 v = (UINT8)((int)(edata.data[i] / hdr.scale) - shift);
			fwrite(&v, sizeof(UINT8), 1, f);
		}
	}
	else if (hdr.dtype == -16) {
		for (int i = 0; i < edata.data.size(); i++) {
			INT16 v = (INT16)((int)(edata.data[i] / hdr.scale) - shift);
			fwrite(&v, sizeof(INT16), 1, f);
		}
	}
	fclose(f);
}

// ==================================================================================

void elvmodwrite(const char *fname, const ElevData &edata, const ElevData &ebasedata, double latmin, double latmax, double lngmin, double lngmax)
{
	const double EPS = 1e-8;

	ELEVFILEHEADER hdr;
	strncpy(hdr.id, "ELE\01", 4);
	hdr.hdrsize = sizeof(ELEVFILEHEADER);
	hdr.xgrd = TILE_ELEVSTRIDE;
	hdr.ygrd = TILE_ELEVSTRIDE;
	hdr.xpad = 1;
	hdr.ypad = 1;
	hdr.latmin = latmin;
	hdr.latmax = latmax;
	hdr.lngmin = lngmin;
	hdr.lngmax = lngmax;
	hdr.emin = edata.dmin;
	hdr.emax = edata.dmax;

	hdr.emean = 0.0;
	for (int j = hdr.ypad; j < hdr.ygrd - hdr.ypad; j++) {
		for (int i = hdr.xpad; i < hdr.xgrd - hdr.xpad; i++) {
			hdr.emean += edata.data[j*TILE_ELEVSTRIDE + i];
		}
	}
	hdr.emean /= (hdr.xgrd - hdr.xpad * 2) * (hdr.ygrd - hdr.ypad * 2);

	hdr.scale = edata.dres;

	int imin = (int)(hdr.emin / hdr.scale);
	int imax = (int)(hdr.emax / hdr.scale);
	int shift;

	while ((imax - imin) > (1 << 16)) { // need to rescale to fit range
		hdr.scale *= 2.0;
		imin = (int)(hdr.emin / hdr.scale);
		imax = (int)(hdr.emax / hdr.scale);
	}

	if (imin == imax) {
		hdr.dtype = 0;
		shift = imin;
	}
	else if (imax - imin < 256) {
		hdr.dtype = 8;
		if (imin >= 0 && imax < 256)
			shift = 0;
		else
			shift = imin;
	}
	else {
		hdr.dtype = -16;
		if (imin >= SHRT_MIN && imax < SHRT_MAX)
			shift = 0;
		else
			shift = (imin + imax) / 2;
	}
	hdr.offset = shift * hdr.scale;

	FILE *f = fopen(fname, "wb");
	fwrite(&hdr, sizeof(ELEVFILEHEADER), 1, f);
	if (hdr.dtype == 8) {
		for (int i = 0; i < edata.data.size(); i++) {
			UINT8 v = fabs(edata.data[i] - ebasedata.data[i]) < EPS ? UCHAR_MAX :
				(UINT8)((int)(edata.data[i] / hdr.scale) - shift);
			fwrite(&v, sizeof(UINT8), 1, f);
		}
	}
	else if (hdr.dtype == -16) {
		for (int i = 0; i < edata.data.size(); i++) {
			INT16 v = fabs(edata.data[i] - ebasedata.data[i]) < EPS ? SHRT_MAX :
				(INT16)((int)(edata.data[i] / hdr.scale) - shift);
			fwrite(&v, sizeof(INT16), 1, f);
		}
	}
	fclose(f);
}

// ==================================================================================

bool elvread_png(const char *fname, const ElevPatchMetaInfo &meta, ElevData &edata)
{
	bool ok = false;
	png_image image;
	memset(&image, 0, sizeof(png_image));
	image.version = PNG_IMAGE_VERSION;
	image.opaque = NULL;
	if (png_image_begin_read_from_file(&image, fname)) {
		image.format = PNG_FORMAT_LINEAR_Y;

		int nblock_x = meta.ilng1 - meta.ilng0;
		int nblock_y = meta.ilat1 - meta.ilat0;
		int w = nblock_x*TILE_FILERES + 3;
		int h = nblock_y*TILE_FILERES + 3;
		int n = w*h;
		unsigned short *buf = new unsigned short[n];
		png_image_finish_read(&image, NULL, buf, w, NULL);

		double scale = meta.scale;
		double offset = meta.offset;
		double vmin = meta.dmin;
		double vmax = meta.dmax;
		double s = (vmax - vmin) / (double)USHRT_MAX;
		edata.dres = scale;

		int idx = 0;
		for (int ih = h - 1; ih >= 0; ih--) {
			for (int iw = 0; iw < w; iw++) {
				unsigned short v16 = buf[idx++];
				double v = (double)v16 * s + vmin;
				edata.data[iw + ih*w] = v;
			}
		}
		delete[]buf;
		ok = true;
	}
	png_image_free(&image);
	return ok;
}

// ==================================================================================

void elvwrite_png(const char *fname, const ElevData &edata, double vmin, double vmax)
{
	int w = edata.width;
	int h = edata.height;
	int n = w*h;
	unsigned short *buf = new unsigned short[n];

	const double v16max = (double)USHRT_MAX;
	double scale = v16max / (vmax - vmin);
	int idx = 0;

	for (int ih = h - 1; ih >= 0; ih--) {
		for (int iw = 0; iw < w; iw++) {
			double v = edata.data[iw + ih*w];
			double vmap = (v - vmin)*scale;
			vmap = max(0.0, min(v16max, vmap));
			buf[idx++] = (unsigned short)vmap;
		}
	}

	png_image image;
	image.opaque = NULL;
	image.version = PNG_IMAGE_VERSION;
	image.format = PNG_FORMAT_LINEAR_Y;
	image.width = w;
	image.height = h;
	image.flags = 0;
	image.colormap_entries = 0;
	png_image_write_to_file(&image, fname, 0, buf, 0, 0);
	png_image_free(&image);
	delete[]buf;
}
