#include <windows.h>
#include <mex.h>
#include <iostream>

struct DDSPIXELFORMAT {
	DWORD dwSize;
	DWORD dwFlags;
	DWORD dwFourCC;
	DWORD dwRGBBitCount;
	DWORD dwRBitMask;
	DWORD dwGBitMask;
	DWORD dwBBitMask;
	DWORD dwABitMask;
};

struct DDSHEADER {
	DWORD dwSize;
	DWORD dwFlags;
	DWORD dwHeight;
	DWORD dwWidth;
	DWORD dwLinearSize;
	DWORD dwDepth;
	DWORD dwMipmapCount;
	DWORD dwReserved[11];
	DDSPIXELFORMAT ddspf;
	DWORD dwCaps;
	DWORD dwCaps2;
	DWORD dwCaps3;
	DWORD dwCaps4;
	DWORD dwReserved2;
};

void ExtractDXT1 (WORD *data, DWORD ndata, mxArray *res);

void mexFunction (int nlhs, mxArray *plhs[], int nrhs,
    const mxArray *prhs[])
{
	char fname[256];
	if (mxGetString (prhs[0], fname, 256))
		mexErrMsgTxt("ddsread: arg 1: expected string");

	FILE *f = fopen(fname, "rb");
	if (!f) {
		plhs[0] = mxCreateDoubleMatrix(0,0,mxREAL);
		return;
	}

	char magic[4];
	fread(magic,sizeof(char),4,f);
	if (strncmp(magic,"DDS ",4))
		mexErrMsgTxt("ddsread: invalid file format");

	DDSHEADER ddsh;
	fread (&ddsh, sizeof(DDSHEADER), 1, f);
	if (ddsh.dwSize != sizeof(DDSHEADER))
		mexErrMsgTxt("ddsread: invalid header size");

	if (strncmp((char*)&ddsh.ddspf.dwFourCC, "DXT1", 4))
		mexErrMsgTxt("ddsread: only implemented for DXT1 format");

	DWORD ndata = ddsh.dwLinearSize/2;
	WORD *data = new WORD[ndata];
	if (fread (data, sizeof(WORD), ndata, f) < ndata)
		mexErrMsgTxt("ddsread: unexpected end of file");
	fclose(f);

	mwSize ndim = 3;
	mwSize dims[3] = {ddsh.dwHeight, ddsh.dwWidth, 4};
	plhs[0] = mxCreateNumericArray(ndim, dims, mxUINT8_CLASS, mxREAL);
	ExtractDXT1 (data, ndata, plhs[0]);

	delete []data;
}

void ExtractDXT1 (WORD *data, DWORD ndata, mxArray *res)
{
	mwSize i;
	WORD *d, c0, c1;
	DWORD lookup, idx, shift, xx, yy;
	bool noalpha;

	BYTE *v = (BYTE*)mxGetData(res);
	const mwSize *dims = mxGetDimensions(res);
	mwSize h = dims[0];
	mwSize w = dims[1];
	mwSize len = h*w;
	BYTE *R = v;
	BYTE *G = v+len;
	BYTE *B = v+len*2;
	BYTE *A = v+len*3;
	
	size_t nxblock = w/4;
	size_t nyblock = h/4;
	mwSize ofs = 0;
	WORD r[4], g[4], b[4];

	for (i = 0; i < len; i++)
		A[i] = 255; // opaque alpha channel by default

	for (mwSize y = 0; y < nyblock; y++) {
		for (mwSize x = 0; x < nxblock; x++) {
			d = data+ofs;
			c0 = d[0];
			c1 = d[1];
			noalpha = (c0 > c1);
			lookup = *(DWORD*)(d+2);

			r[0] = (c0 >> 11) << 3;
			g[0] = ((c0 >> 5) & 63) << 2;
			b[0] = (c0 & 31) << 3;
			r[1] = (c1 >> 11) << 3;
			g[1] = ((c1 >> 5) & 63) << 2;
			b[1] = (c1 & 31) << 3;
			if (noalpha) {
				r[2] = (r[0]*2 + r[1])/3;
				g[2] = (g[0]*2 + g[1])/3;
				b[2] = (b[0]*2 + b[1])/3;
				r[3] = (r[0] + r[1]*2)/3;
				g[3] = (g[0] + g[1]*2)/3;
				b[3] = (b[0] + b[1]*2)/3;
			} else {
				r[2] = (r[0]+r[1])/2;
				g[2] = (g[0]+g[1])/2;
				b[2] = (b[0]+b[1])/2;
				r[3] = g[3] = b[3] = 0;
			}
			for (yy = 0; yy < 4; yy++) {
				for (xx = 0; xx < 4; xx++) {
					shift = (xx+yy*4)*2;
					idx = (lookup >> shift) & 3;
					R[(y*4+yy) + (x*4+xx)*h] = (BYTE)r[idx];
					G[(y*4+yy) + (x*4+xx)*h] = (BYTE)g[idx];
					B[(y*4+yy) + (x*4+xx)*h] = (BYTE)b[idx];
					if (idx == 3 && !noalpha)
						A[(y*4+yy) + (x*4+xx)*h] = 0;
				}
			}
			ofs += 4;
		}
	}
}