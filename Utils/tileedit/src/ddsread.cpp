#include <windows.h>
#include <iostream>
#include "ddsread.h"

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

std::vector<DWORD> ExtractDXT1 (WORD *data, DWORD ndata, DWORD height, DWORD width);

Image ddsread(const char *fname)
{
    Image img;

    FILE *f = fopen(fname, "rb");
    if (!f) {
        return img;
    }

    char magic[4];
    fread(magic, sizeof(char), 4, f);
    if (strncmp(magic, "DDS ", 4)) {
        std::cerr << "ddsread: Invalid file format" << std::endl;
        exit(1);
    }

    DDSHEADER ddsh;
    fread(&ddsh, sizeof(DDSHEADER), 1, f);
    if (ddsh.dwSize != sizeof(DDSHEADER)) {
        std::cerr << "ddsread: Invalid header size" << std::endl;
        exit(1);
    }

    if (strncmp((char*)&ddsh.ddspf.dwFourCC, "DXT1", 4)) {
        std::cerr << "ddsread: Only implemented for DXT1 format" << std::endl;
        exit(1);
    }

    DWORD ndata = ddsh.dwLinearSize/2;
    WORD *data = new WORD[ndata];
    if (fread (data, sizeof(WORD), ndata, f) < ndata) {
        std::cerr << "ddsread: Unexpected end of file" << std::endl;
        exit(1);
    }
    fclose(f);

    img.data = ExtractDXT1(data, ndata, ddsh.dwHeight, ddsh.dwWidth);
    img.width = ddsh.dwWidth;
    img.height = ddsh.dwHeight;

    delete []data;
    return img;
}


Image ddsscan(const BYTE *data, int ndata)
{
	Image img;

	if (ndata < 4 || strncmp((char*)data, "DDS ", 4)) {
		std::cerr << "ddsread: Invalid file format" << std::endl;
		exit(1);
	}
	data += 4;
	ndata -= 4;

	DDSHEADER ddsh;
	memcpy(&ddsh, data, sizeof(DDSHEADER));
	data += sizeof(DDSHEADER);
	ndata -= sizeof(DDSHEADER);
	if (ddsh.dwSize != sizeof(DDSHEADER)) {
		std::cerr << "ddsread: Invalid header size" << std::endl;
		exit(1);
	}

	if (strncmp((char*)&ddsh.ddspf.dwFourCC, "DXT1", 4)) {
		std::cerr << "ddsread: Only implemented for DXT1 format" << std::endl;
		exit(1);
	}

	if (ndata < ddsh.dwLinearSize) {
		std::cerr << "ddsread: Unexpected end of file" << std::endl;
		exit(1);
	}
	ndata = ddsh.dwLinearSize / 2;

	img.data = ExtractDXT1((WORD*)data, ndata, ddsh.dwHeight, ddsh.dwWidth);
	img.width = ddsh.dwWidth;
	img.height = ddsh.dwHeight;

	return img;
}


std::vector<DWORD> ExtractDXT1 (WORD *data, DWORD ndata, DWORD h, DWORD w)
{
    int i;
    WORD *d, c0, c1;
    DWORD lookup, idx, tgt, shift, xx, yy;
    bool noalpha;

    DWORD len = h*w;
    DWORD A, R, G, B;

    size_t nxblock = w/4;
    size_t nyblock = h/4;
    size_t ofs = 0;
    WORD r[4], g[4], b[4];

    std::vector<DWORD> argb(len);

    for (size_t y = 0; y < nyblock; y++) {
        for (size_t x = 0; x < nxblock; x++) {
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
                    R = (BYTE)r[idx];
                    G = (BYTE)g[idx];
                    B = (BYTE)b[idx];
                    A = (idx == 3 && !noalpha ? 0 : 0xFF);
                    //argb[(y*4+yy) + (x*4+xx)*h] = (A << 0x18) | (R << 0x10) | (G << 0x08) | B;
                    argb[(y*4+yy)*w + (x*4+xx)] = (A << 0x18) | (R << 0x10) | (G << 0x08) | B;
                }
            }
            ofs += 4;
        }
    }
    return argb;
}
