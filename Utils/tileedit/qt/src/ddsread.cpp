#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include "ddsread.h"

struct DDSPIXELFORMAT {
    uint32_t dwSize;
    uint32_t dwFlags;
    uint32_t dwFourCC;
    uint32_t dwRGBBitCount;
    uint32_t dwRBitMask;
    uint32_t dwGBitMask;
    uint32_t dwBBitMask;
    uint32_t dwABitMask;
};

struct DDSHEADER {
    uint32_t dwSize;
    uint32_t dwFlags;
    uint32_t dwHeight;
    uint32_t dwWidth;
    uint32_t dwLinearSize;
    uint32_t dwDepth;
    uint32_t dwMipmapCount;
    uint32_t dwReserved[11];
    DDSPIXELFORMAT ddspf;
    uint32_t dwCaps;
    uint32_t dwCaps2;
    uint32_t dwCaps3;
    uint32_t dwCaps4;
    uint32_t dwReserved2;
};

static_assert(sizeof(DDSPIXELFORMAT) == 32 && sizeof(DDSHEADER) == 124, "DDS header must match the on-disk layout");

std::vector<uint32_t> ExtractDXT1 (uint16_t *data, uint32_t ndata, uint32_t height, uint32_t width);

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

    uint32_t ndata = ddsh.dwLinearSize/2;
    uint16_t *data = new uint16_t[ndata];
    if (fread (data, sizeof(uint16_t), ndata, f) < ndata) {
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


Image ddsscan(const uint8_t *data, int ndata)
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

	img.data = ExtractDXT1((uint16_t*)data, ndata, ddsh.dwHeight, ddsh.dwWidth);
	img.width = ddsh.dwWidth;
	img.height = ddsh.dwHeight;

	return img;
}


std::vector<uint32_t> ExtractDXT1 (uint16_t *data, uint32_t ndata, uint32_t h, uint32_t w)
{
    int i;
    uint16_t *d, c0, c1;
    uint32_t lookup, idx, tgt, shift, xx, yy;
    bool noalpha;

    uint32_t len = h*w;
    uint32_t A, R, G, B;

    size_t nxblock = w/4;
    size_t nyblock = h/4;
    size_t ofs = 0;
    uint16_t r[4], g[4], b[4];

    std::vector<uint32_t> argb(len);

    for (size_t y = 0; y < nyblock; y++) {
        for (size_t x = 0; x < nxblock; x++) {
            d = data+ofs;
            c0 = d[0];
            c1 = d[1];
            noalpha = (c0 > c1);
            memcpy(&lookup, d+2, sizeof(lookup));

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
                    R = (uint8_t)r[idx];
                    G = (uint8_t)g[idx];
                    B = (uint8_t)b[idx];
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
