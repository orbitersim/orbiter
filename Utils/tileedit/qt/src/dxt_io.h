#ifndef DXT_IO_H
#define DXT_IO_H

#include "tile.h"

struct SurfPatchMetaInfo
{
	int lvl;
	int ilat0, ilat1, ilng0, ilng1;
	double latmin, latmax, lngmin, lngmax;
	std::vector<std::pair<int, int> > missing;
	bool alphaBlend;
	int colourMatch;
};

void dxt1write(const char *fname, const Image &idata);

bool pngread_tmp(const char *fname, Image &idata);
void pngwrite_tmp(const char *fname, const Image &idata);

int dxtread_png(const char *fname, const SurfPatchMetaInfo &meta, Image &sdata);

#endif // !DXT_IO_H