#ifndef ELV_IO_H
#define ELV_IO_H

#include "elevtile.h"

struct ElevPatchMetaInfo
{
	int lvl;
	int ilat0, ilat1, ilng0, ilng1;
	double dmin, dmax;
	double scale, offset;
	double latmin, latmax, lngmin, lngmax;
	int type;
	int colormap;
	std::vector<std::pair<int, int> > missing;
};

ElevData elvread(const char *fname);
bool elvmodread(const char *fname, ElevData &edata);

ElevData elvscan(const BYTE *data, int ndata);
bool elvmodscan(const BYTE *data, int ndata, ElevData &edata);

void elvwrite(const char *fname, const ElevData &edata, double latmin, double latmax, double lngmin, double lngmax);
void elvmodwrite(const char *fname, const ElevData &edata, const ElevData &ebasedata, double latmin, double latmax, double lngmin, double lngmax);

bool elvread_png(const char *fname, const ElevPatchMetaInfo &meta, ElevData &edata);
void elvwrite_png(const char *fname, const ElevData &edata, double vmin, double vmax);

#endif // !ELV_IO_H
