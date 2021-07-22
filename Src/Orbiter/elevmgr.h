// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#ifndef __ELEVMGR_H
#define __ELEVMGR_H

#include "windows.h"
#include "vecmat.h"
#include "ZTreeMgr.h"
#include <vector>

class CelestialBody;

struct ElevationTile {
	ElevationTile() { data = 0; last_access = 0.0; }
	~ElevationTile() { if (data) delete []data; }
	void Clear() { if (data) { delete[]data; data = 0; } last_access = 0.0; }
	INT16 *data;
	int lvl, tgtlvl;
	double latmin, latmax;
	double lngmin, lngmax;
	double emin, emax;
	double last_access;
	int lat0, lng0;
	bool celldiag;
	int nmlidx;
	Vector normal;
};

class ElevationManager {
public:
	ElevationManager (const CelestialBody *_cbody);
	~ElevationManager();
	double Elevation (double lat, double lng, int reqlvl=0, std::vector<ElevationTile> *tilecache = 0, Vector *normal=0, int *lvl=0) const;
	/**
	* \brief Synthesize an elevation tile by interpolating from the parent
	* \param ilat latitude index of target tile
	* \param ilng longitude index of target tile
	* \param lvl resolution level of target tile
	* \param pilat latitude index of source tile
	* \param pilng longitude index of source tile
	* \param plvl resolution level of source tile
	* \param pelev source tile data
	* \param [out] elev target tile data
	* \param emean if != 0, receives mean elevation
	* \note Requires lvl > plvl. The parent tile parameters must be an actual parent of the target (i.e. target covers a sub-area of the parent)
	*/
	void ElevationGrid (int ilat, int ilng, int lvl, int pilat, int pilng, int plvl, INT16* pelev, INT16 *elev, double *emean=0) const;

protected:
	bool TileIdx (double lat, double lng, int lvl, int *ilat, int *ilng) const;
	INT16 *LoadElevationTile (int lvl, int ilat, int ilng, double tgt_res) const;
	bool ElevationManager::LoadElevationTile_mod (int lvl, int ilat, int ilng, double tgt_res, INT16 *elev) const;

private:
	const CelestialBody *cbody;
	int maxlvl;
	int mode;  // elevation mode (0=no elevation, 1=linear interpolation, 2=cubic interpolation)
	double elev_res;  // elevation resolution [m]
	DWORD tilesource; // bit 1: try loading from cache, bit 2: try loading from archive
	ZTreeMgr *treeMgr[5];
};

#endif // !__ELEVMGR_H