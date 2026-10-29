// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#include "elevmgr.h"
#include "Celbody.h"
#include "Planet.h"
#include "Orbiter.h"
#include <algorithm>
#include <filesystem>
#include <type_traits>

using std::min;
using std::max;

static int elev_grid = 256;
static int elev_stride = elev_grid+3;
static int MAXLVL_LIMIT = SURF_MAX_PATCHLEVEL2 - 7;

extern Orbiter *g_pOrbiter;
extern TimeData td;
extern char DBG_MSG[256];

#pragma pack(push,1)

struct ELEVFILEHEADER { // file header for patch elevation data file
	char id[4];            // ID string + version ('E','L','E',1)
	int hdrsize;           // header size (100 expected)
	int dtype;             // data format (0=flat, no data block; 8=uint8; -8=int8; 16=uint16; -16=int16)
	int xgrd,ygrd;         // data grid size (259 x 259 expected)
	int xpad,ypad;         // horizontal, vertical padding width (1, 1 expected)
	double scale;          // data scaling factor (1.0 expected)
	double offset;         // data offset (elevation = raw value * scale + offset)
	double latmin, latmax; // latitude range [rad]
	double lngmin, lngmax; // longitude range [rad]
	double emin, emax, emean; // min, max, mean elevation [m]
};

#pragma pack(pop)

ElevationManager::ElevationManager (const CelestialBody *_cbody)
: cbody(_cbody)
{
	mode = g_pOrbiter->Cfg()->CfgVisualPrm.ElevMode;
	tilesource = g_pOrbiter->Cfg()->CfgPRenderPrm.TileLoadFlags;
	maxlvl = MAXLVL_LIMIT;
	elev_res = 1.0;
	if (cbody->Type() == OBJTP_PLANET) {
		maxlvl = min ((DWORD)maxlvl, ((Planet*)cbody)->MaxPatchLevel()-7);
		// -7: -4 for level offset of quadtree root, -3 for great-grandfather elevation access mode
		elev_res = ((Planet*)cbody)->ElevationResolution();
	}
	if (tilesource & 0x0002) {
		char cbuf[256];
		g_pOrbiter->Cfg()->PTexPath (cbuf, cbody->Name());
		treeMgr[0] = ZTreeMgr::CreateFromFile(cbuf, ZTreeMgr::LAYER_ELEV);
		treeMgr[1] = ZTreeMgr::CreateFromFile(cbuf, ZTreeMgr::LAYER_ELEVMOD);
	} else {
		for (int i = 0; i < 2; i++)
			treeMgr[i] = 0;
	}

	// Check if Elev dir exists
	char path[MAX_PATH]; char fname[MAX_PATH];
	sprintf(fname, "%s\\Elev", cbody->Name());
	g_pOrbiter->Cfg()->PTexPath(path, fname);
	auto x = std::filesystem::status(path);
	bDirExists = std::filesystem::is_directory(x);

	sprintf(fname, "%s\\Elev_mod", cbody->Name());
	g_pOrbiter->Cfg()->PTexPath(path, fname);
	auto y = std::filesystem::status(path);
	bModExists = std::filesystem::is_directory(y);

	bFlats = g_pOrbiter->Cfg()->CfgVisualPrm.bFlats;
	if (auto gc = g_pOrbiter->GetGraphicsClient()) {
		DWORD val;
		if (gc->clbkGetRenderParam(RP_TERRAINFLATS, &val))
			bFlats = (val != 0);
	}
	if (bFlats)
		LoadFlats();
}

ElevationManager::~ElevationManager ()
{
	if (local_cache) delete local_cache;
	for (int i = 0; i < 2; i++)
		if (treeMgr[i])
			delete treeMgr[i];
}


int ElevationManager::Quadrant(double lat, double lng, int lvl) const
{
	// 0 = North-West, 1 = North-East, 2 = South-West, 3 = South-East corner
	int nlat = 1 << (lvl + 1);
	int nlng = 2 << (lvl + 1);
	int ilat = (int)((Pi05 - lat) / Pi * nlat);
	int ilng = (int)((lng + Pi) / Pi2 * nlng);
	int q = 0;
	if (ilng & 1) q += 1;
	if (ilat & 1) q += 2;
	return q;
}

bool ElevationManager::TileIdx (double lat, double lng, int lvl, int *ilat, int *ilng) const
{
	int nlat = 1 << lvl;
	int nlng = 2 << lvl;

	*ilat = (int)((Pi05-lat)/Pi * nlat);
	*ilng = (int)((lng+Pi)/Pi2 * nlng);
	return true;
}

bool ElevationManager::HasElevationTile(int lvl, int ilat, int ilng) const
{
	if (mode) {
		if (tilesource & 0x0001 && bDirExists) {
			char fname[256], path[256];
			sprintf(fname, "%s\\Elev\\%02d\\%06d\\%06d.elv", cbody->Name(), lvl, ilat, ilng);
			g_pOrbiter->Cfg()->PTexPath(path, fname);
			if (std::filesystem::exists(path)) return true;
		}
		if (treeMgr[0]) {
			if (treeMgr[0]->Idx(lvl, ilat, ilng) != DWORD(-1)) return true;
		}
	}
	return false;
}

INT16 *ElevationManager::LoadElevationTile (int lvl, int ilat, int ilng, double tgt_res) const
{
	INT16 *elev = 0;
	INT16 ofs;

	if (mode) {
		int i;
		const int ndat = elev_stride*elev_stride;
		double scale, offset;
		if (tilesource & 0x0001 && bDirExists) {
			FILE *f;
			char fname[256], path[256];
			sprintf (fname, "%s\\Elev\\%02d\\%06d\\%06d.elv", cbody->Name(), lvl, ilat, ilng);
			g_pOrbiter->Cfg()->PTexPath(path, fname);
			if (f = fopen(path, "rb")) {
				elev = new INT16[ndat];
				ELEVFILEHEADER hdr;
				fread (&hdr, sizeof(ELEVFILEHEADER), 1, f);
				if (hdr.hdrsize != sizeof(ELEVFILEHEADER)) {
					fseek (f, hdr.hdrsize, SEEK_SET);
				}
				scale  = hdr.scale;
				offset = hdr.offset;
				switch (hdr.dtype) {
				case 0: // flat tile, defined by offset
					for (i = 0; i < ndat; i++) elev[i] = 0;
					break;
				case 8: {
					UINT8 *tmp = new UINT8[ndat];
					fread (tmp, sizeof(UINT8), ndat, f);
					for (i = 0; i < ndat; i++)
						elev[i] = (INT16)tmp[i];
					delete []tmp;
					tmp = NULL;
					}
					break;
				case -16:
					fread (elev, sizeof(INT16), ndat, f);
					break;
				}
				fclose(f);
			}
		}
		if (!elev && treeMgr[0]) {
			BYTE *buf;
			DWORD ndata = treeMgr[0]->ReadData(lvl, ilat, ilng, &buf);
			if (ndata) {
				BYTE *p = buf;
				elev = new INT16[ndat];
				ELEVFILEHEADER *phdr = (ELEVFILEHEADER*)p;
				p += phdr->hdrsize;
				scale  = phdr->scale;
				offset = phdr->offset;
				switch (phdr->dtype) {
				case 0: // flat tile, defined by offset
					for (i = 0; i < ndat; i++) elev[i] = 0;
					break;
				case 8:
					for (i = 0; i < ndat; i++)
						elev[i] = (INT16)(*p++);
					break;
				case -16:
					memcpy(elev, p, ndat*sizeof(INT16));
					p += ndat*sizeof(INT16);
					break;
				}
				treeMgr[0]->ReleaseData(buf);
			}
		}
		if (elev) {
			if (scale != tgt_res) { // rescale the data
				double rescale = scale/tgt_res;
				for (i = 0; i < ndat; i++)
					elev[i] = (INT16)(elev[i]*rescale);
			}
			if (offset) {
				INT16 sofs = (INT16)(offset/tgt_res);
				for (i = 0; i < ndat; i++)
					elev[i] += sofs;
			}
		}
	}
	return elev;
}

bool ElevationManager::LoadElevationTile_mod (int lvl, int ilat, int ilng, double tgt_res, INT16 *elev) const
{
	if (mode) {
		int i;
		const int ndat = elev_stride*elev_stride;
		double rescale;
		INT16 offset;
		bool do_shift, do_rescale;
		if (tilesource & 0x0001 && bModExists) {
			FILE *f;
			char fname[256], path[256];
			sprintf (fname, "%s\\Elev_mod\\%02d\\%06d\\%06d.elv", cbody->Name(), lvl, ilat, ilng);
			g_pOrbiter->Cfg()->PTexPath(path, fname);
			if (f = fopen(path, "rb")) {
				ELEVFILEHEADER hdr;
				fread (&hdr, sizeof(ELEVFILEHEADER), 1, f);
				if (hdr.hdrsize != sizeof(ELEVFILEHEADER)) {
					fseek (f, hdr.hdrsize, SEEK_SET);
				}
				rescale = (do_rescale = (hdr.scale != tgt_res)) ? hdr.scale/tgt_res : 1.0;
				offset  = (do_shift   = (hdr.offset != 0.0)) ? (INT16)(hdr.offset/tgt_res) : 0;
				switch (hdr.dtype) {
				case 0: // overwrite the entire tile with a flat offset
					for (i = 0; i < ndat; i++) elev[i] = offset;
					break;
				case 8: {
					const UINT8 mask = UCHAR_MAX;
					UINT8 *tmp = new UINT8[ndat];
					fread (tmp, sizeof(UINT8), ndat, f);
					for (i = 0; i < ndat; i++) {
						if (tmp[i] != mask) {
							elev[i] = (INT16)(do_rescale ? (INT16)(tmp[i]*rescale) : (INT16)tmp[i]);
							if (do_shift) elev[i] += offset;
						}
					}
					delete []tmp;
					tmp = NULL;
					}
					break;
				case -16: {
					const INT16 mask = SHRT_MAX;
					INT16 *tmp = new INT16[ndat];
					INT16 ofs = (INT16)hdr.offset;
					fread (tmp, sizeof(INT16), ndat, f);
					for (i = 0; i < ndat; i++) {
						if (tmp[i] != mask) {
							elev[i] = (do_rescale ? (INT16)(tmp[i]*rescale) : tmp[i]);
							if (do_shift) elev[i] += offset;
						}
					}
					delete []tmp;
					tmp = NULL;
					}
					break;
				}
				fclose(f);
				return true;
			}
		}
		if (treeMgr[1]) {
			BYTE *buf;
			DWORD ndata = treeMgr[1]->ReadData(lvl, ilat, ilng, &buf);
			if (ndata) {
				BYTE *p = buf;
				ELEVFILEHEADER *phdr = (ELEVFILEHEADER*)p;
				p += phdr->hdrsize;
				INT16 ofs = (INT16)phdr->offset;
				rescale = (do_rescale = (phdr->scale != tgt_res)) ? phdr->scale/tgt_res : 1.0;
				offset  = (do_shift   = (phdr->offset != 0.0)) ? (INT16)(phdr->offset/tgt_res) : 0;
				switch (phdr->dtype) {
				case 0:
					for (i = 0; i < ndat; i++) elev[i] = offset;
					break;
				case 8: {
					const UINT8 mask = UCHAR_MAX;
					for (i = 0; i < ndat; i++) {
						if (p[i] != mask) {
							elev[i] = (INT16)(do_rescale ? p[i]*rescale : p[i]);
							if (do_shift) elev[i] += offset;
						}
					}
					} break;
				case -16: {
					const INT16 mask = SHRT_MAX;
					INT16 *buf16 = (INT16*)p;
					for (i = 0; i < ndat; i++) {
						if (buf16[i] != mask) {
							elev[i] = (do_rescale ? (INT16)(buf16[i]*rescale) : buf16[i]);
							if (do_shift) elev[i] += offset;
						}
					}
					} break;
				}
				treeMgr[1]->ReleaseData(buf);
				return true;
			}
		}
	}
	return false;
}

// Tile quadrants and mask bits
// +--------+--------+
// |        |        |
// |   NW   |   NE   |
// |  bit 0 |  bit 1 |
// +--------+--------+
// |        |        |
// |   SW   |   SE   |
// |  bit 2 |  bit 3 |
// +--------+--------+
// tile index (ilat=0 and ilng=0) lies in a NW corner
// lat(PI) = North pole, lat(-PI) = South Pole
// lng(-PI) = 180deg West, lng(PI) = 180deg East

double ElevationManager::Elevation (double lat, double lng, int reqlvl, std::vector<ElevationTile> *tilecache, Vector *normal, int *reslvl) const
{
	double e = 0.0;
	if (reslvl) *reslvl = 0;
	reqlvl = (reqlvl ? min (max(0,reqlvl-7), maxlvl) : maxlvl);

	if (mode) {
		ElevationTile *tile;
		int ntile = 0;
		if (tilecache) {
			tile = tilecache->data();
			ntile = tilecache->size();
		}

		if (!ntile) {
			if (!local_cache) local_cache = new std::vector<ElevationTile>(8);
			tile = local_cache->data();
			ntile = local_cache->size();
		}

		int i, lvl, ilat, ilng;
		ElevationTile *t = 0;

		for (i = 0; i < ntile; i++) {
			if (tile[i].data &&
				reqlvl == tile[i].tgtlvl && tile[i].mgr == this &&
				lat >= tile[i].latmin && lat <= tile[i].latmax &&
				lng >= tile[i].lngmin && lng <= tile[i].lngmax) {
				int q = -1;
				if (tile[i].quadrants != 0) { // Tile contain higher lvl data for some of it's quadtants
					q = 0;
					// Calculate quadrant being accessed
					if (lng > (tile[i].lngmin + tile[i].lngmax) * 0.5) q += 1;
					if (lat < (tile[i].latmin + tile[i].latmax) * 0.5) q += 2;
					if (tile[i].quadrants & (1 << q)) continue; // Tile not usable, continue search
				}
				//oapiWriteLogV("CacheHit idx=%d, lvl=%d, f=0x%X, q=%d, ilat=%d, ilng=%d", i, tile[i].lvl, tile[i].quadrants, q, tile[i].ilat, tile[i].ilng);
				t = tile + i;
				break;
			}
		}
		if (!t) { // correct tile not in list - need to load from file
			t = tile;  // find oldest tile
			for (i = 1; i < ntile; i++) 
				if (tile[i].last_access < t->last_access)
					t = tile+i;

			if (t->data) t->Clear();

			for (lvl = reqlvl; lvl >= 0; lvl--) {
				TileIdx (lat, lng, lvl, &ilat, &ilng);
				t->data = LoadElevationTile (lvl+4, ilat, ilng, elev_res);
				if (t->data) {
					LoadElevationTile_mod (lvl+4, ilat, ilng, elev_res, t->data); // load modifications
					int nlat = 1 << lvl;
					int nlng = 2 << lvl;
					t->mgr = this;
					t->lvl = lvl;
					t->ilat = ilat;
					t->ilng = ilng;
					t->tgtlvl = reqlvl;
					t->latmin = (0.5-(double)(ilat+1)/double(nlat))*Pi;
					t->latmax = (0.5-(double)ilat/double(nlat))*Pi;
					t->lngmin = (double)ilng/(double)nlng*Pi2 - Pi;
					t->lngmax = (double)(ilng+1)/(double)nlng*Pi2 - Pi;
					t->quadrants = 0;

					if (reqlvl > lvl) 
					{
						// Check if higher lvl data exists for any of the quadrants, 
						// set flag bit to mark it dirty (un-usable)
						int qlat = ilat * 2, qlng = ilng * 2, qlvl = lvl + 1;
						t->quadrants |= DWORD(HasElevationTile(qlvl + 4, qlat + 0, qlng + 0)) << 0; // NW
						t->quadrants |= DWORD(HasElevationTile(qlvl + 4, qlat + 0, qlng + 1)) << 1;	// NE
						t->quadrants |= DWORD(HasElevationTile(qlvl + 4, qlat + 1, qlng + 0)) << 2; // SW
						t->quadrants |= DWORD(HasElevationTile(qlvl + 4, qlat + 1, qlng + 1)) << 3;	// SE
					}

					//int q = Quadrant(lat, lng, lvl);
					//oapiWriteLogV("LoadTile[0x%X]: lvl=%d, flags=0x%X, q=%d, i(%d, %d)", t, lvl, t->quadrants, q, ilng, ilat);

					// still need to store emin and emax
					FilterElevation(ilat, ilng, lvl, elev_res, t->data);
					auto gc = g_pOrbiter->GetGraphicsClient();
					if (gc) gc->clbkFilterElevation((OBJHANDLE)cbody, ilat, ilng, lvl, elev_res, t->data);
					break;
				}
			}
			t->lat0 = t->lng0 = t->nmlidx = -1;
		}

		if (t->data) {
			INT16 *elev_base = t->data+elev_stride+1; // strip padding
			double latidx = (lat-t->latmin) * elev_grid/(t->latmax-t->latmin);
			double lngidx = (lng-t->lngmin) * elev_grid/(t->lngmax-t->lngmin);
			int lat0 = (int)latidx;
			int lng0 = (int)lngidx;
			INT16 *eptr = elev_base + lat0*elev_stride + lng0;
			if (mode == 1) { // linear interpolation
				bool tri;
				double w_lat = latidx-lat0;
				double w_lng = lngidx-lng0;

				double e01 = eptr[0]*(1.0-w_lng) + eptr[1]*w_lng;
				double e02 = eptr[elev_stride]*(1.0-w_lng) + eptr[elev_stride+1]*w_lng;
				e = e01*(1.0-w_lat) + e02*w_lat;

				if (normal) {
					double dlat = (t->latmax-t->latmin)/elev_grid;
					double dlng = (t->lngmax-t->lngmin)/elev_grid;
					double dz = dlat * cbody->Size();
					double dx = dlng * cbody->Size() * cos(lat);
					double nx01 = eptr[1]-eptr[0];
					double nx02 = eptr[elev_stride+1]-eptr[elev_stride];
					double nx = w_lat*nx02 + (1.0-w_lat)*nx01;
					Vector vnx(dx,nx,0);
					double nz01 = eptr[elev_stride]-eptr[0];
					double nz02 = eptr[elev_stride+1]-eptr[1];
					double nz = w_lng*nz02 + (1.0-w_lng)*nz01;
					Vector vnz(0,nz,dz);
					*normal = crossp(vnz,vnx).unit();
				}
			} else { // cubic spline interpolation
				double a_m1, a_0, a_p1, a_p2, b_m1, b_0, b_p1, b_p2;
				double tlat = latidx-lat0;
				double tlng = lngidx-lng0;
				a_m1 = eptr[-elev_stride-1];
				a_0  = eptr[-elev_stride];
				a_p1 = eptr[-elev_stride+1];
				a_p2 = eptr[-elev_stride+2];
				b_m1 = 0.5 * (2.0*a_0 + tlng*(-a_m1+a_p1) +
					tlng*tlng*(2.0*a_m1-5.0*a_0+4.0*a_p1-a_p2) +
					tlng*tlng*tlng*(-a_m1+3.0*a_0-3.0*a_p1+a_p2));
				a_m1 = eptr[-1];
				a_0  = eptr[0];
				a_p1 = eptr[1];
				a_p2 = eptr[2];
				b_0 = 0.5 * (2.0*a_0 + tlng*(-a_m1+a_p1) +
					tlng*tlng*(2.0*a_m1-5.0*a_0+4.0*a_p1-a_p2) +
					tlng*tlng*tlng*(-a_m1+3.0*a_0-3.0*a_p1+a_p2));
				a_m1 = eptr[elev_stride-1];
				a_0  = eptr[elev_stride];
				a_p1 = eptr[elev_stride+1];
				a_p2 = eptr[elev_stride+2];
				b_p1 = 0.5 * (2.0*a_0 + tlng*(-a_m1+a_p1) +
					tlng*tlng*(2.0*a_m1-5.0*a_0+4.0*a_p1-a_p2) +
					tlng*tlng*tlng*(-a_m1+3.0*a_0-3.0*a_p1+a_p2));
				a_m1 = eptr[2*elev_stride-1];
				a_0  = eptr[2*elev_stride];
				a_p1 = eptr[2*elev_stride+1];
				a_p2 = eptr[2*elev_stride+2];
				b_p2 = 0.5 * (2.0*a_0 + tlng*(-a_m1+a_p1) +
					tlng*tlng*(2.0*a_m1-5.0*a_0+4.0*a_p1-a_p2) +
					tlng*tlng*tlng*(-a_m1+3.0*a_0-3.0*a_p1+a_p2));
				e =	0.5 * (2.0*b_0 + tlat*(-b_m1+b_p1) +
					tlat*tlat*(2.0*b_m1-5.0*b_0+4.0*b_p1-b_p2) +
					tlat*tlat*tlat*(-b_m1+3.0*b_0-3.0*b_p1+b_p2));
				if (normal) {
					double dlat = (t->latmax-t->latmin)/elev_grid;
					double dlng = (t->lngmax-t->lngmin)/elev_grid;
					double dz = dlat * cbody->Size();
					double dx = dlng * cbody->Size() * cos(lat);
					double dex00 = 0.5*(eptr[1]-eptr[-1]);
					double dex01 = 0.5*(eptr[2]-eptr[0]);
					double dex10 = 0.5*(eptr[elev_stride+1]-eptr[elev_stride-1]);
					double dex11 = 0.5*(eptr[elev_stride+2]-eptr[elev_stride]);
					double dez00 = 0.5*(eptr[elev_stride]-eptr[-elev_stride]);
					double dez01 = 0.5*(eptr[elev_stride*2]-eptr[0]);
					double dez10 = 0.5*(eptr[elev_stride+1]-eptr[-elev_stride+1]);
					double dez11 = 0.5*(eptr[elev_stride*2+1]-eptr[1]);
					double w1_lat = latidx - lat0;
					double w0_lat = 1.0-w1_lat;
					double w1_lng = lngidx - lng0;
					double w0_lng = 1.0-w1_lng;
					double dex = (dex00+dex10)*0.5*w0_lng + (dex01+dex11)*0.5*w1_lng;
					double dez = (dez00+dez10)*0.5*w0_lat + (dez01+dez11)*0.5*w1_lat;
					normal->x = -dex;
					normal->z = -dez;
					normal->y = 0.5*(dx+dz);
					normal->unify();
				}
			}
			t->last_access = td.SysT0;
			t->lat0 = lat0;
			t->lng0 = lng0;
			if (reslvl) *reslvl = t->lvl+7;
		}
	}
	return e*elev_res;
}

void ElevationManager::ElevationGrid (int ilat, int ilng, int lvl, int pilat, int pilng, int plvl, INT16 *pelev, float *elev, double *emean) const
{
	int i, j, nmean;
	int nlng = 2 << lvl;
	int nlat = 1 << lvl;
	double lng, lat, e;
	double latmin = Pi05 * (double)(nlat-2*ilat-2)/(double)nlat;
	double latmax = Pi05 * (double)(nlat-2*ilat)/(double)nlat;
	double lngmin = Pi * (double)(2*ilng-nlng)/(double)nlng;
	double lngmax = Pi * (double)(2*ilng-nlng+2)/(double)nlng;
	double dlat = (latmax-latmin)/elev_grid;
	double dlng = (lngmax-lngmin)/elev_grid;

	int pnlng = 2 << plvl;
	int pnlat = 1 << plvl;
	double platmin = Pi05 * (double)(pnlat-2*pilat-2)/(double)pnlat;
	double platmax = Pi05 * (double)(pnlat-2*pilat)/(double)pnlat;
	double plngmin = Pi * (double)(2*pilng-pnlng)/(double)pnlng;
	double plngmax = Pi * (double)(2*pilng-pnlng+2)/(double)pnlng;
	int plat0 = -1000, plng0 = -1000;
	bool pcelldiag = false;

	float *elev_base = elev + elev_stride+1;
	INT16 *pelev_base = pelev + elev_stride+1;
	if (emean) {
		*emean = 0.0;
		nmean = 0;
	}
	for (i = -1; i <= elev_grid+1; i++) {
		lat = latmin + i*dlat;
		double latidx = (lat-platmin) * elev_grid/(platmax-platmin);
		int lat0 = (int)floor(latidx);
		for (j = -1; j <= elev_grid+1; j++) {
			lng = lngmin + j*dlng;
			double lngidx = (lng-plngmin) * elev_grid/(plngmax-plngmin);
			int lng0 = (int)floor(lngidx);

			INT16 *eptr = pelev_base + lat0*elev_stride + lng0;
			if (mode == 1) { // linear interpolation

				double w_lat = latidx-lat0;
				double w_lng = lngidx-lng0;
				e = (1.0-w_lat)*(eptr[0]*(1.0-w_lng) + eptr[1]*w_lng) + w_lat*(eptr[elev_stride]*(1.0-w_lng) + eptr[elev_stride+1]*w_lng);

			} else { // cubic spline interpolation

				// take care of boundaries
				int latstencil[4] = {-elev_stride,0,elev_stride,2*elev_stride};
				int lngstencil[4] = {-1,0,1,2};
				if (lat0 < 0) latstencil[0] = 0;
				else if (lat0 >= elev_grid) latstencil[3] = elev_stride;
				if (lng0 < 0) lngstencil[0] = 0;
				else if (lng0 >= elev_grid) lngstencil[3] = 1;

				double a_m1, a_0, a_p1, a_p2, b_m1, b_0, b_p1, b_p2;
				double tlat = latidx-lat0;
				double tlng = lngidx-lng0;
				a_m1 = eptr[latstencil[0]+lngstencil[0]];
				a_0  = eptr[latstencil[0]+lngstencil[1]];
				a_p1 = eptr[latstencil[0]+lngstencil[2]];
				a_p2 = eptr[latstencil[0]+lngstencil[3]];
				b_m1 = 0.5 * (2.0*a_0 + tlng*(-a_m1+a_p1) +
					tlng*tlng*(2.0*a_m1-5.0*a_0+4.0*a_p1-a_p2) +
					tlng*tlng*tlng*(-a_m1+3.0*a_0-3.0*a_p1+a_p2));
				a_m1 = eptr[latstencil[1]+lngstencil[0]];
				a_0  = eptr[latstencil[1]+lngstencil[1]];
				a_p1 = eptr[latstencil[1]+lngstencil[2]];
				a_p2 = eptr[latstencil[1]+lngstencil[3]];
				b_0 = 0.5 * (2.0*a_0 + tlng*(-a_m1+a_p1) +
					tlng*tlng*(2.0*a_m1-5.0*a_0+4.0*a_p1-a_p2) +
					tlng*tlng*tlng*(-a_m1+3.0*a_0-3.0*a_p1+a_p2));
				a_m1 = eptr[latstencil[2]+lngstencil[0]];
				a_0  = eptr[latstencil[2]+lngstencil[1]];
				a_p1 = eptr[latstencil[2]+lngstencil[2]];
				a_p2 = eptr[latstencil[2]+lngstencil[3]];
				b_p1 = 0.5 * (2.0*a_0 + tlng*(-a_m1+a_p1) +
					tlng*tlng*(2.0*a_m1-5.0*a_0+4.0*a_p1-a_p2) +
					tlng*tlng*tlng*(-a_m1+3.0*a_0-3.0*a_p1+a_p2));
				a_m1 = eptr[latstencil[3]+lngstencil[0]];
				a_0  = eptr[latstencil[3]+lngstencil[1]];
				a_p1 = eptr[latstencil[3]+lngstencil[2]];
				a_p2 = eptr[latstencil[3]+lngstencil[3]];
				b_p2 = 0.5 * (2.0*a_0 + tlng*(-a_m1+a_p1) +
					tlng*tlng*(2.0*a_m1-5.0*a_0+4.0*a_p1-a_p2) +
					tlng*tlng*tlng*(-a_m1+3.0*a_0-3.0*a_p1+a_p2));
				e =	0.5 * (2.0*b_0 + tlat*(-b_m1+b_p1) +
					tlat*tlat*(2.0*b_m1-5.0*b_0+4.0*b_p1-b_p2) +
					tlat*tlat*tlat*(-b_m1+3.0*b_0-3.0*b_p1+b_p2));
				e = max(-32767.0, min (32766.0, e));
			}
			plat0 = lat0;
			plng0 = lng0;
			elev_base[i*elev_stride+j] = (float)e;
			if (emean && i >= 0 && j >= 0 && i <= elev_grid && j <= elev_grid) {
				*emean += e;
				nmean++;
			}
		}
	}
	if (emean && nmean)
		*emean /= nmean;
	if (FilterElevation(ilat, ilng, lvl, elev_res, elev) && emean) {
		double sum = 0.0;
		for (i = 0; i <= elev_grid; i++)
			for (j = 0; j <= elev_grid; j++)
				sum += elev_base[i*elev_stride+j];
		*emean = sum / ((elev_grid + 1) * (elev_grid + 1));
	}
}



void ElevationManager::ElevationGrid(int ilat, int ilng, int lvl, int pilat, int pilng, int plvl, INT16* pelev, INT16* elev, double* emean) const
{
	int i, j, nmean;
	int nlng = 2 << lvl;
	int nlat = 1 << lvl;
	double lng, lat, e;
	double latmin = Pi05 * (double)(nlat - 2 * ilat - 2) / (double)nlat;
	double latmax = Pi05 * (double)(nlat - 2 * ilat) / (double)nlat;
	double lngmin = Pi * (double)(2 * ilng - nlng) / (double)nlng;
	double lngmax = Pi * (double)(2 * ilng - nlng + 2) / (double)nlng;
	double dlat = (latmax - latmin) / elev_grid;
	double dlng = (lngmax - lngmin) / elev_grid;

	int pnlng = 2 << plvl;
	int pnlat = 1 << plvl;
	double platmin = Pi05 * (double)(pnlat - 2 * pilat - 2) / (double)pnlat;
	double platmax = Pi05 * (double)(pnlat - 2 * pilat) / (double)pnlat;
	double plngmin = Pi * (double)(2 * pilng - pnlng) / (double)pnlng;
	double plngmax = Pi * (double)(2 * pilng - pnlng + 2) / (double)pnlng;
	int plat0 = -1000, plng0 = -1000;
	bool pcelldiag = false;

	INT16* elev_base = elev + elev_stride + 1;
	INT16* pelev_base = pelev + elev_stride + 1;
	if (emean) {
		*emean = 0.0;
		nmean = 0;
	}
	for (i = -1; i <= elev_grid + 1; i++) {
		lat = latmin + i * dlat;
		double latidx = (lat - platmin) * elev_grid / (platmax - platmin);
		int lat0 = (int)floor(latidx);
		for (j = -1; j <= elev_grid + 1; j++) {
			lng = lngmin + j * dlng;
			double lngidx = (lng - plngmin) * elev_grid / (plngmax - plngmin);
			int lng0 = (int)floor(lngidx);

			INT16* eptr = pelev_base + lat0 * elev_stride + lng0;
			if (mode == 1) { // linear interpolation

				double w_lat = latidx - lat0;
				double w_lng = lngidx - lng0;
				e = (1.0 - w_lat) * (eptr[0] * (1.0 - w_lng) + eptr[1] * w_lng) + w_lat * (eptr[elev_stride] * (1.0 - w_lng) + eptr[elev_stride + 1] * w_lng);

			}
			else { // cubic spline interpolation

			 // take care of boundaries
				int latstencil[4] = { -elev_stride,0,elev_stride,2 * elev_stride };
				int lngstencil[4] = { -1,0,1,2 };
				if (lat0 < 0) latstencil[0] = 0;
				else if (lat0 >= elev_grid) latstencil[3] = elev_stride;
				if (lng0 < 0) lngstencil[0] = 0;
				else if (lng0 >= elev_grid) lngstencil[3] = 1;

				double a_m1, a_0, a_p1, a_p2, b_m1, b_0, b_p1, b_p2;
				double tlat = latidx - lat0;
				double tlng = lngidx - lng0;
				a_m1 = eptr[latstencil[0] + lngstencil[0]];
				a_0 = eptr[latstencil[0] + lngstencil[1]];
				a_p1 = eptr[latstencil[0] + lngstencil[2]];
				a_p2 = eptr[latstencil[0] + lngstencil[3]];
				b_m1 = 0.5 * (2.0 * a_0 + tlng * (-a_m1 + a_p1) +
					tlng * tlng * (2.0 * a_m1 - 5.0 * a_0 + 4.0 * a_p1 - a_p2) +
					tlng * tlng * tlng * (-a_m1 + 3.0 * a_0 - 3.0 * a_p1 + a_p2));
				a_m1 = eptr[latstencil[1] + lngstencil[0]];
				a_0 = eptr[latstencil[1] + lngstencil[1]];
				a_p1 = eptr[latstencil[1] + lngstencil[2]];
				a_p2 = eptr[latstencil[1] + lngstencil[3]];
				b_0 = 0.5 * (2.0 * a_0 + tlng * (-a_m1 + a_p1) +
					tlng * tlng * (2.0 * a_m1 - 5.0 * a_0 + 4.0 * a_p1 - a_p2) +
					tlng * tlng * tlng * (-a_m1 + 3.0 * a_0 - 3.0 * a_p1 + a_p2));
				a_m1 = eptr[latstencil[2] + lngstencil[0]];
				a_0 = eptr[latstencil[2] + lngstencil[1]];
				a_p1 = eptr[latstencil[2] + lngstencil[2]];
				a_p2 = eptr[latstencil[2] + lngstencil[3]];
				b_p1 = 0.5 * (2.0 * a_0 + tlng * (-a_m1 + a_p1) +
					tlng * tlng * (2.0 * a_m1 - 5.0 * a_0 + 4.0 * a_p1 - a_p2) +
					tlng * tlng * tlng * (-a_m1 + 3.0 * a_0 - 3.0 * a_p1 + a_p2));
				a_m1 = eptr[latstencil[3] + lngstencil[0]];
				a_0 = eptr[latstencil[3] + lngstencil[1]];
				a_p1 = eptr[latstencil[3] + lngstencil[2]];
				a_p2 = eptr[latstencil[3] + lngstencil[3]];
				b_p2 = 0.5 * (2.0 * a_0 + tlng * (-a_m1 + a_p1) +
					tlng * tlng * (2.0 * a_m1 - 5.0 * a_0 + 4.0 * a_p1 - a_p2) +
					tlng * tlng * tlng * (-a_m1 + 3.0 * a_0 - 3.0 * a_p1 + a_p2));
				e = 0.5 * (2.0 * b_0 + tlat * (-b_m1 + b_p1) +
					tlat * tlat * (2.0 * b_m1 - 5.0 * b_0 + 4.0 * b_p1 - b_p2) +
					tlat * tlat * tlat * (-b_m1 + 3.0 * b_0 - 3.0 * b_p1 + b_p2));
				e = max(-32767.0, min(32766.0, e));
			}
			plat0 = lat0;
			plng0 = lng0;
			elev_base[i * elev_stride + j] = (INT16)e;
			if (emean && i >= 0 && j >= 0 && i <= elev_grid && j <= elev_grid) {
				*emean += e;
				nmean++;
			}
		}
	}
	if (emean && nmean)
		*emean /= nmean;
	if (FilterElevation(ilat, ilng, lvl, elev_res, elev) && emean) {
		double sum = 0.0;
		for (i = 0; i <= elev_grid; i++)
			for (j = 0; j <= elev_grid; j++)
				sum += elev_base[i*elev_stride+j];
		*emean = sum / ((elev_grid + 1) * (elev_grid + 1));
	}
}

namespace {
struct TilePixelPosition {
	int iLat;
	int iLng;
	int X;
	int Y;
	bool operator==(const TilePixelPosition &other) const {
		return iLat == other.iLat && iLng == other.iLng && X == other.X && Y == other.Y;
	}
};

void GetTilePixelPosition (int lvl, double lat, double lng, TilePixelPosition &pos)
{
	if (lat < -90.0) lat = -90.0;
	if (lat > 90.0) lat = 90.0;
	while (lng < -180.0) lng += 360.0;
	while (lng > 180.0) lng -= 360.0;

	double bands = (double)(1 << lvl);
	double width = 180.0 / bands;
	pos.iLat = (int)((-lat + 90.0) / width);
	if (pos.iLat >= (1 << lvl)) pos.iLat = (1 << lvl) - 1;
	pos.iLng = (int)((lng + 180.0) / width);
	if (pos.iLng >= (2 << lvl)) pos.iLng = 0;
	double latmin = 90.0 - width * (pos.iLat + 1);
	double lngmin = -180.0 + width * pos.iLng;
	pos.X = (int)((lng - lngmin) / width * 256.0);
	pos.Y = (int)(-(lat - latmin) / width * 256.0 + 256.0);
}
}

void ElevationManager::LoadFlats ()
{
	char flatRel[MAX_PATH], flatDir[MAX_PATH];
	snprintf(flatRel, sizeof(flatRel), "%s\\Flat", cbody->Name());

	bool foundDir = false;
	if (g_pOrbiter->Cfg()->CfgDirPrm.HightexDir[0]) {
		snprintf(flatDir, sizeof(flatDir), "%s%s", g_pOrbiter->Cfg()->CfgDirPrm.HightexDir, flatRel);
		if (std::filesystem::is_directory(flatDir))
			foundDir = true;
	}
	if (!foundDir) {
		g_pOrbiter->Cfg()->PTexPath(flatDir, flatRel);
		if (flatDir[0] && std::filesystem::is_directory(flatDir))
			foundDir = true;
	}
	if (!foundDir) {
		g_pOrbiter->Cfg()->TexPath(flatDir, flatRel);
		if (flatDir[0] && std::filesystem::is_directory(flatDir))
			foundDir = true;
	}
	if (!foundDir)
		return;

	std::vector<std::filesystem::path> files;
	std::error_code ec;
	for (const auto &entry : std::filesystem::directory_iterator(flatDir, ec)) {
		if (!entry.is_regular_file(ec)) continue;
		std::string ext = entry.path().extension().string();
		if (_stricmp(ext.c_str(), ".flt") == 0)
			files.push_back(entry.path());
	}
	if (files.empty())
		return;

	std::sort(files.begin(), files.end());

	const double radius = cbody->Size();
	if (radius <= 0.0)
		return;

	char line[512], shapeType[256];
	for (const auto &file : files) {
		FILE *f = fopen(file.string().c_str(), "r");
		if (!f) continue;
		while (fgets(line, sizeof(line), f)) {
			char *p = line;
			while (*p == ' ' || *p == '\t') p++;
			if (*p == '\0' || *p == '\r' || *p == '\n' || (p[0] == '/' && p[1] == '/'))
				continue;
			if (char *comment = strstr(p, "//"))
				*comment = '\0';

			int height = 0, dim1 = 0, dim2 = 0, falloff = 0;
			double lat = 0.0, lng = 0.0, phi = 0.0;
			int read = sscanf(p, "%255s %d %lf %lf %d %d %lf %d",
				shapeType, &height, &lng, &lat, &dim1, &dim2, &phi, &falloff);
			if (read < 5) continue;
			if (read < 6) dim2 = dim1;
			if (read < 7) phi = 0.0;
			if (read < 8) falloff = 0;

			double cosLat = cos(lat * RAD);
			if (dim1 <= 0 || dim2 <= 0 || fabs(cosLat) < 1e-9)
				continue;

			double decdeg = 360.0 / (radius * cosLat * Pi2);
			if (_stricmp(shapeType, "ellipse") == 0) {
				flatShapes.push_back(FlatShape{
					lat, lng,
					(double)dim1 * decdeg, (double)dim2 * decdeg,
					cos(-phi * RAD), sin(-phi * RAD),
					(double)falloff / 100.0, height, 1
				});
			} else if (_stricmp(shapeType, "rect") == 0) {
				flatShapes.push_back(FlatShape{
					lat, lng,
					(double)dim1 * decdeg * 0.5, (double)dim2 * decdeg * 0.5,
					cos(-phi * RAD), sin(-phi * RAD),
					(double)falloff / 100.0, height, 4
				});
			}
		}
		fclose(f);
	}

	if (flatShapes.empty())
		return;

	flatTiles.resize(MAXLVL_LIMIT + 1);
	for (size_t idx = 0; idx < flatShapes.size(); idx++) {
		const FlatShape &shape = flatShapes[idx];
		double offset = sqrt(shape.Dim1 * shape.Dim1 + shape.Dim2 * shape.Dim2);
		double latmin = shape.Lat - offset;
		double latmax = shape.Lat + offset;
		double lngmin = shape.Lng - offset;
		double lngmax = shape.Lng + offset;
		TilePixelPosition leftUpper, rightUpper, leftLower, rightLower;
		for (int lvl = MAXLVL_LIMIT; lvl >= 0; lvl--) {
			int latBands = 1 << lvl;
			int lngBands = latBands * 2;

			GetTilePixelPosition(lvl, latmax, lngmin, leftUpper);
			GetTilePixelPosition(lvl, latmax, lngmax, rightUpper);
			GetTilePixelPosition(lvl, latmin, lngmin, leftLower);
			rightLower.iLat = leftLower.iLat;
			rightLower.iLng = rightUpper.iLng;
			rightLower.X = rightUpper.X;
			rightLower.Y = leftLower.Y;

			if (leftUpper == rightUpper || leftLower == rightLower || leftUpper == leftLower || rightUpper == rightLower)
				break;

			if (leftUpper.iLng > rightUpper.iLng) {
				leftUpper.iLng -= lngBands;
				leftLower.iLng -= lngBands;
			}
			if (leftUpper.X == 0) {
				leftUpper.iLng--;
				leftLower.iLng--;
			}
			if (rightUpper.X == 255) {
				rightUpper.iLng++;
				rightLower.iLng++;
			}
			if (leftUpper.Y == 0 && leftUpper.iLat > 0) {
				leftUpper.iLat--;
				rightUpper.iLat--;
			}
			if (leftLower.Y == 255 && leftLower.iLat < latBands - 1) {
				leftLower.iLat++;
				rightLower.iLat++;
			}

			int latStart = max(0, leftUpper.iLat);
			int latEnd = min(latBands - 1, leftLower.iLat);
			for (int ilat = latStart; ilat <= latEnd; ilat++) {
				for (int ilng = leftUpper.iLng; ilng <= rightUpper.iLng; ilng++) {
					int wrappedLng = ((ilng % lngBands) + lngBands) % lngBands;
					uint64_t key = ((uint64_t)(uint32_t)ilat << 32) | (uint32_t)wrappedLng;
					flatTiles[lvl][key].push_back(idx);
				}
			}
		}
	}
}

template<typename T>
bool ElevationManager::FilterElevationImpl (int ilat, int ilng, int lvl, double res, T *elev) const
{
	if (!bFlats || !elev || res <= 0.0 || flatShapes.empty() || lvl < 0 || lvl >= (int)flatTiles.size())
		return false;

	uint64_t key = ((uint64_t)(uint32_t)ilat << 32) | (uint32_t)ilng;
	const auto &lvlMap = flatTiles[lvl];
	auto it = lvlMap.find(key);
	if (it == lvlMap.end())
		return false;

	const auto &shapeIndices = it->second;
	double bands = (double)(1 << lvl);
	double width = 180.0 / bands;
	double step = width / 256.0;
	double latmin = 90.0 - width * (ilat + 1) - step * 1.5;
	double lngmin = -180.0 + width * ilng - step * 1.5;
	const int ndat = elev_stride * elev_stride;
	bool modified = false;

	for (int i = 0; i < ndat; i++) {
		double ptLat = latmin + (i / elev_stride) * step;
		double ptLng = lngmin + (i % elev_stride) * step;
		for (size_t idx : shapeIndices) {
			const FlatShape &shape = flatShapes[idx];
			double y = ptLat - shape.Lat;
			double x = ptLng - shape.Lng;
			if (x < -180.0) x += 360.0;
			else if (x > 180.0) x -= 360.0;
			double x1 = (x * shape.Cos - y * shape.Sin) / shape.Dim1;
			double y1 = (x * shape.Sin + y * shape.Cos) / shape.Dim2;
			for (int pot = 0; pot < shape.Type; pot++) {
				x1 *= x1;
				y1 *= y1;
			}
			double dist = x1 + y1;
			if (dist <= 1.0) {
				double target = (double)shape.Height / res;
				double elevation = target;
				if (shape.Falloff > 0.0) {
					for (int pot = 0; pot < shape.Type; pot++)
						dist = sqrt(dist);
					if (dist >= 1.0 - shape.Falloff) {
						double alpha = (dist - 1.0 + shape.Falloff) / shape.Falloff;
						elevation = (double)elev[i] * alpha + target * (1.0 - alpha);
					}
				}
				if constexpr (std::is_same_v<T, INT16>) {
					elevation = max(-32768.0, min(32767.0, elevation));
					elev[i] = (INT16)elevation;
				} else {
					elev[i] = (T)elevation;
				}
				modified = true;
			}
		}
	}
	return modified;
}

bool ElevationManager::FilterElevation (int ilat, int ilng, int lvl, double res, INT16 *elev) const
{
	return FilterElevationImpl<INT16>(ilat, ilng, lvl, res, elev);
}

bool ElevationManager::FilterElevation (int ilat, int ilng, int lvl, double res, float *elev) const
{
	return FilterElevationImpl<float>(ilat, ilng, lvl, res, elev);
}
