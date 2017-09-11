// ==============================================================
//   ORBITER VISUALISATION PROJECT (OVP)
//   Copyright (C) 2006-2016 Martin Schweiger
//   Dual licensed under GPL v3 and LGPL v3
// ==============================================================

// ==============================================================
// surfmgr2.cpp
// class SurfaceManager2 (implementation)
//
// Planetary surface rendering engine v2, including a simple
// LOD (level-of-detail) algorithm for surface patch resolution.
// ==============================================================

#include "Surfmgr2.h"
#include "Tilemgr2.h"
#include "Cloudmgr2.h"
#include "Texture.h"
#include "D3D9Catalog.h"
#include "D3D9Config.h"
#include "vVessel.h"
#include "VectorHelpers.h"
#include "DebugControls.h"

// =======================================================================

#pragma pack(push,1)

struct ELEVFILEHEADER { // file header for patch elevation data file
	char id[4];            // ID string + version ('E','L','E',1)
	int hdrsize;           // header size (76 expected)
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

// =======================================================================
// Utility functions

static void VtxInterpolate (VERTEX_2TEX &res, const VERTEX_2TEX &a, const VERTEX_2TEX &b, double w)
{
	float w1 = (float)w;
	float w0 = 1.0f-w1;
	res.x = a.x*w0 + b.x*w1;
	res.y = a.y*w0 + b.y*w1;
	res.z = a.z*w0 + b.z*w1;
	res.nx = a.nx*w0 + b.nx*w1;
	res.ny = a.ny*w0 + b.ny*w1;
	res.nz = a.nz*w0 + b.nz*w1;
}

// =======================================================================
// =======================================================================

SurfTile::SurfTile (TileManager2Base *_mgr, int _lvl, int _ilat, int _ilng)
: Tile (_mgr, _lvl, _ilat, _ilng)
{
	smgr = static_cast<TileManager2<SurfTile>* > (_mgr);
	node = 0;
	elev = NULL;
	ggelev = NULL;
	ltex = NULL;
	htex = NULL;
	has_elevfile = false;
	label = NULL;
	MaxRep = mgr->Client()->GetFramework()->GetCaps()->MaxTextureRepeat;
	if (Config->TileMipmaps == 2) bMipmaps = true;
	if (Config->TileMipmaps == 1 && _lvl < 10) bMipmaps = true;
}

// -----------------------------------------------------------------------

SurfTile::~SurfTile ()
{
	if (elev) {
		delete []elev;
		elev = NULL;
	}
	if (ltex && owntex) {
		if (TileCatalog->Remove(ltex)) ltex->Release();
	}
	if (htex) htex->Release();
	DeleteLabels();
}

// -----------------------------------------------------------------------

void SurfTile::PreLoad()
{
	char fname[128];
	char path[MAX_PATH];
	bool ok = false;

	// Load surface texture

	if (mgr->Cprm().tileLoadFlags & 0x0001) { // try loading from individual tile file
		sprintf_s (fname, ARRAYSIZE(fname), "%s\\Surf\\%02d\\%06d\\%06d.dds", mgr->CbodyName(), lvl+4, ilat, ilng);
		ok = mgr->GetClient()->TexturePath(fname, path)
		                && LoadTextureFile(path, &pPreSrf);
	}

	if (!ok && smgr->ZTreeManager(0)) { // try loading from compressed archive
		BYTE *buf;
		DWORD ndata = smgr->ZTreeManager(0)->ReadData(lvl+4, ilat, ilng, &buf);
		if (ndata) {
			ok = LoadTextureFromMemory(buf, ndata, &pPreSrf);
			smgr->ZTreeManager(0)->ReleaseData(buf);
		}
	}

	// Load mask texture

	if (ok && (mgr->Cprm().bSpecular || mgr->Cprm().bLights)) {
		ok = false; // <= in case tileLoadFlags is set to "Archive only", we have to reset this, else no (compressed) Mask would be loaded

		if (mgr->Cprm().tileLoadFlags & 0x0001) { // try loading from individual tile file
			sprintf_s(fname, ARRAYSIZE(fname), "%s\\Mask\\%02d\\%06d\\%06d.dds", mgr->CbodyName(), lvl+4, ilat, ilng);
			ok = mgr->GetClient()->TexturePath(fname, path)
			                && LoadTextureFile(path, &pPreMsk);
		}
		if (!ok && smgr->ZTreeManager(1)) { // try loading from compressed archive
			BYTE *buf;
			DWORD ndata = smgr->ZTreeManager(1)->ReadData(lvl+4, ilat, ilng, &buf);
			if (ndata) {
				LoadTextureFromMemory(buf, ndata, &pPreMsk);
				smgr->ZTreeManager(1)->ReleaseData(buf);
			}
		}
	}

	mgr->TileLabel(pPreSrf, lvl, ilat, ilng);
	mgr->TileLabel(pPreMsk, lvl, ilat, ilng);
}

// -----------------------------------------------------------------------

void SurfTile::Load ()
{

	LPDIRECT3DDEVICE9 pDev = mgr->Dev();

	owntex = true;

	if (CreateTexture(pDev, pPreSrf, &tex) != true) {
		if (GetParentSubTexRange (&texrange)) {
			tex = getSurfParent()->Tex();
			owntex = false;
		} 
	} else TileCatalog->Add(tex);

	// Load mask texture
	if (mgr->Cprm().bSpecular || mgr->Cprm().bLights) {
		if (owntex && tex) {
			CreateTexture(pDev, pPreMsk, &ltex);
			if (ltex) TileCatalog->Add(ltex);
		} else if (node && node->Parent()) {
			ltex = getSurfParent()->ltex;
		}
	}

	// Load elevation data
	INT16 *elev = ElevationData ();
	
	bool shift_origin = (lvl >= 4);
	int res = mgr->GridRes();

	if (lvl <= 0)
	{
		if (!lvl) { // create hemisphere mesh for western or eastern hemispheres
			mesh = CreateMesh_hemisphere(res, elev, 0.0);
	//  } else {    // create full sphere mesh
	//	  // TODO
		}
	} else {
		// create rectangular patch
		mesh = CreateMesh_quadpatch (res, res, elev, mgr->ElevRes(), 0.0, &texrange, shift_origin, &vtxshift, mgr->GetPlanet()->prm.tilebb_excess);
	}

	static const DWORD label_enable = PLN_ENABLE | PLN_LMARK;
	DWORD plnmode = *(DWORD*)smgr->Client()->GetConfigParam(CFGPRM_PLANETARIUMFLAG);
	if ((plnmode & label_enable) == label_enable) {
		CreateLabels();
	}
}

// -----------------------------------------------------------------------

INT16 *SurfTile::ReadElevationFile (const char *name, int lvl, int ilat, int ilng, double tgt_res, double *mean_elev)
{
	const int ndat = TILE_ELEVSTRIDE*TILE_ELEVSTRIDE;
	ELEVFILEHEADER hdr;
	INT16 *e = NULL;
	//INT16 ofs;
	double scale, offset;
	char path[MAX_PATH];
	char fname[128];
	FILE *f;
	int i;

	// Elevation data
	if (mgr->Cprm().tileLoadFlags & 0x0001) { // try loading from individual tile file
		sprintf_s (fname, ARRAYSIZE(fname), "%s\\Elev\\%02d\\%06d\\%06d.elv", name, lvl, ilat, ilng);
		bool found = mgr->GetClient()->TexturePath(fname, path);
		if (found && !fopen_s(&f, path, "rb")) {
			e = new INT16[ndat];
			// read the elevation file header
			fread (&hdr, sizeof(ELEVFILEHEADER), 1, f);
			if (hdr.hdrsize != sizeof(ELEVFILEHEADER)) {
				fseek (f, hdr.hdrsize, SEEK_SET);
			}
			scale = hdr.scale;
			offset = hdr.offset;
			if (mean_elev) *mean_elev = hdr.emean;

			switch (hdr.dtype) {
			case 0: // flat tile, defined by offset
				for (i = 0; i < ndat; i++) e[i] = 0;
				break;
			case 8: {
				UINT8 *tmp = new UINT8[ndat];
				fread (tmp, sizeof(UINT8), ndat, f);
				for (i = 0; i < ndat; i++)
					e[i] = (INT16)tmp[i]; 
				delete []tmp;
				}
				break;
			case -16:
				fread (e, sizeof(INT16), ndat, f);
				break;
			}
			fclose (f);
		}
	}
	if (!e && smgr->ZTreeManager(2)) { // try loading from compressed archive
		BYTE *buf;
		DWORD ndata = smgr->ZTreeManager(2)->ReadData(lvl, ilat, ilng, &buf);
		if (ndata) {
			BYTE *p = buf;
			e = new INT16[ndat];
			ELEVFILEHEADER *phdr = (ELEVFILEHEADER*)p;
			p += phdr->hdrsize;
			if (mean_elev) *mean_elev = phdr->emean;
			scale = phdr->scale;
			offset = phdr->offset;
			switch (phdr->dtype) {
			case 0:
				for (i = 0; i < ndat; i++) e[i] = 0;
				break;
			case 8:
				for (i = 0; i < ndat; i++)
					e[i] = (INT16)(*p++);
				break;
			case -16:
				memcpy(e, p, ndat*sizeof(INT16));
				p += ndat*sizeof(INT16);
				break;
			}
			smgr->ZTreeManager(2)->ReleaseData(buf);
		}
	}

	if (e) {
		if (scale != tgt_res) { // rescale the data
			double rescale = scale / tgt_res;
			for (i = 0; i < ndat; i++)
				e[i] = (INT16)(e[i] * rescale);
		}
		if (offset) {
			INT16 sofs = (INT16)(offset / tgt_res);
			for (i = 0; i < ndat; i++)
				e[i] += sofs;
		}
	}

	// Elevation mod data
	if (e) {
		bool ok = false;
		double rescale;
		INT16 offset;
		bool do_rescale, do_shift;
		if (mgr->Cprm().tileLoadFlags & 0x0001) { // try loading from individual tile file
			sprintf_s (fname, ARRAYSIZE(fname), "%s\\Elev_mod\\%02d\\%06d\\%06d.elv", name, lvl, ilat, ilng);
			bool found = mgr->GetClient()->TexturePath(fname, path);
			if (found && !fopen_s(&f, path, "rb")) {
				fread (&hdr, sizeof(ELEVFILEHEADER), 1, f);
				if (hdr.hdrsize != sizeof(ELEVFILEHEADER)) {
					fseek (f, hdr.hdrsize, SEEK_SET);
				}
				rescale = (do_rescale = (hdr.scale != tgt_res)) ? hdr.scale / tgt_res : 1.0;
				offset = (do_shift = (hdr.offset != 0.0)) ? (INT16)(hdr.offset / tgt_res) : 0;
				switch (hdr.dtype) {
				case 0: // overwrite the entire tile with a flat offset
					for (i = 0; i < ndat; i++) e[i] = offset;
					break;
				case 8: {
					const UINT8 mask = UCHAR_MAX;
					UINT8 *tmp = new UINT8[ndat];
					fread (tmp, sizeof(UINT8), ndat, f);
					for (i = 0; i < ndat; i++) {
						if (tmp[i] != mask) {
							e[i] = (INT16)(do_rescale ? (INT16)(tmp[i] * rescale) : (INT16)tmp[i]);
							if (do_shift) e[i] += offset;
						}
					}
					delete []tmp;
					}
					break;
				case -16: {
					const INT16 mask = SHRT_MAX;
					INT16 *tmp = new INT16[ndat];
					INT16 ofs = (INT16)hdr.offset;
					fread (tmp, sizeof(INT16), ndat, f);
					for (i = 0; i < ndat; i++) {
						if (tmp[i] != mask) {
							e[i] = (do_rescale ? (INT16)(tmp[i] * rescale) : tmp[i]);
							if (do_shift) e[i] += offset;
						}
					}
					delete []tmp;
					}
					break;
				}
				fclose(f);
				ok = true;
			}
		}
		if (!ok && smgr->ZTreeManager(3)) { // try loading from compressed archive
			BYTE *buf;
			DWORD ndata = smgr->ZTreeManager(3)->ReadData(lvl, ilat, ilng, &buf);
			if (ndata) {
				BYTE *p = buf;
				ELEVFILEHEADER *phdr = (ELEVFILEHEADER*)p;
				p += phdr->hdrsize;
				rescale = (do_rescale = (phdr->scale != tgt_res)) ? phdr->scale / tgt_res : 1.0;
				offset = (do_shift = (phdr->offset != 0.0)) ? (INT16)(phdr->offset / tgt_res) : 0;
				switch(phdr->dtype) {
				case 0:
					for (i = 0; i < ndat; i++) e[i] = offset;
					break;
				case 8: {
					const UINT8 mask = UCHAR_MAX;
					for (i = 0; i < ndat; i++) {
						if (p[i] != mask) {
							e[i] = (INT16)(do_rescale ? p[i] * rescale : p[i]);
							if (do_shift) e[i] += offset;
						}
					}
					} break;
				case -16: {
					const INT16 mask = SHRT_MAX;
					INT16 *buf16 = (INT16*)p;
					for (i = 0; i < ndat; i++) {
						if (buf16[i] != mask) {
							e[i] = (do_rescale ? (INT16)(buf16[i] * rescale) : buf16[i]);
							if (do_shift) e[i] += offset;
						}
					}
					} break;
				}
				smgr->ZTreeManager(3)->ReleaseData(buf);
			}
		}
	}
	return e;
}

// -----------------------------------------------------------------------

bool SurfTile::LoadElevationData ()
{
	// Note: a tile's elevation data are retrieved from its great-grandparent tile. Each tile stores the elevation
	// data for its area at 8x higher resolution than required by itself, so they can be used by the grandchildren.
	// This reduces the number of elevation files, by storing more information in each. It also has a caching effect:
	// Once a grandparent has loaded its elevation data on request of a grandchild, any other grandchildren's requests
	// can be served directly without further disk I/O.

	if (elev) return true; // already present

	has_elevfile = false;
	int mode = mgr->Cprm().elevMode;
	if (!mode) return false;

	int ndat = TILE_ELEVSTRIDE*TILE_ELEVSTRIDE;
	elev = ReadElevationFile (mgr->CbodyName(), lvl + 4, ilat, ilng, mgr->ElevRes());
	if (elev) {

		has_elevfile = true;

		/*
		HR(D3DXCreateTexture(mgr->Dev(), TILE_ELEVSTRIDE, TILE_ELEVSTRIDE, 1, D3DUSAGE_DYNAMIC, D3DFMT_L16, D3DPOOL_DEFAULT, &htex));

		if (htex) {
			D3DLOCKED_RECT Rect;
			if (htex->LockRect(0, &Rect, NULL, D3DLOCK_DISCARD)==S_OK) {
				BYTE  *pTgt = (BYTE *)Rect.pBits;
				INT16 *pSrc = elev;
				for (int k=0;k<TILE_ELEVSTRIDE;k++) {
					memcpy(pTgt, pSrc, TILE_ELEVSTRIDE*sizeof(INT16));
					pTgt += Rect.Pitch;
					pSrc += TILE_ELEVSTRIDE;
				}
				htex->UnlockRect(0);
				LogBlu("Height map created 0x%X", htex);	
			}
			else LogErr("Failed to lock a height map");
		}*/

	} 
	else if (lvl > 0) {

		// construct elevation grid by interpolating ancestor data
		ELEVHANDLE hElev = mgr->ElevMgr();
		if (!hElev) return false;
		int plvl = lvl-1;
		int pilat = ilat >> 1;
		int pilng = ilng >> 1;
		INT16 *pelev = 0;
		QuadTreeNode<SurfTile> *parent = node->Parent();
		for (; plvl >= 0; plvl--) { // find ancestor with elevation data
			if (parent && parent->Entry()->has_elevfile) {
				pelev = parent->Entry()->elev;
				break;
			}
			parent = parent->Parent();
			pilat >>= 1;
			pilng >>= 1;
		}
		if (!pelev) return false;
		elev = new INT16[ndat];
		// submit ancestor data to elevation manager for interpolation
		mgr->GetClient()->ElevationGrid (hElev, ilat, ilng, lvl, pilat, pilng, plvl, pelev, elev);

	}
	return (elev != 0);
}

// -----------------------------------------------------------------------

INT16 *SurfTile::ElevationData () const
{
	if (!ggelev) {
		int ancestor_dlvl = 3;
		while (1 << (8-ancestor_dlvl) < mgr->GridRes()) ancestor_dlvl--;
		if (lvl >= ancestor_dlvl) { // traverse quadtree back to great-grandparent
			QuadTreeNode<SurfTile> *ancestor = node; // start at my own node
			int blockRes = TILE_FILERES;
			while (blockRes > mgr->GridRes() && ancestor) {
				ancestor = ancestor->Parent();
				blockRes >>= 1;
			}
			if (ancestor && ancestor->Entry()->LoadElevationData()) {
				// compute pixel offset into great-grandparent tile set
				int nblock = TILE_FILERES/blockRes;
				int mask = nblock-1;
				int ofs = ((mask - ilat & mask) * TILE_ELEVSTRIDE + (ilng & mask)) * blockRes;
				ggelev = ancestor->Entry()->elev + ofs;
			}
		} else {
			SurfTile *ggp = smgr->GlobalTile(lvl - ancestor_dlvl);// +3);
			if (ggp && ggp->LoadElevationData ()) {
				int blockRes = mgr->GridRes();
				int nblock = TILE_FILERES/blockRes;
				int mask = nblock-1;
				int ofs = ((mask - ilat & mask) * TILE_ELEVSTRIDE + (ilng & mask)) * blockRes;
				ggelev = ggp->elev + ofs;
			}
		}
		if (ggelev)
			mean_elev = GetMeanElevation (ggelev);
	}
	return ggelev;
}

// -----------------------------------------------------------------------

double SurfTile::GetMeanElevation(const INT16 *elev) const
{
	int i, j;
	int res = mgr->GridRes();
	double melev = 0.0;
	for (j = 0; j <= res; j++) {
		for (i = 0; i <= res; i++) {
			melev += elev[i];
		}
		elev += TILE_ELEVSTRIDE;
	}
	return melev / ((res + 1)*(res + 1));
}

// -----------------------------------------------------------------------

int SurfTile::GetElevation(double lng, double lat, double *elev, SurfTile **cache, bool bFilter)
{
	static int ndat = TILE_ELEVSTRIDE*TILE_ELEVSTRIDE;
	if (cache) *cache = this;

	if (lat<minlat || lat>maxlat) return -1;
	if (lng<minlng || lng>maxlng) return -1;
	
	if (state==Invisible) return 0;
	
	if (state==ForRender) {
		if (!ggelev) { *elev = 0.0; return 1; }
		else {
			double fRes = double(mgr->GridRes());

			if (!bFilter) {
				int i = int( (lat-minlat) * fRes / (maxlat-minlat) ) + 1;
				int j = int( (lng-minlng) * fRes / (maxlng-minlng) ) + 1;
				*elev = double(ggelev[j+i*TILE_ELEVSTRIDE]);
			}
			else {

				float x = float( (lat-minlat) * fRes / (maxlat-minlat) ) + 1.0f; // 0.5f
				float y = float( (lng-minlng) * fRes / (maxlng-minlng) ) + 1.0f; // 0.5f
				float fx = (x - floor(x));
				float fy = (y - floor(y));

				int i0 = int(x) * TILE_ELEVSTRIDE; 
				int i1 = i0 + TILE_ELEVSTRIDE; 
				int j0 = int(y);
				
				assert(i0 > 0);
				assert(j0 > 0);
				assert((j0 + i1) < ndat);

				float q = lerp(float(ggelev[j0+i0]), float(ggelev[j0+i1]), fx); j0++;
				float w = lerp(float(ggelev[j0+i0]), float(ggelev[j0+i1]), fx);

				*elev = double(lerp(q,w,fy));
			}

			return 1;
		}
	}

	if (state==Active) {
		int i = 0;
		if (lng>(minlng+maxlng)*0.5) i++;
		if (lat<(minlat+maxlat)*0.5) i+=2;
		return  node->Child(i)->Entry()->GetElevation(lng, lat, elev, cache);		
	}

	return -3;
}

// -----------------------------------------------------------------------

SurfTile *SurfTile::getTextureOwner()
{
	if (owntex) return this;
	SurfTile *parent = getSurfParent();
	while (parent) {
		if (parent->owntex) return parent;
		else parent = parent->getSurfParent();
	}
	return NULL;
}

// -----------------------------------------------------------------------

float SurfTile::fixinput(double a, int x)
{
	switch(x) {
		case 0: return float((1.0+floor(a*0.0625))*16.0);
		case 1: return float((1.0+floor(a*0.125))*8.0);
		case 2: return float((1.0+floor(a*0.5))*2.0);
	}
	return 0.0f;
}

// -----------------------------------------------------------------------

double SurfTile::GetCameraDistance()
{	
	VECTOR3 cnt = Centre() * (mgr->CbodySize() + mean_elev);
	cnt = mgr->prm.cpos + mul(mgr->prm.grot, cnt);
	return length(cnt);
}

// -----------------------------------------------------------------------

D3DXVECTOR4 SurfTile::MicroTexRange(SurfTile *pT, int ml) const
{
	float rs = 1.0f / float( 1 << (lvl-pT->Level()) );	// Range subdivision
	float xo = pT->MicroRep[ml].x * texrange.tumin;		
	float yo = pT->MicroRep[ml].y * texrange.tvmin;
	xo -= floor(xo); // Micro texture offset for current tile
	yo -= floor(yo); // Micro texture offset for current tile
	return D3DXVECTOR4(xo, yo, pT->MicroRep[ml].x * rs, pT->MicroRep[ml].y * rs); 
}

// -----------------------------------------------------------------------
// Called during rendering even if this tile is not rendered but children are
// i.e. called for every RENDERED and ACTIVE tile
//
void SurfTile::StepIn ()
{
	LPDIRECT3DDEVICE9 pDev = mgr->Dev();
	ID3DXEffect *Shader = mgr->Shader();
	const vPlanet *vPlanet = mgr->GetPlanet();

	if (vPlanet != mgr->GetScene()->GetCameraProxyVisual()) return;

	// Compute micro texture repeat counts -------------------------------------
	//
	if (owntex && vPlanet->MicroCfg.bEnabled) {
		double s = vPlanet->GetSize();
		for (int i = 0; i < ARRAYSIZE(vPlanet->MicroCfg.Level); ++i) {
			double f = s / vPlanet->MicroCfg.Level[i].size;
			MicroRep[i] = D3DXVECTOR2(fixinput(width*f, i), fixinput(height*f, i));
		}
	}
}


// -----------------------------------------------------------------------

void SurfTile::Render ()
{
	bool render_lights = mgr->Cprm().bLights;
	bool render_shadows = mgr->GetPlanet()->CloudMgr2()!=NULL; // && !mgr->prm.rprm->bCloudFlatShadows);

	if (!mesh) return; // DEBUG : TEMPORARY

	UINT numPasses = 0;

	LPDIRECT3DDEVICE9 pDev = mgr->Dev();
	ID3DXEffect *Shader = mgr->Shader();
	const vPlanet *vPlanet = mgr->GetPlanet();

	static const double rad0 = sqrt(2.0)*PI05;
	double sdist, rad;
	bool has_specular = false;
	bool has_shadows = false;
	bool has_lights = false;
	bool has_microtex = false;
	bool has_atmosphere = vPlanet->HasAtmosphere();
	bool has_ripples = vPlanet->HasRipples();


	if (ltex || render_shadows) {
		sdist = acos (dotp (mgr->prm.sdir, cnt));
		rad = rad0/(double)(2<<lvl); // tile radius
		has_specular = (ltex && sdist < (1.75 + rad));
		has_shadows = (render_shadows && sdist < (PI05+rad));
		has_lights = (render_lights && ltex && sdist > 1.45);
	}

	if (vPlanet->CameraAltitude()>20e3) has_ripples = false;

	HR(Shader->SetVector(TileManager2Base::svCloudOff, &D3DXVECTOR4(0, 0, 1, 1)));


	// Assign micro texture range information to shaders -------------------------
	//
	SurfTile *pT = getTextureOwner();

	if (pT && vPlanet->MicroCfg.bEnabled) {
		HR(Shader->SetVector(TileManager2Base::svMicroScale0, &MicroTexRange(pT, 0)));
		HR(Shader->SetVector(TileManager2Base::svMicroScale1, &MicroTexRange(pT, 1)));
		HR(Shader->SetVector(TileManager2Base::svMicroScale2, &MicroTexRange(pT, 2)));
		has_microtex = true;
	}
	

	
	// Setup cloud shadows -------------------------------------------------------
	//
	if (has_shadows) {

		has_shadows = false;

		const TileManager2<CloudTile> *cmgr = vPlanet->CloudMgr2();
		int maxlvl = min(lvl,9);
		double rot = mgr->prm.rprm->cloudrot;
		double edglat = (minlat+maxlat)*0.5;	// latitude of tile center
		double edglng = wrap(rot+minlng);		// surface tile minlng-edge position on cloud-layer

		for (int attempt=0;attempt<2;attempt++) {

			CloudTile *ctile = (CloudTile *)cmgr->SearchTile(edglng, edglat, maxlvl, true);

			if (ctile) {

				double icsize = double( 1 << ctile->Level() ) / PI;	// inverse of cloud tile size in radians
				
				// Compute surface tile uv origin on a selected claud tile
				// Note: edglng exists always within tile i.e. no wrap from PI to -PI
				double u0 = (edglng - ctile->minlng) * icsize;
				double v0 = (ctile->maxlat - maxlat) * icsize;		// Note: Tile corner is lower-left, texture corner is upper-left
				double u1 = u0 + (maxlng-minlng) * icsize;
				double v1 = v0 + (maxlat-minlat) * icsize;

				// Feed uv-offset and uv-range to the shaders
				HR(Shader->SetVector(TileManager2Base::svCloudOff, &D3DXVECTOR4(float(u0), float(v0), float(u1-u0), float(v1-v0))));
				HR(Shader->SetFloat(TileManager2Base::sfAlpha, 1.0f));
				HR(Shader->SetTexture(TileManager2Base::stCloud, ctile->Tex()));

				// Texture uv range extends to another tile
				if (u1 > 1.0) {

					double csize = PI / double(1<<ctile->Level());			// cloud tile size in radians
					double ctr = (ctile->minlng + ctile->maxlng) * 0.5;		// cloud tile center
					double lng  = wrap(ctr + csize*sign(rot));				// center of the next tile

					// Request an other tile from the same level as the first one
					CloudTile *ctile2 = (CloudTile *)cmgr->SearchTile(lng, edglat, ctile->Level(), true);

					if (ctile2) {
						if (ctile2->Level() == ctile->Level()) {
							// Rendering with dual texture
							HR(Shader->SetTexture(TileManager2Base::stCloud2, ctile2->Tex()));
							has_shadows = true;
							break;
						}
						else {
							// Failed to match a dual texture render requirements (due to no-valid tile available)
							// Try again and request a lower level texture data for the first texture
							maxlvl = ctile2->Level();
							if (attempt==1) LogErr("CloudShadows mapping failed");
						}
					}
				}
				else {
					// Just one texture in use
					has_shadows = true;
					break;
				}
			}
		}
	}
	

	// ---------------------------------------------------------------------
	// Feed tile specific data to shaders
	//
	// ---------------------------------------------------------------------------------------------------
	HR(Shader->SetTexture(TileManager2Base::stDiff, tex));
	HR(Shader->SetTexture(TileManager2Base::stMask, ltex));
	HR(Shader->SetVector(TileManager2Base::svTexOff, &GetTexRangeDX()));
	// ---------------------------------------------------------------------------------------------------
	//HR(Shader->SetInt(TileManager2Base::siTileLvl, int(bIntersect)));
	// ---------------------------------------------------------------------------------------------------
	//HR(Shader->SetBool(TileManager2Base::sbSpecular, has_specular));
	HR(Shader->SetBool(TileManager2Base::sbCloudSh, has_shadows));
	HR(Shader->SetBool(TileManager2Base::sbLights, has_lights));
	// ---------------------------------------------------------------------------------------------------
	if (has_lights) { HR(Shader->SetFloat(TileManager2Base::sfNight, float(mgr->Cprm().lightfac))); }
	else {			  HR(Shader->SetFloat(TileManager2Base::sfNight, 0.0f));	}

	D3DXVECTOR4 vBackup;

	/*if (mgr->GetPickedTile()==this) {
		Shader->GetVector(TileManager2Base::svWhiteBalance, &vBackup);
		Shader->SetVector(TileManager2Base::svWhiteBalance, &D3DXVECTOR4(1.5f, 0.5f, 0.5f, 1.0f));
	}*/

	Shader->CommitChanges();

	// -------------------------------------------------------------------
	// Find suitable technique
	
	int iTech = 0;
	
	if (has_atmosphere) {
		if (has_shadows) {
			if (has_ripples) iTech = 0;		// Earth
			else			 iTech = 1;		// Earth, no ripples
		}
		else {
			if (has_microtex) iTech = 2;	// Mars
			else			  iTech = 3;	// Mars, no micro
		}
	}
	else {
		if (has_microtex) iTech = 4;	// Luna
		else			  iTech = 5;	// Luna, no micro
	}


	// -------------------------------------------------------------------
	// render surface mesh
	//
	HR(Shader->Begin(&numPasses, D3DXFX_DONOTSAVESTATE));
	HR(Shader->BeginPass(iTech));
	pDev->SetVertexDeclaration(pPatchVertexDecl);
	pDev->SetStreamSource(0, mesh->pVB, 0, sizeof(VERTEX_2TEX));
	pDev->SetIndices(mesh->pIB);
	pDev->DrawIndexedPrimitive(D3DPT_TRIANGLELIST, 0, 0, mesh->nv, 0, mesh->nf);
	HR(Shader->EndPass());
	HR(Shader->End());	

	/*
	if (mgr->GetPickedTile() == this) {
		Shader->SetVector(TileManager2Base::svWhiteBalance, &vBackup);
	}*/

	// Render tile bounding box
	//
	if (DebugControls::IsActive()) {
		DWORD flags  = *(DWORD*)mgr->GetClient()->GetConfigParam(CFGPRM_GETDEBUGFLAGS);
		if (flags&DBG_FLAGS_TILEBOXES) {
			D3DXMATRIX wm;
			Shader->GetMatrix(TileManager2Base::smWorld, &wm);
			D3D9Effect::RenderTileBoundingBox(&wm, mesh->Box, &D3DXVECTOR4(1,0,0,1));
		}
	}
}

// -----------------------------------------------------------------------

void SurfTile::MatchEdges ()
{
	if (edgeok) return; // done already
	edgeok = true;
	if (!mesh) return;  // sanity check

	QuadTreeNode<SurfTile> *lngnbr = smgr->FindNode (lvl, ilat, ilng + (ilng & 1 ? 1 : -1));
	QuadTreeNode<SurfTile> *latnbr = smgr->FindNode (lvl, ilat + (ilat & 1 ? 1 : -1), ilng);
	QuadTreeNode<SurfTile> *dianbr = smgr->FindNode (lvl, ilat + (ilat & 1 ? 1 : -1), ilng + (ilng & 1 ? 1 : -1));

	if (lngnbr && !(lngnbr->Entry()->state & TILE_VALID)) lngnbr = 0;
	if (latnbr && !(latnbr->Entry()->state & TILE_VALID)) latnbr = 0;
	if (dianbr && !(dianbr->Entry()->state & TILE_VALID)) dianbr = 0;

	int new_lngnbr_lvl = (lngnbr ? lngnbr->Entry()->lvl : lvl);
	int new_latnbr_lvl = (latnbr ? latnbr->Entry()->lvl : lvl);
	int new_dianbr_lvl = (dianbr ? dianbr->Entry()->lvl : lvl);

	// fix lower-level neighbours first
	bool nbr_updated = false;
	if (new_lngnbr_lvl < lvl) {
		lngnbr->Entry()->MatchEdges ();
		nbr_updated = true;
	}
	if (new_latnbr_lvl < lvl) {
		latnbr->Entry()->MatchEdges ();
		nbr_updated = true;
	}
	if (!nbr_updated && new_dianbr_lvl < lvl) {
		// now we need to check the diagonal neighbour, since this could be lower resolution
		// and thus responsible for the corner node elevation
		dianbr->Entry()->MatchEdges ();
	}

	bool lngedge_changed = (new_lngnbr_lvl != lngnbr_lvl);
	bool latedge_changed = (new_latnbr_lvl != latnbr_lvl);
	bool diaedge_changed = (new_dianbr_lvl != dianbr_lvl);

	if (lngedge_changed || latedge_changed || diaedge_changed) {
		if (new_dianbr_lvl < new_lngnbr_lvl && new_dianbr_lvl < new_latnbr_lvl) {
			FixCorner (dianbr ? dianbr->Entry() : 0);
			FixLatitudeBoundary (latnbr ? latnbr->Entry() : 0, true);
			FixLongitudeBoundary (lngnbr ? lngnbr->Entry() : 0, true);
		} else if (new_latnbr_lvl < new_lngnbr_lvl) {
			FixLatitudeBoundary (latnbr ? latnbr->Entry() : 0);
			FixLongitudeBoundary (lngnbr ? lngnbr->Entry() : 0, true);
		} else {
			FixLongitudeBoundary (lngnbr ? lngnbr->Entry() : 0);
			FixLatitudeBoundary (latnbr ? latnbr->Entry() : 0, true);
		}
		mesh->MapVertices(mgr->Dev()); // copy the updated vertices to the vertex buffer
		lngnbr_lvl = new_lngnbr_lvl;
		latnbr_lvl = new_latnbr_lvl;
		dianbr_lvl = new_dianbr_lvl;
	}
}

// -----------------------------------------------------------------------

void SurfTile::FixCorner (const SurfTile *nbr)
{
	if (!mesh) return; // sanity check

	int res = mgr->GridRes();
	int vtx_idx = (ilat & 1 ? 0 : (res+1)*res) + (ilng & 1 ? res : 0);
	VERTEX_2TEX &vtx_store = mesh->vtx[mesh->nv + (ilat & 1 ? 0 : res)];

	INT16 *elev = ElevationData();
	if (!elev) return;

	if (nbr) {
		INT16 *nbr_elev = nbr->ElevationData();
		if (!nbr_elev) return;

		INT16 corner_elev = elev[TILE_ELEVSTRIDE+1 + (ilat & 1 ? 0 : TILE_ELEVSTRIDE*res) + (ilng & 1 ? res : 0)];
		INT16 nbr_corner_elev = nbr_elev[TILE_ELEVSTRIDE+1 + (ilat & 1 ? TILE_ELEVSTRIDE*res : 0) + (ilng & 1 ? 0 : res)];
	
		double rad = mgr->CbodySize();
		double radfac = (rad+nbr_corner_elev)/(rad+corner_elev);
		mesh->vtx[vtx_idx].x = (float)(vtx_store.x*radfac + vtxshift.x*(radfac-1.0));
		mesh->vtx[vtx_idx].y = (float)(vtx_store.y*radfac + vtxshift.y*(radfac-1.0));
		mesh->vtx[vtx_idx].z = (float)(vtx_store.z*radfac + vtxshift.z*(radfac-1.0));
	}
}

// -----------------------------------------------------------------------

void SurfTile::FixLongitudeBoundary (const SurfTile *nbr, bool keep_corner)
{
	// Fix the left or right edge
	int i, i0, i1, j, vtx_ofs, nbrlvl, dlvl;
	int res = mgr->GridRes();
	VERTEX_2TEX *vtx_store;

	vtx_ofs = (ilng & 1 ? res : 0); // check if neighbour is at left or right edge
	if (nbr && nbr->mesh && nbr->mesh->vtx) {
		nbrlvl = min(nbr->lvl, lvl); // we don't need to worry about neigbour levels higher than ours
		vtx_store = mesh->vtx + mesh->nv;
		if (nbrlvl == lvl) { // put my own edge back
			i0 = 0;
			i1 = res;
			if (keep_corner)
				if (ilat & 1) i0++; else i1--;
			for (i = i0; i <= i1; i++)
				mesh->vtx[vtx_ofs+i*(res+1)] = vtx_store[i];
		} else {  // interpolate to neighbour's left edge
			dlvl = lvl-nbrlvl;
			if (dlvl <= 5) { // for larger tile level differences the interleaved sampling method doesn't work
				INT16 *elev = ElevationData();
				INT16 *nbr_elev = nbr->ElevationData();
				if (elev && nbr_elev) {
					elev += TILE_ELEVSTRIDE+1 + vtx_ofs; // add offset
					int nsub = 1 << dlvl;    // number of tiles fitting alongside the lowres neighbour
					int vtx_skip = nsub*(res+1);  // interleave factor for nodes attaching to neigbour nodes
					int nbr_range = res/nsub; // number of neighbour vertices attaching to us - 1
					int subidxmask = (1<<dlvl)-1;
					int subidx = ilat & subidxmask;
					int nbr_ofs = (subidxmask-subidx) * nbr_range * TILE_ELEVSTRIDE;
					nbr_elev += TILE_ELEVSTRIDE+1 + nbr_ofs + (res-vtx_ofs);
					double rad = mgr->CbodySize();
					// match nodes to neighbour elevations
					i0 = 0;
					i1 = nbr_range;
					if (keep_corner)
						if (ilat & 1) i0++; else i1--;
					for (i = i0; i <= i1; i++) {
						double radfac = (rad+nbr_elev[i*TILE_ELEVSTRIDE])/(rad+elev[i*TILE_ELEVSTRIDE*nsub]);
						mesh->vtx[vtx_ofs+i*vtx_skip].x = (float)(vtx_store[i*nsub].x*radfac + vtxshift.x*(radfac-1.0));
						mesh->vtx[vtx_ofs+i*vtx_skip].y = (float)(vtx_store[i*nsub].y*radfac + vtxshift.y*(radfac-1.0));
						mesh->vtx[vtx_ofs+i*vtx_skip].z = (float)(vtx_store[i*nsub].z*radfac + vtxshift.z*(radfac-1.0));
					}
					// interpolate the nodes that fall between neighbour nodes
					for (i = 0; i < nbr_range; i++)
						for (j = 1; j < nsub; j++)
							VtxInterpolate (mesh->vtx[vtx_ofs+j*(res+1)+i*vtx_skip], mesh->vtx[vtx_ofs+i*vtx_skip], mesh->vtx[vtx_ofs+(i+1)*vtx_skip], (double)j/double(nsub));
				}
			} else {
				// problems
			}
		}
	}
}

// -----------------------------------------------------------------------

void SurfTile::FixLatitudeBoundary (const SurfTile *nbr, bool keep_corner)
{
	// Fix the top or bottom edge
	int i, i0, i1, j, nbrlvl, dlvl;
	int res = mgr->GridRes();
	VERTEX_2TEX *vtx_store;

	int line = (ilat & 1 ? 0 : res);
	int vtx_ofs = (ilat & 1 ? 0 : line*(res+1));
	if (nbr && nbr->mesh && nbr->mesh->vtx) {
		nbrlvl = min(nbr->lvl, lvl); // we don't need to worry about neigbour levels higher than ours
		vtx_store = mesh->vtx + mesh->nv + res + 1;
		if (nbrlvl == lvl) { // put my own edge back
			i0 = 0;
			i1 = res;
			if (keep_corner)
				if (ilng & 1) i1--; else i0++;
			for (i = i0; i <= i1; i++)
				mesh->vtx[vtx_ofs+i] = vtx_store[i];
		} else {
			dlvl = lvl-nbrlvl;
			if (dlvl <= 5) { // for larger tile level differences the interleaved sampling method doesn't work
				INT16 *elev = ElevationData();
				INT16 *nbr_elev = nbr->ElevationData();
				if (elev && nbr_elev) {
					elev += (line+1)*TILE_ELEVSTRIDE + 1; // add offset
					int nsub = 1 << dlvl;    // number of tiles fitting alongside the lowres neighbour
					int vtx_skip = nsub;     // interleave factor for nodes attaching to neigbour nodes
					int nbr_range = res/nsub; // number of neighbour vertices attaching to us - 1
					int subidxmask = (1<<dlvl)-1;
					int subidx = ilng & subidxmask;
					int nbr_ofs = subidx * nbr_range;
					nbr_elev += TILE_ELEVSTRIDE+1 + nbr_ofs + (res-line)*TILE_ELEVSTRIDE;
					double rad = mgr->CbodySize();
					// match nodes to neighbour elevations
					i0 = 0;
					i1 = nbr_range;
					if (keep_corner)
						if (ilng & 1) i1--; else i0++;
					for (i = i0; i <= i1; i++) {
						double radfac = (rad+nbr_elev[i])/(rad+elev[i*nsub]);
						mesh->vtx[vtx_ofs+i*vtx_skip].x = (float)(vtx_store[i*nsub].x*radfac + vtxshift.x*(radfac-1.0));
						mesh->vtx[vtx_ofs+i*vtx_skip].y = (float)(vtx_store[i*nsub].y*radfac + vtxshift.y*(radfac-1.0));
						mesh->vtx[vtx_ofs+i*vtx_skip].z = (float)(vtx_store[i*nsub].z*radfac + vtxshift.z*(radfac-1.0));
					}
					// interpolate the nodes that fall between neighbour nodes
					for (i = 0; i < nbr_range; i++)
						for (j = 1; j < nsub; j++)
							VtxInterpolate (mesh->vtx[vtx_ofs+i*vtx_skip+j], mesh->vtx[vtx_ofs+i*vtx_skip], mesh->vtx[vtx_ofs+(i+1)*vtx_skip], (double)j/double(nsub));
				}
			} else {
				// problems
			}
		}
	}
}

// -----------------------------------------------------------------------

void SurfTile::CreateLabels()
{
	if (!label) {
		label = TileLabel::Create(this);
	}
}

// -----------------------------------------------------------------------

inline void SurfTile::DeleteLabels()
{
	SAFE_DELETE(label);
}

// -----------------------------------------------------------------------

void SurfTile::RenderLabels(oapi::Sketchpad2 *skp, oapi::Font **labelfont, int *fontidx)
{
	if (!label) return;
	label->Render(skp, labelfont, fontidx);
}


// =======================================================================
// =======================================================================

template<>
void TileManager2<SurfTile>::Render (MATRIX4 &dwmat, bool use_zbuf, const vPlanet::RenderPrm &rprm)
{
	// set generic parameters
	SetRenderPrm (dwmat, 0, use_zbuf, rprm);

	double np = 0.0, fp = 0.0;
	int i;
	class Scene *scene = GetClient()->GetScene();

	// adjust scaling parameters (can only be done if no z-buffering is in use)
	if (!use_zbuf) {
		double R = obj_size;
		double D = prm.cdist*R;
		double zmax = (D - R*R/D) * 1.5;
		double zmin = max (2.0, min (zmax*1e-4, (D-R) * 0.8));
		double zscale = 1.0;

		np = scene->GetCameraNearPlane();
		fp = scene->GetCameraFarPlane();
		scene->SetCameraFrustumLimits (zmin, zmax);
	}

	// build a transformation matrix for frustum testing
	MATRIX4 Mproj = _MATRIX4(scene->GetProjectionMatrix());
	Mproj.m33 = 1.0; Mproj.m43 = -1.0;  // adjust near plane to 1, far plane to infinity
	MATRIX4 Mview = _MATRIX4(scene->GetViewMatrix());
	prm.dviewproj = mul(Mview,Mproj);

	// ---------------------------------------------------------------------

	static float spec_base = 0.7f; // 0.95f;

	// ---------------------------------------------------------------------
	// Initialize shading technique and feed planet specific data to shaders
	//
	Shader()->SetTechnique(eTileTech);

	if (use_zbuf) {
		pDev->SetRenderState(D3DRS_ZENABLE, 1);
		pDev->SetRenderState(D3DRS_ZWRITEENABLE, 1);
	}
	else {
		pDev->SetRenderState(D3DRS_ZENABLE, 0);
		pDev->SetRenderState(D3DRS_ZWRITEENABLE, 0);
	}

	HR(Shader()->SetMatrix(smViewProj, scene->GetProjectionViewMatrix()));
	HR(Shader()->SetVector(svWater, &D3DXVECTOR4(spec_base*1.2f, spec_base*1.0f, spec_base*0.8f, 50.0f)));
	HR(Shader()->SetBool(sbEnvEnable, false));

	// -------------------------------------------------------------------
	vVessel *vFocus = scene->GetFocusVisual();
	vPlanet *vProxy = scene->GetCameraProxyVisual();

	// Setup Environment map ---------------------------------------------
	//
	if (vFocus && bEnvMapEnabled) {
		LPDIRECT3DCUBETEXTURE9 pEnv = vFocus->GetEnvMap(0);
		if (pEnv) {
			HR(Shader()->SetTexture(stEnvMap, pEnv));
			HR(Shader()->SetBool(sbEnvEnable, true));
		}
	}

	// Setup micro textures ---------------------------------------------
	//
	if (vp->MicroCfg.bEnabled) {	
		HR(Shader()->SetTexture(stMicroA, vp->MicroCfg.Level[0].pTex));
		HR(Shader()->SetTexture(stMicroB, vp->MicroCfg.Level[1].pTex));
		HR(Shader()->SetTexture(stMicroC, vp->MicroCfg.Level[2].pTex));
		HR(Shader()->SetBool(sbMicroNormals, vp->MicroCfg.bNormals));
	}


	// ------------------------------------------------------------------
	// TODO: render full sphere for levels < 4

	loader->WaitForMutex();

	// update the tree
	for (i = 0; i < 2; i++)
		ProcessNode (tiletree+i);

	vp->tile_cache = NULL;

	// render the tree
	for (i = 0; i < 2; i++)
		RenderNode (tiletree+i);

	loader->ReleaseMutex();

	if (np)
		scene->SetCameraFrustumLimits(np,fp);
}

// -----------------------------------------------------------------------

template<>
void TileManager2<SurfTile>::RenderLabels(oapi::Sketchpad2 *skp, oapi::Font **labelfont, int *fontidx)
{
	for (int i = 0; i < 2; ++i) {
		RenderNodeLabels(tiletree + i, skp, labelfont, fontidx);
	}
}

// -----------------------------------------------------------------------

template<>
void TileManager2<SurfTile>::CreateLabels()
{
	loader->WaitForMutex();
	for (int i = 0; i < 2; ++i) {
		SetSubtreeLabels(tiletree + i, true);
	}
	loader->ReleaseMutex();
}

// -----------------------------------------------------------------------

template<>
void TileManager2<SurfTile>::DeleteLabels()
{
	loader->WaitForMutex();
	for (int i = 0; i < 2; ++i) {
		SetSubtreeLabels(tiletree + i, false);
	}
	loader->ReleaseMutex();
}

// -----------------------------------------------------------------------

template<>
void TileManager2<SurfTile>::SetSubtreeLabels(QuadTreeNode<SurfTile> *node, bool activate)
{
	if (node->Entry()) {
		if (activate) node->Entry()->CreateLabels();
		else          node->Entry()->DeleteLabels();
	}
	for (int i = 0; i < 4; ++i) {
		if (node->Child(i)) {
			SetSubtreeLabels(node->Child(i), activate);
		}
	}
}

// -----------------------------------------------------------------------

template<>
void TileManager2<SurfTile>::LoadZTrees()
{
	treeMgr = new ZTreeMgr*[ntreeMgr = 5]();
	if (cprm.tileLoadFlags & 0x0002) {
		char path[MAX_PATH];
		if (GetClient()->TexturePath(CbodyName(), path)) {
			treeMgr[0] = ZTreeMgr::CreateFromFile(path, ZTreeMgr::LAYER_SURF);
			treeMgr[1] = ZTreeMgr::CreateFromFile(path, ZTreeMgr::LAYER_MASK);
			treeMgr[2] = ZTreeMgr::CreateFromFile(path, ZTreeMgr::LAYER_ELEV);
			treeMgr[3] = ZTreeMgr::CreateFromFile(path, ZTreeMgr::LAYER_ELEVMOD);
			treeMgr[4] = ZTreeMgr::CreateFromFile(path, ZTreeMgr::LAYER_LABEL);
		}
	}
}

// -----------------------------------------------------------------------

template<>
int TileManager2<SurfTile>::GetElevation(double lng, double lat, double *elev, SurfTile **cache)
{
	int rv = 0;
	loader->WaitForMutex();
	if (lng<0) rv = tiletree[0].Entry()->GetElevation(lng, lat, elev, cache);
	else	   rv = tiletree[1].Entry()->GetElevation(lng, lat, elev, cache);
	loader->ReleaseMutex();
	return rv;
}

// -----------------------------------------------------------------------

template<>
void TileManager2<SurfTile>::Pick(TILEPICK *pPick)
{
	if (bFreeze) return;

	double lng, lat, dst, elv;
	double delta, angle = PI2;
	
	std::list<Tile *> tiles;

	// Give me a list of rendered tiles ----------------------------------------------
	//
	QueryTiles(&tiletree[0], tiles);
	QueryTiles(&tiletree[1], tiles);

	auto it = tiles.begin();

	// Remove tiles whose bounding sphere is not intersected by picking ray ---------
	//
	while (it!=tiles.end()) {
		D3DXMATRIX W; D3DXVECTOR3 bs = (*it)->GetBoundingSpherePos();
		MATRIX4toD3DMATRIX(WorldMatrix(*it), W);
		D3DXVec3TransformCoord(&bs, &bs, &W);
		auto er = it; it++;
		if (D3DXSphereBoundProbe(&bs, (*er)->GetBoundingSphereRad(), &D3DXVECTOR3(0, 0, 0), &D3DXVEC(pPick->vRay)) != 1) tiles.erase(er);
	}

	// Compute some parameters to do the rest of the computations -------------------
	//
	VECTOR3 vCam = vp->GetUnitSurfacePos(pPick->cLng, pPick->cLat);	// Camera position vector;
	VECTOR3 vPck = vp->ToLocal(pPick->vRay);							// Picking ray in Planet's local system
	VECTOR3 vRot = vp->GetRotationAxis();							// Planet's rotation axis
	VECTOR3 vEast = unit(crossp(vRot, vCam));						// Tangent plane east direction 
	VECTOR3 vNorth = unit(crossp(vCam, vEast));						// Tangent plane north direction
	VECTOR3 vDir = unit(vPck - vCam * dotp(vCam, vPck));				// Make the picking ray co-planar with tangent plane
	VECTOR3 vPln = crossp(vCam, vDir);								// Compute picking plane
	
	// Angle between vCam, vPick
	double rPck = PI - acos(dotp(vCam, vPck));						
	// Pick heading [0,360] 0=North 90=East
	double rHed = PI - atan2(-dotp(vEast, vDir), -dotp(vNorth, vDir));
	double sPck = sin(rPck);
	double cDst = vp->CamDist();
	
	// Find the closest tile edge intersected by picking plane ----------------------
	//
	SurfTile *pStart = NULL;

	for (it = tiles.begin(); it != tiles.end(); it++) {
		int rv = (*it)->PlaneIntersection(vPln, rHed, pPick->cLng, pPick->cLat, &lng, &lat, &dst);
		if (rv>=0 && dst < angle) {
			angle = dst;
			pStart = (SurfTile *)(*it);
		}
	}

	if (!pStart) return;

	delta = pStart->height / 256.0;
	angle += delta;

	// Finally scan through remaining terrain. Limit to 2k staps just in case --------
	//
	for (int cnt = 0; cnt < 2000; cnt++) {

		vp->GetLngLat(vCam * cos(angle) + vDir * sin(angle), &lng, &lat);

		int rv = 0;
		if (pStart) rv = pStart->GetElevation(lng, lat, &elv, &pStart);		// Try quick search at first
		if (rv!=1)  rv = GetElevation(lng, lat, &elv, &pStart);				// Browse through tile tree

		if (rv > 0 && pStart) {

			double pr = sPck * cDst / sin(PI - angle - rPck);
			double pe = pr - vp->GetSize();

			// Does the ray intersect terrain at this point
			if (elv >= pe) {
				pPick->pTile = pStart;
				pPick->lng = lng;
				pPick->lat = lat;
				pPick->height = elv;
				pPick->dist = sqrt(pr*pr + cDst*cDst - 2.0*pr*cDst*cos(angle));
				return;
			}
			delta = pStart->height / 256.0;
		}
		angle += delta;
	}
}