// ==============================================================
// TileMgr.cpp
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2006 - 2016 Martin Schweiger
//				 2011 - 2016 Jarmo Nikkanen (D3D9Client modification)
// ==============================================================

// ==============================================================
// class TileManager (implementation)
//
// Planetary surface rendering management, including a simple
// LOD (level-of-detail) algorithm for surface patch resolution.
// ==============================================================

#include <ddraw.h>
#include "TileMgr.h"
#include "VPlanet.h"
#include "D3D9Config.h"
#include "D3D9Surface.h"
#include "D3D9Catalog.h"
#include "D3D9Client.h"
#include "OapiExtension.h"

using namespace oapi;

// Max supported patch resolution level
int SURF_MAX_PATCHLEVEL = 14;
const LONG_PTR NOTILE = (LONG_PTR)-1; // "no tile" flag

//static float TEX2_MULTIPLIER = 4.0f; // microtexture multiplier

struct IDXLIST {
	DWORD idx;
	LONG_PTR ofs;
};

// Some debugging parameters
int tmissing = 0;

// =======================================================================
// Local prototypes
void ReleaseTex(LPDIRECT3DTEXTURE9 pTex)
{
	TileCatalog->Remove(pTex);
	pTex->Release();
}

int compare_idx (const void *el1, const void *el2);

// =======================================================================




TileManager::TileManager (D3D9Client *gclient, const vPlanet *vplanet) : D3D9Effect()
{
	vp = vplanet;
	obj = vp->Object();
	char name[256];
	oapiGetObjectName (obj, name, 256); int len = lstrlen(name) + 2;
	objname = new char[len];
	strcpy_s(objname, len, name);
	ntex = 0;
	nhitex = 0;
	nmask = 0;
	nhispec = 0;
	maxlvl = maxbaselvl = 0;
	microtex = NULL;
	microlvl = 0.0;
	tiledesc = NULL;
	texbuf   = NULL;
	specbuf  = NULL;
	cAmbient = 0;
	bNoTextures = false;
	bPreloadTile = (Config->PlanetPreloadMode!=0);
	if (bPreloadTile) LogAlw("PreLoad Highres textures");
}

// =======================================================================

TileManager::~TileManager ()
{
	DWORD i;

	if (ntex && texbuf) {
		for (i = 0; i < ntex; ++i)
			ReleaseTex(texbuf[i]);
		delete []texbuf;
	}
	if (nmask && specbuf) {
		for (i = 0; i < nmask; ++i)
			ReleaseTex(specbuf[i]);
		delete []specbuf;
	}
	DELETE_SURFACE(microtex);
	if (tiledesc) delete []tiledesc;
	if (objname) delete []objname;
}

// =======================================================================

bool TileManager::LoadPatchData ()
{
	// Read information about specular reflective patch masks
	// from a binary data file

	FILE *binf;
	BYTE minres, maxres, flag;
	int i, idx, npatch;
	nmask = 0;
	char fname[128], path[MAX_PATH];
	strcpy_s(fname, ARRAYSIZE(fname), objname);
	strcat_s(fname, ARRAYSIZE(fname), "_lmask.bin");

	if (!(bGlobalSpecular || bGlobalLights) || !gc->TexturePath(fname, path) || fopen_s(&binf, path, "rb")) {

		for (i = 0; i < patchidx[maxbaselvl]; i++)
			tiledesc[i].flag = 1;
		return false; // no specular reflections, no city lights

	} else {

		WORD *tflag = 0;
		LMASKFILEHEADER lmfh;
		fread (&lmfh, sizeof (lmfh), 1, binf);
		if (!strncmp (lmfh.id, "PLTA0100", 8)) { // v.1.00 format
			minres = lmfh.minres;
			maxres = lmfh.maxres;
			npatch = lmfh.npatch;
			tflag = new WORD[npatch];
			fread (tflag, sizeof(WORD), npatch, binf);
		} else {                                 // pre-v.1.00 format
			fseek (binf, 0, SEEK_SET);
			fread (&minres, 1, 1, binf);
			fread (&maxres, 1, 1, binf);
			npatch = patchidx[maxres] - patchidx[minres-1];
			tflag = new WORD[npatch];
			for (i = 0; i < npatch; i++) {
				fread (&flag, 1, 1, binf);
				tflag[i] = flag;
			}
			//LOGOUT1P("*** WARNING: Old-style texture contents file %s_lmask.bin", cbody->Name());
		}
		fclose (binf);

		for (i = idx = 0; i < patchidx[maxbaselvl]; i++) {
			if (i < patchidx[minres-1]) {
				tiledesc[i].flag = 1; // no mask information -> assume opaque, no lights
			} else {
				flag = (BYTE)tflag[idx++];
				tiledesc[i].flag = flag;
				if (((flag & 3) == 3) || (flag & 4))
					nmask++;
			}
		}
		if (tflag) delete []tflag;
		return true;
	}
}

// =======================================================================

bool TileManager::LoadTileData ()
{
	// Read table of contents file for high-resolution planet tiles

	FILE *file;

	if (maxlvl <= 8) // no tile data required
		return false;

	char fname[128], path[MAX_PATH];
	strcpy_s (fname, ARRAYSIZE(fname), objname);
	strcat_s (fname, ARRAYSIZE(fname), "_tile.bin");

	if (!gc->TexturePath (fname, path) || fopen_s (&file, path, "rb")) {
		LogWrn("Surface Tile TOC not found for %s", fname);
		return false; // TOC file not found
	}

	LogAlw("Reading Tile Data for %s", fname);

	// read file header
	char idstr[9] = "        ";
	fread (idstr, 1, 8, file);
	if (!strncmp (idstr, "PLTS", 4)) {
		tilever = 1;
	} else { // no header: old-style file format
		tilever = 0;
		fseek (file, 0, SEEK_SET);
	}

	DWORD n, i, j;
	fread (&n, sizeof(DWORD), 1, file);
	TILEFILESPEC *tfs = new TILEFILESPEC[n];
	fread (tfs, sizeof(TILEFILESPEC), n, file);

	if (bPreloadTile) {
		if (tilever >= 1) { // convert texture offsets to indices
			IDXLIST *idxlist = new IDXLIST[n];
			for (i = 0; i < n; i++) {
				idxlist[i].idx = i;
				idxlist[i].ofs = tfs[i].sidx;
			}
			qsort (idxlist, n, sizeof(IDXLIST), compare_idx);
			for (i = 0; i < n && idxlist[i].ofs != NOTILE; i++)
				tfs[idxlist[i].idx].sidx = i;

			for (i = 0; i < n; i++) {
				idxlist[i].idx = i;
				idxlist[i].ofs = tfs[i].midx;
			}
			qsort (idxlist, n, sizeof(IDXLIST), compare_idx);
			for (i = 0; i < n && idxlist[i].ofs != NOTILE; i++)
				tfs[idxlist[i].idx].midx = i;

			tilever = 0;
			delete []idxlist;
		}
	}

	TILEDESC *tile8 = tiledesc + patchidx[7];
	for (i = 0; i < 364; i++) { // loop over level 8 tiles
		TILEDESC &tile8i = tile8[i];
		for (j = 0; j < 4; j++)
			if (tfs[i].subidx[j])
				AddSubtileData (tile8i, tfs, i, j, 9);
	}

	fclose (file);
	delete []tfs;
	return true;
}

// =======================================================================

int compare_idx (const void *el1, const void *el2)
{
	register const IDXLIST *idx1 = static_cast<const IDXLIST*>(el1);
	register const IDXLIST *idx2 = static_cast<const IDXLIST*>(el2);
	return (idx1->ofs < idx2->ofs ? -1 : idx1->ofs > idx2->ofs ? 1 : 0);
}

// =======================================================================

bool TileManager::AddSubtileData (TILEDESC &td, TILEFILESPEC *tfs, DWORD idx, DWORD sub, DWORD lvl)
{
	DWORD j, subidx = tfs[idx].subidx[sub];
	TILEFILESPEC &t = tfs[subidx];
	bool bSubtiles = false;
	for (j = 0; j < 4; j++)
		if (t.subidx[j]) { bSubtiles = true; break; }
	if (t.flags || bSubtiles) {
		if ((int)lvl <= maxlvl) {
			td.subtile[sub] = tilebuf->AddTile();
			td.subtile[sub]->flag = t.flags;
			td.subtile[sub]->tex = (LPDIRECT3DTEXTURE9)t.sidx;
			if (bGlobalSpecular || bGlobalLights) {
				if (t.midx != NOTILE) {
					td.subtile[sub]->ltex = (LPDIRECT3DTEXTURE9)t.midx;
				}
			} else {
				td.subtile[sub]->flag = 1; // remove specular flag
			}
			td.subtile[sub]->flag |= 0x80; // 'Not-loaded' flag
			if (!tilever)
				td.subtile[sub]->flag |= 0x40; // 'old-style index' flag
			// recursively step down to higher resolutions
			if (bSubtiles) {
				for (j = 0; j < 4; j++) {
					if (t.subidx[j]) AddSubtileData (*td.subtile[sub], tfs, subidx, j, lvl+1);
				}
			}
			nhitex++;
			if (t.midx != NOTILE) nhispec++;
		} else td.subtile[sub] = NULL;
	}
	return true;
}

// =======================================================================

void TileManager::LoadTextures (char *modstr)
{
	// pre-load level 1-8 textures
	ntex = patchidx[maxbaselvl];
	texbuf = new LPDIRECT3DTEXTURE9[ntex];
	char fname[256];
	strcpy_s (fname, 256, objname);
	if (modstr) strcat_s (fname, 256, modstr);
	strcat_s (fname, 256, ".tex");

	if (ntex = LoadPlanetTextures(fname, texbuf, 0, ntex)) {
		while ((int)ntex < patchidx[maxbaselvl]) maxlvl = --maxbaselvl;
		while ((int)ntex > patchidx[maxbaselvl]) ReleaseTex(texbuf[--ntex]);
		// not enough textures loaded for requested resolution level
		for (int i = 0; i < patchidx[maxbaselvl]; ++i)
			tiledesc[i].tex = texbuf[i];
	} else {
		delete []texbuf;
		texbuf = 0;
		bNoTextures = true;
		// no textures at all!
	}

	//  pre-load highres tile textures
	if (bPreloadTile && nhitex) {
		TILEDESC *tile8 = tiledesc + patchidx[7];
		PreloadTileTextures (tile8, nhitex, nhispec);
	}
}

// =======================================================================

void TileManager::PreloadTileTextures (TILEDESC *tile8, DWORD ntex, DWORD nmask)
{
	// Load tile surface and mask/light textures, and copy them into the tile tree

	char fname[256];
	DWORD i, j, nt = 0, nm = 0;
	LPDIRECT3DTEXTURE9 *texbuf = NULL, *maskbuf = NULL;

	if (ntex) {  // load surface textures
		texbuf = new LPDIRECT3DTEXTURE9[ntex];
		strcpy_s (fname, 256, objname);
		strcat_s (fname, 256, "_tile.tex");

		gc->OutputLoadStatus(fname, 1);

		nt = LoadPlanetTextures(fname, texbuf, 0, ntex);
		LogAlw("Number of textures loaded = %u",nt);
	}
	if (nmask) { // load mask/light textures
		maskbuf = new LPDIRECT3DTEXTURE9[nmask];
		strcpy_s (fname, 256, objname);
		strcat_s (fname, 256, "_tile_lmask.tex");

		gc->OutputLoadStatus(fname, 1);

		nm = LoadPlanetTextures(fname, maskbuf, 0, nmask);
	}
	// copy textures into tile tree
	for (i = 0; i < 364; i++) {
		TILEDESC *tile8i = tile8+i;
		for (j = 0; j < 4; j++)
			if (tile8i->subtile[j])
				AddSubtileTextures (tile8i->subtile[j], texbuf, nt, maskbuf, nm);
	}
	// release unused textures
	if (nt) {
		for (i = 0; i < nt; i++)
			if (texbuf[i])
				ReleaseTex(texbuf[i]);
		delete []texbuf;
	}
	if (nm) {
		for (i = 0; i < nm; i++)
			if (maskbuf[i])
				ReleaseTex(maskbuf[i]);
		delete []maskbuf;
	}
}

// =======================================================================

void TileManager::AddSubtileTextures (TILEDESC *td, LPDIRECT3DTEXTURE9 *tbuf, DWORD nt, LPDIRECT3DTEXTURE9 *mbuf, DWORD nm)
{
	DWORD i;

	// --------------------------------------------------------------------------------------------------------------
	// jarmonik: 30-jul-2021
	// This is tricky. At first td->tex is an index to a texture array and later it becomes a pointer to a texture ?!! 
	// --------------------------------------------------------------------------------------------------------------

	LONG_PTR tidx = (LONG_PTR)td->tex;  // copy surface texture
	if (tidx != NOTILE) {
		if (tidx < LONG_PTR(nt)) {
			td->tex = tbuf[tidx];
			tbuf[tidx] = NULL;
		} else {                   // inconsistency
			tmissing++;
			td->tex = NULL;
		}
	} else td->tex = NULL;

	LONG_PTR midx = (LONG_PTR)td->ltex;  // copy mask/light texture
	if (midx != NOTILE) {
		if (midx < LONG_PTR(nm)) {
			td->ltex = mbuf[midx];
			mbuf[midx] = NULL;
		} else {                  // inconsistency
			tmissing++;
			td->ltex = NULL;
		}
	} else td->ltex = NULL;
	td->flag &= ~0x80; // remove "not loaded" flag

	for (i = 0; i < 4; i++) {
		if (td->subtile[i]) AddSubtileTextures (td->subtile[i], tbuf, nt, mbuf, nm);
	}
}

// =======================================================================

void TileManager::LoadSpecularMasks ()
{
	if (nmask) {

		int i;
		DWORD n;
		char fname[256];

		strcpy_s (fname, 256, objname);
		strcat_s (fname, 256, "_lmask.tex");

		gc->OutputLoadStatus(fname, 1);

		specbuf = new LPDIRECT3DTEXTURE9[nmask];
		if (n = LoadPlanetTextures(fname, specbuf, 0, nmask)) {
			if (n < nmask) {
				//LOGOUT1P("Transparency texture mask file too short: %s_lmask.tex", cbody->Name());
				//LOGOUT("Disabling specular reflection for this planet");
				delete []specbuf;
				specbuf = NULL;
				nmask = 0;
				for (i = 0; i < patchidx[maxbaselvl]; i++)
					tiledesc[i].flag = 1;
			} else {
				for (i = n = 0; i < patchidx[maxbaselvl]; i++) {
					if (((tiledesc[i].flag & 3) == 3) || (tiledesc[i].flag & 4)) {
						if (n < nmask) tiledesc[i].ltex = specbuf[n++];
						else tiledesc[i].flag = 1;
					}
					if (!bGlobalLights) tiledesc[i].flag &= 0xFB;
					if (!bGlobalSpecular) tiledesc[i].flag &= 0xFD, tiledesc[i].flag |= 1;
				}
			}
		} else {
			nmask = 0;
			for (i = 0; i < patchidx[maxbaselvl]; i++)
				tiledesc[i].flag = 1;
		}
	}
}

// ==============================================================

void TileManager::SetAmbientColor(D3DCOLOR c)
{
	cAmbient = c;
}

// ==============================================================

void TileManager::Render(LPDIRECT3DDEVICE9 dev, D3DXMATRIX &wmat, double scale, int level, double viewap, bool bfog)
{
	VECTOR3 gpos;
	D3DXMATRIX imat;

	FX->SetFloat(eDistScale, 1.0f/float(scale));

	level = min (level, maxlvl);

	RenderParam.dev = dev;
	D3DMAT_Copy (&RenderParam.wmat, &wmat);
	D3DMAT_Copy (&RenderParam.wmat_tmp, &wmat);
	D3DMAT_MatrixInvert (&imat, &wmat);
	RenderParam.cdir = _V(imat._41, imat._42, imat._43); // camera position in local coordinates (units of planet radii)
	RenderParam.cpos = vp->PosFromCamera() * scale;
	normalise (RenderParam.cdir);                        // camera direction
	RenderParam.bfog = bfog;

	oapiGetRotationMatrix (obj, &RenderParam.grot);
	RenderParam.grot *= scale;
	oapiGetGlobalPos (obj, &gpos);

	RenderParam.bCockpit = (oapiCameraMode()==CAM_COCKPIT);
	RenderParam.objsize = oapiGetSize (obj);
	RenderParam.cdist = vp->CamDist() / vp->GetSize(); // camera distance in units of planet radius
	RenderParam.viewap = (viewap ? viewap : acos (1.0/max (1.0, RenderParam.cdist)));
	RenderParam.sdir = tmul (RenderParam.grot, -gpos);
	RenderParam.horzdist = sqrt(RenderParam.cdist*RenderParam.cdist-1.0) * RenderParam.objsize;
	normalise (RenderParam.sdir); // sun direction in planet frame

	// limit resolution for fast camera movements
	double limitstep, cstep = acos (dotp (RenderParam.cdir, pcdir));
	int maxlevel = SURF_MAX_PATCHLEVEL;
	static double limitstep0 = 5.12 * pow(2.0, -(double)SURF_MAX_PATCHLEVEL);
	for (limitstep = limitstep0; cstep > limitstep && maxlevel > 5; limitstep *= 2.0)
		maxlevel--;
	level = min (level, maxlevel);

	RenderParam.tgtlvl = level;

	int startlvl = min (level, 8);
	int hemisp, ilat, ilng, idx;
	int  nlat = NLAT[startlvl];
	int *nlng = NLNG[startlvl];
	int texofs = patchidx[startlvl-1];
	TILEDESC *td = tiledesc + texofs;

	TEXCRDRANGE range = {0,1,0,1};

	if (level <= 4) {
		int npatch = patchidx[level] - patchidx[level-1];
		RenderSimple(level, npatch, td, &RenderParam.wmat);

	} else {

		WaitForSingleObject (tilebuf->hQueueMutex, INFINITE); // make sure we can write to texture request queue
		for (hemisp = idx = 0; hemisp < 2; hemisp++) {
			if (hemisp) { // flip world transformation to southern hemisphere
				D3DXMatrixMultiply(&RenderParam.wmat, &Rsouth, &RenderParam.wmat);
				D3DMAT_Copy (&RenderParam.wmat_tmp, &RenderParam.wmat);
				RenderParam.grot.m12 = -RenderParam.grot.m12;
				RenderParam.grot.m13 = -RenderParam.grot.m13;
				RenderParam.grot.m22 = -RenderParam.grot.m22;
				RenderParam.grot.m23 = -RenderParam.grot.m23;
				RenderParam.grot.m32 = -RenderParam.grot.m32;
				RenderParam.grot.m33 = -RenderParam.grot.m33;
			}

			InitRenderTile();

			for (ilat = nlat-1; ilat >= 0; ilat--) {
				for (ilng = 0; ilng < nlng[ilat]; ilng++) {
					ProcessTile (startlvl, hemisp, ilat, nlat, ilng, nlng[ilat], td+idx,
						range, td[idx].tex, td[idx].ltex, td[idx].flag,
						range, td[idx].tex, td[idx].ltex, td[idx].flag);
					idx++;
				}
			}

			EndRenderTile();
		}
		ReleaseMutex (tilebuf->hQueueMutex);
	}

	pcdir = RenderParam.cdir; // store camera direction
}

// =======================================================================

void TileManager::ProcessTile (int lvl, int hemisp, int ilat, int nlat, int ilng, int nlng, TILEDESC *tile,
	const TEXCRDRANGE &range, LPDIRECT3DTEXTURE9 tex, LPDIRECT3DTEXTURE9 ltex, DWORD flag,
	const TEXCRDRANGE &bkp_range, LPDIRECT3DTEXTURE9 bkp_tex, LPDIRECT3DTEXTURE9 bkp_ltex, DWORD bkp_flag)
{

	// Check if patch is visible from camera position
	static const double rad0 = sqrt(2.0)*PI05*0.5;
	VECTOR3 cnt = TileCentre (hemisp, ilat, nlat, ilng, nlng);
	double rad = rad0/(double)nlat;
	double x = dotp (RenderParam.cdir, cnt);
	double adist = acos(x) - rad;

	if (adist >= RenderParam.viewap) {
		//if (RenderParam.bCockpit) tilebuf->DeleteSubTiles(tile); // remove tile descriptions below
		return;
	}

	// Set world transformation matrix for patch
	SetWorldMatrix (ilng, nlng, ilat, nlat);

	float bsScale = D3DMAT_BSScaleFactor(&mWorld);

	// Check if patch bounding box intersects viewport
	if (!IsTileInView(lvl, ilat, bsScale)) {
		tilebuf->DeleteSubTiles (tile); // remove tile descriptions below
		return;
	}

	// Reduce resolution for distant or oblique patches
	bool bStepDown = (lvl < RenderParam.tgtlvl);
	bool bCoarseTex = false;
	if (bStepDown && lvl >= 8 && adist > 0.0) {
		double lat1, lat2, lng1, lng2, clat, clng, crad;
		double adist_lng, adist_lat, adist2;
		TileExtents (hemisp, ilat, nlat, ilng, nlng, lat1, lat2, lng1, lng2);
		oapiLocalToEqu (obj, RenderParam.cdir, &clng, &clat, &crad);
		if      (clng < lng1-PI) clng += PI2;
		else if (clng > lng2+PI) clng -= PI2;
		if      (clng < lng1) adist_lng = lng1-clng;
		else if (clng > lng2) adist_lng = clng-lng2;
		else                  adist_lng = 0.0;
		if      (clat < lat1) adist_lat = lat1-clat;
		else if (clat > lat2) adist_lat = clat-lat2;
		else                  adist_lat = 0.0;
		adist2 = max (adist_lng, adist_lat);

		// reduce resolution further for tiles that are visible
		// under a very oblique angle
		double cosa = cos(adist2);
		double a = sin(adist2);
		double b = RenderParam.cdist-cosa;
		double ctilt = b*cosa/sqrt(a*a*(1.0+2.0*b)+b*b); // tile visibility tilt angle cosine
		if (adist2 > rad*(2.0*ctilt+0.3)) {
			bStepDown = false;
			if (adist2 > rad*(4.2*ctilt+0.3))
				bCoarseTex = true;
		}
	}

	// Recursion to next level: subdivide into 2x2 patch
	if (bStepDown) {
		int i, j, idx = 0;
		float du = (range.tumax-range.tumin) * 0.5f;
		float dv = (range.tvmax-range.tvmin) * 0.5f;
		TEXCRDRANGE subrange;
		static TEXCRDRANGE fullrange = {0,1,0,1};
		for (i = 1; i >= 0; i--) {
			subrange.tvmax = (subrange.tvmin = range.tvmin + (1-i)*dv) + dv;
			for (j = 0; j < 2; j++) {
				subrange.tumax = (subrange.tumin = range.tumin + j*du) + du;
				TILEDESC *subtile = tile->subtile[idx];
				bool isfull = true;
				if (!subtile) {
					tile->subtile[idx] = subtile = tilebuf->AddTile();
					isfull = false;
				} else if (subtile->flag & 0x80) { // not yet loaded
					if ((tile->flag & 0x80) == 0) // only load subtile texture if parent texture is present
						tilebuf->LoadTileAsync (objname, subtile);
					isfull = false;
				}
				if (isfull)
					isfull = (subtile->tex != NULL);
				if (isfull)
					ProcessTile (lvl+1, hemisp, ilat*2+i, nlat*2, ilng*2+j, nlng*2, subtile,
						fullrange, subtile->tex, subtile->ltex, subtile->flag,
						subrange, tex, ltex, flag);
				else
					ProcessTile (lvl+1, hemisp, ilat*2+i, nlat*2, ilng*2+j, nlng*2, subtile,
						subrange, tex, ltex, flag,
						subrange, tex, ltex, flag);
				idx++;
			}
		}
	}
	else {

		// check if the tile is visible in the viewport ---------------------------------
		//
		VBMESH *mesh = &PATCH_TPL[lvl][ilat];

		float bsrad = mesh->bsRad * bsScale;
		D3DXVECTOR3 vBS;
		D3DXVec3TransformCoord(&vBS, &mesh->bsCnt, &mWorld);
		float dist = D3DXVec3Length(&vBS);
		if ((dist-bsrad)>RenderParam.horzdist) return; //Tile is behind the horizon
		if (gc->GetScene()->IsVisibleInCamera(&vBS, bsrad)==false) return;

		D3D9Stats.Old.Verts += mesh->nv;
		D3D9Stats.Old.Tiles[lvl]++;

		// actually render the tile at this level ---------------------------------------
		//
		double sdist = acos (dotp (RenderParam.sdir, cnt));

		if (bCoarseTex) {
			//if (sdist > PI05+rad && bkp_flag & 2) bkp_flag &= 0xFD;
			RenderTile (lvl, hemisp, ilat, nlat, ilng, nlng, sdist, tile, bkp_range, bkp_tex, bkp_ltex, bkp_flag);
		} else {
			//if (sdist > PI05+rad && flag & 2) flag &= 0xFD;
			RenderTile (lvl, hemisp, ilat, nlat, ilng, nlng, sdist, tile, range, tex, ltex, flag);
		}
	}
}

// =======================================================================
// returns the direction of the tile centre from the planet centre in local
// planet coordinates

VECTOR3 TileManager::TileCentre (int hemisp, int ilat, int nlat, int ilng, int nlng)
{
	double cntlat = PI*0.5 * ((double)ilat+0.5)/(double)nlat,      slat = sin(cntlat), clat = cos(cntlat);
	double cntlng = PI*2.0 * ((double)ilng+0.5)/(double)nlng + PI, slng = sin(cntlng), clng = cos(cntlng);
	if (hemisp) return _V(clat*clng, -slat, -clat*slng);
	else        return _V(clat*clng,  slat,  clat*slng);
}

// =======================================================================

void TileManager::TileExtents (int hemisp, int ilat, int nlat, int ilng, int nlng, double &lat1, double &lat2, double &lng1, double &lng2) const
{
	lat1 = PI05 * (double)ilat/(double)nlat;
	lat2 = lat1 + PI05/(double)nlat;
	lng1 = PI2 * (double)ilng/(double)nlng + PI;
	lng2 = lng1 + PI2/nlng;
	if (hemisp) {
		double tmp = lat1; lat1 = -lat2; lat2 = -tmp;
		tmp = lng1; lng1 = -lng2; lng2 = -tmp;
		if (lng2 < 0) lng1 += PI2, lng2 += PI2;
	}
}

// =======================================================================

int TileManager::IsTileInView(int lvl, int ilat, float scale)
{
	VBMESH &mesh = PATCH_TPL[lvl][ilat];
	float rad = mesh.bsRad * scale;
	D3DXVECTOR3 vP;
	D3DXVec3TransformCoord(&vP, &mesh.bsCnt, &mWorld);

	//float dist = D3DXVec3Length(&vP);
	//if ((dist-rad)>RenderParam.horzdist) return -1;	 //Tile is behind the horizon

	return gc->GetScene()->IsVisibleInCamera(&vP, rad); //Is the bounding sphere visible in a viewing volume ?
}

// =======================================================================

void TileManager::SetWorldMatrix (int ilng, int nlng, int ilat, int nlat)
{
	// set up world transformation matrix
	D3DXMATRIX rtile, wtrans;
	double lng = PI*2.0 * (double)ilng/(double)nlng + PI; // add pi so texture wraps at +-180°
	D3DMAT_RotY (&rtile, lng);

	if (nlat > 8) {
		// The reference point for these tiles has been shifted from the centre of the sphere
		// to the lower left corner of the tile, to reduce offset distances which cause rounding
		// errors in the single-precision world matrix. The offset calculations are done in
		// double-precision before copying them into the world matrix.
		double lat = PI05 * (double)ilat/(double)nlat;
		double s = RenderParam.objsize;
		double dx = s*cos(lng)*cos(lat); // the offsets between sphere centre and tile corner
		double dy = s*sin(lat);
		double dz = s*sin(lng)*cos(lat);
		RenderParam.wmat_tmp._41 = (float)(dx*RenderParam.grot.m11 + dy*RenderParam.grot.m12 + dz*RenderParam.grot.m13 + RenderParam.cpos.x);
		RenderParam.wmat_tmp._42 = (float)(dx*RenderParam.grot.m21 + dy*RenderParam.grot.m22 + dz*RenderParam.grot.m23 + RenderParam.cpos.y);
		RenderParam.wmat_tmp._43 = (float)(dx*RenderParam.grot.m31 + dy*RenderParam.grot.m32 + dz*RenderParam.grot.m33 + RenderParam.cpos.z);
		D3DXMatrixMultiply(&mWorld, &rtile, &RenderParam.wmat_tmp);
	} else {
		D3DXMatrixMultiply(&mWorld, &rtile, &RenderParam.wmat);
	}
}

// ==============================================================

bool TileManager::SpecularColour (D3DCOLORVALUE *col)
{
	if (!atmc) {
		col->r = col->g = col->b = spec_base;
		return false;
	} else {
		double fac = 0.7; // needs thought ...
		double cosa = dotp (RenderParam.cdir, RenderParam.sdir);
		double alpha = 0.5*acos(cosa); // sun reflection angle
		double scale = sin(alpha)*fac;
		col->r = (float)max(0.0, spec_base - scale*atmc->color0.x);
		col->g = (float)max(0.0, spec_base - scale*atmc->color0.y);
		col->b = (float)max(0.0, spec_base - scale*atmc->color0.z);
		return true;
	}
}

// ==============================================================

void TileManager::GlobalInit (D3D9Client *gclient)
{
	LogAlw("TileManager::GlobalInit()...");

	LPDIRECT3DDEVICE9 dev = gclient->GetDevice();

	bGlobalSpecular = *(bool*)gclient->GetConfigParam (CFGPRM_SURFACEREFLECT);
	bGlobalRipple   = bGlobalSpecular && *(bool*)gclient->GetConfigParam (CFGPRM_SURFACERIPPLE);
	bGlobalLights   = *(bool*)gclient->GetConfigParam (CFGPRM_SURFACELIGHTS);

	// Level 1 patch template
	CreateSphere (dev, PATCH_TPL_1, 6, false, 0, 64);

 	// Level 2 patch template
	CreateSphere (dev, PATCH_TPL_2, 8, false, 0, 128);

 	// Level 3 patch template
	CreateSphere (dev, PATCH_TPL_3, 12, false, 0, 256);

 	// Level 4 patch templates
	CreateSphere (dev, PATCH_TPL_4[0], 16, true, 0, 256);
	CreateSphere (dev, PATCH_TPL_4[1], 16, true, 1, 256);

 	// Level 5 patch template
	CreateSpherePatch (dev, PATCH_TPL_5, 4, 1, 0, 18);

 	// Level 6 patch templates
	CreateSpherePatch (dev, PATCH_TPL_6[0], 8, 2, 0, 10, 16);
	CreateSpherePatch (dev, PATCH_TPL_6[1], 4, 2, 1, 12);

 	// Level 7 patch templates
	CreateSpherePatch (dev, PATCH_TPL_7[0], 16, 4, 0, 12, 12, false);
	CreateSpherePatch (dev, PATCH_TPL_7[1], 16, 4, 1, 12, 12, false);
	CreateSpherePatch (dev, PATCH_TPL_7[2], 12, 4, 2, 10, 16, true);
	CreateSpherePatch (dev, PATCH_TPL_7[3],  6, 4, 3, 12, -1, true);

 	// Level 8 patch templates
	CreateSpherePatch (dev, PATCH_TPL_8[0], 32, 8, 0, 12, 15, false, true, true);
	CreateSpherePatch (dev, PATCH_TPL_8[1], 32, 8, 1, 12, 15, false, true, true);
	CreateSpherePatch (dev, PATCH_TPL_8[2], 30, 8, 2, 12, 16, false, true, true);
	CreateSpherePatch (dev, PATCH_TPL_8[3], 28, 8, 3, 12, 12, false, true, true);
	CreateSpherePatch (dev, PATCH_TPL_8[4], 24, 8, 4, 12, 12, false, true, true);
	CreateSpherePatch (dev, PATCH_TPL_8[5], 18, 8, 5, 12, 12, false, true, true);
	CreateSpherePatch (dev, PATCH_TPL_8[6], 12, 8, 6, 10, 16, true,  true, true);
	CreateSpherePatch (dev, PATCH_TPL_8[7],  6, 8, 7, 12, -1, true,  true, true);


	// Patch templates for level 9 and beyond
	const int n = 8;
	const int nlng8[8] = {32,32,30,28,24,18,12,6};
	const int res8[8] = {15,15,16,12,12,12,12,12};
	int mult = 2, idx, lvl, i, j;
	for (lvl = 9; lvl <= SURF_MAX_PATCHLEVEL; lvl++) {
		idx = 0;
		for (i = 0; i < 8; i++) {
			for (j = 0; j < mult; j++) {
				if (idx < n*mult)
					CreateSpherePatch (dev, PATCH_TPL[lvl][idx], nlng8[i]*mult, n*mult, idx, 12, res8[i], false, true, true, true);
				else
					CreateSpherePatch (dev, PATCH_TPL[lvl][idx], nlng8[i]*mult, n*mult, idx, 12, -1, true, true, true, true);

				idx++;
			}
		}
		mult *= 2;
	}

	// create the system-wide tile cache
	tilebuf = new TileBuffer (gclient);

	// viewport size for clipping calculations
	D3DVIEWPORT9 vp;
	dev->GetViewport (&vp);
	vpX0 = vp.X, vpX1 = vpX0 + vp.Width;
	vpY0 = vp.Y, vpY1 = vpY0 + vp.Height;

	// rotation matrix for flipping patches onto southern hemisphere
	D3DMAT_RotX (&Rsouth, PI);
}

// ==============================================================

void TileManager::GlobalExit ()
{
	int i;
	DestroyVBMesh (PATCH_TPL_1);
	DestroyVBMesh (PATCH_TPL_2);
	DestroyVBMesh (PATCH_TPL_3);
	for (i = 0; i <  2; i++) DestroyVBMesh (PATCH_TPL_4[i]);
	DestroyVBMesh (PATCH_TPL_5);
	for (i = 0; i <  2; i++) DestroyVBMesh (PATCH_TPL_6[i]);
	for (i = 0; i <  4; i++) DestroyVBMesh (PATCH_TPL_7[i]);
	for (i = 0; i <  8; i++) DestroyVBMesh (PATCH_TPL_8[i]);

	const int n = 8;
	int mult = 2, lvl;
	for (lvl = 9; lvl <= SURF_MAX_PATCHLEVEL; lvl++) {
		for (i = 0; i < n*mult; i++) DestroyVBMesh (PATCH_TPL[lvl][i]);
		mult *= 2;
	}

	delete tilebuf;
}

// ==============================================================

void TileManager::SetMicrotexture (const char *fname)
{
	if (fname) microtex = gc->clbkLoadTexture(fname);
	else microtex = NULL;
}

// ==============================================================

void TileManager::SetMicrolevel (double lvl)
{
	microlvl = lvl;
}

// ==============================================================
// static member initialisation

DWORD TileManager::vbMemCaps = 0;
int TileManager::patchidx[9] = {0, 1, 2, 3, 5, 13, 37, 137, 501};
bool TileManager::bGlobalSpecular = false;
bool TileManager::bGlobalRipple = false;
bool TileManager::bGlobalLights = false;

TileBuffer *TileManager::tilebuf = NULL;
D3DXMATRIX  TileManager::Rsouth;

VBMESH TileManager::PATCH_TPL_1;
VBMESH TileManager::PATCH_TPL_2;
VBMESH TileManager::PATCH_TPL_3;
VBMESH TileManager::PATCH_TPL_4[2];
VBMESH TileManager::PATCH_TPL_5;
VBMESH TileManager::PATCH_TPL_6[2];
VBMESH TileManager::PATCH_TPL_7[4];
VBMESH TileManager::PATCH_TPL_8[8];
VBMESH TileManager::PATCH_TPL_9[16];
VBMESH TileManager::PATCH_TPL_10[32];
VBMESH TileManager::PATCH_TPL_11[64];
VBMESH TileManager::PATCH_TPL_12[128];
VBMESH TileManager::PATCH_TPL_13[256];
VBMESH TileManager::PATCH_TPL_14[512];
VBMESH *TileManager::PATCH_TPL[15] = {
	0, &PATCH_TPL_1, &PATCH_TPL_2, &PATCH_TPL_3, PATCH_TPL_4, &PATCH_TPL_5,
	PATCH_TPL_6, PATCH_TPL_7, PATCH_TPL_8, PATCH_TPL_9, PATCH_TPL_10,
	PATCH_TPL_11, PATCH_TPL_12, PATCH_TPL_13, PATCH_TPL_14
};

int TileManager::NLAT[9] = {0,1,1,1,1,1,2,4,8};
int TileManager::NLNG5[1] = {4};
int TileManager::NLNG6[2] = {8,4};
int TileManager::NLNG7[4] = {16,16,12,6};
int TileManager::NLNG8[8] = {32,32,30,28,24,18,12,6};
int *TileManager::NLNG[9] = {0,0,0,0,0,NLNG5,NLNG6,NLNG7,NLNG8};

DWORD TileManager::vpX0, TileManager::vpX1, TileManager::vpY0, TileManager::vpY1;


// =======================================================================
// =======================================================================
// Class TileBuffer: implementation

TileBuffer::TileBuffer (const oapi::D3D9Client *gclient)
	: gc(gclient)
	, nbuf(0)
	, nused(0)
	, last(0)
	, bLoadMip(true)
{
	DWORD id;

	// Initialize statics
	nqueue = queue_in = queue_out = 0;
	hQueueMutex = CreateMutex (0, FALSE, NULL);
	hLoadThread = CreateThread (NULL, 2048, LoadTile_ThreadProc, this, 0, &id);
}

// =======================================================================

TileBuffer::~TileBuffer()
{
	LogAlw("=============== Deleting %u Tile Buffers =================",nbuf);

	CloseHandle(hQueueMutex); hQueueMutex = NULL;

	TerminateLoadThread();

	if (nbuf) {
		for (DWORD i = 0; i < nbuf; i++)
			if (buf[i]) {
				if (!(buf[i]->flag & 0x80)) { // if loaded, release tile textures
					if (buf[i]->tex)  ReleaseTex(buf[i]->tex);
					if (buf[i]->ltex) ReleaseTex(buf[i]->ltex);
				}
				delete buf[i];
			}
		delete []buf;
	}
}

// =======================================================================

bool TileBuffer::ShutDown()
{
	if (hLoadThread) {
		TerminateLoadThread();
		return true;
	}
	return false;
}

// =======================================================================

void TileBuffer::HoldThread(bool bHold)
{
	bHoldThread = bHold;
}

// =======================================================================

void TileBuffer::TerminateLoadThread()
{
	if (hLoadThread) {
		// Signal thread to stop and wait for it to happen
		SetEvent(hStopThread);
		WaitForSingleObject(hLoadThread, INFINITE);
		// Clean up for next run
		ResetEvent(hStopThread);
		CloseHandle(hLoadThread);
		hLoadThread = NULL;
	}
}

// =======================================================================

TILEDESC *TileBuffer::AddTile ()
{
	TILEDESC *td = new TILEDESC;
	memset (td, 0, sizeof(TILEDESC));
	DWORD i, j;

	if (nused == nbuf) {
		TILEDESC **tmp = new TILEDESC*[nbuf+16];
		if (nbuf) {
			memcpy (tmp, buf, nbuf*sizeof(TILEDESC*));
			delete []buf;
		}
		memset (tmp+nbuf, 0, 16*sizeof(TILEDESC*));
		buf = tmp;
		nbuf += 16;
		last = nused;
	} else {
		for (i = 0; i < nbuf; i++) {
			j = (i+last)%nbuf;
			if (!buf[j]) {
				last = j;
				break;
			}
		}
        if (i == nbuf) {
			/* Problems! */;
        }
	}
	buf[last] = td;
	td->ofs = last;
	nused++;
	return td;
}

// =======================================================================

void TileBuffer::DeleteSubTiles (TILEDESC *tile)
{
	for (DWORD i = 0; i < 4; i++)
		if (tile->subtile[i]) {
			if (DeleteTile (tile->subtile[i]))
				tile->subtile[i] = 0;
		}
}

// =======================================================================

bool TileBuffer::DeleteTile (TILEDESC *tile)
{
	bool del = true;
	for (DWORD i = 0; i < 4; i++)
		if (tile->subtile[i]) {
			if (DeleteTile (tile->subtile[i]))
				tile->subtile[i] = 0;
			else
				del = false;
		}

	if (tile->tex || !del) {
		return false; // tile or subtile contains texture -> don't deallocate
	} else {
		buf[tile->ofs] = 0; // remove from list
		delete tile;
		nused--;
		return true;
	}
}

// =======================================================================

bool TileBuffer::LoadTileAsync (const char *name, TILEDESC *tile)
{
	bool ok = true;

	if (nqueue == MAXQUEUE)
		ok = false; // queue full
	else {
		for (int i = 0; i < nqueue; i++) {
			int j = (i+queue_out) % MAXQUEUE;
			if (loadqueue[j].td == tile)
			{ ok = false; break; }// request already present
		}
	}

	if (ok) {
		QUEUEDESC *qd = loadqueue+queue_in;
		qd->name = name;
		qd->td = tile;

		nqueue++;
		queue_in = (queue_in+1) % MAXQUEUE;
	}
	return ok;
}

// =======================================================================

DWORD WINAPI TileBuffer::LoadTile_ThreadProc (void *data)
{
	static const LONG_PTR TILESIZE = 32896; // default texture size for old-style texture files
	TileBuffer *tb = static_cast<TileBuffer*>(data);
	auto device = tb->gc->GetDevice();
	bool load;
	static QUEUEDESC qd;
	DWORD idle = 1000/Config->PlanetLoadFrequency;

	LogAlw("TileBuffer::LoadTile thread started");

	bool bFirstRun = true;
	while (bFirstRun || WAIT_OBJECT_0 != WaitForSingleObject(hStopThread, idle))
	{
		bFirstRun = false;

		if (bHoldThread) continue;

		WaitForSingleObject(hQueueMutex, INFINITE);
		if (load = (nqueue > 0)) {
			memcpy (&qd, loadqueue+queue_out, sizeof(QUEUEDESC));
		}
		ReleaseMutex (hQueueMutex);

		if (load) {
			char fname[MAX_PATH];
			TILEDESC *td = qd.td;
			LPDIRECT3DTEXTURE9 tex, mask = 0;
			LONG_PTR tidx, midx;
			LONG_PTR ofs;

			if ((td->flag & 0x80) == 0)
				MessageBeep (-1);

			tidx = (LONG_PTR)td->tex;
			if (tidx == NOTILE)
				tex = NULL; // "no texture" flag
			else {
				ofs = (td->flag & 0x40) ? tidx * TILESIZE : tidx;
				strcpy_s (fname, 256, qd.name);
				strcat_s (fname, 256, "_tile.tex");

				HRESULT hr = ReadDDSSurface (device, fname, ofs, &tex, false);

				if (hr != S_OK) {
					tex = NULL;
					LogErr("Failed to load a tile using ReadDDSSurface() offset=%u, name=%s, ErrorCode = %d",ofs,fname,hr);
				}
			}
			// Load the specular mask and/or light texture
			if (((td->flag & 3) == 3) || (td->flag & 4)) {
				midx = (LONG_PTR)td->ltex;
				if (midx == NOTILE)
					mask = NULL; // "no mask" flag
				else {
					ofs = (td->flag & 0x40) ? midx * TILESIZE : midx;
					strcpy_s (fname, 256, qd.name);
					strcat_s (fname, 256, "_tile_lmask.tex");
					if (ReadDDSSurface (device, fname, ofs, &mask, false) != S_OK) mask = NULL;
				}
			}
			// apply loaded components
			WaitForSingleObject (hQueueMutex, INFINITE);
			td->tex  = tex;
			td->ltex = mask;
			td->flag &= 0x3F; // mark as loaded
			nqueue--;
			queue_out = (queue_out+1) % MAXQUEUE;
			ReleaseMutex (hQueueMutex);
		}
	}

	LogAlw("TileBuffer::LoadTile thread terminated");
	return 0;
}


HRESULT TileBuffer::ReadDDSSurface (LPDIRECT3DDEVICE9 pDev, const char *fname, LONG_PTR ofs, LPDIRECT3DTEXTURE9* pTex, bool bManaged)
{
	_TRACE;
	char cpath[256];

	DDSURFACEDESC2       ddsd;
	DWORD                dwMagic;

	FILE *f = NULL;

	sprintf_s(cpath,256,"%s%s", OapiExtension::GetHightexDir(), fname);

	if (fopen_s(&f, cpath, "rb")) return -3;

	fseek(f, (long)ofs, SEEK_SET);

	// Read the magic number
	if (!fread(&dwMagic, sizeof(DWORD), 1, f))	return -2;

	if (dwMagic != MAKEFOURCC('D','D','S',' ')) return -4;

	// Read the surface description
	fread(&ddsd, sizeof(DDSURFACEDESC2), 1, f);

	D3DFORMAT Format;

	switch (ddsd.ddpfPixelFormat.dwFourCC) {

		case MAKEFOURCC ('D','X','T','1'):
			Format = D3DFMT_DXT1;
			break;

		case MAKEFOURCC ('D','X','T','3'):
			Format = D3DFMT_DXT3;
			break;

		case MAKEFOURCC ('D','X','T','5'):
			Format = D3DFMT_DXT5;
			break;

		default:
			LogErr("INVALID TEXTURE FORMAT in ReadDDSSurface()");
			return -5;
	}

	if (ddsd.dwHeight>4096 || ddsd.dwWidth>4096) LogErr("Attempting to load very large surface tile (%u,%u)", ddsd.dwWidth, ddsd.dwHeight);

	*pTex = NULL;

	D3DLOCKED_RECT rect;

	if (bManaged) {
		if (pDev->CreateTexture(ddsd.dwWidth, ddsd.dwHeight, 1, 0, Format, D3DPOOL_MANAGED, pTex, NULL)!=S_OK) {
			LogErr("Surface Tile Allocation Failed. w=%u, h=%u", ddsd.dwWidth, ddsd.dwHeight);
			return -10;
		}
		if ((*pTex)==NULL) return -8;
		if (*pTex) TileCatalog->Add(*pTex);
		if ((*pTex)->LockRect(0, &rect, NULL, 0)==S_OK) {
			if (ddsd.dwFlags & DDSD_LINEARSIZE) {
				fread(rect.pBits, ddsd.dwLinearSize, 1, f);
				(*pTex)->UnlockRect(0);
				fclose(f);
				return S_OK;
			}
		}
	}
	else {
		if (pDev->CreateTexture(ddsd.dwWidth, ddsd.dwHeight, 1, 0, Format, D3DPOOL_DEFAULT, pTex, NULL)!=S_OK) {
			LogErr("Surface Tile Allocation Failed. w=%u, h=%u", ddsd.dwWidth, ddsd.dwHeight);
			return -9;
		}
		LPDIRECT3DTEXTURE9 pSys = NULL;
		HR(pDev->CreateTexture(ddsd.dwWidth, ddsd.dwHeight, 1, 0, Format, D3DPOOL_SYSTEMMEM, &pSys, NULL));

		if (pSys==NULL || (*pTex)==NULL) return -8;
		if (*pTex) TileCatalog->Add(*pTex);
		if (pSys->LockRect(0, &rect, NULL, 0)==S_OK) {
			if (ddsd.dwFlags & DDSD_LINEARSIZE) {
				fread(rect.pBits, ddsd.dwLinearSize, 1, f);
				pSys->UnlockRect(0);
				HR(pDev->UpdateTexture(pSys,(*pTex)));
				pSys->Release();
				fclose(f);
				return S_OK;
			}
		}
	}

	fclose(f);
	return -7;
}







// =======================================================================

bool TileBuffer::bHoldThread = false;
int TileBuffer::nqueue = 0;
int TileBuffer::queue_in = 0;
int TileBuffer::queue_out = 0;
HANDLE TileBuffer::hQueueMutex = NULL;
HANDLE TileBuffer::hLoadThread = NULL;
HANDLE TileBuffer::hStopThread(CreateEvent(NULL, FALSE, FALSE, NULL));
struct TileBuffer::QUEUEDESC TileBuffer::loadqueue[MAXQUEUE] = {0};

