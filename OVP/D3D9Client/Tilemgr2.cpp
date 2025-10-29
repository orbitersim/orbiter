// ==============================================================
//   ORBITER VISUALISATION PROJECT (OVP)
//   Copyright (C) 2006-2016 Martin Schweiger
//   Dual licensed under GPL v3 and LGPL v3
// ==============================================================

// ==============================================================
// tilemanager2.cpp
// Rendering of planetary surfaces using texture tiles at
// variable resolutions (new version).
// ==============================================================

#include "Tilemgr2.h"
#include "D3D9Config.h"
#include "D3D9Catalog.h"
#include "Scene.h"
#include "OapiExtension.h"
#include "DirectXCollision.h"

#include <stack>
#include <io.h>
#include <filesystem>

// =======================================================================
// Externals
static TEXCRDRANGE2 fullrange = {0,1,0,1};

int SURF_MAX_PATCHLEVEL2 = 18; // move this somewhere else


bool FileExists(const char* path)
{
	bool exists;
	struct _finddata_t fd;
	intptr_t fh = _findfirst(path, &fd);
	if (exists = (fh != -1))
		_findclose(fh);
	return exists;
}


// =======================================================================
// Class Tile

Tile::Tile (TileManager2Base *_mgr, int _lvl, int _ilat, int _ilng)
: mgr(_mgr), lvl(_lvl), ilat(_ilat), ilng(_ilng),
  lngnbr_lvl(_lvl), latnbr_lvl(_lvl), dianbr_lvl(_lvl),
  texrange(fullrange), microrange(fullrange), overlayrange(fullrange), cnt(Centre()),
  mesh(NULL), tex(NULL), overlay(NULL),
  last_used(0.0),
  state(Invalid),
  edgeok(false), owntex (true), ownoverlay(false)
{
	double f = 1.0 / double(1<<lvl);
	double x = PI * (0.5 - double(ilat+1) * f);
	width = float(PI * cos(x) * f);
	height = float(PI * f);
	tgtscale = 1.0f;
	Extents(&bnd.minlat, &bnd.maxlat, &bnd.minlng, &bnd.maxlng);
	D3D9Stats.TilesAllocated++;
	mgr->TilesLoaded++;
	bMipmaps = false;
}

// -----------------------------------------------------------------------

Tile::~Tile ()
{
	D3D9Stats.TilesAllocated--;
	mgr->TilesLoaded--;
	state = Invalid;
	if (mesh) delete mesh;
}


// ------------------------------------------------------------------------
// Pre Load routine for surface tiles
// ------------------------------------------------------------------------

bool Tile::LoadTextureFile(const char *fullpath, LPDIRECT3DTEXTURE9 *pPre)
{
	auto y = filesystem::status(fullpath);
	if (filesystem::exists(y)) {
		DWORD Mips = 1, Filter = D3DX_FILTER_NONE;
		if (bMipmaps) Filter = D3DX_FILTER_BOX, Mips = 0;
		if (D3DXCreateTextureFromFileEx(mgr->Dev(), fullpath, 0, 0, Mips, 0, D3DFMT_FROM_FILE, D3DPOOL_SYSTEMMEM, D3DX_DEFAULT, Filter, 0, NULL, NULL, pPre) == S_OK) {
			return true;
		}
	}

	*pPre = NULL;
	return false;
}

bool Tile::LoadTextureFromMemory(void *data, DWORD ndata, LPDIRECT3DTEXTURE9 *pPre)
{
	DWORD Mips = 1, Filter = D3DX_FILTER_NONE;
	if (bMipmaps) Filter = D3DX_FILTER_BOX, Mips = 0;
	if (D3DXCreateTextureFromFileInMemoryEx(mgr->Dev(), data, ndata, 0, 0, Mips, 0, D3DFMT_FROM_FILE, D3DPOOL_SYSTEMMEM, D3DX_DEFAULT, Filter, 0, NULL, NULL, pPre) == S_OK) {
		return true;
	}

	*pPre = NULL;
	return false;
}


// ------------------------------------------------------------------------
// Create a texture from a pre-loaded data
// ------------------------------------------------------------------------

bool Tile::CreateTexture(LPDIRECT3DDEVICE9 pDev, LPDIRECT3DTEXTURE9 pPre, LPDIRECT3DTEXTURE9 *pTex)
{
	D3DSURFACE_DESC desc;
	if (pPre) {
		pPre->GetLevelDesc(0, &desc);
		*pTex = g_pTexmgr_tt->New(desc.Width, desc.Format);
		HR(pDev->UpdateTexture(pPre, (*pTex)));
		return true;
	}
	return false;
}

// -----------------------------------------------------------------------

bool Tile::PreDelete ()
{
	switch (state) {
	case Loading:
		return false;                // locked
	case InQueue:
		mgr->loader->Unqueue (this); // remove from load queue
		// fall through
	default:
		return true;
	}
}

// -----------------------------------------------------------------------

void Tile::Render()
{
	if (mgr->gc->IsControlPanelOpen())
		if (D3D9Stats.TilesRendered.count(mgr->GetScene()->GetRenderPass()))
			D3D9Stats.TilesRendered[mgr->GetScene()->GetRenderPass()]++;
}

// -----------------------------------------------------------------------

bool Tile::GetParentSubTexRange (TEXCRDRANGE2 *subrange)
{
	Tile *parent = getParent();
	if (!parent) return false; // haven't got a parent

	if (!(ilat&1)) { // left column
		subrange->tvmin = parent->texrange.tvmin;
		subrange->tvmax = (parent->texrange.tvmin+parent->texrange.tvmax)*0.5f;
	} else {         // right column
		subrange->tvmin = (parent->texrange.tvmin+parent->texrange.tvmax)*0.5f;
		subrange->tvmax = parent->texrange.tvmax;
	}
	if (!(ilng&1)) { // bottom row
		subrange->tumin = parent->texrange.tumin;
		subrange->tumax = (parent->texrange.tumin+parent->texrange.tumax)*0.5f;
	} else {         // top row
		subrange->tumin = (parent->texrange.tumin+parent->texrange.tumax)*0.5f;
		subrange->tumax = parent->texrange.tumax;
	}
	return true;
}

// -----------------------------------------------------------------------

bool Tile::GetParentOverlayRange(TEXCRDRANGE2 *subrange)
{
	Tile *parent = getParent();
	if (!parent) return false; // haven't got a parent

	overlay = parent->overlay;

	if (!(ilat & 1)) { // left column
		subrange->tvmin = parent->overlayrange.tvmin;
		subrange->tvmax = (parent->overlayrange.tvmin + parent->overlayrange.tvmax)*0.5f;
	}
	else {         // right column
		subrange->tvmin = (parent->overlayrange.tvmin + parent->overlayrange.tvmax)*0.5f;
		subrange->tvmax = parent->overlayrange.tvmax;
	}
	if (!(ilng & 1)) { // bottom row
		subrange->tumin = parent->overlayrange.tumin;
		subrange->tumax = (parent->overlayrange.tumin + parent->overlayrange.tumax)*0.5f;
	}
	else {         // top row
		subrange->tumin = (parent->overlayrange.tumin + parent->overlayrange.tumax)*0.5f;
		subrange->tumax = parent->overlayrange.tumax;
	}
	return true;
}

// -----------------------------------------------------------------------

bool Tile::GetParentMicroTexRange(TEXCRDRANGE2 *subrange)
{

	int dlev = imicrolvl - Level();


	if (dlev >= 0) {
		int f = (1 << dlev);
		subrange->tumax = float(f);
		subrange->tvmax = float(f);
		return true;
	}

	Tile *parent = getParent();
	if (!parent) return false; // haven't got a parent

	if (!(ilat & 1)) { // left column
		subrange->tvmin = parent->microrange.tvmin;
		subrange->tvmax = (parent->microrange.tvmin + parent->microrange.tvmax)*0.5f;
	}
	else {         // right column
		subrange->tvmin = (parent->microrange.tvmin + parent->microrange.tvmax)*0.5f;
		subrange->tvmax = parent->microrange.tvmax;
	}
	if (!(ilng & 1)) { // bottom row
		subrange->tumin = parent->microrange.tumin;
		subrange->tumax = (parent->microrange.tumin + parent->microrange.tumax)*0.5f;
	}
	else {         // top row
		subrange->tumin = (parent->microrange.tumin + parent->microrange.tumax)*0.5f;
		subrange->tumax = parent->microrange.tumax;
	}
	return true;
}

// -----------------------------------------------------------------------

FVECTOR4 Tile::GetTexRangeDX (const TEXCRDRANGE2 *subrange) const
{
	return FVECTOR4(subrange->tumin, subrange->tvmin, subrange->tumax - subrange->tumin, subrange->tvmax - subrange->tvmin);
}

// -----------------------------------------------------------------------
// Check if tile bounding box intersects the viewing frustum
// given the transformation matrix transform

bool Tile::InView (const MATRIX4 &transform)
{
	if (!lvl) return true; // no good check for this yet
	if (!mesh) return true; // DEBUG : TEMPORARY

	bool bx1, bx2, by1, by2, bz1;
	bx1 = bx2 = by1 = by2 = bz1 = false;
	int v;
	double hx, hy;
	for (v = 0; v < 8; v++) {
		VECTOR4 vt = mul (mesh->Box[v], transform);
		hx = vt.x/vt.w, hy = vt.y/vt.w;
		if (vt.z > 0.0) bz1 = true;
		if (vt.w < 0.0) hx = -hx, hy = -hy;
		if (hx > -1.0) bx1 = true;
		if (hx <  1.0) bx2 = true;
		if (hy > -1.0) by1 = true;
		if (hy <  1.0) by2 = true;
		if (bx1 && bx2 && by1 && by2 && bz1) return true;
	}
	return false;
}

// -----------------------------------------------------------------------

float Tile::GetBoundingSphereRad() const
{
	if (mesh) return mesh->bsRad;
	return 0.0f;
}

// -----------------------------------------------------------------------

FVECTOR3 Tile::GetBoundingSpherePos() const
{
	if (mesh) return mesh->bsCnt;
	return FVECTOR3(0,0,0);
}

// -----------------------------------------------------------------------

bool Tile::Pick(const FMATRIX4* pW, const FVECTOR3 *vDir, TILEPICK &result)
{
	if (!mesh) {
		LogErr("Tile::Pick() Failed: No Mesh Available");
		return false;
	}
	if (!mesh->idx || !mesh->vtx) {
		LogErr("Tile::Pick() Failed: No Geometry Available");
		return false;
	}

	FVECTOR3 bs = oapiTransformCoord(&mesh->bsCnt, pW);

	float dst = dotp(bs, *vDir);
	float len2 = dotp(bs, bs);

	if (dst < -mesh->bsRad) return false;
	if (sqrt(len2 - dst*dst) > mesh->bsRad) return false;

	FVECTOR3 _a, _b, _c, cp;
	FMATRIX4 mWI;
	float det;

	WORD *pIdc = mesh->idx;
	VERTEX_2TEX *vtx = mesh->vtx;

	oapiMatrixInverse(&mWI, &det, pW);

	XMVECTOR pos = oapiTransformCoord(&(FVECTOR3(0, 0, 0)), &mWI).XM();
	XMVECTOR dir = oapiTransformNormal(vDir, &mWI).XM();

	int idx = -1;

	for (DWORD i = 0; i<mesh->nf; i++)
	{
		WORD a = pIdc[i * 3 + 0];
		WORD b = pIdc[i * 3 + 1];
		WORD c = pIdc[i * 3 + 2];

		_a = FVECTOR3(vtx[a].x, vtx[a].y, vtx[a].z);
		_b = FVECTOR3(vtx[b].x, vtx[b].y, vtx[b].z);
		_c = FVECTOR3(vtx[c].x, vtx[c].y, vtx[c].z);

		float dst;

		cp = crossp((_c - _b), (_a - _b));

		if (dotp(cp, dir)<0) {
			if (DirectX::TriangleTests::Intersects(pos, dir, _c.XM(), _b.XM(), _a.XM(), dst)) {			
				if (dst > 0.1f) {
					if (dst < result.d) {
						idx = i;
						result.d = dst;						
						result.i = i;
						result.pTile = this;
					}
				}
			}
		}
	}

	if (idx >= 0) {

		int   i = result.i;

		WORD a = pIdc[i * 3 + 0];
		WORD b = pIdc[i * 3 + 1];
		WORD c = pIdc[i * 3 + 2];

		_a = FVECTOR3(vtx[a].x, vtx[a].y, vtx[a].z);
		_b = FVECTOR3(vtx[b].x, vtx[b].y, vtx[b].z);
		_c = FVECTOR3(vtx[c].x, vtx[c].y, vtx[c].z);

		cp = crossp((_c - _b), (_a - _b));
		cp = oapiTransformNormal(&cp, pW);

		result._n = unit(cp);
		result._p = *vDir * result.d;

		return true;
	}

	return false;
}



// -----------------------------------------------------------------------
// returns the direction of the tile centre from the planet centre in local
// planet coordinates

VECTOR3 Tile::Centre () const
{
	int nlat = 1 << lvl;
	int nlng = 2 << lvl;
	double cntlat = PI05 - PI * ((double)ilat+0.5)/(double)nlat, slat = sin(cntlat), clat = cos(cntlat);
	double cntlng = PI2  * ((double)ilng+0.5)/(double)nlng + PI, slng = sin(cntlng), clng = cos(cntlng);
	return _V(clat*clng,  slat,  clat*slng);
}


// -----------------------------------------------------------------------

void Tile::Extents (double *_latmin, double *_latmax, double *_lngmin, double *_lngmax) const
{
	int nlat = 1 << lvl;
	int nlng = 2 << lvl;
	*_latmin = PI * (0.5 - (double)(ilat+1)/(double)nlat);
	*_latmax = PI * (0.5 - (double)ilat/(double)nlat);
	*_lngmin = PI2 * (double)(ilng-nlng/2)/(double)nlng;
	*_lngmax = PI2 * (double)(ilng-nlng/2+1)/(double)nlng;
}

// -----------------------------------------------------------------------


bool Tile::IsPointInTile(double lng, double lat)
{
	if (lat > bnd.maxlat || lat < bnd.minlat) return false;
	if (lng > bnd.maxlng || lng < bnd.minlng) return false;
	return true;
}

// -----------------------------------------------------------------------

VBMESH *Tile::CreateMesh_quadpatch (int grdlat, int grdlng, float *elev, double elev_scale, double globelev,
	const TEXCRDRANGE2 *range, bool shift_origin, VECTOR3 *shift, double bb_excess)
{
//	const float TEX2_MULTIPLIER = 1.0f; // was: 4.0f was: 16.0f
	const float c1 = 1.0f, c2 = 0.0f;   // -1.0f/512.0f; // assumes 256x256 texture patches
	int i, j, n, nofs0, nofs1;
	int nlng = (lvl >= 0 ? 2 << lvl : 1);
	int nlat = (lvl >= 0 ? 1 << lvl : 1);
	bool north = (ilat < nlat/2);

	double lat, slat, clat, lng, slng, clng, eradius, dx, dy;
	double minlat = PI * (double)(nlat/2-ilat-1)/(double)nlat;
	double maxlat = PI * (double)(nlat/2-ilat)/(double)nlat;
	double minlng = 0;
	double maxlng = PI2/(double)nlng;
	double radius = mgr->obj_size;
	VECTOR3 pos, tpos, nml;
	if (!range) range = &fullrange;
	//float turange = range->tumax-range->tumin;
	//float tvrange = range->tvmax-range->tvmin;

	int nvtx = (grdlat+1)*(grdlng+1);         // patch mesh node grid
	int nvtxbuf = nvtx + grdlat+1 + grdlng+1; // add buffer for storage of edges (for elevation matching)
	VERTEX_2TEX *vtx = g_pMemgr_vtx->New(nvtxbuf);

	// create transformation for bounding box
	// we define the local coordinates for the patch so that the x-axis points
	// from (minlng,minlat) corner to (maxlng,minlat) corner (origin is halfway between)
	// y-axis points from local origin to middle between (minlng,maxlat) and (maxlng,maxlat)
	// bounding box is created in this system and then transformed back to planet coords.
	double clat0 = cos(minlat), slat0 = sin(minlat);
	double clng0 = cos(minlng), slng0 = sin(minlng);
	double clat1 = cos(maxlat), slat1 = sin(maxlat);
	double clng1 = cos(maxlng), slng1 = sin(maxlng);
	VECTOR3 ex = {clat0*clng1 - clat0*clng0, 0, clat0*slng1 - clat0*slng0}; normalise (ex);
	VECTOR3 ey = {0.5*(clng0+clng1)*(clat1-clat0), slat1-slat0, 0.5*(slng0+slng1)*(clat1-clat0)}; normalise (ey);
	VECTOR3 ez = crossp (ey, ex);
	MATRIX3 R = {ex.x, ex.y, ex.z,  ey.x, ey.y, ey.z,  ez.x, ez.y, ez.z};
	VECTOR3 pref = {radius*clat0*0.5*(clng1+clng0), radius*slat0, radius*clat0*0.5*(slng1+slng0)}; // origin
	VECTOR3 tpmin, tpmax;

	float e = 0.0f;
	
	// patch translation vector
	if (shift_origin) {
		dx = (north ? clat0:clat1)*radius;
		dy = (north ? slat0:slat1)*radius;
	} else {
		dx = dy = 0.0;
	}
	if (shift) {
		shift->x = dx;
		shift->y = dy;
		shift->z = 0.0;
	}

	// create the vertices
	for (i = n = 0; i <= grdlat; i++) {
		lat = minlat + (maxlat-minlat) * (double)i/(double)grdlat;
		slat = sin(lat), clat = cos(lat);
		for (j = 0; j <= grdlng; j++) {
			lng = minlng + (maxlng-minlng) * (double)j/(double)grdlng;
			slng = sin(lng), clng = cos(lng);

			eradius = radius + globelev; // radius including node elevation
			if (elev) e = elev[(i+1)*TILE_ELEVSTRIDE + j+1] * float(elev_scale);
			eradius += double(e);

			nml = _V(clat*clng, slat, clat*slng);
			pos = nml*eradius;
			tpos = mul (R, pos-pref);
			if (!n) {
				tpmin = tpos;
				tpmax = tpos;
			} else {
				if      (tpos.x < tpmin.x) tpmin.x = tpos.x;
			    else if (tpos.x > tpmax.x) tpmax.x = tpos.x;
				if      (tpos.y < tpmin.y) tpmin.y = tpos.y;
				else if (tpos.y > tpmax.y) tpmax.y = tpos.y;
				if      (tpos.z < tpmin.z) tpmin.z = tpos.z;
				else if (tpos.z > tpmax.z) tpmax.z = tpos.z;
			}
			vtx[n].x = float(pos.x - dx); vtx[n].nx = float(nml.x);
			vtx[n].y = float(pos.y - dy); vtx[n].ny = float(nml.y);
			vtx[n].z = float(pos.z);      vtx[n].nz = float(nml.z);
			vtx[n].e = float(e);

			vtx[n].tu0 = float((c1*j)/grdlng+c2); // overlap to avoid seams
			vtx[n].tv0 = float(grdlat-i)/float(grdlat);
			//vtx[n].tu1 = vtx[n].tu0 * TEX2_MULTIPLIER;
			//vtx[n].tv1 = vtx[n].tv0 * TEX2_MULTIPLIER;
			// map texture coordinates to subrange
			//vtx[n].tu0 = vtx[n].tu0*turange + range->tumin;
			//vtx[n].tv0 = vtx[n].tv0*tvrange + range->tvmin;
			n++;
		}
	}

	// create the face indices
	int nidx = 2*grdlat*grdlng * 3;
	WORD *idx = g_pMemgr_w->New(nidx);

	if (elev) { // do adaptive orientation of cell diagonal
		float *elev1, *elev2, *elev1n, *elev2n, err1, err2;
		for (i = n = nofs0 = 0; i < grdlat; i++) {
			nofs1 = nofs0+grdlng+1;
			for (j = 0; j < grdlng; j++) {
				elev1  = elev + TILE_ELEVSTRIDE+2+j+TILE_ELEVSTRIDE*i;
				elev2  = elev1 + TILE_ELEVSTRIDE-1;
				elev1n = elev1 - TILE_ELEVSTRIDE+1;
				elev2n = elev2 + TILE_ELEVSTRIDE-1;
				err1 = fabs(*elev1 * 2 - *elev2 - *elev1n) + fabs(*elev2 * 2 - *elev1 - *elev2n);
				elev1  = elev + TILE_ELEVSTRIDE+1+j+TILE_ELEVSTRIDE*i;
				elev2  = elev1 + TILE_ELEVSTRIDE+1;
				elev1n = elev1 - TILE_ELEVSTRIDE-1;
				elev2n = elev2 + TILE_ELEVSTRIDE+1;
				err2 = fabs(*elev1 * 2 - *elev2 - *elev1n) + fabs(*elev2 * 2 - *elev1 - *elev2n);
				if (err1 < err2) {
					idx[n++] = nofs0+j;
					idx[n++] = nofs1+j;
					idx[n++] = nofs0+j+1;
					idx[n++] = nofs0+j+1;
					idx[n++] = nofs1+j;
					idx[n++] = nofs1+j+1;
				} else {
					idx[n++] = nofs0+j;
					idx[n++] = nofs1+j+1;
					idx[n++] = nofs0+j+1;
					idx[n++] = nofs1+j+1;
					idx[n++] = nofs0+j;
					idx[n++] = nofs1+j;
				}
			}
			nofs0 = nofs1;
		}
	} else { // no elevation => no adaptation of cell diagonals necessary
		for (i = n = nofs0 = 0; i < grdlat; i++) {
			nofs1 = nofs0+grdlng+1;
			for (j = 0; j < grdlng; j++) {
				idx[n++] = nofs0+j;
				idx[n++] = nofs1+j;
				idx[n++] = nofs0+j+1;
				idx[n++] = nofs1+j+1;
				idx[n++] = nofs0+j+1;
				idx[n++] = nofs1+j;
			}
			nofs0 = nofs1;
		}
	}

	// regenerate normals for terrain
	if (elev) {
//		const double shade_exaggerate = 1.0; // 1 = normal, <1 = more dramatic landscape shadows
		double dy, dz, dydz, /*nz_x, ny_x,*/ nx1, ny1, nz1;
		int en;
		dy = radius * PI/(nlat*grdlat);  // y-distance between vertices
//		ny_x = shade_exaggerate*dy;
		for (i = n = 0; i <= grdlat; i++) {
			lat = minlat + (maxlat-minlat) * (double)i/(double)grdlat;
			slat = sin(lat), clat = cos(lat);
			dz = radius * PI2*cos(lat) / (nlng*grdlng); // z-distance between vertices on unit sphere
			dydz = dy*dz;
//			nz_x = shade_exaggerate*dz;
			for (j = 0; j <= grdlng; j++) {
				lng = minlng + (maxlng-minlng) * (double)j/(double)grdlng;
				slng = sin(lng), clng = cos(lng);
				en = (i+1)*TILE_ELEVSTRIDE + (j+1);

				// This version avoids the normalisation of the 4 intermediate face normals
				// It's faster and doesn't seem to make much difference
				VECTOR3 nml = { 2.0*dydz, dz*elev_scale*(elev[en - TILE_ELEVSTRIDE] - elev[en + TILE_ELEVSTRIDE]), dy*elev_scale*(elev[en - 1] - elev[en + 1]) };
				normalise (nml);
				// rotate into place
				nx1 = nml.x*clat - nml.y*slat;
				ny1 = nml.x*slat + nml.y*clat;
				nz1 = nml.z;
				vtx[n].nx = (float)(nx1*clng - nz1*slng);
				vtx[n].ny = (float)(ny1);
				vtx[n].nz = (float)(nx1*slng + nz1*clng);
				n++;
			}
		}
	}

	// store the adaptable edges in the separate vertex area
	for (i = 0, n = nvtx; i <= grdlat; i++) // store left or right edge
		vtx[n++] = vtx[i*(grdlng+1) + ((ilng&1) ? grdlng:0)];
	for (i = 0; i <= grdlng; i++) // store top or bottom edge
		vtx[n++] = vtx[i + ((ilat&1) ? 0 : (grdlng+1)*grdlat)];

	// create the mesh
	VBMESH *mesh = new VBMESH(mgr);
	mesh->vtx = vtx;
	mesh->nv  = nvtx;
	mesh->idx = idx;
	mesh->nf  = nidx/3;

	// set bounding box for visibility calculations
	if (bb_excess) {
		double bb_dx = tpmax.x-tpmin.x;
		double bb_dy = tpmax.y-tpmin.y;
		double bb_dz = tpmax.z-tpmin.z;
		double scale = sqrt(bb_dx*bb_dx + bb_dy*bb_dy + bb_dz*bb_dz)/(2.0*sqrt(3.0))*bb_excess;
		tpmin.x -= scale;
		tpmax.x += scale;
		tpmin.y -= scale;
		tpmax.y += scale;
		tpmin.z -= scale;
		tpmax.z += scale;
	}
	pref.x -= dx;
	pref.y -= dy;

	mesh->Box[0] = _V(tmul (R, _V(tpmin.x, tpmin.y, tpmin.z)) + pref);
	mesh->Box[1] = _V(tmul (R, _V(tpmax.x, tpmin.y, tpmin.z)) + pref);
	mesh->Box[2] = _V(tmul (R, _V(tpmin.x, tpmax.y, tpmin.z)) + pref);
	mesh->Box[3] = _V(tmul (R, _V(tpmax.x, tpmax.y, tpmin.z)) + pref);
	mesh->Box[4] = _V(tmul (R, _V(tpmin.x, tpmin.y, tpmax.z)) + pref);
	mesh->Box[5] = _V(tmul (R, _V(tpmax.x, tpmin.y, tpmax.z)) + pref);
	mesh->Box[6] = _V(tmul (R, _V(tpmin.x, tpmax.y, tpmax.z)) + pref);
	mesh->Box[7] = _V(tmul (R, _V(tpmax.x, tpmax.y, tpmax.z)) + pref);

	mesh->MapVertices(TileManager2Base::pDev);

	return mesh;
}

// -----------------------------------------------------------------------

VBMESH *Tile::CreateMesh_hemisphere (int grd, float *elev, double globelev)
{
	const int texres = 512;
	double radius = mgr->obj_size + globelev;
	double eradius, slat, clat, slng, clng;
	VECTOR3 pos, nml;

	// Allocate memory for the vertices and indices
	int         nVtx = (grd-1)*(grd+1)+2;
	int         nIdx = 6*(grd*(grd-2)+grd);
	VERTEX_2TEX *Vtx = g_pMemgr_vtx->New(nVtx);
	WORD*       Idx = g_pMemgr_w->New(nIdx);

	// Counters
    WORD x, y, nvtx = 0, nidx = 0;
	VERTEX_2TEX *vtx = Vtx;
	WORD *idx = Idx;

	// Angle deltas for constructing the sphere's vertices
    double fDAng   = PI / grd;
    double lng, lat = fDAng;
	DWORD x1 = grd;
	DWORD x2 = x1+1;
	FLOAT du = 0.5f/(FLOAT)texres;
	FLOAT a  = (1.0f-2.0f*du)/(FLOAT)x1;

    // Make the middle of the sphere
    for (y = 1; y < grd; y++) {
		slat = sin(lat), clat = cos(lat);
		FLOAT tv = (float)(lat/PI);

        for (x = 0; x < x2; x++) {
            lng = x*fDAng - PI;  // subtract Pi to wrap at +-180°
			if (ilng) lng += PI;
			slng = sin(lng), clng = cos(lng);
			eradius = radius + globelev; // radius including node elevation
			if (elev) eradius += (double)elev[(grd+1-y)*TILE_ELEVSTRIDE + x+1];
			nml = _V(slat*clng, clat, slat*slng);
			pos = nml*eradius;
			vtx->x = float(pos.x);  vtx->nx = float(nml.x);
			vtx->y = float(pos.y);  vtx->ny = float(nml.y);
			vtx->z = float(pos.z);  vtx->nz = float(nml.z);
			FLOAT tu = a*(FLOAT)x + du;
			vtx->tu0 = tu; // vtx->tu1 = tu;
			vtx->tv0 = tv; // vtx->tv1 = tv;
			vtx++;
			nvtx++;
        }
        lat += fDAng;
    }

    for (y = 0; y < grd-2; y++) {
        for (x = 0; x < x1; x++) {
            *idx++ = (WORD)( (y+0)*x2 + (x+0) );
            *idx++ = (WORD)( (y+0)*x2 + (x+1) );
            *idx++ = (WORD)( (y+1)*x2 + (x+0) );
            *idx++ = (WORD)( (y+0)*x2 + (x+1) );
            *idx++ = (WORD)( (y+1)*x2 + (x+1) );
            *idx++ = (WORD)( (y+1)*x2 + (x+0) );
			nidx += 6;
        }
    }
    // Make top and bottom
	WORD wNorthVtx = nvtx;
	eradius = radius + globelev;
	if (elev) {
		double mn = 0.0;
		for (x = 0; x < x2; x++) mn += (double)elev[TILE_ELEVSTRIDE*(grd+1) + x+1];
		eradius += mn/x2;
	}
	nml = _V(0,1,0);
	pos = nml*eradius;
	vtx->x = float(pos.x);  vtx->nx = float(nml.x);
	vtx->y = float(pos.y);  vtx->ny = float(nml.y);
	vtx->z = float(pos.z);  vtx->nz = float(nml.z);
	vtx->tu0 = 0.5f; // vtx->tu1 = 0.5f;
	vtx->tv0 = 0.0f; // vtx->tv1 = 0.0f;
	vtx++;
    nvtx++;
	WORD wSouthVtx = nvtx;


	eradius = radius + globelev;
	if (elev) {
		double mn = 0.0;
		for (x = 0; x < x2; x++) mn += (double)elev[TILE_ELEVSTRIDE + x+1];
		eradius += mn/x2;
	}
	nml = _V(0,-1,0);
	pos = nml*eradius;
	vtx->x = float(pos.x);  vtx->nx = float(nml.x);
	vtx->y = float(pos.y);  vtx->ny = float(nml.y);
	vtx->z = float(pos.z);  vtx->nz = float(nml.z);
	vtx->tu0 = 0.5f; // vtx->tu1 = 0.5f;
	vtx->tv0 = 1.0f; // vtx->tv1 = 1.0f;
	vtx++;
    nvtx++;

    for (x = 0; x < x1; x++) {
		WORD p1 = wSouthVtx;
		WORD p2 = (WORD)( (grd-2)*x2 + (x+0) );
		WORD p3 = (WORD)( (grd-2)*x2 + (x+1) );

        *idx++ = p1;
        *idx++ = p2;
        *idx++ = p3;
		nidx += 3;
    }

    for (x = 0; x < x1; x++) {
		WORD p1 = wNorthVtx;
		WORD p2 = (WORD)( (0)*x2 + (x+0) );
		WORD p3 = (WORD)( (0)*x2 + (x+1) );

        *idx++ = p1;
        *idx++ = p3;
        *idx++ = p2;
		nidx += 3;
    }

	// regenerate normals for terrain
	if (elev) {
		double dy, dz, dydz, nx1, ny1, nz1;
		int en;
		dy = radius * PI/grd;  // y-distance between vertices
		vtx = Vtx;
		for (y = 1; y < grd; y++) {
			lat = PI05-y*fDAng;
			slat = sin(lat), clat = cos(lat);
			dz = radius * PI*cos(lat) / grd; // z-distance between vertices on unit sphere
			dydz = dy*dz;
			for (x = 0; x < x2; x++) {
				lng = x*fDAng;
				if (!ilng) lng -= PI;
				slng = sin(lng), clng = cos(lng);
				en = (grd+1-y)*TILE_ELEVSTRIDE + x+1;
				VECTOR3 nml = {2.0*dydz, dz*(elev[en-TILE_ELEVSTRIDE]-elev[en+TILE_ELEVSTRIDE]), dy*(elev[en-1]-elev[en+1])};
				normalise(nml);
				// rotate into place
				nx1 = nml.x*clat - nml.y*slat;
				ny1 = nml.x*slat + nml.y*clat;
				nz1 = nml.z;
				vtx->nx = (float)(nx1*clng - nz1*slng);
				vtx->ny = (float)(ny1);
				vtx->nz = (float)(nx1*slng + nz1*clng);
				vtx++;
			}
		}
	}

	// create the mesh
	VBMESH *mesh = new VBMESH(mgr);
	mesh->vtx = Vtx;
	mesh->nv  = nVtx;
	mesh->idx = Idx;
	mesh->nf  = nIdx/3;
	mesh->MapVertices (TileManager2Base::pDev);
	return mesh;
}

// =======================================================================
// =======================================================================

int TileLoader::nqueue = 0;
int TileLoader::queue_in = 0;
int TileLoader::queue_out = 0;
HANDLE TileLoader::hLoadMutex = 0;
struct TileLoader::QUEUEDESC TileLoader::queue[MAXQUEUE2] = {0};

TileLoader::TileLoader (const oapi::D3D9Client *gclient)
	: gc(gclient)
	, hStopThread(CreateEvent(NULL, FALSE, FALSE, NULL))
	, load_frequency(Config->PlanetLoadFrequency)
{
	DWORD id;

	// Initialize statics
	nqueue = queue_in = queue_out = 0;
	hLoadMutex = CreateMutex (0, FALSE, NULL);
	hLoadThread = CreateThread (NULL, 32768, Load_ThreadProc, this, 0, &id);
}

// -----------------------------------------------------------------------

TileLoader::~TileLoader ()
{
	if (hLoadThread) LogErr("TileLoader() Not Yet ShutDown()");
	TerminateLoadThread();
	CloseHandle (hLoadMutex);
	hLoadMutex = NULL;
}

// -----------------------------------------------------------------------

bool TileLoader::ShutDown()
{
	if (hLoadThread) {
		TerminateLoadThread();
		return true;
	}
	return false;
}

// -----------------------------------------------------------------------

void TileLoader::TerminateLoadThread()
{
	if (hLoadThread) {
		// Signal thread to stop and wait for it to happen
		SetEvent(hStopThread);
		WaitForSingleObject(hLoadThread, INFINITE); //4000);
		// Clean up for next run
		ResetEvent(hStopThread);
		CloseHandle(hLoadThread);
		hLoadThread = NULL;
	}
}

// -----------------------------------------------------------------------

bool TileLoader::LoadTileAsync (Tile *tile)
{
	// Note: the queue should probably be sorted according to tile level

	int i, j;

	// Check if request is already present
	for (i = 0; i < nqueue; i++) {
		j = (i+queue_out) % MAXQUEUE2;
		if (queue[j].tile == tile)
			return false;
	}

	if (nqueue == MAXQUEUE2) { // queue full
		int maxlvl, maxlvl_idx = 0;
		for (i = 1, maxlvl = queue[0].tile->lvl; i < nqueue; i++)
			if (queue[i].tile->lvl > maxlvl)
				maxlvl = queue[i].tile->lvl, maxlvl_idx = i;
		if (maxlvl > tile->lvl) { // replace the highest level queued tile (load from lowest to highest)
			queue[maxlvl_idx].tile->state = Tile::Invalid;
			queue[maxlvl_idx].tile = tile;
			tile->state = Tile::InQueue;
		} else {
			tile->state = Tile::Invalid;
		}
		return false;
	}

	// add tile to load queue
	QUEUEDESC *qd = queue+queue_in;
	qd->tile = tile;
	tile->state = Tile::InQueue;
	nqueue++;
	queue_in = (queue_in+1) % MAXQUEUE2;
	return true;
}

// -----------------------------------------------------------------------

void TileLoader::Unqueue (TileManager2Base *mgr)
{
	bool locked = false; // whether a mutex semaphore was taken

	bool found;
	do {
		int i, j;
		found = false;
		for (i = 0; i < nqueue; ++i) {
			j = (queue_out+i) % MAXQUEUE2;
			if (queue[j].tile->mgr == mgr) {
				if (!locked) {
					WaitForMutex();
					locked = true;
				}
				if (i) {
					queue[j].tile = queue[queue_out].tile;
				}
				queue_out = (queue_out+1) % MAXQUEUE2;
				nqueue--;
				found = true;
				break;
			}
		}
	} while (found);

	if (locked) {
		ReleaseMutex(); // ...if mutex was taken
	}
}

// -----------------------------------------------------------------------

bool TileLoader::Unqueue (Tile *tile)
{
	if (tile->state != Tile::InQueue) return false;

	int i, j;
	for (i = 0; i < nqueue; i++) {
		j = (queue_out+i) % MAXQUEUE2;
		if (queue[j].tile == tile) {
			WaitForMutex ();
			if (i) {
				queue[j].tile = queue[queue_out].tile;
			}
			queue_out = (queue_out+1) % MAXQUEUE2;
			nqueue--;
			ReleaseMutex ();
			return true;
		}
	}
	return false;
}

// -----------------------------------------------------------------------
#ifdef UNDEF
DWORD WINAPI TileLoader::Load_ThreadProc (void *data)
{
	TileLoader *loader = (TileLoader*)data;
	DWORD idle = 1000/loader->load_frequency;
	Tile *tile;
	bool load;

	while (WAIT_OBJECT_0 != WaitForSingleObject(loader->hStopThread, idle)) {

		WaitForMutex();

		if (load = (nqueue > 0)) {
			tile = queue[queue_out].tile;
			_ASSERT(TILE_STATE_OK(tile));			// THIS IS TRIGGERED OFTEN
			if (tile->state == Tile::InQueue) {
				tile->state = Tile::Loading; // lock tile and its ancestor tree
			} else {
				load = false;
			}
			queue_out = (queue_out+1) % MAXQUEUE2; // remove from queue
			nqueue--;
		}

		ReleaseMutex();

		if (load) {
			tile->PreLoad(); // Preload data from harddrive to system memory without a Mutex

			WaitForMutex();
			tile->Load(); // Create the actual tile texture from a pre-loaded data
			tile->state = Tile::Inactive; // unlock tile
			ReleaseMutex();

		}
		else {
			Sleep(1);
		}
	}
	return 0;
}
#endif

// -----------------------------------------------------------------------

DWORD WINAPI TileLoader::Load_ThreadProc (void *data)
{
	const int tile_packet_size = 8; // max number of tiles to process from queue
	TileLoader *loader = (TileLoader*)data;
	DWORD idle = 1000/loader->load_frequency;
	Tile *tile[tile_packet_size];
	int nload, i;

	LogAlw("TileLoader::Load thread started");

	bool bFirstRun = true;
	while (bFirstRun || WAIT_OBJECT_0 != WaitForSingleObject(loader->hStopThread, idle))
	{
		bFirstRun = false;

		WaitForMutex ();
		for (nload = 0; nqueue > 0 && nload < tile_packet_size; nload++) {
			tile[nload] = queue[queue_out].tile;
			tile[nload]->state = Tile::Loading; // lock tile and its ancestor tree
			queue_out = (queue_out+1) % MAXQUEUE2; // remove from queue
			nqueue--;
		}
		ReleaseMutex ();

		if (nload) {
			
			for (i = 0; i < nload; i++) {
				tile[i]->PreLoad(); // load/create the tile
				tile[i]->Load();
			}

			WaitForMutex();
			for (i = 0; i < nload; i++) {	
				tile[i]->state = Tile::Inactive; // unlock tile
			}
			ReleaseMutex ();
		} else {
			Sleep (idle);
		}
	}

	LogAlw("TileLoader::Load thread terminated");
	return 0;
}

// =======================================================================
// =======================================================================

TileManager2Base::ConfigPrm TileManager2Base::cprm = {
	2,                  // elevInterpol
	false,				// bSpecular
	false,				// bLights
	false,              // bCloudShadow
	0.5                 // lightfac
};
oapi::D3D9Client* TileManager2Base::gc = NULL;
LPDIRECT3DDEVICE9 TileManager2Base::pDev = NULL;
TileLoader *TileManager2Base::loader = NULL;
double TileManager2Base::resolutionBias = 4.0;
double TileManager2Base::resolutionScale = 1.0;
bool TileManager2Base::bTileLoadThread = true;
HFONT TileManager2Base::hFont = NULL;
LPDIRECT3DTEXTURE9 TileManager2Base::hOcean = NULL;
LPDIRECT3DTEXTURE9 TileManager2Base::hCloudMicro = NULL;
LPDIRECT3DTEXTURE9 TileManager2Base::hCloudMicroNorm = NULL;

// -----------------------------------------------------------------------

TileManager2Base::TileManager2Base (vPlanet *vplanet, int _maxres, int _gridres)
: vp(vplanet), gridRes(_gridres), ElevMode(eElevMode::DontCare), ElevModeLvl(0)
{
	// set persistent parameters
	TilesLoaded = 0;
	prm.maxlvl = max (0, _maxres-4);
	obj = vp->Object();
	obj_size = oapiGetSize (obj);
	oapiGetObjectName (obj, cbody_name, 256);
	emgr = oapiElevationManager(obj);
	elevRes = *(double*)oapiGetObjectParam (obj, OBJPRM_PLANET_ELEVRESOLUTION);
	LogClr("Teal", "Planet ElevRes %s = %g", vplanet->GetName(), elevRes);

	char path[1024];
	gc->PlanetTexturePath(cbody_name, path);
	m_dataRootDir = path;
}

// -----------------------------------------------------------------------

TileManager2Base::~TileManager2Base ()
{
	LogAlw("Deleting TileManagerBase %s ...", _PTR(this));
	if (loader) loader->Unqueue(this);
}

// -----------------------------------------------------------------------

void TileManager2Base::GlobalInit (class oapi::D3D9Client *gclient)
{
	gc = gclient;
	pDev = gc->GetDevice();

	resolutionBias = 4.0 + Config->LODBias;
	DWORD w, h;
	gc->clbkGetViewportSize (&w, &h);
	resolutionScale = 1400.0 / (double)h;
	cprm.bSpecular = *(bool*)gclient->GetConfigParam (CFGPRM_SURFACEREFLECT);
	cprm.bLights = *(bool*)gclient->GetConfigParam (CFGPRM_SURFACELIGHTS);
	cprm.bCloudShadow = *(bool*)gclient->GetConfigParam (CFGPRM_CLOUDSHADOWS);
	cprm.lightfac = *(double*)gclient->GetConfigParam (CFGPRM_SURFACELIGHTBRT);
	cprm.elevMode = *(int*)gclient->GetConfigParam (CFGPRM_ELEVATIONMODE);
	cprm.tileLoadFlags = Config->PlanetTileLoadFlags;
	bTileLoadThread = *(bool*)gclient->GetConfigParam(CFGPRM_TILELOADTHREAD);

	loader = new TileLoader (gc);

	hFont  = CreateFont(42, 0, 0, 0, 600, false, false, 0, 0, 0, 2, CLEARTYPE_QUALITY, 49, "Arial");

	char name[MAX_PATH];

	if (gc->TexturePath("D3D9Ocean.dds", name)) D3DXCreateTextureFromFileA(pDev, name, &hOcean);
	if (gc->TexturePath("cloud1.dds", name)) D3DXCreateTextureFromFileA(pDev, name, &hCloudMicro);
	if (gc->TexturePath("cloud1_norm.dds", name)) D3DXCreateTextureFromFileA(pDev, name, &hCloudMicroNorm);
}

// -----------------------------------------------------------------------

bool TileManager2Base::ShutDown()
{
	return loader->ShutDown();
}

// -----------------------------------------------------------------------

void TileManager2Base::GlobalExit ()
{
	DeleteObject(hFont); hFont = NULL;
	SAFE_RELEASE(hOcean);
	SAFE_RELEASE(hCloudMicro);
	SAFE_RELEASE(hCloudMicroNorm);
	delete loader;
}

// -----------------------------------------------------------------------

void TileManager2Base::SetRenderPrm (MATRIX4 &dwmat, double prerot, bool use_zbuf, const vPlanet::RenderPrm &rprm)
{
	const double minalt = max(0.002,rprm.horizon_excess);
	VECTOR3 obj_pos, cam_pos;

	prm.rprm = &rprm;

	// set up the parameter structure
	oapiGetRotationMatrix(obj, &prm.grot); // planet's rotation matrix
	oapiGetGlobalPos (obj, &obj_pos);      // planet position
	cam_pos = GetScene()->GetCameraGPos(); // camera position

	if (prerot) {
		double srot = sin(prerot), crot = cos(prerot);
		MATRIX4 RM4 = { crot,0,-srot,0,  0,1,0,0,  srot,0,crot,0,  0,0,0,1 };
		prm.dwmat = mul(RM4,dwmat);
		prm.grot = mul (prm.grot, _M(crot,0,srot, 0,1,0, -srot,0,crot));
	} else {
		prm.dwmat = dwmat;
	}
	prm.dwmat_tmp = prm.dwmat;
	prm.cpos = obj_pos-cam_pos;
	prm.cdir = tmul (prm.grot, -prm.cpos); // camera's direction in planet frame
	double cdist = length(prm.cdir);
	prm.cdist = cdist / obj_size;          // camera's distance in units of planet radius
	normalise (prm.cdir);
	prm.sdir = tmul (prm.grot, -obj_pos);  // sun's direction in planet frame
	normalise (prm.sdir);
	// Add 5km threshold to allow slight camera movement with out causing surface tiles to unload
	prm.viewap = acos (1.0/(max ((cdist+5e3) / obj_size, 1.0+minalt)));
	prm.scale = 1.0;
}

// -----------------------------------------------------------------------

MATRIX4 TileManager2Base::WorldMatrix(Tile *tile)
{
	int lvl = tile->lvl;
	int ilng = tile->ilng;
	int ilat = tile->ilat;
	int nlng = 2 << lvl;
	int nlat = 1 << lvl;
	return WorldMatrix(ilng, nlng, ilat, nlat);
}

// -----------------------------------------------------------------------

MATRIX4 TileManager2Base::WorldMatrix (int ilng, int nlng, int ilat, int nlat)
{
	if (nlat < 2) {  // render full sphere
		return prm.dwmat;
	}

	double lat, lng = PI2 * (double)ilng/(double)nlng + PI; // add pi so texture wraps at +-180°
	double slng = sin(lng), clng = cos(lng);
	MATRIX4 lrot = {clng,0,slng,0,  0,1.0,0,0,  -slng,0,clng,0,  0,0,0,1.0};

	if (nlat <= 8) {  // rotate patch into place
		return mul(lrot,prm.dwmat);
	} else {          // shift, scale and rotate
		bool north = (ilat < nlat/2);
		if (north) lat = PI * (double)(nlat/2-ilat-1)/(double)nlat;
		else       lat = PI * (double)(nlat/2-ilat)/(double)nlat;
		double s = obj_size;
		double dx = s*cos(lng)*cos(lat); // the offsets between sphere centre and tile corner
		double dy = s*sin(lat);
		double dz = s*sin(lng)*cos(lat);
		prm.dwmat_tmp.m41 = (dx*prm.grot.m11 + dy*prm.grot.m12 + dz*prm.grot.m13 + prm.cpos.x) * (float)prm.scale;
		prm.dwmat_tmp.m42 = (dx*prm.grot.m21 + dy*prm.grot.m22 + dz*prm.grot.m23 + prm.cpos.y) * (float)prm.scale;
		prm.dwmat_tmp.m43 = (dx*prm.grot.m31 + dy*prm.grot.m32 + dz*prm.grot.m33 + prm.cpos.z) * (float)prm.scale;
		return mul(lrot,prm.dwmat_tmp);
	}
}
