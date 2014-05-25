// ==============================================================
//   ORBITER VISUALISATION PROJECT (OVP)
//   Copyright (C) 2006-2014 Martin Schweiger
//   Dual licensed under GPL v3 and LGPL v3
// ==============================================================

// ==============================================================
// tilemanager2.cpp
// Rendering of planetary surfaces using texture tiles at
// variable resolutions (new version).
// ==============================================================

#include "Tilemgr2.h"
#include "Texture.h"
#include "D3D9Config.h"
#include "D3D9Effect.h"

// =======================================================================
// Externals

static int maxres = 14;    // max tree resolution depth    
static int patch_res = 32; // patch node grid dimensions
static int elev_stride = patch_res*8+3;
static TEXCRDRANGE2 fullrange = {0,1,0,1};

int SURF_MAX_PATCHLEVEL2 = 18; // move this somewhere else


// =======================================================================
// Class Tile

Tile::Tile (TileManager2Base *_mgr, int _lvl, int _ilat, int _ilng)
: mgr(_mgr), lvl(_lvl), ilat(_ilat), ilng(_ilng)
{
	mesh = 0;
	tex = 0;
	mean_elev = 0.0;
	texrange = fullrange;
	owntex = true;
	lngnbr_lvl = latnbr_lvl = dianbr_lvl = _lvl;
	state = Invalid;
	cnt = Centre();
}

// -----------------------------------------------------------------------

Tile::~Tile ()
{
	if (mesh) delete mesh;
	if (tex && owntex) tex->Release();
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
// Check if tile bounding box intersects the viewing frustum
// given the transformation matrix transform

bool Tile::InView (const MATRIX4 &transform)
{
	return true;

	if (!lvl) return true; // no good check for this yet

	if (!mesh) return true; // DEBUG : TEMPORARY

	bool bx1, bx2, by1, by2, bz1, bbvis;
	int v;
	bx1 = bx2 = by1 = by2 = bz1 = bbvis = false;
	double hx, hy, hz;
	for (v = 0; v < 8; v++) {
		VECTOR4 vt = mul (mesh->Box[v], transform);
		hx = vt.x/vt.w, hy = vt.y/vt.w, hz = vt.z/vt.w;
		if (hz <= 1.0) hx = -hx, hy = -hy;
		if (hz >  0.0) bz1 = true;
		if (hx > -1.0) bx1 = true;
		if (hx <  1.0) bx2 = true;
		if (hy > -1.0) by1 = true;
		if (hy <  1.0) by2 = true;
		if (bbvis = bx1 && bx2 && by1 && by2 && bz1) break;
	}
	return bbvis;
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

void Tile::Extents (double *latmin, double *latmax, double *lngmin, double *lngmax) const
{
	int nlat = 1 << lvl;
	int nlng = 2 << lvl;
	*latmin = PI * (0.5 - (double)(ilat+1)/(double)nlat);
	*latmax = PI * (0.5 - (double)ilat/(double)nlat);
	*lngmin = PI2 * (double)(ilng-nlng/2)/(double)nlng;
	*lngmax = PI2 * (double)(ilng-nlng/2+1)/(double)nlng;
}

// -----------------------------------------------------------------------

VBMESH *Tile::CreateMesh_quadpatch (int grdlat, int grdlng, INT16 *elev, double globelev,
	const TEXCRDRANGE2 *range, bool shift_origin, VECTOR3 *shift)
{
	const float TEX2_MULTIPLIER = 4.0f; // was: 16.0f
	const float c1 = 1.0f, c2 = 0.0f;   // -1.0f/512.0f; // assumes 256x256 texture patches
	int i, j, n, nofs0, nofs1;
	int nlng = 2 << lvl;
	int nlat = 1 << lvl;
	bool north = (ilat < nlat/2);

	double lat, slat, clat, lng, slng, clng, eradius, dx, dy;
	double minlat = PI * (double)(nlat/2-ilat-1)/(double)nlat;
	double maxlat = PI * (double)(nlat/2-ilat)/(double)nlat;
	double minlng = 0;
	double maxlng = PI2/(double)nlng;
	double radius = mgr->obj_size;
	VECTOR3 pos, tpos, nml;
	if (!range) range = &fullrange;
	float turange = range->tumax-range->tumin;
	float tvrange = range->tvmax-range->tvmin;

	int nvtx = (grdlat+1)*(grdlng+1);         // patch mesh node grid
	int nvtxbuf = nvtx + grdlat+1 + grdlng+1; // add buffer for storage of edges (for elevation matching)
	VERTEX_2TEX *vtx = new VERTEX_2TEX[nvtxbuf];

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
			if (elev) eradius += (double)elev[(i+1)*elev_stride + j+1];
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
			vtx[n].x = D3DVAL(pos.x - dx); vtx[n].nx = D3DVAL(nml.x);
			vtx[n].y = D3DVAL(pos.y - dy); vtx[n].ny = D3DVAL(nml.y);
			vtx[n].z = D3DVAL(pos.z);      vtx[n].nz = D3DVAL(nml.z);

			vtx[n].tu0 = D3DVAL((c1*j)/grdlng+c2); // overlap to avoid seams
			vtx[n].tv0 = D3DVAL(grdlat-i)/D3DVAL(grdlat);
			vtx[n].tu1 = vtx[n].tu0 * TEX2_MULTIPLIER;
			vtx[n].tv1 = vtx[n].tv0 * TEX2_MULTIPLIER;

			// map texture coordinates to subrange
			vtx[n].tu0 = vtx[n].tu0*turange + range->tumin;
			vtx[n].tv0 = vtx[n].tv0*tvrange + range->tvmin;
			n++;
		}
	}

	// create the face indices
	int nidx = 2*grdlat*grdlng * 3;
	WORD *idx = new WORD[nidx];

	if (elev) { // do adaptive orientation of cell diagonal
		INT16 *elev1, *elev2, *elev1n, *elev2n, err1, err2;
		for (i = n = nofs0 = 0; i < grdlat; i++) {
			nofs1 = nofs0+grdlng+1;
			for (j = 0; j < grdlng; j++) {
				elev1  = elev + elev_stride+2+j+elev_stride*i;
				elev2  = elev1 + elev_stride-1;
				elev1n = elev1 - elev_stride+1;
				elev2n = elev2 + elev_stride-1;
				err1 = abs(*elev1 * 2 - *elev2 - *elev1n) + abs(*elev2 * 2 - *elev1 - *elev2n);
				elev1  = elev + elev_stride+1+j+elev_stride*i;
				elev2  = elev1 + elev_stride+1;
				elev1n = elev1 - elev_stride-1;
				elev2n = elev2 + elev_stride+1;
				err2 = abs(*elev1 * 2 - *elev2 - *elev1n) + abs(*elev2 * 2 - *elev1 - *elev2n);
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
		const double shade_exaggerate = 1.0; // 1 = normal, <1 = more dramatic landscape shadows
		double dy, dz, dydz, nz_x, nz_z, ny_x, ny_y, nx1, ny1, nz1;
		double dy_dezp, dy_dezm, dz_deyp, dz_deym;
		int en;
		dy = radius * PI/(nlat*grdlat);  // y-distance between vertices
		ny_x = shade_exaggerate*dy;
		for (i = n = 0; i <= grdlat; i++) {
			lat = minlat + (maxlat-minlat) * (double)i/(double)grdlat;
			slat = sin(lat), clat = cos(lat);
			dz = radius * PI2*cos(lat) / (nlng*grdlng); // z-distance between vertices on unit sphere
			dydz = dy*dz;
			nz_x = shade_exaggerate*dz;
			for (j = 0; j <= grdlng; j++) {
				lng = minlng + (maxlng-minlng) * (double)j/(double)grdlng;
				slng = sin(lng), clng = cos(lng);
				en = (i+1)*elev_stride + (j+1);

				// This version avoids the normalisation of the 4 intermediate face normals
				// It's faster and doesn't seem to make much difference
				VECTOR3 nml = {2.0*dydz, dz*(elev[en-elev_stride]-elev[en+elev_stride]), dy*(elev[en-1]-elev[en+1])};
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
	VBMESH *mesh = new VBMESH;
	mesh->vtx = vtx;
	mesh->nv  = nvtx;
	mesh->idx = idx;
	mesh->nf  = nidx/3;

	// set bounding box for visibility calculations
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

	mesh->MapVertices(TileManager2Base::pDev); // TODO

	return mesh;
}

// -----------------------------------------------------------------------

VBMESH *Tile::CreateMesh_hemisphere (int grd, INT16 *elev, double globelev)
{
	const int texres = 512;
	double radius = mgr->obj_size + globelev;
	double eradius, slat, clat, slng, clng;
	VECTOR3 pos, nml;

	// Allocate memory for the vertices and indices
	int         nVtx = grd*(grd+1)+2;
	int         nIdx = 6*grd*grd;
	VERTEX_2TEX *Vtx = new VERTEX_2TEX[nVtx];
	WORD*       Idx = new WORD[nIdx];

	// Counters
    WORD x, y, nvtx = 0, nidx = 0;
	VERTEX_2TEX *vtx = Vtx;
	WORD *idx = Idx;

	// Angle deltas for constructing the sphere's vertices
    double fDAng   = PI / grd;
    double lat = fDAng;
	DWORD x1 = grd;
	DWORD x2 = x1+1;
	FLOAT du = 0.5f/(FLOAT)texres;
	FLOAT a  = (1.0f-2.0f*du)/(FLOAT)x1;

    // Make the middle of the sphere
    for (y = 0; y < grd; y++) {
		slat = sin(lat), clat = cos(lat);
		float tv = (float)(lat/PI);

        for (x = 0; x < x2; x++) {
            double lng = x*fDAng - PI;  // subtract Pi to wrap at +-180°
			if (ilng) lng += PI;
			slng = sin(lng), clng = cos(lng);
			eradius = radius; // TODO: elevation
			nml = _V(slat*clng, clat, slat*slng);
			pos = nml*eradius;
			vtx->x = D3DVAL(pos.x);  vtx->nx = D3DVAL(nml.x);
			vtx->y = D3DVAL(pos.y);  vtx->ny = D3DVAL(nml.y);
			vtx->z = D3DVAL(pos.z);  vtx->nz = D3DVAL(nml.z);
			FLOAT tu = a*(FLOAT)x + du;
			vtx->tu0 = vtx->tu1 = tu;
			vtx->tv0 = vtx->tv1 = tv;
			vtx++;
			nvtx++;
        }
        lat += fDAng;
    }

    for (y = 0; y < grd-1; y++) {
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
	eradius = radius;
	nml = _V(0,1,0);
	pos = nml*eradius;
	vtx->x = D3DVAL(pos.x);  vtx->nx = D3DVAL(nml.x);
	vtx->y = D3DVAL(pos.y);  vtx->ny = D3DVAL(nml.y);
	vtx->z = D3DVAL(pos.z);  vtx->nz = D3DVAL(nml.z);
	vtx->tu0 = vtx->tu1 = 0.5f;
	vtx->tv0 = vtx->tv1 = 0.0f;
	vtx++;
    nvtx++;
	WORD wSouthVtx = nvtx;

	
	eradius = radius;
	nml = _V(0,-1,0);
	pos = nml*eradius;
	vtx->x = D3DVAL(pos.x);  vtx->nx = D3DVAL(nml.x);
	vtx->y = D3DVAL(pos.y);  vtx->ny = D3DVAL(nml.y);
	vtx->z = D3DVAL(pos.z);  vtx->nz = D3DVAL(nml.z);
	vtx->tu0 = vtx->tu1 = 0.5f;
	vtx->tv0 = vtx->tv1 = 1.0f;
	vtx++;
    nvtx++;

    for (x = 0; x < x1; x++) {
		WORD p1 = wSouthVtx;
		WORD p2 = (WORD)( (y)*x2 + (x+0) );
		WORD p3 = (WORD)( (y)*x2 + (x+1) );

        *idx++ = p1;
        *idx++ = p3;
        *idx++ = p2;
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

	// create the mesh
	VBMESH *mesh = new VBMESH;
	mesh->vtx = Vtx;
	mesh->nv  = nVtx;
	mesh->idx = Idx;
	mesh->nf  = nIdx/3;
	mesh->MapVertices (TileManager2Base::pDev); // TODO
	return mesh;
}

// =======================================================================
// =======================================================================

const oapi::D3D9Client *TileLoader::gc = 0;
bool TileLoader::bRunThread = true;
int TileLoader::nqueue = 0;
int TileLoader::queue_in = 0;
int TileLoader::queue_out = 0;
HANDLE TileLoader::hLoadMutex = 0;
struct TileLoader::QUEUEDESC TileLoader::queue[MAXQUEUE2] = {0};

TileLoader::TileLoader (const oapi::D3D9Client *gclient)
{
	gc = gclient;
	bRunThread = true;
	nqueue = queue_in = queue_out = 0;
	load_frequency = Config->PlanetLoadFrequency;
	DWORD id;
	hLoadMutex = CreateMutex (0, FALSE, NULL);
	hLoadThread = CreateThread (NULL, 32768, Load_ThreadProc, this, 0, &id);
}

// -----------------------------------------------------------------------

TileLoader::~TileLoader ()
{
	bRunThread = false;
	if (WaitForSingleObject (hLoadThread, 1000) == WAIT_TIMEOUT) {
		TerminateThread (hLoadThread, 0);
	}
	CloseHandle (hLoadThread);
	CloseHandle (hLoadMutex);
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

bool TileLoader::Unqueue (Tile *tile)
{
	if (tile->state != Tile::InQueue) return false;

	int i, j;
	for (i = 0; i < nqueue; i++) {
		j = (queue_out+i) % MAXQUEUE2;
		if (queue[j].tile == tile) {
			if (i) queue[j].tile = queue[queue_out].tile;
			queue_out = (queue_out+1) % MAXQUEUE2;
			nqueue--;
			return true;
		}
	}
	return false;
}

// -----------------------------------------------------------------------

DWORD WINAPI TileLoader::Load_ThreadProc (void *data)
{
	TileLoader *loader = (TileLoader*)data;
	DWORD idle = 1000/loader->load_frequency;
	Tile *tile;
	bool load;

	while (bRunThread) {
		Sleep (idle);
		WaitForMutex ();
		if (load = (nqueue > 0)) {
			tile = queue[queue_out].tile;
			tile->state = Tile::Loading; // lock tile and its ancestor tree
			queue_out = (queue_out+1) % MAXQUEUE2; // remove from queue
			nqueue--;
		}
		ReleaseMutex ();

		if (load) {
			tile->Load(); // load/create the tile

			WaitForMutex ();
			tile->state = Tile::Inactive; // unlock tile
			ReleaseMutex ();
		}
	}
	return 0;
}

// =======================================================================
// =======================================================================

class oapi::D3D9Client *TileManager2Base::gc = NULL; 
LPDIRECT3DDEVICE9 TileManager2Base::pDev = NULL;
TileManager2Base::configPrm TileManager2Base::cprm = {
	1,                  // elevInterpol
	false,				// bSpecular
	false,				// bLights
	false,              // bCloudShadow
	0.5                 // lightfac
};
TileLoader *TileManager2Base::loader = NULL;
double TileManager2Base::resolutionBias = 4.0;
bool TileManager2Base::bTileLoadThread = false;

// ---------------------------------------------------------
ID3DXEffect *TileManager2Base::pShader = NULL;
D3DXHANDLE TileManager2Base::eTileTech = NULL;
D3DXHANDLE TileManager2Base::eCloudTech = NULL;
// ------------------------------------------------------------  
D3DXHANDLE TileManager2Base::smWorld = NULL;
D3DXHANDLE TileManager2Base::smViewProj = NULL;
// ------------------------------------------------------------  
D3DXHANDLE TileManager2Base::svTexOff = NULL;
D3DXHANDLE TileManager2Base::svWater = NULL;
D3DXHANDLE TileManager2Base::svSunDir = NULL;
D3DXHANDLE TileManager2Base::svAddBkg = NULL;
D3DXHANDLE TileManager2Base::svTint = NULL;
// ------------------------------------------------------------
D3DXHANDLE TileManager2Base::sfDistScale = NULL;
D3DXHANDLE TileManager2Base::sfAlpha = NULL;
D3DXHANDLE TileManager2Base::sfNight = NULL;
// ------------------------------------------------------------
D3DXHANDLE TileManager2Base::sbSpecular = NULL;
D3DXHANDLE TileManager2Base::sbCloudSh = NULL;
D3DXHANDLE TileManager2Base::sbLights = NULL;
D3DXHANDLE TileManager2Base::sbLegacyAtm = NULL;
// ------------------------------------------------------------
D3DXHANDLE TileManager2Base::stDiff = NULL;
D3DXHANDLE TileManager2Base::stMask = NULL;
// Atmosphere -------------------------------------------------
D3DXHANDLE TileManager2Base::svFogColor = NULL;
D3DXHANDLE TileManager2Base::sfFogDensity = NULL;
D3DXHANDLE TileManager2Base::sfGlobalAmb = NULL;
D3DXHANDLE TileManager2Base::sfSunAppRad = NULL;
D3DXHANDLE TileManager2Base::sfDispersion = NULL;
D3DXHANDLE TileManager2Base::sfAmbient0 = NULL;
// Scatter model ----------------------------------------------
D3DXHANDLE TileManager2Base::svPhase = NULL;		
D3DXHANDLE TileManager2Base::svODCoEff = NULL;
D3DXHANDLE TileManager2Base::svRayTotal = NULL;	
D3DXHANDLE TileManager2Base::svRayInSct = NULL;
D3DXHANDLE TileManager2Base::svRaySurface = NULL;
D3DXHANDLE TileManager2Base::svMieTotal = NULL;
D3DXHANDLE TileManager2Base::svCameraPos = NULL;		
D3DXHANDLE TileManager2Base::svUnitCameraPos = NULL;		
D3DXHANDLE TileManager2Base::sfSunIntensity = NULL;
D3DXHANDLE TileManager2Base::sfSrfIntensity = NULL;
D3DXHANDLE TileManager2Base::sfScaleHeight = NULL;		
D3DXHANDLE TileManager2Base::sfInvScaleHeight = NULL;
D3DXHANDLE TileManager2Base::sfRadius = NULL;
D3DXHANDLE TileManager2Base::sfCameraAlt = NULL;
D3DXHANDLE TileManager2Base::sfAtmRad2 = NULL;
D3DXHANDLE TileManager2Base::sfBalance = NULL;
D3DXHANDLE TileManager2Base::siMode = NULL;


// -----------------------------------------------------------------------

TileManager2Base::TileManager2Base (const vPlanet *vplanet, int _maxres)
: vp(vplanet)
{
	// set persistent parameters
	prm.maxlvl = max (1, _maxres-4);

	obj = vp->Object();
	obj_size = oapiGetSize (obj);
	oapiGetObjectName (obj, cbody_name, 256);
}

// -----------------------------------------------------------------------

void TileManager2Base::GlobalInit (class oapi::D3D9Client *gclient)
{
	LogAlw("Starting to initialize Surface.fx a shading technique...");

	gc = gclient;
	pDev = gc->GetDevice();

	resolutionBias = 4.0 + *(double*)gclient->GetConfigParam (CFGPRM_RESOLUTIONBIAS);
	cprm.bSpecular = *(bool*)gclient->GetConfigParam (CFGPRM_SURFACEREFLECT);
	cprm.bLights = *(bool*)gclient->GetConfigParam (CFGPRM_SURFACELIGHTS);
	cprm.bCloudShadow = *(bool*)gclient->GetConfigParam (CFGPRM_CLOUDSHADOWS);
	cprm.lightfac = *(double*)gclient->GetConfigParam (CFGPRM_SURFACELIGHTBRT);
	cprm.elevMode = *(int*)gclient->GetConfigParam (CFGPRM_ELEVATIONMODE);
	bTileLoadThread = true; // TODO: g_pOrbiter->Cfg()->CfgPRenderPrm.bLoadOnThread;

	loader = new TileLoader (gc);


	// -----------------------------------------------------------------------
	// Initialize Surface.fx Shader
	// -----------------------------------------------------------------------

	char name[256];

	WORD Model = gc->GetHardwareCaps()->PixelShaderVersion & 0xFFFF;

	if (!strcmp(Config->Shaders,"Level20")) Model = 0x200;

	// Create the Effect from a .fx file.
	ID3DXBuffer* errors = 0;
	D3DXMACRO macro[8]; memset2(&macro, 0, 8*sizeof(D3DXMACRO));

	macro[0].Name = "VS_MOD";
	macro[1].Name = "PS_MOD";

	if (Model==0x200) {
		LogAlw("[Compiling Effects for Shader Model 2.0]");
		oapiWriteLog("D3D9Client: [Compiling Effects for Shader Model 2.0]");
		macro[0].Definition = "vs_2_0";
		macro[1].Definition = "ps_2_0";
		sprintf_s(name,256,"Modules/D3D9Client20/Surface.fx");
	}
	else {
		LogAlw("[Compiling Effects for Shader Model 3.0]");
		oapiWriteLog("D3D9Client: [Compiling Effects for Shader Model 3.0]");
		macro[0].Definition = "vs_3_0";
		macro[1].Definition = "ps_3_0";
		sprintf_s(name,256,"Modules/D3D9Client/Surface.fx");
	}
	
	
	macro[2].Name = "ANISOTROPY_MACRO";
	macro[2].Definition = new char[32];
	sprintf_s((char*)macro[2].Definition,32,"%d",max(2,Config->Anisotrophy));
	
	HR(D3DXCreateEffectFromFileA(pDev, name, macro, 0, 0, 0, &pShader, &errors));
	
	delete []macro[2].Definition;

	if (errors) {
		LogErr("Effect Error: %s",(char*)errors->GetBufferPointer());
		MessageBoxA(0, (char*)errors->GetBufferPointer(), "Surface.fx Error", 0);
		FatalAppExitA(0,"Critical error has occured. See Orbiter.log for details");
	}

	if (!pShader) {
		LogErr("Failed to create an Effect (%s)",name);
		return;
	}

	// ---------------------------------------------------------------------
	// Bind shader-side variables and constants to local handles

	// Techniques ----------------------------------------------------------
	eTileTech			= pShader->GetTechniqueByName("TileTech");
	eCloudTech			= pShader->GetTechniqueByName("CloudTech");
	// ------------------------------------------------------------  
	smWorld				= pShader->GetParameterByName(0,"mWorld");
	smViewProj			= pShader->GetParameterByName(0,"mViewProj");
	// ------------------------------------------------------------  
	svTexOff			= pShader->GetParameterByName(0,"vTexOff");
	svWater				= pShader->GetParameterByName(0,"vWater");
	svSunDir			= pShader->GetParameterByName(0,"vSunDir");
	svAddBkg			= pShader->GetParameterByName(0,"vAddBkg");
	svTint				= pShader->GetParameterByName(0,"vTint");
	// ------------------------------------------------------------
	sfDistScale			= pShader->GetParameterByName(0,"fDistScale");
	sfAlpha				= pShader->GetParameterByName(0,"fAlpha");
	sfNight				= pShader->GetParameterByName(0,"fNight");
	// ------------------------------------------------------------
	sbSpecular			= pShader->GetParameterByName(0,"bSpecular");
	sbCloudSh			= pShader->GetParameterByName(0,"bCloudSh");
	sbLights			= pShader->GetParameterByName(0,"bLights");
	sbLegacyAtm			= pShader->GetParameterByName(0,"bLegacyAtm");
	// ------------------------------------------------------------
	stDiff				= pShader->GetParameterByName(0,"tDiff");
	stMask				= pShader->GetParameterByName(0,"tMask");
	// Atmosphere -----------------------------------------------------------
	svFogColor			= pShader->GetParameterByName(0,"vFogColor");
	sfFogDensity		= pShader->GetParameterByName(0,"fFogDensity");
	sfGlobalAmb			= pShader->GetParameterByName(0,"fGlobalAmb");
	sfSunAppRad			= pShader->GetParameterByName(0,"fSunAppRad");
	sfDispersion		= pShader->GetParameterByName(0,"fDispersion");
	sfAmbient0			= pShader->GetParameterByName(0,"fAmbient0");
	
	// Scatter model --------------------------------------------------------
	svPhase				= pShader->GetParameterByName(0,"vPhase");		
	svODCoEff			= pShader->GetParameterByName(0,"vODCoEff");
	svRayTotal			= pShader->GetParameterByName(0,"vRayTotal");
	svRayInSct			= pShader->GetParameterByName(0,"vRayInSct");
	svRaySurface		= pShader->GetParameterByName(0,"vRaySurface");
	svMieTotal			= pShader->GetParameterByName(0,"vMieTotal");
	svCameraPos			= pShader->GetParameterByName(0,"vCameraPos");		
	svUnitCameraPos		= pShader->GetParameterByName(0,"vUnitCameraPos");		
	sfSunIntensity		= pShader->GetParameterByName(0,"fSunIntensity");
	sfSrfIntensity		= pShader->GetParameterByName(0,"fSrfIntensity");
	sfScaleHeight		= pShader->GetParameterByName(0,"fScaleHeight");		
	sfInvScaleHeight	= pShader->GetParameterByName(0,"fInvScaleHeight");
	sfRadius			= pShader->GetParameterByName(0,"fRadius");
	sfCameraAlt			= pShader->GetParameterByName(0,"fCameraAlt");
	sfAtmRad2			= pShader->GetParameterByName(0,"fAtmRad2");
	sfBalance			= pShader->GetParameterByName(0,"fBalance");
	siMode				= pShader->GetParameterByName(0,"iMode");
}

// -----------------------------------------------------------------------

void TileManager2Base::GlobalExit ()
{
	delete loader;
	SAFE_RELEASE(pShader);
}

// -----------------------------------------------------------------------

void TileManager2Base::InitLegacyAtmosphere ()
{
	VECTOR3 GS, GP;

	OBJHANDLE hPlanet = GetPlanet()->GetObject();

	DWORD dAmbient = *(DWORD*)GClient()->GetConfigParam(CFGPRM_AMBIENTLEVEL);
	float fAmbient = float(dAmbient)*0.0039f;
	
	OBJHANDLE hS = oapiGetGbodyByIndex(0);	// the central star
	oapiGetGlobalPos (hS, &GS);				// sun position
	oapiGetGlobalPos (hPlanet, &GP);		// planet position
			
	float rs = (float)(oapiGetSize(hS) / length(GS-GP));
	
	const ATMCONST *atm = (oapiGetObjectType(hPlanet)==OBJTP_PLANET ? oapiGetPlanetAtmConstants (hPlanet) : NULL);

	HR(pShader->SetFloat(sfGlobalAmb, fAmbient));
	HR(pShader->SetFloat(sfSunAppRad, rs)); 

	if (atm) {
		HR(pShader->SetFloat(sfAmbient0, min(0.7f, log(float(atm->rho0)+1.0f)*0.4f)));
		HR(pShader->SetFloat(sfDispersion, max(0.02f, min(0.9f, log(float(atm->rho0)+1.0f)))));
	}
	else {
		HR(pShader->SetFloat(sfAmbient0, 0.0f));
		HR(pShader->SetFloat(sfDispersion, 0.0f));
	}
}

// -----------------------------------------------------------------------

void TileManager2Base::SetRenderPrm (MATRIX4 &dwmat, double prerot, bool use_zbuf, const vPlanet::RenderPrm &rprm)
{
	const double minalt = 0.002;
	VECTOR3 obj_pos, cam_pos;

	prm.rprm = &rprm;

	// set up the parameter structure
	oapiGetRotationMatrix(obj, &prm.grot); // planet's rotation matrix
	oapiGetGlobalPos (obj, &obj_pos);      // planet position
	oapiCameraGlobalPos (&cam_pos);        // camera position

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
	prm.viewap = acos (1.0/(max (prm.cdist, 1.0+minalt)));
	prm.scale = 1.0;
	prm.fog = rprm.bFog;
	prm.tint = rprm.bTint;
	//if (prm.tint)
	//	prm.atm_tint = rprm.rgbTint;
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

// -----------------------------------------------------------------------

void TileManager2Base::SetWorldMatrix (const MATRIX4 &W)
{
	D3DXMATRIX wtrans;
	MATRIX4toD3DMATRIX (W, wtrans);
	Shader()->SetMatrix(smWorld, &wtrans);
}
