// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//   ORBITER VISUALISATION PROJECT (OVP)
//   D3D7 Client module
// ==============================================================

// ==============================================================
// spherepatch.cpp
// Create meshes for spheres and sphere patches
// ==============================================================

#include "spherepatch.h"

static float TEX2_MULTIPLIER = 4.0f; // microtexture multiplier

// ==============================================================
// struct VBMESH

VBMESH::VBMESH ()
{
	vb = 0;
	bb = 0;
	vtx = 0;
	bbvtx = 0;
	nv = 0;
	idx = 0;
	ni = 0;
}

VBMESH::~VBMESH ()
{
	if (vb) vb->Release();
	if (bb) bb->Release();
	if (nv) delete []vtx;
	if (bbvtx) delete []bbvtx;
	if (ni) delete []idx;
}

void VBMESH::MapVertices (LPDIRECT3D7 d3d, LPDIRECT3DDEVICE7 dev, DWORD MemFlag)
{
	if (vb) vb->Release();

	D3DVERTEXBUFFERDESC vbd = 
		{ sizeof(D3DVERTEXBUFFERDESC), MemFlag | D3DVBCAPS_WRITEONLY, FVF_2TEX, nv };
	d3d->CreateVertexBuffer (&vbd, &vb, 0);
	LPVOID data;
	vb->Lock (DDLOCK_WAIT | DDLOCK_WRITEONLY | DDLOCK_DISCARDCONTENTS, (LPVOID*)&data, NULL);
	memcpy (data, vtx, nv*sizeof(VERTEX_2TEX));
	vb->Unlock();
	vb->Optimize (dev, 0);
}

// ==============================================================
// CreateSphere()
// Create a spherical mesh of radius 1 and resolution defined by nrings
// Below is a list of #vertices and #indices against nrings:
//
// nrings  nvtx   nidx   (nidx = 12 nrings^2)
//   4       38    192
//   6       80    432
//   8      138    768
//  12      302   1728
//  16      530   3072
//  20      822   4800
//  24     1178   6912

void CreateSphere (const oapi::D3D7Client *gclient, VBMESH &mesh, DWORD nrings, bool hemisphere, int which_half, int texres)
{
	const LPDIRECT3D7 d3d = gclient->GetDirect3D7();
	const LPDIRECT3DDEVICE7 dev = gclient->GetDevice();
	DWORD vbMemCaps = (gclient->GetFramework()->IsTLDevice() ? 0 : D3DVBCAPS_SYSTEMMEMORY);

	// Allocate memory for the vertices and indices
	DWORD       nVtx = hemisphere ? nrings*(nrings+1)+2 : nrings*(2*nrings+1)+2;
	DWORD       nIdx = hemisphere ? 6*nrings*nrings : 12*nrings*nrings;
	VERTEX_2TEX* Vtx = new VERTEX_2TEX[nVtx];
	WORD*        Idx = new WORD[nIdx];

	// Counters
    WORD x, y, nvtx = 0, nidx = 0;
	VERTEX_2TEX *vtx = Vtx;
	WORD *idx = Idx;

	// Angle deltas for constructing the sphere's vertices
    FLOAT fDAng   = (FLOAT)PI / nrings;
    FLOAT fDAngY0 = fDAng;
	DWORD x1 = (hemisphere ? nrings : nrings*2);
	DWORD x2 = x1+1;
	FLOAT du = 0.5f/(FLOAT)texres;
	FLOAT a  = (1.0f-2.0f*du)/(FLOAT)x1;

    // Make the middle of the sphere
    for (y = 0; y < nrings; y++) {
        FLOAT y0 = (FLOAT)cos(fDAngY0);
        FLOAT r0 = (FLOAT)sin(fDAngY0);
		FLOAT tv = fDAngY0/(FLOAT)PI;

        for (x = 0; x < x2; x++) {
            FLOAT fDAngX0 = x*fDAng - (FLOAT)PI;  // subtract Pi to wrap at +-180°
			if (hemisphere && which_half) fDAngX0 += (FLOAT)PI;

			D3DVECTOR v = {r0*(FLOAT)cos(fDAngX0), y0, r0*(FLOAT)sin(fDAngX0)};
			FLOAT tu = a*(FLOAT)x + du;
			//FLOAT tu = x/(FLOAT)x1;

            *vtx++ = VERTEX_2TEX (v, v, tu, tv, tu, tv);
			nvtx++;
        }
        fDAngY0 += fDAng;
    }

    for (y = 0; y < nrings-1; y++) {
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
	D3DVECTOR pvy = {0, 1, 0}, nvy = {0,-1,0};
	WORD wNorthVtx = nvtx;
    *vtx++ = VERTEX_2TEX (pvy, pvy, 0.5f, 0.0f, 0.5f, 0.0f);
    nvtx++;
	WORD wSouthVtx = nvtx;
    *vtx++ = VERTEX_2TEX (nvy, nvy, 0.5f, 1.0f, 0.5f, 1.0f);
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

	D3DVERTEXBUFFERDESC vbd = 
	{ sizeof(D3DVERTEXBUFFERDESC), vbMemCaps | D3DVBCAPS_WRITEONLY, FVF_2TEX, nVtx };
	LPVOID data;

	d3d->CreateVertexBuffer (&vbd, &mesh.vb, 0);
	mesh.vb->Lock (DDLOCK_WAIT | DDLOCK_WRITEONLY | DDLOCK_DISCARDCONTENTS, (LPVOID*)&data, NULL);
	memcpy (data, Vtx, nVtx*sizeof(VERTEX_2TEX));
	mesh.vb->Unlock();
	mesh.vb->Optimize (dev, 0);
	delete []Vtx;
	mesh.nv  = nVtx;
	mesh.idx = Idx;
	mesh.ni  = nIdx;
	mesh.bb = 0;
	mesh.vtx = 0;
}

// ==============================================================

void CreateSpherePatch (const oapi::D3D7Client *gclient, VBMESH &mesh, int nlng, int nlat, int ilat, int res, int bseg,
	bool reduce, bool outside, bool store_vtx, bool shift_origin)
{
	const LPDIRECT3D7 d3d = gclient->GetDirect3D7();
	const LPDIRECT3DDEVICE7 dev = gclient->GetDevice();
	DWORD vbMemCaps = (gclient->GetFramework()->IsTLDevice() ? 0 : D3DVBCAPS_SYSTEMMEMORY);

	const float c1 = 1.0f, c2 = 0.0f;
	int i, j, nVtx, nIdx, nseg, n, nofs0, nofs1;
	double minlat, maxlat, lat, minlng, maxlng, lng;
	double slat, clat, slng, clng;
	WORD tmp;
	VECTOR3 pos, tpos;

	minlat = PI*0.5 * (double)ilat/(double)nlat;
	maxlat = PI*0.5 * (double)(ilat+1)/(double)nlat;
	minlng = 0;
	maxlng = PI*2.0/(double)nlng;
	if (bseg < 0 || ilat == nlat-1) bseg = (nlat-ilat)*res;

	// generate nodes
	nVtx = (bseg+1)*(res+1);
	if (reduce) nVtx -= ((res+1)*res)/2;
	VERTEX_2TEX *Vtx = new VERTEX_2TEX[nVtx];

	// create transformation for bounding box
	// we define the local coordinates for the patch so that the x-axis points
	// from (minlng,minlat) corner to (maxlng,minlat) corner (origin is halfway between)
	// y-axis points from local origin to middle between (minlng,maxlat) and (maxlng,maxlat)
	// bounding box is created in this system and then transformed back to planet coords.
	double clat0 = cos(minlat), slat0 = sin(minlat);
	double clng0 = cos(minlng), slng0 = sin(minlng);
	double clat1 = cos(maxlat), slat1 = sin(maxlat);
	double clng1 = cos(maxlng), slng1 = sin(maxlng);
	VECTOR3 ex = {clat0*clng1 - clat0*clng0, 0, clat0*slng1 - clat0*slng0}; normalise(ex);
	VECTOR3 ey = {0.5*(clng0+clng1)*(clat1-clat0), slat1-slat0, 0.5*(slng0+slng1)*(clat1-clat0)}; normalise(ey);
	VECTOR3 ez = crossp (ey, ex);
	MATRIX3 R = {ex.x, ex.y, ex.z,  ey.x, ey.y, ey.z,  ez.x, ez.y, ez.z};
	VECTOR3 pref = {0.5*(clat0*clng1 + clat0*clng0), slat0, 0.5*(clat0*slng1 + clat0*slng0)}; // origin
	VECTOR3 tpmin, tpmax; 

	float dx, dy;
	if (shift_origin) {
		dx = (float)clat0;
		dy = (float)slat0;
	}

	for (i = n = 0; i <= res; i++) {  // loop over longitudinal strips
		lat = minlat + (maxlat-minlat) * (double)i/(double)res;
		slat = sin(lat), clat = cos(lat);
		nseg = (reduce ? bseg-i : bseg);
		for (j = 0; j <= nseg; j++) {
			lng = (nseg ? minlng + (maxlng-minlng) * (double)j/(double)nseg : 0.0);
			slng = sin(lng), clng = cos(lng);
			pos = _V(clat*clng, slat, clat*slng);
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

			Vtx[n].x = Vtx[n].nx = D3DVAL(pos.x);
			Vtx[n].y = Vtx[n].ny = D3DVAL(pos.y);
			Vtx[n].z = Vtx[n].nz = D3DVAL(pos.z);
			if (shift_origin)
				Vtx[n].x -= dx, Vtx[n].y -= dy;

			Vtx[n].tu0 = D3DVAL(nseg ? (c1*j)/nseg+c2 : 0.5f); // overlap to avoid seams
			Vtx[n].tv0 = D3DVAL((c1*(res-i))/res+c2);
			Vtx[n].tu1 = (nseg ? Vtx[n].tu0 * TEX2_MULTIPLIER : 0.5f);
			Vtx[n].tv1 = Vtx[n].tv0 * TEX2_MULTIPLIER;
			if (!outside) {
				Vtx[n].nx = -Vtx[n].nx;
				Vtx[n].ny = -Vtx[n].ny;
				Vtx[n].nz = -Vtx[n].nz;
			}
			n++;
		}
	}

	// generate faces
	nIdx = (reduce ? res * (2*bseg-res) : 2*res*bseg) * 3;
	WORD *Idx = new WORD[nIdx];

	for (i = n = nofs0 = 0; i < res; i++) {
		nseg = (reduce ? bseg-i : bseg);
		nofs1 = nofs0+nseg+1;
		for (j = 0; j < nseg; j++) {
			Idx[n++] = nofs0+j;
			Idx[n++] = nofs1+j;
			Idx[n++] = nofs0+j+1;
			if (reduce && j == nseg-1) break;
			Idx[n++] = nofs0+j+1;
			Idx[n++] = nofs1+j;
			Idx[n++] = nofs1+j+1;
		}
		nofs0 = nofs1;
	}
	if (!outside)
		for (i = 0; i < nIdx/3; i += 3)
			tmp = Idx[i+1], Idx[i+1] = Idx[i+2], Idx[i+2] = tmp;

	D3DVERTEXBUFFERDESC vbd = 
	{ sizeof(D3DVERTEXBUFFERDESC), vbMemCaps | D3DVBCAPS_WRITEONLY, FVF_2TEX, (DWORD)nVtx };
	d3d->CreateVertexBuffer (&vbd, &mesh.vb, 0);
	LPVOID data;
	mesh.vb->Lock (DDLOCK_WAIT | DDLOCK_WRITEONLY | DDLOCK_DISCARDCONTENTS, (LPVOID*)&data, NULL);
	memcpy (data, Vtx, nVtx*sizeof(VERTEX_2TEX));
	mesh.vb->Unlock();
	mesh.vb->Optimize (dev, 0);

	if (store_vtx) {
		mesh.vtx = Vtx;
	} else {
		delete []Vtx;
		mesh.vtx = 0;
	}
	mesh.nv  = nVtx;
	mesh.idx = Idx;
	mesh.ni  = nIdx;

	// set bounding box
	static D3DVERTEXBUFFERDESC bbvbd ={ sizeof(D3DVERTEXBUFFERDESC), D3DVBCAPS_SYSTEMMEMORY, D3DFVF_XYZ, 8 };
	VERTEX_XYZ *V;
	d3d->CreateVertexBuffer (&bbvbd, &mesh.bb, 0);
	mesh.bb->Lock (DDLOCK_WAIT | DDLOCK_WRITEONLY | DDLOCK_DISCARDCONTENTS, (LPVOID*)&V, NULL);

	if (shift_origin) {
		pref.x -= dx;
		pref.y -= dy;
	}

	// transform bounding box back to patch coordinates
	pos = tmul (R, _V(tpmin.x, tpmin.y, tpmin.z)) + pref;
	V[0].x = D3DVAL(pos.x); V[0].y = D3DVAL(pos.y); V[0].z = D3DVAL(pos.z);
	pos = tmul (R, _V(tpmax.x, tpmin.y, tpmin.z)) + pref;
	V[1].x = D3DVAL(pos.x); V[1].y = D3DVAL(pos.y); V[1].z = D3DVAL(pos.z);
	pos = tmul (R, _V(tpmin.x, tpmax.y, tpmin.z)) + pref;
	V[2].x = D3DVAL(pos.x); V[2].y = D3DVAL(pos.y); V[2].z = D3DVAL(pos.z);
	pos = tmul (R, _V(tpmax.x, tpmax.y, tpmin.z)) + pref;
	V[3].x = D3DVAL(pos.x); V[3].y = D3DVAL(pos.y); V[3].z = D3DVAL(pos.z);
	pos = tmul (R, _V(tpmin.x, tpmin.y, tpmax.z)) + pref;
	V[4].x = D3DVAL(pos.x); V[4].y = D3DVAL(pos.y); V[4].z = D3DVAL(pos.z);
	pos = tmul (R, _V(tpmax.x, tpmin.y, tpmax.z)) + pref;
	V[5].x = D3DVAL(pos.x); V[5].y = D3DVAL(pos.y); V[5].z = D3DVAL(pos.z);
	pos = tmul (R, _V(tpmin.x, tpmax.y, tpmax.z)) + pref;
	V[6].x = D3DVAL(pos.x); V[6].y = D3DVAL(pos.y); V[6].z = D3DVAL(pos.z);
	pos = tmul (R, _V(tpmax.x, tpmax.y, tpmax.z)) + pref;
	V[7].x = D3DVAL(pos.x); V[7].y = D3DVAL(pos.y); V[7].z = D3DVAL(pos.z);
	mesh.bb->Unlock ();
}

// ==============================================================

void DestroyVBMesh (VBMESH &mesh)
{
	mesh.vb->Release();
	mesh.vb  = 0;
	mesh.nv  = 0;
	if (mesh.bb) {
		mesh.bb->Release();
		mesh.bb  = 0;
	}
	delete []mesh.idx;
	mesh.idx = 0;
	if (mesh.vtx) {
		delete []mesh.vtx;
		mesh.vtx = 0;
	}
	mesh.ni  = 0;
}

