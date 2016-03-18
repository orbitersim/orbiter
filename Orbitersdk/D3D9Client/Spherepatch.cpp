// ==============================================================
//   ORBITER VISUALISATION PROJECT (OVP)
//   Copyright (C) 2006-2016 Martin Schweiger
//   Dual licensed under GPL v3 and LGPL v3
// ==============================================================

// ==============================================================
// spherepatch.cpp
// Create meshes for spheres and sphere patches
// ==============================================================

#include "Spherepatch.h"
#include "AABBUtil.h"
#include "TileMgr2.h"

static float TEX2_MULTIPLIER = 4.0f; // microtexture multiplier

// ==============================================================
// struct VBMESH

VBMESH::VBMESH (class TileManager2Base *pmgr)
{
	pIB = NULL;
	pVB = NULL;
	idx = NULL;
	vtx = NULL;	
	nv  = 0;
	nf  = 0;
	bBox = false;
	nv_cur = 0;
	nf_cur = 0;
	pMgr = pmgr;
	bsRad = 0.0;
}

VBMESH::VBMESH ()
{
	pIB = NULL;
	pVB = NULL;
	idx = NULL;
	vtx = NULL;	
	nv  = 0;
	nf  = 0;
	bBox = false;
	pMgr = NULL;
	nv_cur = 0;
	nf_cur = 0;
	bsRad = 0.0;
}

VBMESH::~VBMESH ()
{
	if (pMgr) {
		pMgr->RecycleVertexBuffer(0, &pVB);
		pMgr->RecycleIndexBuffer(0, &pIB);
	} else {
		SAFE_RELEASE(pVB);
		SAFE_RELEASE(pIB);
	}
	SAFE_DELETEA(vtx);
	SAFE_DELETEA(idx);
	nv_cur = 0;
	nf_cur = 0;
}



void VBMESH::MapVertices(LPDIRECT3DDEVICE9 pDev, DWORD MemFlag)
{
	if (nv!=nv_cur && vtx) {
		// Resize Vertex Buffer
		if (pMgr) {
			nv_cur = pMgr->RecycleVertexBuffer(nv, &pVB);
		} else {
			SAFE_RELEASE(pVB);
			HR(pDev->CreateVertexBuffer(nv*sizeof(VERTEX_2TEX), D3DUSAGE_DYNAMIC|D3DUSAGE_WRITEONLY, 0, D3DPOOL_DEFAULT, &pVB, NULL));
			nv_cur = nv;
		}
	}

	if (nf!=nf_cur && idx) {
		// Resize Index Buffer
		if (pMgr) {
			nf_cur = pMgr->RecycleIndexBuffer(nf, &pIB);
		} else {
			SAFE_RELEASE(pIB);
			HR(pDev->CreateIndexBuffer(nf*sizeof(WORD)*3, D3DUSAGE_DYNAMIC|D3DUSAGE_WRITEONLY, D3DFMT_INDEX16, D3DPOOL_DEFAULT, &pIB, NULL));
			nf_cur = nf;
		}
	}

	VERTEX_2TEX *pVBuffer;
	WORD *pIBuffer;

	if (vtx) {

		HR(D3DXComputeBoundingSphere((const D3DXVECTOR3 *)&vtx->x, nv, sizeof(VERTEX_2TEX), &bsCnt, &bsRad));

		if (pVB) {
			//if (pVB->Lock(0, 0, (LPVOID*)&pVBuffer, D3DLOCK_DISCARD)==S_OK) {
			if (HROK(pVB->Lock(0, 0, (LPVOID*)&pVBuffer, 0))) {
				memcpy(pVBuffer, vtx, nv*sizeof(VERTEX_2TEX));
				pVB->Unlock();
			}
		} else LogErr("Failed to create vertex buffer");
	}

	if (idx) {
		if (pIB) {
			//if (pIB->Lock(0, 0, (LPVOID*)&pIBuffer, D3DLOCK_DISCARD)==S_OK) {
			if (HROK(pIB->Lock(0, 0, (LPVOID*)&pIBuffer, 0))) {
				memcpy(pIBuffer, idx, nf*sizeof(WORD)*3);
				pIB->Unlock();
			}
		} else LogErr("Failed to create index buffer");
	}
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

void CreateSphere (LPDIRECT3DDEVICE9 pDev, VBMESH &mesh, DWORD nrings, bool hemisphere, int which_half, int texres)
{
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

	mesh.nv  = nVtx;
	mesh.nf  = nIdx/3;
	mesh.vtx = Vtx;
	mesh.idx = Idx;
	mesh.MapVertices(pDev);
	
	delete []Vtx;
	delete []Idx;
	mesh.vtx = NULL;
	mesh.idx = NULL;
}

// ==============================================================

void CreateSpherePatch (LPDIRECT3DDEVICE9 pDev, VBMESH &mesh, int nlng, int nlat, int ilat, int res, int bseg,
	bool reduce, bool outside, bool store_vtx, bool shift_origin)
{

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
	WORD *Idx = new WORD[nIdx]();

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

	mesh.nv  = nVtx;
	mesh.nf  = nIdx/3;
	mesh.vtx = Vtx;
	mesh.idx = Idx;
	mesh.MapVertices(pDev);

	delete []Vtx;
	delete []Idx;

	mesh.vtx = NULL;
	mesh.idx = NULL;	

	if (shift_origin) {
		pref.x -= dx;
		pref.y -= dy;
	}

	// transform bounding box back to patch coordinates
	mesh.Box[0] = _V(tmul (R, _V(tpmin.x, tpmin.y, tpmin.z)) + pref);
	mesh.Box[1] = _V(tmul (R, _V(tpmax.x, tpmin.y, tpmin.z)) + pref);
	mesh.Box[2] = _V(tmul (R, _V(tpmin.x, tpmax.y, tpmin.z)) + pref);
	mesh.Box[3] = _V(tmul (R, _V(tpmax.x, tpmax.y, tpmin.z)) + pref);
	mesh.Box[4] = _V(tmul (R, _V(tpmin.x, tpmin.y, tpmax.z)) + pref);
	mesh.Box[5] = _V(tmul (R, _V(tpmax.x, tpmin.y, tpmax.z)) + pref);
	mesh.Box[6] = _V(tmul (R, _V(tpmin.x, tpmax.y, tpmax.z)) + pref);
	mesh.Box[7] = _V(tmul (R, _V(tpmax.x, tpmax.y, tpmax.z)) + pref);
}



// ====================================================================
// NOTE: This is used to delete a vertex buffers from a static VBMESH
//
void DestroyVBMesh (VBMESH &mesh)
{
	if (mesh.pMgr) {
		mesh.pMgr->RecycleVertexBuffer(0, &mesh.pVB);
		mesh.pMgr->RecycleIndexBuffer(0, &mesh.pIB);
	} else {
		SAFE_RELEASE(mesh.pVB);
		SAFE_RELEASE(mesh.pIB);
	}

	SAFE_DELETEA(mesh.vtx);
	SAFE_DELETEA(mesh.idx);

	mesh.nv = 0;
	mesh.nf = 0;
	mesh.nv_cur = 0;
	mesh.nf_cur = 0;
}

