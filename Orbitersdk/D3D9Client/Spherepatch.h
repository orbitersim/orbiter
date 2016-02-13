// ==============================================================
//   ORBITER VISUALISATION PROJECT (OVP)
//   Copyright (C) 2006-2016 Martin Schweiger
//   Dual licensed under GPL v3 and LGPL v3
// ==============================================================

// ==============================================================
// spherepatch.h
// Create meshes for spheres and sphere patches
// ==============================================================

#ifndef __SPHEREPATCH_H
#define __SPHEREPATCH_H

#include "D3D9Client.h"
#include "D3D9Util.h"

struct VBMESH {

	explicit VBMESH(class TileManager2Base *pMgr);
	VBMESH();
	~VBMESH();

	void MapVertices (LPDIRECT3DDEVICE9 dev, DWORD MemFlag=0); // copy vertices from vtx to vb

	LPDIRECT3DVERTEXBUFFER9 pVB;	// mesh vertex buffer
	LPDIRECT3DINDEXBUFFER9  pIB;	// mesh index buffer

	class TileManager2Base * pMgr; 
	VERTEX_2TEX *vtx;				// separate storage of vertices (NULL if not available)
	WORD *idx;						// list of indices
	DWORD nv;						// number of vertices
	DWORD nf;						// number of faces (number of indices/3)
	DWORD nv_cur;					
	DWORD nf_cur;					
	VECTOR4 Box[8];					// bounding box vertices
	D3DXVECTOR3 bsCnt;				// bounding sphere position
	float bsRad;					// bounding sphere radius
	bool bBox;						// true if bouinding box data is valid
};

void CreateSphere(LPDIRECT3DDEVICE9 pDev, VBMESH &mesh, DWORD nrings, bool hemisphere, int which_half, int texres);
void CreateSpherePatch(LPDIRECT3DDEVICE9 pDev, VBMESH &mesh, int nlng, int nlat, int ilat, int res, int bseg = -1, bool reduce = true, bool outside = true, bool store_vtx = false, bool shift_origin = false);
void DestroyVBMesh (VBMESH &mesh);

#endif // !__SPHEREPATCH_H