// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//   ORBITER VISUALISATION PROJECT (OVP)
//   D3D7 Client module
// ==============================================================

// ==============================================================
// spherepatch.h
// Create meshes for spheres and sphere patches
// ==============================================================

#ifndef __SPHEREPATCH_H
#define __SPHEREPATCH_H

#include "D3D7Client.h"
#include "D3D7Util.h"

struct VBMESH {
	VBMESH();
	~VBMESH();
	void MapVertices (LPDIRECT3D7 d3d, LPDIRECT3DDEVICE7 dev, DWORD MemFlag); // copy vertices from vtx to vb
	LPDIRECT3DVERTEXBUFFER7 vb; // mesh vertex buffer
	LPDIRECT3DVERTEXBUFFER7 bb; // bounding box vertex buffer
	VERTEX_2TEX *vtx;           // separate storage of vertices (NULL if not available)
	VECTOR4 *bbvtx;             // bounding box vertices
	DWORD nv;                   // number of vertices
	LPWORD idx;                 // list of indices
	DWORD ni;                   // number of indices
};

void CreateSphere (const oapi::D3D7Client *gclient, VBMESH &mesh, DWORD nrings, bool hemisphere, int which_half, int texres);

void CreateSpherePatch (const oapi::D3D7Client *gclient, VBMESH &mesh, int nlng, int nlat, int ilat, int res, int bseg = -1,
	bool reduce = true, bool outside = true, bool store_vtx = false, bool shift_origin = false);

void DestroyVBMesh (VBMESH &mesh);

#endif // !__SPHEREPATCH_H