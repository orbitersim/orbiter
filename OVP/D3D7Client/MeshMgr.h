// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//   ORBITER VISUALISATION PROJECT (OVP)
//   D3D7 Client module
// ==============================================================

// ==============================================================
// MeshMgr.h
// Mesh management and storage
// ==============================================================

#ifndef __MESHMGR_H
#define __MESHMGR_H

#include "D3D7Client.h"
#include "Mesh.h"

// ==============================================================
// class MeshManager (interface)
// ==============================================================
/**
 * \brief Simple management of persistent mesh templates
 */

class MeshManager {
public:
	MeshManager (const oapi::D3D7Client *gclient);
	~MeshManager();
	void Flush();
	void StoreMesh (MESHHANDLE hMesh);
	const D3D7Mesh *GetMesh (MESHHANDLE hMesh);

private:
	const oapi::D3D7Client *gc;
	struct MeshBuffer {
		MESHHANDLE hMesh;
		D3D7Mesh *mesh;
	} *mlist;
	int nmlist, nmlistbuf;
};

#endif // !__MESHMGR_H