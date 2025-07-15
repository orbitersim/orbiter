// ==============================================================
// MeshMgr.h
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2006-2016 Martin Schweiger
// ==============================================================

#ifndef __MESHMGR_H
#define __MESHMGR_H

#include "Client.h"
#include "Mesh.h"

// ==============================================================
// class MeshManager (interface)
// ==============================================================
/**
 * \brief Simple management of persistent mesh templates
 */

class MeshManager {
public:
	explicit MeshManager (oapi::vkClient *gclient);
	~MeshManager();
	void DeleteAll();
	int StoreMesh (MESHHANDLE hMesh, const char *name);
	const vkMesh *GetMesh (MESHHANDLE hMesh);

private:
	oapi::vkClient *gc;
	struct MeshBuffer {
		MESHHANDLE hMesh;
		vkMesh *mesh;
	} *mlist;
	int nmlist, nmlistbuf;
};

#endif // !__MESHMGR_H
