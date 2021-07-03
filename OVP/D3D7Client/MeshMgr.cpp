// ==============================================================
//   ORBITER VISUALISATION PROJECT (OVP)
//   D3D7 Client module
//   Copyright (C) 2006-2016 Martin Schweiger
//   Dual licensed under GPL v3 and LGPL v3
// ==============================================================

// ==============================================================
// MeshMgr.cpp
// class MeshManager (implementation)
//
// Simple management of persistent mesh templates
// ==============================================================

#include "Meshmgr.h"

using namespace oapi;

MeshManager::MeshManager (const D3D7Client *gclient)
{
	gc = gclient;
	nmlist = nmlistbuf = 0;
}

MeshManager::~MeshManager ()
{
	Flush();
}

void MeshManager::Flush ()
{
	int i;
	for (i = 0; i < nmlist; i++)
		delete mlist[i].mesh;
	if (nmlistbuf) {
		delete []mlist;
		nmlist = nmlistbuf = 0;
	}
}

void MeshManager::StoreMesh (MESHHANDLE hMesh)
{
	if (GetMesh (hMesh)) return; // mesh already stored

	if (nmlist == nmlistbuf) { // need to allocate buffer
		MeshBuffer *tmp = new MeshBuffer[nmlistbuf += 32];
		if (nmlist) {
			memcpy (tmp, mlist, nmlist*sizeof(MeshBuffer));
			delete []mlist;
		}
		mlist = tmp;
	}
	mlist[nmlist].hMesh = hMesh;
	mlist[nmlist].mesh = new D3D7Mesh (gc, hMesh, true);
	nmlist++;
}

const D3D7Mesh *MeshManager::GetMesh (MESHHANDLE hMesh)
{
	int i;
	for (i = 0; i < nmlist; i++)
		if (mlist[i].hMesh == hMesh) return mlist[i].mesh;

	return NULL;
}