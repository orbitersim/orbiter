// ==============================================================
// MeshMgr.cpp
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Released under GNU General Public License
// Copyright (C) 2007 Martin Schweiger
// ==============================================================

// ==============================================================
// class MeshManager (implementation)
// Simple management of persistent mesh templates
// ==============================================================

#include "Meshmgr.h"

using namespace oapi;

MeshManager::MeshManager(D3D9Client *gclient)
{
	gc = gclient;
	mlist = NULL;
	nmlist = nmlistbuf = 0;
}

MeshManager::~MeshManager()
{
	DeleteAll();
	LogAlw("MeshManager Deleted");
}

void MeshManager::DeleteAll()
{
	__TRY {
		int i;
		for (i=0;i<nmlist;i++) delete mlist[i].mesh;
		if (nmlistbuf) {
			delete []mlist;
			nmlist = nmlistbuf = 0;
		}
	}
	__EXCEPT(ExcHandler(GetExceptionInformation()))
	{
		LogErr("Exception in MeshManager::DeleteAll()");
		FatalAppExitA(0,"Critical error has occured. See Orbiter.log for details");
	}
}

int MeshManager::StoreMesh(MESHHANDLE hMesh, const char *name)
{
	if (hMesh==NULL) {
		LogErr("NULL Mesh in MeshManager::StoreMesh()");
		return -1;
	}

	if (GetMesh(hMesh)) return -1; // mesh already stored

	if (nmlist==nmlistbuf) { // need to allocate buffer
		MeshBuffer *tmp = new MeshBuffer[nmlistbuf += 32];
		if (nmlist) {
			memcpy2 (tmp, mlist, nmlist*sizeof(MeshBuffer));
			delete []mlist;
		}
		mlist = tmp;
	}
	mlist[nmlist].hMesh = hMesh;
	mlist[nmlist].mesh = new D3D9Mesh(gc, hMesh, true);
	mlist[nmlist].mesh->SetName(name);
	nmlist++;

	float lim = 1e3;
	DWORD count = mlist[nmlist-1].mesh->GroupCount();

	for (DWORD i=0;i<count;i++) {
		D3DXVECTOR3 s = mlist[nmlist-1].mesh->GetGroupSize(i);
		if (fabs(s.x)>lim || fabs(s.y)>lim || fabs(s.z)>lim) return i;
	}

	return -1;
}

const D3D9Mesh *MeshManager::GetMesh (MESHHANDLE hMesh)
{
	int i;
	for (i=0;i<nmlist;i++) if (mlist[i].hMesh==hMesh) return mlist[i].mesh;
	// Should we store the mesh here ??
	return NULL;
}

void MeshManager::UpdateMesh (MESHHANDLE hMesh)
{
	int i;
	for (i=0;i<nmlist;i++) if (mlist[i].hMesh==hMesh) {
		if (mlist[i].mesh) {
			DWORD ngrp = mlist[i].mesh->GroupCount();
			for (DWORD k=0;k<ngrp;k++) {
				MESHGROUPEX *mg = oapiMeshGroupEx(hMesh, k);
				if (mg) mlist[i].mesh->UpdateGroupEx(k, mg);
			}
			DWORD nMtrl = oapiMeshMaterialCount(hMesh);
			for (DWORD k=0;k<nMtrl;k++)	{
				UpdateMatExt((const D3DMATERIAL9 *)oapiMeshMaterial(hMesh, k), mlist[i].mesh->GetMaterial(k)); 
			}
		}
		break;
	}
}