// ==============================================================
// Mesh.cpp
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2006 Martin Schweiger
//				 2010-2012 Jarmo Nikkanen (D3D9Client implementation)
// ==============================================================

#define VISIBILITY_TOL 0.0015f

#include "Mesh.h"
#include "Log.h"
#include "Scene.h"
#include "D3D9Surface.h"
#include "D3D9Catalog.h"
#include "D3D9Config.h"
#include "DebugControls.h"

#define FLAG_INSTANCED 0x10000000

using namespace oapi;

// ===========================================================================================
//
void D3D9Mesh::Null()
{
	pVB = NULL;
	pIB = NULL;
	pGB = NULL;
	pGI = NULL;
	Geom = NULL;
	nGeom = 0;
	sunLight = NULL;
	cAmbient = 0;
	MaxFace  = 0;
	MaxVert  = 0;
	bTemplate = false;
	bGlobalTF = false;
	bBSRecompute = true;
	bBSRecomputeAll = true;
	bModulateMatAlpha = false;
	strcpy_s(name, 128, "???");
}

// ===========================================================================================
//
D3D9Mesh::D3D9Mesh(D3D9Client *client, MESHHANDLE hMesh, bool asTemplate) : D3D9Effect()
{
	Null();
	gc = client;
	bTemplate = asTemplate;
	nGrp = oapiMeshGroupCount(hMesh);

	if (nGrp==0) { Null(); return; }

	Grp = new GROUPREC*[nGrp];
	
	memset2(Grp, 0, sizeof(GROUPREC*) * nGrp);

	for (DWORD i=0;i<nGrp;i++) {
		Grp[i] = new GROUPREC;	memset2(Grp[i], 0, sizeof(GROUPREC));
		MESHGROUPEX *mg = oapiMeshGroupEx(hMesh, i);
		memcpy2(Grp[i]->TexIdxEx, mg->TexIdxEx, MAXTEX*sizeof(DWORD));
		memcpy2(Grp[i]->TexMixEx, mg->TexMixEx, MAXTEX*sizeof(float));
		Grp[i]->TexIdx  = mg->TexIdx;
		Grp[i]->MtrlIdx = mg->MtrlIdx;
		Grp[i]->FaceOff = MaxFace;
		Grp[i]->VertOff = MaxVert;
		Grp[i]->nFace   = mg->nIdx/3;
		Grp[i]->nVert   = mg->nVtx;
		MaxFace += Grp[i]->nFace;
		MaxVert += Grp[i]->nVert;
	}

	if (MaxVert==0 || MaxFace==0) { Null(); return; }

	// template meshes are stored in system memory

	D3DPOOL Pool = D3DPOOL_MANAGED;
	DWORD MeshOptions = D3DUSAGE_WRITEONLY;
	if (asTemplate) MeshOptions = 0, Pool = D3DPOOL_SYSTEMMEM;

	HR(gc->GetDevice()->CreateVertexBuffer(MaxVert*sizeof(NMVERTEX), 0, 0, D3DPOOL_MANAGED, &pVB, NULL));
	HR(gc->GetDevice()->CreateIndexBuffer(MaxFace*sizeof(WORD)*3, 0, D3DFMT_INDEX16, D3DPOOL_MANAGED, &pIB, NULL));

	nTex = oapiMeshTextureCount(hMesh)+1;
	Tex = new LPD3D9CLIENTSURFACE[nTex];
	Tex[0] = 0; // 'no texture'

	for (DWORD i=1;i<nTex;i++) Tex[i] = SURFACE(oapiGetTextureHandle(hMesh, i));

	nMtrl = oapiMeshMaterialCount(hMesh);
	if (nMtrl) Mtrl = new D3D9MatExt[nMtrl];

	for (DWORD i=0;i<nMtrl;i++)	CopyMaterial(i, oapiMeshMaterial(hMesh, i));

	ProcessInherit();

	for (DWORD i=0;i<nGrp;i++) {
		MESHGROUPEX *mg = oapiMeshGroupEx(hMesh, i);
		CopyGroupEx(Grp[i], mg, i);
	}

	pGrpTF = new D3DXMATRIX[nGrp];

	D3DXMatrixIdentity(&mTransform);
	D3DXMatrixIdentity(&mTransformInv);
	MeshCatalog->Add(DWORD(this));

	UpdateBoundingBox();
	CheckValidity();
	CreateGeometryBuffers();
}

// ===========================================================================================
//
D3D9Mesh::D3D9Mesh(D3D9Client *client, DWORD groups, const MESHGROUPEX **hGroup, const SURFHANDLE *hSurf) : D3D9Effect()
{
	Null();
	gc = client;
	nGrp = groups;
	Grp = new GROUPREC*[nGrp];

	memset2(Grp, 0, sizeof(GROUPREC*) * nGrp);

	for (DWORD i=0;i<nGrp;i++) {
		Grp[i] = new GROUPREC;	memset2(Grp[i], 0, sizeof(GROUPREC));
		Grp[i]->TexIdxEx[0] = SPEC_DEFAULT;
		Grp[i]->TexMixEx[0] = 0.0f;
		Grp[i]->TexIdx  = i;
		Grp[i]->MtrlIdx = SPEC_DEFAULT;
		Grp[i]->FaceOff = MaxFace;
		Grp[i]->VertOff = MaxVert;
		Grp[i]->nFace   = hGroup[i]->nIdx/3;
		Grp[i]->nVert   = hGroup[i]->nVtx;
		MaxFace += Grp[i]->nFace;
		MaxVert += Grp[i]->nVert;
	}

	HR(gc->GetDevice()->CreateVertexBuffer(MaxVert*sizeof(NMVERTEX), 0, 0, D3DPOOL_MANAGED, &pVB, NULL));
	HR(gc->GetDevice()->CreateIndexBuffer(MaxFace*sizeof(WORD)*3, 0, D3DFMT_INDEX16, D3DPOOL_MANAGED, &pIB, NULL));

	nMtrl = 0;
	nTex = nGrp+1;
	Tex = new LPD3D9CLIENTSURFACE[nTex];
	Tex[0] = 0; // 'no texture'

	for (DWORD i=1;i<nTex;i++) Tex[i] = SURFACE(hSurf[i-1]);

	ProcessInherit();

	for (DWORD i=0;i<nGrp;i++) CopyGroupEx(Grp[i], hGroup[i], i);

	pGrpTF = new D3DXMATRIX[nGrp];

	D3DXMatrixIdentity(&mTransform);
	D3DXMatrixIdentity(&mTransformInv);
	MeshCatalog->Add(DWORD(this));

	UpdateBoundingBox();
	CheckValidity();
	CreateGeometryBuffers();
}



// ===========================================================================================
//
D3D9Mesh::D3D9Mesh(D3D9Client *client, const MESHGROUPEX *pGroup, const MATERIAL *pMat, D3D9ClientSurface *pTex) : D3D9Effect()
{
	Null();
	gc = client;

	// template meshes are stored in system memory
	nGrp   = 1;
	Grp    = new GROUPREC*[nGrp];
	Grp[0] = new GROUPREC; memset2(Grp[0], 0, sizeof(GROUPREC));
	nTex   = 2;
	Tex	   = new LPD3D9CLIENTSURFACE[nTex];
	Tex[0] = 0; // 'no texture'
	Tex[1] = pTex;
	nMtrl  = 1;
	Mtrl   = new D3D9MatExt[nMtrl];
	pGrpTF = new D3DXMATRIX[nGrp];

	memcpy2(Grp[0]->TexIdxEx, pGroup->TexIdxEx, MAXTEX*sizeof(DWORD));
	memcpy2(Grp[0]->TexMixEx, pGroup->TexMixEx, MAXTEX*sizeof(float));

	Grp[0]->TexIdx  = pGroup->TexIdx;
	Grp[0]->MtrlIdx = pGroup->MtrlIdx;
	Grp[0]->VertOff = 0;
	Grp[0]->FaceOff = 0;
	Grp[0]->nFace   = pGroup->nIdx/3;
	Grp[0]->nVert   = pGroup->nVtx;

	MaxFace = Grp[0]->nFace;
	MaxVert = Grp[0]->nVert;

	HR(gc->GetDevice()->CreateVertexBuffer(MaxVert*sizeof(NMVERTEX), 0, 0, D3DPOOL_MANAGED, &pVB, NULL));
	HR(gc->GetDevice()->CreateIndexBuffer(MaxFace*sizeof(WORD)*3, 0, D3DFMT_INDEX16, D3DPOOL_MANAGED, &pIB, NULL));

	CopyMaterial(0, pMat);
	CopyGroupEx(Grp[0], pGroup, 0);

	D3DXMatrixIdentity(&mTransform);
	D3DXMatrixIdentity(&mTransformInv);
	MeshCatalog->Add(DWORD(this));

	UpdateBoundingBox();
	CheckValidity();
	CreateGeometryBuffers();
}


// ===========================================================================================
//
D3D9Mesh::D3D9Mesh(const D3D9Mesh &mesh) : D3D9Effect()
{
	Null();
	// note: 'mesh' must be a template mesh, because we may not be able to
	// access vertex data in video memory
	bModulateMatAlpha = mesh.bModulateMatAlpha;
	strcpy_s(name, 128, mesh.name);

	nGrp = mesh.nGrp;
	Grp = new GROUPREC*[nGrp];
	pGrpTF = new D3DXMATRIX[nGrp];

	MaxFace = mesh.MaxFace;
	MaxVert = mesh.MaxVert;

	for (DWORD i=0;i<nGrp;i++) {
		Grp[i] = new GROUPREC;
		memcpy2(Grp[i], mesh.Grp[i], sizeof(GROUPREC));
	}

	if (mesh.Geom) {
		nGeom = mesh.nGeom;
		Geom = new GEOMREC[nGeom];
		memcpy2(Geom, mesh.Geom, nGeom*sizeof(GEOMREC));
	}

	HR(gc->GetDevice()->CreateVertexBuffer(MaxVert*sizeof(NMVERTEX), 0, 0, D3DPOOL_MANAGED, &pVB, NULL));
	HR(gc->GetDevice()->CreateIndexBuffer(MaxFace*sizeof(WORD)*3, 0, D3DFMT_INDEX16, D3DPOOL_MANAGED, &pIB, NULL));
	

	// ----------------------------------------------------------------

	LPVOID pVSrc, pVTgt;
	LPVOID pISrc, pITgt;

	HR(mesh.pIB->Lock(0, 0, &pISrc, 0));
	HR(pIB->Lock(0, 0, &pITgt, 0));
	memcpy2(pITgt, pISrc, MaxFace*6);
	HR(mesh.pIB->Unlock());
	HR(pIB->Unlock());
	

	HR(mesh.pVB->Lock(0, 0, (LPVOID*)&pVSrc, 0));
	HR(pVB->Lock(0, 0, (LPVOID*)&pVTgt, 0));
	memcpy2(pVTgt, pVSrc, MaxVert*sizeof(NMVERTEX));
	HR(mesh.pVB->Unlock());
	HR(pVB->Unlock());

	// ----------------------------------------------------------------

	if (mesh.pGI && mesh.pGB) {

		// Create Geometry Buffers
		HR(pDev->CreateVertexBuffer(MaxVert*sizeof(D3DXVECTOR4), 0, 0, D3DPOOL_MANAGED, &pGB, NULL));
		HR(pDev->CreateIndexBuffer(MaxFace*sizeof(DWORD)*3, 0, D3DFMT_INDEX32, D3DPOOL_MANAGED, &pGI, NULL));

		HR(mesh.pGI->Lock(0, 0, &pISrc, 0));
		HR(pGI->Lock(0, 0, &pITgt, 0));
		memcpy2(pITgt, pISrc, MaxFace*12);
		HR(mesh.pGI->Unlock());
		HR(pGI->Unlock());
		
		HR(mesh.pGB->Lock(0, 0, (LPVOID*)&pVSrc, 0));
		HR(pGB->Lock(0, 0, (LPVOID*)&pVTgt, 0));
		memcpy2(pVTgt, pVSrc, MaxVert*sizeof(D3DXVECTOR4));
		HR(mesh.pGB->Unlock());
		HR(pGB->Unlock());
	}

	// ----------------------------------------------------------------

	nTex = mesh.nTex;
	Tex = new LPD3D9CLIENTSURFACE[nTex];

	for (DWORD i=0;i<nTex;i++) Tex[i] = mesh.Tex[i];

	nMtrl = mesh.nMtrl;
	if (nMtrl) Mtrl = new D3D9MatExt[nMtrl];
	memcpy2 (Mtrl, mesh.Mtrl, nMtrl*sizeof(D3D9MatExt));

	mTransform = mesh.mTransform;
	mTransformInv = mesh.mTransformInv;
	bGlobalTF = mesh.bGlobalTF;

	MeshCatalog->Add(DWORD(this));

	UpdateBoundingBox();
}

// ===========================================================================================
//
D3D9Mesh::~D3D9Mesh()
{
	_TRACE;
	if (!pVB) return;

	if (MeshCatalog->Remove(DWORD(this))) LogAlw("Mesh 0x%X Removed from catalog",this);
	else 								  LogErr("Mesh 0x%X wasn't in meshcatalog",this);

	if (nGrp && Grp) for (DWORD g=0;g<nGrp;g++) delete Grp[g];

	if (Grp) delete []Grp;
	if (nTex) delete []Tex;
	if (Geom) delete []Geom;
	if (nMtrl) delete []Mtrl;
	if (pGrpTF) delete []pGrpTF;

	if (pIB) pIB->Release();
	if (pVB) pVB->Release();
	if (pGB) pGB->Release();
	if (pGI) pGI->Release();

	pIB = NULL;
	pVB = NULL;
	pGB = NULL;
	pGI = NULL;

	LogOk("Mesh 0x%X Deleted successfully -------------------------------",this);
}

// ===========================================================================================
//
void D3D9Mesh::SetName(const char *fname)
{
	if (fname) strcpy_s(name,128,fname);
	if (!pVB) LogErr("No vertices in a mesh [%s]. Invalid Mesh",name);
}

// ===========================================================================================
//
bool D3D9Mesh::HasShadow()
{
	if (!pVB) return false;
	for (DWORD g=0; g<nGrp; g++) {
		if (Grp[g]->UsrFlag & 3) continue;
		if (Grp[g]->IntFlag & 3) continue;
		return true;
	}
	return false;
}

// ===========================================================================================
//
void D3D9Mesh::CheckValidity()
{
	_TRACE;
	if (!pVB) return;
	if (Constr!=5) {
		float lim = 5e3;
		for (DWORD i=0;i<nGrp;i++) {
			D3DXVECTOR3 s = Grp[i]->BBox.max - Grp[i]->BBox.min;
			if (fabs(s.x)>lim || fabs(s.y)>lim || fabs(s.z)>lim) {
				LogWrn("D3D9Mesh(0x%X) Has a large group (%0.0fm x %0.0fm x %0.0fm) idx=%u/%u nVert=%u, nFace=%u", this, s.x, s.y, s.z, i, nGrp-1, Grp[i]->nVert, Grp[i]->nFace);
			}
		}
	}

	for (DWORD i=0;i<nGrp;i++) {
		if (Grp[i]->nVert==0) LogWrn("MeshGroup has no vertices");
		if (Grp[i]->nFace==0) LogWrn("MeshGroup has no faces");
		if (Grp[i]->BBox.bs.w<1e-3) LogWrn("Small Bounding Sphere rad=%g nVtx=%u", Grp[i]->BBox.bs.w, Grp[i]->nVert);
	}


	for (DWORD i=0;i<nGrp;i++) {

		if (Grp[i]->nVert==0) continue;
		if (Grp[i]->nFace==0) continue;

		NMVERTEX *pVrt = LockVertexBuffer(i);

		for (DWORD k=0;k<Grp[i]->nVert;k++) {
			D3DXVECTOR3 v = D3DXVECTOR3(pVrt[k].nx, pVrt[k].ny, pVrt[k].nz);
			float len = D3DXVec3Length(&v);
			if (fabs(len)<1e-3) {
				LogWrn("Zero length normals in mesh group %d in mesh 0x%X",i,this);
				break;
			}
		}
		UnLockVertexBuffer();
	}
}




// ===========================================================================================
//
void D3D9Mesh::ProcessInherit()
{
	_TRACE;
	if (!pVB) return;
	if (Grp[0]->MtrlIdx == SPEC_INHERIT) Grp[0]->MtrlIdx = SPEC_DEFAULT;
	if (Grp[0]->TexIdx == SPEC_INHERIT) Grp[0]->TexIdx = SPEC_DEFAULT;
	if (Grp[0]->TexIdxEx[0] == SPEC_INHERIT) Grp[0]->TexIdxEx[0] = SPEC_DEFAULT;

	bool bPopUp = false;

	for (DWORD i=0;i<nGrp;i++) {

		if (Grp[i]->UsrFlag & 0x8) LogErr("MeshGroupFlag 0x8 in use (OPERATION NOT IMPLEMENTED)");

		// Inherit Material
		if (Grp[i]->MtrlIdx == SPEC_INHERIT) Grp[i]->MtrlIdx = Grp[i-1]->MtrlIdx;

		// Inherit Texture
		if (Grp[i]->TexIdx == SPEC_DEFAULT) Grp[i]->TexIdx = 0;
		else if (Grp[i]->TexIdx == SPEC_INHERIT) Grp[i]->TexIdx = Grp[i-1]->TexIdx;
		else Grp[i]->TexIdx++;

		// Inherit Night Texture
		if (Grp[i]->TexIdxEx[0] == SPEC_DEFAULT) Grp[i]->TexIdxEx[0] = 0;
		else if (Grp[i]->TexIdxEx[0] == SPEC_INHERIT) Grp[i]->TexIdxEx[0] = Grp[i-1]->TexIdxEx[0];
		else Grp[i]->TexIdxEx[0]++;

		// Do some safety checks
		if (Grp[i]->TexIdx>=nTex) {
			LogErr("Mesh(0x%X) has a texture index %u in group %u out of range. Constr=%u", this, Grp[i]->TexIdx, i, Constr);
			Grp[i]->TexIdx = 0;
			bPopUp = true;
		}
		if (Grp[i]->TexIdxEx[0]>=nTex) {
			LogErr("Mesh(0x%X) has a night texture index %u in group %u out of range. Constr=%u", this, Grp[i]->TexIdxEx[0], i, Constr);
			Grp[i]->TexIdxEx[0] = 0;
			bPopUp = true;
		}

		if (Grp[i]->MtrlIdx!=SPEC_DEFAULT) {
			if (Grp[i]->MtrlIdx>=nMtrl) {
				LogErr("Mesh(0x%X) has a material index %u in group %u out of range. Constr=%u", this, Grp[i]->MtrlIdx, i, Constr);
				Grp[i]->MtrlIdx = SPEC_DEFAULT;
				bPopUp = true;
			}
		}
	}
	if (bPopUp) MessageBoxA(NULL, "Invalid Mesh Detected", "D3D9Client Error:",MB_OK);
}


// ===========================================================================================
// 
void D3D9Mesh::CreateGeometryBuffers()
{
	_TRACE;
	if (!pVB) return;

	DWORD size = 0;

	if (!Geom) {
		size = 3+nGrp/8; // Maximum theoretical size that could be needed
		Geom = new GEOMREC[size];
		memset2(Geom, 0, size*sizeof(GEOMREC));
	}

	bool bs = false;

	// Re-arrange a mesh groups -------------------------------------
	//
	for (DWORD g=0;g<nGrp;g++) {
		
		if (nGeom>=size) {
			LogErr("ERROR: Geometry buffer overflow (%u)", nGeom);
			SAFE_DELETEA(Geom);
			nGeom = 0;
			return;
		}

		if (Grp[g]->bGrouped) continue;

		bool bNoShadow = ((Grp[g]->UsrFlag&0x3)!=0) || ((Grp[g]->IntFlag&0x3)!=0);
		
		Geom[nGeom].bNoShadow = bs = bNoShadow;
		Geom[nGeom].nGrp      = 1;
		Geom[nGeom].GrpIdx[0] = (WORD)g;
		Geom[nGeom].nVert	  = Grp[g]->nVert;
		Geom[nGeom].nFace	  = Grp[g]->nFace;
		Geom[nGeom].bBroken   = false;

		Grp[g]->GeometryRec   = WORD(nGeom);
		Grp[g]->bGrouped      = true;

		for (DWORD w=g+1;w<nGrp;w++) {

			bool bNoShadow = ((Grp[w]->UsrFlag&0x3)!=0) || ((Grp[w]->IntFlag&0x3)!=0);

			if (bNoShadow == bs) {
				// Append the instance group
				Geom[nGeom].GrpIdx[Geom[nGeom].nGrp] = (WORD)w;
				Geom[nGeom].nGrp++;
				Geom[nGeom].nVert += Grp[w]->nVert;
				Geom[nGeom].nFace += Grp[w]->nFace;
				Grp[w]->GeometryRec = WORD(nGeom);
				Grp[w]->bGrouped = true;
			}
			if (Geom[nGeom].nGrp==8) break;
		}
		nGeom++;
	}

	if (!pGB) {
		HR(pDev->CreateVertexBuffer(MaxVert*sizeof(D3DXVECTOR4), 0, 0, D3DPOOL_MANAGED, &pGB, NULL));
		HR(pDev->CreateIndexBuffer(MaxFace*sizeof(DWORD)*3, 0, D3DFMT_INDEX32, D3DPOOL_MANAGED, &pGI, NULL));
	}

	NMVERTEX *pVSrc;
	D3DXVECTOR4 *pVTgt;
	WORD *pISrc;
	DWORD *pITgt;

	HR(pVB->Lock(0, 0, (LPVOID*)&pVSrc, 0));
	HR(pIB->Lock(0, 0, (LPVOID*)&pISrc, 0));
	HR(pGI->Lock(0, 0, (LPVOID*)&pITgt, 0));
	HR(pGB->Lock(0, 0, (LPVOID*)&pVTgt, 0));

	DWORD ti = 0, tv = 0;

	for (DWORD i=0;i<nGeom;i++) {

		Geom[i].FaceOff = (ti==0 ? 0 : ti/3);
		Geom[i].VertOff = tv;

		DWORD sV = 0;

		for (DWORD r=0;r<Geom[i].nGrp;r++) {

			DWORD gr = Geom[i].GrpIdx[r];
			DWORD nI = Grp[gr]->nFace*3;
			DWORD nV = Grp[gr]->nVert;
			DWORD oV = Grp[gr]->VertOff;
			DWORD oI = Grp[gr]->FaceOff*3;

			Grp[gr]->GeoVOff = tv;
			Grp[gr]->GeoBIdx = sV;
			Grp[gr]->GeoFOff = (ti==0 ? 0 : ti/3);

			for (DWORD v=0;v<nV;v++) {
				pVTgt[tv] = D3DXVECTOR4(pVSrc[oV+v].x, pVSrc[oV+v].y, pVSrc[oV+v].z, float(r&0x7)+0.001f);
				tv++;
			}
			for (DWORD x=0;x<nI;x++) {
				pITgt[ti] = DWORD(pISrc[oI+x]) + sV;
				ti++;
			}
			sV += nV;
		}
	}

	HR(pVB->Unlock());
	HR(pIB->Unlock());
	HR(pGI->Unlock());
	HR(pGB->Unlock());
}


// ===========================================================================================
//
void D3D9Mesh::UpdateGeometryBuffer()
{

	if (!pVB || !pGB || !pIB || !pGI) return;

	NMVERTEX *pVSrc = NULL;
	D3DXVECTOR4 *pVTgt = NULL;
	WORD *pISrc = NULL;
	DWORD *pITgt = NULL;

	HR(pVB->Lock(0, 0, (LPVOID*)&pVSrc, 0));
	HR(pIB->Lock(0, 0, (LPVOID*)&pISrc, 0));
	HR(pGI->Lock(0, 0, (LPVOID*)&pITgt, 0));
	HR(pGB->Lock(0, 0, (LPVOID*)&pVTgt, 0));

	if (!pVSrc || !pVTgt || !pISrc || !pITgt) {
		LogErr("UpdateGeometryBuffer() Failed to lock a buffer");
		return;
	}

	for (DWORD i=0;i<nGeom;i++) {
		for (DWORD r=0;r<Geom[i].nGrp;r++) {
			DWORD gr = Geom[i].GrpIdx[r];
			DWORD nV = Grp[gr]->nVert;
			for (DWORD v=0;v<nV;v++) {
				DWORD tv = Grp[gr]->GeoVOff + v;
				DWORD sv = Grp[gr]->VertOff + v;
				pVTgt[tv] = D3DXVECTOR4(pVSrc[sv].x, pVSrc[sv].y, pVSrc[sv].z, float(r&0x7)+0.001f);
			}
		}
	}

	HR(pVB->Unlock());
	HR(pIB->Unlock());
	HR(pGI->Unlock());
	HR(pGB->Unlock());
}



// ===========================================================================================
//
void D3D9Mesh::DynamicGroup(DWORD idx)
{
	if (!Geom) return;
	if (idx>=nGrp) return;
	WORD ir = Grp[idx]->GeometryRec;
	Geom[ir].bBroken = true;
}


// ===========================================================================================
//
D3DXVECTOR3 D3D9Mesh::GetGroupSize(DWORD idx)
{
	_TRACE;
	if (!pVB) return D3DXVECTOR3(0,0,0);
	if (idx>=nGrp) return D3DXVECTOR3(0,0,0);
	if (Grp[idx]->nVert<2) return D3DXVECTOR3(0,0,0);
	return D3DXVECTOR3f4(Grp[idx]->BBox.max - Grp[idx]->BBox.min);
}

// ===========================================================================================
//
void D3D9Mesh::ResetTransformations()
{
	_TRACE;
	if (!pVB) return;
	D3DXMatrixIdentity(&mTransform);
	D3DXMatrixIdentity(&mTransformInv);
	bGlobalTF = false;
	bBSRecompute = true;
	bBSRecomputeAll = true;
	for (DWORD i=0;i<nGrp;i++) {
		D3DXMatrixIdentity(&Grp[i]->Transform);
		D3DXMatrixIdentity(&pGrpTF[i]);
		Grp[i]->bTransform = false;
	}
}

// ===========================================================================================
//
void D3D9Mesh::UpdateTangentSpace(NMVERTEX *pVrt, WORD *pIdx, DWORD nVtx, DWORD nFace, bool bTextured)
{
	if (!pVB) return;

	if (bTextured) {

		D3DXVECTOR3 *ta = new D3DXVECTOR3[nVtx];

		for (DWORD i=0;i<nVtx;i++) ta[i] = D3DXVECTOR3(0,0,0);

		for (DWORD i=0;i<nFace;i++) {

			DWORD i0 = pIdx[i*3];
			DWORD i1 = pIdx[i*3+1];
			DWORD i2 = pIdx[i*3+2];

			D3DXVECTOR3 r0 = D3DXVECTOR3(pVrt[i0].x,  pVrt[i0].y,  pVrt[i0].z);
			D3DXVECTOR3 r1 = D3DXVECTOR3(pVrt[i1].x,  pVrt[i1].y,  pVrt[i1].z);
			D3DXVECTOR3 r2 = D3DXVECTOR3(pVrt[i2].x,  pVrt[i2].y,  pVrt[i2].z);
			D3DXVECTOR2 t0 = D3DXVECTOR2(pVrt[i0].u, pVrt[i0].v);
			D3DXVECTOR2 t1 = D3DXVECTOR2(pVrt[i1].u, pVrt[i1].v);
			D3DXVECTOR2 t2 = D3DXVECTOR2(pVrt[i2].u, pVrt[i2].v);

			float u0 = t1.x - t0.x;
			float v0 = t1.y - t0.y;
			float u1 = t2.x - t0.x;
			float v1 = t2.y - t0.y;

			D3DXVECTOR3 k0 = r1 - r0;
			D3DXVECTOR3 k1 = r2 - r0;

			float q = (u0*v1-u1*v0);
			if (q==0) q = 1.0f;
			else q = 1.0f / q;

			D3DXVECTOR3 t = ((k0*v1 - k1*v0) * q);
			ta[i0]+=t; ta[i1]+=t; ta[i2]+=t;
			pVrt[i0].w = pVrt[i1].w = pVrt[i2].w = (q<0.0f ? 1.0f : -1.0f);
		}

		for (DWORD i=0;i<nVtx; i++) {

			D3DXVECTOR3 n = D3DXVECTOR3(pVrt[i].nx,  pVrt[i].ny,  pVrt[i].nz);
			D3DXVec3Normalize(&n, &n);
			D3DXVECTOR3 t = (ta[i] - n * D3DXVec3Dot(&ta[i],&n));
			D3DXVec3Normalize(&t, &t);

			pVrt[i].tx = t.x;
			pVrt[i].ty = t.y;
			pVrt[i].tz = t.z;
		}

		delete []ta;
	}
	else {
		for (DWORD i=0;i<nVtx; i++) {
			D3DXVECTOR3 n = D3DXVECTOR3(pVrt[i].nx,  pVrt[i].ny,  pVrt[i].nz);
			D3DXVECTOR3 t = Perpendicular(&n);
			D3DXVec3Normalize(&t, &t);
			pVrt[i].tx = t.x;
			pVrt[i].ty = t.y;
			pVrt[i].tz = t.z;
		}
	}
}


// ===========================================================================================
//
bool D3D9Mesh::CopyGroupEx(GROUPREC *grp, const MESHGROUPEX *mg, DWORD gid)
{
	if (!pVB) return false;

	grp->UsrFlag = mg->UsrFlag;
	grp->IntFlag = mg->Flags;
	grp->zBias   = mg->zBias;

	D3DXMatrixIdentity(&grp->Transform);

	WORD *pIndex;
	NMVERTEX *pVert;
	NTVERTEX *pNT = mg->Vtx;

	HR(pIB->Lock(grp->FaceOff*6, grp->nFace*6, (LPVOID*)&pIndex, 0));
	HR(pVB->Lock(grp->VertOff*sizeof(NMVERTEX), grp->nVert*sizeof(NMVERTEX), (LPVOID*)&pVert, 0));

	for (DWORD i=0;i<mg->nIdx;i++) pIndex[i] = mg->Idx[i];

	for (DWORD i=0;i<mg->nVtx; i++) {
		float x = pNT[i].nx; float y = pNT[i].ny; float z = pNT[i].nz;
		float b = 1.0f/sqrt(y*y+z*z+x*x);
		pVert[i].nx = (x*b);
		pVert[i].ny = (y*b);
		pVert[i].nz = (z*b);
		pVert[i].x  = pNT[i].x;
		pVert[i].y  = pNT[i].y;
		pVert[i].z  = pNT[i].z;
		pVert[i].u  = pNT[i].tu;
		pVert[i].v  = pNT[i].tv;
		pVert[i].w  = 1.0f;
		pVert[i].tx = 1.0f;
		pVert[i].ty = 0.0f;
		pVert[i].tz = 0.0f;
	}

	// Check vertex index errors (This is important)
	//
	for (DWORD i=0;i<(mg->nIdx/3);i++) {
		if (pIndex[i*3+0]>=mg->nVtx || pIndex[i*3+1]>=mg->nVtx || pIndex[i*3+2]>=mg->nVtx) {
			LogErr("Vertex index out of range Face=%u, MeshGroup=%u", i, gid);
			pIndex[i*3+0] = 0;
			pIndex[i*3+1] = 0;
			pIndex[i*3+2] = 0;
		}
	}

	// For un-instanced mesh the base-offset is zero
	if (Config->UseNormalMap) UpdateTangentSpace(pVert, pIndex, mg->nVtx, mg->nIdx/3, grp->TexIdx!=0);

	if (mg->nVtx>0) BoundingBox(pVert, mg->nVtx, &grp->BBox);
	else D9ZeroAABB(&grp->BBox);

	HR(pIB->Unlock());
	HR(pVB->Unlock());

	return true;
}



// ===========================================================================================
// Mesh Update routine for AMSO
//
void D3D9Mesh::UpdateGroupEx(DWORD idx, const MESHGROUPEX *mg)
{
	_TRACE;
	if (!pVB) return;
	GROUPREC *grp = Grp[idx];
	NMVERTEX *pVert = LockVertexBuffer(idx);
	NTVERTEX *pNT = mg->Vtx;

	if (pVert) {
		for (DWORD i=0;i<mg->nVtx;i++) {
			pVert[i].x = pNT[i].x;
			pVert[i].y = pNT[i].y;
			pVert[i].z = pNT[i].z;
		}
		if (Config->UseNormalMap) {
			WORD *idx=0;
			if (pIB->Lock(grp->FaceOff*6, grp->nFace*6, (LPVOID*)&idx, 0)==S_OK) {
				UpdateTangentSpace(pVert, idx, grp->nVert, grp->nFace, grp->TexIdx!=0);
				pIB->Unlock();
			}
		}
		
		if (mg->nVtx>0) BoundingBox(pVert, mg->nVtx, &grp->BBox);
		else D9ZeroAABB(&grp->BBox);

		UnLockVertexBuffer();
		UpdateGeometryBuffer();
	}
}


// ===========================================================================================
// Use this only to initialise default materials in a D3D9Mesh constructor
// Material extension part is set to zero.
//
bool D3D9Mesh::CopyMaterial(int idx, const MATERIAL *mat)
{
	if (!pVB) return true;
	CreateMatExt((const D3DMATERIAL9 *)mat, &Mtrl[idx]);
	return true;
}


// ===========================================================================================
// This is required by Client implementation see clbkEditMeshGroup
//
int D3D9Mesh::EditGroup(DWORD grp, GROUPEDITSPEC *ges)
{
	_TRACE;
	if (!pVB) return 1;
	if (grp >= nGrp) return 1;

	bBSRecompute = true;

	GROUPREC *g = Grp[grp];
	DWORD flag = ges->flags;
	DWORD old  = g->UsrFlag;

	if (flag & GRPEDIT_SETUSERFLAG)	     g->UsrFlag  = ges->UsrFlag;
	else if (flag & GRPEDIT_ADDUSERFLAG) g->UsrFlag |= ges->UsrFlag;
	else if (flag & GRPEDIT_DELUSERFLAG) g->UsrFlag &= ~ges->UsrFlag;

	if (g->UsrFlag!=old) DynamicGroup(grp);

	if (flag & GRPEDIT_VTX) {
		NMVERTEX *vtx = LockVertexBuffer(grp);
		DWORD i, vi;
		if (vtx) {
			for (i = 0; i < ges->nVtx; i++) {
				vi = (ges->vIdx ? ges->vIdx[i] : i);
				if (vi < g->nVert) {

					if      (flag & GRPEDIT_VTXCRDX)    vtx[vi].x   = ges->Vtx[i].x;
					else if (flag & GRPEDIT_VTXCRDADDX) vtx[vi].x  += ges->Vtx[i].x;
					if      (flag & GRPEDIT_VTXCRDY)    vtx[vi].y   = ges->Vtx[i].y;
					else if (flag & GRPEDIT_VTXCRDADDY) vtx[vi].y  += ges->Vtx[i].y;
					if      (flag & GRPEDIT_VTXCRDZ)    vtx[vi].z   = ges->Vtx[i].z;
					else if (flag & GRPEDIT_VTXCRDADDZ) vtx[vi].z  += ges->Vtx[i].z;
					if      (flag & GRPEDIT_VTXNMLX)    vtx[vi].nx  = ges->Vtx[i].nx;
					else if (flag & GRPEDIT_VTXNMLADDX) vtx[vi].nx += ges->Vtx[i].nx;
					if      (flag & GRPEDIT_VTXNMLY)    vtx[vi].ny  = ges->Vtx[i].ny;
					else if (flag & GRPEDIT_VTXNMLADDY) vtx[vi].ny += ges->Vtx[i].ny;
					if      (flag & GRPEDIT_VTXNMLZ)    vtx[vi].nz  = ges->Vtx[i].nz;
					else if (flag & GRPEDIT_VTXNMLADDZ) vtx[vi].nz += ges->Vtx[i].nz;
					if      (flag & GRPEDIT_VTXTEXU)    vtx[vi].u  = ges->Vtx[i].tu;
					else if (flag & GRPEDIT_VTXTEXADDU) vtx[vi].u += ges->Vtx[i].tu;
					if      (flag & GRPEDIT_VTXTEXV)    vtx[vi].v  = ges->Vtx[i].tv;
					else if (flag & GRPEDIT_VTXTEXADDV) vtx[vi].v += ges->Vtx[i].tv;
				}
			}

			if (Config->UseNormalMap) {
				WORD *idx=0;
				if (pIB->Lock(g->FaceOff*6, g->nFace*6, (LPVOID*)&idx, 0)==S_OK) {
					UpdateTangentSpace(vtx, idx, g->nVert, g->nFace, g->TexIdx!=0);
					pIB->Unlock();
				}
			}

			if (g->nVert>0) BoundingBox(vtx, g->nVert, &g->BBox);
			else D9ZeroAABB(&g->BBox);

			UnLockVertexBuffer();
		}
	}
	return 0;
}


NTVERTEX Convert(NMVERTEX &v)
{
	NTVERTEX n;
	n.x = v.x; n.y = v.y; n.z = v.z; 
	n.nx = v.nx; n.ny = v.ny; n.nz = v.nz;
	n.tu = v.u;	n.tv = v.v;
	return n;
}


int D3D9Mesh::GetGroup (DWORD grp, GROUPREQUESTSPEC *grs)
{
	static NTVERTEX zero = {0,0,0, 0,0,0, 0,0};
	if (grp >= nGrp) return 1;
	DWORD nv = Grp[grp]->nVert;
	DWORD ni = Grp[grp]->nFace*3;
	DWORD i, vi;
	int ret = 0;

	if (grs->nVtx && grs->Vtx) { // vertex data requested
		NMVERTEX *vtx = LockVertexBuffer(grp);
		if (vtx) {
			if (grs->VtxPerm) { // random access data request
				for (i = 0; i < grs->nVtx; i++) {
					vi = grs->VtxPerm[i];
					if (vi < nv) {
						grs->Vtx[i] = Convert(vtx[vi]);
					} else {
						grs->Vtx[i] = zero;
						ret = 1;
					}
				}
			} else {
				if (grs->nVtx > nv) grs->nVtx = nv;
				for (i=0;i<grs->nVtx;i++) grs->Vtx[i] = Convert(vtx[i]);
			}
			UnLockVertexBuffer();
		}
		else return 1;
	}

	if (grs->nIdx && grs->Idx) { // index data requested
		WORD *idx = LockIndexBuffer(grp);
		if (idx) {
			if (grs->IdxPerm) { // random access data request
				for (i = 0; i < grs->nIdx; i++) {
					vi = grs->IdxPerm[i];
					if (vi < ni) {
						grs->Idx[i] = idx[vi];
					} else {
						grs->Idx[i] = 0;
						ret = 1;
					}
				}
			} else {
				if (grs->nIdx > ni) grs->nIdx = ni;
				for (i=0;i<grs->nIdx;i++) grs->Idx[i] = idx[i];
			}
			UnLockIndexBuffer();
		}
		else return 1;
	}

	grs->MtrlIdx = Grp[grp]->MtrlIdx;
	grs->TexIdx = Grp[grp]->TexIdx;
	return ret;
}


// ===========================================================================================
//
bool D3D9Mesh::SetTexture(DWORD texidx, LPD3D9CLIENTSURFACE tex)
{
	_TRACE;
	if (!pVB) return false;
	if (texidx >= nTex) {
		LogErr("D3D9Mesh::SetTexture(%u, 0x%X) index out of range",texidx,tex);
		return false;
	}
	Tex[texidx] = tex;
	LogBlu("D3D9Mesh(0x%X)::SetTexture(%u, 0x%X) (%s)",this,texidx,tex,SURFACE(tex)->GetName());
	return true;
}

// ===========================================================================================
//
DWORD D3D9Mesh::GetMeshGroupMaterialIdx(DWORD idx)
{
	if (!pVB) return 0;
	if (idx>=nGrp) return 0;
	return Grp[idx]->MtrlIdx;
}

// ===========================================================================================
//
DWORD D3D9Mesh::GetMeshGroupTextureIdx(DWORD idx)
{
	if (!pVB) return 0;
	if (idx>=nGrp) return 0;
	return Grp[idx]->TexIdx;
}

// ===========================================================================================
//
bool D3D9Mesh::HasTexture(SURFHANDLE hSurf)
{
	if (!pVB) return false;
	for (DWORD i=0;i<nTex;i++) if (Tex[i]==hSurf) return true;
	return false;
}

// ===========================================================================================
//
bool D3D9Mesh::IsReflective()
{
	_TRACE;
	if (!pVB) return false;
	for (DWORD i=0;i<nTex;i++) if (Tex[i]) if (SURFACE(Tex[i])->GetReflectionMap()) return true;
	for (DWORD i=0;i<nMtrl;i++) if (Mtrl[i].Fresnel.g!=0.0f || Mtrl[i].Reflect.a!=0.0f) return true;
	return false;
}

// ===========================================================================================
//
void D3D9Mesh::SetTexMixture(DWORD ntex, float mix)
{
	_TRACE;
	if (!pVB) return;
	ntex--;
	for (DWORD g = 0; g < nGrp; g++) if (Grp[g]->TexIdxEx[ntex] != SPEC_DEFAULT) Grp[g]->TexMixEx[ntex] = mix;
}

// ===========================================================================================
//
void D3D9Mesh::SetSunLight(D3D9Light *light)
{
	if (!pVB) return;
	sunLight = light;
}


// ===========================================================================================
//
NMVERTEX * D3D9Mesh::LockVertexBuffer(DWORD grp)
{
	if (!pVB) return NULL;
	NMVERTEX *pVert;
	bBSRecompute = true;

	if (grp>=nGrp) {
		LogErr("D3D9Mesh(0x%X)::GetVertexBuffer(%u) index out of range",this,grp);
		return NULL;
	}

	if (pVB->Lock(Grp[grp]->VertOff*sizeof(NMVERTEX), Grp[grp]->nVert*sizeof(NMVERTEX), (LPVOID*)&pVert, 0)==S_OK) {
		return pVert;
	}
	else {
		LogErr("D3D9Mesh(0x%X)::GetVertexBuffer(%u)",this,grp);
		return NULL;
	}
}

// ===========================================================================================
//
void D3D9Mesh::UnLockVertexBuffer()
{
	if (!pVB) return;
	HR(pVB->Unlock());
}

// ===========================================================================================
//

WORD * D3D9Mesh::LockIndexBuffer(DWORD grp)
{
	if (!pIB) return NULL;
	WORD *pIdx;
	bBSRecompute = true;

	if (grp>=nGrp) {
		LogErr("D3D9Mesh(0x%X)::LockIndexBuffer(%u) index out of range",this,grp);
		return NULL;
	}

	if (pIB->Lock(Grp[grp]->FaceOff*6, Grp[grp]->nFace*6, (LPVOID*)&pIdx, 0)==S_OK) {
		return pIdx;
	}
	else {
		LogErr("D3D9Mesh(0x%X)::LockIndexBuffer(%u)",this,grp);
		return NULL;
	}
}

// ===========================================================================================
//
void D3D9Mesh::UnLockIndexBuffer()
{
	if (!pIB) return;
	HR(pIB->Unlock());
}

// ===========================================================================================
//
D3D9Mesh::GROUPREC *D3D9Mesh::GetGroup(DWORD idx)
{
	if (!pVB) return NULL;
	if (idx<nGrp) return Grp[idx];
	return NULL;
}


// ===========================================================================================
//
void D3D9Mesh::SetAmbientColor(D3DCOLOR c)
{
	_TRACE;
	if (!pVB) return;
	cAmbient = c;
}


// ===========================================================================================
//
void D3D9Mesh::SetupFog(const LPD3DXMATRIX pW)
{
	_TRACE;
	if (!pVB) return;
	FX->SetVector(eAttennuate, &D3DXVECTOR4(1,1,1,1));
	FX->SetVector(eInScatter,  &D3DXVECTOR4(0,0,0,0));
}


// ===========================================================================================
//
void D3D9Mesh::RenderGroup(LPDIRECT3DDEVICE9 pDev, const GROUPREC *grp)
{
	_TRACE;
	if (!pVB) return;
	if (!grp) return;

	pDev->SetVertexDeclaration(pMeshVertexDecl);
	pDev->SetStreamSource(0, pVB, 0, sizeof(NMVERTEX));
	pDev->SetIndices(pIB);
	pDev->DrawIndexedPrimitive(D3DPT_TRIANGLELIST, grp->VertOff, 0, grp->nVert, grp->FaceOff*3, grp->nFace);
	gc->GetStats()->Vertices += grp->nVert;
	gc->GetStats()->Draw++;
}


// Used only by ring manager --------------------------------------------------------------------
//
void D3D9Mesh::RenderRings(LPDIRECT3DDEVICE9 dev, const LPD3DXMATRIX pW, LPDIRECT3DTEXTURE9 pTex)
{
	_TRACE;
	if (!pVB) return;
	if (!pTex) return;

	gc->GetStats()->Vertices += Grp[0]->nVert;
	gc->GetStats()->Meshes++;

	UINT numPasses = 0;
	HR(FX->SetTechnique(eRingTech));
	HR(FX->SetMatrix(eW, pW));
	HR(FX->SetTexture(eTex0, pTex));
	if (sunLight) FX->SetValue(eSun, sunLight, sizeof(D3D9Light));
	HR(FX->SetValue(eMtrl, &defmat, D3D9MATSIZE));
	HR(FX->Begin(&numPasses, D3DXFX_DONOTSAVESTATE));
	HR(FX->BeginPass(0));
	RenderGroup(dev, Grp[0]);
	HR(FX->EndPass());
	HR(FX->End());
}

// Used only by ring manager --------------------------------------------------------------------
//
void D3D9Mesh::RenderRings2(LPDIRECT3DDEVICE9 dev, const LPD3DXMATRIX pW, LPDIRECT3DTEXTURE9 pTex, float irad, float orad)
{
	_TRACE;
	if (!pVB) return;
	if (!pTex) return;

	gc->GetStats()->Vertices += Grp[0]->nVert;
	gc->GetStats()->Meshes++;

	UINT numPasses = 0;
	HR(FX->SetTechnique(eRingTech2));
	HR(FX->SetMatrix(eW, pW));
	HR(FX->SetTexture(eTex0, pTex));
	if (sunLight) FX->SetValue(eSun, sunLight, sizeof(D3D9Light));
	HR(FX->SetValue(eMtrl, &defmat, D3D9MATSIZE));
	HR(FX->SetVector(eTexOff, &D3DXVECTOR4(irad, orad, 0, 0)));
	HR(FX->Begin(&numPasses, D3DXFX_DONOTSAVESTATE));
	HR(FX->BeginPass(0));
	RenderGroup(dev, Grp[0]);
	HR(FX->EndPass());
	HR(FX->End());
}

// ===========================================================================================
// This is a special rendering routine used by VVessel to render MFD screens and HUD
//
// Used by MFDs, HUD
//
void D3D9Mesh::RenderMeshGroup(LPDIRECT3DDEVICE9 dev, DWORD Tech, DWORD idx, const LPD3DXMATRIX pW, LPD3D9CLIENTSURFACE pTex)
{
	_TRACE;
	if (!pVB) return;

	if (idx>=nGrp) return;

	GROUPREC *grp = Grp[idx];

	UINT numPasses = 0;
	D3DXMATRIX q;

	if (Tech==0) HR(FX->SetTechnique(eVCHudTech));
	if (Tech==1) HR(FX->SetTechnique(eVCMFDTech));

	if (grp->zBias) {
		float zBias = float(grp->zBias) * 1.2e-7f;
		dev->SetRenderState(D3DRS_DEPTHBIAS, *((DWORD*)&zBias));
	}


	if (Grp[idx]->bTransform) {
		if (bGlobalTF)  FX->SetMatrix(eGT, D3DXMatrixMultiply(&q, &mTransform, &Grp[idx]->Transform));
		else FX->SetMatrix(eGT, &Grp[idx]->Transform);
	}
	else FX->SetMatrix(eGT, &mTransform);

	HR(FX->SetMatrix(eW, pW));
	HR(FX->SetTexture(eTex0, pTex->GetTexture()));

	if (sunLight) FX->SetValue(eSun, sunLight, sizeof(D3D9Light));

	// Setup Mesh group material ==============================================================================
	//
	D3D9MatExt *mat = &defmat;
	if (Grp[idx]->MtrlIdx!=SPEC_DEFAULT) mat = &Mtrl[Grp[idx]->MtrlIdx];

	HR(FX->SetValue(eMtrl, mat, D3D9MATSIZE));
	HR(FX->Begin(&numPasses, D3DXFX_DONOTSAVESTATE));
	HR(FX->BeginPass(0));
	RenderGroup(dev, grp);
	HR(FX->EndPass());
	HR(FX->End());

	if (grp->zBias) dev->SetRenderState(D3DRS_DEPTHBIAS, 0);
}


// ================================================================================================
// This is a rendering routine for a Exterior Mesh, non-spherical moons/asteroids
//
void D3D9Mesh::Render(LPDIRECT3DDEVICE9 dev, const LPD3DXMATRIX pW, int iTech, LPDIRECT3DCUBETEXTURE9 *pEnv, int nEnv)
{
	_TRACE;
	DWORD flags=0, selmsh=0, selgrp=0, displ=0; // Debug Variables
	bool bActiveVisual = false;
	bool bEmission = false;

	if (!pVB) return;

	if (DebugControls::IsActive()) {
		flags  = *(DWORD*)gc->GetConfigParam(CFGPRM_GETDEBUGFLAGS);
		selmsh = *(DWORD*)gc->GetConfigParam(CFGPRM_GETSELECTEDMESH);
		selgrp = *(DWORD*)gc->GetConfigParam(CFGPRM_GETSELECTEDGROUP);
		displ  = *(DWORD*)gc->GetConfigParam(CFGPRM_GETDISPLAYMODE);
		bActiveVisual = (pCurrentVisual==DebugControls::GetVisual());
		if (displ>0 && !bActiveVisual) return;
		if ((displ==2 || displ==3) && uCurrentMesh!=selmsh) return;
	}

	Scene *scn = gc->GetScene();

	bool bMeshCull = true;
	bool bTextured = true;
	bool bGroupCull = true;
	bool bUseNormalMap = (Config->UseNormalMap==1);

	switch (iTech) {
		case RENDER_VC:
			FX->SetBool(eGlow, false);
			bMeshCull = false;
			bGroupCull = false;
			break;
		case RENDER_BASE:
			FX->SetBool(eGlow, false);
			bMeshCull = false;
			break;
		case RENDER_BASEBS:
			FX->SetBool(eGlow, false);
			bMeshCull = false;
			break;
		case RENDER_ASTEROID:
			FX->SetBool(eGlow, true);
			bMeshCull = false;
			bGroupCull = false;
			break;
		case RENDER_VESSEL:
			FX->SetBool(eGlow, true);
			break;
	}

	D3DXMATRIX mWorldView,  q;
	D3DXMatrixMultiply(&mWorldView, pW, scn->GetViewMatrix());

	D3DXVECTOR4 Field = D9LinearFieldOfView(scn->GetProjectionMatrix());

	if (bMeshCull) if (!D9IsAABBVisible(&BBox, &mWorldView, &Field)) {
		if (flags&(DBG_FLAGS_BOXES|DBG_FLAGS_SPHERES)) RenderBoundingBox(dev, pW);
		return;
	}

	D3DXMATRIX mWorldMesh;
	
	if (bGlobalTF) D3DXMatrixMultiply(&mWorldMesh, &mTransform, pW);
	else mWorldMesh = *pW;

	gc->GetStats()->Meshes++;

	D3D9MatExt *mat, *old_mat = NULL;
	LPD3D9CLIENTSURFACE old_tex = NULL;
	LPDIRECT3DTEXTURE9  pNorm = NULL;
	LPDIRECT3DTEXTURE9  pSpec = NULL;
	LPDIRECT3DTEXTURE9  pEmis = NULL;
	LPDIRECT3DTEXTURE9  pRefl = NULL;
	LPDIRECT3DTEXTURE9  pTransl = NULL;
	LPDIRECT3DTEXTURE9  pTransm = NULL;

	dev->SetVertexDeclaration(pMeshVertexDecl);
	dev->SetStreamSource(0, pVB, 0, sizeof(NMVERTEX));
	dev->SetIndices(pIB);

	/*if (iTech==RENDER_BASEBS) {
		float zBias = -2.0f * scn->GetDepthResolution((float)oapiCameraTargetDist());
		dev->SetRenderState(D3DRS_DEPTHBIAS, *((DWORD*)&zBias));
		dev->SetRenderState(D3DRS_SLOPESCALEDEPTHBIAS, *((DWORD*)&zBias));
	}*/

	if (flags&DBG_FLAGS_DUALSIDED) dev->SetRenderState(D3DRS_CULLMODE, D3DCULL_NONE);
	if (sunLight) FX->SetValue(eSun, sunLight, sizeof(D3D9Light));

	if (iTech==RENDER_VC) FX->SetTechnique(eVCTech);
	else				  FX->SetTechnique(eVesselTech);

	FX->SetBool(eUseSpec, false);
	FX->SetBool(eUseEmis, false);
	FX->SetBool(eUseRefl, false);
	FX->SetBool(eUseDisl, false);
	FX->SetBool(eDebugHL, false);
	FX->SetBool(eUseTransl, false);
	FX->SetBool(eUseTransm, false);

	int nLights = gc->GetScene()->GetLightCount();
	const D3D9Light *pLights = gc->GetScene()->GetLights();

	if (pLights && nLights>0) { // && iTech==RENDER_VESSEL) {
		HR(FX->SetValue(eLights, pLights, 12*sizeof(D3D9Light)));
		FX->SetInt(eLightCount, nLights);
		FX->SetBool(eLocalLights, true);
	}
	else {
		FX->SetInt(eLightCount, 0);
		FX->SetBool(eLocalLights, false);
	}

	UINT numPasses = 0;
	HR(FX->Begin(&numPasses, D3DXFX_DONOTSAVESTATE));

	

	// Pass 0 = Normal Mapped
	// Pass 1 = Textured

	for (DWORD pass=0;pass<numPasses;pass++) {

		if (bUseNormalMap==false && pass==0) continue; // Skip normal mapped rendering pass

		HR(FX->BeginPass(pass));

		if (iTech==RENDER_BASEBS) dev->SetRenderState(D3DRS_ZENABLE, 0);

		for (DWORD g=0; g<nGrp; g++) {

			if (Grp[g]->UsrFlag & 0x2) continue;

			// Mesh Debugger -------------------------------------------------------------------------------------------
			//
			if (DebugControls::IsActive()) {

				if (bActiveVisual) {

					if (displ==3 && g!=selgrp) continue;

					FX->SetBool(eDebugHL, false);

					if (flags&DBG_FLAGS_HLMESH) {
						if (uCurrentMesh==selmsh) {
							FX->SetVector(eColor, &D3DXVECTOR4(0.0f, 0.0f, 0.5f, 0.5f));
							FX->SetBool(eDebugHL, true);
						}
					}
					if (flags&DBG_FLAGS_HLGROUP) {
						if (g==selgrp && uCurrentMesh==selmsh) {
							FX->SetVector(eColor, &D3DXVECTOR4(0.0f, 0.5f, 0.0f, 0.5f));
							FX->SetBool(eDebugHL, true);
						}
					}
				}
			}


			// ---------------------------------------------------------------------------------------------------------
			//
			DWORD ti=Grp[g]->TexIdx;
			DWORD tni=Grp[g]->TexIdxEx[0];

			if (ti==0 && tni!=0) continue;
				
			if (Tex[ti]==NULL || ti==0) bTextured = false;
			else						bTextured = true;

			if (bTextured) {
				pNorm = Tex[ti]->GetNormalMap();
				if (pNorm==NULL && pass==0) continue;
				if (pNorm!=NULL && pass==1) continue;
			}
			else {
				if (pass==0) continue;
				pNorm=NULL;
				old_tex=NULL;
			}

			// Cull unvisible geometry =================================================================================
			//
			if (bGroupCull) if (!D9IsBSVisible(&Grp[g]->BBox, &mWorldView, &Field)) continue;


			// Setup Textures and Normal Maps ==========================================================================
			//
			if (bTextured) {

				if (Tex[ti]!=old_tex) {

					if (tni && Grp[g]->TexMixEx[0]<0.5f) tni=0;

					old_tex = Tex[ti];
					FX->SetTexture(eTex0, Tex[ti]->GetTexture());

					bEmission = false;

					if (tni && Tex[tni]) {
						FX->SetTexture(eEmisMap, Tex[tni]->GetTexture());
						bEmission = true;
					}  

					if (bUseNormalMap) {

						pSpec = Tex[ti]->GetSpecularMap();
						pEmis = Tex[ti]->GetEmissionMap();
						pRefl = Tex[ti]->GetReflectionMap();
						pTransl = Tex[ti]->GetTranslucenceMap();
						pTransm = Tex[ti]->GetTransmittanceMap();

						if (pNorm) FX->SetTexture(eTex3, pNorm);
						if (pSpec) FX->SetTexture(eSpecMap, pSpec);
						if (pEmis) FX->SetTexture(eEmisMap, pEmis);
						if (pRefl) FX->SetTexture(eReflMap, pRefl);
						if (pTransl) FX->SetTexture(eTranslMap, pTransl);
						if (pTransm) FX->SetTexture(eTransmMap, pTransm);

						FX->SetBool(eUseSpec, (pSpec!=NULL));
						FX->SetBool(eUseRefl, (pRefl!=NULL));
						FX->SetBool(eUseTransl, (pTransl!=NULL));
						FX->SetBool(eUseTransm, (pTransm!=NULL));

						if (pEmis) bEmission = true;
					}

					FX->SetBool(eUseEmis, bEmission);
				}
			}


			// Setup Mesh group material ==============================================================================
			//
			if (Grp[g]->MtrlIdx==SPEC_DEFAULT) mat = &defmat;
			else							   mat = &Mtrl[Grp[g]->MtrlIdx];

			if (mat!=old_mat) {

				old_mat = mat;

				FX->SetValue(eMtrl, mat, D3D9MATSIZE);

				if (bModulateMatAlpha || bTextured==false)  FX->SetFloat(eMtrlAlpha, mat->Diffuse.a);
				else										FX->SetFloat(eMtrlAlpha, 1.0f);

				if (nEnv && pEnv) {
					if (mat==&defmat) {
						HR(FX->SetBool(eEnvMapEnable, false));
					}
					else {
						if (pEnv[0]) {
							if (mat->Reflect.a!=0.0f || mat->Fresnel.g!=0.0f || pRefl) {
								FX->SetBool(eEnvMapEnable, true);
								FX->SetTexture(eEnvMap, pEnv[0]);
								if (mat->pDissolve) {
									FX->SetTexture(eDislMap, SURFACE(mat->pDissolve)->GetTexture());
									FX->SetBool(eUseDisl, true);
								}
								else FX->SetBool(eUseDisl, false);

							}
							else FX->SetBool(eEnvMapEnable, false);
						}
					}
				}
			}

			// Apply z-Bias =============================================================================================
			//
			/*
			if (Grp[g]->zBias) {
				float zBias = float(Grp[g]->zBias) * 1.2e-7f;
				dev->SetRenderState(D3DRS_DEPTHBIAS, *((DWORD*)&zBias));
			}*/

			// Apply Animations =========================================================================================
			//
			if (Grp[g]->bTransform) FX->SetMatrix(eW, D3DXMatrixMultiply(&q, &pGrpTF[g], pW));
			else					FX->SetMatrix(eW, &mWorldMesh);

			// Setup Mesh drawing options =================================================================================
			//
			FX->SetBool(eTextured, bTextured);
			FX->SetBool(eFullyLit, (Grp[g]->UsrFlag&0x4)!=0);
			FX->SetBool(eNoColor,  (Grp[g]->UsrFlag&0x10)!=0);

			FX->CommitChanges();

			dev->DrawIndexedPrimitive(D3DPT_TRIANGLELIST, Grp[g]->VertOff,  0, Grp[g]->nVert,  Grp[g]->FaceOff*3, Grp[g]->nFace);
			gc->GetStats()->Vertices += Grp[g]->nVert;
			gc->GetStats()->Draw++;
			gc->GetStats()->MeshGrps++;

			//if (Grp[g]->zBias) dev->SetRenderState(D3DRS_DEPTHBIAS, 0);
		}
		HR(FX->EndPass());
	}
	HR(FX->End());

	if (flags&(DBG_FLAGS_BOXES|DBG_FLAGS_SPHERES)) RenderBoundingBox(dev, pW);
	FX->SetBool(eDebugHL, false);
	if (flags&DBG_FLAGS_DUALSIDED) dev->SetRenderState(D3DRS_CULLMODE, D3DCULL_CCW);

	/*
	if (iTech==RENDER_BASEBS) {
		float zBias = 0.0f;
		dev->SetRenderState(D3DRS_DEPTHBIAS, *((DWORD*)&zBias));
		dev->SetRenderState(D3DRS_SLOPESCALEDEPTHBIAS, *((DWORD*)&zBias));
	}*/
}

// ===========================================================================================
//
void D3D9Mesh::RenderBaseTile(LPDIRECT3DDEVICE9 dev, const LPD3DXMATRIX pW)
{
	if (!pVB) return;

	Scene *scn = gc->GetScene();

	bool bTextured = true;
	bool bGroupCull = true;
	bool bUseNormalMap = (Config->UseNormalMap==1);

	D3DXMATRIX mWorldView,  q;
	D3DXMatrixMultiply(&mWorldView, pW, scn->GetViewMatrix());

	D3DXVECTOR4 Field = D9LinearFieldOfView(scn->GetProjectionMatrix());

	D3D9MatExt *mat, *old_mat = NULL;
	LPD3D9CLIENTSURFACE old_tex = NULL;
	LPDIRECT3DTEXTURE9  pNorm = NULL;
	
	dev->SetVertexDeclaration(pMeshVertexDecl);
	dev->SetStreamSource(0, pVB, 0, sizeof(NMVERTEX));
	dev->SetIndices(pIB);

	if (sunLight) FX->SetValue(eSun, sunLight, sizeof(D3D9Light));

	FX->SetTechnique(eBaseTile);
	FX->SetMatrix(eGT, gc->GetIdentity());
	FX->SetMatrix(eW, pW);
	FX->SetBool(eUseSpec, false);
	FX->SetBool(eUseEmis, false);
	FX->SetBool(eUseRefl, false);
	FX->SetBool(eUseDisl, false);
	FX->SetBool(eDebugHL, false);
	FX->SetInt(eLightCount, 0);
	
	UINT numPasses = 0;
	HR(FX->Begin(&numPasses, D3DXFX_DONOTSAVESTATE));

	for (DWORD pass=0;pass<numPasses;pass++) {

		if (bUseNormalMap==false && pass==0) continue; // Skip normal mapped rendering pass

		HR(FX->BeginPass(pass));

		for (DWORD g=0; g<nGrp; g++) {

			if (Grp[g]->UsrFlag & 0x2) continue;

			// Render group ----------------------------------------------
			//

			DWORD ti=Grp[g]->TexIdx;
			DWORD tni=Grp[g]->TexIdxEx[0];

			if (ti==0 && tni!=0) continue;
				
			if (Tex[ti]==NULL || ti==0) bTextured = false;
			else						bTextured = true;

			if (bTextured) {
				pNorm = Tex[ti]->GetNormalMap();
				if (pNorm==NULL && pass==0) continue;
				if (pNorm!=NULL && pass==1) continue;
			}
			else {
				if (pass==0) continue;
				pNorm=NULL;
				old_tex=NULL;
			}

			// Cull unvisible geometry ------------------------------------------------------
			//
			if (bGroupCull) if (!D9IsBSVisible(&Grp[g]->BBox, &mWorldView, &Field)) continue;


			// Setup Textures and Normal Maps ==========================================================================
			//
			if (bTextured) {

				if (Tex[ti]!=old_tex) {

					if (tni && Grp[g]->TexMixEx[0]<0.5f) tni=0;

					old_tex = Tex[ti];
					FX->SetTexture(eTex0, Tex[ti]->GetTexture());

					if (tni && Tex[tni]) {
						FX->SetTexture(eEmisMap, Tex[tni]->GetTexture());
						FX->SetBool(eUseEmis, true);
					} else FX->SetBool(eUseEmis, false);

					if (bUseNormalMap) if (pNorm) FX->SetTexture(eTex3, pNorm);
				}
			}

			// Setup Mesh group material ==============================================================================
			//
			if (Grp[g]->MtrlIdx==SPEC_DEFAULT) mat = &defmat;
			else							   mat = &Mtrl[Grp[g]->MtrlIdx];

			if (mat!=old_mat) {
				old_mat = mat;
				FX->SetValue(eMtrl, mat, D3D9MATSIZE);
				if (bModulateMatAlpha || bTextured==false) FX->SetFloat(eMtrlAlpha, mat->Diffuse.a);
				else FX->SetFloat(eMtrlAlpha, 1.0f);
			}

			// Setup Mesh drawing options =================================================================================
			//
			FX->SetBool(eTextured, bTextured);
			FX->SetBool(eFullyLit, (Grp[g]->UsrFlag&0x4)!=0);

			FX->CommitChanges();

			dev->DrawIndexedPrimitive(D3DPT_TRIANGLELIST, Grp[g]->VertOff, 0, Grp[g]->nVert, Grp[g]->FaceOff*3, Grp[g]->nFace);

			gc->GetStats()->Vertices += Grp[g]->nVert;
			gc->GetStats()->Draw++;
			gc->GetStats()->MeshGrps++;
		}
		HR(FX->EndPass());
	}
	HR(FX->End());
}


// ================================================================================================
//
void D3D9Mesh::RenderShadows(LPDIRECT3DDEVICE9 dev, float alpha, const LPD3DXMATRIX pW)
{
	if (!pVB || !pGB) return;
	if (!Geom) return;
	
	D3DXMATRIX q, mWorldMesh; UINT numPasses = 0;
	
	if (bGlobalTF) D3DXMatrixMultiply(&mWorldMesh, &mTransform, pW);
	else mWorldMesh = *pW;

	gc->GetStats()->Meshes++;

	dev->SetVertexDeclaration(pVector4Decl);
	dev->SetStreamSource(0, pGB, 0, sizeof(D3DXVECTOR4));
	dev->SetIndices(pGI);
	FX->SetTechnique(eShadowTech);
	FX->SetFloat(eMix, alpha);
	FX->Begin(&numPasses, D3DXFX_DONOTSAVESTATE);
	FX->BeginPass(0);
	
	for (DWORD g=0; g<nGeom; g++) {
		if (Geom[g].bBroken==false && Geom[g].bNoShadow==true) continue;
		for (DWORD k=0;k<Geom[g].nGrp;k++) {
			int gr = Geom[g].GrpIdx[k];
			if (Grp[gr]->bTransform) D3DXMatrixMultiply(&InstMatrix[k], &pGrpTF[gr], pW);		// Apply Animations to instance matrices
			else InstMatrix[k] = mWorldMesh;                 
		}

		FX->SetValue(eInstMatrix, InstMatrix, 8*sizeof(D3DXMATRIX));
		FX->CommitChanges();

		if (Geom[g].bBroken) {
			// If the Geometry group has gone broken, render each group member separately
			for (DWORD k=0;k<Geom[g].nGrp;k++) {
				DWORD gr = Geom[g].GrpIdx[k];
				if (Grp[gr]->UsrFlag & 0x3) continue;
				if (Grp[gr]->IntFlag & 0x3) continue;
				dev->DrawIndexedPrimitive(D3DPT_TRIANGLELIST, Geom[g].VertOff, 0, Geom[g].nVert, Grp[gr]->GeoFOff*3, Grp[gr]->nFace);
			}
		} else {
			// Geometry group is intact, render all at once.
			dev->DrawIndexedPrimitive(D3DPT_TRIANGLELIST, Geom[g].VertOff, 0, Geom[g].nVert, Geom[g].FaceOff*3, Geom[g].nFace);
		}
	}
	FX->EndPass();
	FX->End();
}


// ================================================================================================
//
void D3D9Mesh::RenderShadowsEx(LPDIRECT3DDEVICE9 dev, float alpha, const LPD3DXMATRIX pP, const LPD3DXMATRIX pW, const D3DXVECTOR4 *light, const D3DXVECTOR4 *param)
{
	if (!pVB || !pGB) return;
	if (!Geom) return;

	gc->GetStats()->Meshes++;

	dev->SetVertexDeclaration(pVector4Decl);
	dev->SetStreamSource(0, pGB, 0, sizeof(D3DXVECTOR4));
	dev->SetIndices(pGI);

	FX->SetTechnique(eShadowTech);
	FX->SetMatrix(eW, pW);
	FX->SetMatrix(eGT, pP);
	FX->SetFloat(eMix, alpha);
	FX->SetVector(eColor, light);
	FX->SetVector(eTexOff, param);

	UINT numPasses = 0;
	FX->Begin(&numPasses, D3DXFX_DONOTSAVESTATE);
	FX->BeginPass(1);

	for (DWORD g=0;g<nGeom;g++) {
		if (Geom[g].bBroken==false && Geom[g].bNoShadow==true) continue;
		if (Geom[g].bBroken) {
			// If the Geometry group has gone broken, render each group member separately
			for (DWORD i=0;i<Geom[g].nGrp;i++) {
				DWORD gr = Geom[g].GrpIdx[i];
				if (Grp[gr]->UsrFlag & 0x3) continue;
				if (Grp[gr]->IntFlag & 0x3) continue;
				dev->DrawIndexedPrimitive(D3DPT_TRIANGLELIST, Geom[g].VertOff, 0, Geom[g].nVert, Grp[gr]->GeoFOff*3, Grp[gr]->nFace);
			}
		} else {
			// Geometry group is intact, render all at once.
			dev->DrawIndexedPrimitive(D3DPT_TRIANGLELIST, Geom[g].VertOff, 0, Geom[g].nVert, Geom[g].FaceOff*3, Geom[g].nFace);
		}
	}
	FX->EndPass();
	FX->End();
}




// ================================================================================================
// This is a rendering routine for a Exterior Mesh, non-spherical moons/asteroids
//
void D3D9Mesh::RenderBoundingBox(LPDIRECT3DDEVICE9 dev, const LPD3DXMATRIX pW)
{
	_TRACE;

	if (!pVB) return;
	if (DebugControls::IsActive()==false) return;

	// Scene *scn = gc->GetScene();

	D3DXMATRIX q, qq;

	static D3DVECTOR poly[10] = {
		{0, 0, 0},
		{1, 0, 0},
		{1, 1, 0},
		{0, 1, 0},
		{0, 0, 0},
		{0, 0, 1},
		{1, 0, 1},
		{1, 1, 1},
		{0, 1, 1},
		{0, 0, 1}
	};

	static D3DVECTOR list[6] = {
		{1, 0, 0},
		{1, 0, 1},
		{1, 1, 0},
		{1, 1, 1},
		{0, 1, 0},
		{0, 1, 1}
	};



	DWORD flags  = *(DWORD*)gc->GetConfigParam(CFGPRM_GETDEBUGFLAGS);
	DWORD selmsh = *(DWORD*)gc->GetConfigParam(CFGPRM_GETSELECTEDMESH);
	DWORD selgrp = *(DWORD*)gc->GetConfigParam(CFGPRM_GETSELECTEDGROUP);
	bool  bSel   =  (uCurrentMesh==selmsh);


	if (flags&(DBG_FLAGS_SELVISONLY|DBG_FLAGS_SELMSHONLY|DBG_FLAGS_SELGRPONLY) && DebugControls::GetVisual()!=pCurrentVisual) return;
	if (flags&DBG_FLAGS_SELMSHONLY && !bSel) return;
	if (flags&DBG_FLAGS_SELGRPONLY && !bSel) return;

	if (flags&DBG_FLAGS_BOXES) {

		dev->SetVertexDeclaration(pPositionDecl);

		// ----------------------------------------------------------------
		FX->SetMatrix(eW, pW);
		FX->SetVector(eColor, &D3DXVECTOR4(0, 1, 0, 0.5f));
		FX->SetTechnique(eBBTech);
		// ----------------------------------------------------------------

		UINT numPasses = 0;
		FX->Begin(&numPasses, D3DXFX_DONOTSAVESTATE);
		FX->BeginPass(0);

		for (DWORD g=0; g<nGrp; g++) {

			if (flags&DBG_FLAGS_SELGRPONLY && g!=selgrp) continue;
			if (Grp[g]->UsrFlag & 0x2) continue;

			FX->SetVector(eAttennuate, &Grp[g]->BBox.min);
			FX->SetVector(eInScatter, &Grp[g]->BBox.max);

			// Apply Animations =========================================================================================
			//
			if (Grp[g]->bTransform) {
				if (bGlobalTF)  FX->SetMatrix(eGT, D3DXMatrixMultiply(&q, &mTransform, &Grp[g]->Transform));
				else FX->SetMatrix(eGT, &Grp[g]->Transform);
			}
			else FX->SetMatrix(eGT, &mTransform);


			// Setup Mesh drawing options =================================================================================
			//
			FX->CommitChanges();

			dev->DrawPrimitiveUP(D3DPT_LINESTRIP, 9, &poly, sizeof(D3DVECTOR));
			dev->DrawPrimitiveUP(D3DPT_LINELIST, 3, &list, sizeof(D3DVECTOR));

			gc->GetStats()->Draw+=2;
		}

		FX->EndPass();
		FX->End();
	}

	if (flags&DBG_FLAGS_SPHERES) {
		for (DWORD g=0; g<nGrp; g++) {
			if (flags&DBG_FLAGS_SELGRPONLY && g!=selgrp) continue;
			if (Grp[g]->UsrFlag & 0x2) continue;
			D3D9Effect::RenderBoundingSphere(pW, NULL, &Grp[g]->BBox.bs, &D3DXVECTOR4(0,1,0,0.75f));
		}
	}
	if (flags&DBG_FLAGS_BOXES) D3D9Effect::RenderBoundingBox(pW, &mTransform, &BBox.min, &BBox.max, &D3DXVECTOR4(0,0,1,0.75f));
	if (flags&DBG_FLAGS_SPHERES) D3D9Effect::RenderBoundingSphere(pW, &mTransform, &BBox.bs, &D3DXVECTOR4(0,0,1,0.75f));
}


// ===========================================================================================
//
void D3D9Mesh::BoundingBox(const NMVERTEX *vtx, DWORD n, D9BBox *box)
{
	if (!pVB) return;
	box->min.x = box->min.y = box->min.z =  1e12f;
	box->max.x = box->max.y = box->max.z = -1e12f;
	for (DWORD i=0;i<n;i++) {
		if (vtx[i].x < box->min.x) box->min.x=vtx[i].x;
		if (vtx[i].y < box->min.y) box->min.y=vtx[i].y;
		if (vtx[i].z < box->min.z) box->min.z=vtx[i].z;
		if (vtx[i].x > box->max.x) box->max.x=vtx[i].x;
		if (vtx[i].y > box->max.y) box->max.y=vtx[i].y;
		if (vtx[i].z > box->max.z) box->max.z=vtx[i].z;
	}

	box->min.w = box->max.w = 0.0f;
}


// ===========================================================================================
//
void D3D9Mesh::TransformGroup(DWORD n, const D3DXMATRIX *m)
{
	if (!pVB) return;

	bBSRecompute = true;

	Grp[n]->Transform = Grp[n]->Transform * (*m);
	Grp[n]->bTransform = true;
	Grp[n]->bUpdate = true;

	D3DXMatrixMultiply(&pGrpTF[n], &mTransform, &Grp[n]->Transform);
}

// ===========================================================================================
//
void D3D9Mesh::Transform(const D3DXMATRIX *m)
{
	if (!pVB) return;

	bBSRecompute = true;
	bBSRecomputeAll = true;
	bGlobalTF = true;
	mTransform = mTransform * (*m);

	D3DXMatrixInverse(&mTransformInv, NULL, &mTransform);

	for (DWORD i=0;i<nGrp;i++) {
		if (Grp[i]->bTransform) {
			if (bGlobalTF) D3DXMatrixMultiply(&pGrpTF[i], &mTransform, &Grp[i]->Transform);
			else pGrpTF[i] = Grp[i]->Transform;
		}
		else pGrpTF[i] = mTransform;
	}
}

// ===========================================================================================
//
void D3D9Mesh::UpdateBoundingBox()
{
	if (!pVB) return;
	if (bBSRecompute==false) return;

	for (DWORD i=0;i<nGrp;i++) {
		if (Grp[i]->bUpdate || bBSRecomputeAll) {
			Grp[i]->bUpdate = false;
			if (bGlobalTF) {
				if (Grp[i]->bTransform) D9UpdateAABB(&Grp[i]->BBox, &mTransform, &Grp[i]->Transform);
				else					D9UpdateAABB(&Grp[i]->BBox, &mTransform);
			}
			else {
				if (Grp[i]->bTransform) D9UpdateAABB(&Grp[i]->BBox, &Grp[i]->Transform);
				else					D9UpdateAABB(&Grp[i]->BBox);
			}
		}
	}

	bBSRecomputeAll = false;
	bBSRecompute = false;

	if (nGrp==0) {
		BBox.min = D3DXVECTOR4(0,0,0,0);
		BBox.max = D3DXVECTOR4(0,0,0,0);
	}
	else {
		for (DWORD i=0;i<nGrp;i++) {
			if (Grp[i]->bTransform) {
				if (bGlobalTF) {
					D3DXMATRIX q;
					D3DXMatrixMultiply(&q, D3DXMatrixMultiply(&q, &mTransform, &Grp[i]->Transform), &mTransformInv);
					D9AddAABB(&Grp[i]->BBox, &q, &BBox, i==0);
				}
				else D9AddAABB(&Grp[i]->BBox, &Grp[i]->Transform, &BBox, i==0);
			}
			else {
				D9AddAABB(&Grp[i]->BBox, NULL, &BBox, i==0);
			}
		}
	}

	D9UpdateAABB(&BBox, &mTransform);
}


// ===========================================================================================
//
D9BBox * D3D9Mesh::GetAABB()
{
	if (!pVB) return false;
	UpdateBoundingBox();
	return &BBox;
}

// ===========================================================================================
//
D3DXVECTOR3 D3D9Mesh::GetBoundingSpherePos()
{
	if (!pVB) return D3DXVECTOR3(0,0,0);
	UpdateBoundingBox();
	return D3DXVECTOR3f4(BBox.bs);
}

// ===========================================================================================
//
float D3D9Mesh::GetBoundingSphereRadius()
{
	if (!pVB) return 0.0f;
	UpdateBoundingBox();
	return BBox.bs.w;
}

// ===========================================================================================
//
D3D9Pick D3D9Mesh::Pick(const LPD3DXMATRIX pW, const D3DXVECTOR3 *vDir)
{
	D3D9Pick result;
	result.dist  = 1e30f;
	result.pMesh = NULL;
	result.vObj  = NULL;
	result.face  = -1;
	result.group = -1;

	if (!pGB || !pGI || !Geom) {
		LogErr("D3D9Mesh::Pick() Failed: No Geometry Available");
		return result;
	}

	UpdateBoundingBox();

	D3DXMATRIX mW, mWorldMesh;

	if (bGlobalTF) D3DXMatrixMultiply(&mWorldMesh, &mTransform, pW);
	else mWorldMesh = *pW;

	for (DWORD g=0;g<nGrp;g++) {

		if (Grp[g]->UsrFlag & 0x2) continue;

		D3DXVECTOR3 bs = D3DXVECTOR3f4(Grp[g]->BBox.bs);
		float rad = Grp[g]->BBox.bs.w;

		D3DXVec3TransformCoord(&bs, &bs, pW);

		BOOL bIntersect = D3DXSphereBoundProbe(&bs, rad, &D3DXVECTOR3(0,0,0), vDir);

		if (!bIntersect) continue;

		if (Grp[g]->bTransform) D3DXMatrixMultiply(&mW, &pGrpTF[g], pW);
		else mW = mWorldMesh;         

		DWORD *pIdc = NULL;
		D3DXVECTOR4 *pVrt = NULL;

		D3DXVECTOR3 _a, _b, _c, cp;

		WORD gr = Grp[g]->GeometryRec;

		HR(pGI->Lock(Grp[g]->GeoFOff*12, Grp[g]->nFace*12, (LPVOID*)&pIdc, 0));
		HR(pGB->Lock(Geom[gr].VertOff*sizeof(D3DXVECTOR4), Geom[gr].nVert*sizeof(D3DXVECTOR4), (LPVOID*)&pVrt, 0));
		
		
		for (DWORD i=0;i<Grp[g]->nFace;i++) {

			DWORD a = pIdc[i*3+0];
			DWORD b = pIdc[i*3+1];
			DWORD c = pIdc[i*3+2];
			
			D3DXVec3TransformCoord(&_a, &D3DXVECTOR3f4(pVrt[a]), &mW);
			D3DXVec3TransformCoord(&_b, &D3DXVECTOR3f4(pVrt[b]), &mW);
			D3DXVec3TransformCoord(&_c, &D3DXVECTOR3f4(pVrt[c]), &mW);

			float u, v, dst;

			D3DXVec3Cross(&cp, &(_a-_b), &(_c-_b));

			if (D3DXVec3Dot(&cp, vDir)>0) {
				if (D3DXIntersectTri(&_c, &_b, &_a, &D3DXVECTOR3(0,0,0), vDir, &u, &v, &dst)) {
					if (dst<result.dist) {
						result.dist  = dst;
						result.face  = int(i);
						result.group = int(g);
						result.pMesh = this;
					}
				}
			}
		}

		HR(pGB->Unlock());
		HR(pGI->Unlock());
	}

	return result;
}

// ===========================================================================================
//
void D3D9Mesh::DumpTextures()
{
	/*
	LogBlu("Mesh 0x%X has %u textures",this, nTex-1);
	if (Tex[0]!=NULL) LogErr("Texture in index 0");
	for (DWORD i=1;i<nTex;i++) {
		if (Tex[i]) LogBlu("Texture %u: 0x%X (%s)", i, Tex[i], Tex[i]->GetName());
		else        LogBlu("Texture %u: NULL");
	}*/
}

// ===========================================================================================
//
void D3D9Mesh::DumpGroups()
{
	/*
	LogAlw("Mesh 0x%X has %u groups", this, nGrp);
	for (DWORD i=0;i<nGrp;i++) {
		LogAlw("Group(%u):",i);
		LogAlw("VertexCount = %u",Grp[i]->nVert);
		LogAlw("FaceCount = %u",Grp[i]->nFace);
	}*/
}