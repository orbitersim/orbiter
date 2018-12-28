// ==============================================================
// Mesh.cpp
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2006 - 2016 Martin Schweiger
//				 2010 - 2016 Jarmo Nikkanen (D3D9Client implementation)
// ==============================================================

#define VISIBILITY_TOL 0.0015f

#include "Mesh.h"
#include "Log.h"
#include "Scene.h"
#include "D3D9Surface.h"
#include "D3D9Catalog.h"
#include "D3D9Config.h"
#include "DebugControls.h"
#include "MeshTools.h"
#include "VectorHelpers.h"
#include <xnamath.h>


using namespace oapi;


int compare_lights(const void * a, const void * b)
{
	register float fa = static_cast<const _LightList*>(a)->illuminace;
	register float fb = static_cast<const _LightList*>(b)->illuminace;
	if (fa < fb) return  1;
	if (fa > fb) return -1;
	return 0;
}

// ===========================================================================================
//
void D3D9Mesh::Null()
{
	pVB = NULL;
	pIB = NULL;
	pGB = NULL;
	nGrp = 0;
	Grp = NULL;
	nTex = 0;
	Tex	= NULL;
	pTune = NULL;
	nMtrl = 0;
	Mtrl = NULL;
	pGrpTF = NULL;
	sunLight = NULL;
	cAmbient = 0;
	MaxFace  = 0;
	MaxVert  = 0;
	vClass = 0;
	bDynamic = false;
	bGlobalTF = false;
	bBSRecompute = true;
	bBSRecomputeAll = true;
	bModulateMatAlpha = false;
	bIsReflective = false;
	bCanRenderFast = false;
	bMtrlModidied = false;

	Locals = new LightStruct[Config->MaxLights()];

	memset(Locals, 0, sizeof(LightStruct) * Config->MaxLights());
	memset(LightList, 0, sizeof(LightList));
	strcpy_s(name, 128, "???");
}

// ===========================================================================================
//
D3D9Mesh::D3D9Mesh(const char *name) : D3D9Effect()
{
	Null();
	MESHHANDLE hMesh = oapiLoadMesh(name);

	if (hMesh) {
		LoadMeshFromHandle(hMesh);
		oapiDeleteMesh(hMesh);
	}
}

// ===========================================================================================
//
D3D9Mesh::D3D9Mesh(MESHHANDLE hMesh, bool asTemplate, D3DXVECTOR3 *reorig) : D3D9Effect()
{
	Null();
	LoadMeshFromHandle(hMesh, reorig);
}


// ===========================================================================================
//
D3D9Mesh::D3D9Mesh(DWORD groups, const MESHGROUPEX **hGroup, const SURFHANDLE *hSurf) : D3D9Effect()
{
	Null();
	nGrp = groups;
	Grp = new GROUPREC[nGrp]; memset2(Grp, 0, sizeof(GROUPREC) * nGrp);

	for (DWORD i=0;i<nGrp;i++) {
		SetGroupRec(i, hGroup[i]); 
		Grp[i].TexIdxEx[0] = SPEC_DEFAULT;
		Grp[i].TexMixEx[0] = 0.0f;
		Grp[i].TexIdx  = i;
		Grp[i].MtrlIdx = SPEC_DEFAULT;
	}

	HR(pDev->CreateVertexBuffer(MaxVert*sizeof(NMVERTEX), 0, 0, D3DPOOL_DEFAULT, &pVB, NULL));
	HR(pDev->CreateIndexBuffer(MaxFace*sizeof(WORD)*3, 0, D3DFMT_INDEX16, D3DPOOL_DEFAULT, &pIB, NULL));

	nMtrl = 0;
	nTex = nGrp+1;
	Tex = new LPD3D9CLIENTSURFACE[nTex];
	Tex[0] = 0; // 'no texture'
	for (DWORD i=1;i<nTex;i++) Tex[i] = SURFACE(hSurf[i-1]);

	ProcessInherit();

	for (DWORD i=0;i<nGrp;i++) CopyVertices(&Grp[i], hGroup[i]);

	pGrpTF = new D3DXMATRIX[nGrp];

	D3DXMatrixIdentity(&mTransform);
	D3DXMatrixIdentity(&mTransformInv);
	MeshCatalog->Add(this);

	UpdateBoundingBox();
	UpdateGeometryBuffer();
	CheckMeshStatus();
}


// ===========================================================================================
//
D3D9Mesh::D3D9Mesh(const MESHGROUPEX *pGroup, const MATERIAL *pMat, D3D9ClientSurface *pTex) : D3D9Effect()
{
	Null();
	
	// template meshes are stored in system memory
	nGrp   = 1;
	Grp    = new GROUPREC[nGrp]; memset2(Grp, 0, sizeof(GROUPREC) * nGrp);
	nTex   = 2;
	Tex	   = new LPD3D9CLIENTSURFACE[nTex];
	Tex[0] = 0; // 'no texture'
	Tex[1] = pTex;
	nMtrl  = 1;
	Mtrl   = new D3D9MatExt[nMtrl];
	pGrpTF = new D3DXMATRIX[nGrp];

	SetGroupRec(0, pGroup); 

	HR(pDev->CreateVertexBuffer(MaxVert*sizeof(NMVERTEX), 0, 0, D3DPOOL_DEFAULT, &pVB, NULL));
	HR(pDev->CreateIndexBuffer(MaxFace*sizeof(WORD)*3, 0, D3DFMT_INDEX16, D3DPOOL_DEFAULT, &pIB, NULL));

	SetMaterial((const D3DMATERIAL9*)pMat, 0, false);
	CopyVertices(&Grp[0], pGroup);

	D3DXMatrixIdentity(&mTransform);
	D3DXMatrixIdentity(&mTransformInv);
	MeshCatalog->Add(this);

	UpdateBoundingBox();
	UpdateGeometryBuffer();
	CheckMeshStatus();
}


// ===========================================================================================
//
D3D9Mesh::D3D9Mesh(class AdMesh &mesh, bool bHasUV) : D3D9Effect()
{
	Null();

	nGrp    = mesh.GetGroupCount();
	MaxVert = mesh.GetVertexCount();
	MaxFace = mesh.GetIndexCount() / 3;

	Grp = new GROUPREC[nGrp]; memset2(Grp, 0, sizeof(GROUPREC) * nGrp);
	pGrpTF = new D3DXMATRIX[nGrp];

	HR(pDev->CreateVertexBuffer(MaxVert*sizeof(NMVERTEX), 0, 0, D3DPOOL_DEFAULT, &pVB, NULL));
	HR(pDev->CreateIndexBuffer(MaxFace*sizeof(WORD)*3, 0, D3DFMT_INDEX16, D3DPOOL_DEFAULT, &pIB, NULL));

	NMVERTEX *pVert = NULL;
	WORD *pIdx = NULL;

	HR(pVB->Lock(0, 0, (LPVOID*)&pVert, 0));
	HR(pIB->Lock(0, 0, (LPVOID*)&pIdx, 0));

	for (DWORD i=0;i<nGrp;i++) {

		HGROUP hGrp = mesh.GetGroup(i);
		DWORD  nVtx = hGrp->GetVertexCount();
		DWORD  nIdx = hGrp->GetIndexCount();

		hGrp->GetVertices(pVert);
		hGrp->GetIndices(pIdx);

		UpdateTangentSpace(pVert, pIdx, nVtx, nIdx/3, bHasUV);

		pVert += nVtx;
		pIdx  += nIdx;

		Grp[i].nVert = nVtx;
		Grp[i].nFace = nIdx/3;

		if (i>0) {
			Grp[i].VertOff = Grp[i-1].VertOff + nVtx;
			Grp[i].FaceOff = Grp[i-1].FaceOff + nIdx/3;
		}
		else {
			Grp[0].VertOff = 0;
			Grp[0].FaceOff = 0;
		}
	}

	HR(pVB->Unlock());
	HR(pIB->Unlock());

	D3DXMatrixIdentity(&mTransform);
	D3DXMatrixIdentity(&mTransformInv);
	MeshCatalog->Add(this);

	UpdateBoundingBox();
	UpdateGeometryBuffer();
	CheckMeshStatus();
}


// ===========================================================================================
//
void D3D9Mesh::Copy(const D3D9Mesh &mesh)
{
	Null();

	bModulateMatAlpha = mesh.bModulateMatAlpha;
	strcpy_s(name, 128, mesh.name);

	nGrp = mesh.nGrp;
	Grp = new GROUPREC[nGrp];
	pGrpTF = new D3DXMATRIX[nGrp];

	MaxFace = mesh.MaxFace;
	MaxVert = mesh.MaxVert;

	memcpy2(Grp, mesh.Grp, sizeof(GROUPREC)*nGrp);
	
	HR(pDev->CreateVertexBuffer(MaxVert*sizeof(NMVERTEX), 0, 0, D3DPOOL_DEFAULT, &pVB, NULL));
	HR(pDev->CreateIndexBuffer(MaxFace*sizeof(WORD)*3, 0, D3DFMT_INDEX16, D3DPOOL_DEFAULT, &pIB, NULL));

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

	if (mesh.pGB) {

		// Create Geometry Buffers
		HR(pDev->CreateVertexBuffer(MaxVert*sizeof(D3DXVECTOR4), 0, 0, D3DPOOL_DEFAULT, &pGB, NULL));

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

	MeshCatalog->Add(this);

	UpdateBoundingBox();
	CheckMeshStatus();
}


// ===========================================================================================
//
D3D9Mesh::~D3D9Mesh()
{
	_TRACE;
	if (!pVB) return;

	if (MeshCatalog->Remove(this)) LogAlw("Mesh 0x%X Removed from catalog",this);
	else 						   LogErr("Mesh 0x%X wasn't in meshcatalog",this);

	Release();

	LogOk("Mesh 0x%X Deleted successfully -------------------------------",this);
}


// ===========================================================================================
//
void D3D9Mesh::Release()
{
	SAFE_DELETEA(Locals);
	SAFE_DELETEA(Grp); 
	SAFE_DELETEA(Tex);
	SAFE_DELETEA(Mtrl);
	SAFE_DELETEA(pGrpTF);
	SAFE_DELETEA(pTune);
	SAFE_RELEASE(pIB);
	SAFE_RELEASE(pVB);
	SAFE_RELEASE(pGB);
}


// ===========================================================================================
//
void D3D9Mesh::ReLoadMeshFromHandle(MESHHANDLE hMesh)
{
	Release();
	Null();
	LoadMeshFromHandle(hMesh);
}


// ===========================================================================================
//
void D3D9Mesh::LoadMeshFromHandle(MESHHANDLE hMesh, D3DXVECTOR3 *reorig)
{
	nGrp = oapiMeshGroupCount(hMesh);
	if (nGrp == 0) return;

	Grp = new GROUPREC[nGrp]; memset2(Grp, 0, sizeof(GROUPREC) * nGrp);
	for (DWORD i = 0; i<nGrp; i++) SetGroupRec(i, oapiMeshGroupEx(hMesh, i));
	if (MaxVert == 0 || MaxFace == 0) return;

	HR(pDev->CreateVertexBuffer(MaxVert*sizeof(NMVERTEX), 0, 0, D3DPOOL_DEFAULT, &pVB, NULL));
	HR(pDev->CreateIndexBuffer(MaxFace*sizeof(WORD) * 3, 0, D3DFMT_INDEX16, D3DPOOL_DEFAULT, &pIB, NULL));

	// -----------------------------------------------------------------------
	nTex = oapiMeshTextureCount(hMesh) + 1;
	Tex = new LPD3D9CLIENTSURFACE[nTex];
	Tex[0] = 0; // 'no texture'
	for (DWORD i = 1; i<nTex; i++) Tex[i] = SURFACE(oapiGetTextureHandle(hMesh, i));

	// -----------------------------------------------------------------------
	nMtrl = oapiMeshMaterialCount(hMesh);
	if (nMtrl) Mtrl = new D3D9MatExt[nMtrl];
	for (DWORD i = 0; i<nMtrl; i++)	SetMaterial((const D3DMATERIAL9*)oapiMeshMaterial(hMesh, i), i, false);

	ProcessInherit();

	for (DWORD i = 0; i<nGrp; i++) CopyVertices(&Grp[i], oapiMeshGroupEx(hMesh, i), reorig);

	pGrpTF = new D3DXMATRIX[nGrp];

	D3DXMatrixIdentity(&mTransform);
	D3DXMatrixIdentity(&mTransformInv);
	MeshCatalog->Add(this);

	UpdateBoundingBox();
	UpdateGeometryBuffer();
	CheckMeshStatus();
}


// ===========================================================================================
//
void D3D9Mesh::ConvertToDynamic()
{
	if (bDynamic) return;
	bDynamic = true;

	LPDIRECT3DVERTEXBUFFER9 pVN = NULL;
	LPDIRECT3DINDEXBUFFER9 pIN = NULL;

	HR(pDev->CreateVertexBuffer(MaxVert*sizeof(NMVERTEX), D3DUSAGE_DYNAMIC, 0, D3DPOOL_DEFAULT, &pVN, NULL));
	HR(pDev->CreateIndexBuffer(MaxFace*sizeof(WORD) * 3, D3DUSAGE_DYNAMIC, D3DFMT_INDEX16, D3DPOOL_DEFAULT, &pIN, NULL));
	LPVOID pSrc, pTgt;

	HR(pVB->Lock(0, 0, (LPVOID*)&pSrc, 0));
	HR(pVN->Lock(0, 0, (LPVOID*)&pTgt, 0));
	memcpy2(pTgt, pSrc, MaxVert*sizeof(NMVERTEX));
	HR(pVB->Unlock());
	HR(pVN->Unlock());
	HR(pIB->Lock(0, 0, (LPVOID*)&pSrc, 0));
	HR(pIN->Lock(0, 0, (LPVOID*)&pTgt, 0));
	memcpy2(pTgt, pSrc, MaxFace * 6);
	HR(pIB->Unlock());
	HR(pIN->Unlock());
	SAFE_RELEASE(pVB);
	SAFE_RELEASE(pIB);
	pVB = pVN;
	pIB = pIN;
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
		if (Grp[g].UsrFlag & 3) continue;
		if (Grp[g].IntFlag & 3) continue;
		return true;
	}
	return false;
}


// ===========================================================================================
//
void D3D9Mesh::ProcessInherit()
{
	_TRACE;
	if (!pVB) return;
	if (Grp[0].MtrlIdx == SPEC_INHERIT) Grp[0].MtrlIdx = SPEC_DEFAULT;
	if (Grp[0].TexIdx == SPEC_INHERIT) Grp[0].TexIdx = SPEC_DEFAULT;
	if (Grp[0].TexIdxEx[0] == SPEC_INHERIT) Grp[0].TexIdxEx[0] = SPEC_DEFAULT;

	bool bPopUp = false;

	for (DWORD i=0;i<nGrp;i++) {

		if (Grp[i].UsrFlag & 0x8) LogErr("MeshGroupFlag 0x8 in use (OPERATION NOT IMPLEMENTED)");

		// Inherit Material
		if (Grp[i].MtrlIdx == SPEC_INHERIT) Grp[i].MtrlIdx = Grp[i-1].MtrlIdx;

		// Inherit Texture
		if (Grp[i].TexIdx == SPEC_DEFAULT) Grp[i].TexIdx = 0;
		else if (Grp[i].TexIdx == SPEC_INHERIT) Grp[i].TexIdx = Grp[i-1].TexIdx;
		else Grp[i].TexIdx++;

		// Inherit Night Texture
		if (Grp[i].TexIdxEx[0] == SPEC_DEFAULT) Grp[i].TexIdxEx[0] = 0;
		else if (Grp[i].TexIdxEx[0] == SPEC_INHERIT) Grp[i].TexIdxEx[0] = Grp[i-1].TexIdxEx[0];
		else Grp[i].TexIdxEx[0]++;

		// Do some safety checks
		if (Grp[i].TexIdx>=nTex) {
			LogErr("Mesh(0x%X) has a texture index %u in group %u out of range. Constr=%u", this, Grp[i].TexIdx, i, Constr);
			Grp[i].TexIdx = 0;
			bPopUp = true;
		}
		if (Grp[i].TexIdxEx[0]>=nTex) {
			LogErr("Mesh(0x%X) has a night texture index %u in group %u out of range. Constr=%u", this, Grp[i].TexIdxEx[0], i, Constr);
			Grp[i].TexIdxEx[0] = 0;
			bPopUp = true;
		}

		if (Grp[i].MtrlIdx!=SPEC_DEFAULT) {
			if (Grp[i].MtrlIdx>=nMtrl) {
				LogErr("Mesh(0x%X) has a material index %u in group %u out of range. Constr=%u", this, Grp[i].MtrlIdx, i, Constr);
				Grp[i].MtrlIdx = SPEC_DEFAULT;
				bPopUp = true;
			}
		}
	}
	if (bPopUp) MessageBoxA(NULL, "Invalid Mesh Detected", "D3D9Client Error:",MB_OK);
}


// ===========================================================================================
//
void D3D9Mesh::UpdateGeometryBuffer(int grp)
{
	if (!pVB) return;

	if (!pGB) {
		HR(pDev->CreateVertexBuffer(MaxVert * sizeof(D3DXVECTOR4), 0, 0, D3DPOOL_DEFAULT, &pGB, NULL));
	}

	NMVERTEX *pVSrc = NULL;
	D3DXVECTOR4 *pVTgt = NULL;

	HR(pVB->Lock(0, 0, (LPVOID*)&pVSrc, 0));
	HR(pGB->Lock(0, 0, (LPVOID*)&pVTgt, 0));

	if (!pVSrc || !pVTgt) {
		LogErr("UpdateGeometryBuffer() Failed to lock a buffer");
		return;
	}

	if (grp == -1) {
		for (DWORD v = 0; v < MaxVert; v++) pVTgt[v] = D3DXVECTOR4(pVSrc[v].x, pVSrc[v].y, pVSrc[v].z, 0);
	}
	else {
		DWORD nV = Grp[grp].nVert;
		DWORD vo = Grp[grp].VertOff;
		for (DWORD v = 0; v < nV; v++) pVTgt[vo + v] = D3DXVECTOR4(pVSrc[vo + v].x, pVSrc[vo + v].y, pVSrc[vo + v].z, 0);
	}

	HR(pVB->Unlock());
	HR(pGB->Unlock());
}


// ===========================================================================================
//
D3DXVECTOR3 D3D9Mesh::GetGroupSize(DWORD idx)
{
	if (!pVB) return D3DXVECTOR3(0,0,0);
	if (idx>=nGrp) return D3DXVECTOR3(0,0,0);
	if (Grp[idx].nVert<2) return D3DXVECTOR3(0,0,0);
	return D3DXVECTOR3f4(Grp[idx].BBox.max - Grp[idx].BBox.min);
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
		D3DXMatrixIdentity(&Grp[i].Transform);
		D3DXMatrixIdentity(&pGrpTF[i]);
		Grp[i].bTransform = false;
	}
}


// ===========================================================================================
//
void D3D9Mesh::UpdateTangentSpace(NMVERTEX *pVrt, WORD *pIdx, DWORD nVtx, DWORD nFace, bool bTextured)
{
	if (!pVB) return;

	if (bTextured) {
	
		XMVECTOR *ta = (XMVECTOR*)_aligned_malloc(sizeof(__m128)*(nVtx+1), 16);
		XMVECTOR zero = XMVectorSet(0, 0, 0, 0);
		for (DWORD i = 0; i < nVtx; i++) ta[i] = zero;

		for (DWORD i=0;i<nFace;i++) {

			DWORD i0 = pIdx[i*3];
			DWORD i1 = pIdx[i*3+1];
			DWORD i2 = pIdx[i*3+2];

			XMVECTOR r0 = XMLoadFloat3((XMFLOAT3*)&pVrt[i0].x);
			XMVECTOR r1 = XMLoadFloat3((XMFLOAT3*)&pVrt[i1].x);
			XMVECTOR r2 = XMLoadFloat3((XMFLOAT3*)&pVrt[i2].x);
			D3DXVECTOR2 t0 = D3DXVECTOR2(pVrt[i0].u, pVrt[i0].v);
			D3DXVECTOR2 t1 = D3DXVECTOR2(pVrt[i1].u, pVrt[i1].v);
			D3DXVECTOR2 t2 = D3DXVECTOR2(pVrt[i2].u, pVrt[i2].v);

			float u0 = t1.x - t0.x;
			float v0 = t1.y - t0.y;
			float u1 = t2.x - t0.x;
			float v1 = t2.y - t0.y;

			XMVECTOR k0 = r1 - r0;
			XMVECTOR k1 = r2 - r0;

			float q = (u0*v1-u1*v0);
			if (q==0) q = 1.0f;
			else q = 1.0f / q;

			XMVECTOR t = ((k0*v1 - k1*v0) * q);
			ta[i0]+=t; ta[i1]+=t; ta[i2]+=t;
			pVrt[i0].w = pVrt[i1].w = pVrt[i2].w = (q<0.0f ? 1.0f : -1.0f);
		}

		for (DWORD i=0;i<nVtx; i++) {
			XMVECTOR n = XMVector3Normalize(XMLoadFloat3((XMFLOAT3*)&pVrt[i].nx));
			XMVECTOR t = XMVector3Normalize((ta[i] - n * XMVector3Dot(ta[i], n)));
			XMStoreFloat3((XMFLOAT3*)&pVrt[i].tx, t);
		}

		_aligned_free(ta);
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

/*void D3D9Mesh::UpdateTangentSpace(NMVERTEX *pVrt, WORD *pIdx, DWORD nVtx, DWORD nFace, bool bTextured)
{
	if (!pVB) return;

	if (bTextured) {

		D3DXVECTOR3 *ta = new D3DXVECTOR3[nVtx];

		for (DWORD i = 0; i<nVtx; i++) ta[i] = D3DXVECTOR3(0, 0, 0);

		for (DWORD i = 0; i<nFace; i++) {

			DWORD i0 = pIdx[i * 3];
			DWORD i1 = pIdx[i * 3 + 1];
			DWORD i2 = pIdx[i * 3 + 2];

			D3DXVECTOR3 r0 = D3DXVECTOR3(pVrt[i0].x, pVrt[i0].y, pVrt[i0].z);
			D3DXVECTOR3 r1 = D3DXVECTOR3(pVrt[i1].x, pVrt[i1].y, pVrt[i1].z);
			D3DXVECTOR3 r2 = D3DXVECTOR3(pVrt[i2].x, pVrt[i2].y, pVrt[i2].z);
			D3DXVECTOR2 t0 = D3DXVECTOR2(pVrt[i0].u, pVrt[i0].v);
			D3DXVECTOR2 t1 = D3DXVECTOR2(pVrt[i1].u, pVrt[i1].v);
			D3DXVECTOR2 t2 = D3DXVECTOR2(pVrt[i2].u, pVrt[i2].v);

			float u0 = t1.x - t0.x;
			float v0 = t1.y - t0.y;
			float u1 = t2.x - t0.x;
			float v1 = t2.y - t0.y;

			D3DXVECTOR3 k0 = r1 - r0;
			D3DXVECTOR3 k1 = r2 - r0;

			float q = (u0*v1 - u1*v0);
			if (q == 0) q = 1.0f;
			else q = 1.0f / q;

			D3DXVECTOR3 t = ((k0*v1 - k1*v0) * q);
			ta[i0] += t; ta[i1] += t; ta[i2] += t;
			pVrt[i0].w = pVrt[i1].w = pVrt[i2].w = (q<0.0f ? 1.0f : -1.0f);
		}

		for (DWORD i = 0; i<nVtx; i++) {

			D3DXVECTOR3 n = D3DXVECTOR3(pVrt[i].nx, pVrt[i].ny, pVrt[i].nz);
			D3DXVec3Normalize(&n, &n);
			D3DXVECTOR3 t = (ta[i] - n * D3DXVec3Dot(&ta[i], &n));
			D3DXVec3Normalize(&t, &t);

			pVrt[i].tx = t.x;
			pVrt[i].ty = t.y;
			pVrt[i].tz = t.z;
		}

		delete[]ta;
	}
	else {
		for (DWORD i = 0; i<nVtx; i++) {
			D3DXVECTOR3 n = D3DXVECTOR3(pVrt[i].nx, pVrt[i].ny, pVrt[i].nz);
			D3DXVECTOR3 t = Perpendicular(&n);
			D3DXVec3Normalize(&t, &t);
			pVrt[i].tx = t.x;
			pVrt[i].ty = t.y;
			pVrt[i].tz = t.z;
		}
	}
}*/


// ===========================================================================================
//
void D3D9Mesh::SetGroupRec(DWORD i, const MESHGROUPEX *mg)
{
	if (i>=nGrp) return;
	memcpy2(Grp[i].TexIdxEx, mg->TexIdxEx, MAXTEX*sizeof(DWORD));
	memcpy2(Grp[i].TexMixEx, mg->TexMixEx, MAXTEX*sizeof(float));
	Grp[i].TexIdx  = mg->TexIdx;
	Grp[i].MtrlIdx = mg->MtrlIdx;
	Grp[i].FaceOff = MaxFace;
	Grp[i].VertOff = MaxVert;
	Grp[i].nFace   = mg->nIdx/3;
	Grp[i].nVert   = mg->nVtx;
	MaxFace += Grp[i].nFace;
	MaxVert += Grp[i].nVert;	
}


// ===========================================================================================
//
bool D3D9Mesh::CopyVertices(GROUPREC *grp, const MESHGROUPEX *mg, D3DXVECTOR3 *reorig)
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

		if (reorig) {
			pVert[i].x += reorig->x;
			pVert[i].y += reorig->y;
			pVert[i].z += reorig->z;
		}
	}

	// Check vertex index errors (This is important)
	//
	for (DWORD i=0;i<(mg->nIdx/3);i++) {
		DWORD v0 = i*3;	DWORD v1 = v0+1; DWORD v2 = v0+2;
		if (pIndex[v0]>=mg->nVtx || pIndex[v1]>=mg->nVtx || pIndex[v2]>=mg->nVtx) {
			pIndex[v0] = pIndex[v1] = pIndex[v2] = 0;
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
//
void D3D9Mesh::UpdateGroup(MESHHANDLE hMesh, DWORD idx)
{
	_TRACE;
	if (!pVB) return;
	if (hMesh) {
		MESHGROUPEX *mg = oapiMeshGroupEx(hMesh, idx);
		if (mg) {
			assert(mg->nVtx == Grp[idx].nVert);
			assert(mg->nIdx == Grp[idx].nFace*3);
			CopyVertices(&Grp[idx], mg);
			UpdateGeometryBuffer();
		}
	}
}


// ===========================================================================================
// Mesh Update routine for AMSO
//
void D3D9Mesh::UpdateGroupEx(DWORD idx, const MESHGROUPEX *mg)
{
	_TRACE;
	if (!pVB) return;
	GROUPREC *grp = &Grp[idx];
	NMVERTEX *pVert = LockVertexBuffer(idx,0);
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
// This is required by Client implementation see clbkEditMeshGroup
//
int D3D9Mesh::EditGroup(DWORD grp, GROUPEDITSPEC *ges)
{
	_TRACE;
	if (!pVB) return 1;
	if (grp >= nGrp) return 1;

	bBSRecompute = true;

	GROUPREC *g = &Grp[grp];
	DWORD flag = ges->flags;
	DWORD old  = g->UsrFlag;

	if (flag & GRPEDIT_SETUSERFLAG)	     g->UsrFlag  = ges->UsrFlag;
	else if (flag & GRPEDIT_ADDUSERFLAG) g->UsrFlag |= ges->UsrFlag;
	else if (flag & GRPEDIT_DELUSERFLAG) g->UsrFlag &= ~ges->UsrFlag;

	if (flag & GRPEDIT_VTX) {

		if (!bDynamic) ConvertToDynamic();

		NMVERTEX *vtx = LockVertexBuffer(grp,0);
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
				if (pIB->Lock(g->FaceOff*6, g->nFace*6, (LPVOID*)&idx, D3DLOCK_READONLY)==S_OK) {
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
	DWORD nv = Grp[grp].nVert;
	DWORD ni = Grp[grp].nFace*3;
	DWORD i, vi;
	int ret = 0;

	if (grs->nVtx && grs->Vtx) { // vertex data requested
		NMVERTEX *vtx = LockVertexBuffer(grp, D3DLOCK_READONLY);
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
		WORD *idx = LockIndexBuffer(grp, D3DLOCK_READONLY);
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

	grs->MtrlIdx = Grp[grp].MtrlIdx;
	grs->TexIdx = Grp[grp].TexIdx;
	return ret;
}

// ===========================================================================================
//
void D3D9Mesh::SetMFDScreenId(DWORD idx, WORD id)
{
	if (idx<nGrp) Grp[idx].MFDScreenId = id;
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
	CheckMeshStatus();
	return true;
}

// ===========================================================================================
//
DWORD D3D9Mesh::GetMeshGroupMaterialIdx(DWORD idx) const
{
	if (!pVB) return 0;
	if (idx>=nGrp) return 0;
	return Grp[idx].MtrlIdx;
}

// ===========================================================================================
//
DWORD D3D9Mesh::GetMeshGroupTextureIdx(DWORD idx) const
{
	if (!pVB) return 0;
	if (idx>=nGrp) return 0;
	return Grp[idx].TexIdx;
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
void D3D9Mesh::SetTexMixture(DWORD ntex, float mix)
{
	_TRACE;
	if (!pVB) return;
	ntex--;
	for (DWORD g = 0; g < nGrp; g++) if (Grp[g].TexIdxEx[ntex] != SPEC_DEFAULT) Grp[g].TexMixEx[ntex] = mix;
}

// ===========================================================================================
//
void D3D9Mesh::SetSunLight(const D3D9Sun *light)
{
	if (!pVB) return;
	sunLight = light;
}


// ===========================================================================================
//
NMVERTEX * D3D9Mesh::LockVertexBuffer(DWORD grp, DWORD flags)
{
	if (!pVB) return NULL;
	NMVERTEX *pVert;
	bBSRecompute = true;

	if (grp>=nGrp) {
		LogErr("D3D9Mesh(0x%X)::GetVertexBuffer(%u) index out of range",this,grp);
		return NULL;
	}

	double time = D3D9GetTime();

	if (pVB->Lock(Grp[grp].VertOff*sizeof(NMVERTEX), Grp[grp].nVert*sizeof(NMVERTEX), (LPVOID*)&pVert, flags)==S_OK) {
		D3D9SetTime(D3D9Stats.Timer.LockWait, time);
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

WORD * D3D9Mesh::LockIndexBuffer(DWORD grp, DWORD flags)
{
	if (!pIB) return NULL;
	WORD *pIdx;
	bBSRecompute = true;

	if (grp>=nGrp) {
		LogErr("D3D9Mesh(0x%X)::LockIndexBuffer(%u) index out of range",this,grp);
		return NULL;
	}

	if (pIB->Lock(Grp[grp].FaceOff*6, Grp[grp].nFace*6, (LPVOID*)&pIdx, flags)==S_OK) {
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
DWORD D3D9Mesh::GetVertexCount(int grp) const
{
	if (grp<0) return MaxVert;
	else return Grp[grp].nVert;
}

// ===========================================================================================
//
DWORD D3D9Mesh::GetIndexCount(int grp) const
{
	if (grp<0) return MaxFace*3;
	else return Grp[grp].nFace*3;
}

// ===========================================================================================
//
DWORD D3D9Mesh::GetGroupTransformCount() const
{
	DWORD cnt = 0;
	for (DWORD i=0;i<nGrp;i++) if (Grp[i].bTransform) cnt++;
	return cnt;
}

// ===========================================================================================
//
const D3D9Mesh::GROUPREC *D3D9Mesh::GetGroup(DWORD idx) const
{
	if (!pVB) return NULL;
	if (idx<nGrp) return &Grp[idx];
	return NULL;
}

// ===========================================================================================
//
const D3D9MatExt * D3D9Mesh::GetMaterial(DWORD idx) const
{
	if (idx >= nMtrl) return NULL;
	return &Mtrl[idx];
}

// ===========================================================================================
//
bool D3D9Mesh::GetMaterial(D3D9MatExt *pMat, DWORD idx) const
{
	if (pMat && idx<nMtrl) {
		memcpy(pMat, &Mtrl[idx], sizeof(D3D9MatExt));
		return true;
	}
	return false;
}

// ===========================================================================================
//
void D3D9Mesh::SetMaterial(const D3DMATERIAL9 *pMat, DWORD idx, bool bStat)
{
	D3D9MatExt Mat;
	CreateMatExt(pMat, &Mat);
	SetMaterial(&Mat, idx, bStat);
}

// ===========================================================================================
//
void D3D9Mesh::SetMaterial(const D3D9MatExt *pMat, DWORD idx, bool bStat)
{
	if (idx < nMtrl) {
		memcpy2(&Mtrl[idx], pMat, sizeof(D3D9MatExt));

		if (Mtrl[idx].Specular.w < 0.1f) {
			Mtrl[idx].Specular.x = 0.0f;
			Mtrl[idx].Specular.y = 0.0f;
			Mtrl[idx].Specular.z = 0.0f;
		}
	}
	if (bStat) CheckMeshStatus();
}

// ===========================================================================================
// -1 = idx out of range
// -2 = material property not defined cannot get it
// -3 = invalid marerial id (mid)
// -4 = invalid input parameters
//
int D3D9Mesh::Material(DWORD idx, int mid, COLOUR4 *value, bool bSet)
{

	if (idx >= nMtrl) return -1;

	// SET ---------------------------------------------------------------
	if (bSet && value) {
		switch (mid) {
		case MESHM_DIFFUSE: 
			Mtrl[idx].Diffuse = *((D3DXVECTOR4*)value); 
			Mtrl[idx].ModFlags |= D3D9MATEX_DIFFUSE;
			bMtrlModidied = true;
			return 0;
		case MESHM_AMBIENT: 
			Mtrl[idx].Ambient = *((D3DXVECTOR3*)value); 
			Mtrl[idx].ModFlags |= D3D9MATEX_AMBIENT;
			bMtrlModidied = true;
			return 0;
		case MESHM_SPECULAR: 
			Mtrl[idx].Specular = *((D3DXVECTOR4*)value); 
			Mtrl[idx].ModFlags |= D3D9MATEX_SPECULAR;
			bMtrlModidied = true;
			return 0;
		case MESHM_EMISSION:
			Mtrl[idx].Emissive = *((D3DXVECTOR3*)value); 
			Mtrl[idx].ModFlags |= D3D9MATEX_EMISSIVE;
			bMtrlModidied = true;
			return 0;
		case MESHM_EMISSION2: 
			Mtrl[idx].Emission2 = *((D3DXVECTOR3*)value); 
			Mtrl[idx].ModFlags |= D3D9MATEX_EMISSION2;
			bMtrlModidied = true;
			return 0;
		case MESHM_REFLECT: 
			Mtrl[idx].Reflect = *((D3DXVECTOR3*)value); 
			Mtrl[idx].ModFlags |= D3D9MATEX_REFLECT;
			bMtrlModidied = true;
			return 0;
		case MESHM_ROUGHNESS:
			Mtrl[idx].Roughness = value->g; 
			Mtrl[idx].ModFlags |= D3D9MATEX_ROUGHNESS;
			bMtrlModidied = true;
			return 0;
		case MESHM_FRESNEL: 
			Mtrl[idx].Fresnel = *((D3DXVECTOR3*)value);
			Mtrl[idx].ModFlags |= D3D9MATEX_FRESNEL;
			bMtrlModidied = true;
			return 0;
		}
		return -3;
	}

	// GET ---------------------------------------------------------------
	if ((!bSet) && value) {
		switch (mid) {
		case MESHM_DIFFUSE:
			*((D3DXVECTOR4*)value) = Mtrl[idx].Diffuse;
			return 0;
		case MESHM_AMBIENT:
			*((D3DXVECTOR3*)value) = Mtrl[idx].Ambient;
			return 0;
		case MESHM_SPECULAR:
			*((D3DXVECTOR4*)value) = Mtrl[idx].Specular;
			return 0;
		case MESHM_EMISSION:
			*((D3DXVECTOR3*)value) = Mtrl[idx].Emissive;
			return 0;
		case MESHM_EMISSION2:
			if ((Mtrl[idx].ModFlags&D3D9MATEX_EMISSION2) == 0) return -2;
			*((D3DXVECTOR3*)value) = Mtrl[idx].Emission2;
			return 0;
		case MESHM_REFLECT:
			if ((Mtrl[idx].ModFlags&D3D9MATEX_REFLECT) == 0) return -2;
			*((D3DXVECTOR3*)value) = Mtrl[idx].Reflect;
			return 0;
		case MESHM_ROUGHNESS:
			if ((Mtrl[idx].ModFlags&D3D9MATEX_ROUGHNESS) == 0) return -2;
			value->g = Mtrl[idx].Roughness;
			return 0;
		case MESHM_FRESNEL:
			if ((Mtrl[idx].ModFlags&D3D9MATEX_FRESNEL) == 0) return -2;
			*((D3DXVECTOR3*)value) = Mtrl[idx].Fresnel;
			return 0;
		}
		return -3;
	}

	// CLEAR -------------------------------------------------------------
	if (value==NULL) {
		switch (mid) {
		case MESHM_DIFFUSE:
			Mtrl[idx].Diffuse = D3DXVECTOR4(1, 1, 1, 1);
			Mtrl[idx].ModFlags &= (~D3D9MATEX_DIFFUSE);
			bMtrlModidied = true;
			return 0;
		case MESHM_AMBIENT:
			Mtrl[idx].Ambient = D3DXVECTOR3(1, 1, 1);
			Mtrl[idx].ModFlags &= (~D3D9MATEX_AMBIENT);
			bMtrlModidied = true;
			return 0;
		case MESHM_SPECULAR:
			Mtrl[idx].Specular = D3DXVECTOR4(1, 1, 1, 20.0);
			Mtrl[idx].ModFlags &= (~D3D9MATEX_SPECULAR);
			bMtrlModidied = true;
			return 0;
		case MESHM_EMISSION:
			Mtrl[idx].Emissive = D3DXVECTOR3(0, 0, 0);
			Mtrl[idx].ModFlags &= (~D3D9MATEX_EMISSIVE);
			bMtrlModidied = true;
			return 0;
		case MESHM_EMISSION2: 
			Mtrl[idx].Emission2 = D3DXVECTOR3(1, 1, 1); 
			Mtrl[idx].ModFlags &= (~D3D9MATEX_EMISSION2); 
			bMtrlModidied = true;
			return 0;
		case MESHM_REFLECT: 
			Mtrl[idx].Reflect = D3DXVECTOR3(0,0,0); 
			Mtrl[idx].ModFlags &= (~D3D9MATEX_REFLECT); 
			bMtrlModidied = true;
			return 0;
		case MESHM_ROUGHNESS: 
			Mtrl[idx].Roughness = 0.0f;  
			Mtrl[idx].ModFlags &= (~D3D9MATEX_ROUGHNESS); 
			bMtrlModidied = true;
			return 0;
		case MESHM_FRESNEL: 
			Mtrl[idx].Fresnel = D3DXVECTOR3(1, 0, 1024.0f); 
			Mtrl[idx].ModFlags &= (~D3D9MATEX_FRESNEL); 
			bMtrlModidied = true;
			return 0;
		}
		return -3;
	}
	return -4;
}

// ===========================================================================================
//
bool D3D9Mesh::GetTexTune(D3D9Tune *pT, DWORD idx) const
{
	if (idx<nTex && idx!=0) {
		if (!pTune) D3D9TuneInit(pT);
		else memcpy(pT, &pTune[idx], sizeof(D3D9Tune));
		return true;
	}
	return false;
}

// ===========================================================================================
//
void D3D9Mesh::SetTexTune(const D3D9Tune *pT, DWORD idx)
{
	if (idx < nTex && idx != 0) {
		if (!pTune) {
			pTune = new D3D9Tune[nTex];
			for (DWORD i = 0; i < nTex; i++) D3D9TuneInit(&pTune[i]);
		}
		memcpy(&pTune[idx], pT, sizeof(D3D9Tune));
	}
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
void D3D9Mesh::RenderGroup(int idx)
{
	RenderGroup(GetGroup(idx));
}

// ===========================================================================================
//
void D3D9Mesh::RenderGroup(const GROUPREC *grp)
{
	_TRACE;
	if (!pVB) return;
	if (!grp) return;
	pDev->SetVertexDeclaration(pMeshVertexDecl);
	pDev->SetStreamSource(0, pVB, 0, sizeof(NMVERTEX));
	pDev->SetIndices(pIB);
	pDev->DrawIndexedPrimitive(D3DPT_TRIANGLELIST, grp->VertOff, 0, grp->nVert, grp->FaceOff*3, grp->nFace);
	D3D9Stats.Mesh.Vertices += grp->nVert;
	D3D9Stats.Mesh.MeshGrps++;
}


// reset stucts template
template <typename T> void reset (T& _p)
#if __cplusplus > 201103L // C++11
	{ _p = { 0 }; }
#else
	{ static const T _Empty = { 0 }; _p = _Empty; }
#endif


// ===========================================================================================
//
void D3D9Mesh::ResetRenderStatus()
{
	for (DWORD g = 0; g < nGrp; g++) Grp[g].bRendered = false;
}


// ===========================================================================================
//
bool D3D9Mesh::IsGroupRendered(DWORD idx) const
{
	return Grp[idx].bRendered;
}


// ================================================================================================
// Analyze mesh. Must be called when ever there is a change in textures or materials
//
void D3D9Mesh::CheckMeshStatus()
{
	bCanRenderFast = true;
	bIsReflective = false;
	D3D9MatExt *mat = NULL;

	for (DWORD g = 0; g < nGrp; g++) {

		Grp[g].Shader = SHADER_PBR;
		Grp[g].PBRStatus = 0;
	
		DWORD ti = Grp[g].TexIdx;

		if (Tex[ti] != NULL) {
			if (Tex[ti]->IsAdvanced()) {
				bCanRenderFast = false;
				if (Tex[ti]->GetMap(MAP_SPECULAR)) Grp[g].PBRStatus |= 0x2;
				if (Tex[ti]->GetMap(MAP_ROUGHNESS)) Grp[g].PBRStatus |= 0x4;
				if (Tex[ti]->GetMap(MAP_REFLECTION)) Grp[g].PBRStatus |= 0x8;
				if (Tex[ti]->GetMap(MAP_TRANSLUCENCE)) Grp[g].Shader = SHADER_ADV;
				if (Tex[ti]->GetMap(MAP_TRANSMITTANCE)) Grp[g].Shader = SHADER_ADV;
			}
		}

		if (Grp[g].MtrlIdx == SPEC_DEFAULT) mat = &defmat;
		else mat = &Mtrl[Grp[g].MtrlIdx];

		if (mat->ModFlags&D3D9MATEX_SPECULAR) Grp[g].PBRStatus |= 0x1;
		if (mat->ModFlags&D3D9MATEX_REFLECT) Grp[g].PBRStatus |= 0x8;
		if (mat->ModFlags&D3D9MATEX_ROUGHNESS) Grp[g].PBRStatus |= 0x4;
		if (mat->ModFlags&D3D9MATEX_FRESNEL) Grp[g].PBRStatus |= 0x10;
	}


	for (DWORD g = 0; g < nGrp; g++) {
		if (Grp[g].PBRStatus >= (0x8|0x2)) bIsReflective = true;
		if (Grp[g].PBRStatus >= 0x2) bCanRenderFast = false;
	}
}



// ================================================================================================
// This is a rendering routine for a Exterior Mesh, non-spherical moons/asteroids
//
void D3D9Mesh::Render(const LPD3DXMATRIX pW, int iTech, LPDIRECT3DCUBETEXTURE9 *pEnv, int nEnv)
{
	
	_TRACE;

	// Check material status
	//
	if (bMtrlModidied) {
		CheckMeshStatus();
		bMtrlModidied = false;
	}

	if (DebugControls::IsActive() == false) {
		if (bCanRenderFast && vClass != VCLASS_XR2) {
			RenderFast(pW, iTech);
			return;
		}
	}

	DWORD flags=0, selmsh=0, selgrp=0, displ=0; // Debug Variables
	bool bActiveVisual = false;

	const VCHUDSPEC *hudspec;

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

	bool bWorldMesh = false;
	bool bMeshCull = true;
	bool bTextured = true;
	bool bGroupCull = true;
	bool bUpdateFlow = true;	
	bool bShadowProjection = false;

	
	
	switch (iTech) {
		case RENDER_VC:
			EnablePlanetGlow(false);
			break;
		case RENDER_BASE:
			EnablePlanetGlow(false);
			bMeshCull = false;
			bShadowProjection = true;
			break;
		case RENDER_BASEBS:
			EnablePlanetGlow(false);
			bMeshCull = false;
			bShadowProjection = true;
			break;
		case RENDER_ASTEROID:
			EnablePlanetGlow(false);
			bMeshCull = false;
			bGroupCull = false;
			bShadowProjection = true;
			break;
		case RENDER_VESSEL:
			EnablePlanetGlow(true);
			break;
	}

	HR(D3D9Effect::FX->SetBool(D3D9Effect::eBaseBuilding, bShadowProjection));

	D3DXVECTOR4 Field;
	D3DXMATRIX mWorldView,  q;

	D3DXMatrixMultiply(&mWorldView, pW, scn->GetViewMatrix());

	if (bMeshCull || bGroupCull) Field = D9LinearFieldOfView(scn->GetProjectionMatrix());

	if (bMeshCull) if (!D9IsAABBVisible(&BBox, &mWorldView, &Field)) {
		if (flags&(DBG_FLAGS_BOXES|DBG_FLAGS_SPHERES)) RenderBoundingBox(pW);
		return;
	}

	D3DXMATRIX mWorldMesh;
	
	if (bGlobalTF) D3DXMatrixMultiply(&mWorldMesh, &mTransform, pW);
	else mWorldMesh = *pW;

	D3D9Stats.Mesh.Meshes++;

	D3D9MatExt *mat, *old_mat = NULL;
	LPD3D9CLIENTSURFACE old_tex = NULL;
	
	pDev->SetVertexDeclaration(pMeshVertexDecl);
	pDev->SetStreamSource(0, pVB, 0, sizeof(NMVERTEX));
	pDev->SetIndices(pIB);

	if (flags&DBG_FLAGS_DUALSIDED) pDev->SetRenderState(D3DRS_CULLMODE, D3DCULL_NONE);

	if (sunLight) FX->SetValue(eSun, sunLight, sizeof(D3D9Sun));

	FX->SetTechnique(eVesselTech);
	FX->SetBool(eFresnel, false);
	FX->SetBool(eEnvMapEnable, false);
	FX->SetBool(eTuneEnabled, false);
	FX->SetBool(eLightsEnabled, false);
	FX->SetVector(eColor, &D3DXVECTOR4(0, 0, 0, 0));

	if (DebugControls::IsActive()) if (pTune) FX->SetBool(eTuneEnabled, true);


	TexFlow FC;	reset(FC);

	const D3D9Light *pLights = gc->GetScene()->GetLights();
	int nSceneLights = gc->GetScene()->GetLightCount();

	for (int i = 0; i < Config->MaxLights(); i++) memcpy2(&Locals[i], &null_light, sizeof(LightStruct));

	if (pLights && nSceneLights>0) {

		int nMeshLights = 0;
		D3DXVECTOR3 pos;
		D3DXVec3TransformCoord(&pos, &D3DXVECTOR3f4(BBox.bs), pW);
		
		// Find all local lights effecting this mesh ------------------------------------------
		//
		for (int i = 0; i < nSceneLights; i++) {
			float il = pLights[i].GetIlluminance(pos, BBox.bs.w);
			if (il > 0.005f) {
				LightList[nMeshLights].illuminace = il;
				LightList[nMeshLights++].idx = i;
			}
		}

		if (nMeshLights > 0) {

			FX->SetBool(eLightsEnabled, true);

			// If any, Sort the list based on illuminance ------------------------------------------- 
			qsort(LightList, nMeshLights, sizeof(_LightList), compare_lights);

			nMeshLights = min(nMeshLights, Config->MaxLights());

			// Create a list of N most effective lights ---------------------------------------------
			for (int i = 0; i < nMeshLights; i++) memcpy2(&Locals[i], &pLights[LightList[i].idx], sizeof(LightStruct));		
		}
	}

	FX->SetValue(eLights, Locals, sizeof(LightStruct) * Config->MaxLights());
	

	if (nEnv >= 1 && pEnv[0]) FX->SetTexture(eEnvMapA, pEnv[0]);
	

	UINT numPasses = 0;
	HR(FX->Begin(&numPasses, D3DXFX_DONOTSAVESTATE));

	WORD CurrentShader = 0xFFFF;

	bool bRefl = true;

	if (iTech == RENDER_VC) bRefl = false;	// No reflections in VC
	if (iTech == RENDER_BASEBS) pDev->SetRenderState(D3DRS_ZENABLE, 0);	// Must be here because BeginPass() sets it enabled

	for (DWORD g=0; g<nGrp; g++) {

		
		bool bHUD = (Grp[g].MFDScreenId == 0x100);

		// Inline engine renders HUD/MFDs in a separate rendering pass and flag 0x2 is used to disable rendering during the main rendering pass  
		if ((Grp[g].UsrFlag & 0x2) && (!bHUD)) continue;


		// Check skip conditions ==========================================
		//
		DWORD ti = Grp[g].TexIdx;
		DWORD tni = Grp[g].TexIdxEx[0];

		if (ti == 0 && tni != 0) continue;


		// Cull unvisible geometry ----------------------------------------
		//
		if (bGroupCull) if (!D9IsBSVisible(&Grp[g].BBox, &mWorldView, &Field)) continue;



		// Enforce special shader for XR2 HUD to bypass faulty materials
		if ((vClass == VCLASS_XR2) && (Grp[g].MFDScreenId == 0x100)) Grp[g].Shader = SHADER_XR2HUD;



		// Begin rendering of a specified pass ------------------------------
		//
		if (Grp[g].Shader != CurrentShader) {
			if (CurrentShader!=0xFFFF) { HR(FX->EndPass()); }
			HR(FX->BeginPass(Grp[g].Shader));
			CurrentShader = Grp[g].Shader;
		}
	


		// Mesh Debugger -------------------------------------------------------------------------------------------
		//
		if (DebugControls::IsActive()) {

			if (bActiveVisual) {

				if (displ==3 && g!=selgrp) continue;

				FX->SetVector(eColor, &D3DXVECTOR4(0, 0, 0, 0));

				if (flags&DBG_FLAGS_HLMESH) {
					if (uCurrentMesh==selmsh) {
						FX->SetVector(eColor, &D3DXVECTOR4(0.0f, 0.0f, 0.5f, 0.5f));
					}
				}
				if (flags&DBG_FLAGS_HLGROUP) {
					if (g==selgrp && uCurrentMesh==selmsh) {
						FX->SetVector(eColor, &D3DXVECTOR4(0.0f, 0.5f, 0.0f, 0.5f));
					}
				}
			}
		}


		// ---------------------------------------------------------------------------------------------------------
		//
		if (Tex[ti] != old_tex) {
			if (Tex[ti] == NULL) {
				reset(FC);
				bUpdateFlow = true;
			}
		}

		if (Tex[ti]==NULL) bTextured = false, old_tex = NULL;
		else bTextured = true;



		// Setup Textures and Normal Maps ==========================================================================
		//
		if (bTextured) {

			if (Tex[ti]!=old_tex) {

				D3D9Stats.Mesh.TexChanges++;

				old_tex = Tex[ti];

				FX->SetTexture(eTex0, Tex[ti]->GetTexture());

				bUpdateFlow = true;	// Fix this later

				if (CurrentShader == SHADER_LEGACY) {
					if (tni && Grp[g].TexMixEx[0] < 0.5f) tni = 0;
					if (tni && Tex[tni]) FX->SetTexture(eEmisMap, Tex[tni]->GetTexture()); 
				}
				else {

					if (DebugControls::IsActive()) if (pTune) {
						FX->SetValue(eTune, &pTune[ti], sizeof(D3D9Tune));
					}

					LPDIRECT3DTEXTURE9 pTransl = NULL;
					LPDIRECT3DTEXTURE9 pTransm = NULL;
					LPDIRECT3DTEXTURE9 pSpec = Tex[ti]->GetMap(MAP_SPECULAR);
					LPDIRECT3DTEXTURE9 pNorm = Tex[ti]->GetMap(MAP_NORMAL);
					LPDIRECT3DTEXTURE9 pRefl = Tex[ti]->GetMap(MAP_REFLECTION);
					LPDIRECT3DTEXTURE9 pRghn = Tex[ti]->GetMap(MAP_ROUGHNESS);
					LPDIRECT3DTEXTURE9 pFrsl = Tex[ti]->GetMap(MAP_FRESNEL);
					LPDIRECT3DTEXTURE9 pEmis = Tex[ti]->GetMap(MAP_EMISSION);

					if (tni && Grp[g].TexMixEx[0] < 0.5f) tni = 0;
					if (!pEmis && tni && Tex[tni]) pEmis = Tex[tni]->GetTexture();

					if (pNorm) FX->SetTexture(eTex3, pNorm);
					if (pRghn) FX->SetTexture(eRghnMap, pRghn);
					if (pRefl) FX->SetTexture(eReflMap, pRefl);
					if (pFrsl) FX->SetTexture(eFrslMap, pFrsl);
					if (pSpec) FX->SetTexture(eSpecMap, pSpec);
					if (pEmis) FX->SetTexture(eEmisMap, pEmis);


					if (CurrentShader == SHADER_ADV) {

						pTransl = Tex[ti]->GetMap(MAP_TRANSLUCENCE);
						pTransm = Tex[ti]->GetMap(MAP_TRANSMITTANCE);

						if (pTransl) FX->SetTexture(eTranslMap, pTransl);
						if (pTransm) FX->SetTexture(eTransmMap, pTransm);

						FC.Transl = (pTransl != NULL);
						FC.Transm = (pTransm != NULL);
						FC.Transx = FC.Transl || FC.Transm;
					}
					else {
						FC.Transl = false;
						FC.Transm = false;
						FC.Transx = false;
					}

					FC.Emis = (pEmis != NULL);
					FC.Frsl = (pFrsl != NULL);
					FC.Norm = (pNorm != NULL);
					FC.Rghn = (pRghn != NULL);
					FC.Spec = (pSpec != NULL);
					FC.Refl = (pRefl != NULL);
				}
			}
		}

		// Apply MFD Screen Override ================================================================================
		//
		if (Grp[g].MFDScreenId) {

			bTextured = true;
			old_tex = NULL;
			old_mat = NULL;
			reset(FC);
			bUpdateFlow = true;
				
			SURFHANDLE hMFD;
			if (bHUD) hMFD = gc->GetVCHUDSurface(&hudspec);
			else hMFD = gc->GetMFDSurface(Grp[g].MFDScreenId - 1);

			if (hMFD) FX->SetTexture(eTex0, SURFACE(hMFD)->GetTexture());
			else	  FX->SetTexture(eTex0, gc->GetDefaultTexture()->GetTexture());
			
			if (Grp[g].MtrlIdx==SPEC_DEFAULT) mat = &mfdmat;
			else							  mat = &Mtrl[Grp[g].MtrlIdx];

			if (bModulateMatAlpha || bTextured==false)  FX->SetFloat(eMtrlAlpha, mat->Diffuse.w);
			else										FX->SetFloat(eMtrlAlpha, 1.0f);

			FX->SetValue(eMtrl, mat, sizeof(D3D9MatExt)-4);
		}


		// Setup Mesh group material  ==========================================================================
		//
		else {

			if (Grp[g].MtrlIdx==SPEC_DEFAULT) mat = &defmat;
			else							  mat = &Mtrl[Grp[g].MtrlIdx];

			if (mat!=old_mat) {

				D3D9Stats.Mesh.MtrlChanges++;

				old_mat = mat;

				FX->SetValue(eMtrl, mat, sizeof(D3D9MatExt)-4);

				if (bModulateMatAlpha || bTextured==false)  FX->SetFloat(eMtrlAlpha, mat->Diffuse.w);
				else										FX->SetFloat(eMtrlAlpha, 1.0f);
			}
		}


		// Apply Animations =========================================================================================
		//
		if (Grp[g].bTransform) {
			bWorldMesh = false;
			FX->SetMatrix(eW, D3DXMatrixMultiply(&q, &pGrpTF[g], pW));
		}
		else if (!bWorldMesh) {
			FX->SetMatrix(eW, &mWorldMesh);
			bWorldMesh = true;
		}

		if (bUpdateFlow) {
			bUpdateFlow = false;
			HR(FX->SetValue(eFlow, &FC, sizeof(TexFlow)));
		}
	
		bool bPBR = (Grp[g].PBRStatus & 0xF) == (0x8 + 0x4);
		bool bRGH = (Grp[g].PBRStatus & 0xE) == (0x8 + 0x2);
		bool bNoL = (Grp[g].UsrFlag & 0x04) != 0;
		bool bNoC = (Grp[g].UsrFlag & 0x10) != 0;
		bool bENV = false;
		bool bFRS = false;

		// Setup Mesh drawing options =================================================================================
		//
		FX->SetBool(eTextured, bTextured);
		FX->SetBool(eFullyLit, bNoL);
		FX->SetBool(eNoColor,  bNoC);
		FX->SetBool(eSwitch, bPBR);
		FX->SetBool(eRghnSw, bRGH);
	

		// Update envmap and fresnel status as required
		if (bRefl) {
			bFRS = (Grp[g].PBRStatus & 0x1E) >= 0x10;
			FX->SetBool(eFresnel, bFRS);
			if (bIsReflective) {
				bENV = (Grp[g].PBRStatus & 0x1E) >= 0xA;
				FX->SetBool(eEnvMapEnable, bENV);
			}
		}


		FX->CommitChanges();



		// Mesh Debugger -------------------------------------------------------------------------------------------
		//
		if (DebugControls::IsActive()) {

			if ((bActiveVisual) && (g == selgrp) && (uCurrentMesh == selmsh)) {

				bool bAdd = (Grp[g].UsrFlag & 0x08) != 0;
				bool bNoS = (Grp[g].UsrFlag & 0x01) != 0;

				static const char *YesNo[2] = { "No", "Yes" };
				static const char *LPW[2] = { "Legacy", "PBR" };
				static const char *RGH[2] = { "Disabled", "Enabled" };
				static const char *Shaders[4] = { "PBR", "ADV", "FAST", "XR2" };

				DebugControls::Append("MeshIdx = %d, GrpIdx = %d\n", uCurrentMesh, g);
				DebugControls::Append("MtrlIdx = %d, TexIdx = %d\n", Grp[g].MtrlIdx, Grp[g].TexIdx);
				DebugControls::Append("FaceCnt = %d, VtxCnt = %d\n", Grp[g].nFace, Grp[g].nVert);

				DebugControls::Append("GroupFlags.. = 0x%X\n", Grp[g].UsrFlag);
				DebugControls::Append("Shader...... = %s\n", Shaders[CurrentShader]);
				DebugControls::Append("Textured.... = %s\n", YesNo[bTextured]);
				DebugControls::Append("ModMatAlpha. = %s\n", YesNo[bModulateMatAlpha]);
				DebugControls::Append("NoColor..... = %s\n", YesNo[bNoC]);
				DebugControls::Append("NoLighting.. = %s\n", YesNo[bNoL]);
				DebugControls::Append("NoShadow.... = %s\n", YesNo[bNoS]);
				DebugControls::Append("Additive.... = %s\n", YesNo[bAdd]);

				if (CurrentShader == SHADER_PBR) {

					DebugControls::Append("\nPBR-Shader State:\n");
					DebugControls::Append("PBR-Switch.. = %s\n", LPW[bPBR]);
					DebugControls::Append("Rghn-Conver. = %s\n", RGH[bRGH]);
					DebugControls::Append("Fresnel Mode = %s\n", RGH[bFRS]);
					DebugControls::Append("Env Mapping. = %s\n", RGH[bENV]);

					DebugControls::Append("TextureMaps = [ ");
					if (FC.Emis) DebugControls::Append("emis ");
					if (FC.Frsl) DebugControls::Append("frsl ");
					if (FC.Norm) DebugControls::Append("norm ");
					if (FC.Rghn) DebugControls::Append("rghn ");
					if (FC.Spec) DebugControls::Append("spec ");
					if (FC.Refl) DebugControls::Append("refl ");
					if (FC.Transl) DebugControls::Append("transl ");
					if (FC.Transm) DebugControls::Append("transm ");
					DebugControls::Append("]\n");
				}

				DebugControls::Refresh();
			}
		}


		// Start rendering -------------------------------------------------------------------------------------------
		//
		if (bHUD) {
			pDev->SetRenderState(D3DRS_ZENABLE, 0);
			pDev->SetRenderState(D3DRS_DESTBLEND, D3DBLEND_ONE);
		}

		if (Grp[g].bDualSided) {
			pDev->SetRenderState(D3DRS_CULLMODE, D3DCULL_CW);
			pDev->SetRenderState(D3DRS_ZWRITEENABLE, 0);
			pDev->DrawIndexedPrimitive(D3DPT_TRIANGLELIST, Grp[g].VertOff, 0, Grp[g].nVert, Grp[g].FaceOff * 3, Grp[g].nFace);
			pDev->SetRenderState(D3DRS_CULLMODE, D3DCULL_CCW);
		}

		pDev->DrawIndexedPrimitive(D3DPT_TRIANGLELIST, Grp[g].VertOff,  0, Grp[g].nVert,  Grp[g].FaceOff*3, Grp[g].nFace);

		Grp[g].bRendered = true;

		if (Grp[g].bDualSided) {
			pDev->SetRenderState(D3DRS_ZWRITEENABLE, 1);
		}

		if (bHUD) {
			pDev->SetRenderState(D3DRS_ZENABLE, 1);
			pDev->SetRenderState(D3DRS_DESTBLEND, D3DBLEND_INVSRCALPHA);
		}

		D3D9Stats.Mesh.Vertices += Grp[g].nVert;
		D3D9Stats.Mesh.MeshGrps++;

	}

	if (CurrentShader != 0xFFFF) {
		HR(FX->EndPass());
	}

	HR(FX->End());

	if (flags&(DBG_FLAGS_BOXES|DBG_FLAGS_SPHERES)) RenderBoundingBox(pW);
	FX->SetVector(eColor, &D3DXVECTOR4(0, 0, 0, 0));
	if (flags&DBG_FLAGS_DUALSIDED) pDev->SetRenderState(D3DRS_CULLMODE, D3DCULL_CCW);
}


// ================================================================================================
// Render a legacy orbiter mesh without any additional textures
//
void D3D9Mesh::RenderFast(const LPD3DXMATRIX pW, int iTech)
{

	_TRACE;
	DWORD flags = 0, selmsh = 0, selgrp = 0, displ = 0; // Debug Variables
	bool bActiveVisual = false;

	const VCHUDSPEC *hudspec;

	if (!pVB) return;

	if (DebugControls::IsActive()) {
		flags = *(DWORD*)gc->GetConfigParam(CFGPRM_GETDEBUGFLAGS);
		selmsh = *(DWORD*)gc->GetConfigParam(CFGPRM_GETSELECTEDMESH);
		selgrp = *(DWORD*)gc->GetConfigParam(CFGPRM_GETSELECTEDGROUP);
		displ = *(DWORD*)gc->GetConfigParam(CFGPRM_GETDISPLAYMODE);
		bActiveVisual = (pCurrentVisual == DebugControls::GetVisual());
		if (displ>0 && !bActiveVisual) return;
		if ((displ == 2 || displ == 3) && uCurrentMesh != selmsh) return;
	}

	Scene *scn = gc->GetScene();

	bool bWorldMesh = false;
	bool bMeshCull = true;
	bool bTextured = true;
	bool bGroupCull = true;
	bool bUpdateFlow = true;
	bool bShadowProjection = false;

	switch (iTech) {
		case RENDER_VC:
			EnablePlanetGlow(false);
			break;
		case RENDER_BASE:
			EnablePlanetGlow(false);
			bMeshCull = false;
			bShadowProjection = true;
			break;
		case RENDER_BASEBS:
			EnablePlanetGlow(false);
			bMeshCull = false;
			bShadowProjection = true;
			break;
		case RENDER_ASTEROID:
			EnablePlanetGlow(false);
			bMeshCull = false;
			bGroupCull = false;
			bShadowProjection = true;
			break;
		case RENDER_VESSEL:
			EnablePlanetGlow(true);
			break;
	}

	HR(D3D9Effect::FX->SetBool(D3D9Effect::eBaseBuilding, bShadowProjection));

	D3DXVECTOR4 Field;
	D3DXMATRIX mWorldView, q;

	D3DXMatrixMultiply(&mWorldView, pW, scn->GetViewMatrix());

	if (bMeshCull || bGroupCull) Field = D9LinearFieldOfView(scn->GetProjectionMatrix());

	if (bMeshCull) if (!D9IsAABBVisible(&BBox, &mWorldView, &Field)) {
		if (flags&(DBG_FLAGS_BOXES | DBG_FLAGS_SPHERES)) RenderBoundingBox(pW);
		return;
	}

	D3DXMATRIX mWorldMesh;

	if (bGlobalTF) D3DXMatrixMultiply(&mWorldMesh, &mTransform, pW);
	else mWorldMesh = *pW;

	D3D9Stats.Mesh.Meshes++;

	D3D9MatExt *mat, *old_mat = NULL;
	LPD3D9CLIENTSURFACE old_tex = NULL;
	LPDIRECT3DTEXTURE9 pEmis_old = NULL;

	pDev->SetVertexDeclaration(pMeshVertexDecl);
	pDev->SetStreamSource(0, pVB, 0, sizeof(NMVERTEX));
	pDev->SetIndices(pIB);

	if (flags&DBG_FLAGS_DUALSIDED) pDev->SetRenderState(D3DRS_CULLMODE, D3DCULL_NONE);

	if (sunLight) FX->SetValue(eSun, sunLight, sizeof(D3D9Sun));

	FX->SetTechnique(eVesselTech);
	FX->SetBool(eTuneEnabled, false);
	FX->SetBool(eLightsEnabled, false);
	FX->SetVector(eColor, &D3DXVECTOR4(0, 0, 0, 0));

	if (DebugControls::IsActive()) if (pTune) FX->SetBool(eTuneEnabled, true);

	TexFlow FC;	reset(FC);

	const D3D9Light *pLights = gc->GetScene()->GetLights();
	int nSceneLights = gc->GetScene()->GetLightCount();

	for (int i = 0; i < Config->MaxLights(); i++) memcpy2(&Locals[i], &null_light, sizeof(LightStruct));

	if (pLights && nSceneLights>0) {

		int nMeshLights = 0;
		D3DXVECTOR3 pos;
		D3DXVec3TransformCoord(&pos, &D3DXVECTOR3f4(BBox.bs), pW);

		// Find all local lights effecting this mesh ------------------------------------------
		//
		for (int i = 0; i < nSceneLights; i++) {
			float il = pLights[i].GetIlluminance(pos, BBox.bs.w);
			if (il > 0.0) {
				LightList[nMeshLights].illuminace = il;
				LightList[nMeshLights++].idx = i;
			}
		}

		if (nMeshLights > 0) {

			FX->SetBool(eLightsEnabled, true);

			// If any, Sort the list based on illuminance ------------------------------------------- 
			qsort(LightList, nMeshLights, sizeof(_LightList), compare_lights);

			nMeshLights = min(nMeshLights, Config->MaxLights());

			// Create a list of N most effective lights ---------------------------------------------
			int i;
			for (i = 0; i < nMeshLights; i++) memcpy2(&Locals[i], &pLights[LightList[i].idx], sizeof(LightStruct));		
		}
	}
	
	FX->SetValue(eLights, Locals, sizeof(LightStruct) * Config->MaxLights());

	UINT numPasses = 0;
	HR(FX->Begin(&numPasses, D3DXFX_DONOTSAVESTATE));

	// Begin rendering of a specified pass ------------------------------
	//
	HR(FX->BeginPass(2));

	if (iTech == RENDER_BASEBS) pDev->SetRenderState(D3DRS_ZENABLE, 0);	// Must be here because BeginPass() sets it enabled

	for (DWORD g = 0; g<nGrp; g++) {

		// This mesh group must be skipped by user orders
		if ((Grp[g].UsrFlag & 0x2) && (Grp[g].MFDScreenId != 0x100)) continue;

		bool bHUD = Grp[g].MFDScreenId == 0x100;

		// Mesh Debugger -------------------------------------------------------------------------------------------
		//
		if (DebugControls::IsActive()) {

			if (bActiveVisual) {

				if (displ == 3 && g != selgrp) continue;

				FX->SetVector(eColor, &D3DXVECTOR4(0, 0, 0, 0));

				if (flags&DBG_FLAGS_HLMESH) {
					if (uCurrentMesh == selmsh) {
						FX->SetVector(eColor, &D3DXVECTOR4(0.0f, 0.0f, 0.5f, 0.5f));
					}
				}
				if (flags&DBG_FLAGS_HLGROUP) {
					if (g == selgrp && uCurrentMesh == selmsh) {
						FX->SetVector(eColor, &D3DXVECTOR4(0.0f, 0.5f, 0.0f, 0.5f));
					}
				}
			}
		}


		// ---------------------------------------------------------------------------------------------------------
		//
		DWORD ti = Grp[g].TexIdx;
		DWORD tni = Grp[g].TexIdxEx[0];

		if (ti == 0 && tni != 0) continue;

		if (Tex[ti] == NULL || ti == 0) bTextured = false, old_tex = NULL;
		else						    bTextured = true;


		// Cull unvisible geometry =================================================================================
		//
		if (bGroupCull) if (!D9IsBSVisible(&Grp[g].BBox, &mWorldView, &Field)) continue;


		// Setup Textures and Normal Maps ==========================================================================
		//
		if (bTextured) {

			if (Tex[ti] != old_tex) {

				D3D9Stats.Mesh.TexChanges++;

				if (DebugControls::IsActive()) if (pTune) FX->SetValue(eTune, &pTune[ti], sizeof(D3D9Tune));
			
				old_tex = Tex[ti];
				FX->SetTexture(eTex0, Tex[ti]->GetTexture());

				LPDIRECT3DTEXTURE9 pEmis = Tex[ti]->GetMap(MAP_EMISSION);

				if (tni && Grp[g].TexMixEx[0]<0.5f) tni = 0;
				if (!pEmis && tni && Tex[tni]) pEmis = Tex[tni]->GetTexture();

				if (pEmis != pEmis_old) {
					FX->SetTexture(eEmisMap, pEmis);
					pEmis_old = pEmis;
					FC.Emis = (pEmis != NULL);
					bUpdateFlow = true;
				}
			}
		}

		// Apply MFD Screen Override ================================================================================
		//
		if (Grp[g].MFDScreenId) {
			bTextured = true;
			old_tex = NULL;
			old_mat = NULL;
			reset(FC);
			bUpdateFlow = true;

			SURFHANDLE hMFD;
			if (bHUD) hMFD = gc->GetVCHUDSurface(&hudspec);
			else hMFD = gc->GetMFDSurface(Grp[g].MFDScreenId - 1);

			if (hMFD) FX->SetTexture(eTex0, SURFACE(hMFD)->GetTexture());
			else	  FX->SetTexture(eTex0, gc->GetDefaultTexture()->GetTexture());

			if (Grp[g].MtrlIdx == SPEC_DEFAULT) mat = &mfdmat;
			else							    mat = &Mtrl[Grp[g].MtrlIdx];

			if (bModulateMatAlpha || bTextured == false)  FX->SetFloat(eMtrlAlpha, mat->Diffuse.w);
			else										  FX->SetFloat(eMtrlAlpha, 1.0f);

			FX->SetValue(eMtrl, mat, sizeof(D3D9MatExt)-4);
			FX->SetBool(eEnvMapEnable, false);
			FX->SetBool(eFresnel, false);
		}

		// Setup Mesh group material  ==========================================================================
		//
		else {

			if (Grp[g].MtrlIdx == SPEC_DEFAULT) mat = &defmat;
			else							    mat = &Mtrl[Grp[g].MtrlIdx];

			if (mat != old_mat) {

				D3D9Stats.Mesh.MtrlChanges++;

				old_mat = mat;

				FX->SetValue(eMtrl, mat, sizeof(D3D9MatExt)-4);

				if (bModulateMatAlpha || bTextured == false)  FX->SetFloat(eMtrlAlpha, mat->Diffuse.w);
				else										  FX->SetFloat(eMtrlAlpha, 1.0f);
			}
		}


		// Apply Animations =========================================================================================
		//
		if (Grp[g].bTransform) {
			bWorldMesh = false;
			FX->SetMatrix(eW, D3DXMatrixMultiply(&q, &pGrpTF[g], pW));
		}
		else if (!bWorldMesh) {
			FX->SetMatrix(eW, &mWorldMesh);
			bWorldMesh = true;
		}

		if (bUpdateFlow) {
			bUpdateFlow = false;
			HR(FX->SetValue(eFlow, &FC, sizeof(TexFlow)));
		}

		// Setup Mesh drawing options =================================================================================
		//
		FX->SetBool(eTextured, bTextured);
		FX->SetBool(eFullyLit, (Grp[g].UsrFlag & 0x4) != 0);
		FX->SetBool(eNoColor, (Grp[g].UsrFlag & 0x10) != 0);

		FX->CommitChanges();

		if (bHUD) {
			pDev->SetRenderState(D3DRS_ZENABLE, 0);
			pDev->SetRenderState(D3DRS_DESTBLEND, D3DBLEND_ONE);
		}

		if (Grp[g].bDualSided) {
			pDev->SetRenderState(D3DRS_CULLMODE, D3DCULL_CW);
			pDev->SetRenderState(D3DRS_ZWRITEENABLE, 0);
			pDev->DrawIndexedPrimitive(D3DPT_TRIANGLELIST, Grp[g].VertOff, 0, Grp[g].nVert, Grp[g].FaceOff * 3, Grp[g].nFace);
			pDev->SetRenderState(D3DRS_CULLMODE, D3DCULL_CCW);
		}

		pDev->DrawIndexedPrimitive(D3DPT_TRIANGLELIST, Grp[g].VertOff, 0, Grp[g].nVert, Grp[g].FaceOff * 3, Grp[g].nFace);

		Grp[g].bRendered = true;

		if (Grp[g].bDualSided) {
			pDev->SetRenderState(D3DRS_ZWRITEENABLE, 1);
		}

		if (bHUD) {
			pDev->SetRenderState(D3DRS_ZENABLE, 1);
			pDev->SetRenderState(D3DRS_DESTBLEND, D3DBLEND_INVSRCALPHA);
		}

		D3D9Stats.Mesh.Vertices += Grp[g].nVert;
		D3D9Stats.Mesh.MeshGrps++;
	}

	HR(FX->EndPass());
	HR(FX->End());

	if (flags&(DBG_FLAGS_BOXES | DBG_FLAGS_SPHERES)) RenderBoundingBox(pW);
	FX->SetVector(eColor, &D3DXVECTOR4(0, 0, 0, 0));
	if (flags&DBG_FLAGS_DUALSIDED) pDev->SetRenderState(D3DRS_CULLMODE, D3DCULL_CCW);
}


// ===========================================================================================
//
void D3D9Mesh::RenderBaseTile(const LPD3DXMATRIX pW)
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
	
	pDev->SetVertexDeclaration(pMeshVertexDecl);
	pDev->SetStreamSource(0, pVB, 0, sizeof(NMVERTEX));
	pDev->SetIndices(pIB);

	if (sunLight) FX->SetValue(eSun, sunLight, sizeof(D3D9Sun));

	FX->SetTechnique(eBaseTile);
	FX->SetVector(eColor, &D3DXVECTOR4(0, 0, 0, 0));
	FX->SetMatrix(eGT, gc->GetIdentity());
	FX->SetMatrix(eW, pW);
	//FX->SetBool(eUseSpec, false);
	//FX->SetBool(eUseEmis, false);
	//FX->SetBool(eUseRefl, false);
	
	
	UINT numPasses = 0;
	HR(FX->Begin(&numPasses, D3DXFX_DONOTSAVESTATE));

	for (DWORD pass=0;pass<numPasses;pass++) {

		if (bUseNormalMap==false && pass==0) continue; // Skip normal mapped rendering pass

		HR(FX->BeginPass(pass));

		for (DWORD g=0; g<nGrp; g++) {

			if (Grp[g].UsrFlag & 0x2) continue;

			// Render group ----------------------------------------------
			//

			DWORD ti=Grp[g].TexIdx;
			DWORD tni=Grp[g].TexIdxEx[0];

			if (ti==0 && tni!=0) continue;
				
			if (Tex[ti]==NULL || ti==0) bTextured = false;
			else						bTextured = true;

			if (bTextured) {
				pNorm = Tex[ti]->GetMap(MAP_NORMAL);
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
			if (bGroupCull) if (!D9IsBSVisible(&Grp[g].BBox, &mWorldView, &Field)) continue;


			// Setup Textures and Normal Maps ==========================================================================
			//
			if (bTextured) {

				if (Tex[ti]!=old_tex) {

					if (tni && Grp[g].TexMixEx[0]<0.5f) tni=0;

					old_tex = Tex[ti];
					FX->SetTexture(eTex0, Tex[ti]->GetTexture());

					if (tni && Tex[tni]) {
						FX->SetTexture(eEmisMap, Tex[tni]->GetTexture());
						//FX->SetBool(eUseEmis, true);
					} //else FX->SetBool(eUseEmis, false);

					if (bUseNormalMap) if (pNorm) FX->SetTexture(eTex3, pNorm);
				}
			}

			// Setup Mesh group material ==============================================================================
			//
			if (Grp[g].MtrlIdx==SPEC_DEFAULT) mat = &defmat;
			else							  mat = &Mtrl[Grp[g].MtrlIdx];

			if (mat!=old_mat) {
				old_mat = mat;
				FX->SetValue(eMtrl, mat, sizeof(D3D9MatExt)-4);
				if (bModulateMatAlpha || bTextured==false) FX->SetFloat(eMtrlAlpha, mat->Diffuse.w);
				else FX->SetFloat(eMtrlAlpha, 1.0f);
			}

			// Setup Mesh drawing options =================================================================================
			//
			FX->SetBool(eTextured, bTextured);
			FX->SetBool(eFullyLit, (Grp[g].UsrFlag&0x4)!=0);

			FX->CommitChanges();

			pDev->DrawIndexedPrimitive(D3DPT_TRIANGLELIST, Grp[g].VertOff, 0, Grp[g].nVert, Grp[g].FaceOff*3, Grp[g].nFace);

			D3D9Stats.Mesh.Vertices += Grp[g].nVert;
			D3D9Stats.Mesh.MeshGrps++;
		}
		HR(FX->EndPass());
	}
	HR(FX->End());
}


// ================================================================================================
//
void D3D9Mesh::RenderShadows(float alpha, const LPD3DXMATRIX pW, bool bShadowMap)
{
	if (!pIB || !pGB) return;

	D3DXMATRIX GroupMatrix, mWorldMesh; UINT numPasses = 0;
	
	if (bGlobalTF) D3DXMatrixMultiply(&mWorldMesh, &mTransform, pW);
	else mWorldMesh = *pW;

	D3D9Stats.Mesh.Meshes++;

	pDev->SetVertexDeclaration(pVector4Decl);
	pDev->SetStreamSource(0, pGB, 0, sizeof(D3DXVECTOR4));
	pDev->SetIndices(pIB);

	//if (bShadowMap) pDev->SetRenderState(D3DRS_CULLMODE, D3DCULL_CW);

	if (bShadowMap) FX->SetTechnique(eGeometry);
	else			FX->SetTechnique(eShadowTech);

	FX->SetFloat(eMix, alpha);
	FX->Begin(&numPasses, D3DXFX_DONOTSAVESTATE);
	FX->BeginPass(0);

	bool bInit = true;
	bool bCurrentState = false;
	
	for (DWORD g = 0; g < nGrp; g++) {


		if ((Grp[g].UsrFlag & 0x3) == 0x3) continue;
		if ((Grp[g].IntFlag & 0x3) == 0x3) continue;


		if (Grp[g].bTransform) {
			D3DXMatrixMultiply(&GroupMatrix, &pGrpTF[g], pW);		// Apply Animations to instance matrices
			FX->SetValue(eW, GroupMatrix, sizeof(D3DXMATRIX));
			FX->CommitChanges();
			bInit = true;
		}
		else {
			if (bInit) {
				FX->SetValue(eW, mWorldMesh, sizeof(D3DXMATRIX));
				FX->CommitChanges();
			}
			bInit = false;
		}


		if (bShadowMap) {
			
			bool bDisable = (Grp[g].UsrFlag & 0x1) != 0;

			if (bDisable != bCurrentState) {
				if (bDisable) pDev->SetRenderState(D3DRS_COLORWRITEENABLE, 0);
				else		  pDev->SetRenderState(D3DRS_COLORWRITEENABLE, 0xF);
				bCurrentState = bDisable;
			}
		}

		pDev->DrawIndexedPrimitive(D3DPT_TRIANGLELIST, Grp[g].VertOff, 0, Grp[g].nVert, Grp[g].FaceOff * 3, Grp[g].nFace);

		D3D9Stats.Mesh.Vertices += Grp[g].nVert;
		D3D9Stats.Mesh.MeshGrps++;
	}

	FX->EndPass();
	FX->End();

	if (bShadowMap) {
		pDev->SetRenderState(D3DRS_COLORWRITEENABLE, 0xF);
		pDev->SetRenderState(D3DRS_CULLMODE, D3DCULL_CCW);
	}
}


// ================================================================================================
//
void D3D9Mesh::RenderShadowsEx(float alpha, const LPD3DXMATRIX pP, const LPD3DXMATRIX pW, const D3DXVECTOR4 *light, const D3DXVECTOR4 *param)
{
	if (!pVB || !pGB) return;
	
	D3D9Stats.Mesh.Meshes++;

	pDev->SetVertexDeclaration(pVector4Decl);
	pDev->SetStreamSource(0, pGB, 0, sizeof(D3DXVECTOR4));
	pDev->SetIndices(pIB);

	FX->SetTechnique(eShadowTech);
	FX->SetMatrix(eW, pW);
	FX->SetMatrix(eGT, pP);
	FX->SetFloat(eMix, alpha);
	FX->SetVector(eColor, light);
	FX->SetVector(eTexOff, param);

	UINT numPasses = 0;
	FX->Begin(&numPasses, D3DXFX_DONOTSAVESTATE);
	FX->BeginPass(1);

	for (DWORD g = 0; g<nGrp; g++) {

		if (Grp[g].UsrFlag & 0x3) continue;
		if (Grp[g].IntFlag & 0x3) continue;

		pDev->DrawIndexedPrimitive(D3DPT_TRIANGLELIST, Grp[g].VertOff, 0, Grp[g].nVert, Grp[g].FaceOff * 3, Grp[g].nFace);

		D3D9Stats.Mesh.Vertices += Grp[g].nVert;
		D3D9Stats.Mesh.MeshGrps++;
	}

	FX->EndPass();
	FX->End();
}



// ================================================================================================
// This is a rendering routine for a Exterior Mesh, non-spherical moons/asteroids
//
void D3D9Mesh::RenderBoundingBox(const LPD3DXMATRIX pW)
{
	_TRACE;

	if (!pVB) return;
	if (DebugControls::IsActive()==false) return;

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

		pDev->SetVertexDeclaration(pPositionDecl);

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
			if (Grp[g].UsrFlag & 0x2) continue;

			FX->SetVector(eAttennuate, &Grp[g].BBox.min);
			FX->SetVector(eInScatter, &Grp[g].BBox.max);

			// Apply Animations =========================================================================================
			//
			if (Grp[g].bTransform) {
				if (bGlobalTF)  FX->SetMatrix(eGT, D3DXMatrixMultiply(&q, &mTransform, &Grp[g].Transform));
				else FX->SetMatrix(eGT, &Grp[g].Transform);
			}
			else FX->SetMatrix(eGT, &mTransform);


			// Setup Mesh drawing options =================================================================================
			//
			FX->CommitChanges();

			pDev->DrawPrimitiveUP(D3DPT_LINESTRIP, 9, &poly, sizeof(D3DVECTOR));
			pDev->DrawPrimitiveUP(D3DPT_LINELIST, 3, &list, sizeof(D3DVECTOR));
		}

		FX->EndPass();
		FX->End();
	}

	if (flags&DBG_FLAGS_SPHERES) {
		for (DWORD g=0; g<nGrp; g++) {
			if (flags&DBG_FLAGS_SELGRPONLY && g!=selgrp) continue;
			if (Grp[g].UsrFlag & 0x2) continue;
			D3D9Effect::RenderBoundingSphere(pW, NULL, &Grp[g].BBox.bs, &D3DXVECTOR4(0,1,0,0.75f));
		}
	}
	if (flags&DBG_FLAGS_BOXES) D3D9Effect::RenderBoundingBox(pW, &mTransform, &BBox.min, &BBox.max, &D3DXVECTOR4(0,0,1,0.75f));
	if (flags&DBG_FLAGS_SPHERES) D3D9Effect::RenderBoundingSphere(pW, &mTransform, &BBox.bs, &D3DXVECTOR4(0,0,1,0.75f));
}


// ===========================================================================================
//
/*void D3D9Mesh::BoundingBox(const NMVERTEX *vtx, DWORD n, D9BBox *box)
{
	if (!pVB) return;
	box->min.x = box->max.x = vtx[0].x;
	box->min.y = box->max.y = vtx[0].y;
	box->min.z = box->max.z = vtx[0].z;

	for (DWORD i=1;i<n;i++) {
		if (vtx[i].x < box->min.x) box->min.x=vtx[i].x;
		else if (vtx[i].x > box->max.x) box->max.x=vtx[i].x;
		if (vtx[i].y < box->min.y) box->min.y=vtx[i].y;
		else if (vtx[i].y > box->max.y) box->max.y = vtx[i].y;
		if (vtx[i].z < box->min.z) box->min.z=vtx[i].z;
		else if (vtx[i].z > box->max.z) box->max.z=vtx[i].z;
	}

	box->min.w = box->max.w = 0.0f;
}*/

// ===========================================================================================
//

void D3D9Mesh::BoundingBox(const NMVERTEX *vtx, DWORD n, D9BBox *box)
{
	XMVECTOR mi, mx;
	mi = mx = XMLoadFloat3((XMFLOAT3 *)&vtx[0].x);
	for (DWORD i = 1; i < n; i++) {
		XMVECTOR x = XMLoadFloat3((XMFLOAT3 *)&vtx[i].x);
		mi = XMVectorMin(mi, x);
		mx = XMVectorMax(mx, x);
	}
	XMStoreFloat4((XMFLOAT4 *)&box->min.x, XMVectorSetW(mi, 0));
	XMStoreFloat4((XMFLOAT4 *)&box->max.x, XMVectorSetW(mx, 0));
}

// ===========================================================================================
//
void D3D9Mesh::TransformGroup(DWORD n, const D3DXMATRIX *m)
{
	if (!pVB) return;

	bBSRecompute = true;

	Grp[n].Transform = Grp[n].Transform * (*m);
	Grp[n].bTransform = true;
	Grp[n].bUpdate = true;

	D3DXMatrixMultiply(&pGrpTF[n], &mTransform, &Grp[n].Transform);
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
		if (Grp[i].bTransform) D3DXMatrixMultiply(&pGrpTF[i], &mTransform, &Grp[i].Transform);
		else pGrpTF[i] = mTransform;
	}
}

// ===========================================================================================
//
void D3D9Mesh::SetPosition(VECTOR3 &pos)
{
	bGlobalTF = true;

	mTransform._41 = float(pos.x);
	mTransform._42 = float(pos.y);
	mTransform._43 = float(pos.z);

	D3DXMatrixInverse(&mTransformInv, NULL, &mTransform);

	for (DWORD i = 0; i<nGrp; i++) {
		if (Grp[i].bTransform) D3DXMatrixMultiply(&pGrpTF[i], &mTransform, &Grp[i].Transform);
		else pGrpTF[i] = mTransform;
	}
}

// ===========================================================================================
//
void D3D9Mesh::SetRotation(D3DXMATRIX &rot)
{
	bGlobalTF = true;
	
	memcpy(&mTransform, &rot, 48);
	
	D3DXMatrixInverse(&mTransformInv, NULL, &mTransform);

	for (DWORD i = 0; i<nGrp; i++) {
		if (Grp[i].bTransform) D3DXMatrixMultiply(&pGrpTF[i], &mTransform, &Grp[i].Transform);
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
		if (Grp[i].bUpdate || bBSRecomputeAll) {
			Grp[i].bUpdate = false;
			if (bGlobalTF) {
				if (Grp[i].bTransform)	D9UpdateAABB(&Grp[i].BBox, &mTransform, &Grp[i].Transform);
				else					D9UpdateAABB(&Grp[i].BBox, &mTransform);
			}
			else {
				if (Grp[i].bTransform)	D9UpdateAABB(&Grp[i].BBox, &Grp[i].Transform);
				else					D9UpdateAABB(&Grp[i].BBox);
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
			if (Grp[i].bTransform) {
				if (bGlobalTF) {
					D3DXMATRIX q;
					D3DXMatrixMultiply(&q, D3DXMatrixMultiply(&q, &mTransform, &Grp[i].Transform), &mTransformInv);
					D9AddAABB(&Grp[i].BBox, &q, &BBox, i==0);
				}
				else D9AddAABB(&Grp[i].BBox, &Grp[i].Transform, &BBox, i==0);
			}
			else {
				D9AddAABB(&Grp[i].BBox, NULL, &BBox, i==0);
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

	if (!pGB || !pIB) {
		LogErr("D3D9Mesh::Pick() Failed: No Geometry Available");
		return result;
	}

	UpdateBoundingBox();

	D3DXMATRIX mW, mWorldMesh;

	if (bGlobalTF) D3DXMatrixMultiply(&mWorldMesh, &mTransform, pW);
	else mWorldMesh = *pW;

	for (DWORD g=0;g<nGrp;g++) {

		if ((Grp[g].UsrFlag & 0x2) && (Grp[g].MFDScreenId != 0x100)) continue;

		D3DXVECTOR3 bs = D3DXVECTOR3f4(Grp[g].BBox.bs);
		float rad = Grp[g].BBox.bs.w;

		D3DXVec3TransformCoord(&bs, &bs, pW);

		BOOL bIntersect = D3DXSphereBoundProbe(&bs, rad, &D3DXVECTOR3(0,0,0), vDir);

		if (!bIntersect) continue;

		if (Grp[g].bTransform) D3DXMatrixMultiply(&mW, &pGrpTF[g], pW);
		else mW = mWorldMesh;         

		WORD *pIdc = NULL;
		D3DXVECTOR4 *pVrt = NULL;

		D3DXVECTOR3 _a, _b, _c, cp;

		HR(pIB->Lock(Grp[g].FaceOff*6, Grp[g].nFace*6, (LPVOID*)&pIdc, D3DLOCK_READONLY));
		HR(pGB->Lock(Grp[g].VertOff*sizeof(D3DXVECTOR4), Grp[g].nVert*sizeof(D3DXVECTOR4), (LPVOID*)&pVrt, D3DLOCK_READONLY));
		
		for (DWORD i=0;i<Grp[g].nFace;i++) {

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
		HR(pIB->Unlock());
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
		LogAlw("VertexCount = %u",Grp[i].nVert);
		LogAlw("FaceCount = %u",Grp[i].nFace);
	}*/
}



// ===========================================================================================
// SPECIAL RENDER FUNCTIONS SECTION
// ===========================================================================================
//

// This is a special rendering routine used to render 3D arrow --------------------------------
//
void D3D9Mesh::RenderAxisVector(LPD3DXMATRIX pW, const LPD3DXCOLOR pColor, float len)
{
	UINT numPasses = 0;
	HR(FX->SetTechnique(eAxisTech));
	HR(FX->SetFloat(eMix, len));
	HR(FX->SetValue(eColor, pColor, sizeof(D3DXCOLOR)));
	HR(FX->SetMatrix(eW, pW));
	HR(FX->Begin(&numPasses, D3DXFX_DONOTSAVESTATE));
	HR(FX->BeginPass(0));
	pDev->SetRenderState(D3DRS_CULLMODE, D3DCULL_CW);
	RenderGroup(0);
	pDev->SetRenderState(D3DRS_CULLMODE, D3DCULL_CCW);
	HR(FX->EndPass());
	HR(FX->End());
}

// Used only by ring manager --------------------------------------------------------------------
//
void D3D9Mesh::RenderRings(const LPD3DXMATRIX pW, LPDIRECT3DTEXTURE9 pTex)
{
	_TRACE;
	if (!pVB) return;
	if (!pTex) return;

	D3D9Stats.Mesh.Vertices += Grp[0].nVert;
	D3D9Stats.Mesh.MeshGrps++;

	UINT numPasses = 0;
	HR(FX->SetTechnique(eRingTech));
	HR(FX->SetMatrix(eW, pW));
	HR(FX->SetTexture(eTex0, pTex));
	if (sunLight) FX->SetValue(eSun, sunLight, sizeof(D3D9Sun));
	HR(FX->SetValue(eMtrl, &defmat, sizeof(D3D9MatExt)-4));
	HR(FX->Begin(&numPasses, D3DXFX_DONOTSAVESTATE));
	HR(FX->BeginPass(0));
	RenderGroup(0);
	HR(FX->EndPass());
	HR(FX->End());
}

// Used only by ring manager --------------------------------------------------------------------
//
void D3D9Mesh::RenderRings2(const LPD3DXMATRIX pW, LPDIRECT3DTEXTURE9 pTex, float irad, float orad)
{
	_TRACE;
	if (!pVB) return;
	if (!pTex) return;

	D3D9Stats.Mesh.Vertices += Grp[0].nVert;
	D3D9Stats.Mesh.MeshGrps++;

	UINT numPasses = 0;
	HR(FX->SetTechnique(eRingTech2));
	HR(FX->SetMatrix(eW, pW));
	HR(FX->SetTexture(eTex0, pTex));
	if (sunLight) FX->SetValue(eSun, sunLight, sizeof(D3D9Sun));
	HR(FX->SetValue(eMtrl, &defmat, sizeof(D3D9MatExt)-4));
	HR(FX->SetVector(eTexOff, &D3DXVECTOR4(irad, orad, 0, 0)));
	HR(FX->Begin(&numPasses, D3DXFX_DONOTSAVESTATE));
	HR(FX->BeginPass(0));
	RenderGroup(0);
	HR(FX->EndPass());
	HR(FX->End());
}





// ===========================================================================================
// MESH EDITING SECTION
// ===========================================================================================
//
DWORD D3D9Mesh::AddTexture(D3D9ClientSurface *pTex)
{
	SAFE_DELETEA(pTune);

	void *pBak = Tex;

	if (nTex==0) {
		nTex = 2;
		Tex = new LPD3D9CLIENTSURFACE[nTex];
		Tex[0] = NULL; Tex[1] = pTex;
		return nTex-1;
	} else if (pBak) {
		Tex = new LPD3D9CLIENTSURFACE[nTex+1];
		memcpy(Tex, pBak, sizeof(LPD3D9CLIENTSURFACE) * nTex);
		delete []pBak;
		Tex[nTex] = pTex;
		nTex++;
		return nTex-1;
	}
	return 0;
}


// ===========================================================================================
//
DWORD D3D9Mesh::AddMaterial(D3D9MatExt *pMat)
{
	void *pBak = Mtrl;
	Mtrl = new D3D9MatExt[nMtrl+1];
	if (pBak) {
		memcpy(Mtrl, pBak, sizeof(D3D9MatExt)*nMtrl);
		delete []pBak;
	}
	memcpy(&Mtrl[nMtrl], pMat, sizeof(D3D9MatExt));
	nMtrl++;
	return nMtrl-1;
}


// ===========================================================================================
//
void D3D9Mesh::SetMeshGroupTextureIdx(DWORD grp, DWORD tex_idx)
{
	Grp[grp].TexIdx = tex_idx;
}


// ===========================================================================================
//
void D3D9Mesh::SetMeshGroupMaterialIdx(DWORD grp, DWORD mtrl_idx)
{
	Grp[grp].MtrlIdx = mtrl_idx;
}


// ===========================================================================================
//
bool D3D9Mesh::Bake()
{
	UpdateBoundingBox();
	UpdateGeometryBuffer();
	CheckMeshStatus();
	return true;
}