// =================================================================================================================================
// The MIT Lisence:
//
// Copyright (C) 2016 Jarmo Nikkanen
//
// Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation 
// files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, 
// modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software 
// is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
// OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
// LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR
// IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
// =================================================================================================================================

#include "Mesh.h"
#include "MeshTools.h"


// ===========================================================================================
//
AdMesh::AdMesh()
{

}


// ===========================================================================================
//
AdMesh::AdMesh(MESHHANDLE hMesh)
{
	DWORD nGrp = oapiMeshGroupCount(hMesh);

	for (DWORD i=0;i<nGrp;i++) {
		const MESHGROUPEX *mg = oapiMeshGroupEx(hMesh, i); 
		if (mg) {
			/*HGROUP hGrp = */AddGroup(mg->Vtx, ADMESH_NT_VTX, mg->Idx, mg->nVtx, mg->nIdx);
		}
	}
}


// ===========================================================================================
//
AdMesh::AdMesh(const char *file, bool bValidate)
{
	MATERIAL DefMat;
	memset(&DefMat, 0, sizeof(MATERIAL));

	DefMat.diffuse.r = 1.0f;
	DefMat.diffuse.g = 1.0f;
	DefMat.diffuse.b = 1.0f;
	DefMat.diffuse.a = 1.0f;

	MESHHANDLE hMesh = oapiLoadMesh(file);

	DWORD nGrp = oapiMeshGroupCount(hMesh);
	DWORD nMat = oapiMeshMaterialCount(hMesh);
	DWORD nTex = oapiMeshTextureCount(hMesh);

	for (DWORD i=0;i<nGrp;i++) {
		const MESHGROUPEX *mg = oapiMeshGroupEx(hMesh, i); 
		if (mg) {
			HGROUP hGrp = AddGroup(mg->Vtx, ADMESH_NT_VTX, mg->Idx, mg->nVtx, mg->nIdx);
			hGrp->Mtrl = mg->MtrlIdx;
			hGrp->Tex = mg->TexIdx;
			if (bValidate) {
				hGrp->ComputeBounds();
				hGrp->NormalizeNormals();
			}
		}
	}

	for (DWORD i=0;i<nMat;i++) {
		MATERIAL *pMat = oapiMeshMaterial(hMesh, i);
		if (pMat) Materials.push_back(*pMat);
		else	  Materials.push_back(DefMat);
	}

	Textures.push_back(NULL);

	for (DWORD i=1;i<nTex;i++) {
		Textures.push_back(oapiGetTextureHandle(hMesh, i));
	}

	oapiDeleteMesh(hMesh);
}


// ===========================================================================================
//
AdMesh::~AdMesh()
{
	while (!Groups.empty()) {
		delete Groups.back();
		Groups.pop_back();
	}
}


// ===========================================================================================
//
HGROUP AdMesh::AddGroup()
{
	HGROUP hRec = new AdGroup();
	Groups.push_back(hRec);
	return hRec;
}


// ===========================================================================================
//
HGROUP AdMesh::AddGroup(const void *pV, DWORD fmt, const WORD *pI, DWORD nV, DWORD nI, const LPD3DXMATRIX pW)
{
	HGROUP hRec = new AdGroup();
	Groups.push_back(hRec);
	hRec->Append(pV, fmt, pI, nV, nI, pW);
	return hRec;
}


// ===========================================================================================
//
HGROUP AdMesh::AddGroup(HMESH hMesh, DWORD grp, const LPD3DXMATRIX pW)
{
	HGROUP hRec = new AdGroup();
	Groups.push_back(hRec);
	hRec->Append(hMesh, grp, pW);
	return hRec;
}


// ===========================================================================================
//
HGROUP AdMesh::AddGroup(const HGROUP hSrc, const LPD3DXMATRIX pW)
{	
	HGROUP hRec = new AdGroup();
	Groups.push_back(hRec);
	hRec->Append(hSrc, pW);
	return hRec;
}


// ===========================================================================================
//
HGROUP AdMesh::AddGroup(MESHHANDLE hMesh, DWORD grp, const LPD3DXMATRIX pW)
{	
	HGROUP hRec = new AdGroup();
	Groups.push_back(hRec);

	hRec->Append(hMesh, grp, pW);

	/*
	const MESHGROUPEX *mg = oapiMeshGroupEx(hMesh, grp);
	if (mg) {
		MATERIAL *pMat = oapiMeshMaterial(hMesh, mg->MtrlIdx);
		hRec->Mtrl = AddMaterial(oapiMeshMaterial(hMesh, mg->MtrlIdx));
		if (mg->TexIdx > 0) hRec->Tex = AddTexture(oapiGetTextureHandle(hMesh, mg->TexIdx));	
		else				hRec->Tex = AddTexture(NULL); 
	}*/

	return hRec;
}


// ===========================================================================================
//
void AdMesh::DelGroup(HGROUP hGrp)
{
	for (DWORD i=0;i<Groups.size();i++) if (Groups[i]==hGrp) { Groups.erase(Groups.begin()+i); delete hGrp; break; }	
}


// ===========================================================================================
//
DWORD AdMesh::GetVertexCount() const
{
	DWORD cnt = 0;
	for (DWORD i=0;i<Groups.size();i++) cnt += Groups[i]->Vtx.size();
	return cnt;
}


// ===========================================================================================
//
DWORD AdMesh::GetIndexCount() const
{
	DWORD cnt = 0;
	for (DWORD i=0;i<Groups.size();i++) cnt += Groups[i]->Idx.size();
	return cnt;
}


// ===========================================================================================
//
HGROUP AdMesh::GetGroup(DWORD index)
{
	if (index<Groups.size()) return Groups[index];
	return NULL;
}


// ===========================================================================================
//
/*
bool AdMesh::Load(const char *file)
{
	
	FILE *fp = fopen(file, "rt");
	if (!fp) return false;

	DWORD mtrl=0, tex=0, groups=0, nv=0, ni=0;

	while (fgets(line, 256, fp)) {

		if (!strcmp(line, "GEOM")) {
			sscanf_s(line, "GEOM %u %u", &nv, &ni); 
			ni*=3;
			NTVERTEX *pVtx = new NTVERTEX[nv];
			WORD *pIdx = new WORD[ni];
			for (DWORD i=0;i<nv;i++) {
				float a, b, c, d, e, f, g, h;
				int c = sscanf_s(line, "%f %f %f %f %f %f %f %f", &a, &b, &c, &d, &e, &f, &g, &h);
				if (c>=3) pVtx[i].x  = a, pVtx[i].y  = b, pVtx[i].z = c;
				if (c==5) pVtx[i].tu = d, pVtx[i].tv = e;
				if (c>=6) pVtx[i].nx = d, pVtx[i].ny = e, pVtx[i].nz = f;
				if (c==8) pVtx[i].tu = g, pVtx[i].tv = h;
			}
			for (DWORD i=0;i<ni;i+=3) {
				WORD a, b, c;
				int c = sscanf_s(line, "%hu %hu %hu", &a, &b, &c);
				if (c==3) {
					pIdx[i+0] = a;
					pIdx[i+1] = b;
					pIdx[i+2] = c;
				}
			}
			AddGroup(pVtx, ADMESH_NT_VTX, pIdx, nv, ni); 
		}
		else {
			if (!strcmp(line, "MATERIAL")) sscanf_s(line, "MATERIAL %u", &mtrl);
			if (!strcmp(line, "TEXTURE")) sscanf_s(line, "TEXTURE %u", &tex);
			if (!strcmp(line, "GROUPS")) sscanf_s(line, "GROUPS %u", &groups);
		}
	}
	
	return false;
}*/





// ===========================================================================================
// ADGROUP
// ===========================================================================================
//
AdGroup::AdGroup() : vBSc(), vBSr(0), Mtrl(0), Tex (0)
{
}


// ===========================================================================================
//
AdGroup::~AdGroup()
{
}


// ===========================================================================================
//
void AdGroup::GetVertices(NMVERTEX *pData)
{
	for (DWORD i=0;i<Vtx.size();i++) {
		pData[i].x  = Vtx[i].pos.x;	pData[i].y  = Vtx[i].pos.y;	pData[i].z  = Vtx[i].pos.z;
		pData[i].nx = Vtx[i].nrm.x;	pData[i].ny = Vtx[i].nrm.y;	pData[i].nz = Vtx[i].nrm.z;
		pData[i].u  = Vtx[i].tu;    pData[i].v  = Vtx[i].tv;	pData[i].w  = 0.0f;
		pData[i].tx = 0.0f;			pData[i].ty = 0.0f;			pData[i].tz = 0.0f;
	}
}


// ===========================================================================================
//
void AdGroup::GetIndices(WORD *pData)
{
	for (DWORD i=0;i<Idx.size();i++) pData[i] = Idx[i];
}


// ===========================================================================================
//
void AdGroup::NormalizeNormals()
{
	for (DWORD i=0;i<Vtx.size();i++) D3DXVec3Normalize(&Vtx[i].nrm, &Vtx[i].nrm);
}


// ===========================================================================================
//
void AdGroup::ComputeBounds()
{
	D3DXComputeBoundingSphere(&Vtx[0].pos, Vtx.size(), sizeof(NATIVE), &vBSc, &vBSr);
}


// ===========================================================================================
//
void AdGroup::NormalizeGroupSize()
{
	ComputeBounds();
	float scale = 1.0f / vBSr;
	for (DWORD i=0;i<Vtx.size();i++) Vtx[i].pos *= scale;
}

// ===========================================================================================
//
DWORD AdGroup::GetVertexCount() const
{
	return Vtx.size();
}


// ===========================================================================================
//
DWORD AdGroup::GetIndexCount() const
{
	return Idx.size();
}


// ===========================================================================================
//
DWORD AdGroup::Append(const void *pV, DWORD fmt, const WORD *pI, DWORD nV, DWORD nI, const LPD3DXMATRIX pW)
{
	NTVERTEX *pNT = (NTVERTEX *)pV;
	NMVERTEX *pNM = (NMVERTEX *)pV;

	if ((Vtx.size()+nV) > 65535) return 0x10000;

	WORD ibase = WORD(Vtx.size());
	Vtx.reserve(Vtx.size() + nV);

	for (DWORD i=0;i<nV;i++) {
		if (fmt==ADMESH_NT_VTX) Vtx.push_back(_NT(&pNT[i]));
		else					Vtx.push_back(_NM(&pNM[i]));
		if (pW) TransformVertex(&Vtx.back(), pW);
	}
	
	for (DWORD i=0;i<nI;i++) Idx.push_back(ibase + pI[i]);
	
	return Vtx.size();
}


// ===========================================================================================
//
DWORD AdGroup::Append(const HGROUP hSrc, const LPD3DXMATRIX pW)
{
	std::vector<NATIVE> &svtx = hSrc->Vtx;
	std::vector<WORD>   &sidx = hSrc->Idx;

	if ((svtx.size()+Vtx.size()) > 65535) return 0x10000;

	WORD ibase = WORD(Vtx.size());
	Vtx.reserve(Vtx.size() + svtx.size());

	for (DWORD i=0;i<svtx.size();i++) {
		Vtx.push_back(svtx[i]);
		if (pW) TransformVertex(&Vtx.back(), pW);
	}
	
	for (DWORD i=0;i<sidx.size();i++) Idx.push_back(ibase + sidx[i]);
	
	return Vtx.size();
}


// ===========================================================================================
//
DWORD AdGroup::Append(HMESH hMesh, DWORD grp, const LPD3DXMATRIX pW)
{
	DWORD nVtx = 0;
	NMVERTEX *pV = hMesh->LockVertexBuffer(grp, D3DLOCK_READONLY);
	WORD *pI = hMesh->LockIndexBuffer(grp, D3DLOCK_READONLY);
	if (pV && pI) nVtx = Append(pV, ADMESH_NM_VTX, pI, hMesh->GetVertexCount(grp), hMesh->GetIndexCount(grp), pW);
	if (pV) hMesh->UnLockVertexBuffer();
	if (pI) hMesh->UnLockIndexBuffer();
	return nVtx;
}


// ===========================================================================================
//
DWORD AdGroup::Append(MESHHANDLE hMesh, DWORD grp, const LPD3DXMATRIX pW)
{
	const MESHGROUPEX *mg = oapiMeshGroupEx(hMesh, grp); 
	if (mg) return Append(mg->Vtx, ADMESH_NT_VTX, mg->Idx, mg->nVtx, mg->nIdx, pW);
	return 0;
}