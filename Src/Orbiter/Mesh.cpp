// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#include "Mesh.h"
#include <stdio.h>
#include "D3dmath.h"
#include "Orbiter.h"
#include "Log.h"
#include "Util.h"

using namespace std;

extern Orbiter *g_pOrbiter;
extern DWORD g_vtxcount;
extern char DBG_MSG[256];

#ifndef __linux__
static D3DMATERIAL7 defmat = {{1,1,1,1},{1,1,1,1},{0,0,0,1},{0,0,0,1},0};
#else // __linux__
static MATERIAL defmat = {{1,1,1,1},{1,1,1,1},{0,0,0,1},{0,0,0,1},0};
#endif // __linux__

// =======================================================================
// Class Triangle

Triangle::Triangle ()
{
	hasNodes = false;
	hasNormals = false;
}

Triangle::Triangle (const Triangle &tri)
{
	if (hasNodes = tri.hasNodes)
		for (int i = 0; i < 3; i++) nd[i] = tri.nd[i];
	if (hasNormals = tri.hasNormals)
		for (int i = 0; i < 3; i++) nm[i] = tri.nm[i];
}

Triangle::Triangle (int n0, int n1, int n2)
{
	hasNodes = true;
	hasNormals = false;
	nd[0] = n0;
	nd[1] = n1;
	nd[2] = n2;
}

void Triangle::SetNodes (int n0, int n1, int n2)
{
	hasNodes = true;
	nd[0] = n0;
	nd[1] = n1;
	nd[2] = n2;
}

// =======================================================================
// Class Mesh

Mesh::Mesh ()
{
	name = NULL;
	nGrp = nMtrl = nTex = 0;
	GrpVis   = 0;
	GrpSetup = false;
	bModulateMatAlpha = false;
}

Mesh::Mesh (NTVERTEX *vtx, DWORD nvtx, WORD *idx, DWORD nidx, DWORD matidx, DWORD texidx)
{
	name = NULL;
	nGrp = nMtrl = nTex = 0;
	GrpVis   = 0;
	GrpSetup = false;
	AddGroup (vtx, nvtx, idx, nidx, matidx, texidx);
	bModulateMatAlpha = false;
	Setup();
}

Mesh::Mesh (const Mesh &mesh)
{
	name = NULL;
	nGrp = nMtrl = nTex = 0;
	GrpVis = 0;
	GrpSetup = false;
	Set (mesh);
}

void Mesh::Set (const Mesh &mesh)
{
	DWORD i;

	Clear ();
	if (nGrp = mesh.nGrp) {
		Grp = new GroupSpec[nGrp]; TRACENEW
		memcpy (Grp, mesh.Grp, nGrp*sizeof(GroupSpec));
		for (i = 0; i < nGrp; i++) {
			Grp[i].Vtx = new NTVERTEX[Grp[i].nVtx]; TRACENEW
			memcpy (Grp[i].Vtx, mesh.Grp[i].Vtx, Grp[i].nVtx*sizeof(NTVERTEX));
			Grp[i].Idx = new WORD[Grp[i].nIdx]; TRACENEW
			memcpy (Grp[i].Idx, mesh.Grp[i].Idx, Grp[i].nIdx*sizeof(WORD)); 
#ifndef __linux__
			if (Grp[i].VtxBuf) {
				Grp[i].VtxBuf = 0;
				MakeGroupVertexBuffer (i);
			}
#else // __linux__
			// VtxBuf copy left out: vertex buffers belong to the graphics client
#endif // __linux__
		}
	}
	if (nMtrl = mesh.nMtrl) {
#ifndef __linux__
		Mtrl = new D3DMATERIAL7[nMtrl]; TRACENEW
		memcpy (Mtrl, mesh.Mtrl, nMtrl*sizeof(D3DMATERIAL7));
#else // __linux__
		Mtrl = new MATERIAL[nMtrl]; TRACENEW
		memcpy (Mtrl, mesh.Mtrl, nMtrl*sizeof(MATERIAL));
#endif // __linux__
	}
	if (nTex = mesh.nTex) {
		Tex = new SURFHANDLE[nTex]; TRACENEW
		memcpy (Tex, mesh.Tex, nTex*sizeof(SURFHANDLE));
	}
	if (GrpSetup = mesh.GrpSetup) {
#ifndef __linux__
		GrpCnt = new D3DVECTOR[nGrp]; TRACENEW
		memcpy (GrpCnt, mesh.GrpCnt, nGrp*sizeof(D3DVECTOR));
		GrpRad = new D3DVALUE[nGrp]; TRACENEW
		memcpy (GrpRad, mesh.GrpRad, nGrp*sizeof(D3DVALUE));
#else // __linux__
		GrpCnt = new oapi::FVECTOR3[nGrp]; TRACENEW
		memcpy (GrpCnt, mesh.GrpCnt, nGrp*sizeof(oapi::FVECTOR3));
		GrpRad = new float[nGrp]; TRACENEW
		memcpy (GrpRad, mesh.GrpRad, nGrp*sizeof(float));
#endif // __linux__
		GrpVis = new DWORD[nGrp]; TRACENEW
		memcpy (GrpVis, mesh.GrpVis, nGrp*sizeof(DWORD));
	} else {
		GrpVis = 0;
	}
	SetName(mesh.GetName());
	bModulateMatAlpha = mesh.bModulateMatAlpha;
}

Mesh::~Mesh ()
{
	Clear ();
}

void Mesh::Setup ()
{
	DWORD g;
	if (GrpVis) { // allocated already
		delete []GrpCnt;
		GrpCnt = NULL;
		delete []GrpRad;
		GrpRad = NULL;
		delete []GrpVis;
		GrpVis = 0;
	}
#ifndef __linux__
	GrpCnt  = new D3DVECTOR[nGrp]; TRACENEW
	GrpRad  = new D3DVALUE[nGrp]; TRACENEW
#else // __linux__
	GrpCnt  = new oapi::FVECTOR3[nGrp]; TRACENEW
	GrpRad  = new float[nGrp]; TRACENEW
#endif // __linux__
	GrpVis  = new DWORD[nGrp]; TRACENEW
	GrpSetup = true;
	for (g = 0; g < nGrp; g++) {
		SetupGroup (g);
		// check validity of material/texture indices
		if (Grp[g].MtrlIdx != SPEC_INHERIT && Grp[g].MtrlIdx >= nMtrl) Grp[g].MtrlIdx = SPEC_DEFAULT;
		if (Grp[g].TexIdx != SPEC_INHERIT && Grp[g].TexIdx >= nTex) Grp[g].TexIdx = SPEC_DEFAULT;
	}
}

void Mesh::SetupGroup (DWORD grp)
{
	DWORD i;
#ifndef __linux__
	D3DVALUE x, y, z, dx, dy, dz, d2, d2max;
	D3DVALUE invtx = (D3DVALUE)(1.0/Grp[grp].nVtx);
#else // __linux__
	float x, y, z, dx, dy, dz, d2, d2max;
	float invtx = (float)(1.0/Grp[grp].nVtx);
#endif // __linux__
	x = y = z = 0.0f;
	for (i = 0; i < Grp[grp].nVtx; i++) {
		x += Grp[grp].Vtx[i].x;
		y += Grp[grp].Vtx[i].y;
		z += Grp[grp].Vtx[i].z;
	}
	GrpCnt[grp].x = (x *= invtx);
	GrpCnt[grp].y = (y *= invtx);
	GrpCnt[grp].z = (z *= invtx);
	d2max = 0.0f;
	for (i = 0; i < Grp[grp].nVtx; i++) {
		dx = x - Grp[grp].Vtx[i].x;
		dy = y - Grp[grp].Vtx[i].y;
		dz = z - Grp[grp].Vtx[i].z;
		d2 = dx*dx + dy*dy + dz*dz;
		if (d2 > d2max) d2max = d2;
	}
	GrpRad[grp] = (FLOAT)sqrt (d2max);
}

int Mesh::AddGroup (NTVERTEX *vtx, DWORD nvtx, WORD *idx, DWORD nidx,
	DWORD mtrl_idx, DWORD tex_idx, WORD zbias, DWORD flag, bool deepcopy)
{
	DWORD n;
	GroupSpec *g, *tmp_Grp = new GroupSpec[nGrp+1]; TRACENEW
#ifndef __linux__
	D3DVECTOR *tmp_Cnt = new D3DVECTOR[nGrp+1]; TRACENEW
	D3DVALUE *tmp_Rad = new D3DVALUE[nGrp+1]; TRACENEW
#else // __linux__
	oapi::FVECTOR3 *tmp_Cnt = new oapi::FVECTOR3[nGrp+1]; TRACENEW
	float *tmp_Rad = new float[nGrp+1]; TRACENEW
#endif // __linux__
	DWORD *tmp_Vis = new DWORD[nGrp+1]; TRACENEW
	if (nGrp) {
		memcpy (tmp_Grp, Grp, nGrp*sizeof(GroupSpec));
#ifndef __linux__
		memcpy (tmp_Cnt, GrpCnt, nGrp*sizeof(D3DVECTOR));
		memcpy (tmp_Rad, GrpRad, nGrp*sizeof(D3DVALUE));
#else // __linux__
		memcpy (tmp_Cnt, GrpCnt, nGrp*sizeof(oapi::FVECTOR3));
		memcpy (tmp_Rad, GrpRad, nGrp*sizeof(float));
#endif // __linux__
		memcpy (tmp_Vis, GrpVis, nGrp*sizeof(DWORD));
		delete []Grp;
		Grp = NULL;
		delete []GrpCnt;
		GrpCnt = NULL;
		delete []GrpRad;
		GrpRad = NULL;
		delete []GrpVis;
		GrpVis = 0;
	}
	Grp = tmp_Grp;
	GrpCnt = tmp_Cnt;
	GrpRad = tmp_Rad;
	GrpVis = tmp_Vis;
	g = Grp+nGrp;
	if (deepcopy) {
		g->Vtx = new NTVERTEX[nvtx]; 
		if (vtx) memcpy (g->Vtx, vtx, nvtx*sizeof(NTVERTEX));
		else     memset (g->Vtx, 0, nvtx*sizeof(NTVERTEX));
		g->Idx = new WORD[nidx];
		if (idx) memcpy (g->Idx, idx, nidx*sizeof(WORD));
		else     memset (g->Idx, 0, nidx*sizeof(WORD));
	} else {
		g->Vtx = vtx;
		g->Idx = idx;
	}
	g->nVtx = nvtx;
	g->nIdx = nidx;
	g->MtrlIdx = mtrl_idx;
	g->TexIdx = tex_idx;
	for (n = 0; n < MAXTEX; n++) {
		g->TexIdxEx[n] = SPEC_DEFAULT;
		g->TexMixEx[n] = 0.0f;
	}
	g->zBias = zbias;
	g->Flags = 0;
	g->UsrFlag = flag;
#ifndef __linux__
	g->VtxBuf = 0;
#else // __linux__
	// VtxBuf left out: vertex buffers belong to the graphics client
#endif // __linux__
	if (GrpSetup) {
		SetupGroup (nGrp);
		if (g->MtrlIdx != SPEC_INHERIT && g->MtrlIdx >= nMtrl)
			g->MtrlIdx = SPEC_DEFAULT;
		if (g->TexIdx != SPEC_INHERIT && g->TexIdx >= nTex)
			g->TexIdx = SPEC_DEFAULT;
	}
	return nGrp++;
}

bool Mesh::AddGroupBlock (DWORD grp, const NTVERTEX *vtx, DWORD nvtx, const WORD *idx, DWORD nidx)
{
	if (grp >= nGrp) return false;

	GroupSpec *g = Grp+grp;
	WORD vofs = (WORD)g->nVtx;

	NTVERTEX *v = new NTVERTEX[g->nVtx+nvtx];
	if (g->nVtx) {
		memcpy (v, g->Vtx, g->nVtx*sizeof(NTVERTEX));
		delete []g->Vtx;
		g->Vtx = NULL;
	}
	if (nvtx) {
		memcpy (v+g->nVtx, vtx, nvtx*sizeof(NTVERTEX));
	}
	g->Vtx = v;
	g->nVtx += nvtx;

	WORD *i = new WORD[g->nIdx+nidx];
	if (g->nIdx) {
		memcpy (i, g->Idx, g->nIdx*sizeof(WORD));
		delete []g->Idx;
		g->Idx = NULL;
	}
	if (nidx) {
		for (DWORD j = 0; j < nidx; j++)
			i[g->nIdx+j] = idx[j] + vofs;
	}
	g->Idx = i;
	g->nIdx += nidx;

	return true;
}

#ifndef __linux__
bool Mesh::MakeGroupVertexBuffer (DWORD grp)
{
	return false;
}
#else // __linux__
// MakeGroupVertexBuffer left out: it was an empty stub (vertex buffers belong to the graphics client)
#endif // __linux__

void Mesh::AddMesh (Mesh &mesh)
{
	for (DWORD i = 0; i < mesh.nGroup(); i++) {
		NTVERTEX *vtx2;
		WORD *idx2;
		GroupSpec *gs = mesh.GetGroup (i);
		// need to copy the vertex and index lists
		vtx2 = new NTVERTEX[gs->nVtx];  memcpy (vtx2, gs->Vtx, gs->nVtx*sizeof(NTVERTEX)); TRACENEW
		idx2 = new WORD[gs->nIdx];      memcpy (idx2, gs->Idx, gs->nIdx*sizeof(WORD)); TRACENEW
		AddGroup (vtx2, gs->nVtx, idx2, gs->nIdx);
	}
}

bool Mesh::DeleteGroup (DWORD grp)
{
	if (nGrp == 1) { // delete the only group
		delete []Grp;
		Grp = NULL;
		nGrp = 0;
	} else if (grp < nGrp && grp > 0) { // delete selected group
		if (Grp[grp].Vtx) { delete []Grp[grp].Vtx; Grp[grp].Vtx = NULL; }
		if (Grp[grp].Idx) { delete []Grp[grp].Idx; Grp[grp].Idx = NULL; }
#ifndef __linux__
		if (Grp[grp].VtxBuf) Grp[grp].VtxBuf->Release();
#else // __linux__
		// VtxBuf release left out: vertex buffers belong to the graphics client
#endif // __linux__

		// redo group
		GroupSpec * tmp_Grp = new GroupSpec[nGrp - 1]; TRACENEW
		for (DWORD i = 0, j = 0; i < nGrp; i++)
			if (i != grp) tmp_Grp[j++] = Grp[i];
		delete []Grp;
		Grp = tmp_Grp;
		nGrp--;
	} else {
		return false;
	}
	return true;
}

int Mesh::GetGroup (DWORD grp, GROUPREQUESTSPEC *grs)
{
	static NTVERTEX zero = {0,0,0, 0,0,0, 0,0};
	if (grp >= nGrp) return 1;
	GroupSpec *g = Grp+grp;
	DWORD nv = g->nVtx;
	DWORD ni = g->nIdx;
	DWORD i, vi;
	int ret = 0;

	if (grs->nVtx && grs->Vtx) { // vertex data requested
		if (grs->VtxPerm) { // random access data request
			for (i = 0; i < grs->nVtx; i++) {
				vi = grs->VtxPerm[i];
				if (vi < nv) {
					grs->Vtx[i] = g->Vtx[vi];
				} else {
					grs->Vtx[i] = zero;
					ret = 1;
				}
			}
		} else {
			if (grs->nVtx > nv) grs->nVtx = nv;
			memcpy (grs->Vtx, g->Vtx, grs->nVtx * sizeof(NTVERTEX));
		}
	}

	if (grs->nIdx && grs->Idx) { // index data requested
		if (grs->IdxPerm) { // random access data request
			for (i = 0; i < grs->nIdx; i++) {
				vi = grs->IdxPerm[i];
				if (vi < ni) {
					grs->Idx[i] = g->Idx[vi];
				} else {
					grs->Idx[i] = 0;
					ret = 1;
				}
			}
		} else {
			if (grs->nIdx > ni) grs->nIdx = ni;
			memcpy (grs->Idx, g->Idx, grs->nIdx * sizeof(WORD));
		}
	}

	grs->MtrlIdx = g->MtrlIdx;
	grs->TexIdx = g->TexIdx;
	return ret;
}

int Mesh::EditGroup (DWORD grp, GROUPEDITSPEC *ges)
{
	if (grp >= nGrp) return 1;
	GroupSpec *g = Grp+grp;
	DWORD i, vi;

	DWORD flag = ges->flags;
	if (flag & GRPEDIT_SETUSERFLAG)
		g->UsrFlag = ges->UsrFlag;
	else if (flag & GRPEDIT_ADDUSERFLAG)
		g->UsrFlag |= ges->UsrFlag;
	else if (flag & GRPEDIT_DELUSERFLAG)
		g->UsrFlag &= ~ges->UsrFlag;

	if (flag & GRPEDIT_VTXMOD) {
		for (i = 0; i < ges->nVtx; i++) {
			vi = (ges->vIdx ? ges->vIdx[i] : i);
			if (vi < g->nVtx) {
				if      (flag & GRPEDIT_VTXCRDX)    g->Vtx[vi].x   = ges->Vtx[i].x;
				else if (flag & GRPEDIT_VTXCRDADDX) g->Vtx[vi].x  += ges->Vtx[i].x;
				if      (flag & GRPEDIT_VTXCRDY)    g->Vtx[vi].y   = ges->Vtx[i].y;
				else if (flag & GRPEDIT_VTXCRDADDY) g->Vtx[vi].y  += ges->Vtx[i].y;
				if      (flag & GRPEDIT_VTXCRDZ)    g->Vtx[vi].z   = ges->Vtx[i].z;
				else if (flag & GRPEDIT_VTXCRDADDZ) g->Vtx[vi].z +=  ges->Vtx[i].z;
				if      (flag & GRPEDIT_VTXNMLX)    g->Vtx[vi].nx  = ges->Vtx[i].nx;
				else if (flag & GRPEDIT_VTXNMLADDX) g->Vtx[vi].nx += ges->Vtx[i].nx;
				if      (flag & GRPEDIT_VTXNMLY)    g->Vtx[vi].ny  = ges->Vtx[i].ny;
				else if (flag & GRPEDIT_VTXNMLADDY) g->Vtx[vi].ny += ges->Vtx[i].ny;
				if      (flag & GRPEDIT_VTXNMLZ)    g->Vtx[vi].nz  = ges->Vtx[i].nz;
				else if (flag & GRPEDIT_VTXNMLADDZ) g->Vtx[vi].nz += ges->Vtx[i].nz;
				if      (flag & GRPEDIT_VTXTEXU)    g->Vtx[vi].tu  = ges->Vtx[i].tu;
				else if (flag & GRPEDIT_VTXTEXADDU) g->Vtx[vi].tu += ges->Vtx[i].tu;
				if      (flag & GRPEDIT_VTXTEXV)    g->Vtx[vi].tv  = ges->Vtx[i].tv;
				else if (flag & GRPEDIT_VTXTEXADDV) g->Vtx[vi].tv += ges->Vtx[i].tv;
			}
		}
	}
	if (GrpSetup) SetupGroup(grp);
	return 0;
}

#ifndef __linux__
int Mesh::AddMaterial (D3DMATERIAL7 &mtrl)
#else // __linux__
int Mesh::AddMaterial (MATERIAL &mtrl)
#endif // __linux__
{
#ifndef __linux__
	D3DMATERIAL7 *tmp_Mtrl = new D3DMATERIAL7[nMtrl+1]; TRACENEW
	memcpy (tmp_Mtrl, Mtrl, sizeof(D3DMATERIAL7)*nMtrl);
	memcpy (tmp_Mtrl+nMtrl, &mtrl, sizeof(D3DMATERIAL7));
#else // __linux__
	MATERIAL *tmp_Mtrl = new MATERIAL[nMtrl+1]; TRACENEW
	memcpy (tmp_Mtrl, Mtrl, sizeof(MATERIAL)*nMtrl);
	memcpy (tmp_Mtrl+nMtrl, &mtrl, sizeof(MATERIAL));
#endif // __linux__
	if (nMtrl) {
		delete []Mtrl;
		Mtrl = NULL;
	}
	Mtrl = tmp_Mtrl;
	return nMtrl++;
}

bool Mesh::DeleteMaterial (DWORD matidx)
{
	DWORD i, j;
	if (matidx >= nMtrl) return false;

	// adjust group material indices
	for (i = 0; i < nGrp; i++) {
		if (Grp[i].MtrlIdx >= matidx) {
			if (Grp[i].MtrlIdx == matidx) Grp[i].MtrlIdx = 0;
			else Grp[i].MtrlIdx--;
		}
	}

	// remove material from the list
#ifndef __linux__
	D3DMATERIAL7 *tmp_Mtrl = 0;
#else // __linux__
	MATERIAL *tmp_Mtrl = 0;
#endif // __linux__
	if (nMtrl > 1) {
#ifndef __linux__
		tmp_Mtrl = new D3DMATERIAL7[nMtrl-1]; TRACENEW
#else // __linux__
		tmp_Mtrl = new MATERIAL[nMtrl-1]; TRACENEW
#endif // __linux__
		for (i = j = 0; i < nMtrl; i++) {
#ifndef __linux__
			if (i != matidx) memcpy (tmp_Mtrl+j++, Mtrl+i, sizeof(D3DMATERIAL7));
#else // __linux__
			if (i != matidx) memcpy (tmp_Mtrl+j++, Mtrl+i, sizeof(MATERIAL));
#endif // __linux__
		}
	}
	delete []Mtrl;
	Mtrl = tmp_Mtrl;
	nMtrl--;
	return true;
}

void Mesh::Clear ()
{
	for (DWORD i = 0; i < nGrp; i++) {
		delete []Grp[i].Vtx;
		delete []Grp[i].Idx;
		Grp[i].Vtx = NULL;
		Grp[i].Idx = NULL;
#ifndef __linux__
		if (Grp[i].VtxBuf) Grp[i].VtxBuf->Release();
#else // __linux__
		// VtxBuf release left out: vertex buffers belong to the graphics client
#endif // __linux__
	}
	if (nGrp) {
		delete []Grp;
		Grp = NULL;
		nGrp = 0;
	}
	if (nMtrl) {
		delete []Mtrl;
		Mtrl = NULL;
		nMtrl = 0;
	}
	if (GrpVis) {
		delete []GrpCnt;
		GrpCnt = NULL;
		delete []GrpRad;
		GrpRad = NULL;
		delete []GrpVis;
		GrpVis = 0;
	}
	if (name) {
		delete []name;
		name = NULL;
	}
	GrpSetup = false;
	ReleaseTextures ();
}

#ifndef __linux__
void Mesh::ScaleGroup (DWORD grp, D3DVALUE sx, D3DVALUE sy, D3DVALUE sz)
#else // __linux__
void Mesh::ScaleGroup (DWORD grp, float sx, float sy, float sz)
#endif // __linux__
{
	int i, nv = Grp[grp].nVtx;
	NTVERTEX *vtx = Grp[grp].Vtx;
	for (i = 0; i < nv; i++) {
		vtx[i].x *= sx;
		vtx[i].y *= sy;
		vtx[i].z *= sz;
	}
	if (sx == sy && sx == sz) return; // no change in normals
#ifndef __linux__
	D3DVALUE snx = sy*sz, sny = sx*sz, snz = sx*sy;
#else // __linux__
	float snx = sy*sz, sny = sx*sz, snz = sx*sy;
#endif // __linux__
	for (i = 0; i < nv; i++) {
		vtx[i].nx *= snx;
		vtx[i].ny *= sny;
		vtx[i].nz *= snz;
#ifndef __linux__
		D3DVALUE ilen = (D3DVALUE)(1.0/sqrt (vtx[i].nx*vtx[i].nx + vtx[i].ny*vtx[i].ny + vtx[i].nz*vtx[i].nz));
#else // __linux__
		float ilen = (float)(1.0/sqrt (vtx[i].nx*vtx[i].nx + vtx[i].ny*vtx[i].ny + vtx[i].nz*vtx[i].nz));
#endif // __linux__
		vtx[i].nx *= ilen;
		vtx[i].ny *= ilen;
		vtx[i].nz *= ilen;
	}
	if (GrpSetup) SetupGroup (grp);
}

#ifndef __linux__
void Mesh::Scale (D3DVALUE sx, D3DVALUE sy, D3DVALUE sz)
#else // __linux__
void Mesh::Scale (float sx, float sy, float sz)
#endif // __linux__
{
	for (DWORD grp = 0; grp < nGrp; grp++)
		ScaleGroup (grp, sx, sy, sz);
}

#ifndef __linux__
void Mesh::TranslateGroup (DWORD grp, D3DVALUE dx, D3DVALUE dy, D3DVALUE dz)
#else // __linux__
void Mesh::TranslateGroup (DWORD grp, float dx, float dy, float dz)
#endif // __linux__
{
	int i, nv = Grp[grp].nVtx;
	NTVERTEX *vtx = Grp[grp].Vtx;
	for (i = 0; i < nv; i++) {
		vtx[i].x += dx;
		vtx[i].y += dy;
		vtx[i].z += dz;
	}
	if (GrpSetup) {
		GrpCnt[grp].x += dx;
		GrpCnt[grp].y += dy;
		GrpCnt[grp].z += dz;
	}
#ifndef __linux__
	if (Grp[grp].VtxBuf) { // make this more efficient!
		Grp[grp].VtxBuf->Release();
		Grp[grp].VtxBuf = 0;
	}
#else // __linux__
	// VtxBuf release left out: vertex buffers belong to the graphics client
#endif // __linux__
}

#ifndef __linux__
void Mesh::Translate (D3DVALUE dx, D3DVALUE dy, D3DVALUE dz)
#else // __linux__
void Mesh::Translate (float dx, float dy, float dz)
#endif // __linux__
{
	for (DWORD grp = 0; grp < nGrp; grp++)
		TranslateGroup (grp, dx, dy, dz);
}

#ifndef __linux__
void Mesh::RotateGroup (DWORD grp, RotAxis axis, D3DVALUE angle)
#else // __linux__
void Mesh::RotateGroup (DWORD grp, RotAxis axis, float angle)
#endif // __linux__
{
	int i, nv = Grp[grp].nVtx;
	NTVERTEX *vtx = Grp[grp].Vtx;
#ifndef __linux__
	D3DVALUE cosa = (D3DVALUE)cos(angle), sina = (D3DVALUE)sin(angle);
#else // __linux__
	float cosa = (float)cos(angle), sina = (float)sin(angle);
#endif // __linux__
	switch (axis) {
	case ROTATE_X:
		for (i = 0; i < nv; i++) {
#ifndef __linux__
			D3DVALUE y = vtx[i].y, z = vtx[i].z;
#else // __linux__
			float y = vtx[i].y, z = vtx[i].z;
#endif // __linux__
			vtx[i].y = cosa*y - sina*z;
			vtx[i].z = sina*y + cosa*z;
#ifndef __linux__
			D3DVALUE ny = vtx[i].ny, nz = vtx[i].nz;
#else // __linux__
			float ny = vtx[i].ny, nz = vtx[i].nz;
#endif // __linux__
			vtx[i].ny = cosa*ny - sina*nz;
			vtx[i].nz = sina*ny + cosa*nz;
		}
		if (GrpSetup) {
#ifndef __linux__
			D3DVALUE y = GrpCnt[grp].y, z = GrpCnt[grp].z;
#else // __linux__
			float y = GrpCnt[grp].y, z = GrpCnt[grp].z;
#endif // __linux__
			GrpCnt[grp].y = cosa*y - sina*z;
			GrpCnt[grp].z = sina*y + cosa*z;
		}
		break;
	case ROTATE_Y:
		for (i = 0; i < nv; i++) {
#ifndef __linux__
			D3DVALUE x = vtx[i].x, z = vtx[i].z;
#else // __linux__
			float x = vtx[i].x, z = vtx[i].z;
#endif // __linux__
			vtx[i].x = cosa*x - sina*z;
			vtx[i].z = sina*x + cosa*z;
#ifndef __linux__
			D3DVALUE nx = vtx[i].nx, nz = vtx[i].nz;
#else // __linux__
			float nx = vtx[i].nx, nz = vtx[i].nz;
#endif // __linux__
			vtx[i].nx = cosa*nx - sina*nz;
			vtx[i].nz = sina*nx + cosa*nz;
		}
		if (GrpSetup) {
#ifndef __linux__
			D3DVALUE x = GrpCnt[grp].x, z = GrpCnt[grp].z;
#else // __linux__
			float x = GrpCnt[grp].x, z = GrpCnt[grp].z;
#endif // __linux__
			GrpCnt[grp].x = cosa*x - sina*z;
			GrpCnt[grp].z = sina*x + cosa*z;
		}
		break;
	case ROTATE_Z:
		for (i = 0; i < nv; i++) {
#ifndef __linux__
			D3DVALUE x = vtx[i].x, y = vtx[i].y;
#else // __linux__
			float x = vtx[i].x, y = vtx[i].y;
#endif // __linux__
			vtx[i].x = cosa*x - sina*y;
			vtx[i].y = sina*x + cosa*y;
#ifndef __linux__
			D3DVALUE nx = vtx[i].nx, ny = vtx[i].ny;
#else // __linux__
			float nx = vtx[i].nx, ny = vtx[i].ny;
#endif // __linux__
			vtx[i].nx = cosa*nx - sina*ny;
			vtx[i].ny = sina*nx + cosa*ny;
		}
		if (GrpSetup) {
#ifndef __linux__
			D3DVALUE x = GrpCnt[grp].x, y = GrpCnt[grp].y;
#else // __linux__
			float x = GrpCnt[grp].x, y = GrpCnt[grp].y;
#endif // __linux__
			GrpCnt[grp].x = cosa*x - sina*y;
			GrpCnt[grp].y = sina*x + cosa*y;
		}
		break;
	}
}

#ifndef __linux__
void Mesh::Rotate (RotAxis axis, D3DVALUE angle)
#else // __linux__
void Mesh::Rotate (RotAxis axis, float angle)
#endif // __linux__
{
	for (DWORD grp = 0; grp < nGrp; grp++)
		RotateGroup (grp, axis, angle);
}

#ifndef __linux__
void Mesh::TransformGroup (DWORD grp, const D3DMATRIX &mat)
#else // __linux__
void Mesh::TransformGroup (DWORD grp, const oapi::FMATRIX4 &mat)
#endif // __linux__
{
	int i, nv = Grp[grp].nVtx;
	NTVERTEX *vtx = Grp[grp].Vtx;
	FLOAT x, y, z, w;

	for (i = 0; i < nv; i++) {
		NTVERTEX &v = vtx[i];
#ifndef __linux__
		x = v.x*mat._11 + v.y*mat._21 + v.z* mat._31 + mat._41;
		y = v.x*mat._12 + v.y*mat._22 + v.z* mat._32 + mat._42;
		z = v.x*mat._13 + v.y*mat._23 + v.z* mat._33 + mat._43;
		w = v.x*mat._14 + v.y*mat._24 + v.z* mat._34 + mat._44;
#else // __linux__
		x = v.x*mat.m11 + v.y*mat.m21 + v.z* mat.m31 + mat.m41;
		y = v.x*mat.m12 + v.y*mat.m22 + v.z* mat.m32 + mat.m42;
		z = v.x*mat.m13 + v.y*mat.m23 + v.z* mat.m33 + mat.m43;
		w = v.x*mat.m14 + v.y*mat.m24 + v.z* mat.m34 + mat.m44;
#endif // __linux__
    	v.x = x/w;
		v.y = y/w;
		v.z = z/w;

#ifndef __linux__
		x = v.nx*mat._11 + v.ny*mat._21 + v.nz* mat._31;
		y = v.nx*mat._12 + v.ny*mat._22 + v.nz* mat._32;
		z = v.nx*mat._13 + v.ny*mat._23 + v.nz* mat._33;
#else // __linux__
		x = v.nx*mat.m11 + v.ny*mat.m21 + v.nz* mat.m31;
		y = v.nx*mat.m12 + v.ny*mat.m22 + v.nz* mat.m32;
		z = v.nx*mat.m13 + v.ny*mat.m23 + v.nz* mat.m33;
#endif // __linux__
		w = 1.0f/(FLOAT)sqrt (x*x + y*y + z*z);
		v.nx = x*w;
		v.ny = y*w;
		v.nz = z*w;
	}
	if (GrpSetup) SetupGroup (grp);
}

#ifndef __linux__
void Mesh::Transform (const D3DMATRIX &mat)
#else // __linux__
void Mesh::Transform (const oapi::FMATRIX4 &mat)
#endif // __linux__
{
	for (DWORD grp = 0; grp < nGrp; grp++)
		TransformGroup (grp, mat);
}

#ifndef __linux__
void Mesh::TexScaleGroup (DWORD grp, D3DVALUE su, D3DVALUE sv)
#else // __linux__
void Mesh::TexScaleGroup (DWORD grp, float su, float sv)
#endif // __linux__
{
	int i, nv = Grp[grp].nVtx;
	NTVERTEX *vtx = Grp[grp].Vtx;
	for (i = 0; i < nv; i++) {
		vtx[i].tu *= su;
		vtx[i].tv *= sv;
	}
}

#ifndef __linux__
void Mesh::TexScale (D3DVALUE su, D3DVALUE sv)
#else // __linux__
void Mesh::TexScale (float su, float sv)
#endif // __linux__
{
	for (DWORD grp = 0; grp < nGrp; grp++)
		TexScaleGroup (grp, su, sv);
}

void Mesh::CalcNormals (DWORD grp, bool missingonly)
{
	const float eps = 1e-8f;
	int i, nv = Grp[grp].nVtx, nt = Grp[grp].nIdx/3;
	WORD *idx = Grp[grp].Idx;
	NTVERTEX *vtx = Grp[grp].Vtx;
	bool *calcNml = new bool[nv];
	if (missingonly) {
		for (i = 0; i < nv; i++) {
			if (vtx[i].nx*vtx[i].nx + vtx[i].ny*vtx[i].ny + vtx[i].nz*vtx[i].nz > 0.1f) {
				calcNml[i] = false; // flag for "leave normal alone"
			} else {
				calcNml[i] = true;
				vtx[i].nx = vtx[i].ny = vtx[i].nz = 0.0f;
			}
		}
	} else {
		for (i = 0; i < nv; i++) {
			calcNml[i] = true;
			vtx[i].nx = vtx[i].ny = vtx[i].nz = 0.0f;
		}
	}
	for (i = 0; i < nt; i++) {
		DWORD i0 = idx[i*3], i1 = idx[i*3+1], i2 = idx[i*3+2];
		if (!calcNml[i0] && !calcNml[i1] && !calcNml[i2])
			continue; // nothing to do for this triangle
#ifndef __linux__
		D3DVECTOR V01 = { vtx[i1].x - vtx[i0].x, vtx[i1].y - vtx[i0].y, vtx[i1].z - vtx[i0].z };
		D3DVECTOR V02 = { vtx[i2].x - vtx[i0].x, vtx[i2].y - vtx[i0].y, vtx[i2].z - vtx[i0].z };
		D3DVECTOR V12 = { vtx[i2].x - vtx[i1].x, vtx[i2].y - vtx[i1].y, vtx[i2].z - vtx[i1].z };
		D3DVECTOR nm = D3DMath_CrossProduct (V01, V02);
		D3DVALUE len = D3DMath_Length (nm);
#else // __linux__
		oapi::FVECTOR3 V01 = { vtx[i1].x - vtx[i0].x, vtx[i1].y - vtx[i0].y, vtx[i1].z - vtx[i0].z };
		oapi::FVECTOR3 V02 = { vtx[i2].x - vtx[i0].x, vtx[i2].y - vtx[i0].y, vtx[i2].z - vtx[i0].z };
		oapi::FVECTOR3 V12 = { vtx[i2].x - vtx[i1].x, vtx[i2].y - vtx[i1].y, vtx[i2].z - vtx[i1].z };
		oapi::FVECTOR3 nm = D3DMath_CrossProduct (V01, V02);
		float len = D3DMath_Length (nm);
#endif // __linux__

		if (len >= eps) {
			nm.x /= len, nm.y /= len, nm.z /= len;
#ifndef __linux__
			D3DVALUE d01 = D3DMath_Length(V01);
			D3DVALUE d02 = D3DMath_Length(V02);
			D3DVALUE d12 = D3DMath_Length(V12);
#else // __linux__
			float d01 = D3DMath_Length(V01);
			float d02 = D3DMath_Length(V02);
			float d12 = D3DMath_Length(V12);
#endif // __linux__
			if (calcNml[i0]) {
#ifndef __linux__
				D3DVALUE a0 = acos((d01 * d01 + d02 * d02 - d12 * d12) / (2.0f * d01 * d02));
#else // __linux__
				float a0 = acos((d01 * d01 + d02 * d02 - d12 * d12) / (2.0f * d01 * d02));
#endif // __linux__
				vtx[i0].nx += nm.x * a0, vtx[i0].ny += nm.y * a0, vtx[i0].nz += nm.z * a0;
			}
			if (calcNml[i1]) {
#ifndef __linux__
				D3DVALUE a1 = acos((d01 * d01 + d12 * d12 - d02 * d02) / (2.0f * d01 * d12));
#else // __linux__
				float a1 = acos((d01 * d01 + d12 * d12 - d02 * d02) / (2.0f * d01 * d12));
#endif // __linux__
				vtx[i1].nx += nm.x * a1, vtx[i1].ny += nm.y * a1, vtx[i1].nz += nm.z * a1;
			}
			if (calcNml[i2]) {
#ifndef __linux__
				D3DVALUE a2 = acos((d02 * d02 + d12 * d12 - d01 * d01) / (2.0f * d02 * d12));
#else // __linux__
				float a2 = acos((d02 * d02 + d12 * d12 - d01 * d01) / (2.0f * d02 * d12));
#endif // __linux__
				vtx[i2].nx += nm.x * a2, vtx[i2].ny += nm.y * a2, vtx[i2].nz += nm.z * a2;
			}
		}
	}
	for (i = 0; i < nv; i++)
		if (calcNml[i]) {
#ifndef __linux__
			D3DVECTOR nm = { vtx[i].nx, vtx[i].ny, vtx[i].nz };
			D3DVALUE len = D3DMath_Length(nm);
#else // __linux__
			oapi::FVECTOR3 nm = { vtx[i].nx, vtx[i].ny, vtx[i].nz };
			float len = D3DMath_Length(nm);
#endif // __linux__
			vtx[i].nx /= len, vtx[i].ny /= len, vtx[i].nz /= len;
		}
	delete []calcNml;
	calcNml = NULL;
}

void Mesh::CalcTexCoords (DWORD grp)
{
	// quick hack. not globally usable

	int i, nv = Grp[grp].nVtx;
	NTVERTEX *vtx = Grp[grp].Vtx;
	double ipi = 1.0/Pi, i2pi = 0.5/Pi;

	for (i = 0; i < nv; i++) {
#ifndef __linux__
		D3DVECTOR pos = {vtx[i].x, vtx[i].y, vtx[i].z};
#else // __linux__
		oapi::FVECTOR3 pos = {vtx[i].x, vtx[i].y, vtx[i].z};
#endif // __linux__
		D3DMath_Normalise (pos);
		double tht = acos (pos.y);
		double phi = atan2 (pos.z, pos.x);
#ifndef __linux__
		vtx[i].tu = (D3DVALUE)(phi >= 0.0 ? phi*i2pi : (phi+Pi2)*i2pi);
		vtx[i].tv = (D3DVALUE)(tht*ipi);
#else // __linux__
		vtx[i].tu = (float)(phi >= 0.0 ? phi*i2pi : (phi+Pi2)*i2pi);
		vtx[i].tv = (float)(tht*ipi);
#endif // __linux__
	}
}

int Mesh::AddTexture (SURFHANDLE tex)
{
	DWORD i;

	// check if already present
	for (i = 0; i < nTex; i++)
		if (Tex[i] == tex) return i;

	SURFHANDLE *tmp = new SURFHANDLE[nTex+1]; TRACENEW
	if (nTex) {
		memcpy (tmp, Tex, nTex*sizeof(SURFHANDLE));
		delete []Tex;
		Tex = NULL;
	}
	Tex = tmp;
	Tex[nTex] = tex;
	return nTex++;
}

bool Mesh::SetTexture (DWORD texidx, SURFHANDLE tex, bool release_old)
{
	if (texidx >= nTex) return false;  // index out of range
	if (Tex[texidx] && release_old) {
		if (g_pOrbiter->GetGraphicsClient())
			g_pOrbiter->GetGraphicsClient()->clbkReleaseTexture (Tex[texidx]);
	}
	Tex[texidx] = tex;
	return true;
}

void Mesh::ReleaseTextures ()
{
	if (nTex) {
		for (DWORD i = 0; i < nTex; i++)
			if (Tex[i]) {
				if (g_pOrbiter->GetGraphicsClient())
					g_pOrbiter->GetGraphicsClient()->clbkReleaseTexture (Tex[i]);
			}
		delete []Tex;
		Tex = NULL;
		nTex = 0;
	}
}

void Mesh::SetTexMixture (DWORD grp, DWORD ntex, float mix)
{
	if (grp < nGrp && --ntex < MAXTEX && Grp[grp].TexIdxEx[ntex] != SPEC_DEFAULT)
		Grp[grp].TexMixEx[ntex] = mix;
}

void Mesh::SetTexMixture (DWORD ntex, float mix)
{
	if (--ntex < MAXTEX) {
		for (DWORD grp = 0; grp < nGrp; grp++)
			if (Grp[grp].TexIdxEx[ntex] != SPEC_DEFAULT)
				Grp[grp].TexMixEx[ntex] = mix;
	}
}

void Mesh::GlobalEnableSpecular (bool enable)
{
	bEnableSpecular = enable;
}

void Mesh::EnableMatAlpha (bool enable)
{
	bModulateMatAlpha = enable;
}

const char* Mesh::GetName() const
{
	return name;
}

void Mesh::SetName(const char* n)
{
	if (n) {
#ifndef __linux__
		int len = lstrlen(n) + 1;
#else // __linux__
		int len = strlen(n) + 1;
#endif // __linux__
		name = new char[len];
#ifndef __linux__
		strcpy_s(name, len, n);
#else // __linux__
		strcpy(name, n);
#endif // __linux__
	}
}

#ifndef __linux__
DWORD Mesh::Render (LPDIRECT3DDEVICE7 dev)
{
	return 0;
}

void Mesh::RenderGroup (LPDIRECT3DDEVICE7 dev, DWORD grp, bool setstate) const
{
}
#else // __linux__
// Render/RenderGroup left out: empty Direct3D 7 inline render stubs
#endif // __linux__

istream &operator>> (istream &is, Mesh &mesh)
{
	char cbuf[256];
	int i, j, g, ngrp, nvtx, ntri, nidx, nmtrl, mtrl_idx, ntex, tex_idx, flag, res;
	DWORD uflag;
	WORD zbias;
#ifndef __linux__
	D3DMATERIAL7 mtrl;
#else // __linux__
	MATERIAL mtrl;
#endif // __linux__
	bool term, staticmesh = false;

	mesh.Clear();

	if (!is.getline (cbuf, 256)) return is;
#ifndef __linux__
	if (strcmp (cbuf, "MSHX1")) return is;
#else // __linux__
	if (strcmp (cbuf, "MSHX1") && strcmp (cbuf, "MSHX1\r")) return is; // not upstream: CRLF files keep the '\r' on Linux
#endif // __linux__

	for (;;) {
		if (!is.getline (cbuf, 256)) return is;
#ifndef __linux__
		if (!_strnicmp (cbuf, "GROUPS", 6)) {
#else // __linux__
		if (!strncasecmp (cbuf, "GROUPS", 6)) {
#endif // __linux__
			if (sscanf (cbuf+6, "%d", &ngrp) != 1) return is;
			break;
#ifndef __linux__
		} else if (!_strnicmp (cbuf, "STATICMESH", 10)) {
#else // __linux__
		} else if (!strncasecmp (cbuf, "STATICMESH", 10)) {
#endif // __linux__
			staticmesh = true;
		}
	}

	for (g = 0, term = false; g < ngrp && !term; g++) {

		// set defaults
		NTVERTEX *vtx;
		WORD *idx;
		mtrl_idx = SPEC_INHERIT;
		tex_idx  = SPEC_INHERIT;
		zbias    = 0;
		flag     = (staticmesh ? 0x04 : 0);
		uflag    = 0;
		bool bnormal = true, calcnml = false;
		bool flipidx = false;
		nvtx = ntri = 0;

		for (;;) {
			if (!is.getline (cbuf, 256)) { term = true; break; }
#ifndef __linux__
			if (!_strnicmp (cbuf, "MATERIAL", 8)) {       // read material index
#else // __linux__
			if (!strncasecmp (cbuf, "MATERIAL", 8)) {       // read material index
#endif // __linux__
				sscanf (cbuf+8, "%d", &mtrl_idx);
				mtrl_idx--;
#ifndef __linux__
			} else if (!_strnicmp (cbuf, "TEXTURE", 7)) { // read texture index
#else // __linux__
			} else if (!strncasecmp (cbuf, "TEXTURE", 7)) { // read texture index
#endif // __linux__
				sscanf (cbuf+7, "%d", &tex_idx);
				tex_idx--;
#ifndef __linux__
			} else if (!_strnicmp (cbuf, "ZBIAS", 5)) {   // read z-bias
#else // __linux__
			} else if (!strncasecmp (cbuf, "ZBIAS", 5)) {   // read z-bias
#endif // __linux__
				sscanf (cbuf+5, "%hu", &zbias);
#ifndef __linux__
			} else if (!_strnicmp (cbuf, "TEXWRAP", 7)) { // read wrap flags
#else // __linux__
			} else if (!strncasecmp (cbuf, "TEXWRAP", 7)) { // read wrap flags
#endif // __linux__
				char uvstr[10] = "";
				sscanf (cbuf+7, "%9s", uvstr);
				if (uvstr[0] == 'U' || uvstr[1] == 'U') flag |= 0x01;
				if (uvstr[0] == 'V' || uvstr[1] == 'V') flag |= 0x02;
#ifndef __linux__
			} else if (!_strnicmp (cbuf, "NONORMAL", 8)) {
#else // __linux__
			} else if (!strncasecmp (cbuf, "NONORMAL", 8)) {
#endif // __linux__
				bnormal = false; calcnml = true;
#ifndef __linux__
			} else if (!_strnicmp (cbuf, "FLAG", 4)) {
				sscanf (cbuf+4, "%lx", &uflag);
			} else if (!_strnicmp (cbuf, "FLIP", 4)) {
#else // __linux__
			} else if (!strncasecmp (cbuf, "FLAG", 4)) {
				sscanf (cbuf+4, "%x", &uflag);
			} else if (!strncasecmp (cbuf, "FLIP", 4)) {
#endif // __linux__
				flipidx = true;
#ifndef __linux__
			} else if (!_strnicmp (cbuf, "LABEL", 5)) {
#else // __linux__
			} else if (!strncasecmp (cbuf, "LABEL", 5)) {
#endif // __linux__
				// ignore group labels here
#ifndef __linux__
			} else if (!_strnicmp (cbuf, "STATIC", 6)) {
#else // __linux__
			} else if (!strncasecmp (cbuf, "STATIC", 6)) {
#endif // __linux__
				flag |= 0x04;
#ifndef __linux__
			} else if (!_strnicmp (cbuf, "DYNAMIC", 7)) {
#else // __linux__
			} else if (!strncasecmp (cbuf, "DYNAMIC", 7)) {
#endif // __linux__
				flag ^= 0x04;
#ifndef __linux__
			} else if (!_strnicmp (cbuf, "GEOM", 4)) {    // read geometry
#else // __linux__
			} else if (!strncasecmp (cbuf, "GEOM", 4)) {    // read geometry
#endif // __linux__
				if (sscanf (cbuf+4, "%d%d", &nvtx, &ntri) != 2) break; // parse error - skip group
				nidx = ntri*3;
				vtx = new NTVERTEX[nvtx]; TRACENEW
#ifndef __linux__
				ZeroMemory (vtx, sizeof (NTVERTEX)*nvtx);
#else // __linux__
				memset (vtx, 0, sizeof (NTVERTEX)*nvtx);
#endif // __linux__
				for (i = 0; i < nvtx; i++) {
					NTVERTEX &v = vtx[i];
					if (!is.getline (cbuf, 256)) {
						delete []vtx;
						vtx = NULL;
						nvtx = 0;
						break;
					}
					if (bnormal) {
						j = sscanf (cbuf, "%f%f%f%f%f%f%f%f",
							&v.x, &v.y, &v.z, &v.nx, &v.ny, &v.nz, &v.tu, &v.tv);
						if (j < 6) {
							calcnml = true;
							// sscanf wrote UV coords into normal fields - relocate them
							v.tu = v.nx;
							v.tv = v.ny;
							// Zero the normal so CalcNormals() will recalculate this vertex
							v.nx = 0.0f;
							v.ny = 0.0f;
							v.nz = 0.0f;
						}
					} else {
						j = sscanf (cbuf, "%f%f%f%f%f",
							&v.x, &v.y, &v.z, &v.tu, &v.tv);
					}
				}
				idx = new WORD[nidx]; TRACENEW
#ifndef __linux__
				ZeroMemory (idx, sizeof (WORD)*nidx);
#else // __linux__
				memset (idx, 0, sizeof (WORD)*nidx);
#endif // __linux__
				for (i = j = 0; i < ntri; i++) {
					if (!is.getline (cbuf, 256)) {
						delete []vtx;
						vtx = NULL;
						delete []idx;
						idx = NULL;
						nvtx = nidx = 0;
						break;
					}
					sscanf (cbuf, "%hd%hd%hd", idx+j, idx+j+1, idx+j+2);
					j += 3;
				}
				if (flipidx)
					for (i = 0; i < ntri; i++) {
						WORD tmp = idx[i*3+1]; idx[i*3+1] = idx[i*3+2]; idx[i*3+2] = tmp;
					}

				break;
			}
		}
		if (nvtx && nidx) {
			mesh.AddGroup (vtx, nvtx, idx, nidx, mtrl_idx, tex_idx, zbias);
			mesh.Grp[g].Flags = flag;
			mesh.Grp[g].UsrFlag = uflag;
			if (calcnml) mesh.CalcNormals (g, true);
#ifndef __linux__
			if (flag & 0x04) mesh.MakeGroupVertexBuffer (g);
#else // __linux__
			// flag 0x04 (MakeGroupVertexBuffer) left out: vertex buffers belong to the graphics client
#endif // __linux__
		}
	}

	// read material list
	if (is.getline (cbuf, 256) && !strncmp (cbuf, "MATERIALS", 9) && (sscanf (cbuf+9, "%d", &nmtrl) == 1)) {
		Str256 *matname = new Str256[nmtrl]; TRACENEW
		Str256 mnm;
		for (i = 0; i < nmtrl; i++) {
			is.getline (cbuf, 256);
			sscanf (cbuf, "%s", matname[i]);
		}
		for (i = 0; i < nmtrl; i++) {
#ifndef __linux__
			ZeroMemory (&mtrl, sizeof (D3DMATERIAL7));
#else // __linux__
			memset (&mtrl, 0, sizeof (MATERIAL));
#endif // __linux__
			is.getline (cbuf, 256);
			sscanf (cbuf+8, "%255s", mnm);
			is.getline (cbuf, 256);
			sscanf (cbuf, "%f%f%f%f", &mtrl.diffuse.r, &mtrl.diffuse.g, &mtrl.diffuse.b, &mtrl.diffuse.a);
			is.getline (cbuf, 256);
			sscanf (cbuf, "%f%f%f%f", &mtrl.ambient.r, &mtrl.ambient.g, &mtrl.ambient.b, &mtrl.ambient.a);
			is.getline (cbuf, 256);
			res = sscanf (cbuf, "%f%f%f%f%f", &mtrl.specular.r, &mtrl.specular.g, &mtrl.specular.b, &mtrl.specular.a, &mtrl.power);
			if (res < 5) mtrl.power = 0.0;
			is.getline (cbuf, 256);
			sscanf (cbuf, "%f%f%f%f", &mtrl.emissive.r, &mtrl.emissive.g, &mtrl.emissive.b, &mtrl.emissive.a);
			mesh.AddMaterial (mtrl);
		}
		delete []matname;
		matname = NULL;
	}

	// read texture list
	mesh.ReleaseTextures ();
	if (is.getline (cbuf, 256) && !strncmp (cbuf, "TEXTURES", 8) && (sscanf (cbuf+8, "%d", &ntex) == 1)) {
		mesh.Tex = new SURFHANDLE[mesh.nTex = ntex]; TRACENEW
		Str256 texname, flagstr;
		for (i = 0; i < ntex; i++) {
			is.getline (cbuf, 256);
			flagstr[0] = '\0';
			sscanf (cbuf, "%255s%255s", texname, flagstr);
			mesh.Tex[i] = 0;
			if (texname[0] != '0' || texname[1] != '\0') {
				bool uncompress = (toupper(flagstr[0]) == 'D');
				if (g_pOrbiter->GetGraphicsClient())
					mesh.Tex[i] = g_pOrbiter->GetGraphicsClient()->clbkLoadTexture (texname, 8 | (uncompress ? 2:0));
			}
		}
	}

	mesh.Setup();
	is.clear();
	return is;
}

ostream &operator<< (ostream &os, const Mesh &mesh)
{
	DWORD g, i, ntri;

	os << "MSHX1" << endl;
	os << "GROUPS " << mesh.nGrp << endl;
	for (g = 0; g < mesh.nGrp; g++) {
		if (mesh.Grp[g].MtrlIdx != SPEC_INHERIT)
			os << "MATERIAL "
			   << (mesh.Grp[g].MtrlIdx == SPEC_DEFAULT ? 0 : mesh.Grp[g].MtrlIdx+1)
			   << endl;
		if (mesh.Grp[g].TexIdx != SPEC_INHERIT)
			os << "TEXTURE "
			   << (mesh.Grp[g].TexIdx == SPEC_DEFAULT ? 0 : mesh.Grp[g].TexIdx)
			   << endl;
		if (mesh.Grp[g].zBias)
			os << "ZBIAS " << mesh.Grp[g].zBias << endl;
		if (mesh.Grp[g].Flags & 0x03) {
			os << "TEXWRAP ";
			if (mesh.Grp[g].Flags & 0x01) os << 'U';
			if (mesh.Grp[g].Flags & 0x02) os << 'V';
			os << endl;
		}
		if (mesh.Grp[g].UsrFlag)
			os << "FLAG " << hex << mesh.Grp[g].UsrFlag << dec << endl;
		ntri = mesh.Grp[g].nIdx/3;
		os << "GEOM " << mesh.Grp[g].nVtx << ' ' << ntri << endl;
		for (i = 0; i < mesh.Grp[g].nVtx; i++)
			os << mesh.Grp[g].Vtx[i].x << ' '
			   << mesh.Grp[g].Vtx[i].y << ' '
			   << mesh.Grp[g].Vtx[i].z << ' '
			   << mesh.Grp[g].Vtx[i].nx << ' '
			   << mesh.Grp[g].Vtx[i].ny << ' '
			   << mesh.Grp[g].Vtx[i].nz << ' '
			   << mesh.Grp[g].Vtx[i].tu << ' '
			   << mesh.Grp[g].Vtx[i].tv << endl;
		for (i = 0; i < ntri; i++)
			os << mesh.Grp[g].Idx[i*3] << ' '
			   << mesh.Grp[g].Idx[i*3+1] << ' '
			   << mesh.Grp[g].Idx[i*3+2] << endl;
	}
	if (mesh.nMtrl) {
		os << "MATERIALS " << mesh.nMtrl << endl;
		for (i = 0; i < mesh.nMtrl; i++)
			os << "Material" << i << endl; // note: original material names are not preserved
		for (i = 0; i < mesh.nMtrl; i++) {
			os << "MATERIAL Material" << i << endl;
			os << mesh.Mtrl[i].diffuse.r << ' '
			   << mesh.Mtrl[i].diffuse.g << ' '
			   << mesh.Mtrl[i].diffuse.b << ' '
			   << mesh.Mtrl[i].diffuse.a << endl;
			os << mesh.Mtrl[i].ambient.r << ' '
			   << mesh.Mtrl[i].ambient.g << ' '
			   << mesh.Mtrl[i].ambient.b << ' '
			   << mesh.Mtrl[i].ambient.a << endl;
			os << mesh.Mtrl[i].specular.r << ' '
			   << mesh.Mtrl[i].specular.g << ' '
			   << mesh.Mtrl[i].specular.b << ' '
			   << mesh.Mtrl[i].specular.a;
			if (mesh.Mtrl[i].power) os << ' ' << mesh.Mtrl[i].power;
			os << endl;
			os << mesh.Mtrl[i].emissive.r << ' '
			   << mesh.Mtrl[i].emissive.g << ' '
			   << mesh.Mtrl[i].emissive.b << ' '
			   << mesh.Mtrl[i].emissive.a << endl;
		}
	}
	if (mesh.nTex) {
		os << "TEXTURES " << mesh.nTex << endl;
		for (i = 0; i < mesh.nTex; i++)
			os << "0" << endl; // DUMMY - texture names are not preserved!
	}
	return os;
}

bool Mesh::bEnableSpecular = false;

// =======================================================================
// Class MeshManager

MeshManager::MeshManager()
{
	nmlist = nmlistbuf = 0;
}

MeshManager::~MeshManager()
{
	Flush();
}

void MeshManager::Flush()
{
	for (int i = 0; i < nmlist; i++)
		delete mlist[i].mesh;
	if (nmlistbuf) {
		delete []mlist;
		nmlist = nmlistbuf = 0;
	}
}

const Mesh *MeshManager::LoadMesh (const char *fname, bool *firstload)
{
	int i;
	DWORDLONG crc = Str2Crc (fname);
	for (i = 0; i < nmlist; i++) {
#ifndef __linux__
		if (crc == mlist[i].crc && !_strnicmp (fname, mlist[i].fname, 32)) {
#else // __linux__
		if (crc == mlist[i].crc && !strncasecmp (fname, mlist[i].fname, 32)) {
#endif // __linux__
			if (firstload) *firstload = false;
			return mlist[i].mesh; // found it
		}
	}
	// not found, so load from file
#ifndef __linux__
	ifstream ifs (g_pOrbiter->MeshPath (fname), ios::in);
#else // __linux__
	ifstream ifs (oapiResolvePath(g_pOrbiter->MeshPath (fname)), ios::in);
#endif // __linux__
	Mesh *mesh = new Mesh; TRACENEW
	ifs >> *mesh;
	if (!mesh->nGroup()) { // load error
		if (!fname[0]) LOGOUT_ERR ("Mesh file name not provided");
		else LOGOUT_ERR ("Mesh not found: %s", g_pOrbiter->MeshPath (fname));
		//g_pOrbiter->TerminateOnError ();
		delete mesh;
		return 0;
	}
	if (nmlist == nmlistbuf) { // need to allocate buffer
		MeshBuffer *tmp = new MeshBuffer[nmlistbuf += 32]; TRACENEW
		if (nmlist) {
			memcpy (tmp, mlist, nmlist*sizeof(MeshBuffer));
			delete []mlist;
			mlist = NULL;
		}
		mlist = tmp;
	}
	mesh->SetName(fname);
	mlist[nmlist].mesh = mesh;
	mlist[nmlist].crc  = crc;
	strncpy (mlist[nmlist].fname, fname, 32);
	nmlist++;
	if (firstload) *firstload = true;
	return mesh;
}

// =======================================================================
// Nonmember functions

bool LoadMesh (const char *meshname, Mesh &mesh)
{
#ifndef __linux__
	ifstream ifs (g_pOrbiter->MeshPath (meshname), ios::in);
#else // __linux__
	ifstream ifs (oapiResolvePath(g_pOrbiter->MeshPath (meshname)), ios::in);
#endif // __linux__
	ifs >> mesh;
	if (ifs.good()) {
		mesh.SetName(meshname);
		return true;
	} else {
		LOGOUT_ERR ("Mesh not found: %s", g_pOrbiter->MeshPath (meshname));
		//g_pOrbiter->TerminateOnError ();
		return false;
	}
}


// =======================================================================
// Create a sphere patch.
// nlng is the number of patches required to span the full 360° in longitude
// nlat is the number of patches required to span the latitude range from 0 to 90°
// 0 <= ilat < nlat is the actual latitude strip the patch is to cover
// res >= 1 is the resolution of the patch (= number of internal latitude strips in the patch)
// bseg, if given, is the number of of polygon segments on the lower base line of the patch.
// Default is (nlat-ilat)*res. bseg is ignored for triangular patches (i.e. where upper
// latitude is 90°)

void CreateSpherePatch (Mesh &mesh, int nlng, int nlat, int ilat, int res, int bseg, bool reduce, bool outside)
{
	const float c1 = 1.0f, c2 = 0.0f; // -1.0f/512.0f; // assumes 256x256 texture patches
	//const float c1 = 1.0f-1.0f/256.0f, c2 = 1.0f/512.f;
	int i, j, nVtx, nIdx, nseg, n, nofs0, nofs1;
	double minlat, maxlat, lat, minlng, maxlng, lng;
	double slat, clat, slng, clng;
	WORD tmp;
	
	minlat = Pi05 * (double)ilat/(double)nlat;
	maxlat = Pi05 * (double)(ilat+1)/(double)nlat;
	minlng = 0;
	maxlng = Pi2/(double)nlng;
	if (bseg < 0 || ilat == nlat-1) bseg = (nlat-ilat)*res;

	// generate nodes
	nVtx = (bseg+1)*(res+1);
	if (reduce) nVtx -= ((res+1)*res)/2;
	NTVERTEX *Vtx = new NTVERTEX[nVtx]; TRACENEW

	for (i = n = 0; i <= res; i++) {  // loop over longitudinal strips
		lat = minlat + (maxlat-minlat) * (double)i/(double)res;
		slat = sin(lat), clat = cos(lat);
		nseg = (reduce ? bseg-i : bseg);
		for (j = 0; j <= nseg; j++) {
			lng = (nseg ? minlng + (maxlng-minlng) * (double)j/(double)nseg : 0.0);
			slng = sin(lng), clng = cos(lng);
#ifndef __linux__
			Vtx[n].x = Vtx[n].nx = D3DVAL(clat*clng);
			Vtx[n].y = Vtx[n].ny = D3DVAL(slat);
			Vtx[n].z = Vtx[n].nz = D3DVAL(clat*slng);
			Vtx[n].tu = D3DVAL(nseg ? (c1*j)/nseg+c2 : 0.5); // overlap to avoid seams
			Vtx[n].tv = D3DVAL((c1*(res-i))/res+c2);
#else // __linux__
			Vtx[n].x = Vtx[n].nx = (float)(clat*clng);
			Vtx[n].y = Vtx[n].ny = (float)(slat);
			Vtx[n].z = Vtx[n].nz = (float)(clat*slng);
			Vtx[n].tu = (float)(nseg ? (c1*j)/nseg+c2 : 0.5); // overlap to avoid seams
			Vtx[n].tv = (float)((c1*(res-i))/res+c2);
#endif // __linux__

			if (!outside) {
				Vtx[n].nx = - Vtx[n].nx;
				Vtx[n].ny = - Vtx[n].ny;
				Vtx[n].nz = - Vtx[n].nz;
			}

			n++;
		}
	}

	// generate faces
	nIdx = (reduce ? res * (2*bseg-res) : 2*res*bseg) * 3;
	WORD *Idx = new WORD[nIdx]; TRACENEW

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

	mesh.Clear();
	mesh.AddGroup (Vtx, nVtx, Idx, nIdx, SPEC_INHERIT, SPEC_INHERIT);
	mesh.Setup ();
}
