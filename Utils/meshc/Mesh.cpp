// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#include "Mesh.h"
#include <stdio.h>
#include "D3dmath.h"

using namespace std;

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
	nGrp = nMtrl = nTex = 0;
	GrpVis   = 0;
	GrpSetup = false;
}

Mesh::Mesh (D3DVERTEX *vtx, DWORD nvtx, WORD *idx, DWORD nidx, DWORD matidx, DWORD texidx)
{
	nGrp = nMtrl = nTex = 0;
	GrpVis   = 0;
	GrpSetup = false;
	AddGroup (vtx, nvtx, idx, nidx, matidx, texidx);
	Setup();
}

Mesh::Mesh (const Mesh &mesh)
{
	nGrp = nMtrl = nTex = 0;
	GrpVis   = 0;
	GrpSetup = false;
	Set (mesh);
}

void Mesh::Set (const Mesh &mesh)
{
	DWORD i;

	Clear ();
	if (nGrp = mesh.nGrp) {
		Grp = new GroupSpec[nGrp];
		memcpy (Grp, mesh.Grp, nGrp*sizeof(GroupSpec));
		for (i = 0; i < nGrp; i++) {
			Grp[i].Vtx = new D3DVERTEX[Grp[i].nVtx];
			memcpy (Grp[i].Vtx, mesh.Grp[i].Vtx, Grp[i].nVtx*sizeof(D3DVERTEX));
			Grp[i].Idx = new WORD[Grp[i].nIdx];
			memcpy (Grp[i].Idx, mesh.Grp[i].Idx, Grp[i].nIdx*sizeof(WORD)); 
			if (mesh.Grp[i].Comment) {
				Grp[i].Comment = new char[strlen(mesh.Grp[i].Comment)+1];
				strcpy (Grp[i].Comment, mesh.Grp[i].Comment);
			} else
				Grp[i].Comment = 0;
		}
	}
	if (nMtrl = mesh.nMtrl) {
		Mtrl = new D3DMATERIAL7[nMtrl];
		memcpy (Mtrl, mesh.Mtrl, nMtrl*sizeof(D3DMATERIAL7));
	}
	if (nTex = mesh.nTex) {
		Tex = new LPDIRECTDRAWSURFACE7[nTex];
		memcpy (Tex, mesh.Tex, nTex*sizeof(LPDIRECTDRAWSURFACE7));
		// somehow need to register with g_texmanager
	}
	if (GrpSetup = mesh.GrpSetup) {
		GrpCnt = new D3DVECTOR[nGrp];
		memcpy (GrpCnt, mesh.GrpCnt, nGrp*sizeof(D3DVECTOR));
		GrpRad = new D3DVALUE[nGrp];
		memcpy (GrpRad, mesh.GrpRad, nGrp*sizeof(D3DVALUE));
		GrpVis = new DWORD[nGrp];
		memcpy (GrpVis, mesh.GrpVis, nGrp*sizeof(DWORD));
	} else {
		GrpVis = 0;
	}
}

Mesh::~Mesh ()
{
	Clear ();
}

void Mesh::Setup ()
{
	DWORD g, i;
	D3DVALUE x, y, z, dx, dy, dz, d2, d2max;

	if (GrpVis) { // allocated already
		delete []GrpCnt;
		delete []GrpRad;
		delete []GrpVis;
	}
	GrpCnt  = new D3DVECTOR[nGrp];
	GrpRad  = new D3DVALUE[nGrp];
	GrpVis  = new DWORD[nGrp];
	for (g = 0; g < nGrp; g++) {
		D3DVALUE invtx = (D3DVALUE)(1.0/Grp[g].nVtx);
		x = y = z = 0.0f;
		for (i = 0; i < Grp[g].nVtx; i++) {
			x += Grp[g].Vtx[i].x;
			y += Grp[g].Vtx[i].y;
			z += Grp[g].Vtx[i].z;
		}
		GrpCnt[g].x = (x *= invtx);
		GrpCnt[g].y = (y *= invtx);
		GrpCnt[g].z = (z *= invtx);
		d2max = 0.0f;
		for (i = 0; i < Grp[g].nVtx; i++) {
			dx = x - Grp[g].Vtx[i].x;
			dy = y - Grp[g].Vtx[i].y;
			dz = z - Grp[g].Vtx[i].z;
			d2 = dx*dx + dy*dy + dz*dz;
			if (d2 > d2max) d2max = d2;
		}
		GrpRad[g] = (FLOAT)sqrt (d2max);

		// check validity of material/texture indices
		if (Grp[g].MtrlIdx != SPEC_INHERIT && Grp[g].MtrlIdx >= nMtrl) Grp[g].MtrlIdx = SPEC_DEFAULT;
		if (Grp[g].TexIdx  != SPEC_INHERIT && Grp[g].TexIdx >= nTex)   Grp[g].TexIdx  = SPEC_DEFAULT;
	}
	GrpSetup = true;
}

bool Mesh::GetGroup (DWORD grp, D3DVERTEX *&vtx, DWORD &nvtx, WORD *&idx, DWORD &nidx)
{
	if (grp < nGrp) {
		vtx = Grp[grp].Vtx;  nvtx = Grp[grp].nVtx;
		idx = Grp[grp].Idx;  nidx = Grp[grp].nIdx;
		return true;
	} else {
		return false;
	}
}

int Mesh::AddGroup (D3DVERTEX *vtx, DWORD nvtx, WORD *idx, DWORD nidx,
	DWORD mtrl_idx, DWORD tex_idx, WORD zbias)
{
	GroupSpec *tmp_Grp = new GroupSpec[nGrp+1];
	memcpy (tmp_Grp, Grp, sizeof(GroupSpec)*nGrp);
	if (nGrp) delete []Grp;
	Grp = tmp_Grp;
	Grp[nGrp].Vtx = vtx;  Grp[nGrp].nVtx = nvtx;
	Grp[nGrp].Idx = idx;  Grp[nGrp].nIdx = nidx;
	Grp[nGrp].MtrlIdx = mtrl_idx;
	Grp[nGrp].TexIdx = tex_idx;
	Grp[nGrp].zBias = zbias;
	Grp[nGrp].Flags = 0;
	Grp[nGrp].Comment = 0;
	GrpSetup = false;
	return nGrp++;
}

bool Mesh::DeleteGroup (DWORD grp)
{
	if (grp < nGrp) {
		delete []Grp[grp].Vtx;
		delete []Grp[grp].Idx;
		if (Grp[grp].Comment) delete []Grp[grp].Comment;
		if (nGrp == 1) { // delete the only group
			delete []Grp;
		} else {
			GroupSpec *tmp_Grp = new GroupSpec[nGrp-1];
			for (DWORD i = 0, j = 0; i < nGrp; i++)
				if (i != grp) tmp_Grp[j++] = Grp[i];
			delete []Grp;
			Grp = tmp_Grp;
		}
		nGrp--;
		return true;
	} else {
		return false;
	}
}

void Mesh::AddMesh (Mesh &mesh)
{
	for (int i = 0; i < mesh.nGroup(); i++) {
		D3DVERTEX *vtx, *vtx2;
		WORD *idx, *idx2;
		DWORD nvtx, nidx;
		mesh.GetGroup (i, vtx, nvtx, idx, nidx);
		// need to copy the vertex and index lists
		vtx2 = new D3DVERTEX[nvtx];  memcpy (vtx2, vtx, nvtx*sizeof(D3DVERTEX));
		idx2 = new WORD[nidx];       memcpy (idx2, idx, nidx*sizeof(WORD));
		AddGroup (vtx2, nvtx, idx2, nidx);
	}
}

int Mesh::AddMaterial (D3DMATERIAL7 &mtrl)
{
	D3DMATERIAL7 *tmp_Mtrl = new D3DMATERIAL7[nMtrl+1];
	memcpy (tmp_Mtrl, Mtrl, sizeof(D3DMATERIAL7)*nMtrl);
	memcpy (tmp_Mtrl+nMtrl, &mtrl, sizeof(D3DMATERIAL7));
	if (nMtrl) delete []Mtrl;
	Mtrl = tmp_Mtrl;
	return nMtrl++;
}

void Mesh::Clear ()
{
	for (DWORD i = 0; i < nGrp; i++) {
		delete []Grp[i].Vtx;
		delete []Grp[i].Idx;
		if (Grp[i].Comment) delete []Grp[i].Comment;
	}
	if (nGrp) {
		delete []Grp;
		nGrp = 0;
	}
	if (nMtrl) {
		delete []Mtrl;
		nMtrl = 0;
	}
	if (GrpVis) {
		delete []GrpCnt;
		delete []GrpRad;
		delete []GrpVis;
		GrpVis = 0;
	}
	GrpSetup = false;
	ReleaseTextures ();
}

void Mesh::ScaleGroup (DWORD grp, D3DVALUE sx, D3DVALUE sy, D3DVALUE sz)
{
	int i, nv = Grp[grp].nVtx;
	D3DVERTEX *vtx = Grp[grp].Vtx;
	for (i = 0; i < nv; i++) {
		vtx[i].x *= sx;
		vtx[i].y *= sy;
		vtx[i].z *= sz;
	}
	if (sx == sy && sx == sz) return; // no change in normals
	D3DVALUE snx = sy*sz, sny = sx*sz, snz = sx*sy;
	for (i = 0; i < nv; i++) {
		vtx[i].nx *= snx;
		vtx[i].ny *= sny;
		vtx[i].nz *= snz;
		D3DVALUE ilen = (D3DVALUE)(1.0/sqrt (vtx[i].nx*vtx[i].nx + vtx[i].ny*vtx[i].ny + vtx[i].nz*vtx[i].nz));
		vtx[i].nx *= ilen;
		vtx[i].ny *= ilen;
		vtx[i].nz *= ilen;
	}
	GrpSetup = false;
}

void Mesh::Scale (D3DVALUE sx, D3DVALUE sy, D3DVALUE sz)
{
	for (DWORD grp = 0; grp < nGrp; grp++)
		ScaleGroup (grp, sx, sy, sz);
}

void Mesh::TranslateGroup (DWORD grp, D3DVALUE dx, D3DVALUE dy, D3DVALUE dz)
{
	int i, nv = Grp[grp].nVtx;
	D3DVERTEX *vtx = Grp[grp].Vtx;
	for (i = 0; i < nv; i++) {
		vtx[i].x += dx;
		vtx[i].y += dy;
		vtx[i].z += dz;
	}
	GrpSetup = false;
}

void Mesh::Translate (D3DVALUE dx, D3DVALUE dy, D3DVALUE dz)
{
	for (DWORD grp = 0; grp < nGrp; grp++)
		TranslateGroup (grp, dx, dy, dz);
}

void Mesh::RotateGroup (DWORD grp, RotAxis axis, D3DVALUE angle)
{
	int i, nv = Grp[grp].nVtx;
	D3DVERTEX *vtx = Grp[grp].Vtx;
	D3DVALUE cosa = (D3DVALUE)cos(angle), sina = (D3DVALUE)sin(angle);
	switch (axis) {
	case ROTATE_X:
		for (i = 0; i < nv; i++) {
			D3DVALUE y = vtx[i].y, z = vtx[i].z;
			vtx[i].y = cosa*y - sina*z;
			vtx[i].z = sina*y + cosa*z;
			D3DVALUE ny = vtx[i].ny, nz = vtx[i].nz;
			vtx[i].ny = cosa*ny - sina*nz;
			vtx[i].nz = sina*ny + cosa*nz;
		}
		break;
	case ROTATE_Y:
		for (i = 0; i < nv; i++) {
			D3DVALUE x = vtx[i].x, z = vtx[i].z;
			vtx[i].x = cosa*x - sina*z;
			vtx[i].z = sina*x + cosa*z;
			D3DVALUE nx = vtx[i].nx, nz = vtx[i].nz;
			vtx[i].nx = cosa*nx - sina*nz;
			vtx[i].nz = sina*nx + cosa*nz;
		}
		break;
	case ROTATE_Z:
		for (i = 0; i < nv; i++) {
			D3DVALUE x = vtx[i].x, y = vtx[i].y;
			vtx[i].x = cosa*x - sina*y;
			vtx[i].y = sina*x + cosa*y;
			D3DVALUE nx = vtx[i].nx, ny = vtx[i].ny;
			vtx[i].nx = cosa*nx - sina*ny;
			vtx[i].ny = sina*nx + cosa*ny;
		}
		break;
	}
	GrpSetup = false;
}

void Mesh::Rotate (RotAxis axis, D3DVALUE angle)
{
	for (DWORD grp = 0; grp < nGrp; grp++)
		RotateGroup (grp, axis, angle);
}

void Mesh::MirrorGroup (DWORD grp, MirrorDir dir)
{
	DWORD i, nv = Grp[grp].nVtx;
	DWORD ni = Grp[grp].nIdx;
	WORD *idx = Grp[grp].Idx;
	D3DVERTEX *vtx = Grp[grp].Vtx;

	switch (dir) {
	case MIRROR_X:
		for (i = 0; i < nv; i++) {
			vtx[i].x = - vtx[i].x;
			vtx[i].nx = -vtx[i].nx;
		}
		break;
	case MIRROR_Y:
		for (i = 0; i < nv; i++) {
			vtx[i].y = -vtx[i].y;
			vtx[i].ny = -vtx[i].ny;
		}
		break;
	case MIRROR_Z:
		for (i = 0; i < nv; i++) {
			vtx[i].z = -vtx[i].z;
			vtx[i].nz = -vtx[i].nz;
		}
		break;
	}
	for (i = 0; i < ni/3; i++) {
		WORD tmp = idx[i*3+1];
		idx[i*3+1] = idx[i*3+2];
		idx[i*3+2] = tmp;
	}
	GrpSetup = false;
}

void Mesh::Mirror (MirrorDir dir)
{
	for (DWORD grp = 0; grp < nGrp; grp++)
		MirrorGroup (grp, dir);
}

void Mesh::TexScaleGroup (DWORD grp, D3DVALUE su, D3DVALUE sv)
{
	int i, nv = Grp[grp].nVtx;
	D3DVERTEX *vtx = Grp[grp].Vtx;
	for (i = 0; i < nv; i++) {
		vtx[i].tu *= su;
		vtx[i].tv *= sv;
	}
}

void Mesh::TexScale (D3DVALUE su, D3DVALUE sv)
{
	for (DWORD grp = 0; grp < nGrp; grp++)
		TexScaleGroup (grp, su, sv);
}

void Mesh::CalcNormals (DWORD grp, bool missingonly)
{
	const float eps = 1e-8f;
	int i, nv = Grp[grp].nVtx, nt = Grp[grp].nIdx/3;
	WORD *idx = Grp[grp].Idx;
	D3DVERTEX *vtx = Grp[grp].Vtx;
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
		D3DVECTOR V01 = { vtx[i1].x - vtx[i0].x, vtx[i1].y - vtx[i0].y, vtx[i1].z - vtx[i0].z };
		D3DVECTOR V02 = { vtx[i2].x - vtx[i0].x, vtx[i2].y - vtx[i0].y, vtx[i2].z - vtx[i0].z };
		D3DVECTOR V12 = { vtx[i2].x - vtx[i1].x, vtx[i2].y - vtx[i1].y, vtx[i2].z - vtx[i1].z };
		D3DVECTOR nm = D3DMath_CrossProduct (V01, V02);
		D3DVALUE len = D3DMath_Length (nm);

		if (len >= eps) {
			nm.x /= len, nm.y /= len, nm.z /= len;
			D3DVALUE d01 = D3DMath_Length(V01);
			D3DVALUE d02 = D3DMath_Length(V02);
			D3DVALUE d12 = D3DMath_Length(V12);
			if (calcNml[i0]) {
				D3DVALUE a0 = acos((d01 * d01 + d02 * d02 - d12 * d12) / (2.0f * d01 * d02));
				vtx[i0].nx += nm.x * a0, vtx[i0].ny += nm.y * a0, vtx[i0].nz += nm.z * a0;
			}
			if (calcNml[i1]) {
				D3DVALUE a1 = acos((d01 * d01 + d12 * d12 - d02 * d02) / (2.0f * d01 * d12));
				vtx[i1].nx += nm.x * a1, vtx[i1].ny += nm.y * a1, vtx[i1].nz += nm.z * a1;
			}
			if (calcNml[i2]) {
				D3DVALUE a2 = acos((d02 * d02 + d12 * d12 - d01 * d01) / (2.0f * d02 * d12));
				vtx[i2].nx += nm.x * a2, vtx[i2].ny += nm.y * a2, vtx[i2].nz += nm.z * a2;
			}
		}
	}
	for (i = 0; i < nv; i++)
		if (calcNml[i]) {
			D3DVECTOR nm = { vtx[i].nx, vtx[i].ny, vtx[i].nz };
			D3DVALUE len = D3DMath_Length(nm);
			vtx[i].nx /= len, vtx[i].ny /= len, vtx[i].nz /= len;
		}
	delete []calcNml;
}

void Mesh::CalcTexCoords (DWORD grp)
{
	// quick hack. not globally usable

	int i, nv = Grp[grp].nVtx;
	D3DVERTEX *vtx = Grp[grp].Vtx;
	double ipi = 1.0/Pi, i2pi = 0.5/Pi;

	for (i = 0; i < nv; i++) {
		D3DVECTOR pos = {vtx[i].x, vtx[i].y, vtx[i].z};
		D3DMath_Normalise (pos);
		double tht = acos (pos.y);
		double phi = atan2 (pos.z, pos.x);
		vtx[i].tu = (D3DVALUE)(phi >= 0.0 ? phi*i2pi : (phi+Pi2)*i2pi);
		vtx[i].tv = (D3DVALUE)(tht*ipi);
	}
}

void Mesh::ZeroThreshold (float eps, int which)
{
	DWORD grp, i;
	for (grp = 0; grp < nGrp; grp++) {
		GroupSpec &G = Grp[grp];
		for (i = 0; i < G.nVtx; i++) {
			D3DVERTEX &vtx = G.Vtx[i];
			if (which & 1) {
				if (fabs (vtx.x) < eps) vtx.x = 0.0f;
				if (fabs (vtx.y) < eps) vtx.y = 0.0f;
				if (fabs (vtx.z) < eps) vtx.z = 0.0f;
			}
			if (which & 2) {
				if (fabs (vtx.nx) < eps) vtx.nx = 0.0f;
				if (fabs (vtx.ny) < eps) vtx.ny = 0.0f;
				if (fabs (vtx.nz) < eps) vtx.nz = 0.0f;
			}
			if (which & 4) {
				if (fabs (vtx.tu) < eps) vtx.tu = 0.0f;
				if (fabs (vtx.tv) < eps) vtx.tv = 0.0f;
			}
		}
	}
}

void Mesh::CheckGroup (DWORD grp, DWORD &nd_removed)
{
	DWORD i, j, k, nused, nv = Grp[grp].nVtx, ni = Grp[grp].nIdx;
	bool *nd_used = new bool[nv];
	for (i = 0; i < nv; i++) nd_used[i] = false;
	for (i = 0; i < ni; i++) nd_used[Grp[grp].Idx[i]] = true;
	for (i = nused = 0; i < nv; i++)
		if (nd_used[i]) nused++;
	nd_removed = nv-nused;
	if (nused < nv) {
		D3DVERTEX *tmp = new D3DVERTEX[nused];
		for (i = j = 0; i < nv; i++) {
			if (nd_used[i]) {
				tmp[j++] = Grp[grp].Vtx[i];
			} else {
				for (k = 0; k < ni; k++)
					if (Grp[grp].Idx[k] > j) Grp[grp].Idx[k]--;
			}
		}
		delete []Grp[grp].Vtx;
		Grp[grp].Vtx = tmp;
		Grp[grp].nVtx = nused;
	}
	delete []nd_used;
}

void Mesh::ReleaseTextures ()
{
	if (nTex) {
		//for (DWORD i = 0; i < nTex; i++)
		//	if (Tex[i]) g_texmanager->ReleaseTexture (Tex[i]);
		delete []Tex;
		nTex = 0;
	}
}

DWORD Mesh::Render (LPDIRECT3DDEVICE7 dev)
{
	DWORD i, mi, pmi, ti, pti, zb = 0, gcount = 0, wrap, owrap = 0;

	if (!GrpSetup) Setup();
	dev->ComputeSphereVisibility (GrpCnt, GrpRad, nGrp, 0, GrpVis);
	LPDIRECTDRAWSURFACE7 ptex = 0;

	static D3DMATERIAL7 defmat = {{1,1,1,1},{1,1,1,1},{0,0,0,1},{0,0,0,1},0};

	for (i = 0; i < nGrp; i++) {

		if (GrpVis[i] & D3DSTATUS_DEFAULT)
			continue; // group outside frustrum

		// set material
		mi = Grp[i].MtrlIdx;
		if (mi != SPEC_INHERIT && (!i || mi != pmi)) {
			dev->SetMaterial (mi != SPEC_DEFAULT ? Mtrl+mi : &defmat);
			pmi = mi;
		}

		// set texture
		ti = Grp[i].TexIdx;
		if (ti != SPEC_INHERIT && (!i || (ti != pti))) {
			//dev->SetTexture (0, ti != SPEC_DEFAULT ? Tex[ti] : 0);
			pti = ti;
		}

		if (zb = Grp[i].zBias)
			dev->SetRenderState (D3DRENDERSTATE_ZBIAS, zb);

		wrap = 0;
		if (Grp[i].Flags & 0x03) { // wrap flag
			if (Grp[i].Flags & 0x01) wrap |= D3DWRAP_U;
			if (Grp[i].Flags & 0x02) wrap |= D3DWRAP_V;
		}
		if (wrap != owrap)
			dev->SetRenderState (D3DRENDERSTATE_WRAP0, owrap = wrap);

		dev->DrawIndexedPrimitive (
			D3DPT_TRIANGLELIST, D3DFVF_VERTEX,
			Grp[i].Vtx, Grp[i].nVtx, Grp[i].Idx, Grp[i].nIdx, 0);
		gcount++;
	}

	//if (nGrp > 1 || Grp[0].MtrlIdx != SPEC_INHERIT)
	//	g_scene->SetDefaultMaterial();
	if (zb) dev->SetRenderState (D3DRENDERSTATE_ZBIAS, 0);
	if (owrap) dev->SetRenderState (D3DRENDERSTATE_WRAP0, 0);

	return gcount;
}

istream &operator>> (istream &is, Mesh &mesh)
{
	char cbuf[256], comment[256];
	int i, j, g, ngrp, nvtx, ntri, nidx, nmtrl, mtrl_idx, ntex, tex_idx, flag, res;
	WORD zbias;
	D3DMATERIAL7 mtrl;
	bool term;

	mesh.Clear();

	if (!is.getline (cbuf, 256)) return is;
	if (strcmp (cbuf, "MSHX1")) return is;

	if (!is.getline (cbuf, 256)) return is;
	if (strncmp (cbuf, "GROUPS", 6) || sscanf (cbuf+6, "%d", &ngrp) != 1) return is;

	for (g = 0, term = false; g < ngrp && !term; g++) {

		// set defaults
		D3DVERTEX *vtx;
		WORD *idx;
		mtrl_idx = SPEC_INHERIT;
		tex_idx  = SPEC_INHERIT;
		zbias    = 0;
		flag     = 0;
		comment[0] = '\0';
		bool bnormal = true, calcnml = false;
		nvtx = ntri = 0;

		for (;;) {
			if (!is.getline (cbuf, 256)) { term = true; break; }
			if (!_strnicmp (cbuf, "MATERIAL", 8)) {       // read material index
				sscanf (cbuf+8, "%d", &mtrl_idx);
				mtrl_idx--;
			} else if (!_strnicmp (cbuf, "TEXTURE", 7)) { // read texture index
				sscanf (cbuf+7, "%d", &tex_idx);
				tex_idx--;
			} else if (!_strnicmp (cbuf, "ZBIAS", 5)) {   // read z-bias
				sscanf (cbuf+5, "%hu", &zbias);
			} else if (!_strnicmp (cbuf, "TEXWRAP", 7)) { // read wrap flags
				char uvstr[10] = "";
				sscanf (cbuf+7, "%9s", uvstr);
				if (uvstr[0] == 'U' || uvstr[1] == 'U') flag |= 0x01;
				if (uvstr[0] == 'V' || uvstr[1] == 'V') flag |= 0x02;
			} else if (!_strnicmp (cbuf, "NONORMAL", 8)) {
				bnormal = false; calcnml = true;
			} else if (!_strnicmp (cbuf, "GEOM", 4)) {    // read geometry
				if (sscanf (cbuf+4, "%d%d", &nvtx, &ntri) != 2) break; // parse error - skip group
				for (i = 4; cbuf[i]; i++)           // read comment (preceeded by ';')
					if (cbuf[i] == ';') { strcpy (comment, cbuf+i+1); break; }
				nidx = ntri*3;
				vtx = new D3DVERTEX[nvtx];
				ZeroMemory (vtx, sizeof (D3DVERTEX)*nvtx);
				for (i = 0; i < nvtx; i++) {
					D3DVERTEX &v = vtx[i];
					if (!is.getline (cbuf, 256)) {
						delete []vtx;
						nvtx = 0;
						break;
					}
					if (bnormal) {
						j = sscanf (cbuf, "%f%f%f%f%f%f%f%f",
							&v.x, &v.y, &v.z, &v.nx, &v.ny, &v.nz, &v.tu, &v.tv);
						if (j < 6) calcnml = true;
					} else {
						j = sscanf (cbuf, "%f%f%f%f%f",
							&v.x, &v.y, &v.z, &v.tu, &v.tv);
					}
				}
				idx = new WORD[nidx];
				ZeroMemory (idx, sizeof (WORD)*nidx);
				for (i = j = 0; i < ntri; i++) {
					if (!is.getline (cbuf, 256)) {
						delete []vtx;
						delete []idx;
						nvtx = nidx = 0;
						break;
					}
					sscanf (cbuf, "%hd%hd%hd", idx+j, idx+j+1, idx+j+2);
					j += 3;
				}
				break;
			}
		}
		if (nvtx && nidx) {
			mesh.AddGroup (vtx, nvtx, idx, nidx, mtrl_idx, tex_idx, zbias);
			mesh.Grp[g].Flags = flag;
			if (comment[0]) {
				mesh.Grp[g].Comment = new char[strlen(comment)+1];
				strcpy (mesh.Grp[g].Comment, comment);
			}
			if (calcnml) mesh.CalcNormals (g, true);
		}
	}

	// read material list
	if (is.getline (cbuf, 256) && !strncmp (cbuf, "MATERIALS", 9) && (sscanf (cbuf+9, "%d", &nmtrl) == 1)) {
		Str256 *matname = new Str256[nmtrl];
		Str256 mnm;
		for (i = 0; i < nmtrl; i++) {
			is.getline (cbuf, 256);
			sscanf (cbuf, "%255s", matname[i]);
		}
		for (i = 0; i < nmtrl; i++) {
			ZeroMemory (&mtrl, sizeof (D3DMATERIAL7));
			is.getline (cbuf, 256);
			sscanf (cbuf+8, "%s", mnm);
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
	}

	// read texture list
	mesh.ReleaseTextures ();
	if (is.getline (cbuf, 256) && !strncmp (cbuf, "TEXTURES", 8) && (sscanf (cbuf+8, "%d", &ntex) == 1)) {
		mesh.Tex = new LPDIRECTDRAWSURFACE7[mesh.nTex = ntex];
		Str256 texname;
		for (i = 0; i < ntex; i++) {
			is.getline (cbuf, 256);
			sscanf (cbuf, "%s", texname);
			//if (texname[0] == '0' && texname[1] == '\0')
				mesh.Tex[i] = 0;
			//else
			//	mesh.Tex[i] = g_texmanager->AcquireTexture (texname);
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
			   << (mesh.Grp[g].TexIdx == SPEC_DEFAULT ? 0 : mesh.Grp[g].TexIdx+1)
			   << endl;
		if (mesh.Grp[g].zBias)
			os << "ZBIAS " << mesh.Grp[g].zBias << endl;
		if (mesh.Grp[g].Flags & 0x03) {
			os << "TEXWRAP ";
			if (mesh.Grp[g].Flags & 0x01) os << 'U';
			if (mesh.Grp[g].Flags & 0x02) os << 'V';
			os << endl;
		}
		ntri = mesh.Grp[g].nIdx/3;
		os << "GEOM " << mesh.Grp[g].nVtx << ' ' << ntri;
		if (mesh.Grp[g].Comment) os << " ;" << mesh.Grp[g].Comment;
		os << endl;
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

#ifdef UNDEF
// temporary: cut off semi-infinite domain from mesh
void Mesh::Separate ()
{
	int grp, i, j, k;
	bool remove;

	for (grp = 0; grp < nGrp; grp++) {
		GroupSpec &G = Grp[grp];
		for (i = 0; i < G.nVtx;) {
			if (G.Vtx[i].x < 0.0) {
				for (j = 0; j < G.nIdx/3; j++) {
					for (k = 0, remove = false; k < 3; k++) {
						if (G.Idx[j*3+k] == i)
							remove = true;
						else if (G.Idx[j*3+k] > i)
							G.Idx[j*3+k]--;
					}
					if (remove) {
						for (k = (j+1)*3; k < G.nIdx; k++)
							G.Idx[k-3] = G.Idx[k];
						G.nIdx -= 3;
						j--;
					}
				}
				for (j = i+1; j < G.nVtx; j++)
					G.Vtx[j-1] = G.Vtx[j];
				G.nVtx--;
			} else {
				i++;
			}
		}
	}
}
#endif