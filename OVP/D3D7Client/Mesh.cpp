// ==============================================================
//   ORBITER VISUALISATION PROJECT (OVP)
//   D3D7 Client module
//   Copyright (C) 2006-2016 Martin Schweiger
//   Dual licensed under GPL v3 and LGPL v3
// ==============================================================

// ==============================================================
// Mesh.cpp
// class D3D7Mesh (implementation)
//
// This class represents a mesh in terms of DX7 interface elements
// (vertex buffers, index lists, materials, textures) which allow
// it to be rendered to the D3D7 device.
// ==============================================================

#include "Mesh.h"
#include "Log.h"

using namespace oapi;

static D3DMATERIAL7 defmat = {
	{1,1,1,1},
	{1,1,1,1},
	{0,0,0,1},
	{0,0,0,1},0
};

bool D3D7Mesh::bEnableSpecular = false;

D3D7Mesh::D3D7Mesh (const D3D7Client *client)
{
	gc = client;
	bTemplate = false;
	bVideoMem = (gc->GetFramework()->IsTLDevice() == TRUE);
	bModulateMatAlpha = false;
	nGrp = 0;
	nTex = 1;
	Tex = new LPDIRECTDRAWSURFACE7[nTex];
	Tex[0] = 0;
	nMtrl = 0;
}

D3D7Mesh::D3D7Mesh (const D3D7Client *client, GROUPREC *grp, bool deepcopy)
{
	gc = client;
	bTemplate = false;
	bVideoMem = (gc->GetFramework()->IsTLDevice() == TRUE);
	bModulateMatAlpha = false;
	nGrp = 0;
	nTex = 1;
	Tex = new LPDIRECTDRAWSURFACE7[nTex];
	Tex[0] = 0;
	nMtrl = 0;
	grp->MtrlIdx = SPEC_DEFAULT;
	grp->TexIdx = SPEC_DEFAULT;
	for (DWORD n = 0; n < MAXTEX; n++) {
		grp->TexIdxEx[n] = SPEC_DEFAULT;
		grp->TexMixEx[n] = 0.0;
	}
	AddGroup (grp, deepcopy);
}

D3D7Mesh::D3D7Mesh (const D3D7Client *client, MESHHANDLE hMesh, bool asTemplate)
{
	DWORD i;
	gc = client;
	bTemplate = asTemplate;
	bVideoMem = (gc->GetFramework()->IsTLDevice() && !bTemplate);
	// template meshes are stored in system memory
	bModulateMatAlpha = false;
	nGrp = oapiMeshGroupCount (hMesh);
	Grp = new GROUPREC*[nGrp];
	for (i = 0; i < nGrp; i++) {
		Grp[i] = new GROUPREC;
		MESHGROUPEX *mg = oapiMeshGroupEx (hMesh, i);
		CopyGroup (Grp[i], mg);
	}
	nTex = oapiMeshTextureCount (hMesh)+1;
	Tex = new LPDIRECTDRAWSURFACE7[nTex];
	Tex[0] = 0; // 'no texture'
	for (i = 1; i < nTex; i++) {
		Tex[i] = (LPDIRECTDRAWSURFACE7)oapiGetTextureHandle (hMesh, i);
		// no deep copy here - texture templates shouldn't be modified by vessels
	}
	nMtrl = oapiMeshMaterialCount (hMesh);
	if (nMtrl)
		Mtrl = new D3DMATERIAL7[nMtrl];
	for (i = 0; i < nMtrl; i++)
		CopyMaterial (Mtrl+i, oapiMeshMaterial (hMesh, i));
}

D3D7Mesh::D3D7Mesh (const D3D7Mesh &mesh)
{
	// note: 'mesh' must be a template mesh, because we may not be able to
	// access vertex data in video memory
	DWORD i;
	gc = mesh.gc;
	bTemplate = false;
	bVideoMem = (gc->GetFramework()->IsTLDevice() ? true:false);
	bModulateMatAlpha = mesh.bModulateMatAlpha;
	nGrp = mesh.nGrp;
	Grp = new GROUPREC*[nGrp];
	for (i = 0; i < nGrp; i++) {
		Grp[i] = new GROUPREC;
		CopyGroup (Grp[i], mesh.Grp[i]);
	}
	nTex = mesh.nTex;
	Tex = new LPDIRECTDRAWSURFACE7[nTex];
	for (i = 0; i < nTex; i++) {
		Tex[i] = mesh.Tex[i];
		// no deep copy here - texture templates shouldn't be modified by vessels
	}
	nMtrl = mesh.nMtrl;
	if (nMtrl)
		Mtrl = new D3DMATERIAL7[nMtrl];
	memcpy (Mtrl, mesh.Mtrl, nMtrl*sizeof(D3DMATERIAL7));
}

D3D7Mesh::~D3D7Mesh ()
{
	ClearGroups();
	if (nTex) delete []Tex;
	if (nMtrl) delete []Mtrl;
}

void D3D7Mesh::GlobalEnableSpecular (bool enable)
{
	bEnableSpecular = enable;
}

DWORD D3D7Mesh::AddGroup (GROUPREC *grp, bool deepcopy)
{
	GROUPREC **tmp = new GROUPREC*[nGrp+1];
	if (nGrp) {
		memcpy (tmp, Grp, nGrp*sizeof(GROUPREC*));
		delete []Grp;
	}
	Grp = tmp;
	if (deepcopy) CopyGroup (Grp[nGrp], grp);
	else          Grp[nGrp] = grp;

	return nGrp++;
}

DWORD D3D7Mesh::AddGroup (const MESHGROUPEX *mg)
{
	GROUPREC *tmp = new GROUPREC;
	CopyGroup (tmp, mg);
	return AddGroup (tmp, false);
}

bool D3D7Mesh::CopyGroup (GROUPREC *tgt, const GROUPREC *src)
{
	tgt->nVtx = src->nVtx;
	tgt->nIdx = src->nIdx;
	tgt->Idx = new WORD[tgt->nIdx];
	memcpy (tgt->Idx, src->Idx, tgt->nIdx*sizeof(WORD));
	tgt->TexIdx = src->TexIdx;
	memcpy (tgt->TexIdxEx, src->TexIdxEx, MAXTEX*sizeof(DWORD));
	memcpy (tgt->TexMixEx, src->TexMixEx, MAXTEX*sizeof(float));
	tgt->MtrlIdx = src->MtrlIdx;
	tgt->UsrFlag = src->UsrFlag;
	tgt->zBias   = src->zBias;
	tgt->IntFlag = src->IntFlag;

	// create the vertex buffer
	LPDIRECT3D7 d3d = gc->GetDirect3D7();
	LPDIRECT3DDEVICE7 dev = gc->GetDevice();
	LPVOID data, srcdata;
	bool bVMem = (bVideoMem && (tgt->IntFlag & 0x04));
	DWORD vbCaps = (bVMem ? D3DVBCAPS_WRITEONLY : D3DVBCAPS_SYSTEMMEMORY);
	D3DVERTEXBUFFERDESC vbd = 
		{ sizeof(D3DVERTEXBUFFERDESC), vbCaps, D3DFVF_VERTEX, tgt->nVtx };
	if (d3d->CreateVertexBuffer (&vbd, &tgt->VtxBuf, 0) != D3D_OK) return false;
		// need to come up with a more graceful exit
	tgt->VtxBuf->Lock (DDLOCK_WAIT | DDLOCK_WRITEONLY | DDLOCK_DISCARDCONTENTS, (LPVOID*)&data, NULL);
	src->VtxBuf->Lock (DDLOCK_WAIT | DDLOCK_DISCARDCONTENTS, (LPVOID*)&srcdata, NULL);
	memcpy (data, srcdata, tgt->nVtx*sizeof(D3DVERTEX));
	tgt->VtxBuf->Unlock();
	src->VtxBuf->Unlock();
	if (bVMem)
		tgt->VtxBuf->Optimize (dev, 0);

	return true;
}

bool D3D7Mesh::CopyGroup (GROUPREC *grp, const MESHGROUPEX *mg)
{
	grp->nVtx = mg->nVtx;
	grp->nIdx = mg->nIdx;
	grp->Idx = new WORD[grp->nIdx];
	memcpy (grp->Idx, mg->Idx, grp->nIdx*sizeof(WORD));
	grp->TexIdx = mg->TexIdx;
	memcpy (grp->TexIdxEx, mg->TexIdxEx, MAXTEX*sizeof(DWORD));
	memcpy (grp->TexMixEx, mg->TexMixEx, MAXTEX*sizeof(float));
	if (grp->TexIdx != SPEC_DEFAULT && grp->TexIdx != SPEC_INHERIT) grp->TexIdx++;
	for (DWORD n = 0; n < MAXTEX; n++)
		if (grp->TexIdxEx[n] != SPEC_DEFAULT) grp->TexIdxEx[n]++;
	grp->MtrlIdx = mg->MtrlIdx;
	grp->UsrFlag = mg->UsrFlag;
	grp->zBias   = mg->zBias;
	grp->IntFlag = mg->Flags;

	// create the vertex buffer
	LPDIRECT3D7 d3d = gc->GetDirect3D7();
	LPDIRECT3DDEVICE7 dev = gc->GetDevice();
	LPVOID data;
	bool bVMem = (bVideoMem && (mg->Flags & 0x04));
	DWORD vbCaps = (bVMem ? D3DVBCAPS_WRITEONLY : D3DVBCAPS_SYSTEMMEMORY);
	D3DVERTEXBUFFERDESC vbd = 
		{ sizeof(D3DVERTEXBUFFERDESC), vbCaps, D3DFVF_VERTEX, grp->nVtx };
	if (d3d->CreateVertexBuffer (&vbd, &grp->VtxBuf, 0) != D3D_OK) return false;
		// need to come up with a more graceful exit
	grp->VtxBuf->Lock (DDLOCK_WAIT | DDLOCK_WRITEONLY | DDLOCK_DISCARDCONTENTS, (LPVOID*)&data, NULL);
	memcpy (data, mg->Vtx, grp->nVtx*sizeof(D3DVERTEX));
		// warning: this assumes consistency of Orbiter's NTVERTEX struct with
		// D3DVERTEX. Generally, this will need to be copied element by element
	grp->VtxBuf->Unlock();
	if (bVMem)
		grp->VtxBuf->Optimize (dev, 0);

	return true;
}

bool D3D7Mesh::CopyMaterial (D3DMATERIAL7 *mat7, MATERIAL *mat)
{
	memcpy (mat7, mat, sizeof (D3DMATERIAL7));
	return true;
	// exploits the fact that D3DMATERIAL7 and MATERIAL are identical
	// structures. In general, this needs to be converted from one
	// structure to the other.
}

void D3D7Mesh::DeleteGroup (GROUPREC *grp)
{
	delete []grp->Idx;
	if (grp->VtxBuf) grp->VtxBuf->Release();
	delete grp;
}

int D3D7Mesh::GetGroup (DWORD grp, GROUPREQUESTSPEC *grs)
{
	static NTVERTEX zero = {0,0,0, 0,0,0, 0,0};
	if (grp >= nGrp) return 1;
	GROUPREC *g = Grp[grp];
	D3DVERTEX *vtxd3d;
	NTVERTEX *vtx;
	DWORD nv = g->nVtx;
	DWORD ni = g->nIdx;
	DWORD i, vi;
	int ret = 0;

	if (grs->nVtx && grs->Vtx) { // vertex data requested
		if (g->VtxBuf->Lock (DDLOCK_WRITEONLY, (LPVOID*)&vtxd3d, 0) == D3D_OK) {
			vtx = (NTVERTEX*)vtxd3d; // only allowed because format is compatible
			if (grs->VtxPerm) { // random access data request
				for (i = 0; i < grs->nVtx; i++) {
					vi = grs->VtxPerm[i];
					if (vi < nv) {
						grs->Vtx[i] = vtx[vi];
					} else {
						grs->Vtx[i] = zero;
						ret = 1;
					}
				}
			} else {
				if (grs->nVtx > nv) grs->nVtx = nv;
				memcpy (grs->Vtx, vtx, grs->nVtx * sizeof(NTVERTEX));
			}
			g->VtxBuf->Unlock ();
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

int D3D7Mesh::EditGroup (DWORD grp, GROUPEDITSPEC *ges)
{
	if (grp >= nGrp) return 1;
	GROUPREC *g = Grp[grp];
	DWORD i, vi;

	DWORD flag = ges->flags;
	if (flag & GRPEDIT_SETUSERFLAG)
		g->UsrFlag = ges->UsrFlag;
	else if (flag & GRPEDIT_ADDUSERFLAG)
		g->UsrFlag |= ges->UsrFlag;
	else if (flag & GRPEDIT_DELUSERFLAG)
		g->UsrFlag &= ~ges->UsrFlag;

	if (flag & GRPEDIT_VTXMOD) {
		D3DVERTEX *vtx;
		if (g->VtxBuf->Lock (DDLOCK_WRITEONLY, (LPVOID*)&vtx, 0) == D3D_OK) {
			for (i = 0; i < ges->nVtx; i++) {
				vi = (ges->vIdx ? ges->vIdx[i] : i);
				if (vi < g->nVtx) {
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
					if      (flag & GRPEDIT_VTXTEXU)    vtx[vi].tu  = ges->Vtx[i].tu;
					else if (flag & GRPEDIT_VTXTEXADDU) vtx[vi].tu += ges->Vtx[i].tu;
					if      (flag & GRPEDIT_VTXTEXV)    vtx[vi].tv  = ges->Vtx[i].tv;
					else if (flag & GRPEDIT_VTXTEXADDV) vtx[vi].tv += ges->Vtx[i].tv;
				}
			}
			g->VtxBuf->Unlock ();
		}
	}
	return 0;
}

void D3D7Mesh::ClearGroups ()
{
	if (nGrp) {
		for (DWORD g = 0; g < nGrp; g++)
			DeleteGroup (Grp[g]);
		delete []Grp;
		nGrp = 0;
	}
}

bool D3D7Mesh::SetTexture (DWORD texidx, SURFHANDLE tex)
{
	if (texidx >= nTex) return false;
	// do we need to release the previous texture here?
	Tex[texidx] = (LPDIRECTDRAWSURFACE7)tex;
	return true;
}

void D3D7Mesh::SetTexMixture (DWORD ntex, float mix)
{
	ntex--;
	for (DWORD g = 0; g < nGrp; g++) {
		if (Grp[g]->TexIdxEx[ntex] != SPEC_DEFAULT)
			Grp[g]->TexMixEx[ntex] = mix;
	}
}

void D3D7Mesh::RenderGroup (LPDIRECT3DDEVICE7 dev, GROUPREC *grp)
{
#ifdef UNDEF
	if (setstate) {
		if (grp->TexIdx[0] != SPEC_INHERIT) {
			if (grp->TexIdx[0] < nTex)
				dev->SetTexture (0, Tex[grp->TexIdx[0]]);
			else
				dev->SetTexture (0, 0);
		}
		if (grp->MtrlIdx != SPEC_INHERIT) {
			if (grp->MtrlIdx < nMtrl)
				dev->SetMaterial (Mtrl+grp->MtrlIdx);
			else
				dev->SetMaterial (&defmat);
		}
	}
#endif

	if (grp->nVtx && grp->nIdx) {
		if (FAILED (dev->DrawIndexedPrimitiveVB (
			D3DPT_TRIANGLELIST,
			grp->VtxBuf, 0, grp->nVtx, grp->Idx, grp->nIdx, 0)))
				LOGOUT_ERR("Render error\n");
	}
}

void D3D7Mesh::Render (LPDIRECT3DDEVICE7 dev)
{
	dev->SetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, TRUE);
	if (bModulateMatAlpha)
		dev->SetTextureStageState (0, D3DTSS_ALPHAOP, D3DTOP_MODULATE);

	DWORD g, j, n, mi, pmi, ti, pti, uflag, wrap, zb = 0, owrap = 0;
	bool skipped = false;
	bool texstage[MAXTEX] = {false};
	BOOL specular = FALSE;
	BOOL lighting = TRUE;

	for (g = 0; g < nGrp; g++) {

		uflag = Grp[g]->UsrFlag;
		if (uflag & 2) { // user skip
			skipped = true;
			continue;
		}

		// set material
		if ((mi = Grp[g]->MtrlIdx) == SPEC_INHERIT && skipped) // find last valid material
			for (j = g-1; j >= 0; j--)
				if ((mi = Grp[j]->MtrlIdx) != SPEC_INHERIT) break;
		if (mi != SPEC_INHERIT && (!g || mi != pmi)) {
			LPD3DMATERIAL7 mat = (mi != SPEC_DEFAULT ? Mtrl+mi : &defmat);
			dev->SetMaterial (mat);
			if (bEnableSpecular) {
				if (mat->power) {
					if (!specular) dev->SetRenderState (D3DRENDERSTATE_SPECULARENABLE, specular = TRUE);
				} else {
					if (specular) dev->SetRenderState (D3DRENDERSTATE_SPECULARENABLE, specular = FALSE);
				}
			}
			pmi = mi;
		}
		
		// set primary texture
		if ((ti = Grp[g]->TexIdx) == SPEC_INHERIT && skipped) // find last valid texture
			for (j = g-1; j >= 0; j--)
				if ((ti = Grp[j]->TexIdx) != SPEC_INHERIT) break;
		if (ti != SPEC_INHERIT && (!g || (ti != pti))) {
			LPDIRECTDRAWSURFACE7 tx;
			if (ti != SPEC_DEFAULT) {
				tx = (ti < TEXIDX_MFD0 ? Tex[ti] : (LPDIRECTDRAWSURFACE7)gc->GetMFDSurface(ti-TEXIDX_MFD0));
			} else tx = 0;
			dev->SetTexture (0, tx);
			pti = ti;
		}

		// set additional textures
		for (n = 0; n < MAXTEX; n++) {
			if (Grp[g]->TexMixEx[n] && (ti = Grp[g]->TexIdxEx[n]) != SPEC_DEFAULT) {
				dev->SetTexture (n+1, Tex[ti]);
				dev->SetTextureStageState (n+1, D3DTSS_COLOROP, D3DTOP_ADD);
				dev->SetTextureStageState (n+1, D3DTSS_ADDRESS, D3DTADDRESS_WRAP);
				texstage[n] = true;
			} else if (texstage[n]) {
				dev->SetTextureStageState (n+1, D3DTSS_COLOROP, D3DTOP_DISABLE);
				texstage[n] = false;
			}
		}

		if (zb != Grp[g]->zBias)
			dev->SetRenderState (D3DRENDERSTATE_ZBIAS, zb = Grp[g]->zBias);

		wrap = 0;
		if (Grp[g]->IntFlag & 0x03) { // wrap flag
			if (Grp[g]->IntFlag & 0x01) wrap |= D3DWRAP_U;
			if (Grp[g]->IntFlag & 0x02) wrap |= D3DWRAP_V;
		}
		if (wrap != owrap)
			dev->SetRenderState (D3DRENDERSTATE_WRAP0, owrap = wrap);

		if (!(uflag & 0x4) != lighting)
			dev->SetRenderState (D3DRENDERSTATE_LIGHTING, lighting = !lighting);

		if (uflag & 0x8) { // brighten
			dev->SetRenderState (D3DRENDERSTATE_SRCBLEND, D3DBLEND_SRCALPHA);
			dev->SetRenderState (D3DRENDERSTATE_DESTBLEND, D3DBLEND_ONE);
		}

		if (uflag &0x10) { // skip texture color information
			dev->SetTextureStageState (0, D3DTSS_COLOROP, D3DTOP_SELECTARG2);
		}

		RenderGroup (dev, Grp[g]);

		if (uflag & 0x8) { // reset brighten
			dev->SetRenderState (D3DRENDERSTATE_SRCBLEND, D3DBLEND_SRCALPHA);
			dev->SetRenderState (D3DRENDERSTATE_DESTBLEND, D3DBLEND_INVSRCALPHA);
		}

		if (uflag & 0x10) {
			dev->SetTextureStageState (0, D3DTSS_COLOROP, D3DTOP_MODULATE);
		}

		skipped = false;
	}

	if (owrap)     dev->SetRenderState (D3DRENDERSTATE_WRAP0, 0);
	if (zb)        dev->SetRenderState (D3DRENDERSTATE_ZBIAS, 0);
	if (specular)  dev->SetRenderState (D3DRENDERSTATE_SPECULARENABLE, FALSE);
	if (!lighting) dev->SetRenderState (D3DRENDERSTATE_LIGHTING, TRUE);
	for (n = 0; n < MAXTEX; n++) {
		if (texstage[n]) 
			dev->SetTextureStageState (n+1, D3DTSS_COLOROP, D3DTOP_DISABLE);
	}
	if (bModulateMatAlpha)
		dev->SetTextureStageState (0, D3DTSS_ALPHAOP, D3DTOP_SELECTARG1);
}

void D3D7Mesh::TransformGroup (DWORD n, const D3DMATRIX *m)
{
	GROUPREC *grp = Grp[n];
	int i, nv = grp->nVtx;
	D3DVERTEX *vtx;
	grp->VtxBuf->Lock (DDLOCK_WAIT | DDLOCK_DISCARDCONTENTS, (LPVOID*)&vtx, NULL);
	FLOAT x, y, z, w;

	for (i = 0; i < nv; i++) {
		D3DVERTEX &v = vtx[i];
		x = v.x*m->_11 + v.y*m->_21 + v.z* m->_31 + m->_41;
		y = v.x*m->_12 + v.y*m->_22 + v.z* m->_32 + m->_42;
		z = v.x*m->_13 + v.y*m->_23 + v.z* m->_33 + m->_43;
		w = v.x*m->_14 + v.y*m->_24 + v.z* m->_34 + m->_44;
    	v.x = x/w;
		v.y = y/w;
		v.z = z/w;

		x = v.nx*m->_11 + v.ny*m->_21 + v.nz* m->_31;
		y = v.nx*m->_12 + v.ny*m->_22 + v.nz* m->_32;
		z = v.nx*m->_13 + v.ny*m->_23 + v.nz* m->_33;
		w = 1.0f/(FLOAT)sqrt (x*x + y*y + z*z);
		v.nx = x*w;
		v.ny = y*w;
		v.nz = z*w;
	}
	grp->VtxBuf->Unlock();
	//if (GrpSetup) SetupGroup (grp);
}

