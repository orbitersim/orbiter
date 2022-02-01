// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#define STRICT 1
#define D3D_OVERLOADS
#include <stdio.h>
#include "D3dmath.h"
#include "D3d7util.h"
#include "Spherepatch.h"
#include "Vecmat.h"
#include "Texture.h"
#include "Planet.h"
#include "VPlanet.h"
#include "Orbiter.h"
#include "Scene.h"
#include "Camera.h"
#include "Log.h"
#include "Util.h"
#include "OGraphics.h"

// =======================================================================
// Externals

extern Orbiter *g_pOrbiter;
extern Camera *g_camera;
extern TextureManager2 *g_texmanager2;
extern int patchidx[9];
extern char DBG_MSG[256];

// =======================================================================
// Local prototypes

static int Exist (Vector *node, int nnode, Vector p);
static void CreateRing (Mesh &mesh, float irad, float orad, int nsect);

LPDIRECT3DVERTEXBUFFER7 bbtarget;  // target buffer for bounding box transformation
DWORD VB_MemFlag = D3DVBCAPS_SYSTEMMEMORY; // default; overwritten for T&L devices

float TEX2_MULTIPLIER = 4.0f; // was: 16.0f

// =======================================================================
// Global resources

// Max supported patch resolution level
const int maxlvl = SURF_MAX_PATCHLEVEL;

// sphere patch templates at different resolutions
VBMESH PATCH_TPL_1;
VBMESH PATCH_TPL_2;
VBMESH PATCH_TPL_3;
VBMESH PATCH_TPL_4[2];
VBMESH PATCH_TPL_5;
VBMESH PATCH_TPL_6[2];
VBMESH PATCH_TPL_7[4];
VBMESH PATCH_TPL_8[8];
VBMESH PATCH_TPL_9[16];
VBMESH PATCH_TPL_10[32];
VBMESH PATCH_TPL_11[64];
VBMESH PATCH_TPL_12[128];
VBMESH PATCH_TPL_13[256];
VBMESH PATCH_TPL_14[512];
VBMESH *PATCH_TPL[15] = {0,
	&PATCH_TPL_1, &PATCH_TPL_2, &PATCH_TPL_3, PATCH_TPL_4, &PATCH_TPL_5,
	PATCH_TPL_6, PATCH_TPL_7, PATCH_TPL_8, PATCH_TPL_9, PATCH_TPL_10,
	PATCH_TPL_11, PATCH_TPL_12, PATCH_TPL_13, PATCH_TPL_14
};

int NLAT[9] = {0,1,1,1,1,1,2,4,8};
int NLNG5[1] = {4};
int NLNG6[2] = {8,4};
int NLNG7[4] = {16,16,12,6};
int NLNG8[8] = {32,32,30,28,24,18,12,6};
int *NLNG[9] = {0,0,0,0,0,NLNG5,NLNG6,NLNG7,NLNG8};

// =======================================================================
// Module initialisation

void CreatePatchDeviceObjects (LPDIRECT3D7 d3d, LPDIRECT3DDEVICE7 dev)
{
	VB_MemFlag = (g_pOrbiter->GetInlineGraphicsClient()->GetFramework()->IsTLDevice() ? 0 : D3DVBCAPS_SYSTEMMEMORY);
	// Set vertex buffer memory flag: video memory for T&L devices,  system memory for all others

	// Level 1 patch template
	CreateSphere (d3d, dev, PATCH_TPL_1, 6, false, 0, 64);

	// Level 2 patch template
	CreateSphere (d3d, dev, PATCH_TPL_2, 8, false, 0, 128);

	// Level 3 patch template
	CreateSphere (d3d, dev, PATCH_TPL_3, 12, false, 0, 256);

	// Level 4 patch templates
	CreateSphere (d3d, dev, PATCH_TPL_4[0], 16, true, 0, 256);
	CreateSphere (d3d, dev, PATCH_TPL_4[1], 16, true, 1, 256);

	// Level 5 patch template
	CreateSpherePatch (d3d, dev, PATCH_TPL_5, 4, 1, 0, 18);

	// Level 6 patch templates
	CreateSpherePatch (d3d, dev, PATCH_TPL_6[0], 8, 2, 0, 10, 16);
	CreateSpherePatch (d3d, dev, PATCH_TPL_6[1], 4, 2, 1, 12);

	// Level 7 patch templates
	CreateSpherePatch (d3d, dev, PATCH_TPL_7[0], 16, 4, 0, 12, 12, false);
	CreateSpherePatch (d3d, dev, PATCH_TPL_7[1], 16, 4, 1, 12, 12, false);
	CreateSpherePatch (d3d, dev, PATCH_TPL_7[2], 12, 4, 2, 10, 16, true);
	CreateSpherePatch (d3d, dev, PATCH_TPL_7[3],  6, 4, 3, 12, -1, true);

	// Level 8 patch templates
	CreateSpherePatch (d3d, dev, PATCH_TPL_8[0], 32, 8, 0, 12, 15, false, true, true);
	CreateSpherePatch (d3d, dev, PATCH_TPL_8[1], 32, 8, 1, 12, 15, false, true, true);
	CreateSpherePatch (d3d, dev, PATCH_TPL_8[2], 30, 8, 2, 12, 16, false, true, true);
	CreateSpherePatch (d3d, dev, PATCH_TPL_8[3], 28, 8, 3, 12, 12, false, true, true);
	CreateSpherePatch (d3d, dev, PATCH_TPL_8[4], 24, 8, 4, 12, 12, false, true, true);
	CreateSpherePatch (d3d, dev, PATCH_TPL_8[5], 18, 8, 5, 12, 12, false, true, true);
	CreateSpherePatch (d3d, dev, PATCH_TPL_8[6], 12, 8, 6, 10, 16, true,  true, true);
	CreateSpherePatch (d3d, dev, PATCH_TPL_8[7],  6, 8, 7, 12, -1, true,  true, true);

	// Patch templates for level 9 and beyond
	const int n = 8;
	const int nlng8[8] = {32,32,30,28,24,18,12,6};
	const int res8[8] = {15,15,16,12,12,12,12,12};
	int mult = 2, idx, lvl, i, j;
	for (lvl = 9; lvl <= maxlvl; lvl++) {
		idx = 0;
		for (i = 0; i < 8; i++) {
			for (j = 0; j < mult; j++) {
				if (idx < n*mult)
					CreateSpherePatch (d3d, dev, PATCH_TPL[lvl][idx], nlng8[i]*mult, n*mult, idx, 12, res8[i], false, true, true, true);
				else
					CreateSpherePatch (d3d, dev, PATCH_TPL[lvl][idx], nlng8[i]*mult, n*mult, idx, 12, -1, true, true, true, true);
				idx++;
			}
		}
		mult *= 2;
	}
}

void DestroyPatchDeviceObjects ()
{
	int i;
	DestroyVBMesh (PATCH_TPL_1);
	DestroyVBMesh (PATCH_TPL_2);
	DestroyVBMesh (PATCH_TPL_3);
	for (i = 0; i <  2; i++) DestroyVBMesh (PATCH_TPL_4[i]);
	DestroyVBMesh (PATCH_TPL_5);
	for (i = 0; i <  2; i++) DestroyVBMesh (PATCH_TPL_6[i]);
	for (i = 0; i <  4; i++) DestroyVBMesh (PATCH_TPL_7[i]);
	for (i = 0; i <  8; i++) DestroyVBMesh (PATCH_TPL_8[i]);

	const int n = 8;
	int mult = 2, lvl;
	for (lvl = 9; lvl <= maxlvl; lvl++) {
		for (i = 0; i < n*mult; i++) DestroyVBMesh (PATCH_TPL[lvl][i]);
		mult *= 2;
	}
}

void RestorePatchDeviceObjects (LPDIRECT3D7 d3d, LPDIRECT3DDEVICE7 dev)
{
	if (!(VB_MemFlag & D3DVBCAPS_SYSTEMMEMORY)) {
		DestroyPatchDeviceObjects ();
		CreatePatchDeviceObjects (d3d, dev);
	}
}


// =======================================================================
// Class VBMESH

VBMESH::VBMESH ()
{
	vb = 0;
	bb = 0;
	vtx = 0;
	bbvtx = 0;
	nv = 0;
	idx = 0;
	ni = 0;
}

VBMESH::~VBMESH ()
{
	if (vb) vb->Release();
	if (bb) bb->Release();
	if (nv && vtx) delete []vtx;
	if (bbvtx) delete []bbvtx;
	if (ni) delete []idx;
}

void VBMESH::MapVertices (LPDIRECT3D7 d3d, LPDIRECT3DDEVICE7 dev, DWORD MemFlag)
{
	if (vb) vb->Release();

	D3DVERTEXBUFFERDESC vbd = 
		{ sizeof(D3DVERTEXBUFFERDESC), MemFlag | D3DVBCAPS_WRITEONLY, FVF_2TEX, nv };
	d3d->CreateVertexBuffer (&vbd, &vb, 0);
	LPVOID data;
	vb->Lock (DDLOCK_WAIT | DDLOCK_WRITEONLY | DDLOCK_DISCARDCONTENTS, (LPVOID*)&data, NULL);
	memcpy (data, vtx, nv*sizeof(VERTEX_2TEX));
	vb->Unlock();
	vb->Optimize (dev, 0);
}

// =======================================================================
// Class PatchManager

PatchManager::PatchManager (const char *_name, char _res_id, int _npatch,
	int _nlat, int *_nlng, VBMESH *_patch, D3DMATRIX *_trans,
	Vector *_patchcnt, double *_patchrad,
	LPDIRECTDRAWSURFACE7 *_tex, LPDIRECTDRAWSURFACE7 *_ntex)
: res_id(_res_id), npatch(_npatch), nlat(_nlat), nlng(_nlng),
  vbpatch(_patch), trans(_trans), patchcnt(_patchcnt), patchrad(_patchrad),
  tex(_tex), ntex(_ntex)
{
	strcpy (name, _name);
	ref = 0;
	vis = 0;
	microstruct = 0;
}

PatchManager::~PatchManager ()
{}

bool PatchManager::SetReflectionColour (D3DCOLORVALUE *col)
{
	if (g_pOrbiter->Cfg()->CfgVisualPrm.bWaterreflect && ref && ref->Type() == OBJTP_PLANET) {
		Planet *planet = (Planet*)ref;  // should be generalised for CelesitalBody
		const ATMCONST* ap = planet->AtmParams();
		if (ap) {
			double fac = 0.7; // adjust!
			Vector S = -planet->GPos();
			Vector C = g_camera->GPos() + S;
			double cosa = dotp (S, C) / (S.length() * C.length());
			double alpha = 0.5*acos(cosa); // sun reflection angle
			
			double scale = sin(alpha)*fac;
			col->r = (float)max(0.0, 1.05 - scale*ap->color0.x);
			col->g = (float)max(0.0, 1.05 - scale*ap->color0.y);
			col->b = (float)max(0.0, 1.05 - scale*ap->color0.z);
			return true;
		}
	}
	return false;
}

void PatchManager::Render (LPDIRECT3DDEVICE7 dev, D3DMATRIX &wmat, double visrad, bool addbkg)
{
	int i, j, hemisphere, idx = 0;
	//int k;
	D3DMATRIX wtrans, imat;

	D3DMath_MatrixInvert (imat, wmat);
	Vector rpos(imat._41, imat._42, imat._43);   // camera in local coords
	double id = 1.0 / max (rpos.length(), 1.0);  // inverse camera distance
	if (!visrad) visrad = acos (id);             // aperture of visibility sector
	rpos *= id;                                  // surface point below camera
	bool hasmicro = false;
	int tstage = 0;

	dVERIFY (dev->SetTextureStageState (0, D3DTSS_ADDRESS, D3DTADDRESS_CLAMP), "LPDIRECT3DDEVICE7::SetTextureStageState failed");

	// surface texturing
	D3DVIEWPORT7 vp;
	dev->GetViewport (&vp);
	int v;
	bool bx1, bx2, by1, by2, bz1, bz2, bbvis;
	//bool bbvis_land;
	int nquery = 0, nrender = 0; // temporary
	D3DVALUE x, y;

	if (addbkg) {
		dev->SetTextureStageState (1, D3DTSS_COLOROP, D3DTOP_ADD);
		dev->SetTextureStageState (1, D3DTSS_COLORARG1, D3DTA_CURRENT);
		dev->SetTextureStageState (1, D3DTSS_COLORARG2, D3DTA_TFACTOR);
		Vector bgc = g_pOrbiter->GetInlineGraphicsClient()->GetScene()->BGcol();
		dev->SetRenderState (D3DRENDERSTATE_TEXTUREFACTOR, D3DRGBA(bgc.x, bgc.y, bgc.z, 1));
	}

	DWORD nmlscale = TRUE;
	dev->GetRenderState (D3DRENDERSTATE_NORMALIZENORMALS, &nmlscale);
	if (!nmlscale)
		dev->SetRenderState (D3DRENDERSTATE_NORMALIZENORMALS, TRUE);

	// set microstructure texture
#ifdef OLDMICROCLOUD
	if (microstruct) {
		dev->SetTextureStageState (1, D3DTSS_COLOROP, D3DTOP_SELECTARG1);
		dev->SetTextureStageState (1, D3DTSS_COLORARG1, D3DTA_CURRENT);
		dev->SetTextureStageState (1, D3DTSS_ALPHAOP, D3DTOP_MODULATE);
		dev->SetTextureStageState (1, D3DTSS_ALPHAARG1, D3DTA_TEXTURE);
		dev->SetTextureStageState (1, D3DTSS_ALPHAARG2, D3DTA_CURRENT);
		dev->SetTextureStageState (1, D3DTSS_TEXCOORDINDEX, 1);
		dev->SetTexture (1, cloudstruct);
	}
#else
	if (microstruct && microlevel > 0) {
		dev->SetTextureStageState (0, D3DTSS_COLOROP, D3DTOP_SELECTARG1);
		dev->SetTextureStageState (0, D3DTSS_COLORARG1, D3DTA_DIFFUSE);
		dev->SetTextureStageState (1, D3DTSS_COLOROP, D3DTOP_MODULATE);
		dev->SetTextureStageState (1, D3DTSS_COLORARG1, D3DTA_TEXTURE);
		dev->SetTextureStageState (1, D3DTSS_COLORARG2, D3DTA_CURRENT);

		dev->SetTextureStageState (0, D3DTSS_ALPHAOP, D3DTOP_ADDSMOOTH);
		dev->SetTextureStageState (0, D3DTSS_TEXCOORDINDEX, 1);
		dev->SetTextureStageState (0, D3DTSS_ADDRESS, D3DTADDRESS_WRAP);
		dev->SetTexture (0, cloudstruct);
		dev->SetTextureStageState (0, D3DTSS_ALPHAARG1, D3DTA_TEXTURE);
		dev->SetTextureStageState (0, D3DTSS_ALPHAARG2, D3DTA_TFACTOR);
		dev->SetTextureStageState (1, D3DTSS_ALPHAOP, D3DTOP_MODULATE);
		dev->SetTextureStageState (1, D3DTSS_ALPHAARG1, D3DTA_TEXTURE);
		dev->SetTextureStageState (1, D3DTSS_ALPHAARG2, D3DTA_CURRENT);
		dev->SetTextureStageState (1, D3DTSS_TEXCOORDINDEX, 0);
		dev->SetTextureStageState (1, D3DTSS_ADDRESS, D3DTADDRESS_CLAMP);
		double alpha = 1.0-microlevel;
		dev->SetRenderState (D3DRENDERSTATE_TEXTUREFACTOR, D3DRGBA(1, 1, 1, alpha));
		tstage = 1;
		hasmicro = true;
	}
#endif

	for (hemisphere = 0; hemisphere < 2; hemisphere++) {
		for (i = nlat-1; i >= 0; i--) { // move from poles towards equator
			for (j = 0; j < nlng[i]; j++, idx++) {
				if (vis) vis[idx] = false;

				// 1. check whether patch is within horizon radius of visibility
				double cntdist = acos (rpos & patchcnt[idx]);
				if (cntdist-patchrad[idx] >= visrad) continue;

				nquery++;
				D3DMath_MatrixMultiply (wtrans, wmat, trans[idx]);
				dVERIFY (dev->SetTransform (D3DTRANSFORMSTATE_WORLD, &wtrans), "LPDIRECT3DDEVICE7::SetTransform failed");

				// 2. check whether patch bounding box intersects viewing fustrum
				VERTEX_XYZH *vtx;
				dVERIFY (bbtarget->ProcessVertices (D3DVOP_TRANSFORM, 0, 8, vbpatch[i].bb, 0, dev, 0), "LPDIRECT3DVERTEXBUFFER7::ProcessVertices failed");
				bbtarget->Lock (DDLOCK_WAIT | DDLOCK_READONLY | DDLOCK_DISCARDCONTENTS, (LPVOID*)&vtx, NULL);
				bx1 = bx2 = by1 = by2 = bz1 = bz2 = bbvis = false;
				for (v = 0; v < 8; v++) {
					if (vtx[v].z > 0.0) bz1 = true;
					if (vtx[v].z < 1.0) bz2 = true, x =  vtx[v].x, y =  vtx[v].y;
					else                            x = -vtx[v].x, y = -vtx[v].y;
					if (x > vpX0)       bx1 = true;
					if (x < vpX1)       bx2 = true;
					if (y > vpY0)       by1 = true;
					if (y < vpY1)       by2 = true;
					if (bbvis = bx1 && bx2 && by1 && by2 && bz1 && bz2) break;
				}
				bbtarget->Unlock();
				if (!bbvis) continue;

				if (vis) vis[idx] = true;

#ifdef UNDEF
				// 3. check whether patch is completely hidden under high resolution tiles
				// (only used by PatchManager8)
				if (tm) {
					tm->CalcTileVisibility (dev, wmat, idx, i);
					const TILESPEC &ts = tm->TS()[idx];
					if (!ts.bVisible)
						bbvis = false; // tile not visible at all
					else {
						for (k = 0, bbvis = bbvis_land = false; k < 4; k++) {
							if (ts.bChildVis[k]) { // subtile visible
								if ((ts.ChildFlag[k] & 7) != 5) bbvis = true; // child doesn't cover
								if ((ts.ChildFlag[k] & 7) == 3) bbvis_land = true; // separate landmask render pass required
							}
						}
					}
					if (!bbvis) continue;
				} else bbvis_land = true;
#endif

				nrender++;
				//if (tm) dev->SetTransform (D3DTRANSFORMSTATE_WORLD, &wtrans); // CalcTileVisibility resets the transform

				dVERIFY (dev->SetTexture (tstage, tex ? tex[idx] : 0), "LPDIRECT3DDEVICE7::SetTexture failed");
				dev->DrawIndexedPrimitiveVB (D3DPT_TRIANGLELIST, vbpatch[i].vb, 0, vbpatch[i].nv, vbpatch[i].idx, vbpatch[i].ni, 0);
			}
		}
	}
	dVERIFY (dev->SetTextureStageState (0, D3DTSS_ADDRESS, D3DTADDRESS_WRAP), "LPDIRECT3DDEVICE7::SetTextureStageState failed");

	// remove microstructure texture
	if (hasmicro) {
		dev->SetTextureStageState (0, D3DTSS_COLOROP, D3DTOP_MODULATE);
		dev->SetTextureStageState (0, D3DTSS_COLORARG1, D3DTA_TEXTURE);
		dev->SetTextureStageState (1, D3DTSS_COLOROP, D3DTOP_DISABLE);

		dev->SetTextureStageState (0, D3DTSS_ALPHAOP, D3DTOP_SELECTARG1);
		dev->SetTextureStageState (0, D3DTSS_ALPHAARG1, D3DTA_TEXTURE);
		dev->SetTextureStageState (0, D3DTSS_ALPHAARG2, D3DTA_CURRENT);
		dev->SetTextureStageState (0, D3DTSS_TEXCOORDINDEX, 0);
		dev->SetTextureStageState (1, D3DTSS_ALPHAOP, D3DTOP_DISABLE);
		dev->SetTexture (1, 0);
	}

	if (addbkg)
		dev->SetTextureStageState (1, D3DTSS_COLOROP, D3DTOP_DISABLE);

	if (!nmlscale)
		dev->SetRenderState (D3DRENDERSTATE_NORMALIZENORMALS, FALSE);
}

void PatchManager::RenderNightlights (LPDIRECT3DDEVICE7 dev, D3DMATRIX &wmat, double visrad)
{
	if (!ntex) return;

	static D3DMATERIAL7 pmat, lightmat = {{0,0,0,0},{0,0,0,0},{0,0,0,0},{0,0,0,1},0};
	static D3DMATERIAL7 lightmat_dim = {{0,0,0,0},{0,0,0,0},{0,0,0,0},{0,0,0,1},0};
	D3DMATRIX wtrans;
	lightmat.emissive.b = (lightmat.emissive.r = lightmat.emissive.g = (float)g_pOrbiter->Cfg()->CfgVisualPrm.LightBrightness);
	dVERIFY (dev->GetMaterial (&pmat), "LPDIRECT3DDEVICE7::GetMaterial failed");
	dVERIFY (dev->SetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, TRUE), "LPDIRECT3DDEVICE7::SetRenderState failed");
	int i, j, hemisphere, idx;
	Vector sundir;
	double dcos;
	float scale;
	bool dorender, modmat = true;

#ifdef MICROLIGHTTEX
	// add in additional fine-structure generic texture
	dev->SetTextureStageState (1, D3DTSS_COLOROP, D3DTOP_MODULATE);
	dev->SetTextureStageState (1, D3DTSS_COLORARG1, D3DTA_TEXTURE);
	dev->SetTextureStageState (1, D3DTSS_COLORARG2, D3DTA_CURRENT);
	dev->SetTextureStageState (1, D3DTSS_ALPHAOP, D3DTOP_MODULATE);
	dev->SetTextureStageState (1, D3DTSS_ALPHAARG1, D3DTA_TEXTURE);
	dev->SetTextureStageState (1, D3DTSS_ALPHAARG2, D3DTA_CURRENT);
	dev->SetTextureStageState (1, D3DTSS_TEXCOORDINDEX, 1);
	dev->SetTexture (1, lightstruct1);
#endif

	if (ref) {
		sundir.Set (tmul (ref->GRot(), -ref->GPos()));
		sundir.unify();
	}
	for (hemisphere = idx = 0; hemisphere < 2; hemisphere++) {
		for (i = nlat-1; i >= 0; i--) { // move from poles towards equator	
			for (j = 0; j < nlng[i]; j++, idx++) {
#ifdef UNDEF
				if (spatch && (!(spatch[idx].flag & 1))) continue; // patch has no land component
#endif
				if (vis) dorender = vis[idx];
				else     dorender = true; // (acos (rpos & patchcnt[idx]) - patchrad[idx] < visrad);
				// FIX: vis should always exist
				if (dorender) {     // patch visible from camera
					if (ref) dcos = sundir & patchcnt[idx];
					else     dcos = -1;
					if (dcos < 0.0) { // nightside
						if (dcos > -0.2) {
							scale = -(float)(dcos/0.2);
							lightmat_dim.emissive.r = scale * lightmat.emissive.r;
							lightmat_dim.emissive.g = scale * lightmat.emissive.g;
							lightmat_dim.emissive.b = scale * lightmat.emissive.b;
							dev->SetMaterial (&lightmat_dim);
							modmat = true;
						} else if (modmat) {
							dev->SetMaterial (&lightmat);
							modmat = false;
						}
						D3DMath_MatrixMultiply (wtrans, wmat, trans[idx]);
						dVERIFY (dev->SetTransform (D3DTRANSFORMSTATE_WORLD, &wtrans), "LPDIRECT3DDEVICE7::SetTransform failed");
						dVERIFY (dev->SetTexture (0, ntex[idx]), "LPDIRECT3DDEVICE7::SetTexture failed");
						dev->DrawIndexedPrimitiveVB (D3DPT_TRIANGLELIST, vbpatch[i].vb, 0, vbpatch[i].nv, vbpatch[i].idx, vbpatch[i].ni, 0);
					}
				}
			}
		}
	}
	dVERIFY (dev->SetMaterial (&pmat), "LPDIRECT3DDEVICE7::SetMaterial failed");
	dVERIFY (dev->SetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, FALSE), "LPDIRECT3DDEVICE7::SetRenderState failed");
#ifdef MICROLIGHTTEX
	dev->SetTextureStageState (1, D3DTSS_ALPHAOP, D3DTOP_DISABLE);
	dev->SetTextureStageState (1, D3DTSS_COLOROP, D3DTOP_DISABLE);
	dev->SetTexture (1, 0);
#endif
}

void PatchManager::RenderSimple (LPDIRECT3DDEVICE7 dev, D3DMATRIX &wmat, double visrad, bool addbkg)
{
	dev->SetTransform (D3DTRANSFORMSTATE_WORLD, &wmat);

	if (addbkg) {
		dev->SetTextureStageState (1, D3DTSS_COLOROP, D3DTOP_ADD);
		dev->SetTextureStageState (1, D3DTSS_COLORARG1, D3DTA_CURRENT);
		dev->SetTextureStageState (1, D3DTSS_COLORARG2, D3DTA_TFACTOR);
		Vector bgc = g_pOrbiter->GetInlineGraphicsClient()->GetScene()->BGcol();
		dev->SetRenderState (D3DRENDERSTATE_TEXTUREFACTOR, D3DRGBA(bgc.x, bgc.y, bgc.z, 1));
	}

	for (int idx = 0; idx < npatch; idx++) {
		dVERIFY (dev->SetTexture (0, tex ? tex[idx] : 0), "LPDIRECT3DDEVICE7::SetTexture failed");
		dev->DrawIndexedPrimitiveVB (D3DPT_TRIANGLELIST, vbpatch[idx].vb, 0, vbpatch[idx].nv, vbpatch[idx].idx, vbpatch[idx].ni, 0);
	}

	if (addbkg)
		dev->SetTextureStageState (1, D3DTSS_COLOROP, D3DTOP_DISABLE);
}

void PatchManager::SetupPatchBand (int ilat, D3DMATRIX *trans, Vector *pcnt, double *prad)
{
	double lng1, lng2, slng1, clng1, slng2, clng2;
	double lat1, lat2, slat1, clat1, slat2, clat2;
	Vector crnr[4];
	int i, ncorner, c, nofs = 0, sofs, nl = nlng[ilat];
	for (i = nlat-1; i > ilat; i--) nofs += nlng[i];
	sofs = nofs + npatch/2;

	D3DMATRIX south;
	VMAT_rotx (south, Pi);

	lat1 = Pi05 * (double)ilat/(double)nlat;
	lat2 = Pi05 * (double)(ilat+1)/(double)nlat;
	slat1 = sin(lat1), clat1 = cos(lat1);
	slat2 = sin(lat2), clat2 = cos(lat2);

	for (i = 0; i < nl; i++) {
		lng1  = Pi2 * (double)i/(double)nl + Pi; // subtract Pi so texture wraps at +- 180°
		lng2  = lng1 + Pi2/(double)nl;
		slng1 = sin(lng1), clng1 = cos(lng1);
		slng2 = sin(lng2), clng2 = cos(lng2);

		// set up patch transformation matrices
		VMAT_roty (trans[nofs+i], lng1);
		D3DMath_MatrixMultiply (trans[sofs+i], south, trans[nofs+i]);

		// set up visibility stuff
		crnr[0].Set (clat1*clng1, slat1, clat1*slng1);
		crnr[1].Set (clat1*clng2, slat1, clat1*slng2);
		crnr[2].Set (clat2*clng1, slat2, clat2*slng1);
		crnr[3].Set (clat2*clng2, slat2, clat2*slng2);
		ncorner = (ilat == nlat-1 ? 3 : 4);
		pcnt[nofs+i].Set (0,0,0);
		for (c = 0; c < ncorner; c++) pcnt[nofs+i] += crnr[c] / (double)ncorner;
		for (c = 0, prad[nofs+i] = 0.0; c < ncorner; c++) {
			double cangle = acos (pcnt[nofs+i] & crnr[c]);
			if (cangle > prad[nofs+i]) prad[nofs+i] = cangle;
		}
		pcnt[sofs+i].Set (pcnt[nofs+i].x, -pcnt[nofs+i].y, -pcnt[nofs+i].z);
		prad[sofs+i] = prad[nofs+i];
	}
}

void PatchManager::SetupPatchBands (D3DMATRIX *trans, Vector *pcnt, double *prad)
{
	for (int i = 0; i < nlat; i++)
		SetupPatchBand (i, trans, pcnt, prad);
}

void PatchManager::CreateDeviceObjects (LPDIRECT3D7 d3d, LPDIRECT3DDEVICE7 dev)
{
	g_pOrbiter->OutputLoadStatus ("Creating surface patches", 1);
	
	static D3DVERTEXBUFFERDESC bbvbd = 
	{ sizeof(D3DVERTEXBUFFERDESC), D3DVBCAPS_SYSTEMMEMORY, D3DFVF_XYZRHW, 8 };
	d3d->CreateVertexBuffer (&bbvbd, &bbtarget, 0);

#ifdef UNDEF
	TileManagerOld::CreateDeviceObjects (d3d, dev);
#endif

	D3DVIEWPORT7 vp;
	dev->GetViewport (&vp);
	vpX0 = vp.dwX, vpX1 = vpX0 + vp.dwWidth;
	vpY0 = vp.dwY, vpY1 = vpY0 + vp.dwHeight;

	FILE *f;
#ifdef MICROLIGHTTEX
	if (f = g_pOrbiter->OpenTextureFile ("lights1", ".dds")) {
		g_texmanager2->ReadTexture (f, &lightstruct1);
		fclose (f);
	} else
		lightstruct1 = 0;
#endif

	if (f = g_pOrbiter->OpenTextureFile ("cloud1", ".dds")) {
		g_texmanager2->ReadTexture (f, &cloudstruct);
		fclose (f);
	} else
		cloudstruct = 0;
}

void PatchManager::RestoreDeviceObjects (LPDIRECT3D7 d3d, LPDIRECT3DDEVICE7 dev)
{
	if (!(VB_MemFlag & D3DVBCAPS_SYSTEMMEMORY)) {
		DestroyDeviceObjects ();
		CreateDeviceObjects (d3d, dev);
	}
}

void PatchManager::DestroyDeviceObjects ()
{
	bbtarget->Release();
	bbtarget = 0;

#ifdef UNDEF
	TileManagerOld::DestroyDeviceObjects ();
#endif

#ifdef MICROLIGHTTEX
	if (lightstruct1) {
		lightstruct1->Release();
		lightstruct1 = 0;
	}
#endif
	if (cloudstruct) {
		cloudstruct->Release();
		cloudstruct = 0;
	}
}

// static objects
DWORD PatchManager::vpX0, PatchManager::vpX1, PatchManager::vpY0, PatchManager::vpY1;
LPDIRECTDRAWSURFACE7 PatchManager::lightstruct1 = 0;
LPDIRECTDRAWSURFACE7 PatchManager::cloudstruct = 0;

// =======================================================================
// Class PatchManager1

PatchManager1::PatchManager1 (char *_name, LPDIRECTDRAWSURFACE7 *_tex, LPDIRECTDRAWSURFACE7 *_ntex)
: PatchManager (_name, 'A', 1, 0, 0, &PATCH_TPL_1, 0, 0, 0, _tex, _ntex)
{}

void PatchManager1::Render (LPDIRECT3DDEVICE7 dev, D3DMATRIX &wmat, double visrad, bool addbkg)
{
	PatchManager::RenderSimple (dev, wmat, visrad, addbkg);
}

// =======================================================================
// Class PatchManager2

PatchManager2::PatchManager2 (char *_name, LPDIRECTDRAWSURFACE7 *_tex, LPDIRECTDRAWSURFACE7 *_ntex)
: PatchManager (_name, 'B', 1, 0, 0, &PATCH_TPL_2, 0, 0, 0, _tex, _ntex)
{}

void PatchManager2::Render (LPDIRECT3DDEVICE7 dev, D3DMATRIX &wmat, double visrad, bool addbkg)
{
	PatchManager::RenderSimple (dev, wmat, visrad, addbkg);
}

// =======================================================================
// Class PatchManager3

PatchManager3::PatchManager3 (char *_name, LPDIRECTDRAWSURFACE7 *_tex, LPDIRECTDRAWSURFACE7 *_ntex)
: PatchManager (_name, 'C', 1, 0, 0, &PATCH_TPL_3, 0, 0, 0, _tex, _ntex)
{}

void PatchManager3::Render (LPDIRECT3DDEVICE7 dev, D3DMATRIX &wmat, double visrad, bool addbkg)
{
	PatchManager::RenderSimple (dev, wmat, visrad, addbkg);
}

// =======================================================================
// Class PatchManager4

PatchManager4::PatchManager4 (char *_name, LPDIRECTDRAWSURFACE7 *_tex, LPDIRECTDRAWSURFACE7 *_ntex)
: PatchManager (_name, 'D', 2, 0, 0, PATCH_TPL_4, 0, 0, 0, _tex, _ntex)
{}

void PatchManager4::Render (LPDIRECT3DDEVICE7 dev, D3DMATRIX &wmat, double visrad, bool addbkg)
{
	PatchManager::RenderSimple (dev, wmat, visrad, addbkg);
}

// =======================================================================
// Class PatchManager5

bool      PatchManager5::needsetup = true;
D3DMATRIX PatchManager5::TRANS[8];
Vector    PatchManager5::PATCHCNT[8];
double    PatchManager5::PATCHRAD[8];

PatchManager5::PatchManager5 (char *_name, LPDIRECTDRAWSURFACE7 *_tex, LPDIRECTDRAWSURFACE7 *_ntex)
: PatchManager (_name, 'F', 8, 1, NLNG5, &PATCH_TPL_5, TRANS, PATCHCNT, PATCHRAD, _tex, _ntex)
{
	if (needsetup) {
		SetupPatchBands (TRANS, PATCHCNT, PATCHRAD);
		needsetup = false;
	}
}

// =======================================================================
// Class PatchManager6

bool      PatchManager6::needsetup = true;
D3DMATRIX PatchManager6::TRANS[24];
Vector    PatchManager6::PATCHCNT[24];
double    PatchManager6::PATCHRAD[24];

PatchManager6::PatchManager6 (char *_name, LPDIRECTDRAWSURFACE7 *_tex, LPDIRECTDRAWSURFACE7 *_ntex)
: PatchManager (_name, 'H', 24, 2, NLNG6, PATCH_TPL_6, TRANS, PATCHCNT, PATCHRAD, _tex, _ntex)
{
	if (needsetup) {
		SetupPatchBands (TRANS, PATCHCNT, PATCHRAD);
		needsetup = false;
	}
}

// =======================================================================
// Class PatchManager7

bool      PatchManager7::needsetup = true;
D3DMATRIX PatchManager7::TRANS[100];
Vector    PatchManager7::PATCHCNT[100];
double    PatchManager7::PATCHRAD[100];

PatchManager7::PatchManager7 (char *_name, LPDIRECTDRAWSURFACE7 *_tex, LPDIRECTDRAWSURFACE7 *_ntex)
: PatchManager (_name, 'J', 100, 4, NLNG7, PATCH_TPL_7, TRANS, PATCHCNT, PATCHRAD, _tex, _ntex)
{
	if (needsetup) {
		SetupPatchBands (TRANS, PATCHCNT, PATCHRAD);
		needsetup = false;
	}
}

// =======================================================================
// Class PatchManager8

bool      PatchManager8::needsetup = true;
D3DMATRIX PatchManager8::TRANS[364];
Vector    PatchManager8::PATCHCNT[364];
double    PatchManager8::PATCHRAD[364];

PatchManager8::PatchManager8 (char *_name, LPDIRECTDRAWSURFACE7 *_tex, LPDIRECTDRAWSURFACE7 *_ntex,
	bool *_vis)
	: PatchManager (_name, 'K', 364, 8, NLNG8, PATCH_TPL_8, TRANS, PATCHCNT, PATCHRAD, _tex, _ntex)
{
	if (needsetup) {
		SetupPatchBands (TRANS, PATCHCNT, PATCHRAD);
		needsetup = false;
	}
	vis = _vis;
}

// =======================================================================
// Local utility functions

// =======================================================================
// CreateSphere()   (from boids example)
// Create a spherical mesh of radius 1 and resolution defined by nrings
// Below is a list of #vertices and #indices against nrings:
//
// nrings  nvtx   nidx   (nidx = 12 nrings^2)
//   4       38    192
//   6       80    432
//   8      138    768
//  12      302   1728
//  16      530   3072
//  20      822   4800
//  24     1178   6912

void CreateSphere (LPDIRECT3D7 d3d, LPDIRECT3DDEVICE7 dev, VBMESH &mesh, DWORD nrings, bool hemisphere, int which_half, int texres)
{
	// Allocate memory for the vertices and indices
	DWORD       nVtx = hemisphere ? nrings*(nrings+1)+2 : nrings*(2*nrings+1)+2;
	DWORD       nIdx = hemisphere ? 6*nrings*nrings : 12*nrings*nrings;
	VERTEX_2TEX* Vtx = new VERTEX_2TEX[nVtx]; TRACENEW
	WORD*        Idx = new WORD[nIdx]; TRACENEW

	// Counters
    WORD x, y, nvtx = 0, nidx = 0;
	VERTEX_2TEX *vtx = Vtx;
	WORD *idx = Idx;

	// Angle deltas for constructing the sphere's vertices
    FLOAT fDAng   = (FLOAT)Pi / nrings;
    FLOAT fDAngY0 = fDAng;
	DWORD x1 = (hemisphere ? nrings : nrings*2);
	DWORD x2 = x1+1;
	FLOAT du = 0.5f/(FLOAT)texres;
	FLOAT a  = (1.0f-2.0f*du)/(FLOAT)x1;

    // Make the middle of the sphere
    for (y = 0; y < nrings; y++) {
        FLOAT y0 = (FLOAT)cos(fDAngY0);
        FLOAT r0 = (FLOAT)sin(fDAngY0);
		FLOAT tv = fDAngY0/(FLOAT)Pi;

        for (x = 0; x < x2; x++) {
            FLOAT fDAngX0 = x*fDAng - (FLOAT)Pi;  // subtract Pi to wrap at +-180°
			if (hemisphere && which_half) fDAngX0 += (FLOAT)Pi;

			D3DVECTOR v( r0*(FLOAT)cos(fDAngX0), y0, r0*(FLOAT)sin(fDAngX0) );
			FLOAT tu = a*(FLOAT)x + du;
			//FLOAT tu = x/(FLOAT)x1;

            *vtx++ = VERTEX_2TEX (v, v, tu, tv, tu, tv);
			nvtx++;
        }
        fDAngY0 += fDAng;
    }

    for (y = 0; y < nrings-1; y++) {
        for (x = 0; x < x1; x++) {
            *idx++ = (WORD)( (y+0)*x2 + (x+0) );
            *idx++ = (WORD)( (y+0)*x2 + (x+1) );
            *idx++ = (WORD)( (y+1)*x2 + (x+0) );
            *idx++ = (WORD)( (y+0)*x2 + (x+1) );
            *idx++ = (WORD)( (y+1)*x2 + (x+1) );
            *idx++ = (WORD)( (y+1)*x2 + (x+0) ); 
			nidx += 6;
        }
    }
    // Make top and bottom
	D3DVECTOR vy (0, 1, 0);
	WORD wNorthVtx = nvtx;
    *vtx++ = VERTEX_2TEX ( vy, vy, 0.5f, 0.0f, 0.5f, 0.0f);
    nvtx++;
	WORD wSouthVtx = nvtx;
    *vtx++ = VERTEX_2TEX (-vy,-vy, 0.5f, 1.0f, 0.5f, 1.0f);
    nvtx++;

    for (x = 0; x < x1; x++) {
		WORD p1 = wSouthVtx;
		WORD p2 = (WORD)( (y)*x2 + (x+0) );
		WORD p3 = (WORD)( (y)*x2 + (x+1) );

        *idx++ = p1;
        *idx++ = p3;
        *idx++ = p2;
		nidx += 3;
    }

    for (x = 0; x < x1; x++) {
		WORD p1 = wNorthVtx;
		WORD p2 = (WORD)( (0)*x2 + (x+0) );
		WORD p3 = (WORD)( (0)*x2 + (x+1) );

        *idx++ = p1;
        *idx++ = p3;
        *idx++ = p2;
		nidx += 3;
    }

	D3DVERTEXBUFFERDESC vbd = 
	{ sizeof(D3DVERTEXBUFFERDESC), VB_MemFlag | D3DVBCAPS_WRITEONLY, FVF_2TEX, nVtx };
	LPVOID data;

	d3d->CreateVertexBuffer (&vbd, &mesh.vb, 0);
	LOGOUT_DDERR (mesh.vb->Lock (DDLOCK_WAIT | DDLOCK_WRITEONLY | DDLOCK_DISCARDCONTENTS, (LPVOID*)&data, NULL));
	memcpy (data, Vtx, nVtx*sizeof(VERTEX_2TEX));
	LOGOUT_DDERR (mesh.vb->Unlock());
	LOGOUT_DDERR (mesh.vb->Optimize (dev, 0));
	delete []Vtx;
	mesh.nv  = nVtx;
	mesh.idx = Idx;
	mesh.ni  = nIdx;
	mesh.bb = 0;
	mesh.vtx = 0;
}

// =======================================================================
// check existence of a node

static int Exist (Vector *node, int nnode, Vector p)
{
	const double eps = 1e-6;
	for (int i = 0; i < nnode; i++) {
		double dist2 = (p.x-node[i].x)*(p.x-node[i].x) +
					   (p.y-node[i].y)*(p.y-node[i].y) +
				 	   (p.z-node[i].z)*(p.z-node[i].z);
		if (dist2 < eps) return i;
	}
	return -1;
}

void CreateSpherePatch (LPDIRECT3D7 d3d, LPDIRECT3DDEVICE7 dev, VBMESH &mesh, int nlng, int nlat, int ilat, int res, int bseg,
						bool reduce, bool outside, bool store_vtx, bool shift_origin)
{
	const float c1 = 1.0f, c2 = 0.0f; // -1.0f/512.0f; // assumes 256x256 texture patches
	int i, j, nVtx, nIdx, nseg, n, nofs0, nofs1;
	double minlat, maxlat, lat, minlng, maxlng, lng;
	double slat, clat, slng, clng;
	WORD tmp;
	Vector pos, tpos;

	minlat = Pi05 * (double)ilat/(double)nlat;
	maxlat = Pi05 * (double)(ilat+1)/(double)nlat;
	minlng = 0;
	maxlng = Pi2/(double)nlng;
	if (bseg < 0 || ilat == nlat-1) bseg = (nlat-ilat)*res;

	// generate nodes
	nVtx = (bseg+1)*(res+1);
	if (reduce) nVtx -= ((res+1)*res)/2;
	VERTEX_2TEX *Vtx = new VERTEX_2TEX[nVtx]; TRACENEW

	// create transformation for bounding box
	// we define the local coordinates for the patch so that the x-axis points
	// from (minlng,minlat) corner to (maxlng,minlat) corner (origin is halfway between)
	// y-axis points from local origin to middle between (minlng,maxlat) and (maxlng,maxlat)
	// bounding box is created in this system and then transformed back to planet coords.
	double clat0 = cos(minlat), slat0 = sin(minlat);
	double clng0 = cos(minlng), slng0 = sin(minlng);
	double clat1 = cos(maxlat), slat1 = sin(maxlat);
	double clng1 = cos(maxlng), slng1 = sin(maxlng);
	Vector ex(clat0*clng1 - clat0*clng0, 0, clat0*slng1 - clat0*slng0); ex.unify();
	Vector ey(0.5*(clng0+clng1)*(clat1-clat0), slat1-slat0, 0.5*(slng0+slng1)*(clat1-clat0)); ey.unify();
	Vector ez(crossp (ey, ex));
	Matrix R(ex.x, ex.y, ex.z,  ey.x, ey.y, ey.z,  ez.x, ez.y, ez.z);
	Vector pref (0.5*(clat0*clng1 + clat0*clng0), slat0, 0.5*(clat0*slng1 + clat0*slng0)); // origin
	Vector tpmin, tpmax; 

	float dx, dy;
	if (shift_origin) {
		dx = (float)clat0;
		dy = (float)slat0;
	}

	for (i = n = 0; i <= res; i++) {  // loop over longitudinal strips
		lat = minlat + (maxlat-minlat) * (double)i/(double)res;
		slat = sin(lat), clat = cos(lat);
		nseg = (reduce ? bseg-i : bseg);
		for (j = 0; j <= nseg; j++) {
			lng = (nseg ? minlng + (maxlng-minlng) * (double)j/(double)nseg : 0.0);
			slng = sin(lng), clng = cos(lng);
			pos.Set (clat*clng, slat, clat*slng);
			tpos = mul (R, pos-pref);
			if (!n) {
				tpmin = tpos;
				tpmax = tpos;
			} else {
				if      (tpos.x < tpmin.x) tpmin.x = tpos.x;
			    else if (tpos.x > tpmax.x) tpmax.x = tpos.x;
				if      (tpos.y < tpmin.y) tpmin.y = tpos.y;
				else if (tpos.y > tpmax.y) tpmax.y = tpos.y;
				if      (tpos.z < tpmin.z) tpmin.z = tpos.z;
				else if (tpos.z > tpmax.z) tpmax.z = tpos.z;
			}

			Vtx[n].x = Vtx[n].nx = D3DVAL(pos.x);
			Vtx[n].y = Vtx[n].ny = D3DVAL(pos.y);
			Vtx[n].z = Vtx[n].nz = D3DVAL(pos.z);
			if (shift_origin)
				Vtx[n].x -= dx, Vtx[n].y -= dy;

			Vtx[n].tu0 = D3DVAL(nseg ? (c1*j)/nseg+c2 : 0.5f); // overlap to avoid seams
			Vtx[n].tv0 = D3DVAL((c1*(res-i))/res+c2);
			Vtx[n].tu1 = (nseg ? Vtx[n].tu0 * TEX2_MULTIPLIER : 0.5f);
			Vtx[n].tv1 = Vtx[n].tv0 * TEX2_MULTIPLIER;
			if (!outside) {
				Vtx[n].nx = -Vtx[n].nx;
				Vtx[n].ny = -Vtx[n].ny;
				Vtx[n].nz = -Vtx[n].nz;
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

	D3DVERTEXBUFFERDESC vbd = 
		{ sizeof(D3DVERTEXBUFFERDESC), VB_MemFlag | D3DVBCAPS_WRITEONLY, FVF_2TEX, (DWORD)nVtx };
	d3d->CreateVertexBuffer (&vbd, &mesh.vb, 0);
	LPVOID data;
	mesh.vb->Lock (DDLOCK_WAIT | DDLOCK_WRITEONLY | DDLOCK_DISCARDCONTENTS, (LPVOID*)&data, NULL);
	memcpy (data, Vtx, nVtx*sizeof(VERTEX_2TEX));
	mesh.vb->Unlock();
	mesh.vb->Optimize (dev, 0);

	if (store_vtx) {
		mesh.vtx = Vtx;
	} else {
		delete []Vtx;
		mesh.vtx = 0;
	}
	mesh.nv  = nVtx;
	mesh.idx = Idx;
	mesh.ni  = nIdx;

	if (shift_origin) {
		pref.x -= dx;
		pref.y -= dy;
	}

	// BEGIN OBSOLETE

	// set bounding box
	static D3DVERTEXBUFFERDESC bbvbd ={ sizeof(D3DVERTEXBUFFERDESC), D3DVBCAPS_SYSTEMMEMORY, D3DFVF_XYZ, 8 };
	VERTEX_XYZ *V;
	d3d->CreateVertexBuffer (&bbvbd, &mesh.bb, 0);
	mesh.bb->Lock (DDLOCK_WAIT | DDLOCK_WRITEONLY | DDLOCK_DISCARDCONTENTS, (LPVOID*)&V, NULL);

	// transform bounding box back to patch coordinates
	pos = tmul (R, Vector(tpmin.x, tpmin.y, tpmin.z)) + pref;
	V[0].x = D3DVAL(pos.x); V[0].y = D3DVAL(pos.y); V[0].z = D3DVAL(pos.z);
	pos = tmul (R, Vector(tpmax.x, tpmin.y, tpmin.z)) + pref;
	V[1].x = D3DVAL(pos.x); V[1].y = D3DVAL(pos.y); V[1].z = D3DVAL(pos.z);
	pos = tmul (R, Vector(tpmin.x, tpmax.y, tpmin.z)) + pref;
	V[2].x = D3DVAL(pos.x); V[2].y = D3DVAL(pos.y); V[2].z = D3DVAL(pos.z);
	pos = tmul (R, Vector(tpmax.x, tpmax.y, tpmin.z)) + pref;
	V[3].x = D3DVAL(pos.x); V[3].y = D3DVAL(pos.y); V[3].z = D3DVAL(pos.z);
	pos = tmul (R, Vector(tpmin.x, tpmin.y, tpmax.z)) + pref;
	V[4].x = D3DVAL(pos.x); V[4].y = D3DVAL(pos.y); V[4].z = D3DVAL(pos.z);
	pos = tmul (R, Vector(tpmax.x, tpmin.y, tpmax.z)) + pref;
	V[5].x = D3DVAL(pos.x); V[5].y = D3DVAL(pos.y); V[5].z = D3DVAL(pos.z);
	pos = tmul (R, Vector(tpmin.x, tpmax.y, tpmax.z)) + pref;
	V[6].x = D3DVAL(pos.x); V[6].y = D3DVAL(pos.y); V[6].z = D3DVAL(pos.z);
	pos = tmul (R, Vector(tpmax.x, tpmax.y, tpmax.z)) + pref;
	V[7].x = D3DVAL(pos.x); V[7].y = D3DVAL(pos.y); V[7].z = D3DVAL(pos.z);

	mesh.bb->Unlock ();

	// END OBSOLETE

	// set bounding box in main memory
	mesh.bbvtx = new VECTOR4[8];
	mesh.bbvtx[0] = MakeVECTOR4 (tmul (R, Vector(tpmin.x, tpmin.y, tpmin.z)) + pref);
	mesh.bbvtx[1] = MakeVECTOR4 (tmul (R, Vector(tpmax.x, tpmin.y, tpmin.z)) + pref);
	mesh.bbvtx[2] = MakeVECTOR4 (tmul (R, Vector(tpmin.x, tpmax.y, tpmin.z)) + pref);
	mesh.bbvtx[3] = MakeVECTOR4 (tmul (R, Vector(tpmax.x, tpmax.y, tpmin.z)) + pref);
	mesh.bbvtx[4] = MakeVECTOR4 (tmul (R, Vector(tpmin.x, tpmin.y, tpmax.z)) + pref);
	mesh.bbvtx[5] = MakeVECTOR4 (tmul (R, Vector(tpmax.x, tpmin.y, tpmax.z)) + pref);
	mesh.bbvtx[6] = MakeVECTOR4 (tmul (R, Vector(tpmin.x, tpmax.y, tpmax.z)) + pref);
	mesh.bbvtx[7] = MakeVECTOR4 (tmul (R, Vector(tpmax.x, tpmax.y, tpmax.z)) + pref);
}

void DestroyVBMesh (VBMESH &mesh)
{
	mesh.vb->Release();
	mesh.vb  = 0;
	mesh.nv  = 0;
	if (mesh.bb) {
		mesh.bb->Release();
		mesh.bb  = 0;
	}
	delete []mesh.idx;
	mesh.idx = 0;
	if (mesh.vtx) {
		delete []mesh.vtx;
		mesh.vtx = 0;
	}
	mesh.ni  = 0;
}

// =======================================================================
// =======================================================================

// Classes for rendering graded horizon haze

// =======================================================================
// Class HorizonManager

bool     HorizonManager::need_setup = true;
WORD     HorizonManager::Idx[HORIZON_NSEG*2+2];
struct   HorizonManager::HVERTEX HorizonManager::Vtx[HORIZON_NSEG*2];
DWORD    HorizonManager::nIdx = HORIZON_NSEG*2+2;
D3DVALUE HorizonManager::CosP[HORIZON_NSEG];
D3DVALUE HorizonManager::SinP[HORIZON_NSEG];

HorizonManager::HorizonManager (const Planet *_planet, const VPlanet *_vplanet)
{
	int i;

	planet  = _planet;
	vplanet = _vplanet;
	const ATMCONST *atmp = planet->AtmParams();
	if (atmp) {
		//basecol.Set (atmp->color0.x, atmp->color0.y, atmp->color0.z);
		basecol.Set (planet->AtmHazeColor());
		hralt = (float)(atmp->horizonalt / planet->Size());
		dens0 = (float)(min (1.0, atmp->horizonalt/64e3*planet->AtmHazeDensity()));
	} else {
		basecol.Set (1,1,1);
		hralt = 0.01f;
		dens0 = 1.0f;
	}
	hshift = (float)planet->AtmHazeShift();
	hscale = (float)(1.0-planet->AtmHazeRange());

	if (need_setup) {
		for (i = 0; i < HORIZON_NSEG; i++)
			Idx[i*2] = i*2+1, Idx[i*2+1] = i*2;
		Idx[i*2] = 1, Idx[i*2+1] = 0;

		for (i = 0; i < HORIZON_NSEG; i++) {
			Vtx[i*2].tu = Vtx[i*2+1].tu = (float)(i%2);
			Vtx[i*2].tv = 1.0f;
			Vtx[i*2+1].tv = 0.0f;
			double phi = (double)i/(double)HORIZON_NSEG * Pi2;
			CosP[i] = (float)cos(phi), SinP[i] = (float)sin(phi);
		}
		need_setup = false;
	}
}

void HorizonManager::Render (LPDIRECT3DDEVICE7 dev, D3DMATRIX &wmat, bool dual)
{
	D3DMATRIX imat, transm;

	Vector psun;
	int i, j;
	double phi, csun, alpha, colofs;
	float cosp, sinp, cost, sint, h1, h2, r1, r2, intr, intg, intb;

	D3DMath_MatrixInvert (imat, wmat);
	Vector rpos (imat._41, imat._42, imat._43);   // camera in local coords (planet radius = 1)
	double cdist = rpos.length();

	alpha = dens0 * min (1.0, (cdist-1.0)*200.0);
	if (!dual) alpha = 1.0-alpha;
	if (alpha <= 0.0) return;  // nothing to do
	alpha = min(alpha,1.0);

	Vector cpos (0,cdist,0);
	double id = 1.0 / max (cdist, 1.002);  // inverse camera distance; 1.001: hack to avoid horizon to creep too close
	double visrad = acos (id);             // aperture of visibility sector
	double sinv = sin(visrad);
	h1 = (float)id, h2 = h1 + (float)(hralt*id);
	r1 = (float)sinv, r2 = (1.0f+hralt)*r1;

	if (!dual) { // pull lower horizon edge below surface to avoid problems with elevations < 0
		h1 *= vplanet->prm.horizon_minrad;
		r1 *= vplanet->prm.horizon_minrad;
	}

	if (hshift) {
		double calt;
		if (!planet->CloudParam (calt) || cdist-1.0 > calt/planet->Size()) {
			float dr = (float)(hshift*sinv);
			float dh = (float)(hshift*id);
			h1 += dh, h2 += dh;
			r1 += dr, r2 += dr;
		}
	}

	float dens = (float)max (1.0, 1.4 - 0.3/hralt*(cdist-1.0)); // saturate haze colour at low altitudes
	if (dual) dens *= (float)(0.5 + 0.5/cdist);                 // scale down intensity at large distances

	rpos.unify(); cost = (float)rpos.y, sint = (float)sqrt (1.0-cost*cost);
	phi = atan2 (rpos.z, rpos.x), cosp = (float)cos(phi), sinp = (float)sin(phi);
	D3DMATRIX rmat (cost*cosp, -sint, cost*sinp, 0,
		            sint*cosp,  cost, sint*sinp, 0,
					-sinp,      0,    cosp,      0,
					0,          0,    0,         1);
	D3DMath_MatrixMultiply (transm, wmat, rmat);

	Matrix rrmat (cost*cosp, -sint, cost*sinp,
		          sint*cosp,  cost, sint*sinp,
				  -sinp,      0,    cosp     );
	psun.Set (tmul (planet->GRot(), -planet->GPos())); // sun in planet coords
	psun.Set (mul (rrmat, psun)); // sun in camera-relative horizon coords
	Vector cs (psun-cpos); cs.unify(); // camera->sun
	psun.unify();
	float psunx = (float)psun.x, psuny = (float)psun.y, psunz = (float)psun.z;

	colofs = (dual ? 0.4 : 0.3);

	dev->SetTextureStageState (0, D3DTSS_ALPHAOP, D3DTOP_MODULATE);
	dev->SetTransform (D3DTRANSFORMSTATE_WORLD, &transm);
	dev->SetTexture (0, g_pOrbiter->GetInlineGraphicsClient()->GetScene()->HazeTex());

	dev->SetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, TRUE);
	dev->SetRenderState (D3DRENDERSTATE_LIGHTING, FALSE);
	dev->SetTextureStageState (0, D3DTSS_ADDRESS, D3DTADDRESS_CLAMP);

	for (i = j = 0; i < HORIZON_NSEG; i++) {
		Vector hp (Vtx[j].x = r1*CosP[i], Vtx[j].y = h1, Vtx[j].z = r1*SinP[i]);
		csun = dotp (hp, psun);
		Vector cp(hp-cpos); cp.unify();
		double colsh = 0.5*(dotp (cp,cs) + 1.0);

		// compose a colourful sunset
		double maxred   = colofs-0.18*colsh,  minred   = maxred-0.4;
		double maxgreen = colofs-0.1*colsh,  mingreen = maxgreen-0.4;
		double maxblue  = colofs/*+0.0*colsh*/,  minblue  = maxblue-0.4;
		if      (csun > maxred) intr = 1.0f;
		else if (csun < minred) intr = 0.0f;
		else                    intr = (float)((csun-minred)*2.5);
		if      (csun > maxgreen) intg = 1.0f;
		else if (csun < mingreen) intg = 0.0f;
		else                      intg = (float)((csun-mingreen)*2.5);
		if      (csun > maxblue) intb = 1.0f;
		else if (csun < minblue) intb = 0.0f;
		else                     intb = (float)((csun-minblue)*2.5);
		D3DCOLOR col = D3DRGBA (intr*min(1.0,dens*basecol.x), intg*min(1.0,dens*basecol.y), intb*min(1.0,dens*basecol.z), alpha);

		Vtx[j].dcol = col;
		j++;
		Vtx[j].x = r2*CosP[i];
		Vtx[j].y = h2;
		Vtx[j].z = r2*SinP[i];
		Vtx[j].dcol = col;
		j++;
	}
	dev->DrawIndexedPrimitive (D3DPT_TRIANGLESTRIP, D3DFVF_XYZ | D3DFVF_DIFFUSE |
		D3DFVF_TEX1 | D3DFVF_TEXCOORDSIZE2(0), Vtx, 2*HORIZON_NSEG, Idx, nIdx, 0);

	if (dual) {
		h2 = h1;
		r2 = hscale*r1*r1;

		for (i = j = 0; i < HORIZON_NSEG; i++) {
			j++;
			Vtx[j].x = r2*CosP[i];
			Vtx[j].y = h2;
			Vtx[j].z = r2*SinP[i];
			j++;
		}
		dev->SetRenderState (D3DRENDERSTATE_CULLMODE, D3DCULL_CW);
		dev->DrawIndexedPrimitive (D3DPT_TRIANGLESTRIP, D3DFVF_XYZ | D3DFVF_DIFFUSE |
			D3DFVF_TEX1 | D3DFVF_TEXCOORDSIZE2(0), Vtx, 2*HORIZON_NSEG, Idx, nIdx, 0);
		dev->SetRenderState (D3DRENDERSTATE_CULLMODE, D3DCULL_CCW);
	}

	dev->SetTextureStageState (0, D3DTSS_ADDRESS, D3DTADDRESS_WRAP);
	dev->SetRenderState (D3DRENDERSTATE_LIGHTING, TRUE);
	dev->SetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, FALSE);
	dev->SetTextureStageState (0, D3DTSS_ALPHAOP, D3DTOP_SELECTARG1);
}

// =======================================================================
// =======================================================================

// Classes for rendering planetary ring systems at different resolutions

// =======================================================================
// Class RingManager

RingManager::RingManager (const char *_name, LPDIRECTDRAWSURFACE7 *_tex)
: tex(_tex)
{
}

RingManager::~RingManager ()
{
}

void RingManager::Render (LPDIRECT3DDEVICE7 dev, D3DMATRIX &wmat)
{

}

// =======================================================================
// Class RingManager8

RingManager8::RingManager8 (const char *_name, float irad, float orad, LPDIRECTDRAWSURFACE7 *_tex)
: RingManager (_name, _tex)
{
	CreateRing (mesh, irad, orad, 8);
}

void RingManager8::Render (LPDIRECT3DDEVICE7 dev, D3DMATRIX &wmat)
{
	dev->SetTransform (D3DTRANSFORMSTATE_WORLD, &wmat);
	dev->SetTexture (0, tex ? tex[0] : 0);
	mesh.Render (dev);
}

// =======================================================================
// Class RingManager12

RingManager12::RingManager12 (const char *_name, float irad, float orad, LPDIRECTDRAWSURFACE7 *_tex)
: RingManager (_name, _tex)
{
	CreateRing (mesh, irad, orad, 12);
}

void RingManager12::Render (LPDIRECT3DDEVICE7 dev, D3DMATRIX &wmat)
{
	dev->SetTransform (D3DTRANSFORMSTATE_WORLD, &wmat);
	dev->SetTexture (0, tex ? tex[0] : 0);
	mesh.Render (dev);
}

// =======================================================================
// Class RingManager16

RingManager16::RingManager16 (const char *_name, float irad, float orad, LPDIRECTDRAWSURFACE7 *_tex)
: RingManager (_name, _tex)
{
	CreateRing (mesh, irad, orad, 16);
}

void RingManager16::Render (LPDIRECT3DDEVICE7 dev, D3DMATRIX &wmat)
{
	dev->SetTransform (D3DTRANSFORMSTATE_WORLD, &wmat);
	dev->SetTexture (0, tex ? tex[0] : 0);
	mesh.Render (dev);
}

// =======================================================================
// CreateRing
// Creates mesh for rendering planetary ring system. Creates a ring
// with nsect quadrilaterals. Smoothing the corners of the mesh is
// left to texture transparency. Nsect should be an even number.
// Disc is in xz-plane centered at origin facing up. Size is such that
// a ring of inner radius irad (>=1) and outer radius orad (>irad)
// can be rendered on it.

static void CreateRing (Mesh &mesh, float irad, float orad, int nsect)
{
	int i, j, nVtx, nIdx;
	NTVERTEX  *Vtx = new NTVERTEX[nVtx = 2*nsect]; TRACENEW
	WORD      *Idx = new WORD[nIdx = 6*nsect]; TRACENEW

	double alpha = Pi/(double)nsect;
	float nrad = orad/(float)cos(alpha); // distance for outer nodes
	//float fac = (nrad-irad)*(float)sin(alpha)/(2.0*sqrt(nrad*nrad-orad*orad));
	float fo = 0.5f*(1.0f-orad/nrad);
	float fi = 0.5f*(1.0f-irad/nrad);

	for (i = j = 0; i < nsect; i++) {
		double phi = i*2.0*alpha;
		float cosp = (float)cos(phi), sinp = (float)sin(phi);
		Vtx[i*2].x = nrad*cosp;  Vtx[i*2+1].x = irad*cosp;
		Vtx[i*2].z = nrad*sinp;  Vtx[i*2+1].z = irad*sinp;
		Vtx[i*2].y = Vtx[i*2+1].y = 0.0;
		Vtx[i*2].nx = Vtx[i*2+1].nx = Vtx[i*2].nz = Vtx[i*2+1].nz = 0.0;
		Vtx[i*2].ny = Vtx[i*2+1].ny = 1.0;
		if (!(i&1)) Vtx[i*2].tu = fo,  Vtx[i*2+1].tu = fi;  //fac;
		else        Vtx[i*2].tu = 1.0f-fo,  Vtx[i*2+1].tu = 1.0f-fi; //1.0f-fac;
		Vtx[i*2].tv = 0.0f, Vtx[i*2+1].tv = 1.0f;

		Idx[j++] = i*2;
		Idx[j++] = i*2+1;
		Idx[j++] = (i*2+2) % (2*nsect);
		Idx[j++] = (i*2+3) % (2*nsect);
		Idx[j++] = (i*2+2) % (2*nsect);
		Idx[j++] = i*2+1;
	}

	mesh.Clear();
	mesh.AddGroup (Vtx, nVtx, Idx, nIdx, SPEC_DEFAULT, SPEC_INHERIT);
	mesh.Setup ();
}