// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#include "OGraphics.h"
#include "Orbiter.h"
#include "Vvessel.h"
#include "Config.h"
#include "Mesh.h"
#include "Camera.h"
#include "Pane.h"
#include "VCockpit.h"
#include "Scene.h"
#include "Planet.h"
#include "Psys.h"
#include <fstream>
#include <stdio.h>
#include <stdlib.h>
#include "D3dmath.h"
#include "Log.h"
#include "Astro.h"
#include "Util.h"

#define EXHAUST_SCALEALPHA
//#define EXHAUST_SCALESIZE

extern Orbiter *g_pOrbiter;
extern char DBG_MSG[256];
extern PlanetarySystem *g_psys;
extern Camera *g_camera;
extern Vessel *g_focusobj;
extern Pane *g_pane;
extern TimeData td;
extern bool g_bShowGrapple;

static D3DMATERIAL7 engmat = { // emissive material for engine exhaust
	{0,0,0,1},
	{0,0,0,1},
	{0,0,0,1},
	{1,1,1,1},
	0.0
};

// =======================================================================
// Class VVessel

LPDIRECTDRAWSURFACE7 VVessel::mfdsurf = 0;

VVessel::VVessel (const Vessel *_vessel)
: VObject (_vessel), vessel(_vessel)
{
	mesh_crc = 0;
	nmesh = 0;
	renderpix = false;
	bLocalLight = false;
	bExtRenderPass = false;
	tCheckLight = td.SimT0-1.0;
	lght = *scene->GetLight();

	// load the meshes
	RegisterMeshes();

	// reset the visual animation states to the default (mesh file) states
	if (nanim = vessel->nanimseq) {
		vanim = new double[nanim]; TRACENEW
		for (int i = 0; i < nanim; i++)
			vanim[i] = vessel->animseq[i].defstate;
	}
	if (vessel->nanim) {
		vanim_state = new double[vessel->nanim]; TRACENEW
		for (UINT i = 0; i < vessel->nanim; i++)
			vanim_state[i] = vessel->anim[i].defstate;
	} else vanim_state = 0;

	// update positions and transformation matrices
	// re-apply the logical animation states to the visual
	Update (true, true);
}

VVessel::~VVessel ()
{
	UINT i;
	if (vessel->nanim) {
		for (i = 0; i < vessel->nanim; i++)
			Animate2 (i, vessel->anim[i].defstate);
		delete []vanim_state;
	}
	for (i = 0; i < nmesh; i++)
		delete meshlist[i].mesh;
	if (nmesh) delete []meshlist;
	if (nanim) delete []vanim;

}

void VVessel::CreateDeviceObjects (LPDIRECT3DDEVICE7 dev)
{
	const DWORD texsize = g_pOrbiter->Cfg()->CfgInstrumentPrm.PanelMFDHUDSize;

	if (mfdsurf) mfdsurf->Release();
	mfdsurf = (LPDIRECTDRAWSURFACE7)g_pOrbiter->GetInlineGraphicsClient()->clbkCreateTexture (texsize, texsize);
}

void VVessel::DestroyDeviceObjects ()
{
	if (mfdsurf) {
		mfdsurf->Release();
		mfdsurf = 0;
	}
}

void VVessel::clbkEvent (DWORD msg, DWORD_PTR content)
{
	switch (msg) {
	case EVENT_VESSEL_INSMESH: // insert mesh
		InsertMesh ((UINT)content);
		ResetAnimations ((UINT)content);
		UpdateAnimations ((UINT)content);
		break;
	case EVENT_VESSEL_DELMESH: // mesh deleted
		if (content == (UINT)-1) { // clear all meshes
			UnregisterMeshes ();
			//ResetAnimations (); // instead, we should really delete the animations here!
		} else {
			DelMesh (content);
		}
		break;
	case EVENT_VESSEL_MESHVISMODE: // visibility mode changed
		ScanMeshCaps ();
		break;
	case EVENT_VESSEL_RESETANIM: // reset animations
		ResetAnimations ();
		break;
	case EVENT_VESSEL_CLEARANIM: // clear animations
		ClearAnimations (content?true:false);
		break;
	case EVENT_VESSEL_DELANIM: // remove an animation
		ResetAnimation (content);
		break;
	case EVENT_VESSEL_NEWANIM:
		CreateAnimation (content);
		break;
	case EVENT_VESSEL_MESHOFS:
		ShiftMesh (content);
		break;
	}
}

void VVessel::RegisterMeshes ()
{
	UINT i;
	nmesh = vessel->nmesh;
	if (nmesh) {
		meshlist = new MeshList[nmesh]; TRACENEW
		for (i = 0; i < nmesh; i++) {
			if (vessel->meshlist[i]) {
				meshlist[i].mesh = new Mesh; TRACENEW
				if (vessel->meshlist[i]->hMesh) { // copy from preloaded mesh
					meshlist[i].mesh->Set (*(Mesh*)vessel->meshlist[i]->hMesh);
				} else {                          // load from file
					if (!LoadMesh (vessel->meshlist[i]->meshname, *meshlist[i].mesh)) {
						delete meshlist[i].mesh; meshlist[i].mesh = 0;
					}
				}
				meshlist[i].crc = vessel->meshlist[i]->crc;
				meshlist[i].shifted = (vessel->meshlist[i]->meshofs.x ||
									   vessel->meshlist[i]->meshofs.y ||
									   vessel->meshlist[i]->meshofs.z);
			} else { // "no mesh"
				meshlist[i].mesh = 0;
				meshlist[i].crc = 0;
				meshlist[i].shifted = false;
			}
		}
	} else
		meshlist = 0;

	mesh_crc = vessel->mesh_crc;
	ScanMeshCaps ();
}

void VVessel::InsertMesh (UINT idx)
{
	DWORD i;
	if (idx >= nmesh) {            // append a new entry
		MeshList *tmp = new MeshList[idx+1]; TRACENEW
		if (nmesh) {
			memcpy (tmp, meshlist, nmesh*sizeof(MeshList));
			delete []meshlist;
		}
		meshlist = tmp;
		for (i = nmesh; i < idx; i++) { // zero any intervening entries
			meshlist[i].mesh = 0;
			meshlist[i].crc = 0;
			meshlist[i].shifted = false;
		}
		nmesh = idx+1;
	} else if (meshlist[idx].mesh) { // replace existing entry
		delete meshlist[idx].mesh;
	}
	if (vessel->meshlist[idx]) {
		meshlist[idx].mesh = new Mesh; TRACENEW
		if (vessel->meshlist[idx]->hMesh) // copy from preloaded mesh
			meshlist[idx].mesh->Set (*(Mesh*)vessel->meshlist[idx]->hMesh);
		else                              // load from file
			if (!LoadMesh (vessel->meshlist[idx]->meshname, *meshlist[idx].mesh)) {
				delete meshlist[idx].mesh; meshlist[idx].mesh = 0;
			}
		meshlist[idx].crc = vessel->meshlist[idx]->crc;
		meshlist[idx].shifted = (vessel->meshlist[idx]->meshofs.x ||
								 vessel->meshlist[idx]->meshofs.y ||
								 vessel->meshlist[idx]->meshofs.z);
	} else { // "no mesh"
		meshlist[idx].mesh = 0;
		meshlist[idx].crc = 0;
		meshlist[idx].shifted = false;
	}
	mesh_crc = vessel->mesh_crc;

	ScanMeshCaps();
}

void VVessel::DelMesh (UINT idx)
{
	if (idx >= nmesh) return; // index out of range
	if (!meshlist[idx].mesh) return; // mesh already deleted

	delete meshlist[idx].mesh;
	meshlist[idx].mesh = 0;
	meshlist[idx].crc = 0;
	meshlist[idx].shifted = false;
}

MESHHANDLE VVessel::GetMesh (UINT idx)
{
	return (idx < nmesh ? meshlist[idx].mesh : NULL);
}

void VVessel::ShiftMesh (UINT idx)
{
	if (vessel->meshlist[idx]) {
		meshlist[idx].shifted = (vessel->meshlist[idx]->meshofs.x ||
								 vessel->meshlist[idx]->meshofs.y ||
								 vessel->meshlist[idx]->meshofs.z);
	}
}

void VVessel::UnregisterMeshes ()
{
	for (UINT i = 0; i < nmesh; i++)
		if (meshlist[i].mesh) delete meshlist[i].mesh;
	if (nmesh) delete []meshlist;
	nmesh = 0;
}

void VVessel::ScanMeshCaps ()
{
	UINT i;
	bExtRenderPass = false;

	for (i = 0; i < nmesh; i++) {
		if (!vessel->meshlist[i]) continue;
		if (vessel->meshlist[i]->vismode & MESHVIS_EXTPASS) {
			bExtRenderPass = true;
			break;
		}
	}
}

void VVessel::CreateAnimation (UINT idx)
{
	double *vanim_tmp = new double[idx+1]; TRACENEW
	memcpy (vanim_tmp, vanim_state, idx*sizeof(double));
	if (vanim_state) delete []vanim_state;
	vanim_state = vanim_tmp;
	vanim_state[idx] = vessel->anim[idx].defstate;
}

void VVessel::ResetAnimations (UINT mshidx)
{
	UINT i, j;
	if (mshidx == (UINT)-1) {
		for (i = 0; i < vessel->nanim; i++) {
			vanim_state[i] = vessel->anim[i].defstate;
		}
	} else {
		// reset all animations that contain components
		// referring to mesh 'mshidx'
		// Note: this can cause problems if an animations contains
		// components belonging to different meshes
		for (i = 0; i < vessel->nanim; i++) {
			ANIMATION &A = vessel->anim[i];
			for (j = 0; j < A.ncomp; j++) {
				ANIMATIONCOMP *AC = A.comp[j];
				if (AC->trans->mesh == mshidx) break;
			}
			if (j < A.ncomp) { // found one
				vanim_state[i] = vessel->anim[i].defstate;
			}
		}
	}
}

void VVessel::ResetAnimation (UINT an)
{
	Animate2 (an, vessel->anim[an].defstate);
	vanim_state[an] = vessel->anim[an].defstate;
}

void VVessel::ClearAnimations (bool reset)
{
	if (vessel->nanim) {
		if (reset) {
			for (UINT i = 0; i < vessel->nanim; i++)
				Animate2 (i, vessel->anim[i].defstate);
		}
		delete []vanim_state;
	}
	if (nanim) {
		delete []vanim;
		nanim = 0;
	}
}

bool VVessel::bRenderInternal () const
{
	for (UINT i = 0; i < nmesh; i++) {
		if (!vessel->meshlist[i]) continue;
		if (vessel->meshlist[i]->vismode & MESHVIS_COCKPIT) return true;
		if ((g_pane->GetPanelMode() == 3) && (vessel->meshlist[i]->vismode & MESHVIS_VC)) return true;
	}
	return false;
}

void VVessel::CheckResolution (double iar)
{
	renderpix = (cdist > 0.5 * g_camera->Apprad_dist (1.0, body->Size()));
}

void VVessel::Update (bool moving, bool force)
{
	VObject::Update (moving, force);
	UpdateAnimations ();

	if (td.SimT0 > tCheckLight)
		bLocalLight = ModLighting (&lght);
}

void VVessel::UpdateAnimations (UINT mshidx)
{
	double newstate;
	for (UINT i = 0; i < vessel->nanim; i++) {
		if (!vessel->anim[i].ncomp) continue;
		if (vanim_state[i] != (newstate = vessel->anim[i].state)) {
			Animate2 (i, newstate, mshidx);
			vanim_state[i] = newstate;
		}
	}

	// this animation method is obsolete
	for (int j = 0; j < nanim; j++)
		if (vanim[j] != vessel->animseq[j].state)
			Animate (j);
}

void VVessel::Timejump (bool moving)
{
	tCheckLight = td.SimT0-1.0;
	Update (moving, true);
}

void VVessel::Render (LPDIRECT3DDEVICE7 dev)
{
	Render (dev, false);
}

void VVessel::Render (LPDIRECT3DDEVICE7 dev, bool internalpass)
{
	oapi::GraphicsClient *gc = g_pOrbiter->GetGraphicsClient();

	bool rendercockpit = (vessel == g_focusobj && g_camera->IsInternal());
	// render this vessel from its cockpit view?
	bool needworld = true;
	// we are in the cockpit of the focus object

	if (bLocalLight)  // modify lighting
		dev->SetLight (0, &lght);

	if (renderpix) {

		RenderAsSpot (dev, bLocalLight ? &lght.dcvDiffuse : 0);

	} else {

		bool rendervc = (rendercockpit && g_pane->GetPanelMode() == 3);
		// are we rendering the cockpit interior of the current focus object?

		D3DVECTOR t_store = {mWorld._41, mWorld._42, mWorld._43};
		dev->SetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, TRUE);

		for (UINT i = 0; i < nmesh; i++) {
			if (!vessel->meshlist[i] || !meshlist[i].mesh) continue;
			WORD vismode = vessel->meshlist[i]->vismode;
			if (rendercockpit) {
				if (internalpass && (vismode & MESHVIS_EXTPASS)) continue;
				if (!(vismode & MESHVIS_COCKPIT)) {
					if ((!rendervc) || (!(vismode & MESHVIS_VC))) continue;
				}
			} else {
				if (!(vismode & MESHVIS_EXTERNAL)) continue; // skip this mesh in external view
				//if (!(vismode & (MESHVIS_EXTPASS|MESHVIS_EXTERNAL))) continue; // skip this mesh in external view
			}

			if (meshlist[i].shifted) {
				// encode the relative mesh offset into the world matrix
				// currently only allows for translations, not rotations
				VECTOR3 &ofs = vessel->meshlist[i]->meshofs;
				float ofsx = (float)ofs.x;
				float ofsy = (float)ofs.y;
				float ofsz = (float)ofs.z;
				mWorld._41 += ofsx*mWorld._11 + ofsy*mWorld._21 + ofsz*mWorld._31;
				mWorld._42 += ofsx*mWorld._12 + ofsy*mWorld._22 + ofsz*mWorld._32;
				mWorld._43 += ofsx*mWorld._13 + ofsy*mWorld._23 + ofsz*mWorld._33;
				needworld = true;
			}
			if (needworld) {
				dev->SetTransform (D3DTRANSFORMSTATE_WORLD, &mWorld);
				needworld = false;
			}

			if (rendervc) { // link MFD textures for rendering
				const VirtualCockpit *vc = g_pane->GetVC();
				if (vc) {
					for (int mfd = 0; mfd < MAXMFD; mfd++) {
						const VCMFDSPEC *spec = g_pane->GetVCMFDParams (mfd);
						if (spec && spec->nmesh == i) {
							meshlist[i].mesh->GetGroup(spec->ngroup)->TexIdx = TEXIDX_MFD0+mfd;
						}
					}
				}
			}

			meshlist[i].mesh->Render (dev);

			if (rendervc) {
				// render MFDs and HUD in virtual cockpit
				const VirtualCockpit *vc = g_pane->GetVC();
				if (vc) {
					// render HUD
					if (vc->GetHUDSurf() && g_pane->GetHUD()) {
						const VCHUDSPEC *hspec = vc->GetHUDParams ();
						if (hspec && hspec->nmesh == i) {
							gc->clbkBlt (mfdsurf, 0, 0, vc->GetHUDSurf());
							dev->SetTexture (0, mfdsurf);
							dev->SetRenderState (D3DRENDERSTATE_LIGHTING, FALSE);
							dev->SetRenderState (D3DRENDERSTATE_ZENABLE, FALSE);
							dev->SetRenderState (D3DRENDERSTATE_DESTBLEND, D3DBLEND_ONE);
							meshlist[i].mesh->RenderGroup (dev, hspec->ngroup, false);
							dev->SetRenderState (D3DRENDERSTATE_DESTBLEND, D3DBLEND_INVSRCALPHA);
							dev->SetRenderState (D3DRENDERSTATE_LIGHTING, TRUE);
							dev->SetRenderState (D3DRENDERSTATE_ZENABLE, TRUE);
						}
					}
				}
			}

			if (meshlist[i].shifted) {
				// restore world matrix
				mWorld._41 = t_store.x;
				mWorld._42 = t_store.y;
				mWorld._43 = t_store.z;
				needworld = true;
			}
		}
		dev->SetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, FALSE);
		if (needworld)
			dev->SetTransform (D3DTRANSFORMSTATE_WORLD, &mWorld);

		if (g_bShowGrapple) {
			if (fmod (td.SysT0, 1.0) < 0.5) {
				RenderAttachmentMarkers (dev, true);
				RenderAttachmentMarkers (dev, false);
			}
		}
	}

	if (bLocalLight)
		dev->SetLight (0, scene->GetLight());
}

void SetExhaustVertices (const VECTOR3 &edir, const VECTOR3 &cdir, const VECTOR3 &ref,
						 double lscale, double wscale, double lofs, POSTEXVERTEX *ev)
{
	// need to rotate the billboard so it faces the observer
	const float flarescale = 7.0;
	VECTOR3 sdir = unit (crossp (cdir, edir));
	VECTOR3 tdir = unit (crossp (cdir, sdir));
	D3DVALUE rx = (D3DVALUE)ref.x, ry = (D3DVALUE)ref.y, rz = (D3DVALUE)ref.z;
	if (lofs) rx += (D3DVALUE)(edir.x*lofs), ry += (D3DVALUE)(edir.y*lofs), rz += (D3DVALUE)(edir.z*lofs);
	D3DVALUE sx = (D3DVALUE)(sdir.x*wscale);
	D3DVALUE sy = (D3DVALUE)(sdir.y*wscale);
	D3DVALUE sz = (D3DVALUE)(sdir.z*wscale);
	D3DVALUE ex = (D3DVALUE)(edir.x*lscale);
	D3DVALUE ey = (D3DVALUE)(edir.y*lscale);
	D3DVALUE ez = (D3DVALUE)(edir.z*lscale);
	ev[1].x = (ev[0].x = rx + sx) + ex;
	ev[1].y = (ev[0].y = ry + sy) + ey;
	ev[1].z = (ev[0].z = rz + sz) + ez;
	ev[3].x = (ev[2].x = rx - sx) + ex;
	ev[3].y = (ev[2].y = ry - sy) + ey;
	ev[3].z = (ev[2].z = rz - sz) + ez;
	wscale *= flarescale, sx *= flarescale, sy *= flarescale, sz *= flarescale;
	D3DVALUE tx = (D3DVALUE)(tdir.x*wscale);
	D3DVALUE ty = (D3DVALUE)(tdir.y*wscale);
	D3DVALUE tz = (D3DVALUE)(tdir.z*wscale);
	ev[4].x = rx - sx + tx;   ev[5].x = rx + sx + tx;
	ev[4].y = ry - sy + ty;   ev[5].y = ry + sy + ty;
	ev[4].z = rz - sz + tz;   ev[5].z = rz + sz + tz;
	ev[6].x = rx - sx - tx;   ev[7].x = rx + sx - tx;
	ev[6].y = ry - sy - ty;   ev[7].y = ry + sy - ty;
	ev[6].z = rz - sz - tz;   ev[7].z = rz + sz - tz;
}

void SetReentryVertices (const VECTOR3 &edir, const VECTOR3 &cdir, const VECTOR3 &ref,
						 double lscale, double wscale, double llen, POSTEXVERTEX *ev)
{
	// need to rotate the billboard so it faces the observer
	const float flarescale = 1.0;
	const float lscale1 = (float)(lscale*llen * (1.0 + 0.1*rand()/(double)RAND_MAX));
	const float lscale2 = (float)(lscale * 0.125);
	VECTOR3 sdir = unit (crossp (cdir, edir));
	VECTOR3 tdir = unit (crossp (cdir, sdir));
	D3DVALUE rx = (D3DVALUE)ref.x, ry = (D3DVALUE)ref.y, rz = (D3DVALUE)ref.z;
	D3DVALUE sx = (D3DVALUE)(sdir.x*wscale);
	D3DVALUE sy = (D3DVALUE)(sdir.y*wscale);
	D3DVALUE sz = (D3DVALUE)(sdir.z*wscale);
	D3DVALUE ex = (D3DVALUE)(edir.x*lscale1);
	D3DVALUE ey = (D3DVALUE)(edir.y*lscale1);
	D3DVALUE ez = (D3DVALUE)(edir.z*lscale1);
	ev[1].x = (ev[0].x = rx + sx) + ex;		ev[0].x -= (float)edir.x*lscale2;
	ev[1].y = (ev[0].y = ry + sy) + ey;		ev[0].y -= (float)edir.y*lscale2;
	ev[1].z = (ev[0].z = rz + sz) + ez;		ev[0].z -= (float)edir.z*lscale2;
	ev[3].x = (ev[2].x = rx - sx) + ex;		ev[2].x -= (float)edir.x*lscale2;
	ev[3].y = (ev[2].y = ry - sy) + ey;		ev[2].y -= (float)edir.y*lscale2;
	ev[3].z = (ev[2].z = rz - sz) + ez;		ev[2].z -= (float)edir.z*lscale2;
	wscale *= flarescale, sx *= flarescale, sy *= flarescale, sz *= flarescale;
	D3DVALUE tx = (D3DVALUE)(tdir.x*wscale);
	D3DVALUE ty = (D3DVALUE)(tdir.y*wscale);
	D3DVALUE tz = (D3DVALUE)(tdir.z*wscale);
	ev[4].x = rx - sx + tx;   ev[5].x = rx + sx + tx;
	ev[4].y = ry - sy + ty;   ev[5].y = ry + sy + ty;
	ev[4].z = rz - sz + tz;   ev[5].z = rz + sz + tz;
	ev[6].x = rx - sx - tx;   ev[7].x = rx + sx - tx;
	ev[6].y = ry - sy - ty;   ev[7].y = ry + sy - ty;
	ev[6].z = rz - sz - tz;   ev[7].z = rz + sz - tz;
}

static POSTEXVERTEX ExhaustVtx[8] = {
	{0,0,0, 0.24f,0},
	{0,0,0, 0.24f,1},
	{0,0,0, 0.01f,0},
	{0,0,0, 0.01f,1},
	{0,0,0, 0.50390625f, 0.00390625f},
	{0,0,0, 0.99609375f, 0.00390625f},
	{0,0,0, 0.50390625f, 0.49609375f},
	{0,0,0, 0.99609375f, 0.49609375f}
};
static WORD ExhaustIdx[12] = {0,1,2, 3,2,1, 4,5,6, 7,6,5};

static POSTEXVERTEX ReentryVtx[8] = {
	{0,0,0, 0.49f,0.01f},
	{0,0,0, 0.49f,0.99f},
	{0,0,0, 0.01f,0.01f},
	{0,0,0, 0.01f,0.99f},
	{0,0,0, 0.50390625f, 0.00390625f},
	{0,0,0, 0.99609375f, 0.00390625f},
	{0,0,0, 0.50390625f, 0.49609375f},
	{0,0,0, 0.99609375f, 0.49609375f}
};
static WORD ReentryIdx[12] = {0,1,2, 3,2,1, 4,5,6, 7,6,5};

void VVessel::RenderBeacons (LPDIRECT3DDEVICE7 dev)
{
	if (vessel->nbeacon) {
		bool need_setup = true;
		DWORD doalpha;
		dev->GetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, &doalpha);
		for (DWORD i = 0; i < vessel->nbeacon; i++) {
			BEACONLIGHTSPEC *bs = vessel->beacon[i];
			if (bs->active) {
				if (bs->period &&
					(fmod (td.SimT0+bs->tofs, bs->period) > bs->duration))
					continue;
				double size = bs->size;
				if (cdist > 50.0) size *= pow (cdist/50.0, bs->falloff);
				Vector pos (MakeVector (*bs->pos));
				Vector col (MakeVector (*bs->col));
				if (need_setup) {
					dev->SetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, TRUE);
					dev->SetRenderState (D3DRENDERSTATE_ZWRITEENABLE, FALSE);
					dev->SetRenderState (D3DRENDERSTATE_ZBIAS, 5);
					need_setup = false;
				}
				RenderSpot (dev, &pos, (float)size, col, false, bs->shape);
			}
		}
		if (!need_setup) {
			if (!doalpha) dev->SetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, FALSE);
			dev->SetRenderState (D3DRENDERSTATE_ZWRITEENABLE, TRUE);
			dev->SetRenderState (D3DRENDERSTATE_ZBIAS, 0);
		}
	}
}

void VVessel::RenderExhaust (LPDIRECT3DDEVICE7 dev, LPDIRECTDRAWSURFACE7 defaulttex)
{
	DWORD i;
	double zsize, xsize;
	EXHAUSTSPEC *exhaust;
	static VECTOR3 cdir;
	bool need_setup = true;
	LPDIRECTDRAWSURFACE7 tex, oldtex = 0;

	for (i = 0; i < vessel->nexhaust; i++) {
		exhaust = vessel->exhaust[i];
		if (! *exhaust->level) continue; // nothing to do

		if (need_setup) { // initialise render state
			dev->SetRenderState (D3DRENDERSTATE_ZWRITEENABLE, FALSE);
			dev->SetMaterial (&engmat);
			dev->SetTransform (D3DTRANSFORMSTATE_WORLD, &mWorld);
			cdir = MakeVECTOR3 (tmul (body->GRot(), cpos));
#ifdef EXHAUST_SCALEALPHA
			dev->SetTextureStageState (0, D3DTSS_ALPHAOP, D3DTOP_MODULATE);
			dev->SetTextureStageState (0, D3DTSS_ALPHAARG2, D3DTA_TFACTOR);
#endif
			need_setup = false;
		}

		tex = (LPDIRECTDRAWSURFACE7)(exhaust->tex);
		if (!tex) tex = defaulttex;
		if (tex != oldtex) dev->SetTexture (0, oldtex = tex);

#ifdef EXHAUST_SCALESIZE
		xsize = sqrt (zsize = *exhaust->level);
#else
		xsize = zsize = 1.0;
#endif
#ifdef EXHAUST_SCALEALPHA
		const double irmax = 0.1/(double)RAND_MAX;
		double alpha = *exhaust->level;
		if (exhaust->modulate) alpha *= ((1.0 - exhaust->modulate)+(double)rand()* exhaust->modulate/(double)RAND_MAX);
		dev->SetRenderState (D3DRENDERSTATE_TEXTUREFACTOR, D3DRGBA(1,1,1,alpha));
#endif
		SetExhaustVertices (-(*exhaust->ldir), cdir, *exhaust->lpos,
			zsize * exhaust->lsize, xsize * exhaust->wsize, exhaust->lofs, ExhaustVtx);
		// note that the exhaust render direction is the negative of the
		// thruster force direction
		dev->DrawIndexedPrimitive (D3DPT_TRIANGLELIST, POSTEXVERTEXFLAG,
			ExhaustVtx, 8, ExhaustIdx, 12, 0);
	}
#ifdef EXHAUST_SCALEALPHA
	if (!need_setup) {
		dev->SetTextureStageState (0, D3DTSS_ALPHAOP, D3DTOP_SELECTARG1);
		dev->SetTextureStageState (0, D3DTSS_ALPHAARG2, D3DTA_CURRENT);
	}
#endif

	// render re-entry trail
	if (vessel->sp.is_in_atm && g_pOrbiter->Cfg()->CfgVisualPrm.bReentryFlames && vessel->reentry.do_render) {
		const double afac = 1.0/log(1e11/2e8);
		double friction = 0.5 * pow(vessel->sp.atmrho,0.6) * pow (vessel->sp.airspd, 3);
		double opac = max (0, min (1, log(friction/2e8)*afac));
		if (opac > 0.005) {

			if (need_setup) { // initialise render state
				dev->SetRenderState (D3DRENDERSTATE_ZWRITEENABLE, FALSE);
				//dev->SetMaterial (&engmat);
				dev->SetTransform (D3DTRANSFORMSTATE_WORLD, &mWorld);
				cdir = MakeVECTOR3 (tmul (body->GRot(), cpos));
				need_setup = false;
			}

			dev->SetTexture (0, oldtex = (vessel->reentry.tex ? (LPDIRECTDRAWSURFACE7)vessel->reentry.tex : scene->ReentryTex()));
			static D3DMATERIAL7 mat = {{0,0,0,1},{0,0,0,1},{0,0,0,1},{1,1,1,1},0.0};
			dev->SetTextureStageState (0, D3DTSS_ALPHAOP, D3DTOP_MODULATE);
			dev->SetTextureStageState (0, D3DTSS_ALPHAARG2, D3DTA_DIFFUSE);
			mat.diffuse.a  = (float)opac;
			mat.emissive.g = (float)(0.7+0.3*opac);
			mat.emissive.b = (float)(0.5+0.5*opac);
			dev->SetMaterial (&mat);
			VECTOR3 dir = unit (MakeVECTOR3 (vessel->sp.airvel_ship));
			VECTOR3 ref = dir*(0.2*vessel->size);
			SetReentryVertices (-dir, cdir, ref, vessel->reentry.lscale*vessel->size,
				vessel->reentry.wscale*(0.15+max(opac,0.5))*vessel->size, opac, ReentryVtx);

			dev->DrawIndexedPrimitive (D3DPT_TRIANGLELIST, POSTEXVERTEXFLAG,
				ReentryVtx, 8, ReentryIdx, 12, 0);

			dev->SetTextureStageState (0, D3DTSS_ALPHAOP, D3DTOP_SELECTARG1);
			dev->SetTextureStageState (0, D3DTSS_ALPHAARG2, D3DTA_CURRENT);
			//dev->SetTextureStageState (0, D3DTSS_COLORARG2, D3DTA_CURRENT);
		}
	}

	if (!need_setup) { // reset render state
		dev->SetRenderState (D3DRENDERSTATE_ZWRITEENABLE, TRUE);
	}
}

void VVessel::RenderGroundShadow (LPDIRECT3DDEVICE7 dev, const Planet *planet)
{
	// project vessel shadow onto the surface of planet
	static const double eps = 1e-2;
	static const double shadow_elev_limit = 0.07;
	const SurfParam *sp = vessel->GetSurfParam ();
	if (!sp || sp->ref != (CelestialBody*)planet) return;
	if (sp->alt*eps > vessel->Size()) return;
	double depth = planet->ShadowDepth();
	if (!depth) return;

	Vector pp (planet->GPos());   // planet position
	Vector pv (vessel->GPos());   // vessel position
	Vector pvr (pv-pp);           // rel. vessel position
	Vector sd (pv.unit());        // shadow projection direction
	double R = planet->Size();    // planet radius
	R += sp->elev;  // Note: this only works at low vessel altitudes (shadow close to vessel position)

	// calculate the intersection of the vessel's shadow with the planet surface
	double fac1 = dotp (sd, pvr);
	if (fac1 > 0.0) return;       // shadow doesn't intersect planet surface
	double csun = -fac1/pvr.length(); // sun elevation above horizon
	if (csun < shadow_elev_limit) return; // sun too low to cast shadow
	double arg  = fac1*fac1 - (dotp (pvr, pvr) - R*R);
	if (arg <= 0.0) return;       // shadow doesn't intersect with planet surface
	double a = -fac1 - sqrt(arg);

	Vector sdv (tmul (vessel->GRot(), sd)); // projection direction in vessel frame
	Vector shp (sdv*a);                     // projection point
	//Vector hn (tmul (vessel->GRot(), (sd*a + pvr).unit())); // horizon normal in vessel frame
	Vector hn (tmul (vessel->GRot(), mul (planet->GRot(), tmul (sp->L2H, sp->surfnml))));

	// perform projections
	double nr0 = dotp (hn, shp);
	double nd  = dotp (hn, sdv);
	Vector sdvs (sdv / nd);

	VERTEX_XYZ *pvtx; // shadow vertex buffer
	double ndr, vx, vy, vz;

	// project all vessel meshes. This should be replaced by a dedicated shadow mesh
	dev->SetTransform (D3DTRANSFORMSTATE_WORLD, &mWorld);

	if (g_pOrbiter->UseStencil()) {
		static D3DMATERIAL7 shmat_grey = {{0,0,0,1},{0,0,0,0},{0,0,0,0},{0,0,0,0},0};

		shmat_grey.diffuse.a = (float)(depth * min (1, (csun-0.07)/0.015));
		dev->SetMaterial (&shmat_grey);
	}
	for (UINT i = 0; i < nmesh; i++) {
		if (!vessel->meshlist[i]) continue;
		if (!(vessel->meshlist[i]->vismode & MESHVIS_EXTERNAL)) continue; // only render shadows for externally visible meshes
		Mesh *mesh = meshlist[i].mesh;
		if (!mesh) continue;
		for (DWORD j = 0; j < mesh->nGroup(); j++) {
			if (mesh->GetGroupUsrFlag (j) & 1) continue; // "no shadow" flag
			DWORD v;
			GroupSpec *gs = mesh->GetGroup (j);
			pvtx = GetVertexXYZ (gs->nVtx); // get projection buffer
			if (meshlist[i].shifted) {
				VECTOR3 &ofs = vessel->meshlist[i]->meshofs;
				for (v = 0; v < gs->nVtx; v++) {
					vx = gs->Vtx[v].x+ofs.x, vy = gs->Vtx[v].y+ofs.y, vz = gs->Vtx[v].z+ofs.z;
					ndr = nr0 - (hn.x*vx + hn.y*vy + hn.z*vz);
					pvtx[v].x = (float)(vx + sdvs.x*ndr);
					pvtx[v].y = (float)(vy + sdvs.y*ndr);
					pvtx[v].z = (float)(vz + sdvs.z*ndr);
				}
			} else {
				for (v = 0; v < gs->nVtx; v++) {
					ndr = nr0 - (hn.x*(vx=gs->Vtx[v].x) + hn.y*(vy=gs->Vtx[v].y) + hn.z*(vz=gs->Vtx[v].z));
					pvtx[v].x = (float)(vx + sdvs.x*ndr);
					pvtx[v].y = (float)(vy + sdvs.y*ndr);
					pvtx[v].z = (float)(vz + sdvs.z*ndr);
				}
			}
			dev->DrawIndexedPrimitive (D3DPT_TRIANGLELIST, D3DFVF_XYZ, pvtx, gs->nVtx, gs->Idx, gs->nIdx, 0);
		}
	}
}

void VVessel::UpdateRenderVectors()
{
	VObject::UpdateRenderVectors();

	DWORD flag = *(DWORD*)gc->GetConfigParam(CFGPRM_FORCEVECTORFLAG);
	if (flag & BFV_ENABLE) {
		static double shift = 1e3, lshift = log(shift);
		bool logscale = ((flag & BFV_LOGSCALE) != 0);
		double len, scale, scale2 = vessel->size*0.02;
		scale = (logscale ? 1.0 : vessel->size / (vessel->Mass() * 9.81));
		double pscale = *(float*)gc->GetConfigParam(CFGPRM_FORCEVECTORSCALE);
		float alpha = *(float*)gc->GetConfigParam(CFGPRM_FORCEVECTOROPACITY);
		char cbuf[256];
		Vector F;

		if ((flag & BFV_WEIGHT) && vessel->GetWeightVector (F)) {
			sprintf (cbuf, "G =%sN", FloatStr (len = F.length(), 4));
			if (logscale) len = log(len+shift) - lshift; else len *= scale;
			AddVector (F.unit()*(len*pscale), Vector(0,0,0), scale2, std::string(cbuf), Vector (1,1,0), alpha, D3DRGB (1,1,0));
		}
		if ((flag & BFV_THRUST) && vessel->GetThrustVector (F)) {
			sprintf (cbuf, "T =%sN", FloatStr (len = F.length(), 4));
			if (logscale) len = log(len+shift) - lshift; else len *= scale;
			AddVector (F.unit()*(len*pscale), Vector(0,0,0), scale2, std::string(cbuf), Vector (0,0,1), alpha, D3DRGB (0.5,0.5,1));
		}
		if ((flag & BFV_LIFT) && vessel->GetLiftVector (F)) {
			sprintf (cbuf, "L =%sN", FloatStr (len = F.length(), 4));
			if (logscale) len = log(len+shift) - lshift; else len *= scale;
			AddVector (F.unit()*(len*pscale), Vector(0,0,0), scale2, std::string(cbuf), Vector (0,1,0), alpha, D3DRGB (0.5,1,0.5));
		}
		if ((flag & BFV_DRAG) && vessel->GetDragVector (F)) {
			sprintf (cbuf, "D =%sN", FloatStr (len = vessel->Drag, 4));
			if (logscale) len = log(len+shift) - lshift; else len *= scale;
			AddVector (F.unit()*(len*pscale), Vector(0,0,0), scale2, std::string(cbuf), Vector (1,0,0), alpha, D3DRGB (1,0.5,0.5));
		}
		if ((flag & BFV_TOTAL) && vessel->GetForceVector (F)) {
			sprintf (cbuf, "F =%sN", FloatStr (len = F.length(), 4));
			if (logscale) len = log(len+shift) - lshift; else len *= scale;
			AddVector (F.unit()*(len*pscale), Vector(0,0,0), scale2, std::string(cbuf), Vector (1,1,1), alpha, D3DRGB (1,1,1));
		}
		if (1) {
			for (int i = 0; i < vessel->nforcevec; i++) {
				F = vessel->forcevec[i];
				sprintf (cbuf, "F =%sN", FloatStr (len = F.length(), 4));
				len *= 1e-2;
				if (logscale) len = log(len+shift) - lshift; else len *= scale;
				AddVector (F.unit()*(len*pscale), vessel->forcepos[i], scale2, std::string(cbuf), Vector (0,1,1), alpha, D3DRGB (1,1,1));
			}
		}
		if ((flag & BFV_TORQUE) && vessel->GetTorqueVector(F)) {
			if (len = F.length()) {
				sprintf(cbuf, "M =%sNm", FloatStr(len, 4));
				if (logscale) len = log(len + 1e-5) - log(1e-5); else len *= scale * 1e5;
				AddVector(F.unit() * (len * pscale), Vector(0, 0, 0), scale2 * 0.5, std::string(cbuf), Vector(1, 0, 1), alpha, D3DRGB(1, 0, 1), (float)(0.5 * vessel->size));
			}
		}
	}
}

void VVessel::RenderAttachmentMarkers (LPDIRECT3DDEVICE7 dev, bool pa)
{
	static VERTEX_XYZ vtx[7];
	static WORD idx[18] = { 0,1,6, 0,6,1, 2,3,5, 2,5,3, 3,4,5, 3,5,4 };
	static D3DMATERIAL7 rmat = {{0,0,0,1},{0,0,0,1},{0,0,0,1},{1,0,0,1},0.0};
	static D3DMATERIAL7 gmat = {{0,0,0,1},{0,0,0,1},{0,0,0,1},{0,0.5,1,1},0.0};

	AttachmentSpec **attach;
	DWORD i, j, nattach;
	D3DMATERIAL7 *mat;
	Vector v;

	if (pa) {
		nattach = vessel->npattach;
		attach = vessel->pattach;
		mat = &rmat;
	} else {
		nattach = vessel->ncattach;
		attach = vessel->cattach;
		mat = &gmat;
	}

	if (!nattach) return;

	dev->SetMaterial (mat);
	for (i = 0; i < nattach; i++) {
		AttachmentSpec *as = attach[i];
		//if (as->id[0] != 'G') continue; // only mark grappling points
		float ux = (float)as->ref.x, uy = (float)as->ref.y, uz = (float)as->ref.z;
		vtx[0].x = 0;          vtx[0].y = 0;          vtx[0].z = 0;
		v.Set ((as->dir+as->rot)*0.25);
		vtx[1].x = (float)v.x; vtx[1].y = (float)v.y; vtx[1].z = (float)v.z;
		v.Set (as->dir*0.25 + as->rot*0.125);
		vtx[2].x = (float)v.x; vtx[2].y = (float)v.y; vtx[2].z = (float)v.z;
		v.Set (as->dir*0.5 + as->rot*0.125);
		vtx[3].x = (float)v.x; vtx[3].y = (float)v.y; vtx[3].z = (float)v.z;
		v.Set (as->dir*0.5 - as->rot*0.125);
		vtx[4].x = (float)v.x; vtx[4].y = (float)v.y; vtx[4].z = (float)v.z;
		v.Set (as->dir*0.25 - as->rot*0.125);
		vtx[5].x = (float)v.x; vtx[5].y = (float)v.y; vtx[5].z = (float)v.z;
		v.Set ((as->dir-as->rot)*0.25);
		vtx[6].x = (float)v.x; vtx[6].y = (float)v.y; vtx[6].z = (float)v.z;
		for (j = 0; j < 7; j++) {
			vtx[j].x += ux, vtx[j].y += uy, vtx[j].z += uz;
		}
		dev->DrawIndexedPrimitive (D3DPT_TRIANGLELIST, D3DFVF_XYZ, vtx, 7, idx, 18, D3DDP_WAIT);
	}
}

bool VVessel::ModLighting (LPD3DLIGHT7 light)
{
	const CelestialBody *cb = vessel->ProxyPlanet();
	if (!cb) return false;
	Star *sun = g_psys->GetStar(0); // should really loop over all suns
	Vector S(sun->GPos() - vessel->GPos());
	double s = S.length();
	double as = asin (sun->Size()/s);                     // apparent size of sun disc [rad]
	Vector lcol (1,1,1);
	double amb = 0;
	double dt = 1.0;
	bool lightmod = false;
	int i, j;

	// calculate shadowing by planet

	for (i = 0;; i++) {
		Vector P(cb->GPos() - vessel->GPos());
		double p = P.length();
		if (p < s) {                                      // shadow only if planet closer than sun
			double phi = acos (dotp(S,P)/(s*p));          // angular distance between sun and planet
			double ap = (cb->Size() < p ? asin(cb->Size() / p) : Pi05); // apparent size of planet disc [rad]

			if (cb->Type() == OBJTP_PLANET && ((Planet*)cb)->HasAtmosphere()) { // case 1: planet has atmosphere

				const ATMCONST *atm = ((Planet*)cb)->AtmParams();
				double alt = p-cb->Size();                // vessel altitude
				double altlimit = ((Planet*)cb)->AtmAttenuationLimit();
				double ap1 = ap * (altlimit + cb->Size())/cb->Size(); 
				if (alt < altlimit) {
					static const double delta0 = RAD*100.0;
					// This is the angular separation between planet centre and star below which
					// the atmosphere affects lighting when on the planet surface. (100: when sun
					// is 10 deg above horizon). Should possibly be made atmosphere-specific.
					ap1 = delta0 / (1.0 + alt*(delta0-ap1)/(altlimit*ap1));
				}

				if (as+ap1 >= phi && ap/as > 0.1) {       // overlap and significant planet size
					double dap = ap1-ap;
					Vector plight(1,1,1);
					if (as < ap) {                        // planet disc larger than sun disc
						if (phi < ap-as) {                // totality (sun below horizon)
							plight.Set(0,0,0);
						} else {
							double dispersion = max (0.02, min (0.9, log (atm->rho0+1.0)));
							double r0 = 1.0-0.40*dispersion;
							double g0 = 1.0-0.65*dispersion;
							double b0 = 1.0-1.0 *dispersion;
							if (phi > as+ap) {            // sun above horizon
								double f = (phi-as-ap)/dap;
								plight.x = f*(1.0-r0) + r0;
								plight.y = f*(1.0-g0) + g0;
								plight.z = f*(1.0-b0) + b0;
							} else {                      // sun partially below horizon
								double f = (phi-ap+as)/(2.0*as);
								plight.x = f*r0;
								plight.y = f*g0;
								plight.z = f*b0;
							}
							dt = 0.1;
						}
					} else {
						double maxcover = ap*ap / (as*as);
						if (phi < as-ap)
							plight.Set(1.0-maxcover,1.0-maxcover,1.0-maxcover); // annularity
						else {
							double frac = 1.0 - 0.5*maxcover * (1.0 + (as-phi)/ap); // partial cover
							plight.Set (frac,frac,frac);
							dt = 0.1;
						}
					}
					for	(j = 0; j < 3; j++) lcol.data[j] = min (lcol.data[j], plight.data[j]);
					lightmod = true;
				}

				// modification of ambient lighting
				if (!i && vessel->isInAtmosphere()) {
					double sunelev = phi-ap;
					if (sunelev > - 14.0*RAD) {
						double amb0 = min (0.7, log (atm->rho0+1.0)*0.4);
						double alt = p-cb->Size();
						amb = amb0 / (alt*0.5e-4 + 1.0);
						amb *= min (1.0, (sunelev+14.0*RAD)/(20.0*RAD));
						if (!lightmod) lightmod = (amb > 0.05);
						amb = max (0, amb-0.05);
						// reduce direct light component to avoid overexposure
						lcol *= 1.0-amb*0.5;
					}
				}

			} else {                                                            // case 2: planet has no atmosphere

				if (phi < as+ap && ap/as > 0.1) {         // overlap and significant planet size
					double lfrac;
					if (as < ap) {                        // planet disc larger than sun disc
						if (phi <= ap-as)                 // totality
							lfrac = 0.0;
						else {                            // partial cover
							lfrac = (phi+as-ap)/(2.0*as);
							dt = 0.1;
						}
					} else {                              // sun disc larger than planet disc
						double maxcover = ap*ap / (as*as);
						if (phi < as-ap) {                // annularity
							lfrac = 1.0-maxcover;
						} else {                          // partial cover
							lfrac = 1.0 - 0.5*maxcover * (1.0 + (as-phi)/ap);
							dt = 0.1;
						}
					}
					for (j = 0; j < 3; j++) lcol.data[j] = min (lcol.data[j], lfrac);
					lightmod = true;
				}

			}

		}
		if (cb->isMoon()) cb = cb->ElRef();
		else break;
	}

	if (lightmod) {
		D3DCOLORVALUE starcol = VObject::ColorToD3D(sun->GetLightColor());
		light->dcvDiffuse.r = light->dcvSpecular.r = starcol.r * (float)lcol.x;
		light->dcvDiffuse.g = light->dcvSpecular.g = starcol.g * (float)lcol.y;
		light->dcvDiffuse.b = light->dcvSpecular.b = starcol.b * (float)lcol.z;
		light->dcvAmbient.r = (float)amb;
		light->dcvAmbient.g = (float)amb;
		light->dcvAmbient.b = (float)amb;
		S /= s;
		light->dvDirection.x = -(float)S.x;
		light->dvDirection.y = -(float)S.y;
		light->dvDirection.z = -(float)S.z;
	}

	tCheckLight = td.SimT0 + dt;
	// we might be able to increase the interval when far from the
	// boundary, but then need to force a check in the case of sudden
	// movement (e.g. editor)

	return lightmod;
}

bool VVessel::MeshgroupTransform (const MESHGROUP_TRANSFORM &mt) const
{
	int grp = mt.ngrp;
	UINT msh = (UINT)mt.nmesh;
	if (msh >= nmesh) return false;
	Mesh *mesh = meshlist[msh].mesh;
	if (!mesh) return false;
	if (grp < 0) // transform all groups
		return MeshTransform (mt, mesh);
	else if ((UINT)grp < mesh->nGroup())
		return MeshgroupTransform (mt, mesh, (UINT)grp);
	else
		return false;
}

bool VVessel::MeshTransform (const MESHGROUP_TRANSFORM &mt, Mesh *mesh) const
{
	float dx, dy, dz, phi;
	double ax, ay, az; // rotation axis

	switch (mt.transform) {
	case MESHGROUP_TRANSFORM::ROTATE:
		dx = (float)mt.P.rotparam.ref.x;
		dy = (float)mt.P.rotparam.ref.y;
		dz = (float)mt.P.rotparam.ref.z;
		ax = mt.P.rotparam.axis.x;
		ay = mt.P.rotparam.axis.y;
		az = mt.P.rotparam.axis.z;
		mesh->Translate (-dx, -dy, -dz);
		if (ax == 0.0) {
			if (ay == 0.0) {        // rotate around z-axis
				mesh->Rotate (Mesh::ROTATE_Z, az > 0 ? mt.P.rotparam.angle : -mt.P.rotparam.angle);
			} else if (az == 0.0) { // rotate around y-axis
				mesh->Rotate (Mesh::ROTATE_Y, ay > 0 ? mt.P.rotparam.angle : -mt.P.rotparam.angle);
			} else {                    // rotate around axis in yz-plane
				phi = (float)atan2 (ay, az);
				mesh->Rotate (Mesh::ROTATE_X,  phi);
				mesh->Rotate (Mesh::ROTATE_Z, -mt.P.rotparam.angle);
				mesh->Rotate (Mesh::ROTATE_X, -phi);
				// not very efficient; we should compute the resulting rotation matrix and do a single transform
			}
		} else if (ay == 0.0) {
			if (az == 0.0) {        // rotate around x-axis
				mesh->Rotate (Mesh::ROTATE_X, ax > 0 ? mt.P.rotparam.angle : -mt.P.rotparam.angle);
			} else {                    // rotate around axis in xz-plane
				phi = (float)atan2 (az, ax);
				mesh->Rotate (Mesh::ROTATE_Y, -phi);
				mesh->Rotate (Mesh::ROTATE_X, mt.P.rotparam.angle);
				mesh->Rotate (Mesh::ROTATE_Y,  phi);
			}
		} else if (az == 0.0) {     // rotate around axis in xy-plane
			phi = (float)atan2 (ax, ay);
			mesh->Rotate (Mesh::ROTATE_Z, phi);
			mesh->Rotate (Mesh::ROTATE_Y, mt.P.rotparam.angle);
			mesh->Rotate (Mesh::ROTATE_Z, -phi);
		} else {                        // rotate around general axis
		}
		mesh->Translate (dx, dy, dz);
		break;
	case MESHGROUP_TRANSFORM::TRANSLATE:
		dx = (float)mt.P.transparam.shift.x;
		dy = (float)mt.P.transparam.shift.y;
		dz = (float)mt.P.transparam.shift.z;
		mesh->Translate (dx, dy, dz);
		break;
	case MESHGROUP_TRANSFORM::SCALE:
		dx = (float)mt.P.scaleparam.scale.x;
		dy = (float)mt.P.scaleparam.scale.y;
		dz = (float)mt.P.scaleparam.scale.z;
		mesh->Scale (dx, dy, dz);
		break;
	}
	return true;
}

bool VVessel::MeshgroupTransform (const MESHGROUP_TRANSFORM &mt, Mesh *mesh, UINT grp) const
{
	float dx, dy, dz, phi;
	double ax, ay, az; // rotation axis

	switch (mt.transform) {
	case MESHGROUP_TRANSFORM::ROTATE:
		dx = (float)mt.P.rotparam.ref.x;
		dy = (float)mt.P.rotparam.ref.y;
		dz = (float)mt.P.rotparam.ref.z;
		ax = mt.P.rotparam.axis.x;
		ay = mt.P.rotparam.axis.y;
		az = mt.P.rotparam.axis.z;
		mesh->TranslateGroup (grp, -dx, -dy, -dz);
		if (ax == 0.0) {
			if (ay == 0.0) {        // rotate around z-axis
				mesh->RotateGroup (grp, Mesh::ROTATE_Z, az > 0 ? mt.P.rotparam.angle : -mt.P.rotparam.angle);
			} else if (az == 0.0) { // rotate around y-axis
				mesh->RotateGroup (grp, Mesh::ROTATE_Y, ay > 0 ? mt.P.rotparam.angle : -mt.P.rotparam.angle);
			} else {                    // rotate around axis in yz-plane
				phi = (float)atan2 (ay, az);
				mesh->RotateGroup (grp, Mesh::ROTATE_X,  phi);
				mesh->RotateGroup (grp, Mesh::ROTATE_Z, -mt.P.rotparam.angle);
				mesh->RotateGroup (grp, Mesh::ROTATE_X, -phi);
				// not very efficient; we should compute the resulting rotation matrix and do a single transform
			}
		} else if (ay == 0.0) {
			if (az == 0.0) {        // rotate around x-axis
				mesh->RotateGroup (grp, Mesh::ROTATE_X, ax > 0 ? mt.P.rotparam.angle : -mt.P.rotparam.angle);
			} else {                    // rotate around axis in xz-plane
				phi = (float)atan2 (az, ax);
				mesh->RotateGroup (grp, Mesh::ROTATE_Y, -phi);
				mesh->RotateGroup (grp, Mesh::ROTATE_X, mt.P.rotparam.angle);
				mesh->RotateGroup (grp, Mesh::ROTATE_Y,  phi);
			}
		} else if (az == 0.0) {     // rotate around axis in xy-plane
			phi = (float)atan2 (ax, ay);
			mesh->RotateGroup (grp, Mesh::ROTATE_Z, phi);
			mesh->RotateGroup (grp, Mesh::ROTATE_Y, mt.P.rotparam.angle);
			mesh->RotateGroup (grp, Mesh::ROTATE_Z, -phi);
		} else {                        // rotate around general axis
		}
		mesh->TranslateGroup (grp, dx, dy, dz);
		break;
	case MESHGROUP_TRANSFORM::TRANSLATE:
		dx = (float)mt.P.transparam.shift.x;
		dy = (float)mt.P.transparam.shift.y;
		dz = (float)mt.P.transparam.shift.z;
		mesh->TranslateGroup (grp, dx, dy, dz);
		break;
	case MESHGROUP_TRANSFORM::SCALE:
		dx = (float)mt.P.scaleparam.scale.x;
		dy = (float)mt.P.scaleparam.scale.y;
		dz = (float)mt.P.scaleparam.scale.z;
		mesh->ScaleGroup (grp, dx, dy, dz);
		break;
	}
	return true;
}

void VVessel::Animate (int seq) // OBSOLETE
{
	ANIMSEQ *anim = vessel->animseq+seq;
	double s0, s1, ds;
	float range;
	VECTOR3 shift;
	UINT i, j;
	for (i = 0; i < anim->ncomp; i++) {
		ANIMCOMP *comp = anim->comp[i];
		s0 = vanim[seq]; // current component animation state
		if      (s0 < comp->state0) s0 = comp->state0;
		else if (s0 > comp->state1) s0 = comp->state1;
		s1 = anim->state; // target component animation state
		if      (s1 < comp->state0) s1 = comp->state0;
		else if (s1 > comp->state1) s1 = comp->state1;
		if ((ds = (s1-s0)) == 0) continue; // nothing to do for this component
		ds /= (comp->state1-comp->state0); // stretch to range 0-1
		// now perform transformation
		switch (comp->trans.transform) {
		case MESHGROUP_TRANSFORM::ROTATE:
			range = comp->trans.P.rotparam.angle;
			comp->trans.P.rotparam.angle *= (float)ds;
			for (j = 0; j < comp->ngrp; j++) {
				comp->trans.ngrp = comp->grp[j];
				MeshgroupTransform (comp->trans);
			}
			comp->trans.P.rotparam.angle = range;
			break;
		case MESHGROUP_TRANSFORM::TRANSLATE:
			shift = comp->trans.P.transparam.shift;
			comp->trans.P.transparam.shift *= (float)ds;
			for (j = 0; j < comp->ngrp; j++) {
				comp->trans.ngrp = comp->grp[j];
				MeshgroupTransform (comp->trans);
			}
			comp->trans.P.transparam.shift = shift;
			break;
		}
	}
	vanim[seq] = anim->state;
}

void TransformPoint (VECTOR3 &p, const D3DMATRIX &T)
{
	double x = p.x*T._11 + p.y*T._21 + p.z*T._31 + T._41;
	double y = p.x*T._12 + p.y*T._22 + p.z*T._32 + T._42;
	double z = p.x*T._13 + p.y*T._23 + p.z*T._33 + T._43;
	double w = p.x*T._14 + p.y*T._24 + p.z*T._34 + T._44;
    p.x = x/w;
	p.y = y/w;
	p.z = z/w;
}

void TransformDirection (VECTOR3 &a, const D3DMATRIX &T, bool normalise)
{
	double x = a.x*T._11 + a.y*T._21 + a.z*T._31;
	double y = a.x*T._12 + a.y*T._22 + a.z*T._32;
	double z = a.x*T._13 + a.y*T._23 + a.z*T._33;
	a.x = x, a.y = y, a.z = z;
	if (normalise) {
		double len = sqrt (x*x + y*y + z*z);
		a.x /= len;
		a.y /= len;
		a.z /= len;
	}
}

void VVessel::AnimateComponent (ANIMATIONCOMP *comp, const D3DMATRIX &T)
{
	UINT i;
	MGROUP_TRANSFORM *trans = comp->trans;

	if (trans->mesh == LOCALVERTEXLIST) { // transform a list of individual vertices

		VECTOR3 *vtx = (VECTOR3*)trans->grp;
		for (i = 0; i < trans->ngrp; i++)
			TransformPoint (vtx[i], T);

	} else {                              // transform mesh groups

		if (trans->mesh >= nmesh) return; // mesh index out of range
		Mesh *mesh = meshlist[trans->mesh].mesh;
		if (!mesh) return;

		if (trans->grp) { // animate individual mesh groups
			for (i = 0; i < trans->ngrp; i++)
				mesh->TransformGroup (trans->grp[i], T);
		} else {          // animate complete mesh
			mesh->Transform (T);
		}
	}

	// recursively transform all child animations
	for (i = 0; i < comp->nchildren; i++) {
		ANIMATIONCOMP *child = comp->children[i];
		AnimateComponent (child, T);
		switch (child->trans->Type()) {
		case MGROUP_TRANSFORM::NULLTRANSFORM:
			break;
		case MGROUP_TRANSFORM::ROTATE: {
			MGROUP_ROTATE *rot = (MGROUP_ROTATE*)child->trans;
			TransformPoint (rot->ref, T);
			TransformDirection (rot->axis, T, true);
			} break;
		case MGROUP_TRANSFORM::TRANSLATE: {
			MGROUP_TRANSLATE *lin = (MGROUP_TRANSLATE*)child->trans;
			TransformDirection (lin->shift, T, false);
			} break;
		case MGROUP_TRANSFORM::SCALE: {
			MGROUP_SCALE *scl = (MGROUP_SCALE*)child->trans;
			TransformPoint (scl->ref, T);
			// we can't transform anisotropic scaling vector
			} break;
		}
	}
}

#pragma optimize ("g",off)
void VVessel::Animate2 (UINT an, double state, UINT mshidx)
{
	double s0, s1, ds;
	UINT i, ii;
	D3DMATRIX T;
	ANIMATION *A = vessel->anim+an;
	for (ii = 0; ii < A->ncomp; ii++) {
		i = (state > vanim_state[an] ? ii : A->ncomp-ii-1);
		ANIMATIONCOMP *AC = A->comp[i];
		if (mshidx != (UINT)-1 && mshidx != AC->trans->mesh) continue;
		s0 = vanim_state[an]; // current animation state in the visual
		if      (s0 < AC->state0) s0 = AC->state0;
		else if (s0 > AC->state1) s0 = AC->state1;
		s1 = state;           // required animation state
		if      (s1 < AC->state0) s1 = AC->state0;
		else if (s1 > AC->state1) s1 = AC->state1;
		if ((ds = (s1-s0)) == 0) continue; // nothing to do for this component
		ds /= (AC->state1 - AC->state0);   // stretch to range 0..1

		// Build transformation matrix
		switch (AC->trans->Type()) {
		case MGROUP_TRANSFORM::NULLTRANSFORM:
			VMAT_identity (T);
			AnimateComponent (AC, T);
			break;
		case MGROUP_TRANSFORM::ROTATE: {
			MGROUP_ROTATE *rot = (MGROUP_ROTATE*)AC->trans;
			D3DVECTOR ax = {D3DVAL(rot->axis.x), D3DVAL(rot->axis.y), D3DVAL(rot->axis.z)};
			VMAT_rotation_from_axis (ax, (float)ds*rot->angle, T);
			D3DVALUE dx = D3DVAL(rot->ref.x), dy = D3DVAL(rot->ref.y), dz = D3DVAL(rot->ref.z);
			T._41 = dx - T._11*dx - T._21*dy - T._31*dz;
			T._42 = dy - T._12*dx - T._22*dy - T._32*dz;
			T._43 = dz - T._13*dx - T._23*dy - T._33*dz;
			AnimateComponent (AC, T);
			} break;
		case MGROUP_TRANSFORM::TRANSLATE: {
			MGROUP_TRANSLATE *lin = (MGROUP_TRANSLATE*)AC->trans;
			VMAT_identity (T);
			T._41 = (float)(ds*lin->shift.x);
			T._42 = (float)(ds*lin->shift.y);
			T._43 = (float)(ds*lin->shift.z);
			AnimateComponent (AC, T);
			} break;
		case MGROUP_TRANSFORM::SCALE: {
			MGROUP_SCALE *scl = (MGROUP_SCALE*)AC->trans;
			s0 = (s0-AC->state0)/(AC->state1-AC->state0);
			s1 = (s1-AC->state0)/(AC->state1-AC->state0);
			VMAT_identity (T);
			T._11 = (float)((s1*(scl->scale.x-1)+1)/(s0*(scl->scale.x-1)+1));
			T._22 = (float)((s1*(scl->scale.y-1)+1)/(s0*(scl->scale.y-1)+1));
			T._33 = (float)((s1*(scl->scale.z-1)+1)/(s0*(scl->scale.z-1)+1));
			T._41 = (float)scl->ref.x * (1.0f-T._11);
			T._42 = (float)scl->ref.y * (1.0f-T._22);
			T._43 = (float)scl->ref.z * (1.0f-T._33);
			AnimateComponent (AC, T);
			} break;
		}
	}
}
#pragma optimize ("g",on)
