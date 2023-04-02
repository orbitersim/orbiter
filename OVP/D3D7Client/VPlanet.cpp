// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//   ORBITER VISUALISATION PROJECT (OVP)
//   D3D7 Client module
// ==============================================================

// ==============================================================
// VPlanet.cpp
// class vPlanet (implementation)
//
// A vPlanet is the visual representation of a "planetary" object
// (planet, moon, asteroid).
// Currently this only supports spherical objects, without
// variations in elevation.
// ==============================================================

#define D3D_OVERLOADS

#include "D3D7Client.h"
#include "D3D7Config.h"
#include "VPlanet.h"
#include "VBase.h"
#include "Camera.h"
#include "SurfMgr.h"
#include "surfmgr2.h"
#include "cloudmgr2.h"
#include "CloudMgr.h"
#include "HazeMgr.h"
#include "RingMgr.h"

using namespace oapi;

// ==============================================================

static double farplane = 1e6;
static double max_surf_dist = 1e4;

extern int SURF_MAX_PATCHLEVEL;

// ==============================================================

vPlanet::vPlanet (OBJHANDLE _hObj, const Scene *scene): vObject (_hObj, scene)
{
	rad = (float)size;
	render_rad = (float)(0.1*rad);
	dist_scale = 1.0f;
	max_centre_dist = 0.9*scene->GetCamera()->GetFarlimit();
	maxdist = max (max_centre_dist, max_surf_dist + rad);
	max_patchres = *(DWORD*)oapiGetObjectParam (_hObj, OBJPRM_PLANET_SURFACEMAXLEVEL);
	max_patchres = min (max_patchres, *(DWORD*)gc->GetConfigParam (CFGPRM_SURFACEMAXLEVEL));
	int tilever = *(int*)oapiGetObjectParam (_hObj, OBJPRM_PLANET_TILEENGINE);
	if (tilever < 2) {
		surfmgr = new SurfaceManager (gc, this);
		surfmgr2 = NULL;
	} else {
		surfmgr = NULL;
		int patchlvl = 2 << *(int*)gc->GetConfigParam (CFGPRM_TILEPATCHRES);
		surfmgr2 = new TileManager2<SurfTile> (this, max_patchres, patchlvl);
		prm.horizon_excess = *(double*)oapiGetObjectParam (_hObj, OBJPRM_PLANET_HORIZONEXCESS);
		prm.tilebb_excess = *(double*)oapiGetObjectParam (_hObj, OBJPRM_PLANET_TILEBBEXCESS);
	}
	prm.horizon_minrad = min (1.0 + *(double*)oapiGetObjectParam (_hObj, OBJPRM_PLANET_MINELEVATION)/size, 1.0-1e-4);
	prm.bAtm = oapiPlanetHasAtmosphere (_hObj);
	if (prm.bAtm) {
		const ATMCONST *atmc = oapiGetPlanetAtmConstants(_hObj);
		prm.atm_href = log(atmc->rho0)*2e4 + 2e4;
		prm.atm_amb0 = min (0.7, log (atmc->rho0+1.0)*0.35);
		DWORD amb0 = *(DWORD*)gc->GetConfigParam (CFGPRM_AMBIENTLEVEL);
		prm.amb0col = 0;
		for (int i = 0; i < 4; i++) prm.amb0col |= amb0 << (i<<3);
	}
	hazemgr = 0;
	hashaze = *(bool*)gc->GetConfigParam (CFGPRM_ATMHAZE) && prm.bAtm;
	bRipple = *(bool*)gc->GetConfigParam (CFGPRM_SURFACERIPPLE) &&
		*(bool*)oapiGetObjectParam (_hObj, OBJPRM_PLANET_SURFACERIPPLE);
	if (bRipple) {
		if (surfmgr) surfmgr->SetMicrotexture ("waves.dds");
	}

	shadowalpha = (float)(1.0f - *(double*)oapiGetObjectParam (_hObj, OBJPRM_PLANET_SHADOWCOLOUR));
	bVesselShadow = *(bool*)gc->GetConfigParam (CFGPRM_VESSELSHADOWS) &&
		shadowalpha >= 0.01;

	clouddata = 0;
	cloudmgr2 = 0;
	prm.bCloud = (*(bool*)gc->GetConfigParam (CFGPRM_CLOUDS) &&
		*(bool*)oapiGetObjectParam (_hObj, OBJPRM_PLANET_HASCLOUDS));
	if (prm.bCloud) {
		int cloudtilever = *(int*)oapiGetObjectParam (_hObj, OBJPRM_PLANET_CLOUDTILEENGINE);
		prm.cloudalt = *(double*)oapiGetObjectParam (_hObj, OBJPRM_PLANET_CLOUDALT);
		prm.bCloudBrighten = *(bool*)oapiGetObjectParam (_hObj, OBJPRM_PLANET_CLOUDOVERSATURATE);
		prm.bCloudShadow = *(bool*)gc->GetConfigParam (CFGPRM_CLOUDSHADOWS);
		prm.shadowalpha = 1.0 - *(float*)oapiGetObjectParam (_hObj, OBJPRM_PLANET_CLOUDSHADOWCOL);
		if (prm.shadowalpha < 0.01)
			prm.bCloudShadow = false;
		if (cloudtilever == 1) { // legacy cloud engine
			clouddata = new CloudData;
			clouddata->cloudmgr = new CloudManager (gc, this);
			clouddata->cloudshadow = prm.bCloudShadow;
			if (clouddata->cloudshadow) {
				clouddata->shadowalpha = (float)prm.shadowalpha;
			}
			if (*(bool*)oapiGetObjectParam (_hObj, OBJPRM_PLANET_CLOUDMICROTEX)) {
				clouddata->cloudmgr->SetMicrotexture ("cloud1.dds");
				clouddata->microalt0 = *(double*)oapiGetObjectParam (_hObj, OBJPRM_PLANET_CLOUDMICROALTMIN);
				clouddata->microalt1 = *(double*)oapiGetObjectParam (_hObj, OBJPRM_PLANET_CLOUDMICROALTMAX);
			}
		} else { // v2 cloud engine
			int maxlvl = *(int*)oapiGetObjectParam (_hObj, OBJPRM_PLANET_CLOUDMAXLEVEL);
			maxlvl = min (maxlvl, *(DWORD*)gc->GetConfigParam (CFGPRM_SURFACEMAXLEVEL));
			cloudmgr2 = new TileManager2<CloudTile> (this, maxlvl, 32);
		}
	} else {
		prm.bCloudShadow = false;
	}

	if (*(bool*)oapiGetObjectParam (_hObj, OBJPRM_PLANET_HASRINGS)) {
		double minrad = *(double*)oapiGetObjectParam (_hObj, OBJPRM_PLANET_RINGMINRAD);
		double maxrad = *(double*)oapiGetObjectParam (_hObj, OBJPRM_PLANET_RINGMAXRAD);
		ringmgr = new RingManager (this, minrad, maxrad);
		render_rad = (float)(rad*maxrad);
	} else {
		ringmgr = 0;
	}
	
	memcpy (&fog, oapiGetObjectParam (_hObj, OBJPRM_PLANET_FOGPARAM), sizeof (FogParam));
	prm.bFogEnabled = (fog.dens_0 > 0);

	renderpix = false;
	patchres = 0;
	mipmap_mode = gc->Cfg()->PlanetMipmapMode;
	aniso_mode = gc->Cfg()->PlanetAnisoMode;

	nbase = oapiGetBaseCount (_hObj);
	vbase = new vBase*[nbase];
	for (DWORD i = 0; i < nbase; i++)
		vbase[i] = NULL;

	mesh = NULL;
	if (surfmgr && surfmgr->GetMaxLevel() == 0) {
		char cbuf[256];
		oapiGetObjectName (hObj, cbuf, 256);
		OBJHANDLE hMesh = oapiLoadMesh (cbuf);
		if (hMesh) {
			mesh = new D3D7Mesh (gc, hMesh);
			oapiDeleteMesh (hMesh);
		}
	}
}

// ==============================================================

vPlanet::~vPlanet ()
{
	if (nbase) {
		for (DWORD i = 0; i < nbase; i++)
			if (vbase[i]) delete vbase[i];
		delete []vbase;
	}
	if (surfmgr) delete surfmgr;
	else if (surfmgr2) delete surfmgr2;
	if (cloudmgr2) delete cloudmgr2;

	if (clouddata) {
		delete clouddata->cloudmgr;
		delete clouddata;
	}
	if (hazemgr) delete hazemgr;
	if (ringmgr) delete ringmgr;
	if (mesh)    delete mesh;
}

// ==============================================================

bool vPlanet::Update ()
{
	if (!active) return false;

	vObject::Update();

	int i, j;
	float rad_scale = rad;
	bool rescale = false;
	dist_scale = 1.0f;

	if (cdist > maxdist) {
		rescale = true;
		dist_scale = (FLOAT)(max_centre_dist/cdist);
	}
	//if (cdist+render_rad > farplane && cdist-rad > 1e4) {
	//	rescale = true;
	//	dist_scale = (FLOAT)(farplane/(cdist+render_rad));
	//}
	mWorldScaled = mWorld;

	if (rescale) {
		rad_scale *= dist_scale;
		mWorldScaled._41 *= dist_scale;
		mWorldScaled._42 *= dist_scale;
		mWorldScaled._43 *= dist_scale;
	}

	// scale up from template sphere radius 1
	mWorldScaled._11 *= rad_scale; mWorldScaled._12 *= rad_scale; mWorldScaled._13 *= rad_scale;
	mWorldScaled._21 *= rad_scale; mWorldScaled._22 *= rad_scale; mWorldScaled._23 *= rad_scale;
	mWorldScaled._31 *= rad_scale; mWorldScaled._32 *= rad_scale; mWorldScaled._33 *= rad_scale;

	// cloud layer world matrix
	if (prm.bCloud) {
		double cloudrad = size + prm.cloudalt;
		prm.cloudrot = *(double*)oapiGetObjectParam (hObj, OBJPRM_PLANET_CLOUDROTATION);
		prm.cloudvis = (cdist < cloudrad ? 1:0);
		if (cdist > cloudrad*(1.0-1.5e-4)) prm.cloudvis |= 2;
		prm.bCloudFlatShadows = (cdist >= 1.05*size);

		if (clouddata) {
			if (prm.cloudvis & 1) {
				clouddata->viewap = acos (size/cloudrad);
				if (size < cdist) clouddata->viewap += acos (size/cdist);
			} else {
				clouddata->viewap = 0;
			}

			float cloudscale = (float)(cloudrad/size);

			// world matrix for cloud shadows on the surface
			memcpy (&clouddata->mWorldC0, &mWorldScaled, sizeof (D3DMATRIX));
			if (prm.cloudrot) {
				static D3DMATRIX crot (1,0,0,0, 0,1,0,0, 0,0,1,0, 0,0,0,1);
				crot._11 =   crot._33 = (float)cos(prm.cloudrot);
				crot._13 = -(crot._31 = (float)sin(prm.cloudrot));
				D3DMAT_MatrixMultiply (&clouddata->mWorldC0, &clouddata->mWorldC0, &crot);
			}

			// world matrix for cloud layer
			memcpy (&clouddata->mWorldC, &clouddata->mWorldC0, sizeof (D3DMATRIX));
			for (i = 0; i < 3; i++)
				for (j = 0; j < 3; j++) {
					clouddata->mWorldC.m[i][j] *= cloudscale;
				}

			// set microtexture intensity
			double alt = cdist-rad;
			double lvl = (clouddata->microalt1-alt)/(clouddata->microalt1-clouddata->microalt0);
			clouddata->cloudmgr->SetMicrolevel (max (0, min (1, lvl)));
		}
	}

	// check all base visuals
	if (nbase) {
		VECTOR3 pos, cpos = *scn->GetCamera()->GetGPos();
		double scale = (double)scn->ViewH()/scn->GetCamera()->GetTanAp();
		for (DWORD i = 0; i < nbase; i++) {
			OBJHANDLE hBase = oapiGetBaseByIndex (hObj, i);
			oapiGetGlobalPos (hBase, &pos);
			double rad = oapiGetSize (hBase);
			double dst = dist (pos, cpos);
			double apprad = rad*scale/dst;
			if (vbase[i]) { // base visual exists
				if (apprad < 1.0) { // out of visual range
					delete vbase[i];
					vbase[i] = 0;
				}
			} else {        // base visual doesn't exist
				if (apprad > 2.0) { // within visual range
					vbase[i] = new vBase (hBase, scn);
				}
			}
			if (vbase[i])
				vbase[i]->Update();
		}
	}
	return true;
}

// ==============================================================

void vPlanet::CheckResolution ()
{
	double alt = max (1.0, cdist-rad);
	double apr = rad * scn->ViewH()*0.5 / (alt * scn->GetCamera()->GetTanAp());
	// apparent planet radius in units of screen pixels

	int new_patchres;
	double ntx;

	if (apr < 2.5) { // render planet as 2x2 pixels
		renderpix = true;
		new_patchres = 0;
		ntx = 0;
	} else {
		renderpix = false;
		ntx = PI*2.0 * apr;

		static const double scal2 = 1.0/log(2.0);
		const double shift = (surfmgr2 ? 6.0 : 5.0); // reduce level for tile mgr v2, because of increased patch size
		new_patchres = min (max ((int)(scal2*log(ntx)-shift),1), max_patchres);
	}
	if (new_patchres != patchres) {
		if (hashaze) {
			if (new_patchres < 1) {
				if (hazemgr) { delete hazemgr; hazemgr = 0; }
			} else {
				if (!hazemgr) { hazemgr = new HazeManager (scn->GetClient(), this); }
			}
		}
		if (ringmgr) {
			int ringres = (new_patchres <= 3 ? 0 : new_patchres <= 4 ? 1:2);
			ringmgr->SetMeshRes (ringres);
		}
		patchres = new_patchres;
	}
}

// ==============================================================

void vPlanet::RenderZRange (double *nplane, double *fplane)
{
	double d = dotp (*scn->GetCamera()->GetGDir(), cpos);
	*fplane = max (1e3, d+rad*1.2);
	*nplane = max (1e0, d-rad*1.2);
	*fplane = min (*fplane, *nplane*1e5);
}

// ==============================================================

bool vPlanet::Render (LPDIRECT3DDEVICE7 dev)
{
	if (!active) return false;

	if (renderpix) { // render as 2x2 pixel block

		RenderDot (dev);

	} else {             // render as sphere

		DWORD nmlnml = TRUE;
		if (mesh || !surfmgr2) {
			// old-style and mesh-based planet surfaces use a rescaled world matrix,
			// so we need to make sure that normals are renormalised
			dev->GetRenderState (D3DRENDERSTATE_NORMALIZENORMALS, &nmlnml);
			if (!nmlnml) dev->SetRenderState (D3DRENDERSTATE_NORMALIZENORMALS, TRUE);
		}

		DWORD amb = prm.amb0col;
		bool ringpostrender = false;
		bool clear_zbuf = false;
		float fogfactor;

		prm.bFog = prm.bFogEnabled;
		prm.bTint = prm.bFogEnabled;

		int skybrt = scn->BgBrightnessLevel();
		prm.bAddBkg = (skybrt && (hObj != scn->GetCamera()->GetProxyBody()));

		if (ringmgr) {
			if (cdist < rad*ringmgr->InnerRad()) { // camera inside inner ring edge
				ringmgr->Render (dev, mWorldScaled);
			} else {
				// if the planet has a ring system we update the z-buffer
				// but don't do z-checking for the planet surface
				// This strategy could do with some reconsideration
				dev->SetRenderState (D3DRENDERSTATE_ZENABLE, TRUE);
				dev->SetRenderState (D3DRENDERSTATE_ZWRITEENABLE, TRUE);
				dev->SetRenderState (D3DRENDERSTATE_ZFUNC, D3DCMP_ALWAYS);
				ringpostrender = true;
				clear_zbuf = true;
			}
		}
		if (prm.bCloud && (prm.cloudvis & 1))
			RenderCloudLayer (dev, D3DCULL_CW, prm);      // render clouds from below
		if (hazemgr) hazemgr->Render (dev, mWorldScaled);       // horizon ring

		if (prm.bAtm) {
			if (ModLighting (amb))
				dev->SetRenderState (D3DRENDERSTATE_AMBIENT, amb);
		}

		if (prm.bFog) { // set up distance fog
			double h = max (1.0, cdist-size);

			VECTOR3 fogcol = fog.col;
			double h_ref = fog.alt_ref;   // 3e3;
			double fog_0 = fog.dens_0;    // 5e-5;
			double fog_ref = fog.dens_ref; // 3e-5;
			double h_max = size*1.5; // At this altitude, fog effect drops to zero
			double scl = h_ref*fog_ref;

			if (h < h_ref) {
				// linear zone
				fogfactor = (float)(h/h_ref * (fog_ref-fog_0) + fog_0);
			} else {
				// hyperbolic zone: fogfactor = a/(h+b) + c
				// a, b and c are designed such that
				// * fogfactor(h) is continuous at h = h_ref
				// * d fogfactor / dh is continuous at h = h_ref
				// * fogfactor(h_max) = 0
				double b = - (fog_ref*h_max + (fog_ref-fog_0)*(h_max-h_ref)) / (fog_ref + (fog_ref-fog_0)/h_ref * (h_max-h_ref));
				double a = fog_ref*(h_ref+b)*(h_max+b)/(h_max-h_ref);
				double c = -a/(h_max+b);
				fogfactor = (float)(a/(h+b)+c);
			}

			if (fogfactor < 0.0) prm.bFog = false;
			else {
				// day/nighttime fog lighting
				VECTOR3 ppos;
				oapiGetGlobalPos (hObj, &ppos);
				double cosa = dotp (unit(ppos), unit(cpos));
				double bright = 1.0 * max (0.0, min (1.0, cosa + 0.3));
				float rfog = (float)(bright*(min(1.0,fogcol.x)+0.0)); // "whiten" the fog colour
				float gfog = (float)(bright*(min(1.0,fogcol.y)+0.0));
				float bfog = (float)(bright*(min(1.0,fogcol.z)+0.0));
				dev->SetRenderState (D3DRENDERSTATE_FOGENABLE, TRUE);
				dev->SetRenderState (D3DRENDERSTATE_FOGVERTEXMODE, D3DFOG_NONE);
				dev->SetRenderState (D3DRENDERSTATE_FOGTABLEMODE, D3DFOG_EXP);
				dev->SetRenderState (D3DRENDERSTATE_FOGCOLOR, D3DRGB(rfog,gfog,bfog));
				dev->SetRenderState (D3DRENDERSTATE_FOGDENSITY, *((LPDWORD)(&fogfactor)));
			}
		}

		if (prm.bTint) {
			prm.rgbTint = *(VECTOR3*)oapiGetObjectParam (hObj, OBJPRM_PLANET_ATMTINTCOLOUR);
			double R = oapiGetSize (hObj);
			double alt = cdist - R;
			double alt_ref1 = fog.alt_ref*5.0;
			double alt_ref2 = alt_ref1 * 0.1;
			if (alt < alt_ref1) {
				double scale = (alt-alt_ref2)/(alt_ref1-alt_ref2);
				if (scale <= 0.0) prm.bTint = false;
				else prm.rgbTint *= scale;
			}
		}

		if (mesh) {
			dev->SetTransform (D3DTRANSFORMSTATE_WORLD, &mWorldScaled);
			mesh->Render (dev);
		} else {
			bool using_zbuf;
			RenderSphere (dev, prm, using_zbuf);            // planet surface
			if (using_zbuf) clear_zbuf = false;
		}

		if (nbase) RenderBaseStructures (dev);

		if (prm.bAtm) {
			if (amb != prm.amb0col)
				dev->SetRenderState (D3DRENDERSTATE_AMBIENT, prm.amb0col);
		}

		if (prm.bFog) { // turn off fog
			dev->SetRenderState (D3DRENDERSTATE_FOGENABLE, FALSE);
			dev->SetRenderState (D3DRENDERSTATE_FOGTABLEMODE, D3DFOG_NONE);
		}

		if (ringpostrender) {
			// reset z-comparison function and disable z-buffer
			dev->SetRenderState (D3DRENDERSTATE_ZFUNC, D3DCMP_LESSEQUAL);
			dev->SetRenderState (D3DRENDERSTATE_ZENABLE, FALSE);
		}
		if (prm.bCloud && (prm.cloudvis & 2))
			RenderCloudLayer (dev, D3DCULL_CCW, prm);	  // render clouds from above
		if (hazemgr) hazemgr->Render (dev, mWorldScaled, true); // haze across planet disc
		if (ringpostrender) {
			// turn z-buffer on for ring system
			dev->SetRenderState (D3DRENDERSTATE_ZENABLE, TRUE);
			ringmgr->Render (dev, mWorldScaled);
			dev->SetRenderState (D3DRENDERSTATE_ZENABLE, FALSE);
			dev->SetRenderState (D3DRENDERSTATE_ZWRITEENABLE, FALSE);
		}
		if (clear_zbuf)
			dev->Clear (0, NULL, D3DCLEAR_ZBUFFER, 0, 1.0f, 0L);

		if (!nmlnml) dev->SetRenderState (D3DRENDERSTATE_NORMALIZENORMALS, FALSE);
	}
	return true;
}

// ==============================================================

void vPlanet::ActivateLabels(bool activate)
{
	if (surfmgr2 && *(int*)oapiGetObjectParam(hObj, OBJPRM_PLANET_LABELENGINE) == 2) {
		if (activate) surfmgr2->CreateLabels();
		else          surfmgr2->DeleteLabels();
	}
}

// ==============================================================

void vPlanet::RenderLabels(LPDIRECT3DDEVICE7 dev, oapi::Sketchpad *skp, oapi::Font **labelfont, int *fontidx)
{
	if (surfmgr2 && *(int*)oapiGetObjectParam(hObj, OBJPRM_PLANET_LABELENGINE) == 2)
		surfmgr2->RenderLabels(skp, labelfont, fontidx);
}

// ==============================================================

void vPlanet::RenderDot (LPDIRECT3DDEVICE7 dev)
{
	// to do
}

// ==============================================================

void vPlanet::RenderSphere (LPDIRECT3DDEVICE7 dev, const RenderPrm &prm, bool &using_zbuf)
{
	using_zbuf = false;

	if (mipmap_mode) {
		float fBias = (float)gc->Cfg()->PlanetMipmapBias;
		dev->SetTextureStageState (0, D3DTSS_MIPFILTER, mipmap_mode == 1 ? D3DTFP_POINT:D3DTFP_LINEAR);
		dev->SetTextureStageState (0, D3DTSS_MIPMAPLODBIAS, *((LPDWORD) (&fBias)) );
	}
	if (aniso_mode > 1) {
		dev->SetTextureStageState (0, D3DTSS_MAGFILTER, D3DTFG_ANISOTROPIC);
		dev->SetTextureStageState (0, D3DTSS_MINFILTER, D3DTFN_ANISOTROPIC);
		dev->SetTextureStageState (0, D3DTSS_MAXANISOTROPY, aniso_mode);
	}

	// for planets seen through an atmospheric layer from the surface of
	// another planet, add the ambient atmosphere colour to the rendering
	if (prm.bAddBkg) {
		dev->SetTextureStageState (1, D3DTSS_COLOROP, D3DTOP_ADD);
		dev->SetTextureStageState (1, D3DTSS_COLORARG1, D3DTA_CURRENT);
		dev->SetTextureStageState (1, D3DTSS_COLORARG2, D3DTA_TFACTOR);
		DWORD bgc = gc->GetScene()->BgColourRGBA();
		dev->SetRenderState (D3DRENDERSTATE_TEXTUREFACTOR, bgc);
	}

	if (surfmgr2) {
		if (cdist >= 1.3*rad && cdist > 3e6) {
			surfmgr2->Render (dmWorld, false, prm);
		} else {
			dev->SetRenderState (D3DRENDERSTATE_ZENABLE, TRUE);
			dev->SetRenderState (D3DRENDERSTATE_ZWRITEENABLE, TRUE);
			surfmgr2->Render (dmWorld, true, prm);
			dev->SetRenderState (D3DRENDERSTATE_ZENABLE, FALSE);
			dev->SetRenderState (D3DRENDERSTATE_ZWRITEENABLE, FALSE);
			using_zbuf = true;
		}
	} else {
		surfmgr->Render (dev, mWorldScaled, dist_scale, patchres, 0.0, prm.bFog); // surface
	}

	if (prm.bAddBkg) {
		dev->SetTextureStageState (1, D3DTSS_COLOROP, D3DTOP_DISABLE);
		dev->SetTextureStageState (1, D3DTSS_COLORARG1, D3DTA_TEXTURE);
		dev->SetTextureStageState (1, D3DTSS_COLORARG2, D3DTA_CURRENT);
	}

	if (nbase) {
		RenderBaseSurfaces (dev);                     // base surfaces
		RenderBaseShadows (dev, shadowalpha);         // base shadows
	}

	if (mipmap_mode) {
		float fBias = 0.0f;
		dev->SetTextureStageState (0, D3DTSS_MIPFILTER, D3DTFP_NONE);
		dev->SetTextureStageState (0, D3DTSS_MIPMAPLODBIAS, *((LPDWORD) (&fBias)) );
	}
	if (aniso_mode > 1) {
		dev->SetTextureStageState (0, D3DTSS_MAGFILTER, D3DTFG_LINEAR);
		dev->SetTextureStageState (0, D3DTSS_MINFILTER, D3DTFN_LINEAR);
		dev->SetTextureStageState (0, D3DTSS_MAXANISOTROPY, 1);
	}
	if (prm.bCloudShadow)
		RenderCloudShadows (dev, prm);                // cloud shadows
	if (bVesselShadow && hObj == oapiCameraProxyGbody())
	// cast shadows only on planet closest to camera
		scn->RenderVesselShadows (hObj, shadowalpha); // vessel shadows
}

// ==============================================================

void vPlanet::RenderCloudLayer (LPDIRECT3DDEVICE7 dev, DWORD cullmode, const RenderPrm &prm)
{
	if (cullmode != D3DCULL_CCW) dev->SetRenderState (D3DRENDERSTATE_CULLMODE, cullmode);
	dev->SetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, TRUE);
	if (cloudmgr2)
		cloudmgr2->Render (dmWorld, false, prm);
	else
		clouddata->cloudmgr->Render (dev, clouddata->mWorldC, dist_scale, min(patchres,8), clouddata->viewap); // clouds
	dev->SetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, FALSE);
	if (cullmode != D3DCULL_CCW) dev->SetRenderState (D3DRENDERSTATE_CULLMODE, D3DCULL_CCW);
}

void vPlanet::RenderVectors(LPDIRECT3DDEVICE7 dev)
{
	vObject::RenderVectors(dev);
	RenderBaseVectors(dev);
}

// ==============================================================

void vPlanet::RenderBaseVectors(LPDIRECT3DDEVICE7 dev)
{
	for (DWORD i = 0; i < nbase; i++)
		if (vbase[i])
			vbase[i]->RenderVectors(dev);
}

// ==============================================================

void vPlanet::RenderCloudShadows (LPDIRECT3DDEVICE7 dev, const RenderPrm &prm)
{
	if (cloudmgr2) {
		if (prm.bCloudFlatShadows)
			cloudmgr2->RenderFlatCloudShadows (dmWorld, prm);
	} else if (clouddata) { // legacy method
		D3DMATERIAL7 pmat;
		static D3DMATERIAL7 cloudmat = {{0,0,0,1},{0,0,0,1},{0,0,0,0},{0,0,0,0},0};

		float alpha = clouddata->shadowalpha;
		cloudmat.diffuse.a = cloudmat.ambient.a = alpha;

		dev->GetMaterial (&pmat);
		dev->SetMaterial (&cloudmat);

		DWORD ablend;
		dev->GetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, &ablend);
		if (!ablend)
			dev->SetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, TRUE);
		dev->SetTextureStageState (0, D3DTSS_ALPHAOP, D3DTOP_MODULATE);

		clouddata->cloudmgr->Render (dev, clouddata->mWorldC0, min(patchres,8), (int)clouddata->viewap);

		dev->SetTextureStageState (0, D3DTSS_ALPHAOP, D3DTOP_SELECTARG1);
		if (!ablend)
			dev->SetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, FALSE);
		dev->SetMaterial (&pmat);
	}
}

// ==============================================================

void vPlanet::RenderBaseSurfaces (LPDIRECT3DDEVICE7 dev)
{
	bool state_check = false;
	DWORD i, alpha;

	for (i = 0; i < nbase; i++) {
		if (vbase[i]) {
			if (!state_check) {
				dev->GetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, &alpha);
				if (!alpha)
					dev->SetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, TRUE);
				state_check = true;
			}
			vbase[i]->RenderSurface (dev);
		}
	}

	// restore render state
	if (state_check) {
		if (!alpha)
			dev->SetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, FALSE);
	}
}

// ==============================================================

void vPlanet::RenderBaseShadows (LPDIRECT3DDEVICE7 dev, float depth)
{
	// set device parameters
	DWORD stencilDepth = scn->GetStencilDepth();
	if (stencilDepth) {
		dev->SetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, TRUE);
		dev->SetRenderState (D3DRENDERSTATE_STENCILENABLE, TRUE);
		dev->SetRenderState (D3DRENDERSTATE_STENCILREF, 1);
		dev->SetRenderState (D3DRENDERSTATE_STENCILMASK, 1);
		dev->SetRenderState (D3DRENDERSTATE_STENCILFUNC, D3DCMP_NOTEQUAL);
		dev->SetRenderState (D3DRENDERSTATE_STENCILPASS, D3DSTENCILOP_REPLACE);
	} else {
		depth = 1; // without stencil buffer, use black shadows
	}

	dev->SetTextureStageState (0, D3DTSS_ALPHAARG1, D3DTA_TFACTOR);
	dev->SetRenderState (D3DRENDERSTATE_TEXTUREFACTOR, D3DRGBA(0,0,0,depth));
	dev->SetTextureStageState (0, D3DTSS_COLOROP, D3DTOP_SELECTARG1);
	dev->SetTextureStageState (0, D3DTSS_COLORARG1, D3DTA_TFACTOR);

	for (DWORD i = 0; i < nbase; i++)
		if (vbase[i])
			vbase[i]->RenderGroundShadow (dev);

	// reset device parameters
	if (stencilDepth) {
		dev->SetRenderState (D3DRENDERSTATE_STENCILENABLE, FALSE);
	} else {
		dev->SetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, TRUE);
	}
	dev->SetTextureStageState (0, D3DTSS_ALPHAARG1, D3DTA_TEXTURE);
	dev->SetTextureStageState (0, D3DTSS_COLOROP, D3DTOP_MODULATE);
	dev->SetTextureStageState (0, D3DTSS_COLORARG1, D3DTA_TEXTURE);
}

// ==============================================================

void vPlanet::RenderBaseStructures (LPDIRECT3DDEVICE7 dev)
{
	bool zmod = false, zcheck = false;
	DWORD bz, bzw;

	for (DWORD i = 0; i < nbase; i++) {
		if (vbase[i]) {
			if (!zcheck) { // enable zbuffer
				dev->GetRenderState (D3DRENDERSTATE_ZENABLE, &bz);
				dev->GetRenderState (D3DRENDERSTATE_ZWRITEENABLE, &bzw);
				if (!bz || !bzw) {
					dev->SetRenderState (D3DRENDERSTATE_ZENABLE, TRUE);
					dev->SetRenderState (D3DRENDERSTATE_ZWRITEENABLE, TRUE);
					//scn->GetCamera()->SetFustrumLimits (1, 1e5);
					zmod = true;
				}
				zcheck = true;
			}
			vbase[i]->RenderStructures (dev);
		}
	}
	if (zmod) {
		dev->SetRenderState (D3DRENDERSTATE_ZENABLE, bz);
		dev->SetRenderState (D3DRENDERSTATE_ZWRITEENABLE, bzw);
		//scn->GetCamera()->SetFustrumLimits (10, 1e6);
	}
}

// ==============================================================

bool vPlanet::ModLighting (DWORD &ambient)
{
	// modify ambient light level inside atmospheres as a function of sun elevation
	if (!prm.bAtm) return false;
	if (cdist >= size+prm.atm_href) return false;

	double alpha = acos (dotp (unit(*scn->GetCamera()->GetGPos()), -unit(cpos)));
	// angular distance between sun and planet as seen from camera

	double sunelev = alpha - PI05; // elevation of sun above horizon (assuming camera on ground)
	if (sunelev < -14.0*RAD) return false;  // total darkness

	double rscale = (size-cdist)/prm.atm_href + 1.0;    // effect altitude scale (1 on ground, 0 at reference alt)
	double amb = prm.atm_amb0 * min (1.0, (sunelev+14.0*RAD)/(20.0*RAD)); // effect magnitude (dependent on sun elevation)
	if (amb < 0.05) return false;
	amb = max (0, amb-0.05);

	DWORD addamb = (DWORD)(amb*rscale*256.0);
	DWORD newamb = *(DWORD*)gc->GetConfigParam (CFGPRM_AMBIENTLEVEL) + addamb;
	ambient = 0;
	for (int i = 0; i < 4; i++)
		ambient |= min (255, newamb) << (i<<3);
	return true;
}
