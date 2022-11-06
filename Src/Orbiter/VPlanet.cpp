// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#define STRICT 1
#define OAPI_IMPLEMENTATION

#define D3D_OVERLOADS
#include <d3d.h>
#include "Util.h"
#include "D3dmath.h"
#include "Orbiter.h"
#include "Config.h"
#include "Vecmat.h"
#include "Camera.h"
#include "Scene.h"
#include "Planet.h"
#include "Psys.h"
#include "VPlanet.h"
#include "VBase.h"
#include "Spherepatch.h"
#include "TileMgr.h"
#include "surfmgr2.h"
#include "cloudmgr2.h"
#include "Log.h"
#include "OGraphics.h"
#include <stdio.h>
#include <fstream>
#include <iomanip>

extern Orbiter *g_pOrbiter;
extern PlanetarySystem *g_psys;
extern double g_farplane;
extern Camera *g_camera;
extern TimeData td;
extern int patchidx[9];
extern char DBG_MSG[256];

static double max_surf_dist = 1e4;
static double max_centre_dist = 0.9*g_farplane;
static bool basetilevis[364];

bool VPlanet::bstencil = false;
int VPlanet::mipmap_mode = 2;
int VPlanet::aniso_mode = 1;

// =======================================================================
// Class VPlanet

VPlanet::VPlanet (const Planet *_planet): VObject (_planet)
{
	planet         = _planet;
	pprf           = g_pOrbiter->Cfg()->CfgVisualPrm.PlanetPatchRes;
	dist_scale     = 1.0;
	tCheckRes      = -1.0;
	tCheckRingSh   = -1.0;
	max_patchres   = min (SURF_MAX_PATCHLEVEL2, planet->max_patch_level);
	prm.bAtm = planet->HasAtmosphere();
	if (prm.bAtm) {
		prm.atm_href = log(planet->atm.rho0)*2e4 + 2e4;
	}
	prm.bCloud = planet->bHasCloudlayer;
	if (prm.bCloud) {
		prm.bCloudShadow = g_pOrbiter->Cfg()->CfgVisualPrm.bCloudShadows;
		prm.bBrightClouds = planet->bBrightClouds;
		prm.cloudalt = planet->cloudalt;
		cloudformat = (planet->CloudMgr2() ? 2 : 1);
		min_cloudres = planet->min_cloud_level;
		max_cloudres = planet->max_cloud_level;
	} else {
		prm.bCloudShadow = false;
		cloudformat = 0;
	}
	prm.horizon_excess = planet->horizon_excess;
	prm.horizon_minrad = min (1.0+planet->minelev/planet->size, 1.0-1e-4);
	hashaze        = g_pOrbiter->Cfg()->CfgVisualPrm.bHaze && planet->HasAtmosphere();
	hasfog         = g_pOrbiter->Cfg()->CfgVisualPrm.bFog && planet->HasAtmosphere() && (planet->fog.dens_0 > 0.0);
	hasrings       = planet->bHasRings;
	maxdist        = max_surf_dist + planet->Size();
	if (max_centre_dist > maxdist)
		maxdist    = max_centre_dist;
	patchres       = 0;
	cloudres       = 0;
	ringres        = 0;
	renderpix      = false;
	cloudmanager   = 0;
	horizonmanager = 0;
	ringmanager    = 0;
	nshvtx         = 0;
	mesh           = 0;
	shadowalpha    = (float)planet->shadowalpha;
	bstencil       = (g_pOrbiter->Cfg()->CfgDevPrm.bTryStencil && gc->GetStencilDepth() > 0);
	mipmap_mode    = g_pOrbiter->Cfg()->CfgPRenderPrm.MipmapMode;
	aniso_mode     = g_pOrbiter->Cfg()->CfgPRenderPrm.AnisoMode;
	if (max_patchres == 0) { // check for explicit mesh
		mesh = new Mesh; TRACENEW
		if (!LoadMesh (planet->Name(), *mesh)) {
			delete mesh; mesh = 0;
		}
	}
}

VPlanet::~VPlanet ()
{
	if (cloudmanager) delete cloudmanager;
	if (horizonmanager) delete horizonmanager;
	if (ringmanager) delete ringmanager;
	if (nshvtx) delete []shvtx;
	if (mesh) delete mesh;
}

void VPlanet::Update (bool moving, bool force)
{
	double rad_scale = body->Size();
	bool rescale = false;
	int i, j;

	VObject::Update (moving, force);

	dist_scale = 1.0;

	dmWorld_scaled = dmWorld;
	mWorld_scaled = mWorld;

	// If planet is outside far clipping plane we must rescale
	// distance and diameter
	if (hasrings && cdist/planet->size > 1.01) {
		double rdist = cdist + planet->size*planet->ringmax;
		if (rdist > g_farplane*0.1) { // new: scaling by 0.1 to reduce render artefacts
			rescale = true;
			dist_scale = (FLOAT)(g_farplane*0.1/(cdist + planet->size*planet->ringmax));
		}
		// calculate ring shadow
		if (td.SimT0 > tCheckRingSh) {
			ShadowPlanetOnRing (shvtx, nshvtx);
			tCheckRingSh = td.SimT0 + 86400.0; // once a day should be enough
		}
	} else {
		if (cdist > maxdist) {
			rescale = true;
			dist_scale = (FLOAT)(max_centre_dist/cdist);
		}
	}
	if (rescale) {
		rad_scale *= dist_scale;
		mWorld_scaled._41 = (float)(dmWorld_scaled.m41 *= dist_scale);
		mWorld_scaled._42 = (float)(dmWorld_scaled.m42 *= dist_scale);
		mWorld_scaled._43 = (float)(dmWorld_scaled.m43 *= dist_scale);
	}
	// scale up sphere radius from 1 to planet radius
	mWorld_scaled._11 = (float)(dmWorld_scaled.m11 *= rad_scale);  mWorld_scaled._12 = (float)(dmWorld_scaled.m12 *= rad_scale);  mWorld_scaled._13 = (float)(dmWorld_scaled.m13 *= rad_scale);
	mWorld_scaled._21 = (float)(dmWorld_scaled.m21 *= rad_scale);  mWorld_scaled._22 = (float)(dmWorld_scaled.m22 *= rad_scale);  mWorld_scaled._23 = (float)(dmWorld_scaled.m23 *= rad_scale);
	mWorld_scaled._31 = (float)(dmWorld_scaled.m31 *= rad_scale);  mWorld_scaled._32 = (float)(dmWorld_scaled.m32 *= rad_scale);  mWorld_scaled._33 = (float)(dmWorld_scaled.m33 *= rad_scale);

	if (cloudformat) {
		double cloudrad = planet->cloudalt+planet->size;
		cloudcam = (cdist < cloudrad ? 1:0);
		if (cdist > cloudrad*(1.0-1.5e-4)) cloudcam |= 2;
		prm.bCloudFlatShadows = (cdist >= 1.05*planet->size);
		if (cloudres) {
			FLOAT cloudscale = (FLOAT)(cloudrad/planet->size);
			if (cloudcam & 1) {
				cloudvis = acos (planet->size/cloudrad);
				if (planet->size < cdist) cloudvis += acos (planet->size/cdist);
			} else
				cloudvis = 0;  // use default calculation
			memcpy (&mWorldC0, &mWorld_scaled, sizeof (D3DMATRIX));
			if (planet->cloudrot) {
				static D3DMATRIX crot (1,0,0,0, 0,1,0,0, 0,0,1,0, 0,0,0,1);
				crot._11 =   crot._33 = (float)cos(planet->cloudrot);
				crot._13 = -(crot._31 = (float)sin(planet->cloudrot));
				D3DMath_MatrixMultiply (mWorldC0, mWorldC0, crot);
			}
			memcpy (&mWorldC, &mWorldC0, sizeof (D3DMATRIX));
			for (i = 0; i < 3; i++)
				for (j = 0; j < 3; j++) {
					mWorldC.m[i][j] *= cloudscale;
				}
		
			if (cloudmanager && planet->bCloudMicrotex) {
				double alt = cdist-planet->size;
				double lvl = (planet->cloud_micro_alt1-alt)/(planet->cloud_micro_alt1-planet->cloud_micro_alt0);
				cloudmanager->SetMicroStructure (lvl > 0.0 ? MICROSTRUCT_CLOUD : 0);
				cloudmanager->SetMicroLevel (max (0, min (1, lvl)));
			}
		}
	}
}

void VPlanet::CheckResolution (double iar)
{
	double alt = max (1.0, cdist-planet->Size());
	double apr = planet->Size() * g_pOrbiter->ViewH()*0.5 / (alt * g_camera->TanAperture());
	// apparent planet radius in units of screen pixels

	int new_patchres, new_cloudres, new_ringres;
	double ntx;

	if (apr < 3.0) {  // render planet as 2x2 pixels
		renderpix = true;
		ntx = 0;
		new_patchres = 0;
	} else {          // full sphere rendering
		renderpix = false;
		ntx = Pi2 * apr;  // number of texels required in texture map along equator to have 1 texel per screen pixel
		ntx *= pprf;      // user-supplied resolution factor

		static const double scal2 = 1.0/log(2.0);
		const double shift = (planet->SurfMgr2() ? 6.0 : 5.0); // reduce level for tile mgr v2, because of increased patch size
		new_patchres = min (max ((int)(scal2*log(ntx)-shift),1), max_patchres);
	}

	if (new_patchres > max_patchres)
		new_patchres = max_patchres;

	if (new_patchres != patchres) {

		if (cloudformat == 1) {
			new_cloudres = new_patchres;
			if      (new_cloudres < min_cloudres) new_cloudres = 0;
			else if (new_cloudres > max_cloudres) new_cloudres = max_cloudres;
			if (new_cloudres != cloudres) {
				if (cloudmanager) {
					delete cloudmanager;
					cloudmanager = 0;
				}
				if (new_cloudres) {
					char cbuf[256];
					strcpy (cbuf, body->Name()); strcat (cbuf, "_cloud");
					LPDIRECTDRAWSURFACE7 *tex = planet->cloudtex + patchidx[new_cloudres-1]-patchidx[min_cloudres-1];
					switch (new_cloudres) {
					case 1: cloudmanager = new PatchManager1 (cbuf, tex); TRACENEW break;
					case 2: cloudmanager = new PatchManager2 (cbuf, tex); TRACENEW break;
					case 3: cloudmanager = new PatchManager3 (cbuf, tex); TRACENEW break;
					case 4: cloudmanager = new PatchManager4 (cbuf, tex); TRACENEW break;
					case 5: cloudmanager = new PatchManager5 (cbuf, tex); TRACENEW break;
					case 6: cloudmanager = new PatchManager6 (cbuf, tex); TRACENEW break;
					case 7: cloudmanager = new PatchManager7 (cbuf, tex); TRACENEW break;
					case 8: cloudmanager = new PatchManager8 (cbuf, tex); TRACENEW break;
					}
				}
				cloudres = new_cloudres;
			}
		}

		if (hashaze)
			if (new_patchres < 1) {
				if (horizonmanager) { delete horizonmanager; horizonmanager = 0; }
			} else {
				if (!horizonmanager) { horizonmanager = new HorizonManager (planet, this); TRACENEW }
			}

		if (hasrings) {
			new_ringres = (new_patchres <= 2 ? 1 : new_patchres <= 4 ? 2 : 3);
			if (new_ringres != ringres) {
				if (ringmanager) {
					delete ringmanager;
					ringmanager = 0;
				}
				switch (new_ringres) {
				case 1:
					ringmanager = new RingManager8 (body->Name(), (float)planet->ringmin, (float)planet->ringmax,
						planet->ringtex ? planet->ringtex+0 : 0); TRACENEW
					break;
				case 2:
					ringmanager = new RingManager12 (body->Name(), (float)planet->ringmin, (float)planet->ringmax,
						planet->ringtex ? planet->ringtex+1 : 0); TRACENEW
					break;
				case 3:
					ringmanager = new RingManager16 (body->Name(), (float)planet->ringmin, (float)planet->ringmax,
						planet->ringtex ? planet->ringtex+2 : 0); TRACENEW
					break;
				}
				ringres = new_ringres;
			}
		}
		patchres = new_patchres;
	}
}

void VPlanet::Render (LPDIRECT3DDEVICE7 dev)
{
	if (renderpix) { // render planet as 2x2 pixels

		RenderAsDisc (dev);
		//RenderAsPixel (dev);

	} else {         // render planet as 3D sphere

		DWORD nmlnml = TRUE;
		if (mesh || !planet->SurfMgr2()) {
			// old-style and mesh-based planet surfaces use a rescaled world matrix,
			// so we need to make sure that normals are renormalised
			dev->GetRenderState (D3DRENDERSTATE_NORMALIZENORMALS, &nmlnml);
			if (!nmlnml) dev->SetRenderState (D3DRENDERSTATE_NORMALIZENORMALS, TRUE);
		}

		bool render_ring = false;
		bool clear_zbuf = false;
		bool bkgcol = false;
		float fogfactor;

		prm.bFog = hasfog;
		prm.bTint = hasfog;

		const VECTOR3 &bgcol = scene->BGcol();
		if (bgcol.x || bgcol.y || bgcol.z)
			bkgcol = true;

		if (!mesh) { // disable z-buffer
			if (ringmanager) {
				if (cdist < planet->ringmin * planet->Size()) { // camera inside inner ring edge
					dev->SetRenderState (D3DRENDERSTATE_ZENABLE, FALSE);
					dev->SetRenderState (D3DRENDERSTATE_ZWRITEENABLE, FALSE);
					RenderRing (dev, bkgcol);
				} else {
					// if the planet has a ring system we update the z-buffer
					// but don't do z-checking for the planet surface
					dev->SetRenderState (D3DRENDERSTATE_ZFUNC, D3DCMP_ALWAYS);
					render_ring = true;
					clear_zbuf = true;
				}
			} else {
				dev->SetRenderState (D3DRENDERSTATE_ZENABLE, FALSE);
				dev->SetRenderState (D3DRENDERSTATE_ZWRITEENABLE, FALSE);
			}
		}
		if (horizonmanager) horizonmanager->Render (dev, mWorld_scaled);

		DWORD amb;
		bool bModAmbient = ModLighting (amb);

		if (prm.bCloud) { // render cloud layer?
			prm.cloudrot = planet->cloudrot;
		}

		if (prm.bFog) { // render distance fog?
 			double R = planet->Size();
			double h = max (1.0, cdist-R);

			VECTOR3 fogcol = planet->fog.col;
			double h_ref = planet->fog.alt_ref;   // 3e3;
			double fog_0 = planet->fog.dens_0;    // 5e-5;
			double fog_ref = planet->fog.dens_ref; // 3e-5;
			double h_max = R*1.5; // At this altitude, fog effect drops to zero
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

				//a = -h_ref * (fog_ref-fog_0);
				//b = a /fog_ref - h_ref;
				//c = 0.0;
				fogfactor = (float)(a/(h+b)+c);
			}

			if (fogfactor < 0.0) prm.bFog = false;
			else {
				// day/nighttime fog lighting
				double cosa = dotp (planet->GPos().unit(), (planet->GPos() - g_camera->GPos()).unit());
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
			prm.rgbTint = planet->tintcol;
 			double R = planet->Size();
			double alt = cdist - R;
			double alt_ref1 = planet->fog.alt_ref*5.0;
			double alt_ref2 = alt_ref1 * 0.1;
			if (alt < alt_ref1) {
				double scale = (alt-alt_ref2)/(alt_ref1-alt_ref2);
				if (scale <= 0.0) prm.bTint = false;
				else prm.rgbTint *= scale;
			}
		}
		if (cloudformat && cloudcam & 1) RenderClouds (dev, D3DCULL_CW, prm); // render clouds from below

		if (bModAmbient)
			dev->SetRenderState (D3DRENDERSTATE_AMBIENT, amb);

		if (mesh) {
			dev->SetTransform (D3DTRANSFORMSTATE_WORLD, &mWorld_scaled);
			mesh->Render (dev);
		} else {
			prm.bAddBkg = (bkgcol && planet != g_camera->ProxyPlanet());
			if (mipmap_mode) {
				float fBias = (float)g_pOrbiter->Cfg()->CfgPRenderPrm.MipmapBias;
				dev->SetTextureStageState (0, D3DTSS_MIPFILTER, mipmap_mode == 1 ? D3DTFP_POINT:D3DTFP_LINEAR);
				dev->SetTextureStageState (0, D3DTSS_MIPMAPLODBIAS, *((LPDWORD) (&fBias)) );
			}
			if (aniso_mode > 1) {
				dev->SetTextureStageState (0, D3DTSS_MAGFILTER, D3DTFG_ANISOTROPIC);
				dev->SetTextureStageState (0, D3DTSS_MINFILTER, D3DTFN_ANISOTROPIC);
				dev->SetTextureStageState (0, D3DTSS_MAXANISOTROPY, aniso_mode);
			}

			if (planet->SurfMgr2())
				if (cdist >= 1.3*planet->Size() && cdist > 3e6) {// render without zbuffer
					planet->SurfMgr2()->Render (dev, dmWorld, this, false, prm);
				} else {
					dev->SetRenderState (D3DRENDERSTATE_ZENABLE, TRUE);
					dev->SetRenderState (D3DRENDERSTATE_ZWRITEENABLE, TRUE);
					planet->SurfMgr2()->Render (dev, dmWorld, this, true, prm);
					dev->SetRenderState (D3DRENDERSTATE_ZENABLE, FALSE);
					dev->SetRenderState (D3DRENDERSTATE_ZWRITEENABLE, FALSE);
					clear_zbuf = false;
				}
			else
				planet->TileMgr()->Render (dev, mWorld_scaled, dist_scale, this, patchres, prm.bAddBkg, prm.bFog);

			dev->SetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, TRUE);
			RenderBaseSurfaceTiles (dev);
			RenderBaseSurfaceDecals (dev);
			RenderBaseShadows (dev, shadowalpha);
			dev->SetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, FALSE);

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

			if (g_pOrbiter->Cfg()->CfgVisualPrm.bVesselShadows && planet == g_camera->ProxyPlanet()) {
				scene->RenderVesselShadows();
				dev->SetTransform (D3DTRANSFORMSTATE_WORLD, &mWorld);
			}
			if (prm.bCloudShadow)
				RenderCloudShadows (dev);
		}

		RenderBaseStructures (dev);

		if (bModAmbient)
			dev->SetRenderState (D3DRENDERSTATE_AMBIENT, g_pOrbiter->Cfg()->AmbientColour);

		if (prm.bFog) { // turn off fog
			dev->SetRenderState (D3DRENDERSTATE_FOGENABLE, FALSE);
			dev->SetRenderState (D3DRENDERSTATE_FOGTABLEMODE, D3DFOG_NONE);
		}

		if (render_ring) {
			// reset z-comparison function and disable z-buffer
			dev->SetRenderState (D3DRENDERSTATE_ZFUNC, D3DCMP_LESSEQUAL);
			dev->SetRenderState (D3DRENDERSTATE_ZENABLE, FALSE);
		}
		if (cloudformat && cloudcam & 2) RenderClouds (dev, D3DCULL_CCW, prm); // render clouds from above
		if (horizonmanager) horizonmanager->Render (dev, mWorld_scaled, true);

		// turn z-buffer back on
		dev->SetRenderState (D3DRENDERSTATE_ZENABLE, TRUE);
		dev->SetRenderState (D3DRENDERSTATE_ZWRITEENABLE, TRUE);

		if (render_ring) RenderRing (dev, bkgcol);
		if (clear_zbuf)
			dev->Clear (0, NULL, D3DCLEAR_ZBUFFER, 0, 1.0f, 0L);

		if (!nmlnml) dev->SetRenderState (D3DRENDERSTATE_NORMALIZENORMALS, FALSE);
	}
}

void VPlanet::RenderClouds (LPDIRECT3DDEVICE7 dev, DWORD cullmode, const RenderPrm &prm)
{
	if (cullmode != D3DCULL_CCW) dev->SetRenderState (D3DRENDERSTATE_CULLMODE, cullmode);
	dev->SetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, TRUE);
	if (cloudformat == 1) {
		if (cloudmanager) cloudmanager->Render (dev, mWorldC, cloudvis);
	} else {
		planet->CloudMgr2()->Render (dev, dmWorld, this, false, prm);
	}
	dev->SetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, FALSE);
	if (cullmode != D3DCULL_CCW) dev->SetRenderState (D3DRENDERSTATE_CULLMODE, D3DCULL_CCW);
}

void VPlanet::RenderCloudShadows (LPDIRECT3DDEVICE7 dev)
{
	if (planet->CloudMgr2()) {
		if (prm.bCloudFlatShadows)
			planet->CloudMgr2()->RenderFlatCloudShadows (dev, dmWorld, this);
	} else if (cloudmanager) { // legacy style
		D3DMATERIAL7 pmat;
		static D3DMATERIAL7 cloudmat = {{0,0,0,1},{0,0,0,1},{0,0,0,0},{0,0,0,0},0};
	
		float alpha = 1.0f - planet->cloudshadowcol;
		if (alpha < 0.01f) return; // don't render cloud shadows for this planet
		cloudmat.diffuse.a = cloudmat.ambient.a = alpha;

		dev->GetMaterial (&pmat);
		dev->SetMaterial (&cloudmat);

		dev->SetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, TRUE);
		dev->SetTextureStageState (0, D3DTSS_ALPHAOP, D3DTOP_MODULATE);
		cloudmanager->Render (dev, mWorldC0, cloudvis);
		dev->SetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, FALSE);
		dev->SetTextureStageState (0, D3DTSS_ALPHAOP, D3DTOP_SELECTARG1);

		dev->SetMaterial (&pmat);
	} 
}

void VPlanet::RenderVectors(LPDIRECT3DDEVICE7 dev)
{
	VObject::RenderVectors(dev);
	RenderBaseVectors(dev);
}

void VPlanet::RenderBaseVectors(LPDIRECT3DDEVICE7 dev)
{
	for (DWORD i = 0; i < planet->nBase(); i++) {
		VBase* vbase = (VBase*)planet->GetBase(i)->GetVishandle();
		if (vbase) vbase->RenderVectors(dev);
	}
}

void VPlanet::RenderBaseSurfaceTiles (LPDIRECT3DDEVICE7 dev)
{
	for (DWORD i = 0; i < planet->nBase(); i++) {
		VBase *vbase = (VBase*)planet->GetBase(i)->GetVishandle();
		if (vbase) vbase->RenderSurfaceTiles (dev);
	}
}

void VPlanet::RenderBaseSurfaceDecals (LPDIRECT3DDEVICE7 dev)
{
	for (DWORD i = 0; i < planet->nBase(); i++) {
		VBase *vbase = (VBase*)planet->GetBase(i)->GetVishandle();
		if (vbase) vbase->RenderSurfaceDecals (dev);
	}
}

void VPlanet::RenderBaseShadows (LPDIRECT3DDEVICE7 dev, float alpha)
{
	DWORD isalpha, isalphacur;
	dev->GetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, &isalpha);
	isalphacur = isalpha;
	bool setup_done = false;

	for (DWORD i = 0; i < planet->nBase (); i++) {
		VBase *vbase = (VBase*)planet->GetBase(i)->GetVishandle();
		if (vbase) {
			if (!setup_done) {
				if (alpha < 1.0f && bstencil) {
					if (isalphacur != TRUE)
						dev->SetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, isalphacur = TRUE);
					dev->SetRenderState (D3DRENDERSTATE_STENCILENABLE, TRUE);
					dev->SetRenderState (D3DRENDERSTATE_STENCILREF, 0x1);
					dev->SetRenderState (D3DRENDERSTATE_STENCILMASK, 0x1);
					dev->SetRenderState (D3DRENDERSTATE_STENCILFUNC, D3DCMP_NOTEQUAL);
					dev->SetRenderState (D3DRENDERSTATE_STENCILPASS, D3DSTENCILOP_REPLACE);
				} else // can't do semitransparent shadows without stencil buffer
					alpha = 1.0f;
				dev->SetTextureStageState (0, D3DTSS_ALPHAARG1, D3DTA_TFACTOR);
				dev->SetRenderState (D3DRENDERSTATE_TEXTUREFACTOR, D3DRGBA(0,0,0,alpha));
				dev->SetTextureStageState (0, D3DTSS_COLOROP, D3DTOP_SELECTARG1);
				dev->SetTextureStageState (0, D3DTSS_COLORARG1, D3DTA_TFACTOR);
				setup_done = true;
			}
			vbase->RenderShadows (dev);
		}
	}
	if (setup_done) {
		if (isalphacur != isalpha)
			dev->SetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, isalpha);
		dev->SetRenderState (D3DRENDERSTATE_STENCILENABLE, FALSE);
		dev->SetTextureStageState (0, D3DTSS_ALPHAARG1, D3DTA_TEXTURE);
		dev->SetTextureStageState (0, D3DTSS_COLOROP, D3DTOP_MODULATE);
		dev->SetTextureStageState (0, D3DTSS_COLORARG1, D3DTA_TEXTURE);
	}
}

void VPlanet::RenderBaseStructures (LPDIRECT3DDEVICE7 dev)
{
	bool zmod = false, zcheck = false;
	DWORD i, bz, bzw;

	for (i = 0; i < planet->nBase (); i++) {
		Base *base = planet->GetBase (i);
		VBase *vbase = (VBase*)base->GetVishandle();
		if (vbase) {
			if (!zcheck) { // enable zbuffer
				dev->GetRenderState (D3DRENDERSTATE_ZENABLE, &bz);
				dev->GetRenderState (D3DRENDERSTATE_ZWRITEENABLE, &bzw);
				if (!bz || !bzw) {
					dev->SetRenderState (D3DRENDERSTATE_ZENABLE, TRUE);
					dev->SetRenderState (D3DRENDERSTATE_ZWRITEENABLE, TRUE);
					zmod = true;
				}
				zcheck = true;
			}
			vbase->RenderStructures (dev);
		}
	}
	if (zmod) {
		dev->SetRenderState (D3DRENDERSTATE_ZENABLE, bz);
		dev->SetRenderState (D3DRENDERSTATE_ZWRITEENABLE, bzw);
	}
}

void VPlanet::RenderLabels(oapi::Sketchpad *skp, oapi::Font **labelfont, int *fontidx)
{
	if (planet->SurfMgr2() && planet->LabelFormat() == 2)
		planet->SurfMgr2()->RenderLabels (skp, labelfont, fontidx);
}

// auxiliary function
inline void Map (VERTEX_XYZC *vtx, double x, double y, double cosp, double sinp)
{
	vtx->x = (float)(x*cosp-y*sinp);
	vtx->z = (float)(x*sinp+y*cosp);
	vtx->y = 0.0;
	vtx->col = D3DRGBA(0,0,0,0.9);
}

void VPlanet::RenderRing (LPDIRECT3DDEVICE7 dev, bool addbkg)
{
	DWORD amb = g_pOrbiter->Cfg()->AmbientColour;
	DWORD bpp = g_pOrbiter->ViewBPP();
	Vector ppos (tmul (planet->GRot(), -cpos)); // camera pos in planet coords
	Vector spos (tmul (planet->GRot(), -planet->GPos()));                 // sun pos in planet coords
	bool islit = (ppos.y*spos.y >= 0.0); // we are facing the lit side of the rings
	static D3DMATRIX imat, *ringmat;
	if (ppos.y >= 0) { // camera above equator
		ringmat = &mWorld_scaled;
	} else {             // flip rings
		int i;
		for (i = 0; i < 4; i++) imat.m[0][i] =  mWorld_scaled.m[0][i];
		for (i = 0; i < 4; i++) imat.m[1][i] = -mWorld_scaled.m[1][i];
		for (i = 0; i < 4; i++) imat.m[2][i] = -mWorld_scaled.m[2][i];
		for (i = 0; i < 4; i++) imat.m[3][i] =  mWorld_scaled.m[3][i];
		ringmat = &imat;
	}
	if (islit) // hack up ambient light on lit side to lighten up rings
		// note that this will create visual artefacts for edge-on illumination,
		// since one side will be 4x brighter than the other
		dev->SetRenderState (D3DRENDERSTATE_AMBIENT, amb*4);
	dev->SetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, TRUE);
	if (addbkg) {
		dev->SetRenderState (D3DRENDERSTATE_DESTBLEND, D3DBLEND_ONE);
	}

	ringmanager->Render (dev, *ringmat);
	if (islit) {
		dev->SetRenderState (D3DRENDERSTATE_AMBIENT, 0);

		// undo planet rotation from world matrix
		float cosr = (float)cos(planet->rotation), sinr = (float)sin(planet->rotation);
		memcpy (&imat, &mWorld_scaled, sizeof (D3DMATRIX));
		imat.m[0][0] =  cosr*mWorld_scaled.m[0][0] - sinr*mWorld_scaled.m[2][0];
		imat.m[0][1] =  cosr*mWorld_scaled.m[0][1] - sinr*mWorld_scaled.m[2][1];
		imat.m[0][2] =  cosr*mWorld_scaled.m[0][2] - sinr*mWorld_scaled.m[2][2];
		imat.m[2][0] =  sinr*mWorld_scaled.m[0][0] + cosr*mWorld_scaled.m[2][0];
		imat.m[2][1] =  sinr*mWorld_scaled.m[0][1] + cosr*mWorld_scaled.m[2][1];
		imat.m[2][2] =  sinr*mWorld_scaled.m[0][2] + cosr*mWorld_scaled.m[2][2];
		dev->SetTransform (D3DTRANSFORMSTATE_WORLD, &imat);

		dev->SetTexture (0, 0);
		static DWORD shadowbias = (bpp <= 16 ? 1 : 4);
		dev->SetRenderState (D3DRENDERSTATE_ZBIAS, shadowbias);
		dev->DrawPrimitive (D3DPT_TRIANGLESTRIP, D3DFVF_XYZ | D3DFVF_DIFFUSE,
			shvtx, nshvtx, 0);
		dev->SetRenderState (D3DRENDERSTATE_ZBIAS, 0);
		dev->SetRenderState (D3DRENDERSTATE_AMBIENT, amb);
	}
	dev->SetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, FALSE);
	if (addbkg) {
		dev->SetRenderState (D3DRENDERSTATE_DESTBLEND, D3DBLEND_INVSRCALPHA);
	}
}

int VPlanet::ShadowPlanetOnRing (VERTEX_XYZC *&vtx, DWORD &nvtx)
{
	// all lengths are scaled to a planet radius of 1

	if (!hasrings) return 0; // sanity check

	double r0 = planet->ringmin; // inner rim radius
	double r1 = planet->ringmax; // outer rim radius

	// sun in planet local coords
	Vector spos (tmul (planet->R_ecl, -planet->GPos()));
	bool flip = (spos.y < 0.0); // is underside of rings lit?

	// semi-minor axis of shadow ellipse (= planet radius)
	double b = 1.0;
	double b2 = b*b;

	// square of semi-major axis of shadow ellipse
	double a2 = b2*spos.length2()/(spos.y*spos.y);
	double a = sqrt (a2);

	// intersection of shadow with inner rim
	double i2 = r0*r0;
	if (a2 <= i2) {
		if (nvtx) { delete []vtx; nvtx = 0; }
		return 0; // shadow doesn't reach inner rim
	}
	double x2 = (i2-b2)*a2/(a2-b2);
	double y2 = i2-x2;
	double x = sqrt(x2), y = sqrt(y2);

	int nn = (int)(y*10.0); // 10: sampling resolution (change as required)
	DWORD nnvtx = 4*nn;

	// calculate vertices
	if (nvtx != nnvtx) {
		if (nvtx) delete []vtx;
		if (nvtx = nnvtx)
			vtx = new VERTEX_XYZC[nvtx]; TRACENEW
	}
	if (!nvtx) return 0;

	double phi = atan2 (spos.z,spos.x) + Pi;
	double cosp = cos (phi), sinp = sin (phi);

	double dtheta = atan(y/x)/nn; // angular sampling steps on the inner rim
	double theta, xi, yi, xo, yo, xe;
	int i, idx0, idx1, idx2, idx3;

	Map (vtx+0, x, y, cosp, sinp);
	Map (vtx+(nvtx-1), x, -y, cosp, sinp);
	if (!flip) idx0 = nn*2, idx1 = idx0-1;
	else       idx1 = nn*2, idx0 = idx1-1;
	Map (vtx+idx0, r0, 0.0, cosp, sinp);
	Map (vtx+idx1, (a < r1 ? a : r1), 0.0, cosp, sinp);

	bool outerrim = false;
	for (i = 1; i < nn; i++) {
		theta = (nn-i)*dtheta;
		xi = r0*cos(theta);
		yi = yo = r0*sin(theta);      // (xi,yi) sampling point on inner rim
		xo = sqrt (r1*r1-yi*yi);      // (xo,yo) sampling point on outer rim
		if (!outerrim) {
			xe = a * sqrt (1.0-yi*yi/b2); // (xe,yi) sampling point on shadow ellipse
			if (xe < xo) {
				xo = xe;
			} else {
				double o2 = r1*r1; // intersection of shadow with outer rim
				x2 = (o2-b2)*a2/(a2-b2);
				y2 = o2-x2;
				xo = sqrt(x2), yo = sqrt(y2);
				outerrim = true;
			}
		}
		if (!flip) {
			idx0 = i*2; idx1 = idx0-1;
			idx2 = nvtx-i*2; idx3 = idx2-1;
		} else {
			idx1 = i*2; idx0 = idx1-1;
			idx3 = nvtx-i*2; idx2 = idx3-1;
		}
		Map (vtx+idx0, xi,  yi, cosp, sinp);
		Map (vtx+idx1, xo,  yo, cosp, sinp);
		Map (vtx+idx2, xi, -yi, cosp, sinp);
		Map (vtx+idx3, xo, -yo, cosp, sinp);
	}
	return nvtx;
}

bool VPlanet::ModLighting (DWORD &ambient)
{
	// Eventually we may need to modify the direct light levels (diffuse+specular) as
	// well. In that case we should work directly on a D3DLIGHT7 structure instead of just
	// the ambient level, in a similar way to VBase::ModLighting()

	if (!planet->HasAtmosphere()) return false;
	if (cdist >= planet->Size()+prm.atm_href) return false;

	double alpha = acos (dotp (g_camera->GPos().unit(), (g_camera->GPos()-planet->GPos()).unit()));
	// angular distance between sun and planet as seen from camera

	double sunelev = alpha - PI05; // elevation of sun above horizon (assuming camera on ground)
	if (sunelev < -14.0*RAD) return false;  // total darkness

	double rscale = (planet->Size()-cdist)/prm.atm_href + 1.0;    // effect altitude scale (1 on ground, 0 at reference alt)
	double amb0 = min (0.7, log (planet->atm.rho0+1.0)*0.35);     // effect magnitude scale (planet-atmosphere dependent)
	double amb = amb0 * min (1.0, (sunelev+14.0*RAD)/(20.0*RAD)); // effect magnitude (dependent on sun elevation)
	if (amb < 0.05) return false;
	amb = max (0, amb-0.05);

	DWORD addamb = (DWORD)(amb*rscale*256);
	DWORD baseamb = g_pOrbiter->Cfg()->AmbientColour;
	ambient = 0;
	for (int i = 0; i < 4; i++)
		ambient |= min (255, ((baseamb >> (i*8)) & 0xff) + addamb) << (i*8);
	return true;
}