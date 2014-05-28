// ==============================================================
// VPlanet.cpp
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2006-2014 Martin Schweiger
// Copyright (C) 2010-2014 Jarmo Nikkanen
// ==============================================================

// ==============================================================
// class vPlanet (implementation)
//
// A vPlanet is the visual representation of a "planetary" object
// (planet, moon, asteroid).
// Currently this only supports spherical objects, without
// variations in elevation.
// ==============================================================

#define D3D_OVERLOADS

#include "D3D9Client.h"
#include "D3D9Config.h"
#include "VPlanet.h"
#include "VBase.h"
#include "SurfMgr.h"
#include "surfmgr2.h"
#include "cloudmgr2.h"
#include "CloudMgr.h"
#include "HazeMgr.h"
#include "RingMgr.h"
#include "FileParser.h"
#include "DebugControls.h"
#include "AtmoControls.h"

using namespace oapi;

// ==============================================================

static double farplane = 1e6;
static double max_surf_dist = 1e4;

extern int SURF_MAX_PATCHLEVEL;
extern int SURF_MAX_PATCHLEVEL2;

// ==============================================================

vPlanet::vPlanet (OBJHANDLE _hObj, const Scene *scene): vObject (_hObj, scene)
{
	memset(&SPrm, 0, sizeof(ScatterParams));

	rad = (float)size;
	render_rad = (float)(0.1*rad);
	dist_scale = 1.0f;
	max_centre_dist = 0.9*scene->GetCameraFarPlane();
	maxdist = max (max_centre_dist, max_surf_dist + rad);
	int tilever = *(int*)oapiGetObjectParam (_hObj, OBJPRM_PLANET_TILEENGINE);
	if (tilever < 2) {
		surfmgr = new SurfaceManager (gc, this);
		surfmgr2 = NULL;
	} else {
		surfmgr = NULL;
		int maxlvl = *(DWORD*)oapiGetObjectParam (_hObj, OBJPRM_PLANET_SURFACEMAXLEVEL);
		maxlvl = min (maxlvl, SURF_MAX_PATCHLEVEL2);
		surfmgr2 = new TileManager2<SurfTile> (this, maxlvl);
	}
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
			cloudmgr2 = new TileManager2<CloudTile> (this, maxlvl);
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
	
	memcpy2 (&fog, oapiGetObjectParam (_hObj, OBJPRM_PLANET_FOGPARAM), sizeof (FogParam));
	prm.bFogEnabled = (fog.dens_0 > 0);

	patchres = 0;

	//if (*(bool*)gc->GetConfigParam(CFGPRM_ATMFOG)==false) prm.bFogEnabled = false;

	
	nbase = oapiGetBaseCount (_hObj);
	if (nbase)	vbase = new vBase*[nbase];
	else		vbase = NULL;
	for (DWORD i = 0; i < nbase; i++)
		vbase[i] = NULL;

	VESSEL *hVes = oapiGetFocusInterface();

	if (hVes) {
		if (Config->PreLBaseVis && _hObj==hVes->GetSurfaceRef()) {
			LogAlw("PreLoading Base Visuals");
			for (DWORD i=0;i<nbase;i++) {
				OBJHANDLE hBase = oapiGetBaseByIndex (_hObj, i);
				vbase[i] = new vBase (hBase, scn);
			}
		}
	} 
	
	mesh = NULL;
	if (surfmgr && surfmgr->GetMaxLevel() == 0) {
		char cbuf[256];
		oapiGetObjectName (hObj, cbuf, 256);
		OBJHANDLE hMesh = oapiLoadMesh (cbuf);
		if (hMesh) {
			mesh = new D3D9Mesh (gc, hMesh);
			oapiDeleteMesh (hMesh);
		}
	}

	LoadAtmoConfig();

	albedo = gc->GetFileParser()->GetAlbedo(hObj);
	LogMsg("vPlanet constructor exiting");
	gc->SetLabel("Loading Textures...");
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

DWORD vPlanet::GetBaseCount()
{
	return nbase;
}

// ==============================================================

vBase* vPlanet::GetBaseByIndex(DWORD index)
{
	return vbase[index];
}

// ==============================================================

vBase* vPlanet::GetBaseByHandle(OBJHANDLE hBase)
{
	if (vbase) for (DWORD i=0;i<nbase;i++) if (vbase[i]) if (vbase[i]->Object()==hBase) return vbase[i];
	return NULL;
}

// ===========================================================================================
//
bool vPlanet::GetMinMaxDistance(float *zmin, float *zmax, float *dmin)
{
	if (mesh==NULL) return false;
	if (bBSRecompute) UpdateBoundingBox();

	D3DXVECTOR3 pos = D3DXVECTOR3(mWorld._41, mWorld._42, mWorld._43);

	float dst = D3DXVec3Length(&pos);

	*dmin = dst - float(oapiGetSize(hObj));
	*zmin = *dmin;
	*zmax = dst + float(oapiGetSize(hObj));

	return true;
}


// ===========================================================================================
//
void vPlanet::UpdateBoundingBox()
{
	bBSRecompute = false;
}

// ==============================================================

bool vPlanet::Update ()
{
	_TRACE;
	if (!active) return false;
	
	//sunLight = *scn->GetLight(-1);	

	vObject::Update();

	if (patchres==0) return true;

	float rad_scale = rad;
	bool rescale = false;
	dist_scale = 1.0f;

	if (cdist > maxdist) {
		rescale = true;
		dist_scale = (FLOAT)(max_centre_dist/cdist);
		prm.DistScale = dist_scale;
	}
	if (rescale) {
		rad_scale *= dist_scale;
		mWorld._41 *= dist_scale;
		mWorld._42 *= dist_scale;
		mWorld._43 *= dist_scale;
	}

	// scale up from template sphere radius 1
	mWorld._11 *= rad_scale; mWorld._12 *= rad_scale; mWorld._13 *= rad_scale;
	mWorld._21 *= rad_scale; mWorld._22 *= rad_scale; mWorld._23 *= rad_scale;
	mWorld._31 *= rad_scale; mWorld._32 *= rad_scale; mWorld._33 *= rad_scale;

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
			memcpy2 (&clouddata->mWorldC0, &mWorld, sizeof (D3DMATRIX));
			if (prm.cloudrot) {
				static D3DXMATRIX crot (1,0,0,0, 0,1,0,0, 0,0,1,0, 0,0,0,1);
				crot._11 =   crot._33 = (float)cos(prm.cloudrot);
				crot._13 = -(crot._31 = (float)sin(prm.cloudrot));
				D3DXMatrixMultiply (&clouddata->mWorldC0, &crot, &clouddata->mWorldC0);
			}

			// world matrix for cloud layer
			memcpy2 (&clouddata->mWorldC, &clouddata->mWorldC0, sizeof (D3DMATRIX));
			for (int i = 0; i < 3; i++)
				for (int j = 0; j < 3; j++) {
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
		VECTOR3 pos, cpos = scn->GetCameraGPos();
		double scale = (double)scn->ViewH()/tan(scn->GetCameraAperture());
		for (DWORD i = 0; i < nbase; i++) {
			OBJHANDLE hBase = oapiGetBaseByIndex (hObj, i);
			oapiGetGlobalPos (hBase, &pos);
			double rad = oapiGetSize (hBase);
			double dst = dist (pos, cpos);
			double apprad = rad*scale/dst;

			if (vbase[i]) { // base visual exists
				if (apprad < 1.0 && patchres<3) { // out of visual range
					delete vbase[i];
					vbase[i] = 0;
				}
			} else {        // base visual doesn't exist
				if (apprad > 2.0) { // within visual range
					vbase[i] = new vBase (hBase, scn);
				}
			}

			if (vbase[i]) {
				if (apprad < 1.0) {
					vbase[i]->Activate(false);
				}
				else if (apprad > 2.0) {
					vbase[i]->Activate(true);
				}
				vbase[i]->Update();	
			}
		}
	}
	return true;
}

// ==============================================================

void vPlanet::CheckResolution()
{
	double alt = max(1.0, cdist-rad);
	double apr = rad * scn->ViewH()*0.5 / (alt * tan(scn->GetCameraAperture()));
	// apparent planet radius in units of screen pixels

	int new_patchres;
	double ntx;

	if (apr < 2.5) { // render planet as 2x2 pixels
		new_patchres = 0;
		ntx = 0;
	} else {
		ntx = PI*2.0 * apr;

		static const double scal2 = 1.0/log(2.0);
		const double shift = (surfmgr2 ? 6.0 : 5.0); // reduce level for tile mgr v2, because of increased patch size
		new_patchres = min (max ((int)(scal2*log(ntx)-5.0),1), SURF_MAX_PATCHLEVEL2);
	}
	if (new_patchres != patchres) {
		if (hashaze) {
			if (new_patchres < 3) {
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
	double d = dotp (scn->GetCameraGDir(), cpos);
	*fplane = max (1e3, d+rad*1.2);
	*nplane = max (1e0, d-rad*1.2);
	*fplane = min (*fplane, *nplane*1e5);
}

// ==============================================================

bool vPlanet::Render(LPDIRECT3DDEVICE9 dev)
{
	_TRACE;
	if (!active) return false;

	D3D9Effect::UpdateEffectCamera(hObj);
	D3D9Effect::FX->SetFloat(D3D9Effect::eDistScale, 1.0f/float(dist_scale));
	PlanetRenderer::InitializeScattering(this);

	if (DebugControls::IsActive()) {
		// DWORD flags  = *(DWORD*)gc->GetConfigParam(CFGPRM_GETDEBUGFLAGS);
		DWORD displ  = *(DWORD*)gc->GetConfigParam(CFGPRM_GETDISPLAYMODE);
		vObject *vSel =  DebugControls::GetVisual();
		if (vSel && displ>0) {
			if (vSel->GetObjectA()) {
				if (oapiGetObjectType(vSel->GetObjectA())==OBJTP_VESSEL) return false;
			}
		}
	}

	pCurrentVisual = this;

	if (patchres == 0) { // render as 2x2 pixel block
		RenderDot (dev);
	} 
	else {             // render as sphere
		DWORD amb = prm.amb0col;
		bool ringpostrender = false;
		float fogfactor;

		D3DCOLOR bg		= scn->GetBgColour();
		prm.bFog		= prm.bFogEnabled;
		prm.bTint		= prm.bFogEnabled;
		prm.bAddBkg		= ((bg & 0xFFFFFF) && (hObj != scn->GetCameraProxyBody()));
		prm.FogDensity	= 0.0f;
		prm.SkyColor	= D3DXCOLOR(bg);
		prm.AmbColor	= D3DXCOLOR(0,0,0,0);
		prm.FogColor	= D3DXCOLOR(0,0,0,0);
		prm.TintColor	= D3DXCOLOR(0,0,0,0);
		prm.SunDir		= -scn->GetLight(-1)->Direction;

		if (ringmgr) {
			if (cdist < rad*ringmgr->InnerRad()) { // camera inside inner ring edge
				ringmgr->Render(dev, mWorld, false);
			} else {
				// if the planet has a ring system we update the z-buffer
				// but don't do z-checking for the planet surface
				// This strategy could do with some reconsideration
				ringpostrender = true;
			}
		}

		if (prm.bCloud && (prm.cloudvis & 1))
			RenderCloudLayer (dev, D3DCULL_CW);      // render clouds from below

		//if (hazemgr) hazemgr->Render (dev, mWorld, true);       // horizon ring
		
		VECTOR3 cdir;
		oapiCameraGlobalDir(&cdir);

		double calt = length(cpos) - size;

		if (calt>60e3)	PlanetRenderer::RenderRing(mWorld, float(100e3/size));
		else			PlanetRenderer::RenderSky(cpos, cdir, size, 60.0);

		if (prm.bAtm) {
			if (ModLighting (amb))
				prm.AmbColor = D3DXCOLOR(amb);
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
				prm.FogDensity = fogfactor;
				prm.FogColor = D3DXCOLOR(rfog, gfog, bfog, 1.0f);
			}
		}

		if (prm.bTint) {
			prm.TintColor = _D3DXCOLOR(*(VECTOR3*)oapiGetObjectParam (hObj, OBJPRM_PLANET_ATMTINTCOLOUR));
			double R = oapiGetSize (hObj);
			double alt = cdist - R;
			double alt_ref1 = fog.alt_ref*5.0;
			double alt_ref2 = alt_ref1 * 0.1;
			if (alt < alt_ref1) {
				double scale = (alt-alt_ref2)/(alt_ref1-alt_ref2);
				if (scale <= 0.0) prm.bTint = false;
				else prm.TintColor *= scale;
			}
		}

		if (mesh) {
			//mesh->SetSunLight(&sunLight);
			//mesh->SetAmbientColor(cBackGround);
			//mesh->RenderAsteroid(pDev, &mWorld);
			//LogErr("Rendering Mesh");
		} else {
			RenderSphere (dev);                               
		}

		if (nbase) RenderBaseStructures (dev);

		if (prm.bCloud && (prm.cloudvis & 2))
			RenderCloudLayer (dev, D3DCULL_CCW);	  // render clouds from above

		//if (hazemgr) 
			//hazemgr->Render (dev, mWorld, true); // haze across planet disc

		if (ringpostrender) 
			ringmgr->Render (dev, mWorld, true);
	}
	return true;
}

// ==============================================================

void vPlanet::RenderBeacons(LPDIRECT3DDEVICE9 dev)
{
	// Beacons rendered elsewhere before the cloud layer	
}


// ==============================================================

void vPlanet::RenderSphere (LPDIRECT3DDEVICE9 dev)
{

	if (surfmgr2) {
		if (cdist >= 1.3*rad) surfmgr2->Render (dmWorld, false, prm);
		else				  surfmgr2->Render (dmWorld, true,  prm);
	} else {
		surfmgr->Render (dev, mWorld, dist_scale, patchres, 0.0, prm.bFog); // surface
	}

	if (nbase) {
		RenderBaseSurfaces (dev);                     // base surfaces
		RenderBaseShadows (dev, shadowalpha);         // base shadows
	}

	if (prm.bCloudShadow)
		RenderCloudShadows (dev);                // cloud shadows

	if (bVesselShadow && hObj == oapiCameraProxyGbody())
		scn->RenderVesselShadows (hObj, shadowalpha); // vessel shadows
}

// ==============================================================

void vPlanet::RenderCloudLayer (LPDIRECT3DDEVICE9 dev, DWORD cullmode)
{
	if (cullmode != D3DCULL_CCW) dev->SetRenderState (D3DRS_CULLMODE, cullmode);
	if (cloudmgr2)
		cloudmgr2->Render (dmWorld, false, prm);
	else
		clouddata->cloudmgr->Render (dev, clouddata->mWorldC, dist_scale, min(patchres,8), clouddata->viewap); // clouds
	if (cullmode != D3DCULL_CCW) dev->SetRenderState (D3DRS_CULLMODE, D3DCULL_CCW);
}

// ==============================================================

void vPlanet::RenderCloudShadows (LPDIRECT3DDEVICE9 dev)
{
	return;

	if (cloudmgr2) {
		if (prm.bCloudFlatShadows)
			cloudmgr2->RenderFlatCloudShadows (dmWorld, prm);
	} 
	else if (clouddata) { // legacy method
		/*
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
		*/
	}
}

// ==============================================================

void vPlanet::RenderBaseSurfaces(LPDIRECT3DDEVICE9 dev)
{
	for (DWORD i=0;i<nbase;i++) if (vbase[i]) vbase[i]->RenderSurface(dev);		
}

// ==============================================================

void vPlanet::RenderBaseShadows(LPDIRECT3DDEVICE9 dev, float depth)
{
	if (bObjectShadow) {
		for (DWORD i=0;i<nbase;i++) if (vbase[i]) vbase[i]->RenderGroundShadow(dev, depth);
		// reset device parameters
		dev->SetRenderState(D3DRS_STENCILENABLE, FALSE);
	}
}

// ==============================================================

void vPlanet::RenderBaseStructures (LPDIRECT3DDEVICE9 dev)
{
	for (DWORD i=0;i<nbase;i++) if (vbase[i]) vbase[i]->RenderStructures(dev);
	for (DWORD i=0;i<nbase;i++) if (vbase[i]) vbase[i]->RenderBeacons(dev);
}

// ==============================================================

bool vPlanet::ModLighting (DWORD &ambient)
{
	// modify ambient light level inside atmospheres as a function of sun elevation
	if (!prm.bAtm) return false;
	if (cdist >= size+prm.atm_href) return false;

	double alpha = acos (dotp (unit(scn->GetCameraGPos()), -unit(cpos)));
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

// ==============================================================

float vPlanet::OpticalDepth(float alt, float cd)
{
	float cd2 = cd * cd;
	D3DXVECTOR4 q(1.0f, cd, cd2, cd2*cd);
	return exp(-alt*prm.InvSclHeight) * pow(D3DXVec4Dot(&q, &prm.ODCoEff), -float(SctPwr));
}

// ==============================================================

void vPlanet::UpdateAtmoConfig()
{
	double outer     = size + SPrm.height * 10e3;
	prm.SclHeight	 = float(SPrm.height)*1e3;
	prm.InvSclHeight = 1.0f / float(prm.SclHeight);
	prm.ODCoEff		 = SolveScatter(prm.SclHeight, size, outer);
}


double GaussLobatto(double alt, double dir, double R0, double R1, double h0)
{
	double R = R0 + alt;

	double rdt = -R * cos(dir);
	double Ray = rdt + sqrt(R1*R1 - (R*R - rdt*rdt));	

	double p0 = Ray * 0.0; 
	double p1 = Ray * 0.2765; 
	double p2 = Ray * 0.7235; 
	double p3 = Ray * 1.0; 

	double a0 = sqrt(R*R + p0*p0 + 2.0*R*p0*cos(dir)) - R0;
	double a1 = sqrt(R*R + p1*p1 + 2.0*R*p1*cos(dir)) - R0;
	double a2 = sqrt(R*R + p2*p2 + 2.0*R*p2*cos(dir)) - R0;
	double a3 = sqrt(R*R + p3*p3 + 2.0*R*p3*cos(dir)) - R0;

	double s0 = exp(-a0/h0);
	double s1 = exp(-a1/h0);
	double s2 = exp(-a2/h0);
	double s3 = exp(-a3/h0);

	double sum = (s0*0.167 + s1*0.833 + s2*0.833 + s3*0.167) * Ray * 0.5;

	return sum;
}



   

// ==============================================================

void vPlanet::DumpDebugFile()
{
	
	int samples = 90;
	double max_angle = 90.0;
	double delta = 3.1415*max_angle/float(samples*180);
	double angle = 0.0;

	double outer = size+(prm.SclHeight*10.0);

	FILE *fp = NULL;
	fopen_s(&fp, "OpticalDebug.txt", "w");

	double par = ExactOpticalDepth(0.0, 3.1416/2.0, size, outer, prm.SclHeight) / prm.SclHeight;

	if (fp==NULL) return;

	for (int i=0;i<samples;i++) {
		double accurate = OpticalDepth(0.0, float(cos(angle)));
		double exact	= ExactOpticalDepth(0.0, angle, size, outer, prm.SclHeight) / prm.SclHeight;
		double gauss	= GaussLobatto(0.0, angle, size, outer, prm.SclHeight) / prm.SclHeight;
		double test		= par*exp(0.0)/(1.0+(par-1.0)*cos(angle));

		angle += delta;

		fprintf(fp,"%d %6.6g %6.6g %6.6g %6.6g\n", i, exact, accurate, gauss, test);
	}
	fclose(fp);
	
}

// ==============================================================

void vPlanet::LoadAtmoConfig()
{
	char name[32];
	char path[256];

	oapiGetObjectName(hObj, name, 32);

	sprintf_s(path,"GC/%s.atm.cfg",name);

	FILEHANDLE hFile = oapiOpenFile(path, FILE_IN, CONFIG);

	if (!hFile) return;

	oapiReadItem_float(hFile, "Red", SPrm.red);
	oapiReadItem_float(hFile, "Green", SPrm.green);
	oapiReadItem_float(hFile, "Blue", SPrm.blue);
	oapiReadItem_float(hFile, "RWaveDep", SPrm.wavepow);
	oapiReadItem_float(hFile, "ScaleHeight", SPrm.height);
	oapiReadItem_float(hFile, "OutScatter", SPrm.rout);
	oapiReadItem_float(hFile, "InScatter", SPrm.rin);
	oapiReadItem_float(hFile, "RayleighPhase", SPrm.rphase);
	oapiReadItem_float(hFile, "MiePhase", SPrm.balance);
	oapiReadItem_float(hFile, "SunIntensity", SPrm.rsun);
	oapiReadItem_float(hFile, "SrfColor", SPrm.srfclr);
	oapiReadItem_float(hFile, "SrfIntensity", SPrm.sun);
	oapiReadItem_float(hFile, "MiePower", SPrm.mie);
	oapiReadItem_float(hFile, "MiePhase", SPrm.mphase);
	oapiReadItem_int(hFile,   "Mode", SPrm.mode);
	oapiReadItem_bool(hFile,  "OverSaturation", SPrm.oversat);

	oapiCloseFile(hFile, FILE_IN);

	UpdateAtmoConfig();
	
}

// ==============================================================

void vPlanet::SaveAtmoConfig()
{
	char name[64];
	char path[256];

	oapiGetObjectName(hObj, name, 64);

	sprintf_s(path,"GC/%s.atm.cfg",name);

	FILEHANDLE hFile = oapiOpenFile(path, FILE_OUT, CONFIG);

	if (!hFile) return;

	oapiWriteItem_float(hFile, "Red", SPrm.red);
	oapiWriteItem_float(hFile, "Green", SPrm.green);
	oapiWriteItem_float(hFile, "Blue", SPrm.blue);
	oapiWriteItem_float(hFile, "RWaveDep", SPrm.wavepow);
	oapiWriteItem_float(hFile, "ScaleHeight", SPrm.height);
	oapiWriteItem_float(hFile, "OutScatter", SPrm.rout);
	oapiWriteItem_float(hFile, "InScatter", SPrm.rin);
	oapiWriteItem_float(hFile, "RayleighPhase", SPrm.rphase);
	oapiWriteItem_float(hFile, "MiePhase", SPrm.balance);
	oapiWriteItem_float(hFile, "SunIntensity", SPrm.rsun);
	oapiWriteItem_float(hFile, "SrfColor", SPrm.srfclr);
	oapiWriteItem_float(hFile, "SrfIntensity", SPrm.sun);
	oapiWriteItem_float(hFile, "MiePower", SPrm.mie);
	oapiWriteItem_float(hFile, "MiePhase", SPrm.mphase);
	oapiWriteItem_int(hFile,   "Mode", SPrm.mode);
	oapiWriteItem_bool(hFile,  "OverSaturation", SPrm.oversat);

	oapiCloseFile(hFile, FILE_OUT);

	DumpDebugFile();
}
