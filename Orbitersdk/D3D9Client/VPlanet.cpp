// ==============================================================
// VPlanet.cpp
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Released under GNU General Public License
// Copyright (C) 2006-2010 Martin Schweiger
//				 2010-2012 Jarmo Nikkanen (D3D9Client related parts)
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
#include "CloudMgr.h"
#include "HazeMgr.h"
#include "RingMgr.h"
#include "FileParser.h"
#include "DebugControls.h"

using namespace oapi;

// ==============================================================

static double farplane = 1e6;
extern int SURF_MAX_PATCHLEVEL;

// ==============================================================

vPlanet::vPlanet(OBJHANDLE _hObj, const Scene *scene): vObject (_hObj, scene)
{
	_TRACE;

	char name[64];
	char label[128];
	oapiGetObjectName(_hObj, name, 64);
	sprintf_s(label,"Building %s and Bases...",name);

	gc->SetLabel(label);

	rad			  = (float)oapiGetSize(_hObj);
	render_rad	  = (float)(0.1*rad);
	dist_scale	  = 1.0f;
	patchres	  = 0;
	surfmgr		  = new SurfaceManager(gc, this);
	hazemgr		  = NULL;
	ringmgr		  = NULL;
	clouddata	  = NULL;
	mesh		  = NULL;
	hashaze		  = *(bool*)gc->GetConfigParam(CFGPRM_ATMHAZE) && oapiPlanetHasAtmosphere(_hObj);
	bRipple       = *(bool*)gc->GetConfigParam(CFGPRM_SURFACERIPPLE) && *(bool*)oapiGetObjectParam(_hObj, OBJPRM_PLANET_SURFACERIPPLE);
	shadowalpha   =  (float)(*(double*)oapiGetObjectParam (_hObj, OBJPRM_PLANET_SHADOWCOLOUR));
	bVesselShadow = *(bool*)gc->GetConfigParam (CFGPRM_VESSELSHADOWS) && shadowalpha >= 0.01;
	bObjectShadow = *(bool*)gc->GetConfigParam (CFGPRM_OBJECTSHADOWS);
	
	if (bRipple) surfmgr->SetMicrotexture("waves.dds");

	double apprad = scene->GetObjectAppRad(_hObj);
	if (apprad>2.0) surfmgr->LoadData();
	
	if (*(bool*)gc->GetConfigParam(CFGPRM_CLOUDS) && *(bool*)oapiGetObjectParam (_hObj, OBJPRM_PLANET_HASCLOUDS)) {

		char name[32]; oapiGetObjectName(_hObj,name,32);
		LogBlu("%s has cloud layer",name);

		clouddata = new CloudData;
		clouddata->cloudmgr = new CloudManager (gc, this);
		clouddata->cloudrad = rad + *(double*)oapiGetObjectParam (_hObj, OBJPRM_PLANET_CLOUDALT);
		clouddata->cloudshadow = *(bool*)gc->GetConfigParam (CFGPRM_CLOUDSHADOWS);
		if (clouddata->cloudshadow) {
			clouddata->shadowalpha = 1.0f - *(float*)oapiGetObjectParam (_hObj, OBJPRM_PLANET_CLOUDSHADOWCOL);
			if (clouddata->shadowalpha < 0.01f) clouddata->cloudshadow = false;
		}
		if (*(bool*)oapiGetObjectParam (_hObj, OBJPRM_PLANET_CLOUDMICROTEX)) {
			clouddata->cloudmgr->SetMicrotexture("cloud1.dds");
			clouddata->microalt0 = *(double*)oapiGetObjectParam (_hObj, OBJPRM_PLANET_CLOUDMICROALTMIN);
			clouddata->microalt1 = *(double*)oapiGetObjectParam (_hObj, OBJPRM_PLANET_CLOUDMICROALTMAX);
		}
		if (apprad>2.0) clouddata->cloudmgr->LoadData();
	}

	if (*(bool*)oapiGetObjectParam (_hObj, OBJPRM_PLANET_HASRINGS)) {
		double minrad = *(double*)oapiGetObjectParam (_hObj, OBJPRM_PLANET_RINGMINRAD);
		double maxrad = *(double*)oapiGetObjectParam (_hObj, OBJPRM_PLANET_RINGMAXRAD);
		ringmgr = new RingManager (this, minrad, maxrad);
		render_rad = (float)(rad*maxrad);
	}
	
	memcpy2(&fog, oapiGetObjectParam(_hObj, OBJPRM_PLANET_FOGPARAM), sizeof(FogParam));

	if (*(bool*)gc->GetConfigParam(CFGPRM_ATMFOG) && fog.dens_0 > 0) bFog = true;
	else bFog = false;

	if (bFog) LogAlw("FogParams: alt_ref=%g, dens_0=%g, dens_ref=%g",fog.alt_ref, fog.dens_0, fog.dens_ref);

	nbase = oapiGetBaseCount(_hObj);

	if (nbase)	vbase = new vBase*[nbase];
	else		vbase = NULL;

	for (DWORD i=0;i<nbase;i++) vbase[i] = NULL;

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
	else {
		LogErr("oapiGetFocusInterface() returns NULL");
	}
	
	if (surfmgr->GetMaxLevel()==0) {
		char cbuf[256];
		oapiGetObjectName (hObj, cbuf, 256);
		OBJHANDLE hMesh = oapiLoadMesh(cbuf);
		if (hMesh) {
			LogBlu("Assigning a non-spherical mesh for %s",cbuf);
			mesh = new D3D9Mesh (gc, hMesh);
			oapiDeleteMesh (hMesh);
		}
	}

	albedo = gc->GetFileParser()->GetAlbedo(hObj);

	LogMsg("vPlanet constructor exiting");

	gc->SetLabel("Loading Textures...");
}

// ==============================================================

vPlanet::~vPlanet ()
{
	if (nbase) {
		for (DWORD i = 0; i < nbase; i++) if (vbase[i]) delete vbase[i];
		delete []vbase;
	}
	
	if (clouddata) {
		delete clouddata->cloudmgr;
		delete clouddata;
	}

	SAFE_DELETE(hazemgr);
	SAFE_DELETE(ringmgr);
	SAFE_DELETE(surfmgr);
	SAFE_DELETE(mesh);
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

	vObject::Update();

	if (patchres==0) return true;

	int i, j;
	float rad_scale = rad;
	bool rescale = false;
	dist_scale = 1.0f;

	sunLight = *scn->GetLight(-1);	

	if (cdist+render_rad > farplane && cdist-rad > 1e4) {
		rescale = true;
		dist_scale = (FLOAT)(farplane/(cdist+render_rad));
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
	
	if (clouddata) {
		clouddata->rendermode = (cdist < clouddata->cloudrad ? 1:0);
		if (cdist > clouddata->cloudrad*(1.0-1.5e-4)) clouddata->rendermode |= 2;
		if (clouddata->rendermode & 1) {
			clouddata->viewap = acos (rad/cloudrad);
			if (rad < cdist) clouddata->viewap += acos (rad/cdist);
		} else {
			clouddata->viewap = 0;
		}

		float cloudscale = (float)(clouddata->cloudrad/rad);
		double cloudrot = *(double*)oapiGetObjectParam (hObj, OBJPRM_PLANET_CLOUDROTATION);

		// world matrix for cloud shadows on the surface
		memcpy2 (&clouddata->mWorldC0, &mWorld, sizeof (D3DXMATRIX));

		if (cloudrot) {
			static D3DXMATRIX crot (1,0,0,0, 0,1,0,0, 0,0,1,0, 0,0,0,1);
			crot._11 =   crot._33 = (float)cos(cloudrot);
			crot._13 = -(crot._31 = (float)sin(cloudrot));
			D3DXMatrixMultiply(&clouddata->mWorldC0, &crot, &clouddata->mWorldC0);
		}

		// world matrix for cloud layer
		memcpy2 (&clouddata->mWorldC, &clouddata->mWorldC0, sizeof (D3DXMATRIX));

		for (i = 0; i < 3; i++)	for (j = 0; j < 3; j++)	clouddata->mWorldC.m[i][j] *= cloudscale;
		
		// set microtexture intensity
		double alt = cdist-rad;
		double lvl = (clouddata->microalt1-alt)/(clouddata->microalt1-clouddata->microalt0);
		clouddata->cloudmgr->SetMicrolevel (max (0, min (1, lvl)));
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
				if (apprad < 1.0 && patchres<2) { // out of visual range	
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
	} 
	else {
		ntx = PI*2.0 * apr;

		static const double scal2 = 1.0/log(2.0);
		new_patchres = min (max ((int)(scal2*log(ntx)-5.0),1), SURF_MAX_PATCHLEVEL);
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
			ringmgr->SetMeshRes(ringres);
		}
		patchres = new_patchres;
	}
}

// ==============================================================

void vPlanet::RenderZRange(double *nplane, double *fplane)
{
	double d = dotp (scn->GetCameraGDir(), cpos);
	*fplane = max (1e3, d+rad*1.2);
	*nplane = max (1e0, d-rad*1.2);
	*fplane = min (*fplane, *nplane*1e5);
}

// ==============================================================

bool vPlanet::Render(LPDIRECT3DDEVICE9 pDev)
{
	_TRACE;
	if (!active) return false;

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

	if (patchres == 0) { // render as 2x2 pixel block
		RenderDot(pDev);
	} 
	else {             // render as sphere
		bool bfog = bFog;
		float fogfactor = 0.0f;

		D3D9Effect::UpdateEffectCamera(hObj);

		cBackGround = scn->GetBgColour();
		pCurrentVisual = this;

		bool addambient = ((cBackGround & 0xFFFFFF) && (hObj != scn->GetCameraProxyBody()));

		DWORD dAmbient = *(DWORD*)gc->GetConfigParam(CFGPRM_AMBIENTLEVEL);
		float fAmbient = float(dAmbient)*0.0039f;

		D3D9Effect::InitLegacyAtmosphere(hObj, fAmbient);
		
		D3D9Effect::FX->SetFloat(D3D9Effect::eDistScale, 1.0f/dist_scale);

		// for planets seen through an atmospheric layer from the surface of
		// another planet, add the ambient atmosphere colour to the rendering
		if (!addambient) cBackGround = 0;
			
		if (ringmgr) ringmgr->Render(pDev, mWorld, false);
		
		pDev->SetRenderState(D3DRS_CULLMODE, D3DCULL_CCW);	
		
		if (clouddata && (clouddata->rendermode & 1)) RenderCloudLayer(pDev, D3DCULL_CW);           // render clouds from below

		if (hazemgr) hazemgr->Render(pDev, mWorld); // horizon ring
		
		if (bFog && hObj==scn->GetCameraProxyBody()) { // set up distance fog

 			double R = oapiGetSize (hObj);
			double h = max(1.0, cdist-R);

			VECTOR3 fogcol = fog.col;
			double h_ref = fog.alt_ref;   // 4e3;
			double fog_0 = fog.dens_0;    // 5e-5;
			double fog_ref = fog.dens_ref; // 3e-5;
			double scl = h_ref*fog_ref;

			if (h < h_ref) fogfactor = (float)(h/h_ref * (fog_ref-fog_0) + fog_0); // linear zone
			else		   fogfactor = (float)(scl/h); // hyperbolic zone
		
			if (fogfactor < 0.0f) bfog = false;
			else {
		
				// day/nighttime fog lighting
				VECTOR3 ppos;
				oapiGetGlobalPos(hObj, &ppos);
				
				double cosa = dotp (unit(ppos), unit(cpos));
				double bright = 0.5 * max (0.0, min (1.0, cosa + 0.3));
				float rfog = (float)(bright*(min(1.0,fogcol.x)+0.5)); // "whiten" the fog colour
				float gfog = (float)(bright*(min(1.0,fogcol.y)+0.5));
				float bfog = (float)(bright*(min(1.0,fogcol.z)+0.5));

				D3D9Effect::FX->SetFloat(D3D9Effect::eFogDensity, fogfactor);
				D3D9Effect::FX->SetVector(D3D9Effect::eFogColor, &D3DXVECTOR4(rfog,gfog,bfog,1.0f));
				D3D9Effect::FX->SetInt(D3D9Effect::eHazeMode, 2);
			}
		}

		if (bfog==false) D3D9Effect::FX->SetInt(D3D9Effect::eHazeMode, 0);

		if (mesh) {
			mesh->SetSunLight(&sunLight);
			mesh->SetAmbientColor(cBackGround);
			mesh->RenderAsteroid(pDev, &mWorld);
		}
		else RenderSphere(pDev, bfog); // planet surface

		if (nbase) RenderBaseStructures(pDev);

		if (bfog==true) D3D9Effect::FX->SetInt(D3D9Effect::eHazeMode, 0); // turn off fog
		
		if (clouddata && (clouddata->rendermode & 2)) RenderCloudLayer(pDev, D3DCULL_CCW);		  // render clouds from above
		
		if (hazemgr) hazemgr->Render(pDev, mWorld, true); // haze across planet disc
		if (ringmgr) ringmgr->Render(pDev, mWorld, true);
		
		pDev->SetRenderState(D3DRS_CULLMODE, D3DCULL_CCW);	
	}

	return true;
}

// ==============================================================

void vPlanet::RenderBeacons(LPDIRECT3DDEVICE9 dev)
{
	// Beacons rendered elsewhere before the cloud layer	
}


// ==============================================================

void vPlanet::RenderSphere (LPDIRECT3DDEVICE9 dev, bool bfog)
{
	float fogfactor;
	D3D9Effect::FX->GetFloat(D3D9Effect::eFogDensity, &fogfactor);

	if (bfog) D3D9Effect::FX->SetFloat(D3D9Effect::eFogDensity, fogfactor/dist_scale);

	surfmgr->SetAmbientColor(cBackGround);
	surfmgr->Render(dev, mWorld, dist_scale, patchres, 0.0, bfog); // surface

	if (bfog) D3D9Effect::FX->SetFloat(D3D9Effect::eFogDensity, fogfactor);

	if (nbase) {
		RenderBaseSurfaces(dev);                     // base surfaces
		RenderBaseShadows(dev, shadowalpha);         // base shadows
	}

	if (bfog) D3D9Effect::FX->SetFloat(D3D9Effect::eFogDensity, fogfactor/dist_scale);
	if (clouddata && clouddata->cloudshadow) RenderCloudShadows(dev);         // cloud shadows
	if (bfog) D3D9Effect::FX->SetFloat(D3D9Effect::eFogDensity, fogfactor);

	// cast shadows only on planet closest to camera
	if (bVesselShadow && hObj == oapiCameraProxyGbody()) scn->RenderVesselShadows(hObj, shadowalpha); // vessel shadows

	dev->SetRenderState(D3DRS_STENCILENABLE, FALSE);
}

// ==============================================================

void vPlanet::RenderCloudLayer(LPDIRECT3DDEVICE9 dev, DWORD cullmode)
{
	if (cullmode != D3DCULL_CCW) dev->SetRenderState(D3DRS_CULLMODE, cullmode);
	clouddata->cloudmgr->Render (dev, clouddata->mWorldC, dist_scale, min(patchres,8), clouddata->viewap); // clouds
	if (cullmode != D3DCULL_CCW) dev->SetRenderState(D3DRS_CULLMODE, D3DCULL_CCW);
}

// ==============================================================

void vPlanet::RenderCloudShadows(LPDIRECT3DDEVICE9 dev)
{
	dev->SetRenderState(D3DRS_CULLMODE, D3DCULL_CCW);
	clouddata->cloudmgr->RenderShadow(dev, clouddata->mWorldC0, dist_scale, min(patchres,8), clouddata->viewap, clouddata->shadowalpha);	
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
