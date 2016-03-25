// ==============================================================
// Scene.cpp
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2007 - 2016 Martin Schweiger
//				 2012 - 2016 Jarmo Nikkanen
// ==============================================================

#include "Scene.h"
#include "VPlanet.h"
#include "VVessel.h"
#include "VBase.h"
#include "Particle.h"
#include "CSphereMgr.h"
#include "D3D9Frame.h"
#include "D3D9Util.h"
#include "D3D9Config.h"
#include "D3D9Surface.h"
#include "D3D9TextMgr.h"
#include "D3D9Catalog.h"
#include "AABBUtil.h"
#include "OapiExtension.h"
#include "DebugControls.h"
#include "IProcess.h"



using namespace oapi;

static D3DXMATRIX ident;

const double LABEL_DISTLIMIT = 0.6;

struct PList { // auxiliary structure for object distance sorting
	vObject *vo;
	double dist;
};

const int MAXPLANET = 512; // hard limit; should be fixed
static PList plist[MAXPLANET];

ID3DXEffect * Scene::FX = 0;
D3DXHANDLE Scene::eLine = 0;
D3DXHANDLE Scene::eStar = 0;
D3DXHANDLE Scene::eWVP = 0;
D3DXHANDLE Scene::eColor = 0;
D3DXHANDLE Scene::eTex0 = 0;


// ===========================================================================================
//
Scene::Scene(D3D9Client *_gc, DWORD w, DWORD h)
{
	_TRACE;

	gc = _gc;
	vobjEnv = NULL;
	csphere = NULL;
	Lights = NULL;
	hSun = NULL;
	pAxisFont  = NULL;
	pLabelFont = NULL;
	pDebugFont = NULL;
	viewH = h;
	viewW = w;
	nLights = 0;
	dwTurn = 0;
	dwFrameId = 0;
	// ---------------------------
	psgDepth = NULL;
	psgNormal = NULL;
	psgSpecular = NULL;
	ptgDepth = NULL;
	ptgNormal = NULL;
	ptgSpecular = NULL;
	// ------------------------
	
	pDevice = _gc->GetDevice();
	
	memset(&Camera, 0, sizeof(Camera));

	D3DXMatrixIdentity(&ident);
	
	SetCameraAperture(float(RAD*50.0), float(viewH)/float(viewW));
	SetCameraFrustumLimits(2.5f, 5e6f); // initial limits

	csphere = new CelestialSphere(gc);

	// Create and Clear light pointers
	//
	gc->clbkGetRenderParam(RP_MAXLIGHTS, &maxlight);

	bLocalLight = *(bool*)gc->GetConfigParam(CFGPRM_LOCALLIGHT);
	if (!bLocalLight) maxlight = 0;

	Lights = new D3D9Light[12];
	memset2(Lights, 0, 12*sizeof(D3D9Light));
	memset2(&sunLight, 0, sizeof(D3D9Light));
	
	vobjFirst = vobjLast = NULL;
	camFirst = camLast = camCurrent = NULL;
	nstream = 0;
	iVCheck = 0;

	InitGDIResources();

	cspheremgr = new CSphereManager(_gc, this);

	LogAlw("=========== Initializing G-Buffer ============");

	/*
	//HR(D3DXCreateTexture(pDevice, viewW, viewH, 1, D3DUSAGE_RENDERTARGET, D3DFMT_A2R10G10B10, D3DPOOL_DEFAULT, &ptgNormal));
	HR(D3DXCreateTexture(pDevice, viewW, viewH, 1, D3DUSAGE_RENDERTARGET, D3DFMT_X8R8G8B8,	  D3DPOOL_DEFAULT, &ptgNormal));
	HR(D3DXCreateTexture(pDevice, viewW, viewH, 1, D3DUSAGE_RENDERTARGET, D3DFMT_G16R16F,     D3DPOOL_DEFAULT, &ptgSpecular));
	HR(D3DXCreateTexture(pDevice, viewW, viewH, 1, D3DUSAGE_RENDERTARGET, D3DFMT_R32F,		  D3DPOOL_DEFAULT, &ptgDepth));

	if (ptgNormal)   ptgNormal->GetSurfaceLevel(0, &psgNormal);
	if (ptgSpecular) ptgSpecular->GetSurfaceLevel(0, &psgSpecular);
	if (ptgDepth)    ptgDepth->GetSurfaceLevel(0, &psgDepth);
	*/

	LogAlw("================ Scene Created ===============");

	/*
	SURFHANDLE hTgt = oapiCreateSurfaceEx(viewW, viewH, OAPISURFACE_RENDERTARGET);

	pIPI = new ImageProcessing(pDevice, "Modules/D3D9Client/IPI.hlsl", "PSMain");

	pIPI->SetOutput(0, hTgt);
	pIPI->SetFloat("myVar", 1.0f);
	pIPI->SetFloat("myVec", &D3DXVECTOR4(1.0f, 1.0f, 1.0f, 1.0f), sizeof(D3DXVECTOR4));

	if (!pIPI->Execute()) LogErr("pIPI::Execute Failed");

	oapiBlt(gc->GetBackBufferHandle(), hTgt, 0, 0, 0, 0, viewW, viewH);

	oapiDestroySurface(hTgt);

	pDevice->Present(0, 0, 0, 0);
	Sleep(2000);*/
	
}

// ===========================================================================================
//
Scene::~Scene ()
{
	_TRACE;

	pDevice->SetRenderTarget(0, NULL);
	pDevice->SetRenderTarget(1, NULL);
	pDevice->SetRenderTarget(2, NULL);
	pDevice->SetRenderTarget(3, NULL);

	SAFE_RELEASE(psgNormal);
	SAFE_RELEASE(psgSpecular);
	SAFE_RELEASE(psgDepth);

	SAFE_RELEASE(ptgNormal);
	SAFE_RELEASE(ptgSpecular);
	SAFE_RELEASE(ptgDepth);

	SAFE_DELETE(csphere);
	
	if (Lights) delete []Lights;
	if (cspheremgr) delete cspheremgr;

	// Particle Streams
	if (nstream) {
		for (DWORD j=0;j<nstream;j++) delete pstream[j];
		delete []pstream;
	}

	DeleteAllCustomCameras();
	DeleteAllVisuals();
	ExitGDIResources();
}

// ===========================================================================================
//
void Scene::Initialise()
{
	_TRACE;

	hSun = oapiGetGbodyByIndex(0); // generalise later

	DWORD ambient = *(DWORD*)gc->GetConfigParam(CFGPRM_AMBIENTLEVEL);

	// Setup sunlight -------------------------------
	//
	float pwr = 1.0f;

	sunLight.Diffuse.r = sunLight.Specular.r = pwr; 
	sunLight.Diffuse.g = sunLight.Specular.g = pwr;
	sunLight.Diffuse.b = sunLight.Specular.b = pwr;
	sunLight.Diffuse.a = sunLight.Specular.a = 1.0f;
	sunLight.Ambient.r = float(ambient)*0.0039f;
	sunLight.Ambient.g = float(ambient)*0.0039f;
	sunLight.Ambient.b = float(ambient)*0.0039f;
	sunLight.Ambient.a = 1.0f;
	sunLight.Attenuation[0] = 1.0f; 
    sunLight.Param[D3D9LRange] = FLT_MAX;

	// Update Sunlight direction -------------------------------------
	//
	VECTOR3 rpos, cpos;
	oapiGetGlobalPos(hSun, &rpos);
	oapiCameraGlobalPos(&cpos); rpos-=cpos;
	D3DVEC(-unit(rpos), sunLight.Direction);
	D3DVEC(rpos, sunLight.Position);

	// Do not "pre-create" visuals here. Will cause changed call order for vessel callbacks
}

// ===========================================================================================
//
double Scene::GetObjectAppRad(OBJHANDLE hObj) const
{
	VECTOR3 pos,cam;
	oapiGetGlobalPos (hObj, &pos);
	oapiCameraGlobalPos(&cam); // must use oapiCam.. here. called before camera setup
	double rad = oapiGetSize (hObj);
	double dst = dist (pos, cam);
	return (rad*double(viewH))/(dst*tan(oapiCameraAperture()));
}

// ===========================================================================================
//
double Scene::GetObjectAppRad2(OBJHANDLE hObj) const
{
	VECTOR3 pos;
	oapiGetGlobalPos (hObj, &pos);
	VECTOR3 cam = GetCameraGPos();
	double rad = oapiGetSize (hObj);
	double dst = dist (pos, cam);
	return (rad*double(viewH))/(dst*tan(oapiCameraAperture()));
}

// ===========================================================================================
//
void Scene::CheckVisual(OBJHANDLE hObj)
{
	_TRACE;

	if (hObj==NULL) return;

	VOBJREC *pv = FindVisual(hObj);
	if (!pv) pv = AddVisualRec(hObj);

	pv->apprad = float(GetObjectAppRad(hObj));

	if (pv->type == OBJTP_STAR) {
		pv->vobj->Activate(true);
		return;
	}

	if (pv->vobj->IsActive()) {
		if (pv->apprad < 1.0) pv->vobj->Activate(false);
	} else {
		if (pv->apprad > 2.0) pv->vobj->Activate(true);
	}
	// the range check has a small hysteresis to avoid continuous
	// creation/deletion for objects at the edge of visibility
}

// ===========================================================================================
//
const D3D9Light *Scene::GetLight(int index) const
{
	if ((DWORD)index<maxlight || index>=0) return &Lights[index];
	return &sunLight;
}

// ===========================================================================================
//
Scene::VOBJREC *Scene::FindVisual(OBJHANDLE hObj) const
{
	if (hObj==NULL) return NULL;
	VOBJREC *pv;
	for (pv=vobjFirst; pv; pv=pv->next) if (pv->vobj->Object()==hObj) return pv;
	return NULL;
}

// ===========================================================================================
//
class vObject *Scene::GetVisObject(OBJHANDLE hObj) const
{
	Scene::VOBJREC *v = FindVisual(hObj);
	if (v) return v->vobj;
	return NULL;
}

// ===========================================================================================
//
void Scene::DelVisualRec (VOBJREC *pv)
{
	_TRACE;
	// unlink the entry
	if (pv->prev) pv->prev->next = pv->next;
	else          vobjFirst = pv->next;

	if (pv->next) pv->next->prev = pv->prev;
	else          vobjLast = pv->prev;

	DebugControls::RemoveVisual(pv->vobj);

	vobjEnv = NULL;

	// delete the visual, its children and the entry itself
	gc->UnregisterVisObject(pv->vobj->GetObject());

	delete pv->vobj;
	delete pv;
}

// ===========================================================================================
//
void Scene::DeleteAllVisuals()
{
	_TRACE;
	VOBJREC *pv = vobjFirst;
	while (pv) {
		VOBJREC *pvn = pv->next;
		
		DebugControls::RemoveVisual(pv->vobj);

		__TRY {
			gc->UnregisterVisObject(pv->vobj->GetObject());
		}
		__EXCEPT(ExcHandler(GetExceptionInformation()))
		{
			LogErr("Exception in gc->UnregisterVisObject(pv->vobj->GetObject())");
			FatalAppExitA(0,"Critical error has occured. See Orbiter.log for details");
		}

		LogAlw("Deleting Visual 0x%X",pv->vobj);
		delete pv->vobj;
		delete pv;
		pv = pvn;
	}
	vobjFirst = vobjLast = NULL;
	vobjEnv = NULL;
}

// ===========================================================================================
//
Scene::VOBJREC *Scene::AddVisualRec(OBJHANDLE hObj)
{
	_TRACER;

	char buf[256];

	// create the visual and entry
	VOBJREC *pv = new VOBJREC;

	memset2(pv, 0, sizeof(VOBJREC));

	pv->vobj = vObject::Create(hObj, this);
	pv->type = oapiGetObjectType(hObj);

	oapiGetObjectName(hObj, buf, 255);

	VESSEL *hVes=NULL;
	if (pv->type==OBJTP_VESSEL) hVes = oapiGetVesselInterface(hObj);

	// link entry to end of list
	pv->prev = vobjLast;
	pv->next = NULL;
	if (vobjLast) vobjLast->next = pv;
	else          vobjFirst = pv;
	vobjLast = pv;

	LogAlw("RegisteringVisual (%s) hVessel=0x%X, hObj=0x%X, Vis=0x%X, Rec=0x%X, Type=%d", buf, hVes, hObj, pv->vobj, pv, pv->type);

	__TRY {
		gc->RegisterVisObject(hObj, (VISHANDLE)pv->vobj);
	}
	__EXCEPT(ExcHandler(GetExceptionInformation()))
	{
		char buf[64]; oapiGetObjectName(hObj, buf, 64);
		char *classname = NULL;
		if (oapiGetObjectType(hObj)==OBJTP_VESSEL) classname = oapiGetVesselInterface(hObj)->GetClassNameA();
		LogErr("Critical exception in gc->RegisterVisObject(0x%X, 0x%X) (%s).", hObj, pv->vobj, buf);
		if (classname) LogErr("VesselClass Name = %s",classname);
		gc->EmergencyShutdown();
		FatalAppExitA(0,"Critical error has occured. See Orbiter.log for details");
	}

	// Initialize Meshes
	pv->vobj->PreInitObject();
	
	return pv;
}

// ===========================================================================================
//
DWORD Scene::GetActiveParticleEffectCount()
{
	// render exhaust particle system
	DWORD count = 0;
	for (DWORD n = 0; n < nstream; n++) if (pstream[n]->IsActive()) count++;
	return count;
}

// ===========================================================================================
//
VECTOR3 Scene::SkyColour ()
{
	VECTOR3 col = {0,0,0};
	OBJHANDLE hProxy = oapiCameraProxyGbody();
	if (hProxy && oapiPlanetHasAtmosphere (hProxy)) {
		const ATMCONST *atmp = oapiGetPlanetAtmConstants (hProxy);
		VECTOR3 rc, rp, pc;
		rc = GetCameraGPos();
		oapiGetGlobalPos (hProxy, &rp);
		pc = rc-rp;
		double cdist = length (pc);
		if (cdist < atmp->radlimit) {
			ATMPARAM prm;
			oapiGetPlanetAtmParams (hProxy, cdist, &prm);
			normalise (rp);
			double coss = dotp (pc, rp) / -cdist;
			double intens = min (1.0,(1.0839*coss+0.4581)) * sqrt (prm.rho/atmp->rho0);
			// => intensity=0 at sun zenith distance 115°
			//    intensity=1 at sun zenith distance 60°
			if (intens > 0.0)
				col += _V(atmp->color0.x*intens, atmp->color0.y*intens, atmp->color0.z*intens);
		}
		for (int i=0;i<3;i++) if (col.data[i] > 1.0) col.data[i] = 1.0;
	}
	return col;
}

// ===========================================================================================
//
void Scene::Update ()
{
	_TRACE;

	// update particle streams - should be skipped when paused
	if (!oapiGetPause()) {
		for (DWORD i=0;i<nstream;) {
			if (pstream[i]->Expired()) DelParticleStream(i);
			else pstream[i++]->Update();
		}
	}

	static bool bFirstUpdate = true;

	// check object visibility (one object per frame in the interest
	// of scalability)
	DWORD nobj = oapiGetObjectCount();

	if (bFirstUpdate) {
		bFirstUpdate = false;
		for (DWORD i=0;i<nobj;i++) {
			OBJHANDLE hObj = oapiGetObjectByIndex(i);
			CheckVisual(hObj);
		}
	}
	else {

		if (iVCheck >= nobj) iVCheck = 0;

		// This function will browse through vessels and planets. (not bases)
		// Base visuals don't exist in the visual record. 
		OBJHANDLE hObj = oapiGetObjectByIndex(iVCheck++);
		CheckVisual(hObj);
	}


	// If Camera target has changed, setup mesh debugger
	//
	OBJHANDLE hTgt = oapiCameraTarget();

	if (hTgt!=Camera.hTarget && hTgt!=NULL) {
		
		Camera.hTarget = hTgt;

		if (oapiGetObjectType(hTgt)==OBJTP_SURFBASE) {
			OBJHANDLE hPlanet = oapiGetBasePlanet(hTgt);
			vPlanet *vp = (vPlanet *)GetVisObject(hPlanet);
			if (vp) {
				vBase *vb = vp->GetBaseByHandle(hTgt);
				if (vb) {
					if (DebugControls::IsActive()) {
						DebugControls::SetVisual(vb);
					}
					
				}
			}
			return;
		}

		vObject *vo = GetVisObject(hTgt);
		
		if (vo) {

			if (DebugControls::IsActive()) {
				DebugControls::SetVisual(vo);
			}

			// Why is this here ?
			//
			// kuddel: OrbiterSound 4.0 did not play the sounds of the 'focused'
			//         Vessel when focus changed during playback. Therfore the
			//         D3D9Client does a oapiSetFocusObject call when playback
			//         is running. Is a OrbiterSound error, but we can work-around
			//         this, so we do! To reproduce, just disable the following
			//         code and run the 'Welcome.scn'.
			//         See also: http://www.orbiter-forum.com/showthread.php?p=392689&postcount=18
			//         and following...

			// OrbiterSound 4.0 'playback helper'
			if (OapiExtension::RunsOrbiterSound40() &&
				oapiIsVessel(hTgt) && // oapiGetObjectType(vo->Object()) == OBJTP_VESSEL &&
				dynamic_cast<vVessel*>(vo)->Playback()
				)
			{
				// Orbiter doesn't do this when (only) camera focus changes
				// during playback, therfore we do it ;)
				oapiSetFocusObject(hTgt);
			}
			return;
		}
	}
}



// ===========================================================================================
// Compute a distance to a near/far plane
// ===========================================================================================

float Scene::ComputeNearClipPlane()
{
	float zsurf = 1000.0f;
	VOBJREC *pv = NULL;
	
	OBJHANDLE hObj = oapiCameraProxyGbody();
	VESSEL *hVes = oapiGetFocusInterface();
	
	if (hObj && hVes) {
		VECTOR3 pos;
		oapiGetGlobalPos(hObj,&pos);
		double g = atan(Camera.apsq);
		double t = dotp(unit(Camera.pos-pos), unit(Camera.dir));
		if (t<-1.0) t=1.0; if (t>1.0) t=1.0f;
		double a = PI - acos(t);
		double R = oapiGetSize(Camera.hObj_proxy) + hVes->GetSurfaceElevation();
		double r = length(Camera.pos-pos);
		double h = r - R;
		if (h<10e3) {
			double d = a - g; if (d<0) d=0;
			zsurf = float(h*cos(g)/cos(d));
			if (zsurf>1000.0f || zsurf<0.0f) zsurf=1000.0f;
		}
	}

	float zmin = 1.0f;
	if (GetCameraAltitude()>10e3) zmin = 0.1f;

	int count = 0;
	int actbase = 0;
	vPlanet *pl = NULL;

	float farpoint = 0.0f;
	float nearpoint = 10e3f;
	float neardist = 10e3f;

	for (pv = vobjFirst; pv; pv = pv->next) {

		float nr = 10e3f;
		float fr = 0.0f;
		float dn = 10e3f;

		bool bCockpit = false;

		if (pv->type==OBJTP_VESSEL) {	
			if (pv->vobj==vFocus) {
				bCockpit = oapiCameraInternal();
			}
		}

		if (pv->apprad>0.01 && pv->vobj->IsActive()) {

			vObject *obj = pv->vobj;

			if (pv->type==OBJTP_PLANET) {
				if (obj->Object()==hObj) pl = (vPlanet*)obj;
				obj->GetMinMaxDistance(&nr, &fr, &dn);
				if (dn<neardist) neardist = dn;
				if (nr<nearpoint) nearpoint = nr;
				if (fr>farpoint) farpoint = fr;
				continue;
			}

			if (pv->type==OBJTP_VESSEL) {
		
				if (obj->IsVisible()) {

					if (bCockpit) if (vFocus->HasExtPass()==false) continue; // Ignore MinMax

					obj->GetMinMaxDistance(&nr, &fr, &dn);

					if (dn<neardist) neardist = dn;
					if (nr<nearpoint) nearpoint = nr;
					if (fr>farpoint) farpoint = fr;
					count++;
				}
			}
		}
	}

	if (pl) {

		float nr = 10e3;
		float fr = 0.0f;
		float dn = 10e3;

		DWORD bc = pl->GetBaseCount();
		for (DWORD i=0;i<bc;i++) {
			vBase *vb = pl->GetBaseByIndex(i);
			if (vb) {
				if (vb->IsActive() && vb->IsVisible()) {
					vb->GetMinMaxDistance(&nr, &fr, &dn);
					if (dn<neardist) neardist = dn;
					if (nr<nearpoint) nearpoint = nr;
					if (fr>farpoint) farpoint = fr;
					actbase++;
				}
			}
		}
	}

	DWORD prteff = GetActiveParticleEffectCount();

	if (farpoint==0.0) farpoint = 20e4;

	float znear = D9NearPlane(pDevice, nearpoint, farpoint, neardist, GetProjectionMatrix(), (prteff!=0));

	if (oapiCameraInternal()) {
		if (Config->NearClipPlane==0) zmin = 1.0f;
		else						  zmin = 0.1f;
	}

	znear = min(znear, zsurf);
	znear = max(znear, zmin);

	return znear;
}





// ===========================================================================================
// Prepare scene for rendering
//
// - Update camera for rendering of the main scene
// - Update all visuals
// - Distance sort planets
// - Setup sky color
// - Setup local light sources
// ===========================================================================================

void Scene::UpdateCamVis()
{
	dwFrameId++;				// Advance to a next frame

	// Update camera parameters --------------------------------------
	// and call vObject::Update() for all visuals
	//
	UpdateCameraFromOrbiter(RENDERPASS_MAINSCENE);	

	if (Camera.hObj_proxy) D3D9Effect::UpdateEffectCamera(Camera.hObj_proxy);
	
	// Clear active local lisghts list -------------------------------
	//
	memset2(Lights, 0, 12*sizeof(D3D9Light));

	// Update Sunlight direction -------------------------------------
	// 
	VECTOR3 rpos;
	oapiGetGlobalPos(hSun, &rpos);
	rpos -= Camera.pos; 
	D3DVEC(-unit(rpos), sunLight.Direction);
	D3DVEC(rpos, sunLight.Position);

	// Get focus visual -----------------------------------------------
	// 
	OBJHANDLE hFocus = oapiGetFocusObject();
	vFocus = NULL;
	for (VOBJREC *pv=vobjFirst; pv; pv=pv->next) {
		if (pv->type==OBJTP_VESSEL) if (pv->vobj->Object()==hFocus) {
			vFocus = (vVessel *)pv->vobj;
			break;
		}
	}

	// Compute SkyColor -----------------------------------------------
	//
	sky_color = SkyColour();
	bg_rgba = D3DCOLOR_RGBA ((int)(sky_color.x*255), (int)(sky_color.y*255), (int)(sky_color.z*255), 255);


	// Process Local Light Sources -------------------------------------
	//
	if (bLocalLight) {

		ClearLocalLights();

		VOBJREC *pv = NULL;
		for (pv = vobjFirst; pv; pv = pv->next) {
			if (!pv->vobj->IsActive()) continue;
			OBJHANDLE hObj = pv->vobj->Object();
			if (oapiGetObjectType (hObj) == OBJTP_VESSEL) {
				VESSEL *vessel = oapiGetVesselInterface (hObj);
				DWORD nemitter = vessel->LightEmitterCount();
				for (DWORD j = 0; j < nemitter; j++) {
					const LightEmitter *em = vessel->GetLightEmitter(j);
					if (em->GetVisibility() & LightEmitter::VIS_EXTERNAL) AddLocalLight(em, pv->vobj);
				}
			}
		}
	}


	// ----------------------------------------------------------------
	// render solar system celestial objects (planets and moons)
	// we render without z-buffer, so need to distance-sort the objects
	// ----------------------------------------------------------------

	VOBJREC *pv = NULL;
	nplanets = 0;

	for (pv = vobjFirst; pv && nplanets < MAXPLANET; pv = pv->next) {
		if (pv->apprad < 0.01 && pv->type != OBJTP_STAR) continue;
		if (pv->type == OBJTP_PLANET || pv->type == OBJTP_STAR) {
			plist[nplanets].vo = pv->vobj;
			plist[nplanets].dist = pv->vobj->CamDist();
			nplanets++;
		}
	}

	int distcomp(const void *arg1, const void *arg2);

	qsort((void*)plist, nplanets, sizeof(PList), distcomp);
}

// ===========================================================================================
//
void Scene::ClearLocalLights()
{
	nLights  = 0;
	lmaxdst2 = 0.0f;
	lmaxidx  = 0;
}

// ===========================================================================================
//
void Scene::AddLocalLight(const LightEmitter *le, const vObject *vo)
{
	if (Lights==NULL) return;
	if (le->IsActive()==false || le->GetIntensity()==0.0) return;

	// -----------------------------------------------------------------------------
	D3DXVECTOR3 pos;
	D3DXVec3TransformCoord(&pos, &D3DXVEC(le->GetPosition()), vo->MWorld());
	float Dst2 = D3DXVec3Dot(&pos, &pos);

	if (nLights==maxlight && Dst2>lmaxdst2) return;

	// -----------------------------------------------------------------------------
	D3D9Light lght;
	memset2(&lght, 0, sizeof(D3D9Light)); 

	const double *att = ((PointLight*)le)->GetAttenuation();
	lght.Attenuation = D3DXVECTOR3((float)att[0], (float)att[1], (float)att[2]);
	lght.Param[D3D9LRange] = (float)((SpotLight*)le)->GetRange();
	lght.Dst2 = Dst2;
	lght.Position = pos;

	// -----------------------------------------------------------------------------
	switch (le->GetType()) {

		case LightEmitter::LT_POINT: {
			lght.Type = D3DLIGHT_POINT;
		} break;

		case LightEmitter::LT_SPOT: {
			lght.Type = D3DLIGHT_SPOT;
			lght.Param[D3D9LFalloff] = 1.0f;
			lght.Param[D3D9LTheta]   = (float)((SpotLight*)le)->GetUmbra();
			lght.Param[D3D9LPhi]     = (float)((SpotLight*)le)->GetPenumbra();	
			lght.Param[D3D9LPhi]     = cos(lght.Param[D3D9LPhi]*0.5f);
			lght.Param[D3D9LTheta]   = 1.0f/(cos(lght.Param[D3D9LTheta]*0.5f)-lght.Param[D3D9LPhi]);
		} break;

		default:
			LogErr("Invalid Light Emitter Type");
			break;
	}

	// -----------------------------------------------------------------------------
	double intens = le->GetIntensity();

	const COLOUR4 &col_d = le->GetDiffuseColour();
	lght.Diffuse.r = (float)(col_d.r*intens);
	lght.Diffuse.g = (float)(col_d.g*intens);
	lght.Diffuse.b = (float)(col_d.b*intens);
	lght.Diffuse.a = (float)(col_d.a*intens);

	const COLOUR4 &col_s = le->GetSpecularColour();
	lght.Specular.r = (float)(col_s.r*intens);
	lght.Specular.g = (float)(col_s.g*intens);
	lght.Specular.b = (float)(col_s.b*intens);
	lght.Specular.a = (float)(col_s.a*intens);

	const COLOUR4 &col_a = le->GetAmbientColour();
	lght.Ambient.r = (float)(col_a.r*intens);
	lght.Ambient.g = (float)(col_a.g*intens);
	lght.Ambient.b = (float)(col_a.b*intens);
	lght.Ambient.a = (float)(col_a.a*intens);

	// -----------------------------------------------------------------------------
	if (lght.Type != D3DLIGHT_POINT) {
		const VECTOR3 dir = le->GetDirection();
		D3DXVECTOR3 d((float)dir.x, (float)dir.y, (float)dir.z); 
		D3DXVec3TransformNormal((D3DXVECTOR3 *)&lght.Direction, &d, vo->MWorld());
		D3DXVec3Normalize((D3DXVECTOR3 *)&lght.Direction, (D3DXVECTOR3 *)&lght.Direction);
	}

	// -----------------------------------------------------------------------------
	// Replace or Add
	if (nLights==maxlight) {
		Lights[lmaxidx] = lght;
		lmaxdst2 = 0.0f;
		for (DWORD i=0;i<maxlight;i++) if (Lights[i].Dst2>lmaxdst2) lmaxdst2 = Lights[i].Dst2, lmaxidx = i;	
	}
	else {
		Lights[nLights] = lght;
		if (lght.Dst2>lmaxdst2) lmaxdst2 = lght.Dst2, lmaxidx = nLights;
		nLights++;
	}
}





// ===========================================================================================
//
void Scene::RenderMainScene()
{
	_TRACE;

	UpdateCamVis();

	if (vFocus==NULL) return;

	double scene_time = D3D9GetTime();

	if (DebugControls::IsActive()) {
		HR(pDevice->Clear(0, NULL, D3DCLEAR_TARGET|D3DCLEAR_ZBUFFER|D3DCLEAR_STENCIL, 0, 1.0f, 0L));
		DWORD flags  = *(DWORD*)gc->GetConfigParam(CFGPRM_GETDEBUGFLAGS);
		if (flags&DBG_FLAGS_WIREFRAME) pDevice->SetRenderState(D3DRS_FILLMODE, D3DFILL_WIREFRAME);
		else						   pDevice->SetRenderState(D3DRS_FILLMODE, D3DFILL_SOLID);
	}
	else {
		// Clear the viewport
		HR(pDevice->Clear(0, NULL, D3DCLEAR_TARGET|D3DCLEAR_ZBUFFER|D3DCLEAR_STENCIL, 0, 1.0f, 0L));
	}
	
	if (FAILED (pDevice->BeginScene())) return;
	else bRendering = true;

	float znear = ComputeNearClipPlane();
	
	if (DebugControls::IsActive()) {
		DWORD camMode = *(DWORD*)gc->GetConfigParam(CFGPRM_GETCAMERAMODE);
		if (camMode!=0) znear = 0.1f;
	}

	SetCameraFrustumLimits(znear, 1e8f);

	// -------------------------------------------------------------------------------------------------------
	// render celestial sphere background
	// -------------------------------------------------------------------------------------------------------

	pDevice->SetRenderState(D3DRS_CULLMODE, D3DCULL_CW);

	int bglvl = 0;
	if (bg_rgba) { // suppress stars darker than the background
		bglvl = (bg_rgba & 0xff) + ((bg_rgba >> 8) & 0xff) + ((bg_rgba >> 16) & 0xff);
		bglvl = min (bglvl/2, 255);
	}

	bool bEnableAtmosphere = false;

	vPlanet *vPl = GetCameraProxyVisual();

	if (vPl) {		
		bEnableAtmosphere = vPl->CameraInAtmosphere();
		PlanetRenderer::InitializeScattering(vPl);
	}

	// -------------------------------------------------------------------------------------------------------
	// Render Celestial Sphere Background Image 
	// -------------------------------------------------------------------------------------------------------

	cspheremgr->Render(pDevice, 8, bglvl);

	pDevice->SetRenderState(D3DRS_CULLMODE, D3DCULL_CCW);

	// -------------------------------------------------------------------------------------------------------
	// planetarium mode (celestial sphere elements)
	// -------------------------------------------------------------------------------------------------------

	DWORD plnmode = *(DWORD*)gc->GetConfigParam(CFGPRM_PLANETARIUMFLAG);

	if (plnmode & PLN_ENABLE) {

		float linebrt = 1.0f - float(sky_color.x+sky_color.y+sky_color.z) / 3.0f;

		HR(FX->SetTechnique(eLine));
		HR(FX->SetMatrix(eWVP, GetProjectionViewMatrix()));
		
		// render ecliptic grid ----------------------------------------------------------------------------
		//
		if (plnmode & PLN_EGRID) {
			D3DXVECTOR4 vColor(0.0f, 0.0f, 0.4f*linebrt, 1.0f);
			HR(FX->SetVector(eColor, &vColor));
			csphere->RenderGrid(FX, !(plnmode & PLN_ECL));	
			
		}
		if (plnmode & PLN_ECL)	 {
			D3DXVECTOR4 vColor(0.0f, 0.0f, 0.8f*linebrt, 1.0f);
			HR(FX->SetVector(eColor, &vColor));
			csphere->RenderGreatCircle(FX);
		}

		// render celestial grid ----------------------------------------------------------------------------
		//
		if (plnmode & (PLN_CGRID|PLN_EQU)) {
			double obliquity = 0.4092797095927;
			double coso = cos(obliquity), sino = sin(obliquity);
			D3DXMATRIX rot(1.0f,0.0f,0.0f,0.0f,  0.0f,(float)coso,(float)sino,0.0f,  0.0f,-(float)sino,(float)coso,0.0f,  0.0f,0.0f,0.0f,1.0f);
			
			D3DXMatrixMultiply(&rot, &rot, GetProjectionViewMatrix());
			HR(FX->SetMatrix(eWVP, &rot));

			if (plnmode & PLN_CGRID) {
				D3DXVECTOR4 vColor(0.35f*linebrt, 0.0f, 0.35f*linebrt, 1.0f);
				HR(FX->SetVector(eColor, &vColor));
				csphere->RenderGrid(FX, !(plnmode & PLN_EQU));
			}
			if (plnmode & PLN_EQU)	 {
				D3DXVECTOR4 vColor(0.7f*linebrt, 0.0f, 0.7f*linebrt, 1.0f);
				HR(FX->SetVector(eColor, &vColor));
				csphere->RenderGreatCircle(FX);
			}
		}

		// render constellation lines ----------------------------------------------------------------------------
		//
		if (plnmode & PLN_CONST) {
			HR(FX->SetMatrix(eWVP, GetProjectionViewMatrix()));
			D3DXVECTOR4 vColor(0.4f*linebrt, 0.3f*linebrt, 0.2f*linebrt, 1.0f);
			HR(FX->SetVector(eColor, &vColor));
			csphere->RenderConstellations(FX);		
		}
	}

	// -------------------------------------------------------------------------------------------------------
	// render stars
	// -------------------------------------------------------------------------------------------------------

	HR(FX->SetTechnique(eStar));
	HR(FX->SetMatrix(eWVP, GetProjectionViewMatrix()));
	csphere->RenderStars(FX, (DWORD)-1, &sky_color);


	// -------------------------------------------------------------------------------------------------------
	// Sketchpad for planetarium mode labels and markers
	// -------------------------------------------------------------------------------------------------------

	oapi::Sketchpad *pSketch = oapiGetSketchpad(gc->GetBackBufferHandle());

	pSketch->SetFont(pLabelFont);
	pSketch->SetTextAlign(Sketchpad::CENTER, Sketchpad::BOTTOM);
	
	// -------------------------------------------------------------------------------------------------------
	// render star markers
	// -------------------------------------------------------------------------------------------------------

	if (plnmode & PLN_ENABLE) {

		// constellation labels --------------------------------------------------
		if (plnmode & PLN_CNSTLABEL) {
			const GraphicsClient::LABELSPEC *ls;
			DWORD n, nlist;
			char *label;

			pSketch->SetTextColor(labelCol[5]);
			pSketch->SetPen(lblPen[5]);

			nlist = gc->GetConstellationMarkers(&ls);
			for (n = 0; n < nlist; ++n) {
				label = (plnmode & PLN_CNSTLONG) ? ls[n].label[0] : ls[n].label[1];
				RenderDirectionMarker(pSketch, ls[n].pos, NULL, label, -1, 0);
			}
		} 
		// celestial marker (stars) names ----------------------------------------
		if (plnmode & PLN_CCMARK) {
			const GraphicsClient::LABELLIST *list;
			DWORD n, nlist;
			nlist = gc->GetCelestialMarkers(&list);
				
			for (n = 0; n < nlist; n++) {
				if (list[n].active) {		
					int size = (int)(viewH/80.0*list[n].size+0.5);
				
					pSketch->SetTextColor(labelCol[list[n].colour]);
					pSketch->SetPen(lblPen[list[n].colour]);

					const GraphicsClient::LABELSPEC *ls = list[n].list;
					for (int i=0;i<list[n].length;i++) RenderDirectionMarker(pSketch, ls[i].pos, ls[i].label[0], ls[i].label[1], list[n].shape, size);
				}
			}	
		} 
	}

	// Render Planets

	for (DWORD i=0;i<nplanets;i++) {

		// double nplane, fplane;
		// plist[i].vo->RenderZRange (&nplane, &fplane);
		// cam->SetFrustumLimits (nplane, fplane);
		// since we are not using z-buffers here, we can adjust the projection
		// matrix at will to make sure the object is within the viewing frustum

		OBJHANDLE hObj = plist[i].vo->Object();
		bool isActive = plist[i].vo->IsActive();

		if (isActive) plist[i].vo->Render(pDevice);
		else		  plist[i].vo->RenderDot(pDevice);

		if (plnmode & PLN_ENABLE) {

			if (plnmode & PLN_CMARK) {
				VECTOR3 pp;
				char name[256];
				oapiGetObjectName(hObj, name, 256);
				oapiGetGlobalPos(hObj, &pp);

				pSketch->SetTextColor(labelCol[0]);
				pSketch->SetPen(lblPen[0]);
				RenderObjectMarker(pSketch, pp, name, 0, 0, viewH/80);
			}

			if (isActive && (plnmode & PLN_SURFMARK) && (oapiGetObjectType (hObj) == OBJTP_PLANET)) {

				if (plnmode & PLN_LMARK) { // user-defined planetary surface labels
					double rad = oapiGetSize (hObj);
					double apprad = rad/(plist[i].dist * tan(GetCameraAperture()));
					const GraphicsClient::LABELLIST *list;
					DWORD n, nlist;
					MATRIX3 prot;
					VECTOR3 ppos, cpos;

					nlist = gc->GetSurfaceMarkers (hObj, &list);

					oapiGetRotationMatrix (hObj, &prot);
					oapiGetGlobalPos (hObj, &ppos);
					VECTOR3 cp = GetCameraGPos();
					cpos = tmul (prot, cp-ppos); // camera in local planet coords

					for (n=0;n<nlist;n++) {

						if (list[n].active && apprad*list[n].distfac > LABEL_DISTLIMIT) {
								
							int size = (int)(viewH/80.0*list[n].size+0.5);
							int col = list[n].colour;

							pSketch->SetTextColor(labelCol[col]);
							pSketch->SetPen(lblPen[col]);

							const GraphicsClient::LABELSPEC *ls = list[n].list;
							VECTOR3 sp;
							for (int j=0;j<list[n].length;j++) {
								if (dotp (ls[j].pos, cpos-ls[j].pos) >= 0.0) { // surface point visible?
									sp = mul (prot, ls[j].pos) + ppos;
									RenderObjectMarker(pSketch, sp, ls[j].label[0], ls[j].label[1], list[n].shape, size);
								}
							}
						}
					}
				}

				if (plnmode & PLN_BMARK) {

					DWORD n = oapiGetBaseCount(hObj);
					MATRIX3 prot;
					oapiGetRotationMatrix (hObj, &prot);
					int size = (int)(viewH/80.0);

					pSketch->SetTextColor(labelCol[0]);
					pSketch->SetPen(lblPen[0]);

					for (DWORD i=0;i<n;i++) {

						OBJHANDLE hBase = oapiGetBaseByIndex(hObj, i);

						VECTOR3 ppos, cpos, bpos;

						oapiGetGlobalPos(hObj, &ppos);
						oapiGetGlobalPos(hBase, &bpos);
						VECTOR3 cp = GetCameraGPos();
						cpos = tmul (prot, cp-ppos); // camera in local planet coords
						bpos = tmul (prot, bpos-ppos);

						double apprad = 8000e3 / (length(cpos-bpos) * tan(GetCameraAperture()));

						if (dotp(bpos, cpos-bpos) >= 0.0 && apprad > LABEL_DISTLIMIT) { // surface point visible?
							char name[64]; oapiGetObjectName(hBase, name, 63);
							VECTOR3 sp = mul (prot, bpos) + ppos;
							RenderObjectMarker(pSketch, sp, name, NULL, 0, size);
						}
					}
				}
			}
		} 
	} 

    	
	// -------------------------------------------------------------------------------------------------------
	// render the vessel objects
	// -------------------------------------------------------------------------------------------------------

	D3D9Effect::UpdateEffectCamera(Camera.hObj_proxy);

	pSketch->SetTextColor(labelCol[0]);
	pSketch->SetPen(lblPen[0]);

	VOBJREC *pv = NULL;

	for (pv=vobjFirst; pv; pv=pv->next) {
		if (!pv->vobj->IsActive()) continue;
		if (!pv->vobj->IsVisible()) continue;
	
		OBJHANDLE hObj = pv->vobj->Object();
		if (oapiGetObjectType(hObj) == OBJTP_VESSEL) {

			pv->vobj->Render(pDevice);
			
			if ((plnmode & (PLN_ENABLE|PLN_VMARK)) == (PLN_ENABLE|PLN_VMARK)) {
				VECTOR3 gpos;
				char name[256];
				oapiGetGlobalPos(hObj, &gpos);
				oapiGetObjectName(hObj, name, 256);
				RenderObjectMarker(pSketch, gpos, name, 0, 0, viewH/80);		
			}
		}
	}

	// -------------------------------------------------------------------------------------------------------
	// render the vessel sub-systems
	// -------------------------------------------------------------------------------------------------------

	// render exhausts
	//
	for (pv=vobjFirst; pv; pv=pv->next) {
		if (!pv->vobj->IsActive()) continue;
		if (!pv->vobj->IsVisible()) continue;
		OBJHANDLE hObj = pv->vobj->Object();
		if (oapiGetObjectType(hObj) == OBJTP_VESSEL) {
			((vVessel*)pv->vobj)->RenderExhaust();
		}
	}

	// render beacons
	//
	for (pv=vobjFirst; pv; pv=pv->next) {
		if (!pv->vobj->IsActive()) continue;
		pv->vobj->RenderBeacons(pDevice);
	}

	// render grapple points
    //
    for (pv=vobjFirst; pv; pv=pv->next) {
        if (!pv->vobj->IsActive()) continue;
        pv->vobj->RenderGrapplePoints(pDevice);
    }  

	// render exhaust particle system
	//
	for (DWORD n = 0; n < nstream; n++) pstream[n]->Render(pDevice);
	

	// -------------------------------------------------------------------------------------------------------
	// Render vessel axis vectors
	// -------------------------------------------------------------------------------------------------------

	DWORD bfvmode = *(DWORD*)gc->GetConfigParam(CFGPRM_SHOWBODYFORCEVECTORSFLAG);
	DWORD scamode = *(DWORD*)gc->GetConfigParam(CFGPRM_SHOWCOORDINATEAXESFLAG);

	if (bfvmode&BFV_ENABLE || scamode&SCA_ENABLE)
	{
		pSketch->SetFont(pAxisFont);
		pSketch->SetTextAlign(Sketchpad::LEFT, Sketchpad::TOP);
		pDevice->Clear(0, NULL, D3DCLEAR_ZBUFFER,  0, 1.0f, 0L); // clear z-buffer

		for (pv=vobjFirst; pv; pv=pv->next) {
			if (!pv->vobj->IsActive()) continue;
			if (!pv->vobj->IsVisible()) continue;
			if (oapiCameraInternal() && vFocus==pv->vobj) continue;

			pv->vobj->RenderAxis(pDevice, pSketch);
		}
	}



	// -------------------------------------------------------------------------------------------------------
	// render the internal parts of the focus object in a separate render pass
	// -------------------------------------------------------------------------------------------------------

	if (oapiCameraInternal() && vFocus) {

		// switch cockpit lights on, external-only lights off
		//
		if (bLocalLight) {
			ClearLocalLights();
			VESSEL *vessel = oapiGetFocusInterface();
			DWORD nemitter = vessel->LightEmitterCount();
			for (DWORD j = 0; j < nemitter; j++) {
				const LightEmitter *em = vessel->GetLightEmitter(j);
				if (em->GetVisibility() & LightEmitter::VIS_COCKPIT) AddLocalLight(em, vFocus);
			}
		}

		pDevice->Clear(0, NULL, D3DCLEAR_ZBUFFER,  0, 1.0f, 0L); // clear z-buffer
		double znear = Config->VCNearPlane;
		if (znear<0.01) znear=0.01;
		if (znear>1.0)  znear=1.0;
		OBJHANDLE hFocus = oapiGetFocusObject();
		SetCameraFrustumLimits(znear, oapiGetSize(hFocus));
		vFocus->Render(pDevice, true);
	}

	pDevice->SetRenderState(D3DRS_FILLMODE, D3DFILL_SOLID);
	
	pSketch->SetFont(NULL);
	pSketch->SetPen(NULL);

	oapiReleaseSketchpad(pSketch); pSketch = NULL;

	gc->MakeRenderProcCall(gc->GetBackBufferHandle(), RENDERPROC_HUD_1ST);
	gc->Render2DOverlay();
	gc->MakeRenderProcCall(gc->GetBackBufferHandle(), RENDERPROC_HUD_2ND);


	// -------------------------------------------------------------------------------------------------------
	// Draw Debug String on a bottom of the screen
	// -------------------------------------------------------------------------------------------------------

	int len = strlen(oapiDebugString());

	if (len>0) { 

		oapi::Sketchpad *pSketch = oapiGetSketchpad(gc->GetBackBufferHandle());

		oapi::Pen * nullp = oapiCreatePen(0, 1, 0xFFFFFF);
		oapi::Brush *brush = oapiCreateBrush(0xB0000000);

		pSketch->SetFont(pDebugFont);
		pSketch->SetTextColor(0xFFFFFF);
		pSketch->SetTextAlign(Sketchpad::LEFT, Sketchpad::BOTTOM);
		pSketch->SetPen(nullp);
		pSketch->SetBrush(brush);

		DWORD height = Config->DebugFontSize;
		DWORD width  = pSketch->GetTextWidth(oapiDebugString(), len);

		pSketch->Rectangle(-1, viewH-height-1, width+4, viewH);
		pSketch->Text(2, viewH-2, oapiDebugString(), len);

		pSketch->SetPen(NULL);
		pSketch->SetBrush(NULL);

		oapiReleasePen(nullp);
		oapiReleaseBrush(brush);
		oapiReleaseSketchpad(pSketch);
	}
		
	// End Of Main Scene Rendering ---------------------------------------------
	//
	HR(pDevice->EndScene());
	bRendering = false;


	// -------------------------------------------------------------------------------------------------------
	// Render Custom Camera Views 
	// -------------------------------------------------------------------------------------------------------

	

	if (Config->CustomCamMode == 0 && dwTurn == RENDERTURN_CUSTOMCAM) dwTurn++;
	if (Config->EnvMapMode == 0 && dwTurn == RENDERTURN_ENVCAM) dwTurn++;
	if (dwTurn>RENDERTURN_LAST) dwTurn = 0;

	int RenderCount = max(1, Config->EnvMapFaces);
	
	// --------------------------------------------------------------------------------------------------------
	// Render Custom Camera view for a focus vessel
	//
	if (dwTurn==RENDERTURN_CUSTOMCAM) {
		if (Config->CustomCamMode && vFocus) {
			if (camCurrent==NULL) camCurrent = camFirst;
			OBJHANDLE hVessel = vFocus->GetObjectA();
			while (camCurrent) {
				if (camCurrent->hVessel==hVessel && camCurrent->bActive) {
					RenderCustomCameraView(camCurrent);
					camCurrent = camCurrent->next;
					break;
				}
				camCurrent = camCurrent->next;
			}
		}
	}

	// -------------------------------------------------------------------------------------------------------
	// Render Environmental Map For the Focus Vessel 
	// 
	if (dwTurn==RENDERTURN_ENVCAM) {

		if (Config->EnvMapMode) {
			DWORD flags = 0;
			if (Config->EnvMapMode==1) flags |= 0x01; 
			if (Config->EnvMapMode==2) flags |= 0x03;

			if (vobjEnv==NULL) vobjEnv = vobjFirst;

			while (vobjEnv) {	
				if (vobjEnv->type==OBJTP_VESSEL && vobjEnv->apprad>8.0f) {
					if (vobjEnv->vobj) {
						vVessel *vVes = (vVessel *)vobjEnv->vobj;
						if (vVes->RenderENVMap(pDevice, RenderCount, flags)) {
							vobjEnv = vobjEnv->next;
							break;
						}	
					}
				}
				vobjEnv = vobjEnv->next;
			}
		}
	}



	// -------------------------------------------------------------------------------------------------------
	// EnvMap Debugger  TODO: Should be allowed to visualize other maps as well, not just index 0
	// -------------------------------------------------------------------------------------------------------
	// pDevice->StretchRect(psgNormal, NULL, gc->GetBackBuffer(), NULL, D3DTEXF_LINEAR);
	
	if (DebugControls::IsActive()) {
		DWORD flags  = *(DWORD*)gc->GetConfigParam(CFGPRM_GETDEBUGFLAGS);
		if (flags&DBG_FLAGS_DSPENVMAP) VisualizeCubeMap(vFocus->GetEnvMap(0));
	}
	

	scene_time = D3D9GetTime() - scene_time;

	if (scene_time>D3D9Stats.Timer.ScenePeak) D3D9Stats.Timer.ScenePeak = scene_time;
	D3D9Stats.Timer.Scene += scene_time;

	dwTurn++;
}


// ===========================================================================================
//
void Scene::RenderSecondaryScene(vObject *omit, bool bOmitAtc, DWORD flags)
{
	_TRACE;

	D3D9Effect::UpdateEffectCamera(GetCameraProxyBody());

	// Clear the viewport
	HR(pDevice->Clear(0, NULL, D3DCLEAR_TARGET|D3DCLEAR_ZBUFFER|D3DCLEAR_STENCIL, 0, 1.0f, 0L));
	
	if (FAILED (pDevice->BeginScene())) return;

	VOBJREC *pv = NULL;

	// render planets -------------------------------------------
	//
	if (flags&0x01) {
		for (DWORD i=0;i<nplanets;i++) {
			bool isActive = plist[i].vo->IsActive();
			if (isActive) plist[i].vo->Render(pDevice);  // TODO: Should pass the flags to surface base level
			else		  plist[i].vo->RenderDot(pDevice);
		} 
	}

	// render the vessel objects --------------------------------
	// 
	if (flags&0x02) {
		for (pv=vobjFirst; pv; pv=pv->next) {
			if (!pv->vobj->IsActive()) continue;
			if (!pv->vobj->IsVisible()) continue;
			if (pv->vobj->bOmit) continue;
			OBJHANDLE hObj = pv->vobj->Object();
			if (oapiGetObjectType(hObj) == OBJTP_VESSEL) pv->vobj->Render(pDevice);
		}
	}


	// render exhausts -------------------------------------------
	//
	if (flags&0x04) {
		for (pv=vobjFirst; pv; pv=pv->next) {
			if (!pv->vobj->IsActive()) continue;
			if (!pv->vobj->IsVisible()) continue;
			if (pv->vobj->bOmit) continue;
			OBJHANDLE hObj = pv->vobj->Object();
			if (oapiGetObjectType(hObj) == OBJTP_VESSEL) ((vVessel*)pv->vobj)->RenderExhaust();
		}
	}

	// render beacons -------------------------------------------
	//
	if (flags&0x08) {
		for (pv=vobjFirst; pv; pv=pv->next) {
			if (!pv->vobj->IsActive()) continue;
			if (pv->vobj->bOmit) continue;
			pv->vobj->RenderBeacons(pDevice);
		}
	}

	// render exhaust particle system ----------------------------
	if (flags&0x10) {
		for (DWORD n = 0; n < nstream; n++) pstream[n]->Render(pDevice);
	}
	
	HR(pDevice->EndScene());
}

// ===========================================================================================
//
void Scene::RenderLightPrePass()
{
	_TRACE;
	UpdateCamVis();
	if (vFocus==NULL) return;
	float znear = ComputeNearClipPlane();
	SetCameraFrustumLimits(znear, 1e8f);

	pDevice->SetRenderTarget(0, psgDepth);		// R32F
	pDevice->SetRenderTarget(1, psgNormal);		// A2RGB10
	pDevice->SetRenderTarget(2, psgSpecular);	// G16R16F

	D3D9Effect::UpdateEffectCamera(GetCameraProxyBody());

	// Clear the viewport
	HR(pDevice->Clear(0, NULL, D3DCLEAR_TARGET|D3DCLEAR_ZBUFFER|D3DCLEAR_STENCIL, 0, 1.0f, 0L));
	
	if (FAILED (pDevice->BeginScene())) return;

	VOBJREC *pv = NULL;

	// render the vessel objects --------------------------------
	// 
	for (pv=vobjFirst; pv; pv=pv->next) {
		if (!pv->vobj->IsActive()) continue;
		if (!pv->vobj->IsVisible()) continue;
		OBJHANDLE hObj = pv->vobj->Object();
		if (oapiGetObjectType(hObj) == OBJTP_VESSEL) pv->vobj->Render(pDevice);
	}

	HR(pDevice->EndScene());
}

// ===========================================================================================
//
void Scene::ClearOmitFlags()
{
	VOBJREC *pv = NULL;
	for (pv=vobjFirst; pv; pv=pv->next) pv->vobj->bOmit = false;
}

// ===========================================================================================
//
void Scene::VisualizeCubeMap(LPDIRECT3DCUBETEXTURE9 pCube)
{
	if (!pCube) return;

	LPDIRECT3DSURFACE9 pSrf = NULL;
	LPDIRECT3DSURFACE9 pBack = gc->GetBackBuffer();

	D3DSURFACE_DESC ldesc;
	D3DSURFACE_DESC bdesc;

	if (!pBack) return;

	HR(pBack->GetDesc(&bdesc));
	HR(pCube->GetLevelDesc(0, &ldesc));

	DWORD x, y, h = min(ldesc.Height, bdesc.Height/3);

	for (DWORD i=0;i<6;i++) {

		HR(pCube->GetCubeMapSurface(D3DCUBEMAP_FACES(i), 0, &pSrf));
		
		switch (i) {
			case 0:	x = 2*h; y=h; break;
			case 1:	x = 0;   y=h; break;
			case 2:	x = 1*h; y=0; break;
			case 3:	x = 1*h; y=2*h; break;
			case 4:	x = 1*h; y=h; break;
			case 5:	x = 3*h; y=h; break;
		}

		RECT dr;
		dr.left = x;
		dr.top = y;
		dr.bottom = y+h;
		dr.right = x+h;

		HR(pDevice->StretchRect(pSrf, NULL, pBack, &dr, D3DTEXF_LINEAR));

		SAFE_RELEASE(pSrf);	
	}
}



// ===========================================================================================
//
void Scene::RenderVesselShadows (OBJHANDLE hPlanet, float depth) const
{
	// render vessel shadows
	VOBJREC *pv;
	for (pv = vobjFirst; pv; pv = pv->next) {
		if (!pv->vobj->IsActive()) continue;
		if (oapiGetObjectType(pv->vobj->Object()) == OBJTP_VESSEL)
			((vVessel*)(pv->vobj))->RenderGroundShadow(pDevice, hPlanet, depth);
	}	

	// reset device parameters
	pDevice->SetRenderState(D3DRS_STENCILENABLE, FALSE);
	
	// render particle shadows
	LPDIRECT3DTEXTURE9 tex = 0;
	for (DWORD j=0;j<nstream;j++) pstream[j]->RenderGroundShadow(pDevice, tex);
}

// ===========================================================================================
//
void Scene::RenderDirectionMarker(oapi::Sketchpad *pSkp, const VECTOR3 &rdir, const char *label1, const char *label2, int mode, int scale)
{
	int x, y;
	D3DXVECTOR3 homog;
	D3DXVECTOR3 dir((float)-rdir.x, (float)-rdir.y, (float)-rdir.z);

	if (D3DXVec3Dot(&dir, &Camera.z)>0) return;

	D3DXVec3TransformCoord(&homog, &dir, GetProjectionViewMatrix());

	if (homog.x >= -1.0f && homog.x <= 1.0f &&
		homog.y >= -1.0f && homog.y <= 1.0f) {

		if (_hypot (homog.x, homog.y) < 1e-6) {
			x = viewW/2;
			y = viewH/2;
		} 
		else {
			x = (int)(viewW*0.5*(1.0f+homog.x));
			y = (int)(viewH*0.5*(1.0f-homog.y));
		}

		switch (mode) {

			case 0: // box
				pSkp->Rectangle(x-scale, y-scale, x+scale+1, y+scale+1);
				break;

			case 1: // circle
				pSkp->Ellipse(x-scale, y-scale, x+scale+1, y+scale+1);
				break;
		
			case 2: // diamond
				pSkp->MoveTo(x, y-scale);
				pSkp->LineTo(x+scale, y); pSkp->LineTo(x, y+scale);
				pSkp->LineTo(x-scale, y); pSkp->LineTo(x, y-scale);
				break;

			case 3: { // delta
				int scl1 = (int)(scale*1.1547);
				pSkp->MoveTo(x, y-scale);
				pSkp->LineTo(x+scl1, y+scale); pSkp->LineTo(x-scl1, y+scale); pSkp->LineTo(x, y-scale);
			} break;

			case 4: { // nabla
				int scl1 = (int)(scale*1.1547);
				pSkp->MoveTo(x, y+scale);
				pSkp->LineTo(x+scl1, y-scale); pSkp->LineTo(x-scl1, y-scale); pSkp->LineTo(x, y+scale);
			} break;

			case 5: { // cross
				int scl1 = scale/4;
				pSkp->MoveTo(x, y-scale); pSkp->LineTo(x, y-scl1);
				pSkp->MoveTo(x, y+scale); pSkp->LineTo(x, y+scl1);
				pSkp->MoveTo(x-scale, y); pSkp->LineTo(x-scl1, y);
				pSkp->MoveTo(x+scale, y); pSkp->LineTo(x+scl1, y);
			} break;

			case 6: { // X
				int scl1 = scale/4;
				pSkp->MoveTo(x-scale, y-scale); pSkp->LineTo(x-scl1, y-scl1);
				pSkp->MoveTo(x-scale, y+scale); pSkp->LineTo(x-scl1, y+scl1);
				pSkp->MoveTo(x+scale, y-scale); pSkp->LineTo(x+scl1, y-scl1);
				pSkp->MoveTo(x+scale, y+scale); pSkp->LineTo(x+scl1, y+scl1);
			} break;	
		}
	
		if (label1) pSkp->Text(x, y-scale, label1, strlen(label1));
		if (label2) pSkp->Text(x, y+scale+labelSize[0], label2, strlen(label2));
	}
}

// ===========================================================================================
//
void Scene::RenderObjectMarker(oapi::Sketchpad *pSkp, const VECTOR3 &gpos, const char *label1, const char *label2, int mode, int scale)
{
	VECTOR3 dp (gpos - GetCameraGPos());
	normalise (dp);
	RenderDirectionMarker(pSkp, dp, label1, label2, mode, scale);
}

// ===========================================================================================
//
void Scene::NewVessel(OBJHANDLE hVessel)
{
	CheckVisual(hVessel);
}

// ===========================================================================================
//
void Scene::DeleteVessel(OBJHANDLE hVessel)
{
	VOBJREC *pv = FindVisual(hVessel);
	if (pv) DelVisualRec(pv);
}

// ===========================================================================================
//
void Scene::AddParticleStream (class D3D9ParticleStream *_pstream)
{
	
	D3D9ParticleStream **tmp = new D3D9ParticleStream*[nstream+1];
	if (nstream) {
		memcpy2 (tmp, pstream, nstream*sizeof(D3D9ParticleStream*));
		delete []pstream;
	}
	pstream = tmp;
	pstream[nstream++] = _pstream;
	
}

// ===========================================================================================
//
void Scene::DelParticleStream (DWORD idx)
{
	
	D3D9ParticleStream **tmp;
	if (nstream > 1) {
		DWORD i, j;
		tmp = new D3D9ParticleStream*[nstream-1];
		for (i = j = 0; i < nstream; i++)
			if (i != idx) tmp[j++] = pstream[i];
	} else tmp = 0;
	delete pstream[idx];
	delete []pstream;
	pstream = tmp;
	nstream--;
	
}

// ===========================================================================================
//
void Scene::InitGDIResources ()
{
	char dbgfnt[64]; sprintf_s(dbgfnt,64,"*%s",Config->DebugFont);
	labelSize[0] = 15;
	pAxisFont  = oapiCreateFont(24, false, "Arial", FONT_NORMAL, 0);
	pLabelFont = oapiCreateFont(15, false, "Arial", FONT_NORMAL, 0);
	pDebugFont = oapiCreateFont(Config->DebugFontSize, true, dbgfnt, FONT_NORMAL, 0);
	for (int i=0;i<6;i++) lblPen[i] = oapiCreatePen(1,1,labelCol[i]);
}

// ===========================================================================================
//
void Scene::ExitGDIResources ()
{
	oapiReleaseFont(pAxisFont);
	oapiReleaseFont(pLabelFont);
	oapiReleaseFont(pDebugFont);
	for (int i=0;i<6;i++) oapiReleasePen(lblPen[i]);
}

// ===========================================================================================
//
float Scene::GetDepthResolution(float dist) const 
{ 
	return fabs( (Camera.nearplane-Camera.farplane)*(dist*dist) / (Camera.farplane * Camera.nearplane * 16777215.0f) ); 
}

// ===========================================================================================
//
void Scene::PushCamera() 
{ 
	CameraStack.push(Camera); 
}

// ===========================================================================================
//
void Scene::PopCamera() 
{ 
	Camera = CameraStack.top(); 
	CameraStack.pop();
}

// ===========================================================================================
//
D3D9Pick Scene::PickScene(short xpos, short ypos)
{
	float x = 2.0f*float(xpos)/float(viewW) - 1.0f;
	float y = 2.0f*float(ypos)/float(viewH) - 1.0f;

	D3DXVECTOR3 vPick = Camera.x * (x/Camera.mProj._11) + Camera.y * (-y/Camera.mProj._22) + Camera.z;
	
	D3DXVec3Normalize(&vPick, &vPick);

	VOBJREC *pv = NULL;

	D3D9Pick result;
	result.dist  = 1e30f;
	result.pMesh = NULL;
	result.vObj  = NULL;
	result.face  = -1;
	result.group = -1;

	for (pv=vobjFirst; pv; pv=pv->next) {

		if (pv->type!=OBJTP_VESSEL) continue;
		if (!pv->vobj->IsActive()) continue;
		if (!pv->vobj->IsVisible()) continue;
		
		vVessel *vVes = (vVessel *)pv->vobj;
		double cd = vVes->CamDist();

		if (cd<5e3 && cd>1e-3) {
			D3D9Pick pick = vVes->Pick(&vPick);
			if (pick.pMesh) if (pick.dist<result.dist) result = pick;
		}	
	}

	return result;
}

// ===========================================================================================
//
void Scene::SetCameraAperture(float ap, float as)
{
	Camera.aperture = ap;
	Camera.aspect = as;

	float tanap = tan(ap);
	
	ZeroMemory(&Camera.mProj, sizeof(D3DXMATRIX));

	Camera.mProj._11 = (as / tanap);
	Camera.mProj._22 = (1.0f    / tanap);
	Camera.mProj._43 = (Camera.mProj._33 = Camera.farplane / (Camera.farplane-Camera.nearplane)) * (-Camera.nearplane);
	Camera.mProj._34 = 1.0f;
	
	float x = tanap / as;
	float y = tanap;
	float z = as / tanap;
	
	Camera.apsq = sqrt(x*x*x*z + y*y);

	Camera.vh   = tan(ap);
	Camera.vw   = Camera.vh/as;
	Camera.vhf  = 1.0f / cos(ap);
	Camera.vwf  = Camera.vhf/as;

	D3DXMatrixMultiply(&Camera.mProjView, &Camera.mView, &Camera.mProj);

	D3D9Effect::SetViewProjMatrix(&Camera.mProjView);
}

// ===========================================================================================
//
void Scene::SetCameraFrustumLimits (double nearlimit, double farlimit)
{
	Camera.nearplane = (float)nearlimit;
	Camera.farplane  = (float)farlimit;
	SetCameraAperture(Camera.aperture, Camera.aspect);
}

// ===========================================================================================
//
bool Scene::CameraPan(VECTOR3 pan, double speed)
{
	DWORD camMode = *(DWORD *)gc->GetConfigParam(CFGPRM_GETCAMERAMODE);
	OBJHANDLE hTgt = oapiCameraTarget();

	if (DebugControls::IsActive()==true && hTgt) {
		if (camMode==1) {
			VECTOR3 pos;
			oapiGetGlobalPos(hTgt, &pos);
			Camera.pos = pos + Camera.relpos;
			Camera.pos += Camera.dir * (pan.z*speed) + _VD3DX(Camera.x) * (pan.x*speed) + _VD3DX(Camera.y) * (pan.y*speed);
			Camera.relpos = Camera.pos - pos;
			return true;
		}
	}
	return false;
}


// ===========================================================================================
//
void Scene::UpdateCameraFromOrbiter(DWORD dwPass)
{
	MATRIX3 grot;
	VECTOR3 pos;

	DWORD camMode = *(DWORD *)gc->GetConfigParam(CFGPRM_GETCAMERAMODE);

	OBJHANDLE hTgt = oapiCameraTarget();

	if (hTgt) {
		if (DebugControls::IsActive()==false || camMode==0) {
			// Acquire camera information from Orbiter
			oapiGetGlobalPos(hTgt, &pos);
			oapiCameraGlobalPos(&Camera.pos);
			Camera.relpos = Camera.pos - pos;	// camera_relpos is a mesh debugger paramater
		}
		else {
			// Mesh debugger camera mode active
			oapiGetGlobalPos(hTgt, &pos);
			Camera.pos = pos + Camera.relpos; // Compute from target pos and offset
		}
	}
	else {
		// Camera target doesn't exist. (Should not happen)
		oapiCameraGlobalPos(&Camera.pos);
		Camera.relpos = _V(0,0,0);
	}

	oapiCameraGlobalDir(&Camera.dir);
	oapiCameraRotationMatrix(&grot);
	D3DXMatrixIdentity(&Camera.mView);
	D3DMAT_SetRotation(&Camera.mView, &grot);

	// note: in render space, the camera is always placed at the origin,
	// so that render coordinates are precise in the vicinity of the
	// observer (before they are translated into D3D single-precision
	// format). However, the orientation of the render space is the same
	// as orbiter's global coordinate system. Therefore there is a
	// translational transformation between orbiter global coordinates
	// and render coordinates.

	SetupInternalCamera(&Camera.mView, &Camera.pos, oapiCameraAperture(), double(viewH)/double(viewW), true, dwPass); 
}


// ===========================================================================================
//
void Scene::SetupInternalCamera(D3DXMATRIX *mNew, VECTOR3 *gpos, double apr, double asp, bool bUpdate, DWORD dwPass)
{
	dwRenderPass = dwPass;

	// Update camera orientation if a new matrix is provided
	if (mNew) {
		Camera.mView	  = *mNew;
		Camera.x   = D3DXVECTOR3(Camera.mView._11, Camera.mView._21, Camera.mView._31);
		Camera.y   = D3DXVECTOR3(Camera.mView._12, Camera.mView._22, Camera.mView._32);
		Camera.z   = D3DXVECTOR3(Camera.mView._13, Camera.mView._23, Camera.mView._33);
		Camera.dir = _VD3DX(Camera.z);
	}

	if (gpos) Camera.pos = *gpos;

	// find the planet closest to the current camera position
	Camera.hObj_proxy = oapiCameraProxyGbody();

	// find the visual
	Camera.vProxy = (vPlanet *)GetVisObject(Camera.hObj_proxy);
	
	// Camera altitude over the proxy
	VECTOR3 pos;
	oapiGetGlobalPos(Camera.hObj_proxy, &pos);
	Camera.alt_proxy = dist(Camera.pos, pos) - oapiGetSize(Camera.hObj_proxy);

	// Call SetCameraAparture to update ViewProj Matrix
	SetCameraAperture(float(apr), float(asp));

	// Finally update world matrices from all visuals if camera position vector is provided and update is requested
	// Othervice the world status remains unchanged 
	//
	if (gpos && bUpdate) for (VOBJREC *pv=vobjFirst; pv; pv=pv->next) pv->vobj->Update(dwPass==RENDERPASS_MAINSCENE);
}



// ===========================================================================================
// CUSTOM CAMERA INTERFACE
// ===========================================================================================

int Scene::DeleteCustomCamera(CAMERAHANDLE hCam)
{
	if (!hCam) return 0;

	CAMREC *pv = (CAMREC *)hCam;

	int iError = pv->iError;

	if (pv->prev) pv->prev->next = pv->next;
	else          camFirst = pv->next;

	if (pv->next) pv->next->prev = pv->prev;
	else          camLast = pv->prev;

	delete pv;
	return iError;
}

// ===========================================================================================
//
void Scene::DeleteAllCustomCameras()
{
	CAMREC *pv = camFirst;
	while (pv) {
		CAMREC *pvn = pv->next;
		delete pv;
		pv = pvn;
	}
	camFirst = camLast = camCurrent = NULL;
}

// ===========================================================================================
//
CAMERAHANDLE Scene::SetupCustomCamera(CAMERAHANDLE hCamera, OBJHANDLE hVessel, MATRIX3 &mRot, VECTOR3 &pos, double fov, SURFHANDLE hSurf, DWORD flags)
{
	CAMREC *pv = NULL;

	if (!hSurf) return NULL;
	if (Config->CustomCamMode==0) return NULL;
	if (SURFACE(hSurf)->Is3DRenderTarget()==false) return NULL;
	
	if (hCamera==NULL) {

		pv = new CAMREC;

		memset2(pv, 0, sizeof(CAMREC));

		pv->prev = camLast;
		pv->next = NULL;
		if (camLast) camLast->next = pv;
		else         camFirst = pv;
		camLast = pv;
	}
	else {
		pv = (CAMREC *)hCamera;
	}

	if (!pv) return NULL;

	pv->bActive = true;
	pv->dAperture = fov;
	pv->dwFlags = flags;
	pv->hSurface = hSurf;
	pv->mRotation = mRot;
	pv->vPosition = pos;
	pv->hVessel = hVessel;
	pv->iError = 0;

	return (CAMERAHANDLE)pv;
}

// ===========================================================================================
//
void Scene::CustomCameraOnOff(CAMERAHANDLE hCamera, bool bOn)
{
	if (!hCamera) return;
	CAMREC *pv = (CAMREC *)hCamera;
	pv->bActive = bOn;
}

// ===========================================================================================
//
void Scene::RenderCustomCameraView(CAMREC *cCur)
{
	VESSEL *pVes = oapiGetVesselInterface(cCur->hVessel);

	DWORD w = SURFACE(cCur->hSurface)->GetWidth();
	DWORD h = SURFACE(cCur->hSurface)->GetHeight();

	LPDIRECT3DSURFACE9 pORT = NULL;
	LPDIRECT3DSURFACE9 pODS = NULL;
	LPDIRECT3DSURFACE9 pSrf = SURFACE(cCur->hSurface)->GetSurface();
	LPDIRECT3DSURFACE9 pDSs = SURFACE(cCur->hSurface)->GetDepthStencil();

	if (!pSrf) cCur->iError = -1;
	if (!pDSs) cCur->iError = -2;

	if (cCur->iError!=0) return;

	MATRIX3 grot;
	VECTOR3 gpos;

	pVes->GetRotationMatrix(grot);
	pVes->Local2Global(cCur->vPosition, gpos);
	
	D3DXMATRIX mEnv, mGlo;

	D3DXMatrixIdentity(&mGlo);
	D3DMAT_SetRotation(&mGlo, &grot);
	D3DXMatrixIdentity(&mEnv);
	D3DMAT_SetRotation(&mEnv, &cCur->mRotation);
	D3DXMatrixMultiply(&mEnv, &mGlo, &mEnv);
	
	SetCameraFrustumLimits(0.1, 2e7);
	SetupInternalCamera(&mEnv, &gpos, cCur->dAperture, double(h)/double(w), true, RENDERPASS_CUSTOMCAM);
	
	ClearOmitFlags();

	HR(pDevice->GetRenderTarget(0, &pORT));
	HR(pDevice->GetDepthStencilSurface(&pODS));
    HR(pDevice->SetDepthStencilSurface(pDSs));
	HR(pDevice->SetRenderTarget(0, pSrf));

	RenderSecondaryScene(NULL, false, 0xFF);

	HR(pDevice->SetDepthStencilSurface(pODS));
	HR(pDevice->SetRenderTarget(0, pORT));

	SAFE_RELEASE(pODS);
	SAFE_RELEASE(pORT);
}


// ===========================================================================================
//
bool Scene::IsVisibleInCamera(D3DXVECTOR3 *pCnt, float radius)
{
	float z = Camera.z.x*pCnt->x + Camera.z.y*pCnt->y + Camera.z.z*pCnt->z;
	if (z<(-radius)) return false; 
	if (z<0) z=-z;
	float y = Camera.y.x*pCnt->x + Camera.y.y*pCnt->y + Camera.y.z*pCnt->z;
	if (y<0) y=-y;
	if (y-(radius*Camera.vhf) > (Camera.vh*z)) return false; 
	float x = Camera.x.x*pCnt->x + Camera.x.y*pCnt->y + Camera.x.z*pCnt->z;
	if (x<0) x=-x;
	if (x-(radius*Camera.vwf) > (Camera.vw*z)) return false;
	return true;
}

// ===========================================================================================
//
void Scene::GlobalExit()
{
	SAFE_RELEASE(FX);
}

// ===========================================================================================
//
void Scene::D3D9TechInit(LPDIRECT3DDEVICE9 pDev, const char *folder)
{
	LogMsg("Starting to initialize a SceneTech rendering technique...");

	char name[256];
	sprintf_s(name,256,"Modules/%s/SceneTech.fx", folder);

	// Create the Effect from a .fx file.
	ID3DXBuffer* errors = 0;
	
	HR(D3DXCreateEffectFromFile(pDev, name, 0, 0, 0, 0, &FX, &errors));
	
	if (errors) {
		
		// It's an error
		//
		if (strstr((char*)errors->GetBufferPointer(),"warning")==NULL) {
			LogErr("Effect Error: %s",(char*)errors->GetBufferPointer());
			MessageBoxA(0, (char*)errors->GetBufferPointer(), "SceneTech.fx Error", 0);
			return;
		}

		// It's a warning
		//
		else {
			LogErr("[Effect Warning: %s]",(char*)errors->GetBufferPointer());
			//MessageBoxA(0, (char*)errors->GetBufferPointer(), "CelSphereTech.fx Warning", 0);
		}
	}
	
	if (FX==0) {
		LogErr("Failed to create an Effect (%s)",name);
		return;
	}

	eLine  = FX->GetTechniqueByName("LineTech");
	eStar  = FX->GetTechniqueByName("StarTech");
	eWVP   = FX->GetParameterByName(0,"gWVP");
	eTex0  = FX->GetParameterByName(0,"gTex0");
	eColor = FX->GetParameterByName(0,"gColor");
	
	LogMsg("...rendering technique initialized");
}

// ===========================================================================================
//
int distcomp (const void *arg1, const void *arg2)
{
	double d1 = ((PList*)arg1)->dist;
	double d2 = ((PList*)arg2)->dist;
	return (d1 > d2 ? -1 : d1 < d2 ? 1 : 0);
}

COLORREF Scene::labelCol[6] = {0x00FFFF, 0xFFFF00, 0x4040FF, 0xFF00FF, 0x40FF40, 0xFF8080};
oapi::Pen * Scene::lblPen[6] = {0,0,0,0,0,0};