
// ==============================================================
// Scene.cpp
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Released under GNU General Public License
// Copyright (C) 2007 Martin Schweiger
//				 2012 Jarmo Nikkanen
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
	light = NULL;
	Lights = NULL;
	hSun = NULL;
	hObj_proxy = NULL;
	lightlist  = NULL;
	pAxisFont  = NULL;
	pLabelFont = NULL;
	pDebugFont = NULL;
	hCameraTarget = NULL;
	viewH = h;
	viewW = w;
	nLights = 0;
	
	pDevice = _gc->GetDevice();
	
	D3DXMatrixIdentity(&ident);
	D3DXMatrixIdentity(&mView);

	SetCameraAperture(RAD*50.0, double(viewH)/double(viewW));
	SetCameraFustrumLimits(2.5f, 5e6f); // initial limits

	csphere = new CelestialSphere(gc);

	// Create and Clear light pointers
	//
	gc->clbkGetRenderParam(RP_MAXLIGHTS, &maxlight);

	bLocalLight = *(bool*)gc->GetConfigParam(CFGPRM_LOCALLIGHT);
	if (!bLocalLight) maxlight = 0;

	Lights = new D3D9Light[12];
	memset2(Lights, 0, 12*sizeof(D3D9Light));

	if (maxlight) {
		lightlist = new LIGHTLIST[maxlight];	
		memset2(lightlist, 0, maxlight*sizeof(LIGHTLIST));
	}

	memset2(&sunLight, 0, sizeof(D3D9Light));
	
	vobjFirst = vobjLast = NULL;
	nstream = 0;
	iVCheck = 0;

	InitGDIResources();

	cspheremgr = new CSphereManager(_gc, this);
	
	LogAlw("================ Scene Created ===============");
}

// ===========================================================================================
//
Scene::~Scene ()
{
	_TRACE;

	SAFE_DELETE(csphere);
	
	if (Lights) delete []Lights;
	if (lightlist) delete []lightlist;
	if (cspheremgr) delete cspheremgr;

	// Particle Streams
	if (nstream) {
		for (DWORD j=0;j<nstream;j++) delete pstream[j];
		delete []pstream;
	}

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
	sunLight.Diffuse.r = sunLight.Specular.r = 1.0f; 
	sunLight.Diffuse.g = sunLight.Specular.g = 1.0f;
	sunLight.Diffuse.b = sunLight.Specular.b = 1.0f;
	sunLight.Diffuse.a = sunLight.Specular.a = 1.0f;
	sunLight.Ambient.r = float(ambient)*0.0039f;
	sunLight.Ambient.g = float(ambient)*0.0039f;
	sunLight.Ambient.b = float(ambient)*0.0039f;
	sunLight.Ambient.a = 1.0f;
	sunLight.Attenuation[0] = 1.0f; 
    sunLight.Param[D3D9LRange] = FLT_MAX;

	// check object visibility (one object per frame in the interest
	// of scalability)
	DWORD nobj = oapiGetObjectCount();
	for (DWORD i=0;i<nobj;i++) {
		OBJHANDLE hObj = oapiGetObjectByIndex(iVCheck++);
		CheckVisual(hObj);
	}

	gc->VisualsCreated();
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
Scene::VOBJREC *Scene::FindVisual(OBJHANDLE hObj)
{
	VOBJREC *pv;
	for (pv=vobjFirst; pv; pv=pv->next) if (pv->vobj->Object()==hObj) return pv;
	return NULL;
}

// ===========================================================================================
//
class vObject *Scene::GetVisObject(OBJHANDLE hObj)
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

	__TRY {
		pv->vobj = vObject::Create(hObj, this);
	}
	__EXCEPT(ExcHandler(GetExceptionInformation()))
	{
		char buf[64]; oapiGetObjectName(hObj, buf, 64);
		char *classname = NULL;
		int objtp = oapiGetObjectType(hObj);
		if (objtp==OBJTP_VESSEL) classname = oapiGetVesselInterface(hObj)->GetClassNameA();
		LogErr("Critical exception in Scene::AddVisualRec(0x%X) (%s)(%d)", hObj, buf, objtp);
		if (classname) LogErr("VesselClass Name = %s",classname);
		gc->EmergencyShutdown();
		FatalAppExitA(0,"Critical error has occured. See Orbiter.log for details");
	}

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

	// Parse Vessel Skins
	if (pv->type==OBJTP_VESSEL) ((vVessel *)pv->vobj)->ParseSkins();

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
void Scene::AddLocalLight(const LightEmitter *le, const vObject *vo, DWORD idx)
{
	if (Lights==NULL) return;

	D3D9Light lght;

	switch (le->GetType()) {
	
		case LightEmitter::LT_POINT: {
			lght.Type = D3DLIGHT_POINT;
			lght.Param[D3D9LRange] = (float)((PointLight*)le)->GetRange();
			const double *att = ((PointLight*)le)->GetAttenuation();
			lght.Attenuation[0] = (float)att[0];
			lght.Attenuation[1] = (float)att[1];
			lght.Attenuation[2] = (float)att[2];
		} break;

		case LightEmitter::LT_SPOT: {
			lght.Type = D3DLIGHT_SPOT;
			lght.Param[D3D9LRange] = (float)((SpotLight*)le)->GetRange();
			const double *att = ((SpotLight*)le)->GetAttenuation();
			lght.Attenuation[0] = (float)att[0];
			lght.Attenuation[1] = (float)att[1];
			lght.Attenuation[2] = (float)att[2];
			lght.Param[D3D9LFalloff] = 1.0f;
			lght.Param[D3D9LTheta]   = (float)((SpotLight*)le)->GetUmbra();
			lght.Param[D3D9LPhi]     = (float)((SpotLight*)le)->GetPenumbra();
			
			// We'll do this here to avoid per pixel cosine computation and division
			lght.Param[D3D9LPhi]   = cos(lght.Param[D3D9LPhi]*0.5f);
			lght.Param[D3D9LTheta] = 1.0f/(cos(lght.Param[D3D9LTheta]*0.5f)-lght.Param[D3D9LPhi]);
		} break;
	}

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

	if (lght.Type != D3DLIGHT_DIRECTIONAL) {
		const VECTOR3 pos = le->GetPosition();
		D3DXVECTOR3 p((float)pos.x, (float)pos.y, (float)pos.z); 
		D3DXVec3TransformCoord((D3DXVECTOR3 *)&lght.Position, &p, vo->MWorld());	
	}
	
	if (lght.Type != D3DLIGHT_POINT) {
		const VECTOR3 dir = le->GetDirection();
		D3DXVECTOR3 d((float)dir.x, (float)dir.y, (float)dir.z); 
		D3DXVec3TransformNormal((D3DXVECTOR3 *)&lght.Direction, &d, vo->MWorld());
		D3DXVec3Normalize((D3DXVECTOR3 *)&lght.Direction, (D3DXVECTOR3 *)&lght.Direction);
	}

	if (idx>=maxlight) return;

	if (lght.Attenuation[0]==0.0 && lght.Attenuation[1]==0.0 && lght.Attenuation[2]==0.0) LogErr("LightEmitter zero attennuation");

	if (lght.Ambient.r<0) LogErr("LightEmiter invalid color 1");
	if (lght.Ambient.g<0) LogErr("LightEmiter invalid color 2");
	if (lght.Ambient.b<0) LogErr("LightEmiter invalid color 3");
	if (lght.Ambient.a<0) LogErr("LightEmiter invalid color 4");
	if (lght.Specular.r<0) LogErr("LightEmiter invalid color 5");
	if (lght.Specular.g<0) LogErr("LightEmiter invalid color 6");
	if (lght.Specular.b<0) LogErr("LightEmiter invalid color 7");
	if (lght.Diffuse.r<0) LogErr("LightEmiter invalid color 8");
	if (lght.Diffuse.g<0) LogErr("LightEmiter invalid color 9");
	if (lght.Diffuse.b<0) LogErr("LightEmiter invalid color 10");
	if (lght.Diffuse.a<0) LogErr("LightEmiter invalid color 11");

	Lights[idx] = lght;
	nLights = idx+1;
}

// ===========================================================================================
//
void Scene::Update ()
{
	_TRACE;

	if (hSun==NULL) {
		LogErr("Scene::Update() Scene not yet initialized");
		//Initialise();
		return;
	}
	
	// update particle streams - should be skipped when paused
	if (!oapiGetPause()) {
		for (DWORD i=0;i<nstream;) {
			if (pstream[i]->Expired()) DelParticleStream(i);
			else pstream[i++]->Update();
		}
	}

	// check object visibility (one object per frame in the interest
	// of scalability)
	DWORD nobj = oapiGetObjectCount();

	if (iVCheck >= nobj) {
		iVCheck = 0;
	}

	// This function will browse through vessels and planets. (not bases)
	// Base visuals don't exist in the visual record. 
	OBJHANDLE hObj = oapiGetObjectByIndex(iVCheck++);
	CheckVisual(hObj);


	// If Camera target has changed, setup mesh debugger
	//
	OBJHANDLE hTgt = oapiCameraTarget();

	if (hTgt!=hCameraTarget && hTgt!=NULL) {
		
		hCameraTarget = hTgt;

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

			// Why is this here ??????????

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
	
	if (hObj) {
		VECTOR3 pos;
		oapiGetGlobalPos(hObj,&pos);
		double g = atan(apsq);
		double t = dotp(unit(camera_pos-pos), unit(camera_dir));
		if (t<-1.0) t=1.0; if (t>1.0) t=1.0f;
		double a = PI - acos(t);
		double R = oapiGetSize(hObj_proxy);
		double r = length(camera_pos-pos);
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
	
	UpdateCameraFromOrbiter(); // update camera parameters

	if (hObj_proxy) D3D9Effect::UpdateEffectCamera(hObj_proxy);
	
	// Clear active local lisghts list -------------------------------
	//
	memset2(Lights, 0, 12*sizeof(D3D9Light));

	// Update Sunlight direction -------------------------------------
	// 
	VECTOR3 rpos;
	oapiGetGlobalPos(hSun, &rpos);
	rpos -= camera_pos; 
	D3DVEC(-unit(rpos), sunLight.Direction);
	D3DVEC(rpos, sunLight.Position);


	// update all existing visuals, get focus visual -------------------
	// 
	OBJHANDLE hFocus = oapiGetFocusObject();
	vFocus = NULL;
	for (VOBJREC *pv=vobjFirst; pv; pv=pv->next) {
		if (pv->type==OBJTP_VESSEL) if (pv->vobj->Object()==hFocus) vFocus = (vVessel *)pv->vobj;
		pv->vobj->Update();
	}

   
	// Compute SkyColor -----------------------------------------------
	//
	sky_color = SkyColour();
	bg_rgba = D3DCOLOR_RGBA ((int)(sky_color.x*255), (int)(sky_color.y*255), (int)(sky_color.z*255), 255);


	// Process Local Light Sources -------------------------------------
	//
	int nlight = 1;
	int nEmitter = 0;
	nLights = 0;

	if (bLocalLight && lightlist) {
		DWORD j, k;
		VOBJREC *pv = NULL;
		for (pv = vobjFirst; pv; pv = pv->next) {
			if (!pv->vobj->IsActive()) continue;
			OBJHANDLE hObj = pv->vobj->Object();
			if (oapiGetObjectType (hObj) == OBJTP_VESSEL) {
				VESSEL *vessel = oapiGetVesselInterface (hObj);
				DWORD nemitter = vessel->LightEmitterCount();
				for (j = 0; j < nemitter; j++) {
					const LightEmitter *em = vessel->GetLightEmitter(j);
					if (em->IsActive()==false || em->GetIntensity()==0.0) continue;
					nEmitter++;
					const VECTOR3 *pos = em->GetPositionRef();
					D3DXVECTOR3 q, p = D3DXVECTOR3((float)pos->x, (float)pos->y, (float)pos->z);
					D3DXVec3TransformCoord(&q, &p, pv->vobj->MWorld());
					double dst2 = q.x*q.x + q.y*q.y + q.z*q.z;
					for (k = nlight-1; k >= 1; k--) {
						if (lightlist[k].camdist2 < dst2) break;
						else if (k < maxlight-1) lightlist[k+1] = lightlist[k]; // shift entries to make space
						else nlight--;
					}
					if (k == maxlight-1) continue;
					lightlist[k+1].plight = em;
					lightlist[k+1].vobj = pv->vobj;
					lightlist[k+1].camdist2 = dst2;
					nlight++;
				}
			}
		}

		for (int i=1;i<nlight;i++) AddLocalLight(lightlist[i].plight, lightlist[i].vobj, i-1);

		sprintf_s(oapiDebugString(),256,"Lights = %u", nLights);
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
void Scene::RenderMainScene()
{
	_TRACE;

/*
#ifdef _NVAPI_H
	if (pStereoHandle) {
		if (oapiCameraInternal()) NvAPI_Stereo_SetConvergence(pStereoHandle, 0.2f);
		else				      NvAPI_Stereo_SetConvergence(pStereoHandle, 0.6f);
		NvAPI_Stereo_SetSeparation(pStereoHandle, 70.0f);
	}
#endif
*/	
	if (vFocus==NULL) return;

	double scene_time = D3D9GetTime();

	if (DebugControls::IsActive()) {
		DWORD flags  = *(DWORD*)gc->GetConfigParam(CFGPRM_GETDEBUGFLAGS);
		if (flags&DBG_FLAGS_WIREFRAME) pDevice->SetRenderState(D3DRS_FILLMODE, D3DFILL_WIREFRAME);
		else						   pDevice->SetRenderState(D3DRS_FILLMODE, D3DFILL_SOLID);
	}
	
	// Clear the viewport
	HR(pDevice->Clear(0, NULL, D3DCLEAR_TARGET|D3DCLEAR_ZBUFFER|D3DCLEAR_STENCIL, bg_rgba, 1.0f, 0L));
	
	if (FAILED (pDevice->BeginScene())) return;

	float znear = ComputeNearClipPlane();
	
	if (DebugControls::IsActive()) {
		DWORD camMode = *(DWORD*)gc->GetConfigParam(CFGPRM_GETCAMERAMODE);
		if (camMode!=0) znear = 0.1f;
	}

	SetCameraFustrumLimits(znear, 2e7f);

	// -------------------------------------------------------------------------------------------------------
	// render celestial sphere background
	// -------------------------------------------------------------------------------------------------------

	pDevice->SetRenderState(D3DRS_CULLMODE, D3DCULL_CW);

	int bglvl = 0;
	if (bg_rgba) { // suppress stars darker than the background
		bglvl = (bg_rgba & 0xff) + ((bg_rgba >> 8) & 0xff) + ((bg_rgba >> 16) & 0xff);
		bglvl = min (bglvl/2, 255);
	}
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
		if (plnmode & PLN_CCMARK) {
			const GraphicsClient::LABELLIST *list;
			DWORD n, nlist;
			HDC hDC = NULL;
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
		// cam->SetFustrumLimits (nplane, fplane);
		// since we are not using z-buffers here, we can adjust the projection
		// matrix at will to make sure the object is within the viewing fustrum

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

	D3D9Effect::UpdateEffectCamera(hObj_proxy);

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



	// render the internal parts of the focus object in a separate render pass
	//
	if (oapiCameraInternal() && vFocus) {
		pDevice->Clear(0, NULL, D3DCLEAR_ZBUFFER,  0, 1.0f, 0L); // clear z-buffer
		double znear = Config->VCNearPlane;
		if (znear<0.01) znear=0.01;
		if (znear>1.0)  znear=1.0;
		OBJHANDLE hFocus = oapiGetFocusObject();
		SetCameraFustrumLimits(znear, oapiGetSize(hFocus));
		vFocus->Render(pDevice, true);
	}

	pDevice->SetRenderState(D3DRS_FILLMODE, D3DFILL_SOLID);
	
	gc->Render2DOverlay();

	int len = strlen(oapiDebugString());

	if (len>0) { 

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
	}
		
	pSketch->SetFont(NULL);
	pSketch->SetPen(NULL);

	oapiReleaseSketchpad(pSketch);

	pDevice->EndScene();


	// Render Environmental Map For the Focus Vessel ----------------------------
	//
	if (Config->EnvMapMode) {
		DWORD flags = 0;
		if (Config->EnvMapMode==1) flags |= 0x01; 
		if (Config->EnvMapMode==2) flags |= 0x03;

		if (vobjEnv==NULL) vobjEnv = vobjFirst;

		while (vobjEnv) {	
			if (vobjEnv->type==OBJTP_VESSEL && vobjEnv->apprad>8.0f) {
				if (vobjEnv->vobj) {
					vVessel *vVes = (vVessel *)vobjEnv->vobj;
					if (vVes->RenderENVMap(pDevice, Config->EnvMapFaces, flags)) {
						vobjEnv = vobjEnv->next;
						break;
					}	
				}
			}
			vobjEnv = vobjEnv->next;
		}
	}

	// EnvMap Debugger ----------------------------------------------------------
	// TODO: Should be allowed to visualize other maps as well, not just index 0
	//
	if (DebugControls::IsActive()) {
		DWORD flags  = *(DWORD*)gc->GetConfigParam(CFGPRM_GETDEBUGFLAGS);
		if (flags&DBG_FLAGS_DSPENVMAP) VisualizeCubeMap(vFocus->GetEnvMap(0)); //VisualizeCubeMap(vFocus->GetEnvMap(-1));
	}

	scene_time = D3D9GetTime() - scene_time;

	if (scene_time>gc->GetStats()->ScenePeak) gc->GetStats()->ScenePeak = scene_time;
	gc->GetStats()->Scene += scene_time;
}


// ===========================================================================================
//
void Scene::RenderSecondaryScene(vObject *omit, bool bOmitAtc, DWORD flags)
{
	_TRACE;

	D3D9Effect::UpdateEffectCamera(GetCameraProxyBody());

	// Clear the viewport
	HR(pDevice->Clear(0, NULL, D3DCLEAR_TARGET|D3DCLEAR_ZBUFFER|D3DCLEAR_STENCIL, bg_rgba, 1.0f, 0L));

	HR(pDevice->BeginScene());

	VOBJREC *pv = NULL;

	// render planets -------------------------------------------
	//
	if (flags&0x01) {
		for (DWORD i=0;i<nplanets;i++) {
			OBJHANDLE hObj = plist[i].vo->Object();
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

	if (D3DXVec3Dot(&dir,&camera_z)>0) return;

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
D3D9Pick Scene::PickScene(short xpos, short ypos)
{
	float x = 2.0f*float(xpos)/float(viewW) - 1.0f;
	float y = 2.0f*float(ypos)/float(viewH) - 1.0f;

	D3DXVECTOR3 vPick = camera_x * (x/mProj._11) + camera_y * (-y/mProj._22) + camera_z;
	
	D3DXVec3Normalize(&vPick, &vPick);

	VOBJREC *pv = NULL;

	D3D9Pick result;
	result.dist  = 1e10;
	result.pMesh = NULL;
	result.vObj  = NULL;

	for (pv=vobjFirst; pv; pv=pv->next) {
		if (pv->type==OBJTP_VESSEL) {
			vVessel *vVes = (vVessel *)pv->vobj;
			if (vVes) {
				D3D9Pick pick = vVes->Pick(&vPick);
				if (pick.dist<result.dist) result = pick;
			}
		}
	}

	return result;
}

// ===========================================================================================
//
void Scene::SetCameraAperture(double _ap, double _as)
{
	aperture = (float)_ap;
	aspect = (float)_as;

	float tanap = tan(aperture);
	
	ZeroMemory(&mProj, sizeof(D3DXMATRIX));
	mProj._11 = (aspect / tanap);
	mProj._22 = (1.0f    / tanap);
	mProj._43 = (mProj._33 = farplane / (farplane-nearplane)) * (-nearplane);
	mProj._34 = 1.0f;
	
	float x = tanap / aspect;
	float y = tanap;
	float z = aspect / tanap;
	
	apsq = sqrt(x*x*x*z + y*y);

	vh   = tan(aperture);
	vw   = vh/aspect;
	vhf  = (1.0f/cos(aperture));
	vwf  = vhf/aspect;

	D3DXMatrixMultiply(&mProjView, &mView, &mProj);

	D3D9Effect::SetViewProjMatrix(&mProjView);
}

// ===========================================================================================
//
void Scene::SetCameraFustrumLimits (double nearlimit, double farlimit)
{
	nearplane = (float)nearlimit;
	farplane = (float)farlimit;
	SetCameraAperture(aperture, aspect);
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
			camera_pos = pos + camera_relpos;
			camera_pos += camera_dir * (pan.z*speed) + _VD3DX(camera_x) * (pan.x*speed) + _VD3DX(camera_y) * (pan.y*speed);
			camera_relpos = camera_pos - pos;
			return true;
		}
	}
	return false;
}


// ===========================================================================================
//
void Scene::UpdateCameraFromOrbiter()
{
	MATRIX3 grot;
	VECTOR3 pos;

	DWORD camMode = *(DWORD *)gc->GetConfigParam(CFGPRM_GETCAMERAMODE);
	OBJHANDLE hTgt = oapiCameraTarget();

	if ((DebugControls::IsActive()==false || camMode==0) && hTgt) {
		oapiGetGlobalPos(hTgt, &pos);
		oapiCameraGlobalPos(&camera_pos);
		camera_relpos = camera_pos - pos;	// camera_relpos is a mesh debugger paramater
	}
	else if (hTgt) {
		oapiGetGlobalPos(hTgt, &pos);
		camera_pos = pos + camera_relpos;
	}
	else {
		oapiCameraGlobalPos(&camera_pos);
		camera_relpos = _V(0,0,0);
	}

	oapiCameraGlobalDir(&camera_dir);
	oapiCameraRotationMatrix(&grot);

	D3DXMatrixIdentity(&mView);
	D3DMAT_SetRotation(&mView, &grot);

	camera_x  = D3DXVECTOR3(mView._11, mView._21, mView._31);
	camera_y  = D3DXVECTOR3(mView._12, mView._22, mView._32);
	camera_z  = D3DXVECTOR3(mView._13, mView._23, mView._33);
	
	// note: in render space, the camera is always placed at the origin,
	// so that render coordinates are precise in the vicinity of the
	// observer (before they are translated into D3D single-precision
	// format). However, the orientation of the render space is the same
	// as orbiter's global coordinate system. Therefore there is a
	// translational transformation between orbiter global coordinates
	// and render coordinates.

	// find the planet closest to the current camera position
	hObj_proxy = oapiCameraProxyGbody();
	oapiGetGlobalPos(hObj_proxy, &pos);
	alt_proxy  = dist(camera_pos, pos) - oapiGetSize(hObj_proxy);

	camera_offset = D3DXVECTOR3(0,0,0);
	bCustomCam = false;

	// Call SetCameraAparture to update ViewProj Matrix
	//
	SetCameraAperture(oapiCameraAperture(), double(viewH)/double(viewW));
}

void Scene::SetupCustomCamera(D3DXMATRIX mNew, VECTOR3 disp, double apr, double asp)
{
	mView = mNew;

	camera_x   = D3DXVECTOR3(mView._11, mView._21, mView._31);
	camera_y   = D3DXVECTOR3(mView._12, mView._22, mView._32);
	camera_z   = D3DXVECTOR3(mView._13, mView._23, mView._33);
	camera_dir = _VD3DX(camera_z);

	oapiCameraGlobalPos(&camera_pos);

	camera_pos += disp;
	camera_offset = D3DXVEC(disp);

	bCustomCam = true;

	// Call SetCameraAparture to update ViewProj Matrix
	//
	SetCameraAperture(apr, asp);
}


// ===========================================================================================
//
bool Scene::IsVisibleInCamera(D3DXVECTOR3 *pCnt, float radius)
{
	if (bCustomCam) *pCnt -= camera_offset;
	float z = camera_z.x*pCnt->x + camera_z.y*pCnt->y + camera_z.z*pCnt->z;
	if (z<(-radius)) return false; 
	if (z<0) z=-z;
	float y = camera_y.x*pCnt->x + camera_y.y*pCnt->y + camera_y.z*pCnt->z;
	if (y<0) y=-y;
	if (y-(radius*vhf) > (vh*z)) return false; 
	float x = camera_x.x*pCnt->x + camera_x.y*pCnt->y + camera_x.z*pCnt->z;
	if (x<0) x=-x;
	if (x-(radius*vwf) > (vw*z)) return false;
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