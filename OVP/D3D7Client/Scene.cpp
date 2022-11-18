// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//   ORBITER VISUALISATION PROJECT (OVP)
//   D3D7 Client module
// ==============================================================

// ==============================================================
// Scene.cpp
// Class Scene
//
// A "Scene" represents the 3-D world as seen from a specific
// viewpoint ("camera"). Each scene therefore has a camera object
// associated with it. The Orbiter core supports a single
// camera, but in principle a graphics client could define
// multiple scenes and render them simultaneously into separate
// windows (or into MFD display surfaces, etc.)
// ==============================================================

#include "Scene.h"
#include "Camera.h"
#include "VPlanet.h"
#include "VVessel.h"
#include "VBase.h"
#include "Particle.h"

using namespace oapi;

static D3DMATRIX ident = {
	1,0,0,0,
	0,1,0,0,
	0,0,1,0,
	0,0,0,1
};

D3DMATERIAL7 def_mat = {{1,1,1,1},{1,1,1,1},{1,1,1,1},{0,0,0,1},0};
const double LABEL_DISTLIMIT = 0.6;

struct PList { // auxiliary structure for object distance sorting
	vPlanet *vo;
	double dist;
};

Scene::Scene (D3D7Client *_gc, DWORD w, DWORD h)
{
	gc = _gc;
	dev = gc->GetDevice();
	viewW = w, viewH = h;
	stencilDepth = (gc->UseStencilBuffer() ? gc->GetFramework()->GetStencilBitDepth() : 0);
	zclearflag = D3DCLEAR_ZBUFFER;
	if (stencilDepth) zclearflag |= D3DCLEAR_STENCIL;
	cam = new Camera (dev, w, h);
	m_celSphere = new D3D7CelestialSphere (gc, this);
	vobjFirst = vobjLast = NULL;
	nstream = 0;
	iVCheck = 0;
	surfLabelsActive = false;
	if (!gc->clbkGetRenderParam (RP_MAXLIGHTS, &maxlight)) maxlight = 8;
	DWORD maxlight_request = *(DWORD*)gc->GetConfigParam (CFGPRM_MAXLIGHT);
	if (maxlight_request) maxlight = min (maxlight, maxlight_request);
	locallight = *(bool*)gc->GetConfigParam (CFGPRM_LOCALLIGHT);
	if (locallight)
		lightlist = new LIGHTLIST[maxlight];
	atmidx = 0;
	atmRGBA = 0;
	InitGDIResources();
}

Scene::~Scene ()
{
	while (vobjFirst) DelVisualRec (vobjFirst);
	delete cam;
	delete m_celSphere;
	delete light;
	if (nstream) {
		for (DWORD j = 0; j < nstream; j++)
			delete pstream[j];
		delete []pstream;
	}
	if (locallight) delete []lightlist;
	ExitGDIResources();
}

void Scene::Initialise ()
{
	OBJHANDLE hSun = oapiGetGbodyByIndex(0); // generalise later
	light = new D3D7Light (hSun, D3D7Light::Directional, this, 0);	

    // Set miscellaneous renderstates
	dev->SetRenderState (D3DRENDERSTATE_DITHERENABLE, TRUE);
    dev->SetRenderState (D3DRENDERSTATE_ZENABLE, TRUE);
	dev->SetRenderState (D3DRENDERSTATE_FILLMODE, *(bool*)gc->GetConfigParam (CFGPRM_WIREFRAME) ? D3DFILL_WIREFRAME : D3DFILL_SOLID);
	dev->SetRenderState (D3DRENDERSTATE_SHADEMODE, D3DSHADE_GOURAUD);
	dev->SetRenderState (D3DRENDERSTATE_SPECULARENABLE, FALSE);
    dev->SetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, FALSE);
	dev->SetRenderState (D3DRENDERSTATE_SRCBLEND, D3DBLEND_SRCALPHA);
	dev->SetRenderState (D3DRENDERSTATE_DESTBLEND, D3DBLEND_INVSRCALPHA);
	dev->SetRenderState (D3DRENDERSTATE_NORMALIZENORMALS, FALSE);
	dev->SetRenderState (D3DRENDERSTATE_ZBIAS, 0);
	dev->SetRenderState (D3DRENDERSTATE_WRAP0, 0);
	dev->SetRenderState (D3DRENDERSTATE_AMBIENT, *(DWORD*)gc->GetConfigParam (CFGPRM_AMBIENTLEVEL) * 0x01010101);

	// Set texture renderstates
    dev->SetTextureStageState (0, D3DTSS_COLORARG1, D3DTA_TEXTURE);
    dev->SetTextureStageState (0, D3DTSS_COLORARG2, D3DTA_DIFFUSE);
    dev->SetTextureStageState (0, D3DTSS_COLOROP,   D3DTOP_MODULATE);
	dev->SetTextureStageState (0, D3DTSS_MINFILTER, D3DTFN_LINEAR);
	dev->SetTextureStageState (0, D3DTSS_MAGFILTER, D3DTFG_LINEAR);
	dev->SetTextureStageState (1, D3DTSS_MINFILTER, D3DTFN_LINEAR);
	dev->SetTextureStageState (1, D3DTSS_MAGFILTER, D3DTFG_LINEAR);
}

void Scene::CheckVisual (OBJHANDLE hObj)
{
	VECTOR3 pos;
	oapiGetGlobalPos (hObj, &pos);
	double rad = oapiGetSize (hObj);
	double dst = dist (pos, *cam->GetGPos());
	double apprad = (rad*viewH)/(dst*cam->GetTanAp());
	// apparent radius of the object in units of viewport pixels

	VOBJREC *pv = FindVisual (hObj);
	if (!pv) pv = AddVisualRec (hObj);

	if (pv->vobj->IsActive()) {
		if (apprad < 1.0) pv->vobj->Activate (false);
	} else {
		if (apprad > 2.0) pv->vobj->Activate (true);
	}
#ifdef UNDEF
	if (pv) { // object has an associated visual
		if (apprad < 1.0) DelVisualRec (pv); // delete visual
	} else {  // object has not visual
		if (apprad > 2.0) AddVisualRec (hObj); // create visual
	}
#endif
	// the range check has a small hysteresis to avoid continuous
	// creation/deletion for objects at the edge of visibility
}

const D3DLIGHT7 *Scene::GetLight () const
{
	return light->GetLight();
}

void Scene::OnOptionChanged(int cat, int item)
{
	if (cat == OPTCAT_CELSPHERE)
		m_celSphere->OnOptionChanged(cat, item);
}

Scene::VOBJREC *Scene::FindVisual (OBJHANDLE hObj)
{
	VOBJREC *pv;
	for (pv = vobjFirst; pv; pv = pv->next) {
		if (pv->vobj->Object() == hObj) return pv;
	}
	return NULL;
}

void Scene::DelVisualRec (VOBJREC *pv)
{
	// unlink the entry
	if (pv->prev) pv->prev->next = pv->next;
	else          vobjFirst = pv->next;

	if (pv->next) pv->next->prev = pv->prev;
	else          vobjLast = pv->prev;

	// delete the visual, its children and the entry itself
	gc->UnregisterVisObject (pv->vobj->GetObject());
	delete pv->vobj;
	delete pv;
}

Scene::VOBJREC *Scene::AddVisualRec (OBJHANDLE hObj)
{
	// create the visual and entry
	VOBJREC *pv = new VOBJREC;
	pv->vobj = vObject::Create (hObj, this);
	gc->RegisterVisObject (hObj, (VISHANDLE)pv->vobj);

	// link entry to end of list
	pv->prev = vobjLast;
	pv->next = NULL;
	if (vobjLast) vobjLast->next = pv;
	else          vobjFirst = pv;
	vobjLast = pv;
	return pv;
}

void Scene::AddLocalLight (const LightEmitter *le, const vObject *vo, DWORD idx)
{
	D3DLIGHT7 lght;
	switch (le->GetType()) {
	case LightEmitter::LT_POINT: {
		lght.dltType = D3DLIGHT_POINT;
		lght.dvRange = (float)((PointLight*)le)->GetRange();
		const double *att = ((PointLight*)le)->GetAttenuation();
		lght.dvAttenuation0 = (float)att[0];
		lght.dvAttenuation1 = (float)att[1];
		lght.dvAttenuation2 = (float)att[2];
		} break;
	case LightEmitter::LT_SPOT: {
		lght.dltType = D3DLIGHT_SPOT;
		lght.dvRange = (float)((SpotLight*)le)->GetRange();
		const double *att = ((SpotLight*)le)->GetAttenuation();
		lght.dvAttenuation0 = (float)att[0];
		lght.dvAttenuation1 = (float)att[1];
		lght.dvAttenuation2 = (float)att[2];
		lght.dvFalloff = 1.0f;
		lght.dvTheta = (float)((SpotLight*)le)->GetUmbra();
		lght.dvPhi = (float)((SpotLight*)le)->GetPenumbra();
		} break;
	}
	double intens = le->GetIntensity();
	const COLOUR4 &col_d = le->GetDiffuseColour();
	lght.dcvDiffuse.dvR = (float)(col_d.r*intens);
	lght.dcvDiffuse.dvG = (float)(col_d.g*intens);
	lght.dcvDiffuse.dvB = (float)(col_d.b*intens);
	lght.dcvDiffuse.dvA = (float)(col_d.a*intens);
	const COLOUR4 &col_s = le->GetSpecularColour();
	lght.dcvSpecular.dvR = (float)(col_s.r*intens);
	lght.dcvSpecular.dvG = (float)(col_s.g*intens);
	lght.dcvSpecular.dvB = (float)(col_s.b*intens);
	lght.dcvSpecular.dvA = (float)(col_s.a*intens);
	const COLOUR4 &col_a = le->GetAmbientColour();
	lght.dcvAmbient.dvR = (float)(col_a.r*intens);
	lght.dcvAmbient.dvG = (float)(col_a.g*intens);
	lght.dcvAmbient.dvB = (float)(col_a.b*intens);
	lght.dcvAmbient.dvA = (float)(col_a.a*intens);
	if (lght.dltType != D3DLIGHT_DIRECTIONAL) {
		const VECTOR3 pos = le->GetPosition();
		D3DVECTOR p = { (float)pos.x, (float)pos.y, (float)pos.z }; 
		D3DMAT_VectorMatrixMultiply (&lght.dvPosition, &p, &vo->MWorld());
	}
	if (lght.dltType != D3DLIGHT_POINT) {
		MATRIX3 grot;
		oapiGetRotationMatrix (vo->Object(), &grot);
		VECTOR3 d = mul (grot, le->GetDirection());
		lght.dvDirection.dvX = (float)d.x;
		lght.dvDirection.dvY = (float)d.y;
		lght.dvDirection.dvZ = (float)d.z;
	}
	dev->SetLight (idx, &lght);
	dev->LightEnable (idx, TRUE);
}

void Scene::Update ()
{
	cam->Update (); // update camera parameters

	light->Update (); // update light sources

	// check object visibility (one object per frame in the interest
	// of scalability)
	DWORD nobj = oapiGetObjectCount();
	if (iVCheck >= nobj) iVCheck = 0;
	OBJHANDLE hObj = oapiGetObjectByIndex (iVCheck++);
	CheckVisual (hObj);

	// update all existing visuals
	for (VOBJREC *pv = vobjFirst; pv; pv = pv->next) {
		vObject *vo = pv->vobj;
		//OBJHANDLE hObj = vo->Object();
		vo->Update();
	}

	// update particle streams - should be skipped when paused
	if (!oapiGetPause()) {
		for (DWORD i = 0; i < nstream;) {
			if (pstream[i]->Expired()) DelParticleStream (i);
			else pstream[i++]->Update();
		}
	}
}

VECTOR3 Scene::SkyColour ()
{
	VECTOR3 col = {0,0,0};
	OBJHANDLE hProxy = oapiCameraProxyGbody();
	if (hProxy && oapiPlanetHasAtmosphere (hProxy)) {
		const ATMCONST *atmp = oapiGetPlanetAtmConstants (hProxy);
		VECTOR3 rc, rp, pc;
		oapiCameraGlobalPos (&rc);
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
		for (int i = 0; i < 3; i++)
			if (col.data[i] > 1.0) col.data[i] = 1.0;
	}
	return col;
}

void Scene::Render ()
{
	int i, j;
	DWORD n;
	VOBJREC *pv;

	Update (); // update camera and visuals

	VECTOR3 bgcol = SkyColour();
	double bglvl = (bgcol.x + bgcol.y + bgcol.z) / 3.0;
	atmidx = min(255, (int)(bglvl * 1.5 * 255.0));
	atmRGBA = D3DRGBA(bgcol.x, bgcol.y, bgcol.z, 1);

	// Clear the viewport
	dev->Clear (0, NULL, D3DCLEAR_TARGET|zclearflag, atmRGBA, 1.0f, 0L);

	if (FAILED (dev->BeginScene ())) return;

	light->SetLight (dev);
	int nlight = 1;
	if (locallight) {
		DWORD j, k;
		for (pv = vobjFirst; pv; pv = pv->next) {
			if (!pv->vobj->IsActive()) continue;
			OBJHANDLE hObj = pv->vobj->Object();
			if (oapiGetObjectType (hObj) == OBJTP_VESSEL) {
				VESSEL *vessel = oapiGetVesselInterface (hObj);
				DWORD nemitter = vessel->LightEmitterCount();
				for (j = 0; j < nemitter; j++) {
					const LightEmitter *em = vessel->GetLightEmitter(j);
					if (!em->IsActive() || !em->GetIntensity()) continue;
					if (oapiCameraInternal()) {
						if (em->GetVisibility() == LightEmitter::VIS_EXTERNAL)
							continue;
					}
					else {
						if (em->GetVisibility() == LightEmitter::VIS_COCKPIT)
							continue;
					}
					const VECTOR3 *pos = em->GetPositionRef();
					D3DVECTOR q, p = {(float)pos->x, (float)pos->y, (float)pos->z};
					D3DMAT_VectorMatrixMultiply (&q, &p, &pv->vobj->MWorld());
					double dst2 = q.x*q.x + q.y*q.y + q.z*q.z;
					for (k = nlight-1; k >= 1; k--) {
						if (lightlist[k].camdist2 < dst2) {
							break;
						} else if (k < maxlight-1) {
							lightlist[k+1] = lightlist[k]; // shift entries to make space
						} else
							nlight--;
					}
					if (k == maxlight-1) continue;
					lightlist[k+1].plight = em;
					lightlist[k+1].vobj = pv->vobj;
					lightlist[k+1].camdist2 = dst2;
					nlight++;
				}
			}
		}
		for (i = 1; i < nlight; i++)
			if (lightlist[i].plight->GetVisibility() & LightEmitter::VIS_EXTERNAL)
				AddLocalLight (lightlist[i].plight, lightlist[i].vobj, i);
	}

	dev->SetMaterial (&def_mat);
	dev->SetTexture (0, 0);

	// turn off z-buffer for rendering celestial sphere
	dev->SetRenderState(D3DRENDERSTATE_ZENABLE, FALSE);
	dev->SetRenderState(D3DRENDERSTATE_ZVISIBLE, FALSE);
	dev->SetRenderState(D3DRENDERSTATE_ZWRITEENABLE, FALSE);

	// stretch the z limits to make sure everything is rendered (z-fighting
	// is not an issue here because everything is rendered without z-tests)
	double npl = cam->GetNearlimit();
	double fpl = cam->GetFarlimit();
	cam->SetFrustumLimits(0.1, 10);

	// render the celestial sphere background
	m_celSphere->Render(dev, bgcol);

	cam->SetFrustumLimits(npl, fpl);

	// render solar system celestial objects (planets and moons)
	// we render without z-buffer, so need to distance-sort the objects
	int np;
	const int MAXPLANET = 512; // hard limit; should be fixed
	static PList plist[MAXPLANET];
	for (pv = vobjFirst, np = 0; pv && np < MAXPLANET; pv = pv->next) {
		if (!pv->vobj->IsActive()) continue;
		OBJHANDLE hObj = pv->vobj->Object();
		int objtp = oapiGetObjectType (hObj);
		if (objtp == OBJTP_PLANET || objtp == OBJTP_STAR) {
			plist[np].vo = (vPlanet*)pv->vobj;
			plist[np].dist = pv->vobj->CamDist();
			np++;
		}
	}
	DWORD mkrmode = *(DWORD*)gc->GetConfigParam(CFGPRM_SURFMARKERFLAG);
	int distcomp (const void *arg1, const void *arg2);
	qsort ((void*)plist, np, sizeof(PList), distcomp);
	for (i = 0; i < np; i++) {
		OBJHANDLE hObj = plist[i].vo->Object();
		plist[i].vo->Render (dev);
		if (mkrmode & MKR_ENABLE) {
			oapi::Sketchpad* pSkp = nullptr;
			oapi::Font* font = m_celSphere->MarkerFont();
			if (mkrmode & MKR_CMARK) {
				VECTOR3 pp;
				char name[256];
				oapiGetObjectName(hObj, name, 256);
				oapiGetGlobalPos(hObj, &pp);
				m_celSphere->EnsureMarkerDrawingContext(&pSkp, font, m_celSphere->MarkerColor(0), m_celSphere->MarkerPen(0));
				font = nullptr;
				RenderObjectMarker(pSkp, pp, std::string(name), std::string(), 0, viewH / 80);
			}
			if ((mkrmode & MKR_SURFMARK) && (oapiGetObjectType(hObj) == OBJTP_PLANET)) {
				font = nullptr;
				int label_format = *(int*)oapiGetObjectParam(hObj, OBJPRM_PLANET_LABELENGINE);
				if (label_format < 2 && (mkrmode & MKR_LMARK)) { // user-defined planetary surface labels
					double rad = oapiGetSize(hObj);
					double apprad = rad / (plist[i].dist * cam->GetTanAp());
					const GraphicsClient::LABELLIST* list;
					DWORD n, nlist;
					MATRIX3 prot;
					VECTOR3 ppos, cpos;
					bool bNeedSetup = true;
					nlist = gc->GetSurfaceMarkers(hObj, &list);
					for (n = 0; n < nlist; n++) {
						if (list[n].active && apprad * list[n].distfac > LABEL_DISTLIMIT) {
							if (bNeedSetup) {
								oapiGetRotationMatrix(hObj, &prot);
								oapiGetGlobalPos(hObj, &ppos);
								const VECTOR3* cp = cam->GetGPos();
								cpos = tmul(prot, *cp - ppos); // camera in local planet coords
								bNeedSetup = false;
							}
							int size = (int)(viewH / 80.0 * list[n].size + 0.5);
							int col = list[n].colour;
							m_celSphere->EnsureMarkerDrawingContext(&pSkp, font, m_celSphere->MarkerColor(col), m_celSphere->MarkerPen(col));
							font = nullptr;
							const std::vector<oapi::GraphicsClient::LABELSPEC>& ls = list[n].marker;
							VECTOR3 sp;
							for (j = 0; j < ls.size(); j++) {
								if (dotp(ls[j].pos, cpos - ls[j].pos) >= 0.0) { // surface point visible?
									sp = mul(prot, ls[j].pos) + ppos;
									RenderObjectMarker(pSkp, sp, ls[j].label[0], ls[j].label[1], list[n].shape, size);
								}
							}
						}
					}
				}
			}
			if (pSkp)
				gc->clbkReleaseSketchpad(pSkp);
		}
	}

	// render new-style surface markers
	if ((mkrmode & MKR_ENABLE) && (mkrmode & MKR_LMARK)) {
		oapi::Sketchpad *pSkp = 0;
		int fontidx = -1;
		for (i = 0; i < np; i++) {
			OBJHANDLE hObj = plist[i].vo->Object();
			if (oapiGetObjectType(hObj) != OBJTP_PLANET) continue;
			if (!surfLabelsActive)
				plist[i].vo->ActivateLabels(true);
			int label_format = *(int*)oapiGetObjectParam(hObj, OBJPRM_PLANET_LABELENGINE);
			if (label_format == 2) {
				m_celSphere->EnsureMarkerDrawingContext(&pSkp, 0, 0, m_celSphere->MarkerPen(6));
				((vPlanet*)plist[i].vo)->RenderLabels(dev, pSkp, labelFont, &fontidx);
			}
		}
		surfLabelsActive = true;
		if (pSkp)
			gc->clbkReleaseSketchpad(pSkp);
	} else {
		if (surfLabelsActive)
			surfLabelsActive = false;
	}

	// reset clipping planes and turn z-buffer back on
	dev->SetRenderState(D3DRENDERSTATE_ZENABLE, TRUE);
	dev->SetRenderState(D3DRENDERSTATE_ZVISIBLE, TRUE);
	dev->SetRenderState(D3DRENDERSTATE_ZWRITEENABLE, TRUE);

	// render the vessel objects
	//cam->SetFustrumLimits (1, 1e5);
	OBJHANDLE hFocus = oapiGetFocusObject();
	vVessel *vFocus = NULL;
	for (pv = vobjFirst; pv; pv = pv->next) {
		if (!pv->vobj->IsActive()) continue;
		OBJHANDLE hObj = pv->vobj->Object();
		if (oapiGetObjectType (hObj) == OBJTP_VESSEL) {
			pv->vobj->Render (dev);
			if (hObj == hFocus) vFocus = (vVessel*)pv->vobj; // remember focus visual
		}
	}

	if ((mkrmode & (MKR_ENABLE | MKR_VMARK)) == (MKR_ENABLE | MKR_VMARK)) {
		oapi::Sketchpad* pSkp = nullptr;
		oapi::Font* font = m_celSphere->MarkerFont();
		oapi::Pen* pen = m_celSphere->MarkerPen(0);
		COLORREF col = m_celSphere->MarkerColor(0);
		for (pv = vobjFirst; pv; pv = pv->next) {
			if (!pv->vobj->IsActive()) continue;
			OBJHANDLE hObj = pv->vobj->Object();
			VECTOR3 gpos;
			char name[256];
			oapiGetGlobalPos(hObj, &gpos);
			oapiGetObjectName(hObj, name, 256);
			m_celSphere->EnsureMarkerDrawingContext(&pSkp, font, col, pen);
			font = nullptr;
			pen = nullptr;
			col = 0;
			RenderObjectMarker(pSkp, gpos, std::string(name), std::string(), 0, viewH / 80);

		}
		if (pSkp)
			gc->clbkReleaseSketchpad(pSkp);
	}

	// render static engine exhaust
	DWORD alpha;
	dev->GetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, &alpha);
	if (!alpha) dev->SetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, TRUE);
	for (pv = vobjFirst; pv; pv = pv->next) {
		if (!pv->vobj->IsActive()) continue;
		pv->vobj->RenderBeacons (dev);
		if (oapiGetObjectType (pv->vobj->Object()) == OBJTP_VESSEL) {
			((vVessel*)(pv->vobj))->RenderExhaust (dev);
		}
	}

	// render exhaust particle system
	LPDIRECTDRAWSURFACE7 ptex = 0;
	for (n = 0; n < nstream; n++)
		pstream[n]->Render (dev, ptex);
	if (ptex) dev->SetTexture (0, 0);
	if (!alpha) dev->SetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, FALSE);

	// render object vectors
	if (*(DWORD*)gc->GetConfigParam(CFGPRM_FORCEVECTORFLAG) & BFV_ENABLE || *(DWORD*)gc->GetConfigParam(CFGPRM_FRAMEAXISFLAG) & FAV_ENABLE) {
		cam->SetFrustumLimits(1.0, 1e30);
		RenderVectors();
		cam->SetFrustumLimits(npl, fpl);
	}

	// render the internal parts of the focus object in a separate render pass
	if (oapiCameraInternal() && vFocus) {
		// switch cockpit lights on, external-only lights off
		if (locallight) {
			for (i = 1; i < nlight; i++) {
				switch (lightlist[i].plight->GetVisibility()) {
				case LightEmitter::VIS_EXTERNAL:
					dev->LightEnable (i, FALSE);
					break;
				case LightEmitter::VIS_COCKPIT:
					AddLocalLight (lightlist[i].plight, lightlist[i].vobj, i);
					break;
				}
			}
		}
		// should also check for internal meshes
		dev->Clear (0, NULL, zclearflag,  0, 1.0f, 0L); // clear z-buffer
		double nearp = cam->GetNearlimit();
		double farp  = cam->GetFarlimit ();
		cam->SetFrustumLimits (0.1, oapiGetSize (hFocus));
		vFocus->Render (dev, true);
		cam->SetFrustumLimits (nearp, farp);
	}

	if (locallight)
		for (i = 1; i < nlight; i++)
			dev->LightEnable (i, FALSE);

	// render 2D panel and HUD
	gc->Render2DOverlay();

	dev->EndScene();
}

// ==============================================================

void Scene::RenderVesselShadows (OBJHANDLE hPlanet, float depth) const
{
	// performance note: the device parameters should only be set if
	// any vessels actually want to render their shadows

	// set device parameters
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

	// render vessel shadows
	VOBJREC *pv;
	for (pv = vobjFirst; pv; pv = pv->next) {
		if (!pv->vobj->IsActive()) continue;
		if (oapiGetObjectType (pv->vobj->Object()) == OBJTP_VESSEL)
			((vVessel*)(pv->vobj))->RenderGroundShadow (dev, hPlanet);
	}

	// reset device parameters
	if (stencilDepth) {
		dev->SetRenderState (D3DRENDERSTATE_STENCILENABLE, FALSE);
	} else {
		dev->SetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, TRUE);
	}
	dev->SetTextureStageState (0, D3DTSS_ALPHAARG1, D3DTA_TEXTURE);

	// render particle shadows
	LPDIRECTDRAWSURFACE7 tex = 0;
	for (DWORD j = 0; j < nstream; j++) {
		pstream[j]->RenderGroundShadow (dev, tex);
	}

	dev->SetTextureStageState (0, D3DTSS_COLOROP, D3DTOP_MODULATE);
	dev->SetTextureStageState (0, D3DTSS_COLORARG1, D3DTA_TEXTURE);
}

// ==============================================================

void Scene::RenderObjectMarker (oapi::Sketchpad* pSkp, const VECTOR3 &gpos, const std::string& label1, const std::string& label2, int mode, int scale)
{
	VECTOR3 dp (gpos - *cam->GetGPos());
	normalise (dp);
	m_celSphere->RenderMarker(pSkp, dp, label1, label2, mode, scale);
}

// ==============================================================

void Scene::RenderVectors()
{
	dev->SetRenderState(D3DRENDERSTATE_ZENABLE, FALSE);
	dev->SetTextureStageState(0, D3DTSS_ALPHAARG1, D3DTA_TFACTOR);
	dev->SetTextureStageState(0, D3DTSS_ALPHAOP, D3DTOP_SELECTARG1);

	dev->SetTextureStageState(0, D3DTSS_COLORARG1, D3DTA_TFACTOR);
	dev->SetTextureStageState(0, D3DTSS_COLORARG2, D3DTA_DIFFUSE);
	dev->SetTextureStageState(0, D3DTSS_COLOROP, D3DTOP_MODULATE);

	dev->SetRenderState(D3DRENDERSTATE_SPECULARENABLE, TRUE);
	dev->SetTextureStageState(0, D3DTSS_MAGFILTER, D3DTFG_POINT);
	dev->SetTextureStageState(0, D3DTSS_MINFILTER, D3DTFN_POINT);
	dev->SetTexture(0, 0);
	D3DMATERIAL7 pmtrl, mtrl = { {1,1,1,1},{1,1,1,1},{1,1,1,1},{0.2,0.2,0.2,1},40 };
	dev->GetMaterial(&pmtrl);
	dev->SetMaterial(&mtrl);

	for (VOBJREC* pv = vobjFirst; pv; pv = pv->next) {
		pv->vobj->RenderVectors(dev);
	}

	dev->SetRenderState(D3DRENDERSTATE_ZENABLE, TRUE);
	dev->SetTextureStageState(0, D3DTSS_ALPHAARG1, D3DTA_TEXTURE);
	dev->SetTextureStageState(0, D3DTSS_COLORARG1, D3DTA_TEXTURE);
	dev->SetTextureStageState(0, D3DTSS_COLORARG2, D3DTA_CURRENT);
	dev->SetRenderState(D3DRENDERSTATE_SPECULARENABLE, FALSE);
	dev->SetTextureStageState(0, D3DTSS_MAGFILTER, D3DTFG_LINEAR);
	dev->SetTextureStageState(0, D3DTSS_MINFILTER, D3DTFN_LINEAR);
	dev->SetMaterial(&pmtrl);
}

// ==============================================================

void Scene::NewVessel (OBJHANDLE hVessel)
{
	CheckVisual (hVessel);
}

void Scene::DeleteVessel (OBJHANDLE hVessel)
{
	VOBJREC *pv = FindVisual (hVessel);
	if (pv) DelVisualRec (pv);
}

void Scene::AddParticleStream (D3D7ParticleStream *_pstream)
{
	D3D7ParticleStream **tmp = new D3D7ParticleStream*[nstream+1];
	if (nstream) {
		memcpy (tmp, pstream, nstream*sizeof(D3D7ParticleStream*));
		delete []pstream;
	}
	pstream = tmp;
	pstream[nstream++] = _pstream;
}

void Scene::DelParticleStream (DWORD idx)
{
	D3D7ParticleStream **tmp;
	if (nstream > 1) {
		DWORD i, j;
		tmp = new D3D7ParticleStream*[nstream-1];
		for (i = j = 0; i < nstream; i++)
			if (i != idx) tmp[j++] = pstream[i];
	} else tmp = 0;
	delete pstream[idx];
	delete []pstream;
	pstream = tmp;
	nstream--;
}

void Scene::InitGDIResources ()
{
	const int fsize[4] = {12, 16, 20, 26};
	for (int i = 0; i < 4; i++)
		labelFont[i] = gc->clbkCreateFont(fsize[i], true, "Arial", FontStyle::FONT_BOLD);
}

void Scene::ExitGDIResources ()
{
	for (int i = 0; i < 4; i++)
		gc->clbkReleaseFont(labelFont[i]);
}

int distcomp (const void *arg1, const void *arg2)
{
	double d1 = ((PList*)arg1)->dist;
	double d2 = ((PList*)arg2)->dist;
	return (d1 > d2 ? -1 : d1 < d2 ? 1 : 0);
}
