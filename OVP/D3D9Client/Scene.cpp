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
#include "D3D9Util.h"
#include "D3D9Config.h"
#include "D3D9Surface.h"
#include "D3D9TextMgr.h"
#include "D3D9Catalog.h"
#include "AABBUtil.h"
#include "OapiExtension.h"
#include "DebugControls.h"
#include "IProcess.h"
#include "VectorHelpers.h"
#include <sstream>
#include <vector>

#define IKernelSize 150

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


D3DXVECTOR4 IKernel[IKernelSize];

bool sort_vessels(const vVessel *a, const vVessel *b)
{
	return a->CameraTgtDist() < b->CameraTgtDist();
}

float Rand()
{
	return float(rand()) / 32768.0f;
}

// ===========================================================================================
//
Scene::Scene(D3D9Client *_gc, DWORD w, DWORD h)
{
	_TRACE;

	gc = _gc;
	vobjEnv = NULL;
	vobjIrd = NULL;
	m_celSphere = NULL;
	Lights = NULL;
	hSun = NULL;
	pAxisFont  = NULL;
	pLabelFont = NULL;
	pDebugFont = NULL;
	pBlur = NULL;
	pOffscreenTarget = NULL;
	pLocalCompute = NULL;
	pRenderGlares = NULL;
	pCreateGlare = NULL;
	viewH = h;
	viewW = w;
	nLights = 0;
	dwTurn = 0;
	dwFrameId = 0;
	surfLabelsActive = false;

	pSunTex = NULL;
	pLightGlare = NULL;
	pSunGlare = NULL;
	pSunGlareAtm = NULL;
	pEnvDS = NULL;
	pIrradDS = NULL;
	pIrradiance = NULL;
	pIrradTemp = NULL;
	pIrradTemp2 = NULL;
	pIrradTemp3 = NULL;
	pDepthNormalDS = NULL;
	pVisDepth = NULL;
	pLocalResults = NULL;
	pLocalResultsSL = NULL;

	fDisplayScale = float(viewH) / 1080.0f;

	for (auto& a : DepthSampleKernel) a = FVECTOR2(0, 0);

	memset(&psShmDS, 0, sizeof(psShmDS));
	memset(&ptShmRT, 0, sizeof(ptShmRT));
	memset(&psShmRT, 0, sizeof(psShmRT));


	pDevice = _gc->GetDevice();

	memset(&Camera, 0, sizeof(Camera));

	D3DXMatrixIdentity(&ident);

	SetCameraAperture(float(RAD*50.0), float(viewH)/float(viewW));
	SetCameraFrustumLimits(2.5f, 5e6f); // initial limits

	m_celSphere = new D3D9CelestialSphere(gc, this);
	Lights = new D3D9Light[MAX_SCENE_LIGHTS];

	bLocalLight = *(bool*)gc->GetConfigParam(CFGPRM_LOCALLIGHT);
	
	memset(&sunLight, 0, sizeof(D3D9Sun));
	memset(&smap, 0, sizeof(smap));

	CLEARARRAY(pBlrTemp);
	CLEARARRAY(pTextures);
	CLEARARRAY(ptgBuffer);
	CLEARARRAY(psgBuffer);

	vobjFirst = vobjLast = NULL;
	nstream = 0;
	iVCheck = 0;

	InitGDIResources();

	while (true) {
		float dx = 0;
		float dy = 0;
		for (int i = 0; i < IKernelSize; i++) {
			double r = oapiRand();
			double a = oapiRand() * PI2;
			IKernel[i].x = float(cos(a) * r);
			IKernel[i].y = float(sin(a) * r);
			dx += IKernel[i].x;
			dy += IKernel[i].y;
		}
		if ((abs(dx) < 1.0) && (abs(dy) < 1.0)) break;
	}

	for (int i = 0; i < IKernelSize; i++) {
		float d = IKernel[i].x*IKernel[i].x + IKernel[i].y*IKernel[i].y;
		IKernel[i].z = sqrt(1.0f - saturate(d));
		IKernel[i].w = sqrt(IKernel[i].z);
	}



	// ------------------------------------------------------------------------------
	// Read Sun glare sampling kernel file

	ifstream fs("Modules/D3D9Client/GKernel.txt");
	if (fs.good()) {
		string line; vector<FVECTOR2> data;
		while (getline(fs, line)) {
			std::istringstream iss(line);
			char c;	float a, b;	iss >> a >> c >> b;
			data.push_back(FVECTOR2(a, b));
		}
		if (data.size() != ARRAYSIZE(DepthSampleKernel)) LogErr("Modules/D3D9Client/GKernel.txt Size missmatch. Expecting 57 entries");
		else for (int i = 0; i < ARRAYSIZE(DepthSampleKernel); i++) DepthSampleKernel[i] = data[i];
		data.clear();
	} else LogErr("Failed to read: Modules/D3D9Client/GKernel.txt");
	fs.close();

	CreateSunGlare();


	// ------------------------------------------------------------------------------
	// Initialize a shaders for local lights visibility checks and rendering 

	if (Config->bGlares || Config->bLocalGlares)
	{
		pRenderGlares = new ShaderClass(pDevice, "Modules/D3D9Client/Glare.hlsl", "GlareVS", "GlarePS", "RenderGlares", "");
		pLocalCompute = new ShaderClass(pDevice, "Modules/D3D9Client/Glare.hlsl", "VisibilityVS", "VisibilityPS", "LocalVisCheck", "");
		D3DXCreateTexture(pDevice, 32, 1, 1, D3DUSAGE_RENDERTARGET, D3DFMT_R16F, D3DPOOL_DEFAULT, &pLocalResults);
		HR(pLocalResults->GetSurfaceLevel(0, &pLocalResultsSL));
	}


	// Render screen depth and screen space normals
	//

	if (Config->bGlares || Config->bLocalGlares) {
		pVisDepth = new ImageProcessing(pDevice, "Modules/D3D9Client/LightBlur.hlsl", "PSDepth", NULL);
		pVisDepth->CompileShader("PSNormal");
	}

	// Initialize envmapping and shadow maps -----------------------------------------------------------------------------------------------
	//
	DWORD EnvMapSize = Config->EnvMapSize;
	DWORD ShmMapSize = Config->ShadowMapSize;

	if (Config->EnvMapMode) {
		HR(pDevice->CreateDepthStencilSurface(EnvMapSize, EnvMapSize, D3DFMT_D24S8, D3DMULTISAMPLE_NONE, 0, true, &pEnvDS, NULL));
	}

	if (Config->bIrradiance) {
		HR(pDevice->CreateDepthStencilSurface(128, 128, D3DFMT_D24S8, D3DMULTISAMPLE_NONE, 0, true, &pIrradDS, NULL));
	}


	if (Config->ShadowMapMode) {
		UINT size = ShmMapSize;
		for (int i = 0; i < SHM_LOD_COUNT; i++) {
			HR(pDevice->CreateDepthStencilSurface(size, size, D3DFMT_D24X8, D3DMULTISAMPLE_NONE, 0, true, &psShmDS[i], NULL));
			HR(pDevice->CreateTexture(size, size, 1, D3DUSAGE_RENDERTARGET, D3DFMT_R32F, D3DPOOL_DEFAULT, &ptShmRT[i], NULL));
			HR(ptShmRT[i]->GetSurfaceLevel(0, &psShmRT[i]));
			size >>= 1;
		}

		smap.pShadowMap = ptShmRT[0];
	}



	// Create auxiliary color buffer for on screen GDI
	//
	if (Config->GDIOverlay) {
		HR(D3DXCreateTexture(pDevice, viewW, viewH, 1, D3DUSAGE_DYNAMIC, D3DFMT_X8R8G8B8, D3DPOOL_DEFAULT, &ptgBuffer[GBUF_GDI]));
		pGDIOverlay = new ImageProcessing(pDevice, "Modules/D3D9Client/GDIOverlay.hlsl", "PSMain");
	}
	else pGDIOverlay = NULL;


	// Create an auxiliary screen space normal and depth buffer (i.e. Shader readable depth buffer)
	//
	if (Config->bGlares || Config->bLocalGlares) {
		HR(pDevice->CreateDepthStencilSurface(viewW, viewH, D3DFMT_D24S8, D3DMULTISAMPLE_NONE, 0, true, &pDepthNormalDS, NULL));
		HR(D3DXCreateTexture(pDevice, viewW, viewH, 1, D3DUSAGE_RENDERTARGET, D3DFMT_A16B16G16R16F, D3DPOOL_DEFAULT, &ptgBuffer[GBUF_DEPTH]));
	}


	// Initialize post processing effects --------------------------------------------------------------------------------------------------
	//
	pLightBlur = NULL;

	if (Config->PostProcess) {

		int BufSize = 1;
		int BufFmt = 0;

		// Get the actual back buffer description
		D3DSURFACE_DESC desc;
		gc->GetBackBuffer()->GetDesc(&desc);

		char flags[32] = { 0 };
		if (Config->ShaderDebug) strcpy_s(flags, 32, "DISASM");

		// Load postprocessing effects
		if (Config->PostProcess == PP_DEFAULT)
			pLightBlur = new ImageProcessing(pDevice, "Modules/D3D9Client/LightBlur.hlsl", "PSMain", flags);

		if (pLightBlur) {
			BufSize = pLightBlur->FindDefine("BufferDivider");
			BufFmt = pLightBlur->FindDefine("BufferFormat");
		}

		D3DFORMAT BackBuffer = desc.Format;
		if (BufFmt == 1) BackBuffer = D3DFMT_A16B16G16R16F;
		if (BufFmt == 2) BackBuffer = D3DFMT_A2R10G10B10;

		// Create auxiliary color buffer for color operations
		HR(D3DXCreateTexture(pDevice, viewW, viewH, 1, D3DUSAGE_RENDERTARGET, BackBuffer, D3DPOOL_DEFAULT, &ptgBuffer[GBUF_COLOR]));

		// Load some textures
		char buff[MAX_PATH];
		if (gc->TexturePath("D3D9Noise.dds", buff)) HR(D3DXCreateTextureFromFileA(pDevice, buff, &pTextures[TEX_NOISE]));
		if (gc->TexturePath("D3D9CLUT.dds", buff)) HR(D3DXCreateTextureFromFileA(pDevice, buff, &pTextures[TEX_CLUT]));

		if (pLightBlur) {
			HR(D3DXCreateTexture(pDevice, viewW / BufSize, viewH / BufSize, 1, D3DUSAGE_RENDERTARGET, BackBuffer, D3DPOOL_DEFAULT, &ptgBuffer[GBUF_BLUR]));
			HR(D3DXCreateTexture(pDevice, viewW / BufSize, viewH / BufSize, 1, D3DUSAGE_RENDERTARGET, BackBuffer, D3DPOOL_DEFAULT, &ptgBuffer[GBUF_TEMP]));
		}

		if (pLightBlur) {
			// Construct an offscreen backbuffer with custom pixel format
			if (pDevice->CreateRenderTarget(viewW, viewH, BackBuffer, desc.MultiSampleType, desc.MultiSampleQuality, false, &pOffscreenTarget, NULL) != S_OK) {
				LogErr("Creation of Offscreen render target failed");
				SAFE_DELETE(pLightBlur);
			}
		}
	}

	for (int i = 0; i < ARRAYSIZE(ptgBuffer);i++)  if (ptgBuffer[i]) ptgBuffer[i]->GetSurfaceLevel(0, &psgBuffer[i]);


	if (Config->GDIOverlay) {
		HDC hDC;
		// Clear the GDI Overlay with transparency
		if (psgBuffer[GBUF_GDI]->GetDC(&hDC) == S_OK) {
			DWORD color = 0xF08040; // BGR "Color Key" value for transparency
			HBRUSH hBrush = CreateSolidBrush((COLORREF)color);
			RECT r = _RECT( 0, 0, viewW, viewH );
			FillRect(hDC, &r, hBrush);
			DeleteObject(hBrush);
			psgBuffer[GBUF_GDI]->ReleaseDC(hDC);
		}
	}

	LogAlw("================ Scene Created ===============");
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

	for (int i = 0; i < ARRAYSIZE(psgBuffer); i++) SAFE_RELEASE(psgBuffer[i]);
	for (int i = 0; i < ARRAYSIZE(ptgBuffer); i++) SAFE_RELEASE(ptgBuffer[i]);
	for (int i = 0; i < ARRAYSIZE(pTextures); i++) SAFE_RELEASE(pTextures[i]);

	SAFE_DELETE(pGDIOverlay);
	SAFE_DELETE(pBlur);
	SAFE_DELETE(pVisDepth);
	SAFE_DELETE(pLightBlur);
	SAFE_DELETE(pIrradiance);
	SAFE_DELETE(m_celSphere);
	SAFE_DELETE(pLocalCompute);
	SAFE_DELETE(pRenderGlares);
	SAFE_DELETE(pCreateGlare);

	SAFE_RELEASE(pOffscreenTarget);
	SAFE_RELEASE(pEnvDS);
	SAFE_RELEASE(pIrradDS);
	SAFE_RELEASE(pIrradTemp);
	SAFE_RELEASE(pIrradTemp2);
	SAFE_RELEASE(pIrradTemp3);
	SAFE_RELEASE(pDepthNormalDS);
	SAFE_RELEASE(pLocalResults);
	SAFE_RELEASE(pLocalResultsSL);
	SAFE_RELEASE(pSunTex);
	SAFE_RELEASE(pLightGlare);
	SAFE_RELEASE(pSunGlare);
	SAFE_RELEASE(pSunGlareAtm);

	for (int i = 0; i < ARRAYSIZE(psShmDS); i++) SAFE_RELEASE(psShmDS[i]);
	for (int i = 0; i < ARRAYSIZE(ptShmRT); i++) SAFE_RELEASE(ptShmRT[i]);
	for (int i = 0; i < ARRAYSIZE(psShmRT); i++) SAFE_RELEASE(psShmRT[i]);
	for (int i = 0; i < ARRAYSIZE(pBlrTemp); i++) SAFE_RELEASE(pBlrTemp[i]);

	if (Lights) {
		delete []Lights;
		Lights = NULL;
	}

	// Particle Streams
	if (nstream) {
		for (DWORD j=0;j<nstream;j++) delete pstream[j];
		delete []pstream;
		pstream = NULL;
	}

	DeleteAllCustomCameras();
	DeleteAllVisuals();
	ExitGDIResources();

	FreePooledSketchpads();
}


// ===========================================================================================
//
void Scene::CreateSunGlare()
{
	// ------------------------------------------------------------------------------
	// Create sun texture and glares

	if (pCreateGlare) SAFE_DELETE(pCreateGlare);

	pCreateGlare = new ImageProcessing(pDevice, "Modules/D3D9Client/Glare.hlsl", "CreateSunGlarePS");
	pCreateGlare->CompileShader("CreateLocalGlarePS");
	pCreateGlare->CompileShader("CreateSunGlareAtmPS");
	pCreateGlare->CompileShader("CreateSunTexPS");
	

	if (!pSunTex) {
		UINT ts = (viewH >> 4) & 0xFFFC; // "ts" will be 64 for a Full HD display;  
		HR(D3DXCreateTexture(pDevice, ts * 5, ts * 5, 0, D3DUSAGE_RENDERTARGET | D3DUSAGE_AUTOGENMIPMAP, D3DFMT_A8R8G8B8, D3DPOOL_DEFAULT, &pSunTex));
		HR(D3DXCreateTexture(pDevice, ts * 4, ts * 4, 0, D3DUSAGE_RENDERTARGET | D3DUSAGE_AUTOGENMIPMAP, D3DFMT_R16F, D3DPOOL_DEFAULT, &pLightGlare));
		HR(D3DXCreateTexture(pDevice, ts * 12, ts * 12, 0, D3DUSAGE_RENDERTARGET | D3DUSAGE_AUTOGENMIPMAP, D3DFMT_R16F, D3DPOOL_DEFAULT, &pSunGlare));
		HR(D3DXCreateTexture(pDevice, ts * 12, ts * 12, 0, D3DUSAGE_RENDERTARGET | D3DUSAGE_AUTOGENMIPMAP, D3DFMT_R16F, D3DPOOL_DEFAULT, &pSunGlareAtm));
	}

	LPDIRECT3DSURFACE9 pTgt = NULL;

	pCreateGlare->Activate("CreateSunGlarePS");
	pSunGlare->GetSurfaceLevel(0, &pTgt);
	pCreateGlare->SetOutputNative(0, pTgt);
	if (!pCreateGlare->Execute(false)) LogErr("pCreateGlare Execute Failed (CreateSunGlarePS)");
	SAFE_RELEASE(pTgt);

	pCreateGlare->Activate("CreateSunGlareAtmPS");
	pSunGlareAtm->GetSurfaceLevel(0, &pTgt);
	pCreateGlare->SetOutputNative(0, pTgt);
	if (!pCreateGlare->Execute(false)) LogErr("pCreateGlare Execute Failed (CreateSunGlareAtmPS)");
	SAFE_RELEASE(pTgt);

	pCreateGlare->Activate("CreateLocalGlarePS");
	pLightGlare->GetSurfaceLevel(0, &pTgt);
	pCreateGlare->SetOutputNative(0, pTgt);
	if (!pCreateGlare->Execute(false)) LogErr("pCreateGlare Execute Failed (CreateLocalGlarePS)");
	SAFE_RELEASE(pTgt);

	pCreateGlare->Activate("CreateSunTexPS");
	pSunTex->GetSurfaceLevel(0, &pTgt);
	pCreateGlare->SetOutputNative(0, pTgt);
	if (!pCreateGlare->Execute(false)) LogErr("pCreateGlare Execute Failed (CreateSunTexPS)");
	SAFE_RELEASE(pTgt);
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
	sunLight.Color = 1.0f;
	sunLight.Ambient = float(ambient)*0.0039f;
	sunLight.Transmission = 1.0f;
	sunLight.Incatter = 0.0f;

	// Update Sunlight direction -------------------------------------
	//
	VECTOR3 rpos, cpos;
	oapiGetGlobalPos(hSun, &rpos);
	oapiCameraGlobalPos(&cpos); rpos-=cpos;
	sunLight.Dir = -unit(rpos);

	// Do not "pre-create" visuals here. Will cause changed call order for vessel callbacks
}


// ===========================================================================================
// Pooled Sketchpad API
// ===========================================================================================

static D3D9Pad *_pad = NULL;

#define SKETCHPAD_LABELS        0  ///< Sketchpad for planetarium mode labels and markers
#define SKETCHPAD_2D_OVERLAY    1  ///< Sketchpad for HUD Overlay render to backbuffer directly
#define SKETCHPAD_DEBUG_TEXT    2  ///< Sketchpad to draw Debug String on a bottom of the screen
#define SKETCHPAD_PLANETARIUM   3  ///< Sketchpad to draw user defined planetarium

// ===========================================================================================

void Scene::OnOptionChanged(int cat, int item)
{
	if (cat == OPTCAT_CELSPHERE)
		m_celSphere->OnOptionChanged(cat, item);
}

// ===========================================================================================
// Get pooled Sketchpad instance
//
D3D9Pad *Scene::GetPooledSketchpad (int id) // one of SKETCHPAD_xxx
{
	assert(id <= SKETCHPAD_PLANETARIUM);

	if (!_pad) _pad = new D3D9Pad("POOLED_SKETCHPAD");

	// Automatically binds a Sketchpad to a top render target
	_pad->BeginDrawing();
	_pad->LoadDefaults();

	switch (id)
	{
	case SKETCHPAD_LABELS:
		_pad->SetFont(pLabelFont);
		_pad->SetTextAlign(Sketchpad::CENTER, Sketchpad::BOTTOM);
		break;

	case SKETCHPAD_2D_OVERLAY:
		break;

	case SKETCHPAD_DEBUG_TEXT:
		_pad->SetFont(pDebugFont);
		_pad->SetTextColor(0xFFFFFF);
		_pad->SetTextAlign(Sketchpad::LEFT, Sketchpad::BOTTOM);
		_pad->QuickPen(0xFF000000);
		_pad->QuickBrush(0xB0000000);
		break;

	case SKETCHPAD_PLANETARIUM:
		break;
	}

	return _pad;
}

// ===========================================================================================
// Release pooled Sketchpad instances
void Scene::FreePooledSketchpads()
{
	SAFE_DELETE(_pad);
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
	if ((DWORD)index<MAX_SCENE_LIGHTS || index>=0) return &Lights[index];
	return NULL;
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
std::set<vVessel *> Scene::GetVessels(double max_dst, bool bAct)
{
	std::set<vVessel *> List;
	VOBJREC *pv;
	for (pv = vobjFirst; pv; pv = pv->next) {
		if (pv->type != OBJTP_VESSEL) continue;
		if (bAct && pv->vobj->IsActive() == false) continue;
		if (pv->vobj->CamDist() < max_dst) List.insert((vVessel *)pv->vobj);
	}
	return List;
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
	vobjIrd = NULL;

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

		gc->UnregisterVisObject(pv->vobj->GetObject());
		
		LogAlw("Deleting Visual %s", _PTR(pv->vobj));
		delete pv->vobj;
		delete pv;
		pv = pvn;
	}
	vobjFirst = vobjLast = NULL;
	vobjEnv = NULL;
	vobjIrd = NULL;
}

// ===========================================================================================
//
Scene::VOBJREC *Scene::AddVisualRec(OBJHANDLE hObj)
{
	_TRACE;

	char buf[256];

	// create the visual and entry
	VOBJREC *pv = new VOBJREC;

	memset(pv, 0, sizeof(VOBJREC));

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

	LogAlw("RegisteringVisual (%s) hVessel=%s, hObj=%s, Vis=%s, Rec=%s, Type=%d", buf, _PTR(hVes), _PTR(hObj), _PTR(pv->vobj), _PTR(pv), pv->type);

	gc->RegisterVisObject(hObj, (VISHANDLE)pv->vobj);
	
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
			// => intensity=0 at sun zenith distance 115?
			//    intensity=1 at sun zenith distance 60?
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

		if (DebugControls::IsActive()) {
			if (oapiGetObjectType(hTgt) == OBJTP_SURFBASE) {
				OBJHANDLE hPlanet = oapiGetBasePlanet(hTgt);
				vPlanet *vp = static_cast<vPlanet *>(GetVisObject(hPlanet));
				if (vp) {
					vBase *vb = vp->GetBaseByHandle(hTgt);
					if (vb) {
						DebugControls::SetVisual(vb);
					}
				}
				return; // why?
			}
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
			if (OapiExtension::RunsOrbiter2010() &&
			    OapiExtension::RunsOrbiterSound40() &&
				oapiIsVessel(hTgt) && // oapiGetObjectType(vo->Object()) == OBJTP_VESSEL &&
				dynamic_cast<vVessel*>(vo)->Playback()
				)
			{
				// Orbiter doesn't do this when (only) camera focus changes
				// during playback, therfore we do it ;)
				oapiSetFocusObject(hTgt);
			}
		}
	}
}

// ===========================================================================================
//
double Scene::GetTargetElevation() const
{
	VESSEL *hVes = oapiGetVesselInterface(Camera.hTarget);
	if (hVes) return hVes->GetSurfaceElevation();
	return 0.0;
}


// ===========================================================================================
//
double Scene::GetFocusGroundAltitude() const
{
	VESSEL *hVes = oapiGetFocusInterface();
	if (hVes) return hVes->GetAltitude() - hVes->GetSurfaceElevation();
	return 0.0;
}



// ===========================================================================================
//
double Scene::GetTargetGroundAltitude() const
{
	VESSEL *hVes = oapiGetVesselInterface(Camera.hTarget);
	if (hVes) return hVes->GetAltitude() - hVes->GetSurfaceElevation();
	return 0.0;
}



// ============================================================================================
// Up, North, Forward in Ecliptic frame
//
void Scene::GetLVLH(vVessel *vV, D3DXVECTOR3 *up, D3DXVECTOR3 *nr, D3DXVECTOR3 *fw)
{
	if (!vV || !up || !nr || !fw) return;

	MATRIX3 grot; VECTOR3 rpos;
	VESSEL *hV = vV->GetInterface(); assert(hV);
	OBJHANDLE hRef = hV->GetGravityRef();
	oapiGetRotationMatrix(hRef, &grot);
	hV->GetRelativePos(hRef, rpos);
	VECTOR3 axis = mul(grot, _V(0, 1, 0));
	normalise(rpos);
	*up = D3DXVEC(rpos);
	*fw = D3DXVEC(unit(crossp(axis, rpos)));
	D3DXVec3Cross(nr, up, fw);
	D3DXVec3Normalize(nr, nr);
}



// ===========================================================================================
// Compute a distance to a near/far plane
// ===========================================================================================

float Scene::ComputeNearClipPlane()
{
	float zsurf = 1000.0f;
	VOBJREC *pv = NULL;

	OBJHANDLE hObj = Camera.hObj_proxy;
	OBJHANDLE hTgt = Camera.hTarget;
	VESSEL *hVes = oapiGetVesselInterface(hTgt);

	if (hObj && hVes) {
		VECTOR3 pos;
		oapiGetGlobalPos(hObj,&pos);
		double g = atan(Camera.apsq);
		double t = dotp(unit(Camera.pos-pos), unit(Camera.dir));
		if (t<-1.0) t=1.0; if (t>1.0) t=1.0f;
		double a = PI - acos(t);
		double R = oapiGetSize(hObj) + hVes->GetSurfaceElevation();
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

	// Update camera parameters --------------------------------------
	// and call vObject::Update() for all visuals
	//
	UpdateCameraFromOrbiter(RENDERPASS_MAINSCENE);

	if (Camera.hObj_proxy) D3D9Effect::UpdateEffectCamera(Camera.hObj_proxy);

	// Update Sunlight direction -------------------------------------
	//
	VECTOR3 rpos;
	oapiGetGlobalPos(hSun, &rpos);
	rpos -= Camera.pos;
	sunLight.Dir = -unit(rpos);

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
	bglvl = (sky_color.x + sky_color.y + sky_color.z) / 3.0;
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
					if ((em->GetVisibility() == LightEmitter::VIS_EXTERNAL) || (em->GetVisibility() == LightEmitter::VIS_ALWAYS))
						AddLocalLight(em, pv->vobj);
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

	// Clear active local lisghts list -------------------------------
	for (int i = 0; i < MAX_SCENE_LIGHTS; i++) Lights[i].Reset();
}

// ===========================================================================================
//
void Scene::AddLocalLight(const LightEmitter *le, const vObject *vo)
{
	if (Lights==NULL) return;
	if (le->IsActive()==false || le->GetIntensity()==0.0) return;

	assert(vo != NULL);

	D3D9Light lght(le, vo);

	// -----------------------------------------------------------------------------
	// Replace or Add
	//
	if (nLights == MAX_SCENE_LIGHTS) {
		if (lght.Dst2 > lmaxdst2) return;
		DWORD imax = 0;
		for (DWORD i = 0; i < MAX_SCENE_LIGHTS; i++) if (Lights[i].Dst2 > lmaxdst2) imax = i;
		Lights[imax] = lght;
		lmaxdst2 = lght.Dst2;
	}
	else {
		Lights[nLights] = lght;
		if (lght.Dst2>lmaxdst2) lmaxdst2 = lght.Dst2;
		nLights++;
	}
}

// ===========================================================================================
//
void Scene::ComputeLocalLightsVisibility()
{

	if (!ptgBuffer[GBUF_DEPTH] || !pLocalCompute) {
		Config->bGlares = false;
		Config->bLocalGlares = false;
		return;
	}

	VECTOR3 gsun;
	oapiGetGlobalPos(oapiGetObjectByIndex(0), &gsun);

	// Put the Sun on a top of the list
	LLCBuf[0].index = 0.0f;
	LLCBuf[0].pos = FVECTOR3(unit(gsun - Camera.pos)) * 10e4;
	LLCBuf[0].cone = 1.0f;

	int nGlares = 1;

	for (int i = 0; i < nLights; i++)
	{
		if (Lights[i].cone > 0.0f) {
			LLCBuf[nGlares].index = float(nGlares);
			LLCBuf[nGlares].pos = Lights[i].Position;
			LLCBuf[nGlares].cone = Lights[i].cone;
			Lights[i].GPUId = nGlares;
			nGlares++;
		}
	}

	struct {
		D3DXMATRIX mVP;
		D3DXMATRIX mSVP;
		FVECTOR4 vSrc;
		FVECTOR3 vDir;
	} ComputeData;

	D3DSURFACE_DESC desc;
	pLocalResultsSL->GetDesc(&desc);

	D3DXMatrixOrthoOffCenterLH(&ComputeData.mVP, 0.0f, (float)desc.Width, (float)desc.Height, 0.0f, 0.0f, 1.0f);

	psgBuffer[GBUF_DEPTH]->GetDesc(&desc);

	ComputeData.vSrc = FVECTOR4((float)desc.Width, (float)desc.Height, 1.0f / (float)desc.Width, 1.0f / (float)desc.Height);
	ComputeData.vDir = Camera.z;
	ComputeData.mSVP = Camera.mProjView;

	// Must setup render target before calling Setup()
	gc->PushRenderTarget(pLocalResultsSL, NULL, RENDERPASS_UNKNOWN);

	pLocalCompute->ClearTextures();
	pLocalCompute->SetPSConstants("cbPS", &ComputeData, sizeof(ComputeData));
	pLocalCompute->SetPSConstants("cbKernel", DepthSampleKernel, sizeof(DepthSampleKernel));
	pLocalCompute->SetVSConstants("cbPS", &ComputeData, sizeof(ComputeData));
	pLocalCompute->SetTexture("tDepth", ptgBuffer[GBUF_DEPTH], IPF_CLAMP | IPF_POINT);
	pLocalCompute->Setup(pLocalLightsDecl, false, 0);
	pLocalCompute->UpdateTextures();

	// Compute local lights visibility
	HR(pDevice->DrawPrimitiveUP(D3DPT_POINTLIST, nGlares, &LLCBuf, sizeof(LocalLightsCompute)));

	pLocalCompute->DetachTextures();
	gc->PopRenderTargets();
}


// ===========================================================================================
//
void Scene::RecallDefaultState()
{
	HR(pDevice->SetRenderState(D3DRS_FILLMODE, D3DFILL_SOLID));
	HR(pDevice->SetRenderState(D3DRS_STENCILENABLE, false));
	HR(pDevice->SetRenderState(D3DRS_COLORWRITEENABLE, 0xF));
	HR(pDevice->SetRenderState(D3DRS_ZENABLE, true));
	HR(pDevice->SetRenderState(D3DRS_ZWRITEENABLE, true));
	HR(pDevice->SetRenderState(D3DRS_ALPHATESTENABLE, false));
	HR(pDevice->SetRenderState(D3DRS_ALPHABLENDENABLE, false));
	HR(pDevice->SetRenderState(D3DRS_BLENDOP, D3DBLENDOP_ADD));
	HR(pDevice->SetRenderState(D3DRS_SRCBLEND, D3DBLEND_SRCALPHA));
	HR(pDevice->SetRenderState(D3DRS_DESTBLEND, D3DBLEND_INVSRCALPHA));
	HR(pDevice->SetRenderState(D3DRS_CULLMODE, D3DCULL_CCW));
}


// ===========================================================================================
//
void Scene::RenderMainScene()
{
	_TRACE;

	dwFrameId++; // Advance to a next frame

	double scene_time = D3D9GetTime();
	D3D9SetTime(D3D9Stats.Timer.CamVis, scene_time);

	UpdateCamVis();


	// Update Vessel Animations
	//
	for (VOBJREC *pv = vobjFirst; pv; pv = pv->next) {
		if (pv->type == OBJTP_VESSEL) {
			vVessel *vv = (vVessel *)pv->vobj;
			vv->UpdateAnimations();
		}
	}


	if (vFocus == NULL) return;

	LPDIRECT3DSURFACE9 pBackBuffer;

	if (pOffscreenTarget) pBackBuffer = pOffscreenTarget;
	else				  pBackBuffer = gc->GetBackBuffer();


	// Begin a Scene ------------------------------------------------------------------------------------
	//
	if (FAILED (gc->BeginScene())) return;



	// -------------------------------------------------------------------------------------------------------
	// Render Custom Camera and Environment Views
	// -------------------------------------------------------------------------------------------------------
	bool bIrrad = Config->EnvMapMode && Config->bIrradiance;

	if (Config->CustomCamMode == 0 && dwTurn == RENDERTURN_CUSTOMCAM) dwTurn++;
	if (Config->EnvMapMode == 0 && dwTurn == RENDERTURN_ENVCAM) dwTurn++;
	if (!bIrrad && dwTurn == RENDERTURN_IRRADIANCE) dwTurn++;

	if (dwTurn>RENDERTURN_LAST) dwTurn = 0;

	int RenderCount = max(1, Config->EnvMapFaces);


	// --------------------------------------------------------------------------------------------------------
	// Render Custom Camera view for a focus vessel
	// --------------------------------------------------------------------------------------------------------

	if (dwTurn == RENDERTURN_CUSTOMCAM)
	{
		if (Config->CustomCamMode && (CustomCams.size() > 0))
		{
			if (camCurrent == CustomCams.cend()) camCurrent = CustomCams.cbegin();

			OBJHANDLE hVessel = vFocus->GetObjectA();
			
			vObject *vO = GetVisObject((*camCurrent)->hVessel);
			double maxd = min(500e3, GetCameraAltitude() + 15e3);

			if (vO->CamDist() < maxd && (*camCurrent)->bActive)
			{
				RenderCustomCameraView((*camCurrent));

				if ((*camCurrent)->pRenderProc) {
					D3D9Pad *pSkp = (D3D9Pad * )gc->clbkGetSketchpad((*camCurrent)->hSurface);
					pSkp->LoadDefaults();
					(*camCurrent)->pRenderProc(pSkp, (*camCurrent)->pUser);
					gc->clbkReleaseSketchpad(pSkp);
				}
			}
			camCurrent++;
		}
	}


	// -------------------------------------------------------------------------------------------------------
	// Render Environmental Map For the Vessels
	// -------------------------------------------------------------------------------------------------------

	if (dwTurn == RENDERTURN_ENVCAM) {

		if (Config->EnvMapMode) {
			DWORD flags = 0;
			if (Config->EnvMapMode == 1) flags |= 0x01;
			if (Config->EnvMapMode == 2) flags |= (0x03 | 0x20);

			if (vobjEnv == NULL) vobjEnv = vobjFirst;

			while (vobjEnv) {
				if (vobjEnv->type == OBJTP_VESSEL && vobjEnv->apprad>8.0f) {
					if (vobjEnv->vobj) {
						vVessel *vVes = (vVessel *)vobjEnv->vobj;
						if (vVes->RenderENVMap(pDevice, RenderCount, flags) == false) break; // Not yet done with this vessel
					}
				}
				vobjEnv = vobjEnv->next; // Move to the next one
			}
		}
	}



	// -------------------------------------------------------------------------------------------------------
	// Render Irradiance Map For Vessels
	// -------------------------------------------------------------------------------------------------------

	if (dwTurn == RENDERTURN_IRRADIANCE) {

		if (Config->EnvMapMode && Config->bIrradiance) {
			DWORD flags = 0;
			if (Config->EnvMapMode == 1) flags |= 0x01;
			if (Config->EnvMapMode == 2) flags |= (0x03 | 0x20);

			if (vobjIrd == NULL) vobjIrd = vobjFirst;

			while (vobjIrd) {
				if (vobjIrd->type == OBJTP_VESSEL && vobjIrd->apprad>8.0f) {
					if (vobjIrd->vobj) {
						vVessel *vVes = (vVessel *)vobjIrd->vobj;
						if (vVes->ProbeIrradiance(pDevice, RenderCount, flags) == false) break; // Not yet done with this vessel
					}
				}
				vobjIrd = vobjIrd->next; // Move to the next one
			}
		}
	}


	// ---------------------------------------------------------------------------------------------
	// Init. camera setup and create a render list
	// ---------------------------------------------------------------------------------------------

	VOBJREC* pv = NULL;
	LPDIRECT3DTEXTURE9 pShdMap = NULL;

	UpdateCameraFromOrbiter(RENDERPASS_MAINSCENE);
	UpdateCamVis();

	RenderList.clear();

	for (pv = vobjFirst; pv; pv = pv->next) {
		if (!pv->vobj->IsActive()) continue;
		if (!pv->vobj->IsVisible()) continue;
		if (pv->type == OBJTP_VESSEL) {
			vVessel* vV = (vVessel*)pv->vobj;
			RenderList.push_back(vV);
			vV->bStencilShadow = true;
		}
	}

	float znear_for_vessels = ComputeNearClipPlane();




	// ---------------------------------------------------------------------------------------------
	// Start Rendering of Normal and Depth Buffer for SSAO and (point in scene) visibility checks
	// ---------------------------------------------------------------------------------------------

	if (psgBuffer[GBUF_DEPTH] && pDepthNormalDS)
	{
		SetCameraFrustumLimits(0.1f, 1e6f);
		BeginPass(RENDERPASS_NORMAL_DEPTH);

		gc->PushRenderTarget(psgBuffer[GBUF_DEPTH], pDepthNormalDS, RENDERPASS_NORMAL_DEPTH);

		RecallDefaultState();

		// Clear buffers
		HR(pDevice->Clear(0, NULL, D3DCLEAR_TARGET | D3DCLEAR_ZBUFFER | D3DCLEAR_STENCIL, 0, 1.0f, 0L));

		// Render vessels
		for (auto* vVes : RenderList) vVes->Render(pDevice, false);

		// Render Cockpit
		if (oapiCameraInternal() && vFocus) vFocus->Render(pDevice, true);

		gc->PopRenderTargets();
		PopPass();
	}

	// ---------------------------------------------------------------------------------------------
	// Compute visibility of the Sun and Local light sources. After field depth render ! ! !
	// ---------------------------------------------------------------------------------------------

	ComputeLocalLightsVisibility();


	// -------------------------------------------------------------------------------------------------------
	// Start Main Scene Rendering
	// -------------------------------------------------------------------------------------------------------

	RenderFlags = 0xFFFFFFFF; // Not used for main scene, set to 0xFFFFFFFF 

	// Push main render target and depth surfaces
	//
	gc->PushRenderTarget(pBackBuffer, gc->GetDepthStencil(), RENDERPASS_MAINSCENE);	// Main Scene


	if (DebugControls::IsActive()) {
		HR(pDevice->Clear(0, NULL, D3DCLEAR_TARGET | D3DCLEAR_ZBUFFER | D3DCLEAR_STENCIL, 0, 1.0f, 0L));
		DWORD flags = *(DWORD*)gc->GetConfigParam(CFGPRM_GETDEBUGFLAGS);
		if (flags&DBG_FLAGS_WIREFRAME) pDevice->SetRenderState(D3DRS_FILLMODE, D3DFILL_WIREFRAME);
		else						   pDevice->SetRenderState(D3DRS_FILLMODE, D3DFILL_SOLID);
	}
	else {
		// Clear the viewport
		HR(pDevice->Clear(0, NULL, D3DCLEAR_TARGET | D3DCLEAR_ZBUFFER | D3DCLEAR_STENCIL, 0, 1.0f, 0L));
	}



	// Do we use z-clear render mode or not ?
	bool bClearZBuffer = false;
	if ( (GetTargetGroundAltitude() > 2e3) && (oapiCameraInternal() == false)) bClearZBuffer = true;
	if (IsProxyMesh()) bClearZBuffer = false;


	if (DebugControls::IsActive()) {
		DWORD camMode = *(DWORD*)gc->GetConfigParam(CFGPRM_GETCAMERAMODE);
		if (camMode!=0) znear_for_vessels = 0.1f;
	}

	// -------------------------------------------------------------------------------------------------------
	// render celestial sphere background
	// -------------------------------------------------------------------------------------------------------

	pDevice->SetRenderState(D3DRS_CULLMODE, D3DCULL_CW);

	bool bEnableAtmosphere = false;

	vPlanet *vPl = GetCameraProxyVisual();

	// -------------------------------------------------------------------------------------------------------
	// Render the celestial sphere (background image, stars, planetarium features)
	// -------------------------------------------------------------------------------------------------------

	// Set generic clip plane distances for celestial sphere
	SetCameraFrustumLimits(0.1, 10);

	m_celSphere->Render(pDevice, sky_color);

	// Set Initial Near clip plane distance
	if (bClearZBuffer) SetCameraFrustumLimits(1e3, 3e8f);
	else			   SetCameraFrustumLimits(znear_for_vessels, 3e8f);

	pDevice->SetRenderState(D3DRS_CULLMODE, D3DCULL_CCW);

	// ---------------------------------------------------------------------------------------------
	// Create a caster list for shadow mapping
	// ---------------------------------------------------------------------------------------------

	Casters.clear();

	for (pv = vobjFirst; pv; pv = pv->next) {
		if (!pv->vobj->IsActive()) continue;
		if (pv->type == OBJTP_VESSEL) Casters.push_back((vVessel *)pv->vobj);
	}

	Casters.sort(sort_vessels);



	// ---------------------------------------------------------------------------------------------
	// Render shadow map for vFocus early for surface base and planet rendering
	// ---------------------------------------------------------------------------------------------

	int shadow_lod = -1;
	float bouble_rad = 10.0f;		// Terrain shadow mapping coverage

	if (Config->ShadowMapMode >= 1 && Config->TerrainShadowing == 2) {

		SmapRenderList.clear();
		SmapRenderList.push_back(vFocus);

		D3DXVECTOR3 ld = sunLight.Dir;
		D3DXVECTOR3 pos = vFocus->GetBoundingSpherePosDX();
		float rad = vFocus->GetBoundingSphereRadius();
		float frad = rad;

		vFocus->bStencilShadow = false;


		// What else should be included besides vFocus ?

		for (auto v : Casters)
		{
			if (v == vFocus) continue;
			if (v->HasShadow() == false) continue;

			D3DXVECTOR3 bs_pos = v->GetBoundingSpherePosDX();
			float bs_rad = v->GetBoundingSphereRadius();

			if (bs_rad > 80.0) continue;

			D3DXVECTOR3 bc = bs_pos - pos;
			float z = D3DXVec3Dot(&ld, &bc);
			if (fabs(z) > 1e3) continue;
			D3DXVECTOR3 fbc = bc - ld * z;
			float dst = D3DXVec3Length(&fbc);
			if (dst > 1e3) continue;

			float nrd = (rad + dst + bs_rad) * 0.5f;

			bool bInclude = false;

			if (dst < (bs_rad + frad)) bInclude = true;
			if (nrd < bouble_rad) bInclude = true;

			if (bInclude) {

				v->bStencilShadow = false;
				SmapRenderList.push_back(v);

				if (nrd < rad) continue;

				if (nrd < bs_rad) {
					pos = bs_pos;
					rad = bs_rad;
				}
				else {
					if (dst > 0.001f) pos += fbc * ((nrd - rad) / dst);
					rad = nrd;
				}
			}
		}

		shadow_lod = RenderShadowMap(pos, ld, rad, false, true);

		pShdMap = smap.pShadowMap;
	}



	// ---------------------------------------------------------------------------------------------
	// Render Planets
	// ---------------------------------------------------------------------------------------------

	DWORD plnmode = *(DWORD*)gc->GetConfigParam(CFGPRM_PLANETARIUMFLAG);
	DWORD mkrmode = *(DWORD*)gc->GetConfigParam(CFGPRM_SURFMARKERFLAG);

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


		D3D9Pad *pSketch = GetPooledSketchpad(SKETCHPAD_LABELS);

		if (pSketch) {

			if (isActive) plist[i].vo->RenderVectors(pDevice, pSketch);

			if (mkrmode & MKR_ENABLE) {

				if (mkrmode & MKR_CMARK) {
					VECTOR3 pp;
					char name[256];
					oapiGetObjectName(hObj, name, 256);
					oapiGetGlobalPos(hObj, &pp);

					m_celSphere->EnsureMarkerDrawingContext((oapi::Sketchpad**)&pSketch, 0, m_celSphere->MarkerColor(0), m_celSphere->MarkerPen(0));
					RenderObjectMarker(pSketch, pp, std::string(name), std::string(), 0, viewH / 80);
				}

				if (isActive && (mkrmode & MKR_SURFMARK) && (oapiGetObjectType(hObj) == OBJTP_PLANET))
				{
					int label_format = *(int*)oapiGetObjectParam(hObj, OBJPRM_PLANET_LABELENGINE);
					if (label_format < 2 && (mkrmode & MKR_LMARK)) // user-defined planetary surface labels
					{
						double rad = oapiGetSize(hObj);
						double apprad = rad / (plist[i].dist * tan(GetCameraAperture()));
						const GraphicsClient::LABELLIST *list;
						DWORD n, nlist;
						MATRIX3 prot;
						VECTOR3 ppos, cpos;

						nlist = gc->GetSurfaceMarkers(hObj, &list);

						oapiGetRotationMatrix(hObj, &prot);
						oapiGetGlobalPos(hObj, &ppos);
						VECTOR3 cp = GetCameraGPos();
						cpos = tmul(prot, cp - ppos); // camera in local planet coords

						for (n = 0; n < nlist; n++) {

							if (list[n].active && apprad*list[n].distfac > LABEL_DISTLIMIT) {

								int size = (int)(viewH / 80.0*list[n].size + 0.5);
								int col = list[n].colour;

								m_celSphere->EnsureMarkerDrawingContext((oapi::Sketchpad**)&pSketch, 0, m_celSphere->MarkerColor(col), m_celSphere->MarkerPen(col));
								const std::vector<oapi::GraphicsClient::LABELSPEC>& ls = list[n].marker;
								VECTOR3 sp;
								for (int j = 0; j < ls.size(); j++) {
									if (dotp(ls[j].pos, cpos - ls[j].pos) >= 0.0) { // surface point visible?
										sp = mul(prot, ls[j].pos) + ppos;
										RenderObjectMarker(pSketch, sp, ls[j].label[0], ls[j].label[1], list[n].shape, size);
									}
								}
							}
						}
					}

					if (mkrmode & MKR_BMARK) {

						DWORD n = oapiGetBaseCount(hObj);
						MATRIX3 prot;
						oapiGetRotationMatrix(hObj, &prot);
						int size = (int)(viewH / 80.0);

						m_celSphere->EnsureMarkerDrawingContext((oapi::Sketchpad**)&pSketch, 0, m_celSphere->MarkerColor(0), m_celSphere->MarkerPen(0));

						for (DWORD i = 0; i < n; i++) {

							OBJHANDLE hBase = oapiGetBaseByIndex(hObj, i);

							VECTOR3 ppos, cpos, bpos;

							oapiGetGlobalPos(hObj, &ppos);
							oapiGetGlobalPos(hBase, &bpos);
							VECTOR3 cp = GetCameraGPos();
							cpos = tmul(prot, cp - ppos); // camera in local planet coords
							bpos = tmul(prot, bpos - ppos);

							double apprad = 8000e3 / (length(cpos - bpos) * tan(GetCameraAperture()));

							if (dotp(bpos, cpos - bpos) >= 0.0 && apprad > LABEL_DISTLIMIT) { // surface point visible?
								char name[64]; oapiGetObjectName(hBase, name, 63);
								VECTOR3 sp = mul(prot, bpos) + ppos;
								RenderObjectMarker(pSketch, sp, std::string(name), std::string(), 0, size);
							}
						}
					}
				}
			}

			pSketch->EndDrawing();	// SKETCHPAD_LABELS
		}
	}


	// -------------------------------------------------------------------------------------------------------
	// render a user defined planetarium art
	// -------------------------------------------------------------------------------------------------------

	if (plnmode & PLN_ENABLE) {
		D3D9Pad *pSketch = GetPooledSketchpad(SKETCHPAD_PLANETARIUM);
		gc->MakeRenderProcCall(pSketch, RENDERPROC_PLANETARIUM, GetViewMatrix(), GetProjectionMatrix());
		pSketch->EndDrawing(); // SKETCHPAD_PLANETARIUM
	}


	// -------------------------------------------------------------------------------------------------------
	// render a user defined exterior art
	// -------------------------------------------------------------------------------------------------------

	if (oapiCameraInternal() == false) {
		D3D9Pad *pSketch = GetPooledSketchpad(SKETCHPAD_PLANETARIUM);
		gc->MakeRenderProcCall(pSketch, RENDERPROC_EXTERIOR, GetViewMatrix(), GetProjectionMatrix());
		pSketch->EndDrawing(); // SKETCHPAD_PLANETARIUM
	}

	/*for (DWORD i = 0; i < nplanets; ++i)
	{
		OBJHANDLE hObj = plist[i].vo->Object();
		if (oapiGetObjectType(hObj) != OBJTP_PLANET) continue;
		D3D9Pad* pSketch = GetPooledSketchpad(SKETCHPAD_PLANETARIUM);
		pSketch->LoadDefaults();
		pSketch->SetViewMode(Sketchpad::USER);
		pSketch->SetViewProj(GetViewMatrix(), GetProjectionMatrix());
		static_cast<vPlanet*>(plist[i].vo)->TestComputations(pSketch);
		pSketch->EndDrawing(); // SKETCHPAD_PLANETARIUM
	}*/

	// -------------------------------------------------------------------------------------------------------
	// render new-style surface markers
	// -------------------------------------------------------------------------------------------------------

	if ((mkrmode & MKR_ENABLE) && (mkrmode & MKR_LMARK))
	{
		D3D9Pad* pSketch = GetPooledSketchpad(SKETCHPAD_LABELS);
		m_celSphere->EnsureMarkerDrawingContext((oapi::Sketchpad**)&pSketch, 0, 0, m_celSphere->MarkerPen(6));

		int fontidx = -1;
		for (DWORD i = 0; i < nplanets; ++i)
		{
			OBJHANDLE hObj = plist[i].vo->Object();
			if (oapiGetObjectType(hObj) != OBJTP_PLANET) { continue; }
			if (!surfLabelsActive) {
				static_cast<vPlanet*>( plist[i].vo )->ActivateLabels(true);
			}

			int label_format = *(int*)oapiGetObjectParam(hObj, OBJPRM_PLANET_LABELENGINE);

			if (label_format == 2)
			{
				static_cast<vPlanet*>(plist[i].vo)->RenderLabels(pDevice, pSketch, label_font, &fontidx);
			}
		}

		pSketch->EndDrawing();	// SKETCHPAD_LABELS

		surfLabelsActive = true;
	}
	else {
		surfLabelsActive = false;
	}


	// -------------------------------------------------------------------------------------------------------
	// render the vessel objects
	// -------------------------------------------------------------------------------------------------------

	// Set near clip plane for vessel exterior rendering
	if (bClearZBuffer) {
		pDevice->Clear(0, NULL, D3DCLEAR_ZBUFFER, 0, 1.0f, 0L); // clear z-buffer
		SetCameraFrustumLimits(znear_for_vessels, 1e8f);
	}


	D3D9Effect::UpdateEffectCamera(Camera.hObj_proxy);


	D3D9Pad* pSketch = GetPooledSketchpad(SKETCHPAD_LABELS);
	m_celSphere->EnsureMarkerDrawingContext((oapi::Sketchpad**)&pSketch, 0, m_celSphere->MarkerColor(0), m_celSphere->MarkerPen(0));

	// Render the vessels inside the shadows
	//
	if (Config->ShadowMapMode >= 1) {

		D3DXVECTOR3 ld = sunLight.Dir;
		D3DXVECTOR3 pos = vFocus->GetBoundingSpherePosDX();
		float rad = vFocus->GetBoundingSphereRadius();

		int shadow_lod = RenderShadowMap(pos, ld, rad);

		if (shadow_lod >= 0) {

			pShdMap = ptShmRT[shadow_lod];

			auto it = RenderList.begin();

			while (it != RenderList.end()) {
				if ((*it)->IsInsideShadows()) {
					(*it)->Render(pDevice);
					RenderVesselMarker((*it), pSketch);
					it = RenderList.erase(it);
				}
				else ++it;
			}
		}
	}



	if ((Config->ShadowMapMode >= 2) && (DebugControls::IsActive()==false)) {
		// Don't render more shadows if debug controls are open

		std::list<vVessel *> Intersect;

		// Select the objects to shadow map
		//
		if (Config->ShadowMapMode >= 3) {
			for (auto it = RenderList.begin(); it != RenderList.end(); ++it) {
				if ((*it)->CamDist() < 1e3) Intersect.push_back((*it));
			}
		}
		else {
			for (auto it = RenderList.begin(); it != RenderList.end(); ++it) {
				if ((*it)->IntersectShadowTarget()) Intersect.push_back((*it));
			}
		}


		while (!Intersect.empty()) {

			D3DXVECTOR3 ld = sunLight.Dir;
			D3DXVECTOR3 pos = Intersect.front()->GetBoundingSpherePosDX();
			float rad = Intersect.front()->GetBoundingSphereRadius();

			Intersect.pop_front();

			int lod = RenderShadowMap(pos, ld, rad);

			if (lod >= 0) {

				// Render objects in shadow
				auto it = RenderList.begin();

				while (it != RenderList.end()) {
					if ((*it)->IsInsideShadows()) {
						(*it)->Render(pDevice);
						RenderVesselMarker((*it), pSketch);
						Intersect.remove((*it));
						it = RenderList.erase(it);
					}
					else ++it;
				}
			}
		}
	}


	// Render the remaining vessels those are not yet renderred
	//
	smap.pShadowMap = NULL;

	while (RenderList.empty()==false) {
		RenderList.front()->Render(pDevice);
		RenderVesselMarker(RenderList.front(), pSketch);
		RenderList.pop_front();
	}

	pSketch->EndDrawing();	// SKETCHPAD_LABELS



	// -------------------------------------------------------------------------------------------------------
	// render custom user objects
	// -------------------------------------------------------------------------------------------------------

	if (oapiCameraInternal() == false) {
		if (gc->IsGenericProcEnabled(GENERICPROC_RENDER_EXTERIOR)) {
			gc->MakeGenericProcCall(GENERICPROC_RENDER_EXTERIOR, 0, NULL);
		}
	}


	// -------------------------------------------------------------------------------------------------------
	// render the vessel sub-systems
	// -------------------------------------------------------------------------------------------------------

	// render exhausts
	//
	for (pv=vobjFirst; pv; pv=pv->next) {
		if (!pv->vobj->IsActive() || !pv->vobj->IsVisible() || pv->vobj->GetMeshCount() < 1) continue;
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

	DWORD bfvmode = *(DWORD*)gc->GetConfigParam(CFGPRM_FORCEVECTORFLAG);
	DWORD favmode = *(DWORD*)gc->GetConfigParam(CFGPRM_FRAMEAXISFLAG);

	if (bfvmode & BFV_ENABLE || favmode & FAV_ENABLE)
	{

		pDevice->Clear(0, NULL, D3DCLEAR_ZBUFFER,  0, 1.0f, 0L); // clear z-buffer

		pSketch = GetPooledSketchpad(SKETCHPAD_LABELS);
		pSketch->SetFont(pAxisFont);
		pSketch->SetTextAlign(Sketchpad::LEFT, Sketchpad::TOP);

		for (pv=vobjFirst; pv; pv=pv->next) {
			if (!pv->vobj->IsActive()) continue;
			if (!pv->vobj->IsVisible()) continue;
			if (oapiCameraInternal() && vFocus==pv->vobj) continue;

			pv->vobj->RenderVectors(pDevice, pSketch);
		}

		pSketch->EndDrawing();	// SKETCHPAD_LABELS
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
				if ((em->GetVisibility() == LightEmitter::VIS_COCKPIT) || (em->GetVisibility() == LightEmitter::VIS_ALWAYS))
					AddLocalLight(em, vFocus);
			}
		}

		pDevice->Clear(0, NULL, D3DCLEAR_ZBUFFER,  0, 1.0f, 0L); // clear z-buffer
		double znear = Config->VCNearPlane;
		if (znear<0.01) znear=0.01;
		if (znear>1.0)  znear=1.0;
		OBJHANDLE hFocus = oapiGetFocusObject();
		SetCameraFrustumLimits(znear, oapiGetSize(hFocus)*2.0);
		vFocus->Render(pDevice, true);
	}

	pDevice->SetRenderState(D3DRS_FILLMODE, D3DFILL_SOLID);


	// End Of Main Scene Rendering ---------------------------------------------
	//








	// -------------------------------------------------------------------------------------------------------
	// Copy Offscreen render target to backbuffer
	// -------------------------------------------------------------------------------------------------------


	if (pOffscreenTarget && pLightBlur) {

		int iGensPerFrame = pLightBlur->FindDefine("PassCount");

		D3DSURFACE_DESC colr;
		D3DSURFACE_DESC blur;

		psgBuffer[GBUF_BLUR]->GetDesc(&blur);
		psgBuffer[GBUF_COLOR]->GetDesc(&colr);

		D3DXVECTOR2 scr = D3DXVECTOR2(1.0f / float(colr.Width), 1.0f / float(colr.Height));
		D3DXVECTOR2 sbf = D3DXVECTOR2(1.0f / float(blur.Width), 1.0f / float(blur.Height));


		if (pLightBlur->IsOK())
		{
			float fInt = float(Config->GFXIntensity);
			float fDst = float(Config->GFXDistance);
			float fThr = float(Config->GFXThreshold);
			float fGam = float(Config->GFXGamma);

			// Grap a copy of a backbuffer
			pDevice->StretchRect(pOffscreenTarget, NULL, psgBuffer[GBUF_COLOR], NULL, D3DTEXF_POINT);

			pLightBlur->SetFloat("vSB", &sbf, sizeof(D3DXVECTOR2));
			pLightBlur->SetBool("bBlendIn", false);
			pLightBlur->SetBool("bBlur", false);

			pLightBlur->SetFloat("fIntensity", &fInt, sizeof(float));
			pLightBlur->SetFloat("fDistance", &fDst, sizeof(float));
			pLightBlur->SetFloat("fThreshold", &fThr, sizeof(float));
			pLightBlur->SetFloat("fGamma", &fGam, sizeof(float));	

			// -----------------------------------------------------
			pLightBlur->SetBool("bSample", true);
			pLightBlur->SetTextureNative("tBack", ptgBuffer[GBUF_COLOR], IPF_POINT | IPF_CLAMP);
			pLightBlur->SetOutputNative(0, psgBuffer[GBUF_BLUR]);

			if (!pLightBlur->Execute(true)) LogErr("pLightBlur Execute Failed");

			// -----------------------------------------------------
			pLightBlur->SetBool("bSample", false);
			pLightBlur->SetBool("bBlur", true);

			for (int i = 0; i < iGensPerFrame; i++) {

				pLightBlur->SetBool("bDir", false);
				pLightBlur->SetTextureNative("tBlur", ptgBuffer[GBUF_BLUR], IPF_POINT | IPF_CLAMP);
				pLightBlur->SetOutputNative(0, psgBuffer[GBUF_TEMP]);

				if (!pLightBlur->Execute(true)) LogErr("pLightBlur Execute Failed");

				pLightBlur->SetBool("bDir", true);
				pLightBlur->SetTextureNative("tBlur", ptgBuffer[GBUF_TEMP], IPF_POINT | IPF_CLAMP);
				pLightBlur->SetOutputNative(0, psgBuffer[GBUF_BLUR]);

				if (!pLightBlur->Execute(true)) LogErr("pLightBlur Execute Failed");
			}

			pLightBlur->SetBool("bBlendIn", true);
			pLightBlur->SetBool("bBlur", false);
			pLightBlur->SetTextureNative("tBack", ptgBuffer[GBUF_COLOR], IPF_LINEAR | IPF_CLAMP);
			pLightBlur->SetTextureNative("tBlur", ptgBuffer[GBUF_BLUR], IPF_LINEAR | IPF_CLAMP);
			pLightBlur->SetOutputNative(0, gc->GetBackBuffer());

			if (!pLightBlur->Execute(true)) LogErr("pLightBlur Execute Failed");
		}
		else {
			LogErr("pLightBlur is not o.k.");
		}
	}


	// -------------------------------------------------------------------------------------------------------
	// Render glares for the Sun and local lights
	// -------------------------------------------------------------------------------------------------------

	gc->PushRenderTarget(gc->GetBackBuffer(), gc->GetDepthStencil(), RENDERPASS_MAINSCENE);	
	RenderGlares();
	gc->PopRenderTargets();


	// -------------------------------------------------------------------------------------------------------
	// Render GDI Overlay to backbuffer directly
	// -------------------------------------------------------------------------------------------------------

	if (pGDIOverlay)
	{
		if (pGDIOverlay->IsOK())
		{
			gc->bGDIClear = true; // Must clear background before continuing drawing into overlay
			D3DXCOLOR clr(0x4080F0); // RGB ColorKey
			pGDIOverlay->SetTextureNative("tSrc", ptgBuffer[GBUF_GDI], IPF_POINT | IPF_CLAMP);
			pGDIOverlay->SetFloat("vColorKey", &clr, sizeof(clr));
			pGDIOverlay->SetOutputNative(0, gc->GetBackBuffer());
			if (!pGDIOverlay->Execute(true)) LogErr("pGDIOverlay Execute Failed");
		}
		else
		{
			LogErr("pGDIOverlay is not OK.");
		}
	}


	// -------------------------------------------------------------------------------------------------------
	// Render HUD Overlay to backbuffer directly
	// -------------------------------------------------------------------------------------------------------


	gc->PushRenderTarget(gc->GetBackBuffer(), gc->GetDepthStencil(), RENDERPASS_MAINOVERLAY);	// Overlay

	pSketch = GetPooledSketchpad(SKETCHPAD_2D_OVERLAY);

	if (pSketch) {
		gc->MakeRenderProcCall(pSketch, RENDERPROC_HUD_1ST, NULL, NULL);
		pSketch->EndDrawing(); // SKETCHPAD_2D_OVERLAY
	}
	gc->Render2DOverlay();
	pSketch = GetPooledSketchpad(SKETCHPAD_2D_OVERLAY);
	if (pSketch) {
		gc->MakeRenderProcCall(pSketch, RENDERPROC_HUD_2ND, NULL, NULL);
		pSketch->EndDrawing(); // SKETCHPAD_2D_OVERLAY
	}


	// Enable Freeze mode after the main scene is complete
	//
	if (bFreezeEnable) bFreeze = true;

	
	// -------------------------------------------------------------------------------------------------------
	// EnvMap Debugger  TODO: Should be allowed to visualize other maps as well, not just index 0
	// -------------------------------------------------------------------------------------------------------

	if (DebugControls::IsActive()) {
		
		int sel = DebugControls::GetSelectedEnvMap();

		switch (sel) {
		case 1:		case 2:		case 3:		case 4:
		case 5:
			VisualizeCubeMap(vFocus->GetEnvMap(ENVMAP_MAIN), sel - 1);
			break;
		case 6:
			VisualizeCubeMap(vFocus->GetIrradEnv(), 0);
			break;
		case 7:
			VisualizeCubeMap(pIrradTemp, 0);
			break;
		case 8:
			if (pShdMap) {
				pSketch = GetPooledSketchpad(SKETCHPAD_2D_OVERLAY);
				pSketch->CopyRectNative(pShdMap, NULL, 0, 0);
				pSketch->EndDrawing();
			}
			break;
		case 9:
			if (vFocus->GetIrradianceMap()) {
				pSketch = GetPooledSketchpad(SKETCHPAD_2D_OVERLAY);
				pSketch->CopyRectNative(vFocus->GetIrradianceMap(), NULL, 0, 0);
				pSketch->EndDrawing();
			}
			break;
		case 10:
			if (ptgBuffer[GBUF_BLUR]) {
				pSketch = GetPooledSketchpad(SKETCHPAD_2D_OVERLAY);
				pSketch->CopyRectNative(ptgBuffer[GBUF_BLUR], NULL, 0, 0);
				pSketch->EndDrawing();
			}
			break;
		case 11:
			if (ptgBuffer[GBUF_DEPTH]) {
				if (pVisDepth) {
					if (pVisDepth->IsOK()) {
						pVisDepth->Activate("PSDepth");
						pVisDepth->SetTextureNative("tBack", ptgBuffer[GBUF_DEPTH], IPF_POINT | IPF_CLAMP);
						pVisDepth->SetOutputNative(0, gc->GetBackBuffer());
						pVisDepth->Execute(true);
					}
				}
			}
			break;
		case 12:
			if (ptgBuffer[GBUF_DEPTH]) {
				if (pVisDepth) {
					if (pVisDepth->IsOK()) {
						pVisDepth->Activate("PSNormal");
						pVisDepth->SetTextureNative("tBack", ptgBuffer[GBUF_DEPTH], IPF_POINT | IPF_CLAMP);
						pVisDepth->SetOutputNative(0, gc->GetBackBuffer());
						pVisDepth->Execute(true);
					}
				}
			}
			break;
		case 13:
			if (pLocalResults) {
				pSketch = GetPooledSketchpad(SKETCHPAD_2D_OVERLAY);
				pSketch->SetBlendState(Sketchpad::BlendState::FILTER_POINT);
				pSketch->StretchRectNative(pLocalResults, NULL, &_RECT(0, 0, viewW, 10));
				pSketch->SetBlendState(Sketchpad::BlendState::FILTER_LINEAR);
				pSketch->EndDrawing();
			}
			break;
		default:
			break;
		}
	}


	if (AtmoControls::Visualize())
	{
		vPlanet* vP = GetCameraProxyVisual();
		pSketch = GetPooledSketchpad(SKETCHPAD_2D_OVERLAY);
		pSketch->SetBlendState(Sketchpad::COPY);
		int x = 0, y = ViewH();

		LPDIRECT3DTEXTURE9 pTab = vP->GetScatterTable(RAY_LAND);
		D3DSURFACE_DESC desc;
		if (pTab) {
			pTab->GetLevelDesc(0, &desc);
			pSketch->StretchRectNative(pTab, NULL, &_R(0, y - desc.Height, desc.Width, y));
			y -= (desc.Height + 5);
		}
		pTab = vP->GetScatterTable(MIE_LAND);
		if (pTab) {
			pTab->GetLevelDesc(0, &desc);
			pSketch->StretchRectNative(pTab, NULL, &_R(0, y - desc.Height, desc.Width, y));
			y -= (desc.Height + 5);
		}
		pTab = vP->GetScatterTable(ATN_LAND);
		if (pTab) {
			pTab->GetLevelDesc(0, &desc);
			pSketch->StretchRectNative(pTab, NULL, &_R(0, y - desc.Height, desc.Width, y));
			y -= (desc.Height + 5);
		}
		for (int i=0;i<9;i++)
		{
			if (i == RAY_LAND || i == MIE_LAND || i == ATN_LAND) continue;
			pTab = vP->GetScatterTable(i);
			if (!pTab) continue;
			pTab->GetLevelDesc(0, &desc);
			pSketch->CopyRectNative(pTab, NULL, x, y - desc.Height);
			x += desc.Width + 5;
		}
		pSketch->SetBlendState(Sketchpad::ALPHABLEND);
		pSketch->EndDrawing();
	}


	// -------------------------------------------------------------------------------------------------------
	// Draw Debug String on a bottom of the screen
	// -------------------------------------------------------------------------------------------------------

	const char* dbgString = oapiDebugString();
	int len = lstrlen(dbgString);

	if (len>0 || !D3D9DebugQueue.empty()) {

		pSketch = GetPooledSketchpad(SKETCHPAD_DEBUG_TEXT);

		DWORD height = Config->DebugFontSize;

		// Display Orbiter's debug string
		if (len > 0) {
			DWORD width = pSketch->GetTextWidth(dbgString, len);
			pSketch->Rectangle(-1, viewH - height - 1, width + 4, viewH);
			pSketch->Text(2, viewH - 2, dbgString, len);
		}

		DWORD pos = viewH;

		// Display additional debug string queue
		//
		while (!D3D9DebugQueue.empty()) {
			pos -= (height * 3) / 2;
			std::string str = D3D9DebugQueue.front();
			len = lstrlen(str.c_str());
			DWORD width = pSketch->GetTextWidth(str.c_str(), len);
			pSketch->Rectangle(-1, pos - height - 1, width + 4, pos);
			pSketch->Text(2, pos - 2, str.c_str(), len);
			D3D9DebugQueue.pop();
		}

		pSketch->EndDrawing(); // SKETCHPAD_DEBUG_TEXT
	}


	gc->PopRenderTargets();	// Overlay
	gc->PopRenderTargets();	// Main Scene

	gc->HackFriendlyHack();
	gc->EndScene();

	dwTurn++;
}



// ===========================================================================================
//
void Scene::RenderVesselMarker(vVessel *vV, D3D9Pad *pSketch)
{
	DWORD mkrmode = *(DWORD*)gc->GetConfigParam(CFGPRM_SURFMARKERFLAG);
	if ((mkrmode & (MKR_ENABLE | MKR_VMARK)) == (MKR_ENABLE | MKR_VMARK)) {
		RenderObjectMarker(pSketch, vV->GlobalPos(), std::string(vV->GetName()), std::string(), 0, viewH / 80);
	}
}


// ===========================================================================================
// Lens flare code (SolarLiner)
//
Scene::SUNVISPARAMS Scene::GetSunScreenVisualState()
{
	SUNVISPARAMS result = SUNVISPARAMS();

	VECTOR3 cam = GetCameraGPos();
	VECTOR3 sunGPos;
	oapiGetGlobalPos(oapiGetGbodyByIndex(0), &sunGPos);
	sunGPos -= cam;

	DWORD w, h;
	oapiGetViewportSize(&w, &h);

	const LPD3DXMATRIX pVP = GetProjectionViewMatrix();
	D3DXVECTOR4 pos;
	D3DXVECTOR4 sun = D3DXVECTOR4(float(sunGPos.x), float(sunGPos.y), float(sunGPos.z), 1.0f);
	D3DXVec4Transform(&pos, &sun, pVP);
	result.brightness = saturate(pos.z);

	D3DXVECTOR2 scrPos = D3DXVECTOR2(pos.x, pos.y);
	scrPos /= pos.w;
	scrPos *= 0.5f;
	scrPos.x *= w / h;

	result.position = scrPos;
	result.position.x *= 1.8f;

	short xpos = short((scrPos.x + 0.5f) * w);
	short ypos = short((1.0f - (scrPos.y + 0.5f)) * h);

	D3D9Pick pick = PickScene(xpos, ypos);

	if (pick.pMesh != NULL)
	{
		DWORD matIndex = pick.pMesh->GetMeshGroupMaterialIdx(pick.group);
		D3D9MatExt material;
		pick.pMesh->GetMaterial(&material, matIndex);
		D3DXCOLOR surfCol = material.Diffuse;

		result.visible = (surfCol.a != 1.0f);
		if (result.visible)
		{
			D3DXCOLOR color = D3DXCOLOR(surfCol.r*surfCol.a, surfCol.g*surfCol.a, surfCol.b*surfCol.a, 1.0f);
			color += GetSunDiffColor() * (1 - surfCol.a);

			result.color = color;
		}
		return result;
	}
	result.visible = true;
	result.color = GetSunDiffColor();

	return result;
}


// ===========================================================================================
// Lens flare code (SolarLiner)
//
D3DXCOLOR Scene::GetSunDiffColor()
{
	vPlanet *vP = Camera.vProxy;

	D3DXVECTOR3 _one(1, 1, 1);
	VECTOR3 GS, GP, GO;
	oapiCameraGlobalPos(&GO);

	OBJHANDLE hS = oapiGetGbodyByIndex(0);	// the central star
	OBJHANDLE hP = vP->Object();			// the planet object
	oapiGetGlobalPos(hS, &GS);				// sun position
	oapiGetGlobalPos(hP, &GP);				// planet position

	VECTOR3 S = GS - GO;						// sun's position from object
	VECTOR3 P = GO - GP;

	double s = length(S);

	float pwr = 1.0f;

	if (hP == hS) return GetSun()->Color;

	double r = length(P);
	double pres = 1000.0;
	double size = oapiGetSize(hP) + vP->GetMinElevation();
	double grav = oapiGetMass(hP) * 6.67259e-11 / (size*size);

	float aalt = 1.0f;
	//float amb0 = 0.0f;
	float disp = 0.0f;
	float amb = 0.0f;
	float aq = 0.342f;
	float ae = 0.242f;
	float al = 0.0f;
	float k = float(sqrt(r*r - size*size));		// Horizon distance
	float alt = float(r - size);
	float rs = float(oapiGetSize(hS) / s);
	float ac = float(-dotp(S, P) / (r*s));					// sun elevation

															// Avoid some fault conditions
	if (alt<0) alt = 0, k = 1e3, size = r;

	if (ac>1.0f) ac = 1.0f; if (ac<-1.0f) ac = -1.0f;

	ac = acos(ac) - asin(float(size / r));

	if (ac>1.39f)  ac = 1.39f;
	if (ac<-1.39f) ac = -1.39f;

	float h = tan(ac);

	const ATMCONST *atm = (oapiGetObjectType(hP) == OBJTP_PLANET ? oapiGetPlanetAtmConstants(hP) : NULL);

	if (atm) {
		aalt = float(atm->p0 * log(atm->p0 / pres) / (atm->rho0*grav));
		//amb0 = float(min(0.7, log(atm->rho0 + 1.0f)*0.4));
		disp = float(max(0.02, min(0.9, log(atm->rho0 + 1.0))));
	}

	if (alt>10e3f) al = aalt / k;
	else           al = 0.173f;

	D3DXVECTOR3 lcol(1, 1, 1);
	//D3DXVECTOR3 r0 = _one - D3DXVECTOR3(0.65f, 0.75f, 1.0f) * disp;
	D3DXVECTOR3 r0 = _one - D3DXVECTOR3(1.15f, 1.65f, 2.35f) * disp;

	if (atm) {
		float x = sqrt(saturate(h / al));
		float y = sqrt(saturate((h + rs) / (2.0f*rs)));
		lcol = (r0 + (_one - r0) * x) * y;
	}
	else {
		lcol = r0 * saturate((h + rs) / (2.0f*rs));
	}

	return D3DXCOLOR(lcol.x, lcol.y, lcol.z, 1);
}



// ===========================================================================================
//
int Scene::RenderShadowMap(D3DXVECTOR3 &pos, D3DXVECTOR3 &ld, float rad, bool bInternal, bool bListExists)
{
	rad *= 1.02f;

	smap.pos = pos;
	smap.ld = ld;
	smap.rad = rad;

	float mnd =  1e16f;
	float mxd = -1e16f;
	float rsmax = 0.0f;
	float tanap = float(GetTanAp());
	float viewh = float(ViewH());

	if (!bListExists) {

		// If the list doesn't exists then create it...
		SmapRenderList.clear();

		// browse through vessels to find shadowers --------------------------
		//
		for (VOBJREC *pv = vobjFirst; pv; pv = pv->next) {
			if (pv->type != OBJTP_VESSEL) continue;
			vVessel *vV = (vVessel *)pv->vobj;
			if (!vV->IsActive()) continue;
			if (vV->IntersectShadowVolume()) {
				SmapRenderList.push_back(vV);
				vV->GetMinMaxLightDist(&mnd, &mxd);
			}
		}

		// Compute shadow lod
		rsmax = viewh * rad / (tanap * D3DXVec3Length(&pos));
	}


	if (SmapRenderList.size() == 0) return -1;	// The list is empty, Nothing to render


	if (bListExists) {

		for (auto vV : SmapRenderList)
		{
			// Get shadow min-max distances
			vV->GetMinMaxLightDist(&mnd, &mxd);

			// Compute shadow lod
			D3DXVECTOR3 bspos = vV->GetBoundingSpherePosDX();
			float rs = viewh * rad / (tanap * D3DXVec3Length(&bspos));
			if (rs > rsmax) rsmax = rs;
		}
	}

	smap.depth = (mxd - mnd) + 10.0f;

	D3DXMatrixOrthoOffCenterRH(&smap.mProj, -rad, rad, rad, -rad, 50.0f, 50.0f + smap.depth);

	smap.dist = mnd - 55.0f;

	D3DXVECTOR3 lp = pos + ld * smap.dist;

	D3DXMatrixLookAtRH(&smap.mView, &lp, &pos, &D3DXVECTOR3(0, 1, 0));
	D3DXMatrixMultiply(&smap.mViewProj, &smap.mView, &smap.mProj);

	float lod = log2f(float(Config->ShadowMapSize) / (rsmax*1.5f));

	smap.lod = min(int(round(lod)), SHM_LOD_COUNT - 1);
	smap.lod = max(smap.lod, 0);
	smap.size = Config->ShadowMapSize >> smap.lod;

	gc->PushRenderTarget(psShmRT[smap.lod], psShmDS[smap.lod], RENDERPASS_SHADOWMAP);

	// Clear the viewport
	HR(pDevice->Clear(0, NULL, D3DCLEAR_TARGET | D3DCLEAR_ZBUFFER, 0, 1.0f, 0L));


	// render the vessel objects --------------------------------
	//
	BeginPass(RENDERPASS_SHADOWMAP);

	while(SmapRenderList.size()>0) {
		SmapRenderList.front()->Render(pDevice, bInternal);
		SmapRenderList.pop_front();
	}

	PopPass();

	gc->PopRenderTargets();

	smap.pShadowMap = ptShmRT[smap.lod];

	return smap.lod;
}


// ===========================================================================================
//
void Scene::RenderSecondaryScene(std::set<vVessel*> &RndList, std::set<vVessel*> &LightsList, DWORD flags)
{
	_TRACE;
	RenderFlags = flags;

	// Process Local Light Sources -------------------------------------
	// And toggle external lights on
	//
	if (bLocalLight) {

		ClearLocalLights();

		for (auto vVes : RndList) {
			if (!vVes->IsActive()) continue;
			VESSEL *vessel = vVes->GetInterface();
			DWORD nemitter = vessel->LightEmitterCount();
			for (DWORD j = 0; j < nemitter; j++) {
				const LightEmitter *em = vessel->GetLightEmitter(j);
				if ((em->GetVisibility() == LightEmitter::VIS_EXTERNAL) || (em->GetVisibility() == LightEmitter::VIS_ALWAYS)) AddLocalLight(em, vVes);
			}		
		}

		for (auto vVes : LightsList) {
			if (!vVes->IsActive()) continue;
			if (RndList.count(vVes)) continue; // Already included skip it
			VESSEL *vessel = vVes->GetInterface();
			DWORD nemitter = vessel->LightEmitterCount();
			for (DWORD j = 0; j < nemitter; j++) {
				const LightEmitter *em = vessel->GetLightEmitter(j);
				if ((em->GetVisibility() == LightEmitter::VIS_EXTERNAL) || (em->GetVisibility() == LightEmitter::VIS_ALWAYS)) AddLocalLight(em, vVes);
			}
		}
	}

	D3D9Effect::UpdateEffectCamera(GetCameraProxyBody());

	// Clear the viewport
	HR(pDevice->Clear(0, NULL, D3DCLEAR_TARGET | D3DCLEAR_ZBUFFER | D3DCLEAR_STENCIL, 0xFF000000, 1.0f, 0L));

	
	// render planets -------------------------------------------
	//
	if (flags & 0x01) {
		for (DWORD i = 0; i<nplanets; i++) {
			bool isActive = plist[i].vo->IsActive();
			if (isActive) plist[i].vo->Render(pDevice);
			else		  plist[i].vo->RenderDot(pDevice);
		}
	}

	// render the vessel objects --------------------------------
	//
	if (flags & 0x02) {
		for (auto vVes : RndList) {
			if (!vVes->IsActive()) continue;
			if (!vVes->IsVisible()) continue;
			vVes->Render(pDevice);
		}
	}

	// render exhausts -------------------------------------------
	//
	if (flags & 0x04) {
		for (auto vVes : RndList) {
			if (!vVes->IsActive()) continue;
			if (!vVes->IsVisible()) continue;
			vVes->RenderExhaust();
		}
	}

	// render beacons -------------------------------------------
	//
	if (flags & 0x08) {
		for (auto vVes : RndList) {
			if (!vVes->IsActive()) continue;
			vVes->RenderBeacons(pDevice);
		}
	}

	// render exhaust particle system ----------------------------
	if (flags & 0x10) {
		for (DWORD n = 0; n < nstream; n++) pstream[n]->Render(pDevice);
	}

	// Flags 0x20 = BaseStructures
}


// ===========================================================================================
//
bool Scene::RenderBlurredMap(LPDIRECT3DDEVICE9 pDev, LPDIRECT3DCUBETEXTURE9 pSrc)
{
	bool bQuality = true;

	if (!pSrc) return false;

	if (!pBlur) {
		pBlur = new ImageProcessing(pDev, "Modules/D3D9Client/EnvMapBlur.hlsl", "PSBlur");
	}

	if (!pBlur->IsOK()) {
		LogErr("pBlur is not OK");
		return false;
	}

	if (!pEnvDS) {
		LogErr("EnvDepthStencil doesn't exists");
		return false;
	}

	D3DSURFACE_DESC desc;
	pEnvDS->GetDesc(&desc);
	DWORD width = min(512, desc.Width);


	if (!pBlrTemp[0]) {
		if (D3DXCreateCubeTexture(pDev, width >> 0, 1, D3DUSAGE_RENDERTARGET, D3DFMT_X8R8G8B8, D3DPOOL_DEFAULT, &pBlrTemp[0]) != S_OK) return false;
		if (D3DXCreateCubeTexture(pDev, width >> 1, 1, D3DUSAGE_RENDERTARGET, D3DFMT_X8R8G8B8, D3DPOOL_DEFAULT, &pBlrTemp[1]) != S_OK) return false;
		if (D3DXCreateCubeTexture(pDev, width >> 2, 1, D3DUSAGE_RENDERTARGET, D3DFMT_X8R8G8B8, D3DPOOL_DEFAULT, &pBlrTemp[2]) != S_OK) return false;
		if (D3DXCreateCubeTexture(pDev, width >> 3, 1, D3DUSAGE_RENDERTARGET, D3DFMT_X8R8G8B8, D3DPOOL_DEFAULT, &pBlrTemp[3]) != S_OK) return false;
		if (D3DXCreateCubeTexture(pDev, width >> 4, 1, D3DUSAGE_RENDERTARGET, D3DFMT_X8R8G8B8, D3DPOOL_DEFAULT, &pBlrTemp[4]) != S_OK) return false;
	}


	D3DXVECTOR3 dir, up, cp;
	LPDIRECT3DSURFACE9 pSrf = NULL;
	LPDIRECT3DSURFACE9 pTmp = NULL;

	// Create clurred mip sub-levels
	//
	for (DWORD i = 0; i < 6; i++) {
		pSrc->GetCubeMapSurface(D3DCUBEMAP_FACES(i), 0, &pSrf);
		pBlrTemp[0]->GetCubeMapSurface(D3DCUBEMAP_FACES(i), 0, &pTmp);
		pDevice->StretchRect(pSrf, NULL, pTmp, NULL, D3DTEXF_POINT);
		SAFE_RELEASE(pSrf);
		SAFE_RELEASE(pTmp);
	}


	// Create clurred mip sub-levels
	//
	for (int mip = 1; mip < 5; mip++) {

		pBlur->SetFloat("fD", (4.0f / float(256 >> (mip - 1))));
		pBlur->SetBool("bDir", false);
		pBlur->SetTextureNative("tCube", pBlrTemp[mip-1], IPF_LINEAR);

		for (DWORD i = 0; i < 6; i++) {

			EnvMapDirection(i, &dir, &up);
			D3DXVec3Cross(&cp, &up, &dir);
			D3DXVec3Normalize(&cp, &cp);

			pSrc->GetCubeMapSurface(D3DCUBEMAP_FACES(i), mip, &pSrf);

			pBlur->SetOutputNative(0, pSrf);
			pBlur->SetFloat("vDir", &dir, sizeof(D3DXVECTOR3));
			pBlur->SetFloat("vUp", &up, sizeof(D3DXVECTOR3));
			pBlur->SetFloat("vCp", &cp, sizeof(D3DXVECTOR3));

			if (!pBlur->Execute(true)) {
				LogErr("pBlur Execute Failed");
				return false;
			}

			pBlrTemp[mip-1]->GetCubeMapSurface(D3DCUBEMAP_FACES(i), 0, &pTmp);
			pDevice->StretchRect(pSrf, NULL, pTmp, NULL, D3DTEXF_POINT);
			SAFE_RELEASE(pSrf);
			SAFE_RELEASE(pTmp);
		}

		pBlur->SetBool("bDir", true);

		for (DWORD i = 0; i < 6; i++) {

			EnvMapDirection(i, &dir, &up);
			D3DXVec3Cross(&cp, &up, &dir);
			D3DXVec3Normalize(&cp, &cp);

			pSrc->GetCubeMapSurface(D3DCUBEMAP_FACES(i), mip, &pSrf);

			pBlur->SetOutputNative(0, pSrf);
			pBlur->SetFloat("vDir", &dir, sizeof(D3DXVECTOR3));
			pBlur->SetFloat("vUp", &up, sizeof(D3DXVECTOR3));
			pBlur->SetFloat("vCp", &cp, sizeof(D3DXVECTOR3));

			if (!pBlur->Execute(true)) {
				LogErr("pBlur Execute Failed");
				return false;
			}

			pBlrTemp[mip]->GetCubeMapSurface(D3DCUBEMAP_FACES(i), 0, &pTmp);
			pDevice->StretchRect(pSrf, NULL, pTmp, NULL, D3DTEXF_POINT);
			SAFE_RELEASE(pSrf);
			SAFE_RELEASE(pTmp);
		}
	}

	return true;
}

// ===========================================================================================
//
bool Scene::IntegrateIrradiance(vVessel *vV, LPDIRECT3DCUBETEXTURE9 pSrc, LPDIRECT3DTEXTURE9 pOut)
{
	if (!pSrc) return false;

	if (!pIrradiance) {
		pIrradiance = new ImageProcessing(pDevice, "Modules/D3D9Client/IrradianceInteg.hlsl", "PSPreInteg");
		pIrradiance->CompileShader("PSInteg");
		pIrradiance->CompileShader("PSPostBlur");
	}

	if (!pIrradiance->IsOK()) {
		LogErr("pIrradiance is not OK");
		return false;
	}

	if (!pIrradDS) {
		LogErr("pIrradDS doesn't exists");
		return false;
	}

	LPDIRECT3DSURFACE9 pOuts = NULL;
	HR(pOut->GetSurfaceLevel(0, &pOuts));

	D3DSURFACE_DESC desc, desc_out;
	pIrradDS->GetDesc(&desc);
	pOuts->GetDesc(&desc_out);
	
	if (!pIrradTemp) {
		if (D3DXCreateCubeTexture(pDevice, 16, 1, D3DUSAGE_RENDERTARGET, D3DFMT_A16B16G16R16F, D3DPOOL_DEFAULT, &pIrradTemp) != S_OK) {
			LogErr("Failed to create irradiance temp");
			return false;
		}
		if (D3DXCreateTexture(pDevice, 128, 128, 1, D3DUSAGE_RENDERTARGET, D3DFMT_A16B16G16R16F, D3DPOOL_DEFAULT, &pIrradTemp2) != S_OK) {
			LogErr("Failed to create irradiance temp");
			return false;
		}
		if (D3DXCreateTexture(pDevice, desc_out.Width, desc_out.Height, 1, D3DUSAGE_RENDERTARGET, D3DFMT_A16B16G16R16F, D3DPOOL_DEFAULT, &pIrradTemp3) != S_OK) {
			LogErr("Failed to create irradiance temp");
			return false;
		}
	}

	D3DXVECTOR3 nr, up, cp;
	LPDIRECT3DSURFACE9 pSrf = NULL;
	LPDIRECT3DSURFACE9 pTgt = NULL;
	LPDIRECT3DSURFACE9 pTmp2 = NULL;
	LPDIRECT3DSURFACE9 pTmp3 = NULL;
	
	HR(pIrradTemp2->GetSurfaceLevel(0, &pTmp2));
	HR(pIrradTemp3->GetSurfaceLevel(0, &pTmp3));


	// ---------------------------------------------------------------------
	// Pre-Integrate Irradiance Cube
	//
	pIrradiance->Activate("PSPreInteg");
	pIrradiance->SetFloat("fD", &D3DXVECTOR2(1.0f / float(desc.Width), 1.0f / float(desc.Height)), sizeof(D3DXVECTOR2));

	for (DWORD i = 0; i < 6; i++)
	{
		pSrc->GetCubeMapSurface(D3DCUBEMAP_FACES(i), 0, &pSrf);
		pIrradTemp->GetCubeMapSurface(D3DCUBEMAP_FACES(i), 0, &pTgt);
		
		pDevice->StretchRect(pSrf, NULL, pTmp2, NULL, D3DTEXF_POINT);

		pIrradiance->SetOutputNative(0, pTgt);
		pIrradiance->SetTextureNative("tSrc", pIrradTemp2, IPF_POINT | IPF_CLAMP);

		if (!pIrradiance->Execute(true)) {
			LogErr("pIrradiance Execute Failed");
			return false;
		}

		SAFE_RELEASE(pTgt);
		SAFE_RELEASE(pSrf);
	}

	


	// ---------------------------------------------------------------------
	// Main Integration
	//
	GetLVLH(vV, &up, &nr, &cp);

	float Glow = float(Config->PlanetGlow);

	pIrradiance->Activate("PSInteg");
	pIrradiance->SetOutputNative(0, pTmp3);
	pIrradiance->SetTextureNative("tCube", pIrradTemp, IPF_LINEAR);
	pIrradiance->SetFloat("Kernel", IKernel, sizeof(IKernel));
	pIrradiance->SetFloat("vNr", &nr, sizeof(D3DXVECTOR3));
	pIrradiance->SetFloat("vUp", &up, sizeof(D3DXVECTOR3));
	pIrradiance->SetFloat("vCp", &cp, sizeof(D3DXVECTOR3));
	pIrradiance->SetFloat("fIntensity", &Glow, sizeof(float));
	pIrradiance->SetBool("bUp", false);
	pIrradiance->SetTemplate(0.5f, 1.0f, 0.0f, 0.0f);

	if (!pIrradiance->Execute(true)) {
		LogErr("pIrradiance Execute Failed");
		return false;
	}

	pIrradiance->SetBool("bUp", true);
	pIrradiance->SetTemplate(0.5f, 1.0f, 0.5f, 0.0f);

	if (!pIrradiance->Execute(true)) {
		LogErr("pIrradiance Execute Failed");
		return false;
	}

	// ---------------------------------------------------------------------
	// Post Blur
	//
	pIrradiance->Activate("PSPostBlur");
	pIrradiance->SetFloat("fD", &D3DXVECTOR2(1.0f / float(desc_out.Width), 1.0f / float(desc_out.Height)), sizeof(D3DXVECTOR2));
	pIrradiance->SetOutputNative(0, pOuts);
	pIrradiance->SetTextureNative("tSrc", pIrradTemp3, IPF_POINT | IPF_WRAP);

	if (!pIrradiance->Execute(true)) {
		LogErr("pIrradiance Execute Failed");
		return false;
	}

	SAFE_RELEASE(pTmp2);
	SAFE_RELEASE(pTmp3);
	SAFE_RELEASE(pOuts);

	return true;
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
void Scene::VisualizeCubeMap(LPDIRECT3DCUBETEXTURE9 pCube, int mip)
{
	if (!pCube) return;

	LPDIRECT3DSURFACE9 pSrf = NULL;
	LPDIRECT3DSURFACE9 pBack = gc->GetBackBuffer();

	D3DSURFACE_DESC bdesc;

	if (!pBack) return;

	HR(pBack->GetDesc(&bdesc));

	DWORD x, y, h = bdesc.Height / 3;

	for (DWORD i=0;i<6;i++) {

		HR(pCube->GetCubeMapSurface(D3DCUBEMAP_FACES(i), mip, &pSrf));

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

		HR(pDevice->StretchRect(pSrf, NULL, pBack, &dr, D3DTEXF_POINT));

		SAFE_RELEASE(pSrf);
	}
}



// ===========================================================================================
//
void Scene::RenderVesselShadows (OBJHANDLE hPlanet, float depth) const
{
	// If this planet is not a proxy body skip the rest
	if (hPlanet != oapiCameraProxyGbody()) return;

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
void Scene::RenderMesh(DEVMESHHANDLE hMesh, const oapi::FMATRIX4 *pWorld)
{
	D3D9Mesh *pMesh = (D3D9Mesh *)hMesh;

	const Scene::SHADOWMAPPARAM *shd = GetSMapData();

	float s = float(shd->size);
	float sr = 2.0f * shd->rad / s;

	HR(D3D9Effect::FX->SetMatrix(D3D9Effect::eLVP, &shd->mViewProj));

	if (shd->pShadowMap) {
		HR(D3D9Effect::FX->SetTexture(D3D9Effect::eShadowMap, shd->pShadowMap));
		HR(D3D9Effect::FX->SetVector(D3D9Effect::eSHD, &D3DXVECTOR4(sr, 1.0f / s, float(oapiRand()), 1.0f / shd->depth)));
		HR(D3D9Effect::FX->SetBool(D3D9Effect::eShadowToggle, true));
	}
	else {
		HR(D3D9Effect::FX->SetBool(D3D9Effect::eShadowToggle, false));
	}

	pMesh->SetSunLight(&sunLight);
	pMesh->RenderSimplified(LPD3DXMATRIX(pWorld));
}


// ===========================================================================================
//
bool Scene::WorldToScreenSpace(const VECTOR3 &wpos, oapi::IVECTOR2 *pt, D3DXMATRIX *pVP, float clip)
{
	D3DXVECTOR4 homog;
	D3DXVECTOR3 pos(float(wpos.x), float(wpos.y), float(wpos.z));

	if (pVP) D3DXVec3Transform(&homog, &pos, pVP);
	else D3DXVec3Transform(&homog, &pos, GetProjectionViewMatrix());

	if (homog.w < 0.0f) return false;

	homog.x /= homog.w;
	homog.y /= homog.w;

	bool bClip = false;
	if (homog.x < -clip || homog.x > clip || homog.y < -clip || homog.y > clip) bClip = true;

	if (_hypot(homog.x, homog.y) < 1e-6) {
		pt->x = viewW / 2;
		pt->y = viewH / 2;
	}
	else {
		pt->x = (long)((float(viewW) * 0.5f * (1.0f + homog.x)) + 0.5f);
		pt->y = (long)((float(viewH) * 0.5f * (1.0f - homog.y)) + 0.5f);
	}

	return !bClip;
}


// ===========================================================================================
//
bool Scene::WorldToScreenSpace2(const VECTOR3& wpos, oapi::FVECTOR2* pt, D3DXMATRIX* pVP, float clip)
{
	D3DXVECTOR4 homog;
	D3DXVECTOR3 pos(float(wpos.x), float(wpos.y), float(wpos.z));

	if (pVP) D3DXVec3Transform(&homog, &pos, pVP);
	else D3DXVec3Transform(&homog, &pos, GetProjectionViewMatrix());

	homog.x /= homog.w;
	homog.y /= homog.w;

	bool bClip = false;
	if (homog.w < 0.0f) bClip = true;
	if (homog.x < -clip || homog.x > clip || homog.y < -clip || homog.y > clip) bClip = true;

	if (_hypot(homog.x, homog.y) < 1e-6) {
		pt->x = viewW / 2;
		pt->y = viewH / 2;
	}
	else {
		pt->x = (float(viewW) * 0.5f * (1.0f + homog.x)) + 0.5f;
		pt->y = (float(viewH) * 0.5f * (1.0f - homog.y)) + 0.5f;
	}

	return !bClip;
}

// ===========================================================================================
//
void Scene::RenderObjectMarker(oapi::Sketchpad *pSkp, const VECTOR3 &gpos, const std::string& label1, const std::string& label2, int mode, int scale)
{
	VECTOR3 dp (gpos - GetCameraGPos());
	normalise (dp);
	m_celSphere->RenderMarker(pSkp, dp, label1, label2, mode, scale);
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
		memcpy (tmp, pstream, nstream*sizeof(D3D9ParticleStream*));
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
	pAxisFont  = oapiCreateFont(24, false, "Arial", FONT_NORMAL, 0);
	pLabelFont = oapiCreateFont(15, false, "Arial", FONT_NORMAL, 0);
	pDebugFont = oapiCreateFont(Config->DebugFontSize, true, dbgfnt, FONT_NORMAL, 0);

	const int fsize[4] = { 12, 16, 20, 26 };
	for (int i = 0; i < 4; ++i) {
		label_font[i] = gc->clbkCreateFont(fsize[i], true, "Arial", FONT_BOLD);
	}
	//@todo: different pens for different fonts?
}

// ===========================================================================================
//
void Scene::ExitGDIResources ()
{
	oapiReleaseFont(pAxisFont);
	oapiReleaseFont(pLabelFont);
	oapiReleaseFont(pDebugFont);

	for (int i = 0; i < 4; ++i) {
		gc->clbkReleaseFont(label_font[i]);
	}
}

// ===========================================================================================
//
float Scene::GetDepthResolution(float dist) const
{
	return fabs( (Camera.nearplane-Camera.farplane)*(dist*dist) / (Camera.farplane * Camera.nearplane * 16777215.0f) );
}

// ===========================================================================================
//
float Scene::CameraInSpace() const
{
	if (Camera.vProxy) {
		if (Camera.vProxy->HasAtmosphere()) {
			ConstParams* cp = Camera.vProxy->GetScatterConst();
			if (cp)	return 1.0f - exp(-cp->CamAlt * cp->iH.x);
		}
	}
	return 1.0f;
}

// ===========================================================================================
//
void Scene::GetCameraLngLat(double *lng, double *lat) const
{
	if (lng) *lng = Camera.lng;
	if (lat) *lat = Camera.lat;
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
FMATRIX4 Scene::PushCameraFrustumLimits(float nearlimit, float farlimit)
{
	FRUSTUM fr = { Camera.nearplane, Camera.farplane };
	FrustumStack.push(fr);
	SetCameraFrustumLimits(nearlimit, farlimit);
	return FMATRIX4(GetProjectionViewMatrix());
}

// ===========================================================================================
//
FMATRIX4 Scene::PopCameraFrustumLimits()
{
	SetCameraFrustumLimits(FrustumStack.top().znear, FrustumStack.top().zfar);
	FrustumStack.pop();
	return FMATRIX4(GetProjectionViewMatrix());
}

// ===========================================================================================
//
void Scene::BeginPass(DWORD dwPass)
{
	PassStack.push(dwPass);
}

// ===========================================================================================
//
void Scene::PopPass()
{
	PassStack.pop();
}

// ===========================================================================================
//
DWORD Scene::GetRenderPass() const
{
	if (PassStack.empty()) return RENDERPASS_MAINSCENE;
	return PassStack.top();
}

// ===========================================================================================
//
D3DXVECTOR3 Scene::GetPickingRay(short xpos, short ypos)
{
	float x = 2.0f*float(xpos) / float(ViewW()) - 1.0f;
	float y = 2.0f*float(ypos) / float(ViewH()) - 1.0f;
	D3DXVECTOR3 vPick = Camera.x * (x / Camera.mProj._11) + Camera.y * (-y / Camera.mProj._22) + Camera.z;
	D3DXVec3Normalize(&vPick, &vPick);
	return vPick;
}

// ===========================================================================================
//
TILEPICK Scene::PickSurface(short xpos, short ypos)
{
	TILEPICK tp; memset(&tp, 0, sizeof(TILEPICK));
	vPlanet *vp = GetCameraProxyVisual();
	if (!vp) return tp;
	D3DXVECTOR3 vRay = GetPickingRay(xpos, ypos);
	vp->PickSurface(vRay, &tp);
	return tp;
}

// ===========================================================================================
//
D3D9Pick Scene::PickScene(short xpos, short ypos)
{
	D3DXVECTOR3 vPick = GetPickingRay(xpos, ypos);

	D3D9Pick result;
	result.dist  = 1e30f;
	result.pMesh = NULL;
	result.vObj  = NULL;
	result.group = -1;
	result.idx = -1;

	for (VOBJREC *pv=vobjFirst; pv; pv=pv->next) {

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
D3D9Pick Scene::PickMesh(DEVMESHHANDLE hMesh, const LPD3DXMATRIX pW, short xpos, short ypos)
{
	D3D9Mesh *pMesh = (D3D9Mesh *)hMesh;
	return pMesh->Pick(pW, NULL, &GetPickingRay(xpos, ypos));
}

// ===========================================================================================
//
void Scene::GetAdjProjViewMatrix(LPD3DXMATRIX pMP, float znear, float zfar)
{
	float tanap = tan(Camera.aperture);
	ZeroMemory(pMP, sizeof(D3DXMATRIX));
	pMP->_11 = (Camera.aspect / tanap);
	pMP->_22 = (1.0f / tanap);
	pMP->_43 = (pMP->_33 = zfar / (zfar - znear)) * (-znear);
	pMP->_34 = 1.0f;
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
	Camera.mProj._22 = (1.0f / tanap);
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
bool Scene::IsProxyMesh()
{
	return Camera.vProxy ? Camera.vProxy->IsMesh() : false;
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

	for (VOBJREC *pv = vobjFirst; pv; pv = pv->next) pv->vobj->Update(true);

	SetupInternalCamera(&Camera.mView, NULL, oapiCameraAperture(), double(viewH)/double(viewW));
}



// ===========================================================================================
//
void Scene::SetupInternalCamera(D3DXMATRIX *mNew, VECTOR3 *gpos, double apr, double asp)
{

	// Update camera orientation if a new matrix is provided
	if (mNew) {
		Camera.mView	  = *mNew;
		Camera.x   = D3DXVECTOR3(Camera.mView._11, Camera.mView._21, Camera.mView._31);
		Camera.y   = D3DXVECTOR3(Camera.mView._12, Camera.mView._22, Camera.mView._32);
		Camera.z   = D3DXVECTOR3(Camera.mView._13, Camera.mView._23, Camera.mView._33);
		Camera.dir = _VD3DX(Camera.z);
	}

	if (gpos) Camera.pos = *gpos;

	Camera.upos = D3DXVEC(unit(Camera.pos));

	// find a logical reference body
	Camera.hObj_proxy = oapiCameraProxyGbody();
	Camera.hNear = NULL;

	// find the planet closest to the current camera position
	double closest = 1e32;
	int n = oapiGetGbodyCount();
	for (int i = 0; i < n; i++) {
		VECTOR3 gp; OBJHANDLE hB = oapiGetGbodyByIndex(i);
		oapiGetGlobalPos(hB, &gp);
		double l = length(gp - Camera.pos);
		if (l < closest) {
			closest = l;
			Camera.hNear = hB;
		}	
	}

	if (Camera.hNear) {
		// If the near body is not visible enough, switch to proxy.
		double apr = oapiGetSize(Camera.hNear) / closest;
		if (apr < 4e-3) Camera.hNear = Camera.hObj_proxy;
	}

	// find the visual
	Camera.vProxy = (vPlanet *)GetVisObject(Camera.hObj_proxy);
	Camera.vNear = (vPlanet *)GetVisObject(Camera.hNear);

	// Something is very wrong... abort...
	if (Camera.hObj_proxy == NULL || Camera.vProxy == NULL || Camera.vNear == NULL) return;

	// Camera altitude over the proxy
	VECTOR3 pos; MATRIX3 grot; double rad;
	oapiGetGlobalPos(Camera.hObj_proxy, &pos);
	oapiGetRotationMatrix(Camera.hObj_proxy, &grot);

	oapiLocalToEqu(Camera.hObj_proxy, tmul(grot, Camera.pos - pos), &Camera.lng, &Camera.lat, &rad);

	Camera.alt_proxy = dist(Camera.pos, pos) - oapiGetSize(Camera.hObj_proxy);
	Camera.vProxy->GetElevation(Camera.lng, Camera.lat, &rad);
	Camera.elev = Camera.alt_proxy - rad;

	// Camera altitude over the proxy
	oapiGetGlobalPos(Camera.hNear, &pos);
	Camera.alt_near = dist(Camera.pos, pos) - oapiGetSize(Camera.hNear);

	// Call SetCameraAparture to update ViewProj Matrix
	SetCameraAperture(float(apr), float(asp));

	// Finally update world matrices from all visuals
	//
	if (gpos) for (VOBJREC *pv = vobjFirst; pv; pv = pv->next) pv->vobj->ReOrigin(Camera.pos);
}




// ===========================================================================================
// CUSTOM CAMERA INTERFACE
// ===========================================================================================

int Scene::DeleteCustomCamera(CAMERAHANDLE hCam)
{
	if (!hCam) return 0;
	int iError = CAMERA(hCam)->iError;
	CustomCams.erase(CAMERA(hCam));
	delete CAMERA(hCam);
	camCurrent = CustomCams.cbegin();
	return iError;
}

// ===========================================================================================
//
void Scene::DeleteAllCustomCameras()
{
	for (auto x : CustomCams) delete CAMERA(x);
	CustomCams.clear();
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
		pv = new CAMREC; memset(pv, 0, sizeof(CAMREC));
		CustomCams.insert(pv);
		camCurrent = CustomCams.cbegin();
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
	CAMERA(hCamera)->bActive = bOn;
}

// ===========================================================================================
//
void Scene::RenderCustomCameraView(CAMREC *cCur)
{
	VESSEL *pVes = oapiGetVesselInterface(cCur->hVessel);

	DWORD w = SURFACE(cCur->hSurface)->GetWidth();
	DWORD h = SURFACE(cCur->hSurface)->GetHeight();

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

	PushCamera();

	SetCameraFrustumLimits(0.1, 2e7);
	SetupInternalCamera(&mEnv, &gpos, cCur->dAperture, double(h)/double(w));

	
	VOBJREC *pv = NULL;
	std::set<vVessel*> List;
	std::set<vVessel*> Lights;

	for (pv = vobjFirst; pv; pv = pv->next) if (pv->type == OBJTP_VESSEL) List.insert((vVessel *)pv->vobj);
	
	BeginPass(RENDERPASS_CUSTOMCAM);

	gc->PushRenderTarget(pSrf, pDSs, RENDERPASS_CUSTOMCAM);

	RenderSecondaryScene(List, Lights, 0xFF);

	gc->PopRenderTargets();

	PopPass();
	PopCamera();
}


// ===========================================================================================
//
void Scene::RenderGlares()
{
	// -------------------------------------------------------------------------------------------------------
	// Render glares for the Sun and local lights
	// -------------------------------------------------------------------------------------------------------

	if (pRenderGlares && pLocalResultsSL)
	{
		static SMVERTEX Vertex[4] = { {-1, -1, 0, 0, 0}, {-1, 1, 0, 0, 1}, {1, 1, 0, 1, 1}, {1, -1, 0, 1, 0} };
		static WORD cIndex[6] = { 0, 2, 1, 0, 3, 2 };
		D3DSURFACE_DESC desc; FVECTOR2 pt;
		struct { D3DXMATRIX	mVP; float4	Pos, Color;	float GPUId, Alpha, Blend; } Const;

		Const.Color = FVECTOR4(1, 1, 1, 1);
		D3DXMatrixOrthoOffCenterLH(&Const.mVP, 0.0f, (float)viewW, (float)viewH, 0.0f, 0.0f, 1.0f);
		pLocalResultsSL->GetDesc(&desc);

		pRenderGlares->ClearTextures();
		pRenderGlares->Setup(pPosTexDecl, false, 1);
		pRenderGlares->SetTextureVS("tVis", pLocalResults, IPF_CLAMP | IPF_POINT); // Set texture containing pre-cumputed visibility factors

		if (Config->bGlares && pSunGlare)
		{
			pRenderGlares->SetTexture("tTex0", pSunGlare, IPF_CLAMP | IPF_LINEAR);
			pRenderGlares->UpdateTextures();

			// Render Sun glare
			VECTOR3 gsun; oapiGetGlobalPos(oapiGetObjectByIndex(0), &gsun);
			double sdst = length(gsun - Camera.pos);
			VECTOR3 pos = (gsun - Camera.pos) * 10e4 / sdst;

			if (WorldToScreenSpace2(pos, &pt))
			{
				float cis = 1.0f, glare = float(Config->GFXGlare) * saturate(8.0 * AU / sdst);
				FVECTOR4 clr = FVECTOR4(1, 1, 1, 1);

				vPlanet* vp = GetCameraNearVisual();
			
				if (vp && vp->IsActive())
				{
					VECTOR3 crp = vp->CameraPos();
					clr = vp->SunLightColor(crp, 2.0);
					cis = CameraInSpace();
					glare *= pow(clr.MaxRGB(), 0.33f) * cis;				
				}
							
				float cd = length(pt - FVECTOR2(viewW, viewH) * 0.5f) / float(viewW); // Glare distance from a screen center
				float alpha = 2.0f * glare * max(0.5f, 1.0f - cd);
				float size = 300.0f * GetDisplayScale() * pow(alpha, 0.25f);

				Const.GPUId = 0.5f / float(desc.Width);
				Const.Pos = FVECTOR4(pt.x, pt.y, size, size);
				Const.Color.rgb = clr.rgb / (clr.MaxRGB() + 0.0001f);
				Const.Alpha = alpha * 2.0f;
				Const.Blend = sqrt(cis);

				pRenderGlares->SetVSConstants("Const", &Const, sizeof(Const));
				pRenderGlares->SetPSConstants("Const", &Const, sizeof(Const));
				HR(pDevice->DrawIndexedPrimitiveUP(D3DPT_TRIANGLELIST, 0, 4, 2, &cIndex, D3DFMT_INDEX16, &Vertex, sizeof(SMVERTEX)));		
			}
		}

		if (Config->bLocalGlares && pLightGlare)
		{
			pRenderGlares->SetTexture("tTex0", pLightGlare, IPF_CLAMP | IPF_LINEAR);
			pRenderGlares->UpdateTextures();

			// Render glares for local lights
			for (int i = 0; i < nLights; ++i) {
				int GPUId = Lights[i].GPUId;
				if (GPUId >= 0) {
					if (WorldToScreenSpace2(_V(Lights[i].Position), &pt)) {
						float size = 40.0f;
						Const.GPUId = (float(GPUId) + 0.5f) / desc.Width;
						Const.Pos = FVECTOR4(pt.x, pt.y, size, size);
						Const.Alpha = Lights[i].cone;
						Const.Color = Lights[i].Diffuse;
						Const.Blend = 1.0f;
						pRenderGlares->SetVSConstants("Const", &Const, sizeof(Const));
						pRenderGlares->SetPSConstants("Const", &Const, sizeof(Const));
						HR(pDevice->DrawIndexedPrimitiveUP(D3DPT_TRIANGLELIST, 0, 4, 2, &cIndex, D3DFMT_INDEX16, &Vertex, sizeof(SMVERTEX)));
					}
				}
			}
		}
		pRenderGlares->DetachTextures();
	}
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
bool Scene::CameraDirection2Viewport(const VECTOR3 &dir, int &x, int &y)
{
	D3DXVECTOR3 homog;
	D3DXVECTOR3 idir = D3DXVECTOR3( -float(dir.x), -float(dir.y), -float(dir.z) );
	D3DMAT_VectorMatrixMultiply(&homog, &idir, &Camera.mProjView);
	if (homog.x >= -1.0f && homog.y <= 1.0f && homog.z >= 0.0) {
		if (_hypot(homog.x, homog.y) < 1e-6) {
			x = viewW / 2, y = viewH / 2;
		} else {
			x = (int)(viewW*0.5f*(1.0f + homog.x));
			y = (int)(viewH*0.5f*(1.0f - homog.y));
		}
		return true;
	}
	return false;
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

	D3D9CelestialSphere::D3D9TechInit(FX);
}

// ===========================================================================================
//
int distcomp (const void *arg1, const void *arg2)
{
	double d1 = ((PList*)arg1)->dist;
	double d2 = ((PList*)arg2)->dist;
	return (d1 > d2 ? -1 : d1 < d2 ? 1 : 0);
}
