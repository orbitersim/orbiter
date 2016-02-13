// ===========================================================================================
// D3D9Effect.cpp
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2011 - 2016 Jarmo Nikkanen
// ===========================================================================================

#include "D3D9Effect.h"
#include "Log.h"
#include "Scene.h"
#include "D3D9Surface.h"
#include "D3D9Config.h"
#include "Mesh.h"
#include "VectorHelpers.h"

D3D9Mesh * D3D9Effect::hArrow = 0;
D3D9Client  * D3D9Effect::gc = 0;
ID3DXEffect * D3D9Effect::FX = 0;
LPDIRECT3DVERTEXBUFFER9 D3D9Effect::VB = 0;
LPDIRECT3DTEXTURE9 D3D9Effect::pNoise = 0;
SURFHANDLE D3D9Effect::hNoise = 0;

D3D9MatExt D3D9Effect::defmat;
D3D9MatExt D3D9Effect::night_mat;
D3D9MatExt D3D9Effect::emissive_mat;

// Some general rendering techniques
D3DXHANDLE D3D9Effect::ePanelTech = 0;		// Used to draw a new style 2D panel
D3DXHANDLE D3D9Effect::ePanelTechB = 0;		// Used to draw a new style 2D panel
D3DXHANDLE D3D9Effect::eVesselTech = 0;		// Vessel exterior, surface bases.
D3DXHANDLE D3D9Effect::eBBTech = 0;			// Bounding Box Tech
D3DXHANDLE D3D9Effect::eBSTech = 0;			// Bounding Sphere Tech
D3DXHANDLE D3D9Effect::eSimple = 0;
D3DXHANDLE D3D9Effect::eBaseShadowTech = 0;	// Used to draw transparent surface without texture 
D3DXHANDLE D3D9Effect::eBeaconArrayTech = 0;
D3DXHANDLE D3D9Effect::eExhaust = 0;	// Render engine exhaust texture
D3DXHANDLE D3D9Effect::eSpotTech = 0;	// Vessel beacons
D3DXHANDLE D3D9Effect::eBaseTile = 0;
D3DXHANDLE D3D9Effect::eRingTech = 0;	// Planet rings technique
D3DXHANDLE D3D9Effect::eRingTech2 = 0;	// Planet rings technique
D3DXHANDLE D3D9Effect::eShadowTech = 0; // Vessel ground shadows
D3DXHANDLE D3D9Effect::eArrowTech = 0;  // (Grapple point) arrows
D3DXHANDLE D3D9Effect::eAxisTech = 0;

// VC rendering techniques
D3DXHANDLE D3D9Effect::eVCMFDTech = 0;
D3DXHANDLE D3D9Effect::eVCTech = 0;
D3DXHANDLE D3D9Effect::eVCHudTech = 0;

// Planet Rendering techniques
D3DXHANDLE D3D9Effect::ePlanetTile = 0;
D3DXHANDLE D3D9Effect::eCloudTech = 0;
D3DXHANDLE D3D9Effect::eCloudShadow = 0;
D3DXHANDLE D3D9Effect::eSkyDomeTech = 0;	
D3DXHANDLE D3D9Effect::eHazeTech = 0;

// Particle effect texhniques
D3DXHANDLE D3D9Effect::eDiffuseTech = 0;
D3DXHANDLE D3D9Effect::eEmissiveTech = 0;



D3DXHANDLE D3D9Effect::eVP = 0;			// Combined View & Projection Matrix
D3DXHANDLE D3D9Effect::eW = 0;			// World matrix
D3DXHANDLE D3D9Effect::eWI = 0;			// World inverse matrix
D3DXHANDLE D3D9Effect::eGT = 0;			// Mesh group transformation matrix
D3DXHANDLE D3D9Effect::eGTI = 0;		// Inverse mesh grp ...
D3DXHANDLE D3D9Effect::eInstMatrix = 0;	// Instance Matrix array
D3DXHANDLE D3D9Effect::eMat = 0;		// Material
D3DXHANDLE D3D9Effect::eWater = 0;		// Water
D3DXHANDLE D3D9Effect::eMtrl = 0;
D3DXHANDLE D3D9Effect::eSun = 0;
D3DXHANDLE D3D9Effect::eLights = 0;		// Additional light sources
D3DXHANDLE D3D9Effect::eLightCount = 0;	// Number of additional light sources

D3DXHANDLE D3D9Effect::eTex0 = 0;		// Primary texture
D3DXHANDLE D3D9Effect::eTex1 = 0;		// Secondary texture
D3DXHANDLE D3D9Effect::eTex3 = 0;		// Tertiary texture
D3DXHANDLE D3D9Effect::eSpecMap = 0;
D3DXHANDLE D3D9Effect::eEmisMap = 0;
D3DXHANDLE D3D9Effect::eEnvMap = 0;
D3DXHANDLE D3D9Effect::eDislMap = 0;
D3DXHANDLE D3D9Effect::eReflMap = 0;
D3DXHANDLE D3D9Effect::eTranslMap = 0;
D3DXHANDLE D3D9Effect::eTransmMap = 0;

D3DXHANDLE D3D9Effect::eSpecularMode = 0;
D3DXHANDLE D3D9Effect::eHazeMode = 0;
D3DXHANDLE D3D9Effect::eColor = 0;		// Auxiliary color input
D3DXHANDLE D3D9Effect::eFogColor = 0;	// Fog color input
D3DXHANDLE D3D9Effect::eTexOff = 0;		// Surface tile texture offsets
D3DXHANDLE D3D9Effect::eTime = 0;		// FLOAT Simulation elapsed time
D3DXHANDLE D3D9Effect::eMix = 0;		// FLOAT Auxiliary factor/multiplier
D3DXHANDLE D3D9Effect::eFogDensity = 0;	// 
D3DXHANDLE D3D9Effect::ePointScale = 0;

D3DXHANDLE D3D9Effect::eAtmColor = 0;
D3DXHANDLE D3D9Effect::eProxySize = 0;
D3DXHANDLE D3D9Effect::eMtrlAlpha = 0;

// Shader Flow Controls
D3DXHANDLE D3D9Effect::eModAlpha = 0;	// BOOL if true multiply material alpha with texture alpha
D3DXHANDLE D3D9Effect::eFullyLit = 0;	// BOOL
D3DXHANDLE D3D9Effect::eNormalMap = 0;	// BOOL
D3DXHANDLE D3D9Effect::eTextured = 0;	// BOOL
D3DXHANDLE D3D9Effect::eClamp = 0;		// BOOL
D3DXHANDLE D3D9Effect::eUseSpec = 0;	// BOOL
D3DXHANDLE D3D9Effect::eUseEmis = 0;	// BOOL
D3DXHANDLE D3D9Effect::eDebugHL = 0;	// BOOL
D3DXHANDLE D3D9Effect::eUseDisl = 0;	// BOOL
D3DXHANDLE D3D9Effect::eUseRefl = 0;	// BOOL
D3DXHANDLE D3D9Effect::eUseTransl = 0;	// BOOL
D3DXHANDLE D3D9Effect::eUseTransm = 0;	// BOOL
D3DXHANDLE D3D9Effect::eEnvMapEnable = 0;	// BOOL
D3DXHANDLE D3D9Effect::eInSpace = 0;	// BOOL
D3DXHANDLE D3D9Effect::eNoColor = 0;	// BOOL
D3DXHANDLE D3D9Effect::eLocalLights  = 0;	// BOOL		
D3DXHANDLE D3D9Effect::eGlow = 0;	// BOOL	
// --------------------------------------------------------------
D3DXHANDLE D3D9Effect::eExposure = 0;
D3DXHANDLE D3D9Effect::eCameraPos = 0;	
D3DXHANDLE D3D9Effect::eDistScale = 0;
D3DXHANDLE D3D9Effect::eRadius = 0;
D3DXHANDLE D3D9Effect::eAttennuate = 0;
D3DXHANDLE D3D9Effect::eInScatter = 0;
D3DXHANDLE D3D9Effect::eInvProxySize = 0;
D3DXHANDLE D3D9Effect::eGlowConst = 0;
// --------------------------------------------------------------
D3DXHANDLE D3D9Effect::eGlobalAmb = 0;	 
D3DXHANDLE D3D9Effect::eSunAppRad = 0;	 
D3DXHANDLE D3D9Effect::eAmbient0 = 0;	 
D3DXHANDLE D3D9Effect::eDispersion = 0;	  

LPDIRECT3DDEVICE9 D3D9Effect::pDev = 0;

static D3DMATERIAL9 _emissive_mat = { 
	{0,0,0,1},
	{0,0,0,1},
	{0,0,0,1},
	{1,1,1,1},
	0.0
};

static D3DMATERIAL9 _defmat = {
	{1,1,1,1},
	{1,1,1,1},
	{0,0,0,1},
	{0,0,0,1},10.0f
};

static D3DMATERIAL9 _night_mat = {
	{1,1,1,1},
	{0,0,0,1},
	{0,0,0,1},
	{1,1,1,1},10.0f
};

static WORD billboard_idx[6] = {0,1,2, 3,2,1};

static NTVERTEX billboard_vtx[4] = {
	{0,-1, 1,  -1,0,0,  0,0},
	{0, 1, 1,  -1,0,0,  0,1},
	{0,-1,-1,  -1,0,0,  1,0},
	{0, 1,-1,  -1,0,0,  1,1}
};

static WORD exhaust_idx[12] = {0,1,2, 3,2,1, 4,5,6, 7,6,5};


NTVERTEX exhaust_vtx[8] = {
	{0,0,0, 0,0,0, 0.24f,0},
	{0,0,0, 0,0,0, 0.24f,1},
	{0,0,0, 0,0,0, 0.01f,0},
	{0,0,0, 0,0,0, 0.01f,1},
	{0,0,0, 0,0,0, 0.50390625f, 0.00390625f},
	{0,0,0, 0,0,0, 0.99609375f, 0.00390625f},
	{0,0,0, 0,0,0, 0.50390625f, 0.49609375f},
	{0,0,0, 0,0,0, 0.99609375f, 0.49609375f}
};

// ===========================================================================================
//
D3D9Effect::D3D9Effect() : d3d9id('D3D9')
{

}

// ===========================================================================================
//
D3D9Effect::~D3D9Effect()
{
	
}

void D3D9Effect::GlobalExit()
{
	LogAlw("====== D3D9Effect Global Exit =======");
	SAFE_RELEASE(FX);
	SAFE_RELEASE(VB);
	SAFE_DELETE(hArrow);
}

void D3D9Effect::ShutDown()
{
	if (!FX) return;
	FX->SetTexture(eTex0, NULL);
	FX->SetTexture(eTex1, NULL);
	FX->SetTexture(eTex3, NULL);
	FX->SetTexture(eSpecMap, NULL);
	FX->SetTexture(eEmisMap, NULL);
	oapiReleaseTexture(hNoise);
}


// ===========================================================================================
//
void D3D9Effect::D3D9TechInit(D3D9Client *_gc, LPDIRECT3DDEVICE9 _pDev, const char *folder)
{
	char name[256];

	pDev = _pDev;
	gc = _gc;

	gc->OutputLoadStatus("D3D9Client.fx",1);
	
	LogAlw("Starting to initialize D3D9Client.fx a rendering technique...");
	
	// Create the Effect from a .fx file.
	ID3DXBuffer* errors = 0;
	D3DXMACRO macro[8]; memset2(&macro, 0, 8*sizeof(D3DXMACRO));

	macro[0].Name = "VS_MOD";
	macro[1].Name = "PS_MOD";

	LogAlw("[Compiling Effects for Shader Model 3.0]");
		
	macro[0].Definition = "vs_3_0";
	macro[1].Definition = "ps_3_0";
	sprintf_s(name,256,"Modules/D3D9Client/D3D9Client.fx");
	
	macro[2].Name = "ANISOTROPY_MACRO";
	macro[2].Definition = new char[32];
	sprintf_s((char*)macro[2].Definition,32,"%d",max(2,Config->Anisotrophy));

	int m = 3;
	if (Config->EnableGlass) macro[m++].Name = "_GLASS";
	if (Config->EnableMeshDbg) macro[m++].Name = "_DEBUG";
	if (Config->EnvMapMode) macro[m++].Name = "_ENVMAP"; 
	
	HR(D3DXCreateEffectFromFileA(pDev, name, macro, 0, 0, 0, &FX, &errors));
	
	delete []macro[2].Definition;

	if (errors) {
		LogErr("Effect Error: %s",(char*)errors->GetBufferPointer());
		MessageBoxA(0, (char*)errors->GetBufferPointer(), "D3D9Client.fx Error", 0);
		FatalAppExitA(0,"Critical error has occured. See Orbiter.log for details");
	}

	if (FX==0) {
		LogErr("Failed to create an Effect (%s)",name);
		return;
	}


	// Techniques --------------------------------------------------------------

	eHazeTech	 = FX->GetTechniqueByName("HazeTech");
	ePlanetTile  = FX->GetTechniqueByName("PlanetTech");
	eBaseTile    = FX->GetTechniqueByName("BaseTileTech");
	eSimple      = FX->GetTechniqueByName("SimpleTech");
	eBBTech		 = FX->GetTechniqueByName("BoundingBoxTech");
	eBSTech		 = FX->GetTechniqueByName("BoundingSphereTech");
	eRingTech    = FX->GetTechniqueByName("RingTech");
	eRingTech2   = FX->GetTechniqueByName("RingTech2");
	eExhaust     = FX->GetTechniqueByName("ExhaustTech");
	eSpotTech    = FX->GetTechniqueByName("SpotTech");
	eShadowTech  = FX->GetTechniqueByName("ShadowTech");
	ePanelTech	 = FX->GetTechniqueByName("PanelTech");
	ePanelTechB	 = FX->GetTechniqueByName("PanelTechB");
	eCloudTech   = FX->GetTechniqueByName("PlanetCloudTech");
	eCloudShadow = FX->GetTechniqueByName("PlanetCloudShadowTech");
	eSkyDomeTech = FX->GetTechniqueByName("SkyDomeTech");
	eArrowTech   = FX->GetTechniqueByName("ArrowTech"); 
	eAxisTech    = FX->GetTechniqueByName("AxisTech"); 
	
	eVCMFDTech	 = FX->GetTechniqueByName("VCMFDTech");
	eVCHudTech	 = FX->GetTechniqueByName("VCHudTech");
	eVCTech		 = FX->GetTechniqueByName("VCTech");

	eVesselTech		 = FX->GetTechniqueByName("VesselTech");
	eBaseShadowTech	 = FX->GetTechniqueByName("BaseShadowTech");
	eBeaconArrayTech = FX->GetTechniqueByName("BeaconArrayTech");
	eDiffuseTech     = FX->GetTechniqueByName("ParticleDiffuseTech");
	eEmissiveTech    = FX->GetTechniqueByName("ParticleEmissiveTech");
	
	// Flow Control Booleans -----------------------------------------------
	eModAlpha	  = FX->GetParameterByName(0,"gModAlpha");
	eFullyLit	  = FX->GetParameterByName(0,"gFullyLit");
	eUseEmis	  = FX->GetParameterByName(0,"gUseEmis");
	eUseSpec	  = FX->GetParameterByName(0,"gUseSpec");
	eDebugHL	  = FX->GetParameterByName(0,"gDebugHL");
	eEnvMapEnable = FX->GetParameterByName(0,"gEnvMapEnable");
	eNormalMap	  = FX->GetParameterByName(0,"gNormalMap");
	eTextured	  = FX->GetParameterByName(0,"gTextured");
	eClamp		  = FX->GetParameterByName(0,"gClamp");
	eUseDisl	  = FX->GetParameterByName(0,"gUseDisl");
	eUseRefl	  = FX->GetParameterByName(0,"gUseRefl");
	eUseTransl	  = FX->GetParameterByName(0,"gUseTransl");
	eUseTransm	  = FX->GetParameterByName(0,"gUseTransm");
	eInSpace	  = FX->GetParameterByName(0,"gInSpace");			
	eNoColor	  = FX->GetParameterByName(0,"gNoColor");	
	eLocalLights  = FX->GetParameterByName(0,"gLocalLights");			
	eGlow		  = FX->GetParameterByName(0,"gGlow");	
	// General parameters -------------------------------------------------- 
	eSpecularMode = FX->GetParameterByName(0,"gSpecMode");
	eLights		  = FX->GetParameterByName(0,"gLights");
	eLightCount	  = FX->GetParameterByName(0,"gLightCount");
	eColor		  = FX->GetParameterByName(0,"gColor");
	eDistScale    = FX->GetParameterByName(0,"gDistScale");
	eProxySize    = FX->GetParameterByName(0,"gProxySize");
	eTexOff		  = FX->GetParameterByName(0,"gTexOff");
	eRadius       = FX->GetParameterByName(0,"gRadius");
	eCameraPos	  = FX->GetParameterByName(0,"gCameraPos");
	ePointScale   = FX->GetParameterByName(0,"gPointScale");
	eMix		  = FX->GetParameterByName(0,"gMix");
	eTime		  = FX->GetParameterByName(0,"gTime");
	eMtrlAlpha	  = FX->GetParameterByName(0,"gMtrlAlpha");
	eGlowConst    = FX->GetParameterByName(0,"gGlowConst");
	// ----------------------------------------------------------------------
	eVP			  = FX->GetParameterByName(0,"gVP");
	eW			  = FX->GetParameterByName(0,"gW");
	eWI			  = FX->GetParameterByName(0,"gWI");
	eGT			  = FX->GetParameterByName(0,"gGrpT");
	eGTI		  = FX->GetParameterByName(0,"gGrpTI");
	eInstMatrix   = FX->GetParameterByName(0,"gGrpInst");
	// ----------------------------------------------------------------------
	eSun		  = FX->GetParameterByName(0,"gSun");
	eMat		  = FX->GetParameterByName(0,"gMat");
	eWater		  = FX->GetParameterByName(0,"gWater");
	eMtrl		  = FX->GetParameterByName(0,"gMtrl");
	// ----------------------------------------------------------------------
	eTex0		  = FX->GetParameterByName(0,"gTex0");
	eTex1		  = FX->GetParameterByName(0,"gTex1");
	eTex3		  = FX->GetParameterByName(0,"gTex3");
	eSpecMap	  = FX->GetParameterByName(0,"gSpecMap");
	eEmisMap	  = FX->GetParameterByName(0,"gEmisMap");
	eEnvMap		  = FX->GetParameterByName(0,"gEnvMap");
	eDislMap	  = FX->GetParameterByName(0,"gDislMap");
	eReflMap	  = FX->GetParameterByName(0,"gReflMap");

	// Atmosphere -----------------------------------------------------------
	eGlobalAmb	  = FX->GetParameterByName(0,"gGlobalAmb");
	eSunAppRad	  = FX->GetParameterByName(0,"gSunAppRad");
	eAmbient0	  = FX->GetParameterByName(0,"gAmbient0");
	eDispersion	  = FX->GetParameterByName(0,"gDispersion");
	eFogDensity	  = FX->GetParameterByName(0,"gFogDensity");
	eAttennuate	  = FX->GetParameterByName(0,"gAttennuate");
	eInScatter	  = FX->GetParameterByName(0,"gInScatter");
	eInvProxySize = FX->GetParameterByName(0,"gInvProxySize");
	eFogColor	  = FX->GetParameterByName(0,"gFogColor");
	eAtmColor	  = FX->GetParameterByName(0,"gAtmColor");
	eHazeMode	  = FX->GetParameterByName(0,"gHazeMode");
	eTranslMap	  = FX->GetParameterByName(0,"gTranslMap");
	eTransmMap	  = FX->GetParameterByName(0,"gTransmMap");

	// Initialize default values --------------------------------------
	//
	FX->SetInt(eHazeMode, 0);
	FX->SetBool(eDebugHL, false);
	FX->SetBool(eInSpace, false);
	FX->SetVector(eAttennuate, &D3DXVECTOR4(1,1,1,1)); 
	FX->SetVector(eInScatter,  &D3DXVECTOR4(0,0,0,0));

	CreateMatExt(&_defmat, &defmat);
	CreateMatExt(&_night_mat, &night_mat);
	CreateMatExt(&_emissive_mat, &emissive_mat);

	// Create a Circle Mesh --------------------------------------------
	//
	HR(pDev->CreateVertexBuffer(256*sizeof(D3DXVECTOR3), 0, 0, D3DPOOL_DEFAULT, &VB, NULL));

	D3DXVECTOR3 *pVert;

	if (VB->Lock(0,0,(void **)&pVert,0)==S_OK) {
		float angle=0.0f, step=float(PI2)/255.0f;
		pVert[0] = D3DXVECTOR3(0,0,0);
		for (int i=1;i<256;i++) {
			pVert[i].x = 0;
			pVert[i].y = cos(angle);
			pVert[i].z = sin(angle);
			angle += step;
		}
		VB->Unlock();
	} else LogErr("Failed to Lock vertex buffer");

	hNoise = oapiLoadTexture("D3D9LENoise.dds");
	if (hNoise) pNoise = SURFACE(hNoise)->GetTexture();
	else pNoise = NULL;

	// Create Arrow Mesh --------------------------------------------
	//
	MESHHANDLE hMesh = oapiLoadMesh("D3D9Arrow");
	hArrow = new D3D9Mesh(gc, hMesh);
	oapiDeleteMesh(hMesh);

	if (hArrow==NULL) LogErr("D3D9Arrow.msh not found");
	
	LogMsg("...rendering technique initialized");
}


void D3D9Effect::SetViewProjMatrix(LPD3DXMATRIX pVP)
{
	FX->SetMatrix(eVP, pVP);
}


void D3D9Effect::UpdateEffectCamera(OBJHANDLE hPlanet)
{
	VECTOR3 cam, pla, sun;
	OBJHANDLE hSun = oapiGetGbodyByIndex(0); // generalise later
	cam = gc->GetScene()->GetCameraGPos();
	oapiGetGlobalPos(hPlanet, &pla);
	oapiGetGlobalPos(hSun, &sun);

	double len  = length(cam - pla);
	double rad  = oapiGetSize(hPlanet);
	
	sun = unit(sun - cam);	// Vector pointing to sun from camera
	cam = unit(cam - pla);	// Vector pointing to cam from planet
	
	DWORD width, height;
	oapiGetViewportSize(&width, &height); // BUG:  Custom Camera may have different view size


	float radlimit = float(rad) + 1.0f;
	float rho0 = 1.0f;
	
	D3DXVECTOR4 atm_color(0.5f, 0.5f, 0.5f, 1.0f);

	const ATMCONST *atm = oapiGetPlanetAtmConstants(hPlanet); 
	VESSEL *hVessel = oapiGetFocusInterface();

	if (hVessel==NULL) {
		LogErr("hVessel = NULL in UpdateEffectCamera()");
		return;
	}

	if (atm) {
		radlimit = float(atm->radlimit);
		atm_color = D3DXVEC4(atm->color0, 1.0f);
		rho0 = float(atm->rho0);
	}

	float av = (atm_color.x + atm_color.y + atm_color.z) * 0.3333333f;
	float fc = 1.5f;
	float alt = 1.0f - pow(float(hVessel->GetAtmDensity()/rho0), 0.2f);
	atm_color += D3DXVECTOR4(av,av,av,1.0)*fc;
	atm_color *= 1.0f/(fc+1.0f);
	atm_color *= float(Config->PlanetGlow) * alt;
	
	float ap = gc->GetScene()->GetCameraAperture();
	float rl = float(rad/len);
	float proxy_size = asin(min(1.0f, rl)) + float(40.0*PI/180.0);

	if (rl>1e-3) atm_color *= pow(rl, 1.5f);
	else atm_color = D3DXVECTOR4(0,0,0,1);

	FX->SetValue(eCameraPos, &D3DXVECTOR3(float(cam.x),float(cam.y),float(cam.z)), sizeof(D3DXVECTOR3));
	FX->SetVector(eAtmColor, &atm_color);
	FX->SetVector(eRadius, &D3DXVECTOR4((float)rad, radlimit, (float)len, (float)(len-rad)));
	FX->SetFloat(ePointScale, 0.5f*float(height)/tan(ap));
	FX->SetFloat(eProxySize, cos(proxy_size));
	FX->SetFloat(eInvProxySize, 1.0f/(1.0f-cos(proxy_size)));
	FX->SetFloat(eGlowConst, saturate(float(dotp(cam, sun))));
}

// ===========================================================================================
//
void D3D9Effect::InitLegacyAtmosphere(OBJHANDLE hPlanet, float GlobalAmbient)
{
	VECTOR3 GS, GP;
	
	OBJHANDLE hS = oapiGetGbodyByIndex(0);	// the central star
	oapiGetGlobalPos (hS, &GS);				// sun position
	oapiGetGlobalPos (hPlanet, &GP);		// planet position
			
	float rs = (float)(oapiGetSize(hS) / length(GS-GP));
	
	const ATMCONST *atm = (oapiGetObjectType(hPlanet)==OBJTP_PLANET ? oapiGetPlanetAtmConstants (hPlanet) : NULL);

	FX->SetFloat(eGlobalAmb, GlobalAmbient);
	FX->SetFloat(eSunAppRad, rs); 

	if (atm) {
		FX->SetFloat(eAmbient0, min(0.7f, log(float(atm->rho0)+1.0f)*0.4f));
		FX->SetFloat(eDispersion, max(0.02f, min(0.9f, log(float(atm->rho0)+1.0f))));
	}
	else {
		FX->SetFloat(eAmbient0, 0.0f);
		FX->SetFloat(eDispersion, 0.0f);
	}
}



// ===========================================================================================
//
void D3D9Effect::Render2DPanel(const MESHGROUP *mg, const LPD3D9CLIENTSURFACE pTex, const LPD3DXMATRIX pW, float alpha, float scale, bool additive)
{
	UINT numPasses = 0;

	if (pTex->IsPowerOfTwo()) FX->SetTechnique(ePanelTech);		// ANISOTROPIC filter 
	else					  FX->SetTechnique(ePanelTechB);	// POINT filter (for non-pow2 conditional)
	
	HR(FX->SetMatrix(eW, pW));

	if (pTex) FX->SetTexture(eTex0, pTex->GetTexture());
	else      FX->SetTexture(eTex0, NULL);

	HR(FX->SetFloat(eMix, alpha));
	HR(FX->Begin(&numPasses, D3DXFX_DONOTSAVESTATE));
	HR(FX->BeginPass(0));

	if (additive) pDev->SetRenderState(D3DRS_DESTBLEND, D3DBLEND_ONE);
	else		  pDev->SetRenderState(D3DRS_DESTBLEND, D3DBLEND_INVSRCALPHA);

	pDev->SetVertexDeclaration(pNTVertexDecl);
	pDev->DrawIndexedPrimitiveUP(D3DPT_TRIANGLELIST, 0, mg->nVtx, mg->nIdx/3, mg->Idx, D3DFMT_INDEX16, mg->Vtx, sizeof(NTVERTEX));
	
	gc->GetStats()->Vertices += mg->nVtx;
	gc->GetStats()->Draw++;

	HR(FX->EndPass());
	HR(FX->End());	
}




// ===========================================================================================
//
void D3D9Effect::RenderReEntry(const LPD3D9CLIENTSURFACE pTex, const LPD3DXVECTOR3 vPosA, const LPD3DXVECTOR3 vPosB, const LPD3DXVECTOR3 vDir, float alpha_a, float alpha_b, float size)
{
	static WORD ReentryIdx[6] = {0,1,2, 3,2,1};
	
	static NTVERTEX ReentryVtxB[4] = {
		{0,-1,-1,   0,0,0, 0.51f, 0.01f},
		{0,-1, 1,   0,0,0, 0.99f, 0.01f},
		{0, 1,-1,   0,0,0, 0.51f, 0.49f},
		{0, 1, 1,   0,0,0, 0.99f, 0.49f}
	};

	float x = 4.5f + sin(fmod(float(oapiGetSimTime())*60.0f, 6.283185f)) * 0.5f;

	NTVERTEX ReentryVtxA[4] = {
		{0, 1, 1, 0,0,0, 0.49f, 0.01f},
		{0, 1,-x, 0,0,0, 0.49f, 0.99f},
		{0,-1, 1, 0,0,0, 0.01f, 0.01f},
		{0,-1,-x, 0,0,0, 0.01f, 0.99f},
	};
	
	UINT numPasses = 0;
	D3DXMATRIX WA, WB;
	D3DXVECTOR3 vCam;
	D3DXVec3Normalize(&vCam, vPosA);
	D3DMAT_CreateX_Billboard(&vCam, vPosB, size*(0.8f+x*0.02f), &WB);
	D3DMAT_CreateX_Billboard(&vCam, vPosA, vDir, size, size, &WA);

	pDev->SetVertexDeclaration(pNTVertexDecl);

	FX->SetTechnique(eExhaust);
	FX->SetTexture(eTex0, SURFACE(pTex)->GetTexture());
	FX->SetFloat(eMix, alpha_b);
	FX->SetMatrix(eW, &WB);
	FX->Begin(&numPasses, D3DXFX_DONOTSAVESTATE);
	FX->BeginPass(0);
	pDev->DrawIndexedPrimitiveUP(D3DPT_TRIANGLELIST, 0, 4, 2, &ReentryIdx, D3DFMT_INDEX16, &ReentryVtxB, sizeof(NTVERTEX));
	FX->SetFloat(eMix, alpha_a);
	FX->SetMatrix(eW, &WA);
	FX->CommitChanges();
	pDev->DrawIndexedPrimitiveUP(D3DPT_TRIANGLELIST, 0, 4, 2, &ReentryIdx, D3DFMT_INDEX16, &ReentryVtxA, sizeof(NTVERTEX));

	gc->GetStats()->Draw+=2;

	FX->EndPass();
	FX->End();	
}


// ===========================================================================================
// This is a special rendering routine used to render beacons
//
void D3D9Effect::RenderSpot(float alpha, const LPD3DXCOLOR pColor, const LPD3DXMATRIX pW, LPD3D9CLIENTSURFACE pTex)
{
	UINT numPasses = 0;
	HR(pDev->SetVertexDeclaration(pNTVertexDecl));
	HR(FX->SetTechnique(eSpotTech));
	HR(FX->SetFloat(eMix, alpha));
	HR(FX->SetValue(eColor, pColor, sizeof(D3DXCOLOR)));
	HR(FX->SetMatrix(eW, pW));
	HR(FX->SetTexture(eTex0, pTex->GetTexture()));
	HR(FX->Begin(&numPasses, D3DXFX_DONOTSAVESTATE));
	HR(FX->BeginPass(0));
	HR(pDev->DrawIndexedPrimitiveUP(D3DPT_TRIANGLELIST, 0, 4, 2, billboard_idx, D3DFMT_INDEX16, billboard_vtx, sizeof(NTVERTEX)));
	HR(FX->EndPass());
	HR(FX->End());	
	gc->GetStats()->Draw++;
}


// ===========================================================================================
//
// Used by Render Star only
//
void D3D9Effect::RenderBillboard(const LPD3DXMATRIX pW, LPD3D9CLIENTSURFACE pTex)
{
	UINT numPasses = 0;

	HR(pDev->SetVertexDeclaration(pNTVertexDecl));
	HR(FX->SetTechnique(eSimple));
	HR(FX->SetMatrix(eW, pW));
	HR(FX->SetTexture(eTex0, pTex->GetTexture()));
	HR(FX->Begin(&numPasses, D3DXFX_DONOTSAVESTATE));
	HR(FX->BeginPass(0));
	HR(pDev->DrawIndexedPrimitiveUP(D3DPT_TRIANGLELIST, 0, 4, 2, billboard_idx, D3DFMT_INDEX16, billboard_vtx, sizeof(NTVERTEX)));
	HR(FX->EndPass());
	HR(FX->End());	
	gc->GetStats()->Draw++;
}


// ===========================================================================================
// This is a special rendering routine used to render engine exhaust
//
void D3D9Effect::RenderExhaust(const LPD3DXMATRIX pW, VECTOR3 &cdir, EXHAUSTSPEC *es, LPD3D9CLIENTSURFACE def)
{

	LPD3D9CLIENTSURFACE pTex = SURFACE(es->tex);
	if (!pTex) pTex = def;

	double alpha = *es->level;
	if (es->modulate) alpha *= ((1.0 - es->modulate)+(double)rand()* es->modulate/(double)RAND_MAX);

	VECTOR3 edir = -(*es->ldir);
	VECTOR3 ref =  (*es->lpos) - (*es->ldir)*es->lofs;

	const float flarescale = 7.0;
	VECTOR3 sdir = crossp(cdir, edir); normalise(sdir);
	VECTOR3 tdir = crossp(cdir, sdir); normalise(tdir);
	float rx = (float)ref.x; 
	float ry = (float)ref.y; 
	float rz = (float)ref.z;
	float sx = (float)(sdir.x*es->wsize);
	float sy = (float)(sdir.y*es->wsize);
	float sz = (float)(sdir.z*es->wsize);
	float ex = (float)(edir.x*es->lsize);
	float ey = (float)(edir.y*es->lsize);
	float ez = (float)(edir.z*es->lsize);

	exhaust_vtx[1].x = (exhaust_vtx[0].x = rx + sx) + ex;
	exhaust_vtx[1].y = (exhaust_vtx[0].y = ry + sy) + ey;
	exhaust_vtx[1].z = (exhaust_vtx[0].z = rz + sz) + ez;
	exhaust_vtx[3].x = (exhaust_vtx[2].x = rx - sx) + ex;
	exhaust_vtx[3].y = (exhaust_vtx[2].y = ry - sy) + ey;
	exhaust_vtx[3].z = (exhaust_vtx[2].z = rz - sz) + ez;

	double wscale = es->wsize;
	wscale *= flarescale, sx *= flarescale, sy *= flarescale, sz *= flarescale;

	float tx = (float)(tdir.x*wscale);
	float ty = (float)(tdir.y*wscale);
	float tz = (float)(tdir.z*wscale);
	exhaust_vtx[4].x = rx - sx + tx;   exhaust_vtx[5].x = rx + sx + tx;
	exhaust_vtx[4].y = ry - sy + ty;   exhaust_vtx[5].y = ry + sy + ty;
	exhaust_vtx[4].z = rz - sz + tz;   exhaust_vtx[5].z = rz + sz + tz;
	exhaust_vtx[6].x = rx - sx - tx;   exhaust_vtx[7].x = rx + sx - tx;
	exhaust_vtx[6].y = ry - sy - ty;   exhaust_vtx[7].y = ry + sy - ty;
	exhaust_vtx[6].z = rz - sz - tz;   exhaust_vtx[7].z = rz + sz - tz;

	UINT numPasses = 0;
	HR(pDev->SetVertexDeclaration(pNTVertexDecl));
	HR(FX->SetTechnique(eExhaust));
	HR(FX->SetFloat(eMix, float(alpha)));
	HR(FX->SetMatrix(eW, pW));
	HR(FX->SetTexture(eTex0, pTex->GetTexture()));
	HR(FX->Begin(&numPasses, D3DXFX_DONOTSAVESTATE));
	HR(FX->BeginPass(0));
	HR(pDev->DrawIndexedPrimitiveUP(D3DPT_TRIANGLELIST, 0, 8, 4, exhaust_idx, D3DFMT_INDEX16, exhaust_vtx, sizeof(NTVERTEX)));
	HR(FX->EndPass());
	HR(FX->End());	
	gc->GetStats()->Draw++;
}


void D3D9Effect::RenderBoundingBox(const LPD3DXMATRIX pW, const LPD3DXMATRIX pGT, const D3DXVECTOR4 *bmin, const D3DXVECTOR4 *bmax, const LPD3DXVECTOR4 color)
{
	D3DXMATRIX ident;
	D3DXMatrixIdentity(&ident);

	static D3DVECTOR poly[10] = {
		{0, 0, 0},
		{1, 0, 0},
		{1, 1, 0},
		{0, 1, 0},
		{0, 0, 0},
		{0, 0, 1},
		{1, 0, 1},
		{1, 1, 1},
		{0, 1, 1},
		{0, 0, 1}
	};

	static D3DVECTOR list[6] = {
		{1, 0, 0},
		{1, 0, 1},
		{1, 1, 0},
		{1, 1, 1},
		{0, 1, 0},
		{0, 1, 1}
	};
	
	pDev->SetVertexDeclaration(pPositionDecl);

	FX->SetMatrix(eW, pW);
	FX->SetMatrix(eGT, pGT);
	FX->SetVector(eAttennuate, bmin);
	FX->SetVector(eInScatter, bmax);	
	FX->SetVector(eColor, color);	
	FX->SetTechnique(eBBTech);

	UINT numPasses = 0;
	FX->Begin(&numPasses, D3DXFX_DONOTSAVESTATE);
	FX->BeginPass(0);
	
	pDev->DrawPrimitiveUP(D3DPT_LINESTRIP, 9, &poly, sizeof(D3DVECTOR));	
	pDev->DrawPrimitiveUP(D3DPT_LINELIST, 3, &list, sizeof(D3DVECTOR));	

	FX->EndPass();
	FX->End();	
}


void D3D9Effect::RenderBoundingSphere(const LPD3DXMATRIX pW, const LPD3DXMATRIX pGT, const D3DXVECTOR4 *bs, const LPD3DXVECTOR4 color)
{
	D3DXMATRIX mW;
	
	D3DXVECTOR3 vCam;
	D3DXVECTOR3 vPos;
	
	D3DXVec3TransformCoord(&vPos, &D3DXVECTOR3(bs->x, bs->y, bs->z), pW);

	D3DXVec3Normalize(&vCam, &vPos);
	D3DMAT_CreateX_Billboard(&vCam, &vPos, bs->w, &mW);

	pDev->SetVertexDeclaration(pPositionDecl);

	FX->SetMatrix(eW, &mW);
	FX->SetVector(eColor, color);	
	FX->SetTechnique(eBSTech);

	UINT numPasses = 0;
	FX->Begin(&numPasses, D3DXFX_DONOTSAVESTATE);
	FX->BeginPass(0);
	
	pDev->SetStreamSource(0, VB, 0, sizeof(D3DXVECTOR3));
	pDev->DrawPrimitive(D3DPT_LINESTRIP, 0, 255);	
	
	FX->EndPass();
	FX->End();	
}

// ===========================================================================================
// This is a special rendering routine used to render (grapple point) arrows
//
void D3D9Effect::RenderArrow(OBJHANDLE hObj, const VECTOR3 *ofs, const VECTOR3 *dir, const VECTOR3 *rot, float size, const D3DXCOLOR *pColor)
{
    static D3DVECTOR arrow[18] = {
        // Head (front- & back-face)
        {0.0, 0.0, 0.0},
        {0.0,-1.0, 1.0},
        {0.0, 1.0, 1.0},
        {0.0, 0.0, 0.0},
        {0.0, 1.0, 1.0},
        {0.0,-1.0, 1.0},
        // Body first triangle (front- & back-face)
        {0.0,-0.5, 1.0},
        {0.0,-0.5, 2.0},
        {0.0, 0.5, 2.0},
        {0.0,-0.5, 1.0},
        {0.0, 0.5, 2.0},
        {0.0,-0.5, 2.0},
        // Body second triangle (front- & back-face)
        {0.0,-0.5, 1.0},
        {0.0, 0.5, 2.0},
        {0.0, 0.5, 1.0},
        {0.0,-0.5, 1.0},
        {0.0, 0.5, 1.0},
        {0.0, 0.5, 2.0}
    };

    MATRIX3 grot;
    D3DXMATRIX W;
    VECTOR3 camp, gpos;

    oapiGetRotationMatrix(hObj, &grot);
    oapiGetGlobalPos(hObj, &gpos);
    camp = gc->GetScene()->GetCameraGPos();

    VECTOR3 pos = gpos - camp;
    if (ofs) pos += mul (grot, *ofs);

    VECTOR3 z = mul (grot, unit(*dir)) * size;
    VECTOR3 y = mul (grot, unit(*rot)) * size;
    VECTOR3 x = mul (grot, unit(crossp(*dir, *rot))) * size;

    D3DXMatrixIdentity(&W);

    W._11 = float(x.x);
    W._12 = float(x.y);
    W._13 = float(x.z);

    W._21 = float(y.x);
    W._22 = float(y.y);
    W._23 = float(y.z);

    W._31 = float(z.x);
    W._32 = float(z.y);
    W._33 = float(z.z);

    W._41 = float(pos.x);
    W._42 = float(pos.y);
    W._43 = float(pos.z);

    UINT numPasses = 0;
    HR(pDev->SetVertexDeclaration(pPositionDecl)); // Position only vertex decleration
    HR(FX->SetTechnique(eArrowTech)); // Use arrow shader
    HR(FX->SetValue(eColor, pColor, sizeof(D3DXCOLOR))); // Setup arrow color
    HR(FX->SetMatrix(eW, &W));
    HR(FX->Begin(&numPasses, D3DXFX_DONOTSAVESTATE));
    HR(FX->BeginPass(0));
    HR(pDev->DrawPrimitiveUP(D3DPT_TRIANGLELIST, 6, &arrow, sizeof(D3DVECTOR))); // Draw 6 triangles un-indexed
    HR(FX->EndPass());
    HR(FX->End()); 
    gc->GetStats()->Draw++; // Increment rendering statistics counter
}  


// ===========================================================================================
// This is a special rendering routine used to render beacons
//
void D3D9Effect::RenderAxisVector(LPD3DXMATRIX pW, const LPD3DXCOLOR pColor, float len)
{
	UINT numPasses = 0;
	HR(FX->SetTechnique(eAxisTech));
	HR(FX->SetFloat(eMix, len));
	HR(FX->SetValue(eColor, pColor, sizeof(D3DXCOLOR)));
	HR(FX->SetMatrix(eW, pW));
	HR(FX->Begin(&numPasses, D3DXFX_DONOTSAVESTATE));
	HR(FX->BeginPass(0));

	pDev->SetRenderState(D3DRS_CULLMODE, D3DCULL_CW);
	hArrow->RenderGroup(pDev, hArrow->GetGroup(0));
	pDev->SetRenderState(D3DRS_CULLMODE, D3DCULL_CCW);

	HR(FX->EndPass());
	HR(FX->End());	

	gc->GetStats()->Draw++;
}