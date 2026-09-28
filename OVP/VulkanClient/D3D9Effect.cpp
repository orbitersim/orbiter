// ===========================================================================================
// D3D9Effect.cpp
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2011-2026 Jarmo Nikkanen
// ===========================================================================================

#include "D3D9Effect.h"
#include "Log.h"
#include "Scene.h"
#include "D3D9Surface.h"
#include "D3D9Config.h"
#include "Mesh.h"
#include "VectorHelpers.h"
#include <QMessageBox>

D3D9Client  * D3D9Effect::gc = 0;
VkEffect *D3D9Effect::FX = 0;
VkBuf *D3D9Effect::VB = 0;
D3DXVECTOR4 D3D9Effect::atm_color;			// Earth glow color

D3D9MatExt D3D9Effect::mfdmat;
D3D9MatExt D3D9Effect::defmat;
D3D9MatExt D3D9Effect::night_mat;
D3D9MatExt D3D9Effect::emissive_mat;

// Some general rendering techniques
VkFxHandle D3D9Effect::ePanelTech = 0;		// Used to draw a new style 2D panel
VkFxHandle D3D9Effect::ePanelTechB = 0;		// Used to draw a new style 2D panel
VkFxHandle D3D9Effect::eVesselTech = 0;		// Vessel exterior, surface bases.
VkFxHandle D3D9Effect::eBBTech = 0;			// Bounding Box Tech
VkFxHandle D3D9Effect::eTBBTech = 0;
VkFxHandle D3D9Effect::eBSTech = 0;			// Bounding Sphere Tech
VkFxHandle D3D9Effect::eSimple = 0;
VkFxHandle D3D9Effect::eBaseShadowTech = 0;	// Used to draw transparent surface without texture 
VkFxHandle D3D9Effect::eBeaconArrayTech = 0;
VkFxHandle D3D9Effect::eExhaust = 0;	// Render engine exhaust texture
VkFxHandle D3D9Effect::eSpotTech = 0;	// Vessel beacons
VkFxHandle D3D9Effect::eBaseTile = 0;
VkFxHandle D3D9Effect::eRingTech = 0;	// Planet rings technique
VkFxHandle D3D9Effect::eRingTech2 = 0;	// Planet rings technique
VkFxHandle D3D9Effect::eShadowTech = 0; // Vessel ground shadows
VkFxHandle D3D9Effect::eArrowTech = 0;  // (Grapple point) arrows
VkFxHandle D3D9Effect::eAxisTech = 0;
VkFxHandle D3D9Effect::eSimpMesh = 0;
VkFxHandle D3D9Effect::eGeometry = 0;

// Planet Rendering techniques
VkFxHandle D3D9Effect::ePlanetTile = 0;
VkFxHandle D3D9Effect::eCloudTech = 0;
VkFxHandle D3D9Effect::eCloudShadow = 0;
VkFxHandle D3D9Effect::eSkyDomeTech = 0;	
VkFxHandle D3D9Effect::eHazeTech = 0;

// Particle effect texhniques
VkFxHandle D3D9Effect::eDiffuseTech = 0;
VkFxHandle D3D9Effect::eEmissiveTech = 0;



VkFxHandle D3D9Effect::eVP = 0;			// Combined View & Projection Matrix
VkFxHandle D3D9Effect::eW = 0;			// World matrix
VkFxHandle D3D9Effect::eLVP = 0;		// Light view projection
VkFxHandle D3D9Effect::eGT = 0;			// Mesh group transformation matrix
VkFxHandle D3D9Effect::eMat = 0;		// Material
VkFxHandle D3D9Effect::eWater = 0;		// Water
VkFxHandle D3D9Effect::eMtrl = 0;
VkFxHandle D3D9Effect::eTune = 0;
VkFxHandle D3D9Effect::eSun = 0;
VkFxHandle D3D9Effect::eNight = 0;
VkFxHandle D3D9Effect::eLights = 0;		// Additional light sources

VkFxHandle D3D9Effect::eTex0 = 0;		// Primary texture
VkFxHandle D3D9Effect::eTex1 = 0;		// Secondary texture
VkFxHandle D3D9Effect::eTex3 = 0;		// Tertiary texture
VkFxHandle D3D9Effect::eSpecMap = 0;
VkFxHandle D3D9Effect::eEmisMap = 0;
VkFxHandle D3D9Effect::eEnvMapA = 0;
VkFxHandle D3D9Effect::eEnvMapB = 0;
VkFxHandle D3D9Effect::eReflMap = 0;
VkFxHandle D3D9Effect::eRghnMap = 0;
VkFxHandle D3D9Effect::eMetlMap = 0;
VkFxHandle D3D9Effect::eHeatMap = 0;
VkFxHandle D3D9Effect::eShadowMap = 0;
VkFxHandle D3D9Effect::eTranslMap = 0;
VkFxHandle D3D9Effect::eTransmMap = 0;
VkFxHandle D3D9Effect::eIrradMap = 0;

VkFxHandle D3D9Effect::eSpecularMode = 0;
VkFxHandle D3D9Effect::eHazeMode = 0;
VkFxHandle D3D9Effect::eColor = 0;		// Auxiliary color input
VkFxHandle D3D9Effect::eFogColor = 0;	// Fog color input
VkFxHandle D3D9Effect::eTexOff = 0;		// Surface tile texture offsets
VkFxHandle D3D9Effect::eTime = 0;		// FLOAT Simulation elapsed time
VkFxHandle D3D9Effect::eMix = 0;		// FLOAT Auxiliary factor/multiplier
VkFxHandle D3D9Effect::eFogDensity = 0;	// 
VkFxHandle D3D9Effect::ePointScale = 0;
VkFxHandle D3D9Effect::eSHD = 0;

VkFxHandle D3D9Effect::eAtmColor = 0;
VkFxHandle D3D9Effect::eProxySize = 0;
VkFxHandle D3D9Effect::eMtrlAlpha = 0;
VkFxHandle D3D9Effect::eKernel = 0;
VkFxHandle D3D9Effect::eAtmoParams = 0;

// Shader Flow Controls
VkFxHandle D3D9Effect::eFlow = 0;
VkFxHandle D3D9Effect::eModAlpha = 0;	// BOOL if true multiply material alpha with texture alpha
VkFxHandle D3D9Effect::eFullyLit = 0;	// BOOL
VkFxHandle D3D9Effect::eTextured = 0;	// BOOL
VkFxHandle D3D9Effect::eFresnel = 0;	// BOOL
VkFxHandle D3D9Effect::eSwitch = 0;		// BOOL
VkFxHandle D3D9Effect::eRghnSw = 0;		// BOOL
VkFxHandle D3D9Effect::eShadowToggle = 0;	// BOOL
VkFxHandle D3D9Effect::eEnvMapEnable = 0;	// BOOL
VkFxHandle D3D9Effect::eInSpace = 0;	// BOOL
VkFxHandle D3D9Effect::eNoColor = 0;	// BOOL
VkFxHandle D3D9Effect::eLightsEnabled = 0;	// BOOL	
VkFxHandle D3D9Effect::eTuneEnabled = 0; // BOOL
VkFxHandle D3D9Effect::eBaseBuilding = 0; // BOOL
VkFxHandle D3D9Effect::eOITEnable = 0; // BOOL
// --------------------------------------------------------------
VkFxHandle D3D9Effect::eExposure = 0;
VkFxHandle D3D9Effect::eCameraPos = 0;	
VkFxHandle D3D9Effect::eNorth = 0;
VkFxHandle D3D9Effect::eEast = 0;
VkFxHandle D3D9Effect::eDistScale = 0;
VkFxHandle D3D9Effect::eRadius = 0;
VkFxHandle D3D9Effect::eAttennuate = 0;
VkFxHandle D3D9Effect::eInScatter = 0;
VkFxHandle D3D9Effect::eInvProxySize = 0;
VkFxHandle D3D9Effect::eGlowConst = 0;
// --------------------------------------------------------------
VkFxHandle D3D9Effect::eGlobalAmb = 0;	 
VkFxHandle D3D9Effect::eSunAppRad = 0;	 
VkFxHandle D3D9Effect::eAmbient0 = 0;	 
VkFxHandle D3D9Effect::eDispersion = 0;	  

VkDev *D3D9Effect::pDev = 0;

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

static D3DMATERIAL9 _mfdmat = {
	{1,1,1,1},
	{1,1,1,1},
	{0,0,0,1},
	{1,1,1,1},10.0f
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


// TotalWeight = 21.337518, Count = 27, x - balance = -0.145075, y - balance = -0.012734
/*
static D3DXVECTOR3 shadow_kernel[27] = {
	{ -0.2607f, -0.9643f, 0.9995f },
	{ -0.7879f, -0.5824f, 0.9898f },
	{ -0.3796f, -0.6389f, 0.8620f },
	{ -0.1287f, -0.6631f, 0.8219f },
	{ 0.4393f, -0.6945f, 0.9065f },
	{ -0.5448f, -0.4505f, 0.8408f },
	{ -0.2330f, -0.3496f, 0.6482f },
	{ -0.1185f, -0.2572f, 0.5322f },
	{ 0.4340f, -0.4138f, 0.7744f },
	{ 0.6976f, -0.4110f, 0.8998f },
	{ -0.5738f, 0.0418f, 0.7585f },
	{ -0.3732f, -0.1313f, 0.6290f },
	{ -0.1242f, 0.0847f, 0.3877f },
	{ 0.4157f, 0.0066f, 0.6448f },
	{ 0.7117f, 0.0726f, 0.8458f },
	{ 0.8990f, 0.0801f, 0.9500f },
	{ -0.8740f, 0.2448f, 0.9527f },
	{ -0.5545f, 0.3910f, 0.8237f },
	{ -0.3458f, 0.3380f, 0.6954f },
	{ -0.1168f, 0.2083f, 0.4887f },
	{ 0.2998f, 0.4449f, 0.7325f },
	{ 0.5572f, 0.2251f, 0.7752f },
	{ -0.2250f, 0.5790f, 0.7882f },
	{ 0.0300f, 0.5853f, 0.7656f },
	{ 0.3031f, 0.6734f, 0.8594f },
	{ 0.7652f, 0.6273f, 0.9947f },
	{ -0.0571f, 0.9407f, 0.9708f }
};
*/

static D3DXVECTOR3 shadow_kernel[27] = {
	{ -0.0000f, 0.0000f, 1.0000f },
	{ 0.1915f, 0.0188f, 1.0000f },
	{ 0.0558f, -0.2664f, 1.0000f },
	{ -0.2608f, -0.2076f, 1.0000f },
	{ -0.0706f, 0.3784f, 1.0000f },
	{ 0.3207f, -0.2869f, 1.0000f },
	{ -0.2438f, -0.4035f, 1.0000f },
	{ -0.4869f, -0.1488f, 1.0000f },
	{ -0.3108f, 0.4468f, 1.0000f },
	{ 0.4330f, 0.3819f, 1.0000f },
	{ -0.4208f, -0.4396f, 1.0000f },
	{ -0.6056f, -0.2017f, 1.0000f },
	{ -0.0612f, 0.6638f, 1.0000f },
	{ 0.6489f, -0.2457f, 1.0000f },
	{ 0.1850f, -0.6959f, 1.0000f },
	{ -0.7254f, 0.1711f, 1.0000f },
	{ -0.3309f, 0.6950f, 1.0000f },
	{ 0.6890f, -0.3937f, 1.0000f },
	{ -0.5610f, -0.5933f, 1.0000f },
	{ -0.7326f, -0.4086f, 1.0000f },
	{ -0.2997f, 0.8068f, 1.0000f },
	{ 0.8774f, -0.0892f, 1.0000f },
	{ -0.1133f, -0.8955f, 1.0000f },
	{ -0.9205f, -0.0673f, 1.0000f },
	{ 0.4487f, 0.8292f, 1.0000f },
	{ 0.7320f, 0.6245f, 1.0000f },
	{ 0.6067f, -0.7713f, 1.0000f }
};


// ===========================================================================================
//
D3D9Effect::D3D9Effect() : d3d9id(0x44334439) // 'D3D9': g++ warns on multi-character constants
{

}

// ===========================================================================================
//
D3D9Effect::~D3D9Effect()
{
	
}

// ===========================================================================================
//
void D3D9Effect::GlobalExit()
{
	LogAlw("====== D3D9Effect Global Exit =======");
	SAFE_DELETE(FX);
	SAFE_DELETE(VB);
}

// ===========================================================================================
//
void D3D9Effect::ShutDown()
{
	if (!FX) return;
	FX->SetTexture(eTex0, NULL);
	FX->SetTexture(eTex1, NULL);
	FX->SetTexture(eTex3, NULL);
	FX->SetTexture(eSpecMap, NULL);
	FX->SetTexture(eEmisMap, NULL);
}


// ===========================================================================================
//
void D3D9Effect::D3D9TechInit(D3D9Client *_gc, VkDev *_pDev, const char *folder)
{
	char name[256];

	pDev = _pDev;
	gc = _gc;

	gc->OutputLoadStatus("D3D9Client.fx",1);
	
	LogAlw("Starting to initialize D3D9Client.fx a rendering technique...");
	
	// Create the Effect from a .fx file.
	VkMacros macro; // D3DXMACRO[18]
	char def[32];

	snprintf(name,256,"Modules/VulkanClient/D3D9Client.fx");

	if (Config->ShadowMapMode == 0) Config->ShadowFilter = -1;

	// ------------------------------------------------------------------------------
	snprintf(def, 32, "%d", max(2,Config->Anisotrophy));
	macro.push_back({ "ANISOTROPY_MACRO", def });
	// ------------------------------------------------------------------------------
	snprintf(def, 32, "%d", Config->LightConfig);
	macro.push_back({ "LMODE", def });
	// ------------------------------------------------------------------------------
	snprintf(def, 32, "%d", Config->MaxLights());
	macro.push_back({ "MAX_LIGHTS", def });
	// ------------------------------------------------------------------------------
	snprintf(def, 32, "%d", Config->ShadowFilter + 1);
	macro.push_back({ "SHDMAP", def });
	// ------------------------------------------------------------------------------
	if (Config->ShadowFilter >= 3)  snprintf(def, 32, "%d", 35);
	else							snprintf(def, 32, "%d", 27);
	macro.push_back({ "KERNEL_SIZE", def });
	// ------------------------------------------------------------------------------
	if (Config->ShadowFilter >= 3)  snprintf(def, 32, "%f", 0.0285f);
	else							snprintf(def, 32, "%f", 1.0f / 27.0f); // 0.04634f);
	macro.push_back({ "KERNEL_WEIGHT", def });
	// ------------------------------------------------------------------------------

	if (Config->EnableGlass) macro.push_back({ "_GLASS", "" });
	if (Config->EnableMeshDbg) macro.push_back({ "_DEBUG", "" });
	if (Config->EnvMapMode) macro.push_back({ "_ENVMAP", "" }); 
	if (Config->PostProcess == PP_DEFAULT) macro.push_back({ "_LIGHTGLOW", "" });
	if (Config->bIrradiance && Config->EnvMapMode) macro.push_back({ "_IRRADIANCE", "" });
	
	
	FX = VkEffect::Create(pDev, name, macro); // D3DXSHADER_NO_PRESHADER|D3DXSHADER_PREFER_FLOW_CONTROL: no GLSL counterpart
	
	if (!FX) { // errors buffer: the compiler's messages are in the log
		LogErr("Effect Error: %s", name);
		QMessageBox::critical(NULL, "D3D9Client.fx Error", QString("Failed to create an Effect (%1). See the log.").arg(name));
		LogErr("Critical error has occured. See Orbiter.log for details"); // FatalAppExitA
		exit(1);
	}

	// ShaderDebug: D3DXDisassembleEffect left out, the passes compile on first use (ShaderClass DISASM writes SPIR-V listings)


	// Techniques --------------------------------------------------------------

	eHazeTech	 = FX->GetTechniqueByName("HazeTech");
	ePlanetTile  = FX->GetTechniqueByName("PlanetTech");
	eBaseTile    = FX->GetTechniqueByName("BaseTileTech");
	eSimple      = FX->GetTechniqueByName("SimpleTech");
	eBBTech		 = FX->GetTechniqueByName("BoundingBoxTech");
	eTBBTech	 = FX->GetTechniqueByName("TileBoxTech");
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
	eSimpMesh	 = FX->GetTechniqueByName("SimplifiedTech");
	eGeometry    = FX->GetTechniqueByName("GeometryTech");
	eVesselTech		 = FX->GetTechniqueByName("VesselTech");
	eBaseShadowTech	 = FX->GetTechniqueByName("BaseShadowTech");
	eBeaconArrayTech = FX->GetTechniqueByName("BeaconArrayTech");
	eDiffuseTech     = FX->GetTechniqueByName("ParticleDiffuseTech");
	eEmissiveTech    = FX->GetTechniqueByName("ParticleEmissiveTech");
	
	// Flow Control Booleans -----------------------------------------------
	eModAlpha	  = FX->GetParameterByName(0,"gModAlpha");
	eFullyLit	  = FX->GetParameterByName(0,"gFullyLit");
	eFlow		  = FX->GetParameterByName(0,"gCfg");
	eShadowToggle = FX->GetParameterByName(0,"gShadowsEnabled");
	eEnvMapEnable = FX->GetParameterByName(0,"gEnvMapEnable");
	eTextured	  = FX->GetParameterByName(0,"gTextured");
	eFresnel      = FX->GetParameterByName(0,"gFresnel");
	eSwitch		  = FX->GetParameterByName(0,"gPBRSw");
	eRghnSw		  = FX->GetParameterByName(0,"gRghnSw");
	eInSpace	  = FX->GetParameterByName(0,"gInSpace");			
	eNoColor	  = FX->GetParameterByName(0,"gNoColor");	
	eLightsEnabled = FX->GetParameterByName(0,"gLightsEnabled");	
	eTuneEnabled  = FX->GetParameterByName(0,"gTuneEnabled");
	eBaseBuilding = FX->GetParameterByName(0,"gBaseBuilding");
	eOITEnable	  = FX->GetParameterByName(0,"gOITEnable");

	// General parameters -------------------------------------------------- 
	eSpecularMode = FX->GetParameterByName(0,"gSpecMode");
	eLights		  = FX->GetParameterByName(0,"gLights");
	eColor		  = FX->GetParameterByName(0,"gColor");
	eDistScale    = FX->GetParameterByName(0,"gDistScale");
	eProxySize    = FX->GetParameterByName(0,"gProxySize");
	eTexOff		  = FX->GetParameterByName(0,"gTexOff");
	eRadius       = FX->GetParameterByName(0,"gRadius");
	eCameraPos	  = FX->GetParameterByName(0,"gCameraPos");
	eNorth		  = FX->GetParameterByName(0,"gNorth");
	eEast		  = FX->GetParameterByName(0,"gEast");
	ePointScale   = FX->GetParameterByName(0,"gPointScale");
	eMix		  = FX->GetParameterByName(0,"gMix");
	eTime		  = FX->GetParameterByName(0,"gTime");
	eMtrlAlpha	  = FX->GetParameterByName(0,"gMtrlAlpha");
	eGlowConst    = FX->GetParameterByName(0,"gGlowConst");
	eSHD		  = FX->GetParameterByName(0,"gSHD");
	eKernel		  = FX->GetParameterByName(0,"kernel");
	eAtmoParams	  = FX->GetParameterByName(0,"gAtmo");
	// ----------------------------------------------------------------------
	eVP			  = FX->GetParameterByName(0,"gVP");
	eW			  = FX->GetParameterByName(0,"gW");
	eLVP		  = FX->GetParameterByName(0,"gLVP");
	eGT			  = FX->GetParameterByName(0,"gGrpT");
	// ----------------------------------------------------------------------
	eSun		  = FX->GetParameterByName(0,"gSun");
	eMat		  = FX->GetParameterByName(0,"gMat");
	eWater		  = FX->GetParameterByName(0,"gWater");
	eMtrl		  = FX->GetParameterByName(0,"gMtrl");
	eTune		  = FX->GetParameterByName(0,"gTune");
	// ----------------------------------------------------------------------
	eTex0		  = FX->GetParameterByName(0,"gTex0");
	eTex1		  = FX->GetParameterByName(0,"gTex1");
	eTex3		  = FX->GetParameterByName(0,"gTex3");
	eSpecMap	  = FX->GetParameterByName(0,"gSpecMap");
	eEmisMap	  = FX->GetParameterByName(0,"gEmisMap");
	eEnvMapA	  = FX->GetParameterByName(0,"gEnvMapA");
	eEnvMapB	  = FX->GetParameterByName(0,"gEnvMapB");
	eReflMap	  = FX->GetParameterByName(0,"gReflMap");
	eRghnMap	  = FX->GetParameterByName(0,"gRghnMap");
	eMetlMap	  = FX->GetParameterByName(0,"gMetlMap");
	eHeatMap	  = FX->GetParameterByName(0,"gHeatMap");
	eShadowMap	  = FX->GetParameterByName(0,"gShadowMap");
	eTranslMap	  = FX->GetParameterByName(0, "gTranslMap");
	eTransmMap	  = FX->GetParameterByName(0, "gTransmMap");
	eIrradMap     = FX->GetParameterByName(0,"gIrradianceMap");

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
	eNight		  = FX->GetParameterByName(0, "gNightTime");

	// Initialize default values --------------------------------------
	//
	FX->SetInt(eHazeMode, 0);
	FX->SetBool(eInSpace, false);
	FX->SetVector(eAttennuate, ptr(D3DXVECTOR4(1,1,1,1))); 
	FX->SetVector(eInScatter,  ptr(D3DXVECTOR4(0,0,0,0)));
	FX->SetVector(eColor, ptr(D3DXVECTOR4(0, 0, 0, 0)));

	//if (Config->ShadowFilter>=3) FX->SetValue(eKernel, &shadow_kernel2, sizeof(shadow_kernel2));
	FX->SetValue(eKernel, &shadow_kernel, sizeof(shadow_kernel));

	CreateMatExt(&_mfdmat, &mfdmat);
	CreateMatExt(&_defmat, &defmat);
	CreateMatExt(&_night_mat, &night_mat);
	CreateMatExt(&_emissive_mat, &emissive_mat);

	// Create a Circle Mesh --------------------------------------------
	//
	if (!VB) {
		VB = new VkBuf(pDev, 256 * sizeof(D3DXVECTOR3), VK_BUFFER_USAGE_VERTEX_BUFFER_BIT, false); // D3DPOOL_DEFAULT

		D3DXVECTOR3 pVert[256]; // Lock/Unlock: filled here and uploaded

		{
			float angle = 0.0f, step = float(PI2) / 255.0f;
			pVert[0] = D3DXVECTOR3(0, 0, 0);
			for (int i = 1; i < 256; i++) {
				pVert[i].x = 0;
				pVert[i].y = cos(angle);
				pVert[i].z = sin(angle);
				angle += step;
			}
			VB->Upload(pVert, sizeof(pVert));
		}
	}
}


void D3D9Effect::SetViewProjMatrix(LPD3DXMATRIX pVP)
{
	FX->SetMatrix(eVP, pVP);
}


void D3D9Effect::UpdateEffectCamera(OBJHANDLE hPlanet)
{
	if (!hPlanet) return;
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
	
	atm_color = D3DXVECTOR4(0.5f, 0.5f, 0.5f, 1.0f);

	const ATMCONST *atm = oapiGetPlanetAtmConstants(hPlanet); 
	VESSEL *hVessel = oapiGetFocusInterface();

	OBJHANDLE hGRef = hVessel->GetGravityRef();
	MATRIX3 grot;

	oapiGetRotationMatrix(hGRef, &grot);
	
	VECTOR3 polaraxis = mul(grot, _V(0, 1, 0));
	VECTOR3 east = unit(crossp(polaraxis, cam));
	VECTOR3 north = unit(crossp(cam, east));

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

	FX->SetValue(eEast, ptr(D3DXVEC(east)), sizeof(D3DXVECTOR3));
	FX->SetValue(eNorth, ptr(D3DXVEC(north)), sizeof(D3DXVECTOR3));
	FX->SetValue(eCameraPos, ptr(D3DXVEC(cam)), sizeof(D3DXVECTOR3));
	FX->SetVector(eRadius, ptr(D3DXVECTOR4((float)rad, radlimit, (float)len, (float)(len-rad))));
	FX->SetFloat(ePointScale, 0.5f*float(height)/tan(ap));
	FX->SetFloat(eProxySize, cos(proxy_size));
	FX->SetFloat(eInvProxySize, 1.0f/(1.0f-cos(proxy_size)));
	FX->SetFloat(eGlowConst, saturate(float(dotp(cam, sun))));
}


// ===========================================================================================
//
void D3D9Effect::EnablePlanetGlow(bool bEnabled)
{
	if (bEnabled) FX->SetVector(eAtmColor, &atm_color);
	else FX->SetVector(eAtmColor, ptr(D3DXVECTOR4(0,0,0,0)));
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
		FX->SetFloat(eAmbient0, float(min(0.7, log1p(atm->rho0)*0.4)));
		FX->SetFloat(eDispersion, float(max(0.02, min(0.9, log1p(atm->rho0)))));
	}
	else {
		FX->SetFloat(eAmbient0, 0.0f);
		FX->SetFloat(eDispersion, 0.0f);
	}
}



// ===========================================================================================
//
void D3D9Effect::Render2DPanel(const MESHGROUP *mg, const SURFHANDLE pTex, const LPD3DXMATRIX pW, float alpha, float scale, bool additive)
{
	UINT numPasses = 0;
	if (!pTex || !mg || !pW) return;

	if (SURFACE(pTex)->IsPowerOfTwo() || (!gc->IsLimited())) FX->SetTechnique(ePanelTech);		// ANISOTROPIC filter 
	else FX->SetTechnique(ePanelTechB);	// POINT filter (for non-pow2 conditional)
	
	HR(FX->SetMatrix(eW, pW));

	if (pTex) FX->SetTexture(eTex0, SURFACE(pTex)->GetTexture());
	else      FX->SetTexture(eTex0, NULL);

	HR(FX->SetFloat(eMix, alpha));
	HR(FX->Begin(&numPasses, VKFX_DONOTSAVESTATE));
	HR(FX->BeginPass(0));

	if (additive) pDev->SetDestBlend(VK_BLEND_FACTOR_ONE);
	else		  pDev->SetDestBlend(VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA);

	pDev->SetVertexDecl(pNTVertexDecl);
	pDev->DrawIndexedPrimitiveUP(VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST, mg->nVtx, VkPrimVerts(VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST, mg->nIdx/3), mg->Idx, VK_INDEX_TYPE_UINT16, mg->Vtx, sizeof(NTVERTEX));

	HR(FX->EndPass());
	HR(FX->End());	
}




// ===========================================================================================
//
void D3D9Effect::RenderReEntry(const SURFHANDLE pTex, const LPD3DXVECTOR3 vPosA, const LPD3DXVECTOR3 vPosB, const LPD3DXVECTOR3 vDir, float alpha_a, float alpha_b, float size)
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

	pDev->SetVertexDecl(pNTVertexDecl);

	FX->SetTechnique(eExhaust);
	FX->SetTexture(eTex0, SURFACE(pTex)->GetTexture());
	FX->SetFloat(eMix, alpha_b);
	FX->SetMatrix(eW, &WB);
	FX->Begin(&numPasses, VKFX_DONOTSAVESTATE);
	FX->BeginPass(0);
	pDev->DrawIndexedPrimitiveUP(VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST, 4, VkPrimVerts(VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST, 2), &ReentryIdx, VK_INDEX_TYPE_UINT16, &ReentryVtxB, sizeof(NTVERTEX));
	FX->SetFloat(eMix, alpha_a);
	FX->SetMatrix(eW, &WA);
	FX->CommitChanges();
	pDev->DrawIndexedPrimitiveUP(VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST, 4, VkPrimVerts(VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST, 2), &ReentryIdx, VK_INDEX_TYPE_UINT16, &ReentryVtxA, sizeof(NTVERTEX));

	FX->EndPass();
	FX->End();	
}


// ===========================================================================================
// This is a special rendering routine used to render beacons
//
void D3D9Effect::RenderSpot(float alpha, const LPD3DXCOLOR pColor, const LPD3DXMATRIX pW, SURFHANDLE pTex)
{
	UINT numPasses = 0;
	pDev->SetVertexDecl(pNTVertexDecl);
	HR(FX->SetTechnique(eSpotTech));
	HR(FX->SetFloat(eMix, alpha));
	HR(FX->SetValue(eColor, pColor, sizeof(D3DXCOLOR)));
	HR(FX->SetMatrix(eW, pW));
	HR(FX->SetTexture(eTex0, SURFACE(pTex)->GetTexture()));
	HR(FX->Begin(&numPasses, VKFX_DONOTSAVESTATE));
	HR(FX->BeginPass(0));
	pDev->DrawIndexedPrimitiveUP(VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST, 4, VkPrimVerts(VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST, 2), billboard_idx, VK_INDEX_TYPE_UINT16, billboard_vtx, sizeof(NTVERTEX));
	HR(FX->EndPass());
	HR(FX->End());	
}


// ===========================================================================================
// Used by Render Star only
//
void D3D9Effect::RenderBillboard(const LPD3DXMATRIX pW, VkTex *pTex, float alpha)
{
	UINT numPasses = 0;

	pDev->SetVertexDecl(pNTVertexDecl);
	HR(FX->SetTechnique(eSimple));
	HR(FX->SetMatrix(eW, pW));
	HR(FX->SetFloat(eMix, alpha));
	HR(FX->SetTexture(eTex0, pTex));
	HR(FX->Begin(&numPasses, VKFX_DONOTSAVESTATE));
	HR(FX->BeginPass(0));
	pDev->DrawIndexedPrimitiveUP(VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST, 4, VkPrimVerts(VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST, 2), billboard_idx, VK_INDEX_TYPE_UINT16, billboard_vtx, sizeof(NTVERTEX));
	HR(FX->EndPass());
	HR(FX->End());	
}


// ===========================================================================================
// This is a special rendering routine used to render engine exhaust
//
void D3D9Effect::RenderExhaust(const LPD3DXMATRIX pW, VECTOR3 &cdir, EXHAUSTSPEC *es, SURFHANDLE def)
{

	SURFHANDLE pTex = SURFACE(es->tex);
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
	pDev->SetVertexDecl(pNTVertexDecl);
	HR(FX->SetTechnique(eExhaust));
	HR(FX->SetFloat(eMix, float(alpha)));
	HR(FX->SetMatrix(eW, pW));
	HR(FX->SetTexture(eTex0, SURFACE(pTex)->GetTexture()));
	HR(FX->Begin(&numPasses, VKFX_DONOTSAVESTATE));
	HR(FX->BeginPass(0));
	pDev->DrawIndexedPrimitiveUP(VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST, 8, VkPrimVerts(VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST, 4), exhaust_idx, VK_INDEX_TYPE_UINT16, exhaust_vtx, sizeof(NTVERTEX));
	HR(FX->EndPass());
	HR(FX->End());	
}


void D3D9Effect::RenderBoundingBox(const LPD3DXMATRIX pW, const LPD3DXMATRIX pGT, const D3DXVECTOR4 *bmin, const D3DXVECTOR4 *bmax, const D3DXVECTOR4 *color)
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
	
	pDev->SetVertexDecl(pPositionDecl);

	FX->SetMatrix(eW, pW);
	FX->SetMatrix(eGT, pGT);
	FX->SetVector(eAttennuate, bmin);
	FX->SetVector(eInScatter, bmax);	
	FX->SetVector(eColor, color);	
	FX->SetTechnique(eBBTech);

	UINT numPasses = 0;
	FX->Begin(&numPasses, VKFX_DONOTSAVESTATE);
	FX->BeginPass(0);
	
	pDev->DrawPrimitiveUP(VK_PRIMITIVE_TOPOLOGY_LINE_STRIP, VkPrimVerts(VK_PRIMITIVE_TOPOLOGY_LINE_STRIP, 9), &poly, sizeof(D3DVECTOR));	
	pDev->DrawPrimitiveUP(VK_PRIMITIVE_TOPOLOGY_LINE_LIST, VkPrimVerts(VK_PRIMITIVE_TOPOLOGY_LINE_LIST, 3), &list, sizeof(D3DVECTOR));	

	FX->EndPass();
	FX->End();	
}

void D3D9Effect::RenderTileBoundingBox(const LPD3DXMATRIX pW, VECTOR4 *pVtx, const LPD3DXVECTOR4 color)
{
	D3DXVECTOR3 poly[8];

	for (int i=0;i<8;i++) poly[i] = D3DXVEC(pVtx[i]);

	WORD idc1[10] = { 0, 1, 3, 2, 0, 4, 5, 7, 6, 4 };
	WORD idc2[6] = { 1, 5, 3, 7, 2, 6};

	pDev->SetVertexDecl(pPositionDecl);

	FX->SetMatrix(eW, pW);
	FX->SetVector(eColor, color);	
	FX->SetTechnique(eTBBTech);

	UINT numPasses = 0;
	FX->Begin(&numPasses, VKFX_DONOTSAVESTATE);
	FX->BeginPass(0);
	pDev->DrawIndexedPrimitiveUP(VK_PRIMITIVE_TOPOLOGY_LINE_STRIP, 8, VkPrimVerts(VK_PRIMITIVE_TOPOLOGY_LINE_STRIP, 9), &idc1, VK_INDEX_TYPE_UINT16, &poly, sizeof(D3DXVECTOR3));
	pDev->DrawIndexedPrimitiveUP(VK_PRIMITIVE_TOPOLOGY_LINE_LIST, 8, VkPrimVerts(VK_PRIMITIVE_TOPOLOGY_LINE_LIST, 3), &idc2, VK_INDEX_TYPE_UINT16, &poly, sizeof(D3DXVECTOR3));	
	FX->EndPass();
	FX->End();	
}


void D3D9Effect::RenderLines(const D3DXVECTOR3 *pVtx, const WORD *pIdx, int nVtx, int nIdx, const D3DXMATRIX *pW, DWORD color)
{
	UINT numPasses = 0;
	pDev->SetVertexDecl(pPositionDecl);
	FX->SetMatrix(eW, pW);
	FX->SetVector(eColor, (const D3DXVECTOR4 *)ptr(D3DXCOLOR(color)));
	FX->SetTechnique(eTBBTech);
	FX->Begin(&numPasses, VKFX_DONOTSAVESTATE);
	FX->BeginPass(0);
	pDev->DrawIndexedPrimitiveUP(VK_PRIMITIVE_TOPOLOGY_LINE_LIST, nVtx, VkPrimVerts(VK_PRIMITIVE_TOPOLOGY_LINE_LIST, nIdx/2), pIdx, VK_INDEX_TYPE_UINT16, pVtx, sizeof(D3DXVECTOR3));
	FX->EndPass();
	FX->End();
}


void D3D9Effect::RenderBoundingSphere(const LPD3DXMATRIX pW, const LPD3DXMATRIX pGT, const D3DXVECTOR4 *bs, const D3DXVECTOR4 *color)
{
	D3DXMATRIX mW;
	
	D3DXVECTOR3 vCam;
	D3DXVECTOR3 vPos;
	
	D3DXVec3TransformCoord(&vPos, ptr(D3DXVECTOR3(bs->x, bs->y, bs->z)), pW);

	D3DXVec3Normalize(&vCam, &vPos);
	D3DMAT_CreateX_Billboard(&vCam, &vPos, bs->w, &mW);

	pDev->SetVertexDecl(pPositionDecl);

	FX->SetMatrix(eW, &mW);
	FX->SetVector(eColor, color);	
	FX->SetTechnique(eBSTech);

	UINT numPasses = 0;
	FX->Begin(&numPasses, VKFX_DONOTSAVESTATE);
	FX->BeginPass(0);
	
	pDev->SetStreamSource(0, VB, 0, sizeof(D3DXVECTOR3));
	pDev->DrawPrimitive(VK_PRIMITIVE_TOPOLOGY_LINE_STRIP, 0, VkPrimVerts(VK_PRIMITIVE_TOPOLOGY_LINE_STRIP, 255));	
	
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
    pDev->SetVertexDecl(pPositionDecl); // Position only vertex decleration
    HR(FX->SetTechnique(eArrowTech)); // Use arrow shader
    HR(FX->SetValue(eColor, pColor, sizeof(D3DXCOLOR))); // Setup arrow color
    HR(FX->SetMatrix(eW, &W));
    HR(FX->Begin(&numPasses, VKFX_DONOTSAVESTATE));
    HR(FX->BeginPass(0));
    pDev->DrawPrimitiveUP(VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST, VkPrimVerts(VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST, 6), &arrow, sizeof(D3DVECTOR)); // Draw 6 triangles un-indexed
    HR(FX->EndPass());
    HR(FX->End()); 
}

