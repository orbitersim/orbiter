
// ==============================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2014 - 2016 Jarmo Nikkanen
// ==============================================================


#include "PlanetRenderer.h"
#include "D3D9Config.h"
#include "AABBUtil.h"
#include "VectorHelpers.h"
#include "Scene.h"
#include "VPlanet.h"
#include "D3D9Surface.h"
#include "DebugControls.h"


// ------------------------------------------------------------
class oapi::D3D9Client *PlanetRenderer::gc = NULL; 
LPDIRECT3DDEVICE9 PlanetRenderer::pDev = NULL;
LPDIRECT3DTEXTURE9 PlanetRenderer::hOcean = NULL;
LPDIRECT3DTEXTURE9 PlanetRenderer::hMicroRot = NULL;
VECTOR3 PlanetRenderer::vLPosOld = _V(1,0,0);
bool PlanetRenderer::bEnvMapEnabled = false;
// ------------------------------------------------------------
ID3DXEffect *PlanetRenderer::pShader = NULL;
D3DXHANDLE PlanetRenderer::eTileTech = NULL;
D3DXHANDLE PlanetRenderer::eTileTechNoZ = NULL;
D3DXHANDLE PlanetRenderer::eCloudTech = NULL;
D3DXHANDLE PlanetRenderer::eRingTech = NULL;
D3DXHANDLE PlanetRenderer::eHorizonTech = NULL;
D3DXHANDLE PlanetRenderer::eSkyDomeTech = NULL;
D3DXHANDLE PlanetRenderer::eCloudShadowTech = NULL;
// ------------------------------------------------------------  
D3DXHANDLE PlanetRenderer::smWorld = NULL;
D3DXHANDLE PlanetRenderer::smViewProj = NULL;
// ------------------------------------------------------------  
D3DXHANDLE PlanetRenderer::svTexOff = NULL;
D3DXHANDLE PlanetRenderer::svWater = NULL;
D3DXHANDLE PlanetRenderer::svSunDir = NULL;
D3DXHANDLE PlanetRenderer::svGeneric = NULL;
D3DXHANDLE PlanetRenderer::svTangent = NULL;
D3DXHANDLE PlanetRenderer::svBiTangent = NULL;
D3DXHANDLE PlanetRenderer::svMapUVOffset = NULL;
D3DXHANDLE PlanetRenderer::svMicroScale0 = NULL;
D3DXHANDLE PlanetRenderer::svMicroScale1 = NULL;
D3DXHANDLE PlanetRenderer::svMicroScale2 = NULL;
// ------------------------------------------------------------
D3DXHANDLE PlanetRenderer::sfDistScale = NULL;
D3DXHANDLE PlanetRenderer::sfAlpha = NULL;
D3DXHANDLE PlanetRenderer::sfNight = NULL;
// ------------------------------------------------------------
D3DXHANDLE PlanetRenderer::sbSpecular = NULL;
D3DXHANDLE PlanetRenderer::sbCloudSh = NULL;
D3DXHANDLE PlanetRenderer::sbLights = NULL;
D3DXHANDLE PlanetRenderer::sbInSpace = NULL;
D3DXHANDLE PlanetRenderer::sbOnOff = NULL;
D3DXHANDLE PlanetRenderer::sbEnvEnable = NULL;
D3DXHANDLE PlanetRenderer::sbMicro = NULL;
D3DXHANDLE PlanetRenderer::sbMicroNormals = NULL;
D3DXHANDLE PlanetRenderer::siTileLvl = NULL;
D3DXHANDLE PlanetRenderer::siDebug = NULL;
D3DXHANDLE PlanetRenderer::sbDebug = NULL;
// ------------------------------------------------------------
D3DXHANDLE PlanetRenderer::stDiff = NULL;
D3DXHANDLE PlanetRenderer::stMask = NULL;
D3DXHANDLE PlanetRenderer::stNoise = NULL;
D3DXHANDLE PlanetRenderer::stOcean = NULL;
D3DXHANDLE PlanetRenderer::stEnvMap = NULL;
D3DXHANDLE PlanetRenderer::stMicroA = NULL;
D3DXHANDLE PlanetRenderer::stMicroB = NULL;
D3DXHANDLE PlanetRenderer::stMicroC = NULL;
D3DXHANDLE PlanetRenderer::stMicroRot = NULL;
// ------------------------------------------------------------
D3DXHANDLE PlanetRenderer::sfGlobalAmb = NULL;
D3DXHANDLE PlanetRenderer::sfAmbient0 = NULL;
// Scatter model ----------------------------------------------
D3DXHANDLE PlanetRenderer::svMPhase = NULL;		
D3DXHANDLE PlanetRenderer::svODCoEff = NULL;
D3DXHANDLE PlanetRenderer::svODCoEffEx = NULL;
D3DXHANDLE PlanetRenderer::svMieInSct = NULL;	
D3DXHANDLE PlanetRenderer::svRayInSct = NULL;
D3DXHANDLE PlanetRenderer::svTotOutSct = NULL;
D3DXHANDLE PlanetRenderer::svColorShift = NULL;
D3DXHANDLE PlanetRenderer::svWhiteBalance = NULL;
D3DXHANDLE PlanetRenderer::svCameraPos = NULL;		
D3DXHANDLE PlanetRenderer::svUnitCameraPos = NULL;		
D3DXHANDLE PlanetRenderer::sfSunset = NULL;
D3DXHANDLE PlanetRenderer::sfScaleHeight = NULL;		
D3DXHANDLE PlanetRenderer::sfInvScaleHeight = NULL;
D3DXHANDLE PlanetRenderer::sfInvMieScaleHeight = NULL;
D3DXHANDLE PlanetRenderer::sfRadius = NULL;
D3DXHANDLE PlanetRenderer::sfCameraAlt = NULL;
D3DXHANDLE PlanetRenderer::sfHorizonAlt = NULL;
D3DXHANDLE PlanetRenderer::sfAtmRad2 = NULL;
D3DXHANDLE PlanetRenderer::sfRPhase = NULL;
D3DXHANDLE PlanetRenderer::sfHorizonDst = NULL;
D3DXHANDLE PlanetRenderer::sfExposure = NULL;			
D3DXHANDLE PlanetRenderer::sfAux1 = NULL;			
D3DXHANDLE PlanetRenderer::sfAux2 = NULL;		
D3DXHANDLE PlanetRenderer::sfAux4 = NULL;	
D3DXHANDLE PlanetRenderer::sfInvAux1 = NULL;	
D3DXHANDLE PlanetRenderer::sfInvParameter = NULL;
D3DXHANDLE PlanetRenderer::sfTime = NULL;
// ------------------------------------------------------------ 



PlanetRenderer::PlanetRenderer()
{

}


PlanetRenderer::~PlanetRenderer()
{

}


void PlanetRenderer::GlobalInit (class oapi::D3D9Client *gclient)
{
	char sh_name[32];
	char name[256];

	strcpy_s(sh_name,32,"Surface.fx");
	sprintf_s(name,256,"Modules/D3D9Client/%s",sh_name);

	LogAlw("Starting to initialize %s a shading technique...", sh_name);
	
	gc = gclient;
	pDev = gc->GetDevice();
	gc->OutputLoadStatus(sh_name, 1);
	
	// Create the Effect from a .fx file.
	ID3DXBuffer* errors = 0;
	D3DXMACRO macro[16]; memset2(&macro, 0, 16*sizeof(D3DXMACRO));

	bool bRiples = *(bool*)gc->GetConfigParam(CFGPRM_SURFACERIPPLE);

	// ------------------------------------------------------------------------------
	macro[0].Name = "VS_MOD";
	macro[0].Definition = "vs_3_0";
	macro[1].Name = "PS_MOD";
	macro[1].Definition = "ps_3_0";
	// ------------------------------------------------------------------------------
	int m=2;
	macro[m].Name = "ANISOTROPY_MACRO";
	macro[m].Definition = new char[32];
	sprintf_s((char*)macro[m].Definition,32,"%d",max(2,Config->Anisotrophy));
	// ------------------------------------------------------------------------------
	m=3;
	macro[m].Name = "MICRO_ANISOTROPY";
	macro[m].Definition = new char[32];
	int micro_aniso = (1 << (max(1,Config->MicroFilter) - 1));
	sprintf_s((char*)macro[m].Definition,32,"%d", micro_aniso);
	// ------------------------------------------------------------------------------
	m=4;
	macro[m].Name = "MICRO_FILTER";
	macro[m].Definition = new char[32];
	switch(Config->MicroFilter) {
		case 0:  strcpy_s((char*)macro[m].Definition, 32, "POINT"); break;
		case 1:  strcpy_s((char*)macro[m].Definition, 32, "LINEAR"); break;
		default: strcpy_s((char*)macro[m].Definition, 32, "ANISOTROPIC"); break;
	}
	// ------------------------------------------------------------------------------
	m=5;
	macro[m].Name = "BLEND";
	macro[m].Definition = new char[4];
	switch(Config->BlendMode) {
		case 0:  strcpy_s((char*)macro[m].Definition, 4, "0"); break;
		case 1:  strcpy_s((char*)macro[m].Definition, 4, "1"); break;
		default: strcpy_s((char*)macro[m].Definition, 4, "2"); break;
	}
	m++;
	if (Config->MicroMode==2) macro[m].Name = "_MICROROTATIONS";
	if (Config->MicroMode==3) macro[m].Name = "_DEVELOPPERMODE";
	m++;
	// ------------------------------------------------------------------------------
	if (bRiples) macro[m++].Name = "_SURFACERIPPLES";
	// ------------------------------------------------------------------------------
	if (Config->EnvMapMode && bRiples) {
		macro[m++].Name = "_ENVMAP"; 
		bEnvMapEnabled = true;
	} else bEnvMapEnabled = false;
	// ------------------------------------------------------------------------------
	
	HR(D3DXCreateEffectFromFileA(pDev, name, macro, 0, 0, 0, &pShader, &errors));
	
	delete []macro[2].Definition;
	delete []macro[3].Definition;
	delete []macro[4].Definition;
	delete []macro[5].Definition;

	if (errors) {
		LogErr("Effect Error: %s",(char*)errors->GetBufferPointer());
		MessageBoxA(0, (char*)errors->GetBufferPointer(), "Surface.fx Error", 0);
		FatalAppExitA(0,"Critical error has occured. See Orbiter.log for details");
	}

	if (!pShader) {
		LogErr("Failed to create an Effect (%s)",name);
		return;
	}

	// ---------------------------------------------------------------------
	// Bind shader-side variables and constants to local handles
	//

	// Techniques ----------------------------------------------------------
	eTileTech			= pShader->GetTechniqueByName("TileTech");
	eTileTechNoZ		= pShader->GetTechniqueByName("TileTechNoZ");
	eCloudTech			= pShader->GetTechniqueByName("CloudTech");
	eRingTech			= pShader->GetTechniqueByName("RingTech");
	eHorizonTech		= pShader->GetTechniqueByName("HorizonTech");
	eSkyDomeTech		= pShader->GetTechniqueByName("SkyDomeTech");
	eCloudShadowTech	= pShader->GetTechniqueByName("CloudShadowTech");
	// ------------------------------------------------------------  
	smWorld				= pShader->GetParameterByName(0,"mWorld");
	smViewProj			= pShader->GetParameterByName(0,"mViewProj");
	// ------------------------------------------------------------  
	svTexOff			= pShader->GetParameterByName(0,"vTexOff");
	svWater				= pShader->GetParameterByName(0,"vWater");
	svSunDir			= pShader->GetParameterByName(0,"vSunDir");
	svGeneric			= pShader->GetParameterByName(0,"vGeneric");
	svTangent			= pShader->GetParameterByName(0,"vTangent");
	svBiTangent			= pShader->GetParameterByName(0,"vBiTangent");
	svMapUVOffset		= pShader->GetParameterByName(0,"vMapUVOffset");
	svMicroScale0		= pShader->GetParameterByName(0,"vMSc0");	
	svMicroScale1		= pShader->GetParameterByName(0,"vMSc1");	
	svMicroScale2		= pShader->GetParameterByName(0,"vMSc2");
	// ------------------------------------------------------------
	sfDistScale			= pShader->GetParameterByName(0,"fDistScale");
	sfAlpha				= pShader->GetParameterByName(0,"fAlpha");
	sfNight				= pShader->GetParameterByName(0,"fNight");
	// ------------------------------------------------------------
	sbSpecular			= pShader->GetParameterByName(0,"bSpecular");
	sbCloudSh			= pShader->GetParameterByName(0,"bCloudSh");
	sbLights			= pShader->GetParameterByName(0,"bLights");
	sbInSpace			= pShader->GetParameterByName(0,"bInSpace");
	sbOnOff				= pShader->GetParameterByName(0,"bOnOff");
	sbEnvEnable			= pShader->GetParameterByName(0,"bEnvEnable");
	sbMicro				= pShader->GetParameterByName(0,"bMicro");
	sbMicroNormals		= pShader->GetParameterByName(0,"bMicroNormals");
	siTileLvl			= pShader->GetParameterByName(0,"iTileLvl");
	siDebug				= pShader->GetParameterByName(0,"iDebug");
	sbDebug				= pShader->GetParameterByName(0,"bDebug");
	// ------------------------------------------------------------
	stDiff				= pShader->GetParameterByName(0,"tDiff");
	stMask				= pShader->GetParameterByName(0,"tMask");
	stNoise				= pShader->GetParameterByName(0,"tNoise");
	stOcean				= pShader->GetParameterByName(0,"tOcean");
	stEnvMap			= pShader->GetParameterByName(0,"tEnvMap");
	stMicroA			= pShader->GetParameterByName(0,"tMicroA");
	stMicroB			= pShader->GetParameterByName(0,"tMicroB");
	stMicroC			= pShader->GetParameterByName(0,"tMicroC");
	stMicroRot			= pShader->GetParameterByName(0,"tMicroRot");
	// ------------------------------------------------------------
	sfGlobalAmb			= pShader->GetParameterByName(0,"fGlobalAmb");
	sfAmbient0			= pShader->GetParameterByName(0,"fAmbient");
	// Scatter model ----------------------------------------------
	svMPhase			= pShader->GetParameterByName(0,"vMPhase");		
	svODCoEff			= pShader->GetParameterByName(0,"vODCoEff");
	svODCoEffEx			= pShader->GetParameterByName(0,"vODCoEffEx");
	svMieInSct			= pShader->GetParameterByName(0,"vMieInSct");
	svRayInSct			= pShader->GetParameterByName(0,"vRayInSct");
	svTotOutSct			= pShader->GetParameterByName(0,"vTotOutSct");
	svColorShift		= pShader->GetParameterByName(0,"vColorShift");
	svWhiteBalance		= pShader->GetParameterByName(0,"vWhiteBalance");
	svCameraPos			= pShader->GetParameterByName(0,"vCameraPos");		
	svUnitCameraPos		= pShader->GetParameterByName(0,"vUnitCameraPos");		
	sfSunset			= pShader->GetParameterByName(0,"fSunset");
	sfScaleHeight		= pShader->GetParameterByName(0,"fScaleHeight");		
	sfInvScaleHeight	= pShader->GetParameterByName(0,"fInvScaleHeight");
	sfInvMieScaleHeight	= pShader->GetParameterByName(0,"fInvMieScaleHeight");
	sfRadius			= pShader->GetParameterByName(0,"fRadius");
	sfCameraAlt			= pShader->GetParameterByName(0,"fCameraAlt");
	sfHorizonAlt		= pShader->GetParameterByName(0,"fHorizonAlt");
	sfAtmRad2			= pShader->GetParameterByName(0,"fAtmRad2");
	sfRPhase			= pShader->GetParameterByName(0,"fRPhase");
	sfHorizonDst		= pShader->GetParameterByName(0,"fHorizonDst");
	sfExposure			= pShader->GetParameterByName(0,"fExposure");			
	sfAux1				= pShader->GetParameterByName(0,"fAux1");			
	sfAux2				= pShader->GetParameterByName(0,"fAux2");
	sfAux4				= pShader->GetParameterByName(0,"fAux4");
	sfInvAux1			= pShader->GetParameterByName(0,"fInvAux1");
	sfInvParameter		= pShader->GetParameterByName(0,"fInvParameter");
	sfTime				= pShader->GetParameterByName(0,"fTime");
	// ------------------------------------------------------------
	
	
	HR(D3DXCreateTextureFromFileA(pDev, "Textures/D3D9Ocean.dds", &hOcean));
	HR(D3DXCreateTextureFromFileA(pDev, "Textures/D3D9MicroRot.dds", &hMicroRot));
	
	if (hOcean) {
		HR(pShader->SetTexture(stOcean, hOcean));
		HR(pShader->SetTexture(stMicroRot, hMicroRot));
	}
}

// -----------------------------------------------------------------------

void PlanetRenderer::GlobalExit ()
{
	SAFE_RELEASE(pShader);
	SAFE_RELEASE(hOcean);
	SAFE_RELEASE(hMicroRot);
}

// -----------------------------------------------------------------------

void PlanetRenderer::SetWorldMatrix (const MATRIX4 &W)
{
	D3DXMATRIX wtrans;
	MATRIX4toD3DMATRIX (W, wtrans);
	pShader->SetMatrix(smWorld, &wtrans);
}

// -----------------------------------------------------------------------

void PlanetRenderer::SetViewProjectionMatrix (const D3DXMATRIX *VP)
{
	pShader->SetMatrix(smViewProj, VP);
}

// -----------------------------------------------------------------------

void PlanetRenderer::InitializeScattering(vPlanet *pPlanet)
{
	D3DXVECTOR3 ucam, cam = -_D3DXVECTOR3(pPlanet->PosFromCamera());
	D3DXVec3Normalize(&ucam, &cam);

	OBJHANDLE hPlanet = pPlanet->GetObject();
	D3DXVECTOR3 SunDir = _D3DXVECTOR3(pPlanet->SunDirection());

	const ScatterParams *atmo = pPlanet->GetAtmoParams();

	DWORD dAmbient = *(DWORD*)gc->GetConfigParam(CFGPRM_AMBIENTLEVEL);
	float fAmbient = float(dAmbient)*0.0039f;

	float fWb = -float(atmo->balance);	// White balance
	double pr = pPlanet->GetSize();		// Planet radius
	double cr = pPlanet->CamDist();		// Camera distance from a planet center
	double ca = cr - pr;				// Camera altitude

	D3DXVECTOR3 whitebl = D3DXVECTOR3(pow(float(atmo->red),fWb), pow(float(atmo->green),fWb), pow(float(atmo->blue),fWb));
	whitebl *= 1.0f/whitebl.y;

	// Initialize rendering for all bodies -----------------------------------------------------
	//
	HR(Shader()->SetBool(sbOnOff, false));
	HR(Shader()->SetBool(sbDebug, (DebugControls::GetSceneDebug()>0)));
	HR(Shader()->SetInt (siDebug,  DebugControls::GetSceneDebug()));
	HR(Shader()->SetValue(svCameraPos, &cam, sizeof(D3DXVECTOR3)));
	HR(Shader()->SetValue(svUnitCameraPos, &ucam, sizeof(D3DXVECTOR3)));
	HR(Shader()->SetValue(svSunDir, &SunDir, sizeof(D3DXVECTOR3)));
	HR(Shader()->SetFloat(sfAmbient0, 0.0f));
	HR(Shader()->SetFloat(sfGlobalAmb, fAmbient));
	HR(Shader()->SetValue(svWhiteBalance, &whitebl, sizeof(D3DXVECTOR3)));
	HR(Shader()->SetFloat(sfExposure, float(atmo->expo)));
	HR(Shader()->SetFloat(sfAux4, 1.0f/float(atmo->aux4)));
	HR(Shader()->SetFloat(sfRadius, float(pr)));
	HR(Shader()->SetFloat(sfCameraAlt, float(ca)));
	HR(Shader()->SetTexture(stNoise, gc->GetNoiseTex()->GetTexture()));

	// ---------------------------------------------------------------------
	// Initialize camera centric tangent frame for normal mapped water
	//
	MATRIX3 mRot;
	oapiGetRotationMatrix(hPlanet, &mRot);
	VECTOR3 vCPos = pPlanet->PosFromCamera();
	VECTOR3 vNrm = mul(mRot, pPlanet->ReferencePoint());
	VECTOR3 vRot = mul(mRot, _V(0, 1, 0));
	VECTOR3 vTan = unit(crossp(vRot, vNrm));
	VECTOR3 vBiT = unit(crossp(vTan ,vNrm));

	// ---------------------------------------------------------------------
	HR(Shader()->SetValue(svTangent,     &D3DXVEC(vTan), sizeof(D3DXVECTOR3)));
	HR(Shader()->SetValue(svBiTangent,   &D3DXVEC(vBiT), sizeof(D3DXVECTOR3)));
	HR(Shader()->SetValue(svMapUVOffset, &D3DXVEC(vNrm-vCPos), sizeof(D3DXVECTOR3)));

	// Initialize atmospheric rendering ---------------------------------------------------
	//
	const ATMCONST *atm = oapiGetPlanetAtmConstants(hPlanet);
	if (atm==NULL) return; // No atmosphere... skip the rest


	// Initialize atmospheric rendering constants ----------------------------------------------
	//
	double *pCoEff = pPlanet->prm.ScatterCoEff;

	D3DXVECTOR4 ODCoEff   = D3DXVECTOR4(float(pCoEff[0]), float(pCoEff[1]), float(pCoEff[2]), float(pCoEff[3])); 
	D3DXVECTOR4 ODCoEffEx = D3DXVECTOR4(float(pCoEff[4]), float(pCoEff[5]), float(pCoEff[6]), float(pCoEff[7])); 

	// Phase function variables
	float   g = float(atmo->mphase);
	float   a = float((1.0-g*g) / (4.0*3.14));
	float   b = (1.0f+g*g);
	float   d = (-2.0f*g);
	float  rp = -float(atmo->rpow);
	float  h0 = float(atmo->height*1e3);				// Scale height
	float hm0 = float(atmo->mheight*1e3);
	float  mp = -float(atmo->mpow);
	float  cs =  float(atmo->aux3);

	double me = pPlanet->GetMinElevation();				// Minimum elevation
	double mr = pr+me;									// Minimum radius
	double ur = pr + pPlanet->GetHorizonAlt();			// Atmosphere upper radius (Skydome radius)
	double hd = sqrt(cr*cr-mr*mr);						// Camera to horizon distance
	double tm = fmod(oapiGetSimTime(), 256.0);

	float  fA = float(sqrt(1.0 - (mr*mr)/(cr*cr)));
	float  fB = (-D3DXVec3Dot(&SunDir, &ucam)-fA) / (float(atmo->aux1+0.01)*0.64f);
	float  fP = float(pPlanet->AngleCoEff(0.0));

	float fCorrect = 0.5f + saturate(fB)*1.5f;

	// 1.0 / lambda^4
	D3DXVECTOR3 lambda4 = D3DXVECTOR3(pow(float(atmo->red),rp), pow(float(atmo->green),rp), pow(float(atmo->blue),rp));
	D3DXVECTOR3 lambda2 = D3DXVECTOR3(pow(float(atmo->red),mp), pow(float(atmo->green),mp), pow(float(atmo->blue),mp));
	
	D3DXVec3Normalize(&lambda4, &lambda4);
	D3DXVec3Normalize(&lambda2, &lambda2);

	D3DXVECTOR3 vTotOutSct = lambda4*float(atmo->rout) + lambda2*float(atmo->mie);
	D3DXVECTOR3 vMieInSct  = lambda2*float(atmo->mie);	
	D3DXVECTOR3 vRayInSct  = lambda4*float(atmo->rin * atmo->rout);
	D3DXVECTOR3 vColorShift = lerp(D3DXVECTOR3(1,1,1), lambda4, float(atmo->aux3)*fCorrect) * float(-atmo->expo);

	// Camara altitude dependency multiplier for ambient color of atmosphere
    float fMult = saturate((h0-float(ca*0.1))/h0);
	float fAmbLoc = fMult * min(0.7f, log(float(atm->rho0)+1.0f)*0.4f);

	// Upload parameters to shaders
	HR(Shader()->SetFloat(sfAmbient0, fAmbLoc));
	HR(Shader()->SetValue(svMPhase, &D3DXVECTOR4(a,b,0,d), sizeof(D3DXVECTOR4)));
	HR(Shader()->SetValue(svODCoEff, &D3DXVECTOR4(float(pCoEff[0]), float(pCoEff[1]), float(pCoEff[2]), float(pCoEff[3])), sizeof(D3DXVECTOR4)));
	HR(Shader()->SetValue(svODCoEffEx, &D3DXVECTOR4(float(pCoEff[4]), float(pCoEff[5]), float(pCoEff[6]), float(pCoEff[7])), sizeof(D3DXVECTOR4)));
	HR(Shader()->SetValue(svTotOutSct, &vTotOutSct, sizeof(D3DXVECTOR3)));
	HR(Shader()->SetValue(svMieInSct, &vMieInSct, sizeof(D3DXVECTOR3)));
	HR(Shader()->SetValue(svRayInSct, &vRayInSct, sizeof(D3DXVECTOR3)));
	HR(Shader()->SetValue(svColorShift, &vColorShift, sizeof(D3DXVECTOR3)));
	HR(Shader()->SetFloat(sfSunset, float(atmo->depth)));
	HR(Shader()->SetFloat(sfRPhase, float(atmo->rphase)));
	HR(Shader()->SetFloat(sfAux1, float(atmo->aux1)));
	HR(Shader()->SetFloat(sfAux2, float(atmo->aux2)));
	HR(Shader()->SetFloat(sfInvAux1, 1.0f/float(atmo->aux1)));
	HR(Shader()->SetFloat(sfInvParameter, 1.0f/fP));
	HR(Shader()->SetFloat(sfScaleHeight, h0));
	HR(Shader()->SetFloat(sfInvScaleHeight, 1.0f/h0));
	HR(Shader()->SetFloat(sfInvMieScaleHeight, 1.0f/hm0));
	HR(Shader()->SetFloat(sfHorizonAlt, float(ur-pr)));
	HR(Shader()->SetFloat(sfAtmRad2, float(ur*ur)));
	HR(Shader()->SetFloat(sfHorizonDst, float(hd)));
	HR(Shader()->SetFloat(sfTime, float(tm)));
	HR(Shader()->SetBool(sbInSpace, (cr>ur)));
	HR(Shader()->SetBool(sbOnOff, true));
}