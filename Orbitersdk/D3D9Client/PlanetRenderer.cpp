
// ==============================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2014 Jarmo Nikkanen
// ==============================================================


#include "PlanetRenderer.h"
#include "D3D9Config.h"
#include "AABBUtil.h"
#include "Scene.h"
#include "VPlanet.h"


// ------------------------------------------------------------
class oapi::D3D9Client *PlanetRenderer::gc = NULL; 
LPDIRECT3DDEVICE9 PlanetRenderer::pDev = NULL;
// ------------------------------------------------------------
ID3DXEffect *PlanetRenderer::pShader = NULL;
D3DXHANDLE PlanetRenderer::eTileTech = NULL;
D3DXHANDLE PlanetRenderer::eTileTechNoZ = NULL;
D3DXHANDLE PlanetRenderer::eCloudTech = NULL;
D3DXHANDLE PlanetRenderer::eRingTech = NULL;
D3DXHANDLE PlanetRenderer::eHorizonTech = NULL;
D3DXHANDLE PlanetRenderer::eSkyDomeTech = NULL;
// ------------------------------------------------------------  
D3DXHANDLE PlanetRenderer::smWorld = NULL;
D3DXHANDLE PlanetRenderer::smViewProj = NULL;
// ------------------------------------------------------------  
D3DXHANDLE PlanetRenderer::svTexOff = NULL;
D3DXHANDLE PlanetRenderer::svWater = NULL;
D3DXHANDLE PlanetRenderer::svSunDir = NULL;
D3DXHANDLE PlanetRenderer::svAddBkg = NULL;
// ------------------------------------------------------------
D3DXHANDLE PlanetRenderer::sfDistScale = NULL;
D3DXHANDLE PlanetRenderer::sfAlpha = NULL;
D3DXHANDLE PlanetRenderer::sfNight = NULL;
// ------------------------------------------------------------
D3DXHANDLE PlanetRenderer::sbSpecular = NULL;
D3DXHANDLE PlanetRenderer::sbCloudSh = NULL;
D3DXHANDLE PlanetRenderer::sbLights = NULL;
// ------------------------------------------------------------
D3DXHANDLE PlanetRenderer::stDiff = NULL;
D3DXHANDLE PlanetRenderer::stMask = NULL;
// Atmosphere -------------------------------------------------
//D3DXHANDLE PlanetRenderer::svFogColor = NULL;
//D3DXHANDLE PlanetRenderer::sfFogDensity = NULL;
//D3DXHANDLE PlanetRenderer::sfSunAppRad = NULL;
//D3DXHANDLE PlanetRenderer::sfDispersion = NULL;

D3DXHANDLE PlanetRenderer::sfGlobalAmb = NULL;
D3DXHANDLE PlanetRenderer::sfAmbient0 = NULL;
// Scatter model ----------------------------------------------
D3DXHANDLE PlanetRenderer::svPhase = NULL;		
D3DXHANDLE PlanetRenderer::svODCoEff = NULL;
D3DXHANDLE PlanetRenderer::svODCoEffEx = NULL;
D3DXHANDLE PlanetRenderer::svRayTotal = NULL;	
D3DXHANDLE PlanetRenderer::svRayInSct = NULL;
D3DXHANDLE PlanetRenderer::svRaySurface = NULL;
D3DXHANDLE PlanetRenderer::svMieTotal = NULL;
D3DXHANDLE PlanetRenderer::svCameraPos = NULL;		
D3DXHANDLE PlanetRenderer::svUnitCameraPos = NULL;		
D3DXHANDLE PlanetRenderer::sfDepthClamp = NULL;
D3DXHANDLE PlanetRenderer::sfSrfIntensity = NULL;
D3DXHANDLE PlanetRenderer::sfScaleHeight = NULL;		
D3DXHANDLE PlanetRenderer::sfInvScaleHeight = NULL;
D3DXHANDLE PlanetRenderer::sfRadius = NULL;
D3DXHANDLE PlanetRenderer::sfCameraAlt = NULL;
D3DXHANDLE PlanetRenderer::sfHorizonAlt = NULL;
D3DXHANDLE PlanetRenderer::sfAtmRad2 = NULL;
D3DXHANDLE PlanetRenderer::sfBalance = NULL;
D3DXHANDLE PlanetRenderer::sfHorizonDst = NULL;
D3DXHANDLE PlanetRenderer::sfExposure = NULL;			
D3DXHANDLE PlanetRenderer::sfAux1 = NULL;			
D3DXHANDLE PlanetRenderer::sfAux2 = NULL;		
D3DXHANDLE PlanetRenderer::siMode = NULL;
D3DXHANDLE PlanetRenderer::sbOverSat = false;
D3DXHANDLE PlanetRenderer::sbInSpace = false;
D3DXHANDLE PlanetRenderer::sbOnOff = false;




PlanetRenderer::PlanetRenderer()
{

}


PlanetRenderer::~PlanetRenderer()
{

}


void PlanetRenderer::GlobalInit (class oapi::D3D9Client *gclient)
{
	LogAlw("Starting to initialize Surface.fx a shading technique...");

	gc = gclient;
	pDev = gc->GetDevice();

	gc->clbkSplashLoadMsg("Surface.fx",1);

	char name[256];

	WORD Model = gc->GetHardwareCaps()->PixelShaderVersion & 0xFFFF;

	if (!strcmp(Config->Shaders,"Level20")) Model = 0x200;

	// Create the Effect from a .fx file.
	ID3DXBuffer* errors = 0;
	D3DXMACRO macro[8]; memset2(&macro, 0, 8*sizeof(D3DXMACRO));

	macro[0].Name = "VS_MOD";
	macro[1].Name = "PS_MOD";

	if (Model==0x200) {
		LogAlw("[Compiling Effects for Shader Model 2.0]");
		oapiWriteLog("D3D9Client: [Compiling Effects for Shader Model 2.0]");
		macro[0].Definition = "vs_2_0";
		macro[1].Definition = "ps_2_0";
		sprintf_s(name,256,"Modules/D3D9Client20/Surface.fx");
	}
	else {
		LogAlw("[Compiling Effects for Shader Model 3.0]");
		oapiWriteLog("D3D9Client: [Compiling Effects for Shader Model 3.0]");
		macro[0].Definition = "vs_3_0";
		macro[1].Definition = "ps_3_0";
		sprintf_s(name,256,"Modules/D3D9Client/Surface.fx");
	}
	
	
	macro[2].Name = "ANISOTROPY_MACRO";
	macro[2].Definition = new char[32];
	sprintf_s((char*)macro[2].Definition,32,"%d",max(2,Config->Anisotrophy));
	
	HR(D3DXCreateEffectFromFileA(pDev, name, macro, 0, 0, 0, &pShader, &errors));
	
	delete []macro[2].Definition;

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
	// ------------------------------------------------------------  
	smWorld				= pShader->GetParameterByName(0,"mWorld");
	smViewProj			= pShader->GetParameterByName(0,"mViewProj");
	// ------------------------------------------------------------  
	svTexOff			= pShader->GetParameterByName(0,"vTexOff");
	svWater				= pShader->GetParameterByName(0,"vWater");
	svSunDir			= pShader->GetParameterByName(0,"vSunDir");
	svAddBkg			= pShader->GetParameterByName(0,"vAddBkg");
	// ------------------------------------------------------------
	sfDistScale			= pShader->GetParameterByName(0,"fDistScale");
	sfAlpha				= pShader->GetParameterByName(0,"fAlpha");
	sfNight				= pShader->GetParameterByName(0,"fNight");
	// ------------------------------------------------------------
	sbSpecular			= pShader->GetParameterByName(0,"bSpecular");
	sbCloudSh			= pShader->GetParameterByName(0,"bCloudSh");
	sbLights			= pShader->GetParameterByName(0,"bLights");
	// ------------------------------------------------------------
	stDiff				= pShader->GetParameterByName(0,"tDiff");
	stMask				= pShader->GetParameterByName(0,"tMask");
	// ------------------------------------------------------------
	sfGlobalAmb			= pShader->GetParameterByName(0,"fGlobalAmb");
	sfAmbient0			= pShader->GetParameterByName(0,"fAmbient0");
	// Scatter model --------------------------------------------------------
	svPhase				= pShader->GetParameterByName(0,"vPhase");		
	svODCoEff			= pShader->GetParameterByName(0,"vODCoEff");
	svODCoEffEx			= pShader->GetParameterByName(0,"vODCoEffEx");
	svRayTotal			= pShader->GetParameterByName(0,"vRayTotal");
	svRayInSct			= pShader->GetParameterByName(0,"vRayInSct");
	svRaySurface		= pShader->GetParameterByName(0,"vRaySurface");
	svMieTotal			= pShader->GetParameterByName(0,"vMieTotal");
	svCameraPos			= pShader->GetParameterByName(0,"vCameraPos");		
	svUnitCameraPos		= pShader->GetParameterByName(0,"vUnitCameraPos");		
	sfDepthClamp		= pShader->GetParameterByName(0,"fDepthClamp");
	sfSrfIntensity		= pShader->GetParameterByName(0,"fSrfIntensity");
	sfScaleHeight		= pShader->GetParameterByName(0,"fScaleHeight");		
	sfInvScaleHeight	= pShader->GetParameterByName(0,"fInvScaleHeight");
	sfRadius			= pShader->GetParameterByName(0,"fRadius");
	sfCameraAlt			= pShader->GetParameterByName(0,"fCameraAlt");
	sfHorizonAlt		= pShader->GetParameterByName(0,"fHorizonAlt");
	sfAtmRad2			= pShader->GetParameterByName(0,"fAtmRad2");
	sfBalance			= pShader->GetParameterByName(0,"fBalance");
	sfHorizonDst		= pShader->GetParameterByName(0,"fHorizonDst");
	sfExposure			= pShader->GetParameterByName(0,"fExposure");			
	sfAux1				= pShader->GetParameterByName(0,"fAux1");			
	sfAux2				= pShader->GetParameterByName(0,"fAux2");		
	siMode				= pShader->GetParameterByName(0,"iMode");
	sbOverSat			= pShader->GetParameterByName(0,"bOverSat");
	sbInSpace			= pShader->GetParameterByName(0,"bInSpace");
	sbOnOff				= pShader->GetParameterByName(0,"bOnOff");
}

// -----------------------------------------------------------------------

void PlanetRenderer::GlobalExit ()
{
	SAFE_RELEASE(pShader);
}

// -----------------------------------------------------------------------

void PlanetRenderer::SetWorldMatrix (const MATRIX4 &W)
{
	D3DXMATRIX wtrans;
	MATRIX4toD3DMATRIX (W, wtrans);
	pShader->SetMatrix(smWorld, &wtrans);
}

// -----------------------------------------------------------------------

void PlanetRenderer::SetViewProjectionMatrix (const MATRIX4 &VP)
{
	D3DXMATRIX wtrans;
	MATRIX4toD3DMATRIX (VP, wtrans);
	pShader->SetMatrix(smViewProj, &wtrans);
}

// -----------------------------------------------------------------------

void PlanetRenderer::SetWorldMatrix (const D3DXMATRIX &W)
{
	pShader->SetMatrix(smWorld, &W);
}

// -----------------------------------------------------------------------

void PlanetRenderer::SetViewProjectionMatrix (const D3DXMATRIX &VP)
{
	pShader->SetMatrix(smViewProj, &VP);
}

// -----------------------------------------------------------------------

void PlanetRenderer::InitializeScattering(vPlanet *pPlanet)
{
	D3DXVECTOR3 ucam, cam = -_D3DXVECTOR3(pPlanet->PosFromCamera());
	D3DXVec3Normalize(&ucam, &cam);

	const ScatterParams *atmo = pPlanet->GetAtmoParams();

	// Phase function variables
	float   g = float(atmo->mphase);
	float   a = (1.0f-g*g) / (4.0*3.14);
	float   b = (1.0f+g*g);
	float   d = (-2.0f*g);
	float   c = float(atmo->rphase);
	float  rp = float(atmo->wavepow);
	float  h0 = float(atmo->height*1e3);				// Scale height
	float  mp = 1.0f;
	double pr = pPlanet->GetSize();						// Planet radius
	double ur = pr + pPlanet->prm.atm_hzalt;			// Atmosphere upper radius (Skydome radius)
	double cr = pPlanet->CamDist();						// Camera distance from a planet center
	double ca = cr - pr;								// Camera altutude
	double hd = sqrt(cr*cr-pr*pr);						// Camera to horizon distance

	OBJHANDLE hPlanet = pPlanet->GetObjectA();
	OBJHANDLE hSun = oapiGetGbodyByIndex(0);
	VECTOR3 gpos, gsun;
	oapiGetGlobalPos(hPlanet, &gpos);
	oapiGetGlobalPos(hSun, &gsun);

	const ATMCONST *atm = (oapiGetObjectType(hPlanet)==OBJTP_PLANET ? oapiGetPlanetAtmConstants (hPlanet) : NULL);

	D3DXVECTOR4 ODCoEff = pPlanet->prm.ODCoEff;
	D3DXVECTOR4 ODCoEffEx = pPlanet->prm.ODCoEffEx;
	D3DXVECTOR3 SunDir = _D3DXVECTOR3(unit(gsun-gpos));

	// 1.0 / lambda^4
	D3DXVECTOR3 lambda4 = D3DXVECTOR3(1.0f/pow(float(atmo->red),rp),  1.0f/pow(float(atmo->green),rp),  1.0f/pow(float(atmo->blue),rp));
	D3DXVECTOR3 lambda2 = D3DXVECTOR3(1.0f/pow(float(atmo->red),mp),  1.0f/pow(float(atmo->green),mp),  1.0f/pow(float(atmo->blue),mp));
	
	D3DXVec3Normalize(&lambda4, &lambda4);
	D3DXVec3Normalize(&lambda2, &lambda2);

	D3DXVECTOR3 raytot = lambda4 * float(atmo->rout);
	D3DXVECTOR3 raysct = raytot  * float(atmo->rin);
	D3DXVECTOR3 raysrf = raytot  * float(atmo->srfclr);
	D3DXVECTOR3 mietot = lambda2 * float(atmo->mie);

	DWORD dAmbient = *(DWORD*)gc->GetConfigParam(CFGPRM_AMBIENTLEVEL);
	float fAmbient = float(dAmbient)*0.0039f;

	if (atm) Shader()->SetFloat(sfAmbient0, min(0.7f, log(float(atm->rho0)+1.0f)*0.4f));
	else     Shader()->SetFloat(sfAmbient0, 0.0f);

	// Upload parameters to shaders
	HR(Shader()->SetValue(svPhase, &D3DXVECTOR4(a,b,c,d), sizeof(D3DXVECTOR4)));
	HR(Shader()->SetValue(svODCoEff, &ODCoEff, sizeof(D3DXVECTOR4)));
	HR(Shader()->SetValue(svODCoEffEx, &ODCoEffEx, sizeof(D3DXVECTOR4)));
	HR(Shader()->SetValue(svRayTotal, &raytot, sizeof(D3DXVECTOR3)));
	HR(Shader()->SetValue(svRayInSct, &raysct, sizeof(D3DXVECTOR3)));
	HR(Shader()->SetValue(svRaySurface, &raysrf, sizeof(D3DXVECTOR3)));
	HR(Shader()->SetValue(svMieTotal, &mietot, sizeof(D3DXVECTOR3)));
	HR(Shader()->SetValue(svCameraPos, &cam, sizeof(D3DXVECTOR3)));
	HR(Shader()->SetValue(svUnitCameraPos, &ucam, sizeof(D3DXVECTOR3)));
	HR(Shader()->SetValue(svSunDir, &SunDir, sizeof(D3DXVECTOR3)));
	HR(Shader()->SetFloat(sfDepthClamp, float(atmo->depth)));
	HR(Shader()->SetFloat(sfSrfIntensity, float(atmo->sun)));
	HR(Shader()->SetFloat(sfBalance, float(atmo->balance)));
	HR(Shader()->SetFloat(sfExposure, float(-atmo->expo)));
	HR(Shader()->SetFloat(sfAux1, float(atmo->aux1)));
	HR(Shader()->SetFloat(sfAux2, float(atmo->aux2)));
	HR(Shader()->SetFloat(sfGlobalAmb, fAmbient));
	HR(Shader()->SetFloat(sfScaleHeight, h0));
	HR(Shader()->SetFloat(sfInvScaleHeight, 1.0f/h0));
	HR(Shader()->SetFloat(sfRadius, float(pr)));
	HR(Shader()->SetFloat(sfCameraAlt, float(ca)));
	HR(Shader()->SetFloat(sfHorizonAlt, float(ur-pr)));
	HR(Shader()->SetFloat(sfAtmRad2, float(ur*ur)));
	HR(Shader()->SetFloat(sfHorizonDst, float(hd)));
	HR(Shader()->SetBool(sbOverSat, atmo->oversat));
	HR(Shader()->SetBool(sbInSpace, (cr>ur)));
	HR(Shader()->SetBool(sbOnOff, (atmo->mode==0)));
	HR(Shader()->SetInt(siMode, atmo->mode));
}