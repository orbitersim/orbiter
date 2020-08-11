
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
LPDIRECT3DTEXTURE9 PlanetRenderer::hCloudMicro = NULL;
LPDIRECT3DTEXTURE9 PlanetRenderer::hCloudMicroNorm = NULL;
VECTOR3 PlanetRenderer::vLPosOld = _V(1,0,0);
bool PlanetRenderer::bEnvMapEnabled = false;
// ------------------------------------------------------------
ID3DXEffect *PlanetRenderer::pShader = NULL;
D3DXHANDLE PlanetRenderer::eTileTech = NULL;
D3DXHANDLE PlanetRenderer::eCloudTech = NULL;
D3DXHANDLE PlanetRenderer::eRingTech = NULL;
D3DXHANDLE PlanetRenderer::eHorizonTech = NULL;
D3DXHANDLE PlanetRenderer::eSkyDomeTech = NULL;
// ------------------------------------------------------------  
D3DXHANDLE PlanetRenderer::ssLight = NULL;
// ------------------------------------------------------------  
D3DXHANDLE PlanetRenderer::smWorld = NULL;
D3DXHANDLE PlanetRenderer::smViewProj = NULL;
D3DXHANDLE PlanetRenderer::smLVP = NULL;
// ------------------------------------------------------------  
D3DXHANDLE PlanetRenderer::svTexOff = NULL;
D3DXHANDLE PlanetRenderer::svWater = NULL;
D3DXHANDLE PlanetRenderer::svSunDir = NULL;
D3DXHANDLE PlanetRenderer::svCloudOff = NULL;
D3DXHANDLE PlanetRenderer::svMicroOff = NULL;
D3DXHANDLE PlanetRenderer::svOverlayOff = NULL;
D3DXHANDLE PlanetRenderer::svTangent = NULL;
D3DXHANDLE PlanetRenderer::svBiTangent = NULL;
D3DXHANDLE PlanetRenderer::svPolarAxis = NULL;
D3DXHANDLE PlanetRenderer::svMicroScale0 = NULL;
D3DXHANDLE PlanetRenderer::svMicroScale1 = NULL;
D3DXHANDLE PlanetRenderer::svMicroScale2 = NULL;
D3DXHANDLE PlanetRenderer::svSHD = NULL;
// ------------------------------------------------------------
D3DXHANDLE PlanetRenderer::sfDistScale = NULL;
D3DXHANDLE PlanetRenderer::sfAlpha = NULL;
D3DXHANDLE PlanetRenderer::sfNight = NULL;
// ------------------------------------------------------------
D3DXHANDLE PlanetRenderer::sbCloudSh = NULL;
D3DXHANDLE PlanetRenderer::sbLights = NULL;
D3DXHANDLE PlanetRenderer::sbLocals = NULL;
D3DXHANDLE PlanetRenderer::sbInSpace = NULL;
D3DXHANDLE PlanetRenderer::sbOnOff = NULL;
D3DXHANDLE PlanetRenderer::sbEnvEnable = NULL;
D3DXHANDLE PlanetRenderer::sbMicroNormals = NULL;
D3DXHANDLE PlanetRenderer::siTileLvl = NULL;
D3DXHANDLE PlanetRenderer::siDebug = NULL;
D3DXHANDLE PlanetRenderer::sbDebug = NULL;
D3DXHANDLE PlanetRenderer::sbShadows = NULL;
D3DXHANDLE PlanetRenderer::sbOverlay = NULL;
D3DXHANDLE PlanetRenderer::sbSpherical = NULL;
D3DXHANDLE PlanetRenderer::sbCloudNorm = NULL;
D3DXHANDLE PlanetRenderer::sbEarth = NULL;
// ------------------------------------------------------------
D3DXHANDLE PlanetRenderer::stDiff = NULL;
D3DXHANDLE PlanetRenderer::stMask = NULL;
D3DXHANDLE PlanetRenderer::stCloud = NULL;
D3DXHANDLE PlanetRenderer::stCloud2 = NULL;
D3DXHANDLE PlanetRenderer::stNoise = NULL;
D3DXHANDLE PlanetRenderer::stOcean = NULL;
D3DXHANDLE PlanetRenderer::stEnvMap = NULL;
D3DXHANDLE PlanetRenderer::stMicroA = NULL;
D3DXHANDLE PlanetRenderer::stMicroB = NULL;
D3DXHANDLE PlanetRenderer::stMicroC = NULL;
D3DXHANDLE PlanetRenderer::stMicroRot = NULL;
D3DXHANDLE PlanetRenderer::stShadowMap = NULL;
D3DXHANDLE PlanetRenderer::stCloudMicro = NULL;
D3DXHANDLE PlanetRenderer::stCloudMicroNorm = NULL;
D3DXHANDLE PlanetRenderer::stOverlay = NULL;
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
D3DXHANDLE PlanetRenderer::svHazeMax = NULL;
D3DXHANDLE PlanetRenderer::svCameraPos = NULL;		
D3DXHANDLE PlanetRenderer::svUnitCameraPos = NULL;		
D3DXHANDLE PlanetRenderer::sfCloudInts = NULL;
D3DXHANDLE PlanetRenderer::sfScaleHeight = NULL;		
D3DXHANDLE PlanetRenderer::sfInvScaleHeight = NULL;
D3DXHANDLE PlanetRenderer::sfSunAlt = NULL;
D3DXHANDLE PlanetRenderer::sfRadius = NULL;
D3DXHANDLE PlanetRenderer::sfCameraAlt = NULL;
D3DXHANDLE PlanetRenderer::sfHorizonAlt = NULL;
D3DXHANDLE PlanetRenderer::sfAtmRad2 = NULL;
D3DXHANDLE PlanetRenderer::sfRPhase = NULL;
D3DXHANDLE PlanetRenderer::sfHorizonDst = NULL;
D3DXHANDLE PlanetRenderer::sfExposure = NULL;			
D3DXHANDLE PlanetRenderer::sfAux1 = NULL;			
D3DXHANDLE PlanetRenderer::sfAux2 = NULL;		
D3DXHANDLE PlanetRenderer::sfTrGamma = NULL;	
D3DXHANDLE PlanetRenderer::sfAtmGamma = NULL;
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
	char name[MAX_PATH] = "Modules\\D3D9Client\\";
	char *sh_name = "Surface.fx";

	strcat_s(name, ARRAYSIZE(name), sh_name);

	LogAlw("Starting to initialize %s a shading technique...", sh_name);
	
	gc = gclient;
	pDev = gc->GetDevice();
	gc->OutputLoadStatus(sh_name, 1);
	
	// Create the Effect from a .fx file.
	ID3DXBuffer* errors = 0;
	D3DXMACRO macro[16]; memset2(&macro, 0, 16*sizeof(D3DXMACRO));

	bool bRiples = *(bool*)gc->GetConfigParam(CFGPRM_SURFACERIPPLE);
	bool bShadows = *(bool*)gc->GetConfigParam(CFGPRM_CLOUDSHADOWS);

	// ------------------------------------------------------------------------------
	int m=0;
	macro[m].Name = "ANISOTROPY_MACRO";
	macro[m].Definition = new char[32];
	sprintf_s((char*)macro[m].Definition,32,"%d",max(2,Config->Anisotrophy));
	// ------------------------------------------------------------------------------
	m=1;
	macro[m].Name = "MICRO_ANISOTROPY";
	macro[m].Definition = new char[32];
	int micro_aniso = (1 << (max(1,Config->MicroFilter) - 1));
	sprintf_s((char*)macro[m].Definition,32,"%d", micro_aniso);
	// ------------------------------------------------------------------------------
	m=2;
	macro[m].Name = "MICRO_FILTER";
	macro[m].Definition = new char[32];
	switch(Config->MicroFilter) {
		case 0:  strcpy_s((char*)macro[m].Definition, 32, "POINT"); break;
		case 1:  strcpy_s((char*)macro[m].Definition, 32, "LINEAR"); break;
		default: strcpy_s((char*)macro[m].Definition, 32, "ANISOTROPIC"); break;
	}
	// ------------------------------------------------------------------------------
	m=3;
	macro[m].Name = "BLEND";
	macro[m].Definition = new char[4];
	switch(Config->BlendMode) {
		case 0:  strcpy_s((char*)macro[m].Definition, 4, "0"); break;
		case 1:  strcpy_s((char*)macro[m].Definition, 4, "1"); break;
		default: strcpy_s((char*)macro[m].Definition, 4, "2"); break;
	}
	// ------------------------------------------------------------------------------
	m=4;
	macro[m].Name = "MICRO_BIAS";
	macro[m].Definition = new char[8];
	sprintf_s((char*)macro[m].Definition,8,"%1.1f",float(Config->MicroBias)*0.1f);
	// ------------------------------------------------------------------------------
	m=5;
	// ------------------------------------------------------------------------------
	if (Config->EnvMapMode && bRiples) {
		macro[m++].Name = "_ENVMAP"; 
		bEnvMapEnabled = true;
	} else bEnvMapEnabled = false;
	// ------------------------------------------------------------------------------
	if (bShadows) macro[m++].Name = "_CLOUDSHADOWS";
	// ------------------------------------------------------------------------------
	if (Config->ShadowMapMode) macro[m++].Name = "_SHDMAP";
	// ------------------------------------------------------------------------------
	if (Config->CloudMicro) macro[m++].Name = "_CLOUDMICRO";
	// ------------------------------------------------------------------------------
	if (Config->bCloudNormals) macro[m++].Name = "_CLOUDNORMALS";

	HR(D3DXCreateEffectFromFileA(pDev, name, macro, 0, 0, 0, &pShader, &errors));
	
	delete []macro[0].Definition;
	delete []macro[1].Definition;
	delete []macro[2].Definition;
	delete []macro[3].Definition;
	delete []macro[4].Definition;

	if (errors) {
		LogErr("Effect Error: %s",(char*)errors->GetBufferPointer());
		MessageBoxA(0, (char*)errors->GetBufferPointer(), "Surface.fx Error", 0);
		FatalAppExitA(0,"Critical error has occured. See Orbiter.log for details");
	}

	if (!pShader) {
		LogErr("Failed to create an Effect (%s)",name);
		return;
	}

	if (Config->ShaderDebug) {
		LPD3DXBUFFER pBuffer = NULL;
		if (D3DXDisassembleEffect(pShader, true, &pBuffer) == S_OK) {
			FILE *fp = NULL;
			if (!fopen_s(&fp, "D9D9Planet_asm.html", "w")) {
				pBuffer->GetBufferPointer();
				fwrite(pBuffer->GetBufferPointer(), 1, pBuffer->GetBufferSize(), fp);
				fclose(fp);
			}
			pBuffer->Release();
		}
	}


	// ---------------------------------------------------------------------
	// Bind shader-side variables and constants to local handles
	//

	
	

	// Techniques ----------------------------------------------------------
	eTileTech			= pShader->GetTechniqueByName("TileTech");
	eCloudTech			= pShader->GetTechniqueByName("CloudTech");
	eRingTech			= pShader->GetTechniqueByName("RingTech");
	eHorizonTech		= pShader->GetTechniqueByName("HorizonTech");
	eSkyDomeTech		= pShader->GetTechniqueByName("SkyDomeTech");
	// ------------------------------------------------------------  
	ssLight				= pShader->GetParameterByName(0,"sLights");
	// ------------------------------------------------------------  
	smWorld				= pShader->GetParameterByName(0,"mWorld");
	smViewProj			= pShader->GetParameterByName(0,"mViewProj");
	smLVP				= pShader->GetParameterByName(0,"mLVP");
	// ------------------------------------------------------------  
	svTexOff			= pShader->GetParameterByName(0,"vTexOff");
	svCloudOff			= pShader->GetParameterByName(0,"vCloudOff");
	svMicroOff			= pShader->GetParameterByName(0,"vMicroOff");
	svOverlayOff		= pShader->GetParameterByName(0,"vOverlayOff");
	svWater				= pShader->GetParameterByName(0,"vWater");
	svSunDir			= pShader->GetParameterByName(0,"vSunDir");
	svTangent			= pShader->GetParameterByName(0,"vTangent");
	svBiTangent			= pShader->GetParameterByName(0,"vBiTangent");
	svPolarAxis			= pShader->GetParameterByName(0,"vPolarAxis");
	svMicroScale0		= pShader->GetParameterByName(0,"vMSc0");	
	svMicroScale1		= pShader->GetParameterByName(0,"vMSc1");	
	svMicroScale2		= pShader->GetParameterByName(0,"vMSc2");
	svSHD				= pShader->GetParameterByName(0,"vSHD");
	// ------------------------------------------------------------
	sfDistScale			= pShader->GetParameterByName(0,"fDistScale");
	sfAlpha				= pShader->GetParameterByName(0,"fAlpha");
	sfNight				= pShader->GetParameterByName(0,"fNight");
	// ------------------------------------------------------------
	sbCloudSh			= pShader->GetParameterByName(0,"bCloudSh");
	sbLights			= pShader->GetParameterByName(0,"bLights");
	sbInSpace			= pShader->GetParameterByName(0,"bInSpace");
	sbOnOff				= pShader->GetParameterByName(0,"bOnOff");
	sbEnvEnable			= pShader->GetParameterByName(0,"bEnvEnable");
	sbMicroNormals		= pShader->GetParameterByName(0,"bMicroNormals");
	siTileLvl			= pShader->GetParameterByName(0,"iTileLvl");
	siDebug				= pShader->GetParameterByName(0,"iDebug");
	sbDebug				= pShader->GetParameterByName(0,"bDebug");
	sbLocals			= pShader->GetParameterByName(0,"bLocals");
	sbShadows			= pShader->GetParameterByName(0,"bShadows");
	sbOverlay			= pShader->GetParameterByName(0,"bOverlay");
	sbSpherical			= pShader->GetParameterByName(0,"bSpherical");
	sbCloudNorm			= pShader->GetParameterByName(0,"bCloudNorm");
	sbEarth				= pShader->GetParameterByName(0,"bEarth");
	// ------------------------------------------------------------
	stDiff				= pShader->GetParameterByName(0,"tDiff");
	stMask				= pShader->GetParameterByName(0,"tMask");
	stCloud				= pShader->GetParameterByName(0,"tCloud");
	stCloud2			= pShader->GetParameterByName(0,"tCloud2");
	stNoise				= pShader->GetParameterByName(0,"tNoise");
	stOcean				= pShader->GetParameterByName(0,"tOcean");
	stCloudMicro	    = pShader->GetParameterByName(0,"tCloudMicro");
	stCloudMicroNorm	= pShader->GetParameterByName(0,"tCloudMicroNorm");
	stEnvMap			= pShader->GetParameterByName(0,"tEnvMap");
	stMicroA			= pShader->GetParameterByName(0,"tMicroA");
	stMicroB			= pShader->GetParameterByName(0,"tMicroB");
	stMicroC			= pShader->GetParameterByName(0,"tMicroC");
	stMicroRot			= pShader->GetParameterByName(0,"tMicroRot");
	stShadowMap			= pShader->GetParameterByName(0,"tShadowMap");
	stOverlay			= pShader->GetParameterByName(0,"tOverlay");
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
	svHazeMax			= pShader->GetParameterByName(0,"vHazeMax");
	svCameraPos			= pShader->GetParameterByName(0,"vCameraPos");		
	svUnitCameraPos		= pShader->GetParameterByName(0,"vUnitCameraPos");		
	sfCloudInts			= pShader->GetParameterByName(0,"fCloudInts");
	sfScaleHeight		= pShader->GetParameterByName(0,"fScaleHeight");		
	sfInvScaleHeight	= pShader->GetParameterByName(0,"fInvScaleHeight");
	sfSunAlt			= pShader->GetParameterByName(0,"fSunAlt");
	sfRadius			= pShader->GetParameterByName(0,"fRadius");
	sfCameraAlt			= pShader->GetParameterByName(0,"fCameraAlt");
	sfHorizonAlt		= pShader->GetParameterByName(0,"fHorizonAlt");
	sfAtmRad2			= pShader->GetParameterByName(0,"fAtmRad2");
	sfRPhase			= pShader->GetParameterByName(0,"fRPhase");
	sfHorizonDst		= pShader->GetParameterByName(0,"fHorizonDst");
	sfExposure			= pShader->GetParameterByName(0,"fExposure");			
	sfAux1				= pShader->GetParameterByName(0,"fAux1");			
	sfAux2				= pShader->GetParameterByName(0,"fAux2");
	sfTrGamma			= pShader->GetParameterByName(0,"fTrGamma");
	sfAtmGamma			= pShader->GetParameterByName(0,"fAtmGamma");
	sfInvAux1			= pShader->GetParameterByName(0,"fInvAux1");
	sfInvParameter		= pShader->GetParameterByName(0,"fInvParameter");
	sfTime				= pShader->GetParameterByName(0,"fTime");
	// ------------------------------------------------------------
	
	if (gc->TexturePath("D3D9Ocean.dds", name)) {
		HR(D3DXCreateTextureFromFileA(pDev, name, &hOcean));
	}

	if (gc->TexturePath("cloud1.dds", name)) {
		HR(D3DXCreateTextureFromFileA(pDev, name, &hCloudMicro));
	}

	if (gc->TexturePath("cloud1_norm.dds", name)) {
		HR(D3DXCreateTextureFromFileA(pDev, name, &hCloudMicroNorm));
	}

	if (hOcean) {
		HR(pShader->SetTexture(stOcean, hOcean));
	}

	if (hCloudMicro) {
		HR(pShader->SetTexture(stCloudMicro, hCloudMicro));
	}
	if (hCloudMicroNorm) {
		HR(pShader->SetTexture(stCloudMicroNorm, hCloudMicroNorm));
	}
}

// -----------------------------------------------------------------------

void PlanetRenderer::GlobalExit ()
{
	SAFE_RELEASE(pShader);
	SAFE_RELEASE(hOcean);
	SAFE_RELEASE(hCloudMicro);
	SAFE_RELEASE(hCloudMicroNorm);
}

// -----------------------------------------------------------------------

void PlanetRenderer::SetWorldMatrix (const D3DXMATRIX &W)
{
	pShader->SetMatrix(smWorld, &W);
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


	double pr = pPlanet->GetSize();		// Planet radius
	double cr = pPlanet->CamDist();		// Camera distance from a planet center
	double ca = cr - pr;				// Camera altitude

	float fSunAlt = -D3DXVec3Dot(&cam, &SunDir);

	if (fSunAlt < 0) fSunAlt = float(ca);
	else 			 fSunAlt = float(sqrt(cr*cr - fSunAlt*fSunAlt) - pr);
	

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
	HR(Shader()->SetFloat(sfExposure, float(atmo->expo)));
	HR(Shader()->SetFloat(sfTrGamma, 1.0f/float(atmo->tgamma)));
	HR(Shader()->SetFloat(sfRadius, float(pr)));
	HR(Shader()->SetFloat(sfCameraAlt, float(ca)));
	HR(Shader()->SetFloat(sfSunAlt, fSunAlt));
	HR(Shader()->SetTexture(stNoise, gc->GetNoiseTex()));

	// ---------------------------------------------------------------------
	// Initialize camera centric tangent frame for normal mapped water
	//
	MATRIX3 mRot;
	oapiGetRotationMatrix(hPlanet, &mRot);
	VECTOR3 vCPos = pPlanet->PosFromCamera();
	VECTOR3 vNrm = mul(mRot, pPlanet->ReferencePoint());
	VECTOR3 vRot = unit(mul(mRot, _V(0, 1, 0)));
	VECTOR3 vTan = unit(crossp(vRot, vNrm));
	VECTOR3 vBiT = unit(crossp(vTan ,vNrm));

	// ---------------------------------------------------------------------
	HR(Shader()->SetValue(svTangent,     &D3DXVEC(vTan), sizeof(D3DXVECTOR3)));
	HR(Shader()->SetValue(svBiTangent,   &D3DXVEC(vBiT), sizeof(D3DXVECTOR3)));
	HR(Shader()->SetValue(svPolarAxis,	 &D3DXVEC(vRot), sizeof(D3DXVECTOR3)));

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
	float   a = float((1.0f-g*g) / (4.0f*3.14f));
	float   b = (1.0f+g*g);
	float   d = (-2.0f*g);
	float  rp = -float(atmo->rpow);
	float  h0 = float(atmo->height*1e3);				// Scale height
	float  mp = -float(atmo->mpow);
	float  hp = -float(atmo->hazec);

	double ha = pPlanet->GetHorizonAlt();
	double me = pPlanet->GetMinElevation();				// Minimum elevation
	double mr = pr + me;								// Minimum radius
	double ur = pr + ha;								// Atmosphere upper radius (Skydome radius)
	double ub = pr + ha*8.0;
	double hd = 0;
	double tm = fmod(oapiGetSimTime(), 3600.0f);

	if (cr>mr) hd = sqrt(cr*cr - mr*mr);				// Camera to horizon distance

	if (hd < 1e3) hd = 1e3;

	float  fS = D3DXVec3Dot(&SunDir, &ucam);
	float  fA = 0.0f;
	float  fC = 0.0f;

	if (cr>mr) fA = float(sqrt(1.0 - (mr*mr) / (cr*cr)));
	if (cr>ub) fC = float(sqrt(1.0 - (ub*ub) / (cr*cr)));

	float  fB = (-fS-fA) / (float(atmo->aux1+0.01)*0.64f);
	float  fP = float(pPlanet->AngleCoEff(0.0));

	float fCorrect = 0.5f + saturate(fB)*1.5f;

	float fX = float(saturate((ca / (ha*0.3)) - 1.0));
	float fY = float(saturate((-fS - fC)*3.0f));

	float fSuns = fX * fY;

	// 1.0 / lambda^4
	D3DXVECTOR3 lambda_ray = D3DXVECTOR3(pow(float(atmo->red), rp), pow(float(atmo->green), rp), pow(float(atmo->blue), rp));
	D3DXVECTOR3 lambda_mie = D3DXVECTOR3(pow(float(atmo->red), mp), pow(float(atmo->green), mp), pow(float(atmo->blue), mp));
	D3DXVECTOR3 lambda_haze = D3DXVECTOR3(pow(float(atmo->red), hp), pow(float(atmo->green), hp), pow(float(atmo->blue), hp));
	
	D3DXVec3Normalize(&lambda_ray, &lambda_ray);
	D3DXVec3Normalize(&lambda_mie, &lambda_mie);
	D3DXVec3Normalize(&lambda_haze, &lambda_haze);

	D3DXVECTOR3 vTotOutSct = lambda_ray*float(atmo->rout) + lambda_mie*float(atmo->mie);
	D3DXVECTOR3 vMieInSct  = lambda_mie*float(atmo->mie);
	D3DXVECTOR3 vRayInSct  = lambda_ray*float(atmo->rin * atmo->rout);
	D3DXVECTOR3 vColorShift = lerp(D3DXVECTOR3(1,1,1), lambda_ray, float(atmo->aux3)*fCorrect);

	vColorShift *= float(saturate((150e3 / abs(ca)) + fY + 0.3f));

	D3DXVECTOR3 vHaze = lambda_haze * float(atmo->hazei);
	vHaze = lerp(vHaze, D3DXVECTOR3(1, 1, 1), fSuns);

	// Camara altitude dependency multiplier for ambient color of atmosphere
    float fMult = saturate((h0-float(ca*0.1))/h0);

	float fAmbLoc = fMult * float(min(0.7, log1p(atm->rho0)*0.4));


	// Upload parameters to shaders
	HR(Shader()->SetFloat(sfAmbient0, fAmbLoc));
	HR(Shader()->SetValue(svMPhase, &D3DXVECTOR4(a,b,0,d), sizeof(D3DXVECTOR4)));
	HR(Shader()->SetValue(svODCoEff, &D3DXVECTOR4(float(pCoEff[0]), float(pCoEff[1]), float(pCoEff[2]), float(pCoEff[3])), sizeof(D3DXVECTOR4)));
	HR(Shader()->SetValue(svODCoEffEx, &D3DXVECTOR4(float(pCoEff[4]), float(pCoEff[5]), float(pCoEff[6]), float(pCoEff[7])), sizeof(D3DXVECTOR4)));
	HR(Shader()->SetValue(svTotOutSct, &vTotOutSct, sizeof(D3DXVECTOR3)));
	HR(Shader()->SetValue(svMieInSct, &vMieInSct, sizeof(D3DXVECTOR3)));
	HR(Shader()->SetValue(svRayInSct, &vRayInSct, sizeof(D3DXVECTOR3)));
	HR(Shader()->SetValue(svColorShift, &vColorShift, sizeof(D3DXVECTOR3)));
	HR(Shader()->SetValue(svHazeMax, &vHaze, sizeof(D3DXVECTOR3)));
	HR(Shader()->SetFloat(sfAtmGamma, 1.0f/float(atmo->agamma)));
	HR(Shader()->SetFloat(sfCloudInts, float(atmo->depth)));
	HR(Shader()->SetFloat(sfRPhase, float(atmo->rphase)));
	HR(Shader()->SetFloat(sfAux1, float(atmo->aux1)));
	HR(Shader()->SetFloat(sfAux2, float(atmo->aux2)));
	HR(Shader()->SetFloat(sfInvAux1, 1.0f/float(atmo->aux1)));
	HR(Shader()->SetFloat(sfInvParameter, 1.0f/fP));
	HR(Shader()->SetFloat(sfScaleHeight, h0));
	HR(Shader()->SetFloat(sfInvScaleHeight, 1.0f/h0));
	HR(Shader()->SetFloat(sfHorizonAlt, float(ur-pr)));
	HR(Shader()->SetFloat(sfAtmRad2, float(ur*ur)));
	HR(Shader()->SetFloat(sfHorizonDst, float(hd)));
	HR(Shader()->SetFloat(sfTime, float(tm)));
	HR(Shader()->SetBool(sbInSpace, (cr>ur)));
	HR(Shader()->SetBool(sbOnOff, true));
}