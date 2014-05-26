
// ==============================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2014 Jarmo Nikkanen
// ==============================================================


#include "PlanetRenderer.h"
#include "D3D9Config.h"
#include "AABBUtil.h"


// ------------------------------------------------------------
class oapi::D3D9Client *PlanetRenderer::gc = NULL; 
LPDIRECT3DDEVICE9 PlanetRenderer::pDev = NULL;
// ------------------------------------------------------------
ID3DXEffect *PlanetRenderer::pShader = NULL;
D3DXHANDLE PlanetRenderer::eTileTech = NULL;
D3DXHANDLE PlanetRenderer::eCloudTech = NULL;
D3DXHANDLE PlanetRenderer::eHorizonTech = NULL;
// ------------------------------------------------------------  
D3DXHANDLE PlanetRenderer::smWorld = NULL;
D3DXHANDLE PlanetRenderer::smViewProj = NULL;
// ------------------------------------------------------------  
D3DXHANDLE PlanetRenderer::svTexOff = NULL;
D3DXHANDLE PlanetRenderer::svWater = NULL;
D3DXHANDLE PlanetRenderer::svSunDir = NULL;
D3DXHANDLE PlanetRenderer::svAddBkg = NULL;
D3DXHANDLE PlanetRenderer::svTint = NULL;
// ------------------------------------------------------------
D3DXHANDLE PlanetRenderer::sfDistScale = NULL;
D3DXHANDLE PlanetRenderer::sfAlpha = NULL;
D3DXHANDLE PlanetRenderer::sfNight = NULL;
// ------------------------------------------------------------
D3DXHANDLE PlanetRenderer::sbSpecular = NULL;
D3DXHANDLE PlanetRenderer::sbCloudSh = NULL;
D3DXHANDLE PlanetRenderer::sbLights = NULL;
D3DXHANDLE PlanetRenderer::sbLegacyAtm = NULL;
// ------------------------------------------------------------
D3DXHANDLE PlanetRenderer::stDiff = NULL;
D3DXHANDLE PlanetRenderer::stMask = NULL;
// Atmosphere -------------------------------------------------
D3DXHANDLE PlanetRenderer::svFogColor = NULL;
D3DXHANDLE PlanetRenderer::sfFogDensity = NULL;
D3DXHANDLE PlanetRenderer::sfGlobalAmb = NULL;
D3DXHANDLE PlanetRenderer::sfSunAppRad = NULL;
D3DXHANDLE PlanetRenderer::sfDispersion = NULL;
D3DXHANDLE PlanetRenderer::sfAmbient0 = NULL;
// Scatter model ----------------------------------------------
D3DXHANDLE PlanetRenderer::svPhase = NULL;		
D3DXHANDLE PlanetRenderer::svODCoEff = NULL;
D3DXHANDLE PlanetRenderer::svRayTotal = NULL;	
D3DXHANDLE PlanetRenderer::svRayInSct = NULL;
D3DXHANDLE PlanetRenderer::svRaySurface = NULL;
D3DXHANDLE PlanetRenderer::svMieTotal = NULL;
D3DXHANDLE PlanetRenderer::svCameraPos = NULL;		
D3DXHANDLE PlanetRenderer::svUnitCameraPos = NULL;		
D3DXHANDLE PlanetRenderer::sfSunIntensity = NULL;
D3DXHANDLE PlanetRenderer::sfSrfIntensity = NULL;
D3DXHANDLE PlanetRenderer::sfScaleHeight = NULL;		
D3DXHANDLE PlanetRenderer::sfInvScaleHeight = NULL;
D3DXHANDLE PlanetRenderer::sfRadius = NULL;
D3DXHANDLE PlanetRenderer::sfCameraAlt = NULL;
D3DXHANDLE PlanetRenderer::sfAtmRad2 = NULL;
D3DXHANDLE PlanetRenderer::sfBalance = NULL;
D3DXHANDLE PlanetRenderer::siMode = NULL;
D3DXHANDLE PlanetRenderer::sbOverSat = false;

// Orbital horizon haze ring implementation ------------------

WORD		 PlanetRenderer::Idx[HORIZON2_NSEG*2+2];
D3DXVECTOR3  PlanetRenderer::Vtx[HORIZON2_NSEG*2];
DWORD		 PlanetRenderer::nIdx = HORIZON2_NSEG*2+2;
float		 PlanetRenderer::CosP[HORIZON2_NSEG];
float		 PlanetRenderer::SinP[HORIZON2_NSEG];



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
	eCloudTech			= pShader->GetTechniqueByName("CloudTech");
	eHorizonTech		= pShader->GetTechniqueByName("HorizonTech");
	// ------------------------------------------------------------  
	smWorld				= pShader->GetParameterByName(0,"mWorld");
	smViewProj			= pShader->GetParameterByName(0,"mViewProj");
	// ------------------------------------------------------------  
	svTexOff			= pShader->GetParameterByName(0,"vTexOff");
	svWater				= pShader->GetParameterByName(0,"vWater");
	svSunDir			= pShader->GetParameterByName(0,"vSunDir");
	svAddBkg			= pShader->GetParameterByName(0,"vAddBkg");
	svTint				= pShader->GetParameterByName(0,"vTint");
	// ------------------------------------------------------------
	sfDistScale			= pShader->GetParameterByName(0,"fDistScale");
	sfAlpha				= pShader->GetParameterByName(0,"fAlpha");
	sfNight				= pShader->GetParameterByName(0,"fNight");
	// ------------------------------------------------------------
	sbSpecular			= pShader->GetParameterByName(0,"bSpecular");
	sbCloudSh			= pShader->GetParameterByName(0,"bCloudSh");
	sbLights			= pShader->GetParameterByName(0,"bLights");
	sbLegacyAtm			= pShader->GetParameterByName(0,"bLegacyAtm");
	// ------------------------------------------------------------
	stDiff				= pShader->GetParameterByName(0,"tDiff");
	stMask				= pShader->GetParameterByName(0,"tMask");
	// Atmosphere -----------------------------------------------------------
	svFogColor			= pShader->GetParameterByName(0,"vFogColor");
	sfFogDensity		= pShader->GetParameterByName(0,"fFogDensity");
	sfGlobalAmb			= pShader->GetParameterByName(0,"fGlobalAmb");
	sfSunAppRad			= pShader->GetParameterByName(0,"fSunAppRad");
	sfDispersion		= pShader->GetParameterByName(0,"fDispersion");
	sfAmbient0			= pShader->GetParameterByName(0,"fAmbient0");
	
	// Scatter model --------------------------------------------------------
	svPhase				= pShader->GetParameterByName(0,"vPhase");		
	svODCoEff			= pShader->GetParameterByName(0,"vODCoEff");
	svRayTotal			= pShader->GetParameterByName(0,"vRayTotal");
	svRayInSct			= pShader->GetParameterByName(0,"vRayInSct");
	svRaySurface		= pShader->GetParameterByName(0,"vRaySurface");
	svMieTotal			= pShader->GetParameterByName(0,"vMieTotal");
	svCameraPos			= pShader->GetParameterByName(0,"vCameraPos");		
	svUnitCameraPos		= pShader->GetParameterByName(0,"vUnitCameraPos");		
	sfSunIntensity		= pShader->GetParameterByName(0,"fSunIntensity");
	sfSrfIntensity		= pShader->GetParameterByName(0,"fSrfIntensity");
	sfScaleHeight		= pShader->GetParameterByName(0,"fScaleHeight");		
	sfInvScaleHeight	= pShader->GetParameterByName(0,"fInvScaleHeight");
	sfRadius			= pShader->GetParameterByName(0,"fRadius");
	sfCameraAlt			= pShader->GetParameterByName(0,"fCameraAlt");
	sfAtmRad2			= pShader->GetParameterByName(0,"fAtmRad2");
	sfBalance			= pShader->GetParameterByName(0,"fBalance");
	siMode				= pShader->GetParameterByName(0,"iMode");
	sbOverSat			= pShader->GetParameterByName(0,"bOverSat");


	// -------------------------------------------------------------------
	// 
	int i;
	for (i = 0; i < HORIZON2_NSEG; i++) Idx[i*2] = i*2+1, Idx[i*2+1] = i*2;
	Idx[i*2] = 1, Idx[i*2+1] = 0;

	for (i = 0; i < HORIZON2_NSEG; i++) {
		double phi = (double)i/(double)HORIZON2_NSEG * PI*2.0;
		CosP[i] = (float)cos(phi), SinP[i] = (float)sin(phi);
	}
}

// -----------------------------------------------------------------------

void PlanetRenderer::GlobalExit ()
{
	SAFE_RELEASE(pShader);
}

// -----------------------------------------------------------------------

void PlanetRenderer::InitLegacyAtmosphere(OBJHANDLE hPlanet)
{
	VECTOR3 GS, GP;

	DWORD dAmbient = *(DWORD*)gc->GetConfigParam(CFGPRM_AMBIENTLEVEL);
	float fAmbient = float(dAmbient)*0.0039f;
	
	OBJHANDLE hS = oapiGetGbodyByIndex(0);	// the central star
	oapiGetGlobalPos (hS, &GS);				// sun position
	oapiGetGlobalPos (hPlanet, &GP);		// planet position
			
	float rs = (float)(oapiGetSize(hS) / length(GS-GP));
	
	const ATMCONST *atm = (oapiGetObjectType(hPlanet)==OBJTP_PLANET ? oapiGetPlanetAtmConstants (hPlanet) : NULL);

	HR(pShader->SetFloat(sfGlobalAmb, fAmbient));
	HR(pShader->SetFloat(sfSunAppRad, rs)); 

	if (atm) {
		HR(pShader->SetFloat(sfAmbient0, min(0.7f, log(float(atm->rho0)+1.0f)*0.4f)));
		HR(pShader->SetFloat(sfDispersion, max(0.02f, min(0.9f, log(float(atm->rho0)+1.0f)))));
	}
	else {
		HR(pShader->SetFloat(sfAmbient0, 0.0f));
		HR(pShader->SetFloat(sfDispersion, 0.0f));
	}
}

// -----------------------------------------------------------------------

void PlanetRenderer::SetWorldMatrix (const MATRIX4 &W)
{
	D3DXMATRIX wtrans;
	MATRIX4toD3DMATRIX (W, wtrans);
	pShader->SetMatrix(smWorld, &wtrans);
}


void PlanetRenderer::RenderHaze(D3DXMATRIX &wmat, float hralt)
{
	D3DXMATRIX imat, transm;

	int i, j;
	double phi;
	float cosp, sinp, cost, sint, h1, h2, r1, r2;

	D3DMAT_MatrixInvert (&imat, &wmat);
	VECTOR3 rpos = {imat._41, imat._42, imat._43};   // camera in local coords (planet radius = 1)
	double cdist = length (rpos);

	double id = 1.0 / max (cdist, 1.001);
	double visrad = acos (id);             // aperture of visibility sector
	double sinv = sin(visrad);

	h1 = (float)id;
	h2 = h1 + (float)(hralt*id);
	r1 = (float)sinv;
	r2 = (1.0f+hralt)*r1;

	for (i = j = 0; i < HORIZON2_NSEG; i++) 
	{
		Vtx[j].x = r1*CosP[i];
		Vtx[j].y = h1;
		Vtx[j].z = r1*SinP[i];
		j++;
		Vtx[j].x = r2*CosP[i];
		Vtx[j].y = h2;
		Vtx[j].z = r2*SinP[i];
		j++;
	}

	normalise (rpos);

	cost = (float)rpos.y, sint = (float)sqrt (1.0-cost*cost);
	phi = atan2 (rpos.z, rpos.x), cosp = (float)cos(phi), sinp = (float)sin(phi);

	D3DXMATRIX rmat = D3DXMATRIX(cost*cosp, -sint, cost*sinp, 0,
		              sint*cosp,  cost, sint*sinp, 0,
					  -sinp,      0,    cosp,      0,
					  0,          0,    0,         1);

	D3DXMatrixMultiply(&transm, &rmat, &wmat);
	
	HR(pShader->SetTechnique(eHorizonTech));
	HR(pShader->SetMatrix(smWorld, &transm));
	
	UINT numPasses = 0;
	HR(pShader->Begin(&numPasses, D3DXFX_DONOTSAVESTATE));
	HR(pShader->BeginPass(0));
	
	pDev->SetVertexDeclaration(pPositionDecl);
	pDev->DrawIndexedPrimitiveUP(D3DPT_TRIANGLESTRIP, 0, 2*HORIZON2_NSEG, 2*HORIZON2_NSEG, Idx, D3DFMT_INDEX16, Vtx, sizeof(D3DXVECTOR3));
	
	HR(pShader->EndPass());
	HR(pShader->End());	
}
