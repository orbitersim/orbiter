
// ==============================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2014 Jarmo Nikkanen
// ==============================================================

#ifndef __PLANETRENDERER_H
#define __PLANETRENDERER_H

#include "D3D9Client.h"

class PlanetRenderer {
	
public:
				PlanetRenderer();
				~PlanetRenderer();

	static void GlobalInit(class oapi::D3D9Client *gclient);
	static void GlobalExit();
	static void	InitializeScattering(class vPlanet *pPlanet);

	static LPDIRECT3DDEVICE9 Dev() { return pDev; }
	static ID3DXEffect * Shader() { return pShader; }
	
	
	//void		InitLegacyAtmosphere(OBJHANDLE hPlanet);

	void		SetWorldMatrix (const MATRIX4 &W);
	void		SetViewProjectionMatrix (const MATRIX4 &VP);
	void		SetWorldMatrix (const D3DXMATRIX &W);
	void		SetViewProjectionMatrix (const D3DXMATRIX &VP);
	
	// ------------------------------------------------------------
	static oapi::D3D9Client *gc;
	static LPDIRECT3DDEVICE9 pDev;   
	static ID3DXEffect *pShader;
	// ------------------------------------------------------------
	static D3DXHANDLE eTileTech;
	static D3DXHANDLE eCloudTech;
	static D3DXHANDLE eRingTech;
	static D3DXHANDLE eHorizonTech;
	static D3DXHANDLE eSkyDomeTech;
	// ------------------------------------------------------------  
	static D3DXHANDLE smWorld;
	static D3DXHANDLE smViewProj;
	// ------------------------------------------------------------  
	static D3DXHANDLE svTexOff;
	static D3DXHANDLE svWater;
	static D3DXHANDLE svSunDir;
	static D3DXHANDLE svAddBkg;
	// ------------------------------------------------------------
	static D3DXHANDLE sfDistScale;
	static D3DXHANDLE sfAlpha;
	static D3DXHANDLE sfNight;
	// ------------------------------------------------------------
	static D3DXHANDLE sbSpecular;
	static D3DXHANDLE sbCloudSh;
	static D3DXHANDLE sbLights;
	// ------------------------------------------------------------
	static D3DXHANDLE stDiff;
	static D3DXHANDLE stMask;
	// Atmosphere -------------------------------------------------
	//static D3DXHANDLE svFogColor;
	//static D3DXHANDLE sfFogDensity;
	//static D3DXHANDLE sfGlobalAmb;
	//static D3DXHANDLE sfSunAppRad;
	//static D3DXHANDLE sfDispersion;
	//static D3DXHANDLE sfAmbient0;
	// Scatter model ----------------------------------------------
	static D3DXHANDLE svPhase;		
	static D3DXHANDLE svODCoEff;
	static D3DXHANDLE svRayTotal;
	static D3DXHANDLE svRayInSct;
	static D3DXHANDLE svRaySurface;
	static D3DXHANDLE svMieTotal;
	static D3DXHANDLE svCameraPos;		
	static D3DXHANDLE svUnitCameraPos;		
	static D3DXHANDLE sfSunIntensity;
	static D3DXHANDLE sfSrfIntensity;
	static D3DXHANDLE sfScaleHeight;		
	static D3DXHANDLE sfInvScaleHeight;
	static D3DXHANDLE sfRadius;
	static D3DXHANDLE sfCameraAlt;
	static D3DXHANDLE sfAtmRad;
	static D3DXHANDLE sfBalance;
	static D3DXHANDLE sfHorizonDst;
	static D3DXHANDLE siMode;
	static D3DXHANDLE sbOverSat;
};

#endif