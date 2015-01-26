
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
	static oapi::D3D9Client * Client() { return gc; }
	
	static void	SetWorldMatrix (const MATRIX4 &W);
	static void	SetViewProjectionMatrix (const D3DXMATRIX *VP);
	
	// ------------------------------------------------------------
	static oapi::D3D9Client *gc;
	static LPDIRECT3DDEVICE9 pDev;   
	static ID3DXEffect *pShader;
	static LPDIRECT3DTEXTURE9 hOcean;
	// ------------------------------------------------------------
	static D3DXHANDLE eTileTech;
	static D3DXHANDLE eTileTechNoZ;
	static D3DXHANDLE eCloudTech;
	static D3DXHANDLE eRingTech;
	static D3DXHANDLE eHorizonTech;
	static D3DXHANDLE eSkyDomeTech;
	static D3DXHANDLE eCloudShadowTech;
	// ------------------------------------------------------------  
	static D3DXHANDLE smWorld;
	static D3DXHANDLE smViewProj;
	// ------------------------------------------------------------  
	static D3DXHANDLE svTexOff;
	static D3DXHANDLE svWater;
	static D3DXHANDLE svSunDir;
	static D3DXHANDLE svGeneric;
	static D3DXHANDLE svTangent;
	static D3DXHANDLE svBiTangent;
	// ------------------------------------------------------------
	static D3DXHANDLE sfDistScale;
	static D3DXHANDLE sfAlpha;
	static D3DXHANDLE sfNight;
	// ------------------------------------------------------------
	static D3DXHANDLE sbSpecular;
	static D3DXHANDLE sbCloudSh;
	static D3DXHANDLE sbLights;
	static D3DXHANDLE sbInSpace;
	static D3DXHANDLE sbOnOff;
	// ------------------------------------------------------------
	static D3DXHANDLE stDiff;
	static D3DXHANDLE stMask;
	static D3DXHANDLE stNoise;
	static D3DXHANDLE stOcean;
	// ------------------------------------------------------------
	static D3DXHANDLE sfGlobalAmb;
	static D3DXHANDLE sfAmbient0;
	// Scatter model ----------------------------------------------
	static D3DXHANDLE svMPhase;		
	static D3DXHANDLE svODCoEff;
	static D3DXHANDLE svODCoEffEx;
	static D3DXHANDLE svMieInSct;	
	static D3DXHANDLE svRayInSct;
	static D3DXHANDLE svTotOutSct;
	static D3DXHANDLE svColorShift;
	static D3DXHANDLE svWhiteBalance;
	static D3DXHANDLE svCameraPos;		
	static D3DXHANDLE svUnitCameraPos;		
	static D3DXHANDLE sfSunset;
	static D3DXHANDLE sfInvMieScaleHeight;
	static D3DXHANDLE sfScaleHeight;		
	static D3DXHANDLE sfInvScaleHeight;
	static D3DXHANDLE sfRadius;
	static D3DXHANDLE sfCameraAlt;
	static D3DXHANDLE sfHorizonAlt;
	static D3DXHANDLE sfAtmRad2;
	static D3DXHANDLE sfRPhase;
	static D3DXHANDLE sfHorizonDst;
	static D3DXHANDLE sfExposure;			
	static D3DXHANDLE sfAux1;			
	static D3DXHANDLE sfAux2;	
	static D3DXHANDLE sfAux4;
	static D3DXHANDLE sfInvAux1;
	static D3DXHANDLE sfInvParameter;
	static D3DXHANDLE sfTime;
};

#endif