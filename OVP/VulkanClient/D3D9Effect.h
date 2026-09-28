// ===========================================================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2011-2026 Jarmo Nikkanen
// ===========================================================================================

#ifndef __D3D9EFFECT_H
#define __D3D9EFFECT_H

#define D3D9SM_SPHERE	0x01
#define D3D9SM_ARROW	0x02

#include "D3D9Client.h"
#include "VkShader.h" // d3d9.h/d3dx9.h: ID3DXEffect is VkEffect

// NOTE: a "bool" in HLSL is 32bits (i.e. int)
// Must match with counterpart in D3D9Client.fx

struct TexFlow {
	BOOL Emis;		// Enable Emission Maps
	BOOL Spec;		// Enable Specular Maps
	BOOL Refl;		// Enable Reflection Maps
	BOOL Transl;	// Enable translucent effect
	BOOL Transm;	// Enable transmissive effect
	BOOL Rghn;		// Enable roughness map
	BOOL Norm;		// Enable normal map
	BOOL Metl;		// Enable metalness map
	BOOL Heat;		// Enable heat map
};



using namespace oapi;

class D3D9Effect {

	DWORD d3d9id;

public:
	static void D3D9TechInit(D3D9Client *gc, VkDev *pDev, const char *folder);

	/**
	 * \brief Release global parameters
	 */
	static void GlobalExit();

	static void ShutDown();

	D3D9Effect();
	~D3D9Effect();

	static void EnablePlanetGlow(bool bEnabled);
	static void UpdateEffectCamera(OBJHANDLE hPlanet);
	static void InitLegacyAtmosphere(OBJHANDLE hPlanet, float GlobalAmbient);
	static void SetViewProjMatrix(LPD3DXMATRIX pVP);

	static void RenderLines(const D3DXVECTOR3 *pVtx, const WORD *pIdx, int nVtx, int nIdx, const D3DXMATRIX *pW, DWORD color);
	static void RenderTileBoundingBox(const LPD3DXMATRIX pW, VECTOR4 *pVtx, const LPD3DXVECTOR4 color);
	static void RenderBoundingBox(const LPD3DXMATRIX pW, const LPD3DXMATRIX pGT, const D3DXVECTOR4 *bmin, const D3DXVECTOR4 *bmax, const D3DXVECTOR4 *color);
	static void RenderBoundingSphere(const LPD3DXMATRIX pW, const LPD3DXMATRIX pGT, const D3DXVECTOR4 *bs, const D3DXVECTOR4 *color);
	static void RenderBillboard(const LPD3DXMATRIX pW, VkTex *pTex, float alpha = 1.0f);
	static void RenderExhaust(const LPD3DXMATRIX pW, VECTOR3 &cdir, EXHAUSTSPEC *es, SURFHANDLE def);
	static void RenderSpot(float intens, const LPD3DXCOLOR color, const LPD3DXMATRIX pW, SURFHANDLE pTex);
	static void Render2DPanel(const MESHGROUP *mg, const SURFHANDLE pTex, const LPD3DXMATRIX pW, float alpha, float scale, bool additive);
	static void RenderReEntry(const SURFHANDLE pTex, const LPD3DXVECTOR3 vPosA, const LPD3DXVECTOR3 vPosB, const LPD3DXVECTOR3 vDir, float alpha_a, float alpha_b, float size);
	static void RenderArrow(OBJHANDLE hObj, const VECTOR3 *ofs, const VECTOR3 *dir, const VECTOR3 *rot, float size, const D3DXCOLOR *pColor);  
	
	static VkDev *pDev;      ///< Static (global) render device
	static VkBuf *VB;  ///< Static (global) Vertex buffer pointer
	
	static D3DXVECTOR4 atm_color;		///< Earth glow color

	// Rendering Technique related parameters
	static VkEffect *FX;
	static D3D9Client   *gc; ///< The graphics client instance

	static D3D9MatExt	mfdmat;
	static D3D9MatExt	defmat;
	static D3D9MatExt	night_mat;
	static D3D9MatExt	emissive_mat;
	
	// Techniques ----------------------------------------------------
	static VkFxHandle	eVesselTech;     ///< Vessel exterior, surface bases
	static VkFxHandle	eSimple;
	static VkFxHandle	eBBTech;         ///< Bounding Box Tech
	static VkFxHandle	eTBBTech;        ///< Bounding Box Tech
	static VkFxHandle	eBSTech;         ///< Bounding Sphere Tech
	static VkFxHandle   eExhaust;        ///< Render engine exhaust texture
	static VkFxHandle   eSpotTech;       ///< Vessel beacons
	static VkFxHandle   ePanelTech;      ///< Used to draw a new style 2D panel
	static VkFxHandle   ePanelTechB;     ///< Used to draw a new style 2D panel
	static VkFxHandle	eBaseTile;
	static VkFxHandle	eRingTech;       ///< Planet rings technique
	static VkFxHandle	eRingTech2;      ///< Planet rings technique
	static VkFxHandle	eShadowTech;     ///< Vessel ground shadows
	static VkFxHandle	eGeometry;
	static VkFxHandle	eBaseShadowTech; ///< Used to draw transparent surface without texture
	static VkFxHandle	eBeaconArrayTech;
	static VkFxHandle	eArrowTech;      ///< (Grapple point) arrows
	static VkFxHandle	eAxisTech;
	static VkFxHandle	ePlanetTile;
	static VkFxHandle	eCloudTech;
	static VkFxHandle	eCloudShadow;
	static VkFxHandle	eSkyDomeTech;
	static VkFxHandle	eDiffuseTech;
	static VkFxHandle	eEmissiveTech;
	static VkFxHandle	eHazeTech;
	static VkFxHandle	eSimpMesh;

	// Transformation Matrices ----------------------------------------
	static VkFxHandle	eVP;         ///< Combined View & Projection Matrix
	static VkFxHandle	eW;          ///< World Matrix
	static VkFxHandle	eLVP;        ///< Light view projection
	static VkFxHandle	eGT;         ///< MeshGroup transformation matrix

	// Lighting related parameters ------------------------------------
	static VkFxHandle   eMtrl;
	static VkFxHandle   eTune;
	static VkFxHandle	eMat;        ///< Material
	static VkFxHandle	eWater;      ///< Water
	static VkFxHandle	eSun;        ///< Sun
	static VkFxHandle	eLights;     ///< Additional light sources
	static VkFxHandle	eKernel;
	static VkFxHandle	eAtmoParams;

	// Auxiliary params ----------------------------------------------
	static VkFxHandle   eModAlpha;     ///< BOOL multiply material alpha with texture alpha
	static VkFxHandle	eFullyLit;     ///< BOOL
	static VkFxHandle	eFlow;		   ///< BOOL
	static VkFxHandle	eShadowToggle; ///< BOOL
	static VkFxHandle	eEnvMapEnable; ///< BOOL
	static VkFxHandle	eInSpace;      ///< BOOL
	static VkFxHandle	eNoColor;      ///< BOOL
	static VkFxHandle	eLightsEnabled;///< BOOL
	static VkFxHandle	eBaseBuilding; ///< BOOL
	static VkFxHandle	eTuneEnabled;  ///< BOOL
	static VkFxHandle	eFresnel;	   ///< BOOL
	static VkFxHandle   eSwitch;	   ///< BOOL
	static VkFxHandle   eRghnSw;	   ///< BOOL
	static VkFxHandle	eTextured;	   ///< BOOL
	static VkFxHandle	eOITEnable;	   ///< BOOL
	static VkFxHandle	eInvProxySize;
	static VkFxHandle	eMix;          ///< FLOAT Auxiliary factor/multiplier
	static VkFxHandle   eColor;        ///< Auxiliary color input
	static VkFxHandle   eFogColor;     ///< Fog color input
	static VkFxHandle   eTexOff;       ///< Surface tile texture offsets
	static VkFxHandle	eSpecularMode;
	static VkFxHandle	eHazeMode;
	static VkFxHandle   eTime;         ///< FLOAT Simulation elapsed time
	static VkFxHandle	eExposure;
	static VkFxHandle	eCameraPos;	
	static VkFxHandle   eNorth;
	static VkFxHandle	eEast;
	static VkFxHandle   eDistScale;
	static VkFxHandle   eGlowConst;
	static VkFxHandle   eRadius;
	static VkFxHandle	eFogDensity;
	static VkFxHandle	ePointScale;
	static VkFxHandle	eAtmColor;
	static VkFxHandle	eProxySize;
	static VkFxHandle	eMtrlAlpha;
	static VkFxHandle	eAttennuate;
	static VkFxHandle	eInScatter;
	static VkFxHandle	eSHD;
	static VkFxHandle	eNight;

	// Textures --------------------------------------------------------
	static VkFxHandle	eTex0;    ///< Primary texture
	static VkFxHandle	eTex1;    ///< Secondary texture
	static VkFxHandle	eTex3;    ///< Tertiary texture
	static VkFxHandle	eSpecMap;
	static VkFxHandle	eEmisMap;
	static VkFxHandle	eEnvMapA;
	static VkFxHandle	eEnvMapB;
	static VkFxHandle	eReflMap;
	static VkFxHandle	eMetlMap;
	static VkFxHandle	eHeatMap;
	static VkFxHandle	eRghnMap;
	static VkFxHandle	eTranslMap;
	static VkFxHandle	eTransmMap;
	static VkFxHandle	eShadowMap;
	static VkFxHandle	eIrradMap;

	// Legacy Atmosphere -----------------------------------------------
	static VkFxHandle	eGlobalAmb;	 
	static VkFxHandle	eSunAppRad;	 
	static VkFxHandle	eAmbient0;	 
	static VkFxHandle	eDispersion;	  
};

#endif // !__D3D9EFFECT_H

