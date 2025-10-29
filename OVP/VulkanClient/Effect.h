// ===========================================================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2011 - 2016 Jarmo Nikkanen
// ===========================================================================================

#ifndef __vkEFFECT_H
#define __vkEFFECT_H

#define vkSM_SPHERE	0x01
#define vkSM_ARROW	0x02
#define vkSM_BOX		0x03

#include "Client.h"
#include <d3d9.h> 
#include "MathAPI.h"

// NOTE: a "bool" in HLSL is 32bits (i.e. int)
// Must match with counterpart in vkClient.fx

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
	BOOL Baked;		// Enable pre-baked maps
	BOOL BakedAO;	// Enable pre-baked AO map
	BOOL BakedAmb;	// Enable pre-baked Ambient light map
};



using namespace oapi;

class vkEffect {

	DWORD d3d9id;

public:
	static void vkTechInit(vkClient *gc, LPDIRECT3DDEVICE9 pDev, const char *folder);

	/**
	 * \brief Release global parameters
	 */
	static void GlobalExit();

	static void ShutDown();

	vkEffect();
	~vkEffect();

	static void EnablePlanetGlow(bool bEnabled);
	static void UpdateEffectCamera(OBJHANDLE hPlanet);
	static void InitLegacyAtmosphere(OBJHANDLE hPlanet, float GlobalAmbient);
	static void SetViewProjMatrix(FMATRIX4* pVP);

	static void RenderLines(const FVECTOR3 *pVtx, const WORD *pIdx, int nVtx, int nIdx, const FMATRIX4 *pW, DWORD color);
	static void RenderTileBoundingBox(const FMATRIX4* pW, VECTOR4 *pVtx, const FVECTOR4* color);
	static void RenderBoundingBox(const FMATRIX4* pW, const FMATRIX4* pGT, const FVECTOR4 *bmin, const FVECTOR4 *bmax, const FVECTOR4 *color);
	static void RenderBoundingSphere(const FMATRIX4* pW, const FMATRIX4* pGT, const FVECTOR4 *bs, const FVECTOR4 *color);
	static void RenderBillboard(const FMATRIX4* pW, LPDIRECT3DTEXTURE9 pTex, float alpha = 1.0f);
	static void RenderExhaust(const FMATRIX4* pW, VECTOR3 &cdir, EXHAUSTSPEC *es, SURFHANDLE def);
	static void RenderSpot(float intens, const FVECTOR4* color, const FMATRIX4* pW, SURFHANDLE pTex);
	static void Render2DPanel(const MESHGROUP *mg, const SURFHANDLE pTex, const FMATRIX4* pW, float alpha, float scale, bool additive);
	static void RenderReEntry(const SURFHANDLE pTex, const FVECTOR3* vPosA, const FVECTOR3* vPosB, const FVECTOR3* vDir, float alpha_a, float alpha_b, float size);
	static void RenderArrow(OBJHANDLE hObj, const VECTOR3 *ofs, const VECTOR3 *dir, const VECTOR3 *rot, float size, const FVECTOR4 *pColor);  
	
	static LPDIRECT3DDEVICE9 pDev;      ///< Static (global) render device
	static LPDIRECT3DVERTEXBUFFER9 VB;  ///< Static (global) Vertex buffer pointer
	
	static FVECTOR4 atm_color;		///< Earth glow color

	// Rendering Technique related parameters
	static ID3DXEffect	*FX;
	static vkClient   *gc; ///< The graphics client instance

	static vkMatExt	mfdmat;
	static vkMatExt	defmat;
	static vkMatExt	night_mat;
	static vkMatExt	emissive_mat;
	
	// Techniques ----------------------------------------------------
	static D3DXHANDLE	eVesselTech;     ///< Vessel exterior, surface bases
	static D3DXHANDLE	eSimple;
	static D3DXHANDLE	eBBTech;         ///< Bounding Box Tech
	static D3DXHANDLE	eTBBTech;        ///< Bounding Box Tech
	static D3DXHANDLE	eBSTech;         ///< Bounding Sphere Tech
	static D3DXHANDLE   eExhaust;        ///< Render engine exhaust texture
	static D3DXHANDLE   eSpotTech;       ///< Vessel beacons
	static D3DXHANDLE   ePanelTech;      ///< Used to draw a new style 2D panel
	static D3DXHANDLE   ePanelTechB;     ///< Used to draw a new style 2D panel
	static D3DXHANDLE	eBaseTile;
	static D3DXHANDLE	eRingTech;       ///< Planet rings technique
	static D3DXHANDLE	eRingTech2;      ///< Planet rings technique
	static D3DXHANDLE	eShadowTech;     ///< Vessel ground shadows
	static D3DXHANDLE	eGeometry;
	static D3DXHANDLE	eBaseShadowTech; ///< Used to draw transparent surface without texture
	static D3DXHANDLE	eBeaconArrayTech;
	static D3DXHANDLE	eArrowTech;      ///< (Grapple point) arrows
	static D3DXHANDLE	eAxisTech;
	static D3DXHANDLE	ePlanetTile;
	static D3DXHANDLE	eCloudTech;
	static D3DXHANDLE	eCloudShadow;
	static D3DXHANDLE	eSkyDomeTech;
	static D3DXHANDLE	eDiffuseTech;
	static D3DXHANDLE	eEmissiveTech;
	static D3DXHANDLE	eHazeTech;
	static D3DXHANDLE	eSimpMesh;

	// Transformation Matrices ----------------------------------------
	static D3DXHANDLE	eVP;         ///< Combined View & Projection Matrix
	static D3DXHANDLE	eW;          ///< World Matrix
	static D3DXHANDLE	eLVP;        ///< Light view projection
	static D3DXHANDLE	eGT;         ///< MeshGroup transformation matrix

	// Lighting related parameters ------------------------------------
	static D3DXHANDLE   eMtrl;
	static D3DXHANDLE	eMat;        ///< Material
	static D3DXHANDLE	eWater;      ///< Water
	static D3DXHANDLE	eSun;        ///< Sun
	static D3DXHANDLE	eLights;     ///< Additional light sources
	static D3DXHANDLE	eKernel;
	static D3DXHANDLE	eAtmoParams;

	// Auxiliary params ----------------------------------------------
	static D3DXHANDLE   eModAlpha;     ///< BOOL multiply material alpha with texture alpha
	static D3DXHANDLE	eFullyLit;     ///< BOOL
	static D3DXHANDLE	eFlow;		   ///< BOOL
	static D3DXHANDLE	eShadowToggle; ///< BOOL
	static D3DXHANDLE	eEnvMapEnable; ///< BOOL
	static D3DXHANDLE	eInSpace;      ///< BOOL
	static D3DXHANDLE	eLightsEnabled;///< BOOL
	static D3DXHANDLE	eBaseBuilding; ///< BOOL
	static D3DXHANDLE	eCockpit;	   ///< BOOL
	static D3DXHANDLE	eFresnel;	   ///< BOOL
	static D3DXHANDLE   eSwitch;	   ///< BOOL
	static D3DXHANDLE   eRghnSw;	   ///< BOOL
	static D3DXHANDLE	eTextured;	   ///< BOOL
	static D3DXHANDLE	eOITEnable;	   ///< BOOL
	static D3DXHANDLE	eInvProxySize;
	static D3DXHANDLE	eNoColor;
	static D3DXHANDLE	eVCIrrad;
	static D3DXHANDLE	eMix;          ///< FLOAT Auxiliary factor/multiplier
	static D3DXHANDLE   eColor;        ///< Auxiliary color input
	static D3DXHANDLE   eFogColor;     ///< Fog color input
	static D3DXHANDLE   eTexOff;       ///< Surface tile texture offsets
	static D3DXHANDLE	eSpecularMode;
	static D3DXHANDLE	eHazeMode;
	static D3DXHANDLE   eTime;         ///< FLOAT Simulation elapsed time
	static D3DXHANDLE	eExposure;
	static D3DXHANDLE	eCameraPos;	
	static D3DXHANDLE   eNorth;
	static D3DXHANDLE	eEast;
	static D3DXHANDLE	eVCAmbient;
	static D3DXHANDLE   eDistScale;
	static D3DXHANDLE   eGlowConst;
	static D3DXHANDLE   eRadius;
	static D3DXHANDLE	eFogDensity;
	static D3DXHANDLE	ePointScale;
	static D3DXHANDLE	eAtmColor;
	static D3DXHANDLE	eProxySize;
	static D3DXHANDLE	eMtrlAlpha;
	static D3DXHANDLE	eAttennuate;
	static D3DXHANDLE	eInScatter;
	static D3DXHANDLE	eSHD;
	static D3DXHANDLE   eSHDPx;
	static D3DXHANDLE	eSHDSubRect;
	static D3DXHANDLE	eNight;

	// Textures --------------------------------------------------------
	static D3DXHANDLE	eTex0;    ///< Primary texture
	static D3DXHANDLE	eTex1;    ///< Secondary texture
	static D3DXHANDLE	eTex3;    ///< Tertiary texture
	static D3DXHANDLE	eSpecMap;
	static D3DXHANDLE	eEmisMap;
	static D3DXHANDLE	eEnvMapA;
	static D3DXHANDLE	eReflMap;
	static D3DXHANDLE	eMetlMap;
	static D3DXHANDLE	eHeatMap;
	static D3DXHANDLE	eRghnMap;
	static D3DXHANDLE	eTranslMap;
	static D3DXHANDLE	eTransmMap;
	static D3DXHANDLE	eIrradMap;
	static D3DXHANDLE	eAmbientMap;
	static D3DXHANDLE	eCombinedMap;
	static D3DXHANDLE	eCombSunMap;

	// Legacy Atmosphere -----------------------------------------------
	static D3DXHANDLE	eGlobalAmb;	 
	static D3DXHANDLE	eSunAppRad;	 
	static D3DXHANDLE	eAmbient0;	 
	static D3DXHANDLE	eDispersion;	  
};

#endif // !__vkEFFECT_H
