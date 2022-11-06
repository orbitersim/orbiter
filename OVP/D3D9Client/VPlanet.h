// ==============================================================
// VPlanet.h
// Part of the ORBITER VISUALISATION PROJECT (OVP)
//   Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2006-2016 Martin Schweiger
// Copyright (C) 2012-2016 Jarmo Nikkanen
// ==============================================================

#ifndef __VPLANET_H
#define __VPLANET_H

#include "VObject.h"
#include "AtmoControls.h"
#include "D3D9Util.h"
#include <list>

class D3D9Mesh;
class SurfTile;
class CloudTile;

bool FilterElevationPhysics(OBJHANDLE hPlanet, int lvl, int ilat, int ilng, double elev_res, INT16* elev);


#define SUN_COLOR 0
#define RAY_COLOR 1
#define MIE_COLOR 2
#define RAY_LAND  3
#define MIE_LAND  4
#define AMB_LAND  5
#define SUN_GLARE 6
#define SKY_AMBIENT 7

#define PLT_CONFIG -1
#define PLT_EARTH 0
#define PLT_MARS  1
#define PLT_MOON  2
#define PLT_CLOUDS 3
#define PLT_GIANT 4
#define PLT_G_CLOUDS 5


class PlanetShader : public ShaderClass
{
public:
	PlanetShader(LPDIRECT3DDEVICE9 pDev, const char* file, const char* vs, const char* ps, const char* name, const char* options) :
		ShaderClass(pDev, file, vs, ps, name, options)
	{
		bLocals = false;
		bMicrotex = false;
		bShdMap = false;
		bDevtools = false;
		bAtmosphere = true;
		bWater = false;
		bRipples = false;
		bCloudShd = false;
		bNightlights = false;

		if (string(options).find("_LOCALLIGHTS") != string::npos) bLocals = true;
		if (string(options).find("_MICROTEX") != string::npos) bMicrotex = true;
		if (string(options).find("_SHDMAP") != string::npos) bShdMap = true;
		if (string(options).find("_DEVTOOLS") != string::npos) bDevtools = true;
		if (string(options).find("_NO_ATMOSPHERE") != string::npos) bAtmosphere = false;
		if (string(options).find("_WATER") != string::npos) bWater = true;
		if (string(options).find("_RIPPLES") != string::npos) bRipples = true;
		if (string(options).find("_CLOUDSHD") != string::npos) bCloudShd = true;
		if (string(options).find("_NIGHTLIGHTS") != string::npos) bNightlights = true;

		tCloud = GetPSHandle("tCloud");
		tCloud2 = GetPSHandle("tCloud2");
		tMask = GetPSHandle("tMask");
		tDiff = GetPSHandle("tDiff");
		tShadowMap = GetPSHandle("tShadowMap");
		PrmVS = GetVSHandle("Prm");
		Prm = GetPSHandle("Prm");
		FlowVS = GetVSHandle("FlowVS");
		Flow = GetPSHandle("Flow");
		Lights = GetPSHandle("Lights");
		Spotlight = GetPSHandle("Spotlight");
	};

	~PlanetShader()
	{

	}

	bool bLocals;
	bool bMicrotex;
	bool bShdMap;
	bool bDevtools;
	bool bAtmosphere;
	bool bWater;
	bool bRipples;
	bool bCloudShd;
	bool bNightlights;

	HANDLE tCloud;
	HANDLE tCloud2;
	HANDLE tMask;
	HANDLE tDiff;
	HANDLE tShadowMap;
	HANDLE PrmVS;
	HANDLE Prm;
	HANDLE FlowVS;
	HANDLE Flow;	
	HANDLE Lights;
	HANDLE Spotlight;
};


#pragma pack(push, 4)

struct sFlow {
	BOOL bRay;
	BOOL bAmb;
};

struct ShaderParams
{
	float4x4 mWorld;			// World Matrix
	float4x4 mLVP;				// Light-View-Projection
	float4   vSHD;				// Shadow Map Parameters
	float4   vMSc[3];			// Micro Texture offset-scale
	float4	 vTexOff;			// Texture offset-scale
	float4   vCloudOff;			// Cloud texture offset-scale
	float4   vMicroOff;			// Micro texture offset-scale
	float4   vOverlayOff;       // Overlay texture offset-scale
	float4   vOverlayCtrl[4];
	float	 fAlpha;
	float	 fBeta;
};

struct FlowControlPS
{
	BOOL bInSpace;				// Camera in space (not in atmosphere)
	BOOL bOverlay;				// Overlay on/off	
	BOOL bShadows;				// Shadow Map on/off
	BOOL bLocals;				// Local Lights on/off
	BOOL bMicroNormals;			// Micro texture has normals
	BOOL bCloudShd;
	BOOL bMask;
	BOOL bRipples;
};

struct FlowControlVS
{
	BOOL bInSpace;				// Camera in space (not in atmosphere)
	BOOL bSpherical;			// Ignore elevation, render as sphere
	BOOL bElevOvrl;				// ElevOverlay on/off			
};



struct ConstParams
{
	float4x4 mVP;				// View Projection Matrix
	float3 CamPos;				// Geocentric Camera position
	float3 toCam;				// Geocentric Camera direction (unit vector)
	float3 toSun;				// Geocentric Sun direction (unit vector)
	float3 SunAz;				// Atmo scatter ref.frame (unit vector) (toCam, ZeroAz, SunAz)
	float3 ZeroAz;				// Atmo scatter ref.frame (unit vector)
	float3 Up;					// Sun Ref Frame (Unit Vector) (Up, toSun, ZeroAz)
	float3 vTangent;			// Reference frame for normal mapping (Unit Vector)
	float3 vBiTangent;			// Reference frame for normal mapping (Unit Vector)
	float3 vPolarAxis;			// North Pole (unit vector)
	float3 cSun;				// Sun Color and intensity
	float3 RayWave;				// .rgb Rayleigh Wave lenghts
	float3 MieWave;				// .rgb Mie Wave lenghts
	float3 GlareColor;
	float4 HG;					// Henyey-Greenstein Phase function params
	float2 iH;					// Inverse scale height for ray(.r) and mie(.g) exp(-altitude * iH) 
	float2 rmO;					// Ray and Mie out-scatter factors
	float2 rmI;					// Ray and Mie in-scatter factors
	float  RayPh;				// Phase
	float  PlanetRad;			// Planet Radius
	float  PlanetRad2;			// Planet Radius Squared
	float  AtmoAlt;				// Atmospehere upper altitude limit
	float  AtmoRad;				// Atmospehere outer radius
	float  AtmoRad2;			// Atmospehere outer radius squared
	float  CloudAlt;			// Cloud layer altitude 
	float  MinAlt;				// Minimum terrain altitude
	float  MaxAlt;				// Maximum terrain altitude
	float  iAltRng;				// 1.0 / (MaxAlt - MinAlt);
	float  AngMin;
	float  AngRng;
	float  iAngRng;
	float  AngCtr;				// Cos of View cone angle from planet center that's visible from camera location 
	float  HrzDst;				// Distance to horizon (500 m) minimum if camera below sea level
	float  CamAlt;				// Camera Altitude
	float  CamElev;				// Camera Elevation above surface
	float  CamRad;				// Camera geo-distance
	float  CamRad2;				// Camera geo-distance squared
	float  MaxDst;				// Max "ray" distance through atmosphere
	float  iMaxDst;
	float  Expo;				// "HDR" exposure factor (atmosphere only)
	float  Time;				// Simulation time / 180
	float  TrGamma;				// Terrain "Gamma" correction setting
	float  TrExpo;				// "HDR" exposure factor (terrain only)
	float  Ambient;				// Global ambient light level
	float  Clouds;
	float  Glare;
	float  TW_Multi;
	float  TW_Dst;
	float  SunRadAtHrz;
	float  CamSpace;			// Camera in space scale factor 0.0 = surf, 1.0 = space
};

#pragma pack(pop)



// ==============================================================
// class vPlanet (interface)
// ==============================================================

/**
 * \brief Visual representation of a planetary object.
 *
 * A vPlanet is the visual representation of a "planetary" object (planet, moon,
 * asteroid).
 * Simple planets might only be implemented as spherical objects, without
 * variations in elevation.
 */
class vPlanet: public vObject {

	friend class TileManager;
	friend class SurfaceManager;
	template<class T> friend class TileManager2;
	friend class CloudManager;
	friend class HazeManager;
	friend class HazeManager2;
	friend class RingManager;
	friend class vBase;

public:

	struct sOverlay {
		LPDIRECT3DTEXTURE9 pSurf[4];
		D3DXVECTOR4 Blend[4];
		VECTOR4 lnglat;
	};

	vPlanet (OBJHANDLE _hObj, const Scene *scene);
	~vPlanet ();

	bool			ParseConfig(const char *fname);
	virtual bool	GetMinMaxDistance(float *zmin, float *zmax, float *dmin);
	virtual void	UpdateBoundingBox();

	static void		GlobalInit(oapi::D3D9Client* gc);
	static void		GlobalExit();

	bool			IsMesh() { return mesh != NULL; }
	bool			Update (bool bMainScene);
	void			CheckResolution ();
	void			RenderZRange (double *nplane, double *fplane);
	bool			Render(LPDIRECT3DDEVICE9 dev);
	void			RenderBeacons(LPDIRECT3DDEVICE9 dev);
	void            RenderVectors (LPDIRECT3DDEVICE9 dev, D3D9Pad* pSkp);
	bool			CameraInAtmosphere() const;
	double			CameraAltitude() const { return cdist - size; }
	double			GetHorizonAlt() const;
	double          GetMinElevation() const { return minelev; };
	double			GetMaxElevation() const { return maxelev; };
	VECTOR3			GetUnitSurfacePos(double lng, double lat) const;
	VECTOR3			GetRotationAxis() const { return axis; }
	VECTOR3			ToLocal(VECTOR3 &glob) const;
	void			GetLngLat(VECTOR3 &loc, double *lng, double *lat) const;
	VECTOR3			ReferencePoint();
	void			SetMicroTexture(LPDIRECT3DTEXTURE9 pSrc, int slot);
	int				GetElevation(double lng, double lat, double *elv, FVECTOR3 *nrm = NULL) const;
	SurfTile *		FindTile(double lng, double lat, int maxres);
	void 			PickSurface(D3DXVECTOR3 &vRay, TILEPICK *pPick);
	DWORD			GetPhysicsPatchRes() const { return physics_patchres; }
	sOverlay *		AddOverlaySurface(VECTOR4 lnglat, gcCore::OlayType type, LPDIRECT3DTEXTURE9 pSrf = NULL, sOverlay *pOld = NULL, const FVECTOR4* pB = NULL);
	sOverlay *		IntersectOverlay(VECTOR4 bounds, FVECTOR4* texcoord) const;
	

	// Surface base interface -------------------------------------------------
	DWORD			GetBaseCount() const { return nbase; }
	vBase*			GetBaseByIndex(DWORD index) const { return vbase[index]; }
	vBase*			GetBaseByHandle(OBJHANDLE hBase) const;

	// Atmospheric ------------------------------------------------------------
	ScatterParams * GetAtmoParams(int mode=-1);
	double			AngleCoEff(double cos_dir);
	bool			LoadAtmoConfig();
	void			SaveAtmoConfig();
	void			SaveStruct(FILEHANDLE hFile, ScatterParams* prm, int bO);
	void			LoadStruct(FILEHANDLE hFile, ScatterParams* prm, int bO);
	char*			Label(const char* x);
	bool			HasAtmosphere() const { return prm.bAtm; }
	bool			HasRipples() const { return bRipple; }
	LPDIRECT3DTEXTURE9 GetScatterTable(int i);
	ConstParams*	GetScatterConst() { return &cp; }
	PlanetShader*	GetShader(int id = PLT_CONFIG);
	int				GetShaderID();
	ShaderParams*	GetTerrainParams() { return &sp; }
	FlowControlPS*	GetFlowControl() { return &fcp; }
	FlowControlVS*  GetFlowControlVS() { return &fcv; }
	void			UpdateScatter();
	int				GetAtmoMode() { return atm_mode; }
	FVECTOR3		SunLightColor(FVECTOR3 pos);
	float			SunAltitude();

	// v2 Labels interface ----------------------------------------------------
	void            ActivateLabels(bool activate);
	void            RenderLabels(LPDIRECT3DDEVICE9 dev, D3D9Pad *skp, oapi::Font **labelfont, int *fontidx);

	struct RenderPrm { //< misc. parameters for rendering the planet
		// persistent options
		bool bAtm;              ///< planet has atmosphere
		bool bCloud;            ///< planet has a cloud layer
		bool bCloudShadow;      ///< planet renders cloud shadows on surface
		bool bCloudBrighten;    ///< oversaturate cloud brightness?
		bool bFogEnabled;	    ///< does this planet support fog rendering?
		double atm_href;	    ///< reference altitude for atmospheric effects [m]
		double atm_amb0;        ///< scale parameter for ambient level modification
		double atm_hzalt;		///< Horizon rendering altitude
		DWORD  amb0col;         ///< baseline ambient colour [rgba]
		double cloudalt;        ///< altitude of cloud layer, if present [m]
		double shadowalpha;     ///< opacity of shadows (0-1)
		double horizon_excess;  ///< extend horizon visibility radius
		double tilebb_excess;   ///< extend tile visibility bounding box
		double horizon_minrad;  ///< scale factor for lower edge of rendered horizon (account for negative elevations) (unit sphere radius)
		
		// frame-by-frame options
		bool bAddBkg;		    ///< render additive to sky background (i.e. planet seen through atm.layer of another planet)
		int cloudvis;           ///< cloud visibility: bit0=from below, bit1=from above
		double cloudrot;	    ///< cloud layer rotation state
		bool bFog;			    ///< render distance fog in this frame?
		bool bTint;			    ///< render atmospheric tint?
		//bool bCloudFlatShadows; ///< render cloud shadows onto a sphere?

		// Shader Params
		D3DXCOLOR	TintColor;
		D3DXCOLOR	AmbColor;
		D3DXCOLOR	FogColor;
		D3DXCOLOR   SkyColor;
		D3DXVECTOR3 SunDir;
		float		FogDensity;
		float		DistScale;
		float		SclHeight;		///< Atmospheric scale height
		float		InvSclHeight;	///< Inverse of atmospheric scale height
		double		ScatterCoEff[8];
	} prm;

	list<sOverlay *> overlays;

	// Access functions
	TileManager2<SurfTile> *SurfMgr2() const { return surfmgr2; }
	TileManager2<CloudTile> *CloudMgr2() const { return cloudmgr2; }

	static void ParseMicroTexturesFile(); ///< Parse MicroTex.cfg file (once)

protected:

	void RenderSphere (LPDIRECT3DDEVICE9 dev);
	void RenderCloudLayer (LPDIRECT3DDEVICE9 dev, DWORD cullmode);
	void RenderBaseSurfaces (LPDIRECT3DDEVICE9 dev);
	void RenderBaseStructures (LPDIRECT3DDEVICE9 dev);
	void RenderBaseShadows (LPDIRECT3DDEVICE9 dev, float depth);
	void RenderCloudShadows (LPDIRECT3DDEVICE9 dev);
	bool ModLighting (DWORD &ambient);

	bool ParseMicroTextures();            ///< Read micro-texture config for this planet
	static void LoadMicroTextures(LPDIRECT3DDEVICE9 pDev);

private:

	LPDIRECT3DTEXTURE9 pSunColor, pRaySkyView, pMieSkyView, pLandViewRay, pLandViewMie, pAmbientSky, pLandViewAmb;

	ConstParams cp;
	ShaderParams sp;
	FlowControlPS fcp;
	FlowControlVS fcv;

	static ImageProcessing* pIP;
	static PlanetShader* pRender[8];
	static LPDIRECT3DDEVICE9 pDev;
	static LPDIRECT3DTEXTURE9 pSunTex;
	static int Qc, Wc, Nc;

	float dist_scale;         // planet rescaling factor
	double maxdist,           // ???
	       max_centre_dist;

	double minelev, maxelev;
	double threshold;
	float shadowalpha;        // alpha value for surface shadows
	double cloudrad;          // cloud layer radius [m]
	DWORD max_patchres;       // max surface LOD level used for graphics
	DWORD physics_patchres;   // max surface LOD level used by physics
	int patchres;             // surface LOD level
	int tilever;			  // Surface tile version
	int iConfig;
	int atm_mode;
	bool renderpix;           // render planet as pixel block (at large distance)
	bool hashaze;             // render atmospheric haze
	DWORD nbase;              // number of surface bases
	vBase **vbase;            // list of base visuals
	SurfaceManager *surfmgr;  // planet surface tile manager
	TileManager2<SurfTile> *surfmgr2;   // planet surface tile manager (v2)
	TileManager2<CloudTile> *cloudmgr2; // planet cloud layer tile manager (v2)
	mutable class SurfTile *tile_cache;
	HazeManager *hazemgr;     // horizon haze rendering
	HazeManager2 *hazemgr2;	  // horizon haze rendering
	RingManager *ringmgr;     // ring manager
	bool bRipple;             // render specular ripples on water surfaces
	bool bVesselShadow;       // render vessel shadows on surface
	bool bObjectShadow;       // render object shadows on surface
	bool bFog;                // render distance fog?
	FogParam fog;             // distance fog render parameters
	D3D9Mesh *mesh;           // mesh for non-spherical body
	VECTOR3	vRefPoint;		  // Auxiliary reference point for normal mapped water
	ScatterParams SPrm;		  // Parameters for atmospheric configuration dialog
	ScatterParams OPrm;		  // Parameters for atmospheric configuration dialog
	ScatterParams HPrm;		  // Parameters for atmospheric configuration dialog
	ScatterParams CPrm;		  // Parameters for atmospheric configuration dialog

	struct CloudData {        // cloud render parameters (for legacy interface)
		CloudManager *cloudmgr; // cloud tile manager
		double cloudrad;        // cloud layer radius [m]
		double viewap;          // visible radius
		D3DXMATRIX mWorldC;     // cloud world matrix
		D3DXMATRIX mWorldC0;    // cloud shadow world matrix
		DWORD rendermode;		// bit 0: render from below, bit 1: render from above
		bool cloudshadow;       // render cloud shadows on the surface
		float shadowalpha;      // alpha value for cloud shadows
		double microalt0,       // altitude limits for micro-textures
		       microalt1;
	} *clouddata;

	char ShaderName[32];
public:

	struct _MicroTC {
		char	file[32];		// Texture file name
		double	reso;			// Resolution px/m
		double	size;			// Texture size in meters;
		double	px;				// Size in pixels
		LPDIRECT3DTEXTURE9 pTex;
	};

	struct _MicroCfg {
		_MicroTC Level[3];
		bool bNormals;			// Normals enabled
		bool bEnabled;			// Micro textures enabled
	} MicroCfg;
};

#endif // !__VPLANET_H
