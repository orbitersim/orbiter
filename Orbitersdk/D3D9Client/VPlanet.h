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
#include <list>

class D3D9Mesh;
class SurfTile;
class CloudTile;


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
		LPDIRECT3DTEXTURE9 pSurf;
		VECTOR4 lnglat;
	};

	vPlanet (OBJHANDLE _hObj, const Scene *scene);
	~vPlanet ();

	virtual bool	GetMinMaxDistance(float *zmin, float *zmax, float *dmin);
	virtual void	UpdateBoundingBox();

	bool			IsMesh() { return mesh != NULL; }
	bool			Update (bool bMainScene);
	void			CheckResolution ();
	void			RenderZRange (double *nplane, double *fplane);
	bool			Render(LPDIRECT3DDEVICE9 dev);
	void			RenderBeacons(LPDIRECT3DDEVICE9 dev);
	bool			CameraInAtmosphere() const;
	double			CameraAltitude() const { return cdist - size; }
	double			GetHorizonAlt() const;
	double          GetMinElevation() const;
	double			GetMaxElevation() const;
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
	sOverlay *		AddOverlaySurface(VECTOR4 lnglat, LPDIRECT3DTEXTURE9 pSrf = NULL, sOverlay *pOld = NULL);
	sOverlay *		IntersectOverlay(VECTOR4 bounds, D3DXVECTOR4 *texcoord) const;

	// Surface base interface -------------------------------------------------
	DWORD			GetBaseCount() const { return nbase; }
	vBase*			GetBaseByIndex(DWORD index) const { return vbase[index]; }
	vBase*			GetBaseByHandle(OBJHANDLE hBase) const;

	// Atmospheric ------------------------------------------------------------
	ScatterParams * GetAtmoParams(int mode=-1);
	double			AngleCoEff(double cos_dir);
	D3DXVECTOR3		GetSunLightColor(VECTOR3 vPos, float fAmbient, float fGlobalAmb);
	bool			LoadAtmoConfig(bool bOrbit);
	void			SaveAtmoConfig(bool bOrbit);
	void			UpdateAtmoConfig();
	void			DumpDebugFile();
	bool			HasAtmosphere() const { return bScatter; }
	bool			HasRipples() const { return bRipple; }

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
		double horizon_minelev; ///< scale factor for lower edge of rendered horizon (account for negative elevations) (elevation [m] e.g -7000 for Mars)

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

protected:
	void RenderSphere (LPDIRECT3DDEVICE9 dev);
	void RenderCloudLayer (LPDIRECT3DDEVICE9 dev, DWORD cullmode);
	void RenderBaseSurfaces (LPDIRECT3DDEVICE9 dev);
	void RenderBaseStructures (LPDIRECT3DDEVICE9 dev);
	void RenderBaseShadows (LPDIRECT3DDEVICE9 dev, float depth);
	void RenderCloudShadows (LPDIRECT3DDEVICE9 dev);
	bool ModLighting (DWORD &ambient);

	bool ParseMicroTextures();            ///< Read micro-texture config for this planet
	bool LoadMicroTextures();
	static void ParseMicroTexturesFile(); ///< Parse MicroTex.cfg file (once)

private:
	float dist_scale;         // planet rescaling factor
	double maxdist,           // ???
	       max_centre_dist;
	float shadowalpha;        // alpha value for surface shadows
	double cloudrad;          // cloud layer radius [m]
	DWORD max_patchres;       // max surface LOD level used for graphics
	DWORD physics_patchres;   // max surface LOD level used by physics
	int patchres;             // surface LOD level
	int tilever;			  // Surface tile version
	bool renderpix;           // render planet as pixel block (at large distance)
	bool hashaze;             // render atmospheric haze
	bool bScatter;			  // Planet has scattering parameters
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
	D3D9Mesh *mesh;           // mesh for nonspherical body
	VECTOR3	vRefPoint;		  // Auxiliary reference point for normal mapped water
	ScatterParams SPrm;		  // Parameters for atmospheric configuration dialog
	ScatterParams OPrm;		  // Parameters for atmospheric configuration dialog
	ScatterParams NPrm;		  // Parameters for atmospheric configuration dialog
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

	struct _MicroTC {
		char	file[32];		// Texture file name
		double	reso;			// Resolution px/m
		double	size;			// Texture size in meters;
		double	px;				// pixel count
		LPDIRECT3DTEXTURE9 pTex;
	};

public:
	struct _MicroCfg {
		_MicroTC Level[3];
		bool bNormals;			// Normals enabled
		bool bEnabled;			// Micro textures enables
		bool bLoaded;			// Micro textures loaded
	} MicroCfg;
};

#endif // !__VPLANET_H
