// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//   ORBITER VISUALISATION PROJECT (OVP)
//   D3D7 Client module
// ==============================================================

// ==============================================================
// VPlanet.h
// class vPlanet (interface)
//
// A vPlanet is the visual representation of a "planetary" object
// (planet, moon, asteroid).
// Currently this only supports spherical objects, without
// variations in elevation.
// ==============================================================

#ifndef __VPLANET_H
#define __VPLANET_H

#include "VObject.h"

class D3D7Mesh;
class SurfTile;
class CloudTile;

class vPlanet: public vObject {
	friend class TileManager;
	friend class SurfaceManager;
	template<class T> friend class TileManager2;
	friend class CloudManager;
	friend class HazeManager;
	friend class RingManager;
	friend class vBase;

public:
	vPlanet (OBJHANDLE _hObj, const Scene *scene);
	~vPlanet ();

	bool Update ();
	void CheckResolution ();
	void RenderZRange (double *nplane, double *fplane);
	bool Render (LPDIRECT3DDEVICE7 dev);
	void RenderLabels(LPDIRECT3DDEVICE7 dev, oapi::Sketchpad *skp, oapi::Font **labelfont, int *fontidx);

	struct RenderPrm { // misc. parameters for rendering the planet
		// persistent options
		bool bAtm;              // planet has atmosphere
		bool bCloud;            // planet has a cloud layer
		bool bCloudShadow;      // planet renders cloud shadows on surface
		bool bCloudBrighten;    // oversaturate cloud brightness?
		bool bFogEnabled;	    // does this planet support fog rendering?
		double atm_href;	    // reference altitude for atmospheric effects [m]
		double atm_amb0;        // scale parameter for ambient level modification
		DWORD amb0col;          // baseline ambient colour [rgba]
		double cloudalt;        // altitude of cloud layer, if present [m]
		double shadowalpha;     // opacity of shadows (0-1)
		double horizon_excess;  // extend horizon visibility radius
		double tilebb_excess;   // extend tile visibility bounding box
		double horizon_minrad;  // scale factor for lower edge of rendered horizon (account for negative elevations)

		// frame-by-frame options
		bool bAddBkg;		    // render additive to sky background (i.e. planet seen through atm.layer of another planet)
		int cloudvis;           // cloud visibility: bit0=from below, bit1=from above
		double cloudrot;	    // cloud layer rotation state
		bool bFog;			    // render distance fog in this frame?
		bool bTint;			    // render atmospheric tint?
		bool bCloudFlatShadows; // render cloud shadows onto a sphere?
		VECTOR3 rgbTint;        // tint colour
	} prm;

	// Access functions
	const TileManager2<SurfTile> *SurfMgr2() const { return surfmgr2; }
	const TileManager2<CloudTile> *CloudMgr2() const { return cloudmgr2; }

	void ActivateLabels(bool activate);

protected:
	void RenderDot (LPDIRECT3DDEVICE7 dev);
	void RenderSphere (LPDIRECT3DDEVICE7 dev, const RenderPrm &prm, bool &using_zbuf);
	void RenderCloudLayer (LPDIRECT3DDEVICE7 dev, DWORD cullmode, const RenderPrm &prm);
	void RenderVectors(LPDIRECT3DDEVICE7 dev);
	void RenderBaseVectors(LPDIRECT3DDEVICE7 dev);
	void RenderBaseSurfaces (LPDIRECT3DDEVICE7 dev);
	void RenderBaseStructures (LPDIRECT3DDEVICE7 dev);
	void RenderBaseShadows (LPDIRECT3DDEVICE7 dev, float depth);
	void RenderCloudShadows (LPDIRECT3DDEVICE7 dev, const RenderPrm &prm);
	bool ModLighting (DWORD &ambient);

private:
	float rad;                // planet radius [m]
	float render_rad;         // distance to be rendered past planet centre
	float dist_scale;         // planet rescaling factor
	double maxdist, max_centre_dist;
	float shadowalpha;        // alpha value for surface shadows
	double cloudrad;          // cloud layer radius [m]
	int max_patchres;         // max surface LOD level
	int patchres;             // surface LOD level
	int mipmap_mode;          // mipmapping mode for planet surface (0=none, 1=point sampling, 2=linear interpolation)
	int aniso_mode;           // anisotropic filtering (>= 1, 1=none)
	bool renderpix;           // render planet as pixel block (at large distance)
	bool hashaze;             // render atmospheric haze
	DWORD nbase;              // number of surface bases
	vBase **vbase;            // list of base visuals
	SurfaceManager *surfmgr;  // planet surface tile manager
	TileManager2<SurfTile> *surfmgr2;   // planet surface tile manager (v2)
	TileManager2<CloudTile> *cloudmgr2; // planet cloud layer tile manager (v2)
	HazeManager *hazemgr;     // horizon haze rendering
	RingManager *ringmgr;     // ring manager
	bool bRipple;             // render specular ripples on water surfaces
	bool bVesselShadow;       // render vessel shadows on surface
	FogParam fog;             // distance fog render parameters
	D3D7Mesh *mesh;           // mesh for nonspherical body
	D3DMATRIX mWorldScaled;   // rescaled world matrix

	struct CloudData {        // cloud render parameters (for legacy interface)
		CloudManager *cloudmgr; // cloud tile manager
		double viewap;          // visible radius
		D3DMATRIX mWorldC;      // cloud world matrix
		D3DMATRIX mWorldC0;     // cloud shadow world matrix
		bool cloudshadow;       // render cloud shadows on the surface
		float shadowalpha;      // alpha value for cloud shadows
		double microalt0, microalt1; // altitude limits for micro-textures
	} *clouddata;
};

#endif // !__VPLANET_H