// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// =======================================================================
// Class VPlanet
// Visual for planet objects

#ifndef __VPLANET_H
#define __VPLANET_H

#include "D3d7util.h"
#include "Vobject.h"
#include "Mesh.h"

class Planet;
class PatchManager;
class HorizonManager;
class RingManager;
class TileManagerOld;

// =======================================================================
// Class VPlanet

class VPlanet: public VObject {
	friend class TileManager;

public:
	VPlanet (const Planet *_planet);
	~VPlanet();

	void Update (bool moving, bool force);
	void CheckResolution (double iar);
	void Render (LPDIRECT3DDEVICE7 dev);
	const PatchManager *CloudManager() const { return cloudmanager; } // old cloud manager implementation

	void RenderLabels (oapi::Sketchpad *skp, oapi::Font **labelfont, int *fontidx);
	// render new-style surface labels

	struct RenderPrm {
		// persistent options
		bool bAtm;              // planet has atmosphere?
		bool bCloud;            // planet has cloud layer?
		bool bBrightClouds;     // oversaturate cloud brightness?
		bool bCloudShadow;      // render cloud shadows?
		double atm_href;        // reference altitude for atmospheric effects [m]
		double cloudalt;        // altitude of cloud layer, if present [m]
		double horizon_excess;  // visual range excess for horizon (to render mountains rising ahove horizon)
		double horizon_minrad;  // scale factor for lower edge of rendered horizon (account for negative elevations)

		// frame-by-frame options
		bool bAddBkg;           // render additive to sky background (i.e. planet seen through atm.layer of another planet)
		bool bFog;              // render distance fog?
		bool bTint;             // render atmospheric tint?
		bool bCloudFlatShadows; // render cloud shadows onto a sphere?
		Vector rgbTint;         // tint colour
		double cloudrot;        // cloud layer rotation state
	} prm;

protected:
	bool ModLighting (DWORD &ambient);
	// modification of ambient light levels for planet surface rendering
	// at low camera altitudes close to the terminator (due to atmospheric
	// scattering), so that total darkness doesn't coincide with sunset

private:
	int ShadowPlanetOnRing (VERTEX_XYZC *&vtx, DWORD &nvtx);
	// Calculate the shadow the planet casts on its
	// ring system
	// return value is the number of points on the
	// shadow polygon

	void RenderClouds (LPDIRECT3DDEVICE7 dev, DWORD cullmode, const RenderPrm &prm);
	// render cloud layer

	void RenderCloudShadows (LPDIRECT3DDEVICE7 dev);
	// render cloud shadows on the ground

	void RenderBaseSurfaceTiles (LPDIRECT3DDEVICE7 dev);
	void RenderBaseSurfaceDecals (LPDIRECT3DDEVICE7 dev);
	void RenderBaseShadows (LPDIRECT3DDEVICE7 dev, float alpha);
	void RenderBaseStructures (LPDIRECT3DDEVICE7 dev);
	// render any visible bases on the planet surface (surface features and structures)

	void RenderRing (LPDIRECT3DDEVICE7 dev, bool addbkg = false);
	// render ring system around planet

	void SetupRenderVectorList ();

	PatchManager *cloudmanager;
	HorizonManager *horizonmanager;
	RingManager *ringmanager;
	Mesh *mesh;           // explicit mesh for nonsperical objects
	const Planet *planet; // pointer to logical interface
	D3DMATRIX mWorld_scaled;
	MATRIX4 dmWorld_scaled;
	double maxdist;
	double dist_scale;    // planet rescaling factor
	float shadowalpha;    // alpha setting for shadows
	double pprf;          // planet patch res factor
	int max_patchres;     // patch resolution limit
	int min_cloudres, max_cloudres;
	int patchres;         // surface patch resolution
	int cloudres;         // cloud layer resolution
	int ringres;          // planetary ring resolution
	bool renderpix;       // render planet as pixel block (at large distance)
	int cloudformat;      // 0=no clouds, 1=old format, 2=new format
	bool hashaze;         // planet has horizon haze layer
	bool hasfog;          // add distance fog
	int cloudcam;         // camera position relative to cloud layer
						  // 1=cam below inner cloud boundary
						  // 2=cam above outer cloud boundary
	                      // 3=cam between cloud boundaries
	double cloudvis;      // cloud visibility aperture
	bool hasrings;        // planet has ring system
	VERTEX_XYZC *shvtx;   // polygon for planet shadow on rings
	DWORD nshvtx;
	D3DMATRIX mWorldC;    // cloud layer transformation matrix
	D3DMATRIX mWorldC0;   // cloud ground shadow transformation matrix
	double tCheckRes;     // time of next resolution check
	double tCheckRingSh;  // time of next ring shadow calculation
	static bool bstencil; // use stencil buffer for shadows
	static int mipmap_mode; // enable mipmap interpolation
	static int aniso_mode;  // enable anisotropic filtering if > 1
};

#endif // !__VPLANET_H