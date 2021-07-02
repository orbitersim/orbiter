#ifndef __SURFACE_H
#define __SURFACE_H

// Class Surface for generating dynamic planetary
// surfaces. Used at low altitudes when sphere patches
// no longer work.
// Features:
// - Generic planet-specific textures
// - Elevation mapping

#include <d3d.h>

class Body;
class Planet;
class VPlanet;

static const int MAXLEVEL = 5; // max number of resolution levels supported

typedef struct {
	D3DVERTEX *Vtx[MAXLEVEL];
	int *Texid[MAXLEVEL];
	int nVtx[MAXLEVEL];
	int iLng0[MAXLEVEL];
	int iLng1[MAXLEVEL];
	int iLat0[MAXLEVEL];
	int iLat1[MAXLEVEL];
} SurfMesh;

// =======================================================================
// Class  DynSurface

class DynSurface {
public:
	DynSurface (const Body *_body, const VPlanet *_vplanet);
	~DynSurface ();

	void Init();
	// this should be called whenever the resolution changes
	// e.g. window resized, zoom factor changed, detail level changed by user

	void Render (LPDIRECT3DDEVICE7 dev);

private:
	const Body *body;
	const Planet *planet;
	const VPlanet *vplanet;
	double updT, updDT;
	LPDIRECTDRAWSURFACE7 *tex;
	int ntex;
	float fogStart, fogEnd;  // fog parameters
	unsigned short *dbase;   // surface data base
	int maxres;              // max resolution level
	double *res_dist;        // distance list for resolution levels
	SurfMesh smesh;          // mesh specs
};

#endif // !__SURFACE_H