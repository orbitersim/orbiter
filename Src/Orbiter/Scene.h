// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// =======================================================================
// Class Scene
// This contains the list of all current visuals (vobjects, lights, etc.)
// and is responsible for rendering to the target surface.

#ifndef __SCENE_H
#define __SCENE_H

#include "D3d7util.h"
#include "Vobject.h"
#include "Body.h"
#include "CelSphere.h"

#define MAXCONST 88      // max number of constellations
#define CANVAS_SIZE 256  // size of multi-purpose texture

#define LABEL_NX "\001"
#define LABEL_PX "\002"
#define LABEL_NY "\003"
#define LABEL_PY "\004"
#define LABEL_NZ "\005"
#define LABEL_PZ "\006"

class Star;
class VStar;
class PlanetarySystem;
class Camera;
class D3D7ParticleStream;

class Scene {
public:
	struct STARLIGHT {
		D3DCOLORVALUE col;
		const Vector *gpos;
	};

	Scene (OrbiterGraphics *og);
	~Scene();

	void UnregisterDevices ();
	void RegisterDevices (LPDIRECT3DDEVICE7 dv);
	// To be called before and after a render devices changes

	void OnOptionChanged(int cat, int item);

	void Init3DFonts ();

	int GetObjects (VObject ***list) { *list = vobj; return nobj; }
	// return visual object list in 'list'. Return value is length of list

	//void SkipObject (Body *_bskip) { skipbody = _bskip; }
	// set object to skip during render (0 = don't skip)

	void AddStarlight (Star *star);
	STARLIGHT *GetStarlight (int idx) { return starlight+idx; }
	D3DLIGHT7 *GetLight () { return star_lght; } // temporary

	void AddLocalLight (const LightEmitter *le, const VObject *vo, DWORD idx);

	inline const LPDIRECTDRAWSURFACE7 GetDefaultParticleTexture() const { return gtex[3]; }

	void Update (PlanetarySystem *psys, Camera **camlist, DWORD ncam, bool moving, bool force);
	// Update scene to current simulation time

	void Timejump (PlanetarySystem *psys, Camera **camlist, DWORD ncam, bool moving);
	// discontinuous step

	void DelVisual (Body *body);
	// remove visual for object "body" from the scene

	void AddBody (Body *body, Camera **camlist, int ncam);
	// register a new body (create visual if required)

	void AddParticleStream (D3D7ParticleStream *_pstream);

	bool ParticleStreamExists (const oapi::ParticleStream *_pstream) const;

	inline void SetDefaultMaterial () { dev->SetMaterial (&default_mtrl); }
	inline void SetShadowMaterial () { dev->SetMaterial (&shadow_mtrl); }

	int BgBrightnessLevel () const
	{ return atmidx; }
	// return render brightness level of sky background [0..255]

	void Render (D3DRECT* vp_rect);
	// Render the scene in vp using device dev

	void Render3DLabel (const Vector &p, const char *label, double scale = 1.0, DWORD colour = D3DRGBA(1,1,1,1));
	// Render text "label" at position p using current world matrix

	void RenderObjectMarker (oapi::Sketchpad* pSkp, const Vector &gpos, const std::string& label1, const std::string& label2 = 0, int mode = 0, int scale = 0);
	// Render a box with label to mark an object at global position gpos

	void RenderVesselShadows ();

	VObject *GetVisual (const Body *body);
	// Return a visual for object "body", if it exists. If the object has more than
	// one visual, the first one found is returned. If the object has no visual, "NULL" is
	// returned.

	// some commonly used materials
	static D3DMATERIAL7 default_mtrl;  // default material
	static D3DMATERIAL7 shadow_mtrl;   // shadow material
	D3DLIGHT7    *star_lght;           // star light template

	inline LPDIRECTDRAWSURFACE7 HazeTex() const { return gtex[1]; }
	inline LPDIRECTDRAWSURFACE7 ReentryTex() const { return gtex[2]; }
	inline const VECTOR3& BGcol() const { return bgcol; }

	double MinParticleCameraDist() const;
	// return the minimum distance between the camera and any particle in the scene

protected:
	/**
     * \brief Render vector features for each visual object if requested
     *     (frame axes, force vectors, etc.)
     */
	void RenderVectors();


private:
	OrbiterGraphics *gc;      // graphics client instance

	DWORD viewW, viewH;       // viewport dimensions

	VObject **vobj;           // list of visuals maintained by the scene
	int       nobj;           // number of visuals
	int       nbuf;           // list length
	int       cobj;           // next object to be checked

	VStar   **vsun;           // list of sun visual
	int       nsun;           // number of suns

	OGCelestialSphere* m_celSphere;  // celestial sphere background

	D3D7ParticleStream **pstream; // list of particle streams
	DWORD                nstream; // number of streams

	bool locallight;    // enable local light sources
	DWORD maxlight;     // max number of light sources
	bool sunvis;        // sun visible (for glare calculation)
	int  atmidx;        // sky background brightness level (0-255)
	VECTOR3 bgcol;      // current sky background colour
	mutable double mincamparticledist; // minimum distance between camera and any particle in the scene

	struct LIGHTLIST {
		const LightEmitter *plight;
		VObject *vobj;
		double camdist2;
	} *lightlist;

	DWORD zclearflag;   // flag for zbuffer clear calls (including/excluding stencil buffer)

	void InitGDIResources();
	void ExitGDIResources();
	// allocate and free GDI resources (fonts, pens, brushes)

	void UpdateVisuals (PlanetarySystem *psys, Camera **camlist, int ncam, bool force = false);
	// update the list of visuals for objects in psys, given the list of cameras camlist.
	// Visuals are created if they are within view of at least one camera, destroyed otherwise
	// Unless "force" is set, this only updates a few objects per cycle to retain performance
	// for densely populated solar systems.

	void UpdateVisual (Body *body, Camera **camlist, int ncam);
	// Update visual for body "body" and all its children

	VObject *AddVisual (const Body *body);
	// add a visual for object "body" to the scene

	VObject *AddVisStar (Body *body);
	// add a visual for star "body" to the scene

	void DelAllVisuals ();
	// remove all visuals from the scene

	void DelParticleStream (DWORD idx);
	// remove a particle stream from the list

	/**
	 * \brief Return sky background colour based on atmospheric parameters of closest planet
	 */
	VECTOR3 SkyColour();

	LPDIRECT3DDEVICE7 dev;      // D3D device

	LPDIRECTDRAWSURFACE7 gtex[4];
	// general-use textures: 0=exhaust, 1=horizon haze, 2=reentry, 3=contrail

	LPDIRECTDRAWSURFACE7 gfont[1];
	// font bitmap for use in label textures: Arial

	int gfont_cw[1][95];
	int gfont_ofs[1][95];
	int gfont_yofs[95];
	int comlabel_ofs[6], comlabel_w[6];
	// list of character widths

	LPDIRECTDRAWSURFACE7 gcanvas;
	// multipurpose texture of size CANVAS_SIZE

	STARLIGHT *starlight;
	int nstarlight;        // number of star light objects in the list

	// Sketchpad resources
	oapi::Font* labelFont[4];
};

#endif // !__SCENE_H

