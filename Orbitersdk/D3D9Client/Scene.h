// ==============================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Released under GNU General Public License
// Copyright (C) 2006 Martin Schweiger
//				 2012 Jarmo Nikkanen
// ==============================================================

// ==============================================================
// Class Scene (interface)
//
// A "Scene" represents the 3-D world as seen from a specific
// viewpoint ("camera"). Each scene therefore has a camera object
// associated with it. The Orbiter core supports a single
// camera, but in principle a graphics client could define
// multiple scenes and render them simultaneously into separate
// windows (or into MFD display surfaces, etc.)
// ==============================================================

#ifndef __SCENE_H
#define __SCENE_H

#include "D3D9Client.h"
#include "CelSphere.h"
#include "VObject.h"

class vObject;
class D3D9ParticleStream;
class CSphereManager;
class D3D9Text;

class Scene {

	struct VOBJREC {           // linked list of object visuals
		vObject *vobj;         // visual instance
		int	type;
		float apprad;
		VOBJREC *prev, *next;  // previous and next list entry
	} *vobjFirst, *vobjLast;   // first and last list entry

	struct LIGHTLIST {
		const LightEmitter *plight;
		vObject *vobj;
		double camdist2;
	} *lightlist;

public:

	static void D3D9TechInit(LPDIRECT3DDEVICE9 pDev, const char *folder);
	static void GlobalExit();

	Scene (oapi::D3D9Client *_gc, DWORD w, DWORD h);
	~Scene ();

	/**
	 * \brief Get a pointer to the client
	 */
	inline const oapi::D3D9Client *GetClient() const { return gc; }

	const D3D9Light *GetLight(int index) const;
	const D3D9Light *GetLights() const { return Lights; }
	DWORD GetLightCount() const { return nLights; }
 
	inline DWORD GetStencilDepth() const { return stencilDepth; }

	/**
	 * \brief Get the ambient background colour
	 */
	inline D3DCOLOR GetBgColour() const { return bg_rgba; }

	/**
	 * \brief Get the viewport dimension (width)
	 */
	inline const DWORD ViewW() const { return viewW; }

	/**
	 * \brief Get the viewport dimension (height)
	 */
	inline const DWORD ViewH() const { return viewH; }

	void UpdateCamVis();

	/**
	 * \brief Checks if hObj is within visual range.
	 * Checks if hObj is within visual range, and creates or deletes the
	 * associated vObject as required.
	 */
	void CheckVisual (OBJHANDLE hObj);

	void Initialise ();

	/**
	 * \brief Update camera position, visuals, etc.
	 */
	void Update ();

	/**
	 * \brief Render the whole scene
	 */
	void Render ();

	/**
	 * \brief Render any shadows cast by vessels on planet surfaces
	 * \param hPlanet handle of planet to cast shadows on
	 * \param depth shadow darkness parameter (0=none, 1=black)
	 * \note Uses stencil buffering if available and requested. Otherwise shadows
	 *   are pure black.
	 * \note Requests for any planet other than that closest to the camera
	 *   are ignored.
	 */
	void RenderVesselShadows(OBJHANDLE hPlanet, float depth) const;

	/**
	 * \brief Create a visual for a new vessel if within visual range.
	 * \param hVessel vessel object handle
	 */
	void NewVessel (OBJHANDLE hVessel);

	/**
	 * \brief Delete a vessel visual prior to destruction of the logical vessel.
	 * \param hVessel vessel object handle
	 */
	void DeleteVessel (OBJHANDLE hVessel);

	void AddParticleStream (class D3D9ParticleStream *_pstream);
	void DelParticleStream (DWORD idx);

	void AddLocalLight(const LightEmitter *le, const vObject *vo, DWORD idx);

	double GetObjectAppRad(OBJHANDLE hObj) const;

	const LPD3DXMATRIX GetProjectionViewMatrix() const { return (LPD3DXMATRIX)&mProjView; }
	const LPD3DXMATRIX GetProjectionMatrix() const { return (LPD3DXMATRIX)&mProj; }
	const LPD3DXMATRIX GetViewMatrix() const { return (LPD3DXMATRIX)&mView; }

	// Camera Interface
	//
	void		SetCameraAperture(double _ap);
	void		SetCameraFustrumLimits(double nearlimit, double farlimit);
	void		UpdateCameraFromOrbiter();

	bool		CameraPan(VECTOR3 pan, double speed);
	bool		IsVisibleInCamera(D3DXVECTOR3 *pCnt, float radius);
	float		GetCameraAspect() const { return (float)aspect; }
	float		GetCameraFarPlane() const { return farplane; }
	float		GetCameraAperture() const { return (float)aperture; }
	VECTOR3		GetCameraGPos() const { return camera_pos; }
	//VECTOR3		GetCameraRPos() const { return camera_rpos; }
	VECTOR3		GetCameraGDir() const { return camera_dir; }
	OBJHANDLE	GetCameraProxyBody() const { return hObj_proxy; }
	double		GetCameraAltitude() const { return alt_proxy; }

	const D3DXVECTOR3 *GetCameraX() const { return &camera_x; }
	const D3DXVECTOR3 *GetCameraY() const { return &camera_y; }
	const D3DXVECTOR3 *GetCameraZ() const { return &camera_z; }

	class vObject *GetVisObject(OBJHANDLE hObj);

	// Locate the visual for hObj in the list if present, or return
	// NULL if not found

protected:

	/**
	 * \brief Render a single marker for a given direction
	 * \param hDC device context
	 * \param rdir normalised direction from camera in global (ecliptic) frame
	 * \param label1 label above marker
	 * \param label2 label below marker
	 * \param mode marker shape
	 * \param scale marker size
	 */
	void RenderDirectionMarker(oapi::Sketchpad *pSkp, const VECTOR3 &rdir, const char *label1, const char *label2, int mode, int scale);
	
	/**
	 * \brief Render a single marker at a given global position
	 * \param hDC device context
	 * \param gpos global position (ecliptic frame)
	 * \param label1 label above marker
	 * \param label2 label below marker
	 * \param mode marker shape
	 * \param scale marker size
	 */
	void RenderObjectMarker(oapi::Sketchpad *pSkp, const VECTOR3 &gpos, const char *label1, const char *label2, int mode, int scale);

private:

	DWORD GetActiveParticleEffectCount();

	VOBJREC *FindVisual (OBJHANDLE hObj);
	// Locate the visual for hObj in the list if present, or return
	// NULL if not found

	void DelVisualRec (VOBJREC *pv);
	void DeleteAllVisuals();
	// Delete entry pv from the list of visuals

	VOBJREC *AddVisualRec (OBJHANDLE hObj);
	// Add an entry for object hObj in the list of visuals

	VECTOR3 SkyColour ();
	// Sky background colour based on atmospheric parameters of closest planet

	void InitGDIResources();
	void ExitGDIResources();

	
	

	// Scene variables ================================================================
	//
	oapi::D3D9Client *gc;
	LPDIRECT3DDEVICE9 pDevice; // render device
	DWORD viewW, viewH;        // render viewport size
	DWORD stencilDepth;        // stencil buffer bit depth
	CelestialSphere *csphere;  // celestial sphere background
	DWORD iVCheck;             // index of last object checked for visibility
	bool  bLocalLight;

	OBJHANDLE hSun;

	D3D9ParticleStream **pstream; // list of particle streams
	DWORD                nstream; // number of streams

	D3D9Light **light;		// only one for now
	D3DCOLOR bg_rgba;          // ambient background colour

	// GDI resources ====================================================================
	//
	static COLORREF labelCol[6];
	static oapi::Pen *lblPen[6];
	int   labelSize[1];

	// camera fustrum parameters ========================================================
	//
	float  aperture;        // aperture [rad]
	float  aspect;          // aspect ratio
	float  nearplane;       // fustrum nearplane distance
	float  farplane;        // fustrum farplane distance
	float  apsq;
	float  vh,vw,vhf,vwf;

	VECTOR3		camera_pos;		// Global camera position
	VECTOR3		camera_relpos;	// Relative camera position
	VECTOR3		camera_dir;		// Camera direction
	D3DXVECTOR3 camera_x;
	D3DXVECTOR3 camera_y;
	D3DXVECTOR3 camera_z;

	D3DXMATRIX	mView;       // D3D view matrix for current camera state
	D3DXMATRIX	mProj;       // D3D projection matrix for current camera state
	D3DXMATRIX	mProjView;	// product of projection and view matrix
	D3D9Light*	Lights;
	D3D9Light	sunLight;
	DWORD		nLights;
	DWORD		maxlight;

	oapi::Font *pAxisFont;
	oapi::Font *pLabelFont;
	oapi::Font *pDebugFont;

	D3D9ClientSurface *pLblSrf;
	CSphereManager *cspheremgr;

	OBJHANDLE hObj_proxy;   // closest celestial body
	OBJHANDLE hCameraTarget;
	double alt_proxy;       // camera distance to surface of hObj_proxy
	double dVisualAppRad;

	// Rendering Technique related parameters ============================================
	//
	static ID3DXEffect	*FX;
	static D3DXHANDLE	eLine;
	static D3DXHANDLE	eStar;
	static D3DXHANDLE	eWVP;
	static D3DXHANDLE	eColor;
	static D3DXHANDLE	eTex0;

};

#endif // !__SCENE_H