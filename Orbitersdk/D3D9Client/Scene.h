// ==============================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
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
class vPlanet;
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

	struct CAMREC {
		MATRIX3		mRotation;
		VECTOR3		vPosition;
		double		dAperture;
		SURFHANDLE	hSurface;
		OBJHANDLE	hVessel;
		DWORD		dwFlags;
		int			iError;
		bool		bActive;
		CAMREC  *prev, *next;
	} *camFirst, *camLast, *camCurrent;

public:

	static void D3D9TechInit(LPDIRECT3DDEVICE9 pDev, const char *folder);
	static void GlobalExit();

	Scene (oapi::D3D9Client *_gc, DWORD w, DWORD h);
	~Scene ();

	/**
	 * \brief Get a pointer to the client
	 */
	//inline const oapi::D3D9Client *GetClient() const { return gc; }
	inline oapi::D3D9Client *GetClient() const { return gc; }

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
	void Update();

	/**
	 * \brief Render the whole main scene
	 */
	void RenderMainScene();

	/**
	 * \brief Render a secondary scene. (Env Maps, Shadow Maps, MFD Camera Views)
	 */
	void RenderSecondaryScene(class vObject *omit=NULL, bool bOmitAtc=false, DWORD flags=0xFF);

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

	/**
	 * \brief Get object radius in pixels using oapiCameraGlobalPos()
	 * \param hObj object handle
	 */
	double GetObjectAppRad(OBJHANDLE hObj) const;

	/**
	 * \brief Get object radius in pixels using a custom camera location.
	 * \param hObj object handle
	 */
	double GetObjectAppRad2(OBJHANDLE hObj) const;

	D3D9Pick PickScene(short xpos, short ypos);

	void ClearOmitFlags();

	// Camera Interface
	//
	const LPD3DXMATRIX GetProjectionViewMatrix() const { return (LPD3DXMATRIX)&mProjView; }
	const LPD3DXMATRIX GetProjectionMatrix() const { return (LPD3DXMATRIX)&mProj; }
	const LPD3DXMATRIX GetViewMatrix() const { return (LPD3DXMATRIX)&mView; }


	// Custom Camera Interface ======================================================================================================
	//
	CAMERAHANDLE	SetupCustomCamera(CAMERAHANDLE hCamera, OBJHANDLE hVessel, MATRIX3 &mRot, VECTOR3 &pos, double fov, SURFHANDLE hSurf, DWORD flags);
	int				DeleteCustomCamera(CAMERAHANDLE hCamera);
	void			DeleteAllCustomCameras();
	void			CustomCameraOnOff(CAMERAHANDLE hCamera, bool bOn);
	void			RenderCustomCameraView(CAMREC *cCur);


	// Main Camera Interface =========================================================================================================
	//
	void			SetCameraAperture(double _ap, double _as);
	void			SetCameraFrustumLimits(double nearlimit, double farlimit);
	float			GetDepthResolution(float dist) const { return fabs((nearplane-farplane)*(dist*dist) / (farplane*nearplane*24.0f)); }

					// Imitialize Main Camera Setup
	void			UpdateCameraFromOrbiter();

					// Temporary main camera view change. Will be restored to normal view in a start of the next frame.
	void			SetupCustomCamera(D3DXMATRIX &mRotation, VECTOR3 &disp, double apr, double asp);
	
					// This function will take the camera control from the Orbiter and places the camera in user defined location.
					// Orbiter will continue moving it's own camera while taken so, camera may end-up in an odd place when released.
	void			TakeCamera(MATRIX3 &mRotation, VECTOR3 &disp, double apr);
	void			ReleaseCameraTake();

					// Pan Camera in a mesh debugger
	bool			CameraPan(VECTOR3 pan, double speed);

					// Check if a sphere located in pCnt (relative to cam) with a specified radius is visible in a camera 
	bool			IsVisibleInCamera(D3DXVECTOR3 *pCnt, float radius);
	double			GetTanAp() const { return tan(aperture); }
	float			GetCameraAspect() const { return (float)aspect; }
	float			GetCameraFarPlane() const { return farplane; }
	float			GetCameraNearPlane() const { return nearplane; }
	float			GetCameraAperture() const { return (float)aperture; }
	VECTOR3			GetCameraGPos() const { return camera_pos; }
	VECTOR3			GetCameraGDir() const { return camera_dir; }
	OBJHANDLE		GetCameraProxyBody() const { return hObj_proxy; }
	vPlanet *		GetCameraProxyVisual() const { return vProxy; }
	double			GetCameraAltitude() const { return alt_proxy; }
	D3DXVECTOR3		GetCameraOffset() { return camera_offset; }

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

	DWORD	GetActiveParticleEffectCount();
	float	ComputeNearClipPlane();
	void	VisualizeCubeMap(LPDIRECT3DCUBETEXTURE9 pCube);

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
	int nlight;

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

	// camera frustum parameters ========================================================
	//
	float  aperture;        // aperture [rad]
	float  aspect;          // aspect ratio
	float  nearplane;       // frustum nearplane distance
	float  farplane;        // frustum farplane distance
	float  apsq;
	float  vh,vw,vhf,vwf;
	bool   bCustomCam;		// true if a custom camera mode is in use. (apply camera_offset)

	// Default Camera Override Paramaters =============================================== 
	//
	bool	bTakeCamera;	// true if user application has a control of the camera. (apply camera_offset) 
	MATRIX3 mTakeRotation;
	VECTOR3 vTakeOffset;
	double	dTakeAperture;



	VECTOR3		camera_pos;		// Global camera position
	VECTOR3		camera_relpos;	// Relative camera position (Used by Mesh Debugger)
	VECTOR3		camera_dir;		// Camera direction
	VECTOR3		sky_color;

	D3DXVECTOR3 camera_x;
	D3DXVECTOR3 camera_y;
	D3DXVECTOR3 camera_z;
	D3DXVECTOR3	camera_offset;	

	D3DXMATRIX	mView;			// D3D view matrix for current camera state
	D3DXMATRIX	mProj;			// D3D projection matrix for current camera state
	D3DXMATRIX	mProjView;		// product of projection and view matrix
	D3D9Light*	Lights;
	D3D9Light	sunLight;

	DWORD		nLights;
	DWORD		maxlight;
	DWORD		nplanets;		// Number of distance sorted planets to render
	DWORD		dwTurn;

	oapi::Font *pAxisFont;
	oapi::Font *pLabelFont;
	oapi::Font *pDebugFont;

	D3D9ClientSurface *pLblSrf;
	CSphereManager *cspheremgr;
	ID3DXRenderToEnvMap *pENV;

	class vVessel *vFocus;
	VOBJREC *vobjEnv;

	OBJHANDLE hObj_proxy;		// closest celestial body
	vPlanet *vProxy;			// closest celestial body (visual)
	OBJHANDLE hCameraTarget;	// Current camera target, Mesh Debugger Related
	double alt_proxy;			// camera distance to surface of hObj_proxy
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