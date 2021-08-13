// ==============================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2006-2016 Martin Schweiger
//				 2012-2016 Jarmo Nikkanen
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
#include <stack>
#include <list>
#include <set>

class vObject;
class vPlanet;
class D3D9ParticleStream;
class CSphereManager;
class D3D9Text;
class D3D9Pad;

#define GBUF_COLOR				0
#define GBUF_BLUR				1
#define GBUF_TEMP				2
#define GBUF_DEPTH				3
#define GBUF_GDI				4
#define GBUF_COUNT				5	// Buffer count

#define SHM_LOD_COUNT			5

#define TEX_NOISE				0
#define TEX_CLUT				1
#define TEX_COUNT				2

#define RENDERPASS_MAINSCENE	0x0001
#define RENDERPASS_ENVCAM		0x0002
#define RENDERPASS_CUSTOMCAM	0x0003
#define RENDERPASS_SHADOWMAP	0x0004
#define RENDERPASS_PICKSCENE	0x0005
#define RENDERPASS_SKETCHPAD	0x0006
#define RENDERPASS_MAINOVERLAY	0x0007

#define RESTORE ((LPDIRECT3DSURFACE9)(-1))
#define CURRENT ((LPDIRECT3DSURFACE9)(-2))

#define RENDERTURN_ENVCAM		0
#define RENDERTURN_CUSTOMCAM	1
#define RENDERTURN_IRRADIANCE   2
#define RENDERTURN_LAST			2

#define SMAP_MODE_FOCUS			1
#define SMAP_MODE_SCENE			2

#define OBJTP_BUILDING			1000

class Scene {


	// Visual record ===================================================================
	//
	struct VOBJREC {           // linked list of object visuals
		vObject *vobj;         // visual instance
		int	type;
		float apprad;
		VOBJREC *prev, *next;  // previous and next list entry
	} *vobjFirst, *vobjLast;   // first and last list entry


	// Custom camera parameters ========================================================
	//
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

	// Camera frustum parameters ========================================================
	//
	struct CAMERA {
		float		aperture;   // aperture [rad]
		float		aspect;     // aspect ratio
		float		nearplane;  // frustum nearplane distance
		float		farplane;   // frustum farplane distance
		float		apsq;
		float		vh, vw, vhf, vwf;

		VECTOR3		pos;		// Global camera position
		VECTOR3		relpos;		// Relative camera position (Used by Mesh Debugger)
		VECTOR3		dir;		// Camera direction

		D3DXVECTOR3 x;			// Camera axis vector
		D3DXVECTOR3 y;			// Camera axis vector
		D3DXVECTOR3 z;			// Camera axis vector
		D3DXVECTOR3 upos;		// Camera position unit vector

		D3DXMATRIX	mView;		// D3DX view matrix for current camera state
		D3DXMATRIX	mProj;		// D3DX projection matrix for current camera state
		D3DXMATRIX	mProjView;	// D3DX combined projection view matrix
		D3DXMATRIX  mProjViewInf; // D3DX combined projection view matrix, far plane at infinity
		OBJHANDLE	hTarget;	// Current camera target, Mesh Debugger Related

		OBJHANDLE	hObj_proxy;	// closest celestial body
		vPlanet *	vProxy;		// closest celestial body (visual)
		double		alt_proxy;	// camera distance to surface of hObj_proxy

		OBJHANDLE	hNear;		// closest celestial body
		vPlanet *	vNear;		// closest celestial body (visual)
		double		alt_near;
	};

	// Screen space sun visual parameters ==================================================
	//
	struct SUNVISPARAMS {
		float		brightness;
		bool		visible;
		D3DXVECTOR2 position;
		D3DXCOLOR	color;
	};

	struct SHADOWMAPPARAM {
		LPDIRECT3DTEXTURE9 pShadowMap;
		D3DXMATRIX	mProj, mView, mViewProj;
		D3DXVECTOR3	pos;
		D3DXVECTOR3	ld;
		float		rad;
		float		dist;
		float		depth;
		int			lod;
		int			size;
	} smap;

	static void D3D9TechInit(LPDIRECT3DDEVICE9 pDev, const char *folder);

	/**
	 * \brief Release global parameters
	 */
	static void GlobalExit();

	Scene (oapi::D3D9Client *_gc, DWORD w, DWORD h);
	~Scene ();

	/**
	 * \brief Get a pointer to the client
	 */
	//inline const oapi::D3D9Client *GetClient() const { return gc; }
	inline oapi::D3D9Client *GetClient() const { return gc; }

	const D3D9Sun *Scene::GetSun() const { return &sunLight; }
	const D3D9Light *GetLight(int index) const;
	const D3D9Light *GetLights() const { return Lights; }
	DWORD GetLightCount() const { return nLights; }


	DWORD GetRenderPass() const;
	void BeginPass(DWORD dwPass);
	void PopPass();

	inline DWORD GetStencilDepth() const { return stencilDepth; }
	inline const SHADOWMAPPARAM * GetSMapData() const { return &smap; }
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
	 * \brief Returns screen space sun visual parameters for Lens Flare rendering.
	*/
	SUNVISPARAMS GetSunScreenVisualState();

	/**
	 * \brief Gets sun diffuse color (accounting for atmospheric shift)
	 */
	D3DXCOLOR GetSunDiffColor();

	/**
	 * \brief Render a secondary scene. (Env Maps, Shadow Maps, MFD Camera Views)
	 */
	void RenderSecondaryScene(std::set<class vVessel*> &RndList, std::set<class vVessel*> &AdditionalLightsList, DWORD flags = 0xFF);
	int RenderShadowMap(D3DXVECTOR3 &pos, D3DXVECTOR3 &ld, float rad, bool bInternal = false, bool bListExists = false);

	bool IntegrateIrradiance(vVessel *vV, LPDIRECT3DCUBETEXTURE9 pSrc, LPDIRECT3DTEXTURE9 pOut);
	bool RenderBlurredMap(LPDIRECT3DDEVICE9 pDev, LPDIRECT3DCUBETEXTURE9 pSrc);
	void RenderMesh(DEVMESHHANDLE hMesh, const oapi::FMATRIX4 *pWorld);

	LPDIRECT3DSURFACE9 GetIrradianceDepthStencil() const { return pIrradDS; }
	LPDIRECT3DSURFACE9 GetEnvDepthStencil() const { return pEnvDS; }
	LPDIRECT3DSURFACE9 GetBuffer(int id) const { return psgBuffer[id]; }

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

	void AddLocalLight(const LightEmitter *le, const vObject *vo);
	void ClearLocalLights();

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

	// Picking Functions ============================================================================================================
	//
	D3DXVECTOR3		GetPickingRay(short x, short y);
	D3D9Pick		PickScene(short xpos, short ypos);
	TILEPICK		PickSurface(short xpos, short ypos);
	D3D9Pick		PickMesh(DEVMESHHANDLE hMesh, const LPD3DXMATRIX pW, short xpos, short ypos);

	void			ClearOmitFlags();
	bool			IsRendering() const { return bRendering; }


	// Custom Camera Interface ======================================================================================================
	//
	CAMERAHANDLE	SetupCustomCamera(CAMERAHANDLE hCamera, OBJHANDLE hVessel, MATRIX3 &mRot, VECTOR3 &pos, double fov, SURFHANDLE hSurf, DWORD flags);
	int				DeleteCustomCamera(CAMERAHANDLE hCamera);
	void			DeleteAllCustomCameras();
	void			CustomCameraOnOff(CAMERAHANDLE hCamera, bool bOn);
	void			RenderCustomCameraView(CAMREC *cCur);


	// Camera Matrix Access =========================================================================================================
	//
	void			   GetAdjProjViewMatrix(LPD3DXMATRIX mP, float znear, float zfar);
	const LPD3DXMATRIX GetProjectionViewMatrix() const { return (LPD3DXMATRIX)&Camera.mProjView; }
	const LPD3DXMATRIX GetProjectionMatrix() const { return (LPD3DXMATRIX)&Camera.mProj; }
	const LPD3DXMATRIX GetViewMatrix() const { return (LPD3DXMATRIX)&Camera.mView; }


	// Main Camera Interface =========================================================================================================
	//
	void			SetCameraAperture(float _ap, float _as);
	void			SetCameraFrustumLimits(double nearlimit, double farlimit);
	float			GetDepthResolution(float dist) const;

					// Acquire camera information from the Orbiter and initialize internal camera setup
	void			UpdateCameraFromOrbiter(DWORD dwPass);

					// Manually initialize client's internal camera setup
	void			SetupInternalCamera(D3DXMATRIX *mView, VECTOR3 *pos, double apr, double asp);

					// Pan Camera in a mesh debugger
	bool			CameraPan(VECTOR3 pan, double speed);

					// Check if a sphere located in pCnt (relative to cam) with a specified radius is visible in a camera
	bool			IsVisibleInCamera(D3DXVECTOR3 *pCnt, float radius);
	bool			IsProxyMesh();
	bool            CameraDirection2Viewport(const VECTOR3 &dir, int &x, int &y);
	double			GetTanAp() const { return tan(Camera.aperture); }
	float			GetCameraAspect() const { return (float)Camera.aspect; }
	float			GetCameraFarPlane() const { return Camera.farplane; }
	float			GetCameraNearPlane() const { return Camera.nearplane; }
	float			GetCameraAperture() const { return (float)Camera.aperture; }
	VECTOR3			GetCameraGPos() const { return Camera.pos; }
	VECTOR3			GetCameraGDir() const { return Camera.dir; }
	OBJHANDLE		GetCameraProxyBody() const { return Camera.hObj_proxy; }
	vPlanet *		GetCameraProxyVisual() const { return Camera.vProxy; }
	double			GetCameraAltitude() const { return Camera.alt_proxy; }
	OBJHANDLE		GetCameraNearBody() const { return Camera.hNear; }
	vPlanet *		GetCameraNearVisual() const { return Camera.vNear; }
	double			GetCameraNearAltitude() const { return Camera.alt_near; }

	void			GetCameraLngLat(double *lng, double *lat) const;
	bool			WorldToScreenSpace(const VECTOR3 &rdir, oapi::IVECTOR2 *pt, D3DXMATRIX *pVP, float clip = 1.0f);

	DWORD			GetFrameId() const { return dwFrameId; }

	const D3DXVECTOR3 *GetCameraX() const { return &Camera.x; }
	const D3DXVECTOR3 *GetCameraY() const { return &Camera.y; }
	const D3DXVECTOR3 *GetCameraZ() const { return &Camera.z; }

	const CAMERA *	GetCamera() const { return &Camera; }

	void			PushCamera();	// Push current camera onto a stack
	void			PopCamera();	// Restore a camera from a stack


	// Visual Management =========================================================================================================
	//
	void			GetLVLH(vVessel *vV, D3DXVECTOR3 *up, D3DXVECTOR3 *nr, D3DXVECTOR3 *cp);
	class vObject *	GetVisObject(OBJHANDLE hObj) const;
	class vVessel *	GetFocusVisual() const { return vFocus; }
	void			CheckVisual(OBJHANDLE hObj);
	double			GetFocusGroundAltitude() const;
	double			GetTargetGroundAltitude() const;
	double			GetTargetElevation() const;
	std::set<vVessel *> GetVessels(double max_dst, bool bActive = true);

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

	DWORD		GetActiveParticleEffectCount();
	float		ComputeNearClipPlane();
	void		VisualizeCubeMap(LPDIRECT3DCUBETEXTURE9 pCube, int mip);
	VOBJREC *	FindVisual (OBJHANDLE hObj) const;
	void		RenderVesselMarker(vVessel *vV, D3D9Pad *pSketch);

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

	D3D9Pad *GetPooledSketchpad(int id); ///< Get pooled Sketchpad instance (lazy instantiation)
	void    FreePooledSketchpads();      ///< Release pooled Sketchpad instances



	// Scene variables ================================================================
	//
	oapi::D3D9Client *gc;
	LPDIRECT3DDEVICE9 pDevice; // render device
	DWORD viewW, viewH;        // render viewport size
	DWORD stencilDepth;        // stencil buffer bit depth
	CelestialSphere *csphere;  // celestial sphere background
	DWORD iVCheck;             // index of last object checked for visibility
	//DWORD dwRenderPass;		   // Currently active render pass
	bool  bLocalLight;         // enable local light sources
	bool  surfLabelsActive;    // v.2 surface labels activated?

	OBJHANDLE hSun;

	D3D9ParticleStream **pstream; // list of particle streams
	DWORD                nstream; // number of streams


	D3DCOLOR bg_rgba;          // ambient background colour

	// GDI resources ====================================================================
	//
	static COLORREF labelCol[6];
	static oapi::Pen *lblPen[6];
	int   labelSize[1];

	oapi::Font *label_font[4];
	oapi::Pen  *label_pen;

	std::list<vVessel *> RenderList;
	std::list<vVessel *> SmapRenderList;
	std::list<vVessel *> Casters;
	std::stack<CAMERA>	CameraStack;
	std::stack<DWORD>	PassStack;


	CAMERA		Camera;
	D3D9Light*	Lights;
	D3D9Sun	    sunLight;

	VECTOR3		sky_color;

	float		lmaxdst2;
	DWORD		nLights;
	DWORD		nplanets;		// Number of distance sorted planets to render
	DWORD		dwTurn;
	DWORD		dwFrameId;
	bool		bRendering;

	oapi::Font *pAxisFont;
	oapi::Font *pLabelFont;
	oapi::Font *pDebugFont;

	SurfNative *pLblSrf;
	CSphereManager *cspheremgr;

	class ImageProcessing *pLightBlur, *pBlur, *pFlare, *pGDIOverlay, *pIrradiance;

	class vVessel *vFocus;
	VOBJREC *vobjEnv, *vobjIrd;
	double dVisualAppRad;

	// Blur Sampling Kernel ==============================================================
	LPDIRECT3DCUBETEXTURE9 pBlrTemp[5];
	LPDIRECT3DCUBETEXTURE9 pIrradTemp;
	LPDIRECT3DTEXTURE9 pIrradTemp2, pIrradTemp3;

	// Deferred Experiment ===============================================================
	//
	LPDIRECT3DSURFACE9 psgBuffer[GBUF_COUNT];
	LPDIRECT3DTEXTURE9 ptgBuffer[GBUF_COUNT];
	LPDIRECT3DSURFACE9 pOffscreenTarget;
	LPDIRECT3DTEXTURE9 pTextures[TEX_COUNT];

	LPDIRECT3DSURFACE9 pEnvDS, pIrradDS;
	LPDIRECT3DSURFACE9 psShmDS[SHM_LOD_COUNT];
	LPDIRECT3DSURFACE9 psShmRT[SHM_LOD_COUNT];
	LPDIRECT3DTEXTURE9 ptShmRT[SHM_LOD_COUNT];

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
