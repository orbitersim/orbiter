
// ==============================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Released under GNU General Public License
// Copyright (C) 2007 Martin Schweiger
//				 2012 Jarmo Nikkanen
// ==============================================================

#ifndef __D3D9CLIENT_H
#define __D3D9CLIENT_H

// must be defined before windows includes to fix warnins on VS 2003+
#if defined(_MSC_VER) && (_MSC_VER >= 1300 ) // Microsoft Visual Studio Version 2003 and higher
//#define _CRT_SECURE_NO_DEPRECATE 
//#define _CRT_NONSTDC_NO_WARNINGS
#include <fstream>
#else  // older MSVC++ versions
#include <fstream.h>
#endif

#include "OapiVersion.h"

#include <d3d9.h> 
#include <d3dx9.h>
#include "GraphicsAPI.h"
#include "D3D9util.h"
#include <stdio.h>
#include <OrbiterAPI.h>
#include "D3D9Frame.h"
#include "nvapi.h"

#ifdef _NVAPI_H
extern StereoHandle	pStereoHandle;
#endif

class D3D9Config;
class MeshManager;
class TextureManager;
class Scene;
class VideoTab;
class D3D9ClientSurface;
class CD3DFramework9;
class D3D9Mesh;
class D3D9Annotation;
class D3D9Text;
class CSphereManager;
class D3D9Catalog;
class FileParser;
class OapiExtension;



extern D3D9Catalog	*TileCatalog;
extern D3D9Catalog	*MeshCatalog;
extern D3D9Catalog	*SurfaceCatalog;
extern DWORD uCurrentMesh;
extern class vObject * pCurrentVisual;

int		ExcHandler(EXCEPTION_POINTERS *p);
void	MissingRuntimeError();

typedef class D3D9ClientSurface * LPD3D9CLIENTSURFACE;
typedef char * LPCHAR;


struct D3D9Stat {
	DWORD Vertices;		// Number of vertices rendered
	DWORD Draw;
	DWORD Blits;
	DWORD ColorKey;
	DWORD MeshGrps;
	DWORD Meshes;
	DWORD Tiles[18];
	double Frame, FramePeak;
	double Scene, ScenePeak;
	double count;
};

namespace oapi {



// ==============================================================
// D3D9Client class interface
/// The DX7 render client for Orbiter
// ==============================================================

class D3D9Client: public GraphicsClient {

	friend class ::VideoTab;
	friend class ::Scene;
	friend class ::MeshManager;
	friend class ::TextureManager;
	friend class ::D3D9ClientSurface;
	friend class ::CD3DFramework9;

public:
	D3D9Client (HINSTANCE hInstance);
	~D3D9Client ();

	/**
	 * \brief Message handler for 'video' tab in Orbiter Launchpad dialog.
	 *
	 * Passes the message on to the VideoTab::WndProc() method.
	 * \param hWnd window handle for video tab
	 * \param uMsg Windows message
	 * \param wParam WPARAM message value
	 * \param lParam LPARAM message value
	 * \return The return value of the VideoTab::WndProc() method.
	 */
	BOOL LaunchpadVideoWndProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);

	/**
	 * \brief Render window message handler.
	 * \param hWnd render window handle
	 * \param mMsg Windows message identifier
	 * \param wParam WPARAM message parameter
	 * \param lParam LPARAM message parameter
	 * \return The return value depends on the message being processed.
	 * \note Currently this only intercepts the WM_MOVE message in windowed mode
	 *   to allow DirectX to adjust the render target position.
	 * \note All other messages are passed on to the base class method.
	 */
	LRESULT RenderWndProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);

	/**
	 * \brief Copies video options from the video tab.
	 *
	 * Scans the dialog elements of the Launchpad video tab and stores the values
	 * in the GraphicsClient::VIDEODATA structure pointed to by GetVideoData().
	 */
	void clbkRefreshVideoData ();

	/**
	 * \brief Fullscreen mode flag
	 * \return true in fullscreen mode, false in windowed mode.
	 */
	bool clbkFullscreenMode () const;

	/**
	 * \brief Returns the dimensions of the render viewport
	 * \param width render viewport width [pixel]
	 * \param height render viewport height [pixel]
	 */
	void clbkGetViewportSize (DWORD *width, DWORD *height) const;

	/**
	 * \brief Returns a specific render parameter
	 * \param prm[in] parameter identifier (see \sa renderprm)
	 * \param value[out] value of the queried parameter
	 * \return true if the specified parameter is supported, false if not.
	 */
	bool clbkGetRenderParam (DWORD prm, DWORD *value) const;

	/**
	 * \brief Responds to visual events.
	 * \param hObj handle of the object that created the message
	 * \param vis handle for the visual (pointer to a VisObject)
	 * \param msg event identifier
	 * \param context message context
	 * \return Returns 1 if the event is recognised, 0 otherwise.
	 * \note This callback method receives visual events (mesh addition/deletion, etc.)
	 *   from the orbiter core and distributes them to the appropriate
	 *   visual object.
	 */
	int clbkVisEvent (OBJHANDLE hObj, VISHANDLE vis, DWORD msg, UINT context);

	/**
	 * \brief Return a mesh handle for a visual, defined by its index
	 * \param vis visual identifier
	 * \param idx mesh index (>= 0)
	 * \return Mesh handle (client-specific)
	 * \note This method returns a handle that identifies a mesh for the
	 *   visual (in client-specific format).
	 * \note Orbiter calls this method in response to a \ref VESSEL::GetMesh
	 *   call by an vessel module.
	 */
	virtual MESHHANDLE clbkGetMesh (VISHANDLE vis, UINT idx);

	/**
	 * \brief Mesh group editing interface for device-specific meshes.
	 * \param hMesh device mesh handle
	 * \param grpidx mesh group index (>= 0)
	 * \param ges mesh group modification specs
	 * \return Returns 0 on success, or error flags > 0.
	 */
	int clbkEditMeshGroup (DEVMESHHANDLE hMesh, DWORD grpidx, GROUPEDITSPEC *ges);

	// particle stream methods
	oapi::ParticleStream *clbkCreateParticleStream (PARTICLESTREAMSPEC *pss);
	oapi::ParticleStream *clbkCreateExhaustStream (PARTICLESTREAMSPEC *pss,	OBJHANDLE hVessel, const double *lvl, const VECTOR3 *ref, const VECTOR3 *dir);
	oapi::ParticleStream *clbkCreateExhaustStream (PARTICLESTREAMSPEC *pss,	OBJHANDLE hVessel, const double *lvl, const VECTOR3 &ref, const VECTOR3 &dir);
	oapi::ParticleStream *clbkCreateReentryStream (PARTICLESTREAMSPEC *pss,	OBJHANDLE hVessel);

	bool clbkParticleStreamExists (const oapi::ParticleStream *ps);

	/**
	 * \brief Texture request
	 *
	 * Read a single texture in DXT? format from a file into a device-specific
	 * texture object, and return a generic surface handle for it
	 * \param fname texture file name with relative path
	 * \param flags texture properties (see documentation of parent class method)
	 */
	SURFHANDLE clbkLoadTexture (const char *fname, DWORD flags = 0);

	/**
	 * \brief Texture release request
	 *
	 * Releases the specified texture from device memory.
	 * \param hTex texture handle
	 */
	void clbkReleaseTexture (SURFHANDLE hTex);

	/**
	 * \brief Replace a texture in a device-specific mesh.
	 * \param hMesh device mesh handle
	 * \param texidx texture index (>= 0)
	 * \param tex texture handle
	 * \return \e true if operation successful, \e false otherwise.
	 */
	bool clbkSetMeshTexture (DEVMESHHANDLE hMesh, DWORD texidx, SURFHANDLE tex);

	/**
	 * \brief Replace properties of an existing mesh material.
	 * \param hMesh device mesh handle
	 * \param matidx material index (>= 0)
	 * \param mat pointer to material structure
	 * \return Error flag: 0="success", 4="material index out of range"
	 */
	int clbkSetMeshMaterial (DEVMESHHANDLE hMesh, DWORD matidx, const MATERIAL *mat);

	/**
     * \brief Set custom properties for a device-specific mesh.
	 * \param hMesh device mesh handle
	 * \param property property tag
	 * \param value new mesh property value
	 * \return The method should return \e true if the property tag was recognised
	 *   and the request could be executed, \e false otherwise.
	 * \note Currently only a single mesh property request type is recognised:
	 * - \c MESHPROPERTY_MODULATEMATALPHA \n \n
	 * if value==0 (default) disables material alpha information in textured mesh groups (only use texture alpha channel).\n
	 * if value<>0 modulates (mix) material alpha values with texture alpha maps.
	 */
	bool clbkSetMeshProperty (DEVMESHHANDLE hMesh, DWORD property, DWORD value);

	/**
	 * \brief Popup window open notification.
	 * \note For fullscreen modes, calls FlipToGDISurface, to flip the render
	 *   surface to the GDI surface, in preparation for displaying a new
	 *   popup window.
	 */
	void clbkPreOpenPopup ();

	/**
	 * \brief React to vessel creation
	 * \param hVessel object handle of new vessel
	 * \note Calls Scene::NewVessel() to check for visual
	 */
	void clbkNewVessel (OBJHANDLE hVessel);

	/**
	 * \brief React to vessel destruction
	 * \param hVessel object handle of vessel to be destroyed
	 * \note Calls Scene::DeleteVessel() to remove the visual
	 */
	void clbkDeleteVessel (OBJHANDLE hVessel);

	/// Returns the Direct3D device
    inline const LPDIRECT3DDEVICE9    GetDevice() const       { return pd3dDevice; }

	/// Returns a pointer to the render framework
	inline const CD3DFramework9*      GetFramework() const    { return pFramework; }

	/// Returns a pointer to the scene object
	inline       Scene*               GetScene() const        { return scene; }

	/// Returns a pointer to the texture manager
	inline       TextureManager*      GetTexMgr() const       { return texmgr; }

	/// Returns a pointer to the mesh manager
	inline       MeshManager*         GetMeshMgr() const      { return meshmgr; }

	/// Returns a pointer to the name of orbiter's root folder
	inline const char *				  GetOrbiterRoot() const	{ return pOrbiterRoot; }

	// void SetDefault (D3DVERTEXBUFFERDESC &vbdesc) const;
	// Sets the dwCaps entry of vbdesc to system or video memory, depending
	// of whether a T&L device is used.

	/**
	 * \brief Render an instrument panel in cockpit view as a 2D billboard.
	 * \param hSurf array of texture handles for the panel surface
	 * \param hMesh billboard mesh handle
	 * \param T transformation matrix for panel mesh vertices (2D)
	 * \param transparent flag for transparent (additive) rendering
	 * \note The texture index of each group in the mesh is interpreted as index into the
	 *   hSurf array. Special indices are TEXIDX_MFD0 and above, which specify the
	 *   surfaces representing the MFD displays. These are obtained separately and
	 *   don't need to be present in the hSurf list.
	 */
	void clbkRender2DPanel (SURFHANDLE *hSurf, MESHHANDLE hMesh, MATRIX3 *T, bool transparent = false);

	// ==================================================================
	/// \name Surface-related methods
	// @{

	SURFHANDLE clbkCreateSurface (HBITMAP hBmp);

#ifdef OAPI_BETA
	bool clbkSaveSurfaceToImage(SURFHANDLE  surf,  const char *fname, ImageFileFormat  fmt, float quality=0.7f);
#endif

	ScreenAnnotation* clbkCreateAnnotation(); 

	/**
	 * \brief Creates a new surface.
	 * \param w surface width [pixels]
	 * \param h surface height [pixels]
	 * \param hTemplate template surface
	 * \return surface handle (LPDIRECT3DTEXTURE9 cast to SURFHANDLE)
	 * \note The new surface receives a reference count of 1.
	 * \note If a template surface is provided, the new surface is created
	 *   with the same format.
	 * \sa clbkIncrSurfaceRef, clbkReleaseSurface
	 */
	SURFHANDLE clbkCreateSurface (DWORD w, DWORD h, SURFHANDLE hTemplate = NULL);

	/**
	 * \brief Create a surface for texturing, as a blitting source, etc.
	 * 
	 * Surfaces are used for offscreen bitmap and texture manipulation,
	 * blitting and rendering.
	 * Derived classes should create a device-specific surface, and
	 * return a cast to a generic Orbiter SURFHANDLE.
	 * \param w surface width [pixels]
	 * \param h surface height [pixels]
	 * \param attrib \ref surfacecaps (bitflags). See notes.
	 * \return Surface handle (in the simplest case, just a pointer to the
	 *   surface, cast to a SURFHANDLE). On failure, this method should
	 *   return NULL.
	 * \default None, returns NULL.
	 * \note The attribute flag can contain one of the following main attributes:
	 *  - OAPISURFACE_RO: create a surface that can be read by the GPU pipeline, and that can be updated from system memory.
	 *  - OAPISURFACE_RW: create a surface that can be read and written by the GPU pipeline, and that can be updated from system memory.
	 *  - OAPISURFACE_GDI: create a surface that can be read and written from the CPU, and can be blitted into an uncompressed RO or RW surface without an alpha channel
	 *  In addition, the flag can contain any combination of the following auxiliary attributes:
	 *  - OAPISURFACE_MIPMAPS: create a full chain of mipmaps for the surface if possible
	 *  - OAPISURFACE_NOALPHA: create a surface without an alpha channel
	 */
	SURFHANDLE clbkCreateSurfaceEx(DWORD w, DWORD h, DWORD attrib);

	/**
	 * \brief Creates a new texture surface.
	 * \param w texture width
	 * \param h texture height
	 * \return surface handle (LPDIRECT3DTEXTURE9 cast to SURFHANDLE).
	 * \note The surface is created with the same format as the rendering
	 *   surface, i.e. uncompressed and without an alpha channel.
	 */
	SURFHANDLE clbkCreateTexture (DWORD w, DWORD h);

	/**
	 * \brief Increases the reference counter for a surface
	 * \param surf surface handle
	 */
	void clbkIncrSurfaceRef (SURFHANDLE surf);

	/**
	 * \brief Decreases a surface's reference counter and
	 *   releases the surface if the counter reaches 0.
	 * \param surf surface handle
	 * \return true on success, false if surf==NULL
	 */
	bool clbkReleaseSurface (SURFHANDLE surf);

	/**
	 * \brief Return the width and height of a surface
	 * \param surf surface handle
	 * \param w surface width
	 * \param h surface height
	 * \return true
	 */
	bool clbkGetSurfaceSize (SURFHANDLE surf, DWORD *w, DWORD *h);

	/**
	 * \brief Set transparency colour key for a surface
	 * \param surf surface handle
	 * \param ckey transparency colour key value
	 * \note Only source colour keys are currently supported.
	 */
	bool clbkSetSurfaceColourKey (SURFHANDLE surf, DWORD ckey);

	/**
	 * \brief Convert an RGB colour triplet into a device-specific colour value.
	 * \param r red component
	 * \param g green component
	 * \param b blue component
	 * \return colour value
	 */
	DWORD clbkGetDeviceColour (BYTE r, BYTE g, BYTE b);
	// @}

	// ==================================================================
	/// \name Surface blitting methods
	// @{

	int clbkBeginBltGroup(SURFHANDLE tgt); 
	int clbkEndBltGroup(); 

	/**
	 * \brief Copy one surface into an area of another one.
	 * \param tgt target surface handle
	 * \param tgtx left edge of target rectangle
	 * \param tgty top edge of target rectangle
	 * \param src source surface handle
	 * \param flag blitting parameters (see notes)
	 * \return \e true on success of the blitting call, \e false otherwise.
	 * \note Uses IDirectDrawSurface7::BltFast method
	 * \note Supported blitting flags are BLT_SRCCOLORKEY and BLT_TGTCOLORKEY.
	 */
	bool clbkBlt (SURFHANDLE tgt, DWORD tgtx, DWORD tgty, SURFHANDLE src, DWORD flag = 0) const;

	/**
	 * \brief Copy an area from one surface to another.
	 * \param tgt target surfac handle
	 * \param tgtx left edge of target rectangle
	 * \param tgty top edge of target rectangle
	 * \param src source surface handle
	 * \param srcx left edge of source rectangle
	 * \param srcy top edge of source rectangle
	 * \param w width of rectangle
	 * \param h height of rectangle
	 * \param flag blitting parameters (see notes)
	 * \return \e true on success of the blitting call, \e false otherwise.
	 * \note Uses IDirectDrawSurface7::BltFast method
	 * \note Supported blitting flags are BLT_SRCCOLORKEY and BLT_TGTCOLORKEY.
	 */
	bool clbkBlt (SURFHANDLE tgt, DWORD tgtx, DWORD tgty, SURFHANDLE src, DWORD srcx, DWORD srcy, DWORD w, DWORD h, DWORD flag = 0) const;

	/**
	 * \brief Copy a rectangle from one surface to another, stretching or shrinking as required.
	 * \param tgt target surface handle
	 * \param tgtx left edge of target rectangle
	 * \param tgty top edge of target rectangle
	 * \param tgtw width of target rectangle
	 * \param tgth height of target rectangle
	 * \param src source surface handle
	 * \param srcx left edge of source rectangle
	 * \param srcy top edge of source rectangle
	 * \param srcw width of source rectangle
	 * \param srch height of source rectangle
	 * \param flag blitting parameters (see notes)
	 * \return \e true on success of the blitting call, \e false otherwise.
	 * \note Uses IDirectDrawSurface7::Blt method
	 * \note Supported blitting flags are BLT_SRCCOLORKEY and BLT_TGTCOLORKEY.
	 */
	virtual bool clbkScaleBlt (SURFHANDLE tgt, DWORD tgtx, DWORD tgty, DWORD tgtw, DWORD tgth,
		                       SURFHANDLE src, DWORD srcx, DWORD srcy, DWORD srcw, DWORD srch, DWORD flag = 0) const;


	bool clbkCopyBitmap (SURFHANDLE pdds, HBITMAP hbm, int x, int y, int dx, int dy);

	/**
	 * \brief Fills a surface with a uniform colour
	 * \param surf surface handle
	 * \param col fill colour value
	 * \return true on success, false if the fill operation cannot be performed.
	 * \sa clbkGetDeviceColour
	 */
	bool clbkFillSurface (SURFHANDLE surf, DWORD col) const;

	/**
	 * \brief Fills an area in a surface with a uniform colour
	 * \param surf surface handle
	 * \param tgtx left edge of target rectangle
	 * \param tgty top edge of target rectangle
	 * \param w width of rectangle
	 * \param h height of rectangle
	 * \param col colour value
	 * \return true
	 */
	bool clbkFillSurface (SURFHANDLE surf, DWORD tgtx, DWORD tgty, DWORD w, DWORD h, DWORD col) const;
	// @}

	// ==================================================================
	/// \name GDI-related methods
	// @{

	/**
	 * \brief Returns a Windows graphics device interface handle for a surface
	 * \param surf surface handle
	 * \return GDI handle
	 */
	HDC clbkGetSurfaceDC (SURFHANDLE surf);

	/**
	 * \brief Release a Windows graphics device interface
	 * \param surf surface handle
	 * \param hDC GDI handle
	 */
	void clbkReleaseSurfaceDC (SURFHANDLE surf, HDC hDC);
	// @}


	// ==================================================================
	/// \name 2D Drawing API
	// @{
	oapi::Sketchpad * clbkGetSketchpad(SURFHANDLE surf);
	void			  clbkReleaseSketchpad(oapi::Sketchpad *sp);
	Font *			  clbkCreateFont(int height, bool prop, const char *face, Font::Style style, int orientation) const;
	void			  clbkReleaseFont(Font *font) const;
	Pen *			  clbkCreatePen(int style, int width, DWORD col) const;
	void			  clbkReleasePen(Pen *pen) const;
	Brush *			  clbkCreateBrush(DWORD col) const;
	void			  clbkReleaseBrush(Brush *brush) const;
	// @}

	void WriteLog (const char *msg) const;


	LPD3D9CLIENTSURFACE GetDefaultTexture() const;
	LPD3D9CLIENTSURFACE GetBackBufferHandle() const;
	void				RegisterDissolveMap(SURFHANDLE hSrf);
	SURFHANDLE			GetDissolveMap(DWORD idx) const;
	int					GetIndexOfDissolveMap(SURFHANDLE hSrf) const;
	

	void EmergencyShutdown();
	void SplashScreen();
	void VisualsCreated();	

	void SetLabel(const char *txt);
	void SetItem(const char *txt);

	inline bool IsRunning() { return bRunning; }


	const LPD3DXMATRIX GetIdentity() const { return (const LPD3DXMATRIX)&ident; }

	HWND GetWindow();
	bool HasVertexTextureSupport() { return bVertexTex; }
	D3DCAPS9 *GetHardwareCaps() { return &caps; }
	D3D9Stat *GetStats() { return &stats; }
	FileParser *GetFileParser() { return parser; }
	LPDIRECT3DSURFACE9  GetEnvDepthStencil() { return pEnvDS; }
	LPDIRECT3DSURFACE9  GetShadowMapDepthStencil() { return pShmDS; }
	LPDIRECT3DTEXTURE9  GetShadowMapRenderTarget() { return pShmRT; }
	LPDIRECT3DSURFACE9	GetBackBuffer() { return pBackBuffer; }

	// overwritten
	const void *GetConfigParam (DWORD paramtype) const;
	const char *GetSkinFileLine(DWORD idx) const;

protected:
	/**
	 * \brief Graphics client initialisation
	 *
	 *   - Enumerates devices and selects one.
	 *   - Creates a CD3DFramework9 instance
	 *   - Creates a VideoTab instance
	 *
	 * \return true on success
	 */
	bool clbkInitialise ();

	/**
	 * \brief Start of simulation session
	 *
	 * - Calls parent class method to create the render window
	 * - Calls Initialise3DEnvironment to set up the environment
	 */
	HWND clbkCreateRenderWindow ();

	bool clbkUseLaunchpadVideoTab () const;

	/// \brief Finalise session creation
	///
	/// - Initialises the scene
	void clbkPostCreation ();

	/**
	 * \brief Cleanup of visual components before scenario
	 *   environment is destroyed.
	 * \param fastclose flag for fast-shutdown (skip deallocations)
	 * \note Deletes all current visual objects.
	 * \sa clbkDestroyRenderWindow
	 */
	void clbkCloseSession (bool fastclose);

	/**
	 * \brief End of simulation session
	 *
	 * - Calls parent class method
	 * - Calls Cleanup3DEnvironment to clean device objects
	 * \sa clbkCloseSession
	 */
	void clbkDestroyRenderWindow (bool fastclose);

	/**
	 * \brief Per-frame render call
	 *
	 * - Renders the scene into the back buffer
	 * - Flips the back buffer into view
	 */
	void clbkRenderScene();

	void clbkUpdate(bool running);
	/**
	 * \brief React to a discontinous jump in simulation time.
	 * \param simt new simulation time relative to session start [s]
	 * \param simdt jump interval [s]
	 * \param mjd new absolute simulation time in MJD format [days]
	 * \note Currently, this method does nothing.
	 */
	void clbkTimeJump (double simt, double simdt, double mjd);

	/**
	 * \brief Display rendered scene
	 * \return true
	 */
	bool clbkDisplayFrame ();

	/**
	 * \brief Store preloaded meshes persistently in device-specific format
	 * \param hMesh mesh handle
	 * \param fname mesh file name
	 */
	void clbkStoreMeshPersistent (MESHHANDLE hMesh, const char *fname);

	/**
	 * \brief Renders the fullscreen viewport in the presence of popup windows.
	 * \return \e true if render window has been updated and no more page flipping
	 *   is required, \e false if page still needs to be flipped.
	 */
	bool RenderWithPopupWindows ();

	/// \brief Output 2D graphics on top of the render window
	///
	/// Obtains the GDI of the render surface to output 2D data after
	/// rendering the 3D scene (glass cockpit, date info, etc.)
	void Output2DOverlay();

private:

	void Label(const char *format, ...);
	bool CheckBltGroup(SURFHANDLE src, SURFHANDLE tgt) const;
	void LogRenderParams () const;

	mutable SURFHANDLE		pBltGrpTgt;

	SURFHANDLE				pDislMapList[16];
    LPDIRECT3DDEVICE9		pd3dDevice;
	LPD3D9CLIENTSURFACE	    pDefaultTex;
	LPD3D9CLIENTSURFACE	    pScatterTest;
	LPDIRECT3DSURFACE9		pSplashScreen;
	LPDIRECT3DSURFACE9		pTextScreen;
	LPDIRECT3DSURFACE9		pBackBuffer;
	LPDIRECT3DSURFACE9		pEnvDS;
	LPDIRECT3DSURFACE9		pShmDS;
	LPDIRECT3DTEXTURE9		pShmRT;
	CD3DFramework9*		    pFramework;
	D3DCAPS9				caps;
	D3D9Stat				stats;
	FileParser *		    parser;
	char					ScenarioName[320];

	HWND hRenderWnd;        // render window handle

	bool bControlPanel;
	bool bScatterUpdate;
	bool bSkpGDI;
	bool bFullscreen;       // fullscreen render mode flag
	bool bGDIBB;		
	bool bAAEnabled;	
	bool bFailed;
	bool bScene;
	bool bRunning;
	bool bHalt;
	bool bVertexTex;
	bool bVSync;

	DWORD viewW, viewH;     // dimensions of the render viewport
	DWORD viewBPP;          // bit depth of render viewport
	DWORD ShaderModel;
	DWORD iDisl;
	DWORD nSkins;
	DWORD frame_timer;
	
	// device enumeration callback function

	VideoTab *vtab;			// video selection user interface
	Scene *scene;           // Scene description

	MeshManager *meshmgr;   // mesh manager
	TextureManager *texmgr; // texture manager
	D3DXMATRIX ident;

	LaunchpadItem *lpiCfg, *lpiPlanetRender;

	HFONT hLblFont1;
	HFONT hLblFont2;

	char pOrbiterRoot[256];
	char pLoadLabel[128];
	char pLoadItem[128];
	LPCHAR *pSkinNames;
	char *pSkinBuffer;

	// Control Panel
	
	void Item(WORD id, const char *label, const char *format, ...);
	void RenderControlPanel();
	bool ControlPanelMsg(WPARAM wParam);

	Sketchpad *pItemsSkp;

	DWORD loadd_x, loadd_y, loadd_w, loadd_h;

	WORD ItemList[256];
	WORD Items;
	WORD SelectedItem;
	WORD LabelPos;

}; // class D3D9Client


// ======================================================================
// class VisObject
// ======================================================================
/**
 * \brief Visual object representation.
 *
 * A VisObject is the visual representation of an Orbiter object (vessel,
 * planet, etc.). The 'logical' object representation resides in the Orbiter
 * core, while its 'visual' representation is located in the graphics client.
 *
 * Visual representations should be non-permanent: they should be created
 * when the object enters the visual range of the camera, and deleted when
 * they leave it.
 *
 * Only a single VisObject instance should be created per object, even if
 * the visual is present in multiple views. If the graphics client supports
 * multiple views, the view-specific parameters (e.g. visibility flags) should
 * be implemented by the client, e.g. by deriving a class from VisObject that
 * holds an array of the view-specific data. In that case, the VisObject should
 * be created when the object becomes visible in any one view, and destroyed
 * when the object disappears from the last view.
 */
class VisObject {
public:
	/**
	 * \brief Creates a visual for object hObj.
	 * \param hObj object handle
	 * \sa oapi::GraphicsClient::RegisterVisObject
	 */
	VisObject (OBJHANDLE hObj);

	/**
	 * \brief Destroys the visual.
	 * \sa oapi::GraphicsClient::UnregisterVisObject
	 */
	virtual ~VisObject ();

	/**
	 * \brief Returns the object handle associated with the visual.
	 * \return Object handle
	 */
	OBJHANDLE GetObject () const { return hObject; }

	/**
	 * \brief Message callback.
	 * \param event message identifier
	 * \param context message content (message-specific)
	 * \default None.
	 * \note This method is called by the Orbiter core to notify the visual
	 *   of certain events (e.g. adding and deleting meshes)
	 * \note For currently supported event types, see \ref visevent.
	 */
	virtual void clbkEvent (DWORD event, UINT context) {}

protected:
	OBJHANDLE hObject;	///< Object handle associated with the visual
};

}; // namespace oapi

#endif // !__D3D9CLIENT_H