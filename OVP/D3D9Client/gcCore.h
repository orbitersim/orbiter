// ===================================================
// Copyright (C) 2012-2021 Jarmo Nikkanen
// licensed under LGPL v2
// ===================================================


#include "OrbiterAPI.h"
#include "DrawAPI.h"
#include <assert.h>

#ifndef __GC_CORE
#define __GC_CORE

#define INTERFACE_BUILDER
#define gc_interface static

using namespace oapi;

class gcCore;
class gcCore2;

typedef void (__cdecl* __gcBindCoreMethod)(void** ppFnc, const char* name);

static class gcCore2 *pCoreInterface = NULL;

/**
* \file gcConst.h
* \brief Structures and definitions
*/


/// \defgroup PixelFormats Common pixel formats for surfaces [ Orbiter 2021+ only ]
///@{
#define OAPISURFACE_PF_MASK				0xFF0000	///< PixelFormat Mask
#define OAPISURFACE_PF_XRGB				0x010000	///< 32bit RGB no-alpha
#define OAPISURFACE_PF_ARGB				0x020000	///< 32bit ARGB with-alpha	0xAARRGGBB
#define OAPISURFACE_PF_RGB565			0x030000	///< 16bit RGB no-alpha
#define OAPISURFACE_PF_S16R				0x040000	///< Signed integer 16-bit (1-channel)
#define OAPISURFACE_PF_F32R				0x050000	///< Float 32-bit (1-channel)
#define OAPISURFACE_PF_F32RG			0x060000	///< Float 64-bit (2-channel)
#define OAPISURFACE_PF_F32RGBA			0x070000	///< Float 128-bit (4-channel) float4(r,g,b,a)
#define OAPISURFACE_PF_F16R				0x080000	///< Float 16-bit (1-channel) 
#define OAPISURFACE_PF_F16RG			0x090000	///< Float 32-bit (2-channel) 
#define OAPISURFACE_PF_F16RGBA			0x0A0000	///< Float 64-bit (4-channel) float4(r,g,b,a)
#define OAPISURFACE_PF_DXT1				0x0B0000	///< Compressed DXT1 format
#define OAPISURFACE_PF_DXT3				0x0C0000	///< Compressed DXT3 format
#define OAPISURFACE_PF_DXT5				0x0D0000	///< Compressed DXT5 format
#define OAPISURFACE_PF_ALPHA			0x0E0000	///< Alpha only surface 8-bit
#define OAPISURFACE_PF_GRAY				0x0F0000	///< Grayscale Image 8-bit
///@}


/// \defgroup RenderProc Specify a SketchPad render callback function
///@{
#define RENDERPROC_DELETE				0x0000	///< Unregister/Remove existing callback 
#define RENDERPROC_HUD_1ST				0x0001	///< Register a HUD callback to draw under Orbiter's main HUD
#define RENDERPROC_HUD_2ND				0x0002	///< Register a HUD callback to draw over Orbiter's main HUD
#define RENDERPROC_PLANETARIUM			0x0003	///< Register a HUD callback to draw into a planetarium view using perspective projection
#define RENDERPROC_EXTERIOR				0x0005  ///< Register a callback to draw into an exterior vessel view using perspective projection
///@}


/// \defgroup GenericProc Specify a generic callback function
///@{
#define GENERICPROC_DELETE				0x0000	///< Unregister/Remove existing callback 
#define GENERICPROC_PICK_VESSEL			0x0001	///< Called when user clicks a vessel with LMB, RMB
#define GENERICPROC_PICK_TERRAIN		0x0002	///< Called when user clicks a terrain with LMB, RMB
#define GENERICPROC_HOVER_TERRAIN		0x0003	///< Called when hovering over terrain with mouse (Performance heavy AVOID !!)
#define GENERICPROC_SHUTDOWN			0x0004	///< Callback for resource/memory deallocation
#define GENERICPROC_RENDERTILE			0x0010	///< Render Tile Callback
#define GENERICPROC_RENDER_EXTERIOR		0x0011	///< Render Post Scene Exterior Only Callback
#define GENERICPROC_TILE_CREATED		0x0020	///< 
#define GENERICPROC_TILE_DELETED		0x0021	///< 
///@}

#define IPF_WRAP		0x0000
#define IPF_WRAP_U		0x0000
#define IPF_WRAP_V		0x0000
#define IPF_WRAP_W		0x0000
#define IPF_CLAMP		0x0007
#define IPF_CLAMP_U		0x0001
#define IPF_CLAMP_V		0x0002
#define IPF_CLAMP_W		0x0004
#define IPF_MIRROR		0x0038
#define IPF_MIRROR_U	0x0008
#define IPF_MIRROR_V	0x0010
#define IPF_MIRROR_W	0x0020
#define IPF_POINT		0x0000
#define IPF_LINEAR		0x0040
#define IPF_PYRAMIDAL	0x0080
#define IPF_GAUSSIAN	0x0100
#define IPF_ANISOTROPIC	0x0200
#define IPF_VERTEXTEX	0x0400

/// \defgroup dwFlags for gcSetupCustomCamera() API function
///@{
#define CUSTOMCAM_DEFAULTS				0x00FF
///@}

/// \defgroup Polyline Polyline object creation and update flags
///@{
#define	PF_CONNECT		0x01	///< Connect line endpoints forming a loop
///@}

/// \defgroup Triangle Triangle object creation and update flags
///@{
#define	PF_TRIANGLES	0
#define	PF_STRIP		1
#define	PF_FAN			2
///@}


/// \brief Handle to a surface manager's global overlay
typedef void * HOVERLAY;
/// \brief Handle to a planet/surface manager
typedef void * HPLANETMGR;
/// \brief Handle to a surface tile (SurfTile)
typedef void * HTILE;
/// \brief Custom swapchain handle
typedef void * HSWAP;
/// \brief Custom camera handle
typedef void * CAMERAHANDLE;
/// \brief Poly object handle
typedef void * HPOLY;


namespace gcTileFlags
{
	static const int TEXTURE = 0x1;			///< Texture data
	static const int MASK = 0x2;			///< Nightlights/Water mask
	static const int ELEVATION = 0x4;		///< Elevation
	static const int CLOUD = 0x8;			///< Texture data
	static const int ELEV_MOD = 0x10;		///< Elevation
	// ------------------------------------------------------------------
	static const int TREE = 0x1000;			///< Search/Use Tree Archive
	static const int CACHE = 0x2000;		///< Search/Use Cache
	static const int MOD = 0x4000;			///< Search/Use ElevMod Cache
};



/// \brief Render HUD and Planetarium callback function 
typedef void(__cdecl *__gcRenderProc)(oapi::Sketchpad *pSkp, void *pParam);
typedef void(__cdecl *__gcGenericProc)(int iUser, void *pUser, void *pParam);




// ===========================================================================
/**
* \class gcIPInterface
* \brief Image Processing Interface, Feed data through user supplied shader.
*/
// ===========================================================================

class gcIPInterface 
{

public:

	friend class gcCore2;

	enum ipitemplate { Rect, Octagon, Mesh };
	enum ipicull { None, CCW, CW };

	gcIPInterface(class ImageProcessing* p) : pIPI(p) { }

	virtual ~gcIPInterface();

	// ----------------------------------------------------------------------------------
	// Compile and Activate Additional Shaders, Each shader has it's own local set of variables. 
	// Variables are not shared between shaders 
	// ----------------------------------------------------------------------------------
	virtual bool	CompileShader(const char* PSEntry);
	virtual bool	Activate(const char* Shader = NULL);

	// ----------------------------------------------------------------------------------
	// Use the 'Set' functions to assign a value into a shader constants ( e.g. uniform extern float4 myVector; )
	// If the variable "var" is defined but NOT used by the shader code, the variable "var" doesn't exists in
	// a constant table and an error is printed when trying to assign a value to it.
	// ----------------------------------------------------------------------------------
	virtual void	SetFloat(const char* var, float val);
	virtual void	SetInt(const char* var, int val);
	virtual void	SetBool(const char* var, bool val);
	// ----------------------------------------------------------------------------------
	virtual void	SetFloat(const char* var, const void* val, int bytes);
	virtual void	SetInt(const char* var, const int* val, int bytes);
	virtual void	SetBool(const char* var, const bool* val, int bytes);
	virtual void	SetStruct(const char* var, const void* val, int bytes);

	// ----------------------------------------------------------------------------------
	// Use the 'SetVS' functions to assign a value into a vertex shader constants ( e.g. uniform extern float4 myVector; )
	// If the variable "var" is defined but NOT used by the shader code, the variable "var" doesn't exists in
	// a constant table and an error is printed when trying to assign a value to it.
	// ----------------------------------------------------------------------------------
	virtual void	SetVSFloat(const char* var, float val);
	virtual void	SetVSInt(const char* var, int val);
	virtual void	SetVSBool(const char* var, bool val);
	// ----------------------------------------------------------------------------------
	virtual void	SetVSFloat(const char* var, const void* val, int bytes);
	virtual void	SetVSInt(const char* var, const int* val, int bytes);
	virtual void	SetVSBool(const char* var, const bool* val, int bytes);
	virtual void	SetVSStruct(const char* var, const void* val, int bytes);

	// ----------------------------------------------------------------------------------
	// SetTexture can be used to assign a taxture and a sampler state flags to a sampler
	// In a shader code sampler is defined as (e.g. sampler mySamp; ) where "mySamp" is
	// the variable passed to SetTexture function. It's then used in a shader code like
	// tex2D(mySamp, float2(x,y))
	// ----------------------------------------------------------------------------------
	virtual void	SetTexture(const char* var, SURFHANDLE hTex, DWORD flags);

	// ----------------------------------------------------------------------------------
	// SetOutput assigns a render target to the IP interface. "id" is an index of the render
	// target with a maximum value of 3. It is possible to render in four different targets
	// at the same time. Multisample AA is only supported with one render target. Unbound
	// a render target by setting it to NULL. After a NULL render target all later targets
	// are ignored.
	// ----------------------------------------------------------------------------------
	virtual void	SetOutput(int id, SURFHANDLE hSrf);

	// ----------------------------------------------------------------------------------
	// Set output rect is need to render into a specific subsection
	// ----------------------------------------------------------------------------------
	virtual void	SetOutputRegion(float w = 1.0f, float h = 1.0f, float x = 0.0f, float y = 0.0f);

	// ----------------------------------------------------------------------------------
	// Check that everything is Initialized OK and Execute the shader
	// ----------------------------------------------------------------------------------
	virtual bool	IsOK();
	virtual void	SetMesh(const MESHHANDLE hMesh, const char* tex = NULL, ipicull = ipicull::None);

	virtual bool	Execute(bool bInScene = false);
	virtual bool	Execute(DWORD blendop, bool bInScene = false, ipitemplate mde = Rect);

	// ----------------------------------------------------------------------------------
	virtual int		FindDefine(const char* key);

private:

	class ImageProcessing* pIPI;
};





// ===========================================================================
/**
* \class gcCore
* \brief Core class for graphics services 
*/
// ===========================================================================


INTERFACE_BUILDER class gcCore
{	
public:

	enum class Tile
	{
		TEXTURE = 0x0,		///< Texture data
		MASK = 0x2,			///< Nightlights/Water mask
		ELEVATION = 0x3,	///< Elevation
		CLOUD = 0x4,		///< Texture data
		ELEV_MOD = 0x5,		///< Elevation
		// ------------------------------------------------------------------
		TREE = 0x10,		///< Search/Use Tree Archive
		CACHE = 0x20,		///< Search/Use Cache
		MOD = 0x40			///< Search/Use ElevMod Cache
	};


	enum class MatrixId
	{
		OFFSET = 1,			///< Set/Get Mesh offset matrix, Also used by VESSEL::ShiftMesh()
		MESH = 2,			///< Set/Get Mesh animation matrix, Transforms all the groups in the mesh
		GROUP = 3,			///< Set/Get Group animation matrix, Transforms a single group
		COMBINED = 4		///< Get combined Mesh*Group*Offset matrix. (Can't 'set' this)
	};

	enum class OlayType
	{
		RELEASE_ALL = -1,
		SURFACE = 0,
		MASK = 1,
		ELEVATION = 2
	};

	typedef struct {
		void* pData;
		DWORD			Pitch;
	} Lock;

	typedef struct {
		double			lng, lat;		///< Longitude and Latitude of the point being clicked
		double			elev;			///< Elevation of the point being clicked above mean radius
		double			dist;			///< Distance from a camera to a click point
		DRECT			Bounds;			///< Tile bounds (i.e. min/max * lng/lat)
		FVECTOR3		normal;			///< Normal in ecliptic frame
		FVECTOR3		pos;			///< Position from a camera in ecliptic frame
		float			emax;			///< Max elevation within the tile
		float			emin;			///< Min elevation within the tile
		UINT			msg;			///< Zero or (WM_LBUTTONDOWN, WM_RBUTTONDOWN, WM_LBUTTONUP, WM_RBUTTONUP) or (WM_MOUSEMOVE, WM_MOUSEWHEEL)
		int				level;			///< Tile level
		int				iLng, iLat;		///< Tile Index
		HTILE			hTile;			///< Tile handle being clicked. WARNING: Tile returned by this data entry can become invalid without notice.
	} PickGround;


	typedef struct {
		WORD			Size;			///< sizeof(ElevInfo)
		WORD			Format;
		float*			pElevData;
		double			Resolution;
		double			Offset;
		double			MinElev;
		double			MaxElev;
		double			MeanElev;
	} ElevInfo;


	typedef struct {
		int				Width;
		int				Height;
		int				Mips;
		DWORD			Flags;
	} SurfaceSpecs;


	typedef struct {
		OBJHANDLE		hVessel;		///< Handle to a vessel that was clicked
		MESHHANDLE		mesh;			///< Mesh index that was clicked
		int				group;			///< Mesh group index that was clicked
		float			dist;			///< Distance from a camera to a click point
		FVECTOR3		normal;			///< Normal vector in local vessel coordinates
		FVECTOR3		pos;			///< Position in local vessel coordinates
	} PickData;


	typedef struct {
		int				iUser;			///< Unused variable for user's own purposes.
		int				grp_inst;		///< Mesh group index or instance number that was clicked
		float			dist;			///< Distance from a camera to a click point
		FVECTOR3		normal;			///< Normal vector in local vessel coordinates
		FVECTOR3		pos;			///< Position in local vessel coordinates
	} PickMeshStruct;


	typedef struct {
		int				MaxTexSize;		///< Maximum texture size in pixels
		int				DisplayMode;	///< 0 = True Fullscreen, 1 = Fullscreen Window, 2 = Windowed
		int				MaxTexRep;		///< Maximum texture repeat count
		DWORD			gcAPIVer;		///< gcAPI Build Date 0xYYYYMMDD
	} SystemSpecs;


	typedef struct {
		FVECTOR2 pos;
		DWORD color;
	} clrVtx;


	// ===========================================================================
	/// \name Custom swap-chain management functions
	// ===========================================================================
	//@{
	/**
	* \brief Create a new custom swap-chain (i.e. Frontbufer/Backbuffer) for a user defined window.
	* \param hWnd Handle to a window client rect
	* \param hSwap Handle to an existing swap object to resize it.
	* \param AA Level of requested anti-aliasing. Valid values are 0, 2, 4, 8
	* \return Handle to a Swap object or NULL in a case of an error
	*/
	gc_interface HSWAP RegisterSwap(HWND hWnd, HSWAP hSwap = NULL, int AA = 0);
	
	/**
	* \brief Flip backbuffer to a front
	* \param hSwap Handle to a swap object.
	*/
	gc_interface void FlipSwap(HSWAP hSwap);

	/**
	* \brief Get a backbuffer surface
	* \param hSwap Handle to a swap object.
	* \return A Handle to a rendering surface (i.e. backbuffer)
	*/
	gc_interface SURFHANDLE GetRenderTarget(HSWAP hSwap);

	/**
	* \brief Release a swap object after it's no longer needed.
	* \param hSwap Handle to a swap object.
	*/
	gc_interface void ReleaseSwap(HSWAP hSwap);
	//@}




	// ===========================================================================
	/// \name Custom Camera Interface
	// ===========================================================================
	//@{
	/**
	* \brief Delete/Release a custom camera.
	* \param hCam camera handle to delete.
	* \return zero or an error code if the camara didn't work properly.
	* \note Always delete all cameras bound to a render surface before releasing the rendering surface it-self.
	*/
	gc_interface int DeleteCustomCamera(CAMERAHANDLE hCam);

	/**
	* \brief Toggle camera on and off
	* \param hCam camera handle to toggle
	* \param bOn true to turn on the camera.
	* \note If multiple cameras are sharing the same rendering surface. Flickering will occur if more than one camera is turned on.
	*/
	gc_interface void CustomCameraOnOff(CAMERAHANDLE hCam, bool bOn);

	/**
	* \brief Setup a custom camera overlay drawing callback
	* \param hCam camera handle to toggle
	* \param clbk pointer to a function to be called after each frame.
	*/
	gc_interface void CustomCameraOverlay(CAMERAHANDLE hCam, __gcRenderProc clbk, void* pUser);

	/**
	* \brief Create a new custom camera that can be used to render views into a surfaces and textures
	* \param hCam camera handle to modify an existing camera or, NULL
	* \param hVessel handle to a vessel where the camera is attached to.
	* \param vPos camera position in vessel's local coordinate system
	* \param vDir camera direction in vessel's local coordinate system. [Unit Vector]
	* \param vUp camera up vector. Must be perpendicular to vDir. [Unit Vector]
	* \param dFow camera field of view in radians
	* \param hSurf rendering surface. Must be created at least with OAPISURFACE_RENDER3D | OAPISURFACE_RENDERTARGET. Multiple cameras can share the same surface.
	* \param dwFlags Flags to controls what is drawn and what is not.
	* \return Camera handle, or NULL if an error occurred or if the custom camera interface is disabled.
	* \note Camera count is unlimited.
	* \note Only a cameras attached to currently active vessel are operational and recording.
	* \note Having multiple cameras active at the same time doesn't impact in a frame rate, however, camera refresh rates are reduced.
	*/
	gc_interface CAMERAHANDLE SetupCustomCamera(CAMERAHANDLE hCam, OBJHANDLE hVessel, VECTOR3& vPos, VECTOR3& vDir, VECTOR3& vUp, double dFov, SURFHANDLE hSurf, DWORD dwFlags = 0xFF);
	//@}




	// ===========================================================================
	/// \name Sketchpad related functions
	// ===========================================================================
	//@{
	/**
	* \brief Get the sketchpad version
	* \param pSkp handle to a sketchpad interface.
	* \return Currently returns 2 or (1 in some very special cases). 
	*/
	gc_interface int SketchpadVersion(Sketchpad *pSkp);


	/**
	* \brief Create or Update a polyline composed form piecewise straight segments.
	* \param hPoly Handle to a polyline to be updated or NULL to create a new one.
	* \param pt list of vertex points.
	* \param npt number of points in the list.
	* \param flags additional PolyFlags flags
	* \sa gcDeletePoly, Sketchpad2::DrawPoly()
	* \note Poly objects should be created during initialization not for every frame or update. Updating existing (pre created) poly object is pretty fast.
	* \note During update number of points must be equal or smaller than during initial creation of poly object.
	*/
	gc_interface HPOLY CreatePoly(HPOLY hPoly, const oapi::FVECTOR2* pt, int npt, DWORD flags = 0);


	/**
	* \brief Create or Update a triangle object.
	* \param hPoly Handle to a triangle to be updated or NULL to create a new one.
	* \param pt list of vertex points.
	* \param npt number of points in the list.
	* \param flags additional flags (see below)
	* \sa gcDeletePoly, Sketchpad2::DrawPoly()
	* \note Poly objects should be created during initialization not for every frame or update. Updating existing (pre created) poly object is pretty fast.
	* \note During update number of points must be equal or smaller than during initial creation of poly object.
	* \note Flags:
	* \note PF_TRIANGLES Each independent triangle is composed from three vertex points. ("npt" must be multiple of 3)
	* \note PF_FAN Triangle fan. The first vertex is in the center of the fan/circle and other lie at the edge. ("npt" must be "number of triangles" + 2)
	* \note PF_STRIP Is build from quads. Where each quad requires two vertices. ("npt" must be "number of quads" * 2 + 2)
	*/
	gc_interface HPOLY CreateTriangles(HPOLY hPoly, const clrVtx* pt, int npt, DWORD flags);


	/**
	* \brief Deletes a polyline created with gcCreatePolyPolyline()
	* \param hPoly Handle to a polyline to be deleted
	* \sa gcCreatePolyline
	*/
	gc_interface void DeletePoly(HPOLY hPoly);


	/**
	* \brief Compute a length of a text string
	* \param hFont a Pointer into a font
	* \param pText a Pointer into a text string
	* \param len a Length of the text string to process. -1 will scan to a NULL terminator.
	*/
	gc_interface DWORD GetTextLength(oapi::Font* hFont, const char* pText, int len = -1);


	/**
	* \brief Find index of nearest "cap" between charters in specified location. (i.e. distance from start of the string in pixels)
	* \param hFont a Pointer into a font
	* \param pText a Pointer into a text line
	* \param pos a Position in pixels from start of the string
	* \param len a Length of the text line to process. -1 will process to a NULL terminator.
	* \return index from 0 to number of charters. For just one char it can be either "0" or "1" depending which side is closer to "pos".
	* \note This is used for finding a spot for a "cursor" when a text string is clicked with mouse.
	*/
	gc_interface DWORD GetCharIndexByPosition(oapi::Font* hFont, const char* pText, int pos, int len = -1);


	/**
	* \brief This function will register a custom render callback function
	* \param proc function to be called when render event occur
	* \param id render event id
	* \param pParam a pointer to user data (to a class for an example)
	* \return false if an error occurred, true otherwise.
	*/
	gc_interface bool RegisterRenderProc(__gcRenderProc proc, DWORD id, void* pParam);

	/**
	* \brief Create a Font
	* \param height Font height
	* \param face Name of the font
	* \param width Width of the font (0 for default aspect ration)
	* \param weight Font thickness (400 for default weight)
	* \param style A combination of \see gcFont flags (0 for default)
	* \param spacing A spacing between charters in a string (0.0f for default)
	* \return A pointer to a created or pre-existing font or NULL in a case of an error.
	*/
	gc_interface oapi::Font *CreateSketchpadFont(int height, char *face, int width = 0, int weight = 400, FontStyle Style = FontStyle::FONT_NORMAL, float spacing = 0.0f);
	//@}





	// ===========================================================================
	/// \name Mesh interface functions
	// ===========================================================================
	//@{
	/**
	* \brief This function will register a custom render callback function
	* \param hMesh Handle to a devmesh containing the material
	* \param idx Material index
	* \param prop material property identifier (\ref MeshMaterialFlags)
	* \param value a pointer to COLOUR4 structure containing/receiving the data, or \e NULL to reset a default value or to unspecify a property.
	* \param bSet \e true to set material value, \e false to get a material value
	* \return -4 = Invalid handle \n -3 = Unknown property flag \n -2 = Property not specified cannot get it \n -1 = Index out of range \n 0 = Success
	*/
	gc_interface int GetMeshMaterial(DEVMESHHANDLE hMesh, DWORD idx, MatProp prop, FVECTOR4 *value);

	gc_interface int SetMeshMaterial(DEVMESHHANDLE hMesh, DWORD idx, MatProp prop, const FVECTOR4* value);

	/**
	* \brief A Function to get a mesh transformation/animation matrix.
	* \param matrix_id Id of the matrix to get. One of MatrixId::xxx datatypes.
	* \param hVessel Vessel object handle.
	* \param mesh Mesh index
	* \param group Group index
	* \param pMat A pointer to FMATRIX4 struct for receiving the data.
	* \return 0 = on Success, or error code.
	*/
	gc_interface int GetMatrix(MatrixId mat, OBJHANDLE hVessel, DWORD mesh, DWORD group, oapi::FMATRIX4* pMat);


	/**
	* \brief A Function to set a mesh transformation/animation matrix. Do not use this function for animated parts/meshes.
	* \param matrix_id Id of the matrix to set. One of MatrixId::xxx datatypes.
	* \param hVessel Vessel object handle.
	* \param mesh Mesh index
	* \param group Group index
	* \param pMat A pointer to FMATRIX4 containing the data to set.
	* \return 0 = on Success, or error code.
	*/
	gc_interface int SetMatrix(MatrixId mat, OBJHANDLE hVessel, DWORD mesh, DWORD group, const oapi::FMATRIX4* pMat);


	/**
	* \brief Get device specific mesh from Orbiter mesh template
	* \param hMesh handle to a mesh acquired from oapiLoadMeshGlobal()
	* \param pBox a pointer to an array of 8 FVECTOR3s
	*/
	gc_interface DEVMESHHANDLE GetDevMesh(MESHHANDLE hMesh);

	gc_interface DEVMESHHANDLE LoadDevMeshGlobal(const char* file_name, bool bUseCache = true);

	gc_interface void ReleaseDevMesh(DEVMESHHANDLE hMesh);


	/**
	* \brief Recover tile bounding box data
	* \param hTile handle to a tile
	* \param pBox a pointer to an array of 8 FVECTOR3s
	*/
	gc_interface void RenderMesh(DEVMESHHANDLE hMesh, const FMATRIX4* pWorld);

	gc_interface bool PickMesh(PickMeshStruct* pm, DEVMESHHANDLE hMesh, const FMATRIX4* pWorld, short x, short y);
	//@}




	// ===========================================================================
	/// \name Some Helper Functions
	// ===========================================================================
	//@{
	/**
	* \brief Render a list of independent lines 0-1, 2-3,...
	* \param pVtx a pointer to a vertex array
	* \param pIdx a pointer to index array
	* \param nIdx number of lines to draw multiplied by 2, (ARRAYSIZE() of index array)
	* \param pWorld pointer to World matrix relative to camera
	* \param color color in 0xAABBGGRR
	*/
	gc_interface void RenderLines(const FVECTOR3* pVtx, const WORD* pIdx, int nVtx, int nIdx, const FMATRIX4* pWorld, DWORD color);

	/**
	* \brief Get some system information
	* \param sp A pointer to SystemSpecs struct
	* \param size sizeof(SystemSpecs)
	*/
	gc_interface void GetSystemSpecs(SystemSpecs* sp, int size);

	/**
	* \brief Get some system information
	* \param sp A pointer to SystemSpecs struct
	* \param size sizeof(SystemSpecs)
	*/
	gc_interface bool GetSurfaceSpecs(SURFHANDLE hSrf, SurfaceSpecs *sp, int size);

	/**
	* \brief Load a texture into a specific type of a surface
	* \param fname name of a texture to be loaded.
	* \param flags surface attributes (see: OAPISURFACE_x flags)
	* \return surface handle or NULL in a case of an error
	*/
	gc_interface SURFHANDLE LoadSurface(const char *fname, DWORD flags);

	/**
	* \brief Save a native DirectX surface to a file (*.dds, *.jpg, *.bmp, *.png)
	* \param file filename.
	* \param hSrf handle to a surface to same.
	* \return false in a case of failure.
	*/
	gc_interface bool SaveSurface(const char* file, SURFHANDLE hSrf);

	/**
	* \brief Get a handle to a specific mipmap sub-level
	* \param hSrf Handle to a RenderTarget Texture containing mipmaps.
	* \param level Level of the mipmap to acquire. (level >= 1) (0 = "hSrf" it self with surface interface)
	* \return Surface handle to a render-target or NULL in a case of a failure. Must be released with after no longer accessed.
	*/
	gc_interface SURFHANDLE	GetMipSublevel(SURFHANDLE hSrf, int level);

	/**
	* \brief Realtime Mipmap auto-generation from the top/main level.
	* \param hSurface handle to a surface
	* \return false if an error occurred, true otherwise.
	* \note Surface must be created with (OAPISURFACE_TEXTURE | OAPISURFACE_RENDERTARGET | OAPISURFACE_MIPMAPS)
	* \note Exact attribute requirements/conflicts are unknown.
	*/
	gc_interface bool GenerateMipmaps(SURFHANDLE hSurface);

	/**
	* \brief On the fly texture compression into a DXT format. Input remains uncanged.
	* \param hSurface handle to a surface to compress
	* \param flags combination of OAPISURFACE_PF_DXT1, OAPISURFACE_PF_DXT3, OAPISURFACE_PF_DXT5, OAPISURFACE_MIPMAPS, OAPISURFACE_SYSMEM
	* \return Handle to a compressed texture, user must release this.
	* \note Compression is slow, separate thread recommended for realtime compression.
	*/
	gc_interface SURFHANDLE	CompressSurface(SURFHANDLE hSurface, DWORD flags);

	/**
	* \brief Load a bitmap from file (*.bmp *.png *.jpg *.gif)
	* \param fname name of the file to be loaded.
	* \return Bitmap handle of NULL in a case of an error
	*/
	gc_interface HBITMAP LoadBitmapFromFile(const char *fname);

	/**
	* \brief Get render window handle
	* \return Render window handle
	*/
	gc_interface HWND GetRenderWindow();

	/**
	* \brief Register generic callback function
	* \param proc function to be called when event occur
	* \param id requested callback event id
	* \param pParam a pointer to user data (to a class for an example)
	* \return false if an error occurred, true otherwise.
	*/
	gc_interface bool RegisterGenericProc(__gcGenericProc proc, DWORD id, void *pParam);

	/**
	* \brief Do not use these functions unless you know what's you doing.
	*/
	gc_interface bool StretchRectInScene(SURFHANDLE tgt, SURFHANDLE src, LPRECT tr = NULL, LPRECT sr = NULL);

	gc_interface bool ClearSurfaceInScene(SURFHANDLE tgt, DWORD color, LPRECT tr = NULL);


	/**
	* \brief Find a tile from specified coordinates.
	* \param scr_x screen space x-coordinate.
	* \param scr_y screen space y-coordinate.
	* \return PickGround data structure, all members are zero if ray doesn't intersect ground.
	*/
	gc_interface PickGround ScanScreen(int scr_x, int scr_y);

	/**
	* \brief Lock a surface created with OAPISURFACE_SYSMEM or OAPISURFACE_TEXTURE | OAPISURFACE_GDI flags.
	* \param hSrf Handle to a surface/texture to lock
	* \param pOut Data structure receiving pointer and row pitch/length
	* \param bWait if "true" will wait other threads to release lock on resource if already locked.
	* \return true if lock was gained, false otherwise
	*/
	gc_interface bool LockSurface(SURFHANDLE hSrf, Lock* pOut, bool bWait = false);
	gc_interface void ReleaseLock(SURFHANDLE hSrf);

	/**
	* \brief Convert a floating point color to DWORD color value
	* \param c A pointer to a color
	* \return DWORD color in 0xAABBGGRR
	* \note Alpha will range from 1 to 255. Zero is never returned because of backwards compatibility issues 0-alpha is mapped to 255
	*/
	inline DWORD Color(const COLOUR4* c)
	{
		return FVECTOR4(*c).dword_abgr();
	}

	/**
	* \brief Convert a floating point color to DWORD color value
	* \param c A pointer to a color
	* \return DWORD color in 0xAABBGGRR
	* \note Alpha will range from 1 to 255. Zero is never returned because of backwards compatibility issues 0-alpha is mapped to 255
	*/
	inline DWORD Color(const oapi::FVECTOR4* c)
	{
		return c->dword_abgr();
	}

	/**
	* \brief Convert a DWORD color to floating point COLOUR4 value
	* \param dwABGR A color in 0xAABBGGRR
	* \return COLOUR4
	* \note Alpha will range from 1 to 255. Zero is never used because of backwards compatibility issues 0-alpha is mapped to 255
	*/
	inline COLOUR4 Colour4(DWORD dwABGR)
	{
		return FVECTOR4(dwABGR).Colour4();
	}

	/**
	* \brief Alters objects position. Matrix must be initially valid.
	* \param mat [in/out] Pointer to a matrix to change
	* \param pos New position
	*/
	inline void SetTranslation(FMATRIX4* mat, const VECTOR3& pos)
	{
		mat->m41 = float(pos.x); mat->m42 = float(pos.y); mat->m43 = float(pos.z);
	}

	inline void SetTranslation(FMATRIX4* mat, const FVECTOR3& pos)
	{
		mat->m41 = pos.x; mat->m42 = pos.y; mat->m43 = pos.z;
	}

	/**
	* \brief Creates a world transformation matrix
	* \param mat [out] Pointer to a matrix
	* \param pos Objects position relative to a camera in ecliptic frame
	* \param x X-axis, direction [unit vector]
	* \param z Z-axis, direction [unit vector]
	* \param scale a scale factor (default 1.0)
	*/
	inline void WorldMatrix(FMATRIX4* mat, const VECTOR3& pos, const VECTOR3& x, const VECTOR3& z, double scale = 1.0)
	{
		VECTOR3 y = crossp(x, z);
		mat->m11 = float(x.x * scale); mat->m12 = float(x.y * scale); mat->m13 = float(x.z * scale); mat->m14 = 0.0f;
		mat->m21 = float(y.x * scale); mat->m22 = float(y.y * scale); mat->m23 = float(y.z * scale); mat->m24 = 0.0f;
		mat->m31 = float(z.x * scale); mat->m32 = float(z.y * scale); mat->m33 = float(z.z * scale); mat->m34 = 0.0f;
		mat->m41 = float(pos.x);	   mat->m42 = float(pos.y);		  mat->m43 = float(pos.z);		 mat->m44 = 1.0f;
	}

	inline void WorldMatrix(FMATRIX4* mat, const FVECTOR3& pos, const FVECTOR3& x, const FVECTOR3& z, float scale = 1.0f)
	{
		FVECTOR3 y = cross(x, z);
		mat->m11 = (x.x * scale); mat->m12 = (x.y * scale); mat->m13 = (x.z * scale); mat->m14 = 0.0f;
		mat->m21 = (y.x * scale); mat->m22 = (y.y * scale); mat->m23 = (y.z * scale); mat->m24 = 0.0f;
		mat->m31 = (z.x * scale); mat->m32 = (z.y * scale); mat->m33 = (z.z * scale); mat->m34 = 0.0f;
		mat->m41 = (pos.x);		  mat->m42 = (pos.y);		mat->m43 = (pos.z);		  mat->m44 = 1.0f;
	}
	//@}


	// ===========================================================================
	// Function pointer table
	// ===========================================================================
	//
#define fnc_typedefs
	// 
	// ===========================================================================
};



INTERFACE_BUILDER class gcCore2 : public gcCore
{

public:

	gcCore2(__gcBindCoreMethod pBindCoreMethod)
	{
#define fnc_binder
	}

	// ===========================================================================
	/// \name Planetary surface interface
	/// Graphics client maintains a tile database for a tiles used in rendering
	/// This API can't access tile data outside visual range
	// ===========================================================================
	//@{
	gc_interface HPLANETMGR GetPlanetManager(OBJHANDLE hPlanet);

	gc_interface SURFHANDLE SetTileOverlay(HTILE hTile, const SURFHANDLE hOverlay);

	gc_interface HOVERLAY AddGlobalOverlay(HPLANETMGR hMgr, VECTOR4 mmll, OlayType type, const SURFHANDLE hOverlay = NULL, HOVERLAY hOld = NULL, const FVECTOR4 *pBlend = NULL);

	
	
	/**
	* \brief Find a tile from a specified coordinates. Limited to a highest allocated level found from memory. 
	* \param hMgr handle to a tile/planet manager
	* \param lng longitude of the location.
	* \param lng latitude of the location.
	* \param maxlevel highest level to search, -1 = Current render level.
	* \return NULL, or a tile handle at the current render resolution
	* \note WARNING: Tile returned by this function can become invalid without notice.
	*/
	gc_interface PickGround GetTileData(HPLANETMGR hMgr, double lng, double lat, int maxlevel = -1);

	gc_interface HTILE GetTile(HPLANETMGR hMgr, double lng, double lat, int maxlevel = -1);

	/**
	* \brief Find a tile from a specified coordinates. Limited to a highest allocated level found from memory.
	* \param hMgr handle to a tile/planet manager
	* \param iLng longitude index
	* \param iLng latitude index
	* \param level level of the tile
	* \param flags what to search (see gcTileFlags) 
	* \return NULL, or a tile handle at the current render resolution
	* \note WARNING: Tile returned by this function can become invalid without notice.
	*/
	gc_interface bool HasTileData(HPLANETMGR hMgr, int iLng, int iLat, int level, int flags);

	gc_interface SURFHANDLE SeekTileTexture(HPLANETMGR hMgr, int iLng, int iLat, int level, int flags = 3, void *reserved = NULL);

	gc_interface bool SeekTileElevation(HPLANETMGR hMgr, int iLng, int iLat, int level, int flags, ElevInfo *pEI);
	

	/**
	* \brief Seek surface elevation from within the tile
	* \param hTile handle to a tile
	* \param lng geocentric longitude
	* \param lat geocentric latitude
	* \param out_elev pointer to float receiving the elevation above mean radius.
	* \return 1 = Nominal, 0 = Tile Invisible but valid, -1 = (lng,lat) out of bounds, -3 = Fail
	*/
	gc_interface int GetElevation(HTILE hTile, double lng, double lat, double *out_elev);


	/**
	* \brief Create Image processing interface which allows user to process and create data via GPU
	* \param file *.hlsl file name containing the shader
	* \param PSEntry Entry point name for per pixel operations
	* \param VSEntry Entry point name for vertex operations or NULL for a simple rectangular shape
	* \param ppf a list of preprocessor directives e.g. "_MYSECTION;_DEBUG" used like #if defined(_MYSECTION) ..code.. #endif
	* \return Pointer to IPInterface
	*/
	gc_interface gcIPInterface* CreateIPInterface(const char* file, const char* PSEntry, const char* VSEntry = NULL, const char* ppf = NULL);
	gc_interface void ReleaseIPInterface(gcIPInterface* pIPI);
	//@}
};



inline gcCore2* gcGetCoreInterface()
{
	if (pCoreInterface) return pCoreInterface;
	HMODULE hModule = GetModuleHandle("D3D9Client.dll");
	if (hModule) {
		__gcBindCoreMethod pBindCoreMethod = (__gcBindCoreMethod)GetProcAddress(hModule, "gcBindCoreMethod");
		if (pBindCoreMethod) return (pCoreInterface = new gcCore2(pBindCoreMethod));
		else oapiWriteLogV("gcGetCoreInterface() FAILED");
	} else oapiWriteLogV("gcGetCoreInterface() FAILED. D3D9Client Not Found");
	return NULL;
}

#endif // !__GC_CORE
