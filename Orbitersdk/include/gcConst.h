// =================================================================================================================================
//
// Copyright (C) 2014 - 2020 Jarmo Nikkanen
//
// Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation
// files (the "Software"), to use, copy, modify, merge, publish, distribute, interact with the Software and sublicense copies
// of the Software, subject to the following conditions:
//
// a) You do not sell, rent or auction the Software.
// b) You do not collect distribution fees.
// c) If the Software is distributed in an object code form, it must inform that the source code is available and how to obtain it.
// d) You do not remove or alter any copyright notices contained within the Software.
// e) This copyright notice must be included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
// OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
// LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR
// IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
// =================================================================================================================================


#include "OrbiterAPI.h"
#include "DrawAPI.h"
#include <assert.h>

#pragma once

using namespace std;
using namespace oapi;

class gcCore;


inline gcCore *gcGetCoreInterface()
{
	typedef gcCore * (__cdecl *__gcGetCoreAPI)();
	HMODULE hModule = GetModuleHandle("D3D9Client.dll");
	if (hModule) {
		__gcGetCoreAPI pGetCoreAPI = (__gcGetCoreAPI)GetProcAddress(hModule, "gcGetCoreAPI");
		if (pGetCoreAPI) return pGetCoreAPI();
	}
	return NULL;
}


/**
* \file gcConst.h
* \brief Structures and definations
*/


/// \defgroup PixelFormats Common pixelformats for surfaces, for Native Only (i.e. HSURFNATIVE)
///@{
#define OAPISURFACE_PF_MASK				0xFF0000	///< PixelFormat Mask
#define OAPISURFACE_PF_XRGB				0x000000	///< 32bit RGB no-alpha
#define OAPISURFACE_PF_ARGB				0x010000	///< 32bit ARGB with-alpha	0xAARRGGBB
#define OAPISURFACE_PF_RGB565			0x020000	///< 16bit RGB no-alpha
#define OAPISURFACE_PF_S16R				0x030000	///< Signed integer 16-bit (1-channel)
#define OAPISURFACE_PF_F32R				0x040000	///< Float 32-bit (1-channel)
#define OAPISURFACE_PF_F32RG			0x050000	///< Float 64-bit (2-channel)
#define OAPISURFACE_PF_F32RGBA			0x060000	///< Float 128-bit (4-channel) float4(r,g,b,a)
#define OAPISURFACE_PF_F16R				0x070000	///< Float 16-bit (1-channel) 
#define OAPISURFACE_PF_F16RG			0x080000	///< Float 32-bit (2-channel) 
#define OAPISURFACE_PF_F16RGBA			0x090000	///< Float 64-bit (4-channel) float4(r,g,b,a)
#define OAPISURFACE_PF_DXT1				0x0A0000	///< Compressed DXT1 format
#define OAPISURFACE_PF_DXT3				0x0B0000	///< Compressed DXT3 format
#define OAPISURFACE_PF_DXT5				0x0C0000	///< Compressed DXT5 format
#define OAPISURFACE_PF_DEPTH			0x0D0000	///< Depth-Stencil Surface (This will cause all other OAPISURFACE_* flags being ignored)
#define OAPISURFACE_PF_ALPHA			0x0E0000	///< Alpha only surface 8-bit
#define OAPISURFACE_PF_GRAY				0x0F0000	///< Grayscale Image 8-bit
///@}


/// \defgroup RenderProc Specify a SketchPad render callback function
///@{
#define RENDERPROC_DELETE				0x0000	///< Unregister/Remove existing callback 
#define RENDERPROC_HUD_1ST				0x0001	///< Register a HUD callback to draw under Orbiter's main HUD
#define RENDERPROC_HUD_2ND				0x0002	///< Register a HUD callback to draw over Orbiter's main HUD
#define RENDERPROC_PLANETARIUM			0x0003	///< Register a HUD callback to draw into a planetarium view using perspective projection
#define RENDERPROC_CUSTOMCAM_OVERLAY	0x0004  ///< Register a callback to draw an overlay into a custom camerea view
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


/// \defgroup dwFlags for gcSetupCustomCamera() API function
///@{
#define CUSTOMCAM_DEFAULTS				0x00FF
#define CUSTOMCAM_OVERLAY				0x0100
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

/// \defgroup MeshMaterialFlags Mesh material flags for gcMeshMaterial function
///@{
#define	MESHM_DIFFUSE		0x01	///< Stock material
#define	MESHM_AMBIENT		0x02	///< Stock material
#define	MESHM_SPECULAR		0x04	///< Stock material
#define	MESHM_EMISSION		0x08	///< Stock material
#define	MESHM_EMISSION2		0x10	///< D3D9 material
#define	MESHM_REFLECT		0x20	///< D3D9 material
#define	MESHM_ROUGHNESS		0x40	///< D3D9 material
#define	MESHM_FRESNEL		0x80	///< D3D9 material
///@}

namespace gcGUI 
{
	// -----------------------------
	// Dialog status identifiers 
	//
	static const int INACTIVE = 0;
	static const int DS_FLOAT = 1;
	static const int DS_LEFT = 2;
	static const int DS_RIGHT = 3;

	// -----------------------------
	// Bitmap Identifiers
	//
	static const int BM_TITLE = 0;
	static const int BM_SUBTITLE = 1;
	static const int BM_ICONS = 2;

	// -----------------------------
	// Messages passed to gcGUIApp::clbkMessge
	//
	static const int MSG_OPEN_NODE = 1;
	static const int MSG_CLOSE_NODE = 2;
	static const int MSG_CLOSE_APP = 3;
};


namespace gcMatrix
{
	static const int offset = 1;		///< Set/Get Mesh offset matrix, Also used by VESSEL::ShiftMesh()
	static const int mesh = 2;			///< Set/Get Mesh animation matrix, Transforms all the groups in the mesh
	static const int group = 3;			///< Set/Get Group animation matrix, Transforms a single group
	static const int combined = 4;		///< Get combined Mesh*Group*Offset matrix. (Can't 'set' this)
};

namespace gcTileFlags
{
	static const int TEXTURE = 0x1;			///< Texture data
	static const int MASK = 0x2;			///< Nightlights/Water mask
	static const int ELEVATION = 0x3;		///< Elevation
	static const int CLOUD = 0x4;			///< Texture data
	static const int ELEV_MOD = 0x5;		///< Elevation
	// ------------------------------------------------------------------
	static const int TREE = 0x10;			///< Search/Use Tree Archive
	static const int CACHE = 0x20;			///< Search/Use Cache
	static const int MOD = 0x40;			///< Search/Use ElevMod Cache
};


/**
* \brief Flags for 
*/
namespace gcFont
{
	static const int ITALIC = 0x1;
	static const int UNDERLINE = 0x2;
	static const int STRIKEOUT = 0x4;
	static const int CRISP = 0x8;			///< Override app-default, No Antialiasing
	static const int ANTIALIAS = 0x10;		///< Override app-default, Use Antialiashing
};


/// \brief Handle to a surface manager's glogal overlay
typedef void * HOVERLAY;
/// \brief Handle to a native DirectX9 surface
typedef void * HSURFNATIVE;
/// \brief Handle to a planet/surface manager
typedef void * HPLANETMGR;
/// \brief Handle to an instance buffer
typedef void * HINSTBUF;
/// \brief Handle to a surface tile (SurfTile)
typedef void * HTILE;
/// \brief Hnadle to a dialog node in gcGUI
typedef void * HNODE;
/// \brief Custom swapchain handle
typedef void * HSWAP;
/// \brief Custom camera handle
typedef void * CAMERAHANDLE;
/// \brief Sketchmesh handle
typedef void * SKETCHMESH;
/// \brief Poly object handle
typedef void * HPOLY;


/// \brief Render HUD and Planetarium callback function 
typedef void(__cdecl *__gcRenderProc)(oapi::Sketchpad *pSkp, void *pParam);
typedef void(__cdecl *__gcGenericProc)(int iUser, void *pUser, void *pParam);


namespace oapi {

	/**
	* \brief 32-bit floating point 2D vector type.
	* \note This structure is compatible with the D3DXVECTOR2 type.
	*/
	typedef struct FVECTOR2 {

		FVECTOR2()
		{
			x = y = 0.0f;
		}

		FVECTOR2(float q)
		{
			x = y = q;
		}

		FVECTOR2(float _x, float _y)
		{
			x = _x;
			y = _y;
		}

		FVECTOR2(const POINT &p)
		{
			x = float(p.x);
			y = float(p.y);
		}

		FVECTOR2(const POINT *p)
		{
			x = float(p->x);
			y = float(p->y);
		}

		FVECTOR2(const oapi::IVECTOR2 &p)
		{
			x = float(p.x);
			y = float(p.y);
		}

		inline FVECTOR2 operator* (float f) const
		{
			return FVECTOR2(x * f, y * f);
		}

		inline FVECTOR2 operator/ (float f) const
		{
			f = 1.0f / f;
			return FVECTOR2(x * f, y * f);
		}

		inline FVECTOR2 operator+ (float f) const
		{
			return FVECTOR2(x + f, y + f);
		}

		inline FVECTOR2 operator- (float f) const
		{
			return FVECTOR2(x - f, y - f);
		}

		inline FVECTOR2 operator+ (const FVECTOR2 &f) const
		{
			return FVECTOR2(x + f.x, y + f.y);
		}

		inline FVECTOR2 operator- (const FVECTOR2 &f) const
		{
			return FVECTOR2(x - f.x, y - f.y);
		}

		float x, y;
	} FVECTOR2;



	/**
	* \brief 32-bit floating point 3D vector type.
	* \note This structure is compatible with the D3DXVECTOR3 type.
	*/
	typedef struct FVECTOR3 {

		FVECTOR3()
		{
			x = y = z = 0.0f;
		}

		FVECTOR3(float q)
		{
			x = y = z = q;
		}

		FVECTOR3(float _x, float _y, float _z)
		{
			x = _x;
			y = _y;
			z = _z;
		}

		FVECTOR3(VECTOR3 &v)
		{
			x = float(v.x);
			y = float(v.y);
			z = float(v.z);
		}

		inline FVECTOR3 operator* (float f) const
		{
			return FVECTOR3(x * f, y * f, z * f);
		}

		inline FVECTOR3 operator/ (float f) const
		{
			f = 1.0f / f;
			return FVECTOR3(x * f, y * f, z * f);
		}

		inline FVECTOR3 operator+ (float f) const
		{
			return FVECTOR3(x + f, y + f, z + f);
		}

		inline FVECTOR3 operator- (float f) const
		{
			return FVECTOR3(x - f, y - f, z - f);
		}

		inline FVECTOR3 operator+ (const FVECTOR3 &f) const
		{
			return FVECTOR3(x + f.x, y + f.y, z + f.z);
		}

		inline FVECTOR3 operator- (const FVECTOR3 &f) const
		{
			return FVECTOR3(x - f.x, y - f.y, z - f.z);
		}

		float x, y, z;
	} FVECTOR3;


	/**
	* \brief 32-bit floating point 4D vector type.
	* \note This structure is compatible with the D3DXVECTOR4 type.
	*/
#pragma pack(push, 1)
	typedef union FVECTOR4 
	{
		DWORD dword_abgr() const
		{
			DWORD dr = DWORD(max(0, r) * 255.0f + 0.5f);
			DWORD dg = DWORD(max(0, g) * 255.0f + 0.5f);
			DWORD db = DWORD(max(0, b) * 255.0f + 0.5f);
			DWORD da = DWORD(max(0, a) * 255.0f + 0.5f);
			if (dr > 0xFF) dr = 0xFF;
			if (dg > 0xFF) dg = 0xFF;
			if (db > 0xFF) db = 0xFF;
			if (da > 0xFF) da = 0xFF;		
			return (da << 24) | (db << 16) | (dg << 8) | dr;
		}

		DWORD dword_argb() const
		{
			DWORD dr = DWORD(max(0, r) * 255.0f + 0.5f);
			DWORD dg = DWORD(max(0, g) * 255.0f + 0.5f);
			DWORD db = DWORD(max(0, b) * 255.0f + 0.5f);
			DWORD da = DWORD(max(0, a) * 255.0f + 0.5f);
			if (dr > 0xFF) dr = 0xFF;
			if (dg > 0xFF) dg = 0xFF;
			if (db > 0xFF) db = 0xFF;
			if (da > 0xFF) da = 0xFF;
			return (da << 24) | (dr << 16) | (dg << 8) | db;
		}

		FVECTOR4()
		{
			r = g = b = a = 0.0f;
		}

		FVECTOR4(float q)
		{
			x = y = z = w = q;
		}

		FVECTOR4(const COLOUR4 &c)
		{
			r = c.r;
			g = c.g;
			b = c.b;
			a = c.a;
		}

		FVECTOR4(DWORD abgr)
		{
			DWORD dr = (abgr & 0xFF); abgr >>= 8;
			DWORD dg = (abgr & 0xFF); abgr >>= 8;
			DWORD db = (abgr & 0xFF); abgr >>= 8;
			DWORD da = (abgr & 0xFF);
			//if (da == 0) da = 255;
			float q = 3.92156862e-3f;
			r = float(dr) * q;
			g = float(dg) * q;
			b = float(db) * q;
			a = float(da) * q;
		}

		FVECTOR4(const VECTOR4 &v)
		{
			x = float(v.x);
			y = float(v.y);
			z = float(v.z);
			w = float(v.w);
		}

		FVECTOR4(const VECTOR3 &v, float _w)
		{
			x = float(v.x);
			y = float(v.y);
			z = float(v.z);
			w = _w;
		}

		FVECTOR4(const FVECTOR3 &v, float _w)
		{
			rgb = v;
			w = _w;
		}

		FVECTOR4(float _x, float _y, float _z, float _w)
		{
			x = float(_x);
			y = float(_y);
			z = float(_z);
			w = float(_w);
		}

		FVECTOR4(int _x, int _y, int _z, int _w)
		{
			x = float(_x);
			y = float(_y);
			z = float(_z);
			w = float(_w);
		}

		FVECTOR4(double _x, double _y, double _z, double _w)
		{
			x = float(_x);
			y = float(_y);
			z = float(_z);
			w = float(_w);
		}


		inline FVECTOR4 operator* (float f) const
		{
			return FVECTOR4( x * f, y * f, z * f, w);
		}

		inline FVECTOR4& operator*= (float f)
		{
			x *= f; y *= f; z *= f; w *= f;
			return *this;
		}

		inline FVECTOR4 operator/ (float f) const
		{
			f = 1.0f / f;
			return FVECTOR4(x * f, y * f, z * f, w);
		}
		
		inline FVECTOR4 operator+ (float f) const
		{
			return FVECTOR4(x + f, y + f, z + f, w);
		}

		inline FVECTOR4 operator- (float f) const
		{
			return FVECTOR4(x - f, y - f, z - f, w);
		}

		inline FVECTOR4 operator+ (const FVECTOR4 &f) const
		{
			return FVECTOR4(x + f.x, y + f.y, z + f.z, w);
		}

		inline FVECTOR4 operator- (const FVECTOR4 &f) const
		{
			return FVECTOR4(x - f.x, y - f.y, z - f.z, w);
		}

		float data[4];
		struct { float x, y, z, w; };
		struct { float r, g, b, a; };
		FVECTOR3 xyz;     //  , w; };
		FVECTOR3 rgb;    //   , a; };
	} FVECTOR4;
#pragma pack(pop)


	typedef union DRECT
	{
		DRECT()
		{
			left = right = top = bottom = 0.0;
		}

		DRECT(double l, double t, double r, double b)
		{
			left = l; top = t; right = r; bottom = b;
		}

		DRECT(float l, float t, float r, float b)
		{
			left = double(l); top = double(t); right = double(r); bottom = double(b);
		}

		DRECT(const DRECT &x)
		{
			left = x.left; 
			top = x.top; 
			right = x.right; 
			bottom = x.bottom;
		}

		VECTOR4 vec;

		struct {
			double left, top, right, bottom;
		};

	} DRECT;

	/**
	* \brief Float-valued 4x4 matrix.
	* \note This structure is compatible with the D3DXMATRIX.
	*/
	typedef union FMATRIX4 {
		FMATRIX4() {}

		FMATRIX4(const float *pSrc) {
			for (int i = 0; i < 16; i++) data[i] = pSrc[i];
		}

		void Zero()
		{
			for (int i = 0; i < 16; i++) data[i] = 0.0;
		}

		void Ident() 
		{
			for (int i = 0; i < 16; i++) data[i] = 0.0;
			m11 = m22 = m33 = m44 = 1.0f;
		}

		float data[16];
		struct { FVECTOR4 _x, _y, _z, _p; };
		struct { float m11, m12, m13, m14, m21, m22, m23, m24, m31, m32, m33, m34, m41, m42, m43, m44; };
	} FMATRIX4;


	/**
	* \brief Vector Matrix multiplication
	*/
	inline FVECTOR4 mul(const FVECTOR4 &V, const FMATRIX4 &M)
	{
		float x = V.x*M.m11 + V.y*M.m21 + V.z*M.m31 + V.w*M.m41;
		float y = V.x*M.m12 + V.y*M.m22 + V.z*M.m32 + V.w*M.m42;
		float z = V.x*M.m13 + V.y*M.m23 + V.z*M.m33 + V.w*M.m43;
		float w = V.x*M.m14 + V.y*M.m24 + V.z*M.m34 + V.w*M.m44;
		return FVECTOR4(x, y, z, w);
	}


	/**
	* \brief Transform a position by matrix
	*/
	inline FVECTOR3 TransformCoord(const FVECTOR3 &V, const FMATRIX4 &M)
	{
		float x = V.x*M.m11 + V.y*M.m21 + V.z*M.m31 + M.m41;
		float y = V.x*M.m12 + V.y*M.m22 + V.z*M.m32 + M.m42;
		float z = V.x*M.m13 + V.y*M.m23 + V.z*M.m33 + M.m43;
		float w = V.x*M.m14 + V.y*M.m24 + V.z*M.m34 + M.m44;
		w = 1.0f / w;
		return FVECTOR3(x*w, y*w, z*w);
	}


	/**
	* \brief Transform a normal or direction by matrix
	*/
	inline FVECTOR3 TransformNormal(const FVECTOR3 &V, const FMATRIX4 &M)
	{
		float x = V.x*M.m11 + V.y*M.m21 + V.z*M.m31;
		float y = V.x*M.m12 + V.y*M.m22 + V.z*M.m32;
		float z = V.x*M.m13 + V.y*M.m23 + V.z*M.m33;
		return FVECTOR3(x, y, z);
	}


	inline FVECTOR2 unit(const FVECTOR2 &v)
	{
		float f = 1.0f / sqrt(v.x*v.x + v.y*v.y);
		return FVECTOR2(v.x*f, v.y*f);
	}

	inline FVECTOR3 unit(const FVECTOR3 &v)
	{
		float f = 1.0f / sqrt(v.x*v.x + v.y*v.y + v.z*v.z);
		return FVECTOR3(v.x*f, v.y*f, v.z*f);
	}

	inline float dot(const FVECTOR2 &v, const FVECTOR2 &w)
	{
		return v.x*w.x + v.y*w.y;
	}

	inline float dot(const FVECTOR3 &v, const FVECTOR3 &w)
	{
		return v.x*w.x + v.y*w.y + v.z*w.z;
	}

	inline float dot(const FVECTOR4 &v, const FVECTOR4 &w)
	{
		return v.x*w.x + v.y*w.y + v.z*w.z + v.w*w.w;
	}

	inline float length(const FVECTOR2 &v)
	{
		return sqrt(v.x*v.x + v.y*v.y);
	}

	inline float length(const FVECTOR3 &v)
	{
		return sqrt(v.x*v.x + v.y*v.y + v.z*v.z);
	}

	inline FVECTOR3 cross(const FVECTOR3 &a, const FVECTOR3 &b)
	{
		return FVECTOR3(a.y*b.z - a.z*b.y, a.z*b.x - a.x*b.z, a.x*b.y - a.y*b.x);
	}

	inline float saturate(float x)
	{
		return min(1, max(0, x));
	}

	inline FVECTOR3 saturate(FVECTOR3 &v)
	{
		return FVECTOR3(saturate(v.x), saturate(v.y), saturate(v.z));
	}

	inline FVECTOR4 saturate(FVECTOR4 &v)
	{
		return FVECTOR4(saturate(v.x), saturate(v.y), saturate(v.z), saturate(v.w));
	}
}




// ===========================================================================
/**
* \class gcGUI
* \brief gcGUI Access and management functions
*/
// ===========================================================================

class gcGUIBase
{
	friend class gcGUIApp;

private:

	virtual HNODE			RegisterApplication(gcGUIApp *pApp, const char *label, HWND hDlg, DWORD docked, DWORD color) = 0;
	virtual HNODE			RegisterSubsection(HNODE hNode, const char *label, HWND hDlg, DWORD color) = 0;
	virtual void			UpdateStatus(HNODE hNode, const char *label, HWND hDlg, DWORD color) = 0;
	virtual bool			IsOpen(HNODE hNode) = 0;
	virtual void			OpenNode(HNODE hNode, bool bOpen = true) = 0;
	virtual void			DisplayWindow(HNODE hNode, bool bShow = true) = 0;
	virtual HFONT			GetFont(int id) = 0;
	virtual HNODE			GetNode(HWND hDlg) = 0;
	virtual HWND			GetDialog(HNODE hNode) = 0;
	virtual void			UpdateSize(HWND hDlg) = 0;
	virtual void			UnregisterApp(gcGUIApp *pApp) = 0;
	virtual bool			UnRegister(HNODE hNode) = 0;
};



// ===========================================================================
/**
* \class gcGUI
* \brief gcGUI Access and management functions
*/
// ===========================================================================

class gcGUIApp
{

public:

	gcGUIApp() : pApp(NULL) 
	{  

	}


	~gcGUIApp() 
	{ 
		// Can do nothing here, too late
	}

	// -----------------------------------------------------
	
	virtual void clbkShutdown() 
	{
		if (pApp) pApp->UnregisterApp(this);
	}

	virtual bool clbkMessage(DWORD uMsg, HNODE hNode, int data)
	{
		return false;
	}

	// -----------------------------------------------------

	inline bool Initialize()
	{
		typedef gcGUIBase * (__cdecl *__gcGetGUICore)();
		HMODULE hModule = GetModuleHandle("D3D9Client.dll");
		if (hModule) {
			__gcGetGUICore pGetGUICore = (__gcGetGUICore)GetProcAddress(hModule, "gcGetGUICore");
			if (pGetGUICore) return ((pApp = pGetGUICore()) != NULL);
		}
		return false;
	}

	HNODE RegisterApplication(const char *label, HWND hDlg, DWORD docked, DWORD color = 0)
	{
		assert(pApp);
		return pApp->RegisterApplication(this, label, hDlg, docked, color);
	}

	HNODE RegisterSubsection(HNODE hNode, const char *label, HWND hDlg, DWORD color = 0) 
	{ 
		assert(pApp);
		return pApp->RegisterSubsection(hNode, label, hDlg, color);
	}

	void UpdateStatus(HNODE hNode, const char *label, HWND hDlg, DWORD color = 0)
	{
		assert(pApp);
		return pApp->UpdateStatus(hNode, label, hDlg, color);
	}

	bool IsOpen(HNODE hNode) 
	{
		assert(pApp);
		return pApp->IsOpen(hNode);
	}

	void OpenNode(HNODE hNode, bool bOpen = true) 
	{ 
		assert(pApp);
		pApp->OpenNode(hNode, bOpen);
	}

	void DisplayWindow(HNODE hNode, bool bShow = true) 
	{ 
		assert(pApp);
		pApp->DisplayWindow(hNode, bShow);
	}

	HFONT GetFont(int id) 
	{ 
		assert(pApp);
		return pApp->GetFont(id);
	}

	HNODE GetNode(HWND hDlg) 
	{ 
		assert(pApp);
		return pApp->GetNode(hDlg);
	}

	HWND GetDialog(HNODE hNode)
	{ 
		assert(pApp);
		return pApp->GetDialog(hNode);
	}

	void UpdateSize(HWND hDlg) 
	{ 
		assert(pApp);
		pApp->UpdateSize(hDlg);
	}

	bool UnRegister(HNODE hNode)
	{
		assert(pApp);
		return pApp->UnRegister(hNode);
	}

private:

	gcGUIBase *pApp;
};





// ===========================================================================
/**
* \class gcCore
* \brief Core class for graphics services 
*/
// ===========================================================================

class gcCore
{

public:

	typedef struct {
		int				Width;
		int				Height;
		int				Mips;
		DWORD			Flags;
	} SurfaceSpecs;

	typedef struct {
		FVECTOR2		pos;
		DWORD			color;
	} TriangleVtx;

	typedef struct {
		WORD			Size;			///< sizeof(ElevInfo)
		WORD			Format;
		double			Resolution;
	} ElevInfo;

	typedef struct {
		HTILE			pTile;			
		int				Lvl;
		int				iLng;
		int				iLat;
	} TileCreated;

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
		double			lng, lat;		///< Longitude and Latigude of the point being clicked
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
		int				MaxTexSize;		///< Maximum texture size in pixels
		int				DisplayMode;	///< 0 = True Fullscreen, 1 = Fullscreen Window, 2 = Windowed
		int				MaxTexRep;		///< Maximum texture repeat count
		DWORD			gcAPIVer;		///< gcAPI Build Date 0xYYYYMMDD
	} SystemSpecs;

	typedef struct {
		HTILE			hTile;			///< WARNING: Tile returned by this data entry can become invalid without notice.
		FMATRIX4		mWorld;
	} RenderTileData;


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
	virtual HSWAP		RegisterSwap(HWND hWnd, HSWAP hSwap = NULL, int AA = 0);

	/**
	* \brief Flip backbuffer to a front
	* \param hSwap Handle to a swap object.
	*/
	virtual void		FlipSwap(HSWAP hSwap);

	/**
	* \brief Get a backbuffer surface
	* \param hSwap Handle to a swap object.
	* \return A Handle to a rendering surface (i.e. backbuffer)
	*/
	virtual SURFHANDLE	GetRenderTarget(HSWAP hSwap);
	virtual HSURFNATIVE GetRenderTargetNative(HSWAP hSwap);

	/**
	* \brief Release a swap object after it's no longer needed.
	* \param hSwap Handle to a swap object.
	*/
	virtual void		ReleaseSwap(HSWAP hSwap);
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
	virtual int			DeleteCustomCamera(CAMERAHANDLE hCam);

	/**
	* \brief Toggle camera on and off
	* \param hCam camera handle to toggle
	* \param bOn true to turn on the camera.
	* \note If multiple cameras are sharing the same rendering surface. Flickering will occur if more than one camera is turned on.
	*/
	virtual void		CustomCameraOnOff(CAMERAHANDLE hCam, bool bOn);

	/**
	* \brief Create a new custom camera that can be used to render views into a surfaces and textures
	* \param hCam camera handle to modify an existing camera or, NULL
	* \param hVessel handle to a vessel where the camera is attached to.
	* \param vPos camara position in vessel's local coordinate system
	* \param vDir camara direction in vessel's local coordinate system. [Unit Vector]
	* \param vUp camara up vector. Must be perpendicular to vDir. [Unit Vector]
	* \param dFow camera field of view in radians
	* \param hSurf rendering surface. Must be created atleast with OAPISURFACE_RENDER3D | OAPISURFACE_RENDERTARGET. Multiple cameras can share the same surface.
	* \param dwFlags Flags to controls what is drawn and what is not.
	* \return Camera handle, or NULL if an error occured or if the custom camera interface is disabled.
	* \note Camera count is unlimited.
	* \note Only a cameras attached to currently active vessel are operational and recodring.
	* \note Having multiple cameras active at the same time doesn't impact in a frame rate, however, camera refresh rates are reduced.
	*/
	virtual CAMERAHANDLE SetupCustomCamera(CAMERAHANDLE hCam, OBJHANDLE hVessel, VECTOR3 &vPos, VECTOR3 &vDir, VECTOR3 &vUp, double dFov, SURFHANDLE hSurf, DWORD dwFlags = 0xFF);
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
	virtual int			SketchpadVersion(Sketchpad *pSkp);

	/**
	* \brief Get a Sketchpad for a native DirectX 9 surface
	* \param hSrf handle to a surface
	* \param hDep handle to optional depth stencil surface
	* \return a pointer to a new sketchpad interface or NULL if an error occurs.
	*/
	virtual Sketchpad*	GetSketchpadNative(HSURFNATIVE hSrf, HSURFNATIVE hDep = NULL);

	/**
	* \brief Release a native sketchpad interface acquired by GetSketchpadNative()
	* \param pSkp handle to a sketchpad interface to release.
	*/
	virtual void		ReleaseSketchpadNative(Sketchpad *pSkp);

	/**
	* \brief Load a mesh from a harddrive to be used with Sketchpad2::SketchMesh
	* \param name Name of the mesh file without ".msh" identifier.
	* \sa gcDeleteSketchMesh
	* \note SKETCHMESH handle isn't compatible with MESHHANDLE nor DEVMESHHANDLE.
	*/
	virtual SKETCHMESH	LoadSketchMesh(const char *name);

	/**
	* \brief Delete a mesh previously loaded with gcLoadSketchMesh
	* \sa gcLoadSketchMesh
	*/
	virtual void		DeleteSketchMesh(SKETCHMESH hMesh);

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
	virtual HPOLY		CreatePoly(HPOLY hPoly, const oapi::FVECTOR2 *pt, int npt, DWORD flags = 0);


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
	* \note PF_FAN Triangle fan. The first vertex is in a centre of the fan/circle and other lie at the edge. ("npt" must be "number of triangles" + 2)
	* \note PF_STRIP Is build from quads. Where each quad requires two vertics. ("npt" must be "number of quads" * 2 + 2)
	*/
	virtual HPOLY		CreateTriangles(HPOLY hPoly, const gcCore::TriangleVtx *pt, int npt, DWORD flags);


	/**
	* \brief Deletes a polyline created with gcCreatePolyPolyline()
	* \param hPoly Handle to a polyline to be deleted
	* \sa gcCreatePolyline
	*/
	virtual void		DeletePoly(HPOLY hPoly);

	/**
	* \brief Compute a length of a text string
	* \param hFont a Pointer into a font
	* \param pText a Pointer into a text string
	* \param len a Length of the text string to process. -1 will scan to a NULL terminator.
	*/
	virtual DWORD		GetTextLength(oapi::Font *hFont, const char *pText, int len = -1);

	/**
	* \brief Find index of nearest "cap" between charters in specified location. (i.e. distance from start of the string in pixels)
	* \param hFont a Pointer into a font
	* \param pText a Pointer into a text line
	* \param pos a Position in pixels from start of the string
	* \param len a Length of the text line to process. -1 will process to a NULL terminator.
	* \return index from 0 to number of charters. For just one char it can be either "0" or "1" depending which side is closer to "pos".
	* \note This is used for finding a spot for a "cursor" when a text string is clicked with mouse.
	*/
	virtual DWORD		GetCharIndexByPosition(oapi::Font *hFont, const char *pText, int pos, int len = -1);

	/**
	* \brief This function will register a custom render callback function
	* \param proc function to be called when render event occur
	* \param id render event id
	* \param pParam a pointer to user data (to a class for an example)
	* \return false if an error occured, true otherwise.
	*/
	virtual bool		RegisterRenderProc(__gcRenderProc proc, DWORD id, void *pParam);

	/**
	* \brief Create a Font
	* \param height Font height
	* \param face Name of the font
	* \param width Width of the font (0 for default aspect ration)
	* \param weight Font thikness (400 for default weight)
	* \param style A combination of \see gcFont flags (0 for default)
	* \param spacing A spacing between charters in a string (0.0f for default)
	* \return A pointer to a created or pre-existing font or NULL in a case of an error.
	*/
	virtual oapi::Font *CreateSketchpadFont(int height, char *face, int width = 0, int weight = 400, int gcFontStyle = 0, float spacing = 0.0f);
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
	* \param bSet \e true to set material value, \e false to get a meterial value
	* \return -4 = Invalid handle \n -3 = Unknown property flag \n -2 = Property not specified cannot get it \n -1 = Index out of range \n 0 = Success
	*/
	virtual int			MeshMaterial(DEVMESHHANDLE hMesh, DWORD idx, int prop, COLOUR4 *value, bool bSet);

	/**
	* \brief A Function to get a mesh transformation/animation matrix.
	* \param matrix_id Id of the matrix to get. One of gcMatrix::xxx datatypes.
	* \param hVessel Vessel object handle.
	* \param mesh Mesh index
	* \param group Group index
	* \param pMat A pointer to FMATRIX4 struct for receiving the data.
	* \return 0 = on Success, or error code.
	*/
	virtual int			GetMatrix(int matrix_id, OBJHANDLE hVessel, DWORD mesh, DWORD group, oapi::FMATRIX4 *pMat);


	/**
	* \brief A Function to set a mesh transformation/animation matrix. Do not use this function for animated parts/meshes.
	* \param matrix_id Id of the matrix to set. One of gcMatrix::xxx datatypes.
	* \param hVessel Vessel object handle.
	* \param mesh Mesh index
	* \param group Group index
	* \param pMat A pointer to FMATRIX4 containing the data to set.
	* \return 0 = on Success, or error code.
	*/
	virtual int			SetMatrix(int matrix_id, OBJHANDLE hVessel, DWORD mesh, DWORD group, const oapi::FMATRIX4 *pMat);
	//@}




	// ===========================================================================
	/// \name Some Helper Functions
	// ===========================================================================
	//@{
	/**
	* \brief Get some system information
	* \param sp A pointer to SystemSpecs struct
	* \param size sizeof(SystemSpecs)
	*/
	virtual void			GetSystemSpecs(SystemSpecs *sp, int size);

	/**
	* \brief Conver a floating point color to DWORD color value
	* \param c A pointer to a color
	* \return DWORD color in 0xAABBGGRR
	* \note Alpha will range from 1 to 255. Zero is never returned because of backwards compatibility issues 0-alpha is mapped to 255
	*/
	virtual DWORD			Color(const COLOUR4 *c);

	/**
	* \brief Conver a floating point color to DWORD color value
	* \param c A pointer to a color
	* \return DWORD color in 0xAABBGGRR
	* \note Alpha will range from 1 to 255. Zero is never returned because of backwards compatibility issues 0-alpha is mapped to 255
	*/
	virtual DWORD			Color(const oapi::FVECTOR4 *c);

	/**
	* \brief Conver a DWORD color to floating point COLOUR4 value
	* \param dwABGR A color in 0xAABBGGRR
	* \return COLOUR4
	* \note Alpha will range from 1 to 255. Zero is never used because of backwards compatibility issues 0-alpha is mapped to 255
	*/
	virtual COLOUR4			Colour4(DWORD dwABGR);


	/**
	* \brief Get Surface Attributes (e.g. OAPISURFACE_TEXTURE)
	* \param hSurf handle to a surface
	* \param bCreation if true return creation time attributes, if false return current attributes
	* \return Surface attributes
	*/
	virtual DWORD			GetSurfaceAttribs(SURFHANDLE hSurf, bool bCreation = false);

	/**
	* \brief Convert an existing surface to an other type.
	* \param hSurf handle to a surface
	* \param attrib new attributes
	*/
	virtual void			ConvertSurface(SURFHANDLE hSurf, DWORD attrib);

	/**
	* \brief Load a texture into a specific type of a surface
	* \param fname name of a texture to be loaded.
	* \param flags surface attributes (see: OAPISURFACE_x flags)
	* \return surface handle or NULL in a case of an error
	*/
	virtual SURFHANDLE		LoadSurface(const char *fname, DWORD flags);

	/**
	* \brief Load a bitmap from file (*.bmp *.png *.jpg *.gif)
	* \param fname name of the file to be loaded.
	* \return Bitmap handle of NULL in a case of an error
	*/
	virtual HBITMAP			LoadBitmapFromFile(const char *fname);
	
	/**
	* \brief Get render window handle
	* \return Render window handle
	*/
	virtual HWND			GetRenderWindow();

	/**
	* \brief Register generic callback function
	* \param proc function to be called when event occur
	* \param id requested callback event id
	* \param pParam a pointer to user data (to a class for an example)
	* \return false if an error occured, true otherwise.
	*/
	virtual bool			RegisterGenericProc(__gcGenericProc proc, DWORD id, void *pParam);
	//@}






	// ===========================================================================
	/// \name Planetary surface interface
	/// Graphics client maintains a tile database for a tiles used in rendering
	/// This API can't access tile data outside visual range
	// ===========================================================================
	//@{
	virtual HPLANETMGR		GetPlanetManager(OBJHANDLE hPlanet);
	virtual HSURFNATIVE		SetTileOverlay(HTILE hTile, const HSURFNATIVE hOverlay);
	virtual HOVERLAY		AddGlobalOverlay(HPLANETMGR hMgr, VECTOR4 mmll, const HSURFNATIVE hOverlay = NULL, HOVERLAY hOld = NULL);

	/**
	* \brief Find a tile from a specified coordinates. Limited to a highest allocated level found from memory. 
	* \param hMgr handle to a tile/planet manager
	* \param lng longitude of the location.
	* \param lng latitude of the location.
	* \param maxlevel highest level to search, -1 = Current render level.
	* \return NULL, or a tile handle at the current render resolution
	* \note WARNING: Tile returned by this function can become invalid without notice.
	*/
	virtual PickGround		GetTileData(HPLANETMGR hMgr, double lng, double lat, int maxlevel = -1);
	virtual HTILE			GetTile(HPLANETMGR hMgr, double lng, double lat, int maxlevel = -1);

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
	virtual bool			HasTileData(HPLANETMGR hMgr, int iLng, int iLat, int level, int flags);
	virtual HSURFNATIVE		SeekTileTexture(HPLANETMGR hMgr, int iLng, int iLat, int level, int flags = 3, void *reserved = NULL);
	virtual void *			SeekTileElevation(HPLANETMGR hMgr, int iLng, int iLat, int level, int flags, ElevInfo *pEI);
	

	/**
	* \brief Find a tile from specified coordinates.
	* \param scr_x screen space x-coordinate.
	* \param scr_y screen space y-coordinate.
	* \return PickGround data structure, all members are zero if ray doesn't intersect ground. 
	*/
	virtual PickGround		ScanScreen(int scr_x, int scr_y);

	/**
	* \brief Seek surface elevation from within the tile
	* \param hTile handle to a tile
	* \param lng geocentric longitude
	* \param lat geocentric latitude
	* \param out_elev pointer to float receiving the elevation above mean radius.
	* \return 1 = Nominal, 0 = Tile Invisible but valid, -1 = (lng,lat) out of bounds, -3 = Fail
	*/
	virtual int				GetElevation(HTILE hTile, double lng, double lat, double *out_elev);
	//@}






	// ===========================================================================
	/// \name Native Object Interface
	// ===========================================================================
	//@{

	/**
	* \brief Load a file into native DirectX. Valid formats are (*.dds, *.jpg, *.bmp, *.png)
	* \param file filename.
	* \param flags a combination of OAPISURFACE_ flags.
	* \return NULL in a case of failure.
	*/
	virtual HSURFNATIVE		LoadSurfaceNative(const char *file, DWORD flags);

	/**
	* \brief Create a native DirectX 9 Surface
	* \param width surface width in pixels 
	* \param height surface height in pixels
	* \param flags a combination of OAPISURFACE_ flags.
	* \return Surface handle or NULL in a case of a failure.
	*/
	virtual HSURFNATIVE		CreateSurfaceNative(int width, int height, DWORD flags);

	/**
	* \brief Get a handle to a specific mipmap sub-level
	* \param hSrf Handle to a texture containing mipmaps
	* \param level Level of the mipmap to acquire. (level >= 1) (0 = "hSrf" it self with surface interface)
	* \return Surface handle or NULL in a case of a failure. Must be released with ReleaseSurface() after nolonger accessed.
	*/
	virtual HSURFNATIVE		GetMipSublevel(HSURFNATIVE hSrf, int level);

	virtual void			ReleaseSurface(HSURFNATIVE hSrf);
	virtual bool			GetSurfaceSpecs(HSURFNATIVE hSrf, SurfaceSpecs *pOut);

	/**
	* \brief Save a native DirectX surface to a file (*.dds, *.jpg, *.bmp, *.png)  
	* \param file filename.
	* \param hSrf handle to a surface to same.
	* \return false in a case of failure.
	*/
	virtual bool			SaveSurfaceNative(const char *file, HSURFNATIVE hSrf);

	/**
	* \brief Realtime Mipmap auto-generation from the top/main level.
	* \param hSurface handle to a surface
	* \return false if an error occured, true otherwise.
	* \note Surface must be created with (OAPISURFACE_TEXTURE | OAPISURFACE_RENDERTARGET | OAPISURFACE_MIPMAPS)
	* \note Exact attribute requirements/conflicts are unknown.
	*/
	virtual bool			GenerateMipMaps(HSURFNATIVE hSurface);

	/**
	* \brief On the fly texture compression into a DXT format. Input remains uncanged.
	* \param hSurface handle to a surface to compress
	* \param flags combination of OAPISURFACE_PF_DXT1, OAPISURFACE_PF_DXT3, OAPISURFACE_PF_DXT5, OAPISURFACE_MIPMAPS, OAPISURFACE_SYSMEM
	* \return Handle to a compressed texture, user must release this.
	* \note Compression is slow, separate thread recommended for realtime compression.
	*/
	virtual HSURFNATIVE		CompressSurface(HSURFNATIVE hSurface, DWORD flags);

	/**
	* \brief Get device specific mesh from Orbiter mesh template
	* \param hMesh handle to a mesh acquired from oapiLoadMeshGlobal()
	* \param pBox a pointer to an array of 8 FVECTOR3s
	*/
	virtual DEVMESHHANDLE	GetDevMesh(MESHHANDLE hMesh);
	virtual DEVMESHHANDLE	LoadDevMeshGlobal(const char *file_name, bool bUseCache = true);
	virtual void			ReleaseDevMesh(DEVMESHHANDLE hMesh);

	/**
	* \brief Recover tile bounding box data
	* \param hTile handle to a tile
	* \param pBox a pointer to an array of 8 FVECTOR3s
	*/
	virtual void			RenderMesh(DEVMESHHANDLE hMesh, const FMATRIX4 *pWorld);
	virtual bool			PickMesh(gcCore::PickMeshStruct *pm, DEVMESHHANDLE hMesh, const FMATRIX4 *pWorld, short x, short y);

	/**
	* \brief Recover tile bounding box data
	* \param hTile handle to a tile
	* \param pBox a pointer to an array of 8 FVECTOR3s
	*/
	virtual void			RenderMesh(DEVMESHHANDLE hMesh, HINSTBUF hInst);
	virtual bool			PickMesh(gcCore::PickMeshStruct *pm, DEVMESHHANDLE hMesh, HINSTBUF hInst);

	/**
	* \brief Create or Update instance data buffer
	* \param pData a pointer to world matrix array
	* \param size size of the array in bytes (i.e. sizeof(pData))
	* \param hBuf handle to an existing buffer when updating data.
	*/
	virtual HINSTBUF		CreateInstanceBuffer(const FMATRIX4 *pData, int size, HINSTBUF hBuf = NULL);
	virtual void			ReleaseInstanceBuffer(HINSTBUF hBuf);

	/**
	* \brief Render a list of independent lines 0-1, 2-3,...
	* \param pVtx a pointer to a vertex array
	* \param pIdx a pointer to index array
	* \param nIdx number of lines to draw multiplied by 2, (ARRAYSIZE() of index array) 
	* \param pWorld pointer to World matrix relative to camera
	* \param color color in 0xAABBGGRR
	*/
	virtual void			RenderLines(const FVECTOR3 *pVtx, const WORD *pIdx, int nVtx, int nIdx, const FMATRIX4 *pWorld, DWORD color);
	//@}



	/**
	* \brief Alters objects position. Matrix must be initially valid.
	* \param mat [in/out] Pointer to a matrix to change
	* \param pos New position
	*/
	inline void SetTranslation(FMATRIX4 *mat, const VECTOR3 &pos)
	{
		mat->m41 = float(pos.x); mat->m42 = float(pos.y); mat->m43 = float(pos.z);
	}

	inline void SetTranslation(FMATRIX4 *mat, const FVECTOR3 &pos)
	{
		mat->m41 = pos.x; mat->m42 = pos.y; mat->m43 = pos.z;
	}

	/**
	* \brief Creates a world transformation matrix
	* \param mat [out] Pointer to a matrix
	* \param pos Objects position relative to a camera in ecliptic frame
	* \param x X-axis, major axis [unit vector]
	* \param z Z-axis, minor axis [unit vector]
	* \param scale a sacle factor (default 1.0)
	*/
	inline void WorldMatrix(FMATRIX4 *mat, const VECTOR3 &pos, const VECTOR3 &x, const VECTOR3 &z, double scale = 1.0)
	{
		VECTOR3 y = crossp(x, z);
		mat->m11 = float(x.x * scale); mat->m12 = float(x.y * scale); mat->m13 = float(x.z * scale); mat->m14 = 0.0f;
		mat->m21 = float(y.x * scale); mat->m22 = float(y.y * scale); mat->m23 = float(y.z * scale); mat->m24 = 0.0f;
		mat->m31 = float(z.x * scale); mat->m32 = float(z.y * scale); mat->m33 = float(z.z * scale); mat->m34 = 0.0f;
		mat->m41 = float(pos.x);	   mat->m42 = float(pos.y);		  mat->m43 = float(pos.z);		 mat->m44 = 1.0f;
	}

	inline void WorldMatrix(FMATRIX4 *mat, const FVECTOR3 &pos, const FVECTOR3 &x, const FVECTOR3 &z, float scale = 1.0f)
	{
		FVECTOR3 y = cross(x, z);
		mat->m11 = (x.x * scale); mat->m12 = (x.y * scale); mat->m13 = (x.z * scale); mat->m14 = 0.0f;
		mat->m21 = (y.x * scale); mat->m22 = (y.y * scale); mat->m23 = (y.z * scale); mat->m24 = 0.0f;
		mat->m31 = (z.x * scale); mat->m32 = (z.y * scale); mat->m33 = (z.z * scale); mat->m34 = 0.0f;
		mat->m41 = (pos.x);		  mat->m42 = (pos.y);		mat->m43 = (pos.z);		  mat->m44 = 1.0f;
	}
};
