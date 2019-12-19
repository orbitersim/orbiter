// =================================================================================================================================
//
// Copyright (C) 2014 - 2019 Jarmo Nikkanen
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
#include <string>
#include <assert.h>

using namespace std;

/**
* \file gcConst.h
* \brief Structures and definations
*/

#pragma once

/// \defgroup RenderProc Specify a render HUD callback function
///@{
#define RENDERPROC_DELETE				0x0000	///< Unregister/Remove existing callback 
#define RENDERPROC_HUD_1ST				0x0001	///< Register a HUD callback to draw under Orbiter's main HUD
#define RENDERPROC_HUD_2ND				0x0002	///< Register a HUD callback to draw over Orbiter's main HUD
#define RENDERPROC_PLANETARIUM			0x0003	///< Register a HUD callback to draw into a planetarium view using perspective projection
#define RENDERPROC_CUSTOMCAM_OVERLAY	0x0004  ///< Register a callback to draw overlay into a custom camerea view
///@}

/// \defgroup RenderProc Specify a render HUD callback function
///@{
#define GENERICPROC_DELETE				0x0000	///< Unregister/Remove existing callback 
#define GENERICPROC_LOAD				0x0001	///< Sent a delayed (late) signal to load and initialize OrbiterGUI elements.
#define GENERICPROC_UNLOAD				0x0002	///< Sent a signal to use oapiReleaseTexture() and unload Sketchpad resources (pens, fonts, brushes).
#define GENERICPROC_LOAD_ORBITERGUI		0x0003	///< Sent a signal to load resources
#define GENERICPROC_UNLOAD_ORBITERGUI	0x0004	///< Sent a signal to unload resources
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


namespace gcMatrix
{
	const int offset = 1;		///< Set/Get Mesh offset matrix, Also used by VESSEL::ShiftMesh()
	const int mesh = 2;			///< Set/Get Mesh animation matrix, Transforms all the groups in the mesh
	const int group = 3;		///< Set/Get Group animation matrix, Transforms a single group
	const int combined = 4;		///< Get combined Mesh*Group*Offset matrix. (Can't 'set' this)
}

namespace gcWndFlag
{
	const int LEFT  = 0x000001;	///< Left dock
	const int RIGHT = 0x000002;	///< Right dock
	const int FLOAT = 0x000003;	///< Floating
	const int WSTD  = 0x000010;	///< hWnd is a Standard window HWND
	const int OGUI	= 0x000020;	///< hWnd is a OrbiterGUI window render surface
	const int OPEN  = 0x001000;	///< Window is open
	const int POPUP = 0x002000;	///< Popup Window
}

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

		inline FVECTOR2 operator* (float f)
		{
			return FVECTOR2(x * f, y * f);
		}

		inline FVECTOR2 operator/ (float f)
		{
			f = 1.0f / f;
			return FVECTOR2(x * f, y * f);
		}

		inline FVECTOR2 operator+ (float f)
		{
			return FVECTOR2(x + f, y + f);
		}

		inline FVECTOR2 operator- (float f)
		{
			return FVECTOR2(x - f, y - f);
		}

		inline FVECTOR2 operator+ (const FVECTOR2 &f)
		{
			return FVECTOR2(x + f.x, y + f.y);
		}

		inline FVECTOR2 operator- (const FVECTOR2 &f)
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
	

		inline FVECTOR3 operator* (float f)
		{
			return FVECTOR3(x * f, y * f, z * f);
		}

		inline FVECTOR3 operator/ (float f)
		{
			f = 1.0f / f;
			return FVECTOR3(x * f, y * f, z * f);
		}

		inline FVECTOR3 operator+ (float f)
		{
			return FVECTOR3(x + f, y + f, z + f);
		}

		inline FVECTOR3 operator- (float f)
		{
			return FVECTOR3(x - f, y - f, z - f);
		}

		inline FVECTOR3 operator+ (const FVECTOR3 &f)
		{
			return FVECTOR3(x + f.x, y + f.y, z + f.z);
		}

		inline FVECTOR3 operator- (const FVECTOR3 &f)
		{
			return FVECTOR3(x - f.x, y - f.y, z - f.z);
		}

		float x, y, z;
	} FVECTOR3;


	/**
	* \brief 32-bit floating point 4D vector type.
	* \note This structure is compatible with the D3DXVECTOR4 type.
	*/
	typedef union FVECTOR4 
	{
		
		FVECTOR4()
		{
			r = g = b = a = 0.0f;
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
			if (da == 0) da = 255;
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


		inline FVECTOR4 operator* (float f)
		{
			return FVECTOR4( x * f, y * f, z * f, w);
		}

		inline FVECTOR4 operator/ (float f)
		{
			f = 1.0f / f;
			return FVECTOR4(x * f, y * f, z * f, w);
		}
		
		inline FVECTOR4 operator+ (float f)
		{
			return FVECTOR4(x + f, y + f, z + f, w);
		}

		inline FVECTOR4 operator- (float f)
		{
			return FVECTOR4(x - f, y - f, z - f, w);
		}

		inline FVECTOR4 operator+ (const FVECTOR4 &f)
		{
			return FVECTOR4(x + f.x, y + f.y, z + f.z, w);
		}

		inline FVECTOR4 operator- (const FVECTOR4 &f)
		{
			return FVECTOR4(x - f.x, y - f.y, z - f.z, w);
		}

		float data[4];
		struct {
			float x, y, z, w;
		};
		struct {
			float r, g, b, a;
		};
		struct {
			FVECTOR3 rgb;
			float a;
		};
		struct {
			FVECTOR3 xyz;
			float w;
		};
	} FVECTOR4;



	/**
	* \brief Float-valued 4x4 matrix.
	* \note This structure is compatible with the D3DXMATRIX.
	*/
	typedef union FMATRIX4 {
		FMATRIX4() {}
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
		FVECTOR4(x, y, z, w);
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

	inline float length(const FVECTOR2 &v)
	{
		return sqrt(v.x*v.x + v.y*v.y);
	}

	inline float length(const FVECTOR3 &v)
	{
		return sqrt(v.x*v.x + v.y*v.y + v.z*v.z);
	}

	inline float saturate(float x)
	{
		return min(1, max(0, x));
	}

	inline FVECTOR3 saturate(FVECTOR3 &v)
	{
		return FVECTOR3(saturate(v.x), saturate(v.y), saturate(v.z));
	}

	typedef struct {
		FVECTOR2	pos;
		DWORD		color;
	} TriangleVtx;


	typedef struct {
		OBJHANDLE		hVessel;		///< Handle to a vessel that was clicked
		int				mesh;			///< Mesh index that was clicked
		int				group;			///< Mesh group index that was clicked
		float			dist;			///< Distance from a camera to a click point
		FVECTOR3		normal;			///< Normal vector in local vessel coordinates
		FVECTOR3		pos;			///< Position in local vessel coordinates
	} PickData;




	// OBSOLETE BEGIN -------------------------------------------------
	//
	
	inline FVECTOR4 _FVECTOR4(VECTOR3 &v, float _w = 0.0f)
	{
		FVECTOR4 q(float(v.x), float(v.y), float(v.z), _w);
		return q;
	}

	inline FVECTOR4 _FVECTOR4(float r, float g, float b, float a)
	{
		FVECTOR4 q(r, g, b, a);
		return q;
	}

	inline FVECTOR2 _FVECTOR2(float x, float y)
	{
		return FVECTOR2(x, y);
	}
	
	//
	// OBSOLETE END ----------------------------------------------------
}

/// \brief Custom swapchain handle
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



// ===========================================================================
/**
* \class gcCore
* \brief Core class for graphics services 
*/
// ===========================================================================

class gcCore
{

public:

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
	* \brief Flip Backbuffer to a front
	* \param hSwap Handle to a swap object.
	* \return A Handle to a rendering surface (i.e. backbuffer)
	*/
	virtual SURFHANDLE	GetRenderTarget(HSWAP hSwap);

	/**
	* \brief Release a swap object after it's no longer needed.
	* \param hSwap Handle to a swap object.
	*/
	virtual void		ReleaseSwap(HSWAP hSwap);
	//@}


	// ===========================================================================
	/// \name gcGUI Access and management functions
	// ===========================================================================
	//@{
	virtual void		ConvertSurface(SURFHANDLE hSurf, DWORD attrib);
	virtual DWORD		GetSurfaceAttribs(SURFHANDLE hSurf, bool bCreation);
	virtual HWND		GetRenderWindow();
	//@}


	// ===========================================================================
	/// \name gcGUI Access and management functions
	// ===========================================================================
	//@{
	/*
	virtual HNODE		RegisterApplication(const char *label, HWND hDlg, DWORD color, DWORD docked);
	virtual HNODE		RegisterSubsection(HNODE hNode, const char *label, HWND hDlg, DWORD color);
	virtual void		UnRegister(HNODE hNode);
	virtual void		UpdateStatus(HNODE hNode, const char *label, HWND hDlg, DWORD color);
	virtual bool		IsOpen(HNODE hNode);
	*/
	//@}
};
