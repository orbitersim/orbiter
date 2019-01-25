// =================================================================================================================================
// The MIT Lisence:
//
// Copyright (C) 2014 - 2019 Jarmo Nikkanen
//
// Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation 
// files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, 
// modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software 
// is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
// OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
// LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR
// IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
// =================================================================================================================================

#include "OrbiterAPI.h"
#include "DrawAPI.h"

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
	* \brief 32-bit floating point #D vector type.
	* \note This structure is compatible with the D3DXVECTOR2 type.
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
	* \note This structure is compatible with the D3DXVECTOR2 type.
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
	} FVECTOR4;


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
		return FVECTOR2(x,y);
	}
		

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


	typedef struct {
		FVECTOR2	pos;
		DWORD		color;
	} TriangleVtx;
}

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

	inline FVECTOR2 unit(const FVECTOR2 &v)
	{
		float f = 1.0f / sqrt(v.x*v.x + v.y*v.y);
		return _FVECTOR2( v.x*f, v.y*f );
	}

	inline float dot(const FVECTOR2 &v, const FVECTOR2 &w)
	{
		return v.x*w.x + v.y*w.y;
	}

	inline float length(const FVECTOR2 &v)
	{
		return sqrt(v.x*v.x + v.y*v.y);
	}

	inline float saturate(float x)
	{
		return min(1, max(0, x));
	}

	inline FVECTOR3 saturate(FVECTOR3 &v)
	{
		return FVECTOR3(saturate(v.x), saturate(v.y), saturate(v.z));
	}
}


