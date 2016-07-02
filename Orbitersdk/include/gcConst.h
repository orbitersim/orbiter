// =================================================================================================================================
// The MIT Lisence:
//
// Copyright (C) 2014 - 2016 Jarmo Nikkanen
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

/**
* \file gcConst.h
* \brief Structures and definations
*/

#pragma once

/// \defgroup RenderProc Specify a render HUD callback function
///@{
#define RENDERPROC_DELETE		0x0000	///< Unregister/Remove existing callback 
#define RENDERPROC_HUD_1ST		0x0001	///< Register a HUD callback to draw under Orbiter's main HUD
#define RENDERPROC_HUD_2ND		0x0002	///< Register a HUD callback to draw over Orbiter's main HUD
#define RENDERPROC_PLANETARIUM	0x0003	///< Register a HUD callback to draw into a planetarium view using perspective projection
///@}


/// \defgroup PolyFlags Poly object creation and update flags
///@{
#define	PF_CONNECT		0x01	///< Connect line endpoints forming a loop
///@}


namespace oapi {

	/**
	* \brief 32-bit floating point 2D vector type.
	* \note This structure is compatible with the D3DXVECTOR2 type.
	*/
	typedef struct {
		float x, y;
	} FVECTOR2;

	/**
	* \brief 32-bit floating point 4D vector type.
	* \note This structure is compatible with the D3DXVECTOR2 type.
	*/
	typedef union { 
		float data[4];
		struct {
			float x, y, z, w;
		};
		struct {
			float r, g, b, a;
		};
	} FVECTOR4;


	inline FVECTOR4 _FVECTOR4(VECTOR3 &v, float _w = 0.0f) 
	{ 
		FVECTOR4 q = { float(v.x), float(v.y), float(v.z), _w }; 
		return q;
	}

	inline FVECTOR4 _FVECTOR4(float r, float g, float b, float a) 
	{ 
		FVECTOR4 q = { r, g, b, a }; 
		return q;
	}

	inline FVECTOR2 _FVECTOR2(float x, float y) 
	{ 
		FVECTOR2 q = { x, y }; 
		return q;
	}
		

	/**
	* \brief Float-valued 4x4 matrix.
	* \note This structure is compatible with the D3DXMATRIX.
	*/
	typedef union {
		float data[16];
		struct { FVECTOR4 _x, _y, _z, _p; };
		struct { float m11, m12, m13, m14, m21, m22, m23, m24, m31, m32, m33, m34, m41, m42, m43, m44; };
	} FMATRIX4;
}

/// \brief Custom camera handle
typedef void * CAMERAHANDLE;
/// \brief Sketchmesh handle
typedef void * SKETCHMESH;
/// \brief Poly object handle
typedef void * HPOLY;
/// \brief Render HUD and Planetarium callback function 
typedef void(__cdecl *__gcRenderProc)(oapi::Sketchpad *pSkp, void *pParam);


namespace oapi {

	inline FVECTOR2 unit(FVECTOR2 &v)
	{
		float f = 1.0f / sqrt(v.x*v.x + v.y*v.y);
		return _FVECTOR2( v.x*f, v.y*f );
	}

	inline FVECTOR2 operator* (const FVECTOR2 &v, float f)
	{
		return _FVECTOR2( v.x * f, v.y * f );
	}

	inline FVECTOR4 operator* (const FVECTOR4 &v, float f)
	{
		return _FVECTOR4( v.x * f, v.y * f, v.z * f, v.w * f );
	}
}


