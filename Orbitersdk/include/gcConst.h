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



/**
* \brief Flags for Poly object creation and update
*/
enum PolyFlags { 
	NONE =		0x0,			
	CONNECT =	0x1		///< \details Connect line end-points forming a loop
};

namespace oapi {

	/**
	* \brief 32-bit floating point 2D vector type.
	* \note This structure is compatible with the D3DXVECTOR2 type.
	*/
	typedef struct {
		float x, y;
	} FVECTOR2;


	/**
	* \brief Float-valued 4x4 matrix.
	* \note This structure is compatible with the D3DXMATRIX.
	*/
	typedef union {
		float data[16];
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
