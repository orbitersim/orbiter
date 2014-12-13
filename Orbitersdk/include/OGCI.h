// =================================================================================================================================
// The MIT Lisence:
//
// Copyright (C) 2014 Jarmo Nikkanen
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

#ifndef __OGCI_H
#define __OGCI_H

#include "OrbiterAPI.h"

typedef void * CAMERAHANDLE;

#define OGCIFN __cdecl

#define SKETCHPAD_NONE			 0x0
#define SKETCHPAD_GDI			 0x1
#define SKETCHPAD_DIRECTX		 0x2


#ifndef OAPISURFACE_TEXTURE

#define OAPISURFACE_TEXTURE      0x0001 ///< Surface can be used as a texture (e.g. by associating it with a mesh)
#define OAPISURFACE_RENDERTARGET 0x0002 ///< Surface can be rendered to by the graphics device
#define OAPISURFACE_GDI          0x0004 ///< A HDC context can be requested from the surface for GDI drawing
#define OAPISURFACE_SKETCHPAD    0x0008 ///< A Sketchpad context can be requested from the surface for Sketchpad drawing
#define OAPISURFACE_MIPMAPS      0x0010 ///< Create a full chain of mipmaps for the surface. If loaded from file, add any missing mipmap levels
#define OAPISURFACE_NOMIPMAPS    0x0020 ///< Don't create mipmaps. If loaded from file, ignore any mipmap levels present
#define OAPISURFACE_ALPHA        0x0040 ///< Create an alpha channel for the surface. If loaded from file, add an alpha channel if required
#define OAPISURFACE_NOALPHA      0x0080 ///< Don't create an alpha channel. If loaded from file, strip any existing alpha channel
#define OAPISURFACE_UNCOMPRESS   0x0100 ///< Create an uncompressed surface. If loaded from file, uncompress if required.
#define OAPISURFACE_SYSMEM       0x0200 ///< Create the surface in system (host) memory
#define OAPISURFACE_RENDER3D     0x0400 ///< Create a surface that can act as a target for rendering a 3D scene

typedef SURFHANDLE (OGCIFN *_oapiCreateSurfaceEx)(DWORD  width, DWORD  height, DWORD  attrib);   

#define OGCI_FOR_2010P1
#endif


// Function Prototypes
// -------------------------------------------------------------------------------------------------------------------------------------

typedef DWORD (OGCIFN *_ogciClientID)();

// Camera Take Intaeface
typedef void  (OGCIFN *_ogciTakeCamera)(VECTOR3 &pos, VECTOR3 &dir, VECTOR3 &up, double fov);
typedef void  (OGCIFN *_ogciReleaseCameraTake)();

// Custom Camera Interface
typedef int   (OGCIFN *_ogciDeleteCustomCamera)(CAMERAHANDLE hCam);
typedef void  (OGCIFN *_ogciCustomCameraOnOff)(CAMERAHANDLE hCam, bool bOn);
typedef CAMERAHANDLE (OGCIFN *_ogciSetupCustomCamera)(CAMERAHANDLE hCam, OBJHANDLE hVessel, VECTOR3 &pos, VECTOR3 &dir, VECTOR3 &up, double fov, SURFHANDLE hSurf, DWORD flags);

// Custom Sketchpad Goodies
typedef int   (OGCIFN *_ogciSketchpadVersion)(oapi::Sketchpad *pSkp);
typedef void  (OGCIFN *_ogciSketchBlt)(oapi::Sketchpad *pSkp, SURFHANDLE hSrc, int tx, int ty);
typedef void  (OGCIFN *_ogciSketchBltEx)(oapi::Sketchpad *pSkp, SURFHANDLE hSrc, LPRECT s, LPRECT t, float alpha, VECTOR3 *color);
typedef void  (OGCIFN *_ogciSketchRotateBlt)(oapi::Sketchpad *pSkp, SURFHANDLE hSrc, LPRECT s, int tcx, int tcy, int w, int h, float angle, float alpha, VECTOR3 *color);

bool	ogciInitialize();
bool	ogciEnabled();

#endif