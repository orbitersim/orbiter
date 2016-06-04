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

#ifndef __OGCI_H
#define __OGCI_H

#include "OrbiterAPI.h"
#include "DrawAPI.h"
#include "gcConst.h"

#define OGCIFN __cdecl

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
#define OGCI_FOR_2010P1
#endif


				// -----------------------------------------------------------------------------------------------------------------------------------
				// Some Generic functions
				// -----------------------------------------------------------------------------------------------------------------------------------

				/**
				* \brief Initialize Orbiter Graphics Client Interface (OGCI) 
				* \return true, if connection to a graphics client was established, false otherwise.
				*/
bool			gcInitialize();

				/**
				* \brief Check if OGCI is initialized and operational
				* \return true, if the interface is initialized and operational.
				* \note Always returns flase if the ogciInitialize() is never called or the ogciInitialize() has failed.
				*/
bool			gcEnabled();

				/**
				* \brief Get a client id DWORD
				* \return Client id DWORD (e.g. 'D3D9') (not 0xD3D9)
				*/
DWORD			gcClientID();

				/**
				* \brief Get Surface Attributes (e.g. OAPISURFACE_TEXTURE)
				* \param hSurf, handle to a surface
				* \param bCreation, if true return creation time attributes, if false return current attributes
				* \return Surface attributes
				*/
DWORD			gcGetSurfaceAttribs(SURFHANDLE hSurf, bool bCreation=false);

				/**
				* \brief Convert an existing surface to an other type.
				* \param hSurf, handle to a surface
				* \param attrib, new attributes
				*/
void			gcConvertSurface(SURFHANDLE hSurf, DWORD attrib);

				/**
				* \brief Auto-Generate a full chain of mipmaps from the main level.
				* \param hSurface, handle to a surface
				* \return false if an error occured, true otherwise.
				* \note Surface must be created atleast with (OAPISURFACE_TEXTURE | OAPISURFACE_MIPMAPS)
				* \note Exact attribute requirements/conflicts are unknown.
				*/
bool			gcGenerateMipMaps(SURFHANDLE hSurface);

		
				/**
				* \brief This function will register a custom render callback function
				* \param proc, function to be called when render event occur
				* \param id, render event id
				* \param pParam a pointer to user data (to a class for an example)
				* \return false if an error occured, true otherwise.
				*/
bool			gcRegisterRenderProc(__gcRenderProc proc, DWORD id, void *pParam);



				// -----------------------------------------------------------------------------------------------------------------------------------
				// Custom Camera Interface
				// -----------------------------------------------------------------------------------------------------------------------------------

				/**
				* \brief Delete/Release a custom camera.
				* \param hCam, camera handle to delete.
				* \return zero, or an error code if the camara didn't work properly.
				* \note Always delete all cameras bound to a render surface before releasing the rendering surface it-self.
				*/
int				gcDeleteCustomCamera(CAMERAHANDLE hCam);

				/**
				* \brief Toggle camera on and off
				* \param hCam, camera handle to toggle
				* \param bOn, true to turn on the camera.
				* \note If multible cameras are sharing the same rendering surface. Flickering will occur if more than one camera is turned on. 
				*/
void			gcCustomCameraOnOff(CAMERAHANDLE hCam, bool bOn);

				/**
				* \brief Create a new custom camera that can me used to render views into a surfaces and textures
				* \param hCam, camera handle to modify an existing camera or, NULL
				* \param hVessel, handle to a vessel where the camera is attached to.
				* \param vPos, camara position in vessel's local coordinate system
				* \param vDir, camara direction in vessel's local coordinate system. [Unit Vector]
				* \param vUp, camara up vector. Must be perpendicular to vDir. [Unit Vector]
				* \param dFow, camera field of view in radians
				* \param hSurf, rendering surface. Must be created atleast with OAPISURFACE_RENDER3D | OAPISURFACE_RENDERTARGET. Multible cameras can share the same surface.
				* \param dwFlags, Flags to controls what is drawn and what is not.
				* \return Camera handle, or NULL if an error occured or if the custom camera interface is disabled.
				* \note Camera count is unlimited. 
				* \note Only a cameras attached to currently active vessel are operational and recodring.
				* \note Having multible cameras active at the same time doesn't impact in a frame rate, however, camera refresh rates are reduced.
				*/
CAMERAHANDLE	gcSetupCustomCamera(CAMERAHANDLE hCam, OBJHANDLE hVessel, VECTOR3 &vPos, VECTOR3 &vDir, VECTOR3 &vUp, double dFov, SURFHANDLE hSurf, DWORD dwFlags=0xFF);


				// -----------------------------------------------------------------------------------------------------------------------------------
				// Custom Sketchpad Goodies
				// -----------------------------------------------------------------------------------------------------------------------------------

				/**
				* \brief Get the sketchpad version
				* \param pSkp, handle to a sketchpad interface.
				* \return 1 or 2
				*/
int				gcSketchpadVersion(oapi::Sketchpad *pSkp);

SKETCHMESH		gcLoadSketchMesh(const char *name);
void			gcDeleteSketchMesh(SKETCHMESH hMesh);



void			gcWorldMatrix(MATRIX4 *mat, const VECTOR3 &pos, const VECTOR3 &x, const VECTOR3 &z, double scale);
bool			gcWorldToScreenSpace(const VECTOR3 &rdir, oapi::IVECTOR2 *pt, float clip = 1.0f);

#endif