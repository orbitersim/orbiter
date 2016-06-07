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
				* \brief Initialize Graphics Client interface (gc) 
				* \return true, if connection to a graphics client was established, false otherwise.
				*/
bool			gcInitialize();

				/**
				* \brief Check if GC is initialized and operational.
				* \return true, if the interface is initialized and operational.
				* \note Always returns flase if the gcInitialize() is never called or the gcInitialize() has failed.
				*/
bool			gcEnabled();

				/**
				* \brief Get a client id DWORD, if a client is present
				* \return Client id DWORD (e.g. 'D3D9') or zero
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
				* \return Currently returns 1 or 2
				*/
int				gcSketchpadVersion(oapi::Sketchpad *pSkp);

				/**
				* \brief Load a mesh from a harddrive to be used with Sketchpad2::SketchMesh
				* \param name Name of the mesh file without ".msh" identifier.
				* \sa gcDeleteSketchMesh
				* \note SKETCHMESH handle isn't compatible with MESHHANDLE nor DEVMESHHANDLE.
				*/
SKETCHMESH		gcLoadSketchMesh(const char *name);

				/**
				* \brief Delete a mesh previously loaded with gcLoadSketchMesh
				* \sa gcLoadSketchMesh
				*/
void			gcDeleteSketchMesh(SKETCHMESH hMesh);

				/**
				* \brief Create or Update a polyline composed form piecewise straight segments. Support for filled polygon objects may be added later.
				* \param hPoly Handle to a poly object to be updated or NULL to create a new one.
				* \param pt list of vertex points.
				* \param npt number of points in the list.
				* \param PolyFlags additional flags
				* \sa gcDeletePoly, Sketchpad2::DrawPoly
				* \note Poly objects should be created during initialization not for every frame or update. Updating existing (pre created) poly object is pretty fast.
				* \note During update number of points must be equal or smaller than during initial creation of poly object.
				*/
HPOLY			gcCreatePoly(HPOLY hPoly, const FVECTOR2 *pt, int npt, PolyFlags flags = NONE);

				/**
				* \brief Deletes a polyline created with gcCreatePolyPolyline()
				* \param hPoly Handle to a polyline to be deleted
				* \sa gcCreatePolyline
				*/
void			gcDeletePoly(HPOLY hPoly);


				// -----------------------------------------------------------------------------------------------------------------------------------
				// Other Helper Function
				// -----------------------------------------------------------------------------------------------------------------------------------

				/**
				* \brief Alters objects position. Matrix must be initially valid.
				* \param mat [in/out] Pointer to a matrix to change
				* \param pos New position
				*/
void			gcSetTranslation(FMATRIX4 *mat, const VECTOR3 &pos);

				/**
				* \brief Creates a world transformation matrix
				* \param mat [out] Pointer to a matrix
				* \param pos Objects position relative to a camera in ecliptic frame
				* \param x X-axis, major axis [unit vector]
				* \param z Z-axis, minor axis [unit vector]
				* \param scale a sacle factor (default 1.0)
				*/
void			gcWorldMatrix(FMATRIX4 *mat, const VECTOR3 &pos, const VECTOR3 &x, const VECTOR3 &z, double scale = 1.0);

				/**
				* \brief Compute a screen space location for a point in camera centric ecliptic frame.
				* \param rdir a position or a direction vector.
				* \param pt [out] screen space position in pixels relative to upper-left corner.
				* \param clip Visibility check. Value 1.0 uses actual screen broders where as other values can increase or decrease clipping region size.
				* \return true if the point is in the clipping rectanble, false otherwise in which case pt remains unchanged.
				*/
bool			gcWorldToScreenSpace(const VECTOR3 &rdir, oapi::IVECTOR2 *pt, float clip = 1.0f);

#endif