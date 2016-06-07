
#ifndef __SKETCHPAD2_H
#define __SKETCHPAD2_H

#ifdef D3D9CLIENT_EXPORTS
#define SKP2FUNC
#else
#define SKP2FUNC __declspec(dllimport)
#endif

#include "assert.h"
#include "DrawAPI.h"
#include "gcConst.h"


enum SkpMeshFlags { 
	SMOOTH_SHADE = 0x1,		///< Perform smooth shading (i.e. shallow angles gets darkened) 
	CULL_NONE = 0x2			///< Do not perform front/back face culling
};

namespace oapi {


	class SKP2FUNC Sketchpad2 : public Sketchpad
	{

	public:


							Sketchpad2(SURFHANDLE s) : Sketchpad(s) {}

		virtual				~Sketchpad2() {}


		/**
		* \brief Setup a quick pen, removes any other pen from use
		* \param color Pen color in 0xAABBGGRR
		* \param width Pen width in pixels
		* \param style 0 = Disabled, 1 = Solid, 2 = Dashed
		* \param factor A width scale factor. (Default 1.0f)
		*/
		virtual void QuickPen(DWORD color, float width = 1.0f, DWORD style = 1) { assert(false); }


		/**
		* \brief Setup a quick brush, removes any other brush from use
		* \param color Brush color in 0xAABBGGRR
		*/
		virtual void QuickBrush(DWORD color) { assert(false); }


		/**
		* \brief Set up a global line width scale factor
		* \param factor A width scale factor. (Default 1.0f)
		*/
		virtual void SetGlobalLineScale(float factor = 1.0f) { assert(false); }


		/**
		* \brief Set combined view and projection matrix.
		* \param pVP a pointer to an array of 16 floats or NULL to restore a default projection.
		*/
		virtual void SetViewProjectionMatrix(const FMATRIX4 *pVP = NULL) { assert(false); }


		/**
		* \brief Set up a global world transformation matrix. 
		* \param pWT A pointet to MATRIX4, NULL to reset default settings.
		* \note This function will conflict and resets any settings set by SetOrigin(). Setting to NULL does not restore SetOrigin(). 
		* \note Everything else except Pixel() is transformed including CopyRect() and Text().
		* \warning Graphics results from a CopyRect() and Text() can be blurry when non-default SetProjection() or SetWorldTransform() is in use
		*		due to source-target pixels miss aligments.
		*/
		virtual	void SetWorldTransform(const FMATRIX4 *pWT = NULL) { assert(false); }


		/**
		* \brief Set up a global world transformation matrix.
		* \param scale Graphics scale factor.
		* \param rot Rotation angle [rad]
		* \param ctr Pointer to a IVECTOR containing a rotation center or NULL for origin.
		* \param trl Pointer to a IVECTOR containing a translation or NULL.
		* \note This function will conflict and resets any settings set by SetOrigin(). Setting to NULL does not restore SetOrigin().
		* \note Everything else except Pixel() is transformed including CopyRect() and Text().
		* \warning Graphics results from a CopyRect() and Text() can be blurry when non-default SetProjection() or SetWorldTransform() is in use
		*		due to source-target pixels miss aligments.
		*/
		virtual void SetWorldTransform2D(float scale = 1.0f, float rot = 0.0f, IVECTOR2 *ctr = NULL, IVECTOR2 *trl = NULL) { assert(false); }


		/**
		* \brief Set up a screen space clip rectangle. Usefull when need to draw in a smaller sub section of the render target.
		* \param pClip A pointer to clipping rectangle, Set to NULL to disable clipping.
		*/
		virtual void ClipRect(const LPRECT pClip = NULL) { assert(false); }


		/**
		* \brief Set up a world space clip sphere to clip pixels behind it. Does not work with orthographic projection.
		* \param pPos a pointer to a vector containing sphere position in camera centric ecliptic frame, Set to NULL to disable clipping.
		* \param rad Radius of the sphere.
		*/
		virtual void ClipSphere(const VECTOR3 *pPos = NULL, double rad = 0.0) { assert(false); }


		/**
		* \brief Enable a use of depth buffer. (currently unimplemented)
		* \param bEnable Toggle depth buffer.
		*/
		virtual void DepthEnable(bool bEnable) { assert(false); }


		/**
		* \brief Draws a Mesh group in the render target. Usefull in rendering of complex static shapes and polygons.
		* \param hMesh Pointer to device specific mesh containing the geometry.
		* \param grp Group index to draw.
		* \param flags SkpMeshFlags
		* \return Number of groups in the mesh or -1 if the group index is out of range.
		* \note Use SetWorldTransform() to move, rotate and scale the object.
		* \note Final color = Texture Color * Material Color * Pen Color
		* \sa gcLoadSketchMesh(), gcDeleteSketchmesh();
		*/
		virtual int DrawMeshGroup(SKETCHMESH hMesh, DWORD grp, SkpMeshFlags flags = SMOOTH_SHADE) { return -2; }


		/**
		* \brief Copy 'Blit' a rectangle
		* \param hSrc Source surface handle
		* \param src Source rectangle, (or NULL for whole surface)
		* \param tx Target x-coordinate
		* \param ty Target y-coordinate
		* \note Can alpha-blend
		*/
		virtual void CopyRect(SURFHANDLE hSrc, const LPRECT src, int tx, int ty) { assert(false); }


		/**
		* \brief Copy 'Blit' a rectangle
		* \param hSrc Source surface handle
		* \param src Source rectangle, (or NULL for whole surface)
		* \param tgt Target rectangle, (must be defined) 
		* \note Can alpha-blend
		*/
		virtual void StretchRect(SURFHANDLE hSrc, const LPRECT src, const LPRECT tgt) { assert(false); }


		/**
		* \brief Copy 'Blit' a rectangle with rotation and scaling
		* \param hSrc Source surface handle
		* \param src Source rectangle, (or NULL for whole surface)
		* \param cx Target center x-coordinate
		* \param cy Target center y-coordinate
		* \param angle Rotation angle in radians
		* \param sw Width scale factor
		* \param sh Height scale factor
		* \note Does not change or effect in SetWorldTransform()
		* \note Can alpha-blend
		*/
		virtual void RotateRect(SURFHANDLE hSrc, const LPRECT src, int cx, int cy, float angle = 0.0f, float sw = 1.0f, float sh = 1.0f) { assert(false); }


		/**
		* \brief Copy 'Blit' a rectangle using a color-key stored in a source surface.
		* \param hSrc Source surface handle
		* \param src Source rectangle, (or NULL for whole surface)
		* \param tx Target x-coordinate
		* \param ty Target y-coordinate
		* \note ColorKey() does not work properly with SetWorldTransform() 
		*/
		virtual void ColorKey(SURFHANDLE hSrc, const LPRECT src, int tx, int ty) { assert(false); }


		/**
		* \brief Write a line of text using text scaling and rotation
		* \param x x-coordinate of the text alignment point
		* \param y y-coordinate of the text alignment point
		* \param str pointer to NULL terminated string to write.
		* \param scale Text scale factor (0.0f to 1.0f)
		* \param angle Rotation angle in radians.
		* \sa Text()
		* \note Rotation and scaling can result a blurry text if used with a small fonts. Rotation of ±PI/2 or PI should work fine.
		* \note Rotation spefified during font creation is ignored in this function.
		*/
		virtual void TextEx(float x, float y, const char *str, float scale = 1.0f, float angle = 0.0f) { assert(false); }


		/**
		* \brief Draw a pre-created polyline (or polygon later in the future, maybe)
		* \param hPoly Handle to a poly object
		* \param flags (reserved for later use, set to zero for now)
		* \sa gcCreatePoly, gcDeletePoly
		*/
		virtual void DrawPoly(HPOLY hPoly, DWORD flags = 0) { assert(false); }	
	};

} // namespace oapi

#endif