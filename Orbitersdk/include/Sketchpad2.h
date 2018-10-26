
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

using namespace oapi;

/**
* \file Sketchpad2.h
* \brief 2-D surface drawing support interface extension 2.
*/


/// \defgroup SkpMeshFlags SketchMesh render flags
///@{
#define	MF_SMOOTH_SHADE		0x1					///< Perform smooth shading (i.e. shallow angles gets darkened) 
#define	MF_CULL_NONE		0x2					///< Do not perform front/back face culling
///@}

/// \defgroup FntCreation Font creation flags
///@{
#define FNT_BOLD			0x1 
#define FNT_ITALIC			0x2
#define FNT_UNDERLINE		0x4
#define FNT_CRISP			0x8					///< Create a crisp font without anti-aliasing
// Additional code pages
#define FNT_GREEK			0x1000				///< Request additional code page
///@}

namespace oapi {

	/**
	* \brief Sketchpad2 adds some additional features to an existing Sketchpad interface.
	*/
	class SKP2FUNC Sketchpad2 : public Sketchpad
	{

	public:

		enum SkpView { 
			ORTHO = 0,				///< Default orthographic projection
			USER = 1				///< User defined setup via ViewMatrix() and ProjectionMatrix()
		};

		/**
		* \brief Sketchpad2 constructor.
		*/
		Sketchpad2(SURFHANDLE s) : Sketchpad(s) {}


		/**
		* \brief Sketchpad2 destructor.
		*/
		virtual	~Sketchpad2() {}


		/**
		* \brief Get a render surface size in pixels
		* \param size Pointer to SIZE structure receiving the size 
		*/
		virtual void GetRenderSurfaceSize(LPSIZE size) { assert(false); }


		/**
		* \brief Setup a quick pen, removes any other pen from use. Set to zero to disable a pen from use.
		* \param color Pen color in 0xAABBGGRR
		* \param width Pen width in pixels
		* \param style 0 = Disabled, 1 = Solid, 2 = Dashed
		*/
		virtual void QuickPen(DWORD color, float width = 1.0f, DWORD style = 1) { assert(false); }


		/**
		* \brief Setup a quick brush, removes any other brush from use. Set to zero to disable a brush from use.
		* \param color Brush color in 0xAABBGGRR
		*/
		virtual void QuickBrush(DWORD color) { assert(false); }


		/**
		* \brief Set up a global line width scale factor
		* \param width A line width scale factor. (Default 1.0f)
		* \param pattern Line pattern scale factor. (Default 1.0f)
		*/
		virtual void SetGlobalLineScale(float width = 1.0f, float pattern = 1.0f) { assert(false); }


		/**
		* \brief Set up a global world transformation matrix. 
		* \param pWT A pointet to MATRIX4, NULL to reset default settings.
		* \note This function will conflict and resets any settings set by SetOrigin(). Setting to NULL does not restore SetOrigin(). 
		* \note Everything else except Pixel() is transformed including CopyRect() and Text().
		* \warning Graphics results from a CopyRect() and Text() can be blurry when non-default SetViewProjectionMatrix or SetWorldTransform is in use
		*		due to source-target pixels miss aligments.
		*/
		virtual	void SetWorldTransform(const FMATRIX4 *pWT = NULL) { assert(false); }


		/**
		* \brief Get/Set a View matrix
		*/
		virtual	FMATRIX4 *ViewMatrix() { assert(false); return NULL; }


		/**
		* \brief Get/Set a Projection matrix
		*/
		virtual	FMATRIX4 *ProjectionMatrix() { assert(false); return NULL; }


		/**
		* \brief Get combined view projection matrix. [Read only]
		*/
		virtual	const FMATRIX4 *GetViewProjectionMatrix() { assert(false); return NULL; }


		/**
		* \brief Set an active view mode. Switch between modes doesn't reset the view matrices and setups.
		* \param mode, SkpView mode setting.
		*/
		virtual void SetViewMode(SkpView mode = ORTHO) { assert(false); }


		/**
		* \brief Set up a global world transformation matrix.
		* \param scale Graphics scale factor.
		* \param rot Rotation angle [rad]
		* \param ctr Pointer to a IVECTOR containing a rotation center or NULL for origin.
		* \param trl Pointer to a IVECTOR containing a translation or NULL.
		* \note This function will conflict and resets any settings set by SetOrigin(). Setting to NULL does not restore SetOrigin().
		* \note Everything else except Pixel() is transformed including CopyRect() and Text().
		* \warning Graphics results from a CopyRect() and Text() can be blurry when non-default SetViewProjectionMatrix or SetWorldTransform is in use
		*		due to source-target pixels miss aligments.
		*/
		virtual void SetWorldTransform2D(float scale = 1.0f, float rot = 0.0f, IVECTOR2 *ctr = NULL, IVECTOR2 *trl = NULL) { assert(false); }


		/**
		* \brief Set up a screen space clip rectangle. Usefull when need to draw in a smaller sub section of the render target.
		* \param pClip A pointer to clipping rectangle, Set to NULL to disable clipping.
		*/
		virtual void ClipRect(const LPRECT pClip = NULL) { assert(false); }


		/**
		* \brief Set up a world space clip cone to clip pixels within it. Does not work with orthographic projection.
		* \param idx Index of the clipper object. Valids are "0" and "1".
		* \param pPos a pointer to a unit vector containing cone direction in camera centric frame, Set to NULL to disable clipping.
		* \param angle cosine of the half-angle of the cone.
		* \param dist clip-plane distance, clipping only occur beyond this distance.
		* \note This function is provided due to reasons that z-buffering doesn't really work in all cases.
		*/
		virtual void Clipper(int idx, const VECTOR3 *pPos = NULL, double cos_angle = 0.0, double dist = 0.0) { assert(false); }


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
		* \param hTex a texture override, render with this texture regardless what ever is specified in the mesh.
		* \return Number of groups in the mesh or -1 if the group index is out of range.
		* \note Use SetWorldTransform() to move, rotate and scale the object.
		* \note Final color = Texture Color * Material Color * Pen Color
		* \sa gcLoadSketchMesh, gcDeleteSketchMesh;
		*/
		virtual int DrawSketchMesh(SKETCHMESH hMesh, DWORD grp, DWORD flags = MF_SMOOTH_SHADE, SURFHANDLE hTex = NULL) { assert(false); return -2; }


		/**
		* \brief Draws a template mesh group (from a system memory) in the render target.
		* \param hMesh Pointer to mesh containing the geometry.
		* \param grp Group index to draw.
		* \param flags SkpMeshFlags
		* \param hTex a texture override, render with this texture regardless what ever is specified in the mesh.
		* \return Number of groups in the mesh or -1 if the group index is out of range.
		* \note Use SetWorldTransform() to move, rotate and scale the object.
		* \note Vertex count should be kept low due to rendering from a system memory. This is a good choise when 
		* a vertex data changes in a per frame basis. Bad for static vertex data.
		* \note Final color = Texture Color * Material Color * Pen Color
		* \sa DrawSketchMesh
		*/
		virtual int DrawMeshGroup(MESHHANDLE hMesh, DWORD grp, DWORD flags = MF_SMOOTH_SHADE, SURFHANDLE hTex = NULL) { assert(false); return -2; }


		/**
		* \brief Copy 'Blit' a rectangle
		* \param hSrc Source surface handle
		* \param src Source rectangle, (or NULL for whole surface)
		* \param tx Target x-coordinate
		* \param ty Target y-coordinate
		* \note Can alpha-blend and mirror by a use of negative width/height in source rectangle
		*/
		virtual void CopyRect(SURFHANDLE hSrc, const LPRECT src, int tx, int ty) { assert(false); }


		/**
		* \brief Copy 'Blit' a rectangle
		* \param hSrc Source surface handle
		* \param src Source rectangle, (or NULL for whole surface)
		* \param tgt Target rectangle, (must be defined) 
		* \note Can alpha-blend and mirror by a use of negative width/height in source rectangle
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
		* \note Can alpha-blend, should be able to mirror via negative scale factor.
		*/
		virtual void RotateRect(SURFHANDLE hSrc, const LPRECT src, int cx, int cy, float angle = 0.0f, float sw = 1.0f, float sh = 1.0f) { assert(false); }


		/**
		* \brief Copy 'Blit' a rectangle using a color-key stored in a source surface.
		* \param hSrc Source surface handle
		* \param src Source rectangle, (or NULL for whole surface)
		* \param tx Target x-coordinate
		* \param ty Target y-coordinate
		* \note ColorKey() does not work properly with SetWorldTransform() due to color interpolation 
		* \note Can mirror by a use of negative width/height in source rectangle
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
		* \brief Draw a pre-created polyline
		* \param hPoly Handle to a polyline object
		* \param flags (reserved for later use, set to zero for now)
		* \sa gcCreatePoly, gcDeletePoly
		*/
		virtual void DrawPoly(HPOLY hPoly, DWORD flags = 0) { assert(false); }


		/**
		* \brief Draw a list of independent lines. 0-1, 2-3, 4-5,...
		* \param pt list of vertex points.
		* \param nlines number of lines to draw
		*/
		virtual void Lines(FVECTOR2 *pt1, int nlines) { assert(false); }
	};






#define SKP3_PRM_GAMMA				1
#define SKP3_PRM_NOISE				2


	/**
	* \brief Sketchpad3 adds some additional features to an existing Sketchpad interface.
	*/
	class SKP2FUNC Sketchpad3 : public Sketchpad2
	{

	public:

		/**
		* \brief Sketchpad3 constructor.
		*/
		Sketchpad3(SURFHANDLE s) : Sketchpad2(s) {}


		/**
		* \brief Sketchpad2 destructor.
		*/
		virtual	~Sketchpad3() {}


		/**
		* \brief Get a read only pointer to current ColorMatrix. 
		* \sa SetColorMatrix, SetBrightness
		*/
		virtual const FMATRIX4 *GetColorMatrix() { assert(false); return NULL; }


		/**
		* \brief Set a ColorMatrix for color correrctions. Reset to default by passing NULL pointer
		* \param pMatrix Pointer to a matrix or NULL.
		* \sa GetColorMatrix, SetBrightness
		*/
		virtual void SetColorMatrix(FMATRIX4 *pMatrix = NULL) { assert(false); }


		/**
		* \brief Automatically set a ColorMatrix for brightness control. NULL to restore default settings.
		* \param pBrightness Pointer into a float values color vector, or NULL.
		* \sa GetColorMatrix, SetColorMatrix
		*/
		virtual void SetBrightness(FVECTOR4 *pBrightness = NULL) { assert(false); }


		/**
		* \brief Get a render configuration setting or "effect".
		* \param param A setting ID to get.
		* \note Valid ID Flags:
		* \note SKP3_PRM_GAMMA, Get Gamma correction value (.rgb)
		* \note SKP3_PRM_NOISE, Get Noise configuration. Noise color in (.rgb) and (.a) controls blending between input color (.rgb) and target color. 
		* \sa SetRenderParam
		*/
		virtual FVECTOR4 GetRenderParam(int param) { assert(false); return _FVECTOR4(0, 0, 0, 0); }


		/**
		* \brief Set a render configuration paramater or "effect".
		* \param param A setting ID to set, or NULL to disable effect from use.
		* \sa SetRenderParam
		*/
		virtual void SetRenderParam(int param, FVECTOR4 *data = NULL) { assert(false); }
	};

} // namespace oapi

#endif