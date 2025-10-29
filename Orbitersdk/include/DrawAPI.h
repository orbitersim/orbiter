// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ======================================================================
//                     ORBITER SOFTWARE DEVELOPMENT KIT
// DrawAPI.h
// 2-D surface drawing support interface.
// This API defines an abstraction layer for providing drawing support
// for surfaces (e.g. lines and text). It is closely modelled on the
// Windows GDI, but provides an overload mechanism to insert different
// drawing systems.
// ======================================================================

/**
 * \file DrawAPI.h
 * \brief 2-D surface drawing support interface.
 */

#ifndef __DRAWAPI_H
#define __DRAWAPI_H

#include "OrbiterAPI.h"
#include <assert.h>
#include "MathAPI.h"

#if defined(_MSC_VER) && (_MSC_VER < 1920 ) // Microsoft Visual Studio Version 2017 and lower
#include <algorithm>
#endif
/// \brief Poly object handle
typedef void* HPOLY;


namespace oapi {


// ======================================================================
// class oapi::DrawingTool
// ======================================================================
/**
 * \brief Base class for various 2-D drawing resources (fonts, pens,
 *   brushes, etc.)
 */
class OAPIFUNC DrawingTool {
public:
	/**
	 * \brief Drawing tool constructor.
	 */
	DrawingTool () {}

	/**
	 * \brief Drawing tool destructor.
	 */
	virtual ~DrawingTool () {}
};

// ======================================================================
// class oapi::Font
// ======================================================================
/**
 * \brief A font resource for drawing text. A font has a defined size,
 *   typeface, slant, weight, etc. Fonts can be selected into a Sketchpad
 *   and then apply to all subsequent Text calls.
 */
class OAPIFUNC Font : public DrawingTool 
{
protected:
	/**
	 * \brief Font constructor.
	 * \param height cell or character height [pixel]
	 * \param prop proportional/fixed width flag
	 * \param face font face name
	 * \param style font decoration
	 * \param orientation text orientation [1/10 deg]
	 * \note If \e height > 0, it represents the font cell height. If height < 0,
	 *   its absolute value represents the character height.
	 * \note The \e style parameter can be any combination of the \ref Style
	 *   enumeration items.
	 * \note Overloaded font implementations should understand at least the
	 *   following generic face names: "Fixed" (fixed pitch font), "Sans"
	 *   (sans-serif font, and "Serif" (serif font) and translate them to
	 *   appropriate specific fonts, e.g. "Courier" or "Courier New" for "Fixed",
	 *   "Helvetica" or "Arial" for "Sans", and "Times" or "Times New Roman" for
	 *   "Serif".
	 * \note If a font name is not recognised, the \e prop value should be
	 *   checked. If prop==true, the default "Sans" font should be used. If
	 *   false, the default "Fixed" font should be used.
	 */
	Font(int height, bool prop, const char* face, FontStyle style = FontStyle::FONT_NORMAL, int orientation = 0) { }
	Font(int height, const char* face, int width = 0, int weight = 400, FontStyle style = FontStyle::FONT_NORMAL, float spacing = 0.0f);

public:
	/**
	 * \brief Font destructor.
	 */
	virtual ~Font () {}

	/**
	 * \brief Return the GDI handle for the font, if available.
	 * \return GDI font handle
	 * \note Non-GDI clients should not overload this method.
	 */
	virtual HFONT GetGDIFont () const { return 0; }
};


// ======================================================================
// class oapi::Pen
// ======================================================================
/**
 * \brief A pen is a resource used for drawing lines and the outlines of
 *   closed figures such as retangles, ellipses and polygons.
 */
class OAPIFUNC Pen: public DrawingTool {
protected:
	/**
	 * \brief Pen constructor.
	 * \param style line style (0=invisible, 1=solid, 2=dashed)
	 * \param width line width [pixel]
	 * \param col line colour (format: 0xBBGGRR)
	 */
	Pen (int style, int width, DWORD col) {}

public:
	/**
	 * \brief Pen destructor.
	 */
	virtual ~Pen () {}
};


// ======================================================================
// class oapi::Brush
// ======================================================================
/**
 * \brief A brush is a drawing resource for filling closed figures
 *   (rectangles, ellipses, polygons).
 */
class OAPIFUNC Brush: public DrawingTool {
protected:
	/**
	 * \brief Brush constructor.
	 * \param col brush colour (format: 0xBBGGRR)
	 */
	Brush (DWORD col) {};

public:
	/**
	 * \brief Brush destructor.
	 */
	virtual ~Brush () {}
};


// ======================================================================
// class oapi::Sketchpad
// ======================================================================
/**
 * \brief A %Sketchpad object defines an environment for drawing onto 2-D
 *   surfaces.
 *
 * It defines drawing primitives (lines, text, etc.) that can be used for
 * preparing MFD surfaces, panel elements or vessel markings.
 *
 * The drawing object is an abstract class which must be implemented by
 * derived graphics clients. An example for a DrawingObject implementation
 * is via the Windows GDI (graphics device interface).
 */
class OAPIFUNC Sketchpad 
{
public:

	enum SkpView {
		ORTHO = 0,				///< Default orthographic projection
		USER = 1				///< User defined setup via ViewMatrix() and ProjectionMatrix()
	};

	enum BlendState {
		ALPHABLEND = 0x1,				///< AlphaBlend source.color to destination.color, will retain destination alpha unchanged (if exists). 
		COPY = 0x2,						///< Copy source color and alpha to destination
		COPY_ALPHA = 0x3,				///< Copy source.alpha to destination.alpha, will retain destination color unchanged
		COPY_COLOR = 0x4,				///< Copy source.color to destination.color, will retain destination alpha unchanged
		FILTER_LINEAR = 0x00,			///< Use "linear" filter in CopyRect and similar functions
		FILTER_POINT = 0x10,			///< Use "point" filter in CopyRect and similar functions
		FILTER_ANISOTROPIC = 0x20
	};

	enum RenderParam {
		PRM_GAMMA = 1,			///< Enable/Setup Gamma correction	
		PRM_NOISE = 2			///< Enable/Setup Noise generation	
	};

	enum MeshFlags {
		SMOOTH_SHADE = 0x01,	///< Perform smooth shading (i.e. shallow angles gets darkened)
		CULL_NONE = 0x02,		///< Do not perform front/back face culling
		RENDER_ALL = 0x04		///< Render all meshgroups
	};

	/**
	 * \brief Source layout structure for GUI element drawing 
	 */
	typedef struct {
		RECT intr;				///< Interrior rect
		RECT outr;				///< Outerrior rect
	} skpRegion;

	/**
	 * \brief Constructs a drawing object for a given surface.
	 * \param s surface handle
	 */
	Sketchpad (SURFHANDLE s);

	/**
	 * \brief Destructor. Destroys a drawing object.
	 */
	virtual ~Sketchpad ();

	/**
	 * \brief Selects a new font to use.
	 * \param font pointer to font resource
	 * \return Previously selected font.
	 * \default None, returns NULL.
	 * \sa oapi::Font, oapi::GraphicsClient::clbkCreateFont
	 */
	virtual Font *SetFont (Font *font) { assert(false); return NULL; }

	/**
	 * \brief Selects a new pen to use.
	 * \param pen pointer to pen resource, or NULL to disable outlines
	 * \return Previously selected pen.
	 * \default None, returns NULL.
	 * \sa oapi::Pen, oapi::GraphicsClient::clbkCreatePen
	 */
	virtual Pen *SetPen (Pen *pen) { assert(false); return NULL; }

	/**
	 * \brief Selects a new brush to use.
	 * \param brush pointer to brush resource, or NULL to disable fill mode
	 * \return Previously selected brush.
	 * \default None, returns NULL.
	 * \sa oapi::Brush, oapi::GraphicsClient::clbkCreateBrush
	 */
	virtual Brush *SetBrush (Brush *brush) { assert(false); return NULL; }

	/**
	 * \brief Horizontal text alignment modes.
	 * \sa SetTextAlign
	 */
	enum TAlign_horizontal {
		LEFT,        ///< align left
		CENTER,      ///< align center
		RIGHT        ///< align right
	};

	/**
	 * \brief Vertical text alignment modes.
	 * \sa SetTextAlign
	 */
	enum TAlign_vertical {
		TOP,         ///< align top of text line
		BASELINE,    ///< align base line of text line
		BOTTOM       ///< align bottom of text line
	};

	/**
	 * \brief Set horizontal and vertical text alignment.
	 * \param tah horizontal alignment
	 * \param tav vertical alignment
	 * \default None.
	 */
	virtual void SetTextAlign (TAlign_horizontal tah=LEFT, TAlign_vertical tav=TOP) { assert(false); }

	/**
	 * \brief Set the foreground colour for text output.
	 * \param col colour description (format: 0xBBGGRR)
	 * \return Previous colour setting.
	 * \default None, returns 0.
	 */
	virtual DWORD SetTextColor(DWORD col) { assert(false); return 0; }

	/**
	 * \brief Set the background colour for text output.
	 * \param col background colour description (format: 0xBBGGRR)
	 * \return Previous colour setting
	 * \default None, returns 0.
	 * \note The background colour is only used if the background mode
	 *   is set to BK_OPAQUE.
	 * \sa SetBackgroundMode
	 */
	virtual DWORD SetBackgroundColor (DWORD col) { assert(false); return 0; }

	/**
	 * \brief Background modes for text output.
	 * \sa SetBackgroundMode
	 */
	enum BkgMode {
		BK_TRANSPARENT,  ///< transparent background
		BK_OPAQUE        ///< opaque background
	};

	/**
	 * \brief Set the background mode for text and drawing operations.
	 * \param mode background mode (see \ref BkgMode)
	 * \default None.
	 * \note This function affects text output and dashed line drawing.
	 * \note In opaque background mode, text background and the gaps
	 *   between dashed lines are drawn in the current background colour
	 *   (see SetBackgroundColor). In transparent mode, text background
	 *   and line gaps are not modified.
	 * \note The default background mode (before the first call of
	 *   SetBackgroundMode) should be transparent.
	 * \sa SetBackgroundColor, SetTextColor
	 */
	virtual void SetBackgroundMode (BkgMode mode) { assert(false); }

	/**
	 * \brief Return height and (average) width of a character in the currently
	 *   selected font.
	 * \return Height of character cell [pixel] in the lower 16 bit of the return value,
	 *   and (average) width of character cell [pixel] in the upper 16 bit.
	 * \default None, returns 0.
	 * \note The height value should describe the height of the character cell (i.e.
	 *   the smallest box circumscribing all characters in the font), but without any
	 *   "internal leading", i.e. the gap between characters in two consecutive lines.
	 * \note For proportional fonts, the width value should be an approximate average
	 *   character width.
	 */
	virtual DWORD GetCharSize () { assert(false); return 0; }

	/**
	 * \brief Return the width of a text string in the currently selected font.
	 * \param str text string
	 * \param len string length, or 0 for auto (0-terminated string)
	 * \return width of the string, drawn in the currently selected font [pixel]
	 * \default None, returns 0.
	 * \sa SetFont
	 */
	virtual DWORD GetTextWidth (const char *str, int len = 0) { assert(false); return 0; }

	/**
	 * \brief Set the position in the surface bitmap which is mapped to the
	 *   origin of the coordinate system for all drawing functions.
	 * \param x horizontal position of the origin [pixel]
	 * \param y vertical position of the origin [pixel]
	 * \default None.
	 * \note By default, the reference point for drawing function coordinates is
	 *   the top left corner of the bitmap, with positive x-axis to the right,
	 *   and positive y-axis down.
	 * \note SetOrigin can be used to shift the logical reference point to a
	 *   different position in the surface bitmap (but not to change the
	 *   orientation of the axes).
	 * \note If the drawing system used by an implementation does not support
	 *   this function directly, the derived class should itself account for the
	 *   shift in origin, by subtracting the offset from all coordinate values.
	 * \sa GetOrigin
	 */
	virtual void SetOrigin (int x, int y) { assert(false); }

	/**
	 * \brief Returns the position in the surface bitmap which is mapped to
	 *   the origin of the coordinate system for all drawing functions.
	 * \param [out] x pointer to integer receiving horizontal position of the origin [pixel]
	 * \param [out] y pointer to integer receiving vertical position of the origin [pixel]
	 * \default Returns (0,0)
	 * \sa SetOrigin
	 */
	virtual void GetOrigin (int *x, int *y) const { assert(false); *x = 0, *y = 0; }

	/**
	 * \brief Draw a text string.
	 * \param x reference x position [pixel]
	 * \param y reference y position [pixel]
	 * \param str text string
	 * \param len string length for output
	 * \return \e true on success, \e false on failure.
	 * \default None, returns false.
	 */
	virtual bool Text (int x, int y, const char *str, int len) { assert(false); return false; }

	/**
	 * \brief Draw a text string into a rectangle.
	 * \param x1 left edge [pixel]
	 * \param y1 top edge [pixel]
	 * \param x2 right edge [pixel]
	 * \param y2 bottom edge [pixel]
	 * \param str text string
	 * \param len string length for output
	 * \return \e true on success, \e false on failure.
	 * \default Implementation via \ref Text calls.
	 * \note This method should write the text string into the specified
	 *   rectangle, using the current font. Line breaks should automatically
	 *   be applied as required to fit the text in the box.
	 * \note The bottom edge (y2) should probably be ignored, so text isn't
	 *   truncated if it doesn't fit the box.
	 */
	virtual bool TextBox (int x1, int y1, int x2, int y2, const char *str, int len);

	/**
	 * \brief Draw a single pixel in a specified colour.
	 * \param x x-coordinate of point [pixel]
	 * \param y y-coordinate of point [pixel]
	 * \param col pixel colour (format: 0xBBGGRR)
	 */
	virtual void Pixel (int x, int y, DWORD col) { assert(false); }

	/**
	 * \brief Move the drawing reference to a new point.
	 * \param x x-coordinate of new reference point [pixel]
	 * \param y y-coordinate of new reference point [pixel]
	 * \note Some methods use the drawing reference point for
	 *   drawing operations, e.g. \ref LineTo.
	 * \default None.
	 * \sa LineTo
	 */
	virtual void MoveTo (int x, int y) { assert(false); }

	/**
	 * \brief Draw a line to a specified point.
	 * \param x x-coordinate of line end point [pixel]
	 * \param y y-coordinate of line end point [pixel]
	 * \default None.
	 * \note The line starts at the current drawing reference
	 *   point.
	 * \sa MoveTo
	 */
	virtual void LineTo (int x, int y) { assert(false); }

	/**
	 * \brief Draw a line between two points.
	 * \param x0 x-coordinate of first point [pixel]
	 * \param y0 y-coordinate of first point [pixel]
	 * \param x1 x-coordinate of second point [pixel]
	 * \param y1 y-coordinate of second point [pixel]
	 * \default None.
	 * \note The line is drawn with the currently selected pen.
	 * \sa SetPen
	 */
	virtual void Line (int x0, int y0, int x1, int y1) { assert(false); }

	/**
	 * \brief Draw a rectangle (filled or outline).
	 * \param x0 left edge of rectangle [pixel]
	 * \param y0 top edge of rectangle [pixel]
	 * \param x1 right edge of rectangle [pixel]
	 * \param y1 bottom edge of rectangle [pixel]
	 * \default Draws the rectangle from 4 line segments by
	 *   calling \ref MoveTo and \ref LineTo.
	 * \note Derived classes should overload this method if possible,
	 *   because the default method does not allow to draw filled
	 *   rectangles, and may be less efficient than a dedicated
	 *   implementation.
	 * \note Implementations should fill the rectangle with the
	 *   currently selected brush resource.
	 * \sa MoveTo, LineTo, Ellipse, Polygon
	 */
	virtual void Rectangle (int x0, int y0, int x1, int y1);

	/**
	 * \brief Draw an ellipse from its bounding box.
	 * \param x0 left edge of bounding box [pixel]
	 * \param y0 top edge of bounding box [pixel]
	 * \param x1 right edge of bounding box [pixel]
	 * \param y1 bottom edge of bounding box [pixel]
	 * \default None.
	 * \note Implementations should fill the ellipse with the
	 *   currently selected brush resource.
	 * \sa Rectangle, Polygon
	 */
	virtual void Ellipse (int x0, int y0, int x1, int y1) { assert(false); }

	/**
	 * \brief Draw a closed polygon given by vertex points.
	 * \param pt list of vertex points
	 * \param npt number of points in the list
	 * \default None.
	 * \note Implementations should draw the outline of the 
	 *   polygon with the current pen, and fill it with the
	 *   current brush.
	 * \note The polygon should be closed, i.e. the last point
	 *   joined with the first one.
	 * \sa Polyline, PolyPolygon, Rectangle, Ellipse
	 */
	virtual void Polygon (const IVECTOR2 *pt, int npt) { assert(false); }

	/**
	 * \brief Draw a line of piecewise straight segments.
	 * \param pt list of vertex points
	 * \param npt number of points in the list
	 * \default None
	 * \note Implementations should draw the line with the
	 *   currently selected pen.
	 * \note Polylines are open figures: the end points are
	 *   not connected, and no fill operation is performed.
	 * \sa Polygon, PolyPolyline, Rectangle, Ellipse
	 */
	virtual void Polyline (const IVECTOR2 *pt, int npt) { assert(false); }

	/**
	 * \brief Draw a set of polygons.
	 * \param pt list of vertex points for all polygons
	 * \param npt list of number of points for each polygon
	 * \param nline number of polygons
	 * \default Calls Polygon for each line in the list.
	 * \note The number of entries in npt must be >= nline, and
	 *   the number of points in pt must be at least the sum of
	 *   the values in npt.
	 * \note Implementations should overload this function if
	 *   they can provide efficient direct support for it. Otherwise,
	 *   the base class implementation should be sufficient.
	 * \sa Polygon, Polyline, PolyPolyline
	 */
	virtual void PolyPolygon (const IVECTOR2 *pt, const int *npt, const int nline);

	/**
	 * \brief Draw a set of polylines.
	 * \param pt list of vertex points for all lines
	 * \param npt list of number of points for each line
	 * \param nline number of lines
	 * \default Calls Polyline for each line in the list.
	 * \note The number of entries in npt must be >= nline, and
	 *   the number of points in pt must be at least the sum of
	 *   the values in npt.
	 * \note Implementations should overload this function if
	 *   they can provide efficient direct support for it. Otherwise,
	 *   the base class implementation should be sufficient.
	 * \sa Polyline, Polygon, PolyPolygon
	 */
	virtual void PolyPolyline (const IVECTOR2 *pt, const int *npt, const int nline);

	/**
	 * \brief Returns the surface associated with the drawing object.
	 * \return Surface handle
	 */
	inline SURFHANDLE GetSurface() const { return surf; }

	/**
	 * \brief Obsolete function. Will return NULL.
	 * \return NULL
	 */
	virtual HDC GetDC() { return NULL; }

	/**
	 * \brief Draw a text string using WCHAR.
	 * \param x reference x position [pixel]
	 * \param y reference y position [pixel]
	 * \param str text string
	 * \param len string length for output
	 * \return \e true on success, \e false on failure.
	 * \default None, returns false.
	 */

	/**
	* \brief [DX9] Get a render surface size in pixels
	* \param size Pointer to SIZE structure receiving the size
	*/
	virtual void GetRenderSurfaceSize(LPSIZE size) { assert(false); }

	/**
	* \brief [DX9] Setup a quick pen, removes any other pen from use. Set to zero to disable a pen from use.
	* \param color Pen color in 0xAABBGGRR
	* \param width Pen width in pixels
	* \param style 0 = Disabled, 1 = Solid, 2 = Dashed
	*/
	virtual void QuickPen(DWORD color, float width = 1.0f, DWORD style = 1) { assert(false); }

	/**
	* \brief [DX9] Setup a quick brush, removes any other brush from use. Set to zero to disable a brush from use.
	* \param color Brush color in 0xAABBGGRR
	*/
	virtual void QuickBrush(DWORD color) { assert(false); }

	/**
	* \brief [DX9] Set up a global line width scale factor
	* \param width A line width scale factor. (Default 1.0f)
	* \param pattern Line pattern scale factor. (Default 1.0f)
	*/
	virtual void SetGlobalLineScale(float width = 1.0f, float pattern = 1.0f) { assert(false); }

	/**
	* \brief [DX9] Set up a global world transformation matrix.
	* \param pWT A pointet to FMATRIX4, NULL to reset default settings.
	* \note This function will conflict and resets any settings set by SetOrigin(). Setting to NULL does not restore SetOrigin().
	* \note Everything is transformed including CopyRect() and Text().
	* \warning Graphics results from a CopyRect() and Text() can be blurry when non-default SetViewProjectionMatrix or SetWorldTransform is in use
	*		due to source-target pixels miss aligments.
	*/
	virtual	void SetWorldTransform(const FMATRIX4* pWT = NULL) { assert(false); }

	/**
	* \brief [DX9] Get a View matrix. [Read only]
	*/
	virtual	const FMATRIX4* ViewMatrix() const { assert(false); return NULL; }

	/**
	* \brief [DX9] Get a Projection matrix. [Read only]
	*/
	virtual	const FMATRIX4* ProjectionMatrix() const { assert(false); return NULL; }

	/**
	* \brief [DX9] Get combined view projection matrix. [Read only]
	*/
	virtual	const FMATRIX4* GetViewProjectionMatrix() const { assert(false); return NULL; }

	/**
	* \brief [DX9] Set an active view mode. Switch between modes doesn't reset the view matrices and setups.
	* \param mode, SkpView mode setting.
	*/
	virtual void SetViewMode(SkpView mode = ORTHO) { assert(false); }

	/**
	* \brief [DX9] Set up a global world transformation matrix.
	* \param scale Graphics scale factor.
	* \param rot Rotation angle [rad]
	* \param ctr Pointer to a IVECTOR containing a rotation center or NULL for origin.
	* \param trl Pointer to a IVECTOR containing a translation or NULL.
	* \note This function will conflict and resets any settings set by SetOrigin(). Setting to NULL does not restore SetOrigin().
	* \note Everything is transformed including CopyRect() and Text().
	* \warning Graphics results from a CopyRect() and Text() can be blurry when non-default SetViewProjectionMatrix or SetWorldTransform is in use
	*		due to source-target pixels miss aligments.
	*/
	virtual void SetWorldTransform2D(float scale = 1.0f, float rot = 0.0f, const IVECTOR2* ctr = NULL, const IVECTOR2* trl = NULL) { assert(false); }

	/**
	* \brief [DX9] Set up a screen space clip rectangle. Usefull when need to draw in a smaller sub section of the render target.
	* \param pClip A pointer to clipping rectangle, Set to NULL to disable clipping.
	*/
	virtual void ClipRect(const LPRECT pClip = NULL) { assert(false); }

	/**
	* \brief [DX9] Set up a world space clip cone to clip pixels within it. Does not work with orthographic projection.
	* \param idx Index of the clipper object. Valids are "0" and "1".
	* \param pPos a pointer to a unit vector containing cone direction in camera centric frame, Set to NULL to disable clipping.
	* \param angle cosine of the half-angle of the cone.
	* \param dist clip-plane distance, clipping only occur beyond this distance.
	* \note This function is provided due to reasons that z-buffering doesn't really work in all cases. Designed to be used when
	* drawing into the planetarium view to prevent "see through" planets.
	*/
	virtual void Clipper(int idx, const VECTOR3* pPos = NULL, double cos_angle = 0.0, double dist = 0.0) { assert(false); }

	/**
	* \brief [DX9] Enable a use of depth buffer.
	* \param bEnable Toggle depth buffer.
	*/
	virtual void DepthEnable(bool bEnable) { assert(false); }

	/**
	 * \brief Draw a text string.
	 * \param x reference x position [pixel]
	 * \param y reference y position [pixel]
	 * \param str text string
	 * \param len string length for output
	 * \return \e true on success, \e false on failure.
	 * \default None, returns false.
	 */
	virtual bool TextW(int x, int y, const LPWSTR str, int len) { return false; }
	
	/**
	* \brief [DX9] Draws a mesh group or entire mesh in the render target.
	* \param hMesh Pointer to mesh containing the geometry.
	* \param grp Group index to draw.
	* \param flags MeshFlags
	* \param hTex a texture override, render with this texture regardless what ever is specified in the mesh.
	* \return Number of groups in the mesh or -1 if the group index is out of range.
	* \note Use SetWorldTransform() to move, rotate and scale the object.
	* \note This function creates a local Vertex/Index buffers, so, vertex count isn't a major factor.
	* \note Modifications to input meah has no effects due to local copy.
	* \note Final color = Texture Color * Material Color, only diffure material is in use.
	* \note To draw the entire mesh at once, use MeshFlags::RENDER_ALL flag.
	*/
	virtual int DrawMeshGroup(const MESHHANDLE hMesh, DWORD grp, MeshFlags flags = MeshFlags::SMOOTH_SHADE, const SURFHANDLE hTex = NULL) { assert(false); return -2; }
	
	/**
	* \brief [DX9] Copy 'Blit' a rectangle
	* \param hSrc Source surface handle
	* \param src Source rectangle, (or NULL for whole surface)
	* \param tx Target x-coordinate
	* \param ty Target y-coordinate
	* \note Can alpha-blend and mirror by a use of negative width/height in source rectangle
	*/
	virtual void CopyRect(const SURFHANDLE hSrc, const LPRECT src, int tx, int ty) { assert(false); }

	/**
	* \brief [DX9] Copy 'Blit' a rectangle
	* \param hSrc Source surface handle
	* \param src Source rectangle, (or NULL for whole surface)
	* \param tgt Target rectangle, (or NULL for whole surface)
	* \note Can alpha-blend and mirror by a use of negative width/height in source rectangle
	*/
	virtual void StretchRect(const SURFHANDLE hSrc, const LPRECT src = NULL, const LPRECT tgt = NULL) { assert(false); }

	/**
	* \brief [DX9] Copy 'Blit' a rectangle with rotation and scaling
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
	virtual void RotateRect(const SURFHANDLE hSrc, const LPRECT src, int cx, int cy, float angle = 0.0f, float sw = 1.0f, float sh = 1.0f) { assert(false); }

	/**
	* \brief [DX9] Copy 'Blit' a rectangle using a color-key stored in a source surface.
	* \param hSrc Source surface handle
	* \param src Source rectangle, (or NULL for whole surface)
	* \param tx Target x-coordinate
	* \param ty Target y-coordinate
	* \note ColorKey() does not work properly with SetWorldTransform() due to color interpolation
	* \note Can mirror by a use of negative width/height in source rectangle
	*/
	virtual void ColorKey(const SURFHANDLE hSrc, const LPRECT src, int tx, int ty) { assert(false); }

	/**
	* \brief [DX9] Write a line of text using text scaling and rotation
	* \param x x-coordinate of the text alignment point
	* \param y y-coordinate of the text alignment point
	* \param str pointer to NULL terminated string to write.
	* \param scale Text scale factor (0.0f to 1.0f)
	* \param angle Rotation angle in radians.
	* \sa Text()
	* \note Rotation and scaling can result a blurry text if used with a small fonts. Rotation of ±PI/2 or PI should work fine.
	* \note Rotation spefified during font creation is ignored in this function.
	*/
	virtual void TextEx(float x, float y, const char* str, float scale = 1.0f, float angle = 0.0f) { assert(false); }

	/**
	* \brief [DX9] Draw a pre-created polyline or polygon object
	* \param hPoly Handle to a poly object
	* \param flags (reserved for later use, set to zero for now)
	* \sa gcCreatePoly, gcDeletePoly
	*/
	virtual void DrawPoly(const HPOLY hPoly, DWORD flags = 0) { assert(false); }

	/**
	* \brief [DX9] Draw a list of independent lines. 0-1, 2-3, 4-5,...
	* \param pt list of vertex points.
	* \param nlines number of lines to draw
	*/
	virtual void Lines(const FVECTOR2* pt1, int nlines) { assert(false); }

	/**
	* \brief [DX9] Set up a view matrix.
	* \param pV A pointet to FMATRIX4, NULL to reset default settings.
	*/
	virtual	void SetViewMatrix(const FMATRIX4* pV = NULL) { assert(false); }

	/**
	* \brief [DX9] Set up a projection matrix.
	* \param pP A pointet to FMATRIX4, NULL to reset default settings.
	*/
	virtual	void SetProjectionMatrix(const FMATRIX4* pP = NULL) { assert(false); }

	/**
	* \brief [DX9] Get a read only pointer to current ColorMatrix.
	* \sa SetColorMatrix, SetBrightness
	*/
	virtual const FMATRIX4* GetColorMatrix() { assert(false); return NULL; }

	/**
	* \brief [DX9] Set a ColorMatrix for color correrctions. Reset to default by passing NULL pointer
	* \param pMatrix Pointer to a matrix or NULL.
	* \sa GetColorMatrix, SetBrightness
	*/
	virtual void SetColorMatrix(const FMATRIX4* pMatrix = NULL) { assert(false); }

	/**
	* \brief [DX9] Automatically set a ColorMatrix for brightness control. NULL to restore default settings.
	* \param pBrightness Pointer into a float values color vector, or NULL.
	* \sa GetColorMatrix, SetColorMatrix
	*/
	virtual void SetBrightness(const FVECTOR4* pBrightness = NULL) { assert(false); }

	/**
	* \brief [DX9] Get a render configuration setting or "effect".
	* \param param A setting ID to get.
	* \note Valid ID Flags:
	* \note SKP3_PRM_GAMMA, Get Gamma correction value (.rgb)
	* \note SKP3_PRM_NOISE, Get Noise configuration. Noise color in (.rgb) and (.a) controls blending between input color (.rgb) and target color.
	* \sa SetRenderParam
	*/
	virtual FVECTOR4 GetRenderParam(RenderParam param) { assert(false); return FVECTOR4(0.0f, 0.0f, 0.0f, 0.0f); }

	/**
	* \brief [DX9] Set a render configuration paramater or "effect".
	* \param param A setting ID to set, or NULL to disable effect from use.
	* \sa SetRenderParam
	*/
	virtual void SetRenderParam(RenderParam param, const FVECTOR4* data = NULL) { assert(false); }

	/**
	* \brief [DX9] Setup a blending state
	* \param dwState Desired configuration.
	* \note SKPBS_ALPHABLEND, AlphaBlend source.color to destination.color, will retain destination alpha unchanged (if exists).
	* \note SKPBS_COPY, Copy both source color and alpha to destination
	* \note SKPBS_COPY_ALPHA, Copy source.alpha to destination.alpha, will retain destination color unchanged
	* \note SKPBS_COPY_COLOR, Copy source.color to destination.color, will retain destination alpha unchanged (if exists)
	*/
	virtual void SetBlendState(BlendState State = (BlendState)(BlendState::ALPHABLEND | BlendState::FILTER_LINEAR)) { assert(false); }

	/**
	* \brief [DX9] Get world transformation matrix
	*/
	virtual FMATRIX4 GetWorldTransform() const { return FMATRIX4(); }

	/**
	* \brief [DX9] Push current world transformation matrix onto a stack
	*/
	virtual void PushWorldTransform() { assert(false); }

	/**
	* \brief [DX9] Pop a world transformation matrix from a stack and make it active
	*/
	virtual void PopWorldTransform() { assert(false); }

	/**
	* \brief [DX9] Set up a global world transformation matrix.
	* \param scl Pointer to a FVECTOR2 containing a scaling information or NULL for 1.0
	* \param trl Pointer to a IVECTOR2 containing a translation or NULL.
	* \note This function will conflict and resets any settings set by SetOrigin(). Setting to NULL does not restore SetOrigin().
	* \note Everything is transformed including CopyRect() and Text().
	* \warning Graphics results from a CopyRect() and Text() can be blurry when non-default SetViewProjectionMatrix or SetWorldTransform is in use
	*		due to source-target pixels miss aligments.
	*/
	virtual void SetWorldScaleTransform2D(const FVECTOR2* scl = NULL, const IVECTOR2* trl = NULL) { assert(false); }

	/**
	* \brief [DX9] Fill a rectangle with color gradient
	* \param tgt a Rect specifying the bounds
	* \param c1 Left or Top color
	* \param c2 Right or Bottom color
	* \param bVertical Direction of the gradient.
	*/
	virtual void GradientFillRect(const LPRECT tgt, DWORD c1, DWORD c2, bool bVertical = false) { assert(false); }

	virtual int GetVersion() { return 1; }

	/**
	* \brief [DX9] Fill a rectangle with color
	* \param color Fill color
	* \param tgt a Rect specifying the bounds, NULL for entire surface
	*/
	virtual void ColorFill(DWORD color, const LPRECT tgt) { assert(false); }

	/**
	* \brief [DX9] Drawing function designed for drawing GUI elements. Buttons, Windows, Boxes, etc..  
	*/
	virtual void StretchRegion(const skpRegion* rgn, const SURFHANDLE hSrc, const LPRECT out) { assert(false); }

	/**
	* \brief [DX9] Copy 'Blit' a tetragon
	* \param hSrc Source surface handle
	* \param sr Source rectangle, (or NULL for whole surface)
	* \param pt Pointer to an array of 4 FVECTOR2 target points forming shape of the tetragon.
	*/
	virtual void CopyTetragon(const SURFHANDLE hSrc, const LPRECT sr, const FVECTOR2 pt[4]) { assert(false); }
	virtual void FillTetragon(DWORD color, const FVECTOR2 pt[4]) { assert(false); }

	/**
	* \brief [DX9] Enable and Disable color compatibility mode where Pen/Brush Alpha value 0x00 is translated to 0xFF
	* \note Default: Enabled, Only effects to a colors assigned to pens and brushes after a mode change.
	*/
	virtual void ColorCompatibility(bool bEnable) { assert(false); }

	/**
	* \brief [DX9] Clear drawing surface with a color and a depth buffer
	* \param color Desired background color
	* \param bColor if true, fills the background with 'color'
	* \param bDepth if true, clears the depth buffer (if exists)
	*/
	virtual void Clear(DWORD color = 0, bool bColor = true, bool bDepth = true) { assert(false); }

	/**
	* \brief [DX9] Set near/far clip distances
	* \param _near near distance to be set
	* \param _far far distance to be set
	*/
	virtual void SetClipDistance(float _near, float _far) { assert(false); }

	/**
	* \brief [DX9] Set a world matrix for drawing into a specified 3D location and the drawing will be facing the camera.
	* \param wpos Camera centric location in ECL frame.
	* \param scl Drawing scale factor.
	* \param bFixed If 'true' drawing will retain a constant size recardless of camera distance. (One drawing unit is aprox. one pixel)
	* \param index Direction of drawing stace x-axis in ecliptic frame. If 'NULL' drawing is orientated with screen (i.e. display) 
	* \Note Only works in perspective projection
	*/
	virtual void SetWorldBillboard(const FVECTOR3& wpos, float scl = 1.0f, bool bFixed = true, const FVECTOR3 *index = NULL) { assert(false); }
private:
	SURFHANDLE surf;
};

} // namespace oapi

#endif // !__DRAWAPI_H
