// ======================================================================
//                     ORBITER SOFTWARE DEVELOPMENT KIT
//                    Copyright (C) 2008 Martin Schweiger
//                           All rights reserved
// DrawAPI.h
// 2-D surface drawing support interface.
// This API defines an abstraction layer for providing drawing support
// for surfaces (e.g. lines and text). It is closely modelled on the
// Windows GDI, but provides an overload mechanism to insert different
// drawing systems.
// ======================================================================


// ===================================================================================================================================
// !! ATTENTION !! THIS FILE IS USED ONLY FOR DOCUMENTATION WITH DOXYGEN WITH SOME D3D9 SPECIFIC INFORMATION CHANGES.  !! ATTENTION !! 
// ===================================================================================================================================


/**
 * \file DrawAPI.h
 * \brief 2-D surface drawing support interface.
 */

#ifndef __DRAWAPI_H
#define __DRAWAPI_H

#include "OrbiterAPI.h"

namespace oapi {

/**
 * \brief Integer-valued 2-D vector type.
 * \note This structure is designed to be compatible with the Windows POINT type.
 */
union IVECTOR2 {
	long data[2];  ///< vector data array
	struct {
		long x;    ///< vector x coordinate
		long y;    ///< vector y coordinate
	};
};

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
class OAPIFUNC Font: public DrawingTool {
public:
	/**
	 * \brief Font decoration style.
	 * \sa Font(int,bool,char*,Style)
	 */
	enum Style {
		NORMAL=0,    ///< no decoration
		BOLD=1,      ///< boldface
		ITALIC=2,    ///< italic
		UNDERLINE=4  ///< underlined
	};

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
	Font (int height, bool prop, const char *face, Style style=NORMAL, int orientation=0) {}

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
class OAPIFUNC Sketchpad {
public:
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
	virtual Font *SetFont (Font *font) const { return NULL; }

	/**
	 * \brief Selects a new pen to use.
	 * \param pen pointer to pen resource, or NULL to disable outlines
	 * \return Previously selected pen.
	 * \default None, returns NULL.
	 * \sa oapi::Pen, oapi::GraphicsClient::clbkCreatePen
	 */
	virtual Pen *SetPen (Pen *pen) const { return NULL; }

	/**
	 * \brief Selects a new brush to use.
	 * \param brush pointer to brush resource, or NULL to disable fill mode
	 * \return Previously selected brush.
	 * \default None, returns NULL.
	 * \sa oapi::Brush, oapi::GraphicsClient::clbkCreateBrush
	 */
	virtual Brush *SetBrush (Brush *brush) const { return NULL; }

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
	virtual void SetTextAlign (TAlign_horizontal tah=LEFT, TAlign_vertical tav=TOP) {}

	/**
	 * \brief Set the foreground colour for text output.
	 * \param col colour description (format: 0xBBGGRR)
	 * \return Previous colour setting.
	 * \default None, returns 0.
	 */
	virtual DWORD SetTextColor (DWORD col) { return 0; }

	/**
	 * \brief Set the background colour for text output.
	 * \param col background colour description (format: 0xBBGGRR)
	 * \return Previous colour setting
	 * \default None, returns 0.
	 * \note The background colour is only used if the background mode
	 *   is set to BK_OPAQUE.
	 * \sa SetBackgroundMode
	 */
	virtual DWORD SetBackgroundColor (DWORD col) { return 0; }

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
	virtual void SetBackgroundMode (BkgMode mode) {}

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
	virtual DWORD GetCharSize () { return 0; }

	/**
	 * \brief Return the width of a text string in the currently selected font.
	 * \param str text string
	 * \param len string length, or 0 for auto (0-terminated string)
	 * \return width of the string, drawn in the currently selected font [pixel]
	 * \default None, returns 0.
	 * \sa SetFont
	 */
	virtual DWORD GetTextWidth (const char *str, int len = 0) { return 0; }

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
	virtual void SetOrigin (int x, int y) {}

	/**
	 * \brief Returns the position in the surface bitmap which is mapped to
	 *   the origin of the coordinate system for all drawing functions.
	 * \param [out] x pointer to integer receiving horizontal position of the origin [pixel]
	 * \param [out] y pointer to integer receiving vertical position of the origin [pixel]
	 * \default Returns (0,0)
	 * \sa SetOrigin
	 */
	virtual void GetOrigin (int *x, int *y) const { *x = 0, *y = 0; }

	/**
	 * \brief Draw a text string.
	 * \param x reference x position [pixel]
	 * \param y reference y position [pixel]
	 * \param str text string
	 * \param len string length for output
	 * \return \e true on success, \e false on failure.
	 * \default None, returns false.
	 */
	virtual bool Text (int x, int y, const char *str, int len) { return false; }

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
	virtual void Pixel (int x, int y, DWORD col) {}

	/**
	 * \brief Move the drawing reference to a new point.
	 * \param x x-coordinate of new reference point [pixel]
	 * \param y y-coordinate of new reference point [pixel]
	 * \note Some methods use the drawing reference point for
	 *   drawing operations, e.g. \ref LineTo.
	 * \default None.
	 * \sa LineTo
	 */
	virtual void MoveTo (int x, int y) {}

	/**
	 * \brief Draw a line to a specified point.
	 * \param x x-coordinate of line end point [pixel]
	 * \param y y-coordinate of line end point [pixel]
	 * \default None.
	 * \note The line starts at the current drawing reference
	 *   point.
	 * \sa MoveTo
	 */
	virtual void LineTo (int x, int y) {}

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
	virtual void Line (int x0, int y0, int x1, int y1) {}

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
	virtual void Ellipse (int x0, int y0, int x1, int y1) {}

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
	 * \note Due to performance heavy triangularization maximum 
	 * number of points is limited to 64 when filled with brush. 
	 * \sa Polyline, PolyPolygon, Rectangle, Ellipse
	 */
	virtual void Polygon (const IVECTOR2 *pt, int npt) {}

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
	virtual void Polyline (const IVECTOR2 *pt, int npt) {}

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
	 * \note Surface is locked during existance of Sketchpad interface
	 * and cannot be accessed.
	 */
	inline SURFHANDLE GetSurface() const { return surf; }

	/**
	 * \brief Return the Windows device context handle, if applicable.
	 * \return NULL, or device context handle in some special cases.
	 * \default None, returns NULL.
	 * \note Sketchpad implementations based on the Windows GDI system
	 *   should overload this function to return the device context handle
	 *   here. All other implementations should not overload this function.
	 * \note The device context returned by this function should not be
	 *   released (e.g. with ReleaseDC). The device context is released
	 *   automatically when the Sketchpad instance is destroyed.
	 * \note This method should be regarded as temporary. Ultimately, the
	 *   device-dependent drawing mechanism should be hidden outside the
	 *   sketchpad implementation.
	 */
	virtual HDC GetDC() { return NULL; }

private:
	SURFHANDLE surf;
};

} // namespace oapi

#endif // !__DRAWAPI_H