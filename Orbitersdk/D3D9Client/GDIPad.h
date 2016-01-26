// ==============================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2007 Martin Schweiger
//				 2012 Jarmo Nikkanen
// ==============================================================

#ifndef __GDIPAD_H
#define __GDIPAD_H

#include "OrbiterAPI.h"
#include "D3D9Client.h"
#include <d3d9.h> 
#include <d3dx9.h>


// ======================================================================
// class GDIPad
// ======================================================================
/**
 * \brief The GDIPad class defines the context for 2-D drawing using
 *   Windows GDI calls.
 */
class GDIPad: public oapi::Sketchpad {
public:

	GDIPad (SURFHANDLE s, HDC hdc);
	~GDIPad ();

	/**
	 * \brief Selects a new font to use.
	 * \param font pointer to font resource
	 * \return Previously selected font.
	 * \sa GDIFont, GDIClient::clbkCreateFont
	 */
	oapi::Font *SetFont (oapi::Font *font) const;

	/**
	 * \brief Selects a new pen to use.
	 * \param pen pointer to pen resource, or NULL to disable outlines
	 * \return Previously selected pen.
	 * \sa GDIPen, GDIClient::clbkCreatePen
	 */
	oapi::Pen *SetPen (oapi::Pen *pen) const;

	/**
	 * \brief Selects a new brush to use.
	 * \param brush pointer to brush resource, or NULL to disable fill mode
	 * \return Previously selected brush.
	 * \sa GDIBrush, GDIClient::clbkCreateBrush
	 */
	oapi::Brush *SetBrush (oapi::Brush *brush) const;

	/**
	 * \brief Set horizontal and vertical text alignment.
	 * \param tah horizontal alignment
	 * \param tav vertical alignment
	 */
	void SetTextAlign (TAlign_horizontal tah=LEFT, TAlign_vertical tav=TOP);

	/**
	 * \brief Set the foreground colour for text output.
	 * \param col colour description (format: 0xBBGGRR)
	 * \return Previous colour setting.
	 */
	DWORD SetTextColor (DWORD col);

	/**
	 * \brief Set the background colour for text output.
	 * \param col background colour description (format: 0xBBGGRR)
	 * \return Previous colour setting
	 * \note The background colour is only used if the background mode
	 *   is set to BK_OPAQUE.
	 * \sa SetBackgroundMode
	 */
	DWORD SetBackgroundColor (DWORD col);

	/**
	 * \brief Set the background mode for text output.
	 * \param mode background mode (see \ref BkgMode)
	 * \note In opaque background mode, the text background is drawn
	 *   in the current background colour (see SetBackgroundColor).
	 * \note The default background mode (before the first call of
	 *   SetBackgroundMode) should be transparent.
	 * \sa SetBackgroundColor, SetTextColor
	 */
	void SetBackgroundMode (BkgMode mode);

	/** brief Returns height and (average) width of a character in the currently
	 *   selected font.
	 * \return Height of character cell [pixel] in the lower 16 bit of the return value,
	 *   and (average) width of character cell [pixel] in the upper 16 bit.
	 * \note The height value is given by tmHeight-tmInternalLeading from the
	 *   TEXTMETRIC structure returned by the GDI GetTextMetrics function.
	 * \note The width value is given by tmAveCharWidth from the
	 *   TEXTMETRIC structure returned by the GDI GetTextMetrics function.
	 */
	DWORD GetCharSize ();

	/**
	 * \brief Returns the width of a text string in the currently selected font.
	 * \param str text string
	 * \param len string length, or 0 for auto (0-terminated string)
	 * \return width of the string, drawn in the currently selected font [pixel]
	 * \sa SetFont
	 */
	DWORD GetTextWidth (const char *str, int len = 0);

	/**
	 * \brief Set the position in the surface bitmap which is mapped to the
	 *   origin of the coordinate system for all drawing functions.
	 * \param x horizontal position of the origin [pixel]
	 * \param y vertical position of the origin [pixel]
	 * \note By default, the reference point for drawing function coordinates is
	 *   the top left corner of the bitmap, with positive x-axis to the right,
	 *   and positive y-axis down.
	 * \note SetOrigin can be used to shift the logical reference point to a
	 *   different position in the surface bitmap (but not to change the
	 *   orientation of the axes).
	 */
	void SetOrigin (int x, int y);

	/**
	 * \brief Returns the position in the surface bitmap which is mapped to
	 *   the origin of the coordinate system for all drawing functions.
	 * \param [out] x pointer to integer receiving horizontal position of the origin [pixel]
	 * \param [out] y pointer to integer receiving vertical position of the origin [pixel]
	 * \default Returns (0,0)
	 * \sa SetOrigin
	 */
	void GetOrigin (int *x, int *y) const;

	/**
	 * \brief Draws a text string.
	 * \param x reference x position [pixel]
	 * \param y reference y position [pixel]
	 * \param str text string
	 * \param len string length for output
	 * \return \e true on success, \e false on failure.
	 */
	bool Text (int x, int y, const char *str, int len);

	/**
	 * \brief Draw a text string into a rectangle.
	 * \param x1 left edge [pixel]
	 * \param y1 top edge [pixel]
	 * \param x2 right edge [pixel]
	 * \param y2 bottom edge [pixel]
	 * \param str text string
	 * \param len string length for output
	 * \return \e true on success, \e false on failure.
	 */
	bool TextBox (int x1, int y1, int x2, int y2, const char *str, int len);

	/**
	 * \brief Draws a single pixel in a specified colour.
	 * \param x x-coordinate of point [pixel]
	 * \param y y-coordinate of point [pixel]
	 * \param col pixel colour (format: 0xBBGGRR)
	 */
	void Pixel (int x, int y, DWORD col);

	/**
	 * \brief Moves the drawing reference to a new point.
	 * \param x x-coordinate of new reference point [pixel]
	 * \param y y-coordinate of new reference point [pixel]
	 * \note Some methods use the drawing reference point for
	 *   drawing operations, e.g. \ref LineTo.
	 * \sa LineTo
	 */
	void MoveTo (int x, int y);

	/**
	 * \brief Draws a line to a specified point.
	 * \param x x-coordinate of line end point [pixel]
	 * \param y y-coordinate of line end point [pixel]
	 * \note The line starts at the current drawing reference
	 *   point.
	 * \sa MoveTo
	 */
	void LineTo (int x, int y);

	/**
	 * \brief Draws a line between two points.
	 * \param x0 x-coordinate of first point [pixel]
	 * \param y0 y-coordinate of first point [pixel]
	 * \param x1 x-coordinate of second point [pixel]
	 * \param y1 y-coordinate of second point [pixel]
	 * \note The line is drawn with the currently selected pen.
	 * \sa SetPen
	 */
	void Line (int x0, int y0, int x1, int y1);

	/**
	 * \brief Draw a rectangle (filled or outline).
	 * \param x0 left edge of rectangle [pixel]
	 * \param y0 top edge of rectangle [pixel]
	 * \param x1 right edge of rectangle [pixel]
	 * \param y1 bottom edge of rectangle [pixel]
	 * \note The rectangle is filled with the currently selected
	 *   brush resource.
	 * \sa Ellipse
	 */
	void Rectangle (int x0, int y0, int x1, int y1);

	/**
	 * \brief Draw an ellipse from its bounding box.
	 * \param x0 left edge of bounding box [pixel]
	 * \param y0 top edge of bounding box [pixel]
	 * \param x1 right edge of bounding box [pixel]
	 * \param y1 bottom edge of bounding box [pixel]
	 * \note The ellipse is filled with the currently selected
	 *   brush resource.
	 * \sa Rectangle
	 */
	void Ellipse (int x0, int y0, int x1, int y1);

	/**
	 * \brief Draw a closed polygon given by vertex points.
	 * \param pt list of vertex points
	 * \param npt number of points in the list
	 * \note The polygon is outlined with the current pen, and
	 *   filled with the current brush.
	 * \note The polygon is closed, i.e. the last point is
	 *   joined with the first one.
	 * \sa Polyline, PolyPolygon, Rectangle, Ellipse
	 */
	void Polygon (const oapi::IVECTOR2 *pt, int npt);

	/**
	 * \brief Draw a line of piecewise straight segments.
	 * \param pt list of vertex points
	 * \param npt number of points in the list
	 * \note The line is drawn with the currently selected pen.
	 * \note Polylines are open figures: the end points are
	 *   not connected, and no fill operation is performed.
	 * \sa Polygon, PolyPolyline Rectangle, Ellipse
	 */
	void Polyline (const oapi::IVECTOR2 *pt, int npt);

	/**
	 * \brief Draw a set of polygons.
	 * \param pt list of vertex points for all polygons
	 * \param npt list of number of points for each polygon
	 * \param nline number of polygons
	 * \note The number of entries in npt must be >= nline, and
	 *   the number of points in pt must be at least the sum of
	 *   the values in npt.
	 * \sa Polygon, Polyline, PolyPolyline
	 */
	void PolyPolygon (const oapi::IVECTOR2 *pt, const int *npt, const int nline);

	/**
	 * \brief Draw a set of polylines.
	 * \param pt list of vertex points for all lines
	 * \param npt list of number of points for each line
	 * \param nline number of lines
	 * \note The number of entries in npt must be >= nline, and
	 *   the number of points in pt must be at least the sum of
	 *   the values in npt.
	 * \sa Polyline, Polygon, PolyPolygon
	 */
	void PolyPolyline (const oapi::IVECTOR2 *pt, const int *npt, const int nline);

	HDC GetDC();

private:
	HDC hDC;
	HFONT hFontA;

	mutable HFONT hFont0;      // original GDI font
	mutable oapi::Font *cfont; // currently selected font (NULL if none)
	mutable oapi::Pen *cpen;   // currently selected pen (NULL if none)
	mutable oapi::Brush *cbrush; // currently selected brush (NULL if none)
};

#endif // !__GDICLIENT_H
