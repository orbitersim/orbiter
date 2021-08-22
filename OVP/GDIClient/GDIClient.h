// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//   ORBITER VISUALISATION PROJECT (OVP)
//   D3D7 Client module
// ==============================================================

// ==============================================================
// GDIClient.h
// Defines class GDIClient, acts as a base class for GDI-based
// graphics clients
// ==============================================================

#ifndef __GDICLIENT_H
#define __GDICLIENT_H

#include "GraphicsAPI.h"

#ifdef GDICLIENT_EXPORTS
#define GDICLIENTEXPORT DLLEXPORT
#else
#define GDICLIENTEXPORT
#endif

class GDIPad;
union oapi::IVECTOR2;

// ======================================================================
// class GDIClient
// ======================================================================
/**
 * \brief Base class for all graphics client implementations with GDI
 *   (Windows Graphics Device Interface) support.
 *
 * This class implements 2-D drawing primitives via Windows GDI calls.
 */
class GDICLIENTEXPORT GDIClient: public oapi::GraphicsClient {
public:
	/**
	 * \brief Create a GDI graphics object.
	 * \param hInstance module instance handle (as passed to InitModule)
	 */
	GDIClient (HINSTANCE hInstance);

	// ==================================================================
	/// \name 2-D drawing interface
	//@{
	/**
	 * \brief Creates and returns a GDI drawing object associated with a surface.
	 * \param surf surface handle
	 * \return Pointer to drawing object
	 * \note This function relies on the \ref clbkGetSurfaceDC method of a derived
	 *   class. If this method is not implemented, clbkGetSketchpad returns NULL.
	 * \sa clbkReleaseSketchpad
	 */
	oapi::Sketchpad *clbkGetSketchpad (SURFHANDLE surf);

	/**
	 * \brief Releases a GDI drawing object.
	 * \param db drawing object
	 * sa clbkGetSketchpad
	 */
	void clbkReleaseSketchpad (oapi::Sketchpad *sp);

	/**
	 * \brief Creates a font resource for 2-D drawing.
	 * \param height cell or character height [pixel]
	 * \param prop proportional/fixed width flag
	 * \param face font face name
	 * \param style font decoration style
	 * \param orientation text orientation [1/10 deg]
	 * \return Pointer to font resource
	 * \note For a description of the parameters, see the Font constructor
	 *   \ref oapi::Font::Font
	 * \sa clbkReleaseFont, GDIFont
	 */
	oapi::Font *clbkCreateFont (int height, bool prop, const char *face, FontStyle style = FontStyle::FONT_NORMAL, int orientation = 0) const;
	oapi::Font* clbkCreateFontEx(int height, char* face, int width = 0, int weight = 400, FontStyle style = FontStyle::FONT_NORMAL, float spacing = 0.0f) const;

	/**
	 * \brief De-allocates a font resource.
	 * \param font pointer to font resource
	 * \sa clbkCreateFont, GDIFont
	 */
	void clbkReleaseFont (oapi::Font *font) const;

	/**
	 * \brief Create a pen resource for 2-D drawing.
	 * \param style line style (0=invisible, 1=solid, 2=dashed)
	 * \param width line width [pixel]
	 * \param col line colour (format: 0xBBGGRR)
	 * \return Pointer to pen resource
	 * \sa clbkReleasePen, GDIPen
	 */
	oapi::Pen *clbkCreatePen (int style, int width, DWORD col) const;

	/**
	 * \brief De-allocate a pen resource.
	 * \param pen pointer to pen resource
	 * \sa clbkCreatePen, GDIPen
	 */
	void clbkReleasePen (oapi::Pen *pen) const;

	/**
	 * \brief Create a brush resource for 2-D drawing.
	 * \param col brush colour (format: 0xBBGGRR)
	 * \return Pointer to brush resource
	 * \sa clbkReleaseBrush, GDIBrush
	 */
	oapi::Brush *clbkCreateBrush (DWORD col) const;

	/**
	 * \brief De-allocate a brush resource.
	 * \param brush pointer to brush resource
	 * \sa clbkCreateBrush, GDIBrush
	 */
	void clbkReleaseBrush (oapi::Brush *brush) const;
	//@}

	/**
	 * \brief Saves the contents of a surface to a formatted image file or to the clipboard
	 * \param surf surface handle (0 for primary render surface)
	 * \param fname image file path relative to orbiter root directory (excluding file extension), or NULL to save to clipboard
	 * \param fmt output file format
	 * \return \e true on success
	 */
	bool clbkSaveSurfaceToImage (SURFHANDLE surf, const char *fname, oapi::ImageFileFormat fmt, float quality=0.7f);

	/**
	 * \brief Return the number of allocated pens
	 */
	inline int GetPenCount() const { return refCountPen; }

	/**
	 * \brief Return the number of allocated brushes
	 */
	inline int GetBrushCount() const { return refCountBrush; }

	/**
	 * \brief Return the number of allocated fonts
	 */
	inline int GetFontCount() const { return refCountFont; }

private:
	mutable int refCountPen;
	mutable int refCountBrush;
	mutable int refCountFont;
};


// ======================================================================
// class GDIPad
// ======================================================================
/**
 * \brief The GDIPad class defines the context for 2-D drawing using
 *   Windows GDI calls.
 */
class GDICLIENTEXPORT GDIPad: public oapi::Sketchpad {
public:
	GDIPad (SURFHANDLE s, HDC hdc);
	~GDIPad ();

	/**
	 * \brief Selects a new font to use.
	 * \param font pointer to font resource
	 * \return Previously selected font.
	 * \sa GDIFont, GDIClient::clbkCreateFont
	 */
	oapi::Font *SetFont (oapi::Font *font);

	/**
	 * \brief Selects a new pen to use.
	 * \param pen pointer to pen resource, or NULL to disable outlines
	 * \return Previously selected pen.
	 * \sa GDIPen, GDIClient::clbkCreatePen
	 */
	oapi::Pen *SetPen (oapi::Pen *pen);

	/**
	 * \brief Selects a new brush to use.
	 * \param brush pointer to brush resource, or NULL to disable fill mode
	 * \return Previously selected brush.
	 * \sa GDIBrush, GDIClient::clbkCreateBrush
	 */
	oapi::Brush *SetBrush (oapi::Brush *brush);

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
	 * \brief Set the background mode for text and drawing operations.
	 * \param mode background mode (see \ref BkgMode)
	 * \note This function affects text output and dashed line drawing.
	 * \note In opaque background mode, text background and the gaps
	 *   between dashed lines are drawn in the current background colour
	 *   (see SetBackgroundColor). In transparent mode, text background
	 *   and line gaps are not modified.
	 * \note The default background mode (before the first call of
	 *   SetBackgroundMode) is transparent.
	 * \sa SetBackgroundColor, Text, GDIClient::clbkCreatePen
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

	bool TextW (int x, int y, const LPWSTR str, int len);

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

	HDC GetDC() { return hDC; }

private:
	HDC hDC;

	mutable HFONT hFont0;      // original GDI font
	mutable oapi::Font *cfont; // currently selected font (NULL if none)
	mutable oapi::Pen *cpen;   // currently selected pen (NULL if none)
	mutable oapi::Brush *cbrush; // currently selected brush (NULL if none)
};


// ======================================================================
// class GDIFont
// ======================================================================
/**
 * \brief A font resource for drawing text. This is a wrapper for a GDI
 *   HFONT object.
 */
class GDICLIENTEXPORT GDIFont: public oapi::Font {
	friend class GDIPad;
	friend class GDIClient;

public:
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
	 * \note The following face names are currently recognised: 'Courier New',
	 *   'Arial' and 'Times New Roman'. The generic names 'fixed', 'sans' and
	 *   'serif' are mapped to those specific type names, respectively.
	 * \note If the specified face name is not recognised, then 'sans' is
	 *   selected for \e prop==true, and 'fixed' is selected for \e prop==false.
	 */
	GDIFont (int height, bool prop, const char *face, FontStyle style= FontStyle::FONT_NORMAL, int orientation=0);
	GDIFont (int height, char* face, int width, int weight, FontStyle style, float spacing);

	/**
	 * \brief Font destructor.
	 */
	~GDIFont ();

	/**
	 * \brief Return the GDI handle for the font, if available.
	 * \return GDI font handle
	 */
	HFONT GetGDIFont () const { return hFont; }

private:
	HFONT hFont;
};


// ======================================================================
// class GDIPen
// ======================================================================
/**
 * \brief A pen resource for drawing lines and outlines. This class is a
 *   wrapper for a GDI HPEN object.
 */
class GDICLIENTEXPORT GDIPen: public oapi::Pen {
	friend class GDIPad;
	friend class GDIClient;

public:
	/**
	 * \brief Pen constructor.
	 * \param style line style (0=invisible, 1=solid, 2=dashed)
	 * \param width line width [pixel]
	 * \param col line colour (format: 0xBBGGRR)
	 * \note If \e width=0, the pen is drawn with a width of 1 pixel.
	 * \note Dashed line styles are only valid if the width parameter is <= 1.
	 */
	GDIPen (int style, int width, DWORD col);

	/**
	 * \brief Pen destructor.
	 */
	~GDIPen ();

private:
	HPEN hPen;
};

// ======================================================================
// class GDIBrush
// ======================================================================
/**
 * \brief A brush resource for filling figures. This class is a wrapper for
 *   a GDI HBRUSH object.
 */
class GDICLIENTEXPORT GDIBrush: public oapi::Brush {
	friend class GDIPad;
	friend class GDIClient;

public:
	/**
	 * \brief Brush constructor.
	 * \param col line colour (format: 0xBBGGRR)
	 * \Only solid GDI brushes are supported.
	 */
	GDIBrush (DWORD col);

	/**
	 * \brief Brush destructor.
	 */
	~GDIBrush ();

private:
	HBRUSH hBrush;
};

#endif // !__GDICLIENT_H