// =================================================================================================================================
// The MIT Lisence:
//
// Copyright (C) 2012 Jarmo Nikkanen
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

#ifndef __D3D9PAD_H
#define __D3D9PAD_H

#include "OrbiterAPI.h"
#include "D3D9Client.h"
#include <d3d9.h> 
#include <d3dx9.h>


using namespace oapi;

typedef struct {
	float x,y;
} Vec2D;

extern oapi::Font *deffont;
extern oapi::Pen  *defpen;

/**
 * \brief The D3D9Pad class defines the context for 2-D drawing using
 *  DirectX calls.
 */
class D3D9Pad : public oapi::Sketchpad 
{

public:
	/**
	 * \brief Constructs a drawing object for a given surface.
	 * \param s surface handle
	 */
	explicit D3D9Pad(SURFHANDLE s);

	/**
	 * \brief Destructor. Destroys a drawing object.
	 */
	~D3D9Pad();

	/**
	 * \brief Set up global parameters shared by all instances
	 * \param gclient client instance pointer
	 * \param pDev direct 3D device instance pointer
	 * \param folder shader folder (based on Orbiter's "Modules" path).
	 *   Usually this should be set to "D3D9Client")
	 */
	static void D3D9TechInit(D3D9Client *gc, LPDIRECT3DDEVICE9 pDev, const char *folder);

	/**
	 * \brief Release global parameters
	 */
	static void GlobalExit();

	/**
	 * \brief Return the Windows device context handle, if applicable.
	 * \return device context handle
	 * \default None, returns NULL.
	 * \note The device context returned by this function should not be
	 *   released (e.g. with ReleaseDC). The device context is released
	 *   automatically when the Sketchpad instance is destroyed.
	 * \note This method should be regarded as temporary. Ultimately, the
	 *   device-dependent drawing mechanism should be hidden outside the
	 *   sketchpad implementation.
	 */
	HDC GetDC();

	/**
	 * \brief Selects a new font to use.
	 * \param font pointer to font resource
	 * \return Previously selected font.
	 * \default None, returns NULL.
	 * \sa oapi::Font, oapi::GraphicsClient::clbkCreateFont
	 */
	oapi::Font  *SetFont (oapi::Font *font) const;

	/**
	 * \brief Selects a new pen to use.
	 * \param pen pointer to pen resource, or NULL to disable outlines
	 * \return Previously selected pen.
	 * \default None, returns NULL.
	 * \sa oapi::Pen, oapi::GraphicsClient::clbkCreatePen
	 */
	oapi::Pen   *SetPen (oapi::Pen *pen) const;

	/**
	 * \brief Selects a new brush to use.
	 * \param brush pointer to brush resource, or NULL to disable fill mode
	 * \return Previously selected brush.
	 * \default None, returns NULL.
	 * \sa oapi::Brush, oapi::GraphicsClient::clbkCreateBrush
	 */
	oapi::Brush *SetBrush (oapi::Brush *brush) const;

	/**
	 * \brief Set horizontal and vertical text alignment.
	 * \param tah horizontal alignment
	 * \param tav vertical alignment
	 * \default None.
	 */
	void SetTextAlign(TAlign_horizontal tah=LEFT, TAlign_vertical tav=TOP);

	/**
	 * \brief Set the foreground colour for text output.
	 * \param col colour description (format: 0xBBGGRR)
	 * \return Previous colour setting.
	 * \default None, returns 0.
	 */
	DWORD SetTextColor (DWORD col);

	/**
	 * \brief Set the background colour for text output.
	 * \param col background colour description (format: 0xBBGGRR)
	 * \return Previous colour setting
	 * \default None, returns 0.
	 * \note The background colour is only used if the background mode
	 *   is set to BK_OPAQUE.
	 * \sa SetBackgroundMode
	 */
	DWORD SetBackgroundColor (DWORD col);

	/**
	 * \brief Set the background mode for text output.
	 * \param mode background mode (see \ref BkgMode)
	 * \default None.
	 * \note In opaque background mode, the text background is drawn
	 *   in the current background colour (see SetBackgroundColor).
	 * \note The default background mode (before the first call of
	 *   SetBackgroundMode) should be transparent.
	 * \sa SetBackgroundColor, SetTextColor
	 */
	void  SetBackgroundMode (BkgMode mode);

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
	DWORD GetCharSize ();

	/**
	 * \brief Return the width of a text string in the currently selected font.
	 * \param str text string
	 * \param len string length, or 0 for auto (0-terminated string)
	 * \return width of the string, drawn in the currently selected font [pixel]
	 * \default None, returns 0.
	 * \sa SetFont
	 */
	DWORD GetTextWidth (const char *str, int len = 0);

	/**
	 * \brief Move the drawing reference to a new point.
	 * \param x x-coordinate of new reference point [pixel]
	 * \param y y-coordinate of new reference point [pixel]
	 * \note Some methods use the drawing reference point for
	 *   drawing operations, e.g. \ref LineTo.
	 * \default None.
	 * \sa LineTo
	 */
	void MoveTo (int x, int y);

	/**
	 * \brief Draw a line to a specified point.
	 * \param x x-coordinate of line end point [pixel]
	 * \param y y-coordinate of line end point [pixel]
	 * \default None.
	 * \note The line starts at the current drawing reference
	 *   point.
	 * \sa MoveTo
	 */
	void LineTo (int x, int y);

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
	 * \sa GetOrigin
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
	 * \brief Draw a text string.
	 * \param x reference x position [pixel]
	 * \param y reference y position [pixel]
	 * \param str text string
	 * \param len string length for output
	 * \return \e true on success, \e false on failure.
	 * \default None, returns false.
	 */
	bool Text (int x, int y, const char *str, int len);

	/**
	 * \brief Draw a single pixel in a specified colour.
	 * \param x x-coordinate of point [pixel]
	 * \param y y-coordinate of point [pixel]
	 * \param col pixel colour (format: 0xBBGGRR)
	 */
	void Pixel (int x, int y, DWORD col);

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
	void Line (int x0, int y0, int x1, int y1);

	/**
	 * \brief Draw a rectangle (filled or outline).
	 * \param x0 left edge of rectangle [pixel]
	 * \param y0 top edge of rectangle [pixel]
	 * \param x1 right edge of rectangle [pixel]
	 * \param y1 bottom edge of rectangle [pixel]
	 * \default Draws the rectangle from 4 line segments and
	 *   fills the rectangle with the currently selected brush resource.
	 * \sa MoveTo, LineTo, Ellipse, Polygon
	 */
	void Rectangle (int x0, int y0, int x1, int y1);

	/**
	 * \brief Draw an ellipse from its bounding box.
	 * \param x0 left edge of bounding box [pixel]
	 * \param y0 top edge of bounding box [pixel]
	 * \param x1 right edge of bounding box [pixel]
	 * \param y1 bottom edge of bounding box [pixel]
	 * \default None.
	 * \note The ellipse is filled with the currently selected
	 *   brush resource.
	 * \sa Rectangle, Polygon
	 */
	void Ellipse (int x0, int y0, int x1, int y1);

	/**
	 * \brief Draw a closed polygon given by vertex points.
	 * \param pt list of vertex points
	 * \param npt number of points in the list
	 * \default None.
	 * \note The polygon should be closed, i.e. the last point
	 *   joined with the first one.
	 * \note The outline of the polygon is drawn with the 
	 *   current pen, and filled with the current brush.
	 * \sa Polyline, PolyPolygon, Rectangle, Ellipse
	 */
	void Polygon (const oapi::IVECTOR2 *pt, int npt);

	/**
	 * \brief Draw a line of piecewise straight segments.
	 * \param pt list of vertex points
	 * \param npt number of points in the list
	 * \default None
	 * \note The line is drawn with the currently selected pen.
	 * \note Polylines are open figures: the end points are
	 *   not connected, and no fill operation is performed.
	 * \sa Polygon, PolyPolyline, Rectangle, Ellipse
	 */
	void Polyline (const oapi::IVECTOR2 *pt, int npt);

private:

	bool HasPen();
	bool HasBrush();
	bool HasWidePen();
	bool HasThinPen();
	bool IsDashed();

	float GetPenWidth();


	void Ellipse2(  float x, float y, float w, float h);
	void Rectangle2(float x, float y, float w, float h);
	
	inline D3DXVECTOR2 _DXV2(const IVECTOR2 *pt);
	int	 CheckTriangle(short x, const D3DXVECTOR3 *pt, const WORD *Idx, float hd, short npt, bool bSharp);
	int	 CreatePolyIndexList(const D3DXVECTOR3 *pt, short npt, WORD *Out);
	void CreateLineVertexList(D3DXVECTOR3 *Vtx, const IVECTOR2 *pt, int npt, bool bLoop);
	
	mutable oapi::Font  *cfont;  ///< currently selected font (NULL if none)
	mutable oapi::Pen   *cpen;   ///< currently selected pen (NULL if none)
	mutable oapi::Brush *cbrush; ///< currently selected brush (NULL if none)

	mutable D3DXCOLOR textcolor;
	mutable D3DXCOLOR pencolor;
	mutable D3DXCOLOR brushcolor;
	mutable D3DXCOLOR bkcolor;
	
	DWORD bkmode;
	DWORD halign, valign;
	int origx, origy, cx, cy;
	bool bConvert;
	
	D3D9ClientSurface *pTgt;

	static D3D9Client *gc;
	static LPDIRECT3DDEVICE9 pDev;
	static LPDIRECT3DVERTEXBUFFER9 pCircleLow;
	static LPDIRECT3DVERTEXBUFFER9 pCircleHigh;
	static LPD3DXVECTOR2 pSinCosLow;
	static LPD3DXVECTOR2 pSinCosHigh;
	
	// Rendering pipeline configuration. Applies to every instance of this class
	//
	static ID3DXEffect*	FX;				
	static D3DXHANDLE	eEllipse;
	static D3DXHANDLE	eLine;
	static D3DXHANDLE	eVP;	// Transformation matrix
	static D3DXHANDLE	eColor;	
	static D3DXHANDLE	eTex0;	
	static D3DXHANDLE   eData;
	static D3DXHANDLE   eDash;
};




class D3D9PadFont: public oapi::Font {

	friend class D3D9Pad;
	friend class GDIPad;
	
public:

	static void D3D9TechInit(LPDIRECT3DDEVICE9 pDev);
	
	/**
	 * \brief Font constructor. 
	 * \param height cell or character height [pixel]
	 * \param prop proportional/fixed width flag
	 * \param face font face name
	 * \param style font decoration
	 * \param orientation text orientation [1/10 deg]
	 * \note if \e height > 0, it represents the font cell height. if height < 0,
	 *   its absolute value represents the character height.
	 * \note The \e style parameter can be any combination of the \ref Style
	 *   enumeration items.
	 * \note The following face names are currently recognised: 'Courier New',
	 *   'Arial' and 'Times New Roman'. The generic names 'fixed', 'sans' and
	 *   'serif' are mapped to those specific type names, respectively.
	 * \note if the specified face name is not recognised, then 'sans' is
	 *   selected for \e prop==true, and 'fixed' is selected for \e prop==false.
	 */
	D3D9PadFont (int height, bool prop, const char *face, Style style=NORMAL, int orientation=0);
	
	/**
	 * \brief Font destructor.
	 */
	~D3D9PadFont ();

	HFONT GetGDIFont () const;

private:
	class D3D9Text *pFont;
	HFONT hFont;
	float rotation;
	static LPDIRECT3DDEVICE9 pDev;
};



class D3D9PadPen: public oapi::Pen {

	friend class D3D9Pad;
	friend class GDIPad;

public:
	static void D3D9TechInit(LPDIRECT3DDEVICE9 pDev);

	/**
	 * \brief Pen constructor.
	 * \param style line style (0=invisible, 1=solid, 2=dashed)
	 * \param width line width [pixel]
	 * \param col line colour (format: 0xBBGGRR)
	 * \note if \e width=0, the pen is drawn with a width of 1 pixel.
	 * \note Dashed line styles are only valid if the width parameter is <= 1.
	 */
	D3D9PadPen (int style, int width, DWORD col);

	/**
	 * \brief Pen destructor.
	 */
	~D3D9PadPen ();

private:
	int style;
	int width;
	D3DXCOLOR fcolor;
	HPEN hPen;
	static LPDIRECT3DDEVICE9 pDev;
};



class D3D9PadBrush: public oapi::Brush {
	friend class D3D9Pad;
	friend class GDIPad;
	
public:
	static void D3D9TechInit(LPDIRECT3DDEVICE9 pDev);

	/**
	 * \brief Brush constructor.
	 * \param col line colour (format: 0xBBGGRR)
	 * \Only solid GDI brushes are supported.
	 */
	explicit D3D9PadBrush (DWORD col);

	/**
	 * \brief Brush destructor.
	 */
	~D3D9PadBrush ();

private:
	D3DXCOLOR fcolor;
	HBRUSH hBrush;
	static LPDIRECT3DDEVICE9 pDev;
};



#endif 

