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
	
	static void D3D9TechInit(D3D9Client *gc, LPDIRECT3DDEVICE9 pDev, const char *folder);
	static void GlobalExit();

	D3D9Pad(SURFHANDLE s);
	~D3D9Pad();

	HDC GetDC();

	oapi::Font  *SetFont (oapi::Font *font) const;
	oapi::Pen   *SetPen (oapi::Pen *pen) const;
	oapi::Brush *SetBrush (oapi::Brush *brush) const;

	void SetTextAlign(TAlign_horizontal tah=LEFT, TAlign_vertical tav=TOP);

	DWORD SetTextColor (DWORD col);
	DWORD SetBackgroundColor (DWORD col);
	void  SetBackgroundMode (BkgMode mode);

	DWORD GetCharSize ();
	DWORD GetTextWidth (const char *str, int len = 0);

	void MoveTo (int x, int y);
	void LineTo (int x, int y);
	void SetOrigin (int x, int y);
	bool Text (int x, int y, const char *str, int len);
	void Pixel (int x, int y, DWORD col);
	void Line (int x0, int y0, int x1, int y1);
	void Rectangle (int x0, int y0, int x1, int y1);
	void Ellipse (int x0, int y0, int x1, int y1);
	void Polygon (const oapi::IVECTOR2 *pt, int npt);
	void Polyline (const oapi::IVECTOR2 *pt, int npt);

private:

	bool HasPen();
	bool HasBrush();
	bool HasWidePen();
	bool IsDashed();

	void Ellipse2(  float x, float y, float w, float h);
	void Rectangle2(float x, float y, float w, float h);
	void Rectangle3(float x, float y, float w, float h, int width);
	void Lines(D3DXVECTOR3 *lines, int count);

	int	 CheckTriangle(short x, const D3DXVECTOR3 *pt, const WORD *Idx, float hd, short npt, bool bSharp);
	int	 CreatePolyIndexList(const D3DXVECTOR3 *pt, short npt, WORD *Out);
	
	mutable oapi::Font  *cfont;  // currently selected font (NULL if none)
	mutable oapi::Pen   *cpen;   // currently selected pen (NULL if none)
	mutable oapi::Brush *cbrush; // currently selected brush (NULL if none)

	mutable D3DXCOLOR textcolor;
	mutable D3DXCOLOR pencolor;
	mutable D3DXCOLOR brushcolor;
	mutable D3DXCOLOR bkcolor;
	
	DWORD bkmode;
	DWORD halign, valign;
	int origx, origy, cx, cy;
	bool bConvertTgt;
	
	D3D9ClientSurface *pTgt;

	static D3D9Client *gc;
	static LPDIRECT3DDEVICE9 pDev;
	static LPDIRECT3DVERTEXBUFFER9 pCircleLow;
	static LPDIRECT3DVERTEXBUFFER9 pCircleHigh;
	
	// Rendering pipeline configuration. Applies to every instance of this class
	//
	static ID3DXEffect*	FX;			
	static D3DXHANDLE	eTech;		
	static D3DXHANDLE	eEllipse;
	static D3DXHANDLE	eWideRect;
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
	D3D9PadBrush (DWORD col);

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

