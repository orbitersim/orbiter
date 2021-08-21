// =================================================================================================================================
//
// Copyright (C) 2012-2018 Jarmo Nikkanen
//
// Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation
// files (the "Software"), to use, copy, modify, merge, publish, distribute, interact with the Software and sublicense copies
// of the Software, subject to the following conditions:
//
// a) You do not sell, rent or auction the Software.
// b) You do not collect distribution fees.
// c) If the Software is distributed in an object code form, it must inform that the source code is available and how to obtain it.
// d) You do not remove or alter any copyright notices contained within the Software.
// e) This copyright notice must be included in all copies or substantial portions of the Software.
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
#include <memory>
#include <stack>
#include "DrawAPI.h"

using namespace oapi;

extern oapi::Font *deffont;
extern oapi::Pen  *defpen;

#define SKPCHG_ALL			0xFFFF
#define SKPCHG_PEN			0x0001
#define SKPCHG_FONT			0x0004
#define SKPCHG_TEXTURE		0x0008
#define SKPCHG_CLIPCONE		0x0010
#define SKPCHG_CLIPRECT		0x0020
#define SKPCHG_TRANSFORM	0x0040
#define SKPCHG_EFFECTS		0x0080
#define SKPCHG_DEPTH		0x0100
#define SKPCHG_TOPOLOGY		0x0200
#define SKPCHG_PATTERN		0x0400


#define SKETCHPAD_NONE		0x0000
#define SKETCHPAD_GDI		0x0001
#define SKETCHPAD_DIRECTX	0x0002

// ===============================================================================================
// Feature Switches			//AARRGGBB
//
// Color source:
#define SKPSW_FRAGMENT		0x00000000	// Index 2
#define SKPSW_PENCOLOR		0x00000080	// Index 2
#define SKPSW_TEXTURE		0x000000FF	// Index 2

// Specials:
#define SKPSW_FONT			0x0000FF00	// Index 1
#define SKPSW_COLORKEY		0x00008000	// Index 1
#define SKPSW_LENGTH		0x00FF0000	// Index 0

// Vertex alignment:
#define SKPSW_CENTER		0x80000000	// Index 3
#define SKPSW_WIDEPEN_L		0x00000000	// Index 3
#define SKPSW_WIDEPEN_R		0xFF000000	// Index 3



// ===============================================================================================
// Special properties
//
#define SKP3E_GAMMA			0x00000001
#define SKP3E_NOISE			0x00000002
#define SKP3E_CMATR			0x00000004


// ===============================================================================================
//
#define nLow 17
#define nHigh 65
#define nQueueMax 2048
#define nIndexMax (nQueueMax * 3)

typedef std::shared_ptr<D3D9Text> D3D9TextPtr;

struct SkpColor {

	SkpColor() {
		dclr = 0;
		fclr = D3DXCOLOR(0, 0, 0, 0);
	}

	explicit SkpColor (DWORD c) {
		dclr = c;
		fclr = D3DXCOLOR(c);
		D3DXCOLORSWAP(&fclr);
	}

	explicit SkpColor(const FVECTOR4 &c) {
		dclr = c.dword_argb();
		fclr.r = c.r; fclr.g = c.g;	fclr.b = c.b; fclr.a = c.a;
		D3DXCOLORSWAP(&fclr);
	}

	DWORD dclr;
	D3DXCOLOR fclr;
};



struct SkpVtx {

	SkpVtx() {
		memset(this, 0, sizeof(SkpVtx));
	};

	float x, y, l;			// vertex x, y, length
	float nx, ny;			// next point
	float px, py;			// perivous point
	DWORD clr, fnc;
};

// GradientFillRect [only]
inline void SkpVtxGF(SkpVtx &v, int _x, int _y, DWORD c)
{
	v.x = float(_x);
	v.y = float(_y);
	v.clr = c;
	v.fnc = SKPSW_CENTER | SKPSW_FRAGMENT;
	v.l = 0.0f;
}

// Fill Rect, Ellipse, Polygon [only]
inline void SkpVtxIC(SkpVtx &v, int _x, int _y, SkpColor &c)
{
	v.x = float(_x);
	v.y = float(_y);
	v.clr = c.dclr;
	v.fnc = SKPSW_CENTER | SKPSW_FRAGMENT;
	v.l = 0.0f;
}

// Copy, Stretch, Colorkey Rect [only]
inline void SkpVtxII(SkpVtx &v, int _tx, int _ty, int _sx, int _sy)
{
	v.x = float(_tx) - 0.5f;
	v.y = float(_ty) - 0.5f;
	v.nx = float(_sx);
	v.ny = float(_sy);
	v.l = 0.0f;
};

// Pattern Fill [only]
inline void SkpVtxPF(SkpVtx &v, int _x, int _y, DWORD c)
{
	v.x = float(_x) - 0.5f;
	v.y = float(_y) - 0.5f;
	v.l = 0.0f;
	v.clr = c;
	v.fnc = SKPSW_FRAGMENT | SKPSW_CENTER;
};


// Rotate Rect [only]
inline void SkpVtxFI(SkpVtx &v, float _x, float _y, int _tx, int _ty)
{
	v.x = _x - 0.5f;
	v.y = _y - 0.5f;
	v.nx = float(_tx);
	v.ny = float(_ty);
	v.l = 0.0f;
};

// D3DTextManager Print Font [only]
inline void SkpVtxFF(SkpVtx &v, float _x, float _y, float _tx, float _ty)
{
	v.x = _x - 0.5f;
	v.y = _y - 0.5f;
	v.nx = _tx;
	v.ny = _ty;
	v.l = 0.0f;
};

template <typename Type> int CheckTriangle(short x, const Type *pt, const WORD *Idx, float hd, short npt, bool bSharp);
template <typename Type> int CreatePolyIndexList(const Type *pt, short npt, WORD *Out);

/**
 * \brief The D3D9Pad class defines the context for 2-D drawing using
 *  DirectX calls.
 */
class D3D9Pad : public Sketchpad
{
	friend D3D9Text;

	mutable struct {
		DWORD style;
		DWORD color;
		float width;
		bool  bEnabled;
	} QPen;

	mutable struct {
		bool  bEnabled;
	} QBrush;

	struct {
		D3DXVECTOR3 uDir;
		float ca, dst;
		bool bEnable;
	} ClipData[2];

	enum Topo { NONE, TRIANGLE, LINE };

public:
	/**
	 * \brief Constructs a drawing object for a given surface.
	 * \param s surface handle
	 */
	explicit D3D9Pad(SURFHANDLE s, const char *name = NULL);
	explicit D3D9Pad(const char *name = NULL);

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
	static void D3D9TechInit(D3D9Client *gc, LPDIRECT3DDEVICE9 pDev);
	static void SinCos(int n, int i);
	/**
	 * \brief Release global parameters
	 */
	static void GlobalExit();

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
	 * \note Filled polygon has a maximum of 64 points.
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

	/**
	 * \brief Obsolete. Will return NULL
	 * \return null
	 */
	HDC GetDC();

	bool TextW(int x, int y, const LPWSTR str, int len = -1);

	// ===============================================================================
	// Sketchpad2 Additions
	// ===============================================================================
	void GetRenderSurfaceSize(LPSIZE size);
	void QuickPen(DWORD color, float width = 1.0f, DWORD style = 0);
	void QuickBrush(DWORD color);
	void SetGlobalLineScale(float width = 1.0f, float pattern = 1.0f);
	void SetWorldTransform(const FMATRIX4 *pWT = NULL);
	void SetWorldTransform2D(float scale=1.0f, float rot=0.0f, IVECTOR2 *c=NULL, IVECTOR2 *t=NULL);
	int  DrawMeshGroup(MESHHANDLE hMesh, DWORD grp, Sketchpad::MeshFlags flags, SURFHANDLE hTex = NULL);
	void CopyRect(SURFHANDLE hSrc, const LPRECT src, int tx, int ty);
	void StretchRect(SURFHANDLE hSrc, const LPRECT src, const LPRECT tgt);
	void RotateRect(SURFHANDLE hSrc, const LPRECT src, int cx, int cy, float angle, float sw = 1.0f, float sh = 1.0f);
	void ColorKey(SURFHANDLE hSrc, const LPRECT src, int tx, int ty);
	void TextEx(float x, float y, const char *str, float scale = 100.0f, float angle = 0.0f);
	void ClipRect(const LPRECT clip = NULL);
	void Clipper(int idx, const VECTOR3 *pPos = NULL, double cos_angle = 0.0, double dist = 0.0);
	void DrawPoly(HPOLY hPoly, DWORD flags = 0);
	void Lines(FVECTOR2 *pt1, int nlines);
	void DepthEnable(bool bEnable);

	void SetViewMode(SkpView mode = ORTHO);
	//-----------------------------------------
	const FMATRIX4 *ViewMatrix() const;
	const FMATRIX4 *ProjectionMatrix() const;
	const FMATRIX4 *GetViewProjectionMatrix() const;
	void SetViewMatrix(const FMATRIX4 *pV = NULL);
	void SetProjectionMatrix(const FMATRIX4 *pP = NULL);




	// ===============================================================================
	// Sketchpad3 Additions
	// ===============================================================================

	const FMATRIX4 *GetColorMatrix();
	void SetColorMatrix(const FMATRIX4 *pMatrix = NULL);
	void SetBrightness(const FVECTOR4 *pBrightness = NULL);
	FVECTOR4 GetRenderParam(RenderParam param);
	void SetRenderParam(RenderParam param, const FVECTOR4 *data = NULL);
	void SetBlendState(BlendState dwState);
	FMATRIX4 GetWorldTransform() const;
	void PushWorldTransform();
	void PopWorldTransform();
	void SetWorldScaleTransform2D(const FVECTOR2 *scl = NULL, const IVECTOR2 *trl = NULL);
	void GradientFillRect(const LPRECT rect, DWORD c1, DWORD c2, bool bVertical = false);
	void ColorFill(DWORD color, const LPRECT tgt);
	void StretchRegion(const skpRegion *rgn, SURFHANDLE hSrc, const LPRECT out);
	void CopyTetragon(SURFHANDLE pSrc, const LPRECT _s, const FVECTOR2 pt[4]);
	void ColorCompatibility(bool bEnable);
	

	// ===============================================================================
	// D3D9Client Privates
	// ===============================================================================

	/**
	*  BeginDrawing(), EndDrawing().
	*  - All the other D3D9Pad functions can be only called/used between the BeginDrawing(), EndDrawing() pairs.
	*  - All rendering that is NOT a part of D3D9Pad is prohibited between the Begin/End pairs.
	*  - Calling Begin/End doesn't alter the D3D9Pad state such as Pens, Brushes, etc...
	*  - EndDrawing() will flush all pending instructions from the draw queue.
	*/
	void EndDrawing();
	void BeginDrawing(LPDIRECT3DSURFACE9 pRenderTgt, LPDIRECT3DSURFACE9 pDepthStensil = NULL);
	void BeginDrawing();


	// ===============================================================================
	// D3D9Client Privates
	// ===============================================================================
	LPD3DXMATRIX WorldMatrix();
	DWORD GetLineHeight(); ///< Return height of a character in the currently selected font with "internal leading"
	const char *GetName() const { return name; }
	LPDIRECT3DSURFACE9 GetRenderTarget() const { return pTgt; }
	bool IsStillDrawing() const { return bBeginDraw; }
	void LoadDefaults();
	bool IsNative() const { return bNative; }

	void CopyRectNative(LPDIRECT3DTEXTURE9 pSrc, const LPRECT s, int tx, int ty);
	void StretchRectNative(LPDIRECT3DTEXTURE9 pSrc, const LPRECT s, const LPRECT t);
	

private:

	bool Topology(Topo tRequest);
	void SetEnable(DWORD config);
	void ClearEnable(DWORD config);

	bool HasPen() const;
	bool HasBrush() const;
	bool IsDashed() const;
	bool IsAlphaTarget() const;

	float GetPenWidth() const;

	void Reset();
	bool Flush(HPOLY hPoly = NULL);
	void AddRectIdx(WORD aV);
	void FillRect(int l, int t, int r, int b, SkpColor &c);
	void TexChange(SURFHANDLE hNew);
	bool TexChangeNative(LPDIRECT3DTEXTURE9 hNew);
	const LPRECT CheckRectNative(LPDIRECT3DTEXTURE9 hSrc, const LPRECT s);
	void SetFontTextureNative(LPDIRECT3DTEXTURE9 hNew);
	void SetupDevice(Topo tNew);
	LPRECT CheckRect(SURFHANDLE hSrc, const LPRECT s);
	void IsLineTopologyAllowed() const;
	DWORD ColorComp(DWORD c) const;
	SkpColor ColorComp(const SkpColor &c) const;

	template <typename Type> void AppendLineVertexList(const Type *pt, int npt, bool bLoop);
	template <typename Type> void AppendLineVertexList(const Type *pt);

	mutable oapi::Font  *cfont;  ///< currently selected font (NULL if none)
	mutable oapi::Pen   *cpen;   ///< currently selected pen (NULL if none)
	mutable oapi::Brush *cbrush; ///< currently selected brush (NULL if none)

	mutable SkpColor pencolor;
	mutable SkpColor brushcolor;
	mutable DWORD	 Change;
	mutable bool	 bLine;
	mutable D3DXMATRIX mVP;

	SkpColor		 textcolor;
	SkpColor		 bkcolor;

	bool			 bColorComp;
	bool			 bNative;
	bool			 bColorKey;
	bool			 bEnableScissor;
	bool			 bDepthEnable;
	bool			 bMustEndScene;
	bool			 bBeginDraw;
	RECT			 ScissorRect;
	D3DXCOLOR		 cColorKey;
	SkpView			 vmode;
	Topo			 tCurrent;


	WORD vI, iI;
	D3DXMATRIX mV, mP, mW, mO;
	D3DXVECTOR4 vTarget;
	DWORD bkmode;
	BlendState dwBlendState;
	TAlign_horizontal tah;
	TAlign_vertical tav;
	float linescale, pattern;
	float zfar;
	int cx, cy;
	RECT src;

	D3DSURFACE_DESC	   tgt_desc;
	LPDIRECT3DSURFACE9 pTgt;
	LPDIRECT3DSURFACE9 pDep;
	LPDIRECT3DTEXTURE9 hTexture;
	LPDIRECT3DTEXTURE9 hFontTex;



	// Sketchpad3 --------------------------------------------------------------
	DWORD RenderConfig;
	DWORD Enable;
	FMATRIX4 ColorMatrix;
	FVECTOR4 Gamma, Noise;
	std::stack<D3DXMATRIX> mWStack;



	// -------------------------------------------------------------------------
	bool  _isSaveBuffer;   ///< Flag indicasting that the 'save buffer' can be used
	char* _saveBuffer;     ///< 'Save' string buffer  (null-terminated @ len)
	int   _saveBufferSize; ///< Current size of the 'save' string buffer

	void        ToSaveBuffer (const char *str, int len);        ///< Store len sized string into internal 'save' buffer
	inline void ReleaseSaveBuffer ();                           ///< Mark the buffer as "not save"
	void        WrapOneLine (char* str, int len, int maxWidth); ///< Wraps one text line at maxLenght pixels
	// -------------------------------------------------------------------------
	char name[32];

	void Log(const char *format, ...) const;
	static FILE *log;
	static CRITICAL_SECTION LogCrit;

	static WORD *Idx;				// List of indices
	static SkpVtx *Vtx;		// List of vertices
	static D3D9Client *gc;
	static LPDIRECT3DDEVICE9 pDev;
	static LPD3DXVECTOR2 pSinCos[5];
	static LPDIRECT3DTEXTURE9 pNoise;
	// -------------------------------------------


	// Rendering pipeline configuration. Applies to every instance of this class
	//
	static ID3DXEffect*	FX;
	static D3DXHANDLE	eDrawMesh;
	static D3DXHANDLE	eSketch;
	static D3DXHANDLE	eWVP;			// Transformation matrix
	static D3DXHANDLE	eTex0;
	static D3DXHANDLE	eFnt0;
	static D3DXHANDLE	eNoiseTex;
	static D3DXHANDLE   eNoiseColor;
	static D3DXHANDLE   eColorMatrix;
	static D3DXHANDLE   eGamma;
	static D3DXHANDLE   eW;
	static D3DXHANDLE   ePen;
	static D3DXHANDLE   eVP;
	static D3DXHANDLE   eFov;
	static D3DXHANDLE   eRandom;
	static D3DXHANDLE   eTarget;
	static D3DXHANDLE   eKey;
	static D3DXHANDLE   eDashEn;
	static D3DXHANDLE   eTexEn;
	static D3DXHANDLE   eKeyEn;
	static D3DXHANDLE   eFntEn;
	static D3DXHANDLE   eWidth;
	static D3DXHANDLE   eWide;
	static D3DXHANDLE   eSize;
	static D3DXHANDLE   eMtrl;
	static D3DXHANDLE   eShade;
	static D3DXHANDLE   ePos;
	static D3DXHANDLE   ePos2;
	static D3DXHANDLE   eCov;
	static D3DXHANDLE   eCovEn;
	static D3DXHANDLE   eClearEn;
	static D3DXHANDLE   eEffectsEn;
};





#define SKP_FONT_CRISP		0x1
#define SKP_FONT_ANTIALIAS	0x2
#define SKP_FONT_CLEARTYPE	0x3
#define SKP_FONT_CP_GREEK	0x10



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
	D3D9PadFont (int height, bool prop, const char *face, Style style=NORMAL, int orientation=0, DWORD flags=0);
	D3D9PadFont (int height, char *face, int width = 0, int weight = 400, int gcFontStyle = 0, float spacing = 0.0f);
	/**
	 * \brief Font destructor.
	 */
	~D3D9PadFont ();

	HFONT	GetGDIFont () const;
	DWORD	GetQuality() const { return Quality; }
	int		GetTextLength(const char *pText, int len) const;
	int		GetIndexByPosition(const char *pText, int pos, int len) const;

private:
	D3D9TextPtr pFont;
	HFONT hFont;
	DWORD Quality;
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
	 * \param col line colour (format: 0xAABBGGRR)
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
	SkpColor clr;
	HPEN hPen;
	//static LPDIRECT3DDEVICE9 pDev;
};





class D3D9PadBrush: public oapi::Brush {
	friend class D3D9Pad;
	friend class GDIPad;

public:
	static void D3D9TechInit(LPDIRECT3DDEVICE9 pDev);

	/**
	 * \brief Brush constructor.
	 * \param col line colour (format: 0xAABBGGRR)
	 * \Only solid GDI brushes are supported.
	 */
	explicit D3D9PadBrush (DWORD col);

	/**
	 * \brief Brush destructor.
	 */
	~D3D9PadBrush ();

private:
	SkpColor clr;
	HBRUSH hBrush;
	//static LPDIRECT3DDEVICE9 pDev;
};






class SketchMesh
{

public:

	struct SKETCHGRP {			// mesh group definition
		DWORD VertOff;			// Main mesh Vertex Offset
		DWORD IdxOff;			// Main mesh Index Offset
		DWORD nIdx;				// Index count
		DWORD nVert;			// Vertex count
		DWORD MtrlIdx;			// material index
		DWORD TexIdx;			// texture index 0=None
	};

	explicit		SketchMesh(LPDIRECT3DDEVICE9 pDev);
					~SketchMesh();

	void			Init();
	bool			LoadMesh(const char *name);
	bool			LoadMeshFromHandle(MESHHANDLE hMesh);
	void			RenderGroup(DWORD idx);
	SURFHANDLE		GetTexture(DWORD idx);
	D3DXCOLOR		GetMaterial(DWORD idx);
	DWORD			GroupCount() const { return nGrp; }

private:

	LPDIRECT3DVERTEXBUFFER9 pVB; ///< (Local) Vertex buffer pointer
	LPDIRECT3DINDEXBUFFER9 pIB;

	DWORD MaxVert;
	DWORD MaxIdx;

	DWORD nGrp;                 // number of mesh groups
	DWORD nMtrl;                // number of mesh materials
	DWORD nTex;                 // number of mesh textures

	LPDIRECT3DDEVICE9 pDev;
	SURFHANDLE *Tex;	// list of mesh textures
	SKETCHGRP *Grp;            // list of mesh groups
	D3DXCOLOR *Mtrl;
};



class D3D9PolyBase {
	DWORD alloc_id;
public:
					D3D9PolyBase(int _type) : alloc_id('POLY') { type = _type; version = 1; }
	virtual			~D3D9PolyBase() { }
	virtual	void	Draw(LPDIRECT3DDEVICE9 pDev) = 0;
	virtual void	Release() = 0;

	int version;
	int type;
};




class D3D9PolyLine : public D3D9PolyBase
{

public:
	D3D9PolyLine(LPDIRECT3DDEVICE9 pDev, const FVECTOR2 *pt, int npt, bool bConnect);
	~D3D9PolyLine();

	void Update(const FVECTOR2 *pt, int npt, bool bConnect);
	void Draw(LPDIRECT3DDEVICE9 pDev);
	void Release();

private:
	bool bLoop;
	WORD nVtx, nPt, nIdx, iI, vI;
	LPDIRECT3DVERTEXBUFFER9 pVB; ///< (Local) Vertex buffer pointer
	LPDIRECT3DINDEXBUFFER9 pIB;
};



class D3D9Triangle : public D3D9PolyBase
{

public:
	D3D9Triangle(LPDIRECT3DDEVICE9 pDev, const gcCore::TriangleVtx *pt, int npt, int style);
	~D3D9Triangle();

	void Update(const gcCore::TriangleVtx *pt, int npt);
	void Draw(LPDIRECT3DDEVICE9 pDev);
	void Release();

private:
	int style;
	WORD nPt;
	LPDIRECT3DVERTEXBUFFER9 pVB;
};


#endif

