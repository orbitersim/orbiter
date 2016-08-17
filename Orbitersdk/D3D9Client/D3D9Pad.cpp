// =================================================================================================================================
//
// Copyright (C) 2012-2016 Jarmo Nikkanen
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


#include "D3D9Pad.h"
#include "D3D9Client.h"
#include "D3D9Surface.h"
#include "D3D9Util.h"
#include "D3D9TextMgr.h"
#include "D3D9Config.h"
#include "Log.h"
#include "Mesh.h"
#include "Sketchpad2.h"

using namespace oapi;


struct FontCache {
	int height;
	int orient;
	bool prop;
	char face[64];
	Font::Style style;
	class D3D9Text *pFont;
} fcache[256];


int nfcache = 0;

oapi::Font * deffont = 0;
oapi::Pen * defpen = 0;


// ===============================================================================================
//
void D3D9Pad::D3D9TechInit(D3D9Client *_gc, LPDIRECT3DDEVICE9 pDevice)
{
	memset2(fcache, 0, 256*sizeof(FontCache));
	nfcache = 0;

	pDev = pDevice;
	gc = _gc;

	Idx = new WORD[3 * nQueueMax + 3];
	Vtx = new SkpVtx[3 * nQueueMax + 3];

	pSinCos = new D3DXVECTOR2[64];

	float q = 0.0f; // float(PI2) / 256.0f;
	float s = float(PI2) / 64.0f;
	// ------------------------------------------------------------------
	for (int i = 0; i<64; i++) {
		pSinCos[i].x = sin(q);
		pSinCos[i].y = cos(q);
		q += s;
	}

	// Initialize Techniques -------------------------------------------------------------------------
	//
	char name[256];
	sprintf_s(name, 256, "Modules/D3D9Client/Sketchpad.fx");

	// Create the Effect from a .fx file.
	ID3DXBuffer* errors = 0;

	HR(D3DXCreateEffectFromFileA(pDev, name, 0, 0, 0, 0, &FX, &errors));

	if (errors) {
		LogErr("Effect Error: %s",(char*)errors->GetBufferPointer());
		MessageBoxA(0, (char*)errors->GetBufferPointer(), "Sketchpad.fx Error", 0);
		FatalAppExitA(0,"Critical error has occured. See Orbiter.log for details");
	}

	if (FX==0) {
		LogErr("Failed to create an Effect (%s)",name);
		MissingRuntimeError();
		return;
	}

	eDrawMesh = FX->GetTechniqueByName("SketchMesh");
	eSketch   = FX->GetTechniqueByName("SketchTech");
	eVP       = FX->GetParameterByName(0, "gVP");
	eTex0     = FX->GetParameterByName(0, "gTex0");
	eDashEn   = FX->GetParameterByName(0, "gDashEn");
	eW		  = FX->GetParameterByName(0, "gW");
	eKey	  = FX->GetParameterByName(0, "gKey");
	ePen      = FX->GetParameterByName(0, "gPen");
	eWVP	  = FX->GetParameterByName(0, "gWVP");
	eFov	  = FX->GetParameterByName(0, "gFov");
	eTarget	  = FX->GetParameterByName(0, "gTarget");
	eTexEn	  = FX->GetParameterByName(0, "gTexEn");
	eKeyEn    = FX->GetParameterByName(0, "gKeyEn");
	eWide	  = FX->GetParameterByName(0, "gWide");
	eWidth	  = FX->GetParameterByName(0, "gWidth");
	eSize	  = FX->GetParameterByName(0, "gSize");
	eMtrl	  = FX->GetParameterByName(0, "gMtrl");
	eShade    = FX->GetParameterByName(0, "gShade");
	ePos	  = FX->GetParameterByName(0, "gPos");
	ePos2	  = FX->GetParameterByName(0, "gPos2");
	eCov	  = FX->GetParameterByName(0, "gCov");
	eCovEn	  = FX->GetParameterByName(0, "gClipEn");
	eClearEn  = FX->GetParameterByName(0, "gClearEn");
}


// ===============================================================================================
//
void D3D9Pad::GlobalExit()
{
	LogAlw("Clearing Font Cache... %d Fonts are stored in the cache",nfcache);
	for (int i=0;i<nfcache;i++) if (fcache[i].pFont) delete fcache[i].pFont;

	SAFE_RELEASE(FX);
	SAFE_DELETEA(Idx);
	SAFE_DELETEA(Vtx);
	SAFE_DELETEA(pSinCos);

	memset2(fcache, 0, 256*sizeof(FontCache));
}


// ===============================================================================================
//
void D3D9Pad::Reset()
{

	assert(CurrentTech == 0);	// Must EndDrawing before calling reset

	cfont = deffont;
	cpen = NULL;
	cbrush = NULL;
	hOldMesh = NULL;
	cx = 0;
	cy = 0;
	vI = 0;
	iI = 0;
	bkmode = TRANSPARENT;
	tah = LEFT;
	tav = TOP;
	linescale = 1.0f;
	pattern = 1.0f;
	CurrentTech = 0;

	vmode = ORTHO;

	bPenChange = true;		// New setup required
	bFontChange = true;		// New setup required
	bViewChange = true;		// New setup required

	memset(ClipData, 0, sizeof(ClipData));

	QPen.bEnabled = false;
	QBrush.bEnabled = false;

	brushcolor = SkpColor(0xFF00FF00);
	bkcolor    = SkpColor(0xFF000000);
	textcolor  = SkpColor(0xFF00FF00);
	pencolor   = SkpColor(0xFF00FF00);

	zfar = float(max(tgt_desc.Width, tgt_desc.Height));

	D3DXMatrixIdentity(&mW);
	D3DXMatrixIdentity(&mP);
	D3DXMatrixIdentity(&mV);

	D3DXMatrixOrthoOffCenterLH(&mO, 0.0f, (float)tgt_desc.Width, (float)tgt_desc.Height, 0.0f, 0.0f, zfar);

	HR(FX->SetBool(eCovEn, false));
	HR(FX->SetVector(eTarget, &D3DXVECTOR4(2.0f/(float)tgt_desc.Width, 2.0f/(float)tgt_desc.Height, (float)tgt_desc.Width, (float)tgt_desc.Height)));
}


// ======================================================================
// class GDIPad
// ======================================================================

D3D9Pad::D3D9Pad(SURFHANDLE s) : Sketchpad2(s),
	_isSaveBuffer(false),
	_saveBuffer(NULL),
	_saveBufferSize(0)
{
	_TRACE;

	pTgt = SURFACE(s);

	SURFACE(GetSurface())->SketchPad = SKETCHPAD_DIRECTX;

	if (pTgt->IsBackBuffer()==false) {
		if (pTgt->BindGPU()==false) {
			pTgt=NULL;
			LogErr("D3D9Pad creation failed");
			return;
		}
	}

	pTgt->GetDesc(&tgt_desc);

	CurrentTech = 0;

	Reset();
}


// ===============================================================================================
//
D3D9Pad::D3D9Pad(LPDIRECT3DSURFACE9 s) : Sketchpad2(NULL),
	_isSaveBuffer(false),
	_saveBuffer(NULL),
	_saveBufferSize(0)
{
	_TRACE;
	pTgt = NULL;
	s->GetDesc(&tgt_desc);

	CurrentTech = 0;

	Reset();
}


// ===============================================================================================
//
D3D9Pad::~D3D9Pad ()
{
	_TRACE;

	EndDrawing();

	pDev->SetRenderState(D3DRS_SCISSORTESTENABLE, 0);

	if (GetSurface()) {
		if (pTgt) if (pTgt->IsBackBuffer() == false) pTgt->ReleaseGPU();
		SURFACE(GetSurface())->SketchPad = SKETCHPAD_NONE;
	}

	SAFE_DELETEA(_saveBuffer);

	pTgt = NULL;
}


// ===============================================================================================
//
HDC D3D9Pad::GetDC()
{
	if (pTgt->IsBackBuffer()) return NULL;
	if (!pTgt->bSkpGetDCEr) {
		LogWrn(" - ! - Never Use Sketchpad::GetDC() !! hDC not available, the surface is active render target at a moment - ! -");
		pTgt->bSkpGetDCEr = true;
	}
	return NULL;
}


// ===============================================================================================
//
Font *D3D9Pad::SetFont(Font *font) const
{
	bFontChange = true;

	Font *pfont = cfont;
	if (font) cfont = font;
	else      cfont = deffont;
	return pfont;
}


// ===============================================================================================
//
Brush *D3D9Pad::SetBrush (Brush *brush) const
{
	bPenChange = true;
	QBrush.bEnabled = false;

	Brush *pbrush = cbrush;
	cbrush = brush;
	if (cbrush) brushcolor = static_cast<D3D9PadBrush *>(cbrush)->clr;
	else	    brushcolor = SkpColor(0);
	return pbrush;
}


// ===============================================================================================
//
Pen *D3D9Pad::SetPen (Pen *pen) const
{
	bPenChange = true;
	QPen.bEnabled = false;

	Pen *ppen = cpen;
	if (pen) cpen = pen;
	else     cpen = NULL;
	if (cpen) pencolor = static_cast<D3D9PadPen *>(cpen)->clr;
	return ppen;
}


// ===============================================================================================
//
void D3D9Pad::SetTextAlign (TAlign_horizontal _tah, TAlign_vertical _tav)
{
	tah = _tah;
	tav = _tav;
}


// ===============================================================================================
//
DWORD D3D9Pad::SetTextColor(DWORD col)
{
	bFontChange = true;
	DWORD prev = textcolor.dclr;
	textcolor = SkpColor(col);
	return prev;
}


// ===============================================================================================
//
DWORD D3D9Pad::SetBackgroundColor(DWORD col)
{
	bFontChange = true;
	DWORD prev = bkcolor.dclr;
	bkcolor = SkpColor(col);
	return prev;
}


// ===============================================================================================
//
void D3D9Pad::SetBackgroundMode(BkgMode mode)
{
	// Does not require a Flush

	switch (mode) {
		case BK_TRANSPARENT: bkmode = TRANSPARENT; break;
		case BK_OPAQUE:      bkmode = OPAQUE; break;
	}
}


// ===============================================================================================
//
DWORD D3D9Pad::GetCharSize ()
{
	TEXTMETRIC tm;
	if (cfont==NULL) return 0;
	static_cast<D3D9PadFont *>(cfont)->pFont->GetD3D9TextMetrics(&tm);
	return MAKELONG(tm.tmHeight-tm.tmInternalLeading, tm.tmAveCharWidth);
}


// ===============================================================================================
//
DWORD D3D9Pad::GetTextWidth (const char *str, int len)
{
	if (str) if (str[0] == '_') if (strcmp(str, "_SkpVerInfo") == 0) return 2;
	if (cfont==NULL) return 0;
	return DWORD(static_cast<D3D9PadFont *>(cfont)->pFont->Length2(str, len));
}


// ===============================================================================================
//
void D3D9Pad::SetOrigin (int x, int y)
{
	bViewChange = true;

	mW._41 = float(x);
	mW._42 = float(y);
}


// ===============================================================================================
//
void D3D9Pad::GetOrigin(int *x, int *y) const
{
	if (x) *x = int(mW._41);
	if (y) *y = int(mW._42);
}


// ===============================================================================================
//
bool D3D9Pad::HasPen()
{
	if (QPen.bEnabled) return true;
	if (cpen==NULL) return false;
	if (static_cast<D3D9PadPen*>(cpen)->style==PS_NULL) return false;
	return true;
}


// ===============================================================================================
//
bool D3D9Pad::IsDashed()
{
	if (QPen.bEnabled) return QPen.style == 2;
	if (cpen==NULL) return false;
	if (static_cast<D3D9PadPen*>(cpen)->style==PS_DOT) return true;
	return false;
}


// ===============================================================================================
//
bool D3D9Pad::HasBrush()
{
	if (QBrush.bEnabled) return true;
	return (cbrush != NULL);
}


// ===============================================================================================
//
float D3D9Pad::GetPenWidth()
{
	if (QPen.bEnabled) return linescale * QPen.width;
	if (cpen==NULL) return 1.0f;
	return float(static_cast<D3D9PadPen*>(cpen)->width*linescale);
}


// ===============================================================================================
//
void D3D9Pad::WrapOneLine (char* str, int len, int maxWidth)
{
	D3D9Text *pText = static_cast<D3D9PadFont *>(cfont)->pFont;
	if (pText->Length2(str) > maxWidth) {
		char *pStr = str, // sub-string start
		     *it = pStr,  // 'iterator' char
		     *pEnd = str + len, // <= point to terminating zero
		     *pLastSpace = NULL;
		float currentWidth = 0;
		while (it < pEnd)
		{
			while (it < pEnd && currentWidth < maxWidth) {
				if (*it == ' ') { pLastSpace = it; }
				currentWidth = pText->Length2( pStr, (it - pStr + 1) );
				++it;
			}
			// only split if we have space for it AND we have to (avoids cutting the last word)
			if (pLastSpace != NULL && currentWidth >= maxWidth) {
				*pLastSpace = '\n';
				pStr = pLastSpace + 1; // skip the space (now a newline)
				currentWidth = 0;
				pLastSpace = NULL;
			}
		}
	}
}

// ===============================================================================================
//
bool D3D9Pad::TextBox (int x1, int y1, int x2, int y2, const char *str, int len)
{
	if (cfont==NULL) return false;

	bool result = true;
	int lineSpace = static_cast<D3D9PadFont *>(cfont)->pFont->GetLineSpace();

	ToSaveBuffer(str, len);

	char *pch, *pEnd =_saveBuffer+len; // <= point to terminating zero
	for (pch = strtok(_saveBuffer, "\n"); pch != NULL; pch = strtok(NULL, "\n"))
	{
		int _len = strlen(pch);
		if (_len>1) { WrapOneLine(pch, _len, x2-x1); }
		if (pch+_len < pEnd) { *(pch+_len) = '\n'; } // strtok splits by inserting '\0's => revert'em
	}

	// "forEach(line...)" split multi-lines
	for (pch = strtok(_saveBuffer, "\n"); pch != NULL; pch = strtok(NULL, "\n")) {
		result = Text(x1, y1, pch, -1); // len is irrelevant for pointer into 'save' buffer
		y1 += lineSpace;
	}

	ReleaseSaveBuffer();
	return result;
}

// ===============================================================================================
//
bool D3D9Pad::Text (int x, int y, const char *str, int len)
{

	if (cfont==NULL) return false;

	D3D9Text *pText = static_cast<D3D9PadFont *>(cfont)->pFont;

	switch(tah) {
		default:
		case LEFT:   pText->SetTextHAlign(0); break;
		case CENTER: pText->SetTextHAlign(1); break;
		case RIGHT:  pText->SetTextHAlign(2); break;
	}

	switch(tav) {
		default:
		case TOP:      pText->SetTextVAlign(0); break;
		case BASELINE: pText->SetTextVAlign(1); break;
		case BOTTOM:   pText->SetTextVAlign(2); break;
	}

	pText->SetRotation(static_cast<D3D9PadFont *>(cfont)->rotation);
	pText->SetScaling(1.0f);
	pText->PrintSkp(this, float(x - 1), float(y - 1), str, len, (bkmode == OPAQUE));

	return true;
}


// ===============================================================================================
//
void SwapRB(DWORD *c)
{
	DWORD r = ((*c) & 0x00FF0000) >> 16;
	DWORD b = ((*c) & 0x000000FF) << 16;
	*c = ((*c) & 0xFF00FF00) | b | r;
}


// ===============================================================================================
//
void D3D9Pad::Pixel (int x, int y, DWORD col)
{
	Flush(SKPTECH_PIXLES);
	SwapRB(&col);
	RECT rect = { x, y, x+1, y+1 };
	if (pTgt) pDev->ColorFill(pTgt->pSurf, &rect, col);
}


// ===============================================================================================
//
void D3D9Pad::MoveTo (int x, int y)
{
	cx = x;
	cy = y;
}


// ===============================================================================================
//
void D3D9Pad::LineTo (int tx, int ty)
{
	if (!HasPen()) return;
	Line(cx, cy, tx, ty);
	cx=tx; cy=ty;
}


// ===============================================================================================
//
void D3D9Pad::Line (int x0, int y0, int x1, int y1)
{
	Flush(SKPTECH_DRAW);

	IVECTOR2 pt[2];

	pt[0].x = x0; pt[0].y = y0;
	pt[1].x = x1; pt[1].y = y1;

	AppendLineVertexList<IVECTOR2>(pt);

	cx = x1; cy = y1;
}


// ===============================================================================================
//
void D3D9Pad::FillRect(int l, int t, int r, int b, SkpColor &c)
{
	assert(bTriangles);

	AddRectIdx(vI);
	SkpVtxIC(Vtx[vI++], l, t, c);
	SkpVtxIC(Vtx[vI++], r, t, c);
	SkpVtxIC(Vtx[vI++], r, b, c);
	SkpVtxIC(Vtx[vI++], l, b, c);
}


// ===============================================================================================
//
void D3D9Pad::Rectangle (int l, int t, int r, int b)
{
	if (r <= l) return;
	if (b <= t) return;

	Flush(SKPTECH_DRAW);

	r--;
	b--;

	WORD iIdx = iI;

	// Fill interion ----------------------------------------------
	//
	if (HasBrush()) FillRect(l, t, r, b, brushcolor);

	// Draw outline ------------------------------------------
	//
	if (HasPen()) {

		IVECTOR2 pts[4];
		pts[0].x = pts[3].x = l;
		pts[0].y = pts[1].y = t;
		pts[1].x = pts[2].x = r;
		pts[2].y = pts[3].y = b;

		AppendLineVertexList<IVECTOR2>(pts, 4, true);
	}
}


// ===============================================================================================
//
void D3D9Pad::Ellipse (int l, int t, int r, int b)
{
	if (r <= l) return;
	if (b <= t) return;

	Flush(SKPTECH_DRAW);

	float w = float(r - l); float h = float(b - t);	float fl = float(l); float ft = float(t);
	DWORD z = max((r-l), (b-t));

	w *= 0.5f;
	h *= 0.5f;
	fl += w;
	ft += h;

	IVECTOR2 pts[65];

	WORD k = 4;
	WORD s = 8;
	WORD n = 8;
	if (z > 16) k = 2, s = 4, n = 16;
	if (z > 32) k = 1, s = 2, n = 32;
	if (z > 64) k = 0, s = 1, n = 64;

	for (WORD i = 0; i<n; i++) {
		pts[i].x = long(fl + pSinCos[k].x * w);
		pts[i].y = long(ft + pSinCos[k].y * h);
		k += s;
	}

	WORD iIdx = iI;

	// Fill interion -------------------------------------------
	//
	if (HasBrush()) {

		assert(bTriangles);

		WORD aV = vI;

		SkpVtxIC(Vtx[vI++], (r + l) / 2, (b + t) / 2, brushcolor);

		for (WORD i = 0; i < n; i++) SkpVtxIC(Vtx[vI++], pts[i].x, pts[i].y, brushcolor);
		for (WORD i = 0; i < n; i++) {
			Idx[iI++] = aV;
			Idx[iI++] = aV + i + 1;
			Idx[iI++] = aV + i + 2;
		}
		Idx[iI-1] = aV + 1;
	}

	// Draw outline ------------------------------------------
	//
	if (HasPen()) AppendLineVertexList<IVECTOR2>(pts, n, true);
}


// ===============================================================================================
//
void D3D9Pad::Polygon (const IVECTOR2 *pt, int npt)
{
	Flush(SKPTECH_DRAW);

	if (npt<3) return;
	if (HasBrush() && npt > 64) return;

	// Create filled polygon interior -----------------------------------------
	//
	if (HasBrush()) {

		assert(bTriangles);
		int sIdx = vI;

		// File a vertex buffer.
		for (int i = 0; i<npt; i++) SkpVtxIC(Vtx[vI++], pt[i].x, pt[i].y, brushcolor);

		WORD qIdx[256];
		int nIdx = CreatePolyIndexList<IVECTOR2>(pt, npt, qIdx);

		// Add indices to index buffer
		for (int i = 0; i < nIdx; i++) Idx[iI++] = qIdx[i] + sIdx;
	}

	// Draw outline ------------------------------------------
	//
	if (HasPen()) AppendLineVertexList<IVECTOR2>(pt, npt, true);
}


// ===============================================================================================
//
void D3D9Pad::Polyline (const IVECTOR2 *pt, int npt)
{
	Flush(SKPTECH_DRAW);

	if (npt < 2) return;

	if (HasPen()) AppendLineVertexList<IVECTOR2>(pt, npt, false);
}


// ===============================================================================================
//
void D3D9Pad::DrawPoly (HPOLY hPoly, DWORD flags)
{
	Flush(SKPTECH_POLY);
	if (HasPen()) static_cast<D3D9PolyLine *>(hPoly)->Draw(pDev, flags);
}


// ===============================================================================================
//
void D3D9Pad::Lines(FVECTOR2 *pt, int nlines)
{
	Flush(SKPTECH_DRAW);
	if (HasPen()) {
		for (int i = 0; i < nlines; i++) {
			AppendLineVertexList<FVECTOR2>(pt);
			pt += 2;
		}
	}
}


// -----------------------------------------------------------------------------------------------
// Save buffer helpers (null-terminated string for Text & TextBox)
// -----------------------------------------------------------------------------------------------

// ===============================================================================================
// Copy string to internal 'save' buffer, so it can be changed (adding terminating zeroes, etc.)
void D3D9Pad::ToSaveBuffer (const char *str, int len)
{
	if (_saveBufferSize < len)
	{ // re-allloc bigger space
		if (_saveBuffer) { delete[] _saveBuffer; }
		_saveBuffer = new char[len + 1];
		_saveBufferSize = len;
	}
	strncpy_s(_saveBuffer, len + 1, str, len);
	_isSaveBuffer = true;
}

// ===============================================================================================
//
void D3D9Pad::ReleaseSaveBuffer () {
	_isSaveBuffer = false;
}


// -----------------------------------------------------------------------------------------------
// Subroutines Section
// -----------------------------------------------------------------------------------------------

// ===============================================================================================
//
short mod(short a, short b)
{
	if (a<0) return b-1;
	if (a>=b) return 0;
	return a;
}


// ===============================================================================================
//
template <typename Type>
int CheckTriangle(short x, const Type *pt, const WORD *Idx, float hd, short npt, bool bSharp)
{
	WORD A = Idx[x];
	WORD B = Idx[mod(x-1,npt)];
	WORD C = Idx[mod(x+1,npt)];

	float bx = float(pt[B].x - pt[A].x);
	float by = float(pt[B].y - pt[A].y);
	float ax = float(pt[C].x - pt[A].x);
	float ay = float(pt[C].y - pt[A].y);

	if ((bx*ay-by*ax)*hd > 0) return 0;	// Check handiness

	float aa = ax*ax + ay*ay;			// dot(a,a)
	float ab = ax*bx + ay*by;			// dot(a,b)
	float bb = bx*bx + by*by;			// dot(b,b)

	float qw = fabs(ab) / sqrt(aa*bb);	// abs(cos(a,b))
	if (bSharp && qw>0.9f) return 0;	// Bad Ear
	if (qw>0.9999f) return 0;			// All three points are lined up

	float id = 1.0f / (aa * bb - ab * ab);

	for (int i=0;i<npt;i++) {

		WORD P = Idx[i];

		if (P==B || P==A || P==C) continue;

		float cx = float(pt[P].x - pt[A].x);
		float cy = float(pt[P].y - pt[A].y);
		float ac = ax*cx + ay*cy;
		float bc = bx*cx + by*cy;
		float u  = (bb*ac - ab*bc) * id;
		float v  = (aa*bc - ab*ac) * id;

		// Check if the point is inside the triangle
		// NOTE: Having u+v slightly above 1.0 is a bad condition, should find a better ear.
		if  ((u>-0.001) && (v>-0.001) && ((u+v)<1.001f)) return 0;
	}

	return 1; // It's an ear
}


// ===============================================================================================
//
template <typename Type>
int CreatePolyIndexList(const Type *pt, short npt, WORD *Out)
{
	if (npt > 255) return 0;
	if (npt==3) { Out[0]=0; Out[1]=1; Out[2]=2;	return 3; }

	short idx = 0;		// Number of indices written in the output
	short x = npt-1;		// First ear to test is the last one in the list
	bool bSharp = true; // Avoid sharp ears

	// Build initial index list
	WORD In[256];
	for (int i=0;i<npt;i++) In[i]=i;
	float sum = 0;
	int k = npt-1;
	for (int i=0;i<k;i++) sum += (float(pt[i].x)*float(pt[(i+1)%k].y) - float(pt[(i+1)%k].x)*float(pt[i].y));

	if (sum>0) sum=1.0; else sum=-1.0;

	while (npt>3) {

		switch (CheckTriangle<Type>(x, pt, In, sum, npt, bSharp)) {

			case 0:
			{
				x--;
				if (x<0) { // Restart
					if (!bSharp) { 
						LogErr("bSharp Exiting PolyTri");
						return idx;	
					}
					bSharp=false;
					x=npt-1;
				}
				break;
			}

			case 1:
			{
				Out[idx] = In[mod(x-1,npt)]; idx++;
				Out[idx] = In[mod(x,npt)]; idx++;
				Out[idx] = In[mod(x+1,npt)]; idx++;
				npt--;
				for (int i=x;i<npt;i++) In[i]=In[i+1];
				x = mod(x-1,npt);
				break;
			}
		}
	}

	Out[idx] = In[0]; idx++;
	Out[idx] = In[1]; idx++;
	Out[idx] = In[2]; idx++;

	return idx;
}


// ===============================================================================================
//
inline D3DXVECTOR2 _DXV2(const IVECTOR2 &pt)
{
	return D3DXVECTOR2(float(pt.x), float(pt.y));
}

inline D3DXVECTOR2 _DXV2(const FVECTOR2 &pt)
{
	return D3DXVECTOR2(pt.x, pt.y);
}


// ===============================================================================================
//
template <typename Type>
void D3D9Pad::AppendLineVertexList(const Type *pt, int _npt, bool bLoop)
{
	if (_npt < 2) return;

	WORD npt = WORD(_npt);
	WORD wL = vI;
	WORD li = WORD(npt - 1);
	WORD aV, bV, cV, dV;

	float length = 0.0f;

	// ----------------------------------------------------------------------
	// Draw a thin hairline
	// ----------------------------------------------------------------------

	if (!bTriangles) {

		// Create line segments -------------------------------------------------
		//
		for (WORD i = 0; i<npt; i++) {

			Vtx[vI].x = float(pt[i].x);
			Vtx[vI].y = float(pt[i].y);
			Vtx[vI].l = length;
			Vtx[vI].fnc = SKPSW_THINPEN;
			Vtx[vI].clr = pencolor.dclr;

			if (IsDashed() && i!=li) {
				float x = float(pt[i].x - pt[i+1].x);
				float y = float(pt[i].y - pt[i+1].y);
				length += sqrt(x*x + y*y);
			}
			vI++;
		}
		aV = wL;
		for (WORD i = 0; i < (npt-1); i++) {
			Idx[iI++] = aV;
			aV++;
			Idx[iI++] = aV;
		}

		// Last segment ---------------------------------------------------------
		//
		if (bLoop) {
			Idx[iI++] = vI - 1;
			Idx[iI++] = wL;
		}

		return;
	}



	// ----------------------------------------------------------------------
	// Wide line mode
	// ----------------------------------------------------------------------

	D3DXVECTOR2 pp; // Prev point
	D3DXVECTOR2 np;	// Next point

	// Line Init ------------------------------------------------------------
	//
	if (bLoop) pp = _DXV2(pt[npt - 1]);
	else	   pp = _DXV2(pt[0]) * 2.0 - _DXV2(pt[1]);

	// Create line segments -------------------------------------------------
	//
	for (WORD i = 0; i<npt; i++) {

		if (i != li)	np = _DXV2(pt[i + 1]);
		else {
			if (bLoop)	np = _DXV2(pt[0]);
			else		np = _DXV2(pt[i]) * 2.0 - _DXV2(pt[i-1]);
		}

		WORD vII = vI + 1;

		// --------------------------------------
		Vtx[vI].x = Vtx[vII].x = float(pt[i].x);
		Vtx[vI].y = Vtx[vII].y = float(pt[i].y);
		Vtx[vI].nx = Vtx[vII].nx = np.x;
		Vtx[vI].ny = Vtx[vII].ny = np.y;
		Vtx[vI].px = Vtx[vII].px = pp.x;
		Vtx[vI].py = Vtx[vII].py = pp.y;
		Vtx[vI].l = Vtx[vII].l = length;
		Vtx[vI].clr = Vtx[vII].clr = pencolor.dclr;
		// --------------------------------------

		Vtx[vI].fnc = SKPSW_WIDEPEN_L;
		aV = vI; vI++;
		Vtx[vI].fnc = SKPSW_WIDEPEN_R;
		bV = vI; vI++;
		// --------------------------------------

		if (i > 0) {
			Idx[iI++] = cV;	Idx[iI++] = aV;
			Idx[iI++] = dV;	Idx[iI++] = dV;
			Idx[iI++] = aV;	Idx[iI++] = bV;
		}

		cV = aV;
		dV = bV;

		pp = _DXV2(pt[i]);

		if (IsDashed()) length += D3DXVec2Length(&(np-pp));
	}

	// Last segment ---------------------------------------------------------
	//
	if (bLoop) {
		Idx[iI++] = wL;		Idx[iI++] = aV;
		Idx[iI++] = wL + 1;	Idx[iI++] = wL + 1;
		Idx[iI++] = aV;		Idx[iI++] = bV;
	}
}


// ===============================================================================================
//
template <typename Type>
void D3D9Pad::AppendLineVertexList(const Type *pt)
{

	// ----------------------------------------------------------------------
	// Draw a thin hairline
	// ----------------------------------------------------------------------

	if (!bTriangles) {

		Vtx[vI].x = float(pt[0].x);
		Vtx[vI].y = float(pt[0].y);
		Vtx[vI].fnc = SKPSW_THINPEN;
		Vtx[vI].l = 0.0f;
		Vtx[vI].clr = pencolor.dclr;
		Idx[iI++] = vI;
		vI++;

		Vtx[vI].x = float(pt[1].x);
		Vtx[vI].y = float(pt[1].y);
		Vtx[vI].px = float(pt[0].x);
		Vtx[vI].py = float(pt[0].y);
		Vtx[vI].fnc = SKPSW_THINPEN | SKPSW_LENGTH;
		Vtx[vI].clr = pencolor.dclr;
		Idx[iI++] = vI;
		vI++;

		return;
	}


	// ----------------------------------------------------------------------
	// Wide line mode
	// ----------------------------------------------------------------------

	D3DXVECTOR2  pp = _DXV2(pt[0]) * 2.0 - _DXV2(pt[1]);
	D3DXVECTOR2  np;

	WORD vF = vI;

	for (int i = 0; i < 2; i++) {

		if (i == 0) np = _DXV2(pt[1]);
		else np = _DXV2(pt[1]) * 2.0 - _DXV2(pt[0]);

		WORD vII = vI + 1;

		// --------------------------------------
		Vtx[vI].x = Vtx[vII].x = float(pt[i].x);
		Vtx[vI].y = Vtx[vII].y = float(pt[i].y);
		Vtx[vI].nx = Vtx[vII].nx = np.x;
		Vtx[vI].ny = Vtx[vII].ny = np.y;
		Vtx[vI].px = Vtx[vII].px = pp.x;
		Vtx[vI].py = Vtx[vII].py = pp.y;
		Vtx[vI].l = Vtx[vII].l = 0.0f;
		Vtx[vI].clr = Vtx[vII].clr = pencolor.dclr;
		// --------------------------------------
		Vtx[vI].fnc = SKPSW_WIDEPEN_L;
		if (i) Vtx[vI].fnc |= SKPSW_LENGTH;
		vI++;
		Vtx[vI].fnc = SKPSW_WIDEPEN_R;
		if (i) Vtx[vI].fnc |= SKPSW_LENGTH;
		vI++;
		// --------------------------------------

		pp = _DXV2(pt[i]);
	}

	Idx[iI++] = vF + 0;
	Idx[iI++] = vF + 1;
	Idx[iI++] = vF + 2;
	Idx[iI++] = vF + 1;
	Idx[iI++] = vF + 3;
	Idx[iI++] = vF + 2;
}


// ===============================================================================================
//
D3DXHANDLE   D3D9Pad::eSketch = 0;
D3DXHANDLE   D3D9Pad::eDrawMesh = 0;
D3DXHANDLE   D3D9Pad::eVP = 0;
D3DXHANDLE   D3D9Pad::eW = 0;
D3DXHANDLE   D3D9Pad::eKey = 0;
D3DXHANDLE   D3D9Pad::ePen = 0;
D3DXHANDLE   D3D9Pad::eWVP = 0;
D3DXHANDLE   D3D9Pad::eFov = 0;
D3DXHANDLE   D3D9Pad::eTarget = 0;
D3DXHANDLE   D3D9Pad::eTexEn = 0;
D3DXHANDLE   D3D9Pad::eKeyEn = 0;
D3DXHANDLE   D3D9Pad::eWidth = 0;
D3DXHANDLE   D3D9Pad::eTex0 = 0;
D3DXHANDLE   D3D9Pad::eDashEn = 0;
D3DXHANDLE   D3D9Pad::eSize = 0;
D3DXHANDLE   D3D9Pad::eWide = 0;
D3DXHANDLE   D3D9Pad::eMtrl = 0;
D3DXHANDLE   D3D9Pad::eShade = 0;
D3DXHANDLE   D3D9Pad::ePos = 0;
D3DXHANDLE   D3D9Pad::ePos2 = 0;
D3DXHANDLE   D3D9Pad::eCov = 0;
D3DXHANDLE   D3D9Pad::eCovEn = 0;
D3DXHANDLE   D3D9Pad::eClearEn = 0;

ID3DXEffect* D3D9Pad::FX = 0;
D3D9Client * D3D9Pad::gc = 0;
WORD * D3D9Pad::Idx = 0;
SkpVtx * D3D9Pad::Vtx = 0;
LPD3DXVECTOR2 D3D9Pad::pSinCos = 0;
LPDIRECT3DDEVICE9 D3D9PadFont::pDev = 0;
LPDIRECT3DDEVICE9 D3D9PadPen::pDev = 0;
LPDIRECT3DDEVICE9 D3D9PadBrush::pDev = 0;
LPDIRECT3DDEVICE9 D3D9Pad::pDev = 0;



// ======================================================================
// class GDIFont
// ======================================================================

D3D9PadFont::D3D9PadFont(int height, bool prop, const char *face, Style style, int orientation, DWORD flags) : Font(height, prop, face, style, orientation)
{
	char *def_fixedface = "Courier New";
	char *def_sansface = "Arial";
	char *def_serifface = "Times New Roman";

	if (face[0]!='*') {
		if (!_stricmp (face, "fixed")) face = def_fixedface;
		else if (!_stricmp (face, "sans")) face = def_sansface;
		else if (!_stricmp (face, "serif")) face = def_serifface;
		else if (_stricmp (face, def_fixedface) && _stricmp (face, def_sansface) && _stricmp (face, def_serifface)) face = (prop ? def_sansface : def_fixedface);
	}
	else face++;

	pFont = NULL;
	hFont = NULL;

	if (orientation!=0) rotation = float(orientation) * 0.1f;
	else                rotation = 0.0f;

	// Browse cache ---------------------------------------------------
	//

	for (int i=0;i<nfcache;i++) {
		if (fcache[i].height!=height) continue;
		if (fcache[i].style!=style) continue;
		if (fcache[i].prop!=prop) continue;
		if (_stricmp(fcache[i].face,face)!=0) continue;
		pFont = fcache[i].pFont;
		break;
	}

	int weight = (style & BOLD) ? FW_BOLD : FW_NORMAL;
	DWORD italic = (style & ITALIC) ? TRUE : FALSE;
	DWORD underline = (style & UNDERLINE) ? TRUE : FALSE;

	Quality = NONANTIALIASED_QUALITY;

	if ((flags & 0xF) == 0) {
		if (Config->SketchpadFont == 1) Quality = DRAFT_QUALITY;
		if (Config->SketchpadFont == 2) Quality = CLEARTYPE_QUALITY;
	}
	else {
		if (flags&SKP_FONT_ANTIALIAS) Quality = DRAFT_QUALITY;
		if (flags&SKP_FONT_CLEARTYPE) Quality = CLEARTYPE_QUALITY;
	}

	// Create DirectX accelerated font for a use with D3D9Pad ------------------
	//
	if (pFont==NULL) {

		HFONT hNew = CreateFont(height, 0, 0, 0, weight, italic, underline, 0, 0, 0, 2, Quality, 49, face);

		pFont = new D3D9Text(pDev);
		pFont->Init(hNew);

		DeleteObject(hNew);

		pFont->SetRotation(rotation);

		if (nfcache>250) LogErr("Font Cache is Full.");
		else {
			// Fill the cache --------------------------------
			fcache[nfcache].pFont  = pFont;
			fcache[nfcache].height = height;
			fcache[nfcache].style  = style;
			fcache[nfcache].prop   = prop;
			strcpy_s(fcache[nfcache].face, 64, face);
			nfcache++;
		}
	}

	// Create Rotated windows GDI Font for a use with GDIPad ---------------------------
	//
	hFont = CreateFontA(height, 0, orientation, orientation, weight, italic, underline, 0, 0, 0, 2, Quality, 49, face);

	if (hFont==NULL) {
		face  = (prop ? def_sansface : def_fixedface);
		hFont = CreateFont(height, 0, orientation, orientation, weight, italic, underline, 0, 0, 0, 2, Quality, 49, face);
	}
}

// -----------------------------------------------------------------------------------------------
//
D3D9PadFont::~D3D9PadFont ()
{
	if (pFont) pFont->SetRotation(0.0f);
	if (hFont) DeleteObject(hFont);
}


// -----------------------------------------------------------------------------------------------
//
HFONT D3D9PadFont::GetGDIFont () const
{
	return hFont;
}


// -----------------------------------------------------------------------------------------------
//
void D3D9PadFont::D3D9TechInit(LPDIRECT3DDEVICE9 pDevice)
{
	pDev = pDevice;
}



// ======================================================================
// class GDIPen
// ======================================================================

D3D9PadPen::D3D9PadPen (int s, int w, DWORD col): oapi::Pen (style, width, col)
{
	switch (s) {
		case 0:  style = PS_NULL;  break;
		case 2:  style = PS_DOT;   break;
		default: style = PS_SOLID; break;
	}
	width = w;
	if (width<1) width = 1;
	hPen = CreatePen(style, width, COLORREF(col&0xFFFFFF));
	clr = SkpColor(col);
}

// -----------------------------------------------------------------------------------------------
//
D3D9PadPen::~D3D9PadPen ()
{
	DeleteObject(hPen);
}

// -----------------------------------------------------------------------------------------------
//
void D3D9PadPen::D3D9TechInit(LPDIRECT3DDEVICE9 pDevice)
{
	pDev = pDevice;
}



// ======================================================================
// class GDIBrush
// ======================================================================

D3D9PadBrush::D3D9PadBrush (DWORD col): oapi::Brush (col)
{
	hBrush = CreateSolidBrush(COLORREF(col&0xFFFFFF));
	clr = SkpColor(col);
}

// -----------------------------------------------------------------------------------------------
//
D3D9PadBrush::~D3D9PadBrush ()
{
	DeleteObject(hBrush);
}

// -----------------------------------------------------------------------------------------------
//
void D3D9PadBrush::D3D9TechInit(LPDIRECT3DDEVICE9 pDevice)
{
	pDev = pDevice;
}
