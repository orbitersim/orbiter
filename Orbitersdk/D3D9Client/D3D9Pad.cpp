// =================================================================================================================================
// The MIT Lisence:
//
// Copyright (C) 2012 - 2016 Jarmo Nikkanen
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


#include "D3D9Pad.h"
#include "D3D9Client.h"
#include "D3D9Surface.h"
#include "D3D9Util.h"
#include "D3D9TextMgr.h"
#include "D3D9Config.h"
#include "Log.h"

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
void D3D9Pad::D3D9TechInit(D3D9Client *_gc, LPDIRECT3DDEVICE9 pDevice, const char *folder)
{
	memset2(fcache, 0, 256*sizeof(FontCache));
	nfcache = 0;

	pDev = pDevice;
	gc = _gc;
	pCircleLow = NULL;
	pCircleHigh = NULL;

	// Initialize Ellipses -------------------------------------------------------------------------
	//
	HR(pDev->CreateVertexBuffer(17*sizeof(D3DXVECTOR3), 0, 0, D3DPOOL_DEFAULT, &pCircleLow, NULL));
	HR(pDev->CreateVertexBuffer(65*sizeof(D3DXVECTOR3), 0, 0, D3DPOOL_DEFAULT, &pCircleHigh, NULL));

	D3DXVECTOR3 *pVert;

	pSinCosLow  = new D3DXVECTOR2[17];
	pSinCosHigh = new D3DXVECTOR2[65];

	float angle=0.0f, step=float(PI2)/15.0f;
	
	if (pCircleLow->Lock(0,0,(void **)&pVert,0)==S_OK) {
		pVert[0] = D3DXVECTOR3(0.5f, 0.5f, 0);
		for (int i=1;i<17;i++) {
			pSinCosLow[i].x = pVert[i].x = sin(angle)*0.5f+0.5f;
			pSinCosLow[i].y = pVert[i].y = cos(angle)*0.5f+0.5f;
			pVert[i].z = angle;
			angle += step;
		}
		pCircleLow->Unlock();
	} else LogErr("Failed to Lock vertex buffer");
	
	angle=0.0; step=float(PI2)/63.0f;

	
	if (pCircleHigh->Lock(0,0,(void **)&pVert,0)==S_OK) {
		pVert[0] = D3DXVECTOR3(0.5f, 0.5f, 0.0f);
		for (int i=1;i<65;i++) {
			pSinCosHigh[i].x = pVert[i].x = sin(angle)*0.5f+0.5f;
			pSinCosHigh[i].y = pVert[i].y = cos(angle)*0.5f+0.5f;
			pVert[i].z = angle;
			angle += step;
		}
		pCircleHigh->Unlock();
	} else LogErr("Failed to Lock vertex buffer");
	

	// Initialize Techniques -------------------------------------------------------------------------
	//
	char name[256];
	sprintf_s(name,256,"Modules/%s/CKBlit.fx",folder);

	// Create the Effect from a .fx file.
	ID3DXBuffer* errors = 0;
	
	HR(D3DXCreateEffectFromFileA(pDev, name, 0, 0, 0, 0, &FX, &errors));
	
	if (errors) {
		LogErr("Effect Error: %s",(char*)errors->GetBufferPointer());
		MessageBoxA(0, (char*)errors->GetBufferPointer(), "CKBlit.fx Error", 0);
		FatalAppExitA(0,"Critical error has occured. See Orbiter.log for details");
	}
	
	if (FX==0) {
		LogErr("Failed to create an Effect (%s)",name);
		MissingRuntimeError();
		return;
	}

	eEllipse  = FX->GetTechniqueByName("EllipseTech");
	eLine     = FX->GetTechniqueByName("LineTech");
	eVP       = FX->GetParameterByName(0,"gVP");
	eTex0     = FX->GetParameterByName(0,"gTex0");
	eColor    = FX->GetParameterByName(0,"gColor");
	eData     = FX->GetParameterByName(0,"gData");
	eDash     = FX->GetParameterByName(0,"gDash");
}


// ===============================================================================================
//
void D3D9Pad::GlobalExit()
{
	LogAlw("Clearing Font Cache... %d Fonts are stored in the cache",nfcache);
	for (int i=0;i<nfcache;i++) if (fcache[i].pFont) delete fcache[i].pFont;
	
	SAFE_RELEASE(pCircleLow);
	SAFE_RELEASE(pCircleHigh);
	SAFE_RELEASE(FX);
	SAFE_DELETEA(pSinCosLow);
	SAFE_DELETEA(pSinCosHigh);

	memset2(fcache, 0, 256*sizeof(FontCache));
}



// ======================================================================
// class GDIPad
// ======================================================================

D3D9Pad::D3D9Pad(SURFHANDLE s) : Sketchpad(s)
{
	_TRACE;

	cfont  = deffont;
	cpen   = NULL;
	cbrush = NULL;
	pTgt   = SURFACE(s);
	origx  = 0;
	origy  = 0;
	cx     = 0;
	cy     = 0;
	bkmode = TRANSPARENT;
	halign = TA_LEFT;
	valign = TA_TOP;

	SURFACE(GetSurface())->SketchPad = SKETCHPAD_DIRECTX;

	brushcolor = D3DXCOLOR(0,1,0,1);
	bkcolor    = D3DXCOLOR(0,0,0,1);
	textcolor  = D3DXCOLOR(0,1,0,1);
	pencolor   = D3DXCOLOR(0,1,0,1);
	linescale  = 1;
	
	pVP = pTgt->pVP;

	HR(FX->SetMatrix(eVP, pVP));

	if (pTgt->IsBackBuffer()==false) {
		if (pTgt->BindGPU()==false) { 
			pTgt=NULL;  
			LogErr("D3D9Pad creation failed");
			return;
		}
	}
	
	//linescale = pTgt->GetWidth()>>8;
	//if (linescale<1) linescale = 1;
	//if (pTgt->GetFlags()&SRFFLAG_VC_MFD) linescale = pTgt->GetWidth()>>8;
}


// ===============================================================================================
//
D3D9Pad::D3D9Pad(LPDIRECT3DSURFACE9 s) : Sketchpad(NULL)
{
	_TRACE;

	cfont = deffont;
	cpen = NULL;
	cbrush = NULL;
	pTgt = NULL;
	origx = 0;
	origy = 0;
	cx = 0;
	cy = 0;
	bkmode = TRANSPARENT;
	halign = TA_LEFT;
	valign = TA_TOP;

	brushcolor = D3DXCOLOR(0, 1, 0, 1);
	bkcolor = D3DXCOLOR(0, 0, 0, 1);
	textcolor = D3DXCOLOR(0, 1, 0, 1);
	pencolor = D3DXCOLOR(0, 1, 0, 1);
	linescale = 1;

	D3DSURFACE_DESC desc;
	pVP = &mVP;
	s->GetDesc(&desc);

	D3DXMatrixOrthoOffCenterLH(pVP, 0.0f, (float)desc.Width, (float)desc.Height, 0.0f, 0.0f, 1.0f);

	HR(FX->SetMatrix(eVP, pVP));	
}

// ===============================================================================================
//
D3D9Pad::~D3D9Pad ()
{
	_TRACE;
	if (GetSurface()) {
		if (pTgt) if (pTgt->IsBackBuffer() == false) pTgt->ReleaseGPU();
		SURFACE(GetSurface())->SketchPad = SKETCHPAD_NONE;
	}
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
	Font *pfont = cfont;
	if (font) cfont = font;
	else      cfont = deffont;
	return pfont;
}


// ===============================================================================================
//
Brush *D3D9Pad::SetBrush (Brush *brush) const
{
	Brush *pbrush = cbrush;
	cbrush = brush;
	if (cbrush) brushcolor = ((D3D9PadBrush *)cbrush)->fcolor;
	else	    brushcolor = D3DXCOLOR(DWORD(0));
	return pbrush;
}


// ===============================================================================================
//
Pen *D3D9Pad::SetPen (Pen *pen) const
{
	Pen *ppen = cpen;
	if (pen) cpen = pen;
	else     cpen = NULL;
	if (cpen) pencolor = ((D3D9PadPen *)cpen)->fcolor;	
	return ppen;
}


// ===============================================================================================
//
void D3D9Pad::SetTextAlign (TAlign_horizontal tah, TAlign_vertical tav)
{
	halign = 0; valign = 0;

	switch (tah) {
		case LEFT:     halign |= TA_LEFT;     break;
		case CENTER:   halign |= TA_CENTER;   break;
		case RIGHT:    halign |= TA_RIGHT;    break;
	}
	switch (tav) {
		case TOP:      valign |= TA_TOP;      break;
		case BASELINE: valign |= TA_BASELINE; break;
		case BOTTOM:   valign |= TA_BOTTOM;   break;
	}
}


// ===============================================================================================
//
DWORD D3D9Pad::SetTextColor(DWORD col)
{
	if ((col&0xFF000000)==0) col|=0xFF000000;
	D3DXCOLOR prev = textcolor;
	textcolor = D3DXCOLOR(col);
	D3DXCOLORSWAP(&textcolor);
	D3DXCOLORSWAP(&prev);
	return prev;
}


// ===============================================================================================
//
DWORD D3D9Pad::SetBackgroundColor(DWORD col)
{
	if ((col&0xFF000000)==0) col|=0xFF000000;
	D3DXCOLOR prev = bkcolor;
	bkcolor = D3DXCOLOR(col);
	D3DXCOLORSWAP(&bkcolor);
	D3DXCOLORSWAP(&prev);
	return prev;
}


// ===============================================================================================
//
void D3D9Pad::SetBackgroundMode(BkgMode mode)
{
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
	((D3D9PadFont *)cfont)->pFont->GetD3D9TextMetrics(&tm);
	return MAKELONG(tm.tmHeight-tm.tmInternalLeading, tm.tmAveCharWidth);
}


// ===============================================================================================
//
DWORD D3D9Pad::GetTextWidth (const char *str, int len)
{
	if (cfont==NULL) return 0;
	return DWORD(((D3D9PadFont *)cfont)->pFont->Length2(str, len));
}


// ===============================================================================================
//
void D3D9Pad::SetOrigin (int x, int y)
{
	origx = x;
	origy = y;
}


// ===============================================================================================
//
void D3D9Pad::GetOrigin (int *x, int *y) const
{
	if (x) *x = origx;	
	if (y) *y = origy;
}

// ===============================================================================================
//
bool D3D9Pad::HasPen()
{
	if (cpen==NULL) return false;
	if (((D3D9PadPen*)cpen)->style==PS_NULL) return false;
	return true;
}


// ===============================================================================================
//
bool D3D9Pad::IsDashed()
{
	if (cpen==NULL) return false;
	if (((D3D9PadPen*)cpen)->style==PS_DOT) return true;
	return false;
}


// ===============================================================================================
//
bool D3D9Pad::HasWidePen()
{
	if (cpen==NULL) return false;
	if (linescale>1) return true;
	if (((D3D9PadPen*)cpen)->width>1) return true;
	return false;
}


// ===============================================================================================
//
bool D3D9Pad::HasThinPen()
{
	if (linescale>1) return false;
	if (cpen==NULL) return false;
	if (((D3D9PadPen*)cpen)->width<=1) return true;
	return false;	
}


// ===============================================================================================
//
bool D3D9Pad::HasBrush()
{
	return (cbrush != NULL);
}


// ===============================================================================================
//
float D3D9Pad::GetPenWidth()
{
	if (cpen==NULL) return 1.0f;
	return float(((D3D9PadPen*)cpen)->width*linescale);	
}


// ===============================================================================================
//
bool D3D9Pad::Text (int x, int y, const char *str, int len)
{
	if (cfont==NULL) return false;

	D3D9Text *pText = ((D3D9PadFont *)cfont)->pFont;

	switch(halign) {
		default:
		case TA_LEFT:   pText->SetTextHAlign(0); break;
		case TA_CENTER: pText->SetTextHAlign(1); break;
		case TA_RIGHT:  pText->SetTextHAlign(2); break;
	}

	switch(valign) {
		default:
		case TA_TOP:	  pText->SetTextVAlign(0); break;
		case TA_BASELINE: pText->SetTextVAlign(1); break;
		case TA_BOTTOM:   pText->SetTextVAlign(2); break;
	}

	pText->SetRotation(((D3D9PadFont *)cfont)->rotation);

	if (bkmode==OPAQUE) pText->Print(&textcolor, origx+x-1, origy+y-1, str, len, pVP, &bkcolor);
	else				pText->Print(&textcolor, origx+x-1, origy+y-1, str, len, pVP);

	return true;
}


// ===============================================================================================
//
void D3D9Pad::Pixel (int x, int y, DWORD col)
{
	RECT rect = { origx+x, origy+y, origx+x+1, origy+y+1 };
	pDev->ColorFill(pTgt->pSurf, &rect, col); 
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
	UINT numPasses=0;
	HR(pDev->SetVertexDeclaration(pPositionDecl));

	if (IsDashed()) FX->SetBool(eDash, true);
	else			FX->SetBool(eDash, false);

	HR(FX->SetTechnique(eLine));
	HR(FX->SetVector(eColor, (LPD3DXVECTOR4)&pencolor));
	HR(FX->Begin(&numPasses, D3DXFX_DONOTSAVESTATE));
	HR(FX->BeginPass(0));

	if (HasWidePen()) {

		IVECTOR2 pts[2];
		pts[0].x = x0;	pts[0].y = y0;
		pts[1].x = x1;	pts[1].y = y1;
		
		D3DXVECTOR3 WLVtx[4];
		CreateLineVertexList(WLVtx, pts, 2, false);

		pDev->SetRenderState(D3DRS_CULLMODE, D3DCULL_NONE);
		pDev->DrawPrimitiveUP(D3DPT_TRIANGLESTRIP, 2, WLVtx, sizeof(D3DXVECTOR3));
		pDev->SetRenderState(D3DRS_CULLMODE, D3DCULL_CCW);
	}

	if (HasThinPen()) {

		D3DXVECTOR3 pts[2];
		pts[0] = D3DXVECTOR3(float(x0+origx), float(y0+origy), 0.0f);
		pts[1] = D3DXVECTOR3(float(x1+origx), float(y1+origy), 0.0f);

		if (IsDashed()) pts[1].z = D3DXVec3Length(&(pts[0]-pts[1]));
			
		HR(pDev->DrawPrimitiveUP(D3DPT_LINELIST, 1, pts, sizeof(D3DXVECTOR3)));
	}
	
	HR(FX->EndPass());
	HR(FX->End());	

	cx = x1; cy = y1;
}


// ===============================================================================================
//
void D3D9Pad::Rectangle (int l, int t, int r, int b)
{
	static WORD indices[6] = {0,1,2,0,2,3};
	static D3DVECTOR verts[5] = { { 0.0f, 0.0f, 0.0f}, { 1.0f, 0.0f, 0.0f}, { 1.0f, 1.0f, 0.0f}, { 0.0f, 1.0f, 0.0f}, { 0.0f, 0.0f, 0.0f} };
	
	UINT numPasses=0;
	pDev->SetVertexDeclaration(pPositionDecl);

	FX->SetTechnique(eEllipse);
	FX->SetVector(eData, &D3DXVECTOR4(float(origx+l), float(origy+t), float(r-l-1), float(b-t-1)));	
	FX->SetBool(eDash, false);
	FX->Begin(&numPasses, D3DXFX_DONOTSAVESTATE);
	FX->BeginPass(0);

	// Fill interion ----------------------------------------------
	//
	if (HasBrush()) {
		FX->SetVector(eColor, (LPD3DXVECTOR4)&brushcolor);
		FX->CommitChanges();
		pDev->DrawIndexedPrimitiveUP(D3DPT_TRIANGLELIST, 0, 4, 2, &indices, D3DFMT_INDEX16, &verts, sizeof(D3DXVECTOR3));
	}

	if (IsDashed()) FX->SetBool(eDash, true);
	
	// Draw thin outline ------------------------------------------
	//
	if (HasThinPen()) {
		FX->SetVector(eColor, (LPD3DXVECTOR4)&pencolor);
		FX->CommitChanges();
		pDev->DrawPrimitiveUP(D3DPT_LINESTRIP, 4, &verts, sizeof(D3DXVECTOR3));
	}

	FX->EndPass();
	FX->End();	

	// Draw wide outline ------------------------------------------
	//
	if (HasWidePen()) {

		IVECTOR2 pts[4];
		pts[0].x = l;	pts[0].y = t;
		pts[1].x = r;	pts[1].y = t;
		pts[2].x = r;	pts[2].y = b;
		pts[3].x = l;	pts[3].y = b;
		
		D3DXVECTOR3 WLVtx[2*4+2];
		CreateLineVertexList(WLVtx, pts, 4, true);

		FX->SetTechnique(eLine);
		FX->SetVector(eColor, (LPD3DXVECTOR4)&pencolor);
		FX->Begin(&numPasses, D3DXFX_DONOTSAVESTATE);
		FX->BeginPass(0);

		pDev->SetRenderState(D3DRS_CULLMODE, D3DCULL_NONE);
		pDev->DrawPrimitiveUP(D3DPT_TRIANGLESTRIP, 8, WLVtx, sizeof(D3DXVECTOR3));
		pDev->SetRenderState(D3DRS_CULLMODE, D3DCULL_CCW);

		FX->EndPass();
		FX->End();	
	}
}


// ===============================================================================================
//
void D3D9Pad::Ellipse (int l, int t, int r, int b)
{
	UINT numPasses=0;
	UINT nPrim = 15;
	bool bLow = true;
	
	if ((r-l)>32 || (b-t)>32) {
		bLow=false;
		nPrim = 63;
	}
	
	pDev->SetVertexDeclaration(pPositionDecl);
	
	if (bLow) pDev->SetStreamSource(0, pCircleLow, 0, sizeof(D3DXVECTOR3));
	else 	  pDev->SetStreamSource(0, pCircleHigh, 0, sizeof(D3DXVECTOR3));
		
	FX->SetTechnique(eEllipse);
	FX->SetBool(eDash, false);
	FX->SetVector(eData, &D3DXVECTOR4(float(origx+l), float(origy+t), float(r-l-1), float(b-t-1)));
	FX->Begin(&numPasses, D3DXFX_DONOTSAVESTATE);
	FX->BeginPass(0);

	// Fill interion ----------------------------------------------
	//
	if (HasBrush()) {
		FX->SetVector(eColor, (LPD3DXVECTOR4)&brushcolor);
		FX->CommitChanges();
		pDev->DrawPrimitive(D3DPT_TRIANGLEFAN, 0, nPrim);
	}

	if (IsDashed()) FX->SetBool(eDash, true);

	// Draw thin outline ------------------------------------------
	//
	if (HasThinPen()) {
		FX->SetVector(eColor, (LPD3DXVECTOR4)&pencolor);
		FX->CommitChanges();
		pDev->DrawPrimitive(D3DPT_LINESTRIP, 1, nPrim);
	}

	FX->EndPass();
	FX->End();		

	// Draw wide outline ------------------------------------------
	//
	if (HasWidePen()) {

		float w = float(r-l); float h = float(b-t);	float fl = float(l); float ft = float(t);

		IVECTOR2 pts[65];

		if (bLow) {
			for (UINT i=0;i<nPrim;i++) {
				pts[i].x = long(fl + pSinCosLow[i+1].x * w);
				pts[i].y = long(ft + pSinCosLow[i+1].y * h);
			}
		}
		else {
			for (UINT i=0;i<nPrim+1;i++) {
				pts[i].x = long(fl + pSinCosHigh[i+1].x * w);
				pts[i].y = long(ft + pSinCosHigh[i+1].y * h);
			}
		}
			
		D3DXVECTOR3 WLVtx[2*64+2];
		CreateLineVertexList(WLVtx, pts, nPrim, true);

		FX->SetTechnique(eLine);
		FX->SetVector(eColor, (LPD3DXVECTOR4)&pencolor);
		FX->Begin(&numPasses, D3DXFX_DONOTSAVESTATE);
		FX->BeginPass(0);
		pDev->SetRenderState(D3DRS_CULLMODE, D3DCULL_NONE);
		pDev->DrawPrimitiveUP(D3DPT_TRIANGLESTRIP, nPrim*2, WLVtx, sizeof(D3DXVECTOR3));
		pDev->SetRenderState(D3DRS_CULLMODE, D3DCULL_CCW);
		FX->EndPass();
		FX->End();	
	}
}


// ===============================================================================================
//
void D3D9Pad::Polygon (const IVECTOR2 *pt, int npt)
{
	if (npt<3) return;

	UINT numPasses=0;
	int nIdx = 0;

	WORD *Idx = NULL;

	// Allovate and prepare vertex buffers -----------------------------------------
	//
	D3DXVECTOR3 *Vtx = new D3DXVECTOR3[npt+1];
	for (int i=0;i<npt;i++) Vtx[i].x = float(pt[i].x+origx), Vtx[i].y = float(pt[i].y+origy);
	Vtx[npt].x = float(pt[0].x+origx), Vtx[npt].y = float(pt[0].y+origy);

	// Create filled polygon interior ---------------------------------------------
	//
	if (HasBrush()) {
		Idx = new WORD[(npt-2)*3];
		nIdx = CreatePolyIndexList(Vtx, npt, Idx);
	}

	// Configure render pileline --------------------------------------------------
	//
	pDev->SetVertexDeclaration(pPositionDecl);

	FX->SetTechnique(eLine);
	FX->SetBool(eDash, false);
	FX->Begin(&numPasses, D3DXFX_DONOTSAVESTATE);
	FX->BeginPass(0);

	// Draw filled interior -------------------------------------------------------
	//
	if (nIdx) {
		FX->SetVector(eColor, (LPD3DXVECTOR4)&brushcolor);
		FX->CommitChanges();
		pDev->SetRenderState(D3DRS_CULLMODE, D3DCULL_NONE);
		pDev->DrawIndexedPrimitiveUP(D3DPT_TRIANGLELIST, 0, npt, nIdx/3, Idx, D3DFMT_INDEX16, Vtx, sizeof(D3DXVECTOR3));
		pDev->SetRenderState(D3DRS_CULLMODE, D3DCULL_CCW);
	}

	// Re-configure for outlining --------------------------------------------------
	//
	if (IsDashed()) FX->SetBool(eDash, true);
	FX->SetVector(eColor, (LPD3DXVECTOR4)&pencolor);
	FX->CommitChanges();

	// Draw a wide outline ---------------------------------------------------------
	//
	if (HasWidePen()) {
		D3DXVECTOR3 *WLVtx = new D3DXVECTOR3[npt*2+2];
		CreateLineVertexList(WLVtx, pt, npt, true);
		pDev->SetRenderState(D3DRS_CULLMODE, D3DCULL_NONE);
		pDev->DrawPrimitiveUP(D3DPT_TRIANGLESTRIP, npt*2, WLVtx, sizeof(D3DXVECTOR3));
		pDev->SetRenderState(D3DRS_CULLMODE, D3DCULL_CCW);
		delete []WLVtx;
	}

	// Draw thin outline -------------------------------------------------------------
	//
	if (HasThinPen()) {
		if (IsDashed()) {
			Vtx[0].z = 0;
			for (int i=1;i<=npt;i++) {
				D3DXVECTOR3 d = Vtx[i]-Vtx[i-1]; d.z = 0.0f;
				Vtx[i].z = Vtx[i-1].z + D3DXVec3Length(&d);
			}
		}
		pDev->DrawPrimitiveUP(D3DPT_LINESTRIP, npt, Vtx, sizeof(D3DXVECTOR3));
	}
	
	// Cleanup ----------------------------------------------------------------------
	//
	FX->EndPass();
	FX->End();	

	SAFE_DELETEA(Idx);
	SAFE_DELETEA(Vtx);
}


// ===============================================================================================
//
void D3D9Pad::Polyline (const IVECTOR2 *pt, int npt)
{

	UINT numPasses=0;

	// Prepare render pileline -------------------------------------------------------
	//
	pDev->SetVertexDeclaration(pPositionDecl);

	FX->SetTechnique(eLine);

	if (IsDashed()) FX->SetBool(eDash, true);
	else			FX->SetBool(eDash, false);

	FX->SetVector(eColor, (LPD3DXVECTOR4)&pencolor);
	FX->Begin(&numPasses, D3DXFX_DONOTSAVESTATE);
	FX->BeginPass(0);

	// Draw a wide line ---------------------------------------------------------------
	//
	if (HasWidePen()) {
		D3DXVECTOR3 *Vtx = new D3DXVECTOR3[npt*2];
		CreateLineVertexList(Vtx, pt, npt, false);
		pDev->SetRenderState(D3DRS_CULLMODE, D3DCULL_NONE);
		pDev->DrawPrimitiveUP(D3DPT_TRIANGLESTRIP, npt*2-2, Vtx, sizeof(D3DXVECTOR3));
		pDev->SetRenderState(D3DRS_CULLMODE, D3DCULL_CCW);
		SAFE_DELETEA(Vtx);
	}

	// Draw hairline ------------------------------------------------------------------
	//
	if (HasThinPen()) {
		D3DXVECTOR3 *Vtx = new D3DXVECTOR3[npt];
		for (int i=0;i<npt;i++) Vtx[i].x = float(pt[i].x+origx), Vtx[i].y = float(pt[i].y+origy);
		if (IsDashed()) {
			Vtx[0].z = 0;
			for (int i=1;i<npt;i++) {
				D3DXVECTOR3 d = Vtx[i]-Vtx[i-1]; d.z = 0.0f;
				Vtx[i].z = Vtx[i-1].z + D3DXVec3Length(&d);
			}
		}
		pDev->DrawPrimitiveUP(D3DPT_LINESTRIP, npt-1, Vtx, sizeof(D3DXVECTOR3));
		SAFE_DELETEA(Vtx);
	}

	FX->EndPass();
	FX->End();	
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
int D3D9Pad::CheckTriangle(short x, const D3DXVECTOR3 *pt, const WORD *Idx, float hd, short npt, bool bSharp)
{
	WORD A = Idx[x];
	WORD B = Idx[mod(x-1,npt)];
	WORD C = Idx[mod(x+1,npt)];

	float bx = pt[B].x - pt[A].x;
	float by = pt[B].y - pt[A].y;
	float ax = pt[C].x - pt[A].x;
	float ay = pt[C].y - pt[A].y;

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

		float cx = pt[P].x - pt[A].x;
		float cy = pt[P].y - pt[A].y;
		float ac = ax*cx + ay*cy;	 if (ac<0) continue;	
		float bc = bx*cx + by*cy;	 if (bc<0) continue;
		float u  = (bb*ac - ab*bc) * id;
		float v  = (aa*bc - ab*ac) * id;

		// Check if the point is inside the triangle
		// NOTE: Having u+v slightly above 1.0 is a bad condition, should find a better ear.
		if  ((u>0.0f) && (v>0.0f) && ((u+v)<1.0f)) return 0;
	}

	return 1; // It's an ear
}


// ===============================================================================================
//
int D3D9Pad::CreatePolyIndexList(const D3DXVECTOR3 *pt, short npt, WORD *Out)
{
	if (npt<3) return 0;
	if (npt==3) { Out[0]=0; Out[1]=1; Out[2]=2;	return 3; }

	short idx = 0;		// Number of indices written in the output
	short x = npt-1;	// First ear to test is the last one in the list
	bool bSharp = true; // Avoid sharp ears
	
	// Build initial index list
	WORD *In = new WORD[npt];
	for (int i=0;i<npt;i++) In[i]=i;
	float sum = 0;
	int k = npt-1;
	for (int i=0;i<k;i++) sum += (pt[i].x*pt[(i+1)%k].y - pt[(i+1)%k].x*pt[i].y);

	if (sum>0) sum=1.0; else sum=-1.0;

	while (npt>3) {

		switch (CheckTriangle(x, pt, In, sum, npt, bSharp)) {

			case 0: 
			{
				x--; 
				if (x<0) { // Restart
					if (!bSharp) { delete []In;	return idx;	}
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
	delete []In;
	return idx;
}


// ===============================================================================================
//
inline D3DXVECTOR2 D3D9Pad::_DXV2(const IVECTOR2 *pt)
{
	return D3DXVECTOR2(float(pt->x+origx), float(pt->y+origy));
}


// ===============================================================================================
//
void D3D9Pad::CreateLineVertexList(D3DXVECTOR3 *Vtx, const IVECTOR2 *pt, int npt, bool bLoop)
{
	DWORD n = 0;
	D3DXVECTOR2 jv, dp, d;	

	// Line Init ------------------------------------------------------------
	//
	if (bLoop) dp = _DXV2(&pt[0]) - _DXV2(&pt[npt-1]);
	else	   dp = _DXV2(&pt[1]) - _DXV2(&pt[0]);

	D3DXVec2Normalize(&dp, &dp);

	int li = npt-1;
	float length = 0.0f;
	float width  = GetPenWidth();

	// Create line segments -------------------------------------------------
	//
	for (int i=0;i<npt;i++) {
	
		if (i!=li)		d = _DXV2(&pt[i+1]) - _DXV2(&pt[i]);
		else {
			if (bLoop)	d = _DXV2(&pt[0])  - _DXV2(&pt[li]);
			else		d = dp;
		}

		float ld = D3DXVec2Length(&d);

		d /= ld;
	
		float cd = (d.x*dp.x) + (d.y*dp.y);

		if (fabs(cd-1.0f)>4e-3) {
			D3DXVec2Normalize(&jv, &(D3DXVECTOR2(d.y, -d.x) + D3DXVECTOR2(dp.y, -dp.x)));
			jv *= ((0.5f * width) / sqrt(0.5f * (1.0f+cd)));
		}
		else {
			jv = D3DXVECTOR2(d.y, -d.x) * (0.5f * width);
		}

		Vtx[n++] = D3DXVECTOR3(float(pt[i].x+origx) + jv.x, float(pt[i].y+origy) + jv.y, length);
		Vtx[n++] = D3DXVECTOR3(float(pt[i].x+origx) - jv.x, float(pt[i].y+origy) - jv.y, length);

		length += ld;
		dp = d;
	}

	// Last segment ---------------------------------------------------------
	//
	if (bLoop) {
		Vtx[n++] = D3DXVECTOR3(Vtx[0].x, Vtx[0].y, length);
		Vtx[n++] = D3DXVECTOR3(Vtx[1].x, Vtx[1].y, length);
	}
}


// ===============================================================================================
//
ID3DXEffect* D3D9Pad::FX = 0;		
D3DXHANDLE   D3D9Pad::eLine = 0;
D3DXHANDLE   D3D9Pad::eEllipse = 0;
D3DXHANDLE   D3D9Pad::eVP = 0;			
D3DXHANDLE   D3D9Pad::eColor = 0;	
D3DXHANDLE   D3D9Pad::eTex0 = 0;	
D3DXHANDLE   D3D9Pad::eData = 0;
D3DXHANDLE   D3D9Pad::eDash = 0;
D3D9Client * D3D9Pad::gc = 0;

LPDIRECT3DVERTEXBUFFER9 D3D9Pad::pCircleLow = 0;
LPDIRECT3DVERTEXBUFFER9 D3D9Pad::pCircleHigh = 0;
LPD3DXVECTOR2 D3D9Pad::pSinCosLow = 0;
LPD3DXVECTOR2 D3D9Pad::pSinCosHigh = 0;
LPDIRECT3DDEVICE9 D3D9PadFont::pDev = 0;
LPDIRECT3DDEVICE9 D3D9PadPen::pDev = 0;
LPDIRECT3DDEVICE9 D3D9PadBrush::pDev = 0;
LPDIRECT3DDEVICE9 D3D9Pad::pDev = 0;





// ======================================================================
// class GDIFont
// ======================================================================

D3D9PadFont::D3D9PadFont(int height, bool prop, const char *face, Style style, int orientation) : Font(height, prop, face, style, orientation)
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

	_height = height;
	_prop = prop;
	_style = style;
	strcpy_s(_face, 64, face);

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
	
	int weight = (style & BOLD ? FW_BOLD : FW_NORMAL);
	DWORD italic = (style & ITALIC ? TRUE : FALSE);
	DWORD underline = (style & UNDERLINE ? TRUE : FALSE);

	DWORD AAQuality = NONANTIALIASED_QUALITY;

	if (Config->SketchpadFont==1) AAQuality = DRAFT_QUALITY;
	if (Config->SketchpadFont==2) AAQuality = CLEARTYPE_QUALITY;
	if (Config->SketchpadFont==3) AAQuality = PROOF_QUALITY;

	// Create DirectX accelerated font for a use with D3D9Pad ------------------
	//
	if (pFont==NULL && pDev) {
	
		HFONT hNew = CreateFont(height, 0, 0, 0, weight, italic, underline, 0, 0, 0, 2, AAQuality, 49, face);

		pFont = new D3D9Text(pDev);
		pFont->Init(hNew, 255);

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
	hFont = CreateFontA(height, 0, orientation, orientation, weight, italic, underline, 0, 0, 0, 2, AAQuality, 49, face);

	if (hFont==NULL) {
		face  = (prop ? def_sansface : def_fixedface);
		hFont = CreateFont(height, 0, orientation, orientation, weight, italic, underline, 0, 0, 0, 2, AAQuality, 49, face);
	}
}

// -----------------------------------------------------------------------------------------------
//
void D3D9PadFont::InitD3DFont()
{
	if (pFont) return;

	for (int i = 0; i<nfcache; i++) {
		if (fcache[i].height != _height) continue;
		if (fcache[i].style != _style) continue;
		if (fcache[i].prop != _prop) continue;
		if (_stricmp(fcache[i].face, _face) != 0) continue;
		pFont = fcache[i].pFont;
		break;
	}

	int weight = (_style & BOLD ? FW_BOLD : FW_NORMAL);
	DWORD italic = (_style & ITALIC ? TRUE : FALSE);
	DWORD underline = (_style & UNDERLINE ? TRUE : FALSE);

	DWORD AAQuality = NONANTIALIASED_QUALITY;

	if (Config->SketchpadFont == 1) AAQuality = DRAFT_QUALITY;
	if (Config->SketchpadFont == 2) AAQuality = CLEARTYPE_QUALITY;
	if (Config->SketchpadFont == 3) AAQuality = PROOF_QUALITY;

	// Create DirectX accelerated font for a use with D3D9Pad ------------------
	//
	if (pFont == NULL) {

		HFONT hNew = CreateFont(_height, 0, 0, 0, weight, italic, underline, 0, 0, 0, 2, AAQuality, 49, _face);

		pFont = new D3D9Text(pDev);
		pFont->Init(hNew, 255);
		DeleteObject(hNew);

		pFont->SetRotation(rotation);

		if (nfcache>250) LogErr("Font Cache is Full.");
		else {
			// Fill the cache --------------------------------
			fcache[nfcache].pFont = pFont;
			fcache[nfcache].height = _height;
			fcache[nfcache].style = _style;
			fcache[nfcache].prop = _prop;
			strcpy_s(fcache[nfcache].face, 64, _face);
			nfcache++;
		}
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
	if ((col&0xFF000000)==0) col|=0xFF000000;
	fcolor = D3DXCOLOR(col);
	D3DXCOLORSWAP(&fcolor);
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
	if ((col&0xFF000000)==0) col|=0xFF000000;
	fcolor = D3DXCOLOR(col);
	D3DXCOLORSWAP(&fcolor);	
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
