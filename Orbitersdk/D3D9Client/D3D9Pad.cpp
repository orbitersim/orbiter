// =================================================================================================================================
// The MIT Lisence:
//
// Copyright (C) 2012 - 2014 Jarmo Nikkanen
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
	char face[32];
	Font::Style style;
	class D3D9Text *pFont;
	HFONT hFont;
} fcache[128];


int nfcache = 0;
int pens_allocated = 0;
int brushes_allocated = 0;
int fonts_allocated = 0;

oapi::Font * deffont = 0;
oapi::Pen * defpen = 0;


// -----------------------------------------------------------------------------------------------
//
void D3D9Pad::D3D9TechInit(D3D9Client *_gc, LPDIRECT3DDEVICE9 pDevice, const char *folder)
{
	memset2(fcache, 0, 128*sizeof(FontCache));

	pDev = pDevice;
	gc = _gc;
	pCircleLow = NULL;
	pCircleHigh = NULL;

	// Initialize Ellipses -------------------------------------------------------------------------
	//
	HR(pDev->CreateVertexBuffer(17*sizeof(D3DXVECTOR3), 0, 0, D3DPOOL_DEFAULT, &pCircleLow, NULL));
	HR(pDev->CreateVertexBuffer(65*sizeof(D3DXVECTOR3), 0, 0, D3DPOOL_DEFAULT, &pCircleHigh, NULL));

	D3DXVECTOR3 *pVert;

	float angle=0.0f, step=float(PI2)/15.0f;
	
	if (pCircleLow->Lock(0,0,(void **)&pVert,0)==S_OK) {
		pVert[0] = D3DXVECTOR3(0.5f, 0.5f, 0);
		for (int i=1;i<17;i++) {
			pVert[i].x = sin(angle)*0.5f+0.5f;
			pVert[i].y = cos(angle)*0.5f+0.5f;
			pVert[i].z = angle;
			angle += step;
		}
		pCircleLow->Unlock();
	} else LogErr("Failed to Lock vertex buffer");
	
	angle=0.0; step=float(PI2)/63.0f;

	
	if (pCircleHigh->Lock(0,0,(void **)&pVert,0)==S_OK) {
		pVert[0] = D3DXVECTOR3(0.5f, 0.5f, 0.0f);
		for (int i=1;i<65;i++) {
			pVert[i].x = sin(angle)*0.5f+0.5f;
			pVert[i].y = cos(angle)*0.5f+0.5f;
			pVert[i].z = angle;
			angle += step;
			
		}
		pCircleHigh->Unlock();
	} else LogErr("Failed to Lock vertex buffer");
	

	// Initialize Techniques -------------------------------------------------------------------------
	//
	LogMsg("Starting to initialize a surface rendering technique");
	
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

	eWideRect = FX->GetTechniqueByName("WideRectTech");
	eEllipse  = FX->GetTechniqueByName("EllipseTech");
	eLine     = FX->GetTechniqueByName("LineTech");
	eVP       = FX->GetParameterByName(0,"gVP");
	eTex0     = FX->GetParameterByName(0,"gTex0");
	eColor    = FX->GetParameterByName(0,"gColor");
	eData     = FX->GetParameterByName(0,"gData");
	eDash     = FX->GetParameterByName(0,"gDash");
	
	

	LogMsg("...rendering technique initialized");
}


// -----------------------------------------------------------------------------------------------
//
void D3D9Pad::GlobalExit()
{

	if (pens_allocated!=0)    LogErr("SketchPad Pens still in use %d",pens_allocated);
	if (brushes_allocated!=0) LogErr("SketchPad Brushes still in use %d",brushes_allocated);
	if (fonts_allocated!=0)   LogErr("SketchPad Fonts still in use %d",fonts_allocated);

	if (pens_allocated==0 && brushes_allocated==0 && fonts_allocated==0) LogAlw("Sketchap Exiting... All resources released");

	LogAlw("Clearing Font Cache... %d Fonts are stored in the cache",nfcache);
	for (int i=0;i<nfcache;i++) {
		if (fcache[i].pFont) delete fcache[i].pFont;
		if (fcache[i].hFont) DeleteObject(fcache[i].hFont);
	}

	SAFE_RELEASE(pCircleLow);
	SAFE_RELEASE(pCircleHigh);
	SAFE_RELEASE(FX);

	memset2(fcache, 0, 128*sizeof(FontCache));
}



// ======================================================================
// class GDIPad
// ======================================================================


D3D9Pad::D3D9Pad(SURFHANDLE s) : Sketchpad(s)
{
	_TRACE;

	LogOk("Creating D3D9 SketchPad...");

	cfont  = NULL;
	cpen   = NULL;
	cbrush = NULL;
	pTgt   = SURFACE(s);
	pDev   = pTgt->GetDevice();
	origx  = 0;
	origy  = 0;
	cx     = 0;
	cy     = 0;
	bkmode = TRANSPARENT;
	halign = TA_LEFT;
	valign = TA_TOP;

	bConvertTgt = false;

	brushcolor = D3DXCOLOR(0,1,0,1);
	bkcolor    = D3DXCOLOR(0,0,0,1);
	textcolor  = D3DXCOLOR(0,1,0,1);
	pencolor   = D3DXCOLOR(0,1,0,1);
	

	HR(FX->SetMatrix(eVP, pTgt->pVP));

	if (pTgt->IsBackBuffer()==false) {
		if (pTgt->BindGPU()==false) { 
			pTgt=NULL;  
			LogErr("D3D9Pad creation failed");
			return;
		}
	}

	cfont = deffont;
	//cpen  = defpen;
}


D3D9Pad::~D3D9Pad ()
{
	_TRACE;
	if (pTgt) if (pTgt->IsBackBuffer()==false) pTgt->ReleaseGPU();
	pTgt = NULL;

	//if (cfont!=deffont && cfont!=NULL) LogErr("Custom font still attached in sketchpad 0x%X",this); 
	//if (cpen!=defpen && cpen!=NULL) LogErr("Custom pen still attached in sketchpad 0x%X",this); 
	//if (cbrush!=NULL) LogErr("Custom brush still attached in sketchpad 0x%X",this); 
	
	LogOk("...D3D9 SketchPad Released");
}

Font *D3D9Pad::SetFont(Font *font) const
{
	Font *pfont = cfont;
	if (font) cfont = font;
	else      cfont = deffont;
	return pfont;
}

Brush *D3D9Pad::SetBrush (Brush *brush) const
{
	Brush *pbrush = cbrush;
	cbrush = brush;
	if (cbrush) brushcolor = ((D3D9PadBrush *)cbrush)->fcolor;
	else	    brushcolor = D3DXCOLOR(DWORD(0));
	return pbrush;
}

Pen *D3D9Pad::SetPen (Pen *pen) const
{
	Pen *ppen = cpen;
	if (pen) cpen = pen;
	else     cpen = NULL;
	if (cpen) pencolor = ((D3D9PadPen *)cpen)->fcolor;	
	return ppen;
}


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





DWORD D3D9Pad::SetTextColor(DWORD col)
{
	if ((col&0xFF000000)==0) col|=0xFF000000;
	D3DXCOLOR prev = textcolor;
	textcolor = D3DXCOLOR(col);
	D3DXCOLORSWAP(&textcolor);
	D3DXCOLORSWAP(&prev);
	return prev;
}

DWORD D3D9Pad::SetBackgroundColor(DWORD col)
{
	if ((col&0xFF000000)==0) col|=0xFF000000;
	D3DXCOLOR prev = bkcolor;
	bkcolor = D3DXCOLOR(col);
	D3DXCOLORSWAP(&bkcolor);
	D3DXCOLORSWAP(&prev);
	return prev;
}

void D3D9Pad::SetBackgroundMode(BkgMode mode)
{
	switch (mode) {
		case BK_TRANSPARENT: bkmode = TRANSPARENT; break;
		case BK_OPAQUE:      bkmode = OPAQUE; break;
	}
}

DWORD D3D9Pad::GetCharSize ()
{
	TEXTMETRIC tm;
	if (cfont==NULL) return 0;
	((D3D9PadFont *)cfont)->pFont->GetD3D9TextMetrics(&tm);
	return MAKELONG(tm.tmHeight-tm.tmInternalLeading, tm.tmAveCharWidth);
}

DWORD D3D9Pad::GetTextWidth (const char *str, int len)
{
	if (cfont==NULL) return 0;
	return DWORD(((D3D9PadFont *)cfont)->pFont->Length2(str, len));
}

void D3D9Pad::SetOrigin (int x, int y)
{
	origx = x;
	origy = y;
}

bool D3D9Pad::HasPen()
{
	if (cpen==NULL) return false;
	if (((D3D9PadPen*)cpen)->style==PS_NULL) return false;
	return true;
}

bool D3D9Pad::IsDashed()
{
	if (cpen==NULL) return false;
	if (((D3D9PadPen*)cpen)->style==PS_DOT) return true;
	return false;
}

bool D3D9Pad::HasWidePen()
{
	if (cpen==NULL) return false;
	if (((D3D9PadPen*)cpen)->width>1) return true;
	return false;
}

bool D3D9Pad::HasBrush()
{
	return (cbrush != NULL);
}

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

	if (bkmode==OPAQUE) pText->Print(&textcolor, origx+x-1, origy+y-1, str, len, pTgt->pVP, &bkcolor);
	else				pText->Print(&textcolor, origx+x-1, origy+y-1, str, len, pTgt->pVP);

	return true;
}

void D3D9Pad::Pixel (int x, int y, DWORD col)
{
	RECT rect = { x, y, x+1, y+1 };
	pDev->ColorFill(pTgt->pSurf, &rect, col); 
}

void D3D9Pad::MoveTo (int x, int y)
{
	cx = origx + x; 
	cy = origy + y;
}

void D3D9Pad::LineTo (int x, int y)
{
	if (!HasPen()) return;
	D3DXVECTOR3 pts[2];
	pts[0] = D3DXVECTOR3(float(cx), float(cy), 0.0f);
	pts[1] = D3DXVECTOR3(float(origx+x), float(origy+y), 0.0f);
	Lines(pts, 1);
	cx=origx+x; cy=origy+y;
}

void D3D9Pad::Line (int x0, int y0, int x1, int y1)
{
	if (!HasPen()) return;
	x0+=origx; y0+=origy;
	x1+=origx; y1+=origy;
	D3DXVECTOR3 pts[2];
	pts[0] = D3DXVECTOR3(float(x0), float(y0), 0.0f);
	pts[1] = D3DXVECTOR3(float(x1), float(y1), 0.0f);
	Lines(pts, 1);
	cx = x1; cy = y1;
}

void D3D9Pad::Rectangle (int l, int t, int r, int b)
{
	l+=origx; t+=origy;
	r+=origx; b+=origy;
	r--; b--;
	D3D9PadPen * pen = (D3D9PadPen*)cpen;
	if (HasWidePen()) Rectangle3(float(l+r)*0.5f, float(t+b)*0.5f, float(r-l), float(b-t), pen->width);
	else			  Rectangle2(float(l), float(t), float(r), float(b));	   
}

void D3D9Pad::Ellipse (int l, int t, int r, int b)
{
	l+=origx; t+=origy;
	r+=origx; b+=origy;
	r--; b--;
	Ellipse2(float(l), float(t), float(r), float(b));
}

// -----------------------------------------------------------------------------------------------
//
void D3D9Pad::Lines(D3DXVECTOR3 *pVert, int count)
{

	if (IsDashed()) {
		pVert[0].z = 0;
		for (int i=1;i<(count<<1);i++) {
			D3DXVECTOR3 d = pVert[i]-pVert[i-1]; d.z = 0.0f;
			pVert[i].z = pVert[i-1].z + D3DXVec3Length(&d);
		}
		FX->SetBool(eDash, true);
	}
	else FX->SetBool(eDash, false);

	if (HasPen()) {
		UINT numPasses=0;
		HR(pDev->SetVertexDeclaration(pPositionDecl));
		HR(FX->SetTechnique(eLine));
		HR(FX->SetVector(eColor, (LPD3DXVECTOR4)&pencolor));
		HR(FX->Begin(&numPasses, D3DXFX_DONOTSAVESTATE));
		HR(FX->BeginPass(0));
		HR(pDev->DrawPrimitiveUP(D3DPT_LINELIST, count, pVert, sizeof(D3DXVECTOR3)));
		HR(FX->EndPass());
		HR(FX->End());	
		gc->GetStats()->Draw++;
	}
}

void D3D9Pad::Polygon (const IVECTOR2 *pt, int npt)
{
	if (npt<3) return;

	UINT numPasses=0;
	int nIdx = 0;

	WORD *Idx = NULL;
	D3DXVECTOR3 *Vtx = new D3DXVECTOR3[npt+1];
	for (int i=0;i<npt;i++) Vtx[i].x = float(pt[i].x+origx), Vtx[i].y = float(pt[i].y+origy);

	Vtx[npt].x = float(pt[0].x+origx), Vtx[npt].y = float(pt[0].y+origy);

	if (IsDashed()) {
		Vtx[0].z = 0;
		for (int i=1;i<npt;i++) {
			D3DXVECTOR3 d = Vtx[i]-Vtx[i-1]; d.z = 0.0f;
			Vtx[i].z = Vtx[i-1].z + D3DXVec3Length(&d);
		}
		FX->SetBool(eDash, true);
	}
	else FX->SetBool(eDash, false);


	if (HasBrush()) {
		Idx = new WORD[(npt-2)*3];
		nIdx = CreatePolyIndexList(Vtx, npt, Idx);
	}

	if (Idx) pDev->SetRenderState(D3DRS_CULLMODE, D3DCULL_NONE);

	pDev->SetVertexDeclaration(pPositionDecl);

	FX->SetTechnique(eLine);
	FX->Begin(&numPasses, D3DXFX_DONOTSAVESTATE);
	FX->BeginPass(0);

	if (nIdx) {
		FX->SetVector(eColor, (LPD3DXVECTOR4)&brushcolor);
		FX->CommitChanges();
		pDev->DrawIndexedPrimitiveUP(D3DPT_TRIANGLELIST, 0, npt, nIdx/3, Idx, D3DFMT_INDEX16, Vtx, sizeof(D3DXVECTOR3));
		gc->GetStats()->Draw++;
	}

	if (HasPen()) {
		FX->SetVector(eColor, (LPD3DXVECTOR4)&pencolor);
		FX->CommitChanges();
		pDev->DrawPrimitiveUP(D3DPT_LINESTRIP, npt, Vtx, sizeof(D3DXVECTOR3));
		gc->GetStats()->Draw++;
	}

	FX->EndPass();
	FX->End();	

	if (Idx) {
		pDev->SetRenderState(D3DRS_CULLMODE, D3DCULL_CCW);
		delete []Idx;
	}

	delete []Vtx;
}


void D3D9Pad::Polyline (const IVECTOR2 *pt, int npt)
{
	if (HasPen()) {

		pDev->SetVertexDeclaration(pPositionDecl);

		UINT numPasses=0;
		D3DXVECTOR3 *Vtx = new D3DXVECTOR3[npt];
		for (int i=0;i<npt;i++) Vtx[i].x = float(pt[i].x+origx), Vtx[i].y = float(pt[i].y+origy);

		if (IsDashed()) {
			Vtx[0].z = 0;
			for (int i=1;i<npt;i++) {
				D3DXVECTOR3 d = Vtx[i]-Vtx[i-1]; d.z = 0.0f;
				Vtx[i].z = Vtx[i-1].z + D3DXVec3Length(&d);
			}
			FX->SetBool(eDash, true);
		}
		else FX->SetBool(eDash, false);

		FX->SetTechnique(eLine);
		FX->SetVector(eColor, (LPD3DXVECTOR4)&pencolor);
		FX->Begin(&numPasses, D3DXFX_DONOTSAVESTATE);
		FX->BeginPass(0);
		pDev->DrawPrimitiveUP(D3DPT_LINESTRIP, npt-1, Vtx, sizeof(D3DXVECTOR3));
		FX->EndPass();
		FX->End();	
		gc->GetStats()->Draw++;
		delete []Vtx;
	}
}



// -----------------------------------------------------------------------------------------------
//
void D3D9Pad::Rectangle3(float x, float y, float w, float h, int width)
{
	static WORD o_indices[24] = {0,1,2, 2,1,3, 2,3,4, 4,3,5, 4,5,6, 6,5,7, 6,7,0, 0,7,1};
	static WORD f_indices[6]  = {1,5,3, 1,7,5};

	float i = float(width/2);
	float u = float(width-int(i+1));

	D3DVECTOR verts[8] = {
		{-1.0f,  1.0f,  u},
		{-1.0f,  1.0f, -i},
		{ 1.0f,  1.0f,  u},
		{ 1.0f,  1.0f, -i},
		{ 1.0f, -1.0f,  u},
		{ 1.0f, -1.0f, -i},
		{-1.0f, -1.0f,  u},
		{-1.0f, -1.0f, -i}
	};
		
	UINT numPasses=0;
	pDev->SetVertexDeclaration(pPositionDecl);
	FX->SetVector(eData, &D3DXVECTOR4(x, y, w*0.5f, h*0.5f));	
	FX->SetTechnique(eWideRect);
	FX->SetBool(eDash, false);
	FX->Begin(&numPasses, D3DXFX_DONOTSAVESTATE);
	FX->BeginPass(0);

	if (HasBrush()) {
		FX->SetVector(eColor, (LPD3DXVECTOR4)&brushcolor);
		FX->CommitChanges();
		pDev->DrawIndexedPrimitiveUP(D3DPT_TRIANGLELIST, 0, 8, 2, &f_indices, D3DFMT_INDEX16, &verts, sizeof(D3DXVECTOR3));
		gc->GetStats()->Draw++;
	}
	
	if (HasPen()) {
		FX->SetVector(eColor, (LPD3DXVECTOR4)&pencolor);
		FX->CommitChanges();
		pDev->DrawIndexedPrimitiveUP(D3DPT_TRIANGLELIST, 0, 8, 8, &o_indices, D3DFMT_INDEX16, &verts, sizeof(D3DXVECTOR3));
		gc->GetStats()->Draw++;
	}
	
	FX->EndPass();
	FX->End();	
}

// -----------------------------------------------------------------------------------------------
//
void D3D9Pad::Ellipse2(float l, float t, float r, float b)
{
	UINT numPasses=0;
	bool bLow = true;
	float w = r-l, h = b-t;

	if (w>32.0f || h>32.0f) bLow=false;
	
	pDev->SetVertexDeclaration(pPositionDecl);
	
	if (bLow) pDev->SetStreamSource(0, pCircleLow, 0, sizeof(D3DXVECTOR3));
	else 	  pDev->SetStreamSource(0, pCircleHigh, 0, sizeof(D3DXVECTOR3));
		
	FX->SetTechnique(eEllipse);
	FX->SetBool(eDash, false);
	FX->SetVector(eData, &D3DXVECTOR4(l, t, w, h));
	FX->Begin(&numPasses, D3DXFX_DONOTSAVESTATE);
	FX->BeginPass(0);

	if (HasBrush()) {
		FX->SetVector(eColor, (LPD3DXVECTOR4)&brushcolor);
		FX->CommitChanges();
		pDev->SetRenderState(D3DRS_CULLMODE, D3DCULL_NONE);
		if (bLow) pDev->DrawPrimitive(D3DPT_TRIANGLEFAN, 0, 15);
		else	  pDev->DrawPrimitive(D3DPT_TRIANGLEFAN, 0, 63);
		pDev->SetRenderState(D3DRS_CULLMODE, D3DCULL_CCW);
		gc->GetStats()->Draw++;
	}

	if (IsDashed()) FX->SetBool(eDash, true);

	if (HasPen()) {
		FX->SetVector(eColor, (LPD3DXVECTOR4)&pencolor);
		FX->CommitChanges();
		if (bLow) pDev->DrawPrimitive(D3DPT_LINESTRIP, 1, 15);
		else	  pDev->DrawPrimitive(D3DPT_LINESTRIP, 1, 63);	
		gc->GetStats()->Draw++;
	}

	FX->EndPass();
	FX->End();	

	
}


// -----------------------------------------------------------------------------------------------
//
void D3D9Pad::Rectangle2(float l, float t, float r, float b)
{
	static WORD indices[6] = {0,1,2,0,2,3};

	static D3DVECTOR verts[5] = {
		{ 0.0f, 0.0f, 0.0f},
		{ 1.0f, 0.0f, 0.0f},
		{ 1.0f, 1.0f, 0.0f},
		{ 0.0f, 1.0f, 0.0f},
		{ 0.0f, 0.0f, 0.0f}
	};
		
	UINT numPasses=0;
	pDev->SetVertexDeclaration(pPositionDecl);
	FX->SetTechnique(eEllipse);
	FX->SetVector(eData, &D3DXVECTOR4(l, t, r-l, b-t));	
	FX->SetBool(eDash, false);
	
	FX->Begin(&numPasses, D3DXFX_DONOTSAVESTATE);
	FX->BeginPass(0);

	if (HasBrush()) {
		FX->SetVector(eColor, (LPD3DXVECTOR4)&brushcolor);
		FX->CommitChanges();
		pDev->DrawIndexedPrimitiveUP(D3DPT_TRIANGLELIST, 0, 4, 2, &indices, D3DFMT_INDEX16, &verts, sizeof(D3DXVECTOR3));
		gc->GetStats()->Draw++;
	}
	
	if (HasPen()) {
		FX->SetVector(eColor, (LPD3DXVECTOR4)&pencolor);
		FX->CommitChanges();
		pDev->DrawPrimitiveUP(D3DPT_LINESTRIP, 4, &verts, sizeof(D3DXVECTOR3));
		gc->GetStats()->Draw++;
	}

	FX->EndPass();
	FX->End();	
}




HDC D3D9Pad::GetDC() 
{ 
	if (pTgt->IsBackBuffer()) return NULL;

	/*if (!pTgt->bSkpGetDC) {
		pTgt->bSkpGetDC = true;
		bConvertTgt=true;
	}*/
	
	if (!pTgt->bSkpGetDCEr) {
		LogErr("!!Never Use Sketchpad::GetDC()!!  HDC not available, the surface is active render target at a moment");
		pTgt->bSkpGetDCEr = true;
	}
	return NULL;
}



short mod(short a, short b)
{
	if (a<0) return b-1;
	if (a>=b) return 0;
	return a;
}

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
				/*WORD a = */Out[idx] = In[mod(x-1,npt)]; idx++;
				/*WORD b = */Out[idx] = In[mod(x,npt)]; idx++;
				/*WORD c = */Out[idx] = In[mod(x+1,npt)]; idx++;
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


		


ID3DXEffect* D3D9Pad::FX = 0;	
D3DXHANDLE   D3D9Pad::eWideRect = 0;
D3DXHANDLE   D3D9Pad::eTech = 0;	
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
LPDIRECT3DDEVICE9 D3D9PadFont::pDev = 0;
LPDIRECT3DDEVICE9 D3D9PadPen::pDev = 0;
LPDIRECT3DDEVICE9 D3D9PadBrush::pDev = 0;
LPDIRECT3DDEVICE9 D3D9Pad::pDev = 0;





// ======================================================================
// class GDIFont
// ======================================================================

D3D9PadFont::D3D9PadFont(int height, bool prop, const char *face, Style style, int orientation) : Font(height, prop, face, style, orientation)
{
	fonts_allocated++;

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
		if (_stricmp(fcache[i].face,face)) continue;
		pFont = fcache[i].pFont;
		if (orientation==0) hFont = fcache[i].hFont;
	}
	
	int weight = (style & BOLD ? FW_BOLD : FW_NORMAL);
	DWORD italic = (style & ITALIC ? TRUE : FALSE);
	DWORD underline = (style & UNDERLINE ? TRUE : FALSE);

	DWORD AAQuality = NONANTIALIASED_QUALITY;

	if (Config->SketchpadFont==1) AAQuality = DRAFT_QUALITY;
	if (Config->SketchpadFont==2) AAQuality = CLEARTYPE_QUALITY;
	if (Config->SketchpadFont==3) AAQuality = PROOF_QUALITY;

	// Create Windows GDI Font for a use with GDIPad ---------------------------
	//
	if (hFont==NULL) {
		hFont = CreateFontA(height, 0, orientation, orientation, weight, italic, underline, 0, 0, 0, 2, AAQuality, 49, face);
		if (hFont==NULL) {
			face  = (prop ? def_sansface : def_fixedface);
			hFont = CreateFont(height, 0, orientation, orientation, weight, italic, underline, 0, 0, 0, 2, AAQuality, 49, face);
		}
	}

	// Create DirectX accelerated font for a use with D3D9Pad ------------------
	//
	if (pFont==NULL) {
	
		HFONT hNew = CreateFont(height, 0, 0, 0, weight, italic, underline, 0, 0, 0, 2, AAQuality, 49, face);

		pFont = new D3D9Text(pDev);
		pFont->Init(hNew, 255);

		DeleteObject(hNew);

		pFont->SetRotation(rotation);
		
		if (nfcache>120) {
			LogErr("Font Cache is Full.");
		}

		// Fill the cache --------------------------------
		//
		if (orientation) fcache[nfcache].hFont = NULL;  // Do not cache rotated hFont for Windows GDI
		else             fcache[nfcache].hFont = hFont;

		fcache[nfcache].pFont  = pFont;
		fcache[nfcache].height = height;
		fcache[nfcache].style  = style;
		fcache[nfcache].prop   = prop;
		strcpy_s(fcache[nfcache].face, 32, face);
		nfcache++;
	}
}

D3D9PadFont::~D3D9PadFont ()
{
	if (pFont) pFont->SetRotation(0.0f);
	fonts_allocated--;

	// If the current font is in a cache, do not delete it.
	for (int i=0;i<nfcache;i++) if (hFont == fcache[i].hFont) return;
	DeleteObject(hFont);
}

void D3D9PadFont::D3D9TechInit(LPDIRECT3DDEVICE9 pDevice)
{
	pDev = pDevice;
}


// ======================================================================
// class GDIPen
// ======================================================================

D3D9PadPen::D3D9PadPen (int s, int w, DWORD col): oapi::Pen (style, width, col)
{
	pens_allocated++;

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

D3D9PadPen::~D3D9PadPen ()
{
	pens_allocated--;
	DeleteObject(hPen);
}

void D3D9PadPen::D3D9TechInit(LPDIRECT3DDEVICE9 pDevice)
{
	pDev = pDevice;
}


// ======================================================================
// class GDIBrush
// ======================================================================

D3D9PadBrush::D3D9PadBrush (DWORD col): oapi::Brush (col)
{
	brushes_allocated++;
	hBrush = CreateSolidBrush(COLORREF(col&0xFFFFFF));
	if ((col&0xFF000000)==0) col|=0xFF000000;
	fcolor = D3DXCOLOR(col);
	D3DXCOLORSWAP(&fcolor);	
}

D3D9PadBrush::~D3D9PadBrush ()
{
	brushes_allocated--;
	DeleteObject(hBrush);
}

void D3D9PadBrush::D3D9TechInit(LPDIRECT3DDEVICE9 pDevice)
{
	pDev = pDevice;
}
