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

#include <windows.h>
#include <stdio.h>
#include <time.h>
#include <d3d9.h> 
#include <d3dx9.h>
#include "D3D9TextMgr.h"
#include "Log.h"
#include "D3D9Client.h"
#include "D3D9Surface.h"
#include "D3D9Util.h"
#include "D3D9Config.h"
#include "D3D9Pad.h"

// ----------------------------------------------------------------------------------------
//
D3D9Text::D3D9Text(LPDIRECT3DDEVICE9 pDevice) :
	red        (1.0),
	green      (1.0),
	blue       (1.0),
	alpha      (1.0),
	tex_w      (),
	tex_h      (),
	sharing    (),
	spacing    (),
	scaling	   (1.0f),
	linespacing(),
	max_len    (),
	rotation   (),
	last       (),
	charset    (ANSI_CHARSET),
	halign     (),
	valign     (),
	pDev       (pDevice),
	pTex       (NULL),
	pTgtSurf   (),
	Data       (NULL),
	tm         ()
{
	
}


// ----------------------------------------------------------------------------------------
//
D3D9Text::~D3D9Text()
{
	SAFE_DELETEA(Data);
	SAFE_RELEASE(pTex);
}


// ----------------------------------------------------------------------------------------
//
void D3D9Text::SetCharSet(int set)
{
	charset=set;
}


// ----------------------------------------------------------------------------------------
//
void D3D9Text::SetTextSpace(int space)
{
	spacing = (tm.tmHeight*space)/100;
}

// ----------------------------------------------------------------------------------------
//
void D3D9Text::SetTextHAlign(int x)
{
	halign=x;
}

// ----------------------------------------------------------------------------------------
//
void D3D9Text::SetTextVAlign(int x)
{
	valign=x;
}

// ----------------------------------------------------------------------------------------
//
void D3D9Text::SetTextShare(int share)
{
	sharing = (tm.tmHeight*share)/100;
}


// ----------------------------------------------------------------------------------------
//
void D3D9Text::SetLineSpace(int line)
{
	linespacing = tm.tmHeight + (tm.tmHeight*line)/100;
}


// ----------------------------------------------------------------------------------------
//
int D3D9Text::GetLineSpace()
{
	return linespacing;
}


// ----------------------------------------------------------------------------------------
//
bool D3D9Text::Init(int size, char *fontname, int weight, int last)
{
	// Create A Font
	//
	LOGFONT fnt;

	memset2((void *)&fnt, 0, sizeof(LOGFONT));

	fnt.lfHeight		 = size; 
	fnt.lfWidth			 = 0; 
	fnt.lfEscapement	 = 0; 
	fnt.lfOrientation    = 0; 
	fnt.lfWeight		 = weight; 
	fnt.lfItalic		 = false; 
	fnt.lfUnderline		 = false; 
	fnt.lfStrikeOut		 = false; 
	fnt.lfCharSet		 = charset;
	fnt.lfOutPrecision	 = OUT_DEFAULT_PRECIS; 
	fnt.lfClipPrecision	 = CLIP_DEFAULT_PRECIS; 
	fnt.lfQuality		 = ANTIALIASED_QUALITY; 
	fnt.lfPitchAndFamily = DEFAULT_PITCH | FF_MODERN; 

	if (fontname) strncpy_s(fnt.lfFaceName, 31, fontname, 30); 

	return Init(&fnt, last);
}


// ----------------------------------------------------------------------------------------
//
bool D3D9Text::Init(int size, int style, int weight, int last)
{
	// Create A Font
	//
	LOGFONT fnt;

	memset2((void *)&fnt, 0, sizeof(LOGFONT));

	fnt.lfHeight		 = size; 
	fnt.lfWidth			 = 0; 
	fnt.lfEscapement	 = 0; 
	fnt.lfOrientation    = 0; 
	fnt.lfWeight		 = weight; 
	fnt.lfItalic		 = false; 
	fnt.lfUnderline		 = false; 
	fnt.lfStrikeOut		 = false; 
	fnt.lfCharSet		 = charset; 
	fnt.lfOutPrecision	 = OUT_DEFAULT_PRECIS; 
	fnt.lfClipPrecision	 = CLIP_DEFAULT_PRECIS; 
	fnt.lfQuality		 = ANTIALIASED_QUALITY; 
	fnt.lfPitchAndFamily = style; 
 
	return Init(&fnt, last);
}

bool D3D9Text::Init(LOGFONT *fnt, int final)
{
	HFONT hF = CreateFontIndirect(fnt);
	return Init(hF, final);
}

// ----------------------------------------------------------------------------------------
//
bool D3D9Text::Init(HFONT hFont, int final)
{
	if (hFont==NULL) {
		LogErr("NULL Font in D3D9Text::Init()");
		return false;
	}

	LOGFONT fl;
	memset2((void *)&fl, 0, sizeof(LOGFONT));
	GetObject(hFont, sizeof(LOGFONT), &fl);

	tex_w = 2048;	// Texture Width
	tex_h = 32;
	last  = final;

	int first = 33;		// ANSI code of the First Charter
	
	// Allocate space for data
	//
	try {
		Data = new D3D9FontData[last]();	// zero-initialized
	}
	catch (std::bad_alloc&) {
		return false;
	}

	LogAlw("[NEW FONT] (%31s), Size=%d, Weight=%d Pitch&Family=%x",fl.lfFaceName, fl.lfHeight, fl.lfWeight, fl.lfPitchAndFamily); 

	bool bFirst = true;

restart:

	if (tex_h>=2048) {
		LogErr("^^ Font is too large for pre-rendering");	
		return false;
	}

	LPDIRECT3DTEXTURE9 pSrcTex = NULL;
	LPDIRECT3DSURFACE9 pSurf = NULL;

	if (pDev->CreateTexture(tex_w, tex_h, 1, 0, D3DFMT_R5G6B5, D3DPOOL_SYSTEMMEM, &pSrcTex, NULL)!=S_OK) {
		LogErr("D3D9Text::CreateOffscreenPlainSurface Fail");
		return false;
	}

	HR(pSrcTex->GetSurfaceLevel(0, &pSurf));

	HDC hDC = NULL;

	if (pSurf->GetDC(&hDC)!=S_OK) {
		LogErr("D3D9Text::GetDC Fail");
		return false;
	}
	
	HFONT hOld = (HFONT)SelectObject(hDC, hFont);

	if (hOld == NULL) { LogErr("SelectObject(hFont) FAIL"); return false; }

	if (bFirst) {

		// Get Text Metrics information
		// 
		memset2((void *)&tm, 0, sizeof(TEXTMETRIC));
		
		if (GetTextMetrics(hDC, &tm)==false) {
			LogErr("GetTextMetrics() FAIL");
			return false;
		}
		bFirst = false;
	}

	// Draw Charters
	//
	
	char text[4];	text[1]=0;

	int s = tm.tmMaxCharWidth;
	int a = tm.tmAscent + 1;
	int d = tm.tmDescent + 1;
	int h = a+d;
	int x = 5;
	int y = 5 + h;
	int c = 0;
	
	SIZE fnts;

	SetTextAlign(hDC, TA_BASELINE | TA_LEFT);
	SetTextColor(hDC, 0xFFFFFF);
	SetBkColor(hDC, 0);
	SetBkMode(hDC, TRANSPARENT);

	float tw = 1.0f / float(tex_w);
	float th = 1.0f / float(tex_h);
	
	while ( c < last ) {
		
		text[0] = c;
		text[1] = 0;
	
		TextOutA(hDC, x, y, text, 1);
		GetTextExtentPoint32(hDC, text, 1, &fnts);
		
		Data[c].sp  = float(fnts.cx);		// Char spacing
		Data[c].w   = float(fnts.cx+3);		// Char Width
		Data[c].h   = float(h);				// Char Height

		Data[c].tx0 = float(x-1);
		Data[c].tx1 = float(x-1 + fnts.cx+3);
		
		Data[c].ty0 = float(y - a);
		Data[c].ty1 = float(y + d);
		
		Data[c].tx0 *= tw;
		Data[c].tx1 *= tw;
		Data[c].ty0 *= th;
		Data[c].ty1 *= th;

		c++;	// Next Charter

		x += (fnts.cx + 4);		// --!!-- In order to increase spacing between charters increase this --!!--

		if ((x+s) >= tex_w) {	// Start a New Line
			x = 5;
			y+= (h+3);
		}

		if ((y+h) >= tex_h) {
			pSurf->ReleaseDC(hDC);
			pSurf->Release();
			pSrcTex->Release();
			tex_h *= 2;
			goto restart;
		}
	}

	SelectObject(hDC, hOld);

	pSurf->ReleaseDC(hDC);
	pSurf->Release();

	DeleteObject(hFont);

	HR(pDev->CreateTexture(tex_w, tex_h, 0, D3DUSAGE_AUTOGENMIPMAP, D3DFMT_R5G6B5, D3DPOOL_DEFAULT, &pTex, NULL));

	LogAlw("Font Video Memory Usage = %u kb",tex_w*tex_h*2/1024);


	if (pDev->UpdateTexture(pSrcTex, pTex)!=S_OK) {
		LogErr("D3D9TextMgr: Surface Update Failed");
		return false;
	}

	pTex->GenerateMipSubLevels();

	//char texname[256];
	//sprintf_s(texname, 256, "_%s_%d_0x%X.dds", fl.lfFaceName, fl.lfHeight, DWORD(this));
	//D3DXSaveSurfaceToFile(texname, D3DXIFF_DDS, pSurf, NULL, NULL);

	pSrcTex->Release();
	
	SetLineSpace(0);
	SetTextShare(0);
	SetTextSpace(0);

	LogAlw("Font and Charter set creation succesfull");

	return true;
}

// ----------------------------------------------------------------------------------------
//
void D3D9Text::SetColor(DWORD c)
{
	alpha = ((float)((c>>24)&0xFF)) / 255.0f;
	red   = ((float)((c>>16)&0xFF)) / 255.0f;
	green = ((float)((c>>8)&0xFF)) / 255.0f;
	blue  = ((float)(c&0xFF)) / 255.0f;	
}


// ----------------------------------------------------------------------------------------
//
void D3D9Text::SetColor(float r, float g, float b, float a=1.0)
{
	red=r; green=g; blue=b; alpha=a;	
}

// ----------------------------------------------------------------------------------------
//
void D3D9Text::Reset()
{
	max_len = 0;
}

// ----------------------------------------------------------------------------------------
//
float D3D9Text::Width()
{
	return max_len;
}

// ----------------------------------------------------------------------------------------
//
void D3D9Text::SetRotation(float deg)
{ 
	rotation = deg; 
}

// ----------------------------------------------------------------------------------------
//
void D3D9Text::SetScaling(float factor)
{
	scaling = factor;
}

// ----------------------------------------------------------------------------------------
//
float D3D9Text::Length(const char *format, ...)
{
	va_list args; 
    va_start(args, format); 
	
	int count = _vsnprintf_s((char *)Buffer, 512, 512, format, args); 
	va_end(args);

	return Length2(Buffer);
}

// ----------------------------------------------------------------------------------------
//
float D3D9Text::Length2(const char *str)
{
    float len = 0;
	unsigned char c = 1;
	int i = 0;

	while (str[i]) {
		if (str[i] <= last) len += (Data[str[i]].sp + float(spacing));
		i++;
	}

	len -= float(spacing);
	if (len<0) len=0;
	return len * scaling;
}


// ----------------------------------------------------------------------------------------
//
float D3D9Text::Length(char c)
{
	return (Data[c].sp + float(spacing)) * scaling;
}

// ----------------------------------------------------------------------------------------
//
float D3D9Text::PrintSkp(D3D9Pad *pSkp, float xpos, float ypos, const char *str, bool bBox)
{
	
	float x_orig = xpos;

	if (halign == 1) xpos -= Length2(str) * 0.5f;
	if (halign == 2) xpos -= Length2(str);
	if (valign == 1) ypos -= tm.tmAscent;
	if (valign == 2) ypos -= tm.tmHeight;

	xpos = ceil(xpos);
	ypos = ceil(ypos);
	xpos -= 0.5;
	ypos -= 0.5;

	float h = Data[0].h;

	float bbox_l = xpos - 2;
	float bbox_t = ypos + 1;
	float bbox_b = ypos + h - 1;
	float bbox_r = xpos + 2;

	unsigned char c = str[0];
	DWORD idx = 1;

	while (c && idx<255) {
		bbox_r += ceil(Data[c].sp + float(spacing));
		c = str[idx++];
	}

	D3DXMATRIX rot, out, mBak;
	bool bRestore = false;

	if (fabs(rotation)>1e-3 || fabs(scaling - 1.0f)>0.001f) {
		D3DXVECTOR2 center = D3DXVECTOR2((bbox_l + bbox_r)*0.5f, bbox_t);
		D3DXVECTOR2 scale = D3DXVECTOR2(scaling, scaling);
		center.x = ceil(center.x);
		center.y = ceil(center.y);
		D3DXMatrixTransformation2D(&rot, &center, 0.0f, &scale, &center, -rotation*0.01745329f, NULL);

		memcpy(&mBak, pSkp->WorldMatrix(), sizeof(D3DXMATRIX));
		D3DXMatrixMultiply(pSkp->WorldMatrix(), &rot, &mBak);
		bRestore = true;
	}

	if (bBox) {
		pSkp->Flush(SKPTECH_BLIT);
		pSkp->FillRect(int(bbox_l), int(bbox_t), int(bbox_r), int(bbox_b), pSkp->bkcolor);
	}

	idx = 1;
	c = str[0];

	pSkp->TexChangeNative(pTex);

	SkpVtx *pVtx = pSkp->Vtx;
	WORD *pIdx = pSkp->Idx;
	WORD iI = pSkp->iI;
	WORD vI = pSkp->vI;
	
	DWORD flags = SKPSW_FONT;
	DWORD color = pSkp->textcolor.dclr;

	while (c && idx<255) {

		pIdx[iI++] = vI;
		pIdx[iI++] = vI + 1;
		pIdx[iI++] = vI + 2;
		pIdx[iI++] = vI;
		pIdx[iI++] = vI + 2;
		pIdx[iI++] = vI + 3;

		float w = Data[c].w;

		SkpVtxFF(pVtx[vI++], xpos, ypos, Data[c].tx0, Data[c].ty0);
		SkpVtxFF(pVtx[vI++], xpos, ypos + h, Data[c].tx0, Data[c].ty1);
		SkpVtxFF(pVtx[vI++], xpos + w, ypos + h, Data[c].tx1, Data[c].ty1);
		SkpVtxFF(pVtx[vI++], xpos + w, ypos, Data[c].tx1, Data[c].ty0);

		pVtx[vI - 1].fnc = flags;
		pVtx[vI - 1].clr = color;
		pVtx[vI - 2].fnc = flags;
		pVtx[vI - 2].clr = color;
		pVtx[vI - 3].fnc = flags;
		pVtx[vI - 3].clr = color;
		pVtx[vI - 4].fnc = flags;
		pVtx[vI - 4].clr = color;

		xpos += ceil(Data[c].sp + float(spacing));

		c = str[idx++];
	}

	pSkp->vI = vI;
	pSkp->iI = iI;
	
	if (bRestore) {
		memcpy(pSkp->WorldMatrix(), &mBak, sizeof(D3DXMATRIX));
	}

	float l = xpos - x_orig;
	if (l>max_len) max_len = l;
	return l;
}


// -----------------------------------------------------------------------------------------------
//
void D3D9Text::D3D9TechInit(D3D9Client *_gc, LPDIRECT3DDEVICE9 pDev)
{
	Buffer = new char[512];
}

void D3D9Text::GlobalExit()
{
	SAFE_DELETEA(Buffer);
}

char *		 D3D9Text::Buffer = 0;