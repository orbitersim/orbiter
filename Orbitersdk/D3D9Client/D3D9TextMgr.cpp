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

#if defined(_MSC_VER) && (_MSC_VER <= 1700 ) // Microsoft Visual Studio Version 2012 and lower
#define round(v) floor(v+0.5)
#endif

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
	spacing    (0.0f),
	linespacing(),
	max_len    (),
	rotation   (),
	scaling    (1.0f),
	charset    (ANSI_CHARSET),
	first      (32),
	halign     (),
	valign     (),
	pDev       (pDevice),
	pTex       (NULL),
	FontData   (NULL),
	wfont      (NULL)
{
	ZeroMemory(&tm, sizeof(TEXTMETRIC));
	ZeroMemory(&lf, sizeof(LOGFONT));	
}


// ----------------------------------------------------------------------------------------
//
D3D9Text::~D3D9Text()
{
	SAFE_DELETEA(FontData);
	SAFE_RELEASE(pTex);
	SAFE_RELEASE(wfont);
}


// ----------------------------------------------------------------------------------------
//
void D3D9Text::SetCharSet(int set)
{
	charset=set;
}


// ----------------------------------------------------------------------------------------
//
void D3D9Text::SetTextSpace(float space)
{
	spacing = space;
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
bool D3D9Text::Init(HFONT hFont)
{
	if (hFont==NULL) {
		LogErr("NULL Font in D3D9Text::Init()");
		return false;
	}

	// Receive font attributes
	GetObject(hFont, sizeof(LOGFONT), &lf);

	tex_w = 2048;	// Texture Width
	tex_h = 32;
	
	// Allocate space for data
	//
	FontData = new D3D9FontData[256]();	// zero-initialized
	
	LogAlw("[NEW FONT] (%31s), Size=%d, Weight=%d Pitch&Family=%x", lf.lfFaceName, lf.lfHeight, lf.lfWeight, lf.lfPitchAndFamily);

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
		memset((void *)&tm, 0, sizeof(TEXTMETRIC));
		
		if (GetTextMetrics(hDC, &tm)==false) {
			LogErr("GetTextMetrics() FAIL");
			return false;
		}
		bFirst = false;
	}

	// Draw Charters
	//
	
	char text[] = "c";

	int s = tm.tmMaxCharWidth;
	int a = tm.tmAscent + 1;
	int d = tm.tmDescent + 1;
	int h = a+d;
	int x = 5;
	int y = 5 + h;
	int c = first; // ANSI code of the First Charter
	D3D9FontData *pData;

	SIZE fnts;

	SetTextAlign(hDC, TA_BASELINE | TA_LEFT);
	SetTextColor(hDC, 0xFFFFFF);
	SetBkColor(hDC, 0);
	SetBkMode(hDC, TRANSPARENT);

	float tw = 1.0f / float(tex_w);
	float th = 1.0f / float(tex_h);
	
	while ( c < 256 ) {
		pData = Data(c);

		text[0] = c;
	
		TextOutA(hDC, x, y, text, 1);
		GetTextExtentPoint32(hDC, text, 1, &fnts);
		
		pData->sp  = float(fnts.cx);		// Char spacing
		pData->w   = float(fnts.cx+3);		// Char Width
		pData->h   = float(h);				// Char Height

		pData->tx0 = float(x-1);
		pData->tx1 = float(x-1 + fnts.cx+3);
		
		pData->ty0 = float(y - a);
		pData->ty1 = float(y + d);
		
		pData->tx0 *= tw;
		pData->tx1 *= tw;
		pData->ty0 *= th;
		pData->ty1 *= th;

		c++;	// Next Charter

		x += (fnts.cx + 4);		// --!!-- In order to increase spacing between charters increase this --!!--

		if ((x+s) >= tex_w) {	// Start a New Line
			x = 5;
			y+= (h+5);
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

#ifdef FNTDBG
	char texname[256];
	sprintf_s(texname, 256, "_%s_%d_0x%X.dds", lf.lfFaceName, lf.lfHeight, DWORD(this));
	D3DXSaveSurfaceToFile(texname, D3DXIFF_DDS, pSurf, NULL, NULL);
#endif 
	pSrcTex->Release();

	// Init WCHAR font
	HR(D3DXCreateFont(
		pDev,                 // D3D Device
		lf.lfHeight,          // Font height
		lf.lfWidth,           // Font width
		lf.lfWeight,          // Font Weight
		1,                    // MipLevels
		lf.lfItalic,          // Italic
		lf.lfCharSet,         // CharSet
		lf.lfOutPrecision,    // OutputPrecision
		lf.lfQuality,         // Quality
		lf.lfPitchAndFamily,  // PitchAndFamily
		lf.lfFaceName,        // pFacename,
		&wfont                // ppFont
	));

	SetLineSpace(0);
	SetTextShare(0);
	SetTextSpace(0);

	LogAlw("Font and Charter set creation succesfull");

	return true;
}

// ----------------------------------------------------------------------------------------
//
D3D9FontData *D3D9Text::Data (int c) {
#ifdef _DEBUG
	if (c < first) { c = first; } // <= did *never* happen, but better save than sorry
#endif
	return &FontData[c - first];
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
int	D3D9Text::GetIndex(const char *pText, float pos, int x)
{
	float del = 1e6;
	float len = 0.0f;
	int i = 0;
	int idx = 0;

	const BYTE *str = (const BYTE *)pText; 

	while (i < x || x < 0) {	
		if (fabs(pos - len) < del) {
			del = fabs(pos - len);
			idx = i;
		} else break;
		if (str[i] == 0) break;
		len += (Data(str[i])->sp + spacing);
		i++;
	}

	return idx;
}

// ----------------------------------------------------------------------------------------
//
float D3D9Text::Length2(const char *_str, int l)
{
	float len = 0;
	int i = 0;

	const BYTE *str = (const BYTE *)_str; // Negative index may occur without this

	while ((i<l || l<=0) && str[i]) {
		if (str[i] <= 255) len += (Data(str[i])->sp + spacing);
		i++;
	}

	len -= spacing;
	if (len<0) len=0;
	return len * scaling;
}


// ----------------------------------------------------------------------------------------
//
float D3D9Text::Length(BYTE c)
{
	return (Data(c)->sp + spacing) * scaling;
}

// ----------------------------------------------------------------------------------------
//
float D3D9Text::PrintSkp(D3D9Pad *pSkp, float xpos, float ypos, const char *_str, int len, bool bBox)
{

	pSkp->SetFontTextureNative(pTex);

	if (halign == 1) xpos -= Length2(_str, len) * 0.5f;
	if (halign == 2) xpos -= Length2(_str, len);
	if (valign == 1) ypos -= tm.tmAscent;
	if (valign == 2) ypos -= tm.tmHeight;

	const BYTE *str = (const BYTE *)_str;

	xpos = ceil(xpos);
	ypos = ceil(ypos);
	
	float x_orig = xpos;

	float h = FontData[0].h;

	float bbox_l = xpos - 2;
	float bbox_t = ypos + 1;
	float bbox_b = ypos + h - 1;
	float bbox_r = xpos + 2;

	unsigned char c = str[0];
	int idx = 1;

	while (c && (idx<=len || len<=0)) {
		bbox_r += ceil(Data(c)->sp + spacing);
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
		pSkp->FillRect(int(bbox_l), int(bbox_t+2), int(bbox_r), int(bbox_b), pSkp->bkcolor);
	}

	idx = 1;
	c = str[0];

	// Feed data directly into a drawing queue
	//
	if (pSkp->Topology(D3D9Pad::Topo::TRIANGLE)) {

		SkpVtx *pVtx = pSkp->Vtx;
		WORD *pIdx = pSkp->Idx;
		WORD iI = pSkp->iI;
		WORD vI = pSkp->vI;
	
		DWORD flags = SKPSW_FONT | SKPSW_CENTER | SKPSW_FRAGMENT;
		DWORD color = pSkp->textcolor.dclr;

		while (c && (idx <= len || len <= 0)) {

			D3D9FontData *pData = Data(c);

			pIdx[iI++] = vI;
			pIdx[iI++] = vI + 1;
			pIdx[iI++] = vI + 2;
			pIdx[iI++] = vI;
			pIdx[iI++] = vI + 2;
			pIdx[iI++] = vI + 3;

			float w = pData->w;
			float xp = ceil(xpos);
			SkpVtxFF(pVtx[vI++], xp, ypos, pData->tx0, pData->ty0);
			SkpVtxFF(pVtx[vI++], xp, ypos + h, pData->tx0, pData->ty1);
			SkpVtxFF(pVtx[vI++], xp + w, ypos + h, pData->tx1, pData->ty1);
			SkpVtxFF(pVtx[vI++], xp + w, ypos, pData->tx1, pData->ty0);

			pVtx[vI - 1].fnc = flags;
			pVtx[vI - 1].clr = color;
			pVtx[vI - 2].fnc = flags;
			pVtx[vI - 2].clr = color;
			pVtx[vI - 3].fnc = flags;
			pVtx[vI - 3].clr = color;
			pVtx[vI - 4].fnc = flags;
			pVtx[vI - 4].clr = color;

			xpos += (pData->sp + spacing);

			c = str[idx++];
		}

		pSkp->vI = vI;
		pSkp->iI = iI;
	}
	

	if (bRestore) {
		memcpy(pSkp->WorldMatrix(), &mBak, sizeof(D3DXMATRIX));
	}

	float l = xpos - x_orig;
	if (l>max_len) max_len = l;
	return l;
}

// ----------------------------------------------------------------------------------------
//
float D3D9Text::PrintSkp (D3D9Pad *pSkp, float xpos, float ypos, LPCWSTR str, int len, bool bBox)
{

	if (len == -1) len = int(wcslen(str));

	LONG x = LONG(round(xpos)),
	     y = LONG(round(ypos));
	RECT rect = { x, y, 0, 0 };

	// Must Flush() pending graphics before using ID3DXFont interface
	pSkp->Flush();

	wfont->DrawTextW(
		NULL,                     // pSprite
		str,                      // pString
		len,                      // Count
		&rect,                    // pRect
		DT_CALCRECT | DT_NOCLIP,  // Format
		pSkp->textcolor.dclr      // Color
	);

	if (bBox)
	{
		pSkp->FillRect(rect.left-2, rect.top+1, rect.right+2, rect.bottom-1, pSkp->bkcolor);
	}

	// Must Flush() pending graphics before using ID3DXFont interface
	pSkp->Flush();

	wfont->DrawTextW(
		NULL,                              // pSprite
		str,                               // pString
		len,                               // Count
		&rect,                             // pRect
		DT_VCENTER | DT_LEFT | DT_NOCLIP,  // Format
		pSkp->textcolor.dclr               // Color
	);

	return float(rect.right - rect.left);
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
