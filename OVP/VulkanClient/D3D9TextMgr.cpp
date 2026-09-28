// =================================================================================================================================
// The MIT Lisence:
//
// Copyright (C) 2012-2026 Jarmo Nikkanen
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

// windows.h, d3d9.h and d3dx9.h left out: Qt fonts and VkCore textures
#include <stdio.h>
#include <time.h>
#include <QImage>
#include <QPainter>
#include <QFontMetrics>
#include "D3D9TextMgr.h"
#include "VkTexFile.h"
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
D3D9Text::D3D9Text(VkDev *pDevice) :
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
	first      (0),
	halign     (),
	valign     (),
	pDev       (pDevice),
	pTex       (NULL),
	FontData   (NULL)
{
	memset(&tm, 0, sizeof(D3D9TextMetric));
}


// ----------------------------------------------------------------------------------------
//
D3D9Text::~D3D9Text()
{
	SAFE_DELETEA(FontData);
	SAFE_DELETE(pTex);
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
// not upstream: the character a byte stands for in the font's charset (TextOutA)
static QChar CharsetChar(int c, int charset)
{
	static const unsigned short cp1252[32] = { 0x20AC, 0x81, 0x201A, 0x0192, 0x201E, 0x2026, 0x2020, 0x2021, 0x02C6, 0x2030, 0x0160, 0x2039, 0x0152, 0x8D, 0x017D, 0x8F,
		0x90, 0x2018, 0x2019, 0x201C, 0x201D, 0x2022, 0x2013, 0x2014, 0x02DC, 0x2122, 0x0161, 0x203A, 0x0153, 0x9D, 0x017E, 0x0178 };
	if (charset == GREEK_CHARSET && c >= 0xB8 && c != 0xBB && c != 0xBD && c != 0xD2 && c != 0xFF) return QChar(c + 0x2D0); // Windows-1253 letters
	if (c >= 0x80 && c < 0xA0) return QChar(cp1252[c - 0x80]);
	return QChar(c);
}

bool D3D9Text::Init(QFont *hFont)
{
	if (hFont==NULL) {
		LogErr("NULL Font in D3D9Text::Init()");
		return false;
	}

	// Receive font attributes
	lf = *hFont;

	tex_w = 2048;	// Texture Width
	tex_h = 32;
	
	// Allocate space for data
	//
	FontData = new D3D9FontData[256]();	// zero-initialized
	
	LogAlw("[NEW FONT] (%31s), Size=%d, Weight=%d Pitch&Family=%x", lf.family().toUtf8().constData(), lf.pixelSize(), (int)lf.weight(), lf.fixedPitch() ? 1 : 2);

	bool bFirst = true;

restart:

	if (tex_h>=2048) {
		LogErr("^^ Font is too large for pre-rendering");	
		return false;
	}

	QImage *pSrcTex = new QImage(tex_w, tex_h, QImage::Format_RGB16); // D3DFMT_R5G6B5 system memory texture
	pSrcTex->fill(0);

	QPainter *hDC = new QPainter(pSrcTex);
	
	hDC->setFont(*hFont); // SelectObject
	QFontMetrics fm(*hFont, pSrcTex);

	if (bFirst) {

		// Get Text Metrics information
		// 
		memset((void *)&tm, 0, sizeof(D3D9TextMetric));
		
		tm.tmAscent = fm.ascent(); // GetTextMetrics
		tm.tmDescent = fm.descent();
		tm.tmHeight = fm.height();
		tm.tmInternalLeading = std::max(0, fm.height() - (hFont->pixelSize() > 0 ? hFont->pixelSize() : fm.height()));
		tm.tmExternalLeading = fm.leading();
		tm.tmAveCharWidth = fm.averageCharWidth();
		tm.tmMaxCharWidth = fm.maxWidth();
		tm.tmWeight = (LONG)hFont->weight();
		bFirst = false;
	}

	// Draw Charters
	//
	
	int s = tm.tmMaxCharWidth;
	int a = tm.tmAscent + 1;
	int d = tm.tmDescent + 1;
	int h = a+d;
	int x = 5;
	int y = 5 + h;
	int c = first; // ANSI code of the First Charter
	D3D9FontData *pData;

	SIZE fnts;

	// TA_BASELINE | TA_LEFT: drawText takes the baseline; white text, transparent background
	hDC->setPen(QColor(255, 255, 255));

	float tw = 1.0f / float(tex_w);
	float th = 1.0f / float(tex_h);
	
	while ( c < 256 ) {
		pData = Data(c);

		QChar text = CharsetChar(c, charset);
	
		hDC->drawText(QPoint(x, y), QString(text)); // TextOutA
		fnts.cx = fm.horizontalAdvance(text); // GetTextExtentPoint32
		fnts.cy = fm.height();
		
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
			delete hDC;
			delete pSrcTex;
			tex_h *= 2;
			goto restart;
		}
	}

	hDC->end();
	delete hDC;

	delete hFont; // DeleteObject: the text object took the font over

	// mip chain generated from the top level (D3DUSAGE_AUTOGENMIPMAP, UpdateTexture, GenerateMipSubLevels)
	VkPixels px, mips;
	px.w = tex_w; px.h = tex_h; px.levels = 1; px.layers = 1;
	px.fmt = VK_FORMAT_R5G6B5_UNORM_PACK16;
	px.data.assign(1, std::vector<BYTE>((size_t)tex_w * tex_h * 2));
	for (int row = 0; row < tex_h; row++) memcpy(&px.data[0][(size_t)row * tex_w * 2], pSrcTex->constScanLine(row), (size_t)tex_w * 2);
	delete pSrcTex;

	LogAlw("Font Video Memory Usage = %u kb",tex_w*tex_h*2/1024);

	if (!VkConvertPixels(px, mips, px.fmt, SWZ_NONE, tex_w, tex_h, 0) || !(pTex = VkCreateTexture(pDev, mips, VK_IMAGE_USAGE_SAMPLED_BIT))) {
		LogErr("D3D9TextMgr: Surface Update Failed");
		return false;
	}

#ifdef FNTDBG
	char texname[256];
	snprintf(texname, 256, "_%s_%d_0x%lX.dds", lf.family().toUtf8().constData(), lf.pixelSize(), (unsigned long)(uintptr_t)this);
	VkSavePixels(texname, VKIFF_DDS, mips);
#endif 

	// Init WCHAR font: lf, drawn by PrintSkp(const wchar_t *) (ID3DXFont upstream)

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
float D3D9Text::PrintSkp (D3D9Pad *pSkp, float xpos, float ypos, const wchar_t * str, int len, bool bBox)
{

	if (len == -1) len = int(wcslen(str));

	LONG x = LONG(round(xpos)),
	     y = LONG(round(ypos));
	RECT rect = { x, y, 0, 0 };

	// Must Flush() pending graphics before drawing the text image
	pSkp->Flush();

	QString qs = QString::fromWCharArray(str, len);
	QFontMetrics wfm(lf);
	rect.right = rect.left + wfm.horizontalAdvance(qs); // DT_CALCRECT
	rect.bottom = rect.top + wfm.height();

	LONG width = rect.right - rect.left;

	switch(halign) {
		case 1: // CENTER
			rect.right -= width/2;
			rect.left -= width/2;
			break;
		case 2: // RIGHT
			rect.right -= width;
			rect.left -= width;
			break;
		default: // LEFT
			break;
	}

	switch(valign) {
		case 1: // BASELINE
			rect.top -= wfm.ascent();
			rect.bottom -= wfm.ascent();
			break;
		case 2: // BOTTOM
			rect.top -= wfm.height();
			rect.bottom -= wfm.height();
			break;
		default: // TOP
			break;
	}
	
	if (bBox)
	{
		pSkp->FillRect(rect.left-2, rect.top+1, rect.right+2, rect.bottom-1, pSkp->bkcolor);
	}

	// Must Flush() pending graphics before drawing the text image
	pSkp->Flush();

	// pSkp->textcolor.dclr is in the wrong format
	DWORD col = pSkp->textcolor.dclr & 0xff00ff00;
	col |= (pSkp->textcolor.dclr <<16)&0xff0000;
	col |= (pSkp->textcolor.dclr >>16)&0xff;

	// ID3DXFont::DrawTextW: the string drawn into an image and copied onto the target
	if (width > 0 && rect.bottom > rect.top)
	{
		QImage img(width, rect.bottom - rect.top, QImage::Format_ARGB32);
		img.fill(Qt::transparent);
		QPainter p(&img);
		p.setFont(lf);
		p.setPen(QColor((col >> 16) & 0xFF, (col >> 8) & 0xFF, col & 0xFF, (col >> 24) & 0xFF));
		p.drawText(0, wfm.ascent(), qs);
		p.end();
		VkPixels px;
		px.w = img.width(); px.h = img.height(); px.levels = 1; px.layers = 1;
		px.fmt = VK_FORMAT_B8G8R8A8_UNORM;
		px.data.assign(1, std::vector<BYTE>((size_t)px.w * px.h * 4));
		for (UINT row = 0; row < px.h; row++) memcpy(&px.data[0][(size_t)row * px.w * 4], img.constScanLine(row), (size_t)px.w * 4);
		VkTex *pText = VkCreateTexture(pDev, px, VK_IMAGE_USAGE_SAMPLED_BIT);
		if (pText) {
			pSkp->CopyRectNative(pText, NULL, rect.left, rect.top);
			pSkp->Flush();
			delete pText; // released once the frame is done with it
		}
	}

	return float(rect.right - rect.left);
}

// -----------------------------------------------------------------------------------------------
//
void D3D9Text::D3D9TechInit(D3D9Client *_gc, VkDev *pDev)
{
	Buffer = new char[512];
}

void D3D9Text::GlobalExit()
{
	SAFE_DELETEA(Buffer);
}

char *		 D3D9Text::Buffer = 0;

