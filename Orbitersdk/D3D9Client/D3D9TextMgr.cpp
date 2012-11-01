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

#include <windows.h>
#include <stdio.h>
#include <time.h>
#include <malloc.h>
#include <d3d9.h> 
#include <d3dx9.h>
#include "D3D9TextMgr.h"
#include "Log.h"
#include "D3D9Client.h"
#include "D3D9Surface.h"
#include "D3D9Util.h"
#include "D3D9Config.h"

// ----------------------------------------------------------------------------------------
//
D3D9Text::D3D9Text(LPDIRECT3DDEVICE9 pDevice)
{
	Buffer   = (char *)malloc(512);
	pDev     = pDevice;
	charset  = ANSI_CHARSET;
	sharing  = 0;
	spacing  = 0;
	rotation = 0.0f;
	halign   = 0;
	valign   = 0;
	Abc      = NULL;
	Data     = NULL;
	pTex     = NULL;

	

	red = green = blue = alpha = 1.0;
	max = 0;

	indices = new WORD[6*258];

	for (int i=0,k=0;i<256;i++) {
		int j = i*4;
		indices[k] = j;	k++;
		indices[k] = j+1; k++;
		indices[k] = j+2; k++;
		indices[k] = j; k++;
		indices[k] = j+2; k++;
		indices[k] = j+3; k++;
	}
}


// ----------------------------------------------------------------------------------------
//
D3D9Text::~D3D9Text()
{
	if (Buffer) free(Buffer);	Buffer=NULL;
	if (Data)	free(Data);		Data=NULL;
	if (Abc)	free(Abc);		Abc=NULL;
	if (pTex)   pTex->Release(); pTex=NULL;
	if (indices) delete []indices;
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

	memset((void *)&fnt, 0, sizeof(LOGFONT));

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

	memset((void *)&fnt, 0, sizeof(LOGFONT));

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
	memset((void *)&fl, 0, sizeof(LOGFONT));
	GetObject(hFont, sizeof(LOGFONT), &fl);

	tex_w = 512;	// Texture Width
	//tex_h = 32 + (fl.lfHeight*fl.lfHeight * 4 * 255) / (3*512);	// Texture Height
	tex_h = 128;
	last  = final;

	int first = 33;		// ANSI code of the First Charter
	
	// Allocate space for data
	//
	Data = (struct D3D9FontData *) malloc( last * sizeof(D3D9FontData) );
	if (Data==NULL) return false;
	memset((void *)Data, 0, last * sizeof(D3D9FontData));

	Abc = (_ABC *) malloc( last * sizeof(_ABC) );
	if (Abc==NULL) return false;
	memset((void *)Abc, 0, last * sizeof(_ABC));

	LogAlw("[NEW FONT] (%31s), Size=%d, Weight=%d Pitch&Family=%x",fl.lfFaceName, fl.lfHeight, fl.lfWeight, fl.lfPitchAndFamily); 

	bool bFirst = true;

restart:

	if (tex_h>=2048) return false;

	LPDIRECT3DSURFACE9 pSurf = NULL;

	if (pDev->CreateOffscreenPlainSurface(tex_w, tex_h, D3DFMT_R5G6B5, D3DPOOL_SYSTEMMEM, &pSurf, NULL)!=S_OK) {
		LogErr("D3D9Text::CreateOffscreenPlainSurface Fail");
		return false;
	}

	HDC hDC = NULL;

	if (pSurf->GetDC(&hDC)!=S_OK) {
		LogErr("D3D9Text::GetDC Fail");
		return false;
	}
	
	HFONT hOld = (HFONT)SelectObject(hDC, hFont);

	if (hOld == NULL) { LogErr("SelectObject(hFont) FAIL"); return false; }

	if (bFirst) {

		// Get Char Width/Spacing information from 33 to 255
		//
		if (GetCharABCWidths(hDC, first, last, Abc)==false) {
			LogWrn("!! GetCharABCWidths FAIL !!  Non-Critical");
		}

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
		
		Data[c].sp  = float(fnts.cx);	// Character spacing
		Data[c].w   = float(fnts.cx+2);	// Texture Width
		Data[c].h   = float(h);			// Texture Height

		Data[c].tx0 = float(x-1);
		Data[c].tx1 = float(x-1 + fnts.cx+2);
		
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
			//LogWrn("Charters doesn't fit in used texture (%dx%d)", tex_w, tex_h);	
			pSurf->ReleaseDC(hDC);
			pSurf->Release();
			//tex_h = (tex_h*3)/2;
			tex_h *= 2;
			goto restart;
		}
	}

	SelectObject(hDC, hOld);
	pSurf->ReleaseDC(hDC);

	DeleteObject(hFont);

	HR(pDev->CreateTexture(tex_w, tex_h, 1, 0, D3DFMT_R5G6B5, D3DPOOL_DEFAULT, &pTex, NULL));

	LogAlw("Font Video Memory Usage = %u kb",tex_w*tex_h*2/1024);

	LPDIRECT3DSURFACE9 pTgt;
	pTex->GetSurfaceLevel(0,&pTgt);

	if (pDev->UpdateSurface(pSurf, NULL, pTgt, NULL)!=S_OK) {
		LogErr("D3D9TextMgr: Surface Update Failed");
		return false;
	}

	pTgt->Release();	
	pSurf->Release();
	
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
	max = 0;
}

// ----------------------------------------------------------------------------------------
//
float D3D9Text::Width()
{
	return max;
}

// ----------------------------------------------------------------------------------------
//
void D3D9Text::SetRotation(float deg)
{ 
	rotation = deg; 
}

// ----------------------------------------------------------------------------------------
//
float D3D9Text::Length(const char *format, ...)
{
	va_list args; 
    va_start(args, format); 
	
	int count = _vsnprintf_s((char *)Buffer, 512, 512, format, args); 
	va_end(args);

	return Length2(Buffer, count);
}

// ----------------------------------------------------------------------------------------
//
float D3D9Text::Length2(const char *str, int le)
{
    float len = 0;
	float spe = float(spacing);

	if (le==0 || le==-1) le = strlen(str);

	for (int i=0; i < le; i++) {
		unsigned char c = str[i];
		if (c<=last) len += (Data[c].sp + spe);
	}

	len = len - spe;
	if (len<0) len=0;
	return len;
}


// ----------------------------------------------------------------------------------------
//
float D3D9Text::Length(char c)
{
	return Data[c].sp + float(spacing);
}

// ----------------------------------------------------------------------------------------
//
float D3D9Text::Print(LPD3DXCOLOR color, int x, int y, const char *str, int len, D3DXMATRIX *pVP, LPD3DXCOLOR bbox)
{
	static WORD cIndex[6] = {0,2,1,0,3,2};

	float length = 0.0;
	float xpos = float(x);
	float ypos = float(y);
	float x_orig = xpos;
	
	if (len==0 || str==0) return 0;
	if (len==-1) len = strlen(str);

	if (len>250) {
		LogErr("D3D9Text::Print() Buffer overload");
		return 0.0f;
	}
	
	if (halign==1) xpos -= Length2(str,len) * 0.5f;
	if (halign==2) xpos -= Length2(str,len);
	if (valign==1) ypos -= tm.tmAscent;
	if (valign==2) ypos -= tm.tmHeight;
	
	UINT numPasses = 0;
	HRESULT hr = -1;

	xpos = ceil(xpos);
	ypos = ceil(ypos);

	xpos-=0.5;
	ypos-=0.5;

	float h = Data[0].h;

	SMVERTEX *VBuffer = new SMVERTEX[4*(len+1)];
	SMVERTEX *wri = VBuffer;
	
	float bbox_l = xpos;
	float bbox_t = ypos+1;
	float bbox_b = ypos+h-1;

	for (int i=0;i<len;i++) {

		unsigned char c = str[i];

		float w = Data[c].w;

		wri[0].x  = wri[3].x  = xpos;
		wri[0].y  = wri[1].y  = ypos;
		wri[0].tu = wri[3].tu = Data[c].tx0;
		wri[0].tv = wri[1].tv = Data[c].ty0;
		wri[1].x  = wri[2].x  = xpos+w;
		wri[1].tu = wri[2].tu = Data[c].tx1;
		wri[2].y  = wri[3].y  = ypos+h;
		wri[2].tv = wri[3].tv = Data[c].ty1;
		
		wri+=4;

		xpos += ceil(Data[c].sp + float(spacing));
	}

	float bbox_r = xpos+1;
	D3DXMATRIX rot, out;

	if (fabs(rotation)>1e-3) {
		D3DXMatrixTransformation2D(&rot, NULL, 0.0f, NULL, &D3DXVECTOR2((bbox_l+bbox_r)*0.5f, bbox_t), -rotation*0.01745329f, NULL);
		FX->SetMatrix(eVP, D3DXMatrixMultiply(&out,&rot,pVP));
	}
	else FX->SetMatrix(eVP, pVP);

	if (bbox) {

		D3DVECTOR Vtx[4] = {
			{ bbox_l, bbox_t, 0.0f},
			{ bbox_l, bbox_b, 0.0f},
			{ bbox_r, bbox_b, 0.0f},
			{ bbox_r, bbox_t, 0.0f}
		};

		pDev->SetVertexDeclaration(pPositionDecl);
		FX->SetValue(eColor, bbox, sizeof(D3DXCOLOR));
		FX->SetTechnique(eFill);
		FX->Begin(&numPasses, D3DXFX_DONOTSAVESTATE);
		FX->BeginPass(0);
		pDev->DrawIndexedPrimitiveUP(D3DPT_TRIANGLELIST, 0, 4, 2, &cIndex, D3DFMT_INDEX16, &Vtx, sizeof(D3DXVECTOR3));
		FX->EndPass();
		FX->End();
	}

	HR(pDev->SetVertexDeclaration(pPosTexDecl));
	
	if (Config->SketchpadFont==2) FX->SetTechnique(eClear);
	else					      FX->SetTechnique(eTech);

	HR(FX->SetTexture(eTex0, pTex));
	HR(FX->SetValue(eColor, color, sizeof(D3DXCOLOR)));
	HR(FX->Begin(&numPasses, D3DXFX_DONOTSAVESTATE));
	HR(FX->BeginPass(0));

	if (pDev->DrawIndexedPrimitiveUP(D3DPT_TRIANGLELIST, 0, len<<2, len<<1, indices, D3DFMT_INDEX16, VBuffer, sizeof(SMVERTEX))!=S_OK) {
		LogErr("D3D9Text::DrawIndexedPrimitiveUP Failed");
	}

	gc->GetStats()->Draw++;
	gc->GetStats()->Vertices += len<<2;

	HR(FX->EndPass());
	HR(FX->End());	

	delete []VBuffer;
	
	float l = xpos - x_orig;
	if (l>max) max=l;
	return l;
}


// -----------------------------------------------------------------------------------------------
//
void D3D9Text::D3D9TechInit(D3D9Client *_gc, LPDIRECT3DDEVICE9 pDev, const char *folder)
{
	// Initialize Techniques -------------------------------------------------------------------------
	//
	gc = _gc;

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

	eTech  = FX->GetTechniqueByName("FontTech");
	eClear = FX->GetTechniqueByName("ClearTypeTech");
	eFill  = FX->GetTechniqueByName("FillTech");
	eVP    = FX->GetParameterByName(0,"gVP");
	eTex0  = FX->GetParameterByName(0,"gTex0");
	eColor = FX->GetParameterByName(0,"gColor");
	eData  = FX->GetParameterByName(0,"gData");
	

	LogMsg("...rendering technique initialized");
}

void D3D9Text::GlobalExit()
{
	SAFE_RELEASE(FX);
}


oapi::D3D9Client * D3D9Text::gc = 0;
ID3DXEffect* D3D9Text::FX = 0;			
D3DXHANDLE   D3D9Text::eTech = 0;	
D3DXHANDLE   D3D9Text::eClear = 0;
D3DXHANDLE   D3D9Text::eFill = 0;	
D3DXHANDLE   D3D9Text::eVP = 0;			
D3DXHANDLE   D3D9Text::eColor = 0;	
D3DXHANDLE   D3D9Text::eTex0 = 0;	
D3DXHANDLE   D3D9Text::eData = 0;