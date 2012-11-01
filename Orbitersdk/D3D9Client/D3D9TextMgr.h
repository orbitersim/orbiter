
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
#include <windowsx.h>

#include <stdio.h>
#include <math.h>
#include <malloc.h>
#include <d3d9.h> 
#include <d3dx9.h>

#include "D3D9Client.h"

class D3D9ClientSurface;

// ----------------------------------------------------------------------------------------
//
struct D3D9FontData {
	float w, h;		// X,Y position of the charter baseline
	float sp;
	float tx0, ty0;
	float tx1, ty1;	
	byte s, e;		// s=special charter flag, e=extend charter height
};


// ----------------------------------------------------------------------------------------
//
class D3D9Text {

public:

	static void D3D9TechInit(oapi::D3D9Client *gc, LPDIRECT3DDEVICE9 pDev, const char *folder);
	static void GlobalExit();

				D3D9Text(LPDIRECT3DDEVICE9 pDevice);
			   ~D3D9Text();
	
	void		SetCharSet(int charset=ANSI_CHARSET);	// Must be set before Init

				// Init Will Create Charters from 33 to "last"=255
	bool        Init(HFONT hFont, int last=255);
	bool        Init(LOGFONT *fnt, int last=255);
	bool		Init(int size=24, int Style=FIXED_PITCH|FF_MODERN, int weight=500, int last=255);
	bool		Init(int size=24, char *fontname=NULL, int weight=500, int last=255);

	LPDIRECT3DTEXTURE9	GetTexture() { return pTex; }			
	
	void        SetLineSpace(int percent=10);
	void		SetTextSpace(int space=0);		// Percent of average width (default=0)
	void		SetTextShare(int percent=0);	// Percent of average width (default=0)

	void		SetColor(DWORD c);				// 0xAARRGGBB
	void		SetColor(float red, float green, float blue, float alpha);
	void		SetRotation(float deg); 

	void		Reset();
	float		Width();
	int			GetLineSpace();
	
	float		Length(const char *format, ...);
	float		Length2(const char *str, int len=-1);
	float		Length(char c);

	void		SetTextHAlign(int x); // 0-left, 1=center, 2=right
	void		SetTextVAlign(int x); // 0-top, 1=base, 2=bottom
	float		Print(LPD3DXCOLOR color, int x, int y, const char *str, int len=-1, D3DXMATRIX *pVP=NULL, LPD3DXCOLOR bbox=NULL);
	
	void		GetD3D9TextMetrics(TEXTMETRIC *t) { memcpy(t, &tm, sizeof(TEXTMETRIC)); }

private:

	char	*Buffer;

	float	red, green, blue, alpha;
	
	int     tex_w;  
	int     tex_h;
	int		sharing;
	int		spacing;
	int		linespacing;
	float	max;		  // If several strings are printed. This is the wide of the widest one
	float   rotation;
	int		last;
	int		charset;
	int		halign,valign;

	LPDIRECT3DDEVICE9	pDev;
	LPDIRECT3DTEXTURE9	pTex;
	D3D9ClientSurface	*pTgtSurf;
	D3D9FontData		*Data;
	_ABC				*Abc;
	WORD				*indices;

	TEXTMETRIC  tm;
	D3DXMATRIX  mVP;

	// Rendering pipeline configuration
	//
	static oapi::D3D9Client * gc;
	static ID3DXEffect*	FX;			
	static D3DXHANDLE	eTech;	
	static D3DXHANDLE	eClear;	
	static D3DXHANDLE	eFill;
	static D3DXHANDLE	eVP;	
	static D3DXHANDLE	eColor;	
	static D3DXHANDLE	eTex0;	
	static D3DXHANDLE   eData;
};