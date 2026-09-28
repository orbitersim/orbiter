
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

// windows.h, windowsx.h, d3d9.h and d3dx9.h left out: Qt fonts and VkCore textures

#include <stdio.h>
#include <math.h>
#include <QFont>

#include "D3D9Client.h"
#include "AABBUtil.h"


class SurfNative;

#define ANSI_CHARSET	0	// GDI charsets the client asks for (Windows-1252, Windows-1253)
#define GREEK_CHARSET	161

// not upstream: the TEXTMETRIC fields the client reads, from QFontMetrics
struct D3D9TextMetric {
	LONG tmHeight, tmAscent, tmDescent, tmInternalLeading, tmExternalLeading, tmAveCharWidth, tmMaxCharWidth, tmWeight;
};

// ----------------------------------------------------------------------------------------
//
struct D3D9FontData {
	float w, h;		// X,Y position of the charter baseline
	float sp;
	float tx0, ty0;
	float tx1, ty1;
};


// ----------------------------------------------------------------------------------------
//
class D3D9Text {

public:
	/**
	 * \brief Constructs a new text object
	 * \param pDevice direct 3D device instance pointer
	 */
	explicit D3D9Text(VkDev *pDevice);

	/**
	 * \brief Destroys the text object
	 */
	~D3D9Text();

	static void D3D9TechInit(oapi::D3D9Client *gc, VkDev *pDev);

	/**
	 * \brief Release global parameters
	 */
	static void GlobalExit();

	void		SetCharSet(int charset=ANSI_CHARSET);	// Must be set before Init

				// Init Will Create Charters from "first" (32:space) to "last" (255 ???)
	bool        Init(QFont *hFont);

	VkTex *		GetTexture() const { return pTex; }

	void        SetLineSpace(int percent=10);
	void		SetTextSpace(float space = 0.0f);
	void		SetTextShare(int percent=0);	// Percent of average width (default=0)

	void		SetColor(DWORD c);				// 0xAARRGGBB
	void		SetColor(float red, float green, float blue, float alpha);
	void		SetRotation(float deg);
	void		SetScaling(float factor);

	void		Reset();
	float		Width();
	int			GetLineSpace();

	float		Length2(const char *str, int len = -1);
	float		Length(BYTE c);
	int			GetIndex(const char *pText, float pos, int len = -1);

	void		SetTextHAlign(int x); // 0-left, 1=center, 2=right
	void		SetTextVAlign(int x); // 0-top, 1=base, 2=bottom

	float		PrintSkp (class D3D9Pad *pSkp, float x, float y, const char *str, int len = -1, bool bBox = false);
	float		PrintSkp (class D3D9Pad *pSkp, float x, float y, const wchar_t * str, int len = -1, bool bBox = false);

    void		GetD3D9TextMetrics(D3D9TextMetric *t) { memcpy(t, &tm, sizeof(D3D9TextMetric)); }

private:

	float	red, green, blue, alpha;

	int     tex_w;
	int     tex_h;
	int		sharing;
	float	spacing;
	int		linespacing;
	float	max_len;		  // If several strings are printed. This is the wide of the widest one
	float   rotation;
	float	scaling;
	int		charset;
	int     first;            ///< ANSI code of the first charter (FontData[0])
	int		halign,valign;

	D3D9FontData *Data (int c); ///< Returns FontData reference of a character

	VkDev *				pDev;
	VkTex *				pTex;
	D3D9FontData		*FontData;  ///< Array of font data information ( [c - first] )
	D3D9TextMetric		tm;         ///< Font attributes
	QFont               lf;         ///< Font attributes (and the WCHAR font, ID3DXFont upstream)

	// Rendering pipeline configuration
	//
	static char *		Buffer;
};

