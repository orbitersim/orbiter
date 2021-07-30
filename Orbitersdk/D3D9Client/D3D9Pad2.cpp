
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
#include "D3D9TextMgr.h"
#include "D3D9Surface.h"
#include "Scene.h"
#include "Mesh.h"
#include <d3dx9.h>
#include <sstream>




// ===============================================================================================
// Sketchpad2 Interface
// ===============================================================================================


bool D3D9Pad::IsTexture(HSURFNATIVE pSrc)
{
	LPDIRECT3DRESOURCE9 pResource = static_cast<LPDIRECT3DRESOURCE9>(pSrc);
	if (pResource->GetType() == D3DRTYPE_TEXTURE) return true;
	return false;
}


// ===============================================================================================
//
void D3D9Pad::GetRenderSurfaceSize(LPSIZE size)
{
	size->cx = tgt_desc.Width;
	size->cy = tgt_desc.Height;
}


// ===============================================================================================
//
void D3D9Pad::QuickPen(DWORD color, float width, DWORD style)
{
#ifdef SKPDBG 
	Log("QuickPen(0x%X, %d, %u)", color, width, style);
#endif

	if (QPen.bEnabled) {
		if ((QPen.style == style) && (QPen.width == width) && (QPen.color == color)) return;
	}

	Change |= SKPCHG_PEN;

	cpen = NULL;
	if (color == 0) QPen.bEnabled = false;
	else QPen.bEnabled = true;
	QPen.style = style;
	QPen.width = width;
	QPen.color = color;
	pencolor = SkpColor(ColorComp(color));

	IsLineTopologyAllowed();
}


// ===============================================================================================
//
void D3D9Pad::QuickBrush(DWORD color)
{
#ifdef SKPDBG 
	Log("QuickBrush(0x%X)", color);
#endif

	// No Change flags here
	cbrush = NULL;
	if (color == 0) QBrush.bEnabled = false;
	else QBrush.bEnabled = true;
	brushcolor = SkpColor(ColorComp(color));

	IsLineTopologyAllowed();
}


// ===============================================================================================
//
void D3D9Pad::AddRectIdx(WORD aV)
{
	Idx[iI++] = aV;
	Idx[iI++] = aV + 1;
	Idx[iI++] = aV + 2;
	Idx[iI++] = aV;
	Idx[iI++] = aV + 2;
	Idx[iI++] = aV + 3;
}


// ===============================================================================================
//
LPRECT D3D9Pad::CheckRect(SURFHANDLE hSrc, const LPRECT s)
{
	if (s) return s;
	src.left = 0;
	src.top = 0;
	src.right = SURFACE(hSrc)->GetWidth();
	src.bottom = SURFACE(hSrc)->GetHeight();
	return &src;
}


// ===============================================================================================
//
void D3D9Pad::CopyRect(SURFHANDLE hSrc, const LPRECT _s, int tx, int ty)
{
#ifdef SKPDBG 
	Log("CopyRect(0x%X)", DWORD(hSrc));
#endif

	TexChange(hSrc);

	if (Topology(TRIANGLE)) {

		LPRECT s = CheckRect(hSrc, _s);

		int h = abs(s->bottom - s->top);
		int w = abs(s->right - s->left);

		AddRectIdx(vI);

		SkpVtxII(Vtx[vI++], tx, ty, s->left, s->top);
		SkpVtxII(Vtx[vI++], tx, ty + h, s->left, s->bottom);
		SkpVtxII(Vtx[vI++], tx + w, ty + h, s->right, s->bottom);
		SkpVtxII(Vtx[vI++], tx + w, ty, s->right, s->top);

		DWORD x = SKPSW_TEXTURE | SKPSW_CENTER;

		Vtx[vI - 1].fnc = x;
		Vtx[vI - 2].fnc = x;
		Vtx[vI - 3].fnc = x;
		Vtx[vI - 4].fnc = x;
	}
}


// ===============================================================================================
//
void D3D9Pad::StretchRect(SURFHANDLE hSrc, const LPRECT _s, const LPRECT t)
{
#ifdef SKPDBG 
	Log("StretchRect(0x%X)", DWORD(hSrc));
#endif

	TexChange(hSrc);

	if (Topology(TRIANGLE)) {

		LPRECT s = CheckRect(hSrc, _s);

		AddRectIdx(vI);

		SkpVtxII(Vtx[vI++], t->left, t->top, s->left, s->top);
		SkpVtxII(Vtx[vI++], t->left, t->bottom, s->left, s->bottom);
		SkpVtxII(Vtx[vI++], t->right, t->bottom, s->right, s->bottom);
		SkpVtxII(Vtx[vI++], t->right, t->top, s->right, s->top);

		DWORD x = SKPSW_TEXTURE | SKPSW_CENTER;

		Vtx[vI - 1].fnc = x;
		Vtx[vI - 2].fnc = x;
		Vtx[vI - 3].fnc = x;
		Vtx[vI - 4].fnc = x;
	}
}


// ===============================================================================================
//
void D3D9Pad::RotateRect(SURFHANDLE hSrc, const LPRECT _s, int tcx, int tcy, float angle, float sw, float sh)
{
#ifdef SKPDBG 
	Log("RotateRect(0x%X)", DWORD(hSrc));
#endif

	TexChange(hSrc);

	if (Topology(TRIANGLE)) {

		LPRECT s = CheckRect(hSrc, _s);

		float w = float(s->right - s->left) * sw;
		float h = float(s->bottom - s->top) * sh;

		float san = sin(angle) * 0.5f;
		float can = cos(angle) * 0.5f;

		float ax = float(tcx) + (-w * can + h * san);
		float ay = float(tcy) + (-w * san - h * can);

		float bx = float(tcx) + (-w * can - h * san);
		float by = float(tcy) + (-w * san + h * can);

		float cx = float(tcx) + (+w * can - h * san);
		float cy = float(tcy) + (+w * san + h * can);

		float dx = float(tcx) + (+w * can + h * san);
		float dy = float(tcy) + (+w * san - h * can);

		AddRectIdx(vI);

		SkpVtxFI(Vtx[vI++], ax, ay, s->left, s->top);
		SkpVtxFI(Vtx[vI++], bx, by, s->left, s->bottom);
		SkpVtxFI(Vtx[vI++], cx, cy, s->right, s->bottom);
		SkpVtxFI(Vtx[vI++], dx, dy, s->right, s->top);

		DWORD x = SKPSW_TEXTURE | SKPSW_CENTER;

		Vtx[vI - 1].fnc = x;
		Vtx[vI - 2].fnc = x;
		Vtx[vI - 3].fnc = x;
		Vtx[vI - 4].fnc = x;
	}
}


// ===============================================================================================
//
void D3D9Pad::ColorKey(SURFHANDLE hSrc, const LPRECT _s, int tx, int ty)
{
#ifdef SKPDBG 
	Log("ColorKey(0x%X)", DWORD(hSrc));
#endif

	TexChange(hSrc);

	if (Topology(TRIANGLE)) {

		LPRECT s = CheckRect(hSrc, _s);

		int h = abs(s->bottom - s->top);
		int w = abs(s->right - s->left);

		AddRectIdx(vI);

		SkpVtxII(Vtx[vI++], tx, ty, s->left, s->top);
		SkpVtxII(Vtx[vI++], tx, ty + h, s->left, s->bottom);
		SkpVtxII(Vtx[vI++], tx + w, ty + h, s->right, s->bottom);
		SkpVtxII(Vtx[vI++], tx + w, ty, s->right, s->top);

		DWORD f = SKPSW_TEXTURE | SKPSW_COLORKEY | SKPSW_CENTER;

		Vtx[vI - 1].fnc = f;
		Vtx[vI - 2].fnc = f;
		Vtx[vI - 3].fnc = f;
		Vtx[vI - 4].fnc = f;
	}
}


// ===============================================================================================
//
void D3D9Pad::CopyRectNative(HSURFNATIVE pSrc, const LPRECT _s, int tx, int ty)
{
#ifdef SKPDBG 
	Log("CopyRectNative(0x%X)", DWORD(pSrc));
#endif

	assert(IsTexture(pSrc));
	TexChangeNative((LPDIRECT3DTEXTURE9)pSrc);

	if (Topology(TRIANGLE)) {

		LPRECT s = CheckRectNative((LPDIRECT3DTEXTURE9)pSrc, _s);

		int h = abs(s->bottom - s->top);
		int w = abs(s->right - s->left);

		AddRectIdx(vI);

		SkpVtxII(Vtx[vI++], tx, ty, s->left, s->top);
		SkpVtxII(Vtx[vI++], tx, ty + h, s->left, s->bottom - 1);
		SkpVtxII(Vtx[vI++], tx + w, ty + h, s->right - 1, s->bottom - 1);
		SkpVtxII(Vtx[vI++], tx + w, ty, s->right - 1, s->top);

		DWORD x = SKPSW_TEXTURE | SKPSW_CENTER;

		Vtx[vI - 1].fnc = x;
		Vtx[vI - 2].fnc = x;
		Vtx[vI - 3].fnc = x;
		Vtx[vI - 4].fnc = x;
	}
}


// ===============================================================================================
//
void D3D9Pad::StretchRectNative(HSURFNATIVE pSrc, const LPRECT _s, const LPRECT t)
{
#ifdef SKPDBG 
	Log("StretchRectNative(0x%X)", DWORD(pSrc));
#endif

	assert(IsTexture(pSrc));
	TexChangeNative((LPDIRECT3DTEXTURE9)pSrc);

	if (Topology(TRIANGLE)) {

		LPRECT s = CheckRectNative((LPDIRECT3DTEXTURE9)pSrc, _s);

		AddRectIdx(vI);

		SkpVtxII(Vtx[vI++], t->left, t->top, s->left, s->top);
		SkpVtxII(Vtx[vI++], t->left, t->bottom, s->left, s->bottom - 1);
		SkpVtxII(Vtx[vI++], t->right, t->bottom, s->right - 1, s->bottom - 1);
		SkpVtxII(Vtx[vI++], t->right, t->top, s->right - 1, s->top);

		DWORD x = SKPSW_TEXTURE | SKPSW_CENTER;

		Vtx[vI - 1].fnc = x;
		Vtx[vI - 2].fnc = x;
		Vtx[vI - 3].fnc = x;
		Vtx[vI - 4].fnc = x;
	}
}


// ===============================================================================================
//
void D3D9Pad::CopyQuadNative(HSURFNATIVE pSrc, const LPRECT _s, const FVECTOR2 *pt, const skpPin *pin, int npin)
{
#ifdef SKPDBG 
	Log("CopyQuadNative(0x%X)", DWORD(pSrc));
#endif

	assert(IsTexture(pSrc));
	TexChangeNative((LPDIRECT3DTEXTURE9)pSrc);
	
	IVECTOR2 spn = { 0, 0 };
	FVECTOR2 tpn = 0.0f;

	if (Topology(TRIANGLE)) {

		LPRECT s = CheckRectNative((LPDIRECT3DTEXTURE9)pSrc, _s);

		s->bottom--;
		s->right--;

		if (!pin) {
			tpn = (pt[0] + pt[1] + pt[2] + pt[3]) * 0.25f;
			spn = { (s->left + s->right) / 2 , (s->top + s->bottom) / 2 };
		}
		else if (npin == 1) {
			spn = pin->src;
			tpn = pin->tgt;
		}

		Idx[iI++] = vI + 0;
		Idx[iI++] = vI + 1;
		Idx[iI++] = vI + 4;

		Idx[iI++] = vI + 1;
		Idx[iI++] = vI + 2;
		Idx[iI++] = vI + 4;

		Idx[iI++] = vI + 2;
		Idx[iI++] = vI + 3;
		Idx[iI++] = vI + 4;

		Idx[iI++] = vI + 3;
		Idx[iI++] = vI + 0;
		Idx[iI++] = vI + 4;

		SkpVtxFI(Vtx[vI++], pt[0].x, pt[0].y, s->left, s->top);
		SkpVtxFI(Vtx[vI++], pt[1].x, pt[1].y, s->left, s->bottom);
		SkpVtxFI(Vtx[vI++], pt[2].x, pt[2].y, s->right, s->bottom);
		SkpVtxFI(Vtx[vI++], pt[3].x, pt[3].y, s->right, s->top);
		SkpVtxFI(Vtx[vI++], tpn.x, tpn.y, spn.x, spn.y);

		DWORD x = SKPSW_TEXTURE | SKPSW_CENTER;

		Vtx[vI - 1].fnc = x;
		Vtx[vI - 2].fnc = x;
		Vtx[vI - 3].fnc = x;
		Vtx[vI - 4].fnc = x;
		Vtx[vI - 5].fnc = x;
	}
}


// ===============================================================================================
//
bool D3D9Pad::TextW (int x, int y, const LPWSTR str, int len)
{
#ifdef SKPDBG 
	Log("TextW()");
#endif

	// No "Setup" here, done in PrintSkp()

	if (!cfont) return false;
	if (len == -1) len = int(wcslen(str));
	if (!len) return true;

	D3D9TextPtr pText = static_cast<D3D9PadFont *>(cfont)->pFont;

	int lineSpace = pText->GetLineSpace();

	std::wstring _str(str, str + size_t(len));

	std::wistringstream f(_str);
	std::wstring s;
	int _y = y;
	while (getline(f, s, L'\n')) {
		pText->PrintSkp(this, x - 1.0f, _y - 1.0f, s.c_str(), -1);
		_y += lineSpace;
	}

	return true;
}

// ===============================================================================================
//
void D3D9Pad::TextEx(float x, float y, const char *str, float scale, float angle)
{
#ifdef SKPDBG 
	Log("TextEx()");
#endif

	// No "Setup" here, done in PrintSkp()

	if (cfont == NULL) return;

	D3D9TextPtr pText = static_cast<D3D9PadFont *>(cfont)->pFont;

	switch (tah) {
		default:
		case LEFT:   pText->SetTextHAlign(0); break;
		case CENTER: pText->SetTextHAlign(1); break;
		case RIGHT:  pText->SetTextHAlign(2); break;
	}

	switch (tav) {
		default:
		case TOP:      pText->SetTextVAlign(0); break;
		case BASELINE: pText->SetTextVAlign(1); break;
		case BOTTOM:   pText->SetTextVAlign(2); break;
	}

	pText->SetRotation(angle);
	pText->SetScaling(scale);
	pText->PrintSkp(this, x - 1.0f, y - 1.0f, str, -1, (bkmode == OPAQUE));
}


// ===============================================================================================
//
void D3D9Pad::ClipRect(LPRECT clip)
{
#ifdef SKPDBG 
	Log("ClipRect(0x%X)", DWORD(clip));
#endif

	Change |= SKPCHG_CLIPRECT;

	bEnableScissor = (clip != NULL);
	if (clip) ScissorRect = (*clip);
	else ScissorRect = { 0,0,0,0 };
}


// ===============================================================================================
//
void D3D9Pad::Clipper(int idx, const VECTOR3 *uDir, double cos_angle, double dist)
{
#ifdef SKPDBG 
	Log("Clipper()");
#endif

	Change |= SKPCHG_CLIPCONE;

	if (idx < 0) idx = 0;
	if (idx > 1) idx = 1;

	if (uDir) {
		ClipData[idx].uDir = D3DXVEC(*uDir);
		ClipData[idx].ca = float(cos_angle);
		ClipData[idx].dst = float(dist);
		ClipData[idx].bEnable = true;
	}
	else {
		ClipData[idx].uDir = D3DXVECTOR3(0,0,1);
		ClipData[idx].ca = 2.0f;
		ClipData[idx].dst = 0.0f;
		ClipData[idx].bEnable = false;
	}
}


// ===============================================================================================
//
void D3D9Pad::DepthEnable(bool bEnable)
{
#ifdef SKPDBG 
	Log("DepthEnable(%u)", DWORD(bEnable));
#endif

	Flush(); // Must Flush() here before a mode change

	if (pDep) {
		Change |= SKPCHG_DEPTH;
		bDepthEnable = bEnable;
	}
	else bDepthEnable = false;
}


// ===============================================================================================
//
const FMATRIX4 *D3D9Pad::ViewMatrix() const
{
	return (const FMATRIX4*)&mV;
}


// ===============================================================================================
//
const FMATRIX4 *D3D9Pad::ProjectionMatrix() const
{
	return (const FMATRIX4*)&mP;
}


// ===============================================================================================
//
const FMATRIX4 *D3D9Pad::GetViewProjectionMatrix() const
{
	D3DXMatrixMultiply(&mVP, &mV, &mP);
	return (const FMATRIX4 *)&mVP;
}


// ===============================================================================================
//
void D3D9Pad::SetViewMatrix(const FMATRIX4 *pV)
{
#ifdef SKPDBG 
	Log("SetViewMatrix(0x%X)", DWORD(pV));
#endif
	Change |= SKPCHG_TRANSFORM;
	if (pV) memcpy(&mV, pV, sizeof(FMATRIX4));
	else D3DXMatrixIdentity(&mV);
}


// ===============================================================================================
//
void D3D9Pad::SetProjectionMatrix(const FMATRIX4 *pP)
{
#ifdef SKPDBG 
	Log("SetProjectionMatrix(0x%X)", DWORD(pP));
#endif
	Change |= SKPCHG_TRANSFORM;
	if (pP) memcpy(&mP, pP, sizeof(FMATRIX4));
	else D3DXMatrixIdentity(&mP);
}


// ===============================================================================================
//
void D3D9Pad::SetViewMode(SkpView mode)
{
#ifdef SKPDBG 
	Log("SetViewMode(0x%X)", DWORD(mode));
#endif
	Flush();	// Must Flush() here before a mode change
	vmode = mode;
}


// ===============================================================================================
// !! For a private use in D3D9Client !!
//
LPD3DXMATRIX D3D9Pad::WorldMatrix()
{
#ifdef SKPDBG 
	Log("WorldMatrix() ! - ! - !");
#endif
	Change |= SKPCHG_TRANSFORM;
	return (LPD3DXMATRIX)&mW;
}


// ===============================================================================================
//
void D3D9Pad::SetWorldTransform2D(float scale, float rot, IVECTOR2 *c, IVECTOR2 *t)
{
#ifdef SKPDBG 
	Log("SetWorldTransform2D(%f, %f, 0x%X, 0x%X)", scale, rot, DWORD(c), DWORD(t));
#endif
	Change |= SKPCHG_TRANSFORM;

	D3DXVECTOR2 ctr = D3DXVECTOR2(0, 0);
	D3DXVECTOR2 trl = D3DXVECTOR2(0, 0);

	if (c) ctr = D3DXVECTOR2(float(c->x), float(c->y));
	if (t) trl = D3DXVECTOR2(float(t->x), float(t->y));

	D3DXMatrixAffineTransformation2D(&mW, scale, &ctr, rot, &trl);
}


// ===============================================================================================
//
void D3D9Pad::SetWorldTransform(const FMATRIX4 *pWT)
{
#ifdef SKPDBG 
	Log("SetWorldTransform(0x%X)", DWORD(pWT));
#endif
	Change |= SKPCHG_TRANSFORM;

	if (pWT) memcpy(&mW, pWT, sizeof(FMATRIX4));
	else D3DXMatrixIdentity(&mW);
}


// ===============================================================================================
//
void D3D9Pad::SetGlobalLineScale(float width, float pat)
{
#ifdef SKPDBG 
	Log("SetGlobalLineScale(%f, %f)", width, pat);
#endif
	Change |= SKPCHG_PEN;
	linescale = width;
	pattern = pat;
}


// ===============================================================================================
//
void D3D9Pad::SetFontTextureNative(LPDIRECT3DTEXTURE9 hNew)
{
	if (hNew == hFontTex) return;
	Change |= SKPCHG_FONT;
	hFontTex = hNew;
}


// ===============================================================================================
//
bool D3D9Pad::TexChangeNative(LPDIRECT3DTEXTURE9 hNew)
{
	if (hNew == hTexture) return false;
	Change |= SKPCHG_TEXTURE;
	hTexture = hNew;
	return true;
}


// ===============================================================================================
//
void D3D9Pad::TexChange(SURFHANDLE hNew)
{
	static bool bOnce = true;

	if (!SURFACE(hNew)->IsTexture()) {
		if (bOnce) {
			LogErr("Sketchpad2: Source is not a texture");
			bOnce = false;
		}
		return;
	}

	TexChangeNative(SURFACE(hNew)->GetTexture());

	if (SURFACE(hNew)->IsColorKeyEnabled()) {
		bColorKey = true;
		cColorKey = SURFACE(hNew)->ClrKey;
	}
	else {
		bColorKey = false;
		cColorKey = D3DXCOLOR(DWORD(0));
	}
}


// ===============================================================================================
//
int D3D9Pad::DrawSketchMesh(SKETCHMESH _hMesh, DWORD grp, DWORD flags, SURFHANDLE hTex)
{

#ifdef SKPDBG 
	Log("DrawSketchMesh(0x%X, gpr=%u, flags=0x%X, hTex=0x%X)", DWORD(_hMesh), grp, flags, DWORD(hTex));
#endif

	UINT num;

	SketchMesh *hMesh = static_cast<SketchMesh *>(_hMesh);

	DWORD nGrp = hMesh->GroupCount();

	if (grp >= nGrp) return -1;

	// Flush Pending graphics ------------------------------------
	//
	SetupDevice(tCurrent);


	// Initialize device for drawing a mesh ----------------------
	//
	hMesh->Init();
	pDev->SetVertexDeclaration(pNTVertexDecl);

	if (flags&MF_CULL_NONE) pDev->SetRenderState(D3DRS_CULLMODE, D3DCULL_NONE);
	else				    pDev->SetRenderState(D3DRS_CULLMODE, D3DCULL_CCW);

	if (flags&MF_RENDER_ALL) grp = 0;
	else nGrp = grp + 1;

	HR(FX->SetTechnique(eDrawMesh));
	HR(FX->Begin(&num, D3DXFX_DONOTSAVESTATE));
	HR(FX->BeginPass(0));

	HR(FX->SetBool(eShade, (flags&MF_SMOOTH_SHADE) != 0));
	HR(FX->SetValue(ePen, &pencolor.fclr, sizeof(D3DXCOLOR)));


	// Draw a mesh group(s) ----------------------------------------
	//
	while (grp < nGrp) {

		SURFHANDLE pTex = hMesh->GetTexture(grp);
		D3DXCOLOR   Mat = hMesh->GetMaterial(grp);

		if (hTex) {
			HR(FX->SetTexture(eTex0, SURFACE(hTex)->GetTexture()));
			HR(FX->SetBool(eTexEn, true));
		}
		else {
			if (pTex) {
				HR(FX->SetTexture(eTex0, SURFACE(pTex)->GetTexture()));
				HR(FX->SetBool(eTexEn, true));
			}
			else {
				HR(FX->SetBool(eTexEn, false));
			}
		}

		HR(FX->SetValue(eMtrl, &Mat, sizeof(D3DXCOLOR)));
		HR(FX->CommitChanges());

		hMesh->RenderGroup(grp);

		grp++;
	}

	HR(FX->EndPass());
	HR(FX->End());

	return hMesh->GroupCount();
}


// ===============================================================================================
//
int D3D9Pad::DrawMeshGroup(MESHHANDLE hMesh, DWORD grp, DWORD flags, SURFHANDLE hTex)
{
#ifdef SKPDBG 
	Log("DrawMeshGroup(0x%X, gpr=%u, flags=0x%X, hTex=0x%X)", DWORD(hMesh), grp, flags, DWORD(hTex));
#endif
	UINT num;

	MESHGROUP *gr = oapiMeshGroup(hMesh, grp);

	if (!gr) return -1;

	// Flush Pending graphics ------------------------------------
	//
	SetupDevice(tCurrent);


	// Initialize device for drawing a mesh ----------------------
	//
	pDev->SetVertexDeclaration(pNTVertexDecl);
	HR(FX->SetTechnique(eDrawMesh));
	HR(FX->Begin(&num, D3DXFX_DONOTSAVESTATE));
	HR(FX->BeginPass(0));

	if (!hTex && gr->TexIdx>0) hTex = oapiGetTextureHandle(hMesh, gr->TexIdx);

	MATERIAL *mat = oapiMeshMaterial(hMesh, gr->MtrlIdx);

	if (mat) {
		HR(FX->SetValue(eMtrl, &mat->diffuse, sizeof(D3DXCOLOR)));
	}
	else {
		HR(FX->SetValue(eMtrl, &D3DXCOLOR(1,1,1,1), sizeof(D3DXCOLOR)));
	}

	if (hTex) {
		HR(FX->SetTexture(eTex0, SURFACE(hTex)->GetTexture()));
		HR(FX->SetBool(eTexEn, true));
	}
	else {
		HR(FX->SetBool(eTexEn, false));
	}

	HR(FX->SetBool(eShade, (flags&MF_SMOOTH_SHADE) != 0));
	HR(FX->SetValue(ePen, &pencolor.fclr, sizeof(D3DXCOLOR)));

	HR(FX->CommitChanges());

	if (flags&MF_CULL_NONE) pDev->SetRenderState(D3DRS_CULLMODE, D3DCULL_NONE);
	else				    pDev->SetRenderState(D3DRS_CULLMODE, D3DCULL_CCW);

	if (bDepthEnable && pDep) {
		pDev->SetRenderState(D3DRS_ZENABLE, 1);
		pDev->SetRenderState(D3DRS_ZWRITEENABLE, 1);
	}

	pDev->DrawIndexedPrimitiveUP(D3DPT_TRIANGLELIST, 0, gr->nVtx, gr->nIdx/3, gr->Idx, D3DFMT_INDEX16, gr->Vtx, sizeof(NTVERTEX));

	HR(FX->EndPass());
	HR(FX->End());

	return oapiMeshGroupCount(hMesh);
}





// ===============================================================================================
//
const LPRECT D3D9Pad::CheckRectNative(LPDIRECT3DTEXTURE9 hSrc, const LPRECT s)
{
	if (s) return s;
	D3DSURFACE_DESC desc;
	hSrc->GetLevelDesc(0, &desc);
	src.left = 0;
	src.top = 0;
	src.right = desc.Width;
	src.bottom = desc.Height;
	return &src;
}





// ======================================================================================
// SketchMesh Interface
// ======================================================================================

SketchMesh::SketchMesh(LPDIRECT3DDEVICE9 _pDev) :
	MaxVert(0), MaxIdx(0),
	nGrp(0), nMtrl(0), nTex(0),
	pDev(_pDev),
	Tex(NULL),
	Grp(NULL),
	Mtrl(NULL)
{
}


// ===============================================================================================
//
SketchMesh::~SketchMesh()
{
	SAFE_DELETEA(Mtrl);
	SAFE_DELETEA(Tex);
	SAFE_DELETEA(Grp);
	SAFE_RELEASE(pVB);
	SAFE_RELEASE(pIB);
}

// ===============================================================================================
//
bool SketchMesh::LoadMesh(const char *name)
{
	MESHHANDLE hMesh = oapiLoadMesh(name);

	if (hMesh) {
		bool bRet = LoadMeshFromHandle(hMesh);
		oapiDeleteMesh(hMesh);
		return bRet;
	}

	oapiWriteLogV("gcLoadSketchMesh(%s): Mesh not found", name);

	return false;
}

// ===============================================================================================
//
bool SketchMesh::LoadMeshFromHandle(MESHHANDLE hMesh)
{
	pVB = NULL;
	pIB = NULL;
	Mtrl = NULL;
	Tex = NULL;
	Grp = NULL;

	MaxVert = MaxIdx = 0;

	nGrp = oapiMeshGroupCount(hMesh);
	if (nGrp == 0) return false;

	Grp = new SKETCHGRP[nGrp];
	memset2(Grp, 0, sizeof(SKETCHGRP) * nGrp);

	// -----------------------------------------------------------------------

	nTex = oapiMeshTextureCount(hMesh) + 1;
	Tex = new LPD3D9CLIENTSURFACE[nTex];
	Tex[0] = 0; // 'no texture'
	for (DWORD i = 1; i<nTex; i++) Tex[i] = SURFACE(oapiGetTextureHandle(hMesh, i));

	// -----------------------------------------------------------------------

	nMtrl = oapiMeshMaterialCount(hMesh);
	if (nMtrl) Mtrl = new D3DXCOLOR[nMtrl];
	for (DWORD i = 0; i < nMtrl; i++) {
		MATERIAL *pMat = oapiMeshMaterial(hMesh, i);
		if (pMat) {
			Mtrl[i].r = pMat->diffuse.r;
			Mtrl[i].g = pMat->diffuse.g;
			Mtrl[i].b = pMat->diffuse.b;
			Mtrl[i].a = pMat->diffuse.a;
		}
	}

	// -----------------------------------------------------------------------

	for (DWORD i = 0; i < nGrp; i++) {
		MESHGROUPEX *pEx = oapiMeshGroupEx(hMesh, i);
		Grp[i].MtrlIdx = pEx->MtrlIdx;
		Grp[i].TexIdx = pEx->TexIdx;
		Grp[i].nVert = pEx->nVtx;
		Grp[i].nIdx = pEx->nIdx;
		Grp[i].VertOff = MaxVert;
		Grp[i].IdxOff = MaxIdx;
		MaxVert += pEx->nVtx;
		MaxIdx += pEx->nIdx;
	}

	if (MaxVert == 0 || MaxIdx == 0) return false;

	// -----------------------------------------------------------------------

	if (Grp[0].MtrlIdx == SPEC_INHERIT) Grp[0].MtrlIdx = SPEC_DEFAULT;
	if (Grp[0].TexIdx == SPEC_INHERIT) Grp[0].TexIdx = SPEC_DEFAULT;

	for (DWORD i = 0; i<nGrp; i++) {

		if (Grp[i].MtrlIdx == SPEC_INHERIT) Grp[i].MtrlIdx = Grp[i - 1].MtrlIdx;

		if (Grp[i].TexIdx == SPEC_DEFAULT) Grp[i].TexIdx = 0;
		else if (Grp[i].TexIdx == SPEC_INHERIT) Grp[i].TexIdx = Grp[i - 1].TexIdx;
		else Grp[i].TexIdx++;
	}

	// -----------------------------------------------------------------------

	HR(pDev->CreateVertexBuffer(MaxVert * sizeof(NTVERTEX), 0, 0, D3DPOOL_DEFAULT, &pVB, NULL));
	HR(pDev->CreateIndexBuffer(MaxIdx * sizeof(WORD), 0, D3DFMT_INDEX16, D3DPOOL_DEFAULT, &pIB, NULL));

	NTVERTEX *pVert = NULL;
	WORD *pIndex = NULL;

	for (DWORD i = 0; i < nGrp; i++) {
		MESHGROUPEX *pEx = oapiMeshGroupEx(hMesh, i);
		HR(pIB->Lock(Grp[i].IdxOff * sizeof(WORD), Grp[i].nIdx * sizeof(WORD), (LPVOID*)&pIndex, 0));
		HR(pVB->Lock(Grp[i].VertOff * sizeof(NTVERTEX), Grp[i].nVert * sizeof(NTVERTEX), (LPVOID*)&pVert, 0));
		memcpy(pIndex, pEx->Idx, sizeof(WORD) * pEx->nIdx);
		memcpy(pVert, pEx->Vtx, sizeof(NTVERTEX) * pEx->nVtx);
		HR(pIB->Unlock());
		HR(pVB->Unlock());
	}

	return true;
}


// ===============================================================================================
//
void SketchMesh::Init()
{
	pDev->SetStreamSource(0, pVB, 0, sizeof(NTVERTEX));
	pDev->SetIndices(pIB);
}


// ===============================================================================================
//
void SketchMesh::RenderGroup(DWORD idx)
{
	if (!pVB) return;
	pDev->DrawIndexedPrimitive(D3DPT_TRIANGLELIST, Grp[idx].VertOff, 0, Grp[idx].nVert, Grp[idx].IdxOff, Grp[idx].nIdx/3);
}


// ===============================================================================================
//
SURFHANDLE SketchMesh::GetTexture(DWORD idx)
{
	assert(idx < nGrp);
	if (Grp[idx].TexIdx) return Tex[Grp[idx].TexIdx];
	return NULL;
}


// ===============================================================================================
//
D3DXCOLOR SketchMesh::GetMaterial(DWORD idx)
{
	assert(idx < nGrp);
	if (Grp[idx].MtrlIdx!= SPEC_DEFAULT && Mtrl) return Mtrl[Grp[idx].MtrlIdx];
	return D3DXCOLOR(1,1,1,1);
}







// ======================================================================================
// Polyline Interface
// ======================================================================================



D3D9PolyLine::D3D9PolyLine(LPDIRECT3DDEVICE9 pDev, const FVECTOR2 *pt, int npt, bool bConnect) : D3D9PolyBase(0)
{
	nPt = npt + 2;
	nVtx = 2 * nPt;
	nIdx = 6 * nPt;

	HR(pDev->CreateVertexBuffer(nVtx * sizeof(SkpVtx), D3DUSAGE_DYNAMIC | D3DUSAGE_WRITEONLY, 0, D3DPOOL_DEFAULT, &pVB, NULL));
	HR(pDev->CreateIndexBuffer(nIdx * sizeof(WORD), D3DUSAGE_DYNAMIC | D3DUSAGE_WRITEONLY, D3DFMT_INDEX16, D3DPOOL_DEFAULT, &pIB, NULL));

	WORD *Idx = NULL;

	HR(pIB->Lock(0, 0, (LPVOID*)&Idx, D3DLOCK_DISCARD));

	iI = 0;

	for (WORD i=0,p=0;p<nPt;p++) {
		Idx[iI++] = i+0;
		Idx[iI++] = i+1;
		Idx[iI++] = i+2;
		Idx[iI++] = i+1;
		Idx[iI++] = i+3;
		Idx[iI++] = i+2;
		i += 2;
	}

	HR(pIB->Unlock());

	if (pt) Update(pt, npt, bConnect);
}


// ===============================================================================================
//
D3D9PolyLine::~D3D9PolyLine()
{

}


// ===============================================================================================
//
void D3D9PolyLine::Release()
{
	SAFE_RELEASE(pVB);
	SAFE_RELEASE(pIB);
}


// ===============================================================================================
//
void D3D9PolyLine::Draw(LPDIRECT3DDEVICE9 pDev)
{
	pDev->SetStreamSource(0, pVB, 0, sizeof(SkpVtx));
	pDev->SetIndices(pIB);
	pDev->SetRenderState(D3DRS_CULLMODE, D3DCULL_NONE);
	pDev->DrawIndexedPrimitive(D3DPT_TRIANGLELIST, 0, 0, vI, 0, vI-2);
}


// ===============================================================================================
//
void D3D9PolyLine::Update(const FVECTOR2 *_pt, int _npt, bool bConnect)
{
	SkpVtx *Vtx = NULL;
	D3DXVECTOR2 *pt = (D3DXVECTOR2 *)_pt;

	HR(pVB->Lock(0, 0, (LPVOID*)&Vtx, D3DLOCK_DISCARD));

	WORD npt = WORD(_npt);
	WORD li = WORD(npt - 1);

	vI = 0;
	float length = 0.0f;

	D3DXVECTOR2 pp; // Prev point
	D3DXVECTOR2 np;	// Next point

	bLoop = bConnect;

	// Line Init ------------------------------------------------------------
	//
	if (bLoop) pp = pt[li];
	else	   pp = pt[0] * 2.0 - pt[1];

	// Create line segments -------------------------------------------------
	//
	for (WORD i = 0; i<npt; i++) {

		if (i != li)	np = pt[i + 1];
		else {
			if (bLoop)	np = pt[0];
			else		np = pt[i] * 2 - pt[i - 1];
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
		// --------------------------------------
		Vtx[vI].fnc = SKPSW_WIDEPEN_L | SKPSW_PENCOLOR;
		Vtx[vII].fnc = SKPSW_WIDEPEN_R | SKPSW_PENCOLOR;
		vI+=2;
		// --------------------------------------
		pp = pt[i];
		length += D3DXVec2Length(&(np - pp));
	}

	if (bLoop) {
		Vtx[vI++] = Vtx[0];
		Vtx[vI++] = Vtx[1];
	}

	HR(pVB->Unlock());
}








// ======================================================================================
// Triangle Interface
// ======================================================================================



D3D9Triangle::D3D9Triangle(LPDIRECT3DDEVICE9 pDev, const gcCore::TriangleVtx *pt, int npt, int _style) : D3D9PolyBase(1)
{
	nPt = npt;
	style = _style;
	HR(pDev->CreateVertexBuffer(nPt * sizeof(SkpVtx), D3DUSAGE_DYNAMIC | D3DUSAGE_WRITEONLY, 0, D3DPOOL_DEFAULT, &pVB, NULL));
	if (pt) Update(pt, npt);
}


// ===============================================================================================
//
D3D9Triangle::~D3D9Triangle()
{

}


// ===============================================================================================
//
void D3D9Triangle::Release()
{
	SAFE_RELEASE(pVB);
}


// ===============================================================================================
//
void D3D9Triangle::Draw(LPDIRECT3DDEVICE9 pDev)
{
	pDev->SetStreamSource(0, pVB, 0, sizeof(SkpVtx));
	if (style == PF_TRIANGLES) 	pDev->DrawPrimitive(D3DPT_TRIANGLELIST, 0, nPt / 3);
	if (style == PF_FAN) pDev->DrawPrimitive(D3DPT_TRIANGLEFAN, 0, nPt - 2);
	if (style == PF_STRIP) pDev->DrawPrimitive(D3DPT_TRIANGLESTRIP, 0, nPt - 2);
}


// ===============================================================================================
//
void D3D9Triangle::Update(const gcCore::TriangleVtx *pt, int npt)
{
	SkpVtx *Vtx = NULL;
	HR(pVB->Lock(0, 0, (LPVOID*)&Vtx, D3DLOCK_DISCARD));

	memset(Vtx, 0, sizeof(SkpVtx)*npt);

	for (int i = 0; i < npt; i++) {
		Vtx[i].x = pt[i].pos.x;
		Vtx[i].y = pt[i].pos.y;
		Vtx[i].clr = pt[i].color;
		Vtx[i].fnc = SKPSW_CENTER | SKPSW_FRAGMENT;
	}
	HR(pVB->Unlock());
}
