
// ===================================================
// Copyright (C) 2012-2021 Jarmo Nikkanen
// licensed under LGPL v2
// ===================================================

#include "Pad.h"
#include "Surface.h"
#include "MathAPI.h"
#include <sstream>



// ===============================================================================================
// Sketchpad3 Interface
// ===============================================================================================

void vkPad::ColorCompatibility(bool bEnable)
{
#ifdef SKPDBG 
	Log("ColorCompatibility(%u)", DWORD(bEnable));
#endif
	bColorComp = bEnable;
}

// ===============================================================================================
//
const FMATRIX4 *vkPad::GetColorMatrix()
{
	return &ColorMatrix;
}


// ===============================================================================================
//
void vkPad::SetColorMatrix(const FMATRIX4 *pMatrix)
{
#ifdef SKPDBG 
	Log("SetColorMatrix(0x%X)", DWORD(pMatrix));
#endif
	if (pMatrix) {
		memcpy_s(&ColorMatrix, sizeof(FMATRIX4), pMatrix, sizeof(FMATRIX4));
		SetEnable(SKP3E_CMATR);
	}
	else {
		memset(&ColorMatrix, 0, sizeof(FMATRIX4));
		ColorMatrix.m11 = 1.0f;
		ColorMatrix.m22 = 1.0f;
		ColorMatrix.m33 = 1.0f;
		ColorMatrix.m44 = 1.0f;
		ClearEnable(SKP3E_CMATR);
	}
}


// ===============================================================================================
//
void vkPad::SetBrightness(const FVECTOR4 *pBrightness)
{
#ifdef SKPDBG 
	Log("SetBrightness(0x%X)", DWORD(pBrightness));
#endif
	if (pBrightness == NULL) SetColorMatrix(NULL);
	else {
		memset(&ColorMatrix, 0, sizeof(FMATRIX4));
		ColorMatrix.m11 = pBrightness->r;
		ColorMatrix.m22 = pBrightness->g;
		ColorMatrix.m33 = pBrightness->b;
		ColorMatrix.m44 = pBrightness->a;
		SetEnable(SKP3E_CMATR);
	}
}


// ===============================================================================================
//
FVECTOR4 vkPad::GetRenderParam(RenderParam param)
{
	switch (param) {
	case Sketchpad::RenderParam::PRM_GAMMA: return Gamma;
	case Sketchpad::RenderParam::PRM_NOISE: return Noise;
	}
	return F4_Zero;
}


// ===============================================================================================
//
void vkPad::SetRenderParam(RenderParam param, const FVECTOR4 *d)
{
#ifdef SKPDBG 
	Log("SetRenderParam(%u, 0x%X)", param, DWORD(d));
#endif
	if (d == NULL) {
		switch (param) {
		case Sketchpad::RenderParam::PRM_GAMMA: Gamma = F4_One; ClearEnable(SKP3E_GAMMA); break;
		case Sketchpad::RenderParam::PRM_NOISE: Noise = F4_Zero; ClearEnable(SKP3E_NOISE); break;
		}
		return;
	}

	switch (param) {
	case Sketchpad::RenderParam::PRM_GAMMA: Gamma = FVECTOR4(d->r, d->g, d->b, d->a); SetEnable(SKP3E_GAMMA);  break;
	case Sketchpad::RenderParam::PRM_NOISE: Noise = *d; SetEnable(SKP3E_NOISE);  break;
	}
}

// ===============================================================================================
//
void vkPad::SetEnable(DWORD config)
{
	Change |= SKPCHG_EFFECTS;
	Enable |= config;
}


// ===============================================================================================
//
void vkPad::ClearEnable(DWORD config)
{
	Change |= SKPCHG_EFFECTS;
	Enable &= (~config);
}


// ===============================================================================================
//
void vkPad::SetBlendState(BlendState dwState)
{
#ifdef SKPDBG 
	Log("SetBlendState(%u)", dwState);
#endif
	// Must Flush() here before a mode change
	Flush();
	dwBlendState = dwState;
}


// ===============================================================================================
//
FMATRIX4 vkPad::GetWorldTransform() const
{ 
	FMATRIX4 fm;
	memcpy_s(&fm, sizeof(FMATRIX4), &mW, sizeof(FMATRIX4));
	return fm;
}


// ===============================================================================================
//
void vkPad::PushWorldTransform()
{ 
#ifdef SKPDBG 
	Log("PushWorldTransform()");
#endif
	mWStack.push(mW);
}


// ===============================================================================================
//
void vkPad::PopWorldTransform()
{ 
#ifdef SKPDBG 
	Log("PopWorldTransform()");
#endif
	if (mWStack.empty() == false) {
		mW = mWStack.top();
		mWStack.pop();
		Change |= SKPCHG_TRANSFORM;
	}
}


// ===============================================================================================
//
void vkPad::SetWorldScaleTransform2D(const FVECTOR2 *scl, const IVECTOR2 *trl)
{
#ifdef SKPDBG 
	Log("SetWorldScaleTransform2D(0x%X, 0x%X)", DWORD(scl), DWORD(trl));
#endif
	Change |= SKPCHG_TRANSFORM;

	float sx = 1.0f, sy = 1.0f;

	if (scl) sx = scl->x, sy = scl->y;

	FVECTOR3 t;

	t.x = 0;
	t.y = 0;
	t.z = 0;

	if (trl) t.x = float(trl->x), t.y = float(trl->y);

	D3DMAT_Scale(&mW, sx, sy, 1.0f);
	D3DMAT_SetTranslation(&mW, &t);
}


// ===============================================================================================
//
void vkPad::GradientFillRect(const LPRECT R, DWORD c1, DWORD c2, bool bVertical)
{
#ifdef SKPDBG 
	Log("GradientFillRect()");
#endif

	DWORD a, b, c, d;

	a = d = c1; b = c = c2;

	if (bVertical) { a = b = c1; c = d = c2; }

	int l = R->left;	int r = R->right;
	int t = R->top;		int m = R->bottom;

	r--; m--;

	if (Topology(TRIANGLE)) {
		AddRectIdx(vI);
		SkpVtxGF(Vtx[vI++], l, t, a);
		SkpVtxGF(Vtx[vI++], r, t, b);
		SkpVtxGF(Vtx[vI++], r, m, c);
		SkpVtxGF(Vtx[vI++], l, m, d);
	}
}


// ===============================================================================================
//
void vkPad::ColorFill(DWORD color, const LPRECT tgt)
{
#ifdef SKPDBG 
	Log("ColorFill()");
#endif
	if (tgt) FillRect(tgt->left, tgt->top, tgt->right, tgt->bottom, SkpColor(color));
	else FillRect(0, 0, tgt_desc.Width, tgt_desc.Height, SkpColor(color));
}

// ===============================================================================================
//
void vkPad::StretchRegion(const skpRegion *rgn, const SURFHANDLE hSrc, const LPRECT out)
{
#ifdef SKPDBG 
	Log("StretchRegion()");
#endif
	const RECT *ext = &(rgn->outr);
	const RECT *itr = &(rgn->intr);

	int x0 = ext->left;
	int x1 = itr->left;
	int x2 = itr->right;
	int x3 = ext->right;

	int y0 = ext->top;
	int y1 = itr->top;
	int y2 = itr->bottom;
	int y3 = ext->bottom;

	int tx0 = out->left;
	int tx3 = out->right;
	int ty0 = out->top;
	int ty3 = out->bottom;

	int tx1 = tx0 + (x1 - x0);
	int tx2 = tx3 - (x3 - x2);
	int ty1 = ty0 + (y1 - y0);
	int ty2 = ty3 - (y3 - y2);

	// Corners
	if (x0 != x1 && y0 != y1) CopyRect(hSrc, &(_R(x0, y0, x1, y1)), tx0, ty0);	// TOP-LEFT
	if (x2 != x3 && y0 != y1) CopyRect(hSrc, &(_R(x2, y0, x3, y1)), tx2, ty0);	// TOP-RIGHT
	if (x0 != x1 && y2 != y3) CopyRect(hSrc, &(_R(x0, y2, x1, y3)), tx0, ty2);	// BTM-LEFT
	if (x2 != x3 && y2 != y3) CopyRect(hSrc, &(_R(x2, y2, x3, y3)), tx2, ty2);	// BTM-RIGHT

																					// Sides
	if (x1 != x2 && y0 != y1) StretchRect(hSrc, &(_R(x1, y0, x2, y1)), &(_R(tx1, ty0, tx2, ty1)));	// TOP
	if (x1 != x2 && y2 != y3) StretchRect(hSrc, &(_R(x1, y2, x2, y3)), &(_R(tx1, ty2, tx2, ty3)));	// BOTTOM
	if (x0 != x1 && y1 != y2) StretchRect(hSrc, &(_R(x0, y1, x1, y2)), &(_R(tx0, ty1, tx1, ty2)));	// LEFT
	if (x2 != x3 && y1 != y2) StretchRect(hSrc, &(_R(x2, y1, x3, y2)), &(_R(tx2, ty1, tx3, ty2)));	// RIGHT

	// Center
	StretchRect(hSrc, &(_R(x1, y1, x2, y2)), &(_R(tx1, ty1, tx2, ty2)));
}

// ===============================================================================================
//
void vkPad::Clear(DWORD color, bool bColor, bool bDepth)
{
	DWORD flags = 0;
	if (bColor) flags |= D3DCLEAR_TARGET;
	if (bDepth) flags |= D3DCLEAR_ZBUFFER;
	pDev->Clear(0, NULL, flags, color, 1.0f, 0);
}

// ===============================================================================================
//
void vkPad::SetClipDistance(float nr, float fr)
{
	D3DMAT_OrthoOffCenterLH(&mO, 0.0f, (float)tgt_desc.Width, (float)tgt_desc.Height, 0.0f, nr, fr);
	mP.m33 = fr / (fr - nr);
	mP.m43 = -nr * mP.m33;
}
