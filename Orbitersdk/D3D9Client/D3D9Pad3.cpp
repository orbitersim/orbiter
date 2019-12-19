
// =================================================================================================================================
//
// Copyright (C) 2018 Jarmo Nikkanen
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
#include "D3D9Surface.h"
#include <d3dx9.h>
#include <sstream>



// ===============================================================================================
// Sketchpad3 Interface
// ===============================================================================================


// ===============================================================================================
//
const FMATRIX4 *D3D9Pad::GetColorMatrix()
{
	return &ColorMatrix;
}


// ===============================================================================================
//
void D3D9Pad::SetColorMatrix(FMATRIX4 *pMatrix)
{
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
void D3D9Pad::SetBrightness(FVECTOR4 *pBrightness)
{
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
FVECTOR4 D3D9Pad::GetRenderParam(int param)
{
	switch (param) {
	case SKP3_PRM_GAMMA: return Gamma;
	case SKP3_PRM_NOISE: return Noise;
	}
	return FVECTOR4(0, 0, 0, 0);
}


// ===============================================================================================
//
void D3D9Pad::SetRenderParam(int param, FVECTOR4 *d)
{
	if (d == NULL) {

		switch (param) {
		case SKP3_PRM_GAMMA: Gamma = FVECTOR4(1, 1, 1, 1); ClearEnable(SKP3E_GAMMA); break;
		case SKP3_PRM_NOISE: Noise = FVECTOR4(0, 0, 0, 0); ClearEnable(SKP3E_NOISE); break;
		}

		return;
	}

	switch (param) {
	case SKP3_PRM_GAMMA: Gamma = FVECTOR4(d->r, d->g, d->b, d->a); SetEnable(SKP3E_GAMMA);  break;
	case SKP3_PRM_NOISE: Noise = *d; SetEnable(SKP3E_NOISE);  break;
	}
}

// ===============================================================================================
//
void D3D9Pad::SetEnable(DWORD config)
{
	Change |= SKPCHG_EFFECTS;
	Enable |= config;
}


// ===============================================================================================
//
void D3D9Pad::ClearEnable(DWORD config)
{
	Change |= SKPCHG_EFFECTS;
	Enable &= (~config);
}


// ===============================================================================================
//
void D3D9Pad::SetBlendState(DWORD dwState)
{
	// Must Flush() here before a mode change
	Flush();
	dwBlendState = dwState;
}


// ===============================================================================================
//
FMATRIX4 D3D9Pad::GetWorldTransform() const
{ 
	FMATRIX4 fm;
	memcpy_s(&fm, sizeof(FMATRIX4), &mW, sizeof(D3DXMATRIX));
	return fm;
}


// ===============================================================================================
//
void D3D9Pad::PushWorldTransform()
{ 
	mWStack.push(mW);
}


// ===============================================================================================
//
void D3D9Pad::PopWorldTransform()
{ 
	if (mWStack.empty() == false) {
		mW = mWStack.top();
		mWStack.pop();
		Change |= SKPCHG_TRANSFORM;
	}
}


// ===============================================================================================
//
void D3D9Pad::SetWorldScaleTransform2D(FVECTOR2 *scl, IVECTOR2 *trl)
{

	Change |= SKPCHG_TRANSFORM;

	float sx = 1.0f, sy = 1.0f;

	if (scl) sx = scl->x, sy = scl->y;

	D3DXVECTOR3 t;

	t.x = 0;
	t.y = 0;
	t.z = 0;

	if (trl) t.x = float(trl->x), t.y = float(trl->y);

	D3DXMatrixScaling(&mW, sx, sy, 1.0f);
	D3DMAT_SetTranslation(&mW, &t);
}


// ===============================================================================================
//
void D3D9Pad::GradientFillRect(const LPRECT R, DWORD c1, DWORD c2, bool bVertical)
{
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
void D3D9Pad::PatternFill(SURFHANDLE hSrc, const LPRECT t)
{
	if (!hSrc) return;

	TexChange(hSrc);

	Flush();

	HR(FX->SetVector(ePatScl, &D3DXVECTOR4(float(t->left), float(t->top), 0.0f, 0.0f)));
	HR(FX->SetBool(ePatEn, true));

	DWORD c = 0xFFFFFFFF;
	if (HasBrush()) c = brushcolor.dclr;

	if (Topology(TRIANGLE)) {
		AddRectIdx(vI);
		SkpVtxPF(Vtx[vI++], t->left, t->top, c);
		SkpVtxPF(Vtx[vI++], t->left, t->bottom, c);
		SkpVtxPF(Vtx[vI++], t->right, t->bottom, c);
		SkpVtxPF(Vtx[vI++], t->right, t->top, c);
	}

	Flush();

	HR(FX->SetBool(ePatEn, false));
}


// ===============================================================================================
//
void D3D9Pad::StretchRegion(const skpRegion *rgn, SURFHANDLE hSrc, LPRECT out)
{

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
	if (x0 != x1 && y0 != y1) CopyRect(hSrc, &_R(x0, y0, x1, y1), tx0, ty0);	// TOP-LEFT
	if (x2 != x3 && y0 != y1) CopyRect(hSrc, &_R(x2, y0, x3, y1), tx2, ty0);	// TOP-RIGHT
	if (x0 != x1 && y2 != y3) CopyRect(hSrc, &_R(x0, y2, x1, y3), tx0, ty2);	// BTM-LEFT
	if (x2 != x3 && y2 != y3) CopyRect(hSrc, &_R(x2, y2, x3, y3), tx2, ty2);	// BTM-RIGHT

																					// Sides
	if (x1 != x2 && y0 != y1) StretchRect(hSrc, &_R(x1, y0, x2, y1), &_R(tx1, ty0, tx2, ty1));	// TOP
	if (x1 != x2 && y2 != y3) StretchRect(hSrc, &_R(x1, y2, x2, y3), &_R(tx1, ty2, tx2, ty3));	// BOTTOM
	if (x0 != x1 && y1 != y2) StretchRect(hSrc, &_R(x0, y1, x1, y2), &_R(tx0, ty1, tx1, ty2));	// LEFT
	if (x2 != x3 && y1 != y2) StretchRect(hSrc, &_R(x2, y1, x3, y2), &_R(tx2, ty1, tx3, ty2));	// RIGHT

	// Center
	StretchRect(hSrc, &_R(x1, y1, x2, y2), &_R(tx1, ty1, tx2, ty2));
}