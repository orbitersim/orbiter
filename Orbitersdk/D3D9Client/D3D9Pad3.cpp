
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
	return _FVECTOR4(0, 0, 0, 0);
}


// ===============================================================================================
//
void D3D9Pad::SetRenderParam(int param, FVECTOR4 *d)
{
	if (d == NULL) {

		switch (param) {
		case SKP3_PRM_GAMMA: Gamma = _FVECTOR4(1, 1, 1, 1); ClearEnable(SKP3E_GAMMA); break;
		case SKP3_PRM_NOISE: Noise = _FVECTOR4(0, 0, 0, 0); ClearEnable(SKP3E_NOISE); break;
		}

		return;
	}

	switch (param) {
	case SKP3_PRM_GAMMA: Gamma = _FVECTOR4(d->r, d->g, d->b, d->a); SetEnable(SKP3E_GAMMA);  break;
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