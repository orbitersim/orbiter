
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
	bConfigChange = true;
	Enable |= config;
}


// ===============================================================================================
//
void D3D9Pad::ClearEnable(DWORD config)
{
	bConfigChange = true;
	Enable &= (~config);
}