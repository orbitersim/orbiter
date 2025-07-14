// =================================================================================================================================
// The MIT Lisence:
//
// Copyright (C) 2025 Jarmo Nikkanen
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


#define STRICT 1
#define OAPI_IMPLEMENTATION

#include "MathAPI.h"
/*
using namespace oapi;

DLLEXPORT void oapiMatrixIdentity(FMATRIX4 *x)
{
	x->m21 = x->m31 = x->m41 = x->m12 = x->m32 = x->m42 = 0.0f;
	x->m13 = x->m23 = x->m43 = x->m14 = x->m24 = x->m34 = 0.0f;
	x->m11 = x->m22 = x->m33 = x->m44 = 1.0f;
}

DLLEXPORT FMATRIX4 mul(const FMATRIX4* a, const FMATRIX4* b)
{
	return FMATRIX4(XMMatrixMultiply(a->XM(), b->XM()));
}

DLLEXPORT FMATRIX4* oapiMatrixMultiply(FMATRIX4* o, const FMATRIX4* a, const FMATRIX4* b)
{
	o->Load(XMMatrixMultiply(a->XM(), b->XM()));
	return o;
}

DLLEXPORT FMATRIX4* oapiMatrixInverse(FMATRIX4* o, float* d, const FMATRIX4* a)
{
	XMVECTOR det;
	o->Load(XMMatrixInverse(&det, a->XM()));
	if (d != nullptr) *d = det.m128_f32[0];
	return o;
}

DLLEXPORT FMATRIX4* oapiMatrixRotationAxis(FMATRIX4* o, FVECTOR3* pAxis, float rad)
{
	o->Load(XMMatrixRotationAxis(pAxis->XM(), rad));
	return o;
}

DLLEXPORT FVECTOR3 oapiTransformCoord(const FVECTOR3& V, const FMATRIX4& M)
{
	float x = V.x * M.m11 + V.y * M.m21 + V.z * M.m31 + M.m41;
	float y = V.x * M.m12 + V.y * M.m22 + V.z * M.m32 + M.m42;
	float z = V.x * M.m13 + V.y * M.m23 + V.z * M.m33 + M.m43;
	float w = V.x * M.m14 + V.y * M.m24 + V.z * M.m34 + M.m44;
	w = 1.0f / w;
	return FVECTOR3(x * w, y * w, z * w);
}


DLLEXPORT FVECTOR3 oapiTransformNormal(const FVECTOR3& V, const FMATRIX4& M)
{
	float x = V.x * M.m11 + V.y * M.m21 + V.z * M.m31;
	float y = V.x * M.m12 + V.y * M.m22 + V.z * M.m32;
	float z = V.x * M.m13 + V.y * M.m23 + V.z * M.m33;
	return FVECTOR3(x, y, z);
}*/
