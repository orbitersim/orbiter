// =================================================================================================================================
// The MIT Lisence:
//
// Copyright (C) 2013-2016 Jarmo Nikkanen
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


#include <d3d9.h> 
#include "MathAPI.h"

#ifndef __D3D9TK_H
#define __D3D9TK_H

#define SctPwr 1.0
#define SctPwr2 2.0

using namespace oapi;

struct D9BBox {
	FVECTOR4 mn, mx, bs, a, b, c;
};

float		D9NearPlane(LPDIRECT3DDEVICE9 pDev, float zmin, float zmax, float dmax, const FMATRIX4 *pProj, bool bReduced);
int			D9ComputeMinMaxDistance(LPDIRECT3DDEVICE9 pDev, const D9BBox *in, const FMATRIX4 *pWV, const FVECTOR4 *F, float *zmin, float *zmax, float *dmin);
void		D9AddAABB(const D9BBox *in, const FMATRIX4 *pM, D9BBox *out, bool bReset=false);
void		D9UpdateAABB(D9BBox *box, const FMATRIX4 *pFisrt=NULL, const FMATRIX4 *pSecond=NULL);
void		D9ZeroAABB(D9BBox *box);
void		D9InitAABB(D9BBox *box);
void		D9AddPointAABB(D9BBox *box, FVECTOR3* point);
bool		D9IsAABBVisible(const D9BBox *in, const FMATRIX4 *pWV, const FVECTOR4 *F);
bool		D9IsBSVisible(const D9BBox *in, const FMATRIX4 *pWV, const FVECTOR4 *F);
FVECTOR4	D9LinearFieldOfView(const FMATRIX4 *pProj);
FVECTOR4	D9OffsetRange(double R, double r);
void		EnvMapDirection(int dir, FVECTOR3 *Dir, FVECTOR3 *Up);
FVECTOR3	WorldPickRay(float x, float y, const FMATRIX4* mProj, const FMATRIX4* mView);
bool		SolveLUSystem(int n, double *A, double *b, double *x, double *det=NULL);
#endif
