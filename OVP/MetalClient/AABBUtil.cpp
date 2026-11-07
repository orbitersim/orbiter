#include <algorithm>
// =================================================================================================================================
// The MIT Lisence:
//
// Copyright (C) 2013-2026 Jarmo Nikkanen
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

#include "AABBUtil.h"
#include "OrbiterAPI.h"
#include "VectorHelpers.h"
#include "Log.h"

#pragma warning(push)
#pragma warning(disable : 4838)
#pragma warning(pop)

using std::min;

// =================================================================================================================================
//
bool SolveLUSystem(int n, double *A, double *b, double *x, double *det)
{		
	int e=0, *p = new int[n]; 
	for (int i=0;i<n;i++) p[i] = i;
	for (int k=0;k<n;k++) {
		int r = 0; double d = 0.0; 
		for (int s=k;s<n;s++) if (fabs(A[s*n+k])>d) { d = fabs(A[s*n+k]); r = s; }
		if (d == 0.0) { LogErr("Singular Matrix in SolveLUSystem()"); delete []p; p = NULL; return false; }
		if (r!=k) { // Do Swaps
			for (int i=0;i<n;i++) { double x = A[k*n+i]; A[k*n+i] = A[r*n+i]; A[r*n+i] = x; } 
			int x=p[k]; p[k]=p[r]; p[r]=x; e++;
		}
		for (int i=k+1;i<n;i++) { A[i*n+k]/=A[k*n+k]; for (int j=k+1;j<n;j++) A[i*n+j]-=(A[i*n+k]*A[k*n+j]); }
	}
	for (int i=0;i<n;i++) {	x[i] = b[p[i]];	for (int j=0;j<i;j++) x[i] -= A[i*n+j]*x[j]; }
	for (int i=n-1;i>=0;i--) { for (int j=i+1;j<n;j++) x[i] -= A[i*n+j]*x[j]; x[i] /= A[i*n+i]; }
	if (det) { *det = 1.0; for (int i=0;i<n;i++) *det *= A[i*n+i]; if (e&1) *det*=-1.0; } 
	delete []p;
	p = NULL;
	return true;
}


D3DXVECTOR3 WorldPickRay(float x, float y, const LPD3DXMATRIX mProj, const LPD3DXMATRIX mView)
{
	x = float((x*2.0-1.0)/mProj->_11);
	y = float((y*2.0-1.0)/mProj->_22);
	D3DXVECTOR3 pick(x, 1.0f, y);
	D3DXMATRIX mViewI;
	D3DXMatrixInverse(&mViewI, NULL, (const D3DXMATRIX *)&mView);
	D3DXVec3TransformNormal(&pick, &pick, &mViewI);
	D3DXVec3Normalize(&pick, &pick);
	return pick;
}


void D9ZeroAABB(D9BBox *box)
{
	memset(box, 0, sizeof(D9BBox));
}

void D9InitAABB(D9BBox *box)
{
	box->min = D3DXVECTOR4( 1e12f,  1e12f,  1e12f, 0); 
	box->max = D3DXVECTOR4(-1e12f, -1e12f, -1e12f, 0);
	box->bs  = D3DXVECTOR4(0,0,0,0);
}


void D9AddPointAABB(D9BBox *box, LPD3DXVECTOR3 point)
{
	box->min.x = std::min(box->min.x, point->x); box->min.y = std::min(box->min.y, point->y); box->min.z = std::min(box->min.z, point->z);
	box->max.x = std::max(box->max.x, point->x); box->max.y = std::max(box->max.y, point->y); box->max.z = std::max(box->max.z, point->z);
	box->min.w = std::min(box->min.w, 0.0f); box->max.w = std::max(box->max.w, 0.0f);
}


D3DXVECTOR4 D9LinearFieldOfView(const D3DXMATRIX *pProj)
{
	float a = 1.0f/pProj->_22;
	float s = (pProj->_11/pProj->_22);
	float l = 1.0f/cos(atan(a));
	return D3DXVECTOR4(l, l/s, a, a/s);
}


float D9NearPlane(LPDIRECT3DDEVICE9 pDev, float znear, float zfar, float dmin, const D3DXMATRIX *pProj, bool bReduced)
{
	float b = 1.0f/pProj->_11;
	float a = 1.0f/pProj->_22;
	float q = atan(sqrt(a*a+b*b));

	D3DVIEWPORT9 vp; pDev->GetViewport(&vp);

	dmin = dmin * cos(q);
	
	if (znear>0) dmin = znear;

	float fact = 1500.0f/min(10e3f, zfar);
	float zmax = 500.0f;

	if (bReduced) zmax = 25.0f;
	
	float vmaxi = zmax / (1.0f + fact*fact);
	float value = dmin / (1.0f + fact*fact);
	
	if (value>vmaxi)  value=vmaxi;

	return value;
}


D3DXVECTOR4 D9OffsetRange(double R, double r)
{
	double t  = r*0.5;
	double r2 = r*r;
	double h1 = sqrt(R*R+r2)-R;
	double h2 = sqrt(R*R+t*t)-R;
	double a  =  (h2 - h1*0.0625)/(0.1875*r2);
	double b  =  -(h2 - h1*0.25)/(0.1875*r2*r2);
	return D3DXVECTOR4(float(a), float(b), 1.0f/float(r2), 0.0f);
}


bool D9IsAABBVisible(const D9BBox *in, const D3DXMATRIX *pWV, const D3DXVECTOR4 *F)
{
	
	D3DXVECTOR3 bv;
	
	D3DXVec3TransformCoord(&bv, (LPD3DXVECTOR3)&in->bs, pWV);
	float w = in->bs.w;

	if (bv.z<-w) return false; // Not visible
	float zz = fabs(bv.z);
	float tol = F->z*0.0015f;

	if ((w/zz)<tol) return false; // Not visible
	if (fabs(bv.y)-(F->x*w) > zz*F->z) return false; // Not visible
	if (fabs(bv.x)-(F->y*w) > zz*F->w) return false; // Not visible
	
	D3DXVECTOR4 size = (in->max - in->min)*0.5f;
	D3DXVECTOR3 xv,yv,zv;
	D3DXVec3TransformNormal(&xv, (LPD3DXVECTOR3)&in->a, pWV);
	D3DXVec3TransformNormal(&yv, (LPD3DXVECTOR3)&in->b, pWV);
	D3DXVec3TransformNormal(&zv, (LPD3DXVECTOR3)&in->c, pWV);

	float dx = D3DXVec3Dot(&xv, &bv);
	float dy = D3DXVec3Dot(&yv, &bv);
	float dz = D3DXVec3Dot(&zv, &bv);
	
	float adx = fabs(dx) - size.x;
	float ady = fabs(dy) - size.y;
	float adz = fabs(dz) - size.z;

	float sdx,sdy,sdz;

	if (dx<0) sdx=dx+size.x;
	else	  sdx=dx-size.x; 
	if (dy<0) sdy=dy+size.y;
	else	  sdy=dy-size.y; 
	if (dz<0) sdz=dz+size.z;
	else	  sdz=dz-size.z; 

	float fov = sin(atan(sqrt(F->z*F->z + F->w*F->w)));

	if (fabs(xv.z)>fov && (sdx*xv.z)<0 && adx>0) return false;
	if (fabs(yv.z)>fov && (sdy*yv.z)<0 && ady>0) return false;
	if (fabs(zv.z)>fov && (sdz*zv.z)<0 && adz>0) return false;
	
	return true;
}


bool D9IsBSVisible(const D9BBox *in, const D3DXMATRIX *pWV, const D3DXVECTOR4 *F)
{
	D3DXVECTOR3 bv = D3DXVECTOR3(in->bs.x, in->bs.y, in->bs.z);
	D3DXVec3TransformCoord(&bv, &bv, pWV);
	float r = in->bs.w;

	if (bv.z<-r) return false; // Not visible
	bv.z=fabs(bv.z);

	float tol = F->z*0.0015f;

	if ((r/bv.z)<tol) return false; // Not visible
	if (fabs(bv.y)-(F->x*r) > (bv.z*F->z)) return false; // Not visible
	if (fabs(bv.x)-(F->y*r) > (bv.z*F->w)) return false; // Not visible
	
	return true;
}

int D9ComputeMinMaxDistance(LPDIRECT3DDEVICE9 pDev, const D9BBox *in, const D3DXMATRIX *pWV, const D3DXVECTOR4 *F, float *zmin, float *zmax, float *dst)
{
	
	D3DXVECTOR3 bv = D3DXVECTOR3(in->bs.x, in->bs.y, in->bs.z);
	D3DXVec3TransformCoord(&bv, &bv, pWV);
	float r = in->bs.w;

	if (bv.z<-r) return -1; // Not visible
	float zz = fabs(bv.z);
	float tol = F->z*0.0015f;

	if ((r/zz)<tol) return -2; // Not visible
	if (fabs(bv.y)-(F->x*r) > zz*F->z) return -3; // Not visible
	if (fabs(bv.x)-(F->y*r) > zz*F->w) return -4; // Not visible
	
	
	D3DXVECTOR4 size = (in->max - in->min)*0.5;
	D3DXVECTOR3 xv,yv,zv;
	D3DXVec3TransformNormal(&xv, (LPD3DXVECTOR3)&in->a, pWV);
	D3DXVec3TransformNormal(&yv, (LPD3DXVECTOR3)&in->b, pWV);
	D3DXVec3TransformNormal(&zv, (LPD3DXVECTOR3)&in->c, pWV);

	float dx = D3DXVec3Dot(&xv, &bv);
	float dy = D3DXVec3Dot(&yv, &bv);
	float dz = D3DXVec3Dot(&zv, &bv);
	
	float adx = fabs(dx) - size.x;
	float ady = fabs(dy) - size.y;
	float adz = fabs(dz) - size.z;

	if (adx>0 || ady>0 || adz>0) {

		float sdx,sdy,sdz;

		if (dx<0) sdx=dx+size.x;
		else	  sdx=dx-size.x; 
		if (dy<0) sdy=dy+size.y;
		else	  sdy=dy-size.y; 
		if (dz<0) sdz=dz+size.z;
		else	  sdz=dz-size.z; 

		float fov = sin(atan(sqrt(F->z*F->z + F->w*F->w)));

		if (fabs(xv.z)>fov && (sdx*xv.z)<0 && adx>0) return -5;
		if (fabs(yv.z)>fov && (sdy*yv.z)<0 && ady>0) return -6;
		if (fabs(zv.z)>fov && (sdz*zv.z)<0 && adz>0) return -7;
	}
	
	if (adx<0) adx=0;
	if (ady<0) ady=0;
	if (adz<0) adz=0;

	float d = sqrt(adx*adx + ady*ady + adz*adz);

	if (d<*dst) *dst=d;


	float x = xv.z * size.x;
	float y = yv.z * size.y;
	float z = zv.z * size.z;
	float e = bv.z;
	

	float q[8];

	q[0] = e + (+x+y+z); 
	q[1] = e + (-x+y+z); 
	q[2] = e + (+x-y+z); 
	q[3] = e + (-x-y+z); 
	q[4] = e + (+x+y-z); 
	q[5] = e + (-x+y-z); 
	q[6] = e + (+x-y-z); 
	q[7] = e + (-x-y-z); 

	float mx = q[0];
	float mi = q[0];

	for (int i=1;i<8;i++) {
		float qq = q[i];
		if (qq>mx) mx=qq;
		if (qq<mi) mi=qq;
	}
	
	if (mi<*zmin) *zmin = mi;
	if (mx>*zmax) *zmax = mx;

	return 0;
}


void D9UpdateAABB(D9BBox *box, const D3DXMATRIX *pFirst, const D3DXMATRIX *pSecond)
{
	D3DXVECTOR3 x(1, 0, 0), y(0, 1, 0), z(0, 0, 1);
	D3DXVECTOR3 q(box->min.x, box->min.y, box->min.z), w(box->max.x, box->max.y, box->max.z);
	for (const D3DXMATRIX *M : {pFirst, pSecond}) {
		if (!M) continue;
		D3DXVec3TransformNormal(&x, &x, M);
		D3DXVec3TransformNormal(&y, &y, M);
		D3DXVec3TransformNormal(&z, &z, M);
		D3DXVec3TransformCoord(&q, &q, M);
		D3DXVec3TransformCoord(&w, &w, M);
	}
	D3DXVECTOR3 p = (q + w) * 0.5f;
	D3DXVECTOR3 d = q - w;
	box->bs = D3DXVECTOR4(p.x, p.y, p.z, D3DXVec3Length(&d) * 0.5f);
	box->a = D3DXVECTOR4(x.x, x.y, x.z, 0);
	box->b = D3DXVECTOR4(y.x, y.y, y.z, 0);
	box->c = D3DXVECTOR4(z.x, z.y, z.z, 0);
}


void D9AddAABB(const D9BBox *in, const D3DXMATRIX *pM, D9BBox *out, bool bReset)
{
	D3DXVECTOR3 mi, mx;
	if (bReset) {
		mi = D3DXVECTOR3(1e12f, 1e12f, 1e12f);
		mx = D3DXVECTOR3(-1e12f, -1e12f, -1e12f);
	}
	else {
		mi = D3DXVECTOR3(out->min.x, out->min.y, out->min.z);
		mx = D3DXVECTOR3(out->max.x, out->max.y, out->max.z);
	}
	D3DXVECTOR3 q(in->min.x, in->min.y, in->min.z), w(in->max.x, in->max.y, in->max.z);
	auto grow = [&](const D3DXVECTOR3 &v) {
		mi.x = std::min(mi.x, v.x); mi.y = std::min(mi.y, v.y); mi.z = std::min(mi.z, v.z);
		mx.x = std::max(mx.x, v.x); mx.y = std::max(mx.y, v.y); mx.z = std::max(mx.z, v.z);
	};
	if (pM) {
		for (int k = 0; k < 8; k++) {
			D3DXVECTOR3 c((k & 1) ? w.x : q.x, (k & 2) ? w.y : q.y, (k & 4) ? w.z : q.z), t;
			D3DXVec3TransformCoord(&t, &c, pM);
			grow(t);
		}
	}
	else {
		grow(q);
		grow(w);
	}
	out->min = D3DXVECTOR4(mi.x, mi.y, mi.z, 0);
	out->max = D3DXVECTOR4(mx.x, mx.y, mx.z, 0);
}


void EnvMapDirection(int dir, D3DXVECTOR3 *Dir, D3DXVECTOR3 *Up)
{
    switch (dir) {
        case 0:
            *Dir = D3DXVECTOR3(1.0f, 0.0f, 0.0f);
            *Up  = D3DXVECTOR3(0.0f, 1.0f, 0.0f);
            break;
        case 1:
            *Dir = D3DXVECTOR3(-1.0f, 0.0f, 0.0f);
            *Up  = D3DXVECTOR3( 0.0f, 1.0f, 0.0f);
            break;
        case 2:
            *Dir = D3DXVECTOR3(0.0f, 1.0f,  0.0f);
            *Up  = D3DXVECTOR3(0.0f, 0.0f, -1.0f);
            break;
        case 3:
            *Dir = D3DXVECTOR3(0.0f, -1.0f, 0.0f);
            *Up  = D3DXVECTOR3(0.0f,  0.0f, 1.0f);
            break;
        case 4:
            *Dir = D3DXVECTOR3(0.0f, 0.0f, 1.0f);
            *Up  = D3DXVECTOR3(0.0f, 1.0f, 0.0f);
            break;
        case 5:
            *Dir = D3DXVECTOR3(0.0f, 0.0f, -1.0f);
            *Up  = D3DXVECTOR3(0.0f, 1.0f,  0.0f);
            break;
		default:
			*Dir = D3DXVECTOR3(0.0f, 0.0f, 0.0f);
			*Up  = D3DXVECTOR3(0.0f, 0.0f, 0.0f);
			break;
    }
}

