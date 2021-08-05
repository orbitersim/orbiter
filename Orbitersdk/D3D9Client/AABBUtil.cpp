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

#include "AABBUtil.h"
#include "OrbiterAPI.h"
#include "VectorHelpers.h"
#include "Log.h"

#pragma warning(push)
#pragma warning(disable : 4838)
#include <xnamath.h>
#pragma warning(pop)

// =================================================================================================================================
//
bool SolveLUSystem(int n, double *A, double *b, double *x, double *det)
{		
	int e=0, *p = new int[n]; 
	for (int i=0;i<n;i++) p[i] = i;
	for (int k=0;k<n;k++) {
		int r = 0; double d = 0.0; 
		for (int s=k;s<n;s++) if (fabs(A[s*n+k])>d) { d = fabs(A[s*n+k]); r = s; }
		if (d==0.0) { LogErr("Singular Matrix in SolveLUSystem()"); delete []p; return false; }
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
	XMVECTOR q = XMLoadFloat4((const XMFLOAT4*)&box->min);
	XMVECTOR w = XMLoadFloat4((const XMFLOAT4*)&box->max);
	XMVECTOR p = XMLoadFloat3((const XMFLOAT3*)point);
	XMStoreFloat4((XMFLOAT4*)&box->min, XMVectorMin(q,p));
	XMStoreFloat4((XMFLOAT4*)&box->max, XMVectorMax(w,p));	
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

	XMVECTOR x = XMVectorSet(1, 0, 0, 0);
	XMVECTOR y = XMVectorSet(0, 1, 0, 0);
	XMVECTOR z = XMVectorSet(0, 0, 1, 0);
	XMVECTOR q = XMLoadFloat4((const XMFLOAT4*)&box->min);
	XMVECTOR w = XMLoadFloat4((const XMFLOAT4*)&box->max);

	if (pFirst) {
		XMMATRIX MF = XMLoadFloat4x4((const XMFLOAT4X4*)pFirst);
		x = XMVector3TransformNormal(x, MF);
		y = XMVector3TransformNormal(y, MF);
		z = XMVector3TransformNormal(z, MF);
		q = XMVector3TransformCoord(q, MF);
		w = XMVector3TransformCoord(w, MF);
	}

	if (pSecond) {
		XMMATRIX MS = XMLoadFloat4x4((const XMFLOAT4X4*)pSecond);
		x = XMVector3TransformNormal(x, MS);
		y = XMVector3TransformNormal(y, MS);
		z = XMVector3TransformNormal(z, MS);
		q = XMVector3TransformCoord(q, MS);
		w = XMVector3TransformCoord(w, MS);
	}

	XMVECTOR p = XMVectorScale(XMVectorAdd(q,w), 0.5f);

	XMStoreFloat4((XMFLOAT4*)&box->bs, p);		
	XMStoreFloat4((XMFLOAT4*)&box->a,  x);
	XMStoreFloat4((XMFLOAT4*)&box->b,  y);	
	XMStoreFloat4((XMFLOAT4*)&box->c,  z);

	box->bs.w = XMVectorGetX(XMVector3Length(XMVectorSubtract(q,w))) * 0.5f;
}



	

void D9AddAABB(const D9BBox *in, const D3DXMATRIX *pM, D9BBox *out, bool bReset)
{

	XMVECTOR x,mi,mx;

	if (bReset) {
		mi = XMVectorSet( 1e12f,  1e12f,  1e12f, 0); 
		mx = XMVectorSet(-1e12f, -1e12f, -1e12f, 0); 
	}
	else {
		mi = XMLoadFloat4((const XMFLOAT4*)&out->min); 
		mx = XMLoadFloat4((const XMFLOAT4*)&out->max); 
	}
	
	XMVECTOR q = XMLoadFloat4((const XMFLOAT4*)&in->min);
	XMVECTOR w = XMLoadFloat4((const XMFLOAT4*)&in->max);

	q = XMVectorSetW(q, 0);
	w = XMVectorSetW(w, 0);

	if (pM) {

		XMVECTOR L[8];
	
		L[0] = XMVectorSelectControl(0,0,0,0);
		L[1] = XMVectorSelectControl(1,1,1,0);
		L[2] = XMVectorSelectControl(0,0,1,0);
		L[3] = XMVectorSelectControl(0,1,0,0);
		L[4] = XMVectorSelectControl(0,1,1,0);
		L[5] = XMVectorSelectControl(1,0,0,0);
		L[6] = XMVectorSelectControl(1,0,1,0);
		L[7] = XMVectorSelectControl(1,1,0,0);
		

		XMMATRIX M = XMLoadFloat4x4((const XMFLOAT4X4*)pM);

		for (int k=0;k<8;k++) {
			x  = XMVector3TransformCoord(XMVectorSelect(q, w, L[k]), M);
			mi = XMVectorMin(mi,x);
			mx = XMVectorMax(mx,x);
		}
	}
	else {
		mi = XMVectorMin(mi, XMVectorMin(q,w));
		mx = XMVectorMax(mx, XMVectorMax(q,w));	
	}

	XMStoreFloat4((XMFLOAT4*)&out->min, mi);
	XMStoreFloat4((XMFLOAT4*)&out->max, mx);
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


// =======================================================================
// Compute an optical depth of a ray through an atmosphere to infinity
// alt	= starting altitude of the ray above planet surface [m]
// dir  = ray direction [rad] (zero = up)
// R	= Planet Radius [m]
// R1	= Atmosphere Outer Radius [m]
// h0	= Atmospheric Scale Height [m]
// =======================================================================

double ExactOpticalDepth(double alt, double dir, double R, double R1, double h0)
{
	double delta = 0.2 * PI / 180.0;
	double r0 = R + alt;
	double h0ln = h0; // /log(2.0);

	if (dir<(delta*2.0)) return h0ln * exp2(-(r0-R)/h0) / cos(dir);

	dir = PI - dir;

	double m0 = r0 / sin(PI-dir-delta);

	if (m0*sin(dir)>R1) return h0ln * exp2(-(r0-R)/h0) / cos(PI-dir); 

	double opt = 0.0;
	double sind = sin(delta); 

	while (r0<R1) {

		double m = r0 / sin(PI-dir-delta);
		double r1 = m  * sin(dir);
		double d0 = h0ln * exp2((r0-R)/-h0);
		double d1 = h0ln * exp2((r1-R)/-h0);

		if (fabs(r1-r0)<1e-9) {
			dir += delta;
			continue;
		}

		opt += fabs(d0-d1) * (m*sind) / fabs(r1-r0);

		r0 = r1;
		dir += delta;
	}

	return opt;
}










// ===========================================================================
// Fast evaluation of Optical depth based of taylor series
// alt = Ray starting altitude [m]
// cd  = Cosine of the direction of the ray
// h0  = Atmospheric Scale Height [m]
// prm = Taylor co-efficients
// ============================================================================

float FastOpticalDepth(float alt, float cd, double h0, D3DXVECTOR4 *prm)
{
	float cd2 = cd * cd;
	D3DXVECTOR4 q(1.0f, cd, cd2, cd2*cd);
	return float(h0 * exp2(-alt/h0) * pow(D3DXVec4Dot(&q, prm), -float(SctPwr)));
}

float FastOpticalDepthEx(float alt, float cd, float h0, D3DXVECTOR4 *prm)
{
	cd = 1.0f - cd;
	float cd2 = cd * cd;
	D3DXVECTOR4 q(1.0f, cd, cd2, cd2*cd);
	return float(h0 * exp2(-alt/h0) * exp2(D3DXVec4Dot(&q, prm)));
}

double FastOpticalDepth(double alt, double cd, double h0, double *coeff, int m)
{
	cd = 1.0/(cd+0.2);
	double val = 0.0, x = 1.0;
	for (int i=0;i<m;i++) {	val += coeff[i]*x; x*=cd; }
	return h0 * exp2(-alt/h0) * val;
}

// ===========================================================================
// Solve taylor series co-efficients for a fast approximation of optical depth
// R  = Planet Radius [m]
// R1 = Atmosphere Outer Radius [m]
// h0 = Atmospheric Scale Height [m]
// ============================================================================

D3DXVECTOR4 SolveScatterEx(double h0, double R, double R1)
{

	double x[] = {80.0, 82.0, 84.0, 85.0, 86.0, 87.0, 88.0, 89.0, 89.5, 90.0, 90.5, 91.0, 92.0, 93.0, 94.0, 95.0, 96.0, 97.0, 98.0, 99.0};

	VECTOR4 v, q;
	MATRIX4 m;
	memset(&m, 0, sizeof(MATRIX4));
	memset(&q, 0, sizeof(VECTOR4));

	double y[64];
	double ih0 = 1.0/h0;
//	double ipw = -1.0/2.0;

	int ndata = sizeof(x)/sizeof(double);

	for (int i=0;i<ndata;i++) x[i] = x[i]*RAD;
	for (int i=0;i<ndata;i++) y[i] = log(ExactOpticalDepth(0.0, x[i], R, R1, h0) * ih0) / log(2.0);	
	for (int i=0;i<ndata;i++) 
	{
		double c = 1.0 - cos(x[i]);
	
		v = _V(1.0, c, c*c, c*c*c);
		
		m.m11 += v.x*v.x; m.m21 += v.y*v.x;	m.m22 += v.y*v.y; m.m31 += v.z*v.x;
		m.m32 += v.z*v.y; m.m33 += v.z*v.z;	m.m41 += v.w*v.x; m.m42 += v.w*v.y;
		m.m43 += v.w*v.z; m.m44 += v.w*v.w;

		m.m12 = m.m21;	m.m13 = m.m31;	m.m14 = m.m41;	m.m23 = m.m32;	m.m24 = m.m42;	m.m34 = m.m43;

		q.x += (v.x * y[i]);
		q.y += (v.y * y[i]);
		q.z += (v.z * y[i]);
		q.w += (v.w * y[i]);
	}

	double r[4];
	SolveLUSystem(4, (double *)&m, (double *)&q, r, NULL);
	return D3DXVECTOR4(float(r[0]), float(r[1]), float(r[2]), float(r[3]));
}



// ===========================================================================
// Solve taylor series co-efficients for a fast approximation of optical depth
// R  = Planet Radius [m]
// R1 = Atmosphere Outer Radius [m]
// h0 = Atmospheric Scale Height [m]
// ============================================================================

D3DXVECTOR4 SolveScatter(double h0, double R, double R1)
{

	double x[] = {0.0, 10.0, 20.0, 30.0, 40.0, 50.0, 60.0, 70.0, 75.0, 80.0, 82.0, 84.0, 85.0, 86.0, 87.0, 88.0, 89.0, 89.5, 90.0, 90.5};

	VECTOR4 v, q;
	MATRIX4 m;
	memset(&m, 0, sizeof(MATRIX4));
	memset(&q, 0, sizeof(VECTOR4));

	double y[64];
	double ih0 = 1.0/h0;
	double ipw = 1.0/SctPwr;

	int ndata = sizeof(x)/sizeof(double);

	for (int i=0;i<ndata;i++) x[i] = x[i]*RAD;
	for (int i=0;i<ndata;i++) y[i] = pow(1.0 / (ExactOpticalDepth(0.0, x[i], R, R1, h0) * ih0), ipw);	
	for (int i=0;i<ndata;i++) 
	{
		double c = cos(x[i]);
	
		v = _V(1.0, c, c*c, c*c*c);
		
		m.m11 += v.x*v.x; m.m21 += v.y*v.x;	m.m22 += v.y*v.y; m.m31 += v.z*v.x;
		m.m32 += v.z*v.y; m.m33 += v.z*v.z;	m.m41 += v.w*v.x; m.m42 += v.w*v.y;
		m.m43 += v.w*v.z; m.m44 += v.w*v.w;

		m.m12 = m.m21;	m.m13 = m.m31;	m.m14 = m.m41;	m.m23 = m.m32;	m.m24 = m.m42;	m.m34 = m.m43;

		q.x += (v.x * y[i]);
		q.y += (v.y * y[i]);
		q.z += (v.z * y[i]);
		q.w += (v.w * y[i]);
	}

	double r[4];
	SolveLUSystem(4, (double *)&m, (double *)&q, r, NULL);
	D3DXVECTOR4 fct = D3DXVECTOR4(float(r[0]), float(r[1]), float(r[2]), float(r[3]));

	float d1 = FastOpticalDepth(0.0, 0, h0, &fct);
	float d2 = (float)ExactOpticalDepth(0.0, PI05, R, R1, h0);
	float dv = (d1-d2)/d2;
	if (fabs(dv)>0.2) LogErr("Bad Match %g", dv);
	
	return fct;
}


bool SolveXScatter(double h0, double R, double R1, double *r, double angle, int m)
{
	double delta = angle / 50.0;
	double x[50]; x[0]=0.0; for (int i=1;i<50;i++) x[i] = x[i-1] + delta;
	
	int ndata = sizeof(x)/sizeof(double);

	double *v = new double[m];
	double *q = new double[m];
	double *M = new double[m*m];
	double *y = new double[ndata];

	memset(M, 0, m*m*sizeof(double));
	memset(q, 0, m*sizeof(double));

	double ih0 = 1.0/h0;

	for (int i=0;i<ndata;i++) x[i] = x[i]*RAD;
	for (int i=0;i<ndata;i++) y[i] = ExactOpticalDepth(0.0, x[i], R, R1, h0) * ih0;	
	for (int i=0;i<ndata;i++) {

		double c = 1.0/(cos(x[i])+0.2); v[0] = 1.0;
		
		for (int f=1;f<m;f++) v[f] = v[f-1]*c;
		for (int f=0;f<m;f++) for (int g=0;g<m;g++) M[f*m+g] += (v[f]*v[g]);
		for (int f=0;f<m;f++) q[f] += (v[f]*y[i]);
	}

	bool bRet = SolveLUSystem(m, M, q, r);

	// Run some tests

	for (int i=0;i<50;i++) {
		double dif = y[i] - FastOpticalDepth(0.0, cos(x[i]), h0, r, m) * ih0;
		double prs = fabs(dif) / fabs(y[i]);
		if (prs>0.001) LogErr("Difference greater than 0.001 [%g], angle=%g, y=%g", prs, x[i]*DEG, y[i]);
	}

	delete []v; delete []q;
	delete []y;	delete []M;

	return bRet;
}



