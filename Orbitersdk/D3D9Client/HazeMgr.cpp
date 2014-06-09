// ============================================================================
// HazeMgr.cpp
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2007 Martin Schweiger
//				 2011-2014 Jarmo Nikkanen
// ============================================================================

// ============================================================================
// class HazeManager (implementation)
//
// Planetary atmospheric haze rendering
// Implemented as transparent overlay on planetary disc
// ============================================================================

#include "HazeMgr.h"
#include "VPlanet.h"
#include "Texture.h"
#include "D3D9Surface.h"
#include "D3D9Util.h"
#include "D3D9Effect.h"

using namespace oapi;

HazeManager::HazeManager (const D3D9Client *gclient, const vPlanet *vplanet) : D3D9Effect()
{
	gc = gclient;
	vp = vplanet;
	obj = vp->Object();
	rad = oapiGetSize (obj);
	const ATMCONST *atmc = oapiGetPlanetAtmConstants (obj);
	if (atmc) {
		basecol = *(VECTOR3*)oapiGetObjectParam (obj, OBJPRM_PLANET_HAZECOLOUR);
		hralt = (float)(atmc->horizonalt / rad);
		dens0 = (float)(min (1.0, atmc->horizonalt/64e3 * *(double*)oapiGetObjectParam(obj, OBJPRM_PLANET_HAZEDENSITY)));
	} else {
		basecol = _V(1,1,1);
		hralt = 0.01f;
		dens0 = 1.0f;
	}
	if (*(bool*)oapiGetObjectParam (obj, OBJPRM_PLANET_HASCLOUDS)) {
		hshift = *(double*)oapiGetObjectParam (obj, OBJPRM_PLANET_HAZESHIFT);
		cloudalt = *(double*)oapiGetObjectParam (obj, OBJPRM_PLANET_CLOUDALT);
	} else
		hshift = 0;
	hscale = (float)(1.0 - *(double*)oapiGetObjectParam (obj, OBJPRM_PLANET_HAZEEXTENT));
}

// -----------------------------------------------------------------------

void HazeManager::GlobalInit(D3D9Client *gclient)
{
	int i;
	for (i = 0; i < HORIZON_NSEG; i++) Idx[i*2] = i*2+1, Idx[i*2+1] = i*2;
	Idx[i*2] = 1, Idx[i*2+1] = 0;

	for (i = 0; i < HORIZON_NSEG; i++) {
		Vtx[i*2].tu = Vtx[i*2+1].tu = (float)(i%2);
		Vtx[i*2].tv = 1.0f;
		Vtx[i*2+1].tv = 0.0f;
		double phi = (double)i/(double)HORIZON_NSEG * PI*2.0;
		CosP[i] = (float)cos(phi), SinP[i] = (float)sin(phi);
	}
	gclient->GetTexMgr()->LoadTexture("Horizon.dds", &horizon, 0);
}

// -----------------------------------------------------------------------

void HazeManager::GlobalExit()
{
	SAFE_DELETE(horizon);
}

// -----------------------------------------------------------------------

void HazeManager::Render(LPDIRECT3DDEVICE9 pDev, D3DXMATRIX &wmat, bool dual)
{
	D3DXMATRIX imat, transm;

	VECTOR3 psun;
	int i, j;
	double phi, csun, alpha, colofs;
	float cosp, sinp, cost, sint, h1, h2, r1, r2, intr, intg, intb;

	D3DMAT_MatrixInvert (&imat, &wmat);
	VECTOR3 rpos = {imat._41, imat._42, imat._43};   // camera in local coords (planet radius = 1)
	double cdist = length (rpos);

	alpha = dens0 * min (1.0, (cdist-1.0)*200.0);
	if (!dual) alpha = 1.0-alpha;
	if (alpha <= 0.0) return;  // nothing to do

	// Problem: the top part of horizon haze is rendered twice
	if (dual && cdist<1.001) return;	// Enabled 04.06.2011

	VECTOR3 cpos = {0,cdist,0};
	double id = 1.0 / max (cdist, 1.001);
	double visrad = acos (id);             // aperture of visibility sector
	double sinv = sin(visrad);

	h1 = (float)id;
	h2 = h1 + (float)(hralt*id);
	r1 = (float)sinv, r2 = (1.0f+hralt)*r1;

	if (hshift) {
		if (cdist-1.0 > cloudalt/rad) {
			float dr = (float)(hshift*sinv);
			float dh = (float)(hshift*id);
			h1 += dh, h2 += dh;
			r1 += dr, r2 += dr;
		}
	}

	float dens = (float)max (1.0, 1.4 - 0.3/hralt*(cdist-1.0)); // saturate haze colour at low altitudes
	if (dual) dens *= (float)(0.5 + 0.5/cdist);                 // scale down intensity at large distances

	normalise (rpos);
	cost = (float)rpos.y, sint = (float)sqrt (1.0-cost*cost);
	phi = atan2 (rpos.z, rpos.x), cosp = (float)cos(phi), sinp = (float)sin(phi);

	D3DXMATRIX rmat = D3DXMATRIX(cost*cosp, -sint, cost*sinp, 0,
		              sint*cosp,  cost, sint*sinp, 0,
					  -sinp,      0,    cosp,      0,
					  0,          0,    0,         1);

	D3DXMatrixMultiply(&transm, &rmat, &wmat);

	MATRIX3 rrmat = {cost*cosp, -sint, cost*sinp,
		             sint*cosp,  cost, sint*sinp,
					 -sinp,      0,    cosp     };
	MATRIX3 grot;
	VECTOR3 gpos;
	oapiGetRotationMatrix (obj, &grot);
	oapiGetGlobalPos (obj, &gpos);
	psun = tmul (grot, -gpos); // sun in planet coords
	psun = mul (rrmat, psun);  // sun in camera-relative horizon coords
	VECTOR3 cs = psun-cpos; normalise(cs); // camera->sun
	normalise (psun);
	// float psunx = (float)psun.x, psuny = (float)psun.y, psunz = (float)psun.z;

	colofs = (dual ? 0.4 : 0.3);

	for (i = j = 0; i < HORIZON_NSEG; i++) {
		VECTOR3 hp = {Vtx[j].x = r1*CosP[i], Vtx[j].y = h1, Vtx[j].z = r1*SinP[i]};
		csun = dotp (hp, psun);
		VECTOR3 cp = hp-cpos; normalise(cp);
		double colsh = 0.5*(dotp (cp,cs) + 1.0);

		// compose a colourful sunset
		double maxred   = colofs-0.18*colsh,  minred   = maxred-0.4;
		double maxgreen = colofs-0.1*colsh,  mingreen = maxgreen-0.4;
		double maxblue  = colofs/*+0.0*colsh*/,  minblue  = maxblue-0.4;
		if      (csun > maxred) intr = 1.0f;
		else if (csun < minred) intr = 0.0f;
		else                    intr = (float)((csun-minred)*2.5);
		if      (csun > maxgreen) intg = 1.0f;
		else if (csun < mingreen) intg = 0.0f;
		else                      intg = (float)((csun-mingreen)*2.5);
		if      (csun > maxblue) intb = 1.0f;
		else if (csun < minblue) intb = 0.0f;
		else                     intb = (float)((csun-minblue)*2.5);

		D3DXCOLOR col = D3DXCOLOR(intr*min(1.0f,dens*(float)basecol.x), intg*min(1.0f,dens*(float)basecol.y), intb*min(1.0f,dens*(float)basecol.z), (float)alpha);
		//D3DXCOLOR col = D3DXCOLOR(intr*min(1.0f,dens*(float)basecol.x), intg*min(1.0f,dens*(float)basecol.y), intb*min(1.0f,dens*(float)basecol.z), 1.0f);

		Vtx[j].dcol = col;
		j++;
		Vtx[j].x = r2*CosP[i];
		Vtx[j].y = h2;
		Vtx[j].z = r2*SinP[i];
		Vtx[j].dcol = col;
		j++;
	}
	
	HR(FX->SetTechnique(eHazeTech));
	HR(FX->SetMatrix(eW, &transm));
	HR(FX->SetTexture(eTex0, horizon->GetTexture()));	

	HR(pDev->SetVertexDeclaration(pHazeVertexDecl));

	UINT numPasses = 0;
	HR(FX->Begin(&numPasses, D3DXFX_DONOTSAVESTATE));
	HR(FX->BeginPass(0));
	
	pDev->DrawIndexedPrimitiveUP(D3DPT_TRIANGLESTRIP, 0, 2*HORIZON_NSEG, 2*HORIZON_NSEG, Idx, D3DFMT_INDEX16, Vtx, sizeof(HVERTEX));
	
	if (dual) {

		h2 = h1;
		r2 = hscale * r1*r1; 

		for (i = j = 0; i < HORIZON_NSEG; i++) {
			j++;
			Vtx[j].x = r2*CosP[i];
			Vtx[j].y = h2;
			Vtx[j].z = r2*SinP[i];
			j++;
		}
		pDev->SetRenderState(D3DRS_CULLMODE, D3DCULL_CW);
		pDev->DrawIndexedPrimitiveUP(D3DPT_TRIANGLESTRIP,0, 2*HORIZON_NSEG, 2*HORIZON_NSEG, Idx, D3DFMT_INDEX16, Vtx, sizeof(HVERTEX));
		pDev->SetRenderState(D3DRS_CULLMODE, D3DCULL_CCW);
	}

	HR(FX->EndPass());
	HR(FX->End());	
}

// ============================================================================
// static member initialisation

WORD     HazeManager::Idx[HORIZON_NSEG*2+2];
struct   HazeManager::HVERTEX HazeManager::Vtx[HORIZON_NSEG*2];
DWORD    HazeManager::nIdx = HORIZON_NSEG*2+2;
float HazeManager::CosP[HORIZON_NSEG];
float HazeManager::SinP[HORIZON_NSEG];

LPD3D9CLIENTSURFACE HazeManager::horizon = 0;












// ============================================================================
// class HazeManager2 (implementation)
//
// Planetary atmospheric haze rendering with scattering technique
// ============================================================================

HazeManager2::HazeManager2(const D3D9Client *gclient, const vPlanet *vplanet) 
: PlanetRenderer()
{
	vp = vplanet;
	obj = vp->Object();
	rad = oapiGetSize(obj);
	const ATMCONST *atmc = oapiGetPlanetAtmConstants (obj);
	if (atmc)	hralt = atmc->horizonalt;
	else		hralt = 0.01f;
}

// -----------------------------------------------------------------------

void HazeManager2::GlobalInit(D3D9Client *gclient)
{
	CreateRingBuffers();
	CreateSkydomeBuffers();
}

// -----------------------------------------------------------------------

void HazeManager2::GlobalExit()
{
	SAFE_RELEASE(pSkyVB);
	SAFE_RELEASE(pRingVB);
}

// -----------------------------------------------------------------------

void HazeManager2::Render(D3DXMATRIX &wmat, float horizontal_aperture_deg)
{
	VECTOR3 cdir = vp->GetScene()->GetCameraGDir();
	double calt = vp->CamDist() - rad;	// Camera altitude	

	if (calt>hralt)	RenderRing(vp->PosFromCamera(), cdir, rad, hralt);
	else			RenderSky(vp->PosFromCamera(), cdir, rad, horizontal_aperture_deg);
}

// -----------------------------------------------------------------------

void HazeManager2::RenderSky(VECTOR3 cpos, VECTOR3 cdir, double rad, double apr)
{
	cpos = -cpos;

	double cr = length(cpos);	
	double hd = sqrt(cr*cr - rad*rad);
	double al = asin(rad/cr);
	double mx = hralt * 2.5;
	double ha = hd * 0.25;

	double r1 =  hd * sin(al);
	double h1 = -hd * cos(al);
	
	double r2 = r1 + ha * cos(al);
	double h2 = h1 + ha * sin(al); 

	
	VECTOR3 ur = unit(cpos);
	VECTOR3 ux = unit(crossp(cdir, ur));
	VECTOR3 uy = unit(crossp(ur, ux));

	D3DXMATRIX mWL, mWR, mL, mR;
	D3DMAT_Identity(&mWL);
	D3DMAT_FromAxisT(&mWL, &_D3DXVECTOR3(ux), &_D3DXVECTOR3(ur), &_D3DXVECTOR3(uy));
	mWR = mWL;

	HR(Shader()->SetVector(svTexOff, &D3DXVECTOR4(float(r1), float(r2), float(h1), float(h2))));
	HR(Shader()->SetFloat(sfAlpha, float(ha)));

	RenderSkySegment(mWL);

	double a = 10.0*PI/180.0;

	int n = (int(ceil(apr/10.0))+6)/2;

	D3DXMatrixRotationAxis(&mL, &_D3DXVECTOR3(ur), float(-a));
	D3DXMatrixRotationAxis(&mR, &_D3DXVECTOR3(ur), float(+a));

	for (int i=0;i<n;i++) {
		D3DXMatrixMultiply(&mWL, &mWL, &mL);
		D3DXMatrixMultiply(&mWR, &mWR, &mR);
		RenderSkySegment(mWL);
		RenderSkySegment(mWR);
	}

	HR(Shader()->SetVector(svTexOff, &D3DXVECTOR4(1, 0, 1, 0)));
}

// -----------------------------------------------------------------------

void HazeManager2::RenderSkySegment(D3DXMATRIX &wmat)
{
	HR(Shader()->SetTechnique(eHorizonTech));
	HR(Shader()->SetMatrix(smWorld, &wmat));
	
	UINT prims = 30 * 60 * 2 - 2;
	UINT numPasses = 0;
	HR(Shader()->Begin(&numPasses, D3DXFX_DONOTSAVESTATE));
	HR(Shader()->BeginPass(0));
	Dev()->SetVertexDeclaration(pPositionDecl);
	Dev()->SetStreamSource(0, pSkyVB, 0, sizeof(D3DXVECTOR3));
	Dev()->SetRenderState(D3DRS_CULLMODE, D3DCULL_NONE);
	Dev()->DrawPrimitive(D3DPT_TRIANGLESTRIP, 0, prims);	
	Dev()->SetRenderState(D3DRS_CULLMODE, D3DCULL_CCW);
	HR(Shader()->EndPass());
	HR(Shader()->End());	
}




// -----------------------------------------------------------------------

void HazeManager2::RenderRing(VECTOR3 cpos, VECTOR3 cdir, double rad, double hralt)
{
	cpos = -cpos;
	double cr = length(cpos);	
	double hd = sqrt(cr*cr - rad*rad);
	double al = asin(rad/cr);
	double mx = hralt * 3.0;

	double r1 =  hd * sin(al);
	double h1 = -hd * cos(al);
	double qw =  hralt + (cr-rad);
	if (qw>mx) qw=mx;
	
	double r2 = r1 + qw * cos(al);
	double h2 = h1 + qw * sin(al); 

	
	VECTOR3 ur = unit(cpos);
	VECTOR3 ux = unit(crossp(cdir, ur));
	VECTOR3 uy = unit(crossp(ur, ux));

	D3DXMATRIX mW;
	D3DMAT_Identity(&mW);
	D3DMAT_FromAxisT(&mW, &_D3DXVECTOR3(ux), &_D3DXVECTOR3(ur), &_D3DXVECTOR3(uy));

	HR(Shader()->SetTechnique(eRingTech));
	HR(Shader()->SetMatrix(smWorld, &mW));
	HR(Shader()->SetVector(svTexOff, &D3DXVECTOR4(float(r1), float(r2), float(h1), float(h2))));
	HR(Shader()->SetFloat(sfAlpha, float(qw)));
	
	UINT numPasses = 0;
	UINT nPrims = HORIZON2_NSEG * HORIZON2_NRING * 2 - 2;

	HR(Shader()->Begin(&numPasses, D3DXFX_DONOTSAVESTATE));
	HR(Shader()->BeginPass(0));
	
	Dev()->SetRenderState(D3DRS_CULLMODE, D3DCULL_NONE);
	Dev()->SetVertexDeclaration(pPositionDecl);
	Dev()->SetStreamSource(0, pRingVB, 0, sizeof(D3DXVECTOR3));
	Dev()->DrawPrimitive(D3DPT_TRIANGLESTRIP, 0, nPrims);
	Dev()->SetRenderState(D3DRS_CULLMODE, D3DCULL_CCW);

	HR(Shader()->EndPass());
	HR(Shader()->End());	
}


// -----------------------------------------------------------------------

void HazeManager2::CreateRingBuffers()
{
	int v = 0;
	int nvrt = HORIZON2_NSEG * 2 * HORIZON2_NRING + 2;

	D3DXVECTOR3 *pVrt = new D3DXVECTOR3[nvrt];
	D3DXVECTOR3 *pBuf = NULL;

	float d = 1.0f/float(HORIZON2_NRING);
	double phi = 0.0;
	double dphi = PI2/double(HORIZON2_NSEG-1);
	float x = float(cos(phi));
	float z = float(sin(phi));
	float y = 0.0f;

	for (int k=0;k<HORIZON2_NRING;k++) {
		for (int i=0;i<HORIZON2_NSEG;i++) {
			pVrt[v++] = D3DXVECTOR3(x, y, z);
			phi+=dphi;
			x = float(cos(phi));
			z = float(sin(phi));
			pVrt[v++] = D3DXVECTOR3(x, y+d, z);
		}
		y+=d;
	}

	HR(Dev()->CreateVertexBuffer(v*sizeof(D3DXVECTOR3), 0, 0, D3DPOOL_DEFAULT, &pRingVB, NULL));

	if (pRingVB->Lock(0, 0, (void **)&pBuf,0)==S_OK) {
		memcpy2(pBuf, pVrt, v*sizeof(D3DXVECTOR3));
		pRingVB->Unlock();
	}

	delete []pVrt;
}

// -----------------------------------------------------------------------
void HazeManager2::CreateSkydomeBuffers()
{

	D3DXVECTOR3 *pVrt = new D3DXVECTOR3[HORIZON2_XSEG*HORIZON2_YSEG*2+2];
	D3DXVECTOR3 *pBuf = NULL;

	int k = 0;
	int xseg = HORIZON2_XSEG;
	int yseg = HORIZON2_YSEG;

	double sa, ca;

	double da = (10.0*PI/180.0)/double(xseg-1);
	double db = 1.0/double(yseg-1);
	
	double a = -da*double(xseg/2);
	double b = 0.0;
	
	for (int s=0;s<yseg;s++) {
		for (int i=0;i<xseg;i++) {
			sa = sin(a); ca = cos(a);
			pVrt[k++]=D3DXVECTOR3(float(sa), float(b),   float(ca));
			pVrt[k++]=D3DXVECTOR3(float(sa), float(b+db), float(ca));
			a += da;
		}
		da = -da;
		a += da;
		b += db;
	}

	HR(Dev()->CreateVertexBuffer(k*sizeof(D3DXVECTOR3), 0, 0, D3DPOOL_DEFAULT, &pSkyVB, NULL));

	if (pSkyVB->Lock(0, 0, (void **)&pBuf,0)==S_OK) {
		memcpy2(pBuf, pVrt, k*sizeof(D3DXVECTOR3));
		pSkyVB->Unlock();
	}

	delete []pVrt;

}

// -----------------------------------------------------------------------

LPDIRECT3DVERTEXBUFFER9 HazeManager2::pSkyVB = NULL;
LPDIRECT3DVERTEXBUFFER9 HazeManager2::pRingVB = NULL;

