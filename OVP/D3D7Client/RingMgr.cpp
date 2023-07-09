// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//   ORBITER VISUALISATION PROJECT (OVP)
//   D3D7 Client module
// ==============================================================

// ==============================================================
// RingMgr.cpp
// class RingManager (implementation)
// ==============================================================

#define D3D_OVERLOADS
#include "RingMgr.h"
#include "Texture.h"
#include <algorithm>
using std::min;

using namespace oapi;

RingManager::RingManager (const vPlanet *vplanet, double inner_rad, double outer_rad)
{
	vp = vplanet;
	irad = inner_rad;
	orad = outer_rad;
	rres = (DWORD)-1;
	tres = 0;
	ntex = 0;
	for (DWORD i = 0; i < MAXRINGRES; i++) {
		mesh[i] = 0;
		tex[i] = 0;
	}
}

RingManager::~RingManager ()
{
	DWORD i;
	for (i = 0; i < 3; i++)
		if (mesh[i]) delete mesh[i];
	for (i = 0; i < ntex; i++)
		tex[i]->Release();
}

void RingManager::GlobalInit (const D3D7Client *gclient)
{
	gc = gclient;
	vbMemCaps = (gc->GetFramework()->IsTLDevice() ? 0 : D3DVBCAPS_SYSTEMMEMORY);
}

void RingManager::SetMeshRes (DWORD res)
{
	if (res != rres) {
		rres = res;
		if (!mesh[res])
			mesh[res] = CreateRing (irad, orad, 8+res*4);
		if (!ntex)
			ntex = LoadTextures();
		tres = min (rres, ntex-1);
	}
}

DWORD RingManager::LoadTextures ()
{
	char fname[256];
	oapiGetObjectName (vp->Object(), fname, 256);
	strcat (fname, "_ring.tex");
	return gc->GetTexMgr()->LoadTextures (fname, tex, 0, MAXRINGRES);
}

bool RingManager::Render (LPDIRECT3DDEVICE7 dev, D3DMATRIX &mWorld)
{
	DWORD ablend;
	MATRIX3 grot;
	static D3DMATRIX imat, *ringmat;
	oapiGetRotationMatrix (vp->Object(), &grot);
	VECTOR3 ppos = tmul(grot, -vp->cpos);
	if (ppos.y >= 0) { // camera above equator
		ringmat = &mWorld;
	} else {           // flip rings
		int i;
		for (i = 0; i < 4; i++) imat.m[0][i] =  mWorld.m[0][i];
		for (i = 0; i < 4; i++) imat.m[1][i] = -mWorld.m[1][i];
		for (i = 0; i < 4; i++) imat.m[2][i] = -mWorld.m[2][i];
		for (i = 0; i < 4; i++) imat.m[3][i] =  mWorld.m[3][i];
		ringmat = &imat;
	}

	dev->SetTransform (D3DTRANSFORMSTATE_WORLD, ringmat);
	dev->SetTexture (0, tex[tres]);
	dev->GetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, &ablend);
	if (!ablend)
		dev->SetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, TRUE);

	mesh[rres]->RenderGroup (dev, mesh[rres]->GetGroup(0));

	if (!ablend)
		dev->SetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, FALSE);

	return true;
}

// =======================================================================
// CreateRing
// Creates mesh for rendering planetary ring system. Creates a ring
// with nsect quadrilaterals. Smoothing the corners of the mesh is
// left to texture transparency. Nsect should be an even number.
// Disc is in xz-plane centered at origin facing up. Size is such that
// a ring of inner radius irad (>=1) and outer radius orad (>irad)
// can be rendered on it.

D3D7Mesh *RingManager::CreateRing (double irad, double orad, int nsect)
{
	int i, j;

	D3D7Mesh::GROUPREC *grp = new D3D7Mesh::GROUPREC;
	grp->nVtx = 2*nsect;
	grp->nIdx = 6*nsect;
	grp->Idx = new WORD[grp->nIdx];

	LPDIRECT3D7 d3d = gc->GetDirect3D7();
	LPDIRECT3DDEVICE7 dev = gc->GetDevice();
	D3DVERTEX *Vtx;
	D3DVERTEXBUFFERDESC vbd = 
	{ sizeof(D3DVERTEXBUFFERDESC), vbMemCaps | D3DVBCAPS_WRITEONLY, D3DFVF_VERTEX, grp->nVtx };
	d3d->CreateVertexBuffer (&vbd, &grp->VtxBuf, 0);
	grp->VtxBuf->Lock (DDLOCK_WAIT | DDLOCK_WRITEONLY | DDLOCK_DISCARDCONTENTS, (LPVOID*)&Vtx, NULL);
	WORD *Idx = grp->Idx;

	double alpha = PI/(double)nsect;
	float nrad = (float)(orad/cos(alpha)); // distance for outer nodes
	float ir = (float)irad;
	float fo = (float)(0.5*(1.0-orad/nrad));
	float fi = (float)(0.5*(1.0-irad/nrad));

	for (i = j = 0; i < nsect; i++) {
		double phi = i*2.0*alpha;
		float cosp = (float)cos(phi), sinp = (float)sin(phi);
		Vtx[i*2].x = nrad*cosp;  Vtx[i*2+1].x = ir*cosp;
		Vtx[i*2].z = nrad*sinp;  Vtx[i*2+1].z = ir*sinp;
		Vtx[i*2].y = Vtx[i*2+1].y = 0.0;
		Vtx[i*2].nx = Vtx[i*2+1].nx = Vtx[i*2].nz = Vtx[i*2+1].nz = 0.0;
		Vtx[i*2].ny = Vtx[i*2+1].ny = 1.0;
		if (!(i&1)) Vtx[i*2].tu = fo,  Vtx[i*2+1].tu = fi;  //fac;
		else        Vtx[i*2].tu = 1.0f-fo,  Vtx[i*2+1].tu = 1.0f-fi; //1.0f-fac;
		Vtx[i*2].tv = 0.0f, Vtx[i*2+1].tv = 1.0f;

		Idx[j++] = i*2;
		Idx[j++] = i*2+1;
		Idx[j++] = (i*2+2) % (2*nsect);
		Idx[j++] = (i*2+3) % (2*nsect);
		Idx[j++] = (i*2+2) % (2*nsect);
		Idx[j++] = i*2+1;
	}
	grp->VtxBuf->Unlock();
	grp->VtxBuf->Optimize (dev, 0);
	return new D3D7Mesh (gc, grp, false);
}

const oapi::D3D7Client *RingManager::gc = 0;
DWORD RingManager::vbMemCaps = 0;