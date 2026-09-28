// ===========================================================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2012-2026 Jarmo Nikkanen
// ===========================================================================================

#include "BeaconArray.h"
#include "Log.h"
#include "Scene.h"
#include "D3D9Surface.h"
#include "D3D9Config.h"
#include "VPlanet.h"
#include "VBase.h"

using namespace oapi;

// ===========================================================================================
//
BeaconArray::BeaconArray(BeaconArrayEntry *pEnt, DWORD nEntry, vBase *_vB)
	: D3D9Effect()
	, nVert(nEntry)
	, vB(_vB)
	, pVB(NULL)
	, pVBSys(NULL)
	, hBase(NULL)
	, bidx(0)
	, base_elev()
{
	_TRACE;

	if (vB) hBase = vB->GetObject();

	pBeaconPos = new BeaconPos[nEntry];

	pVB = new VkBuf(gc->GetDevice(), nEntry*sizeof(BAVERTEX), VK_BUFFER_USAGE_VERTEX_BUFFER_BIT, true); // D3DUSAGE_DYNAMIC|D3DUSAGE_POINTS, D3DPOOL_DEFAULT
	pVBSys = new BAVERTEX[nEntry]; // not upstream: Lock/Unlock edit this copy

	BAVERTEX *pVrt = LockVertexBuffer();

	if (pVrt) {

		for (DWORD i=0;i<nEntry;i++) {

			if (vB) pBeaconPos[i].vLoc = unit(vB->ToLocal(pEnt[i].pos, &pBeaconPos[i].lng, &pBeaconPos[i].lat));

			pVrt[i].pos = D3DXVEC(pEnt[i].pos);
			pVrt[i].dir = D3DXVEC(pEnt[i].dir);

			pVrt[i].color = pEnt[i].color;
			pVrt[i].size  = pEnt[i].size;
			pVrt[i].angle = cos(pEnt[i].angle * 0.0174532925f * 0.5f);

			pVrt[i].on  = pEnt[i].lon;
			pVrt[i].off = pEnt[i].loff;

			pVrt[i].bright  = pEnt[i].bright;
			pVrt[i].falloff = pEnt[i].fall;
		}
		UnLockVertexBuffer();
	}
	else {
		LogErr("Failed to lock a vertex buffer in BeaconArray()");
	}

	pBright = gc->clbkLoadTexture("D3D9RwyLight.dds");

	if (pBright==NULL) LogErr("D3D9RwyLight.dds is Missing");
}


// ===========================================================================================
//
BeaconArray::~BeaconArray()
{
	SAFE_DELETEA(pBeaconPos);
	SAFE_DELETE(pVB);
	SAFE_DELETEA(pVBSys);
	gc->clbkReleaseTexture(pBright);
}


// ===========================================================================================
//
void BeaconArray::Update(DWORD nCount, vPlanet *vP)
{
	if (nCount>nVert) nCount = nVert;
	double meanelev = vP->GetSize();
	BAVERTEX *pVrt = LockVertexBuffer();
	if (!pVrt) return;
	for (DWORD i=0;i<nCount;i++) {
		double elv = 0;
		if (vP->GetElevation(pBeaconPos[bidx].lng, pBeaconPos[bidx].lat, &elv)==1) {
			VECTOR3 vLoc = pBeaconPos[bidx].vLoc * (meanelev+elv);
			vB->FromLocal(vLoc, &pVrt[bidx].pos);
		}
		bidx++;	if (bidx>=nVert) bidx=0;
	}
	UnLockVertexBuffer();
}


// ===========================================================================================
//
BAVERTEX * BeaconArray::LockVertexBuffer()
{
	if (!pVB) return NULL;
	return pVBSys; // Lock: the frame in flight may still read pVB
}


// ===========================================================================================
//
void BeaconArray::UnLockVertexBuffer()
{
	if (!pVB) return;
	// Unlock: a new buffer (the D3D9 driver renames a locked dynamic buffer), the old one goes when the GPU is done
	SAFE_DELETE(pVB);
	pVB = new VkBuf(gc->GetDevice(), nVert*sizeof(BAVERTEX), VK_BUFFER_USAGE_VERTEX_BUFFER_BIT, true);
	pVB->Upload(pVBSys, nVert*sizeof(BAVERTEX));
}


// ===========================================================================================
//
void BeaconArray::Render(VkDev *dev, const LPD3DXMATRIX pW, float time)
{
	if (!pVB) return;

	UINT numPasses = 0;
	HR(FX->SetTechnique(eBeaconArrayTech));
	HR(FX->SetMatrix(eW, pW));
	HR(FX->SetTexture(eTex0, SURFACE(pBright)->GetTexture()));
	HR(FX->SetFloat(eTime, time));
	HR(FX->SetFloat(eMix, float(Config->RwyBrightness)));

	HR(FX->Begin(&numPasses, VKFX_DONOTSAVESTATE));
	HR(FX->BeginPass(0));

	//dev->SetRenderState(D3DRS_ZENABLE, 0);

	dev->SetVertexDecl(pBAVertexDecl);
	dev->SetStreamSource(0, pVB, 0, sizeof(BAVERTEX));
	dev->DrawPrimitive(VK_PRIMITIVE_TOPOLOGY_POINT_LIST, 0, nVert);

	dev->SetDepthTest(true);

	HR(FX->EndPass());
	HR(FX->End());

	// D3DRS_POINTSPRITEENABLE off left out: Vulkan point lists always rasterize as sprites (gl_PointCoord)
}

