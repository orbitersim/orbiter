// ===========================================================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2012 - 2016 Jarmo Nikkanen
// ===========================================================================================

#include "BeaconArray.h"
#include "Log.h"
#include "Scene.h"
#include "D3D9Surface.h"
#include "D3D9Config.h"

using namespace oapi;

// ===========================================================================================
//
BeaconArray::BeaconArray(BeaconArrayEntry *pEnt, DWORD nEntry, OBJHANDLE _hBase) : D3D9Effect()
{
	_TRACE;
	pVB = NULL;
	nVert = nEntry;
	hBase = _hBase;
	hPlanet = NULL;
	
	double mean_rad = 0;	// Planet mean radius
	double base_rad = 0;	// Distace of surface base from a geocentre (i.e. mean_rad + base elevation)
	double inv_brad = 0;	// 1.0/base_rad
	double base_lng = 0;
	double base_lat = 0;
	
	HR(gc->GetDevice()->CreateVertexBuffer(nEntry*sizeof(BAVERTEX), D3DUSAGE_DYNAMIC|D3DUSAGE_POINTS, 0, D3DPOOL_DEFAULT, &pVB, NULL));
	
	BAVERTEX *pVrt = LockVertexBuffer();

	if (hBase) {
		hPlanet = oapiGetBasePlanet(hBase);
		oapiGetBaseEquPos(hBase, &base_lng, &base_lat, &mean_rad);	// mrad = mean radius
		VECTOR3 bp;
		oapiGetRelativePos(hBase, hPlanet, &bp);
		base_rad = length(bp);								// brad = base radius
		inv_brad = 1.0/base_rad;
	}

	if (pVrt) {

		for (DWORD i=0;i<nEntry;i++) {

			if (hBase) {
				
				// Beacon lng, lat
				double lat  = base_lat - atan(pEnt[i].pos.x*inv_brad);
				double lng  = base_lng + atan(pEnt[i].pos.z*inv_brad);

				// Beacon distance from a geo-centre
				double elv  = mean_rad + oapiSurfaceElevation(hPlanet, lng, lat);

				// Beacon distance from center of the base
				double dst  = sqrt(pEnt[i].pos.x*pEnt[i].pos.x + pEnt[i].pos.z*pEnt[i].pos.z);

				// Beacon's y-coordinate in local base reference frame
				double y = sqrt(elv*elv - dst*dst) - base_rad;

	  			pEnt[i].pos.y = 0.5 + y;
			}

			pVrt[i].x  = float(pEnt[i].pos.x);
			pVrt[i].y  = float(pEnt[i].pos.y);
			pVrt[i].z  = float(pEnt[i].pos.z);

			pVrt[i].dx = float(pEnt[i].dir.x);
			pVrt[i].dy = float(pEnt[i].dir.y);
			pVrt[i].dz = float(pEnt[i].dir.z);

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
	if (!pVB) return;
	SAFE_RELEASE(pVB);
	gc->clbkReleaseTexture(pBright);
}


// ===========================================================================================
//
BAVERTEX * BeaconArray::LockVertexBuffer()
{
	if (!pVB) return NULL;
	BAVERTEX *pVert;
	if (pVB->Lock(0, nVert*sizeof(BAVERTEX), (LPVOID*)&pVert, 0)==S_OK) return pVert;
	return NULL;
}


// ===========================================================================================
//
void BeaconArray::UnLockVertexBuffer()
{
	if (!pVB) return;
	HR(pVB->Unlock());
}


// ===========================================================================================
//
void BeaconArray::Render(LPDIRECT3DDEVICE9 dev, const LPD3DXMATRIX pW, float time)
{
	if (!pVB) return;

	__TRY {
		gc->GetStats()->Vertices += nVert;
		gc->GetStats()->Draw++;

		UINT numPasses = 0;
		HR(FX->SetTechnique(eBeaconArrayTech));
		HR(FX->SetMatrix(eW, pW));
		HR(FX->SetTexture(eTex0, SURFACE(pBright)->GetTexture()));
		HR(FX->SetFloat(eTime, time));
		HR(FX->SetFloat(eMix, float(Config->RwyBrightness)));
		
		HR(FX->Begin(&numPasses, D3DXFX_DONOTSAVESTATE));
		HR(FX->BeginPass(0));
		
		dev->SetRenderState(D3DRS_ZENABLE, 0);

		dev->SetVertexDeclaration(pBAVertexDecl);
		dev->SetStreamSource(0, pVB, 0, sizeof(BAVERTEX));
		dev->DrawPrimitive(D3DPT_POINTLIST, 0, nVert);
	
		dev->SetRenderState(D3DRS_ZENABLE, 1);

		HR(FX->EndPass());
		HR(FX->End());	

		dev->SetRenderState(D3DRS_POINTSPRITEENABLE, 0);
	}
	__EXCEPT(ExcHandler(GetExceptionInformation()))
	{
		LogErr("Exception in BeaconArray::Render()");
		gc->EmergencyShutdown();
		FatalAppExitA(0,"Critical error has occured. See Orbiter.log for details");
	}
}