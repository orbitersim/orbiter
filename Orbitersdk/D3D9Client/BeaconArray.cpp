// =================================================================================================================================
// The MIT Lisence:
//
// Copyright (C) 2012 Jarmo Nikkanen
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

#include "BeaconArray.h"
#include "Log.h"
#include "Scene.h"
#include "D3D9Surface.h"
#include "D3D9Config.h"

using namespace oapi;


// ===========================================================================================
//
BeaconArray::BeaconArray(const BeaconArrayEntry *pEnt, DWORD nEntry) : D3D9Effect()
{
	_TRACE;
	pVB = NULL;
	nVert = nEntry;

	HR(gc->GetDevice()->CreateVertexBuffer(nEntry*sizeof(BAVERTEX), D3DUSAGE_DYNAMIC|D3DUSAGE_POINTS, 0, D3DPOOL_DEFAULT, &pVB, NULL));
	
	BAVERTEX *pVrt = LockVertexBuffer();

	if (pVrt) {
		for (DWORD i=0;i<nEntry;i++) {

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
		
		dev->SetVertexDeclaration(pBAVertexDecl);
		dev->SetStreamSource(0, pVB, 0, sizeof(BAVERTEX));
		dev->DrawPrimitive(D3DPT_POINTLIST, 0, nVert);
	
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