// ==============================================================
// CelSphere.cpp
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2006-2016 Martin Schweiger
//				 2012-2016 Jarmo Nikkanen
// ==============================================================

// ==============================================================
// Class CelestialSphere (implementation)
//
// This class is responsible for rendering the celestial sphere
// background (stars, constellations, grids, labels, etc.)
// ==============================================================

#include "CelSphere.h"
#include "D3D9Config.h"
#include "AABBUtil.h"


#define NSEG 64 // number of segments in celestial grid lines

using namespace oapi;

// ==============================================================

CelestialSphere::CelestialSphere(D3D9Client *_gc)
	: gc(_gc)
	, pDevice(_gc->GetDevice())
	, maxNumVertices(_gc->GetHardwareCaps()->MaxPrimitiveCount)
{
	sphere_r = 1e6f; // the actual render distance for the celestial sphere
	                 // is irrelevant, since it is rendered without z-buffer,
	                 // but it must be within the frustum limits - check this
	                 // in case the near and far planes are dynamically changed!
	LoadStars ();
	LoadConstellationLines ();
	AllocGrids ();
}

// ==============================================================

CelestialSphere::~CelestialSphere()
{
	if (nsbuf) {
		for (DWORD i = 0; i < nsbuf; ++i) {
			svtx[i]->Release();
		}
		delete []svtx;
		svtx = NULL;
	}
	cvtx->Release();
	grdlng->Release();
	grdlat->Release();
}

// ==============================================================

void CelestialSphere::LoadStars ()
{
	nsvtx = nsbuf = 0;

	StarRenderPrm *prm = (StarRenderPrm*)gc->GetConfigParam (CFGPRM_STARRENDERPRM);

	// Read the star database
	const std::vector<oapi::GraphicsClient::StarRec> starList = gc->LoadStarData(prm->mag_lo);
	if (!starList.size()) return;

	// convert to render parameters
	const std::vector<oapi::GraphicsClient::StarRenderRec> renderList = gc->StarData2RenderData(starList, *prm);
	if (!renderList.size()) return;

	DWORD i, j, nv, k, idx = 0;
	int lvl, plvl = 256;

	// convert star database to vertex buffers
	nsvtx = starList.size();
	nsbuf = (nsvtx + maxNumVertices - 1) / maxNumVertices; // number of buffers required
	svtx = new LPDIRECT3DVERTEXBUFFER9[nsbuf];
	for (i = idx = 0; i < nsbuf; i++) {
		nv = min(maxNumVertices, nsvtx - i * maxNumVertices);
		pDevice->CreateVertexBuffer(UINT(nv*sizeof(VERTEX_XYZC)), D3DUSAGE_WRITEONLY, 0, D3DPOOL_DEFAULT, &svtx[i], NULL);
		VERTEX_XYZC *vbuf;
		svtx[i]->Lock(0, 0, (LPVOID*)&vbuf, 0);
		for (j = 0; j < nv; j++) {
			const oapi::GraphicsClient::StarRenderRec& rec = renderList[idx];
			VERTEX_XYZC &v = vbuf[j];
			v.x = rec.x;
			v.y = rec.y;
			v.z = rec.z;
			v.col = D3DXCOLOR(rec.r, rec.g, rec.b, 1);

			// compute brightness cutoff levels for rendering stars through atmosphere
			lvl = (int)(rec.brightness * 256.0 * 0.5);
			if (lvl > 255) lvl = 255;
			for (int k = lvl; k < plvl; k++) lvlid[k] = idx;
			plvl = lvl;
			idx++;
		}
		svtx[i]->Unlock();
	}

	for (i = 0; i < (DWORD)plvl; i++) lvlid[i] = idx;
}

// ==============================================================

void CelestialSphere::LoadConstellationLines()
{
	ncvtx = 0;

	// Read constellation line database
	const std::vector<oapi::GraphicsClient::ConstRec> clineList = gc->LoadConstellationLineData();
	if (!clineList.size()) return;

	// convert to render parameters
	const std::vector<oapi::GraphicsClient::ConstRenderRec> clineVtx = gc->ConstellationLineData2RenderData(clineList);
	if (!clineVtx.size()) return;

	// create vertex buffer
	ncvtx = clineVtx.size();
	pDevice->CreateVertexBuffer(sizeof(VERTEX_XYZ)*ncvtx, D3DUSAGE_WRITEONLY, 0, D3DPOOL_DEFAULT, &cvtx, NULL);
	VERTEX_XYZ* vbuf;
	cvtx->Lock(0, 0, (LPVOID*)&vbuf, 0);
	for (int i = 0; i < ncvtx; i++) {
		vbuf[i].x = clineVtx[i].x;
		vbuf[i].y = clineVtx[i].y;
		vbuf[i].z = clineVtx[i].z;
	}
	cvtx->Unlock();
}

// ==============================================================

void CelestialSphere::AllocGrids ()
{
	int i, j, idx;
	double lng, lat, xz, y;
	VERTEX_XYZ *vbuf;

	pDevice->CreateVertexBuffer(sizeof(VERTEX_XYZ)*(NSEG+1)*11, D3DUSAGE_WRITEONLY, 0, D3DPOOL_DEFAULT, &grdlng, NULL);
	grdlng->Lock (0, 0, (LPVOID*)&vbuf, 0);
	for (j = idx = 0; j <= 10; j++) {
		lat = (j-5)*15*RAD;
		xz = sphere_r * cos(lat);
		y  = sphere_r * sin(lat);
		for (i = 0; i <= NSEG; i++) {
			lng = 2.0*PI * (double)i/(double)NSEG;
			vbuf[idx].x = (float)(xz * cos(lng));
			vbuf[idx].z = (float)(xz * sin(lng));
			vbuf[idx].y = (float)y;
			idx++;
		}
	}
	grdlng->Unlock();

	pDevice->CreateVertexBuffer(sizeof(VERTEX_XYZ)*(NSEG+1)*12, D3DUSAGE_WRITEONLY, 0, D3DPOOL_DEFAULT, &grdlat, NULL);
	grdlat->Lock (0, 0, (LPVOID*)&vbuf, 0);
	for (j = idx = 0; j < 12; j++) {
		lng = j*15*RAD;
		for (i = 0; i <= NSEG; i++) {
			lat = 2.0*PI * (double)i/(double)NSEG;
			xz = sphere_r * cos(lat);
			y  = sphere_r * sin(lat);
			vbuf[idx].x = (float)(xz * cos(lng));
			vbuf[idx].z = (float)(xz * sin(lng));
			vbuf[idx].y = (float)y;
			idx++;
		}
	}
	grdlat->Unlock();
}

// ==============================================================

void CelestialSphere::RenderStars(ID3DXEffect *FX, DWORD nmax, const VECTOR3 *bgcol)
{
	_TRACE;

	// render in chunks, because some graphics cards have a limit in the
	// vertex list size
	UINT i, j, numPasses = 0;
	if (nmax > nsvtx) nmax = nsvtx; // sanity check

	if (bgcol) { // suppress stars darker than the background
		int bglvl = min (255, (int)((min(bgcol->x,1.0) + min(bgcol->y,1.0) + min(bgcol->z,1.0))*128.0));
		nmax = min (nmax, (DWORD)lvlid[bglvl]);
	}

	HR(pDevice->SetVertexDeclaration(pPosColorDecl));
	HR(FX->Begin(&numPasses, D3DXFX_DONOTSAVESTATE));
	HR(FX->BeginPass(0));
	for (i = j = 0; i < nmax; i += maxNumVertices, j++)
	{
		HR(pDevice->SetStreamSource(0, svtx[j], 0, sizeof(VERTEX_XYZC)));
		HR(pDevice->DrawPrimitive(D3DPT_POINTLIST, 0, min (nmax-i, maxNumVertices)));
	}
	HR(FX->EndPass());
	HR(FX->End());	
}

// ==============================================================

void CelestialSphere::RenderConstellations(ID3DXEffect *FX)
{
	_TRACE;
	UINT numPasses = 0;
	HR(FX->Begin(&numPasses, D3DXFX_DONOTSAVESTATE));
	HR(FX->BeginPass(0));
	HR(pDevice->SetStreamSource(0, cvtx, 0, sizeof(VERTEX_XYZ)));
	HR(pDevice->SetVertexDeclaration(pPositionDecl));
	HR(pDevice->DrawPrimitive(D3DPT_LINELIST, 0, ncvtx));
	HR(FX->EndPass());
	HR(FX->End());	
}

// ==============================================================

void CelestialSphere::RenderGreatCircle(ID3DXEffect *FX)
{
	_TRACE;
	UINT numPasses = 0;
	HR(FX->Begin(&numPasses, D3DXFX_DONOTSAVESTATE));
	HR(FX->BeginPass(0));
	HR(pDevice->SetStreamSource(0, grdlng, 0, sizeof(VERTEX_XYZ)));
	HR(pDevice->SetVertexDeclaration(pPositionDecl));
	HR(pDevice->DrawPrimitive(D3DPT_LINESTRIP, 5*(NSEG+1), NSEG));
	HR(FX->EndPass());
	HR(FX->End());	
	
}

// ==============================================================

void CelestialSphere::RenderGrid(ID3DXEffect *FX, bool eqline)
{
	_TRACE;
	int i;
	UINT numPasses = 0;
	HR(pDevice->SetVertexDeclaration(pPositionDecl));
	HR(pDevice->SetStreamSource(0, grdlng, 0, sizeof(VERTEX_XYZ)));
	HR(FX->Begin(&numPasses, D3DXFX_DONOTSAVESTATE));
	HR(FX->BeginPass(0));
	for (i = 0; i <= 10; i++) if (eqline || i != 5)	HR(pDevice->DrawPrimitive(D3DPT_LINESTRIP, i*(NSEG+1), NSEG));
	HR(pDevice->SetStreamSource(0, grdlat, 0, sizeof(VERTEX_XYZ)));
	for (i = 0; i < 12; i++) HR(pDevice->DrawPrimitive(D3DPT_LINESTRIP, i * (NSEG+1), NSEG));
	HR(FX->EndPass());
	HR(FX->End());	
}
