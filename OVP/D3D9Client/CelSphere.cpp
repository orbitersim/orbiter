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

CelestialSphere::CelestialSphere(D3D9Client *gc)
	: m_gc(gc)
	, pDevice(gc->GetDevice())
	, maxNumVertices(gc->GetHardwareCaps()->MaxPrimitiveCount)
{
	LoadStars ();
	LoadConstellationLines ();
	AllocGrids ();
}

// ==============================================================

CelestialSphere::~CelestialSphere()
{
	if (m_nsBuf) {
		for (size_t i = 0; i < m_nsBuf; ++i) {
			m_sVtx[i]->Release();
		}
		delete []m_sVtx;
		m_sVtx = NULL;
	}
	m_cVtx->Release();
	m_grdLngVtx->Release();
	m_grdLatVtx->Release();
}

// ==============================================================

void CelestialSphere::LoadStars ()
{
	m_nsVtx = m_nsBuf = 0;

	StarRenderPrm *prm = (StarRenderPrm*)m_gc->GetConfigParam (CFGPRM_STARRENDERPRM);

	// Read the star database
	const std::vector<oapi::GraphicsClient::StarRec> starList = m_gc->LoadStarData(prm->mag_lo);
	if (!starList.size()) return;

	// convert to render parameters
	const std::vector<oapi::GraphicsClient::StarRenderRec> renderList = m_gc->StarData2RenderData(starList, *prm);
	if (!renderList.size()) return;

	DWORD i, j, nv, k, idx = 0;
	int lvl, plvl = 256;

	// convert star database to vertex buffers
	m_nsVtx = starList.size();
	m_nsBuf = (m_nsVtx + maxNumVertices - 1) / maxNumVertices; // number of buffers required
	m_sVtx = new LPDIRECT3DVERTEXBUFFER9[m_nsBuf];
	for (i = idx = 0; i < m_nsBuf; i++) {
		nv = min(maxNumVertices, m_nsVtx - i * maxNumVertices);
		pDevice->CreateVertexBuffer(UINT(nv*sizeof(VERTEX_XYZC)), D3DUSAGE_WRITEONLY, 0, D3DPOOL_DEFAULT, &m_sVtx[i], NULL);
		VERTEX_XYZC *vbuf;
		m_sVtx[i]->Lock(0, 0, (LPVOID*)&vbuf, 0);
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
			for (int k = lvl; k < plvl; k++) m_lvlIdx[k] = idx;
			plvl = lvl;
			idx++;
		}
		m_sVtx[i]->Unlock();
	}

	for (i = 0; i < (DWORD)plvl; i++) m_lvlIdx[i] = idx;
}

// ==============================================================

void CelestialSphere::LoadConstellationLines()
{
	m_ncVtx = 0;

	// Read constellation line database
	const std::vector<oapi::GraphicsClient::ConstRec> clineList = m_gc->LoadConstellationLineData();
	if (!clineList.size()) return;

	// convert to render parameters
	const std::vector<oapi::GraphicsClient::ConstRenderRec> clineVtx = m_gc->ConstellationLineData2RenderData(clineList);
	if (!clineVtx.size()) return;

	// create vertex buffer
	m_ncVtx = clineVtx.size();
	pDevice->CreateVertexBuffer(sizeof(VERTEX_XYZ)*m_ncVtx, D3DUSAGE_WRITEONLY, 0, D3DPOOL_DEFAULT, &m_cVtx, NULL);
	VERTEX_XYZ* vbuf;
	m_cVtx->Lock(0, 0, (LPVOID*)&vbuf, 0);
	for (int i = 0; i < m_ncVtx; i++) {
		vbuf[i].x = clineVtx[i].x;
		vbuf[i].y = clineVtx[i].y;
		vbuf[i].z = clineVtx[i].z;
	}
	m_cVtx->Unlock();
}

// ==============================================================

void CelestialSphere::AllocGrids ()
{
	int i, j, idx;
	double lng, lat, xz, y;
	VERTEX_XYZ *vbuf;

	pDevice->CreateVertexBuffer(sizeof(VERTEX_XYZ)*(NSEG+1)*11, D3DUSAGE_WRITEONLY, 0, D3DPOOL_DEFAULT, &m_grdLngVtx, NULL);
	m_grdLngVtx->Lock (0, 0, (LPVOID*)&vbuf, 0);
	for (j = idx = 0; j <= 10; j++) {
		lat = (j-5)*15*RAD;
		xz = cos(lat);
		y  = sin(lat);
		for (i = 0; i <= NSEG; i++) {
			lng = 2.0*PI * (double)i/(double)NSEG;
			vbuf[idx].x = (float)(xz * cos(lng));
			vbuf[idx].z = (float)(xz * sin(lng));
			vbuf[idx].y = (float)y;
			idx++;
		}
	}
	m_grdLngVtx->Unlock();

	pDevice->CreateVertexBuffer(sizeof(VERTEX_XYZ)*(NSEG+1)*12, D3DUSAGE_WRITEONLY, 0, D3DPOOL_DEFAULT, &m_grdLatVtx, NULL);
	m_grdLatVtx->Lock (0, 0, (LPVOID*)&vbuf, 0);
	for (j = idx = 0; j < 12; j++) {
		lng = j*15*RAD;
		for (i = 0; i <= NSEG; i++) {
			lat = 2.0*PI * (double)i/(double)NSEG;
			xz = cos(lat);
			y  = sin(lat);
			vbuf[idx].x = (float)(xz * cos(lng));
			vbuf[idx].z = (float)(xz * sin(lng));
			vbuf[idx].y = (float)y;
			idx++;
		}
	}
	m_grdLatVtx->Unlock();
}

// ==============================================================

void CelestialSphere::RenderStars(ID3DXEffect *FX, DWORD nmax, const VECTOR3 *bgcol)
{
	_TRACE;

	// render in chunks, because some graphics cards have a limit in the
	// vertex list size
	UINT i, j, numPasses = 0;
	if (nmax > m_nsVtx) nmax = m_nsVtx; // sanity check

	if (bgcol) { // suppress stars darker than the background
		int bglvl = min (255, (int)((min(bgcol->x,1.0) + min(bgcol->y,1.0) + min(bgcol->z,1.0))*128.0));
		nmax = min (nmax, (DWORD)m_lvlIdx[bglvl]);
	}

	HR(pDevice->SetVertexDeclaration(pPosColorDecl));
	HR(FX->Begin(&numPasses, D3DXFX_DONOTSAVESTATE));
	HR(FX->BeginPass(0));
	for (i = j = 0; i < nmax; i += maxNumVertices, j++)
	{
		HR(pDevice->SetStreamSource(0, m_sVtx[j], 0, sizeof(VERTEX_XYZC)));
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
	HR(pDevice->SetStreamSource(0, m_cVtx, 0, sizeof(VERTEX_XYZ)));
	HR(pDevice->SetVertexDeclaration(pPositionDecl));
	HR(pDevice->DrawPrimitive(D3DPT_LINELIST, 0, m_ncVtx));
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
	HR(pDevice->SetStreamSource(0, m_grdLngVtx, 0, sizeof(VERTEX_XYZ)));
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
	HR(pDevice->SetStreamSource(0, m_grdLngVtx, 0, sizeof(VERTEX_XYZ)));
	HR(FX->Begin(&numPasses, D3DXFX_DONOTSAVESTATE));
	HR(FX->BeginPass(0));
	for (i = 0; i <= 10; i++) if (eqline || i != 5)	HR(pDevice->DrawPrimitive(D3DPT_LINESTRIP, i*(NSEG+1), NSEG));
	HR(pDevice->SetStreamSource(0, m_grdLatVtx, 0, sizeof(VERTEX_XYZ)));
	for (i = 0; i < 12; i++) HR(pDevice->DrawPrimitive(D3DPT_LINESTRIP, i * (NSEG+1), NSEG));
	HR(FX->EndPass());
	HR(FX->End());	
}
