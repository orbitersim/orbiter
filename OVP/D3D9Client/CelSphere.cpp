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
#include "Scene.h"
#include "D3D9Config.h"
#include "D3D9Pad.h"
#include "AABBUtil.h"


#define NSEG 64 // number of segments in celestial grid lines

using namespace oapi;

// ==============================================================

D3D9CelestialSphere::D3D9CelestialSphere(D3D9Client *gc, Scene *scene)
	: oapi::CelestialSphere(gc)
	, m_gc(gc)
	, m_scene(scene)
	, m_pDevice(gc->GetDevice())
	, maxNumVertices(gc->GetHardwareCaps()->MaxPrimitiveCount)
{
	InitStars();
	InitConstellationLines();
	LoadConstellationLabels();
	AllocGrids();
}

// ==============================================================

D3D9CelestialSphere::~D3D9CelestialSphere()
{
	for (auto it = m_sVtx.begin(); it != m_sVtx.end(); it++)
		(*it)->Release();
	m_cVtx->Release();
	m_grdLngVtx->Release();
	m_grdLatVtx->Release();
}

// ==============================================================

void D3D9CelestialSphere::InitStars ()
{
	const std::vector<oapi::CelestialSphere::StarRenderRec> sList = LoadStars();
	m_nsVtx = sList.size();
	if (!m_nsVtx) return;

	DWORD i, j, nv, k, idx = 0;
	int lvl, plvl = 256;

	// convert star database to vertex buffers
	DWORD nbuf = (m_nsVtx + maxNumVertices - 1) / maxNumVertices; // number of buffers required
	m_sVtx.resize(nbuf);
	for (auto it = m_sVtx.begin(); it != m_sVtx.end(); it++) {
		nv = min(maxNumVertices, m_nsVtx - idx);
		m_pDevice->CreateVertexBuffer(UINT(nv*sizeof(VERTEX_XYZC)), D3DUSAGE_WRITEONLY, 0, D3DPOOL_DEFAULT, &*it, NULL);
		VERTEX_XYZC *vbuf;
		(*it)->Lock(0, 0, (LPVOID*)&vbuf, 0);
		for (j = 0; j < nv; j++) {
			const oapi::CelestialSphere::StarRenderRec& rec = sList[idx];
			VERTEX_XYZC &v = vbuf[j];
			v.x = (float)rec.pos.x;
			v.y = (float)rec.pos.y;
			v.z = (float)rec.pos.z;
			v.col = D3DXCOLOR(rec.col.x, rec.col.y, rec.col.z, 1);

			// compute brightness cutoff levels for rendering stars through atmosphere
			lvl = (int)(rec.brightness * 256.0 * 0.5);
			if (lvl > 255) lvl = 255;
			for (int k = lvl; k < plvl; k++) m_lvlIdx[k] = idx;
			plvl = lvl;
			idx++;
		}
		(*it)->Unlock();
	}

	for (i = 0; i < (DWORD)plvl; i++) m_lvlIdx[i] = idx;
}

// ==============================================================

void D3D9CelestialSphere::InitConstellationLines()
{
	// convert to render parameters
	const std::vector<VECTOR3> clineVtx = LoadConstellationLines();
	m_ncVtx = clineVtx.size();
	if (!m_ncVtx) return;

	// create vertex buffer
	m_pDevice->CreateVertexBuffer(sizeof(VERTEX_XYZ)*m_ncVtx, D3DUSAGE_WRITEONLY, 0, D3DPOOL_DEFAULT, &m_cVtx, NULL);
	VERTEX_XYZ* vbuf;
	m_cVtx->Lock(0, 0, (LPVOID*)&vbuf, 0);
	for (int i = 0; i < m_ncVtx; i++) {
		vbuf[i].x = (float)clineVtx[i].x;
		vbuf[i].y = (float)clineVtx[i].y;
		vbuf[i].z = (float)clineVtx[i].z;
	}
	m_cVtx->Unlock();
}

// ==============================================================

void D3D9CelestialSphere::LoadConstellationLabels()
{
	// Read constellation label database
	m_cLabel = ConstellationLabelData2RenderData(LoadConstellationLabelData());
}

// ==============================================================

void D3D9CelestialSphere::AllocGrids ()
{
	int i, j, idx;
	double lng, lat, xz, y;
	VERTEX_XYZ *vbuf;

	m_pDevice->CreateVertexBuffer(sizeof(VERTEX_XYZ)*(NSEG+1)*11, D3DUSAGE_WRITEONLY, 0, D3DPOOL_DEFAULT, &m_grdLngVtx, NULL);
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

	m_pDevice->CreateVertexBuffer(sizeof(VERTEX_XYZ)*(NSEG+1)*12, D3DUSAGE_WRITEONLY, 0, D3DPOOL_DEFAULT, &m_grdLatVtx, NULL);
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

void D3D9CelestialSphere::RenderStars(ID3DXEffect *FX, DWORD nmax, const VECTOR3 *bgcol)
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

	HR(m_pDevice->SetVertexDeclaration(pPosColorDecl));
	HR(FX->Begin(&numPasses, D3DXFX_DONOTSAVESTATE));
	HR(FX->BeginPass(0));
	for (i = j = 0; i < nmax; i += maxNumVertices, j++) {
		HR(m_pDevice->SetStreamSource(0, m_sVtx[j], 0, sizeof(VERTEX_XYZC)));
		HR(m_pDevice->DrawPrimitive(D3DPT_POINTLIST, 0, min (nmax-i, maxNumVertices)));
	}
	HR(FX->EndPass());
	HR(FX->End());	
}

// ==============================================================

void D3D9CelestialSphere::RenderConstellationLines(ID3DXEffect *FX)
{
	_TRACE;
	UINT numPasses = 0;
	HR(FX->Begin(&numPasses, D3DXFX_DONOTSAVESTATE));
	HR(FX->BeginPass(0));
	HR(m_pDevice->SetStreamSource(0, m_cVtx, 0, sizeof(VERTEX_XYZ)));
	HR(m_pDevice->SetVertexDeclaration(pPositionDecl));
	HR(m_pDevice->DrawPrimitive(D3DPT_LINELIST, 0, m_ncVtx));
	HR(FX->EndPass());
	HR(FX->End());	
}

// ==============================================================

void D3D9CelestialSphere::RenderConstellationLabels(D3D9Pad* pSketch, bool fullName)
{
	pSketch->SetTextColor(0xFF8080);

	//const std::vector<oapi::GraphicsClient::ConstLabelRenderRec>& data = GetConstellationLabels();
	for (auto it = m_cLabel.begin(); it != m_cLabel.end(); it++) {
		const std::string& label = (fullName ? (*it).fullLabel : (*it).abbrLabel);
		RenderMarker(pSketch, (*it).pos, std::string(), label, -1, 0);
	}
}

// ==============================================================

void D3D9CelestialSphere::RenderGreatCircle(ID3DXEffect *FX)
{
	_TRACE;
	UINT numPasses = 0;
	HR(FX->Begin(&numPasses, D3DXFX_DONOTSAVESTATE));
	HR(FX->BeginPass(0));
	HR(m_pDevice->SetStreamSource(0, m_grdLngVtx, 0, sizeof(VERTEX_XYZ)));
	HR(m_pDevice->SetVertexDeclaration(pPositionDecl));
	HR(m_pDevice->DrawPrimitive(D3DPT_LINESTRIP, 5*(NSEG+1), NSEG));
	HR(FX->EndPass());
	HR(FX->End());	
	
}

// ==============================================================

void D3D9CelestialSphere::RenderGrid(ID3DXEffect *FX, bool eqline)
{
	_TRACE;
	int i;
	UINT numPasses = 0;
	HR(m_pDevice->SetVertexDeclaration(pPositionDecl));
	HR(m_pDevice->SetStreamSource(0, m_grdLngVtx, 0, sizeof(VERTEX_XYZ)));
	HR(FX->Begin(&numPasses, D3DXFX_DONOTSAVESTATE));
	HR(FX->BeginPass(0));
	for (i = 0; i <= 10; i++)
		if (eqline || i != 5)
			HR(m_pDevice->DrawPrimitive(D3DPT_LINESTRIP, i*(NSEG+1), NSEG));
	HR(m_pDevice->SetStreamSource(0, m_grdLatVtx, 0, sizeof(VERTEX_XYZ)));
	for (i = 0; i < 12; i++)
		HR(m_pDevice->DrawPrimitive(D3DPT_LINESTRIP, i * (NSEG+1), NSEG));
	HR(FX->EndPass());
	HR(FX->End());	
}

// ==============================================================

bool D3D9CelestialSphere::EclDir2WindowPos(const VECTOR3& dir, int& x, int& y) const
{
	D3DXVECTOR3 homog;
	D3DXVECTOR3 fdir((float)dir.x, (float)dir.y, (float)dir.z);

	D3DXVec3TransformCoord(&homog, &fdir, m_scene->GetProjectionViewMatrix());

	if (homog.x >= -1.0f && homog.x <= 1.0f &&
		homog.y >= -1.0f && homog.y <= 1.0f &&
		homog.z < 1.0f) {

		if (_hypot(homog.x, homog.y) < 1e-6) {
			x = m_scene->ViewW() / 2;
			y = m_scene->ViewH() / 2;
		}
		else {
			x = (int)(m_scene->ViewW() * 0.5 * (1.0 + homog.x));
			y = (int)(m_scene->ViewH() * 0.5 * (1.0 - homog.y));
		}
		return true;
	}
	else {
		return false;
	}
}
