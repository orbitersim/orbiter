// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//   ORBITER VISUALISATION PROJECT (OVP)
//   D3D7 Client module
// ==============================================================

// ==============================================================
// CelSphere.cpp
// Class CelestialSphere (implementation)
//
// This class is responsible for rendering the celestial sphere
// background (stars, constellations, grids, labels, etc.)
// ==============================================================

#include "CelSphere.h"
#include "Scene.h"
#include "Camera.h"

#define NSEG 64 // number of segments in celestial grid lines

using namespace oapi;

// ==============================================================

D3D7CelestialSphere::D3D7CelestialSphere (D3D7Client* gc, Scene* scene)
	: oapi::CelestialSphere(gc)
	, m_gc(gc)
	, m_scene(scene)
{
	InitStars();
	InitConstellationLines();
	LoadConstellationLabels();
	AllocGrids();

	m_viewW = gc->GetViewW();
	m_viewH = gc->GetViewH();
	DWORD fontScale = max(m_viewH / 60, 14);
	m_cLabelFont = gc->clbkCreateFont(fontScale, true, "Arial", FONT_ITALIC);
}

// ==============================================================

D3D7CelestialSphere::~D3D7CelestialSphere ()
{
	for (auto it = m_sVtx.begin(); it != m_sVtx.end(); it++)
		(*it)->Release();
	m_cVtx->Release();
	m_grdLngVtx->Release();
	m_grdLatVtx->Release();

	m_gc->clbkReleaseFont(m_cLabelFont);
}

// ==============================================================

void D3D7CelestialSphere::InitStars()
{
	const std::vector<oapi::CelestialSphere::StarRenderRec> sList = LoadStars();
	m_nsVtx = sList.size();
	if (!m_nsVtx) return;

	const DWORD buflen = D3DMAXNUMVERTICES;
	DWORD i, j, nv, idx = 0;
	int lvl, plvl = 256;

	D3DVERTEXBUFFERDESC vbdesc;
	m_gc->SetDefault (vbdesc);
	vbdesc.dwFVF = D3DFVF_XYZ | D3DFVF_DIFFUSE;

	// convert star database to vertex buffers
	DWORD nbuf = (m_nsVtx + buflen - 1) / buflen; // number of buffers required
	m_sVtx.resize(nbuf);
	for (auto it = m_sVtx.begin(); it != m_sVtx.end(); it++) {
		nv = min(buflen, m_nsVtx - idx);
		vbdesc.dwNumVertices = nv;
		m_gc->GetDirect3D7()->CreateVertexBuffer (&vbdesc, &*it, 0);
		VERTEX_XYZC *vbuf;
		(*it)->Lock (DDLOCK_WAIT | DDLOCK_WRITEONLY | DDLOCK_DISCARDCONTENTS, (LPVOID*)&vbuf, NULL);
		for (j = 0; j < nv; j++) {
			const oapi::CelestialSphere::StarRenderRec& rec = sList[idx];
			VERTEX_XYZC& v = vbuf[j];
			v.x = (D3DVALUE)rec.pos.x;
			v.y = (D3DVALUE)rec.pos.y;
			v.z = (D3DVALUE)rec.pos.z;
			v.col = D3DRGBA(rec.col.x, rec.col.y, rec.col.z, 1);
				
			// compute brightness cutoff levels for rendering stars through atmosphere
			lvl = (int)(rec.brightness * 256.0 * 0.5);
			if (lvl > 255) lvl = 255;
			for (int k = lvl; k < plvl; k++) m_lvlIdx[k] = idx;
			plvl = lvl;
			idx++;
		}
		(*it)->Unlock();
		(*it)->Optimize (m_gc->GetDevice(), 0);
	}

	for (i = 0; i < (DWORD)plvl; i++) m_lvlIdx[i] = idx;
}

// ==============================================================

void D3D7CelestialSphere::InitConstellationLines()
{
	// convert to render parameters
	const std::vector<VECTOR3> clineVtx = LoadConstellationLines();
	m_ncVtx = clineVtx.size();
	if (!m_ncVtx) return;

	// create vertex buffer
	if (m_ncVtx > D3DMAXNUMVERTICES) {
		oapiWriteLogError("Number of constellation line vertices too large (%d). Truncating to %d.", m_ncVtx, D3DMAXNUMVERTICES);
		m_ncVtx = D3DMAXNUMVERTICES;
	}
	D3DVERTEXBUFFERDESC vbdesc;
	vbdesc.dwSize = sizeof(D3DVERTEXBUFFERDESC);
	vbdesc.dwCaps = (m_gc->GetFramework()->IsTLDevice() ? 0 : D3DVBCAPS_SYSTEMMEMORY);
	vbdesc.dwFVF = D3DFVF_XYZ;
	vbdesc.dwNumVertices = m_ncVtx;
	m_gc->GetDirect3D7()->CreateVertexBuffer(&vbdesc, &m_cVtx, 0);
	VERTEX_XYZ* vbuf;
	m_cVtx->Lock(DDLOCK_WAIT | DDLOCK_WRITEONLY | DDLOCK_DISCARDCONTENTS, (LPVOID*)&vbuf, NULL);
	for (int i = 0; i < m_ncVtx; i++) {
		vbuf[i].x = (D3DVALUE)clineVtx[i].x;
		vbuf[i].y = (D3DVALUE)clineVtx[i].y;
		vbuf[i].z = (D3DVALUE)clineVtx[i].z;
	}
	m_cVtx->Unlock();
	m_cVtx->Optimize(m_gc->GetDevice(), 0);
}

// ==============================================================

void D3D7CelestialSphere::LoadConstellationLabels()
{
	// Read constellation label database
	m_cLabel = ConstellationLabelData2RenderData(LoadConstellationLabelData());
}

// ==============================================================

void D3D7CelestialSphere::AllocGrids ()
{
	int i, j, idx;
	double lng, lat, xz, y;

	D3DVERTEXBUFFERDESC vbdesc;
	m_gc->SetDefault (vbdesc);
	vbdesc.dwFVF  = D3DFVF_XYZ;
	vbdesc.dwNumVertices = (NSEG+1) * 11;
	m_gc->GetDirect3D7()->CreateVertexBuffer (&vbdesc, &m_grdLngVtx, 0);
	VERTEX_XYZ *vbuf;
	m_grdLngVtx->Lock (DDLOCK_WAIT | DDLOCK_WRITEONLY | DDLOCK_DISCARDCONTENTS, (LPVOID*)&vbuf, NULL);
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
	m_grdLngVtx->Optimize (m_gc->GetDevice(), 0);

	vbdesc.dwNumVertices = (NSEG+1) * 12;
	m_gc->GetDirect3D7()->CreateVertexBuffer (&vbdesc, &m_grdLatVtx, 0);
	m_grdLatVtx->Lock (DDLOCK_WAIT | DDLOCK_WRITEONLY | DDLOCK_DISCARDCONTENTS, (LPVOID*)&vbuf, NULL);
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
	m_grdLatVtx->Optimize (m_gc->GetDevice(), 0);
}

// ==============================================================

void D3D7CelestialSphere::RenderStars (LPDIRECT3DDEVICE7 dev, DWORD nmax, const VECTOR3 *bgcol)
{
	// render in chunks, because some graphics cards have a limit in the
	// vertex list size

	if (!m_nsVtx) return; // nothing to do

	DWORD i, j, ns = m_nsVtx;

	if (bgcol) { // suppress stars darker than the background
		int bglvl = min (255, (int)((min(bgcol->x,1.0) + min(bgcol->y,1.0) + min(bgcol->z,1.0))*128.0));
		ns = min ((int)ns, m_lvlIdx[bglvl]);
	}

	for (i = j = 0; i < ns; i += D3DMAXNUMVERTICES, j++)
		dev->DrawPrimitiveVB (D3DPT_POINTLIST, m_sVtx[j], 0, min (ns-i, D3DMAXNUMVERTICES), 0);
}

// ==============================================================

void D3D7CelestialSphere::RenderConstellationLines (LPDIRECT3DDEVICE7 dev, VECTOR3 &col)
{
	dev->SetRenderState (D3DRENDERSTATE_TEXTUREFACTOR, D3DRGBA(col.x,col.y,col.z,1));
	dev->DrawPrimitiveVB (D3DPT_LINELIST, m_cVtx, 0, m_ncVtx, 0);
}

// ==============================================================

void D3D7CelestialSphere::RenderConstellationLabels(bool fullName)
{
	oapi::Sketchpad* pSkp = m_gc->clbkGetSketchpad(0);
	pSkp->SetFont(m_cLabelFont);
	pSkp->SetTextAlign(oapi::Sketchpad::CENTER, oapi::Sketchpad::BOTTOM);
	pSkp->SetTextColor(0xA0A0A0);
	pSkp->SetBackgroundMode(oapi::Sketchpad::BK_TRANSPARENT);

	//const std::vector<oapi::GraphicsClient::ConstLabelRenderRec>& data = GetConstellationLabels();
	for (auto it = m_cLabel.begin(); it != m_cLabel.end(); it++) {
		const std::string& label = (fullName ? (*it).fullLabel : (*it).abbrLabel);
		RenderMarker(pSkp, (*it).pos, std::string(), label, -1, 0);
	}

	m_gc->clbkReleaseSketchpad(pSkp);
}

// ==============================================================

void D3D7CelestialSphere::RenderGreatCircle (LPDIRECT3DDEVICE7 dev, VECTOR3 &col)
{
	dev->SetRenderState (D3DRENDERSTATE_TEXTUREFACTOR, D3DRGBA(col.x,col.y,col.z,1));
	dev->DrawPrimitiveVB (D3DPT_LINESTRIP, m_grdLngVtx, 5*(NSEG+1), NSEG+1, 0);
}

// ==============================================================

void D3D7CelestialSphere::RenderGrid (LPDIRECT3DDEVICE7 dev, VECTOR3 &col, bool eqline)
{
	int i;
	dev->SetRenderState (D3DRENDERSTATE_TEXTUREFACTOR, D3DRGBA(col.x,col.y,col.z,1));
	for (i = 0; i <= 10; i++) if (eqline || i != 5)
		dev->DrawPrimitiveVB (D3DPT_LINESTRIP, m_grdLngVtx, i*(NSEG+1), NSEG+1, 0);
	for (i = 0; i < 12; i++)
		dev->DrawPrimitiveVB (D3DPT_LINESTRIP, m_grdLatVtx, i*(NSEG+1), NSEG+1, 0);
}

// ==============================================================

bool D3D7CelestialSphere::EclDir2WindowPos(const VECTOR3& dir, int& x, int& y) const
{
	D3DVECTOR homog;
	D3DVECTOR fdir = { (float)dir.x, (float)dir.y, (float)dir.z };

	D3DMAT_VectorMatrixMultiply(&homog, &fdir, m_scene->GetCamera()->GetProjectionViewMatrix());
	if (homog.x >= -1.0f && homog.x <= 1.0f &&
		homog.y >= -1.0f && homog.y <= 1.0f &&
		homog.z < 1.0f) {

		if (hypot(homog.x, homog.y) < 1e-6) {
			x = m_viewW / 2;
			y = m_viewH / 2;
		}
		else {
			x = (int)(m_viewW * 0.5 * (1.0 + homog.x));
			y = (int)(m_viewH * 0.5 * (1.0 - homog.y));
		}
		return true;
	}
	else {
		return false;
	}
}
