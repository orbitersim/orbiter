// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#include "CelSphere.h"
#include "Vecmat.h"
#include "Log.h"

struct VB_XYZ { float x, y, z; };
#define NSEG 64

// ==============================================================

CelestialSphere::CelestialSphere(OrbiterGraphics* og)
	: m_gc(og)
{
	LoadStars();
	LoadConstellationLines();
	LoadConstellationLabels();
	AllocGrids();
}

// ==============================================================

CelestialSphere::~CelestialSphere()
{
	for (auto it = m_sVtx.begin(); it != m_sVtx.end(); it++)
		(*it)->Release();
	m_cVtx->Release();
	m_grdLngVtx->Release();
	m_grdLatVtx->Release();

	if (vb_target)
		vb_target->Release();
	if (vb_cnstlabel)
		vb_cnstlabel->Release();
}

// ==============================================================

void CelestialSphere::LoadStars()
{
	m_nsVtx = 0;

	StarRenderPrm* prm = (StarRenderPrm*)m_gc->GetConfigParam(CFGPRM_STARRENDERPRM);

	// Read the star database
	const std::vector<oapi::GraphicsClient::StarRec> starList = m_gc->LoadStarData(prm->mag_lo);
	if (!starList.size()) return;

	// convert to render parameters
	const std::vector<oapi::GraphicsClient::StarRenderRec> renderList = m_gc->StarData2RenderData(starList, *prm);
	if (!renderList.size()) return;

	const DWORD buflen = D3DMAXNUMVERTICES;
	DWORD i, j, nv, idx = 0;
	int lvl, plvl = 256;

	D3DVERTEXBUFFERDESC vbdesc;
	vbdesc.dwSize = sizeof(D3DVERTEXBUFFERDESC);
	vbdesc.dwCaps = (m_gc->GetFramework()->IsTLDevice() ? 0 : D3DVBCAPS_SYSTEMMEMORY);
	vbdesc.dwFVF = D3DFVF_XYZ | D3DFVF_DIFFUSE;

	// convert star database to vertex buffers
	m_nsVtx = starList.size();
	DWORD nbuf = (m_nsVtx + buflen - 1) / buflen; // number of buffers required
	m_sVtx.resize(nbuf);
	for (auto it = m_sVtx.begin(); it != m_sVtx.end(); it++) {
		nv = min(buflen, m_nsVtx - idx);
		vbdesc.dwNumVertices = nv;
		m_gc->GetDirect3D7()->CreateVertexBuffer(&vbdesc, &*it, 0);
		VERTEX_XYZC* vbuf;
		(*it)->Lock(DDLOCK_WAIT | DDLOCK_WRITEONLY | DDLOCK_DISCARDCONTENTS, (LPVOID*)&vbuf, NULL);
		for (j = 0; j < nv; j++) {
			const oapi::GraphicsClient::StarRenderRec& rec = renderList[idx];
			VERTEX_XYZC& v = vbuf[j];
			v.x = rec.x;
			v.y = rec.y;
			v.z = rec.z;
			v.col = D3DRGBA(rec.r, rec.g, rec.b, 1);

			// compute brightness cutoff levels for rendering stars through atmosphere
			lvl = (int)(rec.brightness * 256.0 * 0.5);
			if (lvl > 255) lvl = 255;
			for (int k = lvl; k < plvl; k++) m_lvlIdx[k] = idx;
			plvl = lvl;
			idx++;
		}
		(*it)->Unlock();
		(*it)->Optimize(m_gc->GetDevice(), 0);
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
		vbuf[i].x = clineVtx[i].x;
		vbuf[i].y = clineVtx[i].y;
		vbuf[i].z = clineVtx[i].z;
	}
	m_cVtx->Unlock();
	m_cVtx->Optimize(m_gc->GetDevice(), 0);
}

// ==============================================================

void CelestialSphere::LoadConstellationLabels()
{
	FILE* f = fopen("Constell2.bin", "rb");
	if (f) {
		struct {
			double lng, lat;
		} buf;
		int i;
		D3DVERTEXBUFFERDESC vbdesc;
		vbdesc.dwSize = sizeof(D3DVERTEXBUFFERDESC);
		vbdesc.dwCaps = D3DVBCAPS_SYSTEMMEMORY; // 0;
		vbdesc.dwFVF = D3DFVF_XYZ;
		vbdesc.dwNumVertices = MAXCONST + 1;
		// NOTE: without buffersize "+1", after calling ProcessVertices a CTD occurs on exit
		// at rare occasions (device bug?)
		VB_XYZ* vbpos;
		m_gc->GetDirect3D7()->CreateVertexBuffer(&vbdesc, &vb_cnstlabel, 0);
		m_gc->GetDirect3D7()->CreateVertexBuffer(&vbdesc, &vb_target, 0);
		vb_cnstlabel->Lock(DDLOCK_WAIT | DDLOCK_WRITEONLY | DDLOCK_DISCARDCONTENTS, (LPVOID*)&vbpos, NULL);
		for (i = 0; i < MAXCONST; i++) {
			if (!fread(&buf, sizeof(buf), 1, f)) break;
			double xz = cos(buf.lat);
			vbpos[i].x = (float)(xz * cos(buf.lng));
			vbpos[i].z = (float)(xz * sin(buf.lng));
			vbpos[i].y = (float)(sin(buf.lat));
			if (!fread(cnstlabel[i].abbr, 3, 1, f)) break;
			if (!fread(&cnstlabel[i].len, sizeof(int), 1, f)) break;
			cnstlabel[i].full = new char[cnstlabel[i].len]; TRACENEW
				if (!fread(cnstlabel[i].full, cnstlabel[i].len, 1, f)) {
					delete[]cnstlabel[i].full;
					break;
				}
		}
		vb_cnstlabel->Unlock();
		fclose(f);
		ncnstlabel = i;
	}
	else {
		LOGOUT_WARN("Constellation data base for celestial sphere (Constell2.bin) not found. Disabling constellation labels.");
	}
}

// ==============================================================

void CelestialSphere::AllocGrids()
{
	int i, j, idx;
	double lng, lat, xz, y;

	D3DVERTEXBUFFERDESC vbdesc;
	vbdesc.dwSize = sizeof(D3DVERTEXBUFFERDESC);
	vbdesc.dwCaps = (m_gc->GetFramework()->IsTLDevice() ? 0 : D3DVBCAPS_SYSTEMMEMORY);
	vbdesc.dwFVF = D3DFVF_XYZ;
	vbdesc.dwNumVertices = (NSEG + 1) * 11;
	m_gc->GetDirect3D7()->CreateVertexBuffer(&vbdesc, &m_grdLngVtx, 0);
	VERTEX_XYZ* vbuf;
	m_grdLngVtx->Lock(DDLOCK_WAIT | DDLOCK_WRITEONLY | DDLOCK_DISCARDCONTENTS, (LPVOID*)&vbuf, NULL);
	for (j = idx = 0; j <= 10; j++) {
		lat = (j - 5) * 15 * RAD;
		xz = cos(lat);
		y = sin(lat);
		for (i = 0; i <= NSEG; i++) {
			lng = Pi2 * (double)i / (double)NSEG;
			vbuf[idx].x = (float)(xz * cos(lng));
			vbuf[idx].z = (float)(xz * sin(lng));
			vbuf[idx].y = (float)y;
			idx++;
		}
	}
	m_grdLngVtx->Unlock();
	m_grdLngVtx->Optimize(m_gc->GetDevice(), 0);

	vbdesc.dwNumVertices = (NSEG + 1) * 12;
	m_gc->GetDirect3D7()->CreateVertexBuffer(&vbdesc, &m_grdLatVtx, 0);
	m_grdLatVtx->Lock(DDLOCK_WAIT | DDLOCK_WRITEONLY | DDLOCK_DISCARDCONTENTS, (LPVOID*)&vbuf, NULL);
	for (j = idx = 0; j < 12; j++) {
		lng = j * 15 * RAD;
		for (i = 0; i <= NSEG; i++) {
			lat = Pi2 * (double)i / (double)NSEG;
			xz = cos(lat);
			y  = sin(lat);
			vbuf[idx].x = (float)(xz * cos(lng));
			vbuf[idx].z = (float)(xz * sin(lng));
			vbuf[idx].y = (float)y;
			idx++;
		}
	}
	m_grdLatVtx->Unlock();
	m_grdLatVtx->Optimize(m_gc->GetDevice(), 0);
}

// ==============================================================

void CelestialSphere::RenderStars(LPDIRECT3DDEVICE7 dev, DWORD nmax, const Vector* bgcol)
{
	// render in chunks, because some graphics cards have a limit in the
	// vertex list size

	if (!m_nsVtx) return; // nothing to do

	DWORD i, j, ns = m_nsVtx;

	if (bgcol) { // suppress stars darker than the background
		int bglvl = min(255, (int)((min(bgcol->x, 1.0) + min(bgcol->y, 1.0) + min(bgcol->z, 1.0)) * 128.0));
		ns = min((int)ns, m_lvlIdx[bglvl]);
	}

	for (i = j = 0; i < ns; i += D3DMAXNUMVERTICES, j++)
		dev->DrawPrimitiveVB(D3DPT_POINTLIST, m_sVtx[j], 0, min(ns - i, D3DMAXNUMVERTICES), 0);
}

// ==============================================================

void CelestialSphere::RenderConstellationLines(LPDIRECT3DDEVICE7 dev, const Vector& col)
{
	dev->SetRenderState(D3DRENDERSTATE_TEXTUREFACTOR, D3DRGBA(col.x, col.y, col.z, 1));
	dev->DrawPrimitiveVB(D3DPT_LINELIST, m_cVtx, 0, m_ncVtx, 0);
}

// ==============================================================

void CelestialSphere::RenderGreatCircle(LPDIRECT3DDEVICE7 dev, Vector& col)
{
	dev->SetRenderState(D3DRENDERSTATE_TEXTUREFACTOR, D3DRGBA(col.x, col.y, col.z, 1));
	dev->DrawPrimitiveVB(D3DPT_LINESTRIP, m_grdLngVtx, 5 * (NSEG + 1), NSEG + 1, 0);
}

// ==============================================================

void CelestialSphere::RenderGrid(LPDIRECT3DDEVICE7 dev, Vector& col, bool eqline)
{
	int i;
	dev->SetRenderState(D3DRENDERSTATE_TEXTUREFACTOR, D3DRGBA(col.x, col.y, col.z, 1));
	for (i = 0; i <= 10; i++) if (eqline || i != 5)
		dev->DrawPrimitiveVB(D3DPT_LINESTRIP, m_grdLngVtx, i * (NSEG + 1), NSEG + 1, 0);
	for (i = 0; i < 12; i++)
		dev->DrawPrimitiveVB(D3DPT_LINESTRIP, m_grdLatVtx, i * (NSEG + 1), NSEG + 1, 0);
}
