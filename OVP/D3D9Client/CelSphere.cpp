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
#include "CSphereMgr.h"
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
	m_bkgImgMgr = new CSphereManager(gc, scene);

	m_mjdPrecessionChecked = -1e10;
}

// ==============================================================

D3D9CelestialSphere::~D3D9CelestialSphere()
{
	for (auto it = m_sVtx.begin(); it != m_sVtx.end(); it++)
		(*it)->Release();
	m_cVtx->Release();
	m_grdLngVtx->Release();
	m_grdLatVtx->Release();
	delete m_bkgImgMgr;
}

// ==============================================================

void D3D9CelestialSphere::InitCelestialTransform()
{
	MATRIX3 R = Celestial2Ecliptic();

	m_rotCelestial._11 = (float)R.m11; m_rotCelestial._12 = (float)R.m12; m_rotCelestial._13 = (float)R.m13; m_rotCelestial._14 = 0.0f;
	m_rotCelestial._21 = (float)R.m21; m_rotCelestial._22 = (float)R.m22; m_rotCelestial._23 = (float)R.m23; m_rotCelestial._24 = 0.0f;
	m_rotCelestial._31 = (float)R.m31; m_rotCelestial._32 = (float)R.m32; m_rotCelestial._33 = (float)R.m33; m_rotCelestial._34 = 0.0f;
	m_rotCelestial._41 = 0.0f;         m_rotCelestial._42 = 0.0f;         m_rotCelestial._43 = 0.0f;         m_rotCelestial._44 = 1.0f;

	m_mjdPrecessionChecked = oapiGetSimMJD();
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

void D3D9CelestialSphere::Render(LPDIRECT3DDEVICE9 pDevice, double bglvl)
{
	// Get celestial sphere render flags
	DWORD renderFlag = *(DWORD*)m_gc->GetConfigParam(CFGPRM_PLANETARIUMFLAG);

	// celestial sphere background image
	RenderBkgImage(pDevice, bglvl);

	if (renderFlag & PLN_ENABLE) {

		double colScale = 1.0 - bglvl; // feature brightness modifier for lit background

		HR(s_FX->SetTechnique(s_eLine));
		HR(s_FX->SetMatrix(s_eWVP, m_scene->GetProjectionViewMatrix()));

		// render ecliptic grid
		if (renderFlag & PLN_EGRID) {
			D3DXVECTOR4 vColor(0.0f, 0.0f, 0.4f * colScale, 1.0f);
			HR(s_FX->SetVector(s_eColor, &vColor));
			RenderGrid(s_FX, !(renderFlag & PLN_ECL));
		}

		// render ecliptic equator
		if (renderFlag & PLN_ECL) {
			D3DXVECTOR4 vColor(0.0f, 0.0f, 0.8f * colScale, 1.0f);
			HR(s_FX->SetVector(s_eColor, &vColor));
			RenderGreatCircle(s_FX);
		}

		// render celestial grid ----------------------------------------------------------------------------
		if (renderFlag & PLN_CGRID) {
			if (fabs(m_mjdPrecessionChecked - oapiGetSimMJD()) > 1e3)
				InitCelestialTransform();
			D3DXMATRIX rot;
			D3DXMatrixMultiply(&rot, &m_rotCelestial, m_scene->GetProjectionViewMatrix());
			HR(s_FX->SetMatrix(s_eWVP, &rot));
			D3DXVECTOR4 vColor1(0.3f * colScale, 0.0f, 0.3f * colScale, 1.0f);
			HR(s_FX->SetVector(s_eColor, &vColor1));
			RenderGrid(s_FX, false);
			D3DXVECTOR4 vColor2(0.7f * colScale, 0.0f, 0.7f * colScale, 1.0f);
			HR(s_FX->SetVector(s_eColor, &vColor2));
			RenderGreatCircle(s_FX);
		}

		// render constellation lines ----------------------------------------------------------------------------
		//
		if (renderFlag & PLN_CONST) {
			HR(s_FX->SetMatrix(s_eWVP, m_scene->GetProjectionViewMatrix()));
			D3DXVECTOR4 vColor(0.4f * colScale, 0.3f * colScale, 0.2f * colScale, 1.0f);
			HR(s_FX->SetVector(s_eColor, &vColor));
			RenderConstellationLines(s_FX);
		}
	}

	// render stars
	HR(s_FX->SetTechnique(s_eStar));
	HR(s_FX->SetMatrix(s_eWVP, m_scene->GetProjectionViewMatrix()));
	RenderStars(s_FX, (DWORD)-1, bglvl);

	// render markers and labels
	if (renderFlag & PLN_ENABLE) {

		// Sketchpad for planetarium mode labels and markers
		D3D9Pad* pSketch = m_scene->GetPooledSketchpad(0);

		if (pSketch) {

			// constellation labels --------------------------------------------------
			if (renderFlag & PLN_CNSTLABEL) {
				RenderConstellationLabels(pSketch, renderFlag & PLN_CNSTLONG);
			}
			// celestial marker (stars) names ----------------------------------------
			if (renderFlag & PLN_CCMARK) {
				RenderCelestialMarkers((oapi::Sketchpad**)&pSketch);
			}
			pSketch->EndDrawing(); //SKETCHPAD_LABELS
		}
	}

}

// ==============================================================

void D3D9CelestialSphere::RenderStars(ID3DXEffect *FX, DWORD nmax, double bglvl)
{
	_TRACE;

	// render in chunks, because some graphics cards have a limit in the
	// vertex list size
	UINT i, j, numPasses = 0;
	DWORD ns = m_nsVtx;

	if (bglvl) { // suppress stars darker than the background
		int bgidx = min(255, pow(bglvl * 1.4, 0.75) * 255.0);
		ns = min((int)ns, m_lvlIdx[bgidx]);
	}

	HR(m_pDevice->SetVertexDeclaration(pPosColorDecl));
	HR(FX->Begin(&numPasses, D3DXFX_DONOTSAVESTATE));
	HR(FX->BeginPass(0));
	for (i = j = 0; i < ns; i += maxNumVertices, j++) {
		HR(m_pDevice->SetStreamSource(0, m_sVtx[j], 0, sizeof(VERTEX_XYZC)));
		HR(m_pDevice->DrawPrimitive(D3DPT_POINTLIST, 0, min (ns-i, maxNumVertices)));
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

	const std::vector<oapi::GraphicsClient::LABELSPEC>& label = ConstellationLabels();
	for (auto it = label.begin(); it != label.end(); it++) {
		const std::string& label = (*it).label[fullName ? 0 : 1];
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

void D3D9CelestialSphere::RenderBkgImage(LPDIRECT3DDEVICE9 dev, int bglvl)
{
	m_bkgImgMgr->Render(dev, 8, bglvl);
	dev->SetRenderState(D3DRS_CULLMODE, D3DCULL_CCW);
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

void D3D9CelestialSphere::D3D9TechInit(ID3DXEffect* fx)
{
	s_FX = fx;
	s_eStar = fx->GetTechniqueByName("StarTech");
	s_eLine = fx->GetTechniqueByName("LineTech");
	s_eColor = fx->GetParameterByName(0, "gColor");
	s_eWVP = fx->GetParameterByName(0, "gWVP");
}

ID3DXEffect* D3D9CelestialSphere::s_FX = 0;
D3DXHANDLE D3D9CelestialSphere::s_eStar = 0;
D3DXHANDLE D3D9CelestialSphere::s_eLine = 0;
D3DXHANDLE D3D9CelestialSphere::s_eColor = 0;
D3DXHANDLE D3D9CelestialSphere::s_eWVP = 0;
