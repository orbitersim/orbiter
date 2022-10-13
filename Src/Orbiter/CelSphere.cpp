// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#include "CelSphere.h"
#include "Scene.h"
#include "Camera.h"
#include "CSphereMgr.h"
#include "Vecmat.h"
#include "D3dmath.h"
#include "Log.h"

#define NSEG 64

// ==============================================================

OGCelestialSphere::OGCelestialSphere(OrbiterGraphics* gc, Scene* scene)
	: oapi::CelestialSphere(gc)
	, m_gc(gc)
	, m_scene(scene)
{
	m_bkgImgMgr = nullptr;
	m_bkgImgMgr2 = nullptr;

	InitStars();
	InitConstellationLines();
	AllocGrids();
	InitBackgroundManager();

	m_viewW = gc->GetViewW();
	m_viewH = gc->GetViewH();
	m_mjdPrecessionChecked = -1e10;
	DWORD fontScale = max(m_viewH / 60, 14);
	m_cLabelFont = gc->clbkCreateFont(fontScale, true, "Arial", FONT_ITALIC);
}

// ==============================================================

OGCelestialSphere::~OGCelestialSphere()
{
	for (auto it = m_sVtx.begin(); it != m_sVtx.end(); it++)
		(*it)->Release();
	m_cVtx->Release();
	m_grdLngVtx->Release();
	m_grdLatVtx->Release();

	if (m_bkgImgMgr)
		delete m_bkgImgMgr;
	if (m_bkgImgMgr2)
		delete m_bkgImgMgr2;

	m_gc->clbkReleaseFont(m_cLabelFont);
}

// ==============================================================

void OGCelestialSphere::InitCelestialTransform()
{
	// Set up rotation for celestial grid rendering
	double eps, lan;
	OBJHANDLE hEarth = oapiGetGbodyByName("Earth");
	if (hEarth) {  // use current Earth precession axis
		eps = oapiGetPlanetObliquity(hEarth);
		lan = oapiGetPlanetTheta(hEarth);
	} 
	else {         // default: use the J2000 ecliptic
		eps = 0.4092797095927;
		lan = 0.0;
	}
	float coso = (float)cos(eps), sino = (float)sin(eps);
	float cosl = (float)cos(lan), sinl = (float)sin(lan);
	m_rotCelestial._11 = cosl;         m_rotCelestial._12 = 0.0f;  m_rotCelestial._13 = sinl;        m_rotCelestial._14 = 0.0f;
	m_rotCelestial._21 = -sino * sinl; m_rotCelestial._22 = coso;  m_rotCelestial._23 = sino * cosl; m_rotCelestial._24 = 0.0f;
	m_rotCelestial._31 = -coso * sinl; m_rotCelestial._32 = -sino; m_rotCelestial._33 = coso * cosl; m_rotCelestial._34 = 0.0f;
	m_rotCelestial._41 = 0.0f;         m_rotCelestial._42 = 0.0f;  m_rotCelestial._43 = 0.0f;        m_rotCelestial._44 = 1.0f;

	m_mjdPrecessionChecked = oapiGetSimMJD();
}

// ==============================================================

void OGCelestialSphere::InitStars()
{
	const std::vector<StarRenderRec> sList = LoadStars();
	m_nsVtx = sList.size();
	if (!m_nsVtx) return;

	const DWORD buflen = D3DMAXNUMVERTICES;
	DWORD i, j, nv, idx = 0;
	int lvl, plvl = 256;

	D3DVERTEXBUFFERDESC vbdesc;
	vbdesc.dwSize = sizeof(D3DVERTEXBUFFERDESC);
	vbdesc.dwCaps = (m_gc->GetFramework()->IsTLDevice() ? 0 : D3DVBCAPS_SYSTEMMEMORY);
	vbdesc.dwFVF = D3DFVF_XYZ | D3DFVF_DIFFUSE;

	// convert star database to vertex buffers
	DWORD nbuf = (m_nsVtx + buflen - 1) / buflen; // number of buffers required
	m_sVtx.resize(nbuf);
	for (auto it = m_sVtx.begin(); it != m_sVtx.end(); it++) {
		nv = min(buflen, m_nsVtx - idx);
		vbdesc.dwNumVertices = nv;
		m_gc->GetDirect3D7()->CreateVertexBuffer(&vbdesc, &*it, 0);
		VERTEX_XYZC* vbuf;
		(*it)->Lock(DDLOCK_WAIT | DDLOCK_WRITEONLY | DDLOCK_DISCARDCONTENTS, (LPVOID*)&vbuf, NULL);
		for (j = 0; j < nv; j++) {
			const StarRenderRec& rec = sList[idx];
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
		(*it)->Optimize(m_gc->GetDevice(), 0);
	}

	for (i = 0; i < (DWORD)plvl; i++) m_lvlIdx[i] = idx;
}

// ==============================================================

void OGCelestialSphere::InitConstellationLines()
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

void OGCelestialSphere::AllocGrids()
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

void OGCelestialSphere::InitBackgroundManager()
{
	if (m_bkgImgMgr) {
		delete m_bkgImgMgr;
		m_bkgImgMgr = nullptr;
	}
	if (m_bkgImgMgr2) {
		delete m_bkgImgMgr2;
		m_bkgImgMgr2 = nullptr;
	}

	char* cTexPath = (char*)m_gc->GetConfigParam(CFGPRM_CSPHERETEXTURE);
	if (!cTexPath[0]) return;

	char cbuf[256];
	m_gc->Cfg()->TexPath(cbuf, cTexPath);

	DWORD fa = GetFileAttributes(cbuf);
	if (0 /*fa & FILE_ATTRIBUTE_DIRECTORY*/) {  // This requires more work
		m_bkgImgMgr2 = new CsphereManager(cTexPath, 8, 8);

		Matrix R(2000, 0, 0, 0, 2000, 0, 0, 0, 2000), ecl2gal;
		double theta = 60.25 * RAD; // 60.18*RAD;
		double phi = 90.09 * RAD; // 90.02*RAD;
		double lambda = 173.64 * RAD; // 173.6*RAD;
		double sint = sin(theta), cost = cos(theta);
		double sinp = sin(phi), cosp = cos(phi);
		double sinl = sin(lambda), cosl = cos(lambda);
		ecl2gal.Set(cosp, 0, sinp, 0, 1, 0, -sinp, 0, cosp);
		ecl2gal.premul(Matrix(1, 0, 0, 0, cost, sint, 0, -sint, cost));
		ecl2gal.premul(Matrix(cosl, 0, sinl, 0, 1, 0, -sinl, 0, cosl));
		R.premul(ecl2gal);
		m_WMcsphere = _M(R.m11, R.m12, R.m13, 0,
			R.m21, R.m22, R.m23, 0,
			R.m31, R.m32, R.m33, 0,
			0, 0, 0, 1);
	}
	else {
		m_bkgImgMgr = new CSphereManager;
	}
}


// ==============================================================

void OGCelestialSphere::Render(LPDIRECT3DDEVICE7 dev, double bglvl)
{
	static D3DMATRIX ident = {
		1,0,0,0,
		0,1,0,0,
		0,0,1,0,
		0,0,0,1
	};

	// Get celestial sphere render flags
	DWORD renderFlag = m_gc->Cfg()->CfgVisHelpPrm.flagPlanetarium;

	// Turn off z-buffer and lighting calculations
	dev->SetRenderState(D3DRENDERSTATE_ZENABLE, FALSE);
	dev->SetRenderState(D3DRENDERSTATE_ZVISIBLE, FALSE);
	dev->SetRenderState(D3DRENDERSTATE_ZWRITEENABLE, FALSE);
	dev->SetRenderState(D3DRENDERSTATE_LIGHTING, FALSE);

	// celestial sphere background
	RenderBkgImage(dev, bglvl);

	dev->SetTransform(D3DTRANSFORMSTATE_WORLD, &ident);
	dev->SetTexture(0, 0);

	if (renderFlag & PLN_ENABLE) {

		// use explicit colours
		dev->SetTextureStageState(0, D3DTSS_COLORARG1, D3DTA_TFACTOR);
		dev->SetTextureStageState(0, D3DTSS_COLOROP, D3DTOP_SELECTARG1);

		// set additive blending with background
		DWORD dstblend;
		dev->GetRenderState(D3DRENDERSTATE_DESTBLEND, &dstblend);
		dev->SetRenderState(D3DRENDERSTATE_DESTBLEND, D3DBLEND_ONE);
		dev->SetRenderState(D3DRENDERSTATE_ALPHABLENDENABLE, TRUE);
		double colScale = 1.0 - bglvl; // feature brightness modifier for lit background

		// render ecliptic grid
		if (renderFlag & PLN_EGRID)
			RenderGrid(dev, Vector(0, 0, 0.4) * colScale, (renderFlag & PLN_ECL) == 0);

		// render ecliptic equator
		if (renderFlag & PLN_ECL)
			RenderGreatCircle(dev, Vector(0, 0, 0.8) * colScale);

		// render celestial grid
		if (renderFlag & PLN_CGRID) {
			if (fabs(m_mjdPrecessionChecked - oapiGetSimMJD()) > 1e3)
				InitCelestialTransform();
			dev->SetTransform(D3DTRANSFORMSTATE_WORLD, &m_rotCelestial);
			RenderGrid(dev, Vector(0.3, 0, 0.3) * colScale, false);
			RenderGreatCircle(dev, Vector(0.7, 0, 0.7) * colScale);
			dev->SetTransform(D3DTRANSFORMSTATE_WORLD, &ident);
		}

		// render equator of target celestial body
		if (renderFlag & PLN_EQU) {
			OBJHANDLE hRef = oapiCameraProxyGbody();
			if (hRef) {
				MATRIX3 R;
				oapiGetRotationMatrix(hRef, &R);
				D3DMATRIX iRot = { R.m11, R.m21, R.m31, 0.0f,   R.m12, R.m22, R.m32, 0.0f,  R.m13, R.m23, R.m33, 0.0f,  0.0f, 0.0f, 0.0f, 1.0f };
				dev->SetTransform(D3DTRANSFORMSTATE_WORLD, &iRot);
				RenderGreatCircle(dev, Vector(0, 0.6, 0) * colScale);
				dev->SetTransform(D3DTRANSFORMSTATE_WORLD, &ident);
			}
		}

		// render constellation lines
		if (renderFlag & PLN_CONST)
			RenderConstellationLines(dev, Vector(0.4, 0.3, 0.2) * colScale);

		oapi::Sketchpad* pSkp = nullptr;

		// render constellation labels
		if (renderFlag & PLN_CNSTLABEL)
			RenderConstellationLabels(&pSkp, (renderFlag & PLN_CNSTLONG) == PLN_CNSTLONG);

		// render celestial sphere markers
		if (renderFlag & PLN_CCMARK)
			RenderCelestialMarkers(&pSkp);

		if (pSkp)
			m_gc->clbkReleaseSketchpad(pSkp);

		// revert to standard colour selection and turn off alpha blending
		dev->SetTextureStageState(0, D3DTSS_COLORARG1, D3DTA_TEXTURE);
		dev->SetTextureStageState(0, D3DTSS_COLOROP, D3DTOP_MODULATE);
		dev->SetRenderState(D3DRENDERSTATE_DESTBLEND, dstblend);
		dev->SetRenderState(D3DRENDERSTATE_ALPHABLENDENABLE, FALSE);
	}

	// render stars
	RenderStars(dev, (DWORD)-1, bglvl);

	// turn z-buffer back on
	dev->SetRenderState(D3DRENDERSTATE_ZENABLE, TRUE);
	dev->SetRenderState(D3DRENDERSTATE_ZVISIBLE, TRUE);
	dev->SetRenderState(D3DRENDERSTATE_ZWRITEENABLE, TRUE);
	dev->SetRenderState(D3DRENDERSTATE_LIGHTING, TRUE);
}

// ==============================================================

void OGCelestialSphere::RenderStars(LPDIRECT3DDEVICE7 dev, DWORD nmax, double bglvl)
{
	// render in chunks, because some graphics cards have a limit in the
	// vertex list size

	if (!m_nsVtx) return; // nothing to do

	DWORD i, j, ns = m_nsVtx;

	if (bglvl) { // suppress stars darker than the background
		int bgidx = min(255, pow(bglvl * 1.4, 0.75) * 255.0);
		ns = min((int)ns, m_lvlIdx[bgidx]);
	}

	for (i = j = 0; i < ns; i += D3DMAXNUMVERTICES, j++)
		dev->DrawPrimitiveVB(D3DPT_POINTLIST, m_sVtx[j], 0, min(ns - i, D3DMAXNUMVERTICES), 0);

}

// ==============================================================

void OGCelestialSphere::RenderConstellationLines(LPDIRECT3DDEVICE7 dev, const Vector& col)
{
	dev->SetRenderState(D3DRENDERSTATE_TEXTUREFACTOR, D3DRGBA(col.x, col.y, col.z, 1));
	dev->DrawPrimitiveVB(D3DPT_LINELIST, m_cVtx, 0, m_ncVtx, 0);
}

// ==============================================================

void OGCelestialSphere::RenderGreatCircle(LPDIRECT3DDEVICE7 dev, Vector& col)
{
	dev->SetRenderState(D3DRENDERSTATE_TEXTUREFACTOR, D3DRGBA(col.x, col.y, col.z, 1));
	dev->DrawPrimitiveVB(D3DPT_LINESTRIP, m_grdLngVtx, 5 * (NSEG + 1), NSEG + 1, 0);
}

// ==============================================================

void OGCelestialSphere::RenderGrid(LPDIRECT3DDEVICE7 dev, Vector& col, bool eqline)
{
	int i;
	dev->SetRenderState(D3DRENDERSTATE_TEXTUREFACTOR, D3DRGBA(col.x, col.y, col.z, 1));
	for (i = 0; i <= 10; i++) if (eqline || i != 5)
		dev->DrawPrimitiveVB(D3DPT_LINESTRIP, m_grdLngVtx, i * (NSEG + 1), NSEG + 1, 0);
	for (i = 0; i < 12; i++)
		dev->DrawPrimitiveVB(D3DPT_LINESTRIP, m_grdLatVtx, i * (NSEG + 1), NSEG + 1, 0);
}

// ==============================================================

void OGCelestialSphere::RenderBkgImage(LPDIRECT3DDEVICE7 dev, double bglvl)
{
	if (m_bkgImgMgr2) {
		VPlanet::RenderPrm rprm;
		memset(&rprm, 0, sizeof(VPlanet::RenderPrm));
		m_bkgImgMgr2->Render(dev, m_WMcsphere, 0, false, rprm);
	}
	else if (m_bkgImgMgr) {
		m_bkgImgMgr->Render(dev, 8, bglvl);
	}
}

// ==============================================================

bool OGCelestialSphere::EclDir2WindowPos(const VECTOR3& dir, int& x, int& y) const
{
	extern Camera* g_camera;
	D3DVECTOR homog;
	D3DVECTOR fdir = { (float)dir.x, (float)dir.y, (float)dir.z };

	D3DMath_VectorMatrixMultiply(homog, fdir, *g_camera->D3D_ProjViewMatrix());
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
