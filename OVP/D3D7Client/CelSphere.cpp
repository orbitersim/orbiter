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
#include "CSphereMgr.h"
#include "Scene.h"
#include "Camera.h"
#include "Texture.h"

#define NSEG 64 // number of segments in celestial grid lines

using namespace oapi;

// ==============================================================

D3D7CelestialSphere::D3D7CelestialSphere (D3D7Client* gc, Scene* scene)
	: oapi::CelestialSphere(gc)
	, m_gc(gc)
	, m_scene(scene)
{
	for (auto&& vtx : m_azGridLabelVtx)
		vtx = nullptr;
	m_elGridLabelVtx = nullptr;
	m_GridLabelIdx = nullptr;
	m_GridLabelTex = nullptr;

	InitStars();
	InitConstellationLines();
	InitConstellationBoundaries();
	AllocGrids();
	m_bkgImgMgr = new CSphereManager(gc, scene);

	m_mjdPrecessionChecked = -1e10;

	m_viewW = gc->GetViewW();
	m_viewH = gc->GetViewH();

	char cpath[256];
	m_gc->TexturePath("gridlabel.dds", cpath);
	if (FILE* f = fopen(cpath, "rb")) {
		if (FAILED(m_gc->GetTexMgr()->ReadTexture(f, &m_GridLabelTex, 0)))
			m_GridLabelTex = nullptr;
		fclose(f);
	}
	if (!m_GridLabelTex)
		oapiWriteLogError("Failed to load texture %s", cpath);
}

// ==============================================================

D3D7CelestialSphere::~D3D7CelestialSphere ()
{
	ClearStars();
	m_clVtx->Release();
	m_cbVtx->Release();
	m_grdLngVtx->Release();
	m_grdLatVtx->Release();

	for (auto vtx : m_azGridLabelVtx)
		if (vtx) vtx->Release();
	if (m_elGridLabelVtx)
		m_elGridLabelVtx->Release();
	if (m_GridLabelIdx)
		delete[]m_GridLabelIdx;
	if (m_GridLabelTex)
		m_GridLabelTex->Release();

	delete m_bkgImgMgr;
}

// ==============================================================

void D3D7CelestialSphere::InitCelestialTransform()
{
	m_rotCelestial = Ecliptic_CelestialAtEpoch();

	m_transformCelestial._11 = (float)m_rotCelestial.m11; m_transformCelestial._12 = (float)m_rotCelestial.m12; m_transformCelestial._13 = (float)m_rotCelestial.m13; m_transformCelestial._14 = 0.0f;
	m_transformCelestial._21 = (float)m_rotCelestial.m21; m_transformCelestial._22 = (float)m_rotCelestial.m22; m_transformCelestial._23 = (float)m_rotCelestial.m23; m_transformCelestial._24 = 0.0f;
	m_transformCelestial._31 = (float)m_rotCelestial.m31; m_transformCelestial._32 = (float)m_rotCelestial.m32; m_transformCelestial._33 = (float)m_rotCelestial.m33; m_transformCelestial._34 = 0.0f;
	m_transformCelestial._41 = 0.0f;                      m_transformCelestial._42 = 0.0f;                      m_transformCelestial._43 = 0.0f;                      m_transformCelestial._44 = 1.0f;

	m_mjdPrecessionChecked = oapiGetSimMJD();
}

// ==============================================================

bool D3D7CelestialSphere::LocalHorizonTransform(MATRIX3& R, D3DMATRIX& T)
{
	MATRIX3 rot;
	if (LocalHorizon_Ecliptic(rot)) {
		R = transp(rot);
		T = {
			(float)R.m11, (float)R.m12, (float)R.m13, 0.0f,
			(float)R.m21, (float)R.m22, (float)R.m23, 0.0f,
			(float)R.m31, (float)R.m32, (float)R.m33, 0.0f,
			0.0f,         0.0f,         0.0f,         1.0f
		};
		return true;
	}
	return false;
}

// ==============================================================

void D3D7CelestialSphere::InitStars()
{
	ClearStars();

	if (*(bool*)m_gc->GetConfigParam(CFGPRM_CSPHEREUSESTARDOTS)) {

		const std::vector<oapi::CelestialSphere::StarRenderRec> sList = LoadStars();
		m_nsVtx = sList.size();
		if (!m_nsVtx) return;

		const DWORD buflen = D3DMAXNUMVERTICES;
		DWORD i, j, nv, idx = 0;

		D3DVERTEXBUFFERDESC vbdesc;
		m_gc->SetDefault(vbdesc);
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
				const oapi::CelestialSphere::StarRenderRec& rec = sList[idx];
				VERTEX_XYZC& v = vbuf[j];
				v.x = (D3DVALUE)rec.pos.x;
				v.y = (D3DVALUE)rec.pos.y;
				v.z = (D3DVALUE)rec.pos.z;
				v.col = D3DRGBA(rec.col.x, rec.col.y, rec.col.z, 1);
				idx++;
			}
			(*it)->Unlock();
			(*it)->Optimize(m_gc->GetDevice(), 0);
		}

		m_starCutoffIdx = ComputeStarBrightnessCutoff(sList);

	}
}

// ==============================================================

void D3D7CelestialSphere::ClearStars()
{
	for (auto it = m_sVtx.begin(); it != m_sVtx.end(); it++)
		(*it)->Release();
	m_sVtx.clear();
	m_nsVtx = 0;
}

// ==============================================================

int D3D7CelestialSphere::MapLineBuffer(const std::vector<VECTOR3>& lineVtx, LPDIRECT3DVERTEXBUFFER7& buf) const
{
	size_t nv = lineVtx.size();
	if (!nv) return 0;

	// for now, we don't allow line sets exceeding the maximum buffer size
	if (nv > D3DMAXNUMVERTICES) {
		oapiWriteLogError("Celestial sphere: Number of line vertices in dataset too large (%d). Truncating to %d.", nv, D3DMAXNUMVERTICES);
		nv = D3DMAXNUMVERTICES;
	}

	// create vertex buffer
	D3DVERTEXBUFFERDESC vbdesc;
	vbdesc.dwSize = sizeof(D3DVERTEXBUFFERDESC);
	vbdesc.dwCaps = (m_gc->GetFramework()->IsTLDevice() ? 0 : D3DVBCAPS_SYSTEMMEMORY);
	vbdesc.dwFVF = D3DFVF_XYZ;
	vbdesc.dwNumVertices = nv;
	m_gc->GetDirect3D7()->CreateVertexBuffer(&vbdesc, &buf, 0);
	VERTEX_XYZ* vbuf;
	buf->Lock(DDLOCK_WAIT | DDLOCK_WRITEONLY | DDLOCK_DISCARDCONTENTS, (LPVOID*)&vbuf, NULL);
	for (size_t i = 0; i < nv; i++) {
		vbuf[i].x = (D3DVALUE)lineVtx[i].x;
		vbuf[i].y = (D3DVALUE)lineVtx[i].y;
		vbuf[i].z = (D3DVALUE)lineVtx[i].z;
	}
	buf->Unlock();
	buf->Optimize(m_gc->GetDevice(), 0);

	return nv;
}

// ==============================================================

void D3D7CelestialSphere::InitConstellationLines()
{
	m_nclVtx = MapLineBuffer(LoadConstellationLines(), m_clVtx);
}

// ==============================================================

void D3D7CelestialSphere::InitConstellationBoundaries()
{
	m_ncbVtx = MapLineBuffer(LoadConstellationBoundaries(), m_cbVtx);
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

void D3D7CelestialSphere::AllocGridLabels()
{
	struct VERTEX_XYZT {
		D3DVALUE x, y, z;
		D3DVALUE tu, tv;
	};

	const MESHHANDLE hMesh = GridLabelMesh();
	MESHGROUP* grp = oapiMeshGroup(hMesh, 0);

	D3DVERTEXBUFFERDESC vbdesc;
	vbdesc.dwSize = sizeof(D3DVERTEXBUFFERDESC);
	vbdesc.dwCaps = (m_gc->GetFramework()->IsTLDevice() ? 0 : D3DVBCAPS_SYSTEMMEMORY);
	vbdesc.dwFVF = D3DFVF_XYZ | D3DFVF_TEX1;
	vbdesc.dwNumVertices = grp->nVtx;

	// create vertex buffers for longitude labels (azimuth/hour angle/longitude)
	for (size_t idx = 0; idx < m_azGridLabelVtx.size(); idx++) {
		LPDIRECT3DVERTEXBUFFER7& vb = m_azGridLabelVtx[idx];
		m_gc->GetDirect3D7()->CreateVertexBuffer(&vbdesc, &vb, 0);
		VERTEX_XYZT* vbuf;
		vb->Lock(DDLOCK_WAIT | DDLOCK_WRITEONLY | DDLOCK_DISCARDCONTENTS, (LPVOID*)&vbuf, NULL);
		for (int i = 0; i < grp->nVtx; i++) {
			vbuf[i].x = (D3DVALUE)grp->Vtx[i].x;
			vbuf[i].y = (D3DVALUE)grp->Vtx[i].y;
			vbuf[i].z = (D3DVALUE)grp->Vtx[i].z;
			vbuf[i].tu = (D3DVALUE)(grp->Vtx[i].tu + idx * 0.1015625);
			vbuf[i].tv = (D3DVALUE)grp->Vtx[i].tv;
		}
		vb->Unlock();
		vb->Optimize(m_gc->GetDevice(), 0);
	}

	// the index list is used for both azimuth and elevation grid labels
	m_GridLabelIdx = new WORD[grp->nIdx];
	memcpy(m_GridLabelIdx, grp->Idx, grp->nIdx * sizeof(WORD));

	// create vertex buffer for latitude labels (just one shared between all grids)
	grp = oapiMeshGroup(hMesh, 1);
	vbdesc.dwNumVertices = grp->nVtx;
	m_gc->GetDirect3D7()->CreateVertexBuffer(&vbdesc, &m_elGridLabelVtx, 0);
	VERTEX_XYZT* vbuf;
	m_elGridLabelVtx->Lock(DDLOCK_WAIT | DDLOCK_WRITEONLY | DDLOCK_DISCARDCONTENTS, (LPVOID*)&vbuf, NULL);
	for (int i = 0; i < grp->nVtx; i++) {
		vbuf[i].x = (D3DVALUE)grp->Vtx[i].x;
		vbuf[i].y = (D3DVALUE)grp->Vtx[i].y;
		vbuf[i].z = (D3DVALUE)grp->Vtx[i].z;
		vbuf[i].tu = (D3DVALUE)grp->Vtx[i].tu;
		vbuf[i].tv = (D3DVALUE)grp->Vtx[i].tv;
	}
	m_elGridLabelVtx->Unlock();
	m_elGridLabelVtx->Optimize(m_gc->GetDevice(), 0);
}

// ==============================================================

void D3D7CelestialSphere::OnOptionChanged(DWORD cat, DWORD item)
{
	switch (cat) {
	case OPTCAT_CELSPHERE:
		switch (item) {
		case OPTITEM_CELSPHERE_ACTIVATESTARDOTS:
		case OPTITEM_CELSPHERE_STARDISPLAYPARAM:
			InitStars();
			break;
		case OPTITEM_CELSPHERE_ACTIVATESTARIMAGE:
		case OPTITEM_CELSPHERE_STARIMAGECHANGED:
		case OPTITEM_CELSPHERE_ACTIVATEBGIMAGE:
		case OPTITEM_CELSPHERE_BGIMAGECHANGED:
			delete m_bkgImgMgr;
			m_bkgImgMgr = new CSphereManager(m_gc, m_scene);
			break;
		case OPTITEM_CELSPHERE_BGIMAGEBRIGHTNESS:
			if (m_bkgImgMgr) {
				double intens = *(double*)m_gc->GetConfigParam(CFGPRM_CSPHEREINTENS);
				m_bkgImgMgr->SetBgBrightness(intens);
			}
			break;
		}
		break;
	}
}

// ==============================================================

void D3D7CelestialSphere::Render(LPDIRECT3DDEVICE7 dev, const VECTOR3 &skyCol)
{
	static D3DMATRIX ident;
	D3DMAT_Identity(&ident);

	SetSkyColour(skyCol);

	// Get celestial sphere render flags
	DWORD renderFlag = *(DWORD*)m_gc->GetConfigParam(CFGPRM_PLANETARIUMFLAG);

	// Turn off lighting calculations
	dev->SetRenderState(D3DRENDERSTATE_LIGHTING, FALSE);

	// celestial sphere background image
	RenderBkgImage(dev);

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

		// render ecliptic grid
		if (renderFlag & PLN_EGRID) {
			FVECTOR4 baseCol1(0.0f, 0.2f, 0.3f, 1.0f);
			RenderGrid(dev, baseCol1, false);
			FVECTOR4 baseCol2(0.0f, 0.4f, 0.6f, 1.0f);
			RenderGreatCircle(dev, baseCol2);
			double dphi = ElevationScaleRotation(_M(1, 0, 0, 0, 1, 0, 0, 0, 1));
			RenderGridLabels(dev, 2, baseCol2, dphi);
		}

		// render galactic grid
		if (renderFlag & PLN_GGRID) {
			FVECTOR4 baseCol1(0.3f, 0.0f, 0.0f, 1.0f);
			static const MATRIX3& R = Ecliptic_Galactic();
			static D3DMATRIX T = { (float)R.m11, (float)R.m12, (float)R.m13, 0.0f,
								   (float)R.m21, (float)R.m22, (float)R.m23, 0.0f,
								   (float)R.m31, (float)R.m32, (float)R.m33, 0.0f,
								   0.0f,         0.0f,         0.0f,         1.0f };
			dev->SetTransform(D3DTRANSFORMSTATE_WORLD, &T);
			RenderGrid(dev, baseCol1, false);
			FVECTOR4 baseCol2(0.7f, 0.0f, 0.0f, 1.0f);
			RenderGreatCircle(dev, baseCol2);
			double dphi = ElevationScaleRotation(R);
			RenderGridLabels(dev, 2, baseCol2, dphi);
			dev->SetTransform(D3DTRANSFORMSTATE_WORLD, &ident);
		}

		// render celestial grid
		if (renderFlag & PLN_CGRID) {
			if (fabs(m_mjdPrecessionChecked - oapiGetSimMJD()) > 1e3)
				InitCelestialTransform();
			dev->SetTransform(D3DTRANSFORMSTATE_WORLD, &m_transformCelestial);
			FVECTOR4 baseCol1(0.3f, 0.0f, 0.3f, 1.0f);
			RenderGrid(dev, baseCol1, false);
			FVECTOR4 baseCol2(0.7f, 0.0f, 0.7f, 1.0f);
			RenderGreatCircle(dev, baseCol2);
			double dphi = ElevationScaleRotation(m_rotCelestial);
			RenderGridLabels(dev, 1, baseCol2, dphi);
			dev->SetTransform(D3DTRANSFORMSTATE_WORLD, &ident);
		}

		//  render local horizon grid
		if (renderFlag & PLN_HGRID) {
			MATRIX3 R;
			D3DMATRIX T;
			if (LocalHorizonTransform(R, T)) {
				dev->SetTransform(D3DTRANSFORMSTATE_WORLD, &T);
				oapi::FVECTOR4 baseCol1(0.2f, 0.2f, 0.0f, 1.0f);
				RenderGrid(dev, baseCol1);
				oapi::FVECTOR4 baseCol2(0.5f, 0.5f, 0.0f, 1.0f);
				RenderGreatCircle(dev, baseCol2);
				double dphi = ElevationScaleRotation(R);
				RenderGridLabels(dev, 0, baseCol2, dphi);
				dev->SetTransform(D3DTRANSFORMSTATE_WORLD, &ident);
			}
		}

		// render equator of target celestial body
		if (renderFlag & PLN_EQU) {
			OBJHANDLE hRef = oapiCameraProxyGbody();
			if (hRef) {
				MATRIX3 R;
				oapiGetRotationMatrix(hRef, &R);
				D3DMATRIX iR = {
					(float)R.m11, (float)R.m21, (float)R.m31, 0.0f,
					(float)R.m12, (float)R.m22, (float)R.m32, 0.0f,
					(float)R.m13, (float)R.m23, (float)R.m33, 0.0f,
					0.0f,         0.0f,         0.0f,         1.0f
				};
				dev->SetTransform(D3DTRANSFORMSTATE_WORLD, &iR);
				FVECTOR4 baseCol(0.0f, 0.6f, 0.0f, 1.0f);
				RenderGreatCircle(dev, baseCol);
				dev->SetTransform(D3DTRANSFORMSTATE_WORLD, &ident);
			}
		}

		// render constellation boundaries
		if (renderFlag & PLN_CNSTBND) // for now, hijack the constellation line flag
			RenderConstellationBoundaries(dev);

		// render constellation lines
		if (renderFlag & PLN_CONST)
			RenderConstellationLines(dev);

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
	RenderStars(dev);

	// turn z-buffer back on
	dev->SetRenderState(D3DRENDERSTATE_LIGHTING, TRUE);
}

// ==============================================================

void D3D7CelestialSphere::RenderStars (LPDIRECT3DDEVICE7 dev)
{
	// render in chunks, because some graphics cards have a limit in the
	// vertex list size

	if (!m_nsVtx) return; // nothing to do

	DWORD i, j;
	int bgidx = min(255, (int)(GetSkyBrightness() * 256.0));
	int ns = m_starCutoffIdx[bgidx];

	for (i = j = 0; i < ns; i += D3DMAXNUMVERTICES, j++)
		dev->DrawPrimitiveVB (D3DPT_POINTLIST, m_sVtx[j], 0, min (ns-i, D3DMAXNUMVERTICES), 0);
}

// ==============================================================

void D3D7CelestialSphere::RenderConstellationLines (LPDIRECT3DDEVICE7 dev)
{
	const FVECTOR4 baseCol(0.5f, 0.3f, 0.2f, 1.0f);
	dev->SetRenderState(D3DRENDERSTATE_TEXTUREFACTOR, MarkerColorAdjusted(baseCol));
	dev->DrawPrimitiveVB (D3DPT_LINELIST, m_clVtx, 0, m_nclVtx, 0);
}

// ==============================================================

void D3D7CelestialSphere::RenderConstellationBoundaries(LPDIRECT3DDEVICE7 dev)
{
	const FVECTOR4 baseCol(0.25f, 0.2f, 0.15f, 1.0f);
	dev->SetRenderState(D3DRENDERSTATE_TEXTUREFACTOR, MarkerColorAdjusted(baseCol));
	dev->DrawPrimitiveVB(D3DPT_LINELIST, m_cbVtx, 0, m_ncbVtx, 0);
}

// ==============================================================

void D3D7CelestialSphere::RenderGreatCircle (LPDIRECT3DDEVICE7 dev, const FVECTOR4& baseCol)
{
	dev->SetRenderState (D3DRENDERSTATE_TEXTUREFACTOR, MarkerColorAdjusted(baseCol));
	dev->DrawPrimitiveVB (D3DPT_LINESTRIP, m_grdLngVtx, 5*(NSEG+1), NSEG+1, 0);
}

// ==============================================================

void D3D7CelestialSphere::RenderGrid (LPDIRECT3DDEVICE7 dev, const FVECTOR4& baseCol, bool eqline)
{
	int i;
	dev->SetRenderState (D3DRENDERSTATE_TEXTUREFACTOR, MarkerColorAdjusted(baseCol));
	for (i = 0; i <= 10; i++) if (eqline || i != 5)
		dev->DrawPrimitiveVB (D3DPT_LINESTRIP, m_grdLngVtx, i*(NSEG+1), NSEG+1, 0);
	for (i = 0; i < 12; i++)
		dev->DrawPrimitiveVB (D3DPT_LINESTRIP, m_grdLatVtx, i*(NSEG+1), NSEG+1, 0);
}

// ==============================================================

void D3D7CelestialSphere::RenderGridLabels(LPDIRECT3DDEVICE7 dev, int az_idx, const oapi::FVECTOR4& baseCol, double dphi)
{
	if (!m_GridLabelTex) return;
	if (az_idx >= m_azGridLabelVtx.size()) return;
	if (!m_azGridLabelVtx[az_idx])
		AllocGridLabels();
	dev->SetTexture(0, m_GridLabelTex);
	dev->DrawIndexedPrimitiveVB(D3DPT_TRIANGLELIST, m_azGridLabelVtx[az_idx], 0, 24 * 4, m_GridLabelIdx, 24 * 6, 0);

	D3DMATRIX R;
	if (dphi) {
		float cosp = (float)cos(dphi), sinp = (float)sin(dphi);
		D3DMATRIX R2;
		dev->GetTransform(D3DTRANSFORMSTATE_WORLD, &R);
		memcpy(&R2, &R, sizeof(D3DMATRIX));
		R2._11 = cosp * R._11 + sinp * R._31;
		R2._12 = cosp * R._12 + sinp * R._32;
		R2._13 = cosp * R._13 + sinp * R._33;
		R2._31 = -sinp * R._11 + cosp * R._31;
		R2._32 = -sinp * R._12 + cosp * R._32;
		R2._33 = -sinp * R._13 + cosp * R._33;
		dev->SetTransform(D3DTRANSFORMSTATE_WORLD, &R2);
	}

	dev->DrawIndexedPrimitiveVB(D3DPT_TRIANGLELIST, m_elGridLabelVtx, 0, 11 * 4, m_GridLabelIdx, 11 * 6, 0);
	dev->SetTexture(0, 0);

	if (dphi)
		dev->SetTransform(D3DTRANSFORMSTATE_WORLD, &R);
}

// ==============================================================

void D3D7CelestialSphere::RenderBkgImage(LPDIRECT3DDEVICE7 dev)
{
	m_bkgImgMgr->Render(dev, 8, GetSkyBrightness());
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
