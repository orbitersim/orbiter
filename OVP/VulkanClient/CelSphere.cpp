// ==============================================================
// CelSphere.cpp
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2006-2026 Martin Schweiger
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
#include "D3D9Surface.h"
#include "D3D9Effect.h"
#include "D3D9Pad.h"
#include "AABBUtil.h"

using std::min;

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
	for (auto&& vtx : m_azGridLabelVtx)
		vtx = nullptr;
	m_elGridLabelVtx = nullptr;
	m_GridLabelIdx = nullptr;
	m_GridLabelTex = nullptr;

	InitStars();
	InitConstellationLines();
	InitConstellationBoundaries();
	LoadConstellationLabels();
	AllocGrids();
	m_bkgImgMgr = new CSphereManager(gc, scene);

	m_textBlendAdditive = true;
	m_mjdPrecessionChecked = -1e10;

	SURFHANDLE hSurf = m_gc->clbkLoadTexture("gridlabel.dds", 0);
	if (hSurf) m_GridLabelTex = SURFACE(hSurf);
	if (!m_GridLabelTex)
		oapiWriteLogError("Failed to load texture gridlabel.dds");
}

// ==============================================================

D3D9CelestialSphere::~D3D9CelestialSphere()
{
	ClearStars();
	delete m_clVtx;
	delete m_cbVtx;
	delete m_grdLngVtx;
	delete m_grdLatVtx;

	for (auto vtx : m_azGridLabelVtx)
		if (vtx) delete vtx;
	if (m_elGridLabelVtx)
		delete m_elGridLabelVtx;
	if (m_GridLabelIdx)
		delete m_GridLabelIdx;

	delete m_bkgImgMgr;
	if (m_GridLabelTex)
		DELETE_SURFACE(m_GridLabelTex);
}

// ==============================================================

void D3D9CelestialSphere::InitCelestialTransform()
{
	m_rotCelestial = Ecliptic_CelestialAtEpoch();

	m_transformCelestial._11 = (float)m_rotCelestial.m11; m_transformCelestial._12 = (float)m_rotCelestial.m12; m_transformCelestial._13 = (float)m_rotCelestial.m13; m_transformCelestial._14 = 0.0f;
	m_transformCelestial._21 = (float)m_rotCelestial.m21; m_transformCelestial._22 = (float)m_rotCelestial.m22; m_transformCelestial._23 = (float)m_rotCelestial.m23; m_transformCelestial._24 = 0.0f;
	m_transformCelestial._31 = (float)m_rotCelestial.m31; m_transformCelestial._32 = (float)m_rotCelestial.m32; m_transformCelestial._33 = (float)m_rotCelestial.m33; m_transformCelestial._34 = 0.0f;
	m_transformCelestial._41 = 0.0f;                      m_transformCelestial._42 = 0.0f;                      m_transformCelestial._43 = 0.0f;                      m_transformCelestial._44 = 1.0f;

	m_mjdPrecessionChecked = oapiGetSimMJD();
}

// ==============================================================

bool D3D9CelestialSphere::LocalHorizonTransform(MATRIX3& R, D3DXMATRIX& T)
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

void D3D9CelestialSphere::InitStars ()
{
	ClearStars();

	if (*(bool*)m_gc->GetConfigParam(CFGPRM_CSPHEREUSESTARDOTS)) {

		const std::vector<oapi::CelestialSphere::StarRenderRec> sList = LoadStars();
		m_nsVtx = sList.size();
		if (!m_nsVtx) return;

		DWORD i, j, nv, idx = 0;

		// convert star database to vertex buffers
		DWORD nbuf = (m_nsVtx + maxNumVertices - 1) / maxNumVertices; // number of buffers required
		m_sVtx.resize(nbuf);
		for (auto it = m_sVtx.begin(); it != m_sVtx.end(); it++) {
			nv = min((DWORD)maxNumVertices, m_nsVtx - idx);
			*it = new VkBuf(m_pDevice, UINT(nv * sizeof(VERTEX_XYZC)), VK_BUFFER_USAGE_VERTEX_BUFFER_BIT, false); // D3DUSAGE_WRITEONLY, D3DPOOL_DEFAULT
			VERTEX_XYZC* vbuf = new VERTEX_XYZC[nv]; // Lock: filled here, uploaded at Unlock
			for (j = 0; j < nv; j++) {
				const oapi::CelestialSphere::StarRenderRec& rec = sList[idx];
				VERTEX_XYZC& v = vbuf[j];
				v.x = (float)rec.pos.x;
				v.y = (float)rec.pos.y;
				v.z = (float)rec.pos.z;
				v.col = D3DXCOLOR(rec.col.x, rec.col.y, rec.col.z, 1);
				idx++;
			}
			(*it)->Upload(vbuf, nv * sizeof(VERTEX_XYZC)); // Unlock
			delete[] vbuf;
		}

		m_starCutoffIdx = ComputeStarBrightnessCutoff(sList);

	}
}

// ==============================================================

void D3D9CelestialSphere::ClearStars()
{
	for (auto it = m_sVtx.begin(); it != m_sVtx.end(); it++)
		delete (*it);
	m_sVtx.clear();
	m_nsVtx = 0;
}

// ==============================================================

int D3D9CelestialSphere::MapLineBuffer(const std::vector<VECTOR3>& lineVtx, VkBuf*& buf) const
{
	size_t nv = lineVtx.size();
	if (!nv) return 0;

	// create vertex buffer
	buf = new VkBuf(m_pDevice, sizeof(VERTEX_XYZ) * nv, VK_BUFFER_USAGE_VERTEX_BUFFER_BIT, false); // D3DUSAGE_WRITEONLY, D3DPOOL_DEFAULT
	VERTEX_XYZ* vbuf = new VERTEX_XYZ[nv]; // Lock: filled here, uploaded at Unlock
	for (size_t i = 0; i < nv; i++) {
		vbuf[i].x = (float)lineVtx[i].x;
		vbuf[i].y = (float)lineVtx[i].y;
		vbuf[i].z = (float)lineVtx[i].z;
	}
	buf->Upload(vbuf, sizeof(VERTEX_XYZ) * nv); // Unlock
	delete[] vbuf;

	return nv;
}

// ==============================================================

void D3D9CelestialSphere::InitConstellationLines()
{
	m_nclVtx = MapLineBuffer(LoadConstellationLines(), m_clVtx);
}

// ==============================================================

void D3D9CelestialSphere::InitConstellationBoundaries()
{
	m_ncbVtx = MapLineBuffer(LoadConstellationBoundaries(), m_cbVtx);
}

// ==============================================================

void D3D9CelestialSphere::AllocGrids ()
{
	int i, j, idx;
	double lng, lat, xz, y;
	VERTEX_XYZ *vbuf;

	m_grdLngVtx = new VkBuf(m_pDevice, sizeof(VERTEX_XYZ)*(NSEG+1)*11, VK_BUFFER_USAGE_VERTEX_BUFFER_BIT, false); // D3DUSAGE_WRITEONLY, D3DPOOL_DEFAULT
	vbuf = new VERTEX_XYZ[(NSEG+1)*11]; // Lock: filled here, uploaded at Unlock
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
	m_grdLngVtx->Upload(vbuf, sizeof(VERTEX_XYZ)*(NSEG+1)*11); // Unlock
	delete[] vbuf;

	m_grdLatVtx = new VkBuf(m_pDevice, sizeof(VERTEX_XYZ)*(NSEG+1)*12, VK_BUFFER_USAGE_VERTEX_BUFFER_BIT, false); // D3DUSAGE_WRITEONLY, D3DPOOL_DEFAULT
	vbuf = new VERTEX_XYZ[(NSEG+1)*12]; // Lock: filled here, uploaded at Unlock
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
	m_grdLatVtx->Upload(vbuf, sizeof(VERTEX_XYZ)*(NSEG+1)*12); // Unlock
	delete[] vbuf;
}

// ==============================================================

void D3D9CelestialSphere::AllocGridLabels()
{
	VERTEX_XYZ_TEX* vbuf;

	const MESHHANDLE hMesh = GridLabelMesh();
	MESHGROUP* grp = oapiMeshGroup(hMesh, 0);

	// create vertex buffers for longitude labels (azimuth/hour angle/longitude)
	for (size_t idx = 0; idx < m_azGridLabelVtx.size(); idx++) {
		VkBuf*& vb = m_azGridLabelVtx[idx];
		vb = new VkBuf(m_pDevice, sizeof(VERTEX_XYZ_TEX) * grp->nVtx, VK_BUFFER_USAGE_VERTEX_BUFFER_BIT, false); // D3DUSAGE_WRITEONLY, D3DPOOL_DEFAULT
		vbuf = new VERTEX_XYZ_TEX[grp->nVtx]; // Lock: filled here, uploaded at Unlock
		for (int i = 0; i < grp->nVtx; i++) {
			vbuf[i].x = (float)grp->Vtx[i].x;
			vbuf[i].y = (float)grp->Vtx[i].y;
			vbuf[i].z = (float)grp->Vtx[i].z;
			vbuf[i].tu = (float)(grp->Vtx[i].tu + idx * 0.1015625);
			vbuf[i].tv = (float)grp->Vtx[i].tv;
		}
		vb->Upload(vbuf, sizeof(VERTEX_XYZ_TEX) * grp->nVtx); // Unlock
		delete[] vbuf;
	}

	// the index list is used for both azimuth and elevation grid labels
	m_GridLabelIdx = new VkBuf(m_pDevice, sizeof(WORD) * grp->nIdx, VK_BUFFER_USAGE_INDEX_BUFFER_BIT, false); // D3DFMT_INDEX16
	m_GridLabelIdx->Upload(grp->Idx, sizeof(WORD) * grp->nIdx); // Lock/memcpy/Unlock

	// create vertex buffer for latitude labels (just one shared between all grids)
	grp = oapiMeshGroup(hMesh, 1);
	m_elGridLabelVtx = new VkBuf(m_pDevice, sizeof(VERTEX_XYZ_TEX) * grp->nVtx, VK_BUFFER_USAGE_VERTEX_BUFFER_BIT, false); // D3DUSAGE_WRITEONLY, D3DPOOL_DEFAULT
	vbuf = new VERTEX_XYZ_TEX[grp->nVtx]; // Lock: filled here, uploaded at Unlock
	for (int i = 0; i < grp->nVtx; i++) {
		vbuf[i].x = (float)grp->Vtx[i].x;
		vbuf[i].y = (float)grp->Vtx[i].y;
		vbuf[i].z = (float)grp->Vtx[i].z;
		vbuf[i].tu = (float)grp->Vtx[i].tu;
		vbuf[i].tv = (float)grp->Vtx[i].tv;
	}
	m_elGridLabelVtx->Upload(vbuf, sizeof(VERTEX_XYZ_TEX) * grp->nVtx); // Unlock
	delete[] vbuf;
}

// ==============================================================

void D3D9CelestialSphere::OnOptionChanged(DWORD cat, DWORD item)
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

void D3D9CelestialSphere::Render(VkDev *pDevice, const VECTOR3& skyCol)
{
	SetSkyColour(skyCol);

	// Get celestial sphere render flags
	DWORD renderFlag = *(DWORD*)m_gc->GetConfigParam(CFGPRM_PLANETARIUMFLAG);

	// celestial sphere background image
	RenderBkgImage(pDevice);

	if (renderFlag & PLN_ENABLE) {

		HR(s_FX->SetTechnique(s_eLine));
		HR(s_FX->SetMatrix(s_eWVP, m_scene->GetProjectionViewMatrix()));

		// render ecliptic grid
		if (renderFlag & PLN_EGRID) {
			FVECTOR4 baseCol1(0.0f, 0.2f, 0.3f, 1.0f);
			D3DXVECTOR4 vColor1 = ColorAdjusted(baseCol1);
			HR(s_FX->SetVector(s_eColor, &vColor1));
			RenderGrid(s_FX, false);
			FVECTOR4 baseCol2(0.0f, 0.4f, 0.6f, 1.0f);
			D3DXVECTOR4 vColor2 = ColorAdjusted(baseCol2);
			HR(s_FX->SetVector(s_eColor, &vColor2));
			RenderGreatCircle(s_FX);
			MATRIX3 ident = _M(1, 0, 0, 0, 1, 0, 0, 0, 1);
			double dphi = ElevationScaleRotation(ident);
			RenderGridLabels(s_FX, 2, baseCol2, ident, dphi);
		}

		// render galactic grid
		if (renderFlag & PLN_GGRID) {
			static const MATRIX3& R = Ecliptic_Galactic();
			static D3DXMATRIX T = { (float)R.m11, (float)R.m12, (float)R.m13, 0.0f,
						 		    (float)R.m21, (float)R.m22, (float)R.m23, 0.0f,
								    (float)R.m31, (float)R.m32, (float)R.m33, 0.0f,
								    0.0f,         0.0f,         0.0f,         1.0f };
			D3DXMATRIX rot;
			D3DXMatrixMultiply(&rot, &T, m_scene->GetProjectionViewMatrix());
			HR(s_FX->SetMatrix(s_eWVP, &rot));
			FVECTOR4 baseCol1(0.3f, 0.0f, 0.0f, 1.0f);
			D3DXVECTOR4 vColor1 = ColorAdjusted(baseCol1);
			HR(s_FX->SetVector(s_eColor, &vColor1));
			RenderGrid(s_FX, false);
			FVECTOR4 baseCol2(0.7f, 0.0f, 0.0f, 1.0f);
			D3DXVECTOR4 vColor2 = ColorAdjusted(baseCol2);
			HR(s_FX->SetVector(s_eColor, &vColor2));
			RenderGreatCircle(s_FX);
			double dphi = ElevationScaleRotation(R);
			RenderGridLabels(s_FX, 2, baseCol2, R, dphi);
		}

		// render celestial grid
		if (renderFlag & PLN_CGRID) {
			if (fabs(m_mjdPrecessionChecked - oapiGetSimMJD()) > 1e3)
				InitCelestialTransform();
			D3DXMATRIX rot;
			D3DXMatrixMultiply(&rot, &m_transformCelestial, m_scene->GetProjectionViewMatrix());
			HR(s_FX->SetMatrix(s_eWVP, &rot));
			FVECTOR4 baseCol1(0.3f, 0.0f, 0.3f, 1.0f);
			D3DXVECTOR4 vColor1 = ColorAdjusted(baseCol1);
			HR(s_FX->SetVector(s_eColor, &vColor1));
			RenderGrid(s_FX, false);
			FVECTOR4 baseCol2(0.7f, 0.0f, 0.7f, 1.0f);
			D3DXVECTOR4 vColor2 = ColorAdjusted(baseCol2);
			HR(s_FX->SetVector(s_eColor, &vColor2));
			RenderGreatCircle(s_FX);
			double dphi = ElevationScaleRotation(m_rotCelestial);
			RenderGridLabels(s_FX, 1, baseCol2, m_rotCelestial, dphi);
		}

		//  render local horizon grid
		if (renderFlag & PLN_HGRID) {
			MATRIX3 R;
			D3DXMATRIX T, rot;
			if (LocalHorizonTransform(R, T)) {
				D3DXMatrixMultiply(&rot, &T, m_scene->GetProjectionViewMatrix());
				HR(s_FX->SetMatrix(s_eWVP, &rot));
				oapi::FVECTOR4 baseCol1(0.2f, 0.2f, 0.0f, 1.0f);
				D3DXVECTOR4 vColor1 = ColorAdjusted(baseCol1);
				HR(s_FX->SetVector(s_eColor, &vColor1));
				RenderGrid(s_FX, false);
				oapi::FVECTOR4 baseCol2(0.5f, 0.5f, 0.0f, 1.0f);
				D3DXVECTOR4 vColor2 = ColorAdjusted(baseCol2);
				HR(s_FX->SetVector(s_eColor, &vColor2));
				RenderGreatCircle(s_FX);
				double dphi = ElevationScaleRotation(R);
				RenderGridLabels(s_FX, 0, baseCol2, R, dphi);
			}
		}

		// render equator of target celestial body
		if (renderFlag & PLN_EQU) {
			OBJHANDLE hRef = oapiCameraProxyGbody();
			if (hRef) {
				MATRIX3 R;
				oapiGetRotationMatrix(hRef, &R);
				D3DXMATRIX iR = {
					(float)R.m11, (float)R.m21, (float)R.m31, 0.0f,
					(float)R.m12, (float)R.m22, (float)R.m32, 0.0f,
					(float)R.m13, (float)R.m23, (float)R.m33, 0.0f,
					0.0f,         0.0f,         0.0f,         1.0f
				};
				D3DXMATRIX rot;
				D3DXMatrixMultiply(&rot, &iR, m_scene->GetProjectionViewMatrix());
				HR(s_FX->SetMatrix(s_eWVP, &rot));
				FVECTOR4 baseCol(0.0f, 0.6f, 0.0f, 1.0f);
				D3DXVECTOR4 vColor = ColorAdjusted(baseCol);
				HR(s_FX->SetVector(s_eColor, &vColor));
				RenderGreatCircle(s_FX);
			}
		}

		// render constellation boundaries ----------------------------------------
		if (renderFlag & PLN_CNSTBND) { // for now, hijack the constellation line flag
			HR(s_FX->SetMatrix(s_eWVP, m_scene->GetProjectionViewMatrix()));
			RenderConstellationBoundaries(s_FX);
		}

		// render constellation lines ---------------------------------------------
		if (renderFlag & PLN_CONST) {
			HR(s_FX->SetMatrix(s_eWVP, m_scene->GetProjectionViewMatrix()));
			RenderConstellationLines(s_FX);
		}
	}

	// render stars
	HR(s_FX->SetTechnique(s_eStar));
	HR(s_FX->SetMatrix(s_eWVP, m_scene->GetProjectionViewMatrix()));
	RenderStars(s_FX);

	// render markers and labels
	if (renderFlag & PLN_ENABLE) {

		// Sketchpad for planetarium mode labels and markers
		D3D9Pad* pSketch = m_scene->GetPooledSketchpad(0);

		if (pSketch) {

			// constellation labels --------------------------------------------------
			if (renderFlag & PLN_CNSTLABEL) {
				RenderConstellationLabels((oapi::Sketchpad**)&pSketch, renderFlag & PLN_CNSTLONG);
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

void D3D9CelestialSphere::RenderStars(VkEffect *FX)
{
	_TRACE;

	if (!m_nsVtx) return; // nothing to do

	// render in chunks, because some graphics cards have a limit in the
	// vertex list size
	UINT i, j, numPasses = 0;
	int bgidx = min(255, (int)(GetSkyBrightness() * 256.0));
	int ns = m_starCutoffIdx[bgidx];

	m_pDevice->SetVertexDecl(pPosColorDecl);
	HR(FX->Begin(&numPasses, VKFX_DONOTSAVESTATE));
	HR(FX->BeginPass(0));
	for (i = j = 0; i < ns; i += maxNumVertices, j++) {
		m_pDevice->SetStreamSource(0, m_sVtx[j], 0, sizeof(VERTEX_XYZC));
		m_pDevice->DrawPrimitive(VK_PRIMITIVE_TOPOLOGY_POINT_LIST, 0, VkPrimVerts(VK_PRIMITIVE_TOPOLOGY_POINT_LIST, min (ns-i, maxNumVertices)));
	}
	HR(FX->EndPass());
	HR(FX->End());	
}

// ==============================================================

void D3D9CelestialSphere::RenderConstellationLines(VkEffect *FX)
{
	const FVECTOR4 baseCol(0.5f, 0.3f, 0.2f, 1.0f);
	D3DXVECTOR4 vColor = ColorAdjusted(baseCol);
	HR(s_FX->SetVector(s_eColor, &vColor));

	_TRACE;
	UINT numPasses = 0;
	HR(FX->Begin(&numPasses, VKFX_DONOTSAVESTATE));
	HR(FX->BeginPass(0));
	m_pDevice->SetStreamSource(0, m_clVtx, 0, sizeof(VERTEX_XYZ));
	m_pDevice->SetVertexDecl(pPositionDecl);
	m_pDevice->DrawPrimitive(VK_PRIMITIVE_TOPOLOGY_LINE_LIST, 0, VkPrimVerts(VK_PRIMITIVE_TOPOLOGY_LINE_LIST, m_nclVtx));
	HR(FX->EndPass());
	HR(FX->End());	
}

// ==============================================================

void D3D9CelestialSphere::RenderConstellationBoundaries(VkEffect *FX)
{
	const FVECTOR4 baseCol(0.25f, 0.2f, 0.15f, 1.0f);
	D3DXVECTOR4 vColor = ColorAdjusted(baseCol);
	HR(s_FX->SetVector(s_eColor, &vColor));

	_TRACE;
	UINT numPasses = 0;
	HR(FX->Begin(&numPasses, VKFX_DONOTSAVESTATE));
	HR(FX->BeginPass(0));
	m_pDevice->SetStreamSource(0, m_cbVtx, 0, sizeof(VERTEX_XYZ));
	m_pDevice->SetVertexDecl(pPositionDecl);
	m_pDevice->DrawPrimitive(VK_PRIMITIVE_TOPOLOGY_LINE_LIST, 0, VkPrimVerts(VK_PRIMITIVE_TOPOLOGY_LINE_LIST, m_ncbVtx));
	HR(FX->EndPass());
	HR(FX->End());
}

// ==============================================================

void D3D9CelestialSphere::RenderGreatCircle(VkEffect *FX)
{
	_TRACE;
	UINT numPasses = 0;
	HR(FX->Begin(&numPasses, VKFX_DONOTSAVESTATE));
	HR(FX->BeginPass(0));
	m_pDevice->SetStreamSource(0, m_grdLngVtx, 0, sizeof(VERTEX_XYZ));
	m_pDevice->SetVertexDecl(pPositionDecl);
	m_pDevice->DrawPrimitive(VK_PRIMITIVE_TOPOLOGY_LINE_STRIP, 5*(NSEG+1), VkPrimVerts(VK_PRIMITIVE_TOPOLOGY_LINE_STRIP, NSEG));
	HR(FX->EndPass());
	HR(FX->End());	
	
}

// ==============================================================

void D3D9CelestialSphere::RenderGrid(VkEffect *FX, bool eqline)
{
	_TRACE;
	int i;
	UINT numPasses = 0;
	m_pDevice->SetVertexDecl(pPositionDecl);
	m_pDevice->SetStreamSource(0, m_grdLngVtx, 0, sizeof(VERTEX_XYZ));
	HR(FX->Begin(&numPasses, VKFX_DONOTSAVESTATE));
	HR(FX->BeginPass(0));
	for (i = 0; i <= 10; i++)
		if (eqline || i != 5)
			m_pDevice->DrawPrimitive(VK_PRIMITIVE_TOPOLOGY_LINE_STRIP, i*(NSEG+1), VkPrimVerts(VK_PRIMITIVE_TOPOLOGY_LINE_STRIP, NSEG));
	m_pDevice->SetStreamSource(0, m_grdLatVtx, 0, sizeof(VERTEX_XYZ));
	for (i = 0; i < 12; i++)
		m_pDevice->DrawPrimitive(VK_PRIMITIVE_TOPOLOGY_LINE_STRIP, i * (NSEG+1), VkPrimVerts(VK_PRIMITIVE_TOPOLOGY_LINE_STRIP, NSEG));
	HR(FX->EndPass());
	HR(FX->End());	
}

// ==============================================================

void D3D9CelestialSphere::RenderGridLabels(VkEffect *FX, int az_idx, const oapi::FVECTOR4& baseCol, const MATRIX3& R, double dphi)
{
	if (!m_GridLabelTex) return;
	if (az_idx >= m_azGridLabelVtx.size()) return;
	if (!m_azGridLabelVtx[az_idx])
		AllocGridLabels();

	UINT numPasses = 0;
	m_pDevice->SetVertexDecl(pPosTexDecl);
	m_pDevice->SetStreamSource(0, m_azGridLabelVtx[az_idx], 0, sizeof(VERTEX_XYZ_TEX));
	m_pDevice->SetIndices(m_GridLabelIdx);
	HR(FX->SetTechnique(s_eLabel));
	HR(FX->Begin(&numPasses, VKFX_DONOTSAVESTATE));
	HR(FX->BeginPass(0));
	HR(FX->SetTexture(FX->GetParameterByName(0, "gTex0"), m_GridLabelTex->GetTexture())); // SetTexture(0,..): s0 is Tex0S, read from gTex0
	HR(FX->CommitChanges());
	m_pDevice->DrawIndexedPrimitive(VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST, 0, 0, VkPrimVerts(VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST, 24 * 2));
	HR(FX->EndPass());
	HR(FX->End());

	D3DXMATRIX T0, T1;
	if (dphi) {
		FX->GetMatrix(s_eWVP, &T0);
		double cosp = cos(dphi), sinp = sin(dphi);
		D3DXMATRIX R2 = {
			(float)(cosp * R.m11 + sinp * R.m31),  (float)(cosp * R.m12 + sinp * R.m32),  (float)(cosp * R.m13 + sinp * R.m33),  0.0f,
			(float)R.m21,                          (float)R.m22,                          (float)R.m23,                          0.0f,
			(float)(-sinp * R.m11 + cosp * R.m31), (float)(-sinp * R.m12 + cosp * R.m32), (float)(-sinp * R.m13 + cosp * R.m33), 0.0f,
			0.0f,                                  0.0f,                                  0.0f,                                  1.0f
		};
		D3DXMatrixMultiply(&T1, &R2, m_scene->GetProjectionViewMatrix());
		FX->SetMatrix(s_eWVP, &T1);
	}

	m_pDevice->SetStreamSource(0, m_elGridLabelVtx, 0, sizeof(VERTEX_XYZ_TEX));
	HR(FX->Begin(&numPasses, VKFX_DONOTSAVESTATE));
	HR(FX->BeginPass(0));
	HR(FX->SetTexture(FX->GetParameterByName(0, "gTex0"), m_GridLabelTex->GetTexture())); // SetTexture(0,..): s0 is Tex0S, read from gTex0
	HR(FX->CommitChanges());
	m_pDevice->DrawIndexedPrimitive(VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST, 0, 0, VkPrimVerts(VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST, 11 * 2));
	HR(FX->EndPass());
	HR(FX->End());

	HR(FX->SetTechnique(s_eLine)); // Restore default tech

	if (dphi)
		FX->SetMatrix(s_eWVP, &T0);
}

// ==============================================================

void D3D9CelestialSphere::RenderBkgImage(VkDev *dev)
{
	m_bkgImgMgr->Render(dev, 8, GetSkyBrightness());
	dev->SetCullMode(VK_CULL_MODE_BACK_BIT);
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

		if (std::hypot(homog.x, homog.y) < 1e-6) {
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

void D3D9CelestialSphere::D3D9TechInit(VkEffect *fx)
{
	s_FX = fx;
	s_eStar = fx->GetTechniqueByName("StarTech");
	s_eLine = fx->GetTechniqueByName("LineTech");
	s_eLabel = fx->GetTechniqueByName("LabelTech");
	s_eColor = fx->GetParameterByName(0, "gColor");
	s_eWVP = fx->GetParameterByName(0, "gWVP");
}

VkEffect *D3D9CelestialSphere::s_FX = 0;
VkFxHandle D3D9CelestialSphere::s_eStar = 0;
VkFxHandle D3D9CelestialSphere::s_eLine = 0;
VkFxHandle D3D9CelestialSphere::s_eLabel = 0;
VkFxHandle D3D9CelestialSphere::s_eColor = 0;
VkFxHandle D3D9CelestialSphere::s_eWVP = 0;

