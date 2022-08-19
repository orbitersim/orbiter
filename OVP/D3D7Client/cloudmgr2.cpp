// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//   ORBITER VISUALISATION PROJECT (OVP)
//   D3D7 Client module
// ==============================================================

// ==============================================================
// cloudmgr2.cpp
// Rendering of planetary cloud layers, engine v2, including a simple
// LOD (level-of-detail) algorithm for cloud patch resolution.
// ==============================================================

#include "cloudmgr2.h"
#include "Texture.h"
#include "Camera.h"

// =======================================================================
// =======================================================================

CloudTile::CloudTile (TileManager2Base *_mgr, int _lvl, int _ilat, int _ilng)
: Tile (_mgr, _lvl, _ilat, _ilng)
{
	cmgr = static_cast<TileManager2<CloudTile>* > (_mgr);
	node = 0;
	mean_elev = mgr->GetPlanet()->prm.cloudalt;
}

// -----------------------------------------------------------------------

CloudTile::~CloudTile ()
{
}

// -----------------------------------------------------------------------

void CloudTile::Load ()
{
	bool bLoadMip = true; // for now
	bool ok = false;

	DWORD flag = (bLoadMip ? 0:4);
	char path[256];

	// Load cloud texture
	owntex = true;
	if (mgr->Cprm().tileLoadFlags & 0x0001) { // try loading from individual tile file
		sprintf (path, "%s\\Cloud\\%02d\\%06d\\%06d.dds", mgr->CbodyName(), lvl+4, ilat, ilng);
		ok = (mgr->GClient()->GetTexMgr()->LoadTexture (path, &tex, flag) == S_OK);
	}
	if (!ok && cmgr->ZTreeManager(0)) { // try loading from compressed archive
		BYTE *buf;
		DWORD ndata = cmgr->ZTreeManager(0)->ReadData(lvl+4, ilat, ilng, &buf);
		if (ndata) {
			ok = (mgr->GClient()->GetTexMgr()->ReadTextureFromMemory (buf, ndata, &tex, flag) == S_OK);
			cmgr->ZTreeManager(0)->ReleaseData(buf);
		}
	}
	if (!ok) { // no texture found - interpolate subregion from ancestor
		if (GetParentSubTexRange (&texrange)) {
			tex = getParent()->Tex();
			owntex = false;
		} else
			tex = 0;
	}

	bool shift_origin = (lvl >= 4);
	int res = mgr->GridRes();

	if (!lvl) {
		// create hemisphere mesh for western or eastern hemispheres
		mesh = CreateMesh_hemisphere (res, 0, mean_elev);
	//} else if (ilat == 0 || ilat == (1<<lvl)-1) {
		// create triangular patch for north/south pole region
	//	mesh = CreateMesh_tripatch (TILE_PATCHRES, elev, shift_origin, &vtxshift);
	} else {
		// create rectangular patch
		mesh = CreateMesh_quadpatch (res, res, 0, 1.0, mean_elev, &texrange, shift_origin, &vtxshift);
	}
}

// -----------------------------------------------------------------------

void CloudTile::Render ()
{
	TileManager2<CloudTile>::Dev()->SetTexture (0, tex);

	LPDIRECT3DVERTEXBUFFER7 vb = mesh->vb;        // processed vertex buffer
	TileManager2<CloudTile>::Dev()->DrawIndexedPrimitiveVB (D3DPT_TRIANGLELIST, vb, 0,
		mesh->nv, mesh->idx, mesh->ni, 0);
}


// =======================================================================
// =======================================================================

template<>
void TileManager2<CloudTile>::SetRenderPrm(MATRIX4 &dwmat, double prerot, bool use_zbuf, const vPlanet::RenderPrm &rprm)
{
	TileManager2Base::SetRenderPrm(dwmat, prerot, use_zbuf, rprm);
	double cloudrad = 1.0 + rprm.cloudalt / obj_size;
	if (prm.cdist < cloudrad) {  // camera is below cloud layer - clouds rendered from below
		prm.viewap += acos(rprm.horizon_minrad / cloudrad);  // extend visibility radius to planet horizon
	}
}

template<>
void TileManager2<CloudTile>::Render (MATRIX4 &dwmat, bool use_zbuf, const vPlanet::RenderPrm &rprm)
{
	// set generic parameters
	SetRenderPrm (dwmat, rprm.cloudrot, use_zbuf, rprm);

	double np = 0.0, fp = 0.0;
	int i;
	Camera *camera = GClient()->GetScene()->GetCamera();

	if (rprm.bCloudBrighten)
		TileManager2Base::Dev()->SetTextureStageState (0, D3DTSS_COLOROP, D3DTOP_MODULATE2X);

	// adjust scaling parameters (can only be done if no z-buffering is in use)
	if (!use_zbuf) {
		double R = obj_size;
		double Rc = R+rprm.cloudalt;
		R *= rprm.horizon_minrad;
		double D = prm.cdist*R;
		double zmin, zmax;
		if (D > Rc) {
			zmax = sqrt(D*D-Rc*Rc);
			zmin = D-Rc;
		} else {
			zmax = sqrt(D*D-R*R) + sqrt(Rc*Rc-R*R);
			zmin = Rc-D;
		}
		zmin = max (2.0, min (zmax*1e-4, zmin));

		np = camera->GetNearlimit();
		fp = camera->GetFarlimit();
		camera->SetFrustumLimits (zmin, zmax);
	}

	// build a transformation matrix for frustum testing
	MATRIX4 Mproj = camera->ProjectionMatrix();
	Mproj.m33 = 1.0; Mproj.m43 = -1.0;  // adjust near plane to 1, far plane to infinity
	MATRIX4 Mview = camera->ViewMatrix();
	prm.dviewproj = mul(Mview,Mproj);

	// TODO: render full sphere for levels < 4

	loader->WaitForMutex();

	// update the tree
	for (i = 0; i < 2; i++)
		ProcessNode (tiletree+i);

	// render the tree
	Dev()->SetTextureStageState (0, D3DTSS_ADDRESS, D3DTADDRESS_CLAMP);
	for (i = 0; i < 2; i++)
		RenderNode (tiletree+i);
	Dev()->SetTextureStageState (0, D3DTSS_ADDRESS, D3DTADDRESS_WRAP);

	loader->ReleaseMutex ();

	if (rprm.bCloudBrighten)
		TileManager2Base::Dev()->SetTextureStageState (0, D3DTSS_COLOROP, D3DTOP_MODULATE);

	if (np)
		camera->SetFrustumLimits(np,fp);
}

// -----------------------------------------------------------------------

template<>
void TileManager2<CloudTile>::RenderFlatCloudShadows (MATRIX4 &dwmat, const vPlanet::RenderPrm &rprm)
{
	double scale = obj_size/(obj_size+GetPlanet()->prm.cloudalt);
	MATRIX4 dwmat_scaled = {
		dwmat.m11*scale, dwmat.m12*scale, dwmat.m13*scale, dwmat.m14*scale,
		dwmat.m21*scale, dwmat.m22*scale, dwmat.m23*scale, dwmat.m24*scale,
		dwmat.m31*scale, dwmat.m32*scale, dwmat.m33*scale, dwmat.m34*scale,
		dwmat.m41,       dwmat.m42,       dwmat.m43,       dwmat.m44};
	SetRenderPrm (dwmat_scaled, rprm.cloudrot, false, rprm);
	prm.grot *= scale;

	D3DMATERIAL7 pmat;
	static D3DMATERIAL7 cloudmat = {{0,0,0,1},{0,0,0,1},{0,0,0,0},{0,0,0,0},0};
	
	float alpha = (float)rprm.shadowalpha;
	if (alpha < 0.01f) return; // don't render cloud shadows for this planet
	cloudmat.diffuse.a = cloudmat.ambient.a = alpha;

	Dev()->GetMaterial (&pmat);
	Dev()->SetMaterial (&cloudmat);

	Dev()->SetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, TRUE);
	Dev()->SetTextureStageState (0, D3DTSS_ALPHAOP, D3DTOP_MODULATE);
	
	double np = 0.0, fp = 0.0;
	int i;

	Camera *camera = GClient()->GetScene()->GetCamera();
	double R = obj_size;
	double D = prm.cdist*R;
	double zmax = sqrt(D*D-R*R);
	double zmin = max (2.0, min (zmax*1e-4, (D-R)*0.8));
	np = camera->GetNearlimit();
	fp = camera->GetFarlimit();
	camera->SetFrustumLimits (zmin, zmax);

	// build a transformation matrix for frustum testing
	MATRIX4 Mproj = camera->ProjectionMatrix();
	Mproj.m33 = 1.0; Mproj.m43 = -1.0;  // adjust near plane to 1, far plane to infinity
	MATRIX4 Mview = camera->ViewMatrix();
	prm.dviewproj = mul(Mview,Mproj);

	// TODO: render full sphere for levels < 4

	loader->WaitForMutex();

	// render the tree
	Dev()->SetTextureStageState (0, D3DTSS_ADDRESS, D3DTADDRESS_CLAMP);
	for (i = 0; i < 2; i++)
		RenderNode (tiletree+i);
	Dev()->SetTextureStageState (0, D3DTSS_ADDRESS, D3DTADDRESS_WRAP);

	loader->ReleaseMutex ();

	if (np)
		camera->SetFrustumLimits(np,fp);

	Dev()->SetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, FALSE);
	Dev()->SetTextureStageState (0, D3DTSS_ALPHAOP, D3DTOP_SELECTARG1);

	Dev()->SetMaterial (&pmat);
}

// -----------------------------------------------------------------------

template<>
int TileManager2<CloudTile>::Coverage (double latmin, double latmax, double lngmin, double lngmax, int maxlvl, const Tile **tbuf, int nt) const
{
	double crot = GetPlanet()->prm.cloudrot;
	//double crot = prm.rprm->cloudrot;
	lngmin = lngmin + crot;
	lngmax = lngmax + crot;
	if (lngmin > PI) {
		lngmin -= PI2;
		lngmax -= PI2;
	}
	int nfound = 0;
	for (int i = 0; i < 2; i++) {
		CheckCoverage (tiletree+i, latmin, latmax, lngmin, lngmax, maxlvl, tbuf, nt, &nfound);
	}
	return nfound;
}

// -----------------------------------------------------------------------

template<>
void TileManager2<CloudTile>::LoadZTrees()
{
	treeMgr = new ZTreeMgr*[ntreeMgr = 1];
	if (cprm.tileLoadFlags & 0x0002) {
		treeMgr[0] = ZTreeMgr::CreateFromFile(m_dataRootDir.c_str(), ZTreeMgr::LAYER_CLOUD);
	} else
		treeMgr[0] = 0;
}
