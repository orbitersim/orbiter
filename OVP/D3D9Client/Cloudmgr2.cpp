// ==============================================================
//   ORBITER VISUALISATION PROJECT (OVP)
//   Copyright (C) 2006-2016 Martin Schweiger
//   Dual licensed under GPL v3 and LGPL v3
// ==============================================================

// ==============================================================
// cloudmgr2.cpp
// Rendering of planetary cloud layers, engine v2, including a simple
// LOD (level-of-detail) algorithm for cloud patch resolution.
// ==============================================================

#include "cloudmgr2.h"
#include "D3D9Catalog.h"
#include "D3D9Config.h"

// =======================================================================
// =======================================================================

CloudTile::CloudTile (TileManager2Base *_mgr, int _lvl, int _ilat, int _ilng)
: Tile (_mgr, _lvl, _ilat, _ilng)
{
	cmgr = static_cast<TileManager2<CloudTile>* > (_mgr);
	node = 0;
	imicrolvl = 6;	// Cloud micro resolution level
	cloudalt = mgr->GetPlanet()->prm.cloudalt;
	if (Config->TileMipmaps == 2) bMipmaps = true;
	if (Config->TileMipmaps == 1 && _lvl < 10) bMipmaps = true;
}

// -----------------------------------------------------------------------

CloudTile::~CloudTile ()
{
}

// -----------------------------------------------------------------------

void CloudTile::PreLoad()
{

	// Configure microtexture range for "Water texture" and "Cloud microtexture".
	GetParentMicroTexRange(&microrange);
	
	bool ok = false;

	if (cmgr->DoLoadIndividualFiles(0)) { // try loading from individual tile file
		char path[MAX_PATH];
		sprintf_s (path, MAX_PATH, "%s\\Cloud\\%02d\\%06d\\%06d.dds", mgr->DataRootDir().c_str(), lvl+4, ilat, ilng);
		ok = LoadTextureFile(path, &pPreSrf, false);
	}
	if (!ok && cmgr->ZTreeManager(0)) { // try loading from compressed archive
		BYTE *buf;
		DWORD ndata = cmgr->ZTreeManager(0)->ReadData(lvl+4, ilat, ilng, &buf);
		if (ndata) {
			LoadTextureFromMemory(buf, ndata, &pPreSrf, false);
			cmgr->ZTreeManager(0)->ReleaseData(buf);
		}
	}
}


// -----------------------------------------------------------------------

void CloudTile::Load ()
{
	
	LPDIRECT3DDEVICE9 pDev = mgr->Dev();

	owntex = true;

	if (CreateTexture(pDev, pPreSrf, &tex) != true) {
		if (GetParentSubTexRange (&texrange)) {
			tex = getParent()->Tex();
			owntex = false;
		} else tex = 0;
	} else TileCatalog->Add(tex);

	bool shift_origin = (lvl >= 4);
	int res = mgr->GridRes();

	if (!lvl) {
		// create hemisphere mesh for western or eastern hemispheres
		mesh = CreateMesh_hemisphere (res, 0, cloudalt);
	//} else if (ilat == 0 || ilat == (1<<lvl)-1) {
		// create triangular patch for north/south pole region
	//	mesh = CreateMesh_tripatch (TILE_PATCHRES, elev, shift_origin, &vtxshift);
	} else {
		// create rectangular patch
		mesh = CreateMesh_quadpatch (res, res, 0, 1.0, cloudalt, &texrange, shift_origin, &vtxshift);
	}
}

// -----------------------------------------------------------------------

void CloudTile::Render()
{
	LPDIRECT3DDEVICE9 pDev = mgr->Dev();
	vPlanet* vPlanet = mgr->GetPlanet();
	PlanetShader* pShader = mgr->GetShader();
	ShaderParams* sp = vPlanet->GetTerrainParams();

	int cfg = vPlanet->GetShaderID();

	// ---------------------------------------------------------------------
	// Feed tile specific data to shaders
	//
	// ----------------------------------------------------------------------

	bool bTexture = (tex && vPlanet->HasTextures());

	if (bTexture)
	{
		pShader->SetTexture(pShader->tDiff, tex, IPF_ANISOTROPIC | IPF_CLAMP, Config->Anisotrophy);

		sp->vCloudOff = GetTexRangeDX(&texrange);
		sp->vMicroOff = GetTexRangeDX(&microrange);
		sp->fAlpha = 1.0f;
		sp->fBeta = 1.0f;
		sp->mWorld = mWorld;

		// -------------------------------------------------------------------
		// render surface mesh

		pShader->SetPSConstants(pShader->Prm, sp, sizeof(ShaderParams));
		pShader->SetVSConstants(pShader->PrmVS, sp, sizeof(ShaderParams));

		pShader->UpdateTextures();

		pDev->SetStreamSource(0, mesh->pVB, 0, sizeof(VERTEX_2TEX));
		pDev->SetIndices(mesh->pIB);
		pDev->DrawIndexedPrimitive(D3DPT_TRIANGLELIST, 0, 0, mesh->nv, 0, mesh->nf);
	}
}


// =======================================================================
// =======================================================================

template<>
void TileManager2<CloudTile>::Render (MATRIX4 &dwmat, bool use_zbuf, const vPlanet::RenderPrm &rprm)
{
	// set generic parameters
	SetRenderPrm (dwmat, rprm.cloudrot, use_zbuf, rprm);

	int i;
	class Scene *scene = GetClient()->GetScene();

	// adjust scaling parameters (can only be done if no z-buffering is in use)
	if (!use_zbuf) {
		double R = obj_size;
		double Rc = R+rprm.cloudalt;
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

		vp->GetScatterConst()->mVP = scene->PushCameraFrustumLimits(zmin, zmax);
	}

	// build a transformation matrix for frustum testing
	MATRIX4 Mproj = _MATRIX4(scene->GetProjectionMatrix());
	Mproj.m33 = 1.0; Mproj.m43 = -1.0;  // adjust near plane to 1, far plane to infinity
	MATRIX4 Mview = _MATRIX4(scene->GetViewMatrix());
	prm.dviewproj = mul(Mview,Mproj);

	// ---------------------------------------------------------------------
	// Initialize shading technique and feed planet specific data to shaders
	//

	ElevMode = eElevMode::Spherical;
	ElevModeLvl = 0;

	ShaderParams* sp = vp->GetTerrainParams();
	FlowControlPS* fc = vp->GetFlowControl();
	fc->bBelowClouds = vp->CameraAltitude() < rprm.cloudalt;

	int cfg = vp->GetShaderID();

	// Select cloud layer shader
	pShader = (cfg == PLT_GIANT ? vp->GetShader(PLT_G_CLOUDS) : vp->GetShader(PLT_CLOUDS));
	
	pShader->ClearTextures();
	pShader->Setup(pPatchVertexDecl, false, 1);


	// Check Eclipse conditions -------------------------------------------
	//

	vp->InitEclipse(pShader);

	pShader->SetPSConstants("Const", vp->GetScatterConst(), sizeof(ConstParams));
	pShader->SetVSConstants("Const", vp->GetScatterConst(), sizeof(ConstParams));
	pShader->SetPSConstants("Flow", fc, sizeof(FlowControlPS));

	if (cfg != PLT_GIANT)
	{
		if (Config->CloudMicro) {
			pShader->SetTexture("tCloudMicro", hCloudMicro, IPF_ANISOTROPIC | IPF_WRAP, Config->Anisotrophy);
			if (Config->bCloudNormals)
				pShader->SetTexture("tCloudMicroNorm", hCloudMicroNorm, IPF_ANISOTROPIC | IPF_WRAP, Config->Anisotrophy);
		}
		pShader->SetTexture("tSun", vp->GetScatterTable(SUN_COLOR), IPF_LINEAR | IPF_CLAMP);
		pShader->SetTexture("tLndRay", vp->GetScatterTable(RAY_LAND), IPF_LINEAR | IPF_CLAMP);
		pShader->SetTexture("tLndAtn", vp->GetScatterTable(ATN_LAND), IPF_LINEAR | IPF_CLAMP);
		//pShader->SetTexture("tSkyRayColor", vp->GetScatterTable(RAY_COLOR), IPF_LINEAR | IPF_CLAMP);
		//pShader->SetTexture("tSkyMieColor", vp->GetScatterTable(MIE_COLOR), IPF_LINEAR | IPF_CLAMP);
	}

	// TODO: render full sphere for levels < 4

	loader->WaitForMutex();

	// update the tree
	for (i = 0; i < 2; i++)
		ProcessNode (tiletree+i);

	// render the tree
	for (i = 0; i < 2; i++)
		RenderNode (tiletree+i);
	
	loader->ReleaseMutex ();

	// Pop previous frustum configuration, must initialize mVP
	if (!use_zbuf)	vp->GetScatterConst()->mVP = scene->PopCameraFrustumLimits();
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
	treeMgr = new ZTreeMgr*[ntreeMgr = 1]();
	if (cprm.tileLoadFlags & 0x0002) {
		treeMgr[0] = ZTreeMgr::CreateFromFile(m_dataRootDir.c_str(), ZTreeMgr::LAYER_CLOUD);
	}
}

// -----------------------------------------------------------------------

template<>
void TileManager2<CloudTile>::InitHasIndividualFiles()
{
	hasIndividualFiles = new bool[ntreeMgr]();
	if (cprm.tileLoadFlags & 0x0001) {
		char path[MAX_PATH], dummy[MAX_PATH];
		sprintf_s(path, MAX_PATH, "%s\\Cloud", m_dataRootDir.c_str());
		hasIndividualFiles[0] = FileExists(path);
	}
}

// -----------------------------------------------------------------------

template<>
Tile * TileManager2<CloudTile>::SearchTile (double lng, double lat, int maxlvl, bool bOwntex) const
{
	if (lng<0) return SearchTileSub(&tiletree[0], lng, lat, maxlvl, bOwntex);
	else	   return SearchTileSub(&tiletree[1], lng, lat, maxlvl, bOwntex);
}
