// ==============================================================
// VBase.cpp
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2007 - 2016 Martin Schweiger
//				 2011 - 2016 Jarmo Nikkanen (D3D9Client modification)  
// ==============================================================

// ==============================================================
// class vBase (implementation)
//
// A vBase is the visual representation of a surface base
// object (a "spaceport" on the surface of a planet or moon,
// usually with runways or landing pads where vessels can
// land and take off.
// ==============================================================

#include "VBase.h"
#include "TileMgr.h"
#include "D3D9Client.h"
#include "D3D9Surface.h"
#include "BeaconArray.h"
#include "RunwayLights.h"
#include "FileParser.h"
#include "AABBUtil.h"
#include "OrbiterAPI.h"
#include "DebugControls.h"
#include "D3D9Config.h"
#include "VPlanet.h"

vBase::vBase (OBJHANDLE _hObj, const Scene *scene, vPlanet *_vP): vObject (_hObj, scene)
{
	_TRACE;
	DWORD i,j;

	vP = _vP;
	hPlanet = oapiGetBasePlanet(hObj);

	if (!vP) vP = static_cast<vPlanet*>( scene->GetVisObject(hPlanet) );
	
	structure_bs	= NULL;
	structure_as	= NULL;
	nstructure_bs	= 0;
	nstructure_as	= 0;
	tspec			= NULL;
	tilemesh		= NULL;
	numRunwayLights = 0;
	numTaxiLights   = 0;
	runwayLights    = NULL;
	taxiLights		= NULL;
	csun_lights     = RAD * Config->SunAngle;

	// ----------------------------------------------------------------------
	// Compute transformations from local base frame to planet frame and back
	//
	MATRIX3 grot; VECTOR3 gpos;
	oapiGetRotationMatrix(hPlanet, &grot);
	oapiGetRotationMatrix(hObj, &mGlobalRot);
	oapiGetRelativePos(hObj, hPlanet, &gpos);
	vLocalPos = tmul(grot, gpos);

	swap(grot.m12, grot.m21);
	swap(grot.m13, grot.m31);
	swap(grot.m23, grot.m32);

	mGlobalRot = mul(grot, mGlobalRot);

	D3DXMatrixIdentity(&mGlobalInvRot);
	D3DMAT_SetRotation(&mGlobalInvRot, &mGlobalRot);
	//------------------------------------------------------------------------

	// load surface tiles
	DWORD _ntile = gc->GetBaseTileList (_hObj, &tspec);
	ntile = 0;
	for (i=0; i<_ntile; ++i) {
		// Only count (render) tiles where bit0 is set!
		if (tspec[i].texflag & 0x01) {
			++ntile;
		}
	}

	// Do not render tiles for planets having a new tile format
	if (vP->tilever >= 2) ntile = 0;

	if (ntile) {

		MESHGROUPEX **grps = new MESHGROUPEX*[ntile];
		SURFHANDLE *texs = new SURFHANDLE[ntile];

		for (i = 0, j = 0; i < _ntile; ++i) {
			// Only render tiles where bit0 is set!
			if (tspec[i].texflag & 0x01) {
				DWORD ng = oapiMeshGroupCount(tspec[i].mesh);
				if (ng!=1) LogErr("MeshGroup Count = %u",ng);
				else {
					texs[j] = tspec[i].tex;
					grps[j] = oapiMeshGroupEx(tspec[i].mesh, 0);
				}
				++j;
			}
		}
		tilemesh = new D3D9Mesh(ntile, (const MESHGROUPEX**)grps, texs);	
		delete []grps; 
        delete []texs;
	}

	// load meshes for generic structures
	MESHHANDLE *sbs, *sas;
	DWORD nsbs, nsas;
	gc->GetBaseStructures (_hObj, &sbs, &nsbs, &sas, &nsas);

	if (nstructure_bs = nsbs) {
		structure_bs = new D3D9Mesh*[nsbs];
		for (i = 0; i < nsbs; i++) structure_bs[i] = new D3D9Mesh(sbs[i]);
	}

	if (nstructure_as = nsas) {
		structure_as = new D3D9Mesh*[nsas];
		for (i = 0; i < nsas; i++) structure_as[i] = new D3D9Mesh(sas[i]);
	}

	lights = false;
	Tchk = Tlghtchk = oapiGetSimTime()-1.0;

	UpdateBoundingBox();
	
	char name[64];
	oapiGetObjectName(_hObj, name, 64);
	LogAlw("New Base Visual(0x%X) %s hBase=0x%X, nsbs=%u, nsas=%u", this, name, _hObj, nsbs, nsas);

	CreateRunwayLights();
	CreateTaxiLights();
}


// ===========================================================================================
//
VECTOR3 vBase::ToLocal(VECTOR3 pos, double *lng, double *lat) const
{
	double rad;
	VECTOR3 vLoc = mul(mGlobalRot, pos) + vLocalPos;
	if (lng && lat) oapiLocalToEqu(hPlanet, vLoc, lng, lat, &rad);
	return vLoc;
}


// ===========================================================================================
//
VECTOR3 vBase::FromLocal(VECTOR3 pos) const
{
	return tmul(mGlobalRot, pos-vLocalPos);
}

// ===========================================================================================
//
void vBase::FromLocal(VECTOR3 pos, D3DXVECTOR3 *pTgt) const
{
	D3DXVECTOR3 pv(float(pos.x-vLocalPos.x), float(pos.y-vLocalPos.y), float(pos.z-vLocalPos.z));
	D3DXVec3TransformNormal(pTgt, &pv, &mGlobalInvRot);
}

// ===========================================================================================
//
double vBase::GetElevation() const
{
	VECTOR3 bp;
	oapiGetRelativePos(hObj, hPlanet, &bp);
	return length(bp) - oapiGetSize(hPlanet);
}


// ===========================================================================================
//
void vBase::CreateRunwayLights()
{
	const char *file = gc->GetFileParser()->GetConfigFile(hObj);
	if (file) numRunwayLights = RunwayLights::CreateRunwayLights(this, scn, file, runwayLights);
	else LogErr("Configuration file not found for object 0x%X", hObj);
}

// ===========================================================================================
//
void vBase::CreateTaxiLights()
{
	const char *file = gc->GetFileParser()->GetConfigFile(hObj);
	if (file) numTaxiLights = TaxiLights::CreateTaxiLights(hObj, scn, file, taxiLights);
	else LogErr("Configuration file not found for object 0x%X", hObj);
}

// ===========================================================================================
//
vBase::~vBase ()
{
	DWORD i;

	if (tilemesh) delete tilemesh;

	if (nstructure_bs) {
		for (i = 0; i < nstructure_bs; i++)	delete structure_bs[i];
		delete []structure_bs;
	}
	if (nstructure_as) {
		for (i = 0; i < nstructure_as; i++)	delete structure_as[i];
		delete []structure_as;
	}

	if (runwayLights) {
		for(i=0; i<(DWORD)numRunwayLights; i++)
		{
			SAFE_DELETE(runwayLights[i]);
		}
		delete[] runwayLights;
	}

	if (taxiLights) {
		for(i=0; i<(DWORD)numTaxiLights; i++)
		{
			SAFE_DELETE(taxiLights[i]);
		}
		delete[] taxiLights;
	}

	if (DebugControls::IsActive()) {
		DebugControls::RemoveVisual(this);
	}
}

// ===========================================================================================
//
DWORD vBase::GetMeshCount()
{
	if (tilemesh) return nstructure_bs + nstructure_as + 1;
	else          return nstructure_bs + nstructure_as;
}

// ===========================================================================================
//
bool vBase::GetMinMaxDistance(float *zmin, float *zmax, float *dmin)
{
	if (bBSRecompute) UpdateBoundingBox();
	
	D3DXMATRIX mWorldView;

	Scene *scn = gc->GetScene();

	D3DXVECTOR4 Field = D9LinearFieldOfView(scn->GetProjectionMatrix());
	
	D3DXMatrixMultiply(&mWorldView, &mWorld, scn->GetViewMatrix());
	
	if (tilemesh) {
		D9ComputeMinMaxDistance(gc->GetDevice(), tilemesh->GetAABB(), &mWorldView, &Field, zmin, zmax, dmin);
	}

	if (nstructure_bs) {
		for (DWORD i = 0; i < nstructure_bs; i++) {
			D9ComputeMinMaxDistance(gc->GetDevice(), structure_bs[i]->GetAABB(), &mWorldView, &Field, zmin, zmax, dmin);
		}
	}

	if (nstructure_as) {
		for (DWORD i = 0; i < nstructure_as; i++) {
			D9ComputeMinMaxDistance(gc->GetDevice(), structure_as[i]->GetAABB(), &mWorldView, &Field, zmin, zmax, dmin);
		}
	}

	return true;
}


// ===========================================================================================
//
void vBase::UpdateBoundingBox()
{
	bBSRecompute = false;
	
	if (tilemesh || nstructure_bs || nstructure_as) D9InitAABB(&BBox);
	else D9ZeroAABB(&BBox);

	
	if (tilemesh) D9AddAABB(tilemesh->GetAABB(), NULL, &BBox);
		
	if (nstructure_bs) {
		for (DWORD i = 0; i < nstructure_bs; i++) {
			D9AddAABB(structure_bs[i]->GetAABB(), NULL, &BBox);
		}
	}

	if (nstructure_as) {
		for (DWORD i = 0; i < nstructure_as; i++) {
			D9AddAABB(structure_as[i]->GetAABB(), NULL, &BBox);
		}
	}

	D9UpdateAABB(&BBox);
}


// ===========================================================================================
//
bool vBase::Update (bool bMainScene)
{
	_TRACE;
	if (!active) return false;
	if (!vObject::Update(bMainScene)) return false;

	double simt = oapiGetSimTime();

	if (fabs(simt-Tlghtchk)>0.1) {
		sunLight = *scn->GetSun();

		DWORD dAmbient = *(DWORD*)gc->GetConfigParam(CFGPRM_AMBIENTLEVEL);
		float fAmbient = float(dAmbient)*0.0039f;

		SurfaceLighting(&sunLight, oapiGetBasePlanet(hObj), hObj, fAmbient);	
		Tlghtchk = simt;
	}

	if (fabs(simt-Tchk)>1.0) {
		VECTOR3 pos, sdir;
		MATRIX3 rot;
		oapiGetGlobalPos (hObj, &pos); normalise(pos);
		oapiGetRotationMatrix (hObj, &rot);
		sdir = tmul (rot, -pos);
		double csun = sdir.y;
		bool night = csun < csun_lights;
		if (lights != night) {
			DWORD i;
			for (i = 0; i < nstructure_bs; i++)	structure_bs[i]->SetTexMixture (1, night ? 1.0f:0.0f);
			for (i = 0; i < nstructure_as; i++)	structure_as[i]->SetTexMixture (1, night ? 1.0f:0.0f);
			lights = night;
		}
		Tchk = simt;
	}
	return true;
}


// ===========================================================================================
//
bool vBase::RenderSurface(LPDIRECT3DDEVICE9 dev)
{
	// note: assumes z-buffer disabled
	if (!active) return false;
	if (!IsVisible()) return false;

	pCurrentVisual = this;

	// render tiles
	if (tilemesh) {
		uCurrentMesh = 0; // Used for debugging
		tilemesh->SetSunLight(&sunLight);
		tilemesh->RenderBaseTile(&mWorld);
		++uCurrentMesh;
	}

	// render generic objects under shadows
	if (nstructure_bs) {
		for (DWORD i = 0; i < nstructure_bs; ++i) {
			structure_bs[i]->SetSunLight(&sunLight);
			structure_bs[i]->Render(&mWorld, RENDER_BASEBS);
			++uCurrentMesh;
		}
	}

	return true;
}


// ===========================================================================================
//
bool vBase::RenderStructures(LPDIRECT3DDEVICE9 dev)
{
	if (!active) return false;
	if (!IsVisible()) return false;

	pCurrentVisual = this;
	uCurrentMesh = 0; // Used for debugging

	if (tilemesh) uCurrentMesh++;
	uCurrentMesh += nstructure_bs;

	// render generic objects above shadows
	for (DWORD i=0; i<nstructure_as; i++) {
		structure_as[i]->SetSunLight(&sunLight);
		structure_as[i]->Render(&mWorld, RENDER_BASE);
		++uCurrentMesh;
	}
	return true;
}


// ===========================================================================================
//
void vBase::RenderRunwayLights(LPDIRECT3DDEVICE9 dev)
{
	if (!active) return;
	if (!IsVisible()) return;

	pCurrentVisual = this;

	for(int i=0; i<numRunwayLights; i++)
	{
		runwayLights[i]->Update(vP);
		runwayLights[i]->Render(dev, &mWorld, lights);
	}

	for(int i=0; i<numTaxiLights; i++)
	{
		taxiLights[i]->Render(dev, &mWorld, lights);
	}
	
	if (DebugControls::IsActive()) {
		DWORD flags = *(DWORD*)gc->GetConfigParam(CFGPRM_GETDEBUGFLAGS);
		if (flags&DBG_FLAGS_SELVISONLY && this!=DebugControls::GetVisual()) return; // Used for debugging
		if (flags&DBG_FLAGS_BOXES) {
			D3DXMATRIX id;
			D3D9Effect::RenderBoundingBox(&mWorld, D3DXMatrixIdentity(&id), &BBox.min, &BBox.max, &D3DXVECTOR4(1,0,1,0.75f));
		}
	}
}


// ===========================================================================================
//
void vBase::RenderGroundShadow(LPDIRECT3DDEVICE9 dev, float alpha)
{
	if (!nstructure_as) return; // nothing to do
	if (!active) return;
	if (!IsVisible()) return;

	pCurrentVisual = this;

	VECTOR3 sd;
	oapiGetGlobalPos(hObj, &sd); normalise(sd);
	
	D3DXVECTOR3 gsun = D3DXVEC(sd);
	D3DXVECTOR3 lsun;
	D3DXVec3TransformNormal(&lsun, &gsun, &mWorldInv);

	if (lsun.y>0) return;

	float scale = float( min(1, (-lsun.y-0.07)/0.015) );
	if (scale<1) scale = alpha * scale;
	else         scale = alpha;

	// build shadow projection matrix
	D3DXMATRIX mProj;
	D3DXMatrixIdentity(&mProj);

	lsun /= lsun.y;

	mProj._21 -= lsun.x;
	mProj._22 -= lsun.y;
	mProj._23 -= lsun.z;

	D3DXVECTOR4 light(lsun.x, lsun.y, lsun.z, 0);

	OBJHANDLE hPlanet = oapiGetBasePlanet(hObj); 
	D3DXVECTOR4 param = D9OffsetRange(oapiGetSize(hPlanet), 30e3);

	for (DWORD i=0; i<nstructure_as; i++) {
		if (structure_as[i]->HasShadow()) structure_as[i]->RenderShadowsEx(scale, &mProj, &mWorld, &light, &param);
	}
}