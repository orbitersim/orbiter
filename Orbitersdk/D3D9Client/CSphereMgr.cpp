// =======================================================================
// CSphereMgr: Rendering of the celestial sphere background at variable
// resolutions.
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2007 Martin Schweiger
// Copyright (C) 2011-2014 Jarmo Nikkanen (D3D9Client modification) 
// ==============================================================

#include "D3D9util.h"
#include "CSphereMgr.h"
#include "Scene.h"
#include "Texture.h"
#include "D3D9Config.h"
#include "D3D9Catalog.h"
#include "Spherepatch.h"

using namespace oapi;

// =======================================================================
// Externals

DWORD CSphereManager::vpX0, CSphereManager::vpX1, CSphereManager::vpY0, CSphereManager::vpY1;
double CSphereManager::diagscale;
int *CSphereManager::patchidx = 0;
int **CSphereManager::NLNG = 0;
int *CSphereManager::NLAT = 0;
const D3D9Config *CSphereManager::cfg = NULL;

VBMESH CSphereManager::PATCH_TPL_1;
VBMESH CSphereManager::PATCH_TPL_2;
VBMESH CSphereManager::PATCH_TPL_3;
VBMESH CSphereManager::PATCH_TPL_4[2];
VBMESH CSphereManager::PATCH_TPL_5;
VBMESH CSphereManager::PATCH_TPL_6[2];
VBMESH CSphereManager::PATCH_TPL_7[4];
VBMESH CSphereManager::PATCH_TPL_8[8];
VBMESH CSphereManager::PATCH_TPL_9[16];
VBMESH CSphereManager::PATCH_TPL_10[32];
VBMESH CSphereManager::PATCH_TPL_11[64];
VBMESH CSphereManager::PATCH_TPL_12[128];
VBMESH CSphereManager::PATCH_TPL_13[256];
VBMESH CSphereManager::PATCH_TPL_14[512];
VBMESH *CSphereManager::PATCH_TPL[15] = {
	0, &PATCH_TPL_1, &PATCH_TPL_2, &PATCH_TPL_3, PATCH_TPL_4, &PATCH_TPL_5,
	PATCH_TPL_6, PATCH_TPL_7, PATCH_TPL_8, PATCH_TPL_9, PATCH_TPL_10,
	PATCH_TPL_11, PATCH_TPL_12, PATCH_TPL_13, PATCH_TPL_14
};


void ReleaseTex(LPDIRECT3DTEXTURE9 pTex);


// =======================================================================
// Class CSphereManager

CSphereManager::CSphereManager (D3D9Client *gclient, const Scene *scene) : PlanetRenderer()
{
	scn = scene;

	gc->OutputLoadStatus("Loading Celestial Sphere...",0);
	
	patchidx  = TileManager::patchidx;
	NLNG = TileManager::NLNG;
	NLAT = TileManager::NLAT;

	char *c = (char*)gc->GetConfigParam (CFGPRM_CSPHERETEXTURE);

	if (!c[0]) {
		disabled = true;
	} else {
		strncpy (texname, c, 64);
		disabled = false;
	}

	double tmp;
	tmp = *(double*)gc->GetConfigParam (CFGPRM_CSPHEREINTENS);
	intensity = (float)tmp;

	maxlvl = 8; // g_pOrbiter->Cfg()->CSphereMaxLevel;
	maxbaselvl = min (8, maxlvl);
	int maxidx = patchidx[maxbaselvl];
	bPreloadTile = (Config->PlanetPreloadMode != 0);
	nhitex = nhispec = 0;

	tiledesc = new TILEDESC[maxidx];

	memset2 (tiledesc, 0, maxidx*sizeof(TILEDESC));

	LoadPatchData ();
	LoadTileData ();
	LoadTextures ();

	MATRIX3 R = {2000,0,0, 0,2000,0, 0,0,2000};

	// rotation from galactic to ecliptic frame
	double theta = 60.28*RAD;
	double phi = 90.08*RAD;
	double lambda = 173.7*RAD;
	double sint = sin(theta), cost = cos(theta);
	double sinp = sin(phi), cosp = cos(phi);
	double sinl = sin(lambda), cosl = cos(lambda);
	ecl2gal = _M(cosp,0,sinp, 0,1,0, -sinp,0,cosp);
	ecl2gal = mul (_M(1,0,0, 0,cost,sint, 0,-sint,cost), ecl2gal);
	ecl2gal = mul (_M(cosl,0,sinl, 0,1,0, -sinl,0,cosl), ecl2gal);
	R = mul (ecl2gal, R);

	D3DMAT_Identity (&trans);
	trans._11 = float(R.m11);
	trans._12 = float(R.m12);
	trans._13 = float(R.m13);
	trans._21 = float(R.m21);
	trans._22 = float(R.m22);
	trans._23 = float(R.m23);
	trans._31 = float(R.m31);
	trans._32 = float(R.m32);
	trans._33 = float(R.m33);

	LogAlw("CSphere Manager constructed");
}

// =======================================================================

CSphereManager::~CSphereManager ()
{
	if (disabled) return;

	// DWORD i, maxidx = patchidx[maxbaselvl];

	if (ntex) {
		for (DWORD i = 0; i < ntex; ++i)
			ReleaseTex(texbuf[i]);
		delete []texbuf;
	}

	delete []tiledesc;	
}

// =======================================================================

void CSphereManager::GlobalInit(oapi::D3D9Client *gclient)
{
	LogAlw("CSphereManager::GlobalInit()...");

	LPDIRECT3DDEVICE9 dev = gclient->GetDevice();
	D3DVIEWPORT9 vp;
	dev->GetViewport (&vp);
	vpX0 = vp.X, vpX1 = vpX0 + vp.Width;
	vpY0 = vp.Y, vpY1 = vpY0 + vp.Height;
	// viewport size for clipping calculations

	diagscale = (double)vp.Width/(double)vp.Height;
	diagscale = sqrt(1.0 + diagscale*diagscale);

	// Level 1 patch template
	CreateSphere (dev, PATCH_TPL_1, 6, false, 0, 64);
 
 	// Level 2 patch template
	CreateSphere (dev, PATCH_TPL_2, 8, false, 0, 128);
 
 	// Level 3 patch template
	CreateSphere (dev, PATCH_TPL_3, 12, false, 0, 256);
 
 	// Level 4 patch templates
	CreateSphere (dev, PATCH_TPL_4[0], 16, true, 0, 256);
	CreateSphere (dev, PATCH_TPL_4[1], 16, true, 1, 256);
 
 	// Level 5 patch template
	CreateSpherePatch (dev, PATCH_TPL_5, 4, 1, 0, 18);
 
 	// Level 6 patch templates
	CreateSpherePatch (dev, PATCH_TPL_6[0], 8, 2, 0, 10, 16);
	CreateSpherePatch (dev, PATCH_TPL_6[1], 4, 2, 1, 12);
 
 	// Level 7 patch templates
	CreateSpherePatch (dev, PATCH_TPL_7[0], 16, 4, 0, 12, 12, false);
	CreateSpherePatch (dev, PATCH_TPL_7[1], 16, 4, 1, 12, 12, false);
	CreateSpherePatch (dev, PATCH_TPL_7[2], 12, 4, 2, 10, 16, true);
	CreateSpherePatch (dev, PATCH_TPL_7[3],  6, 4, 3, 12, -1, true);
 
	int r = 16;
	if (Config->LODBias<0) r = 8;
	
 	// Level 8 patch templates
	CreateSpherePatch (dev, PATCH_TPL_8[0], 32, 8, 0, (12*r)>>4, (15*r)>>4, false, true, true);
	CreateSpherePatch (dev, PATCH_TPL_8[1], 32, 8, 1, (12*r)>>4, (15*r)>>4, false, true, true);
	CreateSpherePatch (dev, PATCH_TPL_8[2], 30, 8, 2, (12*r)>>4, (16*r)>>4, false, true, true);
	CreateSpherePatch (dev, PATCH_TPL_8[3], 28, 8, 3, (12*r)>>4, (12*r)>>4, false, true, true);
	CreateSpherePatch (dev, PATCH_TPL_8[4], 24, 8, 4, (12*r)>>4, (12*r)>>4, false, true, true);
	CreateSpherePatch (dev, PATCH_TPL_8[5], 18, 8, 5, (12*r)>>4, (12*r)>>4, false, true, true);
	CreateSpherePatch (dev, PATCH_TPL_8[6], 12, 8, 6, (10*r)>>4, (16*r)>>4, true,  true, true);
	CreateSpherePatch (dev, PATCH_TPL_8[7],  6, 8, 7, (12*r)>>4, -1, true,  true, true);


	// Patch templates for level 9 and beyond
	/*
	const int n = 8;
	const int nlng8[8] = {32,32,30,28,24,18,12,6};
	const int res8[8] = {15,15,16,12,12,12,12,12};
	int mult = 2, idx, lvl, i, j;
	for (lvl = 9; lvl <= SURF_MAX_PATCHLEVEL; lvl++) {
		idx = 0;
		for (i = 0; i < 8; i++) {
			for (j = 0; j < mult; j++) {
				if (idx < n*mult)
					CreateSpherePatch (dev, PATCH_TPL[lvl][idx], nlng8[i]*mult, n*mult, idx, 12, res8[i], false, true, true, true);
				else
					CreateSpherePatch (dev, PATCH_TPL[lvl][idx], nlng8[i]*mult, n*mult, idx, 12, -1, true, true, true, true);
				
				idx++;
			}
		}
		mult *= 2;
	}*/

	LogMsg("...Done");
}

// ==============================================================

void CSphereManager::GlobalExit ()
{
	int i;

	DestroyVBMesh (PATCH_TPL_1);
	DestroyVBMesh (PATCH_TPL_2);
	DestroyVBMesh (PATCH_TPL_3);
	for (i = 0; i <  2; i++) DestroyVBMesh (PATCH_TPL_4[i]);
	DestroyVBMesh (PATCH_TPL_5);
	for (i = 0; i <  2; i++) DestroyVBMesh (PATCH_TPL_6[i]);
	for (i = 0; i <  4; i++) DestroyVBMesh (PATCH_TPL_7[i]);
	for (i = 0; i <  8; i++) DestroyVBMesh (PATCH_TPL_8[i]);

	/*
	const int n = 8;
	int mult = 2, lvl;
	for (lvl = 9; lvl <= SURF_MAX_PATCHLEVEL; lvl++) {
		for (i = 0; i < n*mult; i++) DestroyVBMesh (PATCH_TPL[lvl][i]);
		mult *= 2;
	}*/
}

// =======================================================================

void CSphereManager::CreateDeviceObjects (LPDIRECT3D9 d3d, LPDIRECT3DDEVICE9 dev)
{
	D3DVIEWPORT9 vp;
	dev->GetViewport (&vp);
	vpX0 = vp.X, vpX1 = vpX0 + vp.Width;
	vpY0 = vp.Y, vpY1 = vpY0 + vp.Height;
	// viewport size for clipping calculations

	diagscale = (double)vp.Width/(double)vp.Height;
	diagscale = sqrt(1.0 + diagscale*diagscale);
}

// =======================================================================

void CSphereManager::DestroyDeviceObjects ()
{
}

// =======================================================================

bool CSphereManager::LoadPatchData ()
{
	// OBSOLETE
	int i;
	for (i = 0; i < patchidx[maxbaselvl]; i++)	tiledesc[i].flag = 1;
	return false;
}

// =======================================================================

bool CSphereManager::LoadTileData ()
{
	if (maxlvl <= 8) // no tile data required
		return false;

	// TODO
	return true;
}

// =======================================================================

void CSphereManager::LoadTextures ()
{
	if (disabled) return;

	// pre-load level 1-8 textures
	char fname[256];
	strcpy (fname, texname);
	strcat (fname, ".tex");

	gc->OutputLoadStatus(fname,1);

	ntex = patchidx[maxbaselvl];
	texbuf = new LPDIRECT3DTEXTURE9[ntex];
	if (ntex = gc->GetTexMgr()->LoadTextures(fname, texbuf, 0, ntex)) {
		while ((int)ntex < patchidx[maxbaselvl]) maxlvl = --maxbaselvl;
		while ((int)ntex > patchidx[maxbaselvl]) ReleaseTex(texbuf[--ntex]);
		// not enough textures loaded for requested resolution level
		for (int i = 0; i < patchidx[maxbaselvl]; ++i)
			tiledesc[i].tex = texbuf[i];
	} else {
		delete []texbuf;
		texbuf = 0;
		// no textures at all!
	}

	//  pre-load highres tile textures
	if (bPreloadTile && nhitex) {
		//TILEDESC *tile8 = tiledesc + patchidx[7];
		//PreloadTileTextures (tile8, nhitex, nhispec);
	}
	LogAlw("CSphereManager:: Textures Loaded");
}

// =======================================================================

void CSphereManager::Render (LPDIRECT3DDEVICE9 dev, int level, int bglvl)
{
	if (disabled) return;

	float intens = intensity;

	if (bglvl) intens *= exp(-float(bglvl)*0.05f);
	
	Shader()->SetFloat(sfAlpha, intens);

	level = min ((DWORD)level, maxlvl);

	RenderParam.dev = dev;
	RenderParam.tgtlvl = level;
	RenderParam.wmat = trans;
	RenderParam.viewap = atan(diagscale * tan(scn->GetCameraAperture()));

	int startlvl = min (level, 8);
	int hemisp, ilat, ilng, idx;
	int  nlat = NLAT[startlvl];
	int *nlng = NLNG[startlvl];
	int texofs = patchidx[startlvl-1];
	TILEDESC *td = tiledesc + texofs;
	TEXCRDRANGE range = {0,1,0,1};
	tilebuf = TileManager::tilebuf;

	MATRIX3 rcam;
	oapiCameraRotationMatrix(&rcam);

	rcam = mul(ecl2gal, rcam);

	RenderParam.camdir = _V(rcam.m13, rcam.m23, rcam.m33);

	WaitForSingleObject (tilebuf->hQueueMutex, INFINITE);

	HR(Shader()->SetTechnique(eSkyDomeTech));
	HR(Shader()->SetMatrix(smViewProj, scn->GetProjectionViewMatrix()));
	
	pDev->SetVertexDeclaration(pPatchVertexDecl);

	UINT numPasses = 0;
	HR(Shader()->Begin(&numPasses, D3DXFX_DONOTSAVESTATE));

	Shader()->BeginPass(0);
	
	for (hemisp = idx = 0; hemisp < 2; hemisp++) {
		if (hemisp) { // flip world transformation to southern hemisphere
			D3DXMatrixMultiply(&RenderParam.wmat, &TileManager::Rsouth, &RenderParam.wmat);
		}
		for (ilat = nlat-1; ilat >= 0; ilat--) {
			for (ilng = 0; ilng < nlng[ilat]; ilng++) {
				ProcessTile (startlvl, hemisp, ilat, nlat, ilng, nlng[ilat], td+idx, 
					range, td[idx].tex, td[idx].ltex, td[idx].flag,
					range, td[idx].tex, td[idx].ltex, td[idx].flag);
				idx++;
			}
		}
	}

	HR(Shader()->EndPass());
	HR(Shader()->End());	

	ReleaseMutex (tilebuf->hQueueMutex);
}

// =======================================================================

void CSphereManager::ProcessTile (int lvl, int hemisp, int ilat, int nlat, int ilng, int nlng, TILEDESC *tile,
	const TEXCRDRANGE &range, LPDIRECT3DTEXTURE9 tex, LPDIRECT3DTEXTURE9 ltex, DWORD flag,
	const TEXCRDRANGE &bkp_range, LPDIRECT3DTEXTURE9 bkp_tex, LPDIRECT3DTEXTURE9 bkp_ltex, DWORD bkp_flag)
{
	
	static const double rad0 = sqrt(2.0)*PI05;
	VECTOR3 cnt = TileCentre (hemisp, ilat, nlat, ilng, nlng);
	double rad = rad0/(double)nlat;
	double alpha = acos (dotp (RenderParam.camdir, cnt));
	double adist = alpha - rad;
	
	if (adist > RenderParam.viewap) return;

	SetWorldMatrix (ilng, nlng, ilat, nlat);

	// Check if patch bounding box intersects viewport

	if (!TileInView (lvl, ilat)) return;

	RenderTile(lvl, hemisp, ilat, nlat, ilng, nlng, tile, range, tex, ltex, flag);
}

// =======================================================================

void CSphereManager::SetWorldMatrix (int ilng, int nlng, int ilat, int nlat)
{
	// set up world transformation matrix
	D3DXMATRIX rtile;

	double lng = PI2 * (double)ilng/(double)nlng + PI; // add pi so texture wraps at +-180°
	D3DMAT_RotY (&rtile, lng);
	D3DXMatrixMultiply(&mWorld, &rtile, &RenderParam.wmat);
}

// =======================================================================

void CSphereManager::RenderTile (int lvl, int hemisp, int ilat, int nlat, int ilng, int nlng,
	TILEDESC *tile, const TEXCRDRANGE &range, LPDIRECT3DTEXTURE9 tex, LPDIRECT3DTEXTURE9 ltex, DWORD flag)
{
	VBMESH &mesh = PATCH_TPL[lvl][ilat]; // patch template

	gc->GetStats()->Vertices += mesh.nv;
	gc->GetStats()->Tiles[lvl]++;
	gc->GetStats()->Draw++;
	
	Shader()->SetMatrix(smWorld, &mWorld);
	Shader()->SetTexture(stDiff, tex);	
	Shader()->CommitChanges();

	pDev->SetStreamSource(0, mesh.pVB, 0, sizeof(VERTEX_2TEX));
	pDev->SetIndices(mesh.pIB);
	pDev->DrawIndexedPrimitive(D3DPT_TRIANGLELIST, 0, 0, mesh.nv, 0, mesh.nf);
}

// =======================================================================
// returns the direction of the tile centre from the planet centre in local
// planet coordinates

VECTOR3 CSphereManager::TileCentre (int hemisp, int ilat, int nlat, int ilng, int nlng)
{
	double cntlat = PI05 * ((double)ilat+0.5)/(double)nlat,      slat = sin(cntlat), clat = cos(cntlat);
	double cntlng = PI2  * ((double)ilng+0.5)/(double)nlng + PI, slng = sin(cntlng), clng = cos(cntlng);
	if (hemisp) return _V(clat*clng, -slat, -clat*slng);
	else        return _V(clat*clng,  slat,  clat*slng);
}

// =======================================================================

void CSphereManager::TileExtents (int hemisp, int ilat, int nlat, int ilng, int nlng, double &lat1, double &lat2, double &lng1, double &lng2) const
{
	lat1 = PI05 * (double)ilat/(double)nlat;
	lat2 = lat1 + PI05/(double)nlat;
	lng1 = PI2 * (double)ilng/(double)nlng + PI;
	lng2 = lng1 + PI2/nlng;
	if (hemisp) {
		double tmp = lat1; lat1 = -lat2; lat2 = -tmp;
		tmp = lng1; lng1 = -lng2; lng2 = -tmp;
		if (lng2 < 0) lng1 += PI2, lng2 += PI2;
	}
}


// =======================================================================

bool CSphereManager::TileInView (int lvl, int ilat)
{
	VBMESH &mesh = PATCH_TPL[lvl][ilat];
	float rad = mesh.bsRad * 2000.0f;
	D3DXVECTOR3 vP;
	D3DXVec3TransformCoord(&vP, &mesh.bsCnt, &mWorld);
	return gc->GetScene()->IsVisibleInCamera(&vP, rad);
}


