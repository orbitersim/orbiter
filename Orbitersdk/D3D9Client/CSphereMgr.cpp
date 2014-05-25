// =======================================================================
// CSphereMgr: Rendering of the celestial sphere background at variable
// resolutions.
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2007 Martin Schweiger
//				 2011 Jarmo Nikkanen (D3D9Client modification) 
// ==============================================================

#include "D3D9util.h"
#include "CSphereMgr.h"
#include "Scene.h"
#include "Texture.h"
#include "D3D9Config.h"
#include "D3D9Catalog.h"

using namespace oapi;

// =======================================================================
// Externals

extern int patchidx[9];
extern VBMESH *PATCH_TPL[15];

DWORD CSphereManager::vpX0, CSphereManager::vpX1, CSphereManager::vpY0, CSphereManager::vpY1;
double CSphereManager::diagscale;
int *CSphereManager::patchidx = 0;
int **CSphereManager::NLNG = 0;
int *CSphereManager::NLAT = 0;
VBMESH **CSphereManager::PATCH_TPL = 0;
const D3D9Config *CSphereManager::cfg = NULL;


void ReleaseTex(LPDIRECT3DTEXTURE9 pTex);


// =======================================================================
// Class CSphereManager

CSphereManager::CSphereManager (D3D9Client *gclient, const Scene *scene) : D3D9Effect()
{
	gc = gclient;
	scn = scene;

	gc->SetLabel("Loading Celestial Sphere...");
	
	patchidx  = TileManager::patchidx;
	PATCH_TPL = TileManager::PATCH_TPL;
	NLNG = TileManager::NLNG;
	NLAT = TileManager::NLAT;

	char *c = (char*)gc->GetConfigParam (CFGPRM_CSPHERETEXTURE);

	if (!c[0]) {
		disabled = true;
		return;
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
	double theta = 60.18*RAD;
	double phi = 90.02*RAD;
	double lambda = 173.6*RAD;
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
	LPDIRECT3DDEVICE9 dev = gclient->GetDevice();
	D3DVIEWPORT9 vp;
	dev->GetViewport (&vp);
	vpX0 = vp.X, vpX1 = vpX0 + vp.Width;
	vpY0 = vp.Y, vpY1 = vpY0 + vp.Height;
	// viewport size for clipping calculations

	diagscale = (double)vp.Width/(double)vp.Height;
	diagscale = sqrt(1.0 + diagscale*diagscale);
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
	// pre-load level 1-8 textures
	char fname[256];
	strcpy (fname, texname);
	strcat (fname, ".tex");

	gc->SetItem(fname);

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
	if (!intens) return; // sanity check
	
	D3DXCOLOR clr(intens, intens, intens, intens);

	HR(FX->SetValue(eColor, &clr, sizeof(D3DXCOLOR)));

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

	HR(FX->SetTechnique(eSkyDomeTech));
	HR(FX->SetValue(eSun, scn->GetLight(-1), sizeof(D3D9Light)));
	
	LPDIRECT3DDEVICE9 pDev = gc->GetDevice();
	pDev->SetVertexDeclaration(pPatchVertexDecl);

	UINT numPasses = 0;
	HR(FX->Begin(&numPasses, D3DXFX_DONOTSAVESTATE));
	HR(FX->BeginPass(0));

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

	HR(FX->EndPass());
	HR(FX->End());	

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
	
	FX->SetMatrix(eW, &mWorld);
	FX->SetTexture(eTex0, tex);	
	FX->CommitChanges();

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
	float rad = mesh.bsRad * 2000.0f; // * (float)RenderParam.objsize;
	D3DXVECTOR3 vP;
	D3DXVec3TransformCoord(&vP, &mesh.bsCnt, &mWorld);
	return gc->GetScene()->IsVisibleInCamera(&vP, rad);
}


