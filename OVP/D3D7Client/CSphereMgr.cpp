// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//   ORBITER VISUALISATION PROJECT (OVP)
//   D3D7 Client module
// ==============================================================

// =======================================================================
// CSphereMgr.cpp
// Rendering of the celestial sphere background at variable
// resolutions.
// =======================================================================

#include "D3d7util.h"
#include "CSphereMgr.h"
#include "Scene.h"
#include "Camera.h"
#include "Texture.h"
#include "D3D7Config.h"

using namespace oapi;

// =======================================================================
// Externals

extern int patchidx[9];
extern DWORD VB_MemFlag; // dodgy
extern VBMESH *PATCH_TPL[14];
extern LPDIRECT3DVERTEXBUFFER7 bbtarget; // dodgy
extern char DBG_MSG[256];

void ApplyPatchTextureCoordinates (VBMESH &mesh, LPDIRECT3DVERTEXBUFFER7 vtx, const TEXCRDRANGE &range);

static int ntot, nrad, nrender;

DWORD CSphereManager::vpX0, CSphereManager::vpX1, CSphereManager::vpY0, CSphereManager::vpY1;
double CSphereManager::diagscale = 0.0;
int *CSphereManager::patchidx = 0;
int **CSphereManager::NLNG = 0;
int *CSphereManager::NLAT = 0;
DWORD CSphereManager::vbMemCaps = 0;
VBMESH **CSphereManager::PATCH_TPL = 0;
const D3D7Config *CSphereManager::cfg = NULL;

// =======================================================================
// =======================================================================
// Class CSphereManager

CSphereManager::CSphereManager (const D3D7Client *gclient, const Scene *scene)
{
	gc = gclient;
	cfg = gc->Cfg();
	scn = scene;
	vbMemCaps = (gclient->GetFramework()->IsTLDevice() ? 0 : D3DVBCAPS_SYSTEMMEMORY);
	patchidx = TileManager::patchidx;
	PATCH_TPL = TileManager::PATCH_TPL;
	NLNG = TileManager::NLNG;
	NLAT = TileManager::NLAT;

	m_bBkgImg = *(bool*)gc->GetConfigParam(CFGPRM_CSPHEREUSEBGIMAGE);
	if (m_bBkgImg) {
		char* c = (char*)gc->GetConfigParam(CFGPRM_CSPHERETEXTURE);
		if (c[0]) strncpy(texname, c, 128);
		else      m_bBkgImg = false;
	}

	m_bStarImg = *(bool*)gc->GetConfigParam(CFGPRM_CSPHEREUSESTARIMAGE);
	if (m_bStarImg) {
		char* c = (char*)gc->GetConfigParam(CFGPRM_CSPHERESTARTEXTURE);
		if (c[0]) strncpy(starfieldname, c, 128);
		else      m_bStarImg = false;
	}

	m_bDisabled = true;
	if (!m_bBkgImg && !m_bStarImg)
		return;

	double tmp;
	tmp = *(double*)gc->GetConfigParam (CFGPRM_CSPHEREINTENS);
	intensity = (float)tmp;

	maxlvl = 8; // g_pOrbiter->Cfg()->CSphereMaxLevel;
	maxbaselvl = min (8, maxlvl);
	int maxidx = patchidx[maxbaselvl];
	bPreloadTile = (cfg->PlanetPreloadMode != 0);
	nhitex = nhispec = 0;

	tiledesc = new TILEDESC[maxidx];
	memset (tiledesc, 0, maxidx*sizeof(TILEDESC));

	LoadPatchData ();
	LoadTileData ();
	LoadTextures ();

	// rotation from galactic to ecliptic frame
	double lambda = 173.60 * RAD;
	double phi = 90.03 * RAD;
	double theta = 60.19 * RAD;
	double sint = sin(theta), cost = cos(theta);
	double sinp = sin(phi), cosp = cos(phi);
	double sinl = sin(lambda), cosl = cos(lambda);
	ecl2gal = _M(cosp,0,sinp, 0,1,0, -sinp,0,cosp);
	ecl2gal = mul (_M(1,0,0, 0,cost,sint, 0,-sint,cost), ecl2gal);
	ecl2gal = mul (_M(cosl,0,sinl, 0,1,0, -sinl,0,cosl), ecl2gal);

	D3DMAT_Identity (&trans);
	trans._11 = (D3DVALUE)ecl2gal.m11;
	trans._12 = (D3DVALUE)ecl2gal.m12;
	trans._13 = (D3DVALUE)ecl2gal.m13;
	trans._21 = (D3DVALUE)ecl2gal.m21;
	trans._22 = (D3DVALUE)ecl2gal.m22;
	trans._23 = (D3DVALUE)ecl2gal.m23;
	trans._31 = (D3DVALUE)ecl2gal.m31;
	trans._32 = (D3DVALUE)ecl2gal.m32;
	trans._33 = (D3DVALUE)ecl2gal.m33;
}

// =======================================================================

CSphereManager::~CSphereManager ()
{
	if (!m_bDisabled) {
		DWORD i, maxidx = patchidx[maxbaselvl];

		if (m_bBkgImg) {
			for (auto tex : m_texbuf)
				tex->Release();
			m_texbuf.clear();
		}
		if (m_bStarImg) {
			for (auto tex : m_starbuf)
				tex->Release();
			m_starbuf.clear();
		}

		for (i = 0; i < maxidx; i++)
			if (tiledesc[i].vtx) tiledesc[i].vtx->Release();
		delete[]tiledesc;
	}
}

// =======================================================================

void CSphereManager::GlobalInit (oapi::D3D7Client *gclient)
{
	LPDIRECT3DDEVICE7 dev = gclient->GetDevice();
	D3DVIEWPORT7 vp;
	dev->GetViewport (&vp);
	vpX0 = vp.dwX, vpX1 = vpX0 + vp.dwWidth;
	vpY0 = vp.dwY, vpY1 = vpY0 + vp.dwHeight;
	// viewport size for clipping calculations

	diagscale = (double)vp.dwWidth/(double)vp.dwHeight;
	diagscale = sqrt(1.0 + diagscale*diagscale);
}

// =======================================================================

void CSphereManager::DestroyDeviceObjects ()
{
}

// =======================================================================

void CSphereManager::SetBgBrightness(double val)
{
	intensity = (float)val;
}

// =======================================================================

bool CSphereManager::LoadPatchData ()
{
	// OBSOLETE

	int i;
	for (i = 0; i < patchidx[maxbaselvl]; i++)
		tiledesc[i].flag = 1;
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
	int i;
	int texlvl = 0, starlvl = 0;

	if (m_bBkgImg) {
		// pre-load level 1-8 background image textures
		char fname[256];
		strcpy(fname, texname);
		strcat(fname, ".tex");
		int lvl = maxbaselvl;
		int ntex = patchidx[lvl];
		m_texbuf.resize(ntex);
		if (ntex = gc->GetTexMgr()->LoadTextures(fname, m_texbuf.data(), 0, ntex)) {
			while (ntex < patchidx[lvl])
				--lvl;
			while (ntex > patchidx[lvl])
				m_texbuf[--ntex]->Release();
			if (ntex < m_texbuf.size())
				m_texbuf.resize(ntex);
		}
		else {
			m_texbuf.clear();
			m_bBkgImg = false;
		}
		texlvl = lvl;
	}

	if (m_bStarImg) {
		// pre-load level 1-8 starfield image textures
		char fname[256];
		strcpy(fname, starfieldname);
		strcat(fname, ".tex");
		int lvl = maxbaselvl;
		int ntex = patchidx[lvl];
		m_starbuf.resize(ntex);
		if (ntex = gc->GetTexMgr()->LoadTextures(fname, m_starbuf.data(), 0, ntex)) {
			while (ntex < patchidx[lvl])
				--lvl;
			while (ntex > patchidx[lvl])
				m_starbuf[--ntex]->Release();
			if (ntex < m_starbuf.size())
				m_starbuf.resize(ntex);
		}
		else {
			m_starbuf.clear();
			m_bStarImg = false;
		}
		starlvl = lvl;
	}

	// make sure the two texture sets support the same resolution range
	if (m_bBkgImg && m_bStarImg && texlvl != starlvl) {
		if (texlvl < starlvl) {
			for (int i = m_texbuf.size(); i < m_starbuf.size(); i++)
				m_starbuf[i]->Release();
			m_starbuf.resize(m_texbuf.size());
			starlvl = texlvl;
		}
		else {
			for (int i = m_starbuf.size(); i < m_texbuf.size(); i++)
				m_texbuf[i]->Release();
			m_texbuf.resize(m_starbuf.size());
			texlvl = starlvl;
		}
	}
	maxbaselvl = maxlvl = (m_bBkgImg ? texlvl : m_bStarImg ? starlvl : 0);

	for (int i = 0; i < patchidx[maxbaselvl]; i++) {
		if (m_bBkgImg)
			tiledesc[i].tex = m_texbuf[i];
		if (m_bStarImg)
			tiledesc[i].ltex = m_starbuf[i];
	}

	m_bDisabled = !m_bBkgImg && !m_bStarImg;

	//  pre-load highres tile textures
	if (bPreloadTile && nhitex) {
		//TILEDESC *tile8 = tiledesc + patchidx[7];
		//PreloadTileTextures (tile8, nhitex, nhispec);
	}
}

// =======================================================================

void CSphereManager::Render (LPDIRECT3DDEVICE7 dev, int level, double bglvl)
{
	if (m_bDisabled) return;
	ntot = nrad = nrender = 0;

	float intens = intensity;
	float bgscale = (float)exp(-bglvl * 12.5);

	if (bgscale < 1e-3 || (intens < 1e-3 && !m_bStarImg)) return; // sanity check

	level = min (level, (int)maxlvl);

	RenderParam.dev = dev;
	RenderParam.tgtlvl = level;
	D3DMAT_Copy (&RenderParam.wmat, &trans);

	RenderParam.viewap = atan(diagscale * scn->GetCamera()->GetTanAp());

	int startlvl = min (level, 8);
	int hemisp, ilat, ilng, idx;
	int  nlat = NLAT[startlvl];
	int *nlng = NLNG[startlvl];
	int texofs = patchidx[startlvl-1];
	TILEDESC *td = tiledesc + texofs;
	TEXCRDRANGE range = {0,1,0,1};
	tilebuf = TileManager::tilebuf;

	MATRIX3 rcam = *scn->GetCamera()->GetGRot();
	rcam = mul (ecl2gal, rcam);
	RenderParam.camdir = _V(rcam.m13, rcam.m23, rcam.m33);

	dev->SetRenderState (D3DRENDERSTATE_DESTBLEND, D3DBLEND_ONE);
	dev->SetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, TRUE);
	dev->SetRenderState (D3DRENDERSTATE_CULLMODE, D3DCULL_CW);
	dev->SetRenderState(D3DRENDERSTATE_TEXTUREFACTOR, D3DRGBA(intens, intens, intens, bgscale));

	int stage = 0;
	if (m_bBkgImg) {
		dev->SetTextureStageState(stage, D3DTSS_ADDRESS, D3DTADDRESS_CLAMP);
		dev->SetTextureStageState(stage, D3DTSS_COLORARG1, D3DTA_TEXTURE);
		if (intens < 1 - 1e-3) { // scale background image according to user setting
			dev->SetTextureStageState(stage, D3DTSS_COLOROP, D3DTOP_MODULATE);
			dev->SetTextureStageState(stage, D3DTSS_COLORARG2, D3DTA_TFACTOR);
		}
		else {
			dev->SetTextureStageState(stage, D3DTSS_COLOROP, D3DTOP_SELECTARG1);
		}
		if (bglvl > 1e-3) {
			dev->SetTextureStageState(stage, D3DTSS_ALPHAOP, D3DTOP_SELECTARG1);
			dev->SetTextureStageState(stage, D3DTSS_ALPHAARG1, D3DTA_TFACTOR);
		}
		else {
			dev->SetTextureStageState(stage, D3DTSS_ALPHAOP, D3DTOP_DISABLE);
		}
		stage++;
	}
	if (m_bStarImg) {
		dev->SetTextureStageState(stage, D3DTSS_ADDRESS, D3DTADDRESS_CLAMP);
		dev->SetTextureStageState(stage, D3DTSS_COLOROP, stage ? D3DTOP_ADD : D3DTOP_SELECTARG1);
		dev->SetTextureStageState(stage, D3DTSS_COLORARG1, D3DTA_TEXTURE);
		dev->SetTextureStageState(stage, D3DTSS_COLORARG2, D3DTA_CURRENT);
		if (bglvl > 1e-3) {
			dev->SetTextureStageState(stage, D3DTSS_ALPHAOP, D3DTOP_SELECTARG1);
			dev->SetTextureStageState(stage, D3DTSS_ALPHAARG1, D3DTA_TFACTOR);
		}
		else {
			dev->SetTextureStageState(stage, D3DTSS_ALPHAOP, D3DTOP_DISABLE);
		}
		stage++;
	}

	WaitForSingleObject (tilebuf->hQueueMutex, INFINITE);
	for (hemisp = idx = 0; hemisp < 2; hemisp++) {
		if (hemisp) { // flip world transformation to southern hemisphere
			D3DMAT_MatrixMultiply (&RenderParam.wmat, &RenderParam.wmat, &TileManager::Rsouth);
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
	ReleaseMutex (tilebuf->hQueueMutex);

	// Restore default render and texture states
	dev->SetRenderState(D3DRENDERSTATE_DESTBLEND, D3DBLEND_INVSRCALPHA);
	dev->SetRenderState(D3DRENDERSTATE_ALPHABLENDENABLE, FALSE);
	dev->SetRenderState(D3DRENDERSTATE_CULLMODE, D3DCULL_CCW);
	dev->SetTextureStageState(0, D3DTSS_ADDRESS, D3DTADDRESS_WRAP);
	dev->SetTexture(0, 0);

	if (intens < 1.0f) {
		dev->SetRenderState(D3DRENDERSTATE_TEXTUREFACTOR, 0xFFFFFFFF);
		dev->SetTextureStageState(0, D3DTSS_COLORARG2, D3DTA_DIFFUSE);
	}
	else {
		dev->SetTextureStageState(0, D3DTSS_COLOROP, D3DTOP_MODULATE);
	}

	if (stage) {
		dev->SetTextureStageState(0, D3DTSS_ALPHAOP, D3DTOP_SELECTARG1);
		dev->SetTextureStageState(0, D3DTSS_ALPHAARG1, D3DTA_TEXTURE);
		if (stage >= 2) {
			dev->SetTextureStageState(1, D3DTSS_ADDRESS, D3DTADDRESS_WRAP);
			dev->SetTextureStageState(1, D3DTSS_COLOROP, D3DTOP_DISABLE);
			dev->SetTextureStageState(1, D3DTSS_ALPHAOP, D3DTOP_DISABLE);
			dev->SetTextureStageState(1, D3DTSS_ALPHAARG1, D3DTA_TEXTURE);
			dev->SetTexture(1, 0);
		}
	}
}

// =======================================================================

void CSphereManager::ProcessTile (int lvl, int hemisp, int ilat, int nlat, int ilng, int nlng, TILEDESC *tile,
	const TEXCRDRANGE &range, LPDIRECTDRAWSURFACE7 tex, LPDIRECTDRAWSURFACE7 ltex, DWORD flag,
	const TEXCRDRANGE &bkp_range, LPDIRECTDRAWSURFACE7 bkp_tex, LPDIRECTDRAWSURFACE7 bkp_ltex, DWORD bkp_flag)
{
	ntot++;

	static const double rad0 = sqrt(2.0)*PI05;
	VECTOR3 cnt = TileCentre (hemisp, ilat, nlat, ilng, nlng);
	double rad = rad0/(double)nlat;
	double alpha = acos (dotp (RenderParam.camdir, cnt));
	double adist = alpha - rad;
	if (adist > RenderParam.viewap) return;

	nrad++;

	SetWorldMatrix (ilng, nlng, ilat, nlat);

	// Check if patch bounding box intersects viewport
	if (!TileInView (lvl, ilat)) {
		//tilebuf->DeleteSubTiles (tile); // remove tile descriptions below
		return;
	}

	RenderTile (lvl, hemisp, ilat, nlat, ilng, nlng, tile, range, tex, ltex, flag);
	nrender++;
}

// =======================================================================

void CSphereManager::SetWorldMatrix (int ilng, int nlng, int ilat, int nlat)
{
	// set up world transformation matrix
	D3DMATRIX rtile, wtrans;

	double lng = PI2 * (double)ilng/(double)nlng + PI; // add pi so texture wraps at +-180°
	D3DMAT_RotY (&rtile, lng);

	D3DMAT_MatrixMultiply (&wtrans, &RenderParam.wmat, &rtile);
	RenderParam.dev->SetTransform (D3DTRANSFORMSTATE_WORLD, &wtrans);
}

// =======================================================================

void CSphereManager::RenderTile (int lvl, int hemisp, int ilat, int nlat, int ilng, int nlng,
	TILEDESC *tile, const TEXCRDRANGE &range, LPDIRECTDRAWSURFACE7 tex, LPDIRECTDRAWSURFACE7 ltex, DWORD flag)
{
	LPDIRECT3DVERTEXBUFFER7 vb;        // processed vertex buffer
	VBMESH &mesh = PATCH_TPL[lvl][ilat]; // patch template

	if (range.tumin == 0 && range.tumax == 1) {
		vb = mesh.vb; // use vertex buffer directly
	} else {
		if (!tile->vtx) {
			D3DVERTEXBUFFERDESC vbd = 
				{ sizeof(D3DVERTEXBUFFERDESC), vbMemCaps | D3DVBCAPS_WRITEONLY, FVF_2TEX, mesh.nv };
			gc->GetDirect3D7()->CreateVertexBuffer (&vbd, &tile->vtx, 0);
			ApplyPatchTextureCoordinates (mesh, tile->vtx, range);
			tile->vtx->Optimize (RenderParam.dev, 0); // no more change, so we can optimize
		}
		vb = tile->vtx; // use buffer with transformed texture coords
	}

	int stage = 0;
	if (m_bBkgImg)
		RenderParam.dev->SetTexture(stage++, tex);
	if (m_bStarImg)
		RenderParam.dev->SetTexture(stage++, ltex);

	RenderParam.dev->DrawIndexedPrimitiveVB (D3DPT_TRIANGLELIST, vb, 0,
		mesh.nv, mesh.idx, mesh.ni, 0);
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

void CSphereManager::TileExtents (int hemisp, int ilat, int nlat, int ilng, int nlng, double &lat1, double &lat2, double &lng1, double &lng2)
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
	bool bx1, bx2, by1, by2, bz1, bz2, bbvis;
	int v;
	D3DVALUE x, y;
	VERTEX_XYZH *vtx;
	VBMESH &mesh = PATCH_TPL[lvl][ilat];
	bbtarget->ProcessVertices (D3DVOP_TRANSFORM, 0, 8, mesh.bb, 0, RenderParam.dev, 0);
	bbtarget->Lock (DDLOCK_WAIT | DDLOCK_READONLY | DDLOCK_DISCARDCONTENTS, (LPVOID*)&vtx, NULL);
	bx1 = bx2 = by1 = by2 = bz1 = bz2 = bbvis = false;
	for (v = 0; v < 8; v++) {
		if (vtx[v].z > 0.0)  bz1 = true;
		if (vtx[v].z <= 1.0) bz2 = true;
		if (vtx[v].z <= 1.0) x =  vtx[v].x, y =  vtx[v].y;
		else                 x = -vtx[v].x, y = -vtx[v].y;
		if (x > vpX0)        bx1 = true;
		if (x < vpX1)        bx2 = true;
		if (y > vpY0)        by1 = true;
		if (y < vpY1)        by2 = true;
		if (bbvis = bx1 && bx2 && by1 && by2 && bz1 && bz2) break;
	}
	bbtarget->Unlock();
	return bbvis;
}

