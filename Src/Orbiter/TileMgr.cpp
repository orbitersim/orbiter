// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// =======================================================================
// TileManager: Rendering of planetary surfaces using texture tiles at
// variable resolutions.
// =======================================================================

#include "D3dmath.h"
#include "D3d7util.h"
#include "Orbiter.h"
#include "Scene.h"
#include "VPlanet.h"
#include "TileMgr.h"
#include "Texture.h"
#include "Camera.h"
#include "Log.h"
#include "OGraphics.h"

// =======================================================================
// Externals

extern Orbiter *g_pOrbiter;
extern Camera *g_camera;
extern TextureManager2 *g_texmanager2;
extern DWORD g_vtxcount;
extern DWORD g_tilecount;
extern int patchidx[9];
extern DWORD VB_MemFlag; // dodgy
extern LPDIRECT3DVERTEXBUFFER7 bbtarget; // dodgy
extern VBMESH *PATCH_TPL[14];
extern int NLAT[9], *NLNG[9];
extern char DBG_MSG[256];

// =======================================================================
// Local prototypes

void ApplyPatchTextureCoordinates (VBMESH &mesh, LPDIRECT3DVERTEXBUFFER7 vtx, const TEXCRDRANGE &range);

float spec_base = 0.95f;
D3DMATERIAL7 pmat, watermat = {{1,1,1,1},{1,1,1,1},{spec_base,spec_base,spec_base,1},{0,0,0,0},20.0f};

int nrender[15]; // temporary
int tex_alloc = 0; // temporary
int tex_copy = 0; // temporary
int tmissing = 0;

const long TILESIZE = 32896; // size of tile texture - make more general or avoid!
const DWORD NOTILE = (DWORD)-1; // "no tile" flag

struct IDXLIST {
	DWORD idx, ofs;
};
int compare_idx (const void *el1, const void *el2);

// =======================================================================
// =======================================================================
// Class TileManager

bool        TileManager::bSpecular = false;
bool        TileManager::bLights = false;
D3DMATRIX   TileManager::Rsouth;
DWORD       TileManager::vpX0, TileManager::vpX1, TileManager::vpY0, TileManager::vpY1;
TileBuffer *TileManager::tilebuf = NULL;

TileManager::TileManager (const Planet *_cbody)
{
	cbody = _cbody;
	maxlvl = min (g_pOrbiter->Cfg()->CfgVisualPrm.PlanetMaxLevel, cbody->max_patch_level);
	maxlvl = min (maxlvl, SURF_MAX_PATCHLEVEL);
	maxbaselvl = min (8, maxlvl);
	int maxidx = patchidx[maxbaselvl];
	pcdir.Set (1,0,0);
	bRipple = (g_pOrbiter->Cfg()->CfgVisualPrm.bSpecularRipple && cbody->bWaterMicrotex);
	bPreloadTile = (g_pOrbiter->Cfg()->CfgPRenderPrm.PreloadMode > 0);
	lightfac = g_pOrbiter->Cfg()->CfgVisualPrm.LightBrightness;
	nmask = 0;
	nhitex = nhispec = 0;
	wavetex = 0;

	tiledesc = new TILEDESC[maxidx]; TRACENEW
	memset (tiledesc, 0, maxidx*sizeof(TILEDESC));

	LoadPatchData ();	
	LoadTileData ();
	LoadTextures ();
	LoadSpecularMasks();

	if (!bSpecular) bRipple = false;
	spec_base = (bRipple ? 1.05f : 0.95f);
	watermat.power = (bRipple ? 20.0f : 25.0f);
	watermat.specular.r = watermat.specular.g = watermat.specular.b = spec_base;
}

// =======================================================================

TileManager::~TileManager ()
{
	DWORD i, maxidx = patchidx[maxbaselvl];
	DWORD counter = 0;

	if (ntex) {
		for (i = 0; i < ntex; i++) {
			texbuf[i]->Release();
			if (!(++counter % 100))
				g_pOrbiter->UpdateDeallocationProgress();
		}
		delete []texbuf;
	}
	if (nmask) {
		for (i = 0; i < nmask; i++) {
			specbuf[i]->Release();
			if (!(++counter % 100))
				g_pOrbiter->UpdateDeallocationProgress();
		}
		delete []specbuf;
	}
	for (i = 0; i < maxidx; i++) {
		if (tiledesc[i].vtx) tiledesc[i].vtx->Release();
		if (!(++counter % 100))
			g_pOrbiter->UpdateDeallocationProgress();
	}
	delete []tiledesc;
	if (wavetex) wavetex->Release();
	g_pOrbiter->UpdateDeallocationProgress();
}

// =======================================================================

bool TileManager::LoadPatchData ()
{
	// Read information about specular reflective patch masks
	// from a binary data file

	FILE *binf;
	BYTE minres, maxres, flag;
	int i, idx, npatch;
	nmask = 0;

	if (!(bSpecular || bLights) ||
		!(binf = g_pOrbiter->OpenTextureFile (cbody->Name(), "_lmask.bin"))) {

		for (i = 0; i < patchidx[maxbaselvl]; i++)
			tiledesc[i].flag = 1;
		return false; // no specular reflections, no city lights

	} else {

		WORD *tflag = 0;
		LMASKFILEHEADER lmfh;
		fread (&lmfh, sizeof (lmfh), 1, binf);
		if (!strncmp (lmfh.id, "PLTA0100", 8)) { // v.1.00 format
			minres = lmfh.minres;
			maxres = lmfh.maxres;
			npatch = lmfh.npatch;
			tflag = new WORD[npatch]; TRACENEW
			fread (tflag, sizeof(WORD), npatch, binf);
		} else {                                 // pre-v.1.00 format
			fseek (binf, 0, SEEK_SET);
			fread (&minres, 1, 1, binf);
			fread (&maxres, 1, 1, binf);
			npatch = patchidx[maxres] - patchidx[minres-1];
			tflag = new WORD[npatch]; TRACENEW
			for (i = 0; i < npatch; i++) {
				fread (&flag, 1, 1, binf);
				tflag[i] = flag;
			}
			LOGOUT_WARN("Old-style texture contents file %s_lmask.bin", cbody->Name());
		}
		fclose (binf);

		for (i = idx = 0; i < patchidx[maxbaselvl]; i++) {
			if (i < patchidx[minres-1]) {
				tiledesc[i].flag = 1; // no mask information -> assume opaque, no lights
			} else {
				flag = (BYTE)tflag[idx++];
				tiledesc[i].flag = flag;
				if (((flag & 3) == 3) || (flag & 4))
					nmask++;
			}
		}
		if (tflag) delete []tflag;
		return true;

	}
}

// =======================================================================

bool TileManager::LoadTileData ()
{
	FILE *file;

	if (maxlvl <= 8) // no tile data required
		return false;
	if (!(file = g_pOrbiter->OpenTextureFile (cbody->Name(), "_tile.bin")))
		return false;

	DWORD n, i, j;
	char idstr[9] = "        ";
	fread (idstr, 1, 8, file);
	if (!strncmp (idstr, "PLTS", 4)) {
		tilever = 1;
	} else { // no header: old-style file format
		tilever = 0;
		fseek (file, 0, SEEK_SET);
	}
	fread (&n, sizeof(DWORD), 1, file);
	TILEFILESPEC *tfs = new TILEFILESPEC[n]; TRACENEW
	fread (tfs, sizeof(TILEFILESPEC), n, file);

	if (bPreloadTile) {
		if (tilever >= 1) {   //  convert texture offsets to indices
			IDXLIST *idxlist = new IDXLIST[n];
			for (i = 0; i < n; i++) {
				idxlist[i].idx = i;
				idxlist[i].ofs = tfs[i].sidx;
			}
			qsort (idxlist, n, sizeof(IDXLIST), compare_idx);
			for (i = 0; i < n && idxlist[i].ofs != NOTILE; i++)
				tfs[idxlist[i].idx].sidx = i;

			for (i = 0; i < n; i++) {
				idxlist[i].idx = i;
				idxlist[i].ofs = tfs[i].midx;
			}
			qsort (idxlist, n, sizeof(IDXLIST), compare_idx);
			for (i = 0; i < n && idxlist[i].ofs != NOTILE; i++)
				tfs[idxlist[i].idx].midx = i;

			tilever = 0;
			delete []idxlist;
		}
	}

	TILEDESC *tile8 = tiledesc + patchidx[7];
	for (i = 0; i < 364; i++) { // loop over level 8 tiles
		TILEDESC &tile8i = tile8[i];
		for (j = 0; j < 4; j++)
			if (tfs[i].subidx[j])
				AddSubtileData (tile8i, tfs, i, j, 9);
	}

	fclose (file);
	delete []tfs;
	return true;
}

// =======================================================================

int compare_idx (const void *el1, const void *el2)
{
	IDXLIST *idx1 = (IDXLIST*)el1;
	IDXLIST *idx2 = (IDXLIST*)el2;
	return (idx1->ofs < idx2->ofs ? -1 : idx1->ofs > idx2->ofs ? 1 : 0);
}

// =======================================================================

bool TileManager::AddSubtileData (TILEDESC &td, TILEFILESPEC *tfs, DWORD idx, DWORD sub, DWORD lvl)
{
	DWORD j, subidx = tfs[idx].subidx[sub];
	TILEFILESPEC &t = tfs[subidx];
	bool bSubtiles = false;
	for (j = 0; j < 4; j++)
		if (t.subidx[j]) { bSubtiles = true; break; }
	if (t.flags || bSubtiles) {
		if (lvl <= maxlvl) {
			td.subtile[sub] = tilebuf->AddTile();
			td.subtile[sub]->flag = t.flags;
			td.subtile[sub]->tex = (LPDIRECTDRAWSURFACE7)t.sidx;
			if (td.subtile[sub]->tex) tex_copy++;
			if (bSpecular || bLights) {
				if (t.midx != NOTILE) {
					td.subtile[sub]->ltex = (LPDIRECTDRAWSURFACE7)t.midx;
					if (td.subtile[sub]->ltex) tex_copy++;
				}
			}
			if (!bSpecular) td.subtile[sub]->flag = (td.subtile[sub]->flag & 4) + 1;
			if (!bLights)   td.subtile[sub]->flag = (td.subtile[sub]->flag & 3);
			td.subtile[sub]->flag |= 0x80; // 'Not-loaded' flag
			if (!tilever)
				td.subtile[sub]->flag |= 0x40; // 'old-style index' flag
			// recursively step down to higher resolutions
			if (bSubtiles) {
				for (j = 0; j < 4; j++) {
					if (t.subidx[j]) AddSubtileData (*td.subtile[sub], tfs, subidx, j, lvl+1);
				}
			}
			nhitex++;
			if (t.midx != NOTILE) nhispec++;
		} else td.subtile[sub] = NULL;
	}
	return true;
}

// =======================================================================

void TileManager::LoadTextures ()
{
	int i;

	// pre-load level 1-8 textures
	ntex = patchidx[maxbaselvl];
	texbuf = new LPDIRECTDRAWSURFACE7[ntex]; TRACENEW
	if (ntex = g_texmanager2->OpenTextures (cbody->Name(), ".tex", texbuf, ntex)) {
		while (ntex < patchidx[maxbaselvl]) maxlvl = --maxbaselvl;
		while (ntex > patchidx[maxbaselvl]) texbuf[--ntex]->Release();
		// not enough textures loaded for requested resolution level
		for (i = 0; i < patchidx[maxbaselvl]; i++)
			tiledesc[i].tex = texbuf[i];
	} else {
		delete []texbuf;
		texbuf = 0;
		// no textures at all!
	}

	//  pre-load highres tile textures
	if (bPreloadTile && nhitex) {
		TILEDESC *tile8 = tiledesc + patchidx[7];
		PreloadTileTextures (tile8, nhitex, nhispec);
	}

	// load specular ripple microtexture
	if (bSpecular && bRipple) {
		FILE *file;
		if (file = g_pOrbiter->OpenTextureFile ("waves", ".dds")) {
			g_texmanager2->ReadTexture (file, &wavetex);
			fclose (file);
		}
	}
}

// =======================================================================

void TileManager::PreloadTileTextures (TILEDESC *tile8, DWORD ntex, DWORD nmask)
{
	// Load tile surface and mask/light textures, and copy them into the tile tree

	DWORD i, j, nt = 0, nm = 0;
	LPDIRECTDRAWSURFACE7 *texbuf = NULL, *maskbuf = NULL;

	if (ntex) {  // load surface textures
		texbuf = new LPDIRECTDRAWSURFACE7[ntex]; TRACENEW
		nt = g_texmanager2->OpenTextures (cbody->Name(), "_tile.tex", texbuf, ntex);
	}
	if (nmask) { // load mask/light textures
		maskbuf = new LPDIRECTDRAWSURFACE7[nmask]; TRACENEW
		nm = g_texmanager2->OpenTextures (cbody->Name(), "_tile_lmask.tex", maskbuf, nmask);
	}
	// copy textures into tile tree
	for (i = 0; i < 364; i++) {
		TILEDESC *tile8i = tile8+i;
		for (j = 0; j < 4; j++)
			if (tile8i->subtile[j])
				AddSubtileTextures (tile8i->subtile[j], texbuf, nt, maskbuf, nm);
	}
	// release unused textures
	if (nt) {
		for (i = 0; i < nt; i++)
			if (texbuf[i])
				texbuf[i]->Release();
		delete []texbuf;
	}
	if (nm) {
		for (i = 0; i < nm; i++)
			if (maskbuf[i])
				maskbuf[i]->Release();
		delete []maskbuf;
	}
}

// =======================================================================

void TileManager::AddSubtileTextures (TILEDESC *td, LPDIRECTDRAWSURFACE7 *tbuf, DWORD nt, LPDIRECTDRAWSURFACE7 *mbuf, DWORD nm)
{
	DWORD i;

	DWORD tidx = (DWORD)td->tex;  // copy surface texture
	if (tidx != NOTILE) {
		if (tidx < nt) {
			td->tex = tbuf[tidx];
			tbuf[tidx] = NULL;
		} else {                   // inconsistency
			tmissing++;
			td->tex = NULL;
		}
	} else td->tex = NULL;

	DWORD midx = (DWORD)td->ltex;  // copy mask/light texture
	if (midx != NOTILE) {
		if (midx < nm) {
			td->ltex = mbuf[midx];
			mbuf[midx] = NULL;
		} else {                  // inconsistency
			tmissing++;
			td->ltex = NULL;
		}
	} else td->ltex = NULL;
	td->flag &= ~0x80; // remove "not loaded" flag

	for (i = 0; i < 4; i++) {
		if (td->subtile[i]) AddSubtileTextures (td->subtile[i], tbuf, nt, mbuf, nm);
	}
}

// =======================================================================

void TileManager::LoadSpecularMasks ()
{
	int i;
	DWORD n;

	if (nmask) {
		FILE *file;

		if (file = g_pOrbiter->OpenTextureFile (cbody->Name(), "_lmask.tex")) {
			specbuf = new LPDIRECTDRAWSURFACE7[nmask]; TRACENEW
			n = g_texmanager2->ReadTextures (file, specbuf, nmask);
			fclose (file);
			if (n < nmask) {
				LOGOUT_WARN("Transparency texture mask file too short: %s_lmask.tex\nDisabling specular reflection for this planet",
					cbody->Name());
				delete []specbuf;
				specbuf = NULL;
				nmask = 0;
				for (i = 0; i < patchidx[maxbaselvl]; i++)
					tiledesc[i].flag = 1;
			} else {
				for (i = n = 0; i < patchidx[maxbaselvl]; i++) {
					if (((tiledesc[i].flag & 3) == 3) || (tiledesc[i].flag & 4)) {
						if (n < nmask) tiledesc[i].ltex = specbuf[n++];
						else tiledesc[i].flag = 1;
					}
					if (!bLights) tiledesc[i].flag &= 0xFB;
					if (!bSpecular) tiledesc[i].flag &= 0xFD, tiledesc[i].flag |= 1;
				}
			}
		} else {
			LOGOUT_WARN("Transparency texture mask file not found: %s_lmask.tex\nDisabling specular reflection for this planet",
				cbody->Name());
			nmask = 0;
			for (i = 0; i < patchidx[maxbaselvl]; i++)
				tiledesc[i].flag = 1;
		}
	}
}

// =======================================================================

void TileManager::CreateDeviceObjects (LPDIRECT3D7 d3d, LPDIRECT3DDEVICE7 dev)
{
	D3DVIEWPORT7 vp;
	dev->GetViewport (&vp);
	vpX0 = vp.dwX, vpX1 = vpX0 + vp.dwWidth;
	vpY0 = vp.dwY, vpY1 = vpY0 + vp.dwHeight;
	// viewport size for clipping calculations

	bSpecular = g_pOrbiter->Cfg()->CfgVisualPrm.bWaterreflect;
	bLights = g_pOrbiter->Cfg()->CfgVisualPrm.bNightlights;

	VMAT_rotx (Rsouth, Pi);
	// rotation matrix for flipping patches onto southern hemisphere

	tilebuf = new TileBuffer; TRACENEW
	// system-wide tile cache
}

// =======================================================================

void TileManager::DestroyDeviceObjects ()
{
	delete tilebuf;
}

// =======================================================================

void TileManager::Stop ()
{
	if (tilebuf) tilebuf->StopLoadThread();
}

// =======================================================================

void TileManager::Render (LPDIRECT3DDEVICE7 dev, D3DMATRIX &wmat, double scale, VPlanet *vbody, int level, bool addambient, bool addfog)
{
	level = min (level, maxlvl);
	memset(nrender,0,15*sizeof(int)); // temporary
	
	RenderParam.dev  = dev;
	RenderParam.vbody = vbody;
	VMAT_copy (RenderParam.wmat, wmat);
	VMAT_copy (RenderParam.wmat_tmp, wmat);
	RenderParam.grot = cbody->GRot() * scale;
	RenderParam.cpos = vbody->CPos() * scale;
	RenderParam.cdir.Set (tmul (cbody->GRot(), g_camera->GPos()-cbody->GPos()));
	RenderParam.cdist = RenderParam.cdir.length()/cbody->Size();
	RenderParam.cdir.unify();
	RenderParam.sdir.Set (tmul (cbody->GRot(), -cbody->GPos()));
	RenderParam.sdir.unify();
	RenderParam.viewap = acos (1.0/(max (RenderParam.cdist, 1.0)));
	RenderParam.fog = addfog;

	// limit resolution for fast camera movements
	double limitstep, cstep = acos (dotp (RenderParam.cdir, pcdir));
	int maxlevel = SURF_MAX_PATCHLEVEL;
	static double limitstep0 = 5.12 * pow(2.0, -(double)SURF_MAX_PATCHLEVEL);
	for (limitstep = limitstep0; cstep > limitstep && maxlevel > 5; limitstep *= 2.0)
		maxlevel--;
	level = min (level, maxlevel);

	RenderParam.tgtlvl = level;

	int startlvl = min (level, 8);
	int hemisp, ilat, ilng, idx;
	int  nlat = NLAT[startlvl];
	int *nlng = NLNG[startlvl];
	int texofs = patchidx[startlvl-1];
	TILEDESC *td = tiledesc + texofs;
	
	TEXCRDRANGE range = {0,1,0,1};

	// modify colour of specular reflection component
	bool specmod = (bSpecular && SpecularColour (&watermat.specular));

	// for planets seen through an atmospheric layer from the surface of
	// another planet, add the ambient atmosphere colour to the rendering
	if (addambient) {
		dev->SetTextureStageState (1, D3DTSS_COLOROP, D3DTOP_ADD);
		dev->SetTextureStageState (1, D3DTSS_COLORARG1, D3DTA_CURRENT);
		dev->SetTextureStageState (1, D3DTSS_COLORARG2, D3DTA_TFACTOR);
		VECTOR3 bgc = g_pOrbiter->GetInlineGraphicsClient()->GetScene()->BGcol();
		dev->SetRenderState (D3DRENDERSTATE_TEXTUREFACTOR, D3DRGBA(bgc.x, bgc.y, bgc.z, 1));
	}

	dVERIFY (dev->SetTextureStageState (0, D3DTSS_ADDRESS, D3DTADDRESS_CLAMP), "LPDIRECT3DDEVICE7::SetTextureStageState failed");

	if (level <= 4) { // simple full-sphere render

		RenderSimple (level, td);

	} else {          // render visible patches only

		WaitForSingleObject (tilebuf->hQueueMutex, INFINITE);
		for (hemisp = idx = 0; hemisp < 2; hemisp++) {
			if (hemisp) { // flip world transformation to southern hemisphere
				D3DMath_MatrixMultiply (RenderParam.wmat, RenderParam.wmat, Rsouth);
				VMAT_copy (RenderParam.wmat_tmp, RenderParam.wmat);
				RenderParam.grot.m12 = -RenderParam.grot.m12;
				RenderParam.grot.m13 = -RenderParam.grot.m13;
				RenderParam.grot.m22 = -RenderParam.grot.m22;
				RenderParam.grot.m23 = -RenderParam.grot.m23;
				RenderParam.grot.m32 = -RenderParam.grot.m32;
				RenderParam.grot.m33 = -RenderParam.grot.m33;
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
	}

	dVERIFY (dev->SetTextureStageState (0, D3DTSS_ADDRESS, D3DTADDRESS_WRAP), "LPDIRECT3DDEVICE7::SetTextureStageState failed");

	if (specmod)
		watermat.specular.r = watermat.specular.g = watermat.specular.b = 1.0f;

	if (addambient) {
		dev->SetTextureStageState (1, D3DTSS_COLOROP, D3DTOP_DISABLE);
		dev->SetTextureStageState (1, D3DTSS_COLORARG1, D3DTA_TEXTURE);
		dev->SetTextureStageState (1, D3DTSS_COLORARG2, D3DTA_CURRENT);
	}

	pcdir.Set (RenderParam.cdir); // store camera direction

	// temporary
	//if (!strcmp (cbody->Name(), "Earth"))
	//	sprintf (DBG_MSG, "Tiles rendered: %d (14), %d (13), %d (12), %d (11), %d (10), %d (9) %d (8)", nrender[14], nrender[13], nrender[12], nrender[11], nrender[10], nrender[9], nrender[8]);
}

// =======================================================================

void TileManager::ProcessTile (int lvl, int hemisp, int ilat, int nlat, int ilng, int nlng, TILEDESC *tile,
	const TEXCRDRANGE &range, LPDIRECTDRAWSURFACE7 tex, LPDIRECTDRAWSURFACE7 ltex, DWORD flag,
	const TEXCRDRANGE &bkp_range, LPDIRECTDRAWSURFACE7 bkp_tex, LPDIRECTDRAWSURFACE7 bkp_ltex, DWORD bkp_flag)
{
	// Check if patch is visible from camera position
	static const double rad0 = sqrt(2.0)*Pi05*0.5;
	Vector cnt = TileCentre (hemisp, ilat, nlat, ilng, nlng);
	double rad = rad0/(double)nlat;
	double alpha = acos (dotp (RenderParam.cdir, cnt)); // angle between tile centre and camera from planet centre
	double adist = alpha - rad;                         // angle between closest tile corner and camera
	if (adist >= RenderParam.viewap) {
		tilebuf->DeleteSubTiles (tile); // remove tile descriptions below
		return;
	}

	// Set world transformation matrix for patch
	SetWorldMatrix (ilng, nlng, ilat, nlat);

	// Check if patch bounding box intersects viewport
	if (!TileInView (lvl, ilat)) {
		tilebuf->DeleteSubTiles (tile); // remove tile descriptions below
		return;
	}

	// Reduce resolution for distant or oblique patches
	bool bStepDown = (lvl < RenderParam.tgtlvl);
	bool bCoarseTex = false;

	if (bStepDown && lvl >= 8 && adist > 0.0) {
		double lat1, lat2, lng1, lng2, clat, clng, crad;
		double adist_lng, adist_lat, adist2;
		TileExtents (hemisp, ilat, nlat, ilng, nlng, lat1, lat2, lng1, lng2);
		cbody->LocalToEquatorial (RenderParam.cdir, clng, clat, crad);
		if      (clng < lng1-Pi) clng += Pi2;
		else if (clng > lng2+Pi) clng -= Pi2;
		if      (clng < lng1) adist_lng = lng1-clng;
		else if (clng > lng2) adist_lng = clng-lng2;
		else                  adist_lng = 0.0;
		if      (clat < lat1) adist_lat = lat1-clat;
		else if (clat > lat2) adist_lat = clat-lat2;
		else                  adist_lat = 0.0;
		adist2 = max (adist_lng, adist_lat);

		// reduce resolution further for tiles that are visible
		// under a very oblique angle
		double cosa = cos(adist2);
		double a = sin(adist2);
		double b = RenderParam.cdist-cosa;
		double ctilt = b*cosa/sqrt(a*a*(1.0+2.0*b)+b*b); // tile visibility tilt angle cosine
		if (adist2 > rad*(2.0*ctilt+0.3)) {
			bStepDown = false;
			if (adist2 > rad*(4.2*ctilt+0.3))
				bCoarseTex = true;
		}
	}

	// Recursion to next level: subdivide into 2x2 patch
	if (bStepDown) {
		int i, j, idx = 0;
		float du = (range.tumax-range.tumin) * 0.5f;
		float dv = (range.tvmax-range.tvmin) * 0.5f;
		TEXCRDRANGE subrange;
		static TEXCRDRANGE fullrange = {0,1,0,1};
		for (i = 1; i >= 0; i--) {
			subrange.tvmax = (subrange.tvmin = range.tvmin + (1-i)*dv) + dv;
			for (j = 0; j < 2; j++) {
				subrange.tumax = (subrange.tumin = range.tumin + j*du) + du;
				TILEDESC *subtile = tile->subtile[idx];
				bool isfull = true;
				if (!subtile) {
					tile->subtile[idx] = subtile = tilebuf->AddTile();
					isfull = false;
				} else if (subtile->flag & 0x80) {
					if ((tile->flag & 0x80) == 0)
						tilebuf->LoadTileAsync (cbody->Name(), subtile);
					isfull = false;
				}
				if (isfull)
					isfull = (subtile->tex != NULL);
				if (isfull)
					ProcessTile (lvl+1, hemisp, ilat*2+i, nlat*2, ilng*2+j, nlng*2, subtile,
						fullrange, subtile->tex, subtile->ltex, subtile->flag,
						subrange, tex, ltex, flag);
				else
					ProcessTile (lvl+1, hemisp, ilat*2+i, nlat*2, ilng*2+j, nlng*2, subtile,
						subrange, tex, ltex, flag,
						subrange, tex, ltex, flag);
				idx++;
			}
		}
	} else {
		// actually render the tile at this level
		double sdist = acos (dotp (RenderParam.sdir, cnt));
		if (bCoarseTex) {
			if (sdist > Pi05+rad && bkp_flag & 2) bkp_flag &= 0xFD, bkp_flag |= 1; // supress specular reflection on dark side
			RenderTile (lvl, hemisp, ilat, nlat, ilng, nlng, sdist, tile, bkp_range, bkp_tex, bkp_ltex, bkp_flag);
		} else {
			if (sdist > Pi05+rad && flag & 2) flag &= 0xFD, flag |= 1; // supress specular reflection on dark side
			RenderTile (lvl, hemisp, ilat, nlat, ilng, nlng, sdist, tile, range, tex, ltex, flag);
		}
		tilebuf->DeleteSubTiles (tile); // remove tile descriptions below
	}
}

// =======================================================================

void TileManager::RenderTile (int lvl, int hemisp, int ilat, int nlat, int ilng, int nlng, double sdist,
	TILEDESC *tile, const TEXCRDRANGE &range, LPDIRECTDRAWSURFACE7 tex, LPDIRECTDRAWSURFACE7 ltex, DWORD flag)
{
	g_tilecount++;

	LPDIRECT3DVERTEXBUFFER7 vb;        // processed vertex buffer
	VBMESH &mesh = PATCH_TPL[lvl][ilat]; // patch template

	if (range.tumin == 0 && range.tumax == 1) {
		vb = mesh.vb; // use vertex buffer directly
	} else {
		if (!tile->vtx) {
			D3DVERTEXBUFFERDESC vbd = 
				{ sizeof(D3DVERTEXBUFFERDESC), VB_MemFlag | D3DVBCAPS_WRITEONLY, FVF_2TEX, mesh.nv };
			g_pOrbiter->GetInlineGraphicsClient()->GetDirect3D7()->CreateVertexBuffer (&vbd, &tile->vtx, 0);
			ApplyPatchTextureCoordinates (mesh, tile->vtx, range);
			tile->vtx->Optimize (RenderParam.dev, 0); // no more change, so we can optimize
		}
		vb = tile->vtx; // use buffer with transformed texture coords
	}

	bool hasspec = ((flag & 2) == 2);
	bool purespec = ((flag & 3) == 2);
	bool mixedspec = ((flag & 3) == 3);
	bool spec_singlerender = (purespec && !bRipple);
	bool lights = ((flag & 4) && (sdist > 1.45));

	// step 1: render full patch diffusely
	RenderParam.dev->SetTexture (0, tex);
	RenderParam.dev->DrawIndexedPrimitiveVB (D3DPT_TRIANGLELIST, vb, 0,
		mesh.nv, mesh.idx, mesh.ni, 0);
	g_vtxcount += mesh.nv;

	// if there is no specularity and no emissive lights, we are done
	if (!hasspec && !lights) return;

	// disable fog on additional render passes
	if (RenderParam.fog) {
		RenderParam.dev->SetRenderState (D3DRENDERSTATE_FOGENABLE, FALSE);
		RenderParam.dev->SetRenderState (D3DRENDERSTATE_FOGTABLEMODE, D3DFOG_NONE);
	}

	// step 2: add city lights
	// note: I didn't find a way to include this as a texture stage in the
	// previous pass, because the lights need to be multiplied with a factor before
	// adding
	if (lights) {
		double fac = lightfac;
		if (sdist < 1.9) fac *= (sdist-1.45)/(1.9-1.45);
		RenderParam.dev->SetRenderState (D3DRENDERSTATE_TEXTUREFACTOR, D3DRGBA(fac,fac,fac,1));
		RenderParam.dev->SetTextureStageState (0, D3DTSS_COLOROP, D3DTOP_MODULATE);
		RenderParam.dev->SetTextureStageState (0, D3DTSS_COLORARG2, D3DTA_TFACTOR);
		RenderParam.dev->SetTexture (0, ltex);
		RenderParam.dev->SetTextureStageState (0, D3DTSS_COLORARG1, D3DTA_TEXTURE);
		RenderParam.dev->SetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, TRUE);
		RenderParam.dev->SetRenderState (D3DRENDERSTATE_DESTBLEND, D3DBLEND_ONE);
		RenderParam.dev->SetRenderState (D3DRENDERSTATE_SRCBLEND, D3DBLEND_ONE);

		RenderParam.dev->DrawIndexedPrimitiveVB (D3DPT_TRIANGLELIST, vb, 0,
			mesh.nv, mesh.idx, mesh.ni, 0);
		g_vtxcount += mesh.nv;

		RenderParam.dev->SetTextureStageState (0, D3DTSS_COLOROP, D3DTOP_MODULATE);
		RenderParam.dev->SetTextureStageState (0, D3DTSS_COLORARG2, D3DTA_CURRENT);
		RenderParam.dev->SetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, FALSE);
		RenderParam.dev->SetRenderState (D3DRENDERSTATE_SRCBLEND, D3DBLEND_SRCALPHA);
		RenderParam.dev->SetRenderState (D3DRENDERSTATE_DESTBLEND, D3DBLEND_INVSRCALPHA);
	}

	// step 3: add specular highlights (mixed patches only)
	if (mixedspec) {

		RenderParam.dev->GetMaterial (&pmat);
		RenderParam.dev->SetMaterial (&watermat);
		RenderParam.dev->SetRenderState (D3DRENDERSTATE_SPECULARENABLE, TRUE);
		RenderParam.dev->SetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, TRUE);
		RenderParam.dev->SetRenderState (D3DRENDERSTATE_SRCBLEND, D3DBLEND_INVSRCALPHA);
		RenderParam.dev->SetRenderState (D3DRENDERSTATE_DESTBLEND, D3DBLEND_ONE);
		RenderParam.dev->SetTexture (0, ltex);

		if (bRipple) {
			RenderParam.dev->SetTexture (1, wavetex);
			RenderParam.dev->SetTextureStageState (1, D3DTSS_COLOROP, D3DTOP_SELECTARG1);
			RenderParam.dev->SetTextureStageState (1, D3DTSS_COLORARG1, D3DTA_CURRENT);
			RenderParam.dev->SetTextureStageState (1, D3DTSS_ALPHAOP, D3DTOP_ADD/*MODULATE*/);
			RenderParam.dev->SetTextureStageState (1, D3DTSS_TEXCOORDINDEX, 1);
			RenderParam.dev->SetTextureStageState (1, D3DTSS_ADDRESS, D3DTADDRESS_WRAP);
		}

		RenderParam.dev->DrawIndexedPrimitiveVB (D3DPT_TRIANGLELIST, vb, 0,
			mesh.nv, mesh.idx, mesh.ni, 0);
		g_vtxcount += mesh.nv;

		if (bRipple) {
			RenderParam.dev->SetTexture (1, 0);
			RenderParam.dev->SetTextureStageState (1, D3DTSS_COLOROP, D3DTOP_DISABLE);
			RenderParam.dev->SetTextureStageState (1, D3DTSS_ALPHAOP, D3DTOP_DISABLE);
			RenderParam.dev->SetTextureStageState (1, D3DTSS_COLORARG1, D3DTA_TEXTURE);
			RenderParam.dev->SetTextureStageState (1, D3DTSS_TEXCOORDINDEX, 0);
		}

		RenderParam.dev->SetMaterial (&pmat);
		RenderParam.dev->SetRenderState (D3DRENDERSTATE_SPECULARENABLE, FALSE);
		RenderParam.dev->SetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, FALSE);
		RenderParam.dev->SetRenderState (D3DRENDERSTATE_SRCBLEND, D3DBLEND_SRCALPHA);
		RenderParam.dev->SetRenderState (D3DRENDERSTATE_DESTBLEND, D3DBLEND_INVSRCALPHA);

	} else if (purespec) {

		RenderParam.dev->GetMaterial (&pmat);
		RenderParam.dev->SetMaterial (&watermat);
		RenderParam.dev->SetRenderState (D3DRENDERSTATE_SPECULARENABLE, TRUE);
		RenderParam.dev->SetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, TRUE);
		RenderParam.dev->SetRenderState (D3DRENDERSTATE_DESTBLEND, D3DBLEND_ONE);
		RenderParam.dev->SetTextureStageState (0, D3DTSS_COLOROP, D3DTOP_SELECTARG1);
		RenderParam.dev->SetTextureStageState (0, D3DTSS_COLORARG1, D3DTA_TFACTOR);
		RenderParam.dev->SetRenderState (D3DRENDERSTATE_TEXTUREFACTOR, D3DRGBA(0,0,0,1));
		if (bRipple) {
			RenderParam.dev->SetTexture (0, wavetex);
			RenderParam.dev->SetTextureStageState (0, D3DTSS_TEXCOORDINDEX, 1);
			RenderParam.dev->SetTextureStageState (0, D3DTSS_ADDRESS, D3DTADDRESS_WRAP);
			RenderParam.dev->SetRenderState (D3DRENDERSTATE_SRCBLEND, D3DBLEND_INVSRCALPHA);
		} else {
			RenderParam.dev->SetTexture (0, 0);
		}

		RenderParam.dev->DrawIndexedPrimitiveVB (D3DPT_TRIANGLELIST, vb, 0,
			mesh.nv, mesh.idx, mesh.ni, 0);
		g_vtxcount += mesh.nv;

		if (bRipple) {
			RenderParam.dev->SetTextureStageState (0, D3DTSS_TEXCOORDINDEX, 0);
			RenderParam.dev->SetTextureStageState (0, D3DTSS_ADDRESS, D3DTADDRESS_CLAMP); // the default for planet surfaces
			RenderParam.dev->SetRenderState (D3DRENDERSTATE_SRCBLEND, D3DBLEND_SRCALPHA);
		}
		RenderParam.dev->SetMaterial (&pmat);
		RenderParam.dev->SetTextureStageState (0, D3DTSS_COLOROP, D3DTOP_MODULATE);
		RenderParam.dev->SetTextureStageState (0, D3DTSS_COLORARG1, D3DTA_TEXTURE);
		RenderParam.dev->SetRenderState (D3DRENDERSTATE_SPECULARENABLE, FALSE);
		RenderParam.dev->SetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, FALSE);
		RenderParam.dev->SetRenderState (D3DRENDERSTATE_DESTBLEND, D3DBLEND_INVSRCALPHA);
	}

	if (RenderParam.fog) {
		// turn fog back on
		RenderParam.dev->SetRenderState (D3DRENDERSTATE_FOGENABLE, TRUE);
		RenderParam.dev->SetRenderState (D3DRENDERSTATE_FOGTABLEMODE, D3DFOG_EXP);
	}
}

// =======================================================================

void TileManager::RenderSimple (int level, TILEDESC *tile)
{
	int idx, npatch = patchidx[level] - patchidx[level-1];
	g_tilecount += npatch;

	RenderParam.dev->SetTransform (D3DTRANSFORMSTATE_WORLD, &RenderParam.wmat);

	for (idx = 0; idx < npatch; idx++) {

		VBMESH &mesh = PATCH_TPL[level][idx]; // patch template
		bool purespec = ((tile[idx].flag & 3) == 2);
		bool mixedspec = ((tile[idx].flag & 3) == 3);

		// step 1: render full patch, either completely diffuse or completely specular
		if (purespec) { // completely specular
			RenderParam.dev->GetMaterial (&pmat);
			RenderParam.dev->SetMaterial (&watermat);
			RenderParam.dev->SetRenderState (D3DRENDERSTATE_SPECULARENABLE, TRUE);
		}
		RenderParam.dev->SetTexture (0, tile[idx].tex);
		RenderParam.dev->DrawIndexedPrimitiveVB (D3DPT_TRIANGLELIST, mesh.vb, 0,
			mesh.nv, mesh.idx, mesh.ni, 0);
		g_vtxcount += mesh.nv;
		if (purespec) {
			RenderParam.dev->SetMaterial (&pmat);
			RenderParam.dev->SetRenderState (D3DRENDERSTATE_SPECULARENABLE, FALSE);
		}

		// step 2: add specular highlights (mixed patches only)
		if (mixedspec) {
			RenderParam.dev->GetMaterial (&pmat);
			RenderParam.dev->SetMaterial (&watermat);
			RenderParam.dev->SetRenderState (D3DRENDERSTATE_SPECULARENABLE, TRUE);
			RenderParam.dev->SetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, TRUE);
			RenderParam.dev->SetRenderState (D3DRENDERSTATE_SRCBLEND, D3DBLEND_INVSRCALPHA);
			RenderParam.dev->SetRenderState (D3DRENDERSTATE_DESTBLEND, D3DBLEND_ONE);
			RenderParam.dev->SetTexture (0, tile[idx].ltex);
			RenderParam.dev->DrawIndexedPrimitiveVB (D3DPT_TRIANGLELIST, mesh.vb, 0,
				mesh.nv, mesh.idx, mesh.ni, 0);
			g_vtxcount += mesh.nv;
			RenderParam.dev->SetMaterial (&pmat);
			RenderParam.dev->SetRenderState (D3DRENDERSTATE_SPECULARENABLE, FALSE);
			RenderParam.dev->SetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, FALSE);
			RenderParam.dev->SetRenderState (D3DRENDERSTATE_SRCBLEND, D3DBLEND_SRCALPHA);
			RenderParam.dev->SetRenderState (D3DRENDERSTATE_DESTBLEND, D3DBLEND_INVSRCALPHA);
		}
	}
}

// =======================================================================
// returns the direction of the tile centre from the planet centre in local
// planet coordinates

Vector TileManager::TileCentre (int hemisp, int ilat, int nlat, int ilng, int nlng)
{
	double cntlat = Pi05 * ((double)ilat+0.5)/(double)nlat,      slat = sin(cntlat), clat = cos(cntlat);
	double cntlng = Pi2  * ((double)ilng+0.5)/(double)nlng + Pi, slng = sin(cntlng), clng = cos(cntlng);
	if (hemisp) return Vector (clat*clng, -slat, -clat*slng);
	else        return Vector (clat*clng,  slat,  clat*slng);
}

// =======================================================================

void TileManager::TileExtents (int hemisp, int ilat, int nlat, int ilng, int nlng, double &lat1, double &lat2, double &lng1, double &lng2)
{
	lat1 = Pi05 * (double)ilat/(double)nlat;
	lat2 = lat1 + Pi05/(double)nlat;
	lng1 = Pi2 * (double)ilng/(double)nlng + Pi;
	lng2 = lng1 + Pi2/nlng;
	if (hemisp) {
		double tmp = lat1; lat1 = -lat2; lat2 = -tmp;
		tmp = lng1; lng1 = -lng2; lng2 = -tmp;
		if (lng2 < 0) lng1 += Pi2, lng2 += Pi2;
	}
}

// =======================================================================

bool TileManager::TileInView (int lvl, int ilat)
{
	const double eps = 1e-3;
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
		if (vtx[v].z <= 1.0+eps) bz2 = true;
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

// =======================================================================

void TileManager::SetWorldMatrix (int ilng, int nlng, int ilat, int nlat)
{
	// set up world transformation matrix
	D3DMATRIX rtile, wtrans;
	double lng = Pi2 * (double)ilng/(double)nlng + Pi; // add pi so texture wraps at +-180°
	VMAT_roty (rtile, lng);

	if (nlat > 8) {
		// The reference point for these tiles has been shifted from the centre of the sphere
		// to the lower left corner of the tile, to reduce offset distances which cause rounding
		// errors in the single-precision world matrix. The offset calculations are done in
		// double-precision before copying them into the world matrix.
		double lat = Pi05 * (double)ilat/(double)nlat;
		double s = cbody->Size();
		double dx = s*cos(lng)*cos(lat); // the offsets between sphere centre and tile corner
		double dy = s*sin(lat);
		double dz = s*sin(lng)*cos(lat);
		RenderParam.wmat_tmp._41 = (float)(dx*RenderParam.grot.m11 + dy*RenderParam.grot.m12 + dz*RenderParam.grot.m13 + RenderParam.cpos.x);
		RenderParam.wmat_tmp._42 = (float)(dx*RenderParam.grot.m21 + dy*RenderParam.grot.m22 + dz*RenderParam.grot.m23 + RenderParam.cpos.y);
		RenderParam.wmat_tmp._43 = (float)(dx*RenderParam.grot.m31 + dy*RenderParam.grot.m32 + dz*RenderParam.grot.m33 + RenderParam.cpos.z);
		D3DMath_MatrixMultiply (wtrans, RenderParam.wmat_tmp, rtile);
	} else {
		D3DMath_MatrixMultiply (wtrans, RenderParam.wmat, rtile);
	}
	RenderParam.dev->SetTransform (D3DTRANSFORMSTATE_WORLD, &wtrans);
}

// =======================================================================

bool TileManager::SpecularColour (D3DCOLORVALUE *col)
{
	const ATMCONST *ap = cbody->AtmParams();
	if (ap) {
		double fac = 0.7; // adjust!
		double cosa = dotp (RenderParam.cdir, RenderParam.sdir);
		double alpha = 0.5*acos(cosa); // sun reflection angle
			
		double scale = sin(alpha)*fac;
		col->r = (float)max(0.0, spec_base - scale*ap->color0.x);
		col->g = (float)max(0.0, spec_base - scale*ap->color0.y);
		col->b = (float)max(0.0, spec_base - scale*ap->color0.z);
		return true;
	}
	return false;
}

// =======================================================================
// =======================================================================
// Class TileBuffer

TileBuffer::TileBuffer()
{
	DWORD id;

	nbuf = 0;
	nused = 0;
	last = 0;

	bLoadMip = g_pOrbiter->GetInlineGraphicsClient()->GetFramework()->SupportsMipmaps() &&
	   		   g_pOrbiter->Cfg()->CfgPRenderPrm.MipmapMode != 0;
	bRunThread = true;
	nqueue = queue_in = queue_out = 0;
	hQueueMutex = CreateMutex (0, FALSE, NULL);
	hLoadThread = CreateThread (NULL, 2048, LoadTile_ThreadProc, this, 0, &id);
}

// =======================================================================

TileBuffer::~TileBuffer()
{
	bRunThread = false;
	CloseHandle (hLoadThread);
	CloseHandle (hQueueMutex);

	if (nbuf) {
		for (DWORD i = 0; i < nbuf; i++)
			if (buf[i]) {
				if (!(buf[i]->flag & 0x80)) {
					if (buf[i]->tex)  buf[i]->tex->Release();
					if (buf[i]->ltex) buf[i]->ltex->Release();
				}
				if (buf[i]->vtx) {
					buf[i]->vtx->Release();
				}
				delete buf[i];
			}
		delete []buf;
	}
}

// =======================================================================

void TileBuffer::StopLoadThread ()
{
	TerminateThread (hLoadThread, 0);
}

// =======================================================================

TILEDESC *TileBuffer::AddTile ()
{
	// NOTE: Had a malloc failure here on 'new TILEDESC', when there were 74000+ tiles already allocated
	// Running out of stack space?

	TILEDESC *td = new TILEDESC; //TRACENEW
	memset (td, 0, sizeof(TILEDESC));
	DWORD i, j;

	if (nused == nbuf) {
		TILEDESC **tmp = new TILEDESC*[nbuf+16]; TRACENEW
		if (nbuf) {
			memcpy (tmp, buf, nbuf*sizeof(TILEDESC*));
			delete []buf;
		}
		memset (tmp+nbuf, 0, 16*sizeof(TILEDESC*));
		buf = tmp;
		nbuf += 16;
		last = nused;
	} else {
		for (i = 0; i < nbuf; i++) {
			j = (i+last)%nbuf;
			if (!buf[j]) {
				last = j;
				break;
			}
		}
		if (i == nbuf)
			strcpy (DBG_MSG, "Problems!");
	}
	buf[last] = td;
	td->ofs = last;
	nused++;
	return td;
}

// =======================================================================

void TileBuffer::DeleteSubTiles (TILEDESC *tile)
{
	for (DWORD i = 0; i < 4; i++)
		if (tile->subtile[i]) {
			if (DeleteTile (tile->subtile[i]))
				tile->subtile[i] = 0;
		}
}

// =======================================================================

bool TileBuffer::DeleteTile (TILEDESC *tile)
{
	bool del = true;
	for (DWORD i = 0; i < 4; i++)
		if (tile->subtile[i]) {
			if (DeleteTile (tile->subtile[i]))
				tile->subtile[i] = 0;
			else
				del = false;
		}
	if (tile->vtx) {
		tile->vtx->Release();
		tile->vtx = 0;
	}
	if (tile->tex || !del) {
		return false; // tile or subtile contains texture -> don't deallocate
		// NOTE: Not ever deleting subtiles with attached textures may not be
		// sustainable. Need a better mechanism.
	} else {
		buf[tile->ofs] = 0; // remove from list
		delete tile;
		nused--;
		return true;
	}
}

// =======================================================================

void ClearVertexBuffers (TILEDESC *td)
{
	for (int j = 0; j < 4; j++) {
		TILEDESC *sub = td->subtile[j];
		if (sub) {
			if (!sub->tex || sub->flag & 0x80) {
				// child has no own texture, i.e. uses part of parent texture
				if (sub->vtx) {
					sub->vtx->Release();
					sub->vtx = 0;
				}
				ClearVertexBuffers (sub); // recursion up the tree
			}
		}
	}
}

// =======================================================================

bool TileBuffer::LoadTileAsync (const char *name, TILEDESC *tile)
{
	bool ok = true;

	if (nqueue == MAXQUEUE)
		ok = false; // queue full
	else {
		for (int i = 0; i < nqueue; i++) {
			int j = (i+queue_out) % MAXQUEUE;
			if (loadqueue[j].td == tile)
			{ ok = false; break; }// request already present
		}
	}

	if (ok) {
		QUEUEDESC *qd = loadqueue+queue_in;
		qd->name = name;
		qd->td = tile;

		nqueue++;
		queue_in = (queue_in+1) % MAXQUEUE;
	}
	return ok;
}

// =======================================================================

DWORD WINAPI TileBuffer::LoadTile_ThreadProc (void *data)
{
	TileBuffer *tb = (TileBuffer*)data;
	bool load;
	static QUEUEDESC qd;
	static int nloaded = 0; // temporary
	DWORD flag = (tb->bLoadMip ? 0:4);
	DWORD idle = 1000/g_pOrbiter->Cfg()->CfgPRenderPrm.LoadFrequency;

	while (bRunThread) {
		Sleep (idle);
		WaitForSingleObject (hQueueMutex, INFINITE);
		if (load = (nqueue > 0)) {
			memcpy (&qd, loadqueue+queue_out, sizeof(QUEUEDESC));
		}
		ReleaseMutex (hQueueMutex);

		if (load) {
			TILEDESC *td = qd.td;
			LPDIRECTDRAWSURFACE7 tex, mask = 0;
			DWORD tidx, midx;
			long ofs;

			if ((td->flag & 0x80) == 0)
				MessageBeep (-1);

			tidx = (DWORD)td->tex;
			if (tidx == (DWORD)-1)
				tex = NULL; // "no texture" flag
			else {
				ofs = (td->flag & 0x40 ? (long)tidx * TILESIZE : (long)tidx);
				//ofs = (long)tidx * TILESIZE; // file offset of tile
				if (!g_texmanager2->OpenTexture (qd.name, "_tile.tex", ofs, &tex, flag))
					tex = NULL;
			}
			// Load the specular mask and/or light texture
			if (((td->flag & 3) == 3) || (td->flag & 4)) {
				midx = (DWORD)td->ltex;
				if (midx == (DWORD)-1)
					mask = NULL; // "no mask" flag
				else {
					ofs = (td->flag & 0x40 ? (long)midx * TILESIZE : (long)midx);
					//ofs = (long)midx * TILESIZE;
					if (!g_texmanager2->OpenTexture (qd.name, "_tile_lmask.tex", ofs, &mask))
						mask = NULL;
				}
			}
			// apply loaded components
			WaitForSingleObject (hQueueMutex, INFINITE);
			td->tex  = tex;
			td->ltex = mask;
			if (tex) {
				if (td->vtx) {
					td->vtx->Release();
					td->vtx = 0;
				}
				ClearVertexBuffers (td); // invalidate child vertex buffers
			}
			td->flag &= 0x3F; // mark as loaded
			nqueue--;
			queue_out = (queue_out+1) % MAXQUEUE;
			ReleaseMutex (hQueueMutex);

			//sprintf (DBG_MSG, "tiles loaded: %d", ++nloaded);
		}
	}
	return 0;
}

// =======================================================================

bool TileBuffer::bRunThread = true;
int TileBuffer::nqueue = 0;
int TileBuffer::queue_in = 0;
int TileBuffer::queue_out = 0;
HANDLE TileBuffer::hQueueMutex = 0;
struct TileBuffer::QUEUEDESC TileBuffer::loadqueue[MAXQUEUE] = {0};

// =======================================================================
// =======================================================================
// Nonmember functions

void ApplyPatchTextureCoordinates (VBMESH &mesh, LPDIRECT3DVERTEXBUFFER7 vtx, const TEXCRDRANGE &range)
{
	VERTEX_2TEX *tgtdata;
	vtx->Lock (DDLOCK_WAIT | DDLOCK_DISCARDCONTENTS, (LPVOID*)&tgtdata, NULL);
	if (mesh.vtx) { // direct access to vertex data
		memcpy (tgtdata, mesh.vtx, mesh.nv*sizeof(VERTEX_2TEX));
	} else {        // need to lock the buffer
		VERTEX_2TEX *srcdata;
		mesh.vb->Lock (DDLOCK_WAIT | DDLOCK_READONLY, (LPVOID*)&srcdata, NULL);
		memcpy (tgtdata, srcdata, mesh.nv*sizeof(VERTEX_2TEX));
		mesh.vb->Unlock();
	}
	float tuscale = range.tumax-range.tumin, tuofs = range.tumin;
	float tvscale = range.tvmax-range.tvmin, tvofs = range.tvmin;
	for (DWORD i = 0; i < mesh.nv; i++) {
		tgtdata[i].tu0 = tgtdata[i].tu0*tuscale + tuofs;
		tgtdata[i].tv0 = tgtdata[i].tv0*tvscale + tvofs;
	}
	vtx->Unlock();
}

#ifdef UNDEF
// This version only updates the texture coordinates, from scratch, without reference to a
// reference mesh. This doesn't work now, but may be a good alternative if strided vertex
// formats are used for the patches.
void ApplyPatchTextureCoordinates (VBMESH &mesh, LPDIRECT3DVERTEXBUFFER7 vtx, const TEXCRDRANGE &range)
{
	int i, j, n, nseg;
	int res = mesh.res;
	int bseg = mesh.bseg;
	bool reduce = mesh.reduce;

	D3DVALUE src_tu0, src_tv0, tgt_tv0;
	D3DVALUE tuscale = range.tumax-range.tumin, tuofs = range.tumin;
	D3DVALUE tvscale = range.tvmax-range.tvmin, tvofs = range.tvmin;

	VERTEX_2TEX *tgt;
	vtx->Lock (DDLOCK_WAIT | DDLOCK_DISCARDCONTENTS, (LPVOID*)&tgt, NULL);

	for (i = n = 0; i <= res; i++) {
		nseg = (reduce ? bseg-i : bseg);
		src_tv0 = D3DVAL((res-i)/res);
		tgt_tv0 = src_tv0 * tvscale + tvofs;
		for (j = 0; j <= nseg; j++) {
			src_tu0 = D3DVAL(nseg ? (D3DVALUE)j/(D3DVALUE)nseg : 0.5f); // overlap to avoid seams
			tgt[n].tu0 = src_tu0 * tuscale + tuofs;
			tgt[n].tv0 = tgt_tv0;
			n++;
		}
	}

	vtx->Unlock();
}
#endif
