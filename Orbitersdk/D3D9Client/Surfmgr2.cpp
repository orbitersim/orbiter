// ==============================================================
//   ORBITER VISUALISATION PROJECT (OVP)
//   Copyright (C) 2006-2016 Martin Schweiger
//   Dual licensed under GPL v3 and LGPL v3
// ==============================================================

// ==============================================================
// surfmgr2.cpp
// class SurfaceManager2 (implementation)
//
// Planetary surface rendering engine v2, including a simple
// LOD (level-of-detail) algorithm for surface patch resolution.
// ==============================================================

#include "Surfmgr2.h"
#include "Tilemgr2.h"
#include "Texture.h"
#include "D3D9Catalog.h"
#include "D3D9Config.h"
#include "vVessel.h"
#include "VectorHelpers.h"

// =======================================================================

#pragma pack(push,1)

struct ELEVFILEHEADER { // file header for patch elevation data file
	char id[4];            // ID string + version ('E','L','E',1)
	int hdrsize;           // header size (76 expected)
	int dtype;             // data format (0=flat, no data block; 8=uint8; -8=int8; 16=uint16; -16=int16)
	int xgrd,ygrd;         // data grid size (259 x 259 expected)
	int xpad,ypad;         // horizontal, vertical padding width (1, 1 expected)
	double scale;          // data scaling factor (1.0 expected)
	double offset;         // data offset (elevation = raw value * scale + offset)
	double latmin, latmax; // latitude range [rad]
	double lngmin, lngmax; // longitude range [rad]
	double emin, emax, emean; // min, max, mean elevation [m]
};

#pragma pack(pop)

// =======================================================================
// Utility functions

static void VtxInterpolate (VERTEX_2TEX &res, const VERTEX_2TEX &a, const VERTEX_2TEX &b, double w)
{
	float w1 = (float)w;
	float w0 = 1.0f-w1;
	res.x = a.x*w0 + b.x*w1;
	res.y = a.y*w0 + b.y*w1;
	res.z = a.z*w0 + b.z*w1;
	res.nx = a.nx*w0 + b.nx*w1;
	res.ny = a.ny*w0 + b.ny*w1;
	res.nz = a.nz*w0 + b.nz*w1;
}

// =======================================================================
// =======================================================================

SurfTile::SurfTile (TileManager2Base *_mgr, int _lvl, int _ilat, int _ilng)
: Tile (_mgr, _lvl, _ilat, _ilng)
{
	smgr = static_cast<TileManager2<SurfTile>* > (_mgr);
	node = 0;
	elev = NULL;
	ggelev = NULL;
	ltex = NULL;
	has_elevfile = false;
	MaxRep = mgr->Client()->GetFramework()->GetCaps()->MaxTextureRepeat;
}

// -----------------------------------------------------------------------

SurfTile::~SurfTile ()
{
	if (elev) {
		delete []elev;
		elev = NULL;
	}
	if (ltex && owntex) {
		if (TileCatalog->Remove(DWORD(ltex))) ltex->Release();
	}
}

// -----------------------------------------------------------------------

void SurfTile::PreLoad()
{
	char name[MAX_PATH];
	sprintf_s (name, MAX_PATH, "Textures\\%s\\Surf\\%02d\\%06d\\%06d.dds", mgr->CbodyName(), lvl+4, ilat, ilng);
	bool found = LoadTextureFile(name, &pPreSrf);
	if (found) {
		sprintf_s (name, MAX_PATH, "Textures\\%s\\Mask\\%02d\\%06d\\%06d.dds", mgr->CbodyName(), lvl+4, ilat, ilng);
		LoadTextureFile(name, &pPreMsk);
	}
	mgr->TileLabel(pPreSrf, lvl, ilat, ilng);
	mgr->TileLabel(pPreMsk, lvl, ilat, ilng);
}


// -----------------------------------------------------------------------

void SurfTile::Load ()
{

	LPDIRECT3DDEVICE9 pDev = mgr->Dev();

	if (CreateTexture(pDev, pPreSrf, &tex) != true) {
		if (GetParentSubTexRange (&texrange)) {
			tex = getSurfParent()->Tex();
			owntex = false;
		} else
			tex = 0;
	} else TileCatalog->Add(DWORD(tex));

	// Load mask texture
	if (mgr->Cprm().bSpecular || mgr->Cprm().bLights) {
		if (owntex && tex) {
			CreateTexture(pDev, pPreMsk, &ltex);
			if (ltex) TileCatalog->Add(DWORD(ltex));
		} else if (node->Parent()) {
			ltex = getSurfParent()->ltex;
		}
	}

	// Load elevation data
	INT16 *elev = ElevationData ();
	bool shift_origin = (lvl >= 4);
	int res = mgr->Cprm().gridRes;

	if (!lvl) {
		// create hemisphere mesh for western or eastern hemispheres
		mesh = CreateMesh_hemisphere (res, elev, 0.0);
	//} else if (ilat == 0 || ilat == (1<<lvl)-1) {
		// create triangular patch for north/south pole region
	//	mesh = CreateMesh_tripatch (patch_res, elev, shift_origin, &vtxshift);
	} else {
		// create rectangular patch
		mesh = CreateMesh_quadpatch (res, res, elev, 0.0, &texrange, shift_origin, &vtxshift, mgr->GetPlanet()->prm.tilebb_excess);
	}

}

// -----------------------------------------------------------------------

INT16 *SurfTile::ReadElevationFile (const char *name, int lvl, int ilat, int ilng, double *_mean_elev)
{
	INT16 *e = NULL;
	FILE *f = NULL;
	char path[256];
	sprintf_s (path, 256, "Textures\\%s\\Elev\\%02d\\%06d\\%06d.elv", name, lvl, ilat, ilng);
	if (!fopen_s(&f, path, "rb")) {
		//LogWrn("Loading Elevation File [%s]", path);
		int i;
		const int ndat = TILE_ELEVSTRIDE*TILE_ELEVSTRIDE;
		e = new INT16[ndat];
		// read the elevation file header
		ELEVFILEHEADER hdr;
		fread (&hdr, sizeof(ELEVFILEHEADER), 1, f);
		if (hdr.hdrsize != sizeof(ELEVFILEHEADER)) {
			fseek (f, hdr.hdrsize, SEEK_SET);
		}
		if (_mean_elev) *_mean_elev = hdr.emean;

		mgr->ReduceMinElevation(hdr.emin);	
		
		switch (hdr.dtype) {
		case 0: // flat tile, defined by offset
			for (i = 0; i < ndat; i++) e[i] = (INT16)hdr.offset;
			break;
		case 8: {
			UINT8 *tmp = new UINT8[ndat];
			fread (tmp, sizeof(UINT8), ndat, f);
			for (i = 0; i < ndat; i++) e[i] = (INT16)(tmp[i]+hdr.offset);
			delete []tmp;
			}
			break;
		case -16:
			fread (e, sizeof(INT16), ndat, f);
			if (hdr.offset)
				for (i = 0; i < ndat; i++) e[i] = (INT16)(e[i]+hdr.offset);
			break;
		}
		fclose (f);

		// now load the overloaded data if present
		f = NULL;
		sprintf_s (path, 256, "Textures\\%s\\Elev_mod\\%02d\\%06d\\%06d.elv", name, lvl, ilat, ilng);
		if (!fopen_s(&f, path, "rb")) {
			//LogWrn("Loading Elevation File [%s]", path);
			fread (&hdr, sizeof(ELEVFILEHEADER), 1, f);
			if (hdr.hdrsize != sizeof(ELEVFILEHEADER)) {
				fseek (f, hdr.hdrsize, SEEK_SET);
			}
			switch (hdr.dtype) {
			case 0: // overwrite the entire tile with a flat offset
				for (i = 0; i < ndat; i++) e[i] = (INT16)hdr.offset;
				break;
			case 8: {
				const UINT8 mask = UCHAR_MAX;
				UINT8 *tmp = new UINT8[ndat];
				fread (tmp, sizeof(UINT8), ndat, f);
				for (i = 0; i < ndat; i++)
					if (tmp[i] != mask)
						e[i] = (INT16)(tmp[i]+hdr.offset);
				delete []tmp;
				}
				break;
			case -16: {
				const INT16 mask = SHRT_MAX;
				INT16 *tmp = new INT16[ndat];
				INT16 ofs = (INT16)hdr.offset;
				fread (tmp, sizeof(INT16), ndat, f);
				for (i = 0; i < ndat; i++)
					if (tmp[i] != mask)
						e[i] = tmp[i]+ofs;
				delete []tmp;
				}
				break;
			}
			fclose(f);
		}
	}
	return e;
}

// -----------------------------------------------------------------------

bool SurfTile::LoadElevationData ()
{
	// Note: a tile's elevation data are retrieved from its great-grandparent tile. Each tile stores the elevation
	// data for its area at 8x higher resolution than required by itself, so they can be used by the grandchildren.
	// This reduces the number of elevation files, by storing more information in each. It also has a caching effect:
	// Once a grandparent has loaded its elevation data on request of a grandchild, any other grandchildren's requests
	// can be served directly without further disk I/O.

	if (elev) return true; // already present

	has_elevfile = false;
	int mode = mgr->Cprm().elevMode;
	if (!mode) return false;

	int ndat = TILE_ELEVSTRIDE*TILE_ELEVSTRIDE;
	elev = ReadElevationFile (mgr->CbodyName(), lvl+4, ilat, ilng);
	if (elev) {

		has_elevfile = true;

	} else if (lvl > 0) {

		// construct elevation grid by interpolating ancestor data
		ELEVHANDLE hElev = mgr->ElevMgr();
		if (!hElev) return false;
		int plvl = lvl-1;
		int pilat = ilat >> 1;
		int pilng = ilng >> 1;
		INT16 *pelev = 0;
		QuadTreeNode<SurfTile> *parent = node->Parent();
		for (; plvl >= 0; plvl--) { // find ancestor with elevation data
			if (parent && parent->Entry()->has_elevfile) {
				pelev = parent->Entry()->elev;
				break;
			}
			parent = parent->Parent();
			pilat >>= 1;
			pilng >>= 1;
		}
		if (!pelev) return false;
		elev = new INT16[ndat];
		// submit ancestor data to elevation manager for interpolation
		mgr->GetClient()->ElevationGrid (hElev, ilat, ilng, lvl, pilat, pilng, plvl, pelev, elev);

	}
	return (elev != 0);
}

// -----------------------------------------------------------------------

INT16 *SurfTile::ElevationData () const
{
	if (!ggelev) {
		int ancestor_dlvl = 3;
		while (1 << (8-ancestor_dlvl) < mgr->Cprm().gridRes) ancestor_dlvl--;
		if (lvl >= ancestor_dlvl) { // traverse quadtree back to great-grandparent
			QuadTreeNode<SurfTile> *ancestor = node; // start at my own node
			int blockRes = TILE_FILERES;
			while (blockRes > mgr->Cprm().gridRes && ancestor) {
				ancestor = ancestor->Parent();
				blockRes >>= 1;
			}
			if (ancestor && ancestor->Entry()->LoadElevationData()) {
				// compute pixel offset into great-grandparent tile set
				int nblock = TILE_FILERES/blockRes;
				int mask = nblock-1;
				int ofs = ((mask - ilat & mask) * TILE_ELEVSTRIDE + (ilng & mask)) * blockRes;
				ggelev = ancestor->Entry()->elev + ofs;
			}
		} else {
			SurfTile *ggp = smgr->GlobalTile(lvl-ancestor_dlvl+3);
			if (ggp->LoadElevationData ()) {
				int blockRes = mgr->Cprm().gridRes;
				int nblock = TILE_FILERES/blockRes;
				int mask = nblock-1;
				int ofs = ((mask - ilat & mask) * TILE_ELEVSTRIDE + (ilng & mask)) * blockRes;
				ggelev = ggp->elev + ofs;
			}
		}
		if (ggelev)
			mean_elev = GetMeanElevation (ggelev);
	}
	return ggelev;
}

// -----------------------------------------------------------------------

double SurfTile::GetMeanElevation (const INT16 *elev) const
{
	int i, j;
	int res = mgr->Cprm().gridRes;
	double melev = 0.0;
	for (j = 0; j <= res; j++) {
		for (i = 0; i <= res; i++) {
			melev += elev[i];
		}
		elev += TILE_ELEVSTRIDE;
	}
	return melev / ((res+1)*(res+1));	
}

// -----------------------------------------------------------------------

SurfTile *SurfTile::getTextureOwner()
{
	if (owntex) return this;
	SurfTile *parent = getSurfParent();
	while (parent) {
		if (parent->owntex) return parent;
		else parent = parent->getSurfParent();
	}
	return NULL;
}

// -----------------------------------------------------------------------

float SurfTile::fixinput(float a, int x)
{
	switch(x) {
		case 0: return (1.0f+floor(a*0.0625f))*16.0f;
		case 1: return (1.0f+floor(a*0.125f))*8.0f;
		case 2: return (1.0f+floor(a*0.5f))*2.0f;
	}
	return 0.0f;
}

// -----------------------------------------------------------------------

D3DXVECTOR4 SurfTile::MicroTexRange(SurfTile *pT, int ml) const
{
	float rs = 1.0f / float( 1 << (lvl-pT->Level()) );	// Range subdivision
	float xo = pT->MicroRep[ml].x * texrange.tumin;		
	float yo = pT->MicroRep[ml].y * texrange.tvmin;
	xo -= floor(xo); // Micro texture offset for current tile
	yo -= floor(yo); // Micro texture offset for current tile
	return D3DXVECTOR4(xo, yo, pT->MicroRep[ml].x * rs, pT->MicroRep[ml].y * rs); 
}

// -----------------------------------------------------------------------
// Called during rendering even if this tile is not rendered but children are
//
void SurfTile::StepIn ()
{
	LPDIRECT3DDEVICE9 pDev = mgr->Dev();
	ID3DXEffect *Shader = mgr->Shader();
	const vPlanet *vPlanet = mgr->GetPlanet();

	if (vPlanet != mgr->GetScene()->GetCameraProxyVisual()) return;

	// Compute micro texture repeat counts -------------------------------------
	//
	if (owntex && vPlanet->MicroCfg.bEnabled) {
		float s  = float(vPlanet->GetSize());
		float a  = s / vPlanet->MicroCfg.Level[0].size;
		float b  = s / vPlanet->MicroCfg.Level[1].size;
		float c  = s / vPlanet->MicroCfg.Level[2].size;
		MicroRep[0] = D3DXVECTOR2(fixinput(width*a,0), fixinput(height*a,0));
		MicroRep[1] = D3DXVECTOR2(fixinput(width*b,1), fixinput(height*b,1));
		MicroRep[2] = D3DXVECTOR2(fixinput(width*c,2), fixinput(height*c,2));
	}
}


// -----------------------------------------------------------------------

void SurfTile::Render ()
{
	bool render_lights = mgr->Cprm().bLights;
	bool render_shadows = (mgr->GetPlanet()->CloudMgr2() && !mgr->prm.rprm->bCloudFlatShadows);

	if (!mesh) return; // DEBUG : TEMPORARY

	UINT numPasses = 0;

	LPDIRECT3DDEVICE9 pDev = mgr->Dev();
	ID3DXEffect *Shader = mgr->Shader();
	const vPlanet *vPlanet = mgr->GetPlanet();

	static const double rad0 = sqrt(2.0)*PI05;
	double sdist, rad;
	bool has_specular = false;
	bool has_shadows = false;
	bool has_lights = false;
	if (ltex || render_shadows) {
		sdist = acos (dotp (mgr->prm.sdir, cnt));
		rad = rad0/(double)(2<<lvl); // tile radius
		has_specular = (ltex && sdist < PI05+rad);
		has_shadows = (render_shadows && sdist < PI05+rad);
		has_lights = (render_lights && ltex && sdist > 1.45);
	}

	// Assign micro texture range information to shaders
	//
	SurfTile *pT = getTextureOwner();

	if (pT) {
		HR(Shader->SetBool(TileManager2Base::sbMicro, true));
		HR(Shader->SetVector(TileManager2Base::svMicroScale0, &MicroTexRange(pT, 0)));
		HR(Shader->SetVector(TileManager2Base::svMicroScale1, &MicroTexRange(pT, 1)));
		HR(Shader->SetVector(TileManager2Base::svMicroScale2, &MicroTexRange(pT, 2)));
	}
	

	// ---------------------------------------------------------------------
	// Feed tile specific data to shaders
	//
	// ---------------------------------------------------------------------------------------------------
	HR(Shader->SetVector(TileManager2Base::svTexOff,  &D3DXVECTOR4(0, 0, 0, 0)));
	HR(Shader->SetVector(TileManager2Base::svGeneric, &D3DXVECTOR4(1, 1, 1, 1)));
	// ---------------------------------------------------------------------------------------------------
	HR(Shader->SetTexture(TileManager2Base::stDiff, tex));
	HR(Shader->SetTexture(TileManager2Base::stMask, ltex));
	// ---------------------------------------------------------------------------------------------------
	HR(Shader->SetInt(TileManager2Base::siTileLvl, lvl));
	// ---------------------------------------------------------------------------------------------------
	HR(Shader->SetBool(TileManager2Base::sbSpecular, has_specular));
	HR(Shader->SetBool(TileManager2Base::sbCloudSh, has_shadows));
	HR(Shader->SetBool(TileManager2Base::sbLights, has_lights));
	// ---------------------------------------------------------------------------------------------------
	if (has_lights) { HR(Shader->SetFloat(TileManager2Base::sfNight, float(mgr->Cprm().lightfac))); }
	else {			  HR(Shader->SetFloat(TileManager2Base::sfNight, 0.0f));	}

	Shader->CommitChanges();

	HR(Shader->Begin(&numPasses, D3DXFX_DONOTSAVESTATE));

	// -------------------------------------------------------------------
	// render surface mesh

	HR(Shader->BeginPass(0));
	pDev->SetVertexDeclaration(pPatchVertexDecl);
	pDev->SetStreamSource(0, mesh->pVB, 0, sizeof(VERTEX_2TEX));
	pDev->SetIndices(mesh->pIB);
	pDev->DrawIndexedPrimitive(D3DPT_TRIANGLELIST, 0, 0, mesh->nv, 0, mesh->nf);
	HR(Shader->EndPass());

	// add cloud shadows
	if (has_shadows) {
		const TileManager2<CloudTile> *cmgr = mgr->GetPlanet()->CloudMgr2();
		int nlng = 2 << lvl;
		int nlat = 1 << lvl;
		double minlat = PI * (double)(nlat/2-ilat-1)/(double)nlat;
		double maxlat = PI * (double)(nlat/2-ilat)/(double)nlat;
		double minlng = PI2 * (double)(ilng-nlng/2)/(double)nlng;
		double maxlng = PI2 * (double)(ilng-nlng/2+1)/(double)nlng;

		const Tile *tbuf[2];
		int maxlvl = min(lvl,9);
		int ncloud = cmgr->Coverage(minlat, maxlat, minlng, maxlng, maxlvl, tbuf, 2);
		if (ncloud > 0) {
			float alpha = (float)mgr->prm.rprm->shadowalpha;
			if (alpha >= 0.01f) { // otherwise don't render cloud shadows for this planet

				if (ncloud == 1) { // other cases still to be done
					for (int i = 0; i < ncloud; i++) {
						// to be completed: create new mesh with modified texture coordinates
						const TEXCRDRANGE2 *ctexrange = tbuf[i]->GetTexRange();
						double cminlat, cmaxlat, cminlng, cmaxlng;
						float tu0, tu1, tv0, tv1, tuscale, tvscale;
						tbuf[i]->Extents (&cminlat, &cmaxlat, &cminlng, &cmaxlng);

						cminlng -= mgr->prm.rprm->cloudrot;
						cmaxlng -= mgr->prm.rprm->cloudrot;
						if (cmaxlng < -PI) {
							cminlng += PI2;
							cmaxlng += PI2;
						}
						tu0 = ctexrange->tumin + (ctexrange->tumax-ctexrange->tumin)*(float)((minlng-cminlng)/(cmaxlng-cminlng));
						tu1 = ctexrange->tumin + (ctexrange->tumax-ctexrange->tumin)*(float)((maxlng-cminlng)/(cmaxlng-cminlng));
						tv0 = ctexrange->tvmin + (ctexrange->tvmax-ctexrange->tvmin)*(float)((maxlat-cmaxlat)/(cminlat-cmaxlat));
						tv1	= ctexrange->tvmin + (ctexrange->tvmax-ctexrange->tvmin)*(float)((minlat-cmaxlat)/(cminlat-cmaxlat));
						tuscale = (tu1-tu0)/(texrange.tumax-texrange.tumin);
						tvscale = (tv1-tv0)/(texrange.tvmax-texrange.tvmin);

						HR(Shader->SetFloat(TileManager2Base::sfAlpha, alpha));
						HR(Shader->SetVector(TileManager2Base::svTexOff,  &D3DXVECTOR4(tu0, tv0, texrange.tumin, texrange.tvmin)));
						HR(Shader->SetVector(TileManager2Base::svGeneric, &D3DXVECTOR4(tuscale, tvscale, 0, 0)));
						HR(Shader->SetTexture(TileManager2Base::stDiff, tbuf[i]->Tex()));
						HR(Shader->CommitChanges());
						HR(Shader->BeginPass(1)); // 1 = Shadow Pass
						pDev->DrawIndexedPrimitive(D3DPT_TRIANGLELIST, 0, 0, mesh->nv, 0, mesh->nf);
						HR(Shader->EndPass());
					}
				}

			}
		}
	}
	HR(Shader->End());	
}

// -----------------------------------------------------------------------

void SurfTile::MatchEdges ()
{
	if (edgeok) return; // done already
	edgeok = true;
	if (!mesh) return;  // sanity check

	QuadTreeNode<SurfTile> *lngnbr = smgr->FindNode (lvl, ilat, ilng + (ilng & 1 ? 1 : -1));
	QuadTreeNode<SurfTile> *latnbr = smgr->FindNode (lvl, ilat + (ilat & 1 ? 1 : -1), ilng);
	QuadTreeNode<SurfTile> *dianbr = smgr->FindNode (lvl, ilat + (ilat & 1 ? 1 : -1), ilng + (ilng & 1 ? 1 : -1));

	if (lngnbr && !(lngnbr->Entry()->state & TILE_VALID)) lngnbr = 0;
	if (latnbr && !(latnbr->Entry()->state & TILE_VALID)) latnbr = 0;
	if (dianbr && !(dianbr->Entry()->state & TILE_VALID)) dianbr = 0;

	int new_lngnbr_lvl = (lngnbr ? lngnbr->Entry()->lvl : lvl);
	int new_latnbr_lvl = (latnbr ? latnbr->Entry()->lvl : lvl);
	int new_dianbr_lvl = (dianbr ? dianbr->Entry()->lvl : lvl);

	// fix lower-level neighbours first
	bool nbr_updated = false;
	if (new_lngnbr_lvl < lvl) {
		lngnbr->Entry()->MatchEdges ();
		nbr_updated = true;
	}
	if (new_latnbr_lvl < lvl) {
		latnbr->Entry()->MatchEdges ();
		nbr_updated = true;
	}
	if (!nbr_updated && new_dianbr_lvl < lvl) {
		// now we need to check the diagonal neighbour, since this could be lower resolution
		// and thus responsible for the corner node elevation
		dianbr->Entry()->MatchEdges ();
	}

	bool lngedge_changed = (new_lngnbr_lvl != lngnbr_lvl);
	bool latedge_changed = (new_latnbr_lvl != latnbr_lvl);
	bool diaedge_changed = (new_dianbr_lvl != dianbr_lvl);

	if (lngedge_changed || latedge_changed || diaedge_changed) {
		if (new_dianbr_lvl < new_lngnbr_lvl && new_dianbr_lvl < new_latnbr_lvl) {
			FixCorner (dianbr ? dianbr->Entry() : 0);
			FixLatitudeBoundary (latnbr ? latnbr->Entry() : 0, true);
			FixLongitudeBoundary (lngnbr ? lngnbr->Entry() : 0, true);
		} else if (new_latnbr_lvl < new_lngnbr_lvl) {
			FixLatitudeBoundary (latnbr ? latnbr->Entry() : 0);
			FixLongitudeBoundary (lngnbr ? lngnbr->Entry() : 0, true);
		} else {
			FixLongitudeBoundary (lngnbr ? lngnbr->Entry() : 0);
			FixLatitudeBoundary (latnbr ? latnbr->Entry() : 0, true);
		}
		mesh->MapVertices(mgr->Dev()); // copy the updated vertices to the vertex buffer
		lngnbr_lvl = new_lngnbr_lvl;
		latnbr_lvl = new_latnbr_lvl;
		dianbr_lvl = new_dianbr_lvl;
	}
}

// -----------------------------------------------------------------------

void SurfTile::FixCorner (const SurfTile *nbr)
{
	if (!mesh) return; // sanity check

	int res = mgr->Cprm().gridRes;
	int vtx_idx = (ilat & 1 ? 0 : (res+1)*res) + (ilng & 1 ? res : 0);
	VERTEX_2TEX &vtx_store = mesh->vtx[mesh->nv + (ilat & 1 ? 0 : res)];

	INT16 *elev = ElevationData();
	if (!elev) return;

	if (nbr) {
		INT16 *nbr_elev = nbr->ElevationData();
		if (!nbr_elev) return;

		INT16 corner_elev = elev[TILE_ELEVSTRIDE+1 + (ilat & 1 ? 0 : TILE_ELEVSTRIDE*res) + (ilng & 1 ? res : 0)];
		INT16 nbr_corner_elev = nbr_elev[TILE_ELEVSTRIDE+1 + (ilat & 1 ? TILE_ELEVSTRIDE*res : 0) + (ilng & 1 ? 0 : res)];
	
		double rad = mgr->CbodySize();
		double radfac = (rad+nbr_corner_elev)/(rad+corner_elev);
		mesh->vtx[vtx_idx].x = (float)(vtx_store.x*radfac + vtxshift.x*(radfac-1.0));
		mesh->vtx[vtx_idx].y = (float)(vtx_store.y*radfac + vtxshift.y*(radfac-1.0));
		mesh->vtx[vtx_idx].z = (float)(vtx_store.z*radfac + vtxshift.z*(radfac-1.0));
	}
}

// -----------------------------------------------------------------------

void SurfTile::FixLongitudeBoundary (const SurfTile *nbr, bool keep_corner)
{
	// Fix the left or right edge
	int i, i0, i1, j, vtx_ofs, nbrlvl, dlvl;
	int res = mgr->Cprm().gridRes;
	VERTEX_2TEX *vtx_store;

	vtx_ofs = (ilng & 1 ? res : 0); // check if neighbour is at left or right edge
	if (nbr && nbr->mesh && nbr->mesh->vtx) {
		nbrlvl = min(nbr->lvl, lvl); // we don't need to worry about neigbour levels higher than ours
		vtx_store = mesh->vtx + mesh->nv;
		if (nbrlvl == lvl) { // put my own edge back
			i0 = 0;
			i1 = res;
			if (keep_corner)
				if (ilat & 1) i0++; else i1--;
			for (i = i0; i <= i1; i++)
				mesh->vtx[vtx_ofs+i*(res+1)] = vtx_store[i];
		} else {  // interpolate to neighbour's left edge
			dlvl = lvl-nbrlvl;
			if (dlvl <= 5) { // for larger tile level differences the interleaved sampling method doesn't work
				INT16 *elev = ElevationData();
				INT16 *nbr_elev = nbr->ElevationData();
				if (elev && nbr_elev) {
					elev += TILE_ELEVSTRIDE+1 + vtx_ofs; // add offset
					int nsub = 1 << dlvl;    // number of tiles fitting alongside the lowres neighbour
					int vtx_skip = nsub*(res+1);  // interleave factor for nodes attaching to neigbour nodes
					int nbr_range = res/nsub; // number of neighbour vertices attaching to us - 1
					int subidxmask = (1<<dlvl)-1;
					int subidx = ilat & subidxmask;
					int nbr_ofs = (subidxmask-subidx) * nbr_range * TILE_ELEVSTRIDE;
					nbr_elev += TILE_ELEVSTRIDE+1 + nbr_ofs + (res-vtx_ofs);
					double rad = mgr->CbodySize();
					// match nodes to neighbour elevations
					i0 = 0;
					i1 = nbr_range;
					if (keep_corner)
						if (ilat & 1) i0++; else i1--;
					for (i = i0; i <= i1; i++) {
						double radfac = (rad+nbr_elev[i*TILE_ELEVSTRIDE])/(rad+elev[i*TILE_ELEVSTRIDE*nsub]);
						mesh->vtx[vtx_ofs+i*vtx_skip].x = (float)(vtx_store[i*nsub].x*radfac + vtxshift.x*(radfac-1.0));
						mesh->vtx[vtx_ofs+i*vtx_skip].y = (float)(vtx_store[i*nsub].y*radfac + vtxshift.y*(radfac-1.0));
						mesh->vtx[vtx_ofs+i*vtx_skip].z = (float)(vtx_store[i*nsub].z*radfac + vtxshift.z*(radfac-1.0));
					}
					// interpolate the nodes that fall between neighbour nodes
					for (i = 0; i < nbr_range; i++)
						for (j = 1; j < nsub; j++)
							VtxInterpolate (mesh->vtx[vtx_ofs+j*(res+1)+i*vtx_skip], mesh->vtx[vtx_ofs+i*vtx_skip], mesh->vtx[vtx_ofs+(i+1)*vtx_skip], (double)j/double(nsub));
				}
			} else {
				// problems
			}
		}
	}
}

// -----------------------------------------------------------------------

void SurfTile::FixLatitudeBoundary (const SurfTile *nbr, bool keep_corner)
{
	// Fix the top or bottom edge
	int i, i0, i1, j, nbrlvl, dlvl;
	int res = mgr->Cprm().gridRes;
	VERTEX_2TEX *vtx_store;

	int line = (ilat & 1 ? 0 : res);
	int vtx_ofs = (ilat & 1 ? 0 : line*(res+1));
	if (nbr && nbr->mesh && nbr->mesh->vtx) {
		nbrlvl = min(nbr->lvl, lvl); // we don't need to worry about neigbour levels higher than ours
		vtx_store = mesh->vtx + mesh->nv + res + 1;
		if (nbrlvl == lvl) { // put my own edge back
			i0 = 0;
			i1 = res;
			if (keep_corner)
				if (ilng & 1) i1--; else i0++;
			for (i = i0; i <= i1; i++)
				mesh->vtx[vtx_ofs+i] = vtx_store[i];
		} else {
			dlvl = lvl-nbrlvl;
			if (dlvl <= 5) { // for larger tile level differences the interleaved sampling method doesn't work
				INT16 *elev = ElevationData();
				INT16 *nbr_elev = nbr->ElevationData();
				if (elev && nbr_elev) {
					elev += (line+1)*TILE_ELEVSTRIDE + 1; // add offset
					int nsub = 1 << dlvl;    // number of tiles fitting alongside the lowres neighbour
					int vtx_skip = nsub;     // interleave factor for nodes attaching to neigbour nodes
					int nbr_range = res/nsub; // number of neighbour vertices attaching to us - 1
					int subidxmask = (1<<dlvl)-1;
					int subidx = ilng & subidxmask;
					int nbr_ofs = subidx * nbr_range;
					nbr_elev += TILE_ELEVSTRIDE+1 + nbr_ofs + (res-line)*TILE_ELEVSTRIDE;
					double rad = mgr->CbodySize();
					// match nodes to neighbour elevations
					i0 = 0;
					i1 = nbr_range;
					if (keep_corner)
						if (ilng & 1) i1--; else i0++;
					for (i = i0; i <= i1; i++) {
						double radfac = (rad+nbr_elev[i])/(rad+elev[i*nsub]);
						mesh->vtx[vtx_ofs+i*vtx_skip].x = (float)(vtx_store[i*nsub].x*radfac + vtxshift.x*(radfac-1.0));
						mesh->vtx[vtx_ofs+i*vtx_skip].y = (float)(vtx_store[i*nsub].y*radfac + vtxshift.y*(radfac-1.0));
						mesh->vtx[vtx_ofs+i*vtx_skip].z = (float)(vtx_store[i*nsub].z*radfac + vtxshift.z*(radfac-1.0));
					}
					// interpolate the nodes that fall between neighbour nodes
					for (i = 0; i < nbr_range; i++)
						for (j = 1; j < nsub; j++)
							VtxInterpolate (mesh->vtx[vtx_ofs+i*vtx_skip+j], mesh->vtx[vtx_ofs+i*vtx_skip], mesh->vtx[vtx_ofs+(i+1)*vtx_skip], (double)j/double(nsub));
				}
			} else {
				// problems
			}
		}
	}
}

// =======================================================================
// =======================================================================

template<>
void TileManager2<SurfTile>::Render (MATRIX4 &dwmat, bool use_zbuf, const vPlanet::RenderPrm &rprm)
{
	// set generic parameters
	SetRenderPrm (dwmat, 0, use_zbuf, rprm);

	double np = 0.0, fp = 0.0;
	int i;
	class Scene *scene = GetClient()->GetScene();

	// adjust scaling parameters (can only be done if no z-buffering is in use)
	if (!use_zbuf) {
		double R = obj_size;
		double D = prm.cdist*R;
		double zmax = (D - R*R/D) * 1.5;
		double zmin = max (2.0, min (zmax*1e-4, (D-R) * 0.8));
		double zscale = 1.0;

		np = scene->GetCameraNearPlane();
		fp = scene->GetCameraFarPlane();
		scene->SetCameraFrustumLimits (zmin, zmax);
	}

	// build a transformation matrix for frustum testing
	MATRIX4 Mproj = _MATRIX4(scene->GetProjectionMatrix());
	Mproj.m33 = 1.0; Mproj.m43 = -1.0;  // adjust near plane to 1, far plane to infinity
	MATRIX4 Mview = _MATRIX4(scene->GetViewMatrix());
	prm.dviewproj = mul(Mview,Mproj);

	// ---------------------------------------------------------------------

	static float spec_base = 0.7f; // 0.95f;

	// ---------------------------------------------------------------------
	// Initialize shading technique and feed planet specific data to shaders
	//

	if (use_zbuf) Shader()->SetTechnique(eTileTech);
	else		  Shader()->SetTechnique(eTileTechNoZ);

	HR(Shader()->SetMatrix(smViewProj, scene->GetProjectionViewMatrix()));
	HR(Shader()->SetVector(svWater, &D3DXVECTOR4(spec_base*1.2f, spec_base*1.0f, spec_base*0.8f, 50.0f)));
	HR(Shader()->SetBool(sbEnvEnable, false));

	// -------------------------------------------------------------------
	vVessel *vFocus = scene->GetFocusVisual();
	vPlanet *vProxy = scene->GetCameraProxyVisual();

	// Setup Environment map ---------------------------------------------
	//
	if (vFocus && bEnvMapEnabled) {
		LPDIRECT3DCUBETEXTURE9 pEnv = vFocus->GetEnvMap(0);
		if (pEnv) {
			HR(Shader()->SetTexture(stEnvMap, pEnv));
			HR(Shader()->SetBool(sbEnvEnable, true));
		}
	}

	// Setup micro textures ---------------------------------------------
	//
	if (vp->MicroCfg.bEnabled) {	
		HR(Shader()->SetTexture(stMicroA, vp->MicroCfg.Level[0].pTex));
		HR(Shader()->SetTexture(stMicroB, vp->MicroCfg.Level[1].pTex));
		HR(Shader()->SetTexture(stMicroC, vp->MicroCfg.Level[2].pTex));
		HR(Shader()->SetBool(sbMicro, true));
		HR(Shader()->SetBool(sbMicroNormals, vp->MicroCfg.bNormals));
	}
	else {
		HR(Shader()->SetBool(sbMicro, false));
	}


	// ------------------------------------------------------------------
	// TODO: render full sphere for levels < 4

	loader->WaitForMutex();

	// update the tree
	for (i = 0; i < 2; i++)
		ProcessNode (tiletree+i);

	// render the tree
	for (i = 0; i < 2; i++)
		RenderNode (tiletree+i);

	loader->ReleaseMutex();

	if (np)
		scene->SetCameraFrustumLimits(np,fp);
}