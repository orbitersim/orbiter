// ==============================================================
//   ORBITER VISUALISATION PROJECT (OVP)
//   Copyright (C) 2006-2016 Martin Schweiger
//   Dual licensed under GPL v3 and LGPL v3
// ==============================================================

// =======================================================================
// tilemgr2_imp.hpp
// Rendering of planetary surfaces using texture tiles at
// variable resolutions (new version).
// =======================================================================

#ifndef __TILEMGR2_IMP_HPP
#define __TILEMGR2_IMP_HPP

#include "Tilemgr2.h"
#include "DebugControls.h"

// -----------------------------------------------------------------------

template<class TileType>
QuadTreeNode<TileType> *TileManager2Base::FindNode (QuadTreeNode<TileType> root[2], int lvl, int ilng, int ilat)
{
	int i, sublat, sublng, subidx;

	// wrap at longitude += 180
	int nlng = 2 << lvl;
	if (ilng < 0) ilng += nlng;
	else if (ilng >= nlng) ilng -= nlng;

	// Find the level-0 root
	QuadTreeNode<TileType> *node = root + ((ilng >> lvl) & 1);
	for (i = lvl-1; i >= 0; i--) {
		if (node->Entry()->state == Tile::ForRender) return node;
		if (node->Entry()->state == Tile::Invisible) return 0; // tile invisible
		sublat = (ilat >> i) & 1;
		sublng = (ilng >> i) & 1;
		subidx = sublat*2+sublng;
		if (node->Child(subidx) && (node->Child(subidx)->Entry()->state & TILE_ACTIVE)) {
			node = node->Child(subidx);
		} else {
			break;
		}
	}
	return node;
}

// -----------------------------------------------------------------------

template<class TileType>
QuadTreeNode<TileType> *TileManager2Base::LoadChildNode (QuadTreeNode<TileType> *node, int idx)
{
	TileType *parent = node->Entry();
	int lvl = parent->lvl+1;
	int ilat = parent->ilat*2 + idx/2;
	int ilng = parent->ilng*2 + idx%2;
	TileType *tile = new TileType (this, lvl, ilat, ilng);
	QuadTreeNode<TileType> *child = node->AddChild (idx, tile);
	if (bTileLoadThread)
		loader->LoadTileAsync (tile);
	else {
		tile->PreLoad();
		tile->Load();
		tile->state = Tile::Inactive;
	}
	return child;
}

// -----------------------------------------------------------------------

template<class TileType>
void TileManager2Base::QueryTiles(QuadTreeNode<TileType> *node, std::list<Tile*> &tiles)
{
/*Tile *tile = node->Entry();
	for (int i = 0; i < 4; i++) {
		if (node->Child(i)) {
			if (node->Child(i)->Entry()) QueryTiles(node->Child(i), tiles);
		} else tiles.push_back(tile);
	}
*/
	Tile *tile = node->Entry();
	if (tile->state == Tile::ForRender) tiles.push_back(tile);
	else if (tile->state == Tile::Active) {
		for (int i = 0; i < 4; i++) {
			if (node->Child(i)) {
				if (node->Child(i)->Entry() && (node->Child(i)->Entry()->state & TILE_ACTIVE)) QueryTiles(node->Child(i), tiles);
			}
		}
	}
}

// -----------------------------------------------------------------------

template<class TileType>
void TileManager2Base::DebugDump(QuadTreeNode<TileType>* node)
{
	Tile* tile = node->Entry();
	oapiWriteLogV("Tile[0x%X] OwnTex=%u, Mesh=%u, state=%u, lvl=%u", tile, UINT(tile->owntex), UINT(tile->mesh != 0), UINT(tile->state), tile->lvl);
	for (int i = 0; i < 4; i++) {
		auto c = node->Child(i);
		if (c) DebugDump(c);
	}
}

// -----------------------------------------------------------------------

template<class TileType>
void TileManager2Base::ProcessNode (QuadTreeNode<TileType> *node)
{
	static const double res_scale = 1.1; // resolution scale with distance

	const Scene *scene = GetScene();

	Tile *tile = node->Entry();
	int lvl = tile->lvl;
	int ilng = tile->ilng;
	int ilat = tile->ilat;
	int nlng = 2 << lvl;
	int nlat = 1 << lvl;
	bool bstepdown = true;
	double bias = DebugControls::resbias;			// 2 to 6, default 4
	if (ilat < nlat/6 || ilat >= nlat-nlat/6) {		// lower resolution at the poles
		bias -= 1.0;
		if (ilat < nlat/12 || ilat >= nlat-nlat/12)
			bias -= 1.0;
	}

	bool bNoRelease = false;
	
	// Override TileDeletion for forced elevated rendering of asteroids/comets/small moons
	if (ElevMode == eElevMode::ForcedElevated) bNoRelease = true;
	
	tile->dmWorld = WorldMatrix(ilng, nlng, ilat, nlat);
	tile->mWorld = FMATRIX4(tile->dmWorld);

	if (bFreeze) {
		for (int i = 0; i < 4; i++) {
			auto child = node->Child(i);
			if (child) ProcessNode(child);
		}
		return;
	}

	tile->state = Tile::ForRender;
	tile->edgeok = false;

	// check if patch is visible from camera position
	VECTOR3 &cnt = tile->cnt;                   // tile centre in unit planet frame
	static const double rad0 = sqrt(2.0)*PI05;
	double rad = rad0/(double)nlat;
	double alpha = acos (dotp (prm.cdir, cnt)); // angle between tile centre and camera from planet centre
	double adist = alpha - rad;                 // angle between closest tile corner and camera
	if (adist >= prm.viewap) {
		if (lvl == 0)
			bstepdown = false;                // force render at lowest resolution
		else {
			if (scene->GetRenderPass() == RENDERPASS_MAINSCENE) node->DelChildren();
			tile->state = Tile::Invisible;
			return;                           // no need to continue
		}
	}

	// Check if patch bounding box intersects viewport
	MATRIX4 transform = mul (tile->dmWorld, prm.dviewproj);
	if (!tile->InView (transform)) {
		if (lvl == 0)
			bstepdown = false;
		else {
			// Keep a tile allocated as long as the tile can be seen from a current camera position.
			// We have multiple views and only the active (current) view is checked here.
			tile->state = Tile::Invisible;
			//return;  Cannot return here, must check the tile tatget level and release childs if needed.
		}
	}

	int tgtres = -1;

	// Compute target resolution level based on tile distance
	if (bstepdown) {
		double tdist;
		double erad = 1.0 + tile->GetMaxElev()/obj_size; // radius of unit sphere plus elevation
		if (adist < 0.0) { // if we are above the tile, use altitude for distance measurement
			tdist = prm.cdist - erad;
			if (tdist < 0.0) tdist = 0.0;
		} else { // use distance to closest tile edge
			double h = erad*sin(adist);
			double a = prm.cdist - erad*cos(adist);
			double x = a*a + h*h;
			tdist = (x > 0.0) ? sqrt(x) : 0.0;
		}

		bias -=  2.0 * sqrt(max(0.0,adist) / prm.viewap);
		int maxlvl = prm.maxlvl;

		double maxtiles = 1200.0;
		if (Config->MaxTiles == 0) maxtiles = 600.0;
		if (Config->MaxTiles == 2) maxtiles = 2400.0;

		// Dynamic tile count limiter, start reducing above 900 tiles
		double tc = double(TilesLoaded - (maxtiles*0.75)) / 500;
		double fc = tc < 0 ? 1.0 : 1.0 + tc * tc;

		// This doesn't work with narrow FOV, added max() to set low limit
		double apr = tdist * fc * max(0.12, scene->GetTanAp()) * resolutionScale;
		tgtres = (apr < 1e-6 ? maxlvl : max(0, min(maxlvl, (int)(bias - log(apr)*res_scale))));
		bstepdown = (lvl < tgtres);
		tile->tgtscale = pow(2.0f, float(tgtres - lvl));
	}

	if (scene->GetRenderPass() == RENDERPASS_MAINSCENE)
	{
		if (!bstepdown) {
			// Count the tile elevation stats
			if (tile->IsElevated()) elvstat.Elev++;
			else elvstat.Sphe++;
		}

		if (!bstepdown) {	
			// Search elevated tiles from sub-trees
			// This can severally impact in performance if used incorrectly
			if ((ElevMode == eElevMode::ForcedElevated) && (tile->IsElevated() == false)) {
				if (ElevModeLvl == 0) ElevModeLvl = lvl + 1;
				bstepdown = ElevModeLvl >= lvl;
			}
		}
	}
	
	// Recursion to next level: subdivide into 2x2 patch
	if (bstepdown)
	{
		bool subcomplete = true;
		int i, idx;
		// check if all 4 subtiles are available already, and queue any missing for loading
		for (idx = 0; idx < 4; idx++) {
			QuadTreeNode<TileType>* child = node->Child(idx);
			if (!child)
				child = LoadChildNode(node, idx);
			else if (child->Entry()->state == Tile::Invalid)
				loader->LoadTileAsync(child->Entry());
			Tile::TileState state = child->Entry()->state;
			if (!(state & TILE_VALID))
				subcomplete = false;
		}
		if (subcomplete) {
			tile->state = Tile::Active;
			for (i = 0; i < 4; i++)
				ProcessNode(node->Child(i));
			return; // otherwise render at current resolution until all subtiles are available
		}
	}
	else {
		bool bDelete = true;
		for (int idx = 0; idx < 4; idx++) {
			QuadTreeNode<TileType>* child = node->Child(idx);
			// Check if any of the child tiles is rendered resently
			double used = child ? child->Entry()->last_used : 0.0;
			if ((used < 1.0) || ((used + 1.0) > oapiGetSysTime())) {
				// This one been used, or just created, keep the tiles in memory
				bDelete = false;
			}
		}
		// If not then delete all children
		if (bDelete) node->DelChildren();
	}
}

// -----------------------------------------------------------------------

template<class TileType>
void TileManager2Base::RenderNode (QuadTreeNode<TileType> *node)
{
	TileType *tile = node->Entry();
	const Scene *scene = GetScene();
	int lvl = tile->lvl;

	if (bFreeze && bFreezeRenderAll && lvl >= 4) {
		if (tile->state == Tile::Invisible) {	
			tile->StepIn();
			tile->Render();
			return;
		}
	}

	tile->last_used = oapiGetSysTime();

	if (tile->state == Tile::ForRender) {
		if (scene->GetRenderPass() == RENDERPASS_MAINSCENE) tile->MatchEdges ();
		tile->StepIn ();
		tile->Render ();
	} else if (tile->state == Tile::Active) {
		tile->StepIn ();
		for (int i = 0; i < 4; i++) {
			if (node->Child(i)) {
				if (node->Child(i)->Entry() && (node->Child(i)->Entry()->state & TILE_ACTIVE)) {
					RenderNode (node->Child (i));	// step down into subtree
				}
			}
		}
	}
}

// -----------------------------------------------------------------------

template<class TileType>
void TileManager2Base::RenderNodeLabels(QuadTreeNode<TileType> *node, D3D9Pad *skp, oapi::Font **labelfont, int *fontidx)
{
	TileType *tile = node->Entry();
	if (tile->state == Tile::ForRender || tile->state == Tile::Active) {
		tile->RenderLabels(skp, labelfont, fontidx);

		// step down to next quadtree level
		if (tile->state == Tile::Active) {
			for (int i = 0; i < 4; i++)
				if (node->Child(i))
					if (node->Child(i)->Entry() && (node->Child(i)->Entry()->state & TILE_ACTIVE))
						RenderNodeLabels(node->Child(i), skp, labelfont, fontidx);
		}
	}
}

// =======================================================================
// =======================================================================

template<class TileType>
TileManager2<TileType>::TileManager2 (vPlanet *vplanet, int _maxres, int _gridres)
	: TileManager2Base (vplanet, _maxres, _gridres),
	ntreeMgr(0)
{
	// Initialise the compressed packed tile archives
	LoadZTrees();
	InitHasIndividualFiles();

	// Load the low-res full-sphere tiles
	for (int i = 0; i < 3; i++)
	{
		globtile[i] = new TileType(this, i - 3, 0, 0);
		globtile[i]->PreLoad();
		globtile[i]->Load();
	}

	// Set the root tiles for level 0
	for (int i = 0; i < 2; i++) {
		tiletree[i].SetEntry (new TileType (this, 0, 0, i));
		tiletree[i].Entry()->PreLoad();
		tiletree[i].Entry()->Load();
	}
}

// -----------------------------------------------------------------------

template<class TileType>
TileManager2<TileType>::~TileManager2 ()
{
	//oapiWriteLogV("=== Tile Dump for %s ===", vp->GetName());
	//for (int i = 0; i < 2; i++)
	//  DebugDump(&tiletree[i]);

	for (int i = 0; i < 2; i++)
		tiletree[i].DelChildren();
	for (int i = 0; i < 3; i++)
		delete globtile[i];

	if (ntreeMgr) {
		for (int i = 0; i < ntreeMgr; i++)
			if (treeMgr[i]) delete treeMgr[i];
		delete []treeMgr;
		delete[]hasIndividualFiles;
	}
}

// -----------------------------------------------------------------------

template<class TileType>
void TileManager2<TileType>::CheckCoverage (const QuadTreeNode<TileType> *node,
	double latmin, double latmax, double lngmin, double lngmax,
	int maxlvl, const Tile **tbuf, int nt, int *nfound) const
{
	if (*nfound < 0) return; // error state already set
	double t_latmin, t_latmax, t_lngmin, t_lngmax;
	const Tile *t = node->Entry();
	t->Extents (&t_latmin, &t_latmax, &t_lngmin, &t_lngmax);
	if (latmin >= t_latmax || latmax <= t_latmin || lngmin >= t_lngmax || lngmax <= t_lngmin) return; // no overlap
	// note: need to check for longitude wrap here!

	if (t->Level() < maxlvl) {
		bool has_children = false;
		for (int i = 0; i < 4; i++) {
			if (node->Child(i) && node->Child(i)->Entry() && node->Child(i)->Entry()->Tex()) {
				CheckCoverage (node->Child(i), latmin, latmax, lngmin, lngmax, maxlvl, tbuf, nt, nfound);
				has_children = true;
			}
		}
		if (has_children) return;
	}

	if (*nfound == nt) { // no space left
		*nfound = -1; // set error state
	} else {
		tbuf[*nfound] = t;
		*nfound = *nfound+1;
	}
}

// -----------------------------------------------------------------------

template<class TileType>
Tile *TileManager2<TileType>::SearchTileSub (const QuadTreeNode<TileType> *node, double lng, double lat, int maxlvl, bool bOwntex) const
{
	Tile *t = node->Entry();

	if (!t) return NULL;
	if (!t->HasOwnTex() && bOwntex) return NULL;
	if ((t->State()&TILE_VALID)==0) return NULL;
	if (t->Level()==maxlvl) return t;

	int i = 0;
	if (lng > (t->bnd.minlng + t->bnd.maxlng)*0.5) i++;
	if (lat < (t->bnd.minlat + t->bnd.maxlat)*0.5) i += 2;

	const QuadTreeNode<TileType> *next = node->Child(i);
	if (next) {
		Tile *check = SearchTileSub(next, lng, lat, maxlvl, bOwntex);
		if (check) return check;
	}
	return t;
}

#endif // !__TILEMGR2_IMP_HPP
