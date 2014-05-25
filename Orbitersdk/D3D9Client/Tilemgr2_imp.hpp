// ==============================================================
//   ORBITER VISUALISATION PROJECT (OVP)
//   Copyright (C) 2006-2014 Martin Schweiger
//   Dual licensed under GPL v3 and LGPL v3
// ==============================================================

// =======================================================================
// tilemgr2_imp.hpp
// Rendering of planetary surfaces using texture tiles at
// variable resolutions (new version).
// =======================================================================

#ifndef __TILEMGR2_IMP_HPP
#define __TILEMGR2_IMP_HPP

#include "tilemgr2.h"

// -----------------------------------------------------------------------

template<class TileType>
QuadTreeNode<TileType> *TileManager2Base::FindNode (QuadTreeNode<TileType> root[2], int lvl, int ilat, int ilng)
{
	int i, sublat, sublng, subidx;

	// wrap at longitude += 180
	int nlng = 2 << lvl;
	if (ilng < 0) ilng += nlng;
	else if (ilng >= nlng) ilng -= nlng;

	// Find the level-0 root
	QuadTreeNode<TileType> *node = root + ((ilng >> lvl) & 1);
	for (i = lvl-1; i >= 0; i--) {
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
	else
		tile->Load ();
	return child;
}

// -----------------------------------------------------------------------

template<class TileType>
void TileManager2Base::ProcessNode (QuadTreeNode<TileType> *node)
{
	static const double res_scale = 1.1; // resolution scale with distance

	Tile *tile = node->Entry();
	tile->state = Tile::ForRender;
	tile->edgeok = false;
	int lvl = tile->lvl;
	int ilng = tile->ilng;
	int ilat = tile->ilat;
	int nlng = 2 << lvl;
	int nlat = 1 << lvl;
	bool bstepdown = true;
	double bias = resolutionBias;
	if (ilat < nlat/6 || ilat >= nlat-nlat/6) { // lower resolution at the poles
		bias -= 1.0;
		if (ilat < nlat/12 || ilat >= nlat-nlat/12)
			bias -= 1.0;
	}

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
			node->DelChildren ();             // remove the sub-tree
			tile->state = Tile::Invisible;
			return;                           // no need to continue
		}
	}

	// Check if patch bounding box intersects viewport
	MATRIX4 transform = mul (WorldMatrix (ilng, nlng, ilat, nlat), prm.dviewproj);
	if (!tile->InView (transform)) {
		if (lvl == 0)
			bstepdown = false;
		else {
			node->DelChildren ();             // remove the sub-tree
			tile->state = Tile::Invisible;
			return;
		}
	}

	// Compute target resolution level based on tile distance
	if (bstepdown) {
		double tdist;
		double erad = 1.0 + tile->mean_elev/obj_size; // radius of unit sphere plus elevation
		if (adist < 0.0) { // if we are above the tile, use altitude for distance measurement
			tdist = prm.cdist-erad;
		} else { // use distance to closest tile edge
			double h = erad*sin(adist);
			double a = prm.cdist - erad*cos(adist);
			tdist = sqrt(a*a + h*h);
		}
		int tgtres = (tdist < 1e-6 ? prm.maxlvl : max (0, min (prm.maxlvl, (int)(bias - log(tdist)*res_scale))));
		bstepdown = (lvl < tgtres);
	}

	// Recursion to next level: subdivide into 2x2 patch
	if (bstepdown) {
		bool subcomplete = true;
		int i, idx;
		// check if all 4 subtiles are available already, and queue any missing for loading
		for (idx = 0; idx < 4; idx++) {
			QuadTreeNode<TileType> *child = node->Child(idx);
			if (!child)
				child = LoadChildNode (node, idx);
			else if (child->Entry()->state == Tile::Invalid)
				loader->LoadTileAsync (child->Entry());
			Tile::TileState state = child->Entry()->state;
			if (!(state & TILE_VALID))
				subcomplete = false;
		}
		if (subcomplete) {
			tile->state = Tile::Active;
			for (i = 0; i < 4; i++)
				ProcessNode (node->Child(i));
			return; // otherwise render at current resolution until all subtiles are available
		}
	}

	if (!bstepdown)
		node->DelChildren ();
}

// -----------------------------------------------------------------------

template<class TileType>
void TileManager2Base::RenderNode (QuadTreeNode<TileType> *node)
{
	TileType *tile = node->Entry();

	if (tile->state == Tile::ForRender) {
		int lvl = tile->lvl;
		int ilng = tile->ilng;
		int ilat = tile->ilat;
		int nlng = 2 << lvl;
		int nlat = 1 << lvl;
		tile->MatchEdges ();
		SetWorldMatrix (WorldMatrix (ilng, nlng, ilat, nlat));
		tile->Render ();
	} else if (tile->state == Tile::Active) {
		for (int i = 0; i < 4; i++) {
			if (node->Child(i)) {
				if (node->Child(i)->Entry() && (node->Child(i)->Entry()->state & TILE_ACTIVE)) {
					RenderNode (node->Child (i)); // step down into subtree
				}
			}
		}
	}
}


// =======================================================================
// =======================================================================

template<class TileType>
TileManager2<TileType>::TileManager2 (const vPlanet *vplanet, int _maxres)
: TileManager2Base (vplanet, _maxres)
{
	// Set the root tiles for level 0
	for (int i = 0; i < 2; i++) {
		tiletree[i].SetEntry (new TileType (this, 0, 0, i));
		tiletree[i].Entry()->Load();
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

#endif // !__TILEMGR2_IMP_HPP