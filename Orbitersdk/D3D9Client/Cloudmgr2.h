// ==============================================================
//   ORBITER VISUALISATION PROJECT (OVP)
//   Copyright (C) 2006-2016 Martin Schweiger
//   Dual licensed under GPL v3 and LGPL v3
// ==============================================================

#ifndef __CLOUDMGR2_H
#define __CLOUDMGR2_H

#include "Tilemgr2_imp.hpp"

/**
 * \brief Visual representation of planetary cloud tile.
 *
 * Rendering of planetary cloud layers using texture tiles at
 * variable resolutions (new version).
 */
class CloudTile: public Tile {
	friend class TileManager2Base;
	template<class CloudTile> friend class TileManager2;

public:
	CloudTile (TileManager2Base *_mgr, int _lvl, int _ilat, int _ilng);
	~CloudTile ();

	inline void SetNode (QuadTreeNode<CloudTile> *_node) { node = _node; }
	// Register the tile to a quad tree node

	double GetMinElev() const { return cloudalt; }		// virtual from Tile::
	double GetMaxElev() const { return cloudalt; }		// virtual from Tile::
	double GetMeanElev() const { return cloudalt; }		// virtual from Tile::

protected:
	virtual Tile *getParent() const { return node && node->Parent() ? node->Parent()->Entry() : NULL; }
	// Return pointer to parent tile, if exists

	virtual void Load ();
	virtual void PreLoad ();
	virtual void Render ();

	double cloudalt;
	TileManager2<CloudTile> *cmgr;	// cloud tile manager interface
	QuadTreeNode<CloudTile> *node;	// my node in the quad tree, if I'm part of a tree
};

#endif // !__CLOUDMGR2_H
