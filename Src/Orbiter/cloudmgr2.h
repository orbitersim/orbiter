// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#ifndef __CLOUDMGR2_H
#define __CLOUDMGR2_H

// Subsystem for loading, managing and displaying planetary cloud layers

#include "tilemgr2_imp.hpp"

// =======================================================================
// =======================================================================

class CloudTile: public Tile {
	friend class TileManager2Base;
	template<class CloudTile> friend class TileManager2;

public:
	CloudTile (TileManager2Base *_mgr, int _lvl, int _ilat, int _ilng);
	~CloudTile ();

	inline void SetNode (QuadTreeNode<CloudTile> *_node) { node = _node; }
	// Register the tile to a quad tree node

protected:
	virtual Tile *getParent() { return (node && node->Parent()) ? node->Parent()->Entry() : NULL; }
	// Return pointer to parent tile, if exists

	virtual void Load ();
	virtual void Render ();

	TileManager2<CloudTile> *cmgr;	// cloud tile manager interface
	QuadTreeNode<CloudTile> *node;	// my node in the quad tree, if I'm part of a tree
};

#endif // !__CLOUDMGR2_H