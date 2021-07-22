// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#ifndef __CSPHEREMGR2_H
#define __CSPHEREMGR2_H

#include "tilemgr2_imp.hpp"

typedef TileManager2<CsphereTile> CsphereManager;
typedef QuadTreeNode<CsphereTile> CsphereNode;

class CsphereTile : public Tile {
	friend class TileManager2Base;
	template<class CsphereTile> friend class TileManager2;

public:
	CsphereTile(TileManager2Base *_mgr, int _lvl, int _ilat, int _ilng);
	~CsphereTile();

	inline void SetNode(CsphereNode *_node) { node = _node; }

protected:
	virtual Tile *getParent() { return node && node->Parent() ? node->Parent()->Entry() : NULL; }
	inline CsphereTile *getCsphereParent() { return node && node->Parent() ? node->Parent()->Entry() : NULL; }
	// Return pointer to parent tile, if exists

	void Load();
	void Render();

	CsphereManager *cmgr;  // csphere tile manager interface
	CsphereNode *node;  // my node in the quadtree, if I am part of a tree
};

#endif // !__CSPHEREMGR2_H
