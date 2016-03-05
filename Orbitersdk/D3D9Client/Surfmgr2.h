// ==============================================================
//   ORBITER VISUALISATION PROJECT (OVP)
//   Copyright (C) 2006-2016 Martin Schweiger
//   Dual licensed under GPL v3 and LGPL v3
// ==============================================================

#ifndef __SURFMGR2_H
#define __SURFMGR2_H

#include "Tilemgr2_imp.hpp"

/**
 * \brief Planetary surface rendering engine.
 *
 * Planetary surface rendering engine v2, including a simple
 * LOD (level-of-detail) algorithm for surface patch resolution.
 */
class SurfTile: public Tile {
	friend class TileManager2Base;
	template<class SurfTile> friend class TileManager2;

	void MatchEdges ();

public:
	SurfTile (TileManager2Base *_mgr, int _lvl, int _ilat, int _ilng);
	~SurfTile ();

	inline void SetNode (QuadTreeNode<SurfTile> *_node) { node = _node; }
	// Register the tile to a quad tree node

protected:
	virtual Tile *getParent() { return node->Parent() ? node->Parent()->Entry() : NULL; }
	inline SurfTile *getSurfParent() { return node->Parent() ? node->Parent()->Entry() : NULL; }
	SurfTile *getTextureOwner();

	// Return pointer to parent tile, if exists

	void Load ();
	void PreLoad ();
	INT16 *ReadElevationFile (const char *name, int lvl, int ilat, int ilng, double *mean_elev=0);
	bool LoadElevationData ();
	void Render ();
	void StepIn ();

	TileManager2<SurfTile> *smgr;	// surface tile manager interface
	QuadTreeNode<SurfTile> *node;	// my node in the quad tree, if I'm part of a tree

	void FixCorner (const SurfTile *nbr);
	// Match corner elevation to neighbour

	void FixLongitudeBoundary (const SurfTile *nbr, bool keep_corner=false);
	// Match longitude edge elevation to neighbour. If keep_corner==true, skip corner node

	void FixLatitudeBoundary (const SurfTile *nbr, bool keep_corner=false);
	// Match latitude edge elevation to neighbour. If keep_corner==true, skip corner node

private:
	INT16 *ElevationData () const;
	double GetMeanElevation (const INT16 *elev) const;
	float fixinput(double, int);
	D3DXVECTOR4 MicroTexRange(SurfTile *pT, int lvl) const;

	D3DXVECTOR2 MicroRep[3];
	DWORD MaxRep;
	LPDIRECT3DTEXTURE9 ltex;		// landmask/nightlight texture, if applicable
	INT16 *elev;               // elevation data [m] (8x subsampled)
	mutable INT16 *ggelev;     // pointer to my elevation data in the great-grandparent
	bool has_elevfile;          // true if the elevation data for this tile were read from file
};

#endif // !__SURFMGR2_H