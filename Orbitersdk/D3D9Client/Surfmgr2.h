// ==============================================================
//   ORBITER VISUALISATION PROJECT (OVP)
//   Copyright (C) 2006-2016 Martin Schweiger
//   Dual licensed under GPL v3 and LGPL v3
// ==============================================================

#ifndef __SURFMGR2_H
#define __SURFMGR2_H

#include "Tilemgr2_imp.hpp"
#include "TileLabel.h"
#include "D3D9Pad.h"


/**
 * \brief Planetary surface rendering engine.
 *
 * Planetary surface rendering engine v2, including a simple
 * LOD (level-of-detail) algorithm for surface patch resolution.
 */
class SurfTile: public Tile {
	friend class TileManager2Base;
	template<class SurfTile> friend class TileManager2;
	friend class TileLabel;

	void MatchEdges ();

public:
	SurfTile (TileManager2Base *_mgr, int _lvl, int _ilat, int _ilng);
	~SurfTile ();

	inline void SetNode (QuadTreeNode<SurfTile> *_node) { node = _node; }
	// Register the tile to a quad tree node

	int GetElevation(double lng, double lat, double *elev, SurfTile **, bool bFilter=true);

	double GetCameraDistance();

protected:
	virtual Tile *getParent() const { return node && node->Parent() ? node->Parent()->Entry() : NULL; }
	inline SurfTile *getSurfParent() const { return node && node->Parent() ? node->Parent()->Entry() : NULL; }
	SurfTile *getTextureOwner();

	// Return pointer to parent tile, if exists

	void Load ();
	void PreLoad ();
	INT16 *ReadElevationFile (const char *name, int lvl, int ilat, int ilng, double tgt_res, double *mean_elev = 0);
	bool LoadElevationData ();
	void Render ();
	void RenderLabels(D3D9Pad *skp, oapi::Font **labelfont, int *fontidx);
	void StepIn ();
	//void Pick(MATRIX4 &dwMatrix, TILEPICK *pPick);

	TileManager2<SurfTile> *smgr;	// surface tile manager interface
	QuadTreeNode<SurfTile> *node;	// my node in the quad tree, if I'm part of a tree

	void FixCorner (const SurfTile *nbr);
	// Match corner elevation to neighbour

	void FixLongitudeBoundary (const SurfTile *nbr, bool keep_corner=false);
	// Match longitude edge elevation to neighbour. If keep_corner==true, skip corner node

	void FixLatitudeBoundary (const SurfTile *nbr, bool keep_corner=false);
	// Match latitude edge elevation to neighbour. If keep_corner==true, skip corner node

	void CreateLabels();    ///< create the label object from the label tile file, if available
	void DeleteLabels();    ///< delete the TileLabel object if it exists

private:
	bool InterpolateElevationGrid(float *pelev, float *elev);
	float Interpolate(FMATRIX4 &in, float x, float y);
	float *ElevationData () const;
	double GetMeanElevation (const float *elev) const;
	float fixinput(double, int);
	D3DXVECTOR4 MicroTexRange(SurfTile *pT, int lvl) const;

	LPDIRECT3DTEXTURE9 htex;
	D3DXVECTOR2 MicroRep[3];
	DWORD MaxRep;
	LPDIRECT3DTEXTURE9 ltex;	///< landmask/nightlight texture, if applicable
	INT16 *elev_file;			///< elevation data [m]
	float *elev;				///< elevation data [m] (8x subsampled)
	mutable float *ggelev;		///< pointer to my elevation data in the great-grandparent
	bool has_elevfile;			///< true if the elevation data for this tile were read from file

	TileLabel *label;          ///< surface labels associated with this tile
};

#endif // !__SURFMGR2_H
