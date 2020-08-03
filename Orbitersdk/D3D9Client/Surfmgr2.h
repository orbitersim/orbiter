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

#pragma pack(push,1)

struct ELEVFILEHEADER { // file header for patch elevation data file
	char id[4];            // ID string + version ('E','L','E',1)
	int hdrsize;           // header size (76 expected)
	int dtype;             // data format (0=flat, no data block; 8=uint8; -8=int8; 16=uint16; -16=int16)
	int xgrd, ygrd;         // data grid size (259 x 259 expected)
	int xpad, ypad;         // horizontal, vertical padding width (1, 1 expected)
	double scale;          // data scaling factor (1.0 expected)
	double offset;         // data offset (elevation = raw value * scale + offset)
	double latmin, latmax; // latitude range [rad]
	double lngmin, lngmax; // longitude range [rad]
	double emin, emax, emean; // min, max, mean elevation [m]
};

#pragma pack(pop)

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

	inline QuadTreeNode<SurfTile> *GetNode() const { return node; }
	inline void SetNode (QuadTreeNode<SurfTile> *_node) { node = _node; }
	// Register the tile to a quad tree node

	int GetElevation(double lng, double lat, double *elev, FVECTOR3 *nrm=NULL, SurfTile **cache=NULL, bool bFilter=true, bool bGet=false) const;

	double GetCameraDistance();
	SurfTile *getTextureOwner();

	LPDIRECT3DTEXTURE9 SetOverlay(LPDIRECT3DTEXTURE9 pOverlay, bool bOwn = true);
	bool DeleteOverlay(LPDIRECT3DTEXTURE9 pOverlay = NULL);

	double GetMinElev() const { return ehdr.emin; }		// virtual from Tile::
	double GetMaxElev() const { return ehdr.emax; }		// virtual from Tile::
	double GetMeanElev() const { return ehdr.emean; }	// virtual from Tile::

protected:
	virtual Tile *getParent() const { return node && node->Parent() ? node->Parent()->Entry() : NULL; }
	inline SurfTile *getSurfParent() const { return node && node->Parent() ? node->Parent()->Entry() : NULL; }

	// Return pointer to parent tile, if exists

	void Load ();
	void PreLoad ();
	INT16 *ReadElevationFile (const char *name, int lvl, int ilat, int ilng);
	bool LoadElevationData ();
	void Render ();
	void StepIn ();
	bool IsElevated() { return (ggelev!=NULL); }

	TileManager2<SurfTile> *smgr;	// surface tile manager interface
	QuadTreeNode<SurfTile> *node;	// my node in the quad tree, if I'm part of a tree

	void FixCorner (const SurfTile *nbr);
	// Match corner elevation to neighbour

	void FixLongitudeBoundary (const SurfTile *nbr, bool keep_corner=false);
	// Match longitude edge elevation to neighbour. If keep_corner==true, skip corner node

	void FixLatitudeBoundary (const SurfTile *nbr, bool keep_corner=false);
	// Match latitude edge elevation to neighbour. If keep_corner==true, skip corner node

	// v2 Labels interface -----------------------------------------------
	void CreateLabels();    ///< create the label object from the label tile file, if available
	void DeleteLabels();    ///< delete the TileLabel object if it exists
	void RenderLabels(D3D9Pad *skp, oapi::Font **labelfont, int *fontidx);

private:
	bool InterpolateElevationGrid(const float *pelev, float *elev);
	float Interpolate(FMATRIX4 &in, float x, float y);
	float *ElevationData () const;
	void ComputeElevationData(const float *elev) const;
	float fixinput(double, int);
	D3DXVECTOR4 MicroTexRange(SurfTile *pT, int lvl) const;

	mutable ELEVFILEHEADER ehdr;///< Let's store the complete header for later use
	D3DXVECTOR2 MicroRep[3];
	DWORD MaxRep;
	LPDIRECT3DTEXTURE9 ltex;	///< landmask/nightlight texture, if applicable
	INT16 *elev_file;			///< elevation data [m]
	float *elev;				///< elevation data [m] (8x subsampled)
	mutable float *ggelev;		///< pointer to my elevation data in the great-grandparent

	TileLabel *label;			///< surface labels associated with this tile
};

#endif // !__SURFMGR2_H
