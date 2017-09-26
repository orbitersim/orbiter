// ==============================================================
// TileLabel.h
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2017 Martin Schweiger (martins/apogee)
//                    Peter Schneider (Kuddel)
// ==============================================================

#ifndef __TILELABEL_H
#define __TILELABEL_H

#include "Surfmgr2.h"
#include "ZTreeMgr.h"
#include "Sketchpad2.h"

class TileLabel {
public:
	static TileLabel *Create (const SurfTile *stile);  ///< TileLabel factory. Returns NULL if no labels available for this tile

	TileLabel (const SurfTile *stile);
	~TileLabel ();

	void Render (oapi::Sketchpad2 *skp, oapi::Font **labelfont, int *fontidx);

	struct TLABEL {
		TLABEL() : labeltype(0), len(0), label(NULL), pos() {}
		~TLABEL() { SAFE_DELETEA(label); }
		double  lat, lng, alt; ///< spheric coordinates of the label
		VECTOR3 pos;           ///< position of the label
		char    labeltype;     ///< label type ID (what feature group it belongs to)
		int     len;           ///< label length WITHOUT terminating zero!
		LPSTR   label;         ///< the label (might contain multiple lines)
	};

protected:
	bool   Read ();
	bool   ExtractAncestorData (const SurfTile *atile);
	double Elevation (double lat, double lng, double latmin, double latmax, double lngmin, double lngmax, double elev_res) const;

private:
	void   StoreLabel (TLABEL *l, const std::string &name);  ///< store (new) label to label-storage (**label)

	const SurfTile *tile;           ///< associated surface tile
	DWORD nlabel, nbuf;             ///< number of allocated labels and label buffer size
	TLABEL **label;                 ///< the list read from file
	DWORD nrenderlabel, nrenderbuf; ///< number of allocated render-labels and render-label buffer size
	TLABEL **renderlabel;           ///< the list to be rendered, extracted from an ancestor label list
};

#endif // !__TILELABEL_H