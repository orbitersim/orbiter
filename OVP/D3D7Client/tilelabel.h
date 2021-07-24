// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#ifndef __TILELABEL_H
#define __TILELABEL_H

#include "surfmgr2.h"
#include "ZTreeMgr.h"

class TileLabel {
public:
	static TileLabel *Create (const SurfTile *stile);
	// TileLabel factory. Returns NULL if no labels available for this tile

	TileLabel(const SurfTile *stile);
	~TileLabel();
	void Render(oapi::Sketchpad *skp, oapi::Font **labelfont, int *fontidx);

	struct TLABEL {
		TLABEL() { labeltype = 0; label = 0; }
		~TLABEL() { if(label) delete[] label; }
		double lat, lng, alt;
		VECTOR3 pos;
		char labeltype;
		char *label;
	};

protected:
	bool Read();
	bool ExtractAncestorData (const SurfTile *atile);
	double Elevation(double lat, double lng, double latmin, double latmax, double lngmin, double lngmax, double elev_res) const;

private:
	const SurfTile *tile;
	DWORD nlabel, nbuf;
	TLABEL **label;        // the list read from file
	DWORD nrenderlabel, nrenderbuf;
	TLABEL **renderlabel;  // the list to be rendered, extracted from an ancestor label list
	bool makeRenderList;   // flag for 
};

#endif // !__TILELABEL_H