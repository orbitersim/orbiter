#ifndef __TILELABEL_H
#define __TILELABEL_H

#include "Surfmgr2.h"
#include "ZTreeMgr.h"
#include "Sketchpad2.h"

class TileLabel {
public:
	static TileLabel *Create (const SurfTile *stile);  ///< TileLabel factory. Returns NULL if no labels available for this tile

	TileLabel(const SurfTile *stile);
	~TileLabel();
	void Render(oapi::Sketchpad2 *skp, oapi::Font **labelfont, int *fontidx);

	struct TLABEL {
		TLABEL() : labeltype(0), label(NULL) {}
		~TLABEL() { SAFE_DELETEA(label); }
		double  lat, lng, alt; ///< spheric coordinates of the label
		VECTOR3 pos;           ///< position of the label
		char    labeltype;     ///< ???
		char   *label;         ///< the label (its text)
	};

protected:
	bool   Read();
	bool   ExtractAncestorData (const SurfTile *atile);
	double Elevation(double lat, double lng, double latmin, double latmax, double lngmin, double lngmax, double elev_res) const;

private:
	const SurfTile *tile;
	DWORD nlabel, nbuf;
	TLABEL **label;        ///< the list read from file
	DWORD nrenderlabel, nrenderbuf;
	TLABEL **renderlabel;  ///< the list to be rendered, extracted from an ancestor label list
	bool makeRenderList;   ///< flag for ...
};

#endif // !__TILELABEL_H