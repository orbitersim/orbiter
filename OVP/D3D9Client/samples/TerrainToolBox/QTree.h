

#include "OrbiterAPI.h"
#include <Windows.h>
#include "gcCoreAPI.h"

#pragma once

struct SubTex {
	class QTree *pNode;
	RECT range;
};


//      +N
//   X--------
//   | 0 | 1 |
//-W --------- E+
//   | 2 | 3 |
//   ---------
//		 S-



class QTree
{
public:
				QTree(gcCore2 *pCore, OBJHANDLE hPlanet);
				QTree(QTree *pParent, int q);
				~QTree();

	QTree *		GetParent() const { return pParent; }
	QTree *		GetChild(int idx);
	QTree *		GetNextNode(double lng, double lat);
	QTree *		FindNode(double lng, double lat, int level);
	QTree *		HighestOwn(int what, double lng, double lat);
	QTree *		GetTextureOwner();
	SubTex		GetSubTexRange(int flags);
	SURFHANDLE	GetTexture(int flags = 0);	// Do not release the surface
	INT16 *		GetElevation();
	double		Width() { return fabs(Bounds.right - Bounds.left); }
	double		Height() { return fabs(Bounds.bottom - Bounds.top); }
	int			SaveTile(int flags, SURFHANDLE hSurf, SURFHANDLE hTemp, DRECT bounds, int res, float alpha);
	bool		Intersect(DRECT ovl, DRECT *src, DRECT *tgt) const;
	bool		MapRect(DRECT OvlRect, SURFHANDLE hOvlSrf, RECT &src, RECT &tgt);
	bool		HasOwnTex(int type = 0);

	// --------------------
	QTree *		pChild[4];
	QTree *		pParent;
	// --------------------
	gcCore2 *	pCore;
	HPLANETMGR	hMgr;
	SURFHANDLE	hTex;				// Texture
	SURFHANDLE	hMask;				// Nightlights
	INT16 *		pElev;				// Elevation
	// --------------------
	DRECT		Bounds;				// Bounds (i.e. min/max lng/lat)
	double		clng, clat;			// Tile center
	int			level, quarant;
	int			nlng, nlat;			// Number of tiles in a level
	int			ilng, ilat;			// Tile index
};
