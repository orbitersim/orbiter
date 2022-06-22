#ifndef ELEVTILE_H
#define ELEVTILE_H

#include "tile.h"
#include "cmap.h"

#define TILE_FILERES 256
#define TILE_ELEVSTRIDE (TILE_FILERES+3)

struct ElevData {
	DWORD width;              // elevation grid width (including padding)
	DWORD height;             // elevation grid height (including padding)
	std::vector<double> data; // elevation grid data [m]
	double dmin, dmax;        // min, max tile elevation [m]
	double dres;              // target elevation resolution [m] (must be 2^n with integer n)
	ElevData();
	ElevData(const ElevData &edata);
	ElevData &operator=(const ElevData &edata);
	double nodeValue(int ix, int iy) const;
	void setNodeValue(int ix, int iy, double v);
	ElevData SubTile(const std::pair<DWORD, DWORD> &xrange, const std::pair<DWORD, DWORD> &yrange);
	void RescanLimits();
};

struct ElevDisplayParam {
	CmapName cmName;
	bool useWaterMask;
	bool autoRange;
	double rangeMin;
	double rangeMax;

	ElevDisplayParam() {
		cmName = CMAP_GREY;
		useWaterMask = false;
		autoRange = true;
		rangeMin = 0.0;
		rangeMax = 1000.0;
	}
};


class ElevTile : public Tile {
	friend class TileBlock;
	friend class ElevTileBlock;

public:
	ElevTile(const ElevTile &etile);
	static ElevTile *Load(int lvl, int ilat, int ilng);
	static ElevTile *InterpolateFromAncestor(int lvl, int ilat, int ilng, const Cmap *cm = 0);
	static void setTreeMgr(const ZTreeMgr *mgr, const ZTreeMgr *modMgr = 0);
	const std::string Layer() const { return std::string("Elev"); }
	double nodeElevation(int ndx, int ndy);

	virtual void set(const Tile *tile);

	ElevData &getData() { return m_edata; }
	const ElevData &getData() const { return m_edata; }
	ElevData &getBaseData() { return m_edataBase; }
	const ElevData &getBaseData() const { return m_edataBase; }
	bool isModified() const { return m_modified; }
	void dataChanged(int exmin = -1, int exmax = -1, int eymin = -1, int eymax = -1);
	void Save();
	void SaveMod();
	void MatchNeighbourTiles();
	bool mapToAncestors(int minlvl) const;

	/**
	 * \brief Interpolate the tile to the next resolution level
	 */
	TileBlock *ProlongToChildren() const;

	void setWaterMask(const MaskTile *mtile);

protected:
	ElevTile(int lvl, int ilat, int ilng);
	bool Load(bool directOnly = false);
	bool InterpolateFromAncestor();
	void LoadSubset();
	void LoadData(ElevData &edata, int lvl, int ilat, int ilng);
	void LoadModData(ElevData &edata, int lvl, int ilat, int ilng);
	void RescanLimits();

private:
	ElevData m_edata;
	ElevData m_edataBase;
	bool m_modified;
	std::vector<bool> m_waterMask;

	static const ZTreeMgr *s_treeMgr;
	static const ZTreeMgr *s_treeModMgr;
};

#endif // !ELEVTILE_H
