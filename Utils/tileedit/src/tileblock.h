#ifndef TILEBLOCK_H
#define TILEBLOCK_H

#include "tile.h"
#include "elevtile.h"

class TileBlock
{
public:
	TileBlock(int lvl, int ilat0, int ilat1, int ilng0, int ilng1);
	TileBlock(const TileBlock &tileblock);
	virtual ~TileBlock();
	int Level() const { return m_lvl; }
	int minSubLevel() const;
	int iLat0() const { return m_ilat0; }
	int iLat1() const { return m_ilat1; }
	int iLng0() const { return m_ilng0; }
	int iLng1() const { return m_ilng1; }
	int nLat() const { return (m_lvl < 4 ? 1 : 1 << (m_lvl - 4)); }
	int nLng() const { return (m_lvl < 4 ? 1 : 1 << (m_lvl - 3)); }
	int iLng_norm(int ilng) const { int ilngn = ilng; int nlng = nLng(); while (ilngn < 0) ilngn += nlng; while (ilngn >= nlng) ilngn -= nlng; return ilngn; }
	int nLatBlock() const { return m_nblocklat; }
	int nLngBlock() const { return m_nblocklng; }
	int nBlock() const { return m_nblocklat * m_nblocklng; }

	virtual void ExtractImage(Image &img, TileMode mode, int exmin = -1, int exmax = -1, int eymin = -1, int eymax = -1) const = 0;

	virtual bool setTile(int ilat, int ilng, const Tile *tile) { return false; }

	/**
	 * \brief Returns a pointer to one of the constituent tiles, using lat/lng index pair
	 */
	const Tile *getTile(int ilat, int ilng) const;
	Tile *_getTile(int ilat, int ilng);

	/**
	 * \brief Returns a pointer to one of the constituent tiles, unsing linear index
	 */
	const Tile *getTile(int idx) const;

	/**
	 * \brief Copies one of the constituent tiles and returns a pointer to the copy.
	 *
	 * The caller owns the copy and is responsible for deleting it.
	 */
	virtual Tile *copyTile(int ilat, int ilng) const = 0;

	/**
	 * \brief Copy one of the constituent tiles into an existing tile.
	 */
	virtual bool copyTile(int ilat, int ilng, Tile *tile) const = 0;

	/**
	* \brief Map edits to the tileblock back to one of the individual tiles
	*/
	virtual void syncTile(int ilat, int ilng) {};

	/**
	* \brief Map edits to the tileblock back to the individual tiles
	*
	* This must be called before calling Save() or SaveMod() to make sure the tiles are up to date.
	*/
	void syncTiles();

	/**
	 * \brief Returns true if at least one of the tiles has been synthesized from an ancestor
	 */
	bool hasAncestorData() const;

	/**
	* \brief Recursively map edits to parents down to level minlvl
	*
	* Requires SyncTiles to have been called.
	*/
	bool mapToAncestors(int minlvl) const;

protected:

	int m_lvl;
	int m_ilat0, m_ilat1;
	int m_ilng0, m_ilng1;
	int m_nblocklat, m_nblocklng;

	std::vector<Tile*> m_tile; ///< constituent tiles
};


class DXT1TileBlock : public TileBlock
{
public:
	DXT1TileBlock(int lvl, int ilat0, int ilat1, int ilng0, int ilng1);
	virtual void ExtractImage(Image &img, TileMode mode, int exmin = -1, int exmax = -1, int eymin = -1, int eymax = -1) const
	{ img = m_idata; }
	const Image &getData() const { return m_idata; }
	Image &getData() { return m_idata; }

protected:
	Image m_idata;
};


class SurfTileBlock : public DXT1TileBlock
{
public:
	static SurfTileBlock *Load(int lvl, int ilat0, int ilat1, int ilng0, int ilng1);
	SurfTileBlock(int lvl, int ilat0, int ilat1, int ilng0, int ilng1);
	virtual Tile *copyTile(int ilat, int ilng) const;
	virtual bool copyTile(int ilat, int ilng, Tile *tile) const;
	virtual void syncTile(int ilat, int ilng);
};


class MaskTileBlock : public DXT1TileBlock
{
public:
	static MaskTileBlock *Load(int lvl, int ilat0, int ilat1, int ilng0, int ilng1);
	virtual Tile *copyTile(int ilat, int ilng) const;
	virtual bool copyTile(int ilat, int ilng, Tile *tile) const;
	virtual void ExtractImage(Image &img, TileMode mode, int exmin = -1, int exmax = -1, int eymin = -1, int eymax = -1) const;

protected:
	MaskTileBlock(int lvl, int ilat0, int ilat1, int ilng0, int ilng1);
};


class ElevTileBlock : public TileBlock
{
public:
	static ElevTileBlock *Load(int lvl, int ilat0, int ilat1, int ilng0, int ilng1);
	ElevTileBlock(int lvl, int ilat0, int ilat1, int ilng0, int ilng1);
	ElevTileBlock(const ElevTileBlock &etileblock);
	static void setElevDisplayParam(const ElevDisplayParam *elevDisplayParam);
	bool setTile(int ilat, int ilng, const Tile *tile);
	ElevData &getData() { return m_edata; }
	ElevData &getBaseData() { return m_edataBase; }
	virtual Tile *copyTile(int ilat, int ilng) const;
	virtual bool copyTile(int ilat, int ilng, Tile *tile) const;
	void Save();
	void SaveMod();
	void ExportPNG(const std::string &fname, double vmin, double vmax);
	void setWaterMask(const MaskTileBlock *mtileblock);
	double nodeElevation(int ndx, int ndy) const;
	double nodeModElevation(int ndx, int ndy) const;
	void dataChanged(int exmin = -1, int exmax = -1, int eymin = -1, int eymax = -1);
	bool isModified() const { return m_isModified; }
	void RescanLimits();

	/**
	 * \brief Map edits to the tileblock back to one of the individual tiles
	 */
	void syncTile(int ilat, int ilng);

	/**
	* \brief Propagate edits in the boundary overlap zones to the neighbour tiles
	*/
	void MatchNeighbourTiles();

	void ExtractImage(Image &img, TileMode mode, int exmin = -1, int exmax = -1, int eymin = -1, int eymax = -1) const;

protected:
	void ExtractModImage(Image &img, TileMode mode, int exmin = -1, int exmax = -1, int eymin = -1, int eymax = -1) const;

private:
	ElevData m_edata;
	ElevData m_edataBase;
	std::vector<bool> m_waterMask;
	bool m_isModified;
	static const ElevDisplayParam *s_elevDisplayParam;
};

#endif // !TILEBLOCK_H