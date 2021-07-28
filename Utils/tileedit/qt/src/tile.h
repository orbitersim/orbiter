#ifndef TILE_H
#define TILE_H

#include <windows.h>
#include <vector>
#include "ddsread.h"
#include "ZTreeMgr.h"

#define TILE_SURFSTRIDE 512

enum TileMode {
	TILEMODE_NONE,
	TILEMODE_SURFACE,
	TILEMODE_WATERMASK,
	TILEMODE_NIGHTLIGHT,
	TILEMODE_ELEVATION,
	TILEMODE_ELEVMOD
};

enum TileLoadMode {
	TILELOADMODE_USEGLOBALSETTING,
	TILELOADMODE_DIRECTONLY,
	TILELOADMODE_ANCESTORSUBSECTION,
	TILELOADMODE_ANCESTORINTERPOLATE
};

inline int nLat(int lvl) { return (lvl < 4 ? 1 : 1 << (lvl - 4)); }
inline int nLng(int lvl) { return (lvl < 4 ? 1 : 1 << (lvl - 3)); }

void ensureLayerDir(const char *rootDir, const char *layer, int lvl, int ilat);


class Tile
{
public:
    Tile(int lvl, int ilat, int ilng);
	Tile(const Tile &tile);
    int Level() const { return m_lvl; }
    int iLat() const { return m_ilat; }
    int iLng() const { return m_ilng; }
	int subLevel() const { return m_sublvl; }
	int subiLat() const { return m_subilat; }
	int subiLng() const { return m_subilng; }
	int nLat() const { return ::nLat(m_lvl); }
	int nLng() const { return ::nLng(m_lvl); }

	virtual void set(const Tile *tile);
	void setLevel(int lvl) { m_lvl = lvl; }
	void setiLat(int ilat) { m_ilat = ilat; }
	void setiLng(int ilng) { m_ilng = ilng; }
	void setSubLevel(int lvl) { m_sublvl = lvl; }
	void setSubiLat(int ilat) { m_subilat = ilat; }
	void setSubiLng(int ilng) { m_subilng = ilng; }

	virtual const std::string Layer() const = 0;

	static void setRoot(const std::string &root);
	static const std::string &root() { return s_root; }
	static void setOpenMode(int mode);
	static void setGlobalLoadMode(TileLoadMode mode);

	virtual bool mapToAncestors(int minlvl) const { return false; }

protected:
	void ensureLayerDir();
	void ensureTmpLayerDir();

    int m_lvl;
    int m_ilat;
    int m_ilng;
    int m_sublvl;
    int m_subilat;
    int m_subilng;
    std::pair<DWORD, DWORD> lat_subrange;
    std::pair<DWORD, DWORD> lng_subrange;

	static std::string s_root;
	static int s_openMode;
	static TileLoadMode s_globalLoadMode;
};

class DXT1Tile: public Tile
{
	friend class TileBlock;

public:
	DXT1Tile(int lvl, int ilat, int ilng);
	DXT1Tile(const DXT1Tile &tile);
	virtual void set(const Tile *tile);
	Image &getData() { return m_idata; }
	const Image &getData() const { return m_idata; }
	int TileSize() const;

protected:
	void SaveDXT1();
	void SavePNGtmp();
	bool LoadDXT1(const ZTreeMgr *mgr = 0, TileLoadMode mode = TILELOADMODE_USEGLOBALSETTING);
	bool LoadPNGtmp();
	void LoadSubset(const ZTreeMgr *mgr = 0);
	void LoadData(Image &im, int lvl, int ilat, int ilng, const ZTreeMgr *mgr);
	TileBlock *ProlongToChildren() const;

	Image m_idata;
};

class SurfTile: public DXT1Tile
{
public:
	SurfTile(int lvl, int ilat, int ilng);
	static SurfTile *Load(int lvl, int ilat, int ilng, TileLoadMode mode = TILELOADMODE_USEGLOBALSETTING);
	static SurfTile *InterpolateFromAncestor(int lvl, int ilat, int ilng);
	void Save();
	static void setTreeMgr(const ZTreeMgr *mgr);
	const std::string Layer() const { return std::string("Surf"); }
	bool mapToAncestors(int minlvl) const;
	
protected:
	bool InterpolateFromAncestor();
	static const ZTreeMgr *s_treeMgr;
};

class MaskTile : public DXT1Tile
{
public:
	static MaskTile *Load(int lvl, int ilat, int ilng);
	static void setTreeMgr(const ZTreeMgr *mgr);
	const std::string Layer() const { return std::string("Mask"); }

protected:
	MaskTile(int lvl, int ilat, int ilng);
	static const ZTreeMgr *s_treeMgr;
};

#endif // TILE_H
