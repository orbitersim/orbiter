#include "tile.h"
#include "tileblock.h"
#include "ddsread.h"
#include <iostream>
#include <algorithm>
#include <direct.h>
#include <dxt_io.h>

int Tile::s_openMode = 0x3;
TileLoadMode Tile::s_globalLoadMode = TILELOADMODE_ANCESTORSUBSECTION;
std::string Tile::s_root;

// ==================================================================================

void ensureLayerDir(const char *rootDir, const char *layer, int lvl, int ilat)
{
	char path[256];
	sprintf(path, "%s/%s", rootDir, layer);
	mkdir(path);
	sprintf(path, "%s/%s/%02d", rootDir, layer, lvl);
	mkdir(path);
	sprintf(path, "%s/%s/%02d/%06d", rootDir, layer, lvl, ilat);
	mkdir(path);
}


// ==================================================================================

Tile::Tile(int lvl, int ilat, int ilng)
{
    m_lvl = m_sublvl = lvl;
    m_ilat = m_subilat = ilat;
    m_ilng = m_subilng = ilng;

    int sz = 1 << min(lvl+6, 9);
    lat_subrange = std::make_pair(0, sz);
    lng_subrange = std::make_pair(0, sz);
}

Tile::Tile(const Tile &tile)
{
	m_lvl = tile.m_lvl;
	m_ilat = tile.m_ilat;
	m_ilng = tile.m_ilng;
	m_sublvl = tile.m_sublvl;
	m_subilat = tile.m_subilat;
	m_subilng = tile.m_subilng;
	lat_subrange = tile.lat_subrange;
	lng_subrange = tile.lng_subrange;
}

void Tile::set(const Tile *tile)
{
	m_lvl = tile->m_lvl;
	m_ilat = tile->m_ilat;
	m_ilng = tile->m_ilng;
	m_sublvl = tile->m_sublvl;
	m_subilat = tile->m_subilat;
	m_subilng = tile->m_subilng;
	lat_subrange = tile->lat_subrange;
	lng_subrange = tile->lng_subrange;
}

void Tile::setRoot(const std::string &root)
{
	s_root.assign(root);
}

void Tile::setOpenMode(int mode)
{
	s_openMode = mode;
}

void Tile::setGlobalLoadMode(TileLoadMode mode)
{
	s_globalLoadMode = mode;
}

void Tile::ensureLayerDir()
{
	::ensureLayerDir(s_root.c_str(), Layer().c_str(), m_lvl, m_ilat);
}

void Tile::ensureTmpLayerDir()
{
	char cbuf[1024];
	sprintf(cbuf, "%s/tileedit.tmp", s_root.c_str());
	mkdir(cbuf);
	::ensureLayerDir(cbuf, Layer().c_str(), m_lvl, m_ilat);
}

DXT1Tile::DXT1Tile(int lvl, int ilat, int ilng)
	: Tile(lvl, ilat, ilng)
{
}

DXT1Tile::DXT1Tile(const DXT1Tile &tile)
	: Tile(tile)
{
	m_idata = tile.m_idata;
}

void DXT1Tile::set(const Tile *tile)
{
	Tile::set(tile);
	const DXT1Tile *dxt1tile = dynamic_cast<const DXT1Tile*>(tile);
	if (dxt1tile)
		m_idata = dxt1tile->m_idata;
}

int DXT1Tile::TileSize() const
{
	return (m_lvl == 1 ? 128 : m_lvl == 2 ? 256 : TILE_SURFSTRIDE);
}

void DXT1Tile::SaveDXT1()
{
	char path[1024];
	sprintf(path, "%s/%s/%02d/%06d/%06d.dds", s_root.c_str(), Layer().c_str(), m_lvl, m_ilat, m_ilng);
	ensureLayerDir();
	dxt1write(path, m_idata);
}

void DXT1Tile::SavePNGtmp()
{
	char path[1024];
	sprintf(path, "%s/tileedit.tmp/%s/%02d/%06d/%06d.png", s_root.c_str(), Layer().c_str(), m_lvl, m_ilat, m_ilng);
	ensureTmpLayerDir();
	pngwrite_tmp(path, m_idata);
}

bool DXT1Tile::LoadDXT1(const ZTreeMgr *mgr, TileLoadMode mode)
{
	LoadData(m_idata, m_lvl, m_ilat, m_ilng, mgr);
	if (!m_idata.data.size()) {
		if (mode == TILELOADMODE_USEGLOBALSETTING)
			mode = s_globalLoadMode;
		if (mode != TILELOADMODE_DIRECTONLY)
			LoadSubset(mgr);
	}
	return m_idata.data.size() > 0;
}

bool DXT1Tile::LoadPNGtmp()
{
	char path[1024];
	sprintf(path, "%s/tileedit.tmp/%s/%02d/%06d/%06d.png", s_root.c_str(), Layer().c_str(), m_lvl, m_ilat, m_ilng);
	bool ok = pngread_tmp(path, m_idata);
	if (ok && (m_idata.width != TileSize() || m_idata.height != TileSize()))
		ok = false;
	return ok;
}

void DXT1Tile::LoadSubset(const ZTreeMgr *mgr)
{
	if (m_sublvl > 1) {
		lat_subrange.first /= 2;
		lat_subrange.second /= 2;
		lng_subrange.first /= 2;
		lng_subrange.second /= 2;
		if (m_subilat & 1) {
			lat_subrange.first += 256;
			lat_subrange.second += 256;
		}
		if (m_subilng & 1) {
			lng_subrange.first += 256;
			lng_subrange.second += 256;
		}
		m_sublvl -= 1;
		m_subilat /= 2;
		m_subilng /= 2;

		LoadData(m_idata, m_sublvl, m_subilat, m_subilng, mgr);
		if (m_idata.data.size() == 0) {
			LoadSubset(mgr);
		}
		else {
			m_idata = m_idata.SubImage(lng_subrange, lat_subrange);
		}
	}
}

void DXT1Tile::LoadData(Image &im, int lvl, int ilat, int ilng, const ZTreeMgr *mgr)
{
	if (s_openMode & 0x1) { // try cache
		char path[1024];
		sprintf(path, "%s/%s/%02d/%06d/%06d.dds", s_root.c_str(), Layer().c_str(), lvl, ilat, ilng);
		im = ddsread(path);
	}
	if (im.data.size() == 0 && s_openMode & 0x2 && mgr) { // try archive
		BYTE *buf;
		DWORD ndata = mgr->ReadData(lvl, ilat, ilng, &buf);
		if (ndata) {
			im = ddsscan(buf, ndata);
			mgr->ReleaseData(buf);
		}
	}
}

TileBlock *DXT1Tile::ProlongToChildren() const
{
	int i, j, ii, jj;
	int lvl = m_lvl + 1;
	int ilat0 = m_ilat * 2;
	int ilat1 = ilat0 + 2;
	int ilng0 = m_ilng * 2;
	int ilng1 = ilng0 + 2;
	SurfTileBlock *tblock = new SurfTileBlock(lvl, ilat0, ilat1, ilng0, ilng1);
	Image &idata = tblock->getData();
	for (i = 0; i < m_idata.height; i++) {
		for (j = 0; j < m_idata.width; j++) {
			DWORD v = m_idata.data[i*m_idata.width + j];
			for (ii = 0; ii < 2; ii++)
				for (jj = 0; jj < 2; jj++)
					idata.data[(i * 2 + ii)*idata.width + (j * 2 + jj)] = v;
		}
	}
	tblock->syncTiles();
	for (int ilat = ilat0; ilat < ilat1; ilat++) {
		for (int ilng = ilng0; ilng < ilng1; ilng++) {
			Tile *tile = tblock->_getTile(ilat, ilng);
			tile->setSubLevel(m_sublvl);
			tile->setSubiLat(m_subilat);
			tile->setSubiLng(m_subilng);
		}
	}
	return tblock;
}



const ZTreeMgr *SurfTile::s_treeMgr = 0;

SurfTile::SurfTile(int lvl, int ilat, int ilng)
    : DXT1Tile(lvl, ilat, ilng)
{
}

SurfTile *SurfTile::Load(int lvl, int ilat, int ilng, TileLoadMode mode)
{
    SurfTile *stile = new SurfTile(lvl, ilat, ilng);
	if (!stile->LoadPNGtmp() && !stile->LoadDXT1(s_treeMgr, mode)) {
		delete stile;
		return 0;
	}
	return stile;
}

SurfTile *SurfTile::InterpolateFromAncestor(int lvl, int ilat, int ilng)
{
	SurfTile *stile = new SurfTile(lvl, ilat, ilng);
	if (!stile->InterpolateFromAncestor()) {
		delete stile;
		stile = 0;
	}
	return stile;
}

void SurfTile::Save()
{
	SavePNGtmp();
	SaveDXT1();
}

bool SurfTile::InterpolateFromAncestor()
{
	if (m_lvl <= 4) return false;
	int parent_lvl = m_lvl - 1;
	int parent_ilat = m_ilat / 2;
	int parent_ilng = m_ilng / 2;
	SurfTile parent(parent_lvl, parent_ilat, parent_ilng);

	if (!parent.LoadDXT1(s_treeMgr, TILELOADMODE_DIRECTONLY))
		if (!parent.InterpolateFromAncestor())
			return false;

	TileBlock *tblock = parent.ProlongToChildren();
	bool ok = tblock->copyTile(m_ilat, m_ilng, this);
	delete tblock;
	return ok;
}

bool SurfTile::mapToAncestors(int minlvl) const
{
	if (m_lvl <= 4 || m_lvl <= minlvl)
		return false;

	int lvl = m_lvl - 1;
	int ilat = m_ilat / 2;
	int ilng = m_ilng / 2;

	SurfTile *stile = SurfTile::Load(lvl, ilat, ilng, TILELOADMODE_ANCESTORSUBSECTION);
	if (!stile)
		return false;

	if (stile->m_sublvl != lvl) {
		stile->InterpolateFromAncestor();
	}

	Image &idata = stile->getData();
	const int szh = TILE_SURFSTRIDE / 2;
	int xofs = (m_ilng & 1 ? szh : 0);
	int yofs = (m_ilat & 1 ? szh : 0);
	bool isModified = false;

	for (int y = 0; y < szh; y++) {
		for (int x = 0; x < szh; x++) {
			DWORD p1 = m_idata.data[x * 2 + y * 2 * TILE_SURFSTRIDE];
			DWORD p2 = m_idata.data[x * 2 + 1 + y * 2 * TILE_SURFSTRIDE];
			DWORD p3 = m_idata.data[x * 2 + (y * 2 + 1) * TILE_SURFSTRIDE];
			DWORD p4 = m_idata.data[x * 2 + 1 + (y * 2 + 1) * TILE_SURFSTRIDE];

			DWORD c1 = ((DWORD)(p1 & 0xff) + (DWORD)(p2 & 0xff) + (DWORD)(p3 & 0xff) + (DWORD)(p4 & 0xff)) >> 2;
			DWORD c2 = ((DWORD)((p1 >> 8) & 0xff) + (DWORD)((p2 >> 8) & 0xff) + (DWORD)((p3 >> 8) & 0xff) + (DWORD)((p4 >> 8) & 0xff)) >> 2;
			DWORD c3 = ((DWORD)((p1 >> 16) & 0xff) + (DWORD)((p2 >> 16) & 0xff) + (DWORD)((p3 >> 16) & 0xff) + (DWORD)((p4 >> 16) & 0xff)) >> 2;

			DWORD v = 0xff000000 | c1 | (c2 << 8) | (c3 << 16);
			if (v != idata.data[xofs + x + (yofs + y) * TILE_SURFSTRIDE]) {
				idata.data[xofs + x + (yofs + y)*TILE_SURFSTRIDE] = v;
				isModified = true;
			}
		}
	}

	if (isModified) {
		stile->Save();
		stile->mapToAncestors(minlvl);
	}
	delete stile;
	return isModified;
}


void SurfTile::setTreeMgr(const ZTreeMgr *treeMgr)
{
	s_treeMgr = treeMgr;
}


const ZTreeMgr *MaskTile::s_treeMgr = 0;

MaskTile::MaskTile(int lvl, int ilat, int ilng)
	: DXT1Tile(lvl, ilat, ilng)
{
}

MaskTile *MaskTile::Load(int lvl, int ilat, int ilng)
{
	MaskTile *mtile = new MaskTile(lvl, ilat, ilng);
	mtile->LoadDXT1(s_treeMgr);
	if (!mtile->m_idata.data.size()) {
		delete mtile;
		return 0;
	}
	return mtile;
}

void MaskTile::setTreeMgr(const ZTreeMgr *treeMgr)
{
	s_treeMgr = treeMgr;
}
