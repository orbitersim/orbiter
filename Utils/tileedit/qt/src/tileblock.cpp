#include "tileblock.h"
#include "elv_io.h"
#include <algorithm>
#define _USE_MATH_DEFINES
#include <math.h>

TileBlock::TileBlock(int lvl, int ilat0, int ilat1, int ilng0, int ilng1)
{
	m_lvl = lvl;
	m_ilat0 = ilat0; m_ilat1 = ilat1;
	m_ilng0 = ilng0; m_ilng1 = ilng1;

	m_nblocklat = m_ilat1 - m_ilat0;
	m_nblocklng = m_ilng1 - m_ilng0;

	m_tile.resize(m_nblocklat * m_nblocklng);
	for (int i = 0; i < m_tile.size(); i++)
		m_tile[i] = 0;
}

TileBlock::TileBlock(const TileBlock &tileblock)
{
	m_lvl = tileblock.m_lvl;
	m_ilat0 = tileblock.m_ilat0;  m_ilat1 = tileblock.m_ilat1;
	m_ilng0 = tileblock.m_ilng0;  m_ilng1 = tileblock.m_ilng1;

	m_nblocklat = tileblock.m_nblocklat;
	m_nblocklng = tileblock.m_nblocklng;

	m_tile.resize(m_nblocklat * m_nblocklng);
	for (int i = 0; i < m_tile.size(); i++)
		m_tile[i] = 0; // the actual assignment must be done by derived classes
}

TileBlock::~TileBlock()
{
	for (int idx = 0; idx < m_tile.size(); idx++)
		if (m_tile[idx])
			delete m_tile[idx];
}

int TileBlock::minSubLevel() const
{
	int subLevel = m_lvl;
	for (int idx = 0; idx < m_tile.size(); idx++) {
		if (m_tile[idx] && m_tile[idx]->subLevel() < subLevel)
			subLevel = m_tile[idx]->subLevel();
	}
	return subLevel;
}

const Tile *TileBlock::getTile(int ilat, int ilng) const
{
	if (ilat < m_ilat0 || ilat >= m_ilat1) return 0;
	if (ilng < m_ilng0 || ilng >= m_ilng1) return 0;

	int idx = (ilat - m_ilat0) * m_nblocklng + (ilng - m_ilng0);
	return m_tile[idx];
}

Tile *TileBlock::_getTile(int ilat, int ilng)
{
	if (ilat < m_ilat0 || ilat >= m_ilat1) return 0;
	if (ilng < m_ilng0 || ilng >= m_ilng1) return 0;

	int idx = (ilat - m_ilat0) * m_nblocklng + (ilng - m_ilng0);
	return m_tile[idx];
}

const Tile *TileBlock::getTile(int idx) const
{
	if (idx < 0 || idx >= m_tile.size()) return 0;
	return m_tile[idx];
}

void TileBlock::syncTiles()
{
	for (int ilat = m_ilat0; ilat < m_ilat1; ilat++)
		for (int ilng = m_ilng0; ilng < m_ilng1; ilng++)
			syncTile(ilat, ilng);
}

bool TileBlock::hasAncestorData() const
{
	for (int i = 0; i < m_tile.size(); i++)
		if (m_tile[i]->Level() != m_tile[i]->subLevel())
			return true;
	return false;
}

bool TileBlock::mapToAncestors(int minlvl) const
{
	bool isModified = false;
	for (int ilat = m_ilat0; ilat < m_ilat1; ilat++) {
		for (int ilng = m_ilng0; ilng < m_ilng1; ilng++) {
			const Tile *tile = getTile(ilat, ilng);
			isModified = tile->mapToAncestors(minlvl) || isModified;
		}
	}
	return isModified;
}


DXT1TileBlock::DXT1TileBlock(int lvl, int ilat0, int ilat1, int ilng0, int ilng1)
	: TileBlock(lvl, ilat0, ilat1, ilng0, ilng1)
{
	m_idata.width = (ilng1 - ilng0) * TILE_SURFSTRIDE;
	m_idata.height = (ilat1 - ilat0) * TILE_SURFSTRIDE;
	m_idata.data.resize(m_idata.width * m_idata.height);
}


SurfTileBlock::SurfTileBlock(int lvl, int ilat0, int ilat1, int ilng0, int ilng1)
	: DXT1TileBlock(lvl, ilat0, ilat1, ilng0, ilng1)
{
}

Tile *SurfTileBlock::copyTile(int ilat, int ilng) const
{
	if (ilat < m_ilat0 || ilat >= m_ilat1) return 0;
	if (ilng < m_ilng0 || ilng >= m_ilng1) return 0;

	int idx = (ilat - m_ilat0) * m_nblocklng + (ilng - m_ilng0);

	if (!m_tile[idx]) return 0;

	SurfTile *stile = static_cast<SurfTile*>(m_tile[idx]);

	return new SurfTile(*stile);
}

bool SurfTileBlock::copyTile(int ilat, int ilng, Tile *tile) const
{
	if (ilat < m_ilat0 || ilat >= m_ilat1) return false;
	if (ilng < m_ilng0 || ilng >= m_ilng1) return false;

	int idx = (ilat - m_ilat0) * m_nblocklng + (ilng - m_ilng0);
	if (!m_tile[idx]) return false;

	SurfTile *stile = static_cast<SurfTile*>(tile);
	if (!stile) return false;

	stile->set(m_tile[idx]);
	return true;
}

void SurfTileBlock::syncTile(int ilat, int ilng)
{
	int tilesize = (m_lvl == 1 ? 128 : m_lvl == 2 ? 256 : TILE_SURFSTRIDE);

	if (ilat < m_ilat0 || ilat >= m_ilat1) return;
	if (ilng < m_ilng0 || ilng >= m_ilng1) return;

	int nlat = nLat();
	int nlng = nLng();

	SurfTile *stile = (SurfTile*)_getTile(ilat, ilng);
	if (!stile) {
		int ilng_norm = iLng_norm(ilng);
		stile = new SurfTile(m_lvl, ilat, ilng_norm);
		int idx = (ilat - m_ilat0) * m_nblocklng + (ilng - m_ilng0);
		m_tile[idx] = stile;
	}
	Image &idata = stile->getData();
	if (idata.width != tilesize || idata.height != tilesize) {
		idata.width = idata.height = tilesize;
		idata.data.resize(idata.width * idata.height);
	}

	int xblock = ilng - m_ilng0;
	int yblock = ilat - m_ilat0;

	int block_x0 = xblock * tilesize;
	int block_y0 = yblock * tilesize;

	for (int y = 0; y < tilesize; y++) {
		for (int x = 0; x < tilesize; x++) {
			stile->getData().data[y*tilesize + x] =
				m_idata.data[(block_y0 + y) * m_idata.width + (block_x0 + x)];
		}
	}
}

SurfTileBlock *SurfTileBlock::Load(int lvl, int ilat0, int ilat1, int ilng0, int ilng1)
{
	int tilesize = (lvl == 1 ? 128 : lvl == 2 ? 256 : TILE_SURFSTRIDE);

	SurfTileBlock *stileblock = new SurfTileBlock(lvl, ilat0, ilat1, ilng0, ilng1);

	stileblock->m_idata.width = tilesize*stileblock->m_nblocklng;
	stileblock->m_idata.height = tilesize*stileblock->m_nblocklat;
	stileblock->m_idata.data.resize(stileblock->m_idata.width * stileblock->m_idata.height);

	for (int ilat = ilat0; ilat < ilat1; ilat++) {
		for (int ilng = ilng0; ilng < ilng1; ilng++) {
			int idx = (ilat - ilat0)*stileblock->m_nblocklng + (ilng - ilng0);
			SurfTile *stile = SurfTile::Load(lvl, ilat, ilng, TILELOADMODE_ANCESTORSUBSECTION);
			if (!stile) {
				delete stileblock;
				return 0;
			}
			stileblock->m_tile[idx] = stile;
			const Image &im = stile->getData();
			int yrep = tilesize / im.height;
			int xrep = tilesize / im.width;
			int yofs = (ilat - ilat0) * tilesize;
			int xofs = (ilng - ilng0) * tilesize;

			for (int i = 0; i < im.height; i++) {
				for (int ii = 0; ii < yrep; ii++) {
					for (int j = 0; j < im.width; j++) {
						for (int jj = 0; jj < xrep; jj++) {
							DWORD v = im.data[i*im.width + j];
							int idx = yofs * stileblock->m_idata.width + xofs + j*xrep + jj;
							stileblock->m_idata.data[idx] = v;
						}
					}
					yofs++;
				}
			}
		}
	}
	return stileblock;
}


MaskTileBlock::MaskTileBlock(int lvl, int ilat0, int ilat1, int ilng0, int ilng1)
	: DXT1TileBlock(lvl, ilat0, ilat1, ilng0, ilng1)
{
}

Tile *MaskTileBlock::copyTile(int ilat, int ilng) const
{
	if (ilat < m_ilat0 || ilat >= m_ilat1) return 0;
	if (ilng < m_ilng0 || ilng >= m_ilng1) return 0;

	int idx = (ilat - m_ilat0) * m_nblocklng + (ilng - m_ilng0);

	if (!m_tile[idx]) return 0;

	MaskTile *mtile = static_cast<MaskTile*>(m_tile[idx]);

	return new MaskTile(*mtile);
}

bool MaskTileBlock::copyTile(int ilat, int ilng, Tile *tile) const
{
	if (ilat < m_ilat0 || ilat >= m_ilat1) return false;
	if (ilng < m_ilng0 || ilng >= m_ilng1) return false;

	int idx = (ilat - m_ilat0) * m_nblocklng + (ilng - m_ilng0);
	if (!m_tile[idx]) return false;

	MaskTile *mtile = static_cast<MaskTile*>(tile);
	if (!mtile) return false;

	mtile->set(m_tile[idx]);
	return true;
}

void MaskTileBlock::ExtractImage(Image &img, TileMode mode, int exmin, int exmax, int eymin, int eymax) const
{
	img = m_idata;
	if (mode == TILEMODE_WATERMASK)
		for (int i = 0; i < img.data.size(); i++)
			img.data[i] |= 0x00FFFFFF;
	else
		for (int i = 0; i < img.data.size(); i++)
			img.data[i] |= 0xFF000000;
}

MaskTileBlock *MaskTileBlock::Load(int lvl, int ilat0, int ilat1, int ilng0, int ilng1)
{
	const int tilesize = 512;

	MaskTileBlock *mtileblock = new MaskTileBlock(lvl, ilat0, ilat1, ilng0, ilng1);

	mtileblock->m_idata.width = tilesize*mtileblock->m_nblocklng;
	mtileblock->m_idata.height = tilesize*mtileblock->m_nblocklat;
	mtileblock->m_idata.data.resize(mtileblock->m_idata.width * mtileblock->m_idata.height);

	for (int ilat = ilat0; ilat < ilat1; ilat++) {
		for (int ilng = ilng0; ilng < ilng1; ilng++) {
			int idx = (ilat - ilat0)*mtileblock->m_nblocklng + (ilng - ilng0);
			MaskTile *mtile = MaskTile::Load(lvl, ilat, ilng);
			if (!mtile) {
				delete mtileblock;
				return 0;
			}
			mtileblock->m_tile[idx] = mtile;
			const Image &im = mtile->getData();
			int yrep = tilesize / im.height;
			int xrep = tilesize / im.width;
			int yofs = (ilat - ilat0) * tilesize;
			int xofs = (ilng - ilng0) * tilesize;

			for (int i = 0; i < im.height; i++) {
				for (int ii = 0; ii < yrep; ii++) {
					for (int j = 0; j < im.width; j++) {
						for (int jj = 0; jj < xrep; jj++) {
							DWORD v = im.data[i*im.width + j];
							int idx = yofs * mtileblock->m_idata.width + xofs + j*xrep + jj;
							mtileblock->m_idata.data[idx] = v;
						}
					}
					yofs++;
				}
			}
		}
	}
	return mtileblock;
}


const ElevDisplayParam *ElevTileBlock::s_elevDisplayParam = 0;

ElevTileBlock::ElevTileBlock(int lvl, int ilat0, int ilat1, int ilng0, int ilng1)
	: TileBlock(lvl, ilat0, ilat1, ilng0, ilng1)
{
	m_edata.width = (ilng1 - ilng0) * TILE_FILERES + 3;
	m_edata.height = (ilat1 - ilat0) * TILE_FILERES + 3;
	m_edata.data.resize(m_edata.width * m_edata.height);

	m_edataBase.width = (ilng1 - ilng0) * TILE_FILERES + 3;
	m_edataBase.height = (ilat1 - ilat0) * TILE_FILERES + 3;
	m_edataBase.data.resize(m_edataBase.width * m_edataBase.height);

	m_isModified = false;
}

ElevTileBlock::ElevTileBlock(const ElevTileBlock &etileblock)
	: TileBlock(etileblock)
{
	for (int ilat = m_ilat0; ilat < m_ilat1; ilat++)
		for (int ilng = m_ilng0; ilng < m_ilng1; ilng++) {
			int idx = (ilat - m_ilat0)*m_nblocklng + (ilng - m_ilng0);
			m_tile[idx] = etileblock.copyTile(ilat, ilng);
		}

	m_edata = etileblock.m_edata;
	m_edataBase = etileblock.m_edataBase;
	m_isModified = etileblock.m_isModified;
}

ElevTileBlock *ElevTileBlock::Load(int lvl, int ilat0, int ilat1, int ilng0, int ilng1)
{
	ElevTileBlock *tileblock = new ElevTileBlock(lvl, ilat0, ilat1, ilng0, ilng1);
	int nlat = tileblock->nLat();
	int nlng = tileblock->nLng();
	for (int ilat = ilat0; ilat < ilat1; ilat++) {
		if (ilat < 0 || ilat >= nlat) continue;
		for (int ilng = ilng0; ilng < ilng1; ilng++) {
			int idx = (ilat - ilat0)*tileblock->m_nblocklng + (ilng - ilng0);
			int ilngn = ilng;
			while (ilngn < 0) ilngn += nlng;
			while (ilngn >= nlng) ilngn -= nlng;
			ElevTile *tile = ElevTile::Load(lvl, ilat, ilngn);
			if (!tile) {
				delete tileblock;
				return 0;
			}
			if (tile->m_edata.width < TILE_ELEVSTRIDE)
				tile->InterpolateFromAncestor();
			tileblock->setTile(ilat, ilng, tile);
			tileblock->m_tile[idx] = tile;
		}
	}
	tileblock->m_isModified = false;

	return tileblock;
}

void ElevTileBlock::setElevDisplayParam(const ElevDisplayParam *elevDisplayParam)
{
	s_elevDisplayParam = elevDisplayParam;
}

Tile *ElevTileBlock::copyTile(int ilat, int ilng) const
{
	if (ilat < m_ilat0 || ilat >= m_ilat1) return 0;
	if (ilng < m_ilng0 || ilng >= m_ilng1) return 0;

	int idx = (ilat - m_ilat0) * m_nblocklng + (ilng - m_ilng0);

	if (!m_tile[idx]) return 0;

	ElevTile *etile = static_cast<ElevTile*>(m_tile[idx]);

	return new ElevTile(*etile);
}

bool ElevTileBlock::copyTile(int ilat, int ilng, Tile *tile) const
{
	if (ilat < m_ilat0 || ilat >= m_ilat1) return false;
	if (ilng < m_ilng0 || ilng >= m_ilng1) return false;

	int idx = (ilat - m_ilat0) * m_nblocklng + (ilng - m_ilng0);
	if (!m_tile[idx]) return false;
	
	ElevTile *etile = static_cast<ElevTile*>(tile);
	if (!etile) return false;

	etile->set(m_tile[idx]);
	return true;
}

bool ElevTileBlock::setTile(int ilat, int ilng, const Tile *tile)
{
	if (ilat < m_ilat0 || ilat >= m_ilat1) return false;
	if (ilng < m_ilng0 || ilng >= m_ilng1) return false;

	const ElevTile *etile = static_cast<const ElevTile*>(tile);

	int nlat = nLat();
	int nlng = nLng();

	int xblock = ilng - m_ilng0;
	int yblock = m_ilat1 - 1 - ilat;

	int block_x0 = xblock * TILE_FILERES;
	int block_y0 = yblock * TILE_FILERES;

	int x0 = (xblock == 0 ? 0 : 1);
	int x1 = (xblock == m_ilng1 - m_ilng0 - 1 ? TILE_ELEVSTRIDE : TILE_ELEVSTRIDE - 1);
	int y0 = (yblock == 0 || ilat == nlat - 1 ? 0 : 1);
	int y1 = (yblock == m_ilat1 - m_ilat0 - 1 || ilat == 0 ? TILE_ELEVSTRIDE : TILE_ELEVSTRIDE - 1);

	for (int y = y0; y < y1; y++) {
		for (int x = x0; x < x1; x++) {
			m_edata.data[(block_y0 + y) * m_edata.width + (block_x0 + x)] =
				etile->getData().data[y*TILE_ELEVSTRIDE + x];
			m_edataBase.data[(block_y0 + y) * m_edataBase.width + (block_x0 + x)] =
				etile->getBaseData().data[y*TILE_ELEVSTRIDE + x];
		}
	}
	dataChanged();
	return true;
}

void ElevTileBlock::Save()
{
	if (m_isModified) {
		for (int ilat = m_ilat0; ilat < m_ilat1; ilat++)
			for (int ilng = m_ilng0; ilng < m_ilng1; ilng++) {
				ElevTile *etile = (ElevTile*)_getTile(ilat, ilng);
				etile->Save();
			}
		m_isModified = false;
	}
}

void ElevTileBlock::SaveMod()
{
	if (m_isModified) {
		for (int ilat = m_ilat0; ilat < m_ilat1; ilat++)
			for (int ilng = m_ilng0; ilng < m_ilng1; ilng++) {
				ElevTile *etile = (ElevTile*)_getTile(ilat, ilng);
				etile->SaveMod();
			}
		m_isModified = false;
	}
}

void ElevTileBlock::ExportPNG(const std::string &fname, double vmin, double vmax)
{
	const double DEG = 180.0 / M_PI;

	double scale, offset;
	double latmax = (1.0 - (double)m_ilat0 / (double)nLat()) * M_PI - 0.5*M_PI;
	double latmin = (1.0 - (double)m_ilat1 / (double)nLat()) * M_PI - 0.5*M_PI;
	double lngmin = (double)m_ilng0 / (double)nLng() * 2.0*M_PI - M_PI;
	double lngmax = (double)m_ilng1 / (double)nLng() * 2.0*M_PI - M_PI;
	RescanLimits();
	elvwrite_png(fname.c_str(), m_edata, vmin, vmax);

	// write out metadata into a separate file
	char fname_meta[1024];
	strcpy(fname_meta, fname.c_str());
	strcat(fname_meta, ".hdr");
	FILE *f = fopen(fname_meta, "wt");
	if (f) {
		fprintf(f, "vmin=%lf vmax=%lf scale=%lf offset=%lf type=%d padding=1x1 colormap=0 smin=0 emin=0 smean=0 emean=0 smax=0 emax=0 latmin=%+0.10lf latmax=%+0.10lf lngmin=%+0.10lf lngmax=%+0.10lf\n",
			vmin/m_edata.dres, vmax/m_edata.dres, m_edata.dres, 0.0, -16, latmin*DEG, latmax*DEG, lngmin*DEG, lngmax*DEG);
		fprintf(f, "lvl=%d ilat0=%d ilat1=%d ilng0=%d ilng1=%d\n",
			m_lvl, m_ilat0, m_ilat1, m_ilng0, m_ilng1);

		bool writtenMissing = false;
		for (int ilat = m_ilat0; ilat < m_ilat1; ilat++)
			for (int ilng = m_ilng0; ilng < m_ilng1; ilng++) {
				const Tile *tile = getTile(ilat, ilng);
				if (tile->Level() != tile->subLevel()) {
					if (!writtenMissing)
						fprintf(f, "missing");
					fprintf(f, " %d/%d", ilat, ilng);
					writtenMissing = true;
				}
			}
		if (writtenMissing)
			fprintf(f, "\n");
		fclose(f);
	}

}

void ElevTileBlock::syncTile(int ilat, int ilng)
{
	if (ilat < m_ilat0 || ilat >= m_ilat1) return;
	if (ilng < m_ilng0 || ilng >= m_ilng1) return;

	int nlat = nLat();
	int nlng = nLng();

	ElevTile *etile = (ElevTile*)_getTile(ilat, ilng);
	if (!etile) {
		int ilng_norm = iLng_norm(ilng);
		etile = new ElevTile(m_lvl, ilat, ilng_norm);
		int idx = (ilat - m_ilat0) * m_nblocklng + (ilng - m_ilng0);
		m_tile[idx] = etile;
	}
	ElevData &edata = etile->getData();
	if (edata.width < TILE_ELEVSTRIDE || edata.height < TILE_ELEVSTRIDE) {
		edata.width = edata.height = TILE_ELEVSTRIDE;
		edata.data.resize(edata.width * edata.height);
	}
	ElevData &ebdata = etile->getBaseData();
	if (ebdata.width < TILE_ELEVSTRIDE || ebdata.height < TILE_ELEVSTRIDE) {
		ebdata.width = ebdata.height = TILE_ELEVSTRIDE;
		ebdata.data.resize(ebdata.width * ebdata.height);
	}

	int xblock = ilng - m_ilng0;
	int yblock = m_ilat1 - 1 - ilat;

	int block_x0 = xblock * TILE_FILERES;
	int block_y0 = yblock * TILE_FILERES;

	for (int y = 0; y < TILE_ELEVSTRIDE; y++) {
		for (int x = 0; x < TILE_ELEVSTRIDE; x++) {
			double v_old = etile->getData().data[y*TILE_ELEVSTRIDE + x];
			double v_new = m_edata.data[(block_y0 + y) * m_edata.width + (block_x0 + x)];
			if (v_old != v_new) {
				etile->getData().data[y*TILE_ELEVSTRIDE + x] = v_new;
				etile->m_modified = true;
			}
		}
	}
	if (etile->m_modified)
		etile->RescanLimits();
}

void ElevTileBlock::MatchNeighbourTiles()
{
	const double eps = 1e-6;

	int i, xblock, yblock, ilat, ilng, ilng_norm;

	int npadlat = m_nblocklat + 2;
	int npadlng = m_nblocklng + 2;

	int nlat = nLat();
	int nlng = nLng();

	std::vector<ElevTile*> tileGrid(npadlat * npadlng);
	for (i = 0; i < tileGrid.size(); i++)
		tileGrid[i] = 0;

	// load the tile neighbourhood;
	ElevTileBlock etilepad(m_lvl, m_ilat0 - 1, m_ilat1 + 1, m_ilng0 - 1, m_ilng1 + 1);

	for (yblock = 0; yblock < npadlat; yblock++) {
		ilat = m_ilat1 - yblock;
		if (ilat < 0 || ilat >= nlat)
			continue;
		for (xblock = 0; xblock < npadlng; xblock++) {
			ilng = m_ilng0 + xblock - 1;
			if (ilat >= m_ilat0 && ilat < m_ilat1 && ilng >= m_ilng0 && ilng < m_ilng1)
				continue;
			ilng_norm = (ilng < 0 ? ilng + nlng : ilng >= nlng ? ilng - nlng : ilng);
			ElevTile *etile = ElevTile::Load(m_lvl, ilat, ilng_norm);
			if (!etile)
				continue;
			tileGrid[xblock + yblock * npadlng] = etile;
			etilepad.setTile(ilat, ilng, etile);
		}
	}

	// place the modified central tiles - requires tiles to have been synced
	for (ilat = m_ilat0; ilat < m_ilat1; ilat++)
		for (ilng = m_ilng0; ilng < m_ilng1; ilng++)
			etilepad.setTile(ilat, ilng, getTile(ilat, ilng));

	etilepad.syncTiles();

	// check for modifications in the neighbours
	for (yblock = 0; yblock < npadlat; yblock++) {
		for (xblock = 0; xblock < npadlng; xblock++) {
			int idx = xblock + yblock * npadlng;
			if (!tileGrid[idx])
				continue;
			ElevData &edata_old = tileGrid[idx]->getData();
			ilat = m_ilat1 - yblock;
			ilng = m_ilng0 + xblock - 1;
			ElevTile *etile = (ElevTile*)etilepad.getTile(ilat, ilng);  //tileGrid[xblock + yblock*npadlng];
			if (etile) {
				ElevData &edata_new = etile->getData();
				ilng_norm = (ilng < 0 ? ilng + nlng : ilng >= nlng ? ilng - nlng : ilng);
				for (i = 0; i < edata_new.data.size(); i++) {
					if (fabs(edata_new.data[i] - edata_old.data[i]) > eps) {
						etile->dataChanged();
						etile->SaveMod();
						break;
					}
				}
				delete tileGrid[idx];
			}
		}
	}
}

void ElevTileBlock::setWaterMask(const MaskTileBlock *mtileblock)
{
	int w = (m_edata.width - 2) * 2 - 2;
	int h = (m_edata.height - 2) * 2 - 2;

	const Image &mask = mtileblock->getData();
	if (mask.width == w && mask.height == h) {
		m_waterMask.resize(w*h);
		for (int i = 0; i < h; i++) {
			for (int j = 0; j < w; j++) {
				m_waterMask[i*w + j] = ((mask.data[i*w + j] & 0xFF000000) == 0);
			}
		}
	}
}

double ElevTileBlock::nodeElevation(int ndx, int ndy) const
{
	int idx = (ndy + 1)*m_edata.width + (ndx + 1);
	return m_edata.data[idx];
}

double ElevTileBlock::nodeModElevation(int ndx, int ndy) const
{
	int idx = (ndy + 1)*m_edata.width + (ndx + 1);
	double v = m_edata.data[idx];
	double v0 = m_edataBase.data[idx];
	return (v != v0 ? v : DBL_MAX);
}

void ElevTileBlock::dataChanged(int exmin, int exmax, int eymin, int eymax)
{
	m_isModified = true;
	auto minmax = std::minmax_element(m_edata.data.begin(), m_edata.data.end());
	m_edata.dmin = *minmax.first;
	m_edata.dmax = *minmax.second;
}

void ElevTileBlock::RescanLimits()
{
	m_edata.RescanLimits();
}

void ElevTileBlock::ExtractImage(Image &img, TileMode mode, int exmin, int exmax, int eymin, int eymax) const
{
	if (mode == TILEMODE_ELEVMOD) {
		ExtractModImage(img, mode, exmin, exmax, eymin, eymax);
		return;
	}

	double dmin, dmax;

	img.width = (m_edata.width - 2) * 2 - 2;
	img.height = (m_edata.height - 2) * 2 - 2;
	img.data.resize(img.width * img.height);

	if (s_elevDisplayParam->autoRange) {
		dmin = m_edata.dmin;
		dmax = m_edata.dmax;
	}
	else {
		dmin = s_elevDisplayParam->rangeMin;
		dmax = s_elevDisplayParam->rangeMax;
	}
	double dscale = (dmax > dmin ? 256.0 / (dmax - dmin) : 1.0);

	int imin = (exmin < 0 ? 0 : max(0, (exmin - 1) * 2 - 1));
	int imax = (exmax < 0 ? img.width : min((int)img.width, exmax * 2));
	int jmin = (eymax < 0 ? 0 : max(0, (int)img.height - (eymax - 1) * 2));
	int jmax = (eymin < 0 ? img.height : min((int)img.height, (int)img.height - (eymin - 1) * 2 + 1));

	const Cmap &cm = cmap(s_elevDisplayParam->cmName);
	bool useMask = s_elevDisplayParam->useWaterMask && m_waterMask.size();

	for (int j = jmin; j < jmax; j++)
		for (int i = imin; i < imax; i++) {
			int ex = (i + 1) / 2 + 1;
			int ey = (img.height - j) / 2 + 1;
			double d = m_edata.data[ex + ey * m_edata.width];
			int v = max(min((int)((d - dmin) * dscale), 255), 0);
			img.data[i + j * img.width] = (0xff000000 | cm[v]);

			if (useMask && m_waterMask[i + j * img.width])
				img.data[i + j * img.width] = 0xffB9E3FF;
		}
}

void ElevTileBlock::ExtractModImage(Image &img, TileMode mode, int exmin, int exmax, int eymin, int eymax) const
{
	double dmin, dmax;

	img.width = (m_edata.width - 2) * 2 - 2;
	img.height = (m_edata.height - 2) * 2 - 2;
	img.data.resize(img.width * img.height);

	if (s_elevDisplayParam->autoRange) {
		dmin = m_edata.dmin;
		dmax = m_edata.dmax;
	}
	else {
		dmin = s_elevDisplayParam->rangeMin;
		dmax = s_elevDisplayParam->rangeMax;
	}
	double dscale = (dmax > dmin ? 256.0 / (dmax - dmin) : 1.0);

	int imin = (exmin < 0 ? 0 : max(0, (exmin - 1) * 2 - 1));
	int imax = (exmax < 0 ? img.width : min((int)img.width, exmax * 2));
	int jmin = (eymax < 0 ? 0 : max(0, (int)img.height - (eymax - 1) * 2));
	int jmax = (eymin < 0 ? img.height : min((int)img.height, (int)img.height - (eymin - 1) * 2 + 1));

	const Cmap &cm = cmap(s_elevDisplayParam->cmName);
	bool useMask = s_elevDisplayParam->useWaterMask && m_waterMask.size();

	for (int j = jmin; j < jmax; j++)
		for (int i = imin; i < imax; i++) {
			int ex = (i + 1) / 2 + 1;
			int ey = (img.height - j) / 2 + 1;
			double d = m_edata.data[ex + ey * m_edata.width];
			double db = m_edataBase.data[ex + ey * m_edata.width];
			if (d != db) {
				int v = max(min((int)((d - dmin) * dscale), 255), 0);
				img.data[i + j * img.width] = (0xff000000 | cm[v]);
			}
			else {
				bool b = (i / 8 + j / 8) & 1;
				img.data[i + j * img.width] = (b ? 0xff808080 : 0xff909090);
			}
		}
}

#ifdef UNDEF
bool ElevTileBlock::getTile(int ilat, int ilng, Tile *tile) const
{
	ElevTile *etile = static_cast<ElevTile*>(tile);

	int nlat = nLat();
	int nlng = nLng();

	if (ilat < 0 || ilat >= nlat) return false;
	int ilngn = ilng;
	while (ilngn < 0) ilngn += nlng;
	while (ilngn >= nlng) ilngn -= nlng;

	ElevData &edata = etile->getData();

	int xblock = ilng - m_ilng0;
	int yblock = m_ilat1 - 1 - ilat;
	int block_x0 = xblock * TILE_FILERES;
	int block_y0 = yblock * TILE_FILERES;

	for (int y = 0; y < TILE_ELEVSTRIDE; y++) {
		for (int x = 0; x < TILE_ELEVSTRIDE; x++) {
			edata.data[y*TILE_ELEVSTRIDE + x] =
				m_edata.data[(block_y0 + y) * m_edata.width + (block_x0 + x)];
		}
	}

	edata.dmin = *std::min_element(edata.data.begin(), edata.data.end());
	edata.dmax = *std::max_element(edata.data.begin(), edata.data.end());
	etile->dataChanged();

	return true;
}
#endif