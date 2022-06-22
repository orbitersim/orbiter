#include "tileedit.h"
#include "ui_tileedit.h"
#include "tile.h"
#include "elevtile.h"
#include "tileblock.h"
#include "dlgsurfimport.h"
#include "dlgconfig.h"
#include "dlgelevconfig.h"
#include "dlgelevexport.h"
#include "dlgelevimport.h"
#include <random>

#include "QFileDialog"
#include "QResizeEvent"
#include "QMessageBox"
#include "QSettings"

static std::vector<std::pair<int, int> > paintStencil1 = { {0,0} };
static std::vector<std::pair<int, int> > paintStencil2 = { {0,0}, {1,0}, {0,1}, {1,1} };
static std::vector<std::pair<int, int> > paintStencil3 = { {-1,-1}, {0,-1}, {1,-1}, {-1,0}, {0,0}, {1,0}, {-1,1}, {0,1}, {1,1} };
static std::vector<std::pair<int, int> > paintStencil4 = { {0,-1}, {1,-1}, {-1,0}, {0,0}, {1,0}, {2,0}, {-1,1}, {0,1}, {1,1}, {2,1}, {0,2}, {1,2} };
static std::vector<std::pair<int, int> > paintStencil5 = { {-1,-2},{0,-2}, {1,-2}, {-2,-1},{-1,-1}, {0,-1}, {1,-1}, {2,-1}, {-2,0}, {-1,0},{0,0}, {1,0}, {2,0}, {-2,1},{-1,1},{0,1},{1,1},{2,1}, {-1,2}, {0,2},{1,2} };
static std::vector<std::vector<std::pair<int, int>>*> paintStencil = { &paintStencil1, &paintStencil2, &paintStencil3, &paintStencil4, &paintStencil5 };

std::default_random_engine generator;

tileedit::tileedit(QWidget *parent)
    : QMainWindow(parent)
    , ui(new Ui::tileedit)
{
	char path[1024], drive[16], dir[1024], name[1024], ext[1024];
	GetModuleFileNameA(NULL, path, 1024);
	_splitpath(path, drive, dir, name, ext);
	sprintf(path, "%s%s%s.ini", drive, dir, name);
	m_settings = new QSettings(path, QSettings::IniFormat);

	m_elevDisplayParam.cmName = (CmapName)m_settings->value("elevdisp/cmap", CMAP_GREY).toInt();
	m_elevDisplayParam.useWaterMask = m_settings->value("elevdisp/wmask", false).toBool();
	m_elevDisplayParam.autoRange = m_settings->value("elevdisp/autorange", true).toBool();
	m_elevDisplayParam.rangeMin = m_settings->value("elevdisp/rmin", 0.0).toDouble();
	m_elevDisplayParam.rangeMax = m_settings->value("elevdisp/rmax", 1000.0).toDouble();
	m_openMode = m_settings->value("config/openmode", TILESEARCH_CACHE | TILESEARCH_ARCHIVE).toUInt();
	m_globalLoadMode = (TileLoadMode)m_settings->value("config/queryancestor", (int)TILELOADMODE_ANCESTORSUBSECTION).toInt();
	m_blocksize = m_settings->value("config/blocksize", 1).toInt();

	m_sTileBlock = 0;
	m_mTileBlock = 0;
	m_eTileBlock = 0;
	m_eTileBlockRef = 0;

	m_mgrSurf = 0;
	m_mgrMask = 0;
	m_mgrElev = 0;
	m_mgrElevMod = 0;

	Tile::setOpenMode(m_openMode);
	Tile::setGlobalLoadMode(m_globalLoadMode);
	ElevTileBlock::setElevDisplayParam(&m_elevDisplayParam);

	m_mouseDown = false;
	m_rndn = 0;

	m_dlgElevConfig = 0;

    ui->setupUi(this);

    m_panel[0].canvas = ui->widgetCanvas0;
	m_panel[0].colorbar = ui->widgetColourbar0;
	m_panel[0].layerType = ui->comboLayerType0;
	m_panel[0].fileId = ui->labelFileId0;

	m_panel[1].canvas = ui->widgetCanvas1;
	m_panel[1].colorbar = ui->widgetColourbar1;
	m_panel[1].layerType = ui->comboLayerType1;
	m_panel[1].fileId = ui->labelFileId1;

    m_panel[2].canvas = ui->widgetCanvas2;
	m_panel[2].colorbar = ui->widgetColourbar2;
	m_panel[2].layerType = ui->comboLayerType2;
	m_panel[2].fileId = ui->labelFileId2;

    createActions();
    createMenus();

    m_lvl = 1;
    m_ilat = 0;
    m_ilng = 0;

    connect(ui->spinResolution, SIGNAL(valueChanged(int)), this, SLOT(onResolutionChanged(int)));
    connect(ui->spinLatidx, SIGNAL(valueChanged(int)), this, SLOT(onLatidxChanged(int)));
    connect(ui->spinLngidx, SIGNAL(valueChanged(int)), this, SLOT(onLngidxChanged(int)));

	connect(ui->bgrpAction, SIGNAL(buttonClicked(int)), this, SLOT(onActionButtonClicked(int)));
	ui->bgrpAction->setId(ui->btnActionNavigate, 0);
	ui->bgrpAction->setId(ui->btnActionElevEdit, 1);
	connect(ui->bgrpEdit, SIGNAL(buttonClicked(int)), this, SLOT(onEditButtonClicked(int)));
	ui->bgrpEdit->setId(ui->btnElevPaint, 0);
	ui->bgrpEdit->setId(ui->btnElevRandom, 1);
	ui->bgrpEdit->setId(ui->btnElevErase, 2);

	m_actionMode = ACTION_NAVIGATE;
	m_elevEditMode = ELEVEDIT_PAINT;
	setToolOptions();

	ui->widgetElevEditTools->setVisible(false);

	m_settings->beginReadArray("canvas");
	for (int i = 0; i < 3; i++) {
		m_panel[i].canvas->setIdx(i);
		m_settings->setArrayIndex(i);
		m_panel[i].canvas->setTileedit(this);
		int layer = m_settings->value("layer", i).toInt();
		m_panel[i].layerType->setCurrentIndex(layer);
		connect(m_panel[i].canvas, SIGNAL(tileChanged(int, int, int)), this, SLOT(OnTileChangedFromPanel(int, int, int)));
		connect(m_panel[i].canvas, SIGNAL(tileEntered(TileCanvas*)), this, SLOT(OnTileEntered(TileCanvas*)));
		connect(m_panel[i].canvas, SIGNAL(tileLeft(TileCanvas*)), this, SLOT(OnTileLeft(TileCanvas*)));
		connect(m_panel[i].canvas, SIGNAL(mouseMovedInCanvas(int, QMouseEvent*)), this, SLOT(OnMouseMovedInCanvas(int, QMouseEvent*)));
		connect(m_panel[i].canvas, SIGNAL(mousePressedInCanvas(int, QMouseEvent*)), this, SLOT(OnMousePressedInCanvas(int, QMouseEvent*)));
		connect(m_panel[i].canvas, SIGNAL(mouseReleasedInCanvas(int, QMouseEvent*)), this, SLOT(OnMouseReleasedInCanvas(int, QMouseEvent*)));
		m_panel[i].colorbar->setElevDisplayParam(m_elevDisplayParam);
	}
	m_settings->endArray();
    connect(m_panel[0].layerType, SIGNAL(currentIndexChanged(int)), this, SLOT(onLayerType0(int)));
    connect(m_panel[1].layerType, SIGNAL(currentIndexChanged(int)), this, SLOT(onLayerType1(int)));
    connect(m_panel[2].layerType, SIGNAL(currentIndexChanged(int)), this, SLOT(onLayerType2(int)));

    ui->statusBar->addWidget(status = new QLabel());
}

tileedit::~tileedit()
{
    delete ui;
    if (m_sTileBlock)
        delete m_sTileBlock;
	if (m_mTileBlock)
		delete m_mTileBlock;
	if (m_eTileBlock)
		delete m_eTileBlock;

	releaseTreeManagers();

	delete m_settings;
}

void tileedit::createMenus()
{
	QMenu *menu;

    fileMenu = ui->menuBar->addMenu(tr("&File"));
    fileMenu->addAction(openAct);
	fileMenu->addSeparator();
	fileMenu->addAction(actionConfig);
	fileMenu->addSeparator();
	fileMenu->addAction(actionExit);

	menu = ui->menuBar->addMenu(tr("&Surface"));
	menu->addAction(actionSurfImport);

	menu = ui->menuBar->addMenu(tr("&Elevation"));
	menu->addAction(actionElevConfig);
	menu->addSeparator();
	menu->addAction(actionElevExport);
	menu->addAction(actionElevImport);
}

void tileedit::createActions()
{
    openAct = new QAction(tr("&Open"), this);
    connect(openAct, &QAction::triggered, this, &tileedit::openDir);

	actionConfig = new QAction(tr("&Configure"), this);
	connect(actionConfig, &QAction::triggered, this, &tileedit::on_actionConfig_triggered);

	actionExit = new QAction(tr("E&xit"), this);
	connect(actionExit, &QAction::triggered, this, &tileedit::on_actionExit_triggered);

	actionSurfImport = new QAction(tr("&Import from image"), this);
	connect(actionSurfImport, &QAction::triggered, this, &tileedit::onSurfImportImage);

	actionElevConfig = new QAction(tr("&Configure"), this);
	actionElevConfig->setCheckable(true);
	connect(actionElevConfig, &QAction::triggered, this, &tileedit::onElevConfig);

	actionElevExport = new QAction(tr("&Export to image"), this);
	connect(actionElevExport, &QAction::triggered, this, &tileedit::onElevExportImage);

	actionElevImport = new QAction(tr("&Import from image"), this);
	connect(actionElevImport, &QAction::triggered, this, &tileedit::onElevImportImage);
}

void tileedit::elevDisplayParamChanged()
{
	char cbuf[1024];

	if (m_eTileBlock) {
		if (m_elevDisplayParam.autoRange) {
			m_elevDisplayParam.rangeMin = m_eTileBlock->getData().dmin;
			m_elevDisplayParam.rangeMax = m_eTileBlock->getData().dmax;
		}
	}

	for (int i = 0; i < 3; i++) {
		if (m_panel[i].layerType->currentIndex() >= 3) {
			m_panel[i].canvas->updateImage();
		}
		Colorbar *cbar = m_panel[i].colorbar;
		if (cbar) {
			cbar->displayParamChanged();
		}
	}

	m_settings->setValue("elevdisp/cmap", (int)m_elevDisplayParam.cmName);
	m_settings->setValue("elevdisp/wmask", m_elevDisplayParam.useWaterMask);
	m_settings->setValue("elevdisp/autorange", m_elevDisplayParam.autoRange);
	m_settings->setValue("elevdisp/rmin", m_elevDisplayParam.rangeMin);
	m_settings->setValue("elevdisp/rmax", m_elevDisplayParam.rangeMax);
}

void tileedit::setLoadMode(DWORD mode)
{
	if (mode != m_openMode) {
		m_openMode = mode;
		Tile::setOpenMode(m_openMode);
		m_settings->setValue("config/openmode", (uint)m_openMode);

		// reload current tile
		if (Tile::root().size())
			setTile(m_lvl, m_ilat, m_ilng);
	}
}

void tileedit::setAncestorMode(TileLoadMode mode)
{
	if (mode != m_globalLoadMode) {
		m_globalLoadMode = mode;
		Tile::setGlobalLoadMode(m_globalLoadMode);
		m_settings->setValue("config/queryancestor", m_globalLoadMode);

		// reload current tile
		if (Tile::root().size())
			setTile(m_lvl, m_ilat, m_ilng);
	}
}

void tileedit::setBlockSize(int bsize)
{
	if (bsize != m_blocksize) {
		m_blocksize = bsize;

		// reload current tile
		m_ilat = max(0, min(m_ilat, nLat(m_lvl) - bsize));
		m_ilng = max(0, min(m_ilng, nLng(m_lvl) - bsize));
		if (Tile::root().size())
			setTile(m_lvl, m_ilat, m_ilng);

		m_settings->setValue("config/blocksize", m_blocksize);
	}
}

void tileedit::openDir()
{
	QString rootDir;
	if (m_settings->contains("rootdir"))
		rootDir = m_settings->value("rootdir").toString();

    rootDir = QFileDialog::getExistingDirectory(this, tr("Open celestial body"), rootDir);
	if (rootDir.size()) {
		m_settings->setValue("rootdir", rootDir);
		Tile::setRoot(rootDir.toStdString());

		if (m_openMode & TILESEARCH_ARCHIVE)
			setupTreeManagers(rootDir.toStdString());

		setTile(1, 0, 0);
		ensureSquareCanvas(rect().width(), rect().height());

		char cbuf[256];
		sprintf(cbuf, "tileedit [%s]", rootDir.toLatin1().data());
		setWindowTitle(cbuf);
	}
}

void tileedit::on_actionConfig_triggered()
{
	DlgConfig dlg(this);
	dlg.exec();
}

void tileedit::on_actionExit_triggered()
{
	QApplication::quit();
}

void tileedit::onElevConfig()
{
	if (!m_dlgElevConfig) {
		m_dlgElevConfig = new DlgElevConfig(this, m_elevDisplayParam);
		connect(m_dlgElevConfig, SIGNAL(finished(int)), this, SLOT(onElevConfigDestroyed(int)));
		m_dlgElevConfig->show();
		actionElevConfig->setChecked(true);
	}
	else
		onElevConfigDestroyed(0);
}

void tileedit::onElevExportImage()
{
	if (!m_eTileBlock) {
		QMessageBox mbox(QMessageBox::Warning, "tileedit", "No elevation layer loaded.", QMessageBox::Close);
		mbox.exec();
		return;
	}

	DlgElevExport dlg(this);
	dlg.exec();
}

void tileedit::onSurfImportImage()
{
	DlgSurfImport dlg(this);
	if (dlg.exec() == QDialog::Accepted) {
		loadTile(m_lvl, m_ilat, m_ilng); // refresh
	}
}

void tileedit::onElevImportImage()
{
	DlgElevImport dlg(this);
	if (dlg.exec() == QDialog::Accepted) {
		loadTile(m_lvl, m_ilat, m_ilng); // refresh
	}
}

void tileedit::onElevConfigDestroyed(int r)
{
	if (m_dlgElevConfig) {
		delete m_dlgElevConfig;
		m_dlgElevConfig = 0;
		actionElevConfig->setChecked(false);
	}
}

void tileedit::loadTile(int lvl, int ilat, int ilng)
{
	int ilat1 = min(nLat(lvl), ilat + m_blocksize);
	int ilng1 = min(nLng(lvl), ilng + m_blocksize);

    if (m_sTileBlock)
        delete m_sTileBlock;
    m_sTileBlock = SurfTileBlock::Load(lvl, ilat, ilat1, ilng, ilng1);

	if (m_mTileBlock)
		delete m_mTileBlock;
	m_mTileBlock = MaskTileBlock::Load(lvl, ilat, ilat1, ilng, ilng1);

	if (m_eTileBlock)
		delete m_eTileBlock;
	m_eTileBlock = ElevTileBlock::Load(lvl, ilat, ilat1, ilng, ilng1);
	if (m_eTileBlock && m_mTileBlock)
		m_eTileBlock->setWaterMask(m_mTileBlock);

    for (int i = 0; i < 3; i++)
        refreshPanel(i);
}

void tileedit::refreshPanel(int panelIdx)
{
	char cbuf[256];

    switch(m_panel[panelIdx].layerType->currentIndex()) {
    case 0:
        m_panel[panelIdx].canvas->setTileBlock(m_sTileBlock, TILEMODE_SURFACE);
		m_panel[panelIdx].colorbar->setTileMode(TILEMODE_SURFACE);
		break;
	case 1:
		m_panel[panelIdx].canvas->setTileBlock(m_mTileBlock, TILEMODE_WATERMASK);
		m_panel[panelIdx].colorbar->setTileMode(TILEMODE_WATERMASK);
		break;
	case 2:
		m_panel[panelIdx].canvas->setTileBlock(m_mTileBlock, TILEMODE_NIGHTLIGHT);
		m_panel[panelIdx].colorbar->setTileMode(TILEMODE_NIGHTLIGHT);
		break;
	case 3:
		m_panel[panelIdx].canvas->setTileBlock(m_eTileBlock, TILEMODE_ELEVATION);
		m_panel[panelIdx].colorbar->setTileMode(TILEMODE_ELEVATION);
		elevDisplayParamChanged();
		break;
	case 4:
		m_panel[panelIdx].canvas->setTileBlock(m_eTileBlock, TILEMODE_ELEVMOD);
		m_panel[panelIdx].colorbar->setTileMode(TILEMODE_ELEVMOD);
		break;
	default:
        m_panel[panelIdx].canvas->setTileBlock(0, TILEMODE_NONE);
		m_panel[panelIdx].colorbar->setTileMode(TILEMODE_NONE);
		break;
    }
}

void tileedit::onResolutionChanged(int lvl)
{
	while (lvl > m_lvl) {
		m_ilat *= 2;
		m_ilng *= 2;
		m_lvl++;
	}
	while (lvl < m_lvl) {
		m_ilat /= 2;
		m_ilng /= 2;
		m_lvl--;
	}
	setTile(m_lvl, m_ilat, m_ilng);
}

void tileedit::onLatidxChanged(int ilat)
{
	setTile(m_lvl, ilat, m_ilng);
}

void tileedit::onLngidxChanged(int ilng)
{
	setTile(m_lvl, m_ilat, ilng);
}

void tileedit::onActionButtonClicked(int id)
{
	ActionMode newMode = (ActionMode)id;
	if (newMode == m_actionMode)
		return;

	if (newMode == ACTION_ELEVEDIT) {
		if (!m_eTileBlock) {
			ui->btnActionNavigate->setChecked(true);
			return;
		}
		if (m_eTileBlock->minSubLevel() < m_eTileBlock->Level()) {
			QMessageBox mbox(QMessageBox::Warning, "tileedit", "This elevation tile does not exist - the image you see has been synthesized from a subsection of an ancestor.\n\nBefore this tile can be edited it must be created. Do you want to create it now?", QMessageBox::Yes | QMessageBox::No);
			int res = mbox.exec();
			if (res == QMessageBox::Yes) {
				for (int ilat = m_eTileBlock->iLat0(); ilat < m_eTileBlock->iLat1(); ilat++) {
					for (int ilng = m_eTileBlock->iLng0(); ilng < m_eTileBlock->iLng1(); ilng++) {
						if (m_eTileBlock->getTile(ilat, ilng)->subLevel() < m_eTileBlock->Level()) {
							ElevTile *tile = ElevTile::InterpolateFromAncestor(m_lvl, ilat, ilng);
							if (tile) {
								tile->dataChanged();
								tile->Save();
								delete tile;
							}
						}
					}
				}
				loadTile(m_lvl, m_ilat, m_ilng);
			}
			else {
				ui->btnActionNavigate->setChecked(true);
				return; // don't allow editing virtual tiles
			}
		}
	}

	m_actionMode = newMode;
	ui->widgetElevEditTools->setVisible(id == 1);
	ui->gboxToolOptions->setTitle(ModeString());
	setToolOptions();
	for (int i = 0; i < 3; i++)
		m_panel[i].canvas->setGlyphMode((TileCanvas::GlyphMode)id);
}

void tileedit::onEditButtonClicked(int id)
{
	m_elevEditMode = (ElevEditMode)id;
	ui->gboxToolOptions->setTitle(ModeString());
	setToolOptions();
}

void tileedit::onLayerType0(int idx)
{
	m_settings->beginWriteArray("canvas");
	m_settings->setArrayIndex(0);
	m_settings->setValue("layer", idx);
	m_settings->endArray();

    refreshPanel(0);
}

void tileedit::onLayerType1(int idx)
{
	m_settings->beginWriteArray("canvas");
	m_settings->setArrayIndex(1);
	m_settings->setValue("layer", idx);
	m_settings->endArray();

	refreshPanel(1);
}

void tileedit::onLayerType2(int idx)
{
	m_settings->beginWriteArray("canvas");
	m_settings->setArrayIndex(2);
	m_settings->setValue("layer", idx);
	m_settings->endArray();

	refreshPanel(2);
}

void tileedit::resizeEvent(QResizeEvent *event)
{
    int winw = event->size().width();
    int winh = event->size().height();
	ensureSquareCanvas(winw, winh);
}

void tileedit::ensureSquareCanvas(int winw, int winh)
{
    // make canvas areas square
    int w = ui->widgetCanvas0->width();
    int h = ui->widgetCanvas0->height();
    int dw = w-h;

	if (dw) {
		blockSignals(true);
		setFixedHeight(winh + dw);
		blockSignals(false);
	}
}

void tileedit::OnTileChangedFromPanel(int lvl, int ilat, int ilng)
{
	setTile(lvl, ilat, ilng);
}

void tileedit::OnTileEntered(TileCanvas *canvas)
{
	for (int i = 0; i < 3; i++) {
		if (m_actionMode != ACTION_NAVIGATE || m_panel[i].canvas == canvas) {
			m_panel[i].canvas->blockSignals(true);
			m_panel[i].canvas->showOverlay(true);
			m_panel[i].canvas->blockSignals(false);
		}
	}

	int idx = -1;
	const TileBlock *tileblock = canvas->tileBlock();
	if (tileblock) idx = canvas->idx();

	ui->groupTileData->setTitle(idx >= 0 ? m_panel[idx].layerType->currentText() : "Tile");
	ui->labelKey2->setText(idx >= 0 && m_panel[idx].layerType->currentIndex() == 3 ? "Node:" : "Pixel:");

	const char *ValStr[5] = { "Colour:", "Type:", "Colour:", "Elev.:", "Elev.:" };
	ui->labelKey3->setText(idx >= 0 ? ValStr[m_panel[idx].layerType->currentIndex()] : "-");
}

void tileedit::OnTileLeft(TileCanvas *canvas)
{
	for (int i = 0; i < 3; i++) {
		if (m_actionMode != ACTION_NAVIGATE || m_panel[i].canvas == canvas) {
			m_panel[i].canvas->blockSignals(true);
			m_panel[i].canvas->showOverlay(false);
			m_panel[i].canvas->blockSignals(false);
		}
	}
}

void tileedit::OnMouseMovedInCanvas(int canvasIdx, QMouseEvent *event)
{
	TileCanvas *canvas = m_panel[canvasIdx].canvas;
	const TileBlock *tileblock = canvas->tileBlock();
	const Image &img = canvas->getImage();

	int x = event->x();
	int y = event->y();
	int cw = canvas->rect().width();
	int ch = canvas->rect().height();

	if (tileblock && img.data.size()) {
		int iw = img.width;
		int ih = img.height;

		int nlat = (m_lvl < 4 ? 1 : 1 << (m_lvl - 4));
		int nlng = (m_lvl < 4 ? 1 : 1 << (m_lvl - 3));
		double latmax = (1.0 - (double)m_ilat / (double)nlat) * 180.0 - 90.0;
		double latmin = latmax - 180.0 / nlat;
		double lngmin = (double)m_ilng / (double)nlng * 360.0 - 180.0;
		double lngmax = lngmin + 360.0 / nlng;

		double lng = lngmin + ((double)x + 0.5) / (double)cw * (lngmax - lngmin);
		double lat = latmax - ((double)y + 0.5) / (double)ch * (latmax - latmin);

		char cbuf[1024];
		sprintf(cbuf, "Lng=%+0.6lf, Lat=%+0.6lf", lng, lat);
		ui->labelData1->setText(cbuf);

		int mpx = (x*iw) / cw; // pixel coordinates
		int mpy = (y*ih) / ch;
		int mnx = (mpx + 1) / 2;  // node coordinates
		int mny = (ih - mpy) / 2;

		if (m_panel[canvasIdx].layerType->currentIndex() >= 3) { // elevation
			sprintf(cbuf, "x=%d/%d, y=%d/%d", mnx, (iw / 2) + 1, mny, (ih / 2) + 1);
			ui->labelData2->setText(cbuf);
			double elev = ((ElevTileBlock*)tileblock)->nodeElevation(mnx, mny);
			sprintf(cbuf, "%+0.1lfm", elev);
			ui->labelData3->setText(cbuf);
		}
		else {
			sprintf(cbuf, "X=%d/%d, Y=%d/%d", mpx, iw, mpy, ih);
			ui->labelData2->setText(cbuf);
			if (mpx >= 0 && mpx < img.width && mpy >= 0 && mpy < img.height) {
				DWORD col = img.data[mpx + mpy*img.width];
				if (m_panel[canvasIdx].layerType->currentIndex() == 1) {
					ui->labelData3->setText(col & 0xFF000000 ? "Diffuse (Land)" : "Specular (Water)");
				}
				else {
					sprintf(cbuf, "R=%d, G=%d, B=%d", (col >> 0x10) & 0xFF, (col >> 0x08) & 0xFF, col & 0xFF);
					ui->labelData3->setText(cbuf);
				}
			}
		}
		for (int i = 0; i < 3; i++) {
			if (m_panel[i].layerType->currentIndex() >= 3) {
				if (m_eTileBlock) {
					double elev = (m_panel[i].layerType->currentIndex() == 3 ? m_eTileBlock->nodeElevation(mnx, mny) : m_eTileBlock->nodeModElevation(mnx, mny));
					m_panel[i].colorbar->setScalarValue(elev);
				}
			}
			else if (m_panel[i].layerType->currentIndex() == 0 || m_panel[i].layerType->currentIndex() == 2) {
				const Image &img = m_panel[i].canvas->getImage();
				if (mpx >= 0 && mpx < img.width && mpy >= 0 && mpy < img.height) {
					DWORD col = img.data[mpx + mpy*img.width];
					m_panel[i].colorbar->setRGBValue((col >> 0x10) & 0xFF, (col >> 0x08) & 0xFF, col & 0xFF);
				}
			}
			else if (m_panel[i].layerType->currentIndex() == 1) {
				const Image &img = m_panel[i].canvas->getImage();
				if (mpx >= 0 && mpx < img.width && mpy >= 0 && mpy < img.height) {
					bool isWater = (img.data[mpx + mpy*img.width] & 0xFF000000) == 0;
					m_panel[i].colorbar->setScalarValue(isWater ? 0.0 : 1.0);
				}
			}
		}
	}
	else {
		ui->labelData1->setText("-");
		ui->labelData2->setText("-");
	}

	if (m_actionMode == ACTION_ELEVEDIT && m_eTileBlock) {
		if (m_mouseDown)
			editElevation(canvasIdx, event->x(), event->y());
		for (int i = 0; i < 3; i++) {
			int toolrad;
			switch (m_elevEditMode) {
			case ELEVEDIT_PAINT:
				toolrad = ui->spinElevPaintSize->value();
				break;
			case ELEVEDIT_RANDOM:
				toolrad = ui->spinElevRandomSize->value();
				break;
			case ELEVEDIT_ERASE:
				toolrad = ui->spinElevEraseSize->value();
				break;
			}
			double r = (toolrad * 0.5) / (double)(m_eTileBlock->getData().width - 3);
			m_panel[i].canvas->setCrosshair(((double)x + 0.5) / (double)cw, ((double)y + 0.5) / (double)ch, r);
		}
	}
}

std::pair<int, int> tileedit::ElevNodeFromPixCoord(int canvasIdx, int x, int y)
{
	// Should be moved into TileCanvas class

	std::pair<int, int> node;
	TileCanvas *canvas = m_panel[canvasIdx].canvas;
	const Image &img = canvas->getImage();

	if (img.data.size()) {
		int iw = img.width;
		int ih = img.height;
		int cw = canvas->rect().width();
		int ch = canvas->rect().height();
		int mx = (x*iw) / cw;
		int my = (y*ih) / ch;
		mx = (mx + 1) / 2;
		my = (ih - my) / 2;
		node.first = mx;
		node.second = my;
	}
	return node;
}

void tileedit::OnMousePressedInCanvas(int canvasIdx, QMouseEvent *event)
{
	if (m_actionMode == ACTION_ELEVEDIT && m_eTileBlock) {
		if (m_eTileBlockRef)
			delete m_eTileBlockRef;
		m_eTileBlockRef = new ElevTileBlock(*m_eTileBlock);
		if (m_elevEditMode == ELEVEDIT_RANDOM) {
			double mean = (double)ui->spinElevRandomValue->value();
			double std = ui->dspinElevRandomStd->value();
			m_rndn = new std::normal_distribution<double>(mean, std);
		}
		editElevation(canvasIdx, event->x(), event->y());
	}
	m_mouseDown = true;
}

void tileedit::OnMouseReleasedInCanvas(int canvasIdx, QMouseEvent *event)
{
	m_mouseDown = false;
	if (m_eTileBlockRef) {
		delete m_eTileBlockRef;
		m_eTileBlockRef = 0;
	}
	if (m_rndn) {
		delete m_rndn;
		m_rndn = 0;
	}
}

void tileedit::editElevation(int canvasIdx, int x, int y)
{
	std::pair<int, int> elevcrd = ElevNodeFromPixCoord(canvasIdx, x, y);
	int nx = elevcrd.first;
	int ny = elevcrd.second;
	const int padx = 1;
	const int pady = 1;

	switch (m_elevEditMode) {
	case ELEVEDIT_PAINT:
	case ELEVEDIT_RANDOM:
		{
			int v = ui->spinElevPaintValue->value();
			ElevData &edata = m_eTileBlock->getData();
			int sz = (m_elevEditMode == ELEVEDIT_PAINT ?
				ui->spinElevPaintSize->value() :
				ui->spinElevRandomSize->value()
				);
			sz = min(sz, 5);
			int mode = (m_elevEditMode == ELEVEDIT_PAINT ?
				ui->comboElevPaintMode->currentIndex() :
				ui->comboElevRandomMode->currentIndex()
				);
			std::vector<std::pair<int, int>> *stencil = paintStencil[sz - 1];
			bool ismod = false;
			bool boundsChanged = false;
			bool rescanBounds = false;
			for (int i = 0; i < stencil->size(); i++) {
				if (m_elevEditMode == ELEVEDIT_RANDOM)
					v = (int)(*m_rndn)(generator);
				int idx = (ny + pady + (*stencil)[i].second)*edata.width + (nx + padx + (*stencil)[i].first);
				if (idx >= 0 && idx < edata.data.size()) {
					INT16 vold = edata.data[idx];
					switch (mode) {
					case 0:
						edata.data[idx] = (INT16)v;
						break;
					case 1:
						edata.data[idx] = m_eTileBlockRef->getData().data[idx] + (INT16)v;
						break;
					case 2:
						if (edata.data[idx] < (INT16)v)
							edata.data[idx] = (INT16)v;
						break;
					case 3:
						if (edata.data[idx] > (INT16)v)
							edata.data[idx] = (INT16)v;
						break;
					}
					if (edata.data[idx] != vold) {
						if (vold == edata.dmin && edata.data[idx] > vold ||
							vold == edata.dmax && edata.data[idx] < vold) {
							rescanBounds = true;
						}
						else {
							if (edata.data[idx] < edata.dmin) {
								edata.dmin = edata.data[idx];
								boundsChanged = true;
							}
							if (edata.data[idx] > edata.dmax) {
								edata.dmax = edata.data[idx];
								boundsChanged = true;
							}
						}
						ismod = true;
					}
				}
			}
			if (ismod) {
				if (rescanBounds) {
					edata.dmin = *std::min_element(edata.data.begin(), edata.data.end());
					edata.dmax = *std::max_element(edata.data.begin(), edata.data.end());
					boundsChanged = true;
				}
				if (boundsChanged)
					m_eTileBlock->dataChanged();
				else
					m_eTileBlock->dataChanged(nx + padx - (sz / 2 + 1), nx + padx + (sz / 2 + 1), ny + pady - (sz / 2 + 1), ny + pady + (sz / 2 + 1));
				for (int i = 0; i < 3; i++)
					if (m_panel[i].layerType->currentIndex() >= 3) { // elevation
						refreshPanel(i);
					}
			}
		}
		break;
	case ELEVEDIT_ERASE:
		{
			int sz = ui->spinElevEraseSize->value();
			sz = min(sz, 5);
			std::vector<std::pair<int, int>> *stencil = paintStencil[sz - 1];
			ElevData &edata = m_eTileBlock->getData();
			ElevData &edataBase = m_eTileBlock->getBaseData();
			bool ismod = false;
			bool boundsChanged = false;
			for (int i = 0; i < stencil->size(); i++) {
				int idx = (ny + pady + (*stencil)[i].second)*edata.width + (nx + padx + (*stencil)[i].first);
				if (idx >= 0 && idx < edata.data.size() && edata.data[idx] != edataBase.data[idx]) {
					if (edata.data[idx] == edata.dmin || edata.data[idx] == edata.dmax)
						boundsChanged = true;
					edata.data[idx] = edataBase.data[idx];
					ismod = true;
				}
			}
			if (ismod) {
				if (boundsChanged) {
					edata.dmin = *std::min_element(edata.data.begin(), edata.data.end());
					edata.dmax = *std::max_element(edata.data.begin(), edata.data.end());
					m_eTileBlock->dataChanged();
				} else
					m_eTileBlock->dataChanged(nx + padx - (sz / 2 + 1), nx + padx + (sz / 2 + 1), ny + pady - (sz / 2 + 1), ny + pady + (sz / 2 + 1));
				for (int i = 0; i < 3; i++)
					if (m_panel[i].layerType->currentIndex() >= 3) { // elevation
						refreshPanel(i);
					}
			}
		}
		break;
	}
}

QString tileedit::ModeString() const
{
	std::string actionStr[2] = { "Navigate" , "Elevation: " };
	std::string eleveditStr[3] = { "Paint value", "Paint random", "Erase" };
	QString str(QString::fromStdString(actionStr[m_actionMode]));
	if (m_actionMode == 1) {
		str.append(QString::fromStdString(eleveditStr[m_elevEditMode]));
	}
	return str;
}

void tileedit::setToolOptions()
{
	ui->widgetElevPaint->setVisible(m_actionMode == ACTION_ELEVEDIT && m_elevEditMode == ELEVEDIT_PAINT);
	ui->widgetElevRandom->setVisible(m_actionMode == ACTION_ELEVEDIT && m_elevEditMode == ELEVEDIT_RANDOM);
	ui->widgetElevErase->setVisible(m_actionMode == ACTION_ELEVEDIT && m_elevEditMode == ELEVEDIT_ERASE);
}

void tileedit::setTile(int lvl, int ilat, int ilng)
{
	if (m_eTileBlock && m_eTileBlock->isModified()) {
		m_eTileBlock->syncTiles();
		m_eTileBlock->MatchNeighbourTiles();
		m_eTileBlock->SaveMod();
		m_eTileBlock->mapToAncestors(m_eTileBlock->Level() - 5);
	}

	m_lvl = lvl;
	m_ilat = ilat;
	m_ilng = ilng;
	loadTile(lvl, ilat, ilng);

	int nlat = (m_lvl < 4 ? 1 : 1 << (m_lvl - 4));
	int nlng = (m_lvl < 4 ? 1 : 1 << (m_lvl - 3));

	double latmax = (1.0 - (double)m_ilat / (double)nlat) * 180.0 - 90.0;
	double latmin = latmax - 180.0 / nlat;
	double lngmin = (double)m_ilng / (double)nlng * 360.0 - 180.0;
	double lngmax = lngmin + 360.0 / nlng;

	if (ui->spinResolution->value() != lvl) {
		ui->spinResolution->blockSignals(true);
		ui->spinResolution->setValue(lvl);
		ui->spinResolution->blockSignals(false);
	}
	if (ui->spinLatidx->maximum() != nlat - 1 || ui->spinLatidx->value() != ilat) {
		ui->spinLatidx->blockSignals(true);
		ui->spinLatidx->setMaximum(nlat - 1);
		ui->spinLatidx->setValue(ilat);
		ui->spinLatidx->blockSignals(false);
	}
	if (ui->spinLngidx->maximum() != nlng - 1 || ui->spinLngidx->value() != ilng) {
		ui->spinLngidx->blockSignals(true);
		ui->spinLngidx->setMaximum(nlng - 1);
		ui->spinLngidx->setValue(ilng);
		ui->spinLngidx->blockSignals(false);
	}
	char cbuf[256];
	sprintf(cbuf, "%+0.10lf", latmin);
	ui->labelLatmin->setText(cbuf);
	sprintf(cbuf, "%+0.10lf", latmax);
	ui->labelLatmax->setText(cbuf);
	sprintf(cbuf, "%+0.10lf", lngmin);
	ui->labelLngmin->setText(cbuf);
	sprintf(cbuf, "%+0.10lf", lngmax);
	ui->labelLngmax->setText(cbuf);
}

void tileedit::setupTreeManagers(std::string &root)
{
	releaseTreeManagers();

	m_mgrSurf = ZTreeMgr::CreateFromFile(root.c_str(), ZTreeMgr::LAYER_SURF);
	SurfTile::setTreeMgr(m_mgrSurf);

	m_mgrMask = ZTreeMgr::CreateFromFile(root.c_str(), ZTreeMgr::LAYER_MASK);
	MaskTile::setTreeMgr(m_mgrMask);

	m_mgrElev = ZTreeMgr::CreateFromFile(root.c_str(), ZTreeMgr::LAYER_ELEV);
	m_mgrElevMod = ZTreeMgr::CreateFromFile(root.c_str(), ZTreeMgr::LAYER_ELEVMOD);
	ElevTile::setTreeMgr(m_mgrElev, m_mgrElevMod);
}

void tileedit::releaseTreeManagers()
{
	if (m_mgrSurf) {
		delete m_mgrSurf;
		m_mgrSurf = 0;
	}
	if (m_mgrMask) {
		delete m_mgrMask;
		m_mgrMask = 0;
	}
	if (m_mgrElev) {
		delete m_mgrElev;
		m_mgrElev = 0;
	}
	if (m_mgrElevMod) {
		delete m_mgrElevMod;
		m_mgrElevMod = 0;
	}
}
