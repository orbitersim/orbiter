#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <random>

#include <QMainWindow>
#include <QLabel>
#include <QComboBox>
#include <QDoubleSpinBox>
#include <QSettings>

#include "elevtile.h"
#include "ZTreeMgr.h"
#include "colorbar.h"

#define TILESEARCH_CACHE 0x1
#define TILESEARCH_ARCHIVE 0x2

namespace Ui {
class tileedit;
}

class SurfTileBlock;
class MaskTileBlock;
class NightlightTileBlock;
class ElevTileBlock;
class TileCanvas;
class DlgElevConfig;

class tileedit : public QMainWindow
{
	Q_OBJECT

	friend class DlgConfig;
	friend class DlgElevConfig;
	friend class DlgElevExport;
	friend class DlgElevImport;
	friend class TileCanvas;

public:
    explicit tileedit(QWidget *parent = 0);
    ~tileedit();

	void elevDisplayParamChanged();
	void setLoadMode(DWORD mode);
	void setAncestorMode(TileLoadMode mode);

	void setBlockSize(int bsize);
	QSettings *settings() { return m_settings; }

protected:
    void createActions();
    void createMenus();
    void resizeEvent(QResizeEvent *event);
    void ensureSquareCanvas(int winw, int winh);
    void loadTile(int lvl, int ilat, int ilng);
    void refreshPanel(int panelIdx);
	void setTile(int lvl, int ilat, int ilng);

private:
	void setToolOptions();
	QString ModeString() const;
	std::pair<int, int> ElevNodeFromPixCoord(int canvasIdx, int x, int y);
	void editElevation(int canvasIdx, int x, int y);
	void setupTreeManagers(std::string &root);
	void releaseTreeManagers();

private slots:
    void openDir();
	void on_actionExit_triggered();
	void on_actionConfig_triggered();
	void onSurfImportImage();
	void onElevConfig();
	void onElevExportImage();
	void onElevImportImage();
	void onElevConfigDestroyed(int r);
    void onResolutionChanged(int val);
    void onLatidxChanged(int val);
    void onLngidxChanged(int val);
	void onActionButtonClicked(int id);
	void onEditButtonClicked(int id);
    void onLayerType0(int);
    void onLayerType1(int);
    void onLayerType2(int);
    void OnTileChangedFromPanel(int lvl, int ilat, int ilng);
	void OnTileEntered(TileCanvas *canvas);
	void OnTileLeft(TileCanvas *canvas);
	void OnMouseMovedInCanvas(int canvasIdx, QMouseEvent *event);
	void OnMousePressedInCanvas(int canvasIdx, QMouseEvent *event);
	void OnMouseReleasedInCanvas(int canvasIdx, QMouseEvent *event);

private:
    Ui::tileedit *ui;
	QSettings *m_settings;
    QMenu *fileMenu;
    QAction *openAct;
	QAction *actionConfig;
	QAction *actionExit;
	QAction *actionSurfImport;
	QAction *actionElevConfig;
	QAction *actionElevExport;
	QAction *actionElevImport;

    QLabel *status;

    struct TilePanel {
        TileCanvas *canvas;
		Colorbar *colorbar;
        QComboBox *layerType;
		QLabel *fileId;
    } m_panel[3];

	enum ActionMode {
		ACTION_NAVIGATE,
		ACTION_ELEVEDIT
	} m_actionMode;

	enum ElevEditMode {
		ELEVEDIT_PAINT,
		ELEVEDIT_RANDOM,
		ELEVEDIT_ERASE
	} m_elevEditMode;

    //std::string rootDir;
    int m_lvl;
    int m_ilat;
    int m_ilng;

	DWORD m_openMode;
	TileLoadMode m_globalLoadMode;
	int m_blocksize;

	ElevDisplayParam m_elevDisplayParam;

	bool m_mouseDown;

    SurfTileBlock *m_sTileBlock;
	MaskTileBlock *m_mTileBlock;
	ElevTileBlock *m_eTileBlock;
	ElevTileBlock *m_eTileBlockRef;

	// The tree archive accessors
	ZTreeMgr *m_mgrSurf;
	ZTreeMgr *m_mgrMask;
	ZTreeMgr *m_mgrElev;
	ZTreeMgr *m_mgrElevMod;

	DlgElevConfig *m_dlgElevConfig;

	std::normal_distribution<double> *m_rndn;
};

#endif // MAINWINDOW_H
