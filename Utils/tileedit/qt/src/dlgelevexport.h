#ifndef DLGELEVEXPORT_H
#define DLGELEVEXPORT_H

#include <QDialog>

namespace Ui {
	class DlgElevExport;
}

class tileedit;
class ElevTileBlock;

class DlgElevExport : public QDialog
{
	Q_OBJECT

public:
	DlgElevExport(tileedit *parent);

private:
	bool saveWithAncestorData() const;
	void RescanLimits();
	void CheckTruncation();

public slots:
	void onOpenFileDialog();
	void onSelectCurrentTiles();
	void onSelectCustomTiles();
	void onResolution(int);
	void onIlat0(int);
	void onIlat1(int);
	void onIlng0(int);
	void onIlng1(int);
	void onScaleAuto();
	void onScaleManual();
	void onImageValMin(double);
	void onImageValMax(double);
	void onImageResolution(double);
	void accept();

private:
	Ui::DlgElevExport *ui;
	std::string m_fileName;
	tileedit *m_tileedit;
	int m_lvl;
	int m_ilat0, m_ilat1;
	int m_ilng0, m_ilng1;
	double m_elevMin, m_elevMax;
	ElevTileBlock *m_CurrentBlock;
};

class DlgElevExportColorbar : public QWidget
{
	Q_OBJECT

public:
	explicit DlgElevExportColorbar(QWidget *parent = 0);
	void setColorRange(double vmin, double vmax);
	void setDataRange(double dmin, double dmax);
	void setPaintRange(bool paint);

protected:
	void paintEvent(QPaintEvent *event);

private:
	bool m_paintDataRange;
	double m_vmin, m_vmax;
	double m_dmin, m_dmax;
};

#endif // !DLGELEVEXPORT_H
