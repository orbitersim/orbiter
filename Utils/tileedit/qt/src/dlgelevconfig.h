#ifndef DLGELEVCONFIG_H
#define DLGELEVCONFIG_H

#include <QDialog>
#include "elevtile.h"

namespace Ui {
	class DlgElevConfig;
}

class tileedit;

class DlgElevConfig : public QDialog
{
	Q_OBJECT

public:
	DlgElevConfig(tileedit *parent, ElevDisplayParam &elevDisplayParam);

public slots:
	void done(int r);
	void onColourmapChanged(int idx);
	void onWaterMask();
	void onRangeAuto();
	void onRangeFixed();
	void onRangeMin(int val);
	void onRangeMax(int val);

private:
	Ui::DlgElevConfig *ui;
	tileedit *m_tileedit;
	ElevDisplayParam &m_elevDisplayParam;
};

#endif // !DLGELEVCONFIG_H
