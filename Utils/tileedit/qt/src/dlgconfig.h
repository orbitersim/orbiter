#ifndef DLGCONFIG_H
#define DLGCONFIG_H

#include <QDialog>

namespace Ui {
	class DlgConfig;
}

class tileedit;

class DlgConfig : public QDialog
{
	Q_OBJECT

public:
	DlgConfig(tileedit *parent);

public slots:
	void accept();

private:
	Ui::DlgConfig *ui;
	tileedit *m_tileedit;
};

#endif // !DLGCONFIG_H
