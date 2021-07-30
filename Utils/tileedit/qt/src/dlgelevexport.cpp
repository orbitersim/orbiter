#include "dlgelevexport.h"
#include "ui_dlgElevExport.h"
#include "tileedit.h"
#include "tileblock.h"
#include "cmap.h"

#include <QFileDialog>
#include <QMessageBox>
#include <QPainter>

DlgElevExport::DlgElevExport(tileedit *parent)
	: QDialog(parent)
	, m_tileedit(parent)
	, ui(new Ui::DlgElevExport)
{
	ui->setupUi(this);

	connect(ui->pushOpenFileDialog, SIGNAL(clicked()), this, SLOT(onOpenFileDialog()));
	connect(ui->radioCurrentTiles, SIGNAL(clicked()), this, SLOT(onSelectCurrentTiles()));
	connect(ui->radioCustomTiles, SIGNAL(clicked()), this, SLOT(onSelectCustomTiles()));
	connect(ui->spinLvl, SIGNAL(valueChanged(int)), this, SLOT(onResolution(int)));
	connect(ui->spinIlat0, SIGNAL(valueChanged(int)), this, SLOT(onIlat0(int)));
	connect(ui->spinIlat1, SIGNAL(valueChanged(int)), this, SLOT(onIlat1(int)));
	connect(ui->spinIlng0, SIGNAL(valueChanged(int)), this, SLOT(onIlng0(int)));
	connect(ui->spinIlng1, SIGNAL(valueChanged(int)), this, SLOT(onIlng1(int)));
	connect(ui->radioScaleAuto, SIGNAL(clicked()), this, SLOT(onScaleAuto()));
	connect(ui->radioScaleManual, SIGNAL(clicked()), this, SLOT(onScaleManual()));
	connect(ui->dspinImageMin, SIGNAL(valueChanged(double)), this, SLOT(onImageValMin(double)));
	connect(ui->dspinImageMax, SIGNAL(valueChanged(double)), this, SLOT(onImageValMax(double)));
	connect(ui->dspinImageResolution, SIGNAL(valueChanged(double)), this, SLOT(onImageResolution(double)));

	m_CurrentBlock = m_tileedit->m_eTileBlock;
	if (m_CurrentBlock) {
		m_lvl = m_CurrentBlock->Level();
		m_ilat0 = m_CurrentBlock->iLat0();
		m_ilat1 = m_CurrentBlock->iLat1();
		m_ilng0 = m_CurrentBlock->iLng0();
		m_ilng1 = m_CurrentBlock->iLng1();
	}
	else {
		m_lvl = 1;
		m_ilat0 = m_ilat1 = m_ilng0 = m_ilng1 = 0;
	}
	ui->spinLvl->setValue(m_lvl);
	ui->spinIlat0->setValue(m_ilat0);
	ui->spinIlat1->setValue(m_ilat1 - 1);
	ui->spinIlng0->setValue(m_ilng0);
	ui->spinIlng1->setValue(m_ilng1 - 1);
	ui->spinIlat0->setMaximum(nLat(m_lvl) - 1);
	ui->spinIlat1->setMaximum(nLat(m_lvl) - 1);
	ui->spinIlng0->setMaximum(nLng(m_lvl) - 1);
	ui->spinIlng1->setMaximum(nLng(m_lvl) - 1);

	ui->labelTruncationWarning->setVisible(false);
	ui->labelTruncationWarning->setStyleSheet("Color:red;");

	RescanLimits();

	QSettings *settings = m_tileedit->settings();
	QString path = settings->value("export/path", ".").toString();
	char fname[1024];
	sprintf(fname, "/elev_%02d_%06d_%06d.png", m_lvl, m_ilat0, m_ilng0);
	path.append(fname);
	ui->editPath->setText(path);
}

void DlgElevExport::onOpenFileDialog()
{
	QString path = QFileDialog::getSaveFileName(this, tr("Export elevation tiles to image file"), ui->editPath->text(), tr("Portable network graphics (*.png)"));
	if (path.size())
		ui->editPath->setText(path);
}

void DlgElevExport::onSelectCurrentTiles()
{
	ui->widgetTileRange->setEnabled(false);
	RescanLimits();
}

void DlgElevExport::onSelectCustomTiles()
{
	ui->widgetTileRange->setEnabled(true);
	RescanLimits();
}

void DlgElevExport::onResolution(int value)
{
	m_lvl = value;
	ui->spinIlat0->setMaximum(nLat(m_lvl) - 1);
	ui->spinIlat1->setMaximum(nLat(m_lvl) - 1);
	ui->spinIlng0->setMaximum(nLng(m_lvl) - 1);
	ui->spinIlng1->setMaximum(nLng(m_lvl) - 1);
}

void DlgElevExport::onIlat0(int value)
{
	m_ilat0 = value;
	RescanLimits();
}

void DlgElevExport::onIlat1(int value)
{
	m_ilat1 = value + 1;
	RescanLimits();
}

void DlgElevExport::onIlng0(int value)
{
	m_ilng0 = value;
	RescanLimits();
}

void DlgElevExport::onIlng1(int value)
{
	m_ilng1 = value + 1;
	RescanLimits();
}

void DlgElevExport::onScaleAuto()
{
	ui->widgetScale->setEnabled(false);

	ui->dspinImageMin->setValue(m_elevMin);
	ui->dspinImageMax->setValue(m_elevMax);
	ui->dspinImageResolution->setValue((m_elevMax - m_elevMin) / (double)USHRT_MAX);
	ui->widgetColormap->setColorRange(ui->dspinImageMin->value(), ui->dspinImageMax->value());
	ui->labelTruncationWarning->setVisible(false);
}

void DlgElevExport::onScaleManual()
{
	ui->widgetScale->setEnabled(true);
	CheckTruncation();
}

void DlgElevExport::onImageValMin(double v)
{
	ui->dspinImageResolution->blockSignals(true);
	ui->dspinImageResolution->setValue((ui->dspinImageMax->value() - v) / (double)USHRT_MAX);
	ui->dspinImageResolution->blockSignals(false);
	ui->widgetColormap->setColorRange(ui->dspinImageMin->value(), ui->dspinImageMax->value());
	CheckTruncation();
}

void DlgElevExport::onImageValMax(double v)
{
	ui->dspinImageResolution->blockSignals(true);
	ui->dspinImageResolution->setValue((v - ui->dspinImageMin->value()) / (double)USHRT_MAX);
	ui->dspinImageResolution->blockSignals(false);
	ui->widgetColormap->setColorRange(ui->dspinImageMin->value(), ui->dspinImageMax->value());
	CheckTruncation();
}

void DlgElevExport::onImageResolution(double v)
{
	ui->dspinImageMax->blockSignals(true);
	ui->dspinImageMax->setValue(v * (double)USHRT_MAX + ui->dspinImageMin->value());
	ui->dspinImageMax->blockSignals(false);
	ui->widgetColormap->setColorRange(ui->dspinImageMin->value(), ui->dspinImageMax->value());
	CheckTruncation();
}

void DlgElevExport::accept()
{
	bool isWritten = false;

	double vmin = ui->dspinImageMin->value();
	double vmax = ui->dspinImageMax->value();

	if (ui->radioCurrentTiles->isChecked()) {
		if (!m_CurrentBlock->hasAncestorData() || saveWithAncestorData()) {
			m_CurrentBlock->ExportPNG(ui->editPath->text().toStdString(), vmin, vmax);
			isWritten = true;
		}
	}
	else {
		ElevTileBlock *eblock = ElevTileBlock::Load(m_lvl, m_ilat0, m_ilat1, m_ilng0, m_ilng1);
		if (!eblock->hasAncestorData() || saveWithAncestorData()) {
			eblock->ExportPNG(ui->editPath->text().toStdString(), vmin, vmax);
			isWritten = true;
		}
		delete eblock;
	}
	if (isWritten) {
		QString path = ui->editPath->text();
		QFileInfo fi(path);
		QSettings *settings = m_tileedit->settings();
		settings->setValue("export/path", fi.absolutePath());

		QDialog::accept();
	}
}

bool DlgElevExport::saveWithAncestorData() const
{
	QMessageBox mbox(QMessageBox::Warning, tr("tileedit: Warning"), tr("The exported tilegrid contains at least one non-existent tile which has been synthesized from an ancestor sub-region. Export anyway?"), QMessageBox::Yes | QMessageBox::No);
	return (mbox.exec() == QMessageBox::Yes);
}

void DlgElevExport::RescanLimits()
{
	m_elevMin = 0.0;
	m_elevMax = 0.0;

	if (ui->radioCurrentTiles->isChecked()) {
		if (m_CurrentBlock) {
			m_elevMin = m_CurrentBlock->getData().dmin;
			m_elevMax = m_CurrentBlock->getData().dmax;
		}
	}
	else {
		bool isFirst = true;
		for (int ilat = m_ilat0; ilat < m_ilat1; ilat++)
			for (int ilng = m_ilng0; ilng < m_ilng1; ilng++) {
				ElevTile *tile = ElevTile::Load(m_lvl, ilat, ilng);
				if (isFirst || tile->getData().dmin < m_elevMin)
					m_elevMin = tile->getData().dmin;
				if (isFirst || tile->getData().dmax > m_elevMax)
					m_elevMax = tile->getData().dmax;
				isFirst = false;
			}
	}

	char cbuf[1024];
	sprintf(cbuf, "%+0.2lf m", m_elevMin);
	ui->labelDataMin->setText(cbuf);
	sprintf(cbuf, "%+0.2lf m", m_elevMax);
	ui->labelDataMax->setText(cbuf);

	if (ui->radioScaleAuto->isChecked()) {
		ui->dspinImageMin->setValue(m_elevMin);
		ui->dspinImageMax->setValue(m_elevMax);
		ui->dspinImageResolution->setValue((m_elevMax - m_elevMin) / (double)USHRT_MAX);
	}

	ui->widgetColormap->setDataRange(m_elevMin, m_elevMax);
	ui->widgetColormap->setColorRange(ui->dspinImageMin->value(), ui->dspinImageMax->value());
	ui->widgetColormap->setPaintRange(true);
}

void DlgElevExport::CheckTruncation()
{
	bool truncate = (m_elevMin < ui->dspinImageMin->value() || m_elevMax > ui->dspinImageMax->value());
	ui->labelTruncationWarning->setVisible(truncate);
}

// =====================================================================================

DlgElevExportColorbar::DlgElevExportColorbar(QWidget *parent)
	: QWidget(parent)
{
	m_paintDataRange = false;
	m_vmin = m_vmax = m_dmin = m_dmax = 0.0;
}

void DlgElevExportColorbar::setColorRange(double vmin, double vmax)
{
	m_vmin = vmin;
	m_vmax = vmax;
	if (m_paintDataRange)
		this->update();
}

void DlgElevExportColorbar::setDataRange(double dmin, double dmax)
{
	m_dmin = dmin;
	m_dmax = dmax;
	if (m_paintDataRange)
		this->update();
}

void DlgElevExportColorbar::setPaintRange(bool paint)
{
	if (paint != m_paintDataRange) {
		m_paintDataRange = paint;
		this->update();
	}
}

void DlgElevExportColorbar::paintEvent(QPaintEvent *event)
{
	QPainter painter(this);

	const Cmap &cm = cmap(CMAP_GREY);
	DWORD data[256];
	memcpy(data, cm, 256 * sizeof(DWORD));
	for (int i = 0; i < 256; i++)
		data[i] |= 0xff000000;
	QImage qimg((BYTE*)data, 256, 1, QImage::Format_ARGB32);

	int w = rect().width() - 1;
	int h = rect().height() - 1;
	QRect r(0, 0, w, h);

	if (m_paintDataRange) {
		if (m_dmin < m_vmin || m_dmax > m_vmax) {
			painter.setBrush(QBrush(QColor(255, 128, 128)));
			painter.drawRect(r);
			painter.setBrush(Qt::NoBrush);
		}
		double smin = min(m_dmin, m_vmin);
		double smax = max(m_dmax, m_vmax);
		if (m_dmin < m_vmin) {
			r.setLeft((int)((m_vmin - smin) / (smax - smin) * w));
		}
		if (m_dmax > m_vmax)
			r.setRight((int)((m_vmax - smin) / (smax - smin) * w));
	}
	painter.drawImage(r, qimg);
	painter.setPen(QColor(0, 0, 0));
	painter.drawRect(r);

	if (m_paintDataRange) {
		int xmin = max(0, min(w, (int)(w * (m_dmin - m_vmin) / (m_vmax - m_vmin))));
		int xmax = max(0, min(w, (int)(w * (m_dmax - m_vmin) / (m_vmax - m_vmin))));
		painter.setPen(QColor(255, 0, 0));
		painter.drawRect(xmin, 0, xmax - xmin, h);
	}
}
