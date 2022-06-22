#include "dlgelevconfig.h"
#include "ui_dlgElevConfig.h"
#include "tileedit.h"

DlgElevConfig::DlgElevConfig(tileedit *parent, ElevDisplayParam &elevDisplayParam)
	: QDialog(parent)
	, m_tileedit(parent)
	, m_elevDisplayParam(elevDisplayParam)
	, ui(new Ui::DlgElevConfig)
{
	ui->setupUi(this);
	ui->comboColourmap->setCurrentIndex((int)m_tileedit->m_elevDisplayParam.cmName);
	ui->checkWaterMask->setChecked(m_elevDisplayParam.useWaterMask);
	ui->spinRangeMin->setValue(m_elevDisplayParam.rangeMin);
	ui->spinRangeMax->setValue(m_elevDisplayParam.rangeMax);
	ui->radioRangeAuto->setChecked(m_elevDisplayParam.autoRange);
	ui->radioRangeFixed->setChecked(!m_elevDisplayParam.autoRange);
	ui->widgetFixedRange->setEnabled(!m_elevDisplayParam.autoRange);

	connect(ui->comboColourmap, SIGNAL(currentIndexChanged(int)), this, SLOT(onColourmapChanged(int)));
	connect(ui->checkWaterMask, SIGNAL(clicked()), this, SLOT(onWaterMask()));
	connect(ui->radioRangeAuto, SIGNAL(clicked()), this, SLOT(onRangeAuto()));
	connect(ui->radioRangeFixed, SIGNAL(clicked()), this, SLOT(onRangeFixed()));
	connect(ui->spinRangeMin, SIGNAL(valueChanged(int)), this, SLOT(onRangeMin(int)));
	connect(ui->spinRangeMax, SIGNAL(valueChanged(int)), this, SLOT(onRangeMax(int)));
}

void DlgElevConfig::done(int r)
{
	emit finished(r);
}

void DlgElevConfig::onColourmapChanged(int idx)
{
	if (idx != (int)m_elevDisplayParam.cmName) {
		m_elevDisplayParam.cmName = (CmapName)idx;
		m_tileedit->elevDisplayParamChanged();
	}
}

void DlgElevConfig::onWaterMask()
{
	bool checked = ui->checkWaterMask->isChecked();
	if (checked != m_elevDisplayParam.useWaterMask) {
		m_elevDisplayParam.useWaterMask = checked;
		m_tileedit->elevDisplayParamChanged();
	}
}

void DlgElevConfig::onRangeAuto()
{
	ui->widgetFixedRange->setEnabled(false);
	if (!m_elevDisplayParam.autoRange) {
		m_elevDisplayParam.autoRange = true;
		m_tileedit->elevDisplayParamChanged();
	}
}

void DlgElevConfig::onRangeFixed()
{
	ui->widgetFixedRange->setEnabled(true);
	if (m_elevDisplayParam.autoRange) {
		m_elevDisplayParam.autoRange = false;
		m_elevDisplayParam.rangeMin = ui->spinRangeMin->value();
		m_elevDisplayParam.rangeMax = ui->spinRangeMax->value();
		m_tileedit->elevDisplayParamChanged();
	}
}

void DlgElevConfig::onRangeMin(int val)
{
	if (m_elevDisplayParam.rangeMin != val) {
		m_elevDisplayParam.rangeMin = val;
		m_tileedit->elevDisplayParamChanged();
	}
}

void DlgElevConfig::onRangeMax(int val)
{
	if (m_elevDisplayParam.rangeMax != val) {
		m_elevDisplayParam.rangeMax = val;
		m_tileedit->elevDisplayParamChanged();
	}
}