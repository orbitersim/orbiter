#include "dlgsurfimport.h"
#include "ui_dlgSurfImport.h"
#include "tileedit.h"
#include "tileblock.h"

#include <QFileDialog>
#include <QMessageBox>

DlgSurfImport::DlgSurfImport(tileedit *parent)
	: QDialog(parent)
	, m_tileedit(parent)
	, ui(new Ui::DlgSurfImport)
{
	ui->setupUi(this);

	connect(ui->pushOpenFileDialog, SIGNAL(clicked()), this, SLOT(onOpenFileDialog()));
	connect(ui->pushOpenMetaFileDialog, SIGNAL(clicked()), this, SLOT(onOpenMetaFileDialog()));
	connect(ui->radioParamFromMeta, SIGNAL(clicked()), this, SLOT(onParamFromMeta()));
	connect(ui->radioParamFromUser, SIGNAL(clicked()), this, SLOT(onParamFromUser()));
	connect(ui->editMetaPath, SIGNAL(textChanged(const QString&)), this, SLOT(onMetaFileChanged(const QString&)));
	connect(ui->spinLvl, SIGNAL(valueChanged(int)), this, SLOT(onLvl(int)));
	connect(ui->checkPropagateChanges, SIGNAL(stateChanged(int)), this, SLOT(onPropagateChanges(int)));
	m_pathEdited = m_metaEdited = false;
	m_haveMeta = false;
	memset(&m_metaInfo, 0, sizeof(SurfPatchMetaInfo));
}

void DlgSurfImport::onOpenFileDialog()
{
	QString path = ui->editPath->text();
	if (!path.size()) {
		QSettings *settings = m_tileedit->settings();
		path = settings->value("export/path", ".").toString();
	}
	path = QFileDialog::getOpenFileName(this, tr("Import surface tiles from image file"), path, tr("Portable network graphics (*.png)"));
	if (path.size()) {
		ui->editPath->setText(path);
		if (!m_metaEdited) {
			path.append(".hdr");
			ui->editMetaPath->setText(path);
		}
		m_pathEdited = true;
	}
}

void DlgSurfImport::onOpenMetaFileDialog()
{
	QString path = QFileDialog::getOpenFileName(this, tr("Import surface tiles from image file"), ui->editMetaPath->text(), tr("Metadata file (*.hdr)"));
	if (path.size()) {
		ui->editMetaPath->setText(path);
		if (!m_pathEdited) {
			path.truncate(path.length() - 4);
			ui->editPath->setText(path);
		}
		m_metaEdited = true;
	}
}

void DlgSurfImport::onParamFromMeta()
{
	ui->widgetParamMeta->setEnabled(true);
	ui->widgetParamUser->setEnabled(false);
}

void DlgSurfImport::onParamFromUser()
{
	ui->widgetParamMeta->setEnabled(false);
	ui->widgetParamUser->setEnabled(true);
}

void DlgSurfImport::onMetaFileChanged(const QString &name)
{
	m_haveMeta = scanMetaFile(name.toLatin1(), m_metaInfo);
	if (m_haveMeta) {
		ui->spinLvl->setValue(m_metaInfo.lvl);
		ui->spinIlat0->setValue(m_metaInfo.ilat0);
		ui->spinIlat1->setValue(m_metaInfo.ilat1 - 1);
		ui->spinIlng0->setValue(m_metaInfo.ilng0);
		ui->spinIlng1->setValue(m_metaInfo.ilng1 - 1);
	}
	else {
		memset(&m_metaInfo, 0, sizeof(SurfPatchMetaInfo));
		ui->spinLvl->setValue(1);
		ui->spinIlat0->setValue(0);
		ui->spinIlat1->setValue(0);
		ui->spinIlng0->setValue(0);
		ui->spinIlng1->setValue(0);
	}
}

void DlgSurfImport::onLvl(int val)
{
	int nlat = nLat(val);
	int nlng = nLng(val);
	ui->spinIlat0->setMaximum(nlat - 1);
	ui->spinIlat1->setMaximum(nlat - 1);
	ui->spinIlng0->setMaximum(nlng - 1);
	ui->spinIlng1->setMaximum(nlng - 1);
}

void DlgSurfImport::onPropagateChanges(int state)
{
	ui->widgetPropagateChanges->setEnabled(state == Qt::Checked);
}

void DlgSurfImport::accept()
{
	if (ui->radioParamFromUser->isChecked()) {
		m_metaInfo.lvl = ui->spinLvl->value();
		m_metaInfo.ilat0 = ui->spinIlat0->value();
		m_metaInfo.ilat1 = ui->spinIlat1->value() + 1;
		m_metaInfo.ilng0 = ui->spinIlng0->value();
		m_metaInfo.ilng1 = ui->spinIlng1->value() + 1;
	}
	else {
		if (!m_haveMeta) {
			QMessageBox mbox(QMessageBox::Warning, tr("tileedit: Warning"), tr("No valid metadata information is available"), QMessageBox::Close);
			mbox.exec();
			return;
		}
	}
	m_metaInfo.alphaBlend = ui->checkAlphaBlend->isChecked();
	m_metaInfo.colourMatch = ui->comboColourmatch->currentIndex();

	SurfTileBlock *sblock = SurfTileBlock::Load(m_metaInfo.lvl, m_metaInfo.ilat0, m_metaInfo.ilat1, m_metaInfo.ilng0, m_metaInfo.ilng1);
	int res = dxtread_png(ui->editPath->text().toLatin1(), m_metaInfo, sblock->getData());
	if (res != 0) {
		QString msg("Error reading PNG file:\n");
		switch (res) {
		case -1:
			msg.append("File could not be opened.");
			break;
		case -2:
			msg.append("Invalid image size. Expected: ").append(QString::number(sblock->getData().width)).append(" x ").append(QString::number(sblock->getData().width));
			break;
		}
		QMessageBox mbox(QMessageBox::Warning, tr("tileedit: Warning"), msg, QMessageBox::Close);
		mbox.exec();
		return;
	}
	else {
		QString path = ui->editPath->text();
		QFileInfo fi(path);
		QSettings *settings = m_tileedit->settings();
		settings->setValue("export/path", fi.absolutePath());
	}

	for (int ilat = m_metaInfo.ilat0; ilat < m_metaInfo.ilat1; ilat++)
		for (int ilng = m_metaInfo.ilng0; ilng < m_metaInfo.ilng1; ilng++) {
			sblock->syncTile(ilat, ilng);
			SurfTile *stile = (SurfTile*)sblock->_getTile(ilat, ilng);
			stile->Save();
		}
	if (ui->checkPropagateChanges->isChecked())
		sblock->mapToAncestors(ui->spinPropagationLevel->value());
	
	QDialog::accept();
}

bool DlgSurfImport::scanMetaFile(const char *fname, SurfPatchMetaInfo &meta)
{
	FILE *f = fopen(fname, "rt");
	if (!f) return false;

	int ilat, ilng, n;
	char str[1024];
	if (fscanf(f, "lvl=%d ilat0=%d ilat1=%d ilng0=%d ilng1=%d\n",
		&meta.lvl, &meta.ilat0, &meta.ilat1, &meta.ilng0, &meta.ilng1) != 5) {
		meta.lvl = meta.ilat0 = meta.ilat1 = meta.ilng0 = meta.ilng1 = 0;
	}
	else {
		fscanf(f, "%s", str);
		if (!strncmp(str, "missing", 7)) {
			while (true) {
				n = fscanf(f, "%d/%d", &ilat, &ilng);
				if (n == 2) {
					meta.missing.push_back(std::make_pair(ilat, ilng));
				}
				else
					break;
			}
		}
	}
	fclose(f);
	return true;
}