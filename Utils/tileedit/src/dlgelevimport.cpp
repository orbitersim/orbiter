#include "dlgelevimport.h"
#include "ui_dlgElevImport.h"
#include "tileedit.h"
#include "tileblock.h"

#include <QFileDialog>
#include <QMessageBox>

DlgElevImport::DlgElevImport(tileedit *parent)
	: QDialog(parent)
	, m_tileedit(parent)
	, ui(new Ui::DlgElevImport)
{
	ui->setupUi(this);

	connect(ui->pushOpenFileDialog, SIGNAL(clicked()), this, SLOT(onOpenFileDialog()));
	connect(ui->pushOpenMetaFileDialog, SIGNAL(clicked()), this, SLOT(onOpenMetaFileDialog()));
	connect(ui->editMetaPath, SIGNAL(textChanged(const QString&)), this, SLOT(onMetaFileChanged(const QString&)));
	connect(ui->radioImportAll, SIGNAL(clicked()), this, SLOT(onSelectAllTiles()));
	connect(ui->radioImportRange, SIGNAL(clicked()), this, SLOT(onSelectTileRange()));
	connect(ui->checkPropagateChanges, SIGNAL(stateChanged(int)), this, SLOT(onPropagateChanges(int)));

	m_pathEdited = m_metaEdited = false;
	m_haveMeta = false;
	m_propagationLevel = ui->spinPropagationLevel->value();
	memset(&m_metaInfo, 0, sizeof(ElevPatchMetaInfo));
}

void DlgElevImport::onOpenFileDialog()
{
	QString path = ui->editPath->text();
	if (!path.size()) {
		QSettings *settings = m_tileedit->settings();
		path = settings->value("export/path", ".").toString();
	}
	path = QFileDialog::getOpenFileName(this, tr("Import elevation tiles from image file"), path, tr("Portable network graphics (*.png)"));
	if (path.size()) {
		ui->editPath->setText(path);
		if (!m_metaEdited) {
			path.append(".hdr");
			ui->editMetaPath->setText(path);
		}
		m_pathEdited = true;
	}
}

void DlgElevImport::onOpenMetaFileDialog()
{
	QString path = QFileDialog::getOpenFileName(this, tr("Import elevation tiles from image file"), ui->editMetaPath->text(), tr("Metadata file (*.hdr)"));
	if (path.size()) {
		ui->editMetaPath->setText(path);
		if (!m_pathEdited) {
			path.truncate(path.length()-4);
			ui->editPath->setText(path);
		}
		m_metaEdited = true;
	}
}

void DlgElevImport::onMetaFileChanged(const QString &name)
{
	m_haveMeta = scanMetaFile(name.toLatin1(), m_metaInfo);
	if (m_haveMeta) {
		ui->labelLvl->setText(QString::number(m_metaInfo.lvl));
		ui->spinIlat0->setValue(m_metaInfo.ilat0);
		ui->spinIlat1->setValue(m_metaInfo.ilat1 - 1);
		ui->spinIlng0->setValue(m_metaInfo.ilng0);
		ui->spinIlng1->setValue(m_metaInfo.ilng1 - 1);
		ui->spinIlat0->setMinimum(m_metaInfo.ilat0);
		ui->spinIlat1->setMinimum(m_metaInfo.ilat0);
		ui->spinIlng0->setMinimum(m_metaInfo.ilng0);
		ui->spinIlng1->setMinimum(m_metaInfo.ilng0);
		ui->spinIlat0->setMaximum(m_metaInfo.ilat1 - 1);
		ui->spinIlat1->setMaximum(m_metaInfo.ilat1 - 1);
		ui->spinIlng0->setMaximum(m_metaInfo.ilng1 - 1);
		ui->spinIlng1->setMaximum(m_metaInfo.ilng1 - 1);
	}
	else {
		memset(&m_metaInfo, 0, sizeof(ElevPatchMetaInfo));
		ui->labelLvl->setText("-");
		ui->spinIlat0->setValue(0);
		ui->spinIlat1->setValue(0);
		ui->spinIlng0->setValue(0);
		ui->spinIlng1->setValue(0);
		ui->spinIlat0->setMinimum(0);
		ui->spinIlat1->setMinimum(0);
		ui->spinIlng0->setMinimum(0);
		ui->spinIlng1->setMinimum(0);
		ui->spinIlat0->setMaximum(0);
		ui->spinIlat1->setMaximum(0);
		ui->spinIlng0->setMaximum(0);
		ui->spinIlng1->setMaximum(0);
	}
}

void DlgElevImport::onSelectAllTiles()
{
	ui->spinIlat0->setValue(m_metaInfo.ilat0);
	ui->spinIlat1->setValue(m_metaInfo.ilat1 - 1);
	ui->spinIlng0->setValue(m_metaInfo.ilng0);
	ui->spinIlng1->setValue(m_metaInfo.ilng1 - 1);
	ui->widgetTileRange->setEnabled(false);
}

void DlgElevImport::onSelectTileRange()
{
	ui->widgetTileRange->setEnabled(true);

}

void DlgElevImport::onPropagateChanges(int state)
{
	m_propagationLevel = (state == Qt::Checked ? ui->spinPropagationLevel->value() : 0);
	ui->widgetPropagateChanges->setEnabled(state == Qt::Checked);
}

bool DlgElevImport::scanMetaFile(const char *fname, ElevPatchMetaInfo &meta)
{
	FILE *f = fopen(fname, "rt");
	if (!f) return false;

	int ilat, ilng, n;
	double smin, emin, smean, emean, smax, emax;
	char str[1024];
	fscanf(f, "vmin=%lf vmax=%lf scale=%lf offset=%lf type=%d padding=1x1 colormap=%d smin=%lf emin=%lf smean=%lf emean=%lf smax=%lf emax=%lf latmin=%lf latmax=%lf lngmin=%lf lngmax=%lf\n",
		&meta.dmin, &meta.dmax, &meta.scale, &meta.offset, &meta.type, &meta.colormap, &smin, &emin, &smean, &emean, &smax, &emax,
		&meta.latmin, &meta.latmax, &meta.lngmin, &meta.lngmax);
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

void DlgElevImport::accept()
{
	if (!m_haveMeta) {
		QMessageBox mbox(QMessageBox::Warning, tr("tileedit: Warning"), tr("No valid metadata information is available"), QMessageBox::Close);
		mbox.exec();
		return;
	}
	else if (!m_metaInfo.lvl) {
		QMessageBox mbox(QMessageBox::Warning, tr("tileedit: Warning"), tr("Metadata do not contain tile index information. Assume single-tile image, to be imported into the current tile location?"), QMessageBox::Yes | QMessageBox::No);
		if (mbox.exec() == QMessageBox::Yes) {
			m_metaInfo.lvl = m_tileedit->m_lvl;
			m_metaInfo.ilat0 = m_tileedit->m_ilat;
			m_metaInfo.ilat1 = m_metaInfo.ilat0 + 1;
			m_metaInfo.ilng0 = m_tileedit->m_ilng;
			m_metaInfo.ilng1 = m_metaInfo.ilng0 + 1;

			ui->labelLvl->setText(QString::number(m_metaInfo.lvl));
			ui->spinIlat0->setValue(m_metaInfo.ilat0);
			ui->spinIlat1->setValue(m_metaInfo.ilat1 - 1);
			ui->spinIlng0->setValue(m_metaInfo.ilng0);
			ui->spinIlng1->setValue(m_metaInfo.ilng1 - 1);
			ui->spinIlat0->setMinimum(m_metaInfo.ilat0);
			ui->spinIlat1->setMinimum(m_metaInfo.ilat0);
			ui->spinIlng0->setMinimum(m_metaInfo.ilng0);
			ui->spinIlng1->setMinimum(m_metaInfo.ilng0);
			ui->spinIlat0->setMaximum(m_metaInfo.ilat1 - 1);
			ui->spinIlat1->setMaximum(m_metaInfo.ilat1 - 1);
			ui->spinIlng0->setMaximum(m_metaInfo.ilng1 - 1);
			ui->spinIlng1->setMaximum(m_metaInfo.ilng1 - 1);
		}
		else {
			return;
		}
	}

	if (m_metaInfo.colormap != 0) {
		QMessageBox mbox(QMessageBox::Warning, tr("tileedit: Warning"), tr("Unsupported colormap specified in metafile. PNG file must be 16-bit greyscale image (colormap 0)."), QMessageBox::Close);
		mbox.exec();
		return;
	}
	ElevTileBlock *eblock = ElevTileBlock::Load(m_metaInfo.lvl, m_metaInfo.ilat0, m_metaInfo.ilat1, m_metaInfo.ilng0, m_metaInfo.ilng1);
	if (!elvread_png(ui->editPath->text().toLatin1(), m_metaInfo, eblock->getData())) {
		QMessageBox mbox(QMessageBox::Warning, tr("tileedit: Warning"), tr("Error reading PNG file"), QMessageBox::Close);
		mbox.exec();
		return;
	}

	int ilat0 = ui->spinIlat0->value();
	int ilat1 = ui->spinIlat1->value() + 1;
	int ilng0 = ui->spinIlng0->value();
	int ilng1 = ui->spinIlng1->value() + 1;

	for (int ilat = ilat0; ilat < ilat1; ilat++)
		for (int ilng = ilng0; ilng < ilng1; ilng++) {
			eblock->syncTile(ilat, ilng);
			ElevTile *etile = (ElevTile*)eblock->_getTile(ilat, ilng);
			if (etile->Level() == etile->subLevel())
				etile->SaveMod();
			else if (!ui->checkSkipMissing->isChecked()) {
				ElevTile *tile = ElevTile::InterpolateFromAncestor(m_metaInfo.lvl, ilat, ilng);
				if (tile) {
					tile->dataChanged();
					tile->Save();
					tile->getData() = etile->getData();
					tile->dataChanged();
					tile->SaveMod();
				}
			}
		}
	if (m_propagationLevel) {
		eblock->mapToAncestors(m_propagationLevel);
	}

	QDialog::accept();
}