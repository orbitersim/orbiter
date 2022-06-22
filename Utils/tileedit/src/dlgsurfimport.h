#ifndef DLGSURFIMPORT_H
#define DLGSURFIMPORT_H

#include "dxt_io.h"
#include <QDialog>

namespace Ui {
	class DlgSurfImport;
}

class tileedit;

class DlgSurfImport : public QDialog
{
	Q_OBJECT

public:
	DlgSurfImport(tileedit *parent);

public slots:
	void onOpenFileDialog();
	void onOpenMetaFileDialog();
	void onParamFromMeta();
	void onParamFromUser();
	void onMetaFileChanged(const QString&);
	void onLvl(int);
	void onPropagateChanges(int);
	void accept();

protected:
	bool scanMetaFile(const char *fname, SurfPatchMetaInfo &meta);

private:
	Ui::DlgSurfImport *ui;
	tileedit *m_tileedit;
	SurfPatchMetaInfo m_metaInfo;
	bool m_pathEdited, m_metaEdited;
	bool m_haveMeta;
};

#endif // !DLGSURFIMPORT_H
