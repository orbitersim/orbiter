// not upstream: the .ui dialogs place their group boxes at fixed heights made for Windows fonts; this grows them to fit here
#ifndef FITDIALOG_H
#define FITDIALOG_H

#include <QLayout>
#include <QWidget>
#include <algorithm>

// fixed-geometry children that hold a layout grow to their size hint; the widgets below them move down, the dialog grows
inline void FitDialog (QWidget *dlg)
{
	QList<QWidget*> kids;
	for (QObject *o : dlg->children()) {
		QWidget *w = qobject_cast<QWidget*> (o);
		if (w && !w->isWindow()) kids << w;
	}
	std::sort (kids.begin(), kids.end(), [](QWidget *a, QWidget *b) { return a->y() < b->y(); });
	for (QWidget *g : kids) {
		if (!g->layout()) continue;
		int d = g->sizeHint().height() - g->height();
		if (d <= 0) continue;
		int y0 = g->geometry().bottom();
		g->resize (g->width(), g->height() + d);
		for (QWidget *w : kids)
			if (w != g && w->y() > y0) w->move (w->x(), w->y() + d);
		if (dlg->minimumHeight() == dlg->maximumHeight()) dlg->setFixedHeight (dlg->height() + d);
		else dlg->resize (dlg->width(), dlg->height() + d);
	}
}

#endif // !FITDIALOG_H
