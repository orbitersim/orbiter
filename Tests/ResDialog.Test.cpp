// not upstream: Orbiter.rc compiled by rc2cpp.py and built into Qt widgets by ResDialog.cpp
#include <catch2/catch_test_macros.hpp>
#include <QApplication>
#include <QCheckBox>
#include <QLabel>
#include <QLineEdit>
#include <QMenu>
#include <QMenuBar>
#include <QMouseEvent>
#include <QScrollBar>
#include <QProgressBar>
#include <QPushButton>
#include <QRadioButton>
#include <QToolButton>
#include <cmath>
#include <cstring>
#include <strings.h>
#include "ResDialog.h"
#include "DlgCtrl.h"
#include "resource.h"
#include "resource2.h"

// stubs: ResDialog.cpp logs through Log.cpp and finds module tables through Util.cpp
static int nwarn = 0;
static int menuModule; // stands in for the handle of a module whose table is ResMenu.Test.rc
extern "C" const RESTABLE *TestMenuResources ();
void *ModuleProc (void *hModule, const char *name)
{
	return (hModule == &menuModule && !strcmp (name, "oapiModuleResources") ? (void*)TestMenuResources : nullptr);
}
void LogOut_Warning (const char*, const char*, int, const char*, ...) { nwarn++; }

static QApplication &App ()
{
	static int argc = 1;
	static char name[] = "ResDialog.Test", *argv[] = {name, nullptr};
	if (qEnvironmentVariableIsEmpty ("QT_QPA_PLATFORM")) qputenv ("QT_QPA_PLATFORM", "offscreen");
	static QApplication app (argc, argv);
	return app;
}

TEST_CASE("Orbiter.rc compiles to a resource table", "[resdialog]")
{
	App();
	const RESTABLE *t = oapiResourceTable (nullptr);
	REQUIRE(t);
	REQUIRE(t->ndlg > 50);
	REQUIRE(oapiFindResDialog (nullptr, IDD_MAIN));
	REQUIRE(oapiFindResDialog (nullptr, IDD_OPTIONS_VISUAL));
	REQUIRE(!oapiFindResDialog (nullptr, 99999));
	for (size_t i = 0; i < t->nimg; i++) {
		QImage *img = oapiLoadResImage (nullptr, t->img[i].id);
		INFO(t->img[i].name);
		REQUIRE(img);
		REQUIRE(!img->isNull());
		delete img;
	}
}

TEST_CASE("every dialog template builds all its controls", "[resdialog]")
{
	App();
	const RESTABLE *t = oapiResourceTable (nullptr);
	for (size_t i = 0; i < t->ndlg; i++) {
		const RESDIALOG *d = t->dlg + i;
		INFO(d->name);
		QWidget *w = oapiCreateResDialog (nullptr, d->id, nullptr);
		REQUIRE(w);
		REQUIRE(oapiResId (w) == d->id);
		int n = 0;
		for (QObject *o : w->children())
			if (o->isWidgetType() && o->property ("resId").isValid()) n++;
		REQUIRE(n == d->nctrl);
		delete w;
	}
}

TEST_CASE("IDD_MAIN matches its template", "[resdialog]")
{
	App();
	QWidget *dlg = oapiCreateResDialog (nullptr, IDD_MAIN, nullptr);
	REQUIRE(dlg);
	REQUIRE(dlg->windowTitle() == "OpenOrbiter Launchpad");
	double bx = dlg->property ("resBaseX").toDouble();
	int by = dlg->property ("resBaseY").toInt();
	REQUIRE(bx > 3.0);
	REQUIRE(dlg->width() == (int)std::lround (LAUNCHPAD_WIN_WIDTH*bx/4.0));
	REQUIRE(dlg->height() == (int)std::lround (333*by/8.0));
	QPushButton *launch = DlgItem<QPushButton> (dlg, IDLAUNCH);
	REQUIRE(launch);
	REQUIRE(launch->isDefault());
	REQUIRE(launch->text() == "&Launch Orbiter");
	QLabel *logo = DlgItem<QLabel> (dlg, IDC_LOGO);
	REQUIRE(logo);
	REQUIRE(!logo->pixmap().isNull());
	REQUIRE(!DlgItem<QCheckBox> (dlg, IDLAUNCH));
	delete dlg;
}

TEST_CASE("radio buttons group by WS_GROUP", "[resdialog]")
{
	App();
	QWidget *dlg = oapiCreateResDialog (nullptr, IDD_RECPLAY, nullptr);
	QRadioButton *att[2] = {DlgItem<QRadioButton> (dlg, IDC_REC_ATTECL), DlgItem<QRadioButton> (dlg, IDC_REC_ATTHOR)};
	QRadioButton *pos = DlgItem<QRadioButton> (dlg, IDC_REC_POSECL);
	REQUIRE((att[0] && att[1] && pos));
	pos->setChecked (true);
	att[0]->setChecked (true);
	att[1]->setChecked (true);
	REQUIRE(!att[0]->isChecked());
	REQUIRE(pos->isChecked());
	delete dlg;
}

TEST_CASE("labels grow into free space, not over their neighbours", "[resdialog]")
{
	App();
	QWidget *dlg = oapiCreateResDialog (nullptr, IDD_OPTIONS_VISUAL, nullptr);
	QCheckBox *cb = DlgItem<QCheckBox> (dlg, IDC_OPT_VIS_REFWATER);
	QCheckBox *next = DlgItem<QCheckBox> (dlg, IDC_OPT_VIS_RIPPLE);
	REQUIRE((cb && next));
	REQUIRE(cb->geometry().right() < next->geometry().left());
	REQUIRE((cb->width() >= cb->sizeHint().width() || cb->geometry().right() == next->geometry().left() - 2));
	delete dlg;
}

TEST_CASE("up-down control steps toward its upper limit", "[resdialog]")
{
	App();
	QWidget parent;
	QLineEdit *buddy = new QLineEdit (&parent);
	ResUpDown *ud = new ResUpDown (&parent, false, true, false);
	ud->SetBuddy (buddy);
	ud->SetRange (0, 10);
	ud->SetPos (5);
	REQUIRE(buddy->text() == "5");
	int last = 0;
	QObject::connect (ud, &ResUpDown::valueChanged, [&last](int, int delta) { last = delta; });
	QList<QToolButton*> bt = ud->findChildren<QToolButton*>();
	REQUIRE(bt.size() == 2);
	bt[0]->click();
	REQUIRE(ud->Pos() == 6);
	REQUIRE(last == 1);
	buddy->setText ("10");
	bt[0]->click();
	REQUIRE(ud->Pos() == 10);
	ud->SetRange (10, 0); // inverted range: "up" counts down
	ud->SetPos (5);
	bt[0]->click();
	REQUIRE(ud->Pos() == 4);
	REQUIRE(last == -1);
	ResUpDown *w = new ResUpDown (&parent, true, false, true);
	w->SetRange (0, 3);
	w->SetPos (3);
	w->findChildren<QToolButton*>()[0]->click();
	REQUIRE(w->Pos() == 0);
}

TEST_CASE("registered control classes replace placeholders", "[resdialog]")
{
	App();
	const RESTABLE *t = oapiResourceTable (nullptr);
	const RESDIALOG *dg = nullptr;
	int id = 0;
	for (size_t i = 0; i < t->ndlg && !dg; i++)
		for (int j = 0; j < t->dlg[i].nctrl; j++)
			if (t->dlg[i].ctrl[j].cls && !strcasecmp (t->dlg[i].ctrl[j].cls, "OrbiterCtrl_Gauge")) {
				dg = t->dlg + i, id = t->dlg[i].ctrl[j].id;
				break;
			}
	REQUIRE(dg);
	oapiRegisterResControl (nullptr, "orbiterctrl_gauge", [](const RESCONTROL*, QWidget *parent) -> QWidget* { return new QProgressBar (parent); });
	QWidget *dlg = oapiCreateResDialog (nullptr, dg->id, nullptr);
	REQUIRE(DlgItem<QProgressBar> (dlg, id));
	delete dlg;
	oapiUnregisterResControl (nullptr, "OrbiterCtrl_Gauge");
	dlg = oapiCreateResDialog (nullptr, dg->id, nullptr);
	REQUIRE(!DlgItem<QProgressBar> (dlg, id));
	delete dlg;
}

TEST_CASE("DlgCtrl gauge in a dialog template", "[resdialog][dlgctrl]")
{
	App();
	oapiRegisterCustomControls (nullptr);
	QWidget *dlg = oapiCreateResDialog (nullptr, IDD_OPTIONS_JOYSTICK, nullptr);
	GaugeCtrl *g = DlgItem<GaugeCtrl> (dlg, IDC_OPT_JOY_SAT);
	REQUIRE(g);
	GAUGEPARAM gp = {0, 10, GAUGEPARAM::LEFT, GAUGEPARAM::BLACK};
	oapiSetGaugeParams (g, &gp);
	REQUIRE(oapiSetGaugePos (g, 20) == 10);
	REQUIRE(oapiIncGaugePos (g, -3) == 7);
	REQUIRE(oapiGetGaugePos (g) == 7);
	int req = -1, pos = -1;
	QObject::connect (g, &GaugeCtrl::scrolled, [&](int r, int p) { req = r, pos = p; });
	QPointF right (g->width() - 2, g->height()/2), mid (g->width()/2 + 1, g->height()/2);
	QMouseEvent press (QEvent::MouseButtonPress, right, g->mapToGlobal (right), Qt::LeftButton, Qt::LeftButton, Qt::NoModifier);
	QMouseEvent release (QEvent::MouseButtonRelease, right, g->mapToGlobal (right), Qt::LeftButton, Qt::NoButton, Qt::NoModifier);
	QApplication::sendEvent (g, &press);
	QApplication::sendEvent (g, &release);
	REQUIRE(req == GAUGE_LINEINC);
	REQUIRE(pos == 8);
	QMouseEvent drag (QEvent::MouseButtonPress, mid, g->mapToGlobal (mid), Qt::LeftButton, Qt::LeftButton, Qt::NoModifier);
	QApplication::sendEvent (g, &drag);
	QApplication::sendEvent (g, &release);
	REQUIRE(req == GAUGE_THUMBTRACK);
	REQUIRE(pos == 5);
	REQUIRE(!g->grab().isNull());
	delete dlg;
	oapiUnregisterCustomControls (nullptr);
}

TEST_CASE("DlgCtrl switch and property list", "[resdialog][dlgctrl]")
{
	App();
	oapiRegisterCustomControls (nullptr);
	QWidget parent;
	SwitchCtrl *sw = new SwitchCtrl (&parent);
	sw->resize (16, 32);
	SWITCHPARAM sp = {SWITCHPARAM::THREESTATE, SWITCHPARAM::VERTICAL};
	oapiSetSwitchParams (sw, &sp, true);
	int state = -1;
	QObject::connect (sw, &SwitchCtrl::clicked, [&state](int s) { state = s; });
	QPointF lower (8, 28);
	QMouseEvent press (QEvent::MouseButtonPress, lower, sw->mapToGlobal (lower), Qt::LeftButton, Qt::LeftButton, Qt::NoModifier);
	QApplication::sendEvent (sw, &press);
	REQUIRE(state == 2);
	REQUIRE(oapiGetSwitchState (sw) == 2);
	REQUIRE(oapiSetSwitchState (sw, 1, true) == 1);
	REQUIRE(!sw->grab().isNull());

	PropertyListCtrl *pl = new PropertyListCtrl (&parent);
	pl->setProperty ("resId", 77);
	pl->resize (200, 60);
	PropertyList list;
	list.OnInitDialog (&parent, 77);
	PropertyGroup *grp = list.AppendGroup();
	grp->SetTitle ("Group");
	for (int i = 0; i < 8; i++) {
		PropertyItem *it = list.AppendItem (grp);
		it->SetLabel ("label");
		it->SetValue ("value");
	}
	REQUIRE(grp->ItemCount() == 8);
	REQUIRE(pl->verticalScrollBar()->maximum() > 0);
	list.Update();
	REQUIRE(!pl->grab().isNull());
	REQUIRE(list.ExpandGroup (grp, false));
	REQUIRE(pl->verticalScrollBar()->maximum() == 0);
	oapiUnregisterCustomControls (nullptr);
}

TEST_CASE("user-defined resources and string tables", "[resdialog]")
{
	App();
	const RESDATA *txt = oapiFindResData (nullptr, "text", IDT_DISCLAIMER);
	REQUIRE(txt);
	REQUIRE(txt->size > 100);
	REQUIRE(txt->data[txt->size] == 0);
	const RESDATA *img = oapiFindResData (nullptr, "IMAGE", IDR_IMAGE1);
	REQUIRE(img);
	QImage splash;
	REQUIRE(splash.loadFromData (img->data, img->size));
	REQUIRE(!oapiFindResData (nullptr, "IMAGE", IDT_DISCLAIMER));
	char buf[32];
	REQUIRE(oapiLoadResString (nullptr, IDS_TABMODULE, buf, 32) == 7);
	REQUIRE(std::string (buf) == "Modules");
	REQUIRE(oapiLoadResString (nullptr, IDS_TABMODULE, buf, 4) == 3);
	REQUIRE(std::string (buf) == "Mod");
	REQUIRE(oapiLoadResString (nullptr, 99999, buf, 32) == 0);
}

TEST_CASE("module strings are read from the file without loading it", "[resdialog]")
{
	char buf[64];
	REQUIRE(LoadModuleString ("/proc/self/exe", IDS_TABVISUAL, buf, 64) == 14);
	REQUIRE(std::string (buf) == "Visual effects");
	REQUIRE(LoadModuleString ("/proc/self/exe", 99999, buf, 64) == 0);
	REQUIRE(LoadModuleString ("/nonexistent.so", IDS_TABVISUAL, buf, 64) == 0);
}

TEST_CASE("menu templates build menu bars", "[resdialog]")
{
	App();
	void *hMod = &menuModule;
	const RESMENU *rm = oapiFindResMenu (hMod, 200);
	REQUIRE(rm);
	REQUIRE(rm->nitem == 9);
	REQUIRE(!oapiFindResMenu (nullptr, 200));
	QWidget *dlg = oapiCreateResDialog (hMod, 202, nullptr);
	REQUIRE(dlg);
	QMenuBar *bar = dlg->findChild<QMenuBar*>();
	REQUIRE(bar);
	QLineEdit *edit = DlgItem<QLineEdit> (dlg, 400);
	REQUIRE(edit);
	REQUIRE(edit->y() >= bar->height()); // controls sit below the menu
	QList<QAction*> top = bar->actions();
	REQUIRE(top.size() == 2);
	REQUIRE(top[0]->text() == "&File");
	REQUIRE(!top[1]->isEnabled()); // INACTIVE popup
	QList<QAction*> file = top[0]->menu()->actions();
	REQUIRE(file.size() == 4);
	REQUIRE(file[0]->text() == "&Open\tCtrl+O");
	REQUIRE(!file[1]->isEnabled());
	REQUIRE(file[2]->isSeparator());
	REQUIRE(file[3]->menu());
	REQUIRE(file[3]->menu()->actions().size() == 1);
	REQUIRE(top[1]->menu()->actions()[1]->isChecked());
	int got = 0, code = -1;
	oapiConnectDlgCommands (dlg, [&](int id, int c, QWidget *w) { if (!w) got = id, code = c; });
	file[3]->menu()->actions()[0]->trigger();
	REQUIRE(got == 302);
	REQUIRE(code == RESN_CLICKED);
	file[0]->trigger();
	REQUIRE(got == 300);
	delete dlg;
	QWidget w;
	w.resize (100, 50);
	QMenuBar *ex = oapiCreateResMenu (hMod, 201, &w);
	REQUIRE(ex);
	REQUIRE(w.height() == 50 + ex->height());
	QList<QAction*> exitems = ex->actions()[0]->menu()->actions();
	REQUIRE(exitems.size() == 3);
	REQUIRE(exitems[1]->isSeparator());
	REQUIRE((exitems[2]->isChecked() && !exitems[2]->isEnabled()));
}
