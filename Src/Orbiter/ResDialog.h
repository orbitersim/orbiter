// not upstream: Qt widgets built from the .rc dialog templates (see OrbiterResource.h)

#ifndef __RESDIALOG_H
#define __RESDIALOG_H

#include "OrbiterResource.h"
#include <QWidget>
#include <QTabBar>
#include <functional>

class QToolButton;

// Orbiter's own resource table, generated from Orbiter.rc
const RESTABLE *OrbiterResources ();

// LoadLibraryEx (LOAD_LIBRARY_AS_DATAFILE) + LoadString counterpart: reads a string of a module's STRINGTABLE
// from the .oapi_strtab section of the module file, without loading (and so running) the module.
// Returns the number of bytes copied (buf is zero-terminated), 0 if the file or the string is not found.
int LoadModuleString (const char *modulefile, int id, char *buf, int buflen);

// routes the events of one or more objects to a handler (window procedure hook); deleted with the first widget
class EventHook: public QObject {
public:
	typedef std::function<bool (QObject *obj, QEvent *event)> Handler;
	EventHook (QWidget *w, Handler h): QObject (w), handler (h) { w->installEventFilter (this); }
	void Attach (QObject *o) { if (o) o->installEventFilter (this); }
protected:
	bool eventFilter (QObject *obj, QEvent *event) override { return handler (obj, event); }
private:
	Handler handler;
};

// msctls_updown32 counterpart: two arrow buttons stepping an integer position, optionally shown in a buddy control
class ResUpDown: public QWidget {
	Q_OBJECT
public:
	ResUpDown (QWidget *parent, bool horizontal, bool setbuddyint, bool wrap);
	void SetBuddy (QWidget *buddy);
	QWidget *Buddy () const { return buddy; }
	void SetRange (int lower, int upper);
	void GetRange (int &lower, int &upper) const { lower = lo; upper = hi; }
	void SetPos (int pos);
	int Pos () const { return pos; }
signals:
	void valueChanged (int pos, int delta);
	void deltaPos (int iDelta); // UDN_DELTAPOS: requested change of the position, sent before it is applied (also at a limit)
protected:
	void resizeEvent (QResizeEvent *event) override;
private:
	void Step (int dir);
	QToolButton *bt[2];
	QWidget *buddy = nullptr;
	int lo = 100, hi = 0, pos = 0; // Win32 default range: 100 .. 0
	bool horz, buddyint, wrap;
};

// SysTabControl32 counterpart: tab strip over a display area the owner fills with pages
class ResTabControl: public QWidget {
	Q_OBJECT
public:
	ResTabControl (QWidget *parent);
	QTabBar *Bar () const { return bar; }
	QRect DisplayRect () const; // TabCtrl_AdjustRect counterpart, in this control's coordinates
protected:
	void resizeEvent (QResizeEvent *event) override;
	void paintEvent (QPaintEvent *event) override;
private:
	QTabBar *bar;
};

#endif // !__RESDIALOG_H
