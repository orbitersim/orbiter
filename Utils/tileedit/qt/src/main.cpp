#include "tileedit.h"
#include <QtWidgets/QApplication>
#include <QtPlugin>

#ifdef STATIC
#ifndef __linux__
Q_IMPORT_PLUGIN(QWindowsIntegrationPlugin);
#else // __linux__
Q_IMPORT_PLUGIN(QXcbIntegrationPlugin); // static Windows platform plugin -> xcb
#endif // __linux__
#endif

int main(int argc, char *argv[])
{
	QApplication a(argc, argv);
	tileedit w;
	w.show();
	return a.exec();
}
