#include "tileedit.h"
#include <QtWidgets/QApplication>
#include <QtPlugin>

#ifdef STATIC
Q_IMPORT_PLUGIN(QWindowsIntegrationPlugin);
#endif

int main(int argc, char *argv[])
{
	QApplication a(argc, argv);
	tileedit w;
	w.show();
	return a.exec();
}
