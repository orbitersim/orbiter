#include "tileedit.h"
#include <QApplication>

int main(int argc, char *argv[])
{
	QApplication a(argc, argv);
	tileedit w;
	w.show();
	return a.exec();
}
