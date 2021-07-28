#ifndef COLORBAR_H
#define COLORBAR_H

#include "QWidget"
#include "QPen"
#include "cmap.h"
#include "elevtile.h"

class ColorbarOverlay;

class Colorbar : public QWidget
{
	Q_OBJECT

public:
	explicit Colorbar(QWidget *parent = 0);
	~Colorbar();
	void setTileMode(TileMode mode);
	void setElevDisplayParam(const ElevDisplayParam &elevDisplayParam);
	void displayParamChanged();
	void setScalarValue(double val);
	void setRGBValue(BYTE r, BYTE g, BYTE b);

protected:
	void paintEvent(QPaintEvent *event);
	void resizeEvent(QResizeEvent *event);

private:
	const ElevDisplayParam *m_elevDisplayParam;
	ColorbarOverlay *m_overlay;
	TileMode m_mode;
};


class ColorbarOverlay : public QWidget
{
	Q_OBJECT

public:
	explicit ColorbarOverlay(QWidget *parent = 0);
	void setTileMode(TileMode mode);
	void setRange(double vmin, double vmax);
	void setScalarValue(double val);
	void setRGBValue(BYTE r, BYTE g, BYTE b);

protected:
	void paintEvent(QPaintEvent *event);

private:
	TileMode m_mode;
	double m_vmin, m_vmax;
	double m_val;
	BYTE m_r, m_g, m_b;
	QPen m_penIndicator0;
	QPen m_penIndicator1;
};

#endif // !COLORBAR_H