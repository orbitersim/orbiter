#include "colorbar.h"
#include "cmap.h"
#include "QPainter"
#include "QResizeEvent"

Colorbar::Colorbar(QWidget *parent)
	: QWidget(parent)
{
	m_elevDisplayParam = 0;
	m_overlay = new ColorbarOverlay(this);
	m_mode = TILEMODE_NONE;
}

Colorbar::~Colorbar()
{
	delete m_overlay;
}

void Colorbar::setTileMode(TileMode mode)
{
	if (mode != m_mode) {
		m_mode = mode;
		update();
	}
	m_overlay->setTileMode(mode);
}

void Colorbar::setElevDisplayParam(const ElevDisplayParam &elevDisplayParam)
{
	m_elevDisplayParam = &elevDisplayParam;
	m_overlay->setRange(elevDisplayParam.rangeMin, elevDisplayParam.rangeMax);
}

void Colorbar::displayParamChanged()
{
	m_overlay->setRange(m_elevDisplayParam->rangeMin, m_elevDisplayParam->rangeMax);
	update();
}

void Colorbar::setScalarValue(double val)
{
	m_overlay->setScalarValue(val);
}

void Colorbar::setRGBValue(BYTE r, BYTE g, BYTE b)
{
	m_overlay->setRGBValue(r, g, b);
}

void Colorbar::paintEvent(QPaintEvent *event)
{
	QPainter painter(this);

	QFontMetrics fm = painter.fontMetrics();
	int textHeight = fm.height();

	QRect cbRect(rect().left(), rect().top(), rect().width(), rect().height() - textHeight);

	if (m_mode == TILEMODE_ELEVATION || m_mode == TILEMODE_ELEVMOD) {
		if (m_elevDisplayParam) {
			const Cmap &cm = cmap(m_elevDisplayParam->cmName);
			DWORD data[256];
			memcpy(data, cm, 256 * sizeof(DWORD));
			for (int i = 0; i < 256; i++)
				data[i] |= 0xff000000;
			QImage qimg((BYTE*)data, 256, 1, QImage::Format_ARGB32);
			painter.drawImage(cbRect, qimg);
		}
		else {
			QBrush brush(QColor(255, 255, 255));
			painter.setBrush(brush);
			painter.drawRect(cbRect);
		}
	}
	else if (m_mode == TILEMODE_SURFACE || m_mode == TILEMODE_NIGHTLIGHT) {
		DWORD data[256 * 3];
		for (int i = 0; i < 256; i++) {
			data[i] = 0xff000000 | (i << 16);
			data[i + 256] = 0xff000000 | (i << 8);
			data[i + 256 * 2] = 0xff000000 | i;
		}
		QImage qimg((BYTE*)data, 256, 3, QImage::Format_ARGB32);
		painter.drawImage(cbRect, qimg);
	}
	else if (m_mode == TILEMODE_WATERMASK) {
		painter.setPen(QPen("black"));
		painter.setBrush(QColor("black"));
		painter.drawRect(0, 0, cbRect.width() / 2 - 1, cbRect.height() - 1);
		painter.setBrush(QColor("white"));
		painter.drawRect(cbRect.width() / 2, 0, cbRect.width() - cbRect.width() / 2 - 1, cbRect.height() - 1);
	}
}

void Colorbar::resizeEvent(QResizeEvent *event)
{
	m_overlay->resize(event->size());
}



ColorbarOverlay::ColorbarOverlay(QWidget *parent)
	: QWidget(parent)
{
	m_vmin = 0.0;
	m_vmax = 1.0;
	m_val = 0.0;
	m_mode = TILEMODE_NONE;

	m_penIndicator0.setColor(QColor(255, 0, 0));
	m_penIndicator0.setWidth(1);
	m_penIndicator0.setStyle(Qt::SolidLine);
	m_penIndicator1.setColor(QColor(255, 255, 255));
	m_penIndicator1.setWidth(1);
	m_penIndicator1.setStyle(Qt::SolidLine);
}

void ColorbarOverlay::setTileMode(TileMode mode)
{
	if (mode != m_mode) {
		m_mode = mode;
		update();
	}
}

void ColorbarOverlay::setRange(double vmin, double vmax)
{
	m_vmin = vmin;
	m_vmax = vmax;
	update();
}

void ColorbarOverlay::setScalarValue(double val)
{
	m_val = val;
	if (m_mode == TILEMODE_ELEVATION || m_mode == TILEMODE_ELEVMOD || m_mode == TILEMODE_WATERMASK)
		update();
}

void ColorbarOverlay::setRGBValue(BYTE r, BYTE g, BYTE b)
{
	m_r = r;
	m_g = g;
	m_b = b;
	if (m_mode == TILEMODE_SURFACE || m_mode == TILEMODE_NIGHTLIGHT)
		update();
}

void ColorbarOverlay::paintEvent(QPaintEvent *event)
{
	QPainter painter(this);

	QFontMetrics fm = painter.fontMetrics();
	int textHeight = fm.height();

	painter.fillRect(rect(), QColor(0, 0, 0, 0));
	int w = rect().width();
	int h = rect().height();
	int cbh = h - textHeight;

	char cbuf[1024];

	if (m_mode == TILEMODE_ELEVATION || m_mode == TILEMODE_ELEVMOD) {
		if (m_val != DBL_MAX) {
			int x = max(1, min(w - 2, (m_val - m_vmin) / (m_vmax - m_vmin) * w));
			painter.setPen(m_penIndicator0);
			painter.drawLine(x, 0, x, cbh - 1);
			painter.setPen(m_penIndicator1);
			painter.drawLine(x - 1, 0, x - 1, cbh - 1);
			painter.drawLine(x + 1, 0, x + 1, cbh - 1);
		}
		painter.setPen("black");
		sprintf(cbuf, "%+0.1lf m", m_vmin);
		painter.drawText(0, h-2, cbuf);
		sprintf(cbuf, "%+0.1lf m", m_vmax);
		QString qs(cbuf);
		painter.drawText(w - fm.width(qs), h-2, qs);
		if (m_val != DBL_MAX)
			sprintf(cbuf, "%+0.1lf m", m_val);
		else
			strcpy(cbuf, "N/A");
		QString qv(cbuf);
		painter.drawText((w - fm.width(qv)) / 2, h-2, qv);
	}
	else if (m_mode == TILEMODE_SURFACE || m_mode == TILEMODE_NIGHTLIGHT) {
		int y0 = 0, y1 = cbh / 3, y2 = (cbh * 2) / 3, y3 = cbh;
		int xr = ((2*m_r+1) * w) / 512, xg = ((2*m_g+1) * w) / 512, xb = ((2*m_b+1) * w) / 512;
		painter.setPen(m_penIndicator0);
		painter.drawLine(xr, y0, xr, y1 - 1);
		painter.drawLine(xg, y1, xg, y2 - 1);
		painter.drawLine(xb, y2, xb, y3 - 1);
		painter.setPen(m_penIndicator1);
		painter.drawLine(xr - 1, y0, xr - 1, y1 - 1);
		painter.drawLine(xr + 1, y0, xr + 1, y1 - 1);
		painter.drawLine(xg - 1, y1, xg - 1, y2 - 1);
		painter.drawLine(xg + 1, y1, xg + 1, y2 - 1);
		painter.drawLine(xb - 1, y2, xb - 1, y3 - 1);
		painter.drawLine(xb + 1, y2, xb + 1, y3 - 1);

		painter.setPen("black");
		QString qsmin = QString::number(0);
		painter.drawText(0, h-2, qsmin);
		QString qsmax = QString::number(255);
		painter.drawText(w - fm.width(qsmax), h-2, qsmax);

		sprintf(cbuf, "%d / %d / %d", m_r, m_g, m_b);
		QString qrgb(cbuf);
		int x0 = (w - fm.width(qrgb)) / 2;
		painter.setPen(QPen("red"));
		sprintf(cbuf, "%d", m_r);
		QString qr(cbuf);
		painter.drawText(x0, h - 2, qr);
		x0 += fm.width(qr);
		painter.setPen(QPen("black"));
		QString qdash(" / ");
		painter.drawText(x0, h - 2, qdash);
		x0 += fm.width(qdash);
		painter.setPen(QPen("green"));
		sprintf(cbuf, "%d", m_g);
		QString qg(cbuf);
		painter.drawText(x0, h - 2, qg);
		x0 += fm.width(qg);
		painter.setPen(QPen("black"));
		painter.drawText(x0, h - 2, qdash);
		x0 += fm.width(qdash);
		painter.setPen(QPen("blue"));
		sprintf(cbuf, "%d", m_b);
		QString qb(cbuf);
		painter.drawText(x0, h - 2, qb);
	}
	else if (m_mode == TILEMODE_WATERMASK) {
		painter.setPen(m_penIndicator0);
		bool isWater = (m_val == 0.0);
		if (isWater) {
			painter.drawRect(2, 2, w / 2 - 5, cbh - 5);
			painter.setPen(m_penIndicator1);
			painter.drawRect(1, 1, w / 2 - 3, cbh - 3);
			painter.drawRect(3, 3, w / 2 - 7, cbh - 7);
		} 
		else
			painter.drawRect(w / 2 + 2, 2, w - w / 2 - 5, cbh - 5);

		QString s(isWater ? "Water (specular)" : "Land (diffuse)");
		painter.setPen(QPen("black"));
		painter.drawText((w - fm.width(s)) / 2, h - 2, s);
	}
}