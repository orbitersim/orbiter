#ifndef TILECANVAS_H
#define TILECANVAS_H

#include <windows.h>
#include "QWidget"
#include "QBoxLayout"
#include "QPen"
#include "tile.h"
#include "tileblock.h"

class TileCanvasOverlay;
class tileedit;

class TileCanvas: public QWidget
{
    Q_OBJECT

    friend class TileCanvasOverlay;

public:
	enum GlyphMode {
		GLYPHMODE_NAVIGATE,
		GLYPHMODE_CROSSHAIR
	};

	explicit TileCanvas(QWidget *parent = 0);
    ~TileCanvas();

	const TileBlock *tileBlock() const{ return m_tileBlock; }
	int idx() const { return m_canvasIdx; }
	void setIdx(int idx) { m_canvasIdx = idx; }
	void setTileedit(tileedit *te) { m_tileedit = te; }
	const Image &getImage() const { return m_img; }
    void resizeEvent(QResizeEvent *event);
    void paintEvent(QPaintEvent *event);
    void enterEvent(QEvent *event);
    void leaveEvent(QEvent *event);
    void mouseMoveEvent(QMouseEvent *event);
	void mousePressEvent(QMouseEvent *event);
    void mouseReleaseEvent(QMouseEvent *event);
    void setTileBlock(const TileBlock *tileBlock, TileMode mode);
	void updateImage();
	void setGlyphMode(GlyphMode mode);
	void setCrosshair(double x, double y, double rad);
	void showOverlay(bool show);

protected:
    void updateGlyph(int mx, int my);
    TileCanvasOverlay *overlay;

private:
    const TileBlock *m_tileBlock;
	TileMode m_tileMode;
	int m_canvasIdx;
	int m_lvl, m_ilat0, m_ilat1, m_ilng0, m_ilng1;
	GlyphMode m_glyphMode;
	tileedit *m_tileedit;
	Image m_img;

signals:
    void tileChanged(int lvl, int ilat, int ilng);
	void tileEntered(TileCanvas *canvas);
	void tileLeft(TileCanvas *canvas);
	void mouseMovedInCanvas(int canvasIdx, QMouseEvent *event);
	void mousePressedInCanvas(int canvasIdx, QMouseEvent *event);
	void mouseReleasedInCanvas(int canvasIdx, QMouseEvent *event);
};

class TileCanvasOverlay: public QWidget
{
    Q_OBJECT

public:
    enum Glyph {
        GLYPH_NONE,
        GLYPH_RECTFULL,
        GLYPH_RECTLEFT,
        GLYPH_RECTRIGHT,
        GLYPH_RECTNW,
        GLYPH_RECTNE,
        GLYPH_RECTSW,
        GLYPH_RECTSE,
        GLYPH_ARROWTOP,
        GLYPH_ARROWBOTTOM,
        GLYPH_ARROWLEFT,
        GLYPH_ARROWRIGHT,
        GLYPH_CROSSCENTER,
		GLYPH_CROSSHAIR
    };

    explicit TileCanvasOverlay(QWidget *parent = 0);
	void setCanvas(TileCanvas *canvas);
    Glyph glyph() const { return m_glyph; }
    void setGlyph(Glyph glyph);
	void setCrosshair(double x, double y, double rad);
    void paintEvent(QPaintEvent *event);
    void mouseMoveEvent(QMouseEvent *event);
	void mousePressEvent(QMouseEvent *event);
    void mouseReleaseEvent(QMouseEvent *event);
	void setTileBlock(const TileBlock *tileBlock) { m_tileBlock = tileBlock; }

private:
    Glyph m_glyph;
    QPen m_penGlyph;
	QPen m_penCrosshair;
	double m_crosshairX, m_crosshairY;
	double m_crosshairR;
	TileCanvas *m_canvas;
	const TileBlock *m_tileBlock;
	static QFont s_font;
};

#endif // TILECANVAS_H

