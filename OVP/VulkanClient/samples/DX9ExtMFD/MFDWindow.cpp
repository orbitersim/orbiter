// ==============================================================          
// Copyright (C) 2006-2026 Martin Schweiger
// Licensed under the MIT License
// ==============================================================

#include "MFDWindow.h"
#include "resource.h"
#include "OrbiterResource.h"
#include <QFont>
#include <QKeyEvent>
#include <QResizeEvent>
#include <QMouseEvent>
#include <QPainter>
#include <QVariant>
#include <QWindow>
#include <cstring>
#include <functional>
#include <stdio.h> // temporary

using std::min;
using std::max;

#define IDSTICK 999

#define WMSZ_RIGHT       2 // not upstream: winuser.h WM_SIZING edges
#define WMSZ_BOTTOM      6
#define WMSZ_BOTTOMRIGHT 8

// ==============================================================
// prototype definitions

void DlgProc (QWidget *hDlg, void *context);

// not upstream: the Vulkan window inside the display control (RegisterSwap presents into it)
static QWindow *DisplayWindow (QWidget *hDsp)
{
	return (QWindow*)hDsp->property ("DisplayWindow").value<void*>();
}

// not upstream: SetWindowPos (hWnd, NULL, x, y, w, h, SWP_SHOWWINDOW)
static void PlaceWindow (QWidget *hWnd, int x, int y, int w, int h)
{
	hWnd->setGeometry (x, y, w, h);
	hWnd->show();
}

// not upstream: routes the dialog's events to a handler (WM_SIZE, DefDlgProc's WM_CLOSE/Esc)
class DlgEvents: public QObject {
public:
	DlgEvents (QWidget *hWnd, std::function<bool(QEvent*)> handler): QObject (hWnd), handler (handler) { hWnd->installEventFilter (this); }
	bool eventFilter (QObject *o, QEvent *e) override { return handler (e); }
private:
	std::function<bool(QEvent*)> handler;
};

// ==============================================================
// class MFDWindow

MFDWindow::MFDWindow (void *_hInst, const MFDSPEC &spec): ExternMFD (spec), hInst(_hInst)
{
	hSwap = NULL;
	hBtnFnt = 0;
	fnth = 0;
	vstick = false;
	bFailed = false;

	oapiOpenDialogEx (hInst, IDD_MFD, DlgProc, DLG_ALLOWMULTI|DLG_CAPTIONCLOSE|DLG_CAPTIONHELP, this);
}

MFDWindow::~MFDWindow ()
{
	gcCore *pCore = gcGetCoreInterface();
	if (pCore && hSwap) pCore->ReleaseSwap(hSwap);
	oapiCloseDialog (hDlg);
	if (hBtnFnt) delete hBtnFnt;
}

void MFDWindow::Initialise (QWidget *_hDlg)
{
	extern QImage *g_hPin;

	hDlg = _hDlg;
	hDsp = oapiResDlgItem (hDlg, IDC_DISPLAY);
	
	for (int i = 0; i < 16; i++)
		oapiResDlgItem (hDlg, IDC_BUTTON1+i)->setProperty ("GWLP_USERDATA", i);

	oapiAddTitleButton (IDSTICK, g_hPin, DLG_CB_TWOSTATE);
	SetTitle ();
	gap = 3;

	QRect g = hDlg->frameGeometry(); // GetWindowRect
	wr = { g.left(), g.top(), g.left() + g.width(), g.top() + g.height() };
	CheckAspect(&wr, 0);
	Resize();
}

void MFDWindow::SetVessel (OBJHANDLE hV)
{
	ExternMFD::SetVessel (hV);
	SetTitle ();
}

void MFDWindow::SetTitle ()
{
	char cbuf[256] = "Vulkan MFD ["; // not upstream: Vulkan in place of DX9
	oapiGetObjectName (hVessel, cbuf+12, 200);
	strncat (cbuf, "]", 250 - strlen (cbuf) - 1);
	oapiSetDlgText (hDlg, cbuf);		//<<--- Very odd runtime check failure here why now ???  :jarmonik 5-Aug-2021
}

void MFDWindow::CheckAspect(LPRECT r, DWORD q)
{
	RECT c,b;

	QRect cg = hDlg->rect(), bg = hDlg->frameGeometry();
	c = { 0, 0, cg.width(), cg.height() }; // GetClientRect
	b = { bg.left(), bg.top(), bg.left() + bg.width(), bg.top() + bg.height() }; // GetWindowRect

	int ew = (b.right - b.left) - (c.right - c.left);
	int eh = (b.bottom - b.top) - (c.bottom - c.top);

	int bw = (c.right * 35) / 300;

	BW = max(30, min(60, bw));
	BH = max(15, min(40, (bw * 2) / 3));

	int dspw = (r->right - r->left) - (2 * (BW + gap) + 2 * gap + ew);
	int dsph = (r->bottom - r->top) - ((BH + 2 * gap) + 2 * gap + ew);

	ds = (dspw + dsph) / 2;

	int w = (gap + BW) * 2 + ds + gap * 2 + ew;
	int h = (gap + BH) + ds + gap * 2 + eh;

	switch (q) {
	case WMSZ_BOTTOM:
	case WMSZ_RIGHT:
	case WMSZ_BOTTOMRIGHT:
		r->right = r->left + w;
		r->bottom = r->top + h;
		break;
	default:
		r->left = r->right - w;
		r->top = r->bottom - h;
		break;
	}
	return;
}

void MFDWindow::Resize()
{
	RECT r = { 0, 0, hDlg->width(), hDlg->height() }; // GetClientRect
	int bw = (r.right*35)/300;
	BW = max (30, min (60, bw));
	BH = max (15, min (40, (bw*2)/3));
	ds = r.right - (2 * (BW + gap) + 2 * gap);
	
	int fh = BW/3;

	if (fh != fnth) {
		if (hBtnFnt) delete hBtnFnt;
		hBtnFnt = new QFont ("Arial"); // CreateFont (fh, ..., "Arial")
		hBtnFnt->setPixelSize (fnth = fh);
	}

	DH = DW = ds;

	PlaceWindow(hDsp, BW + gap * 2, gap, ds, ds);
	DisplayWindow(hDsp)->resize(ds, ds); // not upstream: SetWindowPos sized the child window at once, the container passes its size on only with a later resize event (the swap chain was 1x1)
	
	int x1 = gap;
	int x2 = r.right-gap-BW;
	int dy = (DH*2)/13, y0 = DH/7, y1 = r.top+y0-BH/2;
	for (int i = 0; i < 6; i++) {
		PlaceWindow (oapiResDlgItem (hDlg, IDC_BUTTON1+i), x1, y1+i*dy, BW, BH);
		PlaceWindow (oapiResDlgItem (hDlg, IDC_BUTTON7+i), x2, y1+i*dy, BW, BH);
	}
	y1 = r.top+DH+gap*2;
	PlaceWindow (oapiResDlgItem (hDlg, IDC_BUTTON_DRV), r.left + DW/2 + (BW*12)/4, y1, BW, BH);
	PlaceWindow (oapiResDlgItem (hDlg, IDC_BUTTON_PWR), r.left + DW/2 - (BW*7)/4, y1, BW, BH);
	PlaceWindow (oapiResDlgItem (hDlg, IDC_BUTTON_SEL), r.left + DW/2 - BW/2, y1, BW, BH);
	PlaceWindow (oapiResDlgItem (hDlg, IDC_BUTTON_MNU), r.left + DW/2 + (BW*3)/4, y1, BW, BH);

	if (!bFailed) {
		gcCore *pCore = gcGetCoreInterface();
		if (pCore) hSwap = pCore->RegisterSwap(DisplayWindow(hDsp), hSwap, 0);
	}

	MFDSPEC spec = {{0,0,DW,DH},6,6,y0,dy};
	ExternMFD::Resize (spec);

	hDlg->update(); // InvalidateRect
}


// RepaintDisplay left out: BeginPaint/EndPaint only validated the display, which is a Vulkan window now

void MFDWindow::RepaintButton (QWidget *hWnd)
{
	int id = hWnd->property ("GWLP_USERDATA").toInt();
	QPainter painter (hWnd); // BeginPaint
	QPainter *hDC = &painter;
	hDC->setPen (QPen (Qt::black, 0)); // BLACK_PEN
	hDC->setBrush (Qt::white); // the DC's default WHITE_BRUSH
	hDC->drawRect (0, 0, BW - 1, BH - 1); // Rectangle (0, 0, BW, BH)
	QColor textcol (Qt::black); // SetTextAlign TA_CENTER: see TextOut below
	const char *label;
	if (id < 12) {
		label = GetButtonLabel (id);
	} else {
		static const char *lbl[3] = {"PWR","SEL","MNU"};
		static const char *drv[2] = {"N/A","N/A"};
		if (id == 15) label = drv[0];
		else {
			label = lbl[id - 12];
			if (id == 12) textcol = QColor (0xFF, 0x00, 0x00); // SetTextColor 0x0000FF
		}
	}
	if (label) {
		hDC->setBackgroundMode (Qt::TransparentMode); // SetBkMode TRANSPARENT
		QFont pFont = hDC->font();
		hDC->setFont (*hBtnFnt);
		QString s = QString::fromLatin1 (label, strlen(label));
		hDC->setPen (textcol);
		hDC->drawText (BW/2 - hDC->fontMetrics().horizontalAdvance (s)/2, (BH-fnth)/2 + hDC->fontMetrics().ascent(), s); // TextOut, TA_CENTER
		hDC->setFont (pFont);
	}
	// EndPaint: the painter ends with this function
}

void MFDWindow::ProcessButton (int bt, int event)
{
	switch (bt) {
	case 12:
		if (event == PANEL_MOUSE_LBDOWN)
			SendKey (OAPI_KEY_ESCAPE);
		break;
	case 13:
		if (event == PANEL_MOUSE_LBDOWN)
			SendKey (OAPI_KEY_F1);
		break;
	case 14:
		if (event == PANEL_MOUSE_LBDOWN)
			SendKey (OAPI_KEY_GRAVE);
		break;
	case 15:
		if (event == PANEL_MOUSE_LBDOWN) {
		}
		break;
	default:
		ExternMFD::ProcessButton (bt, event);
		break;
	}
}

void MFDWindow::clbkRefreshDisplay (SURFHANDLE)
{
	if (bFailed) return;

	gcCore *pCore = gcGetCoreInterface();
	if (!pCore) return;

	if (!hSwap) hSwap = pCore->RegisterSwap(DisplayWindow(hDsp), hSwap, 0);
	if (!hSwap) {
		bFailed = true;
		return;
	}

	SURFHANDLE tgt = pCore->GetRenderTarget(hSwap);
	SURFHANDLE surf = GetDisplaySurface();

	// WARNING: Blit src must be a texture otherwise nVidia driver will crash.
	if (surf) pCore->StretchRectInScene(tgt, surf);
	else pCore->ClearSurfaceInScene(tgt, 0);

	pCore->FlipSwap(hSwap);
}

void MFDWindow::clbkRefreshButtons ()
{
	oapiResDlgItem(hDlg, IDC_BUTTON_DRV)->update(); // InvalidateRect
	for (int i = 0; i < 12; i++)
		oapiResDlgItem (hDlg, IDC_BUTTON1+i)->update();
}

void MFDWindow::clbkFocusChanged (OBJHANDLE hFocus)
{
	if (!vstick) {
		ExternMFD::clbkFocusChanged (hFocus);
		//SetTitle ();
	}
}

void MFDWindow::StickToVessel (bool stick)
{
	vstick = stick;
	if (!vstick) {
		SetVessel (oapiGetFocusObject());
		//SetTitle ();
	}
}

// ==============================================================
// Windows message handler for the dialog box

void DlgProc (QWidget *hDlg, void *context)
{
	// WM_INITDIALOG
		((MFDWindow*)context)->Initialise (hDlg);
	// WM_SIZING: Qt can't adjust the drag rectangle, so the size a drag step gives is fitted afterwards (QEvent::Resize)
	// WM_COMMAND
	auto command = [hDlg](int id, int code, QWidget *hCtrl) {
		switch (id) {
		case IDCANCEL:
			oapiUnregisterExternMFD ((MFDWindow*)oapiGetDialogContext (hDlg));
			return;
		case IDHELP:
			((MFDWindow*)oapiGetDialogContext(hDlg))->OpenModeHelp ();
			return;
		case IDSTICK: // title button: the state comes as the notification code
			((MFDWindow*)oapiGetDialogContext(hDlg))->StickToVessel (code != 0);
			return;
		}
	};
	oapiConnectDlgCommands (hDlg, command);
	new DlgEvents (hDlg, [hDlg, command](QEvent *e) -> bool {
		switch (e->type()) {
		case QEvent::Resize: { // WM_SIZE
			MFDWindow *mfd = (MFDWindow*)oapiGetDialogContext(hDlg);
			if (hDlg->property("aspectFit").toBool())
				hDlg->setProperty("aspectFit", false); // the fitted size arriving
			else if (static_cast<QResizeEvent*>(e)->oldSize().isValid() && hDlg->isVisible()) { // WM_SIZING, WMSZ_BOTTOMRIGHT
				QRect g = hDlg->frameGeometry();
				RECT r = { g.left(), g.top(), g.left() + g.width(), g.top() + g.height() };
				mfd->CheckAspect(&r, WMSZ_BOTTOMRIGHT);
				QSize sz((r.right - r.left) - (g.width() - hDlg->width()), (r.bottom - r.top) - (g.height() - hDlg->height()));
				if (sz != hDlg->size()) {
					hDlg->setProperty("aspectFit", true);
					hDlg->resize(sz);
				}
			}
			mfd->Resize();
			return false;
		}
		case QEvent::Close: // not upstream: DefDlgProc's WM_CLOSE -> IDCANCEL to this procedure
			e->ignore();
			command (IDCANCEL, RESN_CLICKED, NULL);
			return true;
		case QEvent::KeyPress: // not upstream: Esc -> IDCANCEL to this procedure
			if (static_cast<QKeyEvent*>(e)->key() != Qt::Key_Escape) return false;
			command (IDCANCEL, RESN_CLICKED, NULL);
			return true;
		default:
			return false;
		}
	});
	// oapiDefDialogProc left out: oapiOpenDialogEx wires the default dialog behaviour
}


// MFD_WndProc left out: the display is a Vulkan window, with no background to erase and nothing to paint (WM_ERASEBKGND, WM_PAINT)


bool MFD_BtnProc (QWidget *hWnd, QEvent *e)
{
	switch (e->type()) {
	case QEvent::Paint: { // WM_PAINT
		MFDWindow *mfdw = (MFDWindow*)oapiGetDialogContext (hWnd->parentWidget());
		mfdw->RepaintButton (hWnd);
		} return true;
	case QEvent::MouseButtonPress:    // WM_LBUTTONDOWN
	case QEvent::MouseButtonDblClick: { // the class has no CS_DBLCLKS: a second WM_LBUTTONDOWN
		if (static_cast<QMouseEvent*>(e)->button() != Qt::LeftButton) break;
		MFDWindow *mfdw = (MFDWindow*)oapiGetDialogContext (hWnd->parentWidget());
		mfdw->ProcessButton (hWnd->property ("GWLP_USERDATA").toInt(), PANEL_MOUSE_LBDOWN);
		// SetCapture: Qt grabs the mouse for the pressed widget
		} return true;
	case QEvent::MouseButtonRelease: { // WM_LBUTTONUP
		if (static_cast<QMouseEvent*>(e)->button() != Qt::LeftButton) break;
		MFDWindow *mfdw = (MFDWindow*)oapiGetDialogContext (hWnd->parentWidget());
		mfdw->ProcessButton (hWnd->property ("GWLP_USERDATA").toInt(), PANEL_MOUSE_LBUP);
		// ReleaseCapture: the grab ends with the release
		} return true;
	default:
		break;
	}

	return false; // DefWindowProc
}


// not upstream: the window classes registered in ExtMFD.cpp (WNDCLASS: window procedure, background brush)
class MFD_Wnd: public QWidget {
public:
	MFD_Wnd (QWidget *parent, bool (*proc)(QWidget*, QEvent*), const QColor &bg): QWidget (parent), wndproc (proc) {
		setAutoFillBackground (true); // hbrBackground
		QPalette p = palette(); p.setColor (QPalette::Window, bg); setPalette (p);
	}
protected:
	bool event (QEvent *e) override { return wndproc (this, e) || QWidget::event (e); }
private:
	bool (*wndproc)(QWidget*, QEvent*);
};

QWidget *MFD_ButtonCtrl (const RESCONTROL *ctrl, QWidget *parent) // "ExtMFD_Button": MFD_BtnProc, LTGRAY_BRUSH
{
	return new MFD_Wnd (parent, MFD_BtnProc, QColor (192, 192, 192));
}

QWidget *MFD_DisplayCtrl (const RESCONTROL *ctrl, QWidget *parent) // "ExtMFD_Display": BLACK_BRUSH, a Vulkan window for the swapchain
{
	QWindow *w = new QWindow;
	w->setSurfaceType (QSurface::VulkanSurface);
	w->create(); // CreateWindow: the native window exists at once, so RegisterSwap works from WM_INITDIALOG on
	QWidget *hWnd = QWidget::createWindowContainer (w, parent);
	hWnd->setProperty ("DisplayWindow", QVariant::fromValue ((void*)w));
	QPalette p = hWnd->palette(); p.setColor (QPalette::Window, Qt::black); hWnd->setPalette (p);
	hWnd->setAutoFillBackground (true);
	return hWnd;
}

