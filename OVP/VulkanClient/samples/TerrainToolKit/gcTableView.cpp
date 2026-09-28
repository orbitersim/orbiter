// ==================================================================
// Copyright (c) 2021-2026 Jarmo Nikkanen
// Licensed under the MIT License
// ==================================================================

#include "gcPropertyTree.h"
#include "OrbiterResource.h"
#include <sstream>
#include <iomanip>
// windowsx.h, CommCtrl.h left out: the child controls are Qt widgets
#include <list>
#include <locale>
#include <codecvt>
#include <cstring>
#include <QApplication>
#include <QBitmap>
#include <QClipboard>
#include <QComboBox>
#include <QKeyEvent>
#include <QLineEdit>
#include <QMouseEvent>
#include <QPainter>
#include <QSignalBlocker>
#include <QSlider>

#define TB_THUMBTRACK 5 // not upstream: commctrl.h value, the code the tree sends for slider moves

static QColor Colour(COLORREF c) { return QColor(GetRValue(c), GetGValue(c), GetBValue(c)); } // not upstream: COLORREF (0x00bbggrr) -> QColor

static bool PtInRect(const RECT *r, POINT p) { return p.x >= r->left && p.x < r->right && p.y >= r->top && p.y < r->bottom; } // not upstream: winuser.h PtInRect

// not upstream: TextOut (hDC, x, y, s) in a colour; GDI's y is the top of the text, QPainter's the baseline
static void TextOut(QPainter *hDC, int x, int y, const QString &s, const QColor &c = Qt::black)
{
	QPen pen = hDC->pen();
	hDC->setPen(c);
	hDC->drawText(x, y + hDC->fontMetrics().ascent(), s);
	hDC->setPen(pen);
}

list<gcPropertyTree *> g_gcPropertyTrees;
std::wstring_convert<std::codecvt_utf8<wchar_t>> converter;


// ==================================================================================
//
bool gcPropertyTreeProc(QWidget *hWnd, QEvent *e)
{
	for (gcPropertyTree * ptr : g_gcPropertyTrees)
		if (ptr->GetHWND() == hWnd) return ptr->WndProc(hWnd, e);

	QWidget *hParent = hWnd->parentWidget();

	if (hParent)
		for (gcPropertyTree * ptr : g_gcPropertyTrees) 
			if (ptr->GetHWND() == hParent) return ptr->WndProc(hWnd, e);
	
	return false; // DefWindowProc
}


// not upstream: the window class "gcPropertyTreeCtrl" (WNDCLASS: window procedure, WHITE_BRUSH background)
class gcPropertyTreeWnd: public QWidget {
public:
	gcPropertyTreeWnd (QWidget *parent): QWidget (parent) {
		setAutoFillBackground (true); // hbrBackground
		QPalette p = palette(); p.setColor (QPalette::Window, Qt::white); setPalette (p);
		setFocusPolicy (Qt::ClickFocus); // SetFocus on a click, for the Ctrl-C key
	}
protected:
	bool event (QEvent *e) override { return gcPropertyTreeProc (this, e) || QWidget::event (e); }
};

static QWidget *gcPropertyTreeCtrl (const RESCONTROL *ctrl, QWidget *parent)
{
	return new gcPropertyTreeWnd (parent);
}


// ==================================================================================
//
void gcPropertyTreeInitialize(void *hInst)
{
	// WNDCLASS: CS_NOCLOSE, CS_OWNDC, CS_SAVEBITS and the arrow cursor have no Qt counterpart to set
	oapiRegisterResControl(hInst, "gcPropertyTreeCtrl", gcPropertyTreeCtrl);
}


// ==================================================================================
//
void gcPropertyTreeRelease(void *hInst)
{
	oapiUnregisterResControl(hInst, "gcPropertyTreeCtrl"); // upstream unregistered "gcPropertyTree", which never existed
}


// ==================================================================================
//
gcPropertyTree::gcPropertyTree(gcGUIApp *_pApp, QWidget *_hWnd, WORD _idc, GCPROPCLBK pCall, QFont *hFnt, void *_hInst) : alloc_id('gcTV')
{
	pApp = _pApp;
	pCore = gcGetCoreInterface();

	// InitCommonControls left out: the common controls are Qt widgets
	g_gcPropertyTrees.push_back(this);
	idc = _idc;
	hWnd = oapiResDlgItem(_hWnd, idc);
	hDlg = _hWnd;
	hInst = _hInst;
	pCallback = pCall;
	hBuf = NULL;
	pSelected = NULL;
	hBM = NULL;

	hIcons = pCore->LoadBitmapFromFile("D3D9/Icons18.png");

	if (!hIcons) {
		oapiWriteLog((char*)"gcPropertyTree: FAILED to load Textures/D3D9/Icons18.png");
	}
	if (!pCore) {
		oapiWriteLog((char*)"gcPropertyTree: No core interface !!");
	}

	// WS_CLIPCHILDREN: Qt widgets don't paint over their children; WS_EX_STATICEDGE, WS_EX_CONTROLPARENT left out (no edge, Qt handles tab focus)
	hWnd->setProperty("GWLP_USERDATA", QVariant::fromValue((void*)this));

	hBr0 = new QBrush(Colour(0xFFFFFF)); // CreateSolidBrush
	hBr1 = new QBrush(Colour(0xF0F0F0));
	hBr2 = new QBrush(Colour(0xFFFFFF));
	hBrTit[0] = new QBrush(Colour(0xDDFFFF));
	hBrTit[1] = new QBrush(Colour(0xDDFFDD));
	hBrTit[2] = new QBrush(Colour(0xFFFFBB));
	hBr4 = new QBrush(Colour(0xAAFFFF));
	hPen = new QPen(Colour(0x808080), 0); // CreatePen (PS_SOLID, 1)
	hFont = hFnt;

	RECT wc, wd, cr;
	QRect g = hWnd->geometry(); // GetWindowRect, in the dialog's coordinates
	wc = { g.left(), g.top(), g.left() + g.width(), g.top() + g.height() };
	wd = { 0, 0, hDlg->width(), hDlg->height() };
	
	QWidget *hCB = CreateComboBox(0);
	cr = { 0, 0, 100, hCB->sizeHint().height() }; // SetWindowPos (100 x 5): a combo box keeps its own height
	delete hCB; // DestroyWindow

	// Compute Margins
	wmrg = ((wd.right - wd.left) - (wc.right - wc.left)) / 2;
	tmrg = wc.top - wd.top;
	bmrg = wd.bottom - wc.bottom;
	hlbl = (cr.bottom - cr.top); // -1;
	len  = 0;
}


// ==================================================================================
//
gcPropertyTree::~gcPropertyTree()
{
	g_gcPropertyTrees.remove(this);

	for (HPROP hp : Data)
	{
		if (hp->hCtrl) delete hp->hCtrl; // DestroyWindow
		if (hp->pSlider) delete hp->pSlider;
		delete hp;
	}

	// DeleteDC (hBM, hSr): the memory DC lives only during Paint
	if (hBuf) delete hBuf;

	delete hIcons;
	delete hBr0;
	delete hBr1;
	delete hBr2;
	delete hBr4;
	delete hPen;
	delete hBrTit[0];
	delete hBrTit[1];
	delete hBrTit[2];
}


// ==================================================================================
//
void gcPropertyTree::CopyToClipboard()
{	
	if (!pSelected) return;
	// OpenClipboard, EmptyClipboard, GlobalAlloc, SetClipboardData (CF_TEXT), CloseClipboard
	QApplication::clipboard()->setText(QString::fromUtf8(pSelected->val.c_str()));
	return;
}


// ==================================================================================
//
void gcPropertyTree::CloseTree(HPROP hPar)
{
	for (HPROP hp : Data)
	{
		if (hp->parent == hPar) {
			if (hp->bChildren) CloseTree(hp);
			else if (hp->hCtrl) {
				hp->oldx = 65536;
				hp->oldy = 65536;
				hp->hCtrl->hide();
			}
		}
	}
}

// ==================================================================================
//
// not upstream: the WM_HSCROLL / WM_COMMAND part of WndProc, called by the child controls' Qt signals (see Create*)
void gcPropertyTree::CtrlNotify(QWidget *hCtrl, WORD code)
{

	// Post Slider Messages to Main DlgProc
	//
		for (HPROP hp : Data)	{
			if (hp->style != Style::SLIDER) continue;
			if (hp->hCtrl != hCtrl) continue;
			if (hp->pSlider) {
				// WM_COMMAND EN_SETFOCUS/EN_KILLFOCUS of a slider left out: trackbars don't send them
				// WM_HSCROLL TB_THUMBTRACK, TB_THUMBPOSITION, TB_PAGEUP, TB_PAGEDOWN: the slider's valueChanged
					{
						WORD pos = WORD(qobject_cast<QSlider*>(hp->hCtrl)->value()); // TBM_GETPOS
						hp->pSlider->lin_pos = (double(pos) / 1000.0);
						if (pCallback) pCallback(hDlg, hp->idc, TB_THUMBTRACK, hp);
					}
					return;
			}
		}


	// Post Edit and ComboBox Messages to Main DlgProc
	//
		for (HPROP hp : Data)	{
			if (hp->style == Style::TEXTBOX || hp->style == Style::COMBOBOX) {
				if (hp->hCtrl == hCtrl && hp->hCtrl != NULL)	{
					if (pCallback) pCallback(hDlg, hp->idc, code, hp);
				}
			}
		}
}

bool gcPropertyTree::WndProc(QWidget *hCtrl, QEvent *e)
{
	// WM_HSCROLL, WM_COMMAND of the child controls: CtrlNotify above

	// Process gcPropertyTree related stuff
	//
	switch (e->type()) {

	case QEvent::KeyPress: // WM_KEYDOWN
	{
		QKeyEvent *k = static_cast<QKeyEvent*>(e);
		if (k->key() == Qt::Key_C && (k->modifiers() & Qt::ControlModifier)) CopyToClipboard();
		break;
	}

	case QEvent::FocusOut: // WM_KILLFOCUS
	{
		pSelected = NULL;
		hWnd->update(); // InvalidateRect
		break;
	}

	case QEvent::Wheel: // WM_MOUSEWHEEL
		break;

	case QEvent::MouseButtonPress: // WM_LBUTTONDOWN
	{
		QMouseEvent *m = static_cast<QMouseEvent*>(e);
		if (m->button() != Qt::LeftButton) break;
		POINT pt = { LONG(m->position().x()), LONG(m->position().y()) };
		for (HPROP hp : Data) {
			if (PtInRect(&hp->rect, pt) && hp->bVisible) {
				pDown = hp;
				if (hp->bChildren == false && hp->hCtrl == NULL) {
					pSelected = hp;
					hWnd->setFocus(); // SetFocus
					hWnd->update(); // InvalidateRect
					if (pCallback) pCallback(hDlg, idc, GCGUI_MSG_SELECTED, pSelected);
				}
				break;
			}
		}
		break;
	}

	case QEvent::MouseButtonRelease: // WM_LBUTTONUP
	{
		QMouseEvent *m = static_cast<QMouseEvent*>(e);
		if (m->button() != Qt::LeftButton) break;
		POINT pt = { LONG(m->position().x()), LONG(m->position().y()) };
		if (pDown) {
			if (PtInRect(&pDown->rect, pt)) {
				pDown->bOpen = !pDown->bOpen;
				if (pDown->bOpen == false) CloseTree(pDown);
				Update();
			}
		}	
		break;
	}

	// WM_MOUSELEAVE left out: Win32 sends it only after TrackMouseEvent, which is never called here

	case QEvent::MouseMove: // WM_MOUSEMOVE
	{
		break;
	}

	case QEvent::Paint: // WM_PAINT
	{
		QPainter hDC(hWnd); // BeginPaint
		Paint(&hDC);
		return true; // EndPaint
	}

	// WM_ERASEBKGND: Paint covers the whole window from its buffer

	default:
		break;
	}

	return false; // DefWindowProc
}


// ===============================================================================================
//
void gcPropertyTree::PaintIcon(int x, int y, int id)
{
	DWORD yell = RGB(255, 255, 0);
	DWORD mang = RGB(255, 0, 255);

	if (hIcons) {

		DWORD ck = 0, sx = 0;
		QSize ic = hIcons->size(); // GetObject (BITMAP)

		switch (id) {
		case 0:	sx = ic.height() * 0; ck = yell; break;
		case 1: sx = ic.height() * 1; ck = mang; break;
		case 2: sx = ic.height() * 2; ck = yell; break;
		case 3: sx = ic.height() * 3; ck = mang; break;
		}

		int yo = (hlbl - ic.height()) / 2;
		// TransparentBlt: the icon with its key colour masked out
		QPixmap icon = QPixmap::fromImage(hIcons->copy(sx, 0, ic.height(), ic.height()));
		icon.setMask(QBitmap::fromImage(hIcons->copy(sx, 0, ic.height(), ic.height()).createMaskFromColor(Colour(ck).rgb(), Qt::MaskInColor)));
		hBM->drawPixmap(x, y + yo, icon);
	}
}

// ==================================================================================
//
void gcPropertyTree::Paint(QPainter *_hDC)
{
	RECT wr; SIZE size;

	wr = { 0, 0, hWnd->width(), hWnd->height() }; // GetWindowRect

	int w = wr.right - wr.left;
	int h = wr.bottom - wr.top;

	QSize ic = hIcons ? hIcons->size() : QSize(0, 0); // GetObject (BITMAP)

	if (hBuf) {
		if (hBuf->height() != h) {
			delete hBuf;
			hBuf = NULL;
		}
	}

	if (!hBuf) {
		hBuf = new QImage(w, h, QImage::Format_RGB32); // CreateCompatibleBitmap
		QPainter bm(hBuf);
		RECT rect = { 0, 0, w, h };
		bm.fillRect(rect.left, rect.top, rect.right - rect.left, rect.bottom - rect.top, *hBr1); // FillRect
	}

	QPainter bm(hBuf); // CreateCompatibleDC, SelectObject: the memory DC, for this paint
	hBM = &bm;

	// SetTextAlign (TA_TOP | TA_LEFT): TextOut below adds the ascent; SetTextColor 0
	hBM->setBackgroundMode(Qt::TransparentMode); // SetBkMode TRANSPARENT
	hBM->setFont(*hFont);
	hBM->setPen(*hPen);
	
	wlbl = 0;

	for (HPROP hp : Data)
	{
		hp->bVisible = false;

		{	// GetTextExtentExPointA
			size.cx = hBM->fontMetrics().horizontalAdvance(QString::fromLatin1(hp->label.c_str(), hp->label.size()));
			if (hp->bChildren) {
				if (hp->val.size() != 0) size.cx += ic.height();
				else size.cx = 0;
			} 
			if (size.cx > wlbl) wlbl = size.cx;
		}
	}

	wlbl += 8*3;
	bOdd = false;

	// CreateRectRgn, SelectClipRgn: the widget's painter clips to it already

	int y = PaintSection(_hDC, NULL, 0, wlbl, 0, 0);
	bm.end();
	hBM = NULL;
	
	_hDC->drawImage(0, 0, *hBuf); // BitBlt

	for (HPROP hp : Data)	if (hp->bVisible) if (hp->hCtrl) hp->hCtrl->update(); // InvalidateRect
}

// ==================================================================================
//
bool gcPropertyTree::HasMoved(HPROP hP, int x, int y)
{
	if ((x == hP->oldx) && (y == hP->oldy)) return false;
	hP->oldx = x;
	hP->oldy = y;
	return true;
}

// ==================================================================================
//
int gcPropertyTree::PaintSection(QPainter *_hDC, HPROP hPar, int ident, int wlbl, int y, int lvl)
{

	RECT wr;
	wr = { 0, 0, hWnd->width(), hWnd->height() }; // GetWindowRect

	int w = wr.right - wr.left;
	
	QSize ic = hIcons ? hIcons->size() : QSize(0, 0); // GetObject (BITMAP)

	for (HPROP hp : Data)
	{
		if (hp->bOn == false) continue;
		if (hp->parent != hPar) continue;
		
		hp->bVisible = true;

		int z = 2;
		int n = 5;
			
		RECT rect = { ident, y, w, y + hlbl };

		hp->rect = rect;

		QBrush *hSel = 0;
		if (bOdd) hSel = hBr0;
		else	  hSel = hBr1;
		if (pSelected == hp) hSel = hBr4;

		bOdd = !bOdd;

		if (hp->bChildren) hBM->fillRect(rect.left, rect.top, rect.right - rect.left, rect.bottom - rect.top, *hBrTit[lvl%3]); // FillRect
		else hBM->fillRect(rect.left, rect.top, rect.right - rect.left, rect.bottom - rect.top, *hSel);

		hBM->drawLine(ident, y + hlbl - 1, w, y + hlbl - 1); // MoveToEx, LineTo

		if (hp->bChildren == false || hp->val.size() > 0) {
			hBM->drawLine(wlbl, y, wlbl, y + hlbl);
		}

		// Title bar for a sub section
		if (hp->bChildren) {

			if (hp->bOpen) PaintIcon(ident, y - 1, 0);
			else PaintIcon(ident, y - 1, 1);

			TextOut(hBM, ident + ic.height() + n/2, y + z, QString::fromLatin1(hp->label.c_str(), hp->label.size()));
			TextOut(hBM, wlbl + n, y + z, QString::fromLatin1(hp->val.c_str(), hp->val.size()));

			y += hlbl;

			if (hp->bOpen) {
				int slen = GetSubsentionLength(hp);
				hBM->setBrush(*hBr4); // SelectObject
				hBM->drawRect(ident - 1, y - 1, (ident + 4) - (ident - 1) - 1, (y + slen) - (y - 1) - 1); // Rectangle
				y = PaintSection(_hDC, hp, ident + 4, wlbl, y, lvl + 1);
			}

			continue;
		}
		
		TextOut(hBM, n + ident, y + z, QString::fromLatin1(hp->label.c_str(), hp->label.size()));

		if (!hp->hCtrl) {
			QString ws = QString::fromUtf8(hp->val.c_str()); // converter.from_bytes
			TextOut(hBM, wlbl + n, y + z, ws, Colour(hp->color)); // SetTextColor, TextOutW, SetTextColor 0
		}
		else {

			// ExcludeClipRect below left out: the child controls are painted over the tree anyway

			// Textbox
			if (hp->style == Style::TEXTBOX) {
				int l = wlbl + 5;
				int t = y + 2;
				int r = w - 1;
				int b = y + hlbl - 2;
				RECT re = { wlbl + 1, y, w, y + hlbl - 1 };
				hBM->fillRect(re.left, re.top, re.right - re.left, re.bottom - re.top, *hBr2);
				if (HasMoved(hp, l, t))	{ hp->hCtrl->setGeometry(l, t, r - l, b - t); hp->hCtrl->show(); } // SetWindowPos SWP_SHOWWINDOW
			}

			// Combobox
			if (hp->style == Style::COMBOBOX) {
				int l = wlbl;
				int t = y - 1;
				int r = w - 1;
				int b = y + hlbl;
				if (HasMoved(hp, l, t))	{ hp->hCtrl->setGeometry(l, t, r - l, b - t); hp->hCtrl->show(); }
			}

			// Slider
			if (hp->style == Style::SLIDER) {
				int l = wlbl + 1;
				int t = y;
				int r = w - 2;
				int b = y + hlbl - 1;
				if (HasMoved(hp, l, t))	{ hp->hCtrl->setGeometry(l, t, r - l, b - t); hp->hCtrl->show(); }
			}
		}

		y += hlbl;	
	} 

	return y;
}


// ==================================================================================
//
int gcPropertyTree::GetSubsentionLength(HPROP hPar)
{
	int q = 0;
	for (HPROP hp : Data)
	{
		if (hp->bOn) {
			if (hp->parent == hPar) {
				q += hlbl;
				if (hp->bChildren && hp->bOpen) q += GetSubsentionLength(hp);
			}
		}
	}
	return q;
}


// ==================================================================================
//
void gcPropertyTree::Update()
{
	RECT wd;
	wd = { 0, 0, hDlg->width(), hDlg->height() }; // GetWindowRect
	int w = (wd.right - wd.left);
	int h = GetSubsentionLength(NULL);
	
	if (h != len) {
		hDlg->resize(w, h + tmrg + bmrg); hDlg->show(); // SetWindowPos (SWP_NOMOVE | SWP_NOZORDER | SWP_SHOWWINDOW)
		hWnd->setGeometry(wmrg, tmrg, w - wmrg * 2, h); hWnd->show(); // SetWindowPos (SWP_NOZORDER | SWP_SHOWWINDOW)
		pApp->UpdateSize(hDlg);
		len = h;
	}

	hWnd->update(); // InvalidateRect
}


// ==================================================================================
//
QWidget *gcPropertyTree::GetHWND() const 
{ 
	return hWnd;
}


// SendCtrlMessage left out: raw window messages to the child controls (see gcPropertyTree.h)


// ==================================================================================
//
QWidget *gcPropertyTree::CreateEditControl(WORD id, bool bReadOnly)
{
	QLineEdit *hEdit = new QLineEdit(hWnd); // CreateWindowExA "EDIT", WS_CHILD | WS_VISIBLE
	hEdit->setReadOnly(bReadOnly); // ES_READONLY
	hEdit->setFrame(false);
	hEdit->setProperty("resId", id); // HMENU id
	hEdit->setFont(*hFont); // WM_SETFONT
	hEdit->setGeometry(0, 0, 0, 0);
	hEdit->show();
	// WM_COMMAND EN_CHANGE, EN_KILLFOCUS to the parent: CtrlNotify
	QObject::connect(hEdit, &QLineEdit::textChanged, hWnd, [this, hEdit]() { CtrlNotify(hEdit, RESN_CHANGE); });
	QObject::connect(hEdit, &QLineEdit::editingFinished, hWnd, [this, hEdit]() { CtrlNotify(hEdit, RESN_KILLFOCUS); });
	return hEdit;
}


// ==================================================================================
//
QWidget *gcPropertyTree::CreateComboBox(WORD id)
{
	QComboBox *hEdit = new QComboBox(hWnd); // CreateWindowExA "COMBOBOX", WS_CHILD | WS_VISIBLE | CBS_DROPDOWN
	hEdit->setEditable(true);
	hEdit->setProperty("resId", id); // HMENU id
	hEdit->setFont(*hFont); // WM_SETFONT
	hEdit->setGeometry(0, 0, 0, 0);
	hEdit->show();
	// WM_COMMAND CBN_SELCHANGE, CBN_EDITCHANGE to the parent: CtrlNotify
	QObject::connect(hEdit, &QComboBox::activated, hWnd, [this, hEdit]() { CtrlNotify(hEdit, RESN_SELCHANGE); });
	QObject::connect(hEdit, &QComboBox::editTextChanged, hWnd, [this, hEdit]() { CtrlNotify(hEdit, RESN_EDITCHANGE); });
	return hEdit;
}


// ==================================================================================
//
QWidget *gcPropertyTree::CreateSlider(WORD id)
{
	QSlider *hEdit = new QSlider(Qt::Horizontal, hWnd); // CreateWindowExA TRACKBAR_CLASS, WS_CHILD | WS_VISIBLE | TBS_NOTICKS | TBS_BOTH
	hEdit->setTickPosition(QSlider::NoTicks);
	hEdit->setProperty("resId", id); // HMENU id
	hEdit->setRange(0, 1000); // TBM_SETRANGE
	hEdit->setGeometry(0, 0, 0, 0);
	hEdit->show();
	// WM_HSCROLL to the parent: CtrlNotify (TBM_SETPOS blocks the signal, as it doesn't notify)
	QObject::connect(hEdit, &QSlider::valueChanged, hWnd, [this, hEdit]() { CtrlNotify(hEdit, TB_THUMBTRACK); });
	return hEdit;
}


// ==================================================================================
//
HPROP gcPropertyTree::SubSection(const string &lbl, HPROP parent)
{
	HPROP hProp = AddEntry(lbl, parent);
	hProp->bChildren = true;
	return hProp;
}


// ==================================================================================
//
HPROP gcPropertyTree::AddEntry(const string &lbl, HPROP parent)
{
	gcProperty *td = new gcProperty();
	td->bOpen = td->bOn = true;
	td->bVisible = false;
	td->label = lbl;
	td->parent = parent;
	td->val = string("");
	td->hCtrl = NULL;
	td->pSlider = NULL;
	td->style = Style::TEXT;
	td->idc = 0;
	td->oldx = 65536;
	td->oldy = 65536;
	td->color = 0;
	if (parent) parent->bChildren = true;
	Data.push_back(td);
	return td;
}


// ==================================================================================
//
HPROP gcPropertyTree::AddEditControl(const string &lbl, WORD id, HPROP parent, const string &text, void *pUser)
{
	HPROP hProp = AddEntry(lbl, parent);
	hProp->hCtrl = CreateEditControl(id, false);
	hProp->style = Style::TEXTBOX;
	hProp->idc = id;
	hProp->pUser = pUser;
	return hProp;
}


// ==================================================================================
//
HPROP gcPropertyTree::AddComboBox(const string &lbl, WORD id, HPROP parent, void* pUser)
{
	HPROP hProp = AddEntry(lbl, parent);
	hProp->hCtrl = CreateComboBox(id);
	hProp->style = Style::COMBOBOX;
	hProp->idc = id;
	hProp->pUser = pUser;
	return hProp;
}


// ==================================================================================
//
HPROP gcPropertyTree::AddSlider(const string &lbl, WORD id, HPROP parent, void* pUser)
{
	HPROP hProp = AddEntry(lbl, parent);
	hProp->hCtrl = CreateSlider(id);
	hProp->pSlider = new gcSlider;
	hProp->style = Style::SLIDER;
	hProp->idc = id;
	hProp->pUser = pUser;
	return hProp;
}


// ==================================================================================
//
HPROP gcPropertyTree::GetEntry(int idx)
{
	if (idx >= int(Data.size())) return NULL;
	return Data[idx];
}


// ==================================================================================
//
HPROP gcPropertyTree::GetEntry(QWidget *hCtrl)
{
	if (hCtrl == NULL) return NULL;
	for (HPROP hp : Data) if (hp->hCtrl == hCtrl) return hp;
	return NULL;
}

void* gcPropertyTree::GetUserRef(HPROP hEntry)
{
	if (hEntry) return hEntry->pUser;
	return NULL;
}


// ==================================================================================
//
QWidget *gcPropertyTree::GetControl(HPROP hEntry)
{
	if (hEntry) return hEntry->hCtrl;
	return NULL;
}


// ==================================================================================
//
void gcPropertyTree::SetValue(HPROP hEntry, int val)
{
	hEntry->val = std::to_string(val);
}


// ==================================================================================
//
void gcPropertyTree::SetValue(HPROP hEntry, DWORD val, bool bHex)
{
	std::ostringstream oss;
	oss << std::hex;
	oss << val;
	hEntry->val = oss.str();
}


// ==================================================================================
//
void gcPropertyTree::SetValue(HPROP hEntry, double val, int digits, Format st)
{
	string u;
	std::ostringstream oss;
	oss << std::setprecision(digits);
	oss << std::fixed;
	
	if (st == UNITS) {
		double x = fabs(val);
		if (x > 1e9) val /= 1e6, u = "G";
		else if (x > 1e6) val /= 1e6, u = "M";
		else if (x > 1e3) val /= 1e3, u = "k";
		if (x < 1e-6) val /= 1e-6, u = "µ";
		else if (x < 1e-3) val /= 1e-3, u = "m";	
		oss << val << u;
	}

	if (st == LATITUDE) if (val < 0) oss << fabs(val*DEG) << "°S"; else oss << val*DEG << "°N";
	if (st == LONGITUDE) if (val < 0) oss << fabs(val*DEG) << "°W"; else oss << val*DEG << "°E";
	if (st == NORMAL) oss << val;

	hEntry->val = oss.str();
}


// ==================================================================================
//
void gcPropertyTree::SetValue(HPROP hEntry, const string &val, DWORD clr)
{
	hEntry->val = val;
	hEntry->color = clr;
}


// ==================================================================================
//
void  gcPropertyTree::SetValue(HPROP hEntry, const char *lbl, DWORD clr)
{
	hEntry->val = string(lbl);
	hEntry->color = clr;
}


// ==================================================================================
//
void gcPropertyTree::OpenEntry(HPROP hEntry, bool bOpen)
{
	hEntry->bOpen = bOpen;
}


// ==================================================================================
//
void gcPropertyTree::ShowEntry(HPROP hEntry, bool bShow)
{
	hEntry->bOn = bShow;
}

// ==================================================================================
//
void gcPropertyTree::SetSliderScale(HPROP hSlider, double fmin, double fmax, Scale scl)
{
	gcSlider *pSl = hSlider->pSlider;
	if (pSl) {
		pSl->fmin = fmin;
		pSl->fmax = fmax;
		pSl->lfmin = log(fmin);
		pSl->lfmax = log(fmax);
		pSl->scl = scl;
	}
}

// ==================================================================================
//
void gcPropertyTree::SetSliderValue(HPROP hSlider, double val)
{
	gcSlider *pSl = hSlider->pSlider;
	if (pSl) {
		if (pSl->scl == Scale::LOG) pSl->lin_pos = (log(val) - pSl->lfmin) / (pSl->lfmax - pSl->lfmin);
		double lin = (val - pSl->fmin) / (pSl->fmax - pSl->fmin);
		if (pSl->scl == Scale::LINEAR) pSl->lin_pos = lin;
		if (pSl->scl == Scale::SQRT) pSl->lin_pos = lin*lin;
		if (pSl->scl == Scale::SQUARE) pSl->lin_pos = sqrt(lin);
		QSignalBlocker block(hSlider->hCtrl); // TBM_SETPOS doesn't notify
		qobject_cast<QSlider*>(hSlider->hCtrl)->setValue(WORD(pSl->lin_pos*1000.0));
	}
}

// ==================================================================================
//
double gcPropertyTree::GetSliderValue(HPROP hSlider)
{
	gcSlider *pSl = hSlider->pSlider;
	if (pSl) {
		if (pSl->scl == Scale::LOG) return exp((pSl->lfmax - pSl->lfmin) * pSl->lin_pos + pSl->lfmin);
		if (pSl->scl == Scale::LINEAR) return (pSl->fmax - pSl->fmin) * pSl->lin_pos + pSl->fmin;
		if (pSl->scl == Scale::SQRT) return (pSl->fmax - pSl->fmin) * sqrt(pSl->lin_pos) + pSl->fmin;
		if (pSl->scl == Scale::SQUARE) return (pSl->fmax - pSl->fmin) * (pSl->lin_pos*pSl->lin_pos) + pSl->fmin;
	}
	return 0.0;
}

// ==================================================================================
//
string gcPropertyTree::GetTextBoxContent(HPROP hTextBox)
{
	if (hTextBox->style != Style::TEXTBOX) return string();
	if (!hTextBox->hCtrl) return string();
	oapiGetDlgText(hTextBox->hCtrl, buffer, sizeof(buffer)); // GetWindowTextA
	return string(buffer);
}

// ==================================================================================
//
void gcPropertyTree::SetTextBoxContent(HPROP hTextBox, string text)
{
	if (hTextBox->style != Style::TEXTBOX) return;
	if (!hTextBox->hCtrl) return;
	oapiSetDlgText(hTextBox->hCtrl, text.c_str()); // SetWindowTextA
}

// ==================================================================================
//
void gcPropertyTree::SetComboBoxSelection(HPROP hCombo, int idx)
{
	if (hCombo->style != Style::COMBOBOX) return;
	if (!hCombo->hCtrl) return;
	QSignalBlocker block(hCombo->hCtrl); // CB_SETCURSEL doesn't notify
	qobject_cast<QComboBox*>(hCombo->hCtrl)->setCurrentIndex(idx);
}

// ==================================================================================
//
int	gcPropertyTree::GetComboBoxSelection(HPROP hCombo)
{
	if (hCombo->style != Style::COMBOBOX) return 0;
	if (!hCombo->hCtrl) return 0;
	return qobject_cast<QComboBox*>(hCombo->hCtrl)->currentIndex(); // CB_GETCURSEL
}

// ==================================================================================
//
void gcPropertyTree::ClearComboBox(HPROP hCombo)
{
	if (hCombo->style != Style::COMBOBOX) return;
	if (!hCombo->hCtrl) return;
	QSignalBlocker block(hCombo->hCtrl); // CB_RESETCONTENT doesn't notify
	qobject_cast<QComboBox*>(hCombo->hCtrl)->clear();
}

// ==================================================================================
//
int gcPropertyTree::AddComboBoxItem(HPROP hCombo, const char *label)
{
	if (hCombo->style != Style::COMBOBOX) return -1;
	if (!hCombo->hCtrl) return -1;
	QSignalBlocker block(hCombo->hCtrl); // CB_ADDSTRING doesn't notify
	return oapiComboAddString(qobject_cast<QComboBox*>(hCombo->hCtrl), label);
}

