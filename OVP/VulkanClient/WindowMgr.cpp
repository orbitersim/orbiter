

// =================================================================================================================================
//
// Copyright (C) 2019-2026 Jarmo Nikkanen
//
// Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation
// files (the "Software"), to use, copy, modify, merge, publish, distribute, interact with the Software and sublicense copies
// of the Software, subject to the following conditions:
//
// a) You do not sell, rent or auction the Software.
// b) You do not collect distribution fees.
// c) If the Software is distributed in an object code form, it must inform that the source code is available and how to obtain it.
// d) You do not remove or alter any copyright notices contained within the Software.
// e) This copyright notice must be included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
// OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
// LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR
// IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
// =================================================================================================================================


#include "D3DXMath.h" // d3d9.h/d3dx9.h
#include "WindowMgr.h"
// windows.h, WindowsX.h left out: Qt widgets, painters and events
#include "resource.h"
#include "OapiExtension.h"
#include "D3D9Config.h"
#include "Log.h"
#include "D3D9Client.h"
#include "OrbiterResource.h"
#include <list>
#include <QWidget>
#include <QWindow>
#include <QScreen>
#include <QPainter>
#include <QImage>
#include <QFont>
#include <QFontMetricsF>
#include <QMouseEvent>
#include <QWheelEvent>

#define APPNODE(x) ((Node *)x)

extern D3D9Client *g_client;

class WindowManager *g_pWM = NULL;

list<gcGUIApp *> g_gcGUIAppList;

// ===============================================================================================
//
inline bool PointInside(int x, int y, LPRECT r)
{
	if (x < r->left) return false;
	if (x > r->right) return false;
	if (y < r->top) return false;
	if (y > r->bottom) return false;
	return true;
}
// ===============================================================================================
//
inline DWORD _Colour(const FVECTOR4 *c)
{
	DWORD r = DWORD(c->r * 255.0f + 0.5f);
	DWORD g = DWORD(c->g * 255.0f + 0.5f);
	DWORD b = DWORD(c->b * 255.0f + 0.5f);

	if (r > 0xFF) r = 0xFF;
	if (g > 0xFF) g = 0xFF;
	if (b > 0xFF) b = 0xFF;
	
	return (b << 16) | (g << 8) | r;
}
// ===============================================================================================
//
inline FVECTOR4 _Colour(DWORD dwABGR)
{
	DWORD r = (dwABGR & 0xFF); dwABGR >>= 8;
	DWORD g = (dwABGR & 0xFF); dwABGR >>= 8;
	DWORD b = (dwABGR & 0xFF); dwABGR >>= 8;
	FVECTOR4 c;
	float q = 3.92156862e-3f;
	c.r = float(r) * q;
	c.g = float(g) * q;
	c.b = float(b) * q;
	c.a = 1.0f;
	return c;
}
// ===============================================================================================
// not upstream: COLORREF (0x00BBGGRR) ↔ QImage pixel (GetPixel/SetPixel/SetTextColor)
inline COLORREF _CR(QRgb p) { return RGB(qRed(p), qGreen(p), qBlue(p)); }
inline QRgb _QRgb(COLORREF c) { return qRgb(GetRValue(c), GetGValue(c), GetBValue(c)); }

// not upstream: Win32's IntersectRect (empty rect when they don't overlap)
static void IntersectRect(RECT *o, const RECT *a, const RECT *b)
{
	*o = { std::max(a->left, b->left), std::max(a->top, b->top), std::min(a->right, b->right), std::min(a->bottom, b->bottom) };
	if (o->left >= o->right || o->top >= o->bottom) *o = { 0, 0, 0, 0 };
}

// not upstream: CreateFont counterpart (height > 0: cell height, < 0: character height; weight 0 = FW_DONTCARE)
static QFont *CreateFont(int height, int weight, const char *face)
{
	QFont *f = new QFont(QString::fromLatin1(face));
	f->setPixelSize(std::max(1, abs(height)));
	f->setWeight(QFont::Weight(std::clamp(weight ? weight : 400, 1, 1000)));
	f->setStyleHint(QFont::TypeWriter); // pitch and family 49: FF_MODERN | FIXED_PITCH when the face is missing
	if (height > 0) { QFontMetricsF fm(*f); if (fm.height() > 0) f->setPixelSize(std::max(1, int(round(height * height / fm.height())))); }
	return f;
}

// not upstream: the "SideBarWnd" and "Floater" window classes: a widget that hands its events to SideBarWndProc
class SideBarWnd : public QWidget
{
public:
	SideBarWnd(const char *title, Qt::WindowFlags flags) : QWidget(NULL, flags)
	{
		setWindowTitle(title);
		setAttribute(Qt::WA_OpaquePaintEvent); // WM_ERASEBKGND returns 1: no background erase (hbrBackground BLACK_BRUSH unused)
		setMouseTracking(true);                // WM_MOUSEMOVE comes without a button held too
		setCursor(Qt::ArrowCursor);            // hCursor IDC_ARROW
	}
protected:
	bool event(QEvent *e) override
	{
		if (::SideBarWndProc(this, e)) return true;
		return QWidget::event(e);
	}
};

// ===============================================================================================
//
bool SideBarWndProc(QWidget *hWnd, QEvent *event)
{
	if (g_pWM) {
		SideBar *pBar = g_pWM->GetSideBar(hWnd);
		if (pBar) return pBar->SideBarWndProc(hWnd, event);
	}
	return false; // DefWindowProc: the widget's own handling
}
// ===============================================================================================
//
void DummyDlgProc(QWidget *hDlg, void *context)
{
	// WM_INITDIALOG: nothing to set up
}


// ===============================================================================================
//
void DlgProc(QWidget *hDlg, void *context)
{
	// WM_INITDIALOG: nothing to set up
}




// ===============================================================================================
//
void OpenTestClbk(void *context)
{
	/*
	WindowManager *pWM = (WindowManager *)context;
	HINSTANCE hInst = pWM->GetInstance();
	HWND hAppMainWindow = pWM->GetMainWindow();


	HWND hDlg = CreateDialogParam(hInst, MAKEINTRESOURCE(IDD_MESHDEBUG), hAppMainWindow, DlgProc, 0);
	HNODE hRootNode = pWM->RegisterApplication("D3D9 Controls", NULL, 0, gcGUI::DS_LEFT);

	pWM->RegisterSubsection(hRootNode, "Mesh Debugger", hDlg);

	hDlg = CreateDialogParam(hInst, MAKEINTRESOURCE(IDD_MATERIAL), hAppMainWindow, DlgProc, 0);
	pWM->RegisterSubsection(hRootNode, "Material Config", hDlg);

	hDlg = CreateDialogParam(hInst, MAKEINTRESOURCE(IDD_SCENEDEBUG), hAppMainWindow, DlgProc, 0);
	HNODE hSD = pWM->RegisterSubsection(hRootNode, "Scene Debugger", hDlg);

	hDlg = CreateDialogParam(hInst, MAKEINTRESOURCE(IDD_MICROTEXTOOLS), hAppMainWindow, DlgProc, 0);
	HNODE hTT = pWM->RegisterSubsection(hRootNode, "MicroTex Tools", hDlg);

	pWM->OpenNode(hSD, false);
	pWM->OpenNode(hTT, false);

	pWM->DisplayWindow(hRootNode);
	*/
}














// ===============================================================================================
// Node Implementation
// ===============================================================================================
//
Node::Node(SideBar *pSB, const char *label, QWidget *hDlg, DWORD color, Node *pP) :
	pSB(pSB), pParent(pP), hBmp(NULL), hDlg(hDlg), pApp(NULL), bOpen(true), bClose(false)
{

	bm = QSize(0, 0); // memset(&bm, 0, sizeof(BITMAP))

	WindowManager *pMgr = pSB->GetWM();

	pSB->AddWindow(this);

	if (label) strcpy(Label = new char[strlen(label) + 1], label); // _strdup: a new[] copy, the destructor frees it with delete[]
	else Label = NULL;

	QImage *hTit;

	if ((pParent == NULL) && (Config->gcGUIMode == 3) && hDlg) return;	// No Title Bar

	if (pParent == NULL) hTit = pMgr->GetBitmap(gcGUI::BM_TITLE);
	else {
		hTit = pMgr->GetBitmap(gcGUI::BM_SUBTITLE);
		pApp = pParent->pApp;
	}

	FVECTOR4 clr = _Colour(color);
	FVECTOR4 white = _Colour(0xFFFFFFFF);

	// GetDC, CreateCompatibleDC, SelectObject left out: the pixels are read and written on the QImages

	
	bm = hTit ? hTit->size() : QSize(0, 0); // GetObject
	
	hBmp = new QImage(bm, QImage::Format_RGB32); // CreateCompatibleBitmap

	// Recolorize the title bar

	for (int y = 0; y < bm.height(); y++) {
		for (int x = 0; x < bm.width(); x++) {		
			COLORREF cr = _CR(hTit->pixel(x, y)); // GetPixel
			FVECTOR4 c = _Colour(cr);
			FVECTOR4 out = (clr * c.b) + (white * c.g);
			hBmp->setPixel(x, y, _QRgb(_Colour(&out))); // SetPixel
		}
	}
}

// ===============================================================================================
//
Node::~Node()
{
	if (hBmp) delete hBmp; // DeleteObject
	if (Label) delete[] Label;
}

// ===============================================================================================
//
void Node::SetApp(gcGUIApp *_pApp)
{
	pApp = _pApp;
}

// ===============================================================================================
//
void Node::ReColorize(DWORD color)
{
	bm = QSize(0, 0); // memset(&bm, 0, sizeof(BITMAP))

	WindowManager *pMgr = pSB->GetWM();

	QImage *hTit;

	if ((pParent == NULL) && (Config->gcGUIMode == 3) && hDlg) return;	// No Title Bar

	if (pParent == NULL) hTit = pMgr->GetBitmap(gcGUI::BM_TITLE);
	else hTit = pMgr->GetBitmap(gcGUI::BM_SUBTITLE);

	FVECTOR4 clr = _Colour(color);
	FVECTOR4 white = _Colour(0xFFFFFFFF);

	// GetDC, CreateCompatibleDC, SelectObject left out: the pixels are read and written on the QImages

	bm = hTit ? hTit->size() : QSize(0, 0); // GetObject

	if (hBmp) delete hBmp; // DeleteObject

	hBmp = new QImage(bm, QImage::Format_RGB32); // CreateCompatibleBitmap

	// Recolorize the title bar

	for (int y = 0; y < bm.height(); y++) {
		for (int x = 0; x < bm.width(); x++) {
			COLORREF cr = _CR(hTit->pixel(x, y)); // GetPixel
			FVECTOR4 c = _Colour(cr);
			FVECTOR4 out = (clr * c.b) + (white * c.g);
			hBmp->setPixel(x, y, _QRgb(_Colour(&out))); // SetPixel
		}
	}
}


// ===============================================================================================
//
int Node::CellSize()
{
	int y = 0;
	if (hBmp) y += bm.height();
	if (bOpen && hDlg) {
		QRect r = hDlg->frameGeometry(); // GetWindowRect
		y += r.height();
	}
	return y;
}


// ===============================================================================================
//
int Node::Paint(QPainter *hDC, int y)
{
	WindowManager *pMgr = pSB->GetWM();

	int width = pSB->GetWidth();
	int x = 0;
	int wof = 0, hof = 0;
	DWORD ck = 0;

	if (pSB->GetStyle() == gcGUI::DS_FLOAT) x += 1;	
		
	if (hBmp) {
		if (pParent) {
			hDC->setFont(*pMgr->GetSubTitleFont()); // SelectObject
			hDC->setPen(QColor(_QRgb(pMgr->cfg.txt_sub_clr))); // SetTextColor
			wof = pMgr->cfg.txt_sub_x;
			hof = pMgr->cfg.txt_sub_y;
		}
		else {
			hDC->setFont(*pMgr->GetAppTitleFont()); // SelectObject
			hDC->setPen(QColor(_QRgb(pMgr->cfg.txt_main_clr))); // SetTextColor
			wof = pMgr->cfg.txt_main_x;
			hof = pMgr->cfg.txt_main_y;
		}
	}

	
	// Draw Title Bars -----------------------------
	//
	if (hBmp) {
		
		// CreateCompatibleDC left out: the painter draws the QImage
		int z = width - bm.height() - 3;

		trect = { 0, y, width, y + bm.height() };
		crect = { z, y, width, y + bm.height() };

		hDC->drawImage(QPoint(x, y), *hBmp, QRect(0, 0, width - 10, bm.height())); // BitBlt SRCCOPY
		hDC->drawImage(QPoint(width - 10 - x, y), *hBmp, QRect(bm.width() - 10, 0, 10, bm.height()));	// BitBlt SRCCOPY
		hDC->drawText(wof, y + hof + hDC->fontMetrics().ascent(), QString::fromLatin1(Label ? Label : "")); // TextOut: top edge → baseline
		
		// DeleteDC left out

		if (bOpen) PaintIcon(hDC, x, y, 0);
		else PaintIcon(hDC, x, y, 1);
		if (bClose) PaintIcon(hDC, z, y, 2);
			
		y += bm.height();
	}

	pos = { x, y };
	
	if (bOpen && hDlg) {
		QRect r = hDlg->frameGeometry(); // GetWindowRect
		y += r.height();
	}

	return y;
}


// ===============================================================================================
//
void Node::PaintIcon(QPainter *hDC, int x, int y, int id)
{
	WindowManager *pMgr = pSB->GetWM();
	DWORD yell = RGB(255, 255, 0); 
	DWORD mang = RGB(255, 0, 255); 
	
	QImage *hIco = pMgr->GetBitmap(gcGUI::BM_ICONS);

	if (hIco) {
		DWORD ck = 0, sx = 0;
		QSize ic = hIco->size(); // GetObject BITMAP

		switch (id) {
		case 0:	sx = ic.height() * 0; ck = yell; break;
		case 1: sx = ic.height() * 1; ck = mang; break;
		case 2: sx = ic.height() * 2; ck = yell; break;
		case 3: sx = ic.height() * 3; ck = mang; break;
		}

		// CreateCompatibleDC left out: the icon cell is cut from the QImage
		QImage cell = hIco->copy(sx, 0, ic.height(), ic.height()).convertToFormat(QImage::Format_ARGB32);
		int yo = (bm.height() - ic.height()) / 2;
		for (int j = 0; j < cell.height(); j++) for (int i = 0; i < cell.width(); i++)
			if (_CR(cell.pixel(i, j)) == ck) cell.setPixel(i, j, 0); // TransparentBlt: colour key ck → transparent
		hDC->drawImage(QPoint(x, y + yo), cell);
		// DeleteDC left out
	}
}


// ===============================================================================================
//
int Node::Spacer(QPainter *hDC, int y)
{
	WindowManager *pMgr = pSB->GetWM();

	if (pParent) if (pParent->bOpen == false) if (pParent->pSB == pSB) return y;

	int width = pSB->GetWidth();
	int h = CellSize();
	
	hDC->setBrush(QColor(128, 128, 128)); // GRAY_BRUSH
	hDC->setPen(Qt::NoPen); // NULL_PEN
	hDC->drawRect(0, y, width, h); // Rectangle(0, y, width + 1, y + h + 1): a NULL_PEN fill is one pixel smaller
	
	return y + h;
}


// ===============================================================================================
//
void Node::Move()
{
	if (!hDlg) return;
	hDlg->move(pos.x, pos.y); // SetWindowPos SWP_NOSIZE | SWP_NOZORDER | SWP_SHOWWINDOW
	hDlg->show();
}








// ===============================================================================================
// WindowManager Implementation
// ===============================================================================================
//
WindowManager::WindowManager(QWindow *hAppMainWindow, void *_hInst, bool bWindowed)
{
	char path[256];
	char cbuf[256];

	g_pWM = NULL;
	hMainWnd = hAppMainWindow;
	hInst = _hInst;
	sbDrag = NULL;
	sbDragSrc = NULL;
	sbDest = NULL;
	bWin = bWindowed;

	hIcons = hTitle = hSub = NULL;
	hAppFont = hSubFont = NULL; // not upstream: the early returns below left the fonts, Cmd and width unset
	Cmd = 0;
	width = 0;

	RECT rMain = { 0, 0, hAppMainWindow->width(), hAppMainWindow->height() }; // GetClientRect

	AutoFile file;

	if (file.IsInvalid()) {
		snprintf(path, 256, "%sgcGUI.cfg", OapiExtension::GetConfigDir());
		file.pFile = fopen(oapiResolvePath(path).c_str(), "r"); // ConfigDir may be written with '\\'
	}

	if (!file.IsInvalid()) {

		int q, w;
		bool bFound = false;

		while (fgets2(cbuf, 256, file.pFile, 0x0A) >= 0)
		{
			// --------------------------------------------------------------------------------------------
			if (!strncmp(cbuf, "RESOLUTION", 10)) {
				if (sscanf(cbuf, "RESOLUTION %d %d", &q, &w) != 2) LogErr("Invalid Line in (%s): %s", path, cbuf);
				if (q < rMain.bottom && rMain.bottom < w) bFound = true;
				continue;
			}
			if (bFound == false) continue;

			if (!strncmp(cbuf, "END", 3)) break;

			// --------------------------------------------------------------------------------------------
			if (!strncmp(cbuf, "FONT_MAIN", 9)) {
				if (sscanf(cbuf, "FONT_MAIN \"%31[^\"]\" %d %d", cfg.fnt_main, &cfg.txt_main_size, &cfg.txt_main_weight) != 3) LogErr("Invalid Line in (%s): %s", path, cbuf);
				continue;
			}
			// --------------------------------------------------------------------------------------------
			if (!strncmp(cbuf, "FONT_SUB", 8)) {
				if (sscanf(cbuf, "FONT_SUB \"%31[^\"]\" %d %d", cfg.fnt_sub, &cfg.txt_sub_size, &cfg.txt_sub_weight) != 3) LogErr("Invalid Line in (%s): %s", path, cbuf);
				continue;
			}
			// --------------------------------------------------------------------------------------------
			if (!strncmp(cbuf, "MAIN_OFS", 8)) {
				if (sscanf(cbuf, "MAIN_OFS %d %d", &cfg.txt_main_x, &cfg.txt_main_y) != 2) LogErr("Invalid Line in (%s): %s", path, cbuf);
				continue;
			}
			// --------------------------------------------------------------------------------------------
			if (!strncmp(cbuf, "SUB_OFS", 7)) {
				if (sscanf(cbuf, "SUB_OFS %d %d", &cfg.txt_sub_x, &cfg.txt_sub_y) != 2) LogErr("Invalid Line in (%s): %s", path, cbuf);
				continue;
			}
			// --------------------------------------------------------------------------------------------
			if (!strncmp(cbuf, "MAIN_BMP", 8)) {
				if (sscanf(cbuf, "MAIN_BMP %31s", cfg.bmp_main) != 1) LogErr("Invalid Line in (%s): %s", path, cbuf);
				continue;
			}
			// --------------------------------------------------------------------------------------------
			if (!strncmp(cbuf, "SUB_BMP", 7)) {
				if (sscanf(cbuf, "SUB_BMP %31s", cfg.bmp_sub) != 1) LogErr("Invalid Line in (%s): %s", path, cbuf);
				continue;
			}
			// --------------------------------------------------------------------------------------------
			if (!strncmp(cbuf, "ICON_BMP", 8)) {
				if (sscanf(cbuf, "ICON_BMP %31s", cfg.bmp_icon) != 1) LogErr("Invalid Line in (%s): %s", path, cbuf);
				continue;
			}
			// --------------------------------------------------------------------------------------------
			if (!strncmp(cbuf, "MAIN_CLR", 8)) {
				if (sscanf(cbuf, "MAIN_CLR %X", &cfg.txt_main_clr) != 1) LogErr("Invalid Line in (%s): %s", path, cbuf);
				continue;
			}
			// --------------------------------------------------------------------------------------------
			if (!strncmp(cbuf, "SUB_CLR", 7)) {
				if (sscanf(cbuf, "SUB_CLR %X", &cfg.txt_sub_clr) != 1) LogErr("Invalid Line in (%s): %s", path, cbuf);
				continue;
			}
			// --------------------------------------------------------------------------------------------
			if (!strncmp(cbuf, "SCROLL", 6)) {
				if (sscanf(cbuf, "SCROLL %d", &cfg.scroll) != 1) LogErr("Invalid Line in (%s): %s", path, cbuf);
				continue;
			}
		}
		if (bFound == false) {
			LogErr("Configuration not found (%s)", path);
			return;
		}
	}
	else {
		LogErr("Configuration not found (%s)", path);
		return;
	}




	// Create window class for sidebars
	//
	// RegisterClass "SideBarWnd", "Floater" left out: the bars are SideBarWnd widgets (CS_NOCLOSE: mode 1 bars have no caption)

	// ----------------------------------

	hIcons = g_client->gcReadImageFromFile(cfg.bmp_icon);
	hTitle = g_client->gcReadImageFromFile(cfg.bmp_main);
	hSub = g_client->gcReadImageFromFile(cfg.bmp_sub);

	hAppFont = CreateFont(cfg.txt_main_size, cfg.txt_main_weight, cfg.fnt_main);
	hSubFont = CreateFont(cfg.txt_sub_size, cfg.txt_sub_weight, cfg.fnt_sub);
	
	if (!hAppFont) LogErr("Font Not Found [%s]", cfg.fnt_main);
	if (!hSubFont) LogErr("Font Not Found [%s]", cfg.fnt_sub);

	// ----------------------------------


	// Smaple dialog for a proper size and scaling
	//
	QWidget *hDlg = oapiCreateResDialog(hInst, IDD_MESHDEBUG, NULL); // CreateDialogParam
	if (hDlg) DummyDlgProc(hDlg, 0); // WM_INITDIALOG

	QRect r = hDlg ? hDlg->frameGeometry() : QRect(); // GetWindowRect
	width = r.width();

	delete hDlg; // DestroyWindow

	/*
	if (Config->gcGUIMode == 2) {
		Cmd = oapiRegisterCustomCmd("gcGUI Test", "gcGUI Test Program", OpenTestClbk, this);
	}
	else {
		OpenTestClbk(this);
	}*/

	hAppMainWindow->requestActivate(); // SetFocus

	// Must be last one
	g_pWM = this;
}


// ===============================================================================================
//
WindowManager::~WindowManager()
{
	oapiUnregisterCustomCmd(Cmd);

	for(SideBar* sb : sbList) delete sb;
	
	// UnregisterClass left out: no window classes

	if (hTitle) delete hTitle; // DeleteObject
	if (hSub) delete hSub;
	if (hIcons) delete hIcons;
	if (hAppFont) delete hAppFont;
	if (hSubFont) delete hSubFont;
}


// ===============================================================================================
//
bool WindowManager::IsOK() const
{
	if (!hAppFont) return false;
	if (!hSubFont) return false;
	if (!hTitle) return false;
	if (!hSub) return false;
	if (!hIcons) return false;
	return true;
}


// ===============================================================================================
//
QImage *	WindowManager::GetBitmap(int id) const
{
	switch (id) {
	case gcGUI::BM_TITLE: return hTitle;
	case gcGUI::BM_SUBTITLE: return hSub;
	case gcGUI::BM_ICONS: return hIcons;
	}
	return NULL;
}


// ===============================================================================================
//
SideBar * WindowManager::GetSideBar(QWidget *hWnd)
{
	if (sbList.size() == 0) return NULL;
	for (SideBar * sb : sbList) if (sb->GetHWND() == hWnd) return sb;
	return NULL;
}


// ===============================================================================================
// Virtual
//
HNODE WindowManager::RegisterApplication(gcGUIApp *pPtr, const char *label, QWidget *hDlg, DWORD docked, DWORD color)
{

	g_gcGUIAppList.push_back(pPtr);

	SideBar *pSB = NULL;

	if (color == 0) color = 0xC0A020;

	// Always "Float" in this mode
	if (Config->gcGUIMode >= 2) docked = gcGUI::DS_FLOAT;

	if (docked == gcGUI::DS_FLOAT) pSB = NewSideBar(NULL);
	
	if (Config->gcGUIMode == 3) {
		QWidget *hWnd = pSB->GetHWND();
		oapiSetDlgText(hWnd, label); // SetWindowText
	}

	Node *pAp = new Node(pSB, label, hDlg, color, NULL);

	pAp->SetApp(pPtr);
	return HNODE(pAp);
}



// ===============================================================================================
// Virtual
//
HNODE WindowManager::RegisterSubsection(HNODE hNode, const char *label, QWidget *hDlg, DWORD color)
{
	if (APPNODE(hNode)->pParent != NULL) {
		LogErr("RegisterSubsection Failed. Parent cannot be an other subnode");
		return NULL;
	}

	SideBar *pSB = APPNODE(hNode)->GetSideBar();
	if (color == 0) color = pSB->GetAutoColor();
	Node *pAp = new Node(pSB, label, hDlg, color, APPNODE(hNode));
	return HNODE(pAp);
}


// ===============================================================================================
// Virtual
//
void WindowManager::UpdateStatus(HNODE hNode, const char *label, QWidget *hDlg, DWORD color)
{
	Node *pAp = APPNODE(hNode);
	pAp->hDlg = hDlg;
	if (pAp->Label) delete[] pAp->Label;
	if (label) strcpy(pAp->Label = new char[strlen(label) + 1], label); // _strdup: a new[] copy, freed with delete[]
	else pAp->Label = NULL;
	if (color != 0) pAp->ReColorize(color);
	SideBar *pSB = pAp->GetSideBar();
	pSB->RescaleWindow();
	pSB->Invalidate();
}


// ===============================================================================================
// Virtual
//
void WindowManager::DisplayWindow(HNODE hNode, bool bShow)
{
	SideBar *pSB = APPNODE(hNode)->GetSideBar();

	if (pSB) {
		if (pSB->IsFloater()) {
			if (bShow) {
				pSB->ManageButtons();
				pSB->Sort();
				pSB->RescaleWindow();
				pSB->GetHWND()->show(); // ShowWindow SW_SHOW
			}
			else pSB->GetHWND()->hide(); // ShowWindow SW_HIDE
		}
	}
}


// ===============================================================================================
// Virtual
//
QFont * WindowManager::GetFont(int id)
{
	return g_pWM->GetSubTitleFont();
}


// ===============================================================================================
// Virtual
//
QWidget * WindowManager::GetDialog(HNODE hNode)
{
	return ((Node *)(hNode))->hDlg;
}


// ===============================================================================================
// Virtual
//
void WindowManager::UpdateSize(QWidget *hDlg)
{
	HNODE hNode = GetNode(hDlg);
	if (hNode) UpdateStatus(hNode);
}


// ===============================================================================================
// Virtual
//
HNODE WindowManager::GetNode(QWidget *hDlg)
{
	for (SideBar* sb : sbList)
	{
		HNODE hNode = (HNODE)sb->FindNode(hDlg);
		if (hNode) return hNode;
	}
	return NULL;
}


// ===============================================================================================
// Virtual
//
void WindowManager::OpenNode(HNODE hNode, bool bOpen)
{
	APPNODE(hNode)->bOpen = bOpen;
}


// ===============================================================================================
// Virtual
//
bool WindowManager::IsOpen(HNODE hNode)
{
	return APPNODE(hNode)->bOpen;
}


// ===============================================================================================
// Virtual
//
bool WindowManager::UnRegister(HNODE hNode)
{
	if (!DoesExist(APPNODE(hNode))) return false;

	if (APPNODE(hNode)->IsRoot()) return false;

		// Delete/Remove every child node
	/*
		for each (SideBar *sb in sbList)
		{
			list<Node *> remlist;
			for each (Node *pn in sb->wList)if (pn->pParent == hNode) remlist.push_back(pn);
			for each (Node *pn in remlist)
			{
				sb->RemoveWindow(pn);
				delete pn;
			}
	}*/

	SideBar *pSB = APPNODE(hNode)->GetSideBar();
	pSB->RemoveWindow(APPNODE(hNode));
	delete APPNODE(hNode);
	return true;
}

// ===============================================================================================
//
bool WindowManager::DoesExist(Node *pn)
{
	for (SideBar *sb : sbList) if (sb->DoesExists(pn)) return true;
	return false;
}

// ===============================================================================================
//
void WindowManager::UpdateStatus(HNODE hNode)
{
	if (!hNode) return;
	Node *pNode = (Node *)hNode;
	SideBar *pSB = pNode->GetSideBar();
	if (pSB) {
		pSB->RescaleWindow();
		pSB->Invalidate();
	}
}


// ===============================================================================================
//
void WindowManager::CloseWindow(Node *pAp)
{
	SideBar *pSB = pAp->GetSideBar();
	Node *pPar = pAp->pParent;

	if (pPar) {
		pSB->RemoveWindow(pAp);
		pSB->RescaleWindow();
		pSB->Invalidate();

		if (pSB->IsEmpty() && pSB->IsFloater()) ReleaseSideBar(pSB);

		pSB = pPar->GetSideBar();
		pSB->AddWindow(pAp);
		pSB->RescaleWindow();
		pSB->Sort();
		pSB->Invalidate();
	}
	else {
		if (Config->gcGUIMode == 2) {
			for (DWORD i = 0; i < sbList.size(); i++) {
				if (sbList[i] == pSB) {
					sbList.erase(sbList.begin() + i);
					delete pSB;
				}
			}
		}
	}
}

// ===============================================================================================
//
void WindowManager::Animate()
{
	//if (Config->gcGUIMode == 1) for each (SideBar * sb in sbList) if (!sb->IsInactive()) sb->Animate();
}


// ===============================================================================================
//
SideBar *WindowManager::NewSideBar(Node *pAN)
{
	for (SideBar* sb : sbList) if (sb->IsInactive() && sb->IsEmpty()) {
		sb->SetState(gcGUI::DS_FLOAT);
		return sb;
	}

	SideBar *pSB = new SideBar(this, gcGUI::DS_FLOAT);
	sbList.push_back(pSB);
	return pSB;
}


// ===============================================================================================
//
void WindowManager::ReleaseSideBar(SideBar *pSB)
{
	pSB->SetState(gcGUI::INACTIVE);
	pSB->GetHWND()->hide(); // ShowWindow SW_HIDE
}


// ===============================================================================================
//
void WindowManager::SetOffset(int x, int y)
{
	ptOffset = { x, y };
}


// ===============================================================================================
//
SideBar* WindowManager::StartDrag(Node *pAN, int x, int y)
{
	sbDragSrc = pAN->GetSideBar();
	SideBar *pSBOld = pAN->GetSideBar();
	SideBar *pSBNew = NewSideBar(pAN);

	if (pAN->pParent) {
		pSBOld->RemoveWindow(pAN);
		pSBNew->AddWindow(pAN);
	}
	else {
		pSBOld->RemoveWindow(pAN);
		pSBNew->AddWindow(pAN);

		vector<Node*> tmp;

		for (Node * an : pSBOld->wList) if (an->pParent == pAN) tmp.push_back(an);

		for (Node * an : tmp) 
		{
			pSBOld->RemoveWindow(an); // Cant remove directly from a list being browsed 
			pSBNew->AddWindow(an);
		}	
	}

	sbDrag = pSBNew;

	x -= ptOffset.x;
	y -= ptOffset.y;

	pSBNew->ResetWindow(x, y);
	
	pSBNew->GetHWND()->update(); // InvalidateRect
	pSBOld->GetHWND()->update();

	return pSBNew;
}


// ===============================================================================================
//
void WindowManager::BeginMove(Node *pAN, int x, int y)
{
	sbDrag = pAN->GetSideBar();
}


// ===============================================================================================
//
void WindowManager::Drag(int x, int y)
{
	if (sbDrag) {

		if (sbDrag->GetTopNode() == NULL) return;

		QWidget *hBar = sbDrag->GetHWND();
		int w = sbDrag->GetWidth();
		int h = sbDrag->GetHeight();
		x -= ptOffset.x;
		y -= ptOffset.y;

		QRect wa = hBar->screen()->availableGeometry(); // SystemParametersInfo SPI_GETWORKAREA
		RECT rect = { wa.left(), wa.top(), wa.right() + 1, wa.bottom() + 1 };
		int t = sbDrag->GetTopNode()->bm.height();

		if (x < rect.left) x = rect.left;
		if (y < rect.top) y = rect.top;
		if ((x + w) > rect.right) x = rect.right - w;
		if ((y + t) > rect.bottom) y = rect.bottom - t;

		hBar->move(x, y); // SetWindowPos HWND_TOP SWP_SHOWWINDOW
		hBar->resize(w, h);
		hBar->show();
		hBar->raise();
	}
}


// ===============================================================================================
//
void WindowManager::MouseMoved(int x, int y)
{
	RECT r;
	if (hMainWnd) r = { 0, 0, hMainWnd->width(), hMainWnd->height() }; // GetClientRect
	int w = r.right - r.left;
	int h = r.bottom - r.top;
	int q = (width * 3) / 2;
}


// ===============================================================================================
//
void WindowManager::EndDrag()
{
	if (sbDrag) sbDrag->GetHWND()->update(); // InvalidateRect
	sbDrag = NULL;
	sbDragSrc = NULL;
}


// ===============================================================================================
//
SideBar *WindowManager::FindDestination()
{
	if (Config->gcGUIMode != 1) return NULL;

	int z = 0;
	SideBar *pOld = sbDest;
	sbDest = NULL;
	for (SideBar* sb : sbList) {
		if (sb->IsInactive()) continue;
		if (sb != sbDrag) {
			RECT out;
			IntersectRect(&out, ptr(sb->GetRect()), ptr(sbDrag->GetRect()));
			int a = (out.right - out.left) * (out.bottom - out.top);
			if (a > z) { z = a;	sbDest = sb; }
		}
	}
	
	if (sbDest != pOld && pOld != NULL) {
		qInsert.pTgt = NULL;
		qInsert.List.clear();
		if (pOld->GetStyle() == gcGUI::DS_FLOAT) pOld->RescaleWindow();
		pOld->Invalidate();
	}

	qInsert.pTgt = sbDest;
	return sbDest;
}


// ===============================================================================================
// Orbiter Application Main Window Proc
//
bool WindowManager::MainWindowProc(QObject *hWnd, QEvent *event)
{
	static int xpos, ypos;
	switch (event->type()) {

	case QEvent::Leave: // WM_MOUSELEAVE
	case QEvent::MouseButtonPress: // WM_MBUTTONDOWN, WM_LBUTTONDOWN
	case QEvent::MouseButtonRelease: // WM_LBUTTONUP
		return false;

	case QEvent::KeyPress: // WM_KEYDOWN
	{
		return false;
	}

	case QEvent::Wheel: // WM_MOUSEWHEEL
		return false;

	case QEvent::MouseMove: // WM_MOUSEMOVE
		xpos = int(static_cast<QMouseEvent *>(event)->position().x()); // GET_X_LPARAM
		ypos = int(static_cast<QMouseEvent *>(event)->position().y()); // GET_Y_LPARAM
		MouseMoved(xpos, ypos);
		break;

	default:
		break;
	}

	return false;
}








// ===============================================================================================
// SideBar Implementation
// ===============================================================================================
//
SideBar::SideBar(class WindowManager *_pMgr, DWORD _state)
{
	pMgr = _pMgr;
	
	QWindow *hMainWnd = pMgr->GetMainWindow();
	hInst = pMgr->GetInstance();
	width = pMgr->GetWidth();
	state = _state;

	dnNode = NULL;
	dnClose = NULL;
	ypos = 60;
	anim_state = 0;
	rollpos = 0;
	wndlen = 0;
	cidx = 0;
	bOpening = false;
	bIsOpen = false;
	bValidate = true;
	bLock = false;
	bFirstTime = true;
	title_height = 0;
	bWin = pMgr->IsWindowed();

	RECT r = { 0, 0, hMainWnd->width(), hMainWnd->height() }; // GetClientRect
	height = (r.bottom - r.top) - ypos;

	// window styles → window flags: every bar is a tool window owned by the render window (a QWidget can't be a QWindow's child)
	Qt::WindowFlags exstyle = Qt::Tool;
	Qt::WindowFlags style = Qt::FramelessWindowHint; // no WS_CAPTION, no border

	if (Config->gcGUIMode == 1) {	
		if (state == gcGUI::DS_RIGHT) xref = r.right;
		if (state == gcGUI::DS_LEFT) xref = -width;
		if (state == gcGUI::DS_FLOAT) xref = width, height = width;
		if (bWin) {
			if (state == gcGUI::DS_FLOAT) style = Qt::FramelessWindowHint; // WS_CLIPSIBLINGS
			else style = Qt::FramelessWindowHint; // WS_CHILD | WS_CLIPSIBLINGS: placed in the render window's client area below
		} else style = Qt::FramelessWindowHint;
	}

	if (Config->gcGUIMode == 2) {
		state = gcGUI::DS_FLOAT;
		xref = width;
	}

	if (Config->gcGUIMode == 3) {
		state = gcGUI::DS_FLOAT;
		xref = width;
		exstyle = Qt::Tool;
		if (bWin) style = Qt::WindowTitleHint | Qt::WindowCloseButtonHint; // WS_CAPTION | WS_SYSMENU
		else style = Qt::WindowTitleHint | Qt::WindowCloseButtonHint;
	}

	if (state == gcGUI::DS_FLOAT) width += 2, height += 1; // Border

	if (state == gcGUI::DS_FLOAT) hBar = new SideBarWnd("Float", exstyle | style); // CreateWindowExA "Floater" (CS_DROPSHADOW: the WM's shadow)
	else					      hBar = new SideBarWnd("Dock", exstyle | style); // CreateWindowExA "SideBarWnd"

	QPoint p(xref, ypos);
	if (state != gcGUI::DS_FLOAT) p = hMainWnd->mapToGlobal(p); // not upstream: docked bars use the render window's client coordinates
	hBar->move(p);
	hBar->resize(width, height);
	hBar->winId();
	if (hBar->windowHandle()) hBar->windowHandle()->setTransientParent(hMainWnd); // hWndParent: owned by the render window

	// SetWindowLong GWL_STYLE left out: the flags above are the style

	if (Config->gcGUIMode == 3) {
		QRect w = hBar->frameGeometry(), c = hBar->geometry(); // GetWindowRect, GetClientRect
		title_height = w.height() - c.height();
	}

	if (Config->gcGUIMode == 1) hBar->show(); // ShowWindow SW_SHOW
}


// ===============================================================================================
//
SideBar::~SideBar()
{	
	for (Node *v : wList)
	{
		if (v->hDlg) delete v->hDlg; // DestroyWindow
		delete v;
	}
	wList.clear();
	hBar->hide(); // DestroyWindow: deferred, a bar can be closed from its own event handler (CloseWindow)
	hBar->deleteLater();
}


// ===============================================================================================
//
void SideBar::ToggleLock()
{
	bLock = !bLock;
}


// ===============================================================================================
//
void SideBar::ManageButtons()
{
	for (Node* nd : wList)
	{
		nd->bClose = false;

		bool bRootIncluded = false;
		if (nd->pParent) if (nd->pParent->GetSideBar() == this) bRootIncluded = true;
		if (nd->pParent == NULL) bRootIncluded = true;

		if (state == gcGUI::DS_FLOAT && !bRootIncluded) nd->bClose = true;
		if (Config->gcGUIMode == 2) if (nd->pParent == NULL) nd->bClose = true;
	}
}


// ===============================================================================================
//
void SideBar::Invalidate()
{
	ManageButtons();
	hBar->update(); // InvalidateRect
}


// ===============================================================================================
//
void SideBar::ResetWindow(int x, int y)
{
	if (state == gcGUI::DS_FLOAT) {
		height = ComputeLength() + title_height + 1;
	}
	hBar->move(x, y); // SetWindowPos SWP_NOZORDER | SWP_SHOWWINDOW
	hBar->resize(width, height - title_height); // the widget size is the client size
	hBar->show();
}


// ===============================================================================================
//
RECT SideBar::GetRect() const
{
	RECT r = { 0, 0, 0, 0 };
	if (hBar) {
		QRect g = hBar->frameGeometry(); // GetWindowRect
		r = { g.left(), g.top(), g.right() + 1, g.bottom() + 1 };
	}
	return r;
}


// ===============================================================================================
//
void SideBar::Open(bool bO)
{
	if (Config->gcGUIMode >= 2) return;

	if (bLock) {
		bOpening = bIsOpen;
		return;
	}
	if (pMgr->GetDragSource() == this) {
		bOpening = true;
		return;
	}
	if (state == gcGUI::DS_FLOAT) bOpening = true;
	else bOpening = bO;
}

// ===============================================================================================
//
DWORD SideBar::GetAutoColor()
{
	static DWORD color[] = { 0xf5d0ff, 0xfffebb, 0xbbfffc, 0xbaffc4, 0xffc0c0, 0xc6c5ff, 0 };
	DWORD c = color[cidx]; cidx++;
	if (color[cidx] == 0) cidx = 0;
	return c;
}

// ===============================================================================================
//
void SideBar::Animate()
{
	if (bLock && bValidate) return;
	if (bLock && bIsOpen) return;
	if (state == gcGUI::DS_FLOAT) return;

	if (bOpening && anim_state >= 0.9999f) {
		if (bValidate) {
			bIsOpen = true;
			hBar->update(); // InvalidateRect
			bFirstTime = false;
		}
		bValidate = false;
		return;
	}

	if (!bOpening) bIsOpen = false;

	if (!bOpening && anim_state <= 0.0001f) {
		bValidate = true;
		return;
	}

	if (bOpening) anim_state += float(oapiGetSysStep() * 2.5);
	else anim_state -= float(oapiGetSysStep() * 2.5);

	anim_state = saturate(anim_state);

	float as = sin(anim_state * 1.5707f);
	int x = xref;

	if (state == gcGUI::DS_RIGHT) x = xref + int(-as * float(width));
	if (state == gcGUI::DS_LEFT) x = xref + int(as * float(width));


	if (Config->gcGUIMode == 1 && bWin) bFirstTime = true; // GetWindowLong GWL_STYLE & WS_CHILD

	hBar->move(pMgr->GetMainWindow()->mapToGlobal(QPoint(x, ypos))); // MoveWindow (client coordinates of the render window)
	hBar->resize(width, height);
	if (bFirstTime) hBar->update();
}


// ===============================================================================================
//
void SideBar::AddWindow(Node *pAp, bool bSetupOnly)
{
	bFirstTime = true;	// Enable Full redraw
	pAp->pSB = this;
	if (pAp->hDlg) pAp->hDlg->setParent(hBar); // SetParent
	if (!bSetupOnly) wList.push_back(pAp);
}


// ===============================================================================================
//
void SideBar::RemoveWindow(class Node *pAp)
{
	for (DWORD i = 0; i < wList.size(); i++) {
		if (wList[i] == pAp) {
			wList.erase(wList.begin() + i);
			return;
		}
	}
}


// ===============================================================================================
//
bool SideBar::IsOpen() const
{
	if (state == gcGUI::DS_FLOAT) return true;
	return bIsOpen;
}


// ===============================================================================================
//
void SideBar::RescaleWindow()
{
	if (state == gcGUI::DS_FLOAT) {
		height = ComputeLength() + title_height + 1;
	}
	hBar->resize(width, height - title_height); // SetWindowPos SWP_NOMOVE | SWP_NOZORDER | SWP_SHOWWINDOW (client size)
	hBar->show();
}


// ===============================================================================================
//
Node* SideBar::FindNode(QWidget *hDlg)
{
	for (Node* nd : wList) if (nd->hDlg == hDlg) return nd;
	return NULL;
}


// ===============================================================================================
//
void SideBar::GetVisualList(vector<Node*> &tmp)
{
	if (Config->gcGUIMode == 3) {
		for (Node* ap : wList) if (ap->hDlg) tmp.push_back(ap);
	} 
	else 
	{
		for (Node* ap : wList) {
			Node* pPar = ap->pParent;
			if (pPar) {
				if (pPar->pSB == this) {
					if (pPar->bOpen) tmp.push_back(ap);	// Visible Child
					else continue; // Hidden Child
				}
				else tmp.push_back(ap); // Foreing Child
			}
			else tmp.push_back(ap);	// Root Node
		}
	}
}


// ===============================================================================================
//
void SideBar::Sort()
{
	vector<Node*> tmp;
	for (Node* ap : wList) {
		Node* pPar = ap->pParent;
		if (pPar == NULL) {
			tmp.push_back(ap);	// Root Node
			for (Node* ch : wList) if (ch->pParent == ap) tmp.push_back(ch); // Child
		}
		else if (pPar->GetSideBar()!=this) tmp.push_back(ap); // Foreing Child
	}
	wList = tmp;
}


// ===============================================================================================
//
bool SideBar::Insert(Node *pNode, Node *pAfter)
{
	if (!pAfter) {
		wList.insert(wList.begin(), pNode);
		AddWindow(pNode, true);
		return true;
	}
	DWORD i = 0;
	while (i < (wList.size() - 1)) {
		if (wList[i] == pAfter) {
			wList.insert(wList.begin() + i + 1, pNode);
			AddWindow(pNode, true);
			return true;
		}
		i++;
	}
	wList.push_back(pNode);
	AddWindow(pNode, true);
	return true;
}


// ===============================================================================================
//
Node *SideBar::GetTopNode()
{
	if (wList.size() == 0) return NULL;
	return wList.front();
}


// ===============================================================================================
//
bool SideBar::DoesExists(Node *pX)
{
	for (Node* ap : wList) if (ap == pX) return true;
	return false;
}


// ===============================================================================================
//
Node *SideBar::FindClosest(vector<Node*> &vis, Node *pRoot, int yval)
{
	int d = 1000000;
	int y = rollpos;
	Node *out = NULL;
	if (!pRoot) if (abs(y - yval) < d) d = abs(y - yval);
	for (Node* ap : vis) {
		y += ap->CellSize();
		if (ap->pParent == pRoot || ap == pRoot || ap == vis.back()) {
			if (abs(y - yval) < d) d = abs(y - yval), out = ap;
		}
	}
	return out;
}


// ===============================================================================================
//
bool SideBar::TryInsert(SideBar *sbIn)
{
	vector<Node*> drawList;

	GetVisualList(drawList);

	tInsert *pIns = pMgr->InsertList();

	if (pIns->pTgt != this) return false;
	
	map<Node*, Node*> &wIns = pIns->List;
	map<Node*, Node*> wPrev = wIns;

	wIns.clear();

	int yp = sbIn->GetRect().top - GetRect().top;
	int h = sbIn->ComputeLength();
	int y = rollpos;
	
	Node *pNode = sbIn->GetTopNode();
	if (!pNode) return false;

	Node *pParent = pNode->pParent;
	if (!DoesExists(pParent)) pParent = NULL;

	Node *pPlace = FindClosest(drawList, pParent, yp);
	for (Node* an : sbIn->wList) wIns[an] = pPlace;

	return wIns != wPrev;
}


// ===============================================================================================
//
bool SideBar::Apply()
{
	bool bRet = false;

	SideBar *pDG = pMgr->GetDraged();
	tInsert *pIns = pMgr->InsertList();

	if ((pIns->pTgt == this) && (pIns->List.size()>0)) 
	{
		for (auto &var : pIns->List)
		{
			Node *pAfter = var.second;
			Node *pSrc = var.first;
			if (!Insert(pSrc, pAfter)) break;
			pDG->RemoveWindow(pSrc);
			bRet = true;
		}

		if (pDG->IsEmpty()) pMgr->ReleaseSideBar(pDG);

		pIns->List.clear();
		pIns->pTgt = NULL;

		Sort();
		Invalidate();
	}

	return bRet;
}


// ===============================================================================================
//
bool SideBar::SideBarWndProc(QWidget *hWnd, QEvent *event)
{
	static int xpos, ypos;
	static int xof, yof;
	static bool bUpdate = false;

	QWindow *hMain = pMgr->GetMainWindow();

	switch(event->type()) {

	case QEvent::KeyPress: // WM_KEYDOWN
	{
		pMgr->MainWindowProc(hWnd, event);
		break;
	}


	case QEvent::Wheel: // WM_MOUSEWHEEL
	{
		if (GetStyle() == gcGUI::DS_FLOAT) break;

		int old = rollpos;
		short d = short(static_cast<QWheelEvent *>(event)->angleDelta().y()); // GET_WHEEL_DELTA_WPARAM
		if (d>0) rollpos += pMgr->cfg.scroll;
		else rollpos -= pMgr->cfg.scroll;
		int q = height - wndlen;
		if (q > 0) q = 0;
		if (rollpos > 0) rollpos = 0;
		if (rollpos < q) rollpos = q;
		if (old != rollpos) Invalidate();
		break;
	}
	
	case QEvent::MouseButtonPress: // WM_LBUTTONDOWN
	{
		if (static_cast<QMouseEvent *>(event)->button() != Qt::LeftButton) break;
		xpos = int(static_cast<QMouseEvent *>(event)->position().x()); // GET_X_LPARAM
		ypos = int(static_cast<QMouseEvent *>(event)->position().y()); // GET_Y_LPARAM

		for (Node* nd : wList)
		{
			Node *pPar = nd->pParent;

			if (pPar) {
				if (pPar->GetSideBar() == this) {
					if (pPar->bOpen == false) continue;
				}
			}

			if (PointInside(xpos, ypos, &(nd->trect))) {

				if (nd->bClose && PointInside(xpos, ypos, &(nd->crect))) {
					dnClose = nd;
				}
				else {			
					xof = xpos - nd->trect.left;
					yof = ypos - nd->trect.top;				
					dnNode = nd;
				}
				// TrackMouseEvent TME_LEAVE left out: Qt always sends QEvent::Leave
				break; // break for
			}
		}
		break;
	}

	case QEvent::MouseButtonRelease: // WM_LBUTTONUP
	{
		if (static_cast<QMouseEvent *>(event)->button() != Qt::LeftButton) break;
		int xp = int(static_cast<QMouseEvent *>(event)->position().x()); // GET_X_LPARAM
		int yp = int(static_cast<QMouseEvent *>(event)->position().y()); // GET_Y_LPARAM

		SideBar *pDG = pMgr->GetDraged();

		if (pDG) {
			SideBar *pTgt = pMgr->FindDestination();
			if (pTgt) pTgt->Apply();
			pMgr->EndDrag();
			hBar->releaseMouse(); // ReleaseCapture
			dnNode = NULL;
			dnClose = NULL;
			break;
		}
	
		if (dnNode) {
			if (dnNode->GetSideBar() == this) {
				if (PointInside(xp, yp, &(dnNode->trect))) {
					dnNode->bOpen = !dnNode->bOpen;
					if (IsFloater()) RescaleWindow();
					Invalidate();
					DWORD msg = (dnNode->bOpen ? gcGUI::MSG_OPEN_NODE : gcGUI::MSG_CLOSE_NODE);
					dnNode->pApp->clbkMessage(msg, dnNode, 0);
				}
			}
		}

		if (dnClose) {
			if (dnClose->GetSideBar() == this) {
				if (PointInside(xp, yp, &(dnClose->crect))) {
					if (dnClose->pApp->clbkMessage(gcGUI::MSG_CLOSE_APP, NULL, 0))
					{
						pMgr->CloseWindow(dnClose);
					}
				}
			}
		}
		
		dnNode = NULL;
		dnClose = NULL;
		break;
	}


	case QEvent::Leave: // WM_MOUSELEAVE
	{
		dnNode = NULL;
		dnClose = NULL;
		break; 
	}


	case QEvent::MouseMove: // WM_MOUSEMOVE
	{
		int dx = abs(int(static_cast<QMouseEvent *>(event)->position().x()) - xpos);
		int dy = abs(int(static_cast<QMouseEvent *>(event)->position().y()) - ypos);
		int x = int(static_cast<QMouseEvent *>(event)->position().x());
		int y = int(static_cast<QMouseEvent *>(event)->position().y());

		if (Config->gcGUIMode < 3) {

			QPoint g = static_cast<QMouseEvent *>(event)->globalPosition().toPoint(); // ClientToScreen
			POINT scp = { g.x(), g.y() };
				
			// Begin Moving a Window
			//
			if (dnNode && IsFloater() && (GetTopNode() == dnNode) && (dx > 1 || dy > 1))
			{
				hBar->grabMouse(); // SetCapture
				pMgr->SetOffset(xof, yof);
				pMgr->BeginMove(dnNode, scp.x, scp.y);
				dnNode = NULL;
				dnClose = NULL;
				break;
			}

			// Detach a window from a dock
			//
			if (Config->gcGUIMode == 1) {
				if (dnNode && dx > 25) {
					hBar->grabMouse(); // SetCapture
					pMgr->SetOffset(xof, yof);
					SideBar* pTgt = pMgr->StartDrag(dnNode, scp.x, scp.y);
					dnNode = NULL;
					dnClose = NULL;
					break;
				}
			}

			// Move a dragged window and try to insert content into a dock
			//
			if (pMgr->GetDraged()) {
				pMgr->MouseMoved(scp.x, scp.y);
				pMgr->Drag(scp.x, scp.y);
				SideBar *pTgt = pMgr->FindDestination();
				SideBar *pDG = pMgr->GetDraged();
				if (pTgt) if (pTgt->IsOpen()) {
					if (pTgt->TryInsert(pDG)) {
						if (pTgt->GetStyle() == gcGUI::DS_FLOAT) pTgt->RescaleWindow();
						pTgt->Invalidate();
						pDG->Invalidate();
					}
				}
				break;
			}
		}
		break; 
	}

	case QEvent::Paint: // WM_PAINT
		PaintWindow();
		break;
		
	// WM_ERASEBKGND: the widget has WA_OpaquePaintEvent, nothing erases the background
	// WM_CLOSE (mode 3 caption): QWidget hides the bar where DefWindowProc destroyed the window

	default:
		break;
	}


	return false; // DefWindowProc: QWidget's own handling
}


// ===============================================================================================
//
int SideBar::ComputeLength()
{
	int y = 0;
	vector<Node*> drawList;
	GetVisualList(drawList);

	tInsert *pIns = pMgr->InsertList();
	map<Node*, Node*> &wIns = pIns->List;

	bool bInsert = (pIns->pTgt == this) && (wIns.size() > 0);

	if (bInsert) for (auto &var : wIns) if (var.second == NULL) y += var.first->CellSize();

	for (Node* ap : drawList) 
	{
		y += ap->CellSize();
		if (bInsert) for (auto &var : wIns) if (var.second == ap) y += var.first->CellSize();
	}

	wndlen = y;
	return y;
}


// ===============================================================================================
//
void SideBar::PaintWindow()
{
	vector<Node*> drawList;

	GetVisualList(drawList);

	QPainter ps(hBar); // BeginPaint
	QPainter *hDC = &ps;

	int y = rollpos;
	// SetBkMode TRANSPARENT: QPainter draws text without a background by default

	tInsert *pIns = pMgr->InsertList();
	map<Node*, Node*> &wIns = pIns->List;

	bool bInsert = (pIns->pTgt == this) && (wIns.size() > 0);
	
	if (bInsert) for (auto &var : wIns)
	{
		if (var.second == NULL) y = var.first->Spacer(hDC, y);
	}

	for (Node* ap : drawList)
	{
		if (ap != drawList.front()) if (ap->pParent == NULL) {
			RECT fr = { 0, y, width, y + 3 };
			hDC->fillRect(fr.left, fr.top, fr.right - fr.left, fr.bottom - fr.top, Qt::black); // FillRect BLACK_BRUSH
			y += 3;
		}

		y = ap->Paint(hDC, y);

		if (bInsert) for (auto &var : wIns)
		{
			if (var.second == ap) y = var.first->Spacer(hDC, y);
		}
	}

	// Window stack length/height
	wndlen = y - rollpos;

	if (state == gcGUI::DS_FLOAT) {
		hDC->setBrush(Qt::NoBrush); // NULL_BRUSH
		hDC->setPen(QPen(Qt::black, 0)); // BLACK_PEN
		hDC->drawRect(0, 0, width - 1, height - 1); // Rectangle(0, 0, width, height): the outline ends at width - 1, height - 1
	} else {
		if (y < height) {
			hDC->setBrush(QColor(64, 64, 64)); // DKGRAY_BRUSH
			hDC->setPen(Qt::NoPen); // NULL_PEN
			hDC->drawRect(0, y, width, height - y); // Rectangle(0, y, width + 1, height + 1): a NULL_PEN fill is one pixel smaller
		}
	}

	ps.end(); // EndPaint

	// Move dialogs in place
	for (Node* ap : wList) {
		bool bFound = false;
		for (Node* q : drawList) if (q == ap) { bFound = true; break; }
		
		if (bFound && ap->hDlg) {
			if (ap->bOpen) ap->Move();
			else ap->hDlg->hide(); // ShowWindow SW_HIDE
		}
		else if (ap->hDlg) ap->hDlg->hide(); // ShowWindow SW_HIDE
	}
}
