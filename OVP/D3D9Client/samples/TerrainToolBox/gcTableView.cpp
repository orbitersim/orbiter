// ==================================================================
// Copyright (c) 2021 Jarmo Nikkanen
// Licensed under the MIT License
// ==================================================================

#include "gcPropertyTree.h"
#include <sstream>
#include <iomanip>
#include <windowsx.h>
#include <list>
#include <CommCtrl.h>


list<gcPropertyTree *> g_gcPropertyTrees;


// ==================================================================================
//
LRESULT CALLBACK gcPropertyTreeProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	for each (gcPropertyTree * ptr in g_gcPropertyTrees)
		if (ptr->GetHWND() == hWnd) return ptr->WndProc(hWnd, uMsg, wParam, lParam);

	HWND hParent = GetParent(hWnd);

	if (hParent)
		for each (gcPropertyTree * ptr in g_gcPropertyTrees) 
			if (ptr->GetHWND() == hParent) return ptr->WndProc(hWnd, uMsg, wParam, lParam);
	
	return DefWindowProc(hWnd, uMsg, wParam, lParam);
}



// ==================================================================================
//
void gcPropertyTreeInitialize(HINSTANCE hInst)
{
	WNDCLASS wc;
	memset(&wc, 0, sizeof(WNDCLASS));
	wc.style = CS_NOCLOSE | CS_OWNDC | CS_SAVEBITS;
	wc.lpfnWndProc = gcPropertyTreeProc;
	wc.hInstance = hInst;
	wc.hCursor = LoadCursorA(NULL, MAKEINTRESOURCE(IDC_ARROW));
	wc.hbrBackground = (HBRUSH)GetStockObject(WHITE_BRUSH);
	wc.lpszClassName = "gcPropertyTreeCtrl";
	RegisterClass(&wc);
}


// ==================================================================================
//
void gcPropertyTreeRelease(HINSTANCE hInst)
{
	UnregisterClass("gcPropertyTree", hInst);
}


// ==================================================================================
//
gcPropertyTree::gcPropertyTree(gcGUIApp *_pApp, HWND _hWnd, WORD _idc, DLGPROC pCall, HFONT hFnt, HINSTANCE _hInst) : alloc_id('gcTV')
{
	pApp = _pApp;
	pCore = gcGetCoreInterface();

	InitCommonControls();
	g_gcPropertyTrees.push_back(this);
	idc = _idc;
	hWnd = GetDlgItem(_hWnd, idc);
	hDlg = _hWnd;
	hInst = _hInst;
	pCallback = pCall;
	hBuf = NULL;
	pSelected = NULL;
	hBM = NULL;
	hSr = NULL;

	hIcons = pCore->LoadBitmapFromFile("D3D9\\Icons18.png");

	if (!hIcons) {
		oapiWriteLog("gcPropertyTree: FAILED to load Textures/D3D9/Icons18.png");
	}
	if (!pCore) {
		oapiWriteLog("gcPropertyTree: No core interface !!");
	}

	DWORD style = GetWindowLong(hWnd, GWL_STYLE);
	SetWindowLong(hWnd, GWL_STYLE, style | WS_CLIPCHILDREN);
	SetWindowLong(hWnd, GWL_EXSTYLE, WS_EX_STATICEDGE | WS_EX_CONTROLPARENT);
	SetWindowLongPtrA(hWnd, GWLP_USERDATA, (LONG_PTR)this);

	hBr0 = CreateSolidBrush(0xFFFFFF);
	hBr1 = CreateSolidBrush(0xF0F0F0);
	hBr2 = CreateSolidBrush(0xFFFFFF);
	hBrTit[0] = CreateSolidBrush(0xDDFFFF);
	hBrTit[1] = CreateSolidBrush(0xDDFFDD);
	hBrTit[2] = CreateSolidBrush(0xFFFFBB);
	hBr4 = CreateSolidBrush(0xAAFFFF);
	hPen = CreatePen(PS_SOLID, 1, 0x808080);
	hFont = hFnt;

	RECT wc, wd, cr;
	GetWindowRect(hWnd, &wc);
	GetWindowRect(hDlg, &wd);
	
	HWND hCB = CreateComboBox(0);
	SetWindowPos(hCB, NULL, 0, 0, 100, 5, 0);
	GetWindowRect(hCB, &cr);
	DestroyWindow(hCB);

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

	for each (HPROP hp in Data)
	{
		if (hp->hCtrl) DestroyWindow(hp->hCtrl);
		if (hp->pSlider) delete hp->pSlider;
		delete hp;
	}

	if (hBM) DeleteDC(hBM);
	if (hSr) DeleteDC(hSr);
	if (hBuf) DeleteObject(hBuf);

	DeleteObject(hIcons);
	DeleteObject(hBr0);
	DeleteObject(hBr1);
	DeleteObject(hBr2);
	DeleteObject(hBr4);
	DeleteObject(hPen);
	DeleteObject(hBrTit[0]);
	DeleteObject(hBrTit[1]);
	DeleteObject(hBrTit[2]);
}


// ==================================================================================
//
void gcPropertyTree::CopyToClipboard()
{	
	if (!pSelected) return;
	if (OpenClipboard(hWnd)) {
		EmptyClipboard();
		HGLOBAL hData = GlobalAlloc(GMEM_MOVEABLE, pSelected->val.size() + 2);
		if (hData) {
			char *pText = (char *)GlobalLock(hData);
			if (pText) {
				int i = 0;
				while (true) {
					pText[i] = pSelected->val[i];
					if (pText[i] == 0) break;
					i++;
				}
				GlobalUnlock(hData);
				SetClipboardData(CF_TEXT, hData);
			}
		}
		CloseClipboard();
	}
	return;
}


// ==================================================================================
//
void gcPropertyTree::CloseTree(HPROP hPar)
{
	for each (HPROP hp in Data)
	{
		if (hp->parent == hPar) {
			if (hp->bChildren) CloseTree(hp);
			else if (hp->hCtrl) {
				hp->oldx = 65536;
				hp->oldy = 65536;
				ShowWindow(hp->hCtrl, SW_HIDE);
			}
		}
	}
}

// ==================================================================================
//
LRESULT gcPropertyTree::WndProc(HWND hCtrl, UINT uMsg, WPARAM wParam, LPARAM lParam)
{

	// Post Slider Messages to Main DlgProc
	//
	if (uMsg == WM_HSCROLL || uMsg == WM_COMMAND) {
		for each (HPROP hp in Data)	{
			if (hp->style != Style::SLIDER) continue;
			if (hp->hCtrl != HWND(lParam)) continue;
			if (hp->pSlider) {
				if (uMsg == WM_COMMAND) {
					if (LOWORD(wParam) == hp->idc) {
						if (HIWORD(wParam) == EN_SETFOCUS || HIWORD(wParam) == EN_KILLFOCUS) {
							if (pCallback) pCallback(hDlg, WM_COMMAND, MAKELONG(hp->idc, HIWORD(wParam)), LPARAM(hp));
						}
						return 1;
					}
				}
				if (uMsg == WM_HSCROLL) {
					switch (LOWORD(wParam)) {
					case TB_THUMBTRACK:
					case TB_THUMBPOSITION:
					case TB_PAGEUP:
					case TB_PAGEDOWN:
						WORD pos = WORD(SendMessage(hp->hCtrl, TBM_GETPOS, 0, 0));
						hp->pSlider->lin_pos = (double(pos) / 1000.0);
						if (pCallback) pCallback(hDlg, WM_COMMAND, MAKELONG(hp->idc, TB_THUMBTRACK), LPARAM(hp));
						break;
					}
					return 1;
				}
			}
		}
	}


	// Post Edit and ComboBox Messages to Main DlgProc
	//
	if (uMsg == WM_COMMAND) {
		for each (HPROP hp in Data)	{
			if (hp->style == Style::TEXTBOX || hp->style == Style::COMBOBOX) {
				if (hp->hCtrl == HWND(lParam) && hp->hCtrl != NULL)	{
					if (pCallback) pCallback(hDlg, WM_COMMAND, MAKELONG(hp->idc, HIWORD(wParam)), LPARAM(hp));
				}
			}
		}
	}


	// Process gcPropertyTree related stuff
	//
	switch (uMsg) {

	case WM_KEYDOWN:
	{
		if (wParam == 0x43 && GetKeyState(VK_CONTROL) < 0) CopyToClipboard();
		break;
	}

	case WM_KILLFOCUS:
	{
		pSelected = NULL;
		InvalidateRect(hWnd, NULL, false);
		break;
	}

	case WM_MOUSEWHEEL:
		break;

	case WM_LBUTTONDOWN:
	{
		POINT pt = { GET_X_LPARAM(lParam), GET_Y_LPARAM(lParam) };
		for each (HPROP hp in Data) {
			if (PtInRect(&hp->rect, pt) && hp->bVisible) {
				pDown = hp;
				if (hp->bChildren == false && hp->hCtrl == NULL) {
					pSelected = hp;
					SetFocus(hWnd);
					InvalidateRect(hWnd, NULL, false);
					if (pCallback) pCallback(hDlg, WM_COMMAND, MAKELONG(idc, GCGUI_MSG_SELECTED), LPARAM(pSelected));
				}
				break;
			}
		}
		break;
	}

	case WM_LBUTTONUP:
	{
		POINT pt = { GET_X_LPARAM(lParam), GET_Y_LPARAM(lParam) };	
		if (pDown) {
			if (PtInRect(&pDown->rect, pt)) {
				pDown->bOpen = !pDown->bOpen;
				if (pDown->bOpen == false) CloseTree(pDown);
				Update();
			}
		}	
		break;
	}

	case WM_MOUSELEAVE:
	{
		pSelected = NULL;
		InvalidateRect(hWnd, NULL, false);
		break;
	}

	case WM_MOUSEMOVE:
	{
		break;
	}

	case WM_PAINT:
	{
		PAINTSTRUCT ps;
		HDC hDC = BeginPaint(hWnd, &ps);
		Paint(hDC);
		EndPaint(hWnd, &ps);
		break;
	}

	case WM_ERASEBKGND:
		return 1;

	default:
		break;
	}

	return DefWindowProc(hWnd, uMsg, wParam, lParam);
}


// ===============================================================================================
//
void gcPropertyTree::PaintIcon(int x, int y, int id)
{
	DWORD yell = RGB(255, 255, 0);
	DWORD mang = RGB(255, 0, 255);

	if (hIcons) {

		DWORD ck = 0, sx = 0;
		BITMAP ic;
		GetObject(hIcons, sizeof(BITMAP), &ic);

		switch (id) {
		case 0:	sx = ic.bmHeight * 0; ck = yell; break;
		case 1: sx = ic.bmHeight * 1; ck = mang; break;
		case 2: sx = ic.bmHeight * 2; ck = yell; break;
		case 3: sx = ic.bmHeight * 3; ck = mang; break;
		}

		SelectObject(hSr, hIcons);
		int yo = (hlbl - ic.bmHeight) / 2;
		TransparentBlt(hBM, x, y + yo, ic.bmHeight, ic.bmHeight, hSr, sx, 0, ic.bmHeight, ic.bmHeight, ck);
	}
}

// ==================================================================================
//
void gcPropertyTree::Paint(HDC _hDC)
{
	RECT wr; SIZE size;

	if (!hBM) hBM = CreateCompatibleDC(_hDC);
	if (!hSr) hSr = CreateCompatibleDC(_hDC);

	SetTextAlign(hBM, TA_TOP | TA_LEFT);
	SetTextColor(hBM, 0);
	SetBkMode(hBM, TRANSPARENT);
	SelectObject(hBM, hFont);
	SelectObject(hBM, hPen);

	GetWindowRect(hWnd, &wr);

	int w = wr.right - wr.left;
	int h = wr.bottom - wr.top;

	BITMAP ic;
	GetObject(hIcons, sizeof(BITMAP), &ic);

	if (hBuf) {
		BITMAP bm;
		GetObject(hBuf, sizeof(BITMAP), &bm);
		if (bm.bmHeight != h) {
			DeleteObject(hBuf);
			hBuf = NULL;
		}
	}

	if (!hBuf) {
		hBuf = CreateCompatibleBitmap(_hDC, w, h);
		SelectObject(hBM, hBuf);
		RECT rect = { 0, 0, w, h };
		FillRect(hBM, &rect, hBr1);
	}
	else SelectObject(hBM, hBuf);
	
	wlbl = 0;

	for each (HPROP hp in Data)
	{
		hp->bVisible = false;

		if (GetTextExtentExPointA(hBM, hp->label.c_str(), hp->label.size(), 100000, NULL, NULL, &size)) {
			if (hp->bChildren) {
				if (hp->val.size() != 0) size.cx += ic.bmHeight;
				else size.cx = 0;
			} 
			if (size.cx > wlbl) wlbl = size.cx;
		}
	}

	wlbl += 8*3;
	bOdd = false;

	HRGN hRgn = CreateRectRgn(0, 0, w, h);
	SelectClipRgn(_hDC, hRgn);

	int y = PaintSection(_hDC, NULL, 0, wlbl, 0, 0);
	
	if (!BitBlt(_hDC, 0, 0, w, h, hBM, 0, 0, SRCCOPY))
	{
		oapiWriteLogV("gcPropertyTree: BitBlt Failed Error=%u", GetLastError());
	}

	for each (HPROP hp in Data)	if (hp->bVisible) if (hp->hCtrl) InvalidateRect(hp->hCtrl, NULL, false);
	
	SelectClipRgn(_hDC, NULL);
	DeleteObject(hRgn);
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
int gcPropertyTree::PaintSection(HDC _hDC, HPROP hPar, int ident, int wlbl, int y, int lvl)
{

	RECT wr;
	GetWindowRect(hWnd, &wr);

	int w = wr.right - wr.left;
	
	BITMAP ic;
	GetObject(hIcons, sizeof(BITMAP), &ic);

	for each (HPROP hp in Data)
	{
		if (hp->bOn == false) continue;
		if (hp->parent != hPar) continue;
		
		hp->bVisible = true;

		int z = 2;
		int n = 5;
			
		RECT rect = { ident, y, w, y + hlbl };

		hp->rect = rect;

		HBRUSH hSel = 0;
		if (bOdd) hSel = hBr0;
		else	  hSel = hBr1;
		if (pSelected == hp) hSel = hBr4;

		bOdd = !bOdd;

		if (hp->bChildren) FillRect(hBM, &rect, hBrTit[lvl%3]);
		else FillRect(hBM, &rect, hSel);

		MoveToEx(hBM, ident, y + hlbl - 1, NULL);
		LineTo(hBM, w, y + hlbl - 1);

		if (hp->bChildren == false || hp->val.size() > 0) {
			MoveToEx(hBM, wlbl, y, NULL);
			LineTo(hBM, wlbl, y + hlbl);
		}

		// Title bar for a sub section
		if (hp->bChildren) {

			if (hp->bOpen) PaintIcon(ident, y - 1, 0);
			else PaintIcon(ident, y - 1, 1);

			TextOut(hBM, ident + ic.bmHeight + n/2, y + z, hp->label.c_str(), hp->label.size());
			TextOut(hBM, wlbl + n, y + z, hp->val.c_str(), hp->val.size());

			y += hlbl;

			if (hp->bOpen) {
				int slen = GetSubsentionLength(hp);
				SelectObject(hBM, hBr4);
				Rectangle(hBM, ident - 1, y - 1, ident + 4, y + slen);
				y = PaintSection(_hDC, hp, ident + 4, wlbl, y, lvl + 1);
			}

			continue;
		}
		
		TextOut(hBM, n + ident, y + z, hp->label.c_str(), hp->label.size());

		if (!hp->hCtrl)	TextOut(hBM, wlbl + n, y + z, hp->val.c_str(), hp->val.size());
		else {

			// Textbox
			if (hp->style == Style::TEXTBOX) {
				int l = wlbl + 5;
				int t = y + 2;
				int r = w - 1;
				int b = y + hlbl - 2;
				RECT re = { wlbl + 1, y, w, y + hlbl - 1 };
				FillRect(hBM, &re, hBr2);
				ExcludeClipRect(_hDC, l, t, r, b);
				if (HasMoved(hp, l, t))	SetWindowPos(hp->hCtrl, NULL, l, t, r - l, b - t, SWP_SHOWWINDOW);
			}

			// Combobox
			if (hp->style == Style::COMBOBOX) {
				int l = wlbl;
				int t = y - 1;
				int r = w - 1;
				int b = y + hlbl;
				ExcludeClipRect(_hDC, l, t, r, b);
				if (HasMoved(hp, l, t))	SetWindowPos(hp->hCtrl, NULL, l, t, r - l, b - t, SWP_SHOWWINDOW);
			}

			// Slider
			if (hp->style == Style::SLIDER) {
				int l = wlbl + 1;
				int t = y;
				int r = w - 2;
				int b = y + hlbl - 1;
				ExcludeClipRect(_hDC, l, t, r, b);
				if (HasMoved(hp, l, t))	SetWindowPos(hp->hCtrl, NULL, l, t, r - l, b - t, SWP_SHOWWINDOW);
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
	for each (HPROP hp in Data)
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
	GetWindowRect(hDlg, &wd);
	int w = (wd.right - wd.left);
	int h = GetSubsentionLength(NULL);
	
	if (h != len) {
		SetWindowPos(hDlg, NULL, 0, 0, w, h + tmrg + bmrg, SWP_NOMOVE | SWP_NOZORDER | SWP_SHOWWINDOW);
		SetWindowPos(hWnd, NULL, wmrg, tmrg, w - wmrg * 2, h, SWP_NOZORDER | SWP_SHOWWINDOW);
		pApp->UpdateSize(hDlg);
		len = h;
	}

	InvalidateRect(hWnd, NULL, true);
}


// ==================================================================================
//
HWND gcPropertyTree::GetHWND() const 
{ 
	return hWnd;
}


// ==================================================================================
//
LRESULT gcPropertyTree::SendCtrlMessage(HPROP hCtrl, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	if (hCtrl) if (hCtrl->hCtrl) return SendMessageA(hCtrl->hCtrl, uMsg, wParam, lParam);
	return 0;
}


// ==================================================================================
//
HWND gcPropertyTree::CreateEditControl(WORD id, bool bReadOnly)
{
	DWORD dwStyle = WS_CHILD | WS_VISIBLE;
	if (bReadOnly) dwStyle |= ES_READONLY;
	HWND hEdit = CreateWindowExA(0, "EDIT", NULL, dwStyle,
		0, 0, 0, 0, hWnd, (HMENU)id, (HINSTANCE)GetWindowLongPtr(hWnd, GWLP_HINSTANCE), NULL);
	SendMessageA(hEdit, WM_SETFONT, (WPARAM)hFont, 0);
	return hEdit;
}


// ==================================================================================
//
HWND gcPropertyTree::CreateComboBox(WORD id)
{
	DWORD dwStyle = WS_CHILD | WS_VISIBLE | CBS_DROPDOWN;
	HWND hEdit = CreateWindowExA(WS_EX_TRANSPARENT | WS_EX_CLIENTEDGE, "COMBOBOX", NULL, dwStyle,
		0, 0, 0, 0, hWnd, (HMENU)id, (HINSTANCE)GetWindowLongPtr(hWnd, GWLP_HINSTANCE), NULL);
	SendMessageA(hEdit, WM_SETFONT, (WPARAM)hFont, 0);
	return hEdit;
}


// ==================================================================================
//
HWND gcPropertyTree::CreateSlider(WORD id)
{
	DWORD dwStyle = WS_CHILD | WS_VISIBLE | TBS_NOTICKS | TBS_TRANSPARENTBKGND | TBS_BOTH;
	HWND hEdit = CreateWindowExA(WS_EX_TRANSPARENT, TRACKBAR_CLASS, NULL, dwStyle,
		0, 0, 0, 0, hWnd, (HMENU)id, (HINSTANCE)GetWindowLongPtr(hWnd, GWLP_HINSTANCE), NULL);
	SendMessage(hEdit, TBM_SETRANGE, (WPARAM)TRUE, (LPARAM)MAKELONG(0, 1000));
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
HPROP gcPropertyTree::GetEntry(HWND hCtrl)
{
	if (hCtrl == NULL) return NULL;
	for each (HPROP hp in Data) if (hp->hCtrl == hCtrl) return hp;
	return NULL;
}

void* gcPropertyTree::GetUserRef(HPROP hEntry)
{
	if (hEntry) return hEntry->pUser;
	return NULL;
}


// ==================================================================================
//
HWND gcPropertyTree::GetControl(HPROP hEntry)
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
void gcPropertyTree::SetValue(HPROP hEntry, const string &val)
{
	hEntry->val = val;
}


// ==================================================================================
//
void  gcPropertyTree::SetValue(HPROP hEntry, const char *lbl)
{
	hEntry->val = string(lbl);
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
		SendMessageA(hSlider->hCtrl, TBM_SETPOS, 1, WORD(pSl->lin_pos*1000.0));
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
	GetWindowTextA(hTextBox->hCtrl, buffer, sizeof(buffer));
	return string(buffer);
}

// ==================================================================================
//
void gcPropertyTree::SetTextBoxContent(HPROP hTextBox, string text)
{
	if (hTextBox->style != Style::TEXTBOX) return;
	if (!hTextBox->hCtrl) return;
	SetWindowTextA(hTextBox->hCtrl, text.c_str());
}

// ==================================================================================
//
void gcPropertyTree::SetComboBoxSelection(HPROP hCombo, int idx)
{
	if (hCombo->style != Style::COMBOBOX) return;
	if (!hCombo->hCtrl) return;
	SendMessage(hCombo->hCtrl, CB_SETCURSEL, idx, 0);
}

// ==================================================================================
//
int	gcPropertyTree::GetComboBoxSelection(HPROP hCombo)
{
	if (hCombo->style != Style::COMBOBOX) return 0;
	if (!hCombo->hCtrl) return 0;
	return int(SendMessage(hCombo->hCtrl, CB_GETCURSEL, 0, 0));
}

// ==================================================================================
//
void gcPropertyTree::ClearComboBox(HPROP hCombo)
{
	if (hCombo->style != Style::COMBOBOX) return;
	if (!hCombo->hCtrl) return;
	SendMessage(hCombo->hCtrl, CB_RESETCONTENT, 0, 0);
}

// ==================================================================================
//
int gcPropertyTree::AddComboBoxItem(HPROP hCombo, const char *label)
{
	if (hCombo->style != Style::COMBOBOX) return -1;
	if (!hCombo->hCtrl) return -1;
	return int(SendMessage(hCombo->hCtrl, CB_ADDSTRING, 0, (LPARAM)label));
}
