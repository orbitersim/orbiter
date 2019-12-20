
// =================================================================================================================================
//
// Copyright (C) 2019 Jarmo Nikkanen
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


#include <d3d9.h>
#include <d3dx9.h>
#include "WindowMgr.h"
#include "windows.h"
#include "WindowsX.h"
#include "resource.h"
#include "OapiExtension.h"
#include "D3D9Config.h"
#include "Log.h"
#include "D3D9Client.h"

#define APPNODE(x) ((Node *)x)

extern D3D9Client *g_client;

class WindowManager *g_pWM = NULL;

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
//
LRESULT CALLBACK SideBarWndProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	if (g_pWM) {
		SideBar *pBar = g_pWM->GetSideBar(hWnd);
		if (pBar) return pBar->SideBarWndProc(hWnd, uMsg, wParam, lParam);
	}
	return DefWindowProc(hWnd, uMsg, wParam, lParam);
}
// ===============================================================================================
//
BOOL CALLBACK DummyDlgProc(HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg) {
	case WM_INITDIALOG:
		return true;
	}
	return false;
}












// ===============================================================================================
//
BOOL CALLBACK DlgProc(HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg) {
	case WM_INITDIALOG:
		return true;
	}
	return false;
}
// ===============================================================================================
//
void OpenTestClbk(void *context)
{

	WindowManager *pWM = (WindowManager *)context;
	HINSTANCE hInst = pWM->GetInstance();
	HWND hAppMainWindow = pWM->GetMainWindow();


	HWND hDlg = CreateDialogParam(hInst, MAKEINTRESOURCE(IDD_MESHDEBUG), hAppMainWindow, DlgProc, 0);
	HNODE hRootNode = pWM->RegisterApplication("D3D9 Controls", NULL, 0xBB9900, gcGUI::LEFT);

	pWM->RegisterSubsection(hRootNode, "Mesh Debugger", hDlg, 0xF0B0F0);

	hDlg = CreateDialogParam(hInst, MAKEINTRESOURCE(IDD_MATERIAL), hAppMainWindow, DlgProc, 0);
	pWM->RegisterSubsection(hRootNode, "Material Config", hDlg, 0xFFD0B0);

	hDlg = CreateDialogParam(hInst, MAKEINTRESOURCE(IDD_SCENEDEBUG), hAppMainWindow, DlgProc, 0);
	HNODE hSD = pWM->RegisterSubsection(hRootNode, "Scene Debugger", hDlg, 0xB0FFD0);

	hDlg = CreateDialogParam(hInst, MAKEINTRESOURCE(IDD_MICROTEXTOOLS), hAppMainWindow, DlgProc, 0);
	HNODE hTT = pWM->RegisterSubsection(hRootNode, "MicroTex Tools", hDlg, 0xB0D0FF);

	pWM->OpenNode(hSD, false);
	pWM->OpenNode(hTT, false);

	pWM->DisplayWindow(hRootNode);
}














// ===============================================================================================
// Node Implementation
// ===============================================================================================
//
Node::Node(SideBar *pSB, const char *label, HWND hDlg, DWORD color, Node *pP) :
	pSB(pSB), pParent(pP), hBmp(NULL), hDlg(hDlg), bOpen(true), bClose(false)
{

	memset(&bm, 0, sizeof(BITMAP));

	WindowManager *pMgr = pSB->GetWM();

	pSB->AddWindow(this);

	if (label) strncpy_s(Label, 32, label, 31);
	else strcpy_s(Label, 32, "");

	HBITMAP hTit;

	if ((pParent == NULL) && (Config->gcGUIMode == 3) && hDlg) return;	// No Title Bar

	if (pParent == NULL) hTit = pMgr->GetBitmap(gcGUI::BM_TITLE);
	else hTit = pMgr->GetBitmap(gcGUI::BM_SUBTITLE);

	FVECTOR4 clr = _Colour(color);
	FVECTOR4 white = _Colour(0xFFFFFFFF);

	HDC hDC = pSB->GetDC();
	HDC hSrc = CreateCompatibleDC(hDC);
	HDC hTgt = CreateCompatibleDC(hDC);

	
	GetObject(hTit, sizeof(BITMAP), &bm);
	
	hBmp = CreateCompatibleBitmap(hDC, bm.bmWidth, bm.bmHeight);

	pSB->ReleaseDC(hDC);

	SelectObject(hSrc, hTit);
	SelectObject(hTgt, hBmp);

	// Recolorize the title bar

	for (int y = 0; y < bm.bmHeight; y++) {
		for (int x = 0; x < bm.bmWidth; x++) {		
			COLORREF cr = GetPixel(hSrc, x, y);
			FVECTOR4 c = _Colour(cr);
			FVECTOR4 out = (clr * c.b) + (white * c.g);
			SetPixel(hTgt, x, y, _Colour(&out));	
		}
	}

	DeleteDC(hSrc);
	DeleteDC(hTgt);
}


// ===============================================================================================
//
int Node::CellSize()
{
	int y = 0;
	if (hBmp) y += bm.bmHeight;
	if (bOpen && hDlg) {
		RECT r;
		GetWindowRect(hDlg, &r);
		y += (r.bottom - r.top);
	}
	return y;
}


// ===============================================================================================
//
int Node::Paint(HDC hDC, int y)
{
	WindowManager *pMgr = pSB->GetWM();

	int width = pSB->GetWidth();
	int x = 0;
	int wof = 0, hof = 0;
	DWORD ck = 0;

	if (pSB->GetStyle() == gcGUI::FLOAT) x += 1;	
		
	if (hBmp) {
		if (pParent) {
			SelectObject(hDC, pMgr->GetSubTitleFont());
			SetTextColor(hDC, pMgr->cfg.txt_sub_clr);
			wof = pMgr->cfg.txt_sub_x;
			hof = pMgr->cfg.txt_sub_y;
		}
		else {
			SelectObject(hDC, pMgr->GetAppTitleFont());
			SetTextColor(hDC, pMgr->cfg.txt_main_clr);
			wof = pMgr->cfg.txt_main_x;
			hof = pMgr->cfg.txt_main_y;
		}
	}

	
	// Draw Title Bars -----------------------------
	//
	if (hBmp) {
		
		HDC hSr = CreateCompatibleDC(hDC);
		int z = width - bm.bmHeight - 3;

		trect = { 0, y, width, y + bm.bmHeight };
		crect = { z, y, width, y + bm.bmHeight };

		SelectObject(hSr, hBmp);
		BitBlt(hDC, x, y, width - 10,  bm.bmHeight, hSr, 0, 0, SRCCOPY);
		BitBlt(hDC, width - 10 - x, y, 10, bm.bmHeight, hSr, bm.bmWidth - 10, 0, SRCCOPY);	
		TextOut(hDC, wof, y + hof, Label, strlen(Label));
		
		DeleteDC(hSr);

		if (bOpen) PaintIcon(hDC, x, y, 0);
		else PaintIcon(hDC, x, y, 1);
		if (bClose) PaintIcon(hDC, z, y, 2);
			
		y += bm.bmHeight;
	}

	pos = { x, y };
	
	if (bOpen && hDlg) {
		RECT r;
		GetWindowRect(hDlg, &r);
		y += (r.bottom - r.top);
	}

	return y;
}


// ===============================================================================================
//
void Node::PaintIcon(HDC hDC, int x, int y, int id)
{
	WindowManager *pMgr = pSB->GetWM();
	DWORD yell = RGB(255, 255, 0); 
	DWORD mang = RGB(255, 0, 255); 
	
	HBITMAP hIco = pMgr->GetBitmap(gcGUI::BM_ICONS);

	if (hIco) {
		DWORD ck = 0, sx = 0;
		BITMAP ic;
		GetObject(hIco, sizeof(BITMAP), &ic);

		switch (id) {
		case 0:	sx = ic.bmHeight * 0; ck = yell; break;
		case 1: sx = ic.bmHeight * 1; ck = mang; break;
		case 2: sx = ic.bmHeight * 2; ck = yell; break;
		case 3: sx = ic.bmHeight * 3; ck = mang; break;
		}

		HDC hSr = CreateCompatibleDC(hDC);
		SelectObject(hSr, hIco);
		int yo = (bm.bmHeight - ic.bmHeight) / 2;
		TransparentBlt(hDC, x, y + yo, ic.bmHeight, ic.bmHeight, hSr, sx, 0, ic.bmHeight, ic.bmHeight, ck);
		DeleteDC(hSr);
	}
}


// ===============================================================================================
//
int Node::Spacer(HDC hDC, int y)
{
	WindowManager *pMgr = pSB->GetWM();

	if (pParent) if (pParent->bOpen == false) if (pParent->pSB == pSB) return y;

	int width = pSB->GetWidth();
	int h = CellSize();
	
	SelectObject(hDC, (HBRUSH)GetStockObject(GRAY_BRUSH));
	SelectObject(hDC, (HPEN)GetStockObject(NULL_PEN));
	Rectangle(hDC, 0, y, width + 1, y + h + 1);
	
	return y + h;
}


// ===============================================================================================
//
void Node::Move()
{
	if (!hDlg) return;
	SetWindowPos(hDlg, NULL, pos.x, pos.y, 0, 0, SWP_NOSIZE | SWP_NOZORDER | SWP_SHOWWINDOW);
}


// ===============================================================================================
//
Node::~Node()
{
	if (hBmp) DeleteObject(hBmp);
}








// ===============================================================================================
// WindowManager Implementation
// ===============================================================================================
//
WindowManager::WindowManager(HWND hAppMainWindow, HINSTANCE _hInst, bool bWindowed)
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
	sbLeft = NULL;
	sbRight = NULL;

	hIcons = hTitle = hSub = NULL;

	RECT rMain;
	GetClientRect(hAppMainWindow, &rMain);

	AutoFile file;

	if (file.IsInvalid()) {
		sprintf_s(path, 256, "%sgcGUI.cfg", OapiExtension::GetConfigDir());
		fopen_s(&file.pFile, path, "r");
	}

	if (!file.IsInvalid()) {

		int q, w;
		bool bFound = false;

		while (fgets2(cbuf, 256, file.pFile, 0x0A) >= 0)
		{
			// --------------------------------------------------------------------------------------------
			if (!strncmp(cbuf, "RESOLUTION", 10)) {
				if (sscanf_s(cbuf, "RESOLUTION %d %d", &q, &w) != 2) LogErr("Invalid Line in (%s): %s", path, cbuf);
				if (q < rMain.bottom && rMain.bottom < w) bFound = true;
				continue;
			}
			if (bFound == false) continue;

			if (!strncmp(cbuf, "END", 3)) break;

			// --------------------------------------------------------------------------------------------
			if (!strncmp(cbuf, "FONT_MAIN", 9)) {
				if (sscanf_s(cbuf, "FONT_MAIN \"%[^\"]\" %d %d", cfg.fnt_main, 32, &cfg.txt_main_size, &cfg.txt_main_weight) != 3) LogErr("Invalid Line in (%s): %s", path, cbuf);
				continue;
			}
			// --------------------------------------------------------------------------------------------
			if (!strncmp(cbuf, "FONT_SUB", 8)) {
				if (sscanf_s(cbuf, "FONT_SUB \"%[^\"]\" %d %d", cfg.fnt_sub, 32, &cfg.txt_sub_size, &cfg.txt_sub_weight) != 3) LogErr("Invalid Line in (%s): %s", path, cbuf);
				continue;
			}
			// --------------------------------------------------------------------------------------------
			if (!strncmp(cbuf, "MAIN_OFS", 8)) {
				if (sscanf_s(cbuf, "MAIN_OFS %d %d", &cfg.txt_main_x, &cfg.txt_main_y) != 2) LogErr("Invalid Line in (%s): %s", path, cbuf);
				continue;
			}
			// --------------------------------------------------------------------------------------------
			if (!strncmp(cbuf, "SUB_OFS", 7)) {
				if (sscanf_s(cbuf, "SUB_OFS %d %d", &cfg.txt_sub_x, &cfg.txt_sub_y) != 2) LogErr("Invalid Line in (%s): %s", path, cbuf);
				continue;
			}
			// --------------------------------------------------------------------------------------------
			if (!strncmp(cbuf, "MAIN_BMP", 8)) {
				if (sscanf_s(cbuf, "MAIN_BMP %s", cfg.bmp_main, 32) != 1) LogErr("Invalid Line in (%s): %s", path, cbuf);
				continue;
			}
			// --------------------------------------------------------------------------------------------
			if (!strncmp(cbuf, "SUB_BMP", 7)) {
				if (sscanf_s(cbuf, "SUB_BMP %s", cfg.bmp_sub, 32) != 1) LogErr("Invalid Line in (%s): %s", path, cbuf);
				continue;
			}
			// --------------------------------------------------------------------------------------------
			if (!strncmp(cbuf, "ICON_BMP", 8)) {
				if (sscanf_s(cbuf, "ICON_BMP %s", cfg.bmp_icon, 32) != 1) LogErr("Invalid Line in (%s): %s", path, cbuf);
				continue;
			}
			// --------------------------------------------------------------------------------------------
			if (!strncmp(cbuf, "MAIN_CLR", 8)) {
				if (sscanf_s(cbuf, "MAIN_CLR %X", &cfg.txt_main_clr) != 1) LogErr("Invalid Line in (%s): %s", path, cbuf);
				continue;
			}
			// --------------------------------------------------------------------------------------------
			if (!strncmp(cbuf, "SUB_CLR", 7)) {
				if (sscanf_s(cbuf, "SUB_CLR %X", &cfg.txt_sub_clr) != 1) LogErr("Invalid Line in (%s): %s", path, cbuf);
				continue;
			}
			// --------------------------------------------------------------------------------------------
			if (!strncmp(cbuf, "SCROLL", 6)) {
				if (sscanf_s(cbuf, "SCROLL %d", &cfg.scroll) != 1) LogErr("Invalid Line in (%s): %s", path, cbuf);
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
	DWORD flags = 0;
	if (Config->gcGUIMode == 1) flags |= CS_NOCLOSE;

	WNDCLASS wc; 
	memset(&wc, 0, sizeof(WNDCLASS));
	wc.style = flags | CS_OWNDC | CS_SAVEBITS;
	wc.lpfnWndProc = SideBarWndProc;
	wc.hInstance = hInst;
#pragma warning(disable:4302)
	wc.hCursor = LoadCursorA(NULL, MAKEINTRESOURCE(IDC_ARROW));
	wc.hbrBackground = (HBRUSH)GetStockObject(BLACK_BRUSH);
	wc.lpszClassName = "SideBarWnd";

	RegisterClass(&wc);

	memset(&wc, 0, sizeof(WNDCLASS));
	wc.style = flags | CS_OWNDC | CS_SAVEBITS | CS_DROPSHADOW;
	wc.lpfnWndProc = SideBarWndProc;
	wc.hInstance = hInst;
	wc.hCursor = LoadCursorA(NULL, MAKEINTRESOURCE(IDC_ARROW));
#pragma warning(default:4302)
	wc.hbrBackground = (HBRUSH)GetStockObject(BLACK_BRUSH);
	wc.lpszClassName = "Floater";

	RegisterClass(&wc);

	// ----------------------------------

	sprintf_s(path, sizeof(path), "%sD3D9\\%s", OapiExtension::GetTextureDir(), cfg.bmp_icon);
	hIcons = g_client->gcReadImageFromFile(path);
	if (!hIcons) LogErr("FileNotFound: %s", path);

	// ----------------------------------

	sprintf_s(path, sizeof(path), "%sD3D9\\%s", OapiExtension::GetTextureDir(), cfg.bmp_main);
	hTitle = g_client->gcReadImageFromFile(path);
	if (!hTitle) LogErr("FileNotFound: %s", path);

	// ----------------------------------

	sprintf_s(path, sizeof(path), "%sD3D9\\%s", OapiExtension::GetTextureDir(), cfg.bmp_sub);
	hSub = g_client->gcReadImageFromFile(path);
	if (!hSub) LogErr("FileNotFound: %s", path);

	// ----------------------------------

	hAppFont = CreateFont(cfg.txt_main_size, 0, 0, 0, cfg.txt_main_weight, false, false, 0, 0, 0, 2, CLEARTYPE_QUALITY, 49, cfg.fnt_main);
	hSubFont = CreateFont(cfg.txt_sub_size, 0, 0, 0, cfg.txt_sub_weight, false, false, 0, 0, 0, 2, CLEARTYPE_QUALITY, 49, cfg.fnt_sub);
	
	if (!hAppFont) LogErr("Font Not Found [%s]", cfg.fnt_main);
	if (!hSubFont) LogErr("Font Not Found [%s]", cfg.fnt_sub);

	// ----------------------------------


	// Smaple dialog for a proper size and scaling
	//
	HWND hDlg = CreateDialogParam(hInst, MAKEINTRESOURCE(IDD_MESHDEBUG), hAppMainWindow, DummyDlgProc, 0);

	RECT r;
	GetWindowRect(hDlg, &r);
	width = (r.right - r.left);

	DestroyWindow(hDlg);


	if (Config->gcGUIMode == 1) {
		sbLeft = new SideBar(this, gcGUI::LEFT);
		sbRight = new SideBar(this, gcGUI::RIGHT);
		sbList.push_back(sbLeft);
		sbList.push_back(sbRight);
	}

	if (Config->gcGUIMode == 2) {
		Cmd = oapiRegisterCustomCmd("gcGUI Test", "gcGUI Test Program", OpenTestClbk, this);
	}
	else {
		OpenTestClbk(this);
	}

	// Must be last one
	g_pWM = this;
}


// ===============================================================================================
//
WindowManager::~WindowManager()
{
	oapiUnregisterCustomCmd(Cmd);

	for each (SideBar* sb in sbList) delete sb;
	
	UnregisterClass("SideBarWnd", hInst);
	UnregisterClass("Floater", hInst);

	if (hTitle) DeleteObject(hTitle);
	if (hSub) DeleteObject(hSub);
	if (hIcons) DeleteObject(hIcons);
	if (hAppFont) DeleteObject(hAppFont);
	if (hSubFont) DeleteObject(hSubFont);
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
HBITMAP	WindowManager::GetBitmap(int id) const
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
SideBar * WindowManager::GetSideBar(HWND hWnd)
{
	if (sbList.size() == 0) return NULL;
	for each (SideBar * sb in sbList) if (sb->GetHWND() == hWnd) return sb;
	return NULL;
}


// ===============================================================================================
//
HNODE WindowManager::RegisterApplication(const char *label, HWND hDlg, DWORD color, DWORD docked)
{
	SideBar *pSB = NULL;

	// Always "Float" in this mode
	if (Config->gcGUIMode >= 2) docked = gcGUI::FLOAT;

	if (docked == gcGUI::LEFT) pSB = sbLeft;
	if (docked == gcGUI::RIGHT) pSB = sbRight;
	if (docked == gcGUI::FLOAT) pSB = NewSideBar(NULL);
	
	if (Config->gcGUIMode == 3) {
		HWND hWnd = pSB->GetHWND();
		SetWindowText(hWnd, label);
	}

	Node *pAp = new Node(pSB, label, hDlg, color);
	return HNODE(pAp);
}



// ===============================================================================================
//
HNODE WindowManager::RegisterSubsection(HNODE hNode, const char *label, HWND hDlg, DWORD color)
{
	if (APPNODE(hNode)->pParent != NULL) {
		LogErr("RegisterSubsection Failed. Parent cannot be an other subnode");
		return NULL;
	}

	SideBar *pSB = APPNODE(hNode)->GetSideBar();
	Node *pAp = new Node(pSB, label, hDlg, color, APPNODE(hNode));
	return HNODE(pAp);
}


// ===============================================================================================
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
				ShowWindow(pSB->GetHWND(), SW_SHOW);
			}
			else ShowWindow(pSB->GetHWND(), SW_HIDE);
		}
	}
}


// ===============================================================================================
//
void WindowManager::OpenNode(HNODE hNode, bool bOpen)
{
	APPNODE(hNode)->bOpen = bOpen;
}


// ===============================================================================================
//
bool WindowManager::UnRegister(HNODE hNode)
{
	SideBar *pSB = APPNODE(hNode)->GetSideBar();
	pSB->RemoveWindow(APPNODE(hNode));
	return true;
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
	if (Config->gcGUIMode == 1) for each (SideBar * sb in sbList) if (!sb->IsInactive()) sb->Animate();
}


// ===============================================================================================
//
SideBar *WindowManager::NewSideBar(Node *pAN)
{
	for each (SideBar* sb in sbList) if (sb->IsInactive() && sb->IsEmpty()) {
		sb->SetState(gcGUI::FLOAT);
		return sb;
	}

	SideBar *pSB = new SideBar(this, gcGUI::FLOAT);
	sbList.push_back(pSB);
	return pSB;
}


// ===============================================================================================
//
void WindowManager::ReleaseSideBar(SideBar *pSB)
{
	pSB->SetState(gcGUI::INACTIVE);
	ShowWindow(pSB->GetHWND(), SW_HIDE);
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

		for each (Node * an in pSBOld->wList) if (an->pParent == pAN) tmp.push_back(an);

		for each (Node * an in tmp) 
		{
			pSBOld->RemoveWindow(an); // Cant remove directly from a list being browsed 
			pSBNew->AddWindow(an);
		}	
	}

	sbDrag = pSBNew;

	x -= ptOffset.x;
	y -= ptOffset.y;

	pSBNew->ResetWindow(x, y);
	
	InvalidateRect(pSBNew->GetHWND(), NULL, true);
	InvalidateRect(pSBOld->GetHWND(), NULL, true);

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

		HWND hBar = sbDrag->GetHWND();
		int w = sbDrag->GetWidth();
		int h = sbDrag->GetHeight();
		x -= ptOffset.x;
		y -= ptOffset.y;

		RECT rect; 
		SystemParametersInfo(SPI_GETWORKAREA, 0, &rect, 0);
		int t = sbDrag->GetTopNode()->bm.bmHeight;

		if (x < rect.left) x = rect.left;
		if (y < rect.top) y = rect.top;
		if ((x + w) > rect.right) x = rect.right - w;
		if ((y + t) > rect.bottom) y = rect.bottom - t;

		SetWindowPos(hBar, HWND_TOP, x, y, w, h, SWP_SHOWWINDOW);
	}
}


// ===============================================================================================
//
void WindowManager::MouseMoved(int x, int y)
{
	RECT r;
	if (hMainWnd) GetClientRect(hMainWnd, &r);
	int w = r.right - r.left;
	int h = r.bottom - r.top;
	int q = (width * 3) / 2;
	if (x < 5) sbLeft->Open(true);
	if (x > q) sbLeft->Open(false);
	if (x < (w - q)) sbRight->Open(false);
	if (x >(w - 5)) sbRight->Open(true);
}


// ===============================================================================================
//
void WindowManager::EndDrag()
{
	if (sbDrag) InvalidateRect(sbDrag->GetHWND(), NULL, true);
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
	for each (SideBar* sb in sbList) {
		if (sb->IsInactive()) continue;
		if (sb != sbDrag) {
			RECT out;
			IntersectRect(&out, &(sb->GetRect()), &(sbDrag->GetRect()));
			int a = (out.right - out.left) * (out.bottom - out.top);
			if (a > z) { z = a;	sbDest = sb; }
		}
	}
	
	if (sbDest != pOld && pOld != NULL) {
		qInsert.pTgt = NULL;
		qInsert.List.clear();
		if (pOld->GetStyle() == gcGUI::FLOAT) pOld->RescaleWindow();
		pOld->Invalidate();
	}

	qInsert.pTgt = sbDest;
	return sbDest;
}


// ===============================================================================================
// Orbiter Application Main Window Proc
//
bool WindowManager::MainWindowProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	static int xpos, ypos;
	switch (uMsg) {

	case WM_MOUSELEAVE:
	case WM_MBUTTONDOWN:
	case WM_LBUTTONDOWN:
	case WM_LBUTTONUP:
		return false;

	case WM_KEYDOWN:
	{
		if ((GetKeyState(VK_LSHIFT) & 0x8000) && (GetKeyState(VK_LCONTROL) & 0x8000)) {
			if (wParam == VK_LEFT) { sbLeft->ToggleLock(); MouseMoved(xpos, ypos); }
			if (wParam == VK_RIGHT) { sbRight->ToggleLock(); MouseMoved(xpos, ypos); }
			if (wParam == VK_DOWN) {
				sbLeft->ToggleLock();
				sbRight->ToggleLock();
				MouseMoved(xpos, ypos);
			}
		}
		return true;
	}

	case WM_MOUSEWHEEL:
		return false;

	case WM_MOUSEMOVE:
		xpos = GET_X_LPARAM(lParam);
		ypos = GET_Y_LPARAM(lParam);
		MouseMoved(xpos, ypos);
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
	
	HWND hMainWnd = pMgr->GetMainWindow();
	hInst = pMgr->GetInstance();
	width = pMgr->GetWidth();
	state = _state;

	dnNode = NULL;
	dnClose = NULL;
	ypos = 60;
	anim_state = 0;
	rollpos = 0;
	wndlen = 0;
	bOpening = false;
	bIsOpen = false;
	bValidate = true;
	bLock = false;
	bFirstTime = true;
	title_height = 0;
	bWin = pMgr->IsWindowed();

	RECT r;
	GetClientRect(hMainWnd, &r);
	height = (r.bottom - r.top) - ypos;

	DWORD exstyle = 0;
	DWORD style = 0;

	if (Config->gcGUIMode == 1) {	
		if (state == gcGUI::RIGHT) xref = r.right;
		if (state == gcGUI::LEFT) xref = -width;
		if (state == gcGUI::FLOAT) xref = width, height = width;
		if (bWin) style = WS_CHILD | WS_CLIPSIBLINGS;
		else style = 0;
	}

	if (Config->gcGUIMode == 2) {
		state = gcGUI::FLOAT;
		xref = width;
	}

	if (Config->gcGUIMode == 3) {
		state = gcGUI::FLOAT;
		xref = width;
		exstyle = 0;
		if (bWin) style = WS_CAPTION | WS_SYSMENU;
		else style = WS_CAPTION | WS_SYSMENU;
	}

	if (state == gcGUI::FLOAT) width += 2, height += 1; // Border

	if (state == gcGUI::FLOAT) hBar = CreateWindowExA(exstyle, "Floater", "Float", style, xref, ypos, width, height, hMainWnd, NULL, hInst, 0);
	else					   hBar = CreateWindowExA(exstyle, "SideBarWnd", "Dock", style, xref, ypos, width, height, hMainWnd, NULL, hInst, 0);

	SetWindowLong(hBar, GWL_STYLE, style);	// Make it bordeless

	if (Config->gcGUIMode == 3) {
		RECT w, c;
		GetWindowRect(hBar, &w);
		GetClientRect(hBar, &c);
		title_height = (w.bottom - w.top) - c.bottom;
	}

	if (Config->gcGUIMode == 1) ShowWindow(hBar, SW_SHOW);
}


// ===============================================================================================
//
SideBar::~SideBar()
{	
	wList.clear();
	DestroyWindow(hBar);
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
	for each (Node* nd in wList)
	{
		nd->bClose = false;

		bool bRootIncluded = false;
		if (nd->pParent) if (nd->pParent->GetSideBar() == this) bRootIncluded = true;
		if (nd->pParent == NULL) bRootIncluded = true;

		if (state == gcGUI::FLOAT && !bRootIncluded) nd->bClose = true;
		if (Config->gcGUIMode == 2) if (nd->pParent == NULL) nd->bClose = true;
	}
}


// ===============================================================================================
//
void SideBar::Invalidate()
{
	ManageButtons();
	InvalidateRect(hBar, NULL, true);
}


// ===============================================================================================
//
void SideBar::ResetWindow(int x, int y)
{
	if (state == gcGUI::FLOAT) {
		height = ComputeLength() + title_height + 1;
	}
	SetWindowPos(hBar, NULL, x, y, width, height, SWP_NOZORDER | SWP_SHOWWINDOW);
}


// ===============================================================================================
//
RECT SideBar::GetRect() const
{
	RECT r = { 0, 0, 0, 0 };
	if (hBar) GetWindowRect(hBar, &r);
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
	if (state == gcGUI::FLOAT) bOpening = true;
	else bOpening = bO;
}


// ===============================================================================================
//
void SideBar::Animate()
{
	if (bLock && bValidate) return;
	if (bLock && bIsOpen) return;
	if (state == gcGUI::FLOAT) return;

	if (bOpening && anim_state >= 0.9999f) {
		if (bValidate) {
			bIsOpen = true;
			InvalidateRect(hBar, NULL, TRUE);
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

	if (state == gcGUI::RIGHT) x = xref + int(-as * float(width));
	if (state == gcGUI::LEFT) x = xref + int(as * float(width));


	if (GetWindowLong(hBar, GWL_STYLE)&WS_CHILD) bFirstTime = true;

	MoveWindow(hBar, x, ypos, width, height, bFirstTime);
}


// ===============================================================================================
//
void SideBar::AddWindow(Node *pAp, bool bSetupOnly)
{
	bFirstTime = true;	// Enable Full redraw
	pAp->pSB = this;
	if (pAp->hDlg) SetParent(pAp->hDlg, hBar);
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
	if (state == gcGUI::FLOAT) return true;
	return bIsOpen;
}


// ===============================================================================================
//
void SideBar::RescaleWindow()
{
	if (state == gcGUI::FLOAT) {
		height = ComputeLength() + title_height + 1;
	}
	SetWindowPos(hBar, NULL, 0, 0, width, height, SWP_NOMOVE | SWP_NOZORDER | SWP_SHOWWINDOW);
}


// ===============================================================================================
//
void SideBar::GetVisualList(vector<Node*> &tmp)
{
	if (Config->gcGUIMode == 3) {
		for each (Node* ap in wList) if (ap->hDlg) tmp.push_back(ap);
	} 
	else 
	{
		for each (Node* ap in wList) {
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
	for each (Node* ap in wList) {
		Node* pPar = ap->pParent;
		if (pPar == NULL) {
			tmp.push_back(ap);	// Root Node
			for each (Node* ch in wList) if (ch->pParent == ap) tmp.push_back(ch); // Child
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
int SideBar::GetSourceType()
{
	if (wList.size() == 0) return gcGUI::INACTIVE;

	bool bSame = true;
	int root = 0;
	Node* pPar = NULL;

	// Count root nodes
	for each (Node *an in wList) if (an->pParent == NULL) { pPar = an; root++; }

	if (root > 1) return gcGUI::MULTI;
	if (root == 0) pPar = wList.front()->pParent;
	
	if (pPar != NULL) {
		for each (Node *an in wList) if ((an->pParent != pPar) && (an->pParent != NULL)) { bSame = false; break; }
		if (bSame) {
			if (root == 1) return gcGUI::ROOT;
			else return gcGUI::CHILD;
		}
	}

	return gcGUI::MULTI;
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
	for each (Node* ap in wList) if (ap == pX) return true;
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
	for each (Node* ap in vis) {
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
	int type = sbIn->GetSourceType();
	int h = sbIn->ComputeLength();
	int y = rollpos;
	
	if (type == gcGUI::CHILD) {
		
		Node *pNode = sbIn->GetTopNode();
		if (!pNode) return false;

		Node *pParent = pNode->pParent;
		if (!DoesExists(pParent)) pParent = NULL;
	
		Node *pPlace = FindClosest(drawList, pParent, yp);
		for each (Node* an in sbIn->wList) wIns[an] = pPlace;

		return wIns != wPrev;
	}

	if (type == gcGUI::ROOT) {

		Node *pNode = sbIn->GetTopNode();
		if (!pNode) return false;

		Node *pParent = pNode->pParent;
		if (!DoesExists(pParent)) pParent = NULL;

		Node *pPlace = FindClosest(drawList, pParent, yp);
		for each (Node* an in sbIn->wList) wIns[an] = pPlace;

		return wIns != wPrev;
	}

	return false;
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
		for each (auto &var in pIns->List)
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
LRESULT SideBar::SideBarWndProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	static int xpos, ypos;
	static int xof, yof;
	static bool bUpdate = false;

	HWND hMain = pMgr->GetMainWindow();

	switch(uMsg) {

	case WM_KEYDOWN:
	{
		pMgr->MainWindowProc(hWnd, uMsg, wParam, lParam);
		break;
	}


	case WM_MOUSEWHEEL:
	{
		if (GetStyle() == gcGUI::FLOAT) break;

		int old = rollpos;
		short d = GET_WHEEL_DELTA_WPARAM(wParam);
		if (d>0) rollpos += pMgr->cfg.scroll;
		else rollpos -= pMgr->cfg.scroll;
		int q = height - wndlen;
		if (q > 0) q = 0;
		if (rollpos > 0) rollpos = 0;
		if (rollpos < q) rollpos = q;
		if (old != rollpos) Invalidate();
		break;
	}

	case WM_LBUTTONDOWN:
	{
		xpos = GET_X_LPARAM(lParam);
		ypos = GET_Y_LPARAM(lParam);

		for each (Node* nd in wList)
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
				TRACKMOUSEEVENT te; te.cbSize = sizeof(TRACKMOUSEEVENT); te.dwFlags = TME_LEAVE; te.hwndTrack = hBar;
				TrackMouseEvent(&te);
				break; // break for
			}
		}
		break;
	}

	case WM_LBUTTONUP:
	{
		int xp = GET_X_LPARAM(lParam);
		int yp = GET_Y_LPARAM(lParam);

		SideBar *pDG = pMgr->GetDraged();

		if (pDG) {
			SideBar *pTgt = pMgr->FindDestination();
			if (pTgt) pTgt->Apply();
			pMgr->EndDrag();
			ReleaseCapture();
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
				}
			}
		}

		if (dnClose) {
			if (dnClose->GetSideBar() == this) {
				if (PointInside(xp, yp, &(dnClose->crect))) {
					pMgr->CloseWindow(dnClose);
				}
			}
		}
		
		dnNode = NULL;
		dnClose = NULL;
		break;
	}


	case WM_MOUSELEAVE:
	{
		dnNode = NULL;
		dnClose = NULL;
		break; 
	}


	case WM_MOUSEMOVE:
	{
		int dx = abs(GET_X_LPARAM(lParam) - xpos);
		int dy = abs(GET_Y_LPARAM(lParam) - ypos);
		int x = GET_X_LPARAM(lParam);
		int y = GET_Y_LPARAM(lParam);

		if (Config->gcGUIMode < 3) {

			POINT scp = { x, y };
			ClientToScreen(hBar, &scp);
			ScreenToClient(hMain, &scp);

			// Begin Moving a Window
			//
			if (dnNode && IsFloater() && (GetTopNode() == dnNode) && (dx > 5 || dy > 2))
			{
				SetCapture(hBar);
				pMgr->SetOffset(xof, yof);
				pMgr->BeginMove(dnNode, scp.x, scp.y);
				dnNode = NULL;
				dnClose = NULL;
				break;
			}

			// Detach a window from a dock
			//
			if (Config->gcGUIMode == 1) {
				if (dnNode && dx > 50) {
					SetCapture(hBar);
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
						if (pTgt->GetStyle() == gcGUI::FLOAT) pTgt->RescaleWindow();
						pTgt->Invalidate();
						pDG->Invalidate();
					}
				}
				break;
			}
		}
		break; 
	}

	case WM_PAINT:
		PaintWindow();
		break;
		
	case WM_ERASEBKGND:
		return 1;

	default:
		break;
	}


	return DefWindowProc(hWnd, uMsg, wParam, lParam);
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

	if (bInsert) for each (auto &var in wIns) if (var.second == NULL) y += var.first->CellSize();

	for each (Node* ap in drawList) 
	{
		y += ap->CellSize();
		if (bInsert) for each (auto &var in wIns) if (var.second == ap) y += var.first->CellSize();
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

	PAINTSTRUCT ps;
	HDC hDC = BeginPaint(hBar, &ps);

	int y = rollpos;
	SetBkMode(hDC, TRANSPARENT);

	tInsert *pIns = pMgr->InsertList();
	map<Node*, Node*> &wIns = pIns->List;

	bool bInsert = (pIns->pTgt == this) && (wIns.size() > 0);
	
	if (bInsert) for each (auto &var in wIns)
	{
		if (var.second == NULL) y = var.first->Spacer(hDC, y);
	}

	for each (Node* ap in drawList)
	{
		y = ap->Paint(hDC, y);

		if (bInsert)for each (auto &var in wIns)
		{
			if (var.second == ap) y = var.first->Spacer(hDC, y);
		}
	}

	// Window stack length/height
	wndlen = y - rollpos;

	if (state == gcGUI::FLOAT) {
		SelectObject(hDC, (HBRUSH)GetStockObject(NULL_BRUSH));
		SelectObject(hDC, (HPEN)GetStockObject(BLACK_PEN));
		Rectangle(hDC, 0, 0, width, height);
	} else {
		if (y < height) {
			SelectObject(hDC, (HBRUSH)GetStockObject(DKGRAY_BRUSH));
			SelectObject(hDC, (HPEN)GetStockObject(NULL_PEN));
			Rectangle(hDC, 0, y, width + 1, height + 1);
		}
	}
	
	EndPaint(hBar, &ps);

	// Move dialogs in place
	for each (Node* ap in wList) {
		bool bFound = false;
		for each (Node* q in drawList) if (q == ap) { bFound = true; break; }
		
		if (bFound && ap->hDlg) {
			if (ap->bOpen) ap->Move();
			else ShowWindow(ap->hDlg, SW_HIDE);	
		}
		else if (ap->hDlg) ShowWindow(ap->hDlg, SW_HIDE);
	}
}
