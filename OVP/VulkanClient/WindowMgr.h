
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


#ifndef __WNDMGR_H
#define __WNDMGR_H

// Windows.h left out: the side bars are Qt widgets
#include "gcCore.h"
#include <QSize>
class QObject;
#include <vector>
#include <map>
#include "gcGUI.h"

using namespace std;

extern class WindowManager *g_pWM;

class SideBar;
class Node;
class WindowManager;

typedef struct {
	std::map<Node*, Node*> List;
	SideBar *pTgt;
} tInsert;

bool SideBarWndProc(QWidget *hWnd, QEvent *event); // true if the event was handled

class Node
{
public:

	Node(SideBar *pSB, const char *label, QWidget *hDlg, DWORD color, Node *pParent = NULL);
	~Node();

	void		Move();
	int			Paint(QPainter *hDC, int y);
	int			Spacer(QPainter *hDC, int y);
	void		PaintIcon(QPainter *hDC, int x, int y, int id);
	int			CellSize();
	SideBar *	GetSideBar() const { return pSB; }
	string 		Title() const { return Label; }
	void		ReColorize(DWORD color);
	void		SetApp(gcGUIApp *pApp);
	bool		IsRoot() const { return pParent == 0; }

	gcGUIApp *  pApp;
	SideBar *	pSB;
	Node *		pParent;	// Parent node for subsections or NULL for Application root node
	QImage *	hBmp;		// Titlebar graphics
	QSize		bm;			// Titlebar dimensions
	QWidget *	hDlg;		// Dialog handle or NULL
	POINT		pos;		// Top-left corner of dialog window
	RECT		trect;		// Titlebar rect
	RECT		crect;		// Close button rect
	bool		bOpen;		// Open or collapsed
	bool		bClose;		// Display close window icon
	char *		Label;		// Titlebar label
};



class SideBar
{
public:

				SideBar(class WindowManager *pMgr, DWORD flags);
				~SideBar();

	bool		SideBarWndProc(QWidget *hWnd, QEvent *event);
	bool		IsEmpty() const { return wList.size() == 0; }
	bool		IsFloater() const { return state == gcGUI::DS_FLOAT; }
	bool		IsInactive() const { return state == gcGUI::INACTIVE; }
	void		SetState(int x) { state = x; }
	void		Open(bool bOpen);
	void		Invalidate();
	void		ToggleLock();
	void		ManageButtons();
	void		Animate();
	QWidget *	GetHWND() const { return hBar; }
	void		AddWindow(Node *pAp, bool bSetupOnly = false);
	void		RemoveWindow(class Node *pAp);
	bool		IsOpen() const;
	void		PaintWindow();
	// GetDC/ReleaseDC left out: QImage needs no compatible DC, painting happens in the paint event
	int			ComputeLength();
	int			GetWidth() const { return width; }
	int			GetHeight() const { return height; }
	int			GetStyle() const { return state; }
	void		RescaleWindow();
	void		ResetWindow(int x, int y);
	RECT		GetRect() const;
	bool		Insert(Node *pNode, Node *pAfter);
	bool		DoesExists(Node *pX);
	bool		TryInsert(SideBar *anIn);
	void		GetVisualList(vector<Node*> &out);
	void		Sort();
	bool		Apply();
	Node *		GetTopNode();
	Node *		FindClosest(vector<Node*> &vis, Node *pPar, int yval);
	Node *		FindNode(QWidget *hDlg);
	DWORD		GetAutoColor();
	
	WindowManager *GetWM() const { return pMgr; }

	vector<Node*> wList;
	
private:

	class		WindowManager *pMgr;
	QWidget *	hBar;
	void *		hInst;
	DWORD		state;
	float		anim_state;
	bool		bOpening, bIsOpen, bValidate, bLock, bFirstTime, bWin;
	int			xref, ypos, width, height, wndlen, rollpos, title_height;
	Node *		dnNode;
	Node *		dnClose;
	int			cidx;
};


struct Conf
{
	int txt_main_x;
	int txt_main_y;
	int txt_main_size;
	int txt_main_weight;

	int txt_sub_x;
	int txt_sub_y;
	int txt_sub_size;
	int txt_sub_weight;

	int scroll;

	DWORD txt_main_clr;
	DWORD txt_sub_clr;

	char bmp_main[32];
	char bmp_sub[32];
	char bmp_icon[32];

	char fnt_main[32];
	char fnt_sub[32];
};


class WindowManager : public gcGUIBase
{
public:


				WindowManager(QWindow *hAppMainWindow, void *hInst, bool bWindowed);
				~WindowManager();


	// ===============================================================================================
	//
	bool		MainWindowProc(QObject *hWnd, QEvent *event);
	void		Animate();
	int			GetWidth() const { return width; }
	QWindow *	GetMainWindow() const { return hMainWnd; }
	void *		GetInstance() const { return hInst; }
	SideBar *	GetSideBar(QWidget *hWnd);
	SideBar *	NewSideBar(Node *pAN);
	void		ReleaseSideBar(SideBar *pSB);
	void		CloseWindow(Node *pAp);
	bool		IsWindowed() const { return bWin; }
	bool		IsOK() const;
	bool		DoesExist(Node *pn);
	void		UpdateStatus(HNODE hNode);


	// ===============================================================================================
	//
	QImage *	GetBitmap(int id) const;
	QFont *		GetAppTitleFont() const { return hAppFont; }
	QFont *		GetSubTitleFont() const { return hSubFont; }
	

	// ===============================================================================================
	//
	SideBar *	StartDrag(Node *pAN, int x, int y);
	void		BeginMove(Node *pAN, int x, int y);
	void		MouseMoved(int x, int y);
	void		Drag(int x, int y);
	void		SetOffset(int x, int y);
	void		EndDrag();
	SideBar *	GetDraged() const { return sbDrag; }
	SideBar *	GetDragSource() const { return sbDragSrc; }
	SideBar *	FindDestination();
	tInsert	*	InsertList() { return &qInsert; }



	// ===============================================================================================
	//			gcGUIBase virtual overrides
	// ===============================================================================================

	HNODE		RegisterApplication(gcGUIApp *pApp, const char *label, QWidget *hDlg, DWORD docked, DWORD color);
	HNODE		RegisterSubsection(HNODE hNode, const char *label, QWidget *hDlg, DWORD color);
	void		UpdateStatus(HNODE hNode, const char *label, QWidget *hDlg, DWORD color);
	bool		UnRegister(HNODE hNode);
	bool		IsOpen(HNODE hNode);
	void		OpenNode(HNODE hNode, bool bOpen = true);
	void		DisplayWindow(HNODE hNode, bool bShow = true);
	QFont *		GetFont(int id);
	HNODE		GetNode(QWidget *hDlg);
	QWidget *	GetDialog(HNODE hNode);
	void		UpdateSize(QWidget *hDlg);

	// ===============================================================================================

	Conf		cfg;

private:
	
	tInsert		qInsert;
	vector<SideBar*> sbList;
	SideBar *	sbDrag;
	SideBar *	sbDragSrc;
	SideBar *	sbDest;
	POINT		ptOffset;
	int			width;
	QWindow *	hMainWnd;
	void *		hInst;
	QFont *		hAppFont;
	QFont *		hSubFont;
	QImage *	hTitle;
	QImage *	hIcons;
	QImage *	hSub;
	bool		bWin;
	DWORD		Cmd;
};




#endif

