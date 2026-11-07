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

#include <windows.h>
#include <vector>
#include <list>
#include <string>
#include "gcGUI.h"

using namespace std;

extern class WindowManager *g_pWM;
extern list<gcGUIApp *> g_gcGUIAppList;

class SideBar;
class Node;
class WindowManager;
class gcGUIHost;

struct SBRect {
	float left, top, right, bottom;
};


class Node
{
public:

				Node(SideBar *pSB, const char *label, gcGUIDrawProc pDraw, void *pUser, DWORD color, DWORD flags, Node *pParent = NULL);
				~Node();

	void		Draw(float yShift);
	SideBar *	GetSideBar() const { return pSB; }
	string 		Title() const { return Label; }
	void		ReColorize(DWORD color) { Color = color; }
	void		SetApp(gcGUIApp *_pApp) { pApp = _pApp; }
	bool		IsRoot() const { return pParent == NULL; }
	bool		HasTitle() const;
	float		CellSize() const { return yBottom - yTop; }

	gcGUIApp *		pApp;
	SideBar *		pSB;
	Node *			pParent;	// Parent node for subsections or NULL for Application root node
	gcGUIDrawProc	pDraw;		// Content draw function or NULL
	void *			pUser;		// User pointer passed to pDraw
	DWORD			Color;		// Title bar color 0x00BBGGRR
	DWORD			Flags;		// gcGUI::NF_ flags
	float			yTop;		// Screen position of the node top edge, last frame
	float			yBottom;	// Screen position of the node bottom edge, last frame
	bool			bOpen;		// Open or collapsed
	bool			bClose;		// Display close window icon
	string			Label;		// Titlebar label
};



class SideBar
{
public:

				SideBar(class WindowManager *pMgr, DWORD state);
				~SideBar();

	bool		IsEmpty() const { return wList.size() == 0; }
	bool		IsFloater() const { return state == gcGUI::DS_FLOAT; }
	bool		IsInactive() const { return state == gcGUI::INACTIVE; }
	bool		IsShown() const;
	void		Show(bool bShow) { bVisible = bShow; }
	void		SetState(DWORD x) { state = x; }
	void		ManageButtons();
	void		AddWindow(Node *pAp, bool bSetupOnly = false);
	void		RemoveWindow(class Node *pAp);
	bool		IsOpen() const;
	void		Draw();
	float		ComputeLength() const;
	float		GetWidth() const;
	int			GetStyle() const { return state; }
	void		ResetWindow(float x, float y);
	SBRect		GetRect() const;
	bool		Insert(Node *pNode, Node *pAfter);
	bool		DoesExists(Node *pX);
	bool		TryInsert(SideBar *anIn);
	void		GetVisualList(vector<Node*> &out);
	void		Sort();
	bool		Apply();
	Node *		GetTopNode();
	Node *		FindClosest(vector<Node*> &vis, Node *pPar, float yval);
	DWORD		GetAutoColor();

	WindowManager *GetWM() const { return pMgr; }

	vector<Node*> wList;

private:

	void		Spacer(float h);

	class		WindowManager *pMgr;
	DWORD		state;
	bool		bVisible;
	bool		bSetPos;
	bool		bFirst;
	float		xpos, ypos;		// Window position, last frame or pending
	float		width, height;	// Window size, last frame
	float		toppos;			// Screen position of the content top, last frame
	int			cidx;
	int			uid;
};


class WindowManager : public gcGUIBase
{
	friend class SideBar;
	friend class Node;

public:

				WindowManager(HWND hAppMainWindow, HINSTANCE hInst, bool bWindowed);
				~WindowManager();


	// ===============================================================================================
	//
	void		Render();
	float		GetWidth() const;
	HWND		GetMainWindow() const { return hMainWnd; }
	HINSTANCE	GetInstance() const { return hInst; }
	SideBar *	NewSideBar(Node *pAN);
	SideBar *	GetDock(DWORD state);
	void		ReleaseSideBar(SideBar *pSB);
	void		CloseWindow(Node *pAp);
	bool		IsWindowed() const { return bWin; }
	bool		IsOK() const;
	bool		DoesExist(Node *pn);
	void		UpdateStatus(HNODE hNode);
	int			GetMode() const;


	// ===============================================================================================
	//
	SideBar *	StartDrag(Node *pAN, float x, float y);
	void		BeginMove(Node *pAN, float x, float y);
	void		Drag(float x, float y);
	void		SetOffset(float x, float y);
	void		EndDrag();
	SideBar *	GetDraged() const { return sbDrag; }
	SideBar *	GetDragSource() const { return sbDragSrc; }
	SideBar *	FindDestination();
	SideBar *	InsertTarget() const { return pInsTgt; }
	Node *		InsertAfter() const { return pInsAfter; }



	// ===============================================================================================
	//			gcGUIBase virtual overrides
	// ===============================================================================================

	HNODE		RegisterApplication(gcGUIApp *pApp, const char *label, gcGUIDrawProc pDraw, void *pUser, DWORD docked, DWORD color, DWORD flags);
	HNODE		RegisterSubsection(HNODE hNode, const char *label, gcGUIDrawProc pDraw, void *pUser, DWORD color, DWORD flags);
	void		UpdateStatus(HNODE hNode, const char *label, gcGUIDrawProc pDraw, void *pUser, DWORD color);
	bool		UnRegister(HNODE hNode);
	bool		IsOpen(HNODE hNode);
	void		OpenNode(HNODE hNode, bool bOpen = true);
	void		DisplayWindow(HNODE hNode, bool bShow = true);
	HNODE		GetNode(void *pUser);
	void *		GetUserData(HNODE hNode);

	// ===============================================================================================

private:

	struct Message {
		Node *pNode;
		DWORD uMsg;
	};

	void		TitleDown(Node *pNode);
	void		TitleHeld(Node *pNode);
	void		TitleUp(Node *pNode, bool bInside);
	void		CloseClicked(Node *pNode);
	void		Dispatch();
	void		DeleteNode(Node *pNode);

	vector<SideBar*> sbList;
	vector<Message> qMsg;
	vector<Node*> qDelete;
	vector<SideBar*> qRelease;
	SideBar *	sbDrag;
	SideBar *	sbDragSrc;
	SideBar *	sbDest;
	SideBar *	pInsTgt;
	Node *		pInsAfter;
	bool		bInsValid;
	Node *		dnNode;
	float		xOffset, yOffset;
	HWND		hMainWnd;
	HINSTANCE   hInst;
	gcGUIHost *	pHost;
	bool		bWin;
	bool		bRender;
	int			nextuid;
};

#endif
