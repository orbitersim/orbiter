// ===================================================
// Copyright (C) 2021 Jarmo Nikkanen
// licensed under LGPL v2
// ===================================================

#include "OrbiterAPI.h"
#include "DrawAPI.h"
#include <assert.h>

using namespace std;
using namespace oapi;

#ifndef __GC_GUI
#define __GC_GUI

namespace gcGUI 
{
	// -----------------------------
	// Dialog status identifiers 
	//
	static const int INACTIVE = 0;
	static const int DS_FLOAT = 1;
	static const int DS_LEFT = 2;
	static const int DS_RIGHT = 3;

	// -----------------------------
	// Bitmap Identifiers
	//
	static const int BM_TITLE = 0;
	static const int BM_SUBTITLE = 1;
	static const int BM_ICONS = 2;

	// -----------------------------
	// Messages passed to gcGUIApp::clbkMessge
	//
	static const int MSG_OPEN_NODE = 1;
	static const int MSG_CLOSE_NODE = 2;
	static const int MSG_CLOSE_APP = 3;
};

typedef void * HNODE;

class gcGUIBase
{
	friend class gcGUIApp;

public:

	virtual HNODE			RegisterApplication(gcGUIApp *pApp, const char *label, HWND hDlg, DWORD docked, DWORD color) = 0;
	virtual HNODE			RegisterSubsection(HNODE hNode, const char *label, HWND hDlg, DWORD color) = 0;
	virtual void			UpdateStatus(HNODE hNode, const char *label, HWND hDlg, DWORD color) = 0;
	virtual bool			IsOpen(HNODE hNode) = 0;
	virtual void			OpenNode(HNODE hNode, bool bOpen = true) = 0;
	virtual void			DisplayWindow(HNODE hNode, bool bShow = true) = 0;
	virtual HFONT			GetFont(int id) = 0;
	virtual HNODE			GetNode(HWND hDlg) = 0;
	virtual HWND			GetDialog(HNODE hNode) = 0;
	virtual void			UpdateSize(HWND hDlg) = 0;
	virtual bool			UnRegister(HNODE hNode) = 0;
};



// ===========================================================================
/**
* \class gcGUI
* \brief gcGUI Access and management functions
*/
// ===========================================================================

class gcGUIApp
{

public:

	gcGUIApp() : pApp(NULL) 
	{  

	}


	~gcGUIApp() 
	{ 
		// Can do nothing here, too late
	}

	// -----------------------------------------------------
	
	virtual void clbkShutdown() 
	{

	}

	virtual bool clbkMessage(DWORD uMsg, HNODE hNode, int data)
	{
		return false;
	}

	// -----------------------------------------------------

	inline bool Initialize()
	{
		typedef gcGUIBase * (__cdecl *__gcGetGUICore)();
		HMODULE hModule = GetModuleHandle("D3D9Client.dll");
		if (hModule) {
			__gcGetGUICore pGetGUICore = (__gcGetGUICore)GetProcAddress(hModule, "gcGetGUICore");
			if (pGetGUICore) return ((pApp = pGetGUICore()) != NULL);
		}
		return false;
	}

	HNODE RegisterApplication(const char *label, HWND hDlg, DWORD docked, DWORD color = 0)
	{
		assert(pApp);
		return pApp->RegisterApplication(this, label, hDlg, docked, color);
	}

	HNODE RegisterSubsection(HNODE hNode, const char *label, HWND hDlg, DWORD color = 0) 
	{ 
		assert(pApp);
		return pApp->RegisterSubsection(hNode, label, hDlg, color);
	}

	void UpdateStatus(HNODE hNode, const char *label, HWND hDlg, DWORD color = 0)
	{
		assert(pApp);
		return pApp->UpdateStatus(hNode, label, hDlg, color);
	}

	bool IsOpen(HNODE hNode) 
	{
		assert(pApp);
		return pApp->IsOpen(hNode);
	}

	void OpenNode(HNODE hNode, bool bOpen = true) 
	{ 
		assert(pApp);
		pApp->OpenNode(hNode, bOpen);
	}

	void DisplayWindow(HNODE hNode, bool bShow = true) 
	{ 
		assert(pApp);
		pApp->DisplayWindow(hNode, bShow);
	}

	HFONT GetFont(int id) 
	{ 
		assert(pApp);
		return pApp->GetFont(id);
	}

	HNODE GetNode(HWND hDlg) 
	{ 
		assert(pApp);
		return pApp->GetNode(hDlg);
	}

	HWND GetDialog(HNODE hNode)
	{ 
		assert(pApp);
		return pApp->GetDialog(hNode);
	}

	void UpdateSize(HWND hDlg) 
	{ 
		assert(pApp);
		pApp->UpdateSize(hDlg);
	}

	bool UnRegister(HNODE hNode)
	{
		assert(pApp);
		return pApp->UnRegister(hNode);
	}

private:

	gcGUIBase *pApp;
};

#endif