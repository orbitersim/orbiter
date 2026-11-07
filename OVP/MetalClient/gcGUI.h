// ===================================================
// Copyright (C) 2021-2026 Jarmo Nikkanen
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
	// Node flags
	//
	static const DWORD NF_COLLAPSED = 0x1;	///< Node starts collapsed
	static const DWORD NF_NOTITLE = 0x2;	///< Node has no title bar, content is always shown

	// -----------------------------
	// Messages passed to gcGUIApp::clbkMessage
	//
	static const int MSG_OPEN_NODE = 1;
	static const int MSG_CLOSE_NODE = 2;
	static const int MSG_CLOSE_APP = 3;
};

typedef void * HNODE;

class gcGUIApp;

/**
* \brief Draws the content of a node with ImGui widgets.
* Called once per frame while the node is open and its window is shown,
* between the node's title bar and the next node. The current ImGui window
* is the side bar, so the content uses the side bar width.
* \param hNode the node being drawn
* \param pUser user pointer given at registration
*/
typedef void (*gcGUIDrawProc)(HNODE hNode, void *pUser);

class gcGUIBase
{
	friend class gcGUIApp;

public:

	virtual HNODE			RegisterApplication(gcGUIApp *pApp, const char *label, gcGUIDrawProc pDraw, void *pUser, DWORD docked, DWORD color, DWORD flags) = 0;
	virtual HNODE			RegisterSubsection(HNODE hNode, const char *label, gcGUIDrawProc pDraw, void *pUser, DWORD color, DWORD flags) = 0;
	virtual void			UpdateStatus(HNODE hNode, const char *label, gcGUIDrawProc pDraw, void *pUser, DWORD color) = 0;
	virtual bool			IsOpen(HNODE hNode) = 0;
	virtual void			OpenNode(HNODE hNode, bool bOpen = true) = 0;
	virtual void			DisplayWindow(HNODE hNode, bool bShow = true) = 0;
	virtual HNODE			GetNode(void *pUser) = 0;
	virtual void *			GetUserData(HNODE hNode) = 0;
	virtual bool			UnRegister(HNODE hNode) = 0;
};



// ===========================================================================
/**
* \class gcGUIApp
* \brief gcGUI Access and management functions
*/
// ===========================================================================

class gcGUIApp
{

public:

	gcGUIApp() : pApp(NULL)
	{

	}


	virtual ~gcGUIApp()
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
		typedef gcGUIBase * (*__gcGetGUICore)();
		HMODULE hModule = GetModuleHandle("MetalClient.dylib");
		if (hModule) {
			__gcGetGUICore pGetGUICore = (__gcGetGUICore)GetProcAddress(hModule, "gcGetGUICore");
			if (pGetGUICore) return ((pApp = pGetGUICore()) != NULL);
		}
		return false;
	}

	/**
	* \brief Register an application root node
	* \param label Title bar label
	* \param pDraw Content draw function or NULL for a title-only node
	* \param pUser User pointer passed to pDraw
	* \param docked gcGUI::DS_LEFT, gcGUI::DS_RIGHT or gcGUI::DS_FLOAT
	* \param color Title bar color (0x00BBGGRR) or 0 for the default
	* \param flags gcGUI::NF_ flags
	*/
	HNODE RegisterApplication(const char *label, gcGUIDrawProc pDraw, void *pUser, DWORD docked, DWORD color = 0, DWORD flags = 0)
	{
		assert(pApp);
		return pApp->RegisterApplication(this, label, pDraw, pUser, docked, color, flags);
	}

	/**
	* \brief Register a subsection below an application root node
	* \param color Title bar color (0x00BBGGRR) or 0 for an automatic color
	*/
	HNODE RegisterSubsection(HNODE hNode, const char *label, gcGUIDrawProc pDraw, void *pUser, DWORD color = 0, DWORD flags = 0)
	{
		assert(pApp);
		return pApp->RegisterSubsection(hNode, label, pDraw, pUser, color, flags);
	}

	/**
	* \brief Change the label, content and color of a node. A color of 0 keeps the current color.
	*/
	void UpdateStatus(HNODE hNode, const char *label, gcGUIDrawProc pDraw, void *pUser, DWORD color = 0)
	{
		assert(pApp);
		return pApp->UpdateStatus(hNode, label, pDraw, pUser, color);
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

	HNODE GetNode(void *pUser)
	{
		assert(pApp);
		return pApp->GetNode(pUser);
	}

	void *GetUserData(HNODE hNode)
	{
		assert(pApp);
		return pApp->GetUserData(hNode);
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
