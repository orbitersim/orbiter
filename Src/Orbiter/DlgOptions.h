// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ======================================================================
// In-session options dialog
// ======================================================================

/************************************************************************
 * \file DlgOptions.h
 * \brief Implementation of the in-session options dialog.
 */

#ifndef __DLGOPTIONS_H
#define __DLGOPTIONS_H

#include "DialogWin.h"
#include "CustomControls.h"
#include <windows.h>
#include <commctrl.h>
#include <winuser.h>

class OptionsPage;

/************************************************************************
 * \brief Options frame dialog.
 * 
 * This dialog acts as the frame for the individual options pages.
 */
class DlgOptions : public DialogWin {
public:
	/**
	 * \brief Contructor for Options dialog object.
	 * \param hInstance application instance handle
	 * \param hParent parent window handle
	 * \param context pointer to context data
	 */
	DlgOptions(HINSTANCE hInstance, HWND hParent, void* context);

	/**
	 * \brief Destructor for Options dialog object.
	 */
	~DlgOptions();

	const GenericCtrl* PageContainer() const { return &m_container; }

	/**
	 * \brief Disables per-timestep updates.
	 */
	bool UpdateContinuously() const { return false; }

	/**
	 * \brief Update dialog controls from config settings
	 */
	void Update();

	BOOL OnInitDialog(HWND hDlg, WPARAM wParam, LPARAM lParam);

	BOOL OnCommand(HWND hDlg, WORD ctrlId, WORD notification, HWND hCtrl);

	BOOL OnSize(HWND hDlg, WPARAM wParam, int w, int h);

	BOOL OnVScroll(HWND hDlg, WORD request, WORD curpos, HWND hControl);

	BOOL OnNotify(HWND hDlg, int idCtrl, LPNMHDR pnmh);

	void SwitchPage(const char* name);

protected:
	void SwitchPage(HWND hDlg, size_t page);
	void SwitchPage(HWND hDlg, const OptionsPage* page);

	const OptionsPage* FindPage(const char* name) const;

	void SetSize(HWND hDlg);
	void SetPageSize(HWND hDlg);

	/**
	 * \brief Adds a new options page.
	 * \param hDlg dialog handle
	 * \param pPage pointer to new page
	 */
	HTREEITEM AddPage(HWND hDlg, OptionsPage* pPage, HTREEITEM parent = 0);

	void Clear();
	void ExpandAll(HWND hDlg);

private:
	SplitterCtrl m_splitter;
	GenericCtrl m_container;
	std::vector<OptionsPage*> m_pPage;
	size_t m_pageIdx;
	int m_vScrollPos;
	int m_vScrollRange;
	int m_vScrollPage;
	const HELPCONTEXT* m_contextHelp;
};

/************************************************************************
 * \brief Base class for options dialog pages.
 */
class OptionsPage {
public:
	/**
	 * \brief OptionsPage constructor.
	 * \param hParent parent window handle (dialog frame)
	 */
	OptionsPage(DlgOptions* dlg);

	/**
	 * \brief OptionsPage destructor.
	 */
	virtual ~OptionsPage();

	/**
	 * \brief Derived classes return the dialog resource id. 
	 */
	virtual int ResourceId() const = 0;

	/**
	 * \brief Derived classes return the page title as it appears in the tree list.
	 */
	virtual const char* Name() const = 0;

	DlgOptions* Dlg() { return m_dlg; }

	/**
	 * \brief Returns the parent dialog handle.
	 * \return Parent dialog handle
	 */
	HWND HParent() const;

	/**
	 * \brief Returns the page window handle.
	 * \return Page window handle
	 */
	HWND HPage() const { return m_hPage; }

	/**
	 * \brief Creates the page window and assigns \ref m_hPage.
	 */
	HTREEITEM CreatePage(HWND hDlg, HTREEITEM parent = 0);

	/**
	 * \brief Show/hide the page.
	 * \param bShow Show page if true, hide if false
	 */
	void Show(bool bShow);

	/**
	 * \brief Update the dialog controls from config settings.
	 * \param hPage dialog page handle
	 */
	virtual void UpdateControls(HWND hPage) {}

	/**
	 * \brief Returns the name of the option page's help page, if applicable.
	 */
	virtual const HELPCONTEXT* HelpContext() const { return 0; }

protected:
	/**
	 * \brief Default handler for WM_INITDIALOG messages.
	 * \default Nothing, returns TRUE
	 */
	virtual BOOL OnInitDialog(HWND hPage, WPARAM wParam, LPARAM lParam);

	/**
	 * \brief Default handler for WM_COMMAND messages.
	 * \param hPage dialog window handle
	 * \param ctrlId resource identifier of the control (LOWORD(wParam))
	 * \param notification code (HIWORD(wParam))
	 * \param hCtrl control window handle (lParam)
	 * \default Nothing, returns FALSE
	 */
	virtual BOOL OnCommand(HWND hPage, WORD ctrlId, WORD notification, HWND hCtrl) { return FALSE; }

	/**
	 * \brief Default handler for WM_HSCROLL messages.
	 * \default Nothing, returns FALSE
	 * \note This message is called by gauge controls on slider position change.
	 */
	virtual BOOL OnHScroll(HWND hPage, WPARAM wParam, LPARAM lParam) { return FALSE; }

	/**
	 * \brief Default handler for WM_NOTIFY messages.
	 * \param hPage dialog window handle
	 * \param ctrlId resource identifier of the control (wParam)
	 * \param pNmHdr pointer to a NMHDR structure with details of the notification
	 * \default Nothing, returns FALSE
	 */
	virtual BOOL OnNotify(HWND hPage, DWORD ctrlId, const NMHDR* pNmHdr) { return FALSE; }

	/**
	 * \brief Default generic message handler.
	 * \default Nothing, returns FALSE
	 * \note This method is called for any messages which don't have an associated
	 *    specific callback function.
	 */
	virtual BOOL OnMessage(HWND hPage, UINT uMsg, WPARAM wParam, LPARAM lParam) { return FALSE; }

	/**
	 * \Brief page message loop.
	 */
	virtual INT_PTR DlgProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);

private:
	/**
	 * \brief Message loop hook for all options page.
	 * \note This function dereferences the page instance and then calls
	 *    the specific page's DlgProc method.
	 */
	static INT_PTR CALLBACK s_DlgProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);

	DlgOptions* m_dlg; ///< parent dialog
	HWND m_hPage;      ///< page window handle (0 before MakePage has been called)
	HTREEITEM m_hItem; ///< page title in the tree view control
};

/************************************************************************
 * \brief Page for celestial sphere rendering options. 
 */
class OptionsPage_CelSphere : public OptionsPage {
public:
	OptionsPage_CelSphere(DlgOptions* dlg);
	int ResourceId() const;
	const char* Name() const;
	const HELPCONTEXT* HelpContext() const;
	void UpdateControls(HWND hPage);

protected:
	BOOL OnInitDialog(HWND hPage, WPARAM wParam, LPARAM lParam);
	BOOL OnCommand(HWND hPage, WORD ctrlId, WORD notification, HWND hCtrl);
	BOOL OnHScroll(HWND hTab, WPARAM wParam, LPARAM lParam);
	BOOL OnNotify(HWND hPage, DWORD ctrlId, const NMHDR* pNmHdr);
	void PopulateStarmapList(HWND hPage);
	void PopulateBgImageList(HWND hPage);
	void StarPixelActivationChanged(HWND hPage);
	void StarmapActivationChanged(HWND hPage);
	void StarmapImageChanged(HWND hPage);
	void BackgroundActivationChanged(HWND hPage);
	void BackgroundImageChanged(HWND hPage);
	void BackgroundBrightnessChanged(HWND hPage, double level);

private:
	std::vector<std::pair<std::string, std::string>> m_pathStarmap;
	std::vector<std::pair<std::string, std::string>> m_pathBgImage;
};

/************************************************************************
 * \brief Main page for visual helpers options.
 */
class OptionsPage_VisHelper : public OptionsPage {
public:
	OptionsPage_VisHelper(DlgOptions* dlg);
	int ResourceId() const;
	const char* Name() const;
	const HELPCONTEXT* HelpContext() const;
	void UpdateControls(HWND hPage);

protected:
	BOOL OnInitDialog(HWND hPage, WPARAM wParam, LPARAM lParam);
	BOOL OnCommand(HWND hPage, WORD ctrlId, WORD notification, HWND hCtrl);
};

/************************************************************************
 * \brief Visual helpers "Planetarium" options page.
 */
class OptionsPage_Planetarium : public OptionsPage {
public:
	OptionsPage_Planetarium(DlgOptions* dlg);
	int ResourceId() const;
	const char* Name() const;
	const HELPCONTEXT* HelpContext() const;
	void UpdateControls(HWND hPage);

protected:
	BOOL OnInitDialog(HWND hPage, WPARAM wParam, LPARAM lParam);
	BOOL OnCommand(HWND hPage, WORD ctrlId, WORD notification, HWND hCtrl);
	void RescanMarkerList(HWND hPage);
	void OnItemClicked(HWND hPage, WORD ctrlId);
	BOOL OnMarkerSelectionChanged(HWND hPage);
};

/************************************************************************
 * \brief Visual helpers "Labels" options page.
 */
class OptionsPage_Labels : public OptionsPage {
public:
	OptionsPage_Labels(DlgOptions* dlg);
	int ResourceId() const;
	const char* Name() const;
	const HELPCONTEXT* HelpContext() const;
	void UpdateControls(HWND hPage);

protected:
	BOOL OnInitDialog(HWND hPage, WPARAM wParam, LPARAM lParam);
	BOOL OnCommand(HWND hPage, WORD ctrlId, WORD notification, HWND hCtrl);
	void OnItemClicked(HWND hPage, WORD ctrlId);
	void ScanPsysBodies(HWND hPage);
	void UpdateFeatureList(HWND hPage);
	void RescanFeatures(HWND hPage);
};

/************************************************************************
 * \brief Visual helpers "Body force vectors" options page.
 */
class OptionsPage_Forces : public OptionsPage {
public:
	OptionsPage_Forces(DlgOptions* dlg);
	int ResourceId() const;
	const char* Name() const;
	const HELPCONTEXT* HelpContext() const;
	void UpdateControls(HWND hPage);

protected:
	BOOL OnInitDialog(HWND hPage, WPARAM wParam, LPARAM lParam);
	BOOL OnCommand(HWND hPage, WORD ctrlId, WORD notification, HWND hCtrl);
	void OnItemClicked(HWND hPage, WORD ctrlId);
	BOOL OnHScroll(HWND hTab, WPARAM wParam, LPARAM lParam);
};

/************************************************************************
 * \brief Visual helpers "Object frame axes" options page.
 */
class OptionsPage_Axes : public OptionsPage {
public:
	OptionsPage_Axes(DlgOptions* dlg);
	int ResourceId() const;
	const char* Name() const;
	const HELPCONTEXT* HelpContext() const;
	void UpdateControls(HWND hPage);

protected:
	BOOL OnInitDialog(HWND hPage, WPARAM wParam, LPARAM lParam);
	BOOL OnCommand(HWND hPage, WORD ctrlId, WORD notification, HWND hCtrl);
	void OnItemClicked(HWND hPage, WORD ctrlId);
	BOOL OnHScroll(HWND hTab, WPARAM wParam, LPARAM lParam);
};

#endif // !__DLGOPTIONS_H