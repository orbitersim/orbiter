// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ======================================================================
// Template for simulation options pages
// ======================================================================

/************************************************************************
 * \file OptionsPages.h
 * \brief Template for simulation options pages
 */

#ifndef __OPTIONSPAGES_H
#define __OPTIONSPAGES_H

#ifndef __linux__
#include <windows.h>
#include <CommCtrl.h>
#else // __linux__
#include "OrbiterPlatform.h"
#endif // __linux__
#include "CustomControls.h"
#include "OrbiterAPI.h"
#ifdef __linux__
#include <string>
#include <vector>

class QTreeWidgetItem;
#endif // __linux__

class OptionsPage;
class Config;

/************************************************************************
 * \brief Container class for options pages
 */
class OptionsPageContainer {
public:
	/**
	 * \brief Enumerates where the options pages are shown.
	 */
	enum Originator {
		LAUNCHPAD, ///< Show in Launchpad dialog
		INLINE     ///< Show as inline dialog during a simulation session
	};

	OptionsPageContainer(Originator orig, Config* cfg);
	~OptionsPageContainer();

	OptionsPage* CurrentPage();

	Originator Environment() const { return m_orig; }

	Config* Cfg() { return m_cfg; }

#ifndef __linux__
	void SetWindowHandles(HWND hDlg, HWND hSplitter, HWND hPane1, HWND hPane2);
#else // __linux__
	void SetWindowHandles(QWidget *hDlg, QWidget *hSplitter, QWidget *hPane1, QWidget *hPane2);
#endif // __linux__

	const GenericCtrl* ContainerControl() const { return &m_container; }

	/**
	 * \brief Update dialog controls from config settings
	 */
	void UpdatePages(bool resetView);

	/**
	 * \brief Update config object from dialog controls
	 */
	void UpdateConfig();

	void SwitchPage(const char* name);

#ifndef __linux__
	void OnNotifyPagelist(LPNMHDR pnmh);
#else // __linux__
	void OnNotifyPagelist(QTreeWidgetItem *itemNew);
	// TVN_SELCHANGED of the page list
#endif // __linux__

protected:
	/**
     * \brief Adds a new options page.
     * \param hDlg dialog handle
     * \param pPage pointer to new page
     */
#ifndef __linux__
	HTREEITEM AddPage(OptionsPage* pPage, HTREEITEM parent = 0);
#else // __linux__
	QTreeWidgetItem *AddPage(OptionsPage* pPage, QTreeWidgetItem *parent = 0);
#endif // __linux__

	const OptionsPage* FindPage(const char* name) const;

	void SwitchPage(size_t page);
	void SwitchPage(const OptionsPage* page);

	void CreatePages();

	void ExpandAll();

#ifndef __linux__
	void SetPageSize(HWND hDlg);
#else // __linux__
	void SetPageSize(QWidget *hDlg);
#endif // __linux__

	void Clear();

#ifndef __linux__
	BOOL VScroll(HWND hDlg, WORD request, WORD curpos, HWND hControl);
#else // __linux__
	BOOL VScroll(QWidget *hDlg, int pos, QWidget *hControl);
	// the page scroll bar moved to pos
#endif // __linux__

	const HELPCONTEXT* HelpContext() const { return m_contextHelp; }

private:
	Originator m_orig;
	Config* m_cfg;
	SplitterCtrl m_splitter;
	GenericCtrl m_container;
	std::vector<OptionsPage*> m_pPage;
	size_t m_pageIdx;
#ifndef __linux__
	HWND m_hDlg;
	HWND m_hPageList;
	HWND m_hContainer;
#else // __linux__
	QWidget *m_hDlg;
	QWidget *m_hPageList;
	QWidget *m_hContainer;
#endif // __linux__
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
	 * \param container Container owning the page
	 */
	OptionsPage(OptionsPageContainer* container);

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

	/**
	 * \brief Returns the container object owning the page.
	 */
	OptionsPageContainer* Container() { return m_container; }

	Config* Cfg() { return m_container->Cfg(); }

	/**
	 * \brief Returns the parent dialog handle.
	 * \return Parent dialog handle
	 */
#ifndef __linux__
	HWND HParent() const;
#else // __linux__
	QWidget *HParent() const;
#endif // __linux__

	/**
	 * \brief Returns the page window handle.
	 * \return Page window handle
	 */
#ifndef __linux__
	HWND HPage() const { return m_hPage; }
#else // __linux__
	QWidget *HPage() const { return m_hPage; }
#endif // __linux__

	/**
	 * \brief Creates the page window and assigns \ref m_hPage.
	 */
#ifndef __linux__
	HTREEITEM CreatePage(HWND hDlg, HTREEITEM parent = 0);
#else // __linux__
	QTreeWidgetItem *CreatePage(QWidget *hDlg, QTreeWidgetItem *parent = 0);
#endif // __linux__

	/**
	 * \brief Show/hide the page.
	 * \param bShow Show page if true, hide if false
	 */
	void Show(bool bShow);

	/**
	 * \brief Update the dialog controls from config settings.
	 * \param hPage dialog page handle
	 */
#ifndef __linux__
	virtual void UpdateControls(HWND hPage) {}
#else // __linux__
	virtual void UpdateControls(QWidget *hPage) {}
#endif // __linux__

	/**
	 * \brief Update config object from dialog control states.
	 *    Only required for pages which don't react directly to controls
	 *    being modified.
	 */
#ifndef __linux__
	virtual void UpdateConfig(HWND hPage) {}
#else // __linux__
	virtual void UpdateConfig(QWidget *hPage) {}
#endif // __linux__

	/**
	 * \brief Returns the name of the option page's help page, if applicable.
	 */
	virtual const HELPCONTEXT* HelpContext() const { return 0; }

protected:
	/**
	 * \brief Default handler for WM_INITDIALOG messages.
	 * \default Nothing, returns TRUE
	 */
#ifndef __linux__
	virtual BOOL OnInitDialog(HWND hPage, WPARAM wParam, LPARAM lParam);
#else // __linux__
	virtual BOOL OnInitDialog(QWidget *hPage);
#endif // __linux__

#ifndef __linux__
	/**
	 * \brief Default handler for WM_COMMAND messages.
	 * \param hPage dialog window handle
	 * \param ctrlId resource identifier of the control (LOWORD(wParam))
	 * \param notification code (HIWORD(wParam))
	 * \param hCtrl control window handle (lParam)
	 * \default Nothing, returns FALSE
	 */
#else // __linux__
	/**
	 * \brief Default handler for control commands (WM_COMMAND).
	 * \param hPage dialog window handle
	 * \param ctrlId resource identifier of the control
	 * \param notification RESNOTIFY code
	 * \param hCtrl control window handle
	 * \default Nothing, returns FALSE
	 */
#endif // __linux__
#ifndef __linux__
	virtual BOOL OnCommand(HWND hPage, WORD ctrlId, WORD notification, HWND hCtrl) { return FALSE; }
#else // __linux__
	virtual BOOL OnCommand(QWidget *hPage, WORD ctrlId, WORD notification, QWidget *hCtrl) { return FALSE; }
#endif // __linux__

#ifndef __linux__
	/**
	 * \brief Default handler for WM_HSCROLL messages.
	 * \default Nothing, returns FALSE
	 * \note This message is called by gauge controls on slider position change.
	 */
#else // __linux__
	/**
	 * \brief Default handler for gauge control position changes (WM_HSCROLL).
	 * \param request GAUGEREQUEST code
	 * \param pos new gauge position
	 * \default Nothing, returns FALSE
	 */
#endif // __linux__
#ifndef __linux__
	virtual BOOL OnHScroll(HWND hPage, WPARAM wParam, LPARAM lParam) { return FALSE; }
#else // __linux__
	virtual BOOL OnHScroll(QWidget *hPage, int ctrlId, int request, int pos) { return FALSE; }
#endif // __linux__

#ifndef __linux__
	/**
	 * \brief Default handler for WM_NOTIFY messages.
	 * \param hPage dialog window handle
	 * \param ctrlId resource identifier of the control (wParam)
	 * \param pNmHdr pointer to a NMHDR structure with details of the notification
	 * \default Nothing, returns FALSE
	 */
#else // __linux__
	/**
	 * \brief Default handler for up-down control clicks (WM_NOTIFY UDN_DELTAPOS).
	 * \param iDelta requested change of the up-down position
	 * \default Nothing, returns FALSE
	 */
#endif // __linux__
#ifndef __linux__
	virtual BOOL OnNotify(HWND hPage, DWORD ctrlId, const NMHDR* pNmHdr) { return FALSE; }
#else // __linux__
	virtual BOOL OnDeltaPos(QWidget *hPage, int ctrlId, int iDelta) { return FALSE; }
#endif // __linux__

#ifndef __linux__
	/**
	 * \brief Default generic message handler.
	 * \default Nothing, returns FALSE
	 * \note This method is called for any messages which don't have an associated
	 *    specific callback function.
	 */
#else // __linux__
	/**
	 * \brief Default generic event handler.
	 * \default Nothing, returns FALSE
	 * \note This method is called for any events of the page window.
	 */
#endif // __linux__
#ifndef __linux__
	virtual BOOL OnMessage(HWND hPage, UINT uMsg, WPARAM wParam, LPARAM lParam) { return FALSE; }
#else // __linux__
	virtual BOOL OnMessage(QWidget *hPage, QEvent *event) { return FALSE; }
#endif // __linux__

#ifndef __linux__
	/**
	 * \Brief page message loop.
	 */
#else // __linux__
	/**
	 * \Brief connects the page's controls and events to the handlers above (page message loop).
	 */
#endif // __linux__
#ifndef __linux__
	virtual INT_PTR DlgProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
#else // __linux__
	virtual void DlgProc(QWidget *hWnd);
#endif // __linux__

private:
#ifndef __linux__
	/**
	 * \brief Message loop hook for all options page.
	 * \note This function dereferences the page instance and then calls
	 *    the specific page's DlgProc method.
	 */
	static INT_PTR CALLBACK s_DlgProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);

#endif // !__linux__
	OptionsPageContainer* m_container; ///< container owning the page
#ifndef __linux__
	HWND m_hPage;      ///< page window handle (0 before MakePage has been called)
	HTREEITEM m_hItem; ///< page title in the tree view control
#else // __linux__
	QWidget *m_hPage;      ///< page window handle (0 before MakePage has been called)
	QTreeWidgetItem *m_hItem; ///< page title in the tree view control
#endif // __linux__
};

/************************************************************************
 * \brief Page for visual parameters
 */
class OptionsPage_Visual : public OptionsPage {
public:
	OptionsPage_Visual(OptionsPageContainer* container);
	int ResourceId() const;
	const char* Name() const;
	const HELPCONTEXT* HelpContext() const;
#ifndef __linux__
	void UpdateControls(HWND hPage);
	void UpdateConfig(HWND hPage);
#else // __linux__
	void UpdateControls(QWidget *hPage);
	void UpdateConfig(QWidget *hPage);
#endif // __linux__

protected:
#ifndef __linux__
	BOOL OnInitDialog(HWND hPage, WPARAM wParam, LPARAM lParam);
	BOOL OnCommand(HWND hPage, WORD ctrlId, WORD notification, HWND hCtrl);
	void VisualsChanged(HWND hPage);
#else // __linux__
	BOOL OnInitDialog(QWidget *hPage);
	BOOL OnCommand(QWidget *hPage, WORD ctrlId, WORD notification, QWidget *hCtrl);
	void VisualsChanged(QWidget *hPage);
#endif // __linux__

};

/************************************************************************
* \brief Page for physics engine options
*/
class OptionsPage_Physics : public OptionsPage {
public:
	OptionsPage_Physics(OptionsPageContainer* container);
	int ResourceId() const;
	const char* Name() const;
	const HELPCONTEXT* HelpContext() const;
#ifndef __linux__
	void UpdateControls(HWND hPage);
	void UpdateConfig(HWND hPage);
#else // __linux__
	void UpdateControls(QWidget *hPage);
	void UpdateConfig(QWidget *hPage);
#endif // __linux__

protected:
#ifndef __linux__
	BOOL OnInitDialog(HWND hPage, WPARAM wParam, LPARAM lParam);
	BOOL OnCommand( HWND hPage, WORD ctrlId, WORD notification, HWND hCtrl );
#else // __linux__
	BOOL OnInitDialog(QWidget *hPage);
	BOOL OnCommand(QWidget *hPage, WORD ctrlId, WORD notification, QWidget *hCtrl);
#endif // __linux__
};

/************************************************************************
 * \brief Page for instrument and panel options
 */
class OptionsPage_Instrument : public OptionsPage {
public:
	OptionsPage_Instrument(OptionsPageContainer* container);
	int ResourceId() const;
	const char* Name() const;
	const HELPCONTEXT* HelpContext() const;
#ifndef __linux__
	void UpdateControls(HWND hPage);
#else // __linux__
	void UpdateControls(QWidget *hPage);
#endif // __linux__

protected:
#ifndef __linux__
	BOOL OnInitDialog(HWND hPage, WPARAM wParam, LPARAM lParam);
	BOOL OnCommand(HWND hPage, WORD ctrlId, WORD notification, HWND hCtrl);
	BOOL OnNotify(HWND hPage, DWORD ctrlId, const NMHDR* pNmHdr);
#else // __linux__
	BOOL OnInitDialog(QWidget *hPage);
	BOOL OnCommand(QWidget *hPage, WORD ctrlId, WORD notification, QWidget *hCtrl);
	BOOL OnDeltaPos(QWidget *hPage, int ctrlId, int iDelta);
#endif // __linux__
};

/************************************************************************
 * \brief Page for vessel options
 */
class OptionsPage_Vessel : public OptionsPage {
public:
	OptionsPage_Vessel(OptionsPageContainer* container);
	int ResourceId() const;
	const char* Name() const;
	const HELPCONTEXT* HelpContext() const;
#ifndef __linux__
	void UpdateControls(HWND hPage);
#else // __linux__
	void UpdateControls(QWidget *hPage);
#endif // __linux__

protected:
#ifndef __linux__
	BOOL OnInitDialog(HWND hPage, WPARAM wParam, LPARAM lParam);
	BOOL OnCommand(HWND hPage, WORD ctrlId, WORD notification, HWND hCtrl);
#else // __linux__
	BOOL OnInitDialog(QWidget *hPage);
	BOOL OnCommand(QWidget *hPage, WORD ctrlId, WORD notification, QWidget *hCtrl);
#endif // __linux__
};

/************************************************************************
* \brief Page for user interface options
*/
class OptionsPage_UI : public OptionsPage {
public:
	OptionsPage_UI(OptionsPageContainer* container);
	int ResourceId() const;
	const char* Name() const;
	const HELPCONTEXT* HelpContext() const;
#ifndef __linux__
	void UpdateControls(HWND hPage);
#else // __linux__
	void UpdateControls(QWidget *hPage);
#endif // __linux__

protected:
#ifndef __linux__
	BOOL OnInitDialog(HWND hPage, WPARAM wParam, LPARAM lParam);
	BOOL OnCommand(HWND hPage, WORD ctrlId, WORD notification, HWND hCtrl);
#else // __linux__
	BOOL OnInitDialog(QWidget *hPage);
	BOOL OnCommand(QWidget *hPage, WORD ctrlId, WORD notification, QWidget *hCtrl);
#endif // __linux__
};

/************************************************************************
* \brief Page for joystick options
*/
class OptionsPage_Joystick : public OptionsPage {
public:
	OptionsPage_Joystick(OptionsPageContainer* container);
	int ResourceId() const;
	const char* Name() const;
	const HELPCONTEXT* HelpContext() const;
#ifndef __linux__
	void UpdateControls(HWND hPage);
#else // __linux__
	void UpdateControls(QWidget *hPage);
#endif // __linux__

protected:
#ifndef __linux__
	BOOL OnInitDialog(HWND hPage, WPARAM wParam, LPARAM lParam);
	BOOL OnCommand(HWND hPage, WORD ctrlId, WORD notification, HWND hCtrl);
	BOOL OnHScroll(HWND hPage, WPARAM wParam, LPARAM lParam);
#else // __linux__
	BOOL OnInitDialog(QWidget *hPage);
	BOOL OnCommand(QWidget *hPage, WORD ctrlId, WORD notification, QWidget *hCtrl);
	BOOL OnHScroll(QWidget *hPage, int ctrlId, int request, int pos);
#endif // __linux__
};

/************************************************************************
* \brief Page for celestial sphere rendering options.
*/
class OptionsPage_CelSphere : public OptionsPage {
public:
	OptionsPage_CelSphere(OptionsPageContainer* container);
	int ResourceId() const;
	const char* Name() const;
	const HELPCONTEXT* HelpContext() const;
#ifndef __linux__
	void UpdateControls(HWND hPage);
#else // __linux__
	void UpdateControls(QWidget *hPage);
#endif // __linux__

protected:
#ifndef __linux__
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
#else // __linux__
	BOOL OnInitDialog(QWidget *hPage);
	BOOL OnCommand(QWidget *hPage, WORD ctrlId, WORD notification, QWidget *hCtrl);
	BOOL OnHScroll(QWidget *hTab, int ctrlId, int request, int pos);
	BOOL OnDeltaPos(QWidget *hPage, int ctrlId, int iDelta);
	void PopulateStarmapList(QWidget *hPage);
	void PopulateBgImageList(QWidget *hPage);
	void StarPixelActivationChanged(QWidget *hPage);
	void StarmapActivationChanged(QWidget *hPage);
	void StarmapImageChanged(QWidget *hPage);
	void BackgroundActivationChanged(QWidget *hPage);
	void BackgroundImageChanged(QWidget *hPage);
	void BackgroundBrightnessChanged(QWidget *hPage, double level);
#endif // __linux__

private:
	std::vector<std::pair<std::string, std::string>> m_pathStarmap;
	std::vector<std::pair<std::string, std::string>> m_pathBgImage;
};

/************************************************************************
 * \brief Main page for visual helpers options.
 */
class OptionsPage_VisHelper : public OptionsPage {
public:
	OptionsPage_VisHelper(OptionsPageContainer* container);
	int ResourceId() const;
	const char* Name() const;
	const HELPCONTEXT* HelpContext() const;
#ifndef __linux__
	void UpdateControls(HWND hPage);
#else // __linux__
	void UpdateControls(QWidget *hPage);
#endif // __linux__

protected:
#ifndef __linux__
	BOOL OnInitDialog(HWND hPage, WPARAM wParam, LPARAM lParam);
	BOOL OnCommand(HWND hPage, WORD ctrlId, WORD notification, HWND hCtrl);
#else // __linux__
	BOOL OnInitDialog(QWidget *hPage);
	BOOL OnCommand(QWidget *hPage, WORD ctrlId, WORD notification, QWidget *hCtrl);
#endif // __linux__
};

/************************************************************************
 * \brief Visual helpers "Planetarium" options page.
 */
class OptionsPage_Planetarium : public OptionsPage {
public:
	OptionsPage_Planetarium(OptionsPageContainer* container);
	int ResourceId() const;
	const char* Name() const;
	const HELPCONTEXT* HelpContext() const;
#ifndef __linux__
	void UpdateControls(HWND hPage);
#else // __linux__
	void UpdateControls(QWidget *hPage);
#endif // __linux__

protected:
#ifndef __linux__
	BOOL OnInitDialog(HWND hPage, WPARAM wParam, LPARAM lParam);
	BOOL OnCommand(HWND hPage, WORD ctrlId, WORD notification, HWND hCtrl);
	void RescanMarkerList(HWND hPage);
	void OnItemClicked(HWND hPage, WORD ctrlId);
	BOOL OnMarkerSelectionChanged(HWND hPage);
#else // __linux__
	BOOL OnInitDialog(QWidget *hPage);
	BOOL OnCommand(QWidget *hPage, WORD ctrlId, WORD notification, QWidget *hCtrl);
	void RescanMarkerList(QWidget *hPage);
	void OnItemClicked(QWidget *hPage, WORD ctrlId);
	BOOL OnMarkerSelectionChanged(QWidget *hPage);
#endif // __linux__
};

/************************************************************************
 * \brief Visual helpers "Labels" options page.
 */
class OptionsPage_Labels : public OptionsPage {
public:
	OptionsPage_Labels(OptionsPageContainer* container);
	int ResourceId() const;
	const char* Name() const;
	const HELPCONTEXT* HelpContext() const;
#ifndef __linux__
	void UpdateControls(HWND hPage);
#else // __linux__
	void UpdateControls(QWidget *hPage);
#endif // __linux__

protected:
#ifndef __linux__
	BOOL OnInitDialog(HWND hPage, WPARAM wParam, LPARAM lParam);
	BOOL OnCommand(HWND hPage, WORD ctrlId, WORD notification, HWND hCtrl);
	void OnItemClicked(HWND hPage, WORD ctrlId);
	void ScanPsysBodies(HWND hPage);
	void UpdateFeatureList(HWND hPage);
	void RescanFeatures(HWND hPage);
#else // __linux__
	BOOL OnInitDialog(QWidget *hPage);
	BOOL OnCommand(QWidget *hPage, WORD ctrlId, WORD notification, QWidget *hCtrl);
	void OnItemClicked(QWidget *hPage, WORD ctrlId);
	void ScanPsysBodies(QWidget *hPage);
	void UpdateFeatureList(QWidget *hPage);
	void RescanFeatures(QWidget *hPage);
#endif // __linux__
};

/************************************************************************
 * \brief Visual helpers "Body force vectors" options page.
 */
class OptionsPage_Forces : public OptionsPage {
public:
	OptionsPage_Forces(OptionsPageContainer* container);
	int ResourceId() const;
	const char* Name() const;
	const HELPCONTEXT* HelpContext() const;
#ifndef __linux__
	void UpdateControls(HWND hPage);
#else // __linux__
	void UpdateControls(QWidget *hPage);
#endif // __linux__

protected:
#ifndef __linux__
	BOOL OnInitDialog(HWND hPage, WPARAM wParam, LPARAM lParam);
	BOOL OnCommand(HWND hPage, WORD ctrlId, WORD notification, HWND hCtrl);
	void OnItemClicked(HWND hPage, WORD ctrlId);
	BOOL OnHScroll(HWND hTab, WPARAM wParam, LPARAM lParam);
#else // __linux__
	BOOL OnInitDialog(QWidget *hPage);
	BOOL OnCommand(QWidget *hPage, WORD ctrlId, WORD notification, QWidget *hCtrl);
	void OnItemClicked(QWidget *hPage, WORD ctrlId);
	BOOL OnHScroll(QWidget *hTab, int ctrlId, int request, int pos);
#endif // __linux__
};

/************************************************************************
 * \brief Visual helpers "Object frame axes" options page.
 */
class OptionsPage_Axes : public OptionsPage {
public:
	OptionsPage_Axes(OptionsPageContainer* container);
	int ResourceId() const;
	const char* Name() const;
	const HELPCONTEXT* HelpContext() const;
#ifndef __linux__
	void UpdateControls(HWND hPage);
#else // __linux__
	void UpdateControls(QWidget *hPage);
#endif // __linux__

protected:
#ifndef __linux__
	BOOL OnInitDialog(HWND hPage, WPARAM wParam, LPARAM lParam);
	BOOL OnCommand(HWND hPage, WORD ctrlId, WORD notification, HWND hCtrl);
	void OnItemClicked(HWND hPage, WORD ctrlId);
	BOOL OnHScroll(HWND hTab, WPARAM wParam, LPARAM lParam);
#else // __linux__
	BOOL OnInitDialog(QWidget *hPage);
	BOOL OnCommand(QWidget *hPage, WORD ctrlId, WORD notification, QWidget *hCtrl);
	void OnItemClicked(QWidget *hPage, WORD ctrlId);
	BOOL OnHScroll(QWidget *hTab, int ctrlId, int request, int pos);
#endif // __linux__
};

#endif // !__OPTIONSPAGES_H
