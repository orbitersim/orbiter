// Copyright (c) Martin Schweiger
// Licensed under the MIT License

//-----------------------------------------------------------------------------
// Launchpad tab declaration: class ModuleTab
// Tab for module activation/deactivation
//-----------------------------------------------------------------------------

#ifndef __TABMODULE_H
#define __TABMODULE_H

#include "LpadTab.h"
#include "CustomControls.h"

#ifdef __linux__
class QTreeWidgetItem;

#endif // __linux__
namespace orbiter {

	class ModuleTab : public LaunchpadTab {
	public:
		ModuleTab(const LaunchpadDialog* lp);
		~ModuleTab();

		void Create();
#ifndef __linux__
		BOOL OnInitDialog(HWND hWnd, WPARAM wParam, LPARAM lParam);
#else // __linux__
		BOOL OnInitDialog(QWidget *hWnd);
#endif // __linux__

		void GetConfig(const Config* cfg);
		void SetConfig(Config* cfg);

		bool OpenHelp();

		bool DynamicSize() const { return true; }

		BOOL OnSize(int w, int h);

#ifndef __linux__
		BOOL OnNotify(HWND hDlg, int idCtrl, LPNMHDR pnmh);

		BOOL OnMessage(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);

#endif // !__linux__
	protected:
		void Show();

		void RefreshLists();
		// Update active and inactive module lists

		void DeactivateAll();
		// deactivate all modules

		void ActivateFromList();
		// synchronises module activation with list by activating/deactivating
		// appropriate modules

		void InitActivation();
		// activate modules listed in config file and tick entries in list

		void ExpandCollapseAll(bool expand);
#ifndef __linux__
		HTREEITEM GetCategoryItem(char* cat);
#else // __linux__
		QTreeWidgetItem *GetCategoryItem(char* cat);
#endif // __linux__

	private:
		SplitterCtrl splitListDesc;  // splitter control for module list(left) and description(right)
		RECT r_bt0, r_bt1, r_bt2;
		RECT r_lst0, r_dsc0;     // REMOVE!
		RECT r_pane;             // initial position of list/description splitter pane

		struct MODULEREC {
			char* name;
			char* info;
			bool active;
			bool locked;
		} **modulerec;
		int nmodulerec;
	};

}

#endif // !__TABMODULE_H
