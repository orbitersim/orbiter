// Copyright (c) Martin Schweiger
// Licensed under the MIT License

//-----------------------------------------------------------------------------
// Launchpad tab declaration: ScenarioTab
// Scenario selection
//-----------------------------------------------------------------------------

#ifndef __TABSCENARIO_H
#define __TABSCENARIO_H

#include <CommCtrl.h>
#include "LpadTab.h"
#include "CustomControls.h"
#include <filesystem>
#include "SDLUtil.h"
namespace fs = std::filesystem;

namespace orbiter {

	struct ScenarioTreeItem {
		Image* icon;
		Image* selIcon;
		fs::path path;
		std::string name;
	};

	struct ScenarioTree {
		ScenarioTreeItem item;
		std::vector<ScenarioTree> children;
	};

	class ScenarioTab : public orbiter::LaunchpadTab2 {
	public:
		ScenarioTab(const orbiter::LaunchpadDialog2* lp);
		~ScenarioTab();

		void Create() override;

		void GetConfig(const Config* cfg);
		void SetConfig(Config* cfg);

		bool OpenHelp();

		bool DynamicSize() const { return true; }

		BOOL OnSize(int w, int h);

		int GetSelScenario(char* scn, int len);
		// returns name of currently selected scenario file
		// (without path or extension)

		void LaunchpadShowing(bool show);

		BOOL OnNotify(HWND hDlg, int idCtrl, LPNMHDR pnmh);

		BOOL OnMessage(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);

		void OnDraw() override;

	protected:
		void RefreshList(bool preserveSelection);
		// refresh the scenario list

		void ScanDirectory(const fs::path &path, HTREEITEM hti);
		// scan scenario files from a subdirectory

		void ScenarioChanged();
		// Respond to a user scenario selection

		void SaveCurScenario();
		// open dialog to allow saving of current scenario, if available

		int SaveCurScenarioAs(const char* name, char* desc, bool replace = false);
		// copy current scenario file into 'name', replacing description with 'desc'.
		// return value: 0=ok, 1=failed, 2=file exists (only checked if replace=false)

		void ClearQSFolder();
		// remove all scenarios in the quicksave folder

		void OpenScenarioHelp();
		// open the help file associated with the current scenario

	private:
		static INT_PTR CALLBACK SaveProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
		// callback function for "scenario save" dialog

		static DWORD WINAPI threadWatchScnList(LPVOID pPrm);
		// thread function for scenario list watcher

		ScenarioTree BuildScnTree(ScenarioTreeItem root);
		void RenderTree(const ScenarioTree& tree);

		SplitterCtrl splitListDesc;  // splitter control for scenario list(left) and description(right)

		Image* img_folder1;
		Image* img_folder2;
		Image* img_scn1;
		ScenarioTree tree;
		fs::path selection;
		char scnhelp[128];       // scenario help string, if available
		int infoId;              // IDC_SCN_HTML or IDC_SCN_INFO, depending on which is active
		bool htmldesc;           // Use embedded html viewer for scenario description
		HANDLE hThread;          // scenario directory tree watcher
		bool startPaused;
	};

}

#endif // !__TABSCENARIO_H