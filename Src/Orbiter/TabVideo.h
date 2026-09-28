// Copyright (c) Martin Schweiger
// Licensed under the MIT License

//-----------------------------------------------------------------------------
// Launchpad tab declaration: class DefVideoTab
// Tab for default video device parameters
//-----------------------------------------------------------------------------

#ifndef __TABVIDEO_H
#define __TABVIDEO_H

#include "LpadTab.h"
#include <filesystem>
namespace fs = std::filesystem;

namespace orbiter {

	class DefVideoTab : public LaunchpadTab {
	public:
		DefVideoTab(const LaunchpadDialog* lp);
		~DefVideoTab();

		void Create();

#ifndef __linux__
		BOOL OnInitDialog(HWND hWnd, WPARAM wParam, LPARAM lParam);
#else // __linux__
		BOOL OnInitDialog(QWidget *hWnd);
#endif // __linux__

#ifndef __linux__
		void OnGraphicsClientLoaded(oapi::GraphicsClient* gc, const PSTR moduleName);
#else // __linux__
		void OnGraphicsClientLoaded(oapi::GraphicsClient* gc, const char *moduleName);
#endif // __linux__

		void SetConfig(Config* cfg);

		bool OpenHelp();

#ifndef __linux__
		BOOL OnMessage(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);

#endif // !__linux__
	protected:
#ifndef __linux__
		void ShowInterface(HWND hTab, bool show);
#else // __linux__
		void ShowInterface(QWidget *hTab, bool show);
#endif // __linux__

#ifndef __linux__
		void EnumerateClients(HWND hTab);
#else // __linux__
		void EnumerateClients(QWidget *hTab);
#endif // __linux__

#ifndef __linux__
		void ScanDir(HWND hTab, const fs::path &dir);
#else // __linux__
		void ScanDir(QWidget *hTab, const fs::path &dir);
#endif // __linux__
		// scan directory dir (relative to Orbiter root) for graphics clients
		// and enter them in the combo box

		void SelectClientIndex(UINT idx);

		void SetInfoString(PCSTR str);

#ifndef __linux__
		static INT_PTR CALLBACK InfoProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
#else // __linux__
		static void InfoProc(QWidget *hWnd, const char *info);
#endif // __linux__

	private:
		UINT idxClient;
		char* strInfo;
	};

}

#endif // !__TABVIDEO_H
