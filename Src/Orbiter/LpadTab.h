// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#ifndef __LPADTAB_H
#define __LPADTAB_H

#include <windows.h>
#include "Config.h"
#include "Launchpad.h"

namespace orbiter {

	class LaunchpadDialog;

	//-----------------------------------------------------------------------------
	// Name: class LaunchpadTab
	// Desc: Base class for Launchpad tabs. Each tab draws its contents with
	//       ImGui into the page area of the Launchpad window.
	//-----------------------------------------------------------------------------
	class LaunchpadTab {
	public:
		LaunchpadTab(LaunchpadDialog* lp, const char* label, const char* helptopic);
		virtual ~LaunchpadTab();

		inline LaunchpadDialog* Launchpad() const { return pLp; }
		inline Config* Cfg() const { return pCfg; }
		inline const char* Label() const { return m_label; }

		virtual void Draw() = 0;
		// Draw the tab contents (called every frame while the tab is shown)

		virtual void GetConfig(const Config* cfg) {}
		// Read config parameters

		virtual void SetConfig(Config* cfg) {}
		// Write config parameters back

		virtual bool OpenHelp();
		// Open the help page for this tab

		virtual void Activated() {}
		// Called when the tab becomes the current page

		virtual void LaunchpadShowing(bool show) {}

		void OpenTabHelp(const char* topic);

	protected:
		LaunchpadDialog* pLp;
		Config* pCfg;
		const char* m_label;
		const char* m_helptopic;
	};

}

#endif // !__LPADTAB_H
