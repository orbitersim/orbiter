// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// =======================================================================
// class MenuInfoBar
// Rendering and user interface for main menu and info bars

#ifndef __MENUINFOBAR_H
#define __MENUINFOBAR_H

#ifndef __linux__
#define STRICT 1

#endif // !__linux__
#include "OrbiterAPI.h"
#ifdef __linux__
#include <memory>
#include <string>
#include <vector>
#endif // __linux__

// =======================================================================
// class MenuInfoBar
class DynamicMenuBar;
class InfoFrameRate;
class InfoTime;
class InfoTarget;

class MenuInfoBar {
	std::unique_ptr<DynamicMenuBar> dynMenu;
	std::unique_ptr<InfoFrameRate>  infoFps;
	std::unique_ptr<InfoTime>       infoTime;
	std::unique_ptr<InfoTarget>     infoTgt;
public:
	struct MenuPreference {
		std::string label;
		bool enabled;
	};

	MenuInfoBar ();
	~MenuInfoBar ();
	void RegisterMenuItem(const char *label, const char *imagepath, int id, CustomFunc func, void *context);
	void UnregisterMenuItem(int);
	std::vector<MenuPreference> &GetPreferences();
	void SyncPreferences();
};

#endif // !__MENUINFOBAR_H
