// Copyright (c) Martin Schweiger
// Licensed under the MIT License

//-----------------------------------------------------------------------------
// Launchpad tab declaration: ModuleTab
// Plugin module activation
//-----------------------------------------------------------------------------

#ifndef __TABMODULE_H
#define __TABMODULE_H

#include "LpadTab.h"
#include <string>
#include <vector>

namespace orbiter {

	class ModuleTab : public LaunchpadTab {
	public:
		ModuleTab(LaunchpadDialog* lp);

		void Draw();

		void GetConfig(const Config* cfg);
		void SetConfig(Config* cfg);

	protected:
		void RefreshLists();
		// Update the list of available modules

		void SetActive(size_t idx, bool active);
		// activate or deactivate a module

		void DeactivateAll();
		// deactivate all modules

	private:
		struct ModuleRec {
			std::string name;
			std::string info;
			bool active;
			bool locked;
		};
		struct Category {
			std::string name;
			std::vector<size_t> modules;
		};
		std::vector<ModuleRec> m_modules;
		std::vector<Category> m_categories;
		int m_sel;
		int m_expand;
		float m_listWidth;
	};

}

#endif // !__TABMODULE_H
