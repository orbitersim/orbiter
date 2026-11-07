// Copyright (c) Martin Schweiger
// Licensed under the MIT License

//-----------------------------------------------------------------------------
// Launchpad tab declaration: OptionsTab
// Simulation options
//-----------------------------------------------------------------------------

#ifndef __TABOPTIONS_H
#define __TABOPTIONS_H

#include "LpadTab.h"
#include <memory>
#include <string>
#include <vector>

class DlgOptions;

namespace orbiter {

	class OptionsTab : public LaunchpadTab {
	public:
		OptionsTab(LaunchpadDialog* lp);
		~OptionsTab();

		void Draw();
		bool OpenHelp();
		void LaunchpadShowing(bool show);

	private:
		struct Page {
			std::string name;
			const char* helptopic;
			void (DlgOptions::* func)();
			bool child;
		};
		std::vector<Page> m_pages;
		std::unique_ptr<DlgOptions> m_opt;
		size_t m_page;
	};

}

#endif // !__TABOPTIONS_H
