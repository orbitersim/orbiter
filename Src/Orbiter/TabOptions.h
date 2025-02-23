// Copyright (c) Martin Schweiger
// Licensed under the MIT License

//-----------------------------------------------------------------------------
// Launchpad tab declaration: OptionsTab
// Simulation options
//-----------------------------------------------------------------------------

#ifndef __TABOPTIONS_H
#define __TABOPTIONS_H

#include "LpadTab.h"
#include "OptionsPages.h"

namespace orbiter {

	class OptionsTab : public orbiter::LaunchpadTab2, public OptionsPageContainer {
	public:
		explicit OptionsTab(orbiter::LaunchpadDialog2* lp);

		void GetConfig(const Config *cfg) override;
		void SetConfig(Config* cfg) override;

		bool OpenHelp();

		void OnDraw(WithLpImCtx &ctx) override;
	};
}

#endif // !__TABOPTIONS_H
