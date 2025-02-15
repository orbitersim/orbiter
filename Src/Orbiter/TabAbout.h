// Copyright (c) Martin Schweiger
// Licensed under the MIT License

//-----------------------------------------------------------------------------
// Launchpad tab definition: class AboutTab
// Tab for "about" page
//-----------------------------------------------------------------------------

#ifndef __TABABOUT_H
#define __TABABOUT_H

#include "LpadTab.h"

namespace orbiter {

	class AboutTab : public LaunchpadTab2 {
	public:
		explicit AboutTab(const LaunchpadDialog2* lp);

		void OnDraw(oapi::WithImCtx<LpImCtx> &ctx) override;
	private:
		std::shared_ptr<LpImage> icon;
		float savedWidth;
		void RenderCentered(oapi::WithImCtx<LpImCtx> &ctx);
	};

}

#endif // !__TABABOUT_H
