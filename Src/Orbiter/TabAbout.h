// Copyright (c) Martin Schweiger
// Licensed under the MIT License

//-----------------------------------------------------------------------------
// Launchpad tab declaration: AboutTab
// Version, credits and links
//-----------------------------------------------------------------------------

#ifndef __TABABOUT_H
#define __TABABOUT_H

#include "LpadTab.h"

namespace orbiter {

	class AboutTab : public LaunchpadTab {
	public:
		AboutTab(LaunchpadDialog* lp);

		void Draw();

	private:
		bool m_openLicense;
	};

}

#endif // !__TABABOUT_H
