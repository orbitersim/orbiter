// Copyright (c) Martin Schweiger
// Licensed under the MIT License

//-----------------------------------------------------------------------------
// Launchpad tab declaration: class ParameterTab
// General simulation parameters
//-----------------------------------------------------------------------------

#ifndef __TABPARAM_H
#define __TABPARAM_H

#include "LpadTab.h"

namespace orbiter {

	class ParameterTab : public LaunchpadTab {
	public:
		ParameterTab(const LaunchpadDialog* lp);

		void Create();

		void GetConfig(const Config* cfg);
		void SetConfig(Config* cfg);

		bool OpenHelp();
	};

}

#endif // !__TABPARAM_H