// Copyright (c) Martin Schweiger
// Licensed under the MIT License

//-----------------------------------------------------------------------------
// Launchpad tab declaration: class ParameterTab
// General simulation parameters
//-----------------------------------------------------------------------------

#ifndef __TABPARAM_H
#define __TABPARAM_H

#include "LpadTab.h"

class ParameterTab: public LaunchpadTab {
public:
	ParameterTab (const MainDialog *lp);

	void Create ();

	void GetConfig (const Config *cfg);
	void SetConfig (Config *cfg);

	bool OpenHelp ();
};

#endif // !__TABPARAM_H