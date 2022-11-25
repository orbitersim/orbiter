// Copyright (c) Martin Schweiger
// Licensed under the MIT License

//=============================================================================
// ParameterTab class
//=============================================================================

#include <windows.h>
#include <commctrl.h>
#include "TabParam.h"
#include "resource.h"

//-----------------------------------------------------------------------------

orbiter::ParameterTab::ParameterTab (const LaunchpadDialog *_lp): LaunchpadTab (_lp)
{
}

//-----------------------------------------------------------------------------

void orbiter::ParameterTab::Create ()
{
	hTab = CreateTab (IDD_PAGE_OPT);

//	static int item[] = {
//	};

//	RegisterItemPositions (item, ARRAYSIZE(item));
}

//-----------------------------------------------------------------------------

void orbiter::ParameterTab::GetConfig (const Config *cfg)
{
}

//-----------------------------------------------------------------------------

void orbiter::ParameterTab::SetConfig (Config *cfg)
{
	DWORD i;
	char cbuf[128];
	double d;
}

//-----------------------------------------------------------------------------

bool orbiter::ParameterTab::OpenHelp ()
{
	OpenTabHelp ("tab_param");
	return true;
}