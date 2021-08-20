// Copyright (c) Martin Schweiger
// Licensed under the MIT License

//=============================================================================
// VideoTab class
//=============================================================================

#define OAPI_IMPLEMENTATION

#include <windows.h>
#include <commctrl.h>
#include <io.h>
#include "Orbiter.h"
#include "TabVideo.h"
#include "resource.h"

using namespace std;

//-----------------------------------------------------------------------------
// DefVideoTab class

orbiter::DefVideoTab::DefVideoTab (const LaunchpadDialog *lp): LaunchpadTab (lp)
{
}

//-----------------------------------------------------------------------------

void orbiter::DefVideoTab::Create ()
{
	hTab = CreateTab (IDD_PAGE_DEV);

	static int item[] = {
		IDC_VID_STATIC1, IDC_VID_STATIC2, IDC_VID_STATIC3, IDC_VID_STATIC5,
		IDC_VID_STATIC6, IDC_VID_STATIC7, IDC_VID_STATIC8, IDC_VID_STATIC9,
		IDC_VID_DEVICE, IDC_VID_ENUM, IDC_VID_STENCIL,
		IDC_VID_FULL, IDC_VID_WINDOW, IDC_VID_MODE, IDC_VID_BPP, IDC_VID_VSYNC,
		IDC_VID_PAGEFLIP, IDC_VID_WIDTH, IDC_VID_HEIGHT, IDC_VID_ASPECT,
		IDC_VID_4X3, IDC_VID_16X10, IDC_VID_16X9, IDC_VID_INFO
	};

	RegisterItemPositions (item, 24);
}

//-----------------------------------------------------------------------------

void orbiter::DefVideoTab::SetConfig (Config *cfg)
{
	// retrieve standard parameters from client, if available
	oapi::GraphicsClient *gc = pLp->App()->GetGraphicsClient();
	if (gc) {
		gc->clbkRefreshVideoData();
		oapi::GraphicsClient::VIDEODATA *data = gc->GetVideoData();
		cfg->CfgDevPrm.bFullscreen = data->fullscreen;
		cfg->CfgDevPrm.bNoVsync    = data->novsync;
		cfg->CfgDevPrm.bPageflip   = data->pageflip;
		cfg->CfgDevPrm.bTryStencil = data->trystencil;
		cfg->CfgDevPrm.bForceEnum  = data->forceenum;
		cfg->CfgDevPrm.WinW        = data->winw;
		cfg->CfgDevPrm.WinH        = data->winh;
		cfg->CfgDevPrm.Device_idx  = data->deviceidx;
		cfg->CfgDevPrm.Device_mode = data->modeidx;
	} else {
		// should not be required
		cfg->CfgDevPrm.bFullscreen = false;
		cfg->CfgDevPrm.bNoVsync    = true;
		cfg->CfgDevPrm.bPageflip   = true;
		cfg->CfgDevPrm.bTryStencil = false;
		cfg->CfgDevPrm.bForceEnum  = true;
		cfg->CfgDevPrm.WinW        = 400;
		cfg->CfgDevPrm.WinH        = 300;
		cfg->CfgDevPrm.Device_idx  = 0;
		cfg->CfgDevPrm.Device_mode = 0;
	}
	cfg->CfgDevPrm.bStereo = false; // not currently set
}

//-----------------------------------------------------------------------------

bool orbiter::DefVideoTab::OpenHelp ()
{
	OpenTabHelp ("tab_video");
	return true;
}

//-----------------------------------------------------------------------------

INT_PTR orbiter::DefVideoTab::TabProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	// divert video parameters to graphics clients
	oapi::GraphicsClient *gc = pLp->App()->GetGraphicsClient();
	if (gc)
		gc->LaunchpadVideoWndProc (hWnd, uMsg, wParam, lParam);

	return FALSE;
}