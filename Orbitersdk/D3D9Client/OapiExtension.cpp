// ==============================================================
// OapiExtension.cpp
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Released under GNU General Public License
// Copyright (C) 2012 Peter Schneider (Kuddel)
// ==============================================================

#include "OapiExtension.h"
#include "D3D9Config.h"
#include "OrbiterAPI.h"
#include "DebugControls.h"

// ===========================================================================
// Orbiter [v110830] up to [v111105]
#define IDC_BODYFORCE_PAGE           231
#define IDC_BODYFORCE_ENABLE        1194
#define IDC_BODYFORCE_WEIGHT        1195
#define IDC_BODYFORCE_THRUST        1196
#define IDC_BODYFORCE_LIFT          1197
#define IDC_BODYFORCE_DRAG          1198
#define IDC_BODYFORCE_TOTAL         1199
#define IDC_BODYFORCE_TORQUE        1200
#define IDC_BODYFORCE_SCALE_LIN     1201
#define IDC_BODYFORCE_SCALE_LOG     1202
#define IDC_BODYFORCE_SCALE_GAUGE   1204
#define IDC_BODYFORCE_OPACITY_GAUGE 1205

#define IDC_SHOW_AXES_PAGE           233
#define IDC_SHOW_AXES_ENABLE        1206
#define IDC_SHOW_AXES_VESSEL        1207
#define IDC_SHOW_AXES_CELBODY       1208
#define IDC_SHOW_AXES_SURFBASE      1209
#define IDC_SHOW_AXES_NEGATIVE      1210
#define IDC_SHOW_AXES_SCALE_GAUGE   1211
#define IDC_SHOW_AXES_OPACITY_GAUGE	1212

// Little binary helper
#define SETFLAG(bitmap, bit, value) (value ? bitmap |= bit : bitmap &= ~bit)

// ===========================================================================
// Class statics initialization

DWORD OapiExtension::showBodyForceVectorsFlags;
float OapiExtension::bodyForceScale;
float OapiExtension::bodyForceOpacity;

DWORD OapiExtension::showCoordinateAxesFlags;
float OapiExtension::coordinateAxesScale;
float OapiExtension::coordinateAxesOpacity;

// hooking
DWORD OapiExtension::hookMap = 0L;
HOOKINFO OapiExtension::hookInfos[] = {
	//       idc                , hookFlag     , lpWrapWndFunc
	{IDC_BODYFORCE_ENABLE       , BFV_ENABLE   , (WNDPROC)CheckBoxWindowProc, NULL, NULL, NULL, NULL},
	{IDC_BODYFORCE_WEIGHT       , BFV_WEIGHT   , (WNDPROC)CheckBoxWindowProc, NULL, NULL, NULL, NULL},
	{IDC_BODYFORCE_THRUST       , BFV_THRUST   , (WNDPROC)CheckBoxWindowProc, NULL, NULL, NULL, NULL},
	{IDC_BODYFORCE_LIFT         , BFV_LIFT     , (WNDPROC)CheckBoxWindowProc, NULL, NULL, NULL, NULL},
	{IDC_BODYFORCE_DRAG         , BFV_DRAG     , (WNDPROC)CheckBoxWindowProc, NULL, NULL, NULL, NULL},
	{IDC_BODYFORCE_TOTAL        , BFV_TOTAL    , (WNDPROC)CheckBoxWindowProc, NULL, NULL, NULL, NULL},
	{IDC_BODYFORCE_TORQUE       , BFV_TORQUE   , (WNDPROC)CheckBoxWindowProc, NULL, NULL, NULL, NULL},
	{IDC_BODYFORCE_SCALE_LOG    , BFV_SCALE_LOG, (WNDPROC)CheckBoxWindowProc, NULL, NULL, NULL, NULL},
	{IDC_SHOW_AXES_ENABLE       , SCA_ENABLE   , (WNDPROC)CheckBoxWindowProc, NULL, NULL, NULL, NULL},
	{IDC_SHOW_AXES_VESSEL       , SCA_VESSEL   , (WNDPROC)CheckBoxWindowProc, NULL, NULL, NULL, NULL},
	{IDC_SHOW_AXES_CELBODY      , SCA_CELBODY  , (WNDPROC)CheckBoxWindowProc, NULL, NULL, NULL, NULL},
	{IDC_SHOW_AXES_SURFBASE     , SCA_SURFBASE , (WNDPROC)CheckBoxWindowProc, NULL, NULL, NULL, NULL},
	{IDC_SHOW_AXES_NEGATIVE     , SCA_NEGATIVE , (WNDPROC)CheckBoxWindowProc, NULL, NULL, NULL, NULL},
	{IDC_BODYFORCE_PAGE         , 0x02000      , (WNDPROC)GaugeWindowProc   , NULL, NULL, NULL, NULL},
	{IDC_SHOW_AXES_PAGE         , 0x04000      , (WNDPROC)GaugeWindowProc   , NULL, NULL, NULL, NULL}
	//                       Sum: 0x07FFF
};

// ===========================================================================
// Construction
//
OapiExtension::OapiExtension(void)
{
	// Orbiters default here: "Bodyforces = 60 1 1"
	showBodyForceVectorsFlags = (BFV_WEIGHT | BFV_THRUST | BFV_LIFT | BFV_DRAG);
	bodyForceScale = 1.0;
	bodyForceOpacity = 1.0;
	// Orbiters default here: "CoordinateAxes = 4 1 1"
	showCoordinateAxesFlags = SCA_VESSEL;
	coordinateAxesScale = 1.0;
	coordinateAxesOpacity = 1.0;

	GetConfigParameter();

	if (Config->DisableVisualHelperReadout) {
		hookMap = 0x7FFF; // pretend all hooks are already set
	}
}

// ===========================================================================
// Destruction
//
OapiExtension::~OapiExtension(void)
{
	// Unhook still hooked methods, should not happen
	for (int i=0; i < sizeof(hookInfos)/sizeof(HOOKINFO); ++i) {
		RemoveHook(&hookInfos[i]);
	}
}


/*
------------------------------------------------------------------------------
	PUBLIC INTERFACE METHODS
------------------------------------------------------------------------------
*/

// ===========================================================================
// Same functionality than 'official' GetConfigParam, but for non-provided
// config parameters
//
const void *OapiExtension::GetConfigParam (DWORD paramtype) const
{
	switch (paramtype) {
		// Visual Helpers
		case CFGPRM_SHOWBODYFORCEVECTORSFLAG: return (void*)&showBodyForceVectorsFlags;
		case CFGPRM_BODYFORCEVECTORSSCALE   : return (void*)&bodyForceScale;
		case CFGPRM_BODYFORCEVECTORSOPACITY : return (void*)&bodyForceOpacity;
		case CFGPRM_SHOWCOORDINATEAXESFLAG  : return (void*)&showCoordinateAxesFlags;
		case CFGPRM_COORDINATEAXESSCALE     : return (void*)&coordinateAxesScale;
		case CFGPRM_COORDINATEAXESOPACITY   : return (void*)&coordinateAxesOpacity;
		// Debug Controls
		case CFGPRM_GETSELECTEDMESH         : return (void*)&DebugControls::sMesh;
		case CFGPRM_GETSELECTEDGROUP        : return (void*)&DebugControls::sGroup;
		case CFGPRM_GETDEBUGFLAGS           : return (void*)&DebugControls::debugFlags;
		case CFGPRM_GETDISPLAYMODE          : return (void*)&DebugControls::dspMode;
		case CFGPRM_GETCAMERAMODE           : return (void*)&DebugControls::camMode;
		case CFGPRM_GETCAMERASPEED          : return (void*)&DebugControls::camSpeed;

		default                             : return NULL;
	}
}

// ===========================================================================
// Hooks/Unhooks the popup windows; called whenever popup widows
// appear/disappear
//
const void OapiExtension::HandlePopupWindows (const HWND *hPopupWnd, DWORD count) const
{
	// New popup window we need to hook onto?
	if (count && !AllHooksAttached()) {
		for (DWORD i = 0; i < count; ++i) {
			if (IsOurDialog(hPopupWnd[i])) {
				EnumChildWindows(hPopupWnd[i], EnumChildProc, 0L);
			}
		}
	}
}


/*
------------------------------------------------------------------------------
	PRIVATE METHODS
------------------------------------------------------------------------------
*/

// ===========================================================================
// Tries to get the initial settings from Orbiter_NG.cfg file
//
void OapiExtension::GetConfigParameter(void)
{
	FILEHANDLE f = oapiOpenFile("Orbiter_NG.cfg", FILE_IN, ROOT);
	if (f) {
		char  string[32];
		DWORD flags;
		float scale, opacity;

		if (oapiReadItem_string(f, "Bodyforces", string)) {
			if (3 == sscanf_s(string, "%u %f %f", &flags, &scale, &opacity)) {
				showBodyForceVectorsFlags = flags;
				bodyForceScale = scale;
				bodyForceOpacity = opacity;
			}
		}

		if (oapiReadItem_string(f, "CoordinateAxes", string)) {
			if (3 == sscanf_s(string, "%u %f %f", &flags, &scale, &opacity)) {
				showCoordinateAxesFlags = flags;
				coordinateAxesScale = scale;
				coordinateAxesOpacity = opacity;
			}
		}

		oapiCloseFile(f, FILE_IN);
	}
}

// ===========================================================================
// Get pointer to hookInfo structure by control ID
//
const LPHOOKINFO OapiExtension::GetHookInfo(DWORD cid)
{
	for (int i=0; i< sizeof(hookInfos)/sizeof(HOOKINFO); ++i) {
		if (hookInfos[i].cid == cid) {
			return &hookInfos[i];
		}
	}
	return NULL;
}

// ===========================================================================
// Get pointer to hookInfo structure by window handle
//
const LPHOOKINFO OapiExtension::GetHookInfo(HWND hwnd)
{
	for (int i=0; i< sizeof(hookInfos)/sizeof(HOOKINFO); ++i) {
		if (hookInfos[i].hWnd == hwnd) {
			return &hookInfos[i];
		}
	}
	return NULL;
}

// ===========================================================================
// Removes the wrapped window function from the dialog item.
// Note: It does *not* clear the lpOrigWndFunc struct member, 'cause it may
// still be used with a (final) call to CallWindowProc()!
//
const void OapiExtension::RemoveHook(LPHOOKINFO lpHookInfo)
{
	if (lpHookInfo && lpHookInfo->lpOrigWndFunc && lpHookInfo->hWnd)
	{
		SetWindowLongA(lpHookInfo->hWnd, GWLP_WNDPROC, (LONG)lpHookInfo->lpOrigWndFunc);
		lpHookInfo->hWnd = NULL;
		lpHookInfo->hWndScaleGauge = NULL;
		lpHookInfo->hWndOpacityGauge = NULL;
		hookMap &= ~lpHookInfo->hookFlag;
	}
}

// ===========================================================================
// Check helper to avoid hooking 'Orbiter: Configure menu bars' dialog, which
// uses some of the same IDCs as 'Visual helpers' dialog.
//
const bool OapiExtension::IsOurDialog(HWND hwnd)
{
	static char buff[16] = {0};

	if (GetWindowText(hwnd, buff, 16) && !strncmp("Visual helpers", buff, 14)) {
		return true;
	}
	return false;
}

// ===========================================================================
// Child window enumeration callback (hooks the window message functions)
//
BOOL CALLBACK OapiExtension::EnumChildProc(HWND hwnd, LPARAM lParam)
{
	LPHOOKINFO lpHookInfo;

	int CtrlId = ::GetDlgCtrlID(hwnd);

	switch (CtrlId) {

	// --- Body Forces ---
	case IDC_BODYFORCE_ENABLE:
	case IDC_BODYFORCE_WEIGHT:
	case IDC_BODYFORCE_THRUST:
	case IDC_BODYFORCE_LIFT:
	case IDC_BODYFORCE_DRAG:
	case IDC_BODYFORCE_TOTAL:
	case IDC_BODYFORCE_TORQUE:
	case IDC_BODYFORCE_SCALE_LOG:
		lpHookInfo = GetHookInfo(CtrlId);
		SETFLAG(showBodyForceVectorsFlags, lpHookInfo->hookFlag, SendMessage(hwnd, BM_GETCHECK, 0L ,0L));
		lpHookInfo->hWnd = hwnd;
		lpHookInfo->lpOrigWndFunc = (WNDPROC)GetWindowLongA(hwnd, GWLP_WNDPROC);
		SetWindowLongA(hwnd, GWLP_WNDPROC, (LONG)lpHookInfo->lpWrapWndFunc);
		hookMap |= lpHookInfo->hookFlag;
		break;

	// --- Coordinate Axes ---
	case IDC_SHOW_AXES_ENABLE:
	case IDC_SHOW_AXES_VESSEL:
	case IDC_SHOW_AXES_CELBODY:
	case IDC_SHOW_AXES_SURFBASE:
	case IDC_SHOW_AXES_NEGATIVE:
		lpHookInfo = GetHookInfo(CtrlId);
		SETFLAG(showCoordinateAxesFlags, lpHookInfo->hookFlag, SendMessage(hwnd, BM_GETCHECK, 0L ,0L));
		lpHookInfo->hWnd = hwnd;
		lpHookInfo->lpOrigWndFunc = (WNDPROC)GetWindowLongA(hwnd, GWLP_WNDPROC);
		SetWindowLongA(hwnd, GWLP_WNDPROC, (LONG)lpHookInfo->lpWrapWndFunc);
		hookMap |= lpHookInfo->hookFlag<<8;
		break;

	// --- Gauges (body forces page) ---
	case IDC_BODYFORCE_SCALE_GAUGE:
	case IDC_BODYFORCE_OPACITY_GAUGE:
		lpHookInfo = GetHookInfo(IDC_BODYFORCE_PAGE);
		if (!lpHookInfo->hWnd) {
			lpHookInfo->hWnd = GetParent(hwnd);
			lpHookInfo->lpOrigWndFunc = (WNDPROC)GetWindowLongA(GetParent(hwnd), GWLP_WNDPROC);
			SetWindowLongA(GetParent(hwnd), GWLP_WNDPROC, (LONG)lpHookInfo->lpWrapWndFunc);
		}

		(CtrlId == IDC_BODYFORCE_SCALE_GAUGE) ? lpHookInfo->hWndScaleGauge = hwnd
			                                  : lpHookInfo->hWndOpacityGauge = hwnd;

		if (lpHookInfo->hWndScaleGauge && lpHookInfo->hWndOpacityGauge) {
			hookMap |= lpHookInfo->hookFlag;
		}
		break;

	// --- Gauges (coordinate axes page) ---
	case IDC_SHOW_AXES_SCALE_GAUGE:
	case IDC_SHOW_AXES_OPACITY_GAUGE:
		lpHookInfo = GetHookInfo(IDC_SHOW_AXES_PAGE);
		if (!lpHookInfo->hWnd) {
			lpHookInfo->hWnd = GetParent(hwnd);
			lpHookInfo->lpOrigWndFunc = (WNDPROC)GetWindowLongA(GetParent(hwnd), GWLP_WNDPROC);
			SetWindowLongA(GetParent(hwnd), GWLP_WNDPROC, (LONG)lpHookInfo->lpWrapWndFunc);
		}

		(CtrlId == IDC_SHOW_AXES_SCALE_GAUGE) ? lpHookInfo->hWndScaleGauge = hwnd
			                                  : lpHookInfo->hWndOpacityGauge = hwnd;

		if (lpHookInfo->hWndScaleGauge && lpHookInfo->hWndOpacityGauge) {
			hookMap |= lpHookInfo->hookFlag;
		}
		break;

	default:
		break;
	}
	return !AllHooksAttached();
}

// ===========================================================================
// Checkbox message procedure
//
LRESULT CALLBACK OapiExtension::CheckBoxWindowProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	int CtrlId;
	LPHOOKINFO lpHookInfo = GetHookInfo(hwnd);

	switch (uMsg) {

	case BM_SETCHECK:
		CtrlId = ::GetDlgCtrlID(hwnd);
		switch (CtrlId)
		{
		// --- Body Forces ---
		case IDC_BODYFORCE_ENABLE:
		case IDC_BODYFORCE_WEIGHT:
		case IDC_BODYFORCE_THRUST:
		case IDC_BODYFORCE_LIFT:
		case IDC_BODYFORCE_DRAG:
		case IDC_BODYFORCE_TOTAL:
		case IDC_BODYFORCE_TORQUE:
		case IDC_BODYFORCE_SCALE_LOG:
			SETFLAG(showBodyForceVectorsFlags, lpHookInfo->hookFlag, (wParam == BST_CHECKED));
			break;
		// --- Coordinate Axes ---
		case IDC_SHOW_AXES_ENABLE:
		case IDC_SHOW_AXES_VESSEL:
		case IDC_SHOW_AXES_CELBODY:
		case IDC_SHOW_AXES_SURFBASE:
		case IDC_SHOW_AXES_NEGATIVE:
			SETFLAG(showCoordinateAxesFlags, lpHookInfo->hookFlag, (wParam == BST_CHECKED));
			break;

		default:
			break;
		}
		break;

	case WM_DESTROY:
		RemoveHook(lpHookInfo);
		break;
	}
	return CallWindowProc(lpHookInfo->lpOrigWndFunc, hwnd, uMsg, wParam, lParam);
}

// ===========================================================================
// Gauges message procedure (operates on their parent dialog-window)
//
LRESULT CALLBACK OapiExtension::GaugeWindowProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	float      *pValue;
	WORD       scrollRequest;
	LPHOOKINFO lpHookInfo = GetHookInfo(hwnd);
	// --- Scale to Value calculation parameter ---
	float vmin;         // Min value for member value
	float vmax;         // Max value for member value
	float step;         // Step size for SB_LINELEFT, SB_LINERIGHT
	bool  isScaleGauge; // Whether it's a scale-gauge message or a opacity-gauge message

	switch (uMsg) {

	case WM_HSCROLL:

		isScaleGauge = (lpHookInfo->hWndScaleGauge == (HWND)lParam);

		// Select fitting scale 'constants'
		vmin = isScaleGauge ? 0.25f : 0.0f;
		vmax = isScaleGauge ? 4.0f  : 1.0f;
		step = (vmax - vmin) / 50.0f;

		// Get pointer to fitting member
		if (lpHookInfo->cid == IDC_BODYFORCE_PAGE) {
			pValue = isScaleGauge ? &bodyForceScale : &bodyForceOpacity;
		}
		else if (lpHookInfo->cid == IDC_SHOW_AXES_PAGE) {
			pValue = isScaleGauge ? &coordinateAxesScale : &coordinateAxesOpacity;
		}
		else {
			break; // should not happen!
		}

		scrollRequest = LOWORD(wParam);

		if (scrollRequest == SB_THUMBTRACK) {
			*pValue = (vmax - vmin) * HIWORD(wParam) / 50.0f + vmin;
		}
		else if (scrollRequest == SB_LINELEFT) {
			*pValue = max(vmin, *pValue - step);
		}
		else if (scrollRequest == SB_LINERIGHT) {
			*pValue = min(vmax, *pValue + step);
		}
		break;

	case WM_DESTROY:
		RemoveHook(lpHookInfo);
		break;
	}

	return CallWindowProc(lpHookInfo->lpOrigWndFunc, hwnd, uMsg, wParam, lParam);
}

// --- eof ---