// ==============================================================
// OapiExtension.cpp
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2012 - 2018 Peter Schneider (Kuddel)
// ==============================================================

#include <algorithm>
#include "OapiExtension.h"
#include "D3D9Config.h"
#include "OrbiterAPI.h"

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
#define IDC_SHOW_AXES_OPACITY_GAUGE 1212

// Little binary helper
#define SETFLAG(bitmap, bit, value) (value ? bitmap |= bit : bitmap &= ~bit)

// ===========================================================================
// Class statics initialization

DWORD OapiExtension::showBodyForceVectorsFlags = (BFV_WEIGHT | BFV_THRUST | BFV_LIFT | BFV_DRAG);
float OapiExtension::bodyForceScale = 1.0;
float OapiExtension::bodyForceOpacity = 1.0;
// Orbiters default here: "CoordinateAxes = 4 1 1"
DWORD OapiExtension::showCoordinateAxesFlags = SCA_VESSEL;
float OapiExtension::coordinateAxesScale = 1.0;
float OapiExtension::coordinateAxesOpacity = 1.0;
// Orbiters default directories
std::string OapiExtension::configDir(".\\Config\\");
std::string OapiExtension::meshDir(".\\Meshes\\");
std::string OapiExtension::textureDir(".\\Textures\\");
std::string OapiExtension::hightexDir(".\\Textures2\\");
std::string OapiExtension::scenarioDir(".\\Scenarios\\");

std::string OapiExtension::startupScenario = OapiExtension::ScanCommandLine();

bool OapiExtension::configParameterRead = OapiExtension::GetConfigParameter();

// 2010       100606
// 2010-P1    100830
// 2010-P2    110822
// 2010-P2.1  110824
bool OapiExtension::isOrbiter2010 = (oapiGetOrbiterVersion() <= 110824 && oapiGetOrbiterVersion() >= 100606);

bool OapiExtension::orbiterSound40 = false;
bool OapiExtension::tileLoadThread = true;
bool OapiExtension::runsUnderWINE = false;
bool OapiExtension::runsSpacecraftDll = false;

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
	//                       Sum: 0x07FFF (SCA_xxx hookFlag values are shifted! (<<8) )
};

// ===========================================================================
// Construction
//
OapiExtension::OapiExtension(void) {
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
// Initialization
//
void OapiExtension::GlobalInit(D3D9Config &Config)
{
	if (Config.DisableVisualHelperReadout) {
		hookMap = 0x7FFF; // pretend all hooks are already set
	}
}

// ===========================================================================
// Same functionality than 'official' GetConfigParam, but for non-provided
// config parameters
//
const void *OapiExtension::GetConfigParam (DWORD paramtype)
{
	switch (paramtype) {
		case CFGPRM_SHOWBODYFORCEVECTORSFLAG: return (void*)&showBodyForceVectorsFlags;
		case CFGPRM_BODYFORCEVECTORSSCALE   : return (void*)&bodyForceScale;
		case CFGPRM_BODYFORCEVECTORSOPACITY : return (void*)&bodyForceOpacity;
		case CFGPRM_SHOWCOORDINATEAXESFLAG  : return (void*)&showCoordinateAxesFlags;
		case CFGPRM_COORDINATEAXESSCALE     : return (void*)&coordinateAxesScale;
		case CFGPRM_COORDINATEAXESOPACITY   : return (void*)&coordinateAxesOpacity;
		case CFGPRM_TILELOADTHREAD          : return (void*)&tileLoadThread;
		default                             : return NULL;
	}
}

// ===========================================================================
// Hooks/Unhooks the popup windows; called whenever popup widows
// appear/disappear
//
void OapiExtension::HandlePopupWindows (const HWND *hPopupWnd, DWORD count)
{
	// New popup window we need to hook onto?
	if (count && !AllHooksAttached()) {
		for (DWORD i = 0; i < count; ++i) {
			if (IsOurDialog(hPopupWnd[i])) {
				EnumChildWindows(hPopupWnd[i], EnumChildProc, 0L);
				break;
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
bool OapiExtension::GetConfigParameter(void)
{
	char *pLine;
	bool orbiterSoundModuleEnabled = false;

	FILEHANDLE f = oapiOpenFile("Orbiter_NG.cfg", FILE_IN_ZEROONFAIL, ROOT);
	if (f) {
		char  string[MAX_PATH];
		DWORD flags;
		float scale, opacity;

		// General check for OrbiterSound module enabled
		while (oapiReadScenario_nextline(f, pLine)) {
			if (NULL != strstr(pLine, "OrbiterSound")) {
				orbiterSoundModuleEnabled = true;
				break;
			}
		}

		if (oapiReadItem_string(f, "Bodyforces", string)) {
			if (3 == sscanf_s(string, "%lu %f %f", &flags, &scale, &opacity)) {
				showBodyForceVectorsFlags = flags;
				bodyForceScale = scale;
				bodyForceOpacity = opacity;
			}
		}

		if (oapiReadItem_string(f, "CoordinateAxes", string)) {
			if (3 == sscanf_s(string, "%lu %f %f", &flags, &scale, &opacity)) {
				showCoordinateAxesFlags = flags;
				coordinateAxesScale = scale;
				coordinateAxesOpacity = opacity;
			}
		}

		// Get planet rendering parameters
		oapiReadItem_bool(f, "TileLoadThread", tileLoadThread);

		// Get directory config
		if (oapiReadItem_string(f, "ConfigDir", string)) {
			configDir = string;
		}
		if (oapiReadItem_string(f, "MeshDir", string)) {
			meshDir = string;
		}
		if (oapiReadItem_string(f, "TextureDir", string)) {
			textureDir = string;
		}
		if (oapiReadItem_string(f, "HightexDir", string)) {
			hightexDir = string;
		}
		if (oapiReadItem_string(f, "ScenarioDir", string)) {
			scenarioDir = string;
		}

		oapiCloseFile(f, FILE_IN_ZEROONFAIL);

		// Log directory config
		auto logPath = [](const char *name, const std::string &path) {
			TCHAR buff[MAX_PATH];
			if (GetFullPathName(path.c_str(), MAX_PATH, buff, NULL)) {
				DWORD ftyp = GetFileAttributes(buff);
				auto result = (ftyp == INVALID_FILE_ATTRIBUTES || !(ftyp & FILE_ATTRIBUTE_DIRECTORY) ? " [[DIR NOT FOUND!]]" : "");
				oapiWriteLogV("%-11s: %s%s", name, buff, result);
			}
		};
		oapiWriteLog("---------------------------------------------------------------");
		logPath("BaseDir"    , ".\\");
		logPath("ConfigDir"  , configDir);
		logPath("MeshDir"    , meshDir);
		logPath("TextureDir" , textureDir);
		logPath("HightexDir" , hightexDir);
		logPath("ScenarioDir", scenarioDir);
		oapiWriteLog("---------------------------------------------------------------");
	}

	// Check for the OrbiterSound version
	if (orbiterSoundModuleEnabled)  {
		orbiterSound40 = false;

		f = oapiOpenFile("Sound\\version.txt", FILE_IN_ZEROONFAIL, ROOT);
		while (f && oapiReadScenario_nextline(f, pLine)) {
			if (NULL != strstr(pLine, "OrbiterSound 4.0 (3D)")) {
				orbiterSound40 = true;
				break;
			}
		}
		oapiCloseFile(f, FILE_IN_ZEROONFAIL);
	}

	// Check for WINE environment
	HMODULE hntdll = GetModuleHandle("ntdll.dll");
	if (NULL != hntdll)
	{
		// static const char * (CDECL *pwine_get_version)(void);
		void *pWineGetVersion = (void *)GetProcAddress(hntdll, "wine_get_version");
		if (NULL != pWineGetVersion) {
			runsUnderWINE = true;
		} // else { Not running WINE }
	} // else { Not running on NT ?! }

	return true;
}

// ===========================================================================
// Try to read a startup scenario given by "-s" command line parameter
//
std::string OapiExtension::ScanCommandLine (void)
{
	std::string commandLine(GetCommandLine());
	toLower( commandLine );

	// Is there a "-s <scenario_name>" option at all?
	unsigned pos = commandLine.rfind("-s");
	if (pos != std::string::npos)
	{
		std::string scenarioName = trim( commandLine.substr(pos+2, std::string::npos) );

		// Remove (optional) quotes
		std::replace(scenarioName.begin(), scenarioName.end(), '"', ' ');

		// Build the path (like ".\\Scenarios\\(Current State).scn"
		//startupScenario = GetScenarioDir() + trim(scenarioName) + ".scn";
		return GetScenarioDir() + trim(scenarioName) + ".scn";
	}
	return "";
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
// Check helper to avoid hooking 'Orbiter: Configure menu bars' dialog, or
// any other addon-dialog which might use some of the same IDCs as the
// 'Visual helpers' dialog.
//
const bool OapiExtension::IsOurDialog(HWND hwnd)
{
	static char buff[16] = {0};
	return GetWindowText(hwnd, buff, 16)
	    && !strncmp("Visual helpers", buff, 14);
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
	float vmin(0);             // Min value for member value
	float vmax(0);             // Max value for member value
	float step(0);             // Step size for SB_LINELEFT, SB_LINERIGHT
	bool  isScaleGauge(false); // Whether it's a scale-gauge message or a opacity-gauge message

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