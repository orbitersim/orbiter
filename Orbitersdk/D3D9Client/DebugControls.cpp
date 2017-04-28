// ===========================================================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2012 - 2016 Jarmo Nikkanen
// ===========================================================================================


#include "D3D9Client.h"
#include "resource.h"
#include "D3D9Config.h"
#include "D3D9Surface.h"
#include "DebugControls.h"
#include "Commctrl.h"
#include "vObject.h"
#include "vVessel.h"
#include "vPlanet.h"
#include "Mesh.h"
#include "MaterialMgr.h"
#include "VectorHelpers.h"
#include <stdio.h>

using namespace oapi;

extern HINSTANCE g_hInst;
extern D3D9Client *g_client;

// Little binary helper
#define SETFLAG(bitmap, bit, value) (value ? bitmap |= bit : bitmap &= ~bit)
#define CLAMP(x,a,b) min(max(a,x),b) 

namespace DebugControls {

DWORD dwCmd, nMesh, nGroup, sMesh, sGroup, debugFlags, dspMode, camMode, SelColor;
double camSpeed;
float cpr, cpg, cpb, cpa;
double resbias = 4.0;
char visual[64];
int  origwidth;
HWND hDlg = NULL;
HWND hDataWnd = NULL;
vObject *vObj = NULL;
std::string buffer("");

HWND hTipRed, hTipGrn, hTipBlu, hTipAlp;

OPENFILENAMEA OpenTex, SaveTex;
char OpenFileName[255];
char SaveFileName[255];

void UpdateMaterialDisplay(bool bSetup=false);

struct _Variable {
	float min, max, def;
	bool bLog;
	bool bUsed;
	bool bGamma;
	char tip[80];
};

struct _Params {
	_Variable var[4];
};


_Params Params[17] = { 0 };



// ===========================================================================
// Same functionality than 'official' GetConfigParam, but for non-provided
// debug-control config parameters
//
const void *GetConfigParam (DWORD paramtype)
{
	switch (paramtype) {
		case CFGPRM_GETSELECTEDMESH  : return (void*)&sMesh;
		case CFGPRM_GETSELECTEDGROUP : return (void*)&sGroup;
		case CFGPRM_GETDEBUGFLAGS    : return (void*)&debugFlags;
		case CFGPRM_GETDISPLAYMODE   : return (void*)&dspMode;
		case CFGPRM_GETCAMERAMODE    : return (void*)&camMode;
		case CFGPRM_GETCAMERASPEED   : return (void*)&camSpeed;
		default                      : return NULL;
	}
}

// =============================================================================================
//
float GetFloatFromBox(HWND hWnd, int item)
{
	char lbl[32];
	GetWindowTextA(GetDlgItem(hWnd, item), lbl, 32);	
	return float(atof(lbl));
}

// =============================================================================================
//
HWND CreateToolTip(int toolID, HWND hDlg, PTSTR pszText)
{
    if (!toolID || !hDlg || !pszText) return NULL;
    
    // Get the window of the tool.
    HWND hwndTool = GetDlgItem(hDlg, toolID);
    // Create the tooltip. g_hInst is the global instance handle.
    HWND hwndTip = CreateWindowEx(NULL, TOOLTIPS_CLASS, NULL, WS_POPUP |TTS_ALWAYSTIP | TTS_BALLOON, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, hDlg, NULL, g_hInst, NULL);
    
    if (!hwndTool || !hwndTip) return NULL;
                                                          
    // Associate the tooltip with the tool.
    TOOLINFO toolInfo = { 0 };
    toolInfo.cbSize = sizeof(toolInfo);
    toolInfo.hwnd = hDlg;
    toolInfo.uFlags = TTF_IDISHWND | TTF_SUBCLASS;
    toolInfo.uId = (UINT_PTR)hwndTool;
    toolInfo.lpszText = pszText;
    SendMessage(hwndTip, TTM_ADDTOOL, 0, (LPARAM)&toolInfo);

    return hwndTip;
}


void SetToolTip(int toolID, HWND hTip, const char *a)
{
	HWND hwndTool = GetDlgItem(hDlg, toolID);
	TOOLINFO toolInfo = { 0 };
	toolInfo.cbSize = sizeof(toolInfo);
	toolInfo.hwnd = hDlg;
	toolInfo.uFlags = TTF_IDISHWND | TTF_SUBCLASS;
	toolInfo.uId = (UINT_PTR)hwndTool;
	toolInfo.lpszText = (PTSTR)a;
	SendMessage(hTip, TTM_UPDATETIPTEXT, 0, (LPARAM)&toolInfo);
}

// =============================================================================================
//
void Create()
{
	vObj = NULL;
	hDlg = NULL;
	nMesh = 0;
	nGroup = 0;
	sMesh = 0;
	sGroup = 0;
	debugFlags = 0;
	camSpeed = 0.5;
	camMode = 0;
	dspMode = 0;
	SelColor = 0;

	cpr = cpg = cpb = cpa = 0.0f;

	if (Config->EnableMeshDbg) {
		dwCmd = oapiRegisterCustomCmd("D3D9 Debug Controls", "This dialog allows to control various debug controls", OpenDlgClbk, NULL);
	}
	else {
		dwCmd = 0;
	}

	resbias = 4.0 + Config->LODBias;
  
	memset(&OpenTex, 0, sizeof(OPENFILENAME));
	memset(OpenFileName, 0, sizeof(OpenFileName));

	OpenTex.lStructSize = sizeof(OPENFILENAME);
	OpenTex.lpstrFile = OpenFileName;
	OpenTex.lpstrInitialDir = "Textures\0";
	OpenTex.nMaxFile = sizeof(OpenFileName);
	OpenTex.lpstrFilter = "*.dds;*.jpg;*.png;*.hdr;*.bmp;*.tga\0";
	OpenTex.nFilterIndex = 0;
	OpenTex.lpstrFileTitle = NULL;
	OpenTex.nMaxFileTitle = 0;
	OpenTex.Flags = OFN_PATHMUSTEXIST | OFN_FILEMUSTEXIST | OFN_NOCHANGEDIR;

	memset(&SaveTex, 0, sizeof(OPENFILENAME));
	memset(SaveFileName, 0, sizeof(SaveFileName));

	SaveTex.lStructSize = sizeof(OPENFILENAME);
	SaveTex.lpstrFile = SaveFileName;
	SaveTex.lpstrInitialDir = "Textures\0";
	SaveTex.nMaxFile = sizeof(SaveFileName);
	SaveTex.lpstrFilter = "*.dds\0";
	SaveTex.nFilterIndex = 0;
	SaveTex.lpstrFileTitle = NULL;
	SaveTex.nMaxFileTitle = 0;
	SaveTex.Flags = OFN_OVERWRITEPROMPT | OFN_NOCHANGEDIR;
}

// =============================================================================================
//
void Close()
{
	if (hDlg != NULL) {
		oapiCloseDialog(hDlg);
		hDlg = NULL;
	}
	vObj = NULL;
}

// =============================================================================================
//
bool IsActive()
{
	return (hDlg!=NULL);
}

// =============================================================================================
//
int GetSceneDebug()
{
	if (!hDlg) return -1;
	return (int)SendDlgItemMessageA(hDlg, IDC_DBG_SCENEDBG, CB_GETCURSEL, 0, 0);
}

// =============================================================================================
//
int GetSelectedEnvMap()
{
	if (!hDlg) return 0;
	return (int)SendDlgItemMessageA(hDlg, IDC_DBG_ENVMAP, CB_GETCURSEL, 0, 0);
}

// =============================================================================================
//
void Release()
{
	vObj=NULL;
	hDlg=NULL;
	if (dwCmd) oapiUnregisterCustomCmd(dwCmd);
	dwCmd = NULL;
}

// =============================================================================================
//
void UpdateFlags()
{
	SETFLAG(debugFlags, DBG_FLAGS_SELGRPONLY,	(SendDlgItemMessageA(hDlg, IDC_DBG_GRPO, BM_GETCHECK, 0, 0)==BST_CHECKED));
	SETFLAG(debugFlags, DBG_FLAGS_SELMSHONLY,	(SendDlgItemMessageA(hDlg, IDC_DBG_MSHO, BM_GETCHECK, 0, 0)==BST_CHECKED));
	SETFLAG(debugFlags, DBG_FLAGS_TILEBOXES,	(SendDlgItemMessageA(hDlg, IDC_DBG_TILEBB, BM_GETCHECK, 0, 0)==BST_CHECKED));
	SETFLAG(debugFlags, DBG_FLAGS_BOXES,		(SendDlgItemMessageA(hDlg, IDC_DBG_BOXES, BM_GETCHECK, 0, 0)==BST_CHECKED));
	SETFLAG(debugFlags, DBG_FLAGS_SPHERES,		(SendDlgItemMessageA(hDlg, IDC_DBG_SPHERES, BM_GETCHECK, 0, 0)==BST_CHECKED));
	SETFLAG(debugFlags, DBG_FLAGS_HLMESH,		(SendDlgItemMessageA(hDlg, IDC_DBG_HSM, BM_GETCHECK, 0, 0)==BST_CHECKED));
	SETFLAG(debugFlags, DBG_FLAGS_HLGROUP,		(SendDlgItemMessageA(hDlg, IDC_DBG_HSG, BM_GETCHECK, 0, 0)==BST_CHECKED));
	SETFLAG(debugFlags, DBG_FLAGS_SELVISONLY,	(SendDlgItemMessageA(hDlg, IDC_DBG_VISO, BM_GETCHECK, 0, 0)==BST_CHECKED));
	SETFLAG(debugFlags, DBG_FLAGS_AMBIENT,	    (SendDlgItemMessageA(hDlg, IDC_DBG_AMBIENT, BM_GETCHECK, 0, 0)==BST_CHECKED));
	SETFLAG(debugFlags, DBG_FLAGS_WIREFRAME,	(SendDlgItemMessageA(hDlg, IDC_DBG_WIRE, BM_GETCHECK, 0, 0)==BST_CHECKED));
	SETFLAG(debugFlags, DBG_FLAGS_DUALSIDED,	(SendDlgItemMessageA(hDlg, IDC_DBG_DUAL, BM_GETCHECK, 0, 0)==BST_CHECKED));
	SETFLAG(debugFlags, DBG_FLAGS_PICK,			(SendDlgItemMessageA(hDlg, IDC_DBG_PICK, BM_GETCHECK, 0, 0)==BST_CHECKED));
	SETFLAG(debugFlags, DBG_FLAGS_FPSLIM,		(SendDlgItemMessageA(hDlg, IDC_DBG_FPSLIM, BM_GETCHECK, 0, 0)==BST_CHECKED));

	Config->EnableLimiter = (int)((debugFlags&DBG_FLAGS_FPSLIM)>0);
}

// =============================================================================================
//
void SetGroupHighlight(bool bStat)
{
	SETFLAG(debugFlags, DBG_FLAGS_HLGROUP, bStat);
}


inline _Variable DefVar(float min, float max, bool bLog, const char *tip, bool bGamma=false)
{
	_Variable var;
	var.bUsed = true;
	var.bLog = bLog;
	var.bGamma = bGamma;
	var.max = max;
	var.min = min;
	strncpy_s(var.tip, 80, tip, 80);
	return var;
}


// =============================================================================================
//
void OpenDlgClbk(void *context)
{
	DWORD idx = 0;
	HWND l_hDlg = oapiOpenDialog(g_hInst, IDD_D3D9MESHDEBUG, WndProc);

	if (l_hDlg) hDlg = l_hDlg; // otherwise open already
	else return;

	RECT rect;
	GetWindowRect(hDlg, &rect);
	SetWindowPos(hDlg, NULL, rect.left, rect.top, 298, rect.bottom - rect.top, SWP_SHOWWINDOW);
	origwidth = rect.right - rect.left;

	SendDlgItemMessage(hDlg, IDC_DBG_FPSLIM, BM_SETCHECK, Config->EnableLimiter==1, 0);

	SendDlgItemMessageA(hDlg, IDC_DBG_DISPLAY, CB_RESETCONTENT, 0, 0);
	SendDlgItemMessageA(hDlg, IDC_DBG_DISPLAY, CB_ADDSTRING, 0, (LPARAM)"Everything");
	SendDlgItemMessageA(hDlg, IDC_DBG_DISPLAY, CB_ADDSTRING, 0, (LPARAM)"Selected Visual");
	SendDlgItemMessageA(hDlg, IDC_DBG_DISPLAY, CB_ADDSTRING, 0, (LPARAM)"Selected Mesh");
	SendDlgItemMessageA(hDlg, IDC_DBG_DISPLAY, CB_ADDSTRING, 0, (LPARAM)"Selected Group");
	SendDlgItemMessageA(hDlg, IDC_DBG_DISPLAY, CB_SETCURSEL, 0, 0);

	SendDlgItemMessageA(hDlg, IDC_DBG_CAMERA, CB_RESETCONTENT, 0, 0);
	SendDlgItemMessageA(hDlg, IDC_DBG_CAMERA, CB_ADDSTRING, 0, (LPARAM)"Center on visual");
	SendDlgItemMessageA(hDlg, IDC_DBG_CAMERA, CB_ADDSTRING, 0, (LPARAM)"Wheel Fly/Pan Cam");
	SendDlgItemMessageA(hDlg, IDC_DBG_CAMERA, CB_SETCURSEL, 0, 0);

	SendDlgItemMessageA(hDlg, IDC_DBG_MATPRP, CB_RESETCONTENT, 0, 0);
	SendDlgItemMessageA(hDlg, IDC_DBG_MATPRP, CB_ADDSTRING, 0, (LPARAM)"Diffuse");		// 0
	SendDlgItemMessageA(hDlg, IDC_DBG_MATPRP, CB_ADDSTRING, 0, (LPARAM)"Ambient");		// 1
	SendDlgItemMessageA(hDlg, IDC_DBG_MATPRP, CB_ADDSTRING, 0, (LPARAM)"Specular");		// 2
	SendDlgItemMessageA(hDlg, IDC_DBG_MATPRP, CB_ADDSTRING, 0, (LPARAM)"Emission");		// 3
	SendDlgItemMessageA(hDlg, IDC_DBG_MATPRP, CB_ADDSTRING, 0, (LPARAM)"Reflect");		// 4
	SendDlgItemMessageA(hDlg, IDC_DBG_MATPRP, CB_ADDSTRING, 0, (LPARAM)"Roughness");	// 5
	SendDlgItemMessageA(hDlg, IDC_DBG_MATPRP, CB_ADDSTRING, 0, (LPARAM)"Fresnel");		// 6
	SendDlgItemMessageA(hDlg, IDC_DBG_MATPRP, CB_ADDSTRING, 0, (LPARAM)"Emission2");	// 7
	SendDlgItemMessageA(hDlg, IDC_DBG_MATPRP, CB_ADDSTRING, 0, (LPARAM)"---------");		// 8
	SendDlgItemMessageA(hDlg, IDC_DBG_MATPRP, CB_ADDSTRING, 0, (LPARAM)"Tune Albedo");	// 9
	SendDlgItemMessageA(hDlg, IDC_DBG_MATPRP, CB_ADDSTRING, 0, (LPARAM)"Tune _Emis");	// 10
	SendDlgItemMessageA(hDlg, IDC_DBG_MATPRP, CB_ADDSTRING, 0, (LPARAM)"Tune _Refl");	// 11
	SendDlgItemMessageA(hDlg, IDC_DBG_MATPRP, CB_ADDSTRING, 0, (LPARAM)"Tune _Rghn");	// 12
	SendDlgItemMessageA(hDlg, IDC_DBG_MATPRP, CB_ADDSTRING, 0, (LPARAM)"Tune _Transl");	// 13
	SendDlgItemMessageA(hDlg, IDC_DBG_MATPRP, CB_ADDSTRING, 0, (LPARAM)"Tune _Transm");	// 14
	SendDlgItemMessageA(hDlg, IDC_DBG_MATPRP, CB_ADDSTRING, 0, (LPARAM)"Tune _Spec");	// 15
	SendDlgItemMessageA(hDlg, IDC_DBG_MATPRP, CB_ADDSTRING, 0, (LPARAM)"Tune _Frsl");	// 16
	SendDlgItemMessageA(hDlg, IDC_DBG_MATPRP, CB_SETCURSEL, 0, 0);

	SendDlgItemMessageA(hDlg, IDC_DBG_SCENEDBG, CB_RESETCONTENT, 0, 0);
	SendDlgItemMessageA(hDlg, IDC_DBG_SCENEDBG, CB_ADDSTRING, 0, (LPARAM)"None");
	SendDlgItemMessageA(hDlg, IDC_DBG_SCENEDBG, CB_ADDSTRING, 0, (LPARAM)"Normals Global");
	SendDlgItemMessageA(hDlg, IDC_DBG_SCENEDBG, CB_ADDSTRING, 0, (LPARAM)"Normals Tangent");
	SendDlgItemMessageA(hDlg, IDC_DBG_SCENEDBG, CB_ADDSTRING, 0, (LPARAM)"Height");
	SendDlgItemMessageA(hDlg, IDC_DBG_SCENEDBG, CB_ADDSTRING, 0, (LPARAM)"Height Mk2");
	SendDlgItemMessageA(hDlg, IDC_DBG_SCENEDBG, CB_ADDSTRING, 0, (LPARAM)"Tile Level");
	SendDlgItemMessageA(hDlg, IDC_DBG_SCENEDBG, CB_SETCURSEL, 0, 0);

	SendDlgItemMessageA(hDlg, IDC_DBG_ACTION, CB_RESETCONTENT, 0, 0);
	SendDlgItemMessageA(hDlg, IDC_DBG_ACTION, CB_ADDSTRING, 0, (LPARAM)"Convert to DXT5");
	SendDlgItemMessageA(hDlg, IDC_DBG_ACTION, CB_ADDSTRING, 0, (LPARAM)"Convert to RGB8");
	SendDlgItemMessageA(hDlg, IDC_DBG_ACTION, CB_ADDSTRING, 0, (LPARAM)"Convert to RGB4");
	SendDlgItemMessageA(hDlg, IDC_DBG_ACTION, CB_SETCURSEL, 0, 0);

	SendDlgItemMessageA(hDlg, IDC_DBG_TARGET, CB_RESETCONTENT, 0, 0);
	SendDlgItemMessageA(hDlg, IDC_DBG_TARGET, CB_ADDSTRING, 0, (LPARAM)"Save");
	SendDlgItemMessageA(hDlg, IDC_DBG_TARGET, CB_ADDSTRING, 0, (LPARAM)"Assign to slot 0");
	SendDlgItemMessageA(hDlg, IDC_DBG_TARGET, CB_ADDSTRING, 0, (LPARAM)"Assign to slot 1");
	SendDlgItemMessageA(hDlg, IDC_DBG_TARGET, CB_ADDSTRING, 0, (LPARAM)"Assign to slot 2");
	SendDlgItemMessageA(hDlg, IDC_DBG_TARGET, CB_SETCURSEL, 0, 0);

	SendDlgItemMessageA(hDlg, IDC_DBG_ENVMAP, CB_RESETCONTENT, 0, 0);
	SendDlgItemMessageA(hDlg, IDC_DBG_ENVMAP, CB_ADDSTRING, 0, (LPARAM)"None");
	SendDlgItemMessageA(hDlg, IDC_DBG_ENVMAP, CB_ADDSTRING, 0, (LPARAM)"Mirror");
	SendDlgItemMessageA(hDlg, IDC_DBG_ENVMAP, CB_ADDSTRING, 0, (LPARAM)"Blur 1");
	SendDlgItemMessageA(hDlg, IDC_DBG_ENVMAP, CB_ADDSTRING, 0, (LPARAM)"Blur 2");
	SendDlgItemMessageA(hDlg, IDC_DBG_ENVMAP, CB_ADDSTRING, 0, (LPARAM)"Blur 3");
	SendDlgItemMessageA(hDlg, IDC_DBG_ENVMAP, CB_ADDSTRING, 0, (LPARAM)"Blur 4");
	SendDlgItemMessageA(hDlg, IDC_DBG_ENVMAP, CB_SETCURSEL, 0, 0);


	SetWindowText(GetDlgItem(hDlg, IDC_DBG_VARA), "1.3");
	SetWindowText(GetDlgItem(hDlg, IDC_DBG_VARB), "0.01");
	SetWindowText(GetDlgItem(hDlg, IDC_DBG_VARC), "0.00");

	// Speed slider
	SendDlgItemMessage(hDlg, IDC_DBG_RESBIAS, TBM_SETRANGEMAX, 1,  10);
	SendDlgItemMessage(hDlg, IDC_DBG_RESBIAS, TBM_SETRANGEMIN, 1, -10);
	SendDlgItemMessage(hDlg, IDC_DBG_RESBIAS, TBM_SETTICFREQ, 1, 0);
	SendDlgItemMessage(hDlg, IDC_DBG_RESBIAS, TBM_SETPOS, 1, int((resbias-4.0)*5.0));

	// Speed slider
	SendDlgItemMessage(hDlg, IDC_DBG_SPEED, TBM_SETRANGEMAX, 1, 200);
	SendDlgItemMessage(hDlg, IDC_DBG_SPEED, TBM_SETRANGEMIN, 1, 1);
	SendDlgItemMessage(hDlg, IDC_DBG_SPEED, TBM_SETTICFREQ,  1, 0);
	SendDlgItemMessage(hDlg, IDC_DBG_SPEED, TBM_SETPOS,  1, 75);
	SetWindowTextA(GetDlgItem(hDlg, IDC_DBG_SPEEDDSP), "29");

	// Meterial slider
	SendDlgItemMessage(hDlg, IDC_DBG_MATADJ, TBM_SETRANGEMAX, 1, 255);
	SendDlgItemMessage(hDlg, IDC_DBG_MATADJ, TBM_SETRANGEMIN, 1, 0);
	SendDlgItemMessage(hDlg, IDC_DBG_MATADJ, TBM_SETTICFREQ,  1, 0);
	SendDlgItemMessage(hDlg, IDC_DBG_MATADJ, TBM_SETPOS,  1, 0);
	
	camMode = 0;
	dspMode = 0;

	OBJHANDLE hTgt = oapiCameraTarget();

	SetVisual(g_client->GetScene()->GetVisObject(hTgt));	// This will call SetupMeshGroups()

	UpdateFlags();

	CreateToolTip(IDC_DBG_TARGET, hDlg, "Select a target where the resulting image is assigned");
	CreateToolTip(IDC_DBG_SEAMS, hDlg, "Enable seams reduction at each mipmap level");
	CreateToolTip(IDC_DBG_FADE, hDlg, "Enable mipmap post processing. Contrast and detail is reduced from each mipmap to prevent 'stripes' (See:Fa,Fb)");
	CreateToolTip(IDC_DBG_NORM, hDlg, "Center color channels at 0.5f to prevent lightening/darkening the results");
	CreateToolTip(IDC_DBG_VARA, hDlg, "Attennuates high contrast components. Leaves low contrast parts unchanged [1.0 to 1.6]");
	CreateToolTip(IDC_DBG_VARB, hDlg, "Attennuates everything equally. Typical range [0.00 to 0.03]");
	CreateToolTip(IDC_DBG_VARC, hDlg, "Apply noise to main level and all mipmaps before attennuation (Fa,Fb)");
	CreateToolTip(IDC_DBG_MORE, hDlg, "Click to show/hide more options");

	CreateToolTip(IDC_DBG_LINK, hDlg, "Adjust all color channels at the same time");
	CreateToolTip(IDC_DBG_DEFINED, hDlg, "Use the material property for rendering and save it");

	hTipRed = CreateToolTip(IDC_DBG_RED, hDlg, "Red");
	hTipGrn = CreateToolTip(IDC_DBG_GREEN, hDlg, "Green");
	hTipBlu = CreateToolTip(IDC_DBG_BLUE, hDlg, "Blue");
	hTipAlp = CreateToolTip(IDC_DBG_ALPHA, hDlg, "Alpha");

	// Diffuse
	Params[0].var[0] = DefVar(0, 1, false, "Red");
	Params[0].var[1] = DefVar(0, 1, false, "Green");
	Params[0].var[2] = DefVar(0, 1, false, "Blue");
	Params[0].var[3] = DefVar(0, 1, false, "Alpha");

	// Ambient
	Params[1].var[0] = DefVar(0, 1, false, "Red");
	Params[1].var[1] = DefVar(0, 1, false, "Green");
	Params[1].var[2] = DefVar(0, 1, false, "Blue");

	// Specular
	Params[2].var[0] = DefVar(0, 1, true, "Red");
	Params[2].var[1] = DefVar(0, 1, true, "Green");
	Params[2].var[2] = DefVar(0, 1, true, "Blue");
	Params[2].var[3] = DefVar(1, 4096.0f, true, "Specular power");

	// Emission
	Params[3].var[0] = DefVar(0, 1, false, "Red");
	Params[3].var[1] = DefVar(0, 1, false, "Green");
	Params[3].var[2] = DefVar(0, 1, false, "Blue");

	// Reflectivity
	Params[4].var[0] = DefVar(0, 1, false, "Red");
	Params[4].var[1] = DefVar(0, 1, false, "Green");
	Params[4].var[2] = DefVar(0, 1, false, "Blue");

	// Roughness
	Params[5].var[0] = DefVar(0.1f, 1, false, "Roughness");

	// Fresnel
	Params[6].var[0] = DefVar(1, 6, false, "Angle dependency");
	Params[6].var[1] = DefVar(0, 1, false, "Maximum intensity");
	Params[6].var[2] = DefVar(10.0f, 4096.0f, true, "Specular lobe size");
	
	// Emission2
	Params[7].var[0] = DefVar(0, 3, false, "Red");
	Params[7].var[1] = DefVar(0, 3, false, "Green");
	Params[7].var[2] = DefVar(0, 3, false, "Blue");

	// Unused index 8
	
	// Tuning -------------------------------------------------------------------------------------
	// Albedo
	Params[9].var[0] = DefVar(0.2f, 5.0f, true, "Red");
	Params[9].var[1] = DefVar(0.2f, 5.0f, true, "Green");
	Params[9].var[2] = DefVar(0.2f, 5.0f, true, "Blue");
	Params[9].var[3] = DefVar(0.2f, 5.0f, true, "Gamma", true);

	Params[10] = Params[9];	 // Emis
	Params[10].var[3] = DefVar(0.2f, 5.0f, true, "Gamma", true);

	Params[11] = Params[9];	 // Refl
	Params[11].var[3] = DefVar(0.2f, 5.0f, true, "Gamma", true);

	Params[12] = Params[9];  // Regn
	Params[12].var[3] = DefVar(0.2f, 5.0f, true, "Gamma", true);

	Params[13] = Params[9];  // Transl
	Params[13].var[3] = DefVar(0.2f, 5.0f, true, "???");

	Params[14] = Params[9];  // Transm
	Params[14].var[3] = DefVar(0.2f, 5.0f, true, "???");

	Params[15] = Params[9];	 // Spec
	Params[15].var[3] = DefVar(0.1f, 9.9f, true, "Power", false);

	Params[16] = Params[9];  // Frsl
	Params[16].var[3] = DefVar(0.2f, 5.0f, true, "Gamma", true);
}


// =============================================================================================
//
void SetTuningValue(int idx, D3DCOLORVALUE *pClr, DWORD clr, float value)
{
	float mi = Params[idx].var[clr].min;
	float mx = Params[idx].var[clr].max;

	switch (clr) {
		case 0: pClr->r = CLAMP(value, mi, mx); break;
		case 1: pClr->g = CLAMP(value, mi, mx); break;
		case 2: pClr->b = CLAMP(value, mi, mx); break;
		case 3: 
		{
			if (Params[idx].var[clr].bGamma) pClr->a = 1.0f / CLAMP(value, mi, mx);		
			else pClr->a = CLAMP(value, mi, mx);			
		} break;
	}
}

// =============================================================================================
//
float GetTuningValue(int idx, D3DCOLORVALUE *pClr, DWORD clr)
{
	switch (clr) {
		case 0: return pClr->r;
		case 1: return pClr->g;
		case 2: return pClr->b;
		case 3: 
		{
			if (Params[idx].var[clr].bGamma) return 1.0f / pClr->a;
			else return pClr->a;
		}
	}
	return 1.0f;
}

// =============================================================================================
//
float _Clamp(float value, DWORD p, DWORD v)
{
	return CLAMP(value, Params[p].var[v].min, Params[p].var[v].max);
}

// =============================================================================================
//
void UpdateMeshMaterial(float value, DWORD MatPrp, DWORD clr)
{
	OBJHANDLE hObj = vObj->GetObjectA();

	if (!oapiIsVessel(hObj)) return;

	D3D9Mesh *hMesh = (D3D9Mesh *)vObj->GetMesh(sMesh);

	if (!hMesh) return;

	DWORD matidx = hMesh->GetMeshGroupMaterialIdx(sGroup);
	DWORD texidx = hMesh->GetMeshGroupTextureIdx(sGroup);

	D3D9MatExt Mat;
	D3D9Tune Tune;

	if (!hMesh->GetMaterial(&Mat, matidx)) return;

	bool bTune = hMesh->GetTexTune(&Tune, texidx);

	switch(MatPrp) {

		case 0:	// Diffuse
		{
			Mat.ModFlags |= D3D9MATEX_DIFFUSE;
			Mat.Diffuse[clr] = _Clamp(value, MatPrp, clr);
			break;
		}

		case 1:	// Ambient
		{
			Mat.ModFlags |= D3D9MATEX_AMBIENT;
			Mat.Ambient[clr] = _Clamp(value, MatPrp, clr);
			break;
		}

		case 2:	// Specular
		{
			Mat.ModFlags |= D3D9MATEX_SPECULAR;
			Mat.Specular[clr] = _Clamp(value, MatPrp, clr);
			break;
		}

		case 3:	// Emission
		{
			Mat.ModFlags |= D3D9MATEX_EMISSIVE;
			Mat.Emissive[clr] = _Clamp(value, MatPrp, clr);
			break;
		}

		case 4:	// Reflectivity
		{
			Mat.ModFlags |= D3D9MATEX_REFLECT;
			Mat.Reflect[clr] = _Clamp(value, MatPrp, clr);
			break;
		}

		case 5:	// Roughness
		{
			Mat.ModFlags |= D3D9MATEX_ROUGHNESS;
			Mat.Roughness = _Clamp(value, MatPrp, 0);
			break;
		}

		case 6:	// Fresnel
		{
			Mat.ModFlags |= D3D9MATEX_FRESNEL;
			Mat.Fresnel[clr] = _Clamp(value, MatPrp, clr);
			break;
		}

		case 7:	// Emission2
		{
			Mat.ModFlags |= D3D9MATEX_EMISSION2;
			Mat.Emission2[clr] = _Clamp(value, MatPrp, clr);
			break;
		}

		case 9:		// Tune Albedo
		{
			SetTuningValue(MatPrp, &Tune.Albedo, clr, value);
			break;
		}

		case 10:	// Tune Emis
		{
			SetTuningValue(MatPrp, &Tune.Emis, clr, value);
			break;
		}

		case 11:	// Tune Refl
		{
			SetTuningValue(MatPrp, &Tune.Refl, clr, value);
			break;
		}

		case 12:	// Tune _Rghn
		{
			SetTuningValue(MatPrp, &Tune.Rghn, clr, value);
			break;
		}

		case 13:	// Tune _Transl
		{
			SetTuningValue(MatPrp, &Tune.Transl, clr, value);
			break;
		}

		case 14:	// Tune _Transm
		{
			SetTuningValue(MatPrp, &Tune.Transm, clr, value);
			break;
		}

		case 15:	// Tune _Spec
		{
			SetTuningValue(MatPrp, &Tune.Spec, clr, value);
			break;
		}

		case 16:	// Tune _Frsl
		{
			SetTuningValue(MatPrp, &Tune.Frsl, clr, value);
			break;
		}
	}

	if (bTune) hMesh->SetTexTune(&Tune, texidx);
	hMesh->SetMaterial(&Mat, matidx);
	vVessel *vVes = (vVessel *)vObj;

	vVes->GetMaterialManager()->RegisterMaterialChange(hMesh, matidx, &Mat); 
}


// =============================================================================================
//
DWORD GetModFlags(DWORD MatPrp)
{
	switch (MatPrp) {
		case 0:	return D3D9MATEX_DIFFUSE;
		case 1:	return D3D9MATEX_AMBIENT;
		case 2:	return D3D9MATEX_SPECULAR;
		case 3:	return D3D9MATEX_EMISSIVE;
		case 4:	return D3D9MATEX_REFLECT;
		case 5:	return D3D9MATEX_ROUGHNESS;
		case 6:	return D3D9MATEX_FRESNEL;
		case 7:	return D3D9MATEX_EMISSION2;
	}
	return 0;
}


// =============================================================================================
//
bool IsMaterialModified(DWORD MatPrp)
{
	OBJHANDLE hObj = vObj->GetObjectA();

	if (!oapiIsVessel(hObj)) return false;

	D3D9Mesh *hMesh = (D3D9Mesh *)vObj->GetMesh(sMesh);

	if (!hMesh) return false;

	DWORD matidx = hMesh->GetMeshGroupMaterialIdx(sGroup);
	
	D3D9MatExt Mat;

	if (!hMesh->GetMaterial(&Mat, matidx)) return false;

	return (Mat.ModFlags & GetModFlags(MatPrp)) != 0;
}


// =============================================================================================
//
void SetMaterialModified(DWORD MatPrp, bool bState)
{
	OBJHANDLE hObj = vObj->GetObjectA();

	if (!oapiIsVessel(hObj)) return;

	D3D9Mesh *hMesh = (D3D9Mesh *)vObj->GetMesh(sMesh);

	if (!hMesh) return;

	DWORD matidx = hMesh->GetMeshGroupMaterialIdx(sGroup);

	D3D9MatExt Mat;

	if (!hMesh->GetMaterial(&Mat, matidx)) return;

	if (bState) Mat.ModFlags |= GetModFlags(MatPrp);
	else		Mat.ModFlags &= (~GetModFlags(MatPrp));

	hMesh->SetMaterial(&Mat, matidx);
}


// =============================================================================================
//
float GetMaterialValue(DWORD MatPrp, DWORD clr)
{
	OBJHANDLE hObj = vObj->GetObjectA();

	if (!oapiIsVessel(hObj)) return 0.0f;

	D3D9Mesh *hMesh = (D3D9Mesh *)vObj->GetMesh(sMesh);

	if (!hMesh) return 0.0f;

	DWORD matidx = hMesh->GetMeshGroupMaterialIdx(sGroup);
	DWORD texidx = hMesh->GetMeshGroupTextureIdx(sGroup);

	const D3D9MatExt *pMat = hMesh->GetMaterial(matidx);
	
	if (!pMat) return 0.0f;

	D3D9Tune Tune;
	bool bTune = hMesh->GetTexTune(&Tune, texidx);

	switch(MatPrp) {

		case 0:	// Diffuse
		{
			switch(clr) {
				case 0: return pMat->Diffuse.x;
				case 1: return pMat->Diffuse.y;
				case 2: return pMat->Diffuse.z;
				case 3: return pMat->Diffuse.w;
			}
			break;
		}

		case 1:	// Ambient
		{
			switch(clr) {
				case 0: return pMat->Ambient.x;
				case 1: return pMat->Ambient.y;
				case 2: return pMat->Ambient.z;
			}
			break;
		}

		case 2:	// Specular
		{
			switch(clr) {
				case 0: return pMat->Specular.x;
				case 1: return pMat->Specular.y;
				case 2: return pMat->Specular.z;
				case 3: return pMat->Specular.w;
			}
			break;
		}

		case 3:	// Emission
		{
			switch(clr) {
				case 0: return pMat->Emissive.x;
				case 1: return pMat->Emissive.y;
				case 2: return pMat->Emissive.z;
			}
			break;
		}

		case 4:	// Reflectivity
		{
			switch(clr) {
				case 0: return pMat->Reflect.x;
				case 1: return pMat->Reflect.y;
				case 2: return pMat->Reflect.z;
			}
			break;
		}

		case 5:	// Roughness
		{
			switch (clr) {
				case 0: return pMat->Roughness;
			}
			break;
		}

		case 6:	// Fresnel
		{
			switch(clr) {
				case 0: return pMat->Fresnel.x;	// Angle
				case 1: return pMat->Fresnel.y; // Multiplier
				case 2: return pMat->Fresnel.z; // SpecPower
			}
			break;
		}

		case 7:	// Emission2
		{
			switch (clr) {
				case 0: return pMat->Emission2.x;
				case 1: return pMat->Emission2.y;
				case 2: return pMat->Emission2.z;
			}
			break;
		}

		case 9:	// Tune Albedo
		{
			return GetTuningValue(MatPrp, &Tune.Albedo, clr);
		}

		case 10:	// Tune Emis
		{
			return GetTuningValue(MatPrp, &Tune.Emis, clr);
		}

		case 11:	// Tune Refl
		{
			return GetTuningValue(MatPrp, &Tune.Refl, clr);
		}

		case 12:	// Tune _Rghn
		{
			return GetTuningValue(MatPrp, &Tune.Rghn, clr);
		}

		case 13:	// Tune _Transl
		{
			return GetTuningValue(MatPrp, &Tune.Transl, clr);
		}

		case 14:	// Tune _Transm
		{
			return GetTuningValue(MatPrp, &Tune.Transm, clr);
		}

		case 15:	// Tune _Spec
		{
			return GetTuningValue(MatPrp, &Tune.Spec, clr);
		}

		case 16:	// Tune _Frsl
		{
			return GetTuningValue(MatPrp, &Tune.Frsl, clr);
		}
	}

	return 0.0f;
}

// =============================================================================================
//
void SetColorSlider()
{
	DWORD MatPrp = SendDlgItemMessageA(hDlg, IDC_DBG_MATPRP, CB_GETCURSEL, 0, 0);

	float val = GetMaterialValue(MatPrp, SelColor);

	val -= Params[MatPrp].var[SelColor].min;
	val /= (Params[MatPrp].var[SelColor].max - Params[MatPrp].var[SelColor].min);

	if (Params[MatPrp].var[SelColor].bLog) val = sqrt(val);

	SendDlgItemMessage(hDlg, IDC_DBG_MATADJ, TBM_SETPOS,  1, WORD(val*255.0f));
}

// =============================================================================================
//
void DisplayMat(bool bRed, bool bGreen, bool bBlue, bool bAlpha)
{
	char lbl[32];

	DWORD MatPrp = SendDlgItemMessageA(hDlg, IDC_DBG_MATPRP, CB_GETCURSEL, 0, 0);

	float r = GetMaterialValue(MatPrp, 0);
	float g = GetMaterialValue(MatPrp, 1);
	float b = GetMaterialValue(MatPrp, 2);
	float a = GetMaterialValue(MatPrp, 3);

	if (bRed) sprintf_s(lbl,32,"%3.3f", r);
	else	  sprintf_s(lbl,32,"");
	SetWindowText(GetDlgItem(hDlg, IDC_DBG_RED),   lbl);
	
	if (bGreen) sprintf_s(lbl,32,"%3.3f", g);
	else		sprintf_s(lbl,32,"");
	SetWindowText(GetDlgItem(hDlg, IDC_DBG_GREEN), lbl);

	if (bBlue) sprintf_s(lbl,32,"%3.3f", b);
	else	   sprintf_s(lbl,32,"");
	SetWindowText(GetDlgItem(hDlg, IDC_DBG_BLUE),  lbl);

	if (bAlpha) sprintf_s(lbl,32,"%3.3f", a);
	else	    sprintf_s(lbl,32,"");
	SetWindowText(GetDlgItem(hDlg, IDC_DBG_ALPHA), lbl);

	if (bRed)   EnableWindow(GetDlgItem(hDlg, IDC_DBG_RED), true);
	else	    EnableWindow(GetDlgItem(hDlg, IDC_DBG_RED), false);	
	if (bGreen) EnableWindow(GetDlgItem(hDlg, IDC_DBG_GREEN), true);
	else		EnableWindow(GetDlgItem(hDlg, IDC_DBG_GREEN), false);	
	if (bBlue)  EnableWindow(GetDlgItem(hDlg, IDC_DBG_BLUE), true);
	else	    EnableWindow(GetDlgItem(hDlg, IDC_DBG_BLUE), false);	
	if (bAlpha) EnableWindow(GetDlgItem(hDlg, IDC_DBG_ALPHA), true);
	else		EnableWindow(GetDlgItem(hDlg, IDC_DBG_ALPHA), false);

	bool bModified = IsMaterialModified(MatPrp);

	SendDlgItemMessageA(hDlg, IDC_DBG_DEFINED, BM_SETCHECK, bModified, 0);
}

// =============================================================================================
//
void UpdateMaterialDisplay(bool bSetup)
{
	char lbl[256];
	char lbl2[64];

	OBJHANDLE hObj = vObj->GetObjectA();
	if (!oapiIsVessel(hObj)) return;
	
	D3D9Mesh *hMesh = (D3D9Mesh *)vObj->GetMesh(sMesh);
	if (!hMesh) return;

	DWORD matidx = hMesh->GetMeshGroupMaterialIdx(sGroup);

	// Set material info
	const char *skin = NULL;
	if (skin)	sprintf_s(lbl, 256, "Material %u: [Skin %s]", matidx, skin);
	else		sprintf_s(lbl, 256, "Material %u:", matidx);

	GetWindowText(GetDlgItem(hDlg, IDC_DBG_MATGRP), lbl2, 64);
	if (strcmp(lbl, lbl2)) SetWindowText(GetDlgItem(hDlg, IDC_DBG_MATGRP), lbl); // Avoid causing flashing

	if (bSetup) SelColor = 0;

	DWORD MatPrp = SendDlgItemMessageA(hDlg, IDC_DBG_MATPRP, CB_GETCURSEL, 0, 0);

	DisplayMat(Params[MatPrp].var[0].bUsed, Params[MatPrp].var[1].bUsed, Params[MatPrp].var[2].bUsed, Params[MatPrp].var[3].bUsed);

	SetToolTip(IDC_DBG_RED, hTipRed, Params[MatPrp].var[0].tip);
	SetToolTip(IDC_DBG_GREEN, hTipGrn, Params[MatPrp].var[1].tip);
	SetToolTip(IDC_DBG_BLUE, hTipBlu, Params[MatPrp].var[2].tip);
	SetToolTip(IDC_DBG_ALPHA, hTipAlp, Params[MatPrp].var[3].tip);

	DWORD texidx = hMesh->GetMeshGroupTextureIdx(sGroup);

	if (texidx==0) SetWindowText(GetDlgItem(hDlg, IDC_DBG_TEXTURE), "Texture: None");
	else {
		SURFHANDLE hSrf = hMesh->GetTexture(texidx);
		if (hSrf) {
			sprintf_s(lbl, 256, "Texture: %s [%u]", RemovePath(SURFACE(hSrf)->GetName()), texidx);
			SetWindowText(GetDlgItem(hDlg, IDC_DBG_TEXTURE), lbl);
		}
	}

	sprintf_s(lbl, 256, "Mesh: %s", RemovePath(hMesh->GetName()));
	SetWindowText(GetDlgItem(hDlg, IDC_DBG_MESHNAME), lbl);
}

// =============================================================================================
//
bool IsSelectedGroupRendered()
{
	if (!vObj) return false;
	D3D9Mesh *hMesh = (D3D9Mesh *)vObj->GetMesh(sMesh);
	if (hMesh) return hMesh->IsGroupRendered(sGroup);
	return false;
}

// =============================================================================================
//
void UpdateColorSlider(WORD pos)
{
	float val = float(pos)/255.0f;
	
	DWORD MatPrp = SendDlgItemMessageA(hDlg, IDC_DBG_MATPRP, CB_GETCURSEL, 0, 0);
	bool bLink = (SendDlgItemMessageA(hDlg, IDC_DBG_LINK, BM_GETCHECK, 0, 0)==BST_CHECKED);

	if (MatPrp==5 || MatPrp==6) bLink = false;	// Roughness, Fresnel
	if (SelColor==3) bLink = false;				// Alpha, Specular power

	if (Params[MatPrp].var[SelColor].bLog) val = (val*val);

	val *= (Params[MatPrp].var[SelColor].max - Params[MatPrp].var[SelColor].min);
	val += Params[MatPrp].var[SelColor].min;

	float old = GetMaterialValue(MatPrp, SelColor);
	float fct = val/old;
	
	if (old<1e-4) fct = val;

	if (bLink) {
		float r = GetMaterialValue(MatPrp, 0);
		float g = GetMaterialValue(MatPrp, 1);
		float b = GetMaterialValue(MatPrp, 2);
		if (r<1e-4) r=1.0f;
		if (g<1e-4) g=1.0f;
		if (b<1e-4) b=1.0f;
		UpdateMeshMaterial(r*fct, MatPrp, 0);
		UpdateMeshMaterial(g*fct, MatPrp, 1);
		UpdateMeshMaterial(b*fct, MatPrp, 2);
	}
	else UpdateMeshMaterial(val, MatPrp, SelColor);
}

// =============================================================================================
//
DWORD GetSelectedMesh()
{
	return sMesh;
}

// =============================================================================================
//
void SelectGroup(DWORD idx)
{
	if (idx<nGroup) {
		sGroup = idx;
		SetupMeshGroups();
	}
}

// =============================================================================================
//
void SelectMesh(D3D9Mesh *pMesh)
{
	for (DWORD i=0;i<nMesh;i++) {
		if (vObj->GetMesh(i)==pMesh) {
			sMesh = i;
			break;
		}
	}
	SetupMeshGroups();
}

// =============================================================================================
//
void SetupMeshGroups()
{
	char lbl[256];

	if (!vObj) return; 

	SetWindowText(GetDlgItem(hDlg, IDC_DBG_VISUAL), visual);

	if (nMesh!=0) {
		if (sMesh>0xFFFF) sMesh = nMesh-1;
		if (sMesh>=nMesh) sMesh = 0;
	}
	else {
		sMesh=0, sGroup=0, nGroup=0;
		SetWindowText(GetDlgItem(hDlg, IDC_DBG_MESH), "N/A");
		SetWindowText(GetDlgItem(hDlg, IDC_DBG_GROUP), "N/A");
		return;
	}

	sprintf_s(lbl,256,"%u/%u",sMesh,nMesh-1);
	SetWindowText(GetDlgItem(hDlg, IDC_DBG_MESH), lbl);

	D3D9Mesh *mesh = (class D3D9Mesh *)vObj->GetMesh(sMesh);

	if (mesh) nGroup = mesh->GetGroupCount();
	else	  nGroup = 0;

	if (nGroup!=0) {
		if (sGroup>0xFFFF)  sGroup = nGroup-1;
		if (sGroup>=nGroup) sGroup = 0;
	}
	else {
		sGroup=0;
		SetWindowText(GetDlgItem(hDlg, IDC_DBG_GROUP), "N/A");
		return;
	}

	sprintf_s(lbl,256,"%u/%u",sGroup,nGroup-1);
	SetWindowText(GetDlgItem(hDlg, IDC_DBG_GROUP), lbl);

	UpdateMaterialDisplay();
	SetColorSlider();
}

// =============================================================================================
//
double GetVisualSize()
{
	if (hDlg && vObj) {
		OBJHANDLE hObj = vObj->GetObjectA();
		if (hObj) return oapiGetSize(hObj);
	}
	return 1.0;
}

// =============================================================================================
//
vObject * GetVisual()
{
	return vObj;
}

// =============================================================================================
//
void SetVisual(vObject *vo)
{
	if (!hDlg) {
		vObj = NULL;	// Always set the visual to NULL if the dialog isn't open
		return;
	}
	vObj = vo;
	UpdateVisual();
}

// =============================================================================================
//
void UpdateVisual()
{
	if (!vObj || !hDlg) return; 
	nMesh = vObj->GetMeshCount();
	sprintf_s(visual, 64, "Visual: %s", vObj->GetName());
	SetupMeshGroups();
}

// =============================================================================================
//
void RemoveVisual(vObject *vo)
{
	if (vObj==vo) vObj=NULL;
}

// =============================================================================================
//
void SetColorValue(const char *lbl)
{
	DWORD MatPrp = SendDlgItemMessageA(hDlg, IDC_DBG_MATPRP, CB_GETCURSEL, 0, 0);
	UpdateMeshMaterial(float(atof(lbl)), MatPrp, SelColor);
	SetColorSlider();
}

// =============================================================================================
//
float reduce(float v, float q)
{
	if (v>0) return max(0, v-q);
	else     return min(0, v+q);
}

struct PCParam {
	DWORD Action;
	DWORD Func;
	DWORD Mip;
	float a,b,c;
};

// =============================================================================================
//
D3DXCOLOR ProcessColor(D3DXVECTOR4 &C, PCParam *prm, int x, int y)
{
	float fMip = float(prm->Mip);
	float a = prm->a;
	float b = prm->b;
	float c = prm->c;

	// Swap color channels
	C = D3DXVECTOR4(C.z, C.y, C.z, C.x);
	
	if (c>0.001) {
		D3DXVECTOR4 rnd((float)oapiRand(),(float)oapiRand(), (float)oapiRand(), (float)oapiRand());
		C += (rnd*2.0f-1.0f) * (c/(2.0f+fMip));
	}
	
	// Reduce contrast
	if (prm->Func & 0x2) {

		if (prm->Mip==0) return D3DXCOLOR(C.x, C.y, C.z, C.w);		// Do nothing for the main level

		C = C*2.0f - 1.0f;			// Expand to [-1, 1]
		C *= pow(a, -abs(C)*fMip);

		float k = b * fMip;

		C = D3DXVECTOR4(reduce(C.x, k), reduce(C.y, k), reduce(C.z, k), reduce(C.w, k));
		C = C*0.5f + 0.5f;			// Back to [0, 1]
	}

	return D3DXCOLOR(C.x, C.y, C.z, C.w);
}

// =============================================================================================
//
bool Execute(HWND hWnd, LPOPENFILENAME pOF)
{
	LPDIRECT3DDEVICE9 pDevice = g_client->GetDevice();

	SaveTex.hwndOwner = hWnd;

	D3DLOCKED_RECT in, out;
	PCParam prm;

	DWORD Func;
	DWORD Action = SendDlgItemMessageA(hDlg, IDC_DBG_ACTION, CB_GETCURSEL, 0, 0);
	DWORD Target = SendDlgItemMessageA(hDlg, IDC_DBG_TARGET, CB_GETCURSEL, 0, 0);

	if (SendDlgItemMessageA(hDlg, IDC_DBG_NORM, BM_GETCHECK, 0, 0)==BST_CHECKED) Func |= 0x1;
	if (SendDlgItemMessageA(hDlg, IDC_DBG_FADE, BM_GETCHECK, 0, 0)==BST_CHECKED) Func |= 0x2;
	if (SendDlgItemMessageA(hDlg, IDC_DBG_SEAMS, BM_GETCHECK, 0, 0)==BST_CHECKED) Func |= 0x4;
	
	if (Action>=0 && Action<=2) {

		LPDIRECT3DTEXTURE9 pTex = NULL;
		LPDIRECT3DTEXTURE9 pWork = NULL;
		LPDIRECT3DTEXTURE9 pSave = NULL;
		D3DXIMAGE_INFO info;

		HR(D3DXCreateTextureFromFileExA(pDevice, pOF->lpstrFile, D3DX_DEFAULT, D3DX_DEFAULT, 0, 0, D3DFMT_A8R8G8B8, D3DPOOL_SYSTEMMEM, D3DX_DEFAULT, D3DX_DEFAULT, 0, &info, NULL, &pTex));

		if (!pTex) {
			LogErr("Failed to open a file [%s]", pOF->lpstrFile); 
			return false;
		}

		DWORD mips = pTex->GetLevelCount();

		if (true) {

			HR(D3DXCreateTexture(pDevice, info.Width, info.Height, mips, 0, D3DFMT_A8R8G8B8, D3DPOOL_SYSTEMMEM, &pWork));
			if (!pWork) return false;

			if (Action==0) { HR(D3DXCreateTexture(pDevice, info.Width, info.Height, mips, 0, D3DFMT_DXT5, D3DPOOL_SYSTEMMEM, &pSave)); }
			if (Action==1) { HR(D3DXCreateTexture(pDevice, info.Width, info.Height, mips, 0, D3DFMT_A8R8G8B8, D3DPOOL_SYSTEMMEM, &pSave)); }
			if (Action==2) { HR(D3DXCreateTexture(pDevice, info.Width, info.Height, mips, 0, D3DFMT_A4R4G4B4, D3DPOOL_SYSTEMMEM, &pSave)); }

			if (!pSave) return false;

			
			// Process texture ----------------------------------------------
			//
			for (DWORD n=0;n<mips;n++) {

				D3DXCOLOR seam;

				DWORD w = info.Width>>n;
				DWORD h = info.Height>>n;
				HR(pTex->LockRect(n, &in, NULL, 0)); 
				HR(pWork->LockRect(n, &out, NULL, 0));
				DWORD *pIn  = (DWORD *)in.pBits;
				DWORD *pOut = (DWORD *)out.pBits;

				prm.Mip = n;
				prm.Action = Action;
				prm.Func = Func;
				prm.a = GetFloatFromBox(hDlg, IDC_DBG_VARA);
				prm.b = GetFloatFromBox(hDlg, IDC_DBG_VARB);
				prm.c = GetFloatFromBox(hDlg, IDC_DBG_VARC);
				
				for (DWORD y=0;y<h;y++) {
					for (DWORD x=0;x<w;x++) {

						D3DXCOLOR c(pIn[x + y*w]);
						DWORD r = w-1;
						DWORD b = h-1;
						
						if (Func&0x4) {
							seam = c;
							if (x==0) seam = D3DXCOLOR(pIn[r + y*w]);
							if (x==r) seam = D3DXCOLOR(pIn[0 + y*w]);
							if (y==0) seam = D3DXCOLOR(pIn[x + b*w]);
							if (y==b) seam = D3DXCOLOR(pIn[x + 0*w]);
							c = (c*2.0f + seam) * 0.33333f;
						}

						pOut[x + y*w] = ProcessColor(D3DXVECTOR4(c.r, c.g, c.b, c.a), &prm, x, y);
					}
				}

				// Balance fine correction
				if (Func&0x1) {
					D3DXCOLOR c = D3DXCOLOR(0,0,0,0);
					DWORD s = w*h;
					for (DWORD x=0;x<s;x++) c += D3DXCOLOR(pOut[x]);
					c *= 1.0f/float(w*h);
					c -= D3DXCOLOR(0.5f, 0.5f, 0.5f, 0.5f);
					for (DWORD x=0;x<s;x++) {
						D3DXCOLOR q = D3DXCOLOR(pOut[x]);
						pOut[x] = D3DXCOLOR(q.r-c.r, q.g, q.b-c.b, q.a);
					}
				}


				HR(pTex->UnlockRect(n));
				HR(pWork->UnlockRect(n));
			}

			SAFE_RELEASE(pTex);

			// Convert texture format --------------------------------------
			//
			for (DWORD n=0;n<mips;n++) {
				LPDIRECT3DSURFACE9 pIn, pOut;
				pWork->GetSurfaceLevel(n, &pIn);
				pSave->GetSurfaceLevel(n, &pOut);
				if (D3DXLoadSurfaceFromSurface(pOut, NULL, NULL, pIn, NULL, NULL, D3DX_FILTER_LINEAR, 0)!=S_OK) {
					LogErr("D3DXLoadSurfaceFromSurface Failed");
					return false;
				}
				pIn->Release();
				pOut->Release();
			}
			SAFE_RELEASE(pWork);
		}
		else {
			// Copy input to output	
			pSave = pTex;
		}

		// Save the texture into a file -------------------------------
		//
		if (Target==0) {
			strcpy_s(SaveTex.lpstrFile, 255, OpenTex.lpstrFile);
			if (GetSaveFileName(&SaveTex)) {
				if (D3DXSaveTextureToFileA(SaveTex.lpstrFile, D3DXIFF_DDS, pSave, NULL)!=S_OK) {
					LogErr("Failed to create a file [%s]",SaveTex.lpstrFile); 
					return false;
				}
				SAFE_RELEASE(pSave);
				return true;
			}
		}

		// Assign the texture for rendering -----------------------------
		//
		if (Target==1 || Target==2 || Target==3) {
			vPlanet *vP = g_client->GetScene()->GetCameraProxyVisual();	
			if (vP) vP->SetMicroTexture(pSave, Target-1);
			SAFE_RELEASE(pSave);
			return true;
		}
	}

	return false;
}
			

// =============================================================================================
//
void SaveEnvMap()
{
	LPDIRECT3DDEVICE9 pDevice = g_client->GetDevice();

	OBJHANDLE hObj = vObj->Object();

	if (oapiIsVessel(hObj)) {
		vVessel *vVes = (vVessel *)vObj;
		LPDIRECT3DCUBETEXTURE9 pTex = vVes->GetEnvMap(ENVMAP_MIRROR);
		if (D3DXSaveTextureToFileA("EnvMap.dds", D3DXIFF_DDS, pTex, NULL) != S_OK) {
			LogErr("Failed to save envmap");
		}
	}
}

//-------------------------------------------------------------------------------------------
//
void Append(const char *format, ...)
{
	if (hDataWnd == NULL) return;
	char buf[256];
	va_list args;
	va_start(args, format);
	_vsnprintf_s(buf, 256, 256, format, args);
	va_end(args);
	buffer += buf;
}

//-------------------------------------------------------------------------------------------
//
void Refresh()
{
	if (hDataWnd == NULL) return;
	SetWindowTextA(GetDlgItem(hDataWnd, IDC_DBG_DATAVIEW), buffer.c_str());
	buffer.clear();
}


// ==============================================================
// Dialog message handler

BOOL CALLBACK ViewProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	static bool isOpen = false; // IDC_DBG_MORE (full or reduced width)

	DWORD Prp = SendDlgItemMessageA(hDlg, IDC_DBG_MATPRP, CB_GETCURSEL, 0, 0);

	switch (uMsg) {

	case WM_INITDIALOG:
	{
		SetWindowTextA(GetDlgItem(hWnd, IDC_DBG_DATAVIEW), "-- Select a mesh group --");
		buffer.clear();
		return TRUE;	// All Init actions are done in OpenDlgClbk();
	}

	case WM_COMMAND:

		switch (LOWORD(wParam)) {

		case IDCANCEL:
			oapiCloseDialog(hWnd);
			if (!buffer.empty()) buffer.clear();
			hDataWnd = NULL;
			return TRUE;
		}
		break;
	}

	return oapiDefDialogProc(hWnd, uMsg, wParam, lParam);
}


// ==============================================================
// Dialog message handler

BOOL CALLBACK WndProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	char lbl[32];
	RECT rect;
	bool bPaused;
	static bool isOpen = false; // IDC_DBG_MORE (full or reduced width)

	OpenTex.hwndOwner = hWnd;

	DWORD Prp = SendDlgItemMessageA(hDlg, IDC_DBG_MATPRP, CB_GETCURSEL, 0, 0);

	switch (uMsg) {

	case WM_INITDIALOG:
	{
		isOpen = false; // We always start with reduced width
		return TRUE;	// All Init actions are done in OpenDlgClbk();
	}

	case WM_HSCROLL:
	{
		if (LOWORD(wParam)==TB_THUMBTRACK || LOWORD(wParam)==TB_ENDTRACK) {
			WORD pos = HIWORD(wParam);

			if (HWND(lParam)==GetDlgItem(hWnd, IDC_DBG_SPEED)) {
				if (pos==0) pos = WORD(SendDlgItemMessage(hDlg, IDC_DBG_SPEED, TBM_GETPOS,  0, 0));
				double fpos = pow(2.0,double(pos)*13.0/200.0); 
				sprintf_s(lbl,32,"%1.0f",fpos);
				SetWindowTextA(GetDlgItem(hWnd, IDC_DBG_SPEEDDSP), lbl);
				camSpeed = fpos/50.0;
			}

			if (HWND(lParam) == GetDlgItem(hWnd, IDC_DBG_RESBIAS)) {
				resbias = 4.0 + 0.2 * double(SendDlgItemMessage(hDlg, IDC_DBG_RESBIAS, TBM_GETPOS, 0, 0));
			}

			if (HWND(lParam)==GetDlgItem(hWnd, IDC_DBG_MATADJ)) {
				if (pos==0) pos = WORD(SendDlgItemMessage(hDlg, IDC_DBG_MATADJ, TBM_GETPOS,  0, 0));
				UpdateColorSlider(pos);
				UpdateMaterialDisplay();
			}
		}
		return false;
	}

	case WM_COMMAND:

		switch (LOWORD(wParam)) {

			case IDCANCEL:
				Close();
				break;
		
			case IDC_DBG_MATSAVE:
			{
				OBJHANDLE hObj = vObj->GetObjectA();
				if (oapiIsVessel(hObj)) {
					vVessel *vVes = (vVessel *)vObj;
					vVes->GetMaterialManager()->SaveConfiguration();
				}
				break;
			}

			case IDC_DBG_DATAWND:
			{
				HWND hW = oapiOpenDialog(g_hInst, IDD_DEBUGVIEW, ViewProc);
				if (hW) hDataWnd = hW;
			}
				break;

			case IDC_DBG_LINK:
				break;

			case IDC_DBG_DEFINED:
				SetMaterialModified(Prp, (SendDlgItemMessageA(hDlg, IDC_DBG_DEFINED, BM_GETCHECK, 0, 0) == BST_CHECKED));
				break;

			case IDC_DBG_COPY:
			{
				cpr = GetMaterialValue(Prp, 0);
				cpg = GetMaterialValue(Prp, 1);
				cpb = GetMaterialValue(Prp, 2);
				break;
			}

			case IDC_DBG_PASTE:
			{
				UpdateMeshMaterial(cpr, Prp, 0);
				UpdateMeshMaterial(cpg, Prp, 1);
				UpdateMeshMaterial(cpb, Prp, 2);
				UpdateMaterialDisplay();
				SetColorSlider();
				break;
			}

			case IDC_DBG_RED:
				if (HIWORD(wParam)==EN_SETFOCUS) {
					SelColor = 0;
					UpdateMaterialDisplay();
					SetColorSlider();
				}
				if (HIWORD(wParam)==EN_KILLFOCUS) {
					GetWindowTextA(HWND(lParam), lbl, 32);
					SetColorValue(lbl);
				}
				break;

			case IDC_DBG_GREEN:
				if (HIWORD(wParam)==EN_SETFOCUS) {
					SelColor = 1;
					UpdateMaterialDisplay();
					SetColorSlider();
				}
				if (HIWORD(wParam)==EN_KILLFOCUS) {
					GetWindowTextA(HWND(lParam), lbl, 32);
					SetColorValue(lbl);
				}
				break;

			case IDC_DBG_BLUE:
				if (HIWORD(wParam)==EN_SETFOCUS) {
					SelColor = 2;
					UpdateMaterialDisplay();
					SetColorSlider();
				}
				if (HIWORD(wParam)==EN_KILLFOCUS) {
					GetWindowTextA(HWND(lParam), lbl, 32);
					SetColorValue(lbl);
				}
				break;

			case IDC_DBG_ALPHA:
				if (HIWORD(wParam)==EN_SETFOCUS) {
					SelColor = 3;
					UpdateMaterialDisplay();
					SetColorSlider();
				}
				if (HIWORD(wParam)==EN_KILLFOCUS) {
					GetWindowTextA(HWND(lParam), lbl, 32);
					SetColorValue(lbl);
				}
				break;

			case IDC_DBG_MATPRP:
				if (HIWORD(wParam)==CBN_SELCHANGE) {
					UpdateMaterialDisplay(true);
					SetColorSlider();	
				}
				break;

			case IDC_DBG_DISPLAY:
				if (HIWORD(wParam)==CBN_SELCHANGE) dspMode = SendDlgItemMessage(hWnd, IDC_DBG_DISPLAY, CB_GETCURSEL, 0, 0);
				break;

			case IDC_DBG_CAMERA:
				if (HIWORD(wParam)==CBN_SELCHANGE) camMode = SendDlgItemMessage(hWnd, IDC_DBG_CAMERA, CB_GETCURSEL, 0, 0);
				break;

			case IDC_DBG_MSHUP: 
				if (HIWORD(wParam)==BN_CLICKED) sMesh--;
				SetupMeshGroups();
				break;

			case IDC_DBG_MSHDN: 
				if (HIWORD(wParam)==BN_CLICKED) sMesh++;
				SetupMeshGroups();
				break;

			case IDC_DBG_GRPUP: 
				if (HIWORD(wParam)==BN_CLICKED) sGroup--;
				SetupMeshGroups();
				break;

			case IDC_DBG_GRPDN: 
				if (HIWORD(wParam)==BN_CLICKED) sGroup++;	
				SetupMeshGroups();
				break;

			
			case IDC_DBG_MESH: 
				if (HIWORD(wParam)==EN_KILLFOCUS) {
					char cbuf[32];
					GetWindowText(GetDlgItem(hWnd, IDC_DBG_MESH),  cbuf, 32); 
					for (int i=0; i<32;i++) if (cbuf[i]==0 || cbuf[i]=='/') { cbuf[i]=0; break; }
					sMesh = atoi(cbuf);
					SetupMeshGroups();
				}
				break;

			case IDC_DBG_GROUP: 
				if (HIWORD(wParam)==EN_KILLFOCUS) {
					char cbuf[32];
					GetWindowText(GetDlgItem(hWnd, IDC_DBG_GROUP),  cbuf, 32);
					for (int i=0; i<32;i++) if (cbuf[i]==0 || cbuf[i]=='/') { cbuf[i]=0; break; }
					sGroup = atoi(cbuf);
					SetupMeshGroups();
				}
				break;

			case IDC_DBG_OPEN:
				bPaused = oapiGetPause();
				oapiSetPause(true);
				if (GetOpenFileNameA(&OpenTex)) {
					SetWindowText(GetDlgItem(hWnd, IDC_DBG_FILE), OpenTex.lpstrFile);
				}
				oapiSetPause(bPaused);
				break;

			case IDC_DBG_EXECUTE:
				bPaused = oapiGetPause();
				oapiSetPause(true);
				if (Execute(hWnd, &OpenTex)==false) MessageBox(hWnd,"Failed :(","D3D9 Controls", MB_OK);
				oapiSetPause(bPaused);
				break;

			case IDC_DBG_ACTION:
				break;

			case IDC_DBG_MORE:
				GetWindowRect(hDlg, &rect);
				SetWindowPos(hDlg, NULL, rect.left, rect.top, isOpen ?   298 : origwidth, rect.bottom - rect.top, SWP_SHOWWINDOW);
				SetWindowText(GetDlgItem(hWnd, IDC_DBG_MORE), isOpen ? ">>>" : "<<<");
				isOpen = !isOpen;
				break;

			case IDC_DBG_ENVSAVE:
				SaveEnvMap();
				break;

			case IDC_DBG_GRPO:
			case IDC_DBG_VISO:
			case IDC_DBG_MSHO:
			case IDC_DBG_BOXES:
			case IDC_DBG_SPHERES:
			case IDC_DBG_HSM:
			case IDC_DBG_HSG:
			case IDC_DBG_AMBIENT:
			case IDC_DBG_WIRE:
			case IDC_DBG_DUAL:
			case IDC_DBG_PICK:
			case IDC_DBG_FPSLIM:
			case IDC_DBG_TILEBB:
				UpdateFlags();
				break;

			case IDC_DBG_VARA:
			case IDC_DBG_VARB:
			case IDC_DBG_VARC:
			case IDC_DBG_FILE:
				break;
		
			default: 
				LogErr("LOWORD(%hu), HIWORD(0x%hX)",LOWORD(wParam),HIWORD(wParam));
				break;
		}
		break;
	}

	return oapiDefDialogProc(hWnd, uMsg, wParam, lParam);
}

} //namespace


		