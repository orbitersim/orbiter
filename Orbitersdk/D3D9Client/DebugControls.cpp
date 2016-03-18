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

char visual[64];
int  origwidth;
HWND hDlg = NULL;
vObject *vObj = NULL;

OPENFILENAMEA OpenTex, SaveTex;
char OpenFileName[255];
char SaveFileName[255];    

void UpdateMaterialDisplay(bool bSetup=false);

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
	SETFLAG(debugFlags, DBG_FLAGS_DSPENVMAP,	(SendDlgItemMessageA(hDlg, IDC_DBG_ENVMAP, BM_GETCHECK, 0, 0)==BST_CHECKED));
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
	SendDlgItemMessageA(hDlg, IDC_DBG_MATPRP, CB_ADDSTRING, 0, (LPARAM)"Diffuse");
	SendDlgItemMessageA(hDlg, IDC_DBG_MATPRP, CB_ADDSTRING, 0, (LPARAM)"Ambient");
	SendDlgItemMessageA(hDlg, IDC_DBG_MATPRP, CB_ADDSTRING, 0, (LPARAM)"Specular");
	SendDlgItemMessageA(hDlg, IDC_DBG_MATPRP, CB_ADDSTRING, 0, (LPARAM)"Emission");
	SendDlgItemMessageA(hDlg, IDC_DBG_MATPRP, CB_ADDSTRING, 0, (LPARAM)"Reflect");
	SendDlgItemMessageA(hDlg, IDC_DBG_MATPRP, CB_ADDSTRING, 0, (LPARAM)"Dissolve");
	SendDlgItemMessageA(hDlg, IDC_DBG_MATPRP, CB_ADDSTRING, 0, (LPARAM)"Fresnel");
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

	SetWindowText(GetDlgItem(hDlg, IDC_DBG_VARA), "1.3");
	SetWindowText(GetDlgItem(hDlg, IDC_DBG_VARB), "0.01");
	SetWindowText(GetDlgItem(hDlg, IDC_DBG_VARC), "0.00");

	SendDlgItemMessageA(hDlg, IDC_DBG_MATEFF, CB_RESETCONTENT, 0, 0);
	SendDlgItemMessageA(hDlg, IDC_DBG_MATEFF, CB_ADDSTRING, 0, (LPARAM)"None");
	while (true) {
		SURFHANDLE hSrf = g_client->GetDissolveMap(idx++);
		if (hSrf) SendDlgItemMessageA(hDlg, IDC_DBG_MATEFF, CB_ADDSTRING, 0, (LPARAM)SURFACE(hSrf)->GetName());
		else break;
	}
	SendDlgItemMessageA(hDlg, IDC_DBG_MATEFF, CB_SETCURSEL, 0, 0);


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
	CreateToolTip(IDC_DBG_FMIPS, hDlg, "Load mipmaps from file. Do not autogenerate them.");
	CreateToolTip(IDC_DBG_ALPHAG, hDlg, "Check if source image contains normal information in alpha(X) and green(Y) channels");
	CreateToolTip(IDC_DBG_SEAMS, hDlg, "Enable seams reduction at each mipmap level");
	CreateToolTip(IDC_DBG_FADE, hDlg, "Enable mipmap post processing. Contrast and detail is reduced from each mipmap to prevent 'stripes' (See:Fa,Fb)");
	CreateToolTip(IDC_DBG_NORM, hDlg, "Center color channels at 0.5f to prevent lightening/darkening the results");
	CreateToolTip(IDC_DBG_VARA, hDlg, "Attennuates high contrast components. Leaves low contrast parts unchanged [1.0 to 1.6]");
	CreateToolTip(IDC_DBG_VARB, hDlg, "Attennuates everything equally. Typical range [0.00 to 0.03]");
	CreateToolTip(IDC_DBG_VARC, hDlg, "Apply noise to main level and all mipmaps before attennuation (Fa,Fb)");
}

// =============================================================================================
//
void UpdateDissolveMap(SURFHANDLE hSrf)
{
	OBJHANDLE hObj = vObj->GetObjectA();

	if (!oapiIsVessel(hObj)) return;

	D3D9Mesh *hMesh = (D3D9Mesh *)vObj->GetMesh(sMesh);

	if (!hMesh) return;

	DWORD matidx = hMesh->GetMeshGroupMaterialIdx(sGroup);
	D3D9MatExt *pMat = hMesh->GetMaterial(matidx);
	
	if (!pMat) return;

	pMat->pDissolve = hSrf;
	pMat->ModFlags |= D3D9MATEX_DISSOLVE;

	vVessel *vVes = (vVessel *)vObj;

	vVes->GetMaterialManager()->RegisterMaterialChange(hMesh, matidx, pMat); 
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
	D3D9MatExt *pMat = hMesh->GetMaterial(matidx);

	if (!pMat) return;

	switch(MatPrp) {

		case 0:	// Diffuse
		{
			pMat->ModFlags |= D3D9MATEX_DIFFUSE;
			switch(clr) {
				case 0: pMat->Diffuse.r = CLAMP(value, 0.0f, 1.0f); break;
				case 1: pMat->Diffuse.g = CLAMP(value, 0.0f, 1.0f); break;
				case 2: pMat->Diffuse.b = CLAMP(value, 0.0f, 1.0f); break;
				case 3: pMat->Diffuse.a = CLAMP(value, 0.0f, 1.0f); break;
			}
			break;
		}

		case 1:	// Ambient
		{
			pMat->ModFlags |= D3D9MATEX_AMBIENT;
			switch(clr) {
				case 0: pMat->Ambient.r = CLAMP(value, 0.0f, 1.0f); break;
				case 1: pMat->Ambient.g = CLAMP(value, 0.0f, 1.0f); break;
				case 2: pMat->Ambient.b = CLAMP(value, 0.0f, 1.0f); break;
				case 3: pMat->Ambient.a = 0.0; break;
			}
			break;
		}

		case 2:	// Specular
		{
			pMat->ModFlags |= D3D9MATEX_SPECULAR;
			switch(clr) {
				case 0: pMat->Specular.r = CLAMP(value, 0.0f, 1.0f); break;
				case 1: pMat->Specular.g = CLAMP(value, 0.0f, 1.0f); break;
				case 2: pMat->Specular.b = CLAMP(value, 0.0f, 1.0f); break;
				case 3: pMat->Specular.a = CLAMP(value, 0.0f, 1000.0f); break;
			}
			break;
		}

		case 3:	// Emission
		{
			pMat->ModFlags |= D3D9MATEX_EMISSIVE;
			switch(clr) {
				case 0: pMat->Emissive.r = CLAMP(value, 0.0f, 1.0f); break;
				case 1: pMat->Emissive.g = CLAMP(value, 0.0f, 1.0f); break;
				case 2: pMat->Emissive.b = CLAMP(value, 0.0f, 1.0f); break;
				case 3: pMat->Emissive.a = 0.0; break;
			}
			break;
		}

		case 4:	// Reflectivity
		{
			pMat->ModFlags |= D3D9MATEX_REFLECT;
			switch(clr) {
				case 0: pMat->Reflect.r = CLAMP(value, 0.0f, 1.0f); break;
				case 1: pMat->Reflect.g = CLAMP(value, 0.0f, 1.0f); break;
				case 2: pMat->Reflect.b = CLAMP(value, 0.0f, 1.0f); break;
			}
			pMat->Reflect.a = max(max(pMat->Reflect.r, pMat->Reflect.g), pMat->Reflect.b);
			break;
		}

		case 5:	// Dissolve
		{
			pMat->ModFlags |= D3D9MATEX_DISSOLVE;
			switch(clr) {
				case 0: pMat->DislScale = CLAMP(value, 0.0f, 12.0f); break;
				case 1: pMat->DislMag = CLAMP(value, 0.0f, 0.2f); break;
			}
			break;
		}

		case 6:	// Fresnel
		{
			pMat->ModFlags |= D3D9MATEX_FRESNEL;
			switch(clr) {
				case 0: pMat->Fresnel.b = CLAMP(value, 1.0f, 6.0f); 
					break;
				case 1: pMat->Fresnel.g = CLAMP(value, 0.0f, 1.0f); 
					break;
			}
			break;
		}
	}


	vVessel *vVes = (vVessel *)vObj;
	vVes->GetMaterialManager()->RegisterMaterialChange(hMesh, matidx, pMat); 
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
	D3D9MatExt *pMat = hMesh->GetMaterial(matidx);
	
	if (!pMat) return 0.0f;

	switch(MatPrp) {

		case 0:	// Diffuse
		{
			switch(clr) {
				case 0: return pMat->Diffuse.r;
				case 1: return pMat->Diffuse.g;
				case 2: return pMat->Diffuse.b;
				case 3: return pMat->Diffuse.a;
			}
			break;
		}

		case 1:	// Ambient
		{
			switch(clr) {
				case 0: return pMat->Ambient.r;
				case 1: return pMat->Ambient.g;
				case 2: return pMat->Ambient.b;
				case 3: return pMat->Ambient.a;
			}
			break;
		}

		case 2:	// Specular
		{
			switch(clr) {
				case 0: return pMat->Specular.r;
				case 1: return pMat->Specular.g;
				case 2: return pMat->Specular.b;
				case 3: return pMat->Specular.a;
			}
			break;
		}

		case 3:	// Emission
		{
			switch(clr) {
				case 0: return pMat->Emissive.r;
				case 1: return pMat->Emissive.g;
				case 2: return pMat->Emissive.b;
				case 3: return pMat->Emissive.a;
			}
			break;
		}

		case 4:	// Reflectivity
		{
			switch(clr) {
				case 0: return pMat->Reflect.r;
				case 1: return pMat->Reflect.g;
				case 2: return pMat->Reflect.b;
				case 3: return pMat->Reflect.a;
			}
			break;
		}

		case 5:	// Dissolve
		{
			switch(clr) {
				case 0: return pMat->DislScale;
				case 1: return pMat->DislMag;
			}
			break;
		}

		case 6:	// Fresnel
		{
			switch(clr) {
				case 0: return pMat->Fresnel.b;	// Power
				case 1: return pMat->Fresnel.g; // Multiplier
			}
			break;
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
	
	//if (MatPrp==2 && SelColor==3) val/=1000.0; // Specular Power
	if (MatPrp==2 && SelColor==3) val = float(log(val) / 6.907755279); // Specular Power
	if (MatPrp==5 && SelColor==0) val/=12.0;  // Dissolve scale
	if (MatPrp==5 && SelColor==1) val/=0.2f;  // Dissolve scatter
	if (MatPrp==6 && SelColor==0) val/=6.0f;  // Fresnel range
	
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
}

// =============================================================================================
//
void UpdateMaterialDisplay(bool bSetup)
{
	char lbl[256];
	char lbl2[64];

	OBJHANDLE hObj = vObj->GetObjectA();
	if (!oapiIsVessel(hObj)) return;
	
	vVessel *vVes = (vVessel *)vObj;
	D3D9Mesh *hMesh = (D3D9Mesh *)vObj->GetMesh(sMesh);
	if (!hMesh) return;

	DWORD matidx = hMesh->GetMeshGroupMaterialIdx(sGroup);

	// Set material info
	const char *skin = NULL;
	if (skin)	sprintf_s(lbl, 256, "Material %u: [Skin %s]", matidx, skin);
	else		sprintf_s(lbl, 256, "Material %u:", matidx);

	GetWindowText(GetDlgItem(hDlg, IDC_DBG_MATGRP), lbl2, 64);
	if (strcmp(lbl, lbl2)) SetWindowText(GetDlgItem(hDlg, IDC_DBG_MATGRP), lbl); // Avoid causing flashing

	DWORD MatPrp = SendDlgItemMessageA(hDlg, IDC_DBG_MATPRP, CB_GETCURSEL, 0, 0);

	switch(MatPrp) {
		case 0:	// Diffuse
			DisplayMat(true, true, true, true);
			if (bSetup) SelColor = 0;
		break;
		case 1:	// Ambient
			DisplayMat(true, true, true, false);
			if (bSetup) SelColor = 0;
		break;
		case 2:	// Specular
			DisplayMat(true, true, true, true);
			if (bSetup) SelColor = 3;
		break;
		case 3:	// Emission
			DisplayMat(true, true, true, false);
			if (bSetup) SelColor = 0;
		break;
		case 4:	// Reflectivity
			DisplayMat(true, true, true, false);
			if (bSetup) SelColor = 0;
		break;
		case 5:	// Dissolve
			DisplayMat(true, true, false, false);
			if (bSetup) SelColor = 0;
		break;
		case 6:	// Fresnel
			DisplayMat(true, true, false, false);
			if (bSetup) SelColor = 0;
		break;
	}

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

	// Setup dissolve texture
	D3D9MatExt * pMat = hMesh->GetMaterial(matidx);
	if (pMat->pDissolve==NULL) SendDlgItemMessageA(hDlg, IDC_DBG_MATEFF, CB_SETCURSEL, 0, 0);
	else {
		int id = g_client->GetIndexOfDissolveMap(pMat->pDissolve);	
		if (id>=0) SendDlgItemMessageA(hDlg, IDC_DBG_MATEFF, CB_SETCURSEL, id+1, 0);
	}
}

// =============================================================================================
//
void UpdateColorSlider(WORD pos)
{
	float val = float(pos)/255.0f;
	
	DWORD MatPrp = SendDlgItemMessageA(hDlg, IDC_DBG_MATPRP, CB_GETCURSEL, 0, 0);
	bool bLink = (SendDlgItemMessageA(hDlg, IDC_DBG_LINK, BM_GETCHECK, 0, 0)==BST_CHECKED);

	if (MatPrp==5 || MatPrp==6) bLink = false;
	if (SelColor==3) bLink = false;

	//if (MatPrp==2 && SelColor==3) val*=1000.0; // Specular Power
	if (MatPrp==2 && SelColor==3) val = float(exp(val*6.907755279)); // Specular Power
	if (MatPrp==5 && SelColor==0) val*=12.0;  // Dissolve scale
	if (MatPrp==5 && SelColor==1) val*=0.2f;  // Dissolve scatter
	if (MatPrp==6 && SelColor==0) val*=6.0f;  // Fresnel range
	
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
	if ((prm->Func&0x8)==0) C = D3DXVECTOR4(C.z, C.y, C.z, C.x);
	
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
	if (SendDlgItemMessageA(hDlg, IDC_DBG_ALPHAG, BM_GETCHECK, 0, 0)==BST_CHECKED) Func |= 0x8;

	if (Action>=0 && Action<=2) {

		LPDIRECT3DTEXTURE9 pTex = NULL;
		LPDIRECT3DTEXTURE9 pWork = NULL;
		LPDIRECT3DTEXTURE9 pSave = NULL;
		D3DXIMAGE_INFO info;

		DWORD Mips = 0;
		if (SendDlgItemMessageA(hDlg, IDC_DBG_FMIPS, BM_GETCHECK, 0, 0)==BST_CHECKED) Mips = D3DX_FROM_FILE;
	
		HR(D3DXCreateTextureFromFileExA(pDevice, pOF->lpstrFile, D3DX_DEFAULT, D3DX_DEFAULT, Mips, 0, D3DFMT_A8R8G8B8, D3DPOOL_SYSTEMMEM, D3DX_DEFAULT, D3DX_DEFAULT, 0, &info, NULL, &pTex));	
		
		if (!pTex) {
			LogErr("Failed to open a file [%s]", pOF->lpstrFile); 
			return false;
		}

		if (true) {

			HR(D3DXCreateTexture(pDevice, info.Width, info.Height, info.MipLevels, 0, D3DFMT_A8R8G8B8, D3DPOOL_SYSTEMMEM, &pWork));	
			if (!pWork) return false;

			if (Action==0) { HR(D3DXCreateTexture(pDevice, info.Width, info.Height, info.MipLevels, 0, D3DFMT_DXT5, D3DPOOL_SYSTEMMEM, &pSave)); }
			if (Action==1) { HR(D3DXCreateTexture(pDevice, info.Width, info.Height, info.MipLevels, 0, D3DFMT_A8R8G8B8, D3DPOOL_SYSTEMMEM, &pSave)); }
			if (Action==2) { HR(D3DXCreateTexture(pDevice, info.Width, info.Height, info.MipLevels, 0, D3DFMT_A4R4G4B4, D3DPOOL_SYSTEMMEM, &pSave)); }

			if (!pSave) return false;

			
			// Process texture ----------------------------------------------
			//
			for (DWORD n=0;n<info.MipLevels;n++) {

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
			for (DWORD n=0;n<info.MipLevels;n++) {
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
			

// ==============================================================
// Dialog message handler

BOOL CALLBACK WndProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	char lbl[32];
	RECT rect;
	bool bPaused;

	OpenTex.hwndOwner = hWnd;

	DWORD Prp = SendDlgItemMessageA(hDlg, IDC_DBG_MATPRP, CB_GETCURSEL, 0, 0);

	switch (uMsg) {

	case WM_INITDIALOG:
	{
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
				oapiCloseDialog(hWnd);
				hDlg = NULL;
				vObj = NULL;
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

			case IDC_DBG_LINK:
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

			case IDC_DBG_MATEFF:
				if (HIWORD(wParam)==CBN_SELCHANGE) {
					DWORD idx = SendDlgItemMessage(hWnd, IDC_DBG_MATEFF, CB_GETCURSEL, 0, 0);
					if (idx==0) UpdateDissolveMap(NULL);
					else        UpdateDissolveMap(g_client->GetDissolveMap(idx-1));
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
				SetWindowPos(hDlg, NULL, rect.left, rect.top, origwidth, rect.bottom - rect.top, SWP_SHOWWINDOW);
				break;

			case IDC_DBG_LESS:
				GetWindowRect(hDlg, &rect);
				SetWindowPos(hDlg, NULL, rect.left, rect.top, 298, rect.bottom - rect.top, SWP_SHOWWINDOW);
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
			case IDC_DBG_ENVMAP:
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


		