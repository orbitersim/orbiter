// ==============================================================
// Atmospheric controls implementation
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2014 Jarmo Nikkanen
// ==============================================================

#include "D3D9Client.h"
#include "resource.h"
#include "D3D9Config.h"
#include "AtmoControls.h"
#include "Commctrl.h"
#include "vObject.h"
#include "vPlanet.h"
#include "Mesh.h"
#include "Scene.h"
#include <stdio.h>

using namespace oapi;

extern HINSTANCE g_hInst;
extern D3D9Client *g_client;

// ==============================================================

// Defaut c'tor to init members
ScatterParams::ScatterParams() :
	red      ( 0.650 ),  // 0.400 ... 0.700
	green    ( 0.500 ),  // 0.400 ... 0.700
	blue     ( 0.480 ),  // 0.400 ... 0.700
	rpow	 ( 4.0 ),    // -8.0 ... 8.0
	mpow	 ( 1.0 ),    // -2.0 ... 2.0
	height   ( 8.0 ),    // 4.0 ... 40.0 [km]
	depth    ( 1.0 ),    // 0.0 ... 1.5
	// ----------------------------------------
	expo     ( 1.0 ),    // 0.2 ... 3.0
	balance  ( 0.0 ),    // -0.5 ... 0.5
	// ----------------------------------------
	rin      ( 1.0 ),    // 0.0 ... 3.0
	rout     ( 0.592 ),  // 0.0 ... 4.0
	rphase   ( 0.3395 ), // 0.0 ... 3.5
	// ----------------------------------------
	mheight  ( 4.0 ),    // 0.0 ... 2.0
	mie      ( 0.0869 ), // 0.0 ... 8.0
	mphase   ( 0.9831 ), // 0.85 ... 0.999
	// ----------------------------------------
	aux1	 ( 0.0 ),    // 0.0 ... 2.0
	aux2	 ( 0.0 ),    // 0.0 ... 2.0
	aux3	 ( 0.0 ),
	aux4	 ( 0.9 ),
	// ----------------------------------------
	orbit	 ( false )   // [true|false]
{
}

// ==============================================================

namespace AtmoControls {

	struct sSlider {
		double min, max;
		HWND hWnd;
		int id;
		int dsp;
		int style;
	};


ScatterParams defs;
ScatterParams *param = NULL;

sSlider Slider[ATM_SLIDER_COUNT];

DWORD atmmode = 0;
DWORD dwCmd = NULL;
HWND hDlg = NULL;
vPlanet *vObj = NULL;

// ==============================================================

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

// ==============================================================

void Create()
{
	vObj = NULL;
	hDlg = NULL;

	dwCmd = oapiRegisterCustomCmd("D3D9 Atmospheric Controls", "This dialog allows to control various atmospheric parameters and effects", OpenDlgClbk, NULL);
	
	memset(Slider,0,sizeof(Slider));
	
	// ATTENTION: Order of params must match with slider indexes

	Slider[0].id = IDC_ATM_RED;
	Slider[1].id = IDC_ATM_GREEN;
	Slider[2].id = IDC_ATM_BLUE;
	Slider[3].id = IDC_ATM_RPOW;
	Slider[4].id = IDC_ATM_IN;
	Slider[5].id = IDC_ATM_OUT;
	Slider[6].id = IDC_ATM_RPHASE;
	Slider[7].id = IDC_ATM_MIE;
	Slider[8].id = IDC_ATM_MPHASE;
	Slider[9].id = IDC_ATM_BALANCE;
	Slider[10].id = IDC_ATM_HEIGHT;
	Slider[11].id = IDC_ATM_AUX2;
	Slider[12].id = IDC_ATM_DEPTH;
	Slider[13].id = IDC_ATM_MPOW;
	Slider[14].id = IDC_ATM_EXPO;
	Slider[15].id = IDC_ATM_MOFFSET;
	Slider[16].id = IDC_ATM_AUX1;
	Slider[17].id = IDC_ATM_AUX3;
	Slider[18].id = IDC_ATM_AUX4;

	Slider[0].dsp = IDC_ATD_RED;
	Slider[1].dsp = IDC_ATD_GREEN;
	Slider[2].dsp = IDC_ATD_BLUE;
	Slider[3].dsp = IDC_ATD_RPOW;
	Slider[4].dsp = IDC_ATD_IN;
	Slider[5].dsp = IDC_ATD_OUT;
	Slider[6].dsp = IDC_ATD_RPHASE;
	Slider[7].dsp = IDC_ATD_MIE;
	Slider[8].dsp = IDC_ATD_MPHASE;
	Slider[9].dsp = IDC_ATD_BALANCE;
	Slider[10].dsp = IDC_ATD_HEIGHT;
	Slider[11].dsp = IDC_ATD_AUX2;
	Slider[12].dsp = IDC_ATD_DEPTH;
	Slider[13].dsp = IDC_ATD_MPOW;
	Slider[14].dsp = IDC_ATD_EXPO;
	Slider[15].dsp = IDC_ATD_MOFFSET;
	Slider[16].dsp = IDC_ATD_AUX1;
	Slider[17].dsp = IDC_ATD_AUX3;
	Slider[18].dsp = IDC_ATD_AUX4;
}

// ==============================================================

bool IsActive()
{
	return (hDlg!=NULL);
}

// ==============================================================

void Release()
{
	if (dwCmd) oapiUnregisterCustomCmd(dwCmd);
	dwCmd = NULL;
}

// ==============================================================

void OpenDlgClbk(void *context)
{
	HWND l_hDlg = oapiOpenDialog(g_hInst, IDD_D3D9SCATTER, WndProc);

	if (l_hDlg) hDlg = l_hDlg; // otherwise open already
	else return;

	Scene *scene = g_client->GetScene();
	
	if (scene) {
		OBJHANDLE hBody = scene->GetCameraProxyBody();
		if (hBody) vObj = (vPlanet *) scene->GetVisObject(hBody);
	}

	if (vObj) param = vObj->GetAtmoParams(atmmode);
	else      param = &defs;

	for (int i=0;i<ATM_SLIDER_COUNT;i++) Slider[i].hWnd = GetDlgItem(hDlg, Slider[i].id);

	// Style flags
	// 1 = unit in [km] 
	// 2 = same for orbital and surface setup
	// 4 = call vPlanet::UpdateAtmoConfig() on change
	// 8 = x^2 "linearization"
	

	ConfigSlider(IDC_ATM_RED,      0.400, 0.700);
	ConfigSlider(IDC_ATM_GREEN,    0.400, 0.700);
	ConfigSlider(IDC_ATM_BLUE,     0.400, 0.700);
	ConfigSlider(IDC_ATM_RPOW,     -8.0, 8.0);
	ConfigSlider(IDC_ATM_MPOW,     -4.0, 4.0);
	ConfigSlider(IDC_ATM_HEIGHT,   2.0, 400.0, 1|2|4|8);
	ConfigSlider(IDC_ATM_DEPTH,    0.0, 0.3);
	// -------------------------------------------------------
	ConfigSlider(IDC_ATM_EXPO,	   0.2, 5.0);
	ConfigSlider(IDC_ATM_BALANCE,  -0.3, 0.7);
	// -------------------------------------------------------
	ConfigSlider(IDC_ATM_OUT,      0.0, 2.0);
	ConfigSlider(IDC_ATM_IN,       0.5, 2.0);
	ConfigSlider(IDC_ATM_RPHASE,   0.0, 1.5);
	// -------------------------------------------------------
	ConfigSlider(IDC_ATM_MOFFSET,  0.1, 10.0, 1|2);
	ConfigSlider(IDC_ATM_MIE,      0.0, 2.0);
	ConfigSlider(IDC_ATM_MPHASE,   0.80, 0.999);
	// -------------------------------------------------------
	ConfigSlider(IDC_ATM_AUX1,	   0.0, 0.3);
	ConfigSlider(IDC_ATM_AUX2,	   0.0, 1.0);
	ConfigSlider(IDC_ATM_AUX3,	   0.0, 2.0);
	ConfigSlider(IDC_ATM_AUX4,	   0.1, 4.4);
	// -------------------------------------------------------
	CreateToolTip(IDC_ATM_RED,		hDlg, "Wavelength setting for red light (default 0.650)");
	CreateToolTip(IDC_ATM_GREEN,	hDlg, "Wavelength setting for green light (default 0.600)");
	CreateToolTip(IDC_ATM_BLUE,		hDlg, "Wavelength setting for blue light (default 0.480)");
	CreateToolTip(IDC_ATM_RPOW,		hDlg, "Main control for atmospheric rayleigh color composition (4.0 for the Earth)");
	CreateToolTip(IDC_ATM_MPOW,		hDlg, "Main control for atmospheric mie color composition (1.0 for the Earth)");
	CreateToolTip(IDC_ATM_HEIGHT,	hDlg, "Atmosphere scale height (7km - 10km for the Earth)");
	CreateToolTip(IDC_ATM_DEPTH,	hDlg, "Sunset color boost");
	// -------------------------------------------------------
	CreateToolTip(IDC_ATM_EXPO,		hDlg, "[PostProcess] Overall brightness control (i.e. Camera \"exposure\" control) (default 1.0)");
	CreateToolTip(IDC_ATM_BALANCE,	hDlg, "[PostProcess] Camera White balance control (default 1.0)");
	// -------------------------------------------------------
	CreateToolTip(IDC_ATM_OUT,		hDlg, "Overall control for rayleigh scattering (i.e. Haze stickness)");
	CreateToolTip(IDC_ATM_IN,		hDlg, "Controls an intensity of in-scattered sunlight (i.e. Haze glow intensity) (default 1.0)");
	CreateToolTip(IDC_ATM_RPHASE,	hDlg, "Controls a directional dependency of in-scattered sunlight (Most visible when camera, planet and the sun are aligned)");
	// -------------------------------------------------------
	CreateToolTip(IDC_ATM_MOFFSET,	hDlg, "Atmosphere scale height for Mie");
	CreateToolTip(IDC_ATM_MIE,		hDlg, "Overall scale factor for mie scattering");
	CreateToolTip(IDC_ATM_MPHASE,	hDlg, "Directional strength of Henyey-Greenstein phase function");
	// -------------------------------------------------------
	CreateToolTip(IDC_ATM_AUX1,		hDlg, "Distance of light transfer in atmosphere behind terminator");
	CreateToolTip(IDC_ATM_AUX2,		hDlg, "Horizon intensity");
	CreateToolTip(IDC_ATM_AUX3,		hDlg, "Sky color boost to compensate a lack of multible scattering. (Orbital sunrise color)");
	CreateToolTip(IDC_ATM_AUX4,		hDlg, "[PostProcess] Gamma correction");
	// -------------------------------------------------------
	
	SendDlgItemMessageA(hDlg, IDC_ATM_MODE, CB_RESETCONTENT, 0, 0);
	SendDlgItemMessageA(hDlg, IDC_ATM_MODE, CB_ADDSTRING, 0, (LPARAM)"Auto");
	SendDlgItemMessageA(hDlg, IDC_ATM_MODE, CB_ADDSTRING, 0, (LPARAM)"Surface");
	SendDlgItemMessageA(hDlg, IDC_ATM_MODE, CB_ADDSTRING, 0, (LPARAM)"Orbital");
	SendDlgItemMessageA(hDlg, IDC_ATM_MODE, CB_SETCURSEL, atmmode, 0);

	SetTimer(hDlg, 0, 200, NULL);

	char title[256];

	if (vObj) {
		if (param->orbit) sprintf_s(title,256,"Atmospheric Controls [%s] [Orbital]", vObj->GetName());
		else			  sprintf_s(title,256,"Atmospheric Controls [%s] [Surface]", vObj->GetName());

		// Not working for some reason !!!
		//if (param->orbit) SetWindowTextA(GetDlgItem(hDlg, IDC_ATM_COPYTO), "Copy to Surface");
		//else			  SetWindowTextA(GetDlgItem(hDlg, IDC_ATM_COPYTO), "Copy to Orbital");

		SetWindowTextA(GetDlgItem(hDlg, IDC_ATM_COPYTO), "Copy to other config");

		SetWindowTextA(hDlg, title);
	}

	UpdateSliders();
}

// ==============================================================

double GetValue(int id)
{
	for (int i=0;i<ATM_SLIDER_COUNT;i++) if (Slider[i].id==id) return param->data[i];
	LogErr("Invalid Slider ID in AtmoControls");
	return 0.0;
}

// ==============================================================

void ConfigSlider(int id, double min, double max, int style)
{
	for (int i=0;i<ATM_SLIDER_COUNT;i++) if (Slider[i].id==id) {
		Slider[i].max = max;
		Slider[i].min = min;
		Slider[i].style = style;
		UpdateSlider(id);
		return;
	}
	LogErr("Invalid Slider ID in AtmoControls");
}

// ==============================================================

void SetSlider(int id, WORD pos)
{
	if (!vObj) return;
	for (int i=0;i<ATM_SLIDER_COUNT;i++) if (Slider[i].id==id) {
		double x = (1000.0-double(pos))/1000.0;
		if (Slider[i].style&8) x = x*x;
		double v = Slider[i].min*(1.0-x) + Slider[i].max*x;
		
		if (Slider[i].style&2) {
			vObj->GetAtmoParams(1)->data[i] = v;
			vObj->GetAtmoParams(2)->data[i] = v;
		}
		else {
			param->data[i] = v;
		}

		if (Slider[i].style&4) vObj->UpdateAtmoConfig(); 

		UpdateSlider(id, false);

		return;
	}
	LogErr("Invalid Slider ID in AtmoControls");
}

// ==============================================================

void UpdateSliders()
{
	for (int i=0;i<ATM_SLIDER_COUNT;i++) UpdateSlider(Slider[i].id);
}

// ==============================================================

void UpdateSlider(int id, bool bSetPos)
{
	char buf[32];

	if (!param) return;

	for (int i=0;i<ATM_SLIDER_COUNT;i++) if (Slider[i].id==id) {

		double val = param->data[i];
		
		SendDlgItemMessage(hDlg, id, TBM_SETRANGEMAX, 1, 1000);
		SendDlgItemMessage(hDlg, id, TBM_SETRANGEMIN, 1, 0);
		SendDlgItemMessage(hDlg, id, TBM_SETTICFREQ,  1, 0);

		if (bSetPos) {
			double x = (val - Slider[i].min)/(Slider[i].max-Slider[i].min);
			if (Slider[i].style&8) x = sqrt(x);
			DWORD dpos = 1000 - DWORD(x*1000.0);
			SendDlgItemMessage(hDlg, id, TBM_SETPOS,  1, dpos);
		}

		if (Slider[i].style&1) sprintf_s(buf,"%.1lf k", val);
		else				   sprintf_s(buf,"%.3lf", val);
		
		SetWindowTextA(GetDlgItem(hDlg, Slider[i].dsp), buf);
		return;
	}
	LogErr("Invalid Slider ID in AtmoControls");
}

// ==============================================================

vPlanet * GetVisual()
{
	return vObj;
}

// ==============================================================

void SetVisual(vObject *vo)
{
	if (!vo) {
		vObj = NULL;
		param = &defs;
		return;
	}

	if (!hDlg || !dwCmd) return;
	
	OBJHANDLE hObj = vo->GetObjectA();

	if (oapiGetObjectType(hObj)!=OBJTP_PLANET) {
		LogErr("Invalid Object Type in AtmoControls");
		vObj = NULL;
		param = &defs;
		return;
	}

	vObj = (vPlanet *)vo;

	if (vObj) param = vObj->GetAtmoParams();
	else	  param = &defs;

	UpdateSliders();
}


// ==============================================================
// Dialog message handler

BOOL CALLBACK WndProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{

	switch (uMsg) {

	case WM_INITDIALOG:
	{
		vObject *vPl = NULL;
		OBJHANDLE hProxy = g_client->GetScene()->GetCameraProxyBody();
		if (hProxy) vPl = g_client->GetScene()->GetVisObject(hProxy);
		SetVisual(vPl);
		return true;
	}

	case WM_TIMER:
	{
		if (vObj) {
			if (param->orbit != vObj->GetAtmoParams(atmmode)->orbit) {
				char title[256];
				param = vObj->GetAtmoParams(atmmode);
				if (param->orbit) sprintf_s(title,256,"Atmospheric Controls [%s] [Orbital]", vObj->GetName());
				else			  sprintf_s(title,256,"Atmospheric Controls [%s] [Surface]", vObj->GetName());
				SetWindowTextA(hDlg,title);
				UpdateSliders();
			}
		}
	}

	case WM_VSCROLL:
	{
		if (LOWORD(wParam)==TB_THUMBTRACK) {
			WORD pos = HIWORD(wParam);
			for (int i=0;i<ATM_SLIDER_COUNT;i++) if (Slider[i].hWnd==HWND(lParam)) {
				SetSlider(Slider[i].id, pos);
				return true;
			}
		}
		return false;
	}

	case WM_COMMAND:

		switch (LOWORD(wParam)) {

			case IDCANCEL:  
			case IDOK:
				oapiCloseDialog(hWnd);
				hDlg = NULL;
				return TRUE;

			case IDC_ATM_LOAD:
				if (vObj) {
					vObj->LoadAtmoConfig(false);
					vObj->LoadAtmoConfig(true);
					UpdateSliders();
				}
				break;

			case IDC_ATM_SAVE:
				if (vObj) {
					vObj->SaveAtmoConfig(false);
					vObj->SaveAtmoConfig(true);
				}
				break;

			case IDC_ATM_COPYTO:
				if (vObj) {
					if (param->orbit) {
						memcpy2(vObj->GetAtmoParams(1), param, sizeof(ScatterParams));
						vObj->GetAtmoParams(1)->orbit = false;
					}
					else {
						memcpy2(vObj->GetAtmoParams(2), param, sizeof(ScatterParams)); 
						vObj->GetAtmoParams(2)->orbit = true;
					}
				}
				break;

			case IDC_ATM_MODE:
				if (HIWORD(wParam)==CBN_SELCHANGE) {
					atmmode = SendDlgItemMessage(hWnd, IDC_ATM_MODE, CB_GETCURSEL, 0, 0);
				}
				break;

			default: 
				//LogErr("LOWORD(%hu), HIWORD(0x%hX)",LOWORD(wParam),HIWORD(wParam));
				break;
		}
		break;
	}

	return oapiDefDialogProc(hWnd, uMsg, wParam, lParam);;
}

} //namespace


		