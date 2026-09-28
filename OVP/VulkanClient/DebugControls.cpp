// ===========================================================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2012-2026 Jarmo Nikkanen
// ===========================================================================================


#include "D3D9Client.h"
#include "resource.h"
#include "D3D9Config.h"
#include "D3D9Surface.h"
#include "DebugControls.h"
// Commctrl.h left out: trackbars and tooltips are Qt widgets
#include "VObject.h"
#include "VVessel.h"
#include "VPlanet.h"
#include "Mesh.h"
#include "MaterialMgr.h"
#include "VectorHelpers.h"
#include "OrbiterResource.h"
#include <QAbstractButton>
#include <QComboBox>
#include <QFileDialog>
#include <QKeyEvent>
#include <QLineEdit>
#include <QMessageBox>
#include <QSignalBlocker>
#include <QSlider>
#include <functional>
#include <stdarg.h>
#include <stdio.h>
#include <imgui.h>
#include <imgui_extras.h>

enum scale { LIN, SQRT, SQR };

using namespace oapi;
using std::min;
using std::max;

extern void *g_hInst;
extern D3D9Client *g_client;

// Little binary helper
#define SETFLAG(bitmap, bit, value) (value ? bitmap |= bit : bitmap &= ~bit)
#define CLAMP(x,a,b) min(max(a,x),b) 

namespace DebugControls {

DWORD dwGFX, dwCmd, nMesh, nGroup, sMesh, sGroup, debugFlags, dspMode, camMode, SelColor, sEmitter;
double camSpeed;
float cpr, cpg, cpb, cpa;
double resbias = 4.0;
char visual[64];
int  origwidth;
GFXDialog *gfxDlg;
QWidget *hDlg = NULL;
QWidget *hDataWnd = NULL;
vObject *vObj = NULL;
std::string buffer("");
std::string buffer2("");
D3DXVECTOR3 PickLocation;

std::map<int, const LightEmitter*> Emitters;

QWidget *hTipRed, *hTipGrn, *hTipBlu, *hTipAlp;

// OPENFILENAMEA OpenTex, SaveTex left out: QFileDialog takes the folder, filter and flags where the dialogs open
char OpenFileName[255];
char SaveFileName[255];

void UpdateMaterialDisplay(bool bSetup=false);

// not upstream: upstream's 298 px narrow window in this font: the left column, IDC_DBG_MATGRP's right edge plus its left margin
int NarrowWidth()
{
	QWidget *grp = oapiResDlgItem(hDlg, IDC_DBG_MATGRP);
	return grp->x() + grp->width() + grp->x();
}

void OpenGFXDlgClbk(void *context);

// not upstream: EN_SETFOCUS, which has no RESNOTIFY code; the edit boxes' focus events deliver it
const int RESN_SETFOCUS = -1;

// not upstream: Win32 notifications Qt delivers as events: DefDlgProc's WM_CLOSE (and Esc) → IDCANCEL, EN_SETFOCUS
class DlgEvents : public QObject
{
public:
	DlgEvents(QWidget *hWnd, QEvent::Type type, std::function<void()> notify) : QObject(hWnd), type(type), onEvent(notify) { hWnd->installEventFilter(this); }
	bool eventFilter(QObject *o, QEvent *e) override
	{
		bool esc = (type == QEvent::Close && e->type() == QEvent::KeyPress && static_cast<QKeyEvent*>(e)->key() == Qt::Key_Escape);
		if (e->type() != type && !esc) return false;
		if (type == QEvent::Close) e->ignore();
		onEvent();
		return (type == QEvent::Close); // FocusIn still reaches the control
	}
private:
	QEvent::Type type;
	std::function<void()> onEvent;
};

class GFXDialog: public ImGuiDialog
{
public:
	GFXDialog():ImGuiDialog("Graphics Controls") {}
	void OnDraw() override {
		ImGui::PushItemWidth(150.0);
		ImGui::SeparatorText("Post Processing Configuration");


		ImGui::SliderFloatReset("Light glow intensity", &Config->GFXIntensity, 0.0f, 1.0f, 0.5f, "%1.2f");
		ImGui::SliderFloatReset("Light glow distance", &Config->GFXDistance, 0.0f, 1.0f, 0.8f, "%1.2f");
		ImGui::SliderFloatReset("Glow threshold", &Config->GFXThreshold, 0.5f, 2.0f, 1.1f, "%1.2f");
		ImGui::SliderFloatReset("Gamma", &Config->GFXGamma, 0.3f, 2.5f, 1.0f, "%1.2f");

		ImGui::SeparatorText("Light Configuration");

		ImGui::SliderFloatReset("Sunlight Intensity", &Config->GFXSunIntensity, 0.5f, 2.5f, 1.2f, "%1.2f");
		ImGui::SliderFloatReset("Indirect Lighting", &Config->PlanetGlow, 0.01f, 2.0f, 0.7f, "%1.2f");
		ImGui::SliderFloatReset("Local Lights Max", &Config->GFXLocalMax, 0.001f, 1.0f, 0.5f, "%1.2f");
		ImGui::SliderFloatReset("Sun Glare Intensity", &Config->GFXGlare, 0.001f, 1.0f, 0.5f, "%1.2f");

		if(ImGui::Button("Recrete Sun/Glares")) {
			g_client->GetScene()->CreateSunGlare();
		}
		ImGui::PopItemWidth();
	}
};


struct _Variable {
	float min, max, extmax, def;
	scale Scl;
	bool bUsed;
	bool bGamma;
	char tip[80];
};

struct MatParams {
	MatParams(string n, DWORD i) : name(n), id(i) {}
	string name;
	DWORD id;
};

std::vector<MatParams> PrmList;
std::vector<MatParams> Dropdown;

struct _Params {
	_Variable var[4];
};


_Params Params[20] = { 0 };



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
float GetFloatFromBox(QWidget *hWnd, int item)
{
	char lbl[32];
	oapiGetDlgItemText(hWnd, item, lbl, 32);	
	return float(atof(lbl));
}

// =============================================================================================
//
QWidget *CreateToolTip(int toolID, QWidget *hDlg, const char *pszText)
{
    if (!toolID || !hDlg || !pszText) return NULL;
    
    // Get the window of the tool.
    QWidget *hwndTool = oapiResDlgItem(hDlg, toolID);
    // tooltip window left out: the tool shows its own tooltip (TTS_BALLOON style has no Qt counterpart); the tool is returned as its handle
    
    if (!hwndTool) return NULL;
                                                          
    // Associate the tooltip with the tool.
    hwndTool->setToolTip(QString::fromUtf8(pszText)); // TTM_ADDTOOL

    return hwndTool;
}


void SetToolTip(int toolID, QWidget *hTip, const char *a)
{
	QWidget *hwndTool = oapiResDlgItem(hDlg, toolID);
	if (hwndTool) hwndTool->setToolTip(QString::fromUtf8(a)); // TTM_UPDATETIPTEXT
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
	PickLocation = D3DXVECTOR3(0,0,0);

	cpr = cpg = cpb = cpa = 0.0f;

	if (Config->EnableMeshDbg) {
		dwCmd = oapiRegisterCustomCmd((char*)"Vulkan Debug Controls", (char*)"This dialog allows to control various debug and development features", OpenDlgClbk, NULL); // not upstream: Vulkan in place of D3D9
	}
	else {
		dwCmd = 0;
	}

	gfxDlg = new GFXDialog();
	dwGFX = oapiRegisterCustomCmd((char*)"Vulkan Graphics Controls", (char*)"This dialog allows to control various graphics options", OpenGFXDlgClbk, gfxDlg); // not upstream: Vulkan in place of D3D9

	resbias = 4.0 + Config->LODBias;
  
	// OpenTex setup left out: the folder "Textures" and the filter go to QFileDialog::getOpenFileName (existing files only by default)
	memset(OpenFileName, 0, sizeof(OpenFileName));

	// SaveTex setup left out: the filter goes to QFileDialog::getSaveFileName (it asks before overwriting by default)
	memset(SaveFileName, 0, sizeof(SaveFileName));

	PrmList.push_back(MatParams("Diffuse", 0));
	PrmList.push_back(MatParams("Ambient", 1));
	PrmList.push_back(MatParams("Specular", 2));
	PrmList.push_back(MatParams("Emission", 3));
	PrmList.push_back(MatParams("Reflect", 4));
	PrmList.push_back(MatParams("Smoothness", 5));
	PrmList.push_back(MatParams("Fresnel", 6));
	PrmList.push_back(MatParams("Emission2", 7));
	PrmList.push_back(MatParams("Metalness", 8));
	PrmList.push_back(MatParams("SpecialFX", 9));
	PrmList.push_back(MatParams("- - - - - -", 10));
	PrmList.push_back(MatParams("Tune Albedo", 11));
	PrmList.push_back(MatParams("Tune _Emis", 12));
	PrmList.push_back(MatParams("Tune _Refl", 13));
	PrmList.push_back(MatParams("Tune _Rghn", 14));
	PrmList.push_back(MatParams("Tune _Transl", 15));
	PrmList.push_back(MatParams("Tune _Transm", 16));
	PrmList.push_back(MatParams("Tune _Spec", 17));
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
	return (int)DlgItem<QComboBox>(hDlg, IDC_DBG_SCENEDBG)->currentIndex();
}

// =============================================================================================
//
int GetSelectedEnvMap()
{
	if (!hDlg) return 0;
	return (int)DlgItem<QComboBox>(hDlg, IDC_DBG_ENVMAP)->currentIndex();
}

// =============================================================================================
//
void Release()
{
	vObj = NULL;
	hDlg = NULL;
	oapiCloseDialog(gfxDlg);
	delete gfxDlg;
	gfxDlg = NULL;
	if (dwCmd) oapiUnregisterCustomCmd(dwCmd);
	if (dwGFX) oapiUnregisterCustomCmd(dwGFX);
	dwCmd = 0;
	dwGFX = 0;
}

// =============================================================================================
//
void UpdateFlags()
{
	SETFLAG(debugFlags, DBG_FLAGS_SELGRPONLY,	(DlgItem<QAbstractButton>(hDlg, IDC_DBG_GRPO)->isChecked()));
	SETFLAG(debugFlags, DBG_FLAGS_SELMSHONLY,	(DlgItem<QAbstractButton>(hDlg, IDC_DBG_MSHO)->isChecked()));
	SETFLAG(debugFlags, DBG_FLAGS_TILEBOXES,	(DlgItem<QAbstractButton>(hDlg, IDC_DBG_TILEBB)->isChecked()));
	SETFLAG(debugFlags, DBG_FLAGS_BOXES,		(DlgItem<QAbstractButton>(hDlg, IDC_DBG_BOXES)->isChecked()));
	SETFLAG(debugFlags, DBG_FLAGS_SPHERES,		(DlgItem<QAbstractButton>(hDlg, IDC_DBG_SPHERES)->isChecked()));
	SETFLAG(debugFlags, DBG_FLAGS_HLMESH,		(DlgItem<QAbstractButton>(hDlg, IDC_DBG_HSM)->isChecked()));
	SETFLAG(debugFlags, DBG_FLAGS_HLGROUP,		(DlgItem<QAbstractButton>(hDlg, IDC_DBG_HSG)->isChecked()));
	SETFLAG(debugFlags, DBG_FLAGS_SELVISONLY,	(DlgItem<QAbstractButton>(hDlg, IDC_DBG_VISO)->isChecked()));
	SETFLAG(debugFlags, DBG_FLAGS_AMBIENT,	    (DlgItem<QAbstractButton>(hDlg, IDC_DBG_AMBIENT)->isChecked()));
	SETFLAG(debugFlags, DBG_FLAGS_WIREFRAME,	(DlgItem<QAbstractButton>(hDlg, IDC_DBG_WIRE)->isChecked()));
	SETFLAG(debugFlags, DBG_FLAGS_DUALSIDED,	(DlgItem<QAbstractButton>(hDlg, IDC_DBG_DUAL)->isChecked()));
	SETFLAG(debugFlags, DBG_FLAGS_PICK,			(DlgItem<QAbstractButton>(hDlg, IDC_DBG_PICK)->isChecked()));
	SETFLAG(debugFlags, DBG_FLAGS_FPSLIM,		(DlgItem<QAbstractButton>(hDlg, IDC_DBG_FPSLIM)->isChecked()));

	Config->EnableLimiter = (int)((debugFlags&DBG_FLAGS_FPSLIM)>0);
}

// =============================================================================================
//
void SetGroupHighlight(bool bStat)
{
	SETFLAG(debugFlags, DBG_FLAGS_HLGROUP, bStat);
}

inline _Variable DefVar(float min, float max, float extmax, scale scl, const char *tip, bool bGamma = false)
{
	_Variable var;
	var.bUsed = true;
	var.Scl = scl;
	var.bGamma = bGamma;
	var.max = max;
	var.extmax = extmax;
	var.min = min;
	snprintf(var.tip, 80, "%s", tip);
	return var;
}

inline _Variable DefVar(float min, float max, scale scl, const char *tip, bool bGamma=false)
{
	_Variable var;
	var.bUsed = true;
	var.Scl = scl;
	var.bGamma = bGamma;
	var.max = max;
	var.extmax = max;
	var.min = min;
	snprintf(var.tip, 80, "%s", tip);
	return var;
}

DWORD DropdownList(DWORD x)
{
	if (x >= Dropdown.size()) return 0;
	return Dropdown[x].id;
}

// =============================================================================================
//
void InitMatList(WORD shader)
{
	int idx = DlgItem<QComboBox>(hDlg, IDC_DBG_MATPRP)->currentIndex();
	DlgItem<QComboBox>(hDlg, IDC_DBG_MATPRP)->clear();
	
	Dropdown.clear();

	if (shader == SHADER_NULL) {
		std::list<char> list = { 0, 1, 2, 3, 4, 5, 6, 7, 10, 11, 12, 13, 14, 15, 16, 17 };
		for (auto x : list) Dropdown.push_back(PrmList[x]);
		for (auto x : Dropdown) oapiComboAddString(DlgItem<QComboBox>(hDlg, IDC_DBG_MATPRP), x.name.c_str());	
	}

	if (shader == SHADER_METALNESS) {
		std::list<char> list = { 0, 3, 5, 7, 8, 9 };
		for (auto x : list) Dropdown.push_back(PrmList[x]);
		for (auto x : Dropdown) oapiComboAddString(DlgItem<QComboBox>(hDlg, IDC_DBG_MATPRP), x.name.c_str());
	}

	DlgItem<QComboBox>(hDlg, IDC_DBG_MATPRP)->setCurrentIndex(idx);

	switch (shader) {
	case SHADER_NULL:
		Params[6].var[1] = DefVar(0, 1, LIN, "Maximum intensity");
		Params[6].var[2] = DefVar(10.0f, 4096.0f, SQRT, "Specular lobe size");
		break;
	case SHADER_METALNESS:
		Params[6].var[1] = DefVar(0, 1, LIN, "Fresnel effect attennuation 1.0 = disabled, 0.0 = max intensity");
		Params[6].var[2].bUsed = false;
		break;
	}
}



// =============================================================================================
//
void OpenDlgClbk(void *context)
{
	DWORD idx = 0;
	QWidget *l_hDlg = oapiOpenDialog(g_hInst, IDD_D3D9MESHDEBUG, WndProc);

	if (l_hDlg) hDlg = l_hDlg; // otherwise open already
	else return;

	// GetWindowRect/SetWindowPos: the dialog's own size; the 298 px narrow width is the left column (see NarrowWidth)
	int width = hDlg->width();
	hDlg->resize(NarrowWidth(), hDlg->height());
	hDlg->show(); // SWP_SHOWWINDOW
	origwidth = width;

	DlgItem<QAbstractButton>(hDlg, IDC_DBG_FPSLIM)->setChecked(Config->EnableLimiter==1);

	DlgItem<QComboBox>(hDlg, IDC_DBG_DISPLAY)->clear();
	oapiComboAddString(DlgItem<QComboBox>(hDlg, IDC_DBG_DISPLAY), "Everything");
	oapiComboAddString(DlgItem<QComboBox>(hDlg, IDC_DBG_DISPLAY), "Selected Visual");
	oapiComboAddString(DlgItem<QComboBox>(hDlg, IDC_DBG_DISPLAY), "Selected Mesh");
	oapiComboAddString(DlgItem<QComboBox>(hDlg, IDC_DBG_DISPLAY), "Selected Group");
	DlgItem<QComboBox>(hDlg, IDC_DBG_DISPLAY)->setCurrentIndex(0);

	DlgItem<QComboBox>(hDlg, IDC_DBG_CAMERA)->clear();
	oapiComboAddString(DlgItem<QComboBox>(hDlg, IDC_DBG_CAMERA), "Center on visual");
	oapiComboAddString(DlgItem<QComboBox>(hDlg, IDC_DBG_CAMERA), "Wheel Fly/Pan Cam");
	DlgItem<QComboBox>(hDlg, IDC_DBG_CAMERA)->setCurrentIndex(0);

	DlgItem<QComboBox>(hDlg, IDC_DBG_DEFSHADER)->clear();
	oapiComboAddString(DlgItem<QComboBox>(hDlg, IDC_DBG_DEFSHADER), "PBR (Old)");
	oapiComboAddString(DlgItem<QComboBox>(hDlg, IDC_DBG_DEFSHADER), "Metalness PBR");
	DlgItem<QComboBox>(hDlg, IDC_DBG_DEFSHADER)->setCurrentIndex(0);

	DlgItem<QComboBox>(hDlg, IDC_DBG_SCENEDBG)->clear();
	oapiComboAddString(DlgItem<QComboBox>(hDlg, IDC_DBG_SCENEDBG), "None");
	oapiComboAddString(DlgItem<QComboBox>(hDlg, IDC_DBG_SCENEDBG), "Normals Global");
	oapiComboAddString(DlgItem<QComboBox>(hDlg, IDC_DBG_SCENEDBG), "Normals Tangent");
	oapiComboAddString(DlgItem<QComboBox>(hDlg, IDC_DBG_SCENEDBG), "Height");
	oapiComboAddString(DlgItem<QComboBox>(hDlg, IDC_DBG_SCENEDBG), "Height Mk2");
	oapiComboAddString(DlgItem<QComboBox>(hDlg, IDC_DBG_SCENEDBG), "Tile Level");
	DlgItem<QComboBox>(hDlg, IDC_DBG_SCENEDBG)->setCurrentIndex(0);

	DlgItem<QComboBox>(hDlg, IDC_DBG_ACTION)->clear();
	oapiComboAddString(DlgItem<QComboBox>(hDlg, IDC_DBG_ACTION), "Convert to DXT5");
	oapiComboAddString(DlgItem<QComboBox>(hDlg, IDC_DBG_ACTION), "Convert to RGB8");
	oapiComboAddString(DlgItem<QComboBox>(hDlg, IDC_DBG_ACTION), "Convert to RGB4");
	DlgItem<QComboBox>(hDlg, IDC_DBG_ACTION)->setCurrentIndex(0);

	DlgItem<QComboBox>(hDlg, IDC_DBG_TARGET)->clear();
	oapiComboAddString(DlgItem<QComboBox>(hDlg, IDC_DBG_TARGET), "Save");
	oapiComboAddString(DlgItem<QComboBox>(hDlg, IDC_DBG_TARGET), "Assign to slot 0");
	oapiComboAddString(DlgItem<QComboBox>(hDlg, IDC_DBG_TARGET), "Assign to slot 1");
	oapiComboAddString(DlgItem<QComboBox>(hDlg, IDC_DBG_TARGET), "Assign to slot 2");
	DlgItem<QComboBox>(hDlg, IDC_DBG_TARGET)->setCurrentIndex(0);

	DlgItem<QComboBox>(hDlg, IDC_DBG_ENVMAP)->clear();
	oapiComboAddString(DlgItem<QComboBox>(hDlg, IDC_DBG_ENVMAP), "None");
	oapiComboAddString(DlgItem<QComboBox>(hDlg, IDC_DBG_ENVMAP), "Mirror");
	oapiComboAddString(DlgItem<QComboBox>(hDlg, IDC_DBG_ENVMAP), "Blur 1");
	oapiComboAddString(DlgItem<QComboBox>(hDlg, IDC_DBG_ENVMAP), "Blur 2");
	oapiComboAddString(DlgItem<QComboBox>(hDlg, IDC_DBG_ENVMAP), "Blur 3");
	oapiComboAddString(DlgItem<QComboBox>(hDlg, IDC_DBG_ENVMAP), "Blur 4");
	oapiComboAddString(DlgItem<QComboBox>(hDlg, IDC_DBG_ENVMAP), "Irrad.Probe");
	oapiComboAddString(DlgItem<QComboBox>(hDlg, IDC_DBG_ENVMAP), "IrdPreItg");
	oapiComboAddString(DlgItem<QComboBox>(hDlg, IDC_DBG_ENVMAP), "ShadowMap");
	oapiComboAddString(DlgItem<QComboBox>(hDlg, IDC_DBG_ENVMAP), "Irradiance");
	oapiComboAddString(DlgItem<QComboBox>(hDlg, IDC_DBG_ENVMAP), "GlowMask");
	oapiComboAddString(DlgItem<QComboBox>(hDlg, IDC_DBG_ENVMAP), "ScreenDepth");
	oapiComboAddString(DlgItem<QComboBox>(hDlg, IDC_DBG_ENVMAP), "Normals");
	oapiComboAddString(DlgItem<QComboBox>(hDlg, IDC_DBG_ENVMAP), "LightVisbil.");
	oapiComboAddString(DlgItem<QComboBox>(hDlg, IDC_DBG_ENVMAP), "EclipseTbl");
	DlgItem<QComboBox>(hDlg, IDC_DBG_ENVMAP)->setCurrentIndex(0);


	oapiSetDlgItemText(hDlg, IDC_DBG_VARA, "1.3");
	oapiSetDlgItemText(hDlg, IDC_DBG_VARB, "0.01");
	oapiSetDlgItemText(hDlg, IDC_DBG_VARC, "0.00");

	// TBM_ messages don't send WM_HSCROLL
	QSignalBlocker blockRes(DlgItem<QSlider>(hDlg, IDC_DBG_RESBIAS)), blockSpd(DlgItem<QSlider>(hDlg, IDC_DBG_SPEED)), blockMat(DlgItem<QSlider>(hDlg, IDC_DBG_MATADJ));

	// Speed slider
	DlgItem<QSlider>(hDlg, IDC_DBG_RESBIAS)->setMaximum(10);
	DlgItem<QSlider>(hDlg, IDC_DBG_RESBIAS)->setMinimum(-10);
	DlgItem<QSlider>(hDlg, IDC_DBG_RESBIAS)->setTickInterval(1);
	DlgItem<QSlider>(hDlg, IDC_DBG_RESBIAS)->setValue(int((resbias-4.0)*5.0));

	// Speed slider
	DlgItem<QSlider>(hDlg, IDC_DBG_SPEED)->setMaximum(200);
	DlgItem<QSlider>(hDlg, IDC_DBG_SPEED)->setMinimum(1);
	DlgItem<QSlider>(hDlg, IDC_DBG_SPEED)->setTickInterval(1);
	DlgItem<QSlider>(hDlg, IDC_DBG_SPEED)->setValue(75);
	oapiSetDlgItemText(hDlg, IDC_DBG_SPEEDDSP, "29");

	// Meterial slider
	DlgItem<QSlider>(hDlg, IDC_DBG_MATADJ)->setMaximum(255);
	DlgItem<QSlider>(hDlg, IDC_DBG_MATADJ)->setMinimum(0);
	DlgItem<QSlider>(hDlg, IDC_DBG_MATADJ)->setTickInterval(1);
	DlgItem<QSlider>(hDlg, IDC_DBG_MATADJ)->setValue(0);
	
	// Set the "pick" checked
	DlgItem<QAbstractButton>(hDlg, IDC_DBG_PICK)->setChecked(1);

	camMode = 0;
	dspMode = 0;

	OBJHANDLE hTgt = oapiCameraTarget();

	SetVisual(g_client->GetScene()->GetVisObject(hTgt));	// This will call SetupMeshGroups()

	UpdateFlags();

	CreateToolTip(IDC_DBG_TARGET, hDlg, (char*)"Select a target where the resulting image is assigned");
	CreateToolTip(IDC_DBG_SEAMS, hDlg, (char*)"Enable seams reduction at each mipmap level");
	CreateToolTip(IDC_DBG_FADE, hDlg, (char*)"Enable mipmap post processing. Contrast and detail is reduced from each mipmap to prevent 'stripes' (See:Fa,Fb)");
	CreateToolTip(IDC_DBG_NORM, hDlg, (char*)"Center color channels at 0.5f to prevent lightening/darkening the results");
	CreateToolTip(IDC_DBG_VARA, hDlg, (char*)"Attennuates high contrast components. Leaves low contrast parts unchanged [1.0 to 1.6]");
	CreateToolTip(IDC_DBG_VARB, hDlg, (char*)"Attennuates everything equally. Typical range [0.00 to 0.03]");
	CreateToolTip(IDC_DBG_VARC, hDlg, (char*)"Apply noise to main level and all mipmaps before attennuation (Fa,Fb)");
	CreateToolTip(IDC_DBG_MORE, hDlg, (char*)"Click to show/hide more options");
	CreateToolTip(IDC_DBG_EXTEND, hDlg, (char*)"Extend Diffuse/Roughess material range beyond 1.0f to allow texture fine tuning.");
	CreateToolTip(IDC_DBG_LINK, hDlg, (char*)"Adjust all color channels at the same time");
	CreateToolTip(IDC_DBG_DEFINED, hDlg, (char*)"Use the material property for rendering and save it");

	hTipRed = CreateToolTip(IDC_DBG_RED, hDlg, (char*)"Red");
	hTipGrn = CreateToolTip(IDC_DBG_GREEN, hDlg, (char*)"Green");
	hTipBlu = CreateToolTip(IDC_DBG_BLUE, hDlg, (char*)"Blue");
	hTipAlp = CreateToolTip(IDC_DBG_ALPHA, hDlg, (char*)"Alpha");

	// Diffuse
	Params[0].var[0] = DefVar(0, 1, 2, SQRT, "Red");
	Params[0].var[1] = DefVar(0, 1, 2, SQRT, "Green");
	Params[0].var[2] = DefVar(0, 1, 2, SQRT, "Blue");
	Params[0].var[3] = DefVar(0, 1, 2, LIN, "Alpha");

	// Ambient
	Params[1].var[0] = DefVar(0, 1, LIN, "Red");
	Params[1].var[1] = DefVar(0, 1, LIN, "Green");
	Params[1].var[2] = DefVar(0, 1, LIN, "Blue");

	// Specular
	Params[2].var[0] = DefVar(0, 1, SQRT, "Red");
	Params[2].var[1] = DefVar(0, 1, SQRT, "Green");
	Params[2].var[2] = DefVar(0, 1, SQRT, "Blue");
	Params[2].var[3] = DefVar(1, 4096.0f, SQRT, "Specular power");

	// Emission
	Params[3].var[0] = DefVar(0, 1, LIN, "Red");
	Params[3].var[1] = DefVar(0, 1, LIN, "Green");
	Params[3].var[2] = DefVar(0, 1, LIN, "Blue");

	// Reflectivity
	Params[4].var[0] = DefVar(0, 1, LIN, "Red");
	Params[4].var[1] = DefVar(0, 1, LIN, "Green");
	Params[4].var[2] = DefVar(0, 1, LIN, "Blue");

	// Smoothness
	Params[5].var[0] = DefVar(0, 1, 2, LIN, "Smoothness");
	Params[5].var[1] = DefVar(0, 3, SQRT, "Texture linearity (default 1.0)");

	// Fresnel
	Params[6].var[0] = DefVar(0.5f, 2, LIN, "Angle dependency");
	Params[6].var[1] = DefVar(0, 1, LIN, "Maximum intensity");
	Params[6].var[2] = DefVar(10.0f, 4096.0f, SQRT, "Specular lobe size");
	
	// Emission2
	Params[7].var[0] = DefVar(0, 2, LIN, "Red");
	Params[7].var[1] = DefVar(0, 2, LIN, "Green");
	Params[7].var[2] = DefVar(0, 2, LIN, "Blue");

	// Metalness
	Params[8].var[0] = DefVar(0, 1, LIN, "Metalness");

	// SpecialFX
	Params[9].var[0] = DefVar(0, 1, LIN, "Part Temperature");
	

	// Unused index 10
	
	// Tuning -------------------------------------------------------------------------------------
	// Albedo
	int i = 11;
	Params[i].var[0] = DefVar(0.2f, 5.0f, SQRT, "Red");
	Params[i].var[1] = DefVar(0.2f, 5.0f, SQRT, "Green");
	Params[i].var[2] = DefVar(0.2f, 5.0f, SQRT, "Blue");
	Params[i].var[3] = DefVar(0.2f, 5.0f, SQRT, "Gamma", true);

	Params[1 + i] = Params[i];	 // Emis
	Params[1 + i].var[3] = DefVar(0.2f, 5.0f, SQRT, "Gamma", true);

	Params[2 + i] = Params[i];	 // Refl
	Params[2 + i].var[3] = DefVar(0.2f, 5.0f, SQRT, "Gamma", true);

	Params[3 + i] = Params[i];  // Regn
	Params[3 + i].var[3] = DefVar(0.2f, 5.0f, SQRT, "Gamma", true);

	Params[4 + i] = Params[i];  // Transl
	Params[4 + i].var[3] = DefVar(0.2f, 5.0f, SQRT, "???");

	Params[5 + i] = Params[i];  // Transm
	Params[5 + i].var[3] = DefVar(0.2f, 5.0f, SQRT, "???");

	Params[6 + i] = Params[i];	 // Spec
	Params[6 + i].var[3] = DefVar(0.1f, 9.9f, SQRT, "Power", false);
}


// =============================================================================================
//
void SetTuningValue(int idx, D3DCOLORVALUE *pClr, DWORD clr, float value)
{
	bool bExtend = (DlgItem<QAbstractButton>(hDlg, IDC_DBG_EXTEND)->isChecked());

	float mi = Params[idx].var[clr].min;
	float mx = (bExtend ? Params[idx].var[clr].extmax : Params[idx].var[clr].max);

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
	bool bExtend = (DlgItem<QAbstractButton>(hDlg, IDC_DBG_EXTEND)->isChecked());
	return CLAMP(value, Params[p].var[v].min, (bExtend ? Params[p].var[v].extmax : Params[p].var[v].max));
}

// =============================================================================================
//
void UpdateShader()
{
	OBJHANDLE hObj = vObj->GetObject();

	if (!oapiIsVessel(hObj)) return;

	D3D9Mesh *hMesh = (D3D9Mesh *)vObj->GetMesh(sMesh);

	if (!hMesh) return;

	DWORD Shader = DWORD(DlgItem<QComboBox>(hDlg, IDC_DBG_DEFSHADER)->currentIndex());

	vVessel *vVes = (vVessel *)vObj;
	MatMgr *pMgr = vVes->GetMaterialManager();
	
	switch (Shader) {
	case 0:
		hMesh->SetDefaultShader(SHADER_NULL);
		pMgr->RegisterShaderChange(hMesh, SHADER_NULL);
		break;
	case 1:
		hMesh->SetDefaultShader(SHADER_METALNESS);
		pMgr->RegisterShaderChange(hMesh, SHADER_METALNESS);
		break;
	}

	InitMatList(hMesh->GetDefaultShader());
}

// =============================================================================================
//
void UpdateMeshMaterial(float value, DWORD MatPrp, DWORD clr)
{
	OBJHANDLE hObj = vObj->GetObject();

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

		case 5:	// Smoothness
		{
			Mat.ModFlags |= D3D9MATEX_ROUGHNESS;
			Mat.Roughness[clr] = _Clamp(value, MatPrp, clr);
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

		case 8:	// Metalness
		{
			Mat.ModFlags |= D3D9MATEX_METALNESS;
			Mat.Metalness = _Clamp(value, MatPrp, clr);
			break;
		}

		case 9:	// SpecialFX
		{
			Mat.ModFlags |= D3D9MATEX_SPECIALFX;
			Mat.SpecialFX[clr] = _Clamp(value, MatPrp, clr);
			break;
		}

		case 11:	// Tune Albedo
		{
			SetTuningValue(MatPrp, &Tune.Albedo, clr, value);
			break;
		}

		case 12:	// Tune Emis
		{
			SetTuningValue(MatPrp, &Tune.Emis, clr, value);
			break;
		}

		case 13:	// Tune Refl
		{
			SetTuningValue(MatPrp, &Tune.Refl, clr, value);
			break;
		}

		case 14:	// Tune _Rghn
		{
			SetTuningValue(MatPrp, &Tune.Rghn, clr, value);
			break;
		}

		case 15:	// Tune _Transl
		{
			SetTuningValue(MatPrp, &Tune.Transl, clr, value);
			break;
		}

		case 16:	// Tune _Transm
		{
			SetTuningValue(MatPrp, &Tune.Transm, clr, value);
			break;
		}

		case 17:	// Tune _Spec
		{
			SetTuningValue(MatPrp, &Tune.Spec, clr, value);
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
		case 8:	return D3D9MATEX_METALNESS;
		case 9:	return D3D9MATEX_SPECIALFX;
	}
	return 0;
}


// =============================================================================================
//
bool IsMaterialModified(DWORD MatPrp)
{
	OBJHANDLE hObj = vObj->GetObject();

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
	OBJHANDLE hObj = vObj->GetObject();

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
	OBJHANDLE hObj = vObj->GetObject();

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
				case 0: return pMat->Roughness.x;
				case 1: return pMat->Roughness.y;
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

		case 8:	// Metalness
		{
			switch (clr) {
			case 0: return pMat->Metalness;
			}
			break;
		}

		case 9:	// SpecialFX
		{
			switch (clr) {
				case 0: return pMat->SpecialFX.x;
				case 1: return pMat->SpecialFX.y;
				case 2: return pMat->SpecialFX.z;
				case 3: return pMat->SpecialFX.w;
			}
			break;
		}

		case 11:	// Tune Albedo
		{
			return GetTuningValue(MatPrp, &Tune.Albedo, clr);
		}

		case 12:	// Tune Emis
		{
			return GetTuningValue(MatPrp, &Tune.Emis, clr);
		}

		case 13:	// Tune Refl
		{
			return GetTuningValue(MatPrp, &Tune.Refl, clr);
		}

		case 14:	// Tune _Rghn
		{
			return GetTuningValue(MatPrp, &Tune.Rghn, clr);
		}

		case 15:	// Tune _Transl
		{
			return GetTuningValue(MatPrp, &Tune.Transl, clr);
		}

		case 16:	// Tune _Transm
		{
			return GetTuningValue(MatPrp, &Tune.Transm, clr);
		}

		case 17:	// Tune _Spec
		{
			return GetTuningValue(MatPrp, &Tune.Spec, clr);
		}
	}

	return 0.0f;
}

// =============================================================================================
//
void SetColorSlider()
{
	DWORD MatPrp = DropdownList(DWORD(DlgItem<QComboBox>(hDlg, IDC_DBG_MATPRP)->currentIndex()));
	bool bExtend = (DlgItem<QAbstractButton>(hDlg, IDC_DBG_EXTEND)->isChecked());

	float mi = Params[MatPrp].var[SelColor].min;
	float mx = (bExtend ? Params[MatPrp].var[SelColor].extmax : Params[MatPrp].var[SelColor].max);

	float val = GetMaterialValue(MatPrp, SelColor);

	val -= mi; val /= (mx - mi);

	if (Params[MatPrp].var[SelColor].Scl == scale::SQRT) val = sqrt(val);
	if (Params[MatPrp].var[SelColor].Scl == scale::SQR) val = val*val;

	QSignalBlocker block(DlgItem<QSlider>(hDlg, IDC_DBG_MATADJ)); // TBM_SETPOS doesn't send WM_HSCROLL
	DlgItem<QSlider>(hDlg, IDC_DBG_MATADJ)->setValue(WORD(val*255.0f));
}

// =============================================================================================
//
void DisplayMat(bool bRed, bool bGreen, bool bBlue, bool bAlpha)
{
	char lbl[32];

	DWORD MatPrp = DropdownList(DWORD(DlgItem<QComboBox>(hDlg, IDC_DBG_MATPRP)->currentIndex()));
	
	float r = GetMaterialValue(MatPrp, 0);
	float g = GetMaterialValue(MatPrp, 1);
	float b = GetMaterialValue(MatPrp, 2);
	float a = GetMaterialValue(MatPrp, 3);

	if (bRed) snprintf(lbl,32,"%3.3f", r);
	else	  snprintf(lbl,32,"%s","");
	oapiSetDlgItemText(hDlg, IDC_DBG_RED, lbl);
	
	if (bGreen) snprintf(lbl,32,"%3.3f", g);
	else		snprintf(lbl,32,"%s","");
	oapiSetDlgItemText(hDlg, IDC_DBG_GREEN, lbl);

	if (bBlue) snprintf(lbl,32,"%3.3f", b);
	else	   snprintf(lbl,32,"%s","");
	oapiSetDlgItemText(hDlg, IDC_DBG_BLUE, lbl);

	if (bAlpha) snprintf(lbl,32,"%3.3f", a);
	else	    snprintf(lbl,32,"%s","");
	oapiSetDlgItemText(hDlg, IDC_DBG_ALPHA, lbl);

	if (bRed)   oapiResDlgItem(hDlg, IDC_DBG_RED)->setEnabled(true);
	else	    oapiResDlgItem(hDlg, IDC_DBG_RED)->setEnabled(false);	
	if (bGreen) oapiResDlgItem(hDlg, IDC_DBG_GREEN)->setEnabled(true);
	else		oapiResDlgItem(hDlg, IDC_DBG_GREEN)->setEnabled(false);	
	if (bBlue)  oapiResDlgItem(hDlg, IDC_DBG_BLUE)->setEnabled(true);
	else	    oapiResDlgItem(hDlg, IDC_DBG_BLUE)->setEnabled(false);	
	if (bAlpha) oapiResDlgItem(hDlg, IDC_DBG_ALPHA)->setEnabled(true);
	else		oapiResDlgItem(hDlg, IDC_DBG_ALPHA)->setEnabled(false);

	bool bModified = IsMaterialModified(MatPrp);

	DlgItem<QAbstractButton>(hDlg, IDC_DBG_DEFINED)->setChecked(bModified);
}

// =============================================================================================
//
void UpdateMaterialDisplay(bool bSetup)
{
	char lbl[256];
	char lbl2[64];

	OBJHANDLE hObj = vObj->GetObject();
	if (!oapiIsVessel(hObj)) return;
	
	D3D9Mesh *hMesh = (D3D9Mesh *)vObj->GetMesh(sMesh);
	if (!hMesh) return;

	WORD Shader = hMesh->GetDefaultShader();
	if (Shader == SHADER_NULL) DlgItem<QComboBox>(hDlg, IDC_DBG_DEFSHADER)->setCurrentIndex(0);
	if (Shader == SHADER_METALNESS) DlgItem<QComboBox>(hDlg, IDC_DBG_DEFSHADER)->setCurrentIndex(1);

	DWORD matidx = hMesh->GetMeshGroupMaterialIdx(sGroup);

	// Set material info
	const char *skin = NULL;
	if (skin)	snprintf(lbl, 256, "Material %u: [Skin %s]", matidx, skin);
	else		snprintf(lbl, 256, "Material %u:", matidx);

	oapiGetDlgItemText(hDlg, IDC_DBG_MATGRP, lbl2, 64);
	if (strcmp(lbl, lbl2)) oapiSetDlgItemText(hDlg, IDC_DBG_MATGRP, lbl); // Avoid causing flashing

	if (bSetup) SelColor = 0;

	DWORD MatPrp = DropdownList(DWORD(DlgItem<QComboBox>(hDlg, IDC_DBG_MATPRP)->currentIndex()));
	
	DisplayMat(Params[MatPrp].var[0].bUsed, Params[MatPrp].var[1].bUsed, Params[MatPrp].var[2].bUsed, Params[MatPrp].var[3].bUsed);

	SetToolTip(IDC_DBG_RED, hTipRed, Params[MatPrp].var[0].tip);
	SetToolTip(IDC_DBG_GREEN, hTipGrn, Params[MatPrp].var[1].tip);
	SetToolTip(IDC_DBG_BLUE, hTipBlu, Params[MatPrp].var[2].tip);
	SetToolTip(IDC_DBG_ALPHA, hTipAlp, Params[MatPrp].var[3].tip);

	DWORD texidx = hMesh->GetMeshGroupTextureIdx(sGroup);

	if (texidx==0) oapiSetDlgItemText(hDlg, IDC_DBG_TEXTURE, "Texture: None");
	else {
		SURFHANDLE hSrf = hMesh->GetTexture(texidx);
		if (hSrf) {
			snprintf(lbl, 256, "Texture: %s [%u]", RemovePath(SURFACE(hSrf)->GetName()), texidx);
			oapiSetDlgItemText(hDlg, IDC_DBG_TEXTURE, lbl);
		}
	}

	snprintf(lbl, 256, "Mesh: %s", RemovePath(hMesh->GetName()));
	oapiSetDlgItemText(hDlg, IDC_DBG_MESHNAME, lbl);
	
	oapiGetDlgItemText(hDlg, IDC_DBG_MESHGRP, lbl2, 64);
	if (strcmp(lbl, lbl2)) oapiSetDlgItemText(hDlg, IDC_DBG_MESHGRP, lbl); // Avoid causing flashing
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
	
	DWORD MatPrp = DropdownList(DWORD(DlgItem<QComboBox>(hDlg, IDC_DBG_MATPRP)->currentIndex()));	

	bool bLink = (DlgItem<QAbstractButton>(hDlg, IDC_DBG_LINK)->isChecked());
	bool bExtend = (DlgItem<QAbstractButton>(hDlg, IDC_DBG_EXTEND)->isChecked());

	float mi = Params[MatPrp].var[SelColor].min;
	float mx = (bExtend ? Params[MatPrp].var[SelColor].extmax : Params[MatPrp].var[SelColor].max);

	if (MatPrp==5 || MatPrp==6) bLink = false;	// Roughness, Fresnel
	if (SelColor==3) bLink = false;				// Alpha, Specular power

	if (Params[MatPrp].var[SelColor].Scl == scale::SQRT) val = (val*val);
	if (Params[MatPrp].var[SelColor].Scl == scale::SQR) val = sqrt(val);

	val *= (mx - mi); val += mi;

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

void SetPickPos(D3DXVECTOR3 pos)
{
	PickLocation = pos;
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

	oapiSetDlgItemText(hDlg, IDC_DBG_VISUAL, visual);

	if (nMesh!=0) {
		if (sMesh>0xFFFF) sMesh = nMesh-1;
		if (sMesh>=nMesh) sMesh = 0;
	}
	else {
		sMesh=0, sGroup=0, nGroup=0;
		oapiSetDlgItemText(hDlg, IDC_DBG_MESH, "N/A");
		oapiSetDlgItemText(hDlg, IDC_DBG_GROUP, "N/A");
		return;
	}

	snprintf(lbl,256,"%u/%u",sMesh,nMesh-1);
	oapiSetDlgItemText(hDlg, IDC_DBG_MESH, lbl);

	D3D9Mesh *mesh = (class D3D9Mesh *)vObj->GetMesh(sMesh);

	if (mesh) nGroup = mesh->GetGroupCount();
	else	  nGroup = 0;

	if (nGroup!=0) {
		if (sGroup>0xFFFF)  sGroup = nGroup-1;
		if (sGroup>=nGroup) sGroup = 0;
	}
	else {
		sGroup=0;
		oapiSetDlgItemText(hDlg, IDC_DBG_GROUP, "N/A");
		return;
	}

	snprintf(lbl,256,"%u/%u",sGroup,nGroup-1);
	oapiSetDlgItemText(hDlg, IDC_DBG_GROUP, lbl);

	UpdateMaterialDisplay();
	SetColorSlider();
	InitMatList(mesh->GetDefaultShader());
}

// =============================================================================================
//
double GetVisualSize()
{
	if (hDlg && vObj) {
		OBJHANDLE hObj = vObj->GetObject();
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
	snprintf(visual, 64, "Visual: %s", vObj->GetName());
	SetupMeshGroups();

	DlgItem<QComboBox>(hDlg, IDC_DBG_CONES)->clear();
	Emitters.clear();

	if (vObj->Type() == OBJTP_VESSEL) {

		oapiComboAddString(DlgItem<QComboBox>(hDlg, IDC_DBG_CONES), "NONE");
		Emitters[0] = NULL;

		char line[64] = ""; // upstream left it uninitialised for directional emitters before strcat_s

		vVessel *vV = static_cast<vVessel*>(vObj);
		VESSEL *vessel = vV->GetInterface();
		DWORD nemitter = vessel->LightEmitterCount();

		for (DWORD j = 0; j < nemitter; j++) {

			const LightEmitter *em = vessel->GetLightEmitter(j);
			
			if (em->GetType() == LightEmitter::LT_SPOT) {
				const SpotLight *sl = static_cast<const SpotLight*>(em);
				double P = sl->GetPenumbra()*DEG;
				double U = sl->GetUmbra()*DEG;
				double R = sl->GetRange();
				snprintf(line, 64, "%s P%1.0f U%1.0f R%1.0f", _PTR(em), P, U, R);
			}	

			if (em->GetType() == LightEmitter::LT_POINT) {
				const PointLight *pl = static_cast<const PointLight*>(em);
				double R = pl->GetRange();
				snprintf(line, 64, "%s R%1.0f", _PTR(em), R);
			}

			switch (em->GetVisibility())
			{
			case LightEmitter::VIS_EXTERNAL: strncat(line, " EXT", 63 - strlen(line)); break;
			case LightEmitter::VIS_COCKPIT: strncat(line, " VC", 63 - strlen(line)); break;
			case LightEmitter::VIS_ALWAYS: strncat(line, " ALW", 63 - strlen(line)); break;
			}

			if ((em->GetType() == LightEmitter::LT_SPOT) || (em->GetType() == LightEmitter::LT_POINT)) {
				oapiComboAddString(DlgItem<QComboBox>(hDlg, IDC_DBG_CONES), line);
				Emitters[j + 1] = em;
			}
		}
	}

	DlgItem<QComboBox>(hDlg, IDC_DBG_CONES)->setCurrentIndex(0);
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
	DWORD MatPrp = DropdownList(DWORD(DlgItem<QComboBox>(hDlg, IDC_DBG_MATPRP)->currentIndex()));
	UpdateMeshMaterial(float(atof(lbl)), MatPrp, SelColor);
	SetColorSlider();
}

// =============================================================================================
//
float reduce(float v, float q)
{
	if (v>0) return max(0.0f, v-q);
	else     return min(0.0f, v+q);
}

struct PCParam {
	DWORD Action;
	DWORD Func;
	DWORD Mip;
	float a,b,c;
};

// =============================================================================================
//
D3DXCOLOR ProcessColor(D3DXVECTOR4 C, PCParam *prm, int x, int y)
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
		D3DXVECTOR4 e = -abs(C)*fMip; // named: VectorHelpers' pow takes a non-const reference (MSVC bound the temporary)
		C *= pow(a, e);

		float k = b * fMip;

		C = D3DXVECTOR4(reduce(C.x, k), reduce(C.y, k), reduce(C.z, k), reduce(C.w, k));
		C = C*0.5f + 0.5f;			// Back to [0, 1]
	}

	return D3DXCOLOR(C.x, C.y, C.z, C.w);
}

// =============================================================================================
//
bool Execute(QWidget *hWnd, const char *file)
{
	VkDev *pDevice = g_client->GetDevice();

	// SaveTex.hwndOwner: hWnd is the parent of the save dialog below

	// D3DLOCKED_RECT in, out left out: the images are VkPixels in memory (D3DPOOL_SYSTEMMEM)
	PCParam prm;

	DWORD Func = 0;
	DWORD Action = DWORD(DlgItem<QComboBox>(hDlg, IDC_DBG_ACTION)->currentIndex());
	DWORD Target = DWORD(DlgItem<QComboBox>(hDlg, IDC_DBG_TARGET)->currentIndex());

	if (DlgItem<QAbstractButton>(hDlg, IDC_DBG_NORM)->isChecked()) Func |= 0x1;
	if (DlgItem<QAbstractButton>(hDlg, IDC_DBG_FADE)->isChecked()) Func |= 0x2;
	if (DlgItem<QAbstractButton>(hDlg, IDC_DBG_SEAMS)->isChecked()) Func |= 0x4;
	
	if (Action>=0 && Action<=2) {

		VkPixels *pTex = NULL;
		VkPixels *pWork = NULL;
		VkPixels *pSave = NULL;
		VkImageInfo info;

		// D3DXCreateTextureFromFileEx into system memory: the file as A8R8G8B8 with a full mip chain
		VkPixels src;
		if (VkGetImageInfoFromFile(file, &info) && VkLoadPixels(file, src)) {
			pTex = new VkPixels;
			if (!VkConvertPixels(src, *pTex, VK_FORMAT_B8G8R8A8_UNORM, SWZ_NONE, 0, 0, 0)) SAFE_DELETE(pTex);
		}

		if (!pTex) {
			LogErr("Failed to open a file [%s]", file); 
			return false;
		}

		DWORD mips = pTex->levels;

		if (true) {

			pWork = new VkPixels(*pTex); // D3DXCreateTexture A8R8G8B8: the levels of pTex, overwritten below
			if (!pWork) return false;

			// D3DXCreateTexture: the format only, VkConvertPixels fills the levels below
			if (Action==0) { pSave = new VkPixels; pSave->fmt = VK_FORMAT_BC3_UNORM_BLOCK; }
			if (Action==1) { pSave = new VkPixels; pSave->fmt = VK_FORMAT_B8G8R8A8_UNORM; }
			if (Action==2) { pSave = new VkPixels; pSave->fmt = VK_FORMAT_A4R4G4B4_UNORM_PACK16; }

			if (!pSave) return false;

			
			// Process texture ----------------------------------------------
			//
			for (DWORD n=0;n<mips;n++) {

				D3DXCOLOR seam;

				DWORD w = info.Width>>n;
				DWORD h = info.Height>>n;
				// LockRect: the levels are in memory, rows packed (B8G8R8A8 is A8R8G8B8 as DWORDs)
				DWORD *pIn  = (DWORD *)pTex->Level(n).data();
				DWORD *pOut = (DWORD *)pWork->Level(n).data();

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


				// UnlockRect left out: the pixels are in memory
			}

			SAFE_DELETE(pTex);

			// Convert texture format --------------------------------------
			//
			// D3DXLoadSurfaceFromSurface of each level → one conversion of all levels (same sizes: nothing to filter)
			if (!VkConvertPixels(*pWork, *pSave, pSave->fmt, SWZ_NONE, 0, 0, mips)) {
				LogErr("VkConvertPixels Failed");
				return false;
			}
			SAFE_DELETE(pWork);
		}
		else {
			// Copy input to output	
			pSave = pTex;
		}

		// Save the texture into a file -------------------------------
		//
		if (Target==0) {
			snprintf(SaveFileName, sizeof(SaveFileName), "%s", OpenFileName);
			QString save = QFileDialog::getSaveFileName(hWnd, QString(), QString::fromUtf8(SaveFileName), "*.dds"); // GetSaveFileName
			snprintf(SaveFileName, sizeof(SaveFileName), "%s", save.toUtf8().constData());
			if (!save.isEmpty()) {
				if (!VkSavePixels(SaveFileName, VKIFF_DDS, *pSave)) {
					LogErr("Failed to create a file [%s]",SaveFileName); 
					return false;
				}
				SAFE_DELETE(pSave);
				return true;
			}
		}

		// Assign the texture for rendering -----------------------------
		//
		if (Target==1 || Target==2 || Target==3) {
			vPlanet *vP = g_client->GetScene()->GetCameraProxyVisual();	
			VkTex *pSrc = VkCreateTexture(pDevice, *pSave, VK_IMAGE_USAGE_SAMPLED_BIT | VK_IMAGE_USAGE_TRANSFER_SRC_BIT); // the texture SetMicroTexture copies
			if (vP && pSrc) vP->SetMicroTexture(pSrc, Target-1);
			SAFE_DELETE(pSrc);
			SAFE_DELETE(pSave);
			return true;
		}
	}

	return false;
}
			

// =============================================================================================
//
void SaveEnvMap()
{
	VkDev *pDevice = g_client->GetDevice();

	OBJHANDLE hObj = vObj->Object();

	if (oapiIsVessel(hObj)) {
		vVessel *vVes = (vVessel *)vObj;
		VkTex *pTex = vVes->GetEnvMap(ENVMAP_MAIN);
		VkPixels px; // D3DXSaveTextureToFile: all faces and levels read back, then written as DDS
		if (!pTex || !VkReadPixels(pDevice, pTex, px, 0) || !VkSavePixels("EnvMap.dds", VKIFF_DDS, px)) {
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
	vsnprintf(buf, 256, format, args);
	va_end(args);
	buffer += buf;
}

//-------------------------------------------------------------------------------------------
//
void Append2(const char *format, ...)
{
	if (hDataWnd == NULL) return;
	char buf[256];
	va_list args;
	va_start(args, format);
	vsnprintf(buf, 256, format, args);
	va_end(args);
	buffer2 += buf;
}

//-------------------------------------------------------------------------------------------
//
void Refresh2()
{
	if (hDataWnd == NULL) return;

	Append2("LocalPos = [%f, %f, %f]", PickLocation.x, PickLocation.y, PickLocation.z);

	oapiSetDlgItemText(hDataWnd, IDC_DBG_DATAVIEW2, buffer2.c_str());
	buffer2.clear();
}

//-------------------------------------------------------------------------------------------
//
void Refresh()
{
	if (hDataWnd == NULL) return;
	oapiSetDlgItemText(hDataWnd, IDC_DBG_DATAVIEW, buffer.c_str());
	buffer.clear();
	Refresh2();
}

//-------------------------------------------------------------------------------------------
//
void CreateSamplingKernel()
{
	int s = 27;
	int k = 0;
	
	VECTOR3 *data = new VECTOR3[s];
	
	double d = 0.0;
	double a = 0.0;
	
	for (int i = 0; i < s; i++) {	
		double z = (oapiRand()*2.0 - 1.0) * (PI2 / 8);
		data[i].x = sqrt(d) * sin(a+z);
		data[i].y = sqrt(d) * cos(a+z);
		//data[i].x = d * sin(a + z);
		//data[i].y = d * cos(a + z);
		data[i].z = sqrt(data[i].x*data[i].x + data[i].y*data[i].y);
		data[i].z = sqrt(data[i].z);
		data[i].z = 1.0;
		a += PI2 / 4;
		d += 1.0 / double(s);
	}

	for (int i = 0; i < s; i++) {
		oapiWriteLogV("{%4.4ff, %4.4ff, %4.4ff},", data[i].x, data[i].y, data[i].z);
	}

	delete[]data;
}

/*
//-------------------------------------------------------------------------------------------
//
void CreateSamplingKernel()
{
VECTOR3 *data = new VECTOR3[64];

while (true) {

double ava = 0, avb = 0;

for (int i = 0; i < 64; i++) {

double a = oapiRand();
double b = oapiRand() * PI2;
double c = cos(a*PI05);

data[i] = _V(sin(b)*a, cos(b)*a, c);

ava += data[i].x;
avb += data[i].y;
}

// Ensure proper balance
if (ava < 0.1 && avb < 0.1) break;
}

double w = 0.0f;

for (int i = 0; i < 64; i++) {
oapiWriteLogV("{%4.4ff, %4.4ff, %4.4ff},", data[i].x, data[i].y, data[i].z);
w += data[i].z;
}

oapiWriteLogV("TotalWeight = %f", w);
}
*/


// ==============================================================
// Dialog message handler

void ViewProc(QWidget *hWnd, void *context)
{
	static bool isOpen = false; // IDC_DBG_MORE (full or reduced width)

	// DWORD Prp (IDC_DBG_MATPRP selection) left out: unused here, and hDlg may be closed while this window is open
	
	// WM_INITDIALOG
	{
		oapiSetDlgItemText(hWnd, IDC_DBG_DATAVIEW, "-- Select a mesh group --");
		buffer.clear();
		// All Init actions are done in OpenDlgClbk();
	}

	// not upstream: the core destroys the window at session end without IDCANCEL; a stale HWND was harmless, a QWidget* isn't
	QObject::connect(hWnd, &QObject::destroyed, [hWnd]() { if (hDataWnd == hWnd) hDataWnd = NULL; });

	// WM_COMMAND
	auto command = [hWnd](int id, int code, QWidget *hCtrl) {

		switch (id) {

		case IDCANCEL:
			oapiCloseDialog(hWnd);
			if (!buffer.empty()) buffer.clear();
			hDataWnd = NULL;
			return;
		}
	};
	oapiConnectDlgCommands(hWnd, command);
	new DlgEvents(hWnd, QEvent::Close, [command]() { command(IDCANCEL, RESN_CLICKED, NULL); });

	// oapiDefDialogProc left out: oapiOpenDialog wires the default dialog behaviour
}


// ==============================================================
// Dialog message handler

void WndProc(QWidget *hWnd, void *context)
{
	static bool isOpen = false; // IDC_DBG_MORE (full or reduced width)

	// OpenTex.hwndOwner: hWnd is the parent of the file dialog below

	// WM_INITDIALOG
	{
		isOpen = false; // We always start with reduced width
		// All Init actions are done in OpenDlgClbk();
	}

	// not upstream: the core destroys the dialog at session end without IDCANCEL; a stale HWND was harmless, a QWidget* isn't
	QObject::connect(hWnd, &QObject::destroyed, [hWnd]() { if (hDlg == hWnd) hDlg = NULL; });

	// WM_HSCROLL
	for (int id : {IDC_DBG_SPEED, IDC_DBG_RESBIAS, IDC_DBG_MATADJ}) {
		QSlider *tb = DlgItem<QSlider>(hWnd, id);
		QObject::connect(tb, &QSlider::valueChanged, hWnd, [hWnd, tb](int value) { // TB_THUMBTRACK, TB_ENDTRACK
			char lbl[32];
			WORD pos = WORD(value);

			if (tb==DlgItem<QSlider>(hWnd, IDC_DBG_SPEED)) {
				if (pos==0) pos = WORD(DlgItem<QSlider>(hDlg, IDC_DBG_SPEED)->value());
				double fpos = pow(2.0,double(pos)*13.0/200.0); 
				snprintf(lbl,32,"%1.0f",fpos);
				oapiSetDlgItemText(hWnd, IDC_DBG_SPEEDDSP, lbl);
				camSpeed = fpos/50.0;
			}

			if (tb == DlgItem<QSlider>(hWnd, IDC_DBG_RESBIAS)) {
				resbias = 4.0 + 0.2 * double(DlgItem<QSlider>(hDlg, IDC_DBG_RESBIAS)->value());
			}

			if (tb==DlgItem<QSlider>(hWnd, IDC_DBG_MATADJ)) {
				if (pos==0) pos = WORD(DlgItem<QSlider>(hDlg, IDC_DBG_MATADJ)->value());
				UpdateColorSlider(pos);
				UpdateMaterialDisplay();
			}
		});
	}

	// WM_COMMAND
	auto command = [hWnd](int id, int code, QWidget *hCtrl) {
		char lbl[32];
		bool bPaused;

		if (!hDlg) return; // not upstream: a closing dialog still reports focus changes (Win32 sent them to a NULL handle, harmless)

		DWORD Prp = DropdownList(DWORD(DlgItem<QComboBox>(hDlg, IDC_DBG_MATPRP)->currentIndex()));

		switch (id) {

			case IDCANCEL:
				Close();
				break;
		
			case IDC_DBG_KERNEL:
			{
				CreateSamplingKernel();
				break;
			}

			case IDC_DBG_MATSAVE:
			{
				OBJHANDLE hObj = vObj->GetObject();
				if (oapiIsVessel(hObj)) {
					vVessel *vVes = (vVessel *)vObj;
					vVes->GetMaterialManager()->SaveConfiguration();
				}
				break;
			}

			case IDC_DBG_DATAWND:
			{
				QWidget *hW = oapiOpenDialog(g_hInst, IDD_DEBUGVIEW, ViewProc);
				if (hW) hDataWnd = hW;
			}
				break;

			case IDC_DBG_LINK:
				break;

			case IDC_DBG_DEFINED:
				SetMaterialModified(Prp, (DlgItem<QAbstractButton>(hDlg, IDC_DBG_DEFINED)->isChecked()));
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
				if (code==RESN_SETFOCUS) {
					SelColor = 0;
					UpdateMaterialDisplay();
					SetColorSlider();
				}
				if (code==RESN_KILLFOCUS) {
					oapiGetDlgText(hCtrl, lbl, 32);
					SetColorValue(lbl);
				}
				break;

			case IDC_DBG_GREEN:
				if (code==RESN_SETFOCUS) {
					SelColor = 1;
					UpdateMaterialDisplay();
					SetColorSlider();
				}
				if (code==RESN_KILLFOCUS) {
					oapiGetDlgText(hCtrl, lbl, 32);
					SetColorValue(lbl);
				}
				break;

			case IDC_DBG_BLUE:
				if (code==RESN_SETFOCUS) {
					SelColor = 2;
					UpdateMaterialDisplay();
					SetColorSlider();
				}
				if (code==RESN_KILLFOCUS) {
					oapiGetDlgText(hCtrl, lbl, 32);
					SetColorValue(lbl);
				}
				break;

			case IDC_DBG_ALPHA:
				if (code==RESN_SETFOCUS) {
					SelColor = 3;
					UpdateMaterialDisplay();
					SetColorSlider();
				}
				if (code==RESN_KILLFOCUS) {
					oapiGetDlgText(hCtrl, lbl, 32);
					SetColorValue(lbl);
				}
				break;

			case IDC_DBG_MATPRP:
				if (code==RESN_SELCHANGE) {
					UpdateMaterialDisplay(true);
					SetColorSlider();	
				}
				break;

			case IDC_DBG_CONES:
				if (code == RESN_SELCHANGE) {
					sEmitter = DWORD(DlgItem<QComboBox>(hDlg, IDC_DBG_CONES)->currentIndex());
				}
				break;

			case IDC_DBG_DEFSHADER:
				if (code == RESN_SELCHANGE) {
					UpdateShader();
				}
				break;

			case IDC_DBG_DISPLAY:
				if (code==RESN_SELCHANGE) dspMode = DWORD(DlgItem<QComboBox>(hWnd, IDC_DBG_DISPLAY)->currentIndex());
				break;

			case IDC_DBG_CAMERA:
				if (code==RESN_SELCHANGE) camMode = DWORD(DlgItem<QComboBox>(hWnd, IDC_DBG_CAMERA)->currentIndex());
				break;

			case IDC_DBG_MSHUP: 
				if (code==RESN_CLICKED) sMesh--;
				SetupMeshGroups();
				break;

			case IDC_DBG_MSHDN: 
				if (code==RESN_CLICKED) sMesh++;
				SetupMeshGroups();
				break;

			case IDC_DBG_GRPUP: 
				if (code==RESN_CLICKED) sGroup--;
				SetupMeshGroups();
				break;

			case IDC_DBG_GRPDN: 
				if (code==RESN_CLICKED) sGroup++;	
				SetupMeshGroups();
				break;

			
			case IDC_DBG_MESH: 
				if (code==RESN_KILLFOCUS) {
					char cbuf[32];
					oapiGetDlgItemText(hWnd, IDC_DBG_MESH, cbuf, 32); 
					for (int i=0; i<32;i++) if (cbuf[i]==0 || cbuf[i]=='/') { cbuf[i]=0; break; }
					sMesh = atoi(cbuf);
					SetupMeshGroups();
				}
				break;

			case IDC_DBG_GROUP: 
				if (code==RESN_KILLFOCUS) {
					char cbuf[32];
					oapiGetDlgItemText(hWnd, IDC_DBG_GROUP, cbuf, 32);
					for (int i=0; i<32;i++) if (cbuf[i]==0 || cbuf[i]=='/') { cbuf[i]=0; break; }
					sGroup = atoi(cbuf);
					SetupMeshGroups();
				}
				break;

			case IDC_DBG_OPEN:
			{
				bPaused = oapiGetPause();
				oapiSetPause(true);
				// GetOpenFileName (OpenTex): folder "Textures" unless a file was chosen before
				QString open = QFileDialog::getOpenFileName(hWnd, QString(), OpenFileName[0] ? QString::fromUtf8(OpenFileName) : QString("Textures"), "*.dds *.jpg *.png *.hdr *.bmp *.tga");
				if (!open.isEmpty()) {
					snprintf(OpenFileName, sizeof(OpenFileName), "%s", open.toUtf8().constData());
					oapiSetDlgItemText(hWnd, IDC_DBG_FILE, OpenFileName);
				}
				oapiSetPause(bPaused);
			}
				break;

			case IDC_DBG_EXECUTE:
				bPaused = oapiGetPause();
				oapiSetPause(true);
				if (Execute(hWnd, OpenFileName)==false) QMessageBox(QMessageBox::NoIcon, "Vulkan Controls", "Failed :(", QMessageBox::Ok, hWnd).exec(); // not upstream: Vulkan in place of D3D9
				oapiSetPause(bPaused);
				break;

			case IDC_DBG_ACTION:
				break;

			case IDC_DBG_MORE:
				// GetWindowRect/SetWindowPos: the dialog's own size (NarrowWidth is upstream's 298 px)
				hDlg->resize(isOpen ? NarrowWidth() : origwidth, hDlg->height());
				hDlg->show(); // SWP_SHOWWINDOW
				oapiSetDlgItemText(hWnd, IDC_DBG_MORE, isOpen ? ">>>" : "<<<");
				isOpen = !isOpen;
				break;

			case IDC_DBG_RELOADSHD:
				D3D9Effect::D3D9TechInit(g_client, g_client->GetDevice(), "VulkanClient");
				break;

			case IDC_DBG_RELOADTEX:
				if (vObj) {
					if (vObj->Type() == OBJTP_VESSEL) {
						((vVessel *)vObj)->ReloadTextures();
					}
				}
				break;

			case IDC_DBG_ENVSAVE:
				SaveEnvMap();
				break;

			case IDC_DBG_EXTEND:
				SetColorSlider();
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
				LogErr("LOWORD(%hu), HIWORD(0x%hX)",WORD(id),WORD(code));
				break;
		}
	};
	oapiConnectDlgCommands(hWnd, command);
	for (int id : {IDC_DBG_RED, IDC_DBG_GREEN, IDC_DBG_BLUE, IDC_DBG_ALPHA}) { // EN_SETFOCUS
		QWidget *hCtrl = oapiResDlgItem(hWnd, id);
		new DlgEvents(hCtrl, QEvent::FocusIn, [command, id, hCtrl]() { command(id, RESN_SETFOCUS, hCtrl); });
	}
	new DlgEvents(hWnd, QEvent::Close, [command]() { command(IDCANCEL, RESN_CLICKED, NULL); });

	// oapiDefDialogProc left out: oapiOpenDialog wires the default dialog behaviour
}

// =============================================================================================
//
void OpenGFXDlgClbk(void *context)
{
	GFXDialog *gfx = (GFXDialog *)context;
	oapiOpenDialog(gfx);
}

} //namespace


