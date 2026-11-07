// ===========================================================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2012-2026 Jarmo Nikkanen
// ===========================================================================================


#include "D3D9Client.h"
#include "D3D9Config.h"
#include "D3D9Surface.h"
#include "DebugControls.h"
#include "vObject.h"
#include "vVessel.h"
#include "vPlanet.h"
#include "Mesh.h"
#include "MaterialMgr.h"
#include "VectorHelpers.h"
#include "OrbiterPlatform.h"
#include <stdio.h>
#include <stdarg.h>
#include <filesystem>
#include <imgui.h>
#include <imgui_extras.h>

enum scale { LIN, SQRT, SQR };

using namespace oapi;
using std::min;
using std::max;

extern D3D9Client *g_client;

// Little binary helper
#define SETFLAG(bitmap, bit, value) (value ? bitmap |= bit : bitmap &= ~bit)
#define CLAMP(x,a,b) min(max(a,x),b)

namespace DebugControls {

DWORD dwGFX, dwCol, dwCmd, nMesh, nGroup, sMesh, sGroup, debugFlags, dspMode, camMode, SelColor, sEmitter;
double camSpeed;
float cpr, cpg, cpb, cpa;
double resbias = 4.0;
char visual[64];
GFXDialog *gfxDlg;
class ColDialog *colDlg;
class DebugDialog *dbgDlg;
class DataDialog *dataDlg;
vObject *vObj = NULL;
std::string buffer("");
std::string buffer2("");
std::string dspBuffer("-- Select a mesh group --");
std::string dspBuffer2("");
D3DXVECTOR3 PickLocation;

std::map<int, const LightEmitter*> Emitters;

// Dialog state -------------------------------------------------------------------------------
bool bGrpO, bVisO, bMshO, bBoxes, bSpheres, bTileBB, bHSM, bHSG, bAmbient, bWire, bDual, bPick, bFpsLim;
bool bLink, bExtend, bNorm, bFade, bSeams, bMore;
int iDisplay, iCamera, iDefShader, iSceneDbg, iEnvMap, iAction, iTarget, iMatPrp, iCones;
int speedPos, resPos, matAdjPos;
float varA, varB, varC;
float chVal[4];
bool chEdit[4];
char meshBuf[32], groupBuf[32];
std::string texFile;
std::string matTitle("Material");
std::string meshTitle("Mesh options");
std::string texLabel("Texture: None");
std::string meshLabel("Mesh: None");
std::vector<std::string> coneList;

void UpdateMaterialDisplay(bool bSetup=false);
void SetColorSlider();
void SetColorValue(float value);
void UpdateColorSlider(WORD pos);
void UpdateShader();
bool Execute();
void SaveEnvMap();
void CreateSamplingKernel();
float GetMaterialValue(DWORD MatPrp, DWORD clr);
bool IsMaterialModified(DWORD MatPrp);
void SetMaterialModified(DWORD MatPrp, bool bState);
void UpdateMeshMaterial(float value, DWORD MatPrp, DWORD clr);

void OpenGFXDlgClbk(void *context);
void OpenColDlgClbk(void *context);

class GFXDialog: public ImGuiDialog
{
public:
	GFXDialog():ImGuiDialog("Graphics controls") {}
	void OnDraw() override {
		ImGui::PushItemWidth(150.0);
		ImGui::SeparatorText("Post processing");

		ImGui::SliderFloatReset("Light glow intensity", &Config->GFXIntensity, 0.0f, 1.0f, 0.5f, "%1.2f");
		ImGui::SliderFloatReset("Light glow distance", &Config->GFXDistance, 0.0f, 1.0f, 0.8f, "%1.2f");
		ImGui::SliderFloatReset("Glow threshold", &Config->GFXThreshold, 0.5f, 2.0f, 1.1f, "%1.2f");
		ImGui::SliderFloatReset("Gamma", &Config->GFXGamma, 0.3f, 2.5f, 1.0f, "%1.2f");

		ImGui::SeparatorText("Lighting");

		ImGui::SliderFloatReset("Sunlight intensity", &Config->GFXSunIntensity, 0.5f, 2.5f, 1.2f, "%1.2f");
		ImGui::SliderFloatReset("Indirect lighting", &Config->PlanetGlow, 0.01f, 2.0f, 0.7f, "%1.2f");
		ImGui::SliderFloatReset("Local lights max", &Config->GFXLocalMax, 0.001f, 1.0f, 0.5f, "%1.2f");
		ImGui::SliderFloatReset("Sun glare intensity", &Config->GFXGlare, 0.001f, 1.0f, 0.5f, "%1.2f");

		if(ImGui::Button("Recreate sun and glares")) {
			g_client->GetScene()->CreateSunGlare();
		}
		ImGui::PopItemWidth();
	}
};

class ColDialog: public ImGuiDialog
{
public:
	ColDialog():ImGuiDialog("Collider visualization") {}
	void OnDraw() override {
		ImGui::PushItemWidth(150.0);
		ImGui::SeparatorText("Physics colliders");

		bool base = Config->bShowBaseColliders == 1;
		if (ImGui::Checkbox("Show base colliders", &base))
			Config->bShowBaseColliders = base ? 1 : 0;

		bool vessel = Config->bShowVesselColliders == 1;
		if (ImGui::Checkbox("Show vessel colliders", &vessel))
			Config->bShowVesselColliders = vessel ? 1 : 0;

		ImGui::PopItemWidth();
	}
};

class DebugDialog : public ImGuiDialog
{
public:
	DebugDialog() : ImGuiDialog("D3D9 debug controls") {}
	void Display() override
	{
		if (ImGui::Begin(name.c_str(), &active, ImGuiWindowFlags_AlwaysAutoResize)) OnDraw();
		ImGui::End();
		if (!active) OnClose();
	}
	void Hide()
	{
		active = false;
		oapiCloseDialog(this);
	}
	void OnDraw() override;
	void OnClose() override { Close(); }
};

class DataDialog : public ImGuiDialog
{
public:
	DataDialog() : ImGuiDialog("Debug data", { 640.0f, 300.0f }) {}
	void OnDraw() override;
	void OnClose() override
	{
		if (!buffer.empty()) buffer.clear();
		oapiCloseDialog(this);
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
void Create()
{
	vObj = NULL;
	nMesh = 0;
	nGroup = 0;
	sMesh = 0;
	sGroup = 0;
	debugFlags = 0;
	camSpeed = 0.5;
	camMode = 0;
	dspMode = 0;
	SelColor = 0;
	sEmitter = 0;
	PickLocation = D3DXVECTOR3(0,0,0);
	memset(visual, 0, sizeof(visual));

	cpr = cpg = cpb = cpa = 0.0f;

	if (Config->EnableMeshDbg) {
		dwCmd = oapiRegisterCustomCmd((char*)"D3D9 Debug Controls", (char*)"This dialog allows to control various debug and development features", OpenDlgClbk, NULL);
	}
	else {
		dwCmd = 0;
	}

	dbgDlg = new DebugDialog();
	dataDlg = new DataDialog();

	gfxDlg = new GFXDialog();
	dwGFX = oapiRegisterCustomCmd((char*)"D3D9 Graphics Controls", (char*)"This dialog allows to control various graphics options", OpenGFXDlgClbk, gfxDlg);

	colDlg = new ColDialog();
	dwCol = oapiRegisterCustomCmd((char*)"D3D9 Collider Visuals", (char*)"This dialog allows toggling physics collider visualizations", OpenColDlgClbk, colDlg);

	resbias = 4.0 + Config->LODBias;

	PrmList.clear();
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
	if (dbgDlg && dbgDlg->IsActive()) dbgDlg->Hide();
	vObj = NULL;
}

// =============================================================================================
//
bool IsActive()
{
	return dbgDlg && dbgDlg->IsActive();
}

// =============================================================================================
//
int GetSceneDebug()
{
	if (!IsActive()) return -1;
	return iSceneDbg;
}

// =============================================================================================
//
int GetSelectedEnvMap()
{
	if (!IsActive()) return 0;
	return iEnvMap;
}

// =============================================================================================
//
void Release()
{
	vObj = NULL;
	if (dbgDlg) {
		oapiCloseDialog(dbgDlg);
		delete dbgDlg;
		dbgDlg = NULL;
	}
	if (dataDlg) {
		oapiCloseDialog(dataDlg);
		delete dataDlg;
		dataDlg = NULL;
	}
	oapiCloseDialog(gfxDlg);
	delete gfxDlg;
	gfxDlg = NULL;
	oapiCloseDialog(colDlg);
	delete colDlg;
	colDlg = NULL;
	if (dwCmd) oapiUnregisterCustomCmd(dwCmd);
	if (dwGFX) oapiUnregisterCustomCmd(dwGFX);
	if (dwCol) oapiUnregisterCustomCmd(dwCol);
	dwCmd = 0;
	dwGFX = 0;
	dwCol = 0;
}

// =============================================================================================
//
void UpdateFlags()
{
	SETFLAG(debugFlags, DBG_FLAGS_SELGRPONLY,	bGrpO);
	SETFLAG(debugFlags, DBG_FLAGS_SELMSHONLY,	bMshO);
	SETFLAG(debugFlags, DBG_FLAGS_TILEBOXES,	bTileBB);
	SETFLAG(debugFlags, DBG_FLAGS_BOXES,		bBoxes);
	SETFLAG(debugFlags, DBG_FLAGS_SPHERES,		bSpheres);
	SETFLAG(debugFlags, DBG_FLAGS_HLMESH,		bHSM);
	SETFLAG(debugFlags, DBG_FLAGS_HLGROUP,		bHSG);
	SETFLAG(debugFlags, DBG_FLAGS_SELVISONLY,	bVisO);
	SETFLAG(debugFlags, DBG_FLAGS_AMBIENT,	    bAmbient);
	SETFLAG(debugFlags, DBG_FLAGS_WIREFRAME,	bWire);
	SETFLAG(debugFlags, DBG_FLAGS_DUALSIDED,	bDual);
	SETFLAG(debugFlags, DBG_FLAGS_PICK,			bPick);
	SETFLAG(debugFlags, DBG_FLAGS_FPSLIM,		bFpsLim);

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
	strncpy_s(var.tip, 80, tip, 80);
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
	strncpy_s(var.tip, 80, tip, 80);
	return var;
}

DWORD DropdownList(DWORD x)
{
	if (x >= Dropdown.size()) return 0;
	return Dropdown[x].id;
}

DWORD SelectedProperty()
{
	return DropdownList(DWORD(iMatPrp));
}

// =============================================================================================
//
void InitMatList(WORD shader)
{
	Dropdown.clear();

	if (shader == SHADER_NULL) {
		std::list<char> list = { 0, 1, 2, 3, 4, 5, 6, 7, 10, 11, 12, 13, 14, 15, 16, 17 };
		for (auto x : list) Dropdown.push_back(PrmList[x]);
	}

	if (shader == SHADER_METALNESS) {
		std::list<char> list = { 0, 3, 5, 7, 8, 9 };
		for (auto x : list) Dropdown.push_back(PrmList[x]);
	}

	if (iMatPrp < 0 || iMatPrp >= int(Dropdown.size())) iMatPrp = 0;

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
	if (IsActive()) {
		oapiOpenDialog(dbgDlg);
		return;
	}

	bGrpO = bVisO = bMshO = bBoxes = bSpheres = bTileBB = bHSM = bHSG = bAmbient = bWire = bDual = false;
	bLink = bExtend = bNorm = bFade = bSeams = bMore = false;
	bFpsLim = (Config->EnableLimiter == 1);
	bPick = true;

	iDisplay = iCamera = iDefShader = iSceneDbg = iEnvMap = iAction = iTarget = iMatPrp = iCones = 0;
	for (int i = 0; i < 4; i++) chEdit[i] = false, chVal[i] = 0.0f;
	matTitle = "Material";
	meshTitle = "Mesh options";
	texLabel = "Texture: None";
	meshLabel = "Mesh: None";
	coneList.clear();
	sEmitter = 0;

	varA = 1.3f;
	varB = 0.01f;
	varC = 0.0f;

	resPos = int((resbias - 4.0) * 5.0);
	speedPos = 75;
	matAdjPos = 0;

	camMode = 0;
	dspMode = 0;

	oapiOpenDialog(dbgDlg);

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

	InitMatList(SHADER_NULL);

	OBJHANDLE hTgt = oapiCameraTarget();

	SetVisual(g_client->GetScene()->GetVisObject(hTgt));	// This will call SetupMeshGroups()

	UpdateFlags();
}


// =============================================================================================
//
void SetTuningValue(int idx, D3DCOLORVALUE *pClr, DWORD clr, float value)
{

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

	DWORD Shader = DWORD(iDefShader);

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
	DWORD MatPrp = SelectedProperty();

	float mi = Params[MatPrp].var[SelColor].min;
	float mx = (bExtend ? Params[MatPrp].var[SelColor].extmax : Params[MatPrp].var[SelColor].max);

	float val = GetMaterialValue(MatPrp, SelColor);

	val -= mi; val /= (mx - mi);

	if (Params[MatPrp].var[SelColor].Scl == scale::SQRT) val = sqrt(val);
	if (Params[MatPrp].var[SelColor].Scl == scale::SQR) val = val*val;

	matAdjPos = CLAMP(int(WORD(val*255.0f)), 0, 255);
}

// =============================================================================================
//
void UpdateMaterialDisplay(bool bSetup)
{
	char lbl[256];

	if (!vObj) return;
	OBJHANDLE hObj = vObj->GetObject();
	if (!oapiIsVessel(hObj)) return;

	D3D9Mesh *hMesh = (D3D9Mesh *)vObj->GetMesh(sMesh);
	if (!hMesh) return;

	WORD Shader = hMesh->GetDefaultShader();
	if (Shader == SHADER_NULL) iDefShader = 0;
	if (Shader == SHADER_METALNESS) iDefShader = 1;

	DWORD matidx = hMesh->GetMeshGroupMaterialIdx(sGroup);

	sprintf_s(lbl, 256, "Material %u:", matidx);
	matTitle = lbl;

	if (bSetup) SelColor = 0;

	DWORD texidx = hMesh->GetMeshGroupTextureIdx(sGroup);

	if (texidx==0) texLabel = "Texture: None";
	else {
		SURFHANDLE hSrf = hMesh->GetTexture(texidx);
		if (hSrf) {
			sprintf_s(lbl, 256, "Texture: %s [%u]", RemovePath(SURFACE(hSrf)->GetName()), texidx);
			texLabel = lbl;
		}
	}

	sprintf_s(lbl, 256, "Mesh: %s", RemovePath(hMesh->GetName()));
	meshLabel = lbl;
	meshTitle = lbl;
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

	DWORD MatPrp = SelectedProperty();

	bool bLnk = bLink;

	float mi = Params[MatPrp].var[SelColor].min;
	float mx = (bExtend ? Params[MatPrp].var[SelColor].extmax : Params[MatPrp].var[SelColor].max);

	if (MatPrp==5 || MatPrp==6) bLnk = false;	// Roughness, Fresnel
	if (SelColor==3) bLnk = false;				// Alpha, Specular power

	if (Params[MatPrp].var[SelColor].Scl == scale::SQRT) val = (val*val);
	if (Params[MatPrp].var[SelColor].Scl == scale::SQR) val = sqrt(val);

	val *= (mx - mi); val += mi;

	float old = GetMaterialValue(MatPrp, SelColor);
	float fct = val/old;

	if (old<1e-4) fct = val;

	if (bLnk) {
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
	if (!vObj) return;
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
	if (!vObj) return;

	if (nMesh!=0) {
		if (sMesh>0xFFFF) sMesh = nMesh-1;
		if (sMesh>=nMesh) sMesh = 0;
	}
	else {
		sMesh=0, sGroup=0, nGroup=0;
		return;
	}

	D3D9Mesh *mesh = (class D3D9Mesh *)vObj->GetMesh(sMesh);

	if (mesh) nGroup = mesh->GetGroupCount();
	else	  nGroup = 0;

	if (nGroup!=0) {
		if (sGroup>0xFFFF)  sGroup = nGroup-1;
		if (sGroup>=nGroup) sGroup = 0;
	}
	else {
		sGroup=0;
		return;
	}

	UpdateMaterialDisplay();
	SetColorSlider();
	InitMatList(mesh->GetDefaultShader());
}

// =============================================================================================
//
double GetVisualSize()
{
	if (IsActive() && vObj) {
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
	if (!IsActive()) {
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
	if (!vObj || !IsActive()) return;
	nMesh = vObj->GetMeshCount();
	sprintf_s(visual, 64, "Visual: %s", vObj->GetName());
	SetupMeshGroups();

	coneList.clear();
	Emitters.clear();

	if (vObj->Type() == OBJTP_VESSEL) {

		coneList.push_back("NONE");
		Emitters[0] = NULL;

		char line[64];

		vVessel *vV = static_cast<vVessel*>(vObj);
		VESSEL *vessel = vV->GetInterface();
		DWORD nemitter = vessel->LightEmitterCount();

		for (DWORD j = 0; j < nemitter; j++) {

			const LightEmitter *em = vessel->GetLightEmitter(j);
			line[0] = 0;

			if (em->GetType() == LightEmitter::LT_SPOT) {
				const SpotLight *sl = static_cast<const SpotLight*>(em);
				double P = sl->GetPenumbra()*DEG;
				double U = sl->GetUmbra()*DEG;
				double R = sl->GetRange();
				sprintf_s(line, 64, "%s P%1.0f U%1.0f R%1.0f", _PTR(em), P, U, R);
			}

			if (em->GetType() == LightEmitter::LT_POINT) {
				const PointLight *pl = static_cast<const PointLight*>(em);
				double R = pl->GetRange();
				sprintf_s(line, 64, "%s R%1.0f", _PTR(em), R);
			}

			switch (em->GetVisibility())
			{
			case LightEmitter::VIS_EXTERNAL: strcat_s(line, 64, " EXT"); break;
			case LightEmitter::VIS_COCKPIT: strcat_s(line, 64, " VC"); break;
			case LightEmitter::VIS_ALWAYS: strcat_s(line, 64, " ALW"); break;
			}

			if ((em->GetType() == LightEmitter::LT_SPOT) || (em->GetType() == LightEmitter::LT_POINT)) {
				coneList.push_back(line);
				Emitters[j + 1] = em;
			}
		}
	}

	iCones = 0;
}

// =============================================================================================
//
void RemoveVisual(vObject *vo)
{
	if (vObj==vo) vObj=NULL;
}

// =============================================================================================
//
void SetColorValue(float value)
{
	DWORD MatPrp = SelectedProperty();
	UpdateMeshMaterial(value, MatPrp, SelColor);
	SetColorSlider();
}


// =============================================================================================
//
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
		C *= pow(a, -abs(C)*fMip);

		float k = b * fMip;

		C = D3DXVECTOR4(reduce(C.x, k), reduce(C.y, k), reduce(C.z, k), reduce(C.w, k));
		C = C*0.5f + 0.5f;			// Back to [0, 1]
	}

	return D3DXCOLOR(C.x, C.y, C.z, C.w);
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

// =============================================================================================
//
bool Execute()
{
	LPDIRECT3DDEVICE9 pDevice = g_client->GetDevice();

	D3DLOCKED_RECT in, out;
	PCParam prm;

	DWORD Func = 0;
	DWORD Action = DWORD(iAction);
	DWORD Target = DWORD(iTarget);

	if (bNorm) Func |= 0x1;
	if (bFade) Func |= 0x2;
	if (bSeams) Func |= 0x4;

	if (texFile.empty()) return false;

	if (Action<=2) {

		LPDIRECT3DTEXTURE9 pTex = NULL;
		LPDIRECT3DTEXTURE9 pWork = NULL;
		LPDIRECT3DTEXTURE9 pSave = NULL;
		D3DXIMAGE_INFO info;

		HR(D3DXCreateTextureFromFileExA(pDevice, texFile.c_str(), D3DX_DEFAULT_NONPOW2, D3DX_DEFAULT_NONPOW2, 0, 0, D3DFMT_A8R8G8B8, D3DPOOL_SYSTEMMEM, D3DX_DEFAULT, D3DX_DEFAULT, 0, &info, NULL, &pTex));

		if (!pTex) {
			LogErr("Failed to open a file [%s]", texFile.c_str());
			return false;
		}

		DWORD mips = pTex->GetLevelCount();

		HR(D3DXCreateTexture(pDevice, info.Width, info.Height, mips, 0, D3DFMT_A8R8G8B8, D3DPOOL_SYSTEMMEM, &pWork));
		if (!pWork) {
			SAFE_RELEASE(pTex);
			return false;
		}

		if (Action==0) { HR(D3DXCreateTexture(pDevice, info.Width, info.Height, mips, 0, D3DFMT_DXT5, D3DPOOL_SYSTEMMEM, &pSave)); }
		if (Action==1) { HR(D3DXCreateTexture(pDevice, info.Width, info.Height, mips, 0, D3DFMT_A8R8G8B8, D3DPOOL_SYSTEMMEM, &pSave)); }
		if (Action==2) { HR(D3DXCreateTexture(pDevice, info.Width, info.Height, mips, 0, D3DFMT_A4R4G4B4, D3DPOOL_SYSTEMMEM, &pSave)); }

		if (!pSave) {
			SAFE_RELEASE(pTex);
			SAFE_RELEASE(pWork);
			return false;
		}

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
			prm.a = varA;
			prm.b = varB;
			prm.c = varC;

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
				pIn->Release();
				pOut->Release();
				SAFE_RELEASE(pWork);
				SAFE_RELEASE(pSave);
				return false;
			}
			pIn->Release();
			pOut->Release();
		}
		SAFE_RELEASE(pWork);

		// Save the texture into a file -------------------------------
		//
		if (Target==0) {
			std::filesystem::path src(texFile);
			std::string file = oplat::SaveFileDialog("Save texture", src.parent_path().string(), src.stem().string() + ".dds", { "dds" });
			if (!file.empty()) {
				if (D3DXSaveTextureToFileA(file.c_str(), D3DXIFF_DDS, pSave, NULL)!=S_OK) {
					LogErr("Failed to create a file [%s]", file.c_str());
					SAFE_RELEASE(pSave);
					return false;
				}
				SAFE_RELEASE(pSave);
				return true;
			}
			SAFE_RELEASE(pSave);
			return false;
		}

		// Assign the texture for rendering -----------------------------
		//
		if (Target==1 || Target==2 || Target==3) {
			vPlanet *vP = g_client->GetScene()->GetCameraProxyVisual();
			if (vP) vP->SetMicroTexture(pSave, Target-1);
			SAFE_RELEASE(pSave);
			return true;
		}

		SAFE_RELEASE(pSave);
	}

	return false;
}


// =============================================================================================
//
void SaveEnvMap()
{
	if (!vObj) return;

	OBJHANDLE hObj = vObj->Object();

	if (oapiIsVessel(hObj)) {
		vVessel *vVes = (vVessel *)vObj;
		LPDIRECT3DCUBETEXTURE9 pTex = vVes->GetEnvMap(ENVMAP_MAIN);
		if (D3DXSaveTextureToFileA("EnvMap.dds", D3DXIFF_DDS, pTex, NULL) != S_OK) {
			LogErr("Failed to save envmap");
		}
	}
}

//-------------------------------------------------------------------------------------------
//
void Append(const char *format, ...)
{
	if (!dataDlg || !dataDlg->IsActive()) return;
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
	if (!dataDlg || !dataDlg->IsActive()) return;
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
	if (!dataDlg || !dataDlg->IsActive()) return;

	Append2("LocalPos = [%f, %f, %f]", PickLocation.x, PickLocation.y, PickLocation.z);

	dspBuffer2 = buffer2;
	buffer2.clear();
}

//-------------------------------------------------------------------------------------------
//
void Refresh()
{
	if (!dataDlg || !dataDlg->IsActive()) return;
	dspBuffer = buffer;
	buffer.clear();
	Refresh2();
}

// =============================================================================================
//
static bool Combo(const char *label, int *idx, const char *const items[], int count)
{
	if (*idx < 0 || *idx >= count) *idx = 0;
	return ImGui::Combo(label, idx, items, count);
}

// =============================================================================================
//
static bool HasMaterial()
{
	if (!vObj) return false;
	if (!oapiIsVessel(vObj->GetObject())) return false;
	return vObj->GetMesh(sMesh) != NULL && nGroup != 0;
}

// =============================================================================================
//
static void IndexField(const char *id, char *buf, bool &bEdit, DWORD sel, DWORD count, DWORD &target, bool bValid)
{
	if (!bEdit) {
		if (bValid && count) snprintf(buf, 32, "%u/%u", sel, count - 1);
		else snprintf(buf, 32, "N/A");
	}
	ImGui::SetNextItemWidth(ImGui::GetFontSize() * 5.0f);
	ImGui::InputText(id, buf, 32);
	bEdit = ImGui::IsItemActive();
	if (ImGui::IsItemDeactivated()) {
		for (int i = 0; i < 32; i++) if (buf[i] == 0 || buf[i] == '/') { buf[i] = 0; break; }
		target = atoi(buf);
		SetupMeshGroups();
	}
}

// =============================================================================================
//
static void DrawMeshColumn()
{
	static const char *dspItems[] = { "Everything", "Selected visual", "Selected mesh", "Selected group" };
	static const char *camItems[] = { "Center on visual", "Wheel fly/pan cam" };
	static const char *shdItems[] = { "PBR (old)", "Metalness PBR" };

	float lw = ImGui::GetFontSize() * 6.0f;

	ImGui::TextUnformatted(visual[0] ? visual : "Visual: None");

	ImGui::SeparatorText("Mesh debugger");

	ImGui::AlignTextToFramePadding(); ImGui::TextUnformatted("Display"); ImGui::SameLine(lw);
	ImGui::SetNextItemWidth(-FLT_MIN);
	if (Combo("##display", &iDisplay, dspItems, 4)) dspMode = DWORD(iDisplay);

	ImGui::AlignTextToFramePadding(); ImGui::TextUnformatted("Camera"); ImGui::SameLine(lw);
	ImGui::SetNextItemWidth(-FLT_MIN);
	if (Combo("##camera", &iCamera, camItems, 2)) camMode = DWORD(iCamera);

	double fpos = pow(2.0, double(speedPos) * 13.0 / 200.0);
	ImGui::AlignTextToFramePadding(); ImGui::TextUnformatted("Speed"); ImGui::SameLine(lw);
	ImGui::SetNextItemWidth(-ImGui::GetFontSize() * 3.0f);
	if (ImGui::SliderInt("##speed", &speedPos, 1, 200, "")) {
		fpos = pow(2.0, double(speedPos) * 13.0 / 200.0);
		camSpeed = fpos / 50.0;
	}
	ImGui::SameLine(); ImGui::Text("%1.0f", fpos);

	bool bVis = vObj != NULL;

	ImGui::AlignTextToFramePadding(); ImGui::TextUnformatted("Mesh"); ImGui::SameLine(lw);
	static bool bMeshEdit = false;
	IndexField("##mesh", meshBuf, bMeshEdit, sMesh, nMesh, sMesh, bVis);
	ImGui::SameLine(); if (ImGui::ArrowButton("##mshup", ImGuiDir_Left)) { sMesh--; SetupMeshGroups(); }
	ImGui::SameLine(); if (ImGui::ArrowButton("##mshdn", ImGuiDir_Right)) { sMesh++; SetupMeshGroups(); }

	ImGui::AlignTextToFramePadding(); ImGui::TextUnformatted("Group"); ImGui::SameLine(lw);
	static bool bGroupEdit = false;
	IndexField("##group", groupBuf, bGroupEdit, sGroup, nGroup, sGroup, bVis && nMesh != 0);
	ImGui::SameLine(); if (ImGui::ArrowButton("##grpup", ImGuiDir_Left)) { sGroup--; SetupMeshGroups(); }
	ImGui::SameLine(); if (ImGui::ArrowButton("##grpdn", ImGuiDir_Right)) { sGroup++; SetupMeshGroups(); }

	if (ImGui::BeginTable("##flags", 2)) {
		ImGui::TableNextColumn(); if (ImGui::Checkbox("Highlight selected group", &bHSG)) UpdateFlags();
		ImGui::TableNextColumn(); if (ImGui::Checkbox("Dual sided", &bDual)) UpdateFlags();
		ImGui::TableNextColumn(); if (ImGui::Checkbox("Highlight selected mesh", &bHSM)) UpdateFlags();
		ImGui::TableNextColumn(); if (ImGui::Checkbox("Wireframe", &bWire)) UpdateFlags();
		ImGui::TableNextColumn(); if (ImGui::Checkbox("Add ambient light", &bAmbient)) UpdateFlags();
		ImGui::TableNextColumn(); if (ImGui::Checkbox("Pick", &bPick)) UpdateFlags();
		ImGui::EndTable();
	}

	ImGui::SeparatorText(meshTitle.c_str());

	ImGui::AlignTextToFramePadding(); ImGui::TextUnformatted("Default shader"); ImGui::SameLine(lw);
	ImGui::SetNextItemWidth(-FLT_MIN);
	if (Combo("##defshader", &iDefShader, shdItems, 2)) if (vObj) UpdateShader();

	ImGui::SeparatorText(matTitle.c_str());

	bool bMat = HasMaterial();
	DWORD MatPrp = SelectedProperty();

	ImGui::AlignTextToFramePadding(); ImGui::TextUnformatted("Property"); ImGui::SameLine(lw);
	ImGui::SetNextItemWidth(-FLT_MIN);
	const char *preview = Dropdown.empty() ? "" : Dropdown[std::min(size_t(iMatPrp), Dropdown.size() - 1)].name.c_str();
	if (ImGui::BeginCombo("##matprp", preview)) {
		for (int i = 0; i < int(Dropdown.size()); i++) {
			if (ImGui::Selectable(Dropdown[i].name.c_str(), i == iMatPrp)) {
				iMatPrp = i;
				if (bMat) {
					UpdateMaterialDisplay(true);
					SetColorSlider();
				}
			}
		}
		ImGui::EndCombo();
	}
	MatPrp = SelectedProperty();

	float sp = ImGui::GetStyle().ItemSpacing.x;
	float fw = (ImGui::GetContentRegionAvail().x - sp * 3.0f) / 4.0f;

	for (int i = 0; i < 4; i++) {
		if (i) ImGui::SameLine();
		ImGui::PushID(i);
		ImGui::SetNextItemWidth(fw);
		if (!bMat || !Params[MatPrp].var[i].bUsed) {
			char empty[2] = "";
			ImGui::BeginDisabled();
			ImGui::InputText("##ch", empty, 2, ImGuiInputTextFlags_ReadOnly);
			ImGui::EndDisabled();
			chEdit[i] = false;
		}
		else {
			if (!chEdit[i]) chVal[i] = GetMaterialValue(MatPrp, i);
			ImGui::InputFloat("##ch", &chVal[i], 0.0f, 0.0f, "%3.3f");
			if (ImGui::IsItemActivated()) {
				chEdit[i] = true;
				SelColor = i;
				UpdateMaterialDisplay();
				SetColorSlider();
			}
			if (ImGui::IsItemDeactivated()) {
				chEdit[i] = false;
				if (ImGui::IsItemDeactivatedAfterEdit()) SetColorValue(chVal[i]);
			}
			ImGui::SetItemTooltip("%s", Params[MatPrp].var[i].tip);
		}
		ImGui::PopID();
	}

	ImGui::BeginDisabled(!bMat);

	ImGui::SetNextItemWidth(-FLT_MIN);
	if (ImGui::SliderInt("##matadj", &matAdjPos, 0, 255, "")) {
		UpdateColorSlider(WORD(matAdjPos));
		UpdateMaterialDisplay();
	}

	ImGui::Checkbox("Link channels", &bLink);
	ImGui::SetItemTooltip("Adjust all color channels at the same time");
	ImGui::SameLine();
	if (ImGui::Button("Copy")) {
		cpr = GetMaterialValue(MatPrp, 0);
		cpg = GetMaterialValue(MatPrp, 1);
		cpb = GetMaterialValue(MatPrp, 2);
	}
	ImGui::SameLine();
	if (ImGui::Button("Paste")) {
		UpdateMeshMaterial(cpr, MatPrp, 0);
		UpdateMeshMaterial(cpg, MatPrp, 1);
		UpdateMeshMaterial(cpb, MatPrp, 2);
		UpdateMaterialDisplay();
		SetColorSlider();
	}

	if (ImGui::Checkbox("Extend material range", &bExtend)) SetColorSlider();
	ImGui::SetItemTooltip("Extend Diffuse/Roughess material range beyond 1.0f to allow texture fine tuning.");

	bool bDefined = bMat ? IsMaterialModified(MatPrp) : false;
	if (ImGui::Checkbox("Use and save the property", &bDefined)) SetMaterialModified(MatPrp, bDefined);
	ImGui::SetItemTooltip("Use the material property for rendering and save it");

	ImGui::EndDisabled();

	ImGui::TextUnformatted(texLabel.c_str());
	ImGui::TextUnformatted(meshLabel.c_str());

	ImGui::Separator();

	if (ImGui::Button("Save materials")) {
		if (vObj && oapiIsVessel(vObj->GetObject())) {
			vVessel *vVes = (vVessel *)vObj;
			vVes->GetMaterialManager()->SaveConfiguration();
		}
	}
	ImGui::SameLine();
	if (ImGui::Button("Reload textures")) {
		if (vObj && vObj->Type() == OBJTP_VESSEL) ((vVessel *)vObj)->ReloadTextures();
	}
	ImGui::SameLine();
	if (ImGui::Button("Close")) Close();

	if (ImGui::Button("Data window")) oapiOpenDialog(dataDlg);
	ImGui::SameLine();
	if (ImGui::Button("Reload shader")) D3D9Effect::D3D9TechInit(g_client, g_client->GetDevice(), "MetalClient");
	ImGui::SameLine();
	if (ImGui::Button(bMore ? "<<<" : ">>>")) bMore = !bMore;
	ImGui::SetItemTooltip("Click to show/hide more options");
}

// =============================================================================================
//
static void DrawToolsColumn()
{
	static const char *dbgItems[] = { "None", "Normals global", "Normals tangent", "Height", "Height Mk2", "Tile level" };
	static const char *envItems[] = { "None", "Mirror", "Blur 1", "Blur 2", "Blur 3", "Blur 4", "Irrad.Probe", "IrdPreItg", "ShadowMap",
		"Irradiance", "GlowMask", "ScreenDepth", "Normals", "LightVisbil.", "EclipseTbl" };
	static const char *actItems[] = { "Convert to DXT5", "Convert to RGB8", "Convert to RGB4" };
	static const char *tgtItems[] = { "Save", "Assign to slot 0", "Assign to slot 1", "Assign to slot 2" };

	float lw = ImGui::GetFontSize() * 7.5f;

	ImGui::SeparatorText("Misc.");

	if (ImGui::Checkbox("FPS limiter", &bFpsLim)) UpdateFlags();
	ImGui::SameLine();
	if (ImGui::Button("Create kernel")) CreateSamplingKernel();

	ImGui::AlignTextToFramePadding(); ImGui::TextUnformatted("Resolution bias"); ImGui::SameLine(lw);
	ImGui::SetNextItemWidth(-FLT_MIN);
	if (ImGui::SliderInt("##resbias", &resPos, -10, 10)) resbias = 4.0 + 0.2 * double(resPos);

	ImGui::SeparatorText("Scene debugger");

	ImGui::AlignTextToFramePadding(); ImGui::TextUnformatted("Debug mode"); ImGui::SameLine(lw);
	ImGui::SetNextItemWidth(-FLT_MIN);
	Combo("##scenedbg", &iSceneDbg, dbgItems, 6);

	ImGui::AlignTextToFramePadding(); ImGui::TextUnformatted("Special buffer"); ImGui::SameLine(lw);
	ImGui::SetNextItemWidth(-FLT_MIN);
	Combo("##envmap", &iEnvMap, envItems, 15);

	if (ImGui::Button("Save envmap")) SaveEnvMap();

	ImGui::SeparatorText("Texture tools");

	if (ImGui::Button("Open")) {
		bool bPaused = oapiGetPause();
		oapiSetPause(true);
		std::string file = oplat::OpenFileDialog("Open texture", "Textures", { "dds", "jpg", "png", "hdr", "bmp", "tga" });
		if (!file.empty()) texFile = file;
		oapiSetPause(bPaused);
	}
	ImGui::SameLine();
	ImGui::SetNextItemWidth(-FLT_MIN);
	ImGui::BeginDisabled();
	char fbuf[512];
	snprintf(fbuf, sizeof(fbuf), "%s", texFile.c_str());
	ImGui::InputText("##file", fbuf, sizeof(fbuf), ImGuiInputTextFlags_ReadOnly);
	ImGui::EndDisabled();

	ImGui::AlignTextToFramePadding(); ImGui::TextUnformatted("Action"); ImGui::SameLine(lw);
	ImGui::SetNextItemWidth(-FLT_MIN);
	Combo("##action", &iAction, actItems, 3);

	ImGui::Checkbox("Normalize", &bNorm);
	ImGui::SetItemTooltip("Center color channels at 0.5f to prevent lightening/darkening the results");
	ImGui::SameLine();
	ImGui::Checkbox("Reduce seams", &bSeams);
	ImGui::SetItemTooltip("Enable seams reduction at each mipmap level");
	ImGui::Checkbox("Mipmap fade", &bFade);
	ImGui::SetItemTooltip("Enable mipmap post processing. Contrast and detail is reduced from each mipmap to prevent 'stripes' (See:Fa,Fb)");

	ImGui::AlignTextToFramePadding(); ImGui::TextUnformatted("Target"); ImGui::SameLine(lw);
	ImGui::SetNextItemWidth(-FLT_MIN);
	Combo("##target", &iTarget, tgtItems, 4);
	ImGui::SetItemTooltip("Select a target where the resulting image is assigned");

	float sp = ImGui::GetStyle().ItemSpacing.x;
	float fw = (ImGui::GetContentRegionAvail().x - sp * 2.0f) / 3.0f - ImGui::CalcTextSize("Noise").x;
	ImGui::AlignTextToFramePadding(); ImGui::TextUnformatted("Noise"); ImGui::SameLine();
	ImGui::SetNextItemWidth(fw); ImGui::InputFloat("##varc", &varC, 0.0f, 0.0f, "%.2f");
	ImGui::SetItemTooltip("Apply noise to main level and all mipmaps before attennuation (Fa,Fb)");
	ImGui::SameLine(); ImGui::TextUnformatted("Fa"); ImGui::SameLine();
	ImGui::SetNextItemWidth(fw); ImGui::InputFloat("##vara", &varA, 0.0f, 0.0f, "%.2f");
	ImGui::SetItemTooltip("Attennuates high contrast components. Leaves low contrast parts unchanged [1.0 to 1.6]");
	ImGui::SameLine(); ImGui::TextUnformatted("Fb"); ImGui::SameLine();
	ImGui::SetNextItemWidth(-FLT_MIN); ImGui::InputFloat("##varb", &varB, 0.0f, 0.0f, "%.2f");
	ImGui::SetItemTooltip("Attennuates everything equally. Typical range [0.00 to 0.03]");

	if (ImGui::Button("Execute")) {
		bool bPaused = oapiGetPause();
		oapiSetPause(true);
		if (Execute() == false) oapiAddNotification(OAPINOTIF_ERROR, "D3D9 debug controls", "Texture tool failed");
		oapiSetPause(bPaused);
	}

	ImGui::SeparatorText("Bounding geometry");

	if (ImGui::BeginTable("##bounds", 2)) {
		ImGui::TableNextColumn(); if (ImGui::Checkbox("Boxes", &bBoxes)) UpdateFlags();
		ImGui::TableNextColumn(); if (ImGui::Checkbox("Selected group only", &bGrpO)) UpdateFlags();
		ImGui::TableNextColumn(); if (ImGui::Checkbox("Spheres", &bSpheres)) UpdateFlags();
		ImGui::TableNextColumn(); if (ImGui::Checkbox("Selected mesh only", &bMshO)) UpdateFlags();
		ImGui::TableNextColumn(); if (ImGui::Checkbox("Surface tiles", &bTileBB)) UpdateFlags();
		ImGui::TableNextColumn(); if (ImGui::Checkbox("Selected visual only", &bVisO)) UpdateFlags();
		ImGui::EndTable();
	}

	ImGui::SeparatorText("Local light cones");

	ImGui::SetNextItemWidth(-FLT_MIN);
	const char *cprev = (iCones >= 0 && iCones < int(coneList.size())) ? coneList[iCones].c_str() : "";
	if (ImGui::BeginCombo("##cones", cprev)) {
		for (int i = 0; i < int(coneList.size()); i++) {
			if (ImGui::Selectable(coneList[i].c_str(), i == iCones)) {
				iCones = i;
				sEmitter = DWORD(i);
			}
		}
		ImGui::EndCombo();
	}
}

// =============================================================================================
//
void DebugDialog::OnDraw()
{
	float cw = ImGui::GetFontSize() * 21.0f;

	if (ImGui::BeginTable("##dbg", bMore ? 2 : 1, ImGuiTableFlags_BordersInnerV)) {
		ImGui::TableSetupColumn("##c0", ImGuiTableColumnFlags_WidthFixed, cw);
		if (bMore) ImGui::TableSetupColumn("##c1", ImGuiTableColumnFlags_WidthFixed, cw);
		ImGui::TableNextColumn();
		DrawMeshColumn();
		if (bMore) {
			ImGui::TableNextColumn();
			DrawToolsColumn();
		}
		ImGui::EndTable();
	}
}

// =============================================================================================
//
void DataDialog::OnDraw()
{
	float sp = ImGui::GetStyle().ItemSpacing.x;
	ImVec2 sz((ImGui::GetContentRegionAvail().x - sp) * 0.5f, -FLT_MIN);
	ImGui::InputTextMultiline("##data1", (char *)dspBuffer.c_str(), dspBuffer.size() + 1, sz, ImGuiInputTextFlags_ReadOnly);
	ImGui::SameLine();
	ImGui::InputTextMultiline("##data2", (char *)dspBuffer2.c_str(), dspBuffer2.size() + 1, sz, ImGuiInputTextFlags_ReadOnly);
}

// =============================================================================================
//
void OpenGFXDlgClbk(void *context)
{
	GFXDialog *gfx = (GFXDialog *)context;
	oapiOpenDialog(gfx);
}

void OpenColDlgClbk(void *context)
{
	ColDialog *col = (ColDialog *)context;
	oapiOpenDialog(col);
}

} // namespace
