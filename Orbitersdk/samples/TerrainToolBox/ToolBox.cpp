// =================================================================================================================================
//
// Copyright (c) 2020 Jarmo Nikkanen
// All rights reserved
// 
// =================================================================================================================================


#define STRICT 1
#define ORBITER_MODULE


#include <Windows.h>
#include <windowsx.h>
#include "OrbiterAPI.h"
#include "VesselAPI.h"
#include "ModuleAPI.h"
#include "DrawAPI.h"
#include "gcConst.h"
#include "ToolBox.h"
#include "resource.h"
#include "gcPropertyTree.h"
#include "QTree.h"
#include <Commctrl.h>
#include <vector>
#include <list>

using namespace std;

ToolKit *g_pTK = NULL;

const static char *create_selection_first = { "\
You need to select a target zone first. You can later adjust the zone if need to.\
Also, it's better to select 1-6 low-level tiles rather than 200 high-level tiles due to tile alignments." };




// =================================================================================================
// D3D9Client Callback Wrappers
// =================================================================================================
//
void __cdecl RenderClbk(int iUser, void *pUser, void *pParam)
{
	((ToolKit*)pParam)->clbkRender();
}

void __cdecl MouseClickClbk(int iUser, void *pUser, void *pParam)
{
	((ToolKit*)pParam)->clbkMouseClick(iUser, pUser);
}

string LngLat(Position p)
{
	string s;
	if (p.lng < 0) s = std::to_string(-p.lng*DEG) + "°W  ";
	else s = std::to_string(p.lng*DEG) + "°E  ";
	if (p.lat < 0) s += std::to_string(-p.lat*DEG) + "°S";
	else s += std::to_string(p.lat*DEG) + "°N";
	return s;
}



// =================================================================================================
// Initialize module
// =================================================================================================
//
DLLCLBK void InitModule(HINSTANCE hModule)
{
	// Can do very little here since graphics servises are not yet running 
	oapiRegisterModule(new ToolKit(hModule));
}


// =================================================================================================
//
DLLCLBK void ExitModule(HINSTANCE  hModule)
{

}


// ===============================================================================================
//
DLLCLBK void OpenToolsClbk(void *context)
{
	g_pTK = (ToolKit *)context;
	g_pTK->Initialize();
}


// ===============================================================================================
//
BOOL CALLBACK gDlgProc(HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	if (g_pTK) return g_pTK->DlgProc(hDlg, uMsg, wParam, lParam);
	return 0;
}


// ===============================================================================================
//
BOOL ToolKit::DlgProc(HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	
	switch (uMsg) {

	case WM_INITDIALOG:
		return true;

	case WM_COMMAND:

		switch (LOWORD(wParam))
		{
		case IDC_PINX:
		case IDC_PINY:
		case IDC_PINW:
		case IDC_PINN:
		{
			Pin pn;
			if (CreatePin(pn, selection.bounds)) {
				UpdateOverlay();
			}
			break;
		}

		case IDC_DISABLEPIN:
		{
			ComputeLevel();
			UpdateOverlay();
			break;
		}

		case IDC_LIGHTNESS:
		case IDC_BRIGHTNESS:
		case IDC_GAMMA:
		case IDC_ALPHA:
		case IDC_RED:
		case IDC_GREEN:
		case IDC_BLUE:
		{
			UpdateOverlay();
			break;
		}

		case IDC_IMPLEVEL:
		{
			ComputeLevel();
			UpdateOverlay();
			break;
		}

		case IDC_OPENIMAGE:
		{
			if (selection.area.size() == 0) {
				MessageBoxA(hAppMainWnd, create_selection_first, "Info", MB_OK);
				return false;
			}
			ImportImage();
			break;
		}

		case IDC_UPDATECLIP:
		{
			if (selection.area.size() != 0) oldsel = selection;
			selection.area.clear();
			break;
		}

		case IDC_DATAVIEW:
		{
			if (HPROP(lParam) == hILT) sel_corner = 0;
			if (HPROP(lParam) == hILB) sel_corner = 1;
			if (HPROP(lParam) == hIRB) sel_corner = 2;
			if (HPROP(lParam) == hIRT) sel_corner = 3;
			break;
		}
		
		case IDC_CALLBACK:
		{
			break;
		}

		case IDC_CORNERS:
		{
			AutoSelectCorners();
			ComputeLevel();
			UpdateOverlay();
			break;
		}

		case IDC_TRANSP:
		{
			UpdateOverlay();
			break;
		}

		case IDC_STOP:
		{
			StopImport();
			break;
		}

		case IDC_EXPORT:
		{
			Export();
			break;
		}

		case IDC_BAKE:
		{
			BakeImport();
			break;
		}
		}
	}

	return false;
}




// =================================================================================================
// Orbiter Module
// =================================================================================================
//
ToolKit::ToolKit(HINSTANCE hInst) : gcGUIApp(), Module(hInst)
{
	pCore = NULL;
	hMgr = NULL;
	hPlanet = NULL;
	pRoot = pFirst = pSecond = NULL;
	pProp = NULL;
	dmSphere = NULL;
	hAppMainWnd = NULL;
	hOverlaySrf = NULL;
	hOverlayBkg = NULL;
	hOverlay = NULL;
	hSource = NULL;
	hGradient = NULL;
	down_corner = -1;
	sel_corner = -1;
	bGo = false;

	// Can do very little here since graphics servises are not yet running 
	dwCmd = oapiRegisterCustomCmd("TerrainToolBox", "ToolBox for terrain and base editing", OpenToolsClbk, this);
	gcPropertyTreeInitialize(hInst);

	mIdent.Ident();
}


// =================================================================================================
//
ToolKit::~ToolKit()
{
	oapiUnregisterCustomCmd(dwCmd);
	gcPropertyTreeRelease(hModule);
}


// =================================================================================================
// Orbiter is shuting down... Release resources...
//
void ToolKit::clbkShutdown()
{
	if (!pCore) return;

	pCore->RegisterGenericProc(RenderClbk, GENERICPROC_DELETE, this);
	pCore->RegisterGenericProc(MouseClickClbk, GENERICPROC_DELETE, this);
	
	if (hOverlay) pCore->AddGlobalOverlay(hMgr, _V(0, 0, 0, 0), NULL, hOverlay);
	if (hOverlaySrf) oapiReleaseTexture(hOverlaySrf);
	if (hOverlayBkg) oapiReleaseTexture(hOverlayBkg);
	if (hSource) oapiReleaseTexture(hSource);
	if (dmSphere) pCore->ReleaseDevMesh(dmSphere);
		
	selection.area.clear();
	oldsel.area.clear();

	if (pRoot) delete pRoot;	// Delete QTree Tile Manager
	if (pProp) delete pProp;	// Delete Property tree control and sub-controls

	pCore = NULL;
	hMgr = NULL;
	hPlanet = NULL;
	pRoot = pFirst = pSecond = NULL;
	pProp = NULL;
	dmSphere = NULL;
	hAppMainWnd = NULL;
	hOverlaySrf = NULL;
	hOverlayBkg = NULL;
	hOverlay = NULL;
	hSource = NULL;
	hRootNode = NULL;
	down_corner = -1;
	sel_corner = -1;
	bGo = false;

	// Call shutdown for the base class
	gcGUIApp::clbkShutdown();
}


// =================================================================================================
//
bool ToolKit::clbkMessage(DWORD uMsg, HNODE hNode, int data)
{
	if (uMsg == gcGUI::MSG_CLOSE_APP) {
		clbkShutdown();
		return true;
	}

	return gcGUIApp::clbkMessage(uMsg, hNode, data);
}


// =================================================================================================
//
void ToolKit::SetMode(int mode)
{
	if (mode == MODE_EXPORT) {
		pProp->ShowEntry(hSecImp, false);
		pProp->ShowEntry(hSecExp, true);
	}
	if (mode == MODE_IMPORT) {
		pProp->ShowEntry(hSecImp, true);
		pProp->ShowEntry(hSecExp, false);		
	}
}


// =================================================================================================
// Abort Import Process, Release resources...
//
void ToolKit::StopImport()
{
	if (hSource) oapiReleaseTexture(hSource);
	if (hOverlay) pCore->AddGlobalOverlay(hMgr, _V(0, 0, 0, 0), NULL, hOverlay);
	if (hOverlaySrf) oapiReleaseTexture(hOverlaySrf);
	hSource = hOverlaySrf = NULL;
	hOverlay = NULL;
	selection.area.clear();
	oldsel.area.clear();
	pFirst = pSecond = NULL;
	SetMode(MODE_EXPORT);
}


// =================================================================================================
//
void ToolKit::SetupPlanet(OBJHANDLE hPln)
{
	hPlanet = hPln;
	hMgr = pCore->GetPlanetManager(hPln);
}



// =================================================================================================
// Initialize GUI Controls load Meshes and resources, Initialize QTree
// Register Render/Mouse click callbacks with D3D9Client.
//
//
bool ToolKit::Initialize()
{
	pCore = gcGetCoreInterface();
	if (!pCore) return false;

	hAppMainWnd = pCore->GetRenderWindow();

	// Must Initialize the base class
	if (gcGUIApp::Initialize() == false) {
		MessageBox(hAppMainWnd, "gcGUI is disabled, can't launch", "Error", MB_OK);
		pCore = NULL;
		hAppMainWnd = NULL;
		return false;
	}

	pCore->GetSystemSpecs(&sp, sizeof(gcCore::SystemSpecs));

	// Register needed Callback functions ----------------------------------------------------------
	//
	// Register a render callback to a exterior view
	pCore->RegisterGenericProc(RenderClbk, GENERICPROC_RENDER_EXTERIOR, this);

	// Register a mouse callback for terrain clicks
	pCore->RegisterGenericProc(MouseClickClbk, GENERICPROC_PICK_TERRAIN, this);

	
	SetupPlanet(oapiCameraProxyGbody());


	// Create Root Node ----------------------------------------------------------------------------
	// 
	pRoot = new QTree(pCore, hPlanet);

	dmSphere = pCore->LoadDevMeshGlobal("D3D9Sphere");

	if (!dmSphere) {
		oapiWriteLog("TerrainToolKit: Failed to load a file [D3D9Sphere.msh]");
		return false;
	}

	// Create edge-blend gradient
	//
	hGradient = oapiCreateSurfaceEx(128, 128, OAPISURFACE_RENDERTARGET | OAPISURFACE_TEXTURE | OAPISURFACE_MIPMAPS | OAPISURFACE_PF_ARGB);
	
	Sketchpad* pSkp = oapiGetSketchpad(hGradient);
	RECT r = { 0, 0, 129, 129 };
	pSkp->SetBlendState(Sketchpad::BlendState::COPY);
	pSkp->GradientFillRect(&r, 0x00000000, 0xFFFFFFFF, true);
	pSkp->SetBlendState();
	oapiReleaseSketchpad(pSkp);

	pCore->SaveSurface("Grad.dds", hGradient);

	// Create GUI Sections --------------------------------------------------------------------------
	//
	hCtrlDlg = CreateDialogParamA(hModule, MAKEINTRESOURCE(IDD_EXPORT), hAppMainWnd, gDlgProc, 0);
	hRootNode = RegisterApplication("Terrain ToolKit", NULL, gcGUI::DS_LEFT);
	hCtrlNode = RegisterSubsection(hRootNode, "Selection Oprions", hCtrlDlg);
	hImpoDlg = CreateDialogParam(hModule, MAKEINTRESOURCE(IDD_IMPORT), hAppMainWnd, gDlgProc, 0);
	hImpoNode = RegisterSubsection(hRootNode, "Import Options", hImpoDlg, 0xC0FFE0);
	hDataDlg  = CreateDialogParam(hModule, MAKEINTRESOURCE(IDD_DATA), hAppMainWnd, gDlgProc, 0);
	hDataNode = RegisterSubsection(hRootNode, "Properties", hDataDlg);

	DisplayWindow(hRootNode);


	SendDlgItemMessage(hCtrlDlg, IDC_AUTOHIGHLIGHT, BM_SETCHECK, true, 0);

	SendDlgItemMessage(hCtrlDlg, IDC_WHAT, CB_RESETCONTENT, 0, 0);
	SendDlgItemMessage(hCtrlDlg, IDC_WHAT, CB_ADDSTRING, 0, (LPARAM)"Texture");			// 0
	SendDlgItemMessage(hCtrlDlg, IDC_WHAT, CB_ADDSTRING, 0, (LPARAM)"Nightlights");		// 1
	SendDlgItemMessage(hCtrlDlg, IDC_WHAT, CB_ADDSTRING, 0, (LPARAM)"Elevation");		// 2
	SendDlgItemMessage(hCtrlDlg, IDC_WHAT, CB_SETCURSEL, 0, 0);

	SendDlgItemMessage(hCtrlDlg, IDC_SELECT, CB_RESETCONTENT, 0, 0);
	SendDlgItemMessage(hCtrlDlg, IDC_SELECT, CB_ADDSTRING, 0, (LPARAM)"Current Render Lvl");	// 0
	SendDlgItemMessage(hCtrlDlg, IDC_SELECT, CB_ADDSTRING, 0, (LPARAM)"Heighest Existing");		// 1
	SendDlgItemMessage(hCtrlDlg, IDC_SELECT, CB_SETCURSEL, 1, 0);

	for (int i = 5; i < 20; i++) {
		char Lbl[32]; sprintf_s(Lbl, 32, "Level %d", i);
		SendDlgItemMessageA(hCtrlDlg, IDC_SELECT, CB_ADDSTRING, 0, (LPARAM)Lbl);
	}


	pProp = new gcPropertyTree(this, hDataDlg, IDC_DATAVIEW, gDlgProc, GetFont(0), GetModule());

	// ---------------------------------------------------
	hSecCur = pProp->SubSection("Mouse cursor location");
	// ---------------------------------------------------
	hCLng = pProp->AddEntry("Longitude", hSecCur);
	hCLat = pProp->AddEntry("Latitude", hSecCur);
	hCEle = pProp->AddEntry("Elevation", hSecCur);
	hCFil = pProp->AddEntry("Filename", hSecCur);

	// ---------------------------------------------------
	hSecExp = pProp->SubSection("Export selection range");
	// ---------------------------------------------------
	hSLng = pProp->AddEntry("Min Lng", hSecExp);
	hSLat = pProp->AddEntry("Min Lat", hSecExp);
	hMLng = pProp->AddEntry("Max Lng", hSecExp);
	hMLat = pProp->AddEntry("Max Lat", hSecExp);
	hSLvl = pProp->AddEntry("Tile Level", hSecExp);
	hSXPx = pProp->AddEntry("Width pixels", hSecExp);
	hSYPx = pProp->AddEntry("Height pixels", hSecExp);
	
	// ---------------------------------------------------
	hSecImp = pProp->SubSection("Import properties");
	// ---------------------------------------------------
	hIFil = pProp->AddEntry("Filename", hSecImp);
	hIWid = pProp->AddEntry("Width [px]", hSecImp);
	hIHei = pProp->AddEntry("Height [px]", hSecImp);
	hILog = pProp->AddEntry("Log Level", hSecImp);
	
	hILT = pProp->AddEntry("Left-Top", hSecImp);
	hILB = pProp->AddEntry("Left-Btm", hSecImp);
	hIRB = pProp->AddEntry("Right-Btm", hSecImp);
	hIRT = pProp->AddEntry("Right-Top", hSecImp);

	// ---------------------------------------------------
	hSecPin = pProp->SubSection("Pin known location", hSecImp);
	// ---------------------------------------------------
	hPinX = pProp->AddEditControl("Pixel PosX", IDC_PINX, hSecPin);
	hPinY = pProp->AddEditControl("Pixel PosY", IDC_PINY, hSecPin);
	hPinW = pProp->AddEditControl("Longitude", IDC_PINW, hSecPin);
	hPinN = pProp->AddEditControl("Latitude", IDC_PINN, hSecPin);
	hPinF = pProp->AddEntry("Status", hSecPin);


	// ---------------------------------------------------
	hSecGfx = pProp->SubSection("Graphics Adjustments", hSecImp);
	// ---------------------------------------------------
	hGLig = pProp->AddSlider("Lightness", IDC_LIGHTNESS, hSecGfx);
	hGBri = pProp->AddSlider("Brightness", IDC_BRIGHTNESS, hSecGfx);
	hGGam = pProp->AddSlider("Gamma", IDC_GAMMA, hSecGfx);
	hGEdg = pProp->AddSlider("Edge Alpha", IDC_ALPHA, hSecGfx);

	// ---------------------------------------------------
	hSecClr = pProp->SubSection("Color Balance", hSecGfx);
	// ---------------------------------------------------
	hBRed = pProp->AddSlider("Red", IDC_RED, hSecClr);
	hBGrn = pProp->AddSlider("Green", IDC_GREEN, hSecClr);
	hBBlu = pProp->AddSlider("Blue", IDC_BLUE, hSecClr);

	pProp->SetSliderScale(hGLig, -0.4, 0.4, gcPropertyTree::Scale::LINEAR);
	pProp->SetSliderScale(hGBri, 0.625, 1.6, gcPropertyTree::Scale::LOG);
	pProp->SetSliderScale(hGGam, 0.5, 2.0, gcPropertyTree::Scale::LOG);
	pProp->SetSliderScale(hGEdg, 1.0, 1000.0, gcPropertyTree::Scale::LOG);
	pProp->SetSliderScale(hBRed, 0.8333, 1.2, gcPropertyTree::Scale::LOG);
	pProp->SetSliderScale(hBGrn, 0.8333, 1.2, gcPropertyTree::Scale::LOG);
	pProp->SetSliderScale(hBBlu, 0.8333, 1.2, gcPropertyTree::Scale::LOG);

	pProp->SetSliderValue(hGLig, 0.0);
	pProp->SetSliderValue(hGBri, 1.0);
	pProp->SetSliderValue(hGGam, 1.0);
	pProp->SetSliderValue(hGEdg, 32.0);
	pProp->SetSliderValue(hBRed, 1.0);
	pProp->SetSliderValue(hBGrn, 1.0);
	pProp->SetSliderValue(hBBlu, 1.0);
	
	pProp->ShowEntry(hSecImp, false);
	pProp->OpenEntry(hSecClr, false);
	pProp->OpenEntry(hSecPin, false);
	pProp->Update();

	
	memset(&SaveImage, 0, sizeof(OPENFILENAME));
	memset(&SaveDDS, 0, sizeof(OPENFILENAME));
	memset(&SaveElevation, 0, sizeof(OPENFILENAME));
	memset(SaveFileName, 0, sizeof(SaveFileName));
	memset(SaveFileTitle, 0, sizeof(SaveFileTitle));

	SaveImage.hwndOwner = pCore->GetRenderWindow();
	SaveImage.lStructSize = sizeof(OPENFILENAME);
	SaveImage.lpstrFile = SaveFileName;
	SaveImage.nMaxFile = sizeof(SaveFileName);
	SaveImage.lpstrFileTitle = SaveFileTitle;
	SaveImage.nMaxFileTitle = sizeof(SaveFileTitle);
	SaveImage.lpstrInitialDir = "";
	SaveImage.lpstrDefExt = NULL;
	SaveImage.lpstrFilter = 
"Image Files\0*.dds;*.png;*.jpg;*.bmp\0\
Direct Draw Surface (.dds)\0*.dds\0\
Windows Bitmap (.bmp)\0*.bmp\0\
Portable Network Graphics (.png)\0*.png\0\
JPEG Image (.jpg)\0*.jpg\0";
	SaveImage.nFilterIndex = 0;
	SaveImage.Flags = OFN_OVERWRITEPROMPT | OFN_NOCHANGEDIR;

	// ------------------------------------------------------

	SaveDDS.hwndOwner = pCore->GetRenderWindow();
	SaveDDS.lStructSize = sizeof(OPENFILENAME);
	SaveDDS.lpstrFile = SaveFileName;
	SaveDDS.nMaxFile = sizeof(SaveFileName);
	SaveDDS.lpstrFileTitle = SaveFileTitle;
	SaveDDS.nMaxFileTitle = sizeof(SaveFileTitle);
	SaveDDS.lpstrInitialDir = "";
	SaveDDS.lpstrDefExt = NULL;
	SaveDDS.lpstrFilter = "Image Files\0*.dds\0Direct Draw Surface (.dds)\0*.dds\0\0";
	SaveDDS.nFilterIndex = 0;
	SaveDDS.lpstrFileTitle = NULL;
	SaveDDS.nMaxFileTitle = 0;
	SaveDDS.Flags = OFN_OVERWRITEPROMPT | OFN_NOCHANGEDIR;

	// ------------------------------------------------------

	SaveElevation.hwndOwner = pCore->GetRenderWindow();
	SaveElevation.lStructSize = sizeof(OPENFILENAME);
	SaveElevation.lpstrFile = SaveFileName;
	SaveElevation.nMaxFile = sizeof(SaveFileName);
	SaveElevation.lpstrFileTitle = SaveFileTitle;
	SaveElevation.nMaxFileTitle = sizeof(SaveFileTitle);
	SaveElevation.lpstrInitialDir = "";
	SaveElevation.lpstrDefExt = NULL;
	SaveElevation.lpstrFilter =
"Elevation Files\0*.dds\0\
Direct Draw Surface (.dds)\0*.dds\0\0";
	SaveElevation.nFilterIndex = 0;
	SaveElevation.lpstrFileTitle = NULL;
	SaveElevation.nMaxFileTitle = 0;
	SaveElevation.Flags = OFN_OVERWRITEPROMPT | OFN_NOCHANGEDIR;
	
	bGo = true;
	return true;
}

// =================================================================================================
//
bool IsStringValid(const char *a, const char *b)
{
	const char *c = b;
	while (*a != 0) {
		while (true) {
			if (*a == *b) break;
			else b++;
			if (*b == 0) return false;
		}
		a++; b = c;
	}
	return true;
}

// =================================================================================================
//
bool ToolKit::CreatePin(Pin &pn, DRECT bounds)
{
	static const char valids[] = { "0123456789,.NWES " };
	gcCore::SurfaceSpecs specs;
	char buf[128]; bool bOk = false;
	int lenval = strlen(valids);

	if (!hSource) {
		pProp->SetValue(hPinF, "No Image Loaded");
		return false;
	}

	if (!pCore->GetSurfaceSpecs(hSource, &specs, sizeof(gcCore::SurfaceSpecs))) {
		pProp->SetValue(hPinF, "No Image Loaded");
		return false;
	}

	// --------------------------------------------------------------------------

	HWND hEC = pProp->GetControl(hPinX);
	if (hEC) GetWindowTextA(hEC, buf, sizeof(buf));
	pn.x = atoi(buf);

	if (pn.x<=0 || pn.x>specs.Width) {
		pProp->SetValue(hPinF, "Invalid X-Coordinate");
		return false;
	}

	// --------------------------------------------------------------------------

	hEC = pProp->GetControl(hPinY);
	if (hEC) GetWindowTextA(hEC, buf, sizeof(buf));
	pn.y = atoi(buf);

	if (pn.y<=0 || pn.y>specs.Height) {
		pProp->SetValue(hPinF, "Invalid Y-Coordinate");
		return false;
	}

	// --------------------------------------------------------------------------

	hEC = pProp->GetControl(hPinW);
	if (hEC) GetWindowTextA(hEC, buf, sizeof(buf));
	
	if (!IsStringValid(buf, valids)) {
		pProp->SetValue(hPinF, "Invalid chater(s)");
		return false;
	}
	
	char *pos = strstr(buf, "W"); if (!pos) pos = strstr(buf, "E");
	
	if (!pos) {
		pProp->SetValue(hPinF, "Missing 'E' or 'W'");
		return false;
	}

	if (*pos == 'W') { *pos = 0; pn.lng = -atof(buf) * RAD;	}
	else { *pos = 0; pn.lng = atof(buf) * RAD; }

	// --------------------------------------------------------------------------

	hEC = pProp->GetControl(hPinN);
	if (hEC) GetWindowTextA(hEC, buf, sizeof(buf));


	if (!IsStringValid(buf, valids)) {
		pProp->SetValue(hPinF, "Invalid chater(s)");
		return false;
	}

	pos = strstr(buf, "N"); if (!pos) pos = strstr(buf, "S");

	if (!pos) {
		pProp->SetValue(hPinF, "Missing 'N' or 'S'");
		return false;
	}

	if (*pos == 'N') { *pos = 0; pn.lat = atof(buf) * RAD; }
	else { *pos = 0; pn.lat = -atof(buf) * RAD; }

	if (pn.lng<bounds.left || pn.lng>bounds.right || pn.lat > bounds.top || pn.lat < bounds.bottom) {
		pProp->SetValue(hPinF, "Pin is outside bounds");
		return false;
	}

	pProp->SetValue(hPinF, "All is fine");
	return true;
}


// =================================================================================================
//
FMATRIX4 ToolKit::CreateWorldMatrix(OBJHANDLE hPlanet, double lng, double lat, double elev, float scale)
{
	double rad = oapiGetSize(hPlanet) + elev;
	MATRIX3 mRot;
	oapiGetRotationMatrix(hPlanet, &mRot);
	VECTOR3 vRot = mul(mRot, _V(0, 1, 0));

	VECTOR3 lpos, cpos, gp;
	oapiEquToLocal(hPlanet, lng, lat, rad, &lpos);
	lpos = mul(mRot, lpos);

	oapiCameraGlobalPos(&cpos);
	oapiGetGlobalPos(hPlanet, &gp);

	FMATRIX4 m;

	VECTOR3 y = unit(lpos);				// up
	VECTOR3 x = unit(crossp(y, vRot));	// west
	VECTOR3 z = crossp(x, y);			// north
	VECTOR3 p = ((gp + lpos) - cpos);

	x *= scale;
	y *= scale;
	z *= scale;

	m._x = FVECTOR4(x, 0.0f);
	m._y = FVECTOR4(y, 0.0f);
	m._z = FVECTOR4(z, 0.0f);
	m._p = FVECTOR4(p, 1.0f);

	return m;
}


// =================================================================================================
//
void ToolKit::DrawBox(FVECTOR3 *box, DWORD color)
{
	WORD Idx[24] = { 0, 1, 1, 2, 2, 3, 3, 0, 4, 5, 5, 6, 6, 7, 7, 4, 0, 4, 1, 5, 2, 6, 3, 7 };
	pCore->RenderLines(box, Idx, 8, ARRAYSIZE(Idx), &mIdent, color);
}


// =================================================================================================
//

void ToolKit::RenderTileBounds(gcCore::PickGround &pg, DWORD color)
{
	double size = oapiGetSize(hPlanet);
	VECTOR3 cpos, bpos;

	oapiCameraGlobalPos(&cpos);
	oapiGetGlobalPos(hPlanet, &bpos);
	cpos -= bpos;

	VECTOR3 V[4];
	V[0] = GetSurfacePosUnit(pg.Bounds.left, pg.Bounds.top);
	V[1] = GetSurfacePosUnit(pg.Bounds.left, pg.Bounds.bottom);
	V[2] = GetSurfacePosUnit(pg.Bounds.right, pg.Bounds.bottom);
	V[3] = GetSurfacePosUnit(pg.Bounds.right, pg.Bounds.top);

	double h = fabs(pg.Bounds.top - pg.Bounds.bottom);
	double s = size - cos(h*0.5) * size;

	FVECTOR3 box[8];
	for (int i = 0; i < 4; i++) box[i] = FVECTOR3(V[i] * (size + pg.emax + s) - cpos);
	for (int i = 0; i < 4; i++) box[i + 4] = FVECTOR3(V[i] * (size + pg.emin) - cpos);

	DrawBox(box, color);
}


// =================================================================================================
//
void ToolKit::RenderTileBounds(QTree *tn, DWORD color)
{

	// Acquire min/max elevation for this tile
	gcCore::PickGround pg = pCore->GetTileData(hMgr, tn->clng, tn->clat, tn->level);

	if (pg.hTile) {

		double size = oapiGetSize(hPlanet);
		VECTOR3 cpos, bpos;

		oapiCameraGlobalPos(&cpos);
		oapiGetGlobalPos(hPlanet, &bpos);
		cpos -= bpos;

		VECTOR3 V[4];
		V[0] = GetSurfacePosUnit(tn->Bounds.left, tn->Bounds.top);
		V[1] = GetSurfacePosUnit(tn->Bounds.left, tn->Bounds.bottom);
		V[2] = GetSurfacePosUnit(tn->Bounds.right, tn->Bounds.bottom);
		V[3] = GetSurfacePosUnit(tn->Bounds.right, tn->Bounds.top);

		double h = fabs(tn->Bounds.top - tn->Bounds.bottom);
		double s = size - cos(h*0.5) * size;
		
		FVECTOR3 box[8];
		for (int i = 0; i < 4; i++) box[i] = FVECTOR3(V[i] * (size + pg.emax + s) - cpos);
		for (int i = 0; i < 4; i++) box[i + 4] = FVECTOR3(V[i] * (size + pg.emin) - cpos);

		DrawBox(box, color);
	}
}


// =================================================================================================
//
void ToolKit::RenderSelection(sSelection *sel, int mode, DWORD color)
{
	if (mode == 0) {
		for each (selentry se in sel->area) RenderTileBounds(se.pNode, color);
		return;
	}

	float emin =  1e6;
	float emax = -1e6;

	if (mode == 1) {

		for each (selentry se in sel->area) {
			gcCore::PickGround pg = pCore->GetTileData(hMgr, se.pNode->clng, se.pNode->clat, se.pNode->level);
			emin = min(emin, pg.emin);
			emax = max(emax, pg.emax);
		}

		double size = oapiGetSize(hPlanet);
		VECTOR3 cpos, bpos;

		oapiCameraGlobalPos(&cpos);
		oapiGetGlobalPos(hPlanet, &bpos);
		cpos -= bpos;

		VECTOR3 V[4];
		V[0] = GetSurfacePosUnit(sel->bounds.left, sel->bounds.top);
		V[1] = GetSurfacePosUnit(sel->bounds.left, sel->bounds.bottom);
		V[2] = GetSurfacePosUnit(sel->bounds.right, sel->bounds.bottom);
		V[3] = GetSurfacePosUnit(sel->bounds.right, sel->bounds.top);

		double h = fabs(sel->bounds.top - sel->bounds.bottom);
		double w = fabs(sel->bounds.right - sel->bounds.left);
		double s = size - cos(max(w, h)*0.5) * size;

		FVECTOR3 box[8];
		for (int i = 0; i < 4; i++) box[i] = FVECTOR3(V[i] * (size + emax + s) - cpos);
		for (int i = 0; i < 4; i++) box[i + 4] = FVECTOR3(V[i] * (size + emin) - cpos);

		DrawBox(box, color);
	}
}



// =================================================================================================
// Main render Callback Routine called from D3D9Client for custom 'user' post scene rendering
//
void ToolKit::clbkRender()
{
	static COLOUR4 mat[5] = {
	{ 0, 1, 0, 0.6f },	
	{ 1, 0, 0, 0.6f },
	{ 1, 0, 1, 0.6f },
	{ 0.2f, 0.2f, 1, 0.6f },
	{ 1, 1, 1, 0.6f} };


	if (!bGo) return;
	if (!hPlanet) return;
	if (!dmSphere) return;

	bool bAutoHighlight = (SendDlgItemMessageA(hCtrlDlg, IDC_AUTOHIGHLIGHT, BM_GETCHECK, 0, 0) == BST_CHECKED);
	bool bHideClip = (SendDlgItemMessageA(hImpoDlg, IDC_HIDECLIP, BM_GETCHECK, 0, 0) == BST_CHECKED);
	bool bDisablePin = (SendDlgItemMessageA(hImpoDlg, IDC_DISABLEPIN, BM_GETCHECK, 0, 0) == BST_CHECKED);
	
	int select = SendDlgItemMessage(hCtrlDlg, IDC_SELECT, CB_GETCURSEL, 0, 0);
	int what = SendDlgItemMessage(hCtrlDlg, IDC_WHAT, CB_GETCURSEL, 0, 0);

	gcCore::PickGround pg; memset(&pg, 0, sizeof(gcCore::PickGround));
	QTree *pHover = NULL;


	if (bAutoHighlight || !pSecond || !hOverlay || oldsel.area.size() != 0)
	{
		pg = pCore->ScanScreen(xpos, ypos);
		if (pFirst) pHover = pRoot->FindNode(pg.lng, pg.lat, pFirst->level);
	}


	// Auto highlight the tile where mouse is hovering
	//
	if (bAutoHighlight || !pSecond || !hOverlay || oldsel.area.size() != 0)
	{
		if (select == Select::SRender) RenderTileBounds(pg, 0xFF00FF00);
		else {
			if (select == Select::SHighestOwn) {
				int flags = gcTileFlags::CACHE | gcTileFlags::TREE | gcTileFlags::TEXTURE;
				QTree *pSel = pRoot->HighestOwn(flags, pg.lng, pg.lat);
				RenderTileBounds(pSel, 0xFF00FF00);
				UpdateTileInfo(pSel, &pg);
			}
			else {
				int lvl = SelectedLevel();
				if (lvl > 0) {
					QTree *pSel = pRoot->FindNode(pg.lng, pg.lat, lvl);
					RenderTileBounds(pSel, 0xFF00FF00);
					UpdateTileInfo(pSel, &pg);
				}
			}
		}
	}
	

	if (pFirst && (pHover || pSecond))
	{

		QTree *pOther = (pSecond ? pSecond : pHover);

		int alvl = pFirst->level;
		int alng = pFirst->ilng;
		int alat = pFirst->ilat;
		int blng = pOther->ilng;
		int blat = pOther->ilat;

		selw = abs(alng - blng) + 1;
		selh = abs(alat - blat) + 1;

		int max = sp.MaxTexSize / 512;
		if (selw > max) selw = max;
		if (selh > max) selh = max;

		double width = pFirst->Width();
		double height = pFirst->Height();

		// Center of Top-Left Tile
		double clng = (pOther->clng < pFirst->clng ? pFirst->clng - width * double(selw - 1) : pFirst->clng);
		double clat = (pOther->clat > pFirst->clat ? pFirst->clat + height * double(selh - 1) : pFirst->clat);

		if (!pSecond) {

			selection.area.clear();

			for (int y = 0; y < selh; y++) {
				double lat = clat - height * double(y);
				for (int x = 0; x < selw; x++) {
					double lng = clng + width * double(x);
					selentry se;
					se.pNode = pRoot->FindNode(lng, lat, alvl);
					se.x = x;
					se.y = y;
					selection.area.push_back(se);
				}
			}

			selection.selw = selw;
			selection.selh = selh;
			selection.slvl = alvl;
			selection.pLeftTop = pRoot->FindNode(clng, clat, alvl);
			selection.pRightBottom = pRoot->FindNode(clng + width * double(selw - 1), clat - height * double(selh - 1), alvl);

			double mlng = min(pOther->Bounds.left, pFirst->Bounds.left);
			double mlat = max(pOther->Bounds.top, pFirst->Bounds.top);
			double xlng = max(pOther->Bounds.right, pFirst->Bounds.right);
			double xlat = min(pOther->Bounds.bottom, pFirst->Bounds.bottom);

			selection.bounds.left = mlng;
			selection.bounds.top = mlat;
			selection.bounds.right = xlng;
			selection.bounds.bottom = xlat;

			pProp->SetValue(hSLng, selection.bounds.left, 4, gcPropertyTree::Format::LONGITUDE);
			pProp->SetValue(hSLat, selection.bounds.top, 4, gcPropertyTree::Format::LATITUDE);
			pProp->SetValue(hMLng, selection.bounds.right, 4, gcPropertyTree::Format::LONGITUDE);
			pProp->SetValue(hMLat, selection.bounds.bottom, 4, gcPropertyTree::Format::LATITUDE);
			pProp->SetValue(hSXPx, 512 * selw);
			pProp->SetValue(hSYPx, 512 * selh);
		}
	}


	if (oldsel.area.size() != 0) {
		if (!bHideClip) RenderSelection(&oldsel, 1, 0xFF0000FF);
	}


	if (selection.area.size() != 0) {
		if (!bHideClip || !hSource) {
			if (hSource) RenderSelection(&selection, 1, 0xFFFFFF00);
			else RenderSelection(&selection, 0, 0xFFFFFF00);
		}
	}

	if (hOverlay) {
		pProp->SetValue(hILT, LngLat(points[0]));
		pProp->SetValue(hILB, LngLat(points[1]));
		pProp->SetValue(hIRB, LngLat(points[2]));
		pProp->SetValue(hIRT, LngLat(points[3]));
	}

	// Render corner sphere markers
	//
	if (selection.area.size() != 0 && hSource && hOverlay) {
		
		VECTOR3 cpos, bpos;
		oapiCameraGlobalPos(&cpos);
		oapiGetGlobalPos(hPlanet, &bpos);
		cpos -= bpos;

		for (int i = 0; i < 4; i++) {

			int c = i;
			if (sel_corner == i) c = 4;
		
			FVECTOR4 base = mat[c];
			FVECTOR4 emis = mat[c];
			emis.rgb *= 0.5f;	
	
			VECTOR3 uSP = GetSurfacePosUnit(points[i].lng, points[i].lat) * (oapiGetSize(hPlanet) + points[i].elev);
			float scale = float(tan(0.75*RAD) * length(uSP - cpos));
			
			points[i].mWorld = CreateWorldMatrix(hPlanet, points[i].lng, points[i].lat, points[i].elev, scale);

			pCore->SetMeshMaterial(dmSphere, 0, MatProp::Diffuse, &base);
			pCore->SetMeshMaterial(dmSphere, 0, MatProp::Light, &emis);
			pCore->RenderMesh(dmSphere, &points[i].mWorld);
		}
	}

	pProp->Update();
}


// =================================================================================================
//
void ToolKit::Export()
{
	int what = SendDlgItemMessage(hCtrlDlg, IDC_WHAT, CB_GETCURSEL, 0, 0);
	int flags = gcTileFlags::TREE | gcTileFlags::CACHE;

	if (what == 0) flags |= gcTileFlags::TEXTURE;
	if (what == 1) flags |= gcTileFlags::MASK;
	if (what == 2) {
		ExportElev();
		return;
	}
	
	SURFHANDLE hSrf = oapiCreateSurfaceEx(selw * 512, selh * 512, OAPISURFACE_RENDERTARGET);

	if (hSrf) {

		Sketchpad *pSkp = oapiGetSketchpad(hSrf);

		if (pSkp) {

			for each (selentry se in selection.area)
			{
				SubTex st = se.pNode->GetSubTexRange(flags);
				if (st.pNode) {
					SURFHANDLE hSrc = st.pNode->GetTexture(flags); // Do not release
					if (hSrc) {
						RECT t = { se.x * 512, se.y * 512 , (se.x + 1) * 512, (se.y + 1) * 512 };
						pSkp->StretchRect(hSrc, &st.range, &t);
					}
				}
			}
			oapiReleaseSketchpad(pSkp);
		}


		if (SaveFile(SaveImage)) {
			if (!pCore->SaveSurface(SaveImage.lpstrFile, hSrf)) {
				oapiWriteLogV("Failed to create a file [%s]", SaveImage.lpstrFile);
			}	
		}

		oapiReleaseTexture(hSrf);
	}
	else oapiWriteLog("hSrf == NULL");
}


// =================================================================================================
//
SURFHANDLE ToolKit::GetBaseElevation(int elev_fmt)
{

	int Width = 256 * selw;
	int Height = 256 * selh;

	float *pFloat = NULL;
	INT16 *pInt = NULL;

	if (elev_fmt == 0) pInt = new INT16[Width * Height];
	if (elev_fmt == 1) pFloat = new float[Width * Height];

	for each (selentry se in selection.area)
	{
		int pos = se.x * 256 + se.y * 256 * Width;
		INT16 *pElev = se.pNode->GetElevation();
		for (int y = 0; y < 256; y++)
		{
			int q = y * Width + pos;
			int z = y * 258;
			for (int x = 0; x < 256; x++) {
				if (pFloat) pFloat[q + x] = float(pElev[z + x + 259]);
				if (pInt) pInt[q + x] = pElev[z + x + 259];
			}
		}
	}

	DWORD flg = OAPISURFACE_SYSMEM;
	if (elev_fmt == 0) flg |= OAPISURFACE_PF_S16R;
	if (elev_fmt == 1) flg |= OAPISURFACE_PF_F32R;

	/*
	SURFHANDLE hSrf = oapiCreateSurfaceEx(selw * 256, selh * 256, flg);

	if (hSrf)
	{
		gcCore::Lock lock;
		if (pCore->LockSurface(hSrf, &lock, true))
		{
			BYTE *pPtr = (BYTE*)lock.pData;
			for (int y = 0; y < Height; y++)
			{
				int q = y * Width;
				for (int x = 0; x < Width; x++) {
					if (elev_fmt == 0) ((INT16*)pPtr)[x] = pInt[x + q];
					if (elev_fmt == 1) ((float*)pPtr)[x] = pFloat[x + q];
				}
				pPtr += lock.Pitch;
			}

			pCore->ReleaseLock(hSrf);
		}
	}

	return hSrf;*/

	return NULL;
}


// =================================================================================================
//
void ToolKit::ExportElev()
{

	for each (selentry se in selection.area)
	{
		INT16 *pElev = se.pNode->GetElevation();
		if (!pElev) {
			char msg[256];
			sprintf_s(msg, 256, "Tile (iLng=%d, iLat=%d) has no elevation for level %d", se.pNode->ilng, se.pNode->ilat, selection.slvl);
			MessageBoxA(pCore->GetRenderWindow(), msg, "Error:", MB_OK);
			return;
		}
	}


	if (GetSaveFileNameA(&SaveElevation)) {

		int type = 0;

		// Pick the file type from a file name
		if (strstr(SaveImage.lpstrFile, ".dds") || strstr(SaveImage.lpstrFile, ".DDS")) type = 1;

		if (type == 0) {
			// If above fails then use selected "filter" to appeand file "id".
			if (SaveImage.nFilterIndex == 0) strcat_s(SaveImage.lpstrFile, MAX_PATH, ".dds"), type = 1;
		}

		if (type == 0) {
			MessageBoxA(pCore->GetRenderWindow(), "Invalid File Type", "Error:", MB_OK);
			return;
		}

		int fmt = 0;

		SURFHANDLE hSrf = GetBaseElevation(fmt);

		if (hSrf) {
			if (!pCore->SaveSurface(SaveImage.lpstrFile, hSrf)) {
				MessageBoxA(pCore->GetRenderWindow(), "Failed to Save a file", "Error:", MB_OK);
				return;
			}	
			oapiReleaseTexture(hSrf);
		}
	}
}


// =================================================================================================
//
void ToolKit::MakeProgress()
{
	progress++;
	SendDlgItemMessage(hProgDlg, IDC_PROGBAR, PBM_SETPOS, progress, 0);
	UpdateWindow(hProgDlg);
}


// =================================================================================================
//
void ToolKit::BakeImport()
{
	char cur[16];

	int idx = SendDlgItemMessage(hImpoDlg, IDC_IMPLEVEL, CB_GETCURSEL, 0, 0);
	SendDlgItemMessage(hImpoDlg, IDC_IMPLEVEL, CB_GETLBTEXT, idx, (LPARAM)cur);
	int maxlvl = atoi(cur);

	if (maxlvl < selection.slvl) {
		MessageBoxA(pCore->GetRenderWindow(), "Clip level can't be higher than Bake level", "Error:", MB_OK);
		return;
	}

	
	if (MessageBox(pCore->GetRenderWindow(), "Bake and Write the tiles in 'OrbiterRoot/TerrainToolBox/' Folder ?", "Are you sure", MB_YESNO | MB_ICONEXCLAMATION) != IDYES) return;


	SURFHANDLE hTemp = oapiCreateSurfaceEx(512, 512, OAPISURFACE_RENDERTARGET | OAPISURFACE_TEXTURE);

	progress = 0;

	int nTiles = selection.area.size();
	nTiles += (nTiles / 2 + nTiles / 4 + nTiles / 8);
	
	hProgDlg = CreateDialogParamA(hModule, MAKEINTRESOURCE(IDD_PROGRESS), hAppMainWnd, gDlgProc, 0);
	SendDlgItemMessage(hProgDlg, IDC_PROGBAR, PBM_SETRANGE, 0, MAKELONG(0, nTiles));
	SendDlgItemMessage(hProgDlg, IDC_PROGBAR, PBM_SETPOS, 0, 0);

	ShowWindow(hProgDlg, SW_SHOW);

	list<QTree *> parents;

	for each (selentry s in selection.area)
	{
		int rv = s.pNode->SaveTile(hOverlaySrf, hTemp, selection.bounds, maxlvl - 4, 1.0f);
		if (rv < 0) {
			oapiWriteLogV("ERROR: s.pNode->SaveTile() returned %d", rv);
			return;
		}
		MakeProgress();
		parents.push_back(s.pNode->GetParent());
	}

	parents.unique();

	BakeParents(hTemp, parents);

	oapiReleaseTexture(hTemp);
	DestroyWindow(hProgDlg);
}



// =================================================================================================
//
void ToolKit::BakeParents(SURFHANDLE hTemp, list<QTree *> parents)
{
	list<QTree *> grands;

	for each (QTree * qt in parents)
	{
		float alpha = 1.0f;
		if (qt->HasOwnTex() == false) grands.push_back(qt->GetParent());
		else alpha = 0.5f;
		int rv = qt->SaveTile(hOverlaySrf, hTemp, selection.bounds, -1, alpha);
		MakeProgress();
		if (rv < 0) {
			oapiWriteLogV("ERROR: qt->SaveTile() returned %d", rv);
			return;
		}
	}

	grands.unique();
	if (grands.size()) BakeParents(hTemp, grands);
}



// =================================================================================================
//
void ToolKit::ImportImage()
{
	char buf[MAX_PATH + 32];

	if (GetOpenFileNameA(&SaveImage)) 
	{
		if (hSource) {
			oapiReleaseTexture(hSource);
			hSource = NULL;
		}

		hSource = oapiLoadSurfaceEx(SaveImage.lpstrFile, OAPISURFACE_TEXTURE, true);

		if (!hSource) {
			sprintf_s(buf, 32 + MAX_PATH, "Unable to load file [%s]", SaveImage.lpstrFile);
			MessageBoxA(hAppMainWnd, buf, "Error:", MB_OK);
			return;
		}

		gcCore::SurfaceSpecs specs;

		if (pCore->GetSurfaceSpecs(hSource, &specs, sizeof(gcCore::SurfaceSpecs))) {

			pProp->SetValue(hIFil, SaveImage.lpstrFileTitle);
			pProp->SetValue(hIWid, specs.Width);
			pProp->SetValue(hIHei, specs.Height);
			
			AutoSelectCorners();
			ComputeLevel();
			UpdateOverlay();

			SetMode(MODE_IMPORT);
		}
	}
}


// =================================================================================================
// Update the Overlay image seen on a planets surface
//
void ToolKit::UpdateOverlay()
{
	if (!hSource || !hOverlaySrf) return;	// No graphics

	bool bTransp = (SendDlgItemMessageA(hImpoDlg, IDC_TRANSP, BM_GETCHECK, 0, 0) == BST_CHECKED);
	bool bDisPin = (SendDlgItemMessageA(hImpoDlg, IDC_DISABLEPIN, BM_GETCHECK, 0, 0) == BST_CHECKED);
	
	// Acquire Sketchpad for Overlay Surface
	Sketchpad *pSkp = oapiGetSketchpad(hOverlaySrf);

	if (pSkp) {

		float bri = float(pProp->GetSliderValue(hGBri));
		float red = float(pProp->GetSliderValue(hBRed) * bri);
		float grn = float(pProp->GetSliderValue(hBGrn) * bri);
		float blu = float(pProp->GetSliderValue(hBBlu) * bri);
		float lgh = float(pProp->GetSliderValue(hGLig));
		float gma = float(pProp->GetSliderValue(hGGam));

		pSkp->ColorCompatibility(false);
		pSkp->SetBlendState(Sketchpad::BlendState::COPY);	

		// Clear/Initialize the background image
		if (hOverlayBkg) pSkp->CopyRect(hOverlayBkg, NULL, 0, 0);

		FMATRIX4 mColor;
		mColor.Ident();
		mColor.m11 = red;
		mColor.m22 = grn;
		mColor.m33 = blu;
		mColor.m41 = lgh;
		mColor.m42 = lgh;
		mColor.m43 = lgh;

		if (bTransp) mColor.m44 = 0.5f;

		// Setup a color matrix for corrections
		pSkp->SetColorMatrix(&mColor);	

		// Setup gamma correction
		pSkp->SetRenderParam(Sketchpad::RenderParam::PRM_GAMMA, &FVECTOR4(gma, gma, gma, 1.0f));

		SIZE size;
		pSkp->GetRenderSurfaceSize(&size);

		FVECTOR2 pt[4];
		FVECTOR2 in[4];

		double w = fabs(selection.bounds.left - selection.bounds.right);
		double h = fabs(selection.bounds.top - selection.bounds.bottom);

		if (w > PI) w = PI2 - w;

		for (int i = 0; i < 4; i++) {
			pt[i].x = float((points[i].lng - selection.bounds.left) / w) * float(size.cx);
			pt[i].y = float((selection.bounds.top - points[i].lat) / h) * float(size.cy);
		}
		
		int nGrad = int(pProp->GetSliderValue(hGEdg));

		for (int i = 0; i < 4; i++) {
			int q = (i - 1) < 0 ? 3 : i - 1;
			int w = (i + 1) > 3 ? 0 : i + 1;
			FVECTOR2 a = unit((pt[q] - pt[i]) + (pt[w] - pt[i]));
			in[i] = pt[i] + a * float(nGrad);
		}

		// Copy src to tgt with corrections
		pSkp->CopyTetragon(hSource, NULL, pt);	

		// Restore default Matrix and Gamma
		pSkp->SetColorMatrix();
		pSkp->SetRenderParam(Sketchpad::RenderParam::PRM_GAMMA);

		// Render Edge transparency
		pSkp->SetBlendState((Sketchpad::BlendState)(Sketchpad::BlendState::COPY_ALPHA | Sketchpad::BlendState::FILTER_ANISOTROPIC));
		pSkp->ColorFill(0x00000000, NULL); // Clear Alpha Channel
		
		FVECTOR2 a[4] = { pt[0], in[0], in[3], pt[3] };
		pSkp->CopyTetragon(hGradient, NULL, a);

		FVECTOR2 b[4] = { pt[1], in[1], in[0], pt[0] };
		pSkp->CopyTetragon(hGradient, NULL, b);

		FVECTOR2 c[4] = { pt[2], in[2], in[1], pt[1] };
		pSkp->CopyTetragon(hGradient, NULL, c);

		FVECTOR2 d[4] = { pt[3], in[3], in[2], pt[2] };
		pSkp->CopyTetragon(hGradient, NULL, d);
		
		FVECTOR2 e[4] = { in[0], in[1], in[2], in[3] };
		pSkp->FillTetragon(0xFFFFFFFF, e);
	
		// Make sure that the overlay border is transparent
		pSkp->QuickBrush(0);	
		pSkp->QuickPen(FVECTOR4(1.0f, 1.0f, 1.0f, 0.0f).dword_abgr(), 3.0f);
		pSkp->Rectangle(0, 0, size.cx, size.cy);
		
		pSkp->SetBlendState();
		oapiReleaseSketchpad(pSkp);

		// Generate Mip sublevels
		//
		pCore->GenerateMipmaps(hOverlaySrf);
		

		// Process/Fix the edge transparency from mipmaps, the outmost rect fully transpatent in every level
		//
		for (int i = 0;;i++) {
			SURFHANDLE hMip = pCore->GetMipSublevel(hOverlaySrf, i + 1);
			if (hMip) {
				SIZE size;
				Sketchpad *pSkp = oapiGetSketchpad(hMip);
				pSkp->SetBlendState(Sketchpad::BlendState::COPY_ALPHA);
				pSkp->GetRenderSurfaceSize(&size);
				pSkp->ColorCompatibility(false);
				pSkp->QuickPen(0x00FFFFFF);
				pSkp->Rectangle(0, 0, size.cx, size.cy);
				oapiReleaseSketchpad(pSkp);
				oapiReleaseTexture(hMip);
			}
			else break;
		}
	}
}



// =================================================================================================
//
void ToolKit::ComputeLevel()
{
	char buf[16];
	char cur[16];
	gcCore::SurfaceSpecs specs;
	
	if (!hSource) return;

	if (pCore->GetSurfaceSpecs(hSource, &specs, sizeof(gcCore::SurfaceSpecs))) {

		// Figure out the level the input texture is good for ------------------
		//
		double w1 = fabs(points[0].lng - points[3].lng);
		double w2 = fabs(points[1].lng - points[2].lng);
		double h1 = fabs(points[0].lat - points[1].lat);
		double h2 = fabs(points[2].lat - points[3].lat);
		if (w1 > PI) w1 = PI2 - w1;
		if (w2 > PI) w2 = PI2 - w2;

		double sw = fabs(selection.bounds.right - selection.bounds.left);
		double sh = fabs(selection.bounds.top - selection.bounds.bottom);
		if (sw > PI) sw = PI2 - sw;

		double wr = sw / min(w1, w2);
		double hr = sh / min(h1, h2);

		double logw = double(selection.slvl) + log(double(specs.Width) * min(1.0, wr) / double(selection.selw * 512)) / log(2.0);
		double logh = double(selection.slvl) + log(double(specs.Height) * min(1.0, hr) / double(selection.selh * 512)) / log(2.0);
		double logl = max(logw, logh);
		double frac = logl - floor(logl);
		int lvl = int(floor(logl)) + 4;

		// Display current level 
		pProp->SetValue(hILog, logl + 4.0, 2);

		// Get Current value from combobox
		int idx = SendDlgItemMessage(hImpoDlg, IDC_IMPLEVEL, CB_GETCURSEL, 0, 0);
		SendDlgItemMessage(hImpoDlg, IDC_IMPLEVEL, CB_GETLBTEXT, idx, (LPARAM)cur);
	

		sprintf_s(buf, sizeof(buf), "%d", lvl);
		SendDlgItemMessage(hImpoDlg, IDC_IMPLEVEL, CB_RESETCONTENT, 0, 0);
		SendDlgItemMessage(hImpoDlg, IDC_IMPLEVEL, CB_ADDSTRING, 0, (LPARAM)buf);
		
		if (frac > 0.25) {
			sprintf_s(buf, sizeof(buf), "%d", lvl + 1);
			SendDlgItemMessage(hImpoDlg, IDC_IMPLEVEL, CB_ADDSTRING, 0, (LPARAM)buf);
		}

		// Select Item from the combobox 
		if (SendDlgItemMessage(hImpoDlg, IDC_IMPLEVEL, CB_SELECTSTRING, -1, (LPARAM)cur) == CB_ERR) {
			SendDlgItemMessage(hImpoDlg, IDC_IMPLEVEL, CB_SETCURSEL, 0, 0);
		}
		

		// Setup a selected level --------------------------------------
		//
		idx = SendDlgItemMessage(hImpoDlg, IDC_IMPLEVEL, CB_GETCURSEL, 0, 0);
		SendDlgItemMessage(hImpoDlg, IDC_IMPLEVEL, CB_GETLBTEXT, idx, (LPARAM)cur);
		int sellvl = atoi(cur);

		int dlvl = max(0, (sellvl - (selection.slvl + 4)));
		int Width = (1 << dlvl) * selection.selw * 512;
		int Height = (1 << dlvl) * selection.selh * 512;

		if (hOverlaySrf) {
			gcCore::SurfaceSpecs ovlspecs;
			if (pCore->GetSurfaceSpecs(hOverlaySrf, &ovlspecs, sizeof(gcCore::SurfaceSpecs))) {
				if (ovlspecs.Width != Width || ovlspecs.Height != Height) {
					oapiReleaseTexture(hOverlaySrf);
					hOverlaySrf = NULL;
				}
			}
		}

		if (!hOverlaySrf) {

			if (hOverlayBkg) oapiReleaseTexture(hOverlayBkg);

			hOverlaySrf = oapiCreateSurfaceEx(Width, Height, OAPISURFACE_PF_ARGB | OAPISURFACE_RENDERTARGET | OAPISURFACE_TEXTURE | OAPISURFACE_MIPMAPS);
			hOverlayBkg = oapiCreateSurfaceEx(Width, Height, OAPISURFACE_PF_XRGB | OAPISURFACE_RENDERTARGET | OAPISURFACE_TEXTURE);
			hOverlay = pCore->AddGlobalOverlay(hMgr, selection.bounds.vec, hOverlaySrf, hOverlay);

			Sketchpad *pSkp = oapiGetSketchpad(hOverlayBkg);

			if (pSkp) {

				for each (selentry s in selection.area)
				{
					SubTex st = s.pNode->GetSubTexRange(gcTileFlags::TEXTURE | gcTileFlags::TREE | gcTileFlags::CACHE);
					if (st.pNode) {
						RECT src, tgt;
						st.pNode->MapRect(selection.bounds, hOverlaySrf, src, tgt);
						SURFHANDLE hSrf = st.pNode->GetTexture();
						if (hSrf) pSkp->StretchRect(hSrf, &tgt, &src);
					}
				}
				oapiReleaseSketchpad(pSkp);
			}
		}
	}
}


// =================================================================================================
//
void ToolKit::AutoSelectCorners()
{
	points[0].lng = selection.bounds.left;
	points[0].lat = selection.bounds.top;
	points[1].lng = selection.bounds.left;
	points[1].lat = selection.bounds.bottom;
	points[2].lng = selection.bounds.right;
	points[2].lat = selection.bounds.bottom;
	points[3].lng = selection.bounds.right;
	points[3].lat = selection.bounds.top;

	for (int i = 0; i < 4; i++) {
		double elev;
		HTILE hTile = pCore->GetTile(hMgr, points[i].lng, points[i].lat);
		if (hTile) {
			if (pCore->GetElevation(hTile, points[i].lng, points[i].lat, &elev) >= 0) {
				points[i].elev = elev;
			}
		}
	}
}


// =================================================================================================
//
void ToolKit::UpdateTileInfo(QTree *pF, gcCore::PickGround *pP)
{
	if (!pF || !pP) return;
	char name[64];
	sprintf_s(name, 63, "Surf/%d/%d/%d.dds", pF->level + 4, pF->ilat, pF->ilng);
	pProp->SetValue(hCLng, pP->lng, 4, gcPropertyTree::LONGITUDE);
	pProp->SetValue(hCLat, pP->lat, 4, gcPropertyTree::LATITUDE);
	pProp->SetValue(hCEle, pP->elev, 1);
	pProp->SetValue(hCFil, string(name));
	pProp->Update();
}


// =================================================================================================
//
int	ToolKit::SelectedLevel()
{
	int select = SendDlgItemMessage(hCtrlDlg, IDC_SELECT, CB_GETCURSEL, 0, 0);
	return max(select - 1, -1);
}


// =================================================================================================
// Called from D3D9Client when user clicks the ground
//
void ToolKit::clbkMouseClick(int iUser, void *pData)
{
	if (iUser != sizeof(gcCore::PickGround)) return;
	if (!pRoot) return;
	if (!pCore) return;

	int select = SendDlgItemMessage(hCtrlDlg, IDC_SELECT, CB_GETCURSEL, 0, 0);
	int what = SendDlgItemMessage(hCtrlDlg, IDC_WHAT, CB_GETCURSEL, 0, 0);

	gcCore::PickGround *pPick = (gcCore::PickGround *)pData;

	double clng = pPick->lng;
	double clat = pPick->lat;

	int flags = gcTileFlags::TREE | gcTileFlags::CACHE;

	if (what == Select::WTexture) flags |= gcTileFlags::TEXTURE;
	if (what == Select::WNightlight) flags |= gcTileFlags::MASK;
	if (what == Select::WElevation) flags |= gcTileFlags::ELEVATION | gcTileFlags::ELEV_MOD;
	
	if (pPick->hTile) 
	{
		if (selection.area.size() == 0 || !hOverlay) 
		{
			if (pPick->msg == WM_LBUTTONDOWN)
			{
				pFirst = NULL;
				pSecond = NULL;

				if (select == Select::SRender) pFirst = pRoot->FindNode(clng, clat, pPick->level);
				else if (select == Select::SHighestOwn) pFirst = pRoot->HighestOwn(flags, clng, clat);
				else {
					int lvl = SelectedLevel();
					if (lvl > 0) pFirst = pRoot->FindNode(clng, clat, lvl);
				}
				return;
			}
		}

		if (pFirst && !pSecond) 
		{
			if (pPick->msg == WM_LBUTTONUP)
			{
				oldsel.area.clear();
				pSecond = pRoot->FindNode(pPick->lng, pPick->lat, pFirst->level);
				ComputeLevel();
				UpdateOverlay();
				return;
			}
		}
	}
}


// =================================================================================================
// Orbiter Module Callback
//
bool ToolKit::clbkProcessMouse(UINT event, DWORD state, DWORD x, DWORD y)
{
	if (!pCore) return false;

	if (!hOverlay) {
		xpos = x, ypos = y;
		return false;
	}

	gcCore::PickMeshStruct pick; memset(&pick, 0, sizeof(pick));
	pick.dist = 1e16f;
	pick.grp_inst = -1;

	if (event == WM_LBUTTONDOWN) {
		down_corner = -1;
		if (hSource && hOverlay) {
			for (int i = 0; i < 4; i++) {
				if (pCore->PickMesh(&pick, dmSphere, &points[i].mWorld, short(x), short(y))) {
					sel_corner = i;
					down_corner = i;
				}
			}
		}
	}

	if (event == WM_LBUTTONUP) down_corner = -1;

	if ((x != xpos || y != ypos) && down_corner >= 0) {
		gcCore::PickGround pg = pCore->ScanScreen(x, y);
		points[down_corner].lng = pg.lng;
		points[down_corner].lat = pg.lat;
		points[down_corner].elev = pg.elev;
		ComputeLevel();
		UpdateOverlay();
	}

	xpos = x, ypos = y;

	return false;
}


// =================================================================================================
// Orbiter Module Callback
//
bool ToolKit::clbkProcessKeyboardBuffered(DWORD key, char kstate[256], bool simRunning)
{
	if (!pCore) return false;

	if (key == OAPI_KEY_ESCAPE) {
		selection.area.clear();
		oldsel.area.clear();
		pFirst = NULL;
		pSecond = NULL;
	}

	return false;
}


// =================================================================================================
//
bool ToolKit::SaveFile(OPENFILENAMEA &SaveImage)
{
	if (GetSaveFileNameA(&SaveImage)) {

		int type = 0;

		// Pick the file type from a file name
		if (strstr(SaveImage.lpstrFile, ".dds") || strstr(SaveImage.lpstrFile, ".DDS")) type = 1;
		if (strstr(SaveImage.lpstrFile, ".bmp") || strstr(SaveImage.lpstrFile, ".BMP")) type = 2;
		if (strstr(SaveImage.lpstrFile, ".png") || strstr(SaveImage.lpstrFile, ".PGN")) type = 3;
		if (strstr(SaveImage.lpstrFile, ".jpg") || strstr(SaveImage.lpstrFile, ".JPG")) type = 4;
		if (strstr(SaveImage.lpstrFile, ".jpeg") || strstr(SaveImage.lpstrFile, ".JPEG")) type = 4;

		if (type == 0) {
			// If above fails then use selected "filter" to appeand file "id".
			if (SaveImage.nFilterIndex == 0) strcat_s(SaveImage.lpstrFile, MAX_PATH, ".jpg");
			if (SaveImage.nFilterIndex == 1) strcat_s(SaveImage.lpstrFile, MAX_PATH, ".dds");
			if (SaveImage.nFilterIndex == 2) strcat_s(SaveImage.lpstrFile, MAX_PATH, ".bmp");
			if (SaveImage.nFilterIndex == 3) strcat_s(SaveImage.lpstrFile, MAX_PATH, ".png");
			if (SaveImage.nFilterIndex == 4) strcat_s(SaveImage.lpstrFile, MAX_PATH, ".jpg");
		}
		return true;
	}
	return false;
}


// =================================================================================================
//
VECTOR3 ToolKit::GetSurfacePosUnit(double lng, double lat)
{
	MATRIX3 mRot;
	double w = cos(lat);
	oapiGetRotationMatrix(hPlanet, &mRot);
	return mul(mRot, _V(w*cos(lng), sin(lat), w*sin(lng)));
}


// =================================================================================================
// Orbiter Module Callback
//
void ToolKit::clbkSimulationStart(RenderMode rm)
{
	if (!pCore) return;
}


// =================================================================================================
// Orbiter Module Callback, ** USELESS **, By the time this is called D3D9Client and all graphics
// servises has shutdown long ago.
//
void ToolKit::clbkSimulationEnd()
{
}


// =================================================================================================
//
void ToolKit::clbkPreStep(double simt, double simdt, double mjd)
{
	if (!pCore) return;
}