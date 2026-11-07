// ==================================================================
// Copyright (c) 2021-2026 Jarmo Nikkanen
// Licensed under the MIT License
// ==================================================================


#define ORBITER_MODULE

#include "OrbiterAPI.h"
#include "VesselAPI.h"
#include "ModuleAPI.h"
#include "DrawAPI.h"
#include "gcCoreAPI.h"
#include "ToolKit.h"
#include "gcPropertyTree.h"
#include "QTree.h"
#include "imgui.h"
#include <vector>
#include <list>

using namespace std;

ORBITER_MODULE_INFO("Developer resources and samples", "Terrain and Base Development ToolKit");

ToolKit *g_pTK = NULL;
gcCore2* g_pCore = NULL;

// helper function to get address of a temporary
// NB: use with caution
template<typename T>
const T* ptr(const T& x) { return &x; }


// =================================================================================================
// D3D9Client Callback Wrappers
// =================================================================================================
//
void RenderClbk(int iUser, void *pUser, void *pParam)
{
	((ToolKit*)pParam)->clbkRender();
}

void MouseClickClbk(int iUser, void *pUser, void *pParam)
{
	((ToolKit*)pParam)->clbkMouseClick(iUser, pUser);
}

static void DrawSelectionClbk(HNODE hNode, void *pUser)
{
	((ToolKit*)pUser)->DrawSelectionOptions();
}

static void DrawImportClbk(HNODE hNode, void *pUser)
{
	((ToolKit*)pUser)->DrawImportOptions();
}

static void DrawPropertiesClbk(HNODE hNode, void *pUser)
{
	((ToolKit*)pUser)->DrawProperties();
}


string LngLat(Position p);


class BakeDialog : public ImGuiDialog
{
public:
	BakeDialog(ToolKit *pTK) : ImGuiDialog("Baking tiles"), pTK(pTK) {}

	void Display() override
	{
		ImGuiViewport *vp = ImGui::GetMainViewport();
		ImGui::SetNextWindowPos(vp->GetCenter(), ImGuiCond_Appearing, ImVec2(0.5f, 0.5f));
		ImGuiWindowFlags fl = ImGuiWindowFlags_AlwaysAutoResize | ImGuiWindowFlags_NoCollapse | ImGuiWindowFlags_NoSavedSettings;
		if (ImGui::Begin(name.c_str(), NULL, fl)) OnDraw();
		ImGui::End();
		if (!pTK->IsBaking()) oapiCloseDialog(this);
	}

	void OnDraw() override { pTK->DrawBakeProgress(); }

private:
	ToolKit *pTK;
};



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
void ToolKit::OnPropertyEvent(HPROP hEntry, WORD id, gcPropertyTree::Event ev)
{
	switch (id)
	{
	case IDC_LIGHTNESS:
	case IDC_BRIGHTNESS:
	case IDC_GAMMA:
	case IDC_ALPHA:
	case IDC_RED:
	case IDC_GREEN:
	case IDC_BLUE:
	case IDC_ALPHAEDG:
		if (!UpdateOverlays()) oapiWriteLog((char*)"UpdateOverlays() Failed");
		break;

	case IDC_LAYER:
		UpdateOverlays();
		break;

	default:
		break;
	}
}


// ===============================================================================================
//
void ToolKit::DrawSelectionOptions()
{
	static const char *what[] = { "Texture", "Nightlights" };

	ImGui::Checkbox("Display cursor", &bAutoHighlight);
	ImGui::Checkbox("Display selection", &bDispSel);

	ImGui::TextUnformatted("Export or bake level");
	ImGui::SetNextItemWidth(-FLT_MIN);
	char preview[32];
	if (iSelect == Select::SRender) strcpy(preview, "Current render level");
	else if (iSelect == Select::SHighestOwn) strcpy(preview, "Highest existing");
	else snprintf(preview, sizeof(preview), "Level %d", iSelect + 3);
	if (ImGui::BeginCombo("##select", preview)) {
		if (ImGui::Selectable("Current render level", iSelect == Select::SRender)) iSelect = Select::SRender;
		if (ImGui::Selectable("Highest existing", iSelect == Select::SHighestOwn)) iSelect = Select::SHighestOwn;
		for (int i = 5; i < 22; i++) {
			char lbl[32]; snprintf(lbl, sizeof(lbl), "Level %d", i);
			if (ImGui::Selectable(lbl, iSelect == i - 3)) iSelect = i - 3;
		}
		ImGui::EndCombo();
	}

	ImGui::TextUnformatted("What to export");
	ImGui::SetNextItemWidth(-FLT_MIN);
	ImGui::Combo("##what", &iWhat, what, IM_ARRAYSIZE(what));

	if (ImGui::Button("Export selection")) Export();
}


// ===============================================================================================
//
void ToolKit::DrawImportOptions()
{
	ImGuiStyle &style = ImGui::GetStyle();
	ImVec2 bs((ImGui::GetContentRegionAvail().x - style.ItemSpacing.x) * 0.5f, 0.0f);

	ImGui::BeginDisabled(IsBaking());

	if (ImGui::Button("Open surface", bs)) OpenImage(Layer::LayerType::TEXTURE);
	ImGui::SameLine();
	ImGui::BeginDisabled();
	if (ImGui::Button("Open elevation", bs)) oapiAddNotification(OAPINOTIF_INFO, "Terrain ToolKit", "This feature is not yet implemented.");
	ImGui::EndDisabled();

	if (ImGui::Button("Open water", bs)) OpenImage(Layer::LayerType::WATER);
	ImGui::SameLine();
	ImGui::BeginDisabled();
	if (ImGui::Button("Open mesh", bs)) oapiAddNotification(OAPINOTIF_INFO, "Terrain ToolKit", "This feature is not yet implemented.");
	ImGui::EndDisabled();

	if (ImGui::Button("Open nightlights", bs)) OpenImage(Layer::LayerType::NIGHT);
	ImGui::SameLine();
	ImGui::BeginDisabled();
	if (ImGui::Button("Edit elevation", bs)) oapiAddNotification(OAPINOTIF_INFO, "Terrain ToolKit", "This feature is not yet implemented.");
	ImGui::EndDisabled();

	ImGui::Spacing();

	if (ImGui::Button("Start import", bs)) {
		if (selection.selw == 0 || selection.selh == 0) {
			oapiAddNotification(OAPINOTIF_INFO, "Terrain ToolKit", "You need to drag a box around the import area first.");
		}
		else if (CreateOverlays()) {
			AutoSelectCorners();
			if (!UpdateOverlays()) oapiWriteLog((char*)"UpdateOverlays() Failed in IDC_STARTIMPORT");
		}
	}
	ImGui::SameLine();
	if (ImGui::Button("Reset corners", bs)) {
		AutoSelectCorners();
		if (!UpdateOverlays()) oapiWriteLog((char*)"UpdateOverlays() Failed in IDC_CORNERS");
	}

	if (ImGui::Button("Cancel import", bs)) StopImport();
	ImGui::SameLine();
	if (ImGui::Button("Bake", bs)) ImGui::OpenPopup("Bake tiles");

	ImGui::EndDisabled();

	if (ImGui::BeginPopupModal("Bake tiles", NULL, ImGuiWindowFlags_AlwaysAutoResize)) {
		ImGui::TextUnformatted("Bake and write the tiles in the 'OrbiterRoot/TerrainToolKit/' folder?");
		if (ImGui::Button("Bake")) {
			ImGui::CloseCurrentPopup();
			BakeImport();
		}
		ImGui::SameLine();
		if (ImGui::Button("Cancel")) ImGui::CloseCurrentPopup();
		ImGui::EndPopup();
	}
}


// ===============================================================================================
//
void ToolKit::DrawProperties()
{
	if (pProp) pProp->Draw();
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
	hOverlaySrf = NULL;
	hOverlayMsk = NULL;
	hOverlayElv = NULL;
	hOverlay = NULL;
	hGradient = NULL;
	hRootNode = hCtrlNode = hDataNode = hImpoNode = NULL;
	down_corner = -1;
	sel_corner = -1;
	bGo = false;
	bImport = false;
	xpos = ypos = 0;
	selw = selh = 0;
	progress = 0;
	bAutoHighlight = true;
	bDispSel = true;
	iSelect = Select::SHighestOwn;
	iWhat = Select::WTexture;
	memset(pLr, 0, sizeof(pLr));
	selection.selw = selection.selh = selection.slvl = 0;
	selection.pLeftTop = selection.pRightBottom = NULL;
	bake.active = false;
	bake.hTemp = NULL;
	pBakeDlg = new BakeDialog(this);

	// Can do very little here since graphics servises are not yet running 
	dwCmd = oapiRegisterCustomCmd((char*)"TerrainToolKit", (char*)"ToolKit for terrain and base editing", OpenToolsClbk, this);

	mIdent.Ident();
}


// =================================================================================================
//
ToolKit::~ToolKit()
{
	oapiUnregisterCustomCmd(dwCmd);
	delete pBakeDlg;
}


// =================================================================================================
// Orbiter is shuting down... Release resources...
//
void ToolKit::clbkShutdown()
{
	if (!pCore) return;

	pCore->RegisterGenericProc(RenderClbk, GENERICPROC_DELETE, this);
	pCore->RegisterGenericProc(MouseClickClbk, GENERICPROC_DELETE, this);

	if (bake.active) BakeFinish();
	oapiCloseDialog(pBakeDlg);

	if (hOverlay) pCore->AddGlobalOverlay(hMgr, _V(0, 0, 0, 0), gcCore::OlayType::RELEASE_ALL, NULL, hOverlay);
	if (hOverlaySrf) oapiReleaseTexture(hOverlaySrf);
	if (dmSphere) pCore->ReleaseDevMesh(dmSphere);
		
	selection.area.clear();
	oldsel.area.clear();

	if (pRoot) delete pRoot;	// Delete QTree Tile Manager
	for (auto &x : pLr) { delete x; x = NULL; }
	if (pProp) delete pProp;	// Delete Property tree control and sub-controls

	pCore = NULL;
	hMgr = NULL;
	hPlanet = NULL;
	pRoot = pFirst = pSecond = NULL;
	pProp = NULL;
	dmSphere = NULL;
	hOverlaySrf = NULL;
	hOverlayMsk = NULL;
	hOverlayElv = NULL;
	hOverlay = NULL;
	hRootNode = hCtrlNode = hDataNode = hImpoNode = NULL;
	bImport = false;
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
	if (bGo) {
		DisplayWindow(hRootNode);
		return true;
	}

	g_pCore = pCore = gcGetCoreInterface();
	if (!pCore) return false;

	// Must Initialize the base class
	if (gcGUIApp::Initialize() == false) {
		oapiAddNotification(OAPINOTIF_ERROR, "Terrain ToolKit", "gcGUI is disabled, can't launch");
		pCore = NULL;
		return false;
	}

	memset(pLr, 0, sizeof(pLr));

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
		oapiWriteLog((char*)"TerrainToolKit: Failed to load a file [D3D9Sphere.msh]");
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

	// Create GUI Sections --------------------------------------------------------------------------
	//
	hRootNode = RegisterApplication("Terrain ToolKit V1.1", NULL, NULL, gcGUI::DS_LEFT);
	hCtrlNode = RegisterSubsection(hRootNode, "Selection options", DrawSelectionClbk, this);
	hImpoNode = RegisterSubsection(hRootNode, "Import options", DrawImportClbk, this);
	hDataNode = RegisterSubsection(hRootNode, "Properties", DrawPropertiesClbk, this);

	DisplayWindow(hRootNode);

	bAutoHighlight = true;
	bDispSel = true;
	iSelect = Select::SHighestOwn;
	iWhat = Select::WTexture;

	pProp = new gcPropertyTree(IDC_DATAVIEW, [this](HPROP hEntry, WORD id, gcPropertyTree::Event ev) { OnPropertyEvent(hEntry, id, ev); });

	// ---------------------------------------------------
	hSecCur = pProp->SubSection("Mouse cursor location");
	// ---------------------------------------------------
	hCLng = pProp->AddEntry("Longitude", hSecCur);
	hCLat = pProp->AddEntry("Latitude", hSecCur);
	hCEle = pProp->AddEntry("Elevation", hSecCur);
	hCFil = pProp->AddEntry("Filename", hSecCur);

	// ---------------------------------------------------
	hSecExp = pProp->SubSection("Selection and bake");
	// ---------------------------------------------------
	hSLng = pProp->AddEntry("Min lng", hSecExp);
	hSLat = pProp->AddEntry("Min lat", hSecExp);
	hMLng = pProp->AddEntry("Max lng", hSecExp);
	hMLat = pProp->AddEntry("Max lat", hSecExp);
	hSXPx = pProp->AddEntry("Width pixels", hSecExp);
	hSYPx = pProp->AddEntry("Height pixels", hSecExp);
	hCLvl = pProp->AddEntry("Selection level", hSecExp);
	hCEdg = pProp->AddSlider("Edge blend", IDC_ALPHAEDG, hSecExp);
	hBLvs = pProp->AddComboBox("Levels to bake", 0, hSecExp);

	for (int i = 0; i < 8; i++) {
		char buf[8]; snprintf(buf, 8, "%d", i);
		pProp->AddComboBoxItem(hBLvs, buf);
	}
	pProp->SetComboBoxSelection(hBLvs, 0);
	
	pProp->SetSliderScale(hCEdg, 1.0, 480.0, gcPropertyTree::Scale::LOG);
	pProp->SetSliderValue(hCEdg, 32.0);

	bGo = true;
	return true;
}


// =================================================================================================
//
int ToolKit::SelectionFlags()
{
	switch (iWhat) {
	case Select::WTexture: return gcTileFlags::TEXTURE | gcTileFlags::CACHE | gcTileFlags::TREE;
	case Select::WNightlight: return gcTileFlags::MASK | gcTileFlags::CACHE | gcTileFlags::TREE;
	case Select::WElevation: return gcTileFlags::ELEVATION | gcTileFlags::CACHE | gcTileFlags::TREE;
	}
	return 0;
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

	bool bGuides = false;
	bool bHideClip = !bDispSel;
	
	int select = iSelect;
	int flags = SelectionFlags();

	gcCore::PickGround pg; memset(&pg, 0, sizeof(gcCore::PickGround));
	QTree *pHover = NULL;


	// ---------------------------------------------------------------------
	// Optain infomation about mouse cursor location  
	// ---------------------------------------------------------------------

	pg = pCore->ScanScreen(xpos, ypos);
	if (pFirst && !pSecond) pHover = pRoot->FindNode(pg.lng, pg.lat, pFirst->level);
	



	// ---------------------------------------------------------------------
	// Highlight the tile where mouse is hovering and print some information 
	// ---------------------------------------------------------------------

	if (select == Select::SRender) 
	{
		QTree* pSel = pRoot->FindNode(pg.lng, pg.lat, pg.level);
		if (pSel) {
			if (bAutoHighlight || (selection.area.size() == 0)) RenderTileBounds(pSel, 0xFF00FF00);
			UpdateTileInfo(flags, pSel, &pg);
		}
	}
	else 
	{
		if (select == Select::SHighestOwn) 
		{
			QTree *pSel = pRoot->HighestOwn(flags, pg.lng, pg.lat);
			if (pSel) {
				if (bAutoHighlight || (selection.area.size() == 0)) RenderTileBounds(pSel, 0xFF00FF00);
				UpdateTileInfo(flags, pSel, &pg);
			}
		}
		else 
		{
			int lvl = SelectedLevel();
			if (lvl > 0) {
				QTree *pSel = pRoot->FindNode(pg.lng, pg.lat, lvl);
				if (pSel) {
					if (bAutoHighlight || (selection.area.size() == 0)) RenderTileBounds(pSel, 0xFF00FF00);
					UpdateTileInfo(flags, pSel, &pg);
				}
			}
		}
	}
	
	


	

	if (pFirst && (pHover || pSecond))
	{
		// ---------------------------------------------------------------------
		// User has clicked a tile and is dragging a selection box or
		// the selection box is already completed
		// ---------------------------------------------------------------------

		QTree *pOther = (pSecond ? pSecond : pHover);

		int alvl = pFirst->level;
		int alng = pFirst->ilng;
		int alat = pFirst->ilat;
		int blng = pOther->ilng;
		int blat = pOther->ilat;

		selw = abs(alng - blng) + 1;
		selh = abs(alat - blat) + 1;

		int maxts = sp.MaxTexSize / 512;
		if (selw > maxts) selw = maxts;
		if (selh > maxts) selh = maxts;

		double width = pFirst->Width();
		double height = pFirst->Height();

		// Center of Top-Left Tile
		double clng = (pOther->clng < pFirst->clng ? pFirst->clng - width * double(selw - 1) : pFirst->clng);
		double clat = (pOther->clat > pFirst->clat ? pFirst->clat + height * double(selh - 1) : pFirst->clat);


		if (!pSecond)
		{
			// ---------------------------------------------------------------------
			// User is dragging a selection box
			// ---------------------------------------------------------------------

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
			pProp->SetValue(hCLvl, selection.slvl + 4);

			if (bGuides)
			{
				list<QTree*> parents;
				list<QTree*> grands;

				for (auto& x : selection.area) parents.push_back(x.pNode->GetParent());
				parents.unique();

				for (auto x : parents) grands.push_back(x->GetParent());		
				grands.unique();

				RenderSelection(grands, 1, 0xFFFF00FF);
				RenderSelection(parents, 1, 0xFF00FFFF);			
			}
		}
	}



	if (oldsel.area.size() != 0) 
	{
		// ---------------------------------------------------------------------
		// Draw tile outlines for old selection
		// ---------------------------------------------------------------------
		if (!bHideClip) RenderSelection(&oldsel, 1, 0xFF0000FF);
	}

	if (selection.area.size() != 0) 
	{
		// ---------------------------------------------------------------------
		// Draw tile outlines for selection
		// ---------------------------------------------------------------------
		if (!bHideClip) {
			if (bImport) RenderSelection(&selection, 1, 0xFFFFFF00);
			else RenderSelection(&selection, 0, 0xFFFFFF00);
		}
	}

	if (selection.area.size() != 0 && bImport && hOverlay)
	{
		// ---------------------------------------------------------------------
		// Render corner sphere markers
		// ---------------------------------------------------------------------

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
}


// =================================================================================================
// Update the Overlay image seen on a planets surface
//
bool ToolKit::UpdateOverlays()
{
	if (!UpdateOverlay(0)) return false;
	if (!UpdateOverlay(1)) return false;
	return true;
}


// =================================================================================================
// Update the Overlay image seen on a planets surface
//
bool ToolKit::UpdateOverlay(int olay)
{
	if (!bImport) return false;

	SURFHANDLE hSrf = NULL;
	Layer* pLr = NULL;
	
	if (olay == 0) {
		hSrf = hOverlaySrf;
		pLr = GetLayer(Layer::LayerType::TEXTURE);
	}

	if (olay == 1) {
		hSrf = hOverlayMsk;
		pLr = GetLayer(Layer::LayerType::NIGHT);
	}
	
	if (!hSrf) return false;

	bool bWater = IsLayerValid(Layer::LayerType::WATER);
	bool bNight = IsLayerValid(Layer::LayerType::NIGHT);
	bool bSurf = IsLayerValid(Layer::LayerType::TEXTURE);


	// Clear/Initialize the background image
	//
	if (olay == 0) {
		UpdateBackGround(hSrf, gcTileFlags::TEXTURE | gcTileFlags::CACHE | gcTileFlags::TREE);
	}

	if (olay == 1) {
		if (bNight || bWater) UpdateBackGround(hSrf, gcTileFlags::MASK | gcTileFlags::CACHE | gcTileFlags::TREE);
	}


	// Acquire Sketchpad for Overlay Surface
	Sketchpad *pSkp = oapiGetSketchpad(hSrf);

	if (pSkp) 
	{
		FVECTOR4 clr = 1.0f;
		FVECTOR4 adj = FVECTOR4(0, 1, 0, 0);

		if (pLr) {
			clr = pLr->GetColor();
			adj = pLr->GetAdjustments();
		}

		FMATRIX4 mColor;

		mColor.Ident();
		mColor.m11 = clr.r;
		mColor.m22 = clr.g;
		mColor.m33 = clr.b;
		mColor.m41 = adj.x;
		mColor.m42 = adj.x;
		mColor.m43 = adj.x;

		if (pLr) if (pLr->GetTransparency()) mColor.m44 = 0.5f;

		pSkp->ColorCompatibility(false);

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
		
		for (int i = 0; i < 4; i++) {
			int q = (i - 1) < 0 ? 3 : i - 1;
			int w = (i + 1) > 3 ? 0 : i + 1;
			FVECTOR2 a = unit((pt[q] - pt[i]) + (pt[w] - pt[i]));
			in[i] = pt[i] + a * float(pProp->GetSliderValue(hCEdg));
		}



		// Create Surface Overlay
		//
		if (olay == 0) 
		{
			if (bSurf && pLr) {
				pSkp->SetColorMatrix(&mColor);	// Setup a color matrix for corrections
				pSkp->SetRenderParam(Sketchpad::RenderParam::PRM_GAMMA, ptr(FVECTOR4(adj.y, adj.y, adj.y, 1.0f))); // Setup gamma correction
				pSkp->SetBlendState(Sketchpad::BlendState::ALPHABLEND);
				pSkp->CopyTetragon(pLr->hSource, NULL, pt);
			}
			hOverlay = pCore->AddGlobalOverlay(hMgr, selection.bounds.vec, gcCore::OlayType::SURFACE, hOverlaySrf, hOverlay);
		}
		

		// Create Nightlights/Water Overlay
		//
		if (olay == 1)
		{
			if (bNight && pLr) 
			{
				pSkp->SetColorMatrix(&mColor);	// Setup a color matrix for corrections
				pSkp->SetRenderParam(Sketchpad::RenderParam::PRM_GAMMA, ptr(FVECTOR4(adj.y, adj.y, adj.y, 1.0f))); // Setup gamma correction

				int mode = pLr->GetWaterMode();

				if (mode == 0) // No Water in Alpha 
				{
					pSkp->SetBlendState(Sketchpad::BlendState::COPY_COLOR);
					pSkp->CopyTetragon(pLr->hSource, NULL, pt);
					
					if (bWater) // Use additional water layer if available
					{
						Layer* pWaterLr = GetLayer(Layer::LayerType::WATER);

						FMATRIX4 mMix; mMix.Zero(); mMix.m24 = 1.0f;
						pSkp->SetColorMatrix(&mMix); // Mix green channel to alpha
						pSkp->SetRenderParam(Sketchpad::RenderParam::PRM_GAMMA);
						pSkp->SetBlendState(Sketchpad::BlendState::COPY_ALPHA);
						pSkp->CopyTetragon(pWaterLr->hSource, NULL, pt); // Copy to destination alpha	
					}
				}

				if (mode == 1) { // Water mask in alpha channel
					pSkp->SetBlendState(Sketchpad::BlendState::COPY);
					pSkp->CopyTetragon(pLr->hSource, NULL, pt);
				}

				if (mode == 2) { // All Land, no water information
					pSkp->SetBlendState(Sketchpad::BlendState::COPY_COLOR);
					pSkp->CopyTetragon(pLr->hSource, NULL, pt);
					// ---------------
					pSkp->SetRenderParam(Sketchpad::RenderParam::PRM_GAMMA);
					pSkp->SetColorMatrix();
					pSkp->SetBlendState(Sketchpad::BlendState::COPY_ALPHA);
					pSkp->FillTetragon(0xFF000000, pt);
				}
			}
			else 
			{		
				// No Nightlights, just a water layer
				pSkp->SetBlendState(Sketchpad::BlendState::COPY_COLOR);
				pSkp->FillTetragon(0, pt);

				if (bWater) // Use additional water layer if available
				{
					Layer* pWaterLr = GetLayer(Layer::LayerType::WATER);

					FMATRIX4 mMix; mMix.Zero(); mMix.m24 = 1.0f;
					pSkp->SetColorMatrix(&mMix); // Mix green channel to alpha
					pSkp->SetBlendState(Sketchpad::BlendState::COPY_ALPHA);
					pSkp->CopyTetragon(pWaterLr->hSource, NULL, pt); // Copy to destination alpha	
				}
			}

			FVECTOR4 Blend = 1.0f;
			hOverlay = pCore->AddGlobalOverlay(hMgr, selection.bounds.vec, gcCore::OlayType::MASK, hOverlayMsk, hOverlay, &Blend);
		}


		// Restore default Matrix and Gamma
		pSkp->SetColorMatrix();
		pSkp->SetRenderParam(Sketchpad::RenderParam::PRM_GAMMA);

		if (olay == 0)
		{
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
		}

		pSkp->SetBlendState();
		oapiReleaseSketchpad(pSkp);

		return true;
	}

	assert(false);
	return false;
}



// =================================================================================================
//
bool ToolKit::CreateOverlays()
{
	
	int Width = selection.selw * 512;
	int Height = selection.selh * 512;

	if (Width == 0 || Height == 0) return false;

	bool bWater = IsLayerValid(Layer::LayerType::WATER);
	bool bNight = IsLayerValid(Layer::LayerType::NIGHT);
	bool bSurf = IsLayerValid(Layer::LayerType::TEXTURE);

	if (hOverlaySrf) 
	{
		gcCore::SurfaceSpecs ovlspecs;
		if (pCore->GetSurfaceSpecs(hOverlaySrf, &ovlspecs, sizeof(gcCore::SurfaceSpecs))) {
			if (ovlspecs.Width != Width || ovlspecs.Height != Height) {
				oapiReleaseTexture(hOverlaySrf);
				hOverlaySrf = NULL;
			}
		}
	}

	if (hOverlayMsk)
	{
		gcCore::SurfaceSpecs ovlspecs;
		if (pCore->GetSurfaceSpecs(hOverlayMsk, &ovlspecs, sizeof(gcCore::SurfaceSpecs))) {
			if (ovlspecs.Width != Width || ovlspecs.Height != Height) {
				oapiReleaseTexture(hOverlayMsk);
				hOverlayMsk = NULL;
			}
		}
	}

	if (hOverlayElv)
	{
		gcCore::SurfaceSpecs ovlspecs;
		if (pCore->GetSurfaceSpecs(hOverlayElv, &ovlspecs, sizeof(gcCore::SurfaceSpecs))) {
			if (ovlspecs.Width != Width || ovlspecs.Height != Height) {
				oapiReleaseTexture(hOverlayElv);
				hOverlayElv = NULL;
			}
		}
	}

	if (!hOverlaySrf) // Needed for nightlights due to alpha blending control channel
	{
		hOverlaySrf = oapiCreateSurfaceEx(Width, Height, OAPISURFACE_PF_ARGB | OAPISURFACE_RENDERTARGET | OAPISURFACE_TEXTURE | OAPISURFACE_MIPMAPS);
		if (!hOverlaySrf) return false;
	}

	if (!hOverlayMsk && (bNight || bWater))
	{
		hOverlayMsk = oapiCreateSurfaceEx(Width, Height, OAPISURFACE_PF_ARGB | OAPISURFACE_RENDERTARGET | OAPISURFACE_TEXTURE | OAPISURFACE_MIPMAPS);
		if (!hOverlayMsk) return false;
	}

	if (!hOverlayElv && IsLayerValid(Layer::LayerType::ELEVATION))
	{
		hOverlayElv = oapiCreateSurfaceEx(Width, Height, OAPISURFACE_PF_F32R | OAPISURFACE_RENDERTARGET | OAPISURFACE_TEXTURE | OAPISURFACE_NOMIPMAPS);
		if (!hOverlayElv) return false;
		hOverlay = pCore->AddGlobalOverlay(hMgr, selection.bounds.vec, gcCore::OlayType::ELEVATION, hOverlayElv, hOverlay);
	}

	bImport = true;

	return bImport;
}


// =================================================================================================
//
bool ToolKit::UpdateBackGround(SURFHANDLE hSurf, DWORD flags)
{
	if (!hSurf) return false;

	Sketchpad* pSkp = oapiGetSketchpad(hSurf);

	if (pSkp) 
	{
		pSkp->SetColorMatrix();
		pSkp->SetBlendState(Sketchpad::BlendState::COPY);

		for (auto se : selection.area)
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
	return true;
}




// =================================================================================================
// Called from D3D9Client when user clicks the ground
//
void ToolKit::clbkMouseClick(int iUser, void *pData)
{
	if (iUser != sizeof(gcCore::PickGround)) return;
	if (!pRoot) return;
	if (!pCore) return;

	int select = iSelect;
	int what = iWhat;

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
		down_corner = -1; // down_corner is the ID of the draggable corner balls
		if (bImport && hOverlay) {
			for (int i = 0; i < 4; i++) {
				if (pCore->PickMesh(&pick, dmSphere, &points[i].mWorld, short(x), short(y))) {
					sel_corner = i;
					down_corner = i;
				}
			}
		}
	}

	if (event == WM_LBUTTONUP) down_corner = -1;


	if ((x != xpos || y != ypos) && down_corner >= 0)
	{
		gcCore::PickGround pg = pCore->ScanScreen(x, y);
		points[down_corner].lng = pg.lng;
		points[down_corner].lat = pg.lat;
		points[down_corner].elev = pg.elev;
		if (!UpdateOverlays()) oapiWriteLog((char*)"UpdateOverlays() Failed in clbkProcessMouse()");
		else for (auto x : pLr) if (x) x->ComputeLevel(points);
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

