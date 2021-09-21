// ==================================================================
// Copyright (c) 2021 Jarmo Nikkanen
// Licensed under the MIT License
// ==================================================================

#include <Windows.h>
#include <windowsx.h>
#include "OrbiterAPI.h"
#include "VesselAPI.h"
#include "ModuleAPI.h"
#include "DrawAPI.h"
#include "gcCoreAPI.h"
#include "ToolBox.h"
#include "resource.h"
#include "gcPropertyTree.h"
#include "QTree.h"
#include <Commctrl.h>
#include <vector>
#include <list>

using namespace std;

extern ToolKit *g_pTK;

BOOL CALLBACK gDlgProc(HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam);

// =================================================================================================
// Abort Import Process, Release resources...
//
void ToolKit::StopImport()
{
	for (auto x : pLr) {
		if (x->hSource) oapiReleaseTexture(x->hSource);
		x->hSource = NULL;
	}

	if (hOverlay) pCore->AddGlobalOverlay(hMgr, _V(0, 0, 0, 0), gcCore::OlayType::RELEASE_ALL, NULL, hOverlay);
	if (hOverlaySrf) oapiReleaseTexture(hOverlaySrf);
	hOverlaySrf = NULL;
	hOverlay = NULL;
	selection.area.clear();
	oldsel.area.clear();
	pFirst = pSecond = NULL;
}


// =================================================================================================
//
void ToolKit::Export()
{
	int what = SendDlgItemMessage(hCtrlDlg, IDC_WHAT, CB_GETCURSEL, 0, 0);
	int flags = gcTileFlags::TREE | gcTileFlags::CACHE;
	int srff = 0;

	if (what == 0) flags |= gcTileFlags::TEXTURE;
	if (what == 1) flags |= gcTileFlags::MASK, srff |= OAPISURFACE_PF_ARGB;
	if (what == 2) {
		ExportElev();
		return;
	}
	
	SURFHANDLE hSrf = oapiCreateSurfaceEx(selw * 512, selh * 512, OAPISURFACE_RENDERTARGET | srff);

	if (hSrf) {

		Sketchpad *pSkp = oapiGetSketchpad(hSrf);
		pSkp->SetBlendState(Sketchpad::BlendState::COPY);

		if (pSkp) {

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

			pSkp->SetBlendState();
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


	if (GetSaveFileNameA(&SaveElevation)) 
	{
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
void ToolKit::BakeImport()
{

	if (MessageBox(pCore->GetRenderWindow(), "Bake and Write the tiles in 'OrbiterRoot/TerrainToolBox/' Folder ?", "Are you sure", MB_YESNO | MB_ICONEXCLAMATION) != IDYES) return;

	bool bWater = IsLayerValid(Layer::LayerType::WATER);
	bool bNight = IsLayerValid(Layer::LayerType::NIGHT);
	bool bSurf = IsLayerValid(Layer::LayerType::TEXTURE);


	progress = 0;

	int nTiles = selection.area.size();
	nTiles += (nTiles / 2 + nTiles / 4 + nTiles / 8);
	
	hProgDlg = CreateDialogParamA(hModule, MAKEINTRESOURCE(IDD_PROGRESS), hAppMainWnd, (DLGPROC)gDlgProc, 0);
	SendDlgItemMessage(hProgDlg, IDC_PROGBAR, PBM_SETRANGE, 0, MAKELONG(0, nTiles));
	SendDlgItemMessage(hProgDlg, IDC_PROGBAR, PBM_SETPOS, 0, 0);

	ShowWindow(hProgDlg, SW_SHOW);

	// ------------------------------------------------------------------
	//
	if (bSurf) 
	{
		SURFHANDLE hTemp = oapiCreateSurfaceEx(512, 512, OAPISURFACE_PF_XRGB | OAPISURFACE_RENDERTARGET | OAPISURFACE_TEXTURE);

		int levels = pProp->GetComboBoxSelection(hBLvs) + 1;
		int flags = gcTileFlags::TEXTURE | gcTileFlags::CACHE | gcTileFlags::TREE;

		SetWindowText(hProgDlg, "Baking Surface:");
		list<QTree*> parents;

		for (auto s : selection.area)
		{
			if (s.pNode->SaveTile(flags, hOverlaySrf, hTemp, selection.bounds, selection.slvl, 1.0f) < 0) 
			{
				oapiWriteLogV("ERROR: s.pNode->SaveTile() failed");
				return;
			}
			MakeProgress();
			parents.push_back(s.pNode->GetParent());
		}

		parents.unique();
		BakeParents(hOverlaySrf, hTemp, flags, parents, levels);

		oapiReleaseTexture(hTemp);
	}


	// ------------------------------------------------------------------
	//
	if (bNight || bWater)
	{
		SURFHANDLE hTemp = oapiCreateSurfaceEx(512, 512, OAPISURFACE_PF_ARGB | OAPISURFACE_RENDERTARGET | OAPISURFACE_TEXTURE);

		int levels = pProp->GetComboBoxSelection(hBLvs) + 1;
		int flags = gcTileFlags::MASK | gcTileFlags::CACHE | gcTileFlags::TREE;

		progress = 0;
		SetWindowText(hProgDlg, "Baking Nightlights:");
		SendDlgItemMessage(hProgDlg, IDC_PROGBAR, PBM_SETRANGE, 0, MAKELONG(0, nTiles));
		SendDlgItemMessage(hProgDlg, IDC_PROGBAR, PBM_SETPOS, 0, 0);

		list<QTree*> parents;

		for (auto s : selection.area)
		{
			if (s.pNode->SaveTile(flags, hOverlayMsk, hTemp, selection.bounds, selection.slvl, 1.0f) < 0) 
			{
				oapiWriteLogV("ERROR: s.pNode->SaveTile() failed");
				return;
			}
			MakeProgress();
			parents.push_back(s.pNode->GetParent());
		}

		parents.unique();
		BakeParents(hOverlayMsk, hTemp, flags, parents, levels);

		oapiReleaseTexture(hTemp);
	}

	DestroyWindow(hProgDlg);
}



// =================================================================================================
//
void ToolKit::BakeParents(SURFHANDLE hOvrl, SURFHANDLE hTemp, int flags, list<QTree *> parents, int levels)
{
	levels--;
	list<QTree *> grands;

	for (auto qt : parents)
	{
		float alpha = 1.0f;

		if ((qt->HasOwnTex(flags) == false) || (levels > 0))
		{
			MakeProgress();
			grands.push_back(qt->GetParent());

			if (qt->SaveTile(flags, hOvrl, hTemp, selection.bounds, -1, alpha) < 0) 
			{
				oapiWriteLogV("ERROR: qt->SaveTile() failed");
				return;
			}
		}
	}

	grands.unique();
	if (grands.size()) BakeParents(hOvrl, hTemp, flags, grands, levels);
}



// =================================================================================================
//
void ToolKit::OpenImage(Layer::LayerType lr)
{
	char buf[MAX_PATH + 32];

	auto Lr = pLr[(int)lr];

	if (GetOpenFileNameA(&SaveImage)) 
	{
		if (Lr) {
			if (Lr->hSource) {
				oapiReleaseTexture(Lr->hSource);
				Lr->hSource = NULL;
			}
		}

		if (string(SaveImage.lpstrFile).find(".raw") == string::npos)
		{

			SURFHANDLE hSrf = oapiLoadSurfaceEx(SaveImage.lpstrFile, OAPISURFACE_TEXTURE, true);

			if (!hSrf) {
				sprintf_s(buf, 32 + MAX_PATH, "Unable to load file [%s]", SaveImage.lpstrFile);
				MessageBoxA(hAppMainWnd, buf, "Error:", MB_OK);
				return;
			}

			pLr[(int)lr] = new Layer(pProp, pCore, hSrf, lr, string(SaveImage.lpstrFileTitle));			
		}
		else 
		{
			pLr[(int)lr] = new Layer(pProp, pCore, NULL, lr, string(SaveImage.lpstrFileTitle));
		}
	}
}
