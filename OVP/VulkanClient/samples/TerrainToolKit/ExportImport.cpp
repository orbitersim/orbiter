// ==================================================================
// Copyright (c) 2021-2026 Jarmo Nikkanen
// Licensed under the MIT License
// ==================================================================

// Windows.h, windowsx.h left out: the Win32 types come from OrbiterPlatform.h
#include "OrbiterAPI.h"
#include "VesselAPI.h"
#include "ModuleAPI.h"
#include "DrawAPI.h"
#include "gcCoreAPI.h"
#include "ToolKit.h"
#include "resource.h"
#include "gcPropertyTree.h"
#include "QTree.h"
#include "OrbiterResource.h"
// Commctrl.h left out: the common controls are Qt widgets
#include <QComboBox>
#include <QMessageBox>
#include <QProgressBar>
#include <cstring>
#include <vector>
#include <list>

using namespace std;

extern ToolKit *g_pTK;

void gDlgProc(QWidget *hDlg, void *context);

// =================================================================================================
// Abort Import Process, Release resources...
//
void ToolKit::StopImport()
{
	for (auto x : pLr) {
		if (x) {
			if (x->hSource) oapiReleaseTexture(x->hSource);
			x->hSource = NULL;
		}
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
	int what = DlgItem<QComboBox>(hCtrlDlg, IDC_WHAT)->currentIndex(); // CB_GETCURSEL
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
	else oapiWriteLog((char*)"hSrf == NULL");
}


// =================================================================================================
//
void ToolKit::ExportElev()
{

	for (selentry se : selection.area)
	{
		INT16 *pElev = se.pNode->GetElevation();
		if (!pElev) {
			char msg[256];
			snprintf(msg, 256, "Tile (iLng=%d, iLat=%d) has no elevation for level %d", se.pNode->ilng, se.pNode->ilat, selection.slvl);
			QMessageBox box(QMessageBox::NoIcon, "Error:", msg, QMessageBox::Ok);
			oapiExecOwned(&box, pCore->GetRenderWindow()); // MessageBoxA (render window, MB_OK)
			return;
		}
	}


	if (FileDlgSave(SaveElevation)) 
	{
		int type = 0;

		// Pick the file type from a file name
		if (strstr(SaveImage.lpstrFile, ".dds") || strstr(SaveImage.lpstrFile, ".DDS")) type = 1;

		if (type == 0) {
			// If above fails then use selected "filter" to appeand file "id".
			if (SaveImage.nFilterIndex == 0) strncat(SaveImage.lpstrFile, ".dds", MAX_PATH - strlen(SaveImage.lpstrFile) - 1), type = 1;
		}

		if (type == 0) {
			QMessageBox box(QMessageBox::NoIcon, "Error:", "Invalid File Type", QMessageBox::Ok);
			oapiExecOwned(&box, pCore->GetRenderWindow()); // MessageBoxA (render window, MB_OK)
			return;
		}

		int fmt = 0;

		SURFHANDLE hSrf = GetBaseElevation(fmt);

		if (hSrf) {
			if (!pCore->SaveSurface(SaveImage.lpstrFile, hSrf)) {
				QMessageBox box(QMessageBox::NoIcon, "Error:", "Failed to Save a file", QMessageBox::Ok);
				oapiExecOwned(&box, pCore->GetRenderWindow()); // MessageBoxA (render window, MB_OK)
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

	QMessageBox box(QMessageBox::Warning, "Are you sure", "Bake and Write the tiles in 'OrbiterRoot/TerrainToolKit/' Folder ?", QMessageBox::Yes | QMessageBox::No);
	if (oapiExecOwned(&box, pCore->GetRenderWindow()) != QMessageBox::Yes) return; // MessageBox (render window, MB_YESNO | MB_ICONEXCLAMATION)

	bool bWater = IsLayerValid(Layer::LayerType::WATER);
	bool bNight = IsLayerValid(Layer::LayerType::NIGHT);
	bool bSurf = IsLayerValid(Layer::LayerType::TEXTURE);


	progress = 0;

	int nTiles = selection.area.size();
	nTiles += (nTiles / 2 + nTiles / 4 + nTiles / 8);
	
	hProgDlg = oapiCreateResDialog(hModule, IDD_PROGRESS, NULL, hAppMainWnd); gDlgProc(hProgDlg, 0); // CreateDialogParamA
	DlgItem<QProgressBar>(hProgDlg, IDC_PROGBAR)->setRange(0, nTiles); // PBM_SETRANGE
	DlgItem<QProgressBar>(hProgDlg, IDC_PROGBAR)->setValue(0); // PBM_SETPOS

	hProgDlg->show(); // ShowWindow

	// ------------------------------------------------------------------
	//
	if (bSurf) 
	{
		SURFHANDLE hTemp = oapiCreateSurfaceEx(512, 512, OAPISURFACE_PF_XRGB | OAPISURFACE_RENDERTARGET | OAPISURFACE_TEXTURE);

		int levels = pProp->GetComboBoxSelection(hBLvs) + 1;
		int flags = gcTileFlags::TEXTURE | gcTileFlags::CACHE | gcTileFlags::TREE;

		oapiSetDlgText(hProgDlg, "Baking Surface:"); // SetWindowText
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
		oapiSetDlgText(hProgDlg, "Baking Nightlights:");
		DlgItem<QProgressBar>(hProgDlg, IDC_PROGBAR)->setRange(0, nTiles);
		DlgItem<QProgressBar>(hProgDlg, IDC_PROGBAR)->setValue(0);

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

	delete hProgDlg; // DestroyWindow
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

	if (FileDlgOpen(SaveImage)) 
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
				snprintf(buf, 32 + MAX_PATH, "Unable to load file [%s]", SaveImage.lpstrFile);
				QMessageBox box(QMessageBox::NoIcon, "Error:", buf, QMessageBox::Ok);
				oapiExecOwned(&box, hAppMainWnd); // MessageBoxA (render window, MB_OK)
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

