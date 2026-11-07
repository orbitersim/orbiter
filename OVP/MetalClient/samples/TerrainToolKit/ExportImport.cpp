// ==================================================================
// Copyright (c) 2021-2026 Jarmo Nikkanen
// Licensed under the MIT License
// ==================================================================

#include "OrbiterAPI.h"
#include "VesselAPI.h"
#include "ModuleAPI.h"
#include "DrawAPI.h"
#include "gcCoreAPI.h"
#include "ToolKit.h"
#include "gcPropertyTree.h"
#include "QTree.h"
#include "OrbiterPlatform.h"
#include "imgui.h"
#include <vector>
#include <list>
#include <chrono>
#include <filesystem>
#include <algorithm>
#include <cctype>

using namespace std;

extern ToolKit *g_pTK;

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
	int what = iWhat;
	int flags = gcTileFlags::TREE | gcTileFlags::CACHE;
	int srff = 0;

	if (what == 0) flags |= gcTileFlags::TEXTURE;
	if (what == 1) flags |= gcTileFlags::MASK, srff |= OAPISURFACE_PF_ARGB;
	if (what == 2) {
		ExportElev();
		return;
	}

	if (selection.area.size() == 0 || selw == 0 || selh == 0) {
		oapiAddNotification(OAPINOTIF_INFO, "Terrain ToolKit", "You need to drag a box around the export area first.");
		return;
	}
	
	SURFHANDLE hSrf = oapiCreateSurfaceEx(selw * 512, selh * 512, OAPISURFACE_RENDERTARGET | srff);

	if (hSrf) {

		Sketchpad *pSkp = oapiGetSketchpad(hSrf);

		if (pSkp) {

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

			pSkp->SetBlendState();
			oapiReleaseSketchpad(pSkp);
		}

		string path;
		if (SaveFile(path)) {
			if (!pCore->SaveSurface(path.c_str(), hSrf)) {
				oapiWriteLogV("Failed to create a file [%s]", path.c_str());
				oapiAddNotification(OAPINOTIF_ERROR, "Terrain ToolKit", ("Failed to create a file " + path).c_str());
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
			oapiAddNotification(OAPINOTIF_ERROR, "Terrain ToolKit", msg);
			return;
		}
	}

	string path = oplat::SaveFileDialog("Export elevation", "", "", { "dds" });
	if (path.empty()) return;

	string ext = std::filesystem::path(path).extension().string();
	std::transform(ext.begin(), ext.end(), ext.begin(), [](unsigned char c) { return (char)std::tolower(c); });
	if (ext.empty()) path += ".dds";
	else if (ext != ".dds") {
		oapiAddNotification(OAPINOTIF_ERROR, "Terrain ToolKit", "Invalid file type");
		return;
	}

	int fmt = 0;

	SURFHANDLE hSrf = GetBaseElevation(fmt);

	if (hSrf) {
		if (!pCore->SaveSurface(path.c_str(), hSrf)) {
			oapiAddNotification(OAPINOTIF_ERROR, "Terrain ToolKit", "Failed to save a file");
		}
		oapiReleaseTexture(hSrf);
	}
}


// =================================================================================================
//
void ToolKit::BakeImport()
{
	if (bake.active) return;

	bool bWater = IsLayerValid(Layer::LayerType::WATER);
	bool bNight = IsLayerValid(Layer::LayerType::NIGHT);
	bool bSurf = IsLayerValid(Layer::LayerType::TEXTURE);

	bake.phases.clear();

	if (bSurf) {
		bake.phases.push_back({ gcTileFlags::TEXTURE | gcTileFlags::CACHE | gcTileFlags::TREE, hOverlaySrf,
			OAPISURFACE_PF_XRGB | OAPISURFACE_RENDERTARGET | OAPISURFACE_TEXTURE, "Baking surface" });
	}

	if (bNight || bWater) {
		bake.phases.push_back({ gcTileFlags::MASK | gcTileFlags::CACHE | gcTileFlags::TREE, hOverlayMsk,
			OAPISURFACE_PF_ARGB | OAPISURFACE_RENDERTARGET | OAPISURFACE_TEXTURE, "Baking nightlights" });
	}

	if (bake.phases.empty()) return;

	int nTiles = int(selection.area.size());
	bake.nTiles = nTiles + (nTiles / 2 + nTiles / 4 + nTiles / 8);
	bake.active = true;
	bake.phase = 0;
	bake.hTemp = NULL;

	BakeNextPhase();
	if (bake.active) oapiOpenDialog(pBakeDlg);
}


// =================================================================================================
//
void ToolKit::BakeNextPhase()
{
	if (bake.hTemp) oapiReleaseTexture(bake.hTemp);
	bake.hTemp = NULL;

	if (bake.phase >= bake.phases.size()) {
		BakeFinish();
		return;
	}

	bake.hTemp = oapiCreateSurfaceEx(512, 512, bake.phases[bake.phase].tempfmt);
	bake.levels = pProp->GetComboBoxSelection(hBLvs) + 1;
	bake.top = true;
	bake.index = 0;
	bake.work.clear();
	bake.next.clear();
	progress = 0;
}


// =================================================================================================
//
void ToolKit::BakeFinish()
{
	if (bake.hTemp) oapiReleaseTexture(bake.hTemp);
	bake.hTemp = NULL;
	bake.work.clear();
	bake.next.clear();
	bake.active = false;
}


// =================================================================================================
// Bake one tile. Returns false when baking has ended.
//
bool ToolKit::BakeStep()
{
	if (!bake.active) return false;

	const BakePhase &ph = bake.phases[bake.phase];

	if (bake.top)
	{
		if (bake.index < selection.area.size())
		{
			selentry &s = selection.area[bake.index++];
			if (s.pNode->SaveTile(ph.flags, ph.hOvrl, bake.hTemp, selection.bounds, selection.slvl, 1.0f) < 0)
			{
				oapiWriteLogV("ERROR: s.pNode->SaveTile() failed");
				oapiAddNotification(OAPINOTIF_ERROR, "Terrain ToolKit", "Failed to bake a tile");
				BakeFinish();
				return false;
			}
			MakeProgress();
			bake.next.push_back(s.pNode->GetParent());
			return true;
		}

		bake.top = false;
		bake.next.unique();
		bake.work = bake.next;
		bake.next.clear();
		bake.levels--;
		bake.it = bake.work.begin();
		return true;
	}

	while (bake.it != bake.work.end())
	{
		QTree *qt = *bake.it++;
		if (!qt) continue;

		if ((qt->HasOwnTex(ph.flags) == false) || (bake.levels > 0))
		{
			MakeProgress();
			bake.next.push_back(qt->GetParent());

			if (qt->SaveTile(ph.flags, ph.hOvrl, bake.hTemp, selection.bounds, -1, 1.0f) < 0)
			{
				oapiWriteLogV("ERROR: qt->SaveTile() failed");
				oapiAddNotification(OAPINOTIF_ERROR, "Terrain ToolKit", "Failed to bake a tile");
				BakeFinish();
				return false;
			}
			return true;
		}
	}

	bake.next.unique();

	if (bake.next.size())
	{
		bake.work = bake.next;
		bake.next.clear();
		bake.levels--;
		bake.it = bake.work.begin();
		return true;
	}

	bake.phase++;
	BakeNextPhase();
	return bake.active;
}


// =================================================================================================
//
void ToolKit::DrawBakeProgress()
{
	auto t0 = std::chrono::steady_clock::now();
	while (bake.active && std::chrono::steady_clock::now() - t0 < std::chrono::milliseconds(30)) {
		if (!BakeStep()) break;
	}

	if (!bake.active) return;

	char overlay[32];
	snprintf(overlay, sizeof(overlay), "%d / %d", progress, bake.nTiles);
	ImGui::TextUnformatted(bake.phases[bake.phase].title.c_str());
	ImGui::ProgressBar(bake.nTiles > 0 ? float(progress) / float(bake.nTiles) : 0.0f, ImVec2(ImGui::GetFontSize() * 20.0f, 0.0f), overlay);
}


// =================================================================================================
//
void ToolKit::OpenImage(Layer::LayerType lr)
{
	string path = oplat::OpenFileDialog("Open image", "", { "dds", "png", "jpg", "jpeg", "bmp" });
	if (path.empty()) return;

	string title = std::filesystem::path(path).filename().string();

	auto Lr = pLr[(int)lr];

	if (Lr) {
		if (Lr->hSource) {
			oapiReleaseTexture(Lr->hSource);
			Lr->hSource = NULL;
		}
		delete Lr;
		pLr[(int)lr] = NULL;
	}

	if (path.find(".raw") == string::npos)
	{
		SURFHANDLE hSrf = oapiLoadSurfaceEx(path.c_str(), OAPISURFACE_TEXTURE, true);

		if (!hSrf) {
			oapiAddNotification(OAPINOTIF_ERROR, "Terrain ToolKit", ("Unable to load file " + path).c_str());
			return;
		}

		pLr[(int)lr] = new Layer(pProp, pCore, hSrf, lr, title);
	}
	else
	{
		pLr[(int)lr] = new Layer(pProp, pCore, NULL, lr, title);
	}
}
