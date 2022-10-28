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
#include "ToolBox.h"
#include "resource.h"
#include "gcPropertyTree.h"
#include "QTree.h"
#include <Commctrl.h>
#include <vector>
#include <list>

using namespace std;

extern ToolKit *g_pTK;


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
//
bool ToolKit::IsLayerValid(Layer::LayerType type)
{ 
	Layer* pLr = GetLayer(type);
	if (pLr && pLr->hSource) return true;
	return false;
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
//
DRECT ToolKit::GetBounds(list<QTree*> sel)
{
	DRECT dr = DRECT(10.0, -10.0, -10.0, 10.0);

	for (auto x : sel) {
		dr.left = min(dr.left, x->Bounds.left);
		dr.right = max(dr.right, x->Bounds.right);
		dr.top = max(dr.top, x->Bounds.top);
		dr.bottom = min(dr.bottom, x->Bounds.bottom);
	}
	return dr;
}


// =================================================================================================
//
void ToolKit::RenderSelection(list<QTree*> sel, int mode, DWORD color)
{
	if (mode == 0) {
		for (auto x : sel) RenderTileBounds(x, color);
		return;
	}

	float emin = 1e6;
	float emax = -1e6;

	if (mode == 1) 
	{
		for (auto x : sel) 
		{
			gcCore::PickGround pg = pCore->GetTileData(hMgr, x->clng, x->clat, x->level);
			emin = min(emin, pg.emin);
			emax = max(emax, pg.emax);
		}

		DRECT Bounds = GetBounds(sel);
	
		double size = oapiGetSize(hPlanet);
		VECTOR3 cpos, bpos;

		oapiCameraGlobalPos(&cpos);
		oapiGetGlobalPos(hPlanet, &bpos);
		cpos -= bpos;

		VECTOR3 V[4];
		V[0] = GetSurfacePosUnit(Bounds.left, Bounds.top);
		V[1] = GetSurfacePosUnit(Bounds.left, Bounds.bottom);
		V[2] = GetSurfacePosUnit(Bounds.right, Bounds.bottom);
		V[3] = GetSurfacePosUnit(Bounds.right, Bounds.top);

		double h = fabs(Bounds.top - Bounds.bottom);
		double w = fabs(Bounds.right - Bounds.left);
		double s = size - cos(max(w, h) * 0.5) * size;

		FVECTOR3 box[8];
		for (int i = 0; i < 4; i++) box[i] = FVECTOR3(V[i] * (size + emax + s) - cpos);
		for (int i = 0; i < 4; i++) box[i + 4] = FVECTOR3(V[i] * (size + emin) - cpos);

		DrawBox(box, color);
	}
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

	return hSrf;
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

	for (auto x : pLr) if (x) x->ComputeLevel(points);
}


// =================================================================================================
//
void ToolKit::UpdateTileInfo(int flags, QTree* pF, gcCore::PickGround* pP)
{
	if (!pP) return;
	char name[64]; strcpy_s(name, 64, "No Data Available");

	pProp->SetValue(hCLng, pP->lng, 4, gcPropertyTree::LONGITUDE);
	pProp->SetValue(hCLat, pP->lat, 4, gcPropertyTree::LATITUDE);
	pProp->SetValue(hCEle, pP->elev, 1);
	if (pF) {
		if (pF->HasOwnTex(flags))
		{
			if (flags & gcTileFlags::TEXTURE) sprintf_s(name, 63, "Surf/%d/%d/%d.dds", pF->level + 4, pF->ilat, pF->ilng);
			if (flags & gcTileFlags::MASK) sprintf_s(name, 63, "Mask/%d/%d/%d.dds", pF->level + 4, pF->ilat, pF->ilng);
			if (flags & gcTileFlags::ELEVATION) sprintf_s(name, 63, "Elev/%d/%d/%d.dds", pF->level + 4, pF->ilat, pF->ilng);
		}
	}
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
