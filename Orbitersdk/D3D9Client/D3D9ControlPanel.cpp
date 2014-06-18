// ===========================================================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2012 - 2014 Jarmo Nikkanen
// ===========================================================================================


#include "Scene.h"
#include "D3D9Frame.h"
#include "D3D9Util.h"
#include "D3D9Config.h"
#include "D3D9Client.h"
#include "D3D9Surface.h"
#include "D3D9Catalog.h"

using namespace oapi;


DWORD TextureSizeInBytes(LPDIRECT3DTEXTURE9 pTex)
{
	D3DSURFACE_DESC desc;	
	pTex->GetLevelDesc(0, &desc);
	DWORD lev = pTex->GetLevelCount();
	DWORD size = desc.Height*desc.Width;
	if (desc.Format==D3DFMT_DXT1) size=size>>1;
	if (desc.Format==D3DFMT_A8R8G8B8) size=size<<2;
	if (desc.Format==D3DFMT_X8R8G8B8) size=size<<2;
	if (desc.Format==D3DFMT_R5G6B5) size=size<<1;
	if (desc.Format==D3DFMT_A4R4G4B4) size=size<<1;
	if (desc.Format==D3DFMT_R8G8B8) size=size*3;

	if (lev) size += (size>>2) + (size>>4) + (size>>8);
	return size;
}


/*
void D3D9Client::ClearTimers()
{
	TimeGDI = 0.0;
	TimeGDIPad = 0.0;
	TimeD3D9Pad = 0.0;
	TimeBlit = 0.0;
	TimePlanets = 0.0;
	TimeVessels = 0.0;
	TimeVC = 0.0;
	TimeHUD = 0.0;
}*/

void D3D9Client::Item(WORD id, const char *label, const char *format, ...)
{
	/*
	ItemList[Items] = id;
	
	if (SelectedItem==id) pItemsSkp->SetTextColor(0xFFFF00);
	else                  pItemsSkp->SetTextColor(0x888800);		

	char buffer[256];
	va_list args; 
	va_start(args, format); 
			
	_vsnprintf_s(buffer, 255, 255, format, args); 	

	int len = strlen(buffer);

	pItemsSkp->Text(x,   y, label, 0);
	pItemsSkp->Text(x+c, y, buffer, len);

	RECT rect;
	rect.left   = x;
	rect.top    = y;
	rect.bottom = y + (pItemsSkp->GetCharSize()&0x0000FFFF);
	rect.right  = x + c + GetTextWidth(buffer, len);

	ItemRect[Items] = rect;
	Items++;
	*/
}

void D3D9Client::Label(const char *format, ...)
{
	char buffer[256];
	va_list args; 
	va_start(args, format); 
			
	_vsnprintf_s(buffer, 255, 255, format, args); 	

	int len = strlen(buffer);
	pItemsSkp->Text(20, LabelPos, buffer, len);
	LabelPos += 22;
}


void D3D9Client::RenderControlPanel()
{
	static char *OnOff[]={"Off","On"};
	static char *SkpU[]={"Auto","GDI"};

	static double scene_avg = 0.0;
	static double frame_avg = 0.0;
	static double scene_pek = 0.0;
	static double frame_pek = 0.0;
	static double sim_time  = 0.0;

	LPDIRECT3DDEVICE9 dev = pd3dDevice;
	
	pItemsSkp = oapiGetSketchpad(GetBackBufferHandle());

	oapi::Pen * pen    = oapiCreatePen(1, 10, 0xFFFF00);
	oapi::Pen * nullp  = oapiCreatePen(0, 1,  0xFFFFFF);
	oapi::Brush *brush = oapiCreateBrush(0xA0000000);
	oapi::Font *largef = oapiCreateFont(38,false,"Arial", FONT_BOLD);
	oapi::Font *smallf = oapiCreateFont(22,false,"Fixed");

	pItemsSkp->SetPen(nullp);

	if (bGDIBB==false) {
		pItemsSkp->SetBrush(brush);
		pItemsSkp->Rectangle(-1, -1, viewW+1, viewH+1);
	}
	
	
	pItemsSkp->SetTextColor(0x00FF00);
	pItemsSkp->SetFont(largef);
	pItemsSkp->Text(20,70,"D3D9Client Statistics & Control Panel",37);
	pItemsSkp->SetFont(smallf);
	LabelPos = 130;
	
	DWORD plain_count = 0, plain_size = 0;
	DWORD textr_count = 0, textr_size = 0;
	DWORD rendt_count = 0, rendt_size = 0;
	DWORD dyntx_count = 0, dyntx_size = 0;
	DWORD sysme_count = 0, sysme_size = 0;

	DWORD nSurf = SurfaceCatalog->CountEntries();

	for (DWORD i=0;i<nSurf;i++) {
		LPD3D9CLIENTSURFACE pSurf = SURFACE(SurfaceCatalog->Get(i));
		if (pSurf->desc.Pool==D3DPOOL_DEFAULT) {
			if (pSurf->Type==D3D9S_TEXTURE) {
				textr_count++;
				textr_size += pSurf->GetSizeInBytes();
			}
			if (pSurf->Type==D3D9S_RENDER) {
				rendt_count++;
				rendt_size += pSurf->GetSizeInBytes();
			}
			if (pSurf->Type==D3D9S_DYNAMIC) {
				dyntx_count++;
				dyntx_size += pSurf->GetSizeInBytes();
			}
		}
		else {
			sysme_count++;
			sysme_size += pSurf->GetSizeInBytes();
		}
	}

	stats.count += 1.0;

	if (oapiGetSimTime()>(sim_time+1.2)) {
		sim_time  = oapiGetSimTime();
		scene_avg = stats.Scene / stats.count;
		frame_avg = stats.Frame / stats.count;
		scene_pek = stats.ScenePeak;
		frame_pek = stats.FramePeak;
		stats.Frame = stats.Scene = stats.ScenePeak = stats.FramePeak = stats.count = 0.0;
	}
	else {
		if (oapiGetSimTime()<sim_time) sim_time  = oapiGetSimTime();
	}

	Label("Available video mem..: %u MB", dev->GetAvailableTextureMem()>>20);
	Label("Surface Handles......: %u", nSurf);
	Label("SystemMem Surfaces...: %u (%u MB)", sysme_count, sysme_size>>20);
	Label("Dynamic Textures.....: %u (%u MB)", dyntx_count, dyntx_size>>20);
	Label("Render Targets.......: %u (%u MB)", rendt_count, rendt_size>>20);
	Label("Plain Surfaces.......: %u (%u MB)", plain_count, plain_size>>20);
	Label("Textures.............: %u (%u MB)", textr_count, textr_size>>20);

	DWORD mesh_count = MeshCatalog->CountEntries();
	DWORD tile_count = TileCatalog->CountEntries();
	DWORD tile_size = 0;
	
	for (DWORD i=0;i<tile_count;i++) {
		LPDIRECT3DTEXTURE9 pTex = (LPDIRECT3DTEXTURE9)TileCatalog->Get(i);
		if (pTex) tile_size += TextureSizeInBytes(pTex);
	}
				
	Label("Tiles Loaded.........: %u (%u MB)", tile_count, tile_size>>20); 
	Label("Meshes Loaded........: %u", mesh_count); 

	LabelPos += 22;

	DWORD tiles_rendered = 0;
	for (int i=8;i<=14;i++) tiles_rendered += stats.Tiles[i];

	Label("Vertices processed...: %u", stats.Vertices); 
	Label("Mesh groups rendered.: %u", stats.MeshGrps);
	Label("Meshes rendered......: %u", stats.Meshes);
	Label("Direct3D draw calls..: %u", stats.Draw); 
	Label("oapiBlt calls........: %u", stats.Blits); 
	Label("Color keyed blits....: %u", stats.ColorKey); 
	Label("Tiles rendered.......: %u", tiles_rendered);
	Label("Scene rendering......: %.0fus (%.0fus peak)", scene_avg, scene_pek); 
	Label("MFD, HUD, Panels.....: %.0fus (%.0fus peak)", frame_avg, frame_pek); 
	
	/*
	Label("Level 8 Tiles........: %u", stats.Tiles[8]);
	Label("Level 9 Tiles........: %u", stats.Tiles[9]);
	Label("Level 10 Tiles.......: %u", stats.Tiles[10]);
	Label("Level 11 Tiles.......: %u", stats.Tiles[11]);
	Label("Level 12 Tiles.......: %u", stats.Tiles[12]);
	Label("Level 13 Tiles.......: %u", stats.Tiles[13]);
	Label("Level 14 Tiles.......: %u", stats.Tiles[14]);
	*/

	LabelPos += 22;
	Label("Key  Function");
	Label("[S]  Sketchpad Usage.: %s", SkpU[Config->SketchpadMode]);
	
	pItemsSkp->SetPen(NULL);
	pItemsSkp->SetBrush(NULL);

	oapiReleaseFont(smallf);
	oapiReleaseFont(largef);
	oapiReleasePen(pen);
	oapiReleasePen(nullp);
	oapiReleaseBrush(brush);

	/*
	if (pItemsSkp==NULL) { LogErr("Failed to create a sketchpad for control panel"); return; }


	Title("Color of the Sun");
	Item(0x1001,"InScatter","%.5f",1.2);
	Item(0x1002,"OutScatter","%.5f",1.3);
	*/

	oapiReleaseSketchpad(pItemsSkp);
}



bool D3D9Client::ControlPanelMsg(WPARAM wParam)
{
	//if (wParam>='0' && wParam<='9') scatter.uEditMode = 1 + wParam - '0';
	//if (wParam>='A' && wParam<='B') scatter.uEditMode = 11 + wParam - 'A';
	
	if (wParam=='S') {
		Config->SketchpadMode++;
		if (Config->SketchpadMode>1) Config->SketchpadMode = 0;
		return true;
	}

	if (wParam==VK_UP) {
		
	}

	if (wParam==VK_DOWN) {
		
	}

	return false;
}