// ===========================================================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2012 - 2016 Jarmo Nikkanen
// ===========================================================================================


#include "Scene.h"
#include "D3D9Frame.h"
#include "D3D9Util.h"
#include "D3D9Config.h"
#include "D3D9Client.h"
#include "D3D9Surface.h"
#include "D3D9Catalog.h"
#include "Mesh.h"

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


void D3D9Client::Label(const char *format, ...)
{
	char buffer[256];
	va_list args;
	va_start(args, format);
			
	_vsnprintf_s(buffer, 255, 255, format, args);

	va_end(args);

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
	pItemsSkp->Text(20,70,"D3D9Client Statistics",21);
	pItemsSkp->SetFont(smallf);
	LabelPos = 130;
	
	DWORD plain_count = 0, plain_size = 0;
	DWORD textr_count = 0, textr_size = 0;
	DWORD rendt_count = 0, rendt_size = 0;
	DWORD rttex_count = 0, rttex_size = 0;
	DWORD dyntx_count = 0, dyntx_size = 0;
	DWORD sysme_count = 0, sysme_size = 0;
	DWORD duall_count = 0, duall_size = 0;

	DWORD nSurf = SurfaceCatalog->CountEntries();

	for (DWORD i=0;i<nSurf;i++) {
		LPD3D9CLIENTSURFACE pSurf = SURFACE(SurfaceCatalog->Get(i));
		if (pSurf->desc.Pool==D3DPOOL_DEFAULT) {
			if (pSurf->IsRenderTarget()) {
				if (pSurf->IsDualLayer()) {
					duall_count++;
					duall_size += pSurf->GetSizeInBytes();
				}
				else if (pSurf->IsTexture()) {
					rttex_count++;
					rttex_size += pSurf->GetSizeInBytes();
				}
				else {
					rendt_count++;
					rendt_size += pSurf->GetSizeInBytes();
				}
			}
			else if (pSurf->IsDynamic()) {
				dyntx_count++;
				dyntx_size += pSurf->GetSizeInBytes();
			}
			else if (pSurf->IsPlainSurface()) {
				plain_count++;
				plain_size += pSurf->GetSizeInBytes();
			}
			else {
				textr_count++;
				textr_size += pSurf->GetSizeInBytes();
			}
		}
		else {
			sysme_count++;
			sysme_size += pSurf->GetSizeInBytes();
		}
	}

	D3D9Stats.Timer.count += 1.0;

	if (oapiGetSimTime()>(sim_time+1.2)) {
		sim_time  = oapiGetSimTime();
		scene_avg = D3D9Stats.Timer.Scene / D3D9Stats.Timer.count;
		frame_avg = D3D9Stats.Timer.Frame / D3D9Stats.Timer.count;
		scene_pek = D3D9Stats.Timer.ScenePeak;
		frame_pek = D3D9Stats.Timer.FramePeak;
		D3D9Stats.Timer.Frame = D3D9Stats.Timer.Scene = D3D9Stats.Timer.ScenePeak = D3D9Stats.Timer.FramePeak = D3D9Stats.Timer.count = 0.0;
	}
	else {
		if (oapiGetSimTime()<sim_time) sim_time  = oapiGetSimTime();
	}

	Label("Available video mem..: %u MB", dev->GetAvailableTextureMem()>>20);
	Label("Surface Handles......: %u", nSurf);
	Label("SystemMem Surfaces...: %u (%u MB)", sysme_count, sysme_size>>20);
	Label("Dynamic Textures.....: %u (%u MB)", dyntx_count, dyntx_size>>20);
	Label("Render Targets.......: %u (%u MB)", rendt_count, rendt_size>>20);
	Label("Render Textures......: %u (%u MB)", rttex_count, rttex_size>>20);
	Label("Dual Layer Surfaces..: %u (%u MB)", duall_count, duall_size>>20);
	Label("Plain Surfaces.......: %u (%u MB)", plain_count, plain_size>>20);
	Label("Plain Textures.......: %u (%u MB)", textr_count, textr_size>>20);

	DWORD mesh_count = MeshCatalog->CountEntries();
	DWORD tile_count = TileCatalog->CountEntries();
	DWORD tile_size = 0;
	DWORD tile_render_countA = 0;
	DWORD tile_render_countB = 0;
	
	for (DWORD i=0;i<tile_count;i++) {
		LPDIRECT3DTEXTURE9 pTex = (LPDIRECT3DTEXTURE9)TileCatalog->Get(i);
		if (pTex) tile_size += TextureSizeInBytes(pTex);
	}

	for (DWORD i = 0; i<32; i++) {
		tile_render_countA += D3D9Stats.Old.Tiles[i];
		tile_render_countB += D3D9Stats.Surf.Tiles[i];
	}
		
	LabelPos += 22;
	Label("Tile Textures Loaded.: %u (%u MB)", tile_count, tile_size>>20); 
	Label("Tiles Rendered (Old).: %u (%u kVtx)", tile_render_countA, D3D9Stats.Old.Verts>>10);
	Label("Tiles Rendered (New).: %u (%u kVtx)", tile_render_countB, D3D9Stats.Surf.Verts>>10);
	Label("Tiles Allocated (New): %u", D3D9Stats.TilesAllocated);
	Label("Tile Vertex Cache....: %u (%u MB)", D3D9Stats.TilesCached, D3D9Stats.TilesCachedMB>>20);


	

	
	DWORD tot_verts = 0;
	DWORD tot_trans = 0;
	DWORD tot_group = 0;

	for (DWORD i=0;i<mesh_count;i++) {
		D3D9Mesh *pMesh = (D3D9Mesh *)MeshCatalog->Get(i);
		if (pMesh) {
			tot_verts += pMesh->GetVertexCount();
			tot_trans += pMesh->GetGroupTransformCount();
			tot_group += pMesh->GetGroupCount();
		}
	}

	LabelPos += 22;
	Label("Meshes Loaded........: %u ", mesh_count);
	Label("Vertices Allocated...: %u (%u MB)", tot_verts, (tot_verts*sizeof(NMVERTEX))>>20); 
	Label("Groups Allocated.....: %u", tot_group);
	Label("Group Tarnsforms.....: %u", tot_trans); 
	Label("Mesh vertices render.: %u", D3D9Stats.Mesh.Vertices);
	Label("Mesh groups rendered.: %u", D3D9Stats.Mesh.MeshGrps);
	Label("Meshes rendered......: %u", D3D9Stats.Mesh.Meshes);
	LabelPos += 22;
	Label("Scene rendering time.: %.0fus (%.0fus peak)", scene_avg, scene_pek); 
	Label("MFD, HUD, Panels.....: %.0fus (%.0fus peak)", frame_avg, frame_pek); 
	
	pItemsSkp->SetPen(NULL);
	pItemsSkp->SetBrush(NULL);

	oapiReleaseFont(smallf);
	oapiReleaseFont(largef);
	oapiReleasePen(pen);
	oapiReleasePen(nullp);
	oapiReleaseBrush(brush);

	oapiReleaseSketchpad(pItemsSkp);
}



bool D3D9Client::ControlPanelMsg(WPARAM wParam)
{
	return false;
}