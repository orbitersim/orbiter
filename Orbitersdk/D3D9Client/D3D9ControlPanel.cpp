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
#include "DebugControls.h"

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

double Get(D3D9Time &t)
{
	return t.time / max(1.0, min(1000.0, t.count));
}

void Reset(D3D9Time &t)
{
	t.count = t.time = t.peak = 0.0;
}

void D3D9Client::DrawTimeBar(double t, double s, double f, DWORD color, const char *label)
{
	static double x = 8;
	static int z = 100;
	char legend[256];

	if (color == 0) {
		x = 8;
		z = 100;
		return;
	}

	sprintf_s(legend, 256, "%.64s, %0.2fms (%.2f%%)", label, (t/f)*0.001, t*0.0001);

	D3D9PadBrush brush(color);
	int y = viewH - 20;
	int q = viewW - 640;
	pItemsSkp->SetBrush(&brush);
	pItemsSkp->Rectangle(int(x), y, int(x + t*s), y + 16);
	pItemsSkp->Rectangle(q, z, q + 32, z + 16);
	pItemsSkp->Text(q+40, z, legend, -1);
	pItemsSkp->SetBrush(NULL);
	x += t*s;
	z += 32;
}

void D3D9Client::RenderControlPanel()
{
	static char *OnOff[]={"Off","On"};
	static char *SkpU[]={"Auto","GDI"};

//	static double sim_time  = 0.0;

	LPDIRECT3DDEVICE9 dev = pDevice;
	
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

	for (auto it = SurfaceCatalog->cbegin(); it != SurfaceCatalog->cend(); ++it) {
		LPD3D9CLIENTSURFACE pSurf = *it;
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
	
	for (auto it = TileCatalog->cbegin(); it != TileCatalog->cend(); ++it) {
		if (*it) tile_size += TextureSizeInBytes(*it);
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

	for (auto it = MeshCatalog->cbegin(); it != MeshCatalog->cend(); ++it) {
		D3D9Mesh *pMesh = *it;
		if (pMesh) {
			tot_verts += pMesh->GetVertexCount();
			tot_trans += pMesh->GetGroupTransformCount();
			tot_group += pMesh->GetGroupCount();
		}
	}

	static DWORD matchg = 0, texchg = 0;
	static DWORD verts = 0, grps = 0, meshes = 0;
	static double DCPeak = 0.0;
	static double LockPeak = 0.0;

	LabelPos += 22;
	Label("Meshes Loaded........: %u ", mesh_count);
	Label("Vertices Allocated...: %u (%u MB)", tot_verts, (tot_verts*sizeof(NMVERTEX))>>20); 
	Label("Groups Allocated.....: %u", tot_group);
	Label("Group Tarnsforms.....: %u", tot_trans); 
	Label("Mesh vertices render.: %u", verts);
	Label("Mesh groups rendered.: %u", grps);
	Label("Meshes rendered......: %u", meshes);
	Label("Texture changes......: %u", texchg);
	Label("Material changes.....: %u", matchg);
	Label("GetDC peak time......: %0.2fms", DCPeak*0.001);
	Label("Lock wait peak time..: %0.2fms", LockPeak*0.001);

	if (DebugControls::IsActive()) {

		if (DebugControls::IsSelectedGroupRendered()) Label("Group is rendered....: Yes");
		else Label("Group is rendered....: No");

		vObject *vObj = DebugControls::GetVisual();
		if (vObj) {
			DWORD nMesh = vObj->GetMeshCount();
			for (DWORD i = 0; i < nMesh; i++) {
				D3D9Mesh *hMesh = static_cast<D3D9Mesh *>(vObj->GetMesh(i));
				hMesh->ResetRenderStatus();
			}
		}
	}
	
	// DRAW TIME LINE ------------------------------------------------------------------

	static double systime = 0.0;
	static double frames = 1.0;
	static double lock;
	static double blit;
	static double getdc;
	static double scene;
	static double scale;
	static double outside;
	static double update;
	static double display;
	// -------------------------------------
//	static double pln_srf;
//	static double pln_cld;
	// -------------------------------------
//	static double scn_pst;
//	static double scn_ves;
//	static double scn_vc;
//	static double scn_hud;
//	static double scn_cam;
	// -------------------------------------
//	static double prt_cam;
//	static double prt_env;
//	static double prt_blr;
	
	if (oapiGetSysMJD() > systime) {
		
		systime = oapiGetSysMJD() + 0.8 / 86400.0;

		frames = D3D9Stats.Timer.Scene.count;
		double iframes = 1.0 / frames;

		matchg = DWORD(double(D3D9Stats.Mesh.MtrlChanges) * iframes);
		texchg = DWORD(double(D3D9Stats.Mesh.TexChanges) * iframes);
		verts = DWORD(double(D3D9Stats.Mesh.Vertices) * iframes);
		grps = DWORD(double(D3D9Stats.Mesh.MeshGrps) * iframes);
		meshes = DWORD(double(D3D9Stats.Mesh.Meshes) * iframes);
		DCPeak = D3D9Stats.Timer.GetDC.peak;
		LockPeak = D3D9Stats.Timer.LockWait.peak;

		double total = D3D9Stats.Timer.FrameTotal.time;

		scene   = D3D9Stats.Timer.Scene.time;
		update  = D3D9Stats.Timer.Update.time;
		display = D3D9Stats.Timer.Display.time;
		lock    = D3D9Stats.Timer.LockWait.time;
		blit    = D3D9Stats.Timer.BlitTime.time;
		getdc	= D3D9Stats.Timer.GetDC.time;

		// Time spend outside of client
		outside = total - (scene + update + display + lock + blit + getdc);

		scale   = double(viewW - 16) / total;

		// SCENE --------------------------------
//		prt_cam = D3D9Stats.Timer.CustCams.time;
//		prt_env = D3D9Stats.Timer.EnvMap.time;
//		prt_blr = D3D9Stats.Timer.EnvBlur.time;
		// -------------------------------------
//		pln_srf = D3D9Stats.Timer.Surface.time;
//		pln_cld = D3D9Stats.Timer.Clouds.time;
		// -------------------------------------
//		scn_cam = D3D9Stats.Timer.CamVis.time;
//		scn_pst = D3D9Stats.Timer.PostProcess.time;
//		scn_ves = D3D9Stats.Timer.Vessels.time;
//		scn_vc  = D3D9Stats.Timer.VirtualCP.time;
//		scn_hud = D3D9Stats.Timer.HUDOverlay.time;

		// -------------------------------------
		//scene -= prt_cam + prt_env + prt_blr + pln_srf + pln_cld + scn_cam + scn_pst + scn_ves + scn_vc + scn_hud;
		// At this point scene contains a time spend in unmeasured sections

		// -------------------------------------
		Reset(D3D9Stats.Timer.CamVis);
		Reset(D3D9Stats.Timer.Scene);
		Reset(D3D9Stats.Timer.Update);
		Reset(D3D9Stats.Timer.Display);
		Reset(D3D9Stats.Timer.LockWait);
		Reset(D3D9Stats.Timer.BlitTime);
		Reset(D3D9Stats.Timer.GetDC);
		Reset(D3D9Stats.Timer.FrameTotal);
		// -------------------------------------
		Reset(D3D9Stats.Timer.HUDOverlay);
		// -------------------------------------
		memset2(&D3D9Stats.Mesh, 0, sizeof(D3D9Stats.Mesh));
	}
	

	DrawTimeBar(0, 0, 0, 0);
	DrawTimeBar(outside, scale, frames, 0x774411, "Non-client specific tasks");
	DrawTimeBar(blit,    scale, frames, 0xFF5555, "Time used in Bliting");
	DrawTimeBar(getdc,	 scale, frames, 0x33FFFF, "Time used in GetDC");
	DrawTimeBar(lock,    scale, frames, 0x0066FF, "Waiting Locks (CPU-IDLE)");
	DrawTimeBar(display, scale, frames, 0x000099, "Waiting Present (CPU-IDLE)");
	DrawTimeBar(scene,   scale, frames, 0x00CC00, "Drawing Scene");

	//------------------------------
	/*DrawTimeBar(scn_cam, scale, frames, 0x000055, "Visual updates");
	DrawTimeBar(pln_srf, scale, frames, 0x005500, "Planet's surface");
	DrawTimeBar(pln_cld, scale, frames, 0x55FF55, "Planet's cloud layer");
	DrawTimeBar(scn_ves, scale, frames, 0xFF0000, "Vessel objects");
	DrawTimeBar(scn_vc,  scale, frames, 0xFFFF00, "Virtual cockpit");
	DrawTimeBar(scn_hud, scale, frames, 0x777700, "HUD and 2D panels");
	DrawTimeBar(scn_pst, scale, frames, 0xFF00FF, "Post-processing effects");
	//------------------------------
	DrawTimeBar(prt_cam, scale, frames, 0x00FFFF, "Custom cameras");
	DrawTimeBar(prt_env, scale, frames, 0x007777, "Environment map");
	DrawTimeBar(prt_blr, scale, frames, 0x004444, "EnvMap blur");
	//-------------------------------
	DrawTimeBar(scene,   scale, frames, 0x777777, "Other rendering tasks");*/
	
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