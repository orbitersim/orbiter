// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#define STRICT 1
#define OAPI_IMPLEMENTATION

#include "Orbiter.h"
#include "Launchpad.h"
#include "Psys.h"
#include "Pane.h"
#include "VCockpit.h"
#include "GraphicsAPI.h"
#include "DlgMgr.h"
#include "Log.h"
#include "Util.h"
#include "OrbiterPlatform.h"
#include <filesystem>
namespace fs = std::filesystem;

using std::min;

extern Orbiter *g_pOrbiter;
extern PlanetarySystem *g_psys;
extern Pane *g_pane;

using namespace oapi;

extern const TCHAR *g_strAppTitle;

// Render window callback (calls RenderWndProc)
static LRESULT RenderWindowProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	GraphicsClient *gc = (GraphicsClient*)oplat::GetWindowUserData (hWnd);
	if (gc) return gc->RenderWndProc (hWnd, uMsg, wParam, lParam);
	else return DefWindowProc (hWnd, uMsg, wParam, lParam);
}

// ======================================================================
// class GraphicsClient

GraphicsClient::GraphicsClient (HINSTANCE hInstance): Module (hInstance)
{
	hOrbiterInst = g_pOrbiter->GetInstance();
	VideoData.fullscreen = false;
	VideoData.forceenum = true;
	VideoData.trystencil = false;
	VideoData.novsync = true;
	VideoData.pageflip = true;
	VideoData.deviceidx = -1;
	VideoData.modeidx = 0;
	VideoData.outputidx = 0;
	VideoData.style = 1;
	VideoData.winw = 1024;
	VideoData.winh = 768;
	surfBltTgt = RENDERTGT_NONE;
	splashFont = 0;
	hRenderWnd = NULL;
}

// ======================================================================

GraphicsClient::~GraphicsClient ()
{
	if (splashFont) clbkReleaseFont (splashFont);
}

// ======================================================================

bool GraphicsClient::clbkInitialise ()
{
	// set default parameters from config data
	Config *cfg = g_pOrbiter->Cfg();
	VideoData.fullscreen = cfg->CfgDevPrm.bFullscreen;
	VideoData.forceenum  = cfg->CfgDevPrm.bForceEnum;
	VideoData.trystencil = cfg->CfgDevPrm.bTryStencil;
	VideoData.novsync    = cfg->CfgDevPrm.bNoVsync;
	VideoData.pageflip   = cfg->CfgDevPrm.bPageflip;
	VideoData.deviceidx  = cfg->CfgDevPrm.Device_idx;
	VideoData.outputidx  = cfg->CfgDevPrm.Device_out;
	VideoData.style		 = cfg->CfgDevPrm.Device_style;
	VideoData.modeidx    = (int)cfg->CfgDevPrm.Device_mode;
	VideoData.winw       = (int)cfg->CfgDevPrm.WinW;
	VideoData.winh       = (int)cfg->CfgDevPrm.WinH;

	char fname[256];
	GetModuleFileName(hModule, fname, 256);
	if (g_pOrbiter->Launchpad())
		g_pOrbiter->Launchpad()->OnGraphicsClientLoaded(this, fname);

	return true;
}

// ======================================================================

void GraphicsClient::RegisterVisObject (OBJHANDLE hObj, VISHANDLE vis)
{
	((Body*)hObj)->RegisterVisual (vis);
}

// ======================================================================

void GraphicsClient::UnregisterVisObject (OBJHANDLE hObj)
{
	((Body*)hObj)->UnregisterVisual();
}

// ======================================================================

int GraphicsClient::clbkVisEvent (OBJHANDLE hObj, VISHANDLE vis, DWORD msg, DWORD_PTR context)
{
	return 0;
}

// ======================================================================

ParticleStream *GraphicsClient::clbkCreateParticleStream (PARTICLESTREAMSPEC *pss)
{
	return NULL;
}

// ======================================================================

ParticleStream *GraphicsClient::clbkCreateExhaustStream (PARTICLESTREAMSPEC *pss,
	OBJHANDLE hVessel, const double *lvl, const VECTOR3 *ref, const VECTOR3 *dir)
{
	return NULL;
}

// ======================================================================

ParticleStream *GraphicsClient::clbkCreateExhaustStream (PARTICLESTREAMSPEC *pss,
	OBJHANDLE hVessel, const double *lvl, const VECTOR3 &ref, const VECTOR3 &dir)
{
	return NULL;
}

// ======================================================================

ParticleStream *GraphicsClient::clbkCreateReentryStream (PARTICLESTREAMSPEC *pss,
	OBJHANDLE hVessel)
{
	return NULL;
}

// ======================================================================

ScreenAnnotation *GraphicsClient::clbkCreateAnnotation ()
{
	TRACENEW; return new ScreenAnnotation (this);
}

// ======================================================================

bool GraphicsClient::TexturePath (const char *fname, char *path) const
{
	// first try htex directory
	strcpy (path, g_pOrbiter->Cfg()->CfgDirPrm.HightexDir);
	strcat (path, fname);
	if (fs::exists(path)) return true;

	// try tex directory
	strcpy (path, g_pOrbiter->Cfg()->CfgDirPrm.TextureDir);
	strcat (path, fname);

	if (fs::exists(path)) return true;

	return false;
}

// ======================================================================

bool GraphicsClient::PlanetTexturePath(const char* planetname, char* path) const
{
	g_pOrbiter->Cfg()->PTexPath(path, planetname);
	return true;
}

// ======================================================================

SURFHANDLE GraphicsClient::GetVCHUDSurface (const VCHUDSPEC **hudspec) const
{
	VirtualCockpit *vc;
	if (g_pane && g_pane->GetHUD() && (vc = g_pane->GetVC())) {
		*hudspec = vc->GetHUDParams ();
		return vc->GetHUDSurf();
	} else
		return NULL;
}

// ======================================================================

SURFHANDLE GraphicsClient::GetMFDSurface (int mfd) const
{
	return (g_pane ? g_pane->GetMFDSurface (mfd) : NULL);
}

// ======================================================================

SURFHANDLE GraphicsClient::GetVCMFDSurface (int mfd, const VCMFDSPEC **mfdspec) const
{
	*mfdspec = g_pane->GetVCMFDParams (mfd);
	if (g_pane && g_pane->GetVC() && g_pane->MFD (mfd)) {
		return g_pane->MFD(mfd)->Texture();
	} else
		return NULL;
}

// ======================================================================

DWORD GraphicsClient::GetBaseTileList (OBJHANDLE hBase, const SurftileSpec **tile) const
{
	return ((Base*)hBase)->GetTileList (tile);
}

// ======================================================================

void GraphicsClient::GetBaseStructures (OBJHANDLE hBase, MESHHANDLE **mesh_bs, DWORD *nmesh_bs, MESHHANDLE **mesh_as, DWORD *nmesh_as) const
{
	((Base*)hBase)->ExportBaseStructures ((Mesh***)mesh_bs, nmesh_bs, (Mesh***)mesh_as, nmesh_as);
}

// ======================================================================

void GraphicsClient::GetBaseShadowGeometry (OBJHANDLE hBase, MESHHANDLE **mesh_sh, double **elev, DWORD *nmesh_sh) const
{
	((Base*)hBase)->ExportShadowGeometry ((Mesh***)mesh_sh, elev, nmesh_sh);
}

// ======================================================================

void GraphicsClient::GetBaseCollisionStructures (OBJHANDLE hBase, MESHHANDLE **mesh_col, DWORD *nmesh_col) const
{
	((Base*)hBase)->ExportCollisionMeshes ((Mesh***)mesh_col, nmesh_col);
}

// ======================================================================

const void *GraphicsClient::GetConfigParam (DWORD paramtype) const
{
	return g_pOrbiter->Cfg()->GetParam (paramtype);
}

// ======================================================================

HWND GraphicsClient::clbkCreateRenderWindow ()
{
	oplat::WindowDesc desc;
	desc.title = g_strAppTitle;
	desc.width = VideoData.winw;
	desc.height = VideoData.winh;
	desc.fullscreen = VideoData.fullscreen && !g_pOrbiter->Cfg()->CfgCmdlinePrm.bHidden;
	desc.visible = !g_pOrbiter->Cfg()->CfgCmdlinePrm.bHidden;
	desc.resizable = false;
	desc.proc = RenderWindowProc;
	desc.userData = this;
	return oplat::OpenWindow (desc);
}

// ======================================================================

void GraphicsClient::clbkDestroyRenderWindow (bool fastclose)
{
	if (splashFont) {
		clbkReleaseFont (splashFont);
		splashFont = 0;
	}
}

// ======================================================================

void GraphicsClient::Render2DOverlay ()
{
	g_pane->Render();
}

// ======================================================================

bool GraphicsClient::ElevationGrid (ELEVHANDLE hElev, int ilat, int ilng, int lvl,
	int pilat, int pilng, int plvl, INT16 *pelev, float *elev, double *emean) const
{
	if (!hElev) return false;
	ElevationManager *emgr = (ElevationManager*)hElev;
	emgr->ElevationGrid (ilat, ilng, lvl, pilat, pilng, plvl, pelev, elev, emean);
	return true;
}

// ======================================================================

bool GraphicsClient::ElevationGrid(ELEVHANDLE hElev, int ilat, int ilng, int lvl,
	int pilat, int pilng, int plvl, INT16* pelev, INT16* elev, double* emean) const
{
	if (!hElev) return false;
	ElevationManager* emgr = (ElevationManager*)hElev;
	emgr->ElevationGrid(ilat, ilng, lvl, pilat, pilng, plvl, pelev, elev, emean);
	return true;
}

// ======================================================================

void GraphicsClient::ShowDefaultSplash ()
{
	const DWORD texcol = 0xA06060;
	DWORD rw, rh;
	clbkGetViewportSize (&rw, &rh);

	//const DWORD bmw = rw, bmh = (rw*10)/16; // source image is 1920x1200, i.e. 16/20 aspect ratio
	const DWORD bmw = min(rw, (rh*16)/10);
	const DWORD bmh = (bmw*10)/16;
	// copy splash screen to viewport
	SURFHANDLE surf = clbkLoadSurface ("Images/Splash.jpg", OAPISURFACE_RENDERTARGET, true);
	DWORD tgtx = 0, tgty = 0;
	if (bmw < rw) tgtx = (rw-bmw)/2;
	if (bmh < rh) tgty = (rh-bmh)/2;
	if (surf) {
		DWORD sw, sh;
		clbkGetSurfaceSize (surf, &sw, &sh);
		clbkScaleBlt (NULL, tgtx, tgty, bmw, bmh, surf, 0, 0, sw, sh);
		clbkReleaseSurface (surf);
	}

	oapi::Sketchpad *skp = clbkGetSketchpad (NULL);
	skp->SetBackgroundMode (oapi::Sketchpad::BK_TRANSPARENT);
	DWORD fontsize = 16; //max(rw/120,10);
	DWORD x0 = 10;//rw-fontsize*25;
	DWORD y0 = rh-fontsize; //tgty + (DWORD)(rw*0.078);
	if (splashFont) clbkReleaseFont (splashFont);
	splashFont = clbkCreateFont(fontsize,true,"Arial", FONT_NORMAL);
	skp->SetFont (splashFont);
	skp->SetTextColor (texcol);
	skp->SetTextAlign (oapi::Sketchpad::LEFT, oapi::Sketchpad::TOP);

	//DeleteObject(hbm);
	clbkReleaseSketchpad (skp);

	clbkDisplayFrame();
}

// ======================================================================

bool GraphicsClient::ReadImageFromMemory (BYTE *pBuf, DWORD nBuf, std::vector<BYTE> &bgra, UINT &w, UINT &h)
{
	oplat::Image img;
	if (!oplat::LoadImageMemory (pBuf, nBuf, img, (int)w, (int)h))
		return false;
	w = img.width;
	h = img.height;
	bgra.swap (img.bgra);
	return true;
}

// ======================================================================

bool GraphicsClient::ReadImageFromFile (const char *fname, std::vector<BYTE> &bgra, UINT &w, UINT &h)
{
	oplat::Image img;
	if (!oplat::LoadImageFile (fname, img, (int)w, (int)h))
		return false;
	w = img.width;
	h = img.height;
	bgra.swap (img.bgra);
	return true;
}

// ======================================================================

bool GraphicsClient::WriteImageDataToFile (const ImageData &data,
	const char *fname, ImageFileFormat fmt, float quality)
{
	const char *extension[4] = {".bmp", ".png", ".jpg", ".tif"};
	if (fmt < IMAGE_BMP || fmt > IMAGE_TIF)
		return false;
	if (data.bpp != 24 && data.bpp != 32)
		return false;
	if (data.bufsize < data.stride * data.height)
		return false;

	std::string path = std::string(fname) + extension[fmt];
	return oplat::SaveImageFile (path, data.data, data.width, data.height, data.stride, data.bpp,
		(oplat::ImageFileType)fmt, quality);
}

// ======================================================================

void GraphicsClient::clbkRender2DPanel (SURFHANDLE *hSurf, MESHHANDLE hMesh, MATRIX3 *T, bool additive)
{
	// can we do any default device-independent processing here?
}

// ======================================================================

void GraphicsClient::clbkRender2DPanel (SURFHANDLE *hSurf, MESHHANDLE hMesh, MATRIX3 *T, float alpha, bool additive)
{
	// if not implemented by the client, just use default (opaque) rendering
	clbkRender2DPanel (hSurf, hMesh, T, additive);
}

// ======================================================================

int GraphicsClient::clbkBeginBltGroup (SURFHANDLE tgt)
{
	if (tgt == RENDERTGT_NONE)
		return -3;
	if (surfBltTgt != RENDERTGT_NONE && surfBltTgt != tgt)
		return -2;
	surfBltTgt = tgt;
	return 0;
}

// ======================================================================

int GraphicsClient::clbkEndBltGroup ()
{
	if (surfBltTgt == RENDERTGT_NONE)
		return -2;
	surfBltTgt = RENDERTGT_NONE;
	return 0;
}

// ======================================================================

HWND GraphicsClient::InitRenderWnd (HWND hWnd)
{
	if (!hWnd) { // create a hidden window to receive input
		oplat::WindowDesc desc;
		desc.title = "Orbiter";
		desc.width = 10;
		desc.height = 10;
		desc.visible = false;
		desc.metal = false;
		hWnd = oplat::OpenWindow (desc);
	}
	// store class instance with window for access in the message handler
	oplat::SetWindowUserData (hWnd, this);
	oplat::SetWindowProc (hWnd, RenderWindowProc);
	hRenderWnd = hWnd;
	return hRenderWnd;
}

// ======================================================================


LRESULT GraphicsClient::RenderWndProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg) {
	// graphics-specific stuff to go here
	default:
		return g_pOrbiter->MsgProc (hWnd, uMsg, wParam, lParam);
	}
    return DefWindowProc (hWnd, uMsg, wParam, lParam);
}

// ======================================================================

const std::vector<GraphicsClient::LABELLIST>& GraphicsClient::GetCelestialMarkers() const
{
	return g_psys->LabelList();
}

// ==================================================================

DWORD GraphicsClient::GetSurfaceMarkers (OBJHANDLE hObj, const LABELLIST **sm_list) const
{
	int nlist;
	*sm_list = ((Planet*)hObj)->LabelList (&nlist);
	return (DWORD)nlist;
}

// ==================================================================

DWORD GraphicsClient::GetSurfaceMarkerLegend (OBJHANDLE hObj, const LABELTYPE **lspec) const
{
	Planet *planet = (Planet*)hObj;
	*lspec = planet->LabelLegend();
	return planet->NumLabelLegend();
}

// ======================================================================
// ======================================================================
// class ParticleStream

ParticleStream::ParticleStream (GraphicsClient *_gc, PARTICLESTREAMSPEC *pss)
{
	gc = _gc;
	level = NULL;
	hRef = NULL;
	lpos = _V(0,0,0);  pos = &lpos;
	ldir = _V(0,0,0);  dir = &ldir;
}

ParticleStream::~ParticleStream ()
{
	if (hRef) // stream is being deleted while still attached
		((Vessel*)hRef)->DelParticleStream (this);
		// notify vessel
}

void ParticleStream::Attach (OBJHANDLE hObj, const VECTOR3 *ppos, const VECTOR3 *pdir, const double *srclvl)
{
	hRef = hObj;
	SetVariablePos (ppos);
	SetVariableDir (pdir);
	SetLevelPtr (srclvl);
}

void ParticleStream::Attach (OBJHANDLE hObj, const VECTOR3 &_pos, const VECTOR3 &_dir, const double *srclvl)
{
	hRef = hObj;
	SetFixedPos (_pos);
	SetFixedDir (_dir);
	SetLevelPtr (srclvl);
}

void ParticleStream::Detach ()
{
	level = NULL;
	hRef = NULL;
	pos = &lpos;
	dir = &ldir;
}

void ParticleStream::SetFixedPos (const VECTOR3 &_pos)
{
	lpos = _pos;  pos = &lpos;
}

void ParticleStream::SetFixedDir (const VECTOR3 &_dir)
{
	ldir = _dir;  dir = &ldir;
}

void ParticleStream::SetVariablePos (const VECTOR3 *ppos)
{
	pos = ppos;
}

void ParticleStream::SetVariableDir (const VECTOR3 *pdir)
{
	dir = pdir;
}

void ParticleStream::SetLevelPtr (const double *srclvl)
{
	level = srclvl;
}


// ======================================================================
// ======================================================================
// class ScreenAnnotation

ScreenAnnotation::ScreenAnnotation (GraphicsClient *_gc)
{
	gc = _gc;
	_gc->clbkGetViewportSize (&viewW, &viewH);
	txt = 0;
	buflen = 0;
	txtscale = 0;
	font = NULL;
	//hFont = NULL;
	Reset();
}

ScreenAnnotation::~ScreenAnnotation ()
{
	if (txt) delete[]txt;
	if (font) gc->clbkReleaseFont (font);
	//DeleteObject (hFont);
}

void ScreenAnnotation::Reset ()
{
	ClearText();
	SetPosition (0.1, 0.1, 0.5, 0.6);
	SetColour (_V(1.0,0.7,0.2));
	SetSize (1.0);
}

void ScreenAnnotation::SetPosition (double x1, double y1, double x2, double y2)
{
	nx1 = (int)(x1*viewW); nx2 = (int)(x2*viewW); nw = nx2-nx1;
	ny1 = (int)(y1*viewH); ny2 = (int)(y2*viewH); nh = ny2-ny1;
}

void ScreenAnnotation::SetText (char *str)
{
	int len = strlen (str)+1;
	if (len > buflen) {
		if (txt) delete []txt;
		txt = new char[len]; TRACENEW
		buflen = len;
	}
	strcpy (txt, str);
	txtlen = len-1;
	for (int i = 0; i < txtlen-1; i++)
		if (txt[i] == ' ' && txt[i+1] == ' ') {
			txt[i] = '\r';
			txt[i+1] = '\n';
		}
}

void ScreenAnnotation::ClearText ()
{
	txtlen = 0;
}

void ScreenAnnotation::SetSize (double scale)
{
	if (scale != txtscale) {
		txtscale = scale;
		hf = (int)(viewH*txtscale/35.0);
		if (font) gc->clbkReleaseFont (font);
		font = gc->clbkCreateFont (-hf, true, "Sans");
	}
}

void ScreenAnnotation::SetColour (const VECTOR3 &col)
{
	int r = min (255, (int)(col.x*256.0));
	int g = min (255, (int)(col.y*256.0));
	int b = min (255, (int)(col.z*256.0));
	txtcol  = r | (g << 8) | (b << 16);
	txtcol2 = (r/2) | ((g/2) << 8) | ((b/2) << 16);
}

void ScreenAnnotation::Render ()
{
	if (!txtlen) return;

	Sketchpad *skp = gc->clbkGetSketchpad (0);
	if (skp) {
		skp->SetFont (font);
		skp->SetTextColor (txtcol2);
		skp->SetBackgroundMode (oapi::Sketchpad::BK_TRANSPARENT);
		skp->TextBox (nx1+1, ny1+1, nx2+1, ny2+1, txt, txtlen);
		skp->SetTextColor (txtcol);
		skp->TextBox (nx1, ny1, nx2, ny2, txt, txtlen);
		gc->clbkReleaseSketchpad (skp);
	}
}

// ======================================================================
// Nonmember functions

// ======================================================================
// API interface: register/unregister the graphics client

DLLEXPORT bool oapiRegisterGraphicsClient (GraphicsClient *gc)
{
	return g_pOrbiter->AttachGraphicsClient (gc);
}

DLLEXPORT bool oapiUnregisterGraphicsClient (GraphicsClient *gc)
{
	return g_pOrbiter->RemoveGraphicsClient (gc);
}
