// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//   ORBITER VISUALISATION PROJECT (OVP)
//   D3D7 Client module
// ==============================================================

// --------------------------------------------------------------
// D3D7Client.h
// Class D3D7Client
//
// DX7 version of a graphics subsystem for Orbiter, derived from
// the GraphicsClient class in the Orbiter API.
// --------------------------------------------------------------

#define STRICT 1
#define ORBITER_MODULE
#include "orbitersdk.h"
#include "D3D7Client.h"
#include "D3D7Config.h"
#include "D3D7Extra.h"
#include "D3D7Util.h"
#include "Scene.h"
#include "Particle.h"
#include "VVessel.h"
#include "VStar.h"
#include "Texture.h"
#include "MeshMgr.h"
#include "TileMgr.h"
#include "tilemgr2.h"
#include "RingMgr.h"
#include "HazeMgr.h"
#include "CSphereMgr.h"
#include "Log.h"

using namespace oapi;

// ==============================================================
// Global parameters

struct G_PARAM {
	HINSTANCE hInst;                // module instance handle
	D3D7Client* client;             // the client soliton
	LaunchpadItem* lpiD3D7;         // "Extra" group header: D3D7 configuration items
	LaunchpadItem* lpiPlanetRender; // "Extra" item: planet render parameters
} g_Param = { 0, nullptr, nullptr, nullptr };

// ==============================================================
// API interface
// ==============================================================

// ==============================================================
// Initialise module

DLLCLBK void InitModule (HINSTANCE hDLL)
{
	g_Param.hInst = hDLL;
	g_Param.client = new D3D7Client (hDLL);
	if (!oapiRegisterGraphicsClient (g_Param.client)) {
		oapiWriteLogError("Failed to register the D3D7 graphics client.");
		delete g_Param.client;
		g_Param.client = 0;
		// may not be a good idea to continue here - will probably crash later
	}

	// Create and register the Launchpad "Extra" entries for the client
	g_Param.lpiD3D7 = new D3D7ClientCfg;
	LAUNCHPADITEM_HANDLE hL = oapiRegisterLaunchpadItem(g_Param.lpiD3D7);
	g_Param.lpiPlanetRender = new D3D7PlanetRenderCfg(g_Param.client);
	oapiRegisterLaunchpadItem(g_Param.lpiPlanetRender, hL);
}

// ==============================================================
// Clean up module

DLLCLBK void ExitModule (HINSTANCE hDLL)
{
	// Un-register and delete the Launchpad "Extra" entries for the client
	oapiUnregisterLaunchpadItem(g_Param.lpiPlanetRender);
	delete g_Param.lpiPlanetRender;
	g_Param.lpiPlanetRender = 0;
	oapiUnregisterLaunchpadItem(g_Param.lpiD3D7);
	delete g_Param.lpiD3D7;
	g_Param.lpiD3D7 = 0;

	if (g_Param.client) {
		oapiUnregisterGraphicsClient (g_Param.client);
		delete g_Param.client;
		g_Param.client = nullptr;
	}
}

// ==============================================================
// D3D7Client class implementation
// ==============================================================

D3D7Client::D3D7Client (HINSTANCE hInstance): GDIClient (hInstance)
{
	m_pFramework     = nullptr;
	m_pDD            = nullptr;
	m_pD3D           = nullptr;
	m_pD3DDevice     = nullptr;
	m_pDeviceInfo    = nullptr;
	pddsRenderTarget = NULL;
	hRenderWnd       = NULL;
	bFullscreen      = false;
	bStencil         = false;
	viewW = viewH    = 0;
	viewBPP          = 0;
	vtab             = NULL;
	scene            = NULL;
	meshmgr          = NULL;
	texmgr           = NULL;
	clipper          = NULL;
	lstatus.bkgDC    = 0;

	// Create the parameter manager
	cfg              = new D3D7Config;
}

// ==============================================================

D3D7Client::~D3D7Client ()
{
	// Unregister graphics client
	oapiUnregisterGraphicsClient (this);

	delete cfg;
	SAFE_DELETE (m_pFramework);
	D3D7Enum_FreeResources ();
	if (vtab) delete vtab;
	if (scene) delete scene;
	if (meshmgr) delete meshmgr;
	if (texmgr) delete texmgr;

	if (GetPenCount())
		oapiWriteLogV("D3D7Client: Sketchpad pens still allocated: %d", GetPenCount());
	if (GetBrushCount())
		oapiWriteLogV("D3D7Client: Sketchpad brushes still allocated: %d", GetBrushCount());
	if (GetFontCount())
		oapiWriteLogV("D3D7Client: Sketchpad fonts still allocated: %d", GetFontCount());
}

// ==============================================================

bool D3D7Client::clbkInitialise ()
{
	DWORD i;

	// Perform default setup
	if (!GraphicsClient::clbkInitialise ()) return false;

	// enumerate available D3D devices
	if (FAILED (D3D7Enum_EnumerateDevices (clbkConfirmDevice))) {
		LOGOUT_ERR ("Could not enumerate devices");
		return false;
	}

    // Select a device using user parameters from config file.
	VIDEODATA *data = GetVideoData();
	DeviceId dev_id = {(DWORD)data->deviceidx, (DWORD)data->modeidx, (DWORD)data->fullscreen, false};

	if (!(m_pDeviceInfo = PickDevice (&dev_id)))
		if (FAILED (D3D7Enum_SelectDefaultDevice (&m_pDeviceInfo))) {
			LOGOUT_ERR ("Could not select a device");
			return false;
		}

    // Create a new CD3DFramework class. This class does all of our D3D
    // initialization and manages the common D3D objects.
    if (NULL == (m_pFramework = new CD3DFramework7())) {
		LOGOUT_ERR ("Could not create D3D7 framework");
        return false;
    }

	// Output driver info to the Orbiter.log file
	D3D7Enum_DeviceInfo *pDevices;
	DWORD nDevices;
	D3D7Enum_GetDevices (&pDevices, &nDevices);
	LOGOUT ("Enumerated %d devices:", nDevices);
	for (i = 0; i < nDevices; i++) {
		LOGOUT ("[%c] %s (%cW)",
			pDevices[i].guidDevice == m_pDeviceInfo->guidDevice ? 'x':' ',
			pDevices[i].strDesc, pDevices[i].bHardware ? 'H':'S');
	}

	// Create the Launchpad video tab interface
	vtab = new VideoTab (this, ModuleInstance(), OrbiterInstance(), LaunchpadVideoTab());

	return true;
}

// ==============================================================

HWND D3D7Client::clbkCreateRenderWindow ()
{
	hRenderWnd = GraphicsClient::clbkCreateRenderWindow ();

#ifdef UNDEF
	if (VideoData.fullscreen) {
		hRenderWnd = CreateWindow (strWndClass, "[D3D7Client]",
			WS_POPUP | WS_VISIBLE,
			CW_USEDEFAULT, CW_USEDEFAULT, 10, 10, 0, 0, hModule, 0);
	} else {
		hRenderWnd = CreateWindow (strWndClass, strWndTitle,
			WS_OVERLAPPED | WS_CAPTION | WS_SYSMENU | WS_VISIBLE,
			CW_USEDEFAULT, CW_USEDEFAULT, VideoData.winw, VideoData.winh, 0, 0, hModule, 0);
	}
#endif
	SetWindowText (hRenderWnd, "[D3D7Client]");

	Initialise3DEnvironment();

	ValidateRect (hRenderWnd, NULL);
	// avoids white flash after splash screen

	InitOutputLoadStatus ();
	// prepare status output for splash screen

	return hRenderWnd;
}

// ==============================================================

void D3D7Client::clbkPostCreation ()
{
	ExitOutputLoadStatus ();
	if (scene) scene->Initialise();
}

// =======================================================================

bool D3D7Client::clbkSplashLoadMsg (const char *msg, int line)
{
	return OutputLoadStatus (msg, line);
}

// ==============================================================

void D3D7Client::clbkCloseSession (bool fastclose)
{
	GlobalExit();
	GDIClient::clbkCloseSession (fastclose);
	if (scene) {
		delete scene;
		scene = NULL;
	}
}

// ==============================================================

void D3D7Client::clbkDestroyRenderWindow (bool fastclose)
{
	GDIClient::clbkDestroyRenderWindow (fastclose);
	Cleanup3DEnvironment();
	hRenderWnd = NULL;
}

// ==============================================================

void D3D7Client::clbkRenderScene ()
{
	scene->Render();
}

// ==============================================================

void D3D7Client::clbkTimeJump (double simt, double simdt, double mjd)
{
	GDIClient::clbkTimeJump (simt, simdt, mjd);
}

// ==============================================================

bool D3D7Client::clbkDisplayFrame ()
{
	Output2DOverlay();

	if (!RenderWithPopupWindows())
		m_pFramework->ShowFrame();

	return true;
}

// ==============================================================

void D3D7Client::clbkStoreMeshPersistent (MESHHANDLE hMesh, const char *fname)
{
	meshmgr->StoreMesh (hMesh);
}

// =======================================================================
// Particle stream functions
// ==============================================================

ParticleStream *D3D7Client::clbkCreateParticleStream (PARTICLESTREAMSPEC *pss)
{
	return NULL;
}

// =======================================================================

ParticleStream *D3D7Client::clbkCreateExhaustStream (PARTICLESTREAMSPEC *pss,
	OBJHANDLE hVessel, const double *lvl, const VECTOR3 *ref, const VECTOR3 *dir)
{
	ExhaustStream *es = new ExhaustStream (this, hVessel, lvl, ref, dir, pss);
	scene->AddParticleStream (es);
	return es;
}

// =======================================================================

ParticleStream *D3D7Client::clbkCreateExhaustStream (PARTICLESTREAMSPEC *pss,
	OBJHANDLE hVessel, const double *lvl, const VECTOR3 &ref, const VECTOR3 &dir)
{
	ExhaustStream *es = new ExhaustStream (this, hVessel, lvl, ref, dir, pss);
	scene->AddParticleStream (es);
	return es;
}

// ======================================================================

ParticleStream *D3D7Client::clbkCreateReentryStream (PARTICLESTREAMSPEC *pss,
	OBJHANDLE hVessel)
{
	ReentryStream *rs = new ReentryStream (this, hVessel, pss);
	scene->AddParticleStream (rs);
	return rs;
}

// ======================================================================

bool D3D7Client::clbkParticleStreamExists (const ParticleStream *ps)
{
	return false;
}

// ==============================================================

SURFHANDLE D3D7Client::clbkLoadSurface(const char* fname, DWORD attrib, bool bPath)
{
	DWORD flags = 0;
	if (attrib & OAPISURFACE_SYSMEM)     flags |= 0x1;
	if (attrib & OAPISURFACE_UNCOMPRESS) flags |= 0x2;
	if (attrib & OAPISURFACE_NOMIPMAPS)  flags |= 0x4;
	if (attrib & OAPISURFACE_SHARED)     flags |= 0x8;
	return clbkLoadTexture(fname, flags);
}

// ==============================================================

SURFHANDLE D3D7Client::clbkLoadTexture (const char *fname, DWORD flags)
{
	if (!texmgr) return NULL;
	LPDIRECTDRAWSURFACE7 tex = NULL;

	if (flags & 8) // load managed
		texmgr->GetTexture (fname, &tex, flags);
	else           // load individual
		texmgr->LoadTexture (fname, &tex, flags);

	return (SURFHANDLE)tex;
}

// ==============================================================

void D3D7Client::clbkReleaseTexture (SURFHANDLE hTex)
{
	// We ignore the request for texture deallocation at this point,
	// and leave it to the texture manager to perform cleanup operations.
	// But it would be a good idea to decrement a reference count at
	// this point.
}

// ==============================================================

bool D3D7Client::clbkSetMeshTexture (DEVMESHHANDLE hMesh, DWORD texidx, SURFHANDLE tex)
{
	return ((D3D7Mesh*)hMesh)->SetTexture (texidx, tex);
}

// ==============================================================

int D3D7Client::clbkSetMeshMaterial (DEVMESHHANDLE hMesh, DWORD matidx, const MATERIAL *mat)
{
	D3D7Mesh *mesh = (D3D7Mesh*)hMesh;
	DWORD nmat = mesh->MaterialCount();
	if (matidx >= nmat) return 4; // "index out of range"
	D3DMATERIAL7 *meshmat = mesh->GetMaterial (matidx);
	memcpy (meshmat, mat, sizeof(D3DMATERIAL7)); // relies on D3DMATERIAL7 and MATERIAL to be equivalent
	return 0;
}

// ==============================================================

int D3D7Client::clbkMeshMaterial (DEVMESHHANDLE hMesh, DWORD matidx, MATERIAL *mat)
{
	D3D7Mesh *mesh = (D3D7Mesh*)hMesh;
	DWORD nmat = mesh->MaterialCount();
	if (matidx >= nmat) return 4; // "index out of range"
	D3DMATERIAL7 *meshmat = mesh->GetMaterial (matidx);
	memcpy (mat, meshmat, sizeof(D3DMATERIAL7)); // relies on D3DMATERIAL7 and MATERIAL to be equivalent
	return 0;
}

// ==============================================================

bool D3D7Client::clbkSetMeshProperty (DEVMESHHANDLE hMesh, DWORD property, DWORD value)
{
	D3D7Mesh *mesh = (D3D7Mesh*)hMesh;
	switch (property) {
	case MESHPROPERTY_MODULATEMATALPHA:
		mesh->EnableMatAlpha (value != 0);
		return true;
	}
	return false;
}

// ==============================================================

void D3D7Client::clbkPreOpenPopup ()
{
	if (clipper) m_pDD->FlipToGDISurface();
}

// ==============================================================

void D3D7Client::clbkNewVessel (OBJHANDLE hVessel)
{
	scene->NewVessel (hVessel);
}

// ==============================================================

void D3D7Client::clbkDeleteVessel (OBJHANDLE hVessel)
{
	scene->DeleteVessel (hVessel);
}

// ==============================================================

void D3D7Client::SetDefault (D3DVERTEXBUFFERDESC &vbdesc) const
{
	memset (&vbdesc, 0, sizeof(D3DVERTEXBUFFERDESC));
	vbdesc.dwSize = sizeof(D3DVERTEXBUFFERDESC);
	vbdesc.dwCaps = (m_pFramework->IsTLDevice() ? 0 : D3DVBCAPS_SYSTEMMEMORY);
}

// ==============================================================

HRESULT D3D7Client::Initialise3DEnvironment ()
{
	HRESULT hr;
	VIDEODATA *data = GetVideoData();
	DWORD dwFrameworkFlags = D3DFW_ZBUFFER;
	dwFrameworkFlags |= D3DFW_NO_FPUSETUP; // Orbiter needs double-precision FPU
	if (data->fullscreen) dwFrameworkFlags |= D3DFW_FULLSCREEN;
	if (data->novsync)    dwFrameworkFlags |= D3DFW_NOVSYNC;
	if (data->pageflip)   dwFrameworkFlags |= D3DFW_PAGEFLIP;
	if (data->trystencil) dwFrameworkFlags |= D3DFW_TRYSTENCIL;

	if (SUCCEEDED (hr = m_pFramework->Initialize (hRenderWnd,
		m_pDeviceInfo->pDriverGUID,
		m_pDeviceInfo->pDeviceGUID,
		&m_pDeviceInfo->ddsdFullscreenMode,
		dwFrameworkFlags))) {
		LOGOUT ("3D environment ok");

		m_pDD        = m_pFramework->GetDirectDraw();
        m_pD3D       = m_pFramework->GetDirect3D();
        m_pD3DDevice = m_pFramework->GetD3DDevice();
		pddsRenderTarget = m_pFramework->GetRenderSurface();

		// Get dimensions of the render surface 
		DDSURFACEDESC2 ddsd;
		ddsd.dwSize = sizeof (DDSURFACEDESC2);
		pddsRenderTarget->GetSurfaceDesc(&ddsd);
		viewW   = ddsd.dwWidth;
		viewH   = ddsd.dwHeight;
		viewBPP = ddsd.ddpfPixelFormat.dwRGBBitCount;

		// Get additional parameters
		bFullscreen = (m_pFramework->IsFullscreen() ? true:false);
		bStencil = (data->trystencil && m_pFramework->GetStencilBitDepth() > 0);
		D3D7Mesh::GlobalEnableSpecular (*(bool*)GetConfigParam(CFGPRM_OBJECTSPECULAR));

		ShowDefaultSplash();

		ShowDefaultSplash();

		// Output some render parameters to the log
		LogRenderParams();

		// Create the mesh manager instance
		meshmgr = new MeshManager (this);

		// Create the texture manager instance
		texmgr = new TextureManager (this);

		// Create clipper object
		if (bFullscreen) {
			if (m_pDD->CreateClipper (0, &clipper, NULL) == DD_OK)
				clipper->SetHWnd (0, hRenderWnd);
		}

		// Create the dialog manager instance
		//dlgmgr = new DialogManager;
		//dlgmgr->Init (hRenderWnd, data->fullscreen, pDD);

		// Device-specific initialisations
		TileManager::GlobalInit (this);
		TileManager2Base::GlobalInit (this);
		RingManager::GlobalInit (this);
		HazeManager::GlobalInit (this);
		CSphereManager::GlobalInit (this);
		D3D7ParticleStream::GlobalInit (this);
		vObject::GlobalInit (this);
		vVessel::GlobalInit (this);
		vStar::GlobalInit (this);

		// Create scene instance
		scene = new Scene (this, viewW, viewH);
	} else {
		LOGOUT_ERR ("Could not initialise 3D environment");
	}
	return hr;
}

// ==============================================================

void D3D7Client::GlobalExit()
{
	TileManager2Base::GlobalExit();
	TileManager::GlobalExit();
	D3D7ParticleStream::GlobalExit();
	vVessel::GlobalExit();
	vStar::GlobalExit();
	vObject::GlobalExit();
}

// ==============================================================

void D3D7Client::Cleanup3DEnvironment ()
{
	if (texmgr) {
		delete texmgr;
		texmgr = NULL;
	}
	if (clipper) {
		clipper->Release();
		clipper = NULL;
	}
	m_pFramework->DestroyObjects();
	m_pD3DDevice = nullptr;
	pddsRenderTarget = NULL;
	viewW = viewH = viewBPP = 0;
}

// =======================================================================

bool D3D7Client::RenderWithPopupWindows ()
{
	if (!bFullscreen) return false; // no special treatment required in windowed mode

	DWORD nWnd;
	const HWND *hPopupWnd;
	nWnd = GetPopupList (&hPopupWnd);
	if (!nWnd) return false; // nothing to do

	LPDIRECTDRAWSURFACE7 front = m_pFramework->GetFrontBuffer();
	LPDIRECTDRAWSURFACE7 back  = m_pFramework->GetBackBuffer();

	if (clipper) {
		front->SetClipper (clipper);
		front->Blt (NULL, back, NULL, DDBLT_WAIT, NULL);
		return true;
	} else {
		RECT rc;
		int x, y, cx, cy;
		DWORD i;
		HDC	hdcScreen, hdcBackBuffer;
		HRGN hrgn;
		hdcScreen = GetDC(NULL);
		back->GetDC (&hdcBackBuffer);
		for (i = 0; i < nWnd; i++) {
			HWND hDlg = hPopupWnd[i];
			GetWindowRect (hDlg, &rc);
			x  = rc.left;
			y  = rc.top;
			cx = rc.right - rc.left;
			cy = rc.bottom - rc.top;

			// If window has a complex region associated with it, be sure to include it in the draw
			hrgn = CreateRectRgn(0, 0, 0, 0);
			if (GetWindowRgn(hDlg, hrgn) == COMPLEXREGION) {
				OffsetRgn(hrgn, rc.left, rc.top);
				SelectClipRgn(hdcBackBuffer, hrgn);
			}

			BitBlt (hdcBackBuffer, x, y, cx, cy, hdcScreen, x, y, SRCCOPY);

			// Remove clipping region and clean up
			SelectClipRgn (hdcBackBuffer, NULL);
			DeleteObject(hrgn);
		}
		back->ReleaseDC (hdcBackBuffer);
		ReleaseDC (NULL, hdcScreen);
		return false;
		// we return false because the page still needs to be flipped
	}
}

// =======================================================================

static const DWORD LOADHEADERCOL = 0xB0B0B0; //0xFFFFFF
static const DWORD LOADSTATUSCOL = 0xC08080; //0xFFD0D0

void D3D7Client::InitOutputLoadStatus ()
{
#ifdef UNDEF
	HDC hDC;
	TEXTMETRIC tm;

	m_pFramework->GetBackBuffer()->GetDC (&hDC);
	DWORD sw = viewW, sh = viewH;
	GetTextMetrics (hDC, &tm);
	lstatus.h = tm.tmHeight*2;
	lstatus.w = min (400, sw-280);
	DWORD bmw = 1280;
	DWORD srcw = min(bmw,(10*sw*bmw)/(16*sh));
	DWORD srcx = (bmw-srcw)/2;
	lstatus.x = ((748-srcx)*sw)/srcw - lstatus.w;
	lstatus.y = (sh*4)/5;
	HBITMAP hBmp = CreateCompatibleBitmap (hDC, lstatus.w+15, lstatus.h+lstatus.h/2+10);
	lstatus.hBkg = CreateCompatibleBitmap (hDC, lstatus.w, lstatus.h);
	lstatus.bkgDC = CreateCompatibleDC (hDC);
	SelectObject (lstatus.bkgDC, hBmp);
	SelectObject (lstatus.bkgDC, GetStockObject(GRAY_BRUSH));
	Rectangle (lstatus.bkgDC, 0, 0, lstatus.w+15, lstatus.h+lstatus.h/2+10);
	SelectObject (lstatus.bkgDC, lstatus.hBkg);
	DeleteObject (hBmp);
	BitBlt (lstatus.bkgDC, 0, 0, lstatus.w, lstatus.h, hDC, lstatus.x, lstatus.y, SRCCOPY);
	SelectObject (hDC, GetStockObject (NULL_BRUSH));
	HPEN pen, ppen;
	pen = CreatePen (PS_SOLID, 1, LOADHEADERCOL);
	ppen = (HPEN)SelectObject(hDC, pen);
	MoveToEx (hDC, lstatus.x, lstatus.y-1, NULL); LineTo (hDC, lstatus.x+lstatus.w, lstatus.y-1);
	SelectObject (hDC, ppen);
	DeleteObject (pen);
	SetTextColor (hDC, LOADHEADERCOL);
	SetBkMode (hDC, TRANSPARENT);
	SetTextAlign (hDC, TA_RIGHT);
	TextOut (hDC, lstatus.x+lstatus.w, lstatus.y-lstatus.h/2-4, "Loading ...", 11);
	m_pFramework->GetBackBuffer()->ReleaseDC (hDC);
	clbkBlt (m_pFramework->GetFrontBuffer(), 0, 0, m_pFramework->GetBackBuffer()); 
#endif
}

// =======================================================================

void D3D7Client::ExitOutputLoadStatus ()
{
#ifdef UNDEF
	DeleteObject (lstatus.hBkg);
	DeleteDC (lstatus.bkgDC);
	lstatus.bkgDC = 0;
#endif
}

// =======================================================================

bool D3D7Client::OutputLoadStatus (const char *msg, int line)
{
#ifdef UNDEF
	static char linebuf[2][70] = {"", ""};

	if (!lstatus.bkgDC) return false;
	HDC hDC;
	int i;
	strncpy (linebuf[line], msg, 64); linebuf[line][63] = '\0';
	m_pFramework->GetBackBuffer()->GetDC (&hDC);
	SetTextColor (hDC, LOADSTATUSCOL);
	SetBkMode (hDC, TRANSPARENT);
	SetTextAlign (hDC, TA_RIGHT);
	BitBlt (hDC, lstatus.x, lstatus.y, lstatus.w, lstatus.h, lstatus.bkgDC, 0, 0, SRCCOPY);
	for (i = 0; i < 2; i++) {
		if (linebuf[i][0]) 
			TextOut (hDC, lstatus.x+lstatus.w, lstatus.y+i*lstatus.h/2, linebuf[i], strlen (linebuf[i]));
	}
	m_pFramework->GetBackBuffer()->ReleaseDC (hDC);
	clbkDisplayFrame ();
	return true;
#endif
	return false;
}

// ==============================================================

void D3D7Client::Output2DOverlay ()
{
	// Write out the orbiter debug string
	const char *msg = oapiDebugString();
	if (msg[0]) {
		HDC hDC;
		pddsRenderTarget->GetDC (&hDC);
		ExtTextOut (hDC, 0, viewH-16, 0, NULL, msg, strlen(msg), NULL);
		pddsRenderTarget->ReleaseDC (hDC);
	}
}

// ==============================================================

D3D7Enum_DeviceInfo *D3D7Client::PickDevice (DeviceId *id)
{
	D3D7Enum_DeviceInfo *devlist, *dv;
	DWORD ndev;
	D3D7Enum_GetDevices (&devlist, &ndev);

	if (!id || id->dwDevice < 0 || id->dwDevice >= ndev)
		return NULL;
	dv = devlist + id->dwDevice;

	if (id->dwMode >= 0 && id->dwMode < dv->dwNumModes)
		dv->dwCurrentMode = id->dwMode;
	else
		dv->dwCurrentMode = 0;
	dv->ddsdFullscreenMode = dv->pddsdModes[dv->dwCurrentMode];

	if (!id->bFullscreen && dv->bDesktopCompatible)
		dv->bWindowed = TRUE;
	else
		dv->bWindowed = FALSE;

	if (!id->bStereo && dv->bStereoCompatible)
		dv->bStereo = TRUE;
	else
		dv->bStereo = FALSE;

	return dv;
}

// ==============================================================
// copy video options from the video tab

void D3D7Client::clbkRefreshVideoData ()
{
	if (vtab)
		vtab->UpdateConfigData();
}

// ==============================================================
// Fullscreen mode flag

bool D3D7Client::clbkFullscreenMode () const
{
	return bFullscreen;
}

// ==============================================================
// return the dimensions of the render viewport

void D3D7Client::clbkGetViewportSize (DWORD *width, DWORD *height) const
{
	*width = viewW, *height = viewH;
}

// ==============================================================
// Returns a specific render parameter

bool D3D7Client::clbkGetRenderParam (DWORD prm, DWORD *value) const
{
	switch (prm) {
	case RP_COLOURDEPTH:
		*value = viewBPP;
		return true;
	case RP_ZBUFFERDEPTH:
		* value = GetFramework()->GetZBufferBitDepth();
		return true;
	case RP_STENCILDEPTH:
		*value = GetFramework()->GetStencilBitDepth();
		return true;
	case RP_MAXLIGHTS:
		*value = GetFramework()->GetMaxLights();
		return true;
	case RP_ISTLDEVICE:
		*value = GetFramework()->IsTLDevice();
		return true;
	case RP_REQUIRETEXPOW2:
		*value = GetFramework()->RequireTexPow2();
		return true;
	}
	return false;
}

// ==============================================================
// Responds to visual events

int D3D7Client::clbkVisEvent (OBJHANDLE hObj, VISHANDLE vis, DWORD msg, DWORD_PTR context)
{
	VisObject *vo = (VisObject*)vis;
	vo->clbkEvent (msg, context);
	return 1;
}

// ==============================================================
// Returns a mesh for a visual

MESHHANDLE D3D7Client::clbkGetMesh (VISHANDLE vis, UINT idx)
{
	return (vis ? ((vObject*)vis)->GetMesh (idx) : NULL);
}

// =======================================================================

int D3D7Client::clbkGetMeshGroup (DEVMESHHANDLE hMesh, DWORD grpidx, GROUPREQUESTSPEC *grs)
{
	return ((D3D7Mesh*)hMesh)->GetGroup (grpidx, grs);
}

// =======================================================================

int D3D7Client::clbkEditMeshGroup (DEVMESHHANDLE hMesh, DWORD grpidx, GROUPEDITSPEC *ges)
{
	return ((D3D7Mesh*)hMesh)->EditGroup (grpidx, ges);
}

// ==============================================================
// Message handler for render window

LRESULT D3D7Client::RenderWndProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg) {
    case WM_MOVE:
        // If in windowed mode, move the Framework's window
        if (m_pFramework && m_pDeviceInfo->bWindowed)
            m_pFramework->Move ((SHORT)LOWORD(lParam), (SHORT)HIWORD(lParam));
        break;

	case WM_SYSCOMMAND:
		switch (wParam) {
		case SC_KEYMENU:
			// trap Alt system keys
			return 1;
		case SC_MOVE:
		case SC_SIZE:
		case SC_MAXIMIZE:
		case SC_MONITORPOWER:
			// Prevent moving/sizing and power loss in fullscreen mode
			if (bFullscreen)
				return 1;
			break;
		}
		break;

	case WM_SYSKEYUP:
		if (bFullscreen)
			return 0;  // trap Alt-key

	}
	return GraphicsClient::RenderWndProc (hWnd, uMsg, wParam, lParam);
}

// ==============================================================
// Message handler for Launchpad "video" tab

INT_PTR D3D7Client::LaunchpadVideoWndProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	if (vtab)
		return vtab->WndProc (hWnd, uMsg, wParam, lParam);
	else
		return FALSE;
}

// ==============================================================

void D3D7Client::LogRenderParams () const
{
	LOGOUT ("Viewport: %s %d x %d x %d",
		bFullscreen ? "Fullscreen":"Window", viewW, viewH, viewBPP);
	LOGOUT ("Hardware T&L capability: %s", GetFramework()->IsTLDevice() ? "Yes":"No");
	if (GetFramework()->GetZBufferBitDepth())
		LOGOUT ("Z-buffer depth: %d bit", GetFramework()->GetZBufferBitDepth());
	if (GetFramework()->GetStencilBitDepth())
		LOGOUT ("Stencil buffer depth: %d bit", GetFramework()->GetStencilBitDepth());
	if (GetFramework()->GetMaxLights())
		LOGOUT ("Active lights supported: %d", GetFramework()->GetMaxLights());
}

// ==============================================================

#ifdef UNDEF
void D3D7Client::WriteLog (const char *msg, ...) const
{
	char cbuf[256] = "D3D7Client: ";
	va_list ap;
	va_start (ap, msg);
	vsnprintf (cbuf+12, 255-12, msg, ap);
	va_end (ap);
	oapiWriteLog (cbuf);
}
#endif

// =======================================================================

void D3D7Client::clbkRender2DPanel (SURFHANDLE *hSurf, MESHHANDLE hMesh, MATRIX3 *T, bool transparent)
{
	static DWORD nHVTX = 1;
	static struct HVTX {
		float x, y, z;
		float rhw;
		float tu, tv;
	} *hvtx = new HVTX[1];

	DWORD vtxFmt = D3DFVF_XYZRHW | D3DFVF_TEX1 | D3DFVF_TEXCOORDSIZE2(0);
	DWORD dAlpha;
	m_pD3DDevice->GetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, &dAlpha);
	m_pD3DDevice->SetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, TRUE);
	m_pD3DDevice->SetTextureStageState (0, D3DTSS_ADDRESS, D3DTADDRESS_CLAMP);
	if (transparent)
		m_pD3DDevice->SetRenderState (D3DRENDERSTATE_DESTBLEND, D3DBLEND_ONE);
	float rhw = 1;
	DWORD i, j, nvtx, ngrp = oapiMeshGroupCount (hMesh);
	SURFHANDLE surf = 0, newsurf;

	float scalex = (float)(T->m11),  dx = (float)(T->m13);
	float scaley = (float)(T->m22),  dy = (float)(T->m23);

	for (i = 0; i < ngrp; i++) {
		MESHGROUP *grp = oapiMeshGroup (hMesh, i);
		if (grp->UsrFlag & 2) continue; // skip this group

		if (grp->TexIdx == SPEC_DEFAULT) {
			newsurf = 0;
		} else if (grp->TexIdx == SPEC_INHERIT) {
			// nothing to do
		} else if (grp->TexIdx >= TEXIDX_MFD0) {
			int mfdidx = grp->TexIdx-TEXIDX_MFD0;
			newsurf = GetMFDSurface (mfdidx);
			if (!newsurf) continue;
		} else if (hSurf) {
			newsurf = hSurf[grp->TexIdx];
		} else {
			newsurf = oapiGetTextureHandle (hMesh, grp->TexIdx+1);
		}
		if (newsurf != surf) {
			m_pD3DDevice->SetTexture (0, (LPDIRECTDRAWSURFACE7)(surf = newsurf));
		}

		nvtx = grp->nVtx;
		if (nvtx > nHVTX) { // increase buffer size
			delete []hvtx;
			hvtx = new HVTX[nHVTX = nvtx];
			for (j = 0; j < nvtx; j++) {
				hvtx[j].z = 0;
				hvtx[j].rhw = rhw;
			}
		}
		for (j = 0; j < nvtx; j++) {
			HVTX *tgtvtx = hvtx+j;
			NTVERTEX *srcvtx = grp->Vtx+j;
			tgtvtx->x = srcvtx->x*scalex + dx;
			tgtvtx->y = srcvtx->y*scaley + dy;
			tgtvtx->tu = srcvtx->tu;
			tgtvtx->tv = srcvtx->tv;
		}
		m_pD3DDevice->DrawIndexedPrimitive (D3DPT_TRIANGLELIST, vtxFmt, hvtx, nvtx, grp->Idx, grp->nIdx, 0);
	}
	
	m_pD3DDevice->SetTextureStageState (0, D3DTSS_ADDRESS, D3DTADDRESS_WRAP);
	if (transparent)
		m_pD3DDevice->SetRenderState (D3DRENDERSTATE_DESTBLEND, D3DBLEND_INVSRCALPHA);
	if (dAlpha != TRUE)
		m_pD3DDevice->SetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, dAlpha);
}

// =======================================================================

void D3D7Client::clbkRender2DPanel (SURFHANDLE *hSurf, MESHHANDLE hMesh, MATRIX3 *T, float alpha, bool additive)
{
	bool reset = false;
	DWORD alphaop, alphaarg2, tfactor;
	if (alpha < 1.0f) {
		m_pD3DDevice->GetTextureStageState (0, D3DTSS_ALPHAOP, &alphaop);
		m_pD3DDevice->GetTextureStageState (0, D3DTSS_ALPHAARG2, &alphaarg2);
		m_pD3DDevice->GetRenderState (D3DRENDERSTATE_TEXTUREFACTOR, &tfactor);
		m_pD3DDevice->SetTextureStageState (0, D3DTSS_ALPHAOP, D3DTOP_MODULATE);
		m_pD3DDevice->SetTextureStageState (0, D3DTSS_ALPHAARG2, D3DTA_TFACTOR);
		m_pD3DDevice->SetRenderState (D3DRENDERSTATE_TEXTUREFACTOR, D3DRGBA(1,1,1,alpha));
		reset = true;
	}
	clbkRender2DPanel (hSurf, hMesh, T, additive);
	if (reset) {
		m_pD3DDevice->SetTextureStageState (0, D3DTSS_ALPHAOP, alphaop);
		m_pD3DDevice->SetTextureStageState (0, D3DTSS_ALPHAARG2, alphaarg2);
		m_pD3DDevice->SetRenderState (D3DRENDERSTATE_TEXTUREFACTOR, tfactor);
	}
}


// =======================================================================
// Surface functions
// =======================================================================

SURFHANDLE D3D7Client::clbkCreateSurface (DWORD w, DWORD h, SURFHANDLE hTemplate)
{
	HRESULT hr;
	LPDIRECTDRAWSURFACE7 surf;
	DDSURFACEDESC2 ddsd;
    ZeroMemory (&ddsd, sizeof(ddsd));
	ddsd.dwSize = sizeof(ddsd);
    ddsd.dwFlags = DDSD_CAPS | DDSD_HEIGHT | DDSD_WIDTH; 
    ddsd.ddsCaps.dwCaps = DDSCAPS_OFFSCREENPLAIN;
    ddsd.dwWidth  = w;
	ddsd.dwHeight = h;
	if (hTemplate) { // use pixel format information from template surface
		ddsd.ddpfPixelFormat.dwSize = sizeof (DDPIXELFORMAT);
		ddsd.ddpfPixelFormat.dwFlags = 0;
		((LPDIRECTDRAWSURFACE7)hTemplate)->GetPixelFormat (&ddsd.ddpfPixelFormat);
		ddsd.dwFlags |= DDSD_PIXELFORMAT;
	}
	if ((hr = m_pDD->CreateSurface (&ddsd, &surf, NULL)) != DD_OK) {
		LOGOUT_DDERR (hr);
		return NULL;
	}
	return (SURFHANDLE)surf;
}

SURFHANDLE D3D7Client::clbkCreateSurfaceEx (DWORD w, DWORD h, DWORD attrib)
{
	HRESULT hr;
	LPDIRECTDRAWSURFACE7 surf;
	DDSURFACEDESC2 ddsd;
    ZeroMemory (&ddsd, sizeof(ddsd));
	ddsd.dwSize = sizeof(ddsd);
	ddsd.ddpfPixelFormat.dwSize = sizeof (DDPIXELFORMAT);
    ddsd.dwWidth  = w;
	ddsd.dwHeight = h;
    ddsd.dwFlags = DDSD_CAPS | DDSD_HEIGHT | DDSD_WIDTH; 
	if (attrib & OAPISURFACE_TEXTURE)
		ddsd.ddsCaps.dwCaps |= DDSCAPS_TEXTURE;
	else
		ddsd.ddsCaps.dwCaps |= DDSCAPS_OFFSCREENPLAIN;
	if (attrib & OAPISURFACE_SYSMEM || (attrib & OAPISURFACE_GDI || attrib & OAPISURFACE_SKETCHPAD) && !(attrib & OAPISURFACE_TEXTURE)) 
		ddsd.ddsCaps.dwCaps |= DDSCAPS_SYSTEMMEMORY;
	else
		ddsd.ddsCaps.dwCaps |= DDSCAPS_VIDEOMEMORY;
	if ((attrib & OAPISURFACE_ALPHA) && !(attrib & (OAPISURFACE_GDI | OAPISURFACE_SKETCHPAD)))
		ddsd.ddpfPixelFormat.dwFlags |=  DDPF_ALPHAPIXELS; // enable alpha channel
	if ((hr = m_pDD->CreateSurface (&ddsd, &surf, NULL)) != DD_OK) {
		LOGOUT_DDERR (hr);
		return NULL;
	}
	return (SURFHANDLE)surf;
}

SURFHANDLE D3D7Client::clbkCreateTexture (DWORD w, DWORD h)
{
	LPDIRECTDRAWSURFACE7 surf;
	DDSURFACEDESC2 ddsd;
    ZeroMemory (&ddsd, sizeof(ddsd));
	ddsd.dwSize = sizeof(ddsd);
	ddsd.dwFlags = DDSD_WIDTH | DDSD_HEIGHT | DDSD_PIXELFORMAT | DDSD_CAPS /*| DDSD_CKSRCBLT*/;
	ddsd.dwWidth = w;
	ddsd.dwHeight = h;
	ddsd.ddsCaps.dwCaps = DDSCAPS_TEXTURE;
	ddsd.ddpfPixelFormat.dwSize = sizeof (DDPIXELFORMAT);
	ddsd.ddpfPixelFormat.dwFlags = 0;
	//ddsd.ddckCKSrcBlt.dwColorSpaceLowValue = ddsd.ddckCKSrcBlt.dwColorSpaceLowValue = 0;
	GetFramework()->GetBackBuffer()->GetPixelFormat (&ddsd.ddpfPixelFormat);
	// take pixel format from render surface (should make it compatible)
	ddsd.ddpfPixelFormat.dwFlags &= ~DDPF_ALPHAPIXELS; // turn off alpha data
	m_pDD->CreateSurface (&ddsd, &surf, NULL);
	return surf;
}

void D3D7Client::clbkIncrSurfaceRef (SURFHANDLE surf)
{
	((LPDIRECTDRAWSURFACE7)surf)->AddRef();
}

bool D3D7Client::clbkReleaseSurface (SURFHANDLE surf)
{
	if (surf) {
		((LPDIRECTDRAWSURFACE7)surf)->Release();
		return true;
	} else
		return false;
}

bool D3D7Client::clbkGetSurfaceSize (SURFHANDLE surf, DWORD *w, DWORD *h)
{
    DDSURFACEDESC2 ddsd;
    ddsd.dwSize = sizeof(ddsd);
    ddsd.dwFlags = DDSD_HEIGHT | DDSD_WIDTH;
    ((LPDIRECTDRAWSURFACE7)surf)->GetSurfaceDesc(&ddsd);
	*w = ddsd.dwWidth;
	*h = ddsd.dwHeight;
	return true;
}

bool D3D7Client::clbkSetSurfaceColourKey (SURFHANDLE surf, DWORD ckey)
{
	DDCOLORKEY ck = {ckey,ckey};
	((LPDIRECTDRAWSURFACE7)surf)->SetColorKey (DDCKEY_SRCBLT, &ck);
	return true;
}

DWORD D3D7Client::clbkGetDeviceColour (BYTE r, BYTE g, BYTE b)
{
	if (viewBPP >= 24) return ((DWORD)r << 16) + ((DWORD)g << 8) + (DWORD)b;
	else               return ((((DWORD)r*319)/2559) << 11) + ((((DWORD)g*639)/2559) << 5) + (((DWORD)b*319)/2559);
}

// =======================================================================
// Blitting functions
// =======================================================================

bool D3D7Client::clbkBlt (SURFHANDLE tgt, DWORD tgtx, DWORD tgty, SURFHANDLE src, DWORD flag) const
{
	LPDIRECTDRAWSURFACE7 ps_tgt = (tgt ? (LPDIRECTDRAWSURFACE7)tgt : pddsRenderTarget);
	LPDIRECTDRAWSURFACE7 ps_src = (LPDIRECTDRAWSURFACE7)src;
	DWORD bltflag = DDBLTFAST_WAIT;
	if (flag & BLT_SRCCOLORKEY) bltflag |= DDBLTFAST_SRCCOLORKEY;
	if (flag & BLT_TGTCOLORKEY) bltflag |= DDBLTFAST_DESTCOLORKEY;
	HRESULT hr;
	if ((hr = ps_tgt->BltFast (tgtx, tgty, ps_src, NULL, bltflag)) != S_OK) {
		DDSURFACEDESC2 ddsd;
		memset (&ddsd, 0, sizeof(DDSURFACEDESC2));
		ddsd.dwSize = sizeof(DDSURFACEDESC2);
		ddsd.dwFlags = DDSD_HEIGHT | DDSD_WIDTH;
		ps_src->GetSurfaceDesc (&ddsd);
		bltflag = DDBLT_WAIT;
		if (flag & BLT_SRCCOLORKEY) bltflag |= DDBLT_KEYSRC;
		if (flag & BLT_TGTCOLORKEY) bltflag |= DDBLT_KEYDEST;
		RECT dstrct = {(LONG)tgtx, (LONG)tgty, (LONG)(tgtx+ddsd.dwWidth), LONG(tgty+ddsd.dwHeight)};
		hr = ps_tgt->Blt (&dstrct, ps_src, NULL, bltflag, NULL);
		if (hr != S_OK)
			LOGOUT_DDERR (hr);
	}
	return (hr == S_OK);
}

bool D3D7Client::clbkBlt (SURFHANDLE tgt, DWORD tgtx, DWORD tgty, SURFHANDLE src, DWORD srcx, DWORD srcy, DWORD w, DWORD h, DWORD flag) const
{
	RECT srcr = {(LONG)srcx, (LONG)srcy, (LONG)(srcx+w), (LONG)(srcy+h)};
	LPDIRECTDRAWSURFACE7 ps_tgt = (tgt ? (LPDIRECTDRAWSURFACE7)tgt : pddsRenderTarget);
	LPDIRECTDRAWSURFACE7 ps_src = (LPDIRECTDRAWSURFACE7)src;
	DWORD bltflag = DDBLTFAST_WAIT;
	if (flag & BLT_SRCCOLORKEY) bltflag |= DDBLTFAST_SRCCOLORKEY;
	if (flag & BLT_TGTCOLORKEY) bltflag |= DDBLTFAST_DESTCOLORKEY;
	HRESULT hr;
	if ((hr = ps_tgt->BltFast (tgtx, tgty, ps_src, &srcr, bltflag)) != S_OK) {
		bltflag = DDBLT_WAIT;
		if (flag & BLT_SRCCOLORKEY) bltflag |= DDBLT_KEYSRC;
		if (flag & BLT_TGTCOLORKEY) bltflag |= DDBLT_KEYDEST;
		RECT dstrct = {(LONG)tgtx, (LONG)tgty, (LONG)(tgtx+w), (LONG)(tgty+h)};
		RECT srcrct = {(LONG)srcx, (LONG)srcy, (LONG)(srcx+w), (LONG)(srcy+h)};
		hr = ps_tgt->Blt (&dstrct, ps_src, &srcrct, bltflag, NULL);
		if (hr != S_OK)
			LOGOUT_DDERR (hr);
	}
	return (hr == S_OK);
}

bool D3D7Client::clbkScaleBlt (SURFHANDLE tgt, DWORD tgtx, DWORD tgty, DWORD tgtw, DWORD tgth,
		                       SURFHANDLE src, DWORD srcx, DWORD srcy, DWORD srcw, DWORD srch, DWORD flag) const
{
	LPDIRECTDRAWSURFACE7 ps_tgt = (tgt ? (LPDIRECTDRAWSURFACE7)tgt : pddsRenderTarget);
	LPDIRECTDRAWSURFACE7 ps_src = (LPDIRECTDRAWSURFACE7)src;
	RECT srcr = {(LONG)srcx, (LONG)srcy, (LONG)(srcx+srcw), (LONG)(srcy+srch)};
	RECT tgtr = {(LONG)tgtx, (LONG)tgty, (LONG)(tgtx+tgtw), (LONG)(tgty+tgth)};
	DWORD bltflag = DDBLT_WAIT;
	if (flag & BLT_SRCCOLORKEY) bltflag |= DDBLT_KEYSRC;
	if (flag & BLT_TGTCOLORKEY) bltflag |= DDBLT_KEYDEST;
	return (ps_tgt->Blt (&tgtr, ps_src, &srcr, bltflag, 0) == S_OK);
}

bool D3D7Client::clbkFillSurface (SURFHANDLE surf, DWORD col) const
{
	DDBLTFX bltfx;
	ZeroMemory (&bltfx, sizeof(bltfx));
	bltfx.dwSize = sizeof(bltfx);
	bltfx.dwFillColor = col;
	return ((LPDIRECTDRAWSURFACE7)surf)->Blt (NULL, NULL, NULL, DDBLT_COLORFILL, &bltfx) == DD_OK;
}

bool D3D7Client::clbkFillSurface (SURFHANDLE surf, DWORD tgtx, DWORD tgty, DWORD w, DWORD h, DWORD col) const
{
	RECT r = {(LONG)tgtx, (LONG)tgty, (LONG)(tgtx+w), (LONG)(tgty+h)};
	DDBLTFX bltfx;
	ZeroMemory (&bltfx, sizeof(bltfx));
	bltfx.dwSize = sizeof(bltfx);
	bltfx.dwFillColor = col;
	return ((LPDIRECTDRAWSURFACE7)surf)->Blt (&r, NULL, NULL, DDBLT_COLORFILL, &bltfx) == DD_OK;
}

// =======================================================================
// GDI functions
// =======================================================================

HDC D3D7Client::clbkGetSurfaceDC (SURFHANDLE surf)
{
	HDC hDC;
	LPDIRECTDRAWSURFACE7 ps = (surf ? (LPDIRECTDRAWSURFACE7)surf : pddsRenderTarget);
	ps->GetDC (&hDC);
	return hDC;
}

void D3D7Client::clbkReleaseSurfaceDC (SURFHANDLE surf, HDC hDC)
{
	LPDIRECTDRAWSURFACE7 ps = (surf ? (LPDIRECTDRAWSURFACE7)surf : pddsRenderTarget);
	ps->ReleaseDC (hDC);
}


// ======================================================================
// ======================================================================
// class VisObject

VisObject::VisObject (OBJHANDLE hObj)
{
	hObject = hObj;
}

VisObject::~VisObject ()
{
}

//-----------------------------------------------------------------------------
// Name: ConfirmDevice()
// Desc: Allow application to reject 3D devices during enumeration
//-----------------------------------------------------------------------------
HRESULT clbkConfirmDevice (DDCAPS*, D3DDEVICEDESC7*)
{
	// put checks in here if desired - currently we admit all devices
	return S_OK;
}
