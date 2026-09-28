// ==============================================================
// D3D9Client.cpp
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2006-2026 Martin Schweiger
//				 2012-2016 Jarmo Nikkanen
// ==============================================================


// STRICT left out: Win32 build switch
#define ORBITER_MODULE

#include <set> // ...for Brush-, Pen- and Font-accounting
#include "Orbitersdk.h"
#include "D3D9Client.h"
#include "D3D9Config.h"
#include "D3D9Util.h"
#include "D3D9Catalog.h"
#include "D3D9Surface.h"
#include "D3D9TextMgr.h"
#include "D3D9Frame.h"
#include "D3D9Pad.h"
#include "CSphereMgr.h"
#include "Scene.h"
#include "Mesh.h"
#include "VVessel.h"
#include "VStar.h"
#include "MeshMgr.h"
#include "Particle.h"
#include "TileMgr.h"
#include "RingMgr.h"
#include "HazeMgr.h"
#include "Log.h"
#include "VideoTab.h"
#include "GDIPad.h"
#include "OapiExtension.h"
#include "DebugControls.h"
#include "Surfmgr2.h"
#include "gcCore.h"
#include "gcConst.h"
#include <unordered_map>
// d3d9on12.h left out: D3D9on12 is a Windows Direct3D 12 layer
#include "imgui.h"
#include "imgui_impl_vulkan.h" // imgui_impl_dx9.h
// imgui_impl_win32.h left out: the core's imgui_impl_qt is the platform backend
#include "OrbiterResource.h"
#include "VkTexFile.h"
#include <QVulkanInstance>
#include <QGuiApplication>
#include <QVersionNumber>
#include <QWindow>
#include <QWidget>
#include <QScreen>
#include <QMouseEvent>
#include <QKeyEvent>
#include <QWheelEvent>
#include <QPainter>
#include <QImage>
#include <QFont>
#include <QFontMetrics>
#include <QClipboard>
#include <QDBusInterface>
#include <QDBusReply>
#include <QGuiApplication>
#include <sys/stat.h>
#include <thread>
#include <chrono>
#include <mutex>

#if defined(_MSC_VER) && (_MSC_VER <= 1700 ) // Microsoft Visual Studio Version 2012 and lower
#define round(v) floor(v+0.5)
#endif

// ==============================================================
// Structure definitions

struct D3D9Client::RenderProcData {
	__gcRenderProc proc;
	void *pParam;
	DWORD id;
};

struct D3D9Client::GenericProcData {
	__gcGenericProc proc;
	void *pParam;
	DWORD id;
};

using namespace oapi;

void *g_hInst = 0;
D3D9Client *g_client = 0;
class gcConst* g_pConst = 0;
QVulkanInstance* g_pD3DObject = 0;  // Made valid when VideoTab is created
Memgr<float>* g_pMemgr_f = nullptr;
Memgr<INT16>* g_pMemgr_i = nullptr;
Memgr<UINT8>* g_pMemgr_u = nullptr;
Memgr<WORD>* g_pMemgr_w = nullptr;
Memgr<VERTEX_2TEX>* g_pMemgr_vtx = nullptr;
Texmgr<VkTex*>* g_pTexmgr_tt = nullptr;
Vtxmgr<VkBuf*>* g_pVtxmgr_vb = nullptr;
Idxmgr<VkBuf*>* g_pIdxmgr_ib = nullptr;

// __Direct3DCreate9On12 left out: D3D9on12 is a Windows Direct3D 12 layer

set<D3D9Mesh*> MeshCatalog;
set<SurfNative*> SurfaceCatalog;
unordered_map<string, SURFHANDLE> SharedTextures;
unordered_map<string, SURFHANDLE> ClonedTextures;
unordered_map<MESHHANDLE, class SketchMesh*> MeshMap;
unordered_map<std::string, VkTex*> MicroTextures;

DWORD uCurrentMesh = 0;
vObject *pCurrentVisual = 0;
_D3D9Stats D3D9Stats;

#ifdef _NVAPI_H
 StereoHandle pStereoHandle = 0;
#endif

bool bFreeze = false;
bool bFreezeEnable = false;
bool bFreezeRenderAll = false;

// Debuging Brush-, Pen- and Font-accounting
std::set<Font *> g_fonts;
std::set<Pen *> g_pens;
std::set<Brush *> g_brushes;

extern list<gcGUIApp *> g_gcGUIAppList;

// NvOptimusEnablement export left out: a Windows Optimus driver hint (the GPU is the Vulkan adapter picked in the video tab)

// not upstream: the framework's ID3DXFont pLargeFont (record/replay/frozen labels), a Sketchpad font here
static oapi::Font *pLargeFont = NULL;

// not upstream: the CPU copy behind the GDI overlay DC (the D3D9 surface kept it)
static QImage GDIImage;

// not upstream: ImGui descriptor sets of this frame's surfaces (D3D9 passed the texture pointer), and a guard for their release
static DWORD ImGuiGeneration = 0; // not upstream: descriptor sets of an ImGui backend that was shut down are gone with its pool
static void ReleaseImSet (uint64_t s, DWORD gen) { if (gen == ImGuiGeneration) ImGui_ImplVulkan_RemoveTexture ((VkDescriptorSet)s); }

// not upstream: CreateFont for the client's own GDI fonts (a positive height is the cell height, as in GDI)
static QFont *CreateGDIFont(int height, int weight, const char *face)
{
	QFont *hF = new QFont(QString::fromLatin1(face));
	hF->setStyleHint(QFont::TypeWriter); // FF_MODERN when the face is missing
	hF->setWeight(QFont::Weight(weight));
	hF->setPixelSize(height);
	int cell = QFontMetrics(*hF).height();
	if (cell > height) hF->setPixelSize(std::max(1, height * height / cell));
	return hF;
}

// not upstream: GDI TextOut with TA_LEFT|TA_TOP (QPainter puts text on the baseline)
static void TextOut(QPainter *hDC, int x, int y, const char *str, int len)
{
	hDC->drawText(x, y + hDC->fontMetrics().ascent(), QString::fromLatin1(str, len));
}

// not upstream: IDirect3DSurface9::GetDC for the client's own surfaces, a QPainter on a CPU copy of the image
static QPainter *SurfaceDC(VkDev *pDev, VkSurf *pSrf, QImage &img)
{
	VkPixels px, lv, cv;
	if (!pSrf || !VkReadPixels(pDev, pSrf->tex, px, pSrf->level + 1)) return NULL;
	lv.w = pSrf->w; lv.h = pSrf->h; lv.levels = 1; lv.layers = 1; lv.fmt = px.fmt; lv.swz = SWZ_NONE;
	lv.data.assign(1, px.Level(pSrf->level, pSrf->layer));
	if (!VkConvertPixels(lv, cv, VK_FORMAT_B8G8R8A8_UNORM, SWZ_NONE, lv.w, lv.h, 1)) return NULL;
	img = QImage(lv.w, lv.h, QImage::Format_RGB32); // X8R8G8B8
	for (UINT y = 0; y < lv.h; y++) {
		QRgb *p = (QRgb *)img.scanLine(y);
		memcpy(p, &cv.Level(0)[(size_t)y * lv.w * 4], (size_t)lv.w * 4);
		for (UINT x = 0; x < lv.w; x++) p[x] |= 0xFF000000;
	}
	return new QPainter(&img);
}

// not upstream: IDirect3DSurface9::ReleaseDC for SurfaceDC, ends the painter and uploads the copy
static void SurfaceReleaseDC(VkSurf *pSrf, QPainter *hDC, QImage &img)
{
	if (!hDC) return;
	hDC->end();
	delete hDC;
	VkPixels px;
	px.w = img.width(); px.h = img.height(); px.levels = 1; px.layers = 1;
	px.fmt = VK_FORMAT_B8G8R8A8_UNORM;
	px.data.assign(1, std::vector<BYTE>((size_t)px.w * px.h * 4));
	for (UINT y = 0; y < px.h; y++) memcpy(&px.data[0][(size_t)y * px.w * 4], img.constScanLine(y), (size_t)px.w * 4);
	if (pSrf) VkLoadTextureLevel(pSrf->tex, pSrf->level, pSrf->layer, px);
	img = QImage();
}

// not upstream: D3DXLoadSurfaceFrom* into a destination rectangle (D3DX_FILTER_LINEAR), through a blit
static bool LoadSurfaceRect(VkDev *pDev, VkSurf *pDst, const RECT *pRect, const VkPixels &px)
{
	VkPixels cv;
	if (!VkConvertPixels(px, cv, VK_FORMAT_B8G8R8A8_UNORM, SWZ_NONE, 0, 0, 1)) return false;
	VkTex *pImg = VkCreateTexture(pDev, cv, VK_IMAGE_USAGE_SAMPLED_BIT | VK_IMAGE_USAGE_TRANSFER_SRC_BIT);
	if (!pImg) return false;
	{
		VkSurf src(pImg);
		pDev->StretchRect(&src, NULL, pDst, pRect, VK_FILTER_LINEAR);
	}
	delete pImg; // freed once the frame is done
	return true;
}

// not upstream: IDirect3DDevice9::GetAvailableTextureMem, the free device-local memory in VMA's heap budgets
static VkDeviceSize GetAvailableTextureMem(VkDev *pDev)
{
	const VkPhysicalDeviceMemoryProperties *mp;
	VmaBudget budget[VK_MAX_MEMORY_HEAPS];
	vmaGetMemoryProperties(pDev->vma, &mp);
	vmaGetHeapBudgets(pDev->vma, budget);
	VkDeviceSize avail = 0;
	for (UINT i = 0; i < mp->memoryHeapCount; i++)
		if ((mp->memoryHeaps[i].flags & VK_MEMORY_HEAP_DEVICE_LOCAL_BIT) && budget[i].budget > budget[i].usage) avail += budget[i].budget - budget[i].usage;
	return avail;
}

// not upstream: the large font's ID3DXFont::DrawText (DT_CENTER | DT_TOP), through a Sketchpad on the backbuffer
static void DrawLargeText(const char *str, int len, const RECT *r, DWORD color)
{
	oapi::Sketchpad *pSkp = g_client->clbkGetSketchpad(NULL); // RENDERTGT_MAINWINDOW (oapiGetSketchpad refuses NULL)
	if (!pSkp) return;
	pSkp->SetFont(pLargeFont);
	pSkp->SetTextColor(RGB((color >> 16) & 0xFF, (color >> 8) & 0xFF, color & 0xFF)); // D3DCOLOR to COLORREF
	pSkp->SetBackgroundMode(oapi::Sketchpad::BK_TRANSPARENT);
	pSkp->SetTextAlign(oapi::Sketchpad::CENTER, oapi::Sketchpad::TOP);
	pSkp->Text((r->left + r->right) / 2, r->top, str, len);
	g_client->clbkReleaseSketchpad(pSkp);
}

// ==============================================================
// API interface
// ==============================================================

// ==============================================================
// Initialise module

DLLCLBK void InitModule(void *hDLL)
{

#ifdef _DEBUG
	// _CrtSetDbgFlag left out: MSVC debug heap (the ORBITER_SANITIZER build checks leaks)
	// _CrtSetBreakAlloc(8351);

	assert(sizeof(FVECTOR4) == 16);
	assert(sizeof(FVECTOR4::data) == 16);

	assert(sizeof(FVECTOR4::rgb) == 12);
	assert(sizeof(FVECTOR4::xyz) == 12);

	auto dut = FVECTOR4(1.2, 3.4, 5.6, 7.8);
	assert(dut.data[0] == dut.r);
	assert(dut.data[1] == dut.g);
	assert(dut.data[2] == dut.b);
	assert(dut.data[3] == dut.a);

	assert(dut.data[0] == dut.x);
	assert(dut.data[1] == dut.y);
	assert(dut.data[2] == dut.z);
	assert(dut.data[3] == dut.w);

	assert(dut.rgb.x == dut.xyz.x);
	assert(dut.rgb.y == dut.xyz.y);
	assert(dut.rgb.z == dut.xyz.z);

	assert(dut.a == dut.w);

	// Check that 'a' and 'w' are placed behind 'rgb' rsp. 'xyz' and unaffected
	dut.rgb = 0;
	assert(dut.a == dut.w);
#endif

	D3D9InitLog("Modules/VulkanClient/D3D9ClientLog.html");

	g_pMemgr_f = new Memgr<float>("float");
	g_pMemgr_i = new Memgr<INT16>("UINT16");
	g_pMemgr_u = new Memgr<UINT8>("UINT8");
	g_pMemgr_w = new Memgr<WORD>("WORD");
	g_pMemgr_vtx = new Memgr<VERTEX_2TEX>("VERTEX_2TEX");

	// D3DXCheckVersion left out: no D3DX runtime to check (the Vulkan version is checked when the device is made)

#ifdef _NVAPI_H
	if (NvAPI_Initialize()==NVAPI_OK) {
		LogAlw("[nVidia API Initialized]");
		bNVAPI = true;
	}
	else LogAlw("[nVidia API Not Available]");
#else
	LogAlw("[Not Compiled With nVidia API]");
#endif

	Config = new D3D9Config();

	if (Config->ShaderCacheUse) {
		struct stat fa; // GetFileAttributesA, CreateDirectoryA
		if (stat("Cache", &fa) != 0) mkdir("Cache", 0755);
		if (stat("Cache/VulkanClient", &fa) != 0) mkdir("Cache/VulkanClient", 0755);
		if (stat("Cache/VulkanClient/Shaders", &fa) != 0) mkdir("Cache/VulkanClient/Shaders", 0755);
	}

	g_pConst = new gcConst();

	DebugControls::Create();
	AtmoControls::Create();
	vPlanet::ParseMicroTexturesFile();

	D3D9Stats.TilesRendered[RENDERPASS_MAINSCENE] = 0;
	D3D9Stats.TilesRendered[RENDERPASS_CUSTOMCAM] = 0;
	D3D9Stats.TilesRendered[RENDERPASS_ENVCAM] = 0;

	g_hInst = hDLL;
	g_client = new D3D9Client(hDLL);

	if (oapiRegisterGraphicsClient(g_client)==false) {
		delete g_client;
		g_client = 0;
	}
}

// ==============================================================
// Clean up module

DLLCLBK void ExitModule(void *hDLL)
{
	LogAlw("--------------ExitModule------------");

	delete Config;
	delete g_pConst;

	if (g_client) {
		oapiUnregisterGraphicsClient(g_client);
		delete g_client;
		g_client = 0;
	}

	DebugControls::Release();
	AtmoControls::Release();

#ifdef _NVAPI_H
	if (bNVAPI) if (NvAPI_Unload()==NVAPI_OK) LogAlw("[nVidia API Unloaded]");
#endif

	LogAlw("Log Closed");
	D3D9CloseLog();

	SAFE_DELETE(g_pMemgr_f);
	SAFE_DELETE(g_pMemgr_i);
	SAFE_DELETE(g_pMemgr_u);
	SAFE_DELETE(g_pMemgr_w);
	SAFE_DELETE(g_pMemgr_vtx);
}

DLLCLBK gcGUIBase * gcGetGUICore()
{
	return dynamic_cast<gcGUIBase *>(g_pWM);
}


DLLCLBK gcConst * gcGetCoreAPI()
{
	return dynamic_cast<gcConst *>(g_pConst);
}



// ==============================================================
// D3D9Client class implementation
// ==============================================================

D3D9Client::D3D9Client (void *hInstance) :
	GraphicsClient(hInstance),
	vtab(NULL),
	scenarioName("(none selected)"),
	pLoadLabel(""),
	pLoadItem(""),
	pDevice     (NULL),
	pDefaultTex (NULL),
	pScatterTest(NULL),
	pFramework  (NULL),
	pItemsSkp   (NULL),
	hLblFont1   (NULL),
	hLblFont2   (NULL),
	hMainThread (),
	pCaps       (NULL),
	pWM			(NULL),
	pBltSkp		(NULL),
	pBltGrpTgt	(NULL),
	pCustomSplashScreen(NULL),
	pSplashTextColor(0xE0A0A0),
	hRenderWnd(),
	scene     (),
	meshmgr   (),
	bControlPanel (false),
	bScatterUpdate(false),
	bFullscreen   (false),
	bAAEnabled    (false),
	bFailed       (false),
	bRunning      (false),
	bVertexTex    (false),
	bVSync        (false),
	bRendering	  (false),
	bGDIClear	  (true),
	viewW         (0),
	viewH         (0),
	viewBPP       (0),
	frame_timer   (0),
	loadd_x       (0),
	loadd_y       (0),
	loadd_w       (0),
	loadd_h       (0),
	LabelPos      (0)

{
}

// ==============================================================

D3D9Client::~D3D9Client()
{
	LogAlw("D3D9Client destructor called");
	SAFE_DELETE(vtab);
	if (g_pD3DObject && QGuiApplication::instance()) // not upstream: a window still made for this instance (fast exit) gives its VkSurfaceKHR back first
		for (QWindow *w : QGuiApplication::allWindows())
			if (w->vulkanInstance() == g_pD3DObject) w->destroy();
	SAFE_DELETE(g_pD3DObject); // Release: the QVulkanInstance destroys the VkInstance
}


// ==============================================================
// 
bool D3D9Client::ChkDev(const char *fnc) const
{
	if (pDevice) return false;
	LogErr("Call [%s] Failed. D3D9 Graphics services off-line", fnc);
	return true;
}


// ==============================================================
// Overridden
//
const void *D3D9Client::GetConfigParam (DWORD paramtype) const
{
	return (paramtype >= CFGPRM_TILELOADTHREAD)
		 ? (paramtype >= CFGPRM_GETSELECTEDMESH)
		 ? DebugControls::GetConfigParam(paramtype)
		 : OapiExtension::GetConfigParam(paramtype)
		 : GraphicsClient::GetConfigParam(paramtype);
}


// ==============================================================
// This is called only once when the launchpad will appear
// This callback will initialize the Video tab only
//
bool D3D9Client::clbkInitialise()
{
	_TRACE;
	LogAlw("================ clbkInitialise ===============");
	LogAlw("Orbiter Version = %d",oapiGetOrbiterVersion());

	// D3D9ON12_ARGS and Direct3DCreate9On12 left out: D3D9on12 is a Windows layer, the native interface is the only one
	g_pD3DObject = new QVulkanInstance(); // Direct3DCreate9(D3D_SDK_VERSION)
	g_pD3DObject->setApiVersion(QVersionNumber(1, 4));
#ifdef _DEBUG
	g_pD3DObject->setLayers(QByteArrayList() << "VK_LAYER_KHRONOS_validation"); // not upstream: Vulkan validation in debug builds
#endif
	if (!g_pD3DObject->create()) SAFE_DELETE(g_pD3DObject);
	oapiWriteLog("[D3D9] Native Interface");

	if (g_pD3DObject) oapiWriteLog("[D3D9] Vulkan Instance Created...");
	else {
		oapiWriteLog("[D3D9][ERROR] Failed to create a Vulkan 1.4 instance");
		FailedDeviceError();
		return false;
	}

	// Perform default setup
	if (GraphicsClient::clbkInitialise()==false) return false;

	//Create the Launchpad video tab interface
	oapiWriteLog("[D3D9] Initialize VideoTab...");
	vtab = new VideoTab(this, ModuleInstance(), OrbiterInstance(), LaunchpadVideoTab());
	bool bInit = vtab->Initialise();
	if (LaunchpadVideoTab()) LaunchpadVideoWndProc(LaunchpadVideoTab()); // not upstream: the core's call came from GraphicsClient::clbkInitialise, before vtab existed
	return bInit;
}


// ==============================================================
// This is called when a simulation session will begin
//
#ifdef __linux__
// not upstream: SC_MONITORPOWER counterpart, the desktop's screen saver interface keeps the monitor on
static uint s_inhibit = 0;
static void InhibitScreenSaver (bool on)
{
	QDBusInterface ss("org.freedesktop.ScreenSaver", "/org/freedesktop/ScreenSaver", "org.freedesktop.ScreenSaver");
	if (on && !s_inhibit) {
		QDBusReply<uint> r = ss.call("Inhibit", QString("Orbiter"), QString("Fullscreen simulation"));
		if (r.isValid()) s_inhibit = r.value();
		else LogErr("Screen saver inhibit failed: %s", r.error().message().toUtf8().constData());
	}
	else if (!on && s_inhibit) {
		ss.call("UnInhibit", s_inhibit);
		s_inhibit = 0;
	}
}

#endif // __linux__
QWindow *D3D9Client::clbkCreateRenderWindow()
{
	_TRACE;

	LogAlw("================ clbkCreateRenderWindow ===============");

	if (!g_pD3DObject) return NULL;

	Config->WriteParams();
	
	uEnableLog		 = Config->DebugLvl;
	pSplashScreen    = NULL;
	pBackBuffer      = NULL;
	pTextScreen      = NULL;
	hRenderWnd       = NULL;
	pDefaultTex		 = NULL;
	hLblFont1		 = NULL;
	hLblFont2		 = NULL;
	bControlPanel    = false;
	bFullscreen      = false;
	bFailed			 = false;
	bRunning		 = false;
	bVertexTex		 = false;
	viewW = viewH    = 0;
	viewBPP          = 0;
	frame_timer		 = 0;
	scene            = NULL;
	meshmgr          = NULL;
	pFramework       = NULL;
	pDevice			 = NULL;
	pBltGrpTgt		 = NULL;	// Let's set this NULL here, constructor is called only once. Not when exiting and restarting a simulation.
	pNoiseTex		 = NULL;
	surfBltTgt		 = NULL;	// This variable is not used, set it to NULL anyway
	hMainThread		 = std::this_thread::get_id(); // GetCurrentThread

	D3DXMatrixIdentity(&ident);

	oapiDebugString()[0] = '\0';

	MeshCatalog.clear();
	SurfaceCatalog.clear();

	hRenderWnd = GraphicsClient::clbkCreateRenderWindow();
	hRenderWnd->setVulkanInstance(g_pD3DObject); // not upstream: the core's window has a Vulkan surface type, the client brings the instance

	LogAlw("Window Handle = %s",_PTR(hRenderWnd));
	hRenderWnd->setTitle("[VulkanClient]"); // SetWindowText

	LogOk("Starting to initialize device and 3D environment...");

	pFramework = new CD3DFramework9();

	WriteLog("[Vulkan Initialized]");

	int hr = pFramework->Initialize(hRenderWnd, GetVideoData());

	if (hr!=0) {
		LogErr("ERROR: Failed to initialize 3D Framework");
		return NULL;
	}

	// black GDI fill and ValidateRect left out: a Vulkan surface shows nothing before its first present (no white flash)

	pCaps = pFramework->GetCaps();

	WriteLog("[3DDevice Initialized]");

	pDevice		= pFramework->GetD3DDevice();
	viewW		= pFramework->GetWidth();
	viewH		= pFramework->GetHeight();
	bFullscreen = (pFramework->IsFullscreen() == TRUE);
#ifdef __linux__
	if (bFullscreen) InhibitScreenSaver (true); // WM_SYSCOMMAND SC_MONITORPOWER: no power loss in fullscreen mode
#endif // __linux__
	bAAEnabled  = (pFramework->IsAAEnabled() == TRUE);
	viewBPP		= 32;
	bVertexTex  = (pFramework->HasVertexTextureSup() == TRUE);
	bVSync		= (pFramework->GetVSync() == TRUE);

	char fld[] = "VulkanClient";

	g_pTexmgr_tt = new Texmgr<VkTex*>(pDevice, "TileTextures");
	g_pVtxmgr_vb = new Vtxmgr<VkBuf*>(pDevice, "TileVertex");
	g_pIdxmgr_ib = new Idxmgr<VkBuf*>(pDevice, "TileIndices");

	pNoiseTex = VkCreateTextureFromFile(pDevice, oapiResolvePath("Textures/D3D9Noise.dds").c_str(), 0, 0, 0, VK_FORMAT_UNDEFINED, SWZ_NONE, VK_IMAGE_USAGE_SAMPLED_BIT); // D3DXCreateTextureFromFileA
	HR(pNoiseTex ? 0 : -1);

	pDevice->SetRenderTarget(pFramework->GetBackBuffer(), SURFACE(pFramework->GetBackBufferHandle())->GetDepthStencil()); // not upstream: a D3D9 device starts out with these set
	pBackBuffer = pDevice->GetRenderTarget(); // GetRenderTarget(0, ...)
	pDepthStencil = pDevice->GetDepthStencil(); // GetDepthStencilSurface

	LogAlw("Render Target = %s", _PTR(pBackBuffer));
	LogAlw("DepthStencil = %s", _PTR(pDepthStencil));

	meshmgr		= new MeshManager(this);

	// Bring Sketchpad Online
	D3D9PadFont::D3D9TechInit(pDevice);
	D3D9PadPen::D3D9TechInit(pDevice);
	D3D9PadBrush::D3D9TechInit(pDevice);
	D3D9Text::D3D9TechInit(this, pDevice);
	D3D9Pad::D3D9TechInit(this, pDevice);

	deffont = (oapi::Font*) new D3D9PadFont(20, true, "fixed");
	defpen  = (oapi::Pen*)  new D3D9PadPen(1, 1, 0x00FF00);
	pLargeFont = (oapi::Font*) new D3D9PadFont(30, (char*)"Arial", 22, 700); // not upstream: the framework's D3DXCreateFontIndirect (Arial 30x22 bold)

	pDefaultTex = SURFACE(clbkLoadTexture("Null.dds"));
	if (pDefaultTex==NULL) LogErr("Null.dds not found");

	int x=0;
	if (viewW>1282) x=4;

	hLblFont1 = CreateGDIFont(24+x, 700, "Courier New"); // CreateFont(24+x, 0, 0, 0, 700, false, false, 0, 0, 3, 2, 1, 49, "Courier New")
	hLblFont2 = CreateGDIFont(18+x, 700, "Courier New"); // CreateFont(18+x, ...)

	SplashScreen();  // Warning SurfNative is not yet fully initialized here

	hRenderWnd->show(); // ShowWindow(SW_SHOW)

	OutputLoadStatus("Building Shader Programs...",0);

	D3D9Effect::D3D9TechInit(this, pDevice, fld);

	// Device-specific initialisations

	TileManager::GlobalInit(this);
	TileManager2Base::GlobalInit(this);
	RingManager::GlobalInit(this);
	HazeManager::GlobalInit(this);
	HazeManager2::GlobalInit(this);
	D3D9ParticleStream::GlobalInit(this);
	CSphereManager::GlobalInit(this);
	vStar::GlobalInit(this);
	vObject::GlobalInit(this);
	vVessel::GlobalInit(this);
	vPlanet::GlobalInit(this);
	OapiExtension::GlobalInit(*Config);

	OutputLoadStatus("SceneTech.fx",1);
	Scene::D3D9TechInit(pDevice, fld);
	D3D9Mesh::GlobalInit(pDevice);

	// Create scene instance
	scene = new Scene(this, viewW, viewH);

	WriteLog("[D3D9Client Initialized]");
	LogOk("...3D environment initialised");

#ifdef _NVAPI_H
	if (bNVAPI) {
		NvU8 bEnabled = 0;
		NvAPI_Status nvStereo = NvAPI_Stereo_IsEnabled(&bEnabled);

		if (nvStereo!=NVAPI_OK) {
			if (nvStereo==NVAPI_STEREO_NOT_INITIALIZED) LogWrn("Stereo API not initialized");
			if (nvStereo==NVAPI_API_NOT_INITIALIZED) LogErr("nVidia API not initialized");
			if (nvStereo==NVAPI_ERROR) LogErr("nVidia API ERROR");
		}

		if (bEnabled) {
			LogAlw("[nVidia Stereo mode is Enabled]");
			if (NvAPI_Stereo_CreateHandleFromIUnknown(pDevice, &pStereoHandle)!=NVAPI_OK) {
				LogErr("Failed to get StereoHandle");
			}
			else {
				if (pStereoHandle) {
					if (NvAPI_Stereo_SetConvergence(pStereoHandle, float(Config->Convergence))!=NVAPI_OK) {
						LogErr("SetConvergence Failed");
					}
					if (NvAPI_Stereo_SetSeparation(pStereoHandle, float(Config->Separation))!=NVAPI_OK) {
						LogErr("SetSeparation Failed");
					}
				}
			}
		}
		else LogAlw("[nVidia Stereo mode is Disabled]");
	}
#endif

	// Create status queries -----------------------------------------
	// (the Vulkan query types that stand for them)
	LogAlw("D3DQUERYTYPE_OCCLUSION is supported by device"); // VK_QUERY_TYPE_OCCLUSION is core Vulkan

	if (pDevice->props.limits.timestampComputeAndGraphics) LogAlw("D3DQUERYTYPE_PIPELINETIMINGS is supported by device"); // timestamp queries
	else LogAlw("D3DQUERYTYPE_PIPELINETIMINGS not supported by device");

	// D3DQUERYTYPE_BANDWIDTHTIMINGS and D3DQUERYTYPE_PIXELTIMINGS left out: Vulkan has no such query types

	return hRenderWnd;
}


// ==============================================================
// This is called when the simulation is ready to go but the clock
// is not yet ticking
//
void D3D9Client::clbkPostCreation()
{
	_TRACE;
	LogAlw("================ clbkPostCreation ===============");

	if (scene) scene->Initialise();

	// Create Window Manager -----------------------------------------
	//
	if (Config->gcGUIMode != 0) {
		pWM = new WindowManager(hRenderWnd, ModuleInstance(), !(GetVideoData()->fullscreen));
		if (pWM->IsOK() == false) SAFE_DELETE(pWM);
	}

	bRunning = true;

	LogAlw("=============== Loading Completed and Visuals Created ================");

#ifdef _DEBUG
	SketchPadTest();
#endif

	WriteLog("[Scene Initialized]");
}

// ==============================================================
// Perform some routine tests with sketchpad
//
void D3D9Client::SketchPadTest()
{
	SURFHANDLE hSrc = clbkLoadSurface("D3D9/SketchpadTest.dds", OAPISURFACE_TEXTURE);
	SURFHANDLE hTgt = clbkLoadSurface("D3D9/SketchpadTest.dds", OAPISURFACE_RENDERTARGET);

	if (!hSrc || !hTgt) return;

	oapiSetSurfaceColourKey(hSrc, 0xFFFFFFFF);

	Sketchpad *pSkp = oapiGetSketchpad(hTgt);

	RECT r = { 17, 1, 31, 15 };
	RECT t = { 1, 33, 31, 63 };
	RECT q = { 0, 16, 16, 32 };
	RECT w = { 49, 1, 63, 15 };

	pSkp->CopyRect(hSrc, &r, 1, 1);
	pSkp->ColorKey(hSrc, &r, 33, 1);
	pSkp->SetBlendState((Sketchpad::BlendState)(Sketchpad::BlendState::ALPHABLEND | Sketchpad::BlendState::FILTER_POINT));
	pSkp->StretchRect(hSrc, &r, &t);
	pSkp->SetBlendState();
	pSkp->StretchRect(hSrc, &r, &w);
	pSkp->RotateRect(hSrc, &q, 24, 24, float(PI05));
	pSkp->RotateRect(hSrc, &q, 40, 24, float(PI));
	pSkp->RotateRect(hSrc, &q, 56, 24, float(-PI05));

	RECT tr = { 33, 49, 47, 63 };
	pSkp->ColorFill(0xFFFF00FF, &tr);

	pSkp->QuickPen(0xFF000000);
	pSkp->QuickBrush(0x80FFFF00);
	pSkp->Rectangle(49, 33, 63, 47); // 1px margin
	pSkp->Ellipse(49, 49, 63, 63);	// 1px margin

									// No Pen, Brush Only
	pSkp->SetPen(NULL);
	pSkp->QuickBrush(0xFF00FF00);
	pSkp->Rectangle(65, 33, 79, 47); // 1px tlm 1px brm
	pSkp->Ellipse(65, 49, 79, 63);	// 1px tlm 1px brm

	pSkp->QuickPen(0xFFFFFFFF);
	pSkp->MoveTo(65, 1);
	pSkp->LineTo(65, 14);
	pSkp->LineTo(78, 14);
	pSkp->LineTo(78, 1);
	pSkp->LineTo(65, 1);

	RECT cr = { 33, 33, 47, 47 };
	pSkp->ClipRect(&cr);
	pSkp->ColorFill(0xFFFFFF00, NULL);
	pSkp->ClipRect();

	oapiReleaseSketchpad(pSkp);

	oapiSaveSurface("SketchpadOutput", hTgt, ImageFileFormat::IMAGE_PNG);

	oapiReleaseTexture(hSrc);
	oapiReleaseTexture(hTgt);

	 
	// Run Different Kind of Tests
	// 

	hTgt = oapiCreateSurfaceEx(768, 512, OAPISURFACE_RENDERTARGET);

	if (!hTgt) return;

	gcCore2* pCore = gcGetCoreInterface();

	if (!pCore) return;

	float a = 0.0f;
	float s = float(PI2 / 6.0);

	gcCore::clrVtx Vtx[8];
	oapi::FVECTOR2 Pol[6];

	for (int i = 0; i < 6; i++) {
		Pol[i].x = cos(a);
		Pol[i].y = sin(a);
		Vtx[i + 1].pos.x = Pol[i].x;
		Vtx[i + 1].pos.y = Pol[i].y;
		a += s;
	}

	//				 AABBGGRR
	Vtx[1].color = 0xFFFF0000;
	Vtx[2].color = 0xFFFFFF00;
	Vtx[3].color = 0xFF00FF00;
	Vtx[4].color = 0xFF00FFFF;
	Vtx[5].color = 0xFF0000FF;
	Vtx[6].color = 0xFFFF00FF;

	// Center vertex
	Vtx[0].pos.x = 0.0f;
	Vtx[0].pos.y = 0.0f;
	Vtx[0].color = 0xFFFFFFFF;    // White
	Vtx[7] = Vtx[1];

	HPOLY hColors = pCore->CreateTriangles(NULL, Vtx, 8, PF_FAN);
	HPOLY hOutline = pCore->CreatePoly(NULL, Pol, 6, PF_CONNECT);
	HPOLY hOutline2 = pCore->CreatePoly(NULL, Pol, 6);

	Vtx[0].color = 0xFFFF0000;    
	Vtx[1].color = 0xFFFFFF00;	  
	Vtx[2].color = 0xFFF0FF00;   
	Vtx[3].color = 0xFF00FFFF;	
	Vtx[4].color = 0xFF0000FF;    
	Vtx[5].color = 0xFFFF00FF;

	Vtx[0].pos = FVECTOR2(-1, 0);
	Vtx[1].pos = FVECTOR2(-1, 1);
	Vtx[2].pos = FVECTOR2(0, 0);
	Vtx[3].pos = FVECTOR2(0, 1);
	Vtx[4].pos = FVECTOR2(1, 0);
	Vtx[5].pos = FVECTOR2(1, 1);

	HPOLY hStrip = pCore->CreateTriangles(NULL, Vtx, 6, PF_STRIP);


	Vtx[0].color = 0xFF00FF00;    // Green
	Vtx[1].color = 0xFF00FF00;	  // Green	
	Vtx[2].color = 0xFFFF00FF;    // Mangenta
	Vtx[3].color = 0xFFFF00FF;	  // Mangenta
	Vtx[4].color = 0xFF0000FF;    // Blue
	Vtx[5].color = 0xFF0000FF;	  // Blue

	Vtx[0].pos = FVECTOR2(-1, 1);
	Vtx[1].pos = FVECTOR2(-1, 0);
	Vtx[2].pos = FVECTOR2(0, 1);
	Vtx[3].pos = FVECTOR2(0, 0);
	Vtx[4].pos = FVECTOR2(1, 1);
	Vtx[5].pos = FVECTOR2(1, 0);

	HPOLY hStrip2 = pCore->CreateTriangles(NULL, Vtx, 6, PF_STRIP);


	IVECTOR2 pos0 = { 128, 128 };
	IVECTOR2 pos1 = { 128, 384 };
	IVECTOR2 pos2 = { 384, 128 };
	IVECTOR2 pos3 = { 640, 128 };
	IVECTOR2 pos4 = { 384, 384 };
	IVECTOR2 pos5 = { 640, 384 };

	pSkp = oapiGetSketchpad(hTgt);

	pSkp->ColorFill(0xFFFFFFFF, NULL);

	pSkp->QuickBrush(0xA0000088);
	pSkp->QuickPen(0xA0000000, 3.0f);
	pSkp->PushWorldTransform();

	pSkp->SetWorldScaleTransform2D(ptr(FVECTOR2(100.0f, 100.0f)), &pos0);
	pSkp->DrawPoly(hColors);
	pSkp->DrawPoly(hOutline);

	pSkp->SetWorldScaleTransform2D(ptr(FVECTOR2(100.0f, 100.0f)), &pos1);
	pSkp->DrawPoly(hOutline2);

	pSkp->SetWorldScaleTransform2D(ptr(FVECTOR2(100.0f, 100.0f)), &pos2);
	pSkp->DrawPoly(hStrip);

	pSkp->SetWorldScaleTransform2D(ptr(FVECTOR2(100.0f, 100.0f)), &pos3);
	pSkp->DrawPoly(hStrip2);

	pSkp->SetWorldScaleTransform2D(ptr(FVECTOR2(100.0f, 100.0f)), &pos4);
	pSkp->QuickPen(0xFF000000, 25.0f);
	pSkp->DrawPoly(hOutline);

	hSrc = clbkLoadSurface("generic/noisep.dds", OAPISURFACE_TEXTURE);

	pSkp->SetWorldScaleTransform2D(ptr(FVECTOR2(1.0f, 1.0f)), &pos5);

	FVECTOR2 pt[4];
	pt[0] = FVECTOR2(-100.0f, -100.0f);
	pt[1] = FVECTOR2(-100.0f, 50.0f);
	pt[2] = FVECTOR2(100.0f, 100.0f);
	pt[3] = FVECTOR2(100.0f, -50.0f);

	pSkp->CopyTetragon(hSrc, NULL, pt);
	pSkp->PopWorldTransform();


	oapiReleaseTexture(hSrc);
	oapiReleaseSketchpad(pSkp);


	pCore->DeletePoly(hColors); // Must release Sketchpad before releasing any sketchpad resources
	pCore->DeletePoly(hOutline);
	pCore->DeletePoly(hOutline2);
	pCore->DeletePoly(hStrip);
	pCore->DeletePoly(hStrip2);

	oapiSaveSurface("SketchpadOutput2", hTgt, ImageFileFormat::IMAGE_DDS);

	oapiReleaseTexture(hTgt);
}




// ==============================================================
// Called when simulation session is about to be closed
//
void D3D9Client::clbkCloseSession(bool fastclose)
{

	LogAlw("================ clbkCloseSession ===============");

	//	Post shutdown signals for gcGUI applications
	//
	for (auto pApp : g_gcGUIAppList) pApp->clbkShutdown();

	//	Post shutdown signals for user applications
	//
	if (IsGenericProcEnabled(GENERICPROC_SHUTDOWN)) MakeGenericProcCall(GENERICPROC_SHUTDOWN, 0, NULL);


	// Check the status of RenderTarget Stack ------------------------------------------------
	//
	if (RenderStack.empty() == false) {
		LogErr("RenderStack contains %d items:", RenderStack.size());
		while (!RenderStack.empty()) {
			LogErr("RenderTarget=%s, DepthStencil=%s", _PTR(RenderStack.front().pColor), _PTR(RenderStack.front().pDepthStencil));
			RenderStack.pop_front();
		}
	}

	// Disable rendering and some other systems
	//
	bRunning = false;


	// At first, shutdown tile loaders -------------------------------------------------------
	//
	if (TileBuffer::ShutDown()==false) LogErr("Failed to Shutdown TileBuffer()");
	if (TileManager2Base::ShutDown()==false) LogErr("Failed to Shutdown TileManager2Base()");

	// Close dialog if Open and disconnect a visual form debug controls
	DebugControls::Close();

	// Disconnect textures from pipeline (Unlikely nesseccary)
	D3D9Effect::ShutDown();

	// DEBUG: List all textures connected to meshes
	/* DWORD cnt = MeshCatalog->CountEntries();
	for (DWORD i=0;i<cnt;i++) {
		D3D9Mesh *x = (D3D9Mesh*)MeshCatalog->Get(i);
		if (x) x->DumpTextures();
	} */
	//GraphicsClient::clbkCloseSession(fastclose);
	pCustomSplashScreen = NULL;
	pSplashTextColor = 0xE0A0A0;

	SAFE_DELETE(pWM);
	LogAlw("================= Deleting Scene ================");
	Scene::GlobalExit();
	SAFE_DELETE(scene);
	LogAlw("============== Deleting Mesh Manager ============");
	SAFE_DELETE(meshmgr);
	WriteLog("[Session Closed. Scene deleted.]");

}

// ==============================================================

void D3D9Client::clbkDestroyRenderWindow (bool fastclose)
{
	_TRACE;
	oapiWriteLog((char*)"D3D9: [Destroy Render Window Called]");
	LogAlw("============= clbkDestroyRenderWindow ===========");

#ifdef _NVAPI_H
	if (bNVAPI) {
		if (pStereoHandle) {
			if (NvAPI_Stereo_DestroyHandle(pStereoHandle)!=NVAPI_OK) {
				LogErr("Failed to destroy stereo handle");
			}
		}
	}
#endif

	LogAlw("===== Calling GlobalExit() for sub-systems ======");
	HazeManager::GlobalExit();
	HazeManager2::GlobalExit();
	TileManager::GlobalExit();
	TileManager2Base::GlobalExit();
	D3D9ParticleStream::GlobalExit();
	CSphereManager::GlobalExit();
	vStar::GlobalExit();
	vVessel::GlobalExit();
	vPlanet::GlobalExit();
	vObject::GlobalExit();
	D3D9Mesh::GlobalExit();

	SAFE_DELETE(defpen);
	SAFE_DELETE(deffont);
	SAFE_DELETE(pLargeFont); // not upstream: see clbkCreateRenderWindow

	SAFE_DELETE(hLblFont1); // DeleteObject
	SAFE_DELETE(hLblFont2);

	D3D9Pad::GlobalExit();
	D3D9Text::GlobalExit();
	D3D9Effect::GlobalExit();

	SAFE_DELETE(pSplashScreen);	// Splash screen related
	SAFE_DELETE(pTextScreen);		// Splash screen related
	DELETE_SURFACE(pDefaultTex);
	SAFE_DELETE(pNoiseTex);

	SURFHANDLE hBackBuffer = GetBackBufferHandle();

	DELETE_SURFACE(hBackBuffer);

	LogAlw("============ Checking Object Catalogs ===========");

	// Clear microtextures --------------------------------------------------------------------------------------
	//
	for (auto& it : MicroTextures) SAFE_DELETE(it.second);
	MicroTextures.clear();


	// Check surface catalog --------------------------------------------------------------------------------------
	//
	if (SharedTextures.size() > 0)
	{
		LogWrn("Texture Repository has %u entries... Releasing...", (DWORD)SharedTextures.size());
		auto Undeleted(SharedTextures);
		for (auto srf : Undeleted) {
			LogWrn("Texture [%s]", SURFACE(srf.second)->GetName());
			delete lpSurfNative(srf.second);
		}
	}

	// Check surface catalog --------------------------------------------------------------------------------------
	//
	if (SurfaceCatalog.size() > 0)
	{
		LogErr("UnDeleted Surfaces(s) Detected %u... Releasing...", (DWORD)SurfaceCatalog.size());
		auto Undeleted(SurfaceCatalog);
		for (auto srf : Undeleted) {
			LogErr("Surface [%s] (%u, %u)", srf->GetName(), srf->GetWidth(), srf->GetHeight());
			delete srf;
		}
	}

	// Check mesh catalog --------------------------------------------------------------------------------------
	//
	if (MeshCatalog.size() > 0)
	{
		LogErr("UnDeleted Meshe(s) Detected %u", (DWORD)MeshCatalog.size());
		auto Undeleted(MeshCatalog);
		for (auto msh : Undeleted)
		{
			LogErr("Mesh[%s] Handle = %s ", msh->GetName(), _PTR(msh));
			delete msh;
		}
	}

	// Check Fonts catalog --------------------------------------------------------------------------------------
	//
	if (g_fonts.size()) {
		LogWrn("%u un-released fonts!", g_fonts.size());
		for (auto it = g_fonts.begin(); it != g_fonts.end(); ) {
			clbkReleaseFont(*it++);
		}
		g_fonts.clear();
	}

	// --- Brushes
	if (g_brushes.size()) {
		LogWrn("%u un-released brushes!", g_brushes.size());
		for (auto it = g_brushes.begin(); it != g_brushes.end(); ) {
			clbkReleaseBrush(*it++);
		}
		g_brushes.clear();
	}

	// --- Pens
	if (g_pens.size()) {
		LogWrn("%u un-released pens!", g_pens.size());
		for (auto it = g_pens.begin(); it != g_pens.end(); ) {
			clbkReleasePen(*it++);
		}
		g_pens.clear();
	}

	// Check tile catalog --------------------------------------------------------------------------------------
	//

	for (auto it : MeshMap)	SAFE_DELETE(it.second);

	MeshMap.clear();
	SharedTextures.clear();
	SurfaceCatalog.clear();
	MeshCatalog.clear();

	g_pTexmgr_tt->CleanUp();
	g_pVtxmgr_vb->CleanUp();
	g_pIdxmgr_ib->CleanUp();
	SAFE_DELETE(g_pTexmgr_tt);
	SAFE_DELETE(g_pVtxmgr_vb);
	SAFE_DELETE(g_pIdxmgr_ib);

	pFramework->DestroyObjects();

	SAFE_DELETE(pFramework);

	// Close Render Window -----------------------------------------
	GraphicsClient::clbkDestroyRenderWindow(fastclose);
#ifdef __linux__
	InhibitScreenSaver (false);
#endif // __linux__

	hRenderWnd		 = NULL;
	pDevice			 = NULL;
	bFailed			 = false;
	viewW = viewH    = 0;
	viewBPP          = 0;

}

// ==============================================================

void D3D9Client::clbkDebugString(const char* str)
{
	D3D9DebugLog("%s", str);
}


// ==============================================================

void D3D9Client::PushSketchpad(SURFHANDLE surf, D3D9Pad *pSkp) const
{
	if (surf) {
		VkSurf *pTgt = SURFACE(surf)->GetSurface();
		VkSurf *pDep = SURFACE(surf)->GetDepthStencil();
		PushRenderTarget(pTgt, pDep, RENDERPASS_SKETCHPAD);
		RenderStack.front().pSkp = pSkp;
	}
}


// ==============================================================

void D3D9Client::PushRenderTarget(VkSurf *pColor, VkSurf *pDepthStencil, int code) const
{
	static const char *labels[] = { "NULL", "MAIN", "ENV", "CUSTOMCAM", "SHADOWMAP", "PICK", "SKETCHPAD", "OVERLAY" };

	RenderTgtData data;
	data.pColor = pColor;
	data.pDepthStencil = pDepthStencil;
	data.pSkp = NULL;
	data.code = code;

	if (pColor) {
		pDevice->SetViewport(0.0f, 0.0f, (float)pColor->w, (float)pColor->h, 0.0f, 1.0f); // GetDesc, D3DVIEWPORT9
	}

	// If pDepthStencil is NULL set NULL
	// SetDepthStencilSurface + SetRenderTarget(0, pColor) in one call (no failure result); a NULL pColor keeps the current one
	pDevice->SetRenderTarget(pColor ? pColor : pDevice->GetRenderTarget(), pDepthStencil);

	RenderStack.push_front(data);
	LogDbg("Plum", "PUSH:RenderStack[%lu]={%s, %s} %s", RenderStack.size(), _PTR(data.pColor), _PTR(data.pDepthStencil), labels[data.code]);
}

// ==============================================================

void D3D9Client::AlterRenderTarget(VkSurf *pColor, VkSurf *pDepthStencil)
{
	// GetDesc and D3DVIEWPORT9 left out: the VkSurf carries its size

	pDevice->SetViewport(0.0f, 0.0f, (float)pColor->w, (float)pColor->h, 0.0f, 1.0f);
	pDevice->SetRenderTarget(pColor, pDepthStencil); // SetRenderTarget(0, pColor), SetDepthStencilSurface
}

// ==============================================================

void D3D9Client::PopRenderTargets() const
{
	static const char *labels[] = { "NULL", "MAIN", "ENV", "CUSTOMCAM", "SHADOWMAP", "PICK", "SKETCHPAD", "OVERLAY" };

	assert(RenderStack.empty() == false);

	RenderStack.pop_front();

	if (RenderStack.empty()) {
		LogDbg("Orange", "POP: Last one out ------------------------------------");
		return;
	}

	RenderTgtData data = RenderStack.front();

	if (data.pColor) {
		// GetDesc and D3DVIEWPORT9 left out: the VkSurf carries its size

		pDevice->SetViewport(0.0f, 0.0f, (float)data.pColor->w, (float)data.pColor->h, 0.0f, 1.0f);
		pDevice->SetRenderTarget(data.pColor, data.pDepthStencil); // SetRenderTarget(0, ...), SetDepthStencilSurface
	}

	LogDbg("Plum", "POP:RenderStack[%lu]={%s, %s, %s} %s", RenderStack.size(), _PTR(data.pColor), _PTR(data.pDepthStencil), _PTR(data.pSkp), labels[data.code]);
}

// ==============================================================

void D3D9Client::HackFriendlyHack()
{
	// Try to make the application more hackable by setting the D3D Device in 'more' expected state.

	// GetDesc and D3DVIEWPORT9 left out: the VkSurf carries its size
	pDevice->SetViewport(0.0f, 0.0f, (float)GetBackBuffer()->w, (float)GetBackBuffer()->h, 0.0f, 1.0f);
	pDevice->SetRenderTarget(GetBackBuffer(), GetDepthStencil()); // SetRenderTarget(0, ...), SetDepthStencilSurface
}

// ==============================================================

VkSurf *D3D9Client::GetTopDepthStencil()
{
	if (RenderStack.empty()) return NULL;
	return RenderStack.front().pDepthStencil;
}

// ==============================================================

VkSurf *D3D9Client::GetTopRenderTarget()
{
	if (RenderStack.empty()) return NULL;
	return RenderStack.front().pColor;
}

// ==============================================================

D3D9Pad *D3D9Client::GetTopInterface() const
{
	if (RenderStack.empty()) return NULL;
	return RenderStack.front().pSkp;
}


// ==============================================================

void D3D9Client::clbkUpdate(bool running)
{
	_TRACE;
	double tot_update = D3D9GetTime();
	if (bFailed==false && bRunning) scene->Update();
	D3D9SetTime(D3D9Stats.Timer.Update, tot_update);
}

// ==============================================================

double frame_time = 0.0;
double scene_time = 0.0;

void D3D9Client::clbkRenderScene()
{
	_TRACE;

	if (pDevice==NULL || scene==NULL) return;
	if (bFailed) return;
	if (!bRunning) return;

	if (pWM) pWM->Animate();

	if (Config->PresentLocation == 1) PresentScene();

	scene_time = D3D9GetTime();

	// TestCooperativeLevel and the lost device message left out: Vulkan has no lost device state to restore

	UINT mem = UINT(GetAvailableTextureMem(pDevice)>>20);
	if (mem<32) TileBuffer::HoldThread(true);

	scene->RenderMainScene();		// Render the main scene

	VESSEL *hVes = oapiGetFocusInterface();

	if (hVes && Config->LabelDisplayFlags)
	{
		char Label[7] = "";
		if (Config->LabelDisplayFlags & D3D9Config::LABEL_DISPLAY_RECORD && hVes->Recording()) snprintf(Label, 7, "%s", "Record");
		if (Config->LabelDisplayFlags & D3D9Config::LABEL_DISPLAY_REPLAY && hVes->Playback()) snprintf(Label, 7, "%s", "Replay");

		if (Label[0]!=0) {
			// BeginScene: nothing to begin, rendering starts at the first draw
			RECT rect2 = _RECT(0, viewH - 60, viewW, viewH - 20);
			DrawLargeText(Label, 6, &rect2, D3DCOLOR_XRGB(0, 0, 0)); // GetLargeFont()->DrawTextA(DT_CENTER | DT_TOP)
			rect2.left-=4; rect2.top-=4;
			DrawLargeText(Label, 6, &rect2, D3DCOLOR_XRGB(255, 255, 255));
			pDevice->EndRendering(); // EndScene
		}
	}

	if (bFreeze) {
		RECT rect2 = _RECT(0, viewH - 60, viewW, viewH - 20);
		DrawLargeText("Frozen", 6, &rect2, D3DCOLOR_XRGB(0, 255, 255)); // GetLargeFont()->DrawTextA(DT_CENTER | DT_TOP)
	}

	D3D9SetTime(D3D9Stats.Timer.Scene, scene_time);


	if (bControlPanel) RenderControlPanel();

	// Compute total frame time
	D3D9SetTime(D3D9Stats.Timer.FrameTotal, frame_time);
	frame_time = D3D9GetTime();
}

// ==============================================================

void D3D9Client::clbkTimeJump(double simt, double simdt, double mjd)
{
	_TRACE;
	GraphicsClient::clbkTimeJump (simt, simdt, mjd);
}

// ==============================================================

void D3D9Client::PresentScene()
{
	double time = D3D9GetTime();

	if (bFullscreen == false) {
		RenderWithPopupWindows();
		pFramework->Present(); // Present(0, 0, 0, 0)
	}
	else {
		if (!RenderWithPopupWindows()) pFramework->Present();
	}

	D3D9SetTime(D3D9Stats.Timer.Display, time);
}

// ==============================================================

double framer_rater_limit = 0.0;

bool D3D9Client::clbkDisplayFrame()
{
	_TRACE;
//	static int iRefrState = 0;
	double time = D3D9GetTime();

	if (!bRunning && pDevice) {
		RECT txt = _RECT( loadd_x, loadd_y, loadd_x+loadd_w, loadd_y+loadd_h );
		pDevice->StretchRect(pSplashScreen, NULL, pBackBuffer, NULL, VK_FILTER_NEAREST); // D3DTEXF_POINT
		pDevice->StretchRect(pTextScreen, NULL, pBackBuffer, &txt, VK_FILTER_NEAREST);
	}

	if (Config->PresentLocation == 0) PresentScene();

	double frmt = (1000000.0/Config->FrameRate) - (time - framer_rater_limit);

	framer_rater_limit = time;

	if (Config->EnableLimiter && Config->FrameRate>0 && bVSync==false) {
		if (frmt>0) frame_timer++;
		else        frame_timer--;
		if (frame_timer>40) frame_timer=40;
		std::this_thread::sleep_for(std::chrono::milliseconds(frame_timer)); // Sleep
	}

	return true;
}

// ==============================================================

void D3D9Client::clbkPreOpenPopup ()
{
	_TRACE;
	// SetDialogBoxMode(true) left out: Qt dialogs are windows of their own, not drawn through the swapchain
}

// =======================================================================

static DWORD g_lastPopupWindowCount = 0;
static void FixOutOfScreenPositions (QWidget *const *hWnd, DWORD count)
{
	// Only check if a popup window is *added*
	if (count > g_lastPopupWindowCount)
	{
		for (DWORD i=0; i<count; ++i)
		{
			QRect g = hWnd[i]->frameGeometry(); // GetWindowRect
			RECT rect = { g.left(), g.top(), g.left() + g.width(), g.top() + g.height() };

			int x = -1, y; // x != -1 indicates "position change needed"
			if (rect.left < 0) {
				x = 0;
				y = rect.top;
			}
			if (rect.top  < 0) {
				x = rect.left;
				y = 0;
			}

			// For the rest we need monitor information...
			QScreen *monitor = hWnd[i]->screen(); // MonitorFromWindow(MONITOR_DEFAULTTONEAREST)
			QRect rcMonitor = monitor ? monitor->geometry() : QRect(); // GetMonitorInfo

			int monitorWidth = rcMonitor.width(); // info.rcWork....
			int monitorHeight = rcMonitor.height();

			if (rect.right > monitorWidth) {
				x = monitorWidth - (rect.right - rect.left);
				y = rect.top;
			}
			if (rect.bottom > monitorHeight) {
				x = rect.left;
				y = monitorHeight - (rect.bottom - rect.top);
			}

			if (x != -1) {
				hWnd[i]->move(x, y); // MoveWindow(x, y, w, h) with the window's own size w, h
			}
		}

	}
	g_lastPopupWindowCount = count;
}

// =======================================================================

bool D3D9Client::RenderWithPopupWindows()
{
	_TRACE;

	QWidget *const *hPopupWnd;
	DWORD count = GetPopupList(&hPopupWnd);

	// SetDialogBoxMode left out: see clbkPreOpenPopup

	FixOutOfScreenPositions(hPopupWnd, count);

	if (!bFullscreen) {
		for (DWORD i=0;i<count;i++) {
			Qt::WindowFlags val = hPopupWnd[i]->windowFlags(); // GetWindowLongA(GWL_STYLE)
			if ((val & Qt::CustomizeWindowHint) && (val&Qt::WindowSystemMenuHint)==0) { // WS_SYSMENU: only customized flags go without it
				bool bVis = hPopupWnd[i]->isVisible();
				hPopupWnd[i]->setWindowFlags(val|Qt::WindowSystemMenuHint); // SetWindowLongA
				if (bVis) hPopupWnd[i]->show(); // Qt hides a window whose flags change
			}
		}
	}

	return false;
}

#pragma region Particle stream functions

// =======================================================================
// Particle stream functions
// ==============================================================

ParticleStream *D3D9Client::clbkCreateParticleStream(PARTICLESTREAMSPEC *pss)
{
	LogErr("UnImplemented Feature Used clbkCreateParticleStream");
	return NULL;
}

// =======================================================================

ParticleStream *D3D9Client::clbkCreateExhaustStream(PARTICLESTREAMSPEC *pss,
	OBJHANDLE hVessel, const double *lvl, const VECTOR3 *ref, const VECTOR3 *dir)
{
	_TRACE;
	ExhaustStream *es = new ExhaustStream (this, hVessel, lvl, ref, dir, pss);
	scene->AddParticleStream (es);
	return es;
}

// =======================================================================

ParticleStream *D3D9Client::clbkCreateExhaustStream(PARTICLESTREAMSPEC *pss,
	OBJHANDLE hVessel, const double *lvl, const VECTOR3 &ref, const VECTOR3 &dir)
{
	_TRACE;
	ExhaustStream *es = new ExhaustStream (this, hVessel, lvl, ref, dir, pss);
	scene->AddParticleStream (es);
	return es;
}

// ======================================================================

ParticleStream *D3D9Client::clbkCreateReentryStream (PARTICLESTREAMSPEC *pss,
	OBJHANDLE hVessel)
{
	_TRACE;
	ReentryStream *rs = new ReentryStream (this, hVessel, pss);
	scene->AddParticleStream (rs);
	return rs;
}

#pragma endregion

// ==============================================================

ScreenAnnotation* D3D9Client::clbkCreateAnnotation()
{
	_TRACE;
	return GraphicsClient::clbkCreateAnnotation();
}

#pragma region Mesh functions

// ==============================================================

void D3D9Client::clbkStoreMeshPersistent(MESHHANDLE hMesh, const char *fname)
{
	_TRACE;
	if (ChkDev(__FUNCTION__)) return;

	if (fname) {
		LogAlw("Storing a mesh %s (%s)", _PTR(hMesh), fname);
		if (hMesh==NULL) LogErr("D3D9Client::clbkStoreMeshPersistent(%s) hMesh is NULL",fname);
	}
	else {
		LogAlw("Storing a mesh %s", _PTR(hMesh));
		if (hMesh==NULL) LogErr("D3D9Client::clbkStoreMeshPersistent() hMesh is NULL");
	}

	if (hMesh==NULL) return;

	int idx = meshmgr->StoreMesh(hMesh, fname);
}

// ==============================================================

DEVMESHHANDLE D3D9Client::GetDevMesh(MESHHANDLE hMesh)
{
	const D3D9Mesh *pDevMesh = meshmgr->GetMesh(hMesh);
	if (!pDevMesh) {
		meshmgr->StoreMesh(hMesh, "GetDevMesh()");
		pDevMesh = meshmgr->GetMesh(hMesh);
	}

	// Create a new Instance from a template
	return DEVMESHHANDLE(new D3D9Mesh(hMesh, *pDevMesh));
}

// ==============================================================

bool D3D9Client::clbkSetMeshTexture(DEVMESHHANDLE hMesh, DWORD texidx, SURFHANDLE surf)
{
	_TRACE;
	if (hMesh && surf) return ((D3D9Mesh*)hMesh)->SetTexture(texidx, SURFACE(surf));
	return false;
}

// ==============================================================

int D3D9Client::clbkSetMeshMaterial(DEVMESHHANDLE hMesh, DWORD matidx, const MATERIAL *mat)
{
	_TRACE;
	if (!hMesh) return 3;
	D3D9Mesh *mesh = (D3D9Mesh*)hMesh;
	DWORD nmat = mesh->GetMaterialCount();
	if (matidx >= nmat) return 4; // "index out of range"
	D3D9MatExt meshmat;
	//mesh->GetMaterial(&meshmat, matidx);
	CreateMatExt((const D3DMATERIAL9 *)mat, &meshmat);
	mesh->SetMaterial(&meshmat, matidx);
	return 0;
}

// ==============================================================

int D3D9Client::clbkMeshMaterial (DEVMESHHANDLE hMesh, DWORD matidx, MATERIAL *mat)
{
	_TRACE;
	if (!hMesh) return 3;
	D3D9Mesh *mesh = (D3D9Mesh*)hMesh;
	DWORD nmat = mesh->GetMaterialCount();
	if (matidx >= nmat) return 4; // "index out of range"
	const D3D9MatExt *meshmat = mesh->GetMaterial(matidx);
	if (meshmat) GetMatExt(meshmat, (D3DMATERIAL9 *)mat);
	return 0;
}

// ==============================================================

int D3D9Client::clbkSetMeshMaterialEx(DEVMESHHANDLE hMesh, DWORD matidx, MatProp mat, const oapi::FVECTOR4* in)
{
	if (!hMesh) return 3;
	D3D9Mesh* mesh = (D3D9Mesh*)hMesh;
	return mesh->SetMaterialEx(matidx, mat, in);
}

// ==============================================================

int D3D9Client::clbkMeshMaterialEx(DEVMESHHANDLE hMesh, DWORD matidx, MatProp mat, oapi::FVECTOR4* out)
{
	if (!hMesh) return 3;
	D3D9Mesh* mesh = (D3D9Mesh*)hMesh;
	return mesh->GetMaterialEx(matidx, mat, out);
}

// ==============================================================

bool D3D9Client::clbkSetMeshProperty(DEVMESHHANDLE hMesh, DWORD prop, DWORD value)
{
	_TRACE;
	D3D9Mesh *mesh = (D3D9Mesh*)hMesh;
	switch (prop) {
		case MESHPROPERTY_MODULATEMATALPHA:
			mesh->EnableMatAlpha(value!=0);
			return true;
	}
	return false;
}

// ==============================================================
// Returns a dev-mesh for a visual

MESHHANDLE D3D9Client::clbkGetMesh(VISHANDLE vis, UINT idx)
{
	_TRACE;
	if (vis==NULL) {
		LogErr("NULL visual in clbkGetMesh(NULL,%u)",idx);
		return NULL;
	}
	MESHHANDLE hMesh = ((vObject*)vis)->GetMesh(idx);
	if (hMesh==NULL) LogWrn("clbkGetMesh() returns NULL");
	return hMesh;
}

// =======================================================================

int D3D9Client::clbkEditMeshGroup(DEVMESHHANDLE hMesh, DWORD grpidx, GROUPEDITSPEC *ges)
{
	_TRACE;
	return ((D3D9Mesh*)hMesh)->EditGroup(grpidx, ges);
}

// =======================================================================


int D3D9Client::clbkGetMeshGroup (DEVMESHHANDLE hMesh, DWORD grpidx, GROUPREQUESTSPEC *grs)
{
	_TRACE;
	return ((D3D9Mesh*)hMesh)->GetGroup (grpidx, grs);
}

#pragma endregion

// ==============================================================

void D3D9Client::clbkNewVessel(OBJHANDLE hVessel)
{
	_TRACE;
	if (scene) scene->NewVessel(hVessel);
}

// ==============================================================

void D3D9Client::clbkDeleteVessel(OBJHANDLE hVessel)
{
	if (scene) scene->DeleteVessel(hVessel);
}


// ==============================================================
// copy video options from the video tab

void D3D9Client::clbkRefreshVideoData()
{
	_TRACE;
	if (vtab) vtab->UpdateConfigData();
}

// ==============================================================

void D3D9Client::clbkOptionChanged(DWORD cat, DWORD item)
{
	switch (cat) {
	case OPTCAT_CELSPHERE:
		if (scene) scene->OnOptionChanged(cat, item);
		return;
	}
}

// ==============================================================

bool D3D9Client::clbkUseLaunchpadVideoTab() const
{
	_TRACE;
	return true;
}

// ==============================================================
// Fullscreen mode flag

bool D3D9Client::clbkFullscreenMode() const
{
	_TRACE;
	return bFullscreen;
}

// ==============================================================
// return the dimensions of the render viewport

void D3D9Client::clbkGetViewportSize(DWORD *width, DWORD *height) const
{
	_TRACE;
	*width = viewW, *height = viewH;
}

// ==============================================================
// Returns a specific render parameter

bool D3D9Client::clbkGetRenderParam(DWORD prm, DWORD *value) const
{
	_TRACE;
	switch (prm) {
		case RP_COLOURDEPTH:
			*value = viewBPP;
			return true;

		case RP_ZBUFFERDEPTH:
			*value = GetFramework()->GetZBufferBitDepth();
			return true;

		case RP_STENCILDEPTH:
			*value = GetFramework()->GetStencilBitDepth();
			return true;

		case RP_MAXLIGHTS:
			*value = MAX_SCENE_LIGHTS;
			return true;

		case RP_REQUIRETEXPOW2:
			*value = 0;
			return true;
	}
	return false;
}

// ==============================================================
// Responds to visual events

int D3D9Client::clbkVisEvent(OBJHANDLE hObj, VISHANDLE vis, DWORD msg, DWORD_PTR context)
{
	_TRACE;
	VisObject *vo = (VisObject*)vis;
	vo->clbkEvent(msg, context);
	if (DebugControls::IsActive()) {
		if (msg==EVENT_VESSEL_INSMESH || msg==EVENT_VESSEL_DELMESH) {
			if (DebugControls::GetVisual()==vo) DebugControls::UpdateVisual();
		}
	}
	return 1;
}


// ==============================================================
//
void D3D9Client::PickTerrain(DWORD uMsg, int xpos, int ypos)
{
	bool bUD = (uMsg == WM_LBUTTONUP || uMsg == WM_RBUTTONUP || uMsg == WM_LBUTTONDOWN || uMsg == WM_RBUTTONDOWN);
	bool bPrs = IsGenericProcEnabled(GENERICPROC_PICK_TERRAIN) && bUD;
	bool bHov = IsGenericProcEnabled(GENERICPROC_HOVER_TERRAIN) && (uMsg == WM_MOUSEMOVE || uMsg == WM_MOUSEWHEEL);

	if (bPrs || bHov) {
		gcCore::PickGround pg = gcCore2::ScanScreen(xpos, ypos);
		pg.msg = uMsg;
		if (bPrs) MakeGenericProcCall(GENERICPROC_PICK_TERRAIN, sizeof(gcCore::PickGround), &pg);
		if (bHov) MakeGenericProcCall(GENERICPROC_HOVER_TERRAIN, sizeof(gcCore::PickGround), &pg);
	}
}


// ==============================================================
// Message handler for render window

bool D3D9Client::RenderWndProc (QWindow *hWnd, QEvent *event)
{
	static bool bTrackMouse = false;
	static short xpos=0, ypos=0;

	D3D9Pick pick;

	if (hRenderWnd!=hWnd) {
		if (!event->isInputEvent()) return false; // WM_NCDESTROY exception: Qt's teardown events (hide, surface, delete) go on
		LogErr("Invalid Window !! RenderWndProc() called after calling clbkDestroyRenderWindow() event=0x%X", (UINT)event->type());
		return true;
	}

	if (bRunning && DebugControls::IsActive()) {
		// Must update camera to correspond MAIN_SCENE due to Pick() function,
		// because env-maps have altered camera settings
		// GetScene()->UpdateCameraFromOrbiter(RENDERPASS_PICKSCENE);
		// Obsolete: since moving env/cam stuff in pre-scene
	}

	if (pWM) if (pWM->MainWindowProc(hWnd, event)) return true;

	qreal dpr = hWnd->devicePixelRatio(); // not upstream: the LPARAM positions are client coordinates in device pixels
	QMouseEvent *me = dynamic_cast<QMouseEvent*>(event); // mouse button and move events
	Qt::MouseButton button = me ? me->button() : Qt::NoButton;
	bool bDown = (event->type() != QEvent::MouseButtonRelease); // no CS_DBLCLKS: a double click is another button down

	switch (event->type())
	{
		case QEvent::Leave: // WM_MOUSELEAVE (Qt reports it without TrackMouseEvent)
		{
			QMouseEvent up(QEvent::MouseButtonRelease, QPointF(0, 0), QPointF(0, 0), Qt::LeftButton, Qt::NoButton, Qt::NoModifier);
			if (bTrackMouse && bRunning) GraphicsClient::RenderWndProc (hWnd, &up); // WM_LBUTTONUP, 0, 0
			return true;
		}

		case QEvent::MouseButtonPress: // WM_LBUTTONDOWN, WM_RBUTTONDOWN, WM_MBUTTONDOWN
		case QEvent::MouseButtonDblClick:
		case QEvent::MouseButtonRelease: // WM_LBUTTONUP, WM_RBUTTONUP
		{
		if (button == Qt::MiddleButton) // WM_MBUTTONDOWN
		{
			break;
		}

		if (button == Qt::RightButton) // WM_RBUTTONUP, WM_RBUTTONDOWN
		{
			int xp = int(me->position().x() * dpr); // GET_X_LPARAM
			int yp = int(me->position().y() * dpr); // GET_Y_LPARAM
			PickTerrain(bDown ? WM_RBUTTONDOWN : WM_RBUTTONUP, xp, yp);
			break;
		}


		if (button == Qt::LeftButton && bDown) // WM_LBUTTONDOWN
		{
			UINT uMsg = WM_LBUTTONDOWN;
			bTrackMouse = true;
			xpos = int(me->position().x() * dpr); // GET_X_LPARAM
			ypos = int(me->position().y() * dpr); // GET_Y_LPARAM

			GetScene()->vPickRay = GetScene()->GetPickingRay(xpos, ypos);

			// TrackMouseEvent(TME_LEAVE) left out: Qt sends QEvent::Leave without asking

			bool bShift = (me->modifiers() & Qt::ShiftModifier) != 0; // GetAsyncKeyState(VK_SHIFT)
			bool bCtrl = (me->modifiers() & Qt::ControlModifier) != 0; // GetAsyncKeyState(VK_CONTROL)
			bool bPckVsl = IsGenericProcEnabled(GENERICPROC_PICK_VESSEL);

			if (DebugControls::IsActive() || bPckVsl || (bShift && bCtrl)) {
				pick = GetScene()->PickScene(xpos, ypos);
				if (bPckVsl) {
					gcCore::PickData out;
					out.hVessel = pick.vObj ? pick.vObj->GetObject() : NULL; // GetObjectA; upstream dereferenced a NULL vObj when nothing was picked
					out.mesh = MESHHANDLE(pick.pMesh);
					out.group = pick.group;
					out.pos = _FV(pick.pos);
					out.normal = _FV(pick.normal);
					out.dist = pick.dist;
					MakeGenericProcCall(GENERICPROC_PICK_VESSEL, sizeof(gcCore::PickData), &out);
				}
			}

			PickTerrain(uMsg, xpos, ypos);

			// No Debug Controls
			if (bShift && bCtrl && !DebugControls::IsActive() && !oapiCameraInternal()) {

				if (!pick.pMesh) break;

				OBJHANDLE hObj = pick.vObj->Object();
				if (oapiGetObjectType(hObj) == OBJTP_VESSEL) {
					oapiSetFocusObject(hObj);
				}

				break;
			}

			// With Debug Controls
			if (DebugControls::IsActive()) {

				DWORD flags = *(DWORD*)GetConfigParam(CFGPRM_GETDEBUGFLAGS);

				if (flags&DBG_FLAGS_PICK) {

					if (!pick.pMesh) break;

					if (bShift && bCtrl) {
						OBJHANDLE hObj = pick.vObj->Object();
						if (oapiGetObjectType(hObj)==OBJTP_VESSEL) {
							oapiSetFocusObject(hObj);
							break;
						}
					}
					else if (pick.group>=0) {
						DebugControls::SetVisual(pick.vObj);
						DebugControls::SelectMesh(pick.pMesh);
						DebugControls::SelectGroup(pick.group);
						DebugControls::SetGroupHighlight(true);
						DebugControls::SetPickPos(pick.pos);
					}
				}
			}

			break;
		}

		if (button == Qt::LeftButton && !bDown) // WM_LBUTTONUP
		{
			UINT uMsg = WM_LBUTTONUP;
			int xp = int(me->position().x() * dpr); // GET_X_LPARAM
			int yp = int(me->position().y() * dpr); // GET_Y_LPARAM

			PickTerrain(uMsg, xp, yp);

			if (DebugControls::IsActive()) {
				DWORD flags = *(DWORD*)GetConfigParam(CFGPRM_GETDEBUGFLAGS);
				if (flags&DBG_FLAGS_PICK) {
					DebugControls::SetGroupHighlight(false);
				}
			}
			bTrackMouse = false;
			break;
		}
		break;
		}

		case QEvent::KeyPress: // WM_KEYDOWN
		{
			QKeyEvent *ke = static_cast<QKeyEvent*>(event);
			bool bShift = (ke->modifiers() & Qt::ShiftModifier)!=0; // GetAsyncKeyState(VK_SHIFT)
			bool bCtrl  = (ke->modifiers() & Qt::ControlModifier)!=0; // GetAsyncKeyState(VK_CONTROL)
			int wParam = ke->key(); // virtual key: Qt::Key_A..Key_Z are 'A'..'Z'
			if (wParam == 'C' && bShift && bCtrl) bControlPanel = !bControlPanel;
			if (wParam == 'N' && bShift && bCtrl) Config->bCloudNormals = !Config->bCloudNormals;
			if (wParam == 'F' && bShift && bCtrl) {
				if (bFreeze) bFreezeEnable = bFreeze = false;
				else bFreezeEnable = true;
			}
			if (wParam == 'A' && bFreeze) bFreezeRenderAll = !bFreezeRenderAll;

			break;
		}

		case QEvent::Wheel: // WM_MOUSEWHEEL
		{
			if (DebugControls::IsActive()) {
				short d = short(static_cast<QWheelEvent*>(event)->angleDelta().y()); // GET_WHEEL_DELTA_WPARAM
				if (d<-1) d=-1;
				if (d>1) d=1;
				double speed = *(double *)GetConfigParam(CFGPRM_GETCAMERASPEED);
				speed *= (DebugControls::GetVisualSize()/100.0);
				if (scene->CameraPan(_V(0,0,double(d))*2.0, speed)) return true;
			}

			PickTerrain(WM_MOUSEWHEEL, xpos, ypos);
			break;
		}

		case QEvent::MouseMove: // WM_MOUSEMOVE
		{
			int mx = int(me->position().x() * dpr); // GET_X_LPARAM
			int my = int(me->position().y() * dpr); // GET_Y_LPARAM

			if (DebugControls::IsActive())
			{

				double x = double(mx - xpos);
				double y = double(my - ypos);
				xpos = mx;
				ypos = my;

				if (bTrackMouse) {
					double speed = *(double *)GetConfigParam(CFGPRM_GETCAMERASPEED);
					speed *= (DebugControls::GetVisualSize() / 100.0);
					if (scene->CameraPan(_V(-x, y, 0)*0.05, speed)) return true;
				}
			}

			xpos = mx;
			ypos = my;

			PickTerrain(WM_MOUSEMOVE, xpos, ypos);

			break;
		}

		case QEvent::Move: // WM_MOVE
			// If in windowed mode, move the Framework's window
			break;

		// WM_SYSCOMMAND: no Alt menu key or SC_MOVE/SC_SIZE/SC_MAXIMIZE on a Qt window; SC_MONITORPOWER is InhibitScreenSaver

		// WM_SYSKEYUP left out: Alt opens no menu on a Qt window (swallowing the key-up would also hide it from the keyboard device)

		default:
			break;
	}

	// WM_MOUSEFIRST..WM_MOUSELAST (0x0200..0x020E): mouse moves, buttons and wheel
	if (!bRunning && (me || event->type() == QEvent::Wheel)) return true;
	return GraphicsClient::RenderWndProc (hWnd, event);
}


// ==============================================================
// Message handler for Launchpad "video" tab

void D3D9Client::LaunchpadVideoWndProc(QWidget *hWnd)
{
	_TRACE;
	if (vtab) vtab->WndProc(hWnd); // connects the video tab's controls (called once, not per message)
}

// =======================================================================

void D3D9Client::clbkRender2DPanel (SURFHANDLE *hSurf, MESHHANDLE hMesh, MATRIX3 *T, float alpha, bool additive)
{
	_TRACE;

	SURFHANDLE surf = NULL;
	DWORD ngrp = oapiMeshGroupCount(hMesh);

	if (ngrp==0) return;

	float sx = 1.0f/(float)(T->m11),  dx = (float)(T->m13);
	float sy = 1.0f/(float)(T->m22),  dy = (float)(T->m23);
	float vw = (float)viewW;
	float vh = (float)viewH;

	D3DXMATRIX mVP;
	D3DXMatrixOrthoOffCenterRH(&mVP, (0.0f-dx)*sx, (vw-dx)*sx, (vh-dy)*sy, (0.0f-dy)*sy, -100.0f, 100.0f);
	D3D9Effect::SetViewProjMatrix(&mVP);

	for (DWORD i=0;i<ngrp;i++) {

		float scale = 1.0f;

		MESHGROUP *gr = oapiMeshGroup(hMesh, i);

		if (gr->UsrFlag & 2) continue; // skip this group

		DWORD TexIdx = gr->TexIdx;

		if (TexIdx >= TEXIDX_MFD0) {
			int mfdidx = TexIdx - TEXIDX_MFD0;
			surf = GetMFDSurface(mfdidx);
			if (!surf) surf = (SURFHANDLE)pDefaultTex;
		} else if (hSurf) {
			surf = hSurf[TexIdx];
		}
		else surf = oapiGetTextureHandle (hMesh, gr->TexIdx+1);

		for (unsigned int k=0;k<gr->nVtx;k++) gr->Vtx[k].z = 0.0f;

		D3D9Effect::Render2DPanel(gr, SURFACE(surf), &ident, alpha, scale, additive);
	}
}

// =======================================================================

void D3D9Client::clbkRender2DPanel (SURFHANDLE *hSurf, MESHHANDLE hMesh, MATRIX3 *T, bool additive)
{
	_TRACE;
	clbkRender2DPanel (hSurf, hMesh, T, 1.0f, additive);
}

// =======================================================================

DWORD D3D9Client::clbkGetDeviceColour (BYTE r, BYTE g, BYTE b)
{
	_TRACE;
	return ((DWORD)r << 16) + ((DWORD)g << 8) + (DWORD)b;
}



#pragma region Surface, Blitting and Filling Functions



// =======================================================================
// Surface functions
// =======================================================================

bool D3D9Client::clbkSaveSurfaceToImage(SURFHANDLE surf, const char *fname, ImageFileFormat  fmt, float quality)
{
	_TRACE;
	if (ChkDev(__FUNCTION__)) return false;

	if (surf==NULL) surf = pFramework->GetBackBufferHandle();

	// pRTG and pSystem left out: VkReadPixels reads any image back (GetRenderTargetData into system memory)
	VkSurf *pSurf = SURFACE(surf)->GetSurface();

	if (pSurf==NULL) return false;

	bool bRet = false;
	const SurfDesc *desc = SURFACE(surf)->GetDesc();
	VkPixels px, lv, pRect; // D3DLOCKED_RECT: B8G8R8A8 rows

	if (fmt == ImageFileFormat::IMAGE_DDS) {
		char path[MAX_PATH];
		snprintf(path, sizeof(path), "%s.dds", fname);
		return NatSaveSurface(path, pSurf->tex);
	}

	// StretchRect to an X8R8G8B8 target, GetRenderTargetData and LockRect: read back and convert (the D3DPOOL_SYSTEMMEM branch is the same)
	if (VkReadPixels(pDevice, pSurf->tex, px, pSurf->level + 1))
	{
		lv.w = pSurf->w; lv.h = pSurf->h; lv.levels = 1; lv.layers = 1; lv.fmt = px.fmt; lv.swz = desc->Swizzle;
		lv.data.assign(1, px.Level(pSurf->level, pSurf->layer));
		if (VkConvertPixels(lv, pRect, VK_FORMAT_B8G8R8A8_UNORM, SWZ_NONE, lv.w, lv.h, 1))
		{
			if (fname == NULL) {
				// copy device-dependent bitmap to clipboard
				bRet = SaveSurfaceToClipboard(pRect.w, pRect.h, pRect.Level(0).data(), pRect.w * 4);
			} else {
				// save as file
				bRet = SaveSurfaceToFile(pRect.w, pRect.h, pRect.Level(0).data(), pRect.w * 4, fname, fmt, quality);
			}
		}
	}

	return bRet;
}

// ==============================================================

bool oapi::D3D9Client::SaveSurfaceToFile (UINT w, UINT h, const BYTE *pBits, UINT pitch,
                                          const char* fname, oapi::ImageFileFormat fmt, float quality)
{
	bool bRet = false;
	ImageData ID;

	ID.bpp = 24;
	ID.height = h;
	ID.width = w;
	ID.stride = ((ID.width * ID.bpp + 31) & ~31) >> 3;
	ID.bufsize = ID.stride * ID.height;

	BYTE* tgt = ID.data = new BYTE[ID.bufsize];
	const BYTE* src = pBits;

	for (DWORD k = 0; k<h; k++) {
		for (DWORD i = 0; i<w; i++) {
			tgt[0 + i * 3] = src[0 + i * 4];
			tgt[1 + i * 3] = src[1 + i * 4];
			tgt[2 + i * 3] = src[2 + i * 4];
		}
		tgt += ID.stride;
		src += pitch;
	}

	bRet = WriteImageDataToFile(ID, fname, fmt, quality);

	delete[]ID.data;
	ID.data = NULL;

	return bRet;
}

// ==============================================================

bool oapi::D3D9Client::SaveSurfaceToClipboard (UINT w, UINT h, const BYTE *pBits, UINT pitch)
{
	QClipboard *cb = QGuiApplication::clipboard(); // OpenClipboard(hRenderWnd)
	if (cb)
	{
		// window DC BitBlt: the surface rows read back (the window shows the presented backbuffer)
		QImage hBm = QImage(pBits, w, h, pitch, QImage::Format_ARGB32).convertToFormat(QImage::Format_RGB32);

		cb->setImage(hBm); // EmptyClipboard, SetClipboardData(CF_BITMAP), CloseClipboard
		return true;
	}
	return false;
}

// ==============================================================

SURFHANDLE D3D9Client::clbkLoadTexture(const char *fname, DWORD flags)
{
	_TRACE;
	if (ChkDev(__FUNCTION__)) return NULL;

	DWORD attrib = OAPISURFACE_TEXTURE;
	if (flags & 0x1) attrib |= OAPISURFACE_SYSMEM;
	if (flags & 0x2) attrib |= OAPISURFACE_UNCOMPRESS | OAPISURFACE_RENDERTARGET;
	if (flags & 0x4) attrib |= OAPISURFACE_NOMIPMAPS;
	if (flags & 0x8) attrib |= OAPISURFACE_SHARED;

	return clbkLoadSurface(fname, attrib);
}

// ==============================================================

SURFHANDLE D3D9Client::clbkLoadSurface (const char *fname, DWORD attrib, bool bPath)
{
	_TRACE;
	if (ChkDev(__FUNCTION__)) return NULL;

	static const DWORD val = OAPISURFACE_RENDERTARGET | OAPISURFACE_GDI | OAPISURFACE_SYSMEM;
	static const DWORD exclude = ~(OAPISURFACE_SHARED | OAPISURFACE_ORIGIN);

	if (!(attrib & val))
	{
		// It's a regular texture, let's manage it
		//
		string name(fname);

		if (attrib & OAPISURFACE_SHARED)
		{
			auto ent = SharedTextures.find(name);

			if (ent == SharedTextures.end())
			{
				SURFHANDLE hSrf = NatLoadSurface(fname, attrib, bPath);
				if (hSrf) SharedTextures[name] = hSrf;
				return hSrf;
			}
			else return ent->second;
		}

		/*
		auto ent = ClonedTextures.find(name);

		if (ent == ClonedTextures.end())
		{
			SURFHANDLE hSrf = NatLoadSurface(fname, attrib);
			if (hSrf) SharedTextures[name] = hSrf;
			return hSrf;
		}
		else
		{
			DWORD original = SURFACE(ent->second)->GetOAPIFlags();

			if (original & OAPISURFACE_ORIGIN)
			{
				if ((attrib & exclude) == (original & exclude))
				{
					return SURFHANDLE(new SurfNative(SURFACE(ent->second))); // Clone it
				}
			}
			else
			{
				// Create "origin" for cloning
				SURFHANDLE hSrf = NatLoadSurface(fname, attrib | OAPISURFACE_ORIGIN);
				if (hSrf) {
					ent->second = hSrf;
					return SURFHANDLE(new SurfNative(SURFACE(hSrf))); // Clone it
				}
				else return NULL;
			}
		}
		*/
	}
	
	return NatLoadSurface(fname, attrib, bPath);
}

// ==============================================================

QImage *D3D9Client::gcReadImageFromFile(const char *_path)
{
	char path[MAX_PATH];
	snprintf(path, sizeof(path), "%s/%s", OapiExtension::GetTextureDir(), _path);
	return ReadImageFromFile(path);
}

// ==============================================================

void D3D9Client::clbkReleaseTexture(SURFHANDLE hTex)
{
	clbkReleaseSurface(hTex);
}

// ==============================================================

SURFHANDLE D3D9Client::clbkCreateSurfaceEx(DWORD w, DWORD h, DWORD attrib)
{
	_TRACE;
	if (ChkDev(__FUNCTION__)) return NULL;

#ifdef _DEBUG
	LogAttribs(attrib, w, h, "CreateSrfEx");
#endif // _DEBUG

	if (w == 0 || h == 0) return NULL;	// Inline engine returns NULL for a zero surface

	SURFHANDLE hNew = NatCreateSurface(w, h, attrib);
	SURFACE(hNew)->SetName("clbkCreateSurfaceEx");
	return hNew;
}


// =======================================================================

SURFHANDLE D3D9Client::clbkCreateSurface(DWORD w, DWORD h, SURFHANDLE hTemplate)
{
	_TRACE;
	if (ChkDev(__FUNCTION__)) return NULL;
	if (w == 0 || h == 0) return NULL;	// Inline engine returns NULL for a zero surface

	DWORD attrib = OAPISURFACE_PF_XRGB | OAPISURFACE_RENDERTARGET | OAPISURFACE_TEXTURE;

	if (hTemplate) attrib = SURFACE(hTemplate)->GetOAPIFlags();
	
	SURFHANDLE hNew = NatCreateSurface(w, h, attrib);
	SURFACE(hNew)->SetName("clbkCreateSurface");
	return hNew;
}

// =======================================================================

SURFHANDLE D3D9Client::clbkCreateSurface(QImage *hBmp)
{
	_TRACE;
	if (ChkDev(__FUNCTION__)) return NULL;

	SURFHANDLE hSurf = GraphicsClient::clbkCreateSurface(hBmp);
	SURFACE(hSurf)->SetName("clbkCreateSurface_HBITMAP");
	return hSurf;
}

// =======================================================================

SURFHANDLE D3D9Client::clbkCreateTexture(DWORD w, DWORD h)
{
	_TRACE;
	if (ChkDev(__FUNCTION__)) return NULL;
	if (w == 0 || h == 0) return NULL;	// Inline engine returns NULL for a zero surface

	SURFHANDLE hNew = NatCreateSurface(w, h, OAPISURFACE_PF_XRGB | OAPISURFACE_RENDERTARGET | OAPISURFACE_TEXTURE);
	SURFACE(hNew)->SetName("clbkCreateTexture");
	return hNew;
}

// =======================================================================

void D3D9Client::clbkIncrSurfaceRef(SURFHANDLE surf)
{
	_TRACE;
	if (surf) SURFACE(surf)->IncRef();
}

// =======================================================================

bool D3D9Client::clbkReleaseSurface(SURFHANDLE surf)
{
	_TRACE;
	if (ChkDev(__FUNCTION__)) return false;

	// Do not release 'origin' (i.e. reference) for cloned surfaces.
	if (SURFACE(surf)->GetOAPIFlags() & OAPISURFACE_ORIGIN) return false;

	// Do not release surfaces stored in repository
	for (auto ent : SharedTextures) if (ent.second == surf) return false;

	// Don't release surfaces used by meshes
	for (auto mesh : MeshCatalog) if (mesh && mesh->HasTexture(surf)) return false;

	// If the surface exists, delete it.
	if (SURFACE(surf)->DecRef())
	{
		if (SurfaceCatalog.count(SURFACE(surf)))
		{
			delete SURFACE(surf);
			return true;
		}
	}
	return false;
}

// =======================================================================

bool D3D9Client::clbkGetSurfaceSize(SURFHANDLE surf, DWORD *w, DWORD *h)
{
	_TRACE;
	if (!w || !h) return false;
	if (surf==NULL) surf = pFramework->GetBackBufferHandle();
	*w = SURFACE(surf)->GetWidth();
	*h = SURFACE(surf)->GetHeight();
	return true;
}

// =======================================================================

bool D3D9Client::clbkSetSurfaceColourKey(SURFHANDLE surf, DWORD ckey)
{
	_TRACE;
	if (surf==NULL) { LogErr("Surface is NULL"); return false; }
	SURFACE(surf)->SetColorKey(ckey);
	return true;
}



// =======================================================================
// Blitting functions
// =======================================================================

int D3D9Client::clbkBeginBltGroup(SURFHANDLE tgt)
{
	_TRACE;
	if (pBltGrpTgt) return -1;

	if (tgt == RENDERTGT_NONE) {
		pBltGrpTgt = NULL;
		return -2;
	}

	if (tgt == RENDERTGT_MAINWINDOW) pBltGrpTgt = pFramework->GetBackBufferHandle();
	else pBltGrpTgt = tgt;

	if (!SURFACE(tgt)->IsRenderTarget()) {
		pBltGrpTgt = NULL;
		return -3;
	}

	//pBltSkp = SURFACE(tgt)->GetPooledSketchPad();
	//pBltSkp->BeginDrawing();
	return 0;
}

// =======================================================================

int D3D9Client::clbkEndBltGroup()
{
	_TRACE;
	if (pBltGrpTgt==NULL) return -2;
	//pBltSkp->EndDrawing();
	pBltSkp = NULL;
	pBltGrpTgt = NULL;
	return 0;
}

// =======================================================================

bool D3D9Client::clbkBlt(SURFHANDLE tgt, DWORD tgtx, DWORD tgty, SURFHANDLE src, DWORD flag) const
{
	_TRACE;
	const SurfDesc* sd = SURFACE(src)->GetDesc();
	return clbkScaleBlt(tgt, tgtx, tgty, sd->Width, sd->Height, src, 0, 0, sd->Width, sd->Height, flag);
}

// =======================================================================

bool D3D9Client::clbkBlt(SURFHANDLE tgt, DWORD tgtx, DWORD tgty, SURFHANDLE src, DWORD srcx, DWORD srcy, DWORD w, DWORD h, DWORD flag) const
{
	_TRACE;
	return clbkScaleBlt(tgt, tgtx, tgty, w, h, src, srcx, srcy, w, h, flag);
}

// =======================================================================

bool D3D9Client::clbkScaleBlt (SURFHANDLE tgt, DWORD tgtx, DWORD tgty, DWORD tgtw, DWORD tgth,
                               SURFHANDLE src, DWORD srcx, DWORD srcy, DWORD srcw, DWORD srch, DWORD flag) const
{
	
	if (src==NULL) { oapiWriteLog((char*)"ERROR: oapiBlt() Source surface is NULL"); return false; }

	if (tgt==NULL) tgt = pFramework->GetBackBufferHandle();


	RECT rs = _RECT(srcx, srcy, srcx + srcw, srcy + srch);
	RECT rt = _RECT(tgtx, tgty, tgtx + tgtw, tgty + tgth);


	// Can't blit in a clone, declone..
	//
	if (SURFACE(tgt)->IsClone()) SURFACE(tgt)->DeClone();

	// Can't blit in a compressed surface, decompress..
	//
	if (!SURFACE(tgt)->Decompress())
	{
		HALT();
	}

	POINT tp = { (LONG)tgtx, (LONG)tgty };

	const SurfDesc* td = SURFACE(tgt)->GetDesc();
	const SurfDesc* sd = SURFACE(src)->GetDesc();


	// Check failure and abort conditions, Match with know DX7 behavior ---------------------
	//
	if (rt.right > (long)td->Width || rt.bottom > (long)td->Height) return true;
	if (rt.left < 0 || rt.top < 0)  return true;

	if (rs.right > (long)sd->Width || rs.bottom > (long)sd->Height)  return true;
	if (rs.left < 0 || rs.top < 0) return true;

	if (rs.left > rs.right) return true;
	if (rt.left > rt.right) return true;
	if (rs.top > rs.bottom) return true;
	if (rt.top > rt.bottom) return true;

	if (srcw == 0 || srch == 0 || tgtw == 0 || tgth == 0) return true;


	// Check Blt conditions
	//
	bool bCK = (SURFACE(src)->GetColorKey() != SURF_NO_CK) && (SURFACE(src)->GetColorKey() != 0);	// ColorKey In Use
	bool bCL = (srcw != tgtw) || (srch != tgth);		// Scaling In Use
	bool bSC = SURFACE(src)->IsCompressed();			// Compressed source

	VkSurf *pss = SURFACE(src)->GetSurface();
	VkSurf *pts = SURFACE(tgt)->GetSurface();

	bool bSF = (sd->Format == td->Format) && (sd->Swizzle == td->Swizzle); // not upstream: a D3DFORMAT is the Vulkan format and the view swizzle

	if (bSF && !bCK && !bSC)
	{

		// Most common case: Target is a render-target and source is in a video memory
		// (StretchRect has no failure result: the "Failed 1/2" error paths are left out)
		if (td->RenderTarget && !sd->SysMem)
		{
			if (src != tgt)
			{
				pDevice->StretchRect(pss, &rs, pts, &rt, VK_FILTER_NEAREST); // D3DTEXF_POINT
				return true;
			}
			else
			{
				// Source and Target are the same surface, reroute through temp.
				//
				VkSurf *tmp = SURFACE(src)->GetTempSurface();

				pDevice->StretchRect(pss, &rs, tmp, &rs, VK_FILTER_NEAREST);
				pDevice->StretchRect(tmp, &rs, pts, &rt, VK_FILTER_NEAREST);
				return true;
			}
		}
	}

	if (bSF && !bCK && !bSC && !bCL)
	{

		// Texture Update: Source is in system memory and target is a texture
		// (CopySurface has no failure result: the error paths are left out)
		if (sd->SysMem)
		{
			pDevice->CopySurface(pss, &rs, pts, &tp); // UpdateSurface
			return true;
		}


		// Screen Capture: Target is in system memory and source is a render taeget
		// 
		if (td->SysMem && sd->RenderTarget)
		{
			pDevice->CopySurface(pss, NULL, pts, NULL); // GetRenderTargetData
			SURFACE(tgt)->Flags |= OAPISURFACE_CAPTURE;
			return true;
		}
	}


	// Scaling.. Format mismatch.. ColorKey.. Compressed Source..
	// Go for SketchPad
	//
	if (src != tgt)
	{
		if (td->RenderTarget && (SURFACE(src)->GetType() == NATTYPE_TEXTURE) && !sd->SysMem)
		{
			Sketchpad* pSkp = clbkGetSketchpad_const(tgt);

			if (bCK)
			{
				if (bCL) ((D3D9Pad*)pSkp)->ColorKeyStretch(src, &rs, &rt);
				else pSkp->ColorKey(src, &rs, tgtx, tgty);
				clbkReleaseSketchpad_const(pSkp);
				return true;
			}
			else
			{		
				pSkp->StretchRect(src, &rs, &rt);
				clbkReleaseSketchpad_const(pSkp);
				return true;
			}
		}
	}

	LogErr("oapiBlt() Failed (End)");
	BltError(src, tgt, &rs, &rt);
	return false;
}

// =======================================================================

bool D3D9Client::clbkCopyBitmap(SURFHANDLE pdds, QImage *hbm, int x, int y, int dx, int dy)
{
	QPainter *              hdc;

	if (hbm == NULL || pdds == NULL) return false;

	// memory DC (CreateCompatibleDC, SelectObject) left out: QPainter draws from the QImage directly

	// Get size of the bitmap
	//
	dx = dx == 0 ? hbm->width() : dx;     // Use the passed size, unless zero
	dy = dy == 0 ? hbm->height() : dy;


	// Get size of surface.
	//
	DWORD surfW = SURFACE(pdds)->GetWidth();
	DWORD surfH = SURFACE(pdds)->GetHeight();

	if (SURFACE(pdds)->IsGDISurface())
	{
		if ((hdc = clbkGetSurfaceDC(pdds))) {
			hdc->save();
			hdc->setCompositionMode(QPainter::CompositionMode_Source); // SRCCOPY
			hdc->drawImage(QRect(0, 0, surfW, surfH), *hbm, QRect(x, y, dx, dy)); // StretchBlt
			hdc->restore();
			clbkReleaseSurfaceDC(pdds, hdc);
		}
		SURFACE(pdds)->SetName("clbkCopyBitmap");
		return true;
	}
	else 
	{
		// GetGDICache and the StretchRect/UpdateSurface copy back left out: GetDC paints on a copy of any surface and uploads it on release
		if ((hdc = clbkGetSurfaceDC(pdds)))
		{
			hdc->save();
			hdc->setCompositionMode(QPainter::CompositionMode_Source); // SRCCOPY
			hdc->drawImage(QRect(0, 0, surfW, surfH), *hbm, QRect(x, y, dx, dy)); // StretchBlt
			hdc->restore();

			clbkReleaseSurfaceDC(pdds, hdc);

			SURFACE(pdds)->SetName("clbkCopyBitmap");
			return true;
		}
	}
	return false;
}

// =======================================================================

bool D3D9Client::clbkFillSurface(SURFHANDLE tgt, DWORD col) const
{
	_TRACE;
	if (tgt==NULL) tgt = pFramework->GetBackBufferHandle();
	bool ret = SURFACE(tgt)->Fill(NULL, col);
	return ret;
}

// =======================================================================

bool D3D9Client::clbkFillSurface(SURFHANDLE tgt, DWORD tgtx, DWORD tgty, DWORD w, DWORD h, DWORD col) const
{
	_TRACE;
	if (tgt==NULL) tgt = pFramework->GetBackBufferHandle();
	RECT r = _RECT(tgtx, tgty, tgtx+w, tgty+h);
	bool ret = SURFACE(tgt)->Fill(&r, col);
	return ret;
}

// =======================================================================

void D3D9Client::BltError(SURFHANDLE src, SURFHANDLE tgt, const LPRECT s, const LPRECT t, bool bHalt) const
{
	LogErr("Source Rect (%d,%d,%d,%d) (w=%u,h=%u)", s->left, s->top, s->right, s->bottom, abs(s->left - s->right), abs(s->top - s->bottom));
	LogErr("Target Rect (%d,%d,%d,%d) (w=%u,h=%u)", t->left, t->top, t->right, t->bottom, abs(t->left - t->right), abs(t->top - t->bottom));
	LogErr("Source Data Below: ----------------------------------");
	SURFACE(src)->LogSpecs();
	LogErr("Target Data Below: ----------------------------------");
	SURFACE(tgt)->LogSpecs();
	if (bHalt) HALT();
}



#pragma endregion

// =======================================================================
// GDI functions
// =======================================================================

QPainter *D3D9Client::clbkGetSurfaceDC(SURFHANDLE surf)
{
	_TRACE;
	if (ChkDev(__FUNCTION__)) return NULL;

	if (surf == NULL) {
		if (Config->GDIOverlay) {
			VkSurf *pGDI = GetScene()->GetBuffer(GBUF_GDI);
			QPainter *hDC;
			if (pGDI) if ((hDC = SurfaceDC(pDevice, pGDI, GDIImage))) { // GetDC
				if (bGDIClear) {
					bGDIClear = false;
					DWORD color = 0xF08040; // BGR "Color Key" value for transparency
					RECT r = _RECT( 0, 0, viewW, viewH );
					hDC->fillRect(r.left, r.top, r.right - r.left, r.bottom - r.top, QColor(GetRValue(color), GetGValue(color), GetBValue(color))); // CreateSolidBrush, FillRect
				}
				return hDC;
			}
		}
		return NULL;
	}
	QPainter *hDC = SURFACE(surf)->GetDC();
	return hDC;
}

// =======================================================================

void D3D9Client::clbkReleaseSurfaceDC(SURFHANDLE surf, QPainter *hDC)
{
	_TRACE;
	if (ChkDev(__FUNCTION__)) return;

	if (hDC == NULL) { LogErr("D3D9Client::clbkReleaseSurfaceDC() Input hDC is NULL"); return; }
	if (surf == NULL) {
		if (Config->GDIOverlay) {
			VkSurf *pGDI = GetScene()->GetBuffer(GBUF_GDI);
			if (pGDI) SurfaceReleaseDC(pGDI, hDC, GDIImage); // ReleaseDC
		}
		return;
	}
	SURFACE(surf)->ReleaseDC(hDC);
}

// =======================================================================

bool D3D9Client::clbkFilterElevation(OBJHANDLE hPlanet, int ilat, int ilng, int lvl, double elev_res, INT16* elev)
{
	_TRACE;
	return FilterElevationPhysics(hPlanet, lvl, ilat, ilng, elev_res, elev);
}
void D3D9Client::clbkImGuiNewFrame()
{
	_TRACE;
	ImGui_ImplVulkan_NewFrame(); // ImGui_ImplDX9_NewFrame
}
void D3D9Client::clbkImGuiRenderDrawData()
{
	_TRACE;

	// BeginScene: nothing to begin; ImGui's pipeline draws on the backbuffer without a depth buffer
	{
		VkSurf *pRT = pDevice->GetRenderTarget(), *pDS = pDevice->GetDepthStencil(); // not upstream: kept as the DX9 backend's state block did
		ImGui::Render();
		for (auto &surf : ImTextures) pDevice->PrepareSample(SURFACE(surf)->GetTexture()); // not upstream: sampled layout before the pass
		pDevice->SetRenderTarget(pBackBuffer, NULL);
		pDevice->BeginRendering();
		{
			std::lock_guard<std::mutex> lock(pDevice->QueueLock()); // not upstream: the backend's texture uploads submit to the device queue
			ImGui_ImplVulkan_RenderDrawData(ImGui::GetDrawData(), pDevice->Cmd()); // ImGui_ImplDX9_RenderDrawData
		}
		pDevice->EndRendering(); // EndScene
		pDevice->SetState(pDevice->GetState()); // not upstream: ImGui's pipeline replaced the shader objects and dynamic state
		pDevice->SetRenderTarget(pRT, pDS);
	}

	// Update and Render additional Platform Windows
	ImGuiIO& io = ImGui::GetIO();
	if (io.ConfigFlags & ImGuiConfigFlags_ViewportsEnable)
	{
		ImGui::UpdatePlatformWindows();
		ImGui::RenderPlatformWindowsDefault();
	}

	// Release textures that were protected for ImGui usage during the frame
	for(auto &surf: ImTextures) {
		clbkReleaseSurface(surf);
	}
	ImTextures.clear();
}
void D3D9Client::clbkImGuiInit()
{
	_TRACE;
	// ImGui_ImplDX9_Init(pDevice): the Vulkan backend takes the device objects and the backbuffer format (dynamic rendering)
	ImGui_ImplVulkan_InitInfo info = {};
	info.ApiVersion = VK_API_VERSION_1_4;
	info.Instance = pDevice->instance;
	info.PhysicalDevice = pDevice->phys;
	info.Device = pDevice->dev;
	info.QueueFamily = pDevice->queueFamily;
	info.Queue = pDevice->queue;
	info.DescriptorPoolSize = 1024; // sets for the font atlas and clbkImGuiSurfaceTexture
	info.MinImageCount = 2;
	info.ImageCount = VkDev::NFRAMES + 1; // vertex buffers are rewritten only after the frames in flight are done
	info.UseDynamicRendering = true;
	info.PipelineInfoMain.MSAASamples = pBackBuffer->tex->samples;
	info.PipelineInfoMain.PipelineRenderingCreateInfo = { VK_STRUCTURE_TYPE_PIPELINE_RENDERING_CREATE_INFO };
	info.PipelineInfoMain.PipelineRenderingCreateInfo.colorAttachmentCount = 1;
	info.PipelineInfoMain.PipelineRenderingCreateInfo.pColorAttachmentFormats = &pBackBuffer->tex->fmt;
	ImGui_ImplVulkan_Init(&info);
	VkTex::uiRelease = ReleaseImSet; // not upstream: a texture's ImGui descriptor set goes with the texture
}
void D3D9Client::clbkImGuiShutdown()
{
	_TRACE;
	// Clean up also here just in case
	for(auto &surf: ImTextures) {
		clbkReleaseSurface(surf);
	}
	ImTextures.clear();
	ImGuiGeneration++; // not upstream: the textures' descriptor sets are freed with the backend's pool
	pDevice->Flush(); // not upstream: the recorded ImGui draws run before the backend frees its pipeline and buffers
	ImGui_ImplVulkan_Shutdown(); // ImGui_ImplDX9_Shutdown
}
uint64_t D3D9Client::clbkImGuiSurfaceTexture(SURFHANDLE surf)
{
	ImTextures.push_back(surf);
	clbkIncrSurfaceRef(surf);
	VkTex *pTxt = SURFACE(surf)->GetTexture();
	if (!pTxt) return 0; // not upstream: no descriptor set without a texture (upstream passed the NULL pointer on)
	// not upstream: one descriptor set per texture and view, valid as long as the texture (callers keep the ID, as they kept the D3D9 pointer)
	if (pTxt->uiSet && pTxt->uiView == pTxt->view && pTxt->uiGen == ImGuiGeneration) return pTxt->uiSet;
	if (pTxt->uiSet) {
		uint64_t s = pTxt->uiSet;
		DWORD g = pTxt->uiGen;
		pDevice->Defer([s, g]() { ReleaseImSet(s, g); });
	}
	VkSamplerDesc sd = { VK_FILTER_LINEAR, VK_FILTER_LINEAR, VK_SAMPLER_MIPMAP_MODE_NEAREST, VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE,
		VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE, VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE, 0.0f, 0.0f, true }; // the DX9 backend's sampler states
	VkDescriptorSet ds = ImGui_ImplVulkan_AddTexture(pDevice->Sampler(sd), pTxt->view, VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL);
	pTxt->uiSet = (uint64_t)ds;
	pTxt->uiView = pTxt->view;
	pTxt->uiGen = ImGuiGeneration;
	return (uint64_t)ds; // ImTextureID: the descriptor set (the D3D9 texture pointer upstream)
}
// =======================================================================

bool D3D9Client::clbkSplashLoadMsg (const char *msg, int line)
{
	_TRACE;
	return OutputLoadStatus (msg, line);
}

// =======================================================================

lpSurfNative D3D9Client::GetDefaultTexture() const
{
	return pDefaultTex;
}

// =======================================================================

QWindow *D3D9Client::GetWindow()
{
	return pFramework->GetRenderWindow();
}

// =======================================================================

SURFHANDLE D3D9Client::GetBackBufferHandle() const
{
	_TRACE;
	return pFramework->GetBackBufferHandle();
}

// =======================================================================

void D3D9Client::MakeRenderProcCall(Sketchpad *pSkp, DWORD id, LPD3DXMATRIX pV, LPD3DXMATRIX pP)
{
	for (auto it = RenderProcs.cbegin(); it != RenderProcs.cend(); ++it) {
		if (it->id == id) {
			D3D9Pad *pSkp2 = (D3D9Pad *)pSkp;
			pSkp2->LoadDefaults();
			if (id == RENDERPROC_EXTERIOR || id == RENDERPROC_PLANETARIUM) {
				pSkp2->SetViewMode(Sketchpad::USER);
			}
			pSkp2->SetViewProj(pV, pP);
			it->proc(pSkp, it->pParam);
			pSkp2->FlushAll(); // Flush render queue
		}
	}
}

// =======================================================================

void D3D9Client::MakeGenericProcCall(DWORD id, int iUser, void *pUser) const
{
	for (auto it = GenericProcs.cbegin(); it != GenericProcs.cend(); ++it) {
		if (it->id == id) it->proc(iUser, pUser, it->pParam);
	}
}

// =======================================================================

bool D3D9Client::RegisterRenderProc(__gcRenderProc proc, DWORD id, void *pParam)
{
	if (id)	{ // register (add)
		RenderProcData data = { proc, pParam, id };
		RenderProcs.push_back(data);
		return true;
	}
	else { // unregister, mark as unused (remove later)
		for (auto it = RenderProcs.begin(); it != RenderProcs.end(); ++it) {
			if (it->proc == proc) {
				it->id = 0;
				it->pParam = NULL;
				it->proc = NULL;
				return true;
			}
		}
	}
	return false;
}

// =======================================================================

bool D3D9Client::RegisterGenericProc(__gcGenericProc proc, DWORD id, void *pParam)
{
	if (id) { // register (add)
		GenericProcData data = { proc, pParam, id };
		GenericProcs.push_back(data);
		return true;
	}
	else { // unregister, mark as unused (remove later)
		for (auto it = GenericProcs.begin(); it != GenericProcs.end(); ++it) {
			if (it->proc == proc) {
				it->id = 0;
				it->pParam = NULL;
				it->proc = NULL;
				return true;
			}
		}
	}
	return false;
}

// =======================================================================

bool D3D9Client::IsGenericProcEnabled(DWORD id) const
{
	for (const auto &val : GenericProcs) if (val.id == id) return true;
	return false;
}

// =======================================================================

void D3D9Client::WriteLog(const char *msg) const
{
	_TRACE;
	char cbuf[256];
	snprintf(cbuf, 256, "D3D9: %s", msg);
	oapiWriteLog(cbuf);
}

// =======================================================================

bool D3D9Client::OutputLoadStatus(const char *txt, int line)
{

	if (bRunning) return false;

	if (line == 1) snprintf(pLoadItem, 127, "%s", txt); else
	if (line == 0) snprintf(pLoadLabel, 127, "%s", txt), pLoadItem[0] = '\0'; // New top line => clear 2nd line

	if (pTextScreen) {

		// TestCooperativeLevel left out: Vulkan has no lost device state to test

		RECT txt = _RECT( loadd_x, loadd_y, loadd_x+loadd_w, loadd_y+loadd_h );

		pDevice->StretchRect(pSplashScreen, &txt, pTextScreen, NULL, VK_FILTER_NEAREST); // D3DTEXF_POINT

		QImage img;
		QPainter *hDC = SurfaceDC(pDevice, pTextScreen, img); // GetDC
		if (!hDC) { LogErr("GetDC() Failed"); return false; }

		hDC->setFont(*hLblFont1); // SelectObject
		hDC->setPen(QColor(GetRValue(pSplashTextColor), GetGValue(pSplashTextColor), GetBValue(pSplashTextColor))); // SetTextColor
		// SetBkMode(TRANSPARENT) and SetTextAlign(TA_LEFT|TA_TOP): QPainter text has no background, TextOut takes the top edge

		TextOut(hDC, 2, 2, pLoadLabel, strlen(pLoadLabel));

		hDC->setFont(*hLblFont2); // SelectObject
		TextOut(hDC, 2, 36, pLoadItem, strlen(pLoadItem));

		QPen pen(QColor(GetRValue(pSplashTextColor), GetGValue(pSplashTextColor), GetBValue(pSplashTextColor)), 1); // CreatePen(PS_SOLID,1,pSplashTextColor)
		hDC->setPen(pen); // SelectObject

		hDC->drawLine(0, 32, loadd_w - 1, 32); // MoveToEx, LineTo (GDI leaves out the end point)

		// SelectObject(po, hO) and DeleteObject(pen) left out: the painter holds its pen and font by value

		SurfaceReleaseDC(pTextScreen, hDC, img); // ReleaseDC
		pDevice->StretchRect(pSplashScreen, NULL, pBackBuffer, NULL, VK_FILTER_NEAREST);
		pDevice->StretchRect(pTextScreen, NULL, pBackBuffer, &txt, VK_FILTER_NEAREST);

		// GetSwapChain(0), Present(D3DPRESENT_DONOTWAIT)
		if (pFramework->Present() == 0) {
			return true;
		}

		// Prevent "Not Responding" during loading
		QCoreApplication::processEvents(); // PeekMessage, DispatchMessage
	}
	return false;
}

// =======================================================================
void D3D9Client::clbkSetSplashScreen(const char *filename, DWORD textCol)
{
	pCustomSplashScreen = filename;
	pSplashTextColor = textCol;
}

void D3D9Client::SplashScreen()
{

	loadd_x = 279*viewW/1280;
	loadd_y = 545*viewH/800;
	loadd_w = viewW/3;
	loadd_h = 80;

	QRect g = hRenderWnd->frameGeometry(); // GetWindowRect
	RECT rS = { g.left(), g.top(), g.left() + g.width(), g.top() + g.height() };

	LogAlw("Splash Window Size = [%u, %u]", rS.right - rS.left, rS.bottom - rS.top);
	LogAlw("Splash Window LeftTop = [%d, %d]", rS.left, rS.top);

	// TestCooperativeLevel left out: Vulkan has no lost device state to test
	pDevice->Clear(true, true, true, 0x0, 1.0f, 0L); // D3DCLEAR_TARGET|D3DCLEAR_ZBUFFER|D3DCLEAR_STENCIL
	const VkImageUsageFlags u = VK_IMAGE_USAGE_SAMPLED_BIT | VK_IMAGE_USAGE_TRANSFER_SRC_BIT | VK_IMAGE_USAGE_TRANSFER_DST_BIT; // offscreen plain: blits, GetDC
	pTextScreen = new VkSurf(pDevice, loadd_w, loadd_h, VK_FORMAT_B8G8R8A8_UNORM, u); // CreateOffscreenPlainSurface(D3DFMT_X8R8G8B8, D3DPOOL_DEFAULT)
	pSplashScreen = new VkSurf(pDevice, viewW, viewH, VK_FORMAT_B8G8R8A8_UNORM, u);


	if(pCustomSplashScreen != NULL) {
		VkImageInfo info = {}; // D3DXIMAGE_INFO
		std::string file = oapiResolvePath(pCustomSplashScreen);
		HR(VkGetImageInfoFromFile(file.c_str(), &info) ? 0 : -1); // D3DXGetImageInfoFromFile

		double imageW = info.Width;
		double imageH = info.Height;

		double scale = min(viewW / imageW, viewH / imageH);
		double _w = (imageW * scale);
		double _h = (imageH * scale);
		double _l = abs(viewW - _w)/2.0;
		double _t = abs(viewH - _h)/2.0;
		RECT imgRect = {
			static_cast<LONG>( round(_l) ),
			static_cast<LONG>( round(_t) ),
			static_cast<LONG>( round(_w + _l) ),
			static_cast<LONG>( round(_h + _t) )
		};
		pDevice->ColorFill(pSplashScreen, NULL, D3DCOLOR_XRGB(0, 0, 0));
		VkPixels px;
		HR(VkLoadPixels(file.c_str(), px) && LoadSurfaceRect(pDevice, pSplashScreen, &imgRect, px) ? 0 : -1); // D3DXLoadSurfaceFromFile(D3DX_FILTER_LINEAR)
	} else {
		VkPixels Info; // D3DXIMAGE_INFO: the decoded image
		const RESDATA *hRes = oapiFindResData(OrbiterInstance(), "IMAGE", 292); // GetModuleHandleA("orbiter.exe"), FindResourceA
		const void *pData = hRes ? hRes->data : NULL; // LoadResource, LockResource
		DWORD size = hRes ? hRes->size : 0; // SizeofResource

		// Splash screen image is 1920 x 1200 pixel
		double scale = min(viewW / 1920.0, viewH / 1200.0);
		double _w = (1920.0 * scale);
		double _h = (1200.0 * scale);
		double _l = abs(viewW - _w)/2.0;
		double _t = abs(viewH - _h)/2.0;
		RECT imgRect = {
			static_cast<LONG>( round(_l) ),
			static_cast<LONG>( round(_t) ),
			static_cast<LONG>( round(_w + _l) ),
			static_cast<LONG>( round(_h + _t) )
		};
		pDevice->ColorFill(pSplashScreen, NULL, D3DCOLOR_XRGB(0, 0, 0));
		HR(pData && VkLoadPixelsFromMemory((const BYTE *)pData, size, Info) && LoadSurfaceRect(pDevice, pSplashScreen, &imgRect, Info) ? 0 : -1); // D3DXLoadSurfaceFromFileInMemory(D3DX_FILTER_LINEAR)
	}

	QImage img;
	QPainter *hDC = SurfaceDC(pDevice, pSplashScreen, img); // GetDC
	if (!hDC) { LogErr("GetDC() Failed"); return; }

	// LOGFONTA: 18, weight 700, ANSI_CHARSET, ANTIALIASED_QUALITY, DEFAULT_PITCH, "Courier New"
	QFont *hF = CreateGDIFont(18, 700, "Courier New"); // CreateFontIndirect

	hDC->setFont(*hF); // SelectObject
	hDC->setPen(QColor(GetRValue(pSplashTextColor), GetGValue(pSplashTextColor), GetBValue(pSplashTextColor))); // SetTextColor
	// SetBkMode(TRANSPARENT): QPainter text has no background

	const char *months[]={"???","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","???"};

	DWORD d = oapiGetOrbiterVersion();
	DWORD y = d/10000; d-=y*10000;
	DWORD m = d/100; d-=m*100;
	if (m>12) m=0;

	char dataA[256];
	strcpy(dataA, "VulkanClient");
	// " via D3D9on12 emulator" left out: no D3D9on12 on Linux

#ifdef _DEBUG
	strcat(dataA, " (Debug Build)");
#else
	strcat(dataA, " (Release Build)");
#endif

	char dataB[128]; snprintf(dataB,128,"Build %s %u 20%u [%u]", months[m], d, y, oapiGetOrbiterVersion());
	//char dataE[] = { "Note: Cubic Interpolation is use... Consider using linear for better elevation matching" };
	//char dataF[] = { "Note: Terrain flattening offline due to cubic interpolation" };

	int xc = viewW*750/1280;
	int yc = viewH*545/800;

	TextOut(hDC, xc, yc + 0*20, "ORBITER Space Flight Simulator",30);
	TextOut(hDC, xc, yc + 1*20, dataB, strlen(dataB));
	TextOut(hDC, xc, yc + 2*20, dataA, strlen(dataA));

	DWORD VPOS = viewH - 50;
	DWORD LSPACE = 20;

	// SelectObject(hDC, hO) left out: the painter holds its font by value
	delete hF; // DeleteObject

	SurfaceReleaseDC(pSplashScreen, hDC, img); // ReleaseDC


	RECT src = _RECT( loadd_x, loadd_y, loadd_x+loadd_w, loadd_y+loadd_h );
	pDevice->StretchRect(pSplashScreen, &src, pTextScreen, NULL, VK_FILTER_NEAREST); // D3DTEXF_POINT
	pDevice->StretchRect(pSplashScreen, NULL, pBackBuffer, NULL, VK_FILTER_NEAREST);
	pFramework->Present(); // Present(0, 0, 0, 0)
}

// =======================================================================

int D3D9Client::BeginScene()
{
	bRendering = false;
	int hr = 0; // pDevice->BeginScene(): nothing to begin, rendering starts at the first draw
	if (hr == 0) bRendering = true;
	return hr;
}

// =======================================================================

void D3D9Client::EndScene()
{
	pDevice->EndRendering(); // EndScene
	bRendering = false;
}

// =======================================================================

#pragma region Drawing_(Sketchpad)_Interface


double sketching_time;

// =======================================================================
// 2D Drawing Interface
//
oapi::Sketchpad *D3D9Client::clbkGetSketchpad_const(SURFHANDLE surf) const
{
	if (ChkDev(__FUNCTION__)) return NULL;

	if (std::this_thread::get_id() != hMainThread) { // GetCurrentThread (a pseudo handle upstream, so the check never fired)
		LogErr("Sketchpad called from a worker thread !");
		HALT();
	}

	if (surf == RENDERTGT_MAINWINDOW) surf = GetBackBufferHandle();

	if (SURFACE(surf)->IsRenderTarget())
	{
		// Get Pooled Sketchpad
		D3D9Pad *pPad = SURFACE(surf)->GetPooledSketchPad();

		// Get Current interface if any
		D3D9Pad *pCur = GetTopInterface();

		// Do we have an existing SketchPad interface in use
		if (pCur) {
			if (pCur == pPad) {
				LogErr("Sketchpad already exists for this surface");
				HALT();
			}
			pCur->EndDrawing();	// Put the current one in hold
			LogDbg("Red", "Switching to another sketchpad in a middle");
		}

		// Push a new Sketchpad onto a stack
		PushSketchpad(surf, pPad);

		pPad->BeginDrawing();
		pPad->LoadDefaults();

		return pPad;
	}
	else {
		QPainter *hDC = SURFACE(surf)->GetDC();
		if (hDC) return new GDIPad(surf, hDC);
	}

	return NULL;
}

// =======================================================================
// 2D Drawing Interface
//
oapi::Sketchpad* D3D9Client::clbkGetSketchpad(SURFHANDLE surf)
{
	return clbkGetSketchpad_const(surf);
}

// =======================================================================

void D3D9Client::clbkReleaseSketchpad_const(oapi::Sketchpad* sp) const
{
	_TRACE;
	if (ChkDev(__FUNCTION__)) return;

	if (!sp) return;

	SURFHANDLE hSrf = sp->GetSurface();

	if (SURFACE(hSrf)->IsRenderTarget()) {

		D3D9Pad* pPad = ((D3D9Pad*)sp);

		if (GetTopInterface() != pPad) {
			LogErr("Sketchpad release failed. Not a top one.");
			HALT();
		}

		pPad->EndDrawing();

		PopRenderTargets();

		// Do we have an old interface ?
		D3D9Pad* pOld = GetTopInterface();
		if (pOld) {
			pOld->BeginDrawing();	// Continue with the old one
			LogDbg("Red", "Continue Previous Sketchpad");
		}
	}
	else {
		GDIPad* pGDI = (GDIPad*)sp;
		SURFACE(hSrf)->ReleaseDC(pGDI->GetDC());
		delete pGDI;
	}
}

// =======================================================================

void D3D9Client::clbkReleaseSketchpad(oapi::Sketchpad *sp)
{
	clbkReleaseSketchpad_const(sp);
}

// =======================================================================

Font *D3D9Client::clbkCreateFont(int height, bool prop, const char *face, FontStyle style, int orientation) const
{
	_TRACE;
	if (ChkDev(__FUNCTION__)) return NULL;
	return *g_fonts.insert(new D3D9PadFont(height, prop, face, style, orientation)).first;
}

Font* D3D9Client::clbkCreateFontEx(int height, char* face, int width, int weight, FontStyle style, float spacing) const
{
	_TRACE;
	if (ChkDev(__FUNCTION__)) return NULL;
	return *g_fonts.insert(new D3D9PadFont(height, face, width, weight, style, spacing)).first;
}


// =======================================================================

void D3D9Client::clbkReleaseFont(Font *font) const
{
	_TRACE;
	if (!g_fonts.count(font)) return;
	g_fonts.erase(font);
	delete ((D3D9PadFont*)font);
}

// =======================================================================

Pen *D3D9Client::clbkCreatePen(int style, int width, DWORD col) const
{
	_TRACE;
	return *g_pens.insert(new D3D9PadPen(style, width, col)).first;
}

// =======================================================================

void D3D9Client::clbkReleasePen(Pen *pen) const
{
	_TRACE;
	if (!g_pens.count(pen)) return;
	g_pens.erase(pen);
	delete ((D3D9PadPen*)pen);
}

// =======================================================================

Brush *D3D9Client::clbkCreateBrush(DWORD col) const
{
	_TRACE;
	return *g_brushes.insert(new D3D9PadBrush(col)).first;
}

// =======================================================================

void D3D9Client::clbkReleaseBrush(Brush *brush) const
{
	_TRACE;
	if (!g_brushes.count(brush)) return;
	g_brushes.erase(brush);
	delete ((D3D9PadBrush*)brush);
}

#pragma endregion


// ======================================================================
// class VisObject

VisObject::VisObject(OBJHANDLE hObj) : hObj(hObj)
{
	_TRACE;
}

// =======================================================================

VisObject::~VisObject ()
{
}

