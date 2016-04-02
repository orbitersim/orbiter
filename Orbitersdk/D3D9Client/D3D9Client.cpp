// ==============================================================
// D3D9Client.cpp
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2006-2016 Martin Schweiger
//				 2012-2016 Jarmo Nikkanen
// ==============================================================


#define STRICT 1
#define ORBITER_MODULE

#include <set> // ...for Brush-, Pen- and Font-accounting
#include "orbitersdk.h"
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
#include "Texture.h"
#include "MeshMgr.h"
#include "Particle.h"
#include "TileMgr.h"
#include "RingMgr.h"
#include "HazeMgr.h"
#include "Log.h"
#include "VideoTab.h"
#include "GDIPad.h"
#include "windows.h"
#include "psapi.h"
#include "FileParser.h"
#include "OapiExtension.h"
#include "DebugControls.h"
#include "Tilemgr2.h"

using namespace oapi;

HINSTANCE g_hInst = 0;
D3D9Client *g_client = 0;
D3D9Catalog *MeshCatalog = 0;
D3D9Catalog *TileCatalog = 0;
D3D9Catalog *SurfaceCatalog = 0;
DWORD uCurrentMesh = 0;
vObject *pCurrentVisual = 0;
_D3D9Stats D3D9Stats;

#ifdef _NVAPI_H
 StereoHandle pStereoHandle = 0;
#endif

bool bFreeze = false;
bool bSkepchpadOpen = false;

// Module local constellation marker storage
static GraphicsClient::LABELSPEC *g_cm_list = NULL;
static DWORD g_cm_list_count = 0;

// Debuging Brush-, Pen- and Font-accounting
std::set<Font *> g_fonts;
std::set<Pen *> g_pens;
std::set<Brush *> g_brushes;

extern "C" {
	_declspec(dllexport) DWORD NvOptimusEnablement = 0x00000001;
}

// ==============================================================
// API interface
// ==============================================================

// ==============================================================
// Initialise module



void MissingRuntimeError()
{
	MessageBoxA(NULL, "DirectX Runtimes may be missing. See /Doc/D3D9Client.doc for more information", "D3D9Client Initialization Failed",MB_OK);
}

int PrintModules(DWORD pAdr)
{
	HMODULE hMods[1024];
	HANDLE hProcess;
	DWORD cbNeeded;
	unsigned int i;

	// Get a handle to the process.

	hProcess = OpenProcess( PROCESS_QUERY_INFORMATION | PROCESS_VM_READ, FALSE, GetProcessId(GetCurrentProcess()));

	if (NULL==hProcess) return 1;

	if (EnumProcessModules(hProcess, hMods, sizeof(hMods), &cbNeeded)) {
		for ( i = 0; i < (cbNeeded / sizeof(HMODULE)); i++ ) {
			char szModName[MAX_PATH];
			if (GetModuleFileNameExA(hProcess, hMods[i], szModName, sizeof(szModName))) {
				MODULEINFO mi;
				GetModuleInformation(hProcess, hMods[i], &mi, sizeof(MODULEINFO));
				DWORD Base = (DWORD)mi.lpBaseOfDll;
				if (pAdr>Base && pAdr<(Base+mi.SizeOfImage)) LogErr("%s EntryPoint=0x%8.8X, Base=0x%8.8X, Size=%u", szModName, mi.EntryPoint, mi.lpBaseOfDll, mi.SizeOfImage);
				else										 LogOk("%s EntryPoint=0x%8.8X, Base=0x%8.8X, Size=%u", szModName, mi.EntryPoint, mi.lpBaseOfDll, mi.SizeOfImage);
			}
		}
	}
	CloseHandle( hProcess );
	return 0;
}

bool bException = false;
bool bNVAPI = false;
DWORD ECode=0, EAddress=0;


int ExcHandler(EXCEPTION_POINTERS *p)
{
	EXCEPTION_RECORD *pER = p->ExceptionRecord;
	CONTEXT *pEC = p->ContextRecord;
	ECode = pER->ExceptionCode;
	EAddress = (DWORD)pER->ExceptionAddress;
	LogErr("Orbiter Version %d",oapiGetOrbiterVersion());
	LogErr("D3D9Client Build [%s]",__DATE__);
	LogErr("Exception Code=0x%8.8X, Address=0x%8.8X", ECode, EAddress);
	LogErr("EAX=0x%8.8X EBX=0x%8.8X ECX=0x%8.8X EDX=0x%8.8X ESI=0x%8.8X EDI=0x%8.8X EBP=0x%8.8X ESP=0x%8.8X EIP=0x%8.8X", pEC->Eax, pEC->Ebx, pEC->Ecx, pEC->Edx, pEC->Esi, pEC->Edi, pEC->Ebp, pEC->Esp, pEC->Eip);
	PrintModules(EAddress);
	bException = true;
	return 1;
}


DLLCLBK void InitModule(HINSTANCE hDLL)
{

#ifdef _DEBUG
	_CrtSetDbgFlag(_CRTDBG_ALLOC_MEM_DF | _CRTDBG_LEAK_CHECK_DF);
	// _CrtSetBreakAlloc(8351);
#endif

	D3D9InitLog("Modules/D3D9Client/D3D9ClientLog.html");

	if (!D3DXCheckVersion(D3D_SDK_VERSION, D3DX_SDK_VERSION)) {
		MissingRuntimeError();
		return;
	}

	QueryPerformanceFrequency((LARGE_INTEGER*)&qpcFrq);
	QueryPerformanceCounter((LARGE_INTEGER*)&qpcStart);

#ifdef _NVAPI_H
	if (NvAPI_Initialize()==NVAPI_OK) {
		LogAlw("[nVidia API Initialized]");
		bNVAPI = true;
	}
	else LogAlw("[nVidia API Not Available]");
#else
	LogAlw("[Not Compiled With nVidia API]");
#endif

	Config			= new D3D9Config();
	TileCatalog		= new D3D9Catalog("TileCatalog");
	MeshCatalog		= new D3D9Catalog("MeshCatalog");
	SurfaceCatalog  = new D3D9Catalog("SurfaceCatalog");

	DebugControls::Create();
	AtmoControls::Create();

	g_hInst = hDLL;
	g_client = new D3D9Client(hDLL);

	if (oapiRegisterGraphicsClient(g_client)==false) {
		delete g_client;
		g_client = 0;
	}
}

// ==============================================================
// Clean up module



DLLCLBK void ExitModule(HINSTANCE hDLL)
{
	LogAlw("--------------ExitModule------------");

	if (bException) LogErr("!!! Abnormal Program Termination !!!");

	delete TileCatalog;
	delete MeshCatalog;
	delete SurfaceCatalog;
	delete Config;

	DebugControls::Release();
	AtmoControls::Release();

	if (g_client) {
		oapiUnregisterGraphicsClient(g_client);
		g_client = 0;
	}

#ifdef _NVAPI_H
	if (bNVAPI) if (NvAPI_Unload()==NVAPI_OK) LogAlw("[nVidia API Unloaded]");
#endif

	LogAlw("Log Closed");
	D3D9CloseLog();
}

// ==============================================================
// D3D9Client class implementation
// ==============================================================

D3D9Client::D3D9Client (HINSTANCE hInstance) : GraphicsClient(hInstance)
{
	vtab = NULL;
	strcpy_s(ScenarioName, "(none selected)");
}

// ==============================================================

D3D9Client::~D3D9Client()
{

	LogAlw("D3D9Client destructor called");
	SAFE_DELETE(vtab);

	// Free constellation names memory (if allocted)
	if (g_cm_list) {
		for (DWORD n = 0; n < g_cm_list_count; ++n) {
			delete[] g_cm_list[n].label[0];
			delete[] g_cm_list[n].label[1];
		}
		delete[] g_cm_list;
	}

	//
	// Orbiter seems not to release all resources :(
	//

	// --- Fonts
	if (g_fonts.size()) {
		LogErr("%u un-released fonts!", g_fonts.size());
		for (auto it = g_fonts.begin(); it != g_fonts.end(); ) {
			clbkReleaseFont(*it++);
		}
		g_fonts.clear();
	}
	// --- Brushes
	if (g_brushes.size()) {
		LogErr("%u un-released brushes!", g_brushes.size());
		for (auto it = g_brushes.begin(); it != g_brushes.end(); ) {
			clbkReleaseBrush(*it++);
		}
		g_brushes.clear();
	}
	// --- Pens
	if (g_pens.size()) {
		LogErr("%u un-released pens!", g_pens.size());
		for (auto it = g_pens.begin(); it != g_pens.end(); ) {
			clbkReleasePen(*it++);
		}
		g_pens.clear();
	}
}

// ==============================================================
// Overridden
//
const void *D3D9Client::GetConfigParam (DWORD paramtype) const
{
	return (paramtype >= CFGPRM_SHOWBODYFORCEVECTORSFLAG)
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

	// Perform default setup
	if (GraphicsClient::clbkInitialise()==false) return false;
	//Create the Launchpad video tab interface
	vtab = new VideoTab(this, ModuleInstance(), OrbiterInstance(), LaunchpadVideoTab());

	return true;
}


// ==============================================================
// This is called when a simulation session will begin
//
HWND D3D9Client::clbkCreateRenderWindow()
{
	_TRACE;

	LogAlw("================ clbkCreateRenderWindow ===============");

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
	bHalt			 = false;
	bVertexTex		 = false;
	viewW = viewH    = 0;
	viewBPP          = 0;
	iDisl			 = 0;
	frame_timer		 = 0;
	scene            = NULL;
	meshmgr          = NULL;
	texmgr           = NULL;
	pFramework       = NULL;
	pd3dDevice       = NULL;
	parser			 = NULL;
	pBltGrpTgt		 = NULL;	// Let's set this NULL here, constructor is called only once. Not when exiting and restarting a simulation.
	pNoiseTex		 = NULL;
	surfBltTgt		 = NULL;	// This variable is not used, set it to NULL anyway

	memset2(&D3D9Stats, 0, sizeof(D3D9Stats));
	memset2(pDislMapList, 0, 16*sizeof(SURFHANDLE));

	D3DXMatrixIdentity(&ident);

	sprintf_s(oapiDebugString(),255,"");

	TileCatalog->Clear();
	MeshCatalog->Clear();
	SurfaceCatalog->Clear();

	hRenderWnd = GraphicsClient::clbkCreateRenderWindow();

	LogAlw("Window Handle = 0x%X",hRenderWnd);
	SetWindowText(hRenderWnd, "[D3D9Client]");

	LogOk("Starting to initialize device and 3D environment...");

	pFramework = new CD3DFramework9();

	if (pFramework->GetDirect3D()==NULL) return NULL;

	WriteLog("[DirectX 9 Initialized]");

	HRESULT hr = pFramework->Initialize(hRenderWnd, GetVideoData());

	if (hr!=S_OK) {
		LogErr("ERROR: Failed to initialize 3D Framework");
		return NULL;
	}

	RECT rect;
	GetClientRect(hRenderWnd, &rect);
	HDC hWnd = GetDC(hRenderWnd);
	HBRUSH hBr = CreateSolidBrush(RGB(0,0,0));
	FillRect(hWnd, &rect, hBr);
	DeleteObject(hBr);
	ReleaseDC(hRenderWnd, hWnd);
	ValidateRect(hRenderWnd, NULL);	// avoids white flash after splash screen

	caps = pFramework->caps;

	WriteLog("[3DDevice Initialized]");

	pd3dDevice	= pFramework->GetD3DDevice();
	viewW		= pFramework->GetWidth();
	viewH		= pFramework->GetHeight();
	bFullscreen = (pFramework->IsFullscreen() == TRUE);
	bGDIBB		= (pFramework->IsGDIBB() == TRUE);
	bAAEnabled  = (pFramework->IsAAEnabled() == TRUE);
	viewBPP		= 32;
	pEnvDS		= pFramework->GetEnvDepthStencil();
	pShmDS		= pFramework->GetShadowMapDepthStencil();
	pShmRT		= pFramework->GetShadowMapRenderTarget();
	bVertexTex  = (pFramework->HasVertexTextureSup() == TRUE);
	bVSync		= (pFramework->GetVSync() == TRUE);

	char fld[] = "D3D9Client";

	//if (bGDIBB) Config->SketchpadMode = 1;

	D3D9ClientSurface::D3D9TechInit(this, pd3dDevice, fld);

	HR(pd3dDevice->GetRenderTarget(0, &pBackBuffer));
	LogAlw("Render Target = 0x%X", pBackBuffer);

	meshmgr		= new MeshManager(this);
	texmgr	    = new TextureManager(this);

	// Bring Sketchpad Online
	D3D9PadFont::D3D9TechInit(pd3dDevice);
	D3D9PadPen::D3D9TechInit(pd3dDevice);
	D3D9PadBrush::D3D9TechInit(pd3dDevice);
	D3D9Text::D3D9TechInit(this, pd3dDevice, fld);
	D3D9Pad::D3D9TechInit(this, pd3dDevice, fld);

	deffont = (oapi::Font*) new D3D9PadFont(20, true, "fixed");
	defpen  = (oapi::Pen*)  new D3D9PadPen(1, 1, 0x00FF00);

	pNoiseTex = SURFACE(clbkLoadTexture("D3D9Noise.dds"));
	pDefaultTex = SURFACE(clbkLoadTexture("Null.dds"));

	if (pDefaultTex==NULL) LogErr("Null.dds not found");
	if (pNoiseTex==NULL) LogErr("D3D9Noise.dds not found");

	RegisterDissolveMap(clbkLoadTexture("Disl_Crystal.png"));
	RegisterDissolveMap(clbkLoadTexture("Disl_Lines.png"));
	RegisterDissolveMap(clbkLoadTexture("Disl_Lines2.png"));
	RegisterDissolveMap(clbkLoadTexture("Disl_Noise.png"));
	RegisterDissolveMap(clbkLoadTexture("Disl_Pool.png"));

	int x=0;
	if (viewW>1282) x=4;

	hLblFont1 = CreateFont(24+x, 0, 0, 0, 700, false, false, 0, 0, 3, 2, 1, 49, "Courier New");
	hLblFont2 = CreateFont(18+x, 0, 0, 0, 700, false, false, 0, 0, 3, 2, 1, 49, "Courier New");

	SplashScreen();  // Warning D3D9ClientSurface is not yet fully initialized here

	OutputLoadStatus("Building Shader Programs...",0);
	
	D3D9Effect::D3D9TechInit(this, pd3dDevice, fld);

	// Device-specific initialisations

	TileManager::GlobalInit(this);
	TileManager2Base::GlobalInit(this);
	PlanetRenderer::GlobalInit(this);
	RingManager::GlobalInit(this);
	HazeManager::GlobalInit(this);
	HazeManager2::GlobalInit(this);
	D3D9ParticleStream::GlobalInit(this);
	CSphereManager::GlobalInit(this);
	vStar::GlobalInit(this);
	vObject::GlobalInit(this);
	vVessel::GlobalInit(this);
	OapiExtension::GlobalInit(*Config);

	OutputLoadStatus("SceneTech.fx",1);
	Scene::D3D9TechInit(pd3dDevice, fld);

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
			if (NvAPI_Stereo_CreateHandleFromIUnknown(pd3dDevice, &pStereoHandle)!=NVAPI_OK) {
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

	if (parser) return;

	parser = new FileParser(ScenarioName);
	parser->LogContent();

	if (scene) scene->Initialise();

	bRunning = true;

	LogAlw("=============== Loading Completed and Visuals Created ================");

	WriteLog("[Scene Initialized]");
}


// ==============================================================
// Called when simulation session is about to be closed
//
void D3D9Client::clbkCloseSession(bool fastclose)
{
	_TRACE;
	__TRY {

		LogAlw("================ clbkCloseSession ===============");

		// Disable rendering and some other systems
		//
		bRunning = false;

		// At fisrt, shutdown tile loaders -------------------------------------------------------
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

		GraphicsClient::clbkCloseSession(fastclose);

		SAFE_DELETE(parser);
		LogAlw("================= Deleting Scene ================");
		Scene::GlobalExit();
		SAFE_DELETE(scene);
		LogAlw("============== Deleting Mesh Manager ============");
		SAFE_DELETE(meshmgr);
		WriteLog("[Session Closed. Scene deleted.]");
	}
	__EXCEPT(ExcHandler(GetExceptionInformation()))
	{
		LogErr("Exception in clbkCloseSession()");
		FatalAppExitA(0,"Critical error has occured. See Orbiter.log for details");
	}
}

// ==============================================================

void D3D9Client::clbkDestroyRenderWindow (bool fastclose)
{
	_TRACE;
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

	__TRY {

		DWORD i = 0;
		while (true) {
			SURFHANDLE hSrf = GetDissolveMap(i++);
			if (hSrf) clbkReleaseTexture(hSrf);
			else break;
		}

		LogAlw("=========== Clearing Texture Repository =========");
		SAFE_DELETE(texmgr);
		
		LogAlw("===== Calling GlobalExit() for sub-systems ======");
		HazeManager::GlobalExit();
		HazeManager2::GlobalExit();
		TileManager::GlobalExit();
		TileManager2Base::GlobalExit();
		PlanetRenderer::GlobalExit();
		D3D9ParticleStream::GlobalExit();
		CSphereManager::GlobalExit();
		vStar::GlobalExit();
		vVessel::GlobalExit();
		vObject::GlobalExit();

		SAFE_DELETE(defpen);
		SAFE_DELETE(deffont);

		DeleteObject(hLblFont1);
		DeleteObject(hLblFont2);

		D3D9Pad::GlobalExit();
		D3D9Text::GlobalExit();
		D3D9Effect::GlobalExit();
		D3D9ClientSurface::GlobalExit();

		SAFE_RELEASE(pSplashScreen);	// Splash screen related
		SAFE_RELEASE(pTextScreen);		// Splash screen related
		SAFE_RELEASE(pBackBuffer);		// Splash screen related

		LPD3D9CLIENTSURFACE pBBuf = GetBackBufferHandle();

		SAFE_DELETE(pBBuf);
		SAFE_DELETE(pDefaultTex);
		SAFE_DELETE(pNoiseTex);

		LogAlw("============ Checking Object Catalogs ===========");
		
		// Check surface catalog --------------------------------------------------------------------------------------
		//
		DWORD n = SurfaceCatalog->CountEntries();

		if (n) LogErr("UnDeleted Surface(s) Detected");

		while (n) {
			LPD3D9CLIENTSURFACE pSurf = SURFACE(SurfaceCatalog->Get(0));
			if (pSurf) {
				LogErr("Surface 0x%X (%s) (%u,%u)", pSurf, pSurf->GetName(), pSurf->GetWidth(), pSurf->GetHeight());
				SAFE_DELETE(pSurf);
			} else {
				LogErr("A NULL surface in the SurfaceCatalog");
				break;
			}
			n = SurfaceCatalog->CountEntries();
		}

		// Check tile catalog --------------------------------------------------------------------------------------
		//
		DWORD nt = TileCatalog->CountEntries();
		if (nt) LogErr("SurfaceTile catalog contains %u unreleased entries",nt);
		
		SurfaceCatalog->Clear();
		MeshCatalog->Clear();
		TileCatalog->Clear();

		pFramework->DestroyObjects();

		SAFE_DELETE(pFramework);

		// Close Render Window -----------------------------------------
		GraphicsClient::clbkDestroyRenderWindow(fastclose);

		hRenderWnd		 = NULL;
		pd3dDevice		 = NULL;
		bFailed			 = false;
		viewW = viewH    = 0;
		viewBPP          = 0;
	}

	__EXCEPT(ExcHandler(GetExceptionInformation()))
	{
		LogErr("Exception in clbkDestroyRenderWindow()");
		EmergencyShutdown();
		FatalAppExitA(0,"Critical error has occured. See Orbiter.log for details");
	}
}

// ==============================================================

void D3D9Client::clbkUpdate(bool running)
{
	_TRACE;
	if (bFailed==false && bRunning) scene->Update();
}

// ==============================================================

void D3D9Client::clbkRenderScene()
{
	_TRACE;
	
	char Label[7];

	if (pd3dDevice==NULL || scene==NULL) return;
	if (bFailed) return;
	if (!bRunning) return;

	__TRY {

		if (pd3dDevice->TestCooperativeLevel()!=S_OK) {
			bFailed=true;
			MessageBoxA(pFramework->GetRenderWindow(),"Connection to Direct3DDevice is lost\nExit the simulation with Ctrl+Q and restart.\n\nAlt-Tabing not supported in a true fullscreen mode.\nDialog windows won't work with multi-sampling in a true fullscreen mode.","D3D9Client: Lost Device",0);
			return;
		}

		if (bHalt) {
			pd3dDevice->BeginScene();
			RECT rect2 = {0,viewH-60,viewW,viewH-20};
			pFramework->GetLargeFont()->DrawTextA(0, "Critical error has occured", 26, &rect2, DT_CENTER | DT_TOP, D3DCOLOR_XRGB(255, 0, 0));
			rect2.left-=4; rect2.top-=4;
			pFramework->GetLargeFont()->DrawTextA(0, "Critical error has occured", 26, &rect2, DT_CENTER | DT_TOP, D3DCOLOR_XRGB(255, 255, 255));
			pd3dDevice->EndScene();
			return;
		}

		UINT mem = pd3dDevice->GetAvailableTextureMem()>>20;
		if (mem<32) TileBuffer::HoldThread(true);

		scene->RenderMainScene();		// Render the main scene
		
		Label[0]=0;

		VESSEL *hVes = oapiGetFocusInterface();

		if (hVes) {
			if (hVes->Recording()) strcpy_s(Label, 7, "Record");
			if (hVes->Playback()) strcpy_s(Label, 7, "Replay");
		}

		if (Label[0]!=0) {
			pd3dDevice->BeginScene();
			RECT rect2 = {0,viewH-60,viewW,viewH-20};
			pFramework->GetLargeFont()->DrawTextA(0, Label, 6, &rect2, DT_CENTER | DT_TOP, D3DCOLOR_XRGB(0, 0, 0));
			rect2.left-=4; rect2.top-=4;
			pFramework->GetLargeFont()->DrawTextA(0, Label, 6, &rect2, DT_CENTER | DT_TOP, D3DCOLOR_XRGB(255, 255, 255));
			pd3dDevice->EndScene();
		}

		if (bControlPanel) RenderControlPanel();


		memset2(&D3D9Stats.Mesh, 0, sizeof(D3D9Stats.Mesh));
		memset2(&D3D9Stats.Old, 0, sizeof(D3D9Stats.Old));
		memset2(&D3D9Stats.Surf, 0, sizeof(D3D9Stats.Surf));

	}

	__EXCEPT(ExcHandler(GetExceptionInformation()))
	{
		LogErr("Exception in clbkRenderScene()");
		EmergencyShutdown();
		FatalAppExitA(0,"Critical error has occured. See Orbiter.log for details");
	}
}

// ==============================================================

void D3D9Client::clbkTimeJump(double simt, double simdt, double mjd)
{
	_TRACE;
	GraphicsClient::clbkTimeJump (simt, simdt, mjd);
}

// ==============================================================

double framer_rater_limit = 0.0;

bool D3D9Client::clbkDisplayFrame()
{
	_TRACE;
	static int iRefrState = 0;
	double time = D3D9GetTime();
	
	if (!bRunning) {
		RECT txt = { loadd_x, loadd_y, loadd_x+loadd_w, loadd_y+loadd_h };
		pd3dDevice->StretchRect(pSplashScreen, NULL, pBackBuffer, NULL, D3DTEXF_POINT);
		pd3dDevice->StretchRect(pTextScreen, NULL, pBackBuffer, &txt, D3DTEXF_POINT);
	}

	if (bFullscreen==false) {
		RenderWithPopupWindows();
		pd3dDevice->Present(0, 0, 0, 0);
	}
	else {
		if (!RenderWithPopupWindows()) pd3dDevice->Present(0, 0, 0, 0);
	}

	double frmt = (1000000.0/Config->FrameRate) - (time - framer_rater_limit);

	framer_rater_limit = time;

	if (Config->EnableLimiter && Config->FrameRate>0 && bVSync==false) {
		if (frmt>0) frame_timer++;
		else        frame_timer--;
		if (frame_timer>40) frame_timer=40;
		Sleep(frame_timer);
	}

	return true;
}

// ==============================================================

void D3D9Client::clbkPreOpenPopup ()
{
	_TRACE;
	GetDevice()->SetDialogBoxMode(true);
}

// =======================================================================

static DWORD g_lastPopupWindowCount = 0;
static void FixOutOfScreenPositions (const HWND *hWnd, DWORD count)
{
	// Only check if a popup window is *added*
	if (count > g_lastPopupWindowCount)
	{
		for (DWORD i=0; i<count; ++i)
		{
			RECT rect;
			GetWindowRect(hWnd[i], &rect);

			int x = -1, y, w, h; // x != -1 indicates "position change needed"
			if (rect.left < 0) {
				x = 0;
				y = rect.top;
			}
			if (rect.top  < 0) {
				x = rect.left;
				y = 0;
			}

			// For the rest we need monitor information...
			HMONITOR monitor = MonitorFromWindow(hWnd[i], MONITOR_DEFAULTTONEAREST);
			MONITORINFO info;
			info.cbSize = sizeof(MONITORINFO);
			GetMonitorInfo(monitor, &info);

			int monitorWidth = info.rcMonitor.right - info.rcMonitor.left; // info.rcWork....
			int monitorHeight = info.rcMonitor.bottom - info.rcMonitor.top;

			if (rect.right > monitorWidth) {
				x = monitorWidth - (rect.right - rect.left);
				y = rect.top;
			}
			if (rect.bottom > monitorHeight) {
				x = rect.left;
				y = monitorHeight - (rect.bottom - rect.top);
			}

			if (x != -1) {
				w = rect.right - rect.left,
				h = rect.bottom - rect.top;
				MoveWindow(hWnd[i], x, y, w, h, FALSE);
			}
		}

	}
	g_lastPopupWindowCount = count;
}

// =======================================================================

bool D3D9Client::RenderWithPopupWindows()
{
	_TRACE;

	const HWND *hPopupWnd;
	DWORD count = GetPopupList(&hPopupWnd);

	if (bFullscreen) {
		if (count) GetDevice()->SetDialogBoxMode(true);
		else       GetDevice()->SetDialogBoxMode(false);
	}

	// Let the OapiExtension manager know about this..
	OapiExtension::HandlePopupWindows(hPopupWnd, count);

	FixOutOfScreenPositions(hPopupWnd, count);

	if (!bFullscreen) {
		for (DWORD i=0;i<count;i++) {
			DWORD val = GetWindowLongA(hPopupWnd[i], GWL_STYLE);
			if ((val&WS_SYSMENU)==0) {
				SetWindowLongA(hPopupWnd[i], GWL_STYLE, val|WS_SYSMENU);
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
	if (fname) {
		LogAlw("Storing a mesh 0x%X (%s)",hMesh,fname);
		if (hMesh==NULL) LogErr("D3D9Client::clbkStoreMeshPersistent(%s) hMesh is NULL",fname);
	}
	else {
		LogAlw("Storing a mesh 0x%X",hMesh);
		if (hMesh==NULL) LogErr("D3D9Client::clbkStoreMeshPersistent() hMesh is NULL");
	}

	if (hMesh==NULL) return;

	int idx = meshmgr->StoreMesh(hMesh, fname);

	if (idx>=0) {
		if (fname) LogWrn("MeshGroup(%d) in a mesh %s is larger than 1km",idx,fname);
		else	   LogWrn("MeshGroup(%d) in a mesh 0x%X is larger than 1km",idx,hMesh);
	}
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
	D3D9Mesh *mesh = (D3D9Mesh*)hMesh;
	DWORD nmat = mesh->GetMaterialCount();
	if (matidx >= nmat) return 4; // "index out of range"
	D3D9MatExt *meshmat = mesh->GetMaterial(matidx);
	if (meshmat) UpdateMatExt((const D3DMATERIAL9 *)mat, meshmat);
	return 0;
}

// ==============================================================

int D3D9Client::clbkMeshMaterial (DEVMESHHANDLE hMesh, DWORD matidx, MATERIAL *mat)
{
	_TRACE;
	D3D9Mesh *mesh = (D3D9Mesh*)hMesh;
	DWORD nmat = mesh->GetMaterialCount();
	if (matidx >= nmat) return 4; // "index out of range"
	D3D9MatExt *meshmat = mesh->GetMaterial(matidx);
	if (meshmat) GetMatExt(meshmat, (D3DMATERIAL9 *)mat);
	return 0;	
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
// Returns a mesh for a visual

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
	_TRACE;
	__TRY {
		if (scene) scene->DeleteVessel(hVessel);
	}
	__EXCEPT(ExcHandler(GetExceptionInformation()))
	{
		LogErr("Exception in clbkDeleteVessel()");
		EmergencyShutdown();
		FatalAppExitA(0,"Critical error has occured. See Orbiter.log for details");
	}
}


// ==============================================================
// copy video options from the video tab

void D3D9Client::clbkRefreshVideoData()
{
	_TRACE;
	if (vtab) vtab->UpdateConfigData();
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
			*value = max(0, min(12, Config->MaxLights));
			return true;

		case RP_REQUIRETEXPOW2:
			*value = 0;
			return true;
	}
	return false;
}

// ==============================================================
// Responds to visual events

int D3D9Client::clbkVisEvent(OBJHANDLE hObj, VISHANDLE vis, DWORD msg, UINT context)
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

void D3D9Client::EmergencyShutdown()
{
	bool b = oapiSaveScenario("D3D9ClientRescue","Contains the current simulation state before shutdown due to an unexpected error");
	if (b) LogAlw("ORBITER SCENARIO SAVED SUCCESSFULLY (D3D9ClientRescue.scn)");
	else   LogErr("FAILED TO SAVE A SCENARIO");
	bHalt = true;
}

LRESULT CALLBACK LowLevelKeyboardProc(int nCode, WPARAM wParam, LPARAM lParam)
{
	bool eatKeystroke = false;

	if (nCode == HC_ACTION) 
	{
		switch (wParam) 
		{
			case WM_KEYDOWN:  
			case WM_SYSKEYDOWN:
			case WM_KEYUP:    
			case WM_SYSKEYUP:
			{
				PKBDLLHOOKSTRUCT p = (PKBDLLHOOKSTRUCT) lParam;
				// eatKeystroke = (p->vkCode == VK_SNAPSHOT);
				switch (p->vkCode) {
				case 'W': case 'w':
				case 'A': case 'a':
				case 'S': case 's':
				case 'D': case 'd':
					// Here goes your code...
					eatKeystroke = true;
					break;
				}
				break;
			}
		}
	}

	return eatKeystroke
		 ? 1
		 : CallNextHookEx(NULL, nCode, wParam, lParam);
}

// ==============================================================
// Message handler for render window

LRESULT D3D9Client::RenderWndProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	_TRACE;

	// -----------------------------------------------------------------------
	/*
	static bool hooked = false;
	if (!hooked && hRenderWnd == hWnd) {
		// Install the low-level keyboard & mouse hooks
		HINSTANCE hInstance = (HINSTANCE)GetWindowLong(hWnd, GWL_HINSTANCE);
		HHOOK hhkLowLevelKybd  = SetWindowsHookEx(WH_KEYBOARD_LL, LowLevelKeyboardProc, hInstance, 0);
		hooked = true;
	}*/
	// -----------------------------------------------------------------------



	static bool bTrackMouse = false;
	static short xpos=0, ypos=0;

	if (hRenderWnd!=hWnd) {
		LogErr("Invalid Window !! RenderWndProc() called after calling clbkDestroyRenderWindow() uMsg=0x%X");
		return 0;
	}

	if (bRunning && DebugControls::IsActive()) {
		// Must update camera to correspond MAIN_SCENE due to Pick() function, 
		// because env-maps have altered camera settings 
		GetScene()->UpdateCameraFromOrbiter(RENDERPASS_PICKSCENE);
	}

	__TRY {

		switch (uMsg) {

			case WM_MOUSELEAVE:
			{
				if (bTrackMouse && bRunning) GraphicsClient::RenderWndProc (hWnd, WM_LBUTTONUP, 0, 0);
				return 0;
			}

			case WM_MBUTTONDOWN:
			{
				break;
			}

			case WM_LBUTTONDOWN:
			{
				bTrackMouse = true;
				xpos = GET_X_LPARAM(lParam);
				ypos = GET_Y_LPARAM(lParam);
				TRACKMOUSEEVENT te; te.cbSize = sizeof(TRACKMOUSEEVENT); te.dwFlags=TME_LEAVE; te.hwndTrack=hRenderWnd;
				TrackMouseEvent(&te);

				if (DebugControls::IsActive()) {

					DWORD flags = *(DWORD*)GetConfigParam(CFGPRM_GETDEBUGFLAGS);

					if (flags&DBG_FLAGS_PICK) {

						D3D9Pick pick = GetScene()->PickScene(xpos, ypos);

						if (!pick.pMesh) break;
				
						//sprintf_s(oapiDebugString(),256,"vObj=0x%X, Mesh=0x%X, Grp=%d, Face=%d", pick.vObj, pick.pMesh, pick.group, pick.face);

						bool bShift = (GetAsyncKeyState(VK_SHIFT) & 0x8000)!=0;
						bool bCtrl  = (GetAsyncKeyState(VK_CONTROL) & 0x8000)!=0;	
	
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
						}
					}
				}
				break;
			}

			case WM_LBUTTONUP:
			{
				if (DebugControls::IsActive()) {
					DWORD flags = *(DWORD*)GetConfigParam(CFGPRM_GETDEBUGFLAGS);
					if (flags&DBG_FLAGS_PICK) {
						DebugControls::SetGroupHighlight(false);
					}
				}
				bTrackMouse = false;
				break;
			}

			case WM_KEYDOWN:
			{
				if (DebugControls::IsActive()) {
					if (wParam == 'F') bFreeze = !bFreeze;
				}
				bool bShift = (GetAsyncKeyState(VK_SHIFT) & 0x8000)!=0;
				bool bCtrl  = (GetAsyncKeyState(VK_CONTROL) & 0x8000)!=0;
				if (wParam=='C' && bShift && bCtrl) bControlPanel = !bControlPanel;
				break;
			}

			case WM_MOUSEWHEEL:
			{
				if (DebugControls::IsActive()) {
					short d = GET_WHEEL_DELTA_WPARAM(wParam);
					if (d<-1) d=-1;
					if (d>1) d=1;
					double speed = *(double *)GetConfigParam(CFGPRM_GETCAMERASPEED);
					speed *= (DebugControls::GetVisualSize()/100.0);
					if (scene->CameraPan(_V(0,0,double(d))*2.0, speed)) return 0;
				}
			}

			case WM_MOUSEMOVE:
				if (DebugControls::IsActive()) {

					double x = double(GET_X_LPARAM(lParam) - xpos);
					double y = double(GET_Y_LPARAM(lParam) - ypos);
					xpos = GET_X_LPARAM(lParam);
					ypos = GET_Y_LPARAM(lParam);

					if (bTrackMouse) {
						double speed = *(double *)GetConfigParam(CFGPRM_GETCAMERASPEED);
						speed *= (DebugControls::GetVisualSize()/100.0);
						if (scene->CameraPan(_V(-x,y,0)*0.05, speed)) return 0;
					}

					GetScene()->PickSurface(xpos, ypos);
				}
				break;

			case WM_MOVE:
				// If in windowed mode, move the Framework's window
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
						if (bFullscreen) return 1;
						break;
				}
				break;

			case WM_SYSKEYUP:
				if (bFullscreen) return 0;  // trap Alt-key
				break;

		}
	}
	__EXCEPT(ExcHandler(GetExceptionInformation()))
	{
		LogErr("D3D9Client::RenderWndProc(hWnd=0x%X, uMsg=%u, wParam=%u, lParam=%u)",hWnd, uMsg, wParam, lParam);
		EmergencyShutdown();
		FatalAppExitA(0,"Critical error has occured. See Orbiter.log for details");
	}

	__TRY {
		if (!bRunning && uMsg>=0x0200 && uMsg<=0x020E) return 0;
		return GraphicsClient::RenderWndProc (hWnd, uMsg, wParam, lParam);
	}
	__EXCEPT(ExcHandler(GetExceptionInformation()))
	{
		LogErr("GraphicsClient::RenderWndProc(hWnd=0x%X, uMsg=%u, wParam=%u, lParam=%u)",hWnd, uMsg, wParam, lParam);
		EmergencyShutdown();
		FatalAppExitA(0,"Critical error has occured. See Orbiter.log for details");
	}

	return 0;
}


// ==============================================================
// Message handler for Launchpad "video" tab

BOOL D3D9Client::LaunchpadVideoWndProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	_TRACE;
	if (vtab) return vtab->WndProc(hWnd, uMsg, wParam, lParam);
	else return false;
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

bool D3D9Client::clbkSaveSurfaceToImage(SURFHANDLE  surf,  const char *fname, ImageFileFormat  fmt, float quality)
{
	_TRACE;
	if (surf==NULL) surf = pFramework->GetBackBufferHandle();

	LPDIRECT3DSURFACE9 pRTG = NULL;
	LPDIRECT3DSURFACE9 pSystem = NULL;
	LPDIRECT3DSURFACE9 pSurf = SURFACE(surf)->pSurf;

	if (pSurf==NULL) return false;

	bool bRet = false;
	ImageData ID;
	D3DSURFACE_DESC desc;
	D3DLOCKED_RECT pRect;

	SURFACE(surf)->GetDesc(&desc);

	if (desc.Pool!=D3DPOOL_SYSTEMMEM) {

		HR(pd3dDevice->CreateRenderTarget(desc.Width, desc.Height, D3DFMT_X8R8G8B8, D3DMULTISAMPLE_NONE, 0, false, &pRTG, NULL));
		HR(pd3dDevice->CreateOffscreenPlainSurface(desc.Width, desc.Height, D3DFMT_X8R8G8B8, D3DPOOL_SYSTEMMEM, &pSystem, NULL));
		HR(pd3dDevice->StretchRect(pSurf, NULL, pRTG, NULL, D3DTEXF_NONE));
		HR(pd3dDevice->GetRenderTargetData(pRTG, pSystem));

		if (pSystem->LockRect(&pRect, NULL, 0)==S_OK) {

			ID.bpp = 24;
			ID.height = desc.Height;
			ID.width = desc.Width;
			ID.stride = ((ID.width * ID.bpp + 31) & ~31) >> 3;
			ID.bufsize = ID.stride * ID.height;

			BYTE *tgt = ID.data = new BYTE[ID.bufsize];
			BYTE *src = (BYTE *)pRect.pBits;

			for (DWORD k=0;k<desc.Height;k++) {
				for (DWORD i=0;i<desc.Width;i++) {
					tgt[0+i*3] = src[0+i*4];
					tgt[1+i*3] = src[1+i*4];
					tgt[2+i*3] = src[2+i*4];
				}
				tgt += ID.stride;
				src += pRect.Pitch;
			}

			bRet = WriteImageDataToFile(ID, fname, fmt, quality);

			delete []ID.data;
			pSystem->UnlockRect();
		}

		pRTG->Release();
		pSystem->Release();
		return bRet;
	}

	if (pSurf->LockRect(&pRect, NULL, D3DLOCK_READONLY)==S_OK) {

		ID.bpp = 24;
		ID.height = desc.Height;
		ID.width = desc.Width;
		ID.stride = ((ID.width * ID.bpp + 31) & ~31) >> 3;
		ID.bufsize = ID.stride * ID.height;

		BYTE *tgt = ID.data = new BYTE[ID.bufsize];
		BYTE *src = (BYTE *)pRect.pBits;

		for (DWORD k=0;k<desc.Height;k++) {
			for (DWORD i=0;i<desc.Width;i++) {
				tgt[0+i*3] = src[0+i*4];
				tgt[1+i*3] = src[1+i*4];
				tgt[2+i*3] = src[2+i*4];
			}
			tgt += ID.stride;
			src += pRect.Pitch;
		}

		bRet = WriteImageDataToFile(ID, fname, fmt, quality);

		delete []ID.data;
		pSurf->UnlockRect();
		return bRet;
	}

	return false;
}

// ==============================================================

SURFHANDLE D3D9Client::clbkLoadTexture(const char *fname, DWORD flags)
{
	_TRACE;
	LPD3D9CLIENTSURFACE pTex = NULL;

	char cpath[256];

	if (TexturePath(fname, cpath)==false) {
		LogWrn("Texture %s not found.",fname);
		return NULL;
	}

	if (flags & 8) {
		if (texmgr->GetTexture(fname, &pTex, flags)==false) return NULL;
	}
	else {
		if (texmgr->LoadTexture(fname, &pTex, flags)!=S_OK) return NULL;
	}

	return pTex;
}

// ==============================================================

SURFHANDLE D3D9Client::clbkLoadSurface (const char *fname, DWORD attrib)
{
	_TRACE;
	DWORD flags = 0;

	// Process flag conflicts and issues ---------------------------------------------------------------------------------
	//
	flags = OAPISURFACE_RENDERTARGET|OAPISURFACE_SYSMEM;

	if ((attrib&flags)==flags) {
		LogErr("clbkLoadSurface() Can not combine OAPISURFACE_RENDERTARGET | OAPISURFACE_SYSMEM");
		attrib-=OAPISURFACE_SYSMEM;
	}

	if (attrib==OAPISURFACE_SKETCHPAD) {
		attrib |= OAPISURFACE_RENDERTARGET;
		attrib &= ~(OAPISURFACE_SYSMEM|OAPISURFACE_GDI);
	}

	D3D9ClientSurface *surf = new D3D9ClientSurface(pd3dDevice, fname);
	surf->LoadSurface(fname, attrib);
	return surf;
}

// ==============================================================

void D3D9Client::clbkReleaseTexture(SURFHANDLE hTex)
{
	_TRACE;
	__TRY {

		if (texmgr->IsInRepository(hTex)) return;	// Do not release surfaces stored in repository

		if (SURFACE(hTex)->Release()) {
			DWORD nmesh = MeshCatalog->CountEntries();
			for (DWORD i=0;i<nmesh;i++) {
				D3D9Mesh *mesh = (D3D9Mesh *)MeshCatalog->Get(i);
				if (mesh) if (mesh->HasTexture(hTex)) {
					LogErr("Something is attempting to delete a texture (%s) that is currently used by a mesh. Attempt rejected to prevent a CTD",SURFACE(hTex)->GetName());
					return;
				}
			}
			delete SURFACE(hTex);
		}
	}

	__EXCEPT(ExcHandler(GetExceptionInformation()))
	{
		LogErr("Exception in clbkReleaseTexture()");
		EmergencyShutdown();
		FatalAppExitA(0,"Critical error has occured. See Orbiter.log for details");
	}
}

// ==============================================================

SURFHANDLE D3D9Client::clbkCreateSurfaceEx(DWORD w, DWORD h, DWORD attrib)
{
	_TRACE;
	D3D9ClientSurface *surf = new D3D9ClientSurface(pd3dDevice, "clbkCreateSurfaceEx");
	surf->CreateSurface(w, h, attrib);
	return surf;
}


// =======================================================================

SURFHANDLE D3D9Client::clbkCreateSurface(DWORD w, DWORD h, SURFHANDLE hTemplate)
{
	_TRACE;
	D3D9ClientSurface *surf = new D3D9ClientSurface(pd3dDevice, "clbkCreateSurface");
	surf->MakeEmptySurfaceEx(w, h);
	return surf;
}

// =======================================================================

SURFHANDLE D3D9Client::clbkCreateSurface(HBITMAP hBmp)
{
	_TRACE;
	SURFHANDLE hSurf = GraphicsClient::clbkCreateSurface(hBmp);
	return hSurf;
}

// =======================================================================

SURFHANDLE D3D9Client::clbkCreateTexture(DWORD w, DWORD h)
{
	_TRACE;
	D3D9ClientSurface *pSurf = new D3D9ClientSurface(pd3dDevice, "clbkCreateTexture");
	// DO NOT USE ALPHA
	pSurf->MakeEmptyTextureEx(w, h);
	return (SURFHANDLE)pSurf;
}

// =======================================================================

void D3D9Client::clbkIncrSurfaceRef(SURFHANDLE surf)
{
	_TRACE;
	if (surf==NULL) { LogErr("D3D9Client::clbkIncrSurfaceRef() Input Surface is NULL");	return; }
	SURFACE(surf)->IncRef();
}

// =======================================================================

bool D3D9Client::clbkReleaseSurface(SURFHANDLE surf)
{
	_TRACE;

	__TRY {

		if (surf==NULL) { LogErr("D3D9Client::clbkReleaseSurface() Input Surface is NULL");	return false; }

		if (texmgr->IsInRepository(surf)) return false;	// Do not release surfaces stored in repository

		bool bRel = SURFACE(surf)->Release();

		if (bRel) {

			DWORD nmesh = MeshCatalog->CountEntries();

			for (DWORD i=0;i<nmesh;i++) {
				D3D9Mesh *mesh = (D3D9Mesh *)MeshCatalog->Get(i);
				if (mesh) if (mesh->HasTexture(surf)) {
					LogErr("Orbiter is attempting to delete a texture (%s) that is currently used by a mesh. Attempt rejected to prevent a CTD",SURFACE(surf)->GetName());
					return true;
				}
			}
			delete SURFACE(surf);
		}
		return bRel;
	}

	__EXCEPT(ExcHandler(GetExceptionInformation()))
	{
		LogErr("Exception in clbkReleaseSurface()");
		EmergencyShutdown();
		FatalAppExitA(0,"Critical error has occured. See Orbiter.log for details");
	}

	return false;
}

// =======================================================================

bool D3D9Client::clbkGetSurfaceSize(SURFHANDLE surf, DWORD *w, DWORD *h)
{
	_TRACE;
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

	if (tgt==RENDERTGT_NONE) {
		pBltGrpTgt = NULL;
		return -2;
	}

	if (tgt==RENDERTGT_MAINWINDOW) pBltGrpTgt = pFramework->GetBackBufferHandle();
	else						   pBltGrpTgt = tgt;

	if (SURFACE(pBltGrpTgt)->IsRenderTarget()==false) {
		pBltGrpTgt = NULL;
		return -3;
	}

	return 0;
}

int D3D9Client::clbkEndBltGroup()
{
	_TRACE;
	if (pBltGrpTgt==NULL) return -2;
	SURFACE(pBltGrpTgt)->EndBlitGroup(); // Flush queue and release GPU
	pBltGrpTgt = NULL;
	return 0;
}


bool D3D9Client::CheckBltGroup(SURFHANDLE src, SURFHANDLE tgt) const
{
	if (tgt==pBltGrpTgt) {
		if (SURFACE(src)->pTex!=NULL) {
			if (SURFACE(src)->ColorKey) { // Use blit groups only for color keyed source surfaces
				if (SURFACE(pBltGrpTgt)->BeginBlitGroup()==S_OK) return true;
			}
		}
	}
	// Flush queue, release GPU and disable blit group
	SURFACE(pBltGrpTgt)->EndBlitGroup();
	pBltGrpTgt = NULL;
	return false;
}



bool D3D9Client::clbkBlt(SURFHANDLE tgt, DWORD tgtx, DWORD tgty, SURFHANDLE src, DWORD flag) const
{
	_TRACE;

	__TRY {

		if (src==NULL) { LogErr("D3D9Client::clbkBlt() Source surface is NULL"); return false; }
		if (tgt==NULL) tgt = pFramework->GetBackBufferHandle();

		int w = SURFACE(src)->GetWidth();
		int h = SURFACE(src)->GetHeight();

		RECT rs = { 0, 0, w, h };
		RECT rt = { tgtx, tgty, tgtx+w, tgty+h };

		if (pBltGrpTgt) {
			if (CheckBltGroup(src,tgt)) SURFACE(pBltGrpTgt)->AddQueue(SURFACE(src), &rs, &rt);
			else 						SURFACE(tgt)->CopyRect(SURFACE(src), &rs, &rt, flag);
		}
		else SURFACE(tgt)->CopyRect(SURFACE(src), &rs, &rt, flag);
	}

	__EXCEPT(ExcHandler(GetExceptionInformation()))
	{
		LogErr("Exception in clbkBlt(0x%X, %u,%u, 0x%X, 0x%X)",tgt, tgtx, tgty, src, flag);
		FatalAppExitA(0,"Critical error has occured. See Orbiter.log for details");
	}
	return true;
}

// =======================================================================

bool D3D9Client::clbkBlt(SURFHANDLE tgt, DWORD tgtx, DWORD tgty, SURFHANDLE src, DWORD srcx, DWORD srcy, DWORD w, DWORD h, DWORD flag) const
{
	_TRACE;

	__TRY {
		if (src==NULL) { LogErr("D3D9Client::clbkBlt() Source surface is NULL"); return false; }
		if (tgt==NULL) tgt = pFramework->GetBackBufferHandle();

		RECT rs = { srcx, srcy, srcx+w, srcy+h };
		RECT rt = { tgtx, tgty, tgtx+w, tgty+h };

		if (pBltGrpTgt) {
			if (CheckBltGroup(src,tgt)) SURFACE(pBltGrpTgt)->AddQueue(SURFACE(src), &rs, &rt);
			else 						SURFACE(tgt)->CopyRect(SURFACE(src), &rs, &rt, flag);
		}
		else SURFACE(tgt)->CopyRect(SURFACE(src), &rs, &rt, flag);
	}

	__EXCEPT(ExcHandler(GetExceptionInformation()))
	{
		LogErr("Exception in clbkBlt(0x%X, %u,%u, 0x%X, %u,%u,%u,%u, 0x%X)",tgt,tgtx,tgty, src, srcx, srcy, w, h, flag);
		FatalAppExitA(0,"Critical error has occured. See Orbiter.log for details");
	}
	return true;
}

// =======================================================================

bool D3D9Client::clbkScaleBlt (SURFHANDLE tgt, DWORD tgtx, DWORD tgty, DWORD tgtw, DWORD tgth,
                               SURFHANDLE src, DWORD srcx, DWORD srcy, DWORD srcw, DWORD srch, DWORD flag) const
{
	_TRACE;

	__TRY {
		if (src==NULL) { LogErr("D3D9Client::clbkScaleBlt() Source surface is NULL"); return false; }
		if (tgt==NULL) tgt = pFramework->GetBackBufferHandle();

		RECT rs = { srcx, srcy, srcx+srcw, srcy+srch };
		RECT rt = { tgtx, tgty, tgtx+tgtw, tgty+tgth };

		if (pBltGrpTgt) {
			if (CheckBltGroup(src,tgt)) SURFACE(pBltGrpTgt)->AddQueue(SURFACE(src), &rs, &rt);
			else 						SURFACE(tgt)->CopyRect(SURFACE(src), &rs, &rt, flag);
		}
		else SURFACE(tgt)->CopyRect(SURFACE(src), &rs, &rt, flag);
	}

	__EXCEPT(ExcHandler(GetExceptionInformation()))
	{
		LogErr("Exception in clbkScaleBlt(0x%X, %u,%u,%u,%u, 0x%X, %u,%u,%u,%u, 0x%X)",tgt, tgtx,tgty,tgtw,tgth, src, srcx, srcy, srcw, srch, flag);
		FatalAppExitA(0,"Critical error has occured. See Orbiter.log for details");
	}

	return true;
}

// =======================================================================

bool D3D9Client::clbkCopyBitmap(SURFHANDLE pdds, HBITMAP hbm, int x, int y, int dx, int dy)
{
	_TRACE;
	return GraphicsClient::clbkCopyBitmap(pdds, hbm, x,y,dx,dy);;
}

// =======================================================================

bool D3D9Client::clbkFillSurface(SURFHANDLE tgt, DWORD col) const
{
	_TRACE;
	if (tgt==NULL) tgt = pFramework->GetBackBufferHandle();
	bool ret = SURFACE(tgt)->Clear(col);
	return ret;
}

// =======================================================================

bool D3D9Client::clbkFillSurface(SURFHANDLE tgt, DWORD tgtx, DWORD tgty, DWORD w, DWORD h, DWORD col) const
{
	_TRACE;
	if (tgt==NULL) tgt = pFramework->GetBackBufferHandle();
	RECT r = {tgtx, tgty, tgtx+w, tgty+h};
	bool ret = SURFACE(tgt)->Fill(&r, col);
	return ret;
}

#pragma endregion

// =======================================================================
// Constellation name functions
// =======================================================================

DWORD D3D9Client::GetConstellationMarkers(const LABELSPEC **cm_list) const
{
	if ( !g_cm_list ) {
		#pragma pack(1)
		// File entry struct
		typedef struct {
			double lng;    ///< longitude
			double lat;    ///< latitude
			char   abr[3]; ///< abbreviation (short name)
			size_t len;    ///< length of 'fullname'
		} ConstellEntry;
		#pragma pack()

		FILE* file = NULL;
		const size_t e_size = sizeof(ConstellEntry);
		const float sphere_r = 1e6f; // the actual render distance for the celestial sphere
		                             // is irrelevant, since it is rendered without z-buffer,
		                             // but it must be within the frustum limits - check this
		                             // in case the near and far planes are dynamically changed!

		if ( 0 != fopen_s(&file, ".\\Constell2.bin", "rb") || file == NULL ) {
			LogErr("Could not open 'Constell2.bin'");
			return 0;
		}

		ConstellEntry f_entry;
		LABELSPEC *p_out;

		// Get number of labels from file
		while ( !feof(file) && (1 == fread(&f_entry, e_size, 1 , file)) ) {
			++g_cm_list_count;
			fseek(file, f_entry.len, SEEK_CUR);
		}

		rewind(file);

		g_cm_list = new LABELSPEC[g_cm_list_count]();

		for (p_out = g_cm_list; !feof(file); ++p_out) {
			if ( 1 == fread(&f_entry, e_size, 1 , file)) {
				p_out->label[0] = new char[f_entry.len+1]();
				p_out->label[1] = new char[4]();
				// position
				double xz = sphere_r * cos(f_entry.lat);
				p_out->pos.x = xz * cos(f_entry.lng);
				p_out->pos.z = xz * sin(f_entry.lng);
				p_out->pos.y = sphere_r * sin(f_entry.lat);
				// fullname
				fread(p_out->label[0], sizeof(char), f_entry.len, file);
				// shortname
				p_out->label[1][0] = f_entry.abr[0];
				p_out->label[1][1] = f_entry.abr[1];
				p_out->label[1][2] = f_entry.abr[2];
			}
		}

		fclose(file);
	}

	*cm_list = g_cm_list;

	return g_cm_list_count;
}

// =======================================================================
// GDI functions
// =======================================================================

HDC D3D9Client::clbkGetSurfaceDC(SURFHANDLE surf)
{
	_TRACE;
	if (surf==NULL) surf = pFramework->GetBackBufferHandle();
	HDC hDC = SURFACE(surf)->GetDC();
	return hDC;
}

// =======================================================================

void D3D9Client::clbkReleaseSurfaceDC(SURFHANDLE surf, HDC hDC)
{
	_TRACE;
	if (hDC==NULL) { LogErr("D3D9Client::clbkReleaseSurfaceDC() Input hDC is NULL"); return; }
	if (surf==NULL) surf = pFramework->GetBackBufferHandle();
	SURFACE(surf)->ReleaseDC(hDC);
}

// =======================================================================

bool D3D9Client::clbkSplashLoadMsg (const char *msg, int line)
{
	_TRACE;
	return OutputLoadStatus (msg, line);
}

// =======================================================================

LPD3D9CLIENTSURFACE D3D9Client::GetDefaultTexture() const
{
	return pDefaultTex;
}

// =======================================================================

HWND D3D9Client::GetWindow()
{
	return pFramework->GetRenderWindow();
}

// =======================================================================

LPD3D9CLIENTSURFACE D3D9Client::GetBackBufferHandle() const
{
	_TRACE;
	return SURFACE(pFramework->GetBackBufferHandle());
}

// =======================================================================

void D3D9Client::RegisterDissolveMap(SURFHANDLE hSrf)
{
	if (iDisl<16) {
		for (DWORD i=0;i<iDisl;i++) if (pDislMapList[i]==hSrf) return;
		pDislMapList[iDisl] = hSrf;
		iDisl++;
	}
}

// =======================================================================

SURFHANDLE D3D9Client::GetDissolveMap(DWORD idx) const
{
	if (idx<16) return pDislMapList[idx];
	return NULL;
}

// =======================================================================

int D3D9Client::GetIndexOfDissolveMap(SURFHANDLE hSrf) const
{
	for (DWORD i=0;i<iDisl;i++) if (pDislMapList[i]==hSrf) return i;
	return -1;
}

// =======================================================================

void D3D9Client::MakeRenderProcCall(SURFHANDLE hSrf, DWORD id)
{
	for (DWORD i = 0; i < RenderProc.size(); i++) {
		if (RenderProc[i].id == id) {
			RenderProc[i].proc(hSrf);
		}
	}
}

// =======================================================================

bool D3D9Client::RegisterRenderProc(__ogciRenderProc proc, DWORD id)
{
	if (id == 0) {
		for (DWORD i = 0; i < RenderProc.size(); i++) {
			if (RenderProc[i].proc == proc) {
				RenderProc.erase(RenderProc.begin() + i);
				return true;
			}
		}
	}
	else {
		RenderProcData data;
		data.proc = proc;
		data.id = id;
		RenderProc.push_back(data);
		return true;
	}
	return false;
}

// =======================================================================

void D3D9Client::WriteLog(const char *msg) const
{
	_TRACE;
	char cbuf[256];
	sprintf_s(cbuf, 256, "D3D9: %s", msg);
	oapiWriteLog(cbuf);
}

// =======================================================================

bool D3D9Client::OutputLoadStatus(const char *txt, int line)
{
	if (bRunning) return false;

	if (line==1) strcpy_s(pLoadItem, 127, txt);
	if (line==0) strcpy_s(pLoadLabel, 127, txt);

	if (bSkepchpadOpen==false && pTextScreen) {

		if (pd3dDevice->TestCooperativeLevel()!=S_OK) {
			LogErr("TestCooperativeLevel() Failed");
			return false;
		}

		RECT txt = { loadd_x, loadd_y, loadd_x+loadd_w, loadd_y+loadd_h };

		pd3dDevice->StretchRect(pSplashScreen, &txt, pTextScreen, NULL, D3DTEXF_POINT);

		HDC hDC;
		HR(pTextScreen->GetDC(&hDC));

		HFONT hO = (HFONT)SelectObject(hDC, hLblFont1);
		SetTextColor(hDC, 0xE0A0A0);
		SetBkMode(hDC,TRANSPARENT);
		SetTextAlign(hDC, TA_LEFT|TA_TOP);

		TextOut(hDC, 2, 2, pLoadLabel, strlen(pLoadLabel));

		SelectObject(hDC, hLblFont2);
		TextOut(hDC, 2, 36, pLoadItem, strlen(pLoadItem));

		HPEN pen = CreatePen(PS_SOLID,1,0xE0A0A0);
		HPEN po = (HPEN)SelectObject(hDC, pen);

		MoveToEx(hDC, 0, 32, NULL);
		LineTo(hDC, loadd_w, 32);

		SelectObject(hDC, po);
		SelectObject(hDC, hO);
		DeleteObject(pen);

		HR(pTextScreen->ReleaseDC(hDC));
		HR(pd3dDevice->StretchRect(pSplashScreen, NULL, pBackBuffer, NULL, D3DTEXF_POINT));
		HR(pd3dDevice->StretchRect(pTextScreen, NULL, pBackBuffer, &txt, D3DTEXF_POINT));
		
		IDirect3DSwapChain9 *pSwap;

		if (pd3dDevice->GetSwapChain(0, &pSwap)==S_OK) { 
			pSwap->Present(0, 0, 0, 0, D3DPRESENT_DONOTWAIT);
			pSwap->Release();
			return true;
		}

		// Prevent "Not Responding" during loading
		MSG msg;
		while (PeekMessage(&msg, NULL, 0, 0, PM_REMOVE)) DispatchMessage(&msg);
	}
	return false;
}

// =======================================================================

void D3D9Client::SplashScreen()
{

	loadd_x = 279*viewW/1280;
	loadd_y = 545*viewH/800;
	loadd_w = viewW/3;
	loadd_h = 80;

	HR(pd3dDevice->TestCooperativeLevel());
	HR(pd3dDevice->Clear(0, NULL, D3DCLEAR_TARGET|D3DCLEAR_ZBUFFER|D3DCLEAR_STENCIL, 0x0, 1.0f, 0L));
	HR(pd3dDevice->CreateOffscreenPlainSurface(loadd_w, loadd_h, D3DFMT_X8R8G8B8, D3DPOOL_DEFAULT, &pTextScreen, NULL));
	HR(pd3dDevice->CreateOffscreenPlainSurface(viewW, viewH, D3DFMT_X8R8G8B8, D3DPOOL_DEFAULT, &pSplashScreen, NULL));

	D3DXIMAGE_INFO Info;

	HMODULE hOrbiter =  GetModuleHandleA("orbiter.exe");
	HRSRC hRes = FindResourceA(hOrbiter, MAKEINTRESOURCEA(292), "IMAGE");
	HGLOBAL hImage = LoadResource(hOrbiter, hRes);
	LPVOID pData = LockResource(hImage);
	DWORD size = SizeofResource(hOrbiter, hRes);

	HR(D3DXLoadSurfaceFromFileInMemory(pSplashScreen, NULL, NULL, pData, size, NULL, D3DX_FILTER_LINEAR, 0, &Info));

	HDC hDC;
	HR(pSplashScreen->GetDC(&hDC));

	LOGFONTA fnt; memset2((void *)&fnt, 0, sizeof(LOGFONT));

	fnt.lfHeight		 = 18;
	fnt.lfWeight		 = 700;
	fnt.lfCharSet		 = ANSI_CHARSET;
	fnt.lfOutPrecision	 = OUT_DEFAULT_PRECIS;
	fnt.lfClipPrecision	 = CLIP_DEFAULT_PRECIS;
	fnt.lfQuality		 = ANTIALIASED_QUALITY;
	fnt.lfPitchAndFamily = DEFAULT_PITCH;
	strcpy_s(fnt.lfFaceName, "Courier New");

	HFONT hF = CreateFontIndirect(&fnt);

	HFONT hO = (HFONT)SelectObject(hDC, hF);
	SetTextColor(hDC, 0xE0A0A0);
	SetBkMode(hDC,TRANSPARENT);

	char *months[]={"???","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","???"};

	DWORD d = oapiGetOrbiterVersion();
	DWORD y = d/10000; d-=y*10000;
	DWORD m = d/100; d-=m*100;
	if (m>12) m=0;

#ifdef _DEBUG
	char dataA[]={"D3D9Client Beta 22 Debug Build [" __DATE__ "]"};
#else
	char dataA[]={"D3D9Client Beta 22 Build [" __DATE__ "]"};
#endif

	char dataB[128]; sprintf_s(dataB,128,"Build %s %u 20%u [%u]", months[m], d, y, oapiGetOrbiterVersion());
	char dataC[]={"Warning: Running in GDI compatibility mode. Expect a low framerate."};
	char dataD[]={"Warning: Config folder not present in /Modules/Server/. Please create symbolic link."};

	int xc = viewW*814/1280;
	int yc = viewH*545/800;

	TextOut(hDC, xc, yc + 0*20, "ORBITER Space Flight Simulator",30);
	TextOut(hDC, xc, yc + 1*20, dataB, strlen(dataB));
	TextOut(hDC, xc, yc + 2*20, dataA, strlen(dataA));

	if (bGDIBB) {
		SetTextAlign(hDC, TA_CENTER);
		TextOut(hDC, viewW/2, viewH-30, dataC, strlen(dataC));
	}

	DWORD cattrib = GetFileAttributes("Modules/Server/Config");

	if ((cattrib&0x10)==0 || cattrib==INVALID_FILE_ATTRIBUTES) {
		SetTextAlign(hDC, TA_CENTER);
		TextOut(hDC, viewW/2, viewH-50, dataD, strlen(dataD));
	}

	SelectObject(hDC, hO);
	DeleteObject(hF);

	HR(pSplashScreen->ReleaseDC(hDC));


	RECT src = { loadd_x, loadd_y, loadd_x+loadd_w, loadd_y+loadd_h };
	pd3dDevice->StretchRect(pSplashScreen, &src, pTextScreen, NULL, D3DTEXF_POINT);
	pd3dDevice->StretchRect(pSplashScreen, NULL, pBackBuffer, NULL, D3DTEXF_POINT);
	pd3dDevice->Present(0, 0, 0, 0);
}


#pragma region Drawing_(Sketchpad)_Interface


double sketching_time;

// ==============================================================
// 2D Drawing Interface
//
oapi::Sketchpad *D3D9Client::clbkGetSketchpad(SURFHANDLE surf)
{
	_TRACE;
	
	sketching_time = D3D9GetTime();
	if (bSkepchpadOpen) LogErr("an other Sketchpad is already open");

	if (surf == NULL) surf = GetBackBufferHandle();

	LogOk("Creating SketchPad for surface (0x%X)(%u,%u)...", surf, SURFACE(surf)->GetWidth(), SURFACE(surf)->GetHeight());

	// Sketching to backbuffer -----------------------------------------------------
	//
	if (SURFACE(surf)->IsBackBuffer()) {
		bool bScene = GetScene()->IsRendering();
		if (bScene==true || bGDIBB==false) {
			if (bScene==false) pd3dDevice->BeginScene(); // bScene is true between BeginScene() and EndScene() of the main rendering routine
			bSkepchpadOpen = true;
			return new D3D9Pad(surf);
		}
	}
	
	// Auto-select a sketchpad -----------------------------------------------------
	//
	if (SURFACE(surf)->IsRenderTarget()) {
		bSkepchpadOpen = true;
		return new D3D9Pad(surf);	
	}
	else {
		HDC hDC = clbkGetSurfaceDC(surf);
		if (hDC) {
			bSkepchpadOpen = true;
			return new GDIPad(surf, hDC);
		}
	}
	
	return NULL;
}


void D3D9Client::clbkReleaseSketchpad(oapi::Sketchpad *sp)
{
	_TRACE;
	bSkepchpadOpen = false;

	SURFHANDLE surf = sp->GetSurface();

	if (surf) {
		if (SURFACE(surf)->GetSketchPadMode()==SKETCHPAD_GDI) {
			GDIPad *gdip = (GDIPad*)sp;
			clbkReleaseSurfaceDC(gdip->GetSurface(), gdip->GetDC());
			delete gdip;
		}
		if (SURFACE(surf)->GetSketchPadMode()==SKETCHPAD_DIRECTX) {
			if (SURFACE(surf)->IsBackBuffer()) {
				if (!GetScene()->IsRendering()) pd3dDevice->EndScene();
			}
			D3D9Pad *gdip = (D3D9Pad*)sp;
			delete gdip;
		}
	}
	sketching_time = D3D9GetTime() - sketching_time;
	LogOk("...SketchPad Released.  Time spend %.2fms", sketching_time*1e-3);
}

Font *D3D9Client::clbkCreateFont(int height, bool prop, const char *face, Font::Style style, int orientation) const
{
	_TRACE;
	return *g_fonts.insert(new D3D9PadFont(height, prop, face, style, orientation)).first;
}

void D3D9Client::clbkReleaseFont(Font *font) const
{
	_TRACE;
	g_fonts.erase(font);
	delete ((D3D9PadFont*)font);
}

Pen *D3D9Client::clbkCreatePen(int style, int width, DWORD col) const
{
	_TRACE;
	return *g_pens.insert(new D3D9PadPen(style, width, col)).first;
}

void D3D9Client::clbkReleasePen(Pen *pen) const
{
	_TRACE;
	g_pens.erase(pen);
	delete ((D3D9PadPen*)pen);
}

Brush *D3D9Client::clbkCreateBrush(DWORD col) const
{
	_TRACE;
	return *g_brushes.insert(new D3D9PadBrush(col)).first;
}

void D3D9Client::clbkReleaseBrush(Brush *brush) const
{
	_TRACE;
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

VisObject::~VisObject ()
{
}
