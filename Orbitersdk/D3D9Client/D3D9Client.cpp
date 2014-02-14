// ==============================================================
// D3D9Client.cpp
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Released under GNU General Public License
// Copyright (C) 2007 Martin Schweiger
//				 2012 Jarmo Nikkanen
// ==============================================================


#define STRICT 1
#define ORBITER_MODULE

#include "orbitersdk.h"
#include "D3D9Client.h"
#include "D3D9Config.h"
#include "D3D9Extra.h"
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

using namespace oapi;

HINSTANCE g_hInst = 0;
D3D9Client *g_client = 0;
D3D9Catalog *MeshCatalog = 0;
D3D9Catalog *TileCatalog = 0;
D3D9Catalog *SurfaceCatalog = 0;
DWORD uCurrentMesh = 0;
vObject *pCurrentVisual = 0;

#ifdef _NVAPI_H
 StereoHandle pStereoHandle = 0;
#endif

bool bSkepchpadOpen = false;

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
#endif

	//_CrtSetBreakAlloc(17549);

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
#endif

	Config			= new D3D9Config();
	TileCatalog		= new D3D9Catalog("TileCatalog");
	MeshCatalog		= new D3D9Catalog("MeshCatalog");
	SurfaceCatalog  = new D3D9Catalog("SurfaceCatalog");

	DebugControls::Create();

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
	strcpy(ScenarioName, "(none selected)");
}

// ==============================================================

D3D9Client::~D3D9Client()
{
	LogAlw("D3D9Client destructor called");
	SAFE_DELETE(vtab);
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
	_TRACER;
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
	_TRACER;

	LogAlw("================ clbkCreateRenderWindow ===============");

	uEnableLog		 = Config->DebugLvl;
	pSplashScreen    = NULL;
	pBackBuffer      = NULL;
	pTextScreen      = NULL;
	hRenderWnd       = NULL;
	pDefaultTex		 = NULL;
	hLblFont1		 = NULL;
	hLblFont2		 = NULL;
	pSkinNames		 = NULL;
	pSkinBuffer		 = NULL;
	bControlPanel    = false;
	bFullscreen      = false;
	bFailed			 = false;
	bRunning		 = false;
	bScene			 = false;
	bHalt			 = false;
	bVertexTex		 = false;
	bSkpGDI			 = true;
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

#ifdef OAPI_BETA
	surfBltTgt		 = NULL;	// This variable is not used, set it to NULL anyway
#endif

	memset2(&stats, 0, sizeof(stats));
	memset2(pDislMapList, 0, 16*sizeof(SURFHANDLE));

	D3DXMatrixIdentity(&ident);

	sprintf_s(oapiDebugString(),255,"");

	TileCatalog->Clear();
	MeshCatalog->Clear();
	SurfaceCatalog->Clear();

	hRenderWnd = GraphicsClient::clbkCreateRenderWindow();

	LogMsg("Window Handle = 0x%X",hRenderWnd);
	SetWindowText(hRenderWnd, "[D3D9Client]");

	LogOk("Starting to initialize device and 3D environment...");

	pFramework = new CD3DFramework9();

	if (pFramework->GetDirect3D()==NULL) return NULL;

	WriteLog("[DirectX 9 Initialized]");

	HRESULT hr = pFramework->Initialize(hRenderWnd, GetVideoData());

	if (hr!=S_OK) {
		WriteLog("ERROR: Failed to initialize 3D Framework");
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

	if (bGDIBB) Config->SketchpadMode = 1;

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

	pDefaultTex = SURFACE(clbkLoadTexture("Null.dds"));
	if (pDefaultTex==NULL) LogErr("CRITICAL !!   Null.dds not found");

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

	SetLabel("Building Shader Programs...");

	SetItem("D3D9Client.fx");
	D3D9Effect::D3D9TechInit(this, pd3dDevice, fld);

	// Device-specific initialisations

	TileManager::GlobalInit(this);
	RingManager::GlobalInit(this);
	HazeManager::GlobalInit(this);
	D3D9ParticleStream::GlobalInit(this);
	CSphereManager::GlobalInit(this);
	vStar::GlobalInit(this);
	vObject::GlobalInit(this);
	vVessel::GlobalInit(this);
	OapiExtension::GlobalInit(*Config);

	SetItem("SceneTech.fx");
	Scene::D3D9TechInit(pd3dDevice, fld);

	// Create scene instance
	scene = new Scene(this, viewW, viewH);

	WriteLog("[D3D9Client Initialized]");
	LogOk("...3D environment initialised");
	SetLabel("Loading Textures...");

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


	// Create A skin database -------------------------------------------------------------
	//
	AutoFile file;

	char cbuf[256];
	sprintf_s(cbuf,256,"%sGC\\VesselSkin.cfg",OapiExtension::GetConfigDir());

	fopen_s(&file.pFile, cbuf, "r");

	if (!file.IsInvalid()) {

		fseek(file.pFile, 0, SEEK_END);

		DWORD size = ftell(file.pFile);
		fseek(file.pFile, 0, SEEK_SET);

		pSkinNames = new LPCHAR[256]();
		pSkinBuffer = new char[size]();
		nSkins = 0;

		char *_pSkinBuffer = pSkinBuffer; // we don't want to 'move' the original address!
		while (true) {

			int rv = fgets2(cbuf, 256, file.pFile, 0x08);
			if (rv<0) break;
			if (rv==0) continue;

			pSkinNames[nSkins++] = _pSkinBuffer;
			int k=0;
			while (true) {
				*_pSkinBuffer = cbuf[k];
				 _pSkinBuffer++;
				 if (cbuf[k]==0) break;
				 k++;
			}
		}
	}
	else {
		LogErr("Failed to open a vessel skin configuration file");
	}

	return hRenderWnd;
}


// ==============================================================
// This is called when the simulation is ready to go but the clock
const char *D3D9Client::GetSkinFileLine(DWORD idx) const
{
	if (idx<nSkins && pSkinNames) return pSkinNames[idx];
	return NULL;
}


// ==============================================================
// This is called when the simulation is ready to go but the clock
// is not yet ticking
//
void D3D9Client::clbkPostCreation()
{
	_TRACER;
	LogAlw("================ clbkPostCreation ===============");

	if (parser) return;

	parser = new FileParser(ScenarioName);
	parser->LogContent();

	if (scene) scene->Initialise();
	WriteLog("[Scene Initialized]");
}


// ==============================================================

void D3D9Client::clbkCloseSession(bool fastclose)
{
	_TRACER;
	__TRY {

		LogAlw("================ clbkCloseSession ===============");

		if (TileBuffer::ShutDown()==false) {
			LogErr("Failed to shutdown tile buffer");
		}
		else LogOk("Tile buffer shutdown successful");

		DebugControls::SetVisual(NULL);
		D3D9Effect::ShutDown();

		DWORD cnt = MeshCatalog->CountEntries();

		for (DWORD i=0;i<cnt;i++) {
			D3D9Mesh *x = (D3D9Mesh*)MeshCatalog->Get(i);
			if (x) x->DumpTextures();
		}

		GraphicsClient::clbkCloseSession(fastclose);

		SAFE_DELETE(parser);

		LogAlw("================ Deleting Scene ===============");
		Scene::GlobalExit();
		SAFE_DELETE(scene);
		LogAlw("================ Deleting Mesh Manager ===============");
		SAFE_DELETE(meshmgr);
		LogOk("================ clbkCloseSession Success ===============");
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
	_TRACER;
	LogAlw("================ clbkDestroyRenderWindow ===============");

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

		delete[] pSkinBuffer;
		delete[] pSkinNames;

		LogAlw("================ Clearing Texture Repository ===============");
		SAFE_DELETE(texmgr);
		LogAlw("================ Texture Repository Cleared ===============");

		HazeManager::GlobalExit();
		TileManager::GlobalExit();
		D3D9ParticleStream::GlobalExit();
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

		// Check surface catalog --------------------------------------------------------------------------------------
		//
		DWORD n = SurfaceCatalog->CountEntries();

		if (n) LogErr("UnDeleted Surface(s) Detected");

		while (n) {
			LPD3D9CLIENTSURFACE pSurf = SURFACE(SurfaceCatalog->Get(0));
			if (pSurf) {
				LogErr("Surface 0x%X (%s) (%u,%u) Type=%u, Creation=%u", pSurf, pSurf->GetName(), pSurf->GetWidth(), pSurf->GetHeight(), pSurf->Type, pSurf->Creation);
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
		if (nt) LogErr("Tile Catalog contains %u unreleased entries",nt);
		else    LogAlw("[OK] All Tiles are deleted");


		SurfaceCatalog->Clear();
		MeshCatalog->Clear();
		TileCatalog->Clear();

		pFramework->DestroyObjects();

		SAFE_DELETE(pFramework);

		// Close Render Window -----------------------------------------

		bRunning		 = false;

		GraphicsClient::clbkDestroyRenderWindow(fastclose);

		hRenderWnd		 = NULL;
		pd3dDevice		 = NULL;
		bFailed			 = false;
		viewW = viewH    = 0;
		viewBPP          = 0;

		LogOk("================ clbkDestroyRenderWindow Success ===============");
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
	_TRACER;

	if (!parser) clbkPostCreation();

	__TRY {
		if (bFailed==false && bRunning) scene->Update();
	}
	__EXCEPT(ExcHandler(GetExceptionInformation()))
	{
		LogErr("Exception in clbkUpdate()");
		EmergencyShutdown();
		FatalAppExitA(0,"Critical error has occured. See Orbiter.log for details");
	}
}

// ==============================================================

void D3D9Client::VisualsCreated()
{
	bRunning = true;
	LogAlw("=============== Loading Completed and Visuals Created ================");
}

// ==============================================================

double frame_time = 0.0;

void D3D9Client::clbkRenderScene()
{
	_TRACER;
	bScene = false;

	char Label[64];

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

		LogOk("== Begin Scene ==");

		scene->UpdateCamVis();	// Update Camera and Visuals

		bScene = true;
		scene->RenderMainScene();		// Render the main scene
		bScene = false;

		Label[0]=0;

		VESSEL *hVes = oapiGetFocusInterface();

		if (hVes) {
			if (hVes->Recording()) strcpy_s(Label,64,"Recording");
			if (hVes->Playback()) strcpy_s(Label,64,"Replay");
		}

		if (oapiGetPause()) strcpy_s(Label,64,"Paused");

		if (Label[0]!=0) {
			pd3dDevice->BeginScene();
			RECT rect2 = {0,viewH-60,viewW,viewH-20};
			pFramework->GetLargeFont()->DrawTextA(0, Label, 6, &rect2, DT_CENTER | DT_TOP, D3DCOLOR_XRGB(0, 0, 0));
			rect2.left-=4; rect2.top-=4;
			pFramework->GetLargeFont()->DrawTextA(0, Label, 6, &rect2, DT_CENTER | DT_TOP, D3DCOLOR_XRGB(255, 255, 255));
			pd3dDevice->EndScene();
		}

		if (bControlPanel) RenderControlPanel();

		memset2(&stats, 0, 96);
	}

	__EXCEPT(ExcHandler(GetExceptionInformation()))
	{
		LogErr("Exception in clbkRenderScene()");
		EmergencyShutdown();
		FatalAppExitA(0,"Critical error has occured. See Orbiter.log for details");
	}

	frame_time = D3D9GetTime();
}

// ==============================================================

void D3D9Client::clbkTimeJump(double simt, double simdt, double mjd)
{
	_TRACER;
	GraphicsClient::clbkTimeJump (simt, simdt, mjd);
}

// ==============================================================

double framer_rater_limit = 0.0;

bool D3D9Client::clbkDisplayFrame()
{
	static int iRefrState = 0;
	double time = D3D9GetTime();
	frame_time = time - frame_time;
	stats.Frame += frame_time;
	if (frame_time>stats.FramePeak) stats.FramePeak = frame_time;

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
	GetDevice()->SetDialogBoxMode(true);
}

// =======================================================================

bool D3D9Client::RenderWithPopupWindows()
{
	_TRACER;

	const HWND *hPopupWnd;
	DWORD count = GetPopupList(&hPopupWnd);

	if (bFullscreen) {
		if (count) GetDevice()->SetDialogBoxMode(true);
		else       GetDevice()->SetDialogBoxMode(false);
	}

	// Let the OapiExtension manager know about this..
	OapiExtension::HandlePopupWindows(hPopupWnd, count);

	for (DWORD i=0;i<count;i++) {
		DWORD val = GetWindowLongA(hPopupWnd[i], GWL_STYLE);
		if ((val&WS_SYSMENU)==0) {
			SetWindowLongA(hPopupWnd[i], GWL_STYLE, val|WS_SYSMENU);
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
	_TRACER;
	ExhaustStream *es = new ExhaustStream (this, hVessel, lvl, ref, dir, pss);
	scene->AddParticleStream (es);
	return es;
}

// =======================================================================

ParticleStream *D3D9Client::clbkCreateExhaustStream(PARTICLESTREAMSPEC *pss,
	OBJHANDLE hVessel, const double *lvl, const VECTOR3 &ref, const VECTOR3 &dir)
{
	_TRACER;
	ExhaustStream *es = new ExhaustStream (this, hVessel, lvl, ref, dir, pss);
	scene->AddParticleStream (es);
	return es;
}

// ======================================================================

ParticleStream *D3D9Client::clbkCreateReentryStream (PARTICLESTREAMSPEC *pss,
	OBJHANDLE hVessel)
{
	_TRACER;
	ReentryStream *rs = new ReentryStream (this, hVessel, pss);
	scene->AddParticleStream (rs);
	return rs;
}

// ======================================================================

bool D3D9Client::clbkParticleStreamExists(const ParticleStream *ps)
{
	LogErr("UnImplemented Feature Used clbkParticleStreamExists");
	return false;
}

#pragma endregion

// ==============================================================

SURFHANDLE D3D9Client::clbkLoadTexture(const char *fname, DWORD flags)
{
	_TRACER;
	LPD3D9CLIENTSURFACE pTex = NULL;

	char cpath[256];

	SetItem(fname);

	if (TexturePath(fname, cpath)==false) {
		LogWrn("Texture %s not found.",fname);
		return NULL;
	}

	if (flags & 8) {
		if (texmgr->GetTexture(fname, &pTex, flags)==false) return NULL;
	}
	else {
		if (texmgr->LoadTexture(fname, &pTex, flags)!=S_OK) return NULL;
		//else LogBlu("Texture %s loaded. (%u x %u) flags=0x%X", fname, pTex->GetWidth(), pTex->GetHeight(), flags);
	}

	pTex->SetCreation(D3D9C_LOAD);
	return pTex;
}

// ==============================================================

void D3D9Client::clbkReleaseTexture(SURFHANDLE hTex)
{
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

ScreenAnnotation* D3D9Client::clbkCreateAnnotation()
{
	_TRACER;
	return GraphicsClient::clbkCreateAnnotation();
}

#pragma region Mesh functions

// ==============================================================

void D3D9Client::clbkStoreMeshPersistent(MESHHANDLE hMesh, const char *fname)
{
	_TRACER;
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

	LogMsg("Mesh Stored");
}

// ==============================================================

bool D3D9Client::clbkSetMeshTexture(DEVMESHHANDLE hMesh, DWORD texidx, SURFHANDLE surf)
{
	_TRACER;
	if (hMesh && surf) return ((D3D9Mesh*)hMesh)->SetTexture(texidx, SURFACE(surf));
	return false;
}

// ==============================================================

int D3D9Client::clbkSetMeshMaterial(DEVMESHHANDLE hMesh, DWORD matidx, const MATERIAL *mat)
{
	_TRACER;
	D3D9Mesh *mesh = (D3D9Mesh*)hMesh;
	DWORD nmat = mesh->MaterialCount();
	if (matidx >= nmat) return 4; // "index out of range"
	D3D9MatExt *meshmat = mesh->GetMaterial(matidx);
	if (meshmat) UpdateMatExt((const D3DMATERIAL9 *)mat, meshmat);
	return 0;
}

// ==============================================================

bool D3D9Client::clbkSetMeshProperty(DEVMESHHANDLE hMesh, DWORD prop, DWORD value)
{
	_TRACER;
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
	_TRACER;
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
	_TRACER;
	//LogWrn("Editing MeshGroup %u of 0x%X (flags=0x%X)",grpidx, hMesh, ges->flags);
	return ((D3D9Mesh*)hMesh)->EditGroup(grpidx, ges);
}

#pragma endregion

// ==============================================================

void D3D9Client::clbkNewVessel(OBJHANDLE hVessel)
{
	_TRACER;
	if (scene) scene->NewVessel(hVessel);
}

// ==============================================================

void D3D9Client::clbkDeleteVessel(OBJHANDLE hVessel)
{
	_TRACER;
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
	_TRACER;
	if (vtab) vtab->UpdateConfigData();
}

// ==============================================================

bool D3D9Client::clbkUseLaunchpadVideoTab() const
{
	_TRACER;
	return true;
}

// ==============================================================
// Fullscreen mode flag

bool D3D9Client::clbkFullscreenMode() const
{
	_TRACER;
	return bFullscreen;
}

// ==============================================================
// return the dimensions of the render viewport

void D3D9Client::clbkGetViewportSize(DWORD *width, DWORD *height) const
{
	_TRACER;
	LogMsg("Width=%u, Height=%u",viewW,viewH);
	*width = viewW, *height = viewH;
}

// ==============================================================
// Returns a specific render parameter

bool D3D9Client::clbkGetRenderParam(DWORD prm, DWORD *value) const
{
	_TRACER;
	switch (prm) {
		case RP_COLOURDEPTH:
			LogMsg("D3D9Client::clbkGetRenderParam(RP_COLOURDEPTH): %u",viewBPP);
			*value = viewBPP;
			return true;

		case RP_ZBUFFERDEPTH:
			LogMsg("D3D9Client::clbkGetRenderParam(RP_ZBUFFERDEPTH): %u",GetFramework()->GetZBufferBitDepth());
			*value = GetFramework()->GetZBufferBitDepth();
			return true;

		case RP_STENCILDEPTH:
			LogMsg("D3D9Client::clbkGetRenderParam(RP_STENCILDEPTH): %u",GetFramework()->GetStencilBitDepth());
			*value = GetFramework()->GetStencilBitDepth();
			return true;

		case RP_MAXLIGHTS:
			*value = max(0, min(12, Config->MaxLights));
			LogMsg("D3D9Client::clbkGetRenderParam(RP_MAXLIGHTS): %u",*value);
			return true;

#ifdef OAPI_BETA
		case RP_REQUIRETEXPOW2:
			*value = 0;
			return true;
#endif

	}
	return false;
}

// ==============================================================
// Responds to visual events

int D3D9Client::clbkVisEvent(OBJHANDLE hObj, VISHANDLE vis, DWORD msg, UINT context)
{
	_TRACER;
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


// ==============================================================
// Message handler for render window

LRESULT D3D9Client::RenderWndProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	_TRACER;

	static bool bTrackMouse = false;
	static short xpos=0, ypos=0;

	if (hRenderWnd!=hWnd) {
		LogWrn("Invalid Window in RenderWndProc() Window is already destroyed");
		return 0;
	}

	if (bRunning) GetScene()->UpdateCameraFromOrbiter();

	__TRY {

		switch (uMsg) {

			case WM_MOUSELEAVE:
			{
				if (bTrackMouse && bRunning) GraphicsClient::RenderWndProc (hWnd, WM_LBUTTONUP, 0, 0);
				return 0;
			}

			case WM_MBUTTONDOWN:
			{
				if (DebugControls::IsActive()) {
					DWORD flags = *(DWORD*)GetConfigParam(CFGPRM_GETDEBUGFLAGS);
					if (flags&DBG_FLAGS_PICK) {
						D3D9Pick pick = GetScene()->PickScene(GET_X_LPARAM(lParam), GET_Y_LPARAM(lParam));
						if (pick.pMesh) DebugControls::SelectGroup(pick.group);
					}
				}
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
						DebugControls::SetGroupHighlight(true);
						D3D9Pick pick = GetScene()->PickScene(xpos, ypos);
						if (pick.pMesh) {
							DebugControls::SelectMesh(pick.pMesh);
							DebugControls::SelectGroup(pick.group);
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
				bool bShift = (GetAsyncKeyState(VK_SHIFT) & 0x8000)!=0;
				bool bCtrl  = (GetAsyncKeyState(VK_CONTROL) & 0x8000)!=0;
				if (wParam=='C' && bShift && bCtrl) bControlPanel = !bControlPanel;
				if (bControlPanel && bShift && bCtrl) ControlPanelMsg(wParam);
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
					if (bTrackMouse) {
						double x = double(GET_X_LPARAM(lParam) - xpos);
						double y = double(GET_Y_LPARAM(lParam) - ypos);
						xpos = GET_X_LPARAM(lParam);
						ypos = GET_Y_LPARAM(lParam);
						double speed = *(double *)GetConfigParam(CFGPRM_GETCAMERASPEED);
						speed *= (DebugControls::GetVisualSize()/100.0);
						if (scene->CameraPan(_V(-x,y,0)*0.05, speed)) return 0;
					}
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
	_TRACER;
	if (vtab) return vtab->WndProc(hWnd, uMsg, wParam, lParam);
	else return false;
}

// =======================================================================

void D3D9Client::clbkRender2DPanel(SURFHANDLE *hSurf, MESHHANDLE hMesh, MATRIX3 *T, bool transparent)
{
	_TRACER;

	SURFHANDLE surf = 0;
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

		float alpha = 1.0f, scale = 1.0f;

		MESHGROUP *gr = oapiMeshGroup(hMesh, i);

		if (gr->UsrFlag & 2) continue; // skip this group

		DWORD TexIdx = gr->TexIdx;

		if (TexIdx >= TEXIDX_MFD0) {
			if (transparent) alpha = 0.0f;
			else             alpha = 1.0f;
			int mfdidx = TexIdx - TEXIDX_MFD0;
			SURFHANDLE newsurf = GetMFDSurface(mfdidx);
			if (!newsurf) continue;
			surf = newsurf;
		}
		else surf = hSurf[TexIdx];

		for (unsigned int k=0;k<gr->nVtx;k++) gr->Vtx[k].z = 0.0f;

		D3D9Effect::Render2DPanel(gr, SURFACE(surf), &ident, alpha, scale);
	}
}

// =======================================================================

DWORD D3D9Client::clbkGetDeviceColour (BYTE r, BYTE g, BYTE b)
{
	return ((DWORD)r << 16) + ((DWORD)g << 8) + (DWORD)b;
}



#pragma region Surface, Blitting and Filling Functions



// =======================================================================
// Surface functions
// =======================================================================

#ifdef OAPI_BETA

bool D3D9Client::clbkSaveSurfaceToImage(SURFHANDLE  surf,  const char *fname, ImageFileFormat  fmt, float quality)
{
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



SURFHANDLE D3D9Client::clbkCreateSurfaceEx(DWORD w, DWORD h, DWORD attrib)
{
	_TRACER;

#ifdef _SURFACE_EX		// Declare _SURFACE_EX if you want to compile with SurfaceEx support.

	if (uEnableLog>1) {
		_SETLOG(4);
		if (attrib&OAPISURFACE_TEXTURE)			LogMsg("clbkCreateSurfaceEx(OAPISURFACE_TEXTURE)");
		if (attrib&OAPISURFACE_RENDERTARGET)	LogMsg("clbkCreateSurfaceEx(OAPISURFACE_RENDERTARGET)");
		if (attrib&OAPISURFACE_GDI)				LogMsg("clbkCreateSurfaceEx(OAPISURFACE_GDI)");
		if (attrib&OAPISURFACE_SKETCHPAD)		LogMsg("clbkCreateSurfaceEx(OAPISURFACE_SKETCHPAD)");
		if (attrib&OAPISURFACE_MIPMAPS)			LogMsg("clbkCreateSurfaceEx(OAPISURFACE_MIPMAPS)");
		if (attrib&OAPISURFACE_NOMIPMAPS)		LogMsg("clbkCreateSurfaceEx(OAPISURFACE_NOMIPMAPS)");
		if (attrib&OAPISURFACE_ALPHA)			LogMsg("clbkCreateSurfaceEx(OAPISURFACE_ALPHA)");
		if (attrib&OAPISURFACE_NOALPHA)			LogMsg("clbkCreateSurfaceEx(OAPISURFACE_NOALPHA)");
		if (attrib&OAPISURFACE_UNCOMPRESS)		LogMsg("clbkCreateSurfaceEx(OAPISURFACE_UNCOMPRESS)");
		if (attrib&OAPISURFACE_SYSMEM)			LogMsg("clbkCreateSurfaceEx(OAPISURFACE_SYSMEM)");
		_POPLOG;
	}


	D3D9ClientSurface *surf = new D3D9ClientSurface(pd3dDevice, "clbkCreateSurfaceEx");
	surf->SetCreation(D3D9C_SURFACE);


	D3DFORMAT fmt = D3DFMT_X8R8G8B8;
	if (attrib&OAPISURFACE_ALPHA) fmt = D3DFMT_A8R8G8B8;

	D3DPOOL pool = D3DPOOL_DEFAULT;
	if (attrib&OAPISURFACE_SYSMEM) pool = D3DPOOL_SYSTEMMEM;

	bool bTexture = false;
	if (attrib&OAPISURFACE_TEXTURE) bTexture = true;

	DWORD usage = 0;
	bool bLock = false;
	if (attrib&OAPISURFACE_GDI) usage = D3DUSAGE_DYNAMIC, bLock=true;

	// -------------------------------------------------------------------

	if (attrib&OAPISURFACE_RENDERTARGET) {
		surf->MakeRenderTargetEx(w, h, bTexture, bLock, fmt);
		return surf;
	}

	if (attrib&OAPISURFACE_SKETCHPAD) {
		if (bTexture) surf->MakeTextureEx(w,h, D3DUSAGE_DYNAMIC, fmt, pool);
		else		  surf->MakeSurfaceEx(w, h, fmt, pool);
		return surf;
	}

	if (attrib&OAPISURFACE_TEXTURE) {
		surf->MakeTextureEx(w, h, usage, fmt, pool);
		return surf;
	}

	surf->MakeSurfaceEx(w, h, fmt, pool);
	return surf;
#else
	if (attrib&OAPISURFACE_TEXTURE) return clbkCreateTexture(w,h);
	else							return clbkCreateSurface(w,h);
#endif
}

#endif

// =======================================================================

SURFHANDLE D3D9Client::clbkCreateSurface(DWORD w, DWORD h, SURFHANDLE hTemplate)
{
	_TRACER;
	D3D9ClientSurface *surf = new D3D9ClientSurface(pd3dDevice, "clbkCreateSurface");

	if (Config->MemoryLogic==0) surf->MakePlainSurface(w,h);
	else						surf->MakeRenderingTexture(w,h);

	surf->SetCreation(D3D9C_SURFACE);

	return surf;
}

// =======================================================================

SURFHANDLE D3D9Client::clbkCreateSurface(HBITMAP hBmp)
{
	_TRACER;
	SURFHANDLE hSurf = GraphicsClient::clbkCreateSurface(hBmp);
	return hSurf;
}

// =======================================================================

SURFHANDLE D3D9Client::clbkCreateTexture(DWORD w, DWORD h)
{
	_TRACER;
	D3D9ClientSurface *pSurf = new D3D9ClientSurface(pd3dDevice, "clbkCreateTexture");
	// DO NOT USE ALPHA
	pSurf->MakeTexture(w, h, D3DFMT_X8R8G8B8);
	pSurf->SetCreation(D3D9C_TEXTURE);
	return (SURFHANDLE)pSurf;
}

// =======================================================================

void D3D9Client::clbkIncrSurfaceRef(SURFHANDLE surf)
{
	if (surf==NULL) { LogErr("D3D9Client::clbkIncrSurfaceRef() Input Surface is NULL");	return; }
	SURFACE(surf)->IncRef();
}

// =======================================================================

bool D3D9Client::clbkReleaseSurface(SURFHANDLE surf)
{
	_TRACER;

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
	_TRACER;
	if (surf==NULL) surf = pFramework->GetBackBufferHandle();
	*w = SURFACE(surf)->GetWidth();
	*h = SURFACE(surf)->GetHeight();
	return true;
}

// =======================================================================

bool D3D9Client::clbkSetSurfaceColourKey(SURFHANDLE surf, DWORD ckey)
{
	_TRACER;
	if (surf==NULL) { LogErr("Surface is NULL"); return false; }
	SURFACE(surf)->SetColorKey(ckey);
	return true;
}

// =======================================================================
// Blitting functions
// =======================================================================

#ifdef OAPI_BETA

int D3D9Client::clbkBeginBltGroup(SURFHANDLE tgt)
{
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
	if (pBltGrpTgt==NULL) return -2;
	SURFACE(pBltGrpTgt)->EndBlitGroup(); // Flush queue and release GPU
	pBltGrpTgt = NULL;
	return 0;
}


bool D3D9Client::CheckBltGroup(SURFHANDLE src, SURFHANDLE tgt) const
{

	// return false;  // Disable blt groups

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

#endif


bool D3D9Client::clbkBlt(SURFHANDLE tgt, DWORD tgtx, DWORD tgty, SURFHANDLE src, DWORD flag) const
{
	_TRACER;

	__TRY {

		if (src==NULL) { LogErr("D3D9Client::clbkBlt() Source surface is NULL"); return false; }
		if (tgt==NULL) tgt = pFramework->GetBackBufferHandle();

		int w = SURFACE(src)->GetWidth();
		int h = SURFACE(src)->GetHeight();

		RECT rs = { 0, 0, w, h };
		RECT rt = { tgtx, tgty, tgtx+w, tgty+h };

#ifdef OAPI_BETA
		if (pBltGrpTgt) {
			if (CheckBltGroup(src,tgt)) SURFACE(pBltGrpTgt)->AddQueue(SURFACE(src), &rs, &rt);
			else 						SURFACE(tgt)->CopyRect(SURFACE(src), &rs, &rt, flag);
		}
		else SURFACE(tgt)->CopyRect(SURFACE(src), &rs, &rt, flag);
#else
		SURFACE(tgt)->CopyRect(SURFACE(src), &rs, &rt, flag);
#endif

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
	_TRACER;

	__TRY {
		if (src==NULL) { LogErr("D3D9Client::clbkBlt() Source surface is NULL"); return false; }
		if (tgt==NULL) tgt = pFramework->GetBackBufferHandle();

		RECT rs = { srcx, srcy, srcx+w, srcy+h };
		RECT rt = { tgtx, tgty, tgtx+w, tgty+h };

#ifdef OAPI_BETA
		if (pBltGrpTgt) {
			if (CheckBltGroup(src,tgt)) SURFACE(pBltGrpTgt)->AddQueue(SURFACE(src), &rs, &rt);
			else 						SURFACE(tgt)->CopyRect(SURFACE(src), &rs, &rt, flag);
		}
		else SURFACE(tgt)->CopyRect(SURFACE(src), &rs, &rt, flag);
#else
		SURFACE(tgt)->CopyRect(SURFACE(src), &rs, &rt, flag);
#endif

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
	_TRACER;

	__TRY {
		if (src==NULL) { LogErr("D3D9Client::clbkScaleBlt() Source surface is NULL"); return false; }
		if (tgt==NULL) tgt = pFramework->GetBackBufferHandle();

		RECT rs = { srcx, srcy, srcx+srcw, srcy+srch };
		RECT rt = { tgtx, tgty, tgtx+tgtw, tgty+tgth };

#ifdef OAPI_BETA
		if (pBltGrpTgt) {
			if (CheckBltGroup(src,tgt)) SURFACE(pBltGrpTgt)->AddQueue(SURFACE(src), &rs, &rt);
			else 						SURFACE(tgt)->CopyRect(SURFACE(src), &rs, &rt, flag);
		}
		else SURFACE(tgt)->CopyRect(SURFACE(src), &rs, &rt, flag);
#else
		SURFACE(tgt)->CopyRect(SURFACE(src), &rs, &rt, flag);
#endif

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
	_TRACER;
	bool bX  = SURFACE(pdds)->bNoGDI;
	SURFACE(pdds)->bNoGDI = true;
	bool ret = GraphicsClient::clbkCopyBitmap(pdds, hbm, x,y,dx,dy);
	SURFACE(pdds)->bNoGDI = bX;
	return ret;
}

// =======================================================================

bool D3D9Client::clbkFillSurface(SURFHANDLE tgt, DWORD col) const
{
	_TRACER;
	if (tgt==NULL) tgt = pFramework->GetBackBufferHandle();
	bool ret = SURFACE(tgt)->Clear(col);
	return ret;
}

// =======================================================================

bool D3D9Client::clbkFillSurface(SURFHANDLE tgt, DWORD tgtx, DWORD tgty, DWORD w, DWORD h, DWORD col) const
{
	_TRACER;
	if (tgt==NULL) tgt = pFramework->GetBackBufferHandle();
	RECT r = {tgtx, tgty, tgtx+w, tgty+h};
	bool ret = SURFACE(tgt)->Fill(&r, col);
	return ret;
}

#pragma endregion

// =======================================================================
// GDI functions
// =======================================================================

HDC D3D9Client::clbkGetSurfaceDC(SURFHANDLE surf)
{
	_TRACER;
	if (surf==NULL) surf = pFramework->GetBackBufferHandle();
	HDC hDC = SURFACE(surf)->GetDC();
	return hDC;
}

// =======================================================================

void D3D9Client::clbkReleaseSurfaceDC(SURFHANDLE surf, HDC hDC)
{
	if (hDC==NULL) { LogErr("D3D9Client::clbkReleaseSurfaceDC() Input hDC is NULL"); return; }
	if (surf==NULL) surf = pFramework->GetBackBufferHandle();
	SURFACE(surf)->ReleaseDC(hDC);
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
	_TRACER;
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

void D3D9Client::WriteLog(const char *msg) const
{
	_TRACER;
	char cbuf[256] = "D3D9Client: ";
	strcpy_s(cbuf+12, 240, msg);
	oapiWriteLog(cbuf);
}

// =======================================================================

void D3D9Client::SetLabel(const char *txt)
{
	if (bRunning) return;
	strcpy_s(pLoadLabel, 127, txt);
}

// =======================================================================

void D3D9Client::SetItem(const char *txt)
{
	if (bRunning) return;

	strcpy_s(pLoadItem, 127, txt);

	if (bSkepchpadOpen==false && pTextScreen) {

		if (pd3dDevice->TestCooperativeLevel()!=S_OK) {
			LogErr("TestCooperativeLevel() Failed");
			return;
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

		HR(pd3dDevice->Clear(0, NULL, D3DCLEAR_TARGET|D3DCLEAR_ZBUFFER|D3DCLEAR_STENCIL, 0x0, 1.0f, 0L));
		HR(pd3dDevice->StretchRect(pSplashScreen, NULL, pBackBuffer, NULL, D3DTEXF_POINT));
		HR(pd3dDevice->StretchRect(pTextScreen, NULL, pBackBuffer, &txt, D3DTEXF_POINT));
		HR(pd3dDevice->Present(0, 0, 0, 0));

		MSG msg;
		while (PeekMessage( &msg, NULL, 0, 0, PM_REMOVE)) DispatchMessage(&msg);
	}
}

// =======================================================================

void D3D9Client::SplashScreen()
{

	DWORD Width = pFramework->GetRenderWidth();
	DWORD Height = pFramework->GetRenderHeight();
	HINSTANCE hInst = oapiGetOrbiterInstance();

	loadd_x = 279*viewW/1280;
	loadd_y = 545*viewH/800;
	loadd_w = viewW/3;
	loadd_h = 80;

	HR(pd3dDevice->TestCooperativeLevel());
	HR(pd3dDevice->Clear(0, NULL, D3DCLEAR_TARGET|D3DCLEAR_ZBUFFER|D3DCLEAR_STENCIL, 0x0, 1.0f, 0L));
	HR(pd3dDevice->CreateOffscreenPlainSurface(loadd_w, loadd_h, D3DFMT_X8R8G8B8, D3DPOOL_DEFAULT, &pTextScreen, NULL));
	HR(pd3dDevice->CreateOffscreenPlainSurface(Width, Height, D3DFMT_X8R8G8B8, D3DPOOL_DEFAULT, &pSplashScreen, NULL));

	HBITMAP hBM = LoadBitmapA(hInst, MAKEINTRESOURCE(273));
	BITMAP bm;

	GetObject(hBM, sizeof(BITMAP), &bm);

	HDC hDC;
	HR(pSplashScreen->GetDC(&hDC));

	HDC hSrc = CreateCompatibleDC(hDC);

	HGDIOBJ hOld = SelectObject(hSrc, hBM);

	SetStretchBltMode(hDC, HALFTONE);
	StretchBlt(hDC, 0, 0, Width, Height, hSrc, 0, 0, bm.bmWidth, bm.bmHeight, SRCCOPY);

	SelectObject(hSrc, hOld);
	DeleteDC(hSrc);
	DeleteObject(hBM);

	LOGFONTA fnt; memset2((void *)&fnt, 0, sizeof(LOGFONT));

	fnt.lfHeight		 = 18;
	fnt.lfWeight		 = 700;
	fnt.lfCharSet		 = ANSI_CHARSET;
	fnt.lfOutPrecision	 = OUT_DEFAULT_PRECIS;
	fnt.lfClipPrecision	 = CLIP_DEFAULT_PRECIS;
	fnt.lfQuality		 = ANTIALIASED_QUALITY;
	fnt.lfPitchAndFamily = DEFAULT_PITCH;
	strcpy(fnt.lfFaceName, "Courier New");

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
	char dataA[]={"D3D9Client R13 Debug Build [" __DATE__ "]"};
#else
	char dataA[]={"D3D9Client R13 Build [" __DATE__ "]"};
#endif

	char dataB[128]; sprintf_s(dataB,128,"Build %s %u 20%u [%u]", months[m], d, y, oapiGetOrbiterVersion());
	char dataC[]={"Warning: Running in GDI compatibility mode. Expect a low framerate."};
	char dataD[]={"Warning: Config folder not present in /Modules/Server/. Please create symbolic link."};

	int xc = Width*814/bm.bmWidth;
	int yc = Height*544/bm.bmHeight;

	TextOut(hDC, xc, yc + 0*20, "ORBITER Space Flight Simulator",30);
	TextOut(hDC, xc, yc + 1*20, dataB, strlen(dataB));
	TextOut(hDC, xc, yc + 2*20, dataA, strlen(dataA));

	if (bGDIBB) {
		SetTextAlign(hDC, TA_CENTER);
		TextOut(hDC, Width/2, Height-30, dataC, strlen(dataC));
	}

	DWORD cattrib = GetFileAttributes("Modules/Server/Config");

	if ((cattrib&0x10)==0 || cattrib==INVALID_FILE_ATTRIBUTES) {
		SetTextAlign(hDC, TA_CENTER);
		TextOut(hDC, Width/2, Height-50, dataD, strlen(dataD));
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



// ==============================================================
// 2D Drawing Interface

oapi::Sketchpad *D3D9Client::clbkGetSketchpad(SURFHANDLE surf)
{
	_TRACER;

	if (bSkepchpadOpen) LogErr("an other Sketchpad is already open");

	if (surf==NULL) surf = GetBackBufferHandle();

	if (SURFACE(surf)->IsBackBuffer()) {
		if (bScene==true || bGDIBB==false) {
			bSkpGDI=false;
			if (bScene==false) pd3dDevice->BeginScene(); // bScene is true between BeginScene() and EndScene() in the main rendering routine
			bSkepchpadOpen = true;
			return new D3D9Pad(surf);
		}
	}
	else {
		if (Config->SketchpadMode==0) {
			if (SURFACE(surf)->IsRenderTarget()) {
				//LogErr("[INFO] Sketchpad is used for RenderTargetSurface 0x%X Size=(%ux%u)", surf, SURFACE(surf)->GetWidth(), SURFACE(surf)->GetHeight());
				bSkpGDI=false;
				bSkepchpadOpen = true;
				return new D3D9Pad(surf);
			}
		}
	}

	// Fall back into a GDI rendering -----------------------------------------------
	//
	bSkpGDI = true;
	HDC hDC = clbkGetSurfaceDC (surf);
	if (hDC) {
		bSkepchpadOpen = true;
		return new GDIPad(surf, hDC);
	}
	return NULL;
}


void D3D9Client::clbkReleaseSketchpad(oapi::Sketchpad *sp)
{
	bSkepchpadOpen = false;

	SURFHANDLE surf = sp->GetSurface();

	if (surf) {
		if (bSkpGDI) {
			GDIPad *gdip = (GDIPad*)sp;
			clbkReleaseSurfaceDC(gdip->GetSurface(), gdip->GetDC());
			delete gdip;
		}
		else {
			if (SURFACE(surf)->IsBackBuffer()) {
				if (bScene==false) pd3dDevice->EndScene();
			}
			D3D9Pad *gdip = (D3D9Pad*)sp;
			delete gdip;
		}
	}
}

Font *D3D9Client::clbkCreateFont(int height, bool prop, const char *face, Font::Style style, int orientation) const
{
	return new D3D9PadFont(height, prop, face, style, orientation);
}

void D3D9Client::clbkReleaseFont(Font *font) const
{
	delete ((D3D9PadFont*)font);
}

Pen *D3D9Client::clbkCreatePen(int style, int width, DWORD col) const
{
	return new D3D9PadPen(style, width, col);
}

void D3D9Client::clbkReleasePen(Pen *pen) const
{
	delete ((D3D9PadPen*)pen);
}

Brush *D3D9Client::clbkCreateBrush(DWORD col) const
{
	return new D3D9PadBrush(col);
}

void D3D9Client::clbkReleaseBrush(Brush *brush) const
{
	delete ((D3D9PadBrush*)brush);
}

#pragma endregion


// ======================================================================
// class VisObject

VisObject::VisObject(OBJHANDLE hObj)
{
	_TRACER;
	hObject = hObj;
}

VisObject::~VisObject ()
{
}
