// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#define STRICT 1
#define OAPI_IMPLEMENTATION

// Enable visual styles. Source: https://msdn.microsoft.com/en-us/library/windows/desktop/bb773175(v=vs.85).aspx
#pragma comment(linker,"\"/manifestdependency:type='win32' name='Microsoft.Windows.Common-Controls' version='6.0.0.0' processorArchitecture='*' publicKeyToken='6595b64144ccf1df' language='*'\"")

#include <windows.h>
#include <zmouse.h>
#include <commctrl.h>
#include <mmsystem.h>
#include <process.h>
#include <direct.h>
#include <stdio.h>
#include <time.h>
#include <fstream>
#include <strstream>
#include <iomanip>
#include <io.h>
#include "cmdline.h"
#include "D3d7util.h"
#include "D3dmath.h"
#include "Log.h"
#include "console_ng.h"
#include "State.h"
#include "Astro.h"
#include "Camera.h"
#include "Pane.h"
#include "Select.h"
#include "DlgMgr.h"
#include "Psys.h"
#include "Base.h"
#include "Vessel.h"
#include "resource.h"
#include "Orbiter.h"
#include "Launchpad.h"
#include "MenuInfoBar.h"
#include "Dialogs.h"
#include "DialogWin.h"
#include "Script.h"
#include "ddeserver.h"
#include "Memstat.h"
#include "CustomControls.h"
#include "Help.h"
#include "DlgHelp.h" // temporary
#include "htmlctrl.h"
#include "DlgCtrl.h"
#include "GraphicsAPI.h"

#ifdef INLINEGRAPHICS
#include "OGraphics.h"
#include "Texture.h"
#include "Scene.h"
#include "TileMgr.h"
#include "tilemgr2.h"
#include "CSphereMgr.h"
#include "VVessel.h"
#include "ScreenNote.h"
TextureManager* g_texmanager = 0;
#endif // INLINEGRAPHICS

using namespace std;
using namespace oapi;

#define OUTPUT_DBG
#define LOADSTATUSCOL 0xC08080 //0xFFD0D0

//#define OUTPUT_TEXTURE_INFO

#define KEYDOWN(name,key) (name[key] & 0x80) 

const int MAX_TEXTURE_BUFSIZE = 8000000;
// Texture manager buffer size. Should be determined from
// memory size (System or Video?)

TCHAR* g_strAppTitle = "OpenOrbiter";

#ifdef INLINEGRAPHICS
TCHAR* MasterConfigFile = "Orbiter.cfg";
#else
TCHAR* MasterConfigFile = "Orbiter_NG.cfg";
#endif // INLINEGRAPHICS

TCHAR* CurrentScenario = "(Current state)";
char ScenarioName[256] = "\0";
// some global string resources

char cwd[512];

// =======================================================================
// Global variables

Orbiter*        g_pOrbiter       = NULL;  // application
BOOL            g_bFrameMoving   = TRUE;
extern BOOL     g_bAppUseZBuffer;
extern BOOL     g_bAppUseBackBuffer;
extern TCHAR*   g_strAppTitle;
double          g_nearplane      = 5.0;
double          g_farplane       = 5e6;
const double    MinWarpLimit     = 0.1;  // make variable
const double    MaxWarpLimit     = 1e5;  // make variable
DWORD           g_qsaveid        = 0;
DWORD           g_customcmdid    = 0;

// 2D info output flags
BOOL		    g_bOutputTime    = TRUE;
BOOL		    g_bOutputFPS     = TRUE;
BOOL            g_bOutputDim     = TRUE;
bool		    g_bForceUpdate   = true;
bool            g_bShowGrapple   = false;
bool            g_bStateUpdate   = false;

// Timing parameters
DWORD  launch_tick;      // counts the first 3 frames
DWORD  g_vtxcount = 0;   // vertices/frame rendered (for diagnosis)
DWORD  g_tilecount = 0;  // surface tiles/frame rendered (for diagnosis)
BOOL   use_fine_counter;         // high-precision timer available?
double fine_counter_step;        // step interval of high-precision counter (or 0 if not available)
LARGE_INTEGER fine_counter_freq; // high-precision tick frequency
LARGE_INTEGER fine_counter;      // current high-precision time value
TimeData td;             // timing information

// Configuration parameters set from Driver.cfg
DWORD requestDriver     = 0;
DWORD requestFullscreen = 0;
DWORD requestSoftware   = 0;
DWORD requestScreenW    = 640;
DWORD requestScreenH    = 480;
DWORD requestWindowW    = 400;
DWORD requestWindowH    = 300;
DWORD requestZDepth     = 16;

// Logical objects
Camera          *g_camera = 0;         // observer camera
Pane            *g_pane = 0;           // 2D output surface
Select          *g_select = 0;         // global menu resource
InputBox        *g_input = 0;          // global input box resource
PlanetarySystem *g_psys = 0;
Vessel          *g_focusobj = 0;       // current vessel with input focus
Vessel          *g_pfocusobj = 0;      // previous vessel with input focus

// This GUID allows DirectPlay to find other instances of the same game on
// the network.  So it must be unique for every game, and the same for 
// every instance of that game.  // {C6334FC0-3B80-4fed-89F1-A4DEFEB6DB20}
GUID Orbiter::AppGUID = { 0xc6334fc0, 0x3b80, 0x4fed, { 0x89, 0xf1, 0xa4, 0xde, 0xfe, 0xb6, 0xdb, 0x20 } };

char DBG_MSG[256] = "";

enum APPMSGTYPE { MSG_NONE, MSGERR_APPMUSTEXIT, MSGWARN_SWITCHTOSOFTWARE };

// Default help context (for main help system)
HELPCONTEXT DefHelpContext = {
	"html/orbiter.chm",
	0,
	"html/orbiter.chm::/orbiter.hhc",
	"html/orbiter.chm::/orbiter.hhk"
};

// =======================================================================
// Function prototypes

HRESULT ConfirmDevice (DDCAPS*, D3DDEVICEDESC7*);

//LRESULT CALLBACK WndProc3D (HWND, UINT, WPARAM, LPARAM);
INT_PTR CALLBACK BkMsgProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam);
INT_PTR CALLBACK CloseMsgProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam);

VOID    DestroyWorld ();
bool    Select_Main (Select &sel);
void    SetEnvironmentVars ();
HANDLE hMutex = 0;
HANDLE hConsoleMutex = 0;

// =======================================================================
// _matherr()
// trap global math exceptions

int _matherr(struct _exception *except )
{
	if (!strcmp (except->name, "acos")) {
		except->retval = (except->arg1 < 0.0 ? Pi : 0.0);
		return 1;
	}
	return 0;
}


// =======================================================================
// WinMain()
// Application entry containing message loop


INT WINAPI WinMain (HINSTANCE hInstance, HINSTANCE, PSTR strCmdLine, INT nCmdShow)
{
#ifdef INLINEGRAPHICS
	// determine whether another instance already exists
	hMutex = CreateMutex (NULL, TRUE, "Test");
	if (GetLastError() == ERROR_ALREADY_EXISTS) {
		MessageBox (NULL, "Another ORBITER application is already running.",
			"ORBITER Error", MB_OK | MB_ICONEXCLAMATION);
		return 0;
	}
#endif

	SetEnvironmentVars();
	g_pOrbiter = new Orbiter; // application instance

	// Parse command line
	orbiter::CommandLine::Parse(g_pOrbiter, strCmdLine);

	// Initialise the log
	INITLOG("Orbiter.log", g_pOrbiter->Cfg()->CfgCmdlinePrm.bAppendLog); // init log file
#ifdef ISBETA
	LOGOUT("Build %s BETA [v.%06d]", __DATE__, GetVersion());
#else
	LOGOUT("Build %s [v.%06d]", __DATE__, GetVersion());
#endif

	// Initialise random number generator
	//srand ((unsigned)time (NULL));
	srand(12345);
	LOGOUT("Timer precision: %g sec", fine_counter_step);

	HRESULT hr;
	// Create application
	if (FAILED (hr = g_pOrbiter->Create (hInstance))) {
		LOGOUT("Application creation failed");
		MessageBox (NULL, "Application creation failed!\nTerminating.",
			"Orbiter Error", MB_OK | MB_ICONERROR);
		return 1;
	}

	oapiRegisterCustomControls (hInstance);
	setlocale (LC_CTYPE, "");

	int retcode = g_pOrbiter->Run();
	delete g_pOrbiter;
	return retcode;
}

void SetEnvironmentVars ()
{
	// Set search path to "Modules" subdirectory so that DLLs are found
	char *ppath = getenv ("PATH");
	if (ppath) {
		char *cbuf = new char[strlen(ppath)+15]; TRACENEW
		sprintf (cbuf, "PATH=%s;Modules", ppath);
		_putenv (cbuf);
		delete []cbuf;
	} else {
		_putenv ("PATH=Modules");
	}
	_getcwd (cwd, 512);
}

// =======================================================================
// InitializeWorld()
// Create logical objects

bool Orbiter::InitializeWorld (char *name)
{
	if (hRenderWnd)
		g_pane = new Pane (gclient, hRenderWnd, viewW, viewH, viewBPP); TRACENEW
	if (g_camera) delete g_camera;
	g_camera = new Camera (g_nearplane, g_farplane); TRACENEW
	g_camera->ResizeViewport (viewW, viewH);
	if (g_psys) delete g_psys;
	g_psys = new PlanetarySystem (name); TRACENEW
	if (!g_psys->nObj()) {  // sanity check
		DestroyWorld();
		return false;
	}
	return true;
}

// =======================================================================
// DestroyWorld()
// Destroy logical objects

VOID DestroyWorld ()
{
	if (g_camera) { delete g_camera; g_camera = 0; }
	if (g_psys)   { delete g_psys;   g_psys = 0; }
}

//=============================================================================
// Name: class Orbiter
// Desc: Main application class
//=============================================================================

//-----------------------------------------------------------------------------
// Name: Orbiter()
// Desc: Application constructor. Sets attributes for the app.
//-----------------------------------------------------------------------------
Orbiter::Orbiter ()
{
	// override base class defaults
    //m_bAppUseZBuffer  = TRUE;
    //m_fnConfirmDevice = ConfirmDevice;

	// Initialise timer
	timeBeginPeriod(1);
	if (use_fine_counter = QueryPerformanceFrequency(&fine_counter_freq)) {
		double freq = fine_counter_freq.LowPart;
		if (fine_counter_freq.HighPart) freq += fine_counter_freq.HighPart * 4294967296.0;
		fine_counter_step = 1.0 / freq;
	}

	nmodule         = 0;
	pDI             = new DInput(this); TRACENEW
	pConfig         = new Config; TRACENEW
	pState          = NULL;
	pMainDlg        = NULL;
	pDlgMgr         = NULL;
	m_pConsole      = NULL;
	ddeserver       = NULL;
	bFullscreen     = false;
	viewW = viewH = viewBPP = 0;
#ifdef INLINEGRAPHICS
	oclient         = NULL;
#endif
	gclient         = NULL;
#ifdef NETCONNECT
	NetConn         = NULL;
	NetServer       = NULL;
	NetClient       = NULL;
#endif // NETCONNECT
	hRenderWnd      = NULL;
	hBk             = NULL;
	hScnInterp      = NULL;
	snote_playback  = NULL;
	nsnote          = 0;
	bVisible        = false;
	bAllowInput     = false;
	bRunning        = false;
	bRequestRunning = false;
	bSession        = false;
	bEnableLighting = TRUE;
	bUseStencil     = false;
	bKeepFocus      = false;
	bRealtime       = TRUE;
	bEnableAtt      = TRUE;
	bRecord         = false;
	bPlayback       = false;
	bCapture        = false;
	bFastExit       = false;
	bRoughType      = false;
	bStartVideoTab  = false;
	//lstatus.bkgDC   = 0;
	cfglen          = 0;
	ncustomcmd      = 0;
	D3DMathSetup();
	script          = NULL;

	simheapsize     = 0;

	for (int i = 0; i < 15; i++)
		ctrlKeyboard[i] = ctrlJoystick[i] = ctrlTotal[i] = 0; // reset keyboard and joystick attitude requests

	memset (simkstate, 0, 256);

#ifdef NETCONNECT
	OrbiterConnect::Startup();
#endif // NETCONNECT

}

//-----------------------------------------------------------------------------
// Name: ~Orbiter()
// Desc: Application destructor.
//-----------------------------------------------------------------------------
Orbiter::~Orbiter ()
{
	CloseApp ();
}

//-----------------------------------------------------------------------------
// Name: Create()
// Desc: This method selects a D3D device
//-----------------------------------------------------------------------------
HRESULT Orbiter::Create (HINSTANCE hInstance)
{
	if (pMainDlg) return S_OK; // already created

	HRESULT hr;
	WNDCLASS wndClass;

	// Enable tab controls
	InitCommonControls();
	LoadLibrary ("riched20.dll");

	// parameter manager - parses from master config file
	hInst = hInstance;
	pConfig->Load(MasterConfigFile);
	strcpy (cfgpath, pConfig->CfgDirPrm.ConfigDir);   cfglen = strlen (cfgpath);

	if (FAILED (hr = pDI->Create (hInstance))) return hr;

	// validate configuration
	if (pConfig->CfgJoystickPrm.Joy_idx > GetDInput()->NumJoysticks()) pConfig->CfgJoystickPrm.Joy_idx = 0;

	// Read key mapping from file (or write default keymap)
	if (!keymap.Read ("keymap.cfg")) keymap.Write ("keymap.cfg");

    pState = new State(); TRACENEW

	// Register main dialog window class
	GetClassInfo (hInstance, "#32770", &wndClass); // override default dialog class
	wndClass.hIcon = LoadIcon (hInstance, MAKEINTRESOURCE (IDI_MAIN_ICON));
	RegisterClass (&wndClass);

	// Find out if we are running under Linux/WINE
	HKEY key;
	long ret = RegOpenKeyEx (HKEY_CURRENT_USER, TEXT("Software\\Wine"), 0, KEY_QUERY_VALUE, &key);
	RegCloseKey (key);
	bWINEenv = (ret == ERROR_SUCCESS);

	// Register HTML viewer class
	RegisterHtmlCtrl (hInstance, UseHtmlInline());
	SplitterCtrl::RegisterClass (hInstance);

	if (pConfig->CfgCmdlinePrm.bFastExit)
		SetFastExit(true);
	if (pConfig->CfgCmdlinePrm.bOpenVideoTab)
		OpenVideoTab();

	if (pConfig->CfgDemoPrm.bBkImage) {
		hBk = CreateDialog (hInstance, MAKEINTRESOURCE(IDD_DEMOBK), NULL, BkMsgProc);
		ShowWindow (hBk, SW_MAXIMIZE);
	}
	
	// Create the "launchpad" main dialog window
	pMainDlg = new MainDialog (this); TRACENEW
	hDlg     = pMainDlg->Create (bStartVideoTab);

#ifdef INLINEGRAPHICS
	oclient = new OrbiterGraphics (this); TRACENEW
	gclient = oclient;
	gclient->clbkInitialise();
	pMainDlg->UnhidePage (4, "Video");
#else
	SetWindowText (hDlg, "OpenOrbiter Server Launchpad");
#endif // INLINEGRAPHICS

	Instrument::RegisterBuiltinModes();

	script = new ScriptInterface (this); TRACENEW

	{
		BOOL cleartype, ok;
		ok = SystemParametersInfo (SPI_GETFONTSMOOTHING, 0, &cleartype, 0);
		bSysClearType = (ok && cleartype);
		//if (pConfig->CfgDebugPrm.bForceReenableSmoothFont) bSysClearType = true;
	}
	if (pConfig->CfgDebugPrm.bDisableSmoothFont)
		ActivateRoughType();

	// preload fixed plugin modules
	LoadFixedModules ();
	memstat = new MemStat;

	// preload modules from command line requests
	for (auto it = pConfig->CfgCmdlinePrm.LoadPlugins.begin(); it != pConfig->CfgCmdlinePrm.LoadPlugins.end(); it++)
		LoadModule("Modules\\Plugin", it->c_str());

	// preload active plugin modules
	for (int i = 0; i < pConfig->nactmod; i++)
		LoadModule ("Modules\\Plugin", pConfig->actmod[i]);

    return S_OK;
}

//-----------------------------------------------------------------------------
// Name: SaveConfig()
// Desc: Save configuration files (before closedown)
//-----------------------------------------------------------------------------
void Orbiter::SaveConfig ()
{
	pConfig->Write (); // save current settings
	pMainDlg->WriteExtraParams ();
}

//-----------------------------------------------------------------------------
// Name: CloseApp()
// Desc: Cleanup for program end
//-----------------------------------------------------------------------------
VOID Orbiter::CloseApp (bool fast_shutdown)
{
	SaveConfig();
	while (nmodule) UnloadModule (module[0].name);

	if (bRoughType)
		DeactivateRoughType();

	if (!fast_shutdown) {
#ifdef INLINEGRAPHICS
		if (oclient) {
			delete oclient;
			oclient = 0;
			gclient = 0;
		}
#endif
		delete pDI;
		if (memstat) delete memstat;
		if (pConfig)  delete pConfig;
		if (pMainDlg) delete pMainDlg;
		if (hBk) DestroyWindow (hBk);
		if (pState)   delete pState;
		if (ddeserver) delete ddeserver;
		if (script) delete script;
		if (ncustomcmd) {
			for (DWORD i = 0; i < ncustomcmd; i++) delete []customcmd[i].label;
			delete []customcmd;
		}
		oapiUnregisterCustomControls (hInst);
	}
	timeEndPeriod (1);
}

//-----------------------------------------------------------------------------
// Name: GetVersion()
// Desc: Returns orbiter build version as integer in YYMMDD format
//-----------------------------------------------------------------------------
int Orbiter::GetVersion () const
{
	static int v = 0;
	if (!v) {
		static char *mstr[12] = {"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"};
		char ms[32];
		int day, month, year;
		sscanf (__DATE__, "%s%d%d", ms, &day, &year);
		for (month = 0; month < 12; month++)
			if (!_strnicmp (ms, mstr[month], 3)) break;
		v = (year%100)*10000 + (month+1)*100 + day;
	}
	return v;
}

//-----------------------------------------------------------------------------
// Name: LoadFixedModules()
// Desc: Load all plugin modules from the "startup" directory
//-----------------------------------------------------------------------------
void Orbiter::LoadFixedModules ()
{
	char cbuf[256];
	char *path = "Modules\\Startup";
	struct _finddata_t fdata;
	intptr_t fh = _findfirst ("Modules\\Startup\\*.dll", &fdata);
	if (fh == -1) return; // no files found
	do {
		strcpy (cbuf, fdata.name);
		cbuf[(strlen(cbuf)-4)] = '\0'; // cut off extension
		LoadModule (path, cbuf);
	} while (!_findnext (fh, &fdata));
	_findclose (fh);
}

//-----------------------------------------------------------------------------
// Name: LoadModule()
// Desc: Load a named plugin DLL
//-----------------------------------------------------------------------------
HINSTANCE Orbiter::LoadModule (const char *path, const char *name)
{
	char cbuf[256];
	sprintf (cbuf, "%s\\%s.dll", path, name);

	register_module = NULL; // clear the module
	HINSTANCE hi = LoadLibrary (cbuf);
	if (hi) {
		struct DLLModule *tmp = new struct DLLModule[nmodule+1]; TRACENEW
		if (nmodule) {
			memcpy (tmp, module, nmodule*sizeof(struct DLLModule));
			delete []module;
		}
		module = tmp;
		module[nmodule].module = (register_module ? register_module : new oapi::Module (hi));
		module[nmodule].hMod = hi;
		module[nmodule].name = new char[strlen(name)+1]; TRACENEW
		strcpy (module[nmodule].name, name);
		nmodule++;
	} else {
		DWORD err = GetLastError();
		LOGOUT_ERR ("Failed loading module %s (code %d)", cbuf, err);
	}
	return hi;
}

//-----------------------------------------------------------------------------
// Name: UnloadModule()
// Desc: Unload a named plugin DLL
//-----------------------------------------------------------------------------
void Orbiter::UnloadModule (const char *name)
{
	DWORD i, j, k;
	struct DLLModule *tmp;
	for (i = 0; i < nmodule; i++)
		if (!_stricmp (module[i].name, name)) break;
	if (i == nmodule) return; // not present
	delete []module[i].name;
	delete module[i].module;
	FreeLibrary (module[i].hMod);
	if (nmodule > 1) {
		tmp = new struct DLLModule[nmodule-1]; TRACENEW
		for (j = k = 0; j < nmodule; j++)
			if (j != i) tmp[k++] = module[j];
	} else tmp = 0;
	delete []module;
	module = tmp;
	nmodule--;
}

//-----------------------------------------------------------------------------
// Name: UnloadModule()
// Desc: Unload a module by its instance
//-----------------------------------------------------------------------------
void Orbiter::UnloadModule (HINSTANCE hi)
{
	DWORD i, j, k;
	struct DLLModule *tmp;
	for (i = 0; i < nmodule; i++)
		if (hi == module[i].hMod) break;
	if (i == nmodule) return; // not present
	delete []module[i].name;
	delete module[i].module;
	FreeLibrary (module[i].hMod);
	if (nmodule > 1) {
		tmp = new struct DLLModule[nmodule-1]; TRACENEW
		for (j = k = 0; j < nmodule; j++)
			if (j != i) tmp[k++] = module[j];
	} else tmp = 0;
	delete []module;
	module = tmp;
	nmodule--;
}

//-----------------------------------------------------------------------------
// Name: FindModuleProc()
// Desc: Returns address of a procedure in a plugin module
//-----------------------------------------------------------------------------
OPC_Proc Orbiter::FindModuleProc (DWORD nmod, const char *procname)
{
	return (OPC_Proc)GetProcAddress (module[nmod].hMod, procname);
}

//-----------------------------------------------------------------------------
// Name: Launch()
// Desc: Launch simulator
//-----------------------------------------------------------------------------
VOID Orbiter::Launch (const char *scenario)
{
	HCURSOR hCursor = SetCursor (LoadCursor (NULL, IDC_WAIT));
	bool have_state = false;
	pConfig->Write (); // save current settings
	pMainDlg->WriteExtraParams ();

	if (!have_state && !pState->Read (ScnPath (scenario))) {
		LOGOUT_ERR ("Scenario not found: %s", scenario);
		TerminateOnError();
	}
	DlgHelp::SetScenarioHelp (pState->ScnHelp());

	long m0 = memstat->HeapUsage();
	CreateRenderWindow (hDlg, pConfig, scenario);
	simheapsize = memstat->HeapUsage()-m0;
	SetCursor (hCursor);
}

//-----------------------------------------------------------------------------
// Name: CreateRenderWindow()
// Desc: Create the window used for rendering the scene
//-----------------------------------------------------------------------------
HWND Orbiter::CreateRenderWindow (HWND parentWnd, Config *pCfg, const char *scenario)
{
	DWORD i;

	SetLogVerbosity (pCfg->CfgDebugPrm.bVerboseLog);
	LOGOUT("");
	LOGOUT("**** Creating simulation session");

	ShowWindow (hDlg, SW_HIDE); // hide launchpad dialog
	
	if (gclient) {
		hRenderWnd = gclient->InitRenderWnd (gclient->clbkCreateRenderWindow());
		GetRenderParameters ();
	} else {
		hRenderWnd = NULL;
		m_pConsole = new orbiter::ConsoleNG(this);
	}

	if (hRenderWnd) {

		// Create keyboard device
		if (!pDI->CreateKbdDevice (hRenderWnd)) {
			CloseSession ();
			return 0;
		}

		// Create joystick device
		if (pDI->CreateJoyDevice (hRenderWnd)) {
			// initialise startup throttle setting
			plZ4 = 1;
			if (pDI->joyprop.bThrottle && pCfg->CfgJoystickPrm.bThrottleIgnore) {
				DIJOYSTATE2 js;
				if (pDI->PollJoystick (&js))
					plZ4 = *(long*)(((BYTE*)&js)+pDI->joyprop.ThrottleOfs) >> 3;
			}
		}
	}

	// read simulation environment state
	strcpy (ScenarioName, scenario);
	g_qsaveid = 0;
	launch_tick = 3;
	if (pCfg->CfgDebugPrm.TimerMode == 2) use_fine_counter = FALSE;

	// Generate logical world objects
#ifdef INLINEGRAPHICS
	// these should be called from withing oclient
	CreatePatchDeviceObjects (oclient->m_pD3D, oclient->m_pd3dDevice);
	VObject::CreateDeviceObjects (oclient);
	PatchManager::CreateDeviceObjects (oclient->m_pD3D, oclient->m_pd3dDevice);
	TileManager::CreateDeviceObjects (oclient->m_pD3D, oclient->m_pd3dDevice);
	TileManager2Base::CreateDeviceObjects (oclient->m_pD3D, oclient->m_pd3dDevice);
	CSphereManager::CreateDeviceObjects (oclient->m_pD3D, oclient->m_pd3dDevice);
	VVessel::CreateDeviceObjects (oclient->m_pd3dDevice);
#endif // INLINEGRAPHICS
	if (gclient) {
		Base::CreateStaticDeviceObjects();
	}
	BroadcastGlobalInit ();
	RigidBody::GlobalSetup();

	td.Reset (pState->Mjd());
	if (Cfg()->CfgCmdlinePrm.FixedStep > 0.0)
		td.SetFixedStep(Cfg()->CfgCmdlinePrm.FixedStep);
	else if (Cfg()->CfgDebugPrm.FixedStep > 0.0)
		td.SetFixedStep(Cfg()->CfgDebugPrm.FixedStep);

	if (!InitializeWorld (pState->Solsys())) {
		LOGOUT_ERR_FILENOTFOUND_MSG(g_pOrbiter->ConfigPath (pState->Solsys()), "while initialising solar system %s", pState->Solsys());
		TerminateOnError();
		return 0;
	}
	LOGOUT("Finished initialising world");
	ms_prev = timeGetTime () - 1; // make sure SimDT > 0 for first frame

	g_psys->InitState (ScnPath (scenario));

	g_focusobj = 0;
	Vessel *vfocus = g_psys->GetVessel (pState->Focus());
	if (!vfocus)
		vfocus = g_psys->GetVessel ((DWORD)0); // in case no focus vessel was defined
	SetFocusObject (vfocus, false);

	LOGOUT("Finished initialising status");

	g_camera->InitState (scenario, g_focusobj);
	if (g_pane) g_pane->SetFOV (g_camera->Aperture());
	LOGOUT ("Finished initialising camera");

	if (gclient) {
		// GDI resources - NOT VALID FOR ALL CLIENTS!
		InitializeGDIResources (hRenderWnd);
		pDlgMgr = new DialogManager (this, hRenderWnd);

		// global dialog resources
		InlineDialog::GlobalInit (gclient);
		g_select = new Select (gclient, hRenderWnd); TRACENEW
		g_input = new InputBox (gclient, hRenderWnd, 256); TRACENEW
		
		// playback screen annotation manager
		snote_playback = gclient->clbkCreateAnnotation ();
	}
	else {
		pDlgMgr = new DialogManager(this, m_pConsole->WindowHandle());
	}

#ifdef INLINEGRAPHICS
	//snote_playback = new ScreenNote (this, viewW, viewH);
#endif // INLINEGRAPHICS

	ddeserver = new DDEServer (hRenderWnd);

	bSession = true;
	bVisible = (hRenderWnd != NULL);
	bRunning = bRequestRunning = true;
	bRenderOnce = FALSE;
	g_bForceUpdate = true;
#ifdef UNDEF
	if (pCfg->CfgLogicPrm.bStartPaused) {
		BeginTimeStep (true);
		UpdateWorld(); // otherwise it doesn't get initialised during pause
		EndTimeStep (true);
		Pause (TRUE);
	}
#endif
	FRecorder_Reset();
	if (bPlayback = g_focusobj->bFRplayback) {
		FRecorder_OpenPlayback (pState->PlaybackDir());
		if (g_pane && g_pane->MIBar()) g_pane->MIBar()->SetPlayback(true);
	}

	// let plugins read their states from the scenario file
	for (i = 0; i < nmodule; i++) {
		void (*opcLoadState)(FILEHANDLE) = (void(*)(FILEHANDLE))FindModuleProc (i, "opcLoadState");
		if (opcLoadState) {
			ifstream ifs (ScnPath (scenario));
			char cbuf[256] = "BEGIN_";
			strcat (cbuf, module[i].name);
			if (FindLine (ifs, cbuf)) {
				opcLoadState ((FILEHANDLE)&ifs);
			}
		}
	}

	Module::RenderMode rendermode = (
		hRenderWnd ? (bFullscreen ? Module::RENDER_FULLSCREEN : Module::RENDER_WINDOW) : Module::RENDER_NONE
	);
	//for (i = 0; i < nmodule; i++) {
	//	module[i].module->clbkSimulationStart (rendermode);
	//	CHECKCWD(cwd,module[i].name);
	//}

	if (gclient) {
		SetFocus (hDlg);
		Sleep(2);
		SetFocus (hRenderWnd);
	}
	// make sure render window has focus on start

	LOGOUT ("Finished setting up render state");

	const char *scriptcmd = pState->Script();
	hScnInterp = (scriptcmd ? script->RunInterpreter (scriptcmd) : NULL);

	if (gclient) gclient->clbkPostCreation();
	g_psys->PostCreation ();

	for (i = 0; i < nmodule; i++) {
		module[i].module->clbkSimulationStart (rendermode);
		CHECKCWD(cwd,module[i].name);
	}

	if (g_pane) {
		g_pane->InitState (ScnPath (scenario));
		LOGOUT ("Finished initialising panels");
	}

	if (pCfg->CfgLogicPrm.bStartPaused) {
		BeginTimeStep (true);
		UpdateWorld(); // otherwise it doesn't get initialised during pause
		EndTimeStep (true);
		Pause (TRUE);
	}

	if (m_pConsole)
		m_pConsole->EchoIntro();

	return hRenderWnd;
}

void Orbiter::PreCloseSession()
{
	// DEBUG
	if (pDlgMgr)  { pDlgMgr->Clear(); }

	if (gclient && pConfig->CfgDebugPrm.bSaveExitScreen)
		gclient->clbkSaveSurfaceToImage (0, "Images\\CurrentState", oapi::IMAGE_JPG);
}

//-----------------------------------------------------------------------------
// Name: CloseSession()
// Desc: Destroy render window and associated devices
//-----------------------------------------------------------------------------
void Orbiter::CloseSession ()
{
	bSession = false;

	if      (bRecord)   ToggleRecorder();
	else if (bPlayback) EndPlayback();
	char *desc = "Current scenario state\n\n\nContains the latest simulation state.";
	SaveScenario (CurrentScenario, desc);
	if (hScnInterp) {
		script->DelInterpreter (hScnInterp);
		hScnInterp = NULL;
	}
	if (m_pConsole) {
		delete m_pConsole;
		m_pConsole = NULL;
	}
	if (ddeserver) {
		delete ddeserver;
		ddeserver = NULL;
	}

	if (pConfig->CfgDebugPrm.ShutdownMode == 0 && !bFastExit) { // normal cleanup
		pMainDlg->SelRootScenario (CurrentScenario);
		ShowWindow (hDlg, SW_SHOW); // show launchpad dialog
		pMainDlg->ShowWaitPage (hDlg, true, simheapsize);
		RedrawWindow (hDlg, NULL, NULL, RDW_UPDATENOW|RDW_ALLCHILDREN);
		if (gclient) {
			gclient->clbkCloseSession (false);
			Base::DestroyStaticDeviceObjects ();
		}
		if (snote_playback) delete snote_playback;
		if (nsnote) {
			for (int i = 0; i < nsnote; i++) delete snote[i];
			delete []snote;
			nsnote = 0;
		}
		InlineDialog::GlobalExit (gclient);

		if (g_input)  { delete g_input; g_input = 0; }
		if (g_select) { delete g_select; g_select = 0; }
		if (g_pane) { delete g_pane;   g_pane = 0; }
		if (pDlgMgr)  { delete pDlgMgr; pDlgMgr = 0; }
		Instrument::GlobalExit (gclient);
		ReleaseGDIResources();
		meshmanager.Flush(); // destroy buffered meshes
		DestroyWorld ();     // destroy logical objects
		if (gclient)
			gclient->clbkDestroyRenderWindow (false); // destroy graphics objects

		for (int i = 0; i < nmodule; i++)
			module[i].module->clbkSimulationEnd();

		hRenderWnd = NULL;
		pDI->DestroyDevices();
		pMainDlg->ShowWaitPage (hDlg, false);
		RedrawWindow (hDlg, NULL, NULL, RDW_UPDATENOW|RDW_ALLCHILDREN);
	} else {
		if (pDlgMgr)  { delete pDlgMgr; pDlgMgr = 0; }
		if (gclient) {
			gclient->clbkCloseSession (true);
			gclient->clbkDestroyRenderWindow (true);
		}

		for (int i = 0; i < nmodule; i++)
			module[i].module->clbkSimulationEnd();

		hRenderWnd = NULL;
		pDI->DestroyDevices();
		CloseApp (true);
		if (pConfig->CfgDebugPrm.ShutdownMode == 2 || bFastExit) {
			LOGOUT("**** Fast process shutdown\r\n");
			exit (0); // just kill the process
		} else {
			LOGOUT("**** Respawning Orbiter process\r\n");
#ifdef INLINEGRAPHICS
			char *name = "orbiter.exe";
			CloseHandle(hMutex);        // delete mutex so that we don't block the child
#else
			char *name = "modules\\server\\orbiter.exe";
#endif
			_execl (name, name, "-l", NULL);   // respawn the process
		}
	}
	LOGOUT("**** Closing simulation session");
}

// =======================================================================
// Query graphics client for render parameters

void Orbiter::GetRenderParameters ()
{
	if (!gclient) return; // sanity check

	DWORD val;
	gclient->clbkGetViewportSize (&viewW, &viewH);
	viewBPP = (gclient->clbkGetRenderParam (RP_COLOURDEPTH, &val) ? val:0);
	bFullscreen = gclient->clbkFullscreenMode();
	bUseStencil = (pConfig->CfgDevPrm.bTryStencil && 
		gclient->clbkGetRenderParam (RP_STENCILDEPTH, &val) && val >= 1);
}

// =======================================================================
// Send session initialisation signal to various components

void Orbiter::BroadcastGlobalInit ()
{
	Instrument::GlobalInit (gclient);
	DlgMap::GlobalInit();
}

// =======================================================================
// Render3DEnvironment()
// Draws the scene

HRESULT Orbiter::Render3DEnvironment ()
{
	if (gclient) {
		gclient->clbkRenderScene ();
		Output2DData ();
		gclient->clbkDisplayFrame ();
	}
    return S_OK;
}

//-----------------------------------------------------------------------------
// Name: ScreenToClient()
// Desc: Converts screen to client coordinates. In fullscreen mode they are identical
//-----------------------------------------------------------------------------
void Orbiter::ScreenToClient (POINT *pt) const
{
	if (!IsFullscreen() && hRenderWnd)
		::ScreenToClient (hRenderWnd, pt);
}

//-----------------------------------------------------------------------------
// Name: Run()
// Desc: Message-processing loop. Idle time is used to render the scene.
//-----------------------------------------------------------------------------
INT Orbiter::Run ()
{
    // Receive and process Windows messages
    BOOL  bGotMsg, bCanRender, bpCanRender = TRUE;
    MSG   msg;
    PeekMessage (&msg, NULL, 0U, 0U, PM_NOREMOVE);

	if (!pConfig->CfgCmdlinePrm.LaunchScenario.empty())
		Launch (pConfig->CfgCmdlinePrm.LaunchScenario.c_str());
	// otherwise wait for the user to make a selection from the scenario
	// list in the launchpad dialog

	while (WM_QUIT != msg.message) {

#ifdef DO_NETWORK_OLD
		bGotMsg = PeekMessage (&msg, NULL, 0U, 0U, PM_REMOVE);
#else
        // Use PeekMessage() if the app is active, so we can use idle time to
        // render the scene. Else, use GetMessage() to avoid eating CPU time.
		if (bSession) {
            bGotMsg = PeekMessage (&msg, NULL, 0U, 0U, PM_REMOVE);
		} else {
            bGotMsg = GetMessage (&msg, NULL, 0U, 0U);
		}
#endif
        if (bGotMsg) {
			if (!hDlg || !IsDialogMessage (hDlg, &msg)) {
				TranslateMessage (&msg);
				DispatchMessage (&msg);
			}
		} else {

#ifdef DO_NETWORK_OLD
			// pick multiplayer messages
			if (bMultiplayer) GetDPlay()->Receive ();
#endif

			if (bSession) {
				if (bAllowInput) bActive = true, bAllowInput = false;
				if (BeginTimeStep (bRunning)) {
					UpdateWorld();
					EndTimeStep (bRunning);
					if (bVisible) {
						if (bActive) UserInput ();
						bRenderOnce = TRUE;
					}
					if (bRunning && bCapture) {
						CaptureVideoFrame ();
					}
				}
				if (m_pConsole)
					m_pConsole->ParseCmd();
			}
        }
		if (bRenderOnce && bVisible) {
			if (FAILED (Render3DEnvironment ()))
				if (hRenderWnd) DestroyWindow (hRenderWnd);
			bRenderOnce = FALSE;
		}

		if (bSession) {
#ifdef INLINEGRAPHICS
			bCanRender = (oclient->m_pDD->TestCooperativeLevel() == DD_OK);
#else
			bCanRender = TRUE;
#endif
			if (bCanRender && !bpCanRender)
				RestoreDeviceObjects ();
			bpCanRender = bCanRender;
		} else
			bpCanRender = TRUE;
    }
	hRenderWnd = NULL;
    return msg.wParam;
}

void Orbiter::SingleFrame ()
{
	if (bSession) {
		if (bAllowInput) bActive = true, bAllowInput = false;
		if (BeginTimeStep (bRunning)) {
			UpdateWorld();
			EndTimeStep (bRunning);
			if (bVisible) {
				if (bActive) UserInput ();
				Render3DEnvironment();
			}
		}
	}
}

void Orbiter::TerminateOnError ()
{
	LogOut (">>> TERMINATING <<<");
	if (hRenderWnd) ShowWindow (hRenderWnd, FALSE);
	MessageBox (NULL,
		"Terminating after critical error. See Orbiter.log for details.",
		"Orbiter: Critical Error", MB_OK | MB_ICONERROR);
	exit (1);
}

void Orbiter::UpdateServerWnd (HWND hWnd)
{
	char cbuf[256];
	sprintf (cbuf, "%0.0fs", td.SysT0);
	SetWindowText (GetDlgItem (hWnd, IDC_STATIC1), cbuf);
	sprintf (cbuf, "%0.0fs", td.SimT0);
	SetWindowText (GetDlgItem (hWnd, IDC_STATIC2), cbuf);
	sprintf (cbuf, "%0.5f", td.MJD0);
	SetWindowText (GetDlgItem (hWnd, IDC_STATIC3), cbuf);
	sprintf (cbuf, "%0.1fx", td.Warp());
	SetWindowText (GetDlgItem (hWnd, IDC_STATIC4), cbuf);
	sprintf (cbuf, "%f", td.SimDT);
	SetWindowText (GetDlgItem (hWnd, IDC_STATIC5), cbuf);
	sprintf (cbuf, "%f", td.FPS());
	SetWindowText (GetDlgItem (hWnd, IDC_STATIC6), cbuf);
	sprintf (cbuf, "%d", g_psys->nVessel());
	SetWindowText (GetDlgItem (hWnd, IDC_STATIC7), cbuf);
}

void Orbiter::InitRotationMode ()
{
	bKeepFocus = true;
	ShowCursor (FALSE);
	SetCapture (hRenderWnd);

	// Limit cursor to render window confines, so we don't miss the button up event
	if (!bFullscreen && hRenderWnd) {
		RECT rClient;
		GetClientRect (hRenderWnd, &rClient);
		POINT pLeftTop = {rClient.left, rClient.top};
		POINT pRightBottom = {rClient.right, rClient.bottom};
		ClientToScreen (hRenderWnd, &pLeftTop);
		ClientToScreen (hRenderWnd, &pRightBottom);
		RECT rScreen = {pLeftTop.x, pLeftTop.y, pRightBottom.x, pRightBottom.y};
		ClipCursor (&rScreen);
	}
}

void Orbiter::ExitRotationMode ()
{
	bKeepFocus = false;
	ReleaseCapture ();
	ShowCursor (TRUE);

	// Release cursor from render window confines
	if (!bFullscreen && hRenderWnd) {
		ClipCursor (NULL);
	}
}

#ifdef DO_NETWORK_OLD
//-----------------------------------------------------------------------------
// Name: ProcessDPSystemMsg()
// Desc: Evaluates DirectPlay system messages and performs appropriate actions
//-----------------------------------------------------------------------------
void Orbiter::ProcessDPSystemMsg (DPMSG_GENERIC *pMsg, DWORD size, DPID from, DPID to)
{
	switch (pMsg->dwType) {
	case DPSYS_CREATEPLAYERORGROUP: {
		DPMSG_CREATEPLAYERORGROUP *msg = (DPMSG_CREATEPLAYERORGROUP*)pMsg;
		GetDPlay()->RefreshPlayerList();
		pMainDlg->Network_PlayerListChanged();
		LOGOUT1P ("Player joined: %s", (msg->dpnName.lpszLongNameA));
		} break;
	case DPSYS_DESTROYPLAYERORGROUP: {
		DPMSG_DESTROYPLAYERORGROUP *msg = (DPMSG_DESTROYPLAYERORGROUP*)pMsg;
		GetDPlay()->RefreshPlayerList();
		pMainDlg->Network_PlayerListChanged();
		LOGOUT1P ("Player left: %s", (msg->dpnName.lpszLongNameA));
		} break;
	}
}

void Orbiter::ProcessDPApplicationMsg (LPVOID data, DWORD size, DPID from, DPID to)
{
	DpMsg *msg = (DpMsg*)data;
	switch (msg->id) {
	case MPMSG_LAUNCH:
		LOGOUT("Multiplayer game launched by remote server");
		Launch(*(double*)msg->data);
		break;
	}
}
#endif

//-----------------------------------------------------------------------------
// Name: Pause()
// Desc: Stop/continue simulation
//-----------------------------------------------------------------------------
void Orbiter::Pause (bool bPause)
{
	if (bRunning != bPause) return;  // nothing to do
	bRequestRunning = !bPause;
}

void Orbiter::Freeze (bool bFreeze)
{
	if (bRunning != bFreeze) return; // nothing to do
	bRunning = !bFreeze;
	bSession = !bFreeze;

	// broadcast pause state to plugins
	for (DWORD k = 0; k < nmodule; k++) {
		module[k].module->clbkPause (bFreeze);
	}
	if (bFreeze) Suspend ();
	else Resume ();
}

//-----------------------------------------------------------------------------
// Name: SetFocusObject()
// Desc: Select a new user-controlled vessel
//       Return value is the previous focus object
//-----------------------------------------------------------------------------
Vessel *Orbiter::SetFocusObject (Vessel *vessel, bool setview)
{
	if (vessel == g_focusobj) return 0; // nothing to do

	g_pfocusobj = g_focusobj;
	g_focusobj = vessel;

	// Inform pane about focus change
	if (g_pane) g_pane->FocusChanged (g_focusobj);

	// switch camera
	if (setview) SetView (g_focusobj, 2);

	// vessel and plugin callback
	if (g_pfocusobj) g_pfocusobj->FocusChanged (false, g_focusobj, g_pfocusobj);
	g_focusobj->FocusChanged (true, g_focusobj, g_pfocusobj);

	for (DWORD i = 0; i < nmodule; i++)
		module[i].module->clbkFocusChanged (g_focusobj, g_pfocusobj);
		//if (module[i].intf->opcFocusChanged)
		//	module[i].intf->opcFocusChanged (g_focusobj, g_pfocusobj);

	if (pDlgMgr) pDlgMgr->BroadcastMessage (MSG_FOCUSVESSEL, vessel);
	DlgHelp::SetVesselHelp (g_focusobj->HelpContext());

	return g_pfocusobj;
}

// =======================================================================
// SetView()
// Change camera target, or camera mode (cockpit/external)

void Orbiter::SetView (Body *body, int mode)
{
	g_camera->Attach (body, mode);
	g_bForceUpdate = true;
}

//-----------------------------------------------------------------------------
// Name: InsertVessels
// Desc: Insert a newly created vessel into the simulation
//-----------------------------------------------------------------------------
void Orbiter::InsertVessel (Vessel *vessel)
{
	g_psys->AddVessel (vessel);

	// broadcast vessel creation to plugins
	for (DWORD k = 0; k < nmodule; k++)
		module[k].module->clbkNewVessel ((OBJHANDLE)vessel);
#ifdef INLINEGRAPHICS
	oclient->clbkNewVessel ((OBJHANDLE)vessel);
#endif // INLINEGRAPHICS

	if (pDlgMgr) pDlgMgr->BroadcastMessage (MSG_CREATEVESSEL, vessel);
	//if (gclient) gclient->clbkDialogBroadcast (MSG_CREATEVESSEL, vessel);

	vessel->PostCreation();
	vessel->InitSupervessel();
	vessel->ModulePostCreation();
}

//-----------------------------------------------------------------------------
// Name: KillVessels()
// Desc: Kill the vessels that have been marked for deletion in the last time
//       step
//-----------------------------------------------------------------------------
bool Orbiter::KillVessels ()
{
	int i, n = g_psys->nVessel();
	DWORD j;

	for (i = n-1; i >= 0; i--) {
		if (g_psys->GetVessel(i)->KillPending()) {
			Vessel *vessel = g_psys->GetVessel(i);
			// switch to new focus object
			if (vessel == g_focusobj) {
				if (vessel->ProxyVessel() && !vessel->ProxyVessel()->KillPending()) {
					SetFocusObject (vessel->ProxyVessel(), false);
				} else {
					double d, dmin = 1e20;
					Vessel *v, *tgt = 0;
					for (j = 0; j < g_psys->nVessel(); j++) {
						v = g_psys->GetVessel(j);
						if (v->KillPending()) continue;
						if (v != vessel && v->GetEnableFocus()) {
							d = vessel->GPos().dist (v->GPos());
							if (d < dmin) dmin = d, tgt = v;
						}
					}
					if (tgt) SetFocusObject (tgt, false);
					else return false; // no focus object available - give up
				}
			}
			if (vessel == g_pfocusobj)
				g_pfocusobj = 0; // clear previous focus (for Ctrl-F3 fast-switching)

			// switch to new camera target
			if (vessel == g_camera->Target())
				SetView (g_focusobj, 1);
			// broadcast vessel destruction to plugins
			for (DWORD k = 0; k < nmodule; k++) {
				module[k].module->clbkDeleteVessel ((OBJHANDLE)vessel);
			}
#ifdef INLINEGRAPHICS
			oclient->clbkDeleteVessel ((OBJHANDLE)vessel);
#endif
			// broadcast vessel destruction to all vessels
			g_psys->BroadcastVessel (MSG_KILLVESSEL, vessel);
			// broadcast vessel destruction to all MFDs
			if (g_pane) g_pane->DelVessel (vessel);
			// broadcast vessel destruction to all open dialogs
			//if (gclient) gclient->clbkDialogBroadcast (MSG_KILLVESSEL, vessel);
			if (pDlgMgr) pDlgMgr->BroadcastMessage (MSG_KILLVESSEL, vessel);
			// echo deletion on console window
			if (m_pConsole) {
				char cbuf[256];
				sprintf (cbuf, "Vessel %s deleted", vessel->Name());
				m_pConsole->Echo(cbuf);
			}
			// kill the vessel
			g_psys->DelVessel (vessel, 0);
		}
	}
	return true;
}

void Orbiter::NotifyObjectJump (const Body *obj, const Vector &shift)
{
	if (obj == g_camera->Target()) g_camera->Drag (-shift);
	if (g_camera->Target()) g_camera->Update ();

	// notify plugins
	for (DWORD k = 0; k < nmodule; k++)
		module[k].module->clbkVesselJump ((OBJHANDLE)obj);

#ifdef INLINEGRAPHICS
	oclient->GetScene()->Update (g_psys, &g_camera, 1, false/*m_bRunning*/, false);
#endif // INLINEGRAPHICS
}

void Orbiter::NotifyObjectSize (const Body *obj)
{
	if (obj == g_camera->Target()) g_camera->Drag (Vector(0,0,0));
}

//-----------------------------------------------------------------------------
// Name: SetWarpFactor()
// Desc: Set time acceleration factor
//-----------------------------------------------------------------------------
void Orbiter::SetWarpFactor (double warp, bool force, double delay)
{
	if (warp == td.Warp())
		return; // nothing to do
	if (bPlayback && pConfig->CfgRecPlayPrm.bReplayWarp && !force) return;
	const double EPS = 1e-6;
	if      (warp < MinWarpLimit) warp = MinWarpLimit;
	else if (warp > MaxWarpLimit) warp = MaxWarpLimit;
	if (fabs (warp-td.Warp()) > EPS) {
		td.SetWarp (warp, delay);
		if (td.WarpChanged()) ApplyWarpFactor();
		DlgTacc *pDlg = (pDlgMgr ? pDlgMgr->EntryExists<DlgTacc> (hInst) : NULL);
		if (pDlg) pDlg->RegisterWarp(pDlg->GetHwnd(), warp, false, true, true);
		if (bRecord && pConfig->CfgRecPlayPrm.bRecordWarp) {
			char cbuf[256];
			if (delay) sprintf (cbuf, "%f %f", warp, delay);
			else       sprintf (cbuf, "%f", warp);
			FRecorder_SaveEvent ("TACC", cbuf);
			//for (DWORD i = 0; i < g_psys->nVessel(); i++)
			//	g_psys->GetVessel(i)->FRecorder_SaveEvent ("TACC", cbuf);
		}
	}
	if (m_pConsole) {
		char cbuf[256];
		sprintf (cbuf, "Time acceleration set to %0.1f", warp);
		m_pConsole->Echo(cbuf);
	}
}

//-----------------------------------------------------------------------------
// Name: IncWarpFactor()
// Desc: Increment time acceleration factor to next power of 10
//-----------------------------------------------------------------------------
void Orbiter::IncWarpFactor ()
{
	const double EPS = 1e-6;
	double logw = log10 (td.Warp());
	SetWarpFactor (pow (10.0, floor (logw+EPS)+1.0));
}

//-----------------------------------------------------------------------------
// Name: DecWarpFactor()
// Desc: Decrement time acceleration factor to next lower power of 10
//-----------------------------------------------------------------------------
void Orbiter::DecWarpFactor ()
{
	const double EPS = 1e-6;
	double logw = log10 (td.Warp());
	SetWarpFactor (pow (10.0, ceil (logw-EPS)-1.0));
}

//-----------------------------------------------------------------------------
// Name: ApplyWarpFactor()
// Desc: Broadcast new warp factor to components and modules
//-----------------------------------------------------------------------------
void Orbiter::ApplyWarpFactor ()
{
	double nwarp = td.Warp();
	for (DWORD i = 0; i < nmodule; i++)
		module[i].module->clbkTimeAccChanged (nwarp, td.Warp());
	if (g_pane) g_pane->SetWarp (nwarp);
	bRealtime = (fabs (nwarp-1.0) < 1e-6);
}

//-----------------------------------------------------------------------------
// Name: SetFOV()
// Desc: Set field of view. Argument is FOV for vertical half-screen [rad]
//-----------------------------------------------------------------------------

VOID Orbiter::SetFOV (double fov, bool limit_range)
{
	if (g_camera->Aperture() == fov) return;

	fov = g_camera->SetAperture (fov, limit_range);
	if (g_pane) g_pane->SetFOV (fov);
	g_bForceUpdate = true;

	// update Camera dialog
	HWND hCamDlg;
	if (pDlgMgr && (hCamDlg = pDlgMgr->IsEntry (hInst, IDD_CAMERA)))
		SendMessage (hCamDlg, WM_APP, 0, (LPARAM)&fov);
}

//-----------------------------------------------------------------------------
// Name: IncFOV()
// Desc: Increase field of view. Argument is delta FOV for vertical half-screen [rad]
//-----------------------------------------------------------------------------

VOID Orbiter::IncFOV (double dfov)
{
	double fov = g_camera->IncrAperture (dfov);
	if (g_pane) g_pane->SetFOV (fov);
	g_bForceUpdate = true;

	// update Camera dialog
	HWND hCamDlg;
	if (pDlgMgr && (hCamDlg = pDlgMgr->IsEntry (hInst, IDD_CAMERA)))
		SendMessage (hCamDlg, WM_APP, 0, (LPARAM)&fov);
}

//-----------------------------------------------------------------------------
// Name: SaveScenario()
// Desc: save current status in-game
//-----------------------------------------------------------------------------
bool Orbiter::SaveScenario (const char *fname, const char *desc)
{
	pState->Update (desc);

	ofstream ofs (ScnPath (fname));
	if (ofs) {

		// save scenario state
		pState->Write (ofs, pConfig->CfgDebugPrm.bSaveExitScreen ? "CurrentState_img" : "CurrentState");
		g_camera->Write (ofs);
		if (g_pane) g_pane->Write (ofs);
		g_psys->Write (ofs);

		// let plugins save their states to the scenario file
		for (DWORD k = 0; k < nmodule; k++) {
			void (*opcSaveState)(FILEHANDLE) = (void(*)(FILEHANDLE))FindModuleProc (k, "opcSaveState");
			if (opcSaveState) {
				ofs << endl << "BEGIN_" << module[k].name << endl;
				opcSaveState ((FILEHANDLE)&ofs);
				ofs << "END" << endl;
			}
		}

		pMainDlg->RefreshScenarioList();
		return true;

	} else return false;
}

//-----------------------------------------------------------------------------
// Name: SendScenario()
// Desc: send current simulation state as a scenario to a socket
//-----------------------------------------------------------------------------
#ifdef NETCONNECT
bool Orbiter::SendScenario (OrbiterConnect *oc, const char *desc)
{
	pState->Update (desc);

	if (!pState->Send (oc)) return false;
	if (!g_psys->Send (oc)) return false;
	if (!g_camera->Send (oc)) return false;
	return true;
}

void Orbiter::CheckRequests (OrbiterConnect *oc)
{
	DWORD req;
	if (oc->RecvRequest (req) != SOCKET_ERROR) {
		switch (req) {
		case CON_REQ_MJD:
			SendMJD (oc);
			break;
		}
	}
}

struct Packet_MJD {
	DWORD req;
	double mjd;
};

void Orbiter::SendMJD (OrbiterConnect *oc)
{
	Packet_MJD pMJD;
	pMJD.req = CON_REQ_MJD;
	pMJD.mjd = mjd;
	oc->Send ((char*)&pMJD, sizeof(Packet_MJD));
}

bool Orbiter::RecvMJD (OrbiterConnect *oc, double *mjd)
{
	Packet_MJD pMJD;
	if (oc->Recv ((char*)&pMJD, sizeof(Packet_MJD)) == SOCKET_ERROR || pMJD.req != CON_REQ_MJD) return false;
	*mjd = pMJD.mjd;
	return true;
}

#endif // NETCONNECT
//-----------------------------------------------------------------------------
// Name: Quicksave()
// Desc: save current status in-game
//-----------------------------------------------------------------------------
VOID Orbiter::Quicksave ()
{
	int i;
	char desc[256], fname[256];
	sprintf (desc, "Orbiter saved state at T = %0.0f", td.SimT0);
	for (i = strlen(ScenarioName)-1; i > 0; i--)
		if (ScenarioName[i-1] == '\\') break;
	sprintf (fname, "Quicksave\\%s %04d", ScenarioName+i, ++g_qsaveid);
	SaveScenario (fname, desc);
}

//-----------------------------------------------------------------------------
// write a single frame to bmp file (or to clipboard, if fname==NULL)

void Orbiter::CaptureVideoFrame ()
{
	if (gclient) {
		if (video_skip_count == pConfig->CfgCapturePrm.SequenceSkip) {
			char fname[256];
			sprintf (fname, "%s\\%04d", pConfig->CfgCapturePrm.SequenceDir, pConfig->CfgCapturePrm.SequenceStart++);
			oapi::ImageFileFormat fmt = (oapi::ImageFileFormat)pConfig->CfgCapturePrm.ImageFormat;
			float quality = (float)pConfig->CfgCapturePrm.ImageQuality/10.0f;
			gclient->clbkSaveSurfaceToImage (0, fname, fmt, quality);
			video_skip_count = 0;
		} else video_skip_count++;
	}
}

//-----------------------------------------------------------------------------
// Name: PlaybackSave()
// Desc: save the start scenario for a recorded simulation
//-----------------------------------------------------------------------------
VOID Orbiter::SavePlaybackScn (const char *fname)
{
	char desc[256], scn[256] = "Playback\\";
	sprintf (desc, "Orbiter playback scenario at T = %0.0f", td.SimT0);
	strcat (scn, fname);
	SaveScenario (scn, desc);
}

const char *Orbiter::GetDefRecordName (void) const
{
	const char *playbackdir = pState->PlaybackDir();
	int i;
	for (i = strlen(playbackdir)-1; i > 0; i--)
		if (playbackdir[i-1] == '\\') break;
	return playbackdir+i;
}

void Orbiter::ToggleRecorder (bool force, bool append)
{
	if (bPlayback) return; // don't allow recording during playback

	DlgRecorder *pDlg = (pDlgMgr ? pDlgMgr->EntryExists<DlgRecorder> (hInst) : NULL);
	int i, n = g_psys->nVessel();
	const char *sname;
	char cbuf[256];
	bool bStartRecorder = !bRecord;
	if (bStartRecorder) {
		if (pDlg) {
			pDlg->GetRecordName (cbuf, 256);
			sname = cbuf;
		} else sname = GetDefRecordName();
		if (!append && !FRecorder_PrepareDir (sname, force)) {
			bStartRecorder = false;
			OpenDialogEx (IDD_MSG_FRECORDER, (DLGPROC)FRecorderMsg_DlgProc, DLG_CAPTIONCLOSE);
			return;
		}
	} else sname = 0;
	FRecorder_Activate (bStartRecorder, sname, append);
	for (i = 0; i < n; i++)
		g_psys->GetVessel(i)->FRecorder_Activate (bStartRecorder, sname, append);
	if (bStartRecorder)
		SavePlaybackScn (sname);
	if (pDlg) PostMessage (pDlg->GetHwnd(), WM_USER+1, 0, 0);
}

void Orbiter::EndPlayback ()
{
	for (DWORD i = 0; i < g_psys->nVessel(); i++)
		g_psys->GetVessel(i)->FRecorder_EndPlayback ();
	FRecorder_ClosePlayback();
	if (snote_playback) snote_playback->ClearText();
	bPlayback = false;
	if (pDlgMgr) {
		HWND hDlg = pDlgMgr->IsEntry (hInst, IDD_RECPLAY);
		if (hDlg) PostMessage (hDlg, WM_USER+1, 0, 0);
	}
	if (g_pane && g_pane->MIBar()) g_pane->MIBar()->SetPlayback(false);
}

oapi::ScreenAnnotation *Orbiter::CreateAnnotation (bool exclusive, double size, COLORREF col)
{
	if (!gclient) return NULL;
	oapi::ScreenAnnotation *sn = gclient->clbkCreateAnnotation();
	if (!sn) return NULL;
	
	sn->SetSize (size);
	VECTOR3 c = { (col      & 0xFF)/256.0,
		         ((col>>8 ) & 0xFF)/256.0,
				 ((col>>16) & 0xFF)/256.0};
	sn->SetColour (c);
	oapi::ScreenAnnotation **tmp = new oapi::ScreenAnnotation*[nsnote+1]; TRACENEW
	if (nsnote) {
		memcpy (tmp, snote, nsnote*sizeof(oapi::ScreenAnnotation*));
		delete []snote;
	}
	snote = tmp;
	snote[nsnote++] = sn;
	return sn;

	//DWORD w = oclient->GetFramework()->GetRenderWidth();
	//DWORD h = oclient->GetFramework()->GetRenderHeight();

	//ScreenNote *sn = new ScreenNote (this, w, h);
	//sn->SetSize (size);
	//sn->SetColour (col);

	//ScreenNote **tmp = new ScreenNote*[nsnote+1];
	//if (nsnote) {
	//	memcpy (tmp, snote, nsnote*sizeof(ScreenNote*));
	//	delete []snote;
	//}
	//snote = tmp;
	//snote[nsnote++] = sn;
	//return sn;
}

bool Orbiter::DeleteAnnotation (oapi::ScreenAnnotation *sn)
{
	DWORD i, j, k;

	if (!gclient) return false;
	for (i = 0; i < nsnote; i++) {
		if (snote[i] == sn) {
			oapi::ScreenAnnotation **tmp = 0;
			if (nsnote > 1) {
				tmp = new oapi::ScreenAnnotation*[nsnote-1]; TRACENEW
				for (j = k = 0; j < nsnote; j++)
					if (j != i) tmp[k++] = snote[j];
				delete []snote;
			}
			snote = tmp;
			delete sn;
			nsnote--;
			return true;
		}
	}
	return false;
}

//-----------------------------------------------------------------------------
// Name: InitDeviceObjects()
// Desc: Initialize scene objects.
//-----------------------------------------------------------------------------
HRESULT Orbiter::InitDeviceObjects ()
{
	// All of this should be moved into the inline graphics client!
#ifdef INLINEGRAPHICS
    D3DVIEWPORT7 vp;
    oclient->m_pd3dDevice->GetViewport(&vp);
	// viewport-related code here

    // Turn on lighting. Light will be set during FrameMove() call
	oclient->m_pd3dDevice->SetRenderState (D3DRENDERSTATE_LIGHTING, bEnableLighting);
	oclient->m_pd3dDevice->SetRenderState (D3DRENDERSTATE_AMBIENT, g_pOrbiter->Cfg()->AmbientColour);

	//if (!pCWorld->LoadRRTextures(m_hWnd, "textures.dat"))
	//	LOGOUT("LoadRRTextures failed");

    // Set miscellaneous renderstates
	oclient->m_pd3dDevice->SetRenderState (D3DRENDERSTATE_DITHERENABLE, TRUE);
    oclient->m_pd3dDevice->SetRenderState (D3DRENDERSTATE_ZENABLE, TRUE);
	oclient->m_pd3dDevice->SetRenderState (D3DRENDERSTATE_FILLMODE, pConfig->CfgDebugPrm.bWireframeMode ? D3DFILL_WIREFRAME : D3DFILL_SOLID);
	oclient->m_pd3dDevice->SetRenderState (D3DRENDERSTATE_SHADEMODE, D3DSHADE_GOURAUD);
	oclient->m_pd3dDevice->SetRenderState (D3DRENDERSTATE_SPECULARENABLE, FALSE);
    oclient->m_pd3dDevice->SetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, FALSE);
	oclient->m_pd3dDevice->SetRenderState (D3DRENDERSTATE_DESTBLEND, D3DBLEND_INVSRCALPHA);
	oclient->m_pd3dDevice->SetRenderState (D3DRENDERSTATE_NORMALIZENORMALS, pConfig->CfgDebugPrm.bNormaliseNormals ? TRUE : FALSE);

	// Set texture renderstates
    //D3DTextr_RestoreAllTextures( pd3dDevice );
    oclient->m_pd3dDevice->SetTextureStageState (0, D3DTSS_COLORARG1, D3DTA_TEXTURE);
    oclient->m_pd3dDevice->SetTextureStageState (0, D3DTSS_COLORARG2, D3DTA_DIFFUSE);
    oclient->m_pd3dDevice->SetTextureStageState (0, D3DTSS_COLOROP,   D3DTOP_MODULATE);
	oclient->m_pd3dDevice->SetTextureStageState (0, D3DTSS_MINFILTER, D3DTFN_LINEAR);
	oclient->m_pd3dDevice->SetTextureStageState (0, D3DTSS_MAGFILTER, D3DTFG_LINEAR);
	oclient->m_pd3dDevice->SetTextureStageState (1, D3DTSS_MINFILTER, D3DTFN_LINEAR);
	oclient->m_pd3dDevice->SetTextureStageState (1, D3DTSS_MAGFILTER, D3DTFG_LINEAR);

	viewW = oclient->viewW;
	viewH = oclient->viewH;

	g_texmanager = new TextureManager (oclient->m_pd3dDevice, MAX_TEXTURE_BUFSIZE); TRACENEW
	g_texmanager->SetTexturePath (pConfig->CfgDirPrm.TextureDir);
	//g_texmanager2 = new TextureManager2 (oclient->m_pd3dDevice); TRACENEW
#endif // INLINEGRAPHICS

	//InitializeGDIResources (hRenderWnd);

	// Should InitialiseWorld code go here?
    return S_OK;
}

//-----------------------------------------------------------------------------
// Name: RestoreDeviceObjects()
// Desc: Restore objects created for a specific device
//-----------------------------------------------------------------------------

HRESULT Orbiter::RestoreDeviceObjects ()
{
#ifdef INLINEGRAPHICS
	RestorePatchDeviceObjects (oclient->m_pD3D, oclient->m_pd3dDevice);
	PatchManager::RestoreDeviceObjects (oclient->m_pD3D, oclient->m_pd3dDevice);
	g_pane->RestoreDeviceObjects (oclient->m_pD3D, oclient->m_pd3dDevice);
#endif // INLINEGRAPHICS
	return S_OK;
}

//-----------------------------------------------------------------------------
// Name: DeleteDeviceObjects()
// Desc: Delete objects created for a specific device
//-----------------------------------------------------------------------------
HRESULT Orbiter::DeleteDeviceObjects ()
{
#ifdef INLINEGRAPHICS
	//ReleaseGDIResources ();
	SAFE_DELETE (g_texmanager);
	//SAFE_DELETE (g_texmanager2);
#endif // INLINEGRAPHICS
	return S_OK;
}

static char linebuf[2][70] = {"", ""};

void Orbiter::OutputLoadStatus (const char *msg, int line)
{
	if (gclient) {
		strncpy (linebuf[line], msg, 64); linebuf[line][63] = '\0';
		gclient->clbkSplashLoadMsg (linebuf[line], line);
	}
}

void Orbiter::OutputLoadTick (int line, bool ok)
{
	if (gclient) {
		char cbuf[256];
		strcpy (cbuf, linebuf[line]);
		strcat (cbuf, ok ? " ok" : " xx");
		gclient->clbkSplashLoadMsg (cbuf, line);
	}
}

//-----------------------------------------------------------------------------
// Name: InitializeGDIResources()
// Desc: Allocate resources required for GDI display (HUD, MFD)
//-----------------------------------------------------------------------------
void Orbiter::InitializeGDIResources (HWND hWnd)
{
	// Allocate global GDI resources
	if (g_gdires) delete g_gdires;
	g_gdires = new GDIResources (hWnd, viewW, viewH, *pConfig); TRACENEW
}

//-----------------------------------------------------------------------------
// Name: ReleaseGDIResources()
// Desc: Release resources used by GDI
//-----------------------------------------------------------------------------
void Orbiter::ReleaseGDIResources ()
{
	if (g_gdires) delete g_gdires, g_gdires = 0;
}

//-----------------------------------------------------------------------------
// Name: OpenTextureFile()
// Desc: Return file handle for texture file (0=error)
//       First searches in hightex dir, then in standard dir
//-----------------------------------------------------------------------------
FILE *Orbiter::OpenTextureFile (const char *name, const char *ext)
{
	FILE *ftex = 0;
	char *pch = HTexPath (name, ext); // first try high-resolution directory
	if (pch && (ftex = fopen (pch, "rb"))) {
		LOGOUT_FINE("Texture load: %s", pch);
		return ftex;
	}
	pch = TexPath (name, ext);        // try standard texture directory
	LOGOUT_FINE("Texture load: %s", pch);
	return fopen (pch, "rb");
}

SURFHANDLE Orbiter::RegisterExhaustTexture (char *name)
{
	if (gclient) {
		char path[256];
		strcpy (path, name);
		strcat (path, ".dds");
		return gclient->clbkLoadTexture (path, 0x8);
	} else {
        return NULL;
	}
}

//-----------------------------------------------------------------------------
// Load a mesh from file, and store it persistently in the mesh manager
//-----------------------------------------------------------------------------
const Mesh *Orbiter::LoadMeshGlobal (const char *fname)
{
	const Mesh *mesh =  meshmanager.LoadMesh (fname);
	if (gclient) gclient->clbkStoreMeshPersistent ((MESHHANDLE)mesh, fname);
	return mesh;
}

const Mesh *Orbiter::LoadMeshGlobal (const char *fname, LoadMeshClbkFunc fClbk)
{
	bool firstload;
	const Mesh *mesh =  meshmanager.LoadMesh (fname, &firstload);
	if (fClbk) fClbk ((MESHHANDLE)mesh, firstload);
	if (gclient) gclient->clbkStoreMeshPersistent ((MESHHANDLE)mesh, fname);
	return mesh;
}

//-----------------------------------------------------------------------------
// Name: Output2DData()
// Desc: Output HUD and other 2D information on top of the render window
//-----------------------------------------------------------------------------
VOID Orbiter::Output2DData ()
{
	g_pane->Draw ();
	for (DWORD i = 0; i < nsnote; i++)
		snote[i]->Render();
	if (snote_playback && pConfig->CfgRecPlayPrm.bShowNotes) snote_playback->Render();
	if (g_select->IsActive()) g_select->Display (0/*oclient->m_pddsRenderTarget*/);
	if (g_input->IsActive()) g_input->Display (0/*oclient->m_pddsRenderTarget*/);

#ifdef INLINEGRAPHICS
#ifdef OUTPUT_DBG
    HDC hDC;
	if (SUCCEEDED (oclient->m_pddsRenderTarget->GetDC (&hDC))) {
		ExtTextOut (hDC, 0, viewH-15, 0, NULL, DBG_MSG, strlen (DBG_MSG), NULL);
		oclient->m_pddsRenderTarget->ReleaseDC (hDC);
	}
#endif
#endif // INLINEGRAPHICS
}

//-----------------------------------------------------------------------------
// Name: BeginTimeStep()
// Desc: Update timings for the current frame step
//-----------------------------------------------------------------------------
bool Orbiter::BeginTimeStep (bool running)
{
	// Check for a pause/resume request
	if (bRequestRunning != running) {
		running = bRunning = bRequestRunning;
		bool isPaused = !running;
		if (g_pane && g_pane->MIBar()) g_pane->MIBar()->SetPaused (isPaused);
		pDlgMgr->BroadcastMessage (MSG_PAUSE, (void*)isPaused);

		// broadcast pause state to plugins
		for (DWORD k = 0; k < nmodule; k++)
			module[k].module->clbkPause (isPaused);
	}

	// Note that for times > 1e6 the simulation time is represented by
	// an offset and increment, to avoid floating point underflow roundoff
	// when adding the current time step
	double deltat;
	DWORD ms_curr = timeGetTime ();
	LARGE_INTEGER hi_curr;
	if (use_fine_counter) QueryPerformanceCounter (&hi_curr);

	if (launch_tick) {
		// control time interval in first few frames, when loading events occur
		// enforce interval 10ms for first 3 time steps
		deltat = 1e-2;
		ms_prev = ms_curr-10;
		fine_counter.QuadPart = hi_curr.QuadPart - fine_counter_freq.QuadPart/100;
		launch_tick--;
	} else {
		// standard time update
		DWORD ms_delta = ms_curr - ms_prev;
		if (ms_delta < 10000 && use_fine_counter) {
			if (hi_curr.QuadPart <= fine_counter.QuadPart) {
				if (hi_curr.QuadPart < fine_counter.QuadPart) {
					static bool warn = true;
					if (warn) {
						LOGOUT(">>> WARNING: Inconsistent timer value");
						LOGOUT("    The high-performance timer on this system generates negative time steps.");
						LOGOUT("    Switching to low-resolution timer.");
						warn = false;
					}
					if (pConfig->CfgDebugPrm.TimerMode == 0)
						use_fine_counter = false;
				}
				return false;
			}
			// skip this step if the interval is smaller than the timer resolution
			LONGLONG dt = hi_curr.QuadPart - fine_counter.QuadPart;
			deltat = (double)dt * fine_counter_step;
		} else {
			if (!ms_delta) return false;
			// skip this step if the interval is smaller than the timer resolution
			deltat = ms_delta * 0.001;
		}
	}

	if (deltat < fine_counter_step) {
		// this should never be triggered, given the previous timer checks
		return false; // don't allow zero time step (will cause division by zero everywhere!)
	}

	ms_prev = ms_curr;
	fine_counter = hi_curr;
	td.BeginStep (deltat, running);

	if (!running) return true;
	if (td.WarpChanged()) ApplyWarpFactor();

	return true;
}

void Orbiter::EndTimeStep (bool running)
{
	if (running) {
		g_psys->FinaliseUpdate ();
		//ModulePostStep();
	}

	// Copy frame times from T1 to T0
	td.EndStep (running);

	// Update panels
	g_camera->Update ();                           // camera
	if (g_pane) g_pane->Update (td.SimT1, td.SysT1);

	// Update visual states
	if (gclient) gclient->clbkUpdate (bRunning);
	g_bForceUpdate = false;                        // clear flag

	// check for termination of demo mode
	if (SessionLimitReached())
		if (hRenderWnd) PostMessage(hRenderWnd, WM_CLOSE, 0, 0);
		else CloseSession();
}

bool Orbiter::SessionLimitReached() const
{
	if (pConfig->CfgCmdlinePrm.FrameLimit && td.FrameCount() >= pConfig->CfgCmdlinePrm.FrameLimit)
		return true;
	if (pConfig->CfgCmdlinePrm.MaxSysTime && td.SysT0 >= pConfig->CfgCmdlinePrm.MaxSysTime)
		return true;
	if (pConfig->CfgCmdlinePrm.MaxSimTime && td.SimT0 >= pConfig->CfgCmdlinePrm.MaxSimTime)
		return true;
	if (pConfig->CfgDemoPrm.bDemo && td.SysT0 > pConfig->CfgDemoPrm.MaxDemoTime)
		return true;

	return false;
}

bool Orbiter::Timejump (double _mjd, int pmode)
{
	tjump.mode = pmode;
	tjump.dt = td.JumpTo (_mjd);
	g_psys->Timejump ();
	g_camera->Update ();
	if (g_pane) g_pane->Timejump ();

#ifdef INLINEGRAPHICS
	if (oclient) oclient->clbkTimeJump (td.SimT0, tjump.dt, _mjd);
#endif
	for (DWORD i = 0; i < nmodule; i++)
		module[i].module->clbkTimeJump (td.SimT0, tjump.dt, _mjd);

	return true;
}

void Orbiter::Suspend (void)
{
	ms_suspend = timeGetTime ();
}

void Orbiter::Resume (void)
{
	DWORD dt = timeGetTime() - ms_suspend;
	ms_prev += dt;
	if (use_fine_counter)
		fine_counter.QuadPart += (LONGLONG)dt * (LONGLONG)(1e-3/fine_counter_step);
}

//-----------------------------------------------------------------------------
// Custom command registration
//-----------------------------------------------------------------------------

DWORD Orbiter::RegisterCustomCmd (char *label, char *desc, CustomFunc func, void *context)
{
	DWORD id;
	CUSTOMCMD *tmp = new CUSTOMCMD[ncustomcmd+1]; TRACENEW
	if (ncustomcmd) {
		memcpy (tmp, customcmd, ncustomcmd*sizeof(CUSTOMCMD));
		delete []customcmd;
	}
	customcmd = tmp;

	customcmd[ncustomcmd].label = new char[strlen(label)+1]; TRACENEW
	strcpy (customcmd[ncustomcmd].label, label);
	customcmd[ncustomcmd].func = func;
	customcmd[ncustomcmd].context = context;
	customcmd[ncustomcmd].desc = desc;
	id = customcmd[ncustomcmd].id = g_customcmdid++;
	ncustomcmd++;
	return id;
}

bool Orbiter::UnregisterCustomCmd (int cmdId)
{
	DWORD i;
	CUSTOMCMD *tmp = 0;

	for (i = 0; i < ncustomcmd; i++)
		if (customcmd[i].id == cmdId) break;
	if (i == ncustomcmd) return false;

	if (ncustomcmd > 1) {
		tmp = new CUSTOMCMD[ncustomcmd-1]; TRACENEW
		memcpy (tmp, customcmd, i*sizeof(CUSTOMCMD));
		memcpy (tmp+i, customcmd+i+1, (ncustomcmd-i-1)*sizeof(CUSTOMCMD));
	}
	delete []customcmd;
	customcmd = tmp;
	ncustomcmd--;
	return true;
}

//-----------------------------------------------------------------------------
// Name: ModulePreStep()
// Desc: call module pre-timestep callbacks
//-----------------------------------------------------------------------------
void Orbiter::ModulePreStep ()
{
	DWORD i;
	for (i = 0; i < nmodule; i++)
		module[i].module->clbkPreStep (td.SimT0, td.SimDT, td.MJD0);
	for (i = 0; i < g_psys->nVessel(); i++)
		g_psys->GetVessel(i)->ModulePreStep (td.SimT0, td.SimDT, td.MJD0);
}

//-----------------------------------------------------------------------------
// Name: ModulePostStep()
// Desc: call module post-timestep callbacks
//-----------------------------------------------------------------------------
void Orbiter::ModulePostStep ()
{
	DWORD i;
	for (i = 0; i < g_psys->nVessel(); i++)
		g_psys->GetVessel(i)->ModulePostStep (td.SimT1, td.SimDT, td.MJD1);
	for (i = 0; i < nmodule; i++)
		module[i].module->clbkPostStep (td.SimT1, td.SimDT, td.MJD1);
}

//-----------------------------------------------------------------------------
// Name: UpdateWorld()
// Desc: Update world to current time
//-----------------------------------------------------------------------------
VOID Orbiter::UpdateWorld ()
{
	// module pre-timestep callbacks
	if (bRunning) ModulePreStep ();

	// update world
	g_bStateUpdate = true;
	if (bRunning && td.SimDT) {
		if (bPlayback) FRecorder_Play();
		g_psys->Update (g_bForceUpdate);           // logical objects
	}
	if (pDlgMgr) pDlgMgr->UpdateDialogs(); // SHOULD BE DONE BY GRAPHICS CLIENT!

	// module post-timestep callbacks
	if (bRunning) ModulePostStep ();

	g_bStateUpdate = false;

	if (!KillVessels())  // kill any vessels marked for deletion
		if (hRenderWnd) DestroyWindow (hRenderWnd);

	//g_texmanager->OutputInfo();
}

const char *Orbiter::KeyState() const
{
	return simkstate;
}

//-----------------------------------------------------------------------------
// Name: UserInput()
// Desc: Process user input via DirectInput keyboard and joystick (but not
//       keyboard messages sent via window message queue)
//-----------------------------------------------------------------------------
HRESULT Orbiter::UserInput ()
{
	static char buffer[256];
	DIDEVICEOBJECTDATA dod[10];
	LPDIRECTINPUTDEVICE8 didev;
	DWORD i, dwItems = 10;
	HRESULT hr;
	bool skipkbd = false;

	memset(simkstate, 0, 256);
	for (i = 0; i < 15; i++) ctrlKeyboard[i] = ctrlJoystick[i] = 0; // reset keyboard and joystick attitude requests

	// skip keyboard if dialogs are open
	if ((g_input && g_input->IsActive()) ||
	    (g_select && g_select->IsActive())) skipkbd = true;

	if (didev = GetDInput()->GetKbdDevice()) {
		// keyboard input: immediate key interpretation
		hr = didev->GetDeviceState (sizeof(buffer), &buffer);
		if ((hr == DIERR_NOTACQUIRED || hr == DIERR_INPUTLOST) && SUCCEEDED (didev->Acquire()))
			hr = didev->GetDeviceState (sizeof(buffer), &buffer);
		if (SUCCEEDED (hr))
			for (i = 0; i < 256; i++)
				simkstate[i] |= buffer[i];
		bool consume = BroadcastImmediateKeyboardEvent (simkstate);
		if (!skipkbd && !consume) {
			KbdInputImmediate_System (simkstate);
			if (bRunning) KbdInputImmediate_OnRunning (simkstate);
		}

		// keyboard input: buffered key events
		hr = didev->GetDeviceData (sizeof(DIDEVICEOBJECTDATA), dod, &dwItems, 0);
		if ((hr == DIERR_NOTACQUIRED || hr == DIERR_INPUTLOST) && SUCCEEDED (didev->Acquire()))
			hr = didev->GetDeviceData (sizeof(DIDEVICEOBJECTDATA), dod, &dwItems, 0);
		if (SUCCEEDED (hr)) {
			BroadcastBufferedKeyboardEvent (buffer, dod, dwItems);
			if (!skipkbd) {
				KbdInputBuffered_System (buffer, dod, dwItems);
				if (bRunning) KbdInputBuffered_OnRunning (buffer, dod, dwItems);
			}
		}
		//if (hr == DI_BUFFEROVERFLOW) MessageBeep (-1);
	}

	for (i = 0; i < 15; i++) ctrlTotal[i] = ctrlKeyboard[i]; // update attitude requests

	// joystick input
	DIJOYSTATE2 js;
	if (pDI->PollJoystick (&js)) {
		UserJoyInput_System (&js);                  // general joystick functions
		if (bRunning) UserJoyInput_OnRunning (&js); // joystick vessel control functions
		for (i = 0; i < 15; i++) ctrlTotal[i] += ctrlJoystick[i]; // update thrust requests
	}

	g_camera->UpdateMouse();

	// apply manual attitude control
	g_focusobj->ApplyUserAttitudeControls (ctrlTotal);

	return S_OK;
}

//-----------------------------------------------------------------------------
// Name: SendKbdBuffered()
// Desc: Simulate a buffered keyboard event
//-----------------------------------------------------------------------------

bool Orbiter::SendKbdBuffered(DWORD key, DWORD *mod, DWORD nmod, bool onRunningOnly)
{
	if (onRunningOnly && !bRunning) return false;

	DIDEVICEOBJECTDATA dod;
	dod.dwData = 0x80;
	dod.dwOfs = key;
	char buffer[256];
	memset (buffer, 0, 256);
	for (int i = 0; i < nmod; i++)
		buffer[mod[i]] = 0x80;
	BroadcastBufferedKeyboardEvent (buffer, &dod, 1);
	KbdInputBuffered_System (buffer, &dod, 1);
	KbdInputBuffered_OnRunning (buffer, &dod, 1);
	return true;
}

//-----------------------------------------------------------------------------
// Name: SendKbdImmediate()
// Desc: Simulate an immediate key state
//-----------------------------------------------------------------------------

bool Orbiter::SendKbdImmediate(char kstate[256], bool onRunningOnly)
{
	if (onRunningOnly && !bRunning) return false;
	for (int i = 0; i < 256; i++)
		simkstate[i] |= kstate[i];
	bAllowInput = true; // make sure the render window processes inputs
	return true;
}

//-----------------------------------------------------------------------------
// Name: KbdInputImmediate_System ()
// Desc: General user keyboard immediate key interpretation. Processes keys
//       which are also interpreted when simulation is paused (movably)
//-----------------------------------------------------------------------------
void Orbiter::KbdInputImmediate_System (char *kstate)
{
	bool smooth_cam = true; // make user-selectable

	const double cam_acc = 0.02;
	double cam_vmax = td.SysDT * 1.0;
	double max_dv = cam_vmax*cam_acc;

	static double dphi = 0.0, dtht = 0.0;
	static double dphi_gm = 0.0, dtht_gm = 0.0;
	if (g_camera->IsExternal()) { // external camera view
		// rotate external camera horizontally (track mode)
		if      (keymap.IsLogicalKey (kstate, OAPI_LKEY_TrackRotateLeft))  dphi = (smooth_cam ? max (-cam_vmax, dphi-max_dv) : -cam_vmax);
		else if (keymap.IsLogicalKey (kstate, OAPI_LKEY_TrackRotateRight)) dphi = (smooth_cam ? min ( cam_vmax, dphi+max_dv) :  cam_vmax);
		else if (dphi) {
			if (smooth_cam) {
				if (dphi < 0.0) dphi = min (0.0, dphi+max_dv);
				else            dphi = max (0.0, dphi-max_dv);
			} else dphi = 0.0;
		}
		if (dphi) g_camera->ShiftPhi (dphi);

		// rotate external camera vertically (track mode)
		if      (keymap.IsLogicalKey (kstate, OAPI_LKEY_TrackRotateUp))   dtht = (smooth_cam ? max (-cam_vmax, dtht-max_dv) : -cam_vmax);
		else if (keymap.IsLogicalKey (kstate, OAPI_LKEY_TrackRotateDown)) dtht = (smooth_cam ? min ( cam_vmax, dtht+max_dv) :  cam_vmax);
		else if (dtht) {
			if (smooth_cam) {
				if (dtht < 0.0) dtht = min (0.0, dtht+max_dv);
				else            dtht = max (0.0, dtht-max_dv);
			} else dtht = 0.0;
		}
		if (dtht) g_camera->ShiftTheta (dtht);

		// rotate external camera horizontally (ground mode)
		if      (keymap.IsLogicalKey (kstate, OAPI_LKEY_GroundTiltLeft))  dphi_gm = (smooth_cam ? max (-cam_vmax, dphi_gm-max_dv) : -cam_vmax);
		else if (keymap.IsLogicalKey (kstate, OAPI_LKEY_GroundTiltRight)) dphi_gm = (smooth_cam ? min ( cam_vmax, dphi_gm+max_dv) :  cam_vmax);
		else if (dphi_gm) {
			if (smooth_cam) {
				if (dphi_gm < 0.0) dphi_gm = min (0.0, dphi_gm+max_dv);
				else               dphi_gm = max (0.0, dphi_gm-max_dv);
			} else dphi_gm = 0.0;
		}

		// rotate external camera vertically (ground mode)
		if      (keymap.IsLogicalKey (kstate, OAPI_LKEY_GroundTiltUp))   dtht_gm = (smooth_cam ? max (-cam_vmax, dtht_gm-max_dv) : -cam_vmax);
		else if (keymap.IsLogicalKey (kstate, OAPI_LKEY_GroundTiltDown)) dtht_gm = (smooth_cam ? min ( cam_vmax, dtht_gm+max_dv) :  cam_vmax);
		else if (dtht_gm) {
			if (smooth_cam) {
				if (dtht_gm < 0.0) dtht_gm = min (0.0, dtht_gm+max_dv);
				else               dtht_gm = max (0.0, dtht_gm-max_dv);
			} else dtht_gm = 0.0;
		}
		if (dphi_gm || dtht_gm) g_camera->Rotate (0-dphi_gm, -dtht_gm);

	} else {                        // internal camera view
		if      (keymap.IsLogicalKey (kstate, OAPI_LKEY_CockpitRotateLeft))  dphi = (smooth_cam ? max (-cam_vmax, dphi-max_dv) : -cam_vmax);
		else if (keymap.IsLogicalKey (kstate, OAPI_LKEY_CockpitRotateRight)) dphi = (smooth_cam ? min ( cam_vmax, dphi+max_dv) :  cam_vmax);
		else if (dphi) {
			if (smooth_cam) {
				if (dphi < 0.0) dphi = min (0.0, dphi+max_dv);
				else            dphi = max (0.0, dphi-max_dv);
			} else dphi = 0.0;
		}

		if      (keymap.IsLogicalKey (kstate, OAPI_LKEY_CockpitRotateUp))   dtht = (smooth_cam ? max (-cam_vmax, dtht-max_dv) : -cam_vmax);
		else if (keymap.IsLogicalKey (kstate, OAPI_LKEY_CockpitRotateDown)) dtht = (smooth_cam ? min ( cam_vmax, dtht+max_dv) :  cam_vmax);
		else if (dtht) {
			if (smooth_cam) {
				if (dtht < 0.0) dtht = min (0.0, dtht+max_dv);
				else            dtht = max (0.0, dtht-max_dv);
			} else dtht = 0.0;
		}
		if (dphi || dtht) g_camera->Rotate (-dphi, -dtht, true);
	}


	if (g_camera->IsExternal()) {   // external camera view
		// rotate external camera (track mode)
		//if (keymap.IsLogicalKey (kstate, OAPI_LKEY_TrackRotateLeft))    g_camera->ShiftPhi   (-td.SysDT);
		//if (keymap.IsLogicalKey (kstate, OAPI_LKEY_TrackRotateRight))   g_camera->ShiftPhi   ( td.SysDT);
		//if (keymap.IsLogicalKey (kstate, OAPI_LKEY_TrackRotateUp))      g_camera->ShiftTheta (-td.SysDT);
		//if (keymap.IsLogicalKey (kstate, OAPI_LKEY_TrackRotateDown))    g_camera->ShiftTheta ( td.SysDT);
		// move external camera in/out
		if (keymap.IsLogicalKey (kstate, OAPI_LKEY_TrackAdvance))       g_camera->ShiftDist (-td.SysDT);
		if (keymap.IsLogicalKey (kstate, OAPI_LKEY_TrackRetreat))       g_camera->ShiftDist ( td.SysDT);
		// tilt ground observer camera
		//if (keymap.IsLogicalKey (kstate, OAPI_LKEY_GroundTiltLeft))     g_camera->Rotate ( td.SysDT,  0);
		//if (keymap.IsLogicalKey (kstate, OAPI_LKEY_GroundTiltRight))    g_camera->Rotate (-td.SysDT,  0);
		//if (keymap.IsLogicalKey (kstate, OAPI_LKEY_GroundTiltUp))       g_camera->Rotate ( 0,  td.SysDT);
		//if (keymap.IsLogicalKey (kstate, OAPI_LKEY_GroundTiltDown))     g_camera->Rotate ( 0, -td.SysDT);
	} else {                        // internal camera view
		// rotate cockpit camera
		//if (keymap.IsLogicalKey (kstate, OAPI_LKEY_CockpitRotateLeft))  g_camera->Rotate ( td.SysDT,  0, true);
		//if (keymap.IsLogicalKey (kstate, OAPI_LKEY_CockpitRotateRight)) g_camera->Rotate (-td.SysDT,  0, true);
		//if (keymap.IsLogicalKey (kstate, OAPI_LKEY_CockpitRotateUp))    g_camera->Rotate ( 0,  td.SysDT, true);
		//if (keymap.IsLogicalKey (kstate, OAPI_LKEY_CockpitRotateDown))  g_camera->Rotate ( 0, -td.SysDT, true);
		// shift 2-D panels
		if (keymap.IsLogicalKey (kstate, OAPI_LKEY_PanelShiftLeft))     g_pane->ShiftPanel ( td.SysDT*pConfig->CfgLogicPrm.PanelScrollSpeed, 0.0);
		if (keymap.IsLogicalKey (kstate, OAPI_LKEY_PanelShiftRight))    g_pane->ShiftPanel (-td.SysDT*pConfig->CfgLogicPrm.PanelScrollSpeed, 0.0);
		if (keymap.IsLogicalKey (kstate, OAPI_LKEY_PanelShiftUp))       g_pane->ShiftPanel (0.0,  td.SysDT*pConfig->CfgLogicPrm.PanelScrollSpeed);
		if (keymap.IsLogicalKey (kstate, OAPI_LKEY_PanelShiftDown))     g_pane->ShiftPanel (0.0, -td.SysDT*pConfig->CfgLogicPrm.PanelScrollSpeed);
	}
	if (keymap.IsLogicalKey (kstate, OAPI_LKEY_IncFOV)) IncFOV ( 0.4*g_camera->Aperture()*td.SysDT);
	if (keymap.IsLogicalKey (kstate, OAPI_LKEY_DecFOV)) IncFOV (-0.4*g_camera->Aperture()*td.SysDT);
}

//-----------------------------------------------------------------------------
// Name: KbdInputImmediate_OnRunning ()
// Desc: User keyboard input query for running simulation (ship controls etc.)
//-----------------------------------------------------------------------------
void Orbiter::KbdInputImmediate_OnRunning (char *kstate)
{
	if (g_focusobj->ConsumeDirectKey (kstate)) return;  // key is consumed by focus vessel

	// main/retro/hover thruster settings
	if (keymap.IsLogicalKey (kstate, OAPI_LKEY_IncMainThrust))   g_focusobj->IncMainRetroLevel ( 0.2*td.SimDT);
	if (keymap.IsLogicalKey (kstate, OAPI_LKEY_DecMainThrust))   g_focusobj->IncMainRetroLevel (-0.2*td.SimDT);
	if (keymap.IsLogicalKey (kstate, OAPI_LKEY_KillMainRetro)) { g_focusobj->SetThrusterGroupLevel (THGROUP_MAIN, 0.0);
		                                                         g_focusobj->SetThrusterGroupLevel (THGROUP_RETRO, 0.0); }
	if (keymap.IsLogicalKey (kstate, OAPI_LKEY_FullMainThrust))  g_focusobj->OverrideMainLevel ( 1.0);
	if (keymap.IsLogicalKey (kstate, OAPI_LKEY_FullRetroThrust)) g_focusobj->OverrideMainLevel (-1.0);
	if (keymap.IsLogicalKey (kstate, OAPI_LKEY_IncHoverThrust))  g_focusobj->IncThrusterGroupLevel (THGROUP_HOVER,  0.2*td.SimDT);
	if (keymap.IsLogicalKey (kstate, OAPI_LKEY_DecHoverThrust))  g_focusobj->IncThrusterGroupLevel (THGROUP_HOVER, -0.2*td.SimDT);

	// Reaction control system
	if (bEnableAtt) {
		// rotational mode
		if      (keymap.IsLogicalKey (kstate, OAPI_LKEY_RCSPitchUp))     ctrlKeyboard[THGROUP_ATT_PITCHUP]   = 1000;
		else if (keymap.IsLogicalKey (kstate, OAPI_LKEY_LPRCSPitchUp))   ctrlKeyboard[THGROUP_ATT_PITCHUP]   =  100;
		if      (keymap.IsLogicalKey (kstate, OAPI_LKEY_RCSPitchDown))   ctrlKeyboard[THGROUP_ATT_PITCHDOWN] = 1000;
		else if (keymap.IsLogicalKey (kstate, OAPI_LKEY_LPRCSPitchDown)) ctrlKeyboard[THGROUP_ATT_PITCHDOWN] =  100;
		if      (keymap.IsLogicalKey (kstate, OAPI_LKEY_RCSYawLeft))     ctrlKeyboard[THGROUP_ATT_YAWLEFT]   = 1000;
		else if (keymap.IsLogicalKey (kstate, OAPI_LKEY_LPRCSYawLeft))   ctrlKeyboard[THGROUP_ATT_YAWLEFT]   =  100;
		if      (keymap.IsLogicalKey (kstate, OAPI_LKEY_RCSYawRight))    ctrlKeyboard[THGROUP_ATT_YAWRIGHT]  = 1000;
		else if (keymap.IsLogicalKey (kstate, OAPI_LKEY_LPRCSYawRight))  ctrlKeyboard[THGROUP_ATT_YAWRIGHT]  =  100;
		if      (keymap.IsLogicalKey (kstate, OAPI_LKEY_RCSBankLeft))    ctrlKeyboard[THGROUP_ATT_BANKLEFT]  = 1000;
		else if (keymap.IsLogicalKey (kstate, OAPI_LKEY_LPRCSBankLeft))  ctrlKeyboard[THGROUP_ATT_BANKLEFT]  =  100;
		if      (keymap.IsLogicalKey (kstate, OAPI_LKEY_RCSBankRight))   ctrlKeyboard[THGROUP_ATT_BANKRIGHT] = 1000;
		else if (keymap.IsLogicalKey (kstate, OAPI_LKEY_LPRCSBankRight)) ctrlKeyboard[THGROUP_ATT_BANKRIGHT] =  100;
		// linear mode
		if      (keymap.IsLogicalKey (kstate, OAPI_LKEY_RCSUp))          ctrlKeyboard[THGROUP_ATT_UP]        = 1000;
		else if (keymap.IsLogicalKey (kstate, OAPI_LKEY_LPRCSUp))        ctrlKeyboard[THGROUP_ATT_UP]        =  100;
		if      (keymap.IsLogicalKey (kstate, OAPI_LKEY_RCSDown))        ctrlKeyboard[THGROUP_ATT_DOWN]      = 1000;
		else if (keymap.IsLogicalKey (kstate, OAPI_LKEY_LPRCSDown))      ctrlKeyboard[THGROUP_ATT_DOWN]      =  100;
		if      (keymap.IsLogicalKey (kstate, OAPI_LKEY_RCSLeft))        ctrlKeyboard[THGROUP_ATT_LEFT]      = 1000;
		else if (keymap.IsLogicalKey (kstate, OAPI_LKEY_LPRCSLeft))      ctrlKeyboard[THGROUP_ATT_LEFT]      =  100;
		if      (keymap.IsLogicalKey (kstate, OAPI_LKEY_RCSRight))       ctrlKeyboard[THGROUP_ATT_RIGHT]     = 1000;
		else if (keymap.IsLogicalKey (kstate, OAPI_LKEY_LPRCSRight))     ctrlKeyboard[THGROUP_ATT_RIGHT]     =  100;
		if      (keymap.IsLogicalKey (kstate, OAPI_LKEY_RCSForward))     ctrlKeyboard[THGROUP_ATT_FORWARD]   = 1000;
		else if (keymap.IsLogicalKey (kstate, OAPI_LKEY_LPRCSForward))   ctrlKeyboard[THGROUP_ATT_FORWARD]   =  100;
		if      (keymap.IsLogicalKey (kstate, OAPI_LKEY_RCSBack))        ctrlKeyboard[THGROUP_ATT_BACK]      = 1000;
		else if (keymap.IsLogicalKey (kstate, OAPI_LKEY_LPRCSBack))      ctrlKeyboard[THGROUP_ATT_BACK]      =  100;
	}

	// Elevator trim control
	if (keymap.IsLogicalKey (kstate, OAPI_LKEY_IncElevatorTrim)) g_focusobj->IncTrim (AIRCTRL_ELEVATORTRIM);
	if (keymap.IsLogicalKey (kstate, OAPI_LKEY_DecElevatorTrim)) g_focusobj->DecTrim (AIRCTRL_ELEVATORTRIM);

	// Wheel brake control
	if (keymap.IsLogicalKey (kstate, OAPI_LKEY_WheelbrakeLeft))  g_focusobj->SetWBrakeLevel (1.0, 1, false);
	if (keymap.IsLogicalKey (kstate, OAPI_LKEY_WheelbrakeRight)) g_focusobj->SetWBrakeLevel (1.0, 2, false);

	// left/right MFD control
	if (KEYMOD_SHIFT (kstate)) {
		if (KEYMOD_LSHIFT (kstate) && g_pane->MFD(0)) g_pane->MFD(0)->ConsumeKeyImmediate (kstate);
		if (KEYMOD_RSHIFT (kstate) && g_pane->MFD(1)) g_pane->MFD(1)->ConsumeKeyImmediate (kstate);
	}
}

//-----------------------------------------------------------------------------
// Name: KbdInputBuffered_System ()
// Desc: General user keyboard buffered key interpretation. Processes keys
//       which are also interpreted when simulation is paused
//-----------------------------------------------------------------------------
void Orbiter::KbdInputBuffered_System (char *kstate, DIDEVICEOBJECTDATA *dod, DWORD n)
{
	for (DWORD i = 0; i < n; i++) {

		if (!(dod[i].dwData & 0x80)) continue; // only process key down events
		DWORD key = dod[i].dwOfs;

		if      (keymap.IsLogicalKey (key, kstate, OAPI_LKEY_Pause))                TogglePause();
		else if (keymap.IsLogicalKey (key, kstate, OAPI_LKEY_Quicksave))            Quicksave();
		else if (keymap.IsLogicalKey (key, kstate, OAPI_LKEY_StepIncFOV))           SetFOV (ceil((g_camera->Aperture()*DEG+1e-6)/5.0)*5.0*RAD);
		else if (keymap.IsLogicalKey (key, kstate, OAPI_LKEY_StepDecFOV))           SetFOV (floor((g_camera->Aperture()*DEG-1e-6)/5.0)*5.0*RAD);
		else if (keymap.IsLogicalKey (key, kstate, OAPI_LKEY_MainMenu))             { if (g_pane->MIBar()) g_pane->MIBar()->ToggleAutohide(); }
		else if (keymap.IsLogicalKey (key, kstate, OAPI_LKEY_DlgHelp))              pDlgMgr->EnsureEntry<DlgHelp> ();
		else if (keymap.IsLogicalKey (key, kstate, OAPI_LKEY_DlgCamera))            pDlgMgr->EnsureEntry<DlgCamera> ();
		else if (keymap.IsLogicalKey (key, kstate, OAPI_LKEY_DlgSimspeed))          pDlgMgr->EnsureEntry<DlgTacc> ();
		else if (keymap.IsLogicalKey (key, kstate, OAPI_LKEY_DlgCustomCmd))         pDlgMgr->EnsureEntry<DlgFunction> ();
		else if (keymap.IsLogicalKey (key, kstate, OAPI_LKEY_DlgInfo))              pDlgMgr->EnsureEntry<DlgInfo> ();
		else if (keymap.IsLogicalKey (key, kstate, OAPI_LKEY_DlgMap))               pDlgMgr->EnsureEntry<DlgMap> ();
		//else if (keymap.IsLogicalKey (key, kstate, OAPI_LKEY_DlgNavaid))            OpenDialogEx (IDD_NAVAID, (DLGPROC)Navaid_DlgProc, DLG_CAPTIONCLOSE);
		else if (keymap.IsLogicalKey (key, kstate, OAPI_LKEY_DlgRecorder))          pDlgMgr->EnsureEntry<DlgRecorder> ();
		else if (keymap.IsLogicalKey (key, kstate, OAPI_LKEY_ToggleCamInternal))    SetView (g_focusobj, !g_camera->IsExternal());
		else if (keymap.IsLogicalKey (key, kstate, OAPI_LKEY_DlgVisHelper))         pDlgMgr->EnsureEntry<DlgVishelper> ();
		else if (keymap.IsLogicalKey (key, kstate, OAPI_LKEY_DlgCapture))           pDlgMgr->EnsureEntry<DlgCapture> ();
		else if (keymap.IsLogicalKey (key, kstate, OAPI_LKEY_DlgSelectVessel))      pDlgMgr->EnsureEntry<DlgFocus> ();
		else if (keymap.IsLogicalKey (key, kstate, OAPI_LKEY_TogglePlanetarium)) {
			pConfig->TogglePlanetarium();
			DlgVishelper *dlg = pDlgMgr->EntryExists<DlgVishelper> (hInst);
			if (dlg) dlg->Update();
			g_psys->ActivatePlanetLabels(Cfg()->PlanetariumItem(IDC_PLANETARIUM));
		} else if (keymap.IsLogicalKey (key, kstate, OAPI_LKEY_ToggleRecPlay)) {
			if (bPlayback) EndPlayback();
			else ToggleRecorder ();
		} else if (keymap.IsLogicalKey (key, kstate, OAPI_LKEY_Quit)) {
			if (hRenderWnd) PostMessage (hRenderWnd, WM_CLOSE, 0, 0);
		} else if (keymap.IsLogicalKey (key, kstate, OAPI_LKEY_SelectPrevVessel)) {
			if (g_pfocusobj) SetFocusObject (g_pfocusobj);
		}

		if (g_camera->IsInternal()) {
			if      (keymap.IsLogicalKey (key, kstate, OAPI_LKEY_CockpitResetCam))  g_camera->ResetCockpitDir();
			else if (keymap.IsLogicalKey (key, kstate, OAPI_LKEY_TogglePanelMode))  g_pane->TogglePanelMode();
			else if (keymap.IsLogicalKey (key, kstate, OAPI_LKEY_PanelSwitchLeft))  g_pane->SwitchPanel (0);
			else if (keymap.IsLogicalKey (key, kstate, OAPI_LKEY_PanelSwitchRight)) g_pane->SwitchPanel (1);
			else if (keymap.IsLogicalKey (key, kstate, OAPI_LKEY_PanelSwitchUp))    g_pane->SwitchPanel (2);
			else if (keymap.IsLogicalKey (key, kstate, OAPI_LKEY_PanelSwitchDown))  g_pane->SwitchPanel (3);

			else if (keymap.IsLogicalKey (key, kstate, OAPI_LKEY_CockpitDontLean))    g_focusobj->LeanCamera (0); // g_camera->MoveTo (Vector(0,0,0)), g_camera->ResetCockpitDir();
			else if (keymap.IsLogicalKey (key, kstate, OAPI_LKEY_CockpitLeanForward)) g_focusobj->LeanCamera (1); // g_camera->MoveTo (g_focusobj->camdr_fwd), g_camera->ResetCockpitDir();
			else if (keymap.IsLogicalKey (key, kstate, OAPI_LKEY_CockpitLeanLeft))    g_focusobj->LeanCamera (2); // g_camera->MoveTo (g_focusobj->camdr_left), g_camera->ResetCockpitDir(60*RAD, g_camera->ctheta0);
			else if (keymap.IsLogicalKey (key, kstate, OAPI_LKEY_CockpitLeanRight))   g_focusobj->LeanCamera (3); // g_camera->MoveTo (g_focusobj->camdr_right), g_camera->ResetCockpitDir(-60*RAD, g_camera->ctheta0);

			else if (keymap.IsLogicalKey (key, kstate, OAPI_LKEY_HUDColour))        g_pane->ToggleHUDColour();
		} else {
			if (keymap.IsLogicalKey (key, kstate, OAPI_LKEY_ToggleTrackMode))  g_camera->SetTrackMode ((ExtCamMode)(((int)g_camera->GetExtMode()+1)%3));
		}
	}
}

//-----------------------------------------------------------------------------
// Name: KbdInputBuffered_OnRunning ()
// Desc: User keyboard buffered key interpretation in running simulation
//-----------------------------------------------------------------------------
void Orbiter::KbdInputBuffered_OnRunning (char *kstate, DIDEVICEOBJECTDATA *dod, DWORD n)
{
	for (DWORD i = 0; i < n; i++) {

		DWORD key = dod[i].dwOfs;
		bool bdown = (dod[i].dwData & 0x80) != 0;

		if (g_focusobj->ConsumeBufferedKey (key, bdown, kstate)) // offer key to vessel for processing
			continue;
		if (!bdown) // only process key down events
			continue;
		if (key == DIK_LSHIFT || key == DIK_RSHIFT) continue;    // we don't process modifier keys

		// simulation speed control
		if      (keymap.IsLogicalKey (key, kstate, OAPI_LKEY_IncSimSpeed)) IncWarpFactor ();
		else if (keymap.IsLogicalKey (key, kstate, OAPI_LKEY_DecSimSpeed)) DecWarpFactor ();

		if (KEYMOD_CONTROL (kstate)) {    // CTRL-Key combinations

			//switch (key) {
			//case DIK_F3:    // switch focus to previous vessel
			//	if (g_pfocusobj) SetFocusObject (g_pfocusobj);
			//	break;
			//}

		} else if (KEYMOD_SHIFT (kstate)) {  // Shift-key combinations (reserved for MFD control)

			int id = (KEYDOWN (kstate, DIK_LSHIFT) ? 0 : 1);
			g_pane->MFDConsumeKeyBuffered (id, key);

		} else if (KEYMOD_ALT (kstate)) {    // ALT-Key combinations

		} else { // unmodified keys

			//switch (key) {
			//case DIK_F3:       // switch vessel
			//	OpenDialogEx (IDD_JUMPVESSEL, (DLGPROC)SelVessel_DlgProc, DLG_CAPTIONCLOSE | DLG_CAPTIONHELP);
			//	break;
			//}
		}
	}
}

//-----------------------------------------------------------------------------
// Name: UserJoyInput_System ()
// Desc: General user joystick input (also functional when paused)
//-----------------------------------------------------------------------------
void Orbiter::UserJoyInput_System (DIJOYSTATE2 *js)
{
	if (LOWORD (js->rgdwPOV[0]) != 0xFFFF) {
		DWORD dir = js->rgdwPOV[0];
		if (g_camera->IsExternal()) {  // use the joystick's coolie hat to rotate external camera
			if (js->rgbButtons[2]) { // shift instrument panel
				if      (dir <  5000 || dir > 31000) g_camera->Rotate (0,  td.SysDT);
				else if (dir > 13000 && dir < 23000) g_camera->Rotate (0, -td.SysDT);
				if      (dir >  4000 && dir < 14000) g_camera->Rotate (-td.SysDT, 0);
				else if (dir > 22000 && dir < 32000) g_camera->Rotate ( td.SysDT, 0);
			} else {
				if      (dir <  5000 || dir > 31000) g_camera->AddTheta (-td.SysDT);
				else if (dir > 13000 && dir < 23000) g_camera->AddTheta ( td.SysDT);
				if      (dir >  4000 && dir < 14000) g_camera->AddPhi   ( td.SysDT);
				else if (dir > 22000 && dir < 32000) g_camera->AddPhi   (-td.SysDT);
			}
		} else { // internal view
			if (js->rgbButtons[2]) { // shift instrument panel
				if      (dir <  5000 || dir > 31000) g_pane->ShiftPanel (0.0,  td.SysDT*pConfig->CfgLogicPrm.PanelScrollSpeed);
				else if (dir > 13000 && dir < 23000) g_pane->ShiftPanel (0.0, -td.SysDT*pConfig->CfgLogicPrm.PanelScrollSpeed);
				if      (dir >  4000 && dir < 14000) g_pane->ShiftPanel (-td.SysDT*pConfig->CfgLogicPrm.PanelScrollSpeed, 0.0);
				else if (dir > 22000 && dir < 32000) g_pane->ShiftPanel ( td.SysDT*pConfig->CfgLogicPrm.PanelScrollSpeed, 0.0);
			} else {                 // rotate camera
				if      (dir <  5000 || dir > 31000) g_camera->Rotate (0,  td.SysDT, true);
				else if (dir > 13000 && dir < 23000) g_camera->Rotate (0, -td.SysDT, true);
				if      (dir >  4000 && dir < 14000) g_camera->Rotate (-td.SysDT, 0, true);
				else if (dir > 22000 && dir < 32000) g_camera->Rotate ( td.SysDT, 0, true);
			}
		}
	}
}

//-----------------------------------------------------------------------------
// Name: UserJoyInput_OnRunning ()
// Desc: User joystick input query for running simulation (ship controls etc.)
//-----------------------------------------------------------------------------
void Orbiter::UserJoyInput_OnRunning (DIJOYSTATE2 *js)
{
	if (bEnableAtt) {
		if (js->lX) {
			if (js->rgbButtons[2]) { // emulate rudder control
				if (js->lX > 0) ctrlJoystick[THGROUP_ATT_YAWRIGHT] =   js->lX;
				else            ctrlJoystick[THGROUP_ATT_YAWLEFT]  =  -js->lX;
			} else {                 // rotation (bank)
				if (js->lX > 0) ctrlJoystick[THGROUP_ATT_BANKRIGHT] =  js->lX;
				else            ctrlJoystick[THGROUP_ATT_BANKLEFT]  = -js->lX;
			}
		}
		if (js->lY) {                // rotation (pitch) or translation (vertical)
			if (js->lY > 0) ctrlJoystick[THGROUP_ATT_PITCHUP]   = ctrlJoystick[THGROUP_ATT_UP]    =  js->lY;
			else            ctrlJoystick[THGROUP_ATT_PITCHDOWN] = ctrlJoystick[THGROUP_ATT_DOWN]  = -js->lY;
		}
		if (js->lRz) {               // rotation (yaw) or translation (transversal)
			if (js->lRz > 0) ctrlJoystick[THGROUP_ATT_YAWRIGHT] = ctrlJoystick[THGROUP_ATT_RIGHT] =  js->lRz;
			else             ctrlJoystick[THGROUP_ATT_YAWLEFT]  = ctrlJoystick[THGROUP_ATT_LEFT]  = -js->lRz;
		}
	}

	if (pDI->joyprop.bThrottle) { // main thrusters via throttle control
		long lZ4 = *(long*)(((BYTE*)js)+pDI->joyprop.ThrottleOfs) >> 3;
		if (lZ4 != plZ4) {
			if (ignorefirst) {
				if (abs(lZ4-plZ4) > 10) ignorefirst = false;
				else return;
			}
			double th = -0.008 * (plZ4 = lZ4);
			if (th > 1.0) th = 1.0;
			g_focusobj->SetThrusterGroupLevel (THGROUP_MAIN, th);
			g_focusobj->SetThrusterGroupLevel (THGROUP_RETRO, 0.0);
		}
	}
}

bool Orbiter::MouseEvent (UINT event, DWORD state, DWORD x, DWORD y)
{
	if (g_pane->MIBar() && g_pane->MIBar()->ProcessMouse (event, state, x, y)) return true;
	if (BroadcastMouseEvent (event, state, x, y)) return true;
	if (event == WM_MOUSEMOVE) return false; // may be lifted later

	if (bRunning) {
		if (event == WM_LBUTTONDOWN || event == WM_RBUTTONDOWN) {
			if (g_input && g_input->IsActive()) g_input->Close();
			if (g_select && g_select->IsActive()) g_select->Clear (true);
		}
		if (g_pane->ProcessMouse_OnRunning (event, state, x, y, simkstate)) return true;
	}
	if (g_pane->ProcessMouse_System(event, state, x, y, simkstate)) return true;
	if (g_camera->ProcessMouse (event, state, x, y, simkstate)) return true;
	return false;
}

bool Orbiter::BroadcastMouseEvent (UINT event, DWORD state, DWORD x, DWORD y)
{
	bool consume = false;
	for (DWORD k = 0; k < nmodule; k++) {
		if (module[k].module->clbkProcessMouse (event, state, x, y))
			consume = true;
	}
	return consume;
}

bool Orbiter::BroadcastImmediateKeyboardEvent (char *kstate)
{
	bool consume = false;
	for (DWORD k = 0; k < nmodule; k++)
		if (module[k].module->clbkProcessKeyboardImmediate (kstate, bRunning))
			consume = true;
	return consume;
}

void Orbiter::BroadcastBufferedKeyboardEvent (char *kstate, DIDEVICEOBJECTDATA *dod, DWORD n)
{
	for (DWORD i = 0; i < n; i++) {
		bool consume = false;
		if (!(dod[i].dwData & 0x80)) continue; // only process key down events
		DWORD key = dod[i].dwOfs;

		for (DWORD k = 0; k < nmodule; k++) {
			if (module[k].module->clbkProcessKeyboardBuffered (key, kstate, bRunning))
				consume = true;
		}
		if (consume) dod[i].dwData = 0; // remove key from process queue
	}
}

//-----------------------------------------------------------------------------
// Name: MsgProc()
// Desc: Render window message handler
//-----------------------------------------------------------------------------

LRESULT Orbiter::MsgProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	WORD kmod;

	switch (uMsg) {

	case WM_ACTIVATE:
		bActive = (wParam != WA_INACTIVE);
		return 0;

	case WM_CHAR:
		// make dialogs modal to avoid complications
		if (g_input && g_input->IsActive()) {
			if (g_input->ConsumeKey (uMsg, wParam) != Select::key_ignore) bRenderOnce = TRUE;
			return 0;
		}
		if (g_select && g_select->IsActive()) {
			if (g_select->ConsumeKey (uMsg, wParam) != Select::key_ignore) bRenderOnce = TRUE;
			return 0;
		}
		break;

	// *** User Keyboard Input ***
	case WM_KEYDOWN:

		// modifiers
		kmod = 0;
		if (GetKeyState (VK_SHIFT)   & 0x8000) kmod |= 0x01;
		if (GetKeyState (VK_CONTROL) & 0x8000) kmod |= 0x02;

		// make dialogs modal to avoid complications
		if (g_input && g_input->IsActive()) {
			if (g_input->ConsumeKey (uMsg, wParam, kmod) != Select::key_ignore) bRenderOnce = TRUE;
			return 0;
		}
		if (g_select && g_select->IsActive()) {
			if (g_select->ConsumeKey (uMsg, wParam, kmod) != Select::key_ignore) bRenderOnce = TRUE;
			return 0;
		}
		break;

	// Mouse event handler
	case WM_LBUTTONDOWN:
	case WM_RBUTTONDOWN:
	case WM_LBUTTONUP:
	case WM_RBUTTONUP: {
		if (MouseEvent(uMsg, wParam, LOWORD(lParam), HIWORD(lParam)))
			break; //return 0;
		} break;
	case WM_MOUSEWHEEL: {
		int x = LOWORD(lParam);
		int y = HIWORD(lParam);
		if (!bFullscreen) {
			POINT pt = { x, y };
			ScreenToClient(&pt); // for some reason this message passes screen coordinates
			x = pt.x;
			y = pt.y;
		}
		if (MouseEvent(uMsg, wParam, x, y))
			break; //return 0;
		} break;
	case WM_MOUSEMOVE: {
		int x = LOWORD(lParam);
		int y = HIWORD(lParam);
		MouseEvent(uMsg, wParam, x, y);
		if (!bKeepFocus && pConfig->CfgUIPrm.bFocusFollowsMouse && GetFocus() != hWnd) {
			if (GetWindowThreadProcessId(hWnd, NULL) == GetWindowThreadProcessId(GetFocus(), NULL))
				SetFocus(hWnd);
		}
	    }return 0;

#ifdef NETCONNECT
		// Asynchronous network support
	case WM_CONN_EVENT:
		switch LOWORD(lParam) {
		case FD_ACCEPT:
			if (NetServer) {
				NetServer->Accept();
				SendScenario (NetServer, "Sent scenario");
			}
			break;
		case FD_READ:
			if (NetConn) CheckRequests (NetConn);
			break;
		}
		break;
#endif // NETCONNECT

#ifdef UNDEF
		// These messages could be intercepted to suspend the simulation
		// during resizing and menu operations. Not a good idea for real-time
		// applications though
    case WM_ENTERMENULOOP:  // Pause the app when menus are displayed
        Pause (TRUE);
        break;

    case WM_EXITMENULOOP:   // Resume when menu is closed
        Pause (FALSE);
        break;

    case WM_ENTERSIZEMOVE:  // Pause during resizing or moving
        if (m_bRunning) Suspend ();
        break;

    case WM_EXITSIZEMOVE:   // Resume after resizing or moving
        if (m_bRunning) Resume ();
        break;
#endif

    case WM_GETMINMAXINFO:
        ((MINMAXINFO*)lParam)->ptMinTrackSize.x = 100;
        ((MINMAXINFO*)lParam)->ptMinTrackSize.y = 100;
        break;

    case WM_POWERBROADCAST:
        switch (wParam) {
        case PBT_APMQUERYSUSPEND:
            // At this point, the app should save any data for open
            // network connections, files, etc.., and prepare to go into
            // a suspended mode.
			Freeze (true);
			return TRUE;

        case PBT_APMRESUMESUSPEND:
            // At this point, the app should recover any data, network
            // connections, files, etc.., and resume running from when
            // the app was suspended.
			Freeze (false);
			return TRUE;
        }
        break;

    case WM_COMMAND:
        switch (LOWORD(wParam)) {
		case SC_MONITORPOWER:
			// Prevent potential crashes when the monitor powers down
			return 1;

        case IDM_EXIT:
            // Recieved key/menu command to exit render window
            SendMessage (hWnd, WM_CLOSE, 0, 0);
            return 0;
        }
        break;

		// DDE dynamic data exchange support
	case WM_DDE_INITIATE:
	case WM_DDE_REQUEST:
	case WM_DDE_ACK:
		if (ddeserver) ddeserver->MsgProc (hWnd, uMsg, wParam, lParam);
		break;

	case WM_NCHITTEST:
        // Prevent the user from selecting the menu in fullscreen mode
        if (IsFullscreen()) return HTCLIENT;
        break;

		// shutdown options
	case WM_CLOSE:
		PreCloseSession();
		DestroyWindow (hWnd);
		return 0;

	case WM_DESTROY:
		CloseSession ();
        break;
	}
    return DefWindowProc (hWnd, uMsg, wParam, lParam);
}

//-----------------------------------------------------------------------------
// Name: Select_Main()
// Desc: Create main menu
//-----------------------------------------------------------------------------
bool Orbiter::Select_Main (Select &sel)
{
	sel.Append (bRunning ? "Pause":"Resume");
	sel.AppendSeparator();
	sel.Append ("Exit to Launchpad");
	sel.Append ("Quit");
	sel.Open ("Main menu", 0, Callback_Main);
	bRenderOnce = TRUE;
	return true;
}

//-----------------------------------------------------------------------------
// Name: ActivateRoughType()
// Desc: Suppress font smoothing
//-----------------------------------------------------------------------------
bool Orbiter::ActivateRoughType ()
{
	//if (!bSysClearType) return false; // ClearType isn't user-enabled anyway
	if (bRoughType) return false; // active already

	BOOL cleartype;
	BOOL ok = SystemParametersInfo (SPI_GETFONTSMOOTHING, 0, &cleartype, 0);
	if (!ok) return false; // ClearType status can't be determined
	if (!cleartype || SystemParametersInfo (SPI_SETFONTSMOOTHING, FALSE, NULL, SPIF_SENDCHANGE)) {
		bRoughType = true;
		return true;
	} else return false;
}

//-----------------------------------------------------------------------------
// Name: DeactivateRoughType()
// Desc: Re-enable font smoothing
//-----------------------------------------------------------------------------
bool Orbiter::DeactivateRoughType ()
{
	bool bEnforceClearType = pConfig->CfgDebugPrm.bForceReenableSmoothFont;
	if (!bSysClearType && !bEnforceClearType) return false; // ClearType isn't user-enabled anyway
	if (!bRoughType) return false; // not active
	if (SystemParametersInfo (SPI_SETFONTSMOOTHING, TRUE, NULL, SPIF_SENDCHANGE)) {
		bRoughType = false;
		return true;
	} else return false;
}

#ifndef INLINEGRAPHICS
//-----------------------------------------------------------------------------
// Name: AttachGraphicsClient()
// Desc: Link an external graphics render interface
//-----------------------------------------------------------------------------
bool Orbiter::AttachGraphicsClient (oapi::GraphicsClient *gc)
{
	if (gclient) return false; // another client is already attached
	register_module = gc;
	if (gc->clbkUseLaunchpadVideoTab()) pMainDlg->UnhidePage (4, "Video");
	gclient = gc;
	gclient->clbkInitialise();
	return true;
}

//-----------------------------------------------------------------------------
// Name: RemoveGraphicsClient()
// Desc: Unlink an external graphics render interface
//-----------------------------------------------------------------------------
bool Orbiter::RemoveGraphicsClient (oapi::GraphicsClient *gc)
{
	if (!gclient || gclient != gc) return false; // no client attached
	pMainDlg->HidePage (4);
	gclient = NULL;
	return true;
}
#endif // !INLINEGRAPHICS

bool Orbiter::RegisterWindow (HINSTANCE hInstance, HWND hWnd, DWORD flag)
{
	return (pDlgMgr ? (pDlgMgr->AddWindow (hInstance, hWnd, hRenderWnd, flag) != NULL) : NULL);
}

void Orbiter::UpdateDeallocationProgress()
{
	pMainDlg->UpdateWaitProgress();
}

HWND Orbiter::OpenDialog (int id, DLGPROC pDlg, void *context)
{
	return OpenDialog (hInst, id, pDlg, context);
}

HWND Orbiter::OpenDialogEx (int id, DLGPROC pDlg, DWORD flag, void *context)
{
	return OpenDialogEx (hInst, id, pDlg, flag, context);
}

HWND Orbiter::OpenDialog (HINSTANCE hInstance, int id, DLGPROC pDlg, void *context)
{
	return (pDlgMgr ? pDlgMgr->OpenDialog (hInstance, id, hRenderWnd, pDlg, context) : NULL);
}

HWND Orbiter::OpenDialogEx (HINSTANCE hInstance, int id, DLGPROC pDlg, DWORD flag, void *context)
{
	return (pDlgMgr ? pDlgMgr->OpenDialogEx (hInstance, id, hRenderWnd, pDlg, flag, context) : NULL);
}

HWND Orbiter::OpenHelp (HELPCONTEXT *hcontext)
{
	if (pDlgMgr) {
		DlgHelp *pHelp = pDlgMgr->EnsureEntry<DlgHelp> ();
		HWND hHelp = pHelp->GetHwnd();
		PostMessage (hHelp, WM_USER+1, 0, (LPARAM)hcontext);
		return hHelp;
	} else return NULL;
}

void Orbiter::OpenLaunchpadHelp (HELPCONTEXT *hcontext)
{
	::OpenHelp (0, hInst, hcontext->helpfile, hcontext->topic);
}

void Orbiter::CloseDialog (HWND hDlg)
{
	if (pDlgMgr) pDlgMgr->CloseDialog (hDlg);
}

HWND Orbiter::IsDialog (HINSTANCE hInstance, DWORD resId)
{
	return (pDlgMgr ? pDlgMgr->IsEntry (hInstance, resId) : NULL);
}

//-----------------------------------------------------------------------------
// DDE (dynamic data exchange) interface
//-----------------------------------------------------------------------------
void Orbiter::DDEInit (HWND hClient, ATOM topic)
{
	char cbuf[256];
	if (topic) {
		GlobalGetAtomName (topic, cbuf, 256);
		if (!_stricmp (cbuf, "data")) {
			ATOM app = GlobalAddAtom ("orbiter");
			ATOM tpc = GlobalAddAtom ("data");
			SendMessage (hClient, WM_DDE_ACK, (WPARAM)hRenderWnd, MAKELPARAM (app, tpc));
			GlobalDeleteAtom (app);
			GlobalDeleteAtom (tpc);
		}
	}
}

void Orbiter::DDERequest (HWND hClient, int format, ATOM item)
{
	char citem[256], cbuf[256]; cbuf[0] = '\0';
	GlobalGetAtomName (item, citem, 256);
	if (!_stricmp (citem, "simtime")) {
		sprintf (cbuf, "%f", td.SimT0);
	} else if (!_stricmp (citem, "altitude")) {
		const SurfParam *sp = g_focusobj->GetSurfParam ();
		if (sp) sprintf (cbuf, "%f", sp->alt);
	} else if (!_stricmp (citem, "airspeed")) {
		const SurfParam *sp = g_focusobj->GetSurfParam ();
		if (sp) sprintf (cbuf, "%f", sp->airspd);
	} else if (!_stricmp (citem, "cpos")) {
		const CelestialBody *cbody = g_focusobj->ElRef();
		if (cbody) {
			Vector p (g_focusobj->GPos() - cbody->GPos());
			sprintf (cbuf, "%g %g %g", p.x, p.y, p.z);
		}
	}
	if (cbuf[0]) { // request can be served
		HGLOBAL hData = GlobalAlloc (GMEM_MOVEABLE | GMEM_DDESHARE, (LONG)sizeof(DDEDATA) + lstrlen(cbuf) + 1);
		DDEDATA *pData = (DDEDATA*)GlobalLock (hData);
		pData->cfFormat = CF_TEXT;
		pData->fResponse = 1;
		pData->fRelease = 1;
		pData->fAckReq = 0;
		lstrcpy ((LPSTR)pData->Value, (LPSTR)cbuf);
		GlobalUnlock (hData);
		ATOM a = GlobalAddAtom (citem);
		LPARAM lParam = PackDDElParam (WM_DDE_ACK, (UINT_PTR)hData, a);
		PostMessage (hClient, WM_DDE_DATA, (WPARAM)hRenderWnd, lParam);
	} else {       // request can't be served
		// to do
	}
}

//=============================================================================
// Implementation of class TimeData
//=============================================================================

TimeData::TimeData ()
{
	Reset();
}

void TimeData::Reset (double mjd_ref)
{
	TWarp = TWarpTarget = 1.0;
	TWarpDelay = 0.0;
	SysT0 = SysT1 = SysDT = 0.0;
	SimT0 = SimT1 = SimDT = SimDT0 = 0.0;
	SimT1_ofs = SimT1_inc = 0.0;
	MJD_ref = MJD0 = MJD1 = mjd_ref;
	fps = syst_acc = 0.0;
	framecount = frame_tick = sys_tick = 0;
	bWarpChanged = false;

	fixed_step = 0.0;
	bFixedStep = false;
}

void TimeData::SetFixedStep(double step)
{
	fixed_step = step;
	bFixedStep = (fixed_step > 0.0);
}

void TimeData::BeginStep (double deltat, bool running)
{
	bWarpChanged = false;
	SysT1 = SysT0 + (SysDT = deltat);
	iSysDT = 1.0/SysDT; // note that delta_ms==0 is trapped earlier

	framecount++;
	frame_tick++;
	syst_acc += SysDT;
	if ((size_t)SysT1 != sys_tick) {
		fps = frame_tick/syst_acc;
		frame_tick = 0;
		syst_acc = 0.0;
		sys_tick = (size_t)SysT1;
	}

	if (running) { // only advance simulation time if simulation is not paused

		if (TWarp != TWarpTarget) {
			if (TWarpDelay == 0.0)
				TWarp = TWarpTarget;
			else if (TWarpTarget > TWarp)
				TWarp = min (TWarpTarget, TWarp * pow (10, SysDT/TWarpDelay));
			else
				TWarp = max (TWarpTarget, TWarp * pow (10, -SysDT/TWarpDelay));
			bWarpChanged = true;
		}

		SimDT = (bFixedStep ? fixed_step : SysDT) * TWarp;
		iSimDT = 1.0/SimDT;
		if ((SimT1_inc += SimDT) > 1e6) {
			SimT1_ofs += 1e6;
			SimT1_inc -= 1e6;
		}
		SimT1 = SimT1_ofs + SimT1_inc;
		MJD1 = MJD_ref + Day (SimT1);
	}
}

void TimeData::EndStep (bool running)
{
	SysT0 = SysT1;

	if (running) {
		SimT0 = SimT1;
		SimDT0 = SimDT;
		iSimDT0 = iSimDT;
		MJD0 = MJD1;
	}
}

double TimeData::JumpTo (double mjd)
{
	double dt = (mjd-MJD0)*86400.0;
	MJD0 = MJD1 = mjd;
	SimT0 = SimT1 = SimT1_ofs = (mjd-MJD_ref)*86400.0;
	SimT1_inc = 0.0;
	return dt;
}

void TimeData::SetWarp (double warp, double delay) {
	TWarpTarget = warp;
	TWarpDelay  = delay;
	if (delay == 0.0) {
		TWarp = warp;
		bWarpChanged = true;
	}
}

//=============================================================================
// Nonmember functions
//=============================================================================

//-----------------------------------------------------------------------------
// Name: Callback_Main()
// Desc: Callback for main menu
//-----------------------------------------------------------------------------
bool Callback_Main (Select *sel, int item, char*, void *data)
{
	switch (item) {
	case 0:
		g_pOrbiter->Pause (g_pOrbiter->bRunning);
		break;
	case 1:
		if (g_pOrbiter->hRenderWnd) PostMessage (g_pOrbiter->hRenderWnd, WM_CLOSE, 0, 0);
		break;
	case 2:
		if (g_pOrbiter->hRenderWnd) PostMessage (g_pOrbiter->hRenderWnd, WM_CLOSE, 0, 0);
		PostMessage (g_pOrbiter->hDlg, WM_CLOSE, 0, 0);
		break;
	}
	return true;
}

INT_PTR CALLBACK BkMsgProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg) {
	case WM_SIZE: {
		RECT r;
		GetWindowRect (hDlg, &r);
		MoveWindow (GetDlgItem (hDlg, IDC_IMG), 0, 0, r.right, r.bottom, TRUE);
		} return 1;
	}
	return 0;
}

INT_PTR CALLBACK CloseMsgProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	return 0;
}
