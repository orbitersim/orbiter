// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#ifndef __linux__
#define STRICT 1
#endif // !__linux__
#define OAPI_IMPLEMENTATION

#ifndef __linux__
// Enable visual styles. Source: https://msdn.microsoft.com/en-us/library/windows/desktop/bb773175(v=vs.85).aspx
#pragma comment(linker,"\"/manifestdependency:type='win32' name='Microsoft.Windows.Common-Controls' version='6.0.0.0' processorArchitecture='*' publicKeyToken='6595b64144ccf1df' language='*'\"")

#include <windows.h>
#include <direct.h>
#else // __linux__
// common-controls manifest left out: the controls are Qt widgets
#endif // __linux__
#include <stdio.h>
#include <time.h>
#include <fstream>
#ifndef __linux__
#include <process.h> 
#else // __linux__
#include <unistd.h>
#include <dlfcn.h>
#include <clocale>
#endif // __linux__
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
#include "Memstat.h"
#include "CustomControls.h"
#include "Help.h"
#include "Util.h"
#include "DlgHelp.h" // temporary
#include "htmlctrl.h"
#include "DlgCtrl.h"
#include "GraphicsAPI.h"
#include "ConsoleManager.h"
#include "imgui.h"
#ifndef __linux__
#include "imgui_impl_win32.h"
#else // __linux__
#include "imgui_impl_qt.h"
#include "ResDialog.h"
#include "OrbiterResource.h"
#include <QApplication>
#include <QMessageBox>
#include <QWindow>
#include <QCursor>
#include <QCloseEvent>
#include <QKeyEvent>
#include <QMouseEvent>
#include <QWheelEvent>
#include <QAbstractEventDispatcher>
#include <QThread>
#include <QIcon>
#include <QImage>
#include "WlPointer.h"
#include "WlShortcuts.h"
#include "SleepWatch.h"
#endif // __linux__
#include <filesystem>

#include "Tracy.hpp"

namespace fs = std::filesystem;

using namespace std;
using namespace oapi;

#ifndef __linux__
extern IMGUI_IMPL_API LRESULT ImGui_ImplWin32_WndProcHandler(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam);
#endif // !__linux__

#define OUTPUT_DBG
#define LOADSTATUSCOL 0xC08080 //0xFFD0D0

//#define OUTPUT_TEXTURE_INFO

#define KEYDOWN(name,key) (name[key] & 0x80) 

const int MAX_TEXTURE_BUFSIZE = 8000000;
// Texture manager buffer size. Should be determined from
// memory size (System or Video?)

#ifndef __linux__
const TCHAR* g_strAppTitle = "OpenOrbiter";
#else // __linux__
const char* g_strAppTitle = "OpenOrbiter";
#endif // __linux__

#ifndef __linux__
const TCHAR* MasterConfigFile = "Orbiter.cfg";
#else // __linux__
const char* MasterConfigFile = "Orbiter.cfg";
#endif // __linux__

#ifndef __linux__
const TCHAR* CurrentScenario = "(Current state)";
#else // __linux__
const char* CurrentScenario = "(Current state)";
#endif // __linux__
char ScenarioName[256] = "\0";
// some global string resources

char cwd[512];

// =======================================================================
// Global variables

Orbiter*        g_pOrbiter       = NULL;  // application
BOOL            g_bFrameMoving   = TRUE;
extern BOOL     g_bAppUseZBuffer;
extern BOOL     g_bAppUseBackBuffer;
double          g_nearplane      = 5.0;
double          g_farplane       = 5e6;
const double    MinWarpLimit     = 0.1;  // make variable
const double    MaxWarpLimit     = 1e5;  // make variable
DWORD           g_qsaveid        = 0;
DWORD           g_customcmdid    = 0;
int             g_iCursorShowCount = 0;

// 2D info output flags
BOOL g_bOutputTime  = TRUE;
BOOL g_bOutputFPS   = TRUE;
BOOL g_bOutputDim   = TRUE;
bool g_bForceUpdate = true;
bool g_bShowGrapple = false;
bool g_bStateUpdate = false;

// Timing parameters
DWORD  launch_tick;      // counts the first 3 frames
DWORD  g_vtxcount = 0;   // vertices/frame rendered (for diagnosis)
DWORD  g_tilecount = 0;  // surface tiles/frame rendered (for diagnosis)
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

char DBG_MSG[256] = "";

// Default help context (for main help system)
HELPCONTEXT DefHelpContext = {
	(char*)"html/orbiter.chm",
	0,
	(char*)"html/orbiter.chm::/orbiter.hhc",
	(char*)"html/orbiter.chm::/orbiter.hhk"
};

// =======================================================================
// Function prototypes

#ifndef __linux__
HRESULT ConfirmDevice (DDCAPS*, D3DDEVICEDESC7*);
#else // __linux__
// ConfirmDevice (DDCAPS*, D3DDEVICEDESC7*) left out: Direct3D 7 device selection
#endif // __linux__

//LRESULT CALLBACK WndProc3D (HWND, UINT, WPARAM, LPARAM);
#ifndef __linux__
INT_PTR CALLBACK BkMsgProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam);
#else // __linux__
static void BkMsgProc (QWidget *hDlg);
#endif // __linux__

VOID    DestroyWorld ();
void    SetEnvironmentVars ();
#ifndef __linux__
HANDLE hMutex = 0;
HANDLE hConsoleMutex = 0;
#else // __linux__
// hMutex, hConsoleMutex left out: unused handles
#endif // __linux__

// =======================================================================
// _matherr()
// trap global math exceptions
#ifdef __linux__
// glibc has no math error hook: the exe exports its own acos, which every module binds to, as the CRT took the exe's _matherr
#endif // __linux__

#ifndef __linux__
int _matherr(struct _exception *except )
#else // __linux__
extern "C" __attribute__((visibility("default"))) double acos (double x) noexcept
#endif // __linux__
{
#ifndef __linux__
	if (!strcmp (except->name, "acos")) {
		except->retval = (except->arg1 < 0.0 ? Pi : 0.0);
		return 1;
	}
	return 0;
#else // __linux__
	static double (*libm_acos)(double) = (double(*)(double))dlsym (RTLD_NEXT, "acos");
	if (x < -1.0 || x > 1.0) // _DOMAIN
		return (x < 0.0 ? Pi : 0.0);
	return libm_acos (x);
#endif // __linux__
}


// =======================================================================
#ifndef __linux__
// WinMain()
#else // __linux__
// main() (WinMain)
#endif // __linux__
// Application entry containing message loop


#ifndef __linux__
INT WINAPI WinMain (HINSTANCE hInstance, HINSTANCE, LPSTR strCmdLine, INT nCmdShow)
#else // __linux__
int main (int argc, char *argv[])
#endif // __linux__
{
#ifndef __linux__
#ifdef _CRTDBG_MAP_ALLOC
	_CrtSetDbgFlag(_CRTDBG_ALLOC_MEM_DF | _CRTDBG_LEAK_CHECK_DF);
#endif
#else // __linux__
	QApplication app (argc, argv); // the Launchpad and the dialogs are Qt widgets
	app.setQuitOnLastWindowClosed (false); // WM_QUIT comes only from the Launchpad (PostQuitMessage)
	setlocale (LC_ALL, "C"); // QApplication took the environment's locale; Orbiter parses numbers in the C locale, like the MSVC CRT

	// WinMain's command line: the arguments as one string, quoted where they contain blanks
	std::string strCmdLine;
	for (int i = 1; i < argc; i++) {
		bool quote = (strchr (argv[i], ' ') != NULL);
		if (i > 1) strCmdLine += ' ';
		if (quote) strCmdLine += '"';
		strCmdLine += argv[i];
		if (quote) strCmdLine += '"';
	}
	void *hInstance = dlopen (NULL, RTLD_NOW); // HINSTANCE: handle of the executable
#endif // __linux__

	// Verify working directory
#ifndef __linux__
	char dir[1024];
	GetCurrentDirectory(1024, dir);
#else // __linux__
	std::string dir = fs::current_path ().string (); // GetCurrentDirectory
#endif // __linux__
	// If the server version was launched from its own subdirectory, step back
	// up to the Orbiter main directory
#ifndef __linux__
	if (strlen(dir) >= 15 && !stricmp (dir+strlen(dir)-15, "\\Modules\\Server"))
		SetCurrentDirectory("..\\..");
#else // __linux__
	if (dir.size() >= 15 && !strcasecmp (dir.c_str()+dir.size()-15, "/Modules/Server"))
		fs::current_path ("../.."); // SetCurrentDirectory
#endif // __linux__

    // If we're not running from actual console, hide the window
    if (ConsoleManager::IsConsoleExclusive())
        ConsoleManager::ShowConsole(false);
    
    SetEnvironmentVars();
	g_pOrbiter = new Orbiter; // application instance
#ifdef __linux__
	new SleepWatch (qApp); // WM_POWERBROADCAST
#endif // __linux__

	// Parse command line
#ifndef __linux__
	orbiter::CommandLine::Parse(g_pOrbiter, strCmdLine);
#else // __linux__
	orbiter::CommandLine::Parse(g_pOrbiter, &strCmdLine[0]);
#endif // __linux__

	// Initialise the log
	INITLOG("Orbiter.log", g_pOrbiter->Cfg()->CfgCmdlinePrm.bAppendLog); // init log file
#ifdef ISBETA
#ifndef __linux__
	LOGOUT("Build %s BETA [v.%06d]", __DATE__, GetVersion());
#else // __linux__
	LOGOUT("Build %s BETA [v.%06d]", __DATE__, g_pOrbiter->GetVersion()); // ::GetVersion was the Windows version; the build version is meant
#endif // __linux__
#else
#ifndef __linux__
	LOGOUT("Build %s [v.%06d]", __DATE__, GetVersion());
#else // __linux__
	LOGOUT("Build %s [v.%06d]", __DATE__, g_pOrbiter->GetVersion()); // ::GetVersion was the Windows version; the build version is meant
#endif // __linux__
#endif

	// Initialise random number generator
	//srand ((unsigned)time (NULL));
	srand(12345);

	oapiRegisterCustomControls(hInstance);

#ifndef __linux__
	HRESULT hr;
#else // __linux__
	int hr;
#endif // __linux__
	// Create application
#ifndef __linux__
	if (FAILED (hr = g_pOrbiter->Create (hInstance))) {
#else // __linux__
	if ((hr = g_pOrbiter->Create (hInstance)) != 0) { // FAILED
#endif // __linux__
		LOGOUT("Application creation failed");
#ifndef __linux__
		MessageBox (NULL, "Application creation failed!\nTerminating.",
			"Orbiter Error", MB_OK | MB_ICONERROR);
#else // __linux__
		QMessageBox::critical (NULL, "Orbiter Error", "Application creation failed!\nTerminating.");
#endif // __linux__
		return 0;
	}

	setlocale (LC_CTYPE, "");

	g_pOrbiter->Run ();
	delete g_pOrbiter;
	return 0;
}

void SetEnvironmentVars ()
{
#ifndef __linux__
	// Set search path to "Modules" subdirectory so that DLLs are found
	char *ppath = getenv ("PATH");
	if (ppath) {
		char *cbuf = new char[strlen(ppath)+15]; TRACENEW
		sprintf (cbuf, "PATH=%s;Modules", ppath);
		_putenv (cbuf);
		delete []cbuf;
		cbuf = NULL;
	} else {
		_putenv ("PATH=Modules");
	}
	_getcwd (cwd, 512);
#else // __linux__
	// PATH=...;Modules left out: dlopen doesn't search PATH, modules find their libraries through their RUNPATH
	if (!getcwd (cwd, 512)) cwd[0] = '\0';
#endif // __linux__
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

	auto outputCallback = [](const char* msg, int line, void* callbackContext) 
	{ 
		Orbiter* _this = static_cast<Orbiter*>(callbackContext);
		_this->OutputLoadStatus(msg, line); 
	};

	g_psys = new PlanetarySystem(name, pConfig, outputCallback, this); TRACENEW
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

#ifndef __linux__
	// Initialise timer
	timeBeginPeriod(1);
#else // __linux__
	// timeBeginPeriod(1) left out: Linux timers need no resolution request
#endif // __linux__

	pDI             = new DInput(this); TRACENEW
	pConfig         = new Config; TRACENEW
	pState          = NULL;
	m_pLaunchpad    = NULL;
	pDlgMgr         = NULL;
	m_pConsole      = NULL;
	bFullscreen     = false;
	viewW = viewH = viewBPP = 0;
	gclient         = NULL;
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
	memstat = nullptr;

	simheapsize     = 0;

	for (int i = 0; i < 15; i++)
		ctrlKeyboard[i] = ctrlJoystick[i] = ctrlTotal[i] = 0; // reset keyboard and joystick attitude requests

	memset (simkstate, 0, 256);


	RegisterMenuCmd("Ship",     "MenuInfoBar/ship.png",     [](void *) {g_pOrbiter->DlgMgr()->EnsureEntry<DlgFocus>();});
	RegisterMenuCmd("Camera",   "MenuInfoBar/camera.png",   [](void *) {g_pOrbiter->DlgMgr()->EnsureEntry<DlgCamera>();});
	RegisterMenuCmd("Speed",    "MenuInfoBar/speed.png",    [](void *) {g_pOrbiter->DlgMgr()->EnsureEntry<DlgTacc>();});
	RegisterMenuCmd("Pause",    "MenuInfoBar/pause.png",    [](void *) {g_pOrbiter->TogglePause();});
	RegisterMenuCmd("Function", "MenuInfoBar/function.png", [](void *) {g_pOrbiter->DlgMgr()->EnsureEntry<DlgFunction>();});
	RegisterMenuCmd("Info",     "MenuInfoBar/info.png",     [](void *) {g_pOrbiter->DlgMgr()->EnsureEntry<DlgInfo>();});
	RegisterMenuCmd("Options",  "MenuInfoBar/options.png",  [](void *) {g_pOrbiter->DlgMgr()->EnsureEntry<DlgOptions>();});
	RegisterMenuCmd("Map",      "MenuInfoBar/map.png",      [](void *) {g_pOrbiter->DlgMgr()->EnsureEntry<DlgMap>();});
	RegisterMenuCmd("Record",   "MenuInfoBar/record.png",   [](void *) {g_pOrbiter->DlgMgr()->EnsureEntry<DlgRecorder>();});
	RegisterMenuCmd("Help",     "MenuInfoBar/help.png",     [](void *) {
			extern HELPCONTEXT DefHelpContext;
			DefHelpContext.topic = (char*)"/mainmenu.htm";
			g_pOrbiter->OpenHelp (&DefHelpContext);			
		});
	RegisterMenuCmd("Save",     "MenuInfoBar/save.png",     [](void *) {g_pOrbiter->Quicksave();});
#ifndef __linux__
	RegisterMenuCmd("Exit",     "MenuInfoBar/exit.png",     [](void *) {PostMessage(g_pOrbiter->GetRenderWnd(), WM_CLOSE, 0, 0);});
#else // __linux__
	RegisterMenuCmd("Exit",     "MenuInfoBar/exit.png",     [](void *) {QCoreApplication::postEvent(g_pOrbiter->GetRenderWnd(), new QCloseEvent);}); // PostMessage WM_CLOSE
#endif // __linux__

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
#ifndef __linux__
HRESULT Orbiter::Create (HINSTANCE hInstance)
#else // __linux__
int Orbiter::Create (void *hInstance)
#endif // __linux__
{
#ifndef __linux__
	if (m_pLaunchpad) return S_OK; // already created
#else // __linux__
	if (m_pLaunchpad) return 0; // already created
#endif // __linux__

#ifndef __linux__
	HRESULT hr;
	WNDCLASS wndClass;
#else // __linux__
	int hr;
#endif // __linux__

#ifndef __linux__
	// Enable tab controls
	InitCommonControls();
	LoadLibrary ("riched20.dll");
#else // __linux__
	// InitCommonControls and riched20.dll left out: the controls are Qt widgets
#endif // __linux__

	// parameter manager - parses from master config file
	hInst = hInstance;
	pConfig->Load(MasterConfigFile);
	strcpy (cfgpath, pConfig->CfgDirPrm.ConfigDir);   cfglen = strlen (cfgpath);

#ifndef __linux__
	if (FAILED (hr = pDI->Create (hInstance))) return hr;
#else // __linux__
	if ((hr = pDI->Create (hInstance)) != DI_OK) return hr;
#endif // __linux__

	// validate configuration
	if (pConfig->CfgJoystickPrm.Joy_idx > GetDInput()->NumJoysticks()) pConfig->CfgJoystickPrm.Joy_idx = 0;

	// Read key mapping from file (or write default keymap)
	if (!keymap.Read ("keymap.cfg")) keymap.Write ("keymap.cfg");

    pState = new State(); TRACENEW

#ifndef __linux__
	// Register main dialog window class
	GetClassInfo (hInstance, "#32770", &wndClass); // override default dialog class
	wndClass.hIcon = LoadIcon (hInstance, MAKEINTRESOURCE (IDI_MAIN_ICON));
	RegisterClass (&wndClass);

	// Find out if we are running under Linux/WINE
	HKEY key;
	long ret = RegOpenKeyEx (HKEY_CURRENT_USER, TEXT("Software\\Wine"), 0, KEY_QUERY_VALUE, &key);
	RegCloseKey (key);
	bWINEenv = (ret == ERROR_SUCCESS);
#else // __linux__
	// Main dialog icon: the dialog class icon becomes the application's window icon
	if (QImage *icon = oapiLoadResImage (hInstance, IDI_MAIN_ICON)) {
		QApplication::setWindowIcon (QIcon (QPixmap::fromImage (*icon)));
		delete icon;
	}

	// Find out if we are running under Linux/WINE: native, never
	bWINEenv = false;
#endif // __linux__

	// Register HTML viewer class
	RegisterHtmlCtrl (hInstance, UseHtmlInline());
	CustomCtrl::RegisterClass (hInstance);

	if (pConfig->CfgCmdlinePrm.bFastExit)
		SetFastExit(true);
	if (pConfig->CfgCmdlinePrm.bOpenVideoTab)
		OpenVideoTab();

	if (pConfig->CfgDemoPrm.bBkImage) {
#ifndef __linux__
		hBk = CreateDialog (hInstance, MAKEINTRESOURCE(IDD_DEMOBK), NULL, BkMsgProc);
		ShowWindow (hBk, SW_MAXIMIZE);
#else // __linux__
		if ((hBk = oapiCreateResDialog (hInstance, IDD_DEMOBK, NULL))) {
			BkMsgProc (hBk);
			hBk->showMaximized (); // SW_MAXIMIZE
		}
#endif // __linux__
	}
	
	// Create the "launchpad" main dialog window
	m_pLaunchpad = new orbiter::LaunchpadDialog (this); TRACENEW
	m_pLaunchpad->Create (bStartVideoTab);

	Instrument::RegisterBuiltinModes();

	script = new ScriptInterface(this); TRACENEW

	// preload modules from command line requests
#ifndef __linux__
	LoadModules("Modules\\Plugin", pConfig->CfgCmdlinePrm.LoadPlugins);
#else // __linux__
	LoadModules("Modules/Plugin", pConfig->CfgCmdlinePrm.LoadPlugins);
#endif // __linux__

	// preload active plugin modules
#ifndef __linux__
	LoadModules("Modules\\Plugin", pConfig->GetActiveModules());
#else // __linux__
	LoadModules("Modules/Plugin", pConfig->GetActiveModules());
#endif // __linux__

	// preload startup plugin modules
	LoadStartupModules();

	{
#ifndef __linux__
		BOOL cleartype, ok;
		ok = SystemParametersInfo(SPI_GETFONTSMOOTHING, 0, &cleartype, 0);
		bSysClearType = (ok && cleartype);
#else // __linux__
		// SystemParametersInfo (SPI_GETFONTSMOOTHING) left out: Linux desktops smooth fonts, Qt picks it per font
		bSysClearType = true;
#endif // __linux__
		//if (pConfig->CfgDebugPrm.bForceReenableSmoothFont) bSysClearType = true;
	}
	if (pConfig->CfgDebugPrm.bDisableSmoothFont)
		ActivateRoughType();

	memstat = new MemStat;
	
#ifndef __linux__
	return S_OK;
#else // __linux__
	return 0; // S_OK
#endif // __linux__
}

//-----------------------------------------------------------------------------
// Name: SaveConfig()
// Desc: Save configuration files (before closedown)
//-----------------------------------------------------------------------------
void Orbiter::SaveConfig ()
{
	pConfig->Write (); // save current settings
	m_pLaunchpad->WriteExtraParams ();
}

//-----------------------------------------------------------------------------
// Name: CloseApp()
// Desc: Cleanup for program end
//-----------------------------------------------------------------------------
VOID Orbiter::CloseApp (bool fast_shutdown)
{
	SaveConfig();
	while (m_Plugin.size()) UnloadModule (m_Plugin.begin()->hDLL);

	if (bRoughType)
		DeactivateRoughType();

	if (!fast_shutdown) {
		delete pDI;
		if (memstat) delete memstat;
		if (pConfig)  delete pConfig;
		if (m_pLaunchpad) delete m_pLaunchpad;
#ifndef __linux__
		if (hBk) DestroyWindow (hBk);
#else // __linux__
		if (hBk) delete hBk; // DestroyWindow
#endif // __linux__
		if (pState)   delete pState;
		if (script) delete script;
		if (ncustomcmd) {
			for (DWORD i = 0; i < ncustomcmd; i++) {
				delete []customcmd[i].label;
				customcmd[i].label = NULL;
			}
			delete []customcmd;
			customcmd = NULL;
		}
		oapiUnregisterCustomControls (hInst);
	}
#ifndef __linux__
	timeEndPeriod (1);
#else // __linux__
	// timeEndPeriod left out: no timer resolution was requested
#endif // __linux__
}

//-----------------------------------------------------------------------------
// Name: GetVersion()
// Desc: Returns orbiter build version as integer in YYMMDD format
//-----------------------------------------------------------------------------
int Orbiter::GetVersion () const
{
	static int v = 0;
	if (!v) {
		static const char *mstr[12] = {"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"};
		char ms[32];
		int day, month, year;
		sscanf (__DATE__, "%s%d%d", ms, &day, &year);
		for (month = 0; month < 12; month++)
#ifndef __linux__
			if (!_strnicmp (ms, mstr[month], 3)) break;
#else // __linux__
			if (!strncasecmp (ms, mstr[month], 3)) break;
#endif // __linux__
		v = (year%100)*10000 + (month+1)*100 + day;
	}
	return v;
}

//! Finds legacy module consisting of a single DLL
//! @return true on success
//! @param cbufOut returns path to the plugin DLL
static bool FindStandaloneDll(const char *path, const char *name, char* cbufOut)
{
#ifndef __linux__
	sprintf (cbufOut, "%s\\%s.dll", path, name);
#else // __linux__
	sprintf (cbufOut, "%s/%s.so", path, name);
	strcpy (cbufOut, oapiResolvePath (cbufOut).c_str());
#endif // __linux__
	return fs::exists(cbufOut);
}

//! Finds module consisting of a plugin DLL inside a plugin-specific folder
//! @return true on success
//! @param cbufOut returns path to the plugin DLL
static bool FindDllInPluginFolder(const char *path, const char *name, char* cbufOut)
{
#ifndef __linux__
	sprintf(cbufOut, "%s\\%s\\%s.dll", path, name, name);
#else // __linux__
	sprintf(cbufOut, "%s/%s/%s.so", path, name, name);
	strcpy (cbufOut, oapiResolvePath (cbufOut).c_str());
#endif // __linux__
	return fs::exists(cbufOut);
}

void Orbiter::LoadModules(const std::string& path, const std::list<std::string>& names)
{
	for (auto name : names)
		LoadModule(path.c_str(), name.c_str());
}

void Orbiter::LoadModules(const std::string& path)
{
#ifndef __linux__
	for (const auto& entry : fs::directory_iterator(path)) {
#else // __linux__
	for (const auto& entry : fs::directory_iterator(oapiResolvePath(path.c_str()))) {
#endif // __linux__
		auto fpath = entry.path();
#ifndef __linux__
		if (fpath.extension().string() == ".dll") {
#else // __linux__
		if (fpath.extension().string() == ".so") {
#endif // __linux__
			LoadModule(path.c_str(), fpath.stem().string().c_str());
		}
	}
}

//-----------------------------------------------------------------------------
// Name: LoadStartupModules()
// Desc: Load all plugin modules from the "startup" directory
//-----------------------------------------------------------------------------
void Orbiter::LoadStartupModules()
{
#ifndef __linux__
	LoadModules("Modules\\Startup");
#else // __linux__
	LoadModules("Modules/Startup");
#endif // __linux__
}

//-----------------------------------------------------------------------------
// Name: LoadModule()
// Desc: Load a named plugin DLL
//-----------------------------------------------------------------------------
#ifndef __linux__
HINSTANCE Orbiter::LoadModule (const char *path, const char *name)
#else // __linux__
void *Orbiter::LoadModule (const char *path, const char *name)
#endif // __linux__
{
	register_module = NULL; // Clear the module. The loaded library may optionally populate it on LoadLibrary() call below.

	// Load the module DLL
#ifndef __linux__
	HINSTANCE hDLL = NULL;
	char cbuf[256];
#else // __linux__
	void *hDLL = NULL;
	char cbuf[1024]; // 256 upstream; Linux working directories run longer
#endif // __linux__
	if (FindStandaloneDll(path, name, cbuf)) // try to find standalone plugin file
	{
#ifndef __linux__
		hDLL = LoadLibrary (cbuf);
#else // __linux__
		hDLL = dlopen (cbuf, RTLD_NOW); // LoadLibrary
#endif // __linux__
	}
	else // try to find plugin in a plugin folder
	{
#ifndef __linux__
		char cbuf2[256];
#else // __linux__
		char cbuf2[512];
#endif // __linux__
		if (FindDllInPluginFolder(path, name, cbuf2))
		{
#ifndef __linux__
			// Convert to absolute path, otherwise LoadLibraryEx fails with error code 87.
			// See https://stackoverflow.com/questions/36275535/loadlibraryex-error-87-the-parameter-is-incorrect
			sprintf(cbuf, "%s\\%s", cwd, cbuf2);
			hDLL = LoadLibraryEx(cbuf, NULL, LOAD_LIBRARY_SEARCH_DLL_LOAD_DIR | LOAD_LIBRARY_SEARCH_DEFAULT_DIRS);
#else // __linux__
			// absolute path; the module finds the libraries in its folder through its RUNPATH ($ORIGIN), as LOAD_LIBRARY_SEARCH_DLL_LOAD_DIR did
			sprintf(cbuf, "%s/%s", cwd, cbuf2);
			hDLL = dlopen (cbuf, RTLD_NOW); // LoadLibraryEx
#endif // __linux__
		}
		else
		{
			LOGOUT_ERR("Could not find a module named %s. Tried %s and %s.", name, cbuf, cbuf2);
			return NULL;
		}
	}

	// Can't initialize DirectX in DllMain(), let's do it over here (jarmonik 28.12.2023) 
	if (hDLL) {
		if (register_module == gclient && gclient != NULL) {
			if (gclient->clbkInitialise() == false) {
				// If graphics initialization fails remove client
				RemoveGraphicsClient(gclient);
#ifndef __linux__
				FreeLibrary(hDLL);
#else // __linux__
				ModuleFree(hDLL); // FreeLibrary
#endif // __linux__
				LOGOUT_ERR("Client Initialization Failed. Unloading  %s", name);
				hDLL = NULL;		
				return NULL;
			}
		}
	}

	if (hDLL) {
		DLLModule module = { hDLL, register_module ? register_module : new oapi::Module(hDLL), std::string(name), !register_module };
		// If the DLL doesn't provide a Module interface, create a default one which provides the legacy callbacks
		LOGOUT(register_module ? "Loading module %s" : "Loading module %s (legacy interface)", name);
		m_Plugin.push_back(module);
	} else {
#ifndef __linux__
		DWORD err = GetLastError();
		LOGOUT_ERR ("Failed loading module %s (code %d)", cbuf, err);
#else // __linux__
		const char *err = dlerror(); // GetLastError
		LOGOUT_ERR ("Failed loading module %s (%s)", cbuf, err ? err : "unknown error");
#endif // __linux__
	}
	return hDLL;
}

//-----------------------------------------------------------------------------
// Name: UnloadModule()
// Desc: Unload a named plugin DLL
//-----------------------------------------------------------------------------
bool Orbiter::UnloadModule (const std::string &name)
{
	for (auto it = m_Plugin.begin(); it != m_Plugin.end(); it++) {
		if (iequal(it->sName, name)) {
			LOGOUT("Unloading module %s", it->sName.c_str());
			if (it->bLocalAlloc)
				delete it->pModule;
#ifndef __linux__
			FreeLibrary(it->hDLL);
#else // __linux__
			ModuleFree(it->hDLL); // FreeLibrary
#endif // __linux__
			m_Plugin.erase(it);
			return true;
		}
	}
	return false;
}

//-----------------------------------------------------------------------------
// Name: UnloadModule()
// Desc: Unload a module by its instance
//-----------------------------------------------------------------------------
#ifndef __linux__
bool Orbiter::UnloadModule (HINSTANCE hDLL)
#else // __linux__
bool Orbiter::UnloadModule (void *hDLL)
#endif // __linux__
{
	for (auto it = m_Plugin.begin(); it != m_Plugin.end(); it++) {
		if (it->hDLL == hDLL) {
			LOGOUT("Unloading module %s", it->sName.c_str());
			if (it->bLocalAlloc)
				delete it->pModule;
#ifndef __linux__
			FreeLibrary(it->hDLL);
#else // __linux__
			ModuleFree(it->hDLL); // FreeLibrary
#endif // __linux__
			m_Plugin.erase(it);
			return true;
		}
	}
	return false;
}

//-----------------------------------------------------------------------------
// Name: FindModuleProc()
// Desc: Returns address of a procedure in a plugin module
//-----------------------------------------------------------------------------
#ifndef __linux__
OPC_Proc Orbiter::FindModuleProc (HINSTANCE hDLL, const char *procname)
#else // __linux__
OPC_Proc Orbiter::FindModuleProc (void *hDLL, const char *procname)
#endif // __linux__
{
#ifndef __linux__
	return (OPC_Proc)GetProcAddress (hDLL, procname);
#else // __linux__
	return (OPC_Proc)ModuleProc (hDLL, procname); // GetProcAddress
#endif // __linux__
}

//-----------------------------------------------------------------------------
// Name: Launch()
// Desc: Launch simulator
//-----------------------------------------------------------------------------
VOID Orbiter::Launch (const char *scenario)
{
	PrintModules();

#ifndef __linux__
	HCURSOR hCursor = SetCursor (LoadCursor (NULL, IDC_WAIT));
#else // __linux__
	QGuiApplication::setOverrideCursor (Qt::WaitCursor); // SetCursor (IDC_WAIT)
#endif // __linux__
	bool have_state = false;
	pConfig->Write (); // save current settings
	m_pLaunchpad->WriteExtraParams ();

	if (!have_state && !pState->Read (ScnPath (scenario))) {
		LOGOUT_ERR ("Scenario not found: %s", scenario);
		TerminateOnError();
	}

	long m0 = memstat->HeapUsage();
	CreateRenderWindow (pConfig, scenario);
	simheapsize = memstat->HeapUsage()-m0;
#ifndef __linux__
	SetCursor (hCursor);
#else // __linux__
	QGuiApplication::restoreOverrideCursor (); // SetCursor (hCursor)
#endif // __linux__
}

//-----------------------------------------------------------------------------
// Name: CreateRenderWindow()
// Desc: Create the window used for rendering the scene
//-----------------------------------------------------------------------------
#ifndef __linux__
HWND Orbiter::CreateRenderWindow (Config *pCfg, const char *scenario)
#else // __linux__
QWindow *Orbiter::CreateRenderWindow (Config *pCfg, const char *scenario)
#endif // __linux__
{
	DWORD i;

	SetLogVerbosity (pCfg->CfgDebugPrm.bVerboseLog);
	LOGOUT("");
	LOGOUT("**** Creating simulation session");

	m_pLaunchpad->Hide(); // hide launchpad dialog while the render window is visible
	
	if (gclient) {
		if(pState->SplashScreen())
			gclient->clbkSetSplashScreen(pState->SplashScreen(), pState->SplashColor());
#ifdef __linux__
		WlPointerAttach ();
		WlShortcutsAttach ();
#endif // __linux__
		hRenderWnd = gclient->InitRenderWnd (gclient->clbkCreateRenderWindow());
#ifdef __linux__
		if (hRenderWnd->minimumSize() != hRenderWnd->maximumSize()) // WM_GETMINMAXINFO: the tracking size, which a fixed-size window doesn't have
			hRenderWnd->setMinimumSize (QSize (100, 100));
#endif // __linux__
		GetRenderParameters ();
	} else {
		hRenderWnd = NULL;
		m_pConsole = new orbiter::ConsoleNG(this);
	}

	pDI->SetRenderWindow(hRenderWnd);

	if (hRenderWnd) {
		bActive = true;

		// Create keyboard device
		if (!pDI->CreateKbdDevice ()) {
			CloseSession ();
			return 0;
		}

		// Create joystick device
		if (pDI->CreateJoyDevice ())
			plZ4 = 1; // invalidate
	}

	if (gclient) {
		pDlgMgr = new DialogManager (this, hRenderWnd);

		// global dialog resources
		g_select = new Select(); TRACENEW
		pDlgMgr->AddEntry(g_select);
		g_input = new InputBox(); TRACENEW
		pDlgMgr->AddEntry(g_input);

		// playback screen annotation manager
		snote_playback = gclient->clbkCreateAnnotation ();
	}
	else {
		pDlgMgr = new DialogManager(this, m_pConsole->WindowHandle());
	}

	// read simulation environment state
	strcpy (ScenarioName, scenario);
	g_qsaveid = 0;
	launch_tick = 3;

	// Generate logical world objects
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
	time_prev = std::chrono::steady_clock::now() - std::chrono::milliseconds(1); // make sure SimDT > 0 for first frame

	g_psys->InitState (ScnPath (scenario));

	g_focusobj = 0;
	Vessel *vfocus = g_psys->GetVessel (pState->Focus());
	if (!vfocus)
		vfocus = g_psys->GetVessel ((DWORD)0); // in case no focus vessel was defined
	SetFocusObject (vfocus, false);

	LOGOUT("Finished initialising status");

	if (g_camera) {
		g_camera->InitState (scenario, g_focusobj);
	}
	LOGOUT ("Finished initialising camera");

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
	if ((g_focusobj) && (bPlayback = g_focusobj->bFRplayback)) {
		FRecorder_OpenPlayback (pState->PlaybackDir());
	}

	// let plugins read their states from the scenario file
	for (auto it = m_Plugin.begin(); it != m_Plugin.end(); it++) {
		void (*opcLoadState)(FILEHANDLE) = (void(*)(FILEHANDLE))FindModuleProc(it->hDLL, "opcLoadState");
		if (opcLoadState) {
#ifndef __linux__
			ifstream ifs(ScnPath(scenario));
#else // __linux__
			ifstream ifs(oapiResolvePath(ScnPath(scenario)));
#endif // __linux__
			std::string str = "BEGIN_" + it->sName;
			if (FindLine(ifs, str.c_str())) {
				opcLoadState((FILEHANDLE)&ifs);
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

	LOGOUT ("Finished setting up render state");

	const char *scriptcmd = pState->Script();
	hScnInterp = (scriptcmd ? script->RunInterpreter (scriptcmd) : NULL);

	if (gclient) gclient->clbkPostCreation();
	g_psys->PostCreation ();

	for (auto it = m_Plugin.begin(); it != m_Plugin.end(); it++) {
		it->pModule->clbkSimulationStart(rendermode);
		CHECKCWD(cwd, it->sName.c_str());
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

	// suppress throttle update on launch
	if (pDI->joyprop.bThrottle && pCfg->CfgJoystickPrm.bThrottleIgnore) {
#ifndef __linux__
		DIJOYSTATE2 js;
#else // __linux__
		JoyState js;
#endif // __linux__
		if (pDI->PollJoystick(&js))
#ifndef __linux__
			plZ4 = *(long*)(((BYTE*)&js) + pDI->joyprop.ThrottleOfs) >> 3;
#else // __linux__
			plZ4 = *(LONG*)(((BYTE*)&js) + pDI->joyprop.ThrottleOfs) >> 3; // LONG: long is 64-bit on Linux
#endif // __linux__
	}

	return hRenderWnd;
}

void Orbiter::PreCloseSession()
{
	// DEBUG
	if (pDlgMgr)  { pDlgMgr->Clear(); }

	if (gclient && pConfig->CfgDebugPrm.bSaveExitScreen) {
		// Render the scene once without the ImGui dialogs shown
		// so they don't appear on the preview
		Render3DEnvironment(true);
#ifndef __linux__
		gclient->clbkSaveSurfaceToImage (0, "Images\\CurrentState", oapi::IMAGE_JPG);
#else // __linux__
		gclient->clbkSaveSurfaceToImage (0, "Images/CurrentState", oapi::IMAGE_JPG);
#endif // __linux__
	}
}

//-----------------------------------------------------------------------------
// Name: CloseSession()
// Desc: Destroy render window and associated devices
//-----------------------------------------------------------------------------
void Orbiter::CloseSession ()
{
	DWORD i;

	bSession = false;
#ifdef __linux__
	WlShortcutsDetach (); // before the render window's surface goes: KWin keeps an inhibitor of a destroyed surface
#endif // __linux__

	if      (bRecord)   ToggleRecorder();
	else if (bPlayback) EndPlayback();
	const char* desc = pConfig->CfgDebugPrm.bSaveExitScreen ? "CurrentState_img" : "CurrentState";
	SaveScenario (CurrentScenario, desc, 2);
	if (hScnInterp) {
		script->DelInterpreter (hScnInterp);
		hScnInterp = NULL;
	}

	if (ConsoleManager::IsConsoleExclusive())
		ConsoleManager::ShowConsole(false);

	if (m_pConsole) {
		delete m_pConsole;
		m_pConsole = NULL;
	}

	if (pConfig->CfgDebugPrm.ShutdownMode == 0 && !bFastExit) { // normal cleanup
		m_pLaunchpad->Show(); // show launchpad dialog again
		m_pLaunchpad->ShowWaitPage (true, simheapsize);
		if (gclient) {
			gclient->clbkCloseSession (false);
			Base::DestroyStaticDeviceObjects ();
		}
		if (snote_playback) delete snote_playback;
		if (nsnote) {
			for (DWORD i = 0; i < nsnote; i++) delete snote[i];
			delete []snote;
			snote = NULL;
			nsnote = 0;
		}

		if (g_input)  { delete g_input; g_input = 0; }
		if (g_select) { delete g_select; g_select = 0; }
		if (g_pane) { delete g_pane;   g_pane = 0; }
		if (pDlgMgr)  { delete pDlgMgr; pDlgMgr = 0; }
		Instrument::GlobalExit (gclient);
		meshmanager.Flush(); // destroy buffered meshes
		DestroyWorld ();     // destroy logical objects
		if (gclient)
			gclient->clbkDestroyRenderWindow (false); // destroy graphics objects

		for (auto it = m_Plugin.begin(); it != m_Plugin.end(); it++)
			it->pModule->clbkSimulationEnd();

		hRenderWnd = NULL;
#ifdef __linux__
		WlPointerDetach ();
#endif // __linux__
		pDI->DestroyDevices();
		pDI->SetRenderWindow(NULL);

		m_pLaunchpad->ShowWaitPage (false);
	} else {
		if (pDlgMgr)  { delete pDlgMgr; pDlgMgr = 0; }
		if (gclient) {
			gclient->clbkCloseSession (true);
			gclient->clbkDestroyRenderWindow (true);
		}

		for (auto it = m_Plugin.begin(); it != m_Plugin.end(); it++)
			it->pModule->clbkSimulationEnd();

		hRenderWnd = NULL;
		pDI->DestroyDevices();
		CloseApp (true);
		if (pConfig->CfgDebugPrm.ShutdownMode == 2 || bFastExit) {
			LOGOUT("**** Fast process shutdown\r\n");
			exit (0); // just kill the process
		} else {
			LOGOUT("**** Respawning Orbiter process\r\n");
#ifndef __linux__
			const char *name = "orbiter.exe";
			_execl (name, name, "-l", NULL);   // respawn the process
#else // __linux__
			const char *name = "Orbiter";
			execl ("/proc/self/exe", name, "-l", (char*)NULL);   // respawn the process
#endif // __linux__
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
}

// =======================================================================
// Render3DEnvironment()
// Draws the scene

#ifndef __linux__
HRESULT Orbiter::Render3DEnvironment (bool hidedialogs)
#else // __linux__
int Orbiter::Render3DEnvironment (bool hidedialogs)
#endif // __linux__
{
	if (gclient) {
		if(!hidedialogs)
			pDlgMgr->ImGuiNewFrame();
		gclient->clbkRenderScene ();
		Output2DData ();
		if(!hidedialogs)
			gclient->clbkImGuiRenderDrawData();
		gclient->clbkDisplayFrame ();
	}
	// Mark frame boundary for when using the profiler
	FrameMark;
#ifndef __linux__
    return S_OK;
#else // __linux__
    return 0; // S_OK
#endif // __linux__
}

//-----------------------------------------------------------------------------
// Name: ScreenToClient()
// Desc: Converts screen to client coordinates. In fullscreen mode they are identical
//-----------------------------------------------------------------------------
void Orbiter::ScreenToClient (POINT *pt) const
{
#ifndef __linux__
	if (!IsFullscreen() && hRenderWnd)
		::ScreenToClient (hRenderWnd, pt);
#else // __linux__
	// also when fullscreen: the window need not sit at the screen origin; client coordinates are device pixels
	if (hRenderWnd) {
		QPoint p = hRenderWnd->mapFromGlobal (QPoint (pt->x, pt->y));
		qreal dpr = hRenderWnd->devicePixelRatio ();
		pt->x = (LONG)(p.x()*dpr), pt->y = (LONG)(p.y()*dpr);
	}
}

// DestroyWindow for the render window: its WM_DESTROY closed the session, then the window went
static void DestroyRenderWindow (QWindow *hWnd)
{
	g_pOrbiter->CloseSession ();
	hWnd->hide ();
	hWnd->deleteLater ();
#endif // __linux__
}

//-----------------------------------------------------------------------------
// Name: Run()
// Desc: Message-processing loop. Idle time is used to render the scene.
//-----------------------------------------------------------------------------
INT Orbiter::Run ()
{
    // Recieve and process Windows messages
#ifndef __linux__
    BOOL  bGotMsg, bCanRender, bpCanRender = TRUE;
    MSG   msg;
    PeekMessage (&msg, NULL, 0U, 0U, PM_NOREMOVE);
#else // __linux__
	// Qt event loop for PeekMessage/GetMessage: in a session the idle step runs before each wait and wakes the loop again
    BOOL  bCanRender, bpCanRender = TRUE;
	bool  bInFrame = false;
	QAbstractEventDispatcher *dispatcher = QAbstractEventDispatcher::instance ();
#endif // __linux__

	if (!pConfig->CfgCmdlinePrm.LaunchScenario.empty())
		Launch (pConfig->CfgCmdlinePrm.LaunchScenario.c_str());
	// otherwise wait for the user to make a selection from the scenario
	// list in the launchpad dialog

#ifndef __linux__
	while (WM_QUIT != msg.message) {

        // Use PeekMessage() if the app is active, so we can use idle time to
        // render the scene. Else, use GetMessage() to avoid eating CPU time.
		if (bSession) {
            bGotMsg = PeekMessage (&msg, NULL, 0U, 0U, PM_REMOVE);
		} else {
            bGotMsg = GetMessage (&msg, NULL, 0U, 0U);
		}
        if (bGotMsg) {
			if (!m_pLaunchpad || !m_pLaunchpad->ConsumeMessage(&msg)) {
				TranslateMessage (&msg);
				DispatchMessage (&msg);
			}
		} else {
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
			bCanRender = TRUE;
			if (bCanRender && !bpCanRender)
				RestoreDeviceObjects ();
			bpCanRender = bCanRender;
		} else
			bpCanRender = TRUE;
    }
#else // __linux__
	QMetaObject::Connection idle = QObject::connect (dispatcher, &QAbstractEventDispatcher::aboutToBlock, [&]() {
		// nested loops (modal dialogs) run no frames, as their own message loops didn't on Windows
		if (!bSession || bInFrame || QThread::currentThread()->loopLevel() > 1) return;
		bInFrame = true;
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

		if (bRenderOnce && bVisible) {
			if (Render3DEnvironment () != 0) // FAILED
				if (hRenderWnd) DestroyRenderWindow (hRenderWnd);
			bRenderOnce = FALSE;
		}

		if (bSession) {
			bCanRender = TRUE;
			if (bCanRender && !bpCanRender)
				RestoreDeviceObjects ();
			bpCanRender = bCanRender;
		} else
			bpCanRender = TRUE;
		bInFrame = false;
		dispatcher->wakeUp (); // PeekMessage: come straight back for the next frame
	});

	int ret = QCoreApplication::exec (); // returns on WM_QUIT (QCoreApplication::quit)
	QObject::disconnect (idle);
#endif // __linux__
	hRenderWnd = NULL;
#ifndef __linux__
    return msg.wParam;
#else // __linux__
    return ret;
#endif // __linux__
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
#ifndef __linux__
	if (hRenderWnd) ShowWindow (hRenderWnd, FALSE);
	MessageBox (NULL,
		"Terminating after critical error. See Orbiter.log for details.",
		"Orbiter: Critical Error", MB_OK | MB_ICONERROR);
#else // __linux__
	WlShortcutsDetach ();
	if (hRenderWnd) hRenderWnd->hide (); // ShowWindow (FALSE)
	QMessageBox::critical (NULL, "Orbiter: Critical Error",
		"Terminating after critical error. See Orbiter.log for details.");
#endif // __linux__
	exit (1);
}

#ifndef __linux__
void Orbiter::UpdateServerWnd (HWND hWnd)
#else // __linux__
void Orbiter::UpdateServerWnd (QWidget *hWnd)
#endif // __linux__
{
	char cbuf[256];
	sprintf (cbuf, "%0.0fs", td.SysT0);
#ifndef __linux__
	SetWindowText (GetDlgItem (hWnd, IDC_STATIC1), cbuf);
#else // __linux__
	oapiSetDlgItemText (hWnd, IDC_STATIC1, cbuf);
#endif // __linux__
	sprintf (cbuf, "%0.0fs", td.SimT0);
#ifndef __linux__
	SetWindowText (GetDlgItem (hWnd, IDC_STATIC2), cbuf);
#else // __linux__
	oapiSetDlgItemText (hWnd, IDC_STATIC2, cbuf);
#endif // __linux__
	sprintf (cbuf, "%0.5f", td.MJD0);
#ifndef __linux__
	SetWindowText (GetDlgItem (hWnd, IDC_STATIC3), cbuf);
#else // __linux__
	oapiSetDlgItemText (hWnd, IDC_STATIC3, cbuf);
#endif // __linux__
	sprintf (cbuf, "%0.1fx", td.Warp());
#ifndef __linux__
	SetWindowText (GetDlgItem (hWnd, IDC_STATIC4), cbuf);
#else // __linux__
	oapiSetDlgItemText (hWnd, IDC_STATIC4, cbuf);
#endif // __linux__
	sprintf (cbuf, "%f", td.SimDT);
#ifndef __linux__
	SetWindowText (GetDlgItem (hWnd, IDC_STATIC5), cbuf);
#else // __linux__
	oapiSetDlgItemText (hWnd, IDC_STATIC5, cbuf);
#endif // __linux__
	sprintf (cbuf, "%f", td.FPS());
#ifndef __linux__
	SetWindowText (GetDlgItem (hWnd, IDC_STATIC6), cbuf);
#else // __linux__
	oapiSetDlgItemText (hWnd, IDC_STATIC6, cbuf);
#endif // __linux__
	sprintf (cbuf, "%zd", g_psys->nVessel());
#ifndef __linux__
	SetWindowText (GetDlgItem (hWnd, IDC_STATIC7), cbuf);
#else // __linux__
	oapiSetDlgItemText (hWnd, IDC_STATIC7, cbuf);
#endif // __linux__
}

void Orbiter::InitRotationMode ()
{
	bKeepFocus = true;

	// Checks if the cursor is already hidden
#ifndef __linux__
	if (g_iCursorShowCount == 0) {
		g_iCursorShowCount = ShowCursor(FALSE);
#else // __linux__
	if (g_iCursorShowCount == 0 && hRenderWnd) {
		hRenderWnd->setCursor (Qt::BlankCursor); // ShowCursor (FALSE)
		g_iCursorShowCount = -1;
#endif // __linux__
	}

#ifndef __linux__
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
#else // __linux__
	// SetCapture + ClipCursor: the grab delivers the button up anywhere; Camera::UpdateMouse warps the cursor back
	if (hRenderWnd) hRenderWnd->setMouseGrabEnabled (true);
	WlPointerLock (hRenderWnd); // Wayland: no warp holds the pointer, a pointer lock does
#endif // __linux__
}

void Orbiter::ExitRotationMode ()
{
	bKeepFocus = false;
#ifndef __linux__
	ReleaseCapture ();
#else // __linux__
	if (hRenderWnd) hRenderWnd->setMouseGrabEnabled (false); // ReleaseCapture, ClipCursor (NULL)
	WlPointerUnlock ();
#endif // __linux__

	// Checks if the cursor is already hidden
	if (g_iCursorShowCount < 0) {
#ifndef __linux__
		g_iCursorShowCount = ShowCursor (TRUE);
	}

	// Release cursor from render window confines
	if (!bFullscreen && hRenderWnd) {
		ClipCursor (NULL);
#else // __linux__
		if (hRenderWnd) hRenderWnd->unsetCursor (); // ShowCursor (TRUE)
		g_iCursorShowCount = 0;
#endif // __linux__
	}
}

void Orbiter::OnOptionChanged(DWORD cat, DWORD item)
{
	if (gclient)
		gclient->clbkOptionChanged(cat, item);
	if (pDI)
		pDI->OptionChanged(cat, item);
	if (g_psys)
		g_psys->OptionChanged(cat, item);
	if (g_pane)
		g_pane->OptionChanged(cat, item);
}

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
	for (auto it = m_Plugin.begin(); it != m_Plugin.end(); it++)
		it->pModule->clbkPause(bFreeze);

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

	for (auto it = m_Plugin.begin(); it != m_Plugin.end(); it++)
		it->pModule->clbkFocusChanged(g_focusobj, g_pfocusobj);

	if (pDlgMgr) pDlgMgr->BroadcastMessage (MSG_FOCUSVESSEL, vessel);

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
	for (auto it = m_Plugin.begin(); it != m_Plugin.end(); it++)
		it->pModule->clbkNewVessel((OBJHANDLE)vessel);

	if (gclient)
		gclient->clbkNewVessel((OBJHANDLE)vessel);

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
			for (auto it = m_Plugin.begin(); it != m_Plugin.end(); it++)
				it->pModule->clbkDeleteVessel((OBJHANDLE)vessel);

			if (gclient)
				gclient->clbkDeleteVessel((OBJHANDLE)vessel);
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
			g_psys->DelVessel (vessel);
		}
	}
	return true;
}

void Orbiter::NotifyObjectJump (const Body *obj, const Vector &shift)
{
	if (obj == g_camera->Target()) g_camera->Drag (-shift);
	if (g_camera->Target()) g_camera->Update ();

	// notify plugins
	for (auto it = m_Plugin.begin(); it != m_Plugin.end(); it++)
		it->pModule->clbkVesselJump((OBJHANDLE)obj);

	if (gclient)
		gclient->clbkVesselJump((OBJHANDLE)obj);
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

	// notify plugins
	for (auto it = m_Plugin.begin(); it != m_Plugin.end(); it++)
		it->pModule->clbkTimeAccChanged(nwarp, td.Warp());
}

//-----------------------------------------------------------------------------
// Name: SetFOV()
// Desc: Set field of view. Argument is FOV for vertical half-screen [rad]
//-----------------------------------------------------------------------------

VOID Orbiter::SetFOV (double fov, bool limit_range)
{
	if (g_camera->Aperture() == fov) return;

	fov = g_camera->SetAperture (fov, limit_range);
	g_bForceUpdate = true;
}

//-----------------------------------------------------------------------------
// Name: IncFOV()
// Desc: Increase field of view. Argument is delta FOV for vertical half-screen [rad]
//-----------------------------------------------------------------------------

VOID Orbiter::IncFOV (double dfov)
{
	double fov = g_camera->IncrAperture (dfov);
	g_bForceUpdate = true;
}

//-----------------------------------------------------------------------------
// Name: SaveScenario()
// Desc: save current status in-game
//-----------------------------------------------------------------------------
bool Orbiter::SaveScenario (const char *fname, const char *desc, int desc_type)
{
	pState->Update ();

#ifndef __linux__
	ofstream ofs (ScnPath (fname));
#else // __linux__
	ofstream ofs (oapiResolvePath (ScnPath (fname)));
#endif // __linux__
	if (ofs) {
		// save scenario state
		pState->Write(ofs, desc, desc_type, 0);
		//pState->Write(ofs, 0, pConfig->CfgDebugPrm.bSaveExitScreen ? "CurrentState_img" : "CurrentState");
		g_camera->Write (ofs);
		if (g_pane) g_pane->Write (ofs);
		g_psys->Write (ofs);

		// let plugins save their states to the scenario file
		for (auto it = m_Plugin.begin(); it != m_Plugin.end(); it++) {
			void (*opcSaveState)(FILEHANDLE) = (void(*)(FILEHANDLE))FindModuleProc(it->hDLL, "opcSaveState");
			if (opcSaveState) {
				ofs << std::endl << "BEGIN_" << it->sName << std::endl;
				opcSaveState((FILEHANDLE)&ofs);
				ofs << "END" << std::endl;
			}
		}
		return true;
	} else
		return false;
}

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
#ifndef __linux__
		if (ScenarioName[i-1] == '\\') break;
	sprintf (fname, "Quicksave\\%s %04d", ScenarioName+i, ++g_qsaveid);
#else // __linux__
		if (ScenarioName[i-1] == '/' || ScenarioName[i-1] == '\\') break;
	sprintf (fname, "Quicksave/%s %04d", ScenarioName+i, ++g_qsaveid);
#endif // __linux__
	if(SaveScenario (fname, desc, 0))
		oapiAddNotification(OAPINOTIF_SUCCESS, "Scenario saved successfully", fname);
	else
		oapiAddNotification(OAPINOTIF_ERROR, "Failed to save scenario", fname);
}

//-----------------------------------------------------------------------------
// write a single frame to bmp file (or to clipboard, if fname==NULL)

void Orbiter::CaptureVideoFrame ()
{
	if (gclient) {
		if (video_skip_count == pConfig->CfgCapturePrm.SequenceSkip) {
			char fname[256];
#ifndef __linux__
			sprintf (fname, "%s\\%04d", pConfig->CfgCapturePrm.SequenceDir, pConfig->CfgCapturePrm.SequenceStart++);
#else // __linux__
			sprintf (fname, "%s/%04d", pConfig->CfgCapturePrm.SequenceDir, pConfig->CfgCapturePrm.SequenceStart++);
#endif // __linux__
			oapi::ImageFileFormat fmt = (oapi::ImageFileFormat)pConfig->CfgCapturePrm.ImageFormat;
			float quality = (float)pConfig->CfgCapturePrm.ImageQuality/10.0f;
			gclient->clbkSaveSurfaceToImage (0, fname, fmt, quality);
			video_skip_count = 0;
		} else video_skip_count++;
	}
}

//-----------------------------------------------------------------------------

void Orbiter::TogglePlanetariumMode()
{
	pConfig->CfgVisHelpPrm.flagPlanetarium ^= PLN_ENABLE;
}

//-----------------------------------------------------------------------------

void Orbiter::ToggleLabelDisplay()
{
	pConfig->CfgVisHelpPrm.flagMarkers^= MKR_ENABLE;
}

//-----------------------------------------------------------------------------
// Name: PlaybackSave()
// Desc: save the start scenario for a recorded simulation
//-----------------------------------------------------------------------------
VOID Orbiter::SavePlaybackScn (const char *fname)
{
#ifndef __linux__
	char desc[256], scn[256] = "Playback\\";
#else // __linux__
	char desc[256], scn[256] = "Playback/";
#endif // __linux__
	sprintf (desc, "Orbiter playback scenario at T = %0.0f", td.SimT0);
	strcat (scn, fname);
	SaveScenario (scn, desc, 0);
}

const char *Orbiter::GetDefRecordName (void) const
{
	const char *playbackdir = pState->PlaybackDir();
	int i;
	for (i = strlen(playbackdir)-1; i > 0; i--)
#ifndef __linux__
		if (playbackdir[i-1] == '\\') break;
#else // __linux__
		if (playbackdir[i-1] == '/' || playbackdir[i-1] == '\\') break;
#endif // __linux__
	return playbackdir+i;
}

bool Orbiter::ToggleRecorder (bool force, bool append)
{
	if (bPlayback) return true; // don't allow recording during playback

	DlgRecorder *pDlg = (pDlgMgr ? pDlgMgr->EntryExists<DlgRecorder> () : NULL);
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
			return false;
		}
	} else sname = 0;
	FRecorder_Activate (bStartRecorder, sname, append);
	for (i = 0; i < n; i++)
		g_psys->GetVessel(i)->FRecorder_Activate (bStartRecorder, sname, append);
	if (bStartRecorder)
		SavePlaybackScn (sname);
	return true;
}

void Orbiter::EndPlayback ()
{
	for (DWORD i = 0; i < g_psys->nVessel(); i++)
		g_psys->GetVessel(i)->FRecorder_EndPlayback ();
	FRecorder_ClosePlayback();
	if (snote_playback) snote_playback->ClearText();
	bPlayback = false;
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
#ifndef __linux__
HRESULT Orbiter::InitDeviceObjects ()
#else // __linux__
int Orbiter::InitDeviceObjects ()
#endif // __linux__
{
#ifndef __linux__
    return S_OK;
#else // __linux__
    return 0; // S_OK
#endif // __linux__
}

//-----------------------------------------------------------------------------
// Name: RestoreDeviceObjects()
// Desc: Restore objects created for a specific device
//-----------------------------------------------------------------------------

#ifndef __linux__
HRESULT Orbiter::RestoreDeviceObjects ()
#else // __linux__
int Orbiter::RestoreDeviceObjects ()
#endif // __linux__
{
#ifndef __linux__
	return S_OK;
#else // __linux__
	return 0; // S_OK
#endif // __linux__
}

//-----------------------------------------------------------------------------
// Name: DeleteDeviceObjects()
// Desc: Delete objects created for a specific device
//-----------------------------------------------------------------------------
#ifndef __linux__
HRESULT Orbiter::DeleteDeviceObjects ()
#else // __linux__
int Orbiter::DeleteDeviceObjects ()
#endif // __linux__
{
#ifndef __linux__
	return S_OK;
#else // __linux__
	return 0; // S_OK
#endif // __linux__
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
// Name: OpenTextureFile()
// Desc: Return file handle for texture file (0=error)
//       First searches in hightex dir, then in standard dir
//-----------------------------------------------------------------------------
FILE *Orbiter::OpenTextureFile (const char *name, const char *ext)
{
	FILE *ftex = 0;
	char *pch = HTexPath (name, ext); // first try high-resolution directory
#ifndef __linux__
	if (pch && (ftex = fopen (pch, "rb"))) {
#else // __linux__
	if (pch && (ftex = fopen (oapiResolvePath (pch).c_str(), "rb"))) {
#endif // __linux__
		LOGOUT_FINE("Texture load: %s", pch);
		return ftex;
	}
	pch = TexPath (name, ext);        // try standard texture directory
	LOGOUT_FINE("Texture load: %s", pch);
#ifndef __linux__
	return fopen (pch, "rb");
#else // __linux__
	return fopen (oapiResolvePath (pch).c_str(), "rb");
#endif // __linux__
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
	if (g_pane) {
		for (DWORD i = 0; i < nsnote; i++)
			snote[i]->Render();
		if (snote_playback && pConfig->CfgRecPlayPrm.bShowNotes) snote_playback->Render();
	}
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
		pDlgMgr->BroadcastMessage (MSG_PAUSE, (void*)isPaused);

		// broadcast pause state to plugins
		for (auto it = m_Plugin.begin(); it != m_Plugin.end(); it++)
			it->pModule->clbkPause(isPaused);
	}

	// Note that for times > 1e6 the simulation time is represented by
	// an offset and increment, to avoid floating point underflow roundoff
	// when adding the current time step
	double deltat;
	auto time_curr = std::chrono::steady_clock::now();

	if (launch_tick) {
		// control time interval in first few frames, when loading events occur
		// enforce interval 10ms for first 3 time steps
		deltat = 1e-2;
		time_prev = time_curr - std::chrono::milliseconds(10);
		launch_tick--;
	} else {
		// standard time update
		std::chrono::duration<double> time_delta = time_curr - time_prev;
		deltat = time_delta.count();
	}

	if(deltat>0.1) deltat=0.1; // Prevent huge deltat when using breakpoints

	time_prev = time_curr;
	td.BeginStep (deltat, running);

	if (!running) return true;
	if (td.WarpChanged()) ApplyWarpFactor();

	return true;
}

void Orbiter::EndTimeStep (bool running)
{
	if (running) {
		if (g_psys) g_psys->FinaliseUpdate ();
		//ModulePostStep();
	}

	// Copy frame times from T1 to T0
	td.EndStep (running);

	// Update panels
	if (g_camera) g_camera->Update ();                           // camera
	if (g_pane) g_pane->Update (td.SimT1, td.SysT1);

	// Update visual states
	if (gclient) gclient->clbkUpdate (bRunning);
	g_bForceUpdate = false;                        // clear flag

	// check for termination of demo mode
#ifndef __linux__
	if (SessionLimitReached())
		if (hRenderWnd) PostMessage(hRenderWnd, WM_CLOSE, 0, 0);
#else // __linux__
	if (SessionLimitReached()) {
		if (hRenderWnd) QCoreApplication::postEvent (hRenderWnd, new QCloseEvent); // PostMessage WM_CLOSE
#endif // __linux__
		else CloseSession();
#ifdef __linux__
	}
#endif // __linux__
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
	g_psys->Timejump(tjump);
	g_camera->Update ();
	if (g_pane) g_pane->Timejump ();

	if (gclient)
		gclient->clbkTimeJump(td.SimT0, tjump.dt, _mjd);

	// broadcast to modules
	for (auto it = m_Plugin.begin(); it != m_Plugin.end(); it++)
		it->pModule->clbkTimeJump(td.SimT0, tjump.dt, _mjd);

	return true;
}

void Orbiter::Suspend (void)
{
	time_suspend = std::chrono::steady_clock::now();
}

void Orbiter::Resume (void)
{
	auto dt = std::chrono::steady_clock::now() - time_suspend;
	time_prev += dt;
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

int Orbiter::RegisterMenuCmd (const char *label, const char *imagepath, CustomFunc func, void *context)
{
	// share g_customcmdid for unique id
	menuitems.emplace_back(label, imagepath, g_customcmdid++, func, context);
	if(g_pane) {
		g_pane->MIBar()->RegisterMenuItem(label, imagepath, g_customcmdid, func, context);
	}

	return g_customcmdid;
}
void Orbiter::UnregisterMenuCmd (int cmdId)
{
	menuitems.erase(std::remove_if(menuitems.begin(), menuitems.end(), [cmdId](const auto &item) { return item.id == cmdId; }), menuitems.end());
	if(g_pane) {
		g_pane->MIBar()->UnregisterMenuItem(cmdId);
	}
}


//-----------------------------------------------------------------------------
// Name: ModulePreStep()
// Desc: call module pre-timestep callbacks
//-----------------------------------------------------------------------------
void Orbiter::ModulePreStep ()
{
	// broadcast to modules
	for (auto it = m_Plugin.begin(); it != m_Plugin.end(); it++)
		it->pModule->clbkPreStep(td.SimT0, td.SimDT, td.MJD0);

	// broadcast to vessels
	for (DWORD i = 0; i < g_psys->nVessel(); i++)
		g_psys->GetVessel(i)->ModulePreStep (td.SimT0, td.SimDT, td.MJD0);
}

//-----------------------------------------------------------------------------
// Name: ModulePostStep()
// Desc: call module post-timestep callbacks
//-----------------------------------------------------------------------------
void Orbiter::ModulePostStep ()
{
	// broadcast to vessels
	for (DWORD i = 0; i < g_psys->nVessel(); i++)
		g_psys->GetVessel(i)->ModulePostStep (td.SimT1, td.SimDT, td.MJD1);

	// broadcast to modules
	for (auto it = m_Plugin.begin(); it != m_Plugin.end(); it++)
		it->pModule->clbkPostStep(td.SimT1, td.SimDT, td.MJD1);
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
#ifndef __linux__
		if (hRenderWnd) DestroyWindow (hRenderWnd);
#else // __linux__
		if (hRenderWnd) DestroyRenderWindow (hRenderWnd);
#endif // __linux__

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
#ifndef __linux__
HRESULT Orbiter::UserInput ()
#else // __linux__
int Orbiter::UserInput ()
#endif // __linux__
{
	static char buffer[256];
#ifndef __linux__
	DIDEVICEOBJECTDATA dod[10];
	LPDIRECTINPUTDEVICE8 didev;
#else // __linux__
	KeyData dod[10];
	KeyboardDevice *didev;
#endif // __linux__
	DWORD i, dwItems = 10;
#ifndef __linux__
	HRESULT hr;
#else // __linux__
	int hr;
#endif // __linux__
	bool skipkbd = false;

	memset(simkstate, 0, 256);
	for (i = 0; i < 15; i++) ctrlKeyboard[i] = ctrlJoystick[i] = 0; // reset keyboard and joystick attitude requests

	// skip keyboard if dialogs are open
	if ((g_input && g_input->IsActive()) ||
	    (g_select && g_select->IsActive())) skipkbd = true;

#ifndef __linux__
	if (didev = GetDInput()->GetKbdDevice()) {
#else // __linux__
	if ((didev = GetDInput()->GetKbdDevice())) {
#endif // __linux__
		ImGuiIO& io = ImGui::GetIO();

		// When focus-follows-mouse is active and the mouse is not over any
		// ImGui window, route keyboard input to the simulation even though
		// ImGui's WantCaptureKeyboard may still be true (NavEnableKeyboard
		// keeps ImGui's internal focus alive after a click).
		bool imguiWantsKeyboard = io.WantCaptureKeyboard &&
			(pConfig->CfgUIPrm.MouseFocusMode == 0 || io.WantCaptureMouse);

		// keyboard input: immediate key interpretation
		hr = didev->GetDeviceState (sizeof(buffer), &buffer);
#ifndef __linux__
		if ((hr == DIERR_NOTACQUIRED || hr == DIERR_INPUTLOST) && SUCCEEDED (didev->Acquire()))
#else // __linux__
		if ((hr == DIERR_NOTACQUIRED || hr == DIERR_INPUTLOST) && didev->Acquire() == DI_OK)
#endif // __linux__
			hr = didev->GetDeviceState (sizeof(buffer), &buffer);

		// Direct input bypasses the proc loop so we skip it here
#ifndef __linux__
		if (SUCCEEDED (hr) && !imguiWantsKeyboard)
#else // __linux__
		if (hr == DI_OK && !imguiWantsKeyboard)
#endif // __linux__
			for (i = 0; i < 256; i++)
				simkstate[i] |= buffer[i];
		bool consume = BroadcastImmediateKeyboardEvent (simkstate);
		if (!skipkbd && !consume) {
			KbdInputImmediate_System (simkstate);
			if (bRunning) KbdInputImmediate_OnRunning (simkstate);
		}

		// keyboard input: buffered key events
#ifndef __linux__
		hr = didev->GetDeviceData (sizeof(DIDEVICEOBJECTDATA), dod, &dwItems, 0);
		if ((hr == DIERR_NOTACQUIRED || hr == DIERR_INPUTLOST) && SUCCEEDED (didev->Acquire()))
			hr = didev->GetDeviceData (sizeof(DIDEVICEOBJECTDATA), dod, &dwItems, 0);
		if (SUCCEEDED (hr) && !imguiWantsKeyboard) {
#else // __linux__
		hr = didev->GetDeviceData (sizeof(KeyData), dod, &dwItems, 0);
		if ((hr == DIERR_NOTACQUIRED || hr == DIERR_INPUTLOST) && didev->Acquire() == DI_OK)
			hr = didev->GetDeviceData (sizeof(KeyData), dod, &dwItems, 0);
		if (hr == DI_OK && !imguiWantsKeyboard) {
#endif // __linux__
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
#ifndef __linux__
	DIJOYSTATE2 js;
#else // __linux__
	JoyState js;
#endif // __linux__
	if (pDI->PollJoystick (&js)) {
		UserJoyInput_System (&js);                  // general joystick functions
		if (bRunning) UserJoyInput_OnRunning (&js); // joystick vessel control functions
		for (i = 0; i < 15; i++) ctrlTotal[i] += ctrlJoystick[i]; // update thrust requests
	}

	g_camera->UpdateMouse();

	// apply manual attitude control
	g_focusobj->ApplyUserAttitudeControls (ctrlTotal);

#ifndef __linux__
	return S_OK;
#else // __linux__
	return 0; // S_OK
#endif // __linux__
}

//-----------------------------------------------------------------------------
// Name: SendKbdBuffered()
// Desc: Simulate a buffered keyboard event
//-----------------------------------------------------------------------------

bool Orbiter::SendKbdBuffered(DWORD key, DWORD *mod, DWORD nmod, bool onRunningOnly)
{
	if (onRunningOnly && !bRunning) return false;

#ifndef __linux__
	DIDEVICEOBJECTDATA dod;
#else // __linux__
	KeyData dod = {};
#endif // __linux__
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
#ifndef __linux__
void Orbiter::KbdInputBuffered_System (char *kstate, DIDEVICEOBJECTDATA *dod, DWORD n)
#else // __linux__
void Orbiter::KbdInputBuffered_System (char *kstate, KeyData *dod, DWORD n)
#endif // __linux__
{
	for (DWORD i = 0; i < n; i++) {

		if (!(dod[i].dwData & 0x80)) continue; // only process key down events
		DWORD key = dod[i].dwOfs;

		if (keymap.IsLogicalKey(key, kstate, OAPI_LKEY_Pause))                TogglePause();
		else if (keymap.IsLogicalKey(key, kstate, OAPI_LKEY_Quicksave))            Quicksave();
		else if (keymap.IsLogicalKey(key, kstate, OAPI_LKEY_StepIncFOV))           SetFOV(ceil((g_camera->Aperture() * DEG + 1e-6) / 5.0) * 5.0 * RAD);
		else if (keymap.IsLogicalKey(key, kstate, OAPI_LKEY_StepDecFOV))           SetFOV(floor((g_camera->Aperture() * DEG - 1e-6) / 5.0) * 5.0 * RAD);
		else if (keymap.IsLogicalKey(key, kstate, OAPI_LKEY_MainMenu)) { 
			CFG_UIPRM &prm = g_pOrbiter->Cfg()->CfgUIPrm;
			prm.MenuMode = prm.MenuMode == 0 ? 2:0;
		}
		else if (keymap.IsLogicalKey(key, kstate, OAPI_LKEY_DlgHelp))              pDlgMgr->EnsureEntry<DlgHelp>();
		else if (keymap.IsLogicalKey(key, kstate, OAPI_LKEY_DlgCamera))            pDlgMgr->EnsureEntry<DlgCamera>();
		else if (keymap.IsLogicalKey(key, kstate, OAPI_LKEY_DlgSimspeed))          pDlgMgr->EnsureEntry<DlgTacc>();
		else if (keymap.IsLogicalKey(key, kstate, OAPI_LKEY_DlgCustomCmd))         pDlgMgr->EnsureEntry<DlgFunction>();
		else if (keymap.IsLogicalKey(key, kstate, OAPI_LKEY_DlgInfo))              pDlgMgr->EnsureEntry<DlgInfo>();
		else if (keymap.IsLogicalKey(key, kstate, OAPI_LKEY_DlgMap))               pDlgMgr->EnsureEntry<DlgMap>();
		else if (keymap.IsLogicalKey(key, kstate, OAPI_LKEY_DlgRecorder))          pDlgMgr->EnsureEntry<DlgRecorder>();
		else if (keymap.IsLogicalKey(key, kstate, OAPI_LKEY_ToggleCamInternal))    SetView(g_focusobj, !g_camera->IsExternal());
		else if (keymap.IsLogicalKey(key, kstate, OAPI_LKEY_DlgVisHelper))         pDlgMgr->EnsureEntry<DlgOptions>()->SwitchPage("Visual helpers");
		else if (keymap.IsLogicalKey(key, kstate, OAPI_LKEY_DlgCapture))           pDlgMgr->EnsureEntry<DlgCapture>();
		else if (keymap.IsLogicalKey(key, kstate, OAPI_LKEY_DlgSelectVessel))      pDlgMgr->EnsureEntry<DlgFocus>();
		else if (keymap.IsLogicalKey(key, kstate, OAPI_LKEY_DlgOptions))           pDlgMgr->EnsureEntry<DlgOptions>();
		else if (keymap.IsLogicalKey(key, kstate, OAPI_LKEY_TogglePlanetarium))    TogglePlanetariumMode();
		else if (keymap.IsLogicalKey(key, kstate, OAPI_LKEY_ToggleLabels))         ToggleLabelDisplay();
		else if (keymap.IsLogicalKey (key, kstate, OAPI_LKEY_ToggleRecPlay)) {
			if (bPlayback) EndPlayback();
			else ToggleRecorder ();
		} else if (keymap.IsLogicalKey (key, kstate, OAPI_LKEY_Quit)) {
#ifndef __linux__
			if (hRenderWnd) PostMessage (hRenderWnd, WM_CLOSE, 0, 0);
#else // __linux__
			if (hRenderWnd) QCoreApplication::postEvent (hRenderWnd, new QCloseEvent); // PostMessage WM_CLOSE
#endif // __linux__
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
#ifndef __linux__
void Orbiter::KbdInputBuffered_OnRunning (char *kstate, DIDEVICEOBJECTDATA *dod, DWORD n)
#else // __linux__
void Orbiter::KbdInputBuffered_OnRunning (char *kstate, KeyData *dod, DWORD n)
#endif // __linux__
{
	for (DWORD i = 0; i < n; i++) {

		DWORD key = dod[i].dwOfs;
		bool bdown = (dod[i].dwData & 0x80) != 0;

		if (g_focusobj->ConsumeBufferedKey (key, bdown, kstate)) // offer key to vessel for processing
			continue;
		if (!bdown) // only process key down events
			continue;
		if (key == OAPI_KEY_LSHIFT || key == OAPI_KEY_RSHIFT) continue;    // we don't process modifier keys

		// simulation speed control
		if      (keymap.IsLogicalKey (key, kstate, OAPI_LKEY_IncSimSpeed)) IncWarpFactor ();
		else if (keymap.IsLogicalKey (key, kstate, OAPI_LKEY_DecSimSpeed)) DecWarpFactor ();

		if (KEYMOD_CONTROL (kstate)) {    // CTRL-Key combinations

			//switch (key) {
			//case OAPI_KEY_F3:    // switch focus to previous vessel
			//	if (g_pfocusobj) SetFocusObject (g_pfocusobj);
			//	break;
			//}

		} else if (KEYMOD_SHIFT (kstate)) {  // Shift-key combinations (reserved for MFD control)

#ifndef __linux__
			int id = (KEYDOWN (kstate, DIK_LSHIFT) ? 0 : 1);
#else // __linux__
			int id = (KEYDOWN (kstate, OAPI_KEY_LSHIFT) ? 0 : 1); // DIK_LSHIFT
#endif // __linux__
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
#ifndef __linux__
void Orbiter::UserJoyInput_System (DIJOYSTATE2 *js)
#else // __linux__
void Orbiter::UserJoyInput_System (JoyState *js)
#endif // __linux__
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
#ifndef __linux__
void Orbiter::UserJoyInput_OnRunning (DIJOYSTATE2 *js)
#else // __linux__
void Orbiter::UserJoyInput_OnRunning (JoyState *js)
#endif // __linux__
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
#ifndef __linux__
		long lZ4 = *(long*)(((BYTE*)js)+pDI->joyprop.ThrottleOfs) >> 3;
#else // __linux__
		long lZ4 = *(LONG*)(((BYTE*)js)+pDI->joyprop.ThrottleOfs) >> 3; // LONG: long is 64-bit on Linux
#endif // __linux__
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
	// Prioritizes mouse handling while in rotation mode
	if (g_pOrbiter->StickyFocus()) {
		if (event == WM_MOUSEMOVE) return false; // may be lifted later
		if (g_camera->ProcessMouse(event, state, x, y, simkstate)) return true;
	}

	if (BroadcastMouseEvent (event, state, x, y)) return true;
	if (event == WM_MOUSEMOVE) return false; // may be lifted later

	if (bRunning) {
		if (g_pane->ProcessMouse_OnRunning (event, state, x, y, simkstate)) return true;
	}
	if (g_pane->ProcessMouse_System(event, state, x, y, simkstate)) return true;
	if (g_camera->ProcessMouse (event, state, x, y, simkstate)) return true;
	return false;
}

bool Orbiter::BroadcastMouseEvent (UINT event, DWORD state, DWORD x, DWORD y)
{
	bool consume = false;

	for (auto it = m_Plugin.begin(); it != m_Plugin.end(); it++)
		if (it->pModule && it->pModule->clbkProcessMouse(event, state, x, y))
			consume = true;

	return consume;
}

bool Orbiter::BroadcastImmediateKeyboardEvent (char *kstate)
{
	bool consume = false;

	for (auto it = m_Plugin.begin(); it != m_Plugin.end(); it++)
		if (it->pModule && it->pModule->clbkProcessKeyboardImmediate(kstate, bRunning))
			consume = true;

	return consume;
}

#ifndef __linux__
void Orbiter::BroadcastBufferedKeyboardEvent (char *kstate, DIDEVICEOBJECTDATA *dod, DWORD n)
#else // __linux__
void Orbiter::BroadcastBufferedKeyboardEvent (char *kstate, KeyData *dod, DWORD n)
#endif // __linux__
{
	for (DWORD i = 0; i < n; i++) {
		bool consume = false;
		if (!(dod[i].dwData & 0x80)) continue; // only process key down events
		DWORD key = dod[i].dwOfs;

		for (auto it = m_Plugin.begin(); it != m_Plugin.end(); it++)
			if (it->pModule && it->pModule->clbkProcessKeyboardBuffered(key, kstate, bRunning))
				consume = true;

		if (consume) dod[i].dwData = 0; // remove key from process queue
	}
}

//-----------------------------------------------------------------------------
// Name: MsgProc()
// Desc: Render window message handler
//-----------------------------------------------------------------------------

#ifndef __linux__
LRESULT Orbiter::MsgProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
#else // __linux__
// mouse key state flags (the WPARAM of the mouse messages)
static DWORD MouseKeyState (const QSinglePointEvent *e)
#endif // __linux__
{
#ifndef __linux__
	if (ImGui_ImplWin32_WndProcHandler(hWnd, uMsg, wParam, lParam))
		return 0;
#else // __linux__
	DWORD state = 0;
	if (e->buttons() & Qt::LeftButton)       state |= MK_LBUTTON;
	if (e->buttons() & Qt::RightButton)      state |= MK_RBUTTON;
	if (e->buttons() & Qt::MiddleButton)     state |= MK_MBUTTON;
	if (e->modifiers() & Qt::ShiftModifier)   state |= MK_SHIFT;
	if (e->modifiers() & Qt::ControlModifier) state |= MK_CONTROL;
	return state;
}

// a logical key of the keymap: the desktop's shortcut for it is not passed on (WlShortcuts)
static bool IsKeymapKey (const Keymap &keymap, KeyboardDevice *kbd, const QKeyEvent *e)
{
	char kstate[256];
	DWORD dik = KeyboardDevice::DIKCode ((int)e->nativeScanCode() - 8);
	if (!dik) return false;
	if (kbd->GetDeviceState (256, kstate) != DI_OK) { // not acquired yet: any side of a held modifier
		memset (kstate, 0, 256);
		if (e->modifiers() & Qt::ShiftModifier)   kstate[OAPI_KEY_LSHIFT]   = kstate[OAPI_KEY_RSHIFT]   = (char)0x80;
		if (e->modifiers() & Qt::ControlModifier) kstate[OAPI_KEY_LCONTROL] = kstate[OAPI_KEY_RCONTROL] = (char)0x80;
		if (e->modifiers() & Qt::AltModifier)     kstate[OAPI_KEY_LALT]     = kstate[OAPI_KEY_RALT]     = (char)0x80;
	}
	for (int i = 0; i < LKEY_COUNT; i++) {
		DWORD key = dik;
		if (keymap.IsLogicalKey (key, kstate, i, false)) return true;
	}
	return false;
}
#endif // __linux__

#ifndef __linux__
	switch (uMsg) {
#else // __linux__
bool Orbiter::MsgProc (QWindow *hWnd, QEvent *event)
{
	if (hWnd != hRenderWnd) return false; // session closed, the window is on its way out (DefWindowProc)
#endif // __linux__

#ifndef __linux__
	case WM_ACTIVATE:
		bActive = (wParam != WA_INACTIVE);
		return 0;
#else // __linux__
	// DirectInput read the keyboard beside the message queue; here the keyboard device takes every key event first
	if ((event->type() == QEvent::KeyPress || event->type() == QEvent::KeyRelease) && GetKbdDevice()) {
		QKeyEvent *ke = static_cast<QKeyEvent*>(event);
		bool press = (event->type() == QEvent::KeyPress);
		if (WlShortcutsKey (ke, press && IsKeymapKey (keymap, GetKbdDevice(), ke)))
			return true; // the desktop's shortcut (KDE Plasma): passed on, as the desktop would have taken it
		GetKbdDevice()->KeyEvent ((int)ke->nativeScanCode() - 8, press); // xkb keycode -> evdev
	}
	if (event->type() == QEvent::MouseButtonPress || event->type() == QEvent::FocusIn || event->type() == QEvent::FocusOut)
		WlShortcutsReset ();

	if (ImGui_ImplQt_EventHandler(hWnd, event))
		return true;

	qreal dpr = hWnd->devicePixelRatio(); // client coordinates are device pixels

	switch (event->type()) {

	case QEvent::FocusIn:  // WM_ACTIVATE
	case QEvent::FocusOut:
		bActive = (event->type() == QEvent::FocusIn);
		if (!bActive && GetKbdDevice()) GetKbdDevice()->Unacquire(); // DISCL_FOREGROUND: the device lets go with the focus
		return false;
#endif // __linux__

	// *** User Keyboard Input ***
#ifndef __linux__
	case WM_CHAR:
	case WM_KEYDOWN: {
#else // __linux__
	case QEvent::KeyPress: { // WM_CHAR, WM_KEYDOWN
#endif // __linux__
		ImGuiIO& io = ImGui::GetIO();
		bool imguiWantsKbd = io.WantCaptureKeyboard &&
			(pConfig->CfgUIPrm.MouseFocusMode == 0 || io.WantCaptureMouse);
		if (imguiWantsKbd) {
#ifndef __linux__
			return 0;
#else // __linux__
			return true;
#endif // __linux__
		}
		} break;

	// Mouse event handler
#ifndef __linux__
	case WM_LBUTTONDOWN:
	case WM_RBUTTONDOWN:
	case WM_LBUTTONUP:
	case WM_RBUTTONUP: {
#else // __linux__
	case QEvent::MouseButtonPress:
	case QEvent::MouseButtonRelease: {
		QMouseEvent *me = static_cast<QMouseEvent*>(event);
		UINT uMsg;
		if      (me->button() == Qt::LeftButton)  uMsg = (event->type() == QEvent::MouseButtonPress ? WM_LBUTTONDOWN : WM_LBUTTONUP);
		else if (me->button() == Qt::RightButton) uMsg = (event->type() == QEvent::MouseButtonPress ? WM_RBUTTONDOWN : WM_RBUTTONUP);
		else break; // other buttons: DefWindowProc

#endif // __linux__
		if (ImGuiIO& io = ImGui::GetIO(); io.WantCaptureMouse) {
#ifndef __linux__
			return 0;
#else // __linux__
			return true;
#endif // __linux__
		}

#ifndef __linux__
		if (MouseEvent(uMsg, wParam, LOWORD(lParam), HIWORD(lParam)))
#else // __linux__
		if (MouseEvent(uMsg, MouseKeyState(me), (DWORD)(me->position().x()*dpr), (DWORD)(me->position().y()*dpr)))
#endif // __linux__
			break; //return 0;
		} break;
#ifndef __linux__
	case WM_MOUSEWHEEL: {
#else // __linux__
	case QEvent::Wheel: { // WM_MOUSEWHEEL
#endif // __linux__
		if (ImGuiIO& io = ImGui::GetIO(); io.WantCaptureMouse) {
#ifndef __linux__
			return 0;
#else // __linux__
			return true;
#endif // __linux__
		}

#ifndef __linux__
		int x = LOWORD(lParam);
		int y = HIWORD(lParam);
		if (!bFullscreen) {
			POINT pt = { x, y };
			ScreenToClient(&pt); // for some reason this message passes screen coordinates
			x = pt.x;
			y = pt.y;
		}
		if (MouseEvent(uMsg, wParam, x, y))
#else // __linux__
		QWheelEvent *we = static_cast<QWheelEvent*>(event);
		int x = (int)(we->position().x()*dpr); // Qt passes client coordinates, the Win32 message passed screen coordinates
		int y = (int)(we->position().y()*dpr);
		DWORD state = MAKEWPARAM (MouseKeyState(we), (WORD)(short)we->angleDelta().y()); // HIWORD: wheel delta, 120 per notch
		if (MouseEvent(WM_MOUSEWHEEL, state, x, y))
#endif // __linux__
			break; //return 0;
		} break;
#ifndef __linux__
	case WM_MOUSEMOVE: {
#else // __linux__
	case QEvent::MouseMove: { // WM_MOUSEMOVE
			QMouseEvent *me = static_cast<QMouseEvent*>(event);
#endif // __linux__
			// Focus-follows-mouse: must run before the WantCaptureMouse early-out,
			// otherwise moving the mouse over an ImGui window (which sets
			// WantCaptureMouse) would prevent focus from returning to the
			// render window, breaking the "focus follows mouse" setting.
#ifndef __linux__
			if (!bKeepFocus && pConfig->CfgUIPrm.MouseFocusMode != 0 && GetFocus() != hWnd) {
				if (GetWindowThreadProcessId(hWnd, NULL) == GetWindowThreadProcessId(GetFocus(), NULL))
					SetFocus(hWnd);
			}
#else // __linux__
			// GetWindowThreadProcessId check: only take the focus back from another window of this application
			QWindow *focus = QGuiApplication::focusWindow();
			if (!bKeepFocus && pConfig->CfgUIPrm.MouseFocusMode != 0 && focus && focus != hWnd)
				hWnd->requestActivate(); // SetFocus
#endif // __linux__

			if (ImGuiIO& io = ImGui::GetIO(); io.WantCaptureMouse) {
#ifndef __linux__
				return 0;
#else // __linux__
				return true;
#endif // __linux__
			}

#ifndef __linux__
			int x = LOWORD(lParam);
			int y = HIWORD(lParam);
			MouseEvent(uMsg, wParam, x, y);
#else // __linux__
			int x = (int)(me->position().x()*dpr);
			int y = (int)(me->position().y()*dpr);
			MouseEvent(WM_MOUSEMOVE, MouseKeyState(me), x, y);
#endif // __linux__
		}
#ifndef __linux__
		return 0;
#else // __linux__
		return true;
#endif // __linux__

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

#ifndef __linux__
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

	case WM_NCHITTEST:
        // Prevent the user from selecting the menu in fullscreen mode
        if (IsFullscreen()) return HTCLIENT;
        break;
#else // __linux__
	// WM_GETMINMAXINFO: the minimum size is set on the window in CreateRenderWindow
	// WM_POWERBROADCAST: SleepWatch (logind PrepareForSleep) calls Freeze
	// WM_COMMAND (SC_MONITORPOWER, IDM_EXIT) and WM_NCHITTEST left out: the render window has no menu or system commands
#endif // __linux__

		// shutdown options
#ifndef __linux__
	case WM_CLOSE:
#else // __linux__
	case QEvent::Close: // WM_CLOSE
#endif // __linux__
		PreCloseSession();
#ifndef __linux__
		DestroyWindow (hWnd);
		return 0;
#else // __linux__
		DestroyRenderWindow (hWnd); // DestroyWindow; WM_DESTROY -> CloseSession
		return true;
#endif // __linux__

#ifndef __linux__
	case WM_DESTROY:
		CloseSession ();
        break;
#else // __linux__
	default:
		break;
#endif // __linux__
	}
#ifndef __linux__
    return DefWindowProc (hWnd, uMsg, wParam, lParam);
#else // __linux__
    return false; // DefWindowProc: Qt's default handling
#endif // __linux__
}

//-----------------------------------------------------------------------------
// Name: ActivateRoughType()
// Desc: Suppress font smoothing
//-----------------------------------------------------------------------------
bool Orbiter::ActivateRoughType ()
{
	//if (!bSysClearType) return false; // ClearType isn't user-enabled anyway
	if (bRoughType) return false; // active already

#ifndef __linux__
	BOOL cleartype;
	BOOL ok = SystemParametersInfo (SPI_GETFONTSMOOTHING, 0, &cleartype, 0);
	if (!ok) return false; // ClearType status can't be determined
	if (!cleartype || SystemParametersInfo (SPI_SETFONTSMOOTHING, FALSE, NULL, SPIF_SENDCHANGE)) {
		bRoughType = true;
		return true;
	} else return false;
#else // __linux__
	// SPI_SETFONTSMOOTHING left out: no desktop setting changes; the client drops smoothing per font while the flag is set
	bRoughType = true;
	return true;
#endif // __linux__
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
#ifndef __linux__
	if (SystemParametersInfo (SPI_SETFONTSMOOTHING, TRUE, NULL, SPIF_SENDCHANGE)) {
		bRoughType = false;
		return true;
	} else return false;
#else // __linux__
	bRoughType = false; // SystemParametersInfo (SPI_SETFONTSMOOTHING) left out, see ActivateRoughType
	return true;
#endif // __linux__
}

//-----------------------------------------------------------------------------
// Name: AttachGraphicsClient()
// Desc: Link an external graphics render interface
//-----------------------------------------------------------------------------
bool Orbiter::AttachGraphicsClient (oapi::GraphicsClient *gc)
{
	if (gclient) return false; // another client is already attached
	register_module = gc;
	gclient = gc;
	//gclient->clbkInitialise(); // Cannot initialize with-in DllMain() will result DirectX device failure.
	return true;
}

//-----------------------------------------------------------------------------
// Name: RemoveGraphicsClient()
// Desc: Unlink an external graphics render interface
//-----------------------------------------------------------------------------
bool Orbiter::RemoveGraphicsClient (oapi::GraphicsClient *gc)
{
	if (!gclient || gclient != gc) return false; // no client attached
	gclient = NULL;
	return true;
}

#ifndef __linux__
bool Orbiter::RegisterWindow (HINSTANCE hInstance, HWND hWnd, DWORD flag)
#else // __linux__
bool Orbiter::RegisterWindow (void *hInstance, QWidget *hWnd, DWORD flag)
#endif // __linux__
{
#ifndef __linux__
	return (pDlgMgr ? (pDlgMgr->AddWindow (hInstance, hWnd, hRenderWnd, flag) != NULL) : NULL);
#else // __linux__
	return (pDlgMgr ? (pDlgMgr->AddWindow (hInstance, hWnd, hRenderWnd, flag) != NULL) : false);
#endif // __linux__
}

void Orbiter::UpdateDeallocationProgress()
{
	m_pLaunchpad->UpdateWaitProgress();
}

#ifndef __linux__
HWND Orbiter::OpenDialog (int id, DLGPROC pDlg, void *context)
#else // __linux__
QWidget *Orbiter::OpenDialog (int id, DLGINIT pDlg, void *context)
#endif // __linux__
{
	return OpenDialog (hInst, id, pDlg, context);
}

#ifndef __linux__
HWND Orbiter::OpenDialogEx (int id, DLGPROC pDlg, DWORD flag, void *context)
#else // __linux__
QWidget *Orbiter::OpenDialogEx (int id, DLGINIT pDlg, DWORD flag, void *context)
#endif // __linux__
{
	return OpenDialogEx (hInst, id, pDlg, flag, context);
}

#ifndef __linux__
HWND Orbiter::OpenDialog (HINSTANCE hInstance, int id, DLGPROC pDlg, void *context)
#else // __linux__
QWidget *Orbiter::OpenDialog (void *hInstance, int id, DLGINIT pDlg, void *context)
#endif // __linux__
{
	return (pDlgMgr ? pDlgMgr->OpenDialog (hInstance, id, hRenderWnd, pDlg, context) : NULL);
}

#ifndef __linux__
HWND Orbiter::OpenDialogEx (HINSTANCE hInstance, int id, DLGPROC pDlg, DWORD flag, void *context)
#else // __linux__
QWidget *Orbiter::OpenDialogEx (void *hInstance, int id, DLGINIT pDlg, DWORD flag, void *context)
#endif // __linux__
{
	return (pDlgMgr ? pDlgMgr->OpenDialogEx (hInstance, id, hRenderWnd, pDlg, flag, context) : NULL);
}

void Orbiter::OpenHelp (const HELPCONTEXT *hcontext)
{
	if (pDlgMgr) {
		DlgHelp *pHelp = pDlgMgr->EnsureEntry<DlgHelp> ();
		pHelp->OpenHelp(hcontext);
	}
}

void Orbiter::OpenLaunchpadHelp (HELPCONTEXT *hcontext)
{
	::OpenHelp (0, hcontext->helpfile, hcontext->topic);
}

HELPCONTEXT Orbiter::DefaultHelpPage(const char* topic)
{
	static HELPCONTEXT hcontext = DefHelpContext;
	hcontext.topic = (char*)topic;
	return hcontext;
}

#ifndef __linux__
void Orbiter::CloseDialog (HWND hDlg)
#else // __linux__
void Orbiter::CloseDialog (QWidget *hDlg)
#endif // __linux__
{
	if (pDlgMgr) pDlgMgr->CloseDialog (hDlg);
}

#ifndef __linux__
HWND Orbiter::IsDialog (HINSTANCE hInstance, DWORD resId)
#else // __linux__
QWidget *Orbiter::IsDialog (void *hInstance, DWORD resId)
#endif // __linux__
{
	return (pDlgMgr ? pDlgMgr->IsEntry (hInstance, resId) : NULL);
}

//=============================================================================
// Nonmember functions
//=============================================================================

#ifndef __linux__
INT_PTR CALLBACK BkMsgProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
#else // __linux__
// WM_SIZE of the demo background: the image fills the window
static void BkMsgProc (QWidget *hDlg)
#endif // __linux__
{
#ifndef __linux__
	switch (uMsg) {
	case WM_SIZE: {
		RECT r;
		GetWindowRect (hDlg, &r);
		MoveWindow (GetDlgItem (hDlg, IDC_IMG), 0, 0, r.right, r.bottom, TRUE);
		} return 1;
	}
	return 0;
#else // __linux__
	new EventHook (hDlg, [hDlg](QObject *obj, QEvent *event) {
		if (obj == hDlg && event->type() == QEvent::Resize) {
			if (QWidget *img = oapiResDlgItem (hDlg, IDC_IMG))
				img->setGeometry (0, 0, hDlg->width(), hDlg->height()); // MoveWindow
		}
		return false;
	});
#endif // __linux__
}
