// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// =============================================================
// Config.cpp
// Implementation of class Config
// Load configuration settings for Orbiter
// =============================================================

#define __CONFIG_CPP
#define STRICT 1

#include <fstream>
#include <iomanip>
#include <limits>
#include <string.h>
#include <stdio.h>
#include "Config.h"
#include "Astro.h"
#include "Log.h"
#include "VectorMap.h"
#include "GraphicsAPI.h"
#include "resource.h"

using namespace std;

static char g_cbuf[1024];
static int g_buflen = 1024;

// =============================================================
// list of default parameters

const bool bEchoAll_default = false;          // only echo non-default parameters

CFG_DIRPRM CfgDirPrm_default = {
	".\\Config\\",		// ConfigDir
	".\\Meshes\\",		// MeshDir
	".\\Textures\\",	// TextureDir
	".\\Textures2\\",	// HightexDir
	".\\Textures\\",	// PlanetTexDir
	".\\Scenarios\\"	// ScnDir
};

CFG_PHYSICSPRM CfgPhysicsPrm_default = {
	false,		// bDistributedMass (no gravity gradient torque effect)
	false,		// bNonsphericalGrav (no nonspherical gravity effects)
	false,		// bRadiationPressure (no radiation pressure effects)
	false,      // bAtmWind (enable wind effects)
	true,       // bBaseCollision (enable surface base collision)
	true,       // bVesselCollision (enable vessel collision)
	true,		// bOrbitStabilise (use Encke orbit stabilisation)
	0.05,		// Stabilise_PLimit (perturbation limit for stabilisation)
	0.01,		// Stabilise_SLimit (step size limit for stabilisation)
	0.02,		// PPropSubLimit (orbit step target for perturbation subsampling)
	10,			// PPropSubMax (max number of subsampling steps for perturbation integration)
	0.05,		// PPropStepLimit (orbit step limit for nonspherical gravity suppression)
	4,			// nLPropLevel (number of linear propagator definitions)
	{PROP_RK2,PROP_RK4,PROP_RK6,PROP_RK8,PROP_RK8},	// LPropMode (linear propagator methods)
	{0.1,  2.0,  20.0, 200, 500},                   // PropTTgt (time step targets for the propagation levels)
	{0.2*RAD, 2*RAD, 5*RAD, 20*RAD, 50*RAD},        // PropATgt (angle step targets for the propagation levels)
	{0.5, 10.0, 100.0, 1e10, 1e10},					// PropTLimit (time step limits for the propagation levels)
	{1.0*RAD, 4.0*RAD, 10.0*RAD, 1e10, 1e10},		// PropALimit (angle limits for angular propagation levels)
	20.0*RAD,	// APropSubLimit (angle step limit for angular subsampling)
	10, 		// PropSubMax (max number of subsampling steps)
	30.0*RAD,	// APropCouplingLimit (angle step limit for cross term suppresion)
	3600.0*RAD	// APropTorqueLimit (angle step limit for torque suppression)
};

CFG_LOGICPRM CfgLogicPrm_default = {
	false,		// bStartPaused (don't start paused)
	1,			// FlightModelLevel (use realistic flight model)
	0,			// DamageSetting (no vessel damage)
	true,		// bLimitedFuel (fuel is being consumed)
	true,		// bPadRefuel (auto-refuel on pad)
	false,		// bMfdTransparent (no transparent glass cockpit MFDs)
	true,       // bGlasspitCompact (compact layout for widescreen glass cockpit)
	6,			// MFDSize (default glass cockpit MFD size [1-10])
	1,			// MFDMapVersion (new style map MFD mode)
	0.5,		// InstrUpdDT (MFD update interval [s])
	1.0,		// PanelScale (old-style 2D instrument panel scale)
	300.0		// PanelScrollSpeed (scrolling speed for 2D instrument panel [pixel/s])
};

CFG_VISUALPRM CfgVisualPrm_default = {
	true,		// bShadows (generate shadows)
	true,		// bVesselShadows (generate vessel shadows)
	true,		// bClouds (enable cloud layers)
	false,		// bCloudShadows (no cloud shadows)
	true,		// bNightlights (enable city lights)
	true,		// bWaterreflect (specular reflection from water surfaces)
	false,		// bSpecularRipple (no ripple effects on water surfaces)
	true,		// bHaze (enable horizon haze effects)
	true,		// bFog (enable distance fog effects)
	true,		// bSpecular (enable specular reflections from objects)
	true,		// bReentryFlames (enable reentry flame effects)
	true,		// bParticleStreams (enable particle streams)
	false,		// bLocalLight (disable local light sources)
	32,			// MaxLight (max number of light sources, 0=max supported by device)
	0x02,		// AmbientLevel (default ambient light level)
	SURF_MAX_PATCHLEVEL2, // PlanetMaxLevel (max surface resolution level)
	1.0,		// PlanetPatchRes (resolution level scale parameter)
	0.5,		// LightBrightness (city light brightness)
	true,       // bUseStarDots (render background stars as pixels)
	{2.0, 8.0, 0.1, true},	// StarPrm (bright/faint cutoff magnitude, display brightness of faintest, log mapping)
	true,       // bUseStarImage (render background star from image)
	"csphere\\hiptyc_2020",     // StarImagePath (path to star background image)
	true,       // bUseBgImage (render celestial sphere background image)
	"csphere\\milkyway_2020",   // CSphereBgPath (path to celestial background images)
	0.3,		// CSphereBgIntens (intensity of celestial sphere background image)
	2			// ElevMode (cubic spline)
};

CFG_CAPTUREPRM CfgCapturePrm_default = {
	0,          // Write screenshots to clipboard by default
	"capture\\images\\0000",     // screenshot outpuf file name
	"capture\\frames",           // output directory for frame sequence
	2,          // image file format (JPG)
	7,          // image quality setting (1-10)
	0,          // starting frame for sequence
	0           // number of frames to skip
};

CFG_INSTRUMENTPRM CfgInstrumentPrm_default = {
	2,          // bMfdPow2 (auto from driver caps)
	384,        // MfdHiresThreshold (switch to 512x512 at this size if pow2 is active)
	512,		// PanelMFDHUDSize
	512         // VCMfdSize
};

CFG_VISHELPPRM CfgVisHelpPrm_default = {
	PLN_CGRID | PLN_CONST | PLN_CNSTLABEL | PLN_CNSTLONG,	// flagPlanetarium (celestial sphere display flags)
	MKR_CMARK,  // flagMarkers (surface and body marker display flags)
	BFV_WEIGHT | BFV_THRUST | BFV_LIFT | BFV_DRAG,	// flagBodyforce (force display flags)
	1.0f,		// scaleBodyforce (force vector scaling factor)
	1.0f,		// opacBodyforce (force vector opacity)
	FAV_VESSEL,	// flagCrdAxes (frame axis display flags)
	1.0f,		// scaleCrdAxes (axis scaling factor)
	1.0f		// opacCrdAxes (axis opacity)
};

CFG_DEBUGPRM CfgDebugPrm_default = {
	0,			// ShutdownMode (0=deallocate memory)
	0.0,		// fixed time step (0=variable)
	false,      // bDisableSmoothFont (don't disable font smoothing)
	false,      // bForceReenableSmoothFont (don't force reenabling font smoothing on exit)
	2,          // bHtmlScnDesc (use html browser window for scenario desciption in launchpad)
	true,       // bSaveExitScreen (capture screen on scenario exit)
	false,      // bWireframeMode (don't set renderer to wireframe mode)
	false,      // bNormaliseNormals (don't auto-normalise all normals)
	false       // bVerboseLog (no verbose log output)
};

CFG_PLANETRENDERPRM CfgPRenderPrm_default = {
	0,			// preload mode (0=load on demand)
	40,         // CacheSize (number of tiles cached by TilePool class)
	true,       // bLoadOnThread (load tiles in separate thread)
	1,			// mipmap mode (1=point sampling)
	0.0,		// mipmap bias
	0.0,        // resolution bias
	5,          // patch mesh resolution power
	50,			// load frequency (Hz)
	3,			// aniso mode (1=none)
	0x0003      // TileLoadFlags (load from individual tile files + compressed archives)
};

CFG_MAPPRM CfgMapPrm_default = {
	DISP_GRIDLINE | DISP_COASTLINE | DISP_CONTOURS | DISP_BASE | DISP_VESSEL | DISP_ORBITFOCUS | DISP_ORBITSEL | DISP_ORBITPLANE // display flags
};

CFG_RECPLAYPRM CfgRecPlayPrm_default = {
	1,			// RecordPosFrame (equatorial frame for recording position streams)
	1,			// RecordAttFrame (local horizon frame for recording attitude streams)
	true,		// bRecordWarp (record time acceleration events?)
	true,		// bRecordFocus (record focus events?)
	true,		// bReplayWarp (replay time acceleration events?)
	true,		// bReplayFocus (replay focus events?)
	true,		// bReplayCam (replay camera events?)
	true,		// bSysInterval (use system time for sampling intervals?)
	true		// bShowNotes (show playback onscreen annotations?)
};

CFG_DEVPRM CfgDevPrm_default = {
	-1,	        // Device_idx (-1=undefined)
	0,			// Device_mode
	0,
	0,
	true,		// bForceEnum (enumerate devices at each simulation start)
	false,		// bFullscreen (default to window mode)
	false,		// bStereo (no stereo support)
	false,		// bNoVsync (enable vsync)
	false,		// bTryStencil (don't enable stencil buffering)
	true,		// bPageflip (allow page flipping in fullscreen)
	800,		// WinW (render window width - will be overridden with screen width
	600			// WinH (render window height - will be overridden with screen height
};

CFG_JOYSTICKPRM CfgJoystickPrm_default = {
	0,			// Joy_idx (joystick device index, 0=disabled)
	2500,		// Deadzone (neutralise joystick axes within 20% of central position)
	1,			// ThrottleAxis (z-axis by default)
	9500,		// ThrottleSaturation (saturate throttle at the last 5% each end)
	true		// bThrottleIgnore (ignore throttle setting on simulation start)
};

CFG_UIPRM CfgUIPrm_default = {
	1,			// MouseFocusMode (click required for child windows)
	2,          // MenuMode (auto-hide menu bar)
	35,         // MenuButtonSize
	35,         // MenuButtonHoverSize
	18,          // MenuButtonSpacing
	false,      // bMenuLabelAlways (always display labels)
	true,       // bWarpAlways (always display time acceleration != 1)
	0,          // InfoMode (show info bars)
	0,          // FPS (0=hidden, 1=on the left, 2=on the right)
	2,          // MenuOpacity (menubar opacity, 0-10)
	2,          // InfoOpacity (infobar opacity, 0-10)
	10,         // MenuScrollspeed (1-20)
	0,          // SelVesselTab: tab to open in vessel selection dialog
	4,          // SelVesselRange "nearby" range for vessel selection dialog (100km)
	false       // bSelVesselFlat (no flat assemblies) in vessel selection dialog)
};

CFG_DEMOPRM CfgDemoPrm_default = {
	false,		// bDemo (don't start in demo mode)
	false,		// bBkImage (don't cover desktop with background image in demo mode)
	false,		// bBlockExit (don't disable exit button in demo mode)
	300.0,		// MaxDemoTime (max. runtime for a demo simulation [s])
	15.0		// LPIdleTime (max demo idle time in launchpad window [s])
};

CFG_FONTPRM CfgFontPrm_default = {
	1.0f,		// dlgFont_Scale (scaling factor for inline dialog fonts)
	"Arial",	// dlgFont1_Face (default dialog font face name)
	14.0f,		// ImGui_FontSize
	"Fonts/Roboto-Medium.ttf",    // ImGui_DefaultFontFile
	"Fonts/Cousine-Regular.ttf",  // ImGui_ConsoleFontFile
	"Fonts/Lekton-Bold.ttf",      // ImGui_MonospacedFontFile
	"Fonts/architext.regular.ttf" // ImGui_ManuscriptFontFile
};

CFG_CAMERAPRM CfgCameraPrm_default = {
	100.0,		// Panspeed (camera panning speed)
	50.0,       // altitude limit for terrain-following mode [m]
	0			// HUDCol (green HUD colour)
};

CFG_CMDLINEPRM CfgCmdlinePrm_default = {
	false,              // fast exit (false = return to Launchpad dialog)
	false,              // read config from stdin (false = read from Orbiter.cfg)
	false,              // add config from stdin (false = don't allow override from stdin)
	false,              // don't overwrite Orbiter.cfg (false = allow overwrite)
	false,              // write config to stdout (false = write to Orbiter.cfg)
	false,              // force start paused (false = don't force)
	false,              // force start unpaused (false = don't force)
	false,              // open video tab (false = open Scenarios tab)
	false,              // append to log file (false = overwrite)
	false,              // disable graphics (false = don't disable)
	0,                  // FrameLimit (0 = unlimited)
	0.0,                // fixed time step length (0 = disabled)
	0.0,                // Max sys time (0 = unlimited)
	0.0,                // Max sim time (0 = unlimited)
	std::string(),      // launch scenario (empty: open Launchpad dialog)
	std::list<std::string>() // list of plugins to load
};

CFG_WINDOWPOS CfgWindowPos_default = {
	{0,0,0,0},  // map window
	{0,0,0,0},  // info window
	{0,0,0,0},  // camera dialog
	{0,0,0,0},  // focus dialog
	{0,0,0,0},  // time acceleration dialog
	{0,0,0,0},  // options dialog
	{0,0,0,0},  // visual helper dialog
	0,          // launchpad scenario list width
	0,          // launchpad modules list width
	0           // launchpad extras list width
};

// =============================================================

ostream &operator<< (ostream &out, const RECT &r)
{
	out << r.left << ' ' << r.top << ' ' << r.right << ' ' << r.bottom;
	return out;
}

bool operator!= (const RECT &r1, const RECT &r2)
{
	return (r1.left != r2.left || r1.top != r2.top || r1.right != r2.right || r1.bottom != r2.bottom);
}

const char *BoolStr (bool cond)
{
	static const char *Fstr = "FALSE";
	static const char *Tstr = "TRUE";
	return (cond ? Tstr : Fstr);
}

char *trim_string (char *cbuf)
{
	char *c;

	// strip comments starting with ';'
	for (c = cbuf; *c; c++) {
		if (*c == ';') {
			*c = '\0';
			break;
		}
	}
	// strip trailing white space
	for (--c; c >= cbuf; c--) {
		if (*c == ' ' || *c == '\t') *c = '\0';
		else break;
	}
	// skip leading white space
	for (c = cbuf; *c; c++)
		if (*c != ' ' && *c != '\t') return c;

	// should never get here
	return c;
}

char *readline (std::istream &is)
{
	const int inc = 256;
	static int len = 256;
	static char *cbuf = new char[len];

	char *c = cbuf;
	cbuf[0] = '\0';
	int chunk = len;
	for (;;) {
		if (is.getline (c, chunk)) return cbuf;
		else if (is.eof()) return 0;
		else { // buffer too small
			char *tmp = new char[len+inc];
			memcpy (tmp, cbuf, len-1);
			delete []cbuf;
			cbuf = tmp;
			c = cbuf+len-1;
			len += inc;
			chunk = inc+1;
		}
		is.clear();
	}
	return 0; // never gets here
}

// Reads one line like getline, but a line too long for buf is cut off and the rest of it skipped
static bool getline_trunc (istream &is, char *buf, int size)
{
	if (is.getline (buf, size)) return true;
	if (is.eof() || is.bad()) return false;
	is.clear();
	is.ignore (numeric_limits<streamsize>::max(), '\n');
	return true;
}

bool GetItemString (istream &is, const char *label, char *val)
{
	return GetItemString (is, label, val, 512);
}

bool GetItemString (istream &is, const char *label, char *val, size_t size)
{
	char cbuf[512], *cl, *cv;
	int i;

	is.clear();
	is.seekg (0, ios::beg);

	while (getline_trunc (is, cbuf, 512)) {
		cl = trim_string(cbuf);
		if (!_stricmp(cl, "END_PARSE")) return false;
		
		for (i = 0; cl[i] && cl[i] != '='; i++);
		cv = (cl[i] ? cl+(i+1) : cl+i);
		for (cl[i--] = '\0'; i >= 0 && (cl[i] == ' ' || cl[i] == '\t'); i--)
			cl[i] = '\0';
		if (!_stricmp (cl, label)) {
			while (*cv == ' ' || *cv == '\t') cv++;
			if (*cv) {
				snprintf (val, size, "%s", cv);
				return true;
			} else {
				return false;
			}
		}
	}

	is.clear();
	return false;
}

bool GetItemReal (std::istream &is, const char *label, double &val)
{
	if (!GetItemString (is, label, g_cbuf)) return false;
	return (sscanf (g_cbuf, "%lf", &val) == 1);
}

bool GetItemInt (std::istream &is, const char *label, int &val)
{
	if (!GetItemString (is, label, g_cbuf)) return false;
	return (sscanf (g_cbuf, "%d", &val) == 1);
}

bool GetItemSize(std::istream& is, const char* label, size_t& val)
{
	if (!GetItemString(is, label, g_cbuf)) return false;
	return (sscanf(g_cbuf, "%zu", &val) == 1);
}

bool GetItemHex (std::istream &is, const char *label, int &val)
{
	if (!GetItemString (is, label, g_cbuf)) return false;
	return (sscanf (g_cbuf, "%x", &val) == 1);
}

bool GetItemBool (std::istream &is, const char *label, bool &val)
{
	if (!GetItemString (is, label, g_cbuf)) return false;
	if (!_strnicmp (g_cbuf, "true", 4)) { val = true; return true; }
	else if (!_strnicmp (g_cbuf, "false", 5)) { val = false; return true; }
	return false;
}

bool GetItemVector (std::istream &is, const char *label, Vector &val)
{
	double x, y, z;
	if (!GetItemString (is, label, g_cbuf)) return false;
	if (sscanf (g_cbuf, "%lf%lf%lf", &x, &y, &z) != 3) return false;
	val.Set (x,y,z);
	return true;
}

bool GetItemVECTOR (std::istream &is, const char *label, VECTOR3 &val)
{
	double x, y, z;
	if (!GetItemString (is, label, g_cbuf)) return false;
	if (sscanf (g_cbuf, "%lf%lf%lf", &x, &y, &z) != 3) return false;
	val.x = x; val.y = y; val.z = z;
	return true;
}

bool FindLine (std::istream &is, const char *line)
{
	bool ok = false;
	is.seekg (0); // rewind stream
	if (is.good()) {
		int len = strlen(line);
		for (;;) {
			if (!is.getline (g_cbuf, g_buflen)) {
				if (is.eof()) break;               // EOF
				else is.clear();                   // heal stream to continue after truncation error
			}
			if (!_strnicmp (g_cbuf, line, len)) {  // found string
				ok = true;
				break;
			}
		}
	}
	if (!ok) { // reset stream
		is.clear();
		is.seekg(0);
	}
	return ok;
}

int ListIndex (int listlen, char **list, char *label)
{
	for (int i = 0; i < listlen; i++)
		if (!_stricmp (label, list[i])) return i;
	return -1;
}

// =============================================================

Config::Config()
{
	SetDefaults ();
}

Config::Config(const std::string &fname)
{
	SetDefaults();
	Read(fname);
}

bool Config::Read(const std::string &fname)
{
	m_sRoot = fname;

	ifstream ifs(fname);
	if (!ifs) return false;

	found_config_file = true;
	return Read(ifs);
}

bool Config::Read(std::istream &is)
{
	int i;
	double d;
	bool b;
	char cbuf[256], tag[256];

	GetBool (is, "EchoAllParams", bEchoAll);

	// configuration directory
	if (GetString (is, "ConfigDir", CfgDirPrm.ConfigDir, sizeof(CfgDirPrm.ConfigDir)-1))
		if (CfgDirPrm.ConfigDir[strlen(CfgDirPrm.ConfigDir)-1] != '\\')
			strcat (CfgDirPrm.ConfigDir, "\\");
	strcpy (cfgpath, CfgDirPrm.ConfigDir); cfglen = strlen (cfgpath);

	// mesh directory
	if (GetString (is, "MeshDir", CfgDirPrm.MeshDir, sizeof(CfgDirPrm.MeshDir)-1))
		if (CfgDirPrm.MeshDir[strlen(CfgDirPrm.MeshDir)-1] != '\\')
			strcat (CfgDirPrm.MeshDir, "\\");
	strcpy (mshpath, CfgDirPrm.MeshDir);   mshlen = strlen (mshpath);

	// texture directory
	if (GetString (is, "TextureDir", CfgDirPrm.TextureDir, sizeof(CfgDirPrm.TextureDir)-1))
		if (CfgDirPrm.TextureDir[strlen(CfgDirPrm.TextureDir)-1] != '\\')
			strcat (CfgDirPrm.TextureDir, "\\");
	strcpy (texpath, CfgDirPrm.TextureDir);  texlen = strlen (texpath);

	// highres texture directory
	if (GetString (is, "HightexDir", CfgDirPrm.HightexDir, sizeof(CfgDirPrm.HightexDir)-1)) {
		if (CfgDirPrm.HightexDir[strlen(CfgDirPrm.HightexDir)-1] != '\\')
			strcat (CfgDirPrm.HightexDir, "\\");
		strcpy (htxpath, CfgDirPrm.HightexDir);  htxlen = strlen (htxpath);
	}

	// planetary texture directory
	if (GetString(is, "PlanetTexDir", CfgDirPrm.PlanetTexDir, sizeof(CfgDirPrm.PlanetTexDir)-1)) {
		if (CfgDirPrm.PlanetTexDir[strlen(CfgDirPrm.PlanetTexDir) - 1] != '\\')
			strcat(CfgDirPrm.PlanetTexDir, "\\");
		strcpy(ptxpath, CfgDirPrm.PlanetTexDir); ptxlen = strlen(ptxpath);
	}

	// scenario directory
	if (GetString (is, "ScenarioDir", CfgDirPrm.ScnDir, sizeof(CfgDirPrm.ScnDir)-1))
		if (CfgDirPrm.ScnDir[strlen(CfgDirPrm.ScnDir)-1] != '\\')
			strcat (CfgDirPrm.ScnDir, "\\");
	strcpy (scnpath, CfgDirPrm.ScnDir); scnlen = strlen (scnpath);

	// Device information
	GetInt  (is, "DeviceIndex", CfgDevPrm.Device_idx);
	if (GetInt (is, "ModeIndex", i)) CfgDevPrm.Device_mode = (DWORD)i;
	if (GetInt(is, "OutputIndex", i)) CfgDevPrm.Device_out = (DWORD)i;
	if (GetInt(is, "Style", i)) CfgDevPrm.Device_style = (DWORD)i;
	GetBool (is, "DeviceForceEnum", CfgDevPrm.bForceEnum);
	GetBool (is, "Fullscreen", CfgDevPrm.bFullscreen);
	GetBool (is, "Stereo", CfgDevPrm.bStereo);
	GetBool (is, "NoVSync", CfgDevPrm.bNoVsync);
	GetBool (is, "StencilBuffer", CfgDevPrm.bTryStencil);
	GetBool (is, "FullscreenPageflip", CfgDevPrm.bPageflip);
	if (GetInt (is, "WindowWidth", i))  CfgDevPrm.WinW = (DWORD)i;
	if (GetInt (is, "WindowHeight", i)) CfgDevPrm.WinH = (DWORD)i;

	// Joystick information
	if (GetInt (is, "JoystickIndex", i))
		CfgJoystickPrm.Joy_idx = (DWORD)i;
	if (GetInt (is, "JoystickThrottleAxis", i))
		CfgJoystickPrm.ThrottleAxis = max (0, min (3, i));
	if (GetInt (is, "JoystickThrottleSaturation", i))
		CfgJoystickPrm.ThrottleSaturation = max (0, min (10000, i));
	if (GetInt (is, "JoystickDeadzone", i))
		CfgJoystickPrm.Deadzone = max (0, min (10000, i));
	GetBool (is, "IgnoreThrottleOnStart", CfgJoystickPrm.bThrottleIgnore);

	// planet render parameters
	if (GetInt (is, "PlanetPreloadMode", i))
		CfgPRenderPrm.PreloadMode = max (0, min (1, i));
	if (GetInt (is, "TileCacheSize", i))
		CfgPRenderPrm.CacheSize = max (0, i);
	GetBool (is, "TileLoadThread", CfgPRenderPrm.bLoadOnThread);
	if (GetInt (is, "PlanetTexLoadFreq", i))
		CfgPRenderPrm.LoadFrequency = max (1, min (1000, i));
	if (GetInt (is, "PlanetMipmapMode", i))
		CfgPRenderPrm.MipmapMode = max(0, min(2, i));
	if (GetInt (is, "PlanetAnisoMode", i))
		CfgPRenderPrm.AnisoMode = max(1, min (16, i));
	if (GetReal (is, "PlanetMipmapBias", d))
		CfgPRenderPrm.MipmapBias = max (-1.0, min (1.0, d));
	if (GetInt (is, "PlanetPatchGrid", i))
		CfgPRenderPrm.PatchRes = max(4,min(6,i));
	if (GetReal (is, "PlanetResolutionBias", d))
		CfgPRenderPrm.ResolutionBias = max (-2.0, min (2.0, d));
	if (GetInt (is, "TileLoadFlags", i))
		CfgPRenderPrm.TileLoadFlags = max (min(i, 3), 1);

	// map dialog parameters
	if (GetInt (is, "MapDlgFlag", i))
		CfgMapPrm.DispFlag = (DWORD)i;

	// Logical parameters
	GetBool (is, "StartPaused", CfgLogicPrm.bStartPaused);
	GetInt (is, "FlightModel", CfgLogicPrm.FlightModelLevel);
	GetInt (is, "DamageModel", CfgLogicPrm.DamageSetting);
	if (GetBool (is, "UnlimitedFuel", b)) CfgLogicPrm.bLimitedFuel = !b;
	GetBool (is, "RefuelOnPad", CfgLogicPrm.bPadRefuel);
	GetBool (is, "MFDTransparent", CfgLogicPrm.bMfdTransparent);
	GetBool (is, "CompactGlasspit", CfgLogicPrm.bGlasspitCompact);
	GetInt (is, "GenericMFDSize", CfgLogicPrm.MFDSize);
	GetInt (is, "MFDMapVersion", CfgLogicPrm.MFDMapVersion);
	GetReal (is, "InstrumentUpdateInterval", CfgLogicPrm.InstrUpdDT);
	GetReal (is, "PanelScale", CfgLogicPrm.PanelScale);
	GetReal (is, "PanelScrollSpeed", CfgLogicPrm.PanelScrollSpeed);

	// Physics engine
	GetBool (is, "DistributedVesselMass", CfgPhysicsPrm.bDistributedMass);
	GetBool (is, "NonsphericalGravitySources", CfgPhysicsPrm.bNonsphericalGrav);
	GetBool (is, "RadiationPressure", CfgPhysicsPrm.bRadiationPressure);
	GetBool (is, "AtmosphericWind", CfgPhysicsPrm.bAtmWind);
	GetBool (is, "BaseCollision", CfgPhysicsPrm.bBaseCollision);
	GetBool (is, "VesselCollision", CfgPhysicsPrm.bVesselCollision);
	GetBool (is, "StabiliseOrbits", CfgPhysicsPrm.bOrbitStabilise);
	GetReal (is, "StabilisePLimit", CfgPhysicsPrm.Stabilise_PLimit);
	GetReal (is, "StabiliseSLimit", CfgPhysicsPrm.Stabilise_SLimit);
	if (GetString (is, "PertPropSubsampling", cbuf, sizeof(cbuf)))
		sscanf (cbuf, "%d%lf", &CfgPhysicsPrm.PPropSubMax, &CfgPhysicsPrm.PPropSubLimit);
	GetReal (is, "PertPropNonsphericalLimit", CfgPhysicsPrm.PPropStepLimit);
	GetInt (is, "PropStages", CfgPhysicsPrm.nLPropLevel);
	for (i = 0; i < MAX_PROP_LEVEL; i++) {
		int n, mode;
		double ttgt, atgt, tlim, alim;
		sprintf (tag, "PropStage%d", i);
		if (GetString (is, tag, cbuf, sizeof(cbuf))) {
			n = sscanf (cbuf, "%d%lf%lf%lf%lf", &mode, &ttgt, &atgt, &tlim, &alim);
			if (n >= 1 && mode >= 0 && mode < NPROP_METHOD)                     CfgPhysicsPrm.PropMode[i] = mode;
			if (n >= 2 && fabs (ttgt-CfgPhysicsPrm_default.PropTTgt[i]) > 1e-6) CfgPhysicsPrm.PropTTgt[i] = ttgt;
			if (n >= 3 && fabs (atgt-CfgPhysicsPrm_default.PropATgt[i]) > 1e-6) CfgPhysicsPrm.PropATgt[i] = atgt;
			if (n >= 4 && fabs (tlim-CfgPhysicsPrm_default.PropTLim[i]) > 1e-6) CfgPhysicsPrm.PropTLim[i] = tlim;
			if (n >= 5 && fabs (alim-CfgPhysicsPrm_default.PropALim[i]) > 1e-6) CfgPhysicsPrm.PropALim[i] = alim;
		}
	}
	CfgPhysicsPrm.PropTLim[CfgPhysicsPrm.nLPropLevel-1] = 1e10;
	CfgPhysicsPrm.PropALim[CfgPhysicsPrm.nLPropLevel-1] = 1e10;
	GetInt (is, "PropSubsampling", CfgPhysicsPrm.PropSubMax);

	// visualisation parameters
	GetBool (is, "EnableShadows", CfgVisualPrm.bShadows);
	GetBool (is, "EnableVesselShadows", CfgVisualPrm.bVesselShadows);
	GetBool (is, "EnableClouds", CfgVisualPrm.bClouds);
	GetBool (is, "EnableCloudShadows", CfgVisualPrm.bCloudShadows);
	GetBool (is, "EnableNightlights", CfgVisualPrm.bNightlights);
	GetBool (is, "EnableWaterReflection", CfgVisualPrm.bWaterreflect);
	GetBool (is, "EnableSpecularRipples", CfgVisualPrm.bSpecularRipple);
	GetBool (is, "EnableHorizonHaze", CfgVisualPrm.bHaze);
	GetBool (is, "EnableDistanceFog", CfgVisualPrm.bFog);
	GetBool (is, "EnableSpecularReflection", CfgVisualPrm.bSpecular);
	GetBool (is, "EnableReentryFlames", CfgVisualPrm.bReentryFlames);
	GetBool (is, "EnableParticleStreams", CfgVisualPrm.bParticleStreams);
	GetBool (is, "EnableLocalLights", CfgVisualPrm.bLocalLight);
	if (GetInt (is, "MaxLights", i))
		CfgVisualPrm.MaxLight = (DWORD)i;
	if (GetInt (is, "AmbientLevel", i))
		SetAmbientLevel ((DWORD)i);
	if (GetInt (is, "PlanetMaxPatchLevel", i))
		CfgVisualPrm.PlanetMaxLevel = max (1, min (SURF_MAX_PATCHLEVEL2, i));
	if (GetReal (is, "PlanetPatchRes", d))
		CfgVisualPrm.PlanetPatchRes = max (0.1, min (10.0, d));
	if (GetReal (is, "NightlightBrightness", d))
		CfgVisualPrm.LightBrightness = max (0.0, min (1.0, d));
	if (GetInt (is, "ElevationMode", i) && i >= 0 && i <= 1)
		CfgVisualPrm.ElevMode = i;
	GetBool(is, "EnableBackgroundStars", CfgVisualPrm.bUseStarDots);
	if (GetString(is, "StarPrm", cbuf, sizeof(cbuf))) {
		sscanf(cbuf, "%lf%lf%lf%d", &CfgVisualPrm.StarPrm.mag_hi, &CfgVisualPrm.StarPrm.mag_lo, &CfgVisualPrm.StarPrm.brt_min, &i);
		CfgVisualPrm.StarPrm.map_log = (i != 0);
	}
	GetBool(is, "EnableBackgroundStarmap", CfgVisualPrm.bUseStarImage);
	if (GetString(is, "CSphereStarPath", cbuf, sizeof(cbuf)))
		strncpy(CfgVisualPrm.StarImagePath, cbuf, 128);
	GetBool(is, "EnableBackgroundImage", CfgVisualPrm.bUseBgImage);
	if (GetString(is, "CSphereBgPath", cbuf, sizeof(cbuf)))
		strncpy(CfgVisualPrm.CSphereBgPath, cbuf, 128);
	GetReal (is, "CSphereBgIntensity", CfgVisualPrm.CSphereBgIntens);

	// screen capture parameters
	GetInt (is, "CaptureTarget", CfgCapturePrm.ImageTgt);
	if (GetString (is, "CaptureFile", cbuf, sizeof(cbuf)))
		strncpy (CfgCapturePrm.ImageFile, cbuf, 128);
	if (GetString (is, "CaptureSequenceDir", cbuf, sizeof(cbuf)))
		strncpy (CfgCapturePrm.SequenceDir, cbuf, 128);
	GetInt (is, "CaptureImageFormat", CfgCapturePrm.ImageFormat);
	GetInt (is, "CaptureImageQuality", CfgCapturePrm.ImageQuality);
	GetInt (is, "CaptureSequenceStart", CfgCapturePrm.SequenceStart);
	GetInt (is, "CaptureSequenceSkip", CfgCapturePrm.SequenceSkip);

	// instrument parameters
	CfgInstrumentPrm.bMfdPow2 = (GetBool (is, "ForceMfdPow2", b) ? (b ? 1 : 0) : 2);
	GetInt (is, "MfdHiresThreshold", CfgInstrumentPrm.MfdHiresThreshold);
	GetInt (is, "PanelMfdHudSize", CfgInstrumentPrm.PanelMFDHUDSize);
	GetInt (is, "VCMfdSize", CfgInstrumentPrm.VCMFDSize);

	// visual helper parameters
	if (GetInt (is, "Planetarium", i))
		CfgVisHelpPrm.flagPlanetarium = (DWORD)i;
	if (GetInt(is, "SurfMarkers", i))
		CfgVisHelpPrm.flagMarkers = (DWORD)i;
	if (GetString (is, "Bodyforces", cbuf, sizeof(cbuf)))
		sscanf (cbuf, "%u%f%f", &CfgVisHelpPrm.flagBodyForce, &CfgVisHelpPrm.scaleBodyForce, &CfgVisHelpPrm.opacBodyForce);
	if (GetString (is, "CoordinateAxes", cbuf, sizeof(cbuf)))
		sscanf (cbuf, "%u%f%f", &CfgVisHelpPrm.flagFrameAxes, &CfgVisHelpPrm.scaleFrameAxes, &CfgVisHelpPrm.opacFrameAxes);

	// debug options
	if (GetInt (is, "ShutdownMode", i) && i >= 0 && i <= 2)
		CfgDebugPrm.ShutdownMode = i;
	if (GetReal (is, "FixedStep", d) && d >= 0)
		CfgDebugPrm.FixedStep = d;
	GetBool (is, "DisableFontSmoothing", CfgDebugPrm.bDisableSmoothFont);
	GetBool (is, "ForceReenableFontSmoothing", CfgDebugPrm.bForceReenableSmoothFont);
	GetInt (is, "HtmlScnDesc", CfgDebugPrm.bHtmlScnDesc);
	GetBool (is, "SaveExitScreen", CfgDebugPrm.bSaveExitScreen);
	GetBool (is, "WireframeMode", CfgDebugPrm.bWireframeMode);
    GetBool (is, "NormaliseNormals", CfgDebugPrm.bNormaliseNormals);
	GetBool (is, "VerboseLog", CfgDebugPrm.bVerboseLog);

	GetReal (is, "CameraPanspeed", CfgCameraPrm.Panspeed);
	GetReal (is, "CameraTerrainLimit", CfgCameraPrm.TerrainLimit);
	GetInt (is, "HUDColIdx", CfgCameraPrm.HUDCol);

	// user interface parameters
	if (GetInt(is, "MouseFocusMode", i) && i >= 0 && i <= 2)
		CfgUIPrm.MouseFocusMode = (DWORD)i;
	if (GetInt (is, "MenubarMode", i) && i >= 0 && i <= 2)
		CfgUIPrm.MenuMode = (DWORD)i;
	GetBool (is, "MenubarLabelAlways", CfgUIPrm.bMenuLabelAlways);
	if (GetInt (is, "MenuButtonHoverSize", i))
		CfgUIPrm.MenuButtonHoverSize = i;
	if (GetInt (is, "MenuButtonSize", i))
		CfgUIPrm.MenuButtonSize = i;
	if (GetInt (is, "MenuButtonSpacing", i))
		CfgUIPrm.MenuButtonSpacing = i;
	GetBool (is, "ShowWarpAlways", CfgUIPrm.bWarpAlways);
	if (GetInt (is, "InfobarMode", i) && i >= 0 && i <= 2)
		CfgUIPrm.InfoMode = i;
	if (GetInt (is, "FPS", i) && i >= 0 && i <= 2)
		CfgUIPrm.FPS = i;
	if (GetInt (is, "MenubarOpacity", i) && i >= 0 && i <= 10)
		CfgUIPrm.MenuOpacity = i;
	if (GetInt (is, "InfobarOpacity", i) && i >= 0 && i <= 10)
		CfgUIPrm.InfoOpacity = (DWORD)i;
	if (GetInt (is, "MenubarSpeed", i) && i >= 1 && i <= 20)
		CfgUIPrm.MenuScrollspeed = (DWORD)i;
	GetInt (is, "SelVesselTab", CfgUIPrm.SelVesselTab);
	GetInt (is, "SelVesselRange", CfgUIPrm.SelVesselRange);
	GetBool (is, "SelVesselFlat", CfgUIPrm.bSelVesselFlat);

	// demo parameters
	GetBool (is, "DemoMode", CfgDemoPrm.bDemo);
	GetBool (is, "BackgroundImage", CfgDemoPrm.bBkImage);
	GetBool (is, "BlockExit", CfgDemoPrm.bBlockExit);
	GetReal (is, "MaxDemoTime", CfgDemoPrm.MaxDemoTime);
	GetReal (is, "MaxLaunchpadIdleTime", CfgDemoPrm.LPIdleTime);

	// record/playback parameters
	GetInt (is, "RecordPosFrame", CfgRecPlayPrm.RecordPosFrame);
	GetInt (is, "RecordAttFrame", CfgRecPlayPrm.RecordAttFrame);
	GetBool (is, "RecordTimewarp", CfgRecPlayPrm.bRecordWarp);
	GetBool (is, "RecordFocusEvent", CfgRecPlayPrm.bRecordFocus);
	GetBool (is, "ReplayTimewarp", CfgRecPlayPrm.bReplayWarp);
	GetBool (is, "ReplayFocusEvent", CfgRecPlayPrm.bReplayFocus);
	GetBool (is, "ReplayCameraEvent", CfgRecPlayPrm.bReplayCam);
	GetBool (is, "SystimeSampling", CfgRecPlayPrm.bSysInterval);
	GetBool (is, "PlaybackNotes", CfgRecPlayPrm.bShowNotes);

	// font characteristics
	if (GetReal (is, "DialogFont_Scale", d)) CfgFontPrm.dlgFont_Scale = (float)d;
	GetString (is, "DialogFont1_Face", CfgFontPrm.dlgFont1_Face, sizeof(CfgFontPrm.dlgFont1_Face));
	if (GetReal (is, "ImGui_FontSize", d)) CfgFontPrm.ImGui_FontSize = (float)d;
	GetString (is, "ImGui_DefaultFontFile", CfgFontPrm.ImGui_DefaultFontFile, sizeof(CfgFontPrm.ImGui_DefaultFontFile));
	GetString (is, "ImGui_ConsoleFontFile", CfgFontPrm.ImGui_ConsoleFontFile, sizeof(CfgFontPrm.ImGui_ConsoleFontFile));
	GetString (is, "ImGui_MonospacedFontFile", CfgFontPrm.ImGui_MonospacedFontFile, sizeof(CfgFontPrm.ImGui_MonospacedFontFile));
	GetString (is, "ImGui_ManuscriptFontFile", CfgFontPrm.ImGui_ManuscriptFontFile, sizeof(CfgFontPrm.ImGui_ManuscriptFontFile));

	// misc. options
	if (GetString (is, "LPadRect", cbuf, sizeof(cbuf))) {
		sscanf (cbuf, "%d%d%d%d", &rLaunchpad.left, &rLaunchpad.top,
			&rLaunchpad.right, &rLaunchpad.bottom);
	}

	if (GetString (is, "DlgMapPos", cbuf, sizeof(cbuf)))
		sscanf (cbuf, "%d%d%d%d", &CfgWindowPos.DlgMap.left, &CfgWindowPos.DlgMap.top,
			&CfgWindowPos.DlgMap.right, &CfgWindowPos.DlgMap.bottom);
	if (GetString (is, "DlgInfoPos", cbuf, sizeof(cbuf)))
		sscanf (cbuf, "%d%d%d%d", &CfgWindowPos.DlgInfo.left, &CfgWindowPos.DlgInfo.top,
			&CfgWindowPos.DlgInfo.right, &CfgWindowPos.DlgInfo.bottom);
	if (GetString (is, "DlgCamPos", cbuf, sizeof(cbuf)))
		sscanf (cbuf, "%d%d%d%d", &CfgWindowPos.DlgCamera.left, &CfgWindowPos.DlgCamera.top,
			&CfgWindowPos.DlgCamera.right, &CfgWindowPos.DlgCamera.bottom);
	if (GetString (is, "DlgFocusPos", cbuf, sizeof(cbuf)))
		sscanf (cbuf, "%d%d%d%d", &CfgWindowPos.DlgFocus.left, &CfgWindowPos.DlgFocus.top,
			&CfgWindowPos.DlgFocus.right, &CfgWindowPos.DlgFocus.bottom);
	if (GetString (is, "DlgTaccPos", cbuf, sizeof(cbuf)))
		sscanf (cbuf, "%d%d%d%d", &CfgWindowPos.DlgTacc.left, &CfgWindowPos.DlgTacc.top,
			&CfgWindowPos.DlgTacc.right, &CfgWindowPos.DlgTacc.bottom);
	if (GetString(is, "DlgOptionsPos", cbuf, sizeof(cbuf)))
		sscanf(cbuf, "%d%d%d%d", &CfgWindowPos.DlgOptions.left, &CfgWindowPos.DlgOptions.top,
			&CfgWindowPos.DlgOptions.right, &CfgWindowPos.DlgOptions.bottom);
	if (GetString (is, "DlgVhelperPos", cbuf, sizeof(cbuf)))
		sscanf (cbuf, "%d%d%d%d", &CfgWindowPos.DlgVishelper.left, &CfgWindowPos.DlgVishelper.top,
			&CfgWindowPos.DlgVishelper.right, &CfgWindowPos.DlgVishelper.bottom);
	GetInt (is, "LpadScnListWidth", CfgWindowPos.LaunchpadScnListWidth);
	GetInt (is, "LpadModListWidth", CfgWindowPos.LaunchpadModListWidth);
	GetInt (is, "LpadExtListWidth", CfgWindowPos.LaunchpadExtListWidth);

	// list of active modules
	if (FindLine (is, "ACTIVE_MODULES")) {
		char cbuf[256];
		while (getline_trunc (is, cbuf, 256) && _strnicmp (cbuf, "END_MODULES", 11))
			m_activeModules.push_back(std::string(trim_string(cbuf)));
	}
	return true;
}

Config::~Config()
{
}

void Config::SetDefaults ()
{
	strcpy(CfgDirPrm.ConfigDir, CfgDirPrm_default.ConfigDir);
	strcpy(CfgDirPrm.MeshDir, CfgDirPrm_default.MeshDir);
	strcpy(CfgDirPrm.TextureDir, CfgDirPrm_default.TextureDir);
	strcpy(CfgDirPrm.HightexDir, CfgDirPrm_default.HightexDir);
	strcpy(CfgDirPrm.PlanetTexDir, CfgDirPrm_default.PlanetTexDir);
	strcpy(CfgDirPrm.ScnDir, CfgDirPrm_default.ScnDir);

	strcpy(cfgpath, CfgDirPrm.ConfigDir);    cfglen = strlen(cfgpath);
	strcpy(mshpath, CfgDirPrm.MeshDir);      mshlen = strlen(mshpath);
	strcpy(texpath, CfgDirPrm.TextureDir);   texlen = strlen(texpath);
	strcpy(htxpath, CfgDirPrm.HightexDir);   htxlen = strlen(htxpath);
	strcpy(ptxpath, CfgDirPrm.PlanetTexDir); ptxlen = strlen(ptxpath);
	strcpy(scnpath, CfgDirPrm.ScnDir);       scnlen = strlen(scnpath);

	m_sRoot.clear();

	bEchoAll = bEchoAll_default;
	memset (&rLaunchpad, 0, sizeof(RECT));

	RECT r;
	GetWindowRect (GetDesktopWindow(), &r);
	CfgDevPrm_default.WinW = r.right-r.left; CfgDevPrm_default.WinH = r.bottom-r.top;
	// use the screen size as the default render window size

	AmbientColour = 0x0c0c0c0c;

	CfgPhysicsPrm = CfgPhysicsPrm_default;	     // physics engine parameters
	CfgLogicPrm = CfgLogicPrm_default;		     // logical parameters
	CfgVisualPrm = CfgVisualPrm_default;         // visualisation parameters
	CfgCapturePrm = CfgCapturePrm_default;       // screen capture parameters
	CfgInstrumentPrm = CfgInstrumentPrm_default; // instrument parameters
	CfgPRenderPrm = CfgPRenderPrm_default;       // planet render parameters
	CfgMapPrm = CfgMapPrm_default;               // map dialog parameters
	CfgVisHelpPrm = CfgVisHelpPrm_default;       // visual helper parameters
	CfgDebugPrm = CfgDebugPrm_default;		     // debugging parameters
	CfgRecPlayPrm = CfgRecPlayPrm_default;       // recording/playback parameters
	CfgDevPrm = CfgDevPrm_default;               // video device parameters
	CfgJoystickPrm = CfgJoystickPrm_default;     // joystick parameters
	CfgUIPrm = CfgUIPrm_default;                 // user interface parameters
	CfgDemoPrm = CfgDemoPrm_default;             // demo parameters
	CfgFontPrm = CfgFontPrm_default;             // ingame font characteristics
	CfgCameraPrm = CfgCameraPrm_default;         // camera parameters
	CfgWindowPos = CfgWindowPos_default;         // subwindow positions
	CfgCmdlinePrm = CfgCmdlinePrm_default;       // command line parameters

	found_config_file = false;
}

void Config::SetDefaults_Capture ()
{
	CfgCapturePrm = CfgCapturePrm_default;       // screen capture parameters
}

const void *Config::GetParam (DWORD paramtype) const
{
	switch (paramtype) {
	case CFGPRM_SURFACEMAXLEVEL:
		return (void*)&CfgVisualPrm.PlanetMaxLevel;
	case CFGPRM_SURFACEREFLECT:
		return (void*)&CfgVisualPrm.bWaterreflect;
	case CFGPRM_SURFACERIPPLE:
		return (void*)&CfgVisualPrm.bSpecularRipple;
	case CFGPRM_SURFACELIGHTS:
		return (void*)&CfgVisualPrm.bNightlights;
	case CFGPRM_SURFACELIGHTBRT:
		return (void*)&CfgVisualPrm.LightBrightness;
	case CFGPRM_ATMHAZE:
		return (void*)&CfgVisualPrm.bHaze;
	case CFGPRM_ATMFOG:
		return (void*)&CfgVisualPrm.bFog;
	case CFGPRM_CLOUDS:
		return (void*)&CfgVisualPrm.bClouds;
	case CFGPRM_CLOUDSHADOWS:
		return (void*)&CfgVisualPrm.bCloudShadows;
	case CFGPRM_PLANETARIUMFLAG:
		return (void*)&CfgVisHelpPrm.flagPlanetarium;
	case CFGPRM_SURFMARKERFLAG:
		return (void*)&CfgVisHelpPrm.flagMarkers;
	case CFGPRM_STARRENDERPRM:
		return (void*)&CfgVisualPrm.StarPrm;
	case CFGPRM_AMBIENTLEVEL:
		return (void*)&CfgVisualPrm.AmbientLevel;
	case CFGPRM_VESSELSHADOWS:
		return (void*)&CfgVisualPrm.bVesselShadows;
	case CFGPRM_OBJECTSHADOWS:
		return (void*)&CfgVisualPrm.bShadows;
	case CFGPRM_OBJECTSPECULAR:
		return (void*)&CfgVisualPrm.bSpecular;
	case CFGPRM_CSPHEREUSEBGIMAGE:
		return (void*)&CfgVisualPrm.bUseBgImage;
	case CFGPRM_CSPHERETEXTURE:
		return (void*)CfgVisualPrm.CSphereBgPath;
	case CFGPRM_CSPHEREINTENS:
		return (void*)&CfgVisualPrm.CSphereBgIntens;
	case CFGPRM_LOCALLIGHT:
		return (void*)&CfgVisualPrm.bLocalLight;
	case CFGPRM_MAXLIGHT:
		return (void*)&CfgVisualPrm.MaxLight;
	case CFGPRM_RESOLUTIONBIAS:
		return (void*)&CfgPRenderPrm.ResolutionBias;
	case CFGPRM_WIREFRAME:
		return (void*)&CfgDebugPrm.bWireframeMode;
	case CFGPRM_ELEVATIONMODE:
		return (void*)&CfgVisualPrm.ElevMode;
	case CFGPRM_TILELOADFLAGS:
		return (void*)&CfgPRenderPrm.TileLoadFlags;
	case CFGPRM_PANELMFDHUDSIZE:
		return (void*)&CfgInstrumentPrm.PanelMFDHUDSize;
	case CFGPRM_TILEPATCHRES:
		return (void*)&CfgPRenderPrm.PatchRes;
	case CFGPRM_FORCEVECTORFLAG:
		return (void*)&CfgVisHelpPrm.flagBodyForce;
	case CFGPRM_FORCEVECTORSCALE:
		return (void*)&CfgVisHelpPrm.scaleBodyForce;
	case CFGPRM_FORCEVECTOROPACITY:
		return (void*)&CfgVisHelpPrm.opacBodyForce;
	case CFGPRM_FRAMEAXISFLAG:
		return (void*)&CfgVisHelpPrm.flagFrameAxes;
	case CFGPRM_FRAMEAXISSCALE:
		return (void*)&CfgVisHelpPrm.scaleFrameAxes;
	case CFGPRM_FRAMEAXISOPACITY:
		return (void*)&CfgVisHelpPrm.opacFrameAxes;
	case CFGPRM_CSPHERESTARTEXTURE:
		return (void*)&CfgVisualPrm.StarImagePath;
	case CFGPRM_CSPHEREUSESTARIMAGE:
		return (void*)&CfgVisualPrm.bUseStarImage;
	case CFGPRM_CSPHEREUSESTARDOTS:
		return (void*)&CfgVisualPrm.bUseStarDots;
	default:
		return 0;
	}
}

bool Config::Write(const std::string &fname) const
{
	if (!fname.size()) return false;
	ofstream ofs(fname);
	if (!ofs) return false;
	return Write(ofs);
}

bool Config::Write() const
{
	return Write(m_sRoot);
}

bool Config::Write(std::ostream &os) const
{
	int i;

	os << "; === ORBITER Master Configuration File ===\n";
	os << "EchoAllParams = " << BoolStr (bEchoAll) << '\n';
	if (rLaunchpad.right > rLaunchpad.left)
		os << "LPadRect = " << rLaunchpad.left << ' ' << rLaunchpad.top
			<< ' ' << rLaunchpad.right << ' ' << rLaunchpad.bottom << '\n';

	if (strcmp (CfgDirPrm.ConfigDir, CfgDirPrm_default.ConfigDir) || 
		strcmp (CfgDirPrm.MeshDir, CfgDirPrm_default.MeshDir) ||
		strcmp (CfgDirPrm.TextureDir, CfgDirPrm_default.TextureDir) ||
		strcmp (CfgDirPrm.HightexDir, CfgDirPrm_default.HightexDir) ||
		strcmp (CfgDirPrm.PlanetTexDir, CfgDirPrm_default.PlanetTexDir) ||
		strcmp (CfgDirPrm.ScnDir, CfgDirPrm_default.ScnDir) || bEchoAll) {
		os << "\n; === Subdirectory locations\n";
		if (strcmp (CfgDirPrm.ConfigDir, CfgDirPrm_default.ConfigDir) || bEchoAll)
			os << "ConfigDir = " << CfgDirPrm.ConfigDir << '\n';
		if (strcmp (CfgDirPrm.MeshDir, CfgDirPrm_default.MeshDir) || bEchoAll)
			os << "MeshDir = " << CfgDirPrm.MeshDir << '\n';
		if (strcmp (CfgDirPrm.TextureDir, CfgDirPrm_default.TextureDir) || bEchoAll)
			os << "TextureDir = " << CfgDirPrm.TextureDir << endl;
		if (strcmp (CfgDirPrm.HightexDir, CfgDirPrm_default.HightexDir) || bEchoAll)
			os << "HightexDir = " << CfgDirPrm.HightexDir << endl;
		if (strcmp(CfgDirPrm.PlanetTexDir, CfgDirPrm_default.PlanetTexDir) || bEchoAll)
			os << "PlanetTexDir = " << CfgDirPrm.PlanetTexDir << endl;
		if (strcmp (CfgDirPrm.ScnDir, CfgDirPrm_default.ScnDir) || bEchoAll)
			os << "ScenarioDir = " << CfgDirPrm.ScnDir << endl;
	}

	if (memcmp (&CfgLogicPrm, &CfgLogicPrm_default, sizeof(CFG_LOGICPRM)) || bEchoAll) {
		os << "\n; === Logical parameters ===\n";
		if (CfgLogicPrm.bStartPaused != CfgLogicPrm_default.bStartPaused || bEchoAll)
			os << "StartPaused = " << BoolStr(CfgLogicPrm.bStartPaused) << '\n';
		if (CfgLogicPrm.FlightModelLevel != CfgLogicPrm_default.FlightModelLevel || bEchoAll)
			os << "FlightModel = " << CfgLogicPrm.FlightModelLevel << '\n';
		if (CfgLogicPrm.DamageSetting != CfgLogicPrm_default.DamageSetting || bEchoAll)
			os << "DamageModel = " << CfgLogicPrm.DamageSetting << '\n';
		if (CfgLogicPrm.bLimitedFuel != CfgLogicPrm_default.bLimitedFuel || bEchoAll)
			os << "UnlimitedFuel = " << BoolStr(!CfgLogicPrm.bLimitedFuel) << '\n';
		if (CfgLogicPrm.bPadRefuel != CfgLogicPrm_default.bPadRefuel || bEchoAll)
			os << "RefuelOnPad = " << BoolStr(CfgLogicPrm.bPadRefuel) << '\n';
		if (CfgLogicPrm.bMfdTransparent != CfgLogicPrm_default.bMfdTransparent || bEchoAll)
			os << "MFDTransparent = " << BoolStr (CfgLogicPrm.bMfdTransparent) << '\n';
		if (CfgLogicPrm.bGlasspitCompact != CfgLogicPrm_default.bGlasspitCompact || bEchoAll)
			os << "CompactGlasspit = " << BoolStr (CfgLogicPrm.bGlasspitCompact) << '\n';
		if (CfgLogicPrm.MFDSize != CfgLogicPrm_default.MFDSize || bEchoAll)
			os << "GenericMFDSize = " << CfgLogicPrm.MFDSize << '\n';
		if (CfgLogicPrm.MFDMapVersion != CfgLogicPrm_default.MFDMapVersion || bEchoAll)
			os << "MFDMapVersion = " << CfgLogicPrm.MFDMapVersion << '\n';
		if (fabs (CfgLogicPrm.InstrUpdDT-CfgLogicPrm_default.InstrUpdDT) > 1e-8 || bEchoAll)
			os << "InstrumentUpdateInterval = " << CfgLogicPrm.InstrUpdDT << '\n';
		if (fabs (CfgLogicPrm.PanelScale-CfgLogicPrm_default.PanelScale) > 1e-8 || bEchoAll)
			os << "PanelScale = " << CfgLogicPrm.PanelScale << '\n';
		if (fabs (CfgLogicPrm.PanelScrollSpeed-CfgLogicPrm_default.PanelScrollSpeed) > 1e-8 || bEchoAll)
			os << "PanelScrollSpeed = " << CfgLogicPrm.PanelScrollSpeed << '\n';
	}

	if (memcmp (&CfgVisualPrm, &CfgVisualPrm_default, sizeof(CFG_VISUALPRM)) || bEchoAll) {
		os << "\n; === Visual parameters ===\n";
		if (CfgVisualPrm.bShadows != CfgVisualPrm_default.bShadows || bEchoAll)
			os << "EnableShadows = " << BoolStr (CfgVisualPrm.bShadows) << '\n';
		if (CfgVisualPrm.bVesselShadows != CfgVisualPrm_default.bVesselShadows || bEchoAll)
			os << "EnableVesselShadows = " << BoolStr (CfgVisualPrm.bVesselShadows) << '\n';
		if (CfgVisualPrm.bClouds != CfgVisualPrm_default.bClouds || bEchoAll)
			os << "EnableClouds = " << BoolStr (CfgVisualPrm.bClouds) << '\n';
		if (CfgVisualPrm.bCloudShadows != CfgVisualPrm_default.bCloudShadows || bEchoAll)
			os << "EnableCloudShadows = " << BoolStr (CfgVisualPrm.bCloudShadows) << '\n';
		if (CfgVisualPrm.bNightlights != CfgVisualPrm_default.bNightlights || bEchoAll)
			os << "EnableNightlights = " << BoolStr (CfgVisualPrm.bNightlights) << '\n';
		if (CfgVisualPrm.bWaterreflect != CfgVisualPrm_default.bWaterreflect || bEchoAll)
			os << "EnableWaterReflection = " << BoolStr (CfgVisualPrm.bWaterreflect) << '\n';
		if (CfgVisualPrm.bSpecularRipple != CfgVisualPrm_default.bSpecularRipple || bEchoAll)
			os << "EnableSpecularRipples = " << BoolStr (CfgVisualPrm.bSpecularRipple) << '\n';
		if (CfgVisualPrm.bHaze != CfgVisualPrm_default.bHaze || bEchoAll)
			os << "EnableHorizonHaze = " << BoolStr (CfgVisualPrm.bHaze) << '\n';
		if (CfgVisualPrm.bFog != CfgVisualPrm_default.bFog || bEchoAll)
			os << "EnableDistanceFog = " << BoolStr (CfgVisualPrm.bFog) << '\n';
		if (CfgVisualPrm.bSpecular != CfgVisualPrm_default.bSpecular || bEchoAll)
			os << "EnableSpecularReflection = " << BoolStr (CfgVisualPrm.bSpecular) << '\n';
		if (CfgVisualPrm.bReentryFlames != CfgVisualPrm_default.bReentryFlames || bEchoAll)
			os << "EnableReentryFlames = " << BoolStr (CfgVisualPrm.bReentryFlames) << '\n';
		if (CfgVisualPrm.bParticleStreams != CfgVisualPrm_default.bParticleStreams || bEchoAll)
			os << "EnableParticleStreams = " << BoolStr (CfgVisualPrm.bParticleStreams) << '\n';
		if (CfgVisualPrm.bLocalLight != CfgVisualPrm_default.bLocalLight || bEchoAll)
			os << "EnableLocalLights = " << BoolStr (CfgVisualPrm.bLocalLight) << '\n';
		if (CfgVisualPrm.MaxLight != CfgVisualPrm_default.MaxLight || bEchoAll)
			os << "MaxLights = " << CfgVisualPrm.MaxLight << '\n';
		if (CfgVisualPrm.AmbientLevel != CfgVisualPrm_default.AmbientLevel || bEchoAll)
			os << "AmbientLevel = " << CfgVisualPrm.AmbientLevel << '\n';
		if (CfgVisualPrm.PlanetMaxLevel != CfgVisualPrm_default.PlanetMaxLevel || bEchoAll)
			os << "PlanetMaxPatchLevel = " << CfgVisualPrm.PlanetMaxLevel << '\n';
		if (CfgVisualPrm.PlanetPatchRes != CfgVisualPrm_default.PlanetPatchRes || bEchoAll)
			os << "PlanetPatchRes = " << CfgVisualPrm.PlanetPatchRes << '\n';
		if (CfgVisualPrm.LightBrightness != CfgVisualPrm_default.LightBrightness || bEchoAll)
			os << "NightlightBrightness = " << CfgVisualPrm.LightBrightness << '\n';
		if (CfgVisualPrm.bUseStarDots != CfgVisualPrm_default.bUseStarDots || bEchoAll)
			os << "EnableBackgroundStars = " << BoolStr(CfgVisualPrm.bUseStarDots) << '\n';
		if (memcmp (&CfgVisualPrm.StarPrm, &CfgVisualPrm_default.StarPrm, sizeof(StarRenderPrm)) || bEchoAll)
			os << "StarPrm = " << CfgVisualPrm.StarPrm.mag_hi << ' ' << CfgVisualPrm.StarPrm.mag_lo << ' '
				<< CfgVisualPrm.StarPrm.brt_min << ' ' << (CfgVisualPrm.StarPrm.map_log ? 1:0) << '\n';
		if (CfgVisualPrm.bUseStarImage != CfgVisualPrm_default.bUseStarImage || bEchoAll)
			os << "EnableBackgroundStarmap = " << BoolStr(CfgVisualPrm.bUseStarImage) << '\n';
		if (strcmp(CfgVisualPrm.StarImagePath, CfgVisualPrm_default.StarImagePath) || bEchoAll)
			os << "CSphereStarPath = " << CfgVisualPrm.StarImagePath << '\n';
		if (CfgVisualPrm.bUseBgImage != CfgVisualPrm_default.bUseBgImage || bEchoAll)
			os << "EnableBackgroundImage = " << BoolStr(CfgVisualPrm.bUseBgImage) << '\n';
		if (strcmp (CfgVisualPrm.CSphereBgPath, CfgVisualPrm_default.CSphereBgPath) || bEchoAll)
			os << "CSphereBgPath = " << CfgVisualPrm.CSphereBgPath << '\n';
		if (CfgVisualPrm.CSphereBgIntens != CfgVisualPrm_default.CSphereBgIntens || bEchoAll)
			os << "CSphereBgIntensity = " << CfgVisualPrm.CSphereBgIntens << '\n';
		if (CfgVisualPrm.ElevMode != CfgVisualPrm_default.ElevMode || bEchoAll)
			os << "ElevationMode = " << CfgVisualPrm.ElevMode << '\n';
	}

	if (memcmp (&CfgCapturePrm, &CfgCapturePrm_default, sizeof(CFG_CAPTUREPRM)) || bEchoAll) {
		os << "\n; === Screen capture parameters ===\n";
		if (CfgCapturePrm.ImageTgt != CfgCapturePrm_default.ImageTgt || bEchoAll)
			os << "CaptureTarget = " << CfgCapturePrm.ImageTgt << '\n';
		if (strcmp (CfgCapturePrm.ImageFile, CfgCapturePrm_default.ImageFile) || bEchoAll)
			os << "CaptureFile = " << CfgCapturePrm.ImageFile << '\n';
		if (strcmp (CfgCapturePrm.SequenceDir, CfgCapturePrm_default.SequenceDir) || bEchoAll)
			os << "CaptureSequenceDir = " << CfgCapturePrm.SequenceDir << '\n';
		if (CfgCapturePrm.ImageFormat != CfgCapturePrm_default.ImageFormat || bEchoAll)
			os << "CaptureImageFormat = " << CfgCapturePrm.ImageFormat << '\n';
		if (CfgCapturePrm.ImageQuality != CfgCapturePrm_default.ImageQuality || bEchoAll)
			os << "CaptureImageQuality = " << CfgCapturePrm.ImageQuality << '\n';
		if (CfgCapturePrm.SequenceStart != CfgCapturePrm_default.SequenceStart || bEchoAll)
			os << "CaptureSequenceStart = " << CfgCapturePrm.SequenceStart << '\n';
		if (CfgCapturePrm.SequenceSkip != CfgCapturePrm_default.SequenceSkip || bEchoAll)
			os << "CaptureSequenceSkip = " << CfgCapturePrm.SequenceSkip << '\n';
	}

	if (memcmp (&CfgInstrumentPrm, &CfgInstrumentPrm_default, sizeof(CFG_INSTRUMENTPRM)) || bEchoAll) {
		os << "\n; === Instrument parameters ===\n";
		if (CfgInstrumentPrm.bMfdPow2 != CfgInstrumentPrm_default.bMfdPow2 || bEchoAll)
			os << "ForceMfdPow2 = " << ((CfgInstrumentPrm.bMfdPow2 < 2) ?
				BoolStr (CfgInstrumentPrm.bMfdPow2 == 1) : "AUTO") << '\n';
		if (CfgInstrumentPrm.MfdHiresThreshold != CfgInstrumentPrm_default.MfdHiresThreshold || bEchoAll)
			os << "MfdHiresThreshold = " << CfgInstrumentPrm.MfdHiresThreshold << '\n';
		if (CfgInstrumentPrm.PanelMFDHUDSize != CfgInstrumentPrm_default.PanelMFDHUDSize || bEchoAll)
			os << "PanelMfdHudSize = " << CfgInstrumentPrm.PanelMFDHUDSize << '\n';
		if (CfgInstrumentPrm.VCMFDSize != CfgInstrumentPrm_default.VCMFDSize || bEchoAll)
			os << "VCMfdSize = " << CfgInstrumentPrm.VCMFDSize << '\n';
	}

	if (memcmp (&CfgVisHelpPrm, &CfgVisHelpPrm_default, sizeof(CFG_VISHELPPRM)) || bEchoAll) {
		os << "\n; === Visual helper parameters ===\n";
		if (CfgVisHelpPrm.flagPlanetarium != CfgVisHelpPrm_default.flagPlanetarium || bEchoAll)
			os << "Planetarium = " << CfgVisHelpPrm.flagPlanetarium << '\n';
		if (CfgVisHelpPrm.flagMarkers != CfgVisHelpPrm_default.flagMarkers || bEchoAll)
			os << "SurfMarkers = " << CfgVisHelpPrm.flagMarkers << '\n';
		if (CfgVisHelpPrm.flagBodyForce != CfgVisHelpPrm_default.flagBodyForce ||
			CfgVisHelpPrm.scaleBodyForce != CfgVisHelpPrm_default.scaleBodyForce ||
			CfgVisHelpPrm.opacBodyForce != CfgVisHelpPrm_default.opacBodyForce || bEchoAll)
			os << "Bodyforces = " << CfgVisHelpPrm.flagBodyForce << ' ' << CfgVisHelpPrm.scaleBodyForce
				<< ' ' << CfgVisHelpPrm.opacBodyForce << '\n';
		if (CfgVisHelpPrm.flagFrameAxes != CfgVisHelpPrm_default.flagFrameAxes ||
			CfgVisHelpPrm.scaleFrameAxes != CfgVisHelpPrm_default.scaleFrameAxes ||
			CfgVisHelpPrm.opacFrameAxes != CfgVisHelpPrm_default.opacFrameAxes || bEchoAll)
			os << "CoordinateAxes = " << CfgVisHelpPrm.flagFrameAxes << ' ' << CfgVisHelpPrm.scaleFrameAxes
				<< ' ' << CfgVisHelpPrm.opacFrameAxes << '\n';
	}

	if (memcmp (&CfgDebugPrm, &CfgDebugPrm_default, sizeof (CFG_DEBUGPRM)) || bEchoAll) {
		os << "\n; === Debugging options ===\n";
		if (CfgDebugPrm.ShutdownMode != CfgDebugPrm_default.ShutdownMode || bEchoAll)
			os << "ShutdownMode = " << CfgDebugPrm.ShutdownMode << '\n';
		if (CfgDebugPrm.FixedStep != CfgDebugPrm_default.FixedStep || bEchoAll)
			os << "FixedStep = " << CfgDebugPrm.FixedStep << '\n';
		if (CfgDebugPrm.bDisableSmoothFont != CfgDebugPrm_default.bDisableSmoothFont || bEchoAll)
			os << "DisableFontSmoothing = " << BoolStr (CfgDebugPrm.bDisableSmoothFont) << '\n';
		if (CfgDebugPrm.bForceReenableSmoothFont != CfgDebugPrm_default.bForceReenableSmoothFont || bEchoAll)
			os << "ForceReenableFontSmoothing = " << BoolStr (CfgDebugPrm.bForceReenableSmoothFont) << '\n';
		if (CfgDebugPrm.bHtmlScnDesc != CfgDebugPrm_default.bHtmlScnDesc || bEchoAll)
			os << "HtmlScnDesc = " << CfgDebugPrm.bHtmlScnDesc << '\n';
		if (CfgDebugPrm.bSaveExitScreen != CfgDebugPrm_default.bSaveExitScreen || bEchoAll)
			os << "SaveExitScreen = " << BoolStr (CfgDebugPrm.bSaveExitScreen) << '\n';
		if (CfgDebugPrm.bWireframeMode != CfgDebugPrm_default.bWireframeMode || bEchoAll)
			os << "WireframeMode = " << BoolStr (CfgDebugPrm.bWireframeMode) << '\n';
		if (CfgDebugPrm.bNormaliseNormals != CfgDebugPrm_default.bNormaliseNormals || bEchoAll)
			os << "NormaliseNormals = " << BoolStr (CfgDebugPrm.bNormaliseNormals) << '\n';
		if (CfgDebugPrm.bVerboseLog != CfgDebugPrm_default.bVerboseLog || bEchoAll)
			os << "VerboseLog = " << BoolStr (CfgDebugPrm.bVerboseLog) << '\n';
	}

	if (memcmp (&CfgPhysicsPrm, &CfgPhysicsPrm_default, sizeof(CFG_PHYSICSPRM)) || bEchoAll) {
		os << "\n; === Physics engine ===\n";
		if (CfgPhysicsPrm.bDistributedMass != CfgPhysicsPrm_default.bDistributedMass || bEchoAll)
			os << "DistributedVesselMass = " << BoolStr (CfgPhysicsPrm.bDistributedMass) << '\n';
		if (CfgPhysicsPrm.bNonsphericalGrav != CfgPhysicsPrm_default.bNonsphericalGrav || bEchoAll)
			os << "NonsphericalGravitySources = " << BoolStr (CfgPhysicsPrm.bNonsphericalGrav) << '\n';
		if (CfgPhysicsPrm.bRadiationPressure != CfgPhysicsPrm_default.bRadiationPressure || bEchoAll)
			os << "RadiationPressure = " << BoolStr (CfgPhysicsPrm.bRadiationPressure) << '\n';
		if (CfgPhysicsPrm.bAtmWind != CfgPhysicsPrm_default.bAtmWind || bEchoAll)
			os << "AtmosphericWind = " << BoolStr (CfgPhysicsPrm.bAtmWind) << '\n';
		if (CfgPhysicsPrm.bBaseCollision != CfgPhysicsPrm_default.bBaseCollision || bEchoAll)
			os << "BaseCollision = " << BoolStr (CfgPhysicsPrm.bBaseCollision) << '\n';
		if (CfgPhysicsPrm.bVesselCollision != CfgPhysicsPrm_default.bVesselCollision || bEchoAll)
			os << "VesselCollision = " << BoolStr (CfgPhysicsPrm.bVesselCollision) << '\n';
		if (CfgPhysicsPrm.bOrbitStabilise != CfgPhysicsPrm_default.bOrbitStabilise || bEchoAll)
			os << "StabiliseOrbits = " << BoolStr (CfgPhysicsPrm.bOrbitStabilise) << '\n';
		if (CfgPhysicsPrm.Stabilise_PLimit != CfgPhysicsPrm_default.Stabilise_PLimit || bEchoAll)
			os << "StabilisePLimit = " << CfgPhysicsPrm.Stabilise_PLimit << '\n';
		if (CfgPhysicsPrm.Stabilise_SLimit != CfgPhysicsPrm_default.Stabilise_SLimit || bEchoAll)
			os << "StabiliseSLimit = " << CfgPhysicsPrm.Stabilise_SLimit << '\n';
		if (CfgPhysicsPrm.PPropSubMax != CfgPhysicsPrm_default.PPropSubMax || CfgPhysicsPrm.PPropSubLimit != CfgPhysicsPrm_default.PPropSubLimit || bEchoAll)
			os << "PertPropSubsampling = " << CfgPhysicsPrm.PPropSubMax << ' ' << CfgPhysicsPrm.PPropSubLimit << '\n';
		if (CfgPhysicsPrm.PPropStepLimit != CfgPhysicsPrm_default.PPropStepLimit || bEchoAll)
			os << "PertPropNonsphericalLimit = " << CfgPhysicsPrm.PPropStepLimit << '\n';
		if (CfgPhysicsPrm.nLPropLevel != CfgPhysicsPrm_default.nLPropLevel || bEchoAll)
			os << "PropStages = " << CfgPhysicsPrm.nLPropLevel << '\n';
		for (i = 0; i < MAX_PROP_LEVEL; i++)
			if (CfgPhysicsPrm.PropMode[i] != CfgPhysicsPrm_default.PropMode[i] ||
				(i < CfgPhysicsPrm.nLPropLevel && (CfgPhysicsPrm.PropTTgt[i] != CfgPhysicsPrm_default.PropTTgt[i] || CfgPhysicsPrm.PropATgt[i] != CfgPhysicsPrm_default.PropATgt[i])) ||
				(i < CfgPhysicsPrm.nLPropLevel-1 && (CfgPhysicsPrm.PropTLim[i] != CfgPhysicsPrm_default.PropTLim[i] || CfgPhysicsPrm.PropALim[i] != CfgPhysicsPrm_default.PropALim[i])) ||
				bEchoAll) {
				os << "PropStage" << i << " = " << CfgPhysicsPrm.PropMode[i] << ' ' << CfgPhysicsPrm.PropTTgt[i] << ' ' << CfgPhysicsPrm.PropATgt[i];
				if (i < CfgPhysicsPrm.nLPropLevel-1)
					os << ' ' << CfgPhysicsPrm.PropTLim[i] << ' ' << CfgPhysicsPrm.PropALim[i];
				os << '\n';
			}
#ifdef UNDEF
		if (CfgPhysicsPrm.APropSubMax != CfgPhysicsPrm_default.APropSubMax || CfgPhysicsPrm.APropSubLimit != CfgPhysicsPrm_default.APropSubLimit || bEchoAll)
			os << "AngPropSubsampling = " << CfgPhysicsPrm.APropSubMax << ' ' << CfgPhysicsPrm.APropSubLimit << '\n';
		if (CfgPhysicsPrm.APropCouplingLimit != CfgPhysicsPrm_default.APropCouplingLimit || CfgPhysicsPrm.APropTorqueLimit != CfgPhysicsPrm_default.APropTorqueLimit || bEchoAll)
			os << "AngPropLimits = " << CfgPhysicsPrm.APropCouplingLimit << ' ' << CfgPhysicsPrm.APropTorqueLimit << '\n';
#endif
		if (CfgPhysicsPrm.PropSubMax != CfgPhysicsPrm_default.PropSubMax || bEchoAll)
			os << "PropSubsampling = " << CfgPhysicsPrm.PropSubMax << '\n';
	}

	if (memcmp (&CfgPRenderPrm, &CfgPRenderPrm_default, sizeof(CFG_PLANETRENDERPRM)) || bEchoAll) {
		os << "\n; === Planet rendering parameters ===\n";
		if (CfgPRenderPrm.PreloadMode != CfgPRenderPrm_default.PreloadMode || bEchoAll)
			os << "PlanetPreloadMode = " << CfgPRenderPrm.PreloadMode << '\n';
		if (CfgPRenderPrm.CacheSize != CfgPRenderPrm_default.CacheSize || bEchoAll)
			os << "TileCacheSize = " << CfgPRenderPrm.CacheSize << '\n';
		if (CfgPRenderPrm.bLoadOnThread != CfgPRenderPrm_default.bLoadOnThread || bEchoAll)
			os << "TileLoadThread = " << BoolStr (CfgPRenderPrm.bLoadOnThread) << '\n';
		if (CfgPRenderPrm.LoadFrequency != CfgPRenderPrm_default.LoadFrequency || bEchoAll)
			os << "PlanetTexLoadFreq = " << CfgPRenderPrm.LoadFrequency << '\n';
		if (CfgPRenderPrm.AnisoMode != CfgPRenderPrm_default.AnisoMode || bEchoAll)
			os << "PlanetAnisoMode = " << CfgPRenderPrm.AnisoMode << '\n';
		if (CfgPRenderPrm.MipmapMode != CfgPRenderPrm_default.MipmapMode || bEchoAll)
			os << "PlanetMipmapMode = " << CfgPRenderPrm.MipmapMode << '\n';
		if (CfgPRenderPrm.MipmapBias != CfgPRenderPrm_default.MipmapBias || bEchoAll)
			os << "PlanetMipmapBias = " << CfgPRenderPrm.MipmapBias << '\n';
		if (CfgPRenderPrm.PatchRes != CfgPRenderPrm_default.PatchRes || bEchoAll)
			os << "PlanetPatchGrid = " << CfgPRenderPrm.PatchRes << '\n';
		if (CfgPRenderPrm.ResolutionBias != CfgPRenderPrm_default.ResolutionBias || bEchoAll)
			os << "PlanetResolutionBias = " << CfgPRenderPrm.ResolutionBias << '\n';
		if (CfgPRenderPrm.TileLoadFlags != CfgPRenderPrm_default.TileLoadFlags || bEchoAll)
			os << "TileLoadFlags = " << CfgPRenderPrm.TileLoadFlags << '\n';
	}

	if (memcmp (&CfgMapPrm, &CfgMapPrm_default, sizeof (CFG_MAPPRM)) || bEchoAll) {
		os << "\n; === Map dialog parameters ===\n";
		if (CfgMapPrm.DispFlag != CfgMapPrm_default.DispFlag || bEchoAll)
			os << "MapDlgFlag = " << CfgMapPrm.DispFlag << '\n';
	}

	if (memcmp (&CfgCameraPrm, &CfgCameraPrm_default, sizeof(CFG_CAMERAPRM)) || bEchoAll) {
		os << "\n; === Camera parameters ===\n";
		if (CfgCameraPrm.Panspeed != CfgCameraPrm_default.Panspeed || bEchoAll)
			os << "CameraPanspeed = " << CfgCameraPrm.Panspeed << '\n';
		if (CfgCameraPrm.TerrainLimit != CfgCameraPrm_default.TerrainLimit || bEchoAll)
			os << "CameraTerrainLimit = " << CfgCameraPrm.TerrainLimit << '\n';
		if (CfgCameraPrm.HUDCol != CfgCameraPrm_default.HUDCol || bEchoAll)
			os << "HUDColIdx = " << CfgCameraPrm.HUDCol << endl;
	}

	if (CfgDevPrm.Device_idx >= 0) { // otherwise undefined
		os << "\n; === Device settings ===\n";
		os << "DeviceIndex = " << CfgDevPrm.Device_idx << '\n';
		os << "ModeIndex = " << CfgDevPrm.Device_mode << '\n';
		os << "OutputIndex = " << CfgDevPrm.Device_out << '\n';
		os << "Style = " << CfgDevPrm.Device_style << '\n';

		if (CfgDevPrm.bForceEnum != CfgDevPrm_default.bForceEnum || bEchoAll)
			os << "DeviceForceEnum = " << BoolStr (CfgDevPrm.bForceEnum) << '\n';
		if (CfgDevPrm.bFullscreen != CfgDevPrm_default.bFullscreen || bEchoAll)
			os << "Fullscreen = " << BoolStr (CfgDevPrm.bFullscreen) << '\n';
		if (CfgDevPrm.bStereo != CfgDevPrm_default.bStereo || bEchoAll)
			os << "Stereo = " << BoolStr (CfgDevPrm.bStereo) << '\n';
		if (CfgDevPrm.bNoVsync != CfgDevPrm_default.bNoVsync || bEchoAll)
			os << "NoVSync = " << BoolStr (CfgDevPrm.bNoVsync) << '\n';
		if (CfgDevPrm.bTryStencil != CfgDevPrm_default.bTryStencil || bEchoAll)
			os << "StencilBuffer = " << BoolStr (CfgDevPrm.bTryStencil) << '\n';
		if (CfgDevPrm.bPageflip != CfgDevPrm_default.bPageflip || bEchoAll)
			os << "FullscreenPageflip = " << BoolStr (CfgDevPrm.bPageflip) << '\n';
		if (CfgDevPrm.WinW != CfgDevPrm_default.WinW || bEchoAll)
			os << "WindowWidth = " << CfgDevPrm.WinW << '\n';
		if (CfgDevPrm.WinH != CfgDevPrm_default.WinH || bEchoAll)
			os << "WindowHeight = " << CfgDevPrm.WinH << '\n';
	}

	if (memcmp (&CfgJoystickPrm, &CfgJoystickPrm_default, sizeof(CFG_JOYSTICKPRM)) || bEchoAll) {
		os << "\n; === Joystick parameters ===\n";
		if (CfgJoystickPrm.Joy_idx != CfgJoystickPrm_default.Joy_idx || bEchoAll)
			os << "JoystickIndex = " << CfgJoystickPrm.Joy_idx << '\n';
		if (CfgJoystickPrm.ThrottleAxis != CfgJoystickPrm_default.ThrottleAxis || bEchoAll)
			os << "JoystickThrottleAxis = " << CfgJoystickPrm.ThrottleAxis << '\n';
		if (CfgJoystickPrm.ThrottleSaturation != CfgJoystickPrm_default.ThrottleSaturation || bEchoAll)
			os << "JoystickThrottleSaturation = " << CfgJoystickPrm.ThrottleSaturation << '\n';
		if (CfgJoystickPrm.Deadzone != CfgJoystickPrm_default.Deadzone || bEchoAll)
			os << "JoystickDeadzone = " << CfgJoystickPrm.Deadzone << '\n';
		if (CfgJoystickPrm.bThrottleIgnore != CfgJoystickPrm_default.bThrottleIgnore || bEchoAll)
			os << "IgnoreThrottleOnStart = " << BoolStr (CfgJoystickPrm.bThrottleIgnore) << '\n';
	}

	if (memcmp (&CfgUIPrm, &CfgUIPrm_default, sizeof(CFG_UIPRM)) || bEchoAll) {
		os << "\n; === User interface parameters ===\n";
		if (CfgUIPrm.MouseFocusMode != CfgUIPrm_default.MouseFocusMode || bEchoAll)
			os << "MouseFocusMode = " << CfgUIPrm.MouseFocusMode << '\n';
		if (CfgUIPrm.MenuMode != CfgUIPrm_default.MenuMode || bEchoAll)
			os << "MenubarMode = " << CfgUIPrm.MenuMode << '\n';
		if (CfgUIPrm.bMenuLabelAlways != CfgUIPrm_default.bMenuLabelAlways || bEchoAll)
			os << "MenubarLabelAlways = " << BoolStr (CfgUIPrm.bMenuLabelAlways) << '\n';
		if (CfgUIPrm.MenuButtonSize != CfgUIPrm_default.MenuButtonSize || bEchoAll)
			os << "MenuButtonSize = " << CfgUIPrm.MenuButtonSize << '\n';
		if (CfgUIPrm.MenuButtonHoverSize != CfgUIPrm_default.MenuButtonHoverSize || bEchoAll)
			os << "MenuButtonHoverSize = " << CfgUIPrm.MenuButtonHoverSize << '\n';
		if (CfgUIPrm.MenuButtonSpacing != CfgUIPrm_default.MenuButtonSpacing || bEchoAll)
			os << "MenuButtonSpacing = " << CfgUIPrm.MenuButtonSpacing << '\n';
		if (CfgUIPrm.bWarpAlways != CfgUIPrm_default.bWarpAlways || bEchoAll)
			os << "ShowWarpAlways = " << BoolStr (CfgUIPrm.bWarpAlways) << '\n';
		if (CfgUIPrm.InfoMode != CfgUIPrm_default.InfoMode || bEchoAll)
			os << "InfobarMode = " << CfgUIPrm.InfoMode << '\n';
		if (CfgUIPrm.FPS != CfgUIPrm_default.FPS || bEchoAll)
			os << "FPS = " << CfgUIPrm.FPS << '\n';
		if (CfgUIPrm.MenuOpacity != CfgUIPrm_default.MenuOpacity || bEchoAll)
			os << "MenubarOpacity = " << CfgUIPrm.MenuOpacity << '\n';
		if (CfgUIPrm.InfoOpacity != CfgUIPrm_default.InfoOpacity || bEchoAll)
			os << "InfobarOpacity = " << CfgUIPrm.InfoOpacity << '\n';
		if (CfgUIPrm.MenuScrollspeed != CfgUIPrm_default.MenuScrollspeed || bEchoAll)
			os << "MenubarSpeed = " << CfgUIPrm.MenuScrollspeed << '\n';
		if (CfgUIPrm.SelVesselTab != CfgUIPrm_default.SelVesselTab || bEchoAll)
			os << "SelVesselTab = " << CfgUIPrm.SelVesselTab << '\n';
		if (CfgUIPrm.SelVesselRange != CfgUIPrm_default.SelVesselRange || bEchoAll)
			os << "SelVesselRange = " << CfgUIPrm.SelVesselRange << '\n';
		if (CfgUIPrm.bSelVesselFlat != CfgUIPrm_default.bSelVesselFlat || bEchoAll)
			os << "SelVesselFlat = " << BoolStr (CfgUIPrm.bSelVesselFlat) << '\n';
	}

	if (memcmp (&CfgDemoPrm, &CfgDemoPrm_default, sizeof(CFG_DEMOPRM)) || bEchoAll) {
		os << "\n; === Demo parameters ===\n";
		if (CfgDemoPrm.bDemo != CfgDemoPrm_default.bDemo || bEchoAll)
			os << "DemoMode = " << BoolStr (CfgDemoPrm.bDemo) << '\n';
		if (CfgDemoPrm.bBkImage != CfgDemoPrm_default.bBkImage || bEchoAll)
			os << "BackgroundImage = " << BoolStr (CfgDemoPrm.bBkImage) << '\n';
		if (CfgDemoPrm.bBlockExit != CfgDemoPrm_default.bBlockExit || bEchoAll)
			os << "BlockExit = " << BoolStr (CfgDemoPrm.bBlockExit) << '\n';
		if (CfgDemoPrm.MaxDemoTime != CfgDemoPrm_default.MaxDemoTime || bEchoAll)
			os << "MaxDemoTime = " << CfgDemoPrm.MaxDemoTime << '\n';
		if (CfgDemoPrm.LPIdleTime != CfgDemoPrm_default.LPIdleTime || bEchoAll)
			os << "MaxLaunchpadIdleTime = " << CfgDemoPrm.LPIdleTime << '\n';
	}

	if (memcmp (&CfgRecPlayPrm, &CfgRecPlayPrm_default, sizeof(CFG_RECPLAYPRM)) || bEchoAll) {
		os << "\n; === Record/play parameters ===\n";
		if (CfgRecPlayPrm.RecordPosFrame != CfgRecPlayPrm_default.RecordPosFrame || bEchoAll)
			os << "RecordPosFrame = " << CfgRecPlayPrm.RecordPosFrame << '\n';
		if (CfgRecPlayPrm.RecordAttFrame != CfgRecPlayPrm_default.RecordAttFrame || bEchoAll)
			os << "RecordAttFrame = " << CfgRecPlayPrm.RecordAttFrame << '\n';
		if (CfgRecPlayPrm.bRecordWarp != CfgRecPlayPrm_default.bRecordWarp || bEchoAll)
			os << "RecordTimewarp = " << BoolStr (CfgRecPlayPrm.bRecordWarp) << '\n';
		if (CfgRecPlayPrm.bRecordFocus != CfgRecPlayPrm_default.bRecordFocus || bEchoAll)
			os << "RecordFocusEvent = " << BoolStr (CfgRecPlayPrm.bRecordFocus) << '\n';
		if (CfgRecPlayPrm.bReplayWarp != CfgRecPlayPrm_default.bReplayWarp || bEchoAll)
			os << "ReplayTimewarp = " << BoolStr (CfgRecPlayPrm.bReplayWarp) << '\n';
		if (CfgRecPlayPrm.bReplayFocus != CfgRecPlayPrm_default.bReplayFocus || bEchoAll)
			os << "ReplayFocusEvent = " << BoolStr (CfgRecPlayPrm.bReplayFocus) << '\n';
		if (CfgRecPlayPrm.bReplayCam != CfgRecPlayPrm_default.bReplayCam || bEchoAll)
			os << "ReplayCameraEvent = " << BoolStr (CfgRecPlayPrm.bReplayCam) << '\n';
		if (CfgRecPlayPrm.bSysInterval != CfgRecPlayPrm_default.bSysInterval || bEchoAll)
			os << "SystimeSampling = " << BoolStr (CfgRecPlayPrm.bSysInterval) << '\n';
		if (CfgRecPlayPrm.bShowNotes != CfgRecPlayPrm_default.bShowNotes || bEchoAll)
			os << "PlaybackNotes = " << BoolStr (CfgRecPlayPrm.bShowNotes) << '\n';
	}

	if (memcmp (&CfgFontPrm, &CfgFontPrm_default, sizeof(CFG_FONTPRM)) || bEchoAll) {
		os << "\n; === Font parameters ===\n";
		if (CfgFontPrm.dlgFont_Scale != CfgFontPrm_default.dlgFont_Scale || bEchoAll)
			os << "DialogFont_Scale = " << CfgFontPrm.dlgFont_Scale << '\n';
		if (strcmp (CfgFontPrm.dlgFont1_Face, CfgFontPrm_default.dlgFont1_Face) || bEchoAll)
			os << "DialogFont1_Face = " << CfgFontPrm.dlgFont1_Face << '\n';
		if (CfgFontPrm.ImGui_FontSize != CfgFontPrm_default.ImGui_FontSize || bEchoAll)
			os << "ImGui_FontSize = " << CfgFontPrm.ImGui_FontSize << '\n';
		if (strcmp (CfgFontPrm.ImGui_DefaultFontFile, CfgFontPrm_default.ImGui_DefaultFontFile) || bEchoAll)
			os << "ImGui_DefaultFontFile = " << CfgFontPrm.ImGui_DefaultFontFile << '\n';
		if (strcmp (CfgFontPrm.ImGui_ConsoleFontFile, CfgFontPrm_default.ImGui_ConsoleFontFile) || bEchoAll)
			os << "ImGui_ConsoleFontFile = " << CfgFontPrm.ImGui_ConsoleFontFile << '\n';
		if (strcmp (CfgFontPrm.ImGui_MonospacedFontFile, CfgFontPrm_default.ImGui_MonospacedFontFile) || bEchoAll)
			os << "ImGui_MonospacedFontFile = " << CfgFontPrm.ImGui_MonospacedFontFile << '\n';
		if (strcmp (CfgFontPrm.ImGui_ManuscriptFontFile, CfgFontPrm_default.ImGui_ManuscriptFontFile) || bEchoAll)
			os << "ImGui_ManuscriptFontFile = " << CfgFontPrm.ImGui_ManuscriptFontFile << '\n';
	}

	if (memcmp (&CfgWindowPos, &CfgWindowPos_default, sizeof(CFG_WINDOWPOS)) || bEchoAll) {
		os << "\n; === Window positions ===\n";
		if (CfgWindowPos.DlgMap != CfgWindowPos_default.DlgMap || bEchoAll)
			os << "DlgMapPos = " << CfgWindowPos.DlgMap << '\n';
		if (CfgWindowPos.DlgInfo != CfgWindowPos_default.DlgInfo || bEchoAll)
			os << "DlgInfoPos = " << CfgWindowPos.DlgInfo << '\n';
		if (CfgWindowPos.DlgCamera != CfgWindowPos_default.DlgCamera || bEchoAll)
			os << "DlgCamPos = " << CfgWindowPos.DlgCamera << '\n';
		if (CfgWindowPos.DlgFocus != CfgWindowPos_default.DlgFocus || bEchoAll)
			os << "DlgFocusPos = " << CfgWindowPos.DlgFocus << '\n';
		if (CfgWindowPos.DlgTacc != CfgWindowPos_default.DlgTacc || bEchoAll)
			os << "DlgTaccPos = " << CfgWindowPos.DlgTacc << '\n';
		if (CfgWindowPos.DlgOptions != CfgWindowPos_default.DlgOptions || bEchoAll)
			os << "DlgOptionsPos = " << CfgWindowPos.DlgOptions << '\n';
		if (CfgWindowPos.DlgVishelper != CfgWindowPos_default.DlgVishelper || bEchoAll)
			os << "DlgVhelperPos = " << CfgWindowPos.DlgVishelper << '\n';
		if (CfgWindowPos.LaunchpadScnListWidth != CfgWindowPos_default.LaunchpadScnListWidth || bEchoAll)
			os << "LpadScnListWidth = " << CfgWindowPos.LaunchpadScnListWidth << '\n';
		if (CfgWindowPos.LaunchpadModListWidth != CfgWindowPos_default.LaunchpadModListWidth || bEchoAll)
			os << "LpadModListWidth = " << CfgWindowPos.LaunchpadModListWidth << '\n';
		if (CfgWindowPos.LaunchpadExtListWidth != CfgWindowPos_default.LaunchpadExtListWidth || bEchoAll)
			os << "LpadExtListWidth = " << CfgWindowPos.LaunchpadExtListWidth << '\n';
	}

	if (m_activeModules.size()) {
		os << "\n; === Active plugin list ===" << endl;
		os << "ACTIVE_MODULES" << endl;
		for (auto name : m_activeModules)
			os << "  " << name << std::endl;
		os << "END_MODULES" << endl;
	}
	os << flush;
	return true;
}

char *Config::ConfigPath (const char *name) const
{
	snprintf (cfgpath+cfglen, sizeof(cfgpath)-cfglen, "%s.cfg", name);
	return cfgpath;
}

char *Config::ConfigPathNoext (const char *name)
{
	snprintf (cfgpath+cfglen, sizeof(cfgpath)-cfglen, "%s", name);
	return cfgpath;
}

char *Config::MeshPath (const char *name)
{
	snprintf (mshpath+mshlen, sizeof(mshpath)-mshlen, "%s.msh", name);
	return mshpath;
}

char *Config::TexPath (const char *name, const char *ext)
{
	snprintf (texpath+texlen, sizeof(texpath)-texlen, "%s%s", name, ext ? ext : ".dds");
	return texpath;
}

char *Config::HTexPath (const char *name, const char *ext)
{
	if (!htxlen) return 0;
	snprintf (htxpath+htxlen, sizeof(htxpath)-htxlen, "%s%s", name, ext ? ext : ".dds");
	return htxpath;
}

char* Config::PTexPath(const char* name, const char* ext)
{
	if (!ptxlen) return 0;
	snprintf(ptxpath + ptxlen, sizeof(ptxpath) - ptxlen, "%s%s", name, ext ? ext : "");
	return ptxpath;
}

const char *Config::ScnPath (const char *name)
{
	if (name[1] == ':') { // assume full absolute path
		return name;
	} else {
		strcpy (scnpath+scnlen, name);
		return strcat (scnpath, ".scn");
	}
}

void Config::TexPath (char *cbuf, const char *name, const char *ext)
{
	strncpy (cbuf, texpath, texlen);
	if (ext) sprintf (cbuf+texlen, "%s.%s", name, ext);
	else     strcpy (cbuf+texlen, name);
}

void Config::PTexPath(char* cbuf, const char* name, const char* ext)
{
	strncpy(cbuf, ptxpath, ptxlen);
	if (ext) sprintf(cbuf + ptxlen, "%s.%s", name, ext);
	else     strcpy(cbuf + ptxlen, name);
}

bool Config::IsActiveModule(const std::string& name)
{
	return (std::find(m_activeModules.begin(), m_activeModules.end(), name) != m_activeModules.end());
}

void Config::AddActiveModule (const std::string& name)
{
	if (std::find(m_activeModules.begin(), m_activeModules.end(), name) == m_activeModules.end()) // only add if not already present
		m_activeModules.push_back(name);
}

void Config::DelActiveModule (const std::string& name)
{
	auto it = std::find(m_activeModules.begin(), m_activeModules.end(), name);
	if (it != m_activeModules.end()) // erase if present
		m_activeModules.erase(it);
}

bool Config::GetString (std::istream &is, const char *category, char *val)
{
	return GetString (is, category, val, 512);
}

bool Config::GetString (istream &is, const char *category, char *val, size_t size)
{
	char cbuf[512];
	int i;

	is.clear();
	is.seekg (0, ios::beg);
	while (getline_trunc (is, cbuf, 512) && strncmp (cbuf, category, strlen(category)));
	if (!is.good()) {
		is.clear();
		return false;
	}

	// cut comments
	for (i = 0; cbuf[i] && cbuf[i] != ';'; i++);
	cbuf[i] = '\0';

	// find value
	for (i = 0; cbuf[i] && cbuf[i] != '='; i++);
	if (!cbuf[i]) return false;
	i++;
	while (cbuf[i] == ' ' || cbuf[i] == '\t') i++;
	snprintf (val, size, "%s", cbuf+i);
	return true;
}

bool Config::GetReal (std::istream &is, const char *category, double &val)
{
	if (!GetString (is, category, g_cbuf)) return false;
	return (sscanf (g_cbuf, "%lf", &val) == 1);
}

bool Config::GetInt (std::istream &is, const char *category, int &val)
{
	if (!GetString (is, category, g_cbuf)) return false;
	return (sscanf (g_cbuf, "%d", &val) == 1);
}

bool Config::GetSize (std::istream& is, const char* category, size_t& val)
{
	if (!GetString(is, category, g_cbuf)) return false;
	return (sscanf(g_cbuf, "%zu", &val) == 1);
}

bool Config::GetBool (std::istream &is, const char *category, bool &val)
{
	if (!GetString (is, category, g_cbuf)) return false;
	if (!_strnicmp (g_cbuf, "true", 4)) { val = true; return true; }
	else if (!_strnicmp (g_cbuf, "false", 5)) { val = false; return true; }
	return false;
}

bool Config::GetVector (std::istream &is, const char *category, Vector &val)
{
	double x, y, z;
	if (!GetString (is, category, g_cbuf)) return false;
	if (sscanf (g_cbuf, "%lf%lf%lf", &x, &y, &z) < 3) return false;
	val.Set (x, y, z);
	return true;
}
