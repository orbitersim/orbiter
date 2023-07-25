#define STRICT 1
#define ORBITER_MODULE

#include <stdio.h>
#include "TrackIR.h"
#include "TrackIRconfig.h"

#include "NPClient.h"
#include "NPClientWraps.h"

using std::min;
using std::max;

// ==============================================================
// Global parameters

TrackIR *trackir = 0;
GPARAMS gParams;

// ==============================================================
// Local prototypes

bool GetDllLocation (LPTSTR pszPath);

// ==============================================================
// class TrackIR

TrackIR::TrackIR (): ExternalCameraControl (CAMDATA_POS|CAMDATA_DIR, CAMMODE_VC)
{
	// override some of the parent's defaults
	vcmode.rotationrange = 150*PI;
	vcmode.freeze_t = 2.0;

	// read custom parameters from file
	ReadData ();

	strcpy (dllPath, "Not found");
	strcpy (cVersion, "Not active");

    // Locate the TrackIR Enhanced DLL
	if (connected = GetDllLocation (dllPath)) {
		char cbuf[256] = "TrackIR module found: ";
		strcat (cbuf, dllPath);
		oapiWriteLog (cbuf);
	} else {
		oapiWriteLog ((char*)"TrackIR module not found.");
		return;
	}

    // Initialize the the TrackIR Enhanced DLL
	if (connected = (NPClient_Init (dllPath) == NP_OK)) {
		oapiWriteLog ((char*)"TrackIR initialised");
	} else {
		oapiWriteLog ((char*)"TrackIR initialisation failed");
		return;
	}

	// Query for the NaturalPoint TrackIR software version
	unsigned short wNPClientVer;
	if (NP_QueryVersion (&wNPClientVer) == NP_OK) {
		sprintf (cVersion, "%d.%02d", wNPClientVer >> 8, wNPClientVer & 0x00FF);
		char cbuf[256] = "NaturalPoint software version ";
		strcat (cbuf, cVersion);
		//sprintf (cbuf, "NaturalPoint software version %d.%02d", wNPClientVer >> 8, wNPClientVer & 0x00FF);
		oapiWriteLog (cbuf);
	} else {
		oapiWriteLog ((char*)"Error querying NaturalPoint software version");
	}

	laststate = 0;
}

// ==============================================================

TrackIR::~TrackIR ()
{
	extern HMODULE ghNPClientDLL;
	if (ghNPClientDLL) FreeLibrary (ghNPClientDLL);
}

// ==============================================================

bool TrackIR::ReadData ()
{
	// read configuration data
	char cbuf[256];
	int i, mode;
	double t, s;
	FILEHANDLE hFile = oapiOpenFile (cfgfile, FILE_IN, ROOT);
	if (!hFile) return false;
	if (oapiReadItem_int (hFile, (char*)"CMODE", mode)) cameramode = mode;
	if (oapiReadItem_string (hFile, (char*)"VCROT", cbuf) && sscanf (cbuf, "%d%lf", &mode, &t) == 2) {
		vcmode.trackrotation = (mode ? true:false);
		vcmode.rotationrange = max (1.0*RAD, min (PI, t));
	}
	if (oapiReadItem_string (hFile, (char*)"VCPOS", cbuf) && sscanf (cbuf, "%d%lf", &mode, &t) == 2) {
		vcmode.trackposition = (mode ? true:false);
		vcmode.positionrange = max (0.0, min (2.0, t));
	}
	if (oapiReadItem_string (hFile, (char*)"FREEZE", cbuf) && sscanf (cbuf, "%d%lf", &mode, &t) == 2) {
		vcmode.freezeonmouse = (mode ? true:false);
		vcmode.freeze_t = max (0.0, t);
	}
	if (oapiReadItem_string (hFile, (char*)"TRKROT", cbuf) && sscanf (cbuf, "%d%d", &mode, &i) == 2) {
		trkmode.trackrotation = (mode ? true:false);
		trkmode.rotationdata = (i ? TrackMode::BYPOSITION : TrackMode::BYROTATION);
	}
	if (oapiReadItem_string (hFile, (char*)"TRKZOOM", cbuf) && sscanf (cbuf, "%d", &mode) == 1) {
		trkmode.trackzoom = (mode ? true:false);
	}
	if (oapiReadItem_string (hFile, (char*)"TRKPRM", cbuf) && sscanf (cbuf, "%lf%lf", &t, &s) == 2) {
		trkmode.deadzone = t;
		trkmode.speed = s;
	}
	oapiCloseFile (hFile, FILE_IN);
	return true;
}

// ==============================================================

void TrackIR::StartSimulation (HWND hWnd)
{
    // Register your applications Window Handle 
	if (NP_RegisterWindowHandle (hWnd) == NP_OK)
		oapiWriteLog ((char*)"NPClient: Simulation window registered.");
	else
		oapiWriteLog ((char*)"NPClient: Error registering simulation window.");

#ifdef UNDEF
	// Query for the NaturalPoint TrackIR software version
	unsigned short wNPClientVer;
	if (NP_QueryVersion (&wNPClientVer) == NP_OK) {
		char cbuf[256];
		sprintf (cbuf, "NaturalPoint software version %d.%02d", wNPClientVer >> 8, wNPClientVer & 0x00FF);
		oapiWriteLog (cbuf);
	} else {
		oapiWriteLog ("Error querying NaturalPoint software version");
	}
#endif

    // Choose the Axes that you want tracking data for
	unsigned int DataFields = 0;
    
    // Rotation Axes
	DataFields |= NPPitch;
	DataFields |= NPYaw;
	DataFields |= NPRoll;

    // Translation Axes
	DataFields |= NPX;
	DataFields |= NPY;
	DataFields |= NPZ;

    // Register the Axes selection with the TrackIR Enhanced interface
	NP_RequestData(DataFields);

    // Your assigned developer ID needs to be inserted below!    
#define NP_DEVELOPER_ID 10303
    NP_RegisterProgramProfileID (NP_DEVELOPER_ID);

    // Stop the cursor
	if (NP_StopCursor() == NP_OK)
		oapiWriteLog ((char*)"NPClient: Cursor stopped");
	else
		oapiWriteLog ((char*)"NPClient: Error stopping cursor.");

    // Request that the TrackIR software begins sending Tracking Data
	if (NP_StartDataTransmission() == NP_OK)
		oapiWriteLog ((char*)"NPClient: Data transmission started");
	else
		oapiWriteLog ((char*)"NPClient: Error starting data transmission");
}

// ==============================================================

void TrackIR::EndSimulation ()
{
    // Request that the TrackIR software stop sending Tracking Data
    NP_StopDataTransmission();   
    
	// ??
	NP_StartCursor();

    // Un-register your applications Windows Handle
    NP_UnregisterWindowHandle();
}

// ==============================================================

bool TrackIR::clbkPoll (CamData *data)
{
	TRACKIRDATA tid;
	if (NP_GetData (&tid) != NP_OK) return false;             // polling failure
	if (tid.wNPStatus != NPSTATUS_REMOTEACTIVE) return false; // deactivated
	if (tid.wPFrameSignature == laststate) return false;      // no change
	laststate = tid.wPFrameSignature;

	static double anglescale = PI/16383.0;
	data->yaw   = tid.fNPYaw  *  anglescale;
	data->pitch = tid.fNPPitch* -anglescale;
	data->roll  = tid.fNPRoll *  anglescale;

	static double linscale = 1.0/16383.0; // assume 0.5m total range
	data->x     = tid.fNPX * -linscale;
	data->y     = tid.fNPY *  linscale;
	data->z     = tid.fNPZ * -linscale;

	return true;
}

// ==============================================================
// Auxiliary functions

bool GetDllLocation (LPTSTR pszPath)
{
	if (pszPath == NULL) return false;

	//find path to NPClient.dll
	HKEY pKey = NULL;
	//open the registry key 
	if (::RegOpenKeyEx(HKEY_CURRENT_USER,
			"Software\\NaturalPoint\\NATURALPOINT\\NPClient Location",
			0, KEY_READ, &pKey) != ERROR_SUCCESS)
		return false;


	//get the value from the key
	LPTSTR pszValue;
	DWORD dwSize;
	//first discover the size of the value
	if (RegQueryValueEx(pKey, "Path", NULL, NULL, NULL, &dwSize) == ERROR_SUCCESS) {
		//allocate memory for the buffer for the value
		pszValue = (LPTSTR) malloc(dwSize);
		if (pszValue != NULL) { // insufficient memory
	        //now get the value
		    if (RegQueryValueEx(pKey, "Path", NULL, NULL, (LPBYTE)pszValue, &dwSize) == ERROR_SUCCESS) {
				//everything worked
				::RegCloseKey(pKey);
				strcpy(pszPath, pszValue);
				free(pszValue);
				return true;
			}
		}
	}
	::RegCloseKey(pKey);
    strcpy(pszPath, "Error");
	return false;
}

// ==============================================================
// Simulation window startup

DLLCLBK void opcOpenRenderViewport (HWND renderWnd, DWORD width, DWORD height, BOOL fullscreen)
{
	if (trackir && trackir->Connected()) {
		trackir->StartSimulation (renderWnd);
		// connect to the render window
		oapiRegisterExternalCameraControl (trackir);
		// register external control with orbiter
	}
}

// ==============================================================
// Simulation window closedown

DLLCLBK void opcCloseRenderViewport (void)
{
	if (trackir && trackir->Connected()) {
		oapiUnregisterExternalCameraControl();
		// disconnect external control from orbiter
		trackir->EndSimulation ();
	}
}

// ==============================================================
// The DLL entry point
// ==============================================================

DLLCLBK void InitModule (HINSTANCE hDLL)
{
	gParams.hInst = hDLL;
	trackir = new TrackIR;
	// create the TrackIR interface
	gParams.item = new TrackIRconfig (trackir);
	// create the new config item
	oapiRegisterLaunchpadItem (gParams.item, NULL);
	// register the TrackIR config entry
}

// ==============================================================
// The DLL exit point
// ==============================================================

DLLCLBK void ExitModule (HINSTANCE hDLL)
{
	oapiUnregisterLaunchpadItem (gParams.item);
	// Unregister the launchpad item
	delete gParams.item;
	// and delete it
	delete trackir;
	trackir = 0;
	// and delete the interface
}