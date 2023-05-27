// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//                 ORBITER MODULE: ScriptVessel
//                   Part of the ORBITER SDK
//
// ScriptVessel.cpp
// Control module for ScriptVessel vessel class
//
// Notes:
// Implementation of a generic vessel class that acts as an
// interface to script-driven vessel definitions.
// This class creates an interpreter instance, loads a vessel class-
// specific script and and implements the VESSEL2 callback functions
// by calling corresponding script functions.
// ==============================================================

#define STRICT
#define ORBITER_MODULE

extern "C" {
#include "Lua\lua.h"
}
#include "orbitersdk.h"

const int NCLBK        = 4;
const int SETCLASSCAPS = 0;
const int POSTCREATION = 1;
const int PRESTEP      = 2;
const int POSTSTEP     = 3;

const char *CLBKNAME[NCLBK] = {
	"setclasscaps", "postcreation", "prestep", "poststep"
};

// Calculate lift coefficient [Cl] as a function of aoa (angle of attack) over -Pi ... Pi
// Implemented here as a piecewise linear function
double LiftCoeff (double aoa)
{
	int i;
	const int nlift = 9;
	static const double AOA[nlift] = {-180*RAD,-60*RAD,-30*RAD,-1*RAD,15*RAD,20*RAD,25*RAD,60*RAD,180*RAD};
	static const double CL[nlift]  = {       0,      0,   -0.1,     0,   0.2,  0.25,   0.2,     0,      0};
	static const double SCL[nlift] = {(CL[1]-CL[0])/(AOA[1]-AOA[0]), (CL[2]-CL[1])/(AOA[2]-AOA[1]),
		                              (CL[3]-CL[2])/(AOA[3]-AOA[2]), (CL[4]-CL[3])/(AOA[4]-AOA[3]),
									  (CL[5]-CL[4])/(AOA[5]-AOA[4]), (CL[6]-CL[5])/(AOA[6]-AOA[5]),
									  (CL[7]-CL[6])/(AOA[7]-AOA[6]), (CL[8]-CL[7])/(AOA[8]-AOA[7])};
	for (i = 0; i < nlift-1 && AOA[i+1] < aoa; i++);
	return CL[i] + (aoa-AOA[i])*SCL[i];
}

// ==============================================================
// ScriptVessel class interface
// ==============================================================

class ScriptVessel: public VESSEL2 {
public:
	ScriptVessel (OBJHANDLE hVessel, int flightmodel);
	~ScriptVessel ();
	void clbkSetClassCaps (FILEHANDLE cfg);
	void clbkPostCreation ();
	void clbkPreStep (double simt, double simdt, double mjd);
	void clbkPostStep (double simt, double simdt, double mjd);

protected:
	INTERPRETERHANDLE hInterp;
	lua_State *L;

	bool bclbk[NCLBK];
	char func[256];
};

// ==============================================================
// Constructor/destructor
// ==============================================================
ScriptVessel::ScriptVessel (OBJHANDLE hVessel, int flightmodel): VESSEL2 (hVessel, flightmodel)
{
	// create the interpreter instance to run the vessel script
	hInterp = oapiCreateInterpreter();
	L = oapiGetLua (hInterp);
	strcpy (func, "clbk_");
}

ScriptVessel::~ScriptVessel ()
{
	// delete the interpreter instance
	oapiDelInterpreter (hInterp);
}

// ==============================================================
// Overloaded callback functions
// ==============================================================

// --------------------------------------------------------------
// Set the capabilities of the vessel class
// --------------------------------------------------------------
void ScriptVessel::clbkSetClassCaps (FILEHANDLE cfg)
{
	char script[256], cmd[256];
	int i;

	// Load the vessel script
	oapiReadItem_string (cfg, (char*)"Script", script);
	sprintf (cmd, "run_global('Config/Vessels/%s')", script);
	oapiExecScriptCmd (hInterp, cmd);

	// Define the vessel instance
	lua_pushlightuserdata (L, GetHandle());  // push vessel handle
	lua_setfield (L, LUA_GLOBALSINDEX, "hVessel");
	strcpy (cmd, "vi = vessel.get_interface(hVessel)");
	oapiExecScriptCmd (hInterp, cmd);

	// check for defined callback functions in script
	for (i = 0; i < NCLBK; i++) {
		strcpy (func+5, CLBKNAME[i]);
		lua_getfield (L, LUA_GLOBALSINDEX, func);
		bclbk[i] = (lua_isfunction (L,-1) != 0);
		lua_pop(L,1);
	}

	// Run the SetClassCaps function
	if (bclbk[SETCLASSCAPS]) {
		strcpy (func+5, "setclasscaps");
		lua_getfield (L, LUA_GLOBALSINDEX, func);
		lua_pushlightuserdata (L, cfg);
		lua_call (L, 1, 0);
	}
}

void ScriptVessel::clbkPostCreation ()
{
	if (bclbk[POSTCREATION]) {
		strcpy (func+5, "postcreation");
		lua_getfield (L, LUA_GLOBALSINDEX, func);
		lua_call (L, 0, 0);
	}
}

void ScriptVessel::clbkPreStep (double simt, double simdt, double mjd)
{
	if (bclbk[PRESTEP]) {
		strcpy (func+5, "prestep");
		lua_getfield (L, LUA_GLOBALSINDEX, func);
		lua_pushnumber(L,simt);
		lua_pushnumber(L,simdt);
		lua_pushnumber(L,mjd);
		lua_call (L, 3, 0);
	}
}

void ScriptVessel::clbkPostStep (double simt, double simdt, double mjd)
{
	if (bclbk[POSTSTEP]) {
		strcpy (func+5, "poststep");
		lua_getfield (L, LUA_GLOBALSINDEX, func);
		lua_pushnumber(L,simt);
		lua_pushnumber(L,simdt);
		lua_pushnumber(L,mjd);
		lua_call (L, 3, 0);
	}
}

// ==============================================================
// API callback interface
// ==============================================================

// --------------------------------------------------------------
// Vessel initialisation
// --------------------------------------------------------------
DLLCLBK VESSEL *ovcInit (OBJHANDLE hvessel, int flightmodel)
{
	return new ScriptVessel (hvessel, flightmodel);
}

// --------------------------------------------------------------
// Vessel cleanup
// --------------------------------------------------------------
DLLCLBK void ovcExit (VESSEL *vessel)
{
	if (vessel) delete (ScriptVessel*)vessel;
}
