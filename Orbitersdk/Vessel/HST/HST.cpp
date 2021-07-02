// ==============================================================
//                    ORBITER MODULE: HST
//                  Part of the ORBITER SDK
//          Copyright (C) 2001-2007 Martin Schweiger
//                   All rights reserved
//
// HST.cpp
// HST basic specs and animations
//
// HST mesh and textures by David Sundstrom
// ==============================================================

#define ORBITER_MODULE

#include "HST.h"
#include <stdio.h>

// ==============================================================
// HST class implementation
// ==============================================================

// --------------------------------------------------------------
// Constructor
// --------------------------------------------------------------
HST::HST (OBJHANDLE hObj, int fmodel)
: VESSEL3 (hObj, fmodel)
{
	ant_proc = 0.0;
	ant_status = DOOR_CLOSED;
	hatch_proc = 0.0;
	hatch_status = DOOR_CLOSED;
	array_proc = 1.0;
	array_status = DOOR_OPEN;
	DefineAnimations ();
}

// --------------------------------------------------------------
// Define animation sequences for moving parts
// --------------------------------------------------------------
void HST::DefineAnimations (void)
{
	// 1. Hi-gain antenna
	static UINT HiGainAnt1Grp[2] = {1,3};
	static MGROUP_ROTATE HiGainAnt1 (0, HiGainAnt1Grp, 2, _V(0.002579,1.993670,0.238158), _V(-1,0,0), (float)(PI*0.51));
	static UINT HiGainAnt2Grp[2] = {0,2};
	static MGROUP_ROTATE HiGainAnt2 (0, HiGainAnt2Grp, 2, _V(0.002740,-2.013091,0.238118), _V(1,0,0), (float)(PI*0.51));
	anim_ant = CreateAnimation (0.0196);
	AddAnimationComponent (anim_ant, 0, 0.5, &HiGainAnt1);
	AddAnimationComponent (anim_ant, 0, 1,   &HiGainAnt2);

	// 2. Main telescope hatch
	static UINT HatchGrp[1] = {86};
	static MGROUP_ROTATE Hatch (0, HatchGrp, 1, _V(0.089688,1.456229,7.526453), _V(-1,0,0), (float)(RAD*113));
	anim_hatch = CreateAnimation (0);
	AddAnimationComponent (anim_hatch, 0, 1, &Hatch);

	// 3. Solar arrays - folding
	anim_array = CreateAnimation (1);
	static UINT ArrayLFoldGrp[5] = {87,88,89,90,103};
	static UINT ArrayRFoldGrp[5] = {92,93,94,95,102};
	static MGROUP_ROTATE ArrayLFold1 (0, ArrayLFoldGrp, 5, _V(-1.9, 0.053583,1.429349), _V(0,-1,0), (float)(PI*0.5));
	AddAnimationComponent (anim_array, 0,   0.4, &ArrayLFold1);
	static MGROUP_ROTATE ArrayLFold2 (0, ArrayLFoldGrp, 5, _V(0,0.053583,1.429349), _V(-1,0,0), (float)(PI*0.5));
	AddAnimationComponent (anim_array, 0.4, 0.6, &ArrayLFold2);
	static MGROUP_SCALE  ArrayLFold3 (0, ArrayLFoldGrp, 4, _V(0,0.053583,1.429349), _V(1,1,4));
	AddAnimationComponent (anim_array, 0.6, 1,   &ArrayLFold3);
	static MGROUP_ROTATE ArrayRFold1 (0, ArrayRFoldGrp, 5, _V( 1.9, 0.053583,1.429349), _V(0, 1,0), (float)(PI*0.5));
	AddAnimationComponent (anim_array, 0,   0.4, &ArrayRFold1);
	static MGROUP_ROTATE ArrayRFold2 (0, ArrayRFoldGrp, 5, _V(0,0.053583,1.429349), _V(-1,0,0), (float)(PI*0.5));
	AddAnimationComponent (anim_array, 0.4, 0.6, &ArrayRFold2);
	static MGROUP_SCALE  ArrayRFold3 (0, ArrayRFoldGrp, 4, _V(0,0.053583,1.429349), _V(1,1,4));
	AddAnimationComponent (anim_array, 0.6, 1,   &ArrayRFold3);
}

void HST::ActivateAntenna (DoorStatus action)
{
	ant_status = action;
}

void HST::RevertAntenna (void)
{
	ActivateAntenna ((ant_status == DOOR_CLOSED || ant_status == DOOR_CLOSING) ?
		DOOR_OPENING : DOOR_CLOSING);
}

void HST::ActivateHatch (DoorStatus action)
{
	hatch_status = action;
}

void HST::RevertHatch (void)
{
	ActivateHatch ((hatch_status == DOOR_CLOSED || hatch_status == DOOR_CLOSING) ?
		DOOR_OPENING : DOOR_CLOSING);
}

void HST::ActivateArray (DoorStatus action)
{
	array_status = action;
}

void HST::RevertArray (void)
{
	ActivateArray ((array_status == DOOR_CLOSED || array_status == DOOR_CLOSING) ?
		DOOR_OPENING : DOOR_CLOSING);
}

// ==============================================================
// Overloaded callback functions
// ==============================================================

// --------------------------------------------------------------
// Set vessel class parameters
// --------------------------------------------------------------
void HST::clbkSetClassCaps (FILEHANDLE cfg)
{
	SetSize (HST_SIZE);
	SetEmptyMass (HST_EMPTY_MASS);
}

// --------------------------------------------------------------
// Read status from scenario file
// --------------------------------------------------------------
void HST::clbkLoadStateEx (FILEHANDLE scn, void *vs)
{
	char *line;

	while (oapiReadScenario_nextline (scn, line)) {
		if (!_strnicmp (line, "ANT", 3)) {
			sscanf (line+3, "%d%lf", &ant_status, &ant_proc);
		} else if (!_strnicmp (line, "HATCH", 5)) {
			sscanf (line+5, "%d%lf", &hatch_status, &hatch_proc);
		} else if (!_strnicmp (line, "FOLD", 4)) {
			sscanf (line+5, "%d%lf", &array_status, &array_proc);
		} else {
			ParseScenarioLineEx (line, vs);
		}
	}

	SetAnimation (anim_ant, ant_proc);
	SetAnimation (anim_hatch, hatch_proc);
	SetAnimation (anim_array, array_proc);
}

// --------------------------------------------------------------
// Save status to scenario file
// --------------------------------------------------------------
void HST::clbkSaveState (FILEHANDLE scn)
{
	char cbuf[256];
	SaveDefaultState (scn);
	sprintf (cbuf, "%d %0.4f", ant_status, ant_proc);
	oapiWriteScenario_string (scn, "ANT", cbuf);
	sprintf (cbuf, "%d %0.4f", hatch_status, hatch_proc);
	oapiWriteScenario_string (scn, "HATCH", cbuf);
	sprintf (cbuf, "%d %0.4f", array_status, array_proc);
	oapiWriteScenario_string (scn, "FOLD", cbuf);
}

// --------------------------------------------------------------
// Frame update
// --------------------------------------------------------------
void HST::clbkPostStep (double simt, double simdt, double mjd)
{
	// Animate hi-gain antenna
	if (ant_status >= DOOR_CLOSING) {
		double da = simdt * ANTENNA_OPERATING_SPEED;
		if (ant_status == DOOR_CLOSING) {
			if (ant_proc > 0.0) ant_proc = max (0.0, ant_proc-da);
			else                ant_status = DOOR_CLOSED;
		} else {
			if (ant_proc < 1.0) ant_proc = min (1.0, ant_proc+da);
			else                ant_status = DOOR_OPEN;
		}
		SetAnimation (anim_ant, ant_proc);
	}

	// Animate main telescope hatch
	if (hatch_status >= DOOR_CLOSING) {
		double da = simdt * HATCH_OPERATING_SPEED;
		if (hatch_status == DOOR_CLOSING) {
			if (hatch_proc > 0.0) hatch_proc = max (0.0, hatch_proc-da);
			else                  hatch_status = DOOR_CLOSED;
		} else {
			if (hatch_proc < 1.0) hatch_proc = min (1.0, hatch_proc+da);
			else                  hatch_status = DOOR_OPEN;
		}
		SetAnimation (anim_hatch, hatch_proc);
	}

	// Animate solar arrays
	if (array_status >= DOOR_CLOSING) {
		double da = simdt * ARRAY_OPERATING_SPEED;
		if (array_status == DOOR_CLOSING) {
			if (array_proc > 0.0) array_proc = max (0.0, array_proc-da);
			else                  array_status = DOOR_CLOSED;
		} else {
			if (array_proc < 1.0) array_proc = min (1.0, array_proc+da);
			else                  array_status = DOOR_OPEN;
		}
		SetAnimation (anim_array, array_proc);
	}
}

// --------------------------------------------------------------
// Keyboard interface handler (buffered key events)
// --------------------------------------------------------------
int HST::clbkConsumeBufferedKey (DWORD key, bool down, char *kstate)
{
	if (!down) return 0; // only process keydown events

	if (KEYMOD_CONTROL (kstate)) {

		switch (key) {
		case OAPI_KEY_1: // deploy/retract antenna
			RevertAntenna();
			return 1;
		case OAPI_KEY_2: // open/close hatch
			RevertHatch();
			return 1;
		case OAPI_KEY_3: // open/fold solar arrays
			RevertArray();
			return 1;
		}
	}
	return 0;
}

// --------------------------------------------------------------
// Respond to generic messages
// --------------------------------------------------------------
int HST::clbkGeneric (int msgid, int prm, void *context)
{
	switch (msgid) {
	case VMSG_LUAINTERPRETER:
		return Lua_InitInterpreter (context);
	case VMSG_LUAINSTANCE:
		return Lua_InitInstance (context);
	}
	return 0;
}

// ==============================================================
// API callback interface
// ==============================================================

// --------------------------------------------------------------
// Vessel initialisation
// --------------------------------------------------------------
DLLCLBK VESSEL *ovcInit (OBJHANDLE hvessel, int flightmodel)
{
	return new HST (hvessel, flightmodel);
}

// --------------------------------------------------------------
// Vessel cleanup
// --------------------------------------------------------------
DLLCLBK void ovcExit (VESSEL *vessel)
{
	if (vessel) delete (HST*)vessel;
}
