// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//                 ORBITER MODULE: Atlantis
//                  Part of the ORBITER SDK
//
// HST.h
// HST basic specs and animations
//
// HST mesh and textures by David Sundstrom
// ==============================================================

#ifndef __HST_H
#define __HST_H

#define STRICT
#include "orbitersdk.h"

// ==============================================================
// Some parameters and capabilities
// ==============================================================

const double HST_SIZE = 7.52;
const double HST_EMPTY_MASS = 10886;
const double ANTENNA_OPERATING_SPEED = 0.025;
const double HATCH_OPERATING_SPEED = 0.022;
const double ARRAY_OPERATING_SPEED = 0.02;

// ==============================================================
// HST class interface
// ==============================================================

class HST: public VESSEL3 {
public:
	enum DoorStatus { DOOR_CLOSED, DOOR_OPEN, DOOR_CLOSING, DOOR_OPENING } ant_status, hatch_status, array_status;
	HST (OBJHANDLE hObj, int fmodel);
	void DefineAnimations (void);
	void ActivateAntenna (DoorStatus action);
	void RevertAntenna (void);
	void ActivateHatch (DoorStatus action);
	void RevertHatch (void);
	void ActivateArray (DoorStatus action);
	void RevertArray (void);

	// Overloaded callback functions
	void clbkSetClassCaps (FILEHANDLE cfg);
	void clbkLoadStateEx (FILEHANDLE scn, void *vs);
	void clbkSaveState (FILEHANDLE scn);
	void clbkPostStep (double simt, double simdt, double mjd);
	int  clbkConsumeBufferedKey (DWORD key, bool down, char *kstate);
	int  clbkGeneric (int msgid, int prm, void *context);

private:
	UINT anim_ant, anim_hatch, anim_array;
	double ant_proc, hatch_proc, array_proc;

	// script interface-related methods, implemented in HST_Lua.cpp
	int Lua_InitInterpreter (void *context);
	int Lua_InitInstance (void *context);
};

#endif // !__HST_H