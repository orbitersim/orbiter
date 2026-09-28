// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//                ORBITER MODULE: Quadcopter
//                  Part of the ORBITER SDK
//
// Quadcopter.h
// Class interface for quadcopter vessel class module
// ==============================================================

#ifndef __QUADCOPTER_H
#define __QUADCOPTER_H

#ifndef __linux__
#include "orbitersdk.h"
#else // __linux__
#include "Orbitersdk.h"
#endif // __linux__
#include "QuadcopterLua.h"
#ifndef __linux__
#include "..\Common\Instrument.h"
#else // __linux__
#include "../Common/Instrument.h"
#endif // __linux__

class PropulsionSubsystem;

class Quadcopter : public ComponentVessel
{
public:
	Quadcopter(OBJHANDLE hObj, int fmodel);

	PropulsionSubsystem *ssysPropulsion() { return ssys_propulsion; }

	void clbkSetClassCaps(FILEHANDLE cfg);
	int clbkGeneric(int msgid, int prm, void *context);

private:
	// Vessel subsystems
	PropulsionSubsystem *ssys_propulsion;
	LuaInterface lua;

	MESHHANDLE exmesh_tpl;
	PROPELLANT_HANDLE ph;
	THRUSTER_HANDLE th;
	THGROUP_HANDLE tgh;
};

#endif // !__QUADCOPTER_H
