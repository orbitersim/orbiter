// ==============================================================
//                ORBITER MODULE: Quadcopter
//                  Part of the ORBITER SDK
//          Copyright (C) 2001-2019 Martin Schweiger
//                   All rights reserved
//
// Quadcopter.cpp
// Implementation of Quadcopter vessel class module
// ==============================================================

#include "Quadcopter.h"
#include "PropulsionSubsys.h"

Quadcopter::Quadcopter(OBJHANDLE hObj, int fmodel)
	: ComponentVessel(hObj, fmodel)
{
	// Subsystem definitions
	AddSubsystem(ssys_propulsion = new PropulsionSubsystem(this));
}

void Quadcopter::clbkSetClassCaps(FILEHANDLE cfg)
{
	const TOUCHDOWNVTX tdv[12] = {
		{ _V(-0.3, -0.26,  0.3), 1e3, 5e1, 1.6, 1.6 },
		{ _V(-0.3, -0.26, -0.3), 1e3, 5e1, 1.6, 1.6 },
		{ _V( 0.3, -0.26, -0.3), 1e3, 5e1, 1.6, 1.6 },
		{ _V( 0.3, -0.26,  0.3), 1e3, 5e1, 1.6 },
		{ _V( 0.6, 0.15,  0.4), 1e3, 5e1, 1.6 },
		{ _V( 0.6, 0.15, -0.4), 1e3, 5e1, 1.6 }, 
		{ _V(-0.6, 0.15,  0.4), 1e3, 5e1, 1.6 },
		{ _V(-0.6, 0.15, -0.4), 1e3, 5e1, 1.6 },
		{ _V( 0.4, 0.15,  0.6), 1e3, 5e1, 1.6 },
		{ _V(-0.4, 0.15,  0.6), 1e3, 5e1, 1.6 },
		{ _V( 0.4, 0.15, -0.6), 1e3, 5e1, 1.6 },
		{ _V(-0.4, 0.15, -0.6), 1e3, 5e1, 1.6 }
	};

	SetEmptyMass(2.0);
	SetSize(0.3);
	SetPMI(_V(0.02, 0.05, 0.02));
	SetCrossSections(_V(0.1, 0.6, 0.1));
	SetCW(1.0, 1.0, 1.0, 1.0);
	SetTouchdownPoints(tdv, 12);
	exmesh_tpl = oapiLoadMeshGlobal("Quadcopter\\quadcopter");
	SetMeshVisibilityMode(AddMesh(exmesh_tpl), MESHVIS_EXTERNAL);

	// Dummy thruster
	ph = CreatePropellantResource(0.01, 0.01);
	SetDefaultPropellantResource(ph);
	th = CreateThruster(_V(0, 0, 0), _V(0, 1, 0), 1e-10, ph, 1e-5, 1e-5);
	tgh = CreateThrusterGroup(&th, 1, THGROUP_MAIN);
}

int Quadcopter::clbkGeneric(int msgid, int prm, void *context)
{
	switch (msgid) {
	case VMSG_LUAINTERPRETER:
		return lua.InitInterpreter(context);
	case VMSG_LUAINSTANCE:
		return lua.InitInstance(context);
	}
	return 0;
}

DLLCLBK VESSEL *ovcInit(OBJHANDLE hvessel, int flightmodel)
{
	return new Quadcopter(hvessel, flightmodel);
}

DLLCLBK void ovcExit(VESSEL *vessel)
{
	if (vessel) delete (Quadcopter*)vessel;
}