// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//                ORBITER MODULE: DeltaGlider
//                  Part of the ORBITER SDK
//
// AvionicsSubsys.h
// Subsystem for avionics components:
// - angular rate indicator
// ==============================================================

#define STRICT 1

#include "AvionicsSubsys.h"
#include "Horizon.h"
#include "InstrHsi.h"
#include "InstrAoa.h"
#include "InstrVs.h"
#include "FuelMfd.h"
#include "MomentInd.h"
#include "AAPSubsys.h"

// ==============================================================
// Avionics subsystem
// ==============================================================

AvionicsSubsystem::AvionicsSubsystem (DeltaGlider *v)
: DGSubsystem (v)
{
	extern GDIParams g_Param;

	// create component instances
	ELID_INSTRATT = AddElement (instratt = new InstrAtt (v));
	ELID_INSTRHSI = AddElement (instrhsi = new InstrHSI (v));
	ELID_INSTRAOA = AddElement (instraoa = new InstrAOA (v));
	ELID_INSTRVS  = AddElement (instrvs  = new InstrVS (v));
	ELID_FUELMFD  = AddElement (fuelmfd  = new FuelMFD (v));
	ELID_ANGRATEIND = AddElement (angrateind = new AngRateIndicator (v, g_Param.surf));

	// create subsystem instances
	AddSubsystem (aapssys = new AAPSubsystem (this));
	aapssys->AttachHSI(instrhsi);
}

// --------------------------------------------------------------

bool AvionicsSubsystem::clbkLoadPanel2D (int panelid, PANELHANDLE hPanel, DWORD viewW, DWORD viewH)
{
	DGSubsystem::clbkLoadPanel2D (panelid, hPanel, viewW, viewH);

	if (panelid != 0) return false;

	SURFHANDLE instr2dtex = oapiGetTextureHandle(DG()->panelmesh0,2);
	SURFHANDLE panel2dtex = oapiGetTextureHandle(DG()->panelmesh0,1);

	// Artifical horizon display
	DG()->RegisterPanelArea (hPanel, ELID_INSTRATT, _R(0,0,0,0), PANEL_REDRAW_ALWAYS, PANEL_MOUSE_IGNORE, 0, instratt);

	// HSI indicator
	DG()->RegisterPanelArea (hPanel, ELID_INSTRHSI, _R(0,0,0,0), PANEL_REDRAW_ALWAYS, PANEL_MOUSE_IGNORE, 0, instrhsi);

	// AOA/VS tape
	DG()->RegisterPanelArea (hPanel, ELID_INSTRAOA, _R(0,0,0,0), PANEL_REDRAW_ALWAYS, PANEL_MOUSE_IGNORE, panel2dtex, instraoa);
	DG()->RegisterPanelArea (hPanel, ELID_INSTRVS,  _R(0,0,0,0), PANEL_REDRAW_ALWAYS, PANEL_MOUSE_IGNORE, panel2dtex, instrvs);

	// Propellant status display
	DG()->RegisterPanelArea (hPanel, ELID_FUELMFD,  _R(0,0,0,0), PANEL_REDRAW_ALWAYS, PANEL_MOUSE_IGNORE, instr2dtex, fuelmfd);

	// angular rate indicators
	DG()->RegisterPanelArea (hPanel, ELID_ANGRATEIND, _R(0,0,0,0), PANEL_REDRAW_ALWAYS, PANEL_MOUSE_IGNORE, panel2dtex, angrateind);

	return true;
}

// --------------------------------------------------------------

bool AvionicsSubsystem::clbkLoadVC (int vcid)
{
	DGSubsystem::clbkLoadVC (vcid);

	if (vcid != 0) return false;

	// Artifical horizon display
	oapiVCRegisterArea (ELID_INSTRATT, PANEL_REDRAW_ALWAYS, PANEL_MOUSE_IGNORE);

	// HSI indicator
	oapiVCRegisterArea (ELID_INSTRHSI, PANEL_REDRAW_ALWAYS, PANEL_MOUSE_IGNORE);

	// AOA/VS tapes
	oapiVCRegisterArea (ELID_INSTRAOA, PANEL_REDRAW_ALWAYS, PANEL_MOUSE_IGNORE);
	oapiVCRegisterArea (ELID_INSTRVS, PANEL_REDRAW_ALWAYS, PANEL_MOUSE_IGNORE);

	// Propellant status display
	oapiVCRegisterArea (ELID_FUELMFD, _R(0,0,1,1), PANEL_REDRAW_ALWAYS, PANEL_MOUSE_IGNORE, PANEL_MAP_DIRECT, oapiGetTextureHandle (DG()->vcmesh_tpl, 19));

	// angular rate indicators
	oapiVCRegisterArea (ELID_ANGRATEIND, _R(0,0,1,1), PANEL_REDRAW_ALWAYS, PANEL_MOUSE_IGNORE, PANEL_MAP_DIRECT, oapiGetTextureHandle (DG()->vcmesh_tpl, 14));

	return true;
}

