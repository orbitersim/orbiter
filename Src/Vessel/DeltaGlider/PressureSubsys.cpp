// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//                ORBITER MODULE: DeltaGlider
//                  Part of the ORBITER SDK
//
// PressureSubsys.cpp
// Cabin and airlock pressure control subsystem
// ==============================================================

#include "PressureSubsys.h"
#include "DockingSubsys.h"
#include "DeltaGlider.h"
#include "meshres.h"
#include "meshres_p1.h"
#include "meshres_vc.h"
#include "dg_vc_anim.h"

using std::min;
using std::max;

// ==============================================================

double PressureSubsystem::v_cabin = 24.0;
double PressureSubsystem::v_airlock = 4.0;
double PressureSubsystem::p_target = 100e3;

// --------------------------------------------------------------

PressureSubsystem::PressureSubsystem (DeltaGlider *vessel)
: DGSubsystem(vessel)
{
	extern GDIParams g_Param;

	p_cabin = p_airlock = 100e3;
	p_ext_hatch = p_ext_lock = 0.0;
	docked = false;

	AddSubsystem (airlockctrl = new AirlockCtrl (this));
	AddSubsystem (hatchctrl = new TophatchCtrl (this));

	for (int i = 0; i < 5; i++) {
		ELID_PVALVESWITCH[i] = AddElement (valve_switch[i] = new PValveSwitch (this, i));
		valve_status[i] = 0;
	}
	ELID_DISPLAY = AddElement (pind = new PressureIndicator (this, g_Param.surf));
}

// --------------------------------------------------------------

PressureSubsystem::~PressureSubsystem ()
{
}

// --------------------------------------------------------------

const AnimState2 &PressureSubsystem::OLockState () const
{
	return airlockctrl->OLockState();
}

// --------------------------------------------------------------

const AnimState2 &PressureSubsystem::ILockState () const
{
	return airlockctrl->ILockState();
}

// --------------------------------------------------------------

void PressureSubsystem::OpenOuterAirlock ()
{
	airlockctrl->OpenOuterLock ();
}

// --------------------------------------------------------------

void PressureSubsystem::CloseOuterAirlock ()
{
	airlockctrl->CloseOuterLock ();
}

// --------------------------------------------------------------

void PressureSubsystem::OpenInnerAirlock ()
{
	airlockctrl->OpenInnerLock ();
}

// --------------------------------------------------------------

void PressureSubsystem::CloseInnerAirlock ()
{
	airlockctrl->CloseInnerLock ();
}

// --------------------------------------------------------------

void PressureSubsystem::OpenHatch ()
{
	hatchctrl->OpenHatch();
}

// --------------------------------------------------------------

void PressureSubsystem::CloseHatch ()
{
	hatchctrl->CloseHatch();
}

// --------------------------------------------------------------

const AnimState2 &PressureSubsystem::HatchState () const
{
	return hatchctrl->State();
}

// --------------------------------------------------------------

void PressureSubsystem::RepairDamage ()
{
	hatchctrl->RepairDamage ();
}

// --------------------------------------------------------------

void PressureSubsystem::clbkPostStep (double simt, double simdt, double mjd)
{
	DGSubsystem::clbkPostStep (simt, simdt, mjd);

	docked = DG()->DockingStatus(0) != 0;
	double p_static = DG()->GetAtmPressure();
	p_ext_hatch = p_static;
	if (!docked) {
		p_ext_lock = p_static;
		if (!DG()->SubsysDocking()->NconeState().IsClosed())
			p_ext_lock += DG()->GetDynPressure() * DG()->SubsysDocking()->NconeState().State();
	}
	else v_extdock = 2.0; // for now

	double cs, pdiff, dvol, dpc, dpa, pc, pa;

	// exchange cabin - ext.hatch
	cs = (valve_status[1] ? 2e-4:0.0);
	cs += 0.1*HatchState().State();
	if (cs) {
		pdiff = p_ext_hatch-p_cabin;
		dvol = pdiff*cs*simdt*1e3;
		dpc = dvol/v_cabin;
		pc = p_cabin + dpc;
		if (p_cabin > p_ext_hatch) pc = max(pc,p_ext_hatch);
		else                       pc = min(pc,p_ext_hatch);
		p_cabin = pc;
	}

	// exchange airlock - ext.lock
	cs = (valve_status[3] ? 2e-4:0.0);
	cs += 1.0*OLockState().State();
	if (cs) {
		pdiff = p_ext_lock-p_airlock;
		dvol = pdiff*cs*simdt*1e3;
		dpa = dvol/v_airlock;
		pa = p_airlock + dpa;
		if (docked) {
			dpc = -dvol/v_extdock;
			pc = p_ext_lock + dpc;
			if ((p_airlock-p_ext_lock)*(pa-pc) < 0.0)
				pa = pc = (pa*v_airlock + pc*v_extdock)/(v_airlock+v_extdock);
			p_ext_lock = pc;
		} else {
			if (p_airlock > p_ext_lock) pa = max(pa,p_ext_lock);
			else                        pa = min(pa,p_ext_lock);
		}
		p_airlock = pa;
	}

	// exchange cabin - airlock
	cs = (valve_status[2] ? 2e-4:0.0);
	cs += 1.0*ILockState().State();
	if (cs) {
		pdiff = p_cabin-p_airlock;
		dvol = pdiff*cs*simdt*1e3;
		dpc = -dvol/v_cabin;
		dpa = dvol/v_airlock;
		pc = p_cabin + dpc;
		pa = p_airlock + dpa;
		if ((p_airlock-p_cabin)*(pa-pc) < 0.0)
			pa = pc = (pa*v_airlock + pc*v_cabin)/(v_airlock+v_cabin);
		p_cabin = pc;
		p_airlock = pa;
	}

	// supply cabin
	cs = (valve_status[0] ? 5e-5:0.0);
	if (cs) {
		pdiff = 400e3-p_cabin;
		dvol = pdiff*cs*simdt*1e3;
		dpc = dvol/v_cabin;
		pc = p_cabin + dpc;
		pc = min (pc, p_target);
		p_cabin = pc;
	}

	// supply airlock
	cs = (valve_status[4] ? 5e-5:0.0);
	if (cs) {
		pdiff = 400e3-p_airlock;
		dvol = pdiff*cs*simdt*1e3;
		dpa = dvol/v_airlock;
		pa = p_airlock + dpa;
		pa = min (pa, p_target);
		p_airlock = pa;
	}
}

// --------------------------------------------------------------

bool PressureSubsystem::clbkLoadPanel2D (int panelid, PANELHANDLE hPanel, DWORD viewW, DWORD viewH)
{
	bool res = DGSubsystem::clbkLoadPanel2D (panelid, hPanel, viewW, viewH);

	if (panelid != 1) return res;

	// Pressure indicator display
	SURFHANDLE surf = oapiGetTextureHandle(DG()->panelmesh1,2);
	DG()->RegisterPanelArea (hPanel, ELID_DISPLAY, _R(0,0,0,0), PANEL_REDRAW_ALWAYS, PANEL_MOUSE_IGNORE, surf, pind);

	// Pressure valve switches
	surf = oapiGetTextureHandle(DG()->panelmesh1,1);
	for (int i = 0; i < 5; i++) {
		DG()->RegisterPanelArea (hPanel, ELID_PVALVESWITCH[i], _R(388+i*46,42,414+i*46,94), PANEL_REDRAW_MOUSE, PANEL_MOUSE_LBDOWN|PANEL_MOUSE_LBUP, surf, valve_switch[i]);
		valve_switch[i]->DefineAnimation2D (DG()->panelmesh1, GRP_INSTRUMENTS_ABOVE_P1, 24+i*4);
	}

	return true;
}

// --------------------------------------------------------------

bool PressureSubsystem::clbkLoadVC (int vcid)
{
	DGSubsystem::clbkLoadVC (vcid);

	switch (vcid) {
	case 0:
		// Pressure indicator display
		oapiVCRegisterArea (ELID_DISPLAY, PANEL_REDRAW_ALWAYS, PANEL_MOUSE_IGNORE);

		// Pressure valve switches
		oapiVCRegisterArea (ELID_PVALVESWITCH[0], PANEL_REDRAW_MOUSE, PANEL_MOUSE_LBDOWN);
		oapiVCSetAreaClickmode_Quadrilateral (ELID_PVALVESWITCH[0], VC_CABIN_O2_SWITCH_mousearea[0], VC_CABIN_O2_SWITCH_mousearea[1], VC_CABIN_O2_SWITCH_mousearea[2], VC_CABIN_O2_SWITCH_mousearea[3]);
		valve_switch[0]->DefineAnimationVC (VC_CABIN_O2_SWITCH_ref, VC_CABIN_O2_SWITCH_axis, GRP_SWITCH1_VC, VC_CABIN_O2_SWITCH_vofs);

		oapiVCRegisterArea (ELID_PVALVESWITCH[1], PANEL_REDRAW_MOUSE, PANEL_MOUSE_LBDOWN);
		oapiVCSetAreaClickmode_Quadrilateral (ELID_PVALVESWITCH[1], VC_VALVE1_SWITCH_mousearea[0], VC_VALVE1_SWITCH_mousearea[1], VC_VALVE1_SWITCH_mousearea[2], VC_VALVE1_SWITCH_mousearea[3]);
		valve_switch[1]->DefineAnimationVC (VC_VALVE1_SWITCH_ref, VC_VALVE1_SWITCH_axis, GRP_SWITCH1_VC, VC_VALVE1_SWITCH_vofs);

		oapiVCRegisterArea (ELID_PVALVESWITCH[2], PANEL_REDRAW_MOUSE, PANEL_MOUSE_LBDOWN);
		oapiVCSetAreaClickmode_Quadrilateral (ELID_PVALVESWITCH[2], VC_VALVE2_SWITCH_mousearea[0], VC_VALVE2_SWITCH_mousearea[1], VC_VALVE2_SWITCH_mousearea[2], VC_VALVE2_SWITCH_mousearea[3]);
		valve_switch[2]->DefineAnimationVC (VC_VALVE2_SWITCH_ref, VC_VALVE2_SWITCH_axis, GRP_SWITCH1_VC, VC_VALVE2_SWITCH_vofs);

		oapiVCRegisterArea (ELID_PVALVESWITCH[3], PANEL_REDRAW_MOUSE, PANEL_MOUSE_LBDOWN);
		oapiVCSetAreaClickmode_Quadrilateral (ELID_PVALVESWITCH[3], VC_VALVE3_SWITCH_mousearea[0], VC_VALVE3_SWITCH_mousearea[1], VC_VALVE3_SWITCH_mousearea[2], VC_VALVE3_SWITCH_mousearea[3]);
		valve_switch[3]->DefineAnimationVC (VC_VALVE3_SWITCH_ref, VC_VALVE3_SWITCH_axis, GRP_SWITCH1_VC, VC_VALVE3_SWITCH_vofs);

		oapiVCRegisterArea (ELID_PVALVESWITCH[4], PANEL_REDRAW_MOUSE, PANEL_MOUSE_LBDOWN);
		oapiVCSetAreaClickmode_Quadrilateral (ELID_PVALVESWITCH[4], VC_LOCK_O2_SWITCH_mousearea[0], VC_LOCK_O2_SWITCH_mousearea[1], VC_LOCK_O2_SWITCH_mousearea[2], VC_LOCK_O2_SWITCH_mousearea[3]);
		valve_switch[4]->DefineAnimationVC (VC_LOCK_O2_SWITCH_ref, VC_LOCK_O2_SWITCH_axis, GRP_SWITCH1_VC, VC_LOCK_O2_SWITCH_vofs);
		break;
	}
	return true;
}

// ==============================================================
// Airlock controls
// ==============================================================

AirlockCtrl::AirlockCtrl (PressureSubsystem *_subsys)
: DGSubsystem(_subsys)
{
	ostate.SetOperatingSpeed (AIRLOCK_OPERATING_SPEED);
	istate.SetOperatingSpeed (AIRLOCK_OPERATING_SPEED);

	ELID_OSWITCH = AddElement (osw = new OuterLockSwitch (this));
	ELID_ISWITCH = AddElement (isw = new InnerLockSwitch (this));

	// Outer airlock animation
	static UINT OLockGrp[2] = {GRP_OLock1,GRP_OLock2};
	static MGROUP_ROTATE OLock (0, OLockGrp, 2,
		_V(0,-0.080,9.851), _V(1,0,0), (float)(110*RAD));
	static UINT VCOLockGrp[1] = {13};
	static MGROUP_ROTATE VCOLock (1, VCOLockGrp, 1,
		_V(0,-0.080,9.851), _V(1,0,0), (float)(110*RAD));
	anim_olock = DG()->CreateAnimation (0);
	DG()->AddAnimationComponent (anim_olock, 0, 1, &OLock);
	DG()->AddAnimationComponent (anim_olock, 0, 1, &VCOLock);

	// Inner airlock animation
	static UINT ILockGrp[2] = {GRP_ILock1,GRP_ILock2};
	static MGROUP_ROTATE ILock (0, ILockGrp, 2,
		_V(0,-0.573,7.800), _V(1,0,0), (float)(85*RAD));
	// virtual cockpit mesh animation (inner airlock visible from cockpit)
	static UINT VCILockGrp[4] = {GRP_ILOCK1_VC,GRP_ILOCK2_VC,GRP_ILOCK3_VC,GRP_ILOCK_GLASS_VC};
	static MGROUP_ROTATE VCILock (1, VCILockGrp, 4,
		_V(0,-0.573,7.800), _V(1,0,0), (float)(85*RAD));
	anim_ilock = DG()->CreateAnimation (0);
	DG()->AddAnimationComponent (anim_ilock, 0, 1, &ILock);
	DG()->AddAnimationComponent (anim_ilock, 0, 1, &VCILock);
}

// --------------------------------------------------------------

void AirlockCtrl::OpenOuterLock ()
{
	ostate.Open();
	osw->SetState(DGSwitch1::UP);
	DG()->TriggerRedrawArea(1, 0, ELID_OSWITCH);
	DG()->UpdateStatusIndicators();
	DG()->RecordEvent ("OLOCK", "OPEN");
}

// --------------------------------------------------------------

void AirlockCtrl::CloseOuterLock ()
{
	ostate.Close();
	osw->SetState(DGSwitch1::DOWN);
	DG()->TriggerRedrawArea(1, 0, ELID_OSWITCH);
	DG()->UpdateStatusIndicators();
	DG()->RecordEvent ("OLOCK", "CLOSE");
}

// --------------------------------------------------------------

void AirlockCtrl::RevertOuterLock ()
{
	if (ostate.IsOpen() || ostate.IsOpening())
		CloseOuterLock();
	else
		OpenOuterLock();
}

// --------------------------------------------------------------

void AirlockCtrl::OpenInnerLock ()
{
	istate.Open();
	isw->SetState(DGSwitch1::UP);
	DG()->TriggerRedrawArea(1, 0, ELID_ISWITCH);
	DG()->UpdateStatusIndicators();
	DG()->RecordEvent ("ILOCK", "OPEN");
}

// --------------------------------------------------------------

void AirlockCtrl::CloseInnerLock ()
{
	istate.Close();
	isw->SetState(DGSwitch1::DOWN);
	DG()->TriggerRedrawArea(1, 0, ELID_ISWITCH);
	DG()->UpdateStatusIndicators();
	DG()->RecordEvent ("ILOCK", "CLOSE");
}

// --------------------------------------------------------------

void AirlockCtrl::RevertInnerLock ()
{
	if (istate.IsOpen() || istate.IsOpening())
		CloseInnerLock();
	else
		OpenInnerLock();
}

// --------------------------------------------------------------

void AirlockCtrl::clbkSaveState (FILEHANDLE scn)
{
	ostate.SaveState (scn, "AIRLOCK");
	istate.SaveState (scn, "IAIRLOCK");
}

// --------------------------------------------------------------

bool AirlockCtrl::clbkParseScenarioLine (const char *line)
{
	return (ostate.ParseScenarioLine (line, "AIRLOCK") ||
		    istate.ParseScenarioLine (line, "IAIRLOCK"));
}

// --------------------------------------------------------------

void AirlockCtrl::clbkPostCreation ()
{
	DG()->SetAnimation (anim_olock, ostate.State());
	DG()->SetAnimation (anim_ilock, istate.State());	
}

// --------------------------------------------------------------

void AirlockCtrl::clbkPostStep (double simt, double simdt, double mjd)
{
	// animate outer airlock
	if (ostate.Process (simdt)) {
		DG()->SetAnimation (anim_olock, ostate.State());
		DG()->UpdateStatusIndicators();
	}

	// animate inner airlock
	if (istate.Process (simdt)) {
		DG()->SetAnimation (anim_ilock, istate.State());
		DG()->UpdateStatusIndicators();
	}
}

// --------------------------------------------------------------

bool AirlockCtrl::clbkLoadPanel2D (int panelid, PANELHANDLE hPanel, DWORD viewW, DWORD viewH)
{
	if (panelid != 1) return false;

	SURFHANDLE panel2dtex = oapiGetTextureHandle(DG()->panelmesh1,1);

	// Inner airlock open/close switch
	DG()->RegisterPanelArea (hPanel, ELID_ISWITCH, _R(480,192,506,244), PANEL_REDRAW_MOUSE, PANEL_MOUSE_LBDOWN|PANEL_MOUSE_LBUP, panel2dtex, isw);
	isw->DefineAnimation2D (DG()->panelmesh1, GRP_INSTRUMENTS_ABOVE_P1, 4);

	// Outer airlock open/close switch
	DG()->RegisterPanelArea (hPanel, ELID_OSWITCH, _R(526,192,552,244), PANEL_REDRAW_MOUSE, PANEL_MOUSE_LBDOWN|PANEL_MOUSE_LBUP, panel2dtex, osw);
	osw->DefineAnimation2D (DG()->panelmesh1, GRP_INSTRUMENTS_ABOVE_P1, 8);

	return true;
}

// --------------------------------------------------------------

bool AirlockCtrl::clbkLoadVC (int vcid)
{
	if (vcid != 0) return false;

	// Outer airlock open/close switch
	oapiVCRegisterArea (ELID_OSWITCH, PANEL_REDRAW_MOUSE, PANEL_MOUSE_LBDOWN);
	oapiVCSetAreaClickmode_Quadrilateral (ELID_OSWITCH, VC_OLOCK_SWITCH_mousearea[0], VC_OLOCK_SWITCH_mousearea[1], VC_OLOCK_SWITCH_mousearea[2], VC_OLOCK_SWITCH_mousearea[3]);
	osw->DefineAnimationVC (VC_OLOCK_SWITCH_ref, VC_OLOCK_SWITCH_axis, GRP_SWITCH1_VC, VC_OLOCK_SWITCH_vofs);

	// Inner airlock open/close switch
	oapiVCRegisterArea (ELID_ISWITCH, PANEL_REDRAW_MOUSE, PANEL_MOUSE_LBDOWN);
	oapiVCSetAreaClickmode_Quadrilateral (ELID_ISWITCH, VC_ILOCK_SWITCH_mousearea[0], VC_ILOCK_SWITCH_mousearea[1], VC_ILOCK_SWITCH_mousearea[2], VC_ILOCK_SWITCH_mousearea[3]);
	isw->DefineAnimationVC (VC_ILOCK_SWITCH_ref, VC_ILOCK_SWITCH_axis, GRP_SWITCH1_VC, VC_ILOCK_SWITCH_vofs);

	return true;
}

// --------------------------------------------------------------

bool AirlockCtrl::clbkPlaybackEvent (double simt, double event_t, const char *event_type, const char *event)
{
	if (!_stricmp (event_type, "OLOCK")) {
		if (!_stricmp (event, "CLOSE")) CloseOuterLock();
		else                            OpenOuterLock();
		return true;
	} else if (!_stricmp (event_type, "ILOCK")) {
		if (!_stricmp (event, "CLOSE")) CloseInnerLock();
		else                            OpenInnerLock();
		return true;
	}
	return false;
}

// --------------------------------------------------------------

int AirlockCtrl::clbkConsumeBufferedKey (DWORD key, bool down, char *kstate)
{
	if (KEYMOD_ALT(kstate) || KEYMOD_SHIFT(kstate))
		return 0;

	if (key == OAPI_KEY_O) {
		if (KEYMOD_CONTROL(kstate))
			RevertInnerLock();
		else
			RevertOuterLock();
		return 1;
	}
	return 0;
}

// ==============================================================

OuterLockSwitch::OuterLockSwitch (AirlockCtrl *comp)
: DGSwitch1(comp->DG(), DGSwitch1::TWOSTATE), component(comp)
{
}

// --------------------------------------------------------------

void OuterLockSwitch::Reset2D (int panelid, MESHHANDLE hMesh)
{
	SetState (component->ostate.IsClosed() || component->ostate.IsClosing() ?
		DOWN : UP);
}

// --------------------------------------------------------------

void OuterLockSwitch::ResetVC (DEVMESHHANDLE hMesh)
{
	DGSwitch1::ResetVC (hMesh);
	SetState (component->ostate.IsClosed() || component->ostate.IsClosing() ?
		DOWN : UP);
}

// --------------------------------------------------------------

bool OuterLockSwitch::ProcessMouse2D (int event, int mx, int my)
{
	if (DGSwitch1::ProcessMouse2D (event, mx, my)) {
		if (GetState() == UP) component->OpenOuterLock();
		else                  component->CloseOuterLock();
		return true;
	}
	return false;
}

// --------------------------------------------------------------

bool OuterLockSwitch::ProcessMouseVC (int event, VECTOR3 &p)
{
	if (DGSwitch1::ProcessMouseVC (event, p)) {
		if (GetState() == UP) component->OpenOuterLock();
		else                  component->CloseOuterLock();
		return true;
	}
	return false;
}

// ==============================================================

InnerLockSwitch::InnerLockSwitch (AirlockCtrl *comp)
: DGSwitch1(comp->DG(), DGSwitch1::TWOSTATE), component(comp)
{
}

// --------------------------------------------------------------

void InnerLockSwitch::Reset2D (int panelid, MESHHANDLE hMesh)
{
	SetState (component->istate.IsClosed() || component->istate.IsClosing() ?
		DOWN : UP);
}

// --------------------------------------------------------------

void InnerLockSwitch::ResetVC (DEVMESHHANDLE hMesh)
{
	DGSwitch1::ResetVC (hMesh);
	SetState (component->istate.IsClosed() || component->istate.IsClosing() ?
		DOWN : UP);
}

// --------------------------------------------------------------

bool InnerLockSwitch::ProcessMouse2D (int event, int mx, int my)
{
	if (DGSwitch1::ProcessMouse2D (event, mx, my)) {
		if (GetState() == UP) component->OpenInnerLock();
		else                  component->CloseInnerLock();
		return true;
	}
	return false;
}

// --------------------------------------------------------------

bool InnerLockSwitch::ProcessMouseVC (int event, VECTOR3 &p)
{
	if (DGSwitch1::ProcessMouseVC (event, p)) {
		if (GetState() == UP) component->OpenInnerLock();
		else                  component->CloseInnerLock();
		return true;
	}
	return false;
}

// ==============================================================
// Top hatch controls
// ==============================================================

TophatchCtrl::TophatchCtrl (PressureSubsystem *_subsys)
: DGSubsystem(_subsys)
{
	hatch_state.SetOperatingSpeed (HATCH_OPERATING_SPEED);
	hatch_vent   = NULL;
	hatchfail    = 0;

	ELID_SWITCH = AddElement (sw = new HatchCtrlSwitch (this));

	// Top hatch animation
	static UINT HatchGrp[2] = {GRP_Hatch1,GRP_Hatch2};
	static MGROUP_ROTATE Hatch (0, HatchGrp, 2,
		_V(0,2.069,5.038), _V(1,0,0), (float)(110*RAD));
	static UINT VCHatchGrp[1] = {GRP_HATCH_VC};
	static MGROUP_ROTATE VCHatch (1, VCHatchGrp, 1,
		_V(0,2.069,5.038), _V(1,0,0), (float)(110*RAD));
	static UINT RearLadderGrp[2] = {GRP_RearLadder1,GRP_RearLadder2};
	static MGROUP_ROTATE RearLadder1 (0, RearLadderGrp, 2,
		_V(0,1.7621,4.0959), _V(1,0,0), (float)(-20*RAD));
	static MGROUP_ROTATE RearLadder2 (0, RearLadderGrp+1, 1,
		_V(0,1.1173,4.1894), _V(1,0,0), (float)(180*RAD));

	// virtual cockpit ladder animation
	static UINT VCRearLadderGrp[2] = {GRP_LADDER1_VC,GRP_LADDER2_VC};
	static MGROUP_ROTATE VCRearLadder1 (1, VCRearLadderGrp, 2,
		_V(0,1.7621,4.0959), _V(1,0,0), (float)(-20*RAD));
	static MGROUP_ROTATE VCRearLadder2 (1, VCRearLadderGrp+1, 1,
		_V(0,1.1173,4.1894), _V(1,0,0), (float)(180*RAD));
	anim_hatch = DG()->CreateAnimation (0);
	DG()->AddAnimationComponent (anim_hatch, 0, 1, &Hatch);
	DG()->AddAnimationComponent (anim_hatch, 0, 1, &VCHatch);
	DG()->AddAnimationComponent (anim_hatch, 0, 0.25, &RearLadder1);
	DG()->AddAnimationComponent (anim_hatch, 0.25, 0.8, &RearLadder2);
	DG()->AddAnimationComponent (anim_hatch, 0, 0.25, &VCRearLadder1);
	DG()->AddAnimationComponent (anim_hatch, 0.25, 0.8, &VCRearLadder2);
}

// --------------------------------------------------------------

TophatchCtrl::~TophatchCtrl ()
{
	if (hatch_vent)
		DG()->DelExhaustStream (hatch_vent);
}

// --------------------------------------------------------------

void TophatchCtrl::OpenHatch ()
{
	if (hatch_state.IsClosed() && !hatch_vent && DG()->GetAtmPressure() < 10e3) {
		static PARTICLESTREAMSPEC airvent = {
			0, 1.0, 15, 0.5, 0.3, 2, 0.3, 1.0, PARTICLESTREAMSPEC::EMISSIVE,
			PARTICLESTREAMSPEC::LVL_LIN, 0.1, 0.1,
			PARTICLESTREAMSPEC::ATM_FLAT, 0.1, 0.1
		};
		static VECTOR3 pos = {0,2,4};
		static VECTOR3 dir = {0,1,0};
		static double lvl = 0.1;
		hatch_vent = DG()->AddParticleStream (&airvent, pos, dir, &lvl);
		hatch_vent_t = oapiGetSimTime();
	}
	hatch_state.Open();
	sw->SetState(DGSwitch1::UP);
	DG()->TriggerRedrawArea(1, 0, ELID_SWITCH);
	DG()->UpdateStatusIndicators();
	DG()->RecordEvent ("HATCH", "OPEN");
}

// --------------------------------------------------------------

void TophatchCtrl::CloseHatch ()
{
	hatch_state.Close();
	sw->SetState(DGSwitch1::DOWN);
	DG()->TriggerRedrawArea(1, 0, ELID_SWITCH);
	DG()->UpdateStatusIndicators();
	DG()->RecordEvent ("HATCH", "CLOSE");
}

// --------------------------------------------------------------

void TophatchCtrl::Revert ()
{
	if (hatch_state.IsOpen() || hatch_state.IsOpening())
		CloseHatch();
	else
		OpenHatch();
}

// --------------------------------------------------------------

void TophatchCtrl::clbkSaveState (FILEHANDLE scn)
{
	hatch_state.SaveState (scn, "HATCH");
}

// --------------------------------------------------------------

bool TophatchCtrl::clbkParseScenarioLine (const char *line)
{
	return hatch_state.ParseScenarioLine (line, "HATCH");
}

// --------------------------------------------------------------

void TophatchCtrl::clbkPostCreation ()
{
	DG()->SetAnimation (anim_hatch, hatch_state.State());	
}

// --------------------------------------------------------------

void TophatchCtrl::clbkPostStep (double simt, double simdt, double mjd)
{
	// animate top hatch
	if (!hatchfail && hatch_state.Process (simdt)) {
		DG()->SetAnimation (anim_hatch, hatch_state.State());
		DG()->UpdateStatusIndicators();
	}

	// air venting particle stream
	if (hatch_vent && simt > hatch_vent_t + 1.0) {
		DG()->DelExhaustStream (hatch_vent);
		hatch_vent = NULL;
	}

	// test for damage condition
	if (hatchfail < 2 && hatch_state.State() > 0.05 && DG()->GetDynPressure() > 30e3) {
		if (oapiRand() < 1.0 - exp(-simdt*0.2)) {
			if (++hatchfail == 1) {  // jam hatch
				hatch_state.SetState (0.2, 0.0);
				DG()->SetAnimation(anim_hatch, 0.2);
			} else {                 // tear off hatch
				static UINT HatchGrp[2] = {12,88};
				GROUPEDITSPEC ges;
				ges.flags = GRPEDIT_SETUSERFLAG;
				ges.UsrFlag = 3;
				for (int i = 0; i < 2; i++)
					oapiEditMeshGroup (DG()->exmesh, HatchGrp[i], &ges);
			}
		}
	}
}

// --------------------------------------------------------------

bool TophatchCtrl::clbkLoadPanel2D (int panelid, PANELHANDLE hPanel, DWORD viewW, DWORD viewH)
{
	if (panelid != 1) return false;

	// Hatch open/close switch
	SURFHANDLE panel2dtex = oapiGetTextureHandle(DG()->panelmesh1,1);
	DG()->RegisterPanelArea (hPanel, ELID_SWITCH, _R(434,192,460,244), PANEL_REDRAW_MOUSE, PANEL_MOUSE_LBDOWN|PANEL_MOUSE_LBUP, panel2dtex, sw);
	sw->DefineAnimation2D (DG()->panelmesh1, GRP_INSTRUMENTS_ABOVE_P1, 0);

	return true;
}

// --------------------------------------------------------------

bool TophatchCtrl::clbkLoadVC (int vcid)
{
	if (vcid != 0) return false;

	// Hatch open/close switch
	oapiVCRegisterArea (ELID_SWITCH, PANEL_REDRAW_MOUSE, PANEL_MOUSE_LBDOWN);
	oapiVCSetAreaClickmode_Quadrilateral (ELID_SWITCH, VC_HATCH_SWITCH_mousearea[0], VC_HATCH_SWITCH_mousearea[1], VC_HATCH_SWITCH_mousearea[2], VC_HATCH_SWITCH_mousearea[3]);
	sw->DefineAnimationVC (VC_HATCH_SWITCH_ref, VC_HATCH_SWITCH_axis, GRP_SWITCH1_VC, VC_HATCH_SWITCH_vofs);

	return true;
}

// --------------------------------------------------------------

bool TophatchCtrl::clbkPlaybackEvent (double simt, double event_t, const char *event_type, const char *event)
{
	if (!_stricmp (event_type, "HATCH")) {
		if (!_stricmp (event, "CLOSE")) CloseHatch();
		else                            OpenHatch();
		return true;
	}
	return false;
}

// --------------------------------------------------------------

int TophatchCtrl::clbkConsumeBufferedKey(DWORD key, bool down, char *kstate)
{
	if (KEYMOD_ALT(kstate) || KEYMOD_SHIFT(kstate))
		return 0;

	if (key == OAPI_KEY_U) {
		Revert();
		return 1;
	}
	return 0;
}

// --------------------------------------------------------------

void TophatchCtrl::RepairDamage ()
{
	if (hatchfail) {
		hatch_state.SetState (0.0, 0.0);
		DG()->SetAnimation (anim_hatch, 0.0);
		static UINT HatchGrp[2] = {12,88};
		GROUPEDITSPEC ges;
		ges.flags = GRPEDIT_SETUSERFLAG;
		ges.UsrFlag = 0;
		for (int i = 0; i < 2; i++)
			oapiEditMeshGroup (DG()->exmesh, HatchGrp[i], &ges);
	}
}

// ==============================================================

HatchCtrlSwitch::HatchCtrlSwitch (TophatchCtrl *comp)
: DGSwitch1(comp->DG(), DGSwitch1::TWOSTATE), component(comp)
{
}

// --------------------------------------------------------------

void HatchCtrlSwitch::Reset2D (int panelid, MESHHANDLE hMesh)
{
	SetState (component->hatch_state.IsClosed() || component->hatch_state.IsClosing() ?
		DOWN : UP);
}

// --------------------------------------------------------------

void HatchCtrlSwitch::ResetVC (DEVMESHHANDLE hMesh)
{
	DGSwitch1::ResetVC (hMesh);
	SetState (component->hatch_state.IsClosed() || component->hatch_state.IsClosing() ?
		DOWN : UP);
}

// --------------------------------------------------------------

bool HatchCtrlSwitch::ProcessMouse2D (int event, int mx, int my)
{
	if (DGSwitch1::ProcessMouse2D (event, mx, my)) {
		if (GetState() == UP) component->OpenHatch();
		else                  component->CloseHatch();
		return true;
	}
	return false;
}

// --------------------------------------------------------------

bool HatchCtrlSwitch::ProcessMouseVC (int event, VECTOR3 &p)
{
	if (DGSwitch1::ProcessMouseVC (event, p)) {
		if (GetState() == UP) component->OpenHatch();
		else                  component->CloseHatch();
		return true;
	}
	return false;
}


// ==============================================================

PValveSwitch::PValveSwitch (PressureSubsystem *_subsys, int id)
: DGSwitch1(_subsys->DG(), DGSwitch1::TWOSTATE), subsys(_subsys), vid(id)
{
}

// --------------------------------------------------------------

void PValveSwitch::Reset2D (int panelid, MESHHANDLE hMesh)
{
	SetState (subsys->GetPValve(vid) ? UP:DOWN);
}

// --------------------------------------------------------------

void PValveSwitch::ResetVC (DEVMESHHANDLE hMesh)
{
	DGSwitch1::ResetVC (hMesh);
	SetState (subsys->GetPValve(vid) ? UP:DOWN);
}

// --------------------------------------------------------------

bool PValveSwitch::ProcessMouse2D (int event, int mx, int my)
{
	if (DGSwitch1::ProcessMouse2D (event, mx, my)) {
		DGSwitch1::State state = GetState();
		subsys->SetPValve (vid, state==UP ? 1:0);
		return true;
	}
	return false;
}

// --------------------------------------------------------------

bool PValveSwitch::ProcessMouseVC (int event, VECTOR3 &p)
{
	if (DGSwitch1::ProcessMouseVC (event, p)) {
		DGSwitch1::State state = GetState();
		subsys->SetPValve (vid, state==UP ? 1:0);
		return true;
	}
	return false;
}

// ==============================================================

PressureIndicator::PressureIndicator (PressureSubsystem *_subsys, SURFHANDLE blitsrc)
: PanelElement(_subsys->DG()), subsys(_subsys), bsrc(blitsrc)
{
	btgt = 0;
}

// --------------------------------------------------------------

void PressureIndicator::ResetVC (DEVMESHHANDLE hMesh)
{
	upt = 0.0;
	btgt = oapiGetTextureHandle (subsys->DG()->vcmesh_tpl, 14);
	for (int i = 0; i < 4; i++)
		for (int j = 0; j < 8; j++)
			label[i][j] = ' ';
}

// --------------------------------------------------------------

bool PressureIndicator::Redraw ()
{
	if (!btgt) return false;

	double t = oapiGetSimTime();
	if (t < upt && t > upt-1.0) return false;
	upt = t + 0.5;

	char cbuf[16];
	ValStr (subsys->PExtHatch(), cbuf);
	BlitReadout (0, cbuf);
	ValStr (subsys->PCabin(), cbuf);
	BlitReadout (1, cbuf);
	ValStr (subsys->PAirlock(), cbuf);
	BlitReadout (2, cbuf);
	ValStr (subsys->PExtLock(), cbuf);
	BlitReadout (3, cbuf);

	return false;
}

// --------------------------------------------------------------

bool PressureIndicator::Redraw2D (SURFHANDLE surf)
{
	btgt = surf;
	return Redraw();
}

// --------------------------------------------------------------

bool PressureIndicator::RedrawVC (DEVMESHHANDLE hMesh, SURFHANDLE surf)
{
	return Redraw();
}

// --------------------------------------------------------------

void PressureIndicator::ValStr (double p, char *cbuf)
{
	p *= 1e-3; // convert to kPa
	if (p > 300.0) {
		strcpy (cbuf, "-----");
	} else {
		sprintf (cbuf, "%5.1lf", p);
	}
}

// --------------------------------------------------------------

void PressureIndicator::BlitReadout (int which, const char *str)
{
	int tgtx = 65 + which*61;
	int tgty = 17;
	int srcx, srcy = 0;
	int w = 8;
	int h = 11;
	char c, *tgtstr = label[which];
	for (int i = 0; i < 5; i++) {
		if ((c=str[i]) != tgtstr[i]) {
			if (c >= '0' && c <= '9') srcx = (c-'0')*8;
			else switch (c) {
				case '.': srcx = 10*8; break;
				case '+': srcx = 11*8; break;
				case '-': srcx = 12*8; break;
				case 'k': srcx = 13*8; break;
				case 'M': srcx = 14*8; break;
				case 'G': srcx = 15*8; break;
				default:  srcx = 16*8; break;
			}
			oapiBlt (btgt, bsrc, tgtx, tgty, srcx, srcy, w, h);
			tgtstr[i] = c;
		}
		tgtx += w;
	}
}
