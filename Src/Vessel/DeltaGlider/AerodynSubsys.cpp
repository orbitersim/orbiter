// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//                ORBITER MODULE: DeltaGlider
//                  Part of the ORBITER SDK
//
// AerodynSubsys.cpp
// Subsystem for aerodynamic controls (selector dial, elevator
// trim, airbrake)
// ==============================================================

#define STRICT 1

#include "AerodynSubsys.h"
#include "meshres.h"
#include "meshres_p0.h"
#include "meshres_vc.h"
#include "dg_vc_anim.h"

using std::min;
using std::max;

// ==============================================================
// Aerodynamic control subsystem
// ==============================================================

AerodynCtrlSubsystem::AerodynCtrlSubsystem (DeltaGlider *v)
: DGSubsystem (v)
{
	// create component instances
	AddSubsystem (selector = new AerodynSelector (this));
	AddSubsystem (airbrake = new Airbrake (this));
	AddSubsystem (elevtrim = new ElevatorTrim (this));
}

// --------------------------------------------------------------

void AerodynCtrlSubsystem::SetMode (DWORD mode)
{
	selector->SetMode (mode);
}

// --------------------------------------------------------------

void AerodynCtrlSubsystem::ExtendAirbrake ()
{
	airbrake->Extend();
}

// --------------------------------------------------------------

void AerodynCtrlSubsystem::RetractAirbrake ()
{
	airbrake->Retract();
}

// --------------------------------------------------------------

const AnimState2 &AerodynCtrlSubsystem::AirbrakeState() const
{
	return airbrake->State();
}

// ==============================================================
// Control selector dial
// ==============================================================

AerodynSelector::AerodynSelector (AerodynCtrlSubsystem *_subsys)
: DGSubsystem(_subsys)
{
	ELID_DIAL = AddElement (dial = new AerodynSelectorDial (this));
}

// --------------------------------------------------------------

void AerodynSelector::SetMode (DWORD mode)
{
	DWORD curmode = DG()->GetADCtrlMode();
	if (curmode != mode) DG()->SetADCtrlMode (mode);
	DG()->TriggerRedrawArea (0, 0, ELID_DIAL);

}

// --------------------------------------------------------------

DWORD AerodynSelector::GetMode () const
{
	return DG()->GetADCtrlMode();
}

// --------------------------------------------------------------

bool AerodynSelector::IncMode ()
{
	DWORD mode = DG()->GetADCtrlMode();
	if (mode <= 1) {
		DG()->SetADCtrlMode (mode ? 7 : 1);
		return true;
	} else return false;
}

// --------------------------------------------------------------

bool AerodynSelector::DecMode ()
{
	DWORD mode = min (DG()->GetADCtrlMode(),(DWORD)2);
	if (mode) {
		DG()->SetADCtrlMode (mode-1);
		return true;
	} else return false;
}

// --------------------------------------------------------------

bool AerodynSelector::clbkLoadPanel2D (int panelid, PANELHANDLE hPanel, DWORD viewW, DWORD viewH)
{
	if (panelid != 0) return false;

	// mode dial
	SURFHANDLE panel2dtex = oapiGetTextureHandle(DG()->panelmesh0,1);
	DG()->RegisterPanelArea (hPanel, ELID_DIAL, _R(23,69,63,113), PANEL_REDRAW_MOUSE, PANEL_MOUSE_LBDOWN, panel2dtex, dial);

	return true;
}

// --------------------------------------------------------------

bool AerodynSelector::clbkLoadVC (int vcid)
{
	if (vcid != 0) return false;

	// mode dial
	oapiVCRegisterArea (ELID_DIAL, PANEL_REDRAW_USER, PANEL_MOUSE_LBDOWN);
	oapiVCSetAreaClickmode_Quadrilateral (ELID_DIAL, VC_AF_DIAL_mousearea[0], VC_AF_DIAL_mousearea[1], VC_AF_DIAL_mousearea[2], VC_AF_DIAL_mousearea[3]);
	dial->DefineAnimationVC (VC_AF_DIAL_ref, VC_AF_DIAL_axis, GRP_DIAL1_VC, VC_AF_DIAL_vofs);

	return true;
}

// ==============================================================

AerodynSelectorDial::AerodynSelectorDial (AerodynSelector *comp)
: DGDial1(comp->DG(), 3, -50*RAD, 50*RAD), component(comp)
{
}

// --------------------------------------------------------------

void AerodynSelectorDial::Reset2D (int panelid, MESHHANDLE hMesh)
{
	grp = oapiMeshGroup (hMesh, GRP_INSTRUMENTS_ABOVE_P0);
	vtxofs = 4;
}

// --------------------------------------------------------------

void AerodynSelectorDial::ResetVC (DEVMESHHANDLE hMesh)
{
	DGDial1::ResetVC (hMesh);
	DWORD mode = vessel->GetADCtrlMode();
	SetPosition (mode == 0 ? 0 : mode == 7 ? 1 : 2);
}

// --------------------------------------------------------------

bool AerodynSelectorDial::Redraw2D (SURFHANDLE surf)
{
	// constants for texture coordinates
	static const float texw = (float)PANEL2D_TEXW; // texture width
	static const float texh = (float)PANEL2D_TEXH; // texture height
	static const float tx_x0 = 1160.5f;            // left edge of texture block
	static const float tx_y0 = texh-615.5f;        // top edge of texture block
	static const float tx_dx = 39.0f;              // texture block width
	static const float tx_dy = 43.0f;              // texture block height
	static float tu[4] = {tx_x0/texw,(tx_x0+tx_dx)/texw,tx_x0/texw,(tx_x0+tx_dx)/texw};

	float dtu = (float)(min(vessel->GetADCtrlMode(),(DWORD)2)*40.0)/texw;
	for (int i = 0; i < 4; i++)
		grp->Vtx[vtxofs+i].tu = tu[i]+dtu;
	return false;
}

// --------------------------------------------------------------

bool AerodynSelectorDial::RedrawVC (DEVMESHHANDLE hMesh, SURFHANDLE surf)
{
	DWORD mode = component->GetMode();
	SetPosition(mode == 0 ? 0 : mode == 7 ? 1 : 2);
	return DGDial1::RedrawVC (hMesh, surf);
}

// --------------------------------------------------------------

bool AerodynSelectorDial::ProcessMouse2D (int event, int mx, int my)
{
	return (mx < 20 ? component->DecMode() : component->IncMode());
}

// --------------------------------------------------------------

bool AerodynSelectorDial::ProcessMouseVC (int event, VECTOR3 &p)
{
	if (DGDial1::ProcessMouseVC (event, p)) {
		int pos = GetPosition();
		component->SetMode (pos == 0 ? 0 : pos == 1 ? 7 : 1);
		return true;
	}
	return false;
}

// ==============================================================
// Airbrake
// ==============================================================

Airbrake::Airbrake (AerodynCtrlSubsystem *_subsys)
: DGSubsystem(_subsys)
{
	brake_state.SetOperatingSpeed (AIRBRAKE_OPERATING_SPEED);
	lever_state.SetOperatingSpeed (4.0);
	airbrake_tgt = 0;
	ELID_LEVER = AddElement (lever = new AirbrakeLever (this));

	// Airbrake animation
	static UINT RRudderGrp[2] = {GRP_RRudder1,GRP_RRudder2};
	static UINT LRudderGrp[2] = {GRP_LRudder1,GRP_LRudder2};
	static UINT UpperBrakeGrp[4] = {GRP_RUAileron1,GRP_LUAileron1,GRP_LUAileron2,GRP_RUAileron2};
	static MGROUP_ROTATE UpperBrake (0, UpperBrakeGrp, 4,
		_V(0,-0.4,-6.0), _V(1,0,0), (float)(50*RAD));
	static UINT LowerBrakeGrp[4] = {GRP_LLAileron1,GRP_RLAileron1,GRP_LLAileron2,GRP_RLAileron2};
	static MGROUP_ROTATE LowerBrake (0, LowerBrakeGrp, 4,
		_V(0,-0.4,-6.0), _V(1,0,0), (float)(-50*RAD));
	static MGROUP_ROTATE RRudderBrake (0, RRudderGrp, 2,
		_V( 8.668,0.958,-6.204), _V( 0.143,0.975,-0.172), (float)( 25*RAD));
	static MGROUP_ROTATE LRudderBrake (0, LRudderGrp, 2,
		_V(-8.668,0.958,-6.204), _V(-0.143,0.975,-0.172), (float)(-25*RAD));

	anim_brake = DG()->CreateAnimation (0);
	DG()->AddAnimationComponent (anim_brake, 0, 1, &UpperBrake);
	DG()->AddAnimationComponent (anim_brake, 0, 1, &LowerBrake);
	DG()->AddAnimationComponent (anim_brake, 0, 1, &RRudderBrake);
	DG()->AddAnimationComponent (anim_brake, 0, 1, &LRudderBrake);

	// Airbrake lever animation
	static UINT AirbrakeLeverGrp = GRP_AIRBRAKE_LEVER_VC;
	static MGROUP_ROTATE AirbrakeLeverTransform (1, &AirbrakeLeverGrp, 1,
		VC_AIRBRAKELEVER_ref, VC_AIRBRAKELEVER_axis, (float)(-40*RAD));
	anim_airbrakelever = DG()->CreateAnimation(0.8);
	DG()->AddAnimationComponent (anim_airbrakelever, 0, 1, &AirbrakeLeverTransform);
}

// --------------------------------------------------------------

void Airbrake::Extend ()
{
	const double eps = 1e-8;
	brake_state.Open();
	lever_state.Open();
	airbrake_tgt = (lever_state.State() < 0.5-eps ? 1:2);
	DG()->TriggerPanelRedrawArea (0, ELID_LEVER);
	DG()->RecordEvent ("AIRBRAKE", "OPEN");
}

// --------------------------------------------------------------

void Airbrake::Retract ()
{
	const double eps = 1e-8;
	brake_state.Close();
	lever_state.Close();
	airbrake_tgt = (lever_state.State() > 0.5+eps ? 1:0);
	DG()->TriggerPanelRedrawArea (0, ELID_LEVER);
	DG()->RecordEvent ("AIRBRAKE", "CLOSE");
}

// --------------------------------------------------------------

void Airbrake::clbkPostStep (double simt, double simdt, double mjd)
{
	// animate airbrake
	if (brake_state.Process (simdt)) {
		if (airbrake_tgt == 1) { // intermediate position
			if ((brake_state.IsClosing() && brake_state.State() < 0.5) ||
				(brake_state.IsOpening() && brake_state.State() > 0.5))
				brake_state.SetState (0.5, 0.0);
		}
		DG()->SetAnimation (anim_brake, brake_state.State());
		DG()->UpdateStatusIndicators();
	}

	// animate airbrake lever
	if (lever_state.Process (simdt)) {
		if (airbrake_tgt == 1) { // intermediate position
			if ((lever_state.IsClosing() && lever_state.State() < 0.5) ||
				(lever_state.IsOpening() && lever_state.State() > 0.5))
				lever_state.SetState (0.5, 0.0);
		}
		DG()->SetAnimation (anim_airbrakelever, lever_state.State());
	}
}

// --------------------------------------------------------------

bool Airbrake::clbkLoadPanel2D (int panelid, PANELHANDLE hPanel, DWORD viewW, DWORD viewH)
{
	if (panelid != 0) return false;

	// airbrake lever
	SURFHANDLE panel2dtex = oapiGetTextureHandle(DG()->panelmesh0,1);
	DG()->RegisterPanelArea (hPanel, ELID_LEVER, _R(138,299,158,359), PANEL_REDRAW_USER, PANEL_MOUSE_LBDOWN, panel2dtex, lever);

	return true;
}

// --------------------------------------------------------------

bool Airbrake::clbkLoadVC (int vcid)
{
	if (vcid != 0) return false;

	// Airbrake lever
	oapiVCRegisterArea (ELID_LEVER, PANEL_REDRAW_NEVER, PANEL_MOUSE_LBDOWN);
	oapiVCSetAreaClickmode_Quadrilateral (ELID_LEVER, VC_AIRBRAKELEVER_mousearea[0], VC_AIRBRAKELEVER_mousearea[1], VC_AIRBRAKELEVER_mousearea[2], VC_AIRBRAKELEVER_mousearea[3]);

	return true;
}

// --------------------------------------------------------------

void Airbrake::clbkSaveState (FILEHANDLE scn)
{
	brake_state.SaveState (scn, "AIRBRAKE");
}

// --------------------------------------------------------------

bool Airbrake::clbkParseScenarioLine (const char *line)
{
	static const double eps = 1e-8;

	if (brake_state.ParseScenarioLine (line, "AIRBRAKE")) {
		if ((!brake_state.IsActive() && fabs(brake_state.State()-0.5) < eps) ||
			(brake_state.IsClosing() && brake_state.State() > 0.5) ||
			(brake_state.IsOpening() && brake_state.State() < 0.5))
			airbrake_tgt = 1;
		else if (brake_state.State() < 0.5)
			airbrake_tgt = 0;
		else
			airbrake_tgt = 2;
		return true;
	}
	lever_state.SetState (airbrake_tgt*0.5, 0);
	return false;
}

// --------------------------------------------------------------

void Airbrake::clbkPostCreation ()
{
	DG()->SetAnimation (anim_brake, brake_state.State());
	DG()->SetAnimation (anim_airbrakelever, lever_state.State());
}

// --------------------------------------------------------------

bool Airbrake::clbkPlaybackEvent (double simt, double event_t, const char *event_type, const char *event)
{
	if (!_stricmp (event_type, "AIRBRAKE")) {
		if (!_stricmp (event, "CLOSE")) Retract();
		else                            Extend();
		return true;
	}
	return false;
}

// --------------------------------------------------------------

int Airbrake::clbkConsumeBufferedKey (DWORD key, bool down, char *kstate)
{
	if (KEYMOD_ALT(kstate) || KEYMOD_SHIFT(kstate))
		return 0;

	if (key == OAPI_KEY_B) {
		if (KEYMOD_CONTROL(kstate))
			Retract();
		else
			Extend();
		return 1;
	}
	return 0;
}

// ==============================================================

AirbrakeLever::AirbrakeLever (Airbrake *comp)
: PanelElement(comp->DG()), component(comp)
{
}

// --------------------------------------------------------------

void AirbrakeLever::Reset2D (int panelid, MESHHANDLE hMesh)
{
	state = -1;
	grp = oapiMeshGroup (hMesh, GRP_INSTRUMENTS_ABOVE_P0);
	vtxofs = 64;
}

// --------------------------------------------------------------

void AirbrakeLever::ResetVC (DEVMESHHANDLE hMesh)
{
	DeltaGlider *dg = component->DG();
	dg->SetAnimation (component->anim_airbrakelever, component->lever_state.State());
}

// --------------------------------------------------------------

bool AirbrakeLever::Redraw2D (SURFHANDLE surf)
{
	// constants for panel coordinates
	static const float bb_y0 =  301.5f;
	static const float bb_dy =    7.0f;

	DeltaGlider* dg = component->DG();
	int newstate = component->airbrake_tgt;
	if (newstate != state) {
		state = newstate;
		static const float yp[4] = {bb_y0, bb_y0, bb_y0+bb_dy, bb_y0+bb_dy};
		float yshift = state*24.0f;
		for (int i = 0; i < 4; i++)
			grp->Vtx[vtxofs+i].y = yp[i]+yshift;
	}
	return false;
}

// --------------------------------------------------------------

bool AirbrakeLever::ProcessMouse2D (int event, int mx, int my)
{
	if (my > 30) component->Extend();
	else         component->Retract();
	return false;
}

// --------------------------------------------------------------

bool AirbrakeLever::ProcessMouseVC (int event, VECTOR3 &p)
{
	if (p.y > 0.5) component->Retract();
	else           component->Extend();
	return false;
}

// ==============================================================
// Elevator trim control
// ==============================================================

ElevatorTrim::ElevatorTrim (AerodynCtrlSubsystem *_subsys)
: DGSubsystem(_subsys)
{
	ELID_TRIMWHEEL = AddElement (trimwheel = new ElevatorTrimWheel (this));

	// Trim wheel animation
	static UINT TrimWheelGrp = GRP_ETRIM_WHEEL_VC;
	static MGROUP_ROTATE TrimWheelTransform (1, &TrimWheelGrp, 1,
		VC_ETRIMWHEEL_ref, VC_ETRIMWHEEL_axis, (float)(PI*0.06));
	anim_vc_trimwheel = DG()->CreateAnimation (0.5);
	DG()->AddAnimationComponent (anim_vc_trimwheel, 0, 1, &TrimWheelTransform);
}

// --------------------------------------------------------------

void ElevatorTrim::clbkSaveState (FILEHANDLE scn)
{
	double trim = DG()->GetControlSurfaceLevel (AIRCTRL_ELEVATORTRIM);
	if (trim) oapiWriteScenario_float (scn, (char*)"TRIM", trim);
}

// --------------------------------------------------------------

bool ElevatorTrim::clbkParseScenarioLine (const char *line)
{
	if (!_strnicmp (line, "TRIM", 4)) {
		double trim;
		sscanf (line+4, "%lf", &trim);
		DG()->SetControlSurfaceLevel (AIRCTRL_ELEVATORTRIM, trim, true);
		return true;
	}
	return false;
}

// --------------------------------------------------------------

bool ElevatorTrim::clbkLoadPanel2D (int panelid, PANELHANDLE hPanel, DWORD viewW, DWORD viewH)
{
	if (panelid != 0) return false;

	// elevator trim wheel
	SURFHANDLE panel2dtex = oapiGetTextureHandle(DG()->panelmesh0,1);
	DG()->RegisterPanelArea (hPanel, ELID_TRIMWHEEL, _R(87,299,107,359), PANEL_REDRAW_ALWAYS, PANEL_MOUSE_LBDOWN | PANEL_MOUSE_LBPRESSED, panel2dtex, trimwheel);

	return true;
}

// --------------------------------------------------------------

bool ElevatorTrim::clbkLoadVC (int vcid)
{
	if (vcid != 0) return false;

	// Elevator trim wheel
	oapiVCRegisterArea (ELID_TRIMWHEEL, PANEL_REDRAW_ALWAYS, PANEL_MOUSE_LBDOWN|PANEL_MOUSE_LBPRESSED);
	oapiVCSetAreaClickmode_Quadrilateral (ELID_TRIMWHEEL, VC_ETRIMWHEEL_mousearea[0], VC_ETRIMWHEEL_mousearea[1], VC_ETRIMWHEEL_mousearea[2], VC_ETRIMWHEEL_mousearea[3]);

	return true;
}

// ==============================================================

ElevatorTrimWheel::ElevatorTrimWheel (ElevatorTrim *comp)
: PanelElement(comp->DG()), component(comp)
{
}

// --------------------------------------------------------------

void ElevatorTrimWheel::Reset2D (int panelid, MESHHANDLE hMesh)
{
	trimpos2D = 0.0;
	grp = oapiMeshGroup (hMesh, GRP_INSTRUMENTS_ABOVE_P0);
	vtxofs = 60;
}

// --------------------------------------------------------------

void ElevatorTrimWheel::ResetVC (DEVMESHHANDLE hMesh)
{
	trimposVC = 0.0;
}

// --------------------------------------------------------------

bool ElevatorTrimWheel::Redraw2D (SURFHANDLE surf)
{
	// constants for panel coordinates
	static const float bb_y0 =  325.5f;
	static const float bb_dy =    7.0f;

	double level = vessel->GetControlSurfaceLevel (AIRCTRL_ELEVATORTRIM);
	if (level != trimpos2D) {
		static const float yp[4] = {bb_y0, bb_y0, bb_y0+bb_dy, bb_y0+bb_dy};
		float yshift = (float)(level*24.0);
		for (int i = 0; i < 4; i++)
			grp->Vtx[vtxofs+i].y = yp[i]+yshift;
		trimpos2D = level;
	}
	return false;
}

// --------------------------------------------------------------

bool ElevatorTrimWheel::RedrawVC (DEVMESHHANDLE hMesh, SURFHANDLE surf)
{
	if (!hMesh) return false;

	double level = vessel->GetControlSurfaceLevel (AIRCTRL_ELEVATORTRIM);
	if (level != trimposVC) {
		const DWORD nvtx = 3;
		WORD vidx[3] = {VC_ETRIMSCALE_vofs,VC_ETRIMSCALE_vofs+1,VC_ETRIMSCALE_vofs+2};
		NTVERTEX vtx[nvtx];
		GROUPEDITSPEC ges;
		ges.flags = GRPEDIT_VTXCRDY|GRPEDIT_VTXCRDZ;
		ges.nVtx = nvtx;
		ges.Vtx = vtx;
		ges.vIdx = vidx;

		static const double tilt = atan(VC_ETRIMSCALE_axis.z/VC_ETRIMSCALE_axis.y);
		static const double y0[3] = {VC_ETRIMSCALE_ref[0].y,VC_ETRIMSCALE_ref[1].y,VC_ETRIMSCALE_ref[2].y};
		static const double z0[3] = {VC_ETRIMSCALE_ref[0].z,VC_ETRIMSCALE_ref[1].z,VC_ETRIMSCALE_ref[2].z};
		static double range = 0.032;
		static double dy = -range*cos(tilt), dz = -range*sin(tilt);
		for (DWORD i = 0; i < nvtx; i++) {
			vtx[i].y = (float)(y0[i] + level*dy);
			vtx[i].z = (float)(z0[i] + level*dz);
		}
		oapiEditMeshGroup (hMesh, GRP_VC4_LIT_VC, &ges);

		DeltaGlider *dg = (DeltaGlider*)vessel;
		double v;
		vessel->SetAnimation (component->anim_vc_trimwheel, modf((1-level)*20, &v));

		trimposVC = level;
	}
	return false;
}

// --------------------------------------------------------------

bool ElevatorTrimWheel::ProcessMouse2D (int event, int mx, int my)
{
	double tgtlvl = vessel->GetControlSurfaceLevel (AIRCTRL_ELEVATORTRIM);
	tgtlvl += oapiGetSimStep() * (my < 30 ? -0.2:0.2);
	tgtlvl = max (-1.0, min (1.0, tgtlvl));
	vessel->SetControlSurfaceLevel (AIRCTRL_ELEVATORTRIM, tgtlvl);
	return true;
}

// --------------------------------------------------------------

bool ElevatorTrimWheel::ProcessMouseVC (int event, VECTOR3 &p)
{
	double dtrim = oapiGetSimStep() * (p.y < 0.5 ? 0.2:-0.2);
	double trim0 = vessel->GetControlSurfaceLevel (AIRCTRL_ELEVATORTRIM);
	double trim1 = max(-1.0, min(1.0, trim0+dtrim));

	if (trim0 != trim1) {
		vessel->SetControlSurfaceLevel (AIRCTRL_ELEVATORTRIM, trim1);
		return true;
	}
	return false;
}
