// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//                ORBITER MODULE: DeltaGlider
//                  Part of the ORBITER SDK
//
// LightSubsys.cpp
// Cockpit and external light control: instrument/overhead lights,
// landing/docking/navigation/strobe lights
// ==============================================================

#define STRICT 1

#include "LightSubsys.h"
#include "meshres_p1.h"
#include "meshres_vc.h"
#include "dg_vc_anim.h"

using std::min;
using std::max;

// ==============================================================
// Light control subsystem
// ==============================================================

LightCtrlSubsystem::LightCtrlSubsystem (DeltaGlider *v)
: DGSubsystem (v)
{
	// create component instances
	AddSubsystem (instrlight = new InstrumentLight (this));
	AddSubsystem (cockpitlight = new CockpitLight (this));
	AddSubsystem (landdocklight = new LandDockLight (this));
	AddSubsystem (strobelight = new StrobeLight (this));
	AddSubsystem (navlight = new NavLight (this));
}

// ==============================================================
// Instrument lights
// ==============================================================

InstrumentLight::InstrumentLight (LightCtrlSubsystem *_subsys)
: DGSubsystem(_subsys)
{
	light_on   = false;
	brightness = 0.5;
	light_col  = 0;

	ELID_SWITCH = AddElement (sw = new InstrumentLightSwitch (this));
	ELID_DIAL = AddElement (dial = new InstrumentBrightnessDial (this));

	// Instrument brightness dial animation
	static UINT InstrBDialGrp = GRP_INSTR_BRIGHTNESS_VC;
	static MGROUP_ROTATE InstrBDialTransform (1, &InstrBDialGrp, 1,
		VC_INSTR_BRIGHTNESS_ref, VC_INSTR_BRIGHTNESS_axis, (float)(-280*RAD));
	anim_dial = DG()->CreateAnimation (0.5);
	DG()->AddAnimationComponent (anim_dial, 0, 1, &InstrBDialTransform);
}

// --------------------------------------------------------------

void InstrumentLight::SetLight (bool on, bool force)
{
	if (on == light_on && !force) return; // nothing to do

	if (on != light_on) {
		light_on = on;
		sw->SetState (light_on ? DGSwitch1::UP : DGSwitch1::DOWN);
		DG()->TriggerRedrawArea(1, 0, ELID_SWITCH);
	}

	if (DG()->vcmesh) {
		static MATERIAL norm = {{1,1,1,1},{0.8,0.8,0.8,1},{0.1,0.1,0.1,0},{0.15,0.15,0.15,1},5};
		static MATERIAL label_glow[3] = {
			{{0,0,0,0},{0,0,0,0},{0,0,0,0},{0.35,1,0.35,1},0},
		    {{0,0,0,0},{0,0,0,0},{0,0,0,0},{1,0.7,0.15,1},0},
			{{0,0,0,0},{0,0,0,0},{0,0,0,0},{0.6,0.6,1.0,1},0}};
		static MATERIAL btn_glow = {{1,1,1,1},{1,1,1,1},{0.1,0.1,0.1,1},{0.6,0.6,0.6,1},5};
		oapiSetMaterial (DG()->vcmesh, 0, on ? &btn_glow : &norm);
		int idx = max(0, min(2, light_col));
		MATERIAL mat;
		if (on) {
			float scale = (float)(0.2 + brightness*0.8);
			memcpy(&mat, label_glow+light_col, sizeof(MATERIAL));
			mat.emissive.r *= scale;
			mat.emissive.g *= scale;
			mat.emissive.b *= scale;
		}
		oapiSetMaterial (DG()->vcmesh, 11, on ? &mat : &norm);
		GROUPEDITSPEC ges = {(DWORD)(on ? GRPEDIT_ADDUSERFLAG : GRPEDIT_DELUSERFLAG), 0x18, 0,0,0};
		oapiEditMeshGroup (DG()->vcmesh, GRP_LIT_LABELS_VC, &ges);
	}
}

// --------------------------------------------------------------

void InstrumentLight::ModBrightness (bool up)
{
	double dt = oapiGetSimStep();
	double db = dt * (up ? 0.3 : -0.3);
	brightness = max(0.0, min (1.0, brightness + db));
	DG()->SetAnimation (anim_dial, brightness);
	if (light_on) SetLight (true, true);
}

// --------------------------------------------------------------

void InstrumentLight::clbkSaveState (FILEHANDLE scn)
{
	if (light_on || light_col || brightness != 0.5) {
		char cbuf[256];
		sprintf (cbuf, "%d %d %0.2lf", (int)light_on, light_col, brightness);
		oapiWriteScenario_string (scn, (char*)"INSTRLIGHT", cbuf);
	}
}

// --------------------------------------------------------------

bool InstrumentLight::clbkParseScenarioLine (const char *line)
{
	if (!strnicmp (line, "INSTRLIGHT", 10)) {
		int lon;
		sscanf (line+10, "%d%d%lf", &lon, &light_col, &brightness);
		light_on = (lon != 0);
		return true;
	}
	return false;
}

// --------------------------------------------------------------

bool InstrumentLight::clbkLoadVC (int vcid)
{
	if (vcid != 0) return false;

	// Instrument light switch
	oapiVCRegisterArea (ELID_SWITCH, PANEL_REDRAW_MOUSE, PANEL_MOUSE_LBDOWN);
	oapiVCSetAreaClickmode_Quadrilateral (ELID_SWITCH, VC_INSTRLIGHT_SWITCH_mousearea[0], VC_INSTRLIGHT_SWITCH_mousearea[1], VC_INSTRLIGHT_SWITCH_mousearea[2], VC_INSTRLIGHT_SWITCH_mousearea[3]);
	sw->DefineAnimationVC (VC_INSTRLIGHT_SWITCH_ref, VC_INSTRLIGHT_SWITCH_axis, GRP_SWITCH1_VC, VC_INSTRLIGHT_SWITCH_vofs);

	// Instrument brightness dial
	oapiVCRegisterArea (ELID_DIAL, PANEL_REDRAW_NEVER, PANEL_MOUSE_LBDOWN | PANEL_MOUSE_LBPRESSED | PANEL_MOUSE_LBUP);
	oapiVCSetAreaClickmode_Quadrilateral (ELID_DIAL, VC_INSTR_BRIGHTNESS_mousearea[0], VC_INSTR_BRIGHTNESS_mousearea[1], VC_INSTR_BRIGHTNESS_mousearea[2], VC_INSTR_BRIGHTNESS_mousearea[3]);

	return true;
}

// --------------------------------------------------------------

void InstrumentLight::clbkResetVC (int vcid, DEVMESHHANDLE hMesh)
{
	SetLight (light_on, true);
	DG()->SetAnimation (anim_dial, brightness);
}

// ==============================================================

InstrumentLightSwitch::InstrumentLightSwitch (InstrumentLight *comp)
: DGSwitch1(comp->DG()), component(comp)
{
}

// --------------------------------------------------------------

void InstrumentLightSwitch::ResetVC (DEVMESHHANDLE hMesh)
{
	SetState (component->GetLight() ? UP:DOWN);
	DGSwitch1::ResetVC (hMesh);
}

// --------------------------------------------------------------

bool InstrumentLightSwitch::ProcessMouseVC (int event, VECTOR3 &p)
{
	if (DGSwitch1::ProcessMouseVC (event, p)) {
		DGSwitch1::State state = GetState();
		component->SetLight (state==DGSwitch1::UP ? 1:0);
		return true;
	}
	return false;
}

// ==============================================================

InstrumentBrightnessDial::InstrumentBrightnessDial (InstrumentLight *comp)
: PanelElement(comp->DG()), component(comp)
{
}

// --------------------------------------------------------------

bool InstrumentBrightnessDial::ProcessMouseVC (int event, VECTOR3 &p)
{
	component->ModBrightness (p.x > 0.5);
	return true;
}

// ==============================================================
// Cockpit floodlights
// ==============================================================

CockpitLight::CockpitLight (LightCtrlSubsystem *_subsys)
: DGSubsystem(_subsys)
{
	light = NULL;
	light_mode = 0;
	brightness = 0.7;

	ELID_SWITCH = AddElement (sw = new CockpitLightSwitch (this));
	ELID_DIAL = AddElement (dial = new CockpitBrightnessDial (this));

	// Floodlight brightness dial animation
	static UINT FloodBDialGrp = GRP_FLOOD_BRIGHTNESS_VC;
	static MGROUP_ROTATE FloodBDialTransform (1, &FloodBDialGrp, 1,
		VC_FLOOD_BRIGHTNESS_ref, VC_FLOOD_BRIGHTNESS_axis, (float)(-280*RAD));
	anim_dial = DG()->CreateAnimation (0.5);
	DG()->AddAnimationComponent (anim_dial, 0, 1, &FloodBDialTransform);
}

// --------------------------------------------------------------

void CockpitLight::SetLight (int mode, bool force)
{
	if (mode == light_mode && !force) return; // nothing to do

	if (mode != light_mode) {
		light_mode = mode;
		sw->SetState (mode == 0 ? DGSwitch1::CENTER : mode == 1 ? DGSwitch1::UP : DGSwitch1::DOWN);
		DG()->TriggerRedrawArea(1, 0, ELID_SWITCH);
	}

	if (light) {
		DG()->DelLightEmitter (light);
		light = NULL;
	}
	if (mode) {
		static const COLOUR4 zero = {0.0f,0.0f,0.0f,0.0f};
		static const COLOUR4 wcol = {1.0f,1.0f,1.0f,0.0f};
		static const COLOUR4 rcol = {0.6f,0.05f,0.0f,0.0f};
		COLOUR4 col = (mode == 1 ? wcol : rcol);
		light = (PointLight*)DG()->AddPointLight(_V(0,1.65,6.68), 3, 0, 0, 3, col, col, zero);
		light->SetVisibility (LightEmitter::VIS_COCKPIT);
		light->Activate(true);
		double intens = (float)(0.2 + brightness*0.8);
		light->SetIntensity (intens);
	}
}

// --------------------------------------------------------------

void CockpitLight::ModBrightness (bool up)
{
	double dt = oapiGetSimStep();
	double db = dt * (up ? 0.3 : -0.3);
	brightness = max(0.0, min (1.0, brightness + db));
	DG()->SetAnimation (anim_dial, brightness);
	if (light_mode) SetLight (light_mode, true);
}

// --------------------------------------------------------------

void CockpitLight::clbkSaveState (FILEHANDLE scn)
{
	if (light_mode || brightness != 0.7) {
		char cbuf[256];
		sprintf (cbuf, "%d %0.2lf", light_mode, brightness);
		oapiWriteScenario_string (scn, (char*)"FLOODLIGHT", cbuf);
	}
}

// --------------------------------------------------------------

bool CockpitLight::clbkParseScenarioLine (const char *line)
{
	if (!strnicmp (line, "FLOODLIGHT", 10)) {
		sscanf (line+10, "%d%lf", &light_mode, &brightness);
		light_mode = max (0, min (2, light_mode));
		brightness = max (0.0, min (1.0, brightness));
		return true;
	}
	return false;
}

// --------------------------------------------------------------

bool CockpitLight::clbkLoadVC (int vcid)
{
	if (vcid != 0) return false;

	// Floodlight switch
	oapiVCRegisterArea (ELID_SWITCH, PANEL_REDRAW_MOUSE, PANEL_MOUSE_LBDOWN);
	oapiVCSetAreaClickmode_Quadrilateral (ELID_SWITCH, VC_FLOODLIGHT_SWITCH_mousearea[0], VC_FLOODLIGHT_SWITCH_mousearea[1], VC_FLOODLIGHT_SWITCH_mousearea[2], VC_FLOODLIGHT_SWITCH_mousearea[3]);
	sw->DefineAnimationVC (VC_FLOODLIGHT_SWITCH_ref, VC_FLOODLIGHT_SWITCH_axis, GRP_SWITCH1_VC, VC_FLOODLIGHT_SWITCH_vofs);

	// Floodlight brightness dial
	oapiVCRegisterArea (ELID_DIAL, PANEL_REDRAW_NEVER, PANEL_MOUSE_LBDOWN | PANEL_MOUSE_LBPRESSED | PANEL_MOUSE_LBUP);
	oapiVCSetAreaClickmode_Quadrilateral (ELID_DIAL, VC_FLOOD_BRIGHTNESS_mousearea[0], VC_FLOOD_BRIGHTNESS_mousearea[1], VC_FLOOD_BRIGHTNESS_mousearea[2], VC_FLOOD_BRIGHTNESS_mousearea[3]);

	return true;
}

// --------------------------------------------------------------

void CockpitLight::clbkResetVC (int vcid, DEVMESHHANDLE hMesh)
{
	SetLight (light_mode, true);
	DG()->SetAnimation (anim_dial, brightness);
}

// ==============================================================

CockpitLightSwitch::CockpitLightSwitch (CockpitLight *comp)
: DGSwitch1(comp->DG(), DGSwitch1::THREESTATE), component(comp)
{
}

// --------------------------------------------------------------

void CockpitLightSwitch::ResetVC (DEVMESHHANDLE hMesh)
{
	SetState ((DGSwitch1::State)component->GetLight());
	DGSwitch1::ResetVC (hMesh);
}

// --------------------------------------------------------------

bool CockpitLightSwitch::ProcessMouseVC (int event, VECTOR3 &p)
{
	if (DGSwitch1::ProcessMouseVC (event, p)) {
		DGSwitch1::State state = GetState();
		component->SetLight ((int)state);
		return true;
	}
	return false;
}

// ==============================================================

CockpitBrightnessDial::CockpitBrightnessDial (CockpitLight *comp)
: PanelElement(comp->DG()), component(comp)
{
}

// --------------------------------------------------------------

bool CockpitBrightnessDial::ProcessMouseVC (int event, VECTOR3 &p)
{
	component->ModBrightness (p.x > 0.5);
	return true;
}

// ==============================================================
// Landing/docking lights
// ==============================================================

LandDockLight::LandDockLight (LightCtrlSubsystem *_subsys)
: DGSubsystem(_subsys)
{
	light_mode = 0;
	light = NULL;
	ELID_SWITCH = AddElement (sw = new LandDockLightSwitch (this));
}

// --------------------------------------------------------------

void LandDockLight::SetLight (int mode, bool force)
{
	if (mode == light_mode && !force) return; // nothing to do

	if (mode != light_mode) {
		light_mode = mode;
		sw->SetState(mode == 0 ? DGSwitch1::CENTER : mode == 1 ? DGSwitch1::UP : DGSwitch1::DOWN);
		DG()->TriggerRedrawArea(1, 0, ELID_SWITCH);
	}
		
	if (light) {
		DG()->DelLightEmitter (light);
		light = NULL;
	}
	if (mode) {
		COLOUR4 col_a = {0,0,0,0};
		COLOUR4 col_white = {1,1,1,0};
		if (mode == 1) {
			light = (SpotLight*)DG()->AddSpotLight(_V(0.3,0.3,8.5), _V(0,0,1), 150, 1e-3, 0, 1e-3, RAD*30, RAD*60, col_white, col_white, col_a);
		} else {
			double tilt = -10.0*RAD;
			light = (SpotLight*)DG()->AddSpotLight(_V(0.1,-0.3,7.5), _V(0,sin(tilt),cos(tilt)), 5000, 1e-3, 1e-5, 2e-7, RAD*25, RAD*40, col_white, col_white, col_a);
		}
		light->SetVisibility (LightEmitter::VIS_ALWAYS);
	}
}

// --------------------------------------------------------------

void LandDockLight::clbkSaveState (FILEHANDLE scn)
{
	if (light_mode)
		oapiWriteScenario_int (scn, (char*)"LANDDOCKLIGHT", light_mode);
}

// --------------------------------------------------------------

bool LandDockLight::clbkParseScenarioLine (const char *line)
{
	if (!strnicmp (line, "LANDDOCKLIGHT", 13)) {
		sscanf (line+13, "%d", &light_mode);
		light_mode = max (0, min (2, light_mode));
		return true;
	}
	return false;
}

// --------------------------------------------------------------

bool LandDockLight::clbkLoadPanel2D (int panelid, PANELHANDLE hPanel, DWORD viewW, DWORD viewH)
{
	if (panelid != 1) return false;

	// Landing/docking light switch
	SURFHANDLE panel2dtex = oapiGetTextureHandle(DG()->panelmesh1,1);
	DG()->RegisterPanelArea (hPanel, ELID_SWITCH, _R(708,192,734,244), PANEL_REDRAW_MOUSE, PANEL_MOUSE_LBDOWN|PANEL_MOUSE_LBUP, panel2dtex, sw);
	sw->DefineAnimation2D (DG()->panelmesh1, GRP_INSTRUMENTS_ABOVE_P1, 12);

	return true;
}

// --------------------------------------------------------------

bool LandDockLight::clbkLoadVC (int vcid)
{
	if (vcid != 0) return false;

	// Landing/docking light switch
	oapiVCRegisterArea (ELID_SWITCH, PANEL_REDRAW_MOUSE, PANEL_MOUSE_LBDOWN);
	oapiVCSetAreaClickmode_Quadrilateral (ELID_SWITCH, VC_LANDINGLIGHT_SWITCH_mousearea[0], VC_LANDINGLIGHT_SWITCH_mousearea[1], VC_LANDINGLIGHT_SWITCH_mousearea[2], VC_LANDINGLIGHT_SWITCH_mousearea[3]);
	sw->DefineAnimationVC (VC_LANDINGLIGHT_SWITCH_ref, VC_LANDINGLIGHT_SWITCH_axis, GRP_SWITCH1_VC, VC_LANDINGLIGHT_SWITCH_vofs);

	return true;
}

// --------------------------------------------------------------

void LandDockLight::clbkResetVC (int vcid, DEVMESHHANDLE hMesh)
{
	SetLight (light_mode, true);
}

// ==============================================================

LandDockLightSwitch::LandDockLightSwitch (LandDockLight *comp)
: DGSwitch1(comp->DG(), DGSwitch1::THREESTATE), component(comp)
{
}

// --------------------------------------------------------------

bool LandDockLightSwitch::ProcessMouse2D (int event, int mx, int my)
{
	if (DGSwitch1::ProcessMouse2D (event, mx, my)) {
		DGSwitch1::State state = GetState();
		component->SetLight ((int)state);
		return true;
	}
	return false;
}

// --------------------------------------------------------------

bool LandDockLightSwitch::ProcessMouseVC (int event, VECTOR3 &p)
{
	if (DGSwitch1::ProcessMouseVC (event, p)) {
		DGSwitch1::State state = GetState();
		component->SetLight ((int)state);
		return true;
	}
	return false;
}

// --------------------------------------------------------------

void LandDockLightSwitch::Reset2D (int panelid, MESHHANDLE hMesh)
{
	SetState ((DGSwitch1::State)component->GetLight());
	DGSwitch1::Reset2D (panelid, hMesh);
}

// --------------------------------------------------------------

void LandDockLightSwitch::ResetVC (DEVMESHHANDLE hMesh)
{
	SetState ((DGSwitch1::State)component->GetLight());
	DGSwitch1::ResetVC (hMesh);
}

// ==============================================================
// Strobes
// ==============================================================

StrobeLight::StrobeLight (LightCtrlSubsystem *_subsys)
: DGSubsystem(_subsys)
{
	light_on = false;
	ELID_SWITCH = AddElement (sw = new StrobeLightSwitch (this));
}

// --------------------------------------------------------------

void StrobeLight::SetLight (bool on)
{
	if (on != light_on) {
		light_on = on;
		for (int i = 3; i <= 6; i++) DG()->beacon[i].active = on;
		sw->SetState(on ? DGSwitch1::UP : DGSwitch1::DOWN);
		DG()->TriggerRedrawArea(1, 0, ELID_SWITCH);
	}
}

// --------------------------------------------------------------

void StrobeLight::clbkSaveState (FILEHANDLE scn)
{
	if (light_on)
		oapiWriteScenario_int (scn, (char*)"STROBELIGHT", (int)light_on);
}

// --------------------------------------------------------------

bool StrobeLight::clbkParseScenarioLine (const char *line)
{
	if (!strnicmp (line, "STROBELIGHT", 11)) {
		int mode;
		sscanf (line+11, "%d", &mode);
		light_on = (mode != 0);
		return true;
	}
	return false;
}

// --------------------------------------------------------------

bool StrobeLight::clbkLoadPanel2D (int panelid, PANELHANDLE hPanel, DWORD viewW, DWORD viewH)
{
	if (panelid != 1) return false;

	// Landing/docking light switch
	SURFHANDLE panel2dtex = oapiGetTextureHandle(DG()->panelmesh1,1);
	DG()->RegisterPanelArea (hPanel, ELID_SWITCH, _R(754,192,780,244), PANEL_REDRAW_MOUSE, PANEL_MOUSE_LBDOWN|PANEL_MOUSE_LBUP, panel2dtex, sw);
	sw->DefineAnimation2D (DG()->panelmesh1, GRP_INSTRUMENTS_ABOVE_P1, 16);

	return true;
}

// --------------------------------------------------------------

bool StrobeLight::clbkLoadVC (int vcid)
{
	if (vcid != 0) return false;

	// Strobe light switch
	oapiVCRegisterArea (ELID_SWITCH, PANEL_REDRAW_MOUSE, PANEL_MOUSE_LBDOWN);
	oapiVCSetAreaClickmode_Quadrilateral (ELID_SWITCH, VC_STROBELIGHT_SWITCH_mousearea[0], VC_STROBELIGHT_SWITCH_mousearea[1], VC_STROBELIGHT_SWITCH_mousearea[2], VC_STROBELIGHT_SWITCH_mousearea[3]);
	sw->DefineAnimationVC (VC_STROBELIGHT_SWITCH_ref, VC_STROBELIGHT_SWITCH_axis, GRP_SWITCH1_VC, VC_STROBELIGHT_SWITCH_vofs);

	return true;
}

// --------------------------------------------------------------

void StrobeLight::clbkResetVC (int vcid, DEVMESHHANDLE hMesh)
{
	SetLight (light_on);
}

// ==============================================================

StrobeLightSwitch::StrobeLightSwitch (StrobeLight *comp)
: DGSwitch1(comp->DG(), DGSwitch1::TWOSTATE), component(comp)
{
}

// --------------------------------------------------------------

void StrobeLightSwitch::Reset2D (int panelid, MESHHANDLE hMesh)
{
	SetState (component->GetLight() ? UP:DOWN);
	DGSwitch1::Reset2D (panelid, hMesh);
}

// --------------------------------------------------------------

void StrobeLightSwitch::ResetVC (DEVMESHHANDLE hMesh)
{
	SetState (component->GetLight() ? UP:DOWN);
	DGSwitch1::ResetVC (hMesh);
}

// --------------------------------------------------------------

bool StrobeLightSwitch::ProcessMouse2D (int event, int mx, int my)
{
	if (DGSwitch1::ProcessMouse2D (event, mx, my)) {
		DGSwitch1::State state = GetState();
		component->SetLight (state==UP);
		return true;
	}
	return false;
}

// --------------------------------------------------------------

bool StrobeLightSwitch::ProcessMouseVC (int event, VECTOR3 &p)
{
	if (DGSwitch1::ProcessMouseVC (event, p)) {
		DGSwitch1::State state = GetState();
		component->SetLight (state==UP);
		return true;
	}
	return false;
}

// ==============================================================
// Navigation lights
// ==============================================================

NavLight::NavLight (LightCtrlSubsystem *_subsys)
: DGSubsystem(_subsys)
{
	light_on = false;
	ELID_SWITCH = AddElement (sw = new NavLightSwitch (this));
}

// --------------------------------------------------------------

void NavLight::SetLight (bool on)
{
	if (on != light_on) {
		light_on = on;
		for (int i = 0; i <= 2; i++) DG()->beacon[i].active = on;
		sw->SetState(on ? DGSwitch1::UP : DGSwitch1::DOWN);
		DG()->TriggerRedrawArea(1, 0, ELID_SWITCH);
	}
}

// --------------------------------------------------------------

void NavLight::clbkSaveState (FILEHANDLE scn)
{
	if (light_on)
		oapiWriteScenario_int (scn, (char*)"NAVLIGHT", (int)light_on);
}

// --------------------------------------------------------------

bool NavLight::clbkParseScenarioLine (const char *line)
{
	if (!strnicmp (line, "NAVLIGHT", 8)) {
		int mode;
		sscanf (line+8, "%d", &mode);
		light_on = (mode != 0);
		return true;
	}
	return false;
}

// --------------------------------------------------------------

bool NavLight::clbkLoadPanel2D (int panelid, PANELHANDLE hPanel, DWORD viewW, DWORD viewH)
{
	if (panelid != 1) return false;

	// Nav light switch
	SURFHANDLE panel2dtex = oapiGetTextureHandle(DG()->panelmesh1,1);
	DG()->RegisterPanelArea (hPanel, ELID_SWITCH, _R(800,192,826,244), PANEL_REDRAW_MOUSE, PANEL_MOUSE_LBDOWN|PANEL_MOUSE_LBUP, panel2dtex, sw);
	sw->DefineAnimation2D (DG()->panelmesh1, GRP_INSTRUMENTS_ABOVE_P1, 20);

	return true;
}

// --------------------------------------------------------------

bool NavLight::clbkLoadVC (int vcid)
{
	if (vcid != 0) return false;

	// Nav light switch
	oapiVCRegisterArea (ELID_SWITCH, PANEL_REDRAW_MOUSE, PANEL_MOUSE_LBDOWN);
	oapiVCSetAreaClickmode_Quadrilateral (ELID_SWITCH, VC_NAVLIGHT_SWITCH_mousearea[0], VC_NAVLIGHT_SWITCH_mousearea[1], VC_NAVLIGHT_SWITCH_mousearea[2], VC_NAVLIGHT_SWITCH_mousearea[3]);
	sw->DefineAnimationVC (VC_NAVLIGHT_SWITCH_ref, VC_NAVLIGHT_SWITCH_axis, GRP_SWITCH1_VC, VC_NAVLIGHT_SWITCH_vofs);

	return true;
}

// --------------------------------------------------------------

void NavLight::clbkResetVC (int vcid, DEVMESHHANDLE hMesh)
{
	SetLight (light_on);
}

// ==============================================================

NavLightSwitch::NavLightSwitch (NavLight *comp)
: DGSwitch1(comp->DG(), DGSwitch1::TWOSTATE), component(comp)
{
}

// --------------------------------------------------------------

void NavLightSwitch::Reset2D (int panelid, MESHHANDLE hMesh)
{
	SetState (component->GetLight() ? UP:DOWN);
	DGSwitch1::Reset2D (panelid, hMesh);
}

// --------------------------------------------------------------

void NavLightSwitch::ResetVC (DEVMESHHANDLE hMesh)
{
	SetState (component->GetLight() ? UP:DOWN);
	DGSwitch1::ResetVC (hMesh);
}

// --------------------------------------------------------------

bool NavLightSwitch::ProcessMouse2D (int event, int mx, int my)
{
	if (DGSwitch1::ProcessMouse2D (event, mx, my)) {
		DGSwitch1::State state = GetState();
		component->SetLight (state==UP);
		return true;
	}
	return false;
}

// --------------------------------------------------------------

bool NavLightSwitch::ProcessMouseVC (int event, VECTOR3 &p)
{
	if (DGSwitch1::ProcessMouseVC (event, p)) {
		DGSwitch1::State state = GetState();
		component->SetLight (state==UP);
		return true;
	}
	return false;
}
