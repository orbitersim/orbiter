// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//                ORBITER MODULE: DeltaGlider
//                  Part of the ORBITER SDK
//
// AAP.h
// Atmospheric autopilot
//
// Notes:
// The atmospheric autopilot provides functions for altitude,
// airspeed and heading/course.
// The autopilot drives the aerodynamic control surfaces, but
// not the RCS thrusters. It works only at sufficient 
// atmospheric pressure.
// The actual autopilot algorithms are implemented as scripts
// (Script/DG/aap.lua). This class simply provides the user
// interface to the script functions.
// ==============================================================

#include "AAPSubsys.h"
#include "InstrHsi.h"
#include "meshres_p0.h"

static const float texw = (float)PANEL2D_TEXW; // texture width
static const float texh = (float)PANEL2D_TEXH; // texture height
static const int xofs = 742;
static const int yofs = 400;
static const WORD idx_bb[6] = {0,1,2,3,2,1};

// ==============================================================
// Atmospheric autopilot subsystem
// ==============================================================

AAPSubsystem::AAPSubsystem (DGSubsystem *parent)
: DGSubsystem (parent)
{
	ELID_AAP = AddElement (aap = new AAP (this));
}

// --------------------------------------------------------------

bool AAPSubsystem::clbkLoadPanel2D (int panelid, PANELHANDLE hPanel, DWORD viewW, DWORD viewH)
{
	if (panelid != 0) return false;

	int xofs = 90, yofs = 149;
	DG()->RegisterPanelArea (hPanel, ELID_AAP, _R(xofs,yofs,xofs+65,yofs+124), PANEL_REDRAW_MOUSE, PANEL_MOUSE_LBDOWN | PANEL_MOUSE_LBPRESSED | PANEL_MOUSE_LBUP, 0, aap);
	return true;
}

// --------------------------------------------------------------

void AAPSubsystem::clbkSaveState (FILEHANDLE scn)
{
	aap->WriteScenario (scn);
}

// --------------------------------------------------------------

bool AAPSubsystem::clbkParseScenarioLine (const char *line)
{
	if (!_strnicmp (line, "AAP", 3)) {
		aap->SetState (line);
		return true;
	} else
		return false;
}

// --------------------------------------------------------------

void AAPSubsystem::AttachHSI (InstrHSI *_hsi)
{
	aap->AttachHSI(_hsi);
}

// ==============================================================

AAP::AAP (AAPSubsystem *_subsys)
: DGPanelElement (_subsys->DG()), subsys(_subsys)
{
	int i;
	hAAP = oapiCreateInterpreter();
	hsi = NULL;
	oapiExecScriptCmd (hAAP, "run('dg/aap')"); // load the autopilot code

	char setVesselCmd[256];
	sprintf_s(setVesselCmd,256,"setvessel(vessel.get_interface('%s'))",vessel->GetName());
	oapiAsyncScriptCmd (hAAP, setVesselCmd); // set autopilot vessel

	active_block = -1;
	for (i = 0; i < 3; i++) {
		tgt[i] = 0;
		active[i] = pactive[i] = false;
	}
	scanmode = scanpmode = 0;
}

// ==============================================================

AAP::~AAP()
{
	oapiDelInterpreter(hAAP);
}

// ==============================================================

void AAP::Reset2D (int panelid, MESHHANDLE hMesh)
{
	grp = oapiMeshGroup (hMesh, GRP_AAP_P0);
	if (grp)
		vtxofs = 0;

	for (int i = 0; i < 3; i++)
		strcpy (readout[i], "      ");
}

// ==============================================================

bool AAP::Redraw2D (SURFHANDLE surf)
{
	char *c, cbuf[16];
	int i, j, vofs;
	float xofs, yofs;

	// readouts
	c = DispStr(tgt[0])+1; // altitude
	UpdateStr (c, readout[0], 6, grp->Vtx+vtxofs);
	c = DispStr(tgt[1])+1; // airspeed
	UpdateStr (c, readout[1], 6, grp->Vtx+vtxofs+6/*8*/*4);
	sprintf (cbuf, "%03d", (int)(tgt[2]*DEG+0.5) % 360);
	UpdateStr (cbuf, readout[2], 3, grp->Vtx+vtxofs+12/*18*/*4);

	// activation buttons
	for (i = 0; i < 3; i++) {
		if (active[i] != pactive[i]) {
			pactive[i] = active[i];
			yofs = texh - 683 + (active[i] ? 14:0);
			vofs = vtxofs + 18*4 + i*4;
			for (j = 0; j < 4; j++)
				grp->Vtx[vofs+j].tv = (yofs + (j/2)*12.0f)/texh;
		}
	}

	if (active_block >= 0) {
		// scan switch
		if (scanmode != scanpmode) {
			scanpmode = scanmode;
			xofs = 1068.0f + (scanmode == 0 ? 0 : scanmode == -1 ? 16:32);
			vofs = vtxofs + 21*4 + active_block*4;
			for (j = 0; j < 4; j++)
				grp->Vtx[vofs+j].tu = (xofs - (j/2)*14.0f)/texw;
		}
	}
	return false;
}

// ==============================================================

bool AAP::ProcessMouse2D (int event, int mx, int my)
{
	static double t0 = 0.0, tp = 0.0;

	if (event & PANEL_MOUSE_LBDOWN) {
		if (my >= 0 && my < 124) {
			active_block = my/46;
			int dmy = my-active_block*46;
			if (mx >= 0 && mx < 14 && dmy >= 0 && dmy < 14) {
				// activation button
				ToggleActive (active_block);
				return true;
			} else if (mx >= 21 && mx < 65 && dmy >= 19 && dmy < 32) {
				// scan switch
				scanmode = (mx-21 < 22 ? -1 : 1);
				t0 = tp = oapiGetSysTime();
				return true;
			}
		} else {
			active_block = -1;
		}
	} else if (event == PANEL_MOUSE_LBUP) {
		scanmode = 0;
		return (scanmode != scanpmode);
	}

	if (scanmode && event == PANEL_MOUSE_LBPRESSED) {
		double t = oapiGetSysTime();
		double dt = max (t-t0, 0.0);
		double step, mag;
		switch (active_block) {
			case 0:
			case 1:
				mag = (dt < 1 ? 2 : dt < 2 ? 1 : 0);
				if (t-tp > 0.5-mag*0.2) {
					tp = t;
					step = max(1,min(1e4,pow(10,floor(log10 (max(tgt[active_block],1)))-mag)));
					tgt[active_block] = max(0,floor(tgt[active_block]/step)*step + scanmode*step);
					if (active[active_block]) SetValue (active_block, tgt[active_block]);
					return true;
				}
				break;
			case 2: // heading/course
				mag = min (sqrt(dt)*0.2, 0.8);
				step = oapiGetSysStep() * mag * scanmode;
				tgt[2] += step;
				while (tgt[2] < 0.0) tgt[2] += PI2;
				while (tgt[2] >= PI2) tgt[2] -= PI2;
				if (active[2]) SetValue (2, tgt[2]);
				if (hsi) hsi->SetCrs (tgt[2]);
				return true;
		}
	}
	return false;
}

// ==============================================================

void AAP::SetValue (int block, double val)
{
	char cbuf[256];
	switch (block) {
		case 0: // altitude
			sprintf (cbuf, "aap.alt(%e)", val);
			oapiAsyncScriptCmd (hAAP, cbuf);
			break;
		case 1: // airspeed
			sprintf (cbuf, "aap.spd(%e)", val);
			oapiAsyncScriptCmd (hAAP, cbuf);
			break;
		case 2: // heading/course
			sprintf (cbuf, "aap.hdg(%e)", val*DEG);
			oapiAsyncScriptCmd (hAAP, cbuf);
			break;
	}
}

void AAP::ToggleActive (int block)
{
	SetActive (block, !active[block]);
}

void AAP::SetActive (int block, bool activate)
{
	if (activate == active[block]) return; // nothing to do

	char cbuf[256];

	active[block] = activate;
	switch (block) {
		case 0: // altitude
			if (activate) sprintf (cbuf, "aap.alt(%e)", tgt[block]);
			else          strcpy (cbuf, "aap.alt()");
			oapiAsyncScriptCmd (hAAP, cbuf);
			break;
		case 1: // airspeed
			if (activate) sprintf (cbuf, "aap.spd(%e)", tgt[block]);
			else          strcpy (cbuf, "aap.spd()");
			oapiAsyncScriptCmd (hAAP, cbuf);
			break;
		case 2: // heading
			if (activate) sprintf (cbuf, "aap.hdg(%e)", tgt[block]*DEG);
			else          strcpy (cbuf, "aap.hdg()");
			oapiAsyncScriptCmd (hAAP, cbuf);
			break;
	}
}

void AAP::WriteScenario (FILEHANDLE scn)
{
	int i;
	char cbuf[256], line[256] = "";
	for (i = 0; i < 3; i++) {
		sprintf (cbuf, "%s%d:%g", i ? " ":"", active[i], tgt[i]);
		strcat (line, cbuf);
	}
	oapiWriteScenario_string (scn, "AAP", line);
}

void AAP::SetState (const char *str)
{
	int i, nitem, state[3];
	double val[3];
	nitem = sscanf (str, "AAP %d:%lf %d:%lf %d:%lf",
		state+0, val+0, state+1, val+1, state+2, val+2);
	if (nitem == 6) {
		for (i = 0; i < 3; i++) {
			tgt[i] = val[i];
			SetActive (i, state[i] != 0);
		}
	}
}

// ==============================================================

void AAP::UpdateStr (char *str, char *pstr, int n, NTVERTEX *vtx)
{
	static char cbuf[16], *s, *p;
	float x;
	static const float dx = 9.0f;
	int i, j, len;
	len = strlen(str);
	if (len < n) { // flush right
		memset (cbuf, ' ', n-len);
		memcpy (cbuf+(n-len), str, len);
		str = cbuf;
	}
	for (i = 0, s = str, p = pstr; i < n; i++, s++, p++) {
		if (*s != *p) {
			*p = *s;
			if (*s >= '0' && *s <= '9') {
				x = 1126 + (*s-'0')*dx;
			} else switch (*s) {
				case '.': x = 1215.5f; break;
				case 'k': x = 1229.5f; break;
				case 'M': x = 1238.5f; break;
				case 'G': x = 1248.5f; break;
				default:  x = 1221.5f; break;
			}
			for (j = 0; j < 4; j++)
				vtx[i*4+j].tu = (x + (j%2)*dx)/texw;
		}
	}
}