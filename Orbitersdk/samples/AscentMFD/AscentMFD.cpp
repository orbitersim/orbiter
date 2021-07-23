// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//                 ORBITER MODULE: AscentMFD
//                  Part of the ORBITER SDK
//
// AscentMFD.cpp
// Reference implementation of generic user-defined MFD mode
// ==============================================================

#define STRICT
#define ORBITER_MODULE
#include <windows.h>
#include <stdio.h>
#include <math.h>
#include "orbitersdk.h"
#include "AscentMFD.h"

// ==============================================================
// Global variables

const int ndata = 200;  // data points
const double sample_dt = 5.0; // data point interval

static struct {  // "Ascent MFD" parameters
	int mode;      // identifier for new MFD mode
} g_AscentMFD;

static struct {  // global data storage
	double tnext;  // time of next sample
	int   sample;  // current sample index
	float *time;   // sample time
	float *alt;    // altitude data
	float *pitch;  // pitch data
	float *rvel;   // radial velocity data
	float *tvel;   // tangential velocity data
} g_Data;

// ==============================================================
// API interface

DLLCLBK void InitModule (HINSTANCE hDLL)
{
	static char *name = "Ascent profile";
	MFDMODESPECEX spec;
	spec.name    = name;
	spec.key     = OAPI_KEY_P;
	spec.context = NULL;
	spec.msgproc = AscentMFD::MsgProc;

	g_Data.tnext  = 0.0;
	g_Data.sample = 0;
	g_Data.time   = new float[ndata];   memset (g_Data.time,  0, ndata*sizeof(float));
	g_Data.alt    = new float[ndata];   memset (g_Data.alt,   0, ndata*sizeof(float));
	g_Data.pitch  = new float[ndata];   memset (g_Data.pitch, 0, ndata*sizeof(float));
	g_Data.rvel   = new float[ndata];   memset (g_Data.rvel,  0, ndata*sizeof(float));
	g_Data.tvel   = new float[ndata];   memset (g_Data.tvel,  0, ndata*sizeof(float));

	g_AscentMFD.mode = oapiRegisterMFDMode (spec);
}

DLLCLBK void ExitModule (HINSTANCE hDLL)
{
	oapiUnregisterMFDMode (g_AscentMFD.mode);
	delete []g_Data.time;
	delete []g_Data.alt;
	delete []g_Data.pitch;
	delete []g_Data.rvel;
	delete []g_Data.tvel;
}

// We record vessel parameters outside the MFD to keep tracking
// even if the MFD mode doesn't exist

DLLCLBK void opcPreStep (double simt, double simdt, double mjd)
{
	if (simt >= g_Data.tnext) {
		VESSEL *v = oapiGetFocusInterface();
		VECTOR3 vel, pos;
		double a, r2, v2, vr2, vt2;
		double alt = v->GetAltitude();
		if (alt > v->GetSize()) { // start recording
			g_Data.time[g_Data.sample]  = (float)simt;
			g_Data.alt[g_Data.sample]   = (float)(alt*1e-3);
			g_Data.pitch[g_Data.sample] = (float)(v->GetPitch()*DEG);
			v->GetRelativeVel (v->GetSurfaceRef(), vel);
			// get radial and tangential velocity components
			v->GetRelativePos (v->GetSurfaceRef(), pos);
			r2 = pos.x*pos.x + pos.y*pos.y + pos.z*pos.z;
			v2 = vel.x*vel.x + vel.y*vel.y + vel.z*vel.z;
			a  = (vel.x*pos.x + vel.y*pos.y + vel.z*pos.z) / r2;
			vr2 = a*a * r2;
			vt2 = v2 - vr2;
			g_Data.rvel[g_Data.sample] = (vr2 >= 0.0 ? a >= 0.0 ? (float)sqrt(vr2) : -(float)sqrt(vr2) : 0.0f)*1e-3f;
			g_Data.tvel[g_Data.sample] = (vt2 >= 0.0 ? (float)sqrt(vt2) : 0.0f)*1e-3f;
			g_Data.sample = (g_Data.sample+1) % ndata;
			g_Data.tnext += sample_dt;
		}

		// DEBUG
		ELEMENTS el;
		double mjd;
		OBJHANDLE hRef = v->GetElements (el, mjd);
	}
}

// ==============================================================
// Ascent MFD implementation

AscentMFD::AscentMFD (DWORD w, DWORD h, VESSEL *vessel)
: GraphMFD (w, h, vessel)
{
	int g;

	tgt_alt = 200.0; // arbitrary [km]
	ref_alt = new float[ndata];
	ref_tvel = new float[ndata];
	ref = vessel->GetSurfaceRef();

	g = AddGraph ();
	SetAxisTitle (g, 0, "Time: s");
	SetAxisTitle (g, 1, "Alt: km");
	AddPlot (g, g_Data.time, g_Data.alt, ndata, 1, &g_Data.sample);

	g = AddGraph ();
	SetAxisTitle (g, 0, "Alt: km");
	SetAxisTitle (g, 1, "Pitch: deg");
	AddPlot (g, g_Data.alt, g_Data.pitch, ndata, 1, &g_Data.sample);

	g = AddGraph ();
	SetAxisTitle (g, 0, "Alt: km");
	SetAxisTitle (g, 1, "Vel: km/s");
	AddPlot (g, g_Data.alt, g_Data.rvel, ndata, 1, &g_Data.sample);

	g = AddGraph ();
	SetAxisTitle (g, 0, "Alt: km");
	SetAxisTitle (g, 1, "Vel: km/s");
	AddPlot (g, g_Data.alt, g_Data.tvel, ndata, 1, &g_Data.sample);
	AddPlot (g, ref_alt, ref_tvel, ndata, 5);

	alt_auto = true;
	vrad_auto = true;
	vtan_auto = true;
	page = 0;
}

AscentMFD::~AscentMFD ()
{
	delete []ref_alt;
	delete []ref_tvel;
}

void AscentMFD::InitReferences (void)
{
	const double G = 6.67259e-11;
	double M = oapiGetMass (ref);
	double R = oapiGetSize (ref);
	double f0 = graph[0].data_min;
	double f1 = (graph[0].data_max - graph[0].data_min)/(double)(ndata-1);
	double f2 = sqrt (G*M)*1e-3;
	int i;
	for (i = 0; i < ndata; i++) {
		ref_alt[i]  = (float)(f0 + i * f1);
		ref_tvel[i] = (float)(f2/sqrt(ref_alt[i]*1e3+R));
	}	
}

// message parser
int AscentMFD::MsgProc (UINT msg, UINT mfd, WPARAM wparam, LPARAM lparam)
{
	switch (msg) {
	case OAPI_MSG_MFD_OPENEDEX: {
		MFDMODEOPENSPEC *ospec = (MFDMODEOPENSPEC*)wparam;
		return (int)(new AscentMFD (ospec->w, ospec->h, (VESSEL*)lparam));
		}
	}
	return 0;
}

bool AscentMFD::ConsumeKeyBuffered (DWORD key)
{
	bool AltInput (void *id, char *str, void *data);
	bool VradInput (void *id, char *str, void *data);
	bool VtanInput (void *id, char *str, void *data);

	switch (key) {
	case OAPI_KEY_A:
		oapiOpenInputBox ("Altitude range (km) [min max, or 'a' for auto]:", AltInput, 0, 20, (void*)this);
		return true;
	case OAPI_KEY_P:
		page = (page+1) % 2;
		return true;
	case OAPI_KEY_R:
		oapiOpenInputBox ("Vrad range (km/s) [min max, or 'a' for auto]:", VradInput, 0, 20, (void*)this);
		return true;
	case OAPI_KEY_T:
		oapiOpenInputBox ("Vtan range (km/s) [min max, or 'a' for auto]:", VtanInput, 0, 20, (void*)this);
		return true;
	}
	return false;
}

bool AscentMFD::ConsumeButton (int bt, int event)
{
	if (!(event & PANEL_MOUSE_LBDOWN)) return false;
	static const DWORD btkey[4] = { OAPI_KEY_P, OAPI_KEY_A, OAPI_KEY_R, OAPI_KEY_T };
	if (bt < 4) return ConsumeKeyBuffered (btkey[bt]);
	else return false;
}

char *AscentMFD::ButtonLabel (int bt)
{
	char *label[4] = {"PG", "AR", "VRR", "VTR"};
	return (bt < 4 ? label[bt] : 0);
}

int AscentMFD::ButtonMenu (const MFDBUTTONMENU **menu) const
{
	static const MFDBUTTONMENU mnu[4] = {
		{"Select page", 0, 'P'},
		{"Altitude range", 0, 'A'},
		{"Radial", "velocity range", 'R'},
		{"Tangential", "velocity range", 'T'}
	};
	if (menu) *menu = mnu;
	return 4;
}

void AscentMFD::Update (HDC hDC)
{
	Title (hDC, "Ascent profile");

	if (alt_auto) {
		float altmin, altmax, tmp;
		FindRange (g_Data.alt, ndata, altmin, altmax);
		if (altmin > altmax)
			tmp = altmin, altmin = altmax, altmax = tmp;
		if (altmin == altmax)
			altmin -= 0.5, altmax += 0.5;
		SetRange (0, 1, altmin, altmax);
		SetRange (1, 0, altmin, altmax);
		SetRange (2, 0, altmin, altmax);
		SetRange (3, 0, altmin, altmax);
		InitReferences();
	}

	switch (page) {
	case 0:
		SetAutoRange (0, 0);
		Plot (hDC, 0, ch, (H+ch)/2, "Altitude");
		SetAutoRange (1, 1);
		Plot (hDC, 1, (H+ch)/2, H, "Pitch");
		break;
	case 1:
		if (vrad_auto) SetAutoRange (2, 1, 0);
		Plot (hDC, 2, ch, (H+ch)/2, "V rad");
		if (vtan_auto) SetAutoRange (3, 1, 0);
		Plot (hDC, 3, (H+ch)/2, H, "V tan");
		break;
	}
}

bool AscentMFD::SetAltRange (char *rstr)
{
	float altmin, altmax;

	if (rstr[0] == 'a' || rstr[0] == 'A') {
		alt_auto = true;
		return true;
	} else if (sscanf (rstr, "%f%f", &altmin, &altmax) == 2 && altmin < altmax) {
		alt_auto = false;
		SetRange (0, 1, altmin, altmax);
		SetRange (1, 0, altmin, altmax);
		SetRange (2, 0, altmin, altmax);
		SetRange (3, 0, altmin, altmax);
		InitReferences();
		return true;
	}
	return false;
}

bool AscentMFD::SetVradRange (char *rstr)
{
	float rmin, rmax;
	if (rstr[0] == 'a' || rstr[0] == 'A') {
		vrad_auto = true;
		return true;
	} else if (sscanf (rstr, "%f%f", &rmin, &rmax) == 2 && rmin < rmax) {
		vrad_auto = false;
		SetRange (2, 1, rmin, rmax);
		return true;
	}
	return false;
}

bool AscentMFD::SetVtanRange (char *rstr)
{
	float rmin, rmax;
	if (rstr[0] == 'a' || rstr[0] == 'A') {
		vtan_auto = true;
		return true;
	} else if (sscanf (rstr, "%f%f", &rmin, &rmax) == 2 && rmin < rmax) {
		vtan_auto = false;
		SetRange (3, 1, rmin, rmax);
		return true;
	}
	return false;
}

void AscentMFD::WriteStatus (FILEHANDLE scn) const
{
	char cbuf[256];
	oapiWriteScenario_int (scn, "PAGE", page);
	if (alt_auto) strcpy (cbuf, "AUTO");
	else sprintf (cbuf, "%0.1f %0.1f", graph[0].data_min, graph[0].data_max);
	oapiWriteScenario_string (scn, "ALTRANGE", cbuf);
	if (vrad_auto) strcpy (cbuf, "AUTO");
	else sprintf (cbuf, "%0.1f %0.1f", graph[2].data_min, graph[2].data_max);
	oapiWriteScenario_string (scn, "VRADRANGE", cbuf);
	if (vtan_auto) strcpy (cbuf, "AUTO");
	else sprintf (cbuf, "%0.1f %0.1f", graph[3].data_min, graph[3].data_max);
	oapiWriteScenario_string (scn, "VTANRANGE", cbuf);
}

void AscentMFD::ReadStatus (FILEHANDLE scn)
{
    char *line;
	while (oapiReadScenario_nextline (scn, line)) {
		if (!_strnicmp (line, "PAGE", 4))
			sscanf (line+4, "%d", &page);
		else if (!_strnicmp (line, "ALTRANGE", 8))
			SetAltRange (line+9);
		else if (!_strnicmp (line, "VRADRANGE", 9))
			SetVradRange (line+10);
		else if (!_strnicmp (line, "VTANRANGE", 9))
			SetVtanRange (line+10);
	}
}

void AscentMFD::StoreStatus (void) const
{
	saveprm.page     = page;
	saveprm.altmin   = (alt_auto  ? 0.0f : graph[0].data_min);
	saveprm.altmax   = (alt_auto  ? 0.0f : graph[0].data_max);
	saveprm.vradmin  = (vrad_auto ? 0.0f : graph[2].data_min);
	saveprm.vradmax  = (vrad_auto ? 0.0f : graph[2].data_max);
	saveprm.vtanmin  = (vtan_auto ? 0.0f : graph[3].data_min);
	saveprm.vtanmax  = (vtan_auto ? 0.0f : graph[3].data_max);
}

void AscentMFD::RecallStatus (void)
{
	char cbuf[128];
	page = saveprm.page;
	if (saveprm.altmin < saveprm.altmax)
		sprintf (cbuf, "%f %f", saveprm.altmin, saveprm.altmax);
	else strcpy (cbuf, "AUTO");
	SetAltRange (cbuf);
	if (saveprm.vradmin < saveprm.vradmax)
		sprintf (cbuf, "%f %f", saveprm.vradmin, saveprm.vradmax);
	else strcpy (cbuf, "AUTO");
	SetVradRange (cbuf);
	if (saveprm.vtanmin < saveprm.vtanmax)
		sprintf (cbuf, "%f %f", saveprm.vtanmin, saveprm.vtanmax);
	else strcpy (cbuf, "AUTO");
	SetVtanRange (cbuf);
}

bool AltInput (void *id, char *str, void *data)
{
	return ((AscentMFD*)data)->SetAltRange (str);
}

bool VradInput (void *id, char *str, void *data)
{
	return ((AscentMFD*)data)->SetVradRange (str);
}

bool VtanInput (void *id, char *str, void *data)
{
	return ((AscentMFD*)data)->SetVtanRange (str);
}

AscentMFD::SavePrm AscentMFD::saveprm = {0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0};
