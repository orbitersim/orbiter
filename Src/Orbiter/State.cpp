// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// =============================================================
// class State
// Defines a simulation state at a given time (e.g. for load/save)
// Contains solar system environment, time, focus vessel, scenario help page
// =============================================================

#define STRICT 1

#include <fstream>
#include <iomanip>
#include <string.h>
#include <stdio.h>
#include "Orbiter.h"
#include "Config.h"
#include "State.h"
#include "Vessel.h"
#include "Astro.h"

using namespace std;

extern Vessel *g_focusobj;
extern TimeData td;

// =============================================================

State::State ()
{
	desc = 0;
	mjd = mjd0 = MJD (time (NULL)); // default to current system time
	strcpy (solsys, "Sol");         // default name
	memset (scenario, 0, 256);
	memset (context, 0, 64);
	memset (scnhelp, 0, 128);       // no scenario help by default
	memset (script, 0, 128);
	memset (playback, 0, 128);
}

void State::Update (const char *_desc)
{
	desc = _desc;
	mjd  = td.MJD1;
	strcpy (focus, g_focusobj->Name());
}

bool State::Read (const char *fname)
{
	ifstream ifs (fname, ios::in);
	if (!ifs) return false;

	int i;
	strncpy (scenario, fname, 255);
	for (i = strlen(fname); i >= 0; i--)
		if (fname[i] == '.') break;
	if (i >= 0 && i < 256) scenario[i] = '\0';

	char cbuf[256], *pc;
	double t;
	mjd0 = MJD (time (NULL)); // default to current system time
	mjd0 += UTC_CT_diff*day;  // map from UTC to CT (or TDB) time scales
	mjd = mjd0;
	strcpy (solsys, "Sol");         // default name
	memset (context, 0, 64);
	memset (scnhelp, 0, 128);       // no scenario help by default
	memset (script, 0, 128);        // no scenario script by default
	memset (playback, 0, 128);

	if (FindLine (ifs, "BEGIN_ENVIRONMENT")) {
		for (;;) {
			if (!ifs.getline (cbuf, 256)) break;
			pc = trim_string (cbuf);
			if (!_stricmp (pc, "END_ENVIRONMENT")) break;
			if (!_strnicmp (pc, "Date", 4)) {
				pc = trim_string (pc+4);
				if (!_strnicmp (pc, "MJD", 3) && sscanf (pc+3, "%lf", &t) == 1)
					mjd = mjd0 = t;
				else if (!_strnicmp (pc, "JD", 2) && sscanf (pc+2, "%lf", &t) == 1)
					mjd = mjd0 = t-2400000.5;
				else if (!_strnicmp (pc, "JE", 2) && sscanf (pc+2, "%lf", &t) == 1)
					mjd = mjd0 = Jepoch2MJD (t);
			} else if (!_strnicmp (pc, "System", 6)) {
				strcpy (solsys, trim_string (pc+6));
			} else if (!_strnicmp (pc, "Context", 7)) {
				strcpy (context, trim_string (pc+7));
			} else if (!_strnicmp (pc, "Script", 6)) {
				strcpy (script, trim_string (pc+6));
			} else if (!_strnicmp (pc, "Help", 4)) {
				strncpy (scnhelp, trim_string (pc+4), 127);
			} else if (!_strnicmp (pc, "Playback", 8)) {
				strncpy (playback, trim_string (pc+8), 127);
			}
		}
	}
	if (FindLine (ifs, "BEGIN_FOCUS")) {
		for (;;) {
			if (!ifs.getline (cbuf, 256)) break;
			pc = trim_string (cbuf);
			if (!_stricmp (pc, "END_FOCUS")) break;
			if (!_strnicmp (pc, "Ship", 4)) {
				strcpy (focus, trim_string (pc+4));
			}
		}
	}
	return true;
}

void State::Write (ostream &ofs, const char *help) const
{
	ofs.setf (ios::fixed, ios::floatfield);
	ofs.precision (10); // need very high precision MJD output
	if (desc) {
		ofs << "BEGIN_DESC" << endl;
		ofs << desc << endl; // need to break lines
		ofs << "END_DESC" << endl << endl;
	}
	ofs << "BEGIN_ENVIRONMENT" << endl;
	ofs << "  System " << solsys << endl;
	ofs << "  Date MJD " << mjd << endl;
	if (context[0])
		ofs << "  Context " << context << endl;
	if (script[0])
		ofs << "  Script " << script << endl;
	if (scnhelp[0])
		ofs << "  Help " << scnhelp << endl;
	else if (help)
		ofs << "  Help " << help << endl;
	if (playback[0])
		ofs << "  Playback " << playback << endl;
	ofs << "END_ENVIRONMENT" << endl << endl;

	ofs << "BEGIN_FOCUS" << endl;
	ofs << "  Ship " << focus << endl;
	ofs << "END_FOCUS" << endl << endl;
}
