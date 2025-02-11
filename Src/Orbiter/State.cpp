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
#include <string>
#include <stdio.h>
#include "Orbiter.h"
#include "Config.h"
#include "State.h"
#include "Vessel.h"
#include "Astro.h"
#include "Util.h"

using namespace std;

extern Vessel *g_focusobj;
extern TimeData td;

// =============================================================

State::State ()
{
	mjd = mjd0 = MJD (time (NULL)); // default to current system time
	solsys = "Sol";         // default name
}

void State::Update ()
{
	mjd  = td.MJD1;
	focus = g_focusobj->Name();
}

bool State::Read (const char *fname)
{
	ifstream ifs (fname, ios::in);
	if (!ifs) return false;

	int i;
	scenario = fname;
	for (i = strlen(fname); i >= 0; i--)
		if (fname[i] == '.') break;
	if (i >= 0 && i < 256) scenario[i] = '\0';

	char cbuf[256], *pc;
	double t;
	mjd0 = MJD (time (NULL)); // default to current system time
	mjd0 += UTC_CT_diff*day;  // map from UTC to CT (or TDB) time scales
	mjd = mjd0;
	solsys.clear();           // no scenario solsys by default
	context.clear();          // no scenario context by default
	splashscreen.clear();     // no scplashscreen by default
	script.clear();           // no scenario script by default
	scnhelp.clear();          // no scenario help by default
	playback.clear();         // no scenario playback by default
	focus.clear();            // no scenario focus by default

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
				solsys = trim_string (pc+6);
			} else if (!_strnicmp (pc, "Context", 7)) {
				context = trim_string (pc+7);
			} else if (!_strnicmp (pc, "SplashScreen", 12)) {
				char color[256];
				int nChar = 0;
				if(sscanf(pc+12, "%255s %n", &color, &nChar)==1) {
					splashcolor = GetCSSColor(color);
					splashscreen = trim_string (pc+12+nChar);
				}
			} else if (!_strnicmp (pc, "Script", 6)) {
				script = trim_string (pc+6);
			} else if (!_strnicmp (pc, "Help", 4)) {
				scnhelp = trim_string (pc+4);
			} else if (!_strnicmp (pc, "Playback", 8)) {
				playback = trim_string (pc+8);
			}
		}
	}
	if (FindLine (ifs, "BEGIN_FOCUS")) {
		for (;;) {
			if (!ifs.getline (cbuf, 256)) break;
			pc = trim_string (cbuf);
			if (!_stricmp (pc, "END_FOCUS")) break;
			if (!_strnicmp (pc, "Ship", 4)) {
				focus = trim_string (pc+4);
			}
		}
	}
	return true;
}

void State::Write (ostream &ofs, const char *desc, int desc_fmt, const char *help) const
{
	const std::string descTypeStr[3] = { "DESC", "HYPERDESC", "URLDESC" };

	ofs.setf (ios::fixed, ios::floatfield);
	ofs.precision (10); // need very high precision MJD output
	if (desc) {
		if (desc_fmt < 0 || desc_fmt > 2) desc_fmt = 0;
		ofs << "BEGIN_" << descTypeStr[desc_fmt] << std::endl;
		for (const char* c = desc; *c; c++)
			if (*c != '\r') ofs << *c; // DOS madness! Get rid of CR so output stream can add it again ... 
		ofs << endl;
		ofs << "END_" << descTypeStr[desc_fmt] << endl << endl;
	}
	ofs << "BEGIN_ENVIRONMENT" << endl;
	ofs << "  System " << solsys << endl;
	ofs << "  Date MJD " << mjd << endl;
	if (context.length())
		ofs << "  Context " << context << endl;
	if (splashscreen.length())
		ofs << "  SplashScreen " << splashscreen << endl;
	if (script.length())
		ofs << "  Script " << script << endl;
	if (scnhelp.length())
		ofs << "  Help " << scnhelp << endl;
	else if (help)
		ofs << "  Help " << help << endl;
	if (playback.length())
		ofs << "  Playback " << playback << endl;
	ofs << "END_ENVIRONMENT" << endl << endl;

	ofs << "BEGIN_FOCUS" << endl;
	ofs << "  Ship " << focus << endl;
	ofs << "END_FOCUS" << endl << endl;
}
