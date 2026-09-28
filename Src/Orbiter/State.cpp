// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// =============================================================
// class State
// Defines a simulation state at a given time (e.g. for load/save)
// Contains solar system environment, time, focus vessel, scenario help page
// =============================================================

#ifndef __linux__
#define STRICT 1
#else // __linux__
// STRICT left out: windows.h handle type-checking switch
#endif // __linux__

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
#ifndef __linux__
	ifstream ifs (fname, ios::in);
#else // __linux__
	ifstream ifs (oapiResolvePath (fname), ios::in);
#endif // __linux__
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
#ifndef __linux__
			if (!_stricmp (pc, "END_ENVIRONMENT")) break;
			if (!_strnicmp (pc, "Date", 4)) {
#else // __linux__
			if (!strcasecmp (pc, "END_ENVIRONMENT")) break;
			if (!strncasecmp (pc, "Date", 4)) {
#endif // __linux__
				pc = trim_string (pc+4);
#ifndef __linux__
				if (!_strnicmp (pc, "MJD", 3) && sscanf (pc+3, "%lf", &t) == 1)
#else // __linux__
				if (!strncasecmp (pc, "MJD", 3) && sscanf (pc+3, "%lf", &t) == 1)
#endif // __linux__
					mjd = mjd0 = t;
#ifndef __linux__
				else if (!_strnicmp (pc, "JD", 2) && sscanf (pc+2, "%lf", &t) == 1)
#else // __linux__
				else if (!strncasecmp (pc, "JD", 2) && sscanf (pc+2, "%lf", &t) == 1)
#endif // __linux__
					mjd = mjd0 = t-2400000.5;
#ifndef __linux__
				else if (!_strnicmp (pc, "JE", 2) && sscanf (pc+2, "%lf", &t) == 1)
#else // __linux__
				else if (!strncasecmp (pc, "JE", 2) && sscanf (pc+2, "%lf", &t) == 1)
#endif // __linux__
					mjd = mjd0 = Jepoch2MJD (t);
#ifndef __linux__
			} else if (!_strnicmp (pc, "System", 6)) {
#else // __linux__
			} else if (!strncasecmp (pc, "System", 6)) {
#endif // __linux__
				solsys = trim_string (pc+6);
#ifndef __linux__
			} else if (!_strnicmp (pc, "Context", 7)) {
#else // __linux__
			} else if (!strncasecmp (pc, "Context", 7)) {
#endif // __linux__
				context = trim_string (pc+7);
#ifndef __linux__
			} else if (!_strnicmp (pc, "SplashScreen", 12)) {
#else // __linux__
			} else if (!strncasecmp (pc, "SplashScreen", 12)) {
#endif // __linux__
				char color[256];
				int nChar = 0;
				if(sscanf(pc+12, "%255s %n", &color, &nChar)==1) {
					splashcolor = GetCSSColor(color);
					splashscreen = trim_string (pc+12+nChar);
				}
#ifndef __linux__
			} else if (!_strnicmp (pc, "Script", 6)) {
#else // __linux__
			} else if (!strncasecmp (pc, "Script", 6)) {
#endif // __linux__
				script = trim_string (pc+6);
#ifndef __linux__
			} else if (!_strnicmp (pc, "Help", 4)) {
#else // __linux__
			} else if (!strncasecmp (pc, "Help", 4)) {
#endif // __linux__
				scnhelp = trim_string (pc+4);
#ifndef __linux__
			} else if (!_strnicmp (pc, "Playback", 8)) {
#else // __linux__
			} else if (!strncasecmp (pc, "Playback", 8)) {
#endif // __linux__
				playback = trim_string (pc+8);
			}
		}
	}
	if (FindLine (ifs, "BEGIN_FOCUS")) {
		for (;;) {
			if (!ifs.getline (cbuf, 256)) break;
			pc = trim_string (cbuf);
#ifndef __linux__
			if (!_stricmp (pc, "END_FOCUS")) break;
			if (!_strnicmp (pc, "Ship", 4)) {
#else // __linux__
			if (!strcasecmp (pc, "END_FOCUS")) break;
			if (!strncasecmp (pc, "Ship", 4)) {
#endif // __linux__
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
