// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// =============================================================
// class State
// Defines a simulation state at a given time (e.g. for load/save)
// Contains ship positions/velocities, camera state, instrumentation ...

#ifndef __STATE_H
#define __STATE_H

#include <iostream>
#include <fstream>
#include <string>

class State {
public:
	State();

	double Mjd() const { return mjd; }
	char *Solsys() { return const_cast<char *>(solsys.c_str()); }
	const char *Context() const { return context.length() ? context.c_str() : 0; }
	const char *SplashScreen() const { return splashscreen.length() ? splashscreen.c_str() : 0; }
	DWORD SplashColor() const { return splashcolor; }
	const char *Script() const { return script.length() ? script.c_str() : 0; }
	const char *Focus() const { return focus.c_str(); }
	const char *ScnHelp() const { return (scnhelp.length() ? scnhelp.c_str() : 0); }
	const char *PlaybackDir() const { return (playback.length() ? playback.c_str() : scenario.c_str()); }
	void Update ();

	/// \brief Read state from scenario file
	bool Read (const char *fname);

	/// \brief Write state as scenario file
	/// \param ofs scenario file stream
	/// \param desc description string (0 for none)
	/// \param desc_fmt description format: 0 = text (DESC), 1 = inline html (HYPERDESC), 2 = external file (URLDESC)
	/// \param help scenario help url (html or chm,index page) (0 for none)
	void Write (std::ostream &ofs, const char *desc = 0, int desc_fmt = 0, const char *help = 0) const;
	// load/save scenario state

private:
	double mjd0;           // start time (MJD format)
	double mjd;            // current simulation time
	std::string scenario;    // scenario name
	std::string solsys;       // name of planetary system
	std::string context;      // scenario context
	std::string splashscreen;// scenario splash screen
	DWORD splashcolor;     // text color on splashscreen
	std::string script;      // scenario script
	std::string focus;        // current focus vessel
	std::string scnhelp;     // scenario help file
	std::string playback;    // playback folder name, if applicable

};

#endif // !__STATE_H