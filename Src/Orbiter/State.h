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

class State {
public:
	State();

	double Mjd() const { return mjd; }
	char *Solsys() { return solsys; }
	const char *Context() const { return context[0] ? context : 0; }
	const char *Script() const { return script[0] ? script : 0; }
	const char *Focus() const { return focus; }
	const char *ScnHelp() const { return (scnhelp[0] ? scnhelp : 0); }
	const char *PlaybackDir() const { return (playback[0] ? playback : scenario); }
	void Update (const char *_desc = 0);
	bool Read (const char *fname);
	void Write (std::ostream &ofs, const char *help) const;
	// load/save scenario state

private:
	double mjd0;        // start time (MJD format)
	double mjd;         // current simulation time
	char scenario[256]; // scenario name
	char solsys[64];    // name of planetary system
	char context[64];   // scenario context
	char script[128];   // scenario script
	char focus[64];     // current focus vessel
	char scnhelp[128];  // scenario help file
	char playback[128]; // playback folder name, if applicable
	const char *desc;

};

#endif // !__STATE_H