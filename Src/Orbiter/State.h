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
	double mjd0;        // start time (MJD format)
	double mjd;         // current simulation time
	char scenario[256]; // scenario name
	char solsys[64];    // name of planetary system
	char context[64];   // scenario context
	char script[128];   // scenario script
	char focus[64];     // current focus vessel
	char scnhelp[128];  // scenario help file
	char playback[128]; // playback folder name, if applicable

};

#endif // !__STATE_H