// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// =======================================================================
// class Nav
// Navigation signal transmitter
// =======================================================================

#ifndef __NAV_H
#define __NAV_H

#include <windows.h>
#include <fstream>
#include "Vessel.h"

#define NAV_RADIO_FREQ_MIN 108.0
#define NAV_RADIO_FREQ_MAX 140.0
#define NAV_RADIO_NSTEP    640

class Planet;
class Base;

// =======================================================================
// class Nav

class Nav {
	friend class NavManager;
public:
	Nav (float _freq = 100.0, float _range = 500e3);
	virtual DWORD Type() const { return TRANSMITTER_NONE; }
	void SetFreq (float _freq);
	void SetStep (DWORD _step);
	inline float GetFreq() const { return freq; }
	inline DWORD GetStep() const { return step; }
	inline void SetRange (float _range) { range = _range; }
	inline float GetRange() const { return range; }
	inline const char *GetId() const { return id; }
	virtual void GetData (NAVDATA *data) const;
	virtual int IdString (char *str, int len) const;
	virtual void GPos (Vector &gp) const = 0;
	double Dist (const Vector &gpos) const;
	double FieldStrength (const Vector &gpos) const;
	bool InRange (const Vector &gpos) const;

protected:
	DWORD step;
	float freq;
	float range;
	char id[8];
};

// =======================================================================
// class Nav_VOR
// surface-based omnidirectional transmitter

class Nav_VOR: public Nav {
public:
	Nav_VOR (const Planet *_planet) { planet = _planet; lng = lat = 0.0; }
	Nav_VOR (const Planet *_planet, double _lng, double _lat);
	Nav_VOR (const Planet *_planet, double _lng, double _lat, float _freq, float _range = 500e3);
	Nav_VOR (const Planet *_planet, const char *str);
	virtual DWORD Type() const { return TRANSMITTER_VOR; }
	virtual int IdString (char *str, int len) const;
	inline const Planet *GetPlanet() const { return planet; }
	inline void GetEquPos (double &_lng, double &_lat) const { _lng = lng, _lat = lat; }
	inline void LPos (Vector &lp) const { lp = lpos; }
	void GPos (Vector &gp) const;
	void GetData (NAVDATA *data) const;

protected:
	const Planet *planet;
	double lng, lat;
	Vector lpos;
};

// =======================================================================
// class Nav_VTOL
// transmitter for vertical takeoff/landing support

class Nav_VTOL: public Nav_VOR {
public:
	Nav_VTOL (const Base *_base, int _npad, double _lng, double _lat, float _freq, float _range = 30e3);
	inline DWORD Type () const { return TRANSMITTER_VTOL; }
	int IdString (char *str, int len) const;
	inline int GetPad () const { return npad; }
	inline const Base *GetBase () const { return base; }
	void GetData (NAVDATA *data) const;

private:
	const Base *base;
	int npad;
};

// =======================================================================
// class Nav_ILS
// Instrument landing system: runway instrument landing

class Nav_ILS: public Nav_VOR {
public:
	Nav_ILS (const Base *_base, double _dir, double _lng, double _lat, float _freq, float _range = 30e3);
	inline DWORD Type () const { return TRANSMITTER_ILS; }
	int IdString (char *str, int len) const;
	inline double ApprDir () const { return dir; }
	inline const Base *GetBase () const { return base; }
	void GetData (NAVDATA *data) const;

private:
	const Base *base;
	double dir;  // approach direction
};

// =======================================================================
// class Nav_IDS
// Instrument docking system: vessel-mounted transmitter for docking approach

class Nav_IDS: public Nav {
public:
	Nav_IDS (const Vessel *_vessel, const PortSpec *_ps, float _freq, float _range = 2e4);
	inline DWORD Type () const { return TRANSMITTER_IDS; }
	int IdString (char *str, int len) const;
	void GPos (Vector &gp) const;
	inline const Vessel *GetVessel () const { return vessel; }
	inline const PortSpec *GetPortSpec () const { return ps; }
	void GetData (NAVDATA *data) const;

private:
	const Vessel *vessel;
	const PortSpec *ps;
};

// =======================================================================
// class Nav_XPDR
// Transponder (vessel-mounted)

class Nav_XPDR: public Nav {
public:
	Nav_XPDR (const Vessel *_vessel, float _freq, float _range = 1e6);
	inline DWORD Type () const { return TRANSMITTER_XPDR; }
	int IdString (char *str, int len) const;
	inline void GPos (Vector &gp) const { gp.Set (vessel->GPos()); }
	inline const Vessel *GetVessel () const { return vessel; }
	void GetData (NAVDATA *data) const;

private:
	const Vessel *vessel;
};

// =======================================================================
// class NavManager
// maintains a list of transmitters

class NavManager {
public:
	NavManager ();
	~NavManager ();
	void Clear ();
	void AddNav (Nav *_nav);
	DWORD nNav() const { return nnav; }
	inline const Nav *GetNav (DWORD i) const { return nav[i]; }
	inline Nav **GetNavlist () { return nav; }
	DWORD Read (std::ifstream &ifs, const Planet *planet = NULL, bool append = false);
	
private:
	DWORD nnav;   // number of transmitters
	DWORD nbuf;   // buffer length
	Nav **nav;    // list of transmitters
};

// =======================================================================
// standalone methods

Nav *ParseNav (const char *line, const Planet *planet);

#endif // !__NAV_H