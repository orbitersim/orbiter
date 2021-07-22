// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// Star - logical interface

#ifndef __STAR_H
#define __STAR_H

#include "Celbody.h"

class PlanetarySystem;

class Star: public CelestialBody {
	friend class VStar;

public:
	Star (double _mass, double _mean_radius);

	Star (char *fname);
	// create a star from a config file

	~Star ();

	void Setup ();

	int Type() const { return OBJTP_STAR; }

	void SetPsys (const PlanetarySystem *_psys)
	{ psys = _psys; }

	const PlanetarySystem *GetPsys () const
	{ return psys; }

	void Update (bool force = false);
	// Perform time step

	Vector Pos2Barycentre (Vector &pos);

	D3DCOLORVALUE GetLightColor();

	void InitDeviceObjects ();
	void DestroyDeviceObjects ();

//protected:
//	bool CheckVisibility (double &tnext);

private:
	const PlanetarySystem *psys;  // system the star belongs to
	double upd_t;                 // system time of next update
	LPDIRECTDRAWSURFACE7 tex;     // billboard star texture
};

#endif // !__STAR_H