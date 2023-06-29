// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// Star - logical interface

#ifdef INLINEGRAPHICS
#include <d3dtypes.h>
#endif

#ifndef __STAR_H
#define __STAR_H

#include "Celbody.h"

class Star: public CelestialBody {
	friend class VStar;

public:
	Star (double _mass, double _mean_radius);

	Star (char *fname);
	// create a star from a config file

	~Star ();

	void Setup ();

	int Type() const { return OBJTP_STAR; }

	void Update (bool force = false);
	// Perform time step

	Vector Pos2Barycentre (Vector &pos);

	auto GetLightColor() { return VECTOR4{1, 1, 1, 1}; }

#ifdef INLINEGRAPHICS
	void InitDeviceObjects ();
	void DestroyDeviceObjects ();

private:
	LPDIRECTDRAWSURFACE7 tex;     // billboard star texture
#endif //INLINEGRAPHICS
};

#endif // !__STAR_H