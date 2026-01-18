// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#ifndef __EARTHATMJ71G_H
#define __EARTHATMJ71G_H

#include "OrbiterAPI.h"
#include "CelbodyAPI.h"

// ======================================================================
// class EarthAtmosphere_J71G
// Implementation of Jacchia71-Gill atmosphere model
// ======================================================================

class EarthAtmosphere_J71G: public ATMOSPHERE {
public:
	EarthAtmosphere_J71G (CELBODY2 *body);
	const char *clbkName () const;
	bool clbkConstants (ATMCONST *atmc) const;
	bool clbkParams (const PRM_IN *prm_in, PRM_OUT *prm);

private:
	OBJHANDLE hBody;   // handle for the associated celestial body

	// Time-dependent atmospheric parameters at last evaluation
	struct {
		double mjd;        // evaluation time
		double Slng, Slat; // sun's equatorial position in rotating Earth frame
	} atmprm;
};

#endif // !__EARTHATMJ71G_H