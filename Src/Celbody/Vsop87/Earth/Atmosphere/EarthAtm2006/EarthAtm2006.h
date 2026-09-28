// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#ifndef __EARTHATM2006_H
#define __EARTHATM2006_H

#include "OrbiterAPI.h"
#ifndef __linux__
#include "CelbodyAPI.h"
#else // __linux__
#include "CelBodyAPI.h"
#endif // __linux__

// ======================================================================
// class EarthAtmosphere_2006
// Legacy atmospheric model
// ======================================================================

class EarthAtmosphere_2006: public ATMOSPHERE {
public:
	EarthAtmosphere_2006 (CELBODY2 *body): ATMOSPHERE (body) {}
	const char *clbkName () const;
	bool clbkConstants (ATMCONST *atmc) const;
	bool clbkParams (const PRM_IN *prm_in, PRM_OUT *prm);
};

#endif // !__EARTHATM2006_H
