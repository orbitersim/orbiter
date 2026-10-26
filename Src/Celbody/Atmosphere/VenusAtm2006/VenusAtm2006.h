// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#ifndef __VENUSATM2006_H
#define __VENUSATM2006_H

#include "OrbiterAPI.h"
#include "CelbodyAPI.h"

// ======================================================================
// class VenusAtmosphere_2006
// Venus atmosphere model, as used in Orbiter 2006
// ======================================================================

class VenusAtmosphere_2006: public ATMOSPHERE {
public:
	VenusAtmosphere_2006 (CELBODY2 *body): ATMOSPHERE (body) {}
	const char *clbkName () const;
	bool clbkConstants (ATMCONST *atmc) const;
	bool clbkParams (const PRM_IN *prm_in, PRM_OUT *prm);
};

#endif // !__VENUSATM2006_H