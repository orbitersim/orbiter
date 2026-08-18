// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#ifndef __MARSATM2006_H
#define __MARSATM2006_H

#include "OrbiterAPI.h"
#include "CelbodyAPI.h"

// ======================================================================
// class MarsAtmosphere_2006
// Mars atmosphere model, as used in Orbiter 2006
// ======================================================================

class MarsAtmosphere_2006: public ATMOSPHERE {
public:
	MarsAtmosphere_2006 (CELBODY2 *body): ATMOSPHERE (body) {}
	const char *clbkName () const;
	bool clbkConstants (ATMCONST *atmc) const;
	bool clbkParams (const PRM_IN *prm_in, PRM_OUT *prm);
};

#endif // !__MARSATM2006_H