// ==================================================================
// Copyright (c) 2021 Jarmo Nikkanen
// Licensed under the MIT License
// ==================================================================

#ifndef __ORBIT_ReferenceClass_H
#define __ORBIT_ReferenceClass_H

#include "OrbiterAPI.h"

class ReferenceClass {
	
public:

	ReferenceClass();
    ~ReferenceClass();

	OBJHANDLE   FindStar();
	OBJHANDLE   Get2ndReferenceForShip();
	OBJHANDLE	GetReference(OBJHANDLE);
	OBJHANDLE	GetReferenceByName(char *);
	double		GetSOI(OBJHANDLE obj);
	OBJHANDLE * GetSystemList(OBJHANDLE x);
	int			GetSystemCount(OBJHANDLE x);
    bool		IsGbody(OBJHANDLE x);

	OBJHANDLE	StarHandle;

private:

	OBJHANDLE	FindGravityReference(OBJHANDLE);
	void		CreateDatabase();

	struct ReferenceClass_info {	
		OBJHANDLE	handle;
		OBJHANDLE	grf_handle;
		double		soi;
		double		dist;  // Distance where this ReferenceClass is active
		OBJHANDLE   system[256];
		int         sys_index;
	};

	int total_count;
	struct ReferenceClass_info *References;
	
};

extern class ReferenceClass *Refer;

#endif

