#define ORBITER_MODULE
#include "Galsat.h"

// ======================================================================
// class Ganymede: interface
// ======================================================================

class Ganymede: public GALOBJ {
public:
	Ganymede (OBJHANDLE hObj);
};

// ======================================================================
// class Callisto: implementation
// ======================================================================

Ganymede::Ganymede (OBJHANDLE hObj): GALOBJ (hObj)
{
	ksat = GAL_GANYMEDE;
	interval = 400.0;  // sampling interval [s]; position error: 2.4m
}


// ======================================================================
// API interface
// ======================================================================

DLLCLBK CELBODY *InitInstance (OBJHANDLE hBody)
{
	return new Ganymede (hBody);
}

DLLCLBK void ExitInstance (CELBODY *body)
{
	delete (Ganymede*)body;
}
