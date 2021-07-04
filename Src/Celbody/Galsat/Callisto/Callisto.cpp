#define ORBITER_MODULE
#include "Galsat.h"

// ======================================================================
// class Callisto: interface
// ======================================================================

class Callisto: public GALOBJ {
public:
	Callisto (OBJHANDLE hObj);
};

// ======================================================================
// class Callisto: implementation
// ======================================================================

Callisto::Callisto (OBJHANDLE hObj): GALOBJ (hObj)
{
	ksat = GAL_CALLISTO;
	interval = 300.0; // sampling interval [s]; position error: 2.9m
}


// ======================================================================
// API interface
// ======================================================================

DLLCLBK CELBODY *InitInstance (OBJHANDLE hBody)
{
	return new Callisto (hBody);
}

DLLCLBK void ExitInstance (CELBODY *body)
{
	delete (Callisto*)body;
}
