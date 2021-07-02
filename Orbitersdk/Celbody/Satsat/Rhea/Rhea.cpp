#define ORBITER_MODULE
#include "Satsat.h"

// ======================================================================
// class Rhea: interface
// ======================================================================

class Rhea: public SATOBJ {
public:
	Rhea (OBJHANDLE hObj);
};

// ======================================================================
// class Rhea: implementation
// ======================================================================

Rhea::Rhea (OBJHANDLE hObj): SATOBJ (hObj, SAT_RHEA, 391.0)
{
	// sampling interval -> interpolation error <= 3.3m
}


// ======================================================================
// API interface
// ======================================================================

DLLCLBK CELBODY *InitInstance (OBJHANDLE hBody)
{
	return new Rhea (hBody);
}

DLLCLBK void ExitInstance (CELBODY *body)
{
	delete (Rhea*)body;
}
