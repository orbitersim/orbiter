#define ORBITER_MODULE
#include "Satsat.h"

// ======================================================================
// class Iapetus: interface
// ======================================================================

class Iapetus: public SATOBJ {
public:
	Iapetus (OBJHANDLE hObj);
};

// ======================================================================
// class Iapetus: implementation
// ======================================================================

Iapetus::Iapetus (OBJHANDLE hObj): SATOBJ (hObj, SAT_IAPETUS, 721.0)
{
	// sampling interval -> interpolation error <= 5.1m
}


// ======================================================================
// API interface
// ======================================================================

DLLCLBK CELBODY *InitInstance (OBJHANDLE hBody)
{
	return new Iapetus (hBody);
}

DLLCLBK void ExitInstance (CELBODY *body)
{
	delete (Iapetus*)body;
}
