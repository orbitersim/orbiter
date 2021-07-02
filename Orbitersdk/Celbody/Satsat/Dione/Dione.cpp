#define ORBITER_MODULE
#include "Satsat.h"

// ======================================================================
// class Dione: interface
// ======================================================================

class Dione: public SATOBJ {
public:
	Dione (OBJHANDLE hObj);
};

// ======================================================================
// class Dione: implementation
// ======================================================================

Dione::Dione (OBJHANDLE hObj): SATOBJ (hObj, SAT_DIONE, 202.0)
{
	// sampling interval -> interpolation error <= 2.8m
}


// ======================================================================
// API interface
// ======================================================================

DLLCLBK CELBODY *InitInstance (OBJHANDLE hBody)
{
	return new Dione (hBody);
}

DLLCLBK void ExitInstance (CELBODY *body)
{
	delete (Dione*)body;
}
