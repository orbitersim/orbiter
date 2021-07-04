#define ORBITER_MODULE
#include "Satsat.h"

// ======================================================================
// class Enceladus: interface
// ======================================================================

class Enceladus: public SATOBJ {
public:
	Enceladus (OBJHANDLE hObj);
};

// ======================================================================
// class Enceladus: implementation
// ======================================================================

Enceladus::Enceladus (OBJHANDLE hObj): SATOBJ (hObj, SAT_ENCELADUS, 99.0)
{
	// sampling interval -> interpolation error <= 4.0m
}


// ======================================================================
// API interface
// ======================================================================

DLLCLBK CELBODY *InitInstance (OBJHANDLE hBody)
{
	return new Enceladus (hBody);
}

DLLCLBK void ExitInstance (CELBODY *body)
{
	delete (Enceladus*)body;
}
