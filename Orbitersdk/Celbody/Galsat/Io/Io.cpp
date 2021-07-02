#define ORBITER_MODULE
#include "Galsat.h"

// ======================================================================
// class Io: interface
// ======================================================================

class Io: public GALOBJ {
public:
	Io (OBJHANDLE hObj);
};

// ======================================================================
// class Io: implementation
// ======================================================================

Io::Io (OBJHANDLE hObj): GALOBJ (hObj)
{
	ksat = GAL_IO;
	interval = 100.0; // sampling interval [s]; position error: 3.7m
}


// ======================================================================
// API interface
// ======================================================================

DLLCLBK CELBODY *InitInstance (OBJHANDLE hBody)
{
	return new Io (hBody);
}

DLLCLBK void ExitInstance (CELBODY *body)
{
	delete (Io*)body;
}
