#define ORBITER_MODULE
#include "Galsat.h"

// ======================================================================
// class Europa: interface
// ======================================================================

class Europa: public GALOBJ {
public:
	Europa (OBJHANDLE hObj);
};

// ======================================================================
// class Europa: implementation
// ======================================================================

Europa::Europa (OBJHANDLE hObj): GALOBJ (hObj)
{
	ksat = GAL_EUROPA;
	interval = 100.0;  // sampling interval [s]; position error: 3.6m
}


// ======================================================================
// API interface
// ======================================================================

DLLCLBK CELBODY *InitInstance (OBJHANDLE hBody)
{
	return new Europa (hBody);
}

DLLCLBK void ExitInstance (CELBODY *body)
{
	delete (Europa*)body;
}
