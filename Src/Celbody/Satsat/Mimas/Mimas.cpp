// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#define ORBITER_MODULE
#include "Satsat.h"

// ======================================================================
// class Mimas: interface
// ======================================================================

class Mimas: public SATOBJ {
public:
	Mimas (OBJHANDLE hObj);
};

// ======================================================================
// class Mimas: implementation
// ======================================================================

Mimas::Mimas (OBJHANDLE hObj): SATOBJ (hObj, SAT_MIMAS, 49.0)
{
	// sampling interval -> interpolation error <= 6.5m
}


// ======================================================================
// API interface
// ======================================================================

DLLCLBK CELBODY *InitInstance (OBJHANDLE hBody)
{
	return new Mimas (hBody);
}

DLLCLBK void ExitInstance (CELBODY *body)
{
	delete (Mimas*)body;
}
