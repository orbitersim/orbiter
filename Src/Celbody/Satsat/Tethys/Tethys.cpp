// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#define ORBITER_MODULE
#include "Satsat.h"

// ======================================================================
// class Tethys: interface
// ======================================================================

class Tethys: public SATOBJ {
public:
	Tethys (OBJHANDLE hObj);
};

// ======================================================================
// class Tethys: implementation
// ======================================================================

Tethys::Tethys (OBJHANDLE hObj): SATOBJ (hObj, SAT_TETHYS, 201.0)
{
	// sampling interval -> interpolation error <= 2.7m
}


// ======================================================================
// API interface
// ======================================================================

DLLCLBK CELBODY *InitInstance (OBJHANDLE hBody)
{
	return new Tethys (hBody);
}

DLLCLBK void ExitInstance (CELBODY *body)
{
	delete (Tethys*)body;
}
