// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#define ORBITER_MODULE
#include "Satsat.h"

// ======================================================================
// class Titan: interface
// ======================================================================

class Titan: public SATOBJ {
public:
	Titan (OBJHANDLE hObj);
};

// ======================================================================
// class Titan: implementation
// ======================================================================

Titan::Titan (OBJHANDLE hObj): SATOBJ (hObj, SAT_TITAN, 349.0)
{
	// sampling interval -> interpolation error <= 10.6m
}


// ======================================================================
// API interface
// ======================================================================

DLLCLBK CELBODY *InitInstance (OBJHANDLE hBody)
{
	return new Titan (hBody);
}

DLLCLBK void ExitInstance (CELBODY *body)
{
	delete (Titan*)body;
}
