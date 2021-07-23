// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#define ORBITER_MODULE
#include "Satsat.h"

// ======================================================================
// class Hyperion: interface
// ======================================================================

class Hyperion: public SATOBJ {
public:
	Hyperion (OBJHANDLE hObj);
};

// ======================================================================
// class Mimas: implementation
// ======================================================================

Hyperion::Hyperion (OBJHANDLE hObj): SATOBJ (hObj, SAT_HYPERION, 255.0)
{
	// sampling interval -> interpolation error <= 20m
}


// ======================================================================
// API interface
// ======================================================================

DLLCLBK CELBODY *InitInstance (OBJHANDLE hBody)
{
	return new Hyperion (hBody);
}

DLLCLBK void ExitInstance (CELBODY *body)
{
	delete (Hyperion*)body;
}
