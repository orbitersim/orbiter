// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//                 ORBITER MODULE: ShuttleA
//                  Part of the ORBITER SDK
//
// sliderpair.h
// User interface: throttle slider pair
// ==============================================================

#ifndef __SLIDERPAIR_H
#define __SLIDERPAIR_H

#include "ShuttleA.h"
#include "..\Common\Instrument.h"

// ==============================================================

// Panel element: a pair of vertical sliders
class SliderPair: public PanelElement {
public:
	SliderPair (VESSEL3 *v, float basex, float basey, float rangey, int colidx);
	void AddMeshData2D (MESHHANDLE hMesh, DWORD grpidx);
	bool Redraw2D (SURFHANDLE surf, double level[2]);
	int ProcessMouse2D (int event, int mx, int my, double *level);

protected:
	void SetVertices (int which, double pos);
	float bx, by, ry;
	int cidx;
	double sliderlvl[2];
};

// ==============================================================

// Panel element: a pair of throttles
class ThrottlePair: public SliderPair {
public:
	ThrottlePair (VESSEL3 *v, float basex, float basey, float rangey, int colidx, THRUSTER_HANDLE *hthrust);
	bool Redraw2D (SURFHANDLE surf);
	bool ProcessMouse2D (int event, int mx, int my);

protected:
	THRUSTER_HANDLE *th;
};

// ==============================================================

// Panel element: main throttles
class ThrottleMain: public ThrottlePair {
public:
	ThrottleMain (VESSEL3 *v, THRUSTER_HANDLE *hthrust)
		: ThrottlePair (v, THROTTLE_X+104.5f, THROTTLE_Y+126.0f, 113.0f, 0, hthrust) {}
};

// ==============================================================

// Panel element: hover throttles
class ThrottleHover: public ThrottlePair {
public:
	ThrottleHover (VESSEL3 *v, THRUSTER_HANDLE *hthrust)
		: ThrottlePair (v, THROTTLE_X+176.5f, THROTTLE_Y+126.0f, 113.0f, 1, hthrust) {}
};

// ==============================================================

// Panel element: aux pod throttles
class ThrottlePod: public ThrottlePair {
public:
	ThrottlePod (VESSEL3 *v, THRUSTER_HANDLE *hthrust)
		: ThrottlePair (v, THROTTLE_X+32.5f, THROTTLE_Y+126.0f, 113.0f, 2, hthrust) {}
};

#endif // !__SLIDERPAIR_H