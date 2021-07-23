// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//                ORBITER MODULE: DeltaGlider
//                  Part of the ORBITER SDK
//
// DGSwitches.h
// Prototypes for DG-specific cockpit switches and dials
// ==============================================================

#ifndef __DGSWITCHES_H
#define __DGSWITCHES_H

#include "..\Common\Instrument.h"

// ==============================================================
// Flip switch with two or three discrete states:
// up-down (TWOSTATE) or up-down-centered (THREESTATE)
// SPRING automatically returns to center position when released
// SPRING switches must be defined with PANEL_MOUSE_LBDOWN|PANEL_MOUSE_LBUP flag
// otherwise PANEL_MOUSE_LBDOWN is enough

class DGSwitch1: public PanelElement {
public:
	enum Mode { TWOSTATE, THREESTATE, SPRING };
	enum State { CENTER, UP, DOWN };
	DGSwitch1 (VESSEL3 *v, Mode m=TWOSTATE);
	void DefineAnimationVC (const VECTOR3 &ref, const VECTOR3 &axis,
		DWORD meshgrp, int vtxofs);
	void DefineAnimation2D (MESHHANDLE hMesh, DWORD meshgrp, int vtxofs);
	void ResetVC (DEVMESHHANDLE hMesh);
	bool ProcessMouseVC (int event, VECTOR3 &p);
	bool ProcessMouse2D (int event, int mx, int my);
	bool RedrawVC (DEVMESHHANDLE hMesh, SURFHANDLE surf);
	bool Redraw2D (SURFHANDLE surf);

	inline State GetState() const { return state; }
	bool SetState (State s);
	State Up (); // move to next state up and return new state
	State Down (); // move to next state down and return new state

private:
	Mode mode;             // switch mode
	State state;           // logical state
	State vstate, vstate2; // current visual state for VC and 2D panel
	VECTOR3 rf, ax;        // rotation reference point, axis
	DWORD mgrp;            // group index
	int vofs;              // vertex offset
	const static int nvtx; // number of vertices in switch lever
	static double travel;  // switch lever angle from center to up position [rad]
	const static float tu0[3], tv0, tw, th;
};

// ==============================================================
// Rocker switch that can be pressed up or down, and returns
// to neutral

class DGSwitch2: public PanelElement {
public:
	enum State { CENTER, UP, DOWN };
	enum Orientation { VERT, HORZ, HORZ_RL };
	DGSwitch2 (VESSEL3 *v);
	void DefineAnimation2D (Orientation o, DWORD meshgrp, DWORD vofs);
	void DefineAnimationVC (const VECTOR3 &ref, const VECTOR3 &axis,
		DWORD meshgrp, DWORD vofs);
	void Reset2D (int panelid, MESHHANDLE hMesh);
	bool ProcessMouse2D (int event, int mx, int my);
	bool ProcessMouseVC (int event, VECTOR3 &p);
	bool Redraw2D (SURFHANDLE surf);
	bool RedrawVC (DEVMESHHANDLE hMesh, SURFHANDLE surf);

	inline State GetState() const { return state; }
	bool SetState (State s);

private:
	Orientation orient; // layout
	State state;        // logical state
	State vstate;       // current visual state
	VECTOR3 rf, ax;     // rotation reference point, axis
	DWORD mgrp;         // group index
	const static int nvtx; // number of vertices in switch lever
	static double travel;  // switch lever angle from center to up position [rad]
};

// ==============================================================
// Dial with discrete, equidistant positions

class DGDial1: public PanelElement {
public:
	DGDial1 (VESSEL3 *v, int np, double pos0, double delta);
	void DefineAnimationVC (const VECTOR3 &ref, const VECTOR3 &axis,
		DWORD meshgrp, int vtxofs);
	bool ProcessMouseVC (int event, VECTOR3 &p);
	void ResetVC (DEVMESHHANDLE hMesh);
	bool RedrawVC (DEVMESHHANDLE hMesh, SURFHANDLE surf);

	inline int GetPosition() const { return pos; }
	bool SetPosition (int newpos);
	int Left(); // move to next position left and return new position
	int Right(); // move to next position right and return new position

private:
	int pos;     // current logical position (0..np-1; 0=leftmost)
	int vpos;    // current visual position
	int npos;    // number of positions
	double p0;   // angle of first position (from 12o'clock) [rad]
	double dp;   // angle between positions [rad]
	VECTOR3 rf, ax;    // rotation reference point, axis
	DWORD mgrp;        // group index
	int vofs;          // vertex offset
	const static int nvtx;   // number of vertices in switch lever
};

// ==============================================================
// Simple square pushable button

class DGButton2: public PanelElement {
public:
	enum State { OFF, ON };
	DGButton2 (VESSEL3 *v);
	void DefineAnimationVC (const VECTOR3 &axis, DWORD meshgrp, DWORD vofs);
	bool ProcessMouseVC (int event, VECTOR3 &p);
	bool RedrawVC (DEVMESHHANDLE hMesh, SURFHANDLE surf);
	inline State GetState() const { return state; }
	void SetState (State newstate);

private:
	DWORD mgrp;              // group index for button
	const static int nvtx;   // number of vertices in button
	VECTOR3 ax;              // push down direction
	State state, vstate;     // logical, visual button state
};

// ==============================================================
// Push button with illuminated label

class DGButton3: public PanelElement {
public:
	enum State { OFF, ON, PRESSED_FROM_OFF, PRESSED_FROM_ON };
	DGButton3 (VESSEL3 *v);
	void DefineAnimation2D (DWORD meshgrp, DWORD vofs);
	void DefineAnimationVC (const VECTOR3 &axis, DWORD meshgrp, DWORD meshgrp_label,
		DWORD vofs, DWORD vofs_label);
	void Reset2D (int panelid, MESHHANDLE hMesh);
	void ResetVC (DEVMESHHANDLE hMesh);
	bool ProcessMouse2D (int event, int mx, int my);
	bool ProcessMouseVC (int event, VECTOR3 &p);
	bool RedrawVC (DEVMESHHANDLE hMesh, SURFHANDLE surf);
	inline State GetState() const { return state; }
	void SetState (State newstate);

protected:
	State state, vstate;     // logical, visual button state

private:
	DWORD mgrp;              // group index for button
	DWORD mgrp_lbl;          // group index for label
	DWORD vtxofs_lbl;        // vertex offset for label
	VECTOR3 ax;              // push down direction
	const static int nvtx;   // number of vertices in button
	const static int nvtx_lbl; // number of vertices in label
};

#endif // !__DGSWITCHES_H