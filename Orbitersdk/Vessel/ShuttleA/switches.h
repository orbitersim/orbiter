// ==============================================================
//                 ORBITER MODULE: ShuttleA
//                  Part of the ORBITER SDK
//          Copyright (C) 2001-2016 Martin Schweiger
//                   All rights reserved
//
// switches.h
// Panel switch templates
// ==============================================================

#ifndef __SWITCHES_H
#define __SWITCHES_H

#include "ShuttleA.h"
#include "..\Common\Instrument.h"

// ==============================================================
// Switch type 1 (red vertical switch with 2 or 3 positions)

class PanelSwitch1: public PanelElement {
public:
	PanelSwitch1 (ShuttleA *v, bool threestate, MESHHANDLE hMesh, int meshgrp, int vofs);
	PanelSwitch1 (ShuttleA *v, float tgt_x0, float tgt_y0, bool threestate); // obsolete
	void AddMeshData2D (MESHHANDLE hMesh, DWORD grpidx); // obsolete
	void Reset2D (int panelid);
	bool Redraw2D (SURFHANDLE surf);
	bool ProcessMouse2D (int event, int mx, int my);
	virtual int GetTargetState() = 0;  // 0=bottom, 1=middle, 2=top
	virtual void SetTargetState (int state) {}

protected:
	ShuttleA *sh;
	int vstate;   // current visual state

private:
	float x0, y0; // target mesh position (upper left corner)
	bool enablecenter;

	static const float tx_x0, tx_y0; // bitmap position in source texture
	static const float tx_dx, tx_dy; // bitmap size in source texture
};


// ==============================================================
// Indicator type 1 (status display typically shown below a switch)

class PanelIndicator1: public PanelElement {
public:
	PanelIndicator1 (ShuttleA *v, MESHHANDLE hMesh, int meshgrp, int vofs);
	PanelIndicator1 (ShuttleA *v, float tgt_x0, float tgt_y0); // obsolete
	void AddMeshData2D (MESHHANDLE hMesh, DWORD grpidx); // obsolete
	void Reset2D (int panelid);
	bool Redraw2D (SURFHANDLE surf);
	virtual int GetTargetState() = 0;  // label index (>=0, 0=bottom label)

protected:
	ShuttleA *sh;
	int vstate;   // current visual state

private:
	float x0, y0; // target mesh position (upper left corner)

	static const float tx_x0, tx_y0; // bitmap position in source texture
	static const float tx_dx, tx_dy; // bitmap size in source texture
	static const float ystep;        // y offset between labels
};

#endif // !__SWITCHES_H