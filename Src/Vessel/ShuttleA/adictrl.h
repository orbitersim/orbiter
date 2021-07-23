// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//                 ORBITER MODULE: ShuttleA
//                  Part of the ORBITER SDK
//
// adictrl.h
// User interface for attitude reference selector
// ==============================================================

#ifndef __ADICTRL_H
#define __ADICTRL_H

#include "..\Common\Instrument.h"

// ==============================================================

class ADICtrl: public PanelElement {
	friend class ShuttleA;

public:
	ADICtrl (ShuttleA *shuttlea);
	void AddMeshData2D (MESHHANDLE hMesh, DWORD grpidx, DWORD grpidx_disp);
	bool Redraw2D (SURFHANDLE surf);
	bool ProcessMouse2D (int event, int mx, int my);

protected:
	bool ProcessDials (int event, int mx, int my);
	bool ProcessSwitches (int event, int mx, int my);
	void UpdateDisplay (SURFHANDLE surf, bool force = false);
	void DispAngle (SURFHANDLE surf, double angle, int x, int y, char curstr[3]);

private:
	ShuttleA *sh;
	int btstate[3];
	int btactive;
	bool settgt;
	bool errmode_is_local;
	int refmode;
	int tgtmode;
	MESHGROUP *ctrlgrp, *dispgrp;
	DWORD ctrlofs, dispofs;

	struct {
		int frmmode;
		int tgtmode;
		OBJHANDLE frmref;
		NAVHANDLE navref;
		char frmofs[3][3];
		char frmdev[3][3];
		char tgtofs[2][3];
		char tgtdev[2][3];
	} dispprm;
};

#endif // !__ADICTRL_H
