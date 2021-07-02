// ==============================================================
//                ORBITER MODULE: DeltaGlider
//                  Part of the ORBITER SDK
//          Copyright (C) 2001-2008 Martin Schweiger
//                   All rights reserved
//
// InstrAoa.h
// Angle-of-attack tape instrument for the Delta-Glider
// ==============================================================

#ifndef __INSTRAOA_H
#define __INSTRAOA_H

#include "..\Common\Instrument.h"

class InstrAOA: public PanelElement {
public:
	InstrAOA (VESSEL3 *v);
	~InstrAOA ();
	void Reset2D (int panelid, MESHHANDLE hMesh);
	void ResetVC (DEVMESHHANDLE hMesh);
	void LoadVC (int vcid);
	void LoadPanel2D (int panelid, PANELHANDLE hPanel, DWORD viewW, DWORD viewH);

	void AddMeshData2D (MESHHANDLE hMesh, DWORD grpidx);

	bool Redraw2D (SURFHANDLE surf);
	bool RedrawVC (DEVMESHHANDLE hMesh, SURFHANDLE surf);

private:
	void Redraw (NTVERTEX *vtx, NTVERTEX *vtxr);
	double paoa; // previous AOA value
	GROUPREQUESTSPEC vc_grp;         ///< Buffered VC vertex data (tape)
	GROUPREQUESTSPEC vc_grp_readout; ///< Buffered VC vertex data (readout)
	WORD vperm[8];
	WORD vperm_readout[16];
	float ycnt, disph; // display geometry
};

#endif // !__INSTRAOA_H