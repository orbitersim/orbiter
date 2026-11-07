// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//                ORBITER MODULE: DeltaGlider
//                  Part of the ORBITER SDK
//
// FuelMfd.h
// Fuel status display
// ==============================================================

#ifndef __FUELMFD_H
#define __FUELMFD_H

#include "../Common/Instrument.h"

class FuelMFD: public PanelElement {
public:
	FuelMFD (VESSEL3 *v);
	~FuelMFD ();
	void Reset2D (int panelid, MESHHANDLE hMesh);
	void ResetVC (DEVMESHHANDLE hMesh);
	bool Redraw2D (SURFHANDLE surf);
	bool RedrawVC (DEVMESHHANDLE hMesh, SURFHANDLE surf);

private:
	void Redraw (NTVERTEX *Vtx, SURFHANDLE surf, float crd[4]);
	void BltString (char *str, char *pstr, int maxlen, int x, int y, SURFHANDLE surf);

	bool isScram;
	double Tsample;
	double Mmain, Mrcs, Mscram;
	float crd_2D[4], crd_VC[4];
	char sout[9][5];
	int grpId;               ///< mesh group ID for fuel display
	GROUPREQUESTSPEC vc_grp; ///< Buffered VC vertex data
};

#endif // !__FUELMFD_H