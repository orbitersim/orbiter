// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//                ORBITER MODULE: DeltaGlider
//                  Part of the ORBITER SDK
//
// InstrHsi.h
// HSI (Horizonal situation indicator) for the Delta-Glider
// ==============================================================

#ifndef __INSTRHSI_H
#define __INSTRHSI_H

#include "..\Common\Instrument.h"

// ==============================================================

class InstrHSI: public PanelElement {
public:
	InstrHSI (VESSEL3 *v);
	~InstrHSI ();

	/**
	 * \brief Set up parameters for 2D mesh vertex access
	 * \param hMesh mesh handle for 2D main panel
	 */
	void Reset2D (int panelid, MESHHANDLE hMesh);
	void ResetVC (DEVMESHHANDLE hMesh);
	void SetCrs (double newcrs);
	double GetCrs () const;
	bool Redraw2D (SURFHANDLE surf);
	bool RedrawVC (DEVMESHHANDLE hMesh, SURFHANDLE surf);

protected:
	void Redraw (NTVERTEX *Vtx);

private:
	void Orthodome (double lng1, double lat1, double lng2, double lat2,
		double &dist, double &dir);

	double crs;
	double dev;
	double gslope;
	NAVHANDLE nav;
	OBJHANDLE navRef;
	DWORD navType;
	double navlng, navlat;
	GROUPREQUESTSPEC vc_grp; ///< Buffered VC vertex data
};

#endif // !__INSTRHSI_H