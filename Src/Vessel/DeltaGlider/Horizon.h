// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//                ORBITER MODULE: DeltaGlider
//                  Part of the ORBITER SDK
//
// Horizon.h
// Artificial horizon instrument (attitude indicator) for the
// Delta-Glider
// ==============================================================

#ifndef __HORIZON_H
#define __HORIZON_H

#include "../Common/Instrument.h"

// ==============================================================

class InstrAtt: public PanelElement {
public:
	InstrAtt (VESSEL3 *v);
	~InstrAtt ();

	/**
	 * \brief Set up parameters for 2D mesh vertex access
	 * \param panelid ID of currently active panel
	 * \param hMesh mesh handle for 2D main panel
	 */
	void Reset2D (int panelid, MESHHANDLE hMesh);

	/**
	 * \brief Set up parameters for VC mesh vertex access
	 * \param hMesh mesh handle for VC mesh
	 */
	void ResetVC (DEVMESHHANDLE hMesh);

	/**
	 * \brief Update instrument display (2D panel)
	 * \param surf texture used by the instrument
	 * \return \e false to indicate that the texture was not modified
	 */
	bool Redraw2D (SURFHANDLE surf);

	/**
	 * \brief Update instrument display (virtual cockpit)
	 * \param hMesh VC mesh handle
	 * \param surf texture used by the instrument
	 * \return \e true to indicate that the texture was modified
	 */
	bool RedrawVC (DEVMESHHANDLE hMesh, SURFHANDLE surf);

protected:
	/**
	 * \brief Common redraw function for VC and 2D panel
	 * \param Vtx vertex buffer to edit
	 */
	void Redraw (NTVERTEX *Vtx);

private:
	GROUPREQUESTSPEC vc_grp; ///< Buffered VC vertex data
};

// ==============================================================

#endif // !__HORIZON_H