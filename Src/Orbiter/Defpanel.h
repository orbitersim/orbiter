// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// =======================================================================
// class DefaultPanel
// Generic cockpit view with 2 MFD displays and a HUD
// This is supported by all vessels.
// =======================================================================

#ifndef __DEFPANEL_H
#define __DEFPANEL_H

#define STRICT 1
#include "Orbiter.h"
#include "Mfd.h"

// =======================================================================
// class DefaultPanel

class DefaultPanel {
	friend class Pane;

public:
	DefaultPanel (Pane *_pane, int cidx);
	~DefaultPanel ();

	// Restore all devices (e.g. after render window is re-openend
	void RestoreDeviceObjects (LPDIRECT3D7 d3d, LPDIRECT3DDEVICE7 dev);

	// Render the glass cockpit overlay on top of the target surface
	void Render ();

	bool ProcessMouse (UINT event, DWORD state, int x, int y);
	void GetButtonState (int &state, int &mfd, int &btn);
	inline void SetMouseState (int state) { mstate = state; }
	inline void SetNavDisplayMode (int mode) { navdispmode = mode; }
	inline void SetRCSDisplayMode (int mode) { rcsdispmode = mode; }
	void MFDModeChanged(int mfd, int mode);
	void RepaintMFDButtons (int id);
	Instrument::Spec GetMFDSpec () const;

	/**
     * \brief Called when the user interactively changes a simulation option
     * \param cat option category (see \ref optcat)
     * \param item option item (see \ref optitem)
     */
	virtual void OptionChanged(DWORD cat, DWORD item);

private:
	void SetGeometry ();

	/**
     * \brief Reset mesh and display settings
     */
	void ResetGeometry();

	void InitDeviceObjects ();
	void DestroyDeviceObjects ();
	bool GetMFDButton (int mx, int my, int &mfd, int &btn) const;
	bool GetNavButton (int mx, int my, int &btn) const;
	bool GetRCSButton (int mx, int my, int &btn) const;
	bool GetTrimButton (int mx, int my, int &btn) const;
	void PressMFDButton (int mfd, int btn);
	void PressNavButton (int btn);
	void PressRCSButton (int btn);
	void PressTrimButton (int btn);
	void ActivateMFDButton (int mfd, int btn, bool active);
	void SwitchColour (int idx);
	SURFHANDLE LoadTexture (int idx = 0);

	Pane *pane;
	oapi::GraphicsClient *gc;            // render client
	SURFHANDLE shRenderTarget;           // render target surface
	SURFHANDLE surf;                     // surface for panel element textures
	Mesh mesh;                           // mesh for rendering panel elements
	float scale;                         // mesh to pixel transformation
	int viewW, viewH;                    // viewport dimensions [pixel]
	int fw, fh;                          // font width/height units
	int enggrp, navgrp;                  // mesh groups for engine info block/nav buttons
	int mfdx[2], mfdy, mfdw, mfdh, gapw; // MFD geometry parameters
	int btnw, btnh;                      // MFD button geometry parameters
	float blockdx;                       // engine info block offset from left
	float blockdy;                       // engine info block offset from top
	float btnx[2][2], btny[6];
	float bbtnx[2][3], bbtny;            // MFD bottom button geometry parameters
	int /*nvbw, nvbh,*/ nvby;                // navmode button geometry
	bool nvbstate[7];                    // navbutton states
	int nnvb;                            // number of used navmode buttons
	int rcsw, rcsh, rcsx, rcsy;          // RCS indicator geometry
	int colidx;                          // HUD colour index
	int mstate;                          // mouse state
	int activemfd, activebtn;            // mouse-selected MFD and button
	int navdispmode;                     // 0=don't show, 1=show, 2=show active only
	int rcsmode;                         // current RCS mode
	int rcsdispmode;                     // 0=don't show, 1=show
	bool transpmfd;                      // transparent mfds?
	bool compact_layout;                 // MFD layout compact or wide? (for widescreen formats)
	oapi::Font *mfdFont;                 // font for MFD buttons
	oapi::Pen *mfdPen;                   // pen for MFD button borders
	double fuel;                         // current fuel display state
	double elevtrim;                     // current elevator trim state
	double engmain;                      // current main engine display state
	double enghovr;                      // current hover engine display state
	char engstat_str[4][8];              // status strings for engine display block
};

#endif // !__DEFPANEL_H