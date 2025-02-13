// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// =======================================================================
// class Panel2D
// Vessel cockpit represented by 2D instrument panels
//
// This replaces the 'Panel' class. It represents the panels with 
// textured 3D billboards instead of bitmap overlays
// =======================================================================

#ifndef __PANEL2D_H
#define __PANEL2D_H

#define STRICT 1
#include <windows.h>
#include "Mfd.h"

// =======================================================================
// class Panel2D

/**
 * \brief Panel2D represents a vessel instrument panel in cockpit view.
 *
 * A vessel may define one or more panels to allow user interaction
 * (instrument displays, MFDs, buttons, etc.)
 * It reacts to mouse clicks and redraw requests.
 */
class Panel2D {
	friend class Pane;

public:
	Panel2D (int _id, Pane *_pane, double _scale = 1.0);
	~Panel2D ();

	/**
	 * \brief Panel setup routines.
	 */
	void Setup ();

	/**
	 * \brief Returns the panel identifier.
	 * \return panel id (>= 0)
	 * \sa Connect
	 */
	inline int GetId () const { return id; }

	/**
	 * \brief define panel neighbours in four directions.
	 * \param left left panel id  (>=0, -1=none)
	 * \param right right panel id  (>=0, -1=none)
	 * \param top top panel id  (>=0, -1=none)
	 * \param bottom bottom panel id  (>=0, -1=none)
	 */
	void SetConnections (int left, int right, int top, int bottom);

	/**
	 * \brief Return Id of connecting panel in a given direction
	 * \param dir connection direction
	 * \return Id of neighbour panel, or -1 if no neighbour in this direction
	 */
	inline int Connect (int dir) const { return connect[dir]; }

	/**
	 * \brief Defines the surface texture for the panel background.
	 * \param hSurface array of surface handles
	 * \param nsurf number of surfaces
	 * \param hMesh mesh handle for billboard geometry
	 * \param width panel width [pixel]
	 * \param height panel height [pixel]
	 * \param scrollflag attachment and scrolling flags
	 * \return Always 0.
	 */
	int SetBackground (SURFHANDLE *hSurface, DWORD nsurf, MESHHANDLE hMesh, DWORD width, DWORD height, DWORD baseline, DWORD scrollflag);

	/**
	 * \brief Set panel scaling factors
	 * \param scale1 default scale factor
	 * \param scale2 additional scale factor
	 * \return Always returns 0.
	 * \note The scaling factors define the scaling between mesh coordinates
	 *   and screen pixels.
	 * \note \e scale1 is the default factor, \e scale2 is an additional scale
	 *   which can be selected by the user via the mouse wheel.
	 * \note Examples: scale=1: one mesh unit corresponds to one screen pixel,
	 *   scale=viewW/panelW: panel fits screen width
	 */
	int SetScaling (double scale1, double scale2);

	/**
	 * \brief Return current panel scaling factor
	 */
	double GetActiveScale() const { return panelscale; }

	/**
	 * \brief Define an MFD display in the panel mesh.
	 * \param MFD_id MFD identifier (>= 0)
	 * \param nmesh panel mesh index (>= 0)
	 * \param ngroup mesh group index (>= 0)
	 * \return Always returns 0.
	 */
	int RegisterMFDGeometry (int MFD_id, int nmesh, int ngroup);

	/**
	 * \brief Returns the geometry parameters for an MFD.
	 * \param MFD_id MFD identifier (>= 0)
	 * \return MFD display and button parameters
	 */
	Instrument::Spec GetMFDSpec (int MFD_id) const;

	/**
	 * \brief Panel scrolling: move panel by specified amount.
	 * \param dx horizontal panning length [pixel]
	 * \param dy vertical panning length [pixel]
	 */
	void Move (double dx, double dy);

	/**
	 * \brief Render the panel.
	 */
	void Render ();

	/**
	* \brief Process a mouse event for the panel.
	* \param event event type (see \ref panel_mouse)
	* \param state mouse button state
	* \param x mouse screen x position
	* \param y mouse screen y position
	* \return \e true if the panel processes the event.
	*/
	bool ProcessMouse_System(UINT event, DWORD state, int x, int y, const char *kstate);

	/**
	 * \brief Process a mouse event for the panel while the simulation is active.
	 * \param event event type (see \ref panel_mouse)
	 * \param state mouse button state
	 * \param x mouse screen x position
	 * \param y mouse screen y position
	 * \return \e true if the panel processes the event.
	 */
	bool ProcessMouse_OnRunning (UINT event, DWORD state, int x, int y, const char *kstate);

	void GetMouseState (int &idx, int &state, int &mx, int &my) const;

	inline void SetMouseState (int state) { mstate = state; }

	/**
	 * \brief Perform frame update actions.
	 * \param SimT simulation time
	 */
	void Update (double SimT, double SysT);

	int DefineArea (int aid, const RECT &pos, int draw_mode, int mouse_mode, SURFHANDLE surf = NULL, void *context = NULL);
	int DefineArea (int aid, const RECT &pos, int texid, const RECT &texpos, int draw_mode, int mouse_mode, int bkmode);
	void ReleaseAreas ();
	void SetArea (int idx, SURFHANDLE s);
	SURFHANDLE GetArea (int idx);
	void RedrawArea (int idx, int event);
	void RedrawAllAreas (int event);

	static bool Read (std::ifstream &ifs);
	void Write (std::ostream &ofs) const;

protected:
	/**
	 * \brief Returns the list index of an area.
	 * \param aid area identifier
	 * \return area list index (>= 0) or -1 if area doesn't exist
	 */
	inline int AreaIndex (int aid) const
	{ 
		for (int i = 0; i < narea; i++) if (area[i]->id == aid) return i;
		return -1;
	}

	/**
	 * \brief Set the active panel scaling factor.
	 * \param scale scaling factor
	 * \param force apply new scaling even if identical to current scale factor
	 */
	void SetActiveScale (double scale, bool force = false);

	/**
	 * \brief Apply scaling and shift to transformation matrix
	 */
	void SetTransformation ();

	/**
	 * \brief Release the surfaces allocated for the panel.
	 */
	void ReleaseSurfaces ();

private:
	oapi::GraphicsClient *gc; // graphics client object
	Pane *pane;               // logical cockpit object
	HWND cwnd;                // window handle for mouse position offset calculations
	int id;                   // panel identifier (0=main)
	int connect[4];           // neighbour panel identifiers
	bool visible;             // panel visible in viewport?
	bool allowMFDNudge;       // allow nudging the MFD size to fit texture size
	DWORD shiftflag;          // bitflags for shifting modes
	//SURFHANDLE hBkgSurf;      // surface handle for panel background
	MESHHANDLE hBkgMesh;      // mesh handle for panel background
	SURFHANDLE *hSurf;        // surface handles for panel textures
	DWORD nSurf;              // number of surfaces
	DWORD panelW, panelH;     // panel width, height [pixel]
	DWORD ybase;              // reference base line
	DWORD viewW, viewH;       // viewport width, height [pixel]
	double x0, y0;            // screen coordinates of top left panel corner
	double userscale;         // user-defined scaling request
	double panelscale;        // panel scaling factor
	double minscale, maxscale;// min, max scaling factor
	double tgtW, tgtH;        // scaled panel width, height [pixel]
	MATRIX3 transf;           // panel transformation matrix (contains scaling and shift)
	enum ZoomAction {NONE, ZOOM_IN, ZOOM_OUT} zoomaction;
	int refx, refy;
	int idx_mfocus;           // index of area currently receiving mouse focus
	int aid_mfocus;           // id of area currently receiving mouse focus
	int mstate;               // current mouse state
	mutable int mousex, mousey;       // mouse position at last event (area-relative)

	struct Area {
		int id;        // area identifier
		int texid;     // texture index
		RECT pos;      // area rectangle in source panel [logical]
		RECT texpos;   // area rectangle in target texture bitmap [pixel]
		int w, h;      // width, height of area
		int bltmode;   // blt mode: 0=return black surface, 1=return background, 2=return current panel state
		int redraw;    // redraw trigger code: 0=no redraw, 1=each frame, 2=on mouseclick in area
		int mouse;     // mouse states to cause callbacks
		void *context; // user-supplied pointer to context data
		SURFHANDLE surf, bksurf; // area surface, background surface
	} **area;
	int narea, nareabuf;

	struct MFDSpec {
		int nmsh;    // mesh index
		int ngrp;    // group index
		float left, right, top, bottom; // the initial borders of the display (in mesh units)
		DWORD flag;
	} mfdspec[MAXMFD];
};

#endif // !__PANEL2D_H