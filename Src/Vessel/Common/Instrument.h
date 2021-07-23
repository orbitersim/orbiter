// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//             ORBITER MODULE: Common vessel tools
//                  Part of the ORBITER SDK
//
// Instrument.h
// Interface for class PanelElement:
//   Base class for panel and VC instrument visualisations
// Interface for class Subsystem:
//   Base class for a vessel subsystem: acts as a container for
//   a group of panel elements and underlying system logic
// ==============================================================

#ifndef __INSTRUMENT_H
#define __INSTRUMENT_H

#include "Orbitersdk.h"
#include <vector>

class VESSEL3;

// ==============================================================

class PanelElement {
public:
	PanelElement (VESSEL3 *v);
	virtual ~PanelElement ();

	virtual void AddMeshData2D (MESHHANDLE hMesh, DWORD grpidx) {}
	virtual void AddMeshDataVC (MESHHANDLE hMesh, DWORD grpidx) {}
	virtual void Reset2D (int panelid);
	virtual void Reset2D (int panelid, MESHHANDLE hMesh);
	virtual void ResetVC (DEVMESHHANDLE hMesh);
	virtual void LoadVC (int vcid);
	virtual void LoadPanel2D (int panelid, PANELHANDLE hPanel, DWORD viewW, DWORD viewH);
	virtual bool Redraw2D (SURFHANDLE surf);
	virtual bool RedrawVC (DEVMESHHANDLE hMesh, SURFHANDLE surf);
	virtual bool ProcessMouse2D (int event, int mx, int my);
	virtual bool ProcessMouseVC (int event, VECTOR3 &p);

protected:
	/**
	 * \brief Add vertices to a mesh group to define the element
	 * \param hMesh mesh handle
	 * \param grpidx mesh group index
	 * \param vtx pointer to vertex block
	 * \param nvtx number of vertices in block
	 * \param idx pointer to index block
	 * \param nidx number of indices in block
	 */
	void AddGeometry (MESHHANDLE hMesh, DWORD grpidx, const NTVERTEX *vtx, DWORD nvtx, const WORD *idx, DWORD nidx);

	/**
	 * \brief Select an existing set of vertices in a mesh group to define the element
	 * \param hMesh mesh handle
	 * \param grpidx mesh group index
	 * \param vofs block offset from beginning of vertex list
	 */
	void SelectGeometry (MESHHANDLE hMesh, DWORD grpidx, int vofs);

	char *DispStr (double dist, int precision=4);

	VESSEL3 *vessel;
	MESHHANDLE mesh;
	DWORD gidx;
	MESHGROUP *grp; // panel mesh group representing the instrument
	DWORD vtxofs;   // vertex offset in mesh group
};

// ==============================================================

class AnimState2 {
public:
	AnimState2 ();
	AnimState2 (double operating_speed, double initial_state = 0.0);
	void SetOperatingSpeed (double opspeed);
	void SetState (double _state, double _speed);
	void Open();
	void Close();
	void Stop();
	void SetOpened();
	void SetClosed();
	inline bool IsActive() const { return speed != 0.0; }
	inline bool IsOpen() const { return state == 1.0; }
	inline bool IsClosed() const { return state == 0.0; }
	inline bool IsOpening() const { return speed > 0.0; }
	inline bool IsClosing() const { return speed < 0.0; }
	bool Process (double dt);
	inline double State() const { return state; }
	inline double Speed() const { return speed; }
	inline const double *StatePtr() const { return &state; }
	void SaveState (FILEHANDLE scn, const char *label);
	bool ParseScenarioLine (const char *line, const char *label);

private:
	double state;
	double speed;
	double inc_speed;
	double dec_speed;
};

// ==============================================================

class ComponentVessel;

/**
 * \brief Base class for a vessel subsystem
 *
 * This class can be used to represent a logical "subsystem" for a vessel (engine
 * management, autopilots, electrical, thermal, pressure, payload management, etc.
 * Defining subsystems in separate classes rather than directly in the vessel class
 * helps stucturing the code logically and de-clutters the vessel class.
 * Subsystem also acts as a container for the PanelElement objects that provide the
 * user interface for that subsystem. It provides mouse and redraw callback functions
 * that can be called from the corresponding vessel class callback function, and
 * passes the call on to the appropriate subsystem panel element.
 */
class Subsystem {
public:
	/**
	 * \brief Create a new top-level subsystem.
	 * \param v Vessel instance
	 * \param ident subsystem identifier
	 * \note Usually called from the vessel constructor
	 */
	Subsystem (ComponentVessel *v);

	/**
	 * \brief Subsystem destructor
	 * \note Usually called from the vessel destructor
	 */
	virtual ~Subsystem ();

	/**
	 * \brief Returns parent subsystem or 0 if top-level
	 */
	const Subsystem *Parent() const { return parent; }

	/**
	 * \brief Returns pointer to the associated vessel instance
	 */
	inline ComponentVessel *Vessel() const { return vessel; }

	/**
	 * \brief Returns panel identifier
	 */
	inline int Id() const { return id; }

	/**
	 * \brief Add a PanelElement instance to the subsystem.
	 * \param el Pointer to panel element instance
	 * \return Local panel element identifier
	 * \note The panel element instance is managed by the subsystem and destroyed
	 *   together with the subsystem. The calling function must not delete the instance.
	 */
	int AddElement (PanelElement *el);

	/**
	 * \brief Post-creation notification
	 * \note Called after the vessel has been created and its state has been defined.
	 * \If this is called at session start (i.e. for a vessel defined in the scenario)
	 *   all other start-up vessels will also have been created and defined.
	 * \note This method should be called by VESSEL3::clbkPostCreation for all defined
	 *   subsystems.
	 */
	virtual void clbkPostCreation ();

	/**
	 * \brief Subsystem save-state notification
	 * \param scn Scenario file handle
	 * \note Allows a subsystem to write its state to the scenario file.
	 * \note This method should be called by VESSEL3::clbkSaveState for all defined
	 *   subsystems.
	 */
	virtual void clbkSaveState (FILEHANDLE scn);

	/**
	 * \brief Subsystem scenario read-line notification
	 * \param line Scenario line to be processed
	 * \note This method offers a line read from the scenario file for consumption by
	 *   the subsystem. If the subsystem recognises and consumes the line, it should
	 *   return true, otherwise false.
	 * \note This method should be called within the vessel's scenario parse loop in
	 *   VESSEL3::clbkLoadStateEx for all defined subsystems
	 */
	virtual bool clbkParseScenarioLine (const char *line);

	/**
	 * \brief Pre-frame update notification
	 * \param simt Session logical runtime [s]
	 * \param simdt upcoming step interval [s]
	 * \param mjd absolute time in MJD format [days]
	 * \note This method should be called by VESSEL3::clbkPreStep for all defined
	 *   subsystems.
	 */
	virtual void clbkPreStep (double simt, double simdt, double mjd);

	/**
	 * \brief Post-frame update notification
	 * \param simt Session logical runtime [s]
	 * \param simdt last step interval [s]
	 * \param mjd absolute time in MJD format [days]
	 * \note This method should be called by VESSEL3::clbkPostStep for all defined
	 *   subsystems.
	 */
	virtual void clbkPostStep (double simt, double simdt, double mjd);

	/**
	 * \brief Set up the 2D instrument panel for the subsystem
	 * \param panelid Panel ID, as passed to VESSEL3::clbkLoadPanel2D
	 * \param hPanel Panel handle, as passed to VESSEL3::clbkLoadPanel2D
	 * \param viewW viewport width, as passed to VESSEL3::clbkLoadPanel2D
	 * \param viewH viewport height, as passed to VESSEL3::clbkLoadPanel2D
	 * \return true if the subsystem supports 2D panel cockpit mode
     */
	virtual bool clbkLoadPanel2D (int panelid, PANELHANDLE hPanel, DWORD viewW, DWORD viewH);

	/**
	 * \brief Set up the virtual panel elements for the subsystem
	 * \param vcid Virtual cockpit position ID (>= 0)
	 * \return true if the subsystem supports a virtual cockpit mode
	 * \note This method should be called by the vessel clbkLoadVC method
	 *   for all defined subsystems.
	 */
	virtual bool clbkLoadVC (int vcid);

	virtual void clbkReset2D (int panelid, MESHHANDLE hMesh);

	virtual void clbkResetVC (int vcid, DEVMESHHANDLE hMesh);

	/**
	 * \brief Redraw event for a subsystem panel element
	 * \param id Subsystem-local panel element ID
	 * \param event event type, as passed to VESSEL2::clbkVCRedrawEvent
	 * \param hMesh VC mesh handle, as passed to VESSEL2::clbkVCRedrawEvent
	 * \param hSurf texture surface mesh handle, as passed to VESSEL2::clbkVCRedrawEvent
	 * \return The function should return true if it processes the event,
	 *   false otherwise.
	 * \note This method should be called by the vessel clbkVCRedrawEvent method
	 *   after splitting the global element ID into subsystem and local element IDs
	 */
	virtual bool clbkVCRedrawEvent (int id, int event, DEVMESHHANDLE hMesh, SURFHANDLE hSurf);

	virtual bool clbkVCMouseEvent (int id, int event, VECTOR3 &p);

	virtual bool clbkDrawHUD (int mode, const HUDPAINTSPEC *hps, oapi::Sketchpad *skp);

	virtual void clbkRenderHUD (int mode, const HUDPAINTSPEC *hps, SURFHANDLE hTex);

	virtual bool clbkPlaybackEvent (double simt, double event_t, const char *event_type, const char *event);

	/**
	 * \brief Allow a subsystem to respond to a key down/up event
	 * \param key key scan code (see OAPI_KEY_* constants in OrbiterAPI.h)
	 * \param down true if key was pressed, false if key was released
	 * \param kstate current keyboard state
	 * \return The function should return 1 if the subsystem handles the event and
	 *   wants it removed from the processing queue. If it returns 0, the event
	 *   remains in the queue and is offered to the remaining subsystems and the
	 *   vessel object.
	 * \default Offers the key to all child subsystems until a nonzero value is returned.
	 *   If that happens, the function returns that value, otherwise returns 0.
	 * \note There is a potential race condition if more than one subsystem accept the
	 *   same key event: The first subsystem to return 1 will prevent the remaining
	 *   subsystems in the list from being notified about the key. The behaviour
	 *   therefore depends on the subsystem order in ComponentVessel::ssys.
	 * \sa ComponentVessel::clbkConsumeBufferedKey, VESSEL::clbkConsumeBufferedKey
	 */
	virtual int clbkConsumeBufferedKey (DWORD key, bool down, char *kstate);

	/**
	 * \brief Allow a subsystem to respond to a pressed key
	 * \param kstate Keyboard state
	 * \return The function should return 1 only if default processing of the key states
	 *   for subsequent subsystems is to be disabled altogether. Normally it should return 0.
	 * \default Offers the keyboard state to all child subsystems until a nonzero value is
	 *   returned. If that happens, the function returns that value, otherwise returns 0.
	 * \note To test for a pressed key, use the KEYDOWN(kstate,OAPI_KEY_xxx) macro. To test for
	 *   pressed modifier keys, use the KEYMOD_xxx(kstate) macro. To reset the state of a key
	 *   (so it won't be processed by further subsystems), use the RESETKEY(kstate,OAPI_KEY_xxx)
	 *   macro.
	 * \sa ComponentVessel::clbkConsumeDirectKey, VESSEL::clbkConsumeDirectKey
	 */
	virtual int clbkConsumeDirectKey (char *kstate);

protected:
	/**
	 * \brief Create a subsystem as a component of a parent subsystem.
	 * \param p Parent subsystem
	 */
	Subsystem (Subsystem *p);

	/**
	 * \brief Add a new child system.
	 * \param subsys Pointer to dynamically allocated subsystem object.
	 * \note The caller transfers ownership of the pointer. A subsystem automatically deletes
	 *   its child systems on destruction.
	 */
	void AddSubsystem (Subsystem *subsys);

private:
	Subsystem *parent;                  ///< parent systems (0 if top-level system)
	std::vector<Subsystem*> child;      ///< list of child systems
	std::vector<PanelElement*> element; ///< list of panel elements
	ComponentVessel *vessel;            ///< associated vessel object
	int id;                             ///< subsystem ID
};

// ==============================================================

/**
 * \brief A convenience vessel class which incorporates subsystem support.
 */
class ComponentVessel: public VESSEL4 {
	friend class Subsystem;

public:
	ComponentVessel (OBJHANDLE hVessel, int fmodel=1);
	virtual ~ComponentVessel ();
	void AddSubsystem (Subsystem *subsys);
	inline int NumSubsystems() const { return ssys.size(); }

	void clbkSaveState (FILEHANDLE scn);
	bool clbkParseScenarioLine (const char *line);
	void clbkPostCreation ();
	bool clbkDrawHUD (int mode, const HUDPAINTSPEC *hps, oapi::Sketchpad *skp);
	void clbkRenderHUD (int mode, const HUDPAINTSPEC *hps, SURFHANDLE hTex);
	bool clbkPlaybackEvent (double simt, double event_t, const char *event_type, const char *event);
	void clbkPreStep (double simt, double simdt, double mjd);
	void clbkPostStep (double simt, double simdt, double mjd);
	void clbkReset2D (int panelid, MESHHANDLE hMesh);
	void clbkResetVC (int vcid, DEVMESHHANDLE hMesh);
	bool clbkLoadPanel2D (int panelid, PANELHANDLE hPanel, DWORD viewW, DWORD viewH);
	bool clbkLoadVC (int vcid);
	bool clbkVCMouseEvent (int elid, int event, VECTOR3 &p);
	bool clbkVCRedrawEvent (int elid, int event, DEVMESHHANDLE hMesh, SURFHANDLE hSurf);
	int clbkConsumeBufferedKey (DWORD key, bool down, char *kstate);
	int clbkConsumeDirectKey (char *kstate);

private:
	std::vector<Subsystem*> ssys;   // list of subsystems
	int next_ssys_id;               // next subsystem id to be assigned
};

#endif // !__INSTRUMENT_H