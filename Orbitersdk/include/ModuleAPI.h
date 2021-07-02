// ======================================================================
//                     ORBITER SOFTWARE DEVELOPMENT KIT
//                  Copyright (C) 2001-2007 Martin Schweiger
//                           All rights reserved
// ModuleAPI.h
// Defines generic base class Orbiter::Module which can be used by
// plugins to define a set of interface functions to the Orbiter core.
// ======================================================================

#ifndef __MODULEAPI_H
#define __MODULEAPI_H

namespace oapi {

	/**
	 * \brief Generic Orbiter plugin interface class
	 *
	 * Defines generic base class which can be used by plugins to provide a set
	 * of interface functions to the Orbiter core.
	 * This class contains only the non-virtual set of methods (excluding
	 * callback functions). Plugin implementations should normally not derive
	 * their inferface classes from oapi::ModuleNV, but instead from class
	 * oapi::Module which includes the virtual callback methods.
	 */
	class OAPIFUNC ModuleNV {
	public:
		/**
		 * \brief Creates a new ModuleNV instance.
		 * \param hDLL DLL library instance handle (see \ref InitModule)
		 */
		ModuleNV (HINSTANCE hDLL);

		/**
		 * \brief Module interface version
		 * \return version number
		 */
		inline int Version() const { return version; }

		/**
		 * \brief Returns the module instance handle.
		 * \return Module instance handle.
		 */
		inline HINSTANCE GetModule() const { return hModule; }

		/**
		 * \brief Returns simulation time since session start.
		 * \return Simulation session time [s]
		 * \note The simulation session timeis useful mainly for time
		 *   differences. To get an absolute time parameter, use
		 *   GetSimMJD.
		 * \sa GetSimMJD, GetSimStep
		 */
		double GetSimTime() const;

		/**
		 * \brief Returns the length of the last time step
		 * \return Step length [s]
		 * \note This method returns the time difference between the current
		 *   and previous time frame.
		 * \note This parameter is useful for numerical (finite difference)
		 *   calculation of time derivatives.
		 * \sa GetSimTime
		 */
		double GetSimStep() const;

		/**
		 * \brief Returns the absolute simulation time in Modified Julian
		 *   Date format.
		 * \return Current Modified Julian Date [days]
		 * \note Orbiter defines the Modified Julian Date (MJD) as
		 *   JD - 2 400 000.5, where JD is the Julian Date. JD is the
		 *   interval of time in mean solar days elapsed since 4713 BC
		 *   January 1 at Greenwich mean noon.
		 * \sa GetSimTime
		 */
		double GetSimMJD() const;

	protected:
		int version;
		HINSTANCE hModule;
	}; // class ModuleNV

	/**
	 * \brief Generic Orbiter plugin interface class
	 *
	 * Defines generic base class which can be used by plugins to provide a set
	 * of interface functions to the Orbiter core, and callback
	 * functions which can be overloaded by derived classes to react to specific
	 * types of events.
	 */
	class OAPIFUNC Module: public ModuleNV {
	public:
		/**
		 * \brief Creates a new Module instance.
		 * \param hDLL DLL library instance handle (see \ref InitModule)
		 */
		Module (HINSTANCE hDLL);
		virtual ~Module();

		/**
		 * \brief Simulation graphics support type
		 * \sa clbkSimulationStarted
		 */
		enum RenderMode {
			RENDER_NONE,		///< no graphics support
			RENDER_FULLSCREEN,  ///< fullscreen mode
			RENDER_WINDOW       ///< windowed mode
		};

		/**
		 * \brief Simulation start notification
		 *
		 * This method is called immediately after a simulation session has been
		 * set up (i.e. all objects created and their states set according to
		 * the scenario data) and the render window has been opened (if
		 * applicable).
		 * \param mode defines the graphics support (none, fullscreen or windowed)
		 * \default Calls \ref opcOpenRenderViewport, if defined in the module.
		 */
		virtual void clbkSimulationStart (RenderMode mode);

		/**
		 * \brief Simulation end notification
		 *
		 * This method is called immediately before a simulation session is
		 * terminated, and before the render window is closed.
		 * \default Calls \ref opcCloseRenderViewport, if defined in the module.
		 */
		virtual void clbkSimulationEnd ();

		/**
		 * \brief Time step notification before state update
		 *
		 * Called at each time step of the simulation, before the state is
		 * updated to the current simulation time. This function is only
		 * called when the "physical" state of the simulation is propagated
		 * in time. clbkPreStep is not called while the simulation is paused,
		 * even if the user moves the camera.
		 * \param simt simulation time after the currently processed step [s]
		 * \param simdt length of the currently processed step [s]
		 * \param mjd simulation time afte the currently processed step in
		 *   Modified Julian Date format [days]
		 * \default Calls \ref opcPreStep, if defined in the module.
		 * \note This function is called by Orbiter after the new time step
		 *   length (simdt) and simulation time (simt) have been calculated,
		 *   but before the simulation state is integrated to simt. The
		 *   parameters passed to clbkPreStep therefore are the values that
		 *   will be applied in the current simulation step.
		 * \sa clbkPostStep
		 */
		virtual void clbkPreStep (double simt, double simdt, double mjd);

		/**
		 * \brief Time step notification after state update
		 *
		 * Called at each time step of the simulation, after the state has
		 * been updated to the current simulation time.
		 * \param simt current simulation time [s]
		 * \param simdt length of the last time step [s]
		 * \param mjd simulation time in Modified Julian Date format [days]
		 * \default Calls \ref opcPostStep, if defined in the module.
		 * \sa clbkPreStep
		 */
		virtual void clbkPostStep (double simt, double simdt, double mjd);

		/**
		 * \brief Discontinuous simulation time jump notification
		 *
		 * Called after a discontinuous explicit reset of the simulation
		 * time (e.g. using the scenario editor).
		 * \param simt new simulation time relative to session start [s]
		 * \param simdt jump interval [s]
		 * \param mjd new absolute simulation time in MJD format [days]
		 * \default None.
		 * \note simdt can be negative if a jump to an earlier time
		 *   was performed.
		 * \note simt can become negative if a jump prior to the session
		 *   start time was performed.
		 */
		virtual void clbkTimeJump (double simt, double simdt, double mjd) {}

		/**
		 * \brief Change of input focus notification
		 *
		 * Called when input focus (keyboard and joystick control) is
		 * switched to a new vessel (for example as a result of a call to
		 * oapiSetFocus).
		 * \param new_focus handle of vessel receiving the input focus
		 * \param old_focus handle of vessel losing focus
		 * \default Calls \ref opcFocusChanged, if defined in the module.
		 * \note Currently only objects of type "vessel" can receive the
		 *   input focus. This may change in future versions.
		 * \note This callback function is also called at the beginning of
		 *   the simulation, where new_focus is the vessel receiving the
		 *   initial focus, and old_focus is NULL.
		 * \note clbkFocusChanged is sent to non-vessel modules after the
		 *   vessels receiving and losing focus have been notified via
		 *   VESSEL2::clbkFocusChanged.
		 */
		virtual void clbkFocusChanged (OBJHANDLE new_focus, OBJHANDLE old_focus);

		/**
		 * \brief Change of time acceleration notification
		 *
		 * Called when the simulation time acceleration factor changes.
		 * \param new_warp new time acceleration factor
		 * \param old_warp old time acceleration factor
		 * \default Calls \ref opcTimeAccChanged, if defined in the module.
		 */
		virtual void clbkTimeAccChanged (double new_warp, double old_warp);

		/**
		 * \brief Vessel creation notification
		 *
		 * Sent to modules after a new vessel has been created during the
		 * simulation run. Not sent for vessels created from the scenario script
		 * at the start of a session.
		 * \param hVessel object handle for the new vessel
		 * \default None.
		 */
		virtual void clbkNewVessel (OBJHANDLE hVessel) {}

		/**
		 * \brief Vessel destruction notification
		 *
		 * Sent to modules immediately before a vessel is destroyed. After
		 * this callback method returns, the object handle (hVessel) and
		 * will no longer be valid. Modules should make sure that they don't
		 * access the vessel in any form after this point.
		 * \param hVessel object handle for the vessel being destroyed.
		 * \default Calls \ref opcDeleteVessel, if defined in the module.
		 */
		virtual void clbkDeleteVessel (OBJHANDLE hVessel);

		/**
		 * \brief Discontinuous vessel repositioning notification
		 *
		 * Sent to modules after a vessel position has been set explicitly
		 * (rather than via continuous state propagation. This callback can be
		 * used to force a refresh of parameters that depend on vessel position.
		 * \param hVessel vessel object handle
		 * \default None.
		 * \note This method is called after a VESSEL::ShiftCentreOfMass()
		 */
		virtual void clbkVesselJump (OBJHANDLE hVessel) {}

		/**
		 * \brief Simulation pause/resume notification
		 *
		 * Called when the pause/resume state of the simulation has changed.
		 * \param pause pause/resume state: true if simulation has been
		 *   paused, false if simulation has been resumed.
		 * \default Calls \ref opcPause, if defined in the module.
		 */
		virtual void clbkPause (bool pause);

		/**
		 * \brief Process mouse events
		 *
		 * Called to offer a mouse event to the module.
		 * \param event Event type. This is a Windows message identifier (WM_xxx) such as WM_LBUTTONDOWN.
		 * \param state Keyboard state during mouse event. This corresponds to the WPARAM value passed to
		 *   the window message handler for mouse events (e.g. MK_CONTROL).
		 * \param x Mouse x position in render window at event
		 * \param y Mouse y position in render window at event
		 * \return Returning true prevents the event from entering the standard Orbiter processing queue
		 *   (blocking event). Returning false allows Orbiter to also process this event.
		 * \default Nothing, returns false.
		 * \note Even if an overloaded function returns true, the event will still be offered to the other
		 *   plugin modules through their clbkProcessMouse method. It only prevents the standard Orbiter
		 *   processing of the event. This avoids one module blocking all subsequent modules from receiving
		 *   the event notification.
		 * \note Mouse-processing of the Orbiter main menu cannot be blocked.
		 * \sa clbkProcessKeyboardImmediate, clbkProcessKeyboardBuffered
		 */
		virtual bool clbkProcessMouse (UINT event, DWORD state, DWORD x, DWORD y) { return false; }

		/**
		 * \brief Process immediate keyboard events
		 *
		 * Called to offer immediate keyboard events to the module.
		 * \param kstate State flags for all keyboard keys. The indices into the kstate list correspond to
		 *   the OAPI_KEY_xxx constants defined in OrbiterAPI.h. A pressed key is indicated by a set bit 0x80.
		 * \param simRunning Indicates that the simulation is running (not paused). This allows a module
		 *   to differentiate between key processing in running/paused simulation states.
		 * \return Returning true prevents the keyboard event from entering the standard Orbiter processing
		 *   queue (blocking event). Returning false allows Orbiter to also process the event.
		 * \default Nothing, returns false.
		 * \note Even if an overloaded function returns true, the event will still be offered to the other
		 *   plugin modules through their clbkProcessKeyboardBuffered method. It only prevents the standard Orbiter
		 *   processing of the event. This avoids one module blocking all subsequent modules from receiving
		 *   the event notification.
		 * \sa clbkProcessKeyboardBuffered, clbkProcessMouse
		 */
		virtual bool clbkProcessKeyboardImmediate (char kstate[256], bool simRunning) { return false; }

		/**
		 * \brief Process buffered keyboard events
		 *
		 * Called to offer buffered keyboard events to the module.
		 * \param key Key identifier. Corresponds to the OAPI_KEY_xxx constants defined in OrbiterAPI.h.
		 * \param kstate State flags for all keyboard keys. This allows to check for modifier keys pressed when the
		 *   key event was generated. The indices into the kstate list correspond to the OAPI_KEY_xxx constants
		 *   defined in OrbiterAPI.h. A pressed key is indicated by a set bit 0x80.
		 * \param simRunning Indicates that the simulation is running (not paused). This allows a module
		 *   to differentiate between key processing in running/paused simulation states.
		 * \return Returning true prevents the keyboard event from entering the standard Orbiter processing
		 *   queue (blocking event). Returning false allows Orbiter to also process the event.
		 * \default Nothing, returns false.
		 * \note Even if an overloaded function returns true, the event will still be offered to the other
		 *   plugin modules through their clbkProcessKeyboardImmediate method. It only prevents the standard Orbiter
		 *   processing of the event. This avoids one module blocking all subsequent modules from receiving
		 *   the event notification.
		 * \sa clbkProcessKeyboardImmediate, clbkProcessMouse
		 */
		virtual bool clbkProcessKeyboardBuffered (DWORD key, char kstate[256], bool simRunning) { return false; }

	}; // class Module

}; // namespace oapi

#endif // !__MODULEAPI_H