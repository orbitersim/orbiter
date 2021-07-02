/**
 * \defgroup module_clbk Top-level module callback functions
 *
 * This section contains a list of global nonmember callback functions
 * that can be defined by an addon module. Orbiter will call these functions
 * when specific events occur, e.g. a module is activated or deactivated,
 * a simulation session is opened or closed, etc.
 */
//@{

/**
 * \defgroup general_clbk General module callback functions
 *
 * Module initialisation and exit notifications. The two callback functions
 * in this group are called by Orbiter when the module is loaded or unloaded,
 * respecively. It is used for all module types (plugin and vessel modules).
 */
//@{

/**
 * \brief Module initialisation callback function
 * \param hModule module handle
 * \note This function is called by Orbiter when a module becomes active.
 * \note For plugin modules, InitModule is called at program start for all
 *   modules in the <i>active module list</i> of orbiter.def, or whenever
 *   a user activates a module in the <i>Modules</i> tab of the Orbiter
 *   launchpad.
 * \note For vessel modules, InitModule is called whenever the first vessel
 *   of the corresponding type is created (usually at the start of a
 *   simulation session, or during a simulation if the first vessel instance
 *   is created dynamically).
 * \note hModule is the module handle that identifies the addon DLL being
 *   initialised. It can be stored and used later, e.g. for loading resources
 *   from the module. To get the handle of the Orbiter core module, use
 *   \ref oapiGetOrbiterInstance.
 */
DLLCLBK void InitModule (HINSTANCE hModule);

/**
 * \brief Module exit notification callback function
 * \param hModule module handle
 * \note This function is called by Orbiter when a module is deactivated.
 * \note For plugin modules, ExitModule is called at program shutdown for
 *   all active modules, or whenever a user deactivates a module in the
 *   <i>Modules</i> tab of the Orbiter launchpad.
 * \note For vessel modules, ExitModule is called when a simulation session
 *   is closed for any vessel types active at that time, or during a session
 *   when the last vessel of this type is destroyed.
 */
DLLCLBK void ExitModule (HINSTANCE hModule);
//@}

/**
 * \defgroup vessel_clbk Vessel module callback functions
 *
 * This section contains a list of nonmember callback functions for vessel
 * modules. Apart from the general module initialisation and exit functions
 * in \ref module_clbk, the only vessel-specific top-level callback functions
 * are notifications for vessel creation and deletion. During the vessel
 * creation callback, the module should create an instance of a class derived
 * from VESSEL2 or VESSEL3, and delete the instance during the vessel deletion
 * callback. All other events should be handled by overloading the appropriate
 * VESSEL2 and VESSEL3 member callback functions.
 */
//@{

/**
 * \brief Vessel instance creation notification.
 * \param hvessel vessel handle
 * \param flightmodel flight model selection identifier
 * \return The function should return a pointer to the derived VESSEL
 *   instance it created.
 * \note This function is called by Orbiter whenever a vessel of the type
 *   defined by the module is created at the beginning or during a simulation
 *   session.
 * \note The implementation should create an instance of a vessel class
 *   derived from VESSEL, VESSEL2 or VESSEL3 and return a pointer to it.
 * \note hvessel is a handle that identifies the vessel instance in Orbiter.
 * \note flightmodel identifies the realism level of the requested flight
 *   model. This value may be 0 (simple) or 1 (complex). Vessel implementation
 *   that support different flight models for easy/realistic setups can use
 *   this value to define the appropriate model.
 */
DLLCLBK VESSEL *ovcInit (OBJHANDLE hvessel, int flightmodel);

/**
 * \brief Vessel deletion notification.
 * \param vessel pointer to vessel instance
 * \note This function is called by Orbiter whenever a vessel of the type
 *   defined by the module is about to be destroyed at the end or during a
 *   simulation session.
 * \note The pointer passed to the function is the same as the one returned
 *   by ovcInit for the corresponding vessel.
 * \note Typically, the implementation of this function should cast the
 *   pointer to a pointer to the derived vessel class, and delete the object.
 */
DLLCLBK void ovcExit (VESSEL *vessel);

//@}

/**
 * \defgroup plugin_clbk Plugin module callback functions
 *
 * The callback functions in this group are specific for \e plugin modules, i.e.
 * modules that can be activated or deactivated in the Modules tab of the
 * Orbiter Launchpad. They can not be used in vessel modules.
 *
 * Note that most of the top-level plugin callback functions (opcXXX) are
 * now obsolete and should no longer be used. Addon modules should instead
 * create an instance of a class derived from the \ref oapi::Module class during
 * \ref InitModule, and overload the appropriate class-level callback functions.
 */
//@{
/**
 * \brief Called by Orbiter when a graphics-enabled simulation session is started.
 * \deprecated This function has been replaced by \ref oapi::Module::clbkSimulationStart.
 * \param hRenderWnd render window handle
 * \param width viewport width [pixel]
 * \param height viewport height [pixel]
 * \param fullscreen flag for fullscreen mode
 * \note Plugins should no longer implement this function. Instead they should
 *   create an instance of a class derived from oapi::Module during \ref InitModule
 *   that overloads the oapi::Module::clbkSimulationStart method, and register it with
 *   \ref oapiRegisterModule.
 * \note opcOpenRenderViewport is called by Orbiter only if no instance of oapi::Module
 *   is created and registered during \ref InitModule, or if a registered module does
 *   not overload the oapi::Module::clbkSimulationStart method.
 */
DLLCLBK void opcOpenRenderViewport(HWND hRenderWnd, DWORD width, DWORD height, BOOL fullscreen);

/**
 * \brief Called by Orbiter when a graphics-enabled simulation session is closed.
 * \deprecated This function has been replaced by \ref oapi::Module::clbkSimulationEnd.
 * \note Plugins should no longer implement this function. Instead they should
 *   create an instance of a class derived from oapi::Module during \ref InitModule
 *   that overloads the oapi::Module::clbkSimulationEnd method, and register it with
 *   \ref oapiRegisterModule.
 * \note opcCloseRenderViewport is called by Orbiter only if no instance of oapi::Module
 *   is created and registered during \ref InitModule, or if a registered module does
 *   not overload the oapi::Module::clbkSimulationEnd method.
 */
DLLCLBK void opcCloseRenderViewport ();

/**
 * \brief Time step notification before state update. 
 *
 * Called at each time step of the simulation, before the state is updated to the
 * current simulation time. This function is only called when the "physical" state
 * of the simulation is propagated in time. opcPreStep is not called while the
 * simulation is paused, even if the user moves the camera. 
 * \deprecated This function has been replaced by \ref oapi::Module::clbkPreStep.
 * \param simt simulation time after the currently processed step [s]
 * \param simdt length of the currently processed step [s]
 * \param mjd simulation time afte the currently processed step in
 *   Modified Julian Date format [days]
 * \note Plugins should no longer implement this function. Instead they should
 *   create an instance of a class derived from oapi::Module during \ref InitModule
 *   that overloads the oapi::Module::clbkPreStep method, and register it with
 *   \ref oapiRegisterModule.
 * \note opcPreStep is called by Orbiter only if no instance of oapi::Module
 *   is created and registered during \ref InitModule, or if a registered module does
 *   not overload the oapi::Module::clbkPreStep method.
 */
DLLCLBK void opcPreStep (double simt, double simdt, double mjd);

/**
 * \brief Time step notification after state update
 *
 * Called at each time step of the simulation, after the state has
 * been updated to the current simulation time.
 * \deprecated This function has been replaced by \ref oapi::Module::clbkPostStep.
 * \param simt current simulation time [s]
 * \param simdt length of the last time step [s]
 * \param mjd simulation time in Modified Julian Date format [days]
 * \note Plugins should no longer implement this function. Instead they should
 *   create an instance of a class derived from oapi::Module during \ref InitModule
 *   that overloads the oapi::Module::clbkPostStep method, and register it with
 *   \ref oapiRegisterModule.
 * \note opcPostStep is called by Orbiter only if no instance of oapi::Module
 *   is created and registered during \ref InitModule, or if a registered module does
 *   not overload the oapi::Module::clbkPostStep method.
 */
DLLCLBK void opcPostStep (double simt, double simdt, double mjd);

/**
 * \brief Change of input focus notification
 *
 * Called when input focus (keyboard and joystick control) is
 * switched to a new vessel (for example as a result of a call to
 * oapiSetFocus).
 * \deprecated This function has been replaced by \ref oapi::Module::clbkFocusChanged.
 * \param hGainsFocus handle of vessel receiving the input focus
 * \param hLosesFocus handle of vessel losing focus
 * \note Plugins should no longer implement this function. Instead they should
 *   create an instance of a class derived from oapi::Module during \ref InitModule
 *   that overloads the oapi::Module::clbkFocusChanged method, and register it with
 *   \ref oapiRegisterModule.
 * \note opcFocusChanged is called by Orbiter only if no instance of oapi::Module
 *   is created and registered during \ref InitModule, or if a registered module does
 *   not overload the oapi::Module::clbkFocusChanged method.
 */
DLLCLBK void opcFocusChanged (OBJHANDLE hGainsFocus, OBJHANDLE hLosesFocus);

/**
 * \brief Change of time acceleration notification
 *
 * Called when the simulation time acceleration factor changes.
 * \deprecated This function has been replaced by \ref oapi::Module::clbkTimeAccChanged.
 * \param new_warp new time acceleration factor
 * \param old_warp old time acceleration factor
 * \note Plugins should no longer implement this function. Instead they should
 *   create an instance of a class derived from oapi::Module during \ref InitModule
 *   that overloads the oapi::Module::clbkTimeAccChanged method, and register it with
 *   \ref oapiRegisterModule.
 * \note opcTimeAccChanged is called by Orbiter only if no instance of oapi::Module
 *   is created and registered during \ref InitModule, or if a registered module does
 *   not overload the oapi::Module::clbkTimeAccChanged method.
 */
DLLCLBK void opcTimeAccChanged (double new_warp, double old_warp);

/**
 * \brief Simulation pause/resume notification
 *
 * Called when the pause/resume state of the simulation has changed.
 * \deprecated This function has been replaced by \ref oapi::Module::clbkPause.
 * \param pause pause/resume state: true if simulation has been
 *   paused, false if simulation has been resumed.
 * \note Plugins should no longer implement this function. Instead they should
 *   create an instance of a class derived from oapi::Module during \ref InitModule
 *   that overloads the oapi::Module::clbkPause method, and register it with
 *   \ref oapiRegisterModule.
 * \note opcPause is called by Orbiter only if no instance of oapi::Module
 *   is created and registered during \ref InitModule, or if a registered module does
 *   not overload the oapi::Module::clbkPause method.
 */
DLLCLBK void opcPause (bool pause);

/**
 * \brief Vessel destruction notification
 *
 * Sent to modules immediately before a vessel is destroyed. After
 * this callback method returns, the object handle (hVessel) and
 * will no longer be valid. Modules should make sure that they don't
 * access the vessel in any form after this point.
 * \deprecated This function has been replaced by \ref oapi::Module::clbkDeleteVessel.
 * \param hVessel object handle for the vessel being destroyed.
 * \note Plugins should no longer implement this function. Instead they should
 *   create an instance of a class derived from oapi::Module during \ref InitModule
 *   that overloads the oapi::Module::clbkDeleteVessel method, and register it with
 *   \ref oapiRegisterModule.
 * \note opcDeleteVessel is called by Orbiter only if no instance of oapi::Module
 *   is created and registered during \ref InitModule, or if a registered module does
 *   not overload the oapi::Module::clbkDeleteVessel method.
 */
DLLCLBK void opcDeleteVessel (OBJHANDLE hVessel);
//@}

//@}