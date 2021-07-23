// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#ifndef __INTERPRETER_H
#define __INTERPRETER_H

extern "C" {
#include "lua\lua.h"
#include "lua\lualib.h"
#include "lua\lauxlib.h"
}

#include "OrbiterAPI.h"

#define PRMTP_NUMBER        0
#define PRMTP_VECTOR        1
#define PRMTP_STRING        2
#define PRMTP_LIGHTUSERDATA 3
#define PRMTP_TABLE         4
#define PRMTP_BOOLEAN       5

#define ASSERT_SYNTAX(cond,msg) { if(!(cond)) { char cbuf[1024]; sprintf (cbuf, "%s: %s", __FUNCTION__+13, msg); term_strout(L,cbuf); return 0; } }
#define ASSERT_FUNCPRM(L,idx,tp) { if (!AssertPrmtp(L,__FUNCTION__,idx,idx,tp)) return 0; }

#define ASSERT_PRM(L,idx,tp)     { if (!AssertPrmtp(L,__FUNCTION__,idx,idx,tp)) return 0; }
#define ASSERT_NUMBER(L,idx)           ASSERT_PRM(L,idx,PRMTP_NUMBER)
#define ASSERT_VECTOR(L,idx)           ASSERT_PRM(L,idx,PRMTP_VECTOR)
#define ASSERT_STRING(L,idx)           ASSERT_PRM(L,idx,PRMTP_STRING)
#define ASSERT_LIGHTUSERDATA(L,idx)    ASSERT_PRM(L,idx,PRMTP_LIGHTUSERDATA)
#define ASSERT_TABLE(L,idx)            ASSERT_PRM(L,idx,PRMTP_TABLE)
#define ASSERT_BOOLEAN(L,idx)          ASSERT_PRM(L,idx,PRMTP_BOOLEAN)

#define ASSERT_MTDPRM(L,idx,tp)  { if (!AssertPrmtp(L,__FUNCTION__,idx,idx-1,tp)) return 0; }
#define ASSERT_MTDNUMBER(L,idx)        ASSERT_MTDPRM(L,idx,PRMTP_NUMBER)
#define ASSERT_MTDVECTOR(L,idx)        ASSERT_MTDPRM(L,idx,PRMTP_VECTOR)
#define ASSERT_MTDSTRING(L,idx)        ASSERT_MTDPRM(L,idx,PRMTP_STRING)
#define ASSERT_MTDLIGHTUSERDATA(L,idx) ASSERT_MTDPRM(L,idx,PRMTP_LIGHTUSERDATA)
#define ASSERT_MTDTABLE(L,idx)         ASSERT_MTDPRM(L,idx,PRMTP_TABLE)
#define ASSERT_MTDBOOLEAN(L,idx)       ASSERT_MTDPRM(L,idx,PRMTP_BOOLEAN)

#ifdef INTERPRETER_IMPLEMENTATION
#define INTERPRETERLIB DLLEXPORT
#else
#define INTERPRETERLIB DLLIMPORT
#endif

class VESSEL;
class MFD2;

struct AirfoilContext {
	lua_State *L;
	char funcname[128];
};

// ======================================================================
// Nonmember functions

// converts the vector at stack position 'idx' into a VECTOR3
INTERPRETERLIB VECTOR3 lua_tovector (lua_State *L, int idx);

// ======================================================================
// class Interpreter

class INTERPRETERLIB Interpreter {
public:
	Interpreter ();
	virtual ~Interpreter ();

	/**
	 * \brief Set up the interpreter (load libraries etc.)
	 */
	virtual void Initialise ();

	/**
	 * \brief Return the Lua object
	 */
	lua_State *GetState() { return L; }

	/**
	 * \brief Returns interpreter status.
	 * \return 0=normal, 1=kill pending
	 */
	int Status () const;

	/**
	 * \brief Returns interpreter execution status.
	 * \return \e true if interpreter is busy (in the process of running a
	 *   command or script), \e false if it is waiting for intput.
	 */
	bool IsBusy () const;

	/**
	 * \brief Returns the number of background jobs active during idle phase.
	 * \return number of background jobs
	 * \note A command may create background jobs that are still active after
	 *   the command returns. The interpreter idle loop continues processing
	 *   the remaining jobs until all are finished, or until a new command
	 *   is entered which takes over control of the background jobs.
	 */
	inline int nJobs () const { return jobs; }

	/**
	 * \brief Request interpreter termination.
	 * \note This sets the interpreter Status() to 1 (kill pending). It is
	 *   up to the client to delete the interpreter instance and clean up
	 *   (terminate interpreter thread etc.)
	 */
	void Terminate ();
	
	void PostStep (double simt, double simdt, double mjd);

	/**
	 * \brief Wait for thread execution.
	 * \note This is called by either the orbiter thread or the interpreter
	 *   thread when they are waiting to regain execution control.
	 */
	virtual void WaitExec (DWORD timeout = INFINITE);

	/**
	 * \brief Release thread execution.
	 * \param timeout time [ms] to wait for the mutex. Default is infinite
	 *   (wait does not time out).
	 * \note This is called by either the orbiter thread or the interpreter
	 *   thread after finishing a cycle to hand control over to the other
	 *   thread.
	 */
	virtual void EndExec ();

	/**
	 * \brief Define functions for interfacing with Orbiter API
	 */
	virtual void LoadAPI ();

	virtual void LoadVesselAPI ();

	virtual void LoadMFDAPI ();

	virtual void LoadLightEmitterMethods ();

	virtual void LoadSketchpadAPI ();

	/**
	 * \brief Run the interpreter initialisation script
	 */
	virtual void LoadStartupScript ();

	virtual int ProcessChunk (const char *chunk, int n);

	/**
	 * \brief Executes a command or script.
	 * \param chunk command line string
	 * \param n string length
	 * \return Execution status as returned by lua_pcall (0=no error)
	 */
	virtual int RunChunk (const char *chunk, int n);

	/**
	 * \brief Copies a string to the terminal.
	 * \param str string to be displayed.
	 * \note Terminal-type derived classes should implement this method.
	 * \note Default action: none.
	 */
	virtual void term_strout (const char *str, bool iserr=false) {}

	/**
	 * \brief Copies the string on top of the stack to the terminal.
	 * \param L Lua interpreter instance
	 * \note Default action: Passes the string on top of the stack to term_echo().
	 */
	virtual void term_out (lua_State *L, bool iserr=false);

	/**
	 * \brief Push an MFD object onto the stack
	 * \param L Lua interpreter instance
	 * \param mfd pointer to MFD object
	 */
	static void lua_pushmfd (lua_State *L, MFD2 *mfd);

	/**
	 * \brief Push a light source object onto the stack
	 * \param L Lua interpreter instance
	 * \param le pointer to LightEmitter object
	 */
	static void lua_pushlightemitter (lua_State *L, const LightEmitter *le);

	/**
	 * \brief Push a Sketchpad object onto the stack
	 * \param L Lua interpreter instance
	 * \param skp pointer to Sketchpad object
	 */
	static void lua_pushsketchpad (lua_State *L, oapi::Sketchpad *skp);

	void term_setverbosity (int level) { term_verbose = level; }

protected:
	lua_State *L;         // Lua main context

	/**
	 * \brief Load screen annotation methods.
	 */
	void LoadAnnotationAPI ();

	static bool InitialiseVessel (lua_State *L, VESSEL *v);
	static bool LoadVesselExtensions (lua_State *L, VESSEL *v);

	// terminal functions (to be implemented by terminal-based subclasses)
	void term_echo (lua_State *L, int level=1); // terminal verbose echo
	int term_verbose;                       // terminal verbosity level
	bool is_term;                           // interpreter attached to a terminal
	static void term_strout (lua_State *L, const char *str, bool iserr=false);

	static int AssertPrmtp(lua_State *L, const char *fname, int idx, int prm, int tp);

	// suspend script execution for one cycle
	void frameskip (lua_State *L);

	// extract interpreter pointer from lua state
	static Interpreter *GetInterpreter (lua_State *L);

	// Extended version of the built-in lua_tostring function:
	// This also handles vector and nil entries.
	static const char *lua_tostringex (lua_State *L, int idx, char *cbuf = 0);

	// pushes vector 'vec' into a table on top of the stack
	static void lua_pushvector (lua_State *L, const VECTOR3 &vec);

	// returns 1 if stack entry idx is a vector, 0 otherwise
	static int lua_isvector (lua_State *L, int idx);

	// pushes matrix 'mat' into a table on top of the stack
	static void lua_pushmatrix (lua_State *L, const MATRIX3 &mat);

	// converts the matrix at stack position 'idx' into a MATRIX3
	static MATRIX3 lua_tomatrix (lua_State *L, int idx);

	// returns 1 if stack entry idx is a matrix, 0 otherwise
	static int lua_ismatrix (lua_State *L, int idx);

	static COLOUR4 lua_torgba (lua_State *L, int idx);

	// pops an OBJHANDLE from the stack
	static OBJHANDLE lua_toObject (lua_State *L, int idx=-1);

	// pushes a vessel object on the stack
	static void lua_pushvessel (lua_State *L, VESSEL *v);

	// pops a VESSEL interface from the stack
	static VESSEL *lua_tovessel (lua_State *L, int idx=-1);

	// pops an MFD2 interface from the stack
	static MFD2 *lua_tomfd (lua_State *L, int idx=-1);

	// pops a light emitter object from the stack
	static LightEmitter *lua_tolightemitter (lua_State *L, int idx=-1);

	// pops a Sketchpad interface from the stack
	static oapi::Sketchpad *lua_tosketchpad (lua_State *L, int idx=-1);

	// global functions
	static int help (lua_State *L);
	static int help_api (lua_State *L);

	// vector library functions
	static int vec_set (lua_State *L);
	static int vec_add (lua_State *L);
	static int vec_sub (lua_State *L);
	static int vec_mul (lua_State *L);
	static int vec_div (lua_State *L);
	static int vec_dotp (lua_State *L);
	static int vec_crossp (lua_State *L);
	static int vec_length (lua_State *L);
	static int vec_dist (lua_State *L);
	static int vec_unit (lua_State *L);
	static int mat_identity (lua_State *L);
	static int mat_mul (lua_State *L);
	static int mat_tmul (lua_State *L);
	static int mat_mmul (lua_State *L);

	// process library functions
	static int procFrameskip (lua_State *L);

	// -------------------------------------------
	// oapi library functions
	// -------------------------------------------
	static int oapiGetObjectHandle (lua_State *L);
	static int oapiGetObjectCount (lua_State *L);
	static int oapiGetObjectName (lua_State *L);
	static int oapiCreateAnnotation (lua_State *L);
	static int oapiDelAnnotation (lua_State *L);
	static int oapiDbgOut (lua_State *L);
	static int oapiOpenHelp (lua_State *L);
	static int oapiOpenInputBox (lua_State *L);
	static int oapiReceiveInput (lua_State *L);
	static int oapi_global_to_equ (lua_State *L);
	static int oapi_equ_to_global (lua_State *L);
	static int oapi_orthodome (lua_State *L);
	static int oapi_del_vessel (lua_State *L);

	// menu functions
	static int oapi_get_mainmenuvisibilitymode (lua_State *L);
	static int oapi_set_mainmenuvisibilitymode (lua_State *L);
	static int oapi_get_maininfovisibilitymode (lua_State *L);
	static int oapi_set_maininfovisibilitymode (lua_State *L);

	// Time functions
	static int oapi_get_simtime (lua_State *L);
	static int oapi_get_simstep (lua_State *L);
	static int oapi_get_systime (lua_State *L);
	static int oapi_get_sysstep (lua_State *L);
	static int oapi_get_simmjd (lua_State *L);
	static int oapi_set_simmjd (lua_State *L);
	static int oapi_get_sysmjd (lua_State *L);
	static int oapi_time2mjd (lua_State *L);
	static int oapi_get_tacc (lua_State *L);
	static int oapi_set_tacc (lua_State *L);
	static int oapi_get_pause (lua_State *L);
	static int oapi_set_pause (lua_State *L);

	// Body functions
	static int oapi_get_mass (lua_State *L);
	static int oapi_get_size (lua_State *L);
	static int oapi_get_globalpos (lua_State *L);
	static int oapi_get_globalvel (lua_State *L);
	static int oapi_get_relativepos (lua_State *L);
	static int oapi_get_relativevel (lua_State *L);

	// Vessel functions
	static int oapi_get_propellanthandle (lua_State *L);
	static int oapi_get_propellantmass (lua_State *L);
	static int oapi_get_propellantmaxmass (lua_State *L);
	static int oapi_get_fuelmass (lua_State *L);
	static int oapi_get_maxfuelmass (lua_State *L);
	static int oapi_get_emptymass (lua_State *L);
	static int oapi_set_emptymass (lua_State *L);
	static int oapi_get_altitude (lua_State *L);
	static int oapi_get_pitch (lua_State *L);
	static int oapi_get_bank (lua_State *L);
	static int oapi_get_heading (lua_State *L);
	static int oapi_get_groundspeed (lua_State *L);
	static int oapi_get_groundspeedvector (lua_State *L);
	static int oapi_get_airspeed (lua_State *L);
	static int oapi_get_airspeedvector (lua_State *L);
	static int oapi_get_shipairspeedvector (lua_State *L);
	static int oapi_get_equpos (lua_State *L);
	static int oapi_get_atm (lua_State *L);
	static int oapi_get_induceddrag (lua_State *L);
	static int oapi_get_wavedrag (lua_State *L);

	// Navigation radio transmitter functions
	static int oapi_get_navpos (lua_State *L);
	static int oapi_get_navchannel (lua_State *L);
	static int oapi_get_navrange (lua_State *L);
	static int oapi_get_navdata (lua_State *L);
	static int oapi_get_navsignal (lua_State *L);
	static int oapi_get_navtype (lua_State *L);

	// Camera functions
	static int oapi_set_cameramode (lua_State *L);
	static int oapi_get_cameratarget (lua_State *L);
	static int oapi_set_cameratarget (lua_State *L);
	static int oapi_get_cameraaperture (lua_State *L);
	static int oapi_set_cameraaperture (lua_State *L);
	static int oapi_get_cameraglobalpos (lua_State *L);
	static int oapi_get_cameraglobaldir (lua_State *L);
	static int oapi_move_groundcamera (lua_State *L);

	// animation functions
	static int oapi_create_animationcomponent (lua_State *L);
	static int oapi_del_animationcomponent (lua_State *L);

	// instrument panel functions
	static int oapi_open_mfd (lua_State *L);
	static int oapi_set_hudmode (lua_State *L);
	static int oapi_set_panelblink (lua_State *L);

	// user i/o functions
	static int oapi_keydown (lua_State *L);
	static int oapi_resetkey (lua_State *L);
	static int oapi_simulatebufferedkey (lua_State *L);
	static int oapi_simulateimmediatekey (lua_State *L);

	// term library functions
	static int termOut (lua_State *L);

	// screen annotation library functions
	static int noteSetText (lua_State *L);
	static int noteSetPos (lua_State *L);
	static int noteSetSize (lua_State *L);
	static int noteSetColour (lua_State *L);

	// -------------------------------------------
	// vessel access functions
	// -------------------------------------------
	static int vesselGetHandle (lua_State *L);
	static int vesselGetFocusHandle (lua_State *L);
	static int vesselGetInterface (lua_State *L);
	static int vesselGetFocusInterface (lua_State *L);
	static int vesselGetCount (lua_State *L);

	// -------------------------------------------
	// vessel methods
	// -------------------------------------------
	static int vGetHandle (lua_State *L);
	static int vesselSendBufferedKey (lua_State *L);
	static int vesselGetGravityRef (lua_State *L);
	static int vesselGetSurfaceRef (lua_State *L);
	static int vesselGetAltitude (lua_State *L);
	static int vesselGetPitch (lua_State *L);
	static int vesselGetBank (lua_State *L);
	static int vesselGetYaw (lua_State *L);
	static int vesselGetAngularVel (lua_State *L);
	static int vesselSetAngularVel (lua_State *L);
	static int vesselGetElements (lua_State *L);
	static int vesselGetElementsEx (lua_State *L);
	static int vesselSetElements (lua_State *L);
	static int vesselGetProgradeDir (lua_State *L);
	static int vesselGetWeightVector (lua_State *L);
	static int vesselGetThrustVector (lua_State *L);
	static int vesselGetLiftVector (lua_State *L);
	static int v_is_landed (lua_State *L);
	static int v_get_groundcontact (lua_State *L);
	static int v_get_navmode (lua_State *L);
	static int v_set_navmode (lua_State *L);
	static int vesselGetRCSmode (lua_State *L);
	static int vesselSetRCSmode (lua_State *L);
	static int vesselGetADCmode (lua_State *L);
	static int vesselSetADCmode (lua_State *L);
	static int vesselGetADCLevel (lua_State *L);
	static int vesselSetADCLevel (lua_State *L);

	// propellant methods
	static int vesselCreatePropellantResource (lua_State *L);
	static int vesselDelPropellantResource (lua_State *L);
	static int vesselClearPropellantResources (lua_State *L);
	static int vesselGetPropellantCount (lua_State *L);
	static int vesselGetPropellantHandle (lua_State *L);
	static int vesselGetPropellantMaxMass (lua_State *L);
	static int vesselSetPropellantMaxMass (lua_State *L);
	static int vesselGetPropellantMass (lua_State *L);
	static int v_set_propellantmass (lua_State *L);
	static int v_get_totalpropellantmass (lua_State *L);
	static int v_get_propellantefficiency (lua_State *L);
	static int v_set_propellantefficiency (lua_State *L);
	static int v_get_propellantflowrate (lua_State *L);
	static int v_get_totalpropellantflowrate (lua_State *L);

	// thruster methods
	static int v_create_thruster (lua_State *L);
	static int v_del_thruster (lua_State *L);
	static int v_clear_thrusters (lua_State *L);
	static int v_get_thrustercount (lua_State *L);
	static int v_get_thrusterhandle (lua_State *L);
	static int v_get_thrusterresource (lua_State *L);
	static int v_set_thrusterresource (lua_State *L);
	static int v_get_thrusterpos (lua_State *L);
	static int v_set_thrusterpos (lua_State *L);
	static int v_get_thrusterdir (lua_State *L);
	static int v_set_thrusterdir (lua_State *L);
	static int v_get_thrustermax0 (lua_State *L);
	static int v_set_thrustermax0 (lua_State *L);
	static int v_get_thrustermax (lua_State *L);
	static int v_get_thrusterisp0 (lua_State *L);
	static int v_get_thrusterisp (lua_State *L);
	static int v_set_thrusterisp (lua_State *L);
	static int v_get_thrusterlevel (lua_State *L);
	static int v_set_thrusterlevel (lua_State *L);
	static int v_inc_thrusterlevel (lua_State *L);
	static int v_inc_thrusterlevel_singlestep (lua_State *L);

	// thruster group methods
	static int v_create_thrustergroup (lua_State *L);
	static int v_del_thrustergroup (lua_State *L);
	static int v_get_thrustergrouphandle (lua_State *L);
	static int v_get_thrustergrouphandlebyindex (lua_State *L);
	static int v_get_groupthrustercount (lua_State *L);
	static int v_get_groupthruster (lua_State *L);
	static int v_get_thrustergrouplevel (lua_State *L);
	static int v_set_thrustergrouplevel (lua_State *L);
	static int v_inc_thrustergrouplevel (lua_State *L);
	static int v_inc_thrustergrouplevel_singlestep (lua_State *L);

	// general vessel properties
	static int v_get_name (lua_State *L);
	static int v_get_classname (lua_State *L);
	static int v_get_flightmodel (lua_State *L);
	static int v_get_damagemodel (lua_State *L);
	static int v_get_enablefocus (lua_State *L);
	static int v_set_enablefocus (lua_State *L);
	static int v_get_size (lua_State *L);
	static int v_set_size (lua_State *L);
	static int v_get_emptymass (lua_State *L);
	static int v_set_emptymass (lua_State *L);
	static int v_get_pmi (lua_State *L);
	static int v_set_pmi (lua_State *L);
	static int v_get_crosssections (lua_State *L);
	static int v_set_crosssections (lua_State *L);
	static int v_get_gravitygradientdamping (lua_State *L);
	static int v_set_gravitygradientdamping (lua_State *L);
	static int v_get_touchdownpoints (lua_State *L);
	static int v_set_touchdownpoints (lua_State *L);
	static int v_set_visibilitylimit (lua_State *L);

	// vessel state
	static int v_get_mass (lua_State *L);
	static int v_get_globalpos (lua_State *L);
	static int v_get_globalvel (lua_State *L);
	static int v_get_relativepos (lua_State *L);
	static int v_get_relativevel (lua_State *L);
	static int v_get_rotationmatrix (lua_State *L);

	// atmospheric parameters
	static int v_get_atmref (lua_State *L);
	static int v_get_atmtemperature (lua_State *L);
	static int v_get_atmdensity (lua_State *L);
	static int v_get_atmpressure (lua_State *L);

	// aerodynamic state parameters
	static int v_get_dynpressure (lua_State *L);
	static int v_get_machnumber (lua_State *L);
	static int v_get_groundspeed (lua_State *L);
	static int v_get_groundspeedvector (lua_State *L);
	static int v_get_airspeed (lua_State *L);
	static int v_get_airspeedvector (lua_State *L);
	static int v_get_shipairspeedvector (lua_State *L);
	static int v_get_horizonairspeedvector (lua_State *L);
	static int v_get_aoa (lua_State *L);
	static int v_get_slipangle (lua_State *L);

	// airfoil methods
	static int v_create_airfoil (lua_State *L);
	static int v_del_airfoil (lua_State *L);
	static int v_create_controlsurface (lua_State *L);

	// aerodynamic properties (legacy model)
	static int v_get_cw (lua_State *L);
	static int v_set_cw (lua_State *L);
	static int v_get_wingaspect (lua_State *L);
	static int v_set_wingaspect (lua_State *L);
	static int v_get_wingeffectiveness (lua_State *L);
	static int v_set_wingeffectiveness (lua_State *L);
	static int v_get_rotdrag (lua_State *L);
	static int v_set_rotdrag (lua_State *L);
	static int v_get_pitchmomentscale (lua_State *L);
	static int v_set_pitchmomentscale (lua_State *L);
	static int v_get_yawmomentscale (lua_State *L);
	static int v_set_yawmomentscale (lua_State *L);
	static int v_get_trimscale (lua_State *L);
	static int v_set_trimscale (lua_State *L);

	// docking port management
	static int v_create_dock (lua_State *L);
	static int v_del_dock (lua_State *L);
	static int v_set_dockparams (lua_State *L);
	static int v_get_dockparams (lua_State *L);
	static int v_get_dockcount (lua_State *L);
	static int v_get_dockhandle (lua_State *L);
	static int v_get_dockstatus (lua_State *L);
	static int v_undock (lua_State *L);

	// attachment management
	static int v_create_attachment (lua_State *L);
	static int v_del_attachment (lua_State *L);
	static int v_clear_attachments (lua_State *L);
	static int v_set_attachmentparams (lua_State *L);
	static int v_get_attachmentparams (lua_State *L);
	static int v_get_attachmentid (lua_State *L);
	static int v_get_attachmentstatus (lua_State *L);
	static int v_get_attachmentcount (lua_State *L);
	static int v_get_attachmentindex (lua_State *L);
	static int v_get_attachmenthandle (lua_State *L);
	static int v_attach_child (lua_State *L);
	static int v_detach_child (lua_State *L);

	// navigation radio interface
	static int v_enable_transponder (lua_State *L);
	static int v_get_transponder (lua_State *L);
	static int v_set_transponderchannel (lua_State *L);
	static int v_enable_ids (lua_State *L);
	static int v_get_ids (lua_State *L);
	static int v_set_idschannel (lua_State *L);
	static int v_init_navradios (lua_State *L);
	static int v_get_navcount (lua_State *L);
	static int v_set_navchannel (lua_State *L);
	static int v_get_navchannel (lua_State *L);
	static int v_get_navsource (lua_State *L);

	// exhaust and reentry render options
	static int v_add_exhaust (lua_State *L);
	static int v_del_exhaust (lua_State *L);
	static int v_get_exhaustcount (lua_State *L);
	static int v_add_exhauststream (lua_State *L);

	// light source methods
	static int v_add_pointlight (lua_State *L);
	static int v_add_spotlight (lua_State *L);
	static int v_get_lightemitter (lua_State *L);
	static int v_get_lightemittercount (lua_State *L);
	static int v_del_lightemitter (lua_State *L);
	static int v_clear_lightemitters (lua_State *L);

	// camera management
	static int v_get_cameraoffset (lua_State *L);
	static int v_set_cameraoffset (lua_State *L);

	// mesh methods
	static int v_add_mesh (lua_State *L);
	static int v_insert_mesh (lua_State *L);
	static int v_del_mesh (lua_State *L);
	static int v_clear_meshes (lua_State *L);
	static int v_get_meshcount (lua_State *L);
	static int v_shift_mesh (lua_State *L);
	static int v_shift_meshes (lua_State *L);
	static int v_get_meshoffset (lua_State *L);

	// animation methods
	static int v_create_animation (lua_State *L);
	static int v_del_animation (lua_State *L);
	static int v_set_animation (lua_State *L);
	static int v_add_animationcomponent (lua_State *L);

	// -------------------------------------------
	// MFD methods
	// -------------------------------------------
	static int mfd_get_size (lua_State *L);
	static int mfd_set_title (lua_State *L);
	static int mfd_get_defaultpen (lua_State *L);
	static int mfd_get_defaultfont (lua_State *L);
	static int mfd_invalidate_display (lua_State *L);
	static int mfd_invalidate_buttons (lua_State *L);

	// -------------------------------------------
	// Light emitter methods
	// -------------------------------------------
	static int le_get_position (lua_State *L);
	static int le_set_position (lua_State *L);
	static int le_get_direction (lua_State *L);
	static int le_set_direction (lua_State *L);
	static int le_get_intensity (lua_State *L);
	static int le_set_intensity (lua_State *L);
	static int le_get_range (lua_State *L);
	static int le_set_range (lua_State *L);
	static int le_get_attenuation (lua_State *L);
	static int le_set_attenuation (lua_State *L);
	static int le_get_spotaperture (lua_State *L);
	static int le_set_spotaperture (lua_State *L);
	static int le_activate (lua_State *L);
	static int le_is_active (lua_State *L);

	// -------------------------------------------
	// Sketchpad methods
	// -------------------------------------------
	static int skp_text (lua_State *L);
	static int skp_moveto (lua_State *L);
	static int skp_lineto (lua_State *L);
	static int skp_line (lua_State *L);
	static int skp_rectangle (lua_State *L);
	static int skp_ellipse (lua_State *L);
	static int skp_polygon (lua_State *L);
	static int skp_polyline (lua_State *L);
	static int skp_set_origin (lua_State *L);
	static int skp_set_textalign (lua_State *L);
	static int skp_set_textcolor (lua_State *L);
	static int skp_set_backgroundcolor (lua_State *L);
	static int skp_set_backgroundmode (lua_State *L);
	static int skp_set_pen (lua_State *L);
	static int skp_set_font (lua_State *L);
	static int skp_get_charsize (lua_State *L);
	static int skp_get_textwidth (lua_State *L);

	friend int OpenHelp (void *context);

private:
	HANDLE hExecMutex; // flow control synchronisation
	HANDLE hWaitMutex;

	bool bExecLocal;   // flag for locally created mutexes
	bool bWaitLocal;

	static NOTEHANDLE hnote; // screen note (shared between all instances)
	int status;              // interpreter status
	bool is_busy;            // interpreter busy (running a script)
	int jobs;                // number of background jobs left over after command terminates
	int (*postfunc)(void*);
	void *postcontext;
};

#endif // !__INTERPRETER_H