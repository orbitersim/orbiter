// ======================================================================
//                     ORBITER SOFTWARE DEVELOPMENT KIT
//                  Copyright (C) 2001-2004 Martin Schweiger
//                           All rights reserved
// CelBodyAPI.h
// - CELBODY class interface
// - This is the base class for celestial body module implementations
//   (central stars, planets, moons, asteroids, etc.) Plugin modules
//   for celestial bodies define their functionality by deriving a class
//   from CELBODY and overloading the appropriate callback member
//   functions.
// - This class interface replaces the previous interface consisting of
//   global opcXXX callback functions.
// ======================================================================

/**
 * \file CelBodyAPI.h
 * \brief Contains interface classes for celestial bodies: \ref CELBODY and
 *   \ref CELBODY2.
 */

#ifndef __CELBODYAPI_H
#define __CELBODYAPI_H

// ===========================================================================
/// \ingroup defines
/// \defgroup ephem Ephemeris data format bitflags
///  Ephemeris data format bitflags
// ===========================================================================
//@{
#define EPHEM_TRUEPOS     0x01	///< true body position
#define EPHEM_TRUEVEL     0x02	///< true body velocity
#define EPHEM_BARYPOS     0x04	///< barycentric position
#define EPHEM_BARYVEL     0x08	///< barycentric velocity
#define EPHEM_BARYISTRUE  0x10	///< body has no child objects
#define EPHEM_PARENTBARY  0x20	///< ephemerides are computed in terms of the barycentre of the parent body's system
#define EPHEM_POLAR       0x40	///< data is returned in polar format
//@}

// Used for ephemeris interpolation
struct Sample {
	double t;
	double rad;
	double param[6];
};

	/**
	* \page planetmod Planet Modules
	*
	* \details Planet modules can be used to control the motion of a planet (or any other celestial body,
	*  such as a moon, the sun, or an asteroid) within the solar system. This allows to implement
	*  sophisticated analytic ephemerides solutions which take into account perturbations from other
	*  celestial objects.
	*
	*  Planets which are not controlled via a DLL module are updated directly by Orbiter. Depending
	*  on the settings in the definition file, Orbiter either uses an unperturbed 2-body approximation,
	*  resulting in a conic section trajectory (e.g. an ellipse), or uses a dynamic update procedure
	*  based on the gravitational forces acting on the planet. Both methods have limitations: the 2-
	*  body approach ignores perturbations and is only valid if no massive bodies other than the
	*  orbit reference object are nearby. The dynamic update accumulates numerical errors over
	*  time, causing the orbits slowly to diverge from the correct trajectories.
	*
	*  By using a planet module, analytic perturbation solutions can be used which avoid the shortcomings
	*  of the methods described above. Perturbation solutions typically describe the perturbed
	*  orbit of a planet by expressing the state vectors as a trigonometric series. These series
	*  are valid over a limited period of time, after which they start to diverge. Examples of perturbation
	*  solutions used in Orbiter are the VSOP87 solution for the 8 major planets and the sun, or
	*  the ELP2000 solution for the moon.
	*
	*  Planet modules have one additional function: They can be used to define some atmospheric
	*  parameters, such as temperature, pressure and density as a function of altitude. Additional
	*  functions may be added to the planet module interface in the future.
	*
	* \section first_steps First Steps:
	*  To start on your planet module, you should create a new "dynamic link library" project with
	*  your C++ compiler. Add the \e Orbiter.lib and \e Orbitersdk.lib files to the project (<i>found in
	*  Orbitersdk\\lib</i>). Add <i>Orbitersdk\\include</i> to your include path. Create a C++ source 
	*  file for your project, and add the essential API interface functions:
	*
	* \code
	*  #define ORBITER_MODULE
	*  #include "OrbiterAPI.h"
	*  #include "CelbodyAPI.h"
	*
	*  DLLCLBK void InitModule (HINSTANCE hModule)
	*  {
	*     // module initialisation
	*  }
	*
	*  DLLCLBK void ExitModule (HINSTANCE hModule)
	*  {
	*     // module cleanup
	*  }
	*
	*  DLLCKBK CELBODY *InitInstance (OBJHANDLE hBody)
	*  {
	*     // instance initialisation
	*     return new MyPlanet;
	*  }
	*
	*  DLLCLBK void ExitInstance (CELBODY *body)
	*  {
	*     // instance cleanup
	*     delete (MyPlanet*)body;
	*  }
	* \endcode
	*
	* The first line defining \c ORBITER_MODULE is required to ensure that all initialisation functions
	* are properly called by Orbiter.
	*
	* OrbiterAPI.h contains the general API interface, and CelBodyAPI.h contains the planet module-
	* specific interface, in particular the \c CELBODY class, which will be discussed below.
	*
	* The \e InitModule() and \e ExitModule() methods are called only once per Orbiter session, when the
	* DLL module is loaded or unloaded, respectively. They can be used to set up global parameters.
	* You can omit them if your module doesn't need any such initialisation.
	*
	* The \e InitInstance() and \e ExitInstance() functions are more important: You use them to create and
	* destroy an instance of your planet class. This class is derived from \c CELBODY. In this example,
	* we called it MyPlanet.
    *
	* \section class_interface The CELBODY interface class
	*
	* All communication between Orbiter and your planet module will be conducted via the methods
	* of the derived planet class. You overload the various callback functions of the \c CELBODY
	* class to add the required functionality. Check the API Reference manual for a complete list of
	* class methods. A typical implementation might look like this:
	* \code
	*  class MyPlanet: public CELBODY 
	*  {
	*  public:
	*         MyPlanet();
	*    bool bEphemeris() const;
	*    void clbkInit (FILEHANDLE cfg);
	*    int  clbkEphemeris (double mjd, int req, double *ret);
	*    int  clbkFastEphemeris (double simt, int req, double *ret);
	*  };
	* \endcode
	*
	* \code
	*  MyPlanet::MyPlanet(): CELBODY()
	*  {
	*     // add constructor code here
	*  }
	*
	*  bool MyPlanet::bEphemeris() const
	*  {
	*     return true; // class supports ephemeris calculation
	*  }
	*
	*  void MyPlanet::clbkInit (FILEHANDLE cfg)
	*  {
	*    // read parameters from config file (e.g. tolerance limits, etc)
	*    // perform any required initialisation (e.g. read perturbation terms from data files)
	*  }
	*
	*  int MyPlanet::clbkEphemeris (double mjd, int req, double *ret)
	*  {
	*    // return planet position and velocity for Modified Julian date mjd in ret
	*  }
	*
	*  int MyPlanet::clbkFastEphemeris (double simt, int req, double *ret)
	*  {
	*    // return interpolated planet position and velocity for simulation time simt in ret
	*  }
	* \endcode
	*
	* \e clbkEphemeris() and \e clbkFastEphemeris() are the functions which will contain the actual
	* ephemeris calculations for the planet at the requested time. \e clbkEphemeris() is only called by
	* Orbiter if the planet's state at an arbitrary time is required (for example by an instrument
	* calculating the position at some future time). When Orbiter updates the planet's position for
	* the next simulation time frame, the \e clbkFastEphemeris() function will be called instead. This
	* means that \e clbkFastEphemeris() will be called at each frame, each time advancing the time by
	* a small amount. This can be used for a more efficient calculation. Instead of performing a full
	* series evaluation, which can be lengthy, you may implement an interpolation scheme which
	* performs the full calculation only occasionally, and interpolates between these samples to
	* return the state at an intermediate time.
	*
	* For both functions, the requested type of data is specified as a group of \c EPHEM_xxx bitflags
	* in the req parameter. (see \c CELBODY) This can be any combination of position and velocity data for the
	* celesital body itself and/or the barycentre of the system defined by the body and all its
	* children (moons). The functions should calculate all required data, either in cartesian or polar
	* coordinates, and fill the ret array with the results. ret contains 12 entries, used as follows:\n
	*
	* ret[0-2]: true position\n
	* ret[3-5]: true velocity\n
	* ret[6-8]: barycentric position\n
	* ret[9-11]: barycentric velocity\n
	*
	* Only the fields requested by req need to be filled. In cartesian coordinates, the position fields
	* must contain the x, y and z coordinates in [m], and the velocity fields must contain the
	* velocities dx/dt, dy/dt, dz/dt in [m/s]. In spherical polar coordinates, the position fields must
	* contain longitude j [rad], latitude q [rad] and radial distance r [AU], and the velocity fields
	* must contain the polar velocities dj/dt [rad/s], dq/dt [rad/s] and dr/dt [AU/s].
	*
	* The functions should indicate the fields actually calculated via the return value. This is in
	* particular important if not all requests could be satisified (e.g. position and velocity was
	* requested, but only position could be calculated). The return value is interpreted as a bitflag
	* that can contain the same \c EPHEM_xxx flags as the req parameter. If all requests could be
	* satisfied, it should be identical to req. In addition, the return value should contain additional
	* flags indicating the properties of the returned data, including \c EPHEM_POLAR if the data are
	* returned as spherical polar coordinates, or \c EPHEM_TRUEISBARY if the true and barycentric
	* coordinates are identical (i.e. the celestial body does not have child bodies).
	*
	* \note The older standalone module callback functions (opcXXX) are obsolete and should no 
	*  longer be used.
	*
	* \sa CELBODY
	*/
	


// ======================================================================
/**
* \class CELBODY
* \brief This is the base class for celestial body classes.
* \details CELBODY defines callback methods which Orbiter will call whenever it requires
*  information from your planet module. You define the behaviour of the planet by overloading
*  the relevant methods. Below is a list of public CELBODY methods:
* \sa \ref planetmod
*/
// ======================================================================

class OAPIFUNC CELBODY {
public:
	CELBODY ();

	/**
	* \brief Return version number
	* \return Version number (1 for CELBODY, 2 for CELBODY2)
	*/
	inline int Version() const { return version; }

	/**
	* \brief Returns \e true or \e false depending on whether the module supports ephemeris calculation.
	* \return If your module supports ephemeris calculation (that is, if it defines the
	*  clbkEphemeris and clbkFastEphemeris methods) return \e true. Otherwise return \e false.
	* \par Default action: 
	*  Returns \e false.
	*/
	virtual bool bEphemeris() const;
	
	/**
	* \brief Called when the planet is initialised at the beginning of a simulation run.
	* \details This function allows to read any parameters from the configuration file, 
	*  and perform additional initialisation tasks such as reading data files.
	* \param cfg file handle of configuration file
	* \par Default action: 
	*  None.
	*/
	virtual void clbkInit (FILEHANDLE cfg);
	
	/**
	* \brief Called when Orbiter requires (non-sequential) ephemeris data from the planet
	*  for a given time.
	* \param mjd ephemeris date (days, in Modified Julian Date format)
	* \param req data request bitflags (see notes)
	* \param ret pointer to result vector
	* \return bitflags describing returned data (see notes)
	* \par Default action: 
	*  None, returning 0
	* \note The ephemeris data should be calculated with respect to the body's parent
	*  body, in the ecliptic frame (J2000 equator and equinox).
	* \note req specifies the data that should be calculated by the callback function. This
	*  can be any combination of \n
	* - \c EPHEM_TRUEPOS (true body position)
	* - \c EPHEM_TRUEVEL (true body velocity)
	* - \c EPHEM_BARYPOS (barycentric position)
	* - \c EPHEM_BARYVEL (barycentric velocity)
	* \note where the barycentre refers to the system consisting of the body itself and all
	* its children (e.g. moons).
	* \note ret is a pointer to an array of 12 doubles, to which the function should write
	*  its results:
	* - ret[0-2]: true position (if requested)
	* - ret[3-5]: true velocity (if requested)
	* - ret[6-8]: barycentric position (if requested)
	* - ret[9-11]: barycentric velocity (if requested)
	* \note Data can be returned in either polar or cartesian format. In cartesian format,
	*  the position data blocks should contain x,y and z position (in meters), and
	*  the velocity data blocks should contain dx/dt, dy/dt and dz/dt (in m/s), where
	*  x points to the vernal equinox, y points to ecliptic zenith, and z is orthogonal
	*  to both.
	* \note In polar format, the position data blocks should contain longitude j [rad],
	*  latitude q [rad] and radial distance r [AU], and the velocity data blocks should
	*  contain dj/dt [rad/s], dq/dt [rad/s] and d r/dt [AU/s].
	*  When returning data in polar format, include the \c EPHEM_POLAR flag in the
	*  return value.
	* \note The return value should contain the flags for the data that were actually
	*  computed. For example, if both true and barycentric data were requested,
	*  but the module can only compute true positions, it should return
	*  \c EPHEM_TRUEPOS | \c EPHEM_TRUEVEL.
	* \note If the true and barycentric positions are identical (that is, if the body has no
	*  child objects) the return value should contain the additional flag \c EPHEM_BARYISTRUE.
	* \note If both true and barycentric data are requested, but are computationally
	*  expensive to compute (for example, if they require two separate series
	*  evaluations), the module can return true positions only. Orbiter will then
	*  calculate the barycentric data directly, after evaluating the child object positions.
	* \note If a request can't be satisfied at all (e.g. if barycentric data were requested,
	*  but the module can only compute true positions), the module should
	*  calculate whatever data it can, and signal so via the return value. Orbiter will
	*  then try to convert these data to the required ones.
	* \note If the returned ephemerides are computed in terms of the barycentre of the
	*  parent body's system, the return value should include the
	*  \c EPHEM_PARENTBARY flag. If the ephemerides are computed in terms of the
	*  parent body's true position, this flag should not be included.
	* \note This function is not called by Orbiter to update the planet's position during
	*  the normal simulation frame update. (For that purpose, clbkFastEphemeris() is
	*  called instead). clbkEphemeris() is only called if the planet state at some
	*  arbitrary time point is required, e.g. by an instrument calculating a transfer orbit.
	*/
	virtual int clbkEphemeris (double mjd, int req, double *ret);
	
	/**
	* \brief Called by Orbiter to update the body's state to the next simulation frame.
	* \param simt simulation time (seconds)
	* \param req data request bitflags (see notes)
	* \param ret pointer to result vector
	* \return bitflags describing returned data (see notes)
	* \par Default action:
	*  None, returning 0
	* \note This function should perform the same function as clbkEphemeris(), but it will
	*  be called at each simulation frame. This means that the sampling times will
	*  be incremented in small steps, allowing for a potentially more efficient
	*  implementation, e.g. by using an interpolation scheme.
	* \note If possible, a full evaluation of a long series of perturbation terms should be
	*  avoided here, to avoid performance hits.
	* \note Note that the time parameter is passed in the form of simulation time
	*  (seconds) unlike clbkEphemeris(), which uses absolute MJD time. This avoids
	*  rounding errors in the time variable, and allows higher temporal resolutions.
	*/
	virtual int clbkFastEphemeris (double simt, int req, double *ret);
	
	/**
	* \brief Called by Orbiter to obtain atmospheric parameters at a given altitude.
	* \param alt altitude over planet mean radius
	* \param prm pointer to ATMPARAM structure receiving results
	* \return \e true if parameters have been retrieved sucessfully, \e false to indicate 
	*  that the planet has no atmosphere, or if alt is above the cutoff limit for atmospheric
	*  calculations.
	* \par Default action
	*  None, returning false.
	* \note The \c ATMPARAM structure contains the following fields:
	* \code 
	* typedef struct {      
	*   double T;      // temperature [K]
	*   double p;      // pressure [Pa]
	*   double rho;    // density [kg/m<sup>3</sup>]
	* } ATMPARAM; \endcode
	* \note Currently, atmospheric parameters are assumed to be functions of altitude
	*  only. Local variations ("weather") are not yet supported.
	*/
	virtual bool clbkAtmParam (double alt, ATMPARAM *prm);
	
protected:
	/**
	* \brief Convert from polar to cartesian coordinates
	*/
	void Pol2Crt (double *pol, double *crt);
	
	short version;	///< version number
};


// ======================================================================
/**
* \class CELBODY2
* \brief Extension to CELBODY class.
* \details This class introduces extended atmosphere support. It contains an
*   \ref ATMOSPHERE class instance which handles all atmosphere data requests.
*   The atmosphere class can be either defined directly in the celestial body's
*   plugin module, or it can be loaded from an external module. This latter option
*   allows to replace atmospheric models easily, without having to re-implement
*   other parts of the code, such as the ephemeris calculations.
* \sa CELBODY, ATMOSPHERE
*/
// ======================================================================

class OAPIFUNC CELBODY2: public CELBODY {
	friend class ATMOSPHERE;

public:
	/**
	 * \brief Constructor. Creates a CELBODY2 instance for a celestial body.
	 * \param hCBody body handle
	 */
	CELBODY2 (OBJHANDLE hCBody);

	/**
	 * \brief Destructor. Destroys the CELBODY2 instance.
	 * \default Calls the FreeAtmosphere method, to delete the atmosphere instance
	 *   and unload any external atmosphere modules.
	 */
	virtual ~CELBODY2();

	/**
	 * \brief Module initialisation from configuration file settings.
	 * \param cfg file handle for configuration file
	 * \default - Calls the base class \ref CELBODY::clbkInit method
	 * - If an atmosphere module is not already loaded, and if the configuration file
	 *   contains a MODULE_ATM entry, the \ref LoadAtmosphereModule method is called
	 *   with the corresponding module file name.
	 */
	virtual void clbkInit (FILEHANDLE cfg);

	/**
	 * \brief Returns the handle of the associated object.
	 */
	inline OBJHANDLE GetHandle() const { return hBody; }

	/**
	 * \brief Returns the handle for the parent body in the solar system hierarchy.
	 * \return Parent body handle, or NULL if no parent.
	 * \note For primary planets, this method returns a handle to the central
	 *   star. For moons, it returns a handle to the parent planet. For the
	 *   central star itself, it returns NULL.
	 */
	OBJHANDLE GetParent() const;

	/**
	 * \brief Returns for a child body in the solar system hierarchy.
	 * \param idx child body index (>= 0)
	 * \return Child body handle, or NULL if not available.
	 * \note For the central star, this returns the handles of the primary planets.
	 * \note For planets, it returns the handles of the moons.
	 * \note If idx >= number of children, the function returns NULL.
	 */
	OBJHANDLE GetChild (DWORD idx) const;

	/**
	 * \brief Returns the siderial period of the celestial body
	 * \return Siderial rotation period [s]
	 */
	double SidRotPeriod() const;

	/**
	 * \brief Returns the body's atmosphere instance.
	 * \return pointer to atmosphere object, or NULL if the body has no atmosphere.
	 * \note To provide an atmosphere for the body, the CELBODY2 object should
	 *   instantiate the atm member as an object of a derived \ref ATMOSPHERE class.
	 */
	inline ATMOSPHERE *GetAtmosphere() const { return atm; }

	/**
	 * \brief Flags the atmosphere interface version.
	 * \return \e false indicates that Orbiter should use the \ref ATMOSPHERE
	 *   object returned by \ref GetAtmosphere to query atmospheric parameters.
	 *   \e true indicates that Orbiter should use the CELBODY::clbkAtmParam
	 *   method instead.
	 * \note If the body does not have an atmosphere, this method should return
	 *   \e false, and \ref GetAtmosphere should return \e NULL.
	 * \sa GetAtmosphere, CELBODY::clbkAtmParam
	 */
	virtual bool LegacyAtmosphereInterface() const { return false; }

protected:
	/**
	 * \brief Assigns an atmosphere object for the celestial body.
	 * \param a pointer to ATMOSPHERE object
	 * \note Any previously defined atmosphere object is deallocated and replaced.
	 * \note a = NULL will eliminate the body's atmosphere.
	 * \note By default (prior to the first call to SetAtmosphere, a celestial
	 *   body does not have an atmosphere.
	 * \note Use this function if the atmosphere class is defined directly in the
	 *   celestial body's module. For example,
	 * \code
	 * class MyAtmosphere: public ATMOSPHERE
	 * {
	 *     MyAtmosphere(CELBODY2 *body): ATMOSPHERE(body)
	 *     {}
	 *     ...
	 * };
	 *
	 * class MyCelbody: public CELBODY2
	 * {
	 *     MyCelbody(OBJHANDLE body): CELBODY2 (body)
	 *     {
	 *         SetAtmosphere (new MyAtmosphere(this));
	 *         ...
	 *     }
	 *     ...
	 * };
	 * \endcode
	 * \note If the atmosphere class is defined in an external module, use the
	 *   \ref LoadAtmosphereModule method instead.
	 */
	void SetAtmosphere (ATMOSPHERE *a);

	/**
	 * \brief Remove the atmosphere instance.
	 * \return \e true on success, \e false on failure (no atmosphere defined).
	 * \note This method calls \ref FreeAtmosphereModule, if an external atmosphere
	 *   module is loaded. Otherwise, is just deletes the atm instance.
	 */
	bool FreeAtmosphere ();

	/**
	 * \brief Loads an atmosphere instance from a DLL plugin.
	 * \param fname DLL file name (excluding '.dll' extension and relative to 'Modules\' folder)
	 * \return \e true if atmosphere module could be loaded, \e false otherwise
	 * \note If successful, this method sets the hAtmModule member to the atmospheric
	 *   module instance handle, and sets the atm member by calling the CreateAtmosphere
	 *   function in the module. The CreateAtmosphere function has the following interface:
	 *   \code
	 *     ATMOSPHERE *CreateAtmosphere (CELBODY2 *cbody);
	 *   \endcode
	 */
	bool LoadAtmosphereModule (const char *fname);

	/**
	 * \brief Unload the current atmosphere module.
	 * \return \e true indicates success, \e false indicates failure (no module loaded)
	 * \note Before unloading the module, this function first deletes the atmosphere
	 *   instance by calling the module's DeleteAtmosphere function. The interface is
	 *   \code
	 *     void DeleteAtmosphere (ATMOSPHERE *atm);
	 *   \endcode
	 *   If this function is not found in the module, the atmosphere instance is deleted
	 *   directly.
     */
	bool FreeAtmosphereModule ();

	OBJHANDLE hBody;      ///< handle for the associated celestial body
	ATMOSPHERE *atm;      ///< pointer to atmosphere object
	HINSTANCE hAtmModule; ///< library handle for external atmosphere module
};


// ======================================================================
/**
* \class ATMOSPHERE
* \brief Defines the physical atmospheric properties for a celestial body.
* \sa CELBODY2
*/
// ======================================================================

class OAPIFUNC ATMOSPHERE {
public:
	/**
	 * \brief Constructor. Creates an atmosphere instance for 'body'.
	 * \param body pointer to celestial body
	 */
	ATMOSPHERE (CELBODY2 *body);

	/**
	 * \brief A brief name that identifies the atmosphere model.
	 * \return Pointer to persistent string buffer that contains the model name.
	 * \note The returned name should not be longer than approx. 10 characters.
	 */
	virtual const char *clbkName() const = 0;

	/**
	 * \brief Parameter flags for atmospheric data input
	 * \sa ATMPRM_IN
	 */
	enum PRM_IN_FLAG {
		PRM_ALT=0x0001,   ///< altitude valid (otherwise use alt=0)
		PRM_LNG=0x0002,   ///< longitude valid (otherwise use lng=0)
		PRM_LAT=0x0004,   ///< latitude valid (otherwise use lat=0)
		PRM_FBR=0x0008,   ///< average flux valid (otherwise use f107avg=140)
		PRM_F  =0x0010,   ///< current flux valid (otherwise use f107=f107avg)
		PRM_AP =0x0020    ///< geomagnetic index valid (otherwise use ap=3)
	};

	/**
	 * \brief Input parameters for atmospheric data calculation
	 * \sa clbkAtmParam
	 */
	struct PRM_IN {
		double alt;       ///< altitude [m]
		double lng;       ///< longitude [rad]
		double lat;       ///< latitude [rad]
		double f107bar;   ///< average F10.7 flux over recent period
		double f107;      ///< current F10.7 flux
		double ap;        ///< magnetic index
		DWORD  flag;      ///< parameter flags (see \ref PRM_IN_FLAG)
	};

	/**
	 * \brief Output parameters for atmospheric data calculation
	 * \sa clbkAtmParam
	 */
	struct PRM_OUT {
		double T;         ///< temperature [K]
		double p;         ///< pressure [Pa]
		double rho;       ///< density [kg/m^3]
	};

	/**
	 * \brief Returns some general properties of the atmosphere.
	 * \param atmc pointer to structure to be filled by clbkConstants
	 * \return \e true if paramters were supplied, \e false otherwise.
	 * \default Sets the following structure entries to default values:
	 *    - atmc->R = 286.91
     *    - atmc->gamma = 1.4
	 *   but leaves the other values unchanged. Returns \e false.
	 * \note This function should be overloaded to provide appropriate basic
	 *   physical atmospheric properties, such as sea level density and pressure,
	 *   gas constant, cutoff altitude, as well as rendering colour and rendering
	 *   altitude.
	 * \note For complex atmospheric models, some of the parameters in the
	 *   ATMCONST structure may not be constants (e.g. ground density and
	 *   pressure. In that case, the return values should be reasonable mean
	 *   values.
	 * \note Some of these values may be overwritten by configuration file
	 *   settings.
	 */
	virtual bool clbkConstants (ATMCONST *atmc) const;

	/**
	 * \brief Called by Orbiter to obtain atmospheric parameters for a given set
	 *   of input parameters at the current simulation time.
	 * \param prm_in input parameters for atmospheric data calculation (see \ref PRM_IN)
	 * \param prm_out returned data (see \ref PRM_OUT)
	 * \return \e true if atmospheric data were calculated and returned,
	 *   \e false if the planet has no atmosphere or if the specified
	 *   position is outside the supported distance of the atmospheric
	 *   model.
	 * \default None, returns \e false.
	 */
	virtual bool clbkParams (const PRM_IN *prm_in, PRM_OUT *prm_out);

protected:
	CELBODY2 *cbody; ///< associated celestial body instance
};

#endif // !__CELBODYAPI_H