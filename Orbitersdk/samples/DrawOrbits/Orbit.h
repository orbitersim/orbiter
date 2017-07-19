
// =================================================================================================================================
//
// Copyright (C) 2016 Jarmo Nikkanen
//
// Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation 
// files (the "Software"), to use, copy, modify, merge, publish, distribute, interact with the Software and sublicense
// copies of the Software, subject to the following conditions:
//
// a) You do not sell, rent or auction the Software.
// b) You do not collect distribution fees.
// c) You do not remove or alter any copyright notices contained within the Software.
// d) This copyright notice must be included in all copies or substantial portions of the Software.
//
// If the Software is distributed in an object code form then in addition to conditions above:
// e) It must inform that the source code is available and how to obtain it.
// f) It must display "NO WARRANTY" and "DISCLAIMER OF LIABILITY" statements on behalf of all contributors like the one below.
//
// The accompanying materials such as artwork, if any, are provided under the terms of this license unless otherwise noted. 
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
// OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
// LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR
// IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
// =================================================================================================================================

#include "OrbiterAPI.h"
#include "VesselAPI.h"
#include "Tools.h"
#include "gcAPI.h"


using namespace oapi;

class COrbit : public sElements
{

public:

	/*! \details a Null constructor */
	COrbit();

	/*! \details a Copy constructor */
	COrbit(const COrbit &in);

	/*! \details Orbit construction from state vectors */
	COrbit(const VECTOR3 _pos, const VECTOR3 _vel, double mu, double epoch);

	/*! \details Orbit construction from OAPI handles */
	COrbit(OBJHANDLE hObj, OBJHANDLE hRef);

	/*! \details orbit class destructor*/
	~COrbit();

	/*! @name Orbit Creation and Reference Frames */
	//@{
	/*!	\details Create right-handed orbit using OAPI handle */
	void			Create(OBJHANDLE hObj, OBJHANDLE hRef);

	/*!	\details Create right-handed orbit using OAPI vessel */
	void			Create(VESSEL *hVes);

	/*!	\details Create and update an orbit from state vectors */
	void			CreateFromStateVectors(const VECTOR3 &_Pos, const VECTOR3 &_Vel, double Mu, double Epoch_MJD);

	/*! \details Create an Orbit using oapi::ELEMENTS data type. Before using this function ReferenceFrame() must be set
	\param Elem Pointer into ELEMENTS
	\param Mu Gravitational parameter (GM)
	\param Epoch_MJD Epoch MJD */
	void			CreateFromElements(const ELEMENTS *Elem, double Mu, double Epoch_MJD);

	/*! \details Create an Orbit from orbital elements.
	ReferenceFrame() must be configured before using this call. */
	void			CreateFromElements(double SMa, double Ecc, double Inc, double LAN, double AgP, double MnA, double Mu, double Epoch_MJD);

	void			CreateCircular(const VECTOR3 &_Pos, const VECTOR3 &_N, double Mu, double Epoch_MJD);

	void			EscapeOrbit(const VECTOR3 &_Pos, const VECTOR3 &_EV, double Mu, double Epoch_MJD, double Dir);

	void			ApproachOrbit(const VECTOR3 &_Pos, const VECTOR3 &_Pe, double Mu, double Epoch_MJD, double Dir);

	/*! \details Define external reference frame.\n\n
	Required By: Inc(), LAN(), AgP(), LPe(), TrL(), MnL(), CreateFromElements()\n\n
	The reference frame is set to Ecliptic by default
	\code
	Vessel.ReferencePole(_I_ECL, _K_ECL);	// Set into the Ecliptic
	Vessel.ReferencePole(_I_ECL, _K_EQU);	// Set into the Celestial Equator
	Vessel.ReferencePole(SunTransit(hPlanet), RotationAxis(hPlanet));  // a Local Equator
	\endcode */

	void			ReferencePole(const VECTOR3 _Equinox, const VECTOR3 _Pole);

	//@}

	double			PeMJD() const;

	/*! \details Compute the time of travel from current orbit position (epoch)
	into a specific true anomaly (TrA) using the short way which can be negative 
	\sa TimeToRelTrA */
	double			TimeToTrA(double tra) const;

	/*! \details Compute time to Eccentric anomaly (EcA) using the short way which can be negative
	\sa TimeToTrA*/
	double			TimeToEcA(double eca) const;

	/*! \details Compute Orbit True Anomaly TrA by MJD */
	double			TrAByMJD(double MJD) const;

	/*! \details Compute Orbit Eccentric Anomaly EcA by MJD */
	double			EcAByMJD(double MJD) const;

	/*! \details Compute Orbit True Anomaly (TrA) by Time
	where Time is in a seconds from current orbit position */
	double			TrAByTime(double Time) const;

	/*! \details Compute Orbit Eccentric Anomaly (EcA) by Time
	where Time is in a seconds from current orbit position */
	double			EcAByTime(double Time) const;

	/*! \details Compute orbit position vector by MJD */
	VECTOR3			PosByMJD(double MJD) const;

	/*! \details Compute orbit position vector by TrA */
	VECTOR3			PosByTrA(double tra) const;

	/*! \details Compute orbit position vector by EcA */
	VECTOR3			PosByEcA(double eca) const;

	/*! \details Compute orbit position vector in local perifocal system */
	FVECTOR2		PQPosByTrA(double tra) const;

	/*! \details Compute orbit velocity vector by MJD */
	VECTOR3			VelByMJD(double MJD) const;

	/*! \details Compute orbit velocity vector by TrA */
	VECTOR3			VelByTrA(double tra) const;

	/*! \details Compute orbit velocity vector by EcA */
	VECTOR3			VelByEcA(double eca) const;

	/*!	\details Get flight heading by true anomaly */
	double			Heading(double TrA, VECTOR3 &_Rot) const;

	/*! \details Get Orbit State vectors by MJD */
	void			PosVelByMJD(double mjd, VECTOR3 *_pos, VECTOR3 *_vel) const;

	/*!	\details Get MJD by True Anomaly relative to current epoch.
	Input TrA is not limited in 2PI. Positive value will take
	forward in time and negative value backwards.
	\param tra True Anomaly relative to current epoch.
	\return Modified Julian Date
	*/
	double			MJDByRelTrA(double tra) const;

	/*!	\details Get Total TrA travelled since epoch. TrA is not limited in 2PI
	\param mjd Modified Julian Date
	\return Travelled angle since epoch
	*/
	double			RelTrAByMJD(double mjd) const;

	/*!	\details Get flight time by True Anomaly relative to current position.
	Input TrA is not limited in 2PI. Positive value will take
	forward in time and negative value backwards.
	\param tra True Anomaly relative to current position.
	\return Modified Julian Date
	*/
	double			TimeToRelTrA(double tra) const;

	/*!	\details Get relative TrA by time. TrA output is not limited in 2PI
	\param time time from current point
	\return Travelled angle
	*/
	double			RelTrAByTime(double time) const;

	/*! \details Get TrA of vector's projection */
	double			TrAOfProjection(const VECTOR3 _Vector) const;

	/*! \details Get TrA of ascending node computed by using target plane _Normal */
	double			TrAOfAscendingNode(const VECTOR3 _Normal) const;

	/*!	\details Get True Anomaly by radius
	\param rad Orbit Radius
	\return True Anomaly [0, PI] or -1.0 if out of range */
	double			TrAByRadius(double rad) const;

	/*!	\details Get orbit radius by TrA
	\param TrA True Anomaly
	\return Orbit radius */
	double			RadiusByTrA(double TrA) const;

	/*!	\details Get orbit radius by EcA
	\param EcA Eccentric Anomaly
	\return Orbit radius */
	double			RadiusByEcA(double EcA) const;

	/*!	\details Get maximum valid NrA for hyperbolic orbit
	\return Maximum NrA [0, PI] or PI2 if the orbit is elliptic */
	double			MaxNrA() const;

	bool			IsTrAValid(double TrA) const;

	/*!	\details Get flight path angle */
	double			FlightPathAngle(double TrA) const;

	/*! \details Get Escape velocity
	\return Velocity in infinity or -1.0 if orbit is not hyperbolic */
	double			EscapeVelocity() const;

	/*! @name Orbital Elements at Epoch */
	//@{
	double			Vel() const;								//!< \details Current Velocity
	double			Rad() const;								//!< \details Current Radius
	double			Inc() const;								//!< \details Orbit Inclination \sa ReferenceFrame
	double			LAN() const;								//!< \details Longitude of Ascending node \sa ReferenceFrame
	double			AgP() const;								//!< \details Argument of Periapis \sa ReferenceFrame
	double			ApT() const;								//!< \details Orbital Period
	double			PeT() const;								//!< \details Orbital Period
	double			PeV() const;								//!< \details Periapis Velocity
	inline double   Par() const { return par; }					//!< \details Orbital Parameter
	inline double   SMi() const { return smi; }					//!< \details Semi-Minor Axis
	inline double   SMa() const { return sma; }					//!< \details Semi-Major Axis
	inline double   Ecc() const { return ecc; }					//!< \details Orbit Eccentricity
	inline double	OPe() const { return PI2 / mnm; }			//!< \details Orbital Period
	inline double	ApD() const { return sma * (1.0 + ecc); }	//!< \details Apoapis Distance
	inline double	PeD() const { return sma * (1.0 - ecc); }	//!< \details Periapis Distance
	inline double	EcA() const { return eca; }					//!< \details Eccentric Anomaly
	inline double	TrA() const { return eca2tra(eca, ecc); }	//!< \details True Anomaly
	inline double	MnA() const { return mna; }					//!< \details Mean Anomaly
	inline double	MnL() const { return limit(mna + LPe()); }  //!< \details Mean Longitude \sa ReferenceFrame
	inline double	TrL() const { return limit(TrA() + LPe()); }//!< \details True Longitude \sa ReferenceFrame
	inline double	LPe() const { return limit(AgP() + LAN()); }//!< \details Longitude of Periapis \sa ReferenceFrame
	inline double   FpA() const { return eca2fpa(eca, ecc); }	//!< \details Flight Path Angle
	inline double   NrA() const { return eca2nra(eca, ecc); }	//!< \details Get "Normal" Anomaly
	inline double Epoch() const { return epoch; }				//!< \details Epoch of the Elements in MJD
	//@}

	/*! \details Get current epoch position */
	inline VECTOR3	Position() const { return PosByEcA(eca); }

	/*! \details Get current epoch velocity */
	inline VECTOR3	Velocity() const { return VelByEcA(eca); }

	/*! \details Get current reference pole */
	inline VECTOR3  GetPole() const { return _Pol; }

private:

	VECTOR3 _Equ;	//!< Reference Equinox (i.e. _I_ECL)
	VECTOR3 _Aux;	//!< Perpendicular to _Equ and _Pol
	VECTOR3 _Pol;	//!< Reference Pole (i.e. _K_ECL)
};

