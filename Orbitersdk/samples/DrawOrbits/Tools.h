
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

#ifndef __TOOLS_H
#define __TOOLS_H

#include <float.h>
#include <math.h>
#include "OrbiterAPI.h"

/*
Notes:

_J = crossp( _K, _I);
_K = crossp( _I, _J);
_I = crossp( _J, _K);

_AN = crossp( _RefN,  _OrbN);
*/

#define J2000		51544.5			//!< Reference Epoch
#define J2000_Ob    0.40909280422   //!< Obliquity of J2000 (rad) 23°26'21.448"

#define _I_ECL	_V(1,  0,  0)	//!< [LH] Reference equinox
#define _K_ECL	_V(0,  1,  0)	//!< [LH] Reference pole
#define _J_ECL	_V(0,  0,  1)	//!< [LH] Perpendicular to both

#define _I_EQU	_I_ECL
#define _K_EQU	_V(0, 0.9174820621, 0.3977771559) //!< [LH] Mean Earth Equator of J2000
#define _J_EQU	crossp_LH(_K_EQU, _I_EQU)

#define OPDAY   1.157407407407407407407407407407e-5	//!< 1/86400
#define PI      3.141592653589793238462643383279
#define PI2		6.283185307179586476925286766559
#define PI05    1.570796326794896619231321691639
#define PI15    4.712388980384689857693965074919
#define PI120   2.094395102393195492308428922186	//!< 120 degrees
#define SQR2    1.414213562373095048801688724209	//!< sqrt(2)
#define OPSQR2  0.707106781186547524400844362104	//!< 1/sqrt(2)
#define ARCS    4.848136811095359935899141023579e-6	//!< Arcsecond in radians

#define EPSILON 4.440892098500626e-16

#define GMsun	1.32712440018e20
#define AU		1.49597870691e11
#define GC      6.67259e-11


inline double SIGN(double &x) { if (x >= 0.0) return 1.0; return -1.0; }



/*! \struct sElements
\brief Elements data structure
\details This structure is used to store orbit information.
Orientation of the orbit is defined by _P, _Q and _W vectors in J2000 Ecliptic frame of reference.
Use ExtElem() to acquire inc, lan and agp for selected reference frame.
\note
positive \e x - axis is pointing in vernal equinox\n
positive \e z - axis is pointing ecliptic zenith\n
positive \e y - azis is perpendicular to both by the right-hand rule
*/

struct sElements {

	VECTOR3 _P;				//!< [unit] Major Axis TrA=0
	VECTOR3 _Q;				//!< [unit] Minor Axis TrA=90
	VECTOR3 _W;				//!< [unit] Orbit Normal

	double  mu;				//!< Gravitational parameter (GM)
	double  mna;			//!< Mean Anomaly at epoch
	double	eca;			//!< Eccentric Anomaly at Epoch
	double  mnm;			//!< Mean motion
	double  sma;			//!< Semi-major axis 
	double  smi;			//!< Semi-minor axis
	double  par;			//!< Parameter
	double  ecc;			//!< Eccentricity
	double  epoch;			//!< Epoch MJD
	double  up;				//!< sqrt( mu / par );
};

// -------------------------------------------------------------------------
/*! \defgroup auxfn Auxiliary functions
\note functions are independed form left/right-handiness */
// -------------------------------------------------------------------------
//@{

/*! \details A distance from one angular position into an other */
inline double _ad(double f, double t)
{
	double n; if (f<t) n = t - f; else n = PI2 - (f - t);	return(n);
}


/*!	\details Return angular error between 'f' and 't' in range [-PI to PI]. Parameters 'f' and 't' must
be within [-PI2 to 2PI] */
/*
inline double _aerr(double f, double t)
{
	if (f<0) f += PI2; if (t<0) t += PI2;
	double n = t - f;
	if (n>PI)  n -= PI2;
	if (n<-PI) n += PI2;
	return n;
}*/

inline VECTOR3 crossp_LH(const VECTOR3 &r, const VECTOR3 &v)
{
	return _V(v.y*r.z - r.y*v.z, v.z*r.x - r.z*v.x, v.x*r.y - r.x*v.y);
}

/*! \details Limit the input between [0 to 2PI] (i.e. Get modulus of 2PI) */
inline double limit(double x)
{
	if (x>PI2) return fmod(x, PI2); if (x<0) return PI2 - fmod(-x, PI2); return x;
}

/*! \details Square Length */
inline double sqrlen(const VECTOR3 &v)
{
	return (v.x*v.x + v.y*v.y + v.z*v.z);
}

/*! \details Return unit vector. Normalize vector. */
/*
inline VECTOR3 unit(const VECTOR3 &v)
{
double l = v.x*v.x + v.y*v.y + v.z*v.z;
if (l==1.0) return v; if (l==0.0) return _V(0,0,0);
return v * (1.0/sqrt(l));
}*/


/*! \details Return angle between two vectors */
inline double angle(const VECTOR3 &v, const VECTOR3 &h)
{
	double x = dotp(unit(v), unit(h));
	if (x >= 1.0)  return 0.0; else if (x <= -1.0) return PI;
	return(acos(x));
}

/*! \details Return angle between two unit vectors */
inline double anglen(const VECTOR3 &v, const VECTOR3 &h)
{
	double x = dotp(v, h);
	if (x >= 1.0)  return 0.0; else if (x <= -1.0) return PI;
	return(acos(x));
}

/*! \details Hyperbolic arcus cosine */
inline double acosh(double x)
{
	return(log(x + sqrt(x*x - 1)));
}

/*! \details Hyperbolic arcus sine */
inline double asinh(double x)
{
	return(log(x + sqrt(x*x + 1)));
}

inline double cot(double x)
{
	return 1.0 / tan(x);
}

/*!
* \details Return handiness independent angle of the vector "_p" measured in a plane defined by _I and _J.
*  The _I and _J vectors must be perpendicular to each other.
* \param _p A vector to be projected into the plane defined by _Equinox and _Pole.
* \param _I Defines a zero orientation/position
* \param _J Defines auxiliary direction, 90 deg, perpendicular to "_I"
* \return Angle between _p and _Zero with-in a range of [0 to 2PI]
* \note With in this function parameters \e _Zero and \e _Ninety are internaly normalized.
If they are allreay normalized use GetAngle2() instead.
*/
inline double GetAngle(const VECTOR3 &_p, const VECTOR3 &_I, const VECTOR3 &_J)
{
	double x = atan2(dotp(_p, unit(_J)), dotp(_p, unit(_I)));
	if (x<0) return PI2 + x;
	return x;
}

/*! \details This is same as GetAngle() except that _I and _J
inputs are not normalized internally, must be normalized externally. Vector _p doesn't require normalization.
*/
inline double GetAngleN(const VECTOR3 &_p, const VECTOR3 &_I, const VECTOR3 &_J)
{
	double x = atan2(dotp(_p, _J), dotp(_p, _I));
	if (x<0) return PI2 + x; return x;
}

/*!
* \details Return a longitude of the vector "_p" measured in a plane defined by "_Equinox" and "_Pole".
*  The "_Equinox" and "_Pole" must be perpendicular to each other. The longitude is returned in counter-clockwice
*  from _Equinox by the \b right-hand rule.
* \param _p A vector to be projected into the plane defined by _Equinox and _Pole.
* \param _Equinox Defines a zero orientation/position
* \param _Pole Defines the normal vector of the plane
* \return Angle between _p and _Equinox with-in a range of [0 to 2PI]
* \note With in this function parameters \e _Equinox and \e _Pole are internaly normalized.
If they are allreay normalized use Longitude2() instead.
*/
inline double Longitude(const VECTOR3 &_p, const VECTOR3 &_I, const VECTOR3 &_K)
{
	VECTOR3 _J = crossp_LH(_K, _I); return GetAngle(_p, _I, _J);
}


/*! \details Create a unit vector using _I and _J vectors
*/
inline VECTOR3 CreateVector(const VECTOR3 &_I, const VECTOR3 &_J, double angle)
{
	return unit(_I)*cos(angle) + unit(_J)*sin(angle);
}

/*! \details Create a unit vector using _K and _I vectors
*/
inline VECTOR3 CreateVector2(const VECTOR3 &_K, const VECTOR3 &_I, double angle)
{
	return unit(_I)*cos(angle) + unit(crossp_LH(_K, _I))*sin(angle);
}


double  CalculateSOI(OBJHANDLE hObj, OBJHANDLE hRef);
const char *ValueToText(double real, int digits);
const char *AngleToText(double deg, int digits);

/*!
* \details Acquire perifocal frame vectors _p, _q and _w.
* \param LAN Longitude of ascending node
* \param Inc Inclination
* \param AgP Argument of periapis
* \param _i  Reference Equinox
* \param _k  Reference Pole
* \param _p  [out] [unit] Vector pointing periapis
* \param _q  [out] [unit] Perpendicular to _p and orbit normal
*/
void  PQW(double LAN, double Inc, double AgP, const VECTOR3 &_i, const VECTOR3 &_k, VECTOR3 *_p, VECTOR3 *_q);


/*!
* \details Get a burn time.
* \param dv delta velocity.
* \param vessel a Pointer into a vessel executing the burn.
* \param engine Thruster group type.
* \return burn time
*/
double  BurnTimeBydV(double dv, VESSEL *vessel, THGROUP_TYPE engine);

/*!
* \details Get delta velocity by burn time.
* \param time burn time.
* \param vessel a Pointer into a vessel executing the burn.
* \param engine Thruster group type.
* \return Delta velocity
*/
double  dVByBurnTime(double time, VESSEL *vessel, THGROUP_TYPE engine);

/*!
* \details Get a burn time.
* \param dv Delta velocity.
* \param mass Total mass of the vessel [kg]
* \param thr Thrust level [newtons]
* \param rate Fuel consumption rate [kg/s].
* \return burn time
*/
double  BurnTimeBydV(double dv, double mass, double thr, double rate);

/*!
* \details Get delta velocity from burn time
* \param time Burn time
* \param mass Total mass of the vessel [kg]
* \param thr Thrust level [newtons]
* \param rate Fuel consumption rate [kg/s].
* \return Delta velocity
*/
double  dVByBurnTime(double time, double mass, double thr, double rate);





// -------------------------------------------------------------------------
/*! \defgroup oapiext Orbiter API extensions */
// -------------------------------------------------------------------------
//@{
/*!
* \details Get thruster group ISP. The group may contain thrusters with different
*  Thrust and ISP ratings. Return value includes fuel efficiency factor.
* \param vessel a Pointer into the Vessel
* \param thgt a Thruster group type
* \return ISP of thruster group
*/
double  GetThrusterGroupISP(VESSEL *vessel, THGROUP_TYPE thgt);

/*!
* \details Get thruster group exhaust direction. If the thrusters are not paraller the
*  length of the vestor is less than 1.0
* \param vessel a Pointer into the Vessel
* \param thgt a Thruster group type
* \return [LH] Thrust direction vector (<i>in local vessel coordinates</i>)
*/
VECTOR3 GetThrusterGroupDir_LH(VESSEL *vessel, THGROUP_TYPE thgt);

/*!
* \details Get thruster group flow rate. Return value includes efficiency factor
* \param vessel a Pointer into the Vessel
* \param thgt a Thruster group type
* \return Thruster group flow rate at full thrust
*/
double  GetThrusterGroupFlowRate(VESSEL *vessel, THGROUP_TYPE thgt);

/*!
* \details Get thruster group maximum thrust vector in Newtons. Thrusters don't need to be paraller.
* \param vessel a Pointer into the Vessel
* \param thgt a Thruster group type
* \return [LH] Thrust vector in newtons at full thrust in global J2000 ecliptic frame of reference
*/
VECTOR3 GetThrusterGroupThrustVector_LH(VESSEL *vessel, THGROUP_TYPE thgt);

/*!
* \details Get a super structure. A list of all connected vessels.
* \param hVessel A vessel that belongs into the super structure.
* \param Count [out] Number of vessel beloning into the super structure.
* \return A Pointer into a list contining handles of all those vessels.
* \note User must delete the list when no longer needed.
* \code
*  OBJHANDLE *List = GetSuperStructure(hVessel, Count);
*  for (int i=0;i<Count;i++) mass += oapiGetMass(List[i]);
*  delete List;
* \endcode
*/
OBJHANDLE * GetSuperStructure(VESSEL *hVessel, int &Count);

/*!
* \details Get a mass of entire super-structure (multible docked vessels).
* \param vessel A Pointer into the vessel
* \return Mass of the superstructure
*/
double	GetSuperStructureMass(VESSEL *vessel);

/*!
* \details Get thruster ISP. Return value includes efficiency factor
* \param vessel a Pointer into the Vessel
* \param th a Thruster handle
* \return ISP
*/
double  GetISP(VESSEL *vessel, THRUSTER_HANDLE th);

/*!
* \details Get acceleration of the vessel at maximum thrust using defined thruster group
* \param vessel a Pointer into the Vessel
* \param th a Thruster group type
* \return Acceleration
*/
double  GetThrusterGroupAcceleration(VESSEL *ship, THGROUP_TYPE engine);

//@}




// -------------------------------------------------------------------------
/*!
* \defgroup angles Conversions of Angles
* \details Conversion of orbit related angles for Circular, Elliptic and Hyperbolic orbits
* \image html Ellipse.jpg
*
* \param MnA Mean Anomaly
* \param TrA True Anomaly
* \param EcA Eccentric Anomaly
* \param FpA Flight path angle
* \param NrA Angle between normal and major axis
* \param ecc Orbit eccentricity
*
* - Valid range of Flight-path angle (FpA) depends from orbit eccentricity
*   but is quaranteed to be within [-PI/2, +PI/2] in all scenarios.
*
*<b>If the orbit is elliptic:</b>
* - Valid range of MnA, EcA, TrA and NrA is [0, 2PI]
*
*<b>If the orbit is hyperbolic:</b>
* - Range of eccentric anomaly is [-inf, +inf]
* - Range of mean anomaly is [-inf, +inf]
* - Valid range of TrA and NrA depends from orbit eccentricity
*   but is quaranteed to be within [0, 2PI]
*
*<b>If the orbit is circular:</b>
* - FpA is always zero
* - MnA, EcA, TrA, NrA are all equal and in range [0, 2PI]
*
* \sa limit
*/
// -------------------------------------------------------------------------
//@{
/*! \details Convert Mean Anomaly to Eccentric Anomaly */
double	mna2eca(double mna, double ecc);

/*! \details Convert Eccentric Anomaly to Mean Anomaly */
double	eca2mna(double eca, double ecc);

/*! \details Convert True Anomaly to Eccentric Anomaly */
double	tra2eca(double tra, double ecc);

/*! \details Convert Eccentric Anomaly to True Anomaly */
double	eca2tra(double eca, double ecc);

/*! \details Convert True Anomaly to Mean Anomaly */
double	tra2mna(double tra, double ecc);

/*! \details Convert Mean Anomaly to True Anomaly */
double	mna2tra(double mna, double ecc);

/*! \details Convert Flight-Path Angle to Eccentric Anomaly */
double	fpa2eca(double fpa, double ecc);

/*! \details Convert Eccentric Anomaly to Flight-path Angle */
double	eca2fpa(double eca, double ecc);

/*! \details Convert Angle of surface normal to Eccentric Anomaly */
double	nra2eca(double nra, double ecc);

/*! \details Convert Eccentric Anomaly to Angle of surface normal */
double	eca2nra(double eca, double ecc);
//@}


// -------------------------------------------------------------------------
/*! \defgroup planet Planet and surface related functions
\note Funcrions are right-handed */
// -------------------------------------------------------------------------
//@{

/*!	\details SunTransit */
VECTOR3 SunTransit(OBJHANDLE hRef);

/*!
* \details Get a rotation axis or pole of a planet by right-hand rule
* \param hRef handle of a planet or moon
* \return Rotation axis [normalized]
*/
VECTOR3 RotationAxis(OBJHANDLE hRef);

/*!
* \details Get planet's meridian axis of date
* \param hRef handle of a planet or moon
* \param MJD Modified Julian Date
* \return Meridian axis [normalized]
*/
VECTOR3 MeridianAxis(OBJHANDLE hRef, double MJD);

/*!
* \details Get a longitude and latitude of the input vector
* \param _in input vector in global frame. (J2000 Ecliptic)
* \param _RotAxis Rotation axis of planet or moon.
* \param _Meridian Meridian axis of planet or moon.
* \param Lng [out] Longitude in local equatorial coordinates [rad]
* \param Lat [out] Latitude in local equatorial coordinates [rad]
* \note Latitude is negative in southern hemisphere
*/
void	GetLngLat(VECTOR3 _in, VECTOR3 _RotAxis, VECTOR3 _Meridian, double &Lng, double &Lat);

/*!
* \details Get a unit vector by longitude and latitude.
* \param _RotAxis Rotation axis of planet or moon
* \param _Meridian Meridian axis of planet or moon
* \param Lng Longitude in local geocentric coordinates [rad]
* \param Lat Latitude in local geocentric coordinates [rad]
* \return a unit vector
* \note Latitude is negative in southern hemisphere
*/
VECTOR3 VectorByLngLat(VECTOR3 _RotAxis, VECTOR3 _Meridian, double Lng, double Lat);

/*!
* \details Get base position relative to planet center in J2000 Ecliptic frame of reference.
* \param hBase Base handle
* \param mjd Modified Julian Date
* \return base position vector (planet radius)
*/
VECTOR3 GetBasePosition(OBJHANDLE hBase, double mjd);

/*!
* \details Get position vector of surface location in J2000 Ecliptic frame of reference.
* \param hRef Handle of a planet
* \param mjd Modified Julian Date
* \param lng Longitude
* \param lat Latitude (negative in southern hemisphere)
* \return position vector (planet radius)
*/
VECTOR3 GetSurfaceLocation(OBJHANDLE hPlanet, double mjd, double lng, double lat);

/*!
* \details Convert surface location vector in J2000 Ecliptic frame of reference to other date.
* \param hRef Handle of a planet
* \param mjd Target Modified Julian Date
* \param _init Initial fixed surface location vector at imjd
* \param imjd Initial Modified Julian Date
* \return position vector at target mjd
*/
VECTOR3 ConvertSurfaceLocation(OBJHANDLE hPlanet, double mjd, VECTOR3 _init, double imjd);

double  GetHeading(OBJHANDLE Ref, VECTOR3 &_Pos, VECTOR3 &_Vel);

//@}

#endif