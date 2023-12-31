// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// Orbital elements for elliptic orbits

#ifndef __ELEMENT_H
#define __ELEMENT_H

#include "Vecmat.h"
#include "Astro.h"

// =======================================================================
// class Elements

class Elements {
	friend class Vessel; // temporary!

public:
	Elements ();

	Elements (double _a, double _e, double _i,
		double _theta, double _omegab, double _L, double _mjd_ref);

	Elements (const Elements &el);
	// copy constructor

	Elements (char *fname);
	// construct from file

	void Set (double _a, double _e, double _i,
		double _theta, double _omegab, double _L, double _mjd_epoch);

	void Set (const Elements &el);

	void SetMasses (double m, double M);

	void Setup (double m, double M, double mjd);
	// Set up everything, using orbiter mass m and mass of central body M
	// This must be called before any calculations can be performed

	void Reset (double _a, double _e, double _i,
		double _theta, double _omegab, double _L, double _mjd_ref);
	// Change elements.

	inline double MJDepoch () const { return mjd_epoch; }
	// element epoch

	double Epoch () const { return MJD2Jepoch (mjd_epoch); }
	// Return epoch to which elements refer

	inline double MeanLongitude (double t) const { return priv_n*t + L; }
	inline double MeanAnomaly (double t) const { return priv_n * (t-priv_tau); }

	double EccAnomaly (double ma) const;
	// calculate eccentric anomaly (E) from mean anomaly (M)

	double TrueAnomaly_from_EccAnomaly (double ea) const; // ea: eccentric anomaly

	inline double TrueAnomaly (double ma) const           // ma: mean anomaly
	{ return TrueAnomaly_from_EccAnomaly (EccAnomaly (ma)); }

	double MeanAnomaly_from_EccAnomaly (double ea) const;
	double EccAnomaly_from_TrueAnomaly (double ta) const;

	inline double MeanAnomaly_from_TrueAnomaly (double ta) const
	{ return MeanAnomaly_from_EccAnomaly (EccAnomaly_from_TrueAnomaly (ta)); }

	void RelPos (double &r, double &ta, double t) const;
	// object position on the elliptic orbit at time t, given by
	// the length of the radius vector, r, and the true anomaly ta
	// (the angle between the perihelion and the object)
	// Note -Pi <= theta < Pi

	void Pol2Crt (double r, double ta, VECTOR3 &pos) const;
	// Convert orbital position from polar coordinates (radius r, true anomaly ta)
	// to cartesian coordinates (pos)

	inline double Rdist (double phi) const
	{ return priv_p / (1.0 + e*cos(phi)); }
	// return length of radius vector for true anomaly angle phi

	inline double Vmag (double r) const
	{ return sqrt (priv_mu * (2.0/r - 1.0/a)); }
	// return magnitude of orbital velocity for given radius distance

	void PosVel (VECTOR3 &pos, VECTOR3 &vel, double t) const;
	// calculate position and velocity relative to reference
	// body at time t

	VECTOR3 Pos (double t) const;

	void PosVel_TA (VECTOR3 &pos, VECTOR3 &vel, double ta) const;
	// calculate position and velocity relative to reference
	// at true anomaly ta

	double Spd_TA(double ta) const;
	// magnitude of orbital velocity at true anomaly ta

	void Update (VECTOR3 &pos, VECTOR3 &vel);
	// as PosVel, but also updates internal time-varying data to current simulation time

	void Calculate (const VECTOR3 &R, const VECTOR3 &V, double t);
	// Calculate elements from position and velocity at simulation time t

	void PlaneCoeffs (const VECTOR3 &R, const VECTOR3 &V, double &a, double &b, double &c);
	// Given position and velocity vectors, return the coefficients of the
	// orbital plane E: ax + by + cz + d = 0 (d=0 since plane through origin)

	bool AscendingNode (VECTOR3 &asc) const;
	bool DescendingNode (VECTOR3 &desc) const;
	// return cartesian coordinates of ascending and descending nodes
	// in the reference system
	// return value: error flag (true=ok, false=node doesn't exist (only
	// happens for open orbits)

	// primary elements
	double a;           // semi-major axis [m]
	double e;           // numerical eccentricity
	double i;           // inclination [rad]
	double theta;       // longitude of ascending node [rad]
	double omegab;      // longitude of periapsis [rad]
	double L;           // mean longitude at epoch (at mjd_ref)

	// derived elements
	double ArgPer()  const { return priv_omega; } // argument of periapsis
	double SMi()     const { return priv_b; }     // semi-minor axis
	double ApDist()  const { return priv_ad; }    // aphelion distance
	double PeDist()  const { return priv_pd; }    // perihelion distance
	double OrbitT()  const { return priv_T; }     // orbit period
	double PeT()     const { return priv_Tpe; }   // time to next periapsis passage
	double ApT()     const { return priv_Tap; }   // time to next apoapsis passage
	double P()       const { return priv_p; }     // parameter of conic section (semi-latus rectum)

	// time-varying parameters (calculated at the time of the last call to Calculate2)
	const VECTOR3 &RVec() const { return priv_R; }// radius vector
	const VECTOR3 &VVec() const { return priv_V; }// velocity vector
	const VECTOR3 &HVec() const { return priv_H; }// specific angular momentum H = R x V
	double Radius()  const { return priv_r; }     // radius vector length
	double Vel()     const { return priv_v; }     // magnitude of velocity
	double MeanAnm() const { return priv_ma; }    // mean anomaly
	double TrueAnm() const { return priv_tra; }   // true anomaly
	double EccAnm()  const { return priv_ea; }    // eccentric anomaly
	double MeanLng() const { return priv_ml; }    // mean longitude
	double TrueLng() const { return priv_trl; }   // true longitude
	double LinEcc()  const { return priv_le; }    // linear eccentricity
	double Mu()      const { return priv_mu; }    // G (M+m)

	double sint, cost;	// sin and cos of theta
	double sini, cosi;	// sin and cos of i
	double sino, coso;	// sin and cos of omega

private:
	// secondary elements
	double priv_m;		// mass of orbiter [kg]
	double priv_M;		// mass of central object [kg]
	double priv_le;     // linear eccentricity [m]
	double priv_pd;		// periapsis distance [m]
	double priv_ad;		// apoapsis distance [m]
	double priv_b;		// semi-minor axis [m]
	double priv_mu;		// G * (M+m)
	double priv_muh;
	double priv_n;		// 2pi/T
	double priv_T;		// rotation period [s]
	double priv_Tpe;    // time to next periapsis passage [s]
	double priv_Tap;    // time to next apoapsis passage [s]
	double priv_tau;	// periapsis passage (simulation) time [s]
	double priv_omega;	// argument of periapsis [rad]
	double priv_p;		// parameter of conic section: r = p / (1 + e cos nu)
	double priv_tmp;    // for calculation of true anomaly (if e < 1)
	VECTOR3 priv_H;      // normal to orbital plane        (no unit vector)
	VECTOR3 priv_N;      // points towards ascending node  (unit vector)
	VECTOR3 priv_E;      // points towards periapsis       (no unit vector)

	// the following parameters vary with time and are only valid at the
	// time when Calculate or Update is called
	VECTOR3 priv_R;     // radius vector
	VECTOR3 priv_V;     // velocity vector
	double priv_r;      // radius vector length
	double priv_v;      // magnitude of velocity
	double priv_ea;     // eccentric anomaly
	double priv_ma;     // mean anomaly
	double priv_tra;    // true anomaly
	double priv_ml;     // mean longitude
	double priv_trl;    // true longitude

	mutable double priv_ea0;    // last calculated eccentric anomaly
	mutable double priv_ma0;    // mean anomaly corresponding to priv_ea0

	double mjd_epoch;   // element reference time (MJD format)
	double t_epoch;     // element reference time (simt format)
};

#endif // !__ELEMENT_H