// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#ifndef __VECMAT_H
#define __VECMAT_H

#include "vector.hpp"

#include <math.h>
#include <memory.h>
#include <ostream>

// =======================================================================
// Some useful constants

const double Pi    = 3.14159265358979323846;
const double Pi2   = 6.28318530717958647693;
const double Pi05  = 1.57079632679489661923;
const double Pi15  = 4.71238898038468985769;
const double Pi025 = 0.785398163397448309615;
const double _RAD_   = Pi/180.0;
const double _DEG_   = 180.0/Pi;
const double LOG2  = 1.0/log(2.0); // conversion factor from log->log2

inline double Rad (double deg) { return _RAD_*deg; }
inline double Deg (double rad) { return _DEG_*rad; }

class Quaternion;

// =======================================================================
// Auxiliary functions

// Returns integer random number in the range 0 <= r < range
int irand (int range);

#ifdef UNDEF
// Normalise argument to range -Pi <= a < Pi
inline double normangle (double angle)
{
	double a = fmod (angle, Pi2);
	return (a >= Pi ? a-Pi2 : a < -Pi ? a+Pi2 : a);
}

// Normalise argument to range 0 <= a < 2Pi
inline double posangle (double angle)
{
	double a = fmod (angle, Pi2);
	return (a >= 0.0 ? a : a+Pi2);
}
#endif

// Difference a1-a2, redmoving phase wrap
inline double diffangle (double a1, double a2)
{
	a1 = fmod (a1, Pi2); if (a1 < 0) a1 += Pi2;
	a2 = fmod (a2, Pi2); if (a2 < 0) a2 += Pi2;
	if      (a1-a2 > Pi) a2 += Pi2;
	else if (a2-a1 > Pi) a1 += Pi2;
	return a1-a2;
}

inline double asinh (double x)
{
	return log (x + sqrt (x*x+1.0));
}

inline double acosh (double x)
{
	return log (x + sqrt (x*x-1.0));
	// note: sign undefined
}

// =======================================================================
// class Matrix

class Matrix {
public:
	Matrix ();
	Matrix (const Matrix &A);
	Matrix (double a11, double a12, double a13,
		    double a21, double a22, double a23,
			double a31, double a32, double a33);

	void Set (double a11, double a12, double a13,
			  double a21, double a22, double a23,
			  double a31, double a32, double a33)
	{ m11=a11, m12=a12, m13=a13, m21=a21, m22=a22, m23=a23, m31=a31, m32=a32, m33=a33; }

	inline void Set (const Matrix &A)
	{ memcpy (data, A.data, 9*sizeof(double)); }

	void Set (const Quaternion &q);

	void Set (const VECTOR3 &rot);
	// Set from axis rotation vector

	inline double &operator() (int i, int j)
	{ return data[i*4+j]; }

	inline double operator() (int i, int j) const
	{ return data[i*4+j]; }

	Matrix &operator= (const Matrix &A);
	Matrix operator* (const Matrix &A) const;
	Matrix operator* (double s) const;

	inline Matrix &operator+= (const Matrix &A)
	{ for (int i = 0; i < 9; i++) data[i] += A.data[i]; return *this; }

	inline Matrix &operator*= (double s)
	{ for (int i = 0; i < 9; i++) data[i] *= s; return *this; }

	void premul (const Matrix &A);   // *this = A * *this
	void postmul (const Matrix &A);  // *this = *this * A

	void tpremul (const Matrix &A);  // *this = A^T * *this
	void tpostmul (const Matrix &A); // *this = *this * A^T

	void orthogonalise (int axis);

	friend void qrdcmp (Matrix &a, VECTOR3 &c, VECTOR3 &d, int *sing = 0);
	friend void qrsolv (const Matrix &a, const VECTOR3 &c, const VECTOR3 &d, VECTOR3 &b);

	union {
		double data[9];
		struct { double m11, m12, m13, m21, m22, m23, m31, m32, m33; };
	};
};

Matrix IMatrix();		 // returns identity matrix

VECTOR3 mul (const Matrix &A, const VECTOR3 &b);  // returns A * b
VECTOR3 tmul (const Matrix &A, const VECTOR3 &b); // returns A^T * b
Matrix inv (const Matrix &A);  // inverse of A
Matrix transp (const Matrix &A); // transpose of A

// =======================================================================
// class Matrix4:  4x4 dense matrix

class Matrix4 {
public:
	Matrix4 ();
	Matrix4 (const Matrix4 &A);

	inline Matrix4 (double a11, double a12, double a13, double a14,
		     double a21, double a22, double a23, double a24,
			 double a31, double a32, double a33, double a34,
			 double a41, double a42, double a43, double a44)
	{ m11=a11, m12=a12, m13=a13, m14=a14,
	  m21=a21, m22=a22, m23=a23, m24=a24,
	  m31=a31, m32=a32, m33=a33, m34=a34,
	  m41=a41, m42=a42, m43=a43, m44=a44; }

	inline void Set (double a11, double a12, double a13, double a14,
			  double a21, double a22, double a23, double a24,
			  double a31, double a32, double a33, double a34,
			  double a41, double a42, double a43, double a44)
	{ m11=a11, m12=a12, m13=a13, m14=a14,
	  m21=a21, m22=a22, m23=a23, m24=a24,
	  m31=a31, m32=a32, m33=a33, m34=a34,
	  m41=a41, m42=a42, m43=a43, m44=a44; }

	inline void Set (const Matrix4 &A)
	{ memcpy (data, A.data, 16*sizeof(double)); }

	inline double &operator() (int i, int j)
	{ return data[i*4+j]; }

	inline double operator() (int i, int j) const
	{ return data[i*4+j]; }

	friend void qrdcmp (Matrix4 &a, VECTOR4 &c, VECTOR4 &d, int *sing = 0);
	friend void qrsolv (const Matrix4 &a, const VECTOR4 &c, const VECTOR4 &d, VECTOR4 &b);
	friend void QRFactorize (Matrix4 &A, VECTOR4 &c, VECTOR4 &d);
	friend void RSolve (const Matrix4 &A, const VECTOR4 &d, VECTOR4 &b);
	friend void QRSolve (const Matrix4 &A, const VECTOR4 &c,
		const VECTOR4 &d, const VECTOR4 &b, VECTOR4 &x);

	union {
		double data[16];
		struct { double m11, m12, m13, m14, m21, m22, m23, m24, m31, m32, m33, m34, m41, m42, m43, m44; };
	};
};

// =======================================================================
// class Quaternion

class Quaternion {
public:
	inline Quaternion () { qvx = qvy = qvz = 0.0, qs = 1.0; }
	// Create an identity quaternion

	inline Quaternion (const Quaternion &Q) { Set (Q); }
	// Copy constructor

	inline Quaternion (double vx, double vy, double vz, double s) { Set (vx, vy, vz, s); }
	// Constructor from scalar parameters

	inline Quaternion (const VECTOR3 &v, double s) { Set (v, s); }
	// Constructor from vector+scalar parameters

	inline Quaternion (const Matrix &R) { Set (R); }
	// Constructor from rotation matrix

	inline void Set (const Quaternion &Q)
	{ memcpy (data, Q.data, 4*sizeof(double)); }

	void Set (const Matrix &R);
	// set the quaternion from a rotation matrix. R must be orthonormal.

	inline void Set (double vx, double vy, double vz, double s)
	{ qvx = vx, qvy = vy, qvz = vz, qs = s; }

	inline void Set (const VECTOR3 &v, double s)
	{ qvx = v.x, qvy = v.y, qvz = v.z, qs = s; }
	// set the quaternion from a vector and scalar component

	double norm2 () const;
	// Square norm of the quaternion

	friend double dotp (const Quaternion &q1, const Quaternion &q2);
	// dot product of two quaternions

	inline double norm () const { return sqrt(norm2()); }
	// Norm of the quaternion

	inline void normalise ()
	{ double len = norm(); qs /= len; qvx /= len; qvy /= len; qvz /= len; }

	void Rotate (const VECTOR3 &omega);
	// rotate the quaternion by rotation angles omega

	Quaternion Rot (const VECTOR3 &omega) const;
	// returns the quaternion rotated by angles omega

	friend VECTOR3 mul (const Quaternion &q, const VECTOR3 &b);
	// Returns vector p rotated by quaternion

	friend VECTOR3 tmul (const Quaternion &q, const VECTOR3 &p);
	// Returns vector p rotated by inverse quaternion

	Quaternion &operator+= (const Quaternion &Q);
	void premul (const Quaternion &Q);  // *this = Q * *this
	void postmul (const Quaternion &Q); // *this = *this * Q
	void tpostmul (const Quaternion &Q); // *this = *this * Q^-1

	Quaternion operator* (const Quaternion &Q) const; // returns *this * Q

	Quaternion conj (const Quaternion &Q) const // returns conjugate of Q
	{ return Quaternion (-Q.qvx, -Q.qvy, -Q.qvz, Q.qs); }

	void interp (const Quaternion &A, const Quaternion &B, double u);
	// linear interpolation between A and B to intermediate orientation
	// given by fraction u in [0,1].

	friend double angle (const Quaternion &A, const Quaternion &B);
	// angle between two quaternions [rad]

	union {
		double data[4];
		struct { double qvx, qvy, qvz, qs; };
	};
};

// =======================================================================
// State vector struct
// Contains the state of a rigid body (position, linear velocity, orientation,
// angular velocity

class StateVectors {
public:
	void Set (const StateVectors &s);

	void Set (const VECTOR3 &v, const VECTOR3 &p, const VECTOR3 &av, const Quaternion &ap);
	// Set state vectors to linear velocity v, position p, angular velocity av and
	// orientation ap.

	void SetRot (const Matrix &r);     // set rotation state from a rotation matrix
	void SetRot (const Quaternion &q); // set rotation state from a quaternion

	void Advance (double dt, const VECTOR3 &a, const VECTOR3 &v, const VECTOR3 &aa, const VECTOR3 &av);
	// Advance the state vectors by dt, given linear acceleration a, linear velocity v,
	// angular acceleration aa and angular velocity av.

	VECTOR3 pos;     // position in associated frame
	VECTOR3 vel;     // linear velocity in associated frame
	Matrix R;        // rotation matrix in associated frame
	Quaternion Q;    // orientation in associated frame
	VECTOR3 omega;   // angular velocity components in associated frame
};

// =======================================================================
// Geometric utility functions

// Calculate the coefficients of a plane, ax+by+cz+d = 0, from 3 points spanning the plane
// (Note that this assumes a left-handed coordinate system)
void PlaneCoeffs (const VECTOR3 &p1, const VECTOR3 &p2, const VECTOR3 &p3,
	double &a, double &b, double &c, double &d);

// Distance of point 'a' from a line defined by a point 'p' and direction vector 'd'
inline double PointLineDist (const VECTOR3 &a, const VECTOR3 &p, const VECTOR3 &d)
{
	return len(cross(unit(d), a - p));
}

// Distance of point 'p' from a plane defined by coefficients a,b,c,d (ax+by+cz+d=0)
// This is a signed distance, so return value < 0 is possible
double PointPlaneDist(const VECTOR3& p, double a, double b, double c, double d);

// Calculate intersection r of a line (given by point p and direction s) with a plane
// given by coefficients ax+by+cz+d = 0. If return value=false then no intersection exists
bool LinePlaneIntersect (double a, double b, double c, double d, const VECTOR3 &p, const VECTOR3 &s, VECTOR3 &r);

// Return the normal to the plane defined by coefficients a,b,c,d
inline auto PlaneNormal(double a, double b, double c, double d) { return unit(VECTOR3{a, b, c}); }

// Convert a cartesian reference frame given by orthonormal vectors X, Y, Z (expressed in the global frame) into
// a rotation matrix, such that a point p in the global frame is transformed to p' in the XYZ frame by p' = Rp.
// Note: X,Y,Z must be orthonormal (orthogonal, normalised) vectors (not tested)
void VectorBasisToMatrix(const VECTOR3 &X, const VECTOR3 &Y, const VECTOR3 &Z, Matrix &R);

// Convert a cartesian reference frame given by orthonormal vectors Y, Z (expressed in the global frame) into
// a rotation matrix, such that a point p in the global frame is transformed to p' in the YZ frame by p' = Rp.
// The missing X vector of the orthonormal basis is computed internally (left-handed convention)
// Note: Y,Z must be orthonormal (orthogonal, normalised) vectors (not tested)
// This function is useful for rotations involving docking ports which are defined by an approach direction (Z)
// and an up direction (Y) that defines the longitudinal rotation reference.
void DirRotToMatrix(const VECTOR3 &Z, const VECTOR3 &Y, Matrix &R);

#endif // !__VECMAT_H
