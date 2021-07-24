// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#ifndef __VECMAT_H
#define __VECMAT_H

#include <math.h>
#include <memory.h>
#include <ostream>

// =======================================================================
// Some useful constants

#pragma optimize ("", off)

const double Pi    = 3.1415926535897932384626433832795;
const double Pi2   = 2.0*Pi;
const double Pi05  = 0.5*Pi;
const double Pi15  = 1.5*Pi;
const double Pi025 = 0.25*Pi;
const double RAD   = Pi/180.0;
const double DEG   = 180.0/Pi;

#pragma optimize ("", on)

inline double Rad (double deg) { return RAD*deg; }
inline double Deg (double rad) { return DEG*rad; }

// =======================================================================
// Auxiliary functions

// Returns integer random number in the range 0 <= r < range
int irand (int range);

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

char *trim_string (char *cbuf);

// =======================================================================
// class Vector

class Vector {
public:
	inline Vector ()
	{ x = y = z = 0.0; }

	inline Vector (double _x, double _y, double _z)
	{ x = _x, y = _y, z = _z; }

	inline Vector (const Vector &vec)
	{ x = vec.x, y = vec.y, z = vec.z; }

	inline void Set (double _x, double _y, double _z)
	{ x = _x, y = _y, z = _z; }

	inline void Set (const Vector &vec)
	{ x = vec.x, y = vec.y, z = vec.z; }

	inline Vector &operator= (const Vector &vec)
	{ x = vec.x, y = vec.y, z = vec.z; return *this; }

	inline Vector operator+ (const Vector &vec) const
	{ return Vector (x+vec.x, y+vec.y, z+vec.z); }

	inline Vector operator- (const Vector &vec) const
	{ return Vector (x-vec.x, y-vec.y, z-vec.z); }

	inline Vector operator- () const // unary minus
	{ return Vector (-x, -y, -z); }

	inline Vector operator* (double f) const
	{ return Vector (x*f, y*f, z*f); }

	inline Vector operator/ (double f) const
	{ return Vector (x/f, y/f, z/f); }

	inline double operator& (const Vector &vec) const // scalar product
	{ return x*vec.x + y*vec.y + z*vec.z; }

	inline Vector &operator+= (const Vector &vec)
	{ x += vec.x, y += vec.y, z += vec.z; return *this; }

	inline Vector &operator-= (const Vector &vec)
	{ x -= vec.x, y -= vec.y, z -= vec.z; return *this; }

	inline Vector &operator*= (const double f)
	{ x *= f, y *= f, z *= f; return *this; }

	inline Vector &operator/= (const double f)
	{ x /= f, y /= f, z /= f; return *this; }

	friend Vector crossp (const Vector &a, const Vector &b) // cross product
	{ return Vector (a.y*b.z - b.y*a.z, a.z*b.x - b.z*a.x, a.x*b.y - b.x*a.y); }

	friend double dotp (const Vector &a, const Vector &b) // scalar product
	{ return a.x*b.x + a.y*b.y + a.z*b.z; }

	inline double length2 () const   // square of vector length
	{ return x*x + y*y + z*z; }

	inline double length () const    // vector length
	{ return sqrt (length2()); }

	double dist2 (const Vector &vec) const; // square of distance between two points

	inline double dist (const Vector &vec) const   // distance between two points
	{ return sqrt (dist2 (vec)); }

	Vector unit () const;  // return unit vector in direction of *this

	void unify ();         // set length of *this to unity

	friend double xangle (const Vector &a, const Vector &b);
	// angle between two straight lines through the origin, defined
	// by directions of a and b

	friend std::ostream &operator<< (std::ostream &os, const Vector &v)
	{ os << v.x << ' ' << v.y << ' ' << v.z; return os; }

	union {
		double data[3];
		struct { double x, y, z; };
	};
};

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

	void Set (const Matrix &A)
	{ memcpy (data, A.data, 9*sizeof(double)); }

	Matrix &operator= (const Matrix &A);
	Matrix operator* (const Matrix &A) const;

	inline Matrix &operator+= (const Matrix &A)
	{ for (int i = 0; i < 9; i++) data[i] += A.data[i]; return *this; }

	void premul (const Matrix &A);   // *this = A * *this
	void postmul (const Matrix &A);  // *this = *this * A

	friend Matrix IMatrix();		 // returns identity matrix

	friend Vector mul (const Matrix &A, const Vector &b);  // returns A * b
	friend Vector tmul (const Matrix &A, const Vector &b); // returns A^T * b
	friend Matrix inv (const Matrix &A);  // inverse of A
	friend Matrix transp (const Matrix &A); // transpose of A

	union {
		double data[9];
		struct { double m11, m12, m13, m21, m22, m23, m31, m32, m33; };
	};
};

// =======================================================================
// Geometric utility functions

// Distance of point 'a' from a line defined by a point 'p' and direction vector 'd'
inline double PointLineDist (const Vector &a, const Vector &p, const Vector &d)
{
	return dotp(d,a-p)/d.length();
}

#endif // !__VECMAT_H