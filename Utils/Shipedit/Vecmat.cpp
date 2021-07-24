// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#include <stdlib.h>
#include "Vecmat.h"

int irand (int range)
{
	static const double drand_max = (double)(RAND_MAX+0.1);
	return (int)((double)rand()*(double)range/drand_max);
}

// =======================================================================
// class Vector

double Vector::dist2 (const Vector &vec) const
{
	double dx = x-vec.x;
	double dy = y-vec.y;
	double dz = z-vec.z;
	return dx*dx + dy*dy + dz*dz;
}

Vector Vector::unit () const
{
	double ilen = 1.0/length();
	return Vector (x*ilen, y*ilen, z*ilen);
}

void Vector::unify ()
{
	double ilen = 1.0/length();
	x *= ilen, y *= ilen, z *= ilen;
}

double xangle (const Vector &a, const Vector &b)
{
	double cosa = dotp (a.unit(), b.unit());
	if (cosa < 1.0) { // also need to check for > -1
		double angle = acos(cosa);
		if (cosa >= 0.0) return angle;
		else             return Pi2 - angle;
	} else return 0.0;
}

// =======================================================================
// class Matrix

Matrix::Matrix ()
{
	memset (data, 0, 9*sizeof(double));
}

Matrix::Matrix (const Matrix &A)
{
	memcpy (data, A.data, 9*sizeof(double));
}

Matrix::Matrix (double a11, double a12, double a13,
				double a21, double a22, double a23,
				double a31, double a32, double a33)
{
	m11 = a11, m12 = a12, m13 = a13;
	m21 = a21, m22 = a22, m23 = a23;
	m31 = a31, m32 = a32, m33 = a33;
}

Matrix &Matrix::operator= (const Matrix &A)
{
	memcpy (data, A.data, 9 * sizeof (double));
	return *this;
}

Matrix Matrix::operator* (const Matrix &A) const
{
	return Matrix (
		m11*A.m11 + m12*A.m21 + m13*A.m31, m11*A.m12 + m12*A.m22 + m13*A.m32, m11*A.m13 + m12*A.m23 + m13*A.m33,
		m21*A.m11 + m22*A.m21 + m23*A.m31, m21*A.m12 + m22*A.m22 + m23*A.m32, m21*A.m13 + m22*A.m23 + m23*A.m33,
		m31*A.m11 + m32*A.m21 + m33*A.m31, m31*A.m12 + m32*A.m22 + m33*A.m32, m31*A.m13 + m32*A.m23 + m33*A.m33
	);
}

void Matrix::premul (const Matrix &A)
{
	Matrix B(*this);
	m11 = A.m11*B.m11 + A.m12*B.m21 + A.m13*B.m31;
	m12 = A.m11*B.m12 + A.m12*B.m22 + A.m13*B.m32;
	m13 = A.m11*B.m13 + A.m12*B.m23 + A.m13*B.m33;
	m21 = A.m21*B.m11 + A.m22*B.m21 + A.m23*B.m31;
	m22 = A.m21*B.m12 + A.m22*B.m22 + A.m23*B.m32;
	m23 = A.m21*B.m13 + A.m22*B.m23 + A.m23*B.m33;
	m31 = A.m31*B.m11 + A.m32*B.m21 + A.m33*B.m31;
	m32 = A.m31*B.m12 + A.m32*B.m22 + A.m33*B.m32;
	m33 = A.m31*B.m13 + A.m32*B.m23 + A.m33*B.m33;
}

void Matrix::postmul (const Matrix &A)
{
	Matrix B(*this);
	m11 = B.m11*A.m11 + B.m12*A.m21 + B.m13*A.m31;
	m12 = B.m11*A.m12 + B.m12*A.m22 + B.m13*A.m32;
	m13 = B.m11*A.m13 + B.m12*A.m23 + B.m13*A.m33;
	m21 = B.m21*A.m11 + B.m22*A.m21 + B.m23*A.m31;
	m22 = B.m21*A.m12 + B.m22*A.m22 + B.m23*A.m32;
	m23 = B.m21*A.m13 + B.m22*A.m23 + B.m23*A.m33;
	m31 = B.m31*A.m11 + B.m32*A.m21 + B.m33*A.m31;
	m32 = B.m31*A.m12 + B.m32*A.m22 + B.m33*A.m32;
	m33 = B.m31*A.m13 + B.m32*A.m23 + B.m33*A.m33;
}

Matrix IMatrix()
{
	return Matrix (1.0,0.0,0.0,  0.0,1.0,0.0,  0.0,0.0,1.0);
}

Vector mul (const Matrix &A, const Vector &b)
{
	return Vector (
		A.m11*b.x + A.m12*b.y + A.m13*b.z,
		A.m21*b.x + A.m22*b.y + A.m23*b.z,
		A.m31*b.x + A.m32*b.y + A.m33*b.z);
}

Vector tmul (const Matrix &A, const Vector &b)
{
	return Vector (
		A.m11*b.x + A.m21*b.y + A.m31*b.z,
		A.m12*b.x + A.m22*b.y + A.m32*b.z,
		A.m13*b.x + A.m32*b.y + A.m33*b.z);
}

Matrix inv (const Matrix &A)
{
	double det = A.m11 * (A.m22*A.m33 - A.m32*A.m23) -
				 A.m12 * (A.m21*A.m33 - A.m31*A.m23) +
				 A.m13 * (A.m21*A.m32 - A.m31*A.m22);
	return Matrix (
		( A.m22*A.m33 - A.m32*A.m23) / det,
		(-A.m12*A.m33 + A.m32*A.m13) / det,
		( A.m12*A.m23 - A.m22*A.m13) / det,
		(-A.m21*A.m33 + A.m31*A.m23) / det,
		( A.m11*A.m33 - A.m31*A.m13) / det,
		(-A.m11*A.m23 + A.m21*A.m13) / det,
		( A.m21*A.m32 - A.m31*A.m22) / det,
		(-A.m11*A.m32 + A.m31*A.m12) / det,
		( A.m11*A.m22 - A.m21*A.m12) / det);
}

Matrix transp (const Matrix &A)
{
	return Matrix (A.m11, A.m21, A.m31,
		           A.m12, A.m22, A.m32,
				   A.m13, A.m23, A.m33);
}

char *trim_string (char *cbuf)
{
	char *c;

	// strip comments starting with ';'
	for (c = cbuf; *c; c++) {
		if (*c == ';') {
			*c = '\0';
			break;
		}
	}
	// strip trailing white space
	for (--c; c >= cbuf; c--) {
		if (*c == ' ' || *c == '\t') *c = '\0';
		else break;
	}
	// skip leading white space
	for (c = cbuf; *c; c++)
		if (*c != ' ' && *c != '\t') return c;

	// should never get here
	return c;
}

