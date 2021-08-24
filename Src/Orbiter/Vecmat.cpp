// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#include <stdlib.h>
#include <algorithm>
#include <float.h>
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

void Matrix::Set (const Quaternion &q)
{
	// set rotation matrix from quaternion
	double qx2 = 2.0*q.qvx;
	double qy2 = 2.0*q.qvy;
	double qz2 = 2.0*q.qvz;
	double qxx2 = qx2*q.qvx;
	double qyy2 = qy2*q.qvy;
	double qzz2 = qz2*q.qvz;
	double qxy2 = qx2*q.qvy;
	double qxz2 = qx2*q.qvz;
	double qyz2 = qy2*q.qvz;
	double qxw2 = qx2*q.qs;
	double qyw2 = qy2*q.qs;
	double qzw2 = qz2*q.qs;
	m11 = 1.0-qyy2-qzz2;
	m12 = qxy2+qzw2;
	m13 = qxz2-qyw2;
	m21 = qxy2-qzw2;
	m22 = 1.0-qxx2-qzz2;
	m23 = qyz2+qxw2;
	m31 = qxz2+qyw2;
	m32 = qyz2-qxw2;
	m33 = 1.0-qxx2-qyy2;
}

void Matrix::Set (const Vector &rot)
{
	// set rotation matrix from axis rotation vector
	double sinx = sin(rot.x), cosx = cos(rot.x);
	double siny = sin(rot.y), cosy = cos(rot.y);
	double sinz = sin(rot.z), cosz = cos(rot.z);

	m11 = cosy*cosz;                m12 = cosy*sinz;                m13 = -siny;
	m21 = sinx*siny*cosz-cosx*sinz; m22 = sinx*siny*sinz+cosx*cosz; m23 = sinx*cosy;
	m31 = cosx*siny*cosz+sinx*sinz; m32 = cosx*siny*sinz-sinx*cosz; m33 = cosx*cosy;

	// Equivalent to:
	//
	// Set (1,  0,    0,
	//  	0,  cosx, sinx,
	//		0, -sinx, cosx);
	// postmul (Matrix ( cosy, 0,   -siny,
	//	                 0,    1,    0,
	//				     siny, 0,    cosy));
	// postmul (Matrix ( cosz, sinz, 0,
	//	                -sinz, cosz, 0,
	//					 0,    0,    1   ));
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

Matrix Matrix::operator* (double s) const
{
	return Matrix (
		m11*s, m12*s, m13*s,
		m21*s, m22*s, m23*s,
		m31*s, m32*s, m33*s
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

void Matrix::tpremul (const Matrix &A)
{
	Matrix B(*this);
	m11 = A.m11*B.m11 + A.m21*B.m21 + A.m31*B.m31;
	m12 = A.m11*B.m12 + A.m21*B.m22 + A.m31*B.m32;
	m13 = A.m11*B.m13 + A.m21*B.m23 + A.m31*B.m33;
	m21 = A.m12*B.m11 + A.m22*B.m21 + A.m32*B.m31;
	m22 = A.m12*B.m12 + A.m22*B.m22 + A.m32*B.m32;
	m23 = A.m12*B.m13 + A.m22*B.m23 + A.m32*B.m33;
	m31 = A.m13*B.m11 + A.m23*B.m21 + A.m33*B.m31;
	m32 = A.m13*B.m12 + A.m23*B.m22 + A.m33*B.m32;
	m33 = A.m13*B.m13 + A.m23*B.m23 + A.m33*B.m33;
}

void Matrix::tpostmul (const Matrix &A)
{
	Matrix B(*this);
	m11 = B.m11*A.m11 + B.m12*A.m12 + B.m13*A.m13;
	m12 = B.m11*A.m21 + B.m12*A.m22 + B.m13*A.m23;
	m13 = B.m11*A.m31 + B.m12*A.m32 + B.m13*A.m33;
	m21 = B.m21*A.m11 + B.m22*A.m12 + B.m23*A.m13;
	m22 = B.m21*A.m21 + B.m22*A.m22 + B.m23*A.m23;
	m23 = B.m21*A.m31 + B.m22*A.m32 + B.m23*A.m33;
	m31 = B.m31*A.m11 + B.m32*A.m12 + B.m33*A.m13;
	m32 = B.m31*A.m21 + B.m32*A.m22 + B.m33*A.m23;
	m33 = B.m31*A.m31 + B.m32*A.m32 + B.m33*A.m33;
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
		A.m13*b.x + A.m23*b.y + A.m33*b.z);
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

void Matrix::orthogonalise (int axis)
{
	Vector b1, b2, b3;
	switch (axis) {
	case 0:
		b1.Set (m11, m12, m13);  b1.unify();
		b2.Set (m21, m22, m23);  b2.unify();
		b3 = crossp(b1,b2);
		m31 = b3.x, m32 = b3.y, m33 = b3.z;
		break;
	case 1:
		b1.Set (m11, m12, m13);  b1.unify();
		b3.Set (m31, m32, m33);  b3.unify();
		b2 = crossp (b3,b1);
		m21 = b2.x, m22 = b2.y, m23 = b2.z;
		break;
	case 2:
		b2.Set (m21, m22, m23);  b2.unify();
		b3.Set (m31, m32, m33);  b3.unify();
		b1 = crossp (b2,b3);
		m11 = b1.x, m12 = b1.y, m13 = b1.z;
		break;
	}
}

// =======================================================================
// class Matrix4:  4x4 dense matrix

Matrix4::Matrix4 ()
{
	memset (data, 0, 16*sizeof(double));
}

Matrix4::Matrix4 (const Matrix4 &A)
{
	memcpy (data, A.data, 16*sizeof(double));
}


void qrdcmp (Matrix4 &a, Vector4 &c, Vector4 &d, int *sing)
{
	int i, j, k;
	double scale, sigma, sum, tau;
	if (sing) *sing = 0;

	for (k = 0; k < 3; k++) {
		scale = 0.0;
		for (i = k; i < 4; i++)
			scale = std::max (scale, fabs(a(i,k)));
		if (scale == 0.0) {
			if (sing) *sing = 1;
			c(k) = d(k) = 0.0;
		} else {
			for (i = k; i < 4; i++)
				a(i,k) /= scale;
			for (sum = 0.0, i = k; i < 4; i++)
				sum += a(i,k)*a(i,k);
			sigma = (a(k,k) < 0 ? -sqrt(sum) : sqrt(sum));
			a(k,k) += sigma;
			c(k) = sigma*a(k,k);
			d(k) = -scale*sigma;
			for (j = k+1; j < 4; j++) {
				for (sum = 0.0,i = k; i < 4; i++)
					sum += a(i,k)*a(i,j);
				tau = sum/c(k);
				for (i = k; i < 4; i++)
					a(i,j) -= tau*a(i,k);
			}
		}
	}
	d(3) = a(3,3);
	if (sing && d(3) == 0.0) *sing = 1;
}

void qrsolv (const Matrix4 &a, const Vector4 &c, const Vector4 &d, Vector4 &b)
{
	int i, j;
	double sum, tau;
	for (j = 0; j < 3; j++) {
		for (sum = 0.0, i = j; i < 4; i++)
			sum += a(i,j)*b(i);
		tau = sum/c(j);
		for (i = j; i < 4; i++)
			b(i) -= tau*a(i,j);
	}

	b(3) /= d(3);
	for (i = 2; i >= 0; i--) {
		for (sum = 0.0, j = i+1; j < 4; j++)
			sum += a(i,j)*b(j);
		b(i) = (b(i)-sum)/d(i);
	}
}


void QRFactorize (Matrix4 &A, Vector4 &c, Vector4 &d)
{
    int i, j, k;
    double sum, b, f;

    for (k = 0; k < 4; k++) {
        for (sum = 0, i = k; i < 4; i++)
	    sum += A(i,k)*A(i,k);
		d(k) = (A(k,k) < 0 ? -sqrt(sum) : sqrt(sum));
		b = sqrt(2.0*d(k)*(A(k,k) + d(k)));
		A(k,k) = (A(k,k) + d(k))/b;
		for (i = k+1; i < 4; i++)
			A(i,k) /= b;
		for (j = k+1; j < 4; j++) {
			for (sum = 0, i = k; i < 4; i++)
				sum += A(i,k)*A(i,j);
			f = 2.0*sum;
			for (i = k; i < 4; i++)
				A(i,j) -= f*A(i,k);
		}
    }
}

void RSolve (const Matrix4 &A, const Vector4 &d, Vector4 &b)
{
    int i, j;
    double sum;
    b(3) /= -d(3);
    for (i = 2; i >= 0; i--) {
        for (sum = 0.0, j = i+1; j < 4; j++)
	    sum += A(i,j) * b(j);
		b(i) = (b(i)-sum) / -d(i);
    }
}

void QRSolve (const Matrix4 &A, const Vector4 &c,
    const Vector4 &d, const Vector4 &b, Vector4 &x)
{
    int i, k;
    double sum;

    // Calculates y = Q^T b
    x = b;
    for (k = 0; k < 4; k++) {
        for (sum = 0, i = k; i < 4; i++)
			sum += A(i,k)*x(i);
		sum *= 2.0;
		for (i = k; i < 4; i++)
			x(i) -= sum*A(i,k);
    }

    // Solves Rx = y
    RSolve (A, d, x);
}

void qrdcmp (Matrix &a, Vector &c, Vector &d, int *sing)
{
	int i, j, k;
	double scale, sigma, sum, tau;
	if (sing) *sing = 0;

	for (k = 0; k < 2; k++) {
		scale = 0.0;
		for (i = k; i < 3; i++)
			scale = std::max (scale, fabs(a(i,k)));
		if (scale == 0.0) {
			if (sing) *sing = 1;
			c(k) = d(k) = 0.0;
		} else {
			for (i = k; i < 3; i++)
				a(i,k) /= scale;
			for (sum = 0.0, i = k; i < 3; i++)
				sum += a(i,k)*a(i,k);
			sigma = (a(k,k) < 0 ? -sqrt(sum) : sqrt(sum));
			a(k,k) += sigma;
			c(k) = sigma*a(k,k);
			d(k) = -scale*sigma;
			for (j = k+1; j < 3; j++) {
				for (sum = 0.0,i = k; i < 3; i++)
					sum += a(i,k)*a(i,j);
				tau = sum/c(k);
				for (i = k; i < 3; i++)
					a(i,j) -= tau*a(i,k);
			}
		}
	}
	d(2) = a(2,2);
	if (sing && d(2) == 0.0) *sing = 1;
}

void qrsolv (const Matrix &a, const Vector &c, const Vector &d, Vector &b)
{
	int i, j;
	double sum, tau;
	for (j = 0; j < 2; j++) {
		for (sum = 0.0, i = j; i < 3; i++)
			sum += a(i,j)*b(i);
		tau = sum/c(j);
		for (i = j; i < 3; i++)
			b(i) -= tau*a(i,j);
	}

	b(2) /= d(2);
	for (i = 1; i >= 0; i--) {
		for (sum = 0.0, j = i+1; j < 3; j++)
			sum += a(i,j)*b(j);
		b(i) = (b(i)-sum)/d(i);
	}
}

// =======================================================================
// class Quaternion
// Note: Definitions depend on left-handed coordinate system. For right-
// handed systems, some of the expressions will change.

const double eps = 1e-8;

void Quaternion::Set (const Matrix &R)
{
	// set quaternion from rotation matrix
	static const double eps = 1e-12;
	double S, T = 1.0 + R.m11 + R.m22 + R.m33;
	if (T > eps) {
		S = 2.0 * sqrt (T);
		qvx = (R.m23-R.m32)/S;
		qvy = (R.m31-R.m13)/S;
		qvz = (R.m12-R.m21)/S;
		qs  = 0.25*S;
	} else if (R.m11 > R.m22 && R.m11 > R.m33) {
		S = 2.0 * sqrt (1.0+R.m11-R.m22-R.m33);
		qvx = 0.25*S;
		qvy = (R.m12+R.m21)/S;
		qvz = (R.m31+R.m13)/S;
		qs  = (R.m23-R.m32)/S;
	} else if (R.m22 > R.m33) {
		S = 2.0 * sqrt (1.0+R.m22-R.m11-R.m33);
		qvx = (R.m12+R.m21)/S;
		qvy = 0.25*S;
		qvz = (R.m23+R.m32)/S;
		qs  = (R.m31-R.m13)/S;
	} else {
		S = 2.0 * sqrt (1.0+R.m33-R.m11-R.m22);
		qvx = (R.m31+R.m13)/S;
		qvy = (R.m23+R.m32)/S;
		qvz = 0.25*S;
		qs  = (R.m12-R.m21)/S;
	}
}

double Quaternion::norm2 () const
{
	return qs*qs + qvx*qvx + qvy*qvy + qvz*qvz;
}

double dotp (const Quaternion &q1, const Quaternion &q2)
{
	return q1.qs*q2.qs + q1.qvx*q2.qvx + q1.qvy*q2.qvy + q1.qvz*q2.qvz;
}

void Quaternion::Rotate (const Vector &omega)
{
	double dvx = 0.5*( qs *omega.x - qvy*omega.z + qvz*omega.y);
	double dvy = 0.5*( qs *omega.y - qvz*omega.x + qvx*omega.z);
	double dvz = 0.5*( qs *omega.z - qvx*omega.y + qvy*omega.x);
	double ds  = 0.5*(-qvx*omega.x - qvy*omega.y - qvz*omega.z);
	qvx += dvx;
	qvy += dvy;
	qvz += dvz;
	qs  += ds;

	// renormalise
	double arg = qvx*qvx + qvy*qvy + qvz*qvz + qs*qs;
	if (arg > 0.0) {
		double inorm = 1.0/sqrt(arg);
		qvx *= inorm;
		qvy *= inorm;
		qvz *= inorm;
		qs  *= inorm;
	} else { // desperation
		qvx = qvy = qvz = 0.0;
		qs  = 1.0;
	}
}

Quaternion Quaternion::Rot (const Vector &omega) const
{
	return Quaternion (qvx + 0.5*( qs *omega.x - qvy*omega.z + qvz*omega.y),
		               qvy + 0.5*( qs *omega.y - qvz*omega.x + qvx*omega.z),
					   qvz + 0.5*( qs *omega.z - qvx*omega.y + qvy*omega.x),
					   qs  + 0.5*(-qvx*omega.x - qvy*omega.y - qvz*omega.z));
}

Quaternion &Quaternion::operator+= (const Quaternion &Q)
{
	qvx += Q.qvx;
	qvy += Q.qvy;
	qvz += Q.qvz;
	qs  += Q.qs;

	// renormalise
	double inorm = 1.0/sqrt(qvx*qvx + qvy*qvy + qvz*qvz + qs*qs);
	qvx *= inorm;
	qvy *= inorm;
	qvz *= inorm;
	qs  *= inorm;

	return *this;
}

void Quaternion::premul (const Quaternion &Q)
{
	double s  = Q.qs*qs  - Q.qvx*qvx - Q.qvy*qvy - Q.qvz*qvz;
	double vx = Q.qs*qvx + Q.qvx*qs  - Q.qvy*qvz + Q.qvz*qvy;
	double vy = Q.qs*qvy + Q.qvx*qvz + Q.qvy*qs  - Q.qvz*qvx;
	double vz = Q.qs*qvz - Q.qvx*qvy + Q.qvy*qvx + Q.qvz*qs;

#ifdef UNDEF
	// check for renormalisation
	double len2 = s*s + vx*vx + vy*vy + vz*vz;
	if (len2 < 1.0-eps || len2 > 1.0+eps) {
		double len = sqrt(len2);
		s /= len; vx /= len; vy /= len; vz /= len;
	}
#endif

	qvx = vx, qvy = vy, qvz = vz, qs = s;
}

void Quaternion::postmul (const Quaternion &Q)
{
	double s  = qs*Q.qs  - qvx*Q.qvx - qvy*Q.qvy - qvz*Q.qvz;
	double vx = qs*Q.qvx + qvx*Q.qs  - qvy*Q.qvz + qvz*Q.qvy;
	double vy = qs*Q.qvy + qvx*Q.qvz + qvy*Q.qs  - qvz*Q.qvx;
	double vz = qs*Q.qvz - qvx*Q.qvy + qvy*Q.qvx + qvz*Q.qs;

#ifdef UNDEF
	// check for renormalisation
	double len2 = s*s + vx*vx + vy*vy + vz*vz;
	if (len2 < 1.0-eps || len2 > 1.0+eps) {
		double len = sqrt(len2);
		s /= len; vx /= len; vy /= len; vz /= len;
	}
#endif

	qvx = vx, qvy = vy, qvz = vz, qs = s;
}

void Quaternion::tpostmul (const Quaternion &Q)
{
	double s  =  qs*Q.qs  + qvx*Q.qvx + qvy*Q.qvy + qvz*Q.qvz;
	double vx = -qs*Q.qvx + qvx*Q.qs  + qvy*Q.qvz - qvz*Q.qvy;
	double vy = -qs*Q.qvy - qvx*Q.qvz + qvy*Q.qs  + qvz*Q.qvx;
	double vz = -qs*Q.qvz + qvx*Q.qvy - qvy*Q.qvx + qvz*Q.qs;

#ifdef UNDEF
	// check for renormalisation
	double len2 = s*s + vx*vx + vy*vy + vz*vz;
	if (len2 < 1.0-eps || len2 > 1.0+eps) {
		double len = sqrt(len2);
		s /= len; vx /= len; vy /= len; vz /= len;
	}
#endif

	qvx = vx, qvy = vy, qvz = vz, qs = s;
}

Quaternion Quaternion::operator* (const Quaternion &Q) const
{
	return Quaternion (
		qs*Q.qvx + qvx*Q.qs  - qvy*Q.qvz + qvz*Q.qvy,
		qs*Q.qvy + qvx*Q.qvz + qvy*Q.qs  - qvz*Q.qvx,
		qs*Q.qvz - qvx*Q.qvy + qvy*Q.qvx + qvz*Q.qs,
		qs*Q.qs  - qvx*Q.qvx - qvy*Q.qvy - qvz*Q.qvz
	);
}

Vector mul (const Quaternion &q, const Vector &p)
{
	// note that the implementations of mul and tmul are switched w.r.t. a right-handed system
	double vx = q.qs*p.x - q.qvy*p.z + q.qvz*p.y;
	double vy = q.qs*p.y - q.qvz*p.x + q.qvx*p.z;
	double vz = q.qs*p.z - q.qvx*p.y + q.qvy*p.x;
	double qvx2 = -2.0*q.qvx;
	double qvy2 = -2.0*q.qvy;
	double qvz2 = -2.0*q.qvz;
	return Vector (p.x + qvy2*vz - qvz2*vy,
		           p.y + qvz2*vx - qvx2*vz,
				   p.z + qvx2*vy - qvy2*vx);
}

Vector tmul (const Quaternion &q, const Vector &p)
{
	// note that the implementations of mul and tmul are switched w.r.t. a right-handed system
	double vx = q.qs*p.x + q.qvy*p.z - q.qvz*p.y;
	double vy = q.qs*p.y + q.qvz*p.x - q.qvx*p.z;
	double vz = q.qs*p.z + q.qvx*p.y - q.qvy*p.x;
	double qvx2 = 2.0*q.qvx;
	double qvy2 = 2.0*q.qvy;
	double qvz2 = 2.0*q.qvz;
	return Vector (p.x + qvy2*vz - qvz2*vy,
		           p.y + qvz2*vx - qvx2*vz,
				   p.z + qvx2*vy - qvy2*vx);
}

void Quaternion::interp (const Quaternion &A, const Quaternion &B, double u)
{
	double sign = 1.0;
	double dotAB = dotp(A,B);
	if (dotAB < 0.0) {
		// resolve ambiguity of matrix->quaternion conversion by assuming that
		// quaternions differ by < pi/2. Flip B if this is not satisfied.
		dotAB = -dotAB;
		sign = -1.0;
	}
	double omega = acos(dotAB);

	double sino = sin(omega);
	double fa, fb;
	if (fabs(sino) < 1e-8) {
		fa = 1.0-u;
		fb = u;
	} else {
		fa = sin((1.0-u)*omega)/sino;
		fb = sin(u*omega)/sino*sign;
	}

	// now interpolate with weights
	qvx = fa*A.qvx + fb*B.qvx;
	qvy = fa*A.qvy + fb*B.qvy;
	qvz = fa*A.qvz + fb*B.qvz;
	qs  = fa*A.qs  + fb*B.qs;

	// renormalise
	double inorm = 1.0/sqrt(qvx*qvx + qvy*qvy + qvz*qvz + qs*qs);
	if (qs < 0.0) inorm = -inorm;
	qvx *= inorm;
	qvy *= inorm;
	qvz *= inorm;
	qs  *= inorm;
}

double angle (const Quaternion &A, const Quaternion &B)
{
	double alen = sqrt (A.qvx*A.qvx + A.qvy*A.qvy + A.qvz*A.qvz);
	double blen = sqrt (B.qvx*B.qvx + B.qvy*B.qvy + B.qvz*B.qvz);
	if (alen && blen) {
		double cosa = (A.qvx*B.qvx + A.qvy*B.qvy + A.qvz*B.qvz) / (alen*blen);
		return acos (cosa);
	} else
		return 0.0; // problems!
}


// =======================================================================
// StateVectors

void StateVectors::Set (const StateVectors &s)
{
	vel.Set (s.vel);
	pos.Set (s.pos);
	omega.Set (s.omega);
	Q.Set (s.Q);
	R.Set (s.R);
}

void StateVectors::Set (const Vector &v, const Vector &p, const Vector &av, const Quaternion &ap)
{
	vel.Set (v);
	pos.Set (p);
	omega.Set (av);
	Q.Set (ap);
	R.Set (ap);
}

void StateVectors::SetRot (const Matrix &r)
{
	R.Set (r);
	Q.Set (r);
}

void StateVectors::SetRot (const Quaternion &q)
{
	Q.Set (q);
	R.Set (q);
}

void StateVectors::Advance (double dt, const Vector &a, const Vector &v, const Vector &aa, const Vector &av)
{
	vel   += a*dt;
	pos   += v*dt;
	omega += aa*dt;
	Q.Rotate (av*dt);
	R.Set (Q);
}

// =======================================================================
// Geometric utility functions

void PlaneCoeffs (const Vector &p1, const Vector &p2, const Vector &p3,
	double &a, double &b, double &c, double &d)
{
	a = p1.y*(p2.z-p3.z) - p2.y*(p1.z-p3.z) + p3.y*(p1.z-p2.z);
	b = p1.x*(p3.z-p2.z) - p2.x*(p3.z-p1.z) + p3.x*(p2.z-p1.z);
	c = p1.x*(p2.y-p3.y) - p2.x*(p1.y-p3.y) + p3.x*(p1.y-p2.y);
	d = -p1.x*a - p1.y*b - p1.z*c;
}

double PointPlaneDist(const Vector& p, double a, double b, double c, double d)
{
	double D = -sqrt(a * a + b * b + c * c);
	// for a valid plane definition, this should never be 0, so we don't check for division by zero here
	return (a * p.x + b * p.y + c * p.z + d) / D;
}

bool LinePlaneIntersect (double a, double b, double c, double d, const Vector &p, const Vector &s, Vector &r)
{
	double D = a*s.x + b*s.y + c*s.z;
	if (!D) return false;
	r.x = (p.x * (b*s.y + c*s.z) - s.x * (d + b*p.y + c*p.z)) / D;
	r.y = (p.y * (a*s.x + c*s.z) - s.y * (d + a*p.x + c*p.z)) / D;
	r.z = (p.z * (a*s.x + b*s.y) - s.z * (d + a*p.x + b*p.y)) / D;
	return true;
}

void VectorBasisToMatrix(const Vector &X, const Vector &Y, const Vector &Z, Matrix &R)
{
	R.Set(X.x, X.y, X.z,
		  Y.x, Y.y, Y.z,
		  Z.x, Z.y, Z.z);
}

void DirRotToMatrix(const Vector &Z, const Vector &Y, Matrix &R)
{
	// Compute the third orthogonal direction vector from Z and Y
	Vector X(crossp(Y, Z)); // left-handed

	VectorBasisToMatrix(X, Y, Z, R);
}