// =======================================================================
// BodyIntegrator.cpp
// Part of RigidBody class implementation
//
// Integrators for dynamic body state vector propagation
//
// This includes versions which propagate
// - linear state vectors only (position, velocity) - deprecated
// - angular state vectors only (orientation, angular velocity) - deprecated
// - linear and angular state vectors simultaneously
// - perturbations of a 2-body orbit
//
// Parameters for linear state propagators:
//    double h: step interval [s]
//
// Parameters for angular state propagators:
//    AngIntData data: angular state information
//
// Parameters for linear+angular state propagators:
//    double h: step interval for full step [s]
//    int nsub: number of subsamples
//    int isub: current subsample
//    double dt: subsample step interval [s]
//
// Parameters for perturbation propagators:
//    PertIntData data: orbit state informations
//
// All functions rely on the following Body-provided functions:
// GetLinAcc: returns linear acceleration for an arbitrary position at an
//     arbitrary time within the current interval. (The basic RigidBody
//     method applies gravitational acceleration; overloaded classes can
//     add their own components)
// GetTorque: returns the (mass-normalised) torque for an arbitrary
//     position, angular velocity and orientation at an arbitrary time
//     within the current interval. (The basic RigidBody method applies
//     gravity gradient torque with optional damping; overloaded classes
//     can add their own components)
// =======================================================================

#include "Orbiter.h"
#include "Rigidbody.h"
#include "Element.h"
#include "Log.h"
#include <stdio.h>

extern TimeData td;
extern char DBG_MSG[256];

// ===========================================================================
// Runge-Kutta integration parameters (RK5-RK8)
// Used by the driver routines below
// (Note that RK2 and RK4 are implemented directly without using the driver
// routines)
// ===========================================================================

// ---------------------------------------------------------------------------
// RK5 6-stage parameters
// ---------------------------------------------------------------------------

static const int RK5_n = 6;
static const double RK5_alpha[RK5_n-1] = {
	1.0/5.0, 3.0/10.0, 4.0/5.0, 8.0/9.0, 1.0
};
static const double RK5_beta[(RK5_n-1)*(RK5_n-1)] = {
	1.0/5.0, 0, 0, 0, 0,
	3.0/40.0, 9.0/40.0, 0, 0, 0,
	44.0/45.0, -56.0/15.0, 32.0/9.0, 0, 0,
	19372.0/6561.0, -25360.0/2187.0, 64448.0/6561.0, -212.0/729.0, 0,
	9017.0/3168.0, -355.0/33.0, 46732.0/5247.0, 49.0/176.0, -5103.0/18656.0
};
static const double RK5_gamma[RK5_n] = {
	35.0/384.0, 0, 500.0/1113.0, 125.0/192.0, -2187.0/6784.0, 11.0/84.0
};

// ---------------------------------------------------------------------------
// RK6 8-stage parameters
// ---------------------------------------------------------------------------

static const int RK6_n = 8;
static const double RK6_alpha[RK6_n-1] = {
	1.0/6.0, 4.0/15.0, 2.0/3.0, 5.0/6.0, 1.0, 1.0/15.0, 1.0
};
static const double RK6_beta[(RK6_n-1)*(RK6_n-1)] = {
	1.0/6.0, 0, 0, 0, 0, 0, 0,
	4.0/75.0, 16.0/75.0, 0, 0, 0, 0, 0,
	5.0/6.0, -8.0/3.0, 5.0/2.0, 0, 0, 0, 0,
	-165.0/64.0, 55.0/6.0, -425.0/64.0, 85.0/96.0, 0, 0, 0,
	12.0/5.0, -8.0, 4015.0/612.0, -11.0/36.0, 88.0/255.0, 0, 0,
	-8263.0/15000.0, 124.0/75.0, -643.0/680.0, -81.0/250.0, 2484.0/10625.0, 0, 0,
	3501.0/1720.0, -300.0/43.0, 297275.0/52632.0, -319.0/2322.0, 24068.0/84065.0, 0, 3850.0/26703.0
};
static const double RK6_gamma[RK6_n] = {
	3.0/40.0, 0, 875.0/2244.0, 23.0/72.0, 264.0/1955.0, 0, 125.0/11592.0, 43.0/616.0
};

// ---------------------------------------------------------------------------
// RK7 11-stage parameters
// ---------------------------------------------------------------------------

static const int RK7_n = 11;
static const double RK7_alpha[RK7_n-1] = {
	2.0/27.0, 1.0/9.0, 1.0/6.0, 5.0/12.0, 1.0/2.0, 5.0/6.0, 1.0/6.0, 2.0/3.0, 1.0/3.0, 1.0
};
static const double RK7_beta[(RK7_n-1)*(RK7_n-1)] = {
	2.0/27.0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	1.0/36.0, 1.0/12.0, 0, 0, 0, 0, 0, 0, 0, 0,
	1.0/24.0, 0, 1.0/8.0, 0, 0, 0, 0, 0, 0, 0,
	5.0/12.0, 0, -25.0/16.0, 25.0/16.0, 0, 0, 0, 0, 0, 0,
	1.0/20.0, 0, 0, 1.0/4.0, 1.0/5.0, 0, 0, 0, 0, 0,
	-25.0/108.0, 0, 0, 125.0/108.0, -65.0/27.0, 125.0/54.0, 0, 0, 0, 0,
	31.0/300.0, 0, 0, 0, 61.0/225.0, -2.0/9.0, 13.0/900.0, 0, 0, 0,
	2.0, 0, 0, -53.0/6.0, 704.0/45.0, -107.0/9.0, 67.0/90.0, 3.0, 0, 0,
	-91.0/108.0, 0, 0, 23.0/108.0, -976.0/135.0, 311.0/54.0, -19.0/60.0, 17.0/6.0, -1.0/12.0, 0,
	2383.0/4100.0, 0, 0, -341.0/164.0, 4496.0/1025.0, -301.0/82.0, 2133.0/4100.0, 45.0/82.0, 45.0/164.0, 18.0/41.0
};
static const double RK7_gamma[RK7_n] = {
	41.0/840.0, 0, 0, 0, 0, 34.0/105.0, 9.0/35.0, 9.0/35.0, 9.0/280.0, 9.0/280.0, 41.0/840.0
};

// ---------------------------------------------------------------------------
// RK8 13-stage parameters
// ---------------------------------------------------------------------------

static const int RK8_n = 13;
static const double RK8_alpha[RK8_n-1] = {
	2.0/27.0, 1.0/9.0, 1.0/6.0, 5.0/12.0, 1.0/2.0, 5.0/6.0, 1.0/6.0, 2.0/3.0, 1.0/3.0, 1.0, 0, 1.0
};
static const double RK8_beta[(RK8_n-1)*(RK8_n-1)] = {
	2.0/27.0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	1.0/36.0, 1.0/12.0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	1.0/24.0, 0, 1.0/8.0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	5.0/12.0, 0, -25.0/16.0, 25.0/16.0, 0, 0, 0, 0, 0, 0, 0, 0,
	1.0/20.0, 0, 0, 1.0/4.0, 1.0/5.0, 0, 0, 0, 0, 0, 0, 0,
	-25.0/108.0, 0, 0, 125.0/108.0, -65.0/27.0, 125.0/54.0, 0, 0, 0, 0, 0, 0,
	31.0/300.0, 0, 0, 0, 61.0/225.0, -2.0/9.0, 13.0/900.0, 0, 0, 0, 0, 0,
	2.0, 0, 0, -53.0/6.0, 704.0/45.0, -107.0/9.0, 67.0/90.0, 3.0, 0, 0, 0, 0,
	-91.0/108.0, 0, 0, 23.0/108.0, -976.0/135.0, 311.0/54.0, -19.0/60.0, 17.0/6.0, -1.0/12.0, 0, 0, 0,
	2383.0/4100.0, 0, 0, -341.0/164.0, 4496.0/1025.0, -301.0/82.0, 2133.0/4100.0, 45.0/82.0, 45.0/164.0, 18.0/41.0, 0, 0,
	3.0/205.0, 0, 0, 0, 0, -6.0/41.0, -3.0/205.0, -3.0/41.0, 3.0/41.0, 6.0/41.0, 0, 0,
	-1777.0/4100.0, 0, 0, -341.0/164.0, 4496.0/1025.0, -289.0/82.0, 2193.0/4100.0, 51.0/82.0, 33.0/164.0, 12.0/41.0, 0, 1.0
};
static const double RK8_gamma[RK8_n] = {
	0, 0, 0, 0, 0, 34.0/105.0, 9.0/35.0, 9.0/35.0, 9.0/280.0, 9.0/280.0, 0, 41.0/840.0, 41.0/840.0
};

// ===========================================================================
// Propagators for linear and angular state vectors combined
// ===========================================================================

// ---------------------------------------------------------------------------
// 2nd order 2-stage Runge-Kutta (linear+angular)
// ---------------------------------------------------------------------------

void RigidBody::RK2_LinAng (double h, int nsub, int isub)
{
	double h05 = h*0.5;

	Vector acc1, tau;
	StateVectors s;
	s.pos.Set (s1->pos+s1->vel*h05);
	s.vel.Set (s1->vel+acc*h05);
	s.SetRot (s1->Q.Rot(s1->omega*h05));
	s.omega.Set (s1->omega+arot*h05);
	GetIntermediateMoments (acc1, tau, s, (isub+0.5)/nsub, h);

	rpos_add += s.vel*h;
	rvel_add += acc1*h;
	s1->Q.Rotate (s.omega*h);
	arot = EulerInv_full (tau, s.omega); // may need to allow use of the simplified versions here
	s1->omega += arot*h;
}

// ---------------------------------------------------------------------------
// 4th order 4-stage Runge-Kutta (linear+angular)
// ---------------------------------------------------------------------------

void RigidBody::RK4_LinAng (double h, int nsub, int isub)
{
	double h05 = h*0.5;
	double hi6 = h/6.0;

	Vector tau, acc1, aacc1, acc2, aacc2, acc3, aacc3;
	StateVectors sa, sb, sc;
	sa.pos.Set (s1->pos+s1->vel*h05);
	sa.vel.Set (s1->vel+acc*h05);
	sa.SetRot (s1->Q.Rot(s1->omega*h05));
	sa.omega.Set (s1->omega+arot*h05);
	GetIntermediateMoments (acc1, tau, sa, (isub+0.5)/nsub, h);
	aacc1.Set (EulerInv_full (tau, sa.omega));

	sb.pos.Set (s1->pos+sa.vel*h05);
	sb.vel.Set (s1->vel+acc1*h05);
	sb.SetRot (s1->Q.Rot(sa.omega*h05));
	sb.omega.Set (s1->omega+aacc1*h05);
	GetIntermediateMoments (acc2, tau, sb, (isub+0.5)/nsub, h);
	aacc2.Set (EulerInv_full (tau, sb.omega));

	sc.pos.Set (s1->pos+sb.vel*h);
	sc.vel.Set (s1->vel+acc2*h);
	sc.SetRot (s1->Q.Rot(sb.omega*h));
	sc.omega.Set (s1->omega+aacc2*h);
	GetIntermediateMoments (acc3, tau, sc, (isub+1.0)/nsub, h);
	aacc3.Set (EulerInv_full (tau, sc.omega));

	rvel_add += (acc +(acc1+acc2)*2.0+acc3)*hi6;
	rpos_add += (s1->vel+(sa.vel+sb.vel)*2.0+sc.vel)*hi6;
	s1->Q.Rotate ((s1->omega+(sa.omega+sb.omega)*2.0+sc.omega)*hi6);
	s1->omega += (arot+(aacc1+aacc2)*2.0+aacc3)*hi6;
}

// ---------------------------------------------------------------------------
// Driver routine for Runge-Kutta solvers RK5-RK8 (linear+angular)
// Note: NOT THREADSAFE
// ---------------------------------------------------------------------------

void RigidBody::RKdrv_LinAng (double h, int nsub, int isub, int n, const double *alpha, const double *beta, const double *gamma)
{
	int i, j;
	double bh;
	static int nbuf = 8;
	static StateVectors *s = new StateVectors[nbuf]; TRACENEW
	static Vector *a       = new Vector[nbuf]; TRACENEW  // linear acceleration
	static Vector *d       = new Vector[nbuf]; TRACENEW  // angular acceleration
	Vector tau;
	if (n > nbuf) { // grow buffers
		delete []s;
		delete []a;
		delete []d;
		s = new StateVectors[n]; TRACENEW
		a = new Vector[n]; TRACENEW
		d = new Vector[n]; TRACENEW
		nbuf = n;
	}

	s[0].Set (s1->vel, s1->pos, s1->omega, s1->Q);
	a[0].Set (acc);
	d[0].Set (arot);

	for (i = 1; i < n; i++) {
		s[i].Set (s1->vel, s1->pos, s1->omega, s1->Q);
		for (j = 0; j < i; j++)
			s[i].Advance (beta[j]*h, a[j], s[j].vel, d[j], s[j].omega);
		GetIntermediateMoments (a[i],tau,s[i],(isub+alpha[i-1])/nsub, h);
		d[i].Set (EulerInv_full (tau, s[i].omega));
		beta += n-1;
	}
	for (i = 0; i < n; i++) {
		bh = gamma[i]*h;
		rvel_add += a[i]       * bh;
		rpos_add += s[i].vel   * bh;
		s1->Q.Rotate (s[i].omega * bh);
		s1->omega += d[i]      * bh;
	}
}

// ---------------------------------------------------------------------------
// 5th order 6-stage Runge-Kutta (linear+angular)
// ---------------------------------------------------------------------------

void RigidBody::RK5_LinAng (double h, int nsub, int isub)
{
	RKdrv_LinAng (h, nsub, isub, RK5_n, RK5_alpha, RK5_beta, RK5_gamma);
}

// ---------------------------------------------------------------------------
// 6th order 8-stage Runge-Kutta (linear+angular)
// ---------------------------------------------------------------------------

void RigidBody::RK6_LinAng (double h, int nsub, int isub)
{
	RKdrv_LinAng (h, nsub, isub, RK6_n, RK6_alpha, RK6_beta, RK6_gamma);
}

// ---------------------------------------------------------------------------
// 7th order 11-stage Runge-Kutta (linear+angular)
// ---------------------------------------------------------------------------

void RigidBody::RK7_LinAng (double h, int nsub, int isub)
{
	RKdrv_LinAng (h, nsub, isub, RK7_n, RK7_alpha, RK7_beta, RK7_gamma);
}

// ---------------------------------------------------------------------------
// 8th order 13-stage Runge-Kutta (linear+angular)
// ---------------------------------------------------------------------------

void RigidBody::RK8_LinAng (double h, int nsub, int isub)
{
	RKdrv_LinAng (h, nsub, isub, RK8_n, RK8_alpha, RK8_beta, RK8_gamma);
}


// ---------------------------------------------------------------------------
// 2nd order symplectic propagator (linear+angular)
// Note: the propagation of angular state is guesswork ...
// ---------------------------------------------------------------------------

void RigidBody::SY2_LinAng (double h, int nsub, int isub)
{
	double h05 = h*0.5;
	StateVectors s;
	Vector tau;
	rpos_add += (rvel_base+rvel_add)*h05;
	s1->Q.Rotate (s1->omega*h05);
	s.Set (s1->vel, rpos_base+rpos_add, s1->omega, s1->Q);
	GetIntermediateMoments (acc, tau, s, (isub+0.5)/nsub, h);
	arot.Set (EulerInv_full (tau, s.omega));
	rvel_add += acc*h;
	s1->omega += arot*h;
	rpos_add += (rvel_base+rvel_add)*h05;
	s1->Q.Rotate (s1->omega*h05);
}

// ---------------------------------------------------------------------------
// 4nd order symplectic propagator (linear+angular)
// Note: the propagation of angular state is guesswork ...
// ---------------------------------------------------------------------------

void RigidBody::SY4_LinAng (double h, int nsub, int isub)
{
	int i;
	double sec = 0.0;
	static const double b = 1.25992104989487319066654436028;      // 2^1/3
	static const double a = 2 - b;
	static const double x0 = -b / a;
	static const double x1 = 1. / a;
	static const double d4[3] = {x1, x0, x1};
	static const double c4[4] = {x1/2, (x0+x1)/2, (x0+x1)/2, x1/2};
	StateVectors s;
	Vector tau;

	for (i = 0; i < 4; i++) {
		double step = h * c4[i];
		rpos_add += (rvel_base+rvel_add)*step;
		s1->Q.Rotate (s1->omega*step);
		sec += c4[i];
		if (i != 3) {
			s.Set (rvel_base+rvel_add, rpos_base+rpos_add, s1->omega, s1->Q);
			GetIntermediateMoments (acc, tau, s, (isub+sec)/nsub, h);
			arot.Set (EulerInv_full (tau, s.omega));
			rvel_add += acc * (h*d4[i]);
			s1->omega += arot * (h*d4[i]);
		}
	}
}

// ---------------------------------------------------------------------------
// 6nd order symplectic propagator (linear+angular)
// Note: the propagation of angular state is guesswork ...
// ---------------------------------------------------------------------------

void RigidBody::SY6_LinAng (double h, int nsub, int isub)
{
	int i;
	double sec = 0.0;
	static const double w1 = -0.117767998417887E1;
	static const double w2 = 0.235573213359357E0;
	static const double w3 = 0.784513610477560E0;
	static const double w0 = (1-2*(w1+w2+w3));
	static const double d6[7] = { w3, w2, w1, w0, w1, w2, w3 };
	static const double c6[8] = { w3/2, (w3+w2)/2, (w2+w1)/2, (w1+w0)/2,
                         (w1+w0)/2, (w2+w1)/2, (w3+w2)/2, w3/2 };
	StateVectors s;
	Vector tau;

	for (i = 0; i < 8; i++) {
		double step = h * c6[i];
		rpos_add += (rvel_base+rvel_add)*step;
		s1->Q.Rotate (s1->omega*step);
		sec += c6[i];
		if (i != 7) {
			s.Set (rvel_base+rvel_add, rpos_base+rpos_add, s1->omega, s1->Q);
			GetIntermediateMoments (acc, tau, s, (isub+sec)/nsub, h);
			arot.Set (EulerInv_full (tau, s.omega));
			rvel_add += acc * (h*d6[i]);
			s1->omega += arot * (h*d6[i]);
		}
	}
}

// ---------------------------------------------------------------------------
// 8nd order symplectic propagator (linear+angular)
// Note: the propagation of angular state is guesswork ...
// ---------------------------------------------------------------------------

void RigidBody::SY8_LinAng (double h, int nsub, int isub)
{
	int i;
	double sec = 0.0;
	// set 3 from Yoshida's Table 2
	const double W1 =  0.311790812418427e0;
	const double W2 = -0.155946803821447e1;
	const double W3 = -0.167896928259640e1;
	const double W4 =  0.166335809963315e1;
	const double W5 = -0.106458714789183e1;
	const double W6 =  0.136934946416871e1;
	const double W7 =  0.629030650210433e0;
	const double W0 = (1-2*(W1+W2+W3+W4+W5+W6+W7));

	const static double d8[15] = {W7, W6, W5, W4, W3, W2, W1, W0,
		                          W1, W2, W3, W4, W5, W6, W7};
	const static double c8[16] = { W7/2, (W7+W6)/2, (W6+W5)/2, (W5+W4)/2,
                         (W4+W3)/2, (W3+W2)/2, (W2+W1)/2, (W1+W0)/2,
                         (W1+W0)/2, (W2+W1)/2, (W3+W2)/2, (W4+W3)/2,
                         (W5+W4)/2, (W6+W5)/2, (W7+W6)/2,  W7/2 };
	StateVectors s;
	Vector tau;

	for (i = 0; i < 16; i++) {
		double step = h * c8[i];
		rpos_add += (rvel_base+rvel_add)*step;
		s1->Q.Rotate (s1->omega*step);
		sec += c8[i];
		if (i != 15) {
			s.Set (rvel_base+rvel_add, rpos_base+rpos_add, s1->omega, s1->Q);
			GetIntermediateMoments (acc, tau, s, (isub+sec)/nsub, h);
			arot.Set (EulerInv_full (tau, s.omega));
			rvel_add += acc * (h*d8[i]);
			s1->omega += arot * (h*d8[i]);
		}
	}
}

#ifdef UNDEF
// ===========================================================================
// Propagators for 2-body orbit perturbations
// ===========================================================================

// ---------------------------------------------------------------------------
// 2nd order 2-stage Runge-Kutta (perturbation)
// ---------------------------------------------------------------------------

void RigidBody::RK2_LinAng_Encke (double h, int nsub, int isub)
{
	double t = td.SimT0 + h;
	double mu = Ggrav * ((Body*)cbody)->Mass();
	Vector Rb, Vb, alpha, beta;
	double rb, r, f;

	el->PosVel (Rb, Vb, t);
	rb = Rb.length();
	r = cpos.length();
	f = rb/r;
	sprintf (DBG_MSG, "fac=%e", 1.0-f*f*f);
	beta = cpos*(mu*h*(1.0-f*f*f)/(rb*rb*rb)) + acc_pert*h;
	alpha = beta*h;
	cpos = Rb+alpha;
	cvel = Vb+beta;
}

void RigidBody::RK2_Pert (const PertIntData &data)
{
	double h05 = data.dt*0.5;
	Vector cpos05 = el->Pos (data.t1-h05);
	// unperturbed position at half the current step

	Vector accp (GetPertAcc (data, data.p0, 0.0));
	Vector v1 (data.dv + accp*h05);
	Vector a1 (GetPertAcc (data, cpos05+data.dv*h05, 0.5));

	cpos += v1*data.dt;
	cvel += data.dv+a1*data.dt;
}

// ---------------------------------------------------------------------------
// 4th order 4-stage Runge-Kutta (perturbation)
// ---------------------------------------------------------------------------

void RigidBody::RK4_Pert (const PertIntData &data)
{
	double h05 = data.dt*0.5;
	double hi6 = data.dt/6.0;
	Vector cpos05 = el->Pos (data.t1-h05);
	// unperturbed position at half the current step

	Vector accp (GetPertAcc (data, data.p0, 0.0));
	Vector v1 (data.dv + accp*h05);
	Vector a1 (GetPertAcc (data, cpos05+data.dv*h05, 0.5));
	Vector v2 (data.dv + a1*h05);
	Vector a2 (GetPertAcc (data, cpos05+v1*h05, 0.5));
	Vector v3 (data.dv + a2*data.dt);
	Vector a3 (GetPertAcc (data, cpos+v2*data.dt, 1.0));

	cpos += (data.dv+(v1+v2)*2.0+v3)*hi6;
	cvel += (accp+(a1+a2)*2.0+a3)*hi6;
}

// ---------------------------------------------------------------------------
// Driver routine for Runge-Kutta solvers RK5-RK8 (perturbation)
// Note: NOT THREADSAFE
// ---------------------------------------------------------------------------

void RigidBody::RKdrv_Pert (const PertIntData &data, int n, const double *alpha, const double *beta, const double *gamma)
{
	int i, j;
	static int nbuf = 8;

	static Vector *v = new Vector[nbuf];
	static Vector *a = new Vector[nbuf];
	if (n > nbuf) { // grow buffers
		delete []v;
		delete []a;
		v = new Vector[n]; TRACENEW
		a = new Vector[n]; TRACENEW
		nbuf = n;
	}
	Vector pos, vtmp;
	v[0] = data.dv;
	a[0] = GetPertAcc (data, data.p0, 0.0);
	for (i =1; i < n; i++) {
		v[i] = data.dv;
		pos = el->Pos (data.t1+data.dt*(alpha[i-1]-1.0));
		for (j = 0; j < i; j++) {
			v[i] += a[j]*(beta[j]*data.dt);
			pos += v[j]*(beta[j]*data.dt);
		}
		a[i] = GetPertAcc (data, pos, alpha[i-1]);
		beta += n-1;
	}
	for (i = 0; i < n; i++) {
		cpos += v[i]*(gamma[i]*data.dt);
		cvel += a[i]*(gamma[i]*data.dt);
	}
}

// ---------------------------------------------------------------------------
// 8th order 13-stage Runge-Kutta (perturbation)
// ---------------------------------------------------------------------------

void RigidBody::RK8_Pert (const PertIntData &data)
{
	RKdrv_Pert (data, RK8_n, RK8_alpha, RK8_beta, RK8_gamma);
}
#endif


void RigidBody::Encke ()
{
	static double ch[13] = {
		0, 0, 0, 0, 0, 34.0/105.0, 9.0/35.0, 9.0/35.0,
		9.0/280.0, 9.0/280.0, 0, 41.0/840.0, 41.0/840.0
	};
	static double alph[13] = {
		0, 2.0/27.0, 1.0/9.0, 1.0/6.0, 5.0/12.0, 0.5,
		5.0/6.0, 1.0/6.0, 2.0/3.0, 1.0/3.0, 1, 0, 1
	};
	static double beta[13][12] = {
		{ 0,             0,        0,        0,           0,             0,          0,            0,         0,         0,        0,0  },
		{ 2.0/27.0,      0,        0,        0,           0,             0,          0,            0,         0,         0,        0,0  },
		{ 1.0/36.0,      1.0/12.0, 0,        0,           0,             0,          0,            0,         0,         0,        0,0  },
		{ 1.0/24.0,      0,        1.0/8.0,  0,           0,             0,          0,            0,         0,         0,        0,0  },
		{ 5.0/12.0,      0,       -25.0/16.0,25.0/16.0,   0,             0,          0,            0,         0,         0,        0,0  },
		{ 0.05,          0,        0,        0.25,        0.2,           0,          0,            0,         0,         0,        0,0  },
		{-25.0/108.0,    0,        0,        125.0/108.0,-65.0/27.0,     125.0/54.0, 0,            0,         0,         0,        0,0  },
		{ 31.0/300.0,    0,        0,        0,           61.0/225.0,   -2.0/9.0,    13.0/900.0,   0,         0,         0,        0,0  },
		{ 2.0,           0,        0,       -53.0/6.0,    704.0/45.0,   -107.0/9.0,  67.0/90.0,    3.0,       0,         0,        0,0  },
		{-91.0/108.0,    0,        0,        23.0/108.0, -976.0/135.0,   311.0/54.0,-19.0/60.0,    17.0/6.0, -1.0/12.0,  0,        0,0  },
		{ 2383.0/4100.0, 0,        0,       -341.0/164.0, 4496.0/1025.0,-301.0/82.0, 2133.0/4100.0,45.0/82.0, 45.0/164.0,18.0/41.0,0,0  },
		{ 3.0/205.0,     0,        0,        0,           0,            -6.0/41.0,  -3.0/205.0,   -3.0/41.0,  3.0/41.0,  6.0/41.0, 0,0  },
		{-1777.0/4100.0, 0,        0,       -341.0/164.0, 4496.0/1025.0,-289.0/82.0, 2193.0/4100.0,51.0/82.0, 33.0/164.0,12.0/41.0,0,1.0}};

	const int neq = 6;
	int i, j, k, kk;
	double f[6][13], x[6], xwrk[6], temp, t0, ti, tf, twrk, dt, tfrac;
	Vector d[13], omega[13], tau; // angular state parameters
	StateVectors s;

	ti = t0 = td.SimT0;
	tf = td.SimT1;
	dt = (tf-ti)*1;

	for (i = 0; i < 6; i++)
		x[i] = 0.0;

	while (1) {

		twrk = ti;
		for (i = 0; i < 6; i++) xwrk[i] = x[i];

		// check for last step
		if (dt > tf-ti) dt = tf-ti;

		// check for end of integration period
		if (fabs (ti-tf) < 1e-8)
			break;

		tfrac = (ti-t0)/(tf-t0);
		for (i = 0; i < 3; i++) {
			s.pos.data[i] = cpos.data[i]+x[i];
			s.vel.data[i] = cvel.data[i]+x[i+3];
		}
		GetIntermediateMoments_pert (acc_pert, tau, s, tfrac, dt, cbody);

		for (i = 0; i < 3; i++) {
			f[i][0] = x[i+3];
			f[i+3][0] = acc_pert.data[i];
		}

		// angular state
		omega[0].Set (s1->omega);
		d[0].Set (arot);

		for (k = 1; k < 13; k++) {
			kk = k-1;
			for (i = 0; i < neq; i++) {
				temp = 0.0;
				for (j = 0; j < kk; j++) {
					temp += beta[k][j] * f[i][j];
				}
				x[i] = xwrk[i] + dt*temp;
			}
			omega[k].Set (s1->omega);
			for (j = 0; j < kk; j++) {
				omega[k] += d[j]*(beta[k][j]*dt);
			}
			ti = twrk + alph[k] * dt;
			tfrac = (ti-t0)/(tf-t0);
			el->PosVel (cpos, cvel, ti);
			for (i = 0; i < 3; i++) {
				s.pos.data[i] = cpos.data[i]+x[i];
				s.vel.data[i] = cvel.data[i]+x[i+3];
			}
			GetIntermediateMoments_pert (acc_pert, tau, s, tfrac, dt, cbody);
			for (i = 0; i < 3; i++) {
				f[i][k] = x[i+3];
				f[i+3][k] = acc_pert.data[i];
			}
			d[k].Set (EulerInv_simple (tau, omega[k]));
		}

		for (i = 0; i < neq; i++) {
			temp = 0.0;
			for (k = 0; k < 13; k++) {
				temp += ch[k] * f[i][k];
			}
			x[i] = xwrk[i] + dt*temp;
		}
		for (k = 0; k < 13; k++) {
			s1->Q.Rotate (omega[k] * (ch[k]*dt));
			s1->omega += d[k] * (ch[k]*dt);
		}

		// truncation error
		const double tetol = 1e-8;
		double xerr = tetol;
		for (i = 0; i < neq; i++) {
			double ter = fabs((f[i][0] + f[i][10] - f[i][11] - f[i][12]) * ch[11]*dt);
			double tol = fabs(x[i]) * tetol + tetol;
			double tconst = ter/tol;
			if (tconst > xerr) xerr = tconst;
		}

		el->PosVel (cpos, cvel, ti);
		for (i = 0; i < 3; i++) {
			cpos.data[i] += x[i];
			cvel.data[i] += x[i+3];
		}
		// rectify elements
		el->Calculate (cpos, cvel, ti);

		for (i = 0; i < 6; i++) x[i] = 0.0;
	}
}
