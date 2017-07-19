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

#include "Orbit.h"


// =================================================================================================
//
COrbit::COrbit()
{
	_Equ = _I_ECL;
	_Aux = _J_ECL;
	_Pol = _J_ECL;
}

// =================================================================================================
//
COrbit::~COrbit()
{

}

// =================================================================================================
//
COrbit::COrbit(const COrbit &in)
{
	_Equ = in._Equ;
	_Aux = in._Aux;
	_Pol = in._Pol;
	memcpy((void *)this, (void *)&in, sizeof(COrbit));
}

// =================================================================================================
//
COrbit::COrbit(const VECTOR3 _pos, const VECTOR3 _vel, double imu, double iepoch)
{
	_Equ = _I_ECL;
	_Aux = _J_ECL;
	_Pol = _K_ECL;
	CreateFromStateVectors(_pos, _vel, imu, iepoch);
}

// =================================================================================================
//
COrbit::COrbit(OBJHANDLE hObj, OBJHANDLE hRef)
{
	Create(hObj, hRef);
}

// =================================================================================================
//
void COrbit::Create(OBJHANDLE hObj, OBJHANDLE hRef)
{
	VECTOR3 _Pos, _Vel;

	oapiGetRelativePos(hObj, hRef, &_Pos);
	oapiGetRelativeVel(hObj, hRef, &_Vel);

	CreateFromStateVectors(_Pos, _Vel, (oapiGetMass(hObj) + oapiGetMass(hRef))*GC, oapiGetSimMJD());
}

// =================================================================================================
//
void COrbit::Create(VESSEL *hVes)
{
	VECTOR3 _Pos, _Vel;

	OBJHANDLE hR = hVes->GetGravityRef();

	hVes->GetRelativePos(hR, _Pos);
	hVes->GetRelativeVel(hR, _Vel);

	CreateFromStateVectors(_Pos, _Vel, oapiGetMass(hR)*GC, oapiGetSimMJD());
}

// =================================================================================================
//
void COrbit::ReferencePole(const VECTOR3 _Equinox, const VECTOR3 _Pole)
{
	_Equ = unit(_Equinox);
	_Aux = unit(crossp_LH(_Pole, _Equinox));
	_Pol = crossp_LH(_Equ, _Aux);
}

// =================================================================================================
//
double COrbit::EcAByMJD(double MJD) const
{
	double t = (MJD - epoch) * 86400.0;
	return EcAByTime(t);
}

// =================================================================================================
//
double COrbit::TrAByMJD(double MJD) const
{
	double t = (MJD - epoch) * 86400.0;
	return TrAByTime(t);
}

// =================================================================================================
//
double COrbit::EcAByTime(double Time) const
{
	double m = mna + (mnm * Time);
	if (ecc>1.0) return mna2eca(m, ecc);	// Hyperbolic
	return mna2eca(limit(m), ecc);			// Elliptic
}

// =================================================================================================
//
double COrbit::TrAByTime(double Time) const
{
	double m = mna + (mnm * Time);
	if (ecc>1.0) return mna2tra(m, ecc);	// Hyperbolic
	return mna2tra(limit(m), ecc);			// Elliptic
}

// =================================================================================================
//
double COrbit::TimeToTrA(double t) const
{
	double m = tra2mna(t, ecc);
	if (ecc>1.0) return (m - mna) / mnm;	// Hyperbolic
	m = _ad(mna, m); if (m>PI) m -= PI2;	// Elliptic
	return m / mnm;
}

// =================================================================================================
//
double COrbit::TimeToEcA(double e) const
{
	double m = eca2mna(e, ecc);
	if (ecc>1.0) return (m - mna) / mnm;	// Hyperbolic
	m = _ad(mna, m); if (m>PI) m -= PI2;	// Elliptic
	return m / mnm;
}

// =================================================================================================
//
double COrbit::PeMJD() const
{
	if (ecc>1.0) return epoch - (mna / mnm)*OPDAY;	// Hyperbolic
	double m = _ad(mna, 0); if (m>PI) m -= PI2;	// Elliptic
	return epoch + (m / mnm)*OPDAY;
}

// =================================================================================================
//
VECTOR3 COrbit::PosByEcA(double eca) const
{
	if (ecc < 1.0) return _P * ((cos(eca) - ecc)*sma) + _Q * (sin(eca)*smi);
	return _P * ((cosh(eca) - ecc)*sma) + _Q * (sinh(eca)*smi);
}

// =================================================================================================
//
FVECTOR2 COrbit::PQPosByTrA(double tra) const
{
	double ec = tra2eca(tra, ecc);
	if (ecc < 1.0) return { float((cos(ec) - ecc)*sma), float(sin(ec)*smi) };
	return { float((cosh(ec) - ecc)*sma), float(sinh(ec)*smi) };
}

// =================================================================================================
//
VECTOR3 COrbit::PosByTrA(double tra) const
{
	return PosByEcA(tra2eca(tra, ecc));
}
// =================================================================================================
//
VECTOR3 COrbit::PosByMJD(double mjd) const
{
	return PosByEcA(EcAByMJD(mjd));
}

// =================================================================================================
//
VECTOR3 COrbit::VelByTrA(double tra) const
{
	return _Q * ((ecc + cos(tra))*up) - _P * (sin(tra)*up);
}

// =================================================================================================
//
VECTOR3 COrbit::VelByEcA(double eca) const
{
	return VelByTrA(eca2tra(eca, ecc));
}

// =================================================================================================
//
VECTOR3 COrbit::VelByMJD(double mjd) const
{
	return VelByTrA(TrAByMJD(mjd));
}

// =================================================================================================
//
void COrbit::PosVelByMJD(double mjd, VECTOR3 *_pos, VECTOR3 *_vel) const
{
	double s, c;
	double e = EcAByMJD(mjd);
	double t = eca2tra(e, ecc);

	if (ecc>1.0) s = sinh(e), c = cosh(e);
	else         s = sin(e), c = cos(e);

	if (_vel) *_vel = _Q * ((ecc + cos(t))*up) - _P * (sin(t)*up);
	if (_pos) *_pos = _P * ((c - ecc)*sma) + _Q * (s*smi);
}

// =================================================================================================
//
double COrbit::TimeToRelTrA(double tra) const
{
	double f = 0.0;

	if (tra>PI2)  f = floor(tra / PI2);
	else if (tra<-PI2) f = -floor(-tra / PI2) - 1.0;
	else return 0;

	double m = _ad(mna, tra2mna(limit(eca2tra(eca, ecc) + tra), ecc));

	return (f*PI2 + m) / mnm;
}

// =================================================================================================
//
double COrbit::RelTrAByTime(double time) const
{
	double f = 0.0;
	double m = time * mnm;

	if (m>0.0) f = floor(m / PI2);
	else if (m<0.0) f = -floor(-m / PI2) - 1.0;
	else	return 0;

	double t = _ad(mna2tra(mna, ecc), mna2tra(limit(mna + m), ecc));

	return (f*PI2) + t;
}

// =================================================================================================
//
double COrbit::MJDByRelTrA(double tra) const
{
	return epoch + TimeToRelTrA(tra) * OPDAY;
}

// =================================================================================================
//
double COrbit::RelTrAByMJD(double mjd) const
{
	return RelTrAByTime((mjd - epoch)*86400.0);
}

// =================================================================================================
//
double COrbit::TrAOfProjection(const VECTOR3 _p) const
{
	return limit(atan2(dotp(_p, _Q), dotp(_p, _P)));
}

// =================================================================================================
//
double COrbit::TrAOfAscendingNode(const VECTOR3 _n) const
{
	VECTOR3 _p = crossp_LH(_n, _W);
	return limit(atan2(dotp(_p, _Q), dotp(_p, _P)));
}

// =================================================================================================
//
double COrbit::Vel() const
{
	return sqrt(2.0*mu / Rad() - mu / sma);
}

// =================================================================================================
//
double COrbit::Rad() const
{
	return sma*(1.0 - ecc*cos(eca));
}

// =================================================================================================
//
double COrbit::Inc() const
{
	return angle(_Pol, _W);
}

// =================================================================================================
//
double COrbit::LAN() const
{
	VECTOR3 _p = crossp_LH(_Pol, _W);
	double x = atan2(dotp(_p, _Aux), dotp(_p, _Equ));
	if (x<0) return PI2 + x; return x;
}

// =================================================================================================
//
double COrbit::AgP() const
{
	VECTOR3 _p = crossp_LH(_Pol, _W);
	return PI2 - limit(atan2(dotp(_p, _Q), dotp(_p, _P)));
}

// =================================================================================================
//
double COrbit::PeT() const
{
	return _ad(mna, 0.0) / mnm;
}

// =================================================================================================
//
double COrbit::ApT() const
{
	return _ad(mna, PI) / mnm;
}

// =================================================================================================
//
double COrbit::PeV() const
{
	return sqrt(2.0*mu / PeD() - mu / sma);
}

// =================================================================================================
//
double COrbit::Heading(double TrA, VECTOR3 &_Rot) const
{
	VECTOR3 _Pos = PosByTrA(TrA);
	VECTOR3 _East = crossp_LH(_Rot, _Pos);
	VECTOR3 _North = crossp_LH(_Pos, _East);
	return GetAngle(VelByTrA(TrA), _North, _East);
}

// =================================================================================================
//
double COrbit::FlightPathAngle(double TrA) const
{
	return eca2fpa(tra2eca(TrA, ecc), ecc);
}

// =================================================================================================
//
double COrbit::EscapeVelocity() const
{
	if (ecc<1.0) return -1.0;
	return sqrt(-mu / sma);
}

// =================================================================================================
//
double COrbit::TrAByRadius(double rad) const
{
	if (rad>sma*(1.0 - ecc)) {
		if (ecc>1.0)		     return acos((par - rad) / (rad*ecc));
		if (rad<sma*(1.0 + ecc)) return acos((par - rad) / (rad*ecc));
	}
	return -1.0;
}

// =================================================================================================
//
double COrbit::RadiusByTrA(double TrA) const
{
	return par / (1.0 + ecc*cos(TrA));
}

// =================================================================================================
//
double COrbit::RadiusByEcA(double EcA) const
{
	if (ecc<1.0) return sma * (1.0 - ecc*cos(EcA));
	else         return sma * (1.0 - ecc*cosh(EcA));
}

// =================================================================================================
//
double COrbit::MaxNrA() const
{
	if (ecc>1.0) return eca2nra(10.0, ecc);
	return PI2;
}

// =================================================================================================
//
bool COrbit::IsTrAValid(double TrA) const
{
	if (ecc > 1.0) {
		if (ecc*cos(TrA) > -1.0) return true;
		return false;
	}
	return true;
}

// =================================================================================================
//
void COrbit::CreateFromStateVectors(const VECTOR3 &_pos, const VECTOR3 &_vel, double imu, double iepoch)
{
	VECTOR3 _H, _R;
	epoch = iepoch;
	mu = imu;

	double tra;
	double v2 = sqrlen(_vel);
	double r2 = sqrlen(_pos);
	double  r = sqrt(r2);
	double or = 1.0 / r;
	double om = 1.0 / mu;

	// Eccentricity VECTOR3
	_P = ((_pos * (v2 - mu* or )) - (_vel * dotp(_pos, _vel))) * om;
	_H = crossp_LH(_pos, _vel);
	_Q = unit(crossp_LH(_H, _P));
	_R = _pos* or ;

	// Eccentricity
	ecc = length(_P);

	if (ecc < 1e-7) {
		ecc = 0.0;
		VECTOR3 _i = unit(crossp_LH(_K_ECL, _H));
		VECTOR3 _j = unit(crossp_LH(_H, _i));
		_P = -unit(_j*dotp(_i, _J_ECL) - _i*dotp(_i, _I_ECL));
		_Q = unit(crossp_LH(_H, _P));
		par = sqrlen(_H)*om;
		sma = par;
	}
	else {
		_P /= ecc;
		par = sqrlen(_H)*om;
		sma = par / (1.0 - ecc*ecc);
	}

	// Calculate True anomaly
	//
	double x = dotp(_P, _R);
	if (x >= 1.0)  tra = 0.0;
	else if (x <= -1.0) tra = PI;
	else {
		tra = acos(x);
		x = dotp(_pos, _vel);
		if (fabs(x)<1e-9) x = 0.0; // Avoid some precision problems
		if (x <= 0.0) tra = PI2 - tra;
	}

	eca = tra2eca(tra, ecc);
	mna = eca2mna(eca, ecc);
	mnm = sqrt(mu / fabs(sma * sma * sma));
	up = sqrt(mu / par);
	smi = sqrt(fabs(sma*par));

	_W = crossp_LH(_P, _Q);
}

// =================================================================================================
//
void COrbit::CreateFromElements(double iSMa, double iEcc, double iInc, double iLAN, double iAgP, double iMnA, double iMu, double iEpoch)
{
	PQW(iLAN, iInc, iAgP, _Equ, _Pol, &_P, &_Q);

	sma = iSMa;
	ecc = iEcc;
	mna = iMnA;
	eca = mna2eca(mna, ecc);
	par = sma * (1.0 - ecc*ecc);
	smi = sqrt(fabs(par*sma));
	up = sqrt(mu / par);
	mu = iMu;
	epoch = iEpoch;

	_W = crossp_LH(_P, _Q);
}

// =================================================================================================
//
void COrbit::CreateFromElements(const ELEMENTS *Elem, double iMu, double iEpoch)
{
	double AgP = limit(Elem->omegab - Elem->theta);

	PQW(Elem->theta, Elem->i, AgP, _Equ, _Aux, &_P, &_Q);

	sma = Elem->a;
	ecc = Elem->e;
	mna = limit(Elem->L - Elem->theta);
	eca = mna2eca(mna, ecc);
	par = sma * (1.0 - ecc*ecc);
	smi = sqrt(fabs(par*sma));
	up = sqrt(mu / par);
	mu = iMu;
	epoch = iEpoch;

	_W = crossp_LH(_P, _Q);
}


// =================================================================================================
//
void COrbit::EscapeOrbit(const VECTOR3 &_Pos, const VECTOR3 &_Esc, double Mu, double MJD, double Dir)
{
	VECTOR3 _H = crossp_LH(_Pos, _Esc);
	double Es2 = dotp(_Esc, _Esc);

	mu = Mu;
	sma = -mu / Es2;
	epoch = MJD;

	double rad = length(_Pos);
	double ang = PI2 - angle(_Pos, _Esc);

	if (Dir<0.0) { ang = PI2 - ang; _H = -_H; }

	double sq = sin(ang);
	double q = rad*rad*sq*sq;
	double w = 2.0*sma*rad*(cos(ang) - 1.0);

	ecc = sqrt((q - rad * sq * sqrt(q + 2.0*w) + w) / (2.0*sma*sma) + 1.0);

	par = sma * (1.0 - ecc*ecc);

	// Compute true anomaly 
	double x = (par - rad) / (rad*ecc);
	if (x>1.0) x = 1.0; if (x<-1.0) x = -1.0;

	double tra = acos(x);

	double y = PI + acos(1.0 / ecc);
	if (ang<y) tra = PI2 - tra;

	_P = CreateVector(_Pos, crossp_LH(_H, _Pos), PI2 - tra);
	_Q = unit(crossp_LH(_H, _P));

	eca = tra2eca(tra, ecc);
	mna = eca2mna(eca, ecc);

	mnm = sqrt(mu / fabs(sma*sma*sma));
	smi = sqrt(fabs(par*sma));
	up = sqrt(mu / par);

	_W = crossp_LH(_P, _Q);
}



// =================================================================================================
//
void COrbit::ApproachOrbit(const VECTOR3 &_Pos, const VECTOR3 &_Pe, double Mu, double MJD, double Dir)
{
	double tra, ped, rad; VECTOR3 _H;

	epoch = MJD;

	mu = Mu;
	ped = length(_Pe);
	rad = length(_Pos);
	_P = unit(_Pe);
	_H = crossp_LH(_Pos, _P);
	tra = angle(_Pos, _P); if (Dir<0.0) { tra = PI2 - tra; _H = -_H; }
	_Q = unit(crossp_LH(_H, _P));
	ecc = (ped - rad) / (rad*cos(tra) - ped);
	sma = ped / (1.0 - ecc);
	par = sma * (1.0 - ecc*ecc);
	eca = tra2eca(tra, ecc);
	mna = eca2mna(eca, ecc);
	mnm = sqrt(mu / fabs(sma*sma*sma));
	smi = sqrt(fabs(par*sma));
	up = sqrt(mu / par);

	_W = crossp_LH(_P, _Q);
}


// =================================================================================================
//
void COrbit::CreateCircular(const VECTOR3 &_Pos, const VECTOR3 &_N, double Mu, double MJD)
{
	VECTOR3 _Vel = unit(crossp_LH(_N, _Pos)) * sqrt(Mu / length(_Pos));
	CreateFromStateVectors(_Pos, _Vel, Mu, MJD);
}