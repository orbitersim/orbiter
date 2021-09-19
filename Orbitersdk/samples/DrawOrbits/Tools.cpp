
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

#include <math.h>
#include "Tools.h"
#include "OrbiterAPI.h"
#include "VesselAPI.h"
#include <stdio.h>
#include <time.h>
#include <assert.h>
#include <intrin.h>

char ValueToText_Str[32];

// ================================================================================================
//
const char *ValueToText(double real, int digits)
{
	char temp[32];

	memset(temp, 0, 32);
	memset(ValueToText_Str, 0, 32);

	if (_isnan(real)) { strcpy_s(ValueToText_Str, 32, "NAN"); return ValueToText_Str; }

	double v = fabs(real);
	int n;

	char *c = { "" };
	char *k = { "k" };
	char *k2 = { "M" };
	char *k3 = { "G" };
	char *k5 = { "m" };
	char *k6 = { "µ" };
	char *k7 = { "T" };

	n = (int)floor(log10(v)) + 1;

	if (digits>0) digits--;

	if (n>15) {
		if (digits<0) digits = 0;
		sprintf_s(temp, 30, "%1.*e\n", digits, real);
		strncpy_s(ValueToText_Str, 30, temp, 10);
		return ValueToText_Str;
	}

	if (digits>0) {

		if (n>12)		 real /= 1e12, c = k7;
		else if (n>9)	 real /= 1e9, c = k3;
		else if (n>6)	 real /= 1e6, c = k2;
		else if (n>4)	 real /= 1e3, c = k;
		else if (n<(-7)) real = 0.0;
		else if (n<(-4)) real *= 1e6, c = k6;
		else if (n<(-1)) real *= 1e3, c = k5;

		if (fabs(real) >= 9.9999999999999) digits--;
		if (fabs(real) >= 99.999999999999) digits--;
		if (fabs(real) >= 999.99999999999) digits--;
		if (fabs(real) >= 9999.9999999999) digits--;
		if (fabs(real) >= 99999.999999999) digits--;
	}
	else digits = -digits;

	if (digits<0) digits = 0;

	sprintf_s(ValueToText_Str, 30, "%1.*f", digits, real);
	strcat_s(ValueToText_Str, 30, c);

	return ValueToText_Str;
}


// ================================================================================================
//
const char *AngleToText(double deg, int digits)
{
	double m = pow(10.0, fabs((double)digits));
	double d = fabs(deg*m);
	double f = floor(d);

	if ((d - f) >= 0.5) f = (f + 1.0) / m;
	else            f = f / m;

	if (f >= 360.0) f = 0.0;
	if (deg<0) f = -f;

	sprintf_s(ValueToText_Str, 30, "%1.*f°", digits, f);

	return ValueToText_Str;
}

// ================================================================================================
//
void PQW(double lan, double inc, double agp, const VECTOR3 &_i, const VECTOR3 &_k, VECTOR3 *_p, VECTOR3 *_q)
{
	double sl = sin(lan);
	double cl = cos(lan);
	double ca = cos(agp);
	double sa = sin(agp);
	double si = sin(inc);
	double ci = cos(inc);

	VECTOR3 _j = crossp_LH(_k, _i);
	VECTOR3 _P = _i*(cl*ca - sl*ci*sa) + _j*(sl*ca + cl*ci*sa) + _k*(si*sa);
	VECTOR3 _Q = _i*(-cl*sa - sl*ci*ca) + _j*(-sl*sa + cl*ci*ca) + _k*(si*ca);

	if (_p) *_p = _P;
	if (_q) *_q = _Q;
}

// ================================================================================================
//
double GetISP(VESSEL *ship, THRUSTER_HANDLE th)
{
	if (th && ship) {
		PROPELLANT_HANDLE ph = ship->GetThrusterResource(th);

		if (ph) return ship->GetThrusterIsp0(th) * ship->GetPropellantEfficiency(ph);
		return ship->GetThrusterIsp0(th);
	}
	return 0;
}

// ================================================================================================
//
double GetThrusterGroupFlowRate(VESSEL *theVessel, THGROUP_TYPE thgt)
{
	double dec = 0.0;
	THRUSTER_HANDLE th;
	int num = theVessel->GetGroupThrusterCount(thgt);

	for (int index = 0; index < num; index++) {

		th = theVessel->GetGroupThruster(thgt, index);
		double isp = GetISP(theVessel, th);
		double thr = theVessel->GetThrusterMax(th);
		if (isp>0.0) dec += thr / isp;
	}
	return dec;
}

// ================================================================================================
//
double GetThrusterGroupISP(VESSEL *theVessel, THGROUP_TYPE thgt)
{
	int count = theVessel->GetGroupThrusterCount(thgt);
	if (count == 0) return 0;

	double A = 0.0, B = 0.0;

	for (int i = 0; i<count; i++) {
		THRUSTER_HANDLE th = theVessel->GetGroupThruster(thgt, 0);

		double F = theVessel->GetThrusterMax(th);
		double I = GetISP(theVessel, th);

		A += F; B += F / I;
	}

	return A / B;
}

// ================================================================================================
//
VECTOR3 GetThrusterGroupDir_LH(VESSEL *ship, THGROUP_TYPE engine)
{
	VECTOR3 d, dir = _V(0, 0, 0);
	double tot = 0.0;
	int i, c = ship->GetGroupThrusterCount(engine);

	for (i = 0; i<c; i++) {
		THRUSTER_HANDLE th = ship->GetGroupThruster(engine, i);
		ship->GetThrusterDir(th, d);
		double thr = ship->GetThrusterMax0(th);
		tot += thr;
		dir += (unit(d) * thr);
	}
	return dir / tot;
}

// ================================================================================================
//
VECTOR3 GetThrusterGroupThrustVector_LH(VESSEL *ship, THGROUP_TYPE engine)
{
	VECTOR3 d, dir = _V(0, 0, 0);
	int i, c = ship->GetGroupThrusterCount(engine);

	for (i = 0; i<c; i++) {
		THRUSTER_HANDLE th = ship->GetGroupThruster(engine, i);
		ship->GetThrusterDir(th, d);
		dir += (unit(d) * ship->GetThrusterMax0(th));
	}
	ship->GlobalRot(dir, dir);
	return dir;
}

// ================================================================================================
//
double GetThrusterGroupAcceleration(VESSEL *ship, THGROUP_TYPE engine)
{
	double tot = 0.0;
	int i, c = ship->GetGroupThrusterCount(engine);

	for (i = 0; i<c; i++) {
		THRUSTER_HANDLE th = ship->GetGroupThruster(engine, i);
		tot += ship->GetThrusterMax0(th);
	}

	return tot / GetSuperStructureMass(ship);
}

// ================================================================================================
//
double GetSuperStructureMass(VESSEL *hVessel)
{
	if (hVessel == NULL) return 0.0;

	double mass = 0.0;
	int Count;

	OBJHANDLE *List = GetSuperStructure(hVessel, Count);

	if (List) {
		for (int i = 0; i<Count; i++) mass += oapiGetMass(List[i]);
		delete List;
	}
	else mass = hVessel->GetMass();

	return mass;
}


// ================================================================================================
//
void GetSuperStructureSub(VESSEL *hVessel, OBJHANDLE *pList, int vc)
{
	if (hVessel == NULL) return;

	DOCKHANDLE dock; int c = hVessel->DockCount();

	for (int i = 0; i < c; i++) {

		dock = hVessel->GetDockHandle(i);

		if (dock) {

			OBJHANDLE hVes = hVessel->GetDockStatus(dock);

			if (hVes) for (int x = 0; x < vc; x++) {
				if (pList[x] == hVes) break;	// Vessel is allready in a list
				if (pList[x] == NULL) {
					pList[x] = hVes;
					GetSuperStructureSub(oapiGetVesselInterface(hVes), pList, vc);
					break;
				}
			}
		}
	}
}

// ================================================================================================
//
OBJHANDLE * GetSuperStructure(VESSEL *v, int &Count)
{
	int vc = oapiGetVesselCount();
	OBJHANDLE *pV = new OBJHANDLE[vc];

	memset((void *)pV, 0, sizeof(OBJHANDLE)*vc);

	pV[0] = v->GetHandle();

	GetSuperStructureSub(v, pV, vc);

	Count = 0;
	for (int i = 0; i < vc; i++) if (pV[i] != NULL) Count++; else break;
	return pV;
}


// ================================================================================================
//
double BurnTimeBydV(double dv, VESSEL *ship, THGROUP_TYPE engine)
{
	double isp = 0;
	double mas = GetSuperStructureMass(ship);
	double th = 0;
	int i, c = ship->GetGroupThrusterCount(engine);

	for (i = 0; i<c; i++) {
		THRUSTER_HANDLE hand = ship->GetGroupThruster(engine, i);
		th += ship->GetThrusterMax(hand);
		isp += GetISP(ship, hand);
	}

	if (th == 0.0 || isp == 0.0 || dv == 0.0) return 0.0;

	isp /= (double)c;

	double time = ((1.0 - exp(-dv / isp))*mas*isp) / th;

	return time;
}

// ================================================================================================
//
double dVByBurnTime(double time, VESSEL *ship, THGROUP_TYPE engine)
{
	double isp = 0;
	double mas = GetSuperStructureMass(ship);
	double th = 0;

	int i, c = ship->GetGroupThrusterCount(engine);

	for (i = 0; i<c; i++) {
		THRUSTER_HANDLE hand = ship->GetGroupThruster(engine, i);
		th += ship->GetThrusterMax(hand);
		isp += GetISP(ship, hand);
	}

	if (th == 0.0 || isp == 0.0 || time == 0.0) return 0.0;

	isp /= (double)c;

	double dv = -isp * log(-(time*th - mas*isp) / (mas*isp));

	return dv;
}

// ================================================================================================
//
double BurnTimeBydV(double dv, double mass, double thr, double rate)
{
	double isp = thr / rate;
	double time = ((1.0 - exp(-dv / isp))*mass*isp) / thr;
	return time;
}

// ================================================================================================
//
double dVByBurnTime(double time, double mass, double thr, double rate)
{
	double isp = thr / rate;
	double dv = -isp * log(-(time*thr - mass*isp) / (mass*isp));
	return dv;
}

// ================================================================================================
//
void VesselEulerAngles(VECTOR3 &_Dir, double *alpha, double *beta)
{
	*alpha = atan2(dotp(_Dir, _V(0, 1, 0)), dotp(_Dir, _V(0, 0, 1)));
	*beta = asin(dotp(unit(_Dir), _V(-1, 0, 0)));
}


// ================================================================================================
//
double mna2eca(double mna, double ecc)
{

	// iterative calculation of eccentric anomaly from mean anomaly
	register double eca, m, x;
	int i;

	if (ecc<1.0) {

		eca = mna;
		m = mna - eca + ecc*sin(eca);

		for (i = 0; (fabs(m)>1e-14 && i<22); i++) {
			x = m / (1.0 - ecc*cos(eca));
			if (x>1.0) x = 1.0; else if (x<-1.0) x = -1.0;
			eca += x;
			m = mna - eca + ecc*sin(eca);
		}
		return eca;
	}


	else {
		eca = 0;
		m = mna - ecc * sinh(eca) + eca;
		for (i = 0; fabs(m)>1e-14 && i<22; i++) {

			x = m / (ecc * cosh(eca) - 1.0);

			if (x>1.0) x = 1.0; else if (x<-1.0) x = -1.0;
			eca += x;

			m = mna - ecc * sinh(eca) + eca;
		}
	}

	return eca;
}

// ================================================================================================
//
double tra2eca(double tra, double e)
{
	double eca;
	double cos_tra = cos(tra);

	if (e>1.0) {
		eca = acosh((e + cos_tra) / (1.0 + e*cos_tra));
		if (tra>PI) eca = -eca;
		return(eca);
	}

	eca = acos((e + cos_tra) / (1.0 + e*cos_tra));
	if (tra>PI) eca = PI2 - eca;
	return(eca);
}

// ================================================================================================
//
double tra2mna(double tra, double ecc)
{
	if (ecc == 0) return(tra);
	double eca = tra2eca(tra, ecc);
	return(eca2mna(eca, ecc));
}

// ================================================================================================
//
double eca2mna(double eca, double ecc)
{
	if (ecc == 0) return(eca);

	if (ecc<1.0) {
		return eca - (ecc * sin(eca));
		// ATTENTION:  Limit removed 08-11-08 original: limit(eca - ( ecc * sin( eca ) ));
	}

	return ((ecc * sinh(eca)) - eca);
}

// ================================================================================================
//	
double eca2tra(double eca, double ecc)
{
	if (ecc == 0) return(eca);

	if (ecc<1) {
		double cos_ea = cos(eca);
		double tra = acos((cos_ea - ecc) / (1.0 - ecc * cos_ea));
		if (eca > PI) return PI2 - tra; // Limit removed
		return tra; // Limit removed
	}

	double cos_ea = cosh(eca);
	double tra = acos((ecc - cos_ea) / (ecc * cos_ea - 1.0));
	if (eca < 0) return PI2 - tra;
	return tra;
}

// ================================================================================================
//
double mna2tra(double mna, double ecc)
{
	double eca = mna2eca(mna, ecc);
	double tra = eca2tra(eca, ecc);
	return(tra);
}

// ================================================================================================
//
double eca2nra(double eca, double e) // Angle between normal and major axis
{

	if (e<1.0) {
		double ce = cos(eca);
		double t = acos(ce * sqrt((1.0 - e*e) / (1.0 - e*e*ce*ce)));
		if (eca>PI) t = PI2 - t;
		return t;
	}

	double ce = cosh(eca);
	double t = acos(ce * sqrt((1.0 - e*e) / (1.0 - e*e*ce*ce)));

	if (eca<0) t = PI2 - t;
	return t;
}

// ================================================================================================
//
double nra2eca(double nor, double e)
{

	double st = sin(nor);

	if (e<1.0) {
		double eca = acos(cos(nor) / sqrt(1.0 - e*e*st*st));
		if (nor>PI) eca = PI2 - eca;
		return eca;
	}

	double eca = acosh(cos(nor) / sqrt(1.0 - e*e*st*st));
	if (nor>PI) eca = -eca;
	return eca;
}

// ================================================================================================
//
double eca2fpa(double eca, double e)
{
	if (e == 0) return 0.0;

	if (e<1) {
		double co = cos(eca);
		double fpa = acos(sqrt((e*e - 1.0) / (e*e*co*co - 1.0)));

		if (eca>PI) return -fpa;
		return fpa;
	}

	double co = cosh(eca);
	double fpa = acos(sqrt((e*e - 1.0) / (e*e*co*co - 1.0)));

	if (eca<0) return -fpa;
	return fpa;
}

// ================================================================================================
//
double fpa2eca(double fpa, double e)
{
	if (e == 0) return 0;

	double co = cos(fabs(fpa));
	double x = sqrt(co*co + e*e - 1.0) / (e*co);

	if (e<1) {
		double eca = acos(x);
		if (fpa<0) return PI - eca;
		return eca;
	}

	double eca = acosh(x);
	if (fpa<0) return -eca;
	return eca;
}

// ================================================================================================
//
VECTOR3 RotationAxis(OBJHANDLE hRef)
{
	MATRIX3 Mat;
	oapiGetPlanetObliquityMatrix(hRef, &Mat);
	return mul(Mat, _V(0, 1, 0));
}

// ================================================================================================
//
VECTOR3 MeridianAxis(OBJHANDLE hRef, double mjd)
{
	MATRIX3 Mat;

	oapiGetPlanetObliquityMatrix(hRef, &Mat);

	// 11.12.2009 Removed fabs() from oapiGetPlanetPeriod
	double w = (mjd - oapiGetSimMJD())*PI2*86400.0 / oapiGetPlanetPeriod(hRef) + oapiGetPlanetCurrentRotation(hRef);

	Mat = mul(Mat, _M(cos(w), 0, -sin(w), 0, 1, 0, sin(w), 0, cos(w)));

	return mul(Mat, _V(1, 0, 0));	// Meridian Axis
}

// ================================================================================================
//
VECTOR3 SunTransit(OBJHANDLE hRef)
{
	MATRIX3 Mat;
	oapiGetPlanetObliquityMatrix(hRef, &Mat);
	return mul(Mat, _V(1, 0, 0));			// Meridian Axis
}

// ================================================================================================
//
void GetLngLat(VECTOR3 _in, VECTOR3 _RotAxis, VECTOR3 _Meridian, double &Lng, double &Lat)
{
	Lng = Longitude(_in, _Meridian, _RotAxis);
	if (Lng>PI) Lng -= PI2;
	Lat = PI05 - angle(_in, _RotAxis);
}

// ================================================================================================
//
VECTOR3 VectorByLngLat(VECTOR3 _RotAxis, VECTOR3 _Meridian, double Lng, double Lat)
{
	VECTOR3 _Prod = crossp_LH(_RotAxis, _Meridian);

	return unit(_RotAxis)  * (sin(Lat)) +
		unit(_Meridian) * (cos(Lng)*cos(Lat)) +
		unit(_Prod)     * (sin(Lng)*cos(Lat));
}

// ================================================================================================
//
VECTOR3 GetBasePosition(OBJHANDLE hBase, double mjd)
{
	double lng, lat, rad;
	oapiGetBaseEquPos(hBase, &lng, &lat, &rad);
	OBJHANDLE hPlanet = oapiGetBasePlanet(hBase);
	return GetSurfaceLocation(hPlanet, mjd, lng, lat);
}

// ================================================================================================
//
VECTOR3 GetSurfaceLocation(OBJHANDLE hPlanet, double mjd, double lng, double lat)
{
	VECTOR3 _Meridian = MeridianAxis(hPlanet, mjd);
	VECTOR3 _RotAxis = RotationAxis(hPlanet);
	VECTOR3 _Prod = crossp_LH(_RotAxis, _Meridian);

	double rad = oapiGetSize(hPlanet);

	return _RotAxis    * (sin(lat)*rad) +
		_Meridian   * (cos(lng)*cos(lat)*rad) +
		unit(_Prod) * (sin(lng)*cos(lat)*rad);
}

// ================================================================================================
//
VECTOR3 ConvertSurfaceLocation(OBJHANDLE hPlanet, double mjd, VECTOR3 _init, double imjd)
{
	double rad = length(_init);

	VECTOR3 _Meridian = MeridianAxis(hPlanet, imjd);
	VECTOR3 _RotAxis = RotationAxis(hPlanet);

	double lng = Longitude(_init, _Meridian, _RotAxis);
	double lat = PI05 - angle(_init, _RotAxis);

	_Meridian = MeridianAxis(hPlanet, mjd);
	VECTOR3 _Prod = crossp_LH(_RotAxis, _Meridian);

	return _RotAxis    * (sin(lat)*rad) +
		_Meridian   * (cos(lng)*cos(lat)*rad) +
		unit(_Prod) * (sin(lng)*cos(lat)*rad);
}

// ================================================================================================
//
double GetHeading(OBJHANDLE Ref, VECTOR3 &_Pos, VECTOR3 &_Vel)
{
	VECTOR3 _Rot = RotationAxis(Ref);
	VECTOR3 _East = crossp_LH(_Rot, _Pos);
	VECTOR3 _North = crossp_LH(_Pos, _East);
	return GetAngle(_Vel, _North, _East);
}

// ================================================================================================
//
double CalculateSOI(OBJHANDLE obj, OBJHANDLE ref)
{
	if (obj == NULL || ref == NULL) assert(false);
	if (obj == ref) return 100 * AU;

	double mr = oapiGetMass(ref);
	double mp = oapiGetMass(obj);

	VECTOR3 _r, _v;

	oapiGetRelativePos(obj, ref, &_r);
	oapiGetRelativeVel(obj, ref, &_v);

	double r = length(_r);
	double v = length(_v);
	double mu = (mr + mp)*GC;
	double a = -mu*r / (v*v*r - 2.0*mu);
	return a * pow(mp / mr, 2.0 / 5.0);
}

	

