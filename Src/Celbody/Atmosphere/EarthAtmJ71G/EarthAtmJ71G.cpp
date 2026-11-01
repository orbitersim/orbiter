// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ======================================================================
// class EarthAtmosphere_J71G
// Implementation of Jacchia71-Gill atmosphere model
// ======================================================================

#define ORBITER_MODULE
#include "EarthAtmJ71G.h"

#define pow10(x) (pow(10.0,(x))) // VC++ doesn't seem to support this directly

// ===========================================================================
// Local prototypes

double J77_Temp (double Z, double Tinf);
double J77_Dens_low (double Z);
double J77_MW_low (double Z);
double dens_fit (double Z, double Tinf);
double helium_fit (double Z, double Tinf);
double molweight_fit (double Z, double Tinf);


// ===========================================================================
// Constructor

EarthAtmosphere_J71G::EarthAtmosphere_J71G (CELBODY2 *body): ATMOSPHERE (body)
{
	hBody = body->GetHandle();
	atmprm.mjd = -1e10; // invalidate
}

// ===========================================================================
// Model identifier

const char *EarthAtmosphere_J71G::clbkName () const
{
	return "J71G";
}

// ===========================================================================
// Some constants of the atmospheric model

bool EarthAtmosphere_J71G::clbkConstants (ATMCONST *atmc) const
{
	atmc->p0       = 101325;
	atmc->rho0     = 1.2247;
	atmc->R        = 286.91;
	atmc->gamma    = 1.4;
	atmc->altlimit = 2500e3;
	return true;
}

// ===========================================================================
// Implementation of Jacchia-71 atmospheric model (see "Earth Atmosphere Model"
// in "Orbiter Technical Reference")

bool EarthAtmosphere_J71G::clbkParams (const PRM_IN *prm_in, PRM_OUT *prm)
{
	const double c    = 6.02257e26;    // Avogadro constant
	const double k    = 1.38066e-23;   // Boltzmann constant
	const double ck   = c*k;
	const double m_He = 4.002;         // Helium mass

	double Z = (prm_in->flag & PRM_ALT ? prm_in->alt*1e-3 : 0.0);
	double lng = (prm_in->flag & PRM_LNG ? prm_in->lng : 0.0);                lng=0;
	double lat = (prm_in->flag & PRM_LAT ? prm_in->lat : 0.0);                lat=0;
	double Fbar = (prm_in->flag & PRM_FBR ? prm_in->f107bar : 140.0);
	double Kp = (prm_in->flag & PRM_AP ? prm_in->ap : 3.0);

	double mjd = oapiGetSimMJD();
	if (fabs (mjd - atmprm.mjd) > 0.0417) {
		// Update sun's geographic position once an hour
		VECTOR3 gsun;
		double r;
		oapiGetGlobalPos (hBody, &gsun);
		oapiGlobalToEqu (hBody, -gsun, &atmprm.Slng, &atmprm.Slat, &r);
		atmprm.mjd = mjd;
	}	
	double Slat = atmprm.Slat;
	double Slng = atmprm.Slng - PI2*(mjd-atmprm.mjd)*86400.0/cbody->SidRotPeriod();
	if (Slng < -PI) Slng += PI2;

	double mw;

	if (Z < 90.0) {

		// use US standard atmosphere
		prm->T   = J77_Temp (Z, 0);
		prm->rho = J77_Dens_low (Z);
		mw  = J77_MW_low (Z);

	} else {

		// night-time global exospheric temperature
		double Tc = 379.0 + 3.24*Fbar;
	
		// modified hour angle of the sun
		double H = lng - Slng;
		double tau = H - 0.64577 + 0.10472*sin (H+0.7505) + 3.0*PI;

		tau = fmod (tau, PI2) - PI;    // tau in [-pi,+pi]

		// daily varying temperature
		double R = 0.3;
		double stheta = pow((0.5*(1.0-cos(Slat+lat))),1.1);
		double ceta   = pow((0.5*(1.0+cos(Slat-lat))),1.1);
		double TL     = Tc*(1.0+R*(stheta+(ceta-stheta)*pow(cos(0.5*tau),3.0)));

		// temperature delta from geomagnetic activity
		double expKp = exp(Kp);
		double d_Tg18 = 28.0*Kp + 0.03*expKp;

		// temperature and density delta from geomagnetic activity
		double d_Tg20 = 14.0*Kp + 0.02*expKp;
		double d_Gm20 = 0.012*Kp + 1.2e-5*expKp;

		// continuous transition at height 350km
		double F = 0.5*(tanh(0.04*(Z-350.0))+1.0);
		double d_Gm = d_Gm20*(1.0-F);
		double d_Tg = d_Tg20*(1.0-F) + d_Tg18*F;

		// exospheric temp [K]
		double Tinf = TL+d_Tg;

		// phase of semi-annual variation
		double phi = fmod ((mjd-36204.0)/365.2422, 1.0);

		// corrected phase, time and altitude contributions of semi-annual
		// variation
		double tauc = phi +
			0.09544*(pow(0.5+0.5*sin(PI2*phi+6.035),1.65)-0.5);
		double gt   = 0.02835 + 0.3817*(1.0+
			0.4671*sin(PI2*Tc+4.137))*sin(4.0*PI*Tc+4.259);
		double fz   = (5.876e-7*pow(Z,2.33) + 0.06328)*exp(-2.868e-3*Z);

		// semi-annual density correction
		double d_Sa = fz*gt;

		// seasonal-latitudinal correction
		double d_Sl = 0.014*(Z-90)*exp(-0.0013*pow(Z-90,2)) *
			(lat>=0?1:-1)*sin(PI2*phi+1.72)*pow(sin(lat),2);

		// Seasonal-latitudinal Helium correction
		double d_He = 0.65*fabs(Slat)/0.4091609 *
			(pow(sin(0.25*PI-0.5*lat*(Slat>=0?1:-1)),3) - 0.3535534);
		d_He = pow10(helium_fit(Z,Tinf) + (d_He-1.0)) * m_He/c;
	
		prm->T = J77_Temp (Z, Tinf);  // temperature at altitude
		prm->rho = pow10(dens_fit (Z, Tinf)+d_Gm+d_Sa+d_Sl) + d_He;
		// density at altitude

		mw = molweight_fit (Z, Tinf);
	}

	prm->p = prm->rho*ck/mw*prm->T;

	return true;
}



// ===========================================================================
// Atmospheric temperature [K] as a function of altitude z [km] and exospheric
// temperature Tinf [K]
// Extracted from Jacchia-77 code (j77sri.for) and converted from Fortran.
// This function strips out everything from j77sri that is not required for
// temperature calculation. This allows calculating the temperature at a
// given altitude directly, rather than using the integration approach of
// j77sri required for the density calculations
//
// Range limits: 0 <= z <= 2500
// ===========================================================================

double J77_Temp (double Z, double Tinf)
{
	double h, x, hbase, tbase, tgrad, Tx, Gx;
	int flag;

	// For z < 86km, use US Standard Atmosphere 1976

	if (Z < 86.0) {
		h = Z*6369.0/(Z+6369.0);
		if (Z < 33) {
			if (Z < 12) {
				hbase = 0.0;
				tbase = 288.15;
				tgrad = -6.5;
				flag = 0;
			} else if (Z < 21) {
				hbase = 11.0;
				tbase = 216.65;
				tgrad = 0;
				flag = 1;
			} else {
				hbase = 20.0;
				tbase = 216.65;
				tgrad = 1;
				flag = 0;
			}
		} else if (Z < 52) {
			if (Z < 48) {
				hbase = 32.0;
				tbase = 228.65;
				tgrad = 2.8;
				flag = 0;
			} else {
				hbase = 47.0;
				tbase = 270.65;
				tgrad = 0.0;
				flag = 1;
			}
		} else if (Z < 72) {
			hbase = 51.0;
			tbase = 270.65;
			tgrad = -2.8;
			flag = 0;
		} else {
			hbase = 71.0;
			tbase = 214.65;
			tgrad = -2.0;
			flag = 0;
		}

		if (!flag) {
			return tbase + tgrad*(h-hbase);
		} else {
			return tbase;
		}

		// For 85km < Z < 90, integrate barometric equation with
		// fudged molecular weight

	} else if (Z < 90) {
		return 188.0;

		// For Z > 89, use Jacchia 1977

	} else {

		if (Z < 91) {
			return 188.0;
		} else if (Tinf < 188.1) {
			return 188.0;
		} else {
			x = 0.0045 * (Tinf-188.0);
			Tx = 188.0 + 110.5 * log (x + sqrt(x*x+1.0));
			Gx = PI05*1.9*(Tx - 188.0)/(125.0-90.0);
			if (Z < 126)
				return Tx + ((Tx-188.0)/PI05)
				* atan ((Gx/(Tx-188.0))*(Z-125.0)
				* (1.0 + 1.7*pow((Z-125.0)/(Z-90.0),2)));
			else
				return Tx + ((Tinf-Tx)/PI05)
				* atan ((Gx/(Tinf-Tx))*(Z-125.0)
				* (1.0 + 5.5e-5*pow(Z-125.0,2)));
		}
	}
}


// ===========================================================================
// Return log of mass density [kg/m^3] below 90km.
// This is taken from Jacchia-77 (j77sri) which uses the US Standard
// Atmosphere 1976 in this altitude range
// ===========================================================================

double J77_Dens_low (double Z)
{
	const double wm0 = 28.96;

	double h, hbase, pbase, tbase, tgrad, T, CM = 0.0, WM, x, y;
	int flag;

	// For Z < 86km, use US Standard Atmosphere 1976
	if (Z <= 85.0) {
		h = Z*6369.0/(Z+6369.0);
		if (Z <= 32) {
			if (Z <= 11) {
				hbase = 0.0;
				pbase = 1.0;
				tbase = 288.15;
				tgrad = -6.5;
				flag = 0;
			} else if (Z <= 20) {
				hbase = 11.0;
				pbase = 2.233611e-1;
				tbase = 216.65;
				tgrad = 0;
				flag = 1;
			} else {
				hbase = 20.0;
				pbase = 5.403295e-2;
				tbase = 216.65;
				tgrad = 1;
				flag = 0;
			}
		} else if (Z <= 51) {
			if (Z <= 47) {
				hbase = 32.0;
				pbase = 8.5666784e-3;
				tbase = 228.65;
				tgrad = 2.8;
				flag = 0;
			} else {
				hbase = 47.0;
				pbase = 1.0945601e-3;
				tbase = 270.65;
				tgrad = 0.0;
				flag = 1;
			}
		} else if (Z <= 71) {
			hbase = 51.0;
			pbase = 6.6063531e-4;
			tbase = 270.65;
			tgrad = -2.8;
			flag = 0;
		} else {
			hbase = 71.0;
			pbase = 3.9046834e-5;
			tbase = 214.65;
			tgrad = -2.0;
			flag = 0;
		}

		if (!flag) {
			T = tbase + tgrad*(h-hbase);
			x = pow (tbase/T, 34.163195/tgrad);
		} else {
			T = tbase;
			x = exp(-34.163195*(h-hbase)/tbase);
		}
		CM = 2.547E19*(288.15/T)*pbase*x;

		// For 85km < Z < 90, integrate barometric equation with
		// fudged molecular weight

	} else if (Z < 90) {

		double pT, pCM, pWM, pZ, sZ, hZ; // state variables at previous step

		// initialise integration from z=85km
		pZ    = 85.0;
		pT    = 188.8889;
		x     = 0.11260416;
		pCM   = 1.7083677e14;
		y     = 1.7910182e-4;
		pWM   = 28.954813;

		// perform integration at 1-km steps
		hZ = ceil(Z);
		for (sZ = 86.0; sZ <= hZ+0.5; sZ += 1.0) {
			T = 188.0;
			y = pow (10.0, (-3.7469+(sZ-85)*(0.226434-(sZ-85)*5.945e-3)));
			WM = wm0*(1-y);

			CM = pCM*(pT/T)*(WM/pWM) * exp( - 0.5897446*( 
				(pWM/pT)*pow ((1+pZ/6356.766),-2)
				+ (WM/T)*pow((1+sZ/6356.766),-2) ));

			if (sZ <= hZ-0.5) {
				x = 1-y;
				WM =wm0*x;

				pZ = sZ;
				pT = T;
				pCM = CM;
				pWM = WM;
			}
		}
		// now do a linear interpolation for CM
		CM = (Z-pZ)*(CM-pCM)+pCM;
	}

	y = pow10 ((-3.7469+(Z-85)*(0.226434-(Z-85)*5.945e-3)));
	x = 1 - y;
	WM = wm0*x;

	// convert from particle density to mass density, [kg/m^3]
	const double iN = 1e3/6.02257e23;
	return WM*CM*iN;
}


// ===========================================================================
// Return molecular weight [g] of atmospheric gas mixture below 90km
// This is taken from Jacchia-77 (j77sri) which uses the US Standard
// Atmosphere 1976 in this altitude range
// ===========================================================================

double J77_MW_low (double Z)
{
	const double wm0 = 28.96;

	double x, y;

	if (Z > 90) return 0.0; // argument out of range

	y = pow10 (-3.7469+(Z-85.0)*(0.226434-(Z-85.0)*5.945e-3));
	x = 1 - y;
	return wm0*x;
}


// -----------------------------------------------------------------
// 
//  Function DENS_FIT
// 
//  Purpose:
// 
//  Evaluation of density polynomials
// 
//  Input:
// 
//  Z     Satellite height [km]
//  Tinf  Exospheric temperature [K]
// 
//  Output:
// 
//  DENS_FIT Logarithm of density value [cg/m^3]
// 
//  Note:
// 
//  The density values from the Jacchia 1971 atmospheric model
//  have been fitted in the height regime [90km,2500km) and the
//  temperature regime [500K,1900K] using a polynomial approach.
//  The regimes have been divided in 8 separate sections, where
//  coefficients of order 5 in temperature and 6 in height have been
//  obtained using a constrained least squares fit that assures
//  a smooth variation of the density with varying height.
//  The different sections along with the maximum encountered
//  density differences [%] are given in the sequel.
// 
//  ____________________________________
//  |                 |                |
//  |  500 -  850 K   |  850 - 1900 K  |
//  |   90 -  180 km  |   90 -  500 km |
//  |     1.6%        |     3.1%       |
//  |                 |                |
//  ____________________________________
//  |                 |                |
//  |  500 -  850 K   |  850 - 1900 K  |
//  |  180 -  500 km  |  180 -  500 km |
//  |     3.8%        |     1.6%       |
//  |                 |                |
//  ____________________________________
//  |                 |                |
//  |  500 -  850 K   |  850 - 1900 K  |
//  |  500 - 1000 km  |  500 - 1000 km |
//  |     6.5%        |     3.3%       |
//  |                 |                |
//  ____________________________________
//  |                 |                |
//  |  500 -  850 K   |  850 - 1900 K  |
//  | 1000 - 2500 km  | 1000 - 2500 km |
//  |     2.1%        |     7.7%       |
//  |                 |                |
//  ____________________________________
// 
// 
// -----------------------------------------------------------------

double dens_fit (double Z, double Tinf)
{
	const int nT = 4;         // temperature basis order
	const int nZ = 5;         // height basis order

	int i, n, m;              // loop variables
	int it, ith;              // temperature height indices
	double sum;               // local variable
	double pow_Tm, pow_Zn;    // powers of Tinf and Z

	const double Zlimit[5] = {      // height boundaries
		0.09e0, 0.18e0, 0.50e0, 1.00e0, 2.50e0
	};
	const double Tlimit =           // temperature boundary
		0.85e0;
	static const double c[8*(nT+1)][nZ+1] = { // density coefficients
		// H: [ 90km, 180km], T: [ 500K, 850K]
		{ -0.352085629e+02,  0.112921028e+04, -0.152747516e+05,  // (0-2,0)
		0.930204177e+05, -0.273439442e+06,  0.314969634e+06}, // (3-5,0)
		{  0.391262208e+01,  0.119815810e+04, -0.355848094e+05,  // (0-2,1)
		0.364655435e+06, -0.157609709e+07,  0.248772280e+07}, // (3-5,1)
		{ -0.864925858e+02,  0.863379360e+03,  0.189924264e+05,  // (0-2,2)
		-0.329036395e+06,  0.168583089e+07, -0.289912393e+07}, // (3-5,2)
		{  0.150411921e+03, -0.357709109e+04,  0.250824047e+05,  // (0-2,3)
		-0.120963117e+05, -0.428294274e+06,  0.111190398e+07}, // (3-5,3)
		{ -0.710942779e+02,  0.197055758e+04, -0.196825320e+05,  // (0-2,4)
		0.843813722e+05, -0.134559332e+06,  0.329409498e+04}, // (3-5,4)
		// H: [ 90km, 180km], T: [ 850K,1900K]
		{ -0.533541178e+02,  0.197753256e+04, -0.299362006e+05,  // (0-2,0)
		0.211206780e+06, -0.720972149e+06,  0.962596639e+06}, // (3-5,0)
		{  0.290055675e+02, -0.709147804e+03,  0.518728595e+04,  // (0-2,1)
		-0.448302904e+04, -0.768410087e+05,  0.212312699e+06}, // (3-5,1)
		{ -0.204643854e+02,  0.439853751e+03, -0.198979486e+04,  // (0-2,2)
		-0.134997048e+05,  0.125623620e+06, -0.262279285e+06}, // (3-5,2)
		{  0.797714877e+01, -0.156871952e+03,  0.364316576e+03,  // (0-2,3)
		0.951001182e+04, -0.680569856e+05,  0.133712984e+06}, // (3-5,3)
		{ -0.133585294e+01,  0.261546624e+02, -0.570066884e+02,  // (0-2,4)
		-0.165372508e+04,  0.118125702e+05, -0.232999509e+05}, // (3-5,4)
		// H: [ 180km, 500km], T: [ 500K, 850K]
		{  0.231191014e+02, -0.105777591e+04,  0.117722961e+05,  // (0-2,0)
		-0.582766334e+05,  0.125458876e+06, -0.945292227e+05}, // (3-5,0)
		{  0.135529793e+03,  0.608797320e+03, -0.316413234e+05,  // (0-2,1)
		0.218816743e+06, -0.543470990e+06,  0.440802570e+06}, // (3-5,1)
		{ -0.842431012e+03,  0.869056563e+04, -0.107632268e+04,  // (0-2,2)
		-0.242291149e+06,  0.812301589e+06, -0.737940971e+06}, // (3-5,2)
		{  0.128733075e+04, -0.171592213e+05,  0.630262927e+05,  // (0-2,3)
		0.246128588e+05, -0.449043841e+06,  0.509527260e+06}, // (3-5,3)
		{ -0.618120934e+03,  0.905267060e+04, -0.431245935e+05,  // (0-2,4)
		0.604409552e+05,  0.500745829e+05, -0.115419197e+06}, // (3-5,4)
		// H: [ 180km, 500km], T: [ 850K,1900K]
		{  0.404176122e+02, -0.812772013e+03,  0.513004248e+04,  // (0-2,0)
		-0.160017007e+05,  0.238471768e+05, -0.136310388e+05}, // (3-5,0)
		{ -0.130571901e+03,  0.227356511e+04, -0.150130763e+05,  // (0-2,1)
		0.477046885e+05, -0.719906407e+05,  0.415349930e+05}, // (3-5,1)
		{  0.146680893e+03, -0.257726141e+04,  0.171714170e+05,  // (0-2,2)
		-0.547349228e+05,  0.828465304e+05, -0.479358081e+05}, // (3-5,2)
		{ -0.712029554e+02,  0.125904483e+04, -0.844169798e+04,  // (0-2,3)
		0.269966778e+05, -0.409835788e+05,  0.237785404e+05}, // (3-5,3)
		{  0.126960539e+02, -0.225497799e+03,  0.151879634e+04,  // (0-2,4)
		-0.487030568e+04,  0.741192600e+04, -0.431023262e+04}, // (3-5,4)
		// H: [ 500km,1000km], T: [ 500K, 850K]
		{ -0.181572147e+04,  0.985122122e+04, -0.182293162e+05,  // (0-2,0)
		0.129811344e+05, -0.153350954e+04, -0.126367969e+04}, // (3-5,0)
		{  0.979297163e+04, -0.539752453e+05,  0.100242982e+06,  // (0-2,1)
		-0.711343013e+05,  0.781553694e+04,  0.726579206e+04}, // (3-5,1)
		{ -0.183137427e+05,  0.999316898e+05, -0.178448116e+06,  // (0-2,2)
		0.110637478e+06,  0.703756219e+04, -0.209290906e+05}, // (3-5,2)
		{  0.138525479e+05, -0.725945601e+05,  0.114517833e+06,  // (0-2,3)
		-0.382577652e+05, -0.467463592e+05,  0.293609410e+05}, // (3-5,3)
		{ -0.345123396e+04,  0.162255300e+05, -0.164193391e+05,  // (0-2,4)
		-0.166691482e+05,  0.351694860e+05, -0.149167582e+05}, // (3-5,4)
		// H: [ 500km,1000km], T: [ 850K,1900K]
		{ -0.402133495e+02,  0.425578905e+03, -0.182166204e+04,  // (0-2,0)
		0.307023080e+04, -0.219684753e+04,  0.549495863e+03}, // (3-5,0)
		{ -0.132698258e+03,  0.352812623e+03,  0.790535705e+03,  // (0-2,1)
		-0.294154027e+04,  0.258511809e+04, -0.660422507e+03}, // (3-5,1)
		{  0.377886396e+03, -0.207788843e+04,  0.393427057e+04,  // (0-2,2)
		-0.327663903e+04,  0.138277623e+04, -0.332807663e+03}, // (3-5,2)
		{ -0.280866017e+03,  0.172654309e+04, -0.396933359e+04,  // (0-2,3)
		0.442021654e+04, -0.253300602e+04,  0.633570336e+03}, // (3-5,3)
		{  0.651353118e+02, -0.419147661e+03,  0.102799073e+04,  // (0-2,4)
		-0.123077830e+04,  0.745138710e+03, -0.187981171e+03}, // (3-5,4)
		// H: [1000km,2500km], T: [ 500K, 850K]
		{  0.354869756e+03, -0.537085151e+03, -0.234958642e+02,  // (0-2,0)
		0.340707342e+03, -0.169847098e+03,  0.249797337e+02}, // (3-5,0)
		{ -0.250868544e+04,  0.418258598e+04, -0.894184106e+03,  // (0-2,1)
		-0.153158821e+04,  0.898569661e+03, -0.138961772e+03}, // (3-5,1)
		{  0.625274219e+04, -0.115111424e+05,  0.441792700e+04,  // (0-2,2)
		0.217904521e+04, -0.170479728e+04,  0.282005813e+03}, // (3-5,2)
		{ -0.675537591e+04,  0.133891525e+05, -0.673281743e+04,  // (0-2,3)
		-0.884134055e+03,  0.136309843e+04, -0.247286186e+03}, // (3-5,3)
		{  0.267576301e+04, -0.561058041e+04,  0.331260819e+04,  // (0-2,4)
		-0.136976861e+03, -0.381241735e+03,  0.789643901e+02}, // (3-5,4)
		// H: [1000km,2500km], T: [ 850K,1900K]
		{  0.128106144e+02,  0.202425105e+03, -0.575074276e+03,  // (0-2,0)
		0.510620714e+03, -0.189895280e+03,  0.256957682e+02}, // (3-5,0)
		{ -0.338917904e+03,  0.166830201e+03,  0.825982311e+03,  // (0-2,1)
		-0.103201223e+04,  0.434750107e+03, -0.628271027e+02}, // (3-5,1)
		{  0.686193462e+03, -0.114787559e+04,  0.232983196e+03,  // (0-2,2)
		0.485187352e+03, -0.298601120e+03,  0.497107699e+02}, // (3-5,2)
		{ -0.466762722e+03,  0.991894020e+03, -0.650335882e+03,  // (0-2,3)
		0.821409740e+02,  0.542317977e+02, -0.140438474e+02}, // (3-5,3)
		{  0.102966235e+03, -0.243021528e+03,  0.199798890e+03,  // (0-2,4)
		-0.652704815e+02,  0.503945902e+01,  0.845050002e+00}  // (3-5,4)
	};

	Tinf *= 1e-3;               // scale to units close to 1
	Z    *= 1e-3;               // scale to units close to 1

	it = (Tinf > Tlimit ? 1:0); // temperature section index

	if (Z >= Zlimit[0]) {       // check lower bounds
		for (i = 0; i <= 3; i++) {  // loop over four height regimes
			if (Z <= Zlimit[i+1]) {
				ith = 2*i+it;       // ith identifies one out of 8 sections

				// compute density
				sum = 0.0;
				pow_Tm = 1.0;
				for (m = 0; m <= nT; m++) {
					pow_Zn = 1.0;
					for (n = 0; n <= nZ; n++) {
						sum += c[5*ith+m][n] * pow_Tm * pow_Zn;
						pow_Zn *= Z;
					}
					pow_Tm *= Tinf;
				}
				return sum;
			}
		}
	}

	return 0;  // height out of bounds
}


// -----------------------------------------------------------------
//
// Function HELIUM_FIT
//
// Purpose:
//
//   Evaluation of Helium number density polynomials
//
// Input:
//
//   HEIGHT      Satellite height [km]
//   TEMPER      Exospheric temperature [K]
//
// Output:
//
//   HELIUM_FIT  Logarithm of Helium number density value [1/m^3]
//
// Note:
//
//   The Helium number density values from the Jacchia 1971
//   atmospheric model have been fitted in the height regime
//   [90km,2500km) and the temperature regime [500K,1900K] using a
//   polynomial approach. The regimes have been divided in 3
//   separate sections in height, where coefficients of order 5
//   in temperature and 6 in height have been obtained using a
//   constrained least squares fit that assures a smooth variation
//   of the density with varying height.
//
//   The Helium number density is required to compute the
//   seasonal-latitudinal density variation of Helium. The fit
//   has been performed with the constraints that any deviations
//   in the fit error should produce an error less than 5% in the
//   total density. This leads to a2-dim. constraints pattern
//   in height and temperature.
//
//   ________________________________________
//   |                                      |
//   |             500 - 1900 K             |
//   |              90 -  500 km            |
//   |          66% @  100 km 500 K         |
//   |                                      |
//   ________________________________________
//   |                                      |
//   |             500 - 1900 K             |
//   |             500 - 1000 km            |
//   |           7% @ 1000 km 605 K         |
//   |                                      |
//   ________________________________________
//   |                                      |
//   |             500 - 1900 K             |
//   |            1000 - 2500 km            |
//   |          13% @ 2369 km 500 K         |
//   |                                      |
//   ________________________________________
//
//
// -----------------------------------------------------------------

double helium_fit (double Z, double Tinf)
{
	const int nT = 4;               // matrix dimension
	const int nZ = 5;               // matrix dimension

	int i, n, m;                    // loop variables
	int ith;                        // height index
	double sum;                     // local variable
	double pow_Tm, pow_Zn;          // powers of Tinf and Z
	const double Zlimit[4] = {      // height boundaries
		90.0, 500.0, 1000.0, 2500.0
	};
	static const double he[3*(nT+1)][nZ+1] = { // density coefficients
		// H: [ 90km, 500km], T: [ 500K,1900K]
		{ 0.183154936e+02, -0.737400840e-01,  0.438416419e-03,  // (0-2,0)
		-0.141119464e-05,  0.215363863e-08, -0.125513854e-11}, // (3-5,0)
		{ 0.588755629e-02, -0.125107697e-03,  0.865702730e-06,  // (0-2,1)
		-0.248383435e-08,  0.342194382e-11, -0.182725298e-14}, // (3-5,1)
		{-0.481325706e-05,  0.103926920e-06, -0.721694577e-09,  // (0-2,2)
		0.200410722e-11, -0.262896070e-14,  0.132158108e-17}, // (3-5,2)
		{ 0.170173800e-08, -0.367927971e-10,  0.248153370e-12,  // (0-2,3)
		-0.624498540e-15,  0.708565525e-18, -0.288739764e-21}, // (3-5,3)
		{-0.212837367e-12,  0.455525822e-14, -0.285907444e-16,  // (0-2,4)
		0.556100355e-19, -0.327980417e-22, -0.782717842e-26}, // (3-5,4)
		// H: [ 500km,1000km], T: [ 500K,1900K]
		{ 0.162708865e+02, -0.195829652e-01,  0.251425106e-04,  // (0-2,0)
		-0.298331353e-07,  0.180202809e-10, -0.424306744e-14}, // (3-5,0)
		{-0.178681641e-01,  0.138612645e-03, -0.380633890e-06,  // (0-2,1)
		0.585585083e-09, -0.438287770e-12,  0.126883016e-15}, // (3-5,1)
		{ 0.307907898e-04, -0.253246272e-06,  0.769237606e-09,  // (0-2,2)
		-0.121066317e-11,  0.920153031e-15, -0.269580744e-18}, // (3-5,2)
		{-0.204343061e-07,  0.171418298e-09, -0.539476570e-12,  // (0-2,3)
		0.856163162e-15, -0.654393514e-18,  0.192546917e-21}, // (3-5,3)
		{ 0.464341888e-11, -0.393423008e-13,  0.126030389e-15,  // (0-2,4)
		-0.200903018e-18,  0.154021961e-21, -0.454232921e-25}, // (3-5,4)
		// H: [1000km,2500km], T: [ 500K,1900K]
		{ 0.187334592e+02, -0.236253018e-01,  0.189389910e-04,  // (0-2,0)
		-0.113219809e-07,  0.346501347e-11, -0.415670958e-15}, // (3-5,0)
		{ 0.228568321e-01, -0.690761294e-04,  0.114595980e-06,  // (0-2,1)
		-0.743832646e-10,  0.230894316e-13, -0.279192982e-17}, // (3-5,1)
		{-0.686077568e-04,  0.225167974e-06, -0.318325867e-09,  // (0-2,2)
		0.204028767e-12, -0.632046641e-16,  0.763279237e-20}, // (3-5,2)
		{ 0.537962260e-07, -0.179593742e-09,  0.246107555e-12,  // (0-2,3)
		-0.157319125e-15,  0.487141850e-19, -0.588111152e-23}, // (3-5,3)
		{-0.132755885e-10,  0.446365926e-13, -0.604042307e-16,  // (0-2,4)
		0.385703179e-19, -0.119413886e-22,  0.144145505e-26}  // (3-5,4)
	};

	if (Z >= Zlimit[0]) {          // check lower bounds
		for (i = 0; i <= 2; i++) { // loop over three height regimes
			if (Z <= Zlimit[i+1]) {
				ith = i;           // ith identifies one of 3 sections

				// compute Helium number density
				sum = 0.0;
				pow_Tm = 1.0;
				for (m = 0; m <= nT; m++) {
					pow_Zn = 1.0;
					for (n = 0; n <= nZ; n++) {
						sum += he[nZ*ith+m][n] * pow_Tm * pow_Zn;
						pow_Zn *= Z;
					}
					pow_Tm *= Tinf;
				}
				return sum;
			}
		}
	}

	return 0;  // height out of bounds
}


// -----------------------------------------------------------------
// Polynomial series expansion of molecular weight of the atmospheric gas
// mixture as a function of altitude Z [km] and exospheric temperature
// Tinf [K]
// The coefficients were obtained by a least squares fit (see atm_interp.m)
// (My own work)
// -----------------------------------------------------------------

double molweight_fit (double Z, double Tinf)
{
	const int nT = 4;               // Tinf polynomial basis order
	const int nZ = 8;               // Z polynomial basis order

	const double Zmin = 90,  Zmax = 2500; // altitude boundaries [km]
	const double Tmin = 500, Tmax = 1900; // Tinf boundaries [K]

	static const double cm[nZ+1][nT+1] = { // molecular weight coefficients
		// H: [ 90km, 2500km], T: [ 500K,1900K]
		{ 3.60906627e+00, -1.35761290e+01,  2.55465982e+01,
		-1.77204699e+01,  4.07696683e+00},
		{-1.35606636e+01,  1.29084956e+02, -2.84612534e+02,
		2.12822504e+02, -5.11308350e+01},
		{-1.25207810e+01, -3.19579690e+02,  9.77718751e+02,
		-8.21851276e+02,  2.09837747e+02},
		{ 6.98268484e+01,  4.02801988e+02, -1.69988881e+03,
		1.57164796e+03, -4.21561534e+02},
		{-8.44186988e+01, -2.88755547e+02,  1.67090876e+03,
		-1.67274401e+03,  4.67667119e+02},
		{ 4.42735679e+01,  1.24592950e+02, -9.68963604e+02,
		1.03850337e+03, -3.01006087e+02},
		{-7.92712088e+00, -3.81200493e+01,  3.34875778e+02,
		-3.77385160e+02,  1.12634338e+02},
		{-1.14988223e+00,  9.20028301e+00, -6.52157331e+01,
		7.51024325e+01, -2.28683127e+01},
		{ 4.13670214e-01, -1.18725773e+00,  5.60433585e+00,
		-6.36983671e+00,  1.95706136e+00}
#ifdef UNDEF
		// the following coefficients are for obtaining the direct
		// molecular weight, rather than log10. They model the low
		// altitudes better, but have oscillations at high altitudes
		{ 1.1136902e+02, -3.6215096e+02,  5.8123604e+02,
			-3.6329018e+02,  7.6964932e+01},
		{-8.5176459e+02,  4.0863758e+03, -7.1253841e+03,
		4.6760493e+03, -1.0221502e+03},
		{ 1.7269742e+03, -1.2696361e+04,  2.6494601e+04,
		-1.8929680e+04,  4.3448337e+03},
		{-5.8939237e+02,  1.6695342e+04, -4.5304699e+04,
		3.5791361e+04, -8.6649227e+03},
		{-1.9544251e+03, -9.4459009e+03,  4.1231507e+04,
		-3.6709602e+04,  9.4007782e+03},
		{ 2.6867110e+03,  7.1716825e+02, -2.0935963e+04,
		2.1655261e+04, -5.8866818e+03},
		{-1.4653863e+03,  1.5761202e+03,  5.8160872e+03,
		-7.3486844e+03,  2.1296772e+03},
		{ 3.7530698e+02, -6.5664109e+02, -7.9065303e+02,
		1.3344823e+03, -4.1430779e+02},
		{-3.7286733e+01,  8.0961751e+01,  3.6774271e+01,
		-1.0057393e+02,  3.3620454e+01}
#endif
	};

	double sum, pow_Tm, pow_Zn;
	int m, n;

	Tinf *= 1e-3;               // scale to units close to 1
	Z    *= 1e-3;               // scale to units close to 1

	sum = 0.0;

	pow_Zn = 1.0;
	for (n = 0; n <= nZ; n++) {
		pow_Tm = 1.0;
		for (m = 0; m <= nT; m++) {
			sum += cm[n][m] * pow_Tm * pow_Zn;
			pow_Tm *= Tinf;
		}
		pow_Zn *= Z;
	}

	return pow10(sum);
}


// ======================================================================
// API interface
// ======================================================================

DLLCLBK void InitModule (HINSTANCE hModule)
{}

DLLCLBK void ExitModule (HINSTANCE hModule)
{}

DLLCLBK ATMOSPHERE *CreateAtmosphere (CELBODY2 *cbody)
{
	return new EarthAtmosphere_J71G (cbody);
}

DLLCLBK void DeleteAtmosphere (ATMOSPHERE *atm)
{
	delete (EarthAtmosphere_J71G*)atm;
}

DLLCLBK char *ModelName ()
{
	return (char*)"Jacchia71-Gill Atmospheric Model";
}

DLLCLBK char *ModelDesc ()
{
	return (char*)"An implementation of the Jacchia71-Gill (J71C) atmospheric model. This uses a static US Standard Atmosphere model up to 90km, and a diffusion-equilibrium solution from 90 to 2500km altitude.\r\n\r\nSee \"Earth Atmosphere Model\" in \"Orbiter Technical Reference\" for details.";
}