#include <math.h>

#include "OrbiterAPI.h"
#include "VFS.h"

using namespace std;

// #define INCLUDE_TIDAL_PERT
// Uncomment this to add higher-order perturbation terms
// (tidal, relativistic, solar eccentricity)
// Warning: Using this can lead to inconsistencies since these
// effects are not currently modelled in Orbiter's dynamic model.

// ===========================================================
// Global variables
// ===========================================================
typedef double SEQ3[3];
typedef double SEQ6[6];

static const double cpi     = 3.141592653589793;
static const double cpi2    = 2.0*cpi;
static const double pis2    = cpi/2.0;
static const double rad     = 648000.0/cpi;
static const double deg     = cpi/180.0;
static const double c1      = 60.0;
static const double c2      = 3600.0;
static const double ath     = 384747.9806743165;
static const double a0      = 384747.9806448954;
static const double am      = 0.074801329518;
static const double alpha   = 0.002571881335;
static const double dtasm   = 2.0*alpha/(3.0*am);
static const double mjd2000 = 51544.5; // mjd->dj2000 offset
static const double sc      = 36525.0;
static const double precess = 5029.0966/rad;

static       bool need_terms = true; // need to read terms?
static const double def_prec = 1e-5; // default precision
static       double cur_prec = -1.0; // current precision

static double delnu, dele, delg, delnp, delep;
static double p1, p2, p3, p4, p5, q1, q2, q3, q4, q5;
static double w[3][5], p[8][2], eart[5], peri[5], del[4][5], zeta[2];
static int    nterm[3][12], nrang[3][12];    // current length of term sequences
static SEQ6 *pc[3]  = {0,0,0};               // main term sequences
static SEQ3 *per[3] = {0,0,0};               // perturbation term sequences

// ===========================================================
// ELP82_init ()
// Set up invariant global parameters for ELP82 solver
// ===========================================================

void ELP82_init ()
{
	// Lunar arguments

	w[0][0] = (218.0+18.0/c1+59.95571/c2)*deg;
	w[1][0] = (83.0+21.0/c1+11.67475/c2)*deg;
	w[2][0] = (125.0+2.0/c1+40.39816/c2)*deg;
	eart[0] = (100.0+27.0/c1+59.22059/c2)*deg;
	peri[0] = (102.0+56.0/c1+14.42753/c2)*deg;
	w[0][1] = 1732559343.73604/rad;
	w[1][1] = 14643420.2632/rad;
	w[2][1] = -6967919.3622/rad;
	eart[1] = 129597742.2758/rad;
	peri[1] = 1161.2283/rad;
	w[0][2] = -5.8883/rad;
	w[1][2] = -38.2776/rad;
	w[2][2] = 6.3622/rad;
	eart[2] = -0.0202/rad;
	peri[2] = 0.5327/rad;
	w[0][3] = 0.6604e-2/rad;
	w[1][3] = -0.45047e-1/rad;
	w[2][3] = 0.7625e-2/rad;
	eart[3] = 0.9e-5/rad;
	peri[3] = -0.138e-3/rad;
	w[0][4] = -0.3169e-4/rad;
	w[1][4] = 0.21301e-3/rad;
	w[2][4] = -0.3586e-4/rad;
	eart[4] = 0.15e-6/rad;
	peri[4] = 0.0;

	// Planetary arguments

	p[0][0] = (252.0+15.0/c1+3.25986/c2)*deg;
	p[1][0] = (181.0+58.0/c1+47.28305/c2)*deg;
	p[2][0] = eart[0];
	p[3][0] = (355.0+25.0/c1+59.78866/c2)*deg;
	p[4][0] = (34.0+21.0/c1+5.34212/c2)*deg;
	p[5][0] = (50.0+4.0/c1+38.89694/c2)*deg;
	p[6][0] = (314.0+3.0/c1+18.01841/c2)*deg;
	p[7][0] = (304.0+20.0/c1+55.19575/c2)*deg;
	p[0][1] = 538101628.68898/rad;
	p[1][1] = 210664136.43355/rad;
	p[2][1] = eart[1];
	p[3][1] = 68905077.59284/rad;
	p[4][1] = 10925660.42861/rad;
	p[5][1] = 4399609.65932/rad;
	p[6][1] = 1542481.19393/rad;
	p[7][1] = 786550.32074/rad;

	// Corrections of the constants (fit to DE200/LE200)

	delnu = +0.55604/rad/w[0][1];
	dele  = +0.01789/rad;
	delg  = -0.08066/rad;
	delnp = -0.06424/rad/w[0][1];
	delep = -0.12879/rad;

	// Delaunay's arguments

	for (int i = 0; i < 5; i++) {
		del[0][i] = w[0][i] - eart[i];
		del[3][i] = w[0][i] - w[2][i];
		del[2][i] = w[0][i] - w[1][i];
		del[1][i] = eart[i] - peri[i];
	}
	del[0][0] = del[0][0] + cpi;
	zeta[0]   = w[0][0];
	zeta[1]   = w[0][1] + precess;

	// Precession matrix

	p1 =  0.10180391e-4;
	p2 =  0.47020439e-6;
	p3 = -0.5417367e-9;
	p4 = -0.2507948e-11;
	p5 =  0.463486e-14;
	q1 = -0.113469002e-3;
	q2 =  0.12372674e-6;
	q3 =  0.1265417e-8;
	q4 = -0.1371808e-11;
	q5 = -0.320334e-14;
}


// ===========================================================
// ELP82_exit ()
// Cleanup procedures
// ===========================================================

void ELP82_exit ()
{
	if (cur_prec >= 0.0) {
		for (int i = 0; i < 3; i++) {
			if (pc[i])  { delete []pc[i];  pc[i]  = 0; }
			if (per[i]) { delete []per[i]; per[i] = 0; }
		}
	}
	cur_prec = -1.0;
}

// ===========================================================
// ELP82_read ()
// Read the perturbation terms from file and store in global
// parameters. The number of terms read depends on requested precision.
// ===========================================================

int ELP82_read (double prec)
{
	// Term structure interfaces
	typedef struct {
		int ilu[4];
		double coef[7];
	} MainBin;

	typedef struct {
		int iz;
		int ilu[4];
		double pha, x, per;
	} FigurBin;

	typedef struct {
		short ipla[11];
		double pha, x, per;
	} PlanPerBin;

	// Check for existing terms
	if (cur_prec >= 0.0) {
		if (prec == cur_prec) return 0;   // nothing to do
		for (int i = 0; i < 3; i++) {
			if (pc[i])  { delete []pc[i];  pc[i]  = 0; }  // remove existing terms
			if (per[i]) { delete []per[i]; per[i] = 0; }
		}
	}

	int ific, itab, m, mm, i, im, ir, k, ntot = 0, mtot = 0;
	double tgv, xx, y, pre[3], zone[6];

	// Precision paremeters
	pre[0] = prec*rad;
	pre[1] = prec*rad;
	pre[2] = prec*ath;

	const char *datf = "Config\\Moon\\Data\\ELP82.dat";
	VFS::ifstream ifs (datf);  // term data stream
	if (!ifs) {
		oapiWriteLogError("ELP82: Data file not found: %s", datf);
		return -1;
	}

	// Read terms for main problem
	for (ific = 0; ific < 3; ific++) {

		ifs >> m;                          // number of terms available in sequence
		MainBin *block = new MainBin[m];   // temporary storage for terms
		for (ir = mm = 0; ir < m; ir++) {  // read terms from file
			for (i = 0; i < 4; i++)
				ifs >> block[ir].ilu[i];
			for (i = 0; i < 7; i++)
				ifs >> block[ir].coef[i];
			if (fabs(block[ir].coef[0]) >= pre[ific])
				mm++;                     // number of terms used
		}
		ntot += mm;
		mtot += m;
		if (mm) pc[ific] = new SEQ6[mm];
		itab = 0;

		for (im = ir = 0; im < m; im++) {
			MainBin &lin = block[im];
			xx = lin.coef[0];
			if (fabs(xx) < pre[ific]) continue;
			tgv = lin.coef[1] + dtasm*lin.coef[5];
			if (ific == 2) lin.coef[0] -= 2.0*lin.coef[0]*delnu/3.0;
			xx = lin.coef[0] + tgv*(delnp-am*delnu) + lin.coef[2]*delg +
				 lin.coef[3]*dele + lin.coef[4]*delep;
			zone[0] = xx;
			for (k = 0; k <= 4; k++) {
				y = 0.0;
				for (i = 0; i < 4; i++) {
					y += lin.ilu[i]*del[i][k];
				}
				zone[k+1] = y;
			}
			if (ific == 2) zone[1] += pis2;
			for (i = 0; i < 6; i++) pc[ific][ir][i] = zone[i];
			ir++;
		}
		nterm[ific][0] = ir;
		nrang[ific][0] = 0;
		delete []block;
	}

#ifdef INCLUDE_TIDAL_PERT

	int iv, j;

	// Read terms for tides, relativity, solar eccentricity (part 1)
	for (ific = 0; ific < 3; ific++) {
		iv = ific % 3;
		ifs >> m;
		FigurBin *block = new FigurBin[m];
		for (ir = mm = 0; ir < m; ir++) {
			ifs >> block[ir].iz;
			for (i = 0; i < 4; i++)
				ifs >> block[ir].ilu[i];
			ifs >> block[ir].pha >> block[ir].x >> block[ir].per;
			if (block[ir].x >= pre[iv]) 
				mm++;
		}
		ntot += mm;
		mtot += m;
		if (mm) per[ific] = new SEQ3[mm];
		itab = (ific/3) + 1;

		for (im = ir = 0; im < m; im++) {
			FigurBin &lin = block[im];
			xx = lin.x;
			if (lin.x < pre[iv]) continue;
			zone[0] = xx;
			for (k = 0; k <= 1; k++) {
				y = (k ? lin.pha*deg : 0.0);
				y += lin.iz*zeta[k];
				for (i = 0; i < 4; i++)
					y += lin.ilu[i]*del[i][k];
				zone[k+1] = y;
			}
			j = nrang[iv][itab-1] + ir;
			for (i = 0; i < 3; i++) per[iv][i][j] = zone[i];
			ir++;
		}
		nterm[iv][itab] = ir;
		nrang[iv][itab] = nrang[iv][itab-1] + nterm[iv][itab];
		delete []block;
	}

#endif // INCLUDE_TIDAL_PERT

	// Add: PlanetaryPerturbations
	// Add: FiguresTides

	oapiWriteLogV("ELP82: Precision %0.1le, Terms %d/%d", prec, ntot, mtot);

	need_terms = false;
	cur_prec   = prec;

	return 0;
}


// ===========================================================
// ELP82 ()
// Calculate lunar ephemeris using ELP2000-82 perturbation solutions
// MS modifications:
// - Time input is MJD instead of JD
// - Added time derivatives (output in r[3] to r[5])
// ===========================================================

int ELP82 (double mjd, double *r)
{
	int k, iv, nt;
	double t[5];
	double x, y, x1, x2, x3, pw, qw, ra, pwqw, pw2, qw2;
	double x_dot, y_dot, x1_dot, x2_dot, x3_dot, pw_dot, qw_dot;
	double ra_dot, pwqw_dot, pw2_dot, qw2_dot;
	double cosr0, sinr0, cosr1, sinr1;

	// Initialisation

	if (need_terms) ELP82_read (def_prec);

	// substitution of time

	t[0] = 1.0;
    t[1] = (mjd-mjd2000)/sc;
    t[2] = t[1]*t[1];
    t[3] = t[2]*t[1];
    t[4] = t[3]*t[1];

	for (iv = 0; iv < 3; iv++) {
		r[iv] = r[iv+3] = 0.0;
		SEQ6 *pciv = pc[iv];

		// main sequence (itab=0)
		for (nt = 0; nt < nterm[iv][0]; nt++) {
			x = pciv[nt][0];     x_dot = 0.0;
			y = pciv[nt][1];     y_dot = 0.0;
			for (k = 1; k <= 4; k++) {
				y     += pciv[nt][k+1] * t[k];
				y_dot += pciv[nt][k+1] * t[k-1] * k;
			}
			r[iv]   += x*sin(y);
			r[iv+3] += x_dot*sin(y) + x*cos(y)*y_dot;
		}

#ifdef INCLUDE_TIDAL_PERT

		int itab, j;

		// perturbation sequences (itab>0)
		for (itab = 1; itab < 2/*12*/; itab++) {
			for (nt = 0; nt < nterm[iv][itab]; nt++) {
				j = nrang[iv][itab-1] + nt;
				x = per[iv][0][j];
				y = per[iv][1][j] + per[iv][2][j] * t[1];

				if (itab == 2 || itab == 4 || itab == 6 || itab == 8) {
					x_dot = x;
					x    *= t[1];
				}
				if (itab == 11) {
					x_dot = x * t[1] * 2.0;
					x    *= t[2];
				}
				r[iv]   += x*sin(y);
				r[iv+3] += x_dot*sin(y) + x*cos(y)*y_dot;
			}
		}

#endif // INCLUDE_TIDAL_PERT

	}

	// Change of coordinates

	r[0] = r[0]/rad + w[0][0] + w[0][1]*t[1] + w[0][2]*t[2] + w[0][3]*t[3] + 
		              w[0][4]*t[4];
	r[3] = r[3]/rad + w[0][1] + 2*w[0][2]*t[1] + 3*w[0][3]*t[2] + 4*w[0][4]*t[3];
	r[1] = r[1]/rad;
	r[4] = r[4]/rad;
	r[2] = r[2]*a0/ath;
	r[5] = r[5]*a0/ath;
	cosr0 = cos(r[0]), sinr0 = sin(r[0]);
	cosr1 = cos(r[1]), sinr1 = sin(r[1]);
	x1       = r[2]*cosr1;
	x1_dot   = r[5]*cosr1 - r[2]*sinr1*r[4];
	x2       = x1*sinr0;
	x2_dot   = x1_dot*sinr0 + x1*cosr0*r[3];
	x1_dot   = x1_dot*cosr0 - x1*sinr0*r[3];
	x1       = x1*cosr0;
	x3       = r[2]*sinr1;
	x3_dot   = r[5]*sinr1 + r[2]*cosr1*r[4];
	pw       = (p1+p2*t[1]+p3*t[2]+p4*t[3]+p5*t[4])*t[1];
	pw_dot   = p1 + 2*p2*t[1] + 3*p3*t[2] + 4*p4*t[3] + 5*p5*t[4];
	qw       = (q1+q2*t[1]+q3*t[2]+q4*t[3]+q5*t[4])*t[1];
	qw_dot   = q1 + 2*q2*t[1] + 3*q3*t[2] + 4*q4*t[3] + 5*q5*t[4];
	ra       = 2.0*sqrt(1-pw*pw-qw*qw);
	ra_dot   = -4.0*(pw+qw)/ra;
	pwqw     = 2.0*pw*qw;
	pwqw_dot = 2.0*(pw_dot*qw + pw*qw_dot);
	pw2      = 1-2.0*pw*pw;
	pw2_dot  = -4.0*pw;
	qw2      = 1-2.0*qw*qw;
	qw2_dot  = -4.0*qw;
	pw       = pw*ra;
	pw_dot   = pw_dot*ra + pw*ra_dot;
	qw       = qw*ra;
	qw_dot   = qw_dot*ra + qw*ra_dot;
	// at this point we swap y and z components to conform with orbiter convention
	// r[1] <-> r[2] and r[4] <-> r[5]
	r[0] = pw2*x1+pwqw*x2+pw*x3;
	r[3] = pw2_dot*x1 + pw2*x1_dot + pwqw_dot*x2 + pwqw*x2_dot + pw_dot*x3 + pw*x3_dot;
	r[2] = pwqw*x1+qw2*x2-qw*x3;
	r[5] = pwqw_dot*x1 + pwqw*x1_dot + qw2_dot*x2 + qw2*x2_dot - qw_dot*x3 - qw*x3_dot;
	r[1] = -pw*x1+qw*x2+(pw2+qw2-1)*x3;
	r[4] = -pw_dot*x1 - pw*x1_dot + qw_dot*x2 + qw*x2_dot + (pw2_dot+qw2_dot)*x3 + (pw2+qw2-1)*x3_dot;

	// ========= End of ELP82 code =========
	// Below is conversion to Orbiter format

	// convert to m and m/s
	static double pscale = 1e3;
	static double vscale = 1e3/(86400.0*sc);
	r[0] *= pscale;
	r[1] *= pscale;
	r[2] *= pscale;
	r[3] *= vscale;
	r[4] *= vscale;
	r[5] *= vscale;

	return 0;
}