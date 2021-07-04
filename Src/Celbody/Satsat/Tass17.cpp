// ======================================================================
// Implementation of TASS17 perturbation solutions for Saturn moons
// Mimas, Enceladus, Tethys, Dione, Rhea, Titan, Hyperion and Iapetus
// Converted from FORTRAN sources.
// ======================================================================

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>

// temporary
#include <fstream>
#include <iostream>
#include <iomanip>
using namespace std;

// ==========================================================
// local data blocks

typedef double Term[3];
typedef int Iks[8];

static struct SeriesData {
    int ntr[5];          // number of terms in series
    Term *term[4];       // series data
    Iks *iks[4];
    double al0, an0;
} sdata[8];

static double aam[9];
static double aia, oma, tmas[9], gk1;

static struct {
    int nbtp, nbtq, nbtz, nbtzt;
    double t0, cstp, cstq, amm7, serp[120], fap[120], frp[120], serq[240],
	     faq[240], frq[240], serz[200], faz[200], frz[200], serzt[65], 
	    fazt[65], frzt[65];
} serhyp;

// ==========================================================
// local prototypes

static int calclon (double dj, const SeriesData *sd, double *dlo);
static int calcelem (double dj, int is, double *elem, const SeriesData *sd,
    double *dlo);
static int edered (double *elem, double *xyz, double *vxyz, int isat);
static void lithyp (FILE *f);
static int elemhyp (double dj, double *elem);

/* cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc */

/*  tass1.7  :  new version including hyperion  (5 nov 1996) */

/* ************************************************************************ */

/*  from tass1.6 by a. vienne and l. duriez (1995, a&a 297, 588-605) */
/*  and 'theory of motion and ephemerides of hyperion', to appear in a&a */
/*        e-mail: vienne@gat.univ-lille1.fr */
/*        e-mail: duriez@gat.univ-lille1.fr */
/* ************************************************************************ */

// ==========================================================
// positions and velocities of the satellites mimas, enceladus, tethys,
// dione, rhea, titan, hyperion and iapetus referred to the center of
// saturn and to the mean ecliptic and mean equinox for j2000.0 epoch
//
// Parameters:
// dj: Julian date
// is: satellite index:
//     0 = mimas
//     1 = enceladus
//     2 = tethys
//     3 = dione
//     4 = rhea
//     5 = titan
//     6 = hyperion
//     7 = iapetus
// xyz: ecliptic position
// vxyz: ecliptic velocity

int posired (double dj, int is, double *xyz, double *vxyz)
{
    double elem[6];
    double dlo[8];

    if (is == 6) {
		elemhyp (dj, elem);
    } else {
		calclon (dj, sdata, dlo);
		calcelem (dj, is, elem, sdata+is, dlo);
    }
    edered (elem, xyz, vxyz, is);
    return 0;
}

// ==========================================================

int calcelem (double dj, int is, double *elem, const SeriesData *sd,
    double *dlo)
{
    int i, jk;
    double phas, s, t, s1, s2, cs, sn;
    Term *tm;
    Iks *ik;

    t = (dj - 2444240.) / 365.25;
    s = 0.;
    tm = sd->term[0];
    ik = sd->iks[0];
    for (i = 0; i < sd->ntr[0]; ++i) {
		phas = tm[i][1];
		for (jk = 0; jk < 8; ++jk) phas += ik[i][jk] * dlo[jk];
		s += tm[i][0] * cos (phas + t * tm[i][2]);
    }
    elem[0] = s;
    s = dlo[is] + sd->al0;
    tm = sd->term[1];
    ik = sd->iks[1];
    for (i = sd->ntr[4]; i < sd->ntr[1]; ++i) {
		phas = tm[i][1];
		for (jk = 0; jk < 8; ++jk) phas += ik[i][jk] * dlo[jk];
		s += tm[i][0] * sin (phas + t * tm[i][2]);
    }
    s += sd->an0 * t;
    cs = cos (s);
    sn = sin (s);
    elem[1] = atan2 (sn, cs);
    s1 = s2 = 0.;
    tm = sd->term[2];
    ik = sd->iks[2];
    for (i = 0; i < sd->ntr[2]; ++i) {
		phas = tm[i][1];
		for (jk = 0; jk < 8; ++jk) phas += ik[i][jk] * dlo[jk];
		s1 += tm[i][0] * cos (phas + t * tm[i][2]);
		s2 += tm[i][0] * sin (phas + t * tm[i][2]);
    }
    elem[2] = s1;
    elem[3] = s2;
    s1 = 0.;
    s2 = 0.;
    tm = sd->term[3];
    ik = sd->iks[3];
    for (i = 0; i < sd->ntr[3]; ++i) {
		phas = tm[i][1];
		for (jk = 0; jk < 8; ++jk) phas += ik[i][jk] * dlo[jk];
		s1 += tm[i][0] * cos (phas + t * tm[i][2]);
		s2 += tm[i][0] * sin (phas + t * tm[i][2]);
    }
    elem[4] = s1;
    elem[5] = s2;
    return 0;
}

// ==========================================================

int calclon (double dj, const SeriesData *sd, double *dlo)
{
    double s, t;
    int i, is;

    t = (dj - 2444240.) / 365.25;
    for (is = 0; is < 8; ++is) {
	if (is != 6) {
	    const SeriesData *sdi = sd+is;
	    Term *tm = sdi->term[1];
	    s = 0.;
	    for (i = 0; i < sdi->ntr[4]; ++i)
		s += tm[i][0] * sin (tm[i][1] + t * tm[i][2]);
	    dlo[is] = s;
	} else {
	    dlo[is] = 0.;
	}
    }
    return 0;
}

// ==========================================================
// Read perturbation terms

void ReadData (char *fname, int res)
{
    SeriesData *sd = sdata;

    double gk, tas, tam[9], am[9];
    int i, j, k, n, m, is, ieq, nt1, nt2, nt;
    Term tm;
    Iks ik;

    static double radsdg = atan(1.) / 45.;

    FILE *f = fopen (fname, "rt");
    fscanf (f, "%lf", &gk);
    fscanf (f, "%lf", &tas);
    gk1 = pow (gk*365.25, 2.0) / tas;
    fscanf (f, "%lf", &aia);
    fscanf (f, "%lf", &oma);
    aia *= radsdg;
    oma *= radsdg;
    for (i = 0; i < 9; ++i) fscanf (f, "%lf", tam+i);
    for (i = 0; i < 9; ++i) tmas[i] = 1. / tam[i];
    for (i = 0; i < 9; ++i) fscanf (f, "%lf", am+i);
    for (i = 0; i < 9; ++i) aam[i] = am[i] * 365.25;

    for (i = 0; i < 8; i++) { // loop over objects
	if (i == 6) continue; // skip Hyperion
	for (j = 0; j < 4; j++) { // loop over series
	    fscanf (f, "%d%d%d%d", &is, &ieq, &nt1, &nt2);
	    nt = (res == 0 ? nt2 : nt1);
	    sd[i].ntr[j] = nt;
	    sd[i].term[j] = new Term[nt];
	    sd[i].iks[j] = new Iks[nt];
	    if (ieq == 2) {
		fscanf (f, "%d%lf%lf", &k, &sd[i].al0, &sd[i].an0);
		sd[i].ntr[4] = nt1;
	    }
	    for (k = 0; k < nt2; k++) {
		fscanf (f, "%d%lf%lf%lf", &n, tm+0, tm+1, tm+2);
		fscanf (f, "%d%d%d%d%d%d%d%d",
			ik+0, ik+1, ik+2, ik+3, ik+4, ik+5, ik+6, ik+7);
		if (k < nt) {
		    for (m = 0; m < 3; m++) sd[i].term[j][k][m] = tm[m];
		    for (m = 0; m < 8; m++) sd[i].iks[j][k][m] = ik[m];
		}
	    }
	}
    }
    // Read Hyperion data
    lithyp (f);
    fclose (f);
}

// ==========================================================

int edered (double *elem, double *xyz, double *vxyz, int isat)
{
    double xyz2[3], corf, dwho, rsam1, vxyz2[3], x1, y1, cf, ci, 
	    co, sf, rh, si, rk, rl, so, vx1, vy1, dga, dlf, fle, amo, rdg, 
	    phi, asr, psi, rmu, rtp, rtq;

    amo = aam[isat] * (elem[0] + 1.);
    rmu = gk1 * (tmas[isat] + 1.);
    dga = pow (rmu / (amo * amo), .33333333333333331);
    rl = elem[1];
    rk = elem[2];
    rh = elem[3];
    fle = rl - rk * sin(rl) + rh * cos(rl);
    do {
		cf = cos(fle);
		sf = sin(fle);
		corf = (rl - fle + rk * sf - rh * cf) / (1 - rk * cf - rh * sf);
		fle += corf;
    } while (fabs(corf) >= 1e-14);
    cf = cos(fle);
    sf = sin(fle);
    dlf = -rk * sf + rh * cf;
    rsam1 = -rk * cf - rh * sf;
    asr = 1. / (rsam1 + 1.);
    phi = sqrt(1. - rk * rk - rh * rh);
    psi = 1. / (phi + 1.);
    x1 = dga * (cf - rk - psi * rh * dlf);
    y1 = dga * (sf - rh + psi * rk * dlf);
    vx1 = amo * asr * dga * (-sf - psi * rh * rsam1);
    vy1 = amo * asr * dga * (cf + psi * rk * rsam1);
    dwho = sqrt(1. - elem[5] * elem[5] - elem[4] * elem[4]) * 2.;
    rtp = 1. - elem[5] * 2. * elem[5];
    rtq = 1. - elem[4] * 2. * elem[4];
    rdg = elem[5] * 2. * elem[4];
    xyz2[0] = x1 * rtp + y1 * rdg;
    xyz2[1] = x1 * rdg + y1 * rtq;
    xyz2[2] = (-x1 * elem[5] + y1 * elem[4]) * dwho;
    vxyz2[0] = vx1 * rtp + vy1 * rdg;
    vxyz2[1] = vx1 * rdg + vy1 * rtq;
    vxyz2[2] = (-vx1 * elem[5] + vy1 * elem[4]) * dwho;
    ci = cos(aia);
    si = sin(aia);
    co = cos(oma);
    so = sin(oma);
    xyz[0] = co * xyz2[0] - so * ci * xyz2[1] + so * si * xyz2[2];
    xyz[1] = so * xyz2[0] + co * ci * xyz2[1] - co * si * xyz2[2];
    xyz[2] = si * xyz2[1] + ci * xyz2[2];
    vxyz[0] = co * vxyz2[0] - so * ci * vxyz2[1] + so * si * vxyz2[2];
    vxyz[1] = so * vxyz2[0] + co * ci * vxyz2[1] - co * si * vxyz2[2];
    vxyz[2] = si * vxyz2[1] + ci * vxyz2[2];
    return 0;
}

// ==========================================================
// Read perturbation terms for Hyperion

void lithyp (FILE *f)
{
    int i;

    fscanf (f, "%lf", &serhyp.t0);
    fscanf (f, "%lf", &serhyp.amm7);
    fscanf (f, "%d", &serhyp.nbtp);
    fscanf (f, "%lf", &serhyp.cstp);
    for (i = 0; i < serhyp.nbtp; ++i)
		fscanf (f, "%lf%lf%lf",
			serhyp.serp+i, serhyp.fap+i, serhyp.frp+i);
    fscanf (f, "%d", &serhyp.nbtq);
    fscanf (f, "%lf", &serhyp.cstq);
    for (i = 0; i < serhyp.nbtq; ++i)
		fscanf (f, "%lf%lf%lf",
			serhyp.serq+i, serhyp.faq+i, serhyp.frq+i);
    fscanf (f, "%d", &serhyp.nbtz);
    for (i = 0; i < serhyp.nbtz; ++i)
		fscanf (f, "%lf%lf%lf",
			serhyp.serz+i, serhyp.faz+i, serhyp.frz+i);
    fscanf (f, "%d", &serhyp.nbtzt);
    for (i = 0; i < serhyp.nbtzt; ++i)
		fscanf (f, "%lf%lf%lf",
			serhyp.serzt+i, serhyp.fazt+i, serhyp.frzt+i);
}

// ==========================================================

int elemhyp (double dj, double *elem)
{
    int i;
    double p, q, t, vl, zi, wt, zr, zti, ztr;

    t = dj - serhyp.t0;

    p = serhyp.cstp;
    for (i = 1; i <= serhyp.nbtp; ++i) {
		wt = t * serhyp.frp[i - 1] + serhyp.fap[i - 1];
		p += serhyp.serp[i - 1] * cos(wt);
    }
    q = serhyp.cstq;
    for (i = 1; i <= serhyp.nbtq; ++i) {
		wt = t * serhyp.frq[i - 1] + serhyp.faq[i - 1];
		q += serhyp.serq[i - 1] * sin(wt);
    }
    zr = 0.;
    zi = 0.;
    for (i = 1; i <= serhyp.nbtz; ++i) {
		wt = t * serhyp.frz[i - 1] + serhyp.faz[i - 1];
		zr += serhyp.serz[i - 1] * cos(wt);
		zi += serhyp.serz[i - 1] * sin(wt);
    }
    ztr = 0.;
    zti = 0.;
    for (i = 1; i <= serhyp.nbtzt; ++i) {
		wt = t * serhyp.frzt[i - 1] + serhyp.fazt[i - 1];
		ztr += serhyp.serzt[i - 1] * cos(wt);
		zti += serhyp.serzt[i - 1] * sin(wt);
    }
    vl = fmod (serhyp.amm7 * t + q, 6.2831853071795862);
    if (vl < 0.) {
		vl += 6.2831853071795862;
    }
    elem[0] = p;
    elem[1] = vl;
    elem[2] = zr;
    elem[3] = zi;
    elem[4] = ztr;
    elem[5] = zti;

	return 0;
}

int nterm (int is)
{
	int nt = 0;
	if (is != 6) {
		for (int series = 0; series < 4; series++) nt += sdata[is].ntr[series];
	} else {
		nt = serhyp.nbtp + serhyp.nbtq + serhyp.nbtz + serhyp.nbtzt;
	}
	return nt;
}