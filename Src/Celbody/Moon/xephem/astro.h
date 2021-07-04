#ifndef PI
#define	PI		3.141592653589793
#endif

/* conversions among hours (of ra), degrees and radians. */
#define	degrad(x)	((x)*PI/180.)
#define	raddeg(x)	((x)*180./PI)
#define	hrdeg(x)	((x)*15.)
#define	deghr(x)	((x)/15.)
#define	hrrad(x)	degrad(hrdeg(x))
#define	radhr(x)	deghr(raddeg(x))

/* ratio of from synodic (solar) to sidereal (stellar) rate */
#define	SIDRATE		.9972695677

#define	SPD	(24.0*3600.0)	/* seconds per day */

/* manifest names for planets.
 * N.B. must coincide with usage in pelement.c and plans.c.
 * N.B. only the first 8 are valid for use with plans().
 */
#define	MERCURY	0
#define	VENUS	1
#define	MARS	2
#define	JUPITER	3
#define	SATURN	4
#define	URANUS	5
#define	NEPTUNE	6
#define	PLUTO	7

/* a few more handy ones */
#define	SUN	8
#define	MOON	9
#define	OBJX	10
#define	OBJY	11
#define	OBJZ	12
#define	NOBJ	13	/* total number of basic objects */

/* starting point for MJD calculations
 */
#define MJD0  2415020.0
#define J2000 (2451545.0 - MJD0)      /* let compiler optimise */


/* global function declarations */

/* aa_hadec.c */
extern void aa_hadec P_((double lat, double alt, double az, double *ha,
    double *dec));
extern void hadec_aa P_((double lat, double ha, double dec, double *alt,
    double *az)); 

/* aberration.c */
extern void ab_ecl P_((double mjd, double lsn, double *lam, double *bet));
extern void ab_eq P_((double mjd, double lsn, double *ra, double *dec));

/* anomaly.c */
extern void anomaly P_((double ma, double s, double *nu, double *ea));

/* chap95.c */
extern int chap95 P_((double mjd, int obj, double prec, double *ret));

/* chap95_data.c */

/* comet.c */
extern void comet P_((double mjd, double ep, double inc, double ap, double qp,
    double om, double *lpd, double *psi, double *rp, double *rho, double *lam,
    double *bet));

/* deltat.c */
extern double deltat P_((double mjd));

/* eq_ecl.c */
extern void eq_ecl P_((double mjd, double ra, double dec, double *lat,
    double *lng));
extern void ecl_eq P_((double mjd, double lat, double lng, double *ra,
    double *dec));

/* eq_gal.c */
extern void eq_gal P_((double mjd, double ra, double dec, double *lat,
    double *lng));
extern void gal_eq P_((double mjd, double lat, double lng, double *ra,
    double *dec));

/* formats.c */
extern void fs_sexa P_((char *out, double a, int w, int fracbase));
extern void fs_date P_((char out[], double jd));
extern void f_scansex P_((double o, char *str, double *dp));
extern void f_sscandate P_((char *bp, int pref, int *m, double *d, int *y));
extern int scansex P_((char *str, double *dp));

/* helio.c */
extern void heliocorr P_((double jd, double ra, double dec, double *hcp));

/* libration.c */
extern void llibration P_((double JD, double *llatp, double *llonp));

/* misc.c */
extern void zero_mem P_((void *loc, unsigned len));
extern int tickmarks P_((double min, double max, int numdiv, double ticks[]));
extern int lc P_((int cx, int cy, int cw, int x1, int y1, int x2, int y2,
    int *sx1, int *sy1, int *sx2, int *sy2));
extern void hg_mag P_((double h, double g, double rp, double rho, double rsn,
    double *mp));
extern int magdiam P_((int fmag, int magstp, double scale, double mag,
    double size));
extern void gk_mag P_((double g, double k, double rp, double rho, double *mp));
extern double atod P_((char *buf));
extern void solve_sphere P_((double A, double b, double cc, double sc,
    double *cap, double *Bp));
extern double delra P_((double dra));

/* mjd.c */
extern void cal_mjd P_((int mn, double dy, int yr, double *mjd));
extern void mjd_cal P_((double mjd, int *mn, double *dy, int *yr));
extern int mjd_dow P_((double mjd, int *dow));
extern void mjd_dpm P_((double mjd, int *ndays));
extern void mjd_year P_((double mjd, double *yr));
extern void year_mjd P_((double y, double *mjd));
extern void rnd_second P_((double *t));
extern double mjd_day P_((double jd));
extern double mjd_hr P_((double jd));
extern void range P_((double *v, double r));

/* moon.c */
extern void moon P_((double mjd, double *lam, double *bet, double *rho,
    double *msp, double *mdp));

/* mooncolong.c */
extern void moon_colong P_((double jd, double lt, double lg, double *cp, double *kp, double *ap, double *sp));

/* nutation.c */
extern void nutation P_((double mjd, double *deps, double *dpsi));
extern void nut_eq P_((double mjd, double *ra, double *dec));

/* obliq.c */
extern void obliquity P_((double mjd, double *eps));

/* parallax.c */
extern void ta_par P_((double tha, double tdec, double phi, double ht,
    double *rho, double *aha, double *adec));

/* plans.c */
extern void plans P_((double mjd, int p, double *lpd0, double *psi0,
    double *rp0, double *rho0, double *lam, double *bet, double *dia,
    double *mag));

/* precess.c */
extern void precess P_((double mjd1, double mjd2, double *ra, double *dec));

/* reduce.c */
extern void reduce_elements P_((double mjd0, double mjd, double inc0,
    double ap0, double om0, double *inc, double *ap, double *om));

/* refract.c */
extern void unrefract P_((double pr, double tr, double aa, double *ta));
extern void refract P_((double pr, double tr, double ta, double *aa));

/* riset.c */
extern void riset P_((double ra, double dec, double lat, double dis,
    double *lstr, double *lsts, double *azr, double *azs, int *status));

/* sphcart.c */
extern void sphcart P_((double l, double b, double r, double *x, double *y,
    double *z));
extern void cartsph P_((double x, double y, double z, double *l, double *b,
    double *r));

/* sun.c */
extern void sunpos P_((double mjd, double *lsn, double *rsn, double *bsn));

/* utc_gst.c */
extern void utc_gst P_((double mjd, double utc, double *gst));
extern void gst_utc P_((double mjd, double gst, double *utc));

/* vsop87.c */
extern int vsop87 P_((double mjd, int obj, double prec, double *ret));

/* For RCS Only -- Do Not Edit
 * @(#) $RCSfile: astro.h,v $ $Date: 1998/02/26 17:56:24 $ $Revision: 1.1 $ $Name:  $
 */
