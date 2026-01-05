/* zzsgp4.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static doublereal c_b10 = -.66666666666666663;
static doublereal c_b11 = .33333333333333331;
static doublereal c_b16 = 3.5;
static doublereal c_b22 = 1.5;
static logical c_false = FALSE_;

/* $Procedure ZZSGP4 ( SGP4 wrapper ) */
/* Subroutine */ int zzsgp4_0_(int n__, doublereal *geophs, doublereal *elems,
	 integer *opmode, doublereal *t, doublereal *state)
{
    /* System generated locals */
    doublereal d__1, d__2;

    /* Builtin functions */
    double pow_dd(doublereal *, doublereal *), cos(doublereal), sin(
	    doublereal), d_mod(doublereal *, doublereal *), sqrt(doublereal), 
	    atan2(doublereal, doublereal);

    /* Local variables */
    doublereal eccm;
    static doublereal ecco;
    doublereal eccp, coef, eeta;
    static doublereal alta, dedt;
    doublereal cnod;
    static doublereal con41;
    doublereal con42, delm;
    static doublereal didt, dmdt;
    doublereal dndt;
    static doublereal pgho;
    doublereal ainv, cosi;
    static doublereal altp;
    doublereal axnl;
    static doublereal mdot;
    doublereal aynl, emsq;
    static doublereal j3oj2;
    doublereal sini, snod, cosu, temp;
    static doublereal gsto;
    doublereal sinu, tvec[8], xinc;
    static doublereal zmol;
    doublereal posq, xmdf;
    integer iter;
    static integer irez;
    static doublereal zmos;
    doublereal coef1, cc1sq;
    static doublereal t2cof, t3cof, t4cof, t5cof;
    doublereal temp1, temp2, temp3, temp4, cos2u, sin2u;
    static doublereal a;
    doublereal betal, u;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    doublereal eccsq;
    static doublereal atime, aycof;
    doublereal cnodm;
    static doublereal inclo, xfact, pinco;
    doublereal argpm;
    static doublereal argpo, xlcof, xmcof;
    doublereal argpp;
    static doublereal bstar;
    doublereal cosim;
    static doublereal xlamo;
    doublereal cosio;
    static doublereal x1mth2;
    doublereal cosip;
    static doublereal delmo, d2, d3, x7thm1, e3, d4, dnodt;
    doublereal cossu;
    static doublereal domdt;
    doublereal ecose, epoch, esine, etasq, inclm;
    static doublereal j2;
    doublereal j3, j4, nodem;
    static doublereal nodeo;
    doublereal nodep, psisq, qzms24, rdotl, rvdot, s1, s2, s3, s4, s5, s6, s7,
	     sfour, sinim, sinio, sinip, coseo1, sinsu, snodm, t2, t3, cosio2,
	     sineo1, cosio4, t4, tempa, tempe, templ, tumin, tzero, xhdot1, 
	    xincp, xnode, z1, z2, z3;
    extern doublereal twopi_(void);
    doublereal am;
    extern /* Subroutine */ int errdp_(char *, doublereal *, ftnlen);
    doublereal ao;
    extern logical failed_(void);
    static doublereal er;
    doublereal tc;
    extern doublereal pi_(void);
    doublereal mm;
    static doublereal mo;
    doublereal mp;
    static doublereal no;
    doublereal mr, omgadf, pl, mv, qzms2t, rl, delomg, rp;
    static doublereal omgcof;
    doublereal perige, ss, su, ux, uy;
    static doublereal xnodcf;
    doublereal uz;
    static doublereal cc1, sinmao;
    doublereal cc2;
    static doublereal cc4, cc5, ee2;
    doublereal cc3, cosomm, vx, cosisq, el2, eo1, omeosq, sinomm, vy, vz, 
	    rvdotl, rtemsq;
    static doublereal se2;
    doublereal rteosq;
    static doublereal se3, sh2;
    doublereal pinvsq;
    static doublereal sh3, xh2, xh3, xi2, xi3, xl2, xl3, xl4, si2, si3, sl2, 
	    sl3, sl4;
    doublereal ss1, ss2, ss3, ss4, ss5, ss6, ss7, sz1, sz2, sz3, xl;
    static doublereal d2201, d2211, d3210;
    doublereal xn;
    static doublereal d3222, d4410, d5220, d4422, d5232, d5421, d5433;
    doublereal xnoddf, gam, xpidot, z11, z12, z13;
    static doublereal eta;
    doublereal z21, z22, z23, day, z31, z32, z33;
    static integer svmode;
    logical doinit;
    static doublereal peo;
    static logical dosimp, dodeep;
    static doublereal pho, xke, plo;
    doublereal x2o3;
    static doublereal xli;
    doublereal kps;
    static doublereal xni;
    doublereal sz11, sz12, sz13, sz21, sz22, sz23, sz31, sz32, sz33, tsi, xlm;
    extern logical return_(void);
    static doublereal nodedot;
    doublereal xmx, xmy;
    extern /* Subroutine */ int setmsg_(char *, ftnlen);
    static doublereal argpdot;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), ttrans_(char *, char *, doublereal *, ftnlen, ftnlen), 
	    zzinil_(doublereal *, integer *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *);
    static doublereal del1, del2, del3;
    extern /* Subroutine */ int zzdscm_(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *), zzdspr_(integer *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, logical *,
	     doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *), zzdsin_(doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, integer *, doublereal *, doublereal *, doublereal *,
	     doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *), zzdspc_(integer *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *);
    static doublereal sgh2, sgh3, sgh4, xgh2, xgh3, xgh4;
    doublereal tem5;

/* $ Abstract */

/*     Umbrella for the SGP4 initializer and evaluator routines. */

/* $ Disclaimer */

/*     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE */
/*     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S. */
/*     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE */
/*     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE */
/*     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS" */
/*     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY */
/*     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A */
/*     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC */
/*     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE */
/*     SOFTWARE AND RELATED MATERIALS, HOWEVER USED. */

/*     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA */
/*     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT */
/*     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND, */
/*     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS, */
/*     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE */
/*     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY. */

/*     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF */
/*     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY */
/*     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE */
/*     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE. */

/* $ Required_Reading */

/*     None. */

/* $ Keywords */

/*     None. */

/* $ Declarations */
/* $Procedure ZZSGP4 ( SGP4 parameters ) */

/* $ Abstract */

/*      Parameter assignments for SGP4 algorithm as expressed */
/*      by Vallado [2]. */

/* $ Disclaimer */

/*     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE */
/*     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S. */
/*     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE */
/*     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE */
/*     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS" */
/*     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY */
/*     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A */
/*     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC */
/*     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE */
/*     SOFTWARE AND RELATED MATERIALS, HOWEVER USED. */

/*     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA */
/*     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT */
/*     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND, */
/*     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS, */
/*     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE */
/*     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY. */

/*     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF */
/*     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY */
/*     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE */
/*     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE. */

/* $ Required_Reading */

/*     None. */

/* $ Keywords */

/*     None. */

/* $ Declarations */

/*     None. */

/* $ Brief_I/O */

/*     None. */

/* $ Detailed_Input */

/*     None. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     None. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     None. */

/* $ Examples */

/*     J2    = GEOPHS(K_J2) */
/*     J3    = GEOPHS(K_J3) */
/*     J4    = GEOPHS(K_J4) */
/*     ER    = GEOPHS(K_ER) */
/*     XKE   = GEOPHS(K_KE) */

/*     TUMIN = 1.D0/XKE */
/*     J3OJ2 = J3/J2 */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*   [1] Hoots, F. R., and Roehrich, R. L. 1980. "Models for */
/*       Propagation of the NORAD Element Sets." Spacetrack Report #3. */
/*       U.S. Air Force: Aerospace Defense Command. */

/*   [2] Vallado, David, Crawford, Paul, Hujsak, Richard, and Kelso, T.S. */
/*       2006. Revisiting Spacetrack Report #3. Paper AIAA 2006-6753 */
/*       presented at the AIAA/AAS Astrodynamics Specialist Conference, */
/*       August 21-24, 2006. Keystone, CO. */

/* $ Author_and_Institution */

/*     E. D. Wright    (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, MAY-27-2020 (EDW) */

/*        Updated descriptions of GEOPHS constants to be consistent */
/*        with what's used in other routines. */

/* -    SPICELIB Version 1.0.0 22-JUL-2014 (EDW) */

/* -& */
/* $ Index_Entries */

/*  SGP4 */

/* -& */

/*      WGS gravitational constants IDs. */


/*      Gravitational constant indices. */


/*     The following parameters give the indices in the GEOPHS */
/*     array of the various geophysical parameters needed for */
/*     the two line element sets. */

/*     K_J2  --- index of J2 gravitational harmonic for earth */
/*     K_J3  --- index of J3 gravitational harmonic for earth */
/*     K_J4  --- index of J4 gravitational harmonic for earth */
/*     K_KE  --- index of KE = sqrt(GM) in earth-radii**1.5/MIN */
/*     K_QO  --- index of high altitude bound for atmospheric */
/*               model in km */
/*     K_SO  --- index of low altitude bound for atmospheric */
/*               model in km */
/*     K_ER  --- index of earth equatorial radius in km */
/*     K_AE  --- index of distance units/earth radius */


/*     Operation mode values, OPMODE. */


/*     An enumeration of the various components of the */
/*     elements array---ELEMS */

/*     KNDT20  --- location of NDT20 */
/*     KNDD60  --- location of NDD60 */
/*     KBSTAR  --- location of BSTAR */
/*     KINCL   --- location of INCL */
/*     KNODE0  --- location of NODE0 */
/*     KECC    --- location of ECC */
/*     KOMEGA  --- location of OMEGA */
/*     KMO     --- location of MO */
/*     KNO     --- location of NO */

/* $ Brief_I/O */

/*     VARIABLE  I/O  ENTRY POINTS */
/*     --------  ---  -------------------------------------------------- */
/*     GEOPHS     I   XXSGP4I */
/*     ELEMS      I   XXSGP4I */
/*     OPMODE     I   XXSGP4I */
/*     T          I   XXSGP4E */
/*     STATE      O   XXSGP4E */

/* $ Detailed_Input */

/*     See Individual Entry points. */

/* $ Detailed_Output */

/*     See Individual Entry points. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If ZZSGP4 is called, the error SPICE(BOGUSENTRY) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine wraps XXSGP4E and XXSGP4I. As entry points to */
/*     this routine, they share the local memory space. */

/* $ Examples */

/*     The numerical results shown for these examples may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     Use a set of TLEs to calculate a collection of states for a */
/*     time interval centered at the TLE set epoch. */

/*           PROGRAM ZZSGP4_T */

/*     C */
/*     C     Read a data file containing sets of TLEs, then calculate */
/*     C     states at -1440 to 1440 minutes from the epoch of each */
/*     C     TLE set in steps of 10 minutes. */
/*     C */
/*     C     Example cases listed in sgp4-ver.tle. */
/*     C */
/*     C     1 00005U 58002B   00179.78495062  .00000023 */
/*     C       00000-0  28098-4 0  4753 */
/*     C     2 00005  34.2682 348.7242 1859667 331.7664 */
/*     C       19.3264 10.82419157413667 */
/*     C */
/*     C     1 04632U 70093B   04031.91070959 -.00000084 */
/*     C       00000-0  10000-3 0  9955 */
/*     C     2 04632  11.4628 273.1101 1450506 207.6000 */
/*     C       143.9350  1.20231981 44145 */
/*     C */
/*     C     1 06251U 62025E   06176.82412014  .00008885 */
/*     C       00000-0  12808-3 0  3985 */
/*     C     2 06251  58.0579  54.0425 0030035 139.1568 */
/*     C       221.1854 15.56387291  6774 */
/*     C */
/*     C     1 08195U 75081A   06176.33215444  .00000099 */
/*     C       00000-0  11873-3 0   813 */
/*     C     2 08195  64.1586 279.0717 6877146 264.7651 */
/*     C       20.2257  2.00491383225656 */
/*     C */
/*     C     1 09880U 77021A   06176.56157475  .00000421 */
/*     C       00000-0  10000-3 0  9814 */
/*     C     2 09880  64.5968 349.3786 7069051 270.0229 */
/*     C       16.3320  2.00813614112380 */
/*     C */
/*     C     1 09998U 74033F   05148.79417928 -.00000112 */
/*     C       00000-0  00000+0 0  4480 */
/*     C     2 09998   9.4958 313.1750 0270971 327.5225 */
/*     C       30.8097  1.16186785 45878 */
/*     C */
/*     C     1 11801U          80230.29629788  .01431103 */
/*     C       00000-0  14311-1      13 */
/*     C     2 11801  46.7916 230.4354 7318036  47.4722 */
/*     C       10.4117  2.28537848    13 */
/*     C */
/*     C     1 14128U 83058A   06176.02844893 -.00000158 */
/*     C       00000-0  10000-3 0  9627 */
/*     C     2 14128  11.4384  35.2134 0011562  26.4582 */
/*     C       333.5652  0.98870114 46093 */
/*     C */
/*     C     1 16925U 86065D   06151.67415771  .02550794 */
/*     C       -30915-6  18784-3 0  4486 */
/*     C     2 16925  62.0906 295.0239 5596327 245.1593 */
/*     C       47.9690  4.88511875148616 */
/*     C */
/*     C     1 20413U 83020D   05363.79166667  .00000000 */
/*     C       00000-0  00000+0 0  7041 */
/*     C     2 20413  12.3514 187.4253 7864447 196.3027 */
/*     C       356.5478  0.24690082  7978 */
/*     C */
/*     C     1 21897U 92011A   06176.02341244 -.00001273 */
/*     C       00000-0 -13525-3 0  3044 */
/*     C     2 21897  62.1749 198.0096 7421690 253.0462 */
/*     C       20.1561  2.01269994104880 */
/*     C */
/*     C     1 22312U 93002D   06094.46235912  .99999999 */
/*     C       81888-5  49949-3 0  3953 */
/*     C     2 22312  62.1486  77.4698 0308723 267.9229 */
/*     C       88.7392 15.95744531 98783 */
/*     C */
/*     C     1 22674U 93035D   06176.55909107  .00002121 */
/*     C       00000-0  29868-3 0  6569 */
/*     C     2 22674  63.5035 354.4452 7541712 253.3264 */
/*     C       18.7754  1.96679808 93877 */
/*     C */
/*     C     1 23177U 94040C   06175.45752052  .00000386 */
/*     C       00000-0  76590-3 0    95 */
/*     C     2 23177   7.0496 179.8238 7258491 296.0482 */
/*     C       8.3061  2.25906668 97438 */
/*     C */
/*     C     1 23333U 94071A   94305.49999999 -.00172956 */
/*     C       26967-3  10000-3 0    15 */
/*     C     2 23333  28.7490   2.3720 9728298  30.4360 */
/*     C       1.3500  0.07309491    70 */
/*     C */
/*     C     1 23599U 95029B   06171.76535463  .00085586 */
/*     C       12891-6  12956-2 0  2905 */
/*     C     2 23599   6.9327   0.2849 5782022 274.4436 */
/*     C       25.2425  4.47796565123555 */
/*     C */
/*     C     1 24208U 96044A   06177.04061740 -.00000094 */
/*     C       00000-0  10000-3 0  1600 */
/*     C     2 24208   3.8536  80.0121 0026640 311.0977 */
/*     C       48.3000  1.00778054 36119 */
/*     C */
/*     C     1 25954U 99060A   04039.68057285 -.00000108 */
/*     C       00000-0  00000-0 0  6847 */
/*     C     2 25954   0.0004 243.8136 0001765  15.5294 */
/*     C       22.7134  1.00271289 15615 */
/*     C */
/*     C     1 26900U 01039A   06106.74503247  .00000045 */
/*     C       00000-0  10000-3 0  8290 */
/*     C     2 26900   0.0164 266.5378 0003319  86.1794 */
/*     C       182.2590  1.00273847 16981 */
/*     C */
/*     C     1 26975U 78066F   06174.85818871  .00000620 */
/*     C       00000-0  10000-3 0  6809 */
/*     C     2 26975  68.4714 236.1303 5602877 123.7484 */
/*     C       302.5767  2.05657553 67521 */
/*     C */
/*     C     1 28057U 03049A   06177.78615833  .00000060 */
/*     C       00000-0  35940-4 0  1836 */
/*     C     2 28057  98.4283 247.6961 0000884  88.1964 */
/*     C       271.9322 14.35478080140550 */
/*     C */
/*     C     1 28129U 03058A   06175.57071136 -.00000104 */
/*     C       00000-0  10000-3 0   459 */
/*     C     2 28129  54.7298 324.8098 0048506 266.2640 */
/*     C       93.1663  2.00562768 18443 */
/*     C */
/*     C     1 28350U 04020A   06167.21788666  .16154492 */
/*     C       76267-5  18678-3 0  8894 */
/*     C     2 28350  64.9977 345.6130 0024870 260.7578 */
/*     C       99.9590 16.47856722116490 */
/*     C */
/*     C     1 28623U 05006B   06177.81079184  .00637644 */
/*     C       69054-6  96390-3 0  6000 */
/*     C     2 28623  28.5200 114.9834 6249053 170.2550 */
/*     C       212.8965  3.79477162 12753 */
/*     C */
/*     C     1 28626U 05008A   06176.46683397 -.00000205 */
/*     C       00000-0  10000-3 0  2190 */
/*     C     2 28626   0.0019 286.9433 0000335  13.7918 */
/*     C       55.6504  1.00270176  4891 */
/*     C */
/*     C     1 28872U 05037B   05333.02012661  .25992681 */
/*     C       00000-0  24476-3 0  1534 */
/*     C     2 28872  96.4736 157.9986 0303955 244.0492 */
/*     C       110.6523 16.46015938 10708 */
/*     C */
/*     C     1 29141U 85108AA  06170.26783845  .99999999 */
/*     C       00000-0  13519-0 0   718 */
/*     C     2 29141  82.4288 273.4882 0015848 277.2124 */
/*     C       83.9133 15.93343074  6828 */
/*     C */
/*     C     1 29238U 06022G   06177.28732010  .00766286 */
/*     C       10823-4  13334-2 0   101 */
/*     C     2 29238  51.5595 213.7903 0202579  95.2503 */
/*     C       267.9010 15.73823839  1061 */
/*     C */
/*     C     1 88888U          80275.98708465  .00073094 */
/*     C       13844-3  66816-4 0    87 */
/*     C     2 88888  72.8435 115.9689 0086731  52.6988 */
/*     C       110.5714 16.05824518  1058 */
/*     C */

/*           IMPLICIT NONE */

/*           INCLUDE 'zzsgp4.inc' */

/*           CHARACTER*(72)           LINES  ( 2 ) */
/*           CHARACTER*(72)           TLEDAT */

/*           INTEGER                  FRSTYR */
/*           INTEGER                  I */
/*           INTEGER                  OPMODE */

/*           DOUBLE PRECISION         DELT */
/*           DOUBLE PRECISION         ELEMS  ( 10 ) */
/*           DOUBLE PRECISION         EPOCH */
/*           DOUBLE PRECISION         GEOPHS ( 8 ) */
/*           DOUBLE PRECISION         STATE  ( 6 ) */
/*           DOUBLE PRECISION         TF */
/*           DOUBLE PRECISION         TIME */
/*           DOUBLE PRECISION         TS */

/*           LOGICAL                  EOF */

/*     C */
/*     C     SPICELIB routines. */
/*     C */
/*           LOGICAL                  FAILED */

/*     C */
/*     C     Load a leapseconds kernel for time conversion. Required */
/*     C     by the SPK 10 evaluator. */
/*     C */
/*           CALL FURNSH ( '/kernels/gen/lsk/naif0011.tls' ) */

/*     C */
/*     C     Define the geophysical quantities using the values */
/*     C     from geophysical.ker. */
/*     C */
/*           GEOPHS( K_J2 ) =    1.082616D-3 */
/*           GEOPHS( K_J3 ) =   -2.53881D-6 */
/*           GEOPHS( K_J4 ) =   -1.65597D-6 */
/*           GEOPHS( K_KE ) =    7.43669161D-2 */
/*           GEOPHS( K_QO ) =  120.0D0 */
/*           GEOPHS( K_SO ) =   78.0D0 */
/*           GEOPHS( K_ER ) = 6378.135D0 */
/*           GEOPHS( K_AE ) =    1.0D0 */

/*           TLEDAT = 'sgp4-ver1.tle' */

/*     C */
/*     C     Error subsystem to report to ensure execution continues */
/*     C     if an error signals. */
/*     C */
/*           CALL ERRACT( 'SET', 'REPORT') */

/*     C */
/*     C     Use Spacetrack #3 algorithm to calculate sidereal time. */
/*     C */
/*           OPMODE = AFSPC */

/*     C */
/*     C     Identify the earliest year for the elements. */
/*     C */
/*           FRSTYR = 1958 */

/*     C */
/*     C     Start and final offsets from TLE epochs. [-1440, 1400] */
/*     C     minutes. */
/*     C */
/*           TS     =  -1440.0D0 */
/*           TF     =   1440.0D0 */

/*     C */
/*     C     Step size for elements output 10 minutes. */
/*     C */
/*           DELT   = 10.D0 */

/*     C */
/*     C     Read the TLE data file. */
/*     C */
/*           CALL RDTEXT ( TLEDAT, LINES(1), EOF ) */
/*           CALL RDTEXT ( TLEDAT, LINES(2), EOF ) */

/*     C */
/*     C     Loop over data file until end-of-file. */
/*     C */
/*           DO WHILE ( .NOT. EOF ) */

/*     C */
/*     C        Parse the elements to something SPICE can use. */
/*     C */
/*              CALL GETELM ( FRSTYR, LINES, EPOCH, ELEMS ) */

/*              WRITE(*, FMT='(A72)') LINES(1) */
/*              WRITE(*, FMT='(A72)') LINES(2) */
/*              WRITE(*,*) ' ' */

/*     C */
/*     C        Initialize SGP4 calculations based on values in */
/*     C        GEOPHS, ELEMS, and AFSPC. */
/*     C */
/*              CALL XXSGP4I ( GEOPHS, ELEMS, OPMODE ) */

/*     C */
/*     C        Start time keyed in minutes from TLE epoch. */
/*     C */
/*              TIME   = TS */

/*              DO WHILE ( TIME .LE. DABS(TF) .AND. (.NOT. FAILED()) ) */

/*     C */
/*     C           Calculate the STATE at TIME. */
/*     C */
/*                 CALL XXSGP4E ( TIME, STATE ) */

/*     C */
/*     C           If the propagation succeeded, output the STATE. */
/*     C */
/*                 IF ( .NOT. FAILED() ) THEN */

/*                    WRITE(*, FMT ='(7F17.8)' ) TIME, */
/*          .                                    (STATE(I),I=1,6) */

/*                 END IF */

/*     C */
/*     C           Increment the evaluation time by one step. */
/*     C */
/*                 TIME = TIME + DELT */

/*              END DO */

/*              WRITE(*,*) ' ' */

/*     C */
/*     C        reset the error subsystem for the next loop. */
/*     C */
/*              CALL RESET() */

/*     C */
/*     C        Read the next two lines (if any) from the TLE */
/*     C        data file. */
/*     C */
/*              CALL RDTEXT ( TLEDAT, LINES(1), EOF ) */
/*              CALL RDTEXT ( TLEDAT, LINES(2), EOF ) */

/*           END DO */

/*           END */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     [1] Hoots, F. R., and Roehrich, R. L. 1980. "Models for */
/*         Propagation of the NORAD Element Sets." Spacetrack Report #3. */
/*         U.S. Air Force: Aerospace Defense Command. */

/*     [2] Hoots, Felix R. "Spacetrack Report #6: Models for Propagation */
/*         of Space Command Element Sets." Space Command, */
/*         U. S. Air Force, CO. */

/*     [3] Hoots, Felix R., P. W. Schumacher, and R. A. Glover. 2004. */
/*         History of Analytical Orbit Modeling in the U. S. Space */
/*         Surveillance System. Journal of Guidance, Control, and */
/*         Dynamics. 27(2):174-185. */

/*     [4] Vallado, David, Crawford, Paul, Hujsak, Richard, */
/*         and Kelso, T.S. 2006. Revisiting Spacetrack Report #3. Paper */
/*         AIAA 2006-6753 presented at the AIAA/AAS Astrodynamics */
/*         Specialist Conference, August 21-24, 2006. Keystone, CO. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 30-MAY-2021 (EDW) (JDR) */

/*        Correction of documentation error in listing of GEOPHS */
/*        constants. The definition of index 5, "High altitude bound */
/*        for atmospheric model in km.", and index 6, "Low altitude */
/*        bound for atmospheric model in km." were switched. */

/* -    SPICELIB Version 1.0.0, 15-SEP-2014 (EDW) */

/*        Based on routine SGP4, 28-JUN-2005, Vallado 2006 [4]. */

/* -& */
/* $ Index_Entries */

/*     SGP4 */

/* -& */

/*     Local Variables */


/*     DS values */


/*     SPICELIB routines. */

    /* Parameter adjustments */
    if (geophs) {
	}
    if (elems) {
	}
    if (state) {
	}

    /* Function Body */
    switch(n__) {
	case 1: goto L_xxsgp4i;
	case 2: goto L_xxsgp4e;
	}

    chkin_("ZZSGP4", (ftnlen)6);
    setmsg_("The routine ZZSGP4 is an umbrella for the SGP4 initializer and "
	    "propagator entry points. Do not call ZZSGP4. It is likely that a"
	    " programming error has been made.", (ftnlen)160);
    sigerr_("SPICE(BOGUSENTRY)", (ftnlen)17);
    chkout_("ZZSGP4", (ftnlen)6);
    return 0;
/* $Procedure XXSGP4I ( SGP4 initializer ) */

L_xxsgp4i:
/* $ Abstract */

/*     This subroutine initializes variables for SGP4. */

/* $ Disclaimer */

/*     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE */
/*     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S. */
/*     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE */
/*     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE */
/*     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS" */
/*     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY */
/*     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A */
/*     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC */
/*     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE */
/*     SOFTWARE AND RELATED MATERIALS, HOWEVER USED. */

/*     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA */
/*     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT */
/*     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND, */
/*     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS, */
/*     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE */
/*     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY. */

/*     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF */
/*     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY */
/*     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE */
/*     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE. */

/* $ Required_Reading */

/*     None. */

/* $ Keywords */

/*     None. */

/* $ Declarations */

/*     DOUBLE PRECISION         GEOPHS    ( 8 ) */
/*     DOUBLE PRECISION         ELEMS     ( 10 ) */
/*     INTEGER                  OPMODE */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     GEOPHS     I   Geophysical constants. */
/*     ELEMS      I   Two-line element data. */
/*     OPMODE     I   Flag to indicate operation mode for GMST. */

/* $ Detailed_Input */

/*     GEOPHS   is a collection of 8 geophysical constants needed */
/*              for computing a state. The order of these */
/*              constants must be: */

/*              GEOPHS(1) = J2 gravitational harmonic for earth */
/*              GEOPHS(2) = J3 gravitational harmonic for earth */
/*              GEOPHS(3) = J4 gravitational harmonic for earth */

/*              These first three constants are dimensionless. */

/*              GEOPHS(4) = KE: Square root of the GM for earth where */
/*                          GM is expressed in earth radii cubed per */
/*                          minutes squared. */

/*              GEOPHS(5) = QO: High altitude bound for atmospheric */
/*                          model in km. */

/*              GEOPHS(6) = SO: Low altitude bound for atmospheric */
/*                          model in km. */

/*              GEOPHS(7) = RE: Equatorial radius of the earth in km. */

/*              GEOPHS(8) = AE: Distance units/earth radius */
/*                          (normally 1) */

/*              Below are currently recommended values for these */
/*              items: */

/*                J2 =    1.082616D-3 */
/*                J3 =   -2.53881D-6 */
/*                J4 =   -1.65597D-6 */

/*              The next item is the square root of GM for the */
/*              earth given in units of earth-radii**1.5/Minute */

/*                KE =    7.43669161D-2 */

/*              The next two items give the top and */
/*              bottom of the atmospheric drag model */
/*              used by the type 10 ephemeris type. */
/*              Don't adjust these unless you understand */
/*              the full implications of such changes. */

/*                QO =  120.0D0 */
/*                SO =   78.0D0 */

/*              The following is the equatorial radius */
/*              of the earth as used by NORAD in km. */

/*                ER = 6378.135D0 */

/*              The value of AE is the number of */
/*              distance units per earth radii used by */
/*              the NORAD state propagation software. */
/*              The value should be 1 unless you've got */
/*              a very good understanding of the NORAD */
/*              routine SGP4 and the affect of changing */
/*              this value.. */

/*                AE =    1.0D0 */

/*     ELEMS    is an array containing two-line element data */
/*              as prescribed below. The elements XNDD6O and BSTAR */
/*              must already be scaled by the proper exponent stored */
/*              in the two line elements set. Moreover, the */
/*              various items must be converted to the units shown */
/*              here. */

/*                 ELEMS (  1 ) = XNDT2O in radians/minute**2 */
/*                 ELEMS (  2 ) = XNDD6O in radians/minute**3 */
/*                 ELEMS (  3 ) = BSTAR */
/*                 ELEMS (  4 ) = XINCL  in radians */
/*                 ELEMS (  5 ) = XNODEO in radians */
/*                 ELEMS (  6 ) = EO */
/*                 ELEMS (  7 ) = OMEGAO in radians */
/*                 ELEMS (  8 ) = XMO    in radians */
/*                 ELEMS (  9 ) = XNO    in radians/minute */
/*                 ELEMS ( 10 ) = EPOCH of the elements in seconds */
/*                                past ephemeris epoch J2000. */

/*     OPMODE   Flag indicating which technique */
/*              to use to calculate sidereal time. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If radius of perigee has value less-than 1, the error */
/*         SPICE(SUBORBITAL) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine is based on the SGP4INIT code by David Vallado */
/*     corresponding to "Revisiting Spacetrack Report #3". */
/*     The intent is to maintain the original Vallado algorithm, */
/*     changing code only to meet NAIF format standards and to */
/*     integrate with SPICELIB. */

/*        1) Implemented error checks using SPICE error subsystem. */
/*           On detecting an error, control returns to the calling */
/*           routine. This behavior differs from the original */
/*           version. */

/*        2) Comments prefixed with "SGP4FIX" indicate a comment */
/*           from the code by Vallado et. al concerning a correction */
/*           to the STR#3 code. */

/*        3) Eliminated the use of COMMON blocks. */

/*        Removed getgravconst call, replaced with GEOPHS array. */

/*        xBStar, */
/*        xEcco, */
/*        Epoch, */
/*        xArgpo, */
/*        xInclo, */
/*        xMo, */
/*        xNo, */
/*        xnodeo replaced with ELEMS array. */
/*        radiusearthkm replaced with ER */

/* $ Examples */

/*     Refer to $Examples section in ZZSGP4. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     [1] Hoots, F. R., and Roehrich, R. L. 1980. "Models for */
/*         Propagation of the NORAD Element Sets." Spacetrack Report #3. */
/*         U.S. Air Force: Aerospace Defense Command. */

/*     [2] Hoots, Felix R. "Spacetrack Report #6: Models for Propagation */
/*         of Space Command Element Sets." Space Command, */
/*         U. S. Air Force, CO. */

/*     [3] Hoots, Felix R., P. W. Schumacher, and R. A. Glover. 2004. */
/*         History of Analytical Orbit Modeling in the U. S. Space */
/*         Surveillance System. Journal of Guidance, Control, and */
/*         Dynamics. 27(2):174-185. */

/*     [4] Vallado, David, Crawford, Paul, Hujsak, Richard, */
/*         and Kelso, T.S. 2006. Revisiting Spacetrack Report #3. Paper */
/*         AIAA 2006-6753 presented at the AIAA/AAS Astrodynamics */
/*         Specialist Conference, August 21-24, 2006. Keystone, CO. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 30-MAY-2021 (EDW) (JDR) */

/*        Correction of documentation error in listing of GEOPHS */
/*        constants. The definition of index 5, "High altitude bound */
/*        for atmospheric model in km.", and index 6, "Low altitude */
/*        bound for atmospheric model in km." were switched. */

/* -    SPICELIB Version 1.0.0, 11-DEC-2014 (EDW) */

/*        Based on routine SGP4INIT, 28-JUN-2005, Vallado 2006 [4]. */

/* -& */
/* $ Index_Entries */

/*     SGP4 */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("XXSGP4I", (ftnlen)7);

/*     Initialize. */

    dodeep = FALSE_;
    dosimp = FALSE_;
    svmode = *opmode;

/*     This code block replaces the call: */

/*     sgp4fix - note the following variables are also passed directly */
/*     via sgp4 common. It is possible to streamline the XXSGP4I call */
/*     by deleting the "x" variables, but the user would need to set */
/*     the common values first. we include the additional assignment */
/*     in case twoline2rv is not used. */

/*        bstar  = xbstar */
/*        ecco   = xecco */
/*        argpo  = xargpo */
/*        inclo  = xinclo */
/*        mo     = xmo */
/*        no     = xno */
/*        nodeo  = xnodeo */
    bstar = elems[2];
    inclo = elems[3];
    nodeo = elems[4];
    ecco = elems[5];
    argpo = elems[6];
    mo = elems[7];
    no = elems[8];

/*       Remember that sgp4 uses units of days from 0 jan 1950 */
/*       (sgp4epoch) and minutes from the epoch (time) */

/*       2433281.5 JD TDB = 1949-12-31 00:00:00.000000 TDB */
/*       2400000.5 JD TDB = 1858-11-17 00:00:00.000000 TDB */

/*       2433281.5 - 2400000.5 = 33281.0 */


/*     Convert the J2000 TDB representation of the epoch to */
/*     JD UTC then calculate the offset from the JD 2433281.5 UTC */
/*     reference. */

    tvec[0] = elems[9];
    ttrans_("TDB", "JDUTC", tvec, (ftnlen)3, (ftnlen)5);
    epoch = tvec[0] - 2433281.5;
    if (failed_()) {
	chkout_("XXSGP4I", (ftnlen)7);
	return 0;
    }

/*     This code block replaces the call: */

/*     CALL getgravconst( whichconst, tumin, */
/*     .                  mu, radiusearthkm, xke, */
/*     .                  j2, j3, j4, j3oj2 ) */

    j2 = geophs[0];
    j3 = geophs[1];
    j4 = geophs[2];
    er = geophs[6];
    xke = geophs[3];
    tumin = 1. / geophs[3];
    j3oj2 = j3 / j2;

/*     The following assignment and IF block is taken */
/*     from TWOLINE2RVSGP4. */

    d__1 = no * tumin;
    a = pow_dd(&d__1, &c_b10);
    if ((d__1 = ecco - 1., abs(d__1)) > 1e-6) {
	altp = a * (1. - ecco) - 1.;
	alta = a * (ecco + 1.) - 1.;
    } else {
	alta = 999999.9;
	d__1 = no * no;
	altp = 4. / pow_dd(&d__1, &c_b11) * 2.;
    }
    ss = 78. / er + 1.;
/* Computing 4th power */
    d__1 = 42. / er, d__1 *= d__1;
    qzms2t = d__1 * d__1;
    x2o3 = .66666666666666663;

/*     sgp4fix divisor for divide by zero check on inclination */
/*     the old check used 1.0D0 + cos(pi-1.0D-9), but then compared */
/*     it to 1.5D-12, so the threshold was changed to 1.5D-12 for */
/*     consistency. */

    temp4 = 1.5e-12;
    tzero = 0.;
    doinit = TRUE_;
    zzinil_(geophs, opmode, &ecco, &epoch, &inclo, &no, &ainv, &ao, &con41, &
	    con42, &cosio, &cosio2, &eccsq, &omeosq, &posq, &rp, &rteosq, &
	    sinio, &gsto);
    if (failed_()) {
	chkout_("XXSGP4I", (ftnlen)7);
	return 0;
    }

/*       Check RP for a reasonable value. The propagator may not */
/*       calculate correct state values for RP < 1. */

    if (rp < 1.) {
	setmsg_("TLE elements suborbital.", (ftnlen)24);
	sigerr_("SPICE(SUBORBITAL)", (ftnlen)17);
	chkout_("XXSGP4I", (ftnlen)7);
	return 0;
    }

/*       If nodeo and No are gtr 0 */

    if (omeosq >= 0. || no >= 0.) {
	dosimp = FALSE_;
	if (rp < 220. / er + 1.) {
	    dosimp = TRUE_;
	}
	sfour = ss;
	qzms24 = qzms2t;
	perige = (rp - 1.) * er;

/*           For perigees below 156 km, S and Qoms2t are altered. */

	if (perige < 156.) {
	    sfour = perige - 78.;
	    if (perige <= 98.) {
		sfour = 20.;
	    }
/* Computing 4th power */
	    d__1 = (120. - sfour) / er, d__1 *= d__1;
	    qzms24 = d__1 * d__1;
	    sfour = sfour / er + 1.;
	}
	pinvsq = 1. / posq;
	tsi = 1. / (ao - sfour);
	eta = ao * ecco * tsi;
	etasq = eta * eta;
	eeta = ecco * eta;
	psisq = (d__1 = 1. - etasq, abs(d__1));
/* Computing 4th power */
	d__1 = tsi, d__1 *= d__1;
	coef = qzms24 * (d__1 * d__1);
	coef1 = coef / pow_dd(&psisq, &c_b16);
	cc2 = coef1 * no * (ao * (etasq * 1.5 + 1. + eeta * (etasq + 4.)) + 
		j2 * .375 * tsi / psisq * con41 * (etasq * 3. * (etasq + 8.) 
		+ 8.));
	cc1 = bstar * cc2;
	cc3 = 0.;
	if (ecco > 1e-4) {
	    cc3 = coef * -2. * tsi * j3oj2 * no * sinio / ecco;
	}
	x1mth2 = 1. - cosio2;
	cc4 = no * 2. * coef1 * ao * omeosq * (eta * (etasq * .5 + 2.) + ecco 
		* (etasq * 2. + .5) - j2 * tsi / (ao * psisq) * (con41 * -3. *
		 (1. - eeta * 2. + etasq * (1.5 - eeta * .5)) + x1mth2 * .75 *
		 (etasq * 2. - eeta * (etasq + 1.)) * cos(argpo * 2.)));
	cc5 = coef1 * 2. * ao * omeosq * ((etasq + eeta) * 2.75 + 1. + eeta * 
		etasq);
	cosio4 = cosio2 * cosio2;
	temp1 = j2 * 1.5 * pinvsq * no;
	temp2 = temp1 * .5 * j2 * pinvsq;
	temp3 = j4 * -.46875 * pinvsq * pinvsq * no;
	mdot = no + temp1 * .5 * rteosq * con41 + temp2 * .0625 * rteosq * (
		13. - cosio2 * 78. + cosio4 * 137.);
	argpdot = temp1 * -.5 * con42 + temp2 * .0625 * (7. - cosio2 * 114. + 
		cosio4 * 395.) + temp3 * (3. - cosio2 * 36. + cosio4 * 49.);
	xhdot1 = -temp1 * cosio;
	nodedot = xhdot1 + (temp2 * .5 * (4. - cosio2 * 19.) + temp3 * 2. * (
		3. - cosio2 * 7.)) * cosio;
	xpidot = argpdot + nodedot;
	omgcof = bstar * cc3 * cos(argpo);
	xmcof = 0.;
	if (ecco > 1e-4) {
	    xmcof = -x2o3 * coef * bstar / eeta;
	}
	xnodcf = omeosq * 3.5 * xhdot1 * cc1;
	t2cof = cc1 * 1.5;

/*           sgp4fix for divide by zero with xinco = 180 deg. */

	if ((d__1 = cosio + 1., abs(d__1)) > 1.5e-12) {
	    xlcof = j3oj2 * -.25 * sinio * (cosio * 5. + 3.) / (cosio + 1.);
	} else {
	    xlcof = j3oj2 * -.25 * sinio * (cosio * 5. + 3.) / temp4;
	}
	aycof = j3oj2 * -.5 * sinio;
/* Computing 3rd power */
	d__1 = eta * cos(mo) + 1.;
	delmo = d__1 * (d__1 * d__1);
	sinmao = sin(mo);
	x7thm1 = cosio2 * 7. - 1.;

/*           Deep Space Initialization */

	if (twopi_() / no >= 225.) {
	    dodeep = TRUE_;
	    dosimp = TRUE_;
	    tc = 0.;
	    inclm = inclo;

/*               Common. */

	    zzdscm_(&epoch, &ecco, &argpo, &tc, &inclo, &nodeo, &no, &snodm, &
		    cnodm, &sinim, &cosim, &sinomm, &cosomm, &day, &e3, &ee2, 
		    &eccm, &emsq, &gam, &peo, &pgho, &pho, &pinco, &plo, &
		    rtemsq, &se2, &se3, &sgh2, &sgh3, &sgh4, &sh2, &sh3, &si2,
		     &si3, &sl2, &sl3, &sl4, &s1, &s2, &s3, &s4, &s5, &s6, &
		    s7, &ss1, &ss2, &ss3, &ss4, &ss5, &ss6, &ss7, &sz1, &sz2, 
		    &sz3, &sz11, &sz12, &sz13, &sz21, &sz22, &sz23, &sz31, &
		    sz32, &sz33, &xgh2, &xgh3, &xgh4, &xh2, &xh3, &xi2, &xi3, 
		    &xl2, &xl3, &xl4, &xn, &z1, &z2, &z3, &z11, &z12, &z13, &
		    z21, &z22, &z23, &z31, &z32, &z33, &zmol, &zmos);

/*               Long period perturbations. */

	    zzdspr_(opmode, &e3, &ee2, &peo, &pgho, &pho, &pinco, &plo, &se2, 
		    &se3, &sgh2, &sgh3, &sgh4, &sh2, &sh3, &si2, &si3, &sl2, &
		    sl3, &sl4, &tzero, &xgh2, &xgh3, &xgh4, &xh2, &xh3, &xi2, 
		    &xi3, &xl2, &xl3, &xl4, &zmol, &zmos, &inclm, &doinit, &
		    ecco, &inclo, &nodeo, &argpo, &mo);
	    argpm = 0.;
	    nodem = 0.;
	    mm = 0.;

/*               Initialization */

	    zzdsin_(geophs, &cosim, &emsq, &argpo, &s1, &s2, &s3, &s4, &s5, &
		    sinim, &ss1, &ss2, &ss3, &ss4, &ss5, &sz1, &sz3, &sz11, &
		    sz13, &sz21, &sz23, &sz31, &sz33, &tzero, &tc, &gsto, &mo,
		     &mdot, &no, &nodeo, &nodedot, &xpidot, &z1, &z3, &z11, &
		    z13, &z21, &z23, &z31, &z33, &ecco, &eccsq, &eccm, &argpm,
		     &inclm, &mm, &xn, &nodem, &irez, &atime, &d2201, &d2211, 
		    &d3210, &d3222, &d4410, &d4422, &d5220, &d5232, &d5421, &
		    d5433, &dedt, &didt, &dmdt, &dndt, &dnodt, &domdt, &del1, 
		    &del2, &del3, &xfact, &xlamo, &xli, &xni);
	}

/*           Set variables if not deep space or rp < 220 */

	if (! dosimp) {
	    cc1sq = cc1 * cc1;
	    d2 = ao * 4. * tsi * cc1sq;
	    temp = d2 * tsi * cc1 / 3.;
	    d3 = (ao * 17. + sfour) * temp;
	    d4 = temp * .5 * ao * tsi * (ao * 221. + sfour * 31.) * cc1;
	    t3cof = d2 + cc1sq * 2.;
	    t4cof = (d3 * 3. + cc1 * (d2 * 12. + cc1sq * 10.)) * .25;
	    t5cof = (d4 * 3. + cc1 * 12. * d3 + d2 * 6. * d2 + cc1sq * 15. * (
		    d2 * 2. + cc1sq)) * .2;
	}
    }
    doinit = FALSE_;
    chkout_("XXSGP4I", (ftnlen)7);
    return 0;
/* $Procedure XXSGP4E ( SGP4 evaluator ) */

L_xxsgp4e:
/* $ Abstract */

/*     This procedure is the SGP4 prediction model from Space Command. */
/*     This is an updated and combined version of SGP4 and SDP4 */
/*     originally published separately in Spacetrack report #3 [1]. */
/*     This version follows the methodology from the 2006 AIAA */
/*     Vallado paper [4] describing the history and development of */
/*     the code. */

/* $ Disclaimer */

/*     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE */
/*     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S. */
/*     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE */
/*     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE */
/*     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS" */
/*     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY */
/*     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A */
/*     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC */
/*     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE */
/*     SOFTWARE AND RELATED MATERIALS, HOWEVER USED. */

/*     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA */
/*     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT */
/*     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND, */
/*     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS, */
/*     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE */
/*     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY. */

/*     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF */
/*     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY */
/*     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE */
/*     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE. */

/* $ Required_Reading */

/*     None. */

/* $ Keywords */

/*     TLE */
/*     TWO LINE ELEMENTS */

/* $ Declarations */

/*     DOUBLE PRECISION         T */
/*     DOUBLE PRECISION         STATE     ( 6 ) */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     T          I   Time to evaluate state in minutes from epoch. */
/*     STATE      O   Evaluated state. */

/* $ Detailed_Input */

/*     T        is the time in minutes from the elements epoch */
/*              at which to calculate a state from the */
/*              TLE set. */

/* $ Detailed_Output */

/*     STATE    is the state produced by evaluating the input elements */
/*              at the input epoch T. Units are km and km/sec. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the mean motion is less than 0, the error */
/*         SPICE(BADMEANMOTION) is signaled.0D0. */

/*     2)  If the mean eccentricity is not bounded by [-0, the error */
/*         SPICE(BADMECCENTRICITY) is signaled.001, 1]. */

/*     3)  If the mean semimajor axis has value less-than ,, the error */
/*         SPICE(BADMSEMIMAJOR) is signaled.95. */

/*     4)  If the perturbed eccentricity is not bounded by [0, 1], the */
/*         error SPICE(BADPECCENTRICITY) is signaled. */

/*     5)  If the semi-latus rectum has value less-than 0, the error */
/*         SPICE(BADSEMILATUS) is signaled. */

/*     6)  If the scaled orbit radial distance has value less than 1, the */
/*         error SPICE(ORBITDECAY) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine is based on the SGP4 code by David Vallado */
/*     corresponding to "Revisiting Spacetrack Report #3." */
/*     The intent is to maintain the original Vallado algorithm, */
/*     changing code only to meet NAIF format standards and to */
/*     integrate with SPICELIB. */

/*        1) Implemented error checks using SPICE error subsystem. */
/*           On detecting an error, control returns to the calling */
/*           routine. This behavior differs from the original */
/*           version. */

/*        2) Comments prefixed with "SGP4FIX" indicate a comment */
/*           from the code by Vallado et. al concerning a correction */
/*           to the STR#3 code. */

/*        3) Eliminated the use of COMMON blocks. */

/*        Removed getgravconst call, replaced with GEOPHS array. */

/*        Capitalize all variables. */

/*        radiusearthkm replaced with ER */
/*        VKmPerSec     replaced with KPS */
/*        r, v          replaced with STATE */
/*        method        replaced with DODEEP */
/*        isimp         replaced with DOSIMP */
/*        Error         eliminated */
/*        whichconst    eliminated, function provided by GEOPHS */

/* $ Examples */

/*     Refer to $Examples section in ZZSGP4. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     [1] Hoots, F. R., and Roehrich, R. L. 1980. "Models for */
/*         Propagation of the NORAD Element Sets." Spacetrack Report #3. */
/*         U.S. Air Force: Aerospace Defense Command. */

/*     [2] Hoots, Felix R. "Spacetrack Report #6: Models for Propagation */
/*         of Space Command Element Sets." Space Command, */
/*         U. S. Air Force, CO. */

/*     [3] Hoots, Felix R., P. W. Schumacher, and R. A. Glover. 2004. */
/*         History of Analytical Orbit Modeling in the U. S. Space */
/*         Surveillance System. Journal of Guidance, Control, and */
/*         Dynamics. 27(2):174-185. */

/*     [4] Vallado, David, Crawford, Paul, Hujsak, Richard, */
/*         and Kelso, T.S. 2006. Revisiting Spacetrack Report #3. Paper */
/*         AIAA 2006-6753 presented at the AIAA/AAS Astrodynamics */
/*         Specialist Conference, August 21-24, 2006. Keystone, CO. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     B.V. Semenov       (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 30-MAY-2021 (BVS) (JDR) */

/*        Added missing 'D' to literal DP constants. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.0, 15-SEP-2014 (EDW) */

/*        Based on routine SGP4, 28-JUN-2005 [4]. */

/* -& */
/* $ Index_Entries */

/*     SGP4 */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("XXSGP4E", (ftnlen)7);

/*     Local constants. Keep compiler ok for warnings on */
/*     uninitialized variables */

    x2o3 = .66666666666666663;
    mr = 0.;
    coseo1 = 1.;
    sineo1 = 0.;

/*     Set mathematical constants. */

/*     This code block replaces the call: */

/*     sgp4fix identify constants and allow alternate values. */

/*     CALL getgravconst( whichconst, tumin, */
/*     .                  mu, radiusearthkm, xke, */
/*     .                  j2, j3, j4, j3oj2 ) */


/*     sgp4fix divisor for divide by zero check on inclination */
/*     the old check used 1.0D0 + cos(pi-1.0D-9), but then compared it to */
/*     1.5D-12, so the threshold was changed to 1.5D-12 for consistency. */

    temp4 = 1.5e-12;
    kps = er * xke / 60.;

/*     UPDATE FOR SECULAR GRAVITY AND ATMOSPHERIC DRAG */

    xmdf = mo + mdot * *t;
    omgadf = argpo + argpdot * *t;
    xnoddf = nodeo + nodedot * *t;
    argpm = omgadf;
    mm = xmdf;
    t2 = *t * *t;
    nodem = xnoddf + xnodcf * t2;
    tempa = 1. - cc1 * *t;
    tempe = bstar * cc4 * *t;
    templ = t2cof * t2;
    if (! dosimp) {
	delomg = omgcof * *t;
/* Computing 3rd power */
	d__1 = eta * cos(xmdf) + 1.;
	delm = xmcof * (d__1 * (d__1 * d__1) - delmo);
	temp = delomg + delm;
	mm = xmdf + temp;
	argpm = omgadf - temp;
	t3 = t2 * *t;
	t4 = t3 * *t;
	tempa = tempa - d2 * t2 - d3 * t3 - d4 * t4;
	tempe += bstar * cc5 * (sin(mm) - sinmao);
	templ = templ + t3cof * t3 + t4 * (t4cof + *t * t5cof);
    }
    xn = no;
    eccm = ecco;
    inclm = inclo;
    if (dodeep) {
	tc = *t;
	zzdspc_(&irez, &d2201, &d2211, &d3210, &d3222, &d4410, &d4422, &d5220,
		 &d5232, &d5421, &d5433, &dedt, &del1, &del2, &del3, &didt, &
		dmdt, &dnodt, &domdt, &argpo, &argpdot, t, &tc, &gsto, &xfact,
		 &xlamo, &no, &atime, &eccm, &argpm, &inclm, &xli, &mm, &xni, 
		&nodem, &dndt, &xn);
    }

/*     Mean motion less than 0.0. */

    if (xn <= 0.) {
	setmsg_("Mean motion less-than zero. This error may indicate a bad T"
		"LE set.", (ftnlen)66);
	sigerr_("SPICE(BADMEANMOTION)", (ftnlen)20);
	chkout_("XXSGP4E", (ftnlen)7);
	return 0;
    }
    d__1 = xke / xn;
/* Computing 2nd power */
    d__2 = tempa;
    am = pow_dd(&d__1, &x2o3) * (d__2 * d__2);
    xn = xke / pow_dd(&am, &c_b22);
    eccm -= tempe;

/*     Fix tolerance for error recognition. Vallado code used */
/*     a lower limit of -0.001. This value apparently prevents */
/*     an error signal due to roundoff error. */

    if (eccm >= 1. || eccm < -.001) {
	setmsg_("Mean eccentricity value, #, beyond allowed bounds [-0.001,1"
		".0). This error may indicate a bad TLE set.", (ftnlen)102);
	errdp_("#", &eccm, (ftnlen)1);
	sigerr_("SPICE(BADMECCENTRICITY)", (ftnlen)23);
	chkout_("XXSGP4E", (ftnlen)7);
	return 0;
    }
    if (am < .95) {
	setmsg_("Mean semi-major axis value, #, below allowed minimum of 0.9"
		"5. This error may indicate a bad TLE set or a decayed orbit.",
		 (ftnlen)119);
	errdp_("#", &eccm, (ftnlen)1);
	sigerr_("SPICE(BADMSEMIMAJOR)", (ftnlen)20);
	chkout_("XXSGP4E", (ftnlen)7);
	return 0;
    }

/*     sgp4fix change test condition for eccentricity */

    if (eccm < 1e-6) {
	eccm = 1e-6;
    }
    mm += no * templ;
    xlm = mm + argpm + nodem;
    emsq = eccm * eccm;
    temp = 1. - emsq;
    d__1 = twopi_();
    nodem = d_mod(&nodem, &d__1);
    d__1 = twopi_();
    argpm = d_mod(&argpm, &d__1);
    d__1 = twopi_();
    xlm = d_mod(&xlm, &d__1);
    d__1 = xlm - argpm - nodem;
    d__2 = twopi_();
    mm = d_mod(&d__1, &d__2);

/*     Compute extra mean quantities */

    sinim = sin(inclm);
    cosim = cos(inclm);

/*     Add lunar-solar periodics */

    eccp = eccm;
    xincp = inclm;
    argpp = argpm;
    nodep = nodem;
    mp = mm;
    sinip = sinim;
    cosip = cosim;

/*     Use deep space perturbation if indicated. */

    if (dodeep) {
	zzdspr_(&svmode, &e3, &ee2, &peo, &pgho, &pho, &pinco, &plo, &se2, &
		se3, &sgh2, &sgh3, &sgh4, &sh2, &sh3, &si2, &si3, &sl2, &sl3, 
		&sl4, t, &xgh2, &xgh3, &xgh4, &xh2, &xh3, &xi2, &xi3, &xl2, &
		xl3, &xl4, &zmol, &zmos, &inclo, &c_false, &eccp, &xincp, &
		nodep, &argpp, &mp);
	if (xincp < 0.) {
	    xincp = -xincp;
	    nodep += pi_();
	    argpp -= pi_();
	}
	if (eccp < 0. || eccp > 1.) {
	    setmsg_("Perturbed eccentricity value, #, beyond allowed bounds "
		    "[0,1]. This error may indicate a bad TLE set.", (ftnlen)
		    100);
	    errdp_("#", &eccp, (ftnlen)1);
	    sigerr_("SPICE(BADPECCENTRICITY)", (ftnlen)23);
	    chkout_("XXSGP4E", (ftnlen)7);
	    return 0;
	}
    }

/*     Update for long period periodics if a deep space trajectory. */

    if (dodeep) {
	sinip = sin(xincp);
	cosip = cos(xincp);
	aycof = j3oj2 * -.5 * sinip;

/*         sgp4fix for divide by zero with xincp = 180 deg */

	if ((d__1 = cosip + 1., abs(d__1)) > 1.5e-12) {
	    xlcof = j3oj2 * -.25 * sinip * (cosip * 5. + 3.) / (cosip + 1.);
	} else {
	    xlcof = j3oj2 * -.25 * sinip * (cosip * 5. + 3.) / temp4;
	}
    }
    axnl = eccp * cos(argpp);
    temp = 1. / (am * (1. - eccp * eccp));
    aynl = eccp * sin(argpp) + temp * aycof;
    xl = mp + argpp + nodep + temp * xlcof * axnl;

/*     Solve Kepler's equation. */

    d__1 = xl - nodep;
    d__2 = twopi_();
    u = d_mod(&d__1, &d__2);
    eo1 = u;
    iter = 0;

/*     sgp4fix for Kepler iteration the following iteration needs */
/*     better limits on corrections */

    temp = 9999.9;
    while(temp >= 1e-12 && iter < 10) {
	++iter;
	sineo1 = sin(eo1);
	coseo1 = cos(eo1);
	tem5 = 1. - coseo1 * axnl - sineo1 * aynl;
	tem5 = (u - aynl * coseo1 + axnl * sineo1 - eo1) / tem5;
	temp = abs(tem5);

/*        Stop excessive correction. */

	if (temp > 1.) {
	    tem5 /= temp;
	}
	eo1 += tem5;
    }

/*     Short period preliminary quantities. */

    ecose = axnl * coseo1 + aynl * sineo1;
    esine = axnl * sineo1 - aynl * coseo1;
    el2 = axnl * axnl + aynl * aynl;
    pl = am * (1. - el2);

/*     Error check for semi-latus rectum < 0.0 */

    if (pl < 0.) {
	setmsg_("Semi-latus rectum less-than zero.", (ftnlen)33);
	sigerr_("SPICE(BADSEMILATUS)", (ftnlen)19);
	chkout_("XXSGP4E", (ftnlen)7);
	return 0;
    }
    rl = am * (1. - ecose);
    rdotl = sqrt(am) * esine / rl;
    rvdotl = sqrt(pl) / rl;
    betal = sqrt(1. - el2);
    temp = esine / (betal + 1.);
    sinu = am / rl * (sineo1 - aynl - axnl * temp);
    cosu = am / rl * (coseo1 - axnl + aynl * temp);
    su = atan2(sinu, cosu);
    sin2u = (cosu + cosu) * sinu;
    cos2u = 1. - sinu * 2. * sinu;
    temp = 1. / pl;
    temp1 = j2 * .5 * temp;
    temp2 = temp1 * temp;

/*     Update for short period periodics if a deep space trajectory. */

    if (dodeep) {
	cosisq = cosip * cosip;
	con41 = cosisq * 3. - 1.;
	x1mth2 = 1. - cosisq;
	x7thm1 = cosisq * 7. - 1.;
    }
    mr = rl * (1. - temp2 * 1.5 * betal * con41) + temp1 * .5 * x1mth2 * 
	    cos2u;
    su -= temp2 * .25 * x7thm1 * sin2u;
    xnode = nodep + temp2 * 1.5 * cosip * sin2u;
    xinc = xincp + temp2 * 1.5 * cosip * sinip * cos2u;
    mv = rdotl - xn * temp1 * x1mth2 * sin2u / xke;
    rvdot = rvdotl + xn * temp1 * (x1mth2 * cos2u + con41 * 1.5) / xke;

/*     Orientation vectors. */

    sinsu = sin(su);
    cossu = cos(su);
    snod = sin(xnode);
    cnod = cos(xnode);
    sini = sin(xinc);
    cosi = cos(xinc);
    xmx = -snod * cosi;
    xmy = cnod * cosi;
    ux = xmx * sinsu + cnod * cossu;
    uy = xmy * sinsu + snod * cossu;
    uz = sini * sinsu;
    vx = xmx * cossu - cnod * sinsu;
    vy = xmy * cossu - snod * sinsu;
    vz = sini * cossu;

/*     Position and velocity. */

    state[0] = mr * ux * er;
    state[1] = mr * uy * er;
    state[2] = mr * uz * er;
    state[3] = (mv * ux + rvdot * vx) * kps;
    state[4] = (mv * uy + rvdot * vy) * kps;
    state[5] = (mv * uz + rvdot * vz) * kps;

/*     sgp4fix for decaying satellites */

/*     Place this test here to ensure evaluation of STATE. */
/*     The result may be physically invalid. */

    if (mr < 1.) {
	setmsg_("Satellite has decayed.", (ftnlen)22);
	sigerr_("SPICE(ORBITDECAY)", (ftnlen)17);
	chkout_("XXSGP4E", (ftnlen)7);
	return 0;
    }
    chkout_("XXSGP4E", (ftnlen)7);
    return 0;
} /* zzsgp4_ */

/* Subroutine */ int zzsgp4_(doublereal *geophs, doublereal *elems, integer *
	opmode, doublereal *t, doublereal *state)
{
    return zzsgp4_0_(0, geophs, elems, opmode, t, state);
    }

/* Subroutine */ int xxsgp4i_(doublereal *geophs, doublereal *elems, integer *
	opmode)
{
    return zzsgp4_0_(1, geophs, elems, opmode, (doublereal *)0, (doublereal *)
	    0);
    }

/* Subroutine */ int xxsgp4e_(doublereal *t, doublereal *state)
{
    return zzsgp4_0_(2, (doublereal *)0, (doublereal *)0, (integer *)0, t, 
	    state);
    }

