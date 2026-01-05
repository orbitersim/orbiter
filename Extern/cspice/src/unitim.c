/* unitim.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__5 = 5;
static integer c__4 = 4;
static integer c__9 = 9;
static integer c__1 = 1;
static integer c__2 = 2;
static integer c__7 = 7;

/* $Procedure UNITIM ( Uniform time scale transformation ) */
doublereal unitim_(doublereal *epoch, char *insys, char *outsys, ftnlen 
	insys_len, ftnlen outsys_len)
{
    /* Initialized data */

    static logical first = TRUE_;
    static char missed[20*4] = "DELTET/DELTA_T_A, # " "DELTET/K, #         " 
	    "DELTET/EB, #        " "DELTET/M, #         ";
    static logical nodata = TRUE_;
    static char vars__[16*4] = "DELTET/DELTA_T_A" "DELTET/K        " "DELTET"
	    "/EB       " "DELTET/M        ";

    /* System generated locals */
    address a__1[7];
    integer i__1[7], i__2;
    doublereal ret_val;
    char ch__1[466];

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen), s_cat(char *,
	     char **, integer *, integer *, ftnlen);
    integer s_rnge(char *, integer, char *, integer), s_cmp(char *, char *, 
	    ftnlen, ftnlen);
    double sin(doublereal);

    /* Local variables */
    extern logical setc_(char *, char *, char *, ftnlen, ftnlen, ftnlen);
    char myin[8];
    extern /* Subroutine */ int zzcvpool_(char *, integer *, logical *, 
	    ftnlen), zzctruin_(integer *);
    integer i__;
    static doublereal k, m[2];
    integer n;
    extern logical elemc_(char *, char *, ftnlen, ftnlen);
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    static char recog[8*15];
    logical intdb;
    extern /* Subroutine */ int ucase_(char *, char *, ftnlen, ftnlen), 
	    errch_(char *, char *, ftnlen, ftnlen);
    logical found[4], intdt;
    char types[8*8], myout[8];
    static doublereal eb;
    extern logical failed_(void);
    extern /* Subroutine */ int validc_(integer *, integer *, char *, ftnlen);
    static char bslash[1];
    static doublereal secspd;
    logical update;
    extern /* Subroutine */ int gdpool_(char *, integer *, integer *, integer 
	    *, doublereal *, logical *, ftnlen), unionc_(char *, char *, char 
	    *, ftnlen, ftnlen, ftnlen), sigerr_(char *, ftnlen), chkout_(char 
	    *, ftnlen), ssizec_(integer *, char *, ftnlen);
    logical outtdb;
    extern /* Subroutine */ int setmsg_(char *, ftnlen);
    extern logical somfls_(logical *, integer *);
    doublereal mytime;
    static char typtdb[8*10];
    extern /* Subroutine */ int insrtc_(char *, char *, ftnlen, ftnlen);
    extern logical return_(void);
    static integer usrctr[2];
    logical outtdt;
    extern /* Subroutine */ int swpool_(char *, integer *, char *, ftnlen, 
	    ftnlen);
    static char typtdt[8*11];
    extern doublereal j2000_(void);
    static doublereal dta;
    doublereal tdb;
    extern doublereal spd_(void);
    doublereal tdt;
    static doublereal jd2000;

/* $ Abstract */

/*     Transform time from one uniform scale to another. The uniform */
/*     time scales are TAI, GPS, TT, TDT, TDB, ET, JED, JDTDB, JDTDT. */

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

/*     TIME */

/* $ Keywords */

/*     CONVERSION */
/*     TIME */
/*     UTILITY */

/* $ Declarations */
/* $ Abstract */

/*     This include file defines the dimension of the counter */
/*     array used by various SPICE subsystems to uniquely identify */
/*     changes in their states. */

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

/* $ Parameters */

/*     CTRSIZ      is the dimension of the counter array used by */
/*                 various SPICE subsystems to uniquely identify */
/*                 changes in their states. */

/* $ Author_and_Institution */

/*     B.V. Semenov    (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 29-JUL-2013 (BVS) */

/* -& */

/*     End of include file. */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     EPOCH      I   An epoch. */
/*     INSYS      I   The time scale associated with the input EPOCH. */
/*     OUTSYS     I   The time scale associated with the function value. */

/*     The function returns the d.p. in OUTSYS that is equivalent to the */
/*     EPOCH on the INSYS time scale. */

/* $ Detailed_Input */

/*     EPOCH    is an epoch relative to the INSYS time scale. */

/*     INSYS    is a time scale. Acceptable values are: */

/*                 'TAI'     International Atomic Time. */
/*                 'TDB'     Barycentric Dynamical Time. */
/*                 'TDT'     Terrestrial Dynamical Time. */
/*                 'TT'      Terrestrial Time, identical to TDT. */
/*                 'ET'      Ephemeris time (in the SPICE system, this is */
/*                           equivalent to TDB). */
/*                 'JDTDB'   Julian Date relative to TDB. */
/*                 'JDTDT'   Julian Date relative to TDT. */
/*                 'JED'     Julian Ephemeris date (in the SPICE system */
/*                           this is equivalent to JDTDB). */
/*                 'GPS'     Global Positioning System Time. */

/*              The routine is not sensitive to the case of the */
/*              characters in INSYS; 'tai' 'Tai' and 'TAI' are all */
/*              equivalent from the point of view of this routine. */

/*     OUTSYS   is the time scale to which EPOCH should be converted. */
/*              Acceptable values are the same as for INSYS. The */
/*              routine is not sensitive to the case of OUTSYS. */

/* $ Detailed_Output */

/*     The function returns the time in the system specified by OUTSYS */
/*     that is equivalent to the EPOCH in the INSYS time scale. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  The kernel pool must contain the variables: */

/*            'DELTET/DELTA_T_A' */
/*            'DELTET/K' */
/*            'DELTET/EB' */
/*            'DELTET/M' */

/*         If these are not present, the error SPICE(MISSINGTIMEINFO) is */
/*         signaled. (These variables are typically inserted into the */
/*         kernel pool by loading a leapseconds kernel with the SPICE */
/*         routine FURNSH.) */

/*     2)  If the names of either the input or output time types are */
/*         unrecognized, the error SPICE(BADTIMETYPE) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     We use the term uniform time scale to refer to those */
/*     representations of time that are numeric (each epoch is */
/*     represented by a number) and additive. A numeric time system is */
/*     additive if given the representations, E1 and E2, of any pair of */
/*     successive epochs, the time elapsed between the epochs is given by */
/*     E2 - E1. */

/*     Given an epoch in one of the uniform time scales specified by */
/*     INSYS, the function returns the equivalent representation in the */
/*     scale specified by OUTSYS. A list of the recognized uniform time */
/*     scales is given in the detailed input for INSYS. */

/* $ Examples */

/*     To convert an epoch with respect to the International Atomic */
/*     Time (TAI) scale to ET (Barycentric Dynamical Time), make the */
/*     following assignment. */

/*           ET = UNITIM ( TAI, 'TAI', 'ET' ) */

/* $ Restrictions */

/*     1)  The appropriate variable must be loaded into the SPICE kernel */
/*         pool (normally by loading a leapseconds kernel with FURNSH) */
/*         prior to calling this routine. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     H.A. Neilan        (JPL) */
/*     B.V. Semenov       (JPL) */
/*     W.L. Taber         (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.5.0, 05-SEP-2020 (EDW) (JDR) */

/*        Added time system name 'TT' (Terrestrial Time) as alternate */
/*        assignment of 'TDT' (Terrestrial Dynamical Time). */

/*        Included GPS time system mapping. */

/*        Edited the header to comply with NAIF standard. */

/*        Removed references to FURNSH, CLPOOL, KCLEAR, UNLOAD, and */
/*        Required Reading documents and tutorials from the "variables */
/*        not present" long error message. */

/* -    SPICELIB Version 1.4.0, 09-SEP-2013 (BVS) */

/*        Updated to keep track of the POOL counter and call ZZCVPOOL. */

/* -    SPICELIB Version 1.3.0, 05-MAR-2009 (NJB) */

/*        This routine now keeps track of whether its kernel pool */
/*        look-up failed. If so, a kernel pool lookup is attempted on */
/*        the next call to this routine. This change is an enhancement, */
/*        not a bug fix (unlike similar modifications in SCLK routines). */

/* -    SPICELIB Version 1.2.1, 15-NOV-2006 (EDW) (NJB) */

/*        Replaced references to LDPOOL with references to FURNSH. */
/*        Replaced references to RTPOOL with references to GDPOOL. */
/*        Enhanced long error message associated with missing kernel */
/*        variables. */

/* -    SPICELIB Version 1.2.0, 17-FEB-1999 (WLT) */

/*        Added a second call to SWPOOL in the event some required */
/*        kernel pool variable is not supplied. */

/* -    SPICELIB Version 1.1.0, 17-MAY-1994 (HAN) */

/*        If the value of the function RETURN is .TRUE. upon execution of */
/*        this module, this function is assigned a default value of */
/*        either 0, 0.0D0, .FALSE., or blank depending on the type of */
/*        the function. */

/* -    SPICELIB Version 1.0.0, 28-MAR-1992 (WLT) */

/* -& */
/* $ Index_Entries */

/*     Transform between two uniform numeric time systems */
/*     Transform between two additive numeric time systems */
/*     Convert one uniform numeric time system to another */
/*     Convert one additive numeric time system to another */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     LBCELL is the bottom slot of a cell. */


/*     NEEDED is the number of kernel pool variables needed by this */
/*     routine. */


/*     LNGVAR is the length of the longest kernel pool variable name */
/*     that is used by this routine. */


/*     MISLEN is the length required by the MISSED array of strings */
/*     used for error messages. */


/*     TYPLEN is the maximum length allowed for names of uniform */
/*     time types. */


/*     NTDT is the number of time types based on terrestrial dynamical */
/*     time (TDT). */


/*     NTDB is the number of time types base on barycentric dynamical */
/*     time (TDB). */


/*     NRECOG is the total number of recognized types. */


/*     Constant shift between GPS time system and TAI time system. */


/*     Local variables */


/*     Saved variables */


/*     Initial values */


/*     Standard SPICE error handling. */

    if (return_()) {
	ret_val = 0.;
	return ret_val;
    }
    chkin_("UNITIM", (ftnlen)6);
    if (first) {
	first = FALSE_;

/*        Initialize the backslash character.  We use this for error */
/*        message construction. */

	*(unsigned char *)bslash = '\\';

/*        Set up the parameters that we are going to need often. */

	secspd = spd_();
	jd2000 = j2000_();

/*        Initialize the sets that we will use. */

	s_copy(typtdt + 48, "JDTDT", (ftnlen)8, (ftnlen)5);
	s_copy(typtdt + 56, "TAI", (ftnlen)8, (ftnlen)3);
	s_copy(typtdt + 64, "TDT", (ftnlen)8, (ftnlen)3);
	s_copy(typtdt + 72, "GPS", (ftnlen)8, (ftnlen)3);
	s_copy(typtdt + 80, "TT", (ftnlen)8, (ftnlen)2);
	s_copy(typtdb + 48, "ET", (ftnlen)8, (ftnlen)2);
	s_copy(typtdb + 56, "JDTDB", (ftnlen)8, (ftnlen)5);
	s_copy(typtdb + 64, "JED", (ftnlen)8, (ftnlen)3);
	s_copy(typtdb + 72, "TDB", (ftnlen)8, (ftnlen)3);
	validc_(&c__5, &c__5, typtdt, (ftnlen)8);
	validc_(&c__4, &c__4, typtdb, (ftnlen)8);
	ssizec_(&c__9, recog, (ftnlen)8);
	unionc_(typtdt, typtdb, recog, (ftnlen)8, (ftnlen)8, (ftnlen)8);

/*        Initialize the local POOL counter to user value. */

	zzctruin_(usrctr);

/*        Set up the kernel pool watchers */

	swpool_("UNITIM", &c__4, vars__, (ftnlen)6, (ftnlen)16);
    }

/*     Check to see if any of the kernel items required by this */
/*     routine have been updated since the last call to this */
/*     entry point. */

    zzcvpool_("UNITIM", usrctr, &update, (ftnlen)6);
    if (update || nodata) {

/*        Fetch all of the time parameters from the kernel pool. */

	gdpool_("DELTET/DELTA_T_A", &c__1, &c__1, &n, &dta, found, (ftnlen)16)
		;
	gdpool_("DELTET/K", &c__1, &c__1, &n, &k, &found[1], (ftnlen)8);
	gdpool_("DELTET/EB", &c__1, &c__1, &n, &eb, &found[2], (ftnlen)9);
	gdpool_("DELTET/M", &c__1, &c__2, &n, m, &found[3], (ftnlen)8);
	if (failed_()) {
	    nodata = TRUE_;
	    ret_val = 0.;
	    chkout_("UNITIM", (ftnlen)6);
	    return ret_val;
	}

/*        If anything wasn't found, it's an error dude. */

	if (somfls_(found, &c__4)) {
	    nodata = TRUE_;

/*           If we didn't get all of the things we needed for time */
/*           conversion, we need to reset the watch.  Otherwise */
/*           subsequent calls to this routine will never have the */
/*           needed data. */

	    swpool_("UNITIM", &c__4, vars__, (ftnlen)6, (ftnlen)16);
/* Writing concatenation */
	    i__1[0] = 290, a__1[0] = "The following variables, needed to con"
		    "vert between the input uniform time scales, were not fou"
		    "nd in the kernel pool: # Your program may have failed to"
		    " load a leapseconds kernel. Other possible causes of thi"
		    "s problem include loading an invalid leapseconds kernel-"
		    "--one that lacks an initial ";
	    i__1[1] = 1, a__1[1] = bslash;
	    i__1[2] = 10, a__1[2] = "begindata ";
	    i__1[3] = 41, a__1[3] = "marker or final newline character, or i"
		    "s ";
	    i__1[4] = 42, a__1[4] = "otherwise corrupted---or deleting previ"
		    "ous";
	    i__1[5] = 42, a__1[5] = "ly loaded kernel pool variables via cal"
		    "ls ";
	    i__1[6] = 40, a__1[6] = "to routines that clear the kernel pool. "
		    ;
	    s_cat(ch__1, a__1, i__1, &c__7, (ftnlen)466);
	    setmsg_(ch__1, (ftnlen)466);
	    for (i__ = 1; i__ <= 4; ++i__) {
		if (! found[(i__2 = i__ - 1) < 4 && 0 <= i__2 ? i__2 : s_rnge(
			"found", i__2, "unitim_", (ftnlen)485)]) {
		    errch_("#", missed + ((i__2 = i__ - 1) < 4 && 0 <= i__2 ? 
			    i__2 : s_rnge("missed", i__2, "unitim_", (ftnlen)
			    486)) * 20, (ftnlen)1, (ftnlen)20);
		}
	    }
	    errch_(", #", ".", (ftnlen)3, (ftnlen)1);
	    sigerr_("SPICE(MISSINGTIMEINFO)", (ftnlen)22);
	    chkout_("UNITIM", (ftnlen)6);
	    ret_val = *epoch;
	    return ret_val;
	}

/*        At this point the kernel data checks are done. */

	nodata = FALSE_;
    }

/*     Normalize the IN and OUT scale variables */

    ucase_(insys, myin, insys_len, (ftnlen)8);
    ucase_(outsys, myout, outsys_len, (ftnlen)8);
    ssizec_(&c__2, types, (ftnlen)8);
    insrtc_(myin, types, (ftnlen)8, (ftnlen)8);
    insrtc_(myout, types, (ftnlen)8, (ftnlen)8);

/*     We will work with a local copy of EPOCH. */

    mytime = *epoch;

/*     First make sure both types are recognized. */

    if (! setc_(types, "<", recog, (ftnlen)8, (ftnlen)1, (ftnlen)8)) {
	setmsg_("The time types recognized by UNITIM are: TAI, GPS, TT, TDT,"
		" JDTDT, TDB, ET, JED, JDTDB. At least one of the inputs (#, "
		"#) was not in the list of recognized types. ", (ftnlen)163);
	errch_("#", myin, (ftnlen)1, (ftnlen)8);
	errch_("#", myout, (ftnlen)1, (ftnlen)8);
	sigerr_("SPICE(BADTIMETYPE)", (ftnlen)18);
	chkout_("UNITIM", (ftnlen)6);
	ret_val = *epoch;
	return ret_val;
    }

/*     If the input and output types are the same, just copy the input */
/*     epoch to the output and call it quits. */

    if (s_cmp(myin, myout, (ftnlen)8, (ftnlen)8) == 0) {
	ret_val = mytime;
	chkout_("UNITIM", (ftnlen)6);
	return ret_val;
    }

/*     Determine the base types of the input and output types. */

    intdt = elemc_(myin, typtdt, (ftnlen)8, (ftnlen)8);
    outtdt = elemc_(myout, typtdt, (ftnlen)8, (ftnlen)8);
    intdb = ! intdt;
    outtdb = ! outtdt;

/*     The two types, TDT and TDB, will be used as the fundamental */
/*     base used in conversions. */

/*        TAI, GPS, and JDTDT will be converted to TDT */
/*        JED and JDTDB will be converted to TDB. */
/*        ET means TDB; TT means TDT. */


    if (s_cmp(myin, "TAI", (ftnlen)8, (ftnlen)3) == 0) {
	mytime += dta;
    } else if (s_cmp(myin, "GPS", (ftnlen)8, (ftnlen)3) == 0) {
	mytime += dta + 19.;
    } else if (s_cmp(myin, "JDTDT", (ftnlen)8, (ftnlen)5) == 0) {
	mytime = (mytime - jd2000) * secspd;
    } else if (s_cmp(myin, "JED", (ftnlen)8, (ftnlen)3) == 0) {
	mytime = (mytime - jd2000) * secspd;
    } else if (s_cmp(myin, "JDTDB", (ftnlen)8, (ftnlen)5) == 0) {
	mytime = (mytime - jd2000) * secspd;
    }

/*     At this point, MYTIME has been converted from its input */
/*     to one of the base types. */

/*     Next change type from TDB to TDT or vice versa, if */
/*     required.  (The time is already in TDT or TDB). */

    if (intdt && outtdb) {
	tdt = mytime;
	tdb = tdt + k * sin(m[0] + m[1] * tdt + eb * sin(m[0] + m[1] * tdt));
	mytime = tdb;
    } else if (intdb && outtdt) {

/*        What we have to do here is invert the formula used to get */
/*        TDB from TDT that was used above. */

/*        Of course solving the equation */

/*           TDB = TDT + K*SIN { M0 + M1*TDT + EB*SIN( MO + M1*TDT ) } */

/*        analytically for TDT if given TDB is no piece of cake. */
/*        However, we can get as close as we want to TDT if */
/*        we notice a few tricks.  First, let's let f(t) denote the */
/*        function */

/*           f(t) = SIN( M0 + M1*t + EB*SIN( M0 + M1*t ) ) */

/*        With this simpler notation we can rewrite our problem */
/*        as that of solving the equation */

/*           y = t + K*f(t) */

/*        for t given y.  Whichever t satisfies this equation will be */
/*        unique. The uniqueness of the solution is ensured because the */
/*        expression on the right-hand side of the equation is */
/*        monotone increasing in t. */

/*        Let's suppose that t is the solution, then the following */
/*        is true. */

/*           t = y - K*f(t) */

/*        but we can also replace the t on the right hand side of the */
/*        equation by y - K*f(t).  Thus */

/*           t = y - K*f( y - K*f(t)) */

/*             = y - K*f( y - K*f( y - K*f(t))) */

/*             = y - K*f( y - K*f( y - K*f( y - K*f(t)))) */

/*             = y - K*f( y - K*f( y - K*f( y - K*f( y - K*f(t))))) */
/*             . */
/*             . */
/*             . */
/*             = y - K*f( y - K*f( y - K*f( y - K*f( y - K*f(y - ... ))) */

/*        and so on, for as long as we have patience to perform the */
/*        substitutions. */

/*        The point of doing this recursive substitution is that we */
/*        hope to move t to an insignificant part of the computation. */
/*        This would seem to have a reasonable chance of success since */
/*        K is a small number and f is bounded by 1. */

/*        Following this idea, we will attempt to solve for t using */
/*        the recursive method outlined below. */

/*        We will make our first guess at t, call it t_0. */

/*         t_0 = y */

/*        Our next guess, t_1, is given by: */

/*         t_1 = y - K*f(t_0) */

/*        And so on: */

/*         t_2 = y - K*f(t_1)        [ = y - K*f(y - K*f(y))            ] */
/*         t_3 = y - K*f(t_2)        [ = y - K*f(y - K*f(y - K*f(y)))   ] */
/*             . */
/*             . */
/*             . */
/*         t_n = y - K*f(t_(n-1))    [ = y - K*f(y - K*f(y - K*f(y...)))] */

/*        The questions to ask at this point are: */

/*           1) Do the t_i's converge? */
/*           2) If they converge, do they converge to t? */
/*           3) If they converge to t, how fast do they get there? */

/*        1) The sequence of approximations converges. */

/*           | t_n - t_(n-1) | =    [ y - K*f( t_(n-1) ) ] */
/*                               -  [ y - K*f( t_(n-2) ) ] */

/*                             =  K*[ f( t_(n-2) ) - f( t_(n-1) ) ] */

/*           The function f has an important property. The absolute */
/*           value of its derivative is always less than M1*(1+EB). */
/*           This means that for any pair of real numbers s,t */

/*              | f(t) - f(s) |  < M1*(1+EB)*| t - s |. */

/*           From this observation, we can see that */

/*             | t_n - t_(n-1) | < K*M1*(1+EB)*| t_(n-1) - t_(n-2) | */

/*           With this fact available, we could (with a bit more work) */
/*           conclude that the sequence of t_i's converges and that */
/*           it converges at a rate that is at least as fast as the */
/*           sequence L, L**2, L**3, .... */

/*           Where L = K*M1*(1+EB) << 1. */

/*         2) If we let t be the limit of the t_i's then it follows */
/*            that */

/*               t = y - K*f(t). */

/*            or that */

/*               y = t + K*f(t). */

/*         3) As we already pointed out, the sequence of t_i's */
/*            converges at least as fast as the geometric series */
/*            L, L**2, ... */


/*        Since K*M1*(1+EB) is quite small (on the order of 10**-9) */
/*        3 iterations should get us as close as we can get to the */
/*        solution for TDT */

	tdb = mytime;
	tdt = tdb;
	for (i__ = 1; i__ <= 3; ++i__) {
	    tdt = tdb - k * sin(m[0] + m[1] * tdt + eb * sin(m[0] + m[1] * 
		    tdt));
	}
	mytime = tdt;
    }

/*     Now MYTIME is in the base type of the requested output. */
/*     If further conversion is required, we do it here. */

    if (s_cmp(myout, "TAI", (ftnlen)8, (ftnlen)3) == 0) {
	mytime -= dta;
    } else if (s_cmp(myout, "GPS", (ftnlen)8, (ftnlen)3) == 0) {
	mytime -= dta + 19.;
    } else if (s_cmp(myout, "JDTDT", (ftnlen)8, (ftnlen)5) == 0) {
	mytime = mytime / secspd + jd2000;
    } else if (s_cmp(myout, "JED", (ftnlen)8, (ftnlen)3) == 0) {
	mytime = mytime / secspd + jd2000;
    } else if (s_cmp(myout, "JDTDB", (ftnlen)8, (ftnlen)5) == 0) {
	mytime = mytime / secspd + jd2000;
    }
    ret_val = mytime;
    chkout_("UNITIM", (ftnlen)6);
    return ret_val;
} /* unitim_ */

