/* zzgetelm.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__3 = 3;
static integer c__2 = 2;
static integer c__4 = 4;

/* $Procedure ZZGETELM ( Get the components from two-line elements) */
/* Subroutine */ int zzgetelm_(integer *frstyr, char *lines, doublereal *
	epoch, doublereal *elems, logical *ok, char *error, ftnlen lines_len, 
	ftnlen error_len)
{
    /* Initialized data */

    static logical first = TRUE_;

    /* System generated locals */
    address a__1[3], a__2[2];
    integer i__1, i__2, i__3[3], i__4[2];

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_rnge(char *, integer, char *, integer), s_cmp(char *, char *, 
	    ftnlen, ftnlen);
    /* Subroutine */ int s_cat(char *, char **, integer *, integer *, ftnlen);

    /* Local variables */
    static doublereal ndd60;
    static char cday[32];
    static doublereal incl, ndt20;
    static integer bexp, year;
    static doublereal tvec[8];
    static char term[160*13];
    static integer nexp;
    static doublereal node0;
    static integer i__, k;
    static char cndd60[32], cincl[32];
    static doublereal omega;
    static char cndt20[32];
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    static char cyear[32];
    extern /* Subroutine */ int repmc_(char *, char *, char *, char *, ftnlen,
	     ftnlen, ftnlen, ftnlen), repmd_(char *, char *, doublereal *, 
	    integer *, char *, ftnlen, ftnlen, ftnlen);
    static char ciexp[32];
    static integer begyr;
    static doublereal bstar;
    extern /* Subroutine */ int repmi_(char *, char *, integer *, char *, 
	    ftnlen, ftnlen, ftnlen);
    static doublereal power[75];
    extern doublereal twopi_(void);
    static char cnode0[32], comega[32];
    static doublereal mo, no;
    static char cibexp[32];
    static integer yr;
    static char cbstar[32];
    extern integer lastnb_(char *, ftnlen);
    extern /* Subroutine */ int nparsd_(char *, doublereal *, char *, integer 
	    *, ftnlen, ftnlen), nparsi_(char *, integer *, char *, integer *, 
	    ftnlen, ftnlen), chkout_(char *, ftnlen);
    static doublereal d2r, pi2;
    extern /* Subroutine */ int ttrans_(char *, char *, doublereal *, ftnlen, 
	    ftnlen);
    static char errprs[160*13];
    extern logical return_(void);
    static doublereal ecc, day;
    static char cmo[32], cno[32];
    extern doublereal rpd_(void);
    static integer ptr;
    static char cecc[32];

/* $ Abstract */

/*    Given a the "lines" of a two-line element set, parse the */
/*    lines and return the elements in units suitable for use */
/*    in SPICE software. */

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

/*     PARSING */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     FRSTYR     I   year of earliest representable two-line elements */
/*     LINES      I   a pair of "lines" containing two-line elements */
/*     EPOCH      O   The epoch of the elements in seconds past J2000 */
/*     ELEMS      O   The elements converted to SPICE units. */
/*     OK         O   Boolean indicating error state. */
/*     ERROR      O   String describing error. */

/* $ Detailed_Input */

/*     FRSTYR    is the first year possible for two line elements. */
/*               Since two line elements allow only two digits for */
/*               the year, some conventions must be followed concerning */
/*               which century the two digits refer to .  FRSTYR */
/*               is the year of the earliest representable elements. */
/*               The two-digit year is mapped to the year in */
/*               the interval from FRSTYR to FRSTYR + 99 that */
/*               has the same last two digits as the two digit */
/*               year in the element set.  For example if FRSTYR */
/*               is set to 1960  then the two digit years are mapped */
/*               as shown in the table below: */

/*               Two-line         Maps to */
/*               element year */
/*                  00            2000 */
/*                  01            2001 */
/*                  02            2002 */
/*                   .              . */
/*                   .              . */
/*                   .              . */
/*                  58            2058 */
/*                  59            2059 */
/*                 -------------------- */
/*                  60            1960 */
/*                  61            1961 */
/*                  62            1962 */
/*                   .              . */
/*                   .              . */
/*                   .              . */
/*                  99            1999 */

/*                Note that if Space Command should decide to represent */
/*                years in 21st century as 100 + the last two digits */
/*                of the year (for example: 2015 is represented as 115) */
/*                instead of simply dropping the first two digits of */
/*                the year, this routine will correctly map the year */
/*                as long as you set FRSTYR to some value between 1900 */
/*                and 1999. */

/*     LINES      is a pair of lines of text that comprise a Space */
/*                command ``two-line element'' set.  These text lines */
/*                should be the same as they are presented in the */
/*                two-line element files available from Space Command */
/*                (formerly NORAD). Below is an example of a two-line */
/*                set for TOPEX. */

/*  TOPEX */
/*  1 22076U 92052A   97173.53461370 -.00000038  00000-0  10000-3 0   594 */
/*  2 22076  66.0378 163.4372 0008359 278.7732  81.2337 12.80930736227550 */


/* $ Detailed_Output */

/*     EPOCH      is the epoch of the two line elements supplied via */
/*                the input array LINES.  Epoch is returned in TDB */
/*                seconds past J2000. */

/*     ELEMS      is an array containing the elements from the two line */
/*                set supplied via the array LINES.  The elements are */
/*                in units suitable for use by the SPICE routine */
/*                EV2LIN. */

/*                Also note that the elements XNDD6O and BSTAR */
/*                incorporate the exponential factor present in the */
/*                input two line elements in LINES.  (See particulars */
/*                below. */

/*                    ELEMS (  1 ) = XNDT2O in radians/minute**2 */
/*                    ELEMS (  2 ) = XNDD6O in radians/minute**3 */
/*                    ELEMS (  3 ) = BSTAR */
/*                    ELEMS (  4 ) = XINCL  in radians */
/*                    ELEMS (  5 ) = XNODEO in radians */
/*                    ELEMS (  6 ) = EO */
/*                    ELEMS (  7 ) = OMEGAO in radians */
/*                    ELEMS (  8 ) = XMO    in radians */
/*                    ELEMS (  9 ) = XNO    in radians/minute */
/*                    ELEMS ( 10 ) = EPOCH of the elements in seconds */
/*                                   past ephemeris epoch J2000. */

/*     OK         a boolean flag indicating whether an error occured */
/*                while processing the TLE. Processing errors include */
/*                incorrect format for TLEs, angular values beyond */
/*                allowed range. */

/*     ERROR      a string containing a description of any TLE */
/*                processing error. */

/* $ Parameters */

/*     None. */

/* $ Files */

/*     You must have loaded a SPICE leapseconds kernel into the */
/*     kernel pool prior to caling this routine. */

/* $ Exceptions */

/*     This routine does not signal errors, rather it returns an */
/*     error flag and description to the calling program. Errors */
/*     reported: */

/*     1) TLE line has incorrect format. */

/*     2) A TLE '2' line has a different vehicle tag than the */
/*         corresponding '1' line. */

/*     3) TLE data length more than 69 characters, or less than 68 */
/*        characters. */

/*     4) An NPARS* routine cannot parse a string to a numeric value. */

/*     5) The value of an angular measure, NODE0, OMEGA, MO, or INCL */
/*        fails to lie within the expected numerical bounds. */

/* $ Particulars */

/*     This routine parses a Space Command Two-line element set and */
/*     returns the orbital elements properly scaled and in units */
/*     suitable for use by other SPICE software.  Input elements */
/*     have the form: */

/*  1 22076U 92052A   97173.53461370 -.00000038  00000-0  10000-3 0   594 */
/*  2 22076  66.0378 163.4372 0008359 278.7732  81.2337 12.80930736227550 */
/*  ^ */
/*  123456789012345678901234567890123456789012345678901234567890123456789 */
/*           1         2         3         4         5         6 */

/*     The ``raw'' elements in the first  and second lines are marked */
/*     below.  Note that in several instances exponents and decimal */
/*     points are implied.  Also note that */
/*     input units are degrees, degrees/day**n and revolutions/day. */


/*                      DAY OF YEAR             NDD60    BSTAR */
/*                      vvvvvvvvvvvv            vvvvvv   vvvvvv */
/*  --------------------------------------------------------------------- */
/*  1 22076U 92052A   97173.53461370 -.00000038  00000-0  10000-3 0   594 */
/*  --------------------------------------------------------------------- */
/*                    ^^             ^^^^^^^^^^       ^^       ^^ */
/*                    YEAR             NDT20          IEXP     IBEXP */



/*     The ``raw'' elements in the second line are marked below */
/*                   NODE0            OMEGA             N0 */
/*                   vvvvvvvv         vvvvvvvv          vvvvvvvvvvv */
/*  --------------------------------------------------------------------- */
/*  2 22076  66.0378 163.4372 0008359 278.7732  81.2337 12.80930736227550 */
/*  --------------------------------------------------------------------- */
/*          ^^^^^^^^          ^^^^^^^          ^^^^^^^^ */
/*          Inclination       Eccentricity     M0 */

/*     This routine extracts these values ``inserts'' the implied */
/*     decimal points and exponents and then converts the inputs */
/*     to units of radians, radians/minute, radians/minute**2, and */
/*     radians/minute**3 */

/* $ Examples */

/*     Suppose you have a set of two-line elements and an array */
/*     containing the related geophysical constants necessary */
/*     to evaluate a state.  The example below shows how you */
/*     can use this routine together with the routine EV2LIN to */
/*     propagate a state to an epoch of interest. */


/*        The parameters below will make it easier to make assignments */
/*        to the array GEOPHS required by EV2LIN. */

/*        J2  --- location of J2 */
/*        J3  --- location of J3 */
/*        J4  --- location if J4 */
/*        KE  --- location of KE = sqrt(GM) in eart-radii**1.5/MIN */
/*        QO  --- location of upper bound of atmospheric model in KM */
/*        SO  --- location of lower bound of atmospheric model in KM */
/*        ER  --- location of earth equatorial radius in KM. */
/*        AE  --- location of distance units/earth radius */

/*        PARAMETER           ( J2 = 1 ) */
/*        PARAMETER           ( J3 = 2 ) */
/*        PARAMETER           ( J4 = 3 ) */
/*        PARAMETER           ( KE = 4 ) */
/*        PARAMETER           ( QO = 5 ) */
/*        PARAMETER           ( SO = 6 ) */
/*        PARAMETER           ( ER = 7 ) */
/*        PARAMETER           ( AE = 8 ) */


/*        We set the lower bound for the years to be the beginning */
/*        of the space age. */

/*        FRSTYR = 1957 */

/*        Read in the next two lines from the text file that contains */
/*        the two-line elements.  We assume that file has been opened */
/*        properly and that we have set the ``file pointer'' to the */
/*        correct location for reading the next set of elements. */

/*        READ  (UNIT,FMT='(A)' ) LINE(1) */
/*        READ  (UNIT,FMT='(A)' ) LINE(2) */

/*        CALL ZZGETELM ( FRSTYR, LINE, EPOCH, ELEMS, OK, ERROR ) */

/*        Set up the geophysical quantities.  At last check these */
/*        were the values used by Space Command. */

/*        GEOPHS( J2 ) =    1.082616D-3 */
/*        GEOPHS( J3 ) =   -2.53881D-6 */
/*        GEOPHS( J4 ) =   -1.65597D-6 */
/*        GEOPHS( KE ) =    7.43669161D-2 */
/*        GEOPHS( QO ) =  120.0D0 */
/*        GEOPHS( SO ) =   78.0D0 */
/*        GEOPHS( ER ) = 6378.135D0 */
/*        GEOPHS( AE ) =    1.0D0 */

/*        Now propagate the state using EV2LIN to the epoch of */
/*        interest. */

/*        CALL EV2LIN ( ET, GEOPHS, ELEMS, STATE ) */


/* $ Restrictions */

/*    The format of the two-line elements suffer from a "millenium" */
/*    problem---only two digits are used for the year of the elements. */
/*    It is not clear how Space Command will deal with this problem */
/*    as the year 2000 comes and goes.  We hope that by adjusting */
/*    the input FRSTYR you should be able to use this */
/*    routine well into the 21st century.  However, since we can't */
/*    predict how others will resolve the millenium problem we */
/*    can't be sure that our approach will be addequate to deal with */
/*    the problem. */

/*    The approach taken to mapping the two-digit year to the */
/*    full year is given by the code below. Here, YR is the */
/*    integer obtained by parsing the two-digit year from the first */
/*    line of the elements. */

/*        BEGYR = (FRSTYR/100)*100 */
/*        YEAR  = BEGYR + YR */

/*        IF ( YEAR .LT. FRSTYR ) THEN */
/*           YEAR = YEAR + 100 */
/*        END IF */

/*     This mapping will be changed if future two-line element */
/*     representations make this method of computing the full year */
/*     inaccurate. */

/* $ Author_and_Institution */

/*     W.L. Taber      (JPL) */
/*     E.D. Wright     (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */


/* -    SPICELIB Version 1.0.0, 26-APR-2004 (EDW) */

/*        Modified routine GETELM to confirm */
/*        acceptable range for angular measures and exponents. */
/*        The routine does not signal errors,it returns an error */
/*        flag and error string to the calling program. */

/*        Routine named ZZGETELM from GETELM. */

/* -& */
/* $ Index_Entries */

/*     Parse two-line elements */

/* -& */

/*     Spicelib functions */


/*     An enumeration of the various components of the */
/*     elements array---ELEMS */

/*        KNDT20 */
/*        KNDD60 */
/*        KBSTAR */
/*        KINCL */
/*        KNODE0 */
/*        KECC */
/*        KOMEGA */
/*        KMO */
/*        KNO */


/*     Character string lengths */


/*     Maximum exponent (base 10) */


/*     Double precision constants. */


/*     Minutes/day */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("ZZGETELM", (ftnlen)8);

/*     Initialize the error indicators and the elements to zero. */

    *ok = TRUE_;
    s_copy(error, " ", error_len, (ftnlen)1);
    elems[0] = 0.;
    elems[1] = 0.;
    elems[2] = 0.;
    elems[3] = 0.;
    elems[4] = 0.;
    elems[5] = 0.;
    elems[6] = 0.;
    elems[7] = 0.;
    elems[8] = 0.;
    elems[9] = 0.;
    *epoch = 0.;

/*     First entry initialization. */

    if (first) {

/*        Define two constants. This initialization proves the most */
/*        useful when processing thousands of TLE sets. */

	d2r = rpd_();
	pi2 = twopi_();
	first = FALSE_;
	power[37] = 1.;
	for (i__ = 1; i__ <= 37; ++i__) {
	    power[(i__1 = i__ + 37) < 75 && 0 <= i__1 ? i__1 : s_rnge("power",
		     i__1, "zzgetelm_", (ftnlen)535)] = power[(i__2 = i__ + 
		    36) < 75 && 0 <= i__2 ? i__2 : s_rnge("power", i__2, 
		    "zzgetelm_", (ftnlen)535)] * 10.;
	    power[(i__1 = -i__ + 37) < 75 && 0 <= i__1 ? i__1 : s_rnge("power"
		    , i__1, "zzgetelm_", (ftnlen)536)] = 1. / power[(i__2 = 
		    i__ + 37) < 75 && 0 <= i__2 ? i__2 : s_rnge("power", i__2,
		     "zzgetelm_", (ftnlen)536)];
	}
	s_copy(term, "\"YEAR\" (characters 19 to 20 of the first line of a t"
		"wo-line element set)", (ftnlen)160, (ftnlen)72);
	s_copy(term + 160, "\"DAY\" (characters 21 to 32 of the first line o"
		"f a two-line element set)", (ftnlen)160, (ftnlen)71);
	s_copy(term + 320, "\"NDT20\" (characters 34 to 43 of the first line"
		" of a two-line element set)", (ftnlen)160, (ftnlen)73);
	s_copy(term + 480, "\"NDD60\" (characters 45 to 45 of the first line"
		" of a two-line element set)", (ftnlen)160, (ftnlen)73);
	s_copy(term + 640, "\"IEXP\" (characters 51 to 52 of the first line "
		"of a two-line element set)", (ftnlen)160, (ftnlen)72);
	s_copy(term + 800, "\"BSTAR\" (characters 54 to 54 of the first line"
		" of a two-line element set)", (ftnlen)160, (ftnlen)73);
	s_copy(term + 960, "\"IBEXP\" (characters 60 to 61 of the first line"
		" of a two-line element set)", (ftnlen)160, (ftnlen)73);
	s_copy(term + 1120, "\"INCL\" (characters 9 to 16 of the second line"
		" of a two-line element set)", (ftnlen)160, (ftnlen)72);
	s_copy(term + 1280, "\"NODE0\" (characters 18 to 25 of the second li"
		"ne of a two-line element set)", (ftnlen)160, (ftnlen)74);
	s_copy(term + 1440, "\"ECC\" (characters 27 to 33 of the second line"
		" of a two-line element set)", (ftnlen)160, (ftnlen)72);
	s_copy(term + 1600, "\"OMEGA\" (characters 35 to 42 of the second li"
		"ne of a two-line element set)", (ftnlen)160, (ftnlen)74);
	s_copy(term + 1760, "\"MO\" (characters 44 to 51 of the second line "
		"of a two-line element set)", (ftnlen)160, (ftnlen)71);
	s_copy(term + 1920, "\"NO\" (characters 53 to 63 of the second line "
		"of a two-line element set)", (ftnlen)160, (ftnlen)71);
    }

/*     Ensure the vehicle IDs match in each line. */

    if (s_cmp(lines + 1, lines + (lines_len + 1), (ftnlen)6, (ftnlen)6) != 0) 
	    {

/*        Vehicle IDs do not match. Flag an error. */

	s_copy(error, "Line 1 of the TLE pair tagged with vehicle ID #1,  li"
		"ne 2 of TLE pair tagged with vehicle ID #2", error_len, (
		ftnlen)95);
	repmc_(error, "#1", lines + 1, error, error_len, (ftnlen)2, (ftnlen)6,
		 error_len);
	repmc_(error, "#2", lines + (lines_len + 1), error, error_len, (
		ftnlen)2, (ftnlen)6, error_len);
	*ok = FALSE_;
	chkout_("ZZGETELM", (ftnlen)8);
	return 0;
    }

/*    Check line format and length. */

    for (k = 1; k <= 2; ++k) {
	if (lastnb_(lines + ((i__1 = k - 1) < 2 && 0 <= i__1 ? i__1 : s_rnge(
		"lines", i__1, "zzgetelm_", (ftnlen)604)) * lines_len, 
		lines_len) != 68 && lastnb_(lines + ((i__2 = k - 1) < 2 && 0 
		<= i__2 ? i__2 : s_rnge("lines", i__2, "zzgetelm_", (ftnlen)
		604)) * lines_len, lines_len) != 69) {

/*          The TLE data line was not 68 or 69 characters long (ignoring */
/*          trailing whitespace). Flag an error. */

	    s_copy(error, "Line #1 of the TLE has incorrect data length. Exp"
		    "ected length 68 or 69 elements, actual length: #2. TLE l"
		    "ine value: #3", error_len, (ftnlen)118);
	    repmi_(error, "#1", &k, error, error_len, (ftnlen)2, error_len);
	    i__2 = lastnb_(lines + ((i__1 = k - 1) < 2 && 0 <= i__1 ? i__1 : 
		    s_rnge("lines", i__1, "zzgetelm_", (ftnlen)615)) * 
		    lines_len, lines_len);
	    repmi_(error, "#2", &i__2, error, error_len, (ftnlen)2, error_len)
		    ;
	    repmc_(error, "#3", lines + ((i__1 = k - 1) < 2 && 0 <= i__1 ? 
		    i__1 : s_rnge("lines", i__1, "zzgetelm_", (ftnlen)616)) * 
		    lines_len, error, error_len, (ftnlen)2, lines_len, 
		    error_len);
	    *ok = FALSE_;
	    chkout_("ZZGETELM", (ftnlen)8);
	    return 0;
	}
    }

/*     This isn't particularly pretty, but it is straight */
/*     forward.  According to the documentation on the two line */
/*     element sets, (as well as what's indicated by the program */
/*     driver that is documented in SPACETRACK REPORT NO.3 */
/*     we can simply pick out the various components of the */
/*     elements from the input lines 1 and 2. */

/*     For the record we include the DECODE statement in DRIVER */
/*     for fetching the data out of lines one and two (after a bit */
/*     of pretty printing).  Note that some of these formats have */
/*     and implied decimal point.  In particular f6.5 and f7.7  in */
/*     all other cases the decimal points seem to be given explicitely. */

/*     decode (abuf,702) epoch,     xndt20,                       ... */
/*                       xndd60,    iexp,        bstar,    ibexp, ... */
/*                       xincl,     xnodeo,                       ... */
/*                       eo,        omegao,     xmo,       xno */
/*     format(      18x, d14.8, 1x, f10.8, */
/*                   1x, f6.5,      i2,     1x, f6.5,      i2,    /, */
/*                   8x, f8.4,  1x, f8.4, */
/*                   1x, f7.7,  1x, f8.4,   1x, f8.4   1x, f11.8 ) */

/*     Note that in the two-line element sets, the epoch is read */
/*     as a single number.  However the documentation that describes */
/*     this data (as well as the code in THETAG) show that it's a lot */
/*     easier to capture the year and day of year separately. */

    s_copy(cyear, lines + 18, (ftnlen)32, (ftnlen)2);
    s_copy(cday, lines + 20, (ftnlen)32, (ftnlen)12);
    s_copy(cndt20, lines + 33, (ftnlen)32, (ftnlen)10);
/* Writing concatenation */
    i__3[0] = 1, a__1[0] = lines + 44;
    i__3[1] = 1, a__1[1] = ".";
    i__3[2] = 5, a__1[2] = lines + 45;
    s_cat(cndd60, a__1, i__3, &c__3, (ftnlen)32);
    s_copy(ciexp, lines + 50, (ftnlen)32, (ftnlen)2);
/* Writing concatenation */
    i__3[0] = 1, a__1[0] = lines + 53;
    i__3[1] = 1, a__1[1] = ".";
    i__3[2] = 5, a__1[2] = lines + 54;
    s_cat(cbstar, a__1, i__3, &c__3, (ftnlen)32);
    s_copy(cibexp, lines + 59, (ftnlen)32, (ftnlen)2);
    s_copy(cincl, lines + (lines_len + 8), (ftnlen)32, (ftnlen)8);
    s_copy(cnode0, lines + (lines_len + 17), (ftnlen)32, (ftnlen)8);
/* Writing concatenation */
    i__4[0] = 2, a__2[0] = "0.";
    i__4[1] = 7, a__2[1] = lines + (lines_len + 26);
    s_cat(cecc, a__2, i__4, &c__2, (ftnlen)32);
    s_copy(comega, lines + (lines_len + 34), (ftnlen)32, (ftnlen)8);
    s_copy(cmo, lines + (lines_len + 43), (ftnlen)32, (ftnlen)8);
    s_copy(cno, lines + (lines_len + 52), (ftnlen)32, (ftnlen)11);

/*     Parse the numerical values from the data string. */

    nparsi_(cyear, &yr, errprs, &ptr, (ftnlen)32, (ftnlen)160);
    nparsd_(cday, &day, errprs + 160, &ptr, (ftnlen)32, (ftnlen)160);
    nparsd_(cndt20, &ndt20, errprs + 320, &ptr, (ftnlen)32, (ftnlen)160);
    nparsd_(cndd60, &ndd60, errprs + 480, &ptr, (ftnlen)32, (ftnlen)160);
    nparsi_(ciexp, &nexp, errprs + 640, &ptr, (ftnlen)32, (ftnlen)160);
    nparsd_(cbstar, &bstar, errprs + 800, &ptr, (ftnlen)32, (ftnlen)160);
    nparsi_(cibexp, &bexp, errprs + 960, &ptr, (ftnlen)32, (ftnlen)160);
    nparsd_(cincl, &incl, errprs + 1120, &ptr, (ftnlen)32, (ftnlen)160);
    nparsd_(cnode0, &node0, errprs + 1280, &ptr, (ftnlen)32, (ftnlen)160);
    nparsd_(cecc, &ecc, errprs + 1440, &ptr, (ftnlen)32, (ftnlen)160);
    nparsd_(comega, &omega, errprs + 1600, &ptr, (ftnlen)32, (ftnlen)160);
    nparsd_(cmo, &mo, errprs + 1760, &ptr, (ftnlen)32, (ftnlen)160);
    nparsd_(cno, &no, errprs + 1920, &ptr, (ftnlen)32, (ftnlen)160);

/*     Check for parse errors. */

    for (i__ = 1; i__ <= 13; ++i__) {
	if (s_cmp(errprs + ((i__1 = i__ - 1) < 13 && 0 <= i__1 ? i__1 : 
		s_rnge("errprs", i__1, "zzgetelm_", (ftnlen)692)) * 160, 
		" ", (ftnlen)160, (ftnlen)1) != 0) {

/*           Something could not parse. Set the error message then */
/*           return. */

	    s_copy(error, "An error occurred while trying to parse the term "
		    "#. The diagnostic was:  # ", error_len, (ftnlen)75);
	    repmc_(error, "#", term + ((i__1 = i__ - 1) < 13 && 0 <= i__1 ? 
		    i__1 : s_rnge("term", i__1, "zzgetelm_", (ftnlen)700)) * 
		    160, error, error_len, (ftnlen)1, (ftnlen)160, error_len);
	    repmc_(error, "#", errprs + ((i__1 = i__ - 1) < 13 && 0 <= i__1 ? 
		    i__1 : s_rnge("errprs", i__1, "zzgetelm_", (ftnlen)701)) *
		     160, error, error_len, (ftnlen)1, (ftnlen)160, error_len)
		    ;
	    *ok = FALSE_;
	    chkout_("ZZGETELM", (ftnlen)8);
	    return 0;
	}
    }

/*     Check for reasonable exponets; a single digit. These should */
/*     probably be LE 0. */

    if (abs(nexp) > 9) {
	s_copy(error, "NEXP (exponent) not a single digit. Actual value #1", 
		error_len, (ftnlen)51);
	repmi_(error, "#1", &nexp, error, error_len, (ftnlen)2, error_len);
	*ok = FALSE_;
	chkout_("ZZGETELM", (ftnlen)8);
	return 0;
    }
    if (abs(bexp) > 9) {
	s_copy(error, "BEXP (exponent) not a single digit. Actual value #1", 
		error_len, (ftnlen)51);
	repmi_(error, "#1", &bexp, error, error_len, (ftnlen)2, error_len);
	*ok = FALSE_;
	chkout_("ZZGETELM", (ftnlen)8);
	return 0;
    }

/*     Confirm correct bounds on angular values. */

/*     NODE0 - right ascension of the ascending node, [0,360) */

    if (node0 < 0. || node0 >= 360.) {
	s_copy(error, "NODE0 (RA acend node) expected bounds [0,360). Actual"
		" value #1", error_len, (ftnlen)62);
	repmd_(error, "#1", &node0, &c__4, error, error_len, (ftnlen)2, 
		error_len);
	*ok = FALSE_;
	chkout_("ZZGETELM", (ftnlen)8);
	return 0;
    }

/*     OMEAGA - argument of the periapsis, [0,360) */

    if (omega < 0. || omega >= 360.) {
	s_copy(error, "OMEGA (arg periap) expected bounds [0,360). Actual va"
		"lue #1", error_len, (ftnlen)59);
	repmd_(error, "#1", &omega, &c__4, error, error_len, (ftnlen)2, 
		error_len);
	*ok = FALSE_;
	chkout_("ZZGETELM", (ftnlen)8);
	return 0;
    }

/*     MO - mean anomoly, [0,360) */

    if (mo < 0. || mo >= 360.) {
	s_copy(error, "MO (mean anomoly) expected bounds [0,360). Actual val"
		"ue #1", error_len, (ftnlen)58);
	repmd_(error, "#1", &mo, &c__4, error, error_len, (ftnlen)2, 
		error_len);
	*ok = FALSE_;
	chkout_("ZZGETELM", (ftnlen)8);
	return 0;
    }

/*     INCL - inclination, [0,180] */

    if (incl < 0. || incl > 180.) {
	s_copy(error, "INCL (inclination) expected bounds [0,180). Actual va"
		"lue #1", error_len, (ftnlen)59);
	repmd_(error, "#1", &incl, &c__4, error, error_len, (ftnlen)2, 
		error_len);
	*ok = FALSE_;
	chkout_("ZZGETELM", (ftnlen)8);
	return 0;
    }

/*     NO - mean motion (0,20) (Earth orbiter). */

    if (no > 20. || no < 0.) {
	s_copy(error, "NO (mean motion) expected bounds (0,20). Actual value"
		" #1", error_len, (ftnlen)56);
	repmd_(error, "#1", &no, &c__4, error, error_len, (ftnlen)2, 
		error_len);
	*ok = FALSE_;
	chkout_("ZZGETELM", (ftnlen)8);
	return 0;
    }

/*     Finish up the computation of NDD60 and BSTAR */

    ndd60 *= power[(i__1 = nexp + 37) < 75 && 0 <= i__1 ? i__1 : s_rnge("pow"
	    "er", i__1, "zzgetelm_", (ftnlen)827)];
    bstar *= power[(i__1 = bexp + 37) < 75 && 0 <= i__1 ? i__1 : s_rnge("pow"
	    "er", i__1, "zzgetelm_", (ftnlen)828)];

/*     Convert everything from degrees to radians ... */

    node0 *= d2r;
    omega *= d2r;
    mo *= d2r;
    incl *= d2r;

/*     ... and from revolutions/day**n to radians/minutes**n */

    no = no * pi2 / 1440.;
    ndt20 = ndt20 * pi2 / 1440. / 1440.;
    ndd60 = ndd60 * pi2 / 1440. / 1440. / 1440.;

/*     Finally, we need to convert the input epoch to */
/*     seconds past 2000. First let's adjust the year. */
/*     Add to YR the largest multiple of 100 that is */
/*     less than or equal to FRSTYR */

    begyr = *frstyr / 100 * 100;
    year = begyr + yr;
    if (year < *frstyr) {
	year += 100;
    }

/*     Compute the epoch of the year and date. */

    tvec[0] = (doublereal) year;
    tvec[1] = day;
    ttrans_("YD.D", "TDB", tvec, (ftnlen)4, (ftnlen)3);
    *epoch = tvec[0];

/*     That's it.  Load ELEMS with the elements and ship them */
/*     back to the calling routine. */

    elems[0] = ndt20;
    elems[1] = ndd60;
    elems[2] = bstar;
    elems[3] = incl;
    elems[4] = node0;
    elems[5] = ecc;
    elems[6] = omega;
    elems[7] = mo;
    elems[8] = no;
    elems[9] = *epoch;
    chkout_("ZZGETELM", (ftnlen)8);
    return 0;
} /* zzgetelm_ */

