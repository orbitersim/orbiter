/* zzdynvac.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__32 = 32;
static integer c__1 = 1;

/* $Procedure ZZDYNVAC ( Fetch array, character frame kernel variable ) */
/* Subroutine */ int zzdynvac_(char *frname, integer *frcode, char *item, 
	integer *maxn, integer *n, char *values, ftnlen frname_len, ftnlen 
	item_len, ftnlen values_len)
{
    extern /* Subroutine */ int chkin_(char *, ftnlen), errch_(char *, char *,
	     ftnlen, ftnlen), repmc_(char *, char *, char *, char *, ftnlen, 
	    ftnlen, ftnlen, ftnlen);
    logical found;
    extern /* Subroutine */ int repmi_(char *, char *, integer *, char *, 
	    ftnlen, ftnlen, ftnlen);
    char dtype[1];
    extern integer rtrim_(char *, ftnlen);
    extern logical failed_(void);
    integer codeln;
    char kvname[32], cdestr[32];
    integer itemln, nameln;
    extern logical return_(void);
    integer reqnam, reqnum;
    extern /* Subroutine */ int intstr_(integer *, char *, ftnlen), chkout_(
	    char *, ftnlen), dtpool_(char *, logical *, integer *, char *, 
	    ftnlen, ftnlen), setmsg_(char *, ftnlen), errint_(char *, integer 
	    *, ftnlen), sigerr_(char *, ftnlen), gcpool_(char *, integer *, 
	    integer *, integer *, char *, logical *, ftnlen, ftnlen);

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Look up array-valued character frame kernel variable.  The frame */
/*     name or frame ID may be used as part of the variable's name. */

/*     If the kernel variable is not present, or if the variable */
/*     has the wrong data type, signal an error. */

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

/*     FRAMES */
/*     KERNEL */
/*     PRIVATE */
/*     UTILITY */

/* $ Declarations */
/* $ Abstract */

/*     Include file zzdyn.inc */

/*     SPICE private file intended solely for the support of SPICE */
/*     routines.  Users should not include this file directly due */
/*     to the volatile nature of this file */

/*     The parameters defined below are used by the SPICELIB dynamic */
/*     frame subsystem. */

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

/*     This file declares parameters required by the dynamic */
/*     frame routines of the SPICELIB frame subsystem. */

/* $ Restrictions */

/*     The parameter BDNMLN is this routine must be kept */
/*     consistent with the parameter MAXL defined in */

/*        zzbodtrn.inc */


/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 2.0.0, 22-SEP-2020 (NJB) */

/*        Updated to support the product frame family. */

/* -    SPICELIB Version 1.1.0, 12-JAN-2005 (NJB) */

/*        Parameters KWX, KWY, KWZ renamed to KVX, KVY, KVZ. */

/* -    SPICELIB Version 1.0.0, 22-DEC-2004 (NJB) */

/* -& */

/*     String length parameters */
/*     ======================== */


/*     Kernel variable name length.  This parameter must be */
/*     kept consistent with the parameter MAXLEN used in the */
/*     POOL umbrella routine. */


/*     Length of a character kernel pool datum. This parameter must be */
/*     kept consistent with the parameter MAXCHR used in the POOL */
/*     umbrella routine. */


/*     Reference frame name length.  This parameter must be */
/*     kept consistent with the parameter WDSIZE used in the */
/*     FRAMEX umbrella routine. */


/*     Body name length.  This parameter is used to provide a level */
/*     of indirection so the dynamic frame source code doesn't */
/*     have to change if the name of this SPICELIB-scope parameter */
/*     is changed.  The value MAXL used here is defined in the */
/*     INCLUDE file */

/*        zzbodtrn.inc */

/*     Current value of MAXL = 36 */


/*     Numeric parameters */
/*     =================================== */

/*     The parameter MAXCOF is the maximum number of polynomial */
/*     coefficients that may be used to define an Euler angle */
/*     in an "Euler frame" definition */


/*     The parameter MXNFAC is the maximum number of factors in */
/*     a product frame. */


/*     The parameter LBSEP is the default angular separation limit for */
/*     the vectors defining a two-vector frame.  The angular separation */
/*     of the vectors must differ from Pi and 0 by at least this amount. */


/*     The parameter QEXP is used to determine the width of */
/*     the interval DELTA used for the discrete differentiation */
/*     of velocity in the routines ZZDYNFRM, ZZDYNROT, and their */
/*     recursive analogs.  This parameter is appropriate for */
/*     64-bit IEEE double precision numbers; when SPICELIB */
/*     is hosted on platforms where longer mantissas are supported, */
/*     this parameter (and hence this INCLUDE file) will become */
/*     platform-dependent. */

/*     The choice of QEXP is based on heuristics.  It's believed to */
/*     be a reasonable choice obtainable without expensive computation. */

/*     QEXP is the largest power of 2 such that */

/*        1.D0 + 2**QEXP  =  1.D0 */

/*     Given an epoch T0 at which a discrete derivative is to be */
/*     computed, this choice provides a value of DELTA that usually */
/*     contributes no round-off error in the computation of the function */
/*     evaluation epochs */

/*        T0 +/- DELTA */

/*     while providing the largest value of DELTA having this form that */
/*     causes the order of the error term O(DELTA**2) in the quadratic */
/*     function approximation to round to zero.  Note that the error */
/*     itself will normally be small but doesn't necessarily round to */
/*     zero.  Note also that the small function approximation error */
/*     is not a measurement of the error in the discrete derivative */
/*     itself. */

/*     For ET values T0 > 2**27 seconds past J2000, the value of */
/*     DELTA will be set to */

/*        T0 * 2**QEXP */

/*     For smaller values of T0, DELTA should be set to 1.D0. */


/*     Frame kernel parameters */
/*     ======================= */

/*     Parameters relating to kernel variable names (keywords) start */
/*     with the letters */

/*        KW */

/*     Parameters relating to kernel variable values start with the */
/*     letters */

/*        KV */


/*     Generic parameters */
/*     --------------------------------- */

/*     Token used to build the base frame keyword: */


/*     Frame definition style parameters */
/*     --------------------------------- */

/*     Token used to build the frame definition style keyword: */


/*     Token indicating parameterized dynamic frame. */


/*     Freeze epoch parameters */
/*     --------------------------------- */

/*     Token used to build the freeze epoch keyword: */


/*     Rotation state parameters */
/*     --------------------------------- */

/*     Token used to build the rotation state keyword: */


/*     Token indicating rotating rotation state: */


/*     Token indicating inertial rotation state: */


/*     Frame family parameters */
/*     --------------------------------- */

/*     Token used to build the frame family keyword: */


/*     Token indicating mean equator and equinox of date frame. */


/*     Token indicating mean ecliptic and equinox of date frame. */


/*     Token indicating true equator and equinox of date frame. */


/*     Token indicating two-vector frame. */


/*     Token indicating Euler frame. */


/*     Token indicating product frame. */


/*     "Of date" frame family parameters */
/*     --------------------------------- */

/*     Token used to build the precession model keyword: */


/*     Token used to build the nutation model keyword: */


/*     Token used to build the obliquity model keyword: */


/*     Mathematical models used to define "of date" frames will */
/*     likely accrue over time.  We will simply assign them */
/*     numbers. */


/*     Token indicating the Lieske earth precession model: */


/*     Token indicating the IAU 1980 earth nutation model: */


/*     Token indicating the IAU 1980 earth mean obliqity of */
/*     date model.  Note the name matches that of the preceding */
/*     nutation model---this is intentional.  The keyword */
/*     used in the kernel variable definition indicates what */
/*     kind of model is being defined. */


/*     Two-vector frame family parameters */
/*     --------------------------------- */

/*     Token used to build the vector axis keyword: */


/*     Tokens indicating axis values: */


/*     Prefixes used for primary and secondary vector definition */
/*     keywords: */


/*     Token used to build the vector definition keyword: */


/*     Token indicating observer-target position vector: */


/*     Token indicating observer-target velocity vector: */


/*     Token indicating observer-target near point vector: */


/*     Token indicating constant vector: */


/*     Token used to build the vector observer keyword: */


/*     Token used to build the vector target keyword: */


/*     Token used to build the vector frame keyword: */


/*     Token used to build the vector aberration correction keyword: */


/*     Token used to build the constant vector specification keyword: */


/*     Token indicating rectangular coordinates used to */
/*     specify constant vector: */


/*     Token indicating latitudinal coordinates used to */
/*     specify constant vector: */


/*     Token indicating RA/DEC coordinates used to */
/*     specify constant vector: */


/*     Token used to build the cartesian vector literal keyword: */


/*     Token used to build the constant vector latitude keyword: */


/*     Token used to build the constant vector longitude keyword: */


/*     Token used to build the constant vector right ascension keyword: */


/*     Token used to build the constant vector declination keyword: */


/*     Token used to build the angular separation tolerance keyword: */


/*     See the section "Physical unit parameters" below for additional */
/*     parameters applicable to two-vector frames. */


/*     Euler frame family parameters */
/*     --------------------------------- */

/*     Token used to build the epoch keyword: */


/*     Token used to build the Euler axis sequence keyword: */


/*     Tokens used to build the Euler angle coefficients keywords: */


/*     See the section "Physical unit parameters" below for additional */
/*     parameters applicable to Euler frames. */


/*     Product frame family parameters */
/*     --------------------------------- */


/*     Physical unit parameters */
/*     --------------------------------- */

/*     Token used to build the units keyword: */


/*     Token indicating radians: */


/*     Token indicating degrees: */


/*     End of include file zzdyn.inc */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  ------------------------------------------------- */
/*     FRNAME     I   Frame name. */
/*     FRCODE     I   Frame ID code. */
/*     ITEM       I   Item associated with frame definition. */
/*     MAXN       I   Maximum number of values to return. */
/*     N          O   Number of returned values. */
/*     VALUES     O   Output kernel variable. */

/* $ Detailed_Input */

/*     FRNAME         is the name of the reference frame with which */
/*                    the requested variable is associated. */

/*     FRCODE         is the frame ID code of the reference frame with */
/*                    which the requested variable is associated. */

/*     ITEM           is a string identifying the specific datum */
/*                    to be fetched.  The kernel variable name */
/*                    has the form */

/*                       FRAME_<frame ID code>_<ITEM> */

/*                    or */

/*                       FRAME_<frame name>_<ITEM> */

/*                    The former of the two names takes precedence: */
/*                    this routine will look for a character variable */
/*                    of that name first. */

/* $ Detailed_Output */

/*     N              is the number of values returned in the array */
/*                    VALUES. */

/*     VALUES         are the values associated with the requested */
/*                    array-valued, character kernel variable. The */
/*                    kernel variable name of the form */

/*                       FRAME_<frame ID code>_<ITEM> */

/*                    will be looked up first; if this variable */
/*                    is found and has character type, the associated */
/*                    values will be returned.  If this variable is */
/*                    not found, the variable */

/*                       FRAME_<frame name>_<ITEM> */

/*                    will be looked up.  If a character variable */
/*                    having that name is found, the associated */
/*                    values will be returned. */

/* $ Parameters */

/*     See zzdyn.inc. */

/* $ Exceptions */

/*     1) If neither the frame-ID-based or frame-name-based form of the */
/*        requested kernel variable name matches a kernel variable */
/*        present in the kernel pool, the error SPICE(KERNELVARNOTFOUND) */
/*        will be signaled. */

/*     2) If either the frame-ID-based or frame-name-based form of the */
/*        requested kernel variable name has length greater than KVNMLN, */
/*        the excessively long name will not be searched for. A search */
/*        will still be done using the alternative form of the name if */
/*        that form has length less than or equal to KVNMLN. */

/*     3) If both the frame-ID-based and frame-name-based forms of the */
/*        requested kernel variable name have length greater than KVNMLN, */
/*        the error SPICE(VARNAMETOOLONG) will be signaled. */

/*     4) If kernel variable matching one form of the requested kernel */
/*        variable names is found, but that variable has numeric data */
/*        type, the error SPICE(BADVARIABLETYPE) will be signaled. */

/*     5) If kernel variable matching one form of the requested kernel */
/*        variable names is found, but that variable has more than MAXN */
/*        associated values, the error SPICE(BADVARIABLESIZE) will be */
/*        signaled. */

/* $ Files */

/*     1) Kernel variables fetched by this routine are normally */
/*        introduced into the kernel pool by loading one or more */
/*        frame kernels.  See the Frames Required Reading for */
/*        details. */

/* $ Particulars */

/*     This routine centralizes logic for kernel variable lookups that */
/*     must be performed by the SPICELIB frame subsystem. Part of the */
/*     functionality of this routine consists of handling error */
/*     conditions such as the unavailability of required kernel */
/*     variables; hence no "found" flag is returned to the caller. */

/*     As indicated above, the requested kernel variable may have a name */
/*     of the form */

/*        FRAME_<frame ID code>_<ITEM> */

/*     or */

/*        FRAME_<frame name>_<ITEM> */

/*     Because most frame definition keywords have the first form, this */
/*     routine looks for a name of that form first. */

/*     Note that although this routine considers the two forms of the */
/*     names to be synonymous, from the point of view of the kernel pool */
/*     data structure, these names are distinct.  Hence kernel variables */
/*     having names of both forms, but having possibly different */
/*     attributes, can be simultaneously present in the kernel pool. */
/*     Intentional use of this kernel pool feature is discouraged. */

/* $ Examples */

/*     See ZZDYNFRM. */

/* $ Restrictions */

/*     1) This is a SPICE private routine; the routine is subject */
/*        to change without notice.  User applications should not */
/*        call this routine. */

/*     2) A scalar-valued kernel variable matching the "ID code form" */
/*        of the requested kernel variable name could potentially */
/*        mask an array-valued kernel variable matching the "name */
/*        form" of the requested name.  This problem can be prevented */
/*        by sensible frame kernel design. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 18-DEC-2004 (NJB) */

/* -& */

/*     SPICELIB functions */


/*     Local Parameters */


/*     TEMPLN is the length of the keyword template, minus */
/*     the sum of the lengths of the two substitution markers ('#'). */


/*     Local Variables */


/*     Standard SPICE error handling */

    if (return_()) {
	return 0;
    }
    chkin_("ZZDYNVAC", (ftnlen)8);

/*     Prepare to check the name of the kernel variable we're about */
/*     to look up. */

/*     Convert the frame code to a string. */

    intstr_(frcode, cdestr, (ftnlen)32);
    if (failed_()) {
	chkout_("ZZDYNVAC", (ftnlen)8);
	return 0;
    }

/*     Get the lengths of the input frame code, name and item. */
/*     Compute the length of the ID-based kernel variable name; */
/*     check this length against the maximum allowed value.  If */
/*     the name is too long, proceed to look up the form of the */
/*     kernel variable name based on the frame name. */

    codeln = rtrim_(cdestr, (ftnlen)32);
    nameln = rtrim_(frname, frname_len);
    itemln = rtrim_(item, item_len);
    reqnum = codeln + itemln + 7;
    if (reqnum <= 32) {

/*        First try looking for a kernel variable including the frame ID */
/*        code. */

/*        Note the template is */

/*            'FRAME_#_#' */

	repmi_("FRAME_#_#", "#", frcode, kvname, (ftnlen)9, (ftnlen)1, (
		ftnlen)32);
	repmc_(kvname, "#", item, kvname, (ftnlen)32, (ftnlen)1, item_len, (
		ftnlen)32);
	dtpool_(kvname, &found, n, dtype, (ftnlen)32, (ftnlen)1);
    } else {

/*        The ID-based name is too long. We can't find the variable if */
/*        we can't look it up. */

	found = FALSE_;
    }
    if (! found) {

/*        We need to look up the frame name-based kernel variable. */
/*        Determine the length of the name of this variable; make */
/*        sure it's not too long. */

	reqnam = nameln + itemln + 7;
	if (reqnam > 32 && reqnum > 32) {

/*           Both forms of the name are too long. */

	    setmsg_("Kernel variable FRAME_#_# has length #; kernel variable"
		    " FRAME_#_# has length #; maximum allowed length is #.  N"
		    "either variable could be searched for in the kernel pool"
		    " due to these name length errors.", (ftnlen)200);
	    errint_("#", frcode, (ftnlen)1);
	    errch_("#", item, (ftnlen)1, item_len);
	    errint_("#", &reqnum, (ftnlen)1);
	    errch_("#", frname, (ftnlen)1, frname_len);
	    errch_("#", item, (ftnlen)1, item_len);
	    errint_("#", &reqnam, (ftnlen)1);
	    errint_("#", &c__32, (ftnlen)1);
	    sigerr_("SPICE(VARNAMETOOLONG)", (ftnlen)21);
	    chkout_("ZZDYNVAC", (ftnlen)8);
	    return 0;
	} else if (reqnam > 32) {

/*           We couldn't find the variable having the ID-based name, */
/*           and the frame name-based variable name is too long to */
/*           look up. */

/*           Note that at this point KVNAME contains the ID-based */
/*           kernel variable name. */

	    setmsg_("Kernel variable # was expected to be present in the ker"
		    "nel pool but was not found.  The alternative form of ker"
		    "nel variable name FRAME_#_# was not searched for because"
		    " this name has excessive length (# characters vs allowed"
		    " maximum of #).  One of these variables is needed to def"
		    "ine the parameterized dynamic frame #.  Usually this typ"
		    "e of problem is due to an error in a frame definition pr"
		    "ovided in a frame kernel.", (ftnlen)416);
	    errch_("#", kvname, (ftnlen)1, (ftnlen)32);
	    errch_("#", frname, (ftnlen)1, frname_len);
	    errch_("#", item, (ftnlen)1, item_len);
	    errint_("#", &reqnam, (ftnlen)1);
	    errint_("#", &c__32, (ftnlen)1);
	    errch_("#", frname, (ftnlen)1, frname_len);
	    sigerr_("SPICE(KERNELVARNOTFOUND)", (ftnlen)24);
	    chkout_("ZZDYNVAC", (ftnlen)8);
	    return 0;
	}

/*        Now try looking for a kernel variable including the frame */
/*        name. */

	repmc_("FRAME_#_#", "#", frname, kvname, (ftnlen)9, (ftnlen)1, 
		frname_len, (ftnlen)32);
	repmc_(kvname, "#", item, kvname, (ftnlen)32, (ftnlen)1, item_len, (
		ftnlen)32);
	dtpool_(kvname, &found, n, dtype, (ftnlen)32, (ftnlen)1);
	if (! found && reqnum > 32) {

/*           The kernel variable's presence (in one form or the other) */
/*           is mandatory:  signal an error.  The error message */
/*           depends on which variables we were able to try to */
/*           look up.  In this case, we never tried to look up the */
/*           frame ID-based name. */

/*           Note that at this point KVNAME contains the name-based */
/*           kernel variable name. */

	    setmsg_("Kernel variable # was expected to be present in the ker"
		    "nel pool but was not found.  The alternative form of ker"
		    "nel variable name FRAME_#_# was not searched for because"
		    " this name has excessive length (# characters vs allowed"
		    " maximum of #).  One of these variables is needed to def"
		    "ine the parameterized dynamic frame #.  Usually this typ"
		    "e of problem is due to an error in a frame definition pr"
		    "ovided in a frame kernel.", (ftnlen)416);
	    errch_("#", kvname, (ftnlen)1, (ftnlen)32);
	    errint_("#", frcode, (ftnlen)1);
	    errch_("#", item, (ftnlen)1, item_len);
	    errint_("#", &reqnum, (ftnlen)1);
	    errint_("#", &c__32, (ftnlen)1);
	    errch_("#", frname, (ftnlen)1, frname_len);
	    sigerr_("SPICE(KERNELVARNOTFOUND)", (ftnlen)24);
	    chkout_("ZZDYNVAC", (ftnlen)8);
	    return 0;
	} else if (! found) {

/*           We tried to look up both names and failed. */

	    setmsg_("At least one of the kernel variables FRAME_#_# or FRAME"
		    "_#_# was expected to be present in the kernel pool but n"
		    "either was found. One of these variables is needed to de"
		    "fine the parameterized dynamic frame #.  Usually this ty"
		    "pe of problem is due to a missing keyword assignment in "
		    "a frame kernel.  Another, less likely, possibility is th"
		    "at other errors in a frame kernel have confused the fram"
		    "e subsystem into wrongly deciding these variables are ne"
		    "eded.", (ftnlen)452);
	    errint_("#", frcode, (ftnlen)1);
	    errch_("#", item, (ftnlen)1, item_len);
	    errch_("#", frname, (ftnlen)1, frname_len);
	    errch_("#", item, (ftnlen)1, item_len);
	    errch_("#", frname, (ftnlen)1, frname_len);
	    sigerr_("SPICE(KERNELVARNOTFOUND)", (ftnlen)24);
	    chkout_("ZZDYNVAC", (ftnlen)8);
	    return 0;
	}
    }

/*     Getting to this point means we found a kernel variable. The name */
/*     of the variable is KVNAME.  The data type is DTYPE and the */
/*     cardinality is N. */

/*     Rather than using BADKPV, we check the data type and cardinality */
/*     of the kernel variable in-line so we can create a more detailed */
/*     error message if need be. */

    if (*(unsigned char *)dtype == 'N') {
	setmsg_("The kernel variable # has used to define frame # was expect"
		"ed to have character data type but in fact has numeric data "
		"type.  Usually this type of problem is due to an error in a "
		"frame definition provided in a frame kernel.", (ftnlen)223);
	errch_("#", kvname, (ftnlen)1, (ftnlen)32);
	errch_("#", frname, (ftnlen)1, frname_len);
	sigerr_("SPICE(BADVARIABLETYPE)", (ftnlen)22);
	chkout_("ZZDYNVAC", (ftnlen)8);
	return 0;
    }
    if (*n > *maxn) {
	setmsg_("The kernel variable # has used to define frame # was expect"
		"ed to have size not exceeding # but in fact has size #. Usua"
		"lly this type of problem is due to an error in a frame defin"
		"ition provided in a frame kernel.", (ftnlen)212);
	errch_("#", kvname, (ftnlen)1, (ftnlen)32);
	errch_("#", frname, (ftnlen)1, frname_len);
	errint_("#", maxn, (ftnlen)1);
	errint_("#", n, (ftnlen)1);
	sigerr_("SPICE(BADVARIABLESIZE)", (ftnlen)22);
	chkout_("ZZDYNVAC", (ftnlen)8);
	return 0;
    }

/*     Look up the kernel variable. */

    gcpool_(kvname, &c__1, maxn, n, values, &found, (ftnlen)32, values_len);
    if (! found) {
	setmsg_("Variable # not found after DTPOOL indicated it was present "
		"in pool.", (ftnlen)67);
	errch_("#", kvname, (ftnlen)1, (ftnlen)32);
	sigerr_("SPICE(BUG)", (ftnlen)10);
	chkout_("ZZDYNVAC", (ftnlen)8);
	return 0;
    }
    chkout_("ZZDYNVAC", (ftnlen)8);
    return 0;
} /* zzdynvac_ */

