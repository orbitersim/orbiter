/* zzprsmet.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static integer c__100 = 100;

/* $Procedure ZZPRSMET ( Private: parse method string ) */
/* Subroutine */ int zzprsmet_(integer *bodyid, char *method, integer *mxnsrf,
	 char *shape, char *subtyp, logical *pri, integer *nsurf, integer *
	srflst, char *pntdef, char *trmtyp, ftnlen method_len, ftnlen 
	shape_len, ftnlen subtyp_len, ftnlen pntdef_len, ftnlen trmtyp_len)
{
    /* System generated locals */
    integer i__1, i__2, i__3, i__4, i__5, i__6, i__7, i__8, i__9;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_rnge(char *, integer, char *, integer), s_cmp(char *, char *, 
	    ftnlen, ftnlen);

    /* Local variables */
    integer code, begs[102];
    logical done;
    integer ends[102], npri;
    extern /* Subroutine */ int zzlexmet_(char *, integer *, integer *, 
	    integer *, integer *, ftnlen);
    integer i__, n;
    extern /* Subroutine */ int chkin_(char *, ftnlen), errch_(char *, char *,
	     ftnlen, ftnlen);
    extern logical eqstr_(char *, char *, ftnlen, ftnlen), failed_(void);
    integer nshape;
    extern /* Subroutine */ int srfscc_(char *, integer *, integer *, logical 
	    *, ftnlen);
    char locmth[1000];
    extern /* Subroutine */ int sigerr_(char *, ftnlen);
    integer nlmbtp;
    extern /* Subroutine */ int chkout_(char *, ftnlen), ljucrs_(integer *, 
	    char *, char *, ftnlen, ftnlen), setmsg_(char *, ftnlen), errint_(
	    char *, integer *, ftnlen);
    integer nsrfls, nsubtp;
    extern logical return_(void);
    integer ntrmtp;
    logical fnd;
    char chr[1];

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due to the */
/*     volatile nature of this routine. */

/*     Parse a method input string passed by a caller to a SPICE */
/*     high-level geometry API. Return parameter values specified by the */
/*     string. */

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

/*     DSK */

/* $ Keywords */

/*     SCANNING */
/*     PARSING */
/*     CONSTANTS */
/*     TOPOGRAPHY */
/*     UTILITY */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     BODYID     I   Body ID code. */
/*     METHOD     I   Method string. */
/*     MXNSRF     I   Maximum number of surface IDs that can be returned. */
/*     SHAPE      O   Target shape. */
/*     SUBTYP     O   Sub-point type. */
/*     PRI        O   Prioritization flag. */
/*     NSURF      O   Number of surface IDs in list. */
/*     SRFLST     O   Surface ID list. */
/*     PNTDEF     O   Limb or terminator point definition. */
/*     TRMTYP     O   Terminator type. */
/*     MAXSRF     P   DSK subsystem maximum surface list size. */

/* $ Detailed_Input */

/*     BODYID     is the body ID code of an ephemeris object. This */
/*                object is normally the "target" body of a geometric */
/*                computation. Any surfaces specified in the method */
/*                string are associated with this body. BODYID is */
/*                needed to map surface names to surface ID codes. */

/*     METHOD     is a "method" string to be parsed. This string */
/*                normally is passed by a caller to a SPICE high-level */
/*                high-level geometry routine. */

/*                METHOD indicates whether a target body surface is */
/*                to be modeled as an ellipsoid or by DSK data. It */
/*                may contain additional, computation-dependent */
/*                parameters. */

/*     MXNSRF     is the maximum number of surface IDs that can be */
/*                returned in the output array SRFLST. Normally */
/*                the caller should use the parameter MAXSRF to */
/*                set this value. */


/* $ Detailed_Output */

/*     SHAPE      is a string describing the shape model. Values are */

/*                    'ELLIPSOID' or 'DSK' */


/*     SUBTYP     is a string describing a sub-observer or sub-solar */
/*                point definition. Values are */

/*                    'INTERCEPT' or 'NEAR POINT' */


/*     PRI        is a logical flag indicating whether a DSK-based */
/*                computation uses DSK segments in a prioritized */
/*                fashion. PRI is .TRUE. if and only if the data */
/*                usage is prioritized. Note that for the N0066 */
/*                version of SPICE, all DSK-based computations */
/*                use unprioritized data. */


/*     NSURF      is the number of elements of the surface ID list. */


/*     SRFLST     is a list of surface ID codes. All surfaces are */
/*                associated with the body designated by BODYID. */


/*     PNTDEF     is a string describing the point definition used */
/*                to compute limb or terminator points. Values are */

/*                   'TANGENT' or 'GUIDED' */


/*     TRMTYP     is a string that indicates the definition of the */
/*                terminator on the target body. Values are: */

/*                   'UMBRAL' or 'PENUMBRAL' */

/* $ Parameters */

/*     MAXSRF is the maximum surface list size used by the DSK */
/*     subsystem. See dsk.inc for the parameter's value. */

/* $ Exceptions */

/*     1)  If more than one shape keyword is found in METHOD, */
/*         the error SPICE(BADMETHODSYNTAX) is signaled. */

/*     2)  If more than one surface keyword is found in METHOD, */
/*         the error SPICE(BADMETHODSYNTAX) is signaled. */

/*     3)  If the surface keyword is found in METHOD, but no */
/*         surfaces are listed, the error SPICE(BADMETHODSYNTAX) is */
/*         signaled. */

/*     4)  If the equals sign ('=') is missing from the surface list, */
/*         the error SPICE(BADMETHODSYNTAX) is signaled. */

/*     5)  If the method string contains a double quote character (") */
/*         that doesn't delimit a quoted-string, the error */
/*          SPICE(BADMETHODSYNTAX) is signaled. */

/*     6)  If a surface name in the METHOD string can't be converted */
/*         to a surface ID code, the error SPICE(IDCODENOTFOUND) is */
/*         signaled. */

/*     7)  If all ID codes in the surface list can't be stored in the */
/*         SRFLST array, the error SPICE(TOOMANYSURFACES) is signaled. */

/*     8)  If the surface list in METHOD contains a comma not followed */
/*         by a surface name, the error SPICE(BADMETHODSYNTAX) is */
/*         signaled. */

/*     9)  If the surface list in METHOD contains an unrecognized token, */
/*         the error SPICE(BADMETHODSYNTAX) is signaled. */

/*    10)  If the string METHOD contains an extra prioritization token, */
/*         the error SPICE(BADMETHODSYNTAX) is signaled. */

/*    12)  If the string METHOD contains an extra sub-point type token, */
/*         the error SPICE(BADMETHODSYNTAX) is signaled. */

/*    13)  If the string METHOD contains an extra point definition */
/*         token, the error SPICE(BADMETHODSYNTAX) is signaled. */

/*    14)  If the string METHOD contains an extra terminator type token, */
/*         the error SPICE(BADMETHODSYNTAX) is signaled. */

/*    15)  If the string METHOD contains an extra slash character, */
/*         the error SPICE(BADMETHODSYNTAX) is signaled. */

/*    16)  If the string METHOD lacks a prioritization keyword, */
/*         the error SPICE(BADPRIORITYSPEC) is signaled. */

/*    17)  If a "legacy" method string is not one of the expected */
/*         values, the error SPICE(BADMETHODSYNTAX) is signaled. */

/*    18)  If the string METHOD contains any other unexpected tokens, */
/*         the error SPICE(BADMETHODSYNTAX) is signaled. */

/* $ Files */

/*     Surface name-to-ID mappings may be defined at run time by loading */
/*     text kernels containing kernel variable assignments of the form */

/*        NAIF_SURFACE_NAME += ( <surface name 1>, ... ) */
/*        NAIF_SURFACE_CODE += ( <surface code 1>, ... ) */
/*        NAIF_SURFACE_BODY += ( <body code 1>,    ... ) */

/*     Above, the Ith elements of the lists on the assignments' right */
/*     hand sides together define the Ith surface name/ID mapping. */

/*     The same effect can be achieved using assignments formatted as */
/*     follows: */

/*        NAIF_SURFACE_NAME += <surface name 1> */
/*        NAIF_SURFACE_CODE += <surface code 1> */
/*        NAIF_SURFACE_BODY += <body code 1> */

/*        NAIF_SURFACE_NAME += <surface name 2> */
/*        NAIF_SURFACE_CODE += <surface code 2> */
/*        NAIF_SURFACE_BODY += <body code 2> */

/*           ... */

/*     Note the use of the */

/*        += */

/*     operator; this operator appends to rather than overwrites the */
/*     kernel variable named on the left hand side of the assignment. */


/* $ Particulars */

/*     This routine supports parsing of the METHOD input argument */
/*     used by the SPICE geometry APIs */

/*        GFOCLT */
/*        ILLUMF */
/*        ILLUMG */
/*        ILUMIN */
/*        LATSRF */
/*        LIMBPT */
/*        SINCPT */
/*        SUBPNT */
/*        SUBSLR */
/*        TERMPT */

/*     Each API has its own syntax for its METHOD input argument. */
/*     All routines use a common syntax for surface lists. See */
/*     the headers of the routines for the exact syntax supported */
/*     by each routine. */

/*     In the METHOD string, the token delimiters are */

/*        / = , <blank> */

/*     Strings delimited by double quote characters are treated as */
/*     individual tokens. There is no escape syntax for indicating a */
/*     doubly quoted string within a doubly quoted string. */

/* $ Examples */


/*     1) For the routine SUBPNT, */

/*           Method string = */

/*              METHOD = 'INTERCEPT / ' */
/*              //       'DSK/UNPRIORITIZED/SURFACES = "MGS ' */
/*              //       'MOLA 128 PIXEL/DEG", MARS_LOWRES' */

/*          Output parameters: */

/*             SHAPE  = 'DSK' */
/*             SUBTYP = 'INTERCEPT' */
/*             PRI    = .FALSE. */
/*             NSURF  = 2 */
/*             SRFLST = <ID code 1>, <ID code 2> */
/*             PNTDEF = ' ' */
/*             TRMTYP = ' ' */


/*     2) For the routine TERMPT, */

/*           Method string = */

/*              METHOD = 'UMBRAL/TANGENT/ ' */
/*              //       'DSK/UNPRIORITIZED/SURFACES = "MGS ' */
/*              //       'MOLA 128 PIXEL/DEG", MARS_LOWRES' */

/*          Output parameters: */

/*             SHAPE  = 'DSK' */
/*             SUBTYP = ' ' */
/*             PRI    = .FALSE. */
/*             NSURF  = 2 */
/*             SRFLST = <ID code 1>, <ID code 2> */
/*             PNTDEF = 'TANGENT' */
/*             TRMTYP = 'UMBRAL' */


/* $ Restrictions */

/*     This is a specialized parsing routine. It it meant to be used */
/*     only for the parsing "method" strings as described in */
/*     Particulars. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 24-FEB-2016 (NJB) */

/*        Based on version  4.0.0 12-NOV-2015 (NJB) */
/*        First version was 1.0.0 14-JAN-2015 (NJB) */

/* -& */
/* $ Index_Entries */

/*     parse method string for geometry api routines */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */

    if (return_()) {
	return 0;
    }
    chkin_("ZZPRSMET", (ftnlen)8);

/*     No shape or surfaces have been specified yet. */

    s_copy(shape, " ", shape_len, (ftnlen)1);
    s_copy(subtyp, " ", subtyp_len, (ftnlen)1);
    s_copy(pntdef, " ", pntdef_len, (ftnlen)1);
    s_copy(trmtyp, " ", trmtyp_len, (ftnlen)1);
    *pri = TRUE_;
    *nsurf = 0;

/*     Initialize clause counts. */

    npri = 0;
    nshape = 0;
    nsubtp = 0;
    nlmbtp = 0;
    ntrmtp = 0;
    nsrfls = 0;
    *(unsigned char *)locmth = '/';
    begs[0] = 1;
    ends[0] = 1;
    ljucrs_(&c__1, method, locmth + 1, method_len, (ftnlen)999);

/*     Tokenize the input method string. */

    zzlexmet_(locmth + 1, &c__100, &n, &begs[1], &ends[1], (ftnlen)999);
    if (failed_()) {
	chkout_("ZZPRSMET", (ftnlen)8);
	return 0;
    }
    ++n;
    i__1 = n;
    for (i__ = 2; i__ <= i__1; ++i__) {
	begs[(i__2 = i__ - 1) < 102 && 0 <= i__2 ? i__2 : s_rnge("begs", i__2,
		 "zzprsmet_", (ftnlen)437)] = begs[(i__3 = i__ - 1) < 102 && 
		0 <= i__3 ? i__3 : s_rnge("begs", i__3, "zzprsmet_", (ftnlen)
		437)] + 1;
	ends[(i__2 = i__ - 1) < 102 && 0 <= i__2 ? i__2 : s_rnge("ends", i__2,
		 "zzprsmet_", (ftnlen)438)] = ends[(i__3 = i__ - 1) < 102 && 
		0 <= i__3 ? i__3 : s_rnge("ends", i__3, "zzprsmet_", (ftnlen)
		438)] + 1;
    }

/*     Identify the target shape. */

    done = FALSE_;
    i__ = 1;
    while(! done) {
	i__1 = begs[(i__2 = i__ - 1) < 102 && 0 <= i__2 ? i__2 : s_rnge("begs"
		, i__2, "zzprsmet_", (ftnlen)449)] - 1;
	if (s_cmp(locmth + i__1, "ELLIPSOID", ends[(i__3 = i__ - 1) < 102 && 
		0 <= i__3 ? i__3 : s_rnge("ends", i__3, "zzprsmet_", (ftnlen)
		449)] - i__1, (ftnlen)9) == 0) {
	    ++nshape;
	    s_copy(shape, "ELLIPSOID", shape_len, (ftnlen)9);
	} else /* if(complicated condition) */ {
	    i__1 = begs[(i__2 = i__ - 1) < 102 && 0 <= i__2 ? i__2 : s_rnge(
		    "begs", i__2, "zzprsmet_", (ftnlen)454)] - 1;
	    if (s_cmp(locmth + i__1, "DSK", ends[(i__3 = i__ - 1) < 102 && 0 
		    <= i__3 ? i__3 : s_rnge("ends", i__3, "zzprsmet_", (
		    ftnlen)454)] - i__1, (ftnlen)3) == 0) {
		++nshape;
		s_copy(shape, "DSK", shape_len, (ftnlen)3);
	    }
	}
	if (i__ == n) {
	    done = TRUE_;
	} else {
	    ++i__;
	}
    }
    if (nshape != 1) {
	setmsg_("The \"method\" or \"shape\" string must contain exactly one"
		" instance of a shape keyword: ELLIPSOID or DSK, or additiona"
		"lly, in the case of occultation computations, POINT. The num"
		"ber of shape keywords was #. The input string was <#>.", (
		ftnlen)229);
	errint_("#", &nshape, (ftnlen)1);
	errch_("#", method, (ftnlen)1, method_len);
	sigerr_("SPICE(BADMETHODSYNTAX)", (ftnlen)22);
	chkout_("ZZPRSMET", (ftnlen)8);
	return 0;
    }
    if (s_cmp(shape, "DSK", shape_len, (ftnlen)3) == 0) {

/*        Traverse the tokens; identify clauses in the method string. */

	i__ = 1;
	while(i__ < n) {
	    i__1 = begs[(i__2 = i__ - 1) < 102 && 0 <= i__2 ? i__2 : s_rnge(
		    "begs", i__2, "zzprsmet_", (ftnlen)496)] - 1;
	    s_copy(chr, locmth + i__1, (ftnlen)1, ends[(i__3 = i__ - 1) < 102 
		    && 0 <= i__3 ? i__3 : s_rnge("ends", i__3, "zzprsmet_", (
		    ftnlen)496)] - i__1);
	    if (*(unsigned char *)chr == '/') {

/*              If the method string is correct, the next token should */
/*              be a keyword. */

		++i__;
		i__1 = begs[(i__2 = i__ - 1) < 102 && 0 <= i__2 ? i__2 : 
			s_rnge("begs", i__2, "zzprsmet_", (ftnlen)505)] - 1;
		if (s_cmp(locmth + i__1, "SURFACES", ends[(i__3 = i__ - 1) < 
			102 && 0 <= i__3 ? i__3 : s_rnge("ends", i__3, "zzpr"
			"smet_", (ftnlen)505)] - i__1, (ftnlen)8) == 0) {

/*                 Normally, we're looking at the start of a surface */
/*                 list at this point. We need at least an assignment */
/*                 operator ('=') and a surface name or ID code following */
/*                 the SURFACES keyword. */

		    ++nsrfls;
		    if (nsrfls > 1) {

/*                    We have too many surface specifications. */

			setmsg_("Extra SURFACES keyword was found in the met"
				"hod string <#>.", (ftnlen)58);
			errch_("#", method, (ftnlen)1, method_len);
			sigerr_("SPICE(BADMETHODSYNTAX)", (ftnlen)22);
			chkout_("ZZPRSMET", (ftnlen)8);
			return 0;
		    }
		    if (i__ > n - 2) {
			setmsg_("The surface list in the method string appea"
				"rs to be truncated. The method string is <#>."
				, (ftnlen)88);
			errch_("#", method, (ftnlen)1, method_len);
			sigerr_("SPICE(BADMETHODSYNTAX)", (ftnlen)22);
			chkout_("ZZPRSMET", (ftnlen)8);
			return 0;
		    }
		    ++i__;
		    i__1 = begs[(i__2 = i__ - 1) < 102 && 0 <= i__2 ? i__2 : 
			    s_rnge("begs", i__2, "zzprsmet_", (ftnlen)541)] - 
			    1;
		    s_copy(chr, locmth + i__1, (ftnlen)1, ends[(i__3 = i__ - 
			    1) < 102 && 0 <= i__3 ? i__3 : s_rnge("ends", 
			    i__3, "zzprsmet_", (ftnlen)541)] - i__1);
		    if (*(unsigned char *)chr != '=') {
			setmsg_("The surface list in the method string lacks"
				" an \"=\" sign after the SURFACES keyword. T"
				"he method string is <#>.", (ftnlen)109);
			errch_("#", method, (ftnlen)1, method_len);
			sigerr_("SPICE(BADMETHODSYNTAX)", (ftnlen)22);
			chkout_("ZZPRSMET", (ftnlen)8);
			return 0;
		    }

/*                 Prepare to read a list of names, quoted */
/*                 strings, or ID codes. The list must be non-empty. If */
/*                 there are multiple list entries, they're delimited by */
/*                 commas. */

		    ++i__;
		    done = FALSE_;
		    while(! done) {
			i__1 = begs[(i__2 = i__ - 1) < 102 && 0 <= i__2 ? 
				i__2 : s_rnge("begs", i__2, "zzprsmet_", (
				ftnlen)567)] - 1;
			s_copy(chr, locmth + i__1, (ftnlen)1, begs[(i__3 = 
				i__ - 1) < 102 && 0 <= i__3 ? i__3 : s_rnge(
				"begs", i__3, "zzprsmet_", (ftnlen)567)] - 
				i__1);
			if (*(unsigned char *)chr == '"') {

/*                       We expect the current token to be a quoted */
/*                       string. */

			    if (ends[(i__1 = i__ - 1) < 102 && 0 <= i__1 ? 
				    i__1 : s_rnge("ends", i__1, "zzprsmet_", (
				    ftnlen)574)] > begs[(i__2 = i__ - 1) < 
				    102 && 0 <= i__2 ? i__2 : s_rnge("begs", 
				    i__2, "zzprsmet_", (ftnlen)574)] + 1) {
				begs[(i__1 = i__ - 1) < 102 && 0 <= i__1 ? 
					i__1 : s_rnge("begs", i__1, "zzprsme"
					"t_", (ftnlen)576)] = begs[(i__2 = i__ 
					- 1) < 102 && 0 <= i__2 ? i__2 : 
					s_rnge("begs", i__2, "zzprsmet_", (
					ftnlen)576)] + 1;
				ends[(i__1 = i__ - 1) < 102 && 0 <= i__1 ? 
					i__1 : s_rnge("ends", i__1, "zzprsme"
					"t_", (ftnlen)577)] = ends[(i__2 = i__ 
					- 1) < 102 && 0 <= i__2 ? i__2 : 
					s_rnge("ends", i__2, "zzprsmet_", (
					ftnlen)577)] - 1;
			    } else {

/*                          We have an invalid quoted string. */

				setmsg_("The surface list in the method stri"
					"ng contains a double quote character"
					" that is not a delimiter of a valid "
					"quoted string. The method string is "
					"<#>.", (ftnlen)147);
				errch_("#", method, (ftnlen)1, method_len);
				sigerr_("SPICE(BADMETHODSYNTAX)", (ftnlen)22);
				chkout_("ZZPRSMET", (ftnlen)8);
				return 0;
			    }
			}
/*                    We have either a name or an integer in string form. */
/*                    Convert the token to a surface ID code. */

			i__1 = begs[(i__2 = i__ - 1) < 102 && 0 <= i__2 ? 
				i__2 : s_rnge("begs", i__2, "zzprsmet_", (
				ftnlen)602)] - 1;
			srfscc_(locmth + i__1, bodyid, &code, &fnd, ends[(
				i__3 = i__ - 1) < 102 && 0 <= i__3 ? i__3 : 
				s_rnge("ends", i__3, "zzprsmet_", (ftnlen)602)
				] - i__1);
			if (failed_()) {
			    chkout_("ZZPRSMET", (ftnlen)8);
			    return 0;
			}
			if (! fnd) {
			    setmsg_("The surface name <#> could not be trans"
				    "lated to an ID code. The method string i"
				    "s <#>.", (ftnlen)85);
			    i__1 = begs[(i__2 = i__ - 1) < 102 && 0 <= i__2 ? 
				    i__2 : s_rnge("begs", i__2, "zzprsmet_", (
				    ftnlen)615)] - 1;
			    errch_("#", locmth + i__1, (ftnlen)1, ends[(i__3 =
				     i__ - 1) < 102 && 0 <= i__3 ? i__3 : 
				    s_rnge("ends", i__3, "zzprsmet_", (ftnlen)
				    615)] - i__1);
			    errch_("#", method, (ftnlen)1, method_len);
			    sigerr_("SPICE(IDCODENOTFOUND)", (ftnlen)21);
			    chkout_("ZZPRSMET", (ftnlen)8);
			    return 0;
			}
			++(*nsurf);

/*                    Make sure there's room in the surface ID array. */

			if (*nsurf > *mxnsrf) {
			    setmsg_("The surface name <#> could not be store"
				    "d in the surface list due to lack of roo"
				    "m. The maximum number of surfaces that c"
				    "an be specified is #. The method string "
				    "is <#>.", (ftnlen)166);
			    i__1 = begs[(i__2 = i__ - 1) < 102 && 0 <= i__2 ? 
				    i__2 : s_rnge("begs", i__2, "zzprsmet_", (
				    ftnlen)636)] - 1;
			    errch_("#", locmth + i__1, (ftnlen)1, ends[(i__3 =
				     i__ - 1) < 102 && 0 <= i__3 ? i__3 : 
				    s_rnge("ends", i__3, "zzprsmet_", (ftnlen)
				    636)] - i__1);
			    errint_("#", mxnsrf, (ftnlen)1);
			    errch_("#", method, (ftnlen)1, method_len);
			    sigerr_("SPICE(TOOMANYSURFACES)", (ftnlen)22);
			    chkout_("ZZPRSMET", (ftnlen)8);
			    return 0;
			}

/*                    Append the surface ID to the surface ID array. */

			srflst[*nsurf - 1] = code;
			if (i__ == n) {

/*                       We're at the end of the method string. */

			    done = TRUE_;
			} else {

/*                       There are more tokens; the surface list may */
/*                       contain more surface names or ID codes. */

			    ++i__;
			    i__1 = begs[(i__2 = i__ - 1) < 102 && 0 <= i__2 ? 
				    i__2 : s_rnge("begs", i__2, "zzprsmet_", (
				    ftnlen)663)] - 1;
			    s_copy(chr, locmth + i__1, (ftnlen)1, ends[(i__3 =
				     i__ - 1) < 102 && 0 <= i__3 ? i__3 : 
				    s_rnge("ends", i__3, "zzprsmet_", (ftnlen)
				    663)] - i__1);
			    if (*(unsigned char *)chr == '/') {

/*                          We're at the end of the surface list. */

				done = TRUE_;

/*                          Decrement the token pointer so that the */
/*                          slash will be seen as the next token. */

				--i__;
			    } else if (*(unsigned char *)chr == ',') {

/*                          We expect to find another surface name or */
/*                          ID in the list. */

				if (i__ < n) {
				    ++i__;
				} else {

/*                             We have a syntax error in the surface */
/*                             list. */

				    setmsg_("The surface list in the method "
					    "string contains a comma that is "
					    "not followed by a surface name o"
					    "r ID code. The method string is "
					    "<#>.", (ftnlen)131);
				    errch_("#", method, (ftnlen)1, method_len)
					    ;
				    sigerr_("SPICE(BADMETHODSYNTAX)", (ftnlen)
					    22);
				    chkout_("ZZPRSMET", (ftnlen)8);
				    return 0;
				}
			    } else {

/*                          We have a syntax error in the surface list. */

				setmsg_("The surface list in the method stri"
					"ng is followed by the unexpected tok"
					"en <#>. The method string is <#>.", (
					ftnlen)104);
				i__1 = begs[(i__2 = i__ - 1) < 102 && 0 <= 
					i__2 ? i__2 : s_rnge("begs", i__2, 
					"zzprsmet_", (ftnlen)713)] - 1;
				errch_("#", locmth + i__1, (ftnlen)1, ends[(
					i__3 = i__ - 1) < 102 && 0 <= i__3 ? 
					i__3 : s_rnge("ends", i__3, "zzprsme"
					"t_", (ftnlen)713)] - i__1);
				errch_("#", method, (ftnlen)1, method_len);
				sigerr_("SPICE(BADMETHODSYNTAX)", (ftnlen)22);
				chkout_("ZZPRSMET", (ftnlen)8);
				return 0;
			    }
			}
		    }
		} else /* if(complicated condition) */ {
		    i__1 = begs[(i__2 = i__ - 1) < 102 && 0 <= i__2 ? i__2 : 
			    s_rnge("begs", i__2, "zzprsmet_", (ftnlen)726)] - 
			    1;
		    if (s_cmp(locmth + i__1, "UNPRIORITIZED", ends[(i__3 = 
			    i__ - 1) < 102 && 0 <= i__3 ? i__3 : s_rnge("ends"
			    , i__3, "zzprsmet_", (ftnlen)726)] - i__1, (
			    ftnlen)13) == 0) {
			++npri;
			if (npri > 1) {

/*                    We have too many prioritization specifications. */

			    setmsg_("Extra prioritization keyword was found "
				    "in the method string <#>.", (ftnlen)64);
			    errch_("#", method, (ftnlen)1, method_len);
			    sigerr_("SPICE(BADMETHODSYNTAX)", (ftnlen)22);
			    chkout_("ZZPRSMET", (ftnlen)8);
			    return 0;
			}
			*pri = FALSE_;
		    } else /* if(complicated condition) */ {
			i__1 = begs[(i__2 = i__ - 1) < 102 && 0 <= i__2 ? 
				i__2 : s_rnge("begs", i__2, "zzprsmet_", (
				ftnlen)749)] - 1;
			i__4 = begs[(i__5 = i__ - 1) < 102 && 0 <= i__5 ? 
				i__5 : s_rnge("begs", i__5, "zzprsmet_", (
				ftnlen)749)] - 1;
			if (s_cmp(locmth + i__1, "INTERCEPT", ends[(i__3 = 
				i__ - 1) < 102 && 0 <= i__3 ? i__3 : s_rnge(
				"ends", i__3, "zzprsmet_", (ftnlen)749)] - 
				i__1, (ftnlen)9) == 0 || s_cmp(locmth + i__4, 
				"NADIR", ends[(i__6 = i__ - 1) < 102 && 0 <= 
				i__6 ? i__6 : s_rnge("ends", i__6, "zzprsmet_"
				, (ftnlen)749)] - i__4, (ftnlen)5) == 0) {

/*                 This is a sub-point type specification. */

			    ++nsubtp;
			    if (nsubtp > 1) {

/*                    We have too many sub-point specifications. */

				setmsg_("Extra sub-point type <#> was found "
					"in the method string <#>.", (ftnlen)
					60);
				i__1 = begs[(i__2 = i__ - 1) < 102 && 0 <= 
					i__2 ? i__2 : s_rnge("begs", i__2, 
					"zzprsmet_", (ftnlen)764)] - 1;
				errch_("#", locmth + i__1, (ftnlen)1, ends[(
					i__3 = i__ - 1) < 102 && 0 <= i__3 ? 
					i__3 : s_rnge("ends", i__3, "zzprsme"
					"t_", (ftnlen)764)] - i__1);
				errch_("#", method, (ftnlen)1, method_len);
				sigerr_("SPICE(BADMETHODSYNTAX)", (ftnlen)22);
				chkout_("ZZPRSMET", (ftnlen)8);
				return 0;
			    }
			    i__1 = begs[(i__2 = i__ - 1) < 102 && 0 <= i__2 ? 
				    i__2 : s_rnge("begs", i__2, "zzprsmet_", (
				    ftnlen)772)] - 1;
			    s_copy(subtyp, locmth + i__1, subtyp_len, ends[(
				    i__3 = i__ - 1) < 102 && 0 <= i__3 ? i__3 
				    : s_rnge("ends", i__3, "zzprsmet_", (
				    ftnlen)772)] - i__1);
			} else /* if(complicated condition) */ {
			    i__1 = begs[(i__2 = i__ - 1) < 102 && 0 <= i__2 ? 
				    i__2 : s_rnge("begs", i__2, "zzprsmet_", (
				    ftnlen)775)] - 1;
			    i__4 = begs[(i__5 = i__ - 1) < 102 && 0 <= i__5 ? 
				    i__5 : s_rnge("begs", i__5, "zzprsmet_", (
				    ftnlen)775)] - 1;
			    if (s_cmp(locmth + i__1, "TANGENT", ends[(i__3 = 
				    i__ - 1) < 102 && 0 <= i__3 ? i__3 : 
				    s_rnge("ends", i__3, "zzprsmet_", (ftnlen)
				    775)] - i__1, (ftnlen)7) == 0 || s_cmp(
				    locmth + i__4, "GUIDED", ends[(i__6 = i__ 
				    - 1) < 102 && 0 <= i__6 ? i__6 : s_rnge(
				    "ends", i__6, "zzprsmet_", (ftnlen)775)] 
				    - i__4, (ftnlen)6) == 0) {

/*                 This is a point definition specification. */

				++nlmbtp;
				if (nlmbtp > 1) {

/*                    We have too many point definition specifications. */

				    setmsg_("Extra point definition <#> was "
					    "found in the method string <#>.", 
					    (ftnlen)62);
				    i__1 = begs[(i__2 = i__ - 1) < 102 && 0 <=
					     i__2 ? i__2 : s_rnge("begs", 
					    i__2, "zzprsmet_", (ftnlen)790)] 
					    - 1;
				    errch_("#", locmth + i__1, (ftnlen)1, 
					    ends[(i__3 = i__ - 1) < 102 && 0 
					    <= i__3 ? i__3 : s_rnge("ends", 
					    i__3, "zzprsmet_", (ftnlen)790)] 
					    - i__1);
				    errch_("#", method, (ftnlen)1, method_len)
					    ;
				    sigerr_("SPICE(BADMETHODSYNTAX)", (ftnlen)
					    22);
				    chkout_("ZZPRSMET", (ftnlen)8);
				    return 0;
				}
				i__1 = begs[(i__2 = i__ - 1) < 102 && 0 <= 
					i__2 ? i__2 : s_rnge("begs", i__2, 
					"zzprsmet_", (ftnlen)798)] - 1;
				s_copy(pntdef, locmth + i__1, pntdef_len, 
					ends[(i__3 = i__ - 1) < 102 && 0 <= 
					i__3 ? i__3 : s_rnge("ends", i__3, 
					"zzprsmet_", (ftnlen)798)] - i__1);
			    } else /* if(complicated condition) */ {
				i__1 = begs[(i__2 = i__ - 1) < 102 && 0 <= 
					i__2 ? i__2 : s_rnge("begs", i__2, 
					"zzprsmet_", (ftnlen)802)] - 1;
				i__4 = begs[(i__5 = i__ - 1) < 102 && 0 <= 
					i__5 ? i__5 : s_rnge("begs", i__5, 
					"zzprsmet_", (ftnlen)802)] - 1;
				if (s_cmp(locmth + i__1, "UMBRAL", ends[(i__3 
					= i__ - 1) < 102 && 0 <= i__3 ? i__3 :
					 s_rnge("ends", i__3, "zzprsmet_", (
					ftnlen)802)] - i__1, (ftnlen)6) == 0 
					|| s_cmp(locmth + i__4, "PENUMBRAL", 
					ends[(i__6 = i__ - 1) < 102 && 0 <= 
					i__6 ? i__6 : s_rnge("ends", i__6, 
					"zzprsmet_", (ftnlen)802)] - i__4, (
					ftnlen)9) == 0) {

/*                 This is a terminator type specification. */

				    ++ntrmtp;
				    if (ntrmtp > 1) {

/*                    We have too many terminator type specifications. */

					setmsg_("Extra terminator type <#> w"
						"as found in the method strin"
						"g <#>.", (ftnlen)61);
					i__1 = begs[(i__2 = i__ - 1) < 102 && 
						0 <= i__2 ? i__2 : s_rnge(
						"begs", i__2, "zzprsmet_", (
						ftnlen)817)] - 1;
					errch_("#", locmth + i__1, (ftnlen)1, 
						ends[(i__3 = i__ - 1) < 102 &&
						 0 <= i__3 ? i__3 : s_rnge(
						"ends", i__3, "zzprsmet_", (
						ftnlen)817)] - i__1);
					errch_("#", method, (ftnlen)1, 
						method_len);
					sigerr_("SPICE(BADMETHODSYNTAX)", (
						ftnlen)22);
					chkout_("ZZPRSMET", (ftnlen)8);
					return 0;
				    }
				    i__1 = begs[(i__2 = i__ - 1) < 102 && 0 <=
					     i__2 ? i__2 : s_rnge("begs", 
					    i__2, "zzprsmet_", (ftnlen)825)] 
					    - 1;
				    s_copy(trmtyp, locmth + i__1, trmtyp_len, 
					    ends[(i__3 = i__ - 1) < 102 && 0 
					    <= i__3 ? i__3 : s_rnge("ends", 
					    i__3, "zzprsmet_", (ftnlen)825)] 
					    - i__1);
				} else /* if(complicated condition) */ {
				    i__1 = begs[(i__2 = i__ - 1) < 102 && 0 <=
					     i__2 ? i__2 : s_rnge("begs", 
					    i__2, "zzprsmet_", (ftnlen)829)] 
					    - 1;
				    if (s_cmp(locmth + i__1, "/", ends[(i__3 =
					     i__ - 1) < 102 && 0 <= i__3 ? 
					    i__3 : s_rnge("ends", i__3, "zzp"
					    "rsmet_", (ftnlen)829)] - i__1, (
					    ftnlen)1) == 0) {

/*                 Adjust the token index in the message to account */
/*                 for the insertion of a slash at the beginning. */

					setmsg_("An unexpected slash charact"
						"er was found at index # in t"
						"he method string <#>.", (
						ftnlen)76);
					i__2 = begs[(i__1 = i__ - 1) < 102 && 
						0 <= i__1 ? i__1 : s_rnge(
						"begs", i__1, "zzprsmet_", (
						ftnlen)837)] - 1;
					errint_("#", &i__2, (ftnlen)1);
					errch_("#", method, (ftnlen)1, 
						method_len);
					sigerr_("SPICE(BADMETHODSYNTAX)", (
						ftnlen)22);
					chkout_("ZZPRSMET", (ftnlen)8);
					return 0;
				    } else /* if(complicated condition) */ {
					i__1 = begs[(i__2 = i__ - 1) < 102 && 
						0 <= i__2 ? i__2 : s_rnge(
						"begs", i__2, "zzprsmet_", (
						ftnlen)844)] - 1;
					if (s_cmp(locmth + i__1, "DSK", ends[(
						i__3 = i__ - 1) < 102 && 0 <= 
						i__3 ? i__3 : s_rnge("ends", 
						i__3, "zzprsmet_", (ftnlen)
						844)] - i__1, (ftnlen)3) != 0)
						 {

/*                 'DSK' was the only valid token that could appear */
/*                 at this point. */

					    setmsg_("Unexpected token <#> wa"
						    "s found in the method st"
						    "ring <#>.", (ftnlen)56);
					    i__1 = begs[(i__2 = i__ - 1) < 
						    102 && 0 <= i__2 ? i__2 : 
						    s_rnge("begs", i__2, 
						    "zzprsmet_", (ftnlen)851)]
						     - 1;
					    errch_("#", locmth + i__1, (
						    ftnlen)1, ends[(i__3 = 
						    i__ - 1) < 102 && 0 <= 
						    i__3 ? i__3 : s_rnge(
						    "ends", i__3, "zzprsmet_",
						     (ftnlen)851)] - i__1);
					    errch_("#", method, (ftnlen)1, 
						    method_len);
					    sigerr_("SPICE(BADMETHODSYNTAX)", 
						    (ftnlen)22);
					    chkout_("ZZPRSMET", (ftnlen)8);
					    return 0;
					}
				    }
				}
			    }
			}
		    }
		}
	    }
	    ++i__;
	}

/*        Currently, the UNPRIORITIZED keyword must be provided */
/*        for DSK shapes. */

	if (*pri) {
	    setmsg_("The keyword UNPRIORITIZED must be present in the method"
		    " string <#> when the target shape is DSK. Prioritized DS"
		    "K segment searches are not currently supported.", (ftnlen)
		    158);
	    errch_("#", method, (ftnlen)1, method_len);
	    sigerr_("SPICE(BADPRIORITYSPEC)", (ftnlen)22);
	    chkout_("ZZPRSMET", (ftnlen)8);
	    return 0;
	}
    } else if (s_cmp(shape, "ELLIPSOID", shape_len, (ftnlen)9) == 0) {

/*        Check for legacy string inputs. */

	if (eqstr_(method, "ELLIPSOID", method_len, (ftnlen)9)) {

/*           There are no other outputs to set. */

	    chkout_("ZZPRSMET", (ftnlen)8);
	    return 0;
	} else if (eqstr_(method, "NEARPOINT:ELLIPSOID", method_len, (ftnlen)
		19) || eqstr_(method, "ELLIPSOID:NEARPOINT", method_len, (
		ftnlen)19) || eqstr_(method, "NEARPOINT/ELLIPSOID", 
		method_len, (ftnlen)19) || eqstr_(method, "ELLIPSOID/NEARPOI"
		"NT", method_len, (ftnlen)19)) {
	    s_copy(subtyp, "NEAR POINT", subtyp_len, (ftnlen)10);
	    chkout_("ZZPRSMET", (ftnlen)8);
	    return 0;
	} else if (eqstr_(method, "INTERCEPT:ELLIPSOID", method_len, (ftnlen)
		19) || eqstr_(method, "ELLIPSOID:INTERCEPT", method_len, (
		ftnlen)19) || eqstr_(method, "INTERCEPT/ELLIPSOID", 
		method_len, (ftnlen)19) || eqstr_(method, "ELLIPSOID/INTERCE"
		"PT", method_len, (ftnlen)19)) {
	    s_copy(subtyp, "INTERCEPT", subtyp_len, (ftnlen)9);
	    chkout_("ZZPRSMET", (ftnlen)8);
	    return 0;
	}

/*        At this point, we should have a "modern" style of method */
/*        specification for an ellipsoidal shape model. */
/*        Parse the method string. */

/*        Traverse the tokens; identify clauses in the method string. */

	i__ = 1;
	while(i__ <= n) {
	    i__1 = begs[(i__2 = i__ - 1) < 102 && 0 <= i__2 ? i__2 : s_rnge(
		    "begs", i__2, "zzprsmet_", (ftnlen)929)] - 1;
	    s_copy(chr, locmth + i__1, (ftnlen)1, ends[(i__3 = i__ - 1) < 102 
		    && 0 <= i__3 ? i__3 : s_rnge("ends", i__3, "zzprsmet_", (
		    ftnlen)929)] - i__1);
	    if (*(unsigned char *)chr == '/') {

/*              If the method string is correct, the next token should */
/*              be a keyword. There had better be a next token. */

		if (i__ == n) {
		    setmsg_("Expected more tokens after final <#> in the met"
			    "hod string <#>.", (ftnlen)62);
		    i__1 = begs[(i__2 = i__ - 1) < 102 && 0 <= i__2 ? i__2 : 
			    s_rnge("begs", i__2, "zzprsmet_", (ftnlen)940)] - 
			    1;
		    errch_("#", locmth + i__1, (ftnlen)1, ends[(i__3 = i__ - 
			    1) < 102 && 0 <= i__3 ? i__3 : s_rnge("ends", 
			    i__3, "zzprsmet_", (ftnlen)940)] - i__1);
		    errch_("#", method, (ftnlen)1, method_len);
		    sigerr_("SPICE(BADMETHODSYNTAX)", (ftnlen)22);
		    chkout_("ZZPRSMET", (ftnlen)8);
		    return 0;
		}
		++i__;
		i__1 = begs[(i__2 = i__ - 1) < 102 && 0 <= i__2 ? i__2 : 
			s_rnge("begs", i__2, "zzprsmet_", (ftnlen)951)] - 1;
		if (s_cmp(locmth + i__1, "ELLIPSOID", ends[(i__3 = i__ - 1) < 
			102 && 0 <= i__3 ? i__3 : s_rnge("ends", i__3, "zzpr"
			"smet_", (ftnlen)951)] - i__1, (ftnlen)9) == 0) {

/*                 This case is a no-op. */

		} else /* if(complicated condition) */ {
		    i__1 = begs[(i__2 = i__ - 1) < 102 && 0 <= i__2 ? i__2 : 
			    s_rnge("begs", i__2, "zzprsmet_", (ftnlen)956)] - 
			    1;
		    i__4 = begs[(i__5 = i__ - 1) < 102 && 0 <= i__5 ? i__5 : 
			    s_rnge("begs", i__5, "zzprsmet_", (ftnlen)956)] - 
			    1;
		    i__7 = begs[(i__8 = i__ - 1) < 102 && 0 <= i__8 ? i__8 : 
			    s_rnge("begs", i__8, "zzprsmet_", (ftnlen)956)] - 
			    1;
		    if (s_cmp(locmth + i__1, "INTERCEPT", ends[(i__3 = i__ - 
			    1) < 102 && 0 <= i__3 ? i__3 : s_rnge("ends", 
			    i__3, "zzprsmet_", (ftnlen)956)] - i__1, (ftnlen)
			    9) == 0 || s_cmp(locmth + i__4, "NADIR", ends[(
			    i__6 = i__ - 1) < 102 && 0 <= i__6 ? i__6 : 
			    s_rnge("ends", i__6, "zzprsmet_", (ftnlen)956)] - 
			    i__4, (ftnlen)5) == 0 || s_cmp(locmth + i__7, 
			    "NEAR", ends[(i__9 = i__ - 1) < 102 && 0 <= i__9 ?
			     i__9 : s_rnge("ends", i__9, "zzprsmet_", (ftnlen)
			    956)] - i__7, (ftnlen)4) == 0) {

/*                 This is a sub-point type specification. */

			++nsubtp;
			if (nsubtp > 1) {

/*                    We have too many sub-point specifications. */

			    setmsg_("Extra sub-point type <#> was found in t"
				    "he method string <#>.", (ftnlen)60);
			    i__1 = begs[(i__2 = i__ - 1) < 102 && 0 <= i__2 ? 
				    i__2 : s_rnge("begs", i__2, "zzprsmet_", (
				    ftnlen)973)] - 1;
			    errch_("#", locmth + i__1, (ftnlen)1, ends[(i__3 =
				     i__ - 1) < 102 && 0 <= i__3 ? i__3 : 
				    s_rnge("ends", i__3, "zzprsmet_", (ftnlen)
				    973)] - i__1);
			    errch_("#", method, (ftnlen)1, method_len);
			    sigerr_("SPICE(BADMETHODSYNTAX)", (ftnlen)22);
			    chkout_("ZZPRSMET", (ftnlen)8);
			    return 0;
			}
			i__1 = begs[(i__2 = i__ - 1) < 102 && 0 <= i__2 ? 
				i__2 : s_rnge("begs", i__2, "zzprsmet_", (
				ftnlen)981)] - 1;
			if (s_cmp(locmth + i__1, "NEAR", ends[(i__3 = i__ - 1)
				 < 102 && 0 <= i__3 ? i__3 : s_rnge("ends", 
				i__3, "zzprsmet_", (ftnlen)981)] - i__1, (
				ftnlen)4) == 0) {

/*                    This may be a combination of old style and */
/*                    new syntax, e.g. 'ELLIPSOID/NEAR POINT'. */

			    if (s_cmp(shape, "ELLIPSOID", shape_len, (ftnlen)
				    9) == 0 && i__ < n) {

/*                       We allow the "near point" sub-point type */
/*                       for ellipsoids but not for DSK surfaces. */

				i__1 = begs[(i__2 = i__) < 102 && 0 <= i__2 ? 
					i__2 : s_rnge("begs", i__2, "zzprsme"
					"t_", (ftnlen)992)] - 1;
				if (s_cmp(locmth + i__1, "POINT", ends[(i__3 =
					 i__) < 102 && 0 <= i__3 ? i__3 : 
					s_rnge("ends", i__3, "zzprsmet_", (
					ftnlen)992)] - i__1, (ftnlen)5) == 0) 
					{
				    s_copy(subtyp, "NADIR", subtyp_len, (
					    ftnlen)5);
				}
			    }
			} else {
			    i__1 = begs[(i__2 = i__ - 1) < 102 && 0 <= i__2 ? 
				    i__2 : s_rnge("begs", i__2, "zzprsmet_", (
				    ftnlen)1002)] - 1;
			    s_copy(subtyp, locmth + i__1, subtyp_len, ends[(
				    i__3 = i__ - 1) < 102 && 0 <= i__3 ? i__3 
				    : s_rnge("ends", i__3, "zzprsmet_", (
				    ftnlen)1002)] - i__1);
			}
		    } else /* if(complicated condition) */ {
			i__1 = begs[(i__2 = i__ - 1) < 102 && 0 <= i__2 ? 
				i__2 : s_rnge("begs", i__2, "zzprsmet_", (
				ftnlen)1009)] - 1;
			i__4 = begs[(i__5 = i__ - 1) < 102 && 0 <= i__5 ? 
				i__5 : s_rnge("begs", i__5, "zzprsmet_", (
				ftnlen)1009)] - 1;
			if (s_cmp(locmth + i__1, "TANGENT", ends[(i__3 = i__ 
				- 1) < 102 && 0 <= i__3 ? i__3 : s_rnge("ends"
				, i__3, "zzprsmet_", (ftnlen)1009)] - i__1, (
				ftnlen)7) == 0 || s_cmp(locmth + i__4, "GUID"
				"ED", ends[(i__6 = i__ - 1) < 102 && 0 <= i__6 
				? i__6 : s_rnge("ends", i__6, "zzprsmet_", (
				ftnlen)1009)] - i__4, (ftnlen)6) == 0) {

/*                 This is a point definition specification. */

			    ++nlmbtp;
			    if (nlmbtp > 1) {

/*                    We have too many point definition specifications. */

				setmsg_("Extra point definition <#> was foun"
					"d in the method string <#>.", (ftnlen)
					62);
				i__1 = begs[(i__2 = i__ - 1) < 102 && 0 <= 
					i__2 ? i__2 : s_rnge("begs", i__2, 
					"zzprsmet_", (ftnlen)1025)] - 1;
				errch_("#", locmth + i__1, (ftnlen)1, ends[(
					i__3 = i__ - 1) < 102 && 0 <= i__3 ? 
					i__3 : s_rnge("ends", i__3, "zzprsme"
					"t_", (ftnlen)1025)] - i__1);
				errch_("#", method, (ftnlen)1, method_len);
				sigerr_("SPICE(BADMETHODSYNTAX)", (ftnlen)22);
				chkout_("ZZPRSMET", (ftnlen)8);
				return 0;
			    }
			    i__1 = begs[(i__2 = i__ - 1) < 102 && 0 <= i__2 ? 
				    i__2 : s_rnge("begs", i__2, "zzprsmet_", (
				    ftnlen)1033)] - 1;
			    s_copy(pntdef, locmth + i__1, pntdef_len, ends[(
				    i__3 = i__ - 1) < 102 && 0 <= i__3 ? i__3 
				    : s_rnge("ends", i__3, "zzprsmet_", (
				    ftnlen)1033)] - i__1);
			} else /* if(complicated condition) */ {
			    i__1 = begs[(i__2 = i__ - 1) < 102 && 0 <= i__2 ? 
				    i__2 : s_rnge("begs", i__2, "zzprsmet_", (
				    ftnlen)1035)] - 1;
			    i__4 = begs[(i__5 = i__ - 1) < 102 && 0 <= i__5 ? 
				    i__5 : s_rnge("begs", i__5, "zzprsmet_", (
				    ftnlen)1035)] - 1;
			    if (s_cmp(locmth + i__1, "UMBRAL", ends[(i__3 = 
				    i__ - 1) < 102 && 0 <= i__3 ? i__3 : 
				    s_rnge("ends", i__3, "zzprsmet_", (ftnlen)
				    1035)] - i__1, (ftnlen)6) == 0 || s_cmp(
				    locmth + i__4, "PENUMBRAL", ends[(i__6 = 
				    i__ - 1) < 102 && 0 <= i__6 ? i__6 : 
				    s_rnge("ends", i__6, "zzprsmet_", (ftnlen)
				    1035)] - i__4, (ftnlen)9) == 0) {

/*                 This is a terminator type specification. */

				++ntrmtp;
				if (ntrmtp > 1) {

/*                    We have too many terminator type specifications. */

				    setmsg_("Extra terminator type <#> was f"
					    "ound in the method string <#>.", (
					    ftnlen)61);
				    i__1 = begs[(i__2 = i__ - 1) < 102 && 0 <=
					     i__2 ? i__2 : s_rnge("begs", 
					    i__2, "zzprsmet_", (ftnlen)1050)] 
					    - 1;
				    errch_("#", locmth + i__1, (ftnlen)1, 
					    ends[(i__3 = i__ - 1) < 102 && 0 
					    <= i__3 ? i__3 : s_rnge("ends", 
					    i__3, "zzprsmet_", (ftnlen)1050)] 
					    - i__1);
				    errch_("#", method, (ftnlen)1, method_len)
					    ;
				    sigerr_("SPICE(BADMETHODSYNTAX)", (ftnlen)
					    22);
				    chkout_("ZZPRSMET", (ftnlen)8);
				    return 0;
				}
				i__1 = begs[(i__2 = i__ - 1) < 102 && 0 <= 
					i__2 ? i__2 : s_rnge("begs", i__2, 
					"zzprsmet_", (ftnlen)1058)] - 1;
				s_copy(trmtyp, locmth + i__1, trmtyp_len, 
					ends[(i__3 = i__ - 1) < 102 && 0 <= 
					i__3 ? i__3 : s_rnge("ends", i__3, 
					"zzprsmet_", (ftnlen)1058)] - i__1);
			    } else {

/*                 Sorry, no other strings are allowed. */

				setmsg_("Unexpected method string <#> was fo"
					"und specifying an ellipsoid shape.", (
					ftnlen)69);
				errch_("#", method, (ftnlen)1, method_len);
				sigerr_("SPICE(BADMETHODSYNTAX)", (ftnlen)22);
				chkout_("ZZPRSMET", (ftnlen)8);
				return 0;
			    }
			}
		    }
		}
	    } else {
		setmsg_("Unexpected method string <#> was found specifying a"
			"n ellipsoid shape.", (ftnlen)69);
		errch_("#", method, (ftnlen)1, method_len);
		sigerr_("SPICE(BADMETHODSYNTAX)", (ftnlen)22);
		chkout_("ZZPRSMET", (ftnlen)8);
		return 0;
	    }
	    ++i__;
	}
    } else {

/*        This is a backstop error check. */

	setmsg_("Unexpected shape value # was found. This is due to a coding"
		" error, not to user input.", (ftnlen)85);
	errch_("#", shape, (ftnlen)1, shape_len);
	sigerr_("SPICE(BUG)", (ftnlen)10);
	chkout_("ZZPRSMET", (ftnlen)8);
	return 0;
    }
    chkout_("ZZPRSMET", (ftnlen)8);
    return 0;
} /* zzprsmet_ */

