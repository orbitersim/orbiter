/* bods2c.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure BODS2C ( Body string to ID code translation ) */
/* Subroutine */ int bods2c_(char *name__, integer *code, logical *found, 
	ftnlen name_len)
{
    extern /* Subroutine */ int zzbodn2c_(char *, integer *, logical *, 
	    ftnlen), chkin_(char *, ftnlen);
    extern logical beint_(char *, ftnlen);
    extern /* Subroutine */ int nparsi_(char *, integer *, char *, integer *, 
	    ftnlen, ftnlen), chkout_(char *, ftnlen);
    char errmsg[1];
    extern logical return_(void);
    integer ptr;

/* $ Abstract */

/*     Translate a string containing a body name or ID code to an */
/*     integer code. */

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

/*     NAIF_IDS */

/* $ Keywords */

/*     BODY */
/*     CONVERSION */
/*     ID */
/*     NAME */
/*     UTILITY */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     NAME       I   String to be translated to an ID code. */
/*     CODE       O   Integer ID code corresponding to NAME. */
/*     FOUND      O   Flag indicating whether translation succeeded. */

/* $ Detailed_Input */

/*     NAME     is a string containing the name or ID code of a */
/*              body or object, such as a planet, satellite, comet, */
/*              asteroid, barycenter, DSN station, spacecraft, or */
/*              instrument. */

/*              If NAME contains the name of a body or object, that */
/*              name must be "known" to the SPICE system, whether */
/*              through hard-coded registration or run-time */
/*              registration in the SPICE kernel pool. */

/*              Case and leading and trailing blanks in a name are */
/*              not significant. However when a name is made up of */
/*              more than one word, adjacent words must be separated */
/*              by at least one blank. That is, all of the following */
/*              strings are equivalent names: */

/*                 'JUPITER BARYCENTER' */
/*                 'Jupiter Barycenter' */
/*                 'JUPITER BARYCENTER   ' */
/*                 'JUPITER    BARYCENTER' */
/*                 '   JUPITER BARYCENTER' */

/*              However, 'JUPITERBARYCENTER' is not equivalent to */
/*              the names above. */

/*              If NAME is a string representation of an integer, */
/*              for example */

/*                 '399' */

/*              the string will be translated to the equivalent */
/*              INTEGER datum. The input integer need not be one */
/*              recognized by the SPICE system: the integer need not */
/*              be a built-in NAIF ID code, nor need it be associated */
/*              with a name via run-time registration. */

/* $ Detailed_Output */

/*     CODE     is, if NAME contains the name of a body or object, */
/*              the corresponding NAIF or user-defined integer ID */
/*              code, as determined by the SPICE name-code mapping */
/*              subsystem. If NAME represents an integer, the same */
/*              integer is returned in CODE. */

/*              CODE is assigned a value only if FOUND is returned */
/*              as .TRUE.; otherwise it is returned unchanged. */

/*     FOUND    is .TRUE. if NAME has a translation or represents an */
/*              integer within the bounds of representable integers */
/*              as defined by the SPICELIB routines INTMAX and INTMIN. */
/*              Otherwise, FOUND is .FALSE. */

/* $ Parameters */

/*     MAXL     is the maximum allowable length of a body name. The */
/*              current value of this parameter is 36. See the SPICELIB */
/*              include file zzbodtrn.inc for details. */

/* $ Exceptions */

/*     1)  If there is any problem with the body name-ID mapping kernel */
/*         variables present in the kernel pool, an error is signaled by */
/*         a routine in the call tree of this routine. */

/*     2)  Body name strings are upper-cased, their leading and trailing */
/*         blanks removed, and embedded blanks are compressed out, after */
/*         which they get truncated to the maximum body name length MAXL. */
/*         Therefore, two body names that differ only after that maximum */
/*         length are considered equal. */

/* $ Files */

/*     Body-name mappings may be defined at run time by loading text */
/*     kernels containing kernel variable assignments of the form */

/*        NAIF_BODY_NAME += ( <name 1>, ... ) */
/*        NAIF_BODY_CODE += ( <code 1>, ... ) */

/*     See naif_ids.req for details. */

/* $ Particulars */

/*     BODS2C is one of five related subroutines, */

/*        BODS2C      Body string to code */
/*        BODC2S      Body code to string */
/*        BODN2C      Body name to code */
/*        BODC2N      Body code to name */
/*        BODDEF      Body name/code definition */

/*     BODS2C, BODC2S, BODN2C, and BODC2N perform translations between */
/*     body names and their corresponding integer ID codes which are */
/*     used in SPICE files and routines. */

/*     BODS2C is a slightly more general version of BODN2C: support */
/*     for strings containing ID codes in string format enables a caller */
/*     to identify a body using a string, even when no name is */
/*     associated with that body. */

/*     BODC2S is a general version of BODC2N; the routine returns either */
/*     the name assigned in the body ID to name mapping or a string */
/*     representation of the CODE value if no mapping exists. */

/*     BODDEF assigns a body name to ID mapping. The mapping has */
/*     priority in name-to-ID and ID-to-name translations. */

/*     Refer to naif_ids.req for the list of name/code associations built */
/*     into SPICE, and for details concerning adding new name/code */
/*     associations at run time by loading text kernels. */

/* $ Examples */

/*     1)  In the following code fragment, BODEUL returns the Euler */
/*         angles representing the orientation of Jupiter relative to */
/*         the J2000 reference frame. BODEUL requires the NAIF integer */
/*         ID code for Jupiter, so we use BODS2C to convert the name to */
/*         its corresponding integer ID code. */

/*         We know Jupiter has a built-in name-code mapping, so we */
/*         needn't check the FOUND flag. */

/*            CALL BODS2C ( 'JUPITER', JUPID, FOUND ) */

/*            CALL BODEUL ( JUPID, ET, RA, DEC, W, LAMBDA ) */


/*     2)  In this example, we assume that only the set of default */
/*         name/code pairs has been defined. */

/*         Given these names, BODS2C will return the following codes: */

/*            Name                             Code    Found? */
/*            ------------------------       ------    ------ */
/*            'EARTH'                           399    Yes */
/*            '  Earth '                        399    Yes */
/*            '399'                             399    Yes */
/*            ' 399 '                           399    Yes */
/*            'EMB'                               3    Yes */
/*            '3'                                 3    Yes */
/*            '1000000000'               1000000000    Yes */
/*            'Solar System Barycenter'           0    Yes */
/*            'SolarSystemBarycenter'             -    No */
/*            'SSB'                               0    Yes */
/*            'Voyager 2'                       -32    Yes */
/*            'U.S.S. Enterprise'                 -    No */
/*            ' '                                 -    No */
/*            'Halley's Comet'                    -    No */

/*         Given these codes, BODC2N will return the following names: */

/*            Code        Name                        Found? */
/*            -------     -------------------         ------ */
/*            399         'EARTH'                     Yes */
/*              0         'SOLAR SYSTEM BARYCENTER'   Yes */
/*              3         'EARTH BARYCENTER'          Yes */
/*            -77         'GALILEO ORBITER'           Yes */
/*             11          -                          No */
/*     1000000000          -                          No */

/* $ Restrictions */

/*     1)  See exception <2>. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     C.H. Acton         (JPL) */
/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */
/*     B.V. Semenov       (JPL) */
/*     F.S. Turner        (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 07-AUG-2020 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Updated output argument FOUND description in $Detailed_Output. */

/*        Edited the header to comply with NAIF standard. Fixed minor */
/*        typos in header. Added description of MAXL parameter. Added */
/*        $Exceptions and $Restrictions. */

/* -    SPICELIB Version 1.0.2, 16-MAY-2009 (EDW) */

/*        Edit to $Particulars section to document the BODC2S routine. */

/* -    SPICELIB Version 1.0.1, 28-FEB-2008 (BVS) */

/*        Corrected the contents of the $Required_Reading section. */

/* -    SPICELIB Version 1.0.0, 23-JUL-2003 (CHA) (NJB) (KRG) (FST) (EDW) */

/* -& */
/* $ Index_Entries */

/*     body string to code */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */


/*     Standard SPICELIB error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("BODS2C", (ftnlen)6);

/*     Attempt to translate the input name to an integer code.  Call */
/*     the private routine ZZBODN2C to avoid additional CHKIN and */
/*     CHKOUT calls. */

    zzbodn2c_(name__, code, found, name_len);
    if (! (*found)) {

/*        It's possible the name is a string representation */
/*        of an integer, for example, '999'.  If so, find */
/*        the equivalent datum of INTEGER type. */

	if (beint_(name__, name_len)) {

/*           The input conforms to the syntax of an integer, but it may */
/*           be outside of the range of the INTEGER data type. */
/*           Therefore we use the non-error-signaling routine NPARSI */
/*           rather than the cleaner PRSINT to attempt to convert the */
/*           string to an INTEGER. */

	    nparsi_(name__, code, errmsg, &ptr, name_len, (ftnlen)1);

/*           We have an ID code if and only if PTR is zero. */

	    *found = ptr == 0;
	}
    }

/*     FOUND is set.  CODE is set if NAME was a recognized name */
/*     or a string representation of an integer. */

    chkout_("BODS2C", (ftnlen)6);
    return 0;
} /* bods2c_ */

