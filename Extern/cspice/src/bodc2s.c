/* bodc2s.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure BODC2S ( Body ID code to string translation ) */
/* Subroutine */ int bodc2s_(integer *code, char *name__, ftnlen name_len)
{
    extern /* Subroutine */ int zzbodc2n_(integer *, char *, logical *, 
	    ftnlen), chkin_(char *, ftnlen);
    logical found;
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    extern logical return_(void);
    extern /* Subroutine */ int intstr_(integer *, char *, ftnlen);

/* $ Abstract */

/*     Translate a body ID code to either the corresponding name */
/*     or if no name to ID code mapping exists, the string */
/*     representation of the body ID value. */

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
/*     CODE       I   Integer ID code to translate to a string. */
/*     NAME       O   String corresponding to CODE. */

/* $ Detailed_Input */

/*     CODE     is the integer code for a body: planet, satellite, */
/*              barycenter, spacecraft, asteroid, comet, or */
/*              other ephemeris object. */

/* $ Detailed_Output */

/*     NAME     is the string name of the body identified by CODE */
/*              if a mapping between CODE and a body name exists */
/*              within SPICE. */

/*              If CODE has more than one translation, then the */
/*              most recently defined NAME corresponding to CODE */
/*              is returned. NAME will have the exact format (case */
/*              and blanks) as when the name/code pair was defined. */

/*              If the input value of CODE does not map to a body */
/*              name, NAME returns the string representation */
/*              of CODE. */

/* $ Parameters */

/*     MAXL     is the maximum allowable length of a body name. The */
/*              current value of this parameter is 36. See the SPICELIB */
/*              include file zzbodtrn.inc for details. */

/* $ Exceptions */

/*     1)  If there is any problem with the body name-ID mapping kernel */
/*         variables present in the kernel pool, an error is signaled by */
/*         a routine in the call tree of this routine. */

/* $ Files */

/*     Body-name mappings may be defined at run time by loading text */
/*     kernels containing kernel variable assignments of the form */

/*        NAIF_BODY_NAME += ( <name 1>, ... ) */
/*        NAIF_BODY_CODE += ( <code 1>, ... ) */

/*     See naif_ids.req for details. */

/* $ Particulars */

/*     BODC2S is one of five related subroutines, */

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

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Apply the BODC2S call to several IDs representing codes */
/*        included in the default SPICE ID-name lists and codes not */
/*        included in the list. */

/*        Example code begins here. */


/*              PROGRAM BODC2S_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Local variables. */
/*        C */
/*              CHARACTER*(32)         NAME */
/*              INTEGER                CODE (7) */
/*              INTEGER                I */

/*        C */
/*        C     Assign an array of body IDs. Not all the listed IDs */
/*        C     map to a body name. */
/*        C */
/*              CODE(1) = 399 */
/*              CODE(2) = 0 */
/*              CODE(3) = 3 */
/*              CODE(4) = -77 */
/*              CODE(5) = 11 */
/*              CODE(6) = -1 */
/*              CODE(7) = 6000001 */

/*        C */
/*        C     Loop over the CODE array, call BODC2S for each */
/*        C     element of CODE. */
/*        C */
/*              WRITE(*,*) 'Code      Name' */
/*              WRITE(*,*) '-------   -----------------------' */

/*              DO I= 1, 7 */

/*                 CALL BODC2S( CODE(I), NAME ) */

/*                 WRITE(*, '(I8,3x,A)' ) CODE(I),  NAME */

/*              END DO */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*         Code      Name */
/*         -------   ----------------------- */
/*             399   EARTH */
/*               0   SOLAR SYSTEM BARYCENTER */
/*               3   EARTH BARYCENTER */
/*             -77   GALILEO ORBITER */
/*              11   11 */
/*              -1   GEOTAIL */
/*         6000001   6000001 */


/*        Note that the codes 11 and 6000001 did not map to a name so the */
/*        call returns as NAME the string expression of the codes. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     B.V. Semenov       (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 07-JUL-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Fixed minor */
/*        typos in header. Added description of MAXL parameter. Added */
/*        $Exceptions and $Restrictions. */

/* -    SPICELIB Version 1.0.1, 18-APR-2014 (BVS) */

/*        Minor header edits. */

/* -    SPICELIB Version 1.0.0, 10-APR-2010 (EDW) */

/* -& */
/* $ Index_Entries */

/*     body ID code to string */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard SPICELIB error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("BODC2S", (ftnlen)6);

/*     Fortran. No type check available for CODE. Bother. */


/*     Attempt to translate the input CODE to a name. Use */
/*     the private routine ZZBODC2N. */

    zzbodc2n_(code, name__, &found, name_len);
    if (found) {

/*        Success. CODE maps to NAME. Return. */

	chkout_("BODC2S", (ftnlen)6);
	return 0;
    }

/*     If execution reaches this level, the SPICE body ID */
/*     to name mapping lacks an assignment for CODE. Convert */
/*     CODE to a string representation of the integer value. */

    intstr_(code, name__, name_len);
    chkout_("BODC2S", (ftnlen)6);
    return 0;
} /* bodc2s_ */

