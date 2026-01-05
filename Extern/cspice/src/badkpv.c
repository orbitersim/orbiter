/* badkpv.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure BADKPV ( Bad Kernel Pool Variable ) */
logical badkpv_(char *caller, char *name__, char *comp, integer *size, 
	integer *divby, char *type__, ftnlen caller_len, ftnlen name_len, 
	ftnlen comp_len, ftnlen type_len)
{
    /* System generated locals */
    logical ret_val;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    extern logical eqchr_(char *, char *, ftnlen, ftnlen);
    extern /* Subroutine */ int errch_(char *, char *, ftnlen, ftnlen);
    char class__[1];
    logical found;
    integer ratio;
    logical ok;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), dtpool_(char *, logical *, integer *, char *, ftnlen, 
	    ftnlen), setmsg_(char *, ftnlen), errint_(char *, integer *, 
	    ftnlen);
    extern logical return_(void);
    integer dim;

/* $ Abstract */

/*     Determine if a kernel pool variable is present and if so */
/*     that it has the correct size and type. */

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

/*     ERROR */
/*     KERNEL */

/* $ Keywords */

/*     ERROR */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     CALLER     I   Name of the routine calling this routine. */
/*     NAME       I   Name of a kernel pool variable. */
/*     COMP       I   Comparison operator. */
/*     SIZE       I   Expected size of the kernel pool variable. */
/*     DIVBY      I   A divisor of the size of the kernel pool variable. */
/*     TYPE       I   Expected type of the kernel pool variable. */

/*     The function returns .FALSE. if the kernel pool variable is OK. */

/* $ Detailed_Input */

/*     CALLER   is the name of the routine calling this routine */
/*              to check correctness of kernel pool variables. */

/*     NAME     is the name of a kernel pool variable that the */
/*              calling program expects to be present in the */
/*              kernel pool. */

/*     COMP     is the comparison operator to use when comparing */
/*              the number of components of the kernel pool variable */
/*              specified by NAME with the integer SIZE. If DIM is */
/*              is the actual size of the kernel pool variable then */
/*              BADKPV will check that the sentence */

/*                 DIM COMP SIZE */

/*              is a true statement. If it is not a true statement */
/*              an error will be signaled. */

/*              Allowed values for COMP and their meanings are: */

/*                 '='      DIM .EQ. SIZE */
/*                 '<'      DIM .LT. SIZE */
/*                 '>'      DIM .GT. SIZE */
/*                 '=>'     DIM .GE. SIZE */
/*                 '<='     DIM .LE. SIZE */

/*     SIZE     is an integer to compare with the actual */
/*              number of components of the kernel pool variable */
/*              specified by NAME. */

/*     DIVBY    is an integer that is one of the factors of the */
/*              actual dimension of the specified kernel pool variable. */
/*              In other words, it is expected that DIVBY evenly */
/*              divides the actual dimension of NAME. In those */
/*              cases in which the factors of the dimension of NAME */
/*              are not important, set DIVBY to 1 in the calling */
/*              program. */

/*     TYPE     is the expected type of the kernel pool variable. */
/*              Recognized values are */

/*                 'C' for character type */
/*                 'N' for numeric type (integer and double precision) */

/*              The case of TYPE is insignificant. If the value */
/*              of TYPE is not one of the 2 values given above */
/*              no check for the type of the variable will be */
/*              performed. */

/* $ Detailed_Output */

/*     The function returns the value .FALSE. if the kernel pool */
/*     variable has the expected properties. Otherwise the routine */
/*     signals an error and returns the value .TRUE. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the kernel pool variable specified by NAME is not present */
/*         in the kernel pool, the error SPICE(VARIABLENOTFOUND) is */
/*         signaled and the routine will return the value .TRUE. */

/*     2)  If the comparison operator specified by COMP is unrecognized, */
/*         the error SPICE(UNKNOWNCOMPARE) is signaled and the routine */
/*         will return the value .TRUE. */

/*     3)  If the expected type of the kernel pool variable TYPE is not */
/*         one of the supported types, the error SPICE(INVALIDTYPE) is */
/*         signaled and the routine will return the value .TRUE. */

/*     4)  If the comparison of the actual size of the kernel pool */
/*         variable with SIZE is not satisfied, the error */
/*         SPICE(BADVARIABLESIZE) is signaled and the routine will */
/*         return the value .TRUE. */

/*     5)  If the variable does not have the expected type, the error */
/*         SPICE(BADVARIABLETYPE) is signaled and the routine will */
/*         return the value .TRUE. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine takes care of routine checking that often needs */
/*     to be done by programs and routines that rely upon kernel */
/*     pool variables being present and having the correct attributes. */

/*     It checks for the presence of the kernel pool variable and */
/*     examines the type and dimension of the variable to make sure */
/*     they conform to the requirements of the calling routine. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Suppose that you need to fetch a number of variables */
/*        from the kernel pool and want to check that the requested */
/*        items are in fact available prior to performing further */
/*        computations. The code example shows how you might use */
/*        this routine to handle the details of checking of */
/*        the various items. */

/*        Although by default the SPICE error handling system will */
/*        report the error and halt the execution of the program, in */
/*        this example we have decided to change this behavior to */
/*        display the error messages and continue the execution of */
/*        the program. */

/*        Use the kernel shown below to define some variables related */
/*        to the Earth. */


/*           KPL/PCK */

/*           File name: badkpv_ex1.tpc */

/*           The contents of this kernel are not intended for */
/*           real applications. Use only with this example. */

/*           \begindata */

/*              BODY_399_DATA  = ( 3.1416, 2.71828, 0.5, 12.0 ) */
/*              BODY_399_NAMES = ( 'PI', 'E', 'HALF', 'DOZEN' ) */

/*           \begintext */

/*           End of constants kernel */


/*        Example code begins here. */


/*              PROGRAM BADKPV_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     SPICELIB functions. */
/*        C */
/*              LOGICAL               BADKPV */
/*              INTEGER               RTRIM */

/*        C */
/*        C     Local parameters. */
/*        C */
/*              CHARACTER*(*)         CALLER */
/*              PARAMETER           ( CALLER  = 'BADKPV_EX1' ) */

/*              INTEGER               KWDLEN */
/*              PARAMETER           ( KWDLEN = 32 ) */

/*        C */
/*        C     Local variables. */
/*        C */
/*              CHARACTER*2           COMP */
/*              CHARACTER*(KWDLEN)    NAME */
/*              CHARACTER*1           TYPE */

/*              INTEGER               DIVBY */
/*              INTEGER               SIZE */

/*        C */
/*        C     Load the test kernel. */
/*        C */
/*              CALL FURNSH ( 'badkpv_ex1.tpc' ) */

/*        C */
/*        C     Change the default behavior of the SPICE error handling */
/*        C     system to print out all messages and continue the */
/*        C     execution of the program. We do this for demonstration */
/*        C     purposes. Please, refrain from changing the default */
/*        C     behavior on real applications. */
/*        C */
/*              CALL ERRACT ( 'SET', 'REPORT' ) */

/*        C */
/*        C     Assume that we need some data for body 399 and we expect */
/*        C     there to be an even number of items available and at */
/*        C     least 4 such items. Moreover we expect these items to be */
/*        C     numeric. Note that the variable assignments below are */
/*        C     present only to assist in understanding the calls to */
/*        C     BADKPV. */
/*        C */
/*              NAME   = 'BODY_399_DATA' */
/*              COMP   = '=>' */
/*              SIZE   =  4 */
/*              DIVBY  =  2 */
/*              TYPE = 'N' */

/*              IF ( .NOT. BADKPV( CALLER, NAME,  COMP, */
/*             .                   SIZE,   DIVBY, TYPE ) ) THEN */

/*                 WRITE(*,'(3A)') 'Expected form of variable ', */
/*             .                   NAME(:RTRIM(NAME)), */
/*             .                   ' found in kernel pool.' */

/*              END IF */

/*        C */
/*        C     In addition we need the names given to these items. */
/*        C     Improperly indicate the array has type numeric. */
/*        C */
/*              NAME   = 'BODY_399_NAMES' */
/*              COMP   = '=>' */
/*              SIZE   =  4 */
/*              DIVBY  =  1 */
/*              TYPE = 'N' */

/*              IF ( .NOT. BADKPV( CALLER, NAME,  COMP, */
/*             .                   SIZE,   DIVBY, TYPE ) ) THEN */

/*                 WRITE(*,'(3A)') 'Expected form of variable ', */
/*             .                   NAME(:RTRIM(NAME)), */
/*             .                   ' found in kernel pool.' */

/*              END IF */

/*        C */
/*        C     Change the behavior of the SPICE error handling to */
/*        C     its default. */
/*        C */
/*              CALL ERRACT ( 'SET', 'DEFAULT' ) */


/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*        Expected form of variable BODY_399_DATA found in kernel pool. */

/*        ============================================================*** */

/*        Toolkit version: N0066 */

/*        SPICE(BADVARIABLETYPE) -- */

/*        BADKPV_EX1: The kernel pool variable 'BODY_399_NAMES' must b*** */
/*        "NUMERIC". However, the current type is character. */

/*        A traceback follows.  The name of the highest level module i*** */
/*        BADKPV */

/*        ============================================================*** */


/*        Warning: incomplete output. 4 lines extended past the right */
/*        margin of the header and have been truncated. These lines are */
/*        marked by "***" at the end of each line. */


/*        Note that, as expected, the error SPICE(BADVARIABLETYPE) is */
/*        signaled by the second BADKPV call, since we have improperly */
/*        indicated that the requested array is numeric, when actually */
/*        it is of character type. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     B.V. Semenov       (JPL) */
/*     W.L. Taber         (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.2.0, 05-SEP-2021 (JDR) (BVS) */

/*        Fixed typo in long message for the case "comparison not */
/*        favorable." */

/*        Added exception SPICE(INVALIDTYPE) for the case of unknown */
/*        expected kernel pool variable type. */

/*        Edited the header to comply with NAIF standard. Added complete */
/*        code example based on existing fragment. Added required */
/*        readings references. */

/*        Removed references to FURNSH and CLPOOL from the "variable */
/*        not present" long error message. */

/* -    SPICELIB Version 1.1.2, 22-AUG-2006 (EDW) */

/*        Replaced references to LDPOOL with references */
/*        to FURNSH. */

/* -    SPICELIB Version 1.1.1, 10-MAY-2000 (WLT) */

/*        Modified the example section so that it is consistent with */
/*        calling sequence for BADKPV. */

/* -    SPICELIB Version 1.1.0, 26-AUG-1997 (WLT) */

/*        Moved the initial assignment of BADKPV to the lines */
/*        prior to the check of RETURN(). This avoids returning */
/*        without having assigned value to BADKPV. */

/* -    SPICELIB Version 1.0.0, 09-APR-1997 (WLT) */

/* -& */
/* $ Index_Entries */

/*     Check the properties of a kernel pool variable */

/* -& */

/*     SPICELIB Functions */


/*     Local Variables */


/*     Until we know otherwise, we shall assume that we have */
/*     a bad kernel pool variable. */

    ret_val = TRUE_;
    if (return_()) {
	return ret_val;
    }
    chkin_("BADKPV", (ftnlen)6);

/*     Look up the attributes of this variable in the kernel pool. */

    dtpool_(name__, &found, &dim, class__, name_len, (ftnlen)1);
    if (! found) {
	setmsg_("#: The kernel pool variable '#' is not currently present in"
		" the kernel pool. Possible reasons are that the appropriate "
		"text kernel file has not been loaded or that the kernel pool"
		" has been cleared after loading the appropriate text kernel "
		"file. ", (ftnlen)245);
	errch_("#", caller, (ftnlen)1, caller_len);
	errch_("#", name__, (ftnlen)1, name_len);
	sigerr_("SPICE(VARIABLENOTFOUND)", (ftnlen)23);
	chkout_("BADKPV", (ftnlen)6);
	return ret_val;
    }

/*     Compare the dimension of the specified variable with the */
/*     input SIZE. */

    if (s_cmp(comp, "=", comp_len, (ftnlen)1) == 0) {
	ok = dim == *size;
    } else if (s_cmp(comp, "<", comp_len, (ftnlen)1) == 0) {
	ok = dim < *size;
    } else if (s_cmp(comp, ">", comp_len, (ftnlen)1) == 0) {
	ok = dim > *size;
    } else if (s_cmp(comp, "<=", comp_len, (ftnlen)2) == 0) {
	ok = dim <= *size;
    } else if (s_cmp(comp, "=>", comp_len, (ftnlen)2) == 0) {
	ok = dim >= *size;
    } else {
	setmsg_("#: The comparison operator '#' is not a recognized value.  "
		"The recognized values are '<', '<=', '=', '=>', '>'. ", (
		ftnlen)112);
	errch_("#", caller, (ftnlen)1, caller_len);
	errch_("#", comp, (ftnlen)1, comp_len);
	sigerr_("SPICE(UNKNOWNCOMPARE)", (ftnlen)21);
	chkout_("BADKPV", (ftnlen)6);
	return ret_val;
    }

/*     If the comparison was not favorable, signal an error */
/*     and return. */

    if (! ok) {
	setmsg_("#: The kernel pool variable '#' is expected to have a numbe"
		"r of components DIM such that the comparison DIM # # is .TRU"
		"E.  However, the current number of components for '#' is #. ",
		 (ftnlen)179);
	errch_("#", caller, (ftnlen)1, caller_len);
	errch_("#", name__, (ftnlen)1, name_len);
	errch_("#", comp, (ftnlen)1, comp_len);
	errint_("#", size, (ftnlen)1);
	errch_("#", name__, (ftnlen)1, name_len);
	errint_("#", &dim, (ftnlen)1);
	sigerr_("SPICE(BADVARIABLESIZE)", (ftnlen)22);
	chkout_("BADKPV", (ftnlen)6);
	return ret_val;
    }

/*     Check to see that DIVBY evenly divides the dimension of */
/*     the variable. */

    if (*divby != 0) {
	ratio = dim / *divby;
    } else {
	ratio = 1;
    }
    if (*divby * ratio != dim) {
	setmsg_("#: The number of components of the kernel pool variable '#'"
		" is required to be divisible by #.  However, the actual numb"
		"er of components is # which is not evenly divisible by #. ", (
		ftnlen)177);
	errch_("#", caller, (ftnlen)1, caller_len);
	errch_("#", name__, (ftnlen)1, name_len);
	errint_("#", divby, (ftnlen)1);
	errint_("#", &dim, (ftnlen)1);
	errint_("#", divby, (ftnlen)1);
	sigerr_("SPICE(BADVARIABLESIZE)", (ftnlen)22);
	chkout_("BADKPV", (ftnlen)6);
	return ret_val;
    }

/*     Finally check the type of the variable. */

    if (eqchr_(type__, "C", type_len, (ftnlen)1)) {
	if (*(unsigned char *)class__ != 'C') {
	    setmsg_("#: The kernel pool variable '#' must be of type \"CHARA"
		    "CTER\". However, the current type is numeric. ", (ftnlen)
		    99);
	    errch_("#", caller, (ftnlen)1, caller_len);
	    errch_("#", name__, (ftnlen)1, name_len);
	    sigerr_("SPICE(BADVARIABLETYPE)", (ftnlen)22);
	    chkout_("BADKPV", (ftnlen)6);
	    return ret_val;
	}
    } else if (eqchr_(type__, "N", type_len, (ftnlen)1)) {
	if (*(unsigned char *)class__ != 'N') {
	    setmsg_("#: The kernel pool variable '#' must be of type \"NUMER"
		    "IC\".  However, the current type is character. ", (ftnlen)
		    100);
	    errch_("#", caller, (ftnlen)1, caller_len);
	    errch_("#", name__, (ftnlen)1, name_len);
	    sigerr_("SPICE(BADVARIABLETYPE)", (ftnlen)22);
	    chkout_("BADKPV", (ftnlen)6);
	    return ret_val;
	}
    } else {
	setmsg_("#: Unknown expected type of the kernel pool variable '#'. T"
		"he expected type of the kernel pool variable must be either "
		"'C' or 'N'.", (ftnlen)130);
	errch_("#", caller, (ftnlen)1, caller_len);
	errch_("#", type__, (ftnlen)1, type_len);
	sigerr_("SPICE(INVALIDTYPE)", (ftnlen)18);
	chkout_("BADKPV", (ftnlen)6);
	return ret_val;
    }
    ret_val = FALSE_;
    chkout_("BADKPV", (ftnlen)6);
    return ret_val;
} /* badkpv_ */

