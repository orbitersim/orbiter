/* cleard.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure CLEARD ( Clear a double precision array ) */
/* Subroutine */ int cleard_(integer *ndim, doublereal *array)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    integer i__;

/* $ Abstract */

/*     Fill a double precision array with zeros. */

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

/*     ARRAY */
/*     ASSIGNMENT */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     NDIM       I   The number of elements of ARRAY which are to be */
/*                    set to zero. */
/*     ARRAY      O   Double precision array to be filled. */

/* $ Detailed_Input */

/*     NDIM     is the number of elements in ARRAY which are to be */
/*              set to zero. */

/* $ Detailed_Output */

/*     ARRAY    is the double precision array which is to be filled */
/*              with zeros. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1)  If NDIM < 1, the array is not modified. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     None. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Initialize all members of a double precision array to the */
/*        same value and clear it afterwards. */


/*        Example code begins here. */


/*              PROGRAM CLEARD_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Local parameters. */
/*        C */
/*              INTEGER               NDIM */
/*              PARAMETER           ( NDIM = 4 ) */

/*        C */
/*        C     Local variables. */
/*        C */
/*              DOUBLE PRECISION      ARRAY ( NDIM ) */

/*              INTEGER               I */

/*        C */
/*        C     Initialize all member of the array ARRAY to 11.5, and */
/*        C     print out its contents. */
/*        C */
/*              CALL FILLD ( 11.5D0, NDIM, ARRAY ) */

/*              WRITE(*,'(A)') 'Contents of ARRAY before CLEARD:' */
/*              WRITE(*,'(4F6.1)') ( ARRAY(I), I=1, NDIM ) */

/*        C */
/*        C     Clear the contents of ARRAY and print it. */
/*        C */
/*              CALL CLEARD ( NDIM, ARRAY ) */

/*              WRITE(*,*) */
/*              WRITE(*,'(A)') 'Contents of ARRAY after CLEARD:' */
/*              WRITE(*,'(4F6.1)') ( ARRAY(I), I=1, NDIM ) */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*        Contents of ARRAY before CLEARD: */
/*          11.5  11.5  11.5  11.5 */

/*        Contents of ARRAY after CLEARD: */
/*           0.0   0.0   0.0   0.0 */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     W.M. Owen          (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 29-MAY-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Updated the header to comply with NAIF standard. Added */
/*        full code example. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO) */

/* -& */
/* $ Index_Entries */

/*     clear a d.p. array */

/* -& */

/*     Local variables */

    i__1 = *ndim;
    for (i__ = 1; i__ <= i__1; ++i__) {
	array[i__ - 1] = 0.;
    }
    return 0;
} /* cleard_ */

