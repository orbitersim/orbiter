/* brcktd.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure BRCKTD ( Bracket a d.p. value within an interval ) */
doublereal brcktd_(doublereal *number, doublereal *end1, doublereal *end2)
{
    /* System generated locals */
    doublereal ret_val, d__1, d__2;

/* $ Abstract */

/*     Bracket a double precision number. That is, given a number and an */
/*     acceptable interval, make sure that the number is contained in the */
/*     interval. (If the number is already in the interval, leave it */
/*     alone. If not, set it to the nearest endpoint of the interval.) */

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

/*     INTERVALS */
/*     NUMBERS */
/*     UTILITY */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     NUMBER     I   Number to be bracketed. */
/*     END1       I   One of the bracketing endpoints for NUMBER. */
/*     END2       I   The other bracketing endpoint for NUMBER. */

/*     The function returns the bracketed number. */

/* $ Detailed_Input */

/*     NUMBER   is the number to be bracketed. That is, the */
/*              value of NUMBER is constrained to lie in the */
/*              interval bounded by END1 and END2. */

/*     END1, */
/*     END2     are the lower and upper bounds for NUMBER. The */
/*              order is not important. */

/* $ Detailed_Output */

/*     The function returns the bracketed number. That is NUMBER, if it */
/*     was already in the interval provided. Otherwise the returned */
/*     value is the nearest bound of the interval. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine provides a shorthand notation for code fragments */
/*     like the following: */

/*        IF ( END1 .LT. END2 ) THEN */
/*           IF      ( NUMBER .LT. END1 ) THEN */
/*              NUMBER = END1 */
/*           ELSE IF ( NUMBER .GT. END2 ) THEN */
/*              NUMBER = END2 */
/*           END IF */
/*        ELSE */
/*           IF      ( NUMBER .LT. END2 ) THEN */
/*              NUMBER = END2 */
/*           ELSE IF ( NUMBER .GT. END1 ) THEN */
/*              NUMBER = END1 */
/*           END IF */
/*        END IF */

/*     which occur frequently during the processing of program inputs. */

/* $ Examples */

/*     The numerical results shown for these examples may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) The following code example illustrates the operation of */
/*        BRCKTD. */

/*        Example code begins here. */


/*              PROGRAM BRCKTD_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     SPICELIB functions. */
/*        C */
/*              DOUBLE PRECISION        BRCKTD */

/*        C */
/*        C     Local parameters. */
/*        C */
/*              INTEGER                 LISTSZ */
/*              PARAMETER             ( LISTSZ = 4  ) */

/*        C */
/*        C     Local variables. */
/*        C */
/*              DOUBLE PRECISION        END1    ( LISTSZ ) */
/*              DOUBLE PRECISION        END2    ( LISTSZ ) */
/*              DOUBLE PRECISION        NUMBER  ( LISTSZ ) */

/*              INTEGER                 I */

/*        C */
/*        C     Set the values for the example. */
/*        C */
/*              DATA                    END1   /  1.D0,   1.D0, */
/*             .                                 10.D0, -10.D0  / */
/*              DATA                    END2   / 10.D0,  10.D0, */
/*             .                                -10.D0,  -1.D0  / */
/*              DATA                    NUMBER / -1.D0,  29.D0, */
/*             .                                  3.D0,   3.D0  / */


/*              WRITE(*,'(A)') ' Number  End1   End2   Bracketed' */
/*              WRITE(*,'(A)') ' ------  -----  -----  ---------' */

/*              DO I = 1, LISTSZ */

/*                 WRITE(*,'(3F7.1,F11.1)') NUMBER(I), END1(I), END2(I), */
/*             .                 BRCKTD ( NUMBER(I), END1(I), END2(I) ) */

/*              END DO */


/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*         Number  End1   End2   Bracketed */
/*         ------  -----  -----  --------- */
/*           -1.0    1.0   10.0        1.0 */
/*           29.0    1.0   10.0       10.0 */
/*            3.0   10.0  -10.0        3.0 */
/*            3.0  -10.0   -1.0       -1.0 */


/*     2) The following code example illustrates a typical use for */
/*        BRCKTD: force a star magnitude limit to be within a range. */
/*        Note that this code assumes that the user provided value */
/*        is a valid double precision number. */


/*        Example code begins here. */


/*              PROGRAM BRCKTD_EX2 */
/*              IMPLICIT NONE */

/*        C */
/*        C     SPICELIB functions. */
/*        C */
/*              DOUBLE PRECISION        BRCKTD */

/*        C */
/*        C     Local parameters. */
/*        C */
/*              INTEGER                 KWDSZ */
/*              PARAMETER             ( KWDSZ = 30   ) */

/*        C */
/*        C     Local variables. */
/*        C */
/*              CHARACTER*(KWDSZ)       USRIN */

/*              DOUBLE PRECISION        MAGLIN */
/*              DOUBLE PRECISION        MAGLOK */

/*        C */
/*        C     Prompt the user for the star magnitude. */
/*        C */
/*              CALL PROMPT ( 'Enter star magnitude: ', USRIN ) */

/*        C */
/*        C     Convert the user input to double precision. */
/*        C */
/*              CALL PRSDP ( USRIN, MAGLIN ) */

/*        C */
/*        C     Star magnitude must be in the range 0-10. */
/*        C */
/*              MAGLOK = BRCKTD ( MAGLIN, 0.D0, 10.D0 ) */

/*        C */
/*        C     Display confirmation message. */
/*        C */
/*              IF ( MAGLIN .NE. MAGLOK ) THEN */

/*                 WRITE(*,'(A,F4.1,A)') 'Provided star magnitude ', */
/*             .                 MAGLIN, ' is out of range (0-10).' */

/*              ELSE */

/*                 WRITE(*,'(A,F4.1,A)') 'Provided star magnitude ', */
/*             .                 MAGLIN, ' is in range (0-10).' */

/*              END IF */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, using '10.1' as user provided input, the output was: */


/*        Enter star magnitude: 10.1 */
/*        Provided star magnitude 10.1 is out of range (0-10). */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     B.V. Semenov       (JPL) */
/*     W.L. Taber         (JPL) */
/*     I.M. Underwood     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 08-AUG-2021 (JDR) (BVS) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Added complete */
/*        code examples based on existing code fragments. */

/*        Updated code fragment in $Particulars to show that the */
/*        order of endpoints is not important. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (WLT) */

/* -& */
/* $ Index_Entries */

/*     bracket a d.p. value within an interval */

/* -& */
/* $ Revisions */

/* -    Beta Version 1.1.0, 30-DEC-1988 (WLT) */

/*        The routine was modified so that the order of the endpoints */
/*        of the bracketing interval is not needed. The routine now */
/*        determines which is the left endpoint and which is the */
/*        right and acts appropriately. */

/* -& */

/*     What else is there to say? */

    if (*end1 < *end2) {
/* Computing MAX */
	d__1 = *end1, d__2 = min(*end2,*number);
	ret_val = max(d__1,d__2);
    } else {
/* Computing MAX */
	d__1 = *end2, d__2 = min(*end1,*number);
	ret_val = max(d__1,d__2);
    }
    return ret_val;
} /* brcktd_ */

