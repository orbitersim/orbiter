/* pckr02.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;
static integer c__5 = 5;

/* $Procedure PCKR02 ( PCK, read record from type 2 segment ) */
/* Subroutine */ int pckr02_(integer *handle, doublereal *descr, doublereal *
	et, doublereal *record)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    integer nrec;
    doublereal init;
    integer begin;
    extern /* Subroutine */ int chkin_(char *, ftnlen), dafus_(doublereal *, 
	    integer *, integer *, doublereal *, integer *);
    integer recno;
    extern /* Subroutine */ int dafgda_(integer *, integer *, integer *, 
	    doublereal *);
    doublereal dc[2];
    integer ic[5], recadr;
    doublereal intlen;
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    integer recsiz;
    extern logical return_(void);
    integer end;

/* $ Abstract */

/*     Read a single PCK data record from a segment of type 2 */
/*     (Chebyshev, 3-vector only). */

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

/*     PCK */

/* $ Keywords */

/*     EPHEMERIS */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   File handle. */
/*     DESCR      I   Segment descriptor. */
/*     ET         I   Target epoch. */
/*     RECORD     O   Data record. */

/* $ Detailed_Input */

/*     HANDLE, */
/*     DESCR    are the file handle and segment descriptor for */
/*              a PCK segment of type 2. */

/*     ET       is a target epoch, for which a data record from */
/*              a specific segment is required. */

/* $ Detailed_Output */

/*     RECORD   is the record from the specified segment which, */
/*              when evaluated at epoch ET, will give the Euler */
/*              angles (orientation) of some body. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     None. */

/* $ Files */

/*     See argument HANDLE. */

/* $ Particulars */

/*     See the PCK Required Reading file for a description of the */
/*     structure of a data type 2 (Chebyshev polynomials, Euler */
/*     angles only) segment. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Dump the record of a type 2 PCK which, when evaluated at */
/*        a given epoch, will give the Euler angles (orientation) of */
/*        the Moon body-fixed frame with class ID 31004 with respect */
/*        to J2000. */

/*        Note that the data returned is in its rawest form, taken */
/*        directly from the segment. As such, it will be meaningless to */
/*        a user unless he/she understands the structure of the data */
/*        type completely. Given that understanding, however, the PCKR02 */
/*        routine might be used to "dump" and check segment data for a */
/*        particular epoch. */

/*        Use the PCK kernel below to obtain the record. */

/*           moon_pa_de418_1950-2050.bpc */


/*        Example code begins here. */


/*              PROGRAM PCKR02_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Local parameters */
/*        C */
/*              INTEGER               BODY */
/*              PARAMETER           ( BODY   = 31004 ) */

/*              INTEGER               DESCSZ */
/*              PARAMETER           ( DESCSZ = 5     ) */

/*              INTEGER               IDSIZE */
/*              PARAMETER           ( IDSIZE = 40    ) */

/*        C */
/*        C     Set the maximum record size: */
/*        C */
/*        C        RSIZE = 2 + 3 * (PDEG +1) */
/*        C */
/*        C     Assume a maximum polynomial degree of 25, and */
/*        C     knowing that PCKR02 returns RSIZE as first element */
/*        C     of the output record... */
/*        C */
/*              INTEGER               RECRSZ */
/*              PARAMETER           ( RECRSZ = 81    ) */

/*        C */
/*        C     Local variables */
/*        C */
/*              CHARACTER*(IDSIZE)    SEGID */

/*              DOUBLE PRECISION      BEGET */
/*              DOUBLE PRECISION      DESCR  ( DESCSZ ) */
/*              DOUBLE PRECISION      ENDET */
/*              DOUBLE PRECISION      ET */
/*              DOUBLE PRECISION      RECORD ( RECRSZ ) */

/*              INTEGER               BADDR */
/*              INTEGER               BODYID */
/*              INTEGER               EADDR */
/*              INTEGER               FRAMID */
/*              INTEGER               HANDLE */
/*              INTEGER               I */
/*              INTEGER               INDEX */
/*              INTEGER               PCKHDL */
/*              INTEGER               PCKTYP */
/*              INTEGER               PDEG */
/*              INTEGER               RSIZE */

/*              LOGICAL               FOUND */

/*        C */
/*        C     Load the PCK file. */
/*        C */
/*              CALL PCKLOF ( 'moon_pa_de418_1950-2050.bpc', PCKHDL ) */

/*        C */
/*        C     Set the epoch. Use ephemeris time of J2000 epoch. */
/*        C */
/*              ET = 0.D0 */

/*        C */
/*        C     Get a segment applicable to a specified body and epoch. */
/*        C */
/*              CALL PCKSFS ( BODY, ET, HANDLE, DESCR, SEGID, FOUND ) */

/*              IF ( FOUND ) THEN */

/*        C */
/*        C        Unpack the segment. */
/*        C */
/*                 CALL PCKUDS ( DESCR, BODYID, FRAMID, PCKTYP, */
/*             .                 BEGET, ENDET,  BADDR,  EADDR  ) */


/*                 IF ( PCKTYP .EQ. 2 ) THEN */

/*                    CALL PCKR02 ( HANDLE, DESCR, ET, RECORD ) */

/*                    RSIZE = RECORD(1) */
/*                    PDEG  = ( RSIZE - 2 ) / 3 - 1 */

/*        C */
/*        C           Output the data. */
/*        C */
/*                    WRITE(*,*) 'Record size      : ', RSIZE */
/*                    WRITE(*,*) 'Polynomial degree: ', PDEG */

/*                    WRITE(*,*) 'Record data      :' */
/*                    WRITE(*,*) '   Interval midpoint: ', RECORD(2) */
/*                    WRITE(*,*) '   Interval radius  : ', RECORD(3) */

/*                    INDEX = 4 */
/*                    WRITE(*,*) '   RA coefficients  : ' */
/*                    DO I = 0, PDEG */
/*                       WRITE(*,*) '      ', RECORD(INDEX+I) */
/*                    END DO */

/*                    INDEX = 4 + ( PDEG + 1 ) */
/*                    WRITE(*,*) '   DEC coefficients : ' */
/*                    DO I = 0, PDEG */
/*                       WRITE(*,*) '      ', RECORD(INDEX+I) */
/*                    END DO */

/*                    INDEX = 4 + 2 * ( PDEG + 1 ) */
/*                    WRITE(*,*) '   W coefficients   : ' */
/*                    DO I = 0, PDEG */
/*                       WRITE(*,*) '      ', RECORD(INDEX+I) */
/*                    END DO */

/*                 ELSE */

/*                    WRITE(*,*) 'PCK is not type 2' */

/*                 END IF */

/*              ELSE */

/*                 WRITE(*,*) '   ***** SEGMENT NOT FOUND *****' */

/*              END IF */

/*        C */
/*        C     Unload the PCK file. */
/*        C */
/*              CALL PCKUOF ( PCKHDL ) */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*         Record size      :           32 */
/*         Polynomial degree:            9 */
/*         Record data      : */
/*            Interval midpoint:    302400.00000000000 */
/*            Interval radius  :    345600.00000000000 */
/*            RA coefficients  : */
/*                 -5.4242086033301107E-002 */
/*                 -5.2241405162792561E-005 */
/*                  8.9751456289930307E-005 */
/*                 -1.5288696963234620E-005 */
/*                  1.3218870864581395E-006 */
/*                  5.9822156790328180E-007 */
/*                 -6.5967702052551211E-008 */
/*                 -9.9084309118396298E-009 */
/*                  4.9276055963541578E-010 */
/*                  1.1612267413829385E-010 */
/*            DEC coefficients : */
/*                 0.42498898565916610 */
/*                  1.3999219324235620E-004 */
/*                 -1.8855140511098865E-005 */
/*                 -2.1964684808526649E-006 */
/*                  1.4229817868138752E-006 */
/*                 -1.6991716166847001E-007 */
/*                 -3.4824688140649506E-008 */
/*                  2.9208428745895990E-009 */
/*                  4.4217757657060300E-010 */
/*                 -3.9211207055305402E-012 */
/*            W coefficients   : */
/*                  2565.0633504619473 */
/*                 0.92003769451305328 */
/*                 -8.0503797901914501E-005 */
/*                  1.1960860244433900E-005 */
/*                 -1.2237900518372542E-006 */
/*                 -5.3651349407824562E-007 */
/*                  6.0843372260403005E-008 */
/*                  9.0211287487688797E-009 */
/*                 -4.6460429330339309E-010 */
/*                 -1.0446918704281774E-010 */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     E.D. Wright        (JPL) */
/*     K.S. Zukor         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.2, 06-JUL-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. */
/*        Added complete code example from existing fragment. */

/* -    SPICELIB Version 1.1.1, 03-JAN-2014 (EDW) */

/*        Minor edits to $Procedure; clean trailing whitespace. */

/* -    SPICELIB Version 1.1.0, 07-SEP-2001 (EDW) */

/*        Replaced DAFRDA call with DAFGDA. */
/*        Added IMPLICIT NONE. */

/* -    SPICELIB Version 1.0.0, 11-MAR-1993 (KSZ) */

/* -& */
/* $ Index_Entries */

/*     read record from type_2 PCK segment */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 1.1.0, 07-SEP-2001 (EDW) */

/*        Replaced DAFRDA call with DAFGDA. */
/*        Added IMPLICIT NONE. */

/* -& */

/*     SPICELIB functions */


/*     Parameters */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("PCKR02", (ftnlen)6);
    }

/*     Unpack the segment descriptor. */

    dafus_(descr, &c__2, &c__5, dc, ic);
    begin = ic[3];
    end = ic[4];

/*     The segment is made up of a number of logical records, each */
/*     having the same size, and covering the same length of time. */

/*     We can determine which record to return by comparing the input */
/*     epoch with the initial time of the segment and the length of the */
/*     interval covered by each record.  These final two constants are */
/*     located at the end of the segment, along with the size of each */
/*     logical record and the total number of records. */

    i__1 = end - 3;
    dafgda_(handle, &i__1, &end, record);
    init = record[0];
    intlen = record[1];
    recsiz = (integer) record[2];
    nrec = (integer) record[3];
    recno = (integer) ((*et - init) / intlen) + 1;
    recno = min(recno,nrec);

/*     Compute the address of the desired record. */

    recadr = (recno - 1) * recsiz + begin;

/*     Along with the record, return the size of the record. */

    record[0] = record[2];
    i__1 = recadr + recsiz - 1;
    dafgda_(handle, &recadr, &i__1, &record[1]);
    chkout_("PCKR02", (ftnlen)6);
    return 0;
} /* pckr02_ */

