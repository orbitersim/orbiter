/* cknm06.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;
static integer c__6 = 6;

/* $Procedure CKNM06 ( C-kernel, number of mini-segments, type 06 ) */
/* Subroutine */ int cknm06_(integer *handle, doublereal *descr, integer *
	nmini)
{
    /* Builtin functions */
    integer i_dnnt(doublereal *);

    /* Local variables */
    extern /* Subroutine */ int chkin_(char *, ftnlen), dafus_(doublereal *, 
	    integer *, integer *, doublereal *, integer *), dafgda_(integer *,
	     integer *, integer *, doublereal *);
    doublereal dc[2];
    integer ic[6];
    extern logical failed_(void);
    doublereal dpdata[1];
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen), errint_(char *, integer *, 
	    ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Return the number of mini-segments in a type 6 CK segment, given */
/*     the handle of the type 6 CK file and the segment's descriptor. */

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

/*     CK */
/*     DAF */

/* $ Keywords */

/*     POINTING */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   The handle of the file containing the segment. */
/*     DESCR      I   The descriptor of the type 6 segment. */
/*     NMINI      O   The number of pointing instances in the segment. */

/* $ Detailed_Input */

/*     HANDLE   is the handle of the binary CK file containing the */
/*              segment. Normally the CK file should be open for */
/*              read access. See the $Files section below for details. */

/*     DESCR    is the DAF descriptor of a CK data type 6 segment. */

/* $ Detailed_Output */

/*     NMINI    is the number of mini-segments in the CK segment */
/*              identified by HANDLE and DESCR. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the segment indicated by DESCR is not a type 6 segment, */
/*         the error SPICE(CKWRONGDATATYPE) is signaled. */

/*     2)  If the specified handle does not belong to any DAF file that */
/*         is currently known to be open, an error is signaled by a */
/*         routine in the call tree of this routine. */

/*     3)  If DESCR is not a valid descriptor of a valid segment in the */
/*         CK file specified by HANDLE, the results of this routine are */
/*         unpredictable. */

/* $ Files */

/*     The CK file specified by HANDLE may be open for read or write */
/*     access. Normally, the file should have been opened for read */
/*     access. If the file is open for write access, the calling */
/*     application must ensure integrity of the CK segment being read. */
/*     If the structure of the segment is invalid---for example, if the */
/*     segment has been partially written---this routine will either */
/*     return invalid results, or it will cause a system-level runtime */
/*     error. */

/* $ Particulars */

/*     For a complete description of the internal structure of a type 6 */
/*     segment, see the CK required reading. */

/*     This routine returns the number of discrete pointing instances */
/*     contained in the specified segment. It is normally used in */
/*     conjunction with CKMN06, which returns mini-segment parameters, */
/*     and with CKGR06, which returns a specified pointing instance */
/*     from a mini-segment. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) The following program dumps records from a CK file that */
/*        contains only type 6 segments. */


/*        Example code begins here. */


/*              PROGRAM CKNM06_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Dump all records from a CK that */
/*        C     contains only segments of type 6. */
/*        C */
/*              INCLUDE 'ck06.inc' */

/*        C */
/*        C     Local parameters */
/*        C */
/*              INTEGER               ND */
/*              PARAMETER           ( ND     = 2 ) */

/*              INTEGER               NI */
/*              PARAMETER           ( NI     = 6 ) */

/*              INTEGER               DSCSIZ */
/*              PARAMETER           ( DSCSIZ = 5 ) */

/*              INTEGER               FILSIZ */
/*              PARAMETER           ( FILSIZ = 255 ) */

/*        C */
/*        C     RECSIZ is the size of the largest pointing */
/*        C     record, which corresponds to subtype 2. */
/*        C */
/*              INTEGER               RECSIZ */
/*              PARAMETER           ( RECSIZ = C06PS2 + 3 ) */

/*        C */
/*        C     Local variables */
/*        C */
/*              CHARACTER*(FILSIZ)    CK */

/*              DOUBLE PRECISION      DC     ( ND ) */
/*              DOUBLE PRECISION      DESCR  ( DSCSIZ ) */
/*              DOUBLE PRECISION      IVLBDS ( 2 ) */
/*              DOUBLE PRECISION      LSTEPC */
/*              DOUBLE PRECISION      RATE */
/*              DOUBLE PRECISION      RECORD ( RECSIZ ) */

/*              INTEGER               DTYPE */
/*              INTEGER               HANDLE */
/*              INTEGER               IC     ( NI ) */
/*              INTEGER               RECNO */
/*              INTEGER               MSNO */
/*              INTEGER               NMINI */
/*              INTEGER               NREC */
/*              INTEGER               SEGNO */
/*              INTEGER               SUBTYP */
/*              INTEGER               WINSIZ */

/*              LOGICAL               FOUND */


/*              CALL PROMPT ( 'Enter name of CK to dump > ', CK ) */

/*              CALL DAFOPR ( CK, HANDLE ) */

/*        C */
/*        C     Dump data from each CK segment. */
/*        C */
/*              SEGNO = 0 */

/*              CALL DAFBFS ( HANDLE ) */
/*              CALL DAFFNA ( FOUND  ) */

/*              DO WHILE ( FOUND ) */

/*                 SEGNO = SEGNO + 1 */

/*                 WRITE (*,*) ' ' */
/*                 WRITE (*,*) ' ' */
/*                 WRITE (*,*) 'Segment number: ', SEGNO */

/*        C */
/*        C        Fetch and unpack the descriptor of the */
/*        C        current segment; check the data type. */
/*        C */
/*                 CALL DAFGS ( DESCR ) */
/*                 CALL DAFUS ( DESCR, ND, NI, DC, IC ) */

/*                 DTYPE = IC(3) */

/*                 IF ( DTYPE .NE. 6 ) THEN */

/*                    CALL SETMSG ( 'Data type must be 6 but was #.' ) */
/*                    CALL ERRINT ( '#',  DTYPE                      ) */
/*                    CALL SIGERR ( 'SPICE(NOTSUPPORTED)'            ) */

/*                 END IF */

/*        C */
/*        C        Get the mini-segment count for this */
/*        C        segment. */
/*        C */
/*                 CALL CKNM06 ( HANDLE, DESCR, NMINI ) */

/*        C */
/*        C        Dump data from each mini-segment. */
/*        C */
/*                 DO MSNO = 1, NMINI */

/*        C */
/*        C           Get the mini-segment's record count */
/*        C           and time bounds. */
/*        C */
/*                    CALL CKMP06 ( HANDLE, DESCR,  MSNO, */
/*             .                    RATE,   SUBTYP, WINSIZ, */
/*             .                    NREC,   IVLBDS, LSTEPC ) */

/*                    WRITE (*,*) ' ' */
/*                    WRITE (*,*) '   Mini-segment number: ', MSNO */
/*                    WRITE (*,*) '      Rate:           ',   RATE */
/*                    WRITE (*,*) '      Subtype:        ',   SUBTYP */
/*                    WRITE (*,*) '      Window size:    ',   WINSIZ */
/*                    WRITE (*,*) '      Interval start: ',   IVLBDS(1) */
/*                    WRITE (*,*) '      Interval stop:  ',   IVLBDS(2) */
/*                    WRITE (*,*) '      Last epoch:     ',   LSTEPC */
/*                    WRITE (*,*) ' ' */

/*                    DO RECNO = 1, NREC */

/*                       CALL CKGR06 ( HANDLE, DESCR, */
/*             .                       MSNO,   RECNO,  RECORD ) */

/*                       WRITE (*,*) '      Record number: ', RECNO */
/*                       WRITE (*,*) '         SCLKDP:     ', RECORD(1) */
/*                       WRITE (*,*) '         Clock rate: ', RECORD(3) */

/*                       IF ( SUBTYP .EQ. C06TP0 ) THEN */

/*                          WRITE (*,*) '         Q(0): ', RECORD(4) */
/*                          WRITE (*,*) '         Q(1): ', RECORD(5) */
/*                          WRITE (*,*) '         Q(2): ', RECORD(6) */
/*                          WRITE (*,*) '         Q(3): ', RECORD(7) */
/*                          WRITE (*,*) '    d Q(0)/dt: ', RECORD(8) */
/*                          WRITE (*,*) '    d Q(1)/dt: ', RECORD(9) */
/*                          WRITE (*,*) '    d Q(2)/dt: ', RECORD(10) */
/*                          WRITE (*,*) '    d Q(3)/dt: ', RECORD(11) */

/*                       ELSE IF ( SUBTYP .EQ. C06TP1 ) THEN */

/*                          WRITE (*,*) '         Q(0): ', RECORD(4) */
/*                          WRITE (*,*) '         Q(1): ', RECORD(5) */
/*                          WRITE (*,*) '         Q(2): ', RECORD(6) */
/*                          WRITE (*,*) '         Q(3): ', RECORD(7) */

/*                       ELSE IF ( SUBTYP .EQ. C06TP2 ) THEN */

/*                          WRITE (*,*) '         Q(0): ', RECORD(4) */
/*                          WRITE (*,*) '         Q(1): ', RECORD(5) */
/*                          WRITE (*,*) '         Q(2): ', RECORD(6) */
/*                          WRITE (*,*) '         Q(3): ', RECORD(7) */
/*                          WRITE (*,*) '    d Q(0)/dt: ', RECORD(8) */
/*                          WRITE (*,*) '    d Q(1)/dt: ', RECORD(9) */
/*                          WRITE (*,*) '    d Q(2)/dt: ', RECORD(10) */
/*                          WRITE (*,*) '    d Q(3)/dt: ', RECORD(11) */
/*                          WRITE (*,*) '        AV(1): ', RECORD(12) */
/*                          WRITE (*,*) '        AV(2): ', RECORD(13) */
/*                          WRITE (*,*) '        AV(3): ', RECORD(14) */
/*                          WRITE (*,*) '   d AV(1)/dt: ', RECORD(15) */
/*                          WRITE (*,*) '   d AV(2)/dt: ', RECORD(16) */
/*                          WRITE (*,*) '   d AV(3)/dt: ', RECORD(17) */

/*                       ELSE IF ( SUBTYP .EQ. C06TP3 ) THEN */

/*                          WRITE (*,*) '         Q(0): ', RECORD(4) */
/*                          WRITE (*,*) '         Q(1): ', RECORD(5) */
/*                          WRITE (*,*) '         Q(2): ', RECORD(6) */
/*                          WRITE (*,*) '         Q(3): ', RECORD(7) */
/*                          WRITE (*,*) '        AV(1): ', RECORD(8) */
/*                          WRITE (*,*) '        AV(2): ', RECORD(9) */
/*                          WRITE (*,*) '        AV(3): ', RECORD(10) */

/*                       ELSE */
/*                          CALL SETMSG ( 'Subtype # is not ' */
/*             .            //            'recognized.'         ) */
/*                          CALL ERRINT ( '#', SUBTYP           ) */
/*                          CALL SIGERR ( 'SPICE(NOTSUPPORTED)' ) */
/*                       END IF */

/*                       WRITE (*,*) ' ' */

/*                   END DO */

/*                 END DO */

/*                 CALL DAFFNA ( FOUND ) */

/*              END DO */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, using the Rosetta CK file named */
/*        RATT_DV_257_02_01_T6_00344.BC, the output was: */


/*        Enter name of CK to dump > RATT_DV_257_02_01_T6_00344.BC */


/*         Segment number:            1 */

/*            Mini-segment number:            1 */
/*               Rate:              1.5258789062500000E-005 */
/*               Subtype:                   1 */
/*               Window size:              10 */
/*               Interval start:    24471796593941.691 */
/*               Interval stop:     24472844252095.523 */
/*               Last epoch:        24472844252095.523 */

/*               Record number:            1 */
/*                  SCLKDP:        24471796593941.691 */
/*                  Clock rate:    1.5258789062500000E-005 */
/*                  Q(0):  -0.95514652599900884 */
/*                  Q(1):   0.16277660709912350 */
/*                  Q(2):   0.11688592199582469 */
/*                  Q(3):  -0.21802883133317097 */

/*               Record number:            2 */
/*                  SCLKDP:        24472234538651.801 */
/*                  Clock rate:    1.5258789062500000E-005 */
/*                  Q(0):  -0.95746293340938016 */
/*                  Q(1):   0.14880147654385018 */
/*                  Q(2):   0.12021705739210503 */
/*                  Q(3):  -0.21603405018065600 */

/*               Record number:            3 */
/*                  SCLKDP:        24472676416997.039 */
/*                  Clock rate:    1.5258789062500000E-005 */
/*                  Q(0):  -0.95956954083287593 */
/*                  Q(1):   0.13478976855182764 */
/*                  Q(2):   0.12355113537344563 */
/*                  Q(3):  -0.21399329790313779 */

/*               Record number:            4 */
/*                  SCLKDP:        24472844252095.523 */
/*                  Clock rate:    1.5258789062500000E-005 */
/*                  Q(0):  -0.96030932381589129 */
/*                  Q(1):   0.12949634043544370 */
/*                  Q(2):   0.12480922302154081 */
/*                  Q(3):  -0.21321200307405938 */


/*            Mini-segment number:            2 */
/*               Rate:              1.5258789062500000E-005 */
/*               Subtype:                   1 */
/*               Window size:              10 */
/*               Interval start:    24472844252095.523 */
/*               Interval stop:     24472863912889.105 */
/*               Last epoch:        24472863912889.105 */

/*               Record number:            1 */
/*                  SCLKDP:        24472844252095.523 */
/*                  Clock rate:    1.5258789062500000E-005 */
/*                  Q(0):  -0.96030932403888680 */
/*                  Q(1):   0.12949633879120778 */
/*                  Q(2):   0.12480922338599261 */
/*                  Q(3):  -0.21321200285498659 */

/*               Record number:            2 */
/*                  SCLKDP:        24472851309816.297 */
/*                  Clock rate:    1.5258789062500000E-005 */
/*                  Q(0):  -0.96035266600496283 */
/*                  Q(1):   0.12922730685291675 */
/*                  Q(2):   0.12480259688433022 */
/*                  Q(3):  -0.21318389214860939 */

/*               Record number:            3 */
/*                  SCLKDP:        24472859879905.805 */
/*                  Clock rate:    1.5258789062500000E-005 */
/*                  Q(0):  -0.96041575813224878 */
/*                  Q(1):   0.12886248165419970 */
/*                  Q(2):   0.12474605317805663 */
/*                  Q(3):  -0.21315359384649502 */

/*               Record number:            4 */
/*                  SCLKDP:        24472863912889.105 */
/*                  Clock rate:    1.5258789062500000E-005 */
/*                  Q(0):  -0.96043784177251290 */
/*                  Q(1):   0.12871819083493355 */
/*                  Q(2):   0.12475418449192528 */
/*                  Q(3):  -0.21313651233726627 */


/*            Mini-segment number:            3 */
/*               Rate:              1.5258789062500000E-005 */
/*               Subtype:                   1 */
/*               Window size:              10 */
/*               Interval start:    24472863912889.105 */
/*               Interval stop:     24473139163999.207 */
/*               Last epoch:        24473139163999.207 */

/*               Record number:            1 */
/*                  SCLKDP:        24472863912889.105 */
/*                  Clock rate:    1.5258789062500000E-005 */
/*                  Q(0):  -0.96043784177455394 */
/*                  Q(1):   0.12871819083614683 */

/*        [...] */


/*        Warning: incomplete output. Only 100 out of 2378824 lines have */
/*        been provided. */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     J.M. Lynch         (JPL) */
/*     B.V. Semenov       (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 12-AUG-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.0, 14-MAR-2014 (NJB) (JML) (BVS) */

/* -& */
/* $ Index_Entries */

/*     number of mini-segments in CK type_6 segment */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */

/*        ND         is the number of double precision components in an */
/*                   unpacked C-kernel descriptor. */

/*        NI         is the number of integer components in an unpacked */
/*                   C-kernel descriptor. */

/*        DTYPE      is the data type of the segment that this routine */
/*                   operates on. */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("CKNM06", (ftnlen)6);

/*     The number of discrete pointing instances contained in a data */
/*     type 6 segment is stored in the last double precision word of the */
/*     segment. Since the address of the last word is stored in the */
/*     sixth integer component of the segment descriptor, it is a */
/*     trivial matter to extract the count. */

/*     The unpacked descriptor contains the following information */
/*     about the segment: */

/*        DC(1)  Initial encoded SCLK */
/*        DC(2)  Final encoded SCLK */

/*        IC(1)  CK frame class ID (aka "instrument") */
/*        IC(2)  Inertial reference frame */
/*        IC(3)  Data type */
/*        IC(4)  Angular velocity flag */
/*        IC(5)  Initial address of segment data */
/*        IC(6)  Final address of segment data */


    dafus_(descr, &c__2, &c__6, dc, ic);

/*     If this segment is not of data type 6, then signal an error. */

    if (ic[2] != 6) {
	setmsg_("Data type of the segment should be 6: Passed descriptor sho"
		"ws type = #.", (ftnlen)71);
	errint_("#", &ic[2], (ftnlen)1);
	sigerr_("SPICE(CKWRONGDATATYPE)", (ftnlen)22);
	chkout_("CKNM06", (ftnlen)6);
	return 0;
    }

/*     The number of mini-segments is the final word in the segment. */

    dafgda_(handle, &ic[5], &ic[5], dpdata);
    if (failed_()) {
	chkout_("CKNM06", (ftnlen)6);
	return 0;
    }
    *nmini = i_dnnt(dpdata);
    chkout_("CKNM06", (ftnlen)6);
    return 0;
} /* cknm06_ */

