/* ckfrot.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;
static integer c__6 = 6;

/* $Procedure CKFROT ( CK frame, find position rotation ) */
/* Subroutine */ int ckfrot_(integer *inst, doublereal *et, doublereal *
	rotate, integer *ref, logical *found)
{
    logical have, pfnd, sfnd;
    doublereal time;
    extern /* Subroutine */ int sce2c_(integer *, doublereal *, doublereal *);
    char segid[40];
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    doublereal descr[5];
    extern /* Subroutine */ int dafus_(doublereal *, integer *, integer *, 
	    doublereal *, integer *), ckbss_(integer *, doublereal *, 
	    doublereal *, logical *), ckpfs_(integer *, doublereal *, 
	    doublereal *, doublereal *, logical *, doublereal *, doublereal *,
	     doublereal *, logical *), cksns_(integer *, doublereal *, char *,
	     logical *, ftnlen), xpose_(doublereal *, doublereal *);
    extern logical failed_(void);
    doublereal av[3];
    integer handle;
    extern /* Subroutine */ int ckhave_(logical *);
    logical needav;
    extern /* Subroutine */ int ckmeta_(integer *, char *, integer *, ftnlen);
    integer sclkid;
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    doublereal clkout;
    extern logical return_(void), zzsclk_(integer *, integer *);
    doublereal dcd[2];
    integer icd[6];
    doublereal tol, rot[9]	/* was [3][3] */;

/* $ Abstract */

/*     Find the position rotation matrix from a C-kernel (CK) frame with */
/*     the specified frame class ID (CK ID) to the base frame of the */
/*     highest priority CK segment containing orientation data for this */
/*     CK frame at the time requested. */

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

/* $ Keywords */

/*     POINTING */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     INST       I   Frame class ID (CK ID) of a CK frame. */
/*     ET         I   Epoch measured in seconds past J2000 TDB. */
/*     ROTATE     O   Rotation matrix from CK frame to frame REF. */
/*     REF        O   Frame ID of the base reference. */
/*     FOUND      O   .TRUE. when requested pointing is available. */

/* $ Detailed_Input */

/*     INST     is the unique frame class ID (CK ID) of the CK frame for */
/*              which data is being requested. */

/*     ET       is the epoch for which the position rotation is desired. */
/*              ET should be given in seconds past the epoch of J2000 */
/*              TDB. */

/* $ Detailed_Output */

/*     ROTATE   is a position rotation matrix that converts positions */
/*              relative to the CK frame given by its frame class ID, */
/*              INST, to positions relative to the base frame given by */
/*              its frame ID, REF. */

/*              Thus, if a position S has components x,y,z in the CK */
/*              frame, then S has components x', y', z' in the base */
/*              frame. */

/*                 .-  -.     .-        -. .- -. */
/*                 | x' |     |          | | x | */
/*                 | y' |  =  |  ROTATE  | | y | */
/*                 | z' |     |          | | z | */
/*                 `-  -'     `-        -' `- -' */


/*     REF      is the ID code of the base reference frame to which */
/*              ROTATE will transform positions. */

/*     FOUND    is .TRUE. if a record was found to satisfy the pointing */
/*              request. FOUND will be .FALSE. otherwise. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If no CK files were loaded prior to calling this routine, an */
/*         error is signaled by a routine in the call tree of this */
/*         routine. */

/*     2)  If no SCLK correlation data needed to read CK files were */
/*         loaded prior to calling this routine, an error is signaled by */
/*         a routine in the call tree of this routine. */

/*     3)  If the input time ET cannot be converted to an encoded SCLK */
/*         time, using SCLK data associated with INST, an error is */
/*         signaled by a routine in the call tree of this routine. */

/* $ Files */

/*     CKFROT searches through loaded CK files to locate a segment that */
/*     can satisfy the request for position rotation data for the CK */
/*     frame with the specified frame class ID at time ET. You must load */
/*     a CK file containing such data before calling this routine. You */
/*     must also load SCLK and possibly LSK files needed to convert the */
/*     input ET time to the encoded SCLK time with which the orientation */
/*     data stored inside that CK is tagged. */

/* $ Particulars */

/*     CKFROT searches through loaded CK files to satisfy a pointing */
/*     request. Last-loaded files are searched first, and individual */
/*     files are searched in backwards order, giving priority to */
/*     segments that were added to a file later than the others. */

/*     The search ends when a segment is found that can give pointing */
/*     for the specified CK frame at the request time. */

/*     Segments with and without angular velocities are considered by */
/*     this routine. */

/*     This routine uses the CKMETA routine to determine the SCLK ID */
/*     used to convert the input ET time to the encoded SCLK time used */
/*     to look up pointing data in loaded CK files. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Use CKFROT to compute the instantaneous angular velocity */
/*        vector for the Mars Global Surveyor (MGS) spacecraft frame, */
/*        'MGS_SPACECRAFT', relative to the inertial frame used as the */
/*        base frame in CK files containing MGS spacecraft orientation */
/*        at 2003-JUL-25 13:00:00. The frame class ID (CK ID) for the */
/*        'MGS_SPACECRAFT' frame is -94000. */


/*        Suppose that R(t) is the rotation matrix whose columns */
/*        represent the inertial pointing vectors of the MGS spacecraft */
/*        axes at time `t'. */

/*        Then the angular velocity vector points along the vector given */
/*        by: */

/*                                T */
/*            limit  AXIS( R(t+h)R ) */
/*            h-->0 */


/*        And the magnitude of the angular velocity at time `t' is given */
/*        by: */

/*                                T */
/*            d ANGLE ( R(t+h)R(t) ) */
/*           ------------------------   at   h = 0 */
/*                      dh */


/*        Use the meta-kernel shown below to load the required SPICE */
/*        kernels. */


/*           KPL/MK */

/*           File name: ckfrot_ex1.tm */

/*           This meta-kernel is intended to support operation of SPICE */
/*           example programs. The kernels shown here should not be */
/*           assumed to contain adequate or correct versions of data */
/*           required by SPICE-based user applications. */

/*           In order for an application to use this meta-kernel, the */
/*           kernels referenced here must be present in the user's */
/*           current working directory. */

/*           The names and contents of the kernels referenced */
/*           by this meta-kernel are as follows: */

/*              File name                     Contents */
/*              ---------                     -------- */
/*              naif0012.tls                  Leapseconds */
/*              mgs_sclkscet_00061.tsc        MGS SCLK coefficients */
/*              mgs_sc_ext12.bc               MGS s/c bus attitude */

/*           \begindata */

/*           KERNELS_TO_LOAD = ( 'naif0012.tls', */
/*                               'mgs_sclkscet_00061.tsc', */
/*                               'mgs_sc_ext12.bc' ) */

/*           \begintext */

/*           End of meta-kernel */


/*        Example code begins here. */


/*              PROGRAM CKFROT_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Local parameters. */
/*        C */
/*              CHARACTER*(*)         EPOCH */
/*              PARAMETER           ( EPOCH  = '2003-JUL-25 13:00:00' ) */

/*              INTEGER               INST */
/*              PARAMETER           ( INST   = -94000 ) */

/*        C */
/*        C     Local variables. */
/*        C */
/*              DOUBLE PRECISION      ANGLE */
/*              DOUBLE PRECISION      ANGVEL ( 3    ) */
/*              DOUBLE PRECISION      AXIS   ( 3    ) */
/*              DOUBLE PRECISION      ET */
/*              DOUBLE PRECISION      INFROT ( 3, 3 ) */
/*              DOUBLE PRECISION      H */
/*              DOUBLE PRECISION      RET    ( 3, 3 ) */
/*              DOUBLE PRECISION      RETH   ( 3, 3 ) */

/*              INTEGER               REF */
/*              INTEGER               REFH */

/*              LOGICAL               FOUND */
/*              LOGICAL               FOUNDH */

/*        C */
/*        C     Load the required LSK, SCLK and CK. Use a */
/*        C     meta-kernel for convenience. */
/*        C */
/*              CALL FURNSH ( 'ckfrot_ex1.tm' ) */

/*        C */
/*        C     First convert the time to seconds past J2000. Set the */
/*        C     delta time (1 ms). */
/*        C */
/*              CALL STR2ET ( EPOCH, ET ) */
/*              H = 1.D-3 */

/*        C */
/*        C     Now, look up the rotation from the MGS spacecraft */
/*        C     frame specified by its frame class ID (CK ID) to a */
/*        C     base reference frame (returned by CKFROT), at ET */
/*        C     and ET+H. */
/*        C */
/*              CALL CKFROT ( INST, ET,   RET,  REF,  FOUND  ) */
/*              CALL CKFROT ( INST, ET+H, RETH, REFH, FOUNDH ) */

/*        C */
/*        C     If both rotations were computed and if the base */
/*        C     reference frames are the same, compute the */
/*        C     instantaneous angular velocity vector. */
/*        C */
/*              IF ( FOUND .AND. FOUNDH .AND. REF .EQ. REFH ) THEN */

/*        C */
/*        C        Compute the infinitesimal rotation R(t+h)R(t)**T. */
/*        C */
/*                 CALL MXMT ( RETH, RET, INFROT ) */

/*        C */
/*        C        Compute the AXIS and ANGLE of the infinitesimal */
/*        C        rotation. */
/*        C */
/*                 CALL RAXISA ( INFROT, AXIS, ANGLE ) */

/*        C */
/*        C        Scale AXIS to get the angular velocity vector. */
/*        C */
/*                 CALL VSCL ( ANGLE/H, AXIS, ANGVEL ) */

/*        C */
/*        C        Output the results. */
/*        C */
/*                 WRITE(*,'(A)') */
/*             .           'Instantaneous angular velocity vector:' */
/*                 WRITE(*,'(3F15.10)') ANGVEL */
/*                 WRITE(*,'(A,I5)') 'Reference frame ID:', REF */

/*              ELSE */

/*                 WRITE(*,*) 'ERROR: data not found or frame mismatch.' */

/*              END IF */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*        Instantaneous angular velocity vector: */
/*           0.0001244121   0.0008314866   0.0003028634 */
/*        Reference frame ID:    1 */


/* $ Restrictions */

/*     1)  A CK file must be loaded prior to calling this routine. */

/*     2)  LSK and SCLK files needed for time conversions must be loaded */
/*         prior to calling this routine. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     B.V. Semenov       (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.3.0, 13-DEC-2021 (JDR) (BVS) (NJB) */

/*        Edited the header to comply with NAIF standard and modern */
/*        SPICE CK and frames terminology. Added initialization of local */
/*        variable SFND. */

/*        Added complete code example. */

/* -    SPICELIB Version 1.2.0, 17-FEB-2000 (WLT) */

/*        The routine now checks to make sure convert ET to TICKS */
/*        and that at least one C-kernel is loaded before trying */
/*        to look up the transformation. Also the routine now calls */
/*        SCE2C instead of SCE2T. */

/* -    SPICELIB Version 1.0.0, 03-MAR-1999 (WLT) */

/* -& */
/* $ Index_Entries */

/*     get instrument frame rotation and reference frame */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */

/*        NDC        is the number of double precision components in an */
/*                   unpacked C-kernel segment descriptor. */

/*        NIC        is the number of integer components in an unpacked */
/*                   C-kernel segment descriptor. */

/*        NC         is the number of components in a packed C-kernel */
/*                   descriptor.  All DAF summaries have this formulaic */
/*                   relationship between the number of its integer and */
/*                   double precision components and the number of packed */
/*                   components. */

/*        IDLEN      is the length of the C-kernel segment identifier. */
/*                   All DAF names have this formulaic relationship */
/*                   between the number of summary components and */
/*                   the length of the name (You will notice that */
/*                   a name and a summary have the same length in bytes.) */


/*     Local variables */


/*     Set FOUND to .FALSE. right now in case we end up */
/*     returning before doing any work. */

    *found = FALSE_;
    *ref = 0;

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("CKFROT", (ftnlen)6);

/*     We don't need angular velocity data. */
/*     Assume the segment won't be found until it really is. */

    needav = FALSE_;
    tol = 0.;

/*     Begin a search for this instrument and time, and get the first */
/*     applicable segment. */

    ckhave_(&have);
    ckmeta_(inst, "SCLK", &sclkid, (ftnlen)4);
    if (! have) {
	chkout_("CKFROT", (ftnlen)6);
	return 0;
    } else if (! zzsclk_(inst, &sclkid)) {
	chkout_("CKFROT", (ftnlen)6);
	return 0;
    }

/*     Initialize SFND here in case an error occurs before CKSNS can */
/*     set its value. */

    sfnd = FALSE_;
    sce2c_(&sclkid, et, &time);
    ckbss_(inst, &time, &tol, &needav);
    cksns_(&handle, descr, segid, &sfnd, (ftnlen)40);

/*     Keep trying candidate segments until a segment can produce a */
/*     pointing instance within the specified time tolerance of the */
/*     input time. */

/*     Check FAILED to prevent an infinite loop if an error is detected */
/*     by a SPICELIB routine and the error handling is not set to abort. */

    while(sfnd && ! failed_()) {
	ckpfs_(&handle, descr, &time, &tol, &needav, rot, av, &clkout, &pfnd);
	if (pfnd) {

/*           Found one. Fetch the ID code of the reference frame */
/*           from the descriptor. */

	    dafus_(descr, &c__2, &c__6, dcd, icd);
	    *ref = icd[1];
	    *found = TRUE_;

/*           We now have the rotation matrix from */
/*           REF to INS. We invert ROT to get the rotation */
/*           from INST to REF. */

	    xpose_(rot, rotate);
	    chkout_("CKFROT", (ftnlen)6);
	    return 0;
	}
	cksns_(&handle, descr, segid, &sfnd, (ftnlen)40);
    }
    chkout_("CKFROT", (ftnlen)6);
    return 0;
} /* ckfrot_ */

