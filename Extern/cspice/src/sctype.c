/* sctype.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure SCTYPE ( SCLK type ) */
integer sctype_(integer *sc)
{
    /* System generated locals */
    integer ret_val;

    /* Local variables */
    extern /* Subroutine */ int chkin_(char *, ftnlen), scty01_(integer *, 
	    integer *), chkout_(char *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Return the spacecraft clock type for a specified spacecraft. */

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

/*     SCLK */

/* $ Keywords */

/*     TIME */

/* $ Declarations */
/* $ Abstract */

/*     This include file defines the dimension of the counter */
/*     array used by various SPICE subsystems to uniquely identify */
/*     changes in their states. */

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

/*     CTRSIZ      is the dimension of the counter array used by */
/*                 various SPICE subsystems to uniquely identify */
/*                 changes in their states. */

/* $ Author_and_Institution */

/*     B.V. Semenov    (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 29-JUL-2013 (BVS) */

/* -& */

/*     End of include file. */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     SC         I   NAIF spacecraft ID code. */

/*     The function returns the spacecraft clock type associated with */
/*     the spacecraft specified by SC. */

/* $ Detailed_Input */

/*     SC       is a NAIF ID code for a spacecraft, whose */
/*              spacecraft clock `type' is desired. */

/* $ Detailed_Output */

/*     The function returns the spacecraft clock type associated with */
/*     the spacecraft specified by SC. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the kernel variable that assigns a SCLK type to the */
/*         spacecraft specified by SC is not found in the kernel pool, an */
/*         error is signaled by a routine in the call tree of this */
/*         routine. SCTYPE returns the value 0 in this case. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The raison d'etre of this routine is that it consolidates the code */
/*     that maps spacecraft ID's to clock types. While any routine may */
/*     call SCTYPE, it is unlikely that there will be a need for */
/*     non-SPICELIB routines to call this routine directly. */

/* $ Examples */

/*     1)  Find the SCLK type for Galileo. */

/*            During program initialization, we load a SCLK kernel file */
/*            into the kernel pool. We will pretend that the name of */
/*            this file is GLLSCLK.KER. You must use the actual name of */
/*            an SCLK kernel that is accessible by your program to try */
/*            this example. */

/*                C */
/*                C     Load the SCLK kernel. */
/*                C */
/*                      CALL FURNSH ( 'GLLSCLK.KER' ) */
/*                                 . */
/*                                 . */
/*                                 . */
/*                C */
/*                C     Print out the clock type for Galileo. */
/*                C */
/*                      TYPE = SCTYPE ( -77 ) */

/*                      PRINT *, 'Galileo clock type is ', TYPE */


/*     2)  Find the SCLK type for Mars Observer. */


/*                C */
/*                C     Load the SCLK kernel. */
/*                C */
/*                      CALL FURNSH ( 'MOSCLK.KER' ) */
/*                                 . */
/*                                 . */
/*                                 . */
/*                C */
/*                C     Print out the clock type for Mars Observer. */
/*                C */
/*                      TYPE = SCTYPE ( -94 ) */

/*                      PRINT *, 'Mars Observer clock type is ', TYPE */

/* $ Restrictions */

/*     1)  This routine assumes that an SCLK kernel appropriate to the */
/*         spacecraft specified by SC has been loaded into the kernel */
/*         pool. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     J.M. Lynch         (JPL) */
/*     B.V. Semenov       (JPL) */
/*     W.L. Taber         (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.4.0, 12-AUG-2021 (NJB) (JDR) */

/*        Re-implemented using the type 1 SCLK database in order to */
/*        improve performance. The routine still works as before for */
/*        clock types other than 1. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.3.0, 09-SEP-2013 (BVS) */

/*        Updated to keep track of the POOL counter and call ZZCVPOOL. */

/* -    SPICELIB Version 1.2.0, 05-MAR-2009 (NJB) */

/*        Bug fix: this routine now keeps track of whether its */
/*        kernel pool look-up succeeded. If not, a kernel pool */
/*        lookup is attempted on the next call to this routine. */

/* -    SPICELIB Version 1.1.1, 22-AUG-2006 (EDW) */

/*        Replaced references to LDPOOL with references */
/*        to FURNSH. */

/* -    SPICELIB Version 1.1.0, 22-MAR-1993 (JML) */

/*        1) The routine now uses the kernel pool watch capability. */

/*        2) The routine now returns a value of zero if RETURN is */
/*           .TRUE. on entry. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 04-SEP-1990 (NJB) */

/* -& */
/* $ Index_Entries */

/*     spacecraft_clock type */

/* -& */

/*     SPICELIB functions */


/*     Standard SPICE error handling. */

    if (return_()) {
	ret_val = 0;
	return ret_val;
    }
    chkin_("SCTYPE", (ftnlen)6);

/*     Get the type from the type 1 subsystem. */

    scty01_(sc, &ret_val);
    chkout_("SCTYPE", (ftnlen)6);
    return ret_val;
} /* sctype_ */

