/* eul2m.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure EUL2M ( Euler angles to matrix ) */
/* Subroutine */ int eul2m_(doublereal *angle3, doublereal *angle2, 
	doublereal *angle1, integer *axis3, integer *axis2, integer *axis1, 
	doublereal *r__)
{
    logical badax;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    doublereal r1[9]	/* was [3][3] */;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), rotate_(doublereal *, integer *, doublereal *), setmsg_(
	    char *, ftnlen), errint_(char *, integer *, ftnlen), rotmat_(
	    doublereal *, doublereal *, integer *, doublereal *);
    extern logical return_(void);

/* $ Abstract */

/*     Construct a rotation matrix from a set of Euler angles. */

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

/*     ROTATION */

/* $ Keywords */

/*     MATRIX */
/*     ROTATION */
/*     TRANSFORMATION */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     ANGLE3, */
/*     ANGLE2, */
/*     ANGLE1     I   Rotation angles about third, second, and first */
/*                    rotation axes (radians). */
/*     AXIS3, */
/*     AXIS2, */
/*     AXIS1      I   Axis numbers of third, second, and first rotation */
/*                    axes. */
/*     R          O   Product of the 3 rotations. */

/* $ Detailed_Input */

/*     ANGLE3, */
/*     ANGLE2, */
/*     ANGLE1, */
/*     AXIS3, */
/*     AXIS2, */
/*     AXIS1    are, respectively, a set of three angles and three */
/*              coordinate axis numbers; each pair ANGLEx and AXISx */
/*              specifies a coordinate transformation consisting of a */
/*              rotation by ANGLEx radians about the coordinate axis */
/*              indexed by AXISx. These coordinate transformations are */
/*              typically symbolized by */

/*                 [ ANGLEx ]     . */
/*                           AXISx */

/*              See the $Particulars section below for details concerning */
/*              this notation. */

/*              Note that these coordinate transformations rotate vectors */
/*              by */

/*                 -ANGLEx */

/*              radians about the axis indexed by AXISx. */

/*              The values of AXISx may be 1, 2, or 3, indicating the X, */
/*              Y, and Z axes respectively. */

/* $ Detailed_Output */

/*     R        is a rotation matrix representing the composition of the */
/*              rotations defined by the input angle-axis pairs. */
/*              Together, the three pairs specify a composite */
/*              transformation that is the result of performing the */
/*              rotations about the axes indexed by AXIS1, AXIS2, and */
/*              AXIS3, in that order. So, */

/*                 R = [ ANGLE3 ]      [ ANGLE2 ]      [ ANGLE1 ] */
/*                               AXIS3           AXIS2           AXIS1 */

/*              See the $Particulars section below for details concerning */
/*              this notation. */

/*              The resulting matrix R may be thought of as a coordinate */
/*              transformation; applying it to a vector yields the */
/*              vector's coordinates in the rotated system. */

/*              Viewing R as a coordinate transformation matrix, the */
/*              basis that R transforms vectors to is created by rotating */
/*              the original coordinate axes first by ANGLE1 radians */
/*              about the coordinate axis indexed by AXIS1, next by */
/*              ANGLE2 radians about the coordinate axis indexed by */
/*              AXIS2, and finally by ANGLE3 radians about coordinate */
/*              axis indexed by AXIS3. At the second and third steps of */
/*              this process, the coordinate axes about which rotations */
/*              are performed belong to the bases resulting from the */
/*              previous rotations. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If any of AXIS3, AXIS2, or AXIS1 do not have values in */

/*            { 1, 2, 3 } */

/*         the error SPICE(BADAXISNUMBERS) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     A word about notation: the symbol */

/*        [ x ] */
/*             i */

/*     indicates a rotation of x radians about the ith coordinate axis. */
/*     To be specific, the symbol */

/*        [ x ] */
/*             1 */

/*     indicates a coordinate system rotation of x radians about the */
/*     first, or x-, axis; the corresponding matrix is */

/*        .-                    -. */
/*        |  1      0       0    | */
/*        |                      | */
/*        |  0    cos(x)  sin(x) | */
/*        |                      | */
/*        |  0   -sin(x)  cos(x) | */
/*        `-                    -' */

/*     Remember, this is a COORDINATE SYSTEM rotation by x radians; this */
/*     matrix, when applied to a vector, rotates the vector by -x */
/*     radians, not x radians. Applying the matrix to a vector yields */
/*     the vector's representation relative to the rotated coordinate */
/*     system. */

/*     The analogous rotation about the second, or y-, axis is */
/*     represented by */

/*        [ x ] */
/*             2 */

/*     which symbolizes the matrix */

/*        .-                    -. */
/*        | cos(x)   0   -sin(x) | */
/*        |                      | */
/*        |  0       1      0    | */
/*        |                      | */
/*        | sin(x)   0    cos(x) | */
/*        `-                    -' */

/*     and the analogous rotation about the third, or z-, axis is */
/*     represented by */

/*        [ x ] */
/*             3 */

/*     which symbolizes the matrix */

/*        .-                    -. */
/*        |  cos(x)  sin(x)   0  | */
/*        |                      | */
/*        | -sin(x)  cos(x)   0  | */
/*        |                      | */
/*        |  0        0       1  | */
/*        `-                    -' */

/*     From time to time, (depending on one's line of work, perhaps) one */
/*     may happen upon a pair of coordinate systems related by a */
/*     sequence of rotations. For example, the coordinate system */
/*     defined by an instrument such as a camera is sometime specified */
/*     by RA, DEC, and twist; if alpha, delta, and phi are the rotation */
/*     angles, then the series of rotations */

/*        [ phi ]     [ pi/2  -  delta ]     [ alpha ] */
/*               3                      2             3 */

/*     produces a transformation from inertial to camera coordinates. */

/*     This routine is related to the SPICELIB routine M2EUL, which */
/*     produces a sequence of Euler angles, given a rotation matrix. */
/*     This routine is a "left inverse" of M2EUL: the sequence of */
/*     calls */

/*        CALL M2EUL ( R,  AXIS3,   AXIS2,   AXIS1, */
/*       .                 ANGLE3,  ANGLE2,  ANGLE1     ) */

/*        CALL EUL2M (     ANGLE3,  ANGLE2,  ANGLE1, */
/*       .                 AXIS3,   AXIS2,   AXIS1,   R ) */

/*     preserves R, except for round-off error. */


/*     On the other hand, the sequence of calls */

/*        CALL EUL2M (     ANGLE3,  ANGLE2,  ANGLE1, */
/*       .                 AXIS3,   AXIS2,   AXIS1,   R ) */

/*        CALL M2EUL ( R,  AXIS3,   AXIS2,   AXIS1, */
/*       .                 ANGLE3,  ANGLE2,  ANGLE1     ) */

/*     preserve ANGLE3, ANGLE2, and ANGLE1 only if these angles start */
/*     out in the ranges that M2EUL's outputs are restricted to. */

/* $ Examples */

/*     1)  Create a coordinate transformation matrix by rotating */
/*         the original coordinate axes first by 30 degrees about */
/*         the z axis, next by 60 degrees about the y axis resulting */
/*         from the first rotation, and finally by -50 degrees about */
/*         the z axis resulting from the first two rotations. */


/*            C */
/*            C     Create the coordinate transformation matrix */
/*            C */
/*            C                   o          o          o */
/*            C        R  =  [ -50  ]   [  60  ]   [  30  ] */
/*            C                      3          2          3 */
/*            C */
/*            C     All angles in radians, please. The SPICELIB */
/*            C     function RPD (radians per degree) gives the */
/*            C     conversion factor. */
/*            C */
/*            C     The z axis is `axis 3'; the y axis is `axis 2'. */
/*            C */
/*                  ANGLE1 = RPD() *  30.D0 */
/*                  ANGLE2 = RPD() *  60.D0 */
/*                  ANGLE3 = RPD() * -50.D0 */

/*                  AXIS1  = 3 */
/*                  AXIS2  = 2 */
/*                  AXIS3  = 3 */

/*                  CALL EUL2M (  ANGLE3, ANGLE2, ANGLE1, */
/*                 .              AXIS3,  AXIS2,  AXIS1,   R  ) */


/*     2)  A trivial example using actual numbers. */

/*         The code fragment */

/*            CALL EUL2M (  0.D0,     0.D0,     HALFPI(), */
/*           .                 1,        1,            3,      R  ) */

/*         sets R equal to the matrix */

/*            +-                  -+ */
/*            |  0      1       0  | */
/*            |                    | */
/*            | -1      0       0  |. */
/*            |                    | */
/*            |  0      0       1  | */
/*            +-                  -+ */


/*     3)  Finding the rotation matrix specified by a set of `clock, */
/*         cone, and twist' angles, as defined on the Voyager 2 project: */

/*            Voyager 2 narrow angle camera pointing, relative to the */
/*            Sun-Canopus coordinate system, was frequently specified */
/*            by a set of Euler angles called `clock, cone, and twist'. */
/*            These defined a 3-2-3 coordinate transformation matrix */
/*            TSCTV as the product */

/*               [ twist ]  [ cone ]   [ clock ] . */
/*                        3         2           3 */

/*            Given the angles CLOCK, CONE, and TWIST (in units of */
/*            radians), we can compute TSCTV with the code fragment */

/*               CALL EUL2M (  TWIST,  CONE,  CLOCK, */
/*              .              3,      2,     3,      TSCTV  ) */


/*     4)  Finding the rotation matrix specified by a set of `right */
/*         ascension, declination, and twist' angles, as defined on the */
/*         Galileo project: */

/*            Galileo scan platform pointing, relative to an inertial */
/*            reference frame, (EME50 variety) is frequently specified */
/*            by a set of Euler angles called `right ascension (RA), */
/*            declination (Dec), and twist'. These define a 3-2-3 */
/*            coordinate transformation matrix TISP as the product */

/*               [ Twist ]  [ pi/2 - Dec ]   [ RA ] . */
/*                        3               2        3 */

/*            Given the angles RA, DEC, and TWIST (in units of radians), */
/*            we can compute TISP with the code fragment */

/*               CALL EUL2M (  TWIST,   HALFPI()-DEC,   RA, */
/*              .              3,       2,              3,   TISP  ) */

/* $ Restrictions */

/*     1)  Beware: more than one definition of "RA, DEC and twist" */
/*         exists. */

/* $ Literature_References */

/*     [1]  W. Owen, "Galileo Attitude and Camera Models," JPL */
/*          Interoffice Memorandum 314-323, Nov. 11, 1983. NAIF document */
/*          number 204.0. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     L.S. Elson         (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.3.0, 06-JUL-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Removed */
/*        unnecessary entries from $Revisions section. */

/* -    SPICELIB Version 1.2.1, 26-DEC-2006 (NJB) */

/*        Corrected header typo. */

/* -    SPICELIB Version 1.2.0, 25-AUG-2005 (NJB) */

/*        Updated to remove non-standard use of duplicate arguments */
/*        in ROTMAT calls. */

/* -    SPICELIB Version 1.1.2, 14-OCT-2004 (LSE) */

/*        Corrected a typo in the header. */

/* -    SPICELIB Version 1.1.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.1.0, 02-NOV-1990 (NJB) */

/*        Names of input arguments changed to reflect the order in */
/*        which the rotations are applied when their product is */
/*        computed. The header was upgraded to describe notation in */
/*        more detail. Examples were added. */

/* -    SPICELIB Version 1.0.0, 30-AUG-1990 (NJB) */

/* -& */
/* $ Index_Entries */

/*     euler angles to matrix */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 1.1.0, 02-NOV-1990 (NJB) */

/*        Argument names were changed to describe the use of the */
/*        arguments more accurately. The axis and angle numbers */
/*        now decrease, rather than increase, from left to right. */
/*        The current names reflect the order of operator */
/*        application when the Euler angle rotations are applied to */
/*        a vector: the rightmost matrix */

/*           [ ANGLE1 ] */
/*                     AXIS1 */

/*        is applied to the vector first, followed by */

/*           [ ANGLE2 ] */
/*                     AXIS2 */

/*        and then */

/*           [ ANGLE3 ] */
/*                     AXIS3 */

/*        Previously, the names reflected the order in which the */
/*        Euler angle matrices appear on the page, from left to */
/*        right. This naming convention was found to be a bit */
/*        obtuse by a various users. */

/*        No change in functionality was made; the operation of the */
/*        routine is identical to that of the previous version. */

/*        Two new examples were added to assist users in verifying */
/*        their understanding of the routine. */

/*        Also, the header was upgraded to describe the notation in */
/*        more detail. The symbol */

/*           [ x ] */
/*                i */

/*        is explained at mind-numbing length. An example was */
/*        added that shows a specific set of inputs and the */
/*        resulting output matrix. */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("EUL2M", (ftnlen)5);
    }

/*     Make sure the axis numbers are all right:  They must belong to */
/*     the set {1, 2, 3}. */

    badax = *axis3 < 1 || *axis3 > 3 || (*axis2 < 1 || *axis2 > 3) || (*axis1 
	    < 1 || *axis1 > 3);
    if (badax) {
	setmsg_("Axis numbers are #,  #,  #. ", (ftnlen)28);
	errint_("#", axis3, (ftnlen)1);
	errint_("#", axis2, (ftnlen)1);
	errint_("#", axis1, (ftnlen)1);
	sigerr_("SPICE(BADAXISNUMBERS)", (ftnlen)21);
	chkout_("EUL2M", (ftnlen)5);
	return 0;
    }

/*     Just do it. */

    rotate_(angle1, axis1, r__);
    rotmat_(r__, angle2, axis2, r1);
    rotmat_(r1, angle3, axis3, r__);
    chkout_("EUL2M", (ftnlen)5);
    return 0;
} /* eul2m_ */

