/*

-Procedure pxform_c ( Position Transformation Matrix )

-Abstract

   Return the matrix that transforms position vectors from one
   specified frame to another at a specified epoch.

-Disclaimer

   THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
   CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
   GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
   ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
   PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
   TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
   WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
   PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
   SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
   SOFTWARE AND RELATED MATERIALS, HOWEVER USED.

   IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
   BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
   LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
   INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
   REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
   REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.

   RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
   THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
   CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
   ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.

-Required_Reading

   FRAMES

-Keywords

   FRAMES

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"
   #include "SpiceZst.h"


   void pxform_c ( ConstSpiceChar   * from,
                   ConstSpiceChar   * to,
                   SpiceDouble        et,
                   SpiceDouble        rotate[3][3] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   from       I   Name of the frame to transform from.
   to         I   Name of the frame to transform to.
   et         I   Epoch of the rotation matrix.
   rotate     O   A rotation matrix.

-Detailed_Input

   from        is the name of a reference frame in which a position
               vector is known.

   to          is the name of a reference frame in which it is desired
               to represent a position vector.

   et          is the epoch in ephemeris seconds past the epoch of J2000
               (TDB) at which the position transformation matrix `rotate'
               should be evaluated.

-Detailed_Output

   rotate      is the matrix that transforms position vectors from
               the reference frame `from' to the frame `to' at epoch `et'.
               If (x, y, z) is a position relative to the frame `from'
               then the vector ( x', y', z') is the same position
               relative to the frame `to' at epoch `et'. Here the
               vector ( x', y', z' ) is defined by the equation:

                  .-   -.     .-        -.   .-  -.
                  | x'  |     |          |   | x  |
                  | y'  |  =  |  rotate  |   | y  |
                  | z'  |     |          |   | z  |
                  `-   -'     `-        -'   `-  -'

-Parameters

   None.

-Exceptions

   1)  If sufficient information has not been supplied via loaded
       SPICE kernels to compute the transformation between the
       two frames, an error is signaled by a routine
       in the call tree of this routine.

   2)  If either frame `from' or `to' is not recognized, the error
       SPICE(UNKNOWNFRAME) is signaled by a routine in the call tree
       of this routine.

   3)  If any of the `from' or `to' input string pointers is null,
       the error SPICE(NULLPOINTER) is signaled.

   4)  If any of the `from' or `to' input strings has zero length,
       the error SPICE(EMPTYSTRING) is signaled.

-Files

   None.

-Particulars

   This routine provides the user level interface to computing
   position transformations from one reference frame to another.

   Note that the reference frames may be inertial or non-inertial.
   However, the user must take care that sufficient SPICE kernel
   information is loaded to provide a complete position
   transformation path from the `from' frame to the `to' frame.

   A common type of reference frame transformation is one from one
   time-dependent frame to another, where the orientations of the
   frames are computed at different epochs. For example, a remote
   sensing application may compute the transformation from a target
   body-fixed frame, with its orientation evaluated at the epoch of
   photon emission, to a spacecraft instrument frame, with its
   orientation evaluated at the epoch of photon reception. The
   CSPICE routine pxfrm2_c computes this type of frame
   transformation.

-Examples

   Suppose that you have geodetic coordinates of a station on the
   surface of the earth and that you need the inertial (J2000)
   position of this station. The following code fragment
   illustrates how to transform the position of the station to a
   J2000 position.

      #include "SpiceUsr.h"
            .
            .
            .
      bodvcd_c ( 399, radii, 3, &n, abc );

      equatr   =  abc[0];
      polar    =  abc[2];
      f        =  ( equatr - polar ) / equatr;

      georec_c ( long,        lat,      0.0,   equatr,   f,   epos );
      pxform_c ( "IAU_EARTH", "J2000",  et,    rotate              );
      mxv_c    (  rotate,   epos,     jpos                         );

   The position jpos is the desired J2000 position of the station.

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   C.H. Acton          (JPL)
   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   B.V. Semenov        (JPL)
   W.L. Taber          (JPL)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.0.5, 10-AUG-2021 (JDR) (NJB)

       Edited the header to comply with NAIF standard.

       Added entries #3 and #4 to -Exceptions section. Updated the
       -Particulars section to mention pxfrm2_c.

   -CSPICE Version 1.0.4, 27-FEB-2008 (BVS)

       Added FRAMES to the -Required_Reading section of the header.

   -CSPICE Version 1.0.3, 24-OCT-2005 (NJB)

       Header updates: example had invalid flattening factor
       computation; this was corrected. Reference to bodvar_c was
       replaced with reference to bodvcd_c.

   -CSPICE Version 1.0.2, 07-JAN-2004 (EDW)

       Trivial typo correction to example section.

   -CSPICE Version 1.0.1, 29-JUL-2003 (NJB) (CHA)

       Various header corrections were made.

   -CSPICE Version 1.0.0, 20-JUN-1999 (NJB) (WLT)

-Index_Entries

   Find a position transformation matrix

-&
*/

{ /* Begin pxform_c */


   /*
   Participate in error tracing.
   */
   chkin_c ( "pxform_c" );


   /*
   Check the input strings to make sure the pointers are non-null
   and the string lengths are non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "pxform_c", from );
   CHKFSTR ( CHK_STANDARD, "pxform_c", to   );

   /*
   Call the f2c'd routine.
   */
   pxform_ ( ( char       * ) from,
             ( char       * ) to,
             ( doublereal * ) &et,
             ( doublereal * ) rotate,
             ( ftnlen       ) strlen(from),
             ( ftnlen       ) strlen(to)    );


   /*
   Transpose the output to obtain row-major order.
   */
   xpose_c ( rotate, rotate );


   chkout_c ( "pxform_c" );

} /* End pxform_c */
