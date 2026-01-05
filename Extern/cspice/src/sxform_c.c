/*

-Procedure sxform_c ( State Transformation Matrix )

-Abstract

   Return the state transformation matrix from one frame to
   another at a specified epoch.

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


   void sxform_c ( ConstSpiceChar  * from,
                   ConstSpiceChar  * to,
                   SpiceDouble       et,
                   SpiceDouble       xform[6][6] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   from       I   Name of the frame to transform from.
   to         I   Name of the frame to transform to.
   et         I   Epoch of the state transformation matrix.
   xform      O   A state transformation matrix.

-Detailed_Input

   from        is the name of a reference frame in which a state is
               known.

   to          is the name of a reference frame in which it is desired
               to represent the state.

   et          is the epoch in ephemeris seconds past the epoch of
               J2000 (TDB) at which the state transformation matrix
               should be evaluated.

-Detailed_Output

   xform       is the matrix that transforms states from the reference
               frame `from' to the frame `to' at epoch `et'. If (x, y,
               z, dx, dy, dz) is a state relative to the frame `from'
               then the vector ( x', y', z', dx', dy', dz' ) is the
               same state relative to the frame `to' at epoch `et'.
               Here the vector ( x', y', z', dx', dy', dz' ) is defined
               by the equation:

                  -   -       -          -     -  -
                 | x'  |     |            |   | x  |
                 | y'  |     |            |   | y  |
                 | z'  |  =  |   xform    |   | z  |
                 | dx' |     |            |   | dx |
                 | dy' |     |            |   | dy |
                 | dz' |     |            |   | dz |
                  -   -       -          -     -  -

-Parameters

   None.

-Exceptions

   1)  If sufficient information has not been supplied via loaded
       SPICE kernels to compute the transformation between the two
       frames, an error is signaled by a routine in the call tree of
       this routine.

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

   This routine provides the user level interface for computing
   state transformations from one reference frame to another.

   Note that the reference frames may be inertial or non-inertial.
   However, the user must take care that sufficient SPICE kernel
   information is loaded to provide a complete state transformation
   path from the `from' frame to the `to' frame.

-Examples

   Suppose that you have geodetic coordinates of a station on
   the surface of the earth and that you need the inertial
   (J2000) state of this station. The following code fragment
   illustrates how to transform the position of the station to
   a J2000 state.

      #include "SpiceUsr.h"
          .
          .
          .
      bodvcd_c ( 399, radii, 3, &n, abc  );

      equatr   =  abc[0];
      polar    =  abc[2];
      f        = (equatr - polar) / equatr;

      georec_c ( long, lat, 0.0,  equatr, f,  estate );

      estate[3] = 0.0;
      estate[4] = 0.0;
      estate[5] = 0.0;

      sxform_c ( "IAU_EARTH", "J2000",   et,    xform  );
      mxvg_c   (  xform,       estate,   6,  6, jstate );

   The state `jstate' is the desired J2000 state of the station.

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

-Version

   -CSPICE Version 1.1.4, 10-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard.

       Added entries #3 and #4 in -Exceptions section.

   -CSPICE Version 1.1.3, 27-FEB-2008 (BVS)

       Added FRAMES to the -Required_Reading section of the header.

   -CSPICE Version 1.1.2, 24-OCT-2005 (NJB)

       Header updates: example had invalid flattening factor
       computation; this was corrected. Reference to bodvar_c was
       replaced with reference to bodvcd_c.

   -CSPICE Version 1.1.1, 03-JUL-2003 (NJB) (CHA)

       Various header corrections were made.

   -CSPICE Version 1.1.0, 08-FEB-1998 (NJB)

       References to C2F_CreateStr_Sig were removed; code was
       cleaned up accordingly. String checks are now done using
       the macro CHKFSTR.

   -CSPICE Version 1.0.0, 25-OCT-1997 (NJB) (WLT)

      Based on SPICELIB Version 1.0.0, 19-SEP-1995 (WLT)

-Index_Entries

   Find a state transformation matrix

-&
*/

{ /* Begin sxform_c */

   /*
   Participate in error tracing.
   */
   chkin_c ( "sxform_c");


   /*
   Check the input strings to make sure the pointers are non-null
   and the string lengths are non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "sxform_c", from );
   CHKFSTR ( CHK_STANDARD, "sxform_c", to   );


   /*
   Get the desired matrix from sxform_.
   */
   sxform_ (  ( char           * ) from,
              ( char           * ) to,
              ( doublereal     * ) &et,
              ( doublereal     * ) xform,
              ( ftnlen           ) strlen(from),
              ( ftnlen           ) strlen(to)    );

   /*
   Transpose the matrix on output.
   */
   xpose6_c ( xform, xform );


   chkout_c ( "sxform_c");

} /* End sxform_c */
