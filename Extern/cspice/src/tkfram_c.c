/*

-Procedure tkfram_c ( TK frame, find position rotation )

-Abstract

   Find the position rotation matrix from a Text Kernel (TK) frame
   with the specified frame class ID to its base frame.

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

   POINTING

*/
   #include "SpiceUsr.h"
   #include "SpiceZfc.h"

   void tkfram_c ( SpiceInt            frcode,
                   SpiceDouble         rot    [3][3],
                   SpiceInt          * frame,
                   SpiceBoolean      * found         )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   frcode     I   Frame class ID of a TK frame.
   rot        O   Rotation matrix from TK frame to frame `frame'.
   frame      O   Frame ID of the base reference.
   found      O   SPICETRUE if the rotation could be determined.

-Detailed_Input

   frcode      is the unique frame class ID of the TK frame for which
               data is being requested. For TK frames the frame class
               ID is always equal to the frame ID.

-Detailed_Output

   rot         is a position rotation matrix that converts positions
               relative to the TK frame given by its frame class ID,
               `frcode', to positions relative to the base frame given by
               its frame ID, `frame'.

               Thus, if a position S has components x,y,z in the TK
               frame, then S has components x', y', z' in the base
               frame.

                  .-  -.     .-     -. .- -.
                  | x' |     |       | | x |
                  | y' |  =  |  rot  | | y |
                  | z' |     |       | | z |
                  `-  -'     `-     -' `- -'


   frame       is the ID code of the base reference frame to which `rot'
               will transform positions.

   found       is a logical indicating whether or not a frame definition for
               the TK frame with the frame class ID, `frcode', was
               constructed from kernel pool data. If `rot' and `frame' were
               constructed, `found' will be returned with the value
               SPICETRUE. Otherwise it will be returned with the value
               SPICEFALSE.

-Parameters

   None.

-Exceptions

   1)  If some kernel variable associated with this frame is not
       present in the kernel pool, or does not have the proper type
       or dimension, an error is signaled by a routine in the call
       tree of this routine. In such a case `found' will be set to
       SPICEFALSE.

   2)  If the input `frcode' has the value 0, the error
       SPICE(ZEROFRAMEID) is signaled by a routine in the call tree
       of this routine. `found' will be set to SPICEFALSE.

   3)  If the name of the frame corresponding to `frcode' cannot be
       determined, the error SPICE(INCOMPLETEFRAME) is signaled by a
       routine in the call tree of this routine.

   4)  If the frame given by `frcode' is defined relative to a frame
       that is unrecognized, the error SPICE(BADFRAMESPEC) is
       signaled by a routine in the call tree of this routine. `found'
       will be set to SPICEFALSE.

   5)  If the kernel pool specification for the frame given by `frcode'
       is not one of 'MATRIX', 'ANGLES' or 'QUATERNION', the error
       SPICE(UNKNOWNFRAMESPEC) is signaled by a routine in the call
       tree of this routine. `found' will be set to SPICEFALSE.

   6)  If the frame `frcode' is equal to the relative frame ID (i.e.
       the frame is defined relative to itself), the error
       SPICE(BADFRAMESPEC2) is signaled by a routine in the call tree
       of this routine. `found' will be set to SPICEFALSE.

   7)  If name-based and ID-based forms of any TKFRAME_ keyword are
       detected in the kernel pool at the same time, the error
       SPICE(COMPETINGFRAMESPEC) is signaled by a routine in the call
       tree of this routine. `found' will be set to SPICEFALSE.

-Files

   This routine makes use of the loaded text kernels to determine
   the rotation from a constant offset TK frame to its base frame.

-Particulars

   This routine is used to construct the rotation from some frame
   that is a constant rotation offset from some other reference
   frame. This rotation is derived from data stored in the kernel
   pool.

   This routine is intended to be used as a low level routine by the
   frame system software. However, you could use this routine to
   directly retrieve the rotation from a fixed offset TK frame to
   its base frame.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Compute the rotation from the DSS-34 topocentric frame to
      its base Earth body-fixed frame and use it to determine the
      geodetic latitude and longitude of the DSS-34 site.


      Use the FK kernel below to load the required topocentric
      reference frame definition for the DSS-34 site.

         earth_topo_050714.tf


      Example code begins here.


      /.
         Program tkfram_ex1
      ./
      #include <stdio.h>
      #include <stdlib.h>
      #include <string.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local parameters
         ./
         #define MYTOPO       "DSS-34_TOPO"
         #define MXFRLN       27

         /.
         Local variables
         ./
         SpiceChar            frname [MXFRLN];

         SpiceDouble          lat;
         SpiceDouble          lon;
         SpiceDouble          rad;
         SpiceDouble          rot    [3][3];
         SpiceDouble          z      [3];

         SpiceInt             frame;
         SpiceInt             frcode;

         SpiceBoolean         found;

         /.
         Load the FK that contains the topocentric reference
         frame definition for DSS-34.
         ./
         furnsh_c ( "earth_topo_050714.tf" );

         /.
         The name of the topocentric frame is MYTOPO.
         First we get the ID code of the topocentric frame.
         ./
         namfrm_c ( MYTOPO, &frcode );

         /.
         Next get the rotation from the topocentric frame to
         the body-fixed frame. We can use the TK frame ID in
         place of the TK frame class ID in this call because
         for TK frames these IDs are identical.
         ./
         tkfram_c ( frcode, rot, &frame, &found );

         /.
         Make sure the topocentric frame is relative to one of
         the Earth fixed frames.
         ./
         frmnam_c ( frame, MXFRLN, frname );

         if (    strncmp( frname, "IAU_EARTH", 10 )
              && strncmp( frname, "EARTH_FIXED", 12 )
              && strncmp( frname, "ITRF93", 7 )       )
         {
            printf( "The frame %s does not appear to be\n", MYTOPO );
            printf( "defined relative to an Earth fixed frame.\n" );
            exit( EXIT_FAILURE );
         }

         /.
         Things look ok. Get the location of the Z-axis in the
         topocentric frame.
         ./
         z[0] = rot[0][2];
         z[1] = rot[1][2];
         z[2] = rot[2][2];

         /.
         Convert the `z' vector to latitude, longitude and radius.
         ./
         reclat_c ( z, &rad, &lat, &lon );

         printf( "The geodetic coordinates of the center\n" );
         printf( "of the topographic frame are:\n" );
         printf( "\n" );
         printf( "   Latitude  (deg):  %19.13f\n", lat*dpr_c() );
         printf( "   Longitude (deg):  %19.13f\n", lon*dpr_c() );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      The geodetic coordinates of the center
      of the topographic frame are:

         Latitude  (deg):    148.9819650021110
         Longitude (deg):    -35.3984778756552


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.0.0, 05-AUG-2021 (JDR)

-Index_Entries

   Fetch the rotation and frame of a text kernel frame
   Fetch the rotation and frame of a constant offset frame

-&
*/

{ /* Begin tkfram_c */

   /*
   Local variables.
   */
   logical            logCfound;

   /*
   Participate in error tracing.
   */
   chkin_c ( "tkfram_c" );

   /*
   Call the f2c'd Fortran routine.
   */
   tkfram_ (  ( integer    * ) &frcode,
              ( doublereal * )  rot,
              ( integer    * )  frame,
              ( logical    * ) &logCfound  );

   /*
   Transpose the output matrix to put it in row-major order.
   */
   xpose_c ( rot, rot );

   /*
   Set the output SpiceBoolean `found' flag.
   */
   *found = (SpiceBoolean)logCfound;

   chkout_c ( "tkfram_c" );

} /* End tkfram_c */
