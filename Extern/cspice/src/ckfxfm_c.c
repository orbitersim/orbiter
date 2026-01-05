/*

-Procedure ckfxfm_c ( CK frame, find state transformation )

-Abstract

   Find the state transformation matrix from a C-kernel (CK) frame
   with the specified frame class ID (CK ID) to the base frame of
   the highest priority CK segment containing orientation and
   angular velocity data for this CK frame at the time requested.

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

   CK

-Keywords

   POINTING

*/
   #include "SpiceUsr.h"
   #include "SpiceZfc.h"

   void ckfxfm_c ( SpiceInt            inst,
                   SpiceDouble         et,
                   SpiceDouble         xform  [6][6],
                   SpiceInt          * ref,
                   SpiceBoolean      * found         )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   inst       I   Frame class ID (CK ID) of a CK frame.
   et         I   Epoch measured in seconds past J2000 TDB.
   xform      O   Transformation from CK frame to frame `ref'.
   ref        O   Frame ID of the base reference.
   found      O   SPICETRUE when requested pointing is available.

-Detailed_Input

   inst        is the unique frame class ID (CK ID) of the CK frame for
               which data is being requested.

   et          is the epoch for which the state transformation is
               desired. `et' should be given in seconds past the epoch of
               J2000 TDB.

-Detailed_Output

   xform       is a state transformation matrix that converts states
               relative to the CK frame given by its frame class ID,
               `inst', to states relative to the base frame given by its
               frame ID, `ref'.

               Thus, if a state S has components x, y, z, dx, dy, dz in
               the CK frame, then S has components x', y', z', dx',
               dy', dz' in the base frame `ref'.

                  .-   -.     .-         -. .-  -.
                  |  x' |     |           | |  x |
                  |  y' |     |           | |  y |
                  |  z' |     |           | |  z |
                  | dx' |  =  |   xform   | | dx |
                  | dy' |     |           | | dy |
                  | dz' |     |           | | dz |
                  `-   -'     `-         -' `-  -'


   ref         is the ID code of the base reference frame to which
               `xform' will transform states.

   found       is SPICETRUE if a record was found to satisfy the pointing
               request. `found' will be SPICEFALSE otherwise.

-Parameters

   None.

-Exceptions

   1)  If no CK files were loaded prior to calling this routine, an
       error is signaled by a routine in the call tree of this
       routine.

   2)  If no SCLK correlation data needed to read CK files were
       loaded prior to calling this routine, an error is signaled by
       a routine in the call tree of this routine.

   3)  If the input time `et' cannot be converted to an encoded SCLK
       time, using SCLK data associated with `inst', an error is
       signaled by a routine in the call tree of this routine.

-Files

   ckfxfm_c searches through loaded CK files to locate a segment that
   can satisfy the request for state transformation data for the CK
   frame with the specified frame class ID at time `et'. You must load
   a CK file containing such data before calling this routine. You
   must also load SCLK and possibly LSK files needed to convert the
   input `et' time to the encoded SCLK time with which the orientation
   data stored inside that CK is tagged.

-Particulars

   ckfxfm_c searches through loaded CK files to satisfy a pointing
   request. Last-loaded files are searched first, and individual
   files are searched in backwards order, giving priority to
   segments that were added to a file later than the others.

   The search ends when a segment is found that can give pointing
   for the specified CK frame at the request time.

   Only segments with angular velocities are considered by this
   routine.

   This routine uses the ckmeta_c routine to determine the SCLK ID
   used to convert the input `et' time to the encoded SCLK time used
   to look up pointing data in loaded CK files.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Use ckfxfm_c to compute the angular rate of rotation for the Mars
      Global Surveyor (MGS) spacecraft frame, "MGS_SPACECRAFT",
      relative to the inertial frame used as the base frame in CK
      files containing MGS spacecraft orientation at 2003-JUL-25
      13:00:00. The frame class ID (CK ID) for the "MGS_SPACECRAFT"
      frame is -94000.

      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File name: ckfxfm_ex1.tm

         This meta-kernel is intended to support operation of SPICE
         example programs. The kernels shown here should not be
         assumed to contain adequate or correct versions of data
         required by SPICE-based user applications.

         In order for an application to use this meta-kernel, the
         kernels referenced here must be present in the user's
         current working directory.

         The names and contents of the kernels referenced
         by this meta-kernel are as follows:

            File name                     Contents
            ---------                     --------
            naif0012.tls                  Leapseconds
            mgs_sclkscet_00061.tsc        MGS SCLK coefficients
            mgs_sc_ext12.bc               MGS s/c bus attitude

         \begindata

         KERNELS_TO_LOAD = ( 'naif0012.tls',
                             'mgs_sclkscet_00061.tsc',
                             'mgs_sc_ext12.bc' )

         \begintext

         End of meta-kernel


      Example code begins here.


      /.
         Program ckfxfm_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local parameters.
         ./
         #define EPOCH        "2003-JUL-25 13:00:00"
         #define INST         -94000

         /.
         Local variables.
         ./
         SpiceDouble          av     [3];
         SpiceDouble          et;
         SpiceDouble          rot    [3][3];
         SpiceDouble          xform  [6][6];

         SpiceInt             ref;

         SpiceBoolean         found;

         /.
         Load the required LSK, SCLK and CK. Use a
         meta-kernel for convenience.
         ./
         furnsh_c ( "ckfxfm_ex1.tm" );

         /.
         First convert the time to seconds past J2000.
         ./
         str2et_c ( EPOCH, &et );

         /.
         Now, look up the state transformation from the MGS
         spacecraft frame specified by its frame class ID
         (CK ID) to a base reference frame (returned by
         ckfxfm_c), at `et'.
         ./
         ckfxfm_c ( INST, et, xform, &ref, &found );

         /.
         Next determine the angular velocity of the
         transformation.
         ./
         xf2rav_c ( xform, rot, av );

         /.
         The angular rate of change (in radians/second) is just
         the magnitude of `av'.
         ./
         printf( "Angular rate of change (rad/s): %19.16f\n",
                                               vnorm_c ( av ) );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Angular rate of change (rad/s):  0.0008907319999591


-Restrictions

   1)  A CK file must be loaded prior to calling this routine.

   2)  LSK and SCLK files needed for time conversions must be loaded
       prior to calling this routine.

-Literature_References

   None.

-Author_and_Institution

   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.0.0, 13-DEC-2021 (JDR)

-Index_Entries

   get instrument frame transformation and reference frame

-&
*/

{ /* Begin ckfxfm_c */

   /*
   Local variables.
   */
   logical            logCfound;

   /*
   Participate in error tracing.
   */
   chkin_c ( "ckfxfm_c" );

   /*
   Call the f2c'd Fortran routine.
   */
   ckfxfm_ (  ( integer    * ) &inst,
              ( doublereal * ) &et,
              ( doublereal * )  xform,
              ( integer    * )  ref,
              ( logical    * ) &logCfound  );

   /*
   Transpose the output matrix to put it in row-major order.
   */
   xpose6_c ( xform, xform );

   /*
   Set the output SpiceBoolean `found' flag.
   */
   *found = (SpiceBoolean)logCfound;

   chkout_c ( "ckfxfm_c" );

} /* End ckfxfm_c */
