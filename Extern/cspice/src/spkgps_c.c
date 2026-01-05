/*

-Procedure spkgps_c ( S/P Kernel, geometric position )

-Abstract

   Compute the geometric position of a target body relative to an
   observing body.

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

   SPK

-Keywords

   EPHEMERIS

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"


   void spkgps_c ( SpiceInt           targ,
                   SpiceDouble        et,
                   ConstSpiceChar   * ref,
                   SpiceInt           obs,
                   SpiceDouble        pos[3],
                   SpiceDouble      * lt     )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   targ       I   Target body.
   et         I   Target epoch.
   ref        I   Target reference frame.
   obs        I   Observing body.
   pos        O   Position of target.
   lt         O   Light time.

-Detailed_Input

   targ        is the standard NAIF ID code for a target body.

   et          is the epoch (ephemeris time) at which the position
               of the target body is to be computed.

   ref         is the name of the reference frame to
               which the vectors returned by the routine should
               be rotated. This may be any frame supported by
               the CSPICE subroutine sxform_c.

   obs         is the standard NAIF ID code for an observing body.

-Detailed_Output

   pos         contains the position of the target
               body, relative to the observing body. This vector is
               rotated into the specified reference frame. Units
               are always km.

   lt          is the one-way light time from the observing body
               to the geometric position of the target body at the
               specified epoch.

-Parameters

   None.

-Exceptions

   1)  If insufficient ephemeris data has been loaded to compute
       the necessary positions, the error SPICE(SPKINSUFFDATA) is
       signaled by a routine in the call tree of this routine.

   2)  If the `ref' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   3)  If the `ref' input string has zero length, the error
       SPICE(EMPTYSTRING) is signaled.

-Files

   See -Restrictions.

-Particulars

   spkgps_c computes the geometric position, T(t), of the target
   body and the geometric position, O(t), of the observing body
   relative to the first common center of motion. Subtracting
   O(t) from T(t) gives the geometric position of the target
   body relative to the observer.


      CENTER ----- O(t)
          |      /
          |     /
          |    /
          |   /  T(t) - O(t)
          |  /
         T(t)


   The one-way light time, tau, is given by


             | T(t) - O(t) |
      tau = -----------------
                    c


   For example, if the observing body is -94, the Mars Observer
   spacecraft, and the target body is 401, Phobos, then the
   first common center is probably 4, the Mars Barycenter.
   O(t) is the position of -94 relative to 4 and T(t) is the
   position of 401 relative to 4.

   The center could also be the Solar System Barycenter, body 0.
   For example, if the observer is 399, Earth, and the target
   is 299, Venus, then O(t) would be the position of 399 relative
   to 0 and T(t) would be the position of 299 relative to 0.

   Ephemeris data from more than one segment may be required
   to determine the positions of the target body and observer
   relative to a common center. spkgps_c reads as many segments
   as necessary, from as many files as necessary, using files
   that have been loaded by previous calls to spklef_c (load
   ephemeris file).

   spkgps_c is similar to spkgeo_c but returns geometric positions
   only.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Return the geometric position vector of Mars (499) as seen from
      Earth (399) in the J2000 frame and the one-way light time
      between them at the epoch July 4, 2003 11:00 AM PST.

      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File: spkgps_ex1.tm

         This meta-kernel is intended to support operation of SPICE
         example programs. The kernels shown here should not be
         assumed to contain adequate or correct versions of data
         required by SPICE-based user applications.

         In order for an application to use this meta-kernel, the
         kernels referenced here must be present in the user's
         current working directory.

         The names and contents of the kernels referenced
         by this meta-kernel are as follows:

            File name                        Contents
            ---------                        --------
            de430.bsp                        Planetary ephemeris
            mar097.bsp                       Mars satellite ephemeris
            naif0011.tls                     Leapseconds


         \begindata

            KERNELS_TO_LOAD = ( 'de430.bsp',
                                'mar097.bsp',
                                'naif0011.tls' )

         \begintext

         End of meta-kernel


      Example code begins here.


      /.
         Program spkgps_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main()
      {
         /.
         Local variables.
         ./
         SpiceChar             * epoch;
         SpiceChar             * reffrm;

         SpiceDouble             et;
         SpiceDouble             lt;
         SpiceDouble             pos    [3];

         SpiceInt                obsrvr;
         SpiceInt                target;

         /.
         Load kernels.
         ./
         furnsh_c ( "spkgps_ex1.tm" );


         /.
         Define parameters for a position lookup:
         ./
         target = 499;
         epoch  = "July 4, 2003 11:00 AM PST";
         reffrm = "J2000";
         obsrvr = 399;

         /.
         Convert the epoch to ephemeris time.
         ./
         str2et_c ( epoch, &et );

         /.
         Look-up the state for the defined parameters.
         ./
         spkgps_c ( target, et, reffrm, obsrvr, pos, &lt );

         /.
         Output...
         ./
         printf( "The position of    : %d\n", target );
         printf( "As observed from   : %d\n", obsrvr );
         printf( "In reference frame : %s\n", reffrm );
         printf( "At epoch           : %s\n", epoch  );
         printf( "\n" );
         printf( "   R   (km): %17.6f %17.6f %17.6f\n",
                               pos[0], pos[1], pos[2] );
         printf( "\n" );
         printf( "Light time (s) between observer and target: %18.6f\n",
                                                              lt       );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      The position of    : 499
      As observed from   : 399
      In reference frame : J2000
      At epoch           : July 4, 2003 11:00 AM PST

         R   (km):   73826216.435288  -27128030.732406  -18741973.868287

      Light time (s) between observer and target:         269.702648


-Restrictions

   1)  The ephemeris files to be used by spkgps_c must be loaded
       by spklef_c before spkgps_c is called.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   J.E. McLean         (JPL)
   W.L. Taber          (JPL)

-Version

   -CSPICE Version 1.0.1, 10-AUG-2021 (JDR)

       Edited the header to comply to NAIF standard. Added
       complete code example, problem statement and solution.

       Added entries #2 and #3 in -Exceptions section.

   -CSPICE Version 1.0.0, 30-MAY-1999 (NJB) (JEM) (WLT)

-Index_Entries

   geometric position of one body relative to another

-&
*/

{ /* Begin spkgps_c */



   /*
   Participate in error tracing.
   */
   chkin_c ( "spkgps_c" );


   /*
   Check the input string ref to make sure the pointer is non-null
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "spkgps_c", ref );

   /*
   Call the f2c'd routine.
   */

   spkgps_ (  ( integer     * ) &targ,
              ( doublereal  * ) &et,
              ( char        * ) ref,
              ( integer     * ) &obs,
              ( doublereal  * ) pos,
              ( doublereal  * ) lt,
              ( ftnlen        ) strlen(ref)  );

   chkout_c ( "spkgps_c" );

} /* End spkgps_c */
