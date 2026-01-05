/*

-Procedure spkgeo_c ( S/P Kernel, geometric state )

-Abstract

   Compute the geometric state (position and velocity) of a target
   body relative to an observing body.

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
   #include "SpiceZmc.h"
   #include "SpiceZfc.h"

   void spkgeo_c ( SpiceInt          targ,
                   SpiceDouble       et,
                   ConstSpiceChar  * ref,
                   SpiceInt          obs,
                   SpiceDouble       state[6],
                   SpiceDouble     * lt       )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   targ       I   Target body.
   et         I   Target epoch.
   ref        I   Target reference frame.
   obs        I   Observing body.
   state      O   State of target.
   lt         O   Light time.

-Detailed_Input

   targ        is the standard NAIF ID code for a target body.

   et          is the epoch (ephemeris time) at which the state
               of the target body is to be computed.

   ref         is the name of the reference frame to which the state
               vector returned by the routine should be rotated. This
               may be any frame supported by the SPICELIB subroutine
               FRMCHG. See also the Frames Required Reading for a list
               of supported frames.

   obs         is the standard NAIF ID code for an observing body.

-Detailed_Output

   state       contains the geometric position and velocity of the
               target body, relative to the observing body, at epoch
               `et'. `state' has six elements: the first three contain
               the target's position; the last three contain the
               target's velocity. These vectors are transformed into
               the specified reference frame.

               Units are always km and km/sec.

   lt          is the one-way light time from the observing body
               to the geometric position of the target body
               in seconds at the specified epoch.

-Parameters

   None.

-Exceptions

   1)  If insufficient ephemeris data has been loaded to compute
       the necessary states, the error SPICE(SPKINSUFFDATA) is
       signaled by a routine in the call tree of this routine.

   2)  If the `ref' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   3)  If the `ref' input string has zero length, the error
       SPICE(EMPTYSTRING) is signaled.

-Files

   See -Restrictions.

-Particulars

   spkgeo_c computes the geometric state, targ(t), of the target
   body and the geometric state, obs(t), of the observing body
   relative to the first common center of motion. Subtracting
   obs(t) from targ(t) gives the geometric state of the target
   body relative to the observer.


      center ----- obs(t)
          |      /
          |     /
          |    /
          |   /  targ(t) - obs(t)
          |  /
        targ(t)


   The one-way light time, tau, is given by


             | targ(t) - obs(t) |
      tau = ----------------------
                      C


   For example, if the observing body is -94, the Mars Observer
   spacecraft, and the target body is 401, Phobos, then the
   first common center is probably 4, the Mars Barycenter.
   obs(t) is the state of -94 relative to 4 and targ(t) is the
   state of 401 relative to 4.

   The center could also be the Solar System Barycenter, body 0.
   For example, if the observer is 399, Earth, and the target
   is 299, Venus, then obs(t) would be the state of 399 relative
   to 0 and targ(t) would be the state of 299 relative to 0.

   Ephemeris data from more than one segment may be required
   to determine the states of the target body and observer
   relative to a common center. spkgeo_c reads as many segments
   as necessary, from as many files as necessary, using files
   that have been loaded by previous calls to furnsh_c or
   spklef_c (load ephemeris file).

   spkgeo_c is similar to spkez_c but returns geometric states
   only, with no option to make planetary (light-time) nor
   stellar aberration corrections. The geometric states
   returned by spkez_c and spkgeo_c are the same.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.


   1) Return the geometric state vector of Mars (499) as seen from
      Earth (399) in the J2000 frame and the one-way light time
      between them at the epoch July 4, 2003 11:00 AM PST.

      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File: spkgeo_ex1.tm

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
         Program spkgeo_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local variables.
         ./
         SpiceChar          * epoch;
         SpiceChar          * reffrm;

         SpiceDouble          et;
         SpiceDouble          lt;
         SpiceDouble          state  [6];

         SpiceInt             obsrvr;
         SpiceInt             target;

         /.
         Load a set of kernels. Use a meta
         kernel for convenience.
         ./
         furnsh_c ( "spkgeo_ex1.tm" );

         /.
         Define parameters for a state lookup.
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
         spkgeo_c ( target, et, reffrm, obsrvr, state, &lt );

         /.
         Output...
         ./
         printf( "The position of    : %2d\n", target );
         printf( "As observed from   : %2d\n", obsrvr );
         printf( "In reference frame : %s\n", reffrm );
         printf( "At epoch           : %s\n", epoch );
         printf( " \n" );

         /.
         The first three entries of state contain the
         X, Y, Z position components. The final three contain
         the Vx, Vy, Vz velocity components.
         ./
         printf( "R   (km): %17.5f %17.5f %17.5f\n",
                        state[0], state[1], state[2] );
         printf( "V (km/s): %17.5f %17.5f %17.5f\n",
                        state[3], state[4], state[5] );
         printf( " \n" );
         printf( "Light time (s) between observer and target:"
                 "  %18.13f\n", lt );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      The position of    : 499
      As observed from   : 399
      In reference frame : J2000
      At epoch           : July 4, 2003 11:00 AM PST

      R   (km):    73826216.43529   -27128030.73241   -18741973.86829
      V (km/s):          -6.80950           7.51381           3.00129

      Light time (s) between observer and target:   269.7026477631753


-Restrictions

   1)  The ephemeris files to be used by spkgeo_c must be loaded
       by furnsh_c or spklef_c before spkgeo_c is called.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   J.E. McLean         (JPL)
   B.V. Semenov        (JPL)
   W.L. Taber          (JPL)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.1.3, 10-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard.
       Added complete code example to -Examples section.

       Added reference to furnsh_c in -Particulars and -Restrictions
       sections.

       Added entries #2 and #3 in -Exceptions section.

   -CSPICE Version 1.1.2, 08-JAN-2014 (BVS)

       Fixed description of "state" in -Detailed_Output. Replaced
       spklef_c with furnsh_c and fixed errors in -Examples.

   -CSPICE Version 1.1.1, 13-OCT-2003 (EDW)

       Various minor header changes were made to improve clarity.
       Added mention that 'lt' returns a value in seconds.

   -CSPICE Version 1.1.0, 08-FEB-1998 (NJB)

       References to C2F_CreateStr_Sig were removed; code was
       cleaned up accordingly. String checks are now done using
       the macro CHKFSTR.

   -CSPICE Version 1.0.0, 25-OCT-1997 (NJB) (JEM) (WLT)

      Based on SPICELIB Version 2.2.0, 11-APR-1997 (WLT)

-Index_Entries

   geometric state of one body relative to another

-&
*/

{ /* Begin spkgeo_c */


   /*
   Participate in error tracing.
   */
   chkin_c ( "spkgeo_c" );


   /*
   Check the input string 'ref' to make sure the pointer is non-null
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "spkgeo_c", ref );


   /*
   Call the f2c'd Fortran routine.  Use explicit type casts for every
   type defined by f2c.
   */
   spkgeo_ (  ( integer    * )  &targ,
              ( doublereal * )  &et,
              ( char       * )  ref,
              ( integer    * )  &obs,
              ( doublereal * )  state,
              ( doublereal * )  lt,
              ( ftnlen       )  strlen(ref)   );


   chkout_c ( "spkgeo_c" );

} /* End spkgeo_c */
