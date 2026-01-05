/*

-Procedure lspcn_c  ( Longitude of the sun, planetocentric )

-Abstract

   Compute L_s, the planetocentric longitude of the sun, as seen
   from a specified body.

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

   NAIF_IDS
   PCK
   TIME
   SPK

-Keywords

   GEOMETRY
   TIME

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"

   SpiceDouble lspcn_c ( ConstSpiceChar   * body,
                         SpiceDouble        et,
                         ConstSpiceChar   * abcorr )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   body       I   Name of central body.
   et         I   Epoch in seconds past J2000 TDB.
   abcorr     I   Aberration correction.

   The function returns the value of L_s for the specified body
   at the specified time.

-Detailed_Input

   body        is the name of the central body, typically a planet.

   et          is the epoch at which the longitude of the sun (L_s) is
               to be computed. `et' is expressed as seconds past J2000
               TDB (Barycentric Dynamical Time).

   abcorr      indicates the aberration corrections to be applied
               when computing the longitude of the sun. `abcorr' may
               be any of the following.

                  "NONE"     Apply no correction.

                  "LT"       Correct the position of the sun,
                             relative to the central body, for
                             planetary (light time) aberration.

                  "LT+S"     Correct the position of the sun,
                             relative to the central body, for
                             planetary and stellar aberrations.

-Detailed_Output

   The function returns the planetocentric longitude of the Sun,
   often called "L_s," for the specified body at the specified time.
   This is the longitude of the body-Sun vector in a right-handed
   frame whose basis vectors are defined as follows:

   -  The positive Z direction is given by the instantaneous
      angular velocity vector of the orbit of the body about
      the Sun.

   -  The positive X direction is that of the cross product of the
      instantaneous north spin axis of the body with the positive
      Z direction.

   -  The positive Y direction is Z x X.

   Units are radians; the range is 0 to 2*pi. Longitudes are
   positive to the east.

-Parameters

   None.

-Exceptions

   1)  If the input body name cannot be translated to an ID code,
       and if the name is not a string representation of an integer
       (for example, "399"), the error SPICE(NOTRANSLATION) is
       signaled by a routine in the call tree of this routine.

   2)  If no SPK (ephemeris) file has been loaded prior to calling
       this routine, or if the SPK data has insufficient coverage, an
       error is signaled by a routine in the call tree of this
       routine.

   3)  If a PCK file containing rotational elements for the central
       body has not been loaded prior to calling this routine, an
       error is signaled by a routine in the call tree of this
       routine.

   4)  If the instantaneous angular velocity and spin axis of `body'
       are parallel, an error is signaled by a routine in the call
       tree of this routine.

   5)  If the `body' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled. The function returns the value
       0.0.

   6)  If the `body' input string has zero length, the error
       SPICE(EMPTYSTRING) is signaled. The function returns the value
       0.0.

-Files

   Appropriate SPICE kernels must be loaded by the calling program
   before this routine is called.

   The following data are required:

   -  An SPK file (or files) containing ephemeris data sufficient to
      compute the geometric state of the central body relative to
      the sun at `et' must be loaded before this routine is called. If
      light time correction is used, data must be available that
      enable computation of the state the sun relative to the solar
      system barycenter at the light-time corrected epoch. If
      stellar aberration correction is used, data must be available
      that enable computation of the state the central body relative
      to the solar system barycenter at `et'.

   -  A PCK file containing rotational elements for the central body
      must be loaded before this routine is called.

-Particulars

   The direction of the vernal equinox for the central body is
   determined from the instantaneous equatorial and orbital planes
   of the central body. This equinox definition is specified in
   reference [1]. The "instantaneous orbital plane" is interpreted
   in this routine as the plane normal to the cross product of the
   position and velocity of the central body relative to the sun.
   The geometric state of the central body relative to the sun is
   used for this normal vector computation. The "instantaneous
   equatorial plane" is normal to the central body's north pole
   at the requested epoch. The pole direction is determined from
   rotational elements loaded via a PCK file.

   The result returned by this routine will depend on the
   ephemeris data and rotational elements used. The result may
   differ from that given in any particular version of the
   Astronomical Almanac, due to differences in these input data,
   and due to differences in precision of the computations.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) A simple program that computes L_s for a body and time
      supplied interactively. The geometric state of the Sun is
      used.

      Example code begins here.


      /.
         Program lspcn_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main()
      {
         #define  ABCORR         "NONE"
         #define  FILSIZ         256
         #define  NAMLEN         37
         #define  TIMLEN         41
         #define  ABCORR         "NONE"

         SpiceChar               body   [ NAMLEN ];
         SpiceChar               lsk    [ FILSIZ ];
         SpiceChar               pck    [ FILSIZ ];
         SpiceChar               spk    [ FILSIZ ];
         SpiceChar               timstr [ TIMLEN ];

         SpiceDouble             et;
         SpiceDouble             lon;

         prompt_c ( "Enter name of leapseconds kernel > ", FILSIZ, lsk );
         prompt_c ( "Enter name of PCK file           > ", FILSIZ, pck );
         prompt_c ( "Enter name of SPK file           > ", FILSIZ, spk );

         furnsh_c ( spk );
         furnsh_c ( lsk );
         furnsh_c ( pck );

         printf ( "\n"
                  "Kernels have been loaded.\n"
                  "\n"                           );

         prompt_c ( "Enter name of central body       > ",
                     NAMLEN,
                     body                                   );
         prompt_c ( "Enter calendar, JD, or DOY time  > ",
                     TIMLEN,
                     timstr                                 );

         str2et_c ( timstr, &et );

         /.
         Convert longitude to degrees.
         ./
         lon = dpr_c() * lspcn_c ( body, et, ABCORR );

         printf ( "\n"
                  "Central body              = %s\n"
                  "Time                      = %s\n"
                  "Planetocentric L_s (deg.) = %f\n"
                  "\n",
                  body,
                  timstr,
                  lon                               );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, using the LSK file named naif0012.tls, the PCK file
      named pck00010.tpc, the SPK file named de421.bsp; the "EARTH"
      as central body and the time string "2018 Mar 8  17:59 UTC",
      the output was:


      Enter name of leapseconds kernel > naif0012.tls
      Enter name of PCK file           > pck00010.tpc
      Enter name of SPK file           > de421.bsp

      Kernels have been loaded.

      Enter name of central body       > EARTH
      Enter calendar, JD, or DOY time  > 2018 Mar 8  17:59 UTC

      Central body              = EARTH
      Time                      = 2018 Mar 8  17:59 UTC
      Planetocentric L_s (deg.) = 348.115940


-Restrictions

   None.

-Literature_References

   [1]  "The Astronomical Almanac for the Year 2005," page L9,
        United States Naval Observatory, U.S. Government Printing
        Office, Washington, D.C., 2004.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.0.1, 25-AUG-2021 (JDR)

       Edited the header to comply to comply with NAIF standard. Removed while
       loop from example code, and added solution.

   -CSPICE Version 1.0.0, 06-JAN-2005 (NJB)

-Index_Entries

   planetocentric longitude of Sun
   compute L_s
   compute Ls
   compute L_sub_s

-&
*/

{ /* Begin lspcn_c */

   /*
   Local variables
   */
   SpiceDouble             retval;


   /*
   Give the function an initial value:
   */
   retval = 0.0;

   /*
   Participate in error tracing.
   */
   if ( return_c()  )
   {
      return ( retval );
   }
   chkin_c ( "lspcn_c" );

   /*
   Check the input string body to make sure the pointer is non-null
   and the string length is non-zero.
   */
   CHKFSTR_VAL ( CHK_STANDARD, "lspcn_c", body, retval );

   /*
   Call the f2c'd Fortran routine.
   */
   retval = lspcn_ ( ( char       * ) body,
                     ( doublereal * ) &et,
                     ( char       * ) abcorr,
                     ( ftnlen       ) strlen(body),
                     ( ftnlen       ) strlen(abcorr)  );

   chkout_c ( "lspcn_c" );

   return ( retval );

} /* End lspcn_c */
