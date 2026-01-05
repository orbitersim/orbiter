/*

-Procedure spkpos_c ( S/P Kernel, position )

-Abstract

   Return the position of a target body relative to an observing
   body, optionally corrected for light time (planetary aberration)
   and stellar aberration.

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

   ABCORR
   SPK
   NAIF_IDS
   FRAMES
   TIME

-Keywords

   EPHEMERIS

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"


   void spkpos_c ( ConstSpiceChar   * targ,
                   SpiceDouble        et,
                   ConstSpiceChar   * ref,
                   ConstSpiceChar   * abcorr,
                   ConstSpiceChar   * obs,
                   SpiceDouble        ptarg[3],
                   SpiceDouble      * lt        )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   targ       I   Target body name.
   et         I   Observer epoch.
   ref        I   Reference frame of output position vector.
   abcorr     I   Aberration correction flag.
   obs        I   Observing body name.
   ptarg      O   Position of target.
   lt         O   One way light time between observer and target.

-Detailed_Input

   targ        is the name of a target body. Optionally, you may
               supply the integer ID code for the object as an
               integer string. For example both "MOON" and "301"
               are legitimate strings that indicate the moon is the
               target body.

               The target and observer define a position vector
               which points from the observer to the target.

   et          is the ephemeris time, expressed as seconds past
               J2000 TDB, at which the position of the target body
               relative to the observer is to be computed. `et'
               refers to time at the observer's location.

   ref         is the name of the reference frame relative to which
               the output position vector should be expressed. This
               may be any frame supported by the SPICE system,
               including built-in frames (documented in the Frames
               Required Reading) and frames defined by a loaded
               frame kernel (FK).

               When `ref' designates a non-inertial frame, the
               orientation of the frame is evaluated at an epoch
               dependent on the selected aberration correction. See
               the description of the output position vector `ptarg'
               for details.

   abcorr      indicates the aberration corrections to be applied to
               the position of the target body to account for
               one-way light time and stellar aberration. See the
               discussion in the -Particulars section for
               recommendations on how to choose aberration
               corrections.

               `abcorr' may be any of the following:

                  "NONE"     Apply no correction. Return the
                             geometric position of the target body
                             relative to the observer.

               The following values of `abcorr' apply to the
               "reception" case in which photons depart from the
               target's location at the light-time corrected epoch
               et-lt and *arrive* at the observer's location at `et':

                  "LT"       Correct for one-way light time (also
                             called "planetary aberration") using a
                             Newtonian formulation. This correction
                             yields the position of the target at
                             the moment it emitted photons arriving
                             at the observer at `et'.

                             The light time correction uses an
                             iterative solution of the light time
                             equation (see -Particulars for details).
                             The solution invoked by the "LT" option
                             uses one iteration.

                  "LT+S"     Correct for one-way light time and
                             stellar aberration using a Newtonian
                             formulation. This option modifies the
                             position obtained with the "LT" option
                             to account for the observer's velocity
                             relative to the solar system
                             barycenter. The result is the apparent
                             position of the target---the position
                             as seen by the observer.

                  "CN"       Converged Newtonian light time
                             correction. In solving the light time
                             equation, the "CN" correction iterates
                             until the solution converges (three
                             iterations on all supported platforms).
                             Whether the "CN+S" solution is
                             substantially more accurate than the
                             "LT" solution depends on the geometry
                             of the participating objects and on the
                             accuracy of the input data. In all
                             cases this routine will execute more
                             slowly when a converged solution is
                             computed. See the -Particulars section
                             below for a discussion of precision of
                             light time corrections.

                  "CN+S"     Converged Newtonian light time
                             correction and stellar aberration
                             correction.


               The following values of `abcorr' apply to the
               "transmission" case in which photons *depart* from
               the observer's location at `et' and arrive at the
               target's location at the light-time corrected epoch
               et+lt:

                  "XLT"      "Transmission" case: correct for
                             one-way light time using a Newtonian
                             formulation. This correction yields the
                             position of the target at the moment it
                             receives photons emitted from the
                             observer's location at `et'.

                  "XLT+S"    "Transmission" case: correct for one-way
                             light time and stellar aberration using a
                             Newtonian formulation. This option
                             modifies the position obtained with the
                             "XLT" option to account for the observer's
                             velocity relative to the solar system
                             barycenter. The computed target position
                             indicates the direction that photons
                             emitted from the observer's location must
                             be "aimed" to hit the target.

                  "XCN"      "Transmission" case: converged
                             Newtonian light time correction.

                  "XCN+S"    "Transmission" case: converged Newtonian
                             light time correction and stellar
                             aberration correction.


               Neither special nor general relativistic effects are
               accounted for in the aberration corrections applied
               by this routine.

               Case and blanks are not significant in the string
               `abcorr'.

   obs         is the name of an observing body. Optionally, you may
               supply the ID code of the object as an integer string.
               For example, both "EARTH" and "399" are legitimate
               strings to supply to indicate the observer is
               Earth.

-Detailed_Output

   ptarg       is a Cartesian 3-vector representing the position of
               the target body relative to the specified observer.
               `ptarg' is corrected for the specified aberrations, and
               is expressed with respect to the reference frame
               specified by `ref'. The three components of `ptarg'
               represent the x-, y- and z-components of the target's
               position.

               `ptarg' points from the observer's location at `et' to
               the aberration-corrected location of the target.
               Note that the sense of this position vector is
               independent of the direction of radiation travel
               implied by the aberration correction.

               Units are always km.

               Non-inertial frames are treated as follows: letting
               `ltcent' be the one-way light time between the observer
               and the central body associated with the frame, the
               orientation of the frame is evaluated at et-ltcent,
               et+ltcent, or `et' depending on whether the requested
               aberration correction is, respectively, for received
               radiation, transmitted radiation, or is omitted.
               `ltcent' is computed using the method indicated by
               `abcorr'.

   lt          is the one-way light time between the observer and
               target in seconds. If the target position is
               corrected for aberrations, then `lt' is the one-way
               light time between the observer and the light time
               corrected target location.

-Parameters

   None.

-Exceptions

   1)  If name of target or observer cannot be translated to its NAIF
       ID code, the error SPICE(IDCODENOTFOUND) is signaled by a
       routine in the call tree of this routine.

   2)  If the reference frame `ref' is not a recognized reference
       frame, the error SPICE(UNKNOWNFRAME) is signaled by a routine
       in the call tree of this routine.

   3)  If the loaded kernels provide insufficient data to compute the
       requested position vector, an error is signaled by a routine
       in the call tree of this routine.

   4)  If an error occurs while reading an SPK or other kernel file,
       the error  is signaled by a routine in the call tree
       of this routine.

   5)  If any of the `targ', `ref', `abcorr' or `obs' input string
       pointers is null, the error SPICE(NULLPOINTER) is signaled.

   6)  If any of the `targ', `ref', `abcorr' or `obs' input strings
       has zero length, the error SPICE(EMPTYSTRING) is signaled.

-Files

   This routine computes positions using SPK files that have been
   loaded into the SPICE system, normally via the kernel loading
   interface routine furnsh_c. See the routine furnsh_c and the SPK
   and KERNEL Required Reading for further information on loading
   (and unloading) kernels.

   If the output position `ptarg' is to be expressed relative to a
   non-inertial frame, or if any of the ephemeris data used to
   compute `ptarg' are expressed relative to a non-inertial frame in
   the SPK files providing those data, additional kernels may be
   needed to enable the reference frame transformations required to
   compute the position. These additional kernels may be C-kernels,
   PCK files or frame kernels. Any such kernels must already be
   loaded at the time this routine is called.

-Particulars

   This routine is part of the user interface to the SPICE ephemeris
   system. It allows you to retrieve position information for any
   ephemeris object relative to any other in a reference frame that
   is convenient for further computations.

   This routine is identical in function to the routine spkezp_c
   except that it allows you to refer to ephemeris objects by name
   (via a character string).

   Please refer to the Aberration Corrections Required Reading
   abcorr.req for detailed information describing the nature and
   calculation of the applied corrections.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Load a planetary ephemeris SPK, then look up a series of
      geometric positions of the moon relative to the earth,
      referenced to the J2000 frame.

      Use the SPK kernel below to load the required Earth and
      Moon ephemeris data.

         de421.bsp


      Example code begins here.


      /.
         Program spkpos_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main()
      {

         #define        ABCORR        "NONE"
         #define        FRAME         "J2000"
         #define        SPK           "de421.bsp"

         /.
         ET0 represents the date 2000 Jan 1 12:00:00 TDB.
         ./
         #define        ET0           0.0

         /.
         Use a time step of 1 hour; look up 4 positions.
         ./
         #define        STEP          3600.0
         #define        MAXITR        4

         #define        OBSERVER      "earth"
         #define        TARGET        "moon"


         /.
         Local variables
         ./
         SpiceInt       i;

         SpiceDouble    et;
         SpiceDouble    lt;
         SpiceDouble    pos [3];


         /.
         Load the spk file.
         ./
         furnsh_c ( SPK );

         /.
         Step through a series of epochs, looking up a position vector
         at each one.
         ./
         for ( i = 0;  i < MAXITR;  i++ )
         {
            et  =  ET0 + i*STEP;

            spkpos_c ( TARGET,    et,   FRAME,  ABCORR,
                       OBSERVER,  pos,  &lt             );

            printf( "\net = %20.10f\n\n",                 et     );
            printf( "J2000 x-position (km):   %20.10f\n", pos[0] );
            printf( "J2000 y-position (km):   %20.10f\n", pos[1] );
            printf( "J2000 z-position (km):   %20.10f\n", pos[2] );
         }

         return ( 0 );

      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      et =         0.0000000000

      J2000 x-position (km):     -291608.3853096409
      J2000 y-position (km):     -266716.8329467875
      J2000 z-position (km):      -76102.4871467836

      et =      3600.0000000000

      J2000 x-position (km):     -289279.8983133120
      J2000 y-position (km):     -269104.1084289378
      J2000 z-position (km):      -77184.2420729120

      et =      7200.0000000000

      J2000 x-position (km):     -286928.0014055001
      J2000 y-position (km):     -271469.9902460162
      J2000 z-position (km):      -78259.9083077002

      et =     10800.0000000000

      J2000 x-position (km):     -284552.9026554719
      J2000 y-position (km):     -273814.3097527430
      J2000 z-position (km):      -79329.4060465982


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   C.H. Acton          (JPL)
   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   W.L. Taber          (JPL)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 3.0.2, 01-NOV-2021 (JDR)

       Edited the header to comply with NAIF standard. Added
       reference to the required SPK for the example code. Reduced
       the number of positions to be computed.

       Moved SPK required reading from -Literature_References to
       -Required_Reading section. Added entries #5 and #6 to -Exceptions
       section.

       Updated -Particulars to refer to Aberration Corrections
       Required Reading document, which was added to
       -Required_Reading list.

   -CSPICE Version 3.0.1, 07-JUL-2014 (NJB)

       Discussion of light time corrections was updated. Assertions
       that converged light time corrections are unlikely to be
       useful were removed.

   -CSPICE Version 2.0.4, 04-APR-2008 (NJB)

       Corrected minor error in description of XLT+S aberration
       correction.

   -CSPICE Version 2.0.3, 17-APR-2005 (NJB)

       Error was corrected in example program: variable name `state'
       was changed to `pos' in printf calls.

   -CSPICE Version 2.0.2, 13-OCT-2003 (EDW)

       Various minor header changes were made to improve clarity.
       Added mention that 'lt' returns a value in seconds.

   -CSPICE Version 2.0.1, 27-JUL-2003 (NJB) (CHA)

       Various header corrections were made.

   -CSPICE Version 2.0.0, 31-DEC-2001 (NJB)

       Updated to handle aberration corrections for transmission
       of radiation. Formerly, only the reception case was
       supported. The header was revised and expanded to explain
       the functionality of this routine in more detail.

   -CSPICE Version 1.0.0, 29-MAY-1999 (NJB) (WLT)

-Index_Entries

   using names get target position relative to an observer
   position relative to observer corrected for aberrations
   read ephemeris data
   read trajectory data

-&
*/

{ /* Begin spkpos_c */

   /*
   Participate in error tracing.
   */

   chkin_c ( "spkpos_c" );


   /*
   Check the input strings to make sure the pointers are non-null
   and the string lengths are non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "spkpos_c", targ   );
   CHKFSTR ( CHK_STANDARD, "spkpos_c", ref    );
   CHKFSTR ( CHK_STANDARD, "spkpos_c", abcorr );
   CHKFSTR ( CHK_STANDARD, "spkpos_c", obs    );


   /*
   Call the f2c'd Fortran routine.  Use explicit type casts for every
   type defined by f2c.
   */
   spkpos_ ( ( char       * )  targ,
             ( doublereal * )  &et,
             ( char       * )  ref,
             ( char       * )  abcorr,
             ( char       * )  obs,
             ( doublereal * )  ptarg,
             ( doublereal * )  lt,
             ( ftnlen       )  strlen(targ),
             ( ftnlen       )  strlen(ref),
             ( ftnlen       )  strlen(abcorr),
             ( ftnlen       )  strlen(obs)    );


   chkout_c ( "spkpos_c" );

} /* End spkpos_c */
