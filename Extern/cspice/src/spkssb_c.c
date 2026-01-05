/*

-Procedure spkssb_c ( S/P Kernel, solar system barycenter )

-Abstract

   Return the state (position and velocity) of a target body
   relative to the solar system barycenter.

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
   #include "SpiceZmc.h"


   void spkssb_c ( SpiceInt           targ,
                   SpiceDouble        et,
                   ConstSpiceChar   * ref,
                   SpiceDouble        starg[6] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   targ       I   Target body.
   et         I   Target epoch.
   ref        I   Target reference frame.
   starg      O   State of target.

-Detailed_Input

   targ        is the standard NAIF ID code for a target body.

   et          is the epoch (ephemeris time) at which the state of the
               target body is to be computed.

   ref         is the name of the reference frame relative to which the
               output state vector should be expressed. This may be any
               frame supported by the CSPICE frame system, including
               dynamic and other non-inertial frames.

-Detailed_Output

   starg       is a Cartesian state vector representing the position and
               velocity of the target body, relative to the solar system
               barycenter, at epoch `et'. This vector is rotated into the
               specified reference frame. Units are always km and
               km/sec.

-Parameters

   None.

-Exceptions

   1)  If sufficient information has not been "loaded" via the
       routine furnsh_c, spklef_c or the PCK kernel loaders, an error is
       signaled by a routine in the call tree of this routine.

   2)  If the `ref' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   3)  If the `ref' input string has zero length, the error
       SPICE(EMPTYSTRING) is signaled.

-Files

   See -Restrictions.

-Particulars

   In order to compute the state of one body relative to another,
   the states of the two bodies must be known relative to a third
   body. One simple solution is to use the solar system barycenter
   as the third body.

   Ephemeris data from more than one segment may be required
   to determine the state of a body relative to the barycenter.
   spkssb_c reads as many segments as necessary, from as many
   files as necessary, using files that have been loaded by
   previous calls to furnsh_c or spklef_c (load ephemeris file).

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) In the following example, spkssb_c is used to display
      the distance from Earth (Body 399) to Mars (body 499) at
      a given epoch.

      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File name: spkssb_ex1.tm

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
            de418.bsp                     Planetary ephemeris
            naif0009.tls                  Leapseconds

         \begindata

            KERNELS_TO_LOAD = ( 'de418.bsp',
                                'naif0009.tls'  )

         \begintext

         End of meta-kernel


      Example code begins here.


      /.
         Program spkssb_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main ()
      {

         /.
         Define parameters for a state lookup:

         Return the state vector of Mars (499)
         and Earth (399) with respect to the Solar System
         Barycenter in the J2000 frame at epoch
         July 4, 2003 11:00 AM PST.
         ./
         #define EARTH   399
         #define EPOCH   "July 4, 2003 11:00 AM PST"
         #define FRAME   "J2000"
         #define MARS    499

         /.
         Local variables.
         ./
         SpiceDouble             dist;
         SpiceDouble             et;
         SpiceDouble             searth[6];
         SpiceDouble             smars [6];


         /.
         Load the required kernels.
         ./
         furnsh_c ( "spkssb_ex1.tm" );

         /.
         Convert the epoch to ephemeris time.
         ./
         str2et_c ( EPOCH, &et );

         /.
         Look-up the states for the defined parameters.
         ./
         spkssb_c ( EARTH, et, FRAME, searth );
         spkssb_c ( MARS,  et, FRAME, smars  );

         /.
         What measure of distance separates the two bodies
         at epoch.
         ./
         dist = vdist_c( searth, smars );

         printf ( "The absolute distance (km)     : %23.10f\n", dist  );
         printf ( "between Mars and Earth at epoch: %s\n",      EPOCH );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      The absolute distance (km)     :     80854819.7017317861
      between Mars and Earth at epoch: July 4, 2003 11:00 AM PST


      Note that the two spkssb_c calls could be replaced by

         spkgeo_c ( EARTH, et, frame, MARS, state, lt );

      or

         spkezr_c ( "EARTH", et, frame, "NONE", "MARS", state, lt );

      using the norm of the position components of the `state'
      vector to compute the distance between the bodies.

-Restrictions

   1)  The ephemeris files to be used by spkssb_c must be loaded
       by furnsh_c or spklef_c before spkssb_c is called.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   W.L. Taber          (JPL)
   I.M. Underwood      (JPL)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.0.3, 05-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard.

       Added example's meta-kernel and problem statement. Created complete code
       example from existing code fragments.

       Moved SPK required reading from -Literature_References to
       -Required_Reading section. Added references to furnsh_c as valid
       mechanism to load the SPK files used by this routine.

   -CSPICE Version 1.0.2, 20-NOV-2004 (NJB)

       Updated description of input argument `ref' to indicate all
       frames supported by CSPICE are allowed.

   -CSPICE Version 1.0.1, 14-OCT-2003 (EDW)

       Various minor corrections to the header.

   -CSPICE Version 1.0.0, 23-JUN-1999 (NJB) (WLT) (IMU)

-Index_Entries

   state relative to solar system barycenter

-&
*/

{ /* Begin spkssb_c */



   /*
   Participate in error tracing.
   */
   chkin_c ( "spkssb_c" );


   /*
   Check the input string 'ref' to make sure the pointer
   is non-null and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "spkssb_c", ref );


   spkssb_ (  ( integer    * ) &targ,
              ( doublereal * ) &et,
              ( char       * ) ref,
              ( doublereal * ) starg,
              ( ftnlen       ) strlen(ref)  );


   chkout_c ( "spkssb_c" );

} /* End spkssb_c */
