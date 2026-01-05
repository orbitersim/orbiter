/*

-Procedure spkw15_c ( SPK, write a type 15 segment )

-Abstract

   Write an SPK segment of type 15 given a type 15 data record.

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
   #undef    spkw15_c


   void spkw15_c ( SpiceInt           handle,
                   SpiceInt           body,
                   SpiceInt           center,
                   ConstSpiceChar   * frame,
                   SpiceDouble        first,
                   SpiceDouble        last,
                   ConstSpiceChar   * segid,
                   SpiceDouble        epoch,
                   ConstSpiceDouble   tp     [3],
                   ConstSpiceDouble   pa     [3],
                   SpiceDouble        p,
                   SpiceDouble        ecc,
                   SpiceDouble        j2flg,
                   ConstSpiceDouble   pv     [3],
                   SpiceDouble        gm,
                   SpiceDouble        j2,
                   SpiceDouble        radius     )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   handle     I   Handle of an SPK file open for writing.
   body       I   Body code for ephemeris object.
   center     I   Body code for the center of motion of the body.
   frame      I   The reference frame of the states.
   first      I   First valid time for which states can be computed.
   last       I   Last valid time for which states can be computed.
   segid      I   Segment identifier.
   epoch      I   Epoch of the periapse.
   tp         I   Trajectory pole vector.
   pa         I   Periapsis vector.
   p          I   Semi-latus rectum.
   ecc        I   Eccentricity.
   j2flg      I   J2 processing flag.
   pv         I   Central body pole vector.
   gm         I   Central body GM.
   j2         I   Central body J2.
   radius     I   Equatorial radius of central body.

-Detailed_Input

   All units are radians, km, seconds.

   handle      is the file handle of an SPK file that has been
               opened for writing.

   body        is the NAIF ID for the body whose states are
               to be recorded in an SPK file.

   center      is the NAIF ID for the center of motion associated
               with body.

   frame       is the reference frame that states are referenced to,
               for example "J2000".

   first       are the bounds on the ephemeris times, expressed as
   last        seconds past J2000.

   segid       is the segment identifier. An SPK segment identifier
               may contain up to 40 characters.

   epoch       is the epoch of the orbit elements at periapse
               in ephemeris seconds past J2000.

   tp          is a vector parallel to the angular momentum vector of
               the orbit at epoch expressed relative to `frame'. A unit
               vector parallel to `tp' will be stored in the output
               segment.

   pa          is a vector parallel to the position vector of the
               trajectory at periapsis of epoch expressed relative to
               `frame'. A unit vector parallel to `pa' will be stored
               in the output segment.

   p           is the semi-latus rectum---p in the equation:

                  r = p/(1 + ecc*cos(Nu))

   ecc         is the eccentricity.

   j2flg       is the J2 processing flag describing what J2
               corrections are to be applied when the orbit is
               propagated.

               All J2 corrections are applied if the value of j2flg
               is not 1, 2 or 3.

               If the value of the flag is 3 no corrections are
               done.

               If the value of the flag is 1 no corrections are
               computed for the precession of the line of apsides.
               However, regression of the line of nodes is
               performed.

               If the value of the flag is 2 no corrections are
               done for the regression of the line of nodes.
               However, precession of the line of apsides is
               performed.

               Note that J2 effects are computed only if the orbit
               is elliptic and does not intersect the central body.

   pv          is a vector parallel to the north pole vector of the
               central body expressed relative to `frame'. A unit vector
               parallel to `pv' will be stored in the output segment.

   gm          is the central body gm.

   j2          is the central body J2 (dimensionless).

   radius      is the equatorial radius of the central body.

-Detailed_Output

   None. A type 15 segment is written to the file attached
   to handle.

-Parameters

   None.

-Exceptions

   1)  If the eccentricity is less than zero, the error
       SPICE(BADECCENTRICITY) is signaled by a routine in the call
       tree of this routine.

   2)  If the semi-latus rectum is 0, the error SPICE(BADLATUSRECTUM)
       is signaled by a routine in the call tree of this routine.

   3)  If the pole vector, trajectory pole vector or periapsis vector
       have zero length, the error SPICE(BADVECTOR) is signaled by a
       routine in the call tree of this routine.

   4)  If the trajectory pole vector and the periapsis vector are not
       orthogonal, the error SPICE(BADINITSTATE) is signaled by a
       routine in the call tree of this routine. The test for
       orthogonality is very crude. The routine simply checks that
       the dot product of the unit vectors parallel to the trajectory
       pole and periapse vectors is less than 0.00001. This check is
       intended to catch blunders, not to enforce orthogonality to
       double precision capacity.

   5)  If the mass of the central body is non-positive, the error
       SPICE(NONPOSITIVEMASS) is signaled by a routine in the call
       tree of this routine.

   6)  If the radius of the central body is negative, the error
       SPICE(BADRADIUS) is signaled by a routine in the call tree of
       this routine.

   7)  If the segment identifier has more than 40 non-blank
       characters, the error SPICE(SEGIDTOOLONG) is signaled by a
       routine in the call tree of this routine.

   8)  If the segment identifier contains non-printing characters,
       the error SPICE(NONPRINTABLECHARS) is signaled by a routine in
       the call tree of this routine.

   9)  If there are inconsistencies in the `body', `center', `frame' or
       `first' and `last' times, an error is signaled by
       a routine in the call tree of this routine.

   10) If any of the `frame' or `segid' input string pointers is
       null, the error SPICE(NULLPOINTER) is signaled.

   11) If any of the `frame' or `segid' input strings has zero
       length, the error SPICE(EMPTYSTRING) is signaled.

-Files

   A new type 15 SPK segment is written to the SPK file attached
   to handle.

-Particulars

   This routine writes an SPK type 15 data segment to the open SPK file
   according to the format described in the type 15 section of the SPK
   Required Reading. The SPK file must have been opened with write
   access.

   This routine is provided to provide direct support for the MASL
   precessing orbit formulation.

-Examples

   Suppose that at time epoch you have the J2000 periapsis
   state of some object relative to some central body and would
   like to create a type 15 SPK segment to model the motion of
   the object using simple regression and precession of the
   line of nodes and apsides. The following code fragment
   illustrates how you can prepare such a segment. We shall
   assume that you have in hand the J2000 direction of the
   central body's pole vector, its GM, J2 and equatorial
   radius. In addition we assume that you have opened an SPK
   file for write access and that it is attached to handle.

   (If your state is at an epoch other than periapse the
   fragment below will NOT produce a "correct" type 15 segment
   for modeling the motion of your object.)

      #include "SpiceUsr.h"
          .
          .
          .

      /.
      First we get the osculating elements.
      /
      oscelt_c ( state, epoch, gm, elts );


      /.
      From these collect the eccentricity and semi-latus rectum.
      ./
      ecc = elts [ 1 ];
      p   = elts [ 0 ] * ( 1.0 + ecc );


      /.
      Next get the trajectory pole vector and the
      periapsis vector.
      ./
      ucrss_c ( state, state+4, tp );
      vhat_c  ( state,          pa );


      /.
      Enable both J2 corrections.
      ./
      j2flg = 0.0;


      /.
      Now add the segment.
      ./
      spkw15_c ( handle,  body,   center,  frame,  first,  last,
                 segid,   epoch,  tp,      pa,     p,      ecc,
                 j2flg,   pv,     gm,      j2,     radius       );

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   W.L. Taber          (JPL)

-Version

   -CSPICE Version 1.0.2, 05-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard.

   -CSPICE Version 1.0.1, 29-MAY-2012 (NJB)

       Header was updated to relect changes in handling
       of inputs `tp', `pa', and `pv' by the f2c'd routine
       spkw15_. That routine now writes unit length
       versions of these vectors to the output segment.

       A header comment typo was corrected.

   -CSPICE Version 1.0.0, 21-JUN-1999 (WLT)

-Index_Entries

   Write a type 15 SPK segment

-&
*/

{ /* Begin spkw15_c */

   /*
   Local constants
   */


   /*
   Local macros
   */


   /*
   Local variables
   */


   /*
   Static variables
   */


   /*
   Participate in error tracing.
   */

   chkin_c ( "spkw15_c" );

   /*
   Check the input strings to make sure the pointers
   are non-null and the string lengths are non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "spkw15_c", frame );
   CHKFSTR ( CHK_STANDARD, "spkw15_c", segid );


   /*
   Write the segment.
   */
   spkw15_ ( ( integer    * ) &handle,
             ( integer    * ) &body,
             ( integer    * ) &center,
             ( char       * ) frame,
             ( doublereal * ) &first,
             ( doublereal * ) &last,
             ( char       * ) segid,
             ( doublereal * ) &epoch,
             ( doublereal * ) tp,
             ( doublereal * ) pa,
             ( doublereal * ) &p,
             ( doublereal * ) &ecc,
             ( doublereal * ) &j2flg,
             ( doublereal * ) pv,
             ( doublereal * ) &gm,
             ( doublereal * ) &j2,
             ( doublereal * ) &radius,
             ( ftnlen       ) strlen(frame),
             ( ftnlen       ) strlen(segid)  );


   chkout_c ( "spkw15_c" );

} /* End spkw15_c */
