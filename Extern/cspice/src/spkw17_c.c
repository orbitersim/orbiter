/*

-Procedure spkw17_c ( SPK, write a type 17 segment )

-Abstract

   Write an SPK segment of type 17 given a type 17 data record.

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
   #undef    spkw17_c


   void spkw17_c ( SpiceInt           handle,
                   SpiceInt           body,
                   SpiceInt           center,
                   ConstSpiceChar   * frame,
                   SpiceDouble        first,
                   SpiceDouble        last,
                   ConstSpiceChar   * segid,
                   SpiceDouble        epoch,
                   ConstSpiceDouble   eqel   [9],
                   SpiceDouble        rapol,
                   SpiceDouble        decpol      )

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
   epoch      I   Epoch of elements in seconds past J2000.
   eqel       I   Array of equinoctial elements.
   rapol      I   Right Ascension of the reference plane's pole.
   decpol     I   Declination of the reference plane's pole.

-Detailed_Input

   handle      is the file handle of an SPK file that has been
               opened for writing.

   body        is the NAIF ID for the body whose states are
               to be recorded in the SPK file.

   center      is the NAIF ID for the center of motion associated
               with `body'.

   frame       is the reference frame that states are referenced to,
               for example "J2000".

   first,
   last        are the bounds on the ephemeris times, expressed as
               seconds past J2000.

   segid       is the segment identifier. An SPK segment identifier
               may contain up to 40 characters.

   epoch       is the epoch of equinoctial elements in seconds
               past the J2000 epoch.

   eqel        is an array of 9 double precision numbers that
               are the equinoctial elements for some orbit relative
               to the equatorial frame of a central body.

                  Note: The Z-axis of the equatorial frame is the
                  direction of the pole of the central body relative
                  to `frame'. The X-axis is given by the cross product of
                  the Z-axis of `frame' with the direction of the pole of
                  the central body. The Y-axis completes a right handed
                  frame.

               The specific arrangement of the elements is spelled
               out below. The following terms are used in the
               discussion of elements of `eqel':

                  inc  --- inclination of the orbit
                  argp --- argument of periapse
                  node --- longitude of the ascending node
                  e    --- eccentricity of the orbit
                  m0   --- mean anomaly

               eqel[0]   is the semi-major axis (A) of the orbit in km.

               eqel[1]   is the value of H at the specified epoch:

                            H =  e * sin( argp + node )

               eqel[2]   is the value of K at the specified epoch:

                            K =  e * cos( argp + node )

               eqel[3]   is the mean longitude at the epoch of the
                         elements measured in radians:

                            ( m0 + argp + node )

               eqel[4]   is the value of P at the specified epoch:

                            P =  tan( inc/1 ) * sin( node )

               eqel[5]   is the value of Q at the specified epoch:

                            Q =  tan( inc/1 ) * cos( node );

               eqel[6]   is the rate of the longitude of periapse
                         at the epoch of the elements.

                            ( dargp/dt + dnode/dt )

                         This rate is assumed to hold for all time. The
                         rate is measured in radians per second.

               eqel[7]   is the derivative of the mean longitude:

                            ( dm0/dt + dargp/dt + dnode/dt )

                         This rate is assumed to be constant and is
                         measured in radians/second.

               eqel[8]   is the rate of the longitude of the ascending
                         node:

                            ( dnode/dt )

                         This rate is measured in radians per second.

   rapol       is the Right Ascension of the pole of the reference
               plane relative to `frame' measured in radians.

   decpol      is the declination of the pole of the reference plane
               relative to `frame' measured in radians.

-Detailed_Output

   None.

   The routine writes an SPK type 17 segment to the file attached to
   `handle'.

-Parameters

   None.

-Exceptions

   1)  If the semi-major axis is less than or equal to zero, the
       error SPICE(BADSEMIAXIS) is signaled by a routine in the call
       tree of this routine.

   2)  If the eccentricity of the orbit corresponding to the values
       of H and K ( eqel[1] and eqel[2] ) is greater than 0.9, the
       error SPICE(ECCOUTOFRANGE) is signaled by a routine in the
       call tree of this routine.

   3)  If the segment identifier has more than 40 non-blank
       characters, the error SPICE(SEGIDTOOLONG) is signaled by a
       routine in the call tree of this routine.

   4)  If the segment identifier contains non-printing characters,
       the error SPICE(NONPRINTABLECHARS) is signaled by a routine in
       the call tree of this routine.

   5)  If there are inconsistencies in the `body', `center', `frame' or
       `first' and `last' times, an error is signaled by a routine in
       the call tree of this routine.

   6)  If any of the `frame' or `segid' input string pointers is
       null, the error SPICE(NULLPOINTER) is signaled.

   7)  If any of the `frame' or `segid' input strings has zero
       length, the error SPICE(EMPTYSTRING) is signaled.

-Files

   A new type 17 SPK segment is written to the SPK file attached
   to `handle'.

-Particulars

   This routine writes an SPK type 17 data segment to the open SPK
   file according to the format described in the type 17 section of
   the SPK Required Reading. The SPK file must have been opened with
   write access.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Suppose that at a given time you have the classical elements
      of Daphnis relative to the equatorial frame of Saturn. These
      can be converted to equinoctial elements and stored in an SPK
      file as a type 17 segment so that Daphnis can be used within
      the SPK subsystem of the SPICE system.

      The example code shown below creates an SPK type 17 kernel
      with a single segment using such data.


      Example code begins here.


      /.
         Program spkw17_ex1
      ./
      #include <math.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local parameters.
         ./
         #define SPK17        "spkw17_ex1.bsp"

         /.
         The SPK type 17 segment will contain data for Daphnis
         (ID 635) with respect to Saturn (ID 699) in the J2000
         reference frame.
         ./
         #define BODY         635
         #define CENTER       699
         #define FRMNAM       "J2000"

         /.
         This is the list of parameters used to represent the
         classical elements:

            Variable     Meaning
            --------     ---------------------------------------
            A            Semi-major axis in km.
            ECC          Eccentricity of orbit.
            INC          Inclination of orbit.
            NODE         Longitude of the ascending node at
                         epoch.
            OMEGA        Argument of periapse at epoch.
            M            Mean anomaly at epoch.
            DMDT         Mean anomaly rate in radians/second.
            DNODE        Rate of change of longitude of
                         ascending node in radians/second.
            DOMEGA       Rate of change of argument of periapse
                         in radians/second.
            EPOCH        The epoch of the elements in seconds
                         past the J2000 epoch.
         ./
         #define A            1.36505608e+05
         #define ECC          -2.105898062e-05
         #define INC          -3.489710429e-05
         #define NODE         -3.349237456e-02
         #define OMEGA        1.52080206722
         #define M            1.21177109734
         #define DMDT         1.218114014e-04
         #define DNODE        -5.96845468e-07
         #define DOMEGA       1.196601093e-06
         #define EPOCH        0.0

         /.
         In addition, the spkw17_c routine requires the Right
         Ascension and Declination of the pole of the
         reference plane relative to the J2000 frame, in radians.
         ./
         #define RAPOL        7.08332284e-01
         #define DECPOL       1.45800286

         /.
         Local variables.
         ./
         SpiceChar          * ifname;
         SpiceChar          * segid;

         SpiceDouble          eqel   [9];
         SpiceDouble          first;
         SpiceDouble          last;

         SpiceInt             handle;
         SpiceInt             ncomch;

         /.
         Set the start and end times of interval covered by
         segment.
         ./
         first  =  504878400.0;
         last   = 1578657600.0;

         /.
         `ncomch' is the number of characters to reserve for the
         kernel's comment area. This example doesn't write
         comments, so set to zero.
         ./
         ncomch = 0;

         /.
         Internal file name and segment ID.
         ./
         ifname = "Test for type 17 SPK internal file name";
         segid  = "SPK type 17 test segment";

         /.
         Open a new SPK file.
         ./
         spkopn_c ( SPK17, ifname, ncomch, &handle );

         /.
         Convert the classical elements to equinoctial elements
         (in the order compatible with type 17).
         ./
         eqel[0] = A;
         eqel[1] = ECC * sin ( OMEGA + NODE );
         eqel[2] = ECC * cos ( OMEGA + NODE );

         eqel[3] = M + OMEGA + NODE;

         eqel[4] = tan( INC/2.0 ) * sin( NODE );
         eqel[5] = tan( INC/2.0 ) * cos( NODE );

         eqel[6] =        DOMEGA + DNODE;
         eqel[7] = DMDT + DOMEGA + DNODE;
         eqel[8] = DNODE;

         /.
         Now add the segment.
         ./
         spkw17_c ( handle, BODY,  CENTER, FRMNAM, first,  last,
                    segid,  EPOCH, eqel,   RAPOL,  DECPOL );

         /.
         Close the SPK file.
         ./
         spkcls_c ( handle );

         return ( 0 );
      }


      When this program is executed, no output is presented on
      screen. After run completion, a new SPK type 17 exists in
      the output directory.

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   W.L. Taber          (JPL)

-Version

   -CSPICE Version 1.0.1, 05-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard. Added complete
       code example based on existing example.

   -CSPICE Version 1.0.0, 21-JUN-1999 (NJB) (WLT)

-Index_Entries

   Write a type 17 SPK segment

-&
*/

{ /* Begin spkw17_c */



   /*
   Participate in error tracing.
   */
   chkin_c ( "spkw17_c" );


   /*
   Check the input strings to make sure the pointers
   are non-null and the string lengths are non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "spkw17_c", frame );
   CHKFSTR ( CHK_STANDARD, "spkw17_c", segid );


   /*
   Write the segment.
   */
   spkw17_ ( ( integer    * ) &handle,
             ( integer    * ) &body,
             ( integer    * ) &center,
             ( char       * ) frame,
             ( doublereal * ) &first,
             ( doublereal * ) &last,
             ( char       * ) segid,
             ( doublereal * ) &epoch,
             ( doublereal * ) eqel,
             ( doublereal * ) &rapol,
             ( doublereal * ) &decpol,
             ( ftnlen       ) strlen(frame),
             ( ftnlen       ) strlen(segid)  );


   chkout_c ( "spkw17_c" );

} /* End spkw17_c */
