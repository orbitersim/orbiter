/*

-Procedure spkw09_c ( Write SPK segment, type 9 )

-Abstract

   Write a type 9 segment to an SPK file.

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
   SPC
   SPK
   TIME

-Keywords

   EPHEMERIS
   FILES

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"
   #undef    spkw09_c


   void spkw09_c ( SpiceInt             handle,
                   SpiceInt             body,
                   SpiceInt             center,
                   ConstSpiceChar     * frame,
                   SpiceDouble          first,
                   SpiceDouble          last,
                   ConstSpiceChar     * segid,
                   SpiceInt             degree,
                   SpiceInt             n,
                   ConstSpiceDouble     states[][6],
                   ConstSpiceDouble     epochs[]     )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   handle     I   Handle of an SPK file open for writing.
   body       I   NAIF code for an ephemeris object.
   center     I   NAIF code for center of motion of body.
   frame      I   Reference frame name.
   first      I   Start time of interval covered by segment.
   last       I   End time of interval covered by segment.
   segid      I   Segment identifier.
   degree     I   Degree of interpolating polynomials.
   n          I   Number of states.
   states     I   Array of states.
   epochs     I   Array of epochs corresponding to states.
   maxdeg     P   Maximum allowed degree of interpolating polynomial.

-Detailed_Input

   handle      is the file handle of an SPK file that has been
               opened for writing.

   body        is the NAIF integer code for an ephemeris object
               whose state relative to another body is described
               by the segment to be created.

   center      is the NAIF integer code for the center of motion
               of the object identified by body.

   frame       is the NAIF name for a reference frame
               relative to which the state information for body
               is specified.

   first,
   last        are, respectively, the start and stop times of
               the time interval over which the segment defines
               the state of body.

   segid       is the segment identifier. An SPK segment
               identifier may contain up to 40 characters.

   degree      is the degree of the Lagrange polynomials used to
               interpolate the states. All components of the
               state vectors are interpolated by polynomials of
               fixed degree.

   n           is the number of states in the input state vector
               array.

   states      contains a time-ordered array of geometric states
               (x, y, z, dx/dt, dy/dt, dz/dt, in kilometers and
               kilometers per second) of body relative to center,
               specified relative to frame.

   epochs      is an array of epochs corresponding to the members
               of the state array. The epochs are specified as
               seconds past J2000, TDB.

-Detailed_Output

   None.

   See -Particulars for a description of the effect of this routine.

-Parameters

   MAXDEG         is the maximum allowed degree of the interpolating
                  polynomial. If the value of MAXDEG is increased,
                  the CSPICE routine spkpvn_ must be changed
                  accordingly. In particular, the size of the
                  record passed to spkrNN_ and spkeNN_ must be
                  increased, and comments describing the record size
                  must be changed.

                  The current value of MAXDEG is 15.

-Exceptions

   If any of the following exceptions occur, this routine will return
   without creating a new segment.

   1)  If `frame' is not a recognized name, the error
       SPICE(INVALIDREFFRAME) is signaled by a routine in the call
       tree of this routine.

   2)  If the last non-blank character of `segid' occurs past index 40,
       the error SPICE(SEGIDTOOLONG) is signaled by a routine in the
       call tree of this routine.

   3)  If `segid' contains any nonprintable characters, the error
       SPICE(NONPRINTABLECHARS) is signaled by a routine in the call
       tree of this routine.

   4)  If `degree' is not at least 1 or is greater than MAXDEG, the
       error SPICE(INVALIDDEGREE) is signaled by a routine in the
       call tree of this routine.

   5)  If the number of states `n' is not at least degree+1, the error
       SPICE(TOOFEWSTATES) is signaled by a routine in the call tree
       of this routine.

   6)  If `first' is greater than or equal to `last', the error
       SPICE(BADDESCRTIMES) is signaled by a routine in the call tree
       of this routine.

   7)  If the elements of the array `epochs' are not in strictly
       increasing order, the error SPICE(TIMESOUTOFORDER) is
       signaled by a routine in the call tree of this routine.

   8)  If the first epoch, epochs[0], is greater than `first', the
       error SPICE(BADDESCRTIMES) is signaled by a routine in the
       call tree of this routine.

   9)  If the last epoch, epochs[n-1], is less than `last', the error
       SPICE(BADDESCRTIMES) is signaled by a routine in the call tree
       of this routine.

   10) If any of the `frame' or `segid' input string pointers is
       null, the error SPICE(NULLPOINTER) is signaled.

   11) If any of the `frame' or `segid' input strings has zero
       length, the error SPICE(EMPTYSTRING) is signaled.

-Files

   A new type 9 SPK segment is written to the SPK file attached
   to handle.

-Particulars

   This routine writes an SPK type 09 data segment to the open SPK
   file according to the format described in the type 09 section of
   the SPK Required Reading. The SPK file must have been opened with
   write access.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Suppose that you have a time-ordered array of geometric states
      of a new object that follows Phobos, with a delay of 1 hour,
      in its orbit around Mars and are prepared to produce a segment
      of type 09 in an SPK file. Create a new SPK file with this
      segment. Use an existing SPK to create the input data for the
      SPK segment.

      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File: spkw09_ex1.tm

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
            mar097.bsp                       Mars satellite ephemeris
            naif0012.tls                     Leapseconds

         \begindata

            KERNELS_TO_LOAD = ( 'mar097.bsp',
                                'naif0012.tls' )

         \begintext

         End of meta-kernel


      Example code begins here.


      /.
         Program spkw09_ex1
      ./
      #include <stdio.h>
      #include <math.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local parameters.
         ./
         #define SPKNAM       "spkw09_ex1.bsp"
         #define DEGREE       3
         #define MARS         499
         #define NEPOCS       800
         #define NOBJ         403

         /.
         Local variables.
         ./
         SpiceChar          * ifname;
         SpiceChar          * segid;

         SpiceDouble          delta;
         SpiceDouble          et;
         SpiceDouble          epochs [NEPOCS];
         SpiceDouble          lt;
         SpiceDouble          state  [6];
         SpiceDouble          states [NEPOCS][6];
         SpiceDouble          step;
         SpiceDouble          time;

         SpiceInt             i;
         SpiceInt             handle;

         /.
         Load the input SPK file.
         ./
         furnsh_c ( "spkw09_ex1.tm" );

         /.
         Convert the input UTC to ephemeris time
         ./
         str2et_c ( "2018 Apr 03 08:35", &et );

         /.
         Create the time-ordered array of geometric states,
         at unequal time steps.
         ./
         time  = et;
         step  = 60.0;
         delta = 10.0;

         for ( i = 0; i < NEPOCS; i++ )
         {

            spkezr_c ( "PHOBOS",    time, "J2000", "NONE", "MARS",
                        states[i], &lt                           );

            epochs[i] = time + 3600.0;
            time = time + step + sin( halfpi_c ( ) * i / 2.0 ) * delta;

         }

         /.
         Open a new SPK file, with 5000 characters reserved
         for comments.
         ./
         ifname = "Test SPK type 9 internal filename.";
         spkopn_c ( SPKNAM, ifname, 5000, &handle );

         /.
         Create a segment identifier.
         ./
         segid = "MY_SAMPLE_SPK_TYPE_9_SEGMENT";

         /.
         Write the segment.
         ./
         spkw09_c ( handle,    NOBJ,             MARS, "J2000",
                    epochs[0], epochs[NEPOCS-1], segid, DEGREE,
                    NEPOCS,    states,           epochs        );

         /.
         Close the new SPK file.
         ./
         spkcls_c ( handle );

         /.
         Compute the state of Phobos as seen from Mars,
         12 hours after the input UTC time.
         ./
         et = et + 43200.0;
         spkezr_c ( "PHOBOS", et, "J2000", "NONE", "MARS",
                     state,  &lt                          );

         printf( "Phobos as seen from Mars at t0:\n" );
         printf( "   Epoch       (s): %19.5f\n", et );
         printf( "   Position   (km): %13.5f %13.5f %13.5f\n",
                                state[0], state[1], state[2] );
         printf( "   Velocity (km/s): %13.5f %13.5f %13.5f\n",
                                state[3], state[4], state[5] );
         printf( "\n" );

         /.
         Load the newly created kernel, and compute the state
         of the new object as seen from Mars, 13 hours after
         the input UTC time.
         ./
         furnsh_c ( SPKNAM );
         et = et + 3600.0;

         spkezr_c ( "403",   et, "J2000", "NONE", "MARS",
                     state, &lt                         );

         printf( "Object 403 as seen from Mars at t0 + 1h:\n" );
         printf( "   Epoch       (s): %19.5f\n", et );
         printf( "   Position   (km): %13.5f %13.5f %13.5f\n",
                                state[0], state[1], state[2] );
         printf( "   Velocity (km/s): %13.5f %13.5f %13.5f\n",
                                state[3], state[4], state[5] );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Phobos as seen from Mars at t0:
         Epoch       (s):     576059769.18566
         Position   (km):   -7327.26277    2414.32655    5207.10638
         Velocity (km/s):      -0.94289      -1.89473      -0.39671

      Object 403 as seen from Mars at t0 + 1h:
         Epoch       (s):     576063369.18566
         Position   (km):   -7327.26277    2414.32655    5207.10638
         Velocity (km/s):      -0.94289      -1.89473      -0.39671


      Note that after run completion, a new SPK file exists in
      the output directory.

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   K.R. Gehringer      (JPL)
   J.M. Lynch          (JPL)
   W.L. Taber          (JPL)

-Version

   -CSPICE Version 1.0.1, 10-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard. Added
       complete code example to the -Examples section.

   -CSPICE Version 1.0.0, 21-JUN-1999 (KRG) (NJB) (JML) (WLT)

-Index_Entries

   write SPK type_9 ephemeris data segment

-&
*/

{ /* Begin spkw09_c */



   /*
   Participate in error tracing.
   */
   chkin_c ( "spkw09_c" );


   /*
   Check the input strings to make sure the pointers
   are non-null and the string lengths are non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "spkw09_c", frame );
   CHKFSTR ( CHK_STANDARD, "spkw09_c", segid );


   /*
   Write the segment.
   */
   spkw09_ ( ( integer    * ) &handle,
             ( integer    * ) &body,
             ( integer    * ) &center,
             ( char       * ) frame,
             ( doublereal * ) &first,
             ( doublereal * ) &last,
             ( char       * ) segid,
             ( integer    * ) &degree,
             ( integer    * ) &n,
             ( doublereal * ) states,
             ( doublereal * ) epochs,
             ( ftnlen       ) strlen(frame),
             ( ftnlen       ) strlen(segid)  );


   chkout_c ( "spkw09_c" );

} /* End spkw09_c */
