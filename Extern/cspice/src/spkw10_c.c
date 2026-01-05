/*

-Procedure spkw10_c (SPK - write a type 10 segment )

-Abstract

   Write an SPK type 10 segment to the file specified by
   the input `handle'.

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
   SPK

-Keywords

   SPK

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"
   #undef    spkw10_c

   void spkw10_c ( SpiceInt           handle,
                   SpiceInt           body,
                   SpiceInt           center,
                   ConstSpiceChar   * frame,
                   SpiceDouble        first,
                   SpiceDouble        last,
                   ConstSpiceChar   * segid,
                   ConstSpiceDouble   consts [8],
                   SpiceInt           n,
                   ConstSpiceDouble   elems  [],
                   ConstSpiceDouble   epochs []  )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   handle     I   The handle of a DAF file open for writing.
   body       I   The NAIF ID code for the body of the segment.
   center     I   The center of motion for `body'.
   frame      I   The reference frame for this segment.
   first      I   The first epoch for which the segment is valid.
   last       I   The last  epoch for which the segment is valid.
   segid      I   The string to use for segment identifier.
   consts     I   Array of geophysical constants for the segment.
   n          I   The number of element/epoch pairs to be stored
   elems      I   The collection of "two-line" element sets.
   epochs     I   The epochs associated with the element sets.

-Detailed_Input

   handle      is the file handle of a SPK file that has been opened
               for writing.

   body        is the NAIF ID for the body whose states are
               to be recorded in an SPK file.

   center      is the NAIF ID for the center of motion associated
               with `body'.

   frame       is the reference frame that states are referenced to,
               for example "J2000".

   first,
   last        are the bounds on the ephemeris times, expressed as
               seconds past J2000, for which the states can be used
               to interpolate a state for `body'.

   segid       is the segment identifier. An SPK segment identifier
               may contain up to 40 characters.

   consts      are the geophysical constants needed for evaluation
               of the two line elements sets. The order of these
               constants must be:

                  consts[0] = J2 gravitational harmonic for Earth.
                  consts[1] = J3 gravitational harmonic for Earth.
                  consts[2] = J4 gravitational harmonic for Earth.

               These first three constants are dimensionless.

                  consts[3] = KE: Square root of the GM for Earth where
                              GM is expressed in Earth radii cubed
                              per minutes squared.

                  consts[4] = QO: High altitude bound for atmospheric
                              model in km.

                  consts[5] = SO: Low altitude bound for atmospheric
                              model in km.

                  consts[6] = RE: Equatorial radius of the earth in km.

                  consts[7] = AE: Distance units/earth radius
                             (normally 1).

               Below are currently recommended values for these
               items:

                  J2 =    1.082616e-3
                  J3 =   -2.53881e-6
                  J4 =   -1.65597e-6

               The next item is the square root of GM for the Earth
               given in units of earth-radii**1.5/Minute

                  KE =    7.43669161e-2

               The next two items define the top and bottom of the
               atmospheric drag model used by the type 10 ephemeris
               type. Don't adjust these unless you understand the full
               implications of such changes.

                  QO =  120.0e0
                  SO =   78.0e0

               The ER value is the equatorial radius in km of the Earth
               as used by NORAD.

                  ER = 6378.135e0

               The value of AE is the number of distance units per
               Earth radii used by the NORAD state propagation
               software. The value should be 1 unless you've got a very
               good understanding of the NORAD routine SGP4 and the
               affect of changing this value.

                  AE =    1.0e0

   n           is the number of "two-line" element sets and epochs
               to be stored in the segment.

   elems       is a time-ordered array of two-line elements as supplied
               in NORAD two-line element files. The i'th set of
               elements should be stored as shown here:

                  base = i*10

                  elems[ base + 0 ] = NDT20 in radians/minute**2
                  elems[ base + 1 ] = NDD60 in radians/minute**3
                  elems[ base + 2 ] = BSTAR
                  elems[ base + 3 ] = INCL  in radians
                  elems[ base + 4 ] = NODE0 in radians
                  elems[ base + 5 ] = ECC
                  elems[ base + 6 ] = OMEGA in radians
                  elems[ base + 7 ] = M0    in radians
                  elems[ base + 8 ] = N0    in radians/minute
                  elems[ base + 9 ] = EPOCH the elements in seconds
                                      past ephemeris epoch J2000.

               The meaning of these variables is defined by the
               format of the two-line element files available from
               NORAD.

   epochs      is an n-dimensional array that contains the epochs
               (ephemeris seconds past J2000) corresponding to the
               elements in `elems'. The i'th epoch must equal the epoch
               of the i'th element set. `epochs' must form a strictly
               increasing sequence.

-Detailed_Output

   None.

   The routine writes an SPK type 10 segment to the file attached to
   `handle'.

-Parameters

   None.

-Exceptions

   1)  If the structure or content of the inputs are invalid, an
       error is signaled by a routine in the call tree of this
       routine.

   2)  If any file access error occurs, the error is signaled by a
       routine in the call tree of this routine.

   3)  If any of the `frame' or `segid' input string pointers is
       null, the error SPICE(NULLPOINTER) is signaled.

   4)  If any of the `frame' or `segid' input strings has zero
       length, the error SPICE(EMPTYSTRING) is signaled.

-Files

   None.

-Particulars

   This routine writes a type 10 SPK segment to the SPK file open
   for writing that is attached to `handle'.

   The routine getelm_c reads two-line element sets, as those
   distributed by NORAD, and converts them to the elements in units
   suitable for use in this routine.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Suppose that you have collected the two-line element data
      for a spacecraft with NORAD ID 18123. The following example
      code demonstrates how you could go about creating a type 10
      SPK segment.

      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File name: spkw10_ex1.tm

         This meta-kernel is intended to support operation of SPICE
         example programs. The kernels shown here should not be
         assumed to contain adequate or correct versions of data
         required by SPICE-based user applications.

         In order for an application to use this meta-kernel, the
         kernels referenced here must be present in the user's
         current working directory.

         The names and contents of the kernels referenced
         by this meta-kernel are as follows:

            File name           Contents
            ---------           ------------------------------------
            naif0012.tls        Leapseconds
            geophysical.ker     geophysical constants for evaluation
                                of two-line element sets.

         The geophysical.ker is a PCK file that is provided with the
         SPICE toolkit under the "/data" directory.

         \begindata

            KERNELS_TO_LOAD = ( 'naif0012.tls',
                                'geophysical.ker'  )

         \begintext

         End of meta-kernel


      Example code begins here.


      /.
         Program spkw10_ex1
      ./
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local parameters.
         ./
         #define PNAMLN       3
         #define SPK10        "spkw10_ex1.bsp"
         #define TLELLN       70

         /.
         The SPK type 10 segment will contain 18 two-line
         elements sets for the NORAD spacecraft 18123 with
         respect to the Earth (ID 399) in the J2000 reference
         frame.

         As stated in the naif_ids required reading, for Earth
         orbiting spacecraft lacking a DSN identification code,
         the NAIF ID is derived from the tracking ID assigned to
         it by NORAD via:

            NAIF ID = -100000 - NORAD ID code
         ./
         #define TLESSZ       9
         #define BODY         -118123
         #define CENTER       399
         #define FRMNAM       "J2000"

         /.
         Local variables.
         ./
         SpiceChar          * ifname;
         SpiceChar          * segid;

         SpiceDouble          consts [8];
         SpiceDouble          elems  [10*TLESSZ];
         SpiceDouble          epochs [TLESSZ];
         SpiceDouble          first;
         SpiceDouble          last;

         SpiceInt             handle;
         SpiceInt             i;
         SpiceInt             n;
         SpiceInt             ncomch;

         /.
         These are the variables that will hold the constants
         required by SPK type 10. These constants are available
         from the loaded PCK file, which provides the actual
         values and units as used by NORAD propagation model.

            Constant   Meaning
            --------   ------------------------------------------
            J2         J2 gravitational harmonic for Earth.
            J3         J3 gravitational harmonic for Earth.
            J4         J4 gravitational harmonic for Earth.
            KE         Square root of the GM for Earth.
            QO         High altitude bound for atmospheric model.
            SO         Low altitude bound for atmospheric model.
            ER         Equatorial radius of the Earth.
            AE         Distance units/earth radius.
         ./
         SpiceChar            noadpn [8][PNAMLN] = {
                                   "J2", "J3", "J4", "KE",
                                   "QO", "SO", "ER", "AE" };

         /.
         Define the Two-Line Element sets.
         ./
         SpiceChar            tle    [2*TLESSZ][TLELLN] = {
                            "1 18123U 87 53  A 87324.61041692 -.00000023"
                                             "  00000-0 -75103-5 0 00675",
                            "2 18123  98.8296 152.0074 0014950 168.7820 "
                                             "191.3688 14.12912554 21686",
                            "1 18123U 87 53  A 87326.73487726  .00000045"
                                             "  00000-0  28709-4 0 00684",
                            "2 18123  98.8335 154.1103 0015643 163.5445 "
                                             "196.6235 14.12912902 21988",
                            "1 18123U 87 53  A 87331.40868801  .00000104"
                                             "  00000-0  60183-4 0 00690",
                            "2 18123  98.8311 158.7160 0015481 149.9848 "
                                             "210.2220 14.12914624 22644",
                            "1 18123U 87 53  A 87334.24129978  .00000086"
                                             "  00000-0  51111-4 0 00702",
                            "2 18123  98.8296 161.5054 0015372 142.4159 "
                                             "217.8089 14.12914879 23045",
                            "1 18123U 87 53  A 87336.93227900 -.00000107"
                                             "  00000-0 -52860-4 0 00713",
                            "2 18123  98.8317 164.1627 0014570 135.9191 "
                                             "224.2321 14.12910572 23425",
                            "1 18123U 87 53  A 87337.28635487  .00000173"
                                             "  00000-0  10226-3 0 00726",
                            "2 18123  98.8284 164.5113 0015289 133.5979 "
                                             "226.6438 14.12916140 23475",
                            "1 18123U 87 53  A 87339.05673569  .00000079"
                                             "  00000-0  47069-4 0 00738",
                            "2 18123  98.8288 166.2585 0015281 127.9985 "
                                             "232.2567 14.12916010 24908",
                            "1 18123U 87 53  A 87345.43010859  .00000022"
                                             "  00000-0  16481-4 0 00758",
                            "2 18123  98.8241 172.5226 0015362 109.1515 "
                                             "251.1323 14.12915487 24626",
                            "1 18123U 87 53  A 87349.04167543  .00000042"
                                             "  00000-0  27370-4 0 00764",
                            "2 18123  98.8301 176.1010 0015565 100.0881 "
                                             "260.2047 14.12916361 25138"  };

         /.
         Load the PCK file that provides the geophysical
         constants required for the evaluation of the two-line
         elements sets. Load also an LSK, as it is required by
         getelm_c to perform time conversions. Use a metakernel for
         convenience.
         ./
         furnsh_c ( "spkw10_ex1.tm" );

         /.
         Retrieve the data from the kernel, and place it on
         the `consts' array.
         ./
         for ( i = 0; i < 8; i++ )
         {
            bodvcd_c ( CENTER, noadpn[i], 1, &n, consts+i );
         }

         /.
         Convert the Two Line Elements lines to the
         element sets.
         ./
         for ( i = 0; i < TLESSZ; i++ )
         {
            getelm_c ( 1950, TLELLN, tle[i*2], epochs+i, elems+(i*10) );
         }

         /.
         Define the beginning and end of the segment to be
         -/+ 12 hours from the first and last epochs,
         respectively.
         ./
         first = epochs[0] - 0.5 * spd_c();
         last  = epochs[TLESSZ-1] + 0.5 * spd_c();

         /.
         `ncomch' is the number of characters to reserve for the
         kernel's comment area. This example doesn't write
         comments, so set to zero.
         ./
         ncomch = 0;

         /.
         Internal file name and segment ID.
         ./
         ifname = "Test for type 10 SPK internal file name";
         segid  = "SPK type 10 test segment";

         /.
         Open a new SPK file.
         ./
         spkopn_c ( SPK10, ifname, ncomch, &handle );

         /.
         Now add the segment.
         ./
         spkw10_c ( handle, BODY,   CENTER, FRMNAM, first,  last,
                    segid,  consts, TLESSZ, elems,  epochs );

         /.
         Close the SPK file.
         ./
         spkcls_c ( handle );

         return ( 0 );
      }


      When this program is executed, no output is presented on
      screen. After run completion, a new SPK type 10 exists in
      the output directory.

-Restrictions

   None.

-Literature_References

   [1]  F. Hoots and R. Roehrich, "Spacetrack Report #3: Models for
        Propagation of the NORAD Element Sets," U.S. Air Force
        Aerospace Defense Command, Colorado Springs, CO, 1980.

   [2]  F. Hoots, "Spacetrack Report #6: Models for Propagation of
        Space Command Element Sets,"  U.S. Air Force Aerospace
        Defense Command, Colorado Springs, CO, 1986.

   [3]  F. Hoots, P. Schumacher and R. Glover, "History of Analytical
        Orbit Modeling in the U. S. Space Surveillance System,"
        Journal of Guidance, Control, and Dynamics. 27(2):174-185,
        2004.

   [4]  D. Vallado, P. Crawford, R. Hujsak and T. Kelso, "Revisiting
        Spacetrack Report #3," paper AIAA 2006-6753 presented at the
        AIAA/AAS Astrodynamics Specialist Conference, Keystone, CO.,
        August 21-24, 2006.

-Author_and_Institution

   N.J. Bachman        (JPL)
   M. Costa Sitja      (JPL)
   J. Diaz del Rio     (ODC Space)
   B.V. Semenov        (JPL)
   W.L. Taber          (JPL)

-Version

   -CSPICE Version 1.0.2, 05-NOV-2021 (JDR) (MCS)

       Edited the header to comply with NAIF standard. Added complete
       code example based on existing example.

       Added NAIF_IDS to the list of required readings. Added relevant
       reports and papers to -Literature_References.

   -CSPICE Version 1.0.1, 30-OCT-2006 (BVS)

       Deleted "inertial" from the FRAME description in the -Brief_I/O
       section of the header.

   -CSPICE Version 1.0.0, 29-JUN-1999 (NJB) (WLT)

-Index_Entries

   write a type_10 SPK segment

-&
*/

{ /* Begin spkw10_c */



   /*
   Participate in error tracing.
   */
   chkin_c ( "spkw10_c" );


   /*
   Check the input strings to make sure the pointers
   are non-null and the string lengths are non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "spkw10_c", frame );
   CHKFSTR ( CHK_STANDARD, "spkw10_c", segid );


   /*
   Write the segment.
   */
   spkw10_ ( ( integer    * ) &handle,
             ( integer    * ) &body,
             ( integer    * ) &center,
             ( char       * ) frame,
             ( doublereal * ) &first,
             ( doublereal * ) &last,
             ( char       * ) segid,
             ( doublereal * ) consts,
             ( integer    * ) &n,
             ( doublereal * ) elems,
             ( doublereal * ) epochs,
             ( ftnlen       ) strlen(frame),
             ( ftnlen       ) strlen(segid)  );


   chkout_c ( "spkw10_c" );

} /* End spkw10_c */
