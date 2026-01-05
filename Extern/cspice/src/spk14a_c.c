/*

-Procedure spk14a_c ( SPK, add data to a type 14 segment )

-Abstract

   Add data to a type 14 SPK segment associated with `handle'.

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

   SPK

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"
   #undef    spk14a_c


   void spk14a_c ( SpiceInt           handle,
                   SpiceInt           ncsets,
                   ConstSpiceDouble   coeffs [],
                   ConstSpiceDouble   epochs []  )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   handle     I   The handle of an SPK file open for writing.
   ncsets     I   The number of coefficient sets and epochs.
   coeffs     I   The collection of coefficient sets.
   epochs     I   The epochs associated with the coefficient sets.

-Detailed_Input

   handle      is the file handle of an SPK file that has been
               opened for writing.

   ncsets      is the number of Chebyshev coefficient sets and epochs
               to be stored in the segment.

   coeffs      is a time-ordered array of `ncsets' of Chebyshev polynomial
               coefficients to be placed in the segment of the SPK file.
               Each set has size SETSZ = 2 + 6*(CHBDEG+1), where CHBDEG
               is the degree of the Chebyshev polynomials used to
               represent the ephemeris information.

               These sets are used to compute the state vector, which
               consists of position components X, Y, Z and velocity
               components dX/dt, dY/dt, dZ/dt, of a body relative to
               a center of motion.

               See the -Particulars section for details on how to store
               the coefficient sets in the array.

   epochs      contains the initial epochs (ephemeris seconds past
               J2000) corresponding to the Chebyshev coefficients in
               `coeffs'. The I'th epoch is associated with the I'th
               Chebyshev coefficient set. The epochs must form a
               strictly increasing sequence.

-Detailed_Output

   None. The ephemeris data is stored in a segment in the SPK file
   associated with `handle'.

   See the -Particulars section for details about the structure of a
   type 14 SPK segment.

-Parameters

   None.

-Exceptions

   1)  If the number of coefficient sets and epochs is not positive,
       the error SPICE(INVALIDARGUMENT) is signaled by a routine in
       the call tree of this routine.

-Files

   None.

-Particulars

   This routine adds data to a type 14 SPK segment that is associated
   with the input argument handle. The segment must have been started
   by a call to the routine spk14b_c, the routine which begins a type
   14 SPK segment.

   This routine is one of a set of three routines for creating and
   adding data to type 14 SPK segments. These routines are:

      spk14b_c: Begin a type 14 SPK segment. This routine must be
                 called before any data may be added to a type 14
                 segment.

      spk14a_c: Add data to a type 14 SPK segment. This routine may be
                 called any number of times after a call to spk14b_c to
                 add type 14 records to the SPK segment that was
                 started.

      spk14e_c: End a type 14 SPK segment. This routine is called to
                 make the type 14 segment a permanent addition to the
                 SPK file. Once this routine is called, no further type
                 14 records may be added to the segment. A new segment
                 must be started.

   A type 14 SPK segment consists of coefficient sets for fixed order
   Chebyshev polynomials over consecutive time intervals, where the
   time intervals need not all be of the same length. The Chebyshev
   polynomials represent the position, X, Y, and Z coordinates, and
   the velocities, dX/dt, dY/dt, and dZ/dt, of body relative to
   center.

   The ephemeris data supplied to the type 14 SPK writer is packed
   into an array as a sequence of logical records,

      -----------------------------------------------------
      | Record 1 | Record 2 | ... | Record N-1 | Record N |
      -----------------------------------------------------

   with each record has the following format.

         ------------------------------------------------
         |  The midpoint of the approximation interval  |
         ------------------------------------------------
         |  The radius of the approximation interval    |
         ------------------------------------------------
         |  chbdeg+1 coefficients for the X coordinate  |
         ------------------------------------------------
         |  chbdeg+1 coefficients for the Y coordinate  |
         ------------------------------------------------
         |  chbdeg+1 coefficients for the Z coordinate  |
         ------------------------------------------------
         |  chbdeg+1 coefficients for the X velocity    |
         ------------------------------------------------
         |  chbdeg+1 coefficients for the Y velocity    |
         ------------------------------------------------
         |  chbdeg+1 coefficients for the Z velocity    |
         ------------------------------------------------

-Examples

   The numerical results shown for these examples may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) This example demonstrates how to create an SPK type 14 kernel
      containing only one segment, given a set of Chebyshev
      coefficients and their associated epochs.


      Example code begins here.


      /.
         Program spk14a_ex1
      ./
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Define the segment identifier parameters.
         ./
         #define SPK14        "spk14a_ex1.bsp"
         #define REF          "J2000"
         #define BODY         3
         #define CENTER       10
         #define CHBDEG       2
         #define NRECS        4
         #define RECSIZ     ( 2 + 6*(CHBDEG+1) )

         /.
         Local variables.
         ./
         SpiceChar          * ifname;
         SpiceChar          * segid;

         SpiceDouble          first;
         SpiceDouble          last;

         SpiceInt             handle;
         SpiceInt             ncomch;

         /.
         Define the epochs and coefficients.
         ./
         SpiceDouble          epochs [NRECS+1] = {
                        100.0, 200.0, 300.0, 400.0, 500.0 };

         SpiceDouble          recrds [NRECS][RECSIZ] = {
                                   {150.0, 50.0,
                                    1.0101, 1.0102, 1.0103,
                                    1.0201, 1.0202, 1.0203,
                                    1.0301, 1.0302, 1.0303,
                                    1.0401, 1.0402, 1.0403,
                                    1.0501, 1.0502, 1.0503,
                                    1.0601, 1.0602, 1.0603},
                                   {250.0, 50.0,
                                    2.0101, 2.0102, 2.0103,
                                    2.0201, 2.0202, 2.0203,
                                    2.0301, 2.0302, 2.0303,
                                    2.0401, 2.0402, 2.0403,
                                    2.0501, 2.0502, 2.0503,
                                    2.0601, 2.0602, 2.0603},
                                   {350.0, 50.0,
                                    3.0101, 3.0102, 3.0103,
                                    3.0201, 3.0202, 3.0203,
                                    3.0301, 3.0302, 3.0303,
                                    3.0401, 3.0402, 3.0403,
                                    3.0501, 3.0502, 3.0503,
                                    3.0601, 3.0602, 3.0603},
                                   {450.0, 50.0,
                                    4.0101, 4.0102, 4.0103,
                                    4.0201, 4.0202, 4.0203,
                                    4.0301, 4.0302, 4.0303,
                                    4.0401, 4.0402, 4.0403,
                                    4.0501, 4.0502, 4.0503,
                                    4.0601, 4.0602, 4.0603} };

         /.
         Set the start and end times of interval covered by
         segment.
         ./
         first = epochs[0];
         last  = epochs[NRECS];

         /.
         `ncomch' is the number of characters to reserve for the
         kernel's comment area. This example doesn't write
         comments, so set to zero.
         ./
         ncomch = 0;

         /.
         Internal file name and segment ID.
         ./
         ifname = "Type 14 SPK internal file name.";
         segid  = "SPK type 14 test segment";

         /.
         Open a new SPK file.
         ./
         spkopn_c ( SPK14, ifname, ncomch, &handle );

         /.
         Begin the segment.
         ./
         spk14b_c ( handle, segid, BODY, CENTER, REF, first, last, CHBDEG );

         /.
         Add the data to the segment all at once.
         ./
         spk14a_c ( handle, NRECS, recrds, epochs );

         /.
         End the segment, making the segment a permanent addition
         to the SPK file.
         ./
         spk14e_c ( handle );

         /.
         Close the SPK file.
         ./
         spkcls_c ( handle );

         return ( 0 );
      }


      When this program is executed, no output is presented on
      screen. After run completion, a new SPK type 14 exists in
      the output directory.

   2) This example demonstrates how to add type 14 SPK records to the
      segment being written, one at a time. The ability to write the
      records in this way is useful if computer memory is limited. It
      may also be convenient from a programming perspective to write
      the records this way.


      Example code begins here.


      /.
         Program spk14a_ex2
      ./
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Define the segment identifier parameters.
         ./
         #define SPK14        "spk14a_ex2.bsp"
         #define REF          "J2000"
         #define BODY         3
         #define CENTER       10
         #define CHBDEG       2
         #define NRECS        4
         #define RECSIZ     ( 2 + 6*(CHBDEG+1) )

         /.
         Local variables.
         ./
         SpiceChar          * ifname;
         SpiceChar          * segid;

         SpiceDouble          first;
         SpiceDouble          last;

         SpiceInt             handle;
         SpiceInt             i;
         SpiceInt             ncomch;

         /.
         Define the epochs and coefficients.
         ./
         SpiceDouble          epochs [NRECS+1] = {
                        100.0, 200.0, 300.0, 400.0, 500.0 };

         SpiceDouble          recrds [NRECS][RECSIZ] = {
                                   {150.0, 50.0,
                                    1.0101, 1.0102, 1.0103,
                                    1.0201, 1.0202, 1.0203,
                                    1.0301, 1.0302, 1.0303,
                                    1.0401, 1.0402, 1.0403,
                                    1.0501, 1.0502, 1.0503,
                                    1.0601, 1.0602, 1.0603},
                                   {250.0, 50.0,
                                    2.0101, 2.0102, 2.0103,
                                    2.0201, 2.0202, 2.0203,
                                    2.0301, 2.0302, 2.0303,
                                    2.0401, 2.0402, 2.0403,
                                    2.0501, 2.0502, 2.0503,
                                    2.0601, 2.0602, 2.0603},
                                   {350.0, 50.0,
                                    3.0101, 3.0102, 3.0103,
                                    3.0201, 3.0202, 3.0203,
                                    3.0301, 3.0302, 3.0303,
                                    3.0401, 3.0402, 3.0403,
                                    3.0501, 3.0502, 3.0503,
                                    3.0601, 3.0602, 3.0603},
                                   {450.0, 50.0,
                                    4.0101, 4.0102, 4.0103,
                                    4.0201, 4.0202, 4.0203,
                                    4.0301, 4.0302, 4.0303,
                                    4.0401, 4.0402, 4.0403,
                                    4.0501, 4.0502, 4.0503,
                                    4.0601, 4.0602, 4.0603} };

         /.
         Set the start and end times of interval covered by
         segment.
         ./
         first = epochs[0];
         last  = epochs[NRECS];

         /.
         `ncomch' is the number of characters to reserve for the
         kernel's comment area. This example doesn't write
         comments, so set to zero.
         ./
         ncomch = 0;

         /.
         Internal file name and segment ID.
         ./
         ifname = "Type 14 SPK internal file name.";
         segid  = "SPK type 14 test segment";

         /.
         Open a new SPK file.
         ./
         spkopn_c ( SPK14, ifname, ncomch, &handle );

         /.
         Begin the segment.
         ./
         spk14b_c ( handle, segid, BODY, CENTER, REF, first, last, CHBDEG );

         /.
         Write the records to the segment in the
         SPK file one at at time.
         ./
         for ( i = 0; i < NRECS; i++ )
         {
            spk14a_c ( handle, 1, recrds[i], epochs+i );
         }

         /.
         End the segment, making the segment a permanent addition
         to the SPK file.
         ./
         spk14e_c ( handle );

         /.
         Close the SPK file.
         ./
         spkcls_c ( handle );

         return ( 0 );
      }


      When this program is executed, no output is presented on
      screen. After run completion, a new SPK type 14 exists in
      the output directory.

-Restrictions

   1)  The type 14 SPK segment to which we are adding data must have
       been started by the routine SPK14B, the routine which begins a
       type 14 SPK segment.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   K.R. Gehringer      (JPL)

-Version

   -CSPICE Version 1.0.1, 27-AUG-2021 (JDR) (NJB)

       Updated header to comply with NAIF standard. Added complete examples
       code from existing fragments.

       Extended "coeffs" argument description to provide the size of the
       Chebyshev polynomials sets.

       Re-ordered header sections. Removed reference to other routines
       from -Abstract section (already in -Particulars).

   -CSPICE Version 1.0.0, 29-JUL-1999 (NJB) (KRG)

-Index_Entries

   add data to a type_14 SPK segment

-&
*/

{ /* Begin spk14a_c */



   /*
   Participate in error tracing.
   */
   chkin_c ( "spk14a_c" );


   spk14a_ ( ( integer    * ) &handle,
             ( integer    * ) &ncsets,
             ( doublereal * ) coeffs,
             ( doublereal * ) epochs  );


   chkout_c ( "spk14a_c" );

} /* End spk14a_c */
