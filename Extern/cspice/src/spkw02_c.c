/*

-Procedure spkw02_c ( Write SPK segment, type 2 )

-Abstract

   Write a type 2 segment to an SPK file.

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

-Keywords

   EPHEMERIS

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"
   #undef    spkw02_c


   void spkw02_c ( SpiceInt                handle,
                   SpiceInt                body,
                   SpiceInt                center,
                   ConstSpiceChar        * frame,
                   SpiceDouble             first,
                   SpiceDouble             last,
                   ConstSpiceChar        * segid,
                   SpiceDouble             intlen,
                   SpiceInt                n,
                   SpiceInt                polydg,
                   ConstSpiceDouble        cdata [],
                   SpiceDouble             btime     )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   MAXDEG     P   Maximum degree of Chebyshev expansions.
   TOLSCL     P   Scale factor used to compute time bound tolerance.
   handle     I   Handle of an SPK file open for writing.
   body       I   Body code for ephemeris object.
   center     I   Body code for the center of motion of the body.
   frame      I   The reference frame of the states.
   first      I   First valid time for which states can be computed.
   last       I   Last valid time for which states can be computed.
   segid      I   Segment identifier.
   intlen     I   Length of time covered by logical record.
   n          I   Number of coefficient sets.
   polydg     I   Chebyshev polynomial degree.
   cdata      I   Array of Chebyshev coefficients.
   btime      I   Begin time of first logical record.

-Detailed_Input

   handle      is the DAF handle of an SPK file to which a type 2
               segment is to be added. The SPK file must be open for
               writing.

   body        is the NAIF integer code for an ephemeris object whose
               state relative to another body is described by the
               segment to be created.

   center      is the NAIF integer code for the center of motion of the
               object identified by `body'.

   frame       is the NAIF name for a reference frame relative to which
               the state information for `body' is specified.

   first,
   last        are the start and stop times of the time interval over
               which the segment defines the state of body.

   segid       is the segment identifier. An SPK segment identifier may
               contain up to 40 characters.

   intlen      is the length of time, in seconds, covered by each set of
               Chebyshev polynomial coefficients (each logical record).
               Each set of Chebyshev coefficients must cover this fixed
               time interval, `intlen'.

   n           is the number of sets of Chebyshev polynomial
               coefficients for coordinates (number of logical records)
               to be stored in the segment. There is one set of
               Chebyshev coefficients for each time period.

   polydg      is the degree of each set of Chebyshev polynomials used
               to represent the ephemeris information. That is, the
               number of Chebyshev coefficients per coordinate minus
               one. `polydg' must not exceed MAXDEG (see -Parameters
               below).

   cdata       is a time-ordered array of `n' sets of Chebyshev polynomial
               coefficients to be placed in the segment of the SPK file.
               Each set has size setsz = 3*(polydg+1). The coefficients
               are stored in `cdata' in order as follows:

                  the (polydg + 1) coefficients for the first
                  coordinate of the first logical record,

                  the coefficients for the second coordinate,

                  the coefficients for the third coordinate,

                  the coefficients for the first coordinate for
                  the second logical record, ...

                  and so on.

   btime       is the begin time (seconds past J2000 TDB) of first set
               of Chebyshev polynomial coefficients (first logical
               record). `first' is an appropriate value for `btime'.

-Detailed_Output

   None.

   The routine writes to the SPK file referred to by `handle' a type 02
   SPK segment containing the data in `cdata'.

   See the -Particulars section for details about the structure of a
   type 02 SPK segment.

-Parameters

   The parameters below are declared in the Fortran include file
   spk02.inc, which is part of the Fortran SPICE Toolkit (aka
   SPICELIB). The values of those parameters are used in CSPICE code
   generated by running f2c on SPICELIB source code. They are not
   directly referenced by code in this module.


      MAXDEG         is the maximum allowed degree of the input
                     Chebyshev expansions.

                     The value of MAXDEG is 27.


      TOLSCL         is a tolerance scale factor (also called a
                     "relative tolerance") used for time coverage bound
                     checking. TOLSCL is unitless. TOLSCL produces a
                     tolerance value via the formula

                        TOL = TOLSCL * max( abs(first), abs(last) )

                     where `first' and `last' are the coverage time
                     bounds of a type 2 segment, expressed as seconds
                     past J2000 TDB.

                     The resulting parameter TOL is used as a tolerance
                     for comparing the input segment descriptor time
                     bounds to the first and last epoch covered by the
                     sequence of time intervals defined by the inputs
                     to spkw02_c:

                        btime
                        intlen
                        n

                     The value of TOLSCL is 1.e-13.

-Exceptions

   1)  If the number of sets of coefficients is not positive, the
       error SPICE(NUMCOEFFSNOTPOS) is signaled by a routine in the
       call tree of this routine.

   2)  If the interval length is not positive, the error
       SPICE(INTLENNOTPOS) is signaled by a routine in the call tree
       of this routine.

   3)  If the name of the reference frame is not recognized, the
       error SPICE(INVALIDREFFRAME) is signaled by a routine in the
       call tree of this routine.

   4)  If segment stop time is not greater then the begin time, the
       error SPICE(BADDESCRTIMES) is signaled by a routine in the
       call tree of this routine.

   5)  If the start time of the first record exceeds the descriptor
       begin time by more than a computed tolerance, or if the end
       time of the last record precedes the descriptor end time by
       more than a computed tolerance, the error SPICE(COVERAGEGAP)
       is signaled by a routine in the call tree of this routine. See
       the -Parameters section above for a description of the
       tolerance.

   6)  If the input degree `polydg' is less than 0 or greater than
       MAXDEG, the error SPICE(INVALIDDEGREE) is signaled by a
       routine in the call tree of this routine.

   7)  If the last non-blank character of `segid' occurs past index 40,
       or if `segid' contains any nonprintable characters, an error is
       signaled by a routine in the call tree of this routine.

   8)  If any of the `frame' or `segid' input string pointers is
       null, the error SPICE(NULLPOINTER) is signaled.

   9)  If any of the `frame' or `segid' input strings has zero
       length, the error SPICE(EMPTYSTRING) is signaled.

-Files

   A new type 2 SPK segment is written to the SPK file attached
   to handle.

-Particulars

   This routine writes an SPK type 2 data segment to the designated
   SPK file, according to the format described in the SPK Required
   Reading.

   Each segment can contain data for only one target, central body,
   and reference frame. The Chebyshev polynomial degree and length
   of time covered by each logical record are also fixed. However,
   an arbitrary number of logical records of Chebyshev polynomial
   coefficients can be written in each segment. Minimizing the
   number of segments in an SPK file will help optimize how the SPICE
   system accesses the file.

   The ephemeris data supplied to the type 2 SPK writer is packed
   into an array as a sequence of records. The logical data records
   are stored contiguously:

      +----------+
      | Record 1 |
      +----------+
      | Record 2 |
      +----------+
          ...
      +----------+
      | Record n |
      +----------+

   The contents of an individual record are:

      +--------------------------------------+
      | Coeff set for X position component   |
      +--------------------------------------+
      | Coeff set for Y position component   |
      +--------------------------------------+
      | Coeff set for Z position component   |
      +--------------------------------------+

   Each coefficient set has the structure:

      +--------------------------------------+
      | Coefficient of T_0                   |
      +--------------------------------------+
      | Coefficient of T_1                   |
      +--------------------------------------+
                        ...
      +--------------------------------------+
      | Coefficient of T_POLYDG              |
      +--------------------------------------+

   Where T_n represents the Chebyshev polynomial
   of the first kind of degree n.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) This example demonstrates how to create an SPK type 2 kernel
      containing only one segment, given a set of Chebychev
      coefficients and their associated epochs.


      Example code begins here.


      /.
         Program spkw02_ex1
      ./
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Define the segment identifier parameters.
         ./
         #define SPK2         "spkw02_ex1.bsp"
         #define REF          "J2000"
         #define BODY         3
         #define CENTER       10
         #define CHBDEG       2
         #define NRECS        4
         #define RECSIZ     ( 3*(CHBDEG+1) )

         /.
         Local variables.
         ./
         SpiceChar          * ifname;
         SpiceChar          * segid;

         SpiceDouble          first;
         SpiceDouble          intlen;
         SpiceDouble          last;

         SpiceInt             handle;
         SpiceInt             ncomch;

         /.
         Define the coefficients.
         ./
         SpiceDouble          recrds [NRECS][RECSIZ] = {
                                   {1.0101, 1.0102, 1.0103,
                                    1.0201, 1.0202, 1.0203,
                                    1.0301, 1.0302, 1.0303},
                                   {2.0101, 2.0102, 2.0103,
                                    2.0201, 2.0202, 2.0203,
                                    2.0301, 2.0302, 2.0303},
                                   {3.0101, 3.0102, 3.0103,
                                    3.0201, 3.0202, 3.0203,
                                    3.0301, 3.0302, 3.0303},
                                   {4.0101, 4.0102, 4.0103,
                                    4.0201, 4.0202, 4.0203,
                                    4.0301, 4.0302, 4.0303} };

         /.
         Set the start and end times of interval covered by
         segment, and the length of time covered by logical
         record.
         ./
         first  = 100.0;
         last   = 500.0;
         intlen = 100.0;

         /.
         `ncomch' is the number of characters to reserve for the
         kernel's comment area. This example doesn't write
         comments, so set to zero.
         ./
         ncomch = 0;

         /.
         Internal file name and segment ID.
         ./
         ifname = "Type 2 SPK internal file name.";
         segid  = "SPK type 2 test segment";

         /.
         Open a new SPK file.
         ./
         spkopn_c ( SPK2, ifname, ncomch, &handle );

         /.
         Write the segment.
         ./
         spkw02_c ( handle, BODY,   CENTER, REF,    first, last,
                    segid,  intlen, NRECS,  CHBDEG, recrds, first );

         /.
         Close the SPK file.
         ./
         spkcls_c ( handle );

         return ( 0 );
      }


      When this program is executed, no output is presented on
      screen. After run completion, a new SPK type 2 exists in
      the output directory.

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   K.S. Zukor          (JPL)

-Version

   -CSPICE Version 2.0.1, 10-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard. Added complete
       example code from existing fragment.

       Extended "polydg" and "cdata" arguments description to provide the
       size of the Chebyshev polynomials sets. Moved the details of the SPK
       structure from "cdata" argument description to -Particulars section.

       Added entry #4 in -Exceptions section.

   -CSPICE Version 2.0.0, 09-JAN-2014 (NJB)

       Relaxed test on relationship between the time bounds of the
       input record set (determined by `btime', `intlen', and `n') and
       the descriptor bounds `first' and `last'. Now the descriptor
       bounds may extend beyond the time bounds of the record set by a
       ratio computed using the parameter TOLSCL (see -Parameters above
       for details). Added checks on input polynomial degree.

   -CSPICE Version 1.0.0, 21-JUL-1999 (NJB) (KSZ)

-Index_Entries

   write SPK type_2 data segment

-&
*/

{ /* Begin spkw02_c */

   /*
   Participate in error tracing.
   */
   chkin_c ( "spkw02_c" );


   /*
   Check the input strings to make sure the pointers
   are non-null and the string lengths are non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "spkw02_c", frame );
   CHKFSTR ( CHK_STANDARD, "spkw02_c", segid );


   /*
   Write the segment.
   */

   spkw02_ ( ( integer    * ) &handle,
             ( integer    * ) &body,
             ( integer    * ) &center,
             ( char       * ) frame,
             ( doublereal * ) &first,
             ( doublereal * ) &last,
             ( char       * ) segid,
             ( doublereal * ) &intlen,
             ( integer    * ) &n,
             ( integer    * ) &polydg,
             ( doublereal * ) cdata,
             ( doublereal * ) &btime,
             ( ftnlen       ) strlen(frame),
             ( ftnlen       ) strlen(segid)  );


   chkout_c ( "spkw02_c" );

} /* End spkw02_c */
