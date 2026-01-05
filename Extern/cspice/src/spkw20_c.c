/*

-Procedure spkw20_c ( Write SPK segment, type 20 )

-Abstract

   Write a type 20 segment to an SPK file.

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

   DAF
   NAIF_IDS
   TIME
   SPK

-Keywords

   EPHEMERIS

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"
   #undef spkw20_c


   void spkw20_c  ( SpiceInt            handle,
                    SpiceInt            body,
                    SpiceInt            center,
                    ConstSpiceChar    * frame,
                    SpiceDouble         first,
                    SpiceDouble         last,
                    ConstSpiceChar    * segid,
                    SpiceDouble         intlen,
                    SpiceInt            n,
                    SpiceInt            polydg,
                    ConstSpiceDouble    cdata[],
                    SpiceDouble         dscale,
                    SpiceDouble         tscale,
                    SpiceDouble         initjd,
                    SpiceDouble         initfr  )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   handle     I   Handle of SPK file open for writing.
   body       I   NAIF code for ephemeris object.
   center     I   NAIF code for the center of motion of the body.
   frame      I   Reference frame name.
   first      I   Start time of interval covered by segment.
   last       I   End time of interval covered by segment.
   segid      I   Segment identifier.
   intlen     I   Length of time covered by logical record (days).
   n          I   Number of logical records in segment.
   polydg     I   Chebyshev polynomial degree.
   cdata      I   Array of Chebyshev coefficients and positions.
   dscale     I   Distance scale of data.
   tscale     I   Time scale of data.
   initjd     I   Integer part of begin time (TDB Julian date) of
                  first record.
   initfr     I   Fractional part of begin time (TDB Julian date) of
                  first record.
   MAXDEG     P   Maximum allowed degree of Chebyshev expansions.
   DTOL       P   Absolute tolerance for coverage bound checking.
   TOLSCL     P   Tolerance scale for coverage bound checking.

-Detailed_Input

   handle      is the DAF handle of an SPK file to which a type 20
               segment is to be added. The SPK file must be open
               for writing.

   body        is the NAIF integer code for an ephemeris object
               whose state relative to another body is described
               by the segment to be created.

   center      is the NAIF integer code for the center of motion
               of the object identified by `body'.

   frame       is the NAIF name for a reference frame relative to
               which the state information for `body' is specified.

   first,
   last        are the start and stop times of the time interval
               over which the segment defines the state of the
               object identified by `body'.

   segid       is a segment identifier. An SPK segment identifier
               may contain up to 40 characters, not counting the
               terminating null character.

   intlen      is the length of time, in TDB Julian days, covered
               by each set of Chebyshev polynomial coefficients
               (each logical record).

   n           is the number of number of logical records to be
               stored in the segment. There is one logical record
               for each time period. Each logical record contains
               three sets of Chebyshev coefficients---one for each
               coordinate---and three position vector components.

   polydg      is the degree of each set of Chebyshev
               polynomials, i.e. the number of Chebyshev
               coefficients per coordinate minus one. `polydg' must
               be less than or equal to the parameter MAXDEG.

   cdata       is an array containing all the sets of Chebyshev
               polynomial coefficients and position components to
               be placed in the new segment of the SPK file.
               There are three sets of coefficients and position
               components for each time interval covered by the
               segment.

               The coefficients and position components are
               stored in `cdata' in order as follows:

                  the (degree + 1) coefficients for the first
                  coordinate of the first logical record,
                  followed by the X component of position at the
                  first interval midpoint.

                  the coefficients for the second coordinate,
                  followed by the Y component of position at the
                  first interval midpoint.

                  the coefficients for the third coordinate,
                  followed by the Z component of position at the
                  first interval midpoint.

                  the coefficients for the first coordinate for
                  the second logical record, followed by the X
                  component of position at the second interval
                  midpoint.

                  and so on.


               A diagram follows:

                  +--------------------------------------+
                  | Coeff set for X velocity component   |
                  +--------------------------------------+
                  | X position component                 |
                  +--------------------------------------+
                  | Coeff set for Y velocity component   |
                  +--------------------------------------+
                  | Y position component                 |
                  +--------------------------------------+
                  | Coeff set for Z velocity component   |
                  +--------------------------------------+
                  | Z position component                 |
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


   dscale,
   tscale      are, respectively, the distance scale of the input
               position and velocity data in km, and the time
               scale of the input velocity data in TDB seconds.

               For example, if the input distance data have units
               of astronomical units (AU), `dscale' should be set
               to the number of km in one AU. If the input
               velocity data have time units of Julian days, then
               `tscale' should be set to the number of seconds per
               Julian day (86400).


   initjd      is the integer part of the Julian ephemeris date of
               initial epoch of the first record. `initjd' may be
               less than, equal to, or greater than the initial
               epoch.

   initfr      is the fractional part of the Julian ephemeris date
               of initial epoch of the first record. `initfr' has
               units of Julian days. `initfr' has magnitude strictly
               less than 1 day. The sum

                  initjd + initfr

               equals the Julian ephemeris date of the initial
               epoch of the first record.

-Detailed_Output

   None. This routine writes data to an SPK file.

-Parameters

   The parameters listed below are not used directly in this
   routine; they are used by the underlying SPICELIB code that
   has been translated to C via f2c.

   MAXDEG         is the maximum allowed degree of the input
                  Chebyshev expansions. MAXDEG is declared in the
                  SPICELIB Fortran INCLUDE file spk20.inc.

                  The current value of MAXDEG is 50.


   TOLSCL         is a tolerance scale factor (also called a
                  "relative tolerance") used for time coverage
                  bound checking. TOLSCL is unitless. TOLSCL
                  produces a tolerance value via the formula

                     TOL = TOLSCL * MAX( ABS(FIRST), ABS(LAST) )

                  where FIRST and LAST are the coverage time bounds
                  of a type 20 segment, expressed as seconds past
                  J2000 TDB.

                  The resulting parameter TOL is used as a tolerance
                  for comparing the input segment descriptor time
                  bounds to the first and last epoch covered by the
                  sequence of time intervals defined by the inputs

                     initjd
                     initfr
                     intlen
                     n

                  See the -Exceptions section below for a description
                  of the error check using this tolerance.

                  The current value of TOLSCL is 1e-13.

-Exceptions

   1)  If the number of sets of coefficients is not positive, the
       error SPICE(INVALIDCOUNT) is signaled by a routine in the call
       tree of this routine.

   2)  If the interval length is not positive, the error
       SPICE(INTLENNOTPOS) is signaled by a routine in the call tree
       of this routine.

   3)  If the name of the reference frame is not recognized, the
       error SPICE(INVALIDREFFRAME) is signaled by a routine in the
       call tree of this routine.

   4)  If segment stop time is not greater than or equal to the begin
       time, the error SPICE(BADDESCRTIMES) is signaled by a routine
       in the call tree of this routine.

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

   8)  If either the distance or time scale is non-positive, the
       error SPICE(NONPOSITIVESCALE) is signaled by a routine in the
       call tree of this routine.

   9)  If any of the `frame' or `segid' input string pointers is
       null, the error SPICE(NULLPOINTER) is signaled.

   10) If any of the `frame' or `segid' input strings has zero
       length, the error SPICE(EMPTYSTRING) is signaled.

-Files

   A new type 20 SPK segment is written to the SPK file attached
   to `handle'.

-Particulars

   This routine writes an SPK type 20 data segment to the designated
   SPK file, according to the format described in the SPK Required
   Reading.

   Each segment can contain data for only one target, central body,
   and reference frame. The Chebyshev polynomial degree and length
   of time covered by each logical record are also fixed. However,
   an arbitrary number of logical records of Chebyshev polynomial
   coefficients can be written in each segment. Minimizing the
   number of segments in an SPK file will help optimize how the
   SPICE system accesses the file.

-Examples

   Suppose that you have in an array `cdata' sets of Chebyshev
   polynomial coefficients and position vectors representing the state
   of the moon (NAIF ID = 301), relative to the Earth-moon barycenter
   (NAIF ID = 3), in the J2000 reference frame, and you want to put
   these into a type 20 segment in an existing SPK file. The following
   code could be used to add one new type 20 segment. To add multiple
   segments, put the call to spkw20_c in a loop.

      #include "SpiceUsr.h"
           .
           .
           .

      /.
      First open the SPK file and get a handle for it.
      ./
      spkopa_c ( spknam, &handle );

      /.
      Create a segment identifier.
      ./
      segid = "MY_SAMPLE_SPK_TYPE_20_SEGMENT";

      /.
      Note that the interval length `intlen' has units
      of Julian days. The start time of the first record
      is expressed using two inputs: integer and fractional
      portions of the Julian ephemeris date of the start
      time.

      Write the segment.
      ./
      spkw20_c ( handle, 301,    3,      "J2000",
                 first,  last,   segid,  intlen,
                 n,      polydg, cdata,  dscale,
                 tscale, initjd, initfr          );

      /.
      Close the file.
      ./
      spkcls_c ( handle );

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.0.1, 10-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard.

   -CSPICE Version 1.0.0, 23-DEC-2013 (NJB)

-Index_Entries

   write SPK type_20 data segment

-&
*/

{ /* Begin spkw20_c */


   /*
   Participate in error tracing.
   */
   chkin_c ( "spkw20_c" );

   /*
   Check the input strings to make sure the pointers
   are non-null and the string lengths are non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "spkw20_c", frame );
   CHKFSTR ( CHK_STANDARD, "spkw20_c", segid );


   /*
   Write the segment.
   */
   spkw20_ ( ( integer    * ) &handle,
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
             ( doublereal * ) &dscale,
             ( doublereal * ) &tscale,
             ( doublereal * ) &initjd,
             ( doublereal * ) &initfr,
             ( ftnlen       ) strlen(frame),
             ( ftnlen       ) strlen(segid)  );


   chkout_c ( "spkw20_c" );

} /* End spkw20_c */
