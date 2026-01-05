/*

-Procedure spkw18_c ( Write SPK segment, type 18 )

-Abstract

   Write a type 18 segment to an SPK file.

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
   #undef    spkw18_c


   void spkw18_c ( SpiceInt             handle,
                   SpiceSPK18Subtype    subtyp,
                   SpiceInt             body,
                   SpiceInt             center,
                   ConstSpiceChar     * frame,
                   SpiceDouble          first,
                   SpiceDouble          last,
                   ConstSpiceChar     * segid,
                   SpiceInt             degree,
                   SpiceInt             n,
                   const void         * packts,
                   ConstSpiceDouble     epochs[]     )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   handle     I   Handle of an SPK file open for writing.
   subtyp     I   SPK type 18 subtype code.
   body       I   NAIF code for an ephemeris object.
   center     I   NAIF code for center of motion of `body'.
   frame      I   Reference frame name.
   first      I   Start time of interval covered by segment.
   last       I   End time of interval covered by segment.
   segid      I   Segment identifier.
   degree     I   Degree of interpolating polynomials.
   n          I   Number of packets.
   packts     I   Array of packets.
   epochs     I   Array of epochs corresponding to packets.
   MAXDEG     P   Maximum allowed degree of interpolating polynomial.

-Detailed_Input

   handle      is the file handle of an SPK file that has been opened
               for writing.

   subtyp      is an integer code indicating the subtype of the
               segment to be created.

   body        is the NAIF integer code for an ephemeris object whose
               state relative to another body is described by the
               segment to be created.

   center      is the NAIF integer code for the center of motion of the
               object identified by `body'.

   frame       is the NAIF name for a reference frame relative to which
               the state information for `body' is specified.

   first,
   last        are, respectively, the start and stop times of the time
               interval over which the segment defines the state of
               `body'.

   segid       is the segment identifier. An SPK segment identifier may
               contain up to 40 characters.

   degree      is the nominal degree of the polynomials used to
               interpolate the states contained in the input packets.
               All components of the state vectors are interpolated by
               polynomials of the specified degree, except near the
               segment boundaries, or if the total number of states in
               the segment is too few to allow interpolation using the
               specified degree.

               If the actual interpolation degree is reduced, the
               highest degree feasible degree valid for the
               interpolation type is used.

   n           is the number of packets in the input packet array.

   packts      is a time-ordered array of data packets representing
               geometric states of `body' relative to `center', specified
               relative to `frame'. The packet structure depends on the
               segment subtype as follows:

                  Type 0 (indicated by code S18TP0):

                     x,  y,  z,  dx/dt,  dy/dt,  dz/dt,
                     vx, vy, vz, dvx/dt, dvy/dt, dvz/dt

                  where x, y, z represent Cartesian position components
                  and  vx, vy, vz represent Cartesian velocity
                  components. Note well: vx, vy, and vz *are not
                  necessarily equal* to the time derivatives of x, y,
                  and z. This packet structure mimics that of the
                  Rosetta/MEX orbit file from which the data are taken.

                  Type 1 (indicated by code S18TP1):

                     x,  y,  z,  dx/dt,  dy/dt,  dz/dt

                  where x, y, z represent Cartesian position components
                  and  vx, vy, vz represent Cartesian velocity
                  components.

               Position units are kilometers, velocity units are
               kilometers per second, and acceleration units are
               kilometers per second per second.

   epochs      is an array of epochs corresponding to the members of the
               packets array. The epochs are specified as seconds past
               J2000, TDB.

-Detailed_Output

   None. See -Particulars for a description of the effect of this
   routine.

-Parameters

   MAXDEG      is the maximum allowed degree of the interpolating polynomial.
               Its current value is 15.

-Exceptions

   If any of the following exceptions occur, this routine will
   return without creating a new segment.

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

   5)  If the window size implied by `degree' is odd, the error
       SPICE(INVALIDDEGREE) is signaled by a routine in the call tree
       of this routine.

   6)  If the number of packets `n' is not at least 2, the error
       SPICE(TOOFEWSTATES) is signaled by a routine in the call tree
       of this routine.

   7)  If `first' is greater than or equal to `last', the error
       SPICE(BADDESCRTIMES) is signaled by a routine in the call tree
       of this routine.

   8)  If the elements of the array `epochs' are not in strictly
       increasing order, the error SPICE(TIMESOUTOFORDER) is
       signaled by a routine in the call tree of this routine.

   9)  If the first epoch epochs[0] is greater than `first', the error
       SPICE(BADDESCRTIMES) is signaled by a routine in the call tree
       of this routine.

   10) If the last epoch epochs[n-1] is less than `last', the error
       SPICE(BADDESCRTIMES) is signaled by a routine in the call tree
       of this routine.

   11) If the subtype code is not recognized, the error
       SPICE(INVALIDVALUE) is signaled by a routine in the call tree
       of this routine.

   12) If any of the `frame' or `segid' input string pointers is
       null, the error SPICE(NULLPOINTER) is signaled.

   13) If any of the `frame' or `segid' input strings has zero
       length, the error SPICE(EMPTYSTRING) is signaled.

-Files

   A new type 18 SPK segment is written to the SPK file attached
   to `handle'.

-Particulars

   This routine writes an SPK type 18 data segment to the open SPK
   file according to the format described in the type 18 section of
   the SPK Required Reading. The SPK file must have been opened with
   write access.

-Examples

   Suppose that you have states and are prepared to produce
   a segment of type 18 in an SPK file.

   The following code fragment could be used to add the new segment
   to a previously opened SPK file attached to `handle'. The file must
   have been opened with write access.

       #include "SpiceUsr.h"
            .
            .
            .

       /.
       Create a segment identifier.
       ./
       #define  SEGID  "MY_SAMPLE_SPK_TYPE_18_SEGMENT"


       /.
       Write the segment.
       ./
       spkw18_c ( handle,  subtyp,  body,    center,
                  frame,   first,   last,    segid,
                  degree,  n,       states,  epochs );

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.0.2, 13-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard.

       Changed, in the documentation, the argument name "state" to
       "packts". Fixed exception entry #6, since the number of packets must
       be at least 2 and not 1. Added exception entry #11. Updated
       -Parameters section to remove reference to SPICELIB routines and
       add the actual value of the MAXDEG parameter.

   -CSPICE Version 1.0.1, 29-APR-2003 (NJB)

       Description of error condition arising from invalid window
       size was corrected.

   -CSPICE Version 1.0.0, 16-AUG-2002 (NJB)

-Index_Entries

   write SPK type_18 ephemeris data segment

-&
*/

{ /* Begin spkw18_c */


   /*
   Local variables
   */
   SpiceInt                locSubtype;



   /*
   Participate in error tracing.
   */
   if ( return_c() )
   {
      return;
   }
   chkin_c ( "spkw18_c" );

   /*
   Check the input strings to make sure the pointers
   are non-null and the string lengths are non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "spkw18_c", frame );
   CHKFSTR ( CHK_STANDARD, "spkw18_c", segid );


   locSubtype = (SpiceInt) subtyp;

   /*
   Write the segment.
   */
   spkw18_ ( ( integer    * ) &handle,
             ( integer    * ) &locSubtype,
             ( integer    * ) &body,
             ( integer    * ) &center,
             ( char       * ) frame,
             ( doublereal * ) &first,
             ( doublereal * ) &last,
             ( char       * ) segid,
             ( integer    * ) &degree,
             ( integer    * ) &n,
             ( doublereal * ) packts,
             ( doublereal * ) epochs,
             ( ftnlen       ) strlen(frame),
             ( ftnlen       ) strlen(segid)  );


   chkout_c ( "spkw18_c" );


} /* End spkw18_c */
