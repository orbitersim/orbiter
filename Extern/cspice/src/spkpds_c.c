/*

-Procedure spkpds_c ( SPK pack descriptor )

-Abstract

   Perform routine error checks and if all check pass, pack the
   descriptor for an SPK segment

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


   void spkpds_c ( SpiceInt           body,
                   SpiceInt           center,
                   ConstSpiceChar   * frame,
                   SpiceInt           type,
                   SpiceDouble        first,
                   SpiceDouble        last,
                   SpiceDouble        descr[5] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   body       I   The NAIF ID code for the body of the segment.
   center     I   The center of motion for body.
   frame      I   The frame for this segment.
   type       I   The type of SPK segment to create.
   first      I   The first epoch for which the segment is valid.
   last       I   The last  epoch for which the segment is valid.
   descr      O   An SPK segment descriptor.

-Detailed_Input

   body        is the NAIF ID code for the body of the segment.

   center      is the center of motion for BODY.

   frame       is a string that names the frame to which states for
               the body shall be referenced.

   type        is the type of SPK segment to create.

   first       is the first epoch for which the segment will have
               ephemeris data.

   last        is the last epoch for which the segment will have
               ephemeris data.

-Detailed_Output

   descr       is a valid SPK segment descriptor to use
               when creating a DAF segment for this body.

-Parameters

   None.

-Exceptions

   1)  If the value of `body' is 0, the error SPICE(BARYCENTEREPHEM) is
       signaled by a routine in the call tree of this routine.

   2)  If the values of `body' and `center' are the same, the error
       SPICE(BODYANDCENTERSAME) is signaled by a routine in the call
       tree of this routine.

   3)  If `frame' is not one of the known SPICE reference frames, the
       error SPICE(INVALIDREFFRAME) is signaled by a routine in the
       call tree of this routine.

   4)  If `first' is greater than or equal to `last', the error
       SPICE(BADDESCRTIMES) is signaled by a routine in the call tree
       of this routine.

   5)  If the value of `type' is outside the range 1 to 1000
       (inclusive), the error SPICE(UNKNOWNSPKTYPE) is signaled by a
       routine in the call tree of this routine. This does not ensure
       that the `type' is a legitimate SPK segment type, but it is a
       simple check that helps avoid problems that arise from
       uninitialized values or improperly ordered calling arguments.

   6)  If the `frame' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   7)  If the `frame' input string has zero length, the error
       SPICE(EMPTYSTRING) is signaled.

-Files

   None.

-Particulars

   This is a utility routine for validating and creating
   the descriptor for an SPK segment. It is intended for
   use only by routines that create SPK segments.

-Examples

   Suppose that you wish to create an SPK segment of type X
   and that you are writing a routine to handle the details
   of the segment creation. This routine can be used to
   ensure that the descriptor needed for the segment is
   properly formed and that the information in that descriptor
   is reasonable.

   Having collected the needed information you can create the
   descriptor and then begin a new segment as shown below.

      #include "SpiceUsr.h"
           .
           .
           .
      spkpds_c ( body,   center, frame, type, first, last, descr );
      dafbna_c ( handle, descr,  segid );

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   K.R. Gehringer      (JPL)
   W.L. Taber          (JPL)

-Version

   -CSPICE Version 1.0.1, 05-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard.

   -CSPICE Version 1.0.0, 19-JUL-1999 (NJB) (KRG) (WLT)

-Index_Entries

   Validate and pack an SPK segment descriptor

-&
*/

{ /* Begin spkpds_c */

   /*
   Participate in error tracing.
   */
   chkin_c ( "spkpds_c" );

   /*
   Check the input frame string to make sure the pointer
   is non-null and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "spkpds_c", frame );

   /*
   Call the f2c'd routine.
   */
   spkpds_ (  ( integer     * ) &body,
              ( integer     * ) &center,
              ( char        * ) frame,
              ( integer     * ) &type,
              ( doublereal  * ) &first,
              ( doublereal  * ) &last,
              ( doublereal  * ) descr,
              ( ftnlen        ) strlen(frame)  );


   chkout_c ( "spkpds_c" );

} /* End spkpds_c */
