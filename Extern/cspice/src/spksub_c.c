/*

-Procedure spksub_c ( S/P Kernel, subset )

-Abstract

   Extract a subset of the data in an SPK segment into a
   separate segment.

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
   DAF

-Keywords

   EPHEMERIS

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"

   void spksub_c ( SpiceInt            handle,
                   SpiceDouble         descr[5],
                   ConstSpiceChar    * ident,
                   SpiceDouble         begin,
                   SpiceDouble         end,
                   SpiceInt            newh    )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   handle     I   Handle of source segment.
   descr      I   Descriptor of source segment.
   ident      I   Identifier of source segment.
   begin      I   Beginning (initial epoch) of subset.
   end        I   End (final epoch) of subset.
   newh       I   Handle of new segment.

-Detailed_Input

   handle,
   descr,
   ident       are the file handle assigned to an SPK file, the
               descriptor for a segment within the file, and the
               identifier for that segment. Together they determine a
               complete set of ephemeris data, from which a subset is to
               be extracted.

   begin,
   end         are the initial and final epochs (ephemeris time) of the
               subset.

   newh        is the file handle assigned to the file in which the new
               segment is to be written. The file must be open for write
               access. `newh' and `handle' may refer to the same file.

-Detailed_Output

   See -Files section.

-Parameters

   None.

-Exceptions

   1)  If the condition

          alpha  <=  begin  <=  end  <=  omega

       is not satisfied (where `alpha' and `omega' are the initial and
       final epochs of the segment respectively), the error
       SPICE(SPKNOTASUBSET) is signaled by a routine in the call tree
       of this routine.

   2)  If the segment type is not supported by the current version of
       spksub_c, the error SPICE(SPKTYPENOTSUPP) is signaled by a
       routine in the call tree of this routine.

   3)  If the `ident' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   4)  If the `ident' input string has zero length, the error
       SPICE(EMPTYSTRING) is signaled.

-Files

   A new segment, which contains a subset of the data in the
   segment specified by `descr' and `handle', is written to the SPK
   file attached to `newh'.

-Particulars

   Sometimes, the segments in official source files---planetary
   Developmental Ephemeris (DE) files, archival spacecraft
   ephemeris files, and so on---contain more data than is needed
   by a particular user. spksub_c allows a user to extract from a
   segment the smallest amount of ephemeris data sufficient to
   cover a specific interval.

   The new segment is written with the same identifier as the
   original segment, and with the same descriptor, with the
   following components changed:

   1)  `alpha' and `omega' (dc[0] and dc[1]) are assigned the values
       specified by `begin' and `end'.

   2)  The beginning and ending segment addresses (ic[4] and ic[5])
       are changed to reflect the location of the new segment.

-Examples

   In the following code fragment, the descriptor for each segment
   in a source SPK file is examined. For each segment that covers
   a target interval, the smallest possible subset is extracted into
   a custom SPK file.

   Assume that the source and custom files have been opened, for
   read and write access, with handles srchan and custhan respectively.

      #include "SpiceUsr.h"
           .
           .
           .
      dafbfs_c ( srchan );
      daffna_c ( &found );

      while ( found )
      {
         dafgs_c ( descr );
         dafus_c ( descr, 2, 6, dc, ic );

         if (  ( dc[0] <= begin ) && ( end <= dc[1] )  )
         {
            dafgn_c  ( ident );
            spksub_c ( srchan, descr, ident, begin, end, custhan );
         }

         daffna_c ( &found );
      }

-Restrictions

   1)  There is no way for spksub_c to verify that the descriptor and
       identifier are the original ones for the segment. Changing
       the descriptor can cause the data in the new segment to be
       evaluated incorrectly; changing the identifier can destroy
       the path from the data back to its original source.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   K.R. Gehringer      (JPL)
   J.M. Lynch          (JPL)
   W.L. Taber          (JPL)
   R.E. Thurman        (JPL)
   I.M. Underwood      (JPL)

-Version

   -CSPICE Version 1.0.1, 05-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard. Moved SPK required
       reading from -Literature_References to -Required_Reading section.

   -CSPICE Version 1.0.0, 29-JUN-1999 (KRG) (WLT) (NJB) (JML) (RET) (IMU)

-Index_Entries

   subset of SPK file

-&
*/

{ /* Begin spksub_c */



   /*
   Participate in error tracing.
   */
   chkin_c ( "spksub_c" );

   /*
   Check the input strings to make sure the pointers
   are non-null and the string lengths are non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "spksub_c", ident );


   spksub_ (  ( integer    * ) &handle,
              ( doublereal * ) descr,
              ( char       * ) ident,
              ( doublereal * ) &begin,
              ( doublereal * ) &end,
              ( integer    * ) &newh,
              ( ftnlen       ) strlen(ident) );


   chkout_c ( "spksub_c" );

} /* End spksub_c */
