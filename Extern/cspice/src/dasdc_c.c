/*

-Procedure dasdc_c    ( DAS, delete comments )

-Abstract

   Delete the entire comment area of a previously opened binary
   DAS file.

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

   DAS

-Keywords

   None.

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #undef dasdc_c


   void dasdc_c ( SpiceInt handle )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   handle     I   The handle of a binary DAS file opened for writing.

-Detailed_Input

   handle      The handle of a binary DAS file that is to have its
               entire comment area deleted. The DAS file should have
               been opened with write access.

-Detailed_Output

   None.

-Parameters

   None.

-Exceptions

   1)  If the binary DAS file attached to `handle' is not open with
       write access, an error is signaled by a routine in the call
       tree of this routine.

-Files

   See argument `handle' in -Detailed_Input.

-Particulars

   Binary DAS files contain an area which is reserved for storing
   annotations or descriptive textual information about the data
   contained in a file. This area is referred to as the ``comment
   area'' of the file. The comment area of a DAS file is a line
   oriented medium for storing textual information. The comment area
   preserves any leading or embedded white space in the line(s) of text
   which are stored, so that the appearance of the information will be
   unchanged when it is retrieved (extracted) at some other time.
   Trailing blanks, however, are NOT preserved, due to the way that
   character strings are represented in standard Fortran 77.

   This routine will delete the entire comment area from the binary
   DAS file attached to `handle'. The size of the binary DAS file will
   remain unchanged. The space that was used by the comment records
   is reclaimed.

-Examples

   Let

         `handle' be the handle for a DAS file which has been opened
         with write access.

   The call

         dasdc_c ( handle )

   will delete the entire comment area of the binary DAS file
   attached to `handle'.

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   K.R. Gehringer      (JPL)

-Version

   -CSPICE Version 1.0.1, 10-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard.

   -CSPICE Version 1.0.0, 11-NOV-2016 (NJB) (KRG)

-Index_Entries

   delete DAS comment area

-&
*/

{ /* Begin dasdc_c */


   /*
   Participate in error tracing.
   */
   chkin_c ( "dasdc_c" );


   dasdc_ ( (integer *) &handle );


   chkout_c ( "dasdc_c" );

} /* End dasdc_c */
