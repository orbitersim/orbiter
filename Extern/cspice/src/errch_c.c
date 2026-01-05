/*

-Procedure errch_c  ( Insert String into Error Message Text )

-Abstract

   Substitute a character string for the first occurrence of
   a marker in the current long error message.

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

   ERROR

-Keywords

   CONVERSION
   ERROR

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"


   void errch_c ( ConstSpiceChar * marker,
                  ConstSpiceChar * string )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  ---------------------------------------------------
   marker     I   A substring of the error message to be replaced.
   string     I   The character string to substitute for marker.

-Detailed_Input

   marker      is a character string that marks a position in
               the long error message where a character string
               is to be substituted. Leading and trailing blanks
               in marker are not significant.

               Case IS significant:  "XX" is considered to be
               a different marker from "xx".

   string      is a character string that will be substituted for
               the first occurrence of marker in the long error
               message. This occurrence of the substring indicated
               by marker will be removed and replaced by string.
               Leading and trailing blanks in string are not
               significant. However, if string is completely blank,
               a single blank character will be substituted for
               the marker.

-Detailed_Output

   None.

-Parameters

   LMSGLN  is the maximum length of the long error message. See
           the include file errhnd.inc for the value of LMSGLN.

-Exceptions

   1)  If the character string resulting from the substitution
       exceeds the maximum length of the long error message, the
       long error message is truncated on the right. No error is
       signaled.

   2)  If `marker' is blank, no substitution is performed. No error
       is signaled.

   3)  If `string' is blank, then the first occurrence of `marker'
       is replaced by a single blank.

   4)  If `marker' does not appear in the long error message, no
       substitution is performed. No error is signaled.

   5)  If changes to the long error message are disabled, this
       routine has no effect.

   6)  If any of the `marker' or `string' input string pointers is
       null, the error SPICE(NULLPOINTER) is signaled.

   7)  If any of the `marker' or `string' input strings has zero
       length, the error SPICE(EMPTYSTRING) is signaled.

-Files

   None.

-Particulars

   The purpose of this routine is to allow you to tailor the long
   error message to include specific information that is available
   only at run time. This capability is somewhat like being able to
   put variables in your error messages.

-Examples

   1)   In this example, the marker is  "#".  We'll signal a file
        open error, and we'll include in the error message the name
        of the file we tried to open. There are three steps:

           -- Set the long message, using a marker for the location
              where a value is to be substituted.

           -- Substitute the file name into the error message.

           -- Signal the error (causing output of error messages)
              using the CSPICE routine sigerr_c.

           /.
           Error on file open attempt. Signal an error.
           The character string variable FILE contains the
           file name.

           After the call to errch_c, the long error message
           will contain the file name held in the string
           FILE. For example, if FILE contains the name
           "MYFILE.DAT", the long error message will be

               "File open error. File is MYFILE.DAT."

           ./

           setmsg_c ( "File open error. File is #." );
           errch_c  ( "#",  FILE                     );
           sigerr_c ( SPICE(FILEOPENFAILED)        );


   2)   Same example as (1), except this time we'll use a better-
        looking and more descriptive marker than "#".  Instead,
        we'll use the marker "FILENAME".  This does not affect the
        long error message; it just makes the code more readable.

           /.
           Error on file open attempt. Signal an error.
           The character string variable FILE contains the
           file name.
           ./

           setmsg_c ( "File open error. File is FILENAME.");
           errch_c  ( "FILENAME",  FILE                   );
           sigerr_c ( SPICE(FILEOPENFAILED)             );


   3)   Same example as (2), except this time there's a problem with
        the variable FILE: it's blank. This time, the code fragment

           /.
           Error on file open attempt. Signal an error.
           The character string variable FILE contains the
           file name.
           ./
           setmsg_c ( "File open error. File is FILENAME." );
           errch_c  ( "FILENAME",  FILE                    );

        sets the long error message to

           "File open error. File is  "

-Restrictions

   1)  The caller must ensure that the message length, after sub-
       stitution is performed, doesn't exceed LMSGLN characters.
       See errch.c.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.2.1, 02-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard.

   -CSPICE Version 1.2.0, 08-FEB-1998 (NJB)

       Re-implemented routine without dynamically allocated, temporary
       strings. Made various header fixes.

   -CSPICE Version 1.0.0, 25-OCT-1997 (EDW)

-Index_Entries

   insert string into error message text

-&
*/

{ /* Begin errch_c */


   /*
   Check the input strings to make sure the pointers are non-null
   and the string lengths are non-zero.  Since we don't check in
   prior to this, use the discovery check-in option.
   */
   CHKFSTR ( CHK_DISCOVER, "errch_c", marker );
   CHKFSTR ( CHK_DISCOVER, "errch_c", string );


   /*
   Call the f2c'd Fortran routine.
   */
   errch_ ( ( char * ) marker,
            ( char * ) string,
            ( ftnlen ) strlen(marker),
            ( ftnlen ) strlen(string) );


} /* End errch_c */
