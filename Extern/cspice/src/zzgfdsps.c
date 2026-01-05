/*

-Procedure zzgfdsps_  ( GF, display string  )

-Abstract
 
   SPICE Private routine intended solely for the support of SPICE 
   routines.  Users should not call this routine directly due 
   to the volatile nature of this routine. 
 
   Display a character string at a position at the first column on 
   the previous line on the screen. 
 
   This is an overlay routine for the f2c'd routine zzgfdsps_.

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
 
   GF
 
-Keywords
 
   STRING 
   DISPLAY 
   CURSOR 
   POSITION 
 
*/

   #include <stdio.h>
   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"

   int zzgfdsps_ ( integer  * nlead,
                   char     * string,
                   char     * fmt,
                   integer  * ntrail,
                   ftnlen     stringLen,
                   ftnlen     fmtLen     ) 

/*

-Brief_I/O
 
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   nlead      I   Number of leading blank lines to write. 
   string     I   The string to display. 
   fmt        I   Format in which the string is to be written. 
   ntrail     I   Number of trailing blank lines to write. 
   stringLen  I   Length of input argument `string'.
   fmtLen     I   Length of input argument `fmt'.
 
-Detailed_Input
 
   nlead          is the number of blank lines to write before 
                  writing the output text string. 
 
   string         is a message to be displayed on the standard 
                  output stream. This is a Fortran-style string
                  without a terminating null character.
 
   fmt            is a Fortran format specification used to write 
                  the output string. This is a Fortran-style string
                  without a terminating null character.
 
                  FMT may be left to default ("A"), or may be used 
                  to control the length of the string ("A10"). 

                  **NOTE**: this argument is provided only for
                  compatibility with the Fortran version of this
                  routine; the argument is currently ignored.
 
   ntrail         is the number of blank lines to write after 
                  writing the output text string. 

   stringLen      is the length of the input string `string'.

   fmtLen         is the length of the input string `fmt'.
 
-Detailed_Output
 
   None. This program has no output arguments but writes to the 
   standard output stream. 
 
-Parameters
 
   None. 
 
-Exceptions
 
   1) If an error occurs when this routine attempts to 
      allocate memory dynamically, the error will be
      diagnosed by routines in the call tree of this routine.
 
   2) If the either of the input arguments `nlead' or `ntrail' 
      is non-positive, then no leading or trailing blank 
      lines will be written, respectively. This case is not 
      considered an error. 
 
-Files
 
   None. 
 
-Particulars
 
   This is an overlay routine for the f2c'd routine zzgfdsps_;
   as such, this routine has an f2c-style calling sequence.

   CSPICE GF routines should call this routine rather than
   zzgfdsps_.

   Since ANSI C supports the cursor control capabilities required
   for GF progress reporting, it's not necessary to rely on ANSI
   control sequences to effect cursor control.

   This routine supports the default GF progress report display. 
   Output is written to the standard output stream; normally this 
   results in output on a terminal window. 
 
   After the output line is written, this routine moves the cursor 
   up and to the first column, so a subsequent call will overwrite 
   output from the current call. 
 
-Examples 
 
   See calls made to this routine by the entry points of 
   zzgfrpwrk. 
 
-Restrictions
 
   The input Fortran format argument is ignored.
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman     (JPL) 
 
-Version
 
   -CSPICE Version 1.0.0, 27-FEB-2009 (NJB)

-Index_Entries
 
   GF output progress report string 
 
-&
*/

{ /* Begin zzgfdsps_ */


   /*
   Local variables
   */      
   SpiceChar             * CFmtPtr;
   SpiceChar             * CStringPtr;

   SpiceInt                i;
   SpiceInt                nl;
   SpiceInt                nt;
   SpiceInt                outlen;
 

   /*
   Participate in error tracing.
   */
   chkin_c ( "zzgfdsps_" );

   /*
   The input strings are Fortran-style; they're not 
   null-terminated. Convert these to C-style strings
   so we can work with them. We'll need to use dynamic
   memory to hold the C-style strings. 
   */
   F2C_CreateStr_Sig ( stringLen, string, &CStringPtr );
   
   if ( failed_c() ) 
   {
      /*
      The CSPICE string utilities do their own clean-up of
      allocated memory, so we won't attempt to free the
      C string. 
      */
      chkout_c ( "zzgfdsps_" );

      return (-1);
   }

   F2C_CreateStr_Sig ( fmtLen, fmt, &CFmtPtr );

   if ( failed_c() ) 
   {
      /*
      Failure at this point requires that we free the previous,
      successfully allocated string. 
      */
      free ( CStringPtr );

      chkout_c ( "zzgfdsps_" );

      return(-1);
   }

   /*
   Display any blank lines indicated by `nlead'. 
   */

   nl = *nlead;
   nt = *ntrail;

 
   for ( i = 0;  i < nl; i++ )
   {
      putc ( '\n', stdout );
   }

   /*
   Save the length of the output string. 
   */
   outlen = strlen( CStringPtr );

   /*
   Write the string to standard output without a trailing newline
   character. 
   */   
   printf ( "%s", CStringPtr );  


   /*
   Force a write of any buffered, unwritten output data. 

   Without this call, progress report updates may not be displayed in a
   timely fashion. There can be a long pause, followed by an
   announcement that the task is 100% done. This behavior rather
   defeats the purpose of the report.
   */
   fflush ( stdout );

   /*
   Back up the cursor to the start of the line. 
   */
   for ( i = 0; i < outlen; i++ )
   {
      putc ( '\b', stdout );
   }

   /*
   Display any blank lines indicated by `ntrail'. 
   */
   for ( i = 0;  i < nt; i++ )
   {
      putc ( '\n', stdout );
   }

   /*
   Free the dynamically allocated strings. 
   */
   free ( CStringPtr );
   free ( CFmtPtr    );
   
   chkout_c ( "zzgfdsps_" );

   return ( 0 );


} /* End zzgfdsps_ */
