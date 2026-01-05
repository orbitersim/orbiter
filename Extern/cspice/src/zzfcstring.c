/*

-Procedure zzfcstring ( Fortran/C string conversion utilities )

-Abstract

   CSPICE Fortran/C string conversion utility package.  Contains
   multiple functions.

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

   None.

-Keywords

   PRIVATE
   STRING
   UTILITY

-Brief_I/O

   See functions below.

-Detailed_Input

   See functions below.

-Detailed_Output

   See functions below.

-Parameters

   See functions below.

-Exceptions

   See functions below.

-Files

   None.

-Particulars

   Contains the following functions:

       C_OptEmptyStr        ( Return pointer to non-empty input string or
                              blank string if input string is empty )

       C2F_CreateStr        ( Create a Fortran string from C string )

       C2F_CreateStr_Sig    ( Create a Fortran string from C string,
                              error signaling version              )

       C2F_MapStrArr        ( Create a Fortran string array from 2-d
                              C string array                          )

       C2F_MapFixStrArr     ( Create a Fortran string array from 2-d
                              C string array, string length fixed by
                              caller                                  )

       C2F_CreateStrArr     ( Create a Fortran string array from array
                              of C strings                            )

       C2F_CreateStrArr_Sig ( Create a Fortran string array from array
                              of C strings, error signaling version   )

       C2F_CreateFixStrArr ( Create a Fortran string array from C string
                             array, string length fixed by caller      )

       C2F_StrCpy           ( Copy a C string into a Fortran string )

       F_Alloc              ( Allocate a string for Fortran output )

       F2C_ConvertStr       ( Convert a Fortran string to a C string )

       F2C_ConvertStrArr    ( Convert a Fortran string to an array of
                              C strings                              )

       F2C_CreateStr        ( Create a C string from a Fortran string )

       F2C_CreateStr_Sig    ( Create a C string from a Fortran string,
                              error signaling version                )

       F2C_CreateStrArr     ( Create an array of C strings from an
                              array of Fortran strings             )

       F2C_CreateTrStrArr   ( Create an array of trimmed C strings from
                              an array of Fortran strings              )

       F2C_StrCpy           ( Copy a Fortran string into a C string )

       F_StrLen             ( Find the number of characters, excluding
                              trailing blanks, in a Fortran string     )

-Examples

   None.

-Restrictions

   See functions below.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   K.R. Gehringer      (JPL)
   J. Diaz del Rio     (JPL)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 6.0.0, 07-OCT-2021 (NJB) (JDR)

       Added function C_OptEmptyStr. Made minimal updates to header.

       Updated short error message for consistency within CSPICE wrapper
       interface: CSPICE(MALLOCFAILURE) -> SPICE(MALLOCFAILED) in F_Alloc
       routine.

       Added initializers for local variables in function F_Strlen.
       This is not a bug fix; it was done to pacify Valgrind.

   -CSPICE Version 5.0.0, 10-JUL-2002 (NJB)

       Renamed file to zzfcstring.c.

       Added routines C2F_MapStrArr and C2F_MapFixStrArr. These are analogs
       of C2F_CreateStrArr_Sig and C2F_CreateFixStrArr that operate
       on a 2-dimensional character array containing null-terminated strings.

       Fixed an error message in C2F_CreateStrArr_Sig; the long
       error message for a malloc failure specified an incorrect
       number of bytes which the routine had attempted to allocate.

   -CSPICE Version 4.0.0, 14-FEB-2000 (NJB)

       Added routine C2F_CreateStrArr_Sig. This is an error-signaling
       version of C2F_CreateStrArr.

       Corrected various typos and formatting errors.

   -CSPICE Version 3.0.0, 09-JUL-1999 (NJB)

       Added routine F2C_ConvertTrStrArr.

   -CSPICE Version 2.0.1, 09-FEB-1998 (EDW) (NJB)

       Added routine F2C_ConvertStrArr. Modified argument list of
       F2C_ConvertStr to be consistent with the new routine.

   -CSPICE Version 2.0.0, 03-JAN-1997 (NJB)

       Added routine F2C_ConvertStr. Adjusted indentation of comment
       delimiters.

   -CSPICE Version 1.0.0, 25-OCT-1997 (NJB) (KRG) (EDW)

-Index_Entries

   None.
-&
*/

   #include <string.h>

   #include "SpiceUsr.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"


/*
Function C_OptEmptyStr returns a pointer to a non-empty string, given a
pointer to an optionally empty string.

The function returns its input string pointer if that pointer refers to a
non-empty string. If the input pointer refers to an empty string, the function
returns a pointer to a static string consisting of one blank followed by a
terminating null character.

This function supports CSPICE wrappers that accept input string arguments
that are allowed to be empty.

This routine performs no error handling. The input pointer must be non-null.
*/
ConstSpiceChar *   C_OptEmptyStr( ConstSpiceChar   * cStr )
{
   /*
   Local static constants
   */
   static ConstSpiceChar  * blankStr = " ";


   if ( strlen(cStr) > 0 )
   {
      return cStr;
   }
   else
   {
      return blankStr;
   }
}



SpiceStatus C2F_CreateStr ( ConstSpiceChar  *cStr,
                            SpiceInt        *fStrLen,
                            SpiceChar       **fStr    )

{
   SpiceInt    length;
   SpiceStatus  status;
   SpiceChar   *tempStr;

   /*
   Compute the length of the input C string.
   */
   length  = strlen ( cStr );

   /*
   Dynamically allocate sufficient memory to hold the string for
   use as a Fortran string. If the memory allocation fails, return
   a failure status.
   */
   tempStr = (SpiceChar *) malloc ( length );

   if ( tempStr == (SpiceChar *) NULL )
      {
      *fStr    = (SpiceChar *) NULL;
      *fStrLen = 0;
      return (SPICEFAILURE);
      } /* end if */

   /*
   Copy the C string into the Fortran string.
   */
   status = C2F_StrCpy ( cStr, length, tempStr );

   if ( status == SPICEFAILURE )
      {
      free ( tempStr );
      *fStr    = (SpiceChar *) NULL;
      *fStrLen = 0;
      return (SPICEFAILURE);
      } /* end if */

   /*
   Set the output values.
   */
   *fStr    = tempStr;
   *fStrLen = length;

   /*
   Return success status.
   */
   return (SPICESUCCESS);

} /* end C2F_CreateStr */





void      C2F_CreateStr_Sig ( ConstSpiceChar  * cStr,
                              SpiceInt        * fStrLen,
                              SpiceChar      ** fStr     )
   /*
   Error signaling version of C2F_CreateStr
   */
{
   SpiceStatus  status;

   status = C2F_CreateStr ( cStr, fStrLen, fStr );

   if ( status == SPICEFAILURE )
   {
      chkin_c  ( "C2F_CreateStr_Sig" );
      setmsg_c ( "An attempt to create a temporary string failed." );
      sigerr_c ( "SPICE(STRINGCREATEFAIL)" );
      chkout_c ( "C2F_CreateStr_Sig" );
      return;
   }

} /* end C2F_CreateStr_Sig */





SpiceStatus C2F_CreateStrArr ( SpiceInt           nStr,
                               ConstSpiceChar  ** cStrArr,
                               SpiceInt         * fStrLen,
                               SpiceChar       ** fStrArr )
{
   SpiceInt    i;
   SpiceInt    j;
   SpiceInt    maxLen;
   SpiceInt    tempLen;
   SpiceStatus  status;
   SpiceChar   *tempStrArr;

   /*
   Find the length of the longest C string in the input array.
   */
   maxLen = 0;
   for (i=0; i<nStr; i++)
      {
      tempLen = strlen ( *(cStrArr+i) );
      if ( tempLen > maxLen )
         {
         maxLen = tempLen;
         } /* end if */
      } /* end for */

   /*
   Allocate the memory for the Fortran string array. It must be
   maxLen characters wide and long enough to hold nStr Fortran strings.
   */
   tempStrArr = (SpiceChar *) malloc ( maxLen * nStr );

   if ( tempStrArr == (SpiceChar *)NULL )
      {
      *fStrArr = (SpiceChar *) NULL;
      *fStrLen = 0;
      return (SPICEFAILURE);
      } /* end if */

   /*
   Copy the C strings into the memory for the Fortran string array. The
   Copy function knows to leave the NULL character that terminates each
   line behind.
   */
   for ( i=0; i<nStr; i++)
      {
      j = i * maxLen;
      status = C2F_StrCpy ( *(cStrArr+i), maxLen, (tempStrArr+j) );

      if ( status == SPICEFAILURE )
         {
         free ( tempStrArr );
         *fStrArr = (SpiceChar *) NULL;
         *fStrLen = 0;
         return (SPICEFAILURE);
         } /* end if */
      } /* end for */

   /*
   Set the return values.
   */
   *fStrArr = tempStrArr;
   *fStrLen = maxLen;

   /*
   Return success status.
   */
   return (SPICESUCCESS);

} /* end C2F_CreateStrArr */




void C2F_CreateStrArr_Sig ( SpiceInt           nStr,
                            ConstSpiceChar  ** cStrArr,
                            SpiceInt         * fStrLen,
                            SpiceChar       ** fStrArr )
{
   /*
   Error signaling version of C2F_CreateStrArr
   */

   /*
   Local variables
   */
   SpiceInt     i;
   SpiceInt     maxLen;

   SpiceStatus  status;


   /*
   Create the Fortran string array using the non-signaling routine.
   */
   status = C2F_CreateStrArr ( nStr, cStrArr, fStrLen, fStrArr );

   if ( status == SPICEFAILURE )
   {
      /*
      Find the length of the longest C string in the input array.
      */
      maxLen = 0;

      for ( i = 0;  i < nStr;  i++ )
      {
         maxLen =  MaxVal (  strlen( *(cStrArr+i) ),  maxLen );
      }

      chkin_c  ( "C2F_CreateStrArr_Sig"                           );
      setmsg_c ( "An attempt to create a temporary string array "
                 "failed.  Attempted to allocate # bytes."        );
      errint_c ( "#",  maxLen * nStr                              );
      sigerr_c ( "SPICE(STRINGCREATEFAIL)"                        );
      chkout_c ( "C2F_CreateStrArr_Sig"                           );
      return;
   }
}





/*
   C2F_MapStrArr is an analog of C2F_CreateStrArr_Sig; instead of
   taking an array of string pointers as an input, this routine takes a
   void pointer that refers to a character array of specified string
   length and cardinality.   The rows of the array must contain
   null-terminated strings.

   This turns out to be the form of string array most commonly input to
   CSPICE wrappers.

   NOTE:  This routine does something a bit unconventional regarding
   checking in and out:  it supports "delegated check-in".  The caller
   passes in its name, and if an error is signaled, this routine
   performs a discovery style check-in on the caller's behalf before
   checking in itself.  After checking out, this routine checks out
   for the caller.

   This oddity allows the caller to do discovery check-in in the event
   that the caller calls no other routines that signal errors.
*/


void C2F_MapStrArr ( ConstSpiceChar   * caller,
                     SpiceInt           nStr,
                     SpiceInt           cStrLen,
                     const void       * cStrArr,
                     SpiceInt         * fStrLen,
                     SpiceChar       ** fStrArr  )
{
   /*
   Local variables
   */

   SpiceChar             * tempStrArr;

   SpiceInt                i;
   SpiceInt                j;
   SpiceInt                k;
   SpiceInt                maxLen;
   SpiceInt                tempLen;

   SpiceStatus             status;

   /*
   Find the length of the longest C string in the input array.
   */
   maxLen = 0;

   for ( i = 0;  i < nStr;  i++ )
   {
      tempLen = strlen (  ( (SpiceChar *)cStrArr ) +  i * cStrLen  );

      maxLen  = maxi_c ( 2, tempLen, maxLen );
   }

   /*
   Allocate the memory for the Fortran string array. It must be
   maxLen characters wide and long enough to hold nStr Fortran strings.
   */
   tempStrArr = (SpiceChar *) malloc ( maxLen * nStr );

   if ( tempStrArr == NULLCPTR )
   {
      *fStrArr = (SpiceChar *) NULL;
      *fStrLen = 0;

      /*
      Check in for the caller, then perform normal discovery check-in.
      */
      chkin_c  ( caller );

      chkin_c  ( "C2F_MapStrArr"                                  );
      setmsg_c ( "An attempt to create a temporary string array "
                 "failed.  Attempted to allocate # bytes."        );
      errint_c ( "#",  maxLen * nStr                              );
      sigerr_c ( "SPICE(STRINGCREATEFAIL)"                        );
      chkout_c ( "C2F_MapStrArr"                                  );

      /*
      Check out for the caller.
      */
      chkout_c ( caller );
      return;
   }


   /*
   Copy the C strings into the memory for the Fortran string array. The
   copy function knows to leave the NULL character that terminates each
   line behind.
   */
   for ( i = 0;  i < nStr;  i++ )
   {
      j      = i * cStrLen;
      k      = i * maxLen;

      status = C2F_StrCpy (  ( (SpiceChar *)cStrArr ) +  j,
                             maxLen,
                             tempStrArr + k                 );

      if ( status == SPICEFAILURE )
      {
         free ( tempStrArr );

         *fStrArr = (SpiceChar *) NULL;
         *fStrLen = 0;

         chkin_c  ( caller );

         chkin_c  ( "C2F_MapStrArr"                                  );
         setmsg_c ( "An attempt to copy a C string to a temporary "
                    "string of length # failed.  This may be due "
                    "to an unterminated input string."               );
         errint_c ( "#",  maxLen                                     );
         sigerr_c ( "SPICE(STRINGCOPYFAIL)"                          );
         chkout_c ( "C2F_MapStrArr"                                  );

         chkout_c ( caller );
         return;
      }
   }

   /*
   Set the return values.
   */
   *fStrArr = tempStrArr;
   *fStrLen = maxLen;


} /* end C2F_MapStrArr */



/*
   C2F_MapFixStrArr is just like C2F_MapStrArr, except that the string
   length of the output array is fixed by the caller:  it is always
   cStrLen-1.  For convenience, the output string length is returned
   anyway.

   Like C2F_MapStrArr, this routine supports delegated check-in.
*/




void C2F_MapFixStrArr ( ConstSpiceChar   * caller,
                        SpiceInt           nStr,
                        SpiceInt           cStrLen,
                        const void       * cStrArr,
                        SpiceInt         * fStrLen,
                        SpiceChar       ** fStrArr  )
{
   /*
   Local variables
   */

   SpiceChar             * tempStrArr;

   SpiceInt                i;
   SpiceInt                j;
   SpiceInt                k;
   SpiceInt                fLen;

   SpiceStatus             status;

   /*
   Set the Fortran string length.
   */
   fLen = cStrLen - 1;


   /*
   Allocate the memory for the Fortran string array. It must be
   maxLen characters wide and long enough to hold nStr Fortran strings.
   */
   tempStrArr = (SpiceChar *) malloc ( fLen * nStr );

   if ( tempStrArr == NULLCPTR )
   {
      *fStrArr = (SpiceChar *) NULL;
      *fStrLen = 0;

      chkin_c  ( caller );

      chkin_c  ( "C2F_MapFixStrArr"                                  );
      setmsg_c ( "An attempt to create a temporary string array "
                 "failed.  Attempted to allocate # bytes."           );
      errint_c ( "#",  fLen * nStr                                   );
      sigerr_c ( "SPICE(STRINGCREATEFAIL)"                           );
      chkout_c ( "C2F_MapFixStrArr"                                  );

      chkout_c ( caller );
      return;
   }


   /*
   Copy the C strings into the memory for the Fortran string array. The
   copy function knows to leave the NULL character that terminates each
   line behind.
   */
   for ( i = 0;  i < nStr;  i++ )
   {
      j      = i * cStrLen;
      k      = i * fLen;

      status = C2F_StrCpy (  ( (SpiceChar *)cStrArr ) +  j,
                             fLen,
                             tempStrArr + k                 );

      if ( status == SPICEFAILURE )
      {
         free ( tempStrArr );

         *fStrArr = (SpiceChar *) NULL;
         *fStrLen = 0;

         chkin_c  ( caller );

         chkin_c  ( "C2F_MapFixStrArr"                                  );
         setmsg_c ( "An attempt to copy a C string to a temporary "
                    "string of length # failed.  This may be due "
                    "to an unterminated input string."                  );
         errint_c ( "#",  fLen                                          );
         sigerr_c ( "SPICE(STRINGCOPYFAIL)"                             );
         chkout_c ( "C2F_MapFixStrArr"                                  );

         chkout_c ( caller );
         return;
      }
   }

   /*
   Set the return values.
   */
   *fStrArr = tempStrArr;
   *fStrLen = fLen;


} /* end C2F_MapFixStrArr */







void C2F_CreateFixStrArr ( SpiceInt           nStr,
                           SpiceInt           cStrDim,
                           ConstSpiceChar  ** cStrArr,
                           SpiceInt         * fStrLen,
                           SpiceChar       ** fStrArr  )
{
   SpiceInt      i;
   SpiceInt      j;
   SpiceInt      fLen;

   SpiceStatus   status;

   SpiceChar   * tempStrArr;

   /*
   The input argument cStrDim is the declared string length of the input
   C string array.
   */

   fLen = cStrDim - 1;

   /*
   Allocate the memory for the Fortran string array. It must be
   cStrDim characters wide and long enough to hold nStr Fortran strings.
   */
   tempStrArr = (SpiceChar *) malloc ( fLen * nStr );

   if ( tempStrArr == (SpiceChar *)NULL )
   {
      *fStrArr = (SpiceChar *) NULL;
      fLen     = 0;

      chkin_c  ( "C2F_CreateFixStrArr"                             );
      setmsg_c ( "An attempt to create a temporary string array "
                 "failed.  Attempted to allocate # bytes."         );
      errint_c ( "#", fLen * nStr                                  );
      sigerr_c ( "SPICE(STRINGCREATEFAIL)"                         );
      chkout_c ( "C2F_CreateFixStrArr"                             );
      return;
   }

   /*
   Copy the C strings into the memory for the Fortran string array. The
   Copy function knows to leave the NULL character that terminates each
   line behind.
   */
   for ( i=0;  i < nStr;  i++ )
   {
      j      = i * fLen;
      status = C2F_StrCpy ( *(cStrArr+i), fLen, (tempStrArr+j) );

      if ( status == SPICEFAILURE )
      {
         free ( tempStrArr );

         *fStrArr = (SpiceChar *) NULL;
         fLen     = 0;

         chkin_c  ( "C2F_CreateFixStrArr"                           );
         setmsg_c ( "An attempt to copy a string using C2F_StrCpy "
                    "failed."                                       );
         sigerr_c ( "SPICE(STRINGCOPYFAIL)"                         );
         chkout_c ( "C2F_CreateFixStrArr"                           );
         return;
      }
   }

   /*
   Set the output values.
   */

   *fStrArr = tempStrArr;
   *fStrLen = fLen;


} /* end C2F_CreateFixStrArr */






SpiceStatus C2F_StrCpy ( ConstSpiceChar  *cStr,
                         SpiceInt        fStrLen,
                         SpiceChar       *fStr   )

{
   SpiceInt i;
   SpiceInt nChars;

   /*
   Find the number of characters, excluding the NULL in Cstring.
   */
   nChars = strlen( cStr );

   /*
   Check to see if there is enough room in the Fortran string to hold
   all of the characters in the C string except for the NULL character.
   If not, return a failure status.
   */
   if ( nChars > fStrLen )
      {
      return (SPICEFAILURE);
      } /* end if */

   /*
   Blank fill the Fortran string. This must always be done, even if
   the number of characters is zero.
   */
   for ( i=0; i<fStrLen; i++ )
      {
      *(fStr+i) = ' ';
      } /* end for */

   /*
   Move the C string into the Fortran string, leaving the trailing
   NULL behind, if there are any characters to move.
   */
   if ( nChars > 0 )
      {
      strncpy ( fStr, cStr, nChars );
      } /* end if */

   /*
   Return success status.
   */
   return (SPICESUCCESS);

} /* end C2F_StrCpy */



void      F_Alloc ( SpiceInt         fStrLen,
                    SpiceChar     ** fStr     )
{
   /*
   Local variables
   */
   SpiceInt                i;

   /*
   Allocate a temporary string of the specified length.  The string
   is blank filled for safety, since it'll normally be passed to a
   Fortran routine.
   */

   *fStr = (SpiceChar *) malloc ( fStrLen );

   if ( *fStr == (SpiceChar *)NULL )
   {
      chkin_c  ( "F_Alloc"                                        );
      setmsg_c ( "Attempt to allocate string of length # failed." );
      errint_c ( "#", fStrLen                                     );
      sigerr_c ( "SPICE(MALLOCFAILED)"                            );
      chkout_c ( "F_Alloc"                                        );
      return;
   }

   for ( i = 0;  i < fStrLen;  i++ )
   {
      (*fStr)[i] = ' ';
   }

   return;
}






SpiceStatus F2C_CreateStr (  SpiceInt           fStrLen,
                             ConstSpiceChar   * fStr,
                             SpiceChar       ** cStr    )
{
   SpiceInt    nChars;
   SpiceStatus  status;
   SpiceChar   *tempStr;

   /*
   Find the number of characters, excluding trailing blanks.
   */
   nChars = F_StrLen( fStrLen, fStr );

   /*
   Add one for the NULL.
   */
   nChars++;

   /*
   Now we allocate a string just big enough for all of the characters
   we have. If there is an error, then we return a failure status.
   */
   tempStr = (SpiceChar *) malloc ( nChars );

   if ( tempStr == (SpiceChar *)NULL )
      {
      *cStr = (SpiceChar *) NULL;
      return (SPICEFAILURE);
      } /* end if */

   /*
   Copy the Fortran string into the C string, leaving the trailing
   blanks behind and putting on the trailing NULL character.
   */
   status = F2C_StrCpy ( fStrLen, fStr, nChars, tempStr );

   if ( status == SPICEFAILURE )
      {
      free ( tempStr );
      *cStr = (SpiceChar *) NULL;
      return (SPICEFAILURE);
      } /* end if */

   /*
   Set the output C string.
   */
   *cStr = tempStr;

   /*
   Return success status.
   */
   return (SPICESUCCESS);

} /* end F2C_CreateStr */





void F2C_CreateStr_Sig ( SpiceInt           fStrLen,
                         ConstSpiceChar   * fStr,
                         SpiceChar       ** cStr    )

   /*
   Error signaling version of F2C_CreateStr
   */
{
   SpiceStatus  status;

   status = F2C_CreateStr ( fStrLen, fStr, cStr );

   if ( status == SPICEFAILURE )
   {
      chkin_c  ( "F2C_CreateStr_Sig" );
      setmsg_c ( "An attempt to create a temporary string failed." );
      sigerr_c ( "SPICE(STRINGCREATEFAIL)" );
      chkout_c ( "F2C_CreateStr_Sig" );
      return;
   }

} /* end C2F_CreateStr_Sig */






SpiceStatus F2C_CreateStrArr ( SpiceInt            nStr,
                               SpiceInt            fStrLen,
                               ConstSpiceChar    * fStrArr,
                               SpiceChar       *** cStrArr )
{
   SpiceInt    i;
   SpiceInt    j;
   SpiceInt    length;
   SpiceInt    nChars;
   SpiceStatus  status;
   SpiceChar   *tempStr;
   SpiceChar   *tempPtr;
   SpiceChar   **tempStrList;

   /*
   Find the number of characters, excluding trailing blanks.
   */
   nChars=0;

   for (i=0; i<nStr; i++)
      {
      j = i * fStrLen;
      length = F_StrLen( fStrLen, (fStrArr+j) );
      nChars += length;
      } /* end for */

   /*
   Add in space for all of the Null characters.
   */
   nChars += nStr;

   /*
   Allocate nStr pointers to character strings.
   */
   tempStrList = (SpiceChar **) malloc( nStr * sizeof(SpiceChar *) );

   if ( tempStrList == (SpiceChar **)NULL )
      {
      *cStrArr = (SpiceChar **)NULL;
      return (SPICEFAILURE);
      } /* end if */

   /*
   Now we allocate memory for a block of memory that is just big
   enough to hold all of the characters we have.
   */
   tempStr = (SpiceChar *) malloc ( nChars );

   if ( tempStr == (SpiceChar *)NULL )
      {
      free ( tempStrList );
      *cStrArr = (SpiceChar **)NULL;
      return (SPICEFAILURE);
      } /* end if */

   /*
   Move the Fortran strings into the block of memory, leaving the
   trailing blanks behind and inserting null characters. Also set the
   string pointers.
   */
   tempPtr = tempStr;

   for ( i=0; i<nStr; i++)
      {
      *(tempStrList+i) = tempPtr;

      j   = i * fStrLen;
      length = F_StrLen( fStrLen, (fStrArr+j) );

      status = F2C_StrCpy ( fStrLen, (fStrArr+j), nChars, tempPtr );

      if ( status == SPICEFAILURE )
         {
         free ( tempStr );
         free ( tempStrList );
         *cStrArr = (SpiceChar **)NULL;
         return (SPICEFAILURE);
         }

      length++;
      tempPtr += length;
      nChars  -= length;

      } /* end for */

   /*
   Set the return value.
   */
   *cStrArr = tempStrList;

   /*
   Return success status.
   */
   return (SPICESUCCESS);

} /* end F2C_CreateStrArr */





void F2C_CreateStrArr_Sig ( SpiceInt             nStr,
                            SpiceInt             fStrLen,
                            ConstSpiceChar     * fStrArr,
                            SpiceChar        *** cStrArr )
   /*
   Error signaling version of F2C_CreateStrArr
   */
{
   SpiceStatus  status;

   status = F2C_CreateStrArr ( nStr, fStrLen, fStrArr, cStrArr );



   if ( status == SPICEFAILURE )
   {
      chkin_c  ( "F2C_CreateStrArr_Sig"                            );
      setmsg_c ( "An attempt to create a temporary string failed." );
      sigerr_c ( "SPICE(STRINGCREATEFAIL)"                         );
      chkout_c ( "F2C_CreateStrArr_Sig"                            );
      return;
   }

   return;

} /* end C2F_CreateStr_Sig */





void F2C_FreeStrArr ( SpiceChar  **cStrArr )

   /*
   Free C strings and pointers allocated via the F2C_CreateStrArr
   and F2C_CreateStrArr_Sig functions.
   */
{


   /*
   The string array structure created by F2C_CreateStrArr (which is
   called by F2C_CreateStrArr_Sig) consists of two memory blocks:

      - a contiguous array of character pointers, whose base address is
        the input argument cStrArr

      - a contiguous array of characters in which all of the
        character strings are stored

   So there are two "free" operations to perform:  first, we
   free the character data block, then the pointer block.

   The first string pointer in the pointer block is the base
   address of the character block.
   */

   free ( cStrArr[0] );
   free ( cStrArr    );


} /* end F2C_FreeStrArr */



SpiceStatus F2C_StrCpy ( SpiceInt            fStrLen,
                         ConstSpiceChar    * fStr,
                         SpiceInt            cStrMax,
                         SpiceChar         * cStr    )
{
   SpiceInt nChars;

   /*
   Find the number of characters, excluding trailing blanks in the
   Fortran string.
   */
   nChars = F_StrLen( fStrLen, fStr );

   /*
   Now we check to see if there is enough room in the C string to
   hold all of the characters in the Fortran string plus an extra
   NULL character.
   */
   if ( nChars + 1 > cStrMax )
      {
      return (SPICEFAILURE);
      } /* end if */

   /*
   Move the Fortran string into the block of memory, leaving the
   trailing blanks behind.
   */
   if ( nChars > 0 )
      {
      strncpy ( cStr, fStr, nChars );
      } /* end if */

   /*
   Put in the NULL character.
   */
   *(cStr + nChars) = '\0';

   /*
   Return success status.
   */
   return (SPICESUCCESS);

} /* end F2C_StrCpy */



void F2C_ConvertStr ( SpiceInt         CStrLen,
                      SpiceChar      * fStr    )
{
   /*
   This routine converts a Fortran string to a C string in place.
   A null terminator is placed after the last non-blank character
   in the Fortran string.  The input CStrLen indicates the number of
   characters available in the array pointing to by fStr.  The last
   character is assumed not to contain data; it will be overwritten by
   a null terminator if the input string contains a non-blank character
   at position fStr+CStrLen-2.
   */


   /*
   Local variables
   */
   SpiceInt                nChars;

   /*
   Find the non-blank length of the input String.
   */
   nChars = F_StrLen( CStrLen-1, fStr );

   /*
   Place a null at index nChars.
   */
   fStr[ nChars ] = NULLCHAR;

   return;

} /* End F2C_ConvertStr */



SpiceInt F_StrLen ( SpiceInt       fStrLen,
                    ConstSpiceChar *fStr    )
{
   /*
   These initializations are performed to pacify Valgrind.
   They're not needed by the algorithm below.
   */
   SpiceInt length  = 0;
   SpiceInt nBlanks = 0;
   SpiceInt nChars  = 0;

   /*
   We find the number of characters, excluding trailing blanks in
   a Fortran string.
   */
   nBlanks = 0;
   length  = fStrLen-1;

   while ( length >= 0 )
      {
      if ( *(fStr+length) == ' ' )
         {
         length--;
         nBlanks++;
         } /* end if */
      else
         {
         break;
         } /* end else */
      } /* end while */

   if ( nBlanks == fStrLen )
      {
      nChars = 0;
      } /* end if */
   else
      {
      nChars = fStrLen - nBlanks;
      } /* end else */

   /*
   Return the length of the Fortran string.
   */
   return (nChars);

} /* end F_StrLen */







/*

-Procedure F2C_ConvertStrArr (String to string array)

-Abstract

   A private routine to convert a single string into an array of n
   strings each element having length lenout, including the null
   terminator.

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

   NONE

-Keywords

   STRING
   STRING ARRAY

*/

   void F2C_ConvertStrArr ( SpiceInt     n,
                            SpiceInt     lenout,
                            SpiceChar  * cvals   )
/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   n          I   Number of array values.
   lenout     I   The length of the output string.
   cvals     I/O  Values associated with name.

-Detailed_Input

   n          is the number of array elements needed.

   lenout     The allowed length of the output string array elements.
              This length must large enough to hold the output string
              plus the terminator.

   cvals      on input, a character array containing n Fortran-style
              strings of length lenout-1, packed together contiguously
              without null terminators.

-Detailed_Output

   cvals      on output, a character array containing n null-terminated
              C-style strings of length lenout, including the final
              nulls, packed together contiguously.

-Parameters

   None.

-Exceptions

   None.

-Files

   None.

-Particulars

   This routine is a private routine to the CSPICE library and should
   not be called directly by any user.  It converts a single string into
   an array of strings of equal, specified length, where each element of
   the array is a substring of the original string.

   The purpose of this routine is to convert Fortran-style string arrays
   to C-style arrays.

-Examples

   None.  Don't call this routine.  It is private for NAIF.

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   E.D. Wright  (JPL)

-Version

   -CSPICE Version 1.0.0  9-FEB-1998   (EDW)

-Index_Entries

   CONVERT a string into an array of strings

-&
*/


{
   SpiceChar        * From;
   SpiceChar        * To;

   SpiceInt           i;

   /*
   Loop over the number of requested items.  Start with the last
   string, so we don't overwrite anything as we shift strings towards
   the end of the array.
   */

   for ( i = n; i > 0; i-- )
      {

      /* Get the pointer locations for the from and to locations. */

      To   = ( SpiceChar * ) cvals + ( lenout   ) * ( i - 1 );
      From = ( SpiceChar * ) cvals + ( lenout-1 ) * ( i - 1 );


      memmove ( To, From , lenout - 1);


      /*
      Null-terminate the ith string in the output array.  The terminator
      goes in the element having ordinal position lenout, equivalent to
      index lenout-1.
      */

      To[ lenout - 1 ] = NULLCHAR;

      }

}




/*

-Procedure F2C_ConvertTrStrArr (String to trimmed string array)

-Abstract

   A private routine to convert a single string into an array of n
   strings each element having length lenout, including the null
   terminator.  Each element of the output array has a null character
   following the last non-blank data character.

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

   NONE

-Keywords

   STRING
   STRING ARRAY

*/

   void F2C_ConvertTrStrArr ( SpiceInt     n,
                              SpiceInt     lenout,
                              SpiceChar  * cvals   )
/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   n          I   Number of array values.
   lenout     I   The length of the output string.
   cvals     I/O  Values associated with name.

-Detailed_Input

   n          is the number of array elements needed.

   lenout     The allowed length of the output string array elements.
              This length must large enough to hold the output string
              plus the terminator.

   cvals      on input, a character array containing n Fortran-style
              strings of length lenout-1, packed together contiguously
              without null terminators.

-Detailed_Output

   cvals      on output, a character array containing n null-terminated
              C-style strings of length lenout, including the final
              nulls, packed together contiguously.  The caller should
              declare cvals

                 SpiceChar cvals [n][lenout]

              Each string in the array cvals is "trimmed":  a null
              is placed after the last non-blank character in the
              corresponding input string.


-Parameters

   None.

-Exceptions

   None.

-Files

   None.

-Particulars

   This routine is a private routine to the CSPICE library and should
   not be called directly by any user.  It converts a single string into
   an array of strings of equal, specified length, where each element of
   the array is a substring of the original string.

   The purpose of this routine is to convert Fortran-style string arrays
   to C-style arrays.

-Examples

   None.  Don't call this routine.  It is private for NAIF.

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman (JPL)
   E.D. Wright  (JPL)

-Version

   -CSPICE Version 1.0.0  09-JUL-1999  (NJB) (EDW)

-Index_Entries

   CONVERT a string into a trimmed array of strings

-&
*/


{
   SpiceChar             * strPtr;

   SpiceInt                i;
   SpiceInt                npos;


   /*
   Use the traditional converter to obtain a array of C-style strings,
   each having a null at index lenout-1.
   */
   F2C_ConvertStrArr ( n, lenout, cvals );


   /*
   Place a null after the last non-blank data character of each
   string.
   */

   for ( i = 0; i < n; i++ )
   {
      strPtr = cvals + i*lenout;

      npos = F_StrLen ( lenout-1, strPtr );

      *( strPtr + npos ) = NULLCHAR;
   }
}
