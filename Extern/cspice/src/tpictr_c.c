/*

-Procedure tpictr_c ( Create a Time Format Picture )

-Abstract

   Create a time format picture suitable for use by the routine
   timout_c from a given sample time string.

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

   TIME

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"
   #include "SpiceZst.h"


   void tpictr_c ( ConstSpiceChar * sample,
                   SpiceInt         pictln,
                   SpiceInt         errmln,
                   SpiceChar      * pictur,
                   SpiceBoolean   * ok,
                   SpiceChar      * errmsg )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   sample     I   A sample time string.
   pictln     I   The length for the output picture string.
   errmln     I   The length for the output error string.
   pictur     O   A format picture that describes sample.
   ok         O   Flag indicating whether sample parsed successfully.
   errmsg     O   Diagnostic returned if sample cannot be parsed.

-Detailed_Input

   sample      is a representative time string to use as a model to
               format time strings.

   pictln      is the allowed length for the output picture. This length
               must large enough to hold the output string plus the null
               terminator. If the output string is expected to have x
               characters, `pictln' needs to be x + 1. 80 is a reasonable
               value for `pictln' (79 characters plus the null
               terminator).

   errmln      is the allowed length for the output error string.

-Detailed_Output

   pictur      is a format picture suitable for use with the SPICE
               routine timout_c. This picture, when used to format an
               epoch via timout_c, will yield the same time components in
               the same order as the components in sample.

   ok          is a logical flag indicating whether the input format
               sample could be parsed. If all of the components of
               sample are recognizable, ok will be returned with the
               value SPICEFALSE. If some part of pictur cannot be
               parsed, ok will be returned with the value SPICEFALSE.

   errmsg      is a diagnostic message that indicates what part of
               sample was not recognizable. If sample was successfully
               parsed, ok will be SPICEFALSE and errmsg will be
               returned as an empty string.

-Parameters

   None.

-Exceptions

   1)  All problems with the inputs are reported via `ok' and `errmsg'.

   2)  If a format picture can not be created from the sample
       time string, `pictur' is returned as a blank string.

   3)  If the `sample' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   4)  If the `sample' input string has zero length, the error
       SPICE(EMPTYSTRING) is signaled.

   5)  If any of the `pictur' or `errmsg' output string pointers is
       null, the error SPICE(NULLPOINTER) is signaled.

   6)  If any of the `pictur' or `errmsg' output strings has length
       less than two characters, the error SPICE(STRINGTOOSHORT) is
       signaled, since the output string is too short to contain one
       character of output data plus a null terminator.

-Files

   None.

-Particulars

   Although the routine timout_c provides CSPICE users with a great
   deal of flexibility in formatting time strings, users must
   master the means by which a time picture is constructed
   suitable for use by timout_c.

   This routine allows CSPICE users to supply a sample time string
   from which a corresponding time format picture can be created,
   freeing users from the task of mastering the intricacies of
   the routine timout_c.

   Note that timout_c can produce many time strings whose patterns
   can not be discerned by this routine. When such outputs are
   called for, the user must consult timout_c and construct the
   appropriate format picture "by hand." However, these exceptional
   formats are not widely used and are not generally recognizable
   to an uninitiated reader.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as input,
   the compiler and supporting libraries, and the machine specific
   arithmetic implementation.

   1) Given a sample with the format of the UNIX date string
      local to California, create a SPICE time picture for use
      in timout_c.

      Using that SPICE time picture, convert a series of ephemeris
      times to that picture format.

      Use the LSK kernel below to load the leap seconds and time
      constants required for the conversions.

         naif0012.tls


      Example code begins here.


      /.
         Program tpictr_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local parameters.
         ./
         #define ERRLEN       400
         #define TIMLEN       65

         /.
         Local variables
         ./
         SpiceBoolean         ok;

         SpiceChar            err    [ERRLEN];
         SpiceChar            pictur [TIMLEN];
         SpiceChar          * sample;
         SpiceChar            timstr [TIMLEN];
         SpiceChar          * utcstr;

         SpiceDouble          et;


         /.
         Load LSK file.
         ./
         furnsh_c ( "naif0012.tls" );

         /.
         Create the required time picture.
         ./
         sample = "Thu Oct 01 11:11:11 PDT 1111";

         tpictr_c ( sample, ERRLEN, TIMLEN, pictur, &ok, err );

         if ( ! ok )
         {

            printf( "Invalid time picture.\n" );
            printf( "%s\n", err );

         }
         else
         {

            /.
            Convert the input UTC time to ephemeris time.
            ./
            utcstr = "24 Mar 2018  16:23:00 UTC";
            str2et_c ( utcstr, &et );

            /.
            Now convert `et' to the desired output format.
            ./
            timout_c ( et, pictur, TIMLEN, timstr );

            printf( "Sample format:  %s\n", sample );
            printf( "Time picture :  %s\n", pictur );
            printf( "\n" );
            printf( "Input UTC    :  %s\n", utcstr );
            printf( "Output       :  %s\n", timstr );

         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Sample format:  Thu Oct 01 11:11:11 PDT 1111
      Time picture :  Wkd Mon DD HR:MN:SC PDT YYYY ::UTC-7

      Input UTC    :  24 Mar 2018  16:23:00 UTC
      Output       :  Sat Mar 24 09:23:00 PDT 2018


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   J. Diaz del Rio     (ODC Space)
   W.L. Taber          (JPL)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.1.0, 05-AUG-2021 (JDR)

       Changed the input argument names "lenout" and "lenerr" to
       "pictln" and "errmln" for consistency with other routines.

       Edited the header to comply with NAIF standard.
       Converted the existing code fragments into complete example
       and added reference to required LSK.

   -CSPICE Version 1.0.0, 23-JUL-1999 (EDW) (WLT)

-Index_Entries

   Use a sample time string to produce a time format picture

-&
*/

{ /* Begin tpictr_c */

   /*
   Local variables
   */
   logical                 okeydoke;

   /*
   Participate in error tracing.
   */
   chkin_c ( "tpictr_c" );


   /*
   Check the input string sample to make sure the pointer is non-null
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "tpictr_c", sample );


   /*
   Make sure the output strings have at least enough room for one output
   character and a null terminator.  Also check for a null pointer.
   */
   CHKOSTR ( CHK_STANDARD, "tpictr_c",  pictur, pictln );
   CHKOSTR ( CHK_STANDARD, "tpictr_c",  errmsg, errmln );


   /*
   Call the f2c'd routine.
   */
   tpictr_( ( char    * ) sample,
            ( char    * ) pictur,
            ( logical * ) &okeydoke,
            ( char    * ) errmsg,
            ( ftnlen    ) strlen( sample ),
            ( ftnlen    ) pictln - 1,
            ( ftnlen    ) errmln - 1       );


   /*
   Convert the output strings to C style.
   */
   F2C_ConvertStr( pictln, pictur );
   F2C_ConvertStr( errmln, errmsg );


   /*
   Convert the status flag from logical to SpiceBoolean.
   */

   *ok = okeydoke;


   chkout_c ( "tpictr_c" );


} /* End tpictr_c */
