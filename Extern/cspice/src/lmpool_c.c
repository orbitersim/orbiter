/*

-Procedure lmpool_c ( Load variables from memory into the pool )

-Abstract

   Load the variables contained in an internal buffer into the
   kernel pool.

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

   KERNEL

-Keywords

   CONSTANTS
   FILES

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"
   #include "SpiceZim.h"
   #undef    lmpool_c


   void lmpool_c ( const void  * cvals,
                   SpiceInt      cvalen,
                   SpiceInt      n       )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   cvals      I   An array that contains a SPICE text kernel.
   cvalen     I   Length of strings in cvals.
   n          I   The number of entries in cvals.

-Detailed_Input

   cvals       is an array of strings that contains lines of text
               that could serve as a SPICE text kernel. cvals is
               declared as follows:

                  ConstSpiceChar   cvals [n][cvalen]

               Each string in cvals is null-terminated.

   cvalen      is the common length of the strings in cvals,
               including the terminating nulls.

   n           is the number of strings in cvals.

-Detailed_Output

   None.

-Parameters

   None.

-Exceptions

   1)  If any of the kernel pool variables names or their values, as
       provided in the input `cvals' array, cannot be parsed, an error
       is signaled by a routine in the call tree of this routine.

   2)  If there is no room left in the kernel pool to store all
       variables present in the input `cvals' array, an error is
       signaled by a routine in the call tree of this routine.

   3)  If the length of any kernel pool variable name present in the
       input `cvals' array exceeds its maximum allowed length (see
       Kernel Required Reading, kernel.req), an error is signaled by
       a routine in the call tree of this routine.

   4)  If the `cvals' input array pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   5)  If the `cvals' input array strings have length less than two
       characters, the error SPICE(STRINGTOOSHORT) is signaled.

-Files

   None.

-Particulars

   This routine allows you to store a text kernel in an internal
   array of your program and load this array into the kernel pool
   without first storing its contents as a text kernel.

   Kernel pool variable names are restricted to a length of 32
   characters or less.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Create an LSK kernel in a text buffer and load the variables
      contained within the buffer into the kernel pool. Ensure the
      loaded data exists in the kernel pool. Query the pool for
      each expected name, and print the size of the variable with
      that name, and the type of data for that name.

      Convert a UTC time string to ephemeris time to verify that
      the LSK loaded from the text buffer works as if it was loaded
      using a furnsh_c call.


      Example code begins here.


      /.
         Program lmpool_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local constants.
         ./
         #define LNSIZE          81
         #define NLINES          27

         /.
         Local variables.
         ./
         SpiceBoolean            found;

         SpiceChar               dtype  [ 20 ];

         SpiceInt                i;
         SpiceInt                n;

         SpiceDouble             et;


         /.
         Kernel pool variable's names.
         ./
         SpiceChar             * varnam [] = { "DELTET/DELTA_T_A",
                                               "DELTET/K",
                                               "DELTET/EB",
                                               "DELTET/M",
                                               "DELTET/DELTA_AT" };

         /.
         Create a kernel in a text buffer.
         ./
         SpiceChar               textbuf[NLINES][LNSIZE] =
                     {
                        "DELTET/DELTA_T_A = 32.184",
                        "DELTET/K         = 1.657D-3",
                        "DELTET/EB        = 1.671D-2",
                        "DELTET/M         = ( 6.239996 1.99096871D-7 )",
                        "DELTET/DELTA_AT  = ( 10, @1972-JAN-1",
                        "                     11, @1972-JUL-1",
                        "                     12, @1973-JAN-1",
                        "                     13, @1974-JAN-1",
                        "                     14, @1975-JAN-1",
                        "                     15, @1976-JAN-1",
                        "                     16, @1977-JAN-1",
                        "                     17, @1978-JAN-1",
                        "                     18, @1979-JAN-1",
                        "                     19, @1980-JAN-1",
                        "                     20, @1981-JUL-1",
                        "                     21, @1982-JUL-1",
                        "                     22, @1983-JUL-1",
                        "                     23, @1985-JUL-1",
                        "                     24, @1988-JAN-1",
                        "                     25, @1990-JAN-1",
                        "                     26, @1991-JAN-1",
                        "                     27, @1992-JUL-1",
                        "                     28, @1993-JUL-1",
                        "                     29, @1994-JUL-1",
                        "                     30, @1996-JAN-1",
                        "                     31, @1997-JUL-1",
                        "                     32, @1999-JAN-1 )"
                     };

         /.
         Load the kernel data into the kernel pool.
         ./
         lmpool_c ( textbuf, LNSIZE, NLINES );

         /.
         Ensure the loaded data exists in the kernel pool.
         Query the pool for each expected name, size of the
         variable with that name, and the type of data
         for that name.
         ./
         printf ( "Checking data loaded in the kernel pool...\n");
         for ( i = 0; i < 5; i++ )
         {
            dtpool_c ( varnam[i], &found, &n, dtype );

            if ( found )
            {
               printf ( "Found %s\n", varnam[i] );
               printf ( "   # values assigned to name : %d\n", (int)n );
               printf ( "   with data type            : %s\n", dtype  );
            }
            else
            {
               printf ( "Variable '%s' not found in the kernel pool\n",
                         varnam[i]                                     );
            }

         }

         /.
         Convert an UTC string to ephemeris time.
         ./
         str2et_c ( "1991-NOV-26", &et );
         printf ( "\nPerforming time conversion...\n" );
         printf ( "   Ephemeris time (1991-NOV-26): %f\n", et );

         return ( 0 );

      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Checking data loaded in the kernel pool...
      Found DELTET/DELTA_T_A
         # values assigned to name : 1
         with data type            : N
      Found DELTET/K
         # values assigned to name : 1
         with data type            : N
      Found DELTET/EB
         # values assigned to name : 1
         with data type            : N
      Found DELTET/M
         # values assigned to name : 2
         with data type            : N
      Found DELTET/DELTA_AT
         # values assigned to name : 46
         with data type            : N

      Performing time conversion...
         Ephemeris time (1991-NOV-26): -255614341.817042


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   W.L. Taber          (JPL)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.4.0, 04-AUG-2021 (JDR)

       Changed the input argument name "lenvals" to "cvalen" for
       consistency with other routines.

       Edited the header to comply with NAIF standard. Added complete
       code example. Extended the -Exceptions section.

   -CSPICE Version 1.3.1, 10-FEB-2010 (EDW)

       Added mention of the restriction on kernel pool variable
       names to 32 characters or less.

   -CSPICE Version 1.3.0, 12-JUL-2002 (NJB)

       Call to C2F_CreateStrArr_Sig replaced with call to C2F_MapStrArr.

   -CSPICE Version 1.2.0, 28-AUG-2001 (NJB)

       Const-qualified input array.

   -CSPICE Version 1.1.0, 14-FEB-2000 (NJB)

       Calls to C2F_CreateStrArr replaced with calls to error-signaling
       version of this routine:  C2F_CreateStrArr_Sig.

   -CSPICE Version 1.0.0, 08-JUN-1999 (NJB) (WLT)

-Index_Entries

   Load the kernel pool from an internal text buffer

-&
*/

{ /* Begin lmpool_c */



   /*
   Local variables
   */

   SpiceChar             * fCvalsArr;

   SpiceInt                fCvalsLen;


   /*
   Participate in error tracing.
   */
   chkin_c ( "lmpool_c" );

   /*
   Make sure the input string pointer is non-null and that the
   length cvalen is sufficient.
   */
   CHKOSTR ( CHK_STANDARD, "lmpool_c", cvals, cvalen );


   /*
   Create a Fortran-style string array.
   */
   C2F_MapStrArr ( "lmpool_c", n, cvalen, cvals, &fCvalsLen, &fCvalsArr );

   if ( failed_c() )
   {
      chkout_c ( "lmpool_c" );
      return;
   }


   /*
   Call the f2c'd routine.
   */
   lmpool_ (  ( char       * ) fCvalsArr,
              ( integer    * ) &n,
              ( ftnlen       ) fCvalsLen );


   /*
   Free the dynamically allocated array.
   */
   free ( fCvalsArr );

   chkout_c ( "lmpool_c" );

} /* End lmpool_c */
