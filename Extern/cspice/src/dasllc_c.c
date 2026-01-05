/*

-Procedure dasllc_c ( DAS, low-level close )

-Abstract

   Close the DAS file associated with a given handle, without
   flushing buffered data or segregating the file.

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

   DAS
   FILES

*/
   #include "SpiceUsr.h"
   #include "SpiceZfc.h"

   void dasllc_c ( SpiceInt            handle )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   handle     I   Handle of a DAS file to be closed.

-Detailed_Input

   handle      is the handle of a previously opened DAS file.

-Detailed_Output

   None.

   See -Particulars for a description of the effect of this routine.

-Parameters

   None.

-Exceptions

   1)  If the specified handle does not belong to a DAS file that is
       currently open, this routine returns without signaling an
       error.

-Files

   See the description of the argument `handle' in -Detailed_Input.

-Particulars

   Normally, routines outside of CSPICE will not need to call this
   routine. Application programs should close DAS files by calling
   the CSPICE routine dascls_c. This routine is a lower-level
   routine that is called by dascls_c, but (obviously) does not have
   the full functionality of dascls_c.

   This routine closes a DAS file and updates the DAS file manager's
   bookkeeping information on open DAS files. Because the DAS file
   manager must keep track of which files are open at any given time,
   it is important that DAS files be closed only with dascls_c or
   dasllc_c, to prevent the remaining DAS routines from failing,
   sometimes mysteriously.

   Note that when a file is opened more than once for read or write
   access, dasopr_c returns the same handle each time it is re-opened.
   Each time the file is closed, dasllc_c checks to see if any other
   claims on the file are still active before physically closing
   the file.

   Unlike dascls_c, this routine does not force a write of updated,
   buffered records to the indicated file, nor does it segregate the
   data records in the file.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Write a DAS file by adding data to it over multiple passes.
      Avoid spending time on file segregation between writes.

      Each pass opens the file, adds character, double precision,
      and integer data to the file, writes out buffered data by
      calling daswbr_c, and closes the file without segregating the
      data by calling dasllc_c.

      The program also checks the file: after the final write,
      the program reads the data and compares it to expected values.

      Note that most user-oriented applications should segregate a
      DAS file after writing it, since this greatly enhances file
      reading efficiency. The technique demonstrated here may be
      useful for cases in which a file will be written via many
      small data additions, and in which the file is read between
      write operations.


      Example code begins here.


      /.
         Program dasllc_ex1
      ./
      #include <stdio.h>
      #include <stdlib.h>
      #include <string.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local parameters
         ./
         #define CHRLEN       50
         #define IBUFSZ       20
         #define DBUFSZ       30

         /.
         Local variables
         ./
         SpiceChar            chrbuf [CHRLEN];
         SpiceChar            chrstr [CHRLEN+1];
         SpiceChar            xchrbf [CHRLEN+1];

         SpiceDouble          dpbuf  [DBUFSZ];
         SpiceDouble          xdpbuf [DBUFSZ];

         SpiceInt             firstc;
         SpiceInt             firstd;
         SpiceInt             firsti;
         SpiceInt             handle;
         SpiceInt             i;
         SpiceInt             intbuf [IBUFSZ];
         SpiceInt             j;
         SpiceInt             lastc;
         SpiceInt             lastd;
         SpiceInt             lasti;
         SpiceInt             passno;
         SpiceInt             xintbf [IBUFSZ];

         /.
         Initial values
         ./
         ConstSpiceChar     * fname = "dasllc_ex1.das";
         ConstSpiceChar     * ftype = "ANG";
         SpiceInt             ncall = 1000;
         SpiceInt             ncomr = 10;
         SpiceInt             npass = 3;

         /.
         Open a new DAS file. We'll allocate `ncomr' records
         for comments. The file type is not one of the standard
         types recognized by SPICE; however it can be used to
         ensure the database file is of the correct type.

         We'll use the file name as the internal file name.
         ./
         dasonw_c ( fname, ftype, fname, ncomr, &handle );

         /.
         Add data of character, integer, and double precision
         types to the file in interleaved fashion. We'll add to
         the file over `npass' "passes," in each of which we close
         the file after writing.
         ./
         for ( passno = 1; passno <= npass; passno++ )
         {
            if ( passno > 1 )
            {
               printf( "Opening file for write access...\n" );

               dasopw_c ( fname, &handle );
            }

            for ( i = 1; i <= ncall; i++ )
            {

               /.
               Add string data to the file.
               ./
               strncpy( chrbuf, "Character value #", 18 );
               repmi_c ( chrbuf, "#", i, CHRLEN, chrbuf );

               dasadc_c ( handle, CHRLEN, 0, CHRLEN-1, CHRLEN, chrbuf );

               /.
               Add double precision data to the file.
               ./
               for ( j = 1; j <= DBUFSZ; j++ )
               {
                  dpbuf[j-1] = (SpiceDouble)( 100000000*passno + 100*i + j );
               }

               dasadd_c ( handle, DBUFSZ, dpbuf );

               /.
               Add integer data to the file.
               ./
               for ( j = 1; j <= IBUFSZ; j++ )
               {
                  intbuf[j-1] = 100000000*passno  +  100 * i  +  j;
               }

               dasadi_c ( handle, IBUFSZ, intbuf );
            }

            /.
            Write buffered data to the file.
            ./
            printf( "Writing buffered data...\n" );
            daswbr_c ( handle );

            /.
            Close the file without segregating it.
            ./
            printf( "Closing DAS file...\n" );
            dasllc_c ( handle );
         }

         printf( "File write is done.\n" );

         /.
         Check file contents.
         ./
         dasopr_c ( fname, &handle );

         /.
         Read data from the file; compare to expected values.

         Initialize end addresses.
         ./
         lastc = 0;
         lastd = 0;
         lasti = 0;

         for ( passno = 1; passno <= npass; passno++ )
         {
            for ( i = 1; i <= ncall; i++ )
            {

               /.
               Check string data.
               ./
               strncpy( xchrbf, "Character value #", 18 );
               repmi_c ( xchrbf, "#", i, CHRLEN+1, xchrbf );

               firstc = lastc + 1;
               lastc  = lastc + CHRLEN;

               dasrdc_c ( handle,   firstc, lastc, 0,
                          CHRLEN-1, CHRLEN, chrbuf   );

               if ( strncmp( chrbuf, xchrbf, CHRLEN ) )
               {
                  /.
                  Add null-terminating character to `chrbuf' in order to
                  print it to the screen.
                  ./
                  strncpy( chrstr, chrbuf, CHRLEN );
                  chrstr[CHRLEN] = '\0';

                  printf( "Character data mismatch:\n" );
                  printf( "PASS     = %d\n", (int)passno );
                  printf( "I        = %d\n", (int)i );
                  printf( "Expected = %s\n", xchrbf );
                  printf( "Actual   = %s\n", chrstr );
                  exit( EXIT_FAILURE );
               }

               /.
               Check double precision data.
               ./
               for ( j = 1; j <= DBUFSZ; j++ )
               {
                  xdpbuf[j-1] = (SpiceDouble)( 100000000*passno + 100*i + j );
               }

               firstd = lastd + 1;
               lastd  = lastd + DBUFSZ;

               dasrdd_c ( handle, firstd, lastd, dpbuf );

               for ( j = 0; j < DBUFSZ; j++ )
               {
                  if ( dpbuf[j] != xdpbuf[j] )
                  {
                     printf( "Double precision data mismatch:\n" );
                     printf( "PASS     = %d\n", (int)passno );
                     printf( "I        = %d\n", (int)i );
                     printf( "J        = %d\n", (int)j );
                     printf( "Expected = %f\n", xdpbuf[j] );
                     printf( "Actual   = %f\n", dpbuf[j] );
                     exit( EXIT_FAILURE );
                  }
               }

               /.
               Check integer data.
               ./
               for ( j = 1; j <= IBUFSZ; j++ )
               {
                  xintbf[j-1] = 100000000*passno  +  100 * i  +  j;
               }

               firsti = lasti + 1;
               lasti  = lasti + IBUFSZ;

               dasrdi_c ( handle, firsti, lasti, intbuf );

               for ( j = 0; j < IBUFSZ; j++ )
               {
                  if ( intbuf[j] != xintbf[j] )
                  {
                     printf( "Integer data mismatch:\n" );
                     printf( "PASS     = %d\n", (int)passno );
                     printf( "I        = %d\n", (int)i );
                     printf( "J        = %d\n", (int)j );
                     printf( "Expected = %d\n", xintbf[j] );
                     printf( "Actual   = %d\n", intbuf[j] );
                     exit( EXIT_FAILURE );
                  }
               }
            }
         }

         printf( "File check is done.\n" );

         /.
         Close the file.
         ./
         dascls_c ( handle );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Writing buffered data...
      Closing DAS file...
      Opening file for write access...
      Writing buffered data...
      Closing DAS file...
      Opening file for write access...
      Writing buffered data...
      Closing DAS file...
      File write is done.
      File check is done.


      Note that after run completion, a new DAS file exists in the
      output directory.

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.0.0, 23-FEB-2021 (JDR)

-Index_Entries

   close a DAS file

-&
*/

{ /* Begin dasllc_c */

   /*
   Participate in error tracing.
   */
   chkin_c ( "dasllc_c" );

   /*
   Call the f2c'd Fortran routine.
   */
   dasllc_ (  ( integer    * ) &handle  );

   chkout_c ( "dasllc_c" );

} /* End dasllc_c */
