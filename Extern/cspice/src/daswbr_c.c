/*

-Procedure daswbr_c ( DAS, write buffered records )

-Abstract

   Write out all buffered records of a specified DAS file.

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

   ASSIGNMENT
   DAS
   FILES

*/
   #include "SpiceUsr.h"
   #include "SpiceZfc.h"

   void daswbr_c ( SpiceInt            handle )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   handle     I   Handle of DAS file.

-Detailed_Input

   handle      is the handle of a DAS file opened for writing.

-Detailed_Output

   None.

   See -Particulars for a description of the action of this routine.

-Parameters

   None.

-Exceptions

   1)  If the input file handle is invalid, an error is signaled
       by a routine in the call tree of this routine. The indicated
       file will not be modified.

   2)  If a write operation attempted by this routine fails, an
       error is signaled by a routine in the call tree of this
       routine. The status of the DAS file written to is uncertain
       in this case.

-Files

   See the description of the argument `handle' in -Detailed_Input.

-Particulars

   This routine writes buffered records out to the DAS file to which
   they correspond.

   Because the DAS system buffers records that are written as well as
   those that are read, data supplied to the DAS add data (dasadc_c,
   dasadd_c, dasadi_c) and DAS update data (dasudc_c, dasudd_c, dasudi_c)
   routines on input has not necessarily been physically written to
   the DAS file specified by the caller of those routines, at the
   time those routines return. Before closing a DAS file that has
   been opened for writing, the DAS system must write out to the file
   any updated records present in the DAS buffers. The CSPICE
   routine dascls_c uses this routine to perform this function. The
   routines dasac_c and dasdc_c, through the use of the SPICELIB routines
   DASACR and DASRCR, which respectively add comment records to or
   delete comment records from a DAS file, use this routine to ensure
   that the SPICELIB routine DASRWR record buffers don't become out of
   sync with the file they operate upon.

   In addition, this routine can be used by application programs
   that create or update DAS files. The reason for calling this
   routine directly would be to provide a measure of safety when
   writing a very large file: if the file creation or update were
   interrupted, the amount of work lost due to the loss of buffered,
   unwritten records could be reduced.

   However, routines outside of CSPICE will generally not need to
   call this routine directly.

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
         Program daswbr_ex1
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
         ConstSpiceChar     * fname = "daswbr_ex1.das";
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

   -CSPICE Version 1.0.0, 19-MAY-2021 (JDR)

-Index_Entries

   write buffered records to a DAS file

-&
*/

{ /* Begin daswbr_c */

   /*
   Participate in error tracing.
   */
   chkin_c ( "daswbr_c" );

   /*
   Call the f2c'd Fortran routine.
   */
   daswbr_ (  ( integer    * ) &handle  );

   chkout_c ( "daswbr_c" );

} /* End daswbr_c */
