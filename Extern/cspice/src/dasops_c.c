/*

-Procedure dasops_c ( DAS, open scratch )

-Abstract

   Open a scratch DAS file for writing.

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
   UTILITY

*/
   #include "SpiceUsr.h"
   #include "SpiceZfc.h"

   void dasops_c ( SpiceInt          * handle )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   handle     O   Handle assigned to a scratch DAS file.

-Detailed_Input

   None.

-Detailed_Output

   handle      is the file handle associated with the scratch file
               opened by this routine. This handle is used to
               identify the file in subsequent calls to other DAS
               routines.

-Parameters

   SPICE_DAS_FTSIZE

               is the maximum number of DAS files that a user can have open
               simultaneously. This includes any files used by the DAS system
               when closing files opened with write access. Currently, dascls_c
               (via the SPICELIB routine DASSDR) opens a scratch DAS file using
               dasops_c to segregate (sort by data type) the records in the DAS
               file being closed. Segregating the data by type improves the
               speed of access to the data.

               In order to avoid the possibility of overflowing the DAS file
               table we recommend, when at least one DAS file is open with
               write access, that users of this software limit themselves to at
               most SPICE_DAS_FTSIZE - 2 other open DAS files. If no files are
               to be open with write access, then users may open
               SPICE_DAS_FTSIZE files with no possibility of overflowing the
               DAS file table.

-Exceptions

   1)  If the specified file cannot be opened without exceeding the
       maximum allowed number of open DAS files, the error
       SPICE(DASFTFULL) is signaled by a routine in the call tree of
       this routine. No file will be created.

   2)  If file cannot be opened properly, an error is signaled by a
       routine in the call tree of this routine. No file will be
       created.

   3)  If the initial records in the file cannot be written, the
       error SPICE(DASWRITEFAIL) is signaled by a routine in the call
       tree of this routine. No file will be created.

   4)  If no logical units are available, an error is signaled by a
       routine in the call tree of this routine. No file will be
       created.

-Files

   See output argument `handle'.

   See SPICE_DAS_FTSIZE in the -Parameters section for a description of a
   potential problem with overflowing the DAS file table when at
   least one DAS file is opened with write access.

-Particulars

   This routine is a utility used by the DAS system to provide
   work space needed when creating new DAS files.

   The DAS files created by this routine have initialized file
   records. The file type for a DAS scratch file is "SCR ", so the
   file type "SCR " is not available for general use. As with new
   permanent files, these files are opened for write access. DAS
   files opened by dasops_c are automatically deleted when they are
   closed.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Create a DAS scratch file containing 10 integers, 5 double
      precision numbers, and 4 characters, then print the logical
      address ranges in use.


      Example code begins here.


      /.
         Program dasops_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local variables.
         ./
         SpiceDouble          dbli;
         SpiceInt             handle;
         SpiceInt             i;
         SpiceInt             lastc;
         SpiceInt             lastd;
         SpiceInt             lasti;

         /.
         Use a scratch file, since there's no reason to keep
         the file.
         ./
         dasops_c ( &handle );

         for ( i = 1; i < 11; i++ )
         {
            dasadi_c ( handle, 1, &i );
         }

         for ( i = 1; i < 6; i++ )
         {
            dbli = (SpiceDouble)( i );
            dasadd_c ( handle, 1, &dbli );
         }

         /.
         Add character data to the file. DAS character data are
         treated as a character array, not as a string. The
         following call adds only the first 4 characters to the
         DAS file.
         ./
         dasadc_c ( handle, 4, 0, 3, 8, "SPUDWXY" );

         /.
         Now check the logical address ranges.
         ./
         daslla_c ( handle, &lastc, &lastd, &lasti );

         printf( "Last character address in use: %d\n", (int)lastc );
         printf( "Last d.p. address in use     : %d\n", (int)lastd );
         printf( "Last integer address in use  : %d\n", (int)lasti );

         /.
         Scratch files are automatically deleted when they are
         closed.
         ./
         dascls_c ( handle );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Last character address in use: 4
      Last d.p. address in use     : 5
      Last integer address in use  : 10


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.0.0, 09-JUL-2021 (JDR)

-Index_Entries

   open a scratch DAS file

-&
*/

{ /* Begin dasops_c */

   /*
   Participate in error tracing.
   */
   chkin_c ( "dasops_c" );

   /*
   Call the f2c'd Fortran routine.
   */
   dasops_ (  ( integer    * )  handle  );

   chkout_c ( "dasops_c" );

} /* End dasops_c */
