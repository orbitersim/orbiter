/*

-Procedure dashfs_c ( DAS, handle to file summary )

-Abstract

   Return a file summary for a specified DAS file.

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

   void dashfs_c ( SpiceInt            handle,
                   SpiceInt          * nresvr,
                   SpiceInt          * nresvc,
                   SpiceInt          * ncomr,
                   SpiceInt          * ncomc,
                   SpiceInt          * free,
                   SpiceInt            lastla [3],
                   SpiceInt            lastrc [3],
                   SpiceInt            lastwd [3] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   handle     I   Handle of a DAS file.
   nresvr     O   Number of reserved records in file.
   nresvc     O   Number of characters in use in reserved rec. area.
   ncomr      O   Number of comment records in file.
   ncomc      O   Number of characters in use in comment area.
   free       O   Number of first free record.
   lastla     O   Array of last logical addresses for each data type.
   lastrc     O   Record number of last descriptor of each data type.
   lastwd     O   Word number of last descriptor of each data type.

-Detailed_Input

   handle      is the handle of a previously opened DAS file. The file
               may be open for read or write access.

-Detailed_Output

   nresvr      is the number of reserved records in a specified DAS
               file.

   nresvc      is the number of characters in use in the reserved record
               area of a specified DAS file.

   ncomr       is the number of comment records in a specified DAS file.

   ncomc       is the number of characters in use in the comment area of
               a specified DAS file.

   free        is the 1-based record number of the first free record in
               a specified DAS file.

   lastla      is an array containing the highest current 1-based logical
               addresses, in the specified DAS file, of data of
               character, double precision, and integer types, in that
               order.

   lastrc      is an array containing the 1-based record numbers, in the
               specified DAS file, of the directory records containing
               the current last descriptors of clusters of character,
               double precision, and integer data records, in that
               order.

   lastwd      is an array containing the 1-based word indices, within
               the respective descriptor records identified by the
               elements of `lastrc', of the current last descriptors of
               clusters of character, double precision, and integer data
               records, in that order.

-Parameters

   See header file SpiceDAS.h for declarations and descriptions of
   parameters used throughout the DAS system.

   SPICE_DAS_CHARDT,
   SPICE_DAS_DPDT,
   SPICE_DAS_INTDT

               are data type specifiers which indicate SpiceChar,
               SpiceDouble, and SpiceInt respectively. These
               parameters are used in all DAS routines that require a
               data type specifier.

-Exceptions

   1)  If the specified handle does not belong to any file that is
       currently known to be open, the error SPICE(DASNOSUCHHANDLE)
       is signaled by a routine in the call tree of this routine.

-Files

   None.

-Particulars

   The quantities

      nresvr
      nresvc
      ncomr
      ncomc
      free
      lastla
      lastrc
      lastwd

   define the "state" of a DAS file, and in particular the state of
   the directory structure of the file. This information is needed by
   other DAS routines, but application programs will usually have no
   need for it. The one exception is the array of "last" logical
   addresses `lastla': these addresses indicate how many words of data
   of each type are contained in the specified DAS file. The elements
   of `lastla' can be conveniently retrieved by calling daslla_c.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Create a DAS file containing 10 integers, 5 double precision
      numbers, and 4 characters. Print the summary of the file and
      dump its contents.


      Example code begins here.


      /.
         Program dashfs_ex1
      ./
      #include <stdio.h>
      #include <string.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local parameters.
         ./
         #define FNAME        "dashfs_ex1.das"
         #define LINLEN       2

         /.
         Local variables.
         ./
         ConstSpiceChar     * ifname;
         ConstSpiceChar     * type;
         SpiceChar            line   [LINLEN];
         SpiceChar            linstr [LINLEN+1];

         SpiceDouble          dbli;
         SpiceDouble          x;

         SpiceInt             first;
         SpiceInt             free;
         SpiceInt             handle;
         SpiceInt             i;
         SpiceInt             last;
         SpiceInt             lastla [3];
         SpiceInt             lastrc [3];
         SpiceInt             lastwd [3];
         SpiceInt             n;
         SpiceInt             ncomc;
         SpiceInt             ncomr;
         SpiceInt             nread;
         SpiceInt             nresvc;
         SpiceInt             nresvr;
         SpiceInt             remain;

         /.
         Open a new DAS file. Reserve no records for comments.
         ./
         type   = "TEST";
         ifname = "TEST.DAS/NAIF/NJB/11-NOV-1992-20:12:20";

         dasonw_c ( FNAME, type, ifname, 0, &handle );

         /.
         Obtain the file summary.
         ./
         dashfs_c ( handle, &nresvr, &nresvc,
                    &ncomr, &ncomc,  &free,
                    lastla, lastrc,  lastwd );

         /.
         Print the summary of the new file.
         ./
         printf( "Summary before adding data:\n" );
         printf( "   Number of reserved records     : %d\n", (int)nresvr );
         printf( "   Characters in reserved records : %d\n", (int)nresvc );
         printf( "   Number of comment records      : %d\n", (int)ncomr );
         printf( "   Characters in comment area     : %d\n", (int)ncomc );
         printf( "   Number of first free record    : %d\n", (int)free );
         printf( "   Last logical character address : %d\n",
                               (int)lastla[SPICE_DAS_CHARDT] );
         printf( "   Last logical d.p. address      : %d\n",
                                 (int)lastla[SPICE_DAS_DPDT] );
         printf( "   Last logical integer address   : %d\n",
                                (int)lastla[SPICE_DAS_INTDT] );
         printf( "   Last character descriptor      : %d\n",
                               (int)lastrc[SPICE_DAS_CHARDT] );
         printf( "   Last d.p descriptor            : %d\n",
                                 (int)lastrc[SPICE_DAS_DPDT] );
         printf( "   Last integer descriptor        : %d\n",
                                (int)lastrc[SPICE_DAS_INTDT] );
         printf( "   Character word position in desc: %d\n",
                               (int)lastwd[SPICE_DAS_CHARDT] );
         printf( "   d.p. word position in desc     : %d\n",
                                 (int)lastwd[SPICE_DAS_DPDT] );
         printf( "   Integer word position in desc  : %d\n",
                                (int)lastwd[SPICE_DAS_INTDT] );
         printf( "\n" );

         /.
         Add the data.
         ./
         for ( i = 1; i < 11; i++ )
         {
            dasadi_c ( handle, 1, &i );
         }

         for ( i = 1; i < 6; i++ )
         {
            dbli = (double)i;
            dasadd_c ( handle, 1, &dbli );
         }

         /.
         Add character data to the file. DAS character data are
         treated as a character array, not as a null-terminated
         string. The following call adds only the first 4 characters
         to the DAS file.
         ./
         dasadc_c ( handle, 4, 0, 3, 4, "SPUDWXY" );

         /.
         Close the file and open it for reading.
         ./
         dascls_c ( handle );
         dasopr_c ( FNAME, &handle );

         /.
         Obtain again the file summary.
         ./
         dashfs_c ( handle, &nresvr, &nresvc,
                    &ncomr, &ncomc,  &free,
                    lastla, lastrc,  lastwd );

         printf( "Summary after adding data:\n" );
         printf( "   Number of reserved records     : %d\n", (int)nresvr );
         printf( "   Characters in reserved records : %d\n", (int)nresvc );
         printf( "   Number of comment records      : %d\n", (int)ncomr );
         printf( "   Characters in comment area     : %d\n", (int)ncomc );
         printf( "   Number of first free record    : %d\n", (int)free );
         printf( "   Last logical character address : %d\n",
                               (int)lastla[SPICE_DAS_CHARDT] );
         printf( "   Last logical d.p. address      : %d\n",
                                 (int)lastla[SPICE_DAS_DPDT] );
         printf( "   Last logical integer address   : %d\n",
                                (int)lastla[SPICE_DAS_INTDT] );
         printf( "   Last character descriptor      : %d\n",
                               (int)lastrc[SPICE_DAS_CHARDT] );
         printf( "   Last d.p descriptor            : %d\n",
                                 (int)lastrc[SPICE_DAS_DPDT] );
         printf( "   Last integer descriptor        : %d\n",
                                (int)lastrc[SPICE_DAS_INTDT] );
         printf( "   Character word position in desc: %d\n",
                               (int)lastwd[SPICE_DAS_CHARDT] );
         printf( "   d.p. word position in desc     : %d\n",
                                 (int)lastwd[SPICE_DAS_DPDT] );
         printf( "   Integer word position in desc  : %d\n",
                                (int)lastwd[SPICE_DAS_INTDT] );
         printf( "\n" );

         /.
         Read the integers and dump them.
         ./
         printf( "Integer data in the DAS file:\n" );
         for ( i = 1; i <= lastla[SPICE_DAS_INTDT]; i++ )
         {
            dasrdi_c ( handle, i, i, &n );
            printf( "   %d\n", (int)n );
         }

         /.
         Now the d.p. numbers:
         ./
         printf( "\n" );
         printf( "Double precision data in the DAS file:\n" );
         for ( i = 1; i <= lastla[SPICE_DAS_DPDT]; i++ )
         {
            dasrdd_c ( handle, i, i, &x );
            printf( "   %f\n", x );
         }

         /.
         Now the characters. In this case, we read the
         data one line at a time.
         ./
         first   =  0;
         last    =  0;
         remain  =  lastla[SPICE_DAS_CHARDT];

         printf( "\n" );
         printf( "Character data in the DAS file:\n" );
         while ( remain > 0 )
         {
            nread = mini_c( 2, LINLEN, remain );
            first = last + 1;
            last  = last + nread;

            dasrdc_c ( handle, first, last, 0, nread-1, LINLEN, line );

            /.
            Add null-terminating character to `line' in order to
            print it to the screen.
            ./
            strncpy( linstr, line, LINLEN );
            linstr[LINLEN] = '\0';

            printf( "   %s\n", linstr );

            remain = remain - nread;
         }

         /.
         Close the file.
         ./
         dascls_c ( handle );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Summary before adding data:
         Number of reserved records     : 0
         Characters in reserved records : 0
         Number of comment records      : 0
         Characters in comment area     : 0
         Number of first free record    : 3
         Last logical character address : 0
         Last logical d.p. address      : 0
         Last logical integer address   : 0
         Last character descriptor      : 0
         Last d.p descriptor            : 0
         Last integer descriptor        : 0
         Character word position in desc: 0
         d.p. word position in desc     : 0
         Integer word position in desc  : 0

      Summary after adding data:
         Number of reserved records     : 0
         Characters in reserved records : 0
         Number of comment records      : 0
         Characters in comment area     : 0
         Number of first free record    : 6
         Last logical character address : 4
         Last logical d.p. address      : 5
         Last logical integer address   : 10
         Last character descriptor      : 2
         Last d.p descriptor            : 2
         Last integer descriptor        : 2
         Character word position in desc: 10
         d.p. word position in desc     : 11
         Integer word position in desc  : 12

      Integer data in the DAS file:
         1
         2
         3
         4
         5
         6
         7
         8
         9
         10

      Double precision data in the DAS file:
         1.000000
         2.000000
         3.000000
         4.000000
         5.000000

      Character data in the DAS file:
         SP
         UD


      Note that after run completion, a new DAS file exists in the
      output directory.

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.0.0, 16-JUL-2021 (JDR)

-Index_Entries

   return the file summary of a DAS file
   find the amount of data in a DAS file

-&
*/

{ /* Begin dashfs_c */

   /*
   Participate in error tracing.
   */
   chkin_c ( "dashfs_c" );

   /*
   Call the f2c'd Fortran routine.
   */
   dashfs_ (  ( integer    * ) &handle,
              ( integer    * )  nresvr,
              ( integer    * )  nresvc,
              ( integer    * )  ncomr,
              ( integer    * )  ncomc,
              ( integer    * )  free,
              ( integer    * )  lastla,
              ( integer    * )  lastrc,
              ( integer    * )  lastwd  );

   chkout_c ( "dashfs_c" );

} /* End dashfs_c */
