/*

-Procedure dlaopn_c ( DLA, open new file )

-Abstract

   Open a new DLA file and set the file type.

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
   DLA

-Keywords

   DAS
   DLA
   FILES

*/
   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"

   void dlaopn_c ( ConstSpiceChar    * fname,
                   ConstSpiceChar    * ftype,
                   ConstSpiceChar    * ifname,
                   SpiceInt            ncomch,
                   SpiceInt          * handle )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   fname      I   Name of a DLA file to be opened.
   ftype      I   Mnemonic code for type of data in the DLA file.
   ifname     I   Internal file name.
   ncomch     I   Number of comment characters to allocate.
   handle     O   Handle assigned to the opened DLA file.

-Detailed_Input

   fname       is the name of a new DLA file to be created. The file
               will be left opened for write access.

   ftype       is a code for type of data placed into a DLA file. The
               non-blank part of `ftype' is used as the "file type"
               portion of the ID word in the DLA file.

               The first nonblank character and the three, or fewer,
               characters immediately following it, giving four
               characters, are used to represent the type of the data
               placed in the DLA file. This is provided as a convenience
               for higher level software. It is an error if this string
               is blank. Also, the file type may not contain any
               nonprinting characters. When written to the DLA file, the
               value for the type IS case sensitive.

               NAIF has reserved for its own use file types consisting
               of the upper case letters (A-Z) and the digits 0-9. NAIF
               recommends lower case or mixed case file types be used by
               all others in order to avoid any conflicts with NAIF file
               types.

   ifname      is the internal file name for the new file. The name may
               contain as many as 60 characters. This name should
               uniquely identify the file.

   ncomch      is the number of comment characters to allocate.

               `ncomch' is used to establish the number of comment records
               that will be allocated to the new DLA file. The number of
               comment records allocated is the minimum required to
               store the specified number of comment characters.

               Allocating comment records at file creation time may
               reduce the likelihood of having to expand the
               comment area later.

-Detailed_Output

   handle      is the file handle associated with the file. This handle
               is used to identify the file in subsequent calls to other
               DLA routines.

-Parameters

   None.

-Exceptions

   1)  If the input filename is blank, an error is signaled by a
       routine in the call tree of this routine. No file will be
       created.

   2)  If the specified file cannot be opened without exceeding the
       maximum allowed number of open DAS files, an error is signaled
       by a routine in the call tree of this routine. No file will be
       created.

   3)  If the file cannot be opened properly, an error is signaled by
       a routine in the call tree of this routine. No file will be
       created.

   4)  If the initial records in the file cannot be written, an error
       is signaled by a routine in the call tree of this routine. No
       file will be created.

   5)  If no logical units are available, an error is signaled by a
       routine in the call tree of this routine. No file will be
       created.

   6)  If the file type is blank, an error is signaled by a routine
       in the call tree of this routine. No file will be created.

   7)  If the file type contains nonprinting characters, decimal 0-31
       and 127-255, an error is signaled by a routine in the call
       tree of this routine. No file will be created.

   8)  If the number of comment characters allocated to be allocated,
       `ncomch', is negative, the error SPICE(BADRECORDCOUNT) is
       signaled by a routine in the call tree of this routine. No
       file will be created.

   9)  If any of the `fname', `ftype' or `ifname' input string
       pointers is null, the error SPICE(NULLPOINTER) is signaled.

   10) If any of the `fname', `ftype' or `ifname' input strings has
       zero length, the error SPICE(EMPTYSTRING) is signaled.

-Files

   See argument `fname'.

-Particulars

   DLA files are built using the DAS low-level format; DLA files are
   a specialized type of DAS file in which data are organized as a
   doubly linked list of segments. Each segment's data belong to
   contiguous components of character, double precision, and integer
   type.

   This routine creates a new DLA file and sets the type of the
   file to the mnemonic code passed to it.

   DLA files created by this routine have initialized file records.
   The ID word in a DLA file record has the form

      DAS/xxxx

   where the characters following the slash are supplied by the
   caller of this routine.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Create a DLA file containing one segment; the segment
      contains character, double precision, and integer data.
      After writing and closing the file, open the file for
      read access; dump the data to standard output.


      Example code begins here.


      /.
         Program dlaopn_ex1
      ./
      #include <stdio.h>
      #include <string.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local parameters
         ./
         #define DLA          "dlaopn_ex1.dla"
         #define LNSIZE       61
         #define MAXC         5
         #define MAXD         50
         #define MAXI         100

         /.
         Local variables
         ./
         SpiceChar            buffer [2];
         SpiceChar            cvals  [MAXC][LNSIZE];
         SpiceChar            cvals2 [MAXC][LNSIZE];
         SpiceChar            cvastr [LNSIZE+1];
         SpiceChar          * ifname;

         SpiceDouble          dvals  [MAXD];
         SpiceDouble          dvals2 [MAXD];

         SpiceDLADescr        descr;

         SpiceInt             base;
         SpiceInt             handle;
         SpiceInt             i;
         SpiceInt             ivals  [MAXI];
         SpiceInt             ivals2 [MAXI];
         SpiceInt             j;
         SpiceInt             k;
         SpiceInt             n;
         SpiceInt             ncomch;

         SpiceBoolean         found;

         /.
         Set the internal file name.  Don't reserve characters in
         the DAS comment area.
         ./
         ifname = "Example DLA file for testing";
         ncomch = 0;

         /.
         Open a new DLA file.
         ./
         dlaopn_c ( DLA, "DLA", ifname, ncomch, &handle );

         /.
         Begin a new segment.
         ./
         dlabns_c ( handle );

         /.
         Add character data to the segment.
         ./
         for ( i = 0; i < MAXC; i++ )
         {
            for ( j = 0; j < LNSIZE; j++ )
            {

               /.
               Remove null-terminating character added to `buffer' in order to
               add it to the DLA file.
               ./
               k = ( (j+i+1) % 10 );
               snprintf(buffer, 2, "%d", k);
               cvals[i][j] = buffer[0];
            }
         }

         dasadc_c ( handle, MAXC*LNSIZE, 0, LNSIZE-1, LNSIZE, cvals );

         /.
         Add integer and double precision data to the segment.
         ./
         for ( i = 0; i < MAXI; i++ )
         {
            ivals[i] = i+1;
         }

         dasadi_c ( handle, MAXI, ivals );

         for ( i = 0; i < MAXD; i++ )
         {
            dvals[i] = (double)i+1;
         }

         dasadd_c ( handle, MAXD, dvals );

         /.
         End the segment.
         ./
         dlaens_c ( handle );

         /.
         Close the file.  The routine dascls_c flushes the DAS
         buffers and segregates the file before closing it.
         ./
         dascls_c ( handle );

         /.
         Now read the file and check the data.
         ./
         dasopr_c ( DLA, &handle );

         /.
         Obtain the segment descriptor for the sole segment
         in the file. We need not check the found flag
         in this case because we know there is one segment
         in the file.
         ./
         dlabfs_c ( handle, &descr, &found );

         /.
         Fetch character data from the segment.  Obtain the
         base address of the character data and the
         character count from the descriptor.
         ./
         base = descr.cbase;
         n    = descr.csize;

         dasrdc_c ( handle, base+1, base+n, 0, LNSIZE-1, LNSIZE, cvals2 );

         /.
         Display the character data.
         ./
         printf( "\n" );
         printf( "Character array:\n" );

         for ( i = 0; i < n/LNSIZE; i++ )
         {
            /.
            Add null-terminating character to `cvals2[i]' in order to
            print it to the screen.
            ./
            strncpy( cvastr, cvals2[i], LNSIZE );
            cvastr[LNSIZE] = '\0';

            printf( "%s\n", cvastr );
         }

         /.
         Fetch and display the integer and double precision data.
         ./
         base = descr.ibase;
         n    = descr.isize;

         dasrdi_c ( handle, base+1, base+n, ivals2 );

         printf( "\n" );
         printf( "Integer array:\n" );
         for ( i = 0; i < n/10; i++ )
         {
            for ( j = 0; j < 10; j++ )
            {
               printf( "%6d", ivals2[i*10+j] );
            }
            printf( "\n" );
         }

         base = descr.dbase;
         n    = descr.dsize;

         dasrdd_c ( handle, base+1, base+n, dvals2 );

         printf( "\n" );
         printf( "Double precision array:\n" );
         for ( i = 0; i < n/10; i++ )
         {
            for ( j = 0; j < 10; j++ )
            {
               printf( "%6.1f", dvals2[i*10+j] );
            }
            printf( "\n" );
         }

         /.
         Close the file.  This step is unnecessary in this
         program, but is a good practice in general
         because closing the file frees resources.
         ./
         dascls_c ( handle );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Character array:
      1234567890123456789012345678901234567890123456789012345678901
      2345678901234567890123456789012345678901234567890123456789012
      3456789012345678901234567890123456789012345678901234567890123
      4567890123456789012345678901234567890123456789012345678901234
      5678901234567890123456789012345678901234567890123456789012345

      Integer array:
           1     2     3     4     5     6     7     8     9    10
          11    12    13    14    15    16    17    18    19    20
          21    22    23    24    25    26    27    28    29    30
          31    32    33    34    35    36    37    38    39    40
          41    42    43    44    45    46    47    48    49    50
          51    52    53    54    55    56    57    58    59    60
          61    62    63    64    65    66    67    68    69    70
          71    72    73    74    75    76    77    78    79    80
          81    82    83    84    85    86    87    88    89    90
          91    92    93    94    95    96    97    98    99   100

      Double precision array:
         1.0   2.0   3.0   4.0   5.0   6.0   7.0   8.0   9.0  10.0
        11.0  12.0  13.0  14.0  15.0  16.0  17.0  18.0  19.0  20.0
        21.0  22.0  23.0  24.0  25.0  26.0  27.0  28.0  29.0  30.0
        31.0  32.0  33.0  34.0  35.0  36.0  37.0  38.0  39.0  40.0
        41.0  42.0  43.0  44.0  45.0  46.0  47.0  48.0  49.0  50.0


      Note that after run completion, a new DLA file exists in the
      output directory.

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.0.0, 19-OCT-2021 (JDR)

-Index_Entries

   open a new DLA file
   open a new DLA file with write access

-&
*/

{ /* Begin dlaopn_c */

   /*
   Participate in error tracing.
   */
   chkin_c ( "dlaopn_c" );

   /*
   Check the input string arguments:

      fname
      ftype
      ifname

   Make sure each pointer is non-null and each string contains
   at least one data character: that is, one character
   preceding the null terminator.
   */
   CHKFSTR ( CHK_STANDARD, "dlaopn_c", fname  );
   CHKFSTR ( CHK_STANDARD, "dlaopn_c", ftype  );
   CHKFSTR ( CHK_STANDARD, "dlaopn_c", ifname );

   /*
   Call the f2c'd Fortran routine.
   */
   dlaopn_ (  ( char       * )  fname,
              ( char       * )  ftype,
              ( char       * )  ifname,
              ( integer    * ) &ncomch,
              ( integer    * )  handle,
              ( ftnlen       )  strlen(fname),
              ( ftnlen       )  strlen(ftype),
              ( ftnlen       )  strlen(ifname)  );

   chkout_c ( "dlaopn_c" );

} /* End dlaopn_c */
