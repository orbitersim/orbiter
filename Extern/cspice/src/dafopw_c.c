/*

-Procedure dafopw_c ( DAF, open for write )

-Abstract

   Open a DAF for subsequent write requests.

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

   DAF

-Keywords

   DAF
   FILES

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"
   #include "SpiceZst.h"


   void dafopw_c ( ConstSpiceChar  * fname,
                   SpiceInt        * handle )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   fname      I   Name of DAF to be opened.
   handle     O   Handle assigned to DAF.

-Detailed_Input

   fname       is the name of a DAF to be opened with write access.

-Detailed_Output

   handle      is the file handle associated with the file. This handle
               is used to identify the file in subsequent calls to other
               DAF routines.

-Parameters

   None.

-Exceptions

   1)  If the specified file has already been opened, either by the
       DAF routines or by other code, an error is signaled by a
       routine in the call tree of this routine. Note that this
       response is not paralleled by dafopr_c, which allows you to open
       a DAF for reading even if it is already open for reading.

   2)  If the specified file cannot be opened without exceeding the
       maximum number of files, the error SPICE(DAFFTFULL) is
       signaled by a routine in the call tree of this routine.

   3)  If the attempt to read the file's file record fails, the error
       SPICE(FILEREADFAILED) is signaled by a routine in the call
       tree of this routine.

   4)  If the specified file is not a DAF file, an error is signaled
       by a routine in the call tree of this routine.

   5)  If no logical units are available, an error is signaled by a
       routine in the call tree of this routine.

   6)  If the file does not exist, an error is signaled by a routine
       in the call tree of this routine.

   7)  If an I/O error occurs in the process of opening the file, the
       error is signaled by a routine in the call tree of this
       routine.

   8)  If the file name is blank or otherwise inappropriate, an error
       is signaled by a routine in the call tree of this routine.

   9)  If the file was transferred improperly via FTP, an error is
       signaled by a routine in the call tree of this routine.

   10) If the file utilizes a non-native binary file format, an error
       is signaled by a routine in the call tree of this routine.

   11) If the `fname' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   12) If the `fname' input string has zero length, the error
       SPICE(EMPTYSTRING) is signaled.

-Files

   See argument `fname'.

-Particulars

   Most DAFs require only read access. If you do not need to
   change the contents of a file, you should open it with dafopr_c.
   Use dafopw_c when you need to

      -- change (update) one or more summaries, names, or
         arrays within a file; or

      -- add new arrays to a file.

-Examples

   The numerical results shown for these examples may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Delete the entire comment area of a DAF file. Note that this
      action should only be performed if fresh new comments are to
      be placed within the DAF file.

      Use the SPK kernel below as input DAF file for the program.

         earthstns_itrf93_201023.bsp


      Example code begins here.


      /.
         Program dafopw_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {
         /.
         Local parameters
         ./
         #define KERNEL       "earthstns_itrf93_201023.bsp"
         #define BUFFSZ       10
         #define LINLEN       1000

         /.
         Local variables.
         ./
         SpiceChar            buffer [BUFFSZ][LINLEN];

         SpiceInt             handle;
         SpiceInt             i;
         SpiceInt             n;

         SpiceBoolean         done;

         /.
         Open a DAF for write. Return a `handle' referring to the
         file.
         ./
         dafopw_c ( KERNEL, &handle );

         /.
         Print the first 10 lines of comments from the DAF file.
         ./
         printf( "Comment area of input DAF file (max. 10 lines): \n" );
         printf( "--------------------------------"
                 "------------------------------\n" );

         dafec_c ( handle, BUFFSZ, LINLEN, &n, buffer, &done );

         for ( i = 0; i < n; i++ )
         {
            printf( "%s\n", buffer[i] );
         }

         printf( "--------------------------------"
                 "------------------------------\n" );
         printf( " \n" );
         printf( "Deleting entire comment area...\n" );

         /.
         Delete all the comments from the DAF file.
         ./
         dafdc_c ( handle );

         /.
         Close the DAF file and re-open it for read
         access to work around the dafec_c restriction
         on comments not to be modified while they are
         being extracted.
         ./
         dafcls_c ( handle );

         dafopr_c ( KERNEL, &handle );

         /.
         Check if the comments have indeed been deleted.
         ./
         dafec_c ( handle, BUFFSZ, LINLEN, &n, buffer, &done );

         if ( done  &&  ( n == 0 ) )
         {
            printf( " \n" );
            printf( "   Successful operation.\n" );
         }
         else
         {
            printf( " \n" );
            printf( "   Operation failed.\n" );
         }

         /.
         Safely close the DAF.
         ./
         dafcls_c ( handle );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Comment area of input DAF file (max. 10 lines):
      --------------------------------------------------------------

         SPK for DSN Station Locations
         =====================================================================

         Original file name:                   earthstns_itrf93_201023.bsp
         Creation date:                        2020 October 28 12:30
         Created by:                           Nat Bachman  (NAIF/JPL)


         Introduction
      --------------------------------------------------------------

      Deleting entire comment area...

         Successful operation.


   2) In the following code example, dafopw_c is used to open a
      file, which is then searched for arrays containing data for
      a particular object. The code for the object is then changed
      (perhaps to reflect some new convention).


      Example code begins here.


      /.
         Program dafopw_ex2
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main()
      {
         /.
         Local parameters.
         ./
         #define  ARCHLN         4
         #define  DSCSIZ         5
         #define  FILSIZ         256
         #define  KTYPLN         5
         #define  LINSIZ         81
         #define  MAXOBJ         1000
         #define  ND             2
         #define  NI             6

         /.
         Local variables.
         ./
         SPICEINT_CELL         ( ids,   MAXOBJ );

         SpiceBoolean            found;

         SpiceChar               arch       [ ARCHLN ];
         SpiceChar               fname      [ FILSIZ ];
         SpiceChar               ktype      [ KTYPLN ];
         SpiceChar               line       [ LINSIZ ];

         SpiceDouble             dc         [ ND ];
         SpiceDouble             sum        [ DSCSIZ ];

         SpiceInt                handle;
         SpiceInt                i;
         SpiceInt                ic         [ NI ];
         SpiceInt                new_code;
         SpiceInt                obj;
         SpiceInt                old_code;


         /.
         Get the input data.
         ./
         prompt_c ( "Enter name of existing DAF > ", FILSIZ, fname );

         prompt_c ( "Enter ID code to change    > ", LINSIZ, line  );
         prsint_c ( line, &old_code );

         prompt_c ( "Enter replacement code     > ", LINSIZ, line  );
         prsint_c ( line, &new_code );

         /.
         Open the existing DAF file for write access.
         ./
         dafopw_c ( fname, &handle );

         /.
         Start a forward search through the file.
         ./
         dafbfs_c ( handle );

         /.
         Find the first array (segment).
         ./
         daffna_c ( &found );

         while ( found )
         {
            /.
            Read and unpack the current DAF array summary
            (aka segment descriptor) sum:
            ./
            dafgs_c ( sum );
            dafus_c ( sum, ND, NI, dc, ic );


            if ( ic[0] == old_code )
            {
               ic[0] = new_code;

               /.
               Re-pack the descriptor; replace the descriptor
               in the file.
               ./
               dafps_c ( ND, NI, dc, ic, sum );
               dafrs_c ( sum );

            }

            /.
            Find the next segment.
            ./
            daffna_c ( &found );
         }

         /.
         Close the DAF.
         ./
         dafcls_c ( handle );

         /.
         Find the set of objects in the DAF file.
         ./
         getfat_c ( fname, ARCHLN, KTYPLN, arch, ktype );

         if ( eqstr_c ( ktype, "spk" ) )
         {
            spkobj_c ( fname, &ids );
         }
         else if ( eqstr_c ( ktype, "ck" ) )
         {
            ckobj_c  ( fname, &ids );
         }
         else if ( eqstr_c ( ktype, "pck" ) )
         {
            pckfrm_c ( fname, &ids );
         }

         printf( "Objects in the DAF file:\n\n" );
         for ( i = 0;  i < card_c( &ids );  i++  )
         {
            obj  =  SPICE_CELL_ELEM_I( &ids, i );

            printf( "  %d", (int)obj );
         }
         printf( "\n" );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, using the SPK file named de430.bsp as input
      DAF file and requesting to change the Moon ID code "301"
      to another (fake) ID, "-999", the output was:


      Enter name of existing DAF > de430.bsp
      Enter ID code to change    > 301
      Enter replacement code     > -999
      Objects in the DAF file:

        -999  1  2  3  4  5  6  7  8  9  10  199  299  399


-Restrictions

   1)  Only files of the native binary file format may be opened
       with this routine.

   2)  Files opened using this routine must be closed with dafcls_c.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   K.R. Gehringer      (JPL)
   J.M. Lynch          (JPL)
   J.E. McLean         (JPL)
   W.L. Taber          (JPL)
   F.S. Turner         (JPL)
   I.M. Underwood      (JPL)

-Version

   -CSPICE Version 1.0.1, 25-NOV-2021 (JDR)

       Edited the header to comply with NAIF standard.

       Extended code example to generate outputs, provide example's solution
       and replaced f2c'ed functions by CSPICE wrappers. Moved DAF required
       reading from -Literature_References to -Required_Reading section.

   -CSPICE Version 1.0.0, 13-OCT-2004 (NJB) (KRG) (JML) (JEM) (WLT) (FST) (IMU)

-Index_Entries

   open existing DAF for write

-&
*/

{ /* Begin dafopw_c */


   /*
   Participate in error tracing.
   */
   if ( return_c() )
   {
      return;
   }
   chkin_c ( "dafopw_c" );

   /*
   Check the file name to make sure the pointer is non-null
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "dafopw_c", fname );

   /*
   Let the f2c'd routine do the work.
   */
   dafopw_ ( ( char     * ) fname,
             ( integer  * ) handle,
             ( ftnlen     ) strlen(fname) );


   chkout_c ( "dafopw_c" );

} /* End dafopw_c */
