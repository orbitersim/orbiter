/*

-Procedure unload_c ( Unload a kernel )

-Abstract

   Unload a SPICE kernel.

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

   KERNEL

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"


   void unload_c ( ConstSpiceChar  * file )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   file       I   The name of a kernel to unload.

-Detailed_Input

   file        is the name of a file to unload. This file
               should be one loaded through the interface furnsh_c.
               If the file is not on the list of loaded kernels
               no action is taken.

               Note that if file is a meta-text kernel, all of
               the files loaded as a result of loading the meta-text
               kernel will be unloaded.

-Detailed_Output

   None.

-Parameters

   None.

-Exceptions

   1)  If the specified kernel is not on the list of loaded kernels
       no action is taken.

   2)  If the `file' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   3)  If the `file' input string has zero length, the error
       SPICE(EMPTYSTRING) is signaled.

-Files

   None.

-Particulars

   The call

      unload_c ( file );

   has the effect of "erasing" the last previous call:

      furnsh_c ( file );

   This interface allows you to unload binary and text kernels.
   Moreover, if you used a meta-text kernel to set up your
   working environment, you can unload all of the kernels loaded
   through the meta-kernel by unloading the meta-kernel.


   Unloading Text Kernels or Meta-Kernels
   --------------------------------------

   Part of the action of unloading text (or meta-text kernels) is
   clearing the kernel pool and re-loading any kernels that were not in
   the specified set of kernels to unload. Since loading of text
   kernels is not a very fast process, unloading text kernels takes
   considerably longer than unloading binary kernels. Moreover, since
   the kernel pool is cleared, any kernel pool variables you have set
   from your program by using one of the interfaces pcpool_c, pdpool_c,
   pipool_c, or lmpool_c will be removed from the kernel pool. For
   this reason, if you plan to use this feature in your program,
   together with one of the routines specified above, you will need to
   take special precautions to make sure kernel pool variables required
   by your program do not inadvertently disappear.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Load a meta-kernel with a PCK, an LSK and an SPK, and
      separately, a text kernel and a binary PCK. Loop over the
      loaded kernels, outputting file information for each of
      them.

      Then unload the text kernels, check that they have been
      unloaded, and finally unload the meta-kernel.


      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File name: unload_ex1.tm

         This meta-kernel is intended to support operation of SPICE
         example programs. The kernels shown here should not be
         assumed to contain adequate or correct versions of data
         required by SPICE-based user applications.

         In order for an application to use this meta-kernel, the
         kernels referenced here must be present in the user's
         current working directory.

         The names and contents of the kernels referenced
         by this meta-kernel are as follows:

            File name                     Contents
            ---------                     --------
            de421.bsp                     Planetary ephemeris
            pck00009.tpc                  Planet orientation and
                                          radii
            naif0012.tls                  Leapseconds

         \begindata

            KERNELS_TO_LOAD = ( 'de421.bsp',
                                'naif0012.tls',
                                'pck00009.tpc' )

         \begintext

         End of meta-kernel


      Use the PCK kernel below as the binary PCK required for the
      example.

         earth_latest_high_prec.bpc


      Use the FK kernel below as the text kernel required for the
      example.

         RSSD0002.TF


      Example code begins here.


      /.
         Program unload_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local constants.
         ./
         #define FNAMLN       256
         #define FTYPLN       33
         #define SRCLEN       256

         /.
         Local variables.
         ./
         SpiceChar            file   [FNAMLN];
         SpiceChar            filtyp [FTYPLN];
         SpiceChar            srcfil [SRCLEN];

         SpiceInt             count;
         SpiceInt             handle;

         SpiceBoolean         found;

         /.
         Load several kernel files.
         ./
         furnsh_c ( "unload_ex1.tm"              );
         furnsh_c ( "RSSD0002.TF"                );
         furnsh_c ( "earth_latest_high_prec.bpc" );

         /.
         Count the number of loaded kernel files.
         ./
         ktotal_c ( "ALL", &count );

         printf( "The total number of kernels after final furnsh_c:  %1d\n",
                                                                     count );
         printf( "\n" );

         /.
         Unload the text kernels.
         ./
         ktotal_c ( "TEXT", &count );

         printf( "\n" );
         printf( "Unloading %1d text kernels...\n", count );
         printf( "\n" );

         while ( count > 0 )
         {

            kdata_c ( 0,    "TEXT", FNAMLN,  FTYPLN,  SRCLEN,
                      file, filtyp, srcfil, &handle, &found  );

            /.
            If the kernel is found in the pool, unload it.
            ./
            if ( found )
            {

               unload_c ( file );

               /.
               Check if the file has been unloaded.
               ./
               kinfo_c ( file,    FTYPLN,  SRCLEN, filtyp,
                         srcfil, &handle, &found          );

               if ( found )
               {
                  printf( "  Error"  );
               }
               else
               {
                  printf( "  Success" );
               }

               printf( " unloading %s\n", file );

            }

            /.
            Something is not working. Inform NAIF.
            ./
            else
            {
               printf( " ERROR: No kernel found but ktotal_c returns %d\n",
                                                                     count );
            }

            /.
            Check if we have more text kernels to unload from
            the kernel pool. Note that unloading a text kernel
            or meta-kernel implies that the kernel pool is
            cleared, and any kernel[s - 1] that were not to be
            unloaded are re-loaded. Therefore the `count' value
            changes, and the indexing of the files within the
            kernel pool too.
            ./
            ktotal_c ( "TEXT", &count );

         }

         ktotal_c ( "ALL", &count );

         printf( " \n" );
         printf( "The total number of kernels after unload_c calls:  %1d\n",
                                                                     count );

         /.
         Clear the KEEPER system, retrieve the number of loaded
         after the clear.
         ./
         unload_c ( "unload_ex1.tm" );

         ktotal_c ( "ALL", &count );

         printf( " \n" );
         printf( "The total number of kernels after final unload_c:  %1d\n",
                                                                     count );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      The total number of kernels after final furnsh_c:  6


      Unloading 3 text kernels...

        Success unloading naif0012.tls
        Success unloading pck00009.tpc
        Success unloading RSSD0002.TF

      The total number of kernels after unload_c calls:  3

      The total number of kernels after final unload_c:  1


-Restrictions

   1)  See the note regarding the unloading of text and meta-text
       kernels.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   W.L. Taber          (JPL)

-Version

   -CSPICE Version 1.0.1, 03-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard.
       Created complete code example from existing code fragments.

       Re-ordered header sections.

   -CSPICE Version 1.0.0, 01-SEP-1999 (NJB) (WLT)

-Index_Entries

   Unload a SPICE kernel

-&
*/

{ /* Begin unload_c */



   /*
   Participate in error tracing.
   */

   chkin_c ( "unload_c" );


   /*
   Check the input filename to make sure the pointer is non-null
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "unload_c", file );


   /*
   Call the f2c'd Fortran routine.
   */
   unload_ ( ( char   * ) file,
             ( ftnlen   ) strlen(file) );


   chkout_c ( "unload_c" );

} /* End unload_c */
