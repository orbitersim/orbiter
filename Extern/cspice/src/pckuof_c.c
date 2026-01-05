/*

-Procedure pckuof_c ( PCK, unload binary file )

-Abstract

   Unload a binary PCK file so that it will no longer be searched by
   the readers.

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
   PCK

-Keywords

   FILES
   PCK

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"

   void pckuof_c ( SpiceInt handle )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   handle     I   Handle of file to be unloaded

-Detailed_Input

   handle      is the integer handle assigned to the PCK file upon
               loading.

-Detailed_Output

   None.

-Parameters

   None.

-Exceptions

   1)  Unloading a file that has not been loaded is a no-op.
       No error is signaled.

-Files

   The file referred to by `handle' is unloaded.

-Particulars

   A PCK file is removed from consideration during a search by the
   readers by a call to pckuof_c.

   The file table entry corresponding to the file referenced by
   `handle' is removed and the file is closed. Any segment table
   entry which came from the specified file is also deleted.

   If the file specified by `handle' is not currently loaded in the
   PCK system, no action is taken.

-Examples

   The numerical results shown for these examples may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Unload a binary PCK kernel specified by an integer handle, making
      room to load another PCK.

         pckuof_c ( handle );


   2) Load a high precision PCK file for the Earth and compute the
      position transformation matrix from ITRF93 to J2000 at
      2000 Jan 01 12:00:00 TDB.

      Use the PCK kernel below to load the required triaxial
      ellipsoidal shape model and orientation data for the Earth.

         earth_720101_070426.bpc


      Example code begins here.


      /.
         Program pckuof_ex2
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main()
      {

         /.
         Local variables.
         ./
         SpiceDouble             xform  [3][3];

         SpiceInt                handle;
         SpiceInt                i;

         /.
         Open the PCK for read access. This call may be replaced (as
         recommended by NAIF) by furnsh_c.
         ./
         pcklof_c ( "earth_720101_070426.bpc", &handle );

         /.
         Find the position transformation matrix at

            2000 Jan 01 12:00:00 TDB

         which corresponds to ephemeris time 0.
         ./
         pxform_c( "ITRF93", "J2000", 0.0, xform );

         /.
         Display the results.
         ./
         printf ("Position transformation from ITRF93 to J2000 frame:\n\n" );
         for ( i = 0; i < 3; i++ )
         {
            printf("%20.10f %19.10f %19.10f\n",
                    xform[i][0], xform[i][1], xform[i][2] );
         }

         /.
         Close the PCK file. This call may be replaced (as
         recommended by NAIF) by unload_c, if furnsh_c has
         been used to load the file.
         ./
         pckuof_c ( handle );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Position transformation from ITRF93 to J2000 frame:

              0.1769805935        0.9842143409       -0.0000251874
             -0.9842143410        0.1769805928       -0.0000274792
             -0.0000225878        0.0000296531        0.9999999993


   3) The following example extracts the first 20 lines of the
      comment area of a binary PCK, displaying the comments on
      the terminal screen.


      Example code begins here.


      /.
         Program pckuof_ex3
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main()
      {

         /.
         Local parameters.
         ./
         #define FILSIZ          256
         #define LINLEN          1001
         #define BUFFSZ          20

         /.
         Local variables.
         ./
         SpiceBoolean            done;

         SpiceChar               pcknam [FILSIZ];
         SpiceChar               buffer [BUFFSZ][LINLEN];

         SpiceInt                handle;
         SpiceInt                i;
         SpiceInt                n;


         prompt_c ( "Enter name of PCK > ", FILSIZ, pcknam );

         /.
         Open the PCK for read access. This operation could have
         been done with dafopr_c.
         ./
         pcklof_c ( pcknam, &handle );

         /.
         Extract up to 20 lines from the comment area of the
         loaded PCK file and display them on the terminal screen.
         ./
         dafec_c ( handle, BUFFSZ, LINLEN, &n, buffer, &done );

         for ( i = 0;  i < n;  i++ )
         {
               printf ( "%s\n", buffer[i] );
         }

         /.
         Close the PCK file. This operation could have been done
         with dafcls_c.
         ./
         pckuof_c ( handle );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, using the hight precision PCK file for the Earth named
      earth_720101_070426.bpc as input PCK file, the output was:


      Enter name of PCK > earth_720101_070426.bpc


      Binary "High Accuracy" Earth PCK File
      ======================================

      Created 27-APR-2007 by NJB (NAIF/JPL)
      Original file name:   earth_720101_070426.bpc

      Data Source

         Input file:           EOP file 2007_04_26_long.eop
                               (Copied from WWW URL
                               http://epic.jpl.nasa.gov/nav/eop/latest.long)

      Coverage

         ET Start time:             1972 JAN 01 00:00:42.183
         ET Stop time:              2007 APR 26 00:01:05.185

         UTC Epoch of last datum:   26-APR-2007


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   K.S. Zukor          (JPL)

-Version

   -CSPICE Version 1.0.1, 10-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard. Added complete
       code examples.

       Improved the documentation of -Keywords, -Exceptions, -Particulars
       and -Index_Entries sections.

   -CSPICE Version 1.0.0, 08-FEB-1998 (NJB) (KSZ)

       Based on SPICELIB Version 1.0.0, 16-MAR-1994 (KSZ)

-Index_Entries

   unload PCK file

-&
*/

{ /* Begin pckuof_c */

   /*
   Participate in error tracing.
   */
   chkin_c ( "pckuof_c" );

   /*
   Call the f2c'd Fortran routine.
   */
   pckuof_ ( ( integer * ) &handle );


   chkout_c ( "pckuof_c" );


} /* End pckuof_c */
