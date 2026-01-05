/*

-Procedure pcklof_c ( PCK, load binary file )

-Abstract

   Load a binary PCK file for use by the readers. Return the
   handle of the loaded file which is used by other PCK routines to
   refer to the file.

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
   #include "SpiceZst.h"
   #include "SpiceZmc.h"


   void pcklof_c ( ConstSpiceChar * fname,
                   SpiceInt       * handle )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   fname      I   Name of the file to be loaded.
   handle     O   Loaded file's handle.

-Detailed_Input

   fname       is the character name of the file to be loaded.

-Detailed_Output

   handle      is the integer handle assigned to the file upon loading.
               Other PCK routines will subsequently use this number to
               refer to the file.

-Parameters

   None.

-Exceptions

   1)  If an attempt is made to open more DAF files than is
       specified by the parameter FTSIZE in DAF system, an error
       is signaled by a routine in the call tree of this routine.

   2)  If an attempt is made to load more files than is specified by
       the parameter FTSIZE in the PCK subsystem, and if the DAF
       system has room to load another file, the error
       SPICE(PCKFILETABLEFULL) is signaled by a routine in the call
       tree of this routine. The current setting of FTSIZE does not
       allow this situation to arise: the DAF system will trap the
       error before this routine has the chance.

   3)  This routine makes use of DAF file system routines and is
       subject to all of the constraints imposed by the DAF file
       system. See the DAF Required Reading daf.req or individual DAF
       routines for details.

   4)  If the `fname' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   5)  If the `fname' input string has zero length, the error
       SPICE(EMPTYSTRING) is signaled.

-Files

   A file specified by `fname', to be loaded. The file is assigned a
   handle by pcklof_c, which will be used by other routines to refer
   to it.

-Particulars

   If there is room for a new file in the file table, pcklof_c creates
   an entry for it, and opens the file for reading.

   Also, if the file table is empty, pcklof_c initializes it.

-Examples

   The numerical results shown for these examples may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Load a high precision PCK file for the Earth and compute the
      position transformation matrix from ITRF93 to J2000 at
      2000 Jan 01 12:00:00 TDB.

      Use the PCK kernel below to load the required triaxial
      ellipsoidal shape model and orientation data for the Earth.

         earth_720101_070426.bpc


      Example code begins here.


      /.
         Program pcklof_ex1
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
            printf("%19.10f %19.10f %19.10f\n",
                    xform[i][0], xform[i][1], xform[i][2] );
         }

         /.
         Close the PCK file. This call may be replaced (as
         recommended by NAIF) by unload_c.
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


   2) The following example extracts the first 20 lines of the
      comment area of a binary PCK, displaying the comments on
      the terminal screen.


      Example code begins here.


      /.
         Program pcklof_ex2
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
   E.D. Wright         (JPL)
   K.S. Zukor          (JPL)

-Version

   -CSPICE Version 2.1.0, 01-NOV-2021 (JDR)

       Changed input argument name "filename" to "fname" for
       consistency with other routines.

       Edited the header with NAIF standard. Added complete
       code examples.

       Moved the reference to DAF required reading from
       -Literature_References to -Required_Reading.

   -CSPICE Version 2.0.1, 20-MAR-1998 (EDW)

       Minor correction to header.

   -CSPICE Version 2.0.0, 08-FEB-1998 (NJB)

       Input argument "filename" was changed to type ConstSpiceChar *.

       Re-implemented routine without dynamically allocated, temporary
       strings.

   -CSPICE Version 1.0.0, 25-OCT-1997 (EDW) (KSZ)

-Index_Entries

   load PCK file

-&
*/

{ /* Begin pcklof_c */


   /*
   Participate in error tracing.
   */
   chkin_c ( "pcklof_c" );


   /*
   Check the input string `fname' to make sure the pointer is non-null
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "pcklof_c", fname );


   /*
   Call the f2c'd Fortran routine.
   */
   pcklof_ ( ( char       * )  fname,
             ( integer    * )  handle,
             ( ftnlen       )  strlen(fname)    );


   chkout_c ( "pcklof_c" );


} /* End pcklof_c */
