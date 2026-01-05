/*

-Procedure ekuef_c  ( EK, unload event file )

-Abstract

   Unload an EK file, making its contents inaccessible to the
   EK reader routines, and clearing space in order to allow other
   EK files to be loaded.

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

   EK

-Keywords

   EK
   FILES

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"

   void ekuef_c ( SpiceInt handle )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   handle     I   Handle of EK file.

-Detailed_Input

   handle      is a file handle returned by eklef_c.

-Detailed_Output

   None.

   See -Particulars for a description of the effect of this routine.

-Parameters

   None.

-Exceptions

   1)  Unloading a file that is not loaded has no effect.

-Files

   This routine unloads a binary EK file from the EK query system.

-Particulars

   This routine removes information about an EK file from the
   EK system, freeing space to increase the number of other EK
   files that can be loaded. The file is also unloaded from
   the DAS system and closed.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Load two EK files and perform a query on them. During query
      execution, all files will be searched. Unload the previous
      file before each new file is loaded. Unloading files prevents
      them from being searched during query execution.

      Use the EK kernel below to load the Cassini Science Plan
      SPICE E-Kernel File based upon the integrated science
      plan #78.

         S78_CIMSSSUPa.bep

      Use the EK kernel below to load the data based upon the
      integrated science plan #79.

         S79_CIMSSSUPa.bep


      Example code begins here.


      /.
         Program ekuef_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local parameters.
         ./
         #define EKNMLN       18
         #define ERRLEN       1841

         /.
         Local variables
         ./
         SpiceChar            errmsg [ERRLEN];
         SpiceChar          * query;

         SpiceInt             handle;
         SpiceInt             i;
         SpiceInt             nmrows;

         SpiceBoolean         error;

         /.
         Set up the array holding the EK file names.
         ./
         SpiceChar            eknams [2][EKNMLN] = {
                                   "S78_CIMSSSUPa.bep", "S79_CIMSSSUPa.bep" };

         /.
         The EK files contain a table "CASSINI_SP_OBSERVATION",
         that contains columns named:

            NOTES, OBSERVATION_ID, OBSERVATION_TITLE,
            OBS_DESCRIPTION, SCIENCE_OBJECTIVE, SEQUENCE,
            SUBSYSTEM

         Define a set of constraints to perform a query on all
         loaded EK files (the SELECT clause).
         ./
         query = "Select SUBSYSTEM, SCIENCE_OBJECTIVE, OBSERVATION_ID from "
                 "CASSINI_SP_OBSERVATION order by SUBSYSTEM";

         /.
         Load the EK files. This call could be replaced by a call
         to furnsh_c.
         ./
         for ( i = 0; i < 2; i++ )
         {

            eklef_c ( eknams[i], &handle );
            printf( "Loading EK: %s\n", eknams[i] );

            /.
            Query the EK system for data rows matching the
            SELECT constraints.
            ./
            ekfind_c ( query, ERRLEN, &nmrows, &error, errmsg );

            /.
            Check whether an error occurred while processing the
            SELECT clause. If so, output the error message.
            ./
            if ( error )
            {
               printf( "SELECT clause error: %s\n", errmsg );
            }
            else
            {
               /.
               If no error, `nmrows' contains the number of rows
               matching the constraints specified in the query
               string.
               ./
               printf( "Number of matching rows:  %2d\n", nmrows );
            }

            /.
            Unload the current file. Unloading files prevents
            them from being searched during query execution.
            ./
            ekuef_c ( handle );
            printf( "Unloading EK: %s\n", eknams[i] );
            printf( "\n" );

         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Loading EK: S78_CIMSSSUPa.bep
      Number of matching rows:   4
      Unloading EK: S78_CIMSSSUPa.bep

      Loading EK: S79_CIMSSSUPa.bep
      Number of matching rows:   5
      Unloading EK: S79_CIMSSSUPa.bep


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.0.1, 10-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard. Added complete
       code example.

   -CSPICE Version 1.0.0, 26-JUL-1998 (NJB)

      Based on SPICELIB Version 1.0.1, 07-JUL-1996 (NJB)

-Index_Entries

   unload EK file

-&
*/

{ /* Begin ekuef_c */

   /*
   Participate in error tracing.
   */
   chkin_c ( "ekuef_c" );


   /*
   Call the f2c'd Fortran routine.
   */
   ekuef_ ( (integer *) &handle );


   chkout_c ( "ekuef_c" );

} /* End ekuef_c */
