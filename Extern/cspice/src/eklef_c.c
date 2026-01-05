/*

-Procedure eklef_c ( EK, load event file )

-Abstract

   Load an EK file, making it accessible to the EK readers.

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
   SEARCH

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"

   void eklef_c ( ConstSpiceChar  * fname,
                  SpiceInt        * handle )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   fname      I   Name of EK file to load.
   handle     O   File handle of loaded EK file.

-Detailed_Input

   fname       is the name of a binary EK file to be loaded.

-Detailed_Output

   handle      is the handle of the EK file. The file is
               accessible by the EK reader routines once it
               has been loaded.

-Parameters

   None.

-Exceptions

   1)  If the EK file indicated by `fname' contains a column whose name
       matches that of a column in an already loaded EK, but whose
       declared attributes don't match those of the loaded column of
       the same name, the error SPICE(BADATTRIBUTES) is signaled by a
       routine in the call tree of this routine. `handle' is is
       undefined in this case.

   2)  Loading an EK file that is already loaded does not cause side
       effects. The handle already associated with the file will be
       returned.

   3)  If a file open error occurs, the error is signaled by a
       routine in the call tree of this routine. `handle' is undefined
       in this case.

   4)  If loading the input file would cause the maximum number of
       loaded EK files to be exceeded, the error
       SPICE(EKFILETABLEFULL) is signaled by a routine in the call
       tree of this routine. `handle' is undefined in this case. This
       routine will attempt to unload the file from the DAS system.

   5)  If loading the input file would cause the maximum number of
       loaded DAS files to be exceeded, an error is signaled by a
       routine in the call tree of this routine. `handle' is undefined
       in this case. This routine will attempt to unload the file
       from the DAS system.

   6)  If loading the input file would cause the maximum number of
       segments allowed in loaded EK files to be exceeded, the error
       SPICE(EKSEGMENTTABLEFULL) is signaled by a routine in the call
       tree of this routine. `handle' is undefined in this case. This
       routine will attempt to unload the file from the DAS system.

   7)  If loading the input file would cause the maximum number of
       columns allowed in loaded EK files to be exceeded, the error
       SPICE(EKCOLDESCTABLEFULL) is signaled by a routine in the call
       tree of this routine. `handle' is undefined in this case. This
       routine will attempt to unload the file from the DAS system.

   8)  If loading the input file would cause the maximum allowed
       number of columns having distinct attributes in loaded EK
       files to be exceeded, the error SPICE(EKCOLATTRTABLEFULL) is
       signaled by a routine in the call tree of this routine. `handle'
       is undefined in this case. This routine will attempt to unload
       the file from the DAS system.

   9)  If loading the input file would cause the maximum number of
       instrument codes allowed in loaded EK files to be exceeded,
       the error SPICE(EKIDTABLEFULL) is signaled by a routine in the
       call tree of this routine. `handle' is undefined in this case.
       This routine will attempt to unload the file from the DAS
       system.

   10) If the input file does not contain at least one segment, the
       error SPICE(EKNOSEGMENTS) is signaled by a routine in the call
       tree of this routine.

   11) If the `fname' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   12) If the `fname' input string has zero length, the error
       SPICE(EMPTYSTRING) is signaled.

-Files

   See description of `fname' in -Detailed_Input.

-Particulars

   This routine makes EK files known to the EK system. It is
   necessary to load EK files using this routine in order to
   query the files using the EK readers.

-Examples

   The numerical results shown for these examples may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Load two EK files and perform a query on them. During query
      execution, all files will be searched.

      Use the EK kernel below to load the Cassini Science Plan
      SPICE E-Kernel File based upon the integrated science
      plan #78.

         S78_CIMSSSUPa.bep

      Use the EK kernel below to load the data based upon the
      integrated science plan #79.

         S79_CIMSSSUPa.bep


      Example code begins here.


      /.
         Program eklef_ex1
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
         Load the EK files. This call could be replaced by a call
         to furnsh_c (in this case, a meta-kernel listing the EKs
         to be loaded could also be used).
         ./
         for ( i = 0; i < 2; i++ )
         {

            eklef_c ( eknams[i], &handle );
            printf( "Loading EK: %s\n", eknams[i] );

         }

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
         Query the EK system for data rows matching the
         SELECT constraints.
         ./
         ekfind_c ( query, ERRLEN, &nmrows, &error, errmsg );

         /.
         Check whether an error occurred while processing the
         SELECT clause. If so, output the error message.
         ./
         printf( "\n" );
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

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Loading EK: S78_CIMSSSUPa.bep
      Loading EK: S79_CIMSSSUPa.bep

      Number of matching rows:   9


   2) Repeat the previous exercise, using the same input kernels,
      but this time unloading the previous file before each new
      file is loaded. Unloading files prevents them from being
      searched during query execution.


      Example code begins here.


      /.
         Program eklef_ex2
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

   1)  EK files containing columns having the same name but
       inconsistent declarations are not diagnosed. Such kernels
       are invalid in any case.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.0.1, 10-AUG-2021 (NJB) (JDR)

       Edited the header to comply with NAIF standard. Added complete
       code examples based on existing fragments.

   -CSPICE Version 1.0.0, 26-MAR-1998 (NJB)

       Based on SPICELIB Version 1.0.1, 07-JUL-1996 (NJB)

-Index_Entries

   load EK file
   load E-Kernel

-&
*/

{ /* Begin eklef_c */


   /*
   Participate in error tracing.
   */
   chkin_c ( "eklef_c" );

   /*
   Check the file name to make sure the pointer is non-null
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "eklef_c", fname );

   /*
   Call the f2c'd Fortran routine.  Use explicit type casts for every
   type defined by f2c.
   */
   eklef_ (  ( char     * )  fname,
             ( integer  * )  handle,
             ( ftnlen     )  strlen(fname)  );


   chkout_c ( "eklef_c" );

} /* End eklef_c */
