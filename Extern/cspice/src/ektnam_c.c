/*

-Procedure ektnam_c  ( EK, return name of loaded table )

-Abstract

   Return the name of a specified, loaded table.

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
   #include "SpiceZst.h"
   #include "SpiceZmc.h"

   void ektnam_c ( SpiceInt     n,
                   SpiceInt     tablen,
                   SpiceChar  * table  )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   n          I   Index of table.
   tablen     I   Maximum table name length.
   table      O   Name of table.

-Detailed_Input

   n           is the index of the table whose name is desired.
               The value of `n' ranges from 0 to the number of
               loaded tables minus 1, which count may be obtained
               from ekntab_c.

   tablen      is the maximum allowed table name length, including
               space for the terminating null character. Normally
               the caller should allow enough room for
               SPICE_EK_TSTRLN characters; this parameter is
               declared in the header SpiceEK.h.

-Detailed_Output

   table       is the name of the n'th loaded table. If `table'
               is too small to accommodate the name, the name will
               be truncated on the right.

-Parameters

   SPICE_EK_TSTRLN

               is the maximum allowed table name length, including
               space for the terminating null character. See the
               header file SpiceEK.h for the actual value of
               this parameter.

-Exceptions

   1)  If this routine is called when no files are loaded, the error
       SPICE(NOLOADEDFILES) is signaled by a routine in the call tree
       of this routine.

   2)  If the input `n' is out of range, the error SPICE(INVALDINDEX)
       is signaled by a routine in the call tree of this routine.

   3)  If the `table' output string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   4)  If the `table' output string has length less than two
       characters, the error SPICE(STRINGTOOSHORT) is signaled, since
       the output string is too short to contain one character of
       output data plus a null terminator.

-Files

   The returned name is based on the currently loaded EK files.

-Particulars

   This routine is a utility that provides the caller with the
   name of a specified loaded table. The index of a table with
   a given name depends on the kernels loaded and possibly on
   the order in which the files have been loaded.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Dump the names of all the loaded tables.

      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File name: ektnam_ex1.tm

         This meta-kernel is intended to support operation of SPICE
         example programs. The kernels shown here should not be
         assumed to contain adequate or correct versions of data
         required by SPICE-based user applications.

         In order for an application to use this meta-kernel, the
         kernels referenced here must be present in the user's
         current working directory.

         The names and contents of the kernels referenced
         by this meta-kernel are as follows:

            File name                 Contents
            ---------                 --------
            S78_CIMSSSUPa.bep         Cassini Science Plan #78
            S79_CIMSSSUPa.bep         Cassini Science Plan #79
            S79_status_pf.bes         Cassini Spacecraft Sequence
                                      Status #79


         \begindata

            KERNELS_TO_LOAD = ( 'S78_CIMSSSUPa.bep',
                                'S79_CIMSSSUPa.bep',
                                'S79_status_pf.bes'  )

         \begintext

         End of meta-kernel


      Example code begins here.


      /.
         Program ektnam_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local parameters.
         ./
         #define META         "ektnam_ex1.tm"

         /.
         Local variables.
         ./
         SpiceChar            tabnam [SPICE_EK_TSTRLN];

         SpiceInt             ntab;
         SpiceInt             tab;

         /.
         Load the EK files. Use a meta-kernel for convenience.
         ./
         furnsh_c ( META );

         /.
         Get the number of loaded tables. The count refers to the
         number of logical tables; if multiple EKs contain data
         for the same table, these EKs collectively contribute
         only one table to the count.
         ./
         ekntab_c ( &ntab );

         printf( "Number of tables in EK subsystem: %2d\n", ntab );

         for ( tab = 0; tab < ntab; tab++ )
         {

            /.
            Get the name of the current table, and display it.
            ./
            ektnam_c ( tab, SPICE_EK_TSTRLN, tabnam );
            printf( "   TABLE = %s\n", tabnam );

         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Number of tables in EK subsystem:  4
         TABLE = CASSINI_SP_REQUEST
         TABLE = CASSINI_SP_OBSERVATION
         TABLE = CASSINI_SP_REQ_OBS
         TABLE = CASSINI_STATUS


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.1.0, 02-AUG-2021 (JDR)

       Changed the input argument name "lenout" to "tablen" for consistency
       with other routines.

       Edited the header to comply with NAIF standard. Added complete
       code example.

       Added description of SPICE_EK_TSTRLN parameter.

   -CSPICE Version 1.0.1, 26-MAR-2003 (NJB)

       Fixed description of exception (5): replaced "tablen-1"
       with "tablen." Removed spurious word "clock" from string
       description.

   -CSPICE Version 1.0.0, 07-JAN-2002 (NJB)

-Index_Entries

   return name of a loaded table

-&
*/

{ /* Begin ektnam_c */


   /*
   Participate in error tracing.
   */
   chkin_c ( "ektnam_c" );


   /*
   Make sure the output table has at least enough room for one output
   character and a null terminator.  Also check for a null pointer.
   */
   CHKOSTR ( CHK_STANDARD, "ektnam_c", table, tablen );

   /*
   Map the table index to a Fortran-style index.
   */
   n++;

   ektnam_ (  ( integer * ) &n,
              ( char    * ) table,
              ( ftnlen    ) tablen-1  );

   /*
   Convert the Fortran string to a C string by placing a null
   after the last non-blank character.  This operation is valid
   whether or not the CSPICE routine signaled an error.
   */
   F2C_ConvertStr ( tablen, table );


   chkout_c ( "ektnam_c" );

} /* End ektnam_c */
