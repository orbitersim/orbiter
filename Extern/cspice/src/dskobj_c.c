/*

-Procedure dskobj_c ( DSK, get object IDs )

-Abstract

   Find the set of body ID codes of all objects for which
   topographic data are provided in a specified DSK file.

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

   CELLS
   DAS
   DSK
   SETS
   NAIF_IDS

-Keywords

   COVERAGE
   TOPOGRAPHY

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"
   #undef dskobj_c

   void dskobj_c ( ConstSpiceChar   * dskfnm,
                   SpiceCell        * bodids )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   dskfnm     I   Name of DSK file.
   bodids    I-O  Set of ID codes of objects in DSK file.

-Detailed_Input

   dskfnm      is the name of a DSK file. This file will be opened for
               read access by this routine.

   bodids      is an initialized SPICE set data structure.

               `bodids' optionally may contain a set of body ID codes on
               input; on output, the data already present in `bodids' will
               be combined with ID code set found for the file `dskfnm'.

               If `bodids' contains no data on input, its size and
               cardinality still must be initialized.

               `bodids' must be declared as an integer SpiceCell.

               CSPICE provides the following macro, which declares and
               initializes the cell

                  SPICEINT_CELL           ( bodids, BODIDSSZ );

               where BODIDSSZ is the maximum capacity of `bodids'.

-Detailed_Output

   bodids      is a SPICE set data structure that contains the union
               of its contents upon input with the set of body ID codes
               of segments in the indicated DSK file.

               The elements of SPICE sets are unique; each ID code in
               `bodids' appears only once, even if the DSK file contains
               multiple segments for that ID code.

               See the -Examples section below for a complete example
               program showing how to retrieve the body and surface ID
               codes from a DSK file.

-Parameters

   None.

-Exceptions

   1)  If the input file has transfer format, the error
       SPICE(INVALIDFORMAT) is signaled by a routine in the call tree
       of this routine.

   2)  If the input file is not a transfer file but has architecture
       other than DAS, the error SPICE(INVALIDARCHTYPE) is signaled
       by a routine in the call tree of this routine.

   3)  If the input file is a binary DAS file of type other than DSK,
       the error SPICE(INVALIDFILETYPE) is signaled by a routine in
       the call tree of this routine.

   4)  If the DSK file cannot be opened or read, an error is signaled
       by a routine in the call tree of this routine.

   5)  If the size of the output set argument `bodids' is insufficient
       to contain the actual number of ID codes of objects covered by
       the indicated DSK file, the error SPICE(CELLTOOSMALL) is
       signaled by a routine in the call tree of this routine.

   6)  If the `dskfnm' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   7)  If the `dskfnm' input string has zero length, the error
       SPICE(EMPTYSTRING) is signaled.

   8)  If the `bodids' cell argument has a type other than SpiceInt,
       the error SPICE(TYPEMISMATCH) is signaled.

-Files

   See the description of the argument `dskfnm' above.

-Particulars

   This routine provides an API via which applications can determine
   the set of objects for which there are topographic data in a
   specified DSK file.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Display the coverage for each object in a specified DSK file.
      Find the set of objects in the file. Loop over the contents
      of the ID code set: find the surface ID for each item in the
      set and display the surface ID.


      Example code begins here.


      /.
         Program dskobj_ex1
      ./

      #include <stdio.h>
      #include "SpiceUsr.h"

      /.
      Examine a DSK file and identify the set of
      central bodies associated with the segments
      in the file. For each body, find the
      set of surfaces associated with that body.
      ./
      int main()
      {
         /.
         Local constants
         ./
         #define FILSIZ          256
         #define MAXID           10000

         /.
         Local variables
         ./
         SPICEINT_CELL         ( bodids, MAXID );
         SPICEINT_CELL         ( srfids, MAXID );

         SpiceChar               dskfnm [ FILSIZ ];

         SpiceInt                i;
         SpiceInt                j;

         /.
         Prompt for the name of a DSK file.
         ./
         prompt_c ( "Enter name of DSK file > ", FILSIZ, dskfnm );

         dskobj_c ( dskfnm, &bodids );

         for ( i = 0;  i < card_c( &bodids );  i++ )
         {
            printf ( "\n"
                     "Body ID:     %d\n",
                     (int)SPICE_CELL_ELEM_I( &bodids, i) );

            /.
            Get the surface IDs for the Ith body.
            ./
            dsksrf_c ( dskfnm, SPICE_CELL_ELEM_I( &bodids, i), &srfids );

            for ( j = 0;  j < card_c( &srfids );  j++ )
            {
               printf ( "   Surface ID:  %d\n",
                        (int)SPICE_CELL_ELEM_I( &srfids, j) );
            }
         }
         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, using the DSK file named phobos512.bds, the output
      was:


      Enter name of DSK file > phobos512.bds

      Body ID:     401
         Surface ID:  401


-Restrictions

   1)  If an error occurs while this routine is updating the set
       `bodids', the set may be corrupted.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.1.0, 24-AUG-2021 (JDR)

       Changed input argument name "dsk" to "dskfnm" for consistency with other
       routines.

       Edited the header to comply with NAIF standard. Corrected short error
       message in entries #2, #3 and #5 in -Exceptions section. Changed the DKS
       file used in the code example's solution. Added NAIF_IDS to the
       -Required_Reading section.

       Extended description of argument "bodids" in -Detailed_Input to include
       type and preferred declaration method.

   -CSPICE Version 1.0.0, 27-JAN-2016 (NJB)

-Index_Entries

   find id codes of ephemeris objects in DSK file
   find id codes of bodies in DSK file

-&
*/

{ /* Begin dskobj_c */



   /*
   Participate in error tracing.
   */
   chkin_c ( "dskobj_c" );

   /*
   Check the input string to make sure the pointer is non-null and
   the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "dskobj_c", dskfnm );

   /*
   Make sure cell data type is integer.
   */
   CELLTYPECHK ( CHK_STANDARD, "dskobj_c", SPICE_INT, bodids );

   /*
   Initialize the cell if necessary.
   */
   CELLINIT ( bodids );

   /*
   The f2c'd routine will operate on the integer array associated
   with the input cell.
   */
   dskobj_ ( (char     *) dskfnm,
             (integer  *) bodids->base,
             (ftnlen    ) strlen(dskfnm)   );

   /*
   Sync the cell.
   */
   if ( !failed_c() )
   {
      zzsynccl_c ( F2C, bodids );
   }

   chkout_c ( "dskobj_c" );

} /* End dskobj_c */
