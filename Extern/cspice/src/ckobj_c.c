/*

-Procedure ckobj_c ( CK objects )

-Abstract

   Find the set of ID codes of all objects in a specified CK file.

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
   CK
   DAF
   NAIF_IDS
   SETS

-Keywords

   POINTING
   UTILITY

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"


   void ckobj_c ( ConstSpiceChar  * ckfnm,
                  SpiceCell       * ids )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   ckfnm      I   Name of CK file.
   ids       I-O  Set of ID codes of objects in CK file.

-Detailed_Input

   ckfnm       is the name of a C-kernel.

   ids         is an initialized SPICE set data structure. `ids'
               optionally may contain a set of ID codes on input; on
               output, the data already present in `ids' will be combined
               with ID code set found for the file `ckfnm'.

               If `ids' contains no data on input, its size and
               cardinality still must be initialized.

               `ids' must be declared as an integer SpiceCell.

               CSPICE provides the following macro, which declares and
               initializes the cell

                  SPICEINT_CELL           ( ids, IDSSZ );

               where IDSSZ is the maximum capacity of `ids'.

-Detailed_Output

   ids         is a SPICE set data structure which contains the union
               of its contents upon input with the set of ID codes of
               each object for which pointing data are present in the
               indicated CK file. The elements of SPICE sets are
               unique; hence each ID code in `ids' appears only once, even
               if the CK file contains multiple segments for that ID
               code.

               See the -Examples section below for a complete example
               program showing how to retrieve the ID codes from `ids'.

-Parameters

   None.

-Exceptions

   1)  If the input file has transfer format, the error
       SPICE(INVALIDFORMAT) is signaled by a routine in the call tree
       of this routine.

   2)  If the input file is not a transfer file but has architecture
       other than DAF, the error SPICE(INVALIDARCHTYPE) is signaled
       by a routine in the call tree of this routine.

   3)  If the input file is a binary DAF file of type other than CK,
       the error SPICE(INVALIDFILETYPE) is signaled by a routine in
       the call tree of this routine.

   4)  If the CK file cannot be opened or read, an error is signaled
       by a routine in the call tree of this routine.

   5)  If the size of the output set argument `ids' is insufficient to
       contain the actual number of ID codes of objects covered by
       the indicated CK file, an error is signaled by a routine in
       the call tree of this routine.

   6)  If the `ckfnm' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   7)  If the `ckfnm' input string has zero length, the error
       SPICE(EMPTYSTRING) is signaled.

   8)  If the `ids' cell argument has a type other than SpiceInt, the
       error SPICE(TYPEMISMATCH) is signaled.

-Files

   This routine reads a C-kernel.

-Particulars

   This routine provides an API via which applications can determine
   the set of objects for which there are pointing data in a
   specified CK file.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.


   1) Display the interval-level coverage for each object in a
      specified CK file. Use tolerance of zero ticks. Do not
      request angular velocity. Express the results in the TDB time
      system.

      Find the set of objects in the file. Loop over the contents
      of the ID code set: find the coverage for each item in the
      set and display the coverage.


      Example code begins here.


      /.
         Program ckobj_ex1
      ./

      #include <stdio.h>
      #include "SpiceUsr.h"

      int main()
      {

         /.
         Local parameters
         ./
         #define  FILSIZ         256
         #define  MAXIV          100000
         #define  WINSIZ         ( 2 * MAXIV )
         #define  TIMLEN         51
         #define  MAXOBJ         1000

         /.
         Local variables
         ./
         SPICEDOUBLE_CELL        ( cover, WINSIZ );
         SPICEINT_CELL           ( ids,   MAXOBJ );

         SpiceChar               ckfnm   [ FILSIZ ];
         SpiceChar               lsk     [ FILSIZ ];
         SpiceChar               sclk    [ FILSIZ ];
         SpiceChar               timstr  [ TIMLEN ];

         SpiceDouble             b;
         SpiceDouble             e;

         SpiceInt                i;
         SpiceInt                j;
         SpiceInt                niv;
         SpiceInt                obj;


         /.
         Load a leapseconds kernel and SCLK kernel for output time
         conversion.  Note that we assume a single spacecraft clock is
         associated with all of the objects in the CK.
         ./
         prompt_c ( "Name of leapseconds kernel > ", FILSIZ, lsk );
         furnsh_c ( lsk );

         prompt_c ( "Name of SCLK kernel        > ", FILSIZ, sclk );
         furnsh_c ( sclk );

         /.
         Get name of CK file.
         ./
         prompt_c ( "Name of CK file            > ", FILSIZ, ckfnm );

         /.
         Find the set of objects in the CK file.
         ./
         ckobj_c ( ckfnm, &ids );

         /.
         We want to display the coverage for each object. Loop over
         the contents of the ID code set, find the coverage for
         each item in the set, and display the coverage.
         ./
         for ( i = 0;  i < card_c( &ids );  i++  )
         {
            /.
            Find the coverage window for the current object.
            Empty the coverage window each time so we don't
            include data for the previous object.
            ./
            obj  =  SPICE_CELL_ELEM_I( &ids, i );

            scard_c ( 0,  &cover );
            ckcov_c ( ckfnm,       obj,  SPICEFALSE,
                      "INTERVAL",  0.0,  "TDB",       &cover );

            /.
            Get the number of intervals in the coverage window.
            ./
            niv = wncard_c( &cover );

            /.
            Display a simple banner.
            ./
            printf ( "%s\n", "========================================" );

            printf ( "Coverage for object %d\n", (int)obj );

            /.
            Convert the coverage interval start and stop times to TDB
            calendar strings.
            ./
            for ( j = 0;  j < niv;  j++  )
            {
               /.
               Get the endpoints of the jth interval.
               ./
               wnfetd_c ( &cover, j, &b, &e );

               /.
               Convert the endpoints to TDB calendar
               format time strings and display them.
               ./
               timout_c ( b,
                          "YYYY MON DD HR:MN:SC.###### (TDB) ::TDB",
                          TIMLEN,
                          timstr                                    );

               printf ( "\n"
                        "Interval:  %d\n"
                        "Start:     %s\n",
                        (int)j,
                        timstr            );

               timout_c ( e,
                          "YYYY MON DD HR:MN:SC.###### (TDB) ::TDB",
                          TIMLEN,
                          timstr                                    );
               printf ( "Stop:      %s\n", timstr );

            }
            printf ( "%s\n", "========================================" );

         }
         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, using the LSK file named naif0010.tls, the SCLK file
      named cas00145.tsc and the CK file named 08052_08057ra.bc, the
      output was:


      Name of leapseconds kernel > naif0010.tls
      Name of SCLK kernel        > cas00145.tsc
      Name of CK file            > 08052_08057ra.bc
      ========================================
      Coverage for object -82000

      Interval:  0
      Start:     2008 FEB 21 00:01:07.771186 (TDB)
      Stop:      2008 FEB 23 22:53:30.001738 (TDB)

      Interval:  1
      Start:     2008 FEB 23 22:58:13.999732 (TDB)
      Stop:      2008 FEB 24 02:22:25.913175 (TDB)

      Interval:  2
      Start:     2008 FEB 24 02:27:49.910886 (TDB)
      Stop:      2008 FEB 24 19:46:33.470587 (TDB)

      Interval:  3
      Start:     2008 FEB 24 19:49:33.469315 (TDB)
      Stop:      2008 FEB 25 04:25:21.250677 (TDB)

      Interval:  4
      Start:     2008 FEB 25 04:29:33.248897 (TDB)
      Stop:      2008 FEB 25 15:23:44.971594 (TDB)

      Interval:  5
      Start:     2008 FEB 25 15:24:12.971396 (TDB)
      Stop:      2008 FEB 25 20:25:04.843864 (TDB)

      Interval:  6
      Start:     2008 FEB 25 20:25:48.843553 (TDB)
      Stop:      2008 FEB 26 00:01:04.752306 (TDB)
      ========================================


-Restrictions

   1)  If an error occurs while this routine is updating the set
       `ids', the set may be corrupted.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.1.0, 24-AUG-2021 (JDR)

       Changed input argument name "ck" to "ckfnm" for consistency with other
       routines.

       Edited the header to comply with NAIF standard. Added solution using
       CASSINI data. Corrected short error message in entries #2 and #3 in
       -Exceptions section.

       Extended description of argument "ids" in -Detailed_Input to include
       type and preferred declaration method.

   -CSPICE Version 1.0.2, 12-JUL-2016 (EDW)

       Edit to example program to use "%d" with explicit casts
       to int for printing SpiceInts with printf.

   -CSPICE Version 1.0.1, 30-NOV-2007 (NJB)

       Corrected bug in example program in header:
       program now empties result window prior to collecting
       data for each object. Updated example to use wncard_c
       rather than card_c.

   -CSPICE Version 1.0.0, 30-DEC-2004 (NJB)

-Index_Entries

   find id codes of objects in CK file

-&
*/

{ /* Begin ckobj_c */


   /*
   Participate in error tracing.
   */
   if ( return_c() )
   {
      return;
   }
   chkin_c ( "ckobj_c" );

   /*
   Check the input string `ckfnm' to make sure the pointer is non-null
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "ckobj_c", ckfnm );

   /*
   Make sure cell data type is SpiceInt.
   */
   CELLTYPECHK ( CHK_STANDARD, "ckobj_c", SPICE_INT, ids );

   /*
   Initialize the cell if necessary.
   */
   CELLINIT ( ids );

   /*
   Call the f2c'd Fortran routine.
   */
   ckobj_ ( ( char       * ) ckfnm,
            ( integer    * ) (ids->base),
            ( ftnlen       ) strlen(ckfnm)   );

   /*
   Sync the output cell.
   */
   if ( !failed_c() )
   {
      zzsynccl_c ( F2C, ids );
   }


   chkout_c ( "ckobj_c" );

} /* End ckobj_c */
