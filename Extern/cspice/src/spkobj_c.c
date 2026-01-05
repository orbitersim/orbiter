/*

-Procedure spkobj_c ( SPK objects )

-Abstract

   Find the set of ID codes of all objects in a specified SPK file.

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
   DAF
   NAIF_IDS
   SETS
   SPK

-Keywords

   EPHEMERIS
   UTILITY

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"


   void spkobj_c ( ConstSpiceChar  * spkfnm,
                   SpiceCell       * ids )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   spkfnm     I   Name of SPK file.
   ids       I-O  Set of ID codes of objects in SPK file.

-Detailed_Input

   spkfnm      is the name of an SPK file.

   ids         is an initialized SPICE set data structure. `ids'
               optionally may contain a set of ID codes on input; on
               output, the data already present in `ids' will be combined
               with ID code set found for the file `spkfnm'.

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
               each object for which ephemeris data are present in the
               indicated SPK file. The elements of SPICE sets are
               unique; hence each ID code in `ids' appears only once, even
               if the SPK file contains multiple segments for that ID
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

   3)  If the input file is a binary DAF file of type other than SPK,
       the error SPICE(INVALIDFILETYPE) is signaled by a routine in
       the call tree of this routine.

   4)  If the SPK file cannot be opened or read, an error is signaled
       by a routine in the call tree of this routine.

   5)  If the size of the output set argument `ids' is insufficient to
       contain the actual number of ID codes of objects covered by
       the indicated SPK file, an error is signaled by a routine in
       the call tree of this routine.

   6)  If the `spkfnm' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   7)  If the `spkfnm' input string has zero length, the error
       SPICE(EMPTYSTRING) is signaled.

   8)  If the `ids' cell argument has a type other than SpiceInt, the
       error SPICE(TYPEMISMATCH) is signaled.

-Files

   This routine reads an SPK file.

-Particulars

   This routine provides an API via which applications can determine
   the set of objects for which there are ephemeris data in a
   specified SPK file.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) This example demonstrates combined usage of spkobj_c and the
      related SPK utility spkcov_c.

      Display the coverage for each object in a specified SPK file.
      Find the set of objects in the file. Loop over the contents
      of the ID code set: find the coverage for each item in the
      set and display the coverage.


      Example code begins here.


      /.
         Program spkobj_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main()
      {
         /.
         Local parameters
         ./
         #define  FILSIZ         256
         #define  MAXIV          1000
         #define  WINSIZ         ( 2 * MAXIV )
         #define  TIMLEN         51
         #define  MAXOBJ         1000

         /.
         Local variables
         ./
         SPICEDOUBLE_CELL        ( cover, WINSIZ );
         SPICEINT_CELL           ( ids,   MAXOBJ );

         SpiceChar               lsk     [ FILSIZ ];
         SpiceChar               spkfnm  [ FILSIZ ];
         SpiceChar               timstr  [ TIMLEN ];

         SpiceDouble             b;
         SpiceDouble             e;

         SpiceInt                i;
         SpiceInt                j;
         SpiceInt                niv;
         SpiceInt                obj;


         /.
         Load a leapseconds kernel for output time conversion.
         spkcov_c itself does not require a leapseconds kernel.
         ./
         prompt_c ( "Name of leapseconds kernel > ", FILSIZ, lsk );
         furnsh_c ( lsk );

         /.
         Get name of SPK file.
         ./
         prompt_c ( "Name of SPK file           > ", FILSIZ, spkfnm );

         /.
         Find the set of objects in the SPK file.
         ./
         spkobj_c ( spkfnm, &ids );

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

            scard_c  ( 0,        &cover );
            spkcov_c ( spkfnm, obj, &cover );

            /.
            Get the number of intervals in the coverage window.
            ./
            niv = wncard_c ( &cover );

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
                          "YYYY MON DD HR:MN:SC.### (TDB) ::TDB",
                          TIMLEN,
                          timstr                                  );

               printf ( "\n"
                        "Interval:  %d\n"
                        "Start:     %s\n",
                        (int)j,
                        timstr            );

               timout_c ( e,
                          "YYYY MON DD HR:MN:SC.### (TDB) ::TDB",
                          TIMLEN,
                          timstr                                  );
               printf ( "Stop:      %s\n", timstr );

            }

         }
         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, using the LSK file named naif0012.tls and the SPK
      file named mar097.bsp, the output was:


      Name of leapseconds kernel > naif0012.tls
      Name of SPK file           > mar097.bsp
      ========================================
      Coverage for object 3

      Interval:  0
      Start:     1900 JAN 04 00:00:41.184 (TDB)
      Stop:      2100 JAN 01 00:01:07.183 (TDB)
      ========================================
      Coverage for object 4

      Interval:  0
      Start:     1900 JAN 04 00:00:41.184 (TDB)
      Stop:      2100 JAN 01 00:01:07.183 (TDB)
      ========================================
      Coverage for object 10

      Interval:  0
      Start:     1900 JAN 04 00:00:41.184 (TDB)
      Stop:      2100 JAN 01 00:01:07.183 (TDB)
      ========================================
      Coverage for object 399

      Interval:  0
      Start:     1900 JAN 04 00:00:41.184 (TDB)
      Stop:      2100 JAN 01 00:01:07.183 (TDB)
      ========================================
      Coverage for object 401

      Interval:  0
      Start:     1900 JAN 04 00:00:41.184 (TDB)
      Stop:      2100 JAN 01 00:01:07.183 (TDB)
      ========================================
      Coverage for object 402

      Interval:  0
      Start:     1900 JAN 04 00:00:41.184 (TDB)
      Stop:      2100 JAN 01 00:01:07.183 (TDB)
      ========================================
      Coverage for object 499

      Interval:  0
      Start:     1900 JAN 04 00:00:41.184 (TDB)
      Stop:      2100 JAN 01 00:01:07.183 (TDB)


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

       Changed input argument name "spk" to "spkfnm" for consistency with other
       routines.

       Edited the header to comply with NAIF standard. Added example's
       solution. Corrected short error message in entries #2 and #3 in
       -Exceptions section and added entry #8.

       Added NAIF_IDS to -Required_Reading section.

       Extended description of argument "ids" in -Detailed_Input to include
       type and preferred declaration method.

   -CSPICE Version 1.0.3, 14-JUN-2016 (EDW)

       Edit to example program to use "%d" with explicit casts
       to int for printing SpiceInts with printf.

   -CSPICE Version 1.0.2, 01-JUL-2014 (NJB)

       Updated index entries.

   -CSPICE Version 1.0.1, 30-NOV-2007 (NJB)

       Corrected bug in first example program in header:
       program now empties result window prior to collecting
       data for each object. Deleted declaration of unused
       constant NAMLEN. Updated example to use wncard_c
       rather than card_c.

   -CSPICE Version 1.0.0, 30-DEC-2004 (NJB)

-Index_Entries

   find id codes of ephemeris objects in SPK file
   find id codes of bodies in SPK file

-&
*/

{ /* Begin spkobj_c */


   /*
   Participate in error tracing.
   */
   if ( return_c() )
   {
      return;
   }
   chkin_c ( "spkobj_c" );


   /*
   Check the input string `spkfnm' to make sure the pointer is non-null
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "spkobj_c", spkfnm );

   /*
   Make sure cell data type is SpiceInt.
   */
   CELLTYPECHK ( CHK_STANDARD, "spkobj_c", SPICE_INT, ids );

   /*
   Initialize the cell if necessary.
   */
   CELLINIT ( ids );

   /*
   Call the f2c'd Fortran routine.
   */
   spkobj_ ( ( char       * ) spkfnm,
             ( integer    * ) (ids->base),
             ( ftnlen       ) strlen(spkfnm)   );

   /*
   Sync the output cell.
   */
   if ( !failed_c() )
   {
      zzsynccl_c ( F2C, ids );
   }


   chkout_c ( "spkobj_c" );

} /* End spkobj_c */
