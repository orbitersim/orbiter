/*

-Procedure card_c ( Cardinality of a cell )

-Abstract

   Return the cardinality (current number of elements) in a
   cell of any data type.

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
   SETS
   WINDOWS

-Keywords

   CELLS
   SETS
   WINDOWS

*/

   #include "SpiceUsr.h"
   #include "SpiceZmc.h"

   SpiceInt card_c ( SpiceCell  * cell )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   cell       I   Input cell.

   The function returns the cardinality of the input cell.

-Detailed_Input

   cell        is a SPICE cell of character, double precision, or integer data
               type.

               `cell' must be declared as a character, double precision or
               integer SpiceCell.

               CSPICE provides the following macros, which declare and
               initialize the cell

                  SPICECHAR_CELL          ( cell, CELLSZ, CELLMLEN );
                  SPICEDOUBLE_CELL        ( cell, CELLSZ );
                  SPICEINT_CELL           ( cell, CELLSZ );

               where CELLSZ is the maximum capacity of `cell' and CELLMLEN is
               the maximum length of any member in the character cell.

-Detailed_Output

   The function returns the cardinality of (current number of elements in) the
   input cell.

-Parameters

   None.

-Exceptions

   1)  If the input cell has invalid cardinality, the error
       SPICE(INVALIDCARDINALITY) is signaled. card_c returns an
       unspecified value in this case.

   2)  If the input cell has invalid size, the error SPICE(INVALIDSIZE)
       is signaled. card_c returns an unspecified value in this case.

   3)  If the input cell does not have a recognized data type, the error
       SPICE(NOTSUPPORTED) is signaled by a routine in the call tree of
       this routine. card_c returns an unspecified value in this case.

-Files

   None.

-Particulars

   This is a generic function which may be used on SpiceCells of
   character, double precision, or integer data type.

-Examples

   The numerical results shown for these examples may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) The size_c function is typically used in conjunction
      with the card_c function to predict (and subsequently
      avoid) overflows when manipulating cells. In the following
      example, card_c and size_c are used to determine whether
      the union of two sets is safe.


      Example code begins here.


      /.
         Program card_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"


      int main( )
      {

         /.
         Local constants.
         ./
         #define SETDIM       5

         /.
         Local variables.

         Declare two SPICE cells.
         ./
         SPICEINT_CELL      ( seta, SETDIM );
         SPICEINT_CELL      ( setb, SETDIM );
         SPICEINT_CELL      ( setc, SETDIM );

         SpiceInt             i;

         /.
         Create a list of items and even numbers.
         ./
         SpiceInt             even   [SETDIM] = {  0,  2,  4,  4,  8 };

         SpiceInt             items  [SETDIM] = {  0,  1,  1,  2,  8 };

         /.
         Initialize the empty sets.
         ./
         valid_c ( SETDIM, 0, &seta );
         valid_c ( SETDIM, 0, &setb );

         /.
         Insert `even' on `seta' and `items' on `setb'
         ./
         for ( i = 0; i < SETDIM; i++ )
         {
            insrti_c ( even[i],  &seta );
            insrti_c ( items[i], &setb );
         }

         /.
         Perform the union if possible.
         ./
         if ( card_c ( &seta ) + card_c ( &setb ) <= size_c ( &setc ) )
         {
            printf( "Union will not overflow the output set.\n" );
            union_c ( &seta, &setb, &setc );
         }
         else
         {
            printf( "Union may overflow...\n" );
            inter_c ( &seta, &setb, &setc );

            if ( card_c ( &seta ) + card_c ( &setb ) - card_c ( &setc ) <=
                 size_c ( &setc ) )
            {
               printf( "   ... but inter_c indicates it is safe!\n" );
               union_c ( &seta, &setb, &setc );
            }
            else
            {
               printf( "  ... and inter_c confirms it!\n" );
            }

         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Union may overflow...
         ... but inter_c indicates it is safe!


   2) The cardinality function cspice_card is also typically used to
      process each of the elements of a cell, set or window. In this
      example, we will build a double precision cell via a series of
      calls to appndd_c. Create a set from this set by calling
      valid_c and display the results.


      Example code begins here.


      /.
         Program card_ex2
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main()
      {
         /.
         Local parameters.

         SETSIZ is the maximum capacity of the set.
         ./
         #define SETSIZ          1000000

         /.
         INISIZ will be the initial number of elements in the set.
         ./
         #define INISIZ          10

         /.
         Declare the set.
         ./
         SPICEDOUBLE_CELL ( dpSet, SETSIZ );

         /.
         Other local variables.
         ./
         SpiceDouble             item;

         SpiceInt                i;

         /.
         Initialize the cell's data array.  We use bogus values to
         simplify the example.
         ./
         for ( i = INISIZ-1;  i >= 0;  i-- )
         {
            appndd_c (  (SpiceDouble)(i),  &dpSet  );
         }

         printf( "Initial cell:\n" );
         for ( i = 0; i < card_c( &dpSet ); i++ )
         {
            item  =  SPICE_CELL_ELEM_D( &dpSet, i );
            printf( "  %f\n", item );
         }

         /.
         After the append, does the cell key as a set?
         ./
         if ( dpSet.isSet )
         {
            printf( "Cell is a set after append.\n" );
         }
         else
         {
            printf( "Cell is not a set after append.\n" );
         }

         /.
         Validate the set.  The elements of the set will be arranged
         in increasing order after this call.
         ./
         valid_c ( SETSIZ, INISIZ, &dpSet );

         /.
         Output the elements of the set.
         ./
         printf( "Cell after valid_c call:\n" );
         for ( i = 0; i < card_c( &dpSet ); i++ )
         {
            item  =  SPICE_CELL_ELEM_D( &dpSet, i );
            printf( "  %f\n", item );
         }

         /.
         After the valid_c call, does the cell key as a set?
         ./
         if ( dpSet.isSet )
         {
            printf( "Cell is a set after validate.\n" );
         }
         else
         {
            printf( "Cell is not a set after validate.\n" );
         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Initial cell:
        9.000000
        8.000000
        7.000000
        6.000000
        5.000000
        4.000000
        3.000000
        2.000000
        1.000000
        0.000000
      Cell is not a set after append.
      Cell after valid_c call:
        0.000000
        1.000000
        2.000000
        3.000000
        4.000000
        5.000000
        6.000000
        7.000000
        8.000000
        9.000000
      Cell is a set after validate.


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   C.A. Curzon         (JPL)
   J. Diaz del Rio     (ODC Space)
   H.A. Neilan         (JPL)
   W.L. Taber          (JPL)
   I.M. Underwood      (JPL)

-Version

   -CSPICE Version 1.0.1, 27-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard. Added complete
       code examples; first one based on existing example fragments.

       Added SETS and WINDOWS to the list of required readings and
       keywords.

       Extended description of argument "cell" in -Detailed_Input to include
       type and preferred declaration method.

       Added entry #3 to -Exceptions section.

   -CSPICE Version 1.0.0, 06-AUG-2002 (NJB) (CAC) (HAN) (WLT) (IMU)

-Index_Entries

   cardinality of an integer cell

-&
*/

{ /* Begin card_c */


   /*
   Participate in error tracing.
   */
   if ( return_c() )
   {
      return ( cell->card );
   }
   chkin_c ( "card_c" );


   /*
   Initialize the cell if necessary.
   */
   CELLINIT ( cell );


   /*
   Check the size and cardinality of the input cell.
   */
   if (  cell->size < 0  )
   {
      setmsg_c ( "Invalid cell size.  The size was #." );
      errint_c ( "#", cell->size                       );
      sigerr_c ( "SPICE(INVALIDSIZE)"                  );
      chkout_c ( "card_c"                              );

      return   ( cell->card );
   }

   else if ( cell->card < 0 )
   {
      setmsg_c ( "Invalid cell cardinality.  The "
                 "cardinality was #."                  );
      errint_c ( "#", cell->card                       );
      sigerr_c ( "SPICE(INVALIDCARDINALITY)"           );
      chkout_c ( "card_c"                              );

      return   ( cell->card );
   }

   else if ( cell->card  >  cell->size )
   {
      setmsg_c ( "Invalid cell cardinality; cardinality exceeds "
                 " cell size.  The cardinality was #.  The size "
                 " was #."                                        );
      errint_c ( "#", cell->card                                  );
      errint_c ( "#", cell->size                                  );
      sigerr_c ( "SPICE(INVALIDCARDINALITY)"                      );
      chkout_c ( "card_c"                                         );

      return   ( cell->card );
   }


   chkout_c ( "card_c" );

   return ( cell->card );


} /* End card_c */
