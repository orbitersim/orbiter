/*

-Procedure set_c ( Compare sets )

-Abstract

   Compare two sets of any data type, as indicated by a relational operator.

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

-Keywords

   CELLS
   SETS

*/


   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"

   SpiceBoolean set_c (  SpiceCell        * a,
                         ConstSpiceChar   * op,
                         SpiceCell        * b   )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   a          I   First set.
   op         I   Comparison operator.
   b          I   Second set.

   The function returns the result of the comparison: a (op) b.

-Detailed_Input

   a           is a SPICE set.

               `a' must be declared as a character, double precision or
               integer SpiceCell.

               CSPICE provides the following macros, which declare and
               initialize the cell

                  SPICECHAR_CELL          ( a, ASZ, AMLEN );
                  SPICEDOUBLE_CELL        ( a, ASZ );
                  SPICEINT_CELL           ( a, ASZ );

               where ASZ is the maximum capacity of `a' and AMLEN is the
               maximum length of any member in the character cell.

   op          is a comparison operator, indicating the way in
               which the input sets are to be compared. `op' may
               be any of the following:

                  Operator             Meaning
                  --------  -------------------------------------
                    "="     a = b is true if a and b are equal
                            (contain the same elements).

                    "<>"    a <> b is true if a and b are not
                            equal.

                    "<="    a <= b is true if a is a subset of b.

                    "<"     a < b is true if a is a proper subset
                            of b.

                    ">="    a >= b is true if b is a subset of a.

                    ">"     a > b is true if b is a proper subset
                            of a.

                    "&"     a & b is true if a and b have one or more
                            elements in common (the intersection of
                            the two sets in non-empty.)

                    "~"     a ~ b is true if a and b are disjoint
                            sets.

               When comparing elements of character sets, this routine
               ignores trailing blanks.

   b           is a second SPICE set, to be compared to `a'.

               `b' must be declared as a SpiceCell of the same data type
               as `a'.

               CSPICE provides the following macros, which declare and
               initialize the cell

                  SPICECHAR_CELL          ( b, BSZ, BMLEN );
                  SPICEDOUBLE_CELL        ( b, BSZ );
                  SPICEINT_CELL           ( b, BSZ );

               where BSZ is the maximum capacity of `b' and BMLEN is the
               maximum length of any member in the character cell.

-Detailed_Output

   The function returns the result of the comparison: a (op) b.

-Parameters

   None.

-Exceptions

   1)  If the set relational operator is not recognized, the error
       SPICE(INVALIDOPERATION) is signaled by a routine in the call tree
       of this routine.

   2)  If the `op' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   3)  If the `op' input string has zero length, the error
       SPICE(EMPTYSTRING) is signaled.

   4)  If the `a' and `c' cell arguments do not have identical data
       types, the error SPICE(TYPEMISMATCH) is signaled.

   5)  If any of the `a' or `c' cell arguments does not qualify as a
       SPICE set, the error SPICE(NOTASET) is signaled. SPICE sets
       have their data elements stored in increasing order and contain
       no duplicate elements.

   6)  If any of the `a' or `c' cell arguments does not have a
       recognized data type, the error SPICE(NOTSUPPORTED) is signaled.

   7)  If the cell arguments are of type SpiceChar and the string length
       associated with any of them is non-positive or too short to be
       usable when constructing the equivalent SPICE character cells
       required by the wrapped SPICELIB routine, an error is signaled by
       a routine in the call tree of this routine.

-Files

   None.

-Particulars

   None.

-Examples

   1) In the following code fragment, set_c is used to repeat an operation
      for as long as the integer set finished remains a proper
      subset of the integer set planned.

         #include "SpiceUsr.h"
               .
               .
               .
         while (  set_c( &finished, "<", &planned )  )
         {
               .
               .
               .
         }


   2) In the following example, let the integer sets a, b, and c
      contain the elements listed below. Let e be an empty integer
      set.

         a        b        c
        ---      ---      ---
         1        1        1
         2        3        3
         3
         4

   Then all of the following expressions are SPICETRUE.

      set_c ( b, "=",  c )      "b is equal to c"
      set_c ( a, "<>", c )      "a is not equal to c"
      set_c ( a, ">",  b )      "a is a proper superset of b"
      set_c ( b, "<=", c )      "b is a subset of c"
      set_c ( c, "<=", b )      "c is a subset of b"
      set_c ( a, "<=", a )      "a is a subset of a"
      set_c ( e, "<=", b )      "e is a subset of b"
      set_c ( e, "<",  b )      "e is a proper subset of b"
      set_c ( e, "<=", e )      "e is a subset of e"
      set_c ( a, "&",  b )      "a has elements in common with b."
      set_c ( b, "&",  c )      "b has elements in common with c."

   And all of the following are SPICEFALSE.

      set_c ( b, "<>", c )      "b is not equal to c"
      set_c ( a, "=",  c )      "a is equal to c"
      set_c ( a, "<",  b )      "a is a proper subset of b"
      set_c ( b, "<",  c )      "b is a proper subset of c"
      set_c ( b, ">=", a )      "b is a superset of a"
      set_c ( a, ">",  a )      "a is a proper superset of a"
      set_c ( e, ">=", a )      "e is a superset of a"
      set_c ( e, "<",  e )      "e is a proper subset of e"
      set_c ( a, "~",  b )      "a and b are disjoint sets."

-Restrictions

   1)  String comparisons performed by this routine are Fortran-style:
       trailing blanks in the input sets are ignored. This gives
       consistent behavior with CSPICE code generated by the f2c
       translator, as well as with the Fortran SPICE Toolkit.

       Note that this behavior is not identical to that of the ANSI
       C library functions strcmp and strncmp.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   C.A. Curzon         (JPL)
   J. Diaz del Rio     (ODC Space)
   W.L. Taber          (JPL)
   I.M. Underwood      (JPL)

-Version

   -CSPICE Version 1.1.1, 29-OCT-2021 (JDR)

       Edited the header to comply with NAIF standard.

       Extended description of arguments "a", and "b" to include
       type and preferred declaration method.

       Added entries #5 and #6 in -Exceptions section.

   -CSPICE Version 1.1.0, 15-FEB-2005 (NJB)

       Bug fix: loop bound changed from 1 to 2 in loop used
       to free dynamically allocated arrays.

   -CSPICE Version 1.0.0, 08-AUG-2002 (NJB) (CAC) (WLT) (IMU)

-Index_Entries

   compare sets

-&
*/

{ /* Begin set_c */


   /*
   Local variables
   */
   SpiceBoolean            retval;

   SpiceChar             * fCell[2];

   SpiceInt                fLen [2];
   SpiceInt                i;


  /*
   Standard SPICE error handling.
   */

   if ( return_c() )
   {
      return ( SPICEFALSE );
   }
   chkin_c ( "set_c" );


   /*
   Check the input string op to make sure the pointer is non-null
   and the string length is non-zero.
   */
   CHKFSTR_VAL ( CHK_STANDARD, "set_c", op, SPICEFALSE );


   /*
   Make sure data types match.
   */
   CELLMATCH2_VAL ( CHK_STANDARD, "set_c", a, b, SPICEFALSE );


   /*
   Make sure the input cells are sets.
   */
   CELLISSETCHK2_VAL ( CHK_STANDARD, "set_c", a, b, SPICEFALSE );


   /*
   Initialize the cells if necessary.
   */
   CELLINIT2 ( a, b );


   /*
   Call the set routine appropriate for the data type of the cells.
   */
   if ( a->dtype == SPICE_CHR )
   {

      /*
      Construct Fortran-style sets suitable for passing to setc_.
      */
      C2F_MAP_CELL2 ( "set_c",
                      a, fCell,   fLen,
                      b, fCell+1, fLen+1 );


      if ( failed_c() )
      {
         chkout_c ( "set_c" );
         return ( SPICEFALSE );
      }


      retval =  (SpiceBoolean) setc_ ( (char    * )  fCell[0],
                                       (char    * )  op,
                                       (char    * )  fCell[1],
                                       (ftnlen    )  fLen[0],
                                       (ftnlen    )  strlen(op),
                                       (ftnlen    )  fLen[1]     );
      /*
      We're done with the dynamically allocated Fortran-style arrays.
      */
      for ( i = 0;  i < 2;  i++ )
      {
         free ( fCell[i] );
      }
   }

   else if ( a->dtype == SPICE_DP )
   {

      retval =  (SpiceBoolean) setd_ ( (doublereal * ) (a->base),
                                       (char       * )  op,
                                       (doublereal * ) (b->base),
                                       (ftnlen       )  strlen(op)  );
   }

   else if ( a->dtype == SPICE_INT )
   {
      retval =  (SpiceBoolean) seti_ ( (integer * ) (a->base),
                                       (char    * )  op,
                                       (integer * ) (b->base),
                                       (ftnlen    )  strlen(op)  );
   }

   else
   {
      /*
      We get to this point only if we have an invalid cell type.
      */
      setmsg_c ( "Cell a contains unrecognized data type code #." );
      errint_c ( "#",  (SpiceInt) (a->dtype)                      );
      sigerr_c ( "SPICE(NOTSUPPORTED)"                            );
      chkout_c ( "set_c"                                          );
      return   ( SPICEFALSE                                       );
   }


   chkout_c ( "set_c" );
   return   ( retval  );


} /* End set_c */
