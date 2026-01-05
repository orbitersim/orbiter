/*

-Procedure wnreld_c ( Compare two DP windows )

-Abstract

   Compare two double precision windows.

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

   WINDOWS

-Keywords

   WINDOWS

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"

   SpiceBoolean wnreld_c ( SpiceCell       * a,
                           ConstSpiceChar  * op,
                           SpiceCell       * b   )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   a          I   First window.
   op         I   Comparison operator.
   b          I   Second window.

   The function returns the result of comparison: a (op) b.

-Detailed_Input

   a,
   b           are SPICE windows, each of which contains zero or more
               intervals.

               `a' and `b' must be declared as double precision SpiceCells.

               CSPICE provides the following macros, which declare and
               initialize the cells

                  SPICEDOUBLE_CELL        ( a, ASZ );
                  SPICEDOUBLE_CELL        ( b, BSZ );

               where ASZ and BSZ are the maximum capacity of `a' and `b',
               respectively.

   op          is a comparison operator, indicating the way in which the
               input sets are to be compared. `op' may be any of the
               following:

                  Operator             Meaning
                  --------  ----------------------------------------------
                    "="     a = b is SPICETRUE if `a' and `b' are equal
                            (contain the same intervals).

                    "<>"    a <> b is SPICETRUE if `a' and `b' are not
                            equal.

                    "<="    a <= b is SPICETRUE if `a' is a subset of `b'.

                    "<"     a < b is SPICETRUE if `a' is a proper subset
                            of `b'.

                    ">="    a >= b is SPICETRUE if `b' is a subset of `a'.

                    ">"     a > b is SPICETRUE if `b' is a proper subset
                            of `a'.

-Detailed_Output

   The function returns the result of the comparison.

-Parameters

   None.

-Exceptions

   1)  If the relational operator is not recognized, the error
       SPICE(INVALIDOPERATION) is signaled by a routine in the call
       tree of this routine.

   2)  The cardinality of the input windows must be even. Left
       endpoints of stored intervals must be strictly greater than
       preceding right endpoints. Right endpoints must be greater
       than or equal to corresponding left endpoints. Invalid window
       data are not diagnosed by this routine and may lead to
       unpredictable results.

   3)  If the `op' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled. The function returns the value
       SPICEFALSE.

   4)  If the `op' input string has zero length, the error
       SPICE(EMPTYSTRING) is signaled. The function returns the value
       SPICEFALSE.

   5)  If any the `a' or `b' cell arguments has a type other than
       SpiceDouble, the error SPICE(TYPEMISMATCH) is signaled. The
       function returns the value SPICEFALSE.

-Files

   None.

-Particulars

   This function returns SPICETRUE whenever the specified relationship
   between the input windows, `a' and `b', is satisfied. For example,
   the expression

      wnreld_c ( &needed, "<=", &avail )

   is SPICETRUE whenever the window `needed' is a subset of the window
   `avail'. One window is a subset of another window if each of the
   intervals in the first window is included in one of the intervals in the
   second window. In addition, the first window is a proper subset of the
   second if the second window contains at least one point not contained in
   the first window. (Thus, "<" implies "<=", and ">" implies ">=".)

   The following pairs of expressions are equivalent.

      wnreld_c ( &a, ">",  &b )
      wnreld_c ( &b, "<",  &a )

      wnreld_c ( &a, ">=", &b )
      wnreld_c ( &b, "<=", &a )

-Examples

   Let a contain the intervals

      [ 1, 3 ]  [ 7, 11 ]  [ 23, 27 ]

   Let b and c contain the intervals

      [ 1, 2 ]  [ 9, 9 ]  [ 24, 27 ]

   Let d contain the intervals

      [ 5, 10 ]  [ 15, 25 ]

   Finally, let e and f be empty windows (containing no intervals).

   Because b and c contain the same intervals,

      wnreld_c ( &b, "=",  &c )
      wnreld_c ( &b, "<=", &c )
      wnreld_c ( &b, ">=", &c )

   are all SPICETRUE, while

      wnreld_c ( &b, "<>", &c )

   is SPICEFALSE. Because neither b nor c contains any points not also
   contained by the other, neither is a proper subset of the other.
   Thus,

      wnreld_c ( &b, "<", &c )
      wnreld_c ( &b, ">", &c )

   are both SPICEFALSE.

   Every point contained in b and c is also contained in a. Thus,

      wnreld_c ( &b, "<=", &a )
      wnreld_c ( &a, ">=", &c )

   are both SPICETRUE. In addition, a contains points not contained in
   b and c. (That is, the differences a-b and a-c are not empty.)
   Thus, b and c are proper subsets of a as well, and

      wnreld_c ( &b, "<", &a )
      wnreld_c ( &a, ">", &b )

   are both SPICETRUE.

   Although a and d have points in common, neither contains the
   other. Thus

      wnreld_c ( &a, "=",  &d )
      wnreld_c ( &a, "<=", &d )
      wnreld_c ( &a, ">=", &d )

   are all SPICEFALSE.

   In addition, any window is equal to itself, a subset of itself,
   and a superset of itself. Thus,

      wnreld_c ( &a, "=",  &a )
      wnreld_c ( &a, "<=", &a )
      wnreld_c ( &a, ">=", &a )

   are always SPICETRUE. However, no window is a proper subset or a
   proper superset of itself. Thus,

      wnreld_c ( &a, "<", &a )
      wnreld_c ( &a, ">", &a )

   are always SPICEFALSE.

   Finally, an empty window is a proper subset of any window
   except another empty window. Thus,

      wnreld_c ( &e, "<", &a )

   is SPICETRUE, but

      wnreld_c ( &e, "<", &f )

   is SPICEFALSE.

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   H.A. Neilan         (JPL)
   W.L. Taber          (JPL)
   I.M. Underwood      (JPL)

-Version

   -CSPICE Version 1.0.1, 24-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard. Added entry #2
       in -Exceptions section.

       Extended description of arguments "a" and "b" to include
       type and preferred declaration method.

   -CSPICE Version 1.0.0, 27-JUL-2002 (NJB) (HAN) (WLT) (IMU)

-Index_Entries

   compare two d.p. windows

-&
*/

{ /* Begin wnreld_c */

   /*
   Local variables
   */
   SpiceBoolean            retval;



   /*
   Participate in error tracing.
   */
   if ( failed_c() )
   {
      return ( SPICEFALSE );
   }
   chkin_c ( "wnreld_c" );


   /*
   Check the input string str to make sure the pointer is non-null
   and the string length is non-zero.
   */
   CHKFSTR_VAL ( CHK_STANDARD, "wnreld_c", op, SPICEFALSE );


   /*
   Make sure cell data types are d.p.
   */
   CELLTYPECHK2_VAL ( CHK_STANDARD, "wnreld_c", SPICE_DP, a, b, SPICEFALSE );


   /*
   Initialize the cells if necessary.
   */
   CELLINIT2 ( a, b );


   /*
   Let the f2c'd routine do the work.
   */
   retval = wnreld_ ( (doublereal * ) (a->base),
                      (char       * ) op,
                      (doublereal * ) (b->base),
                      (ftnlen       ) strlen(op)  );


   chkout_c ( "wnreld_c" );

   return ( retval );

} /* End wnreld_c */
