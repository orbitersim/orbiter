/*

-Procedure copy_c ( Copy a SPICE cell )

-Abstract

   Copy the contents of a SpiceCell of any data type to another
   cell of the same type.

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

-Keywords

   CELLS

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"


   void copy_c (  SpiceCell   * cell,
                  SpiceCell   * copy  )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   cell       I   Cell to be copied.
   copy       O   New cell.

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

   copy        is a SPICE cell which contains the same elements as the input
               cell, in the same order.

               `copy' must be declared as a SpiceCell of the same data type
               as `cell'.

               CSPICE provides the following macros, which declare and
               initialize the cell

                  SPICECHAR_CELL          ( copy, COPYSZ, COPYMLEN );
                  SPICEDOUBLE_CELL        ( copy, COPYSZ );
                  SPICEINT_CELL           ( copy, COPYSZ );

               where COPYSZ is the maximum capacity of `copy' and COPYMLEN is
               the maximum length of any member in the character cell.

-Parameters

   None.

-Exceptions

   1)  If the output cell in not large enough to hold the elements of
       the input cell, the error SPICE(CELLTOOSMALL) is signaled by a
       routine in the call tree of this routine.

   2)  If the `cell' and `copy' cell arguments are of type SpiceChar and
       the length of the elements of the output cell is less than the
       length of the elements of the input cell, the error
       SPICE(INSUFFLEN) is signaled.

   3)  If the `cell' and `copy' cell arguments do not have identical data
       types, the error SPICE(TYPEMISMATCH) is signaled.

   4)  If any of the `cell' or `copy' cell arguments does not have a
       recognized data type, the error SPICE(NOTSUPPORTED) is signaled.

   5)  If the cell arguments are of type SpiceChar and the string length
       associated with any of them is non-positive or too short to be
       usable when constructing the equivalent SPICE character cells
       required by the wrapped SPICELIB routine, an error is signaled by
       a routine in the call tree of this routine.

-Files

   None.

-Particulars

   This routine is used primarily to manipulate working cells, since
   many routines that use cells (binary set routines, for instance) do
   not allow cells to be combined or manipulated in place.

-Examples

   In the following example, copy_c is used to copy the result
   of the union of two character CSICE sets from a temporary
   working set back into the one of the original set.

      #include "SpiceUsr.h"
           .
           .
           .
      /.
      Declare the cell names with string length LNSIZE and maximum
      number of strings SIZE.
      ./
      SPICECHAR_CELL ( bodies,  SIZE, LNSIZE );
      SPICECHAR_CELL ( planets, SIZE, LNSIZE );
      SPICECHAR_CELL ( temp,    SIZE, LNSIZE );
           .
           .
           .
      union_c ( &bodies, &planets, &temp );
      copy_c  ( &temp,   &bodies         );


   If the size of the temporary cell is greater than the size
   of the original set, the function failed_c should be checked to be
   sure that no overflow occurred. If bodies is at least as
   large as temp, no such check is necessary.

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   C.A. Curzon         (JPL)
   J. Diaz del Rio     (ODC Space)
   W.L. Taber          (JPL)
   I.M. Underwood      (JPL)

-Version

   -CSPICE Version 1.0.1, 27-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard.

       Extended description of arguments "cell" and "copy" to include
       type and preferred declaration method.

       Added entry #4 and #5 in -Exceptions section.

   -CSPICE Version 1.0.0, 08-AUG-2002 (NJB) (CAC) (WLT) (IMU)

-Index_Entries

   copy a character cell

-&
*/

{ /* Begin copy_c */


   /*
   Local variables
   */
   SpiceChar             * fCell[2];

   SpiceInt                fLen [2];
   SpiceInt                i;


   /*
   Standard SPICE error handling.
   */
   if ( return_c() )
   {
      return;
   }
   chkin_c ( "copy_c" );


   /*
   Make sure data types match.
   */
   CELLMATCH2 ( CHK_STANDARD, "copy_c", cell, copy );


   /*
   Initialize the cells if necessary.
   */
   CELLINIT2 ( cell, copy );


   /*
   Call the copy routine appropriate for the data type of the cells.
   */
   if ( cell->dtype == SPICE_CHR )
   {

      /*
      Construct Fortran-style sets suitable for passing to copyc_.
      */
      C2F_MAP_CELL2 ( "copy_c",
                      cell, fCell,   fLen,
                      copy, fCell+1, fLen+1 );

      if ( failed_c() )
      {
         chkout_c ( "copy_c" );
         return;
      }

      copyc_ ( (char    * )  fCell[0],
               (char    * )  fCell[1],
               (ftnlen    )  fLen[0],
               (ftnlen    )  fLen[1]  );

      if ( !failed_c() )
      {
         /*
         Map the copy back to a C style cell.
         */
         F2C_MAP_CELL ( fCell[1], fLen[1], copy );
      }

      /*
      We're done with the dynamically allocated Fortran-style arrays.
      */
      for ( i = 0;  i < 2;  i++ )
      {
         free ( fCell[i] );
      }
   }

   else if ( cell->dtype == SPICE_DP )
   {
      copyd_ ( (doublereal * )  (cell->base),
               (doublereal * )  (copy->base)   );

      /*
      Sync the output cell.
      */
      if ( !failed_c() )
      {
         zzsynccl_c ( F2C, copy );
      }

   }

   else if ( cell->dtype == SPICE_INT )
   {
      copyi_ ( (integer * )  (cell->base),
               (integer * )  (copy->base)  );

      /*
      Sync the output cell.
      */
      if ( !failed_c() )
      {
         zzsynccl_c ( F2C, copy );
      }

   }

   else
   {
       setmsg_c ( "Source cell contains unrecognized data type code #." );
       errint_c ( "#",  (SpiceInt) (cell->dtype)                        );
       sigerr_c ( "SPICE(NOTSUPPORTED)"                                 );
       chkout_c ( "copy_c"                                              );
       return;
   }

   chkout_c ( "copy_c" );

} /* End copy_c */
