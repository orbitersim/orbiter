/*

-Procedure ssize_c ( Set the size of a cell )

-Abstract

   Set the size (maximum cardinality) of a SPICE cell of any data
   type.

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
   #include "SpiceZmc.h"

   void ssize_c (  SpiceInt      size,
                   SpiceCell   * cell  )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   size       I   Size (maximum cardinality) of the cell.
   cell       O   The cell.

-Detailed_Input

   size        is the new value of the size (maximum number of elements) of
               the cell.

               `size' must be non-negative and must be no larger than the
               initial declared size of the cell.

   cell        is a SPICE cell of any data type.

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

   cell        on output, is the input cell with its size updated to the value
               given by the input argument size.

               The cardinality of the cell is set to 0.

               The cell becomes a  SPICE set: the cell's "is a set?" attribute
               becomes SPICETRUE. The cell then can be used as an input to the
               SPICE set routines such as insrtX_c.

               Unlike the cell "set size" routines in the Fortran SPICE
               Toolkit's SPICELIB library, this routine does not clear the
               unused portion of the cell's control area.

-Parameters

   None.

-Exceptions

   1)  If an attempt is made to set the size of the cell to a negative
       number, the error SPICE(INVALIDSIZE) is signaled.

   2)  The size of a cell may not be set to a value larger than the
       original declared size. However, the SPICE cell routines
       cannot detect this error.

   3)  If the input `cell' cell argument does not have a recognized data
       type, the error SPICE(NOTSUPPORTED) is signaled.

-Files

   None.

-Particulars

   Unlike their counterparts in the Fortran SPICELIB library, cells in
   CSPICE are initialized automatically when accessed via the SPICE cell
   API routines, so there is normally no reason to call this routine.

   This routine is provided for the sake of completeness.

-Examples

   1) Declare an integer cell. Populate the cell, then reset
      the size to 1/2 the originally declared size, in order
      to inhibit write access to the last portion of the cell.

         #include "SpiceUsr.h"
               .
               .
               .

         #define SIZE          10

         /.
         Declare a cell with room for SIZE integers.
         ./
         SPICEINT_CELL         ( icell, SIZE );
               .
               .
               .
         /.
         Reduce the size of the cell.
         ./
         ssize_c ( SIZE/2, &icell );

-Restrictions

   1)  See exception (2) in the -Exceptions section.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   C.A. Curzon         (JPL)
   J. Diaz del Rio     (ODC Space)
   W.L. Taber          (JPL)
   I.M. Underwood      (JPL)

-Version

   -CSPICE Version 1.0.1, 24-NOV-2021 (JDR)

       Edited the header to comply with NAIF standard.

       Extended description of argument "cell" in -Detailed_Input to include
       type and preferred declaration method.

       Added entry #3 in -Exceptions section.

   -CSPICE Version 1.0.0, 21-AUG-2002 (NJB) (CAC) (WLT) (IMU)

-Index_Entries

   set the size of a cell

-&
*/

{ /* Begin ssize_c */


   /*
   Participate in error tracing.
   */
   if ( return_c() )
   {
      return;
   }
   chkin_c ( "ssize_c" );


   if ( size < 0 )
   {
      setmsg_c ( "Attempt to set the size of cell to invalid "
                 "value.  The value was #."                    );
      errint_c ( "#",  size                                    );
      sigerr_c ( "SPICE(INVALIDSIZE)"                          );
      chkout_c ( "ssize_c"                                     );
      return;
   }


   /*
   Initialize the cell if necessary.
   */
   CELLINIT ( cell );


   /*
   Do what the Fortran ssizec routine does:  set the cell's size
   and reset the cardinality to zero.
   */
   cell->size  =  size;
   cell->card  =  0;


   /*
   Sync the cell.
   */
   zzsynccl_c ( C2F, cell );


   /*
   The cell becomes a set since it's empty.
   */
   cell->isSet = SPICETRUE;


   chkout_c ( "ssize_c" );

} /* End ssize_c */
