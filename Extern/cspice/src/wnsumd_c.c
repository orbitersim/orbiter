/*

-Procedure wnsumd_c ( Summary of a double precision window )

-Abstract

   Summarize the contents of a double precision window.

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
   #include "SpiceZmc.h"

   void wnsumd_c ( SpiceCell      * window,
                   SpiceDouble    * meas,
                   SpiceDouble    * avg,
                   SpiceDouble    * stddev,
                   SpiceInt       * idxsml,
                   SpiceInt       * idxlon   )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   window     I   Window to be summarized.
   meas       O   Total measure of intervals in window.
   avg        O   Average measure.
   stddev     O   Standard deviation.
   idxsml,
   idxlon     O   Locations of shortest, longest intervals.

-Detailed_Input

   window      is a window containing zero or more intervals.

               `window' must be declared as a double precision SpiceCell.

               CSPICE provides the following macro, which declares and
               initializes the cell

                  SPICEDOUBLE_CELL        ( window, WINDOWSZ );

               where WINDOWSZ is the maximum capacity of `window'.

-Detailed_Output

   meas        is the total measure of the intervals in the input
               window. This is just the sum of the measures of the
               individual intervals.

   avg         is the average of the measures of the intervals in the
               input window.

   stddev      is the standard deviation of the measures of the
               intervals in the input window.

   idxsml,
   idxlon      are the locations of the shortest and longest intervals
               in the input window. The shortest interval is

                  [   SPICE_CELL_ELEM_D( window, idxsml   ),
                      SPICE_CELL_ELEM_D( window, idxsml+1 )   ]

               and the longest is

                  [   SPICE_CELL_ELEM_D( window, idxlon   ),
                      SPICE_CELL_ELEM_D( window, idxlon+1 )   ]

               `idxsml' and `idxlon' are both -1 if the input window
               contains no intervals.

               If `window' contains multiple intervals having the shortest
               length, `idxsml' is the index of the first such interval.
               Likewise for the longest length.

               Indices range from 0 to 2N-2, where N is the number of
               intervals in the window.

-Parameters

   None.

-Exceptions

   1)  If `window' has odd cardinality, the error
       SPICE(INVALIDCARDINALITY) is signaled by a routine in the call
       tree of this routine.

   2)  Left endpoints of stored intervals must be strictly greater
       than preceding right endpoints. Right endpoints must be
       greater than or equal to corresponding left endpoints.
       Invalid window data are not diagnosed by this routine and may
       lead to unpredictable results.

   3)  If the `window' cell argument has a type other than
       SpiceDouble, the error SPICE(TYPEMISMATCH) is signaled.

-Files

   None.

-Particulars

   This routine provides a summary of the input window, consisting
   of the following items:

   -  The measure of the window.

   -  The average and standard deviation of the measures
      of the individual intervals in the window.

   -  The indices of the left endpoints of the shortest
      and longest intervals in the window.

   All of these quantities are zero if the window contains no
   intervals.

-Examples

   The numerical results shown for these examples may differ across
   platforms. The results depend on the SPICE kernels used as input,
   the compiler and supporting libraries, and the machine specific
   arithmetic implementation.

   1) Define a window with six intervals, and calculate the
      summary for that window.

      Example code begins here.


      /.
         Program wnsumd_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {
         /.
         Local parameters.
         ./
         #define WNSIZE           12

         /.
         Local variables.
         ./
         SpiceDouble              avg;
         SpiceDouble              left;
         SpiceDouble              meas;
         SpiceDouble              right;
         SpiceDouble              stddev;

         SPICEDOUBLE_CELL ( window, WNSIZE );

         SpiceInt                 idxlon;
         SpiceInt                 idxsml;
         SpiceInt                 intlon;
         SpiceInt                 intsml;

         /.
         Validate the window with size WNSIZE and zero elements.
         ./
         wnvald_c ( WNSIZE, 0, &window );

         /.
         Insert the intervals

            [ 1, 3 ] [ 7, 11 ] [ 18, 18 ] [ 23, 27 ] [ 30, 69 ] [ 72, 80 ]

         into `window'.
         ./
         wninsd_c (  1.0,   3.0, &window );
         wninsd_c (  7.0,  11.0, &window );
         wninsd_c ( 18.0,  18.0, &window );
         wninsd_c ( 23.0,  27.0, &window );
         wninsd_c ( 30.0,  69.0, &window );
         wninsd_c ( 72.0,  80.0, &window );

         /.
         Calculate the summary for `window'.
         ./
         wnsumd_c ( &window, &meas, &avg, &stddev, &idxsml, &idxlon );

         /.
         `idxsml' and `idxlon' refer to the indices of
         the SPICE Cell data array.
         ./
         intsml = idxsml/2;
         intlon = idxlon /2;

         printf( "Measure           : %f\n", meas   );
         printf( "Average           : %f\n", avg    );
         printf( "Standard Dev      : %f\n", stddev );
         printf( "Index shortest    : %d\n", idxsml );
         printf( "Index longest     : %d\n", idxlon );
         printf( "Interval shortest : %d\n", intsml );
         printf( "Interval longest  : %d\n", intlon );

         /.
         Output the shortest and longest intervals.
         ./
         wnfetd_c( &window, intsml, &left, &right );
         printf( "Shortest interval : [ %f, %f ]\n", left, right );
         wnfetd_c( &window, intlon, &left, &right );
         printf( "Longest interval  : [ %f, %f ]\n", left, right );

         return( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Measure           : 57.000000
      Average           : 9.500000
      Standard Dev      : 13.413302
      Index shortest    : 4
      Index longest     : 8
      Interval shortest : 2
      Interval longest  : 4
      Shortest interval : [ 18.000000, 18.000000 ]
      Longest interval  : [ 30.000000, 69.000000 ]


   2) Let A contain the intervals

         [ 1, 3 ]  [ 7, 11 ]  [ 23, 27 ]

      Let B contain the singleton intervals

         [ 2, 2 ]  [ 9, 9 ]  [ 27, 27 ]

      The measures of A and B are

         (3-1) + (11-7) + (27-23) = 10

      and

         (2-2) + (9-9) + (27-27) = 0

      respectively. Each window has three intervals; thus, the average
      measures of the windows are 10/3 and 0. The standard deviations
      are

           ------------------------------------------
          |       2         2          2
          |  (3-1)  + (11-7)  + (27-23)           2           1/2
          |  ---------------------------  - (10/3)     = (8/9)
          |             3
        \ |
         \|

      and 0. Neither window has one "shortest" interval or "longest"
      interval; so the first ones found are returned: `idxsml' and
      `idxlon' are 0 and 2 for A, 0 and 0 for B.

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
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.1.0, 05-AUG-2021 (JDR)

       Changed output argument names "shortest" and "longest" to
       "idxsml" and "idxlon" for consistency with other routines.

       Edited the header to comply with NAIF standard. Added complete code
       example.

       Extended description of argument "window" in -Detailed_Input to include
       type and preferred declaration method.

       Fixed error in documentation of arguments "idxsml" and "idxlon": if the
       input window contains no intervals, both are -1 and not zero.

       Added entries #1 and #2 in -Exceptions section.

   -CSPICE Version 1.0.1, 27-JAN-2009 (EDW)

       Corrected argument names shown in Brief I/O list.
       "short" to "shortest"; "long" to "longest".

   -CSPICE Version 1.0.0, 29-JUL-2002 (NJB) (HAN) (WLT) (IMU)

-Index_Entries

   summary of a d.p. window

-&
*/

{ /* Begin wnsumd_c */


   /*
   Use discovery check-in.

   Make sure cell data type is d.p.
   */
   CELLTYPECHK ( CHK_DISCOVER, "wnsumd_c", SPICE_DP, window );

   /*
   Initialize the cell if necessary.
   */
   CELLINIT ( window );

   /*
   Let the f2c'd routine do the work.
   */
   wnsumd_ ( (doublereal * ) (window->base),
             (doublereal * ) meas,
             (doublereal * ) avg,
             (doublereal * ) stddev,
             (integer    * ) idxsml,
             (integer    * ) idxlon        );

   /*
   Map idxsml and idxlon from Fortran style to C style indices.
   */
   (*idxsml ) --;
   (*idxlon ) --;

} /* End wnsumd_c */
