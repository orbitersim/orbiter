/*

-Procedure bltfrm_c ( Built-in frame IDs )

-Abstract

   Return a SPICE set containing the frame IDs of all built-in frames
   of a specified class.

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
   FRAMES
   NAIF_IDS
   SETS

-Keywords

   FRAME
   SET
   UTILITY

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"


   void bltfrm_c ( SpiceInt      frmcls,
                   SpiceCell   * idset  )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   frmcls     I   Frame class.
   idset      O   Set of ID codes of frames of the specified class.

-Detailed_Input

   frmcls      is an integer code specifying the frame class or
               classes for which built-in frame ID codes are
               requested. `frmcls' may designate a single class or
               "all classes."

               The CSPICE header file SpiceFrm.h declares parameters
               identifying frame classes. The supported values
               and corresponding meanings of `frmcls' are

                  Parameter            Value    Meaning
                  ===================  =====    ==================
                  SPICE_FRMTYP_ALL       -1     All frame classes.
                  SPICE_FRMTYP_INERTL     1     Built-in inertial.
                  SPICE_FRMTYP_PCK        2     PCK-based frame.
                  SPICE_FRMTYP_CK         3     CK-based frame.
                  SPICE_FRMTYP_TK         4     Fixed offset ("text
                                                kernel") frame.
                  SPICE_FRMTYP_DYN        5     Dynamic frame.
                  SPICE_FRMTYP_SWTCH      6     Switch frame.

-Detailed_Output

   idset       is a SPICE set containing the ID codes of all
               built-in reference frames of the specified class
               or classes.

               If `idset' is non-empty on input, its contents will be
               discarded.

               `idset' must be declared as an integer SpiceCell.

               CSPICE provides the following macro, which declares and
               initializes the cell

                  SPICEINT_CELL           ( idset, IDSETSZ );

               where IDSETSZ is the maximum capacity of `idset'.

-Parameters

   See the CSPICE header file SpiceFrm.h.

-Exceptions

   1)  If the input frame class argument is not defined in
       SpiceFrm.h, the error SPICE(BADFRAMECLASS) is signaled by a
       routine in the call tree of this routine.

   2)  If the size of `idset' is too small to hold the requested frame
       ID set, the error SPICE(SETTOOSMALL) is signaled by a routine
       in the call tree of this routine.

   3)  If the `idset' cell argument has a type other than SpiceInt,
       the error SPICE(TYPEMISMATCH) is signaled.

-Files

   None.

-Particulars

   This routine has a counterpart

      kplfrm_c

   which fetches the frame IDs of all frames specified in the kernel
   pool.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Display the IDs and names of all SPICE built-in frames.
      Group the outputs by frame class. Also fetch and display
      the entire set of IDs and names using the parameter
      SPICE_FRMTYP_ALL.


      Example code begins here.


      /.
         Program bltfrm_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main()
      {
         /.
         Local parameters
         ./
         #define FRNMLN          33
         #define NFRAME        ( SPICE_NFRAME_NINERT +  \
                                 SPICE_NFRAME_NNINRT   )
         #define LNSIZE          81

         /.
         Local variables
         ./
         SPICEINT_CELL           ( idset, NFRAME );

         SpiceChar               frname  [ FRNMLN ];
         SpiceChar               outlin  [ LNSIZE ];

         SpiceInt                i;
         SpiceInt                j;
         SpiceInt                nfrms;

         /.
         Get the Toolkit version number and display it.
         ./
         printf( "Toolkit version: %s\n", tkvrsn_c( "TOOLKIT" ) );

         /.
         Fetch and display the frames of each class.
         ./
         for ( i = 1;  i <= 7;  i++ )
         {

            if ( i < 7 )
            {
               /.
               Fetch the frames of class i.
               ./
               bltfrm_c ( i, &idset );

               sprintf ( outlin,
                         "Number of frames of class %d: %d",
                         (int) i,
                         (int) card_c(&idset)                );

            }
            else
            {

               /.
               Fetch IDs of all built-in frames.
               ./
               bltfrm_c ( SPICE_FRMTYP_ALL, &idset );

               sprintf ( outlin,
                         "Number of built-in frames: %d",
                         (int) card_c(&idset)                );

            }

            /.
            Display the NAIF ID and name of a maximum of 5 frames
            per family.
            ./
            printf ( "\n"
                     "%s\n"
                     "   Frame IDs and names\n",
                     outlin                     );

            nfrms = mini_c ( 2, 5, card_c(&idset) );

            for ( j = 0;  j < nfrms;  j++ )
            {

               frmnam_c ( ((SpiceInt *)idset.data)[j], FRNMLN, frname );
               printf ( "%12ld   %s\n",
                        ( (long) ((SpiceInt *)idset.data)[j] ), frname );

            }

         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Toolkit version: CSPICE_N0067

      Number of frames of class 1: 21
         Frame IDs and names
                 1   J2000
                 2   B1950
                 3   FK4
                 4   DE-118
                 5   DE-96

      Number of frames of class 2: 105
         Frame IDs and names
             10001   IAU_MERCURY_BARYCENTER
             10002   IAU_VENUS_BARYCENTER
             10003   IAU_EARTH_BARYCENTER
             10004   IAU_MARS_BARYCENTER
             10005   IAU_JUPITER_BARYCENTER

      Number of frames of class 3: 0
         Frame IDs and names

      Number of frames of class 4: 1
         Frame IDs and names
             10081   EARTH_FIXED

      Number of frames of class 5: 0
         Frame IDs and names

      Number of frames of class 6: 0
         Frame IDs and names

      Number of built-in frames: 145
         Frame IDs and names
                 1   J2000
                 2   B1950
                 3   FK4
                 4   DE-118
                 5   DE-96


      Note that the set of built-in frames, particularly the
      non-inertial ones, will grow over time, so the output
      shown here may be out of sync with that produced by a
      current SPICE Toolkit. Only the first 5 frames of each
      family are presented in the output.

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.1.0, 26-AUG-2021 (JDR) (NJB)

       Swapped the calls to CELLINIT and CELLTYPECHK: initialize the control
       area only if the input cell data type is of type SpiceInt.

       Updated to account for switch frame class.

       Edited the header to comply with NAIF standard. Updated code example to
       limit the number of frames presented in the output. Added Toolkit
       version information to the output of the example.

       Extended description of argument "idset" in -Detailed_Output to include
       type and preferred declaration method, and to indicate that its
       contents will be discarded by this routine.

   -CSPICE Version 1.0.1, 12-JUL-2016 (EDW)

       Edit to example program to use "%d" with explicit casts
       to int for printing SpiceInts with printf.

   -CSPICE Version 1.0.0, 22-MAY-2012 (NJB)

-Index_Entries

   fetch IDs of built-in reference frames

-&
*/

{ /* Begin bltfrm_c */


   /*
   Participate in error tracing.
   */
   chkin_c ( "bltfrm_c" );

   /*
   Make the sure cell data type is SpiceInt.
   */
   CELLTYPECHK ( CHK_STANDARD, "bltfrm_c", SPICE_INT, idset );

   /*
   Initialize the control area of the cell's data array
   if necessary.
   */
   CELLINIT ( idset );

   /*
   Let the f2'd routine do the work.
   */
   bltfrm_ ( ( integer  * ) &frmcls,
             ( integer  * ) (idset->base) );

   /*
   Sync the output cell.
   */
   if ( !failed_c() )
   {
     zzsynccl_c ( F2C, idset ) ;
   }

   chkout_c ( "bltfrm_c" );

} /* End bltfrm_c */
