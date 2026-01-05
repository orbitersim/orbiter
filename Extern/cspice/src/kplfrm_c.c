/*

-Procedure kplfrm_c ( Kernel pool frame IDs )

-Abstract

   Return a SPICE set containing the frame IDs of all reference
   frames of a given class having specifications in the kernel pool.

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
   KERNEL
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

   void kplfrm_c ( SpiceInt      frmcls,
                   SpiceCell   * idset   )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   frmcls     I   Frame class.
   idset      O   Set of ID codes of frames of the specified class.

-Detailed_Input

   frmcls      is an integer code specifying the frame class or
               classes for which frame ID codes are requested. The
               applicable reference frames are those having
               specifications present in the kernel pool.

               `frmcls' may designate a single class or "all
               classes."

               The CSPICE header file SpiceFrm.h declares parameters
               identifying frame classes. The supported values
               and corresponding meanings of `frmcls' are

                  Parameter            Value   Meaning
                  ===================  =====   ====================
                  SPICE_FRMTYP_ALL       -1    All frame classes
                                               specified in the
                                               kernel pool. Class 1
                                               is not included.

                  SPICE_FRMTYP_INERTL     1    Built-in inertial.
                                               No frames will be
                                               returned in the
                                               output set.

                  SPICE_FRMTYP_PCK        2    PCK-based frame.

                  SPICE_FRMTYP_CK         3    CK-based frame.

                  SPICE_FRMTYP_TK         4    Fixed rotational
                                               offset ("text
                                               kernel") frame.

                  SPICE_FRMTYP_DYN        5    Dynamic frame.

                  SPICE_FRMTYP_SWTCH      6    Switch frame.

-Detailed_Output

   idset       is a SPICE set containing the ID codes of all
               reference frames having specifications present in
               the kernel pool and belonging to the specified
               class or classes.

               Note that if `frmcls' is set to SPICE_FRMTYP_INERTL, `idset'
               will be empty on output.

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

   3)  Frames of class 1 may not be specified in the kernel pool.
       However, for the convenience of users, this routine does not
       signal an error if the input class is set to SPICE_FRMTYP_INERTL.
       In this case the output set will be empty.

   4)  This routine relies on the presence of just three kernel
       variable assignments for a reference frame in order to
       determine that that reference frame has been specified:

          FRAME_<frame name>       = <ID code>
          FRAME_<ID code>_NAME     = <frame name>

       and either

          FRAME_<ID code>_CLASS    = <class>

       or

          FRAME_<frame name>_CLASS = <class>

       It is possible for the presence of an incomplete frame
       specification to trick this routine into incorrectly
       deciding that a frame has been specified. This routine
       does not attempt to diagnose this problem.

   5)  If the `idset' cell argument has a type other than SpiceInt,
       the error SPICE(TYPEMISMATCH) is signaled.

-Files

   Reference frame specifications for frames that are not built in
   are typically established by loading frame kernels.

-Particulars

   This routine enables SPICE-based applications to conveniently
   find the frame ID codes of reference frames having specifications
   present in the kernel pool. Such frame specifications are
   introduced into the kernel pool either by loading frame kernels
   or by means of calls to the kernel pool "put" API routines

      pcpool_c
      pdpool_c
      pipool_c

   Given a reference frame's ID code, other attributes of the
   frame can be obtained via calls to the CSPICE routines

      frmnam_c {Return a frame's name}
      frinfo_c {Return a frame's center, class, and class ID}

   This routine has a counterpart

      bltfrm_c

   which fetches the frame IDs of all built-in reference frames.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Display the IDs and names of all reference frames having
      specifications present in the kernel pool. Group the outputs
      by frame class. Also fetch and display the entire set of IDs
      and names using the parameter SPICE_FRMTYP_ALL.

      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File: kplfrm_ex1.tm

         This meta-kernel is intended to support operation of SPICE
         example programs. The kernels shown here should not be
         assumed to contain adequate or correct versions of data
         required by SPICE-based user applications.

         In order for an application to use this meta-kernel, the
         kernels referenced here must be present in the user's
         current working directory.

         The names and contents of the kernels referenced
         by this meta-kernel are as follows:

            File name            Contents
            --------------       --------------------------
            clem_v20.tf          Clementine FK
            moon_060721.tf       Generic Lunar SPICE frames


         \begindata

            KERNELS_TO_LOAD = ( 'clem_v20.tf'
                                'moon_060721.tf' )
         \begintext

         End of meta-kernel


      Example code begins here.


      /.
         Program kplfrm_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main()
      {
         /.
         Local parameters
         ./
         #define META            "kplfrm_ex1.tm"
         #define FRNMLN          33
         #define NFRAME          1000
         #define LNSIZE          81

         /.
         Local variables
         ./
         SPICEINT_CELL           ( idset, NFRAME );

         SpiceChar               frname  [ FRNMLN ];
         SpiceChar               outlin  [ LNSIZE ];

         SpiceInt                i;
         SpiceInt                j;

         /.
         Load kernels that contain frame specifications.
         ./
         furnsh_c ( META );

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
               kplfrm_c ( i, &idset );

               sprintf ( outlin,
                         "Number of frames of class %d: %d",
                         (int) i,
                         (int) card_c(&idset)                );

            }
            else
            {

               /.
               Fetch IDs of all frames specified in the kernel pool.
               ./
               kplfrm_c ( SPICE_FRMTYP_ALL, &idset );

               sprintf ( outlin,
                         "Number of frames in the kernel pool: %d",
                         (int) card_c(&idset)                     );

            }

            /.
            Display the fetched frame IDs and corresponding names.
            ./
            printf ( "\n"
                     "%s\n"
                     "   Frame IDs and names\n",
                     outlin                     );

            for ( j = 0;  j < card_c(&idset);  j++ )
            {

               frmnam_c ( ((SpiceInt *)idset.data)[j], FRNMLN, frname );

               printf ( "%12ld   %s\n",
                        ( (long) ((SpiceInt *)idset.data)[j] ),  frname );

            }

         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Number of frames of class 1: 0
         Frame IDs and names

      Number of frames of class 2: 1
         Frame IDs and names
             31002   MOON_PA_DE403

      Number of frames of class 3: 1
         Frame IDs and names
            -40000   CLEM_SC_BUS

      Number of frames of class 4: 11
         Frame IDs and names
            -40008   CLEM_CPT
            -40007   CLEM_BSTAR
            -40006   CLEM_ASTAR
            -40005   CLEM_LIDAR
            -40004   CLEM_LWIR
            -40003   CLEM_NIR
            -40002   CLEM_UVVIS
            -40001   CLEM_HIRES
             31000   MOON_PA
             31001   MOON_ME
             31003   MOON_ME_DE403

      Number of frames of class 5: 0
         Frame IDs and names

      Number of frames of class 6: 0
         Frame IDs and names

      Number of frames in the kernel pool: 13
         Frame IDs and names
            -40008   CLEM_CPT
            -40007   CLEM_BSTAR
            -40006   CLEM_ASTAR
            -40005   CLEM_LIDAR
            -40004   CLEM_LWIR
            -40003   CLEM_NIR
            -40002   CLEM_UVVIS
            -40001   CLEM_HIRES
            -40000   CLEM_SC_BUS
             31000   MOON_PA
             31001   MOON_ME
             31002   MOON_PA_DE403
             31003   MOON_ME_DE403


-Restrictions

   1)  This routine will work correctly if the kernel pool contains
       no invalid frame specifications. See the description of
       exception 4 above. Users must ensure that no invalid frame
       specifications are introduced into the kernel pool, either by
       loaded kernels or by means of the kernel pool "put" APIs.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.1.0, 08-AUG-2021 (JDR)

       Swapped the calls to CELLINIT and CELLTYPECHK: initialize the control
       area only if the input cell data type is of type SpiceInt.

       Updated to account for switch frame class.

       Fixed typos in header documentation.

       Edited the header section to comply with NAIF standard. Updated
       Example's kernels set to use Clementine PDS archived data.

       Extended description of argument "idset" in -Detailed_Output to include
       type and preferred declaration method.

       Added entry #5 to -Exceptions section.

   -CSPICE Version 1.0.1, 12-JUL-2016 (EDW)

       Edit to example program to use "%d" with explicit casts
       to int for printing SpiceInts with printf.

   -CSPICE Version 1.0.0, 22-MAY-2012 (NJB)

-Index_Entries

   fetch IDs of reference_frames from the kernel_pool

-&
*/

{ /* Begin kplfrm_c */

   /*
   Participate in error tracing.
   */
   chkin_c ( "kplfrm_c" );

   /*
   Make the sure cell data type is SpiceInt.
   */
   CELLTYPECHK ( CHK_STANDARD, "kplfrm_c", SPICE_INT, idset );

   /*
   Initialize the control area of the cell's data array
   if necessary.
   */
   CELLINIT ( idset );

   /*
   Let the f2c'd routine do the work.
   */
   kplfrm_ ( ( integer  * ) &frmcls,
             ( integer  * ) (idset->base) );

   /*
   Sync the output cell.
   */
   if ( !failed_c() )
   {
     zzsynccl_c ( F2C, idset ) ;
   }

   chkout_c ( "kplfrm_c" );

} /* End kplfrm_c */
