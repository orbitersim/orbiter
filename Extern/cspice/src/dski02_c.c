/*

-Procedure dski02_c ( DSK, fetch integer type 2 data )

-Abstract

   Fetch integer data from a type 2 DSK segment.

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

   DAS
   DSK

-Keywords

   DAS
   DSK
   FILES
   TOPOGRAPHY

*/
   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #undef dski02_c

   void dski02_c ( SpiceInt              handle,
                   ConstSpiceDLADescr  * dladsc,
                   SpiceInt              item,
                   SpiceInt              start,
                   SpiceInt              room,
                   SpiceInt            * n,
                   SpiceInt            * values   )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   handle     I   DSK file handle.
   dladsc     I   DLA descriptor.
   item       I   Keyword identifying item to fetch.
   start      I   Start index.
   room       I   Amount of room in output array.
   n          O   Number of values returned.
   values     O   Array containing requested item.

-Detailed_Input

   handle      is the handle of a DSK file containing a type 2
               segment from which data are to be fetched.

   dladsc      is the DLA descriptor associated with the segment
               from which data are to be fetched.

   item        is an integer "keyword" parameter designating the
               integer data item to fetch.

               Names, meanings, and value of keyword parameters
               supported by this routine are given in the header
               file

                  SpiceDSK.h

               The keyword parameters for integer data listed there
               are supported by this routine.

   start       is the start index within specified data item from
               which data are to be fetched. The index of the first
               element of each data item is 0. This convention
               applies uniformly to all data, even if the data are
               associated with a set of 1-based indices. For
               example, the plate ID range starts at 1 (this fact is
               language-independent), but a caller would use a
               `start' value of 0 to fetch the vertex indices of the
               first plate.

   room        is the amount of room in the output array. It is
               permissible to provide an output array that has
               too little room to fetch an item in one call. `room'
               has units of integers: for example, the room
               required to fetch one plate is 3.

-Detailed_Output

   n           is the number of elements fetched to the output
               array `values'. `n' is normally in the range
               1:room; if an error occurs on the call, `n' is
               undefined.

   values      is a contiguous set of elements of the item
               designated by `item'. The correspondence of
               `values' with the elements of the data item is:

                  values[0]      item[start]
                    ...             ...
                  values[n-1]    item[start+n-1]

               If an error occurs on the call, `values' is
               undefined.

-Parameters

   See the header file

      SpiceDLA.h

   for declarations of DLA descriptor sizes and documentation of the
   contents of DLA descriptors.

   See the header file

      SpiceDSK.h

   for declarations of DSK descriptor sizes and documentation of the
   contents of DSK descriptors.

   See the header file

      SpiceDSK.h

   for declarations of DSK data type 2 (plate model) parameters.

-Exceptions

   1)  If the input handle is invalid, an error is signaled by a
       routine in the call tree of this routine.

   2)  If a file read error occurs, the error is signaled by a
       routine in the call tree of this routine.

   3)  If the input DLA descriptor is invalid, the effect of this
       routine is undefined. The error *may* be diagnosed by
       routines in the call tree of this routine, but there are no
       guarantees.

   4)  If `room' is non-positive, the error SPICE(VALUEOUTOFRANGE)
       is signaled by a routine in the call tree of this routine.

   5)  If the coarse voxel scale read from the designated segment is less
       than 1, the error SPICE(VALUEOUTOFRANGE) is signaled by a routine in
       the call tree of this routine.

   6)  If the input keyword parameter is not recognized, the error
       SPICE(NOTSUPPORTED) is signaled by a routine in the call tree of this
       routine.

   7)  If `start' is less than 0 or greater than or equal to the size of
       the item to be fetched, the error SPICE(INDEXOUTOFRANGE) is
       signaled by a routine in the call tree of this routine.

-Files

   See input argument `handle'.

-Particulars

   Most SPICE applications will not need to call this routine. The
   routines dskv02_c, dskp02_c, and dskz02_c provide a higher-level
   interface for fetching DSK type 2 vertex and plate data.

   DSK files are built using the DLA low-level format and
   the DAS architecture; DLA files are a specialized type of DAS
   file in which data are organized as a doubly linked list of
   segments. Each segment's data belong to contiguous components of
   character, double precision, and integer type.

   Note that the DSK descriptor for the segment is not needed by this
   routine; the DLA descriptor contains the base address and size
   information for the integer, double precision, and character
   components of the segment, and these suffice for the purpose of
   fetching data.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Look up all the vertices associated with each plate
      of the model contained in a specified type 2 segment.
      For the first 5 plates, display the plate's vertices.

      For this example, we'll show the context of this look-up:
      opening the DSK file for read access, traversing a trivial,
      one-segment list to obtain the segment of interest.


      Example code begins here.


      /.
         Program dski02_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main()
      {

         /.
         Local parameters
         ./
         #define FILSIZ          256

         /.
         Local variables
         ./
         SpiceBoolean            found;

         SpiceChar               dsk     [ FILSIZ ];

         SpiceDLADescr           dladsc;

         SpiceDouble             vrtces  [3][3];

         SpiceInt                handle;
         SpiceInt                i;
         SpiceInt                j;
         SpiceInt                n;
         SpiceInt                np;
         SpiceInt                start;
         SpiceInt                vrtids  [3];


         /.
         Prompt for the name of the DSK to read.
         ./
         prompt_c ( "Enter DSK name > ", FILSIZ, dsk );

         /.
         Open the DSK file for read access. We use the DAS-level
         interface for this function.
         ./
         dasopr_c ( dsk, &handle );

         /.
         Begin a forward search through the kernel, treating the
         file as a DLA. In this example, it's a very short search.
         ./
         dlabfs_c ( handle, &dladsc, &found );

         if ( !found  )
         {
            /.
            We arrive here only if the kernel
            contains no segments.  This is
            unexpected, but we're prepared for it.
            ./
            setmsg_c ( "No segments found in DSK file #." );
            errch_c  ( "#", dsk                           );
            sigerr_c ( "SPICE(NODATA)"                    );
         }

         /.
         If we made it this far, `dladsc' is the
         DLA descriptor of the first segment.

         Find the number of plates in the model.
         ./
         dski02_c ( handle, &dladsc, SPICE_DSK02_KWNP,
                    0,      1,       &n,             &np );

         /.
         For the first 5 plates, look up the desired data.
         ./
         np = mini_c ( 2, 5, np );
         for ( i = 1;  i <= np;  i++ )
         {
            /.
            For the Ith plate, find the associated
            vertex IDs.  We must take into account
            the fact that each plate has three
            vertices when we compute the start
            index.
            ./
            start = 3*(i-1);

            dski02_c ( handle, &dladsc, SPICE_DSK02_KWPLAT, start,
                       3,      &n,      vrtids                   );

            for ( j = 0;  j < 3;  j++  )
            {
               /.
               Fetch the vertex associated with
               the jth vertex ID.  Again, each
               vertex is a 3-vector.  Note that
               the vertices are double-precision
               data, so we fetch them using
               dskd02_c.
               ./
               start = (vrtids[j]-1)*3;

               dskd02_c ( handle, &dladsc, SPICE_DSK02_KWVERT, start,
                          3,      &n,      vrtces[j]               );
            }

            /.
            Display the vertices of the ith plate:
            ./
            printf ( "\n"
                     "Plate number:  %d\n"
                     "   Vertex 1: ( %+e   %+e   %+e )\n"
                     "   Vertex 2: ( %+e   %+e   %+e )\n"
                     "   Vertex 3: ( %+e   %+e   %+e )\n",
                     (int)i,
                     vrtces[0][0], vrtces[0][1], vrtces[0][2],
                     vrtces[1][0], vrtces[1][1], vrtces[1][2],
                     vrtces[2][0], vrtces[2][1], vrtces[2][2]  );
         }

         /.
         Close the kernel.  This isn't necessary in a stand-
         alone program, but it's good practice in subroutines
         because it frees program and system resources.
         ./
         dascls_c ( handle );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, using the DSK file named phobos512.bds, the output
      was:


      Enter DSK name > phobos512.bds

      Plate number:  1
         Vertex 1: ( -6.774440e+00   +6.268150e+00   +6.011490e+00 )
         Vertex 2: ( -6.762380e+00   +6.257280e+00   +6.025560e+00 )
         Vertex 3: ( -6.757100e+00   +6.277540e+00   +6.020960e+00 )

      Plate number:  2
         Vertex 1: ( -6.774440e+00   +6.268150e+00   +6.011490e+00 )
         Vertex 2: ( -6.779730e+00   +6.247900e+00   +6.016100e+00 )
         Vertex 3: ( -6.762380e+00   +6.257280e+00   +6.025560e+00 )

      Plate number:  3
         Vertex 1: ( -6.779730e+00   +6.247900e+00   +6.016100e+00 )
         Vertex 2: ( -6.767680e+00   +6.237010e+00   +6.030190e+00 )
         Vertex 3: ( -6.762380e+00   +6.257280e+00   +6.025560e+00 )

      Plate number:  4
         Vertex 1: ( -6.779730e+00   +6.247900e+00   +6.016100e+00 )
         Vertex 2: ( -6.784990e+00   +6.227620e+00   +6.020700e+00 )
         Vertex 3: ( -6.767680e+00   +6.237010e+00   +6.030190e+00 )

      Plate number:  5
         Vertex 1: ( -6.784990e+00   +6.227620e+00   +6.020700e+00 )
         Vertex 2: ( -6.772990e+00   +6.216740e+00   +6.034820e+00 )
         Vertex 3: ( -6.767680e+00   +6.237010e+00   +6.030190e+00 )


-Restrictions

   1)  The underlying SPICELIB routine

          DSKI02

       called by this routine uses discovery check-in to boost execution
       speed. However, that routine is in violation of NAIF standards
       for use of discovery check-in:  routines called from that routine
       may signal errors. If errors are signaled in routines called
       from the SPICELIB routine DSKI02, that routine's name will be missing
       from the traceback message.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.0.1, 02-JUL-2021 (JDR)

       Edited the header to comply with NAIF standard. Updated
       code example to reduce the number of plates whose vertices are
       shown on output and to remove unnecessary include files.

   -CSPICE Version 1.0.0, 04-APR-2017 (NJB)

       Updated parameter references in example program.
       Removed unnecessary include statements.
       Updated header.

       DSKLIB_C Version 1.0.1, 11-JUL-2014 (NJB)

          Corrected minor header comment typos.

       DSKLIB_C Version 1.0.0, 11-FEB-2010 (NJB)

-Index_Entries

   fetch integer data from a type 2 DSK segment

-&
*/

{ /* Begin dski02_c */


   /*
   Local variables
   */
   SpiceInt                fDLADescr  [ SPICE_DLA_DSCSIZ ];

   integer                 fItem;
   integer                 fRoom;
   integer                 fStart;


   /*
   Participate in error tracing.
   */
   chkin_c ( "dski02_c" );

   /*
   Populate the Fortran DLA descriptor array fCurrent with the contents
   of the input descriptor.
   */
   fDLADescr[SPICE_DLA_BWDIDX] = dladsc->bwdptr;
   fDLADescr[SPICE_DLA_FWDIDX] = dladsc->fwdptr;
   fDLADescr[SPICE_DLA_IBSIDX] = dladsc->ibase;
   fDLADescr[SPICE_DLA_ISZIDX] = dladsc->isize;
   fDLADescr[SPICE_DLA_DBSIDX] = dladsc->dbase;
   fDLADescr[SPICE_DLA_DSZIDX] = dladsc->dsize;
   fDLADescr[SPICE_DLA_CBSIDX] = dladsc->cbase;
   fDLADescr[SPICE_DLA_CSZIDX] = dladsc->csize;

   /*
   Store the requested item in an integer variable.
   */
   fItem  = item;

   /*
   Adjust the start value:  convert to Fortran-style indexing.
   */
   fStart = start + 1;

   /*
   Store the room value in an integer variable.
   */
   fRoom  = room;

   /*
   Call the f2c'd routine.
   */
   dski02_ ( ( integer      * ) &handle,
             ( integer      * ) fDLADescr,
             ( integer      * ) &fItem,
             ( integer      * ) &fStart,
             ( integer      * ) &fRoom,
             ( integer      * ) n,
             ( integer      * ) values     );


   chkout_c ( "dski02_c" );

} /* End dski02_c */
