/*

-Procedure dlabfs_c ( DLA, begin forward search )

-Abstract

   Begin a forward segment search in a DLA file.

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
   DLA

-Keywords

   DAS
   DLA
   FILES

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceDLA.h"


   void dlabfs_c ( SpiceInt          handle,
                   SpiceDLADescr   * dladsc,
                   SpiceBoolean    * found  )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   handle     I   Handle of open DLA file.
   dladsc     O   Descriptor of first segment in DLA file.
   found      O   Flag indicating whether a segment was found.

-Detailed_Input

   handle      is the DAS integer handle associated with the file to be
               searched. This handle is used to identify the file in
               subsequent calls to other DLA or DAS routines.

-Detailed_Output

   dladsc      is the descriptor of the first DLA segment in the
               file associated with `handle'.

               The segment descriptor layout is:

                +---------------+
                | BACKWARD PTR  | Linked list backward pointer
                +---------------+
                | FORWARD PTR   | Linked list forward pointer
                +---------------+
                | BASE INT ADDR | Base DAS integer address
                +---------------+
                | INT COMP SIZE | Size of integer segment component
                +---------------+
                | BASE DP ADDR  | Base DAS d.p. address
                +---------------+
                | DP COMP SIZE  | Size of d.p. segment component
                +---------------+
                | BASE CHR ADDR | Base DAS character address
                +---------------+
                | CHR COMP SIZE | Size of character segment component
                +---------------+

               `dladsc' is valid only if the output argument `found' is
               SPICETRUE.


   found       is a logical flag indicating whether a segment was
               found.  `found' has the value SPICETRUE if the file
               contains at least one segment; otherwise `found' is
               SPICEFALSE.

-Parameters

   None.

-Exceptions

   1)  If the input file handle is invalid, an error is signaled by a
       routine in the call tree of this routine.

   2)  If an error occurs while reading the DLA file, the error
       is signaled by a routine in the call tree of this
       routine.

-Files

   See description of input argument `handle'.

-Particulars

   DLA files are built using the DAS low-level format; DLA files are
   a specialized type of DAS file in which data are organized as a
   doubly linked list of segments. Each segment's data belong to
   contiguous components of character, double precision, and integer
   type.

   This routine supports forward traversal of a DLA file's segment
   list. Note that it is not necessary to call this routine to
   conduct a forward traversal; all that is necessary is to have
   access to the first descriptor in the file, which this routine
   provides.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as input,
   the compiler and supporting libraries, and the machine specific
   arithmetic implementation.

   1) Open a DLA file for read access, traverse the segment
      list from front to back, and display segment address
      and size attributes.

      Example code begins here.


      /.
         Program dlabfs_ex1
      ./

      #include "SpiceUsr.h"
      #include "SpiceDLA.h"
      #include <stdio.h>

      int main()
      {
         /.
         Local parameters
         ./
         #define FILSIZ           256

         /.
         Local variables
         ./
         SpiceBoolean            found;
         SpiceChar               fname  [ FILSIZ ];
         SpiceDLADescr           current;
         SpiceDLADescr           dladsc;
         SpiceInt                handle;
         SpiceInt                segno;

         /.
         Prompt for the name of the file to search.
         ./
         prompt_c ( "Name of DLA file > ", FILSIZ, fname );

         /.
         Open the DLA file for read access.  Since DLA
         files use the DAS architecture, we can use DAS
         routines to open and close the file.
         ./
         dasopr_c ( fname, &handle );

         /.
         Begin a forward search.  Let `dladsc' contain
         the descriptor of the first segment.
         ./
         segno = 0;

         dlabfs_c ( handle, &dladsc, &found );

         while ( found )
         {
            /.
            Display the contents of the current segment
            descriptor.
            ./

            ++segno;

            printf ( "\n"
                     "\n"
                     "Segment number = %d\n"
                     "\n"
                     "   Backward segment pointer         = %d\n"
                     "   Forward segment pointer          = %d\n"
                     "   Integer component base address   = %d\n"
                     "   Integer component size           = %d\n"
                     "   D.p. component base address      = %d\n"
                     "   D.p. component size              = %d\n"
                     "   Character component base address = %d\n"
                     "   Character component size         = %d\n",
                     (int)(segno),
                     (int)(dladsc.bwdptr),
                     (int)(dladsc.fwdptr),
                     (int)(dladsc.ibase),
                     (int)(dladsc.isize),
                     (int)(dladsc.dbase),
                     (int)(dladsc.dsize),
                     (int)(dladsc.cbase),
                     (int)(dladsc.csize)                           );

            /.
            Find the next segment.
            ./
            current = dladsc;

            dlafns_c ( handle, &current, &dladsc, &found );
         }

         /.
         Close the file using the DAS close routine.
         ./
         dascls_c ( handle );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, using the DSK file named phobos512.bds, the output
      was:


      Name of DLA file > phobos512.bds


      Segment number = 1

         Backward segment pointer         = -1
         Forward segment pointer          = -1
         Integer component base address   = 11
         Integer component size           = 29692614
         D.p. component base address      = 0
         D.p. component size              = 4737076
         Character component base address = 0
         Character component size         = 0


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.1.0, 10-AUG-2021 (JDR)

       Changed the output argument name "descr" to "dladsc" for
       consistency with other routines.

       Edited the header to comply with NAIF standard.

   -CSPICE Version 1.0.0, 09-JAN-2017 (EDW) (NJB)

       Edit to example program to use "%d" with explicit casts
       to int for printing SpiceInts with printf.

     23-JAN-2013 (NJB)

       Deleted third exception description from the -Exceptions
       header section.

     17-NOV-2009 (NJB)

       First release.

-Index_Entries

   begin forward search in DLA file

-&
*/

{ /* Begin dlabfs_c */

   /*
   Local variables
   */
   integer                 fDLADescr [ SPICE_DLA_DSCSIZ ];

   logical                 fnd;


   /*
   Participate in error tracing.
   */
   chkin_c ( "dlabfs_c" );


   /*
   Call the f2c'd routine.
   */
   dlabfs_ ( ( integer    * ) &handle,
             ( integer    * ) fDLADescr,
             ( logical    * ) &fnd       );

   /*
   Set the output SpiceBoolean found flag.
   */

   *found = (SpiceBoolean) fnd;


   /*
   If a segment was found, set the output descriptor.
   */
   if ( *found )
   {
      /*
      Assign values to the members of the output descriptor.
      */
      dladsc->bwdptr = fDLADescr[SPICE_DLA_BWDIDX];
      dladsc->fwdptr = fDLADescr[SPICE_DLA_FWDIDX];
      dladsc->ibase  = fDLADescr[SPICE_DLA_IBSIDX];
      dladsc->isize  = fDLADescr[SPICE_DLA_ISZIDX];
      dladsc->dbase  = fDLADescr[SPICE_DLA_DBSIDX];
      dladsc->dsize  = fDLADescr[SPICE_DLA_DSZIDX];
      dladsc->cbase  = fDLADescr[SPICE_DLA_CBSIDX];
      dladsc->csize  = fDLADescr[SPICE_DLA_CSZIDX];
   }


   chkout_c ( "dlabfs_c" );

} /* End dlabfs_c */
