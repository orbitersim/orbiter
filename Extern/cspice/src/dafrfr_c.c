/*

-Procedure dafrfr_c ( DAF, read file record )

-Abstract

   Read the contents of the file record of a DAF.

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

   DAF

-Keywords

   FILES

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"

   void dafrfr_c ( SpiceInt     handle,
                   SpiceInt     ifnlen,
                   SpiceInt   * nd,
                   SpiceInt   * ni,
                   SpiceChar  * ifname,
                   SpiceInt   * fward,
                   SpiceInt   * bward,
                   SpiceInt   * free    )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   handle     I   Handle of an open DAF file.
   ifnlen     I   Available room in the output string `ifname'.
   nd         O   Number of double precision components in summaries.
   ni         O   Number of integer components in summaries.
   ifname     O   Internal file name.
   fward      O   Forward list pointer.
   bward      O   Backward list pointer.
   free       O   Free address pointer.

-Detailed_Input

   handle      is the handle assigned to a DAF file opened for
               reading.

   ifnlen      is the maximum number of characters that can be
               accommodated in the output string `ifname'. This count
               includes room for the terminating null character.
               DAF internal file names may contain up to 60
               characters, so `ifnlen' normally should be set to 61.

-Detailed_Output

   nd,
   ni          are the numbers of double precision and integer
               components, respectively, in each array summary in
               the specified file.

   ifname      is the internal file name stored in the first
               (or file) record of the specified file. `ifname'
               should be declared with the length specified by
               `ifnlen'.

   fward       is the forward list pointer. This points to the
               first summary record in the file. (Records between
               the first record and the first summary record are
               reserved when the file is created, and are invisible
               to DAF routines.)

               DAF list pointers are actually Fortran record numbers,
               and as such, start at one.

   bward       is the backward list pointer. This points
               to the final summary record in the file.


   free        is the free address pointer. This contains the
               first free address in the file. (That is, the
               initial address of the next array to be added
               to the file.)

               `free' is a DAF address; for compatibility with
               SPICELIB, the range of DAF addresses starts at 1.

-Parameters

   None.

-Exceptions

   1)  If the handle passed to this routine is not the handle of an
       open DAF file, an error is signaled by a routine in the call
       tree of this routine.

   2)  If the specified DAF file is not open for read access, an
       error is signaled by a routine in the call tree of this
       routine.

   3)  If the specified record cannot (for some reason) be read, the
       error SPICE(DAFFRNOTFOUND) is signaled by a routine in the
       call tree of this routine.

-Files

   The input `handle' should refer to a DAF file open for read
   or write access.

-Particulars

   The file record of a DAF is the only record that contains
   any global information about the file. This record is created
   when the file is created, and is updated only when new arrays
   are added.

   Like character records, file records are not buffered.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) In the following example, the file record of a DAF is read
      to determine the first free address in the file.


      Use the SPK kernel below as input DAF file for the example.

         de421.bsp


      Example code begins here.


      /.
         Program dafrfr_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main ()
      {
         #define IFNLEN   61

         SpiceChar               ifname[IFNLEN];

         SpiceInt                bward;
         SpiceInt                free;
         SpiceInt                fward;
         SpiceInt                handle;
         SpiceInt                nd;
         SpiceInt                ni;
         SpiceChar             * kernel = "de421.bsp";

         dafopr_c ( kernel, &handle );

         dafrfr_c ( handle, IFNLEN, &nd, &ni, ifname, &fward,
                    &bward, &free                              );

         printf ( "First free DAF address is %d.\n", (int)free );

         /.
         Safely close the DAF.
         ./
         dafcls_c ( handle  );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      First free DAF address is 2098645.


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   K.R. Gehringer      (JPL)
   I.M. Underwood      (JPL)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.1.0, 13-AUG-2021 (JDR)

       Changed the input argument name "lenout" to "ifnlen" for
       consistency with other routines.

       Edited the header to comply with NAIF standard.

   -CSPICE Version 1.0.1, 28-JUN-2016 (EDW)

      Edit to Example code, SpiceInts output as ints using
      explicit casting.

   -CSPICE Version 1.0.0, 17-JUN-2009 (NJB) (KRG) (IMU)

-Index_Entries

   read DAF file record

-&
*/

{ /* Begin dafrfr_c */

   /*
   Participate in error tracing.
   */
   chkin_c ( "dafrfr_c" );

   dafrfr_ ( (integer *) &handle,
             (integer *) nd,
             (integer *) ni,
             (char    *) ifname,
             (integer *) fward,
             (integer *) bward,
             (integer *) free,
             (ftnlen   ) ifnlen-1 );

   /*
   Convert the internal file name to a C-style string.
   */
   F2C_ConvertStr ( ifnlen, ifname );


   chkout_c ( "dafrfr_c" );

} /* End dafrfr_c */
