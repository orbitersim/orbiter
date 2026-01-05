/*

-Procedure pckcov_c ( PCK coverage )

-Abstract

   Find the coverage window for a specified reference frame in a
   specified binary PCK file.

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
   DAF
   PCK
   TIME
   WINDOWS

-Keywords

   ORIENTATION
   TIME
   UTILITY

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"


   void pckcov_c ( ConstSpiceChar   * pckfnm,
                   SpiceInt           idcode,
                   SpiceCell        * cover   )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   pckfnm     I   Name of PCK file.
   idcode     I   Class ID code of PCK reference frame.
   cover     I-O  Window giving coverage in `pckfnm' for `idcode'.

-Detailed_Input

   pckfnm      is the name of a binary PCK file.

   idcode      is the integer frame class ID code of a PCK reference
               frame for which data are expected to exist in the
               specified PCK file.

   cover       is an initialized SPICE window data structure. `cover'
               optionally may contain coverage data on input; on output,
               the data already present in `cover' will be combined with
               coverage found for the reference frame designated by
               `idcode' in the file `pckfnm'.

               If `cover' contains no data on input, its size and
               cardinality still must be initialized.

               `cover' must be declared as a double precision SpiceCell.

               CSPICE provides the following macro, which declares and
               initializes the cell

                  SPICEDOUBLE_CELL        ( cover, COVERSZ );

               where COVERSZ is the maximum capacity of `cover'.

-Detailed_Output

   cover       is a SPICE window data structure which represents the
               merged coverage for the reference frame having frame
               class ID `idcode'. This is the set of time intervals for
               which data for `idcode' are present in the file `pckfnm',
               merged with the set of time intervals present in `cover' on
               input. The merged coverage is represented as the union of
               one or more disjoint time intervals. The window `cover'
               contains the pairs of endpoints of these intervals.

               The interval endpoints contained in `cover' are ephemeris
               times, expressed as seconds past J2000 TDB.

               See the -Examples section below for a complete example
               program showing how to retrieve the endpoints from `cover'.

-Parameters

   None.

-Exceptions

   1)  If the input file has transfer format, the error
       SPICE(INVALIDFORMAT) is signaled by a routine in the call tree
       of this routine.

   2)  If the input file is not a transfer file but has architecture
       other than DAF, the error SPICE(INVALIDARCHTYPE) is signaled
       by a routine in the call tree of this routine.

   3)  If the input file is a binary DAF file of type other than PCK,
       the error SPICE(INVALIDFILETYPE) is signaled by a routine in
       the call tree of this routine.

   4)  If the PCK file cannot be opened or read, an error is signaled
       by a routine in the call tree of this routine. The output
       window will not be modified.

   5)  If the size of the output window argument `cover' is
       insufficient to contain the actual number of intervals in the
       coverage window for `idcode', an error is signaled by a routine
       in the call tree of this routine.

   6)  If the `pckfnm' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   7)  If the `pckfnm' input string has zero length, the error
       SPICE(EMPTYSTRING) is signaled.

   8)  If the `cover' cell argument has a type other than
       SpiceDouble, the error SPICE(TYPEMISMATCH) is signaled.

-Files

   This routine reads a PCK file.

-Particulars

   This routine provides an API via which applications can determine
   the coverage a specified PCK file provides for a specified
   PCK class reference frame.

-Examples

   The numerical results shown for these examples may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) This example demonstrates combined usage of pckcov_c and the
      related PCK utility pckfrm_c.

      Display the coverage for each object in a specified PCK file.
      Find the set of objects in the file; for each object, find
      and display the coverage.


      Example code begins here.


      /.
         Program pckcov_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main()
      {
         /.
         Local parameters
         ./
         #define  FILSIZ         256
         #define  MAXIV          1000
         #define  WINSIZ         ( 2 * MAXIV )
         #define  TIMLEN         51
         #define  MAXOBJ         1000

         /.
         Local variables
         ./
         SPICEDOUBLE_CELL        ( cover, WINSIZ );
         SPICEINT_CELL           ( ids,   MAXOBJ );

         SpiceChar               lsk     [ FILSIZ ];
         SpiceChar               pckfnm  [ FILSIZ ];
         SpiceChar               timstr  [ TIMLEN ];

         SpiceDouble             b;
         SpiceDouble             e;

         SpiceInt                i;
         SpiceInt                j;
         SpiceInt                niv;
         SpiceInt                obj;


         /.
         Load a leapseconds kernel for output time conversion.
         pckcov_c itself does not require a leapseconds kernel.
         ./
         prompt_c ( "Name of leapseconds kernel > ", FILSIZ, lsk );
         furnsh_c ( lsk );

         /.
         Get name of PCK file.
         ./
         prompt_c ( "Name of PCK file           > ", FILSIZ, pckfnm    );

         /.
         Find the set of frames in the PCK file.
         ./
         pckfrm_c ( pckfnm, &ids );

         /.
         We want to display the coverage for each frame. Loop over
         the contents of the ID code set, find the coverage for
         each item in the set, and display the coverage.
         ./
         for ( i = 0;  i < card_c( &ids );  i++  )
         {
            /.
            Find the coverage window for the current frame.
            Empty the coverage window each time so we don't
            include data for the previous frame.
            ./
            obj  =  SPICE_CELL_ELEM_I( &ids, i );

            scard_c  ( 0,        &cover );
            pckcov_c ( pckfnm, obj, &cover );

            /.
            Get the number of intervals in the coverage window.
            ./
            niv = wncard_c ( &cover );

            /.
            Display a simple banner.
            ./
            printf ( "%s\n", "========================================" );

            printf ( "Coverage for frame %d\n", (int)obj );

            /.
            Convert the coverage interval start and stop times to TDB
            calendar strings.
            ./
            for ( j = 0;  j < niv;  j++  )
            {
               /.
               Get the endpoints of the jth interval.
               ./
               wnfetd_c ( &cover, j, &b, &e );

               /.
               Convert the endpoints to TDB calendar
               format time strings and display them.
               ./
               timout_c ( b,
                          "YYYY MON DD HR:MN:SC.### (TDB) ::TDB",
                          TIMLEN,
                          timstr                                  );

               printf ( "\n"
                        "Interval:  %d\n"
                        "Start:     %s\n",
                        (int)j,
                        timstr            );

               timout_c ( e,
                          "YYYY MON DD HR:MN:SC.### (TDB) ::TDB",
                          TIMLEN,
                          timstr                                  );
               printf ( "Stop:      %s\n", timstr );

            }

         }
         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, using the LSK file named naif0012.tls and the PCK file
      named earth_720101_070426.bpc, the output was:


      Name of leapseconds kernel > naif0012.tls
      Name of PCK file           > earth_720101_070426.bpc
      ========================================
      Coverage for frame 3000

      Interval:  0
      Start:     1962 JAN 20 00:00:41.184 (TDB)
      Stop:      2007 APR 26 00:01:05.185 (TDB)


   2) Find the coverage for the frame designated by `idcode'
      provided by the set of PCK files loaded via a metakernel.

      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File name: pckcov_ex2.tm

         This meta-kernel is intended to support operation of SPICE
         example programs. The kernels shown here should not be
         assumed to contain adequate or correct versions of data
         required by SPICE-based user applications.

         In order for an application to use this meta-kernel, the
         kernels referenced here must be present in the user's
         current working directory.

         The names and contents of the kernels referenced
         by this meta-kernel are as follows:

            File name                        Contents
            ---------                        --------
            earth_720101_070426.bpc          Earth historical
                                             binary PCK
            earth_070425_370426_predict.bpc  Earth predicted
                                             binary PCK
            naif0012.tls                     Leapseconds


         \begindata

         KERNELS_TO_LOAD = ( 'earth_070425_370426_predict.bpc',
                             'earth_720101_070426.bpc',
                             'naif0012.tls'                    )


         \begintext

         End of meta-kernel.


      Example code begins here.


      /.
         Program pckcov_ex2
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main()
      {

         /.
         Local parameters
         ./
         #define  FILSIZ         256
         #define  LNSIZE         81
         #define  MAXCOV         100000
         #define  WINSIZ         ( 2 * MAXCOV )
         #define  TIMLEN         51

         /.
         Local variables
         ./
         SPICEDOUBLE_CELL        ( cover, WINSIZ );

         SpiceBoolean            found;

         SpiceChar               file    [ FILSIZ ];
         SpiceChar               idch    [ LNSIZE ];
         SpiceChar               meta    [ FILSIZ ];
         SpiceChar               source  [ FILSIZ ];
         SpiceChar               timstr  [ TIMLEN ];
         SpiceChar               type    [ LNSIZE ];

         SpiceDouble             b;
         SpiceDouble             e;

         SpiceInt                count;
         SpiceInt                handle;
         SpiceInt                i;
         SpiceInt                idcode;
         SpiceInt                niv;


         /.
         Prompt for the metakernel name; load the metakernel.
         The metakernel lists the PCK files whose coverage
         for `idcode' we'd like to determine.  The metakernel
         must also specify a leapseconds kernel.
         ./
         prompt_c ( "Name of metakernel > ", FILSIZ, meta );
         furnsh_c ( meta );

         /.
         Get the ID code of interest.
         ./
         prompt_c ( "Enter ID code      > ", LNSIZE, idch );
         prsint_c ( idch,  &idcode );

         /.
         Find out how many kernels are loaded.  Loop over the
         kernels:  for each loaded PCK file, add its coverage
         for `idcode', if any, to the coverage window.
         ./
         ktotal_c ( "PCK", &count );

         for ( i = 0;  i < count;  i++  )
         {
            kdata_c  ( i,     "PCK",   FILSIZ,  LNSIZE,   FILSIZ,
                       file,  type,    source,  &handle,  &found );

            pckcov_c ( file,  idcode,  &cover );
         }

         /.
         Display results.

         Get the number of intervals in the coverage window.
         ./
         niv = wncard_c ( &cover );

         /.
         Display a simple banner.
         ./
         printf ( "\nCoverage for frame %d\n", (int)idcode );

         /.
         Convert the coverage interval start and stop times to TDB
         calendar strings.
         ./
         for ( i = 0;  i < niv;  i++  )
         {
            /.
            Get the endpoints of the ith interval.
            ./
            wnfetd_c ( &cover, i, &b, &e );

            /.
            Convert the endpoints to TDB calendar
            format time strings and display them.
            ./
            timout_c ( b,
                       "YYYY MON DD HR:MN:SC.### (TDB) ::TDB",
                       TIMLEN,
                       timstr                                  );

            printf ( "\n"
                     "Interval:  %d\n"
                     "Start:     %s\n",
                     (int)i,
                     timstr            );

            timout_c ( e,
                       "YYYY MON DD HR:MN:SC.### (TDB) ::TDB",
                       TIMLEN,
                       timstr                                  );
            printf ( "Stop:      %s\n", timstr );

         }
         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, using the meta-kernel named pckcov_ex2.tm provided
      above to find the coverage window for the ITRF93 frame using
      its ID code, "3000", the output was:


      Name of metakernel > pckcov_ex2.tm
      Enter ID code      > 3000

      Coverage for frame 3000

      Interval:  0
      Start:     1962 JAN 20 00:00:41.184 (TDB)
      Stop:      2037 JUL 17 00:01:05.183 (TDB)


-Restrictions

   1)  If an error occurs while this routine is updating the window
       `cover', the window may be corrupted.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.1.0, 25-AUG-2021 (JDR)

       Changed input argument name "pck" to "pckfnm" for consistency with other
       routines.

       Edited the header to comply with NAIF standard. Corrected short error
       message in entries #2 and #3 in -Exceptions section. Added examples'
       solution and meta-kernel for example #2.

       Extended description of argument "cover" in -Detailed_Input to include
       type and preferred declaration method.

       Added entry #8 to -Exceptions section.

   -CSPICE Version 1.0.2, 12-JUL-2016 (EDW)

       Edit to example program to use "%d" with explicit casts
       to int for printing SpiceInts with printf.

   -CSPICE Version 1.0.1, 01-JUL-2014 (NJB)

       Updated index entries.

   -CSPICE Version 1.0.0, 30-NOV-2007 (NJB)

-Index_Entries

   get coverage window for binary PCK reference frame
   get coverage start and stop time for binary PCK frame

-&
*/

{ /* Begin pckcov_c */


   /*
   Participate in error tracing.
   */
   if ( return_c() )
   {
      return;
   }
   chkin_c ( "pckcov_c" );


   /*
   Check the input string `pckfnm' to make sure the pointer is non-null
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "pckcov_c", pckfnm );

   /*
   Make sure cell data type is d.p.
   */
   CELLTYPECHK ( CHK_STANDARD, "pckcov_c", SPICE_DP, cover );

   /*
   Initialize the cell if necessary.
   */
   CELLINIT ( cover );

   /*
   Call the f2c'd Fortran routine.
   */
   pckcov_ ( ( char       * ) pckfnm,
             ( integer    * ) &idcode,
             ( doublereal * ) (cover->base),
             ( ftnlen       ) strlen(pckfnm)   );

   /*
   Sync the output cell.
   */
   if ( !failed_c() )
   {
      zzsynccl_c ( F2C, cover );
   }


   chkout_c ( "pckcov_c" );

} /* End pckcov_c */
