/*

-Procedure furnsh_c ( Furnish a program with SPICE kernels )

-Abstract

   Load one or more SPICE kernels into a program.

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

   None.

-Keywords

   UTILITY

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"


   void furnsh_c ( ConstSpiceChar  * file )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   file       I   Name of SPICE kernel file (text or binary).

-Detailed_Input

   file        is the name of a SPICE kernel file. The file may be
               either binary or text. If the file is a binary SPICE
               kernel it will be loaded into the appropriate SPICE
               subsystem. If `file' is a SPICE text kernel it will be
               loaded into the kernel pool. If `file' is a SPICE
               meta-kernel containing initialization instructions
               (through use of the correct kernel pool variables), the
               files specified in those variables will be loaded into
               the appropriate SPICE subsystem.

               The SPICE text kernel format supports association of
               names and data values using a "keyword = value" format.
               The keyword-value pairs thus defined are called "kernel
               variables."

               While any information can be placed in a text kernel
               file, the following string valued kernel variables are
               recognized by SPICE as meta-kernel keywords:

                  KERNELS_TO_LOAD
                  PATH_SYMBOLS
                  PATH_VALUES

               Each kernel variable is discussed below.

                  KERNELS_TO_LOAD   is a list of SPICE kernels to be
                                    loaded into a program. If file
                                    names do not fit within the kernel
                                    pool 80 character limit, they may be
                                    continued to subsequent array
                                    elements by placing the continuation
                                    character ('+') at the end of an
                                    element and then placing the
                                    remainder of the file name in the
                                    next array element. (See the
                                    examples below for an illustration
                                    of this technique or consult the
                                    routine stpool_c for further
                                    details.)

                                    Alternatively you may use a
                                    PATH_SYMBOL (see below) to
                                    substitute for some part of a file
                                    name.

                  PATH_SYMBOLS      is a list of strings (without
                                    embedded blanks), which if
                                    encountered following the '$'
                                    character will be replaced with the
                                    corresponding PATH_VALUES string.
                                    Note that PATH_SYMBOLS are
                                    interpreted only in the
                                    KERNELS_TO_LOAD variable. There must
                                    be a one-to-one correspondence
                                    between the values supplied for
                                    PATH_SYMBOLS and PATH_VALUES.

                  PATH_VALUES       is a list of expansions to use when
                                    PATH_SYMBOLS are encountered. See
                                    the -Examples section for an
                                    illustration of use of PATH_SYMBOLS
                                    and PATH_VALUES.

               These kernel pool variables persist within the kernel
               pool only until all kernels associated with the
               variable KERNELS_TO_LOAD have been loaded. Once all
               specified kernels have been loaded, the variables
               KERNELS_TO_LOAD, PATH_SYMBOLS and PATH_VALUES are
               removed from the kernel pool.

-Detailed_Output

   None. The routine loads various SPICE kernels for use by your
   application.

-Parameters

   FILSIZ     is the maximum file name length that can be accommodated
              by the kernel pool. FILSIZ is currently set to 255.

-Exceptions

   1)  If a problem is encountered while trying to load `file', an
       error is signaled by a routine in the call tree of this
       routine.

   2)  If the input `file' is a meta-kernel and some file in the
       KERNELS_TO_LOAD assignment cannot be found, or if an error
       occurs while trying to load a file specified by this
       assignment, the error is signaled by a routine in the call
       tree of this routine, and this routine will return. Any files
       loaded prior to encountering the failure, including those
       referenced by the KERNELS_TO_LOAD assignment, will remain
       loaded.

   3)  If an attempt to load a text kernel fails while the kernel is
       being parsed, any kernel variable assignments made before
       the failure occurred will be retained in the kernel pool.

   4)  If a PATH_SYMBOLS assignment is specified without a
       corresponding PATH_VALUES assignment, the error
       SPICE(NOPATHVALUE) is signaled by a routine in the call tree
       of this routine.

   5)  If a meta-text kernel is supplied to furnsh_c that contains
       instructions specifying that another meta-text kernel be
       loaded, the error SPICE(RECURSIVELOADING) is signaled by a
       routine in the call tree of this routine.

   6)  If the input file name has non-blank length exceeding FILSIZ
       characters, the error SPICE(FILENAMETOOLONG) is signaled by a
       routine in the call tree of this routine.

   7)  If the input file is a meta-kernel and some file in the
       KERNELS_TO_LOAD assignment has name length exceeding FILSIZ
       characters, the error SPICE(FILENAMETOOLONG) is signaled by a
       routine in the call tree of this routine.

   8)  If the input file is a meta-kernel and some value in the
       PATH_VALUES assignment has length exceeding FILSIZ characters,
       the error SPICE(PATHTOOLONG) is signaled by a routine in the
       call tree of this routine.

   9)  If the input file is a meta-kernel and some file in the
       KERNELS_TO_LOAD assignment has, after symbol substitution,
       combined name and path length exceeding FILSIZ characters, the
       error SPICE(FILENAMETOOLONG) is signaled by a routine in the
       call tree of this routine.

   10) If a kernel pool variable name length exceeds its maximum
       allowed length (see Kernel Required Reading, kernel.req), an
       error is signaled by a routine in the call tree of this
       routine.

   11) If the `file' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   12) If the `file' input string has zero length, the error
       SPICE(EMPTYSTRING) is signaled.

-Files

   The input file is examined and loaded into the appropriate
   SPICE subsystem. If the file is a meta-kernel, any kernels
   specified by the KERNELS_TO_LOAD keyword (and if present,
   the PATH_SYMBOLS and PATH_VALUES keywords) are loaded as well.

   In this version of the toolkit the maximum number of kernels that
   can loaded together is limited to 5300. Each time a kernel is loaded
   via furnsh_c, an internal kernel database entry is created for that
   kernel. If a meta-kernel is loaded, a database entry is created for
   the meta-kernel itself and for all files referenced in the
   meta-kernel's KERNELS_TO_LOAD specification. Unloading a kernel or
   meta-kernel deletes database entries created when the file was
   loaded.

   The value above is an upper bound on number of SPICE kernels that
   can be loaded at any time via the furnsh_c interface, but the number
   of kernels that can be loaded may be smaller, since re-loading a
   loaded kernel or meta-kernel results in creation of additional
   database entries.

   Kernels loaded via furnsh_c are subject to constraints imposed by
   lower-level subsystems. The binary kernel systems (SPK, CK, binary
   PCK, EK, and DSK) have their own limits on the maximum number of
   kernels that may be loaded.

   The total number of DAF-based files (this set includes SPKs, CKs,
   and binary PCKs) and DAS-based files (this set includes EKs and
   DSKs) that may be loaded at any time may not exceed 5000. This
   limit applies whether the files are loaded via furnsh_c or
   lower-level loaders such as spklef_c or dafopr_c. File access
   performance normally will degrade slightly as the number of loaded
   kernels increases.

-Particulars

   This routine provides a uniform interface to the SPICE kernel
   loading systems. It allows you to easily assemble a list of
   SPICE kernels required by your application and to modify that set
   without modifying the source code of programs that make use of
   these kernels.

   Text kernels input to this routine need not have native line
   terminators for the platform. Lower level CSPICE routines can
   read and process non-native text files. This functionality does
   not exist in the Fortran SPICELIB.

   Only text kernel readers include the non-native read capability,
   (ldpool_c and furnsh_c), the generic text file line reader, rdtext_c
   requires native text files.

   Please refer to kernel.req for additional information.

   Kernel pool variable names are restricted to a length of 32
   characters or less.

-Examples

   The numerical results shown for these examples may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Load the leapseconds kernel naif0007.tls and the planetary
      ephemeris SPK file de405s.bsp.

         furnsh_c ( "naif0007.tls" );
         furnsh_c ( "de405s.bsp"   );


   2) This example illustrates how you could create a meta-kernel
      file for a program that requires several text and binary
      kernels.

      First create a list of the kernels you need in a text file as
      shown below.


         KPL/MK

         File name: furnsh_ex2.tm

         Here are the SPICE kernels required for my application
         program.

         Note that kernels are loaded in the order listed. Thus
         we need to list the highest priority kernel last.


         \begindata

         KERNELS_TO_LOAD = (

            '/home/mydir/kernels/spk/lowest_priority.bsp',
            '/home/mydir/kernels/spk/next_priority.bsp',
            '/home/mydir/kernels/spk/highest_priority.bsp',
            '/home/mydir/kernels/text/leapsecond.ker',
            '/home/mydir/kernels+',
            '/custom+',
            '/kernel_data/constants.ker',
            '/home/mydir/kernels/text/sclk.tsc',
            '/home/mydir/kernels/ck/c-kernel.bc' )

         \begintext

         End of meta-kernel


      Note that the file name

         /home/mydir/kernels/custom/kernel_data/constants.ker

      is continued across several lines in the right hand side of
      the assignment of the kernel variable KERNELS_TO_LOAD.

      Once you've created your list of kernels, call furnsh_c near the
      beginning of your application program to load the meta-kernel
      automatically at program start up.

         furnsh_c ( "furnsh_ex2.tm" );

      This will cause each of the kernels listed in your meta-kernel
      to be loaded.


   3) This example illustrates how you can simplify the previous
      kernel list by using PATH_SYMBOLS.


         KPL/MK

         File name: furnsh_ex3.tm

         Here are the SPICE kernels required for my application
         program.


         We are going to let A substitute for the directory that
         contains SPK files; B substitute for the directory that
         contains C-kernels; and C substitute for the directory that
         contains text kernels. And we'll let D substitute for
         a "custom" directory that contains a special planetary
         constants kernel made just for our mission.

         Note that our PATH_VALUES and the corresponding
         PATH_SYMBOLS must be listed in the same order.


         \begindata

         PATH_VALUES  = ( '/home/mydir/kernels/spk',
                          '/home/mydir/kernels/ck',
                          '/home/mydir/kernels/text',
                          '/home/mydir/kernels/custom/kernel_data' )

         PATH_SYMBOLS = ( 'A',
                          'B',
                          'C',
                          'D'  )

         KERNELS_TO_LOAD = (  '$A/lowest_priority.bsp',
                              '$A/next_priority.bsp',
                              '$A/highest_priority.bsp',
                              '$C/leapsecond.ker',
                              '$D/constants.ker',
                              '$C/sclk.tsc',
                              '$B/c-kernel.bc'         )

         \begintext

         End of meta-kernel


   4) This example illustrates continuation of path values. The
      meta-kernel shown here is a modified version of that from
      example 3.


         KPL/MK

         File name: furnsh_ex4.tm

         Here are the SPICE kernels required for my application
         program.

         We are going to let A substitute for the directory that
         contains SPK files; B substitute for the directory that
         contains C-kernels; and C substitute for the directory that
         contains text kernels. And we'll let D substitute for
         a "custom" directory that contains a special planetary
         constants kernel made just for our mission.

         Note that our PATH_VALUES and the corresponding
         PATH_SYMBOLS must be listed in the same order.

         The values for path symbols A and D are continued over
         multiple lines.

         \begindata

         PATH_VALUES  = ( '/very_long_top_level_path_name/mydir/+',
                          'kernels/spk',
                          '/home/mydir/kernels/ck',
                          '/home/mydir/kernels/text',
                          '/very_long_top_level_path_name+',
                          '/mydir/kernels/custom+',
                          '/kernel_data'                )

         PATH_SYMBOLS = ( 'A',
                          'B',
                          'C',
                          'D'  )

         KERNELS_TO_LOAD = (  '$A/lowest_priority.bsp',
                              '$A/next_priority.bsp',
                              '$A/highest_priority.bsp',
                              '$C/leapsecond.ker',
                              '$D/constants.ker',
                              '$C/sclk.tsc',
                              '$B/c-kernel.bc'         )

         \begintext

         End of meta-kernel


   5) Load a meta-kernel containing three kernels, and separately,
      a text kernel and a binary PCK. Count the number of loaded
      files before and after calling kclear_c.


      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File name: furnsh_ex5.tm

         This meta-kernel is intended to support operation of SPICE
         example programs. The kernels shown here should not be
         assumed to contain adequate or correct versions of data
         required by SPICE-based user applications.

         In order for an application to use this meta-kernel, the
         kernels referenced here must be present in the user's
         current working directory.

         The names and contents of the kernels referenced
         by this meta-kernel are as follows:

            File name                     Contents
            ---------                     --------
            de421.bsp                     Planetary ephemeris
            pck00009.tpc                  Planet orientation and
                                          radii
            naif0012.tls                  Leapseconds

         \begindata

            KERNELS_TO_LOAD = ( 'de421.bsp',
                                'naif0012.tls',
                                'pck00009.tpc' )

         \begintext

         End of meta-kernel


      Use the PCK kernel below as the binary PCK required for the
      example.

         earth_latest_high_prec.bpc


      Use the FK kernel below as the text kernel required for the
      example.

         RSSD0002.TF


      Example code begins here.


      /.
         Program furnsh_ex5
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local variables.
         ./
         SpiceInt             count;

         /.
         Load several kernel files.
         ./
         furnsh_c ( "furnsh_ex5.tm"              );
         furnsh_c ( "RSSD0002.TF"                );
         furnsh_c ( "earth_latest_high_prec.bpc" );

         /.
         Count the number of loaded kernel files.
         ./
         ktotal_c ( "ALL", &count );

         printf( "The total number of kernels after final furnsh_c:  %1d\n",
                                                                     count );

         /.
         Clear the KEEPER system, retrieve the number of loaded
         after the clear.
         ./
         kclear_c ();

         ktotal_c ( "ALL", &count );

         printf( "The total number of kernels after kclear_c      :  %1d\n",
                                                                     count );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      The total number of kernels after final furnsh_c:  6
      The total number of kernels after kclear_c      :  0


-Restrictions

   1)  A meta-kernel cannot reference another meta-kernel.

   2)  Failure during an attempt to load a text kernel or a
       meta-kernel can result in a subset of the intended kernel
       variables being set or a subset of the intended files
       being loaded. furnsh_c does not "clean up" so as to undo the
       effects of a failed load operation.

   3)  When a kernel is specified with a relative path, this path 
       should be valid at the time when furnsh_c is called and stay 
       valid for the rest of the application run. This is required
       because SPICE stores kernel names as provided by the caller
       and uses them to open and close binary kernels as needed 
       by the DAF/DAS handle manager subsystem (behind the scenes,
       to allow reading many more binary kernels than available
       logical units), and to automatically reload into the POOL
       the rest of text kernels that should stay loaded when a 
       particular text kernel is unloaded.
       
       Changing the working directory from within an application 
       during an application run after calling furnsh_c to load 
       kernels specified using relative paths is likely to 
       invalidate stored paths and prevent open/close and unload
       operations mentioned above. A simple workaround when this
       is needed is to specify kernels using absolute paths.

-Literature_References

   None.

-Author_and_Institution

   C.H. Acton          (JPL)
   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   B.V. Semenov        (JPL)
   W.L. Taber          (JPL)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.3.5, 29-DEC-2021 (JDR)

       Edited the header to comply with NAIF standard. Added extra
       examples, including a complete code example.

       Added entries #5 to #9 to -Exceptions section, and extended
       entry #10.

       Added FILSIZ description to -Parameters section.

       Added a restriction about specifying kernels using relative 
       paths to the -Restrictions section. 

   -CSPICE Version 1.3.4, 01-FEB-2017 (BVS)

       Updated discussion in the -Files section to mention the maximum
       number of kernels that can be loaded together.

   -CSPICE Version 1.3.3, 01-JUL-2014 (NJB)

       Updated discussion of partially completed kernel loading.

   -CSPICE Version 1.3.2, 10-FEB-2010 (EDW)

       Corrected header section order. Added mention of the
       restriction on kernel pool variable names to 32 characters
       or less.

   -CSPICE Version 1.0.4, 17-OCT-2005 (EDW)

       Added text to -Particulars section informing of the
       non-native kernel text file reading capability.

   -CSPICE Version 1.0.3, 29-JUL-2003 (NJB) (CHA)

       Numerous updates to improve clarity. Some corrections
       were made.

   -CSPICE Version 1.0.2, 03-JUL-2002 (NJB)

       Documentation fix: corrected second code example. The example
       previously used the kernel variable PATH_NAMES; that name has been
       replaced with the correct name PATH_VALUES.

   -CSPICE Version 1.0.1, 13-APR-2000 (NJB)

       Replaced single quotes with double quotes in a code example.

   -CSPICE Version 1.0.0, 01-SEP-1999 (NJB) (WLT)

-Index_Entries

   Load SPICE data from a list of items

-&
*/

{ /* Begin furnsh_c */



   /*
   Participate in error tracing.
   */
   chkin_c ( "furnsh_c" );


   /*
   Check the input filename to make sure the pointer is non-null
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "furnsh_c", file );


   /*
   Call the f2c'd Fortran routine.
   */
   furnsh_ ( ( char   * ) file,
             ( ftnlen   ) strlen(file) );


   chkout_c ( "furnsh_c" );

} /* End furnsh_c */
