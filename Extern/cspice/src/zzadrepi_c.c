/*

-Procedure zzadrpin_c (GF, progress report initialization adapter )

-Abstract
 
   Provide an f2c-style interface allowing f2c'd Fortran code to call a
   CSPICE-style GF progress reporting initialization function.
 
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
   #include "SpiceZst.h"
   #include "SpiceZad.h"
   #undef   zzadrepi_c

   int zzadrepi_c ( doublereal  * cnfine, 
                    char        * begmss,
                    char        * endmss,
                    ftnlen        begmssLen,
                    ftnlen        endmssLen  )

 
/*

-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   cnfine     I   Confinement window. 
   begmss     I   Beginning of the text portion of the output message. 
   endmss     I   End of the text portion of the output message. 
 
-Detailed_Input
 
   cnfine         is an array of type double containing a SPICE window.
                  This is the confinement window associated with some
                  GF root finding activity. It is used to determine how
                  much total time is being searched in order to find
                  the events of interest.
 

   begmss         is the beginning of the output message reported by
                  the routine gfrpwk_. This output message has the form
 
                     begmss ' xx.xx% ' endmss
 
                  `begmss' is a Fortran-style string.
                  
 
   endmss         is the last portion of the output message reported by 
                  the routine gfrpwk_. 

                  `endmss' is a Fortran-style string.


   begmssLen      is the length of the string `begmss'. The total
                  length of `begmss' must be less than 40 characters.

   endmssLen      is the length of the string `endmss'. The total
                  length of `endmss' must be less than 40 characters.

 
-Detailed_Output
 
   None. 
 
-Parameters
 
   None. 
 
-Exceptions
 
   1) A run-time error will result if this routine is called before
      a valid pointer to a CSPICE-style GF progress reporting
      initialization function has been stored via a call to zzadsave_c.

      The argument list of the stored function must match that of
      gfrepi_c.
    
-Files
 
   None. 
 
-Particulars
  
   This routine is meant to be passed to f2c'd Fortran GF code that
   requires a progress reporting initialization function input argument.
   The argument list of this routine matches that of the f2c'd routine

      gfrepi_

   This routine calls the CSPICE-style progress reporting
   initialization function passed into a CSPICE wrapper for an
   intermediate-level GF function. A pointer to this progress reporting
   initialization function must be stored via a call to zzadsave_c
   before this routine is called.
 
   The argument list of the function referenced by the saved pointer
   must match that of
 
      gfrepi_c

-Examples

   None.

-Restrictions
 
   None. 
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman   (JPL)
   W.L. Taber     (JPL) 
   I.M. Underwood (JPL) 
   L.S. Elson     (JPL) 
   E.D. Wright    (JPL)  
 
-Version
 
   -CSPICE Version 1.0.0, 29-MAR-2008 (NJB)

-Index_Entries
 
   provide status of a job in progress
 
-&
*/

{ /* Begin zzadrepi_c */



   /*
   Local variables 
   */
   SpiceCell               cnfineCell;

   SpiceChar             * prefstr;
   SpiceChar             * suffstr;

   SpiceInt                nBytes;

   /*
   Function pointer for CSPICE-style progress reporting
   initialization function:
   */
   void           ( * fPtr ) ( ConstSpiceCell *,
                               ConstSpiceChar *,
                               ConstSpiceChar * );

   /*
   Participate in error tracing.
   */
   if ( return_c() )
   {
      return ( 0 );
   }
   chkin_c ( "zzadrepi_c" );


   /*
   In order to call the saved, passed-in progress report 
   initialization function, we'll have to prepare
   some of the input arguments. We need C-style strings,
   and we need a confinement cell rather than an array.

   Set up the cell first.
   */
   cnfineCell.dtype   =  SPICE_DP;
   cnfineCell.length  =  0;
   cnfineCell.size    =  sized_(cnfine);
   cnfineCell.card    =  cardd_(cnfine);
   cnfineCell.isSet   =  SPICEFALSE;
   cnfineCell.adjust  =  SPICEFALSE;
   cnfineCell.init    =  SPICETRUE;
   cnfineCell.base    =  cnfine;
   cnfineCell.data    =  (SpiceDouble *)cnfine + SPICE_CELL_CTRLSZ;

   /*
   Allocate memory to hold C-style versions of the input strings. 
   
   First create a C-style prefix string.
   */
   nBytes  = (begmssLen+1) * sizeof(char);

   prefstr = (SpiceChar *) malloc( nBytes );

   if ( !prefstr )
   {
      setmsg_c ( "Could not allocate # bytes for progress report "
                 "prefix string."                                  );
      errint_c ( "#",  nBytes                                      );
      sigerr_c ( "SPICE(MALLOCFAILURE)"                            );
      chkout_c ( "zzadrepi_c"                                      );

      /*
      Return status of "0" because we don't want to invoke any f2c
      error handling mechanism that may exist.
      */
      return ( 0 );
   }

   strncpy ( prefstr, begmss, begmssLen );
   prefstr[begmssLen] = NULLCHAR;


   /*
   Create a C-style suffix string. 
   */
   nBytes  = (endmssLen+1) * sizeof(char);

   suffstr = (SpiceChar *) malloc( nBytes );

   if ( !suffstr )
   {
      /*
      Free the dynamically allocated prefix string before doing 
      anything else. 
      */
      free ( prefstr );


      setmsg_c ( "Could not allocate # bytes for progress report "
                 "suffix string."                                  );
      errint_c ( "#",  nBytes                                      );
      sigerr_c ( "SPICE(MALLOCFAILURE)"                            );
      chkout_c ( "zzadrepi_c"                                      );

      return   ( 0 );
   }

   strncpy ( suffstr, endmss, endmssLen );
   suffstr[endmssLen] = NULLCHAR;

   /*
   Retrieve the stored pointer for the passed-in function; cast
   the pointer from (void *) to that of a function whose argument
   list matches that of gfrepi_c.
   */

   fPtr = (  void (*) ( ConstSpiceCell *, 
                        ConstSpiceChar *,
                        ConstSpiceChar *  )  )  zzadget_c ( UDREPI );
   /*
   At this point we have the inputs required by the saved
   GF progress report initialization function.
   */
   
   ( *fPtr ) ( (ConstSpiceCell *) &cnfineCell, 
               (ConstSpiceChar *) prefstr, 
               (ConstSpiceChar *) suffstr      );

   /*
   Free the dynamically allocated strings. 
   */
   free ( prefstr );
   free ( suffstr );


   chkout_c ( "zzadrepi_c" );

   return ( 0 );

} /* End zzadrepi_c */
