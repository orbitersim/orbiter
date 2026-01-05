/*
-Procedure s_rnge (Array bounds overrun error response)

-Abstract

   Called when a subscript is out of range.

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

   None.

-Brief_I/O

   None.

-Detailed_Input

   None.

-Detailed_Output

   None.

-Parameters

   None.

-Exceptions

   None.

-Files

   None.

-Particulars

   The f2c application library provides an option to add code to the f2c
   converted routines to detect array overruns. When such an condition occurs, 
   the array check code executes this function. The f2c library s_rnge.c 
   streams an error message to stderr then executes an abort. This action 
   has proven inconvenient with CSPICE since the error output lacks a call 
   traceback. This version of s_rnge.c includes the error subsystem 
   traceback in output. 

-Examples

   None.

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman    (JPL)
   E.D. Wright     (JPL)

-Version

   -CSPICE Version 1.1.0, 03-APR-2009 (EDW)

      Rewrote routine to a NAIF format and to also output the SPICE error 
      subsystem call traceback.

   -CSPICE Version 1.0.0, 06-FEB-1999 (NJB)

      The statement
   
         return 0;
      
      for the normal C case was added to suppress compilation warnings.

-Index_Entries

   subscript index out-of-range

-&      
*/

#include <string.h>
#include <stdio.h>
#include "SpiceUsr.h"
#include "SpiceZst.h"


/* 
Undefine min and max macros to prevent a macro redefine warning
from the min and max defintions in f2c.h. 
*/
#undef min
#undef max

#include "f2c.h"

#define     ERRLEN              32
#define     TRC_LEN             32
#define     MAXMOD              100


/*
CSPICE routines cannot include both SpiceZfc.h and f2c.h.
Explicitly provide prototypes for the f2c library and 
CSPICE routines.
*/
VOID  sig_die(char*,int);
int   trcdep_(integer *depth);
int   trcnam_(integer *index, char *name__, ftnlen name_len);


integer s_rnge(char *varn, ftnint offset, char *procn, ftnint line)
  {

   SpiceChar      errproc[ERRLEN];
   SpiceChar      errvar [ERRLEN];
   SpiceChar      trname [TRC_LEN];

   /*
   Define an error message string for the case if the trcdep_
   call returns a value larger than MAXMOD.
   */
   SpiceChar    * depth_err = "SPICE(BUG): The trcdep_ routine "
                              "returned a depth, %i, larger than the "
                              "maximum allowed depth, %i. This error should "
                              "never signal. Please contact NAIF.\n\n";

   /*
   Define the error message for the subscript/index out of range.
   This string remains similar but not identical to the original
   f2c error message.
   */
   SpiceChar    * index_err = "SPICE(BADSUBSCRIPT): Subscript out "
                              "of range on file line %ld, procedure "
                              "\"%s\". Attempt to access element %ld "
                              "of variable \"%s\".\n\n";

   SpiceInt    depth;
   SpiceChar   trlist[MAXMOD*TRC_LEN];

   SpiceInt i;
   SpiceInt j;

   (void) memset( trlist, 0, MAXMOD*TRC_LEN );

   /* Set a loop counter for use as an index. */
   j = 0;

   /*
   Extract from 'procn' the name of the routine executing when the error
   occurred.

   Ensure the loop does not exceed ERRLEN.
   */
   while((i = *procn) && i != '_' && i != ' ' && j < ERRLEN )
      {
      errproc[j] = *procn++;
      j++;
      }

   /* 
   Properly terminate the 'errproc' string. 
   */
   errproc[j] = '\0';

   /* Reset the loop counter for 'errvar' (error variable name). */
   j = 0;

   /*
   Extract from 'varn' the name of the variable with the bad subscript.

   Ensure the loop does not exceed ERRLEN.
   */
   while((i = *varn) && i != ' ' && j < ERRLEN )
      {
      errvar[j] = *varn++;
      j++;
      }

   /*
   Properly terminate the 'errvar' string.
   */
   errvar[j] = '\0';

   (void) fprintf(stderr, index_err,
                          (long)line,
                          errproc,
                          (long)(offset+1),
                          errvar );

   /*
   Create the traceback string so the user will have some information 
   describing the program flow leading to this error.
   */

   /*
   Retrieve the depth of the call traceback stack.
   */
   (void) trcdep_( &depth );

   /*
   Check 'depth' as less-than or equal-to MAXMOD. Output an error
   if 'depth' greater-than MAXMOD.
   */
   if ( depth > MAXMOD )
      {
      (void) fprintf( stderr, depth_err, depth, MAXMOD );
      }
   else
      {

      /*
      Loop over the number of items in the trace list.
      Index starts at 1 as trcnam_ is an f2c'd routine.
      */
      for ( i=1; i<= depth; i++)
         {
   
         /*
         Retrieve the name (as a FORTRAN string) of the ith routine's name
         from the trace stack. No SPICE call name has a string length longer
         than TRC_LEN characters.
         */
         (void) trcnam_( (integer *) &i, trname, (ftnlen) TRC_LEN );
   
         /*
         The f2c code returns a FORTRAN type string, so null terminate
         the string for C.
         */
         F2C_ConvertStr( TRC_LEN, trname);

         /*
         Create the trace list string by concatenation. Add '->' as a
         marker between the routine names except on the first pass through
         the loop.
         */
         if ( i != 1 )
            {
            (void) strcat( trlist, "->" );
            }
         (void) strcat( trlist, trname );
         }
   
      (void) fprintf( stderr, "A traceback follows. The name of the "
                              "highest level module is first.\n%s", 
                               trlist );

      }

   sig_die("", 1);
 
   return 0;

   }


