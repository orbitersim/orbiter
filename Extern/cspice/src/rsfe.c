/*
-Procedure rsfe ( SPICE version of f2c library file rsfe.c )

-Abstract

   This file replaces the standard f2c rsfe.c library file. The functions
   below contain modifications to enable proper reading of non-native
   text files.

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

   None

-Keywords

   kernel reader non-native

*/

#include "f2c.h"
#include "fio.h"
#include "fmt.h"

/*
   The variable read_non_native is set via the function zzsetnnread_.
   This variable has file scope; functions in this file use it 
   to decide whether to handle non-native line termination.
   The functions rdknew_ and rdkdat_ should turn on non-native
   line termination handling before calling rdtext_ and turn this
   feature off immediately after rdtext_ returns.
*/
static logical read_non_native = 0;

logical zzcheckeol ( int ch );
void    zzsetnnread_( logical * on );

/*

-Brief_I/O

   None
   
-Detailed_Input

   None

-Detailed_Output

   None

-Parameters

   None

-Exceptions

   None.

-Files

   None

-Particulars

   The CSPICE implementation of the SPICE toolkit now includes the 
   capability to read text file containing non-native line terminators. NAIF
   applied this capability only to the text kernel readers.

-Examples

   None

-Restrictions

  1) Requires CSPICE f2c.h header file. Use this routine only as part 
     of the CSPICE library.

-Literature_References

-Author_and_Institution

   N. J. Bachman   (JPL)
   E. D. Wright    (JPL)

-Version

   -CSPICE Version 1.0.0, 28-SEP-2005 (EDW)

-Index_Entries

-&
*/

xrd_SL(Void)
   {

   int ch;
   
   if(!f__curunit->uend)
      {

      while ( !zzcheckeol( ch = getc(f__cf) )  )
         {
         
         if (ch == EOF) 
            {
            f__curunit->uend = 1;
            break;
            }

         }

      }

   f__cursor=f__recpos=0;
   
   return(1);
   }


x_getc(Void)
   {
   
   int ch;

   if( f__curunit->uend)
      {
      return(EOF);
      }

   ch = getc(f__cf);
   
   /*
   Does 'ch' represent an end-of-file, a \n or \r?
   If neither, return 'ch' to the caller.
   */
   if(ch!=EOF && !zzcheckeol(ch) )
      {
      f__recpos++;
      return(ch);
      }
   
   /*
   'ch' represents either a end-of-line or a newline, 
   return the platform native newline.
   */
   if( zzcheckeol(ch) )
      {
      (void) ungetc( '\n',f__cf);
      return('\n');
      }
      
   if(f__curunit->uend || feof(f__cf))
      {
      errno            = 0;
      f__curunit->uend = 1;
      return(-1);
      }

   return(-1);

   }


x_endp(Void)
   {
   xrd_SL();
   return f__curunit->uend == 1 ? EOF : 0;
   }

x_rev(Void)
   {
   (void) xrd_SL();
   return(0);
   }


#ifdef KR_headers
integer s_rsfe(a) cilist *a;
#else
integer s_rsfe(cilist *a)
#endif
   {
   int n;
   
   if(!f__init) 
      {
      f_init();
      }
   
   f__reading          = 1;
   f__sequential       = 1;
   f__formatted        = 1;
   f__external         = 1;

   if(n=c_sfe(a)) 
      {
      return(n);
      }

   f__elist            = a;
   f__cursor=f__recpos = 0;
   f__scale            = 0;
   f__fmtbuf           = a->cifmt;
   f__cf               = f__curunit->ufd;

   if(pars_f(f__fmtbuf)<0) 
      {
      err(a->cierr,100,"startio");
      }

   f__getn             = x_getc;
   f__doed             = rd_ed;
   f__doned            = rd_ned;
   
   fmt_bg();
   
   f__doend            = x_endp;
   f__donewrec         = xrd_SL;
   f__dorevert         = x_rev;
   f__cblank           = f__curunit->ublnk;
   f__cplus            = 0;
   
   if( f__curunit->uwrt && f__nowreading(f__curunit) )
      {
      err(a->cierr,errno,"read start");
      }
   
   if(f__curunit->uend)
      {
      err(f__elist->ciend,(EOF),"read start");
      }
   
   return(0);
   }


logical zzcheckeol ( int ch )
   {

   if ( read_non_native )
      {

      /*
      Handle non-native as well as native line terminators.
      */
      if (  ( ch == '\n' ) || ( ch == '\r' )  )
         {

         /*
         Treat the character 'ch' as a newline character.  This
         may result in extra blank lines being returned, but
         this does not interfere with correct parsing of the
         kernel.
         */
         return 1;

         }
      else
         {

         /*
         The character 'ch' does not represent a newline of any type.
         */
         return 0;

         }
       
       }
   else
      {
      
      /*
      Don't attempt to handle non-native line terminators.
      Just indicate whether 'ch' is a line terminator.
      */
      return ( (logical)( ch == '\n' ) );
      
      }

   }





/*

-Procedure zzsetnnread_( Set non-native text read state )


-Abstract

   The function zzsetnnread_ is the control mechanism for enabling
   or disabling handling non-native line termination.  All this
   function does is set the value of 'read_non_native' to the
   input value 'on'.

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

-Keywords

-Brief_I/O

   Variable  I/O  Description 
   --------  ---  -------------------------------------------------- 
   on         I   Logical indicating state to set function

-Detailed_Input

   on     a file scoped scalar boolean used to control program flow 
          above in x_getc

-Detailed_Output

   None.

-Parameters

   None.

-Exceptions

   None.

-Files

   None.

-Particulars

   None.

-Examples
    
   /.
   Signal rdtext_ to read non-platform native text files.
   ./
   zzsetnnread_(&c_true);

   rdtext_(kernel, first, &end, kernel_len, (ftnlen)80);

   /.
   Reset rdtext_ to read only platform native text files.
   ./  
   zzsetnnread_(&c_false);

-Restrictions

  1) Use this routine only as part of the CSPICE library.

-Literature_References

   None.

-Author_and_Institution

   N. J. Bachman   (JPL)
   E. D. Wright    (JPL)

-Version

   -CSPICE Version 1.0.0, 30-SEP-2005 (EDW)

-Index_Entries

-&
*/

void zzsetnnread_( logical * on )
   {
   read_non_native = *on;
   }





