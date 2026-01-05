/*
-Procedure err ( CSPICE version of f2c routine err )

-Abstract

   Collection of routines used by the f2c error mechanism.

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

   CSPICE

-Keywords

*/

#ifndef NON_UNIX_STDIO
#define _INCLUDE_POSIX_SOURCE   /* for HP-UX */
#define _INCLUDE_XOPEN_SOURCE   /* for HP-UX */
#include "sys/types.h"
#include "sys/stat.h"
#endif

#include "f2c.h"

#ifdef KR_headers
extern char *malloc();
#else
#undef abs
#undef min
#undef max
#include "stdlib.h"
#endif

#include "fio.h"
#include "fmt.h"   /* for struct syl */

/*
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

   Routines defined in this file:

   int f__canseek(FILE *f)
   void f__fatal(int n, char *s)
   VOID f_init(Void)
   int f__nowreading(unit *x)
   int f__nowwriting(unit *x)
   int err__fl(int f, int m, char *s)

-Examples

   None.

-Restrictions

   None.

-Literature_References

   [1] f2c. (n.d.). In Wikipedia. Retrieved March 27, 2014,
       from http://en.wikipedia.org/wiki/F2c.

   [2] f2c primary website: http://www.netlib.org/f2c/.

-Author_and_Institution

    E.D. Wright    (JPL)

-Version

   -CSPICE Version 1.0.0, 28-OCT-2015 (EDW)

      Based on err.c from 1998 f2c library. Edits to eliminate
      warning signals from current generation compilers.

      Eliminated KR_headers ifdefs. CSPICE does not and will
      not support KR C.

-Index_Entries

   None.

-&
*/


/*global definitions*/
unit f__units[MXUNIT];   /*unit table*/
flag f__init;   /*0 on entry, 1 after initializations*/
cilist *f__elist;   /*active external io list*/
icilist *f__svic;   /*active internal io list*/
flag f__reading;   /*1 if reading, 0 if writing*/
flag f__cplus,f__cblank;
char *f__fmtbuf;
flag f__external;   /*1 if external io, 0 if internal */

int (*f__getn)(void);   /* for formatted input */
void (*f__putn)(int);   /* for formatted output */
int (*f__doed)(struct syl*, char*, ftnlen),(*f__doned)(struct syl*);
int (*f__dorevert)(void),(*f__donewrec)(void),(*f__doend)(void);

flag f__sequential;   /*1 if sequential io, 0 if direct*/
flag f__formatted;   /*1 if formatted io, 0 if unformatted*/
FILE *f__cf;   /*current file*/
unit *f__curunit;   /*current unit*/
int f__recpos;   /*place in current record*/
int f__cursor, f__hiwater, f__scale;
char *f__icptr;

/*error messages*/
char *F_err[] =
{
   "error in format",                 /* 100 */
   "illegal unit number",             /* 101 */
   "formatted io not allowed",        /* 102 */
   "unformatted io not allowed",      /* 103 */
   "direct io not allowed",           /* 104 */
   "sequential io not allowed",       /* 105 */
   "can't backspace file",            /* 106 */
   "null file name",                  /* 107 */
   "can't stat file",                 /* 108 */
   "unit not connected",              /* 109 */
   "off end of record",               /* 110 */
   "truncation failed in endfile",    /* 111 */
   "incomprehensible list input",     /* 112 */
   "out of free space",               /* 113 */
   "unit not connected",              /* 114 */
   "read unexpected character",       /* 115 */
   "bad logical input field",         /* 116 */
   "bad variable type",               /* 117 */
   "bad namelist name",               /* 118 */
   "variable not in namelist",        /* 119 */
   "no end record",                   /* 120 */
   "variable count incorrect",        /* 121 */
   "subscript for scalar variable",   /* 122 */
   "invalid array section",           /* 123 */
   "substring out of bounds",         /* 124 */
   "subscript out of bounds",         /* 125 */
   "can't read file",                 /* 126 */
   "can't write file",                /* 127 */
   "'new' file exists",               /* 128 */
   "can't append to file",            /* 129 */
   "non-positive record number"       /* 130 */
};
#define MAXERR (sizeof(F_err)/sizeof(char *)+100)


/*
Added type identifier for routine.  29-OCT-2015 (EDW)
*/
int f__canseek(FILE *f) /*SYSDEP*/
{
#ifdef NON_UNIX_STDIO
   return !isatty(fileno(f));
#else
   struct stat x;

   if (fstat(fileno(f),&x) < 0)
      return(0);
#ifdef S_IFMT
   switch(x.st_mode & S_IFMT) {
   case S_IFDIR:
   case S_IFREG:
      if(x.st_nlink > 0)   /* !pipe */
         return(1);
      else
         return(0);
   case S_IFCHR:
      if(isatty(fileno(f)))
         return(0);
      return(1);
#ifdef S_IFBLK
   case S_IFBLK:
      return(1);
#endif
   }
#else
#ifdef S_ISDIR
   /* POSIX version */
   if (S_ISREG(x.st_mode) || S_ISDIR(x.st_mode)) {
      if(x.st_nlink > 0)   /* !pipe */
         return(1);
      else
         return(0);
      }
   if (S_ISCHR(x.st_mode)) {
      if(isatty(fileno(f)))
         return(0);
      return(1);
      }
   if (S_ISBLK(x.st_mode))
      return(1);
#else
   Help! How does fstat work on this system?
#endif
#endif
   return(0);   /* who knows what it is? */
#endif
}


void f__fatal(int n, char *s)
{
   if(n<100 && n>=0) perror(s); /*SYSDEP*/
   else if(n >= (int)MAXERR || n < -1)
   {   fprintf(stderr,"%s: illegal error number %d\n",s,n);
   }
   else if(n == -1) fprintf(stderr,"%s: end of file\n",s);
   else
      fprintf(stderr,"%s: %s\n",s,F_err[n-100]);
   if (f__curunit) {
      fprintf(stderr,"apparent state: unit %ld ",f__curunit-f__units);

      /*
      Edited line to include %s in "(unnamed)" portion of format string.
      This edit eliminated the "data argument not used by format string"
      warning.

      29-OCT-2015 (EDW)
      */
      fprintf(stderr, f__curunit->ufnm ? "named %s\n" : "(unnamed) %s\n",
         f__curunit->ufnm);
      }
   else
      fprintf(stderr,"apparent state: internal I/O\n");
   if (f__fmtbuf)
      fprintf(stderr,"last format: %s\n",f__fmtbuf);
   fprintf(stderr,"lately %s %s %s %s",f__reading?"reading":"writing",
      f__sequential?"sequential":"direct",
      f__formatted?"formatted":"unformatted",
      f__external?"external":"internal");
   sig_die(" IO", 1);
}


/*initialization routine*/
VOID f_init(Void)
{   unit *p;

   f__init=1;
   p= &f__units[0];
   p->ufd=stderr;
   p->useek=f__canseek(stderr);
   p->ufmt=1;
   p->uwrt=1;
   p = &f__units[5];
   p->ufd=stdin;
   p->useek=f__canseek(stdin);
   p->ufmt=1;
   p->uwrt=0;
   p= &f__units[6];
   p->ufd=stdout;
   p->useek=f__canseek(stdout);
   p->ufmt=1;
   p->uwrt=1;
}


/*
Added type identifier for routine.  29-OCT-2015 (EDW)
*/
int f__nowreading(unit *x)
{
   long loc;
   int ufmt, urw;
   extern char *f__r_mode[], *f__w_mode[];

   if (x->urw & 1)
      goto done;
   if (!x->ufnm)
      goto cantread;
   ufmt = x->url ? 0 : x->ufmt;
   loc = ftell(x->ufd);
   urw = 3;
   if (!freopen(x->ufnm, f__w_mode[ufmt|2], x->ufd)) {
      urw = 1;
      if(!freopen(x->ufnm, f__r_mode[ufmt], x->ufd)) {
 cantread:
         errno = 126;
         return 1;
         }
      }
   fseek(x->ufd,loc,SEEK_SET);
   x->urw = urw;
 done:
   x->uwrt = 0;
   return 0;
}


/*
Added type identifier for routine.  29-OCT-2015 (EDW)
*/
int f__nowwriting(unit *x)
{
   long loc;
   int ufmt;
   extern char *f__w_mode[];

   if (x->urw & 2)
      goto done;
   if (!x->ufnm)
      goto cantwrite;
   ufmt = x->url ? 0 : x->ufmt;
   if (x->uwrt == 3) { /* just did write, rewind */
      if (!(f__cf = x->ufd =
            freopen(x->ufnm,f__w_mode[ufmt],x->ufd)))
         goto cantwrite;
      x->urw = 2;
      }
   else {
      loc=ftell(x->ufd);
      if (!(f__cf = x->ufd =
         freopen(x->ufnm, f__w_mode[ufmt |= 2], x->ufd)))
         {
         x->ufd = NULL;
 cantwrite:
         errno = 127;
         return(1);
         }
      x->urw = 3;
      fseek(x->ufd,loc,SEEK_SET);
      }
 done:
   x->uwrt = 1;
   return 0;
}


int err__fl(int f, int m, char *s)
{
   if (!f)
      f__fatal(m, s);
   if (f__doend)
      (*f__doend)();
   return errno = m;
}


