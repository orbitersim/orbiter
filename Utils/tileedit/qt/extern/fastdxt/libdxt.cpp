/******************************************************************************
 * Fast DXT - a realtime DXT compression tool
 *
 * Author : Luc Renambot
 *
 * Copyright (C) 2007 Electronic Visualization Laboratory,
 * University of Illinois at Chicago
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either Version 2.1 of the License, or
 * (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser Public License along
 * with this library; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 *
 *****************************************************************************/

#include "libdxt.h"

#if defined(__APPLE__)
#define memalign(x,y) malloc((y))
#else
#include <malloc.h>
#endif

typedef struct _work_t {
	int width, height;
	int nbb;
	byte *in, *out;
} work_t;



void *slave1(void *arg)
{
	work_t *param = (work_t*) arg;
	int nbbytes = 0;
	CompressImageDXT1( param->in, param->out, param->width, param->height, nbbytes);
	param->nbb = nbbytes;
	return NULL;
}

void *slave5(void *arg)
{
	work_t *param = (work_t*) arg;
	int nbbytes = 0;
	CompressImageDXT5( param->in, param->out, param->width, param->height, nbbytes);
	param->nbb = nbbytes;	
	return NULL;
}

void *slave5ycocg(void *arg)
{
	work_t *param = (work_t*) arg;
	int nbbytes = 0;
	CompressImageDXT5YCoCg( param->in, param->out, param->width, param->height, nbbytes);
	param->nbb = nbbytes;
	return NULL;
}

int CompressDXT(const byte *in, byte *out, int width, int height, int format)
{ 
  int        nbbytes;

  work_t job;

  job.width = width;
  job.height = height;
  job.nbb = 0;
  job.in =  (byte*)in;
  job.out = out;
  
  switch (format) {
      case FORMAT_DXT1:
          slave1(&job);
          break;
      case FORMAT_DXT5:
          slave5(&job);
          break;
      case FORMAT_DXT5YCOCG:
          slave5ycocg(&job);
          break;
  }

  // Join all the threads
  nbbytes = job.nbb;
  return nbbytes;
}
