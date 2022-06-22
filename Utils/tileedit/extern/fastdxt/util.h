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


#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <vector>
#include <string>
#include <sys/stat.h>
#include <sys/types.h>

#if !defined(_WIN32)
#include <unistd.h>
#include <sys/time.h>
#else
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#pragma warning(disable:4786)   // symbol size limitation ... STL
#endif


#include <sys/stat.h>
#include <sys/types.h>
#include <fcntl.h>

#include <stdarg.h>
//#include <values.h>


void   aInitialize();

double aTime();

void   aLog(const char* format,...);
void aError(const char* format,...);

void* aAlloc(size_t const n);
void aFree(void* const p);

#if defined(WIN32)
float drand48(void);
#endif

#if defined(__APPLE__)
#define memalign(x,y) malloc((y))
#else
#include <malloc.h>
#endif


#if defined(WIN32)
void *aligned_malloc(size_t size, size_t align_size);
#define memalign(x,y) aligned_malloc(y, x)
void aligned_free(void *ptr);
#define memfree(x) aligned_free(x)
#else
#define memfree(x) free(x)
#endif


// From NVIDIA Toolkit

#ifndef DATA_PATH_H
#define DATA_PATH_H

class data_path
{
public:
  std::string              file_path;
  std::string              path_name;
  std::vector<std::string> path;
  
  std::string get_path(std::string filename);
  std::string get_file(std::string filename);
  
  FILE *fopen(std::string filename, const char * mode = "rb");
    
#ifdef WIN32
  int fstat(std::string filename, struct _stat * stat);
#else
  int fstat(std::string filename, struct stat * stat);
#endif
};

#endif
