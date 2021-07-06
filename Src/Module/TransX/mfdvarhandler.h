/* Copyright (c) 2007 Duncan Sharpe, Steve Arch
**
** Permission is hereby granted, free of charge, to any person obtaining a copy
** of this software and associated documentation files (the "Software"), to deal
** in the Software without restriction, including without limitation the rights
** to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
** copies of the Software, and to permit persons to whom the Software is
** furnished to do so, subject to the following conditions:
** 
** The above copyright notice and this permission notice shall be included in
** all copies or substantial portions of the Software.
** 
** THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
** IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
** FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
** AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
** LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
** OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
** THE SOFTWARE.*/

#ifndef __MFDVARHANDLER_H
#define __MFDVARHANDLER_H

#include "mfdvariable.h"
#include "doublelink.h"


class MFDvarhandler{
private:
	class dbllist listhead;
	class dblliter *variterator;
	class MFDvariable *currvariable[5]; //The current variable for each viewmode
	class MFDvariable *current;//The current variable in the current viewmode
	int currviewmode;
public:
	void addtolist(class MFDvariable *item);
	bool crosscopy(MFDvarhandler &othervars);
	void setprevcurrent(int viewmode);
	void setnextcurrent(int viewmode);
	void saveallvariables(FILEHANDLE scn);
	bool loadallvariables(FILEHANDLE scn);
	class MFDvariable* getcurrent(int viewmode);
	MFDvarhandler();//Sets MFDvariable list to 0
	~MFDvarhandler();//Deletes all MFDvariables created through the class
	void deleteall(); // Deletes and resets MFD variable system - typically following inability to assign space
	void resetvars();
};

#endif