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

#ifndef __VIEWSTATE_H
#define __VIEWSTATE_H

#include "transx.h"
#include "transxstate.h"

class viewstate
{
	int mfdposition;
	int viewmode;
	int varviewmode;
	int viewfunction,varviewfunction;
	bool switchmode;
	static bool renderviewport;
	bool mfdactive;//If there's an MFD associated with this
	viewstate(UINT tmfd, class shipptrs *shipptrs);
	class transxstate *state;//Pointer to the transxstate associated with this
	void selfdownshift();
	void resetshift();
public:
	bool doupdate(oapi::Sketchpad *sketchpad, int tw, int th, TransxMFD *mfdptr);
	bool getrenderviewport(){return renderviewport;};
	class MFDvariable *GetCurrVariable();
	static void preparetoclose();
	void setmfdactive(bool temp){mfdactive=temp;};
	void movetonextfunction();
	void movetopreviousfunction();
	void inc_viewmode();
	void togglefunctionswitch(){switchmode=!switchmode;viewfunction=varviewfunction;viewmode=varviewmode;};
	void savecurrent(FILEHANDLE scn);
	void restoresave(FILEHANDLE scn);
	int getvariableviewmode(){return varviewmode;};
	class MFDvarhandler *GetVarhandler();
	void fliphelpsystem();
	~viewstate();
	friend class shipptrs;
};

#endif
