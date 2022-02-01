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

#define STRICT

#include <windows.h>
#include <cstdio>
#include <cmath>
#include "orbitersdk.h"
#include "shiplist.h"
#include "viewstate.h"

bool viewstate::renderviewport=true;

viewstate::viewstate(UINT tmfd, class shipptrs *shipptrs)
{
	viewmode=2;
	varviewmode=2;
	viewfunction=1;
	varviewfunction=1;
	switchmode=true;
	mfdactive=false;
	renderviewport=true;
	mfdposition=tmfd;
	state=shipptrs->gettransxstate();
}

viewstate::~viewstate()
{}

void viewstate::preparetoclose()
{
	// MS-100704: Added the following line, otherwise
	// shipptrs::destroyshipptrs() is never called from
	// TransX::~TransX(), leading to CTD on next session.

	renderviewport=false;
}

void viewstate::movetonextfunction()
{
	varviewfunction=state->movetonextfunction(varviewfunction);
	if (switchmode) viewfunction=varviewfunction;
}

void viewstate::movetopreviousfunction()
{
	varviewfunction=state->movetopreviousfunction(varviewfunction);
	if (switchmode) viewfunction=varviewfunction;
}

void viewstate::inc_viewmode()
{
	varviewmode=state->inc_viewmode(varviewfunction,varviewmode);
	if (switchmode) viewmode=varviewmode;
}

void viewstate::savecurrent(FILEHANDLE scn)
{
	state->savecurrent(scn);
}

void viewstate::restoresave(FILEHANDLE scn)
{
	state->restoresave(scn);
}

bool viewstate::doupdate(oapi::Sketchpad *sketchpad,int tw,int th,TransxMFD *mfdptr)
{
	int numfunctions=state->getnumfunctions();
	if (viewfunction>numfunctions && numfunctions>0) viewfunction=numfunctions;
	if (varviewfunction>numfunctions && numfunctions>0) varviewfunction=numfunctions;
	return state->doupdate(sketchpad,tw,th,viewfunction,viewmode,varviewfunction,varviewmode,mfdptr);
}

void viewstate::selfdownshift()
{
	if (viewfunction>1) viewfunction--;
	if (varviewfunction>1) varviewfunction--;
}

void viewstate::resetshift()
{
	viewfunction=varviewfunction=1;
}

class MFDvariable *viewstate::GetCurrVariable()
{
	return state->GetCurrVariable(varviewfunction,varviewmode);
}

void viewstate::fliphelpsystem()
{
	state->fliphelpsystem();
}

class MFDvarhandler *viewstate::GetVarhandler()
{
	return state->GetVarhandler(varviewfunction);
}
