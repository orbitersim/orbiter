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

#ifndef __TRANSXSTATE_H
#define __TRANSXSTATE_H

#include "mfdfunction.h"
#include "mfdvarhandler.h"
#include "mfdvartypes.h"
#include "cmdnugget.h"
#include <deque>
#define SECONDS_PER_DAY 86400

class transxstate:private MFDFunction {
public:
	transxstate(OBJHANDLE thcraft,class shipptrs *tshipptrs);
	~transxstate();
	void baseonvessel(OBJHANDLE p_craft);
	bool baseonexisting(class transxstate *existing);

	virtual void dolowpriaction();
	void UpdateForOptimiser();

	bool doupdate(oapi::Sketchpad *sketchpad,int tw, int th, unsigned int curfunction,int currview, unsigned int curvarfunction, int currvarview,class TransxMFD *tmfdpointer);
	void savecurrent(FILEHANDLE scn);
	bool restoresave(FILEHANDLE scn);

	class TransxMFD *GetMFDpointer();//Returns pointer to current MFD
	class MFDvarhandler *GetVarhandler(unsigned int curvarfunction); //Gets the variable handler for the current function
	class MFDvariable *GetCurrVariable(unsigned int curvarfunction,int currviewmode); //Gets the current variable in the current function
	static void updatefocusvessel(OBJHANDLE newfocus); //Updates handles when vessel changes
	void updateownfocusvessel(OBJHANDLE newfocus); //Non-void section that updates focus vessel in every function
	bool sethelpsystem(bool thelpsystem) {helpsystem=thelpsystem;};
	bool fliphelpsystem(){return helpsystem=!helpsystem;};
	void togglefunctionswitch();
	int getnumfunctions(){return baselist.size();};
	int movetonextfunction(unsigned int curvarfunction);//Step forward to next function, creating one if needed/possible
	int movetopreviousfunction(unsigned int curvarfunction);//Step back to previous function
	int inc_viewmode(unsigned int curfunction, int currview);//Increment viewmode
	class basefunction *getnextfunction(int positionnumber);
	class basefunction *getpreviousfunction(int positionnumber);
	class basefunction *getbasefn(unsigned int stagenumber);
	class shipptrs *getshipptrs(){return shipptrs;};
	void setshipptrs(class shipptrs *ptr){shipptrs=ptr;};
	void showinitialstage(oapi::Sketchpad *sketchpad,int linespacing,int tw);
	bool checkbasefunction();//checks if first function should now be deleted due to non-validity
private:
	bool initfunctions();
	bool initialisevars();
	bool restartallfunctions();
	bool initflag;
	bool saveflag;
	bool selectshipvars;//whether ship level stuff is shown or not
	bool checkdelete();//delete one of the functions on the to delete list
	bool helpsystem;//On or off
	bool functionswitch;//whether to switch view with the variables
	bool mfdactive;
	int eastereggswitch;//Choose which Easter Egg to use
	int currcalcfunction;
	int actionframe;
	class TransxMFD *mfdpointer;
	class shipptrs *shipptrs;//List of viewstates for this transxstate
	class MFDvarhandler vars;
	class MFDvarshiplist m_ships;
	class mapfunction *mappointer;//The mapfunction pointer
	OBJHANDLE hcraft;
	std::deque<class basefunction*> baselist,todeletelist;
};

class copytransxstatecmd : public cmdnugget
{
private:
	class transxstate *mytxstate;
public:
	virtual void execute();
	void settransxstate(class transxstate *state){mytxstate=state;};
};


#endif
