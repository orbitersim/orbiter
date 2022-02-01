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

#ifndef __MFDVARIABLE_H
#define __MFDVARIABLE_H
#include "mfdvarhandler.h"
#include "doublelink.h"
class cmdnugget;

#define MAX_NAME_LENGTH			20
#define MAX_HELPSTRING_LENGTH	40

class MFDvariable : listelement
{
private:
	int viewmode1,viewmode2;
	int execstatus,execcountdown;//Simple system to provide feedback message on pressing of EXE
	cmdnugget *inugget;
protected:
	char name[MAX_NAME_LENGTH]; //Name of variable as displayed on MFD
	char helpstring1[MAX_HELPSTRING_LENGTH]; //Help string
	char helpstring2[MAX_HELPSTRING_LENGTH];//Help string
	//int adjmode; // Integer representing the adjustment mode - use depends on class
	bool showvariable;
	bool continuous; // allows continuous adjustment of variable (eg, angle)
public:
	MFDvariable();
	bool IsContinuous() {return continuous;};
	void setcmdnugget(cmdnugget *nugget);
	void execute();//Executes any command nugget installed.
	void initialise(class MFDvarhandler *vars,int viewmode1,int viewmode2);
	virtual ~MFDvariable();
	void setshow(bool value);
	bool showgeneric(oapi::Sketchpad *sketchpad,int width,int line, char *inbuff);
	virtual void showadjustment(oapi::Sketchpad *sketchpad, int width, int line) const {}; //Show the adjustment mode
	void gethelpstrings(char *help1, char *help2) const;//Returns help strings
	void sethelpstrings(char *help1, char *help2);//Sets help strings
	void getname(char *buffer) const;
	virtual void getsaveline(char *buffer) const = 0 ;//Creates second line of save - overloaded
	virtual bool loadvalue(char *buffer);//Set value according to string
	virtual void setadjmode(int adjmode) {};//Sets the adjustment mode
	virtual void ch_adjmode() {}; // Change the adjustment mode
	virtual void chm_adjmode() {};
	virtual void inc_variable() = 0; //Increase the variable
	virtual void dec_variable() {inc_variable();}; //Decrease the variable
	virtual void enter_variable() {};	// = 0;//{}; // typing skills FTW
	virtual bool show(oapi::Sketchpad *sketchpad, int width, int line);
	virtual void sethandle(OBJHANDLE tpointer);
	virtual void setall(class MFDvariable *var);
	virtual OBJHANDLE gethandle() const;
	friend class MFDvarhandler;
protected:
	virtual void InheritValues(MFDvariable *var) = 0;
};

#endif
