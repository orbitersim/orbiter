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
#include "mfd.h"
#include "cmdnugget.h"
#include "mfdvariable.h"


MFDvariable::MFDvariable()
{
	continuous = false;
	showvariable=true;
	inugget=NULL;
	execstatus=execcountdown=0;
}

void MFDvariable::execute()
{
	if (inugget!=NULL)
	{
		inugget->execute();
		execstatus=1;
	}
	else
	{
		execstatus=2;
	}
	execcountdown=10;
}

void MFDvariable::initialise(MFDvarhandler *vars,int tviewmode1,int tviewmode2)
{
	viewmode1=tviewmode1;
	viewmode2=tviewmode2;
	if (vars!=NULL)
	{
		vars->addtolist(this);
	}
}

void MFDvariable::setshow(bool value)
{
	showvariable=value;
}

void MFDvariable::setall(class MFDvariable *var)
{
	InheritValues(var);
	sethandle(var->gethandle());
}

void MFDvariable::setcmdnugget(cmdnugget *nugget)
{
	if (nugget==NULL) return;
	nugget->setmfdvariable(this);
	inugget=nugget;
}

bool MFDvariable::showgeneric(oapi::Sketchpad *sketchpad,int width,int line, char *inbuff)
{
// This is a helper function that formats output to the MFD screen
	char buffer[MAX_NAME_LENGTH]="";
	int linecentre=(int) width/2;
	int linepos= 6*line;
	int inlength=strlen(inbuff);
	strcpy(buffer,name);
	int length=strlen(buffer);
	sketchpad->Text(linecentre, linepos, buffer, length);
	showadjustment(sketchpad, width, line);
	linepos+=line+line;
	sketchpad->Text(linecentre, linepos, inbuff,inlength);
	if (execcountdown>0)
	{
		linepos+=line;
		if (execstatus==1)
		{
			strcpy(buffer,"Executed");
		}
		else
		{
			strcpy(buffer,"No Command");
		}
		execcountdown--;
		if (execcountdown==0) execstatus=0;
		length=strlen(buffer);
		sketchpad->Text(linecentre,linepos,buffer,length);
	}
	return true;
}

bool MFDvariable::show(oapi::Sketchpad *sketchpad, int width, int line)
//This is a virtual function that will not normally be used. Although MFDvariable is not
// a pure virtual class, it is only the derived classes that are created in practice.
// All these show() functions describe the MFDvariable on the screen
{
	return showgeneric(sketchpad, width, line, " ");
}

bool MFDvariable::loadvalue(char *buffer)
{
	return true;
}

void MFDvariable::getname(char *buffer) const
{
	strcpy(buffer,name);
}

void MFDvariable::gethelpstrings(char *help1,char *help2) const
{
	strcpy(help1,helpstring1);
	strcpy(help2,helpstring2);
}

void MFDvariable::sethelpstrings(char *help1,char *help2)
{
	strcpy(helpstring1,help1);
	strcpy(helpstring2,help2);
}

void MFDvariable::sethandle(OBJHANDLE tpointer)
{}

OBJHANDLE MFDvariable::gethandle() const
{
	OBJHANDLE temp=NULL;
	return temp;
}

MFDvariable::~MFDvariable()
{
	if (inugget!=NULL) delete inugget;
}
