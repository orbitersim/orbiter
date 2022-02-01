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
#include "transxstate.h"
#include "TransXFunction.h"
#include "transx.h"

TransXFunction::TransXFunction(class transxstate *tstate, OBJHANDLE thmajor, OBJHANDLE thminor, OBJHANDLE thtarget, OBJHANDLE thcraft, OBJHANDLE thbase)
{
	state=tstate;
	sethandles(thmajor,thminor,thtarget,thcraft,thbase);
	simstartMJD=oapiTime2MJD(0);
	initpens();
}

TransXFunction::TransXFunction(class transxstate *tstate, OBJHANDLE thmajor, OBJHANDLE thminor,OBJHANDLE thcraft)
{
	hmajor=thmajor;
	hminor=thminor;
	state=tstate;
	hmajtarget=NULL;
	hbase=NULL;
	hcraft=thcraft;
	simstartMJD=oapiTime2MJD(0);
	initpens();
}

TransXFunction::~TransXFunction()
{
	//Need to delete the pens to stop them eating system resources
	deletepens();
}

void TransXFunction::saveself(FILEHANDLE scn)
{
	//Write terminator
	oapiWriteScenario_string(scn,"Finish","TransXFunction");
} //Should never be used - overloaded by further functions

void TransXFunction::restoreself(FILEHANDLE scn)
{
	//Search for the terminator, then return
	char *bufferpointer;
	char tempbuffer[18], finalbuffer[18];
	bool ok;
	do
	{
		ok=oapiReadScenario_nextline(scn,bufferpointer);
		strncpy(tempbuffer,bufferpointer,16);
		sscanf(tempbuffer,"%s",finalbuffer);
	}
	while (strcmp(finalbuffer,"Finish")!=0 && ok==true);
}

void TransXFunction::UpdateAllPlans()
{
	state->UpdateForOptimiser();
}

bool TransXFunction::loadhandle(FILEHANDLE scn,OBJHANDLE *handle)
{
	char *member,*bufferpointer;
	int length;
	bool ok=oapiReadScenario_nextline(scn,bufferpointer);
	if (!ok) return false;//Unexpected end of file found
	parser.parseline(bufferpointer);
	if (!parser.getlineelement(0,&member,&length)) return false;
	if (strcmp(member,"Finish")==0) return false;
	if (strcmp(member,"Handle")!=0) return true;
	if (!parser.getlineelement(1,&member,&length)) return false;//Field missing
	if (strcmp(member,"NULL")==0)
	{
		*handle=NULL;
		return true;//Handle set null
	}
	OBJHANDLE thandle=oapiGetObjectByName(member);
	if (thandle!=NULL) *handle=thandle;
	return true;
}


void TransXFunction::findfinish(FILEHANDLE scn)
{
	char *tbuffer,*member;
	int length;
	do
	{
		if (!oapiReadScenario_nextline(scn,tbuffer)) return;
		parser.parseline(tbuffer);
		parser.getlineelement(0,&member, &length);
	}
	while (strcmp(member,"Finish"));
}


void TransXFunction::savedouble(FILEHANDLE scn, double savenumber)
{
	char buffer[80]="";
	sprintf(buffer," %.12g",savenumber);
	oapiWriteScenario_string(scn,"Double",buffer);
}

void TransXFunction::savevector(FILEHANDLE scn, VECTOR3 &vector)
{
	char buffer[100]="";
	sprintf(buffer," %.12g %.12g %.12g",vector.x,vector.y,vector.z);
	oapiWriteScenario_string(scn,"Vector",buffer);
}


bool TransXFunction::loaddouble(FILEHANDLE scn, double *loadednumber)
{
	char *member,*bufferpointer;
	int length;
	bool ok=oapiReadScenario_nextline(scn,bufferpointer);
	if (!ok) return false;//Unexpected end of file found
	parser.parseline(bufferpointer);
	if (!parser.getlineelement(0,&member,&length)) return false;
	if (strcmp(member,"Finish")==0) return false; //Unexpected end of function found
	if (strcmp(member,"Double")!=0) return true; //Not a double
	if (!parser.getlineelement(1,&member,&length)) return false;
	*loadednumber=atof(member);
	return true;
}

bool TransXFunction::loadint(FILEHANDLE scn, int *loadedint)
{
	char *member,*bufferpointer;
	int length;
	bool ok=oapiReadScenario_nextline(scn,bufferpointer);
	if (!ok) return false;//Unexpected end of file found
	parser.parseline(bufferpointer);
	if (!parser.getlineelement(0,&member,&length)) return false;
	if (strcmp(member,"Finish")==0) return false; //Unexpected end of function found
	if (strcmp(member,"Int")!=0) return true; //Not an Int
	if (!parser.getlineelement(1,&member,&length)) return false;
	sscanf(member," %i",loadedint);
	return true;
}


bool TransXFunction::loadvector(FILEHANDLE scn, VECTOR3 *loadedvector)
{
	char *bufferpointer,*member;
	int length;
	bool ok=oapiReadScenario_nextline(scn,bufferpointer);
	if (!ok) return false;//Unexpected end of file found
	parser.parseline(bufferpointer);
	if (!parser.getlineelement(0,&member,&length)) return false;
	if (strcmp(member,"Finish")==0) return false; //Unexpected end of function found
	if (strcmp(member,"Vector")!=0) return true; //Not a vector
	if (!parser.getlineelement(1,&member,&length)) return false;
	(*loadedvector).x=atof(member);
	if (!parser.getlineelement(2,&member,&length)) return false;
	(*loadedvector).y=atof(member);
	if (!parser.getlineelement(3,&member,&length)) return false;
	(*loadedvector).z=atof(member);
	return true;
}


bool TransXFunction::loadorbit(FILEHANDLE scn,OrbitElements *loadorbit)
{
	char *bufferpointer, *member;
	bool ok=oapiReadScenario_nextline(scn,bufferpointer);
	int length;
	if (!ok) return false;
	parser.parseline(bufferpointer);
	if (!parser.getlineelement(0,&member,&length)) return false;//No fields
	if (strcmp(member,"Orbit")!=0) return false; //Not an orbit
	if (!parser.getlineelement(1,&member,&length)) return false;//No second field
	if (strcmp(member,"False")==0)
	{
		//Uninitialised orbit structure
		loadorbit->setinvalid();
		return true;
	}
	VECTOR3 tpos,tvel;
	double gmplanet, timestamp;
	if (!loadvector(scn,&tpos)) return false;
	if (!loadvector(scn,&tvel)) return false;
	if (!loaddouble(scn,&gmplanet)) return false;
	if (!loaddouble(scn,&timestamp)) return false;
	timestamp=(timestamp-simstartMJD)*SECONDS_PER_DAY;
	loadorbit->init(tpos,tvel,timestamp,gmplanet);
	return true;
}

void TransXFunction::savehandle(FILEHANDLE scn, OBJHANDLE handle)
{
	char namebuffer[30];
	if (handle!=NULL)
		oapiGetObjectName(handle,namebuffer,30);
	else
		strcpy(namebuffer,"NULL");
	oapiWriteScenario_string(scn,"Handle",namebuffer);
	return;
}


void TransXFunction::saveorbit(FILEHANDLE scn, const OrbitElements &saveorbit)
{
	char validvalue[6];
	if (saveorbit.isvalid())
		strcpy(validvalue,"True");
	else
		strcpy(validvalue,"False");
	oapiWriteScenario_string(scn,"Orbit",validvalue);
	if (!saveorbit.isvalid()) return;
	VECTOR3 tpos,tvel;
	saveorbit.getcurrentvectors(&tpos,&tvel);
	savevector(scn,tpos);
	savevector(scn,tvel);
	double planet=saveorbit.getgmplanet();
	savedouble(scn,planet);
	double time=saveorbit.gettimestamp()/SECONDS_PER_DAY+simstartMJD;
	savedouble(scn,time);
}

MFDvarhandler *TransXFunction::getvariablehandler()
{
	return &vars;
}

MFDvariable *TransXFunction::getcurrentvariable(int view)
{
	return vars.getcurrent(view);
}

void TransXFunction::sethmajor(OBJHANDLE handle)
{
	hmajor=handle;
	hminor=hmajtarget=NULL;
}

bool TransXFunction::sethminor(OBJHANDLE handle)
{//Virtual to give local ability to tweak this function
	return sethminorstd(handle);
}

bool TransXFunction::sethminorstd(OBJHANDLE handle)
{//Non-virtual function
	if (hmajor==handle) return false;
	if (handle==hmajtarget) hmajtarget=NULL;
	hminor=handle;
	if (hminor==NULL) return false;
	gravbodyratio=pow(oapiGetMass(hminor)/oapiGetMass(hmajor), double (0.8));
	return true;
}

bool TransXFunction::sethmajtarget(OBJHANDLE handle)
{
	if (hmajor==handle) return false;
	if (hminor==handle) hminor=NULL;
	hmajtarget=handle;
	return true;
}

void TransXFunction::sethcraft(OBJHANDLE handle)
{
	hcraft=handle;
}

void TransXFunction::sethbase(OBJHANDLE handle)
{
	hbase=handle;
}

void TransXFunction::sethandles(OBJHANDLE thmajor, OBJHANDLE thminor, OBJHANDLE thtarget, OBJHANDLE thcraft, OBJHANDLE thbase)
{
	hmajor=thmajor;
	gravbodyratio=0;
	sethminor(thminor);
	hmajtarget=thtarget;
	hcraft=thcraft;
	hbase=thbase;
}

void TransXFunction::gethandles(OBJHANDLE *thmajor, OBJHANDLE *thminor, OBJHANDLE *thtarget, OBJHANDLE *thcraft, OBJHANDLE *thbase)
{
	*thmajor=hmajor;
	*thminor=hminor;
	*thtarget=hmajtarget;
	*thcraft=hcraft;
	*thbase=hbase;
}
void TransXFunction::initpens(void)								//(rbd+)
{
    DWORD green =   RGB(0x00, 0xFF, 0x00);
    DWORD blue =    RGB(0x64, 0x95, 0xED);
    DWORD yellow =  RGB(0xCD, 0xCD, 0x00);
    DWORD red =     RGB(0xFF, 0x00, 0x00);
    DWORD grey =    RGB(0xC0, 0xC0, 0xC0);
    DWORD white =   RGB(0xFF, 0xFF, 0xFF);
	if (!pens[Green])	pens[Green]		= oapiCreatePen(1, 1, green);	// Green - stands for craft
	if (!pens[Blue])	pens[Blue]		= oapiCreatePen(1, 1, blue);	// Blue - stands for planet
	if (!pens[Yellow])	pens[Yellow]	= oapiCreatePen(2, 1, yellow);	// Bright yellow - hypos
	if (!pens[Red])		pens[Red]		= oapiCreatePen(1, 1, red);	    // Bright red - unused, but danger
	if (!pens[Grey])	pens[Grey]		= oapiCreatePen(1, 1, grey);	// Light Grey
	if (!pens[GreyDashed])	pens[GreyDashed] = oapiCreatePen(2, 1, grey);	// Light Grey dashed for line of nodes
	if (!pens[White])	pens[White]		= oapiCreatePen(1, 1, white);	// Bright white - unused
	if (!brush[Green])	brush[Green]    = oapiCreateBrush (green);
	if (!brush[Blue])	brush[Blue]		= oapiCreateBrush (blue);
	if (!brush[Yellow])	brush[Yellow]	= oapiCreateBrush (yellow);
	if (!brush[Red])	brush[Red]		= oapiCreateBrush (red);
	if (!brush[Grey])	brush[Grey]		= oapiCreateBrush (grey);
	if (!brush[GreyDashed])	brush[GreyDashed] = oapiCreateBrush (grey);
	if (!brush[White])	brush[White]	= oapiCreateBrush (white);
}

void TransXFunction::deletepens()
{
	for (int a=0;a<NUM_PENS;a++)
	{
		if (pens[a]) {
			oapiReleasePen (pens[a]);
			pens[a] = 0;
		}
		if (brush[a]) {
			oapiReleaseBrush (brush[a]);
			brush[a] = 0;
		}
	}
}
															//(rbd-)
oapi::Pen* TransXFunction::SelectDefaultPen(oapi::Sketchpad *sketchpad, int value)
{
	oapi::Pen* ret;
	if(value < NUM_PENS) {//(rbd+)
		ret=sketchpad->SetPen(pens[value]);
	} else {
		ret=sketchpad->SetPen(pens[Green]);
	}
	if(ret==NULL) {
		char dstr[256];
		sprintf(dstr,"TransX: SelectDefaultPen(%i): Pen got nuked, fixing.",value);oapiWriteLog(dstr);
		initpens();
	}
	return ret;
}

oapi::Brush* TransXFunction::SelectBrush(oapi::Sketchpad *sketchpad, int value)
{
	if(value < NUM_PENS && value >= 0) //(rbd+)
		return sketchpad->SetBrush(brush[value]);		// Custom brush
	else //(rbd-)
		return sketchpad->SetBrush(NULL);
}

void TransXFunction::sethelp(char *help1,char *help2,char *help3,char *help4,char *help5)
{
	strcpy(helpstring1,help1);
	strcpy(helpstring2,help2);
	strcpy(helpstring3,help3);
	strcpy(helpstring4,help4);
	strcpy(helpstring5,help5);
}

void TransXFunction::gethelp(char *help1,char *help2,char *help3,char *help4,char *help5) const
{
	strcpy(help1,helpstring1);
	strcpy(help2,helpstring2);
	strcpy(help3,helpstring3);
	strcpy(help4,helpstring4);
	strcpy(help5,helpstring5);
}

oapi::Pen* TransXFunction::pens[NUM_PENS] = {0};
oapi::Brush* TransXFunction::brush[NUM_PENS] = {0};
