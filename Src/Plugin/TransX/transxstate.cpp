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

#include "orbitersdk.h"
#include <deque>
#include "mfd.h"
#include "intercept.h"
#include "mapfunction.h"
#include "mfdvarhandler.h"
#include "basefunction.h"
#include "viewstate.h"
#include "shiplist.h"
#include "transxstate.h"

MFDvarhandler *transxstate::GetVarhandler(unsigned int curvarfunction)
{
	if (curvarfunction==0 || baselist.empty()) return &vars;
	if (curvarfunction<1 || curvarfunction>baselist.size()) curvarfunction=1;
	class basefunction *cfunction=baselist[curvarfunction-1];
	return cfunction->getvariablehandler();
}

MFDvariable *transxstate::GetCurrVariable(unsigned int curvarfunction,int currviewmode)
{
	if (curvarfunction==0 || baselist.empty()) return vars.getcurrent(2);//return the variables for this
	if (curvarfunction<1 || curvarfunction>baselist.size()) curvarfunction=1;
	class basefunction *cfunction=baselist[curvarfunction-1];
	return cfunction->getcurrentvariable(currviewmode);
}

int transxstate::movetonextfunction(unsigned int curvarfunction)
{
	//Check for validity of passed curvarfunction
	if (baselist.empty())
	{
		m_ships.initbybody(hcraft,true);//Reset the content of this variable
		return 0;
	}
	if (curvarfunction<1 || curvarfunction>baselist.size())
	{//Invalid value, reset to valid value
		return 1;
	}
	class basefunction *cfunction=baselist[curvarfunction-1];

	OBJHANDLE tmajor,tminor,actualhmajor;
	VESSELSTATUS status;
	oapiGetVesselInterface(hcraft)->GetStatus(status);	
	if (status.status==1 && !cfunction->getpreviousfunc()) // craft is landed and we are in the first stage
		cfunction->setplanstate(0, 1); // force an escape plan
	cfunction->updateplan();
	cfunction->handlesfornextfunction(&tmajor,&tminor);
	if (curvarfunction<baselist.size())//if not the last function
	{
		//check if function matches - if not, delete it and its following functions
		actualhmajor=baselist[curvarfunction]->gethmajor();
		if (actualhmajor==tmajor)
		{//function is still legal, just move to it
			return curvarfunction+1;
		}
		else
		{//function does not match, therefore
			//Move all functions beyond this point to the delete list
			for (unsigned int a=curvarfunction;a<baselist.size();a++)
			{
				todeletelist.push_back(baselist[a]);
				baselist[a]->delist();
				baselist[a]->onplaceindeletebuffer();
			}
			baselist.resize(curvarfunction);//removes these elements from the baselist
			addaction(0);
		}
	}
	//Will now be the last function!
	if (tmajor==NULL) return curvarfunction;//No new legal function to create, so return
	//Create new function, and move to it
	class basefunction *temp=new basefunction(this,baselist[curvarfunction-1],tmajor,tminor,hcraft);
	int prevplantype,prevplan,targettype;
	cfunction->getplanstate(&prevplantype,&prevplan,&targettype);
	temp->setnextplanstate(prevplantype,prevplan,targettype);
	baselist.push_back(temp);
	return curvarfunction+1;
}

bool transxstate::checkdelete()
{
	if (todeletelist.empty()) return false;
	delete todeletelist[0];
	todeletelist.pop_front();
	addaction(0);
	return true;
}

int transxstate::movetopreviousfunction(unsigned int curvarfunction)
{
	if (curvarfunction<1 || curvarfunction>baselist.size()) return 1;
	if (curvarfunction==1)
	{
		m_ships.initbybody(hcraft,true);//Reset the content of this variable
		return 0;//The special case that we look at the ship level variables
	}
	baselist[curvarfunction-1]->updateplan();
	return curvarfunction-1;
}

int transxstate::inc_viewmode(unsigned int curfunction,int currview)
{
	if (curfunction<1 || curfunction>baselist.size()) return 2;//Also correct for case 0
	return baselist[curfunction-1]->calcnewview(currview,curfunction==1);
}

void transxstate::dolowpriaction()
{

	if (!initflag) return;//do not attempt to run if initialisation incomplete
	if (checkdelete()) return;
	//Do nothing most of the time, to reduce system load on orbiter
	if (actionframe>0)
	{
		actionframe--;
		addaction(0);
		return;
	}
	actionframe=0;
	if (currcalcfunction<0)
	{
		currcalcfunction=0;
		checkbasefunction();
	}
	VECTOR3 temp;
	if (currcalcfunction>=(int)baselist.size())
	{
		currcalcfunction=-1;
		return;
	}
	baselist[currcalcfunction]->calculate(&temp);
	currcalcfunction++;
	addaction(0);
}


bool transxstate::checkbasefunction()
{
	if (baselist.empty())
	{
		initfunctions();
		return false;//system not yet initialised
	}
	basefunction *firststage=baselist[0];
	if (firststage->soistatus()) return false;//no need to delete this stage
	//Now know that current base function is seeking a successor!
	if (baselist.size()<2)//no next stage
	{
		restartallfunctions();//Keeps system current
		return false;//No successor
	}
	basefunction *nextstage=baselist[1];
	if (!nextstage->soistatus())
	{
		return false;//Successor no better - stick with current soi
		//Done this way to avoid invalidation of plans when blundering close by a jovian moon.
		//Strictly wrong, but until system can predict such encounters, leave.
	}
	//Delete the current function
	//Move pointer to first function to the delete list
	baselist.pop_front();
	delete firststage;//deletes this basefunction
	shipptrs->downshift();//Tell all viewstates to move their position numbers down 1
	return true;
}

bool transxstate::restartallfunctions()
{
	for (unsigned int a=0;a<baselist.size();a++)
	{
		todeletelist.push_back(baselist[a]);
		baselist[a]->delist();
		baselist[a]->onplaceindeletebuffer();
	}
	baselist.clear();
	addaction(0);//start process of deleting functions
	return initfunctions();
}

class TransxMFD *transxstate::GetMFDpointer()
{
	return mfdpointer;
}

void transxstate::updateownfocusvessel(OBJHANDLE newfocus)
{
	for (unsigned int a=0;a<baselist.size();a++)
		baselist[a]->sethcraft(newfocus);
	hcraft=newfocus;
	m_ships.initbybody(newfocus,true);
}

void transxstate::savecurrent(FILEHANDLE scn)

{
	if (saveflag) return;
	saveflag=true;
	//Save each function
	char buffer[20];//Too big, but who cares!
	strcpy(buffer,"FNumber");
	oapiWriteScenario_int(scn,buffer,baselist.size());
	std::deque<class basefunction*>::iterator a;
	for (a=baselist.begin();a!=baselist.end();a++)
		(*a)->saveself(scn);
}

bool transxstate::restoresave(FILEHANDLE scn)
{
	char *buffer,*member;
	Parser parser;
	int length;
	if (!oapiReadScenario_nextline(scn,buffer))
	{
		initfunctions();
		return true;
	}
	parser.parseline(buffer);
	unsigned int numfunctions;
	if (parser.getlineelement(1,&member,&length))
	{
		sscanf(member,"%i",&numfunctions);
	}
	else
	{
		initfunctions();
		return false;
	}
	OBJHANDLE thmajor=oapiGetGbodyByIndex(0);//should be the central sun - only a safe stand-in anyway until real values loaded
	if (numfunctions<1 || numfunctions>100)
	{// No proper state to load
		initfunctions();
		return true;
	}
	bool success=true;
	unsigned int a;
	basefunction *temp=NULL;
	for (a=0;a<numfunctions;a++)
	{
		temp=new basefunction(this,temp,thmajor,NULL,hcraft);
		if (temp==NULL)
		{
			success=false;
			break;
		}
		baselist.push_back(temp);
	}
	if (success)
	{
		for (a=0;a<numfunctions;a++)
			baselist[a]->restoreself(scn);
	}
	else
	{
		for (a=0;a<baselist.size();a++)
			delete baselist[a];
		baselist.clear();
	}
	currcalcfunction=-1;
	return true;
}

transxstate::transxstate(OBJHANDLE thcraft, class shipptrs *tshipptrs)
{
	shipptrs=tshipptrs;
	hcraft=thcraft;
	mappointer=mapfunction::getthemap();
	baselist.clear();
	todeletelist.clear();
	initflag=false;
	helpsystem=false;//Switch help system off by default
	mfdactive=true;
	currcalcfunction=0;
	functionswitch=true;
	saveflag=false;
	actionframe=0;
	selectshipvars=false;
	eastereggswitch=long(oapiGetSimMJD()*SECONDS_PER_DAY)%5-1;
	initialisevars();
	initfunctions();
}


void copytransxstatecmd::execute()
{
	OBJHANDLE craft=ivar->gethandle();//Get the handle this is based on
	if (craft!=NULL)
		mytxstate->baseonvessel(craft);
}


void transxstate::baseonvessel(OBJHANDLE p_craft)
{
	class shipptrs *ship=shipptrs::findship(p_craft);
	baseonexisting(ship->gettransxstate());//Will do it if map is up and running, and the ship is legal
}

bool transxstate::baseonexisting(class transxstate *existing)
{
	if (!mappointer->getinitialised()) return false;
	//Delete the existing stuff in our own list
	int numentries=baselist.size();
	for (int a=0;a<numentries;a++)
	{
		class basefunction *base=baselist[a];
		base->delist();
		base->onplaceindeletebuffer();
		todeletelist.push_back(base);
	}
	addaction(0);
	baselist.clear();

	//Check that initial body is correct
	class basefunction *currbase=existing->baselist[0];
	class basefunction *newbase,*previous;
	previous=NULL;
	if (currbase->gethmajor()!=mappointer->getcurrbody(hcraft)) return false;//check that initial body is correct
	numentries=existing->baselist.size();
	for (int a=0;a<numentries;a++)
	{
		currbase=existing->baselist[a];
		newbase=new basefunction(this,previous,currbase,hcraft);//creates new basefunction matching old one
		previous=newbase;
		baselist.push_back(newbase);
	}
	currcalcfunction=-1;

	return true;
}



bool transxstate::initialisevars()
{
	m_ships.init(&vars,2,2,"Inherit from");
	//Make invisible all variables that sometimes are invisible

	//set up help system
	m_ships.sethelpstrings(
		"Select the ship from which",
		"you wish to inherit settings.");
	class copytransxstatecmd *nugget=new copytransxstatecmd;
	nugget->settransxstate(this);
	m_ships.setcmdnugget(nugget);

	return true;
}



bool transxstate::initfunctions()//sets up transxstate if no saved state to reload
{	
	//Create some default values for these functions
	OBJHANDLE thmajor,thminor;
	thminor=NULL;
	if (!mappointer->getinitialised()) return false;
	thmajor=mappointer->getcurrbody(hcraft);
	shipptrs->resetshift();
	//Initialise functions
	basefunction *temp=new basefunction(this,0,thmajor,thminor,hcraft);//basefunction in first slot
	baselist.push_back(temp);
	currcalcfunction=-1;
	return true;
}

basefunction *transxstate::getpreviousfunction(int positionnumber)
{//retained for backward compatibility 
	return getbasefn(positionnumber);
}

basefunction *transxstate::getbasefn(unsigned int stagenumber)
{
	if (stagenumber<1 || stagenumber>baselist.size()) return NULL;
	return baselist[stagenumber-1];
}

basefunction *transxstate::getnextfunction(int positionnumber)
{//retained for backward compatibility
	return getbasefn(positionnumber+2);
}


transxstate::~transxstate()
{
	for (unsigned int a=0;a<baselist.size();a++)
		delete baselist[a];
	for (unsigned int a=0;a<todeletelist.size();a++)
		delete todeletelist[a];
	baselist.clear();
	todeletelist.clear();
}

bool transxstate::doupdate(Sketchpad *sketchpad, int tw, int th,unsigned int curfunction,int currview, unsigned int curvarfunction, int currvarview,TransxMFD *tmfdpointer)
{
	saveflag=false;
	selectshipvars=(curvarfunction<1);
	//Part of system that allows only one save to take place
	mfdpointer=tmfdpointer;
	int linespacing=th/24;
	if (mappointer==NULL) return false; //Just don't do anything if system not initialised
	if (mappointer->getinitialised()==false)
	{//show the Easter eggs!

		sketchpad->Text(tw/3,6*linespacing,"TransX",6);
		sketchpad->Text(0,22*linespacing,"(C) Sirius Cybernetics Corporation.",35);

		switch (eastereggswitch)
		{
		case 0:
			sketchpad->Text(tw/6,9*linespacing,"Change your world.",18);
			return false;
		case 1:
			sketchpad->Text(tw/6,9*linespacing,"It doesn't do dishes.",21);
			sketchpad->Text(tw/6,10*linespacing,"Sorry.",6);
			return false;
		case 2:
			sketchpad->Text(tw/6,8*linespacing,"Check out our new robotics",26);
			sketchpad->Text(tw/6,9*linespacing,"range.",6);
			sketchpad->Text(tw/6,11*linespacing,"Your plastic pal who's",22);
			sketchpad->Text(tw/6,12*linespacing,"fun to be with!",15);
			return false;
		case 3:
			sketchpad->Text(tw/6,8*linespacing,"Blue sky thinking -",19);
			sketchpad->Text(tw/6,9*linespacing,"we look down on it",18);
			return false;
		case 4:
			sketchpad->Text(tw/6,9*linespacing,"Change your world.",18);
			return false;
		}
		return false;
	}
	if (selectshipvars)
	{
		m_ships.initbybody(hcraft,false);//Initialise variable if it hasn't been done.
		showinitialstage(sketchpad,linespacing,tw);
		return true;//If this is the case, only the variable system for ship-level is active.
	}
	if (baselist.empty())
	{
		if (!initfunctions()) return false;
	}
	if (curfunction<0 || curfunction > baselist.size()) curfunction=1;
	class basefunction *cfunction=baselist[curfunction-1];
	if (curvarfunction <0 || curvarfunction > baselist.size()) curvarfunction=curfunction;
	class basefunction *cvarfunction=baselist[curvarfunction-1];
	if (currcalcfunction==-1) addaction(0);
	initflag=true;
	if (helpsystem)
	{
		char help1[40],help2[40],help3[40],help4[40],help5[40];
		//Help for variable
		MFDvariable *variable;
		variable=cvarfunction->getcurrentvariable(currvarview);
		if (variable!=NULL)
		{
			variable->gethelpstrings(help1,help2);
			sketchpad->Text(0,11*linespacing,help1,strlen(help1));
			sketchpad->Text(0,12*linespacing,help2,strlen(help2));
		}
		//Help for function
		cfunction->gethelp(help1,help2,help3,help4,help5);
		sketchpad->Text(0,16*linespacing,help1,strlen(help1));
		sketchpad->Text(0,17*linespacing,help2,strlen(help2));
		sketchpad->Text(0,18*linespacing,help3,strlen(help3));
		sketchpad->Text(0,19*linespacing,help4,strlen(help4));
		sketchpad->Text(0,20*linespacing,help5,strlen(help5));
	}
	else
	{
		cvarfunction->processvisiblevars();//Update any visibility changes
		cfunction->doupdate(sketchpad,tw,th,currview);
	}
	char buffer[20];
	int length=sprintf(buffer,"Stage %i:%z",curfunction,baselist.size());
	sketchpad->Text(tw/2,0,buffer,length);
	length=sprintf(buffer,"Vars Stage %i",curvarfunction);
	sketchpad->Text(tw/2,4*linespacing,buffer,length);
	return true;
}

void transxstate::showinitialstage(Sketchpad *sketchpad,int linespacing,int tw)
{
	sketchpad->Text(tw/2,0,"General Setup",13);
}