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
#include "orbiterapi.h"
#include "basefunction.h"
#include "transxstate.h"
#include "TransXFunction.h"
#include "mfd.h"
#include "transx.h"

#define GGRAV_ 6.67259e-11

extern double debug;

basefunction::basefunction(class transxstate *tstate, class basefunction *tpreviousfunc, OBJHANDLE thmajor, OBJHANDLE thminor,OBJHANDLE thcraft)
: TransXFunction(tstate, thmajor, thminor,thcraft)
{
	iplantype=0;
	previousfunc=tpreviousfunc;
	nextfunc=NULL;
	if (previousfunc!=NULL) previousfunc->setnextfunc(this);
	planpointer=NULL;
	hcontext=NULL;
	iplan=0;
	interceptwith=3;
	oldorbitsahead=0;
	previousexists=false;//until we find otherwise
	valid=initialisevars();
	mappointer=mapfunction::getthemap();
}

basefunction::basefunction(class transxstate *tstate, class basefunction *tpreviousfunc, class basefunction *templbase, OBJHANDLE thcraft)
: TransXFunction(tstate, templbase->hmajor, templbase->hminor, thcraft)
{
	iplantype=templbase->iplantype;
	previousfunc=tpreviousfunc;
	nextfunc=NULL;
	if (previousfunc!=NULL) previousfunc->setnextfunc(this);
	mappointer=mapfunction::getthemap();
	hcontext=templbase->hcontext;
	iplan=templbase->iplan;
	interceptwith=templbase->interceptwith;
	oldorbitsahead=templbase->oldorbitsahead;
	previousexists=templbase->previousexists;
	valid=initialisevars();
	if (templbase->planpointer!=NULL)
	{
		planpointer=templbase->planpointer->clone(&vars,this);
		planpointer->init(&vars,this);
	}
	else
	{
		planpointer=NULL;
	}
	m_target.updatecentralbody(hmajor);
	m_minor.updatecentralbody(hmajor);
	vars.crosscopy(templbase->vars);//copies all values from variables in one basefunction / planfunction to the other
}


void basefunction::dolowpriaction()
{
	VECTOR3 temp;
	calculate(&temp);
}

bool basefunction::soistatus()
{
	//Check that we're not too far outside SOI
	double soi=mappointer->getsoisize(hmajor);
	VECTOR3 posvector;
	oapiGetRelativePos(hmajor,hcraft,&posvector);
	if (soi*soi*4<length2my(posvector)) return false;

	//Check that we're not too far inside target SOI
	if (hmajtarget==NULL) return true;
	soi=mappointer->getsoisize(hmajtarget);
	oapiGetRelativePos(hmajtarget,hcraft,&posvector);
	if (soi*soi>length2my(posvector)) return false;
	return true;
}


void basefunction::setnextplanstate(int plantype,int plan,int targettype)
{//Uses previous plan type to guess the next
	m_planthrough.setshow(false);
	m_planinitial.setshow(false);
	m_planminor.setshow(false);
	switch (targettype)
	{//Cases 0 and 3 do not permit propagation
	case 1://Escaped from previous object
		if (plantype==0)//Last was first object
		{
			m_plantype=2;
			m_planminor.setshow(true);
			m_planminor=1;//Major Eject
		}
		else
		{
			m_plantype=2;
			m_planminor.setshow(true);
			m_planminor=2;//Sling Direct
		}
		break;
	case 2://Target of previous object
		m_plantype=1;
		m_planthrough.setshow(true);
		m_planthrough=1;//Slingshot (Encounter in same group)
		break;
	}
}

void basefunction::setplanstate(int plantype,int plan)
{
	m_plantype=plantype;
	switch (plantype) {
	case 0:
		m_planinitial.setshow(true);
		m_planinitial=plan;
		break;
	case 1:
		m_planminor.setshow(true);
		m_planminor=plan;
		break;
	case 2:
		m_planthrough.setshow(true);
		m_planminor=plan;
		break;
	}
}

void basefunction::getplanstate(int *xplantype, int *xplan,int *targettype)
{
	*xplantype=m_plantype;
	*targettype=m_target;
	switch (m_target)
	{
	case 0:
		*xplan=m_planinitial;
		return;
	case 1:
		*xplan=m_planminor;
		return;
	case 2:
		*xplan=m_planthrough;
		return;
	}
	return;
}

void basefunction::autoplan()
{
	if (m_planauto==0)
	{
		m_planinitial.setshow(false);
		m_planthrough.setshow(false);
		m_planminor.setshow(false);
		switch (m_plantype)
		{
		case 0:
			if (m_target==1)//=escape
			{
				m_planinitial=1;
			}
			else
			{
				m_planinitial=0;
			}
			return;
		case 1:
			m_planthrough=2-m_target;//Just happens to sum up the required relationship
			return;
		case 2:
			if (previousfunc==NULL)
			{
				m_planminor=0;
				return;
			}
			int prevtarget=previousfunc->gettargettype();
			if (prevtarget==0) m_planminor=1;
			if (prevtarget==1) m_planminor=2;
			if (prevtarget==2) m_planminor=0;
			return;
		}
	}
}


void basefunction::updateplan()
{
	bool changeplan=false;
	if (m_plantype==iplantype)
	{
		switch (iplantype){
		case 0:
			if (m_planinitial!=iplan) changeplan=true;
			break;
		case 1:
			if (m_planthrough!=iplan) changeplan=true;
			break;
		case 2:
			if (m_planminor!=iplan) changeplan=true;
			break;
		}
	}
	else
	{
		changeplan=true;
	}
	if (!changeplan) return;

	if (planpointer!=NULL)
	{
		delete planpointer;
		planpointer=NULL;
	}
	switch (m_plantype) {
	case 0:
		if (m_planinitial==1)
		{
			planpointer=new minorejectplan();
		}
		iplan=m_planinitial;
		break;
	case 1:
		if (m_planthrough==1) planpointer=new slingshot();
		if (m_planthrough==2) planpointer=new encounterplan();
		iplan=m_planthrough;
		break;
	case 2:
		if (m_planminor==1) planpointer=new majorejectplan();
		if (m_planminor==2) planpointer=new slingejectplan();
		iplan=m_planminor;
		break;
	}
	if (planpointer!=NULL) planpointer->init(&vars,this);
	iplantype=m_plantype;
}


void basefunction::onplaceindeletebuffer()//ESSENTIAL cleanup
{
	if (previousfunc!=NULL) previousfunc->setnextfunc(NULL);
	if (nextfunc!=NULL) nextfunc->setpreviousfunc(NULL);
	previousfunc=nextfunc=NULL;
}


basefunction::~basefunction()
{
	delete planpointer;//Delete any plan that this function has
	//target.release(); // release any shared memory in the target FIXME - this needs to be released somewhere.
	if (previousfunc!=NULL) previousfunc->setnextfunc(NULL);
	if (nextfunc!=NULL) nextfunc->setpreviousfunc(NULL);
}


int basefunction::calcnewview(int oldview,bool firststage)
{
	switch (oldview)
	{
	case 1:
		return 2;
	case 2:
		updateplan();//Check plan is up to date
		if (planpointer==NULL)
		{
			return 4;
		}
		return 3;
	case 3:
		if (firststage) return 4;
		//otherwise fall through to next case
	case 4:
		if (m_prograde*m_prograde+m_outwardvel*m_outwardvel+m_chplvel*m_chplvel>0.0001)
		{
			return 1;
		}
		return 2;
	}
	//should never happen
	return 2;
}


void basefunction::handlesfornextfunction(OBJHANDLE *thmajor, OBJHANDLE *thminor)
{
	OBJHANDLE basichandle=m_target.gethandle();
	if (m_target==2)
	{
		//aiming for target
		*thmajor=basichandle;
		*thminor=NULL;
		return;
	}
	if (m_target==1)
	{
		//escape
		*thmajor=basichandle;
		*thminor=hmajor;
		return;
	}
	//Not allowed to create new function
	*thmajor=NULL;
}



void basefunction::saveself(FILEHANDLE scn)
{
	int id;
	if (planpointer==NULL)
	{
		id=0;
	}
	else
	{
		id=planpointer->getplanid();
	}
	oapiWriteScenario_int(scn,"Int",id);//Saves the plan type
	saveorbit(scn,basisorbit);
	savehandle(scn,hmajor);
	savehandle(scn,hminor);
	savehandle(scn,hmajtarget);
	vars.saveallvariables(scn);
	oapiWriteScenario_string(scn,"Finish","BaseFunction");
}

void basefunction::restoreself(FILEHANDLE scn)
{
	//Load the plan type
	int temp2;
	if (!loadint(scn,&temp2)) return;
	//Check plan constant is in range
	if (temp2<0) return;
	loadplan(temp2);
	OBJHANDLE temp;
	if (!loadorbit(scn,&basisorbit)) return;
	if (!loadhandle(scn,&temp)) return;
	if (temp!=NULL) sethmajor(temp);//hmajor must never be set null
	if (!loadhandle(scn,&temp)) return;
	sethminor(temp);
	m_target.updatecentralbody(hmajor);
	m_minor.updatecentralbody(hmajor);
	if (!loadhandle(scn,&hmajtarget)) return;
	if (!vars.loadallvariables(scn)) return;
	findfinish(scn);
}


void basefunction::loadplan(int plan)
{
	switch (plan){
	case 0:
		planpointer=NULL;
		return;
	case 1:
		planpointer=new minorejectplan();
		iplantype=0;
		iplan=1;
		break;
	case 2:
		planpointer=new majorejectplan();
		iplantype=2;
		iplan=1;
		break;
	case 3:
		planpointer=new slingejectplan();
		iplantype=2;
		iplan=2;
		break;
	case 4:
		planpointer=new slingshot();
		iplantype=1;
		iplan=1;
		break;
	case 5:
		planpointer=new encounterplan();
		iplantype=1;
		iplan=2;
		break;
	}
	if (planpointer!=NULL) planpointer->init(&vars,this);
}


bool basefunction::initialisevars()
{
	m_target.init(&vars,2,2,"Select Target",hmajor);
	m_planauto.init(&vars,2,2,"Autoplan",0,1,"On","Off","","","");
	m_plantype.init(&vars,2,2,"Plan type",0,2,"Initial","Through point","Cruise plan","","");
	m_planinitial.init(&vars,2,2,"Plan",0,1,"None","Escape","","","");//No minor & not hyp. approach
	m_planthrough.init(&vars,2,2,"Plan",0,2,"None","Slingshot","Encounter","","");//Hyp approach
	m_planminor.init(&vars,2,2,"Plan",0,2,"None","Eject","Sling Direct","","");//Minor body defined
	m_minor.init(&vars,2,2,"Select Minor",hmajor);
	m_manoeuvremode.init(&vars,4,4,"Manoeuvre mode",0,1,"Off","On","","","");
	m_updbaseorbit.init(&vars,4,4,"Base Orbit",1,1,"++ Updates","Updating","","","");
	m_prograde.init(&vars,4,4,"Prograde vel.", 0, -1e8, 1e8, 0.1, 1000);
	m_ejdate.init(&vars,4,4,"Man. date", 0, 0, 1e20, 0.00001, 1000000);
	m_outwardvel.init(&vars,4,4,"Outward vel.", 0,-1e8,1e8,0.1,1000);
	m_chplvel.init(&vars,4,4,"Ch. plane vel.", 0, -1e8, 1e8, 0.1,1000);
	m_intwith.init(&vars,2,2,"Intercept with",0,3,"Auto","Plan","Manoeuvre","Focus","");
	m_orbitsahead.init(&vars,2,2,"Orbits to Icept",0);
	m_graphprj.init(&vars,2,2,"Graph projection",0,4, "Ecliptic","Focus","Manoeuvre","Plan","Edge On");
	m_scale.init(&vars,2,2,"Scale to view",0,2,"All","Target","Craft","","");
	m_advanced.init(&vars,2,2,"Advanced",0,1,"Off","On","","","");
	valid=true;
	m_ejdate=oapiGetSimMJD();

	//Make invisible all variables that sometimes are invisible
	m_plantype.setshow(false);
	m_planinitial.setshow(false);
	m_planthrough.setshow(false);
	m_planminor.setshow(false);
	m_minor.setshow(false);
	m_updbaseorbit.setshow(false);
	m_prograde.setshow(false);
	m_ejdate.setshow(false);
	m_outwardvel.setshow(false);
	m_chplvel.setshow(false);


	//set up help system
	m_target.sethelpstrings(
		"Select where you're going.",
		"Choose object, or escape");
	m_plantype.sethelpstrings(
		"Advanced:Select type of plan",
		"you want to follow.");
	m_planinitial.sethelpstrings(
		"None for simple trips.",
		"Escape when leaving SOI");
	m_planthrough.sethelpstrings(
		"Choose what to do at this",
		"planet");
	m_planminor.sethelpstrings(
		"None for simple trips,",
		"Eject for leaving an SOI");
	m_minor.sethelpstrings(
		"Advanced:Set minor body",
		"");
	m_manoeuvremode.sethelpstrings(
		"Enable manoeuvre settings",
		"");
	m_advanced.sethelpstrings(
		"Enable advanced settings",
		"");
	m_scale.sethelpstrings(
		"Set to 'Target' to obtain better",
		"view when target is small");
	m_updbaseorbit.sethelpstrings(
		"Orbit on which manoeuvres",
		"are based. ++ sets to focus orbit");
	m_prograde.sethelpstrings(
		"Positive numbers to outer planets.",
		"Negative numbers to inner planets.");
	m_ejdate.sethelpstrings(
		"Your planned launch date/time",
		"Alter to seek good launch windows");
	m_outwardvel.sethelpstrings(
		"Add velocity outwards/inwards.",
		"Allows launch date flexibility.");
	m_chplvel.sethelpstrings(
		"Use to swing grey plane intersection",
		"line towards intercept point.");
	m_graphprj.sethelpstrings(
		"Adjusts the way MFD displays orbit",
		"Adjust to taste");
	m_intwith.sethelpstrings(
		"Intercept info shown for whatever",
		"you select here.");
	m_orbitsahead.sethelpstrings(
		"The number of orbits ahead to",
		"look for an intercept with target");
	sethelp(
		"This plan (None) is used largely for",
		"simple cruise to a target.",
		"Select an alternative plan for more",
		"complex manoeuvres. Use FWD to",
		"create next stage.");

	return true;
}

void basefunction::processvisiblevars(int currview)
{//Deals with changes in variable visibility
	switchadvanced();
	switchplantype();
	switchmanoeuvremode(currview);
	autoplan();
}

void basefunction::switchplantype()
{
	m_planinitial.setshow(false);
	m_planthrough.setshow(false);
	m_planminor.setshow(false);
	switch (m_plantype){
	case 0:
		m_planinitial.setshow(true);
		return;
	case 1:
		m_planthrough.setshow(true);
		return;
	case 2:
		m_planminor.setshow(true);
		return;
	}
}


void basefunction::switchmanoeuvremode(int currview)
{
	if (m_manoeuvremode==1)
	{
		m_updbaseorbit.setshow(true);
		m_prograde.setshow(true);
		m_ejdate.setshow(true);
		m_outwardvel.setshow(true);
		m_chplvel.setshow(true);
	}
	else
	{
		m_updbaseorbit.setshow(false);
		m_prograde.setshow(false);
		m_ejdate.setshow(false);
		m_outwardvel.setshow(false);
		m_chplvel.setshow(false);
		m_prograde=m_outwardvel=m_chplvel=0;
		m_ejdate=oapiGetSimMJD();
	}
}

void basefunction::switchadvanced()
{
	if (m_advanced==1)
	{
		m_plantype.setshow(true);
		m_minor.setshow(true);
	}
	else
	{
		m_plantype.setshow(false);
		m_minor.setshow(false);
	}
}



void basefunction::getcraftorbitattarget(OrbitElements *tcraft)
{
	if (hmajtarget==NULL || !primary.getvalid())//No attempt made to target at all
	{
		tcraft->setinvalid();
		return;
	}
	if (interceptwith==3)
	{
		tcraft->majortominorinit(hmajtarget,hcraft, primary,mappointer->getsoisize(hmajtarget));
	}
	else
	{//hypothetical
		if (getpreviousfunc()==NULL)
		{
			tcraft->majortominorinit(hmajtarget,hcraft, primary,mappointer->getsoisize(hmajtarget));//compensates better this way when it's actually time to burn a manoeuvre
		}
		else
		{
			tcraft->majortominorinit(hmajtarget,NULL, primary,mappointer->getsoisize(hmajtarget));//compensates better this way when it's actually time to burn a manoeuvre
		}
	}
}


void basefunction::calculate(VECTOR3 *targetvel)
// This is the update mode underlying all plans
{
	//Update the orbit offset from current intercept
	int full,half;
	if (fabs(oldorbitsahead-m_orbitsahead)>0.2)
	{//user input
		primary.resetintercept();
		oldorbitsahead=m_orbitsahead;
	}
	primary.getorbitsoffset(&full,&half);
	if (full!=-1)
	{//Adjusts value
		oldorbitsahead=full+0.5*half;
		m_orbitsahead=oldorbitsahead;
	}
	//Get current graph parameters from current MFD
	hypormaj.setinvalid();
	if (!m_target.validate()) hmajtarget=NULL;//Ensure handle still exists in Orbiter
	VECTOR3 temp={0,0,0};
	// Get positions of major, minor bodies and spacecraft
	if (previousfunc==NULL)
	{
		hcontext=mappointer->getmajor(hmajor);
		if (hcontext!=hmajor)
		{
			context.init(hcontext,hmajor);//the orbit of hmajor around its boss
		}
		else
		{
			context.setinvalid();//No body above this one
			hcontext=NULL;
		}
		// Initialise orbit info for craft from Orbiter data
		previousexists=false;
		VECTOR3 pos,vel;
		oapiGetRelativeVel(hcraft,hmajor,&vel);
		oapiGetRelativePos(hcraft,hmajor,&pos);
		double distance=length(pos);
		double velenergy=length2my(vel)*0.5;
		double overallenergy=-GRAVITY*oapiGetMass(hmajor)/distance+velenergy;
		craft.init(hmajor, hcraft);
		if (hminor==NULL)
			rmin.setinvalid();//No minor body on first graph
		else
			rmin.init(hmajor,hminor);
	}
	else//There is a graph before this one
	{
		//There is a previous function - must inherit craft params from it
		previousexists=true;
		craft.setinvalid();
		OBJHANDLE temp=previousfunc->gethmajor();
		if (temp==hminor)
		{
			//Inheriting from 'escape' on previous function
			hcontext=mappointer->getmajor(hmajor);
			if (hcontext!=hmajor)
			{
				context.init(hcontext,hmajor);
			}
			else
			{
				context.setinvalid();
				hcontext=NULL;
			}
			rmin=previousfunc->getcontextorbit();
			craft.minortomajorinit(previousfunc->getpassforwardorbit(),rmin,mappointer->getsoisize(hminor));
		}
		else
		{
			//Inheriting from 'target' on previous function
			if (previousfunc->gethtarget()==hmajor)
			{
				context=previousfunc->gettargetorbit();
				hcontext=previousfunc->gethmajor();
			}
			else
			{//user has changed target since initialisation!
				//getting context ourselves
				hcontext=mappointer->getmajor(hmajor);
				context.init(hcontext,hmajor);
			}
			rmin.setinvalid();
			previousfunc->getcraftorbitattarget(&craft);
		}
	}

	//Calculate the plan update
	if (planpointer!=NULL)
	{
		planpointer->calculate(&vars,this);
	}

	//Initialise orbit info for target
	//Set hmajtarget
	target.setinvalid();
	if (m_target>1)
	{
		hmajtarget=m_target.gethandle();
		target.init(hmajor,hmajtarget);
	}
	else
	{
		hmajtarget=NULL;
	}

	if ((!basisorbit.isvalid() || m_updbaseorbit==1 || m_manoeuvremode==0) && craft.isvalid())
	{
		basisorbit=craft;//set the basis orbit even if we're not using it
		m_updbaseorbit=0;
	}
	Getmode2hypo(targetvel);
	OrbitElements planorbit;
	if (planpointer!=NULL)
	{
		planpointer->getplanorbit(&planorbit);
	}

	//Now have craft,minor,target and hypo
	interceptflag=false;
	interceptwith=m_intwith;
	double orbitnum=m_orbitsahead;
	if (interceptwith==0)
	{
		if (craft.isvalid()) interceptwith=3;
		if (hypormaj.isvalid()) interceptwith=2;
		if (planorbit.isvalid()) interceptwith=1;
	}
	if (target.isvalid() && interceptwith==3 && craft.isvalid())
	{
		primary.updateintercept(craft,target,orbitnum);
		interceptflag=true;
		return;
	}
	if (target.isvalid() && interceptwith==2 && hypormaj.isvalid())
	{
		primary.updateintercept(hypormaj,target,orbitnum);
		interceptflag=true;
		return;
	}
	if (target.isvalid() && interceptwith==1 && planorbit.isvalid())
	{
		primary.updateintercept(planorbit,target,orbitnum);
		interceptflag=true;
		return;
	}
}

const OrbitElements & basefunction::getpassforwardorbit()
{
	switch (interceptwith)
	{
	case 1://Never pass the plan - this is communicated in a different way
		return craft;
	case 2:
		return hypormaj;
	case 3:
		return craft;
	}
	return craft;
}

void basefunction::doupdate(oapi::Sketchpad *sketchpad,int tw, int th,int viewmode)
{
	if (!valid) return;
	if (!m_target.validate()) hmajtarget=NULL;
	int linespacing=th/24;
	VECTOR3 targetvel;
	calculate(&targetvel);

	int wpos=0;
	int hpos=linespacing;
	char buffer[20]="";
	OrbitElements plan;
	if (planpointer!=NULL)
	{
		planpointer->getlabel(buffer);
		sketchpad->Text(wpos,hpos,buffer,strlen(buffer));
		planpointer->getplanorbit(&plan);
	}
	hpos+=linespacing;

	//Print the bodies
	TextShow(sketchpad,"MAJ:",wpos,hpos,hmajor);
	hpos+=linespacing;
	if (hminor!=NULL) TextShow(sketchpad,"MIN:",wpos,hpos,hminor);
	hpos-=linespacing;
	wpos=tw/2;
	if (hmajtarget!=NULL) TextShow(sketchpad,"TGT:",wpos,hpos,hmajtarget);
	hpos-=linespacing;
	switch (viewmode)
	{
	case 1:
		strcpy(buffer,"View:Target");
		break;
	case 2:
		strcpy(buffer,"View:Setup");
		break;
	case 3:
		if (planpointer!=NULL)
		{
			planpointer->getviewname(buffer);
			break;
		}
		else
		{
			viewmode=4;
			//No break here
		}
	case 4:
		strcpy(buffer,"View:Manoeuvre");
		break;
	default:
		strcpy(buffer,"View:Setup");
		viewmode=2;
		break;
	}
	sketchpad->Text(wpos,hpos,buffer,strlen(buffer));
	graph.setviewwindow(0,0,tw,th);
	if (viewmode==1 && hypormaj.isvalid())//Target view
	{
		double timeoffset=(m_ejdate-simstartMJD)*SECONDS_PER_DAY-craft.gettimestamp();
		VECTOR3 craftpos,craftvel;
		craft.timetovectors(timeoffset,&deltavel);//New eccentricity insensitive timetovectors
		deltavel.getposvel(&craftpos,&craftvel);
		VECTOR3 diffTgtVel = targetvel-craftvel;
		VESSEL *pV=oapiGetVesselInterface(hcraft);
		double rvel=graph.vectorpointdisplay(sketchpad, diffTgtVel, state->GetMFDpointer(), pV, false);
		double burnStart = GetBurnStart(pV, THGROUP_MAIN, timeoffset, rvel);
		TextShow(sketchpad,"Delta V: ",0,18*linespacing,rvel);
		TextShow(sketchpad,"T to Mnvre: ",0,19*linespacing,timeoffset);
		TextShow(sketchpad,"Begin Burn: ",0,20*linespacing,burnStart);
	}
	else
	{
		VECTOR3 ntemp={0,-1,0};
		VECTOR3 edgeon={0,0,-1};
		graph.setprojection(ntemp);
		switch (m_graphprj)
		{
		case 1:
			graph.setprojection(craft);
			break;
		case 2:
			graph.setprojection(hypormaj);
			break;
		case 3:
			graph.setprojection(plan);
			break;
        case 4:
			graph.setprojection(edgeon);
			break;
		}
		if (planpointer!=NULL && viewmode==3)
		{
			if (!planpointer->maingraph(sketchpad,&graph,this))
			{
				planpointer->wordupdate(sketchpad,tw,th,this);
				return;
			}
		}
		double scale=0;

		// Set the viewscale for the graph
		graph.setviewscale(scale);
		graph.setviewscalesize(oapiGetSize(hmajor)*1.2);//Ensure no zoom inside the central body
		if (m_scale!=1)// || !target.isvalid()) //Only set viewscale to view these if user desires it
		{
			if (previousexists && craft.geteccentricity()>1)
			{
				graph.setviewscalesize(craft.getpedistance()*4);
			}
			else
			{
				graph.setviewscale(craft);
			}
			graph.setviewscale(rmin);
		}
		if (target.isvalid() && m_scale!=2) graph.setviewscale(target);
		if (hypormaj.isvalid() && m_scale==0) graph.setviewscale(hypormaj);
		if (planpointer!=NULL && m_scale==0)
		{
			planpointer->graphscale(&graph);//Set scale for the plan
		}

		// Draw the craft orbit
//////////////////////////////////////////////////	FIXME
		SelectDefaultPen(sketchpad,Green);
//////////////////////////////////////////////////	This kills pens[]
		graph.draworbit(craft, sketchpad, !previousexists);

		//Draw the minor object orbit
		SelectDefaultPen(sketchpad,Blue);
		graph.draworbit(rmin, sketchpad, true);

		//Draw the hypothetical manoeuvre orbit
		if (hypormaj.isvalid())
		{
			SelectDefaultPen(sketchpad,Yellow);
			graph.draworbit(hypormaj,sketchpad,true);
		}

		if (planpointer!=NULL)
		{
			planpointer->graphupdate(sketchpad,&graph,this);
		}

		//Draw the central body
		SelectDefaultPen(sketchpad,PEN_ATMOSPHERE);
		graph.drawatmosphere(sketchpad,hmajor);
		SelectDefaultPen(sketchpad,Grey);
		graph.drawplanet(sketchpad,hmajor);

		// If there is a target, draw it, and if there's an intercept,the targeting lines
		if (target.isvalid())
		{
			SelectDefaultPen(sketchpad,Blue);
			graph.draworbit(target, sketchpad, true);
			if (interceptflag)
			{
				SelectDefaultPen(sketchpad,Yellow);
				VECTOR3 craftpos, targetpos, intersect;
				primary.getpositions(&craftpos,&targetpos);
				graph.drawtwovector(sketchpad, craftpos,targetpos);
				primary.getplanecept(&intersect);
				SelectDefaultPen(sketchpad,GreyDashed);
				// Draws orbits intersection line with plane change
				graph.drawvectorline(sketchpad,intersect);
				SelectDefaultPen(sketchpad,Grey);
				VECTOR3 intersectRef = GetLineOfNodes(); // No change plane line of nodes
				graph.drawvectorline(sketchpad,intersectRef);

				//Describe targeting quality
				int hpos=8*linespacing;
				int wpos=0;
				double closestApp = length(craftpos-targetpos);
				TextShow(sketchpad, "Cl. App.: ", wpos, hpos, closestApp);
				hpos+=linespacing;
				TextShow(sketchpad, "Hohmann dv: ", wpos, hpos, GetHohmannDV());
				hpos+=linespacing;
				VECTOR3 relvel;
				primary.getrelvel(&relvel);
				TextShow(sketchpad,"Enc. V:", wpos, hpos, length(relvel));
				hpos+=linespacing;
				double intercepttime=primary.gettimeintercept();
				double arrmjd=oapiTime2MJD(intercepttime);
				int len=snprintf(buffer, sizeof(buffer) - 1, "Enc. MJD %.4f", arrmjd);
				sketchpad->Text(wpos, hpos, buffer, len);
			}
		}
		else
		{
			//Describe information relevant to central body
			int hpos=8*linespacing;
			int wpos=0;
			TextShow(sketchpad,"Maj. Rad: ",wpos,hpos,oapiGetSize(hmajor));
			hpos+=linespacing;
			if (craft.isvalid())
			{
				TextShow(sketchpad,"Focus PeD:", wpos, hpos, craft.getpedistance());
				if (craft.geteccentricity()<1)
				{
					hpos+=linespacing;
					TextShow(sketchpad,"Focus ApD:", wpos,hpos, craft.getapodistance());
				}
				hpos+=linespacing;
				char buffer[20]="";
				int length=snprintf(buffer, sizeof(buffer) - 1, "Pe MJD:   %.4f",(craft.getpedeltatime()+craft.gettimestamp())/SECONDS_PER_DAY+simstartMJD);
				sketchpad->Text(wpos,hpos,buffer, length);
				hpos+=linespacing;
				VECTOR3 south = {0, -1, 0};
				length = snprintf(buffer, sizeof(buffer) - 1, "Inc:      %.4g°", 180/PI*acos(cosangle(south, craft.getplanevector())));
				sketchpad->Text(wpos,hpos,buffer, length);
				hpos+=linespacing;
			}
			if (hypormaj.isvalid())
			{
				TextShow(sketchpad,"Hyp PeD ",wpos,hpos,hypormaj.getpedistance());
				hpos+=linespacing;
				double timeatped=(hypormaj.gettimestamp()+hypormaj.getpedeltatime())/SECONDS_PER_DAY+simstartMJD;
				int length=snprintf(buffer, sizeof(buffer) - 1, "H. Pe MJD %.2f", timeatped);
				sketchpad->Text(wpos,hpos,buffer,length);
			}
		}
		if (viewmode==3 && planpointer!=NULL)
		{
			planpointer->wordupdate(sketchpad,tw,th,this);
		}
	}
}

void basefunction::Getmode2hypo(VECTOR3 *targetvel)
// This obtains the current hypothetical orbit in rmin influence
{
	hypormaj.setinvalid();
	if (!basisorbit.isvalid()) return;
	if (m_prograde*m_prograde+m_outwardvel*m_outwardvel+m_chplvel*m_chplvel<0.0001) return;//No need to calculate if result is less than 1 cm per second!
	// New section
	double difftime=(m_ejdate-simstartMJD)*SECONDS_PER_DAY-basisorbit.gettimestamp();//Converts to seconds from present time
	VECTOR3 ejradius, ejvel;

	basisorbit.timetovectors(difftime,&mode2orbittime);
	mode2orbittime.getposvel(&ejradius,&ejvel);

	//basisorbit.timetovectors(difftime, &ejradius, &ejvel);
	VECTOR3 forward=unit(ejvel)*m_prograde;
	VECTOR3 outward=unit(crossp(ejvel, basisorbit.getplanevector()))*m_outwardvel;
	VECTOR3 sideward=unit(basisorbit.getplanevector())*m_chplvel;
	VECTOR3 ejectvector=forward+outward+sideward; //=Eject vector in basisorbit frame

	VECTOR3 hypopos, hypovel;
	// Add to basisorbit vectors at eject time to give ejection vector in rmaj
	hypopos=ejradius;
	hypovel=ejectvector+ejvel;
	*targetvel=hypovel;

	//Create hypothetical orbit in rmaj
	hypormaj.init(hypopos, hypovel, (m_ejdate-simstartMJD)*SECONDS_PER_DAY, basisorbit.getgmplanet());
}

bool basefunction::IsPlanSlingshot()
{
    return getplanpointer() && getplanpointer()->getplanid() == 3;
}

double basefunction::GetTimeIntercept()
{
    double intercepttime=primary.gettimeintercept();
	double arrmjd=oapiTime2MJD(intercepttime);
	return arrmjd;
}

double basefunction::GetHohmannDVExtend(double r1, double r2, double mass)
{
	const double m_mi = mass * GGRAV_;
	if (r1 > r2)
		std::swap(r1, r2);

	const double dv = sqrt(m_mi / r1) * (sqrt(2 * r2 / (r1 + r2)) - 1);
	return dv;
}

double basefunction::GetHohmannDV()
{
    OBJHANDLE currentMinor = hminor ? hminor : oapiGetFocusObject(); // Eject or Manoeuvre?
    if (!currentMinor || !hmajor || !hmajtarget)
        return 0;
    VECTOR3 posSrc, posTgt;
    oapiGetRelativePos(currentMinor, hmajor, &posSrc);
    oapiGetRelativePos(hmajtarget, hmajor, &posTgt);
    double radSrc = length(posSrc);
    double radTgt = length(posTgt);
    double dv = GetHohmannDVExtend(radSrc, radTgt, oapiGetMass(hmajor));
    if (radTgt > radSrc)
        return dv;
    else
        return -dv;
}

VECTOR3 basefunction::GetPlaneAxis(const OBJHANDLE hObj, const OBJHANDLE hRef)
{
	VECTOR3 pos, vel;
	oapiGetRelativePos(hObj, hRef, &pos);
	oapiGetRelativeVel(hObj, hRef, &vel);
	const VECTOR3& axis = crossp(pos, vel);

	return axis;
}

VECTOR3 basefunction::GetLineOfNodes()
{
    OBJHANDLE currentMinor = hminor ? hminor : oapiGetFocusObject(); // Eject or Manoeuvre?
   if (!currentMinor || !hmajor || !hmajtarget)
        return _V(0,0,0);
    const VECTOR3 & planeMinor = GetPlaneAxis(currentMinor, hmajor);
    const VECTOR3 & planeMajor = GetPlaneAxis(hmajtarget, hmajor);
	const VECTOR3 & lineOfNodes = crossp(planeMinor, planeMajor);
    return lineOfNodes;
}
