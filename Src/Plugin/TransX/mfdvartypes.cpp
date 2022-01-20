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
#include <string>
#include "orbitersdk.h"
#include "mfd.h"
#include "mfdvartypes.h"
#include "doublelink.h"
#include "basefunction.h"
#include "shiplist.h"

liststring::liststring(bool manageme) : listelement(manageme)
{
	ZeroMemory(buffer, MAX_STRING_LENGTH);
}

void MFDsemiintdiscrete::init(MFDvarhandler *vars,int viewmode1,int viewmode2,char *vname,int tvalue)
{
	value=tvalue;
	initialise(vars,viewmode1,viewmode2);
	strcpy(name,vname);
}

void MFDvarmoon::init(MFDvarhandler *vars,int viewmode1,int viewmode2,char *vname,OBJHANDLE tcentralbody)
{
	adjMode = Planet;
	initialise(vars,viewmode1,viewmode2);
	strcpy(name,vname);
	strcpy(intbuffer,"");
	centralbody=tcentralbody;
	mappointer=mapfunction::getthemap();
	value=0;
	target=NULL;
}

void MFDvarmoon::setall(class MFDvariable *var)
{
	class MFDvarmoon *ivar=dynamic_cast<class MFDvarmoon*>(var);
	if (ivar==NULL) return;
	target=ivar->target;
	centralbody=ivar->centralbody;
	value=ivar->value;
	initvalidate();
}

MFDvarshiplist::MFDvarshiplist()
{
	iterator=shiplisthead.getiterator();
	initialised=false;
}

MFDvarshiplist::~MFDvarshiplist()
{
	delete iterator;
}

void MFDvarshiplist::initbybody(OBJHANDLE craft,bool reset)
{
	if (craft==NULL) return;
	if (initialised && !reset) return;//Check to see if reinitialisation necessary
	initialised=true;
	shiplisthead.empty();
	class mapfunction *map=mapfunction::getthemap();
	class shipptr_itr shiterator;
	OBJHANDLE currship=NULL;
	OBJHANDLE craftplanet=map->getcurrbody(craft);
	OBJHANDLE currplanet=NULL;
	class shipptrs *curr=shiterator.getfirst();
	while (curr!=NULL)
	{
		currship=oapiGetVesselByName(curr->getname());
		if (currship!=NULL && currship!=craft)
		{//vessel exists and isn't our own vessel
			currplanet=curr->gettransxstate()->getbasefn(1)->gethmajor();//Get the current planet
			//from the first stage for this vessel
			if (currplanet==craftplanet)
			{
				addtolist(curr->getname());
			}
		}
		curr=shiterator.getnext();
	}
	iterator->front();
}


void MFDvarshiplist::addtolist(char *name)
{
	class liststring *temp=new liststring();
	strcpy(temp->getbuffer(),name);
	shiplisthead.addfront(temp);
}

void MFDvarmoon::dec_variable()
{
	if (adjMode == Planet)
	{
		switch (value){
		case 0:
			++value;
			break;
		case 1:
			++value;
			target=mappointer->getfirstmoon(centralbody);
			initvalidate();
			if (target==NULL)
			{
				value=0;
			}
			break;
		case 2:
			target=mappointer->getnextpeer(target);
			initvalidate();
			if (target==NULL)
			{
				value=0;
				strcpy(intbuffer,"");
			}
			break;
		default:
			value=0;
			target=NULL;
			break;
		}
	}
	else
		oapiOpenInputBox("Select Ship",SelectVariableBody,0,20, (void*)this);
}

void MFDvarmoon::initvalidate()
{
	if (target!=NULL)
		oapiGetObjectName(target,intbuffer,30);
}

void MFDvarmoon::enter_variable() {
	if (adjMode == Planet) {
		oapiOpenInputBox("Select Planet (STUB) Only ships",SelectVariableBody,0,20, (void*)this);
	} else {
		oapiOpenInputBox("Select Ship",SelectVariableBody,0,20, (void*)this);
	}
}

void MFDvarmoon::inc_variable()
{
	if (adjMode == Planet)
	{
		switch (value){
		case 0:
			value=2;
			target=mappointer->getlastmoon(centralbody);
			initvalidate();
			if (target==NULL)
				value=1;
			break;
		case 1:
			--value;
			break;
		case 2:
			target=mappointer->getpreviouspeer(target);
			initvalidate();
			if (target==NULL)
			{
				value=1;
				strcpy(intbuffer,"");
			}
			break;
		default:
			value=0;
			target=NULL;
			break;
		}
	}
	else
		oapiOpenInputBox("Select Ship",SelectVariableBody,0,20, (void*)this);
}

void MFDvarmoon::ch_adjmode()
{
	if(adjMode == Planet)
		adjMode = Craft;
	else
		adjMode = Planet;
}

void MFDvarmoon::showadjustment(oapi::Sketchpad *sketchpad, int width, int line) const
// This shows the mode of adjustment currently in force for the current MFDvariable
{
	char buffer[20]="";
	int ypos=int(7*line);
	int xpos=int(width/2);
	int length;
	switch (adjMode)
	{
	case Planet:
		length=sprintf(buffer,"Planets/Moons");
		break;
	case Craft:
		length=sprintf(buffer,"Ships");
		break;
	}
	sketchpad->Text(xpos, ypos, buffer, length);
}

void MFDvarmoon::getsaveline(char *buffer) const
{
	sprintf(buffer, " %d ", adjMode);
	char tbuffer[30];
	if (target!=NULL)
		oapiGetObjectName(target,tbuffer,30);
	else
	{
		if (value==1)
			strcpy(tbuffer,"Escape");
		else
			strcpy(tbuffer,"None");
	}
	strcat(buffer,tbuffer);
}

bool MFDvarmoon::loadvalue(char *buffer)
{
	strcpy(intbuffer,buffer);
	if (strcmp(buffer,"None")==0)
	{
		target=NULL;
		value=0;
		return true;
	}
	if (strcmp(buffer,"Escape")==0)
	{
		target=NULL;
		value=1;
		return true;
	}
	value=2;
	target=oapiGetGbodyByName(buffer);
	if (target==NULL)
	{
		target=oapiGetObjectByName(buffer);
		value=3;
	}
	if (target==NULL)
		value=0;
	else
		strcpy(intbuffer,buffer);
	return true;
}


bool MFDvarmoon::SetVariableBody(char *str)
{
	OBJHANDLE temp=oapiGetGbodyByName(str);
	if (temp !=NULL)
		return false;
	temp=oapiGetObjectByName(str);
	if (temp==NULL) return false;
	OBJHANDLE temp2=mappointer->getcurrbody(temp);
	if (temp2!=centralbody)
		return false;
	target=temp;
	strcpy(intbuffer,str);
	value=3;
	return true;
}

bool MFDvarmoon::validate()
{
	OBJHANDLE temp=oapiGetObjectByName(intbuffer);
	if (temp==target) return true;
	return false;
}

bool MFDvarmoon::show(oapi::Sketchpad *sketchpad, int width, int line)
{
	char buffer[20]="";
	if (target!=NULL)
	{
		oapiGetObjectName(target,buffer,20);
		strcpy(intbuffer,buffer);
	}
	else
	{
		if (value==1)
			strcpy(buffer,"Escape");
		else
			strcpy(buffer,"None");
	}
	showgeneric(sketchpad, width, line, buffer);
	return true;
}

OBJHANDLE MFDvarmoon::gethandle() const
{
	if (value!=1) return target;
	OBJHANDLE temp=mappointer->getmajor(centralbody);
	if (temp==centralbody)
		return NULL;
	else
		return temp;
}

int MFDvarmoon::getvalue() const
{
	return value;
}

MFDvarfloat::MFDvarfloat()
: adjMode(Coarse)
{
	continuous = true;
}

void MFDvarfloat::init(MFDvarhandler *vars,int viewmode1,int viewmode2,char *vname, double vvalue, double vmin, double vmax, double vincrement, double vlogborder)
{
	initialise(vars,viewmode1,viewmode2);
	strcpy(name,vname);
	defaultvalue=value=vvalue;
	min=vmin;
	max=vmax;
	increment=vincrement;
	logborder=vlogborder;
}

bool MFDvarshiplist::show(oapi::Sketchpad *sketchpad,int width,int line)
{
	char buffer[20]="";
	liststring *entry=static_cast<liststring*>(iterator->current());//It is this type
	if (entry==NULL)
		strcpy(buffer,"New Plan");
	else
	{
		char *shipname=entry->getbuffer();
		int length=strlen(shipname);
		if (length>19)
			length=19;
		strncpy(buffer,shipname,length);
		buffer[length]=0;
	}
	return showgeneric(sketchpad, width,line,buffer);
}


OBJHANDLE MFDvarshiplist::gethandle() const
{
	liststring *entry=static_cast<liststring*>(iterator->current());//It is this type
	if (entry==NULL)
		return NULL;
	return oapiGetVesselByName(entry->getbuffer());
}

void MFDvarshiplist::enter_variable() {
	oapiOpenInputBox("Select Ship",SelectVariableBody,0,20, (void*)this);
}

void MFDvarshiplist::inc_variable()
{
	iterator->next();
}

void MFDvarshiplist::dec_variable()
{
	iterator->previous();
}

bool MFDvarfloat::show(oapi::Sketchpad *sketchpad, int width, int line)
{
	char buffer[20]="";
	int linecentre=(int) width/2;
	int linepos= 6*line;
	strcpy(buffer,name);
	int length=strlen(buffer);
	sketchpad->Text(linecentre, linepos, buffer, length);
	showadjustment(sketchpad, width, line);
	linepos+=line+line;
	TextForm(buffer," ",value);
	showgeneric(sketchpad, width, line, buffer);
	return true;
}

void MFDvarfloat::getsaveline(char *buffer) const
{
	sprintf(buffer, " %i ", adjMode);
	char tbuffer[30];
	sprintf(tbuffer," %.12g",value);
	strcat(buffer,tbuffer);
}

bool MFDvarfloat::loadvalue(char *buffer)
{
	value=atof(buffer);
	return true;
}

void MFDvarfloat::ch_adjmode()
// Change the adjustment mode of this MFDvariable
{
    if (adjMode == Reset)
        adjMode = Rough;
    else
        adjMode = (AdjustMode)((int)adjMode + 1);

    if (adjMode == AutoMin)
        ch_adjmode(); // Ignore this mode if there's no optimiser
}

void MFDvarfloat::chm_adjmode()
{
    if (adjMode == Rough)
        adjMode = Reset;
    else
        adjMode = (AdjustMode)((int)adjMode - 1);

    if (adjMode == AutoMin)
        chm_adjmode(); // Ignore this mode if there's no optimiser
}

void MFDvarfloat::showadjustment(oapi::Sketchpad *sketchpad, int width, int line) const
// This shows the mode of adjustment currently in force for the current MFDvariable
{
	char buffer[MAX_NAME_LENGTH]="";
	int ypos=int(7*line);
	int xpos=int(width/2);
	int length;
	switch (adjMode)
	{
    case Rough:
		length=sprintf(buffer,"Rough");
		break;
	case Coarse:
		length=sprintf(buffer,"Coarse");
		break;
	case Medium:
		length=sprintf(buffer,"Medium");
		break;
	case Fine:
		length=sprintf(buffer,"Fine");
		break;
	case Super:
		length=sprintf(buffer,"Super");
		break;
	case Ultra:
		length=sprintf(buffer,"Ultra");
		break;
	case Hyper:
		length=sprintf(buffer,"Hyper");
		break;
    case Micro:
		length=sprintf(buffer,"Micro");
		break;
	case AutoMin:
		length=sprintf(buffer,"Auto-Min™");
		break;
	case Reset:
		length=sprintf(buffer,"Reset");
		break;
	}
	sketchpad->Text(xpos, ypos, buffer, length);
}

void MFDvarfloat::enter_variable() {
    char tbuffer[128];
	sprintf_s(tbuffer,"%.12g",value);
	oapiOpenInputBox("Enter number. 'x' to reset, 'number+/number-' to inc/decrement",SelectVariableFloat,tbuffer,20, (void*)this);
}

bool MFDvarfloat::floatvalidate(char * str, double * dfinal, double valcurrent, double valdefault) {
	double d;
	char *endstr;
	d = strtod( str, &endstr );
	int lfullstr=strlen(str);
	int lendstr=strlen(endstr);
	if(d==0.0) {
		if(lfullstr==lendstr) {
			if(lfullstr==1 && *endstr=='x') {
				*dfinal=valdefault;
				return true;
			}
		}
	} else if ( lendstr==1 && ( *endstr=='+' || *endstr=='-' ) ) {
		*dfinal=(*endstr=='+'?valcurrent+(d):valcurrent-(d));
		return true;
	} else if (lendstr==0) {
		*dfinal=d;
		return true;
	}
	return false;
}

bool MFDvarfloat::SetVariableFloat(char *str) { // FIXME: isangle
	double num=0.0;
	int ret=floatvalidate(str,&num,value,defaultvalue);
	if(ret==1) {
		if(num<=max && num>=min) {
			value=num;
			return true;
		}
	}
	return false;
}

bool MFDvarfloat::ShouldBeOptimised()
{
    return adjMode == AutoMin;
}

double MFDvarfloat::GetAdjuster()
{
    switch (adjMode){
	case Rough:
		return 0.5;
	case Coarse:
		return 0.1;
	case Medium:
		return 0.01;
	case Fine:
		return 0.001;
	case Super:
		return 0.0001;
	case Ultra:
		return 0.00001;
	case Hyper:
		return 0.000001;
	case Micro:
		return 0.0000001;
    default:
        return 0;
	}
}

bool MFDvarfloat::IsAdjusterSpecialCase()
{
    switch (adjMode)
    {
	    case AutoMin:
            return true;
        case Reset:
            value=defaultvalue;
            return true;
        default:
        return false;
	}
	return false;
}

double MFDvarfloat::CalcAdjustedValue(bool positive, double adjuster)
{
    double sign = positive ? 1 : -1;
    double temp=value;
	if (temp>logborder || temp<-logborder)
		temp+=sign*fabs(temp)*adjuster*increment;
	else
		temp+=sign*logborder*adjuster*increment;
    if (positive) {
        if (temp>max) temp=max;
    } else {
        if (temp<min) temp=min;
    }
	return temp;
}

void MFDvarfloat::inc_variable()
{
    if (IsAdjusterSpecialCase())
        return;
	double adjuster=GetAdjuster();
	value=CalcAdjustedValue(true, adjuster);
}

void MFDvarfloat::dec_variable()
{
    if (IsAdjusterSpecialCase())
        return;
	double adjuster=GetAdjuster();
	value=CalcAdjustedValue(false, adjuster);
}

void MFDvarfloat::setvalue(double tvalue)
{
	value=tvalue;
}

double MFDvarfloat::getvalue() const
{
	return value;
}

MFDvarfloat::~MFDvarfloat()
{}

bool MFDsemiintdiscrete::show(oapi::Sketchpad *sketchpad, int width, int line)
{
	char buffer[20]="";
	double temp=value*0.5;
	sprintf(buffer,"%.1f",temp);
	showgeneric(sketchpad, width, line,buffer);
	return true;
}

bool MFDvarMJD::show(oapi::Sketchpad *sketchpad, int width, int line)
{
	char buffer[20]="";
	sprintf(buffer,"%.4f", value);
	showgeneric(sketchpad, width, line, buffer);
	return true;
}

void MFDvarMJD::CalcAdjustedValue(bool positive)
{
    double sign = positive ? 1 : -1;
	if(adjMode == Coarse)
	{
		value += sign * 5500.8249307686044282429796200623 / SECONDS_PER_DAY;
	}
	else
	{
	    if (positive)
            MFDvarfloat::inc_variable();
        else
            MFDvarfloat::dec_variable();
        if(adjMode == Reset)
            value = oapiGetSimMJD();
	}
}

void MFDvarMJD::inc_variable()
{
    CalcAdjustedValue(true);
}

void MFDvarMJD::dec_variable()
{
    CalcAdjustedValue(false);
}

void MFDvarshiplist::init(MFDvarhandler *vars,int viewmode1,int viewmode2,char *vname)
{
	initialise(vars,viewmode1,viewmode2);
	strcpy(name,vname);//brings in variable name
}

void MFDvardiscrete::init(MFDvarhandler *vars,int viewmode1,int viewmode2,char *vname, int vvalue, int vlimit, char *st1, char *st2, char *st3, char *st4, char *st5)
{
	initialise(vars,viewmode1,viewmode2);
	strcpy(name,vname);
	value=vvalue;
	limit=vlimit;
	strcpy(label[0], st1);
	strcpy(label[1], st2);
	strcpy(label[2], st3);
	strcpy(label[3], st4);
	strcpy(label[4], st5);
}


bool MFDvardiscrete::show(oapi::Sketchpad *sketchpad, int width, int line)
{
	showgeneric(sketchpad, width, line, label[value]);
	return true;
}

void MFDvardiscrete::getsaveline(char *buffer) const
{
	sprintf(buffer,"0 %i",value);	// need to have the adjustmode (irrelevant here) saved first
}

void MFDsemiintdiscrete::getsaveline(char *buffer) const
{
	sprintf(buffer,"0 %i",value);	// need to have the adjustmode (irrelevant here) saved first
}

bool MFDsemiintdiscrete::loadvalue(char *buffer)
{
	sscanf(buffer,"%i",&value);
	return true;
}


bool MFDvardiscrete::loadvalue(char *buffer)
{
	sscanf(buffer,"%i",&value);
	return true;
}


void MFDvardiscrete::inc_variable()
{
	if (++value>limit)
		value=0;
}

void MFDvarangle::init(MFDvarhandler *vars,char *vname, bool vloop)
{
	initialise(vars,3,3); //FIXME
	strcpy(name,vname);
	defaultvalue=value=0;
	min=-PI;
	max=PI;
	increment=PI/90.0;
	loop=vloop;
}

bool MFDvarangle::show(oapi::Sketchpad *sketchpad, int width, int line)
{
	char buffer[20]="";
	sprintf(buffer,"%.4f'", value/PI*180);
	showgeneric(sketchpad, width,line,buffer);
	return true;
}

void MFDvarangle::enter_variable() {
	char tbuffer[128];
	sprintf_s(tbuffer,"%.12g", (value/PI)*180 );
	oapiOpenInputBox("Enter cookie, but no bufu. 'x' to reset, 'num+/num-' to inc/decrement",SelectVariableAngle,tbuffer,20, (void*)this);
}

bool MFDvarangle::SetVariableAngle(char *str) { // FIXME: silly code duplication
	double num=0.0;
	int ret=floatvalidate(str,&num,(value/PI)*180,(defaultvalue/PI)*180); // Should probably give this an isangle bool because this is just silly
	if(ret==1) {
		num=(num*PI)/180;
		if(num<=max && num>=min) {
			value=num;
			return true;
		}
	}
	return false;
}

void MFDvarangle::inc_variable()
{
    if (IsAdjusterSpecialCase())
        return;
	double adjuster=GetAdjuster();
	double temp=value;
	temp+=adjuster*increment;
	if (temp>max)
	{
		if (loop)
			temp=min;
		else
			temp=max;
	}
	value=temp;
}

void MFDvarangle::dec_variable()
{
    if (IsAdjusterSpecialCase())
        return;
	double adjuster=GetAdjuster();
	double temp=value;
	temp-=adjuster*increment;
	if (temp<min)
	{
		if (loop)
			temp=max;
		else
			temp=min;
	}
	value=temp;
}


double MFDvarangle::getsin() const
{
	return sin(value);
}

double MFDvarangle::getcos() const
{
	return cos(value);
}

