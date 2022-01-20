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
#include "parser.h"
#include "mapfunction.h"
#include "shiplist.h"

class shipptrs* shipptrs::first=NULL;
class shipptrs* shipptrs::current=NULL;
bool shipptrs::saved=false;

shipptrs::shipptrs()
{
	OBJHANDLE hcraft=oapiGetFocusObject();//Sets up new shipptrs for focus object
	ZeroMemory(shipname, SHIPNAME_LENGTH);
	oapiGetObjectName(hcraft,shipname,SHIPNAME_LENGTH - 1); // Why is this -1?
	subcreate();
	state=new transxstate(hcraft,this);//A new plan base for this vessel
}

shipptrs::shipptrs(OBJHANDLE hcraft)
{
	ZeroMemory(shipname, SHIPNAME_LENGTH);
	oapiGetObjectName(hcraft,shipname,SHIPNAME_LENGTH - 1);
	state=new transxstate(hcraft,this);//A new plan base for this vessel
	subcreate();
}

void shipptrs::subcreate()
{
	previous=NULL;
	for (int a=0;a<MFDLIST_LENGTH;a++)
		mfdlist[a]=NULL;//No views yet
	next=first;
	first=this;
	if (next!=NULL)next->previous=this;
}

shipptrs::~shipptrs()
{
	for (int a=0;a<MFDLIST_LENGTH;a++)
		delete mfdlist[a];
	delete state;
	if (first==this) first=next;
	if (next!=NULL) next->previous=previous;
	if (previous!=NULL) previous->next=next;
}

void shipptrs::backgroundaction()
{
	class mapfunction *map=mapfunction::getthemap();
	if (!map->getinitialised()) return;
	if (current==NULL)
	{
		current=first;
		if (current==NULL) return;
	}
	current->state->checkbasefunction();
	current=current->next;
}

class shipptrs *shipptrs::findship(OBJHANDLE hcraft)
{
	char tname[30];
	oapiGetObjectName(hcraft,tname,30);
	return findship(tname);
}

class shipptrs *shipptrs::findship(char *tname)
{
	if (first==NULL) return NULL;
	class shipptrs *ptr=first;
	do
	{
		if (strcmp(ptr->shipname,tname)==0) return ptr;
		ptr=ptr->next;
	}
	while (ptr!=NULL);
	return NULL;
}

void shipptrs::saveallships(FILEHANDLE scn)
{
	if (saved) return;
	class shipptrs *current=first;
	while (current!=NULL)
	{
		current->savecurrent(scn);
		current=current->next;
	}
	saved=true;
}

void shipptrs::restoreallships(FILEHANDLE scn)
{
	char *buffer,*member;
	Parser parser;
	int length;
	while (oapiReadScenario_nextline(scn,buffer))
	{
		parser.parseline(buffer);
		bool ok=parser.getlineelement(0,&member,&length);
		if (!ok) return;
		if (strcmp("Ship",member)==0)
		{
			if (!parser.getlineelement(1,&member,&length)) return;//return if ship label doesn't exist
			if (length>30) return;//return if ship label is bad
			class shipptrs *temp=findship(member);
			if (temp==NULL)
			{
				OBJHANDLE temphandle=oapiGetVesselByName(member);
				if (temphandle!=NULL) temp=new shipptrs(temphandle);
			}
			if (temp!=NULL) temp->restorecurrent(scn);
		}
	}
}


void shipptrs::savecurrent(FILEHANDLE scn)
{
	oapiWriteScenario_string(scn,"Ship ",shipname);
	state->savecurrent(scn);
}

void shipptrs::restorecurrent(FILEHANDLE scn)
{
	state->restoresave(scn);
}

class shipptrs *shipptrs::getshipptrs()
{
	OBJHANDLE hcraft=oapiGetFocusObject();
	class shipptrs *ptr=findship(hcraft);
	if (ptr==NULL)
		ptr=new shipptrs();
	return ptr;
}

void shipptrs::destroyshipptrs()
{
	current=NULL;
	while (first!=NULL)
	{
		class shipptrs *temp=first;
		delete temp;//Destructor modifies first
	}
	//Also destroys the map
	class mapfunction *map=mapfunction::getthemap();
	if (map!=NULL)
		delete map;
}

void shipptrs::downshift()
{
	for (int a=0; a<MFDLIST_LENGTH; a++)
		if (mfdlist[a]!=NULL) mfdlist[a]->selfdownshift();
}

void shipptrs::resetshift()
{
	for (int a=0;a<MFDLIST_LENGTH;a++)
		if (mfdlist[a]!=NULL) mfdlist[a]->resetshift();
}


class viewstate *shipptrs::getviewstate(int mfdpos,class TransxMFD *mfdptr)
{
	if (mfdpos<0 || mfdpos>MFDLIST_LENGTH - 1) mfdpos=0;
	if (mfdlist[mfdpos]==NULL)
		mfdlist[mfdpos]=new viewstate(mfdpos,this);
	return mfdlist[mfdpos];
}

class shipptrs* shipptr_itr::getnext()
{
	if (current!=NULL) current=current->next;
	return current;
}

class shipptrs* shipptr_itr::getprev()
{
	if (current!=NULL) current=current->previous;
	return current;
}
