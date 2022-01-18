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
#include "doublelink.h"


void dbllist::addfront(listelement *element)
{
	if (element==NULL) return;
	element->uncouple();
	if (first!=NULL)
	{
		first->previous=element;
	}
	if (last==NULL) last=element;
	element->next=first;
	element->previous=NULL;
	first=element;
	element->head=this;
}

class dblliter *dbllist::getiterator()
{
	return new dblliter(this);
}

class listelement *dbllist::isinlist(listelement *element)
{
	class dblliter it(this);
	class listelement *current=it.front();
	while (current!=NULL)
	{
		if (current==element) return element;
		current=it.next();
	}
	return NULL;
}

void dbllist::addend(listelement *element)
{
	if (element==NULL) return;
	element->uncouple();
	if (last!=NULL)
	{
		last->next=element;
	}
	if (first==NULL) first=element;
	element->previous=last;
	element->next=NULL;
	last=element;
	element->head=this;
}

void dbllist::empty()
{
	listelement *currelement,*nextelement;
	currelement=first;
	do
	{
		if (currelement!=NULL)
		{
			nextelement=currelement->next;
			if (currelement->managethis) delete currelement;//Only deletes it if element has been created appropriately
		}
		else
		{
			nextelement=NULL;
		}
		currelement=nextelement;
	}
	while (currelement!=NULL);
}

dblliter::dblliter(class dbllist *thelist)
{
	mylist=thelist;
	curpos=mylist->first;
	curpos=NULL;
	nextiter=thelist->firstiter;
	previter=NULL;
	if (nextiter!=NULL)nextiter->previter=this;
	thelist->firstiter=this;
}

dblliter::~dblliter()
{
	//Take yourself out of the iterator list
	if (nextiter!=NULL)nextiter->previter=previter;
	if (previter!=NULL)previter->nextiter=nextiter;
	if (mylist->firstiter==this) mylist->firstiter=nextiter;
}

dbllist::~dbllist()
{
	empty();
}

void dbllist::ivegone(listelement *ptr)
{
	dblliter *iter=firstiter;
	while (iter!=NULL)
	{
		if (iter->curpos==ptr)iter->curpos=NULL;
		iter=iter->nextiter;
	}
}


void listelement::uncouple()
{
	if (previous!=NULL)
	{
		previous->next=next;
	}
	if (next!=NULL)
	{
		next->previous=previous;
	}
	if (head!=NULL)
	{
		if (head->first==this) head->first=next;
		if (head->last==this) head->last=previous;
		head->ivegone(this);
	}
	//Make sure any iterators that are pointing to me are informed
}

listelement::~listelement()
{
	uncouple();
}
