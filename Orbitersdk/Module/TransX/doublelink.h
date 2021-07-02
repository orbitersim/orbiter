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

#ifndef __DBLLIST_H
#define __DBLLIST_H

class dbllist
{
	class listelement *first,*last;
	class dblliter *firstiter;
	void ivegone(listelement *ptr);//informs the dbllist that an element has been removed so it can inform iterators
public:
	void addfront(class listelement *element);
	void addend(class listelement *element);
	class listelement *isinlist(listelement *element);//slow way to check if element is in list
	class dblliter *getiterator();
	void empty();//empties the list
	~dbllist();//deletes all elements if list is classified as managed
	dbllist(){first=last=NULL;firstiter=NULL;};
	friend class listelement;
	friend class dblliter;
};

class listelement
{
	class listelement *previous,*next;
	class dbllist *head;
	bool managethis;//If true, is deleted if list is deleted.
public:
	listelement(bool manageme = false){managethis=manageme;previous=next=NULL;head=NULL;};
	void uncouple();//Takes element out of its current list
	virtual ~listelement();
	friend class dbllist;
	friend class dblliter;
};



class dblliter
{
	dblliter(class dbllist *thelist);//{mylist=thelist;curpos=mylist->first;curpos=NULL;};
	class dblliter *previter,*nextiter;
	class listelement *curpos;
	class dbllist *mylist;
public:
	class listelement *front(){curpos=mylist->first;return curpos;};
	~dblliter();
	class listelement *current(){return curpos;};
	class listelement *back(){curpos=mylist->last;return curpos;};
	class listelement *next(){if (curpos==NULL) return curpos=mylist->first;curpos=curpos->next;return curpos;};
	class listelement *previous(){if (curpos==NULL) return curpos=mylist->last;curpos=curpos->previous;return curpos;};
	friend class dbllist;
};


#endif