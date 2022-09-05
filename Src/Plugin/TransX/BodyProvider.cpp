/* Copyright © 2007-9 Steve Arch, Duncan Sharpe
** Copyright © 2011 atomicdryad - 'ENT' button & Pen allocation fix
** Copyright © 2013 Dimitris Gatsoulis (dgatsoulis) - Hacks
** Copyright © 2013 Szymon Ender (Enjo) - Auto-Min™, Auto-Center™ & other hacks
**
** X11 License ("MIT License")
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

#include "BodyProvider.h"
using namespace std;

BodyProvider::BodyProvider()
: sun(NULL)
, bodyCached(NULL)
, handleCached(NULL)
{
}

BodyProvider::~BodyProvider()
{
    DeleteGBody(sun); // deallocate memory recursively
}

GBODY * BodyProvider::GetSun()
{
    return sun;
}

GBODY * BodyProvider::GetBody(OBJHANDLE handle)
{
    if (handleCached != handle)
    {
        handleCached = handle;
        bodyCached = bodyMap[handle];
    }
    return bodyCached;
}

void BodyProvider::InitialiseSolarSystem()
{
	int totalbodies=oapiGetGbodyCount();
	vector<GBODY*> templist; // used for easy accesss to the bodies rather than traversing the tree
	for (int i = 0; i < totalbodies; i++)
	{
		GBODY *body = new GBODY;
		body->next = body->previous = NULL;

		body->bodyhandle = oapiGetGbodyByIndex(i);
		body->mass = oapiGetMass(body->bodyhandle);
		bodyMap[body->bodyhandle] = body;
		if(i == 0)
		{
			// The central star
			body->soisize2 = 1e100;//Infinite SOI
			body->parent = body->next = body->previous = NULL;//Sun is its own parent!
			sun = body;
		}
		else
		{
			// gravbodyratio2 and soisize
			// Find the parent body (what it is orbiting)
			vector<GBODY*>::iterator it = templist.begin();
			GBODY* currparent;
			double currdistance2;
			while(it != templist.end())
			{
				char itname[30], name[30];
				oapiGetObjectName((*it)->bodyhandle, itname, 30);
				oapiGetObjectName(body->bodyhandle, name, 30);

				VECTOR3 pos;
				oapiGetRelativePos(body->bodyhandle, (*it)->bodyhandle, &pos);
				double distance2 = dotp(pos, pos);
				if(distance2 < (*it)->soisize2)
				{
					currparent = *it;
					currdistance2 = distance2;
				}

				it++;
			}
			body->parent = currparent;
			if(body->parent->satellites.size() > 0)
			{
				body->previous = body->parent->satellites.back();
				body->previous->next = body;
			}
			body->parent->satellites.push_back(body);
			body->gravbodyratio2 = pow(body->mass / body->parent->mass, 0.8);
			body->soisize2 = currdistance2 * body->gravbodyratio2;//Internal soi size
		}
		templist.push_back(body); // add it to the temp list
	}
}

void BodyProvider::DeleteGBody(GBODY *body)
{
	if(body)
	{
		// recursively eletes the GBODY and the tree of satellites
		vector<GBODY*>::iterator it;
		for(it = body->satellites.begin(); it != body->satellites.end(); ++it)
			DeleteGBody(*it);
		body->satellites.clear();
		delete body;
	}
}
