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
#include <stdio.h>
#include <math.h>
#include "orbitersdk.h"
#include "mfd.h"
#include "mapfunction.h"

mapfunction *mapfunction::themap=NULL;

mapfunction::~mapfunction()
{
	DeleteGBody(sun); // deallocate memory recursively
	themap=NULL;
}

mapfunction::mapfunction() : 
sun(NULL)
{
	initialised=false;
	addaction(0);//Place single low priority action into queue
}

mapfunction *mapfunction::getthemap()
{
	if (themap==NULL)
		themap=new mapfunction();
	return themap;
}

void mapfunction::DeleteGBody(GBODY *body)
{
	if(body)
	{
		// recursively eletes the GBODY and the tree of satellites 
		list<GBODY*>::iterator it;
		for(it = body->satellites.begin(); it != body->satellites.end(); ++it)
			DeleteGBody(*it);
		body->satellites.clear();
		delete body;
	}
}

void mapfunction::dolowpriaction()
{//Function is called multiple times - addaction(0) prompts it to be recalled
	//System allows large initialisation computation to be broken up into parts
	// and staged through multiple timesteps
	InitialiseSolarSystem();
	initialised=true;
}

OBJHANDLE mapfunction::getcurrbody(OBJHANDLE vessel)//Finds current body for current focus vessel
{
	if (!initialised || vessel==NULL) return NULL;
	VECTOR3 currentpos,bodypos,parentpos,relvector;
	double distance2,bodyfromparent2;
	oapiGetGlobalPos(vessel,&currentpos);
	GBODY *currentsoi=NULL;
	GBODY *body = sun;
	while(body)
	{//Recomputes distance as time may have passed since initialisation
		oapiGetGlobalPos(body->bodyhandle,&bodypos);
		if(body->parent == NULL)
			parentpos = bodypos; // The sun
		else
			oapiGetGlobalPos(body->parent->bodyhandle,&parentpos);
		relvector=parentpos-bodypos;
		bodyfromparent2=length2(relvector);
		relvector=currentpos-bodypos;
		distance2=length2(relvector);
		if (distance2<body->gravbodyratio2*bodyfromparent2 || body == sun)//In this soi
		{
			currentsoi=body;
			if(body->satellites.size() > 0)
				body = body->satellites.front();
			else
				body = NULL;
		}
		else
		{
			body = body->next;//Move to next possible object
		}
	}
	//We now have the actual body for this object
	return currentsoi->bodyhandle;
}
	
double mapfunction::getsoisize(OBJHANDLE handle)
{
	double radius;
	if(!bodyMap[handle])
		return 0;	// probably a craft or something similar.
	if (bodyMap[handle]->parent == NULL)
		return 1e80;//virtually infinite SOI for largest body
	else
	{
		GBODY *body = bodyMap[handle];
		OBJHANDLE parent = body->parent->bodyhandle;
		VECTOR3 vecradius;
		
		oapiGetRelativePos(parent,handle,&vecradius);
		radius=sqrt(dotp(vecradius,vecradius)*body->gravbodyratio2);
	}
	return radius;
}


OBJHANDLE mapfunction::getfirstmoon(OBJHANDLE handle)
{
	GBODY* body = bodyMap[handle];
	if(body)
		if(body->satellites.size() > 0)
			return body->satellites.front()->bodyhandle;
	return NULL;
}

OBJHANDLE mapfunction::getlastmoon(OBJHANDLE handle)
{
	GBODY* body = bodyMap[handle];
	if(body)
		if(body->satellites.size() > 0)
			return body->satellites.back()->bodyhandle;
	return NULL;
}

OBJHANDLE mapfunction::getnextpeer(OBJHANDLE handle)
{
	if(bodyMap[handle])
		if(bodyMap[handle]->next)
			return bodyMap[handle]->next->bodyhandle;
	return NULL;
}

OBJHANDLE mapfunction::getpreviouspeer(OBJHANDLE handle)
{	
	if(bodyMap[handle])
		if(bodyMap[handle]->previous)
			return bodyMap[handle]->previous->bodyhandle;
	return NULL;
}


OBJHANDLE mapfunction::getmajor(OBJHANDLE handle)
{
	if(bodyMap[handle])
		if(bodyMap[handle]->parent)
			return bodyMap[handle]->parent->bodyhandle;
	return NULL;
}

VECTOR3 mapfunction::getweightedvector(OBJHANDLE body, void (*func)(OBJHANDLE, VECTOR3*))
{
	OBJHANDLE moon = getfirstmoon(body);
	double totalmass = oapiGetMass(body), bodymass;
	VECTOR3 barycentre = {0,0,0}, bodypos;
	func(body, &bodypos);
	barycentre.x += bodypos.x * totalmass;
	barycentre.y += bodypos.y * totalmass;
	barycentre.z += bodypos.z * totalmass;

	while(moon)
	{
		func(moon, &bodypos);
		bodymass = oapiGetMass(moon);
		barycentre.x += bodypos.x * bodymass;
		barycentre.y += bodypos.y * bodymass;
		barycentre.z += bodypos.z * bodymass;
		totalmass += bodymass;
		
		moon = getnextpeer(moon);
	}

	barycentre.x /= totalmass;
	barycentre.y /= totalmass;
	barycentre.z /= totalmass;

	return barycentre;
}

VECTOR3 mapfunction::getbarycentrevel(OBJHANDLE body)
{
	return getweightedvector(body, &oapiGetGlobalVel);
}

VECTOR3 mapfunction::getbarycentre(OBJHANDLE body)
{
	return getweightedvector(body, &oapiGetGlobalPos);
}

void mapfunction::InitialiseSolarSystem()
{
	int totalbodies=oapiGetGbodyCount();
	list<GBODY*> templist; // used for easy accesss to the bodies rather than traversing the tree
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
			list<GBODY*>::iterator it = templist.begin();
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

double mapfunction::GetApproxAtmosphericLimit(OBJHANDLE body)
{
	static map<OBJHANDLE, double> atmLimit;
	if(!oapiPlanetHasAtmosphere(body))
		return 0;
	if(atmLimit[body] != 0)
		return atmLimit[body];	// return the limit if we have already found it
	
	// Perform a binary search (trial and error) to see find the altitude for a given static pressure
	double alt = oapiGetPlanetAtmConstants(body)->radlimit;
	double step = oapiGetPlanetAtmConstants(body)->radlimit / 2;

	// Search for an altitude that matches the magic value (that corresponds to ~150km altitude for Earth
	const double PRESSURE = 0.000557;	// in Pa - the target pressure that we want to get an altitude for
	const double TOLERANCE = 0.000001;	// in Pa - used for the trial-and-error method to detect when we're 'close enough'
	ATMPARAM prm;
	prm.p = 0;	// Initialise to be zero at limit of atmosphere
	do
	{
		if(prm.p > PRESSURE)	// pressure is too high at our current location, raise altitude to lower pressure
			alt += step;
		else					// pressure is too low at our current location, lower altitude to raise pressure
			alt -= step;
		step /= 2;		// halve the search step
		oapiGetPlanetAtmParams(body, alt, 0, 0, &prm);	// get the pressure at the equator on the meridian as it's only rough anyway
	}
	while(fabs(prm.p - PRESSURE) > TOLERANCE && alt >= 1 && step >= 1);
	return atmLimit[body] = alt;
}