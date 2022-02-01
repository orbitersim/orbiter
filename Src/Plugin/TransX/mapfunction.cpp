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
#include "mapfunction.h"

using namespace std;

mapfunction *mapfunction::themap=NULL;

mapfunction::~mapfunction()
{
	themap=NULL;
}

mapfunction::mapfunction()
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

void mapfunction::dolowpriaction()
{//Function is called multiple times - addaction(0) prompts it to be recalled
	//System allows large initialisation computation to be broken up into parts
	// and staged through multiple timesteps
	InitialiseSolarSystem();
	initialised=true;
}

void mapfunction::InitialiseSolarSystem()
{
    m_bodyProvider.InitialiseSolarSystem();
}

OBJHANDLE mapfunction::getcurrbody(OBJHANDLE vessel)//Finds current body for current focus vessel
{
	if (!initialised || vessel==NULL) return NULL;
	VECTOR3 currentpos,bodypos,parentpos,relvector;
	double distance2,bodyfromparent2;
	oapiGetGlobalPos(vessel,&currentpos);
	GBODY *currentsoi=NULL;
	GBODY *body = m_bodyProvider.GetSun();
	while(body)
	{//Recomputes distance as time may have passed since initialisation
		oapiGetGlobalPos(body->bodyhandle,&bodypos);
		if(body->parent == NULL)
			parentpos = bodypos; // The sun
		else
			oapiGetGlobalPos(body->parent->bodyhandle,&parentpos);
		relvector=parentpos-bodypos;
		bodyfromparent2=length2my(relvector);
		relvector=currentpos-bodypos;
		distance2=length2my(relvector);
		if (distance2<body->gravbodyratio2*bodyfromparent2 || body == m_bodyProvider.GetSun())//In this soi
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
    if (cacheSOISize.NeedsUpdate(handle))
        cacheSOISize = BodyCache(handle, m_bodyProvider.GetBody(handle));
    GBODY *body = cacheSOISize.Gbody();
	double radius;
	if(!body)
		return 0;	// probably a craft or something similar.
	if (body->parent == NULL)
		return 1e80;//virtually infinite SOI for largest body
	else
	{
		OBJHANDLE parent = body->parent->bodyhandle;
		VECTOR3 vecradius;

		oapiGetRelativePos(parent,handle,&vecradius);
		radius=sqrt(dotp(vecradius,vecradius)*body->gravbodyratio2);
	}
	return radius;
}


OBJHANDLE mapfunction::getfirstmoon(OBJHANDLE handle)
{
    if (cacheFistsMoon.NeedsUpdate(handle))
        cacheFistsMoon = BodyCache(handle, m_bodyProvider.GetBody(handle));
    GBODY *body = cacheFistsMoon.Gbody();

	if(body)
		if(body->satellites.size() > 0)
			return body->satellites.front()->bodyhandle;
	return NULL;
}

OBJHANDLE mapfunction::getlastmoon(OBJHANDLE handle)
{
    if (cacheLastMoon.NeedsUpdate(handle))
        cacheLastMoon = BodyCache(handle, m_bodyProvider.GetBody(handle));
    GBODY *body = cacheLastMoon.Gbody();

	if(body)
		if(body->satellites.size() > 0)
			return body->satellites.back()->bodyhandle;
	return NULL;
}

OBJHANDLE mapfunction::getnextpeer(OBJHANDLE handle)
{
    if (cacheNextPeer.NeedsUpdate(handle))
        cacheNextPeer = BodyCache(handle, m_bodyProvider.GetBody(handle));
    GBODY *body = cacheNextPeer.Gbody();

	if(body)
		if(body->next)
			return body->next->bodyhandle;
	return NULL;
}

OBJHANDLE mapfunction::getpreviouspeer(OBJHANDLE handle)
{
    if (cachePreviousPeer.NeedsUpdate(handle))
        cachePreviousPeer = BodyCache(handle, m_bodyProvider.GetBody(handle));
    GBODY *body = cachePreviousPeer.Gbody();

	if(body)
		if(body->previous)
			return body->previous->bodyhandle;
	return NULL;
}


OBJHANDLE mapfunction::getmajor(OBJHANDLE handle)
{
    if (cacheMajor.NeedsUpdate(handle))
        cacheMajor = BodyCache(handle, m_bodyProvider.GetBody(handle));
    GBODY *body = cacheMajor.Gbody();

	if(body)
		if(body->parent)
			return body->parent->bodyhandle;
	return NULL;
}

VECTOR3 mapfunction::getweightedvector(OBJHANDLE body, void (*func)(OBJHANDLE, VECTOR3*))
{
	OBJHANDLE moon = getfirstmoon(body);
	double totalmass = oapiGetMass(body);
	VECTOR3 barycentre = {0,0,0}, bodypos;
	func(body, &bodypos);
	barycentre.x += bodypos.x * totalmass;
	barycentre.y += bodypos.y * totalmass;
	barycentre.z += bodypos.z * totalmass;

	while(moon)
	{
		func(moon, &bodypos);
		double bodymass = oapiGetMass(moon);
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
