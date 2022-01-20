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
#include "intercept.h"

Intercept::Intercept()
{
	iceptmethod=1;
	newintercept=true;
	gain=1;
	lasttimecorrection=0;
	fullorbits=halforbits=-1;
	shouldUpdateBarycenter = true;
}

void Intercept::resetintercept()
{
	iceptmethod=1;
	newintercept=true;
	fullorbits=halforbits=-1;
}

void Intercept::adjustorbitsdown()
{
	if (halforbits>0)
	{
		halforbits=0;
	}
	else
	{
		if (fullorbits>0)
		{
			fullorbits--;
			halforbits=1;
		}
	}
}

void Intercept::improveinterceptstraightline(const OrbitElements &craft, const OrbitElements &target)
{
	double craftorbittime;
	if (craft.geteccentricity()<1)
	{
		craftorbittime=craft.gettimeorbit();
	}
	else
	{
		craftorbittime=0;
		fullorbits=halforbits=0;
	}
	double timeoffset=craft.gettimestamp()-target.gettimestamp();
	newintercept=false;
	double craftcosthi,craftsinthi,targetcosthi,targetsinthi;
	craft.vectortothi(iceptalpha,&craftcosthi,&craftsinthi);
	target.vectortothi(iceptbeta,&targetcosthi,&targetsinthi);
	double crafttimeest=craft.GetTimeToThi(craftcosthi,craftsinthi,fullorbits,halforbits)+timeoffset;
	if (crafttimeest-icepttimeoffset>craftorbittime/4)
	{//You've just hit a border point on the orbit system
		adjustorbitsdown();
		crafttimeest=craft.GetTimeToThi(craftcosthi,craftsinthi,fullorbits,halforbits)+timeoffset;
	}
	double targettimeest=target.GetTimeToThi(targetcosthi,targetsinthi);
	double orbittime;
	double orbitnumber;
	int temp;
	target.gettimeorbit(&temp,&orbittime,targettimeest);
	double diff=crafttimeest-targettimeest;
	orbitnumber=floor(diff/orbittime+0.5);
	diff-=orbitnumber*orbittime;
	//
	//Now correcting the costhi of the craft to account for the time error
	//
	double craftradius=craft.thitoradius(craftcosthi);
	double craftangle=acos(craftcosthi);
	if (craftsinthi<0) craftangle=-craftangle;
	craftangle+=(-diff)*sqrt(craft.getangmomentum2())/(craftradius*craftradius);

	//Get time corrected cos and sin of thi
	craftcosthi=cos(craftangle);
	craftsinthi=sin(craftangle);
	// Get vectors for angles now that time error has been reduced
	craft.thitovectors(craftcosthi,craftsinthi,&icraftpos,&icraftvel);
	target.thitovectors(targetcosthi,targetsinthi,&itargetpos,&itargetvel);
	OrbitElements *targetaboutbarycentreorbit = target.getminorbarycentricorbit();

	// modify orbit if it target is based around the barycentre.
	if(targetaboutbarycentreorbit != NULL && shouldUpdateBarycenter)
	{
	VECTOR3 pos, vel;
	targetaboutbarycentreorbit->timetovectors(diff, &pos, &vel);
	if(pos.x == pos.x)
		itargetpos += pos;
	if(vel.x == vel.x)
		itargetvel += vel;
	}

	//Find the (Now reduced) time error again
	crafttimeest=craft.GetTimeToThi(craftcosthi,craftsinthi,fullorbits,halforbits)+timeoffset;
	if (crafttimeest-icepttimeoffset>craftorbittime/4)
	{//You've just hit a border point on the orbit system
		adjustorbitsdown();//This
		crafttimeest=craft.GetTimeToThi(craftcosthi,craftsinthi,fullorbits,halforbits)+timeoffset;
	}
	//targettimeest=target.GetTimeToThi(targetcosthi,targetsinthi);
	diff=crafttimeest-targettimeest;

	orbitnumber=floor(diff/orbittime+0.5);
	diff-=orbitnumber*orbittime;
	//Remove this error linearly from the craft position vector
	icraftpos=icraftpos+icraftvel*(-diff);

	//Find the time to closest approach from here
	//We now have craft and target position and velocity
	//Now use dotp to estimate remaining course correction
	VECTOR3 relcraftpos=icraftpos-itargetpos;
	VECTOR3 relcraftvel=icraftvel-itargetvel;
	double timecorrection=-dotp(relcraftpos,relcraftvel)/dotp(relcraftvel,relcraftvel);
	if (timecorrection*lasttimecorrection<0 && fabs(timecorrection/orbittime)>0.0001)
	{//Oscillatory
		gain=gain*0.5;
	}
	else
	{
		gain=gain*1.1;
		if (gain>1) gain=1;
	}
	lasttimecorrection=timecorrection;
	//Now calculate new vectors from the time correction
	icraftpos=icraftpos+icraftvel*timecorrection*gain;
	itargetpos=itargetpos+itargetvel*timecorrection*gain;
	relcraftpos=icraftpos-itargetpos;
	iceptalpha=icraftpos;
	iceptbeta=itargetpos;

	icepttimeoffset=crafttimeest+timecorrection*gain;
	itimeintercept=icepttimeoffset+target.gettimestamp();
	if (length(relcraftpos)*3>length(icraftpos))//Allows method to switch back if solution is no longer good
	{
		iceptmethod=1;
	}
	return;
}

void Intercept::getorbitsoffset(int *ifullorbits,int *ihalforbits) const
{
	*ifullorbits=fullorbits;
	*ihalforbits=halforbits;
}

void Intercept::updateintercept(const OrbitElements &craft, const OrbitElements &target,double craftorbitsahead)
// Updates the intercept structure holding closest approach between 'craft' and target. 'craft' may be hypothetical or actual
// Only one orbit may be hyperbolic
//To be broken up into submethods next time it's revised
{
	if (!craft.isvalid() || !target.isvalid()) return;//Ensure no void updates!
	double timeoffset=craft.gettimestamp()-target.gettimestamp();
	iplanecept=crossp(craft.getplanevector(),target.getplanevector());
	if (iceptmethod==2)
	{
		improveinterceptstraightline(craft,target);
		return;
	}

	fullorbits=halforbits=-1;//This is not provided in this case

	//Our task now is to come up with an initial guess for the intercept that works

	//First option is to check to see whether the plane intercept vector is a good first guess.
	//

	const OrbitElements *alpha, *beta;
	//The inversion functionality no longer matters - timetovectors is now good enough to avoid the problem

	alpha=&craft;
	beta=&target;

	double betacos=beta->getcurrcosthi();
	double alphacos=alpha->getcurrcosthi();
	double alphasin;
	if (newintercept || iceptradius != iceptradius)
	{
		iceptradius=beta->getcurrradius();
		newintercept=false;
	}
	double term1=alpha->getangmomentum2()/(alpha->getgmplanet()*alpha->geteccentricity());
	double term2=1/alpha->geteccentricity();
	double timea, timeb;
	VECTOR3 alphaposa, alphaposb, alphavela,alphavelb, betaposa, betaposb, betavela, betavelb;
	alphacos=term1/iceptradius-term2; //This gives the cos of the angle - can be (and frequently is) beyond range of cosine
	double tradius=iceptradius;
	if (alphacos>1) //Make angle legal !
	{
		alphacos=1;
		tradius=fabs(alpha->getangmomentum2()/(alpha->getgmplanet()*(1-alpha->geteccentricity())));
	}
	if (alphacos<-1)
	{
		alphacos=-1;
		tradius=fabs(alpha->getangmomentum2()/(alpha->getgmplanet()*(alpha->geteccentricity()+1)));
	}
	int full=int(floor(craftorbitsahead));
	int half=int((craftorbitsahead-full)*2);
	alpha->GetTimesToThi(alphacos, &timea, &timeb,full,half);//times are found so that you can tell where beta is
	timea+=timeoffset;
	timeb+=timeoffset;
	alphasin=1-alphacos*alphacos;
	if (alphasin>0)
	{
		alphasin=sqrt(alphasin);
	}
	else
	{
		alphasin=0;
	}
	alpha->thitovectors(alphacos,alphasin,&alphaposa,&alphavela);
	alpha->thitovectors(alphacos,-alphasin,&alphaposb,&alphavelb);
	if (tradius<alpha->getcurrradius() || alpha->geteccentricity()>1)
	{
		VECTOR3 temp=alphaposa;
		alphaposa=alphaposb;
		alphasin=-alphasin;
		alphaposb=temp;
	}
	// Pos now guaranteed to match

	//New system
	beta->timetovectors(timea,&alphatime);
	beta->timetovectors(timeb,&betatime);
	alphatime.getposvel(&betaposa,&betavela);
	betatime.getposvel(&betaposb,&betavelb);

	bool abetter=(length2my(betaposa-alphaposa)<length2my(betaposb-alphaposb)); //Picks best on grounds of distance
	if (timea<timeoffset)
	{
		if (craftorbitsahead<0.3)
		{
			abetter=false;//Invalidates first solution if in the past, if not looking ahead
		}
		else
		{
			timea+=alpha->gettimeorbit();
		}
	}
	if (abetter)
	{
		//a is better
		iceptradius=(length(betaposa)+iceptradius)/2;// Takes average of new and old as this converges better
		itimeintercept=timea;
		if (length2my(betaposa-alphaposa)*9<length2my(alphaposa))
		{
			iceptmethod=2;
			icepttimeoffset=timea;
			iceptalpha=alphaposa;
			iceptbeta=betaposa;
			fullorbits=full;
			halforbits=half;
		}
	}
	else
	{
		//b is better
		iceptradius=(length(betaposb)+iceptradius)/2;
		itimeintercept=timeb;
		if (length2my(betaposb-alphaposb)*9<length2my(alphaposb))
		{
			iceptmethod=2;
			icepttimeoffset=timeb;
			iceptalpha=alphaposb;
			iceptbeta=betaposb;
			fullorbits=full;
			halforbits=half;
		}
	}
	if (abetter)
	{
		icraftpos=alphaposa;
		icraftvel=alphavela;
		itargetpos=betaposa;
		itargetvel=betavela;
	}
	else
	{
		icraftpos=alphaposb;
		icraftvel=alphavelb;
		itargetpos=betaposb;
		itargetvel=betavelb;
	}
	itimeintercept+=target.gettimestamp();
}



void Intercept::getplanecept(VECTOR3 *planecept) const
{
	*planecept=iplanecept;
}

void Intercept::getpositions(VECTOR3 *craftpos, VECTOR3 *targetpos) const
{
	*craftpos=icraftpos;
	*targetpos=itargetpos;
}

void Intercept::getrelpos(VECTOR3 *relpos) const
{
	*relpos=icraftpos-itargetpos;
}

void Intercept::getrelvel(VECTOR3 *relvel) const
{
	*relvel=icraftvel-itargetvel;
}

void Intercept::getvelocities(VECTOR3 *craftvel, VECTOR3 *targetvel) const
{
	*craftvel=icraftvel;
	*targetvel=itargetvel;
}

double Intercept::gettimeintercept() const
{
	return itimeintercept;
}

