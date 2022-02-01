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
#include "mapfunction.h"
#include "mfd.h"
#include "graph.h"
#include "transxstate.h"
#include "basefunction.h"
#include "planfunction.h"
#include "TransXFunction.h"

bool minorejectplan::init(class MFDvarhandler *vars, class basefunction *base)
{
	double radius;
	radius=oapiGetSize(base->gethmajor());
	m_ped.init(vars,3,3,"Pe Distance", radius*1.2, 0, radius*1000, 0.05, 1000);
	m_ejorient.init(vars,"Ej Orientation", true);
	m_equatorial.init(vars,3,3,"Equatorial view",0,1,"No","Yes","","","");
	ibase=base;
	m_ped.sethelpstrings(
		"Periapsis of planned orbit",
		"(Distance from planet centre)");
	m_ejorient.sethelpstrings(
		"Use to rotate planned orbit around",
		"axis of departure direction.");
	base->sethelp(
		"Used to leave SOI of this planet/moon",
		"for another. eg Earth to",
		"Mars. Best to plan before you take",
		"off. Most efficient way to plan",
		"");
	return true;
}

bool encounterplan::init(class MFDvarhandler *vars, class basefunction *base)
{
	m_drawbase.init(vars,3,3,"Draw Base",0,1,"Yes","No","","","");
	m_drawbase.sethelpstrings(
		"Choose to not draw the base","");
	base->sethelp(
		"Used to show information on the",
		"arrival planet. No function other than",
		"to show extra information on a planet",
		"including a surface base","");
	return true;
}

bool slingshot::init(class MFDvarhandler *vars, class basefunction *base)
{
	m_selectorbit.init(vars,3,3,"View Orbit",0,1,"Approach","Depart","","","");
	goodness=0;
	m_selectorbit.sethelpstrings(
		"Choose to view approach",
		"or departure orbit");
	return true;
}

void slingshot::calculate(class MFDvarhandler *vars,basefunction *base)

{//Get vectors from both functions
	basefunction *previous, *next;
	previous=base->getpreviousfunc();
	next=base->getnextfunc();
	if (next==NULL) return;//No way to know where slingshot should go, so stop here
	//First, get handed-on orbit from previous
	planorbit.setinvalid();

	const OrbitElements & craft=base->getcraftorbit();
	VECTOR3 inward={0,0,0};
	if (craft.isvalid())
		craft.getinfinityvelvector(false,&inward);
	mapfunction *map=mapfunction::getthemap();
	OBJHANDLE hmajor=base->gethmajor();

	double soisize=map->getsoisize(hmajor);
	double gmhmajor=oapiGetMass(hmajor)*GRAVITY;
	plan *nextplan;
	VECTOR3 ejectvector;
	ejectvelocity2=0;
	double ejecttime;
	if (next!=NULL)
	{
		nextplan=next->getplanpointer();
		if (nextplan!=NULL)
		{
			ejectvector=nextplan->getvelocityvector();
			if (craft.isvalid())
			{
				ejecttime=craft.getpedeltatime()+craft.gettimestamp();
				eventtime=ejecttime;//For any function that takes time from us
			}
			else
			{
				eventtime=0;
				ejecttime=nextplan->geteventtime();//Get from future variable if nothing else
			}
			ejectvelocity2=length2my(ejectvector);
		}
		else
			return;
	}
	else
	{
		nextplan=NULL;
		return;
	}
	if (!craft.isvalid()) return;
	inwardvelocity=length(inward);
	if (ejectvelocity2<1 || inwardvelocity<1)
		return;
	//We now have both vectors
	double sinfirst;
	if (periapsisguess<0 || periapsisguess != periapsisguess) // go into this section if NaN
	{//No guess before
		//get angle between the vectors
		double cos2a=cosangle(inward,ejectvector);
		sinfirst=sqrt((1-cos2a)/2);//complex trig argument leads to this
		//first cos2a=1-2sin^2 a
		//but this sin is - the cos we're looking for, when angles are done
		double ecc=1/sinfirst;
		periapsisguess=(ecc-1)/(inwardvelocity*inwardvelocity)*gmhmajor;//got a trial periapsis
	}
	else
	{
		//previous guess - try to improve on it!
		//calculate the current 'throw' angle
		double ecc,sinsecond,totalangle;
		double reqdangle=acos(cosangle(inward,ejectvector));
		do {
			ecc=periapsisguess*inwardvelocity*inwardvelocity/gmhmajor+1;
			sinfirst=1/ecc;
			ecc=periapsisguess*ejectvelocity2/gmhmajor+1;
			sinsecond=1/ecc;
			totalangle=asin(sinfirst)+asin(sinsecond);
			if (totalangle<PI/2)
				periapsisguess=periapsisguess*totalangle/reqdangle;
			else
				periapsisguess=periapsisguess*(PI-reqdangle)/(PI-totalangle);
			goodness=totalangle/reqdangle;
		}
		while (fabs(goodness-1)>1e-5);
	}
	//Calculate elements for required new orbit
	//set up coordinate system
	ratiotoradius=periapsisguess/oapiGetSize(hmajor);
	VECTOR3 asvel=inward*(1/inwardvelocity);
	VECTOR3 rightangletovel=unit(asvel*dotp(asvel,ejectvector)-ejectvector);
	double cosfirst=sqrt(1-sinfirst*sinfirst);
	VECTOR3 peposition=(rightangletovel*cosfirst+asvel*sinfirst)*periapsisguess;
	VECTOR3 pevel=rightangletovel*(-sinfirst)+asvel*cosfirst;
	if (m_selectorbit==0)
		pevel=pevel*sqrt(inwardvelocity*inwardvelocity+2*gmhmajor/periapsisguess);
	else
		pevel=pevel*sqrt(ejectvelocity2+2*gmhmajor/periapsisguess);

	planorbit.init(peposition,pevel,ejecttime,gmhmajor);
}


void minorejectplan::graphscale(Graph *graph)
{//Needs revising to remove references to focus vessel. OK for now.
	if (planorbit.isvalid())
		graph->setviewscale(planorbit);
	if (m_equatorial==0) return;
	VESSEL *vessel;
	vessel=oapiGetFocusInterface();
	VESSELSTATUS status;
	vessel->GetStatus(status);
	VECTOR3 svelocity,relvector,craftpos;
	OBJHANDLE hmajor=ibase->gethmajor();
	if (hmajor==vessel->GetSurfaceRef())
	{
		oapiGetFocusRelativeVel(hmajor, &svelocity); //This is the velocity under the ship
		oapiGetFocusShipAirspeedVector(&relvector);
		svelocity=svelocity-relvector; //This is the velocity of the surface
		svelocity=unit(svelocity); //unit to give vector parallel to rotation
		oapiGetFocusRelativePos(hmajor,&craftpos);
		VECTOR3 tradius=unit(craftpos);//Get unit radius vector
		VECTOR3 tvector=unit(crossp(tradius, svelocity));
		double longitude, latitude, tnumber;
		oapiGetFocusEquPos(&longitude, &latitude, &tnumber);//Only works if current planet's surface dominates
		double cosl=cos(-latitude);
		double sinl=sin(-latitude);
		VECTOR3 tzaxis=tradius*cosl-tvector*sinl;
		VECTOR3 tyaxis=tradius*sinl+tvector*cosl;
		graph->setprojection(svelocity, tyaxis,tzaxis);
	}
}


void slingshot::graphscale(Graph *graph)
{
	if (planorbit.isvalid())
		graph->setviewscale(planorbit);
}

bool minorejectplan::maingraph(oapi::Sketchpad *sketchpad,Graph *graph, basefunction *base)
{
	const OrbitElements & craft=base->getcraftorbit();
	if (craft.isvalid() && planorbit.isvalid())
	{
		// Draw intersect line
		base->SelectDefaultPen(sketchpad, TransXFunction::Grey);
		VECTOR3 intersect=planorbit.getintersectvector(craft);
		graph->drawvectorline(sketchpad,intersect);
		// Draw Ascending Node (filled)
		intersect = unit(intersect) * this->ibase->getcraftorbit().getapodistance(); // Put markers out of the way
		base->SelectBrush(sketchpad, TransXFunction::Grey);
		graph->drawmarker(sketchpad, -intersect, Graph::Circle);
		// Draw Descending Node (hollow)
		base->SelectBrush(sketchpad, TransXFunction::Hollow);
		graph->drawmarker(sketchpad, intersect, Graph::Circle);
	}
	return true;
}

bool slingshot::maingraph(oapi::Sketchpad *sketchpad,Graph *graph,basefunction *base)
{
	const OrbitElements & craft=base->getcraftorbit();
	OBJHANDLE hmajor=base->gethmajor();
	double planetsize=oapiGetSize(hmajor);
	if (!craft.isvalid()) return true;
	if (!planorbit.isvalid()) return true;//Get base to do a graph
	graph->setviewscale(0);
	graph->setviewscalesize(planetsize);
	VECTOR3 planpos,craftpos,velvector;
	planorbit.thitovectors(1,0,&planpos,&velvector);
	graph->setviewscalesize(length(planpos));
	craft.thitovectors(1,0,&craftpos,&velvector);
	graph->setviewscalesize(length(craftpos));
	craft.getinfinityvelvector(false,&velvector);//Now finally set for real
	graph->setprojection(velvector);

	oapi::Pen *pen=base->SelectDefaultPen(sketchpad,TransXFunction::PEN_ATMOSPHERE);
	graph->drawatmosphere(sketchpad,hmajor);
	pen=base->SelectDefaultPen(sketchpad,TransXFunction::Grey);
	graph->drawplanet(sketchpad,hmajor);
	pen=base->SelectDefaultPen(sketchpad,TransXFunction::Green);
	graph->drawvector(sketchpad,craftpos);
	pen=base->SelectDefaultPen(sketchpad,TransXFunction::Yellow);
	graph->drawvector(sketchpad,planpos);
	return false;//No need to draw any more graphs
}


void encounterplan::graphupdate(oapi::Sketchpad *sketchpad,Graph *graph,basefunction *base)
{
	drawnbase=false;
	oapi::Pen* pen=base->SelectDefaultPen(sketchpad,TransXFunction::Yellow);
	OBJHANDLE hvessel=base->gethcraft();
	VESSEL *curfocus=oapiGetVesselInterface(hvessel);
	VESSELSTATUS status;
	curfocus->GetStatus(status);
	OBJHANDLE surfbase=status.base;
	if (surfbase==NULL) return;
	OBJHANDLE hmaj=base->gethmajor();
	oapiGetRelativePos(surfbase,hmaj,&baseposition);
	double rot = oapiGetPlanetPeriod(hmaj);
	const OrbitElements & craft=base->getcraftorbit();

	// Get the base position at the Pe/impact by rotating the body by the time until Pe/impact
	double radius = oapiGetSize(hmaj);
	double deltatime;
	if(craft.getpedistance() > radius)
		deltatime = craft.getpedeltatime();	// Get time of Pe
	else
		deltatime = craft.GetTimeToRadius(radius, false); // Get time of impact (distance = radius of planet)
	MATRIX3 oblrot;
	oapiGetPlanetObliquityMatrix(hmaj, &oblrot);
	MATRIX3 invoblrot = getinvmatrix(oblrot);
	double theta = 2 * PI * deltatime / rot; // angle rotated by planet in 1 second
	MATRIX3 majrot = {cos(theta), 0, -sin(theta),
					  0,		  1, 0,
					  sin(theta), 0, cos(theta)};
	VECTOR3 v1 = mul(invoblrot, baseposition);
	VECTOR3 v2 = mul(majrot, v1);
	baseposition = mul(oblrot, v2);

	double distance2=length2my(baseposition);
	double radius2=radius*radius;
	if (radius2*1.5>distance2)
	{
		graph->drawvector(sketchpad,baseposition);
		drawnbase=true;
	}
}


void slingshot::graphupdate(oapi::Sketchpad *sketchpad, Graph *graph,basefunction *base)
{
	oapi::Pen* pen=base->SelectDefaultPen(sketchpad,TransXFunction::Yellow);
	planorbit.draworbit(sketchpad,graph,false);
	const OrbitElements & craft=base->getcraftorbit();
	if (!craft.isvalid()) return;
	const VECTOR3 & intersect=planorbit.getintersectvector(craft);
	pen=base->SelectDefaultPen(sketchpad,TransXFunction::Grey);
	graph->drawvectorline(sketchpad,intersect);
}

void minorejectplan::wordupdate(oapi::Sketchpad *sketchpad, int width, int height, basefunction *base)
{
	char buffer[20];
	int linespacing=height/24;
	int pos=16*linespacing;
	int len;
	const OrbitElements & craft=base->getcraftorbit();
	if (!craft.isvalid() || !planorbit.isvalid()) return;
	OBJHANDLE hcraft=base->gethcraft();
	VESSEL *pV=oapiGetVesselInterface(hcraft);
	VESSELSTATUS status;
	pV->GetStatus(status);
	double angle=180/PI*acos(cosangle(planorbit.getplanevector(),craft.getplanevector()));
	VECTOR3 tpos,tvel;
	tpos=status.rpos;
	tvel=status.rvel;

	// Get the target inclination (absolute wrt global coordinates)
	VECTOR3 down = {0, -1, 0};
	double targetInc = 180/PI*acos(cosangle(down, planorbit.getplanevector()));
	sprintf(buffer, "Incl.  :%.4g°", targetInc);
	sketchpad->Text( 0, pos, buffer, strlen(buffer));
	pos += linespacing;

	// Get the target LAN (absolute wrt global coordinates)
	VECTOR3 LAN = crossp(planorbit.getplanevector(), down);
	double lan = atan2(LAN.z, LAN.x) * 180 / PI;
	if(lan < 0)
		lan += 360;
	sprintf(buffer, "LAN    :%.4g°", lan);
	sketchpad->Text( 0, pos, buffer, strlen(buffer));
	pos += linespacing;

	if (status.status==1)
	{ //Vessel is landed!
		VECTOR3 tintersectvector=planorbit.getintersectvector(craft);//Gets intersection vector
		tpos=status.rpos;
		if (dotp(tintersectvector,tpos)<0)
			angle+=90;
		else
			angle=90-angle;
		if (angle<0) angle+=360;
		OBJHANDLE hmajor = base->gethmajor();
		if(oapiGetPlanetPeriod(hmajor) < 0) // retrograde rotation
		{
			angle += 180;
			if(angle > 360)
				angle -= 360;
		}
		len=sprintf(buffer,"Heading:%.4g°", angle);
	}
	else
	{
		len=sprintf(buffer, "Rel Inc:%.4g°", angle);
	}
	sketchpad->Text( 0, pos, buffer, len);

	// Calculate Delta-v
	if (status.rbody!=base->gethmajor()) return;
	//if (craft.getpedistance()<oapiGetSize(status.rbody)) return;//Haven't got into orbit yet
	double radius=length(tpos);
	double deltav=sqrt(planorbit.getgmplanet()*(2/radius+1/planorbit.getsemimajor()))-length(tvel);
	pos+=linespacing;
	TextShow(sketchpad,"Delta V:", 0, pos, deltav);
	pos+=linespacing;

	//Calculate timings for burn
	VECTOR3 planpos,craftpos,crmajaxis,plmajaxis,temp;
	double craftcosthi,craftsinthi;
	planorbit.getcurrentvectors(&planpos,&temp);//Plan is defined at periapsis
	craft.getcurrentvectors(&craftpos,&temp);
	craft.vectortothi(planpos,&craftcosthi,&craftsinthi);//Project onto current orbit
	double timefromstamp=craft.GetTimeToThi(craftcosthi,craftsinthi);
	//Only display if timestamp is current
	if (fabs(craft.gettimestamp()-oapiGetSimTime())<1)
	{
		TextShow(sketchpad,"T to Pe:",0,pos,timefromstamp);
		pos+=linespacing;
		TextShow(sketchpad,"Begin Burn:",0,pos,GetBurnStart(pV, THGROUP_MAIN, timefromstamp, deltav));
		pos+=linespacing;
		double angle=180/PI*acos(cosangle(planpos,craftpos));
		TextShow(sketchpad,"Ang. to Pe:",0,pos,angle);
		pos+=linespacing;
		if (craft.geteccentricity()>0.03)
		{
			craft.getaxes(&crmajaxis,&temp);
			planorbit.getaxes(&plmajaxis,&temp);
			angle=180/PI*acos(cosangle(crmajaxis,plmajaxis));
			TextShow(sketchpad,"S.maj diff:",0,pos,angle);
		}
	}
}


void slingshot::wordupdate(oapi::Sketchpad *sketchpad, int width, int height, basefunction *base)
{
	int linespacing=height/24;
	int pos=15*linespacing;
	const OrbitElements & craft=base->getcraftorbit();
	if (!craft.isvalid() || !planorbit.isvalid()) return;
	TextShow(sketchpad,"R. Inc:",0,pos,180*acos(cosangle(planorbit.getplanevector(),craft.getplanevector()))/PI);
	pos+=linespacing;
	TextShow(sketchpad,"Pe Ratio:",0,pos,craft.getpedistance()/planorbit.getpedistance());
	pos+=linespacing;
	double timetope=craft.GetTimeToThi(1,0)+craft.gettimestamp()-oapiGetSimTime();
	TextShow(sketchpad,"Time to Pe:",0,pos,timetope);
	pos+=linespacing;
	//Calculate reqd delta using energy calculation
	double craftreqvel=craft.getvelocityatdist(planorbit.getpedistance());
	double outplanpevel=sqrt(ejectvelocity2+2*planorbit.getgmplanet()/planorbit.getpedistance());
	if (fabs(outplanpevel-craftreqvel)>0.1)//Only show if a manoeuvre is required
	{
		TextShow(sketchpad,"Delta V:",0,pos,outplanpevel-craftreqvel);
		pos+=linespacing;
		OBJHANDLE hcraft=base->gethcraft();
		VESSEL *pV=oapiGetVesselInterface(hcraft);
		TextShow(sketchpad,"Begin Burn",0,pos,GetBurnStart(pV, THGROUP_MAIN, timetope, (outplanpevel-craftreqvel)));
		pos+=linespacing;
	}
}

void encounterplan::getplanorbit(OrbitElements *planorbit)
{
	planorbit->setinvalid();
}


void encounterplan::wordupdate(oapi::Sketchpad *sketchpad, int width, int height, basefunction *base)
{
	int linespacing=height/24;
	int pos=16*linespacing;
	//OrbitElements craft=base->getmanoeuvreorbit();
	//if (!craft.isvalid()) craft=base->getcraftorbit();//Gets manoeuvre if it's valid, otherwise craft
	const OrbitElements & craft = base->getmanoeuvreorbit().isvalid() ? base->getmanoeuvreorbit() : base->getcraftorbit();

	OBJHANDLE hmajor=base->gethmajor();
	double radius=oapiGetSize(hmajor);
	if (!craft.isvalid()) return;
	double ped=craft.getpedistance();
	if (ped<radius)
	{
		VECTOR3 position,velocity;
		craft.radiustovectors(radius,false,&position,&velocity);
		if (m_drawbase==0 && drawnbase)
		{
			double distfrombase=length(position-baseposition);
			TextShow(sketchpad,"L.site dist to Base:",0,pos,distfrombase);
			pos+=linespacing;
		}
		else
		{
			MATRIX3 planetrot;
			oapiGetRotationMatrix(hmajor, &planetrot);
			VECTOR3 relposition = mul(getinvmatrix(planetrot), position);
			double lat = asin(relposition.y / radius) * 180 / PI;
			double lng = (atan2(relposition.z, relposition.x)) * 180 / PI;
			lng -= 360.0 * craft.GetTimeToRadius(radius, false) / oapiGetPlanetPeriod(hmajor);
			while(lng < -180)
				lng += 360;
			char output[64];
			sprintf(output, "Land Site Lat/Long: %.4g, %.4g", lat, lng);
			sketchpad->Text( 0, pos, output, strlen(output));
			pos += linespacing;
		}
	}
	else
	{
		TextShow(sketchpad,"Min Alt:",0,pos,ped-radius);
		pos+=linespacing;
		double planetenergy=GRAVITY*oapiGetMass(hmajor)/ped;
		double escvelocity=sqrt(2*planetenergy);
		double actualpevelocity=sqrt(craft.getangmomentum2())/ped;
		TextShow(sketchpad,"Pe Vel:",0,pos,actualpevelocity);
		pos+=linespacing;
		TextShow(sketchpad,"Capture Delta:",0,pos,actualpevelocity-escvelocity);
		pos+=linespacing;
		TextShow(sketchpad,"Circ. Delta:",0,pos,actualpevelocity-(escvelocity/1.4142135623731));
		pos+=linespacing;

		VECTOR3 position,velocity;
		craft.radiustovectors(ped+10,false,&position,&velocity);
		if (m_drawbase==0 && drawnbase)
		{
			VECTOR3 plane=unit(craft.getplanevector());
			double distoffplane=dotp(plane,baseposition);
			TextShow(sketchpad,"Offplane Dist:",0,pos,distoffplane);
			pos+=linespacing;
			double distfrombase=length(position-baseposition);
			TextShow(sketchpad,"Pe dist to Base:",0,pos,distfrombase);
			pos+=linespacing;
		}
		else
		{
			MATRIX3 planetrot;
			oapiGetRotationMatrix(hmajor, &planetrot);
			VECTOR3 relposition = mul(getinvmatrix(planetrot), position);
			double lat = asin(relposition.y / radius) * 180 / PI;
			double lng = (atan2(relposition.z, relposition.x)) * 180 / PI;
			lng -= 360.0 * craft.GetTimeToRadius(radius, false) / oapiGetPlanetPeriod(hmajor);
			while(lng < -180)
				lng += 360;
			char output[64];
			sprintf(output, "Pe Lat/Long: %.4g, %.4g", lat, lng);
			sketchpad->Text( 0, pos, output, strlen(output));
			pos += linespacing;
		}
	}

	VECTOR3 ecliptic={0,-1,0};
	double inclination=acos(dotp(unit(craft.getplanevector()),ecliptic));
	inclination=inclination/PI*180;
	TextShow(sketchpad,"Inclination:",0,pos,inclination);
	pos+=linespacing;
	VECTOR3 lanvector=unit(crossp(craft.getplanevector(),ecliptic));
	double lan=acos(lanvector.x)/PI*180;
	if (lanvector.z<0) lan=-lan;
	TextShow(sketchpad,"LAN ",0,pos,lan);
}

void majejectplan::wordupdate(oapi::Sketchpad *sketchpad,int width, int height, basefunction *base)
{
    int linespacing=height/24;
    int wspace=width/19;
    int wpos=9*wspace;

    if (ratioorbit>0) //Don't show if View: Sling Direct
    {
        TextShow(sketchpad,"Pe/Pl Rad:",0,15*linespacing,ratioorbit);
    }
    else
    {
        double numDays = base->GetTimeIntercept() - m_ejdate;
        TextShow(sketchpad,"TOF (days):",wpos,22*linespacing,numDays);
        double totaldv=length(ejectvector);
        if (totaldv>0.1)
        {
            //bottom right, watch both glass cockpit AND panel view for correct placement.
            TextShow(sketchpad,"Total DeltaV:",wpos,23*linespacing,totaldv);
        }
    }
}

void minorejectplan::graphupdate(oapi::Sketchpad *sketchpad, Graph *graph,basefunction *base)
{
	base->SelectDefaultPen(sketchpad,TransXFunction::Yellow);
	planorbit.draworbit(sketchpad,graph,true);
}

void majejectplan::graphscale(Graph *graph)
{
	if (planorbit.isvalid())
		graph->setviewscale(planorbit);
}

void majejectplan::graphupdate(oapi::Sketchpad *sketchpad, Graph *graph,basefunction *base)
{
	base->SelectDefaultPen(sketchpad,TransXFunction::Yellow);
	planorbit.draworbit(sketchpad,graph,false);
}

bool slingejectplan::init(class MFDvarhandler *vars, class basefunction *base)
{
	m_totalvel.init(vars,3,3,"Velocity.",0, 0,1e8,0.1,1000);
	m_outwardangle.init(vars, "Outward angle",true);
	m_incangle.init(vars, "Inc. angle",false);
	m_inheritvel.init(vars,3,3,"Inherit Vel.",0,1,"Yes","No","","","");
	m_ejdate.init(vars,3,3,"Eject date",0,0,1e20,0.000005,1000000);
	m_ejdate=oapiGetSimMJD();//Temporary default.

	m_totalvel.sethelpstrings(
		"Total velocity of escape","");
	m_outwardangle.sethelpstrings(
		"Angle away from prograde direction",
		"(Direction of Minor body)");
	m_incangle.sethelpstrings(
		"Rotates around prograde vector",
		"");
	m_inheritvel.sethelpstrings(
		"Inherit velocity and date from",
		"previous stage's plan.");
	m_ejdate.sethelpstrings(
		"Date of periapsis of minor body.",
		"Normally leave as inherited.");
	base->sethelp(
		"Used to leave SOI of this planet/moon",
		"for another. Designed for use in",
		"combination with the slingshot plan",
		"in the previous stage","");
	return true;
}


bool majorejectplan::init(class MFDvarhandler *vars, class basefunction *base)
{
	m_prograde.init(vars,3,3,"Prograde vel.", 0, -1e8, 1e8, 0.1, 1000);
	m_ejdate.init(vars,3,3,"Eject date", 0, 0, 1e20, 0.000005, 1000000);
	m_ejdate=oapiGetSimMJD();//Temporary default
	m_inheritvel=1;//MFDvariable capabilities not used in this class
	m_outwardvel.init(vars,3,3,"Outward vel.", 0,-1e8,1e8,0.1,1000);
	m_chplvel.init(vars,3,3,"Ch. plane vel.", 0, -1e8, 1e8, 0.1,1000);

	m_prograde.sethelpstrings(
		"Positive to move outward from MAJ",
		"Negative to move inward toward MAJ");
	m_ejdate.sethelpstrings(
		"Your planned launch date. Adjust to",
		"find efficient launch window.");
	m_outwardvel.sethelpstrings(
		"Adjust to give you launch date",
		"flexibility or fast flight.");
	m_chplvel.sethelpstrings(
		"Adjust grey target plane intersect",
		"line to coincide with intercept.");
	base->sethelp(
		"Used to set direction at exit of",
		"SOI to aim for another planet eg",
		"Earth to Mars. MAJ is body all",
		"orbit. Best to plan before you",
		"take off.");
	return true;
}

void majorejectplan::calcejectvector(const VECTOR3 &rminplane,const VECTOR3 &minorvel, double inheritedvelocity)
{//Final param not used here
	VECTOR3 forward=unit(minorvel)*m_prograde;
	VECTOR3 outward=unit(crossp(minorvel, rminplane))*m_outwardvel;
	VECTOR3 sideward=unit(rminplane)*m_chplvel;
	ejectvector=forward+outward+sideward; //=Eject vector in RMin frame
}

void slingejectplan::calcejectvector(const VECTOR3 &rminplane,const VECTOR3 &minorvel, double inheritedvelocity)
{
	VECTOR3 forward=unit(minorvel)*m_outwardangle.getcos()*m_incangle.getcos();
	VECTOR3 outward=unit(crossp(minorvel,rminplane))*m_outwardangle.getsin()*m_incangle.getcos();
	VECTOR3 sideward=unit(rminplane)*m_incangle.getsin();
	ejectvector=forward+outward+sideward;
	if (m_inheritvel==0 && inheritedvelocity>0)
		ejectvector=ejectvector*inheritedvelocity;
	else
		ejectvector=ejectvector*m_totalvel;
}


void majejectplan::calculate(class MFDvarhandler *vars,basefunction *base)
{
	//get the position and velocity vectors of the minor planet at the eject time
	const OrbitElements & rmin=base->getminororbit();
	VECTOR3 minorpos,minorvel;
	planorbit.setinvalid();
	mapfunction *map=mapfunction::getthemap();
	basefunction *previous=base->getpreviousfunc();

	OrbitElements craftinrmin;
	double escvel=-1;
	double soisize;
	plan *previousplan;
	if (previous==NULL) return;//Don't calculate plan if there's no previous stage at all
	//Has the effect of switching plan off after reasonable distance from minor body is reached

	previousplan=previous->getplanpointer();
	soisize=map->getsoisize(previous->gethmajor());
	if (previousplan!=NULL)
	{
		escvel=previousplan->getentryvelocity();
		previousplan->getplanorbit(&craftinrmin);
		ratioorbit=previousplan->getratio2radius();
		timefromnow=previousplan->geteventtime();
		if (m_inheritvel==0 && craftinrmin.isvalid())
			m_ejdate=timefromnow/SECONDS_PER_DAY+oapiTime2MJD(0);
		previous->addaction(0);//Schedules an update for previous function - helps fluidity of slingshots
	}
	timefromnow=(m_ejdate-oapiTime2MJD(0))*SECONDS_PER_DAY;
	rmin.timetovectors(timefromnow-rmin.gettimestamp(),&minorplanetattime);
	minorplanetattime.getposvel(&minorpos,&minorvel);
	//rmin.timetovectors(timefromnow-rmin.gettimestamp(),&minorpos,&minorvel);

	//derive the velocity vector described by the variables
	calcejectvector(rmin.getplanevector(),minorvel,escvel);//Calculates ejectvector
	double ejectvector2=length2my(ejectvector);
	if (ejectvector2<1) return;
	if (fabs(ejectvector2-escvel*escvel)*20<escvel*escvel && craftinrmin.isvalid())
		planorbit.minortomajorinit(craftinrmin,rmin,soisize);
	else
		planorbit.init(minorpos,minorvel+ejectvector,timefromnow,rmin.getgmplanet());
}

VECTOR3 plan::getvelocityvector()
{
	VECTOR3 temp={0,0,0};
	return temp;
}

void plan::getplanorbit(OrbitElements *planorbit)
{
	planorbit->setinvalid();
}

class plan *plan::clone(class MFDvarhandler *vars,basefunction *base)
{
	class plan *thisplan=iclone();
	thisplan->init(vars,base);
	return thisplan;
}

class plan *minorejectplan::iclone()
{
	class minorejectplan *newplan=new minorejectplan();
	newplan->planorbit=planorbit;
	return newplan;
}

void slingshot::getplanorbit(OrbitElements *tplanorbit)
{//Only pass on plan orbit if it's basically correct
	if (goodness<1.01 && goodness>0.99)
		*tplanorbit=planorbit;
	else
		tplanorbit->setinvalid();
}

VECTOR3 majejectplan::getvelocityvector()
{
	return ejectvector;
}

void minorejectplan::calculate(class MFDvarhandler *vars,basefunction *base)
{
	//get the required velocity vector from the function in front of this one
	planorbit.setinvalid();
	basefunction *next=base->getnextfunc();
	mapfunction *map=mapfunction::getthemap();
	double soisize=map->getsoisize(base->gethmajor());
	double gmhmajor=oapiGetMass(base->gethmajor())*GRAVITY;
	plan *nextplan;
	VECTOR3 ejectvector;
	double ejectvelocity2=0;
	double ejecttime;
	if (next!=NULL)
	{
		nextplan=next->getplanpointer();
		if (nextplan!=NULL)
		{
			ejectvector=nextplan->getvelocityvector();
			ejecttime=nextplan->geteventtime();
			ejectvelocity2=dotp(ejectvector,ejectvector);
		}
	}
	else
	{
		nextplan=NULL;
		return;
	}
	if (ejectvelocity2<1)
	{//checks if a velocity has been set!
		return;
	}

	//Set up temporary coordinate system
	VECTOR3 rminplane=(base->getcontextorbit()).getplanevector();
	VECTOR3 txaxis=unit(ejectvector);
	VECTOR3 tzaxis=unit(crossp(ejectvector, rminplane));
	VECTOR3 tyaxis=unit(crossp(ejectvector, tzaxis));
	double ejorientsinvalue = m_ejorient.getsin();
	double ejorientcosvalue = m_ejorient.getcos();
	VECTOR3 tnaxis=tzaxis*ejorientcosvalue+tyaxis*ejorientsinvalue;

	//Get eccentricity for hypothetical orbit
	double peridistance=m_ped;
	if (peridistance*peridistance>soisize*soisize)//Ensures periapsis is not outside sphere of influence.
	{
		peridistance=soisize*0.9;
		m_ped=peridistance;
	}
	double ecc=peridistance*ejectvelocity2/gmhmajor+1;
	//Get cos and sin thi for infinite radius
	double costhiinfangle=1/ecc;
	double sinthiinfangle=sqrt(1-costhiinfangle*costhiinfangle);

	// Use this, in conjunction with other elements, to obtain
	//radius and velocity vectors for periapsis of required orbit
	VECTOR3 periposvector=(txaxis*(-costhiinfangle)+tnaxis*sinthiinfangle)*peridistance; //Periapsis position vector
	double tvel=sqrt(2*gmhmajor/peridistance+ejectvelocity2); // Velocity size at periapsis
	VECTOR3 perivelvector=(txaxis*(sinthiinfangle)+tnaxis*costhiinfangle)*tvel; //Velocity vector

	// Create orbit for hypothetical orbit in rmin
	planorbit.init(periposvector, perivelvector, ejecttime, gmhmajor);

	//use that and the periapsis distance to calculate the eccentricity
	//use that and the rotation vector to generate the required vectors
	//make the orbit, then track back to also give parameters to pass to the major function
}

void minorejectplan::getplanorbit(OrbitElements *tplanorbit)
{
	*tplanorbit=planorbit;
}



