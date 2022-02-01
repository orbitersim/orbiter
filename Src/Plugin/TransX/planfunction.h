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

#ifndef __PLANFUNCTION_H
#define __PLANFUNCTION_H

#include "mfdvarhandler.h"
#include "mfdvartypes.h"
#include "orbitelements.h"

class basefunction;

class plan
{
public:
	virtual void getplanorbit(OrbitElements *planorbit) = 0;//All plans can return an orbit to the main function
	virtual bool maingraph(oapi::Sketchpad *sketchpad,Graph *graph,basefunction *base){return true;};
	virtual int getplanid() = 0;
	virtual class plan *clone(class MFDvarhandler *vars,basefunction *base);//returns a pointer to a clone of this plan
	virtual class plan *iclone() = 0;
	virtual void graphscale(Graph *graph){};//Use to scale graph for later update
	virtual void graphupdate(oapi::Sketchpad *sketchpad, Graph *graph,basefunction *base){};//Update graphs
	virtual void wordupdate(oapi::Sketchpad *sketchpad,int width, int height, basefunction *base){};//Update text
	virtual bool init(class MFDvarhandler *vars, basefunction *base) = 0;//Initialises plan - sets up variableset
	virtual void calculate(class MFDvarhandler *vars,basefunction *base) = 0;
	virtual double getentryvelocity(){return 0;};
	virtual double getratio2radius(){return 0;};
	virtual VECTOR3 getvelocityvector();
	virtual void getlabel(char *buffer) = 0;
	virtual void getviewname(char *buffer) = 0;
	virtual double geteventtime(){return 0;};
	virtual ~plan(){};
};

class minorejectplan:public plan
{
public:
	virtual int getplanid(){return 1;};
	virtual void getlabel(char *buffer){strcpy(buffer,"Plan:Escape");};
	virtual void getviewname(char *buffer){strcpy(buffer,"View:Escape Plan");};
	virtual void getplanorbit(OrbitElements *tplanorbit);
	virtual void graphscale(Graph *graph);//Use to scale graph for later update
	virtual bool maingraph(oapi::Sketchpad *sketchpad,Graph *graph, basefunction *base);
	virtual void wordupdate(oapi::Sketchpad *sketchpad, int width, int height, basefunction *base);
	virtual void graphupdate(oapi::Sketchpad *sketchpad,Graph *graph,basefunction *base);
	virtual bool init(class MFDvarhandler *vars, basefunction *base);
	virtual void calculate(class MFDvarhandler *vars,basefunction *base);
	virtual ~minorejectplan(){};
	virtual class plan *iclone();
private:
	OrbitElements planorbit;
	basefunction *ibase;
	MFDvarfloat m_ped;
	MFDvarangle m_ejorient;
	MFDvardiscrete m_equatorial;
};

class majejectplan:public plan
{
public:
	virtual void getplanorbit(OrbitElements *tplanorbit){*tplanorbit=planorbit;};

	virtual void graphscale(Graph *graph);//Use to scale graph for later update
	virtual void graphupdate(oapi::Sketchpad *sketchpad,Graph *graph,basefunction *base);
	virtual void wordupdate(oapi::Sketchpad *sketchpad,int width, int height, basefunction *base);
	virtual void calculate(class MFDvarhandler *vars,basefunction *base);
	virtual VECTOR3 getvelocityvector();
	virtual double geteventtime(){return timefromnow;};
	virtual ~majejectplan(){};
protected:
	OrbitTime minorplanetattime;
	virtual void calcejectvector(const VECTOR3 &rminplane,const VECTOR3 &minorvel, double inheritedvelocity) = 0;
	MFDvarMJD m_ejdate;
	MFDvardiscrete m_inheritvel;
	double ratioorbit;
	double timefromnow;
	OrbitElements planorbit;
	VECTOR3 ejectvector;
};

class majorejectplan:public majejectplan
{
private:
	MFDvarfloat m_prograde,m_outwardvel,m_chplvel;
	virtual bool init(class MFDvarhandler *vars, basefunction *base);
	virtual void calcejectvector(const VECTOR3 &rminplane,const VECTOR3 &minorvel, double inheritedvelocity);
public:
	virtual int getplanid(){return 2;};
	virtual void getlabel(char *buffer){strcpy(buffer,"Plan:Eject");};
	virtual void getviewname(char *buffer){strcpy(buffer,"View:Eject Plan");};
	virtual class plan *iclone(){return new majorejectplan();};
	virtual ~majorejectplan(){};
};

class slingejectplan:public majejectplan
{
private:
	MFDvarfloat m_totalvel;
	MFDvarangle m_outwardangle, m_incangle;


	virtual bool init(class MFDvarhandler *vars, basefunction *base);

	void calcejectvector(const VECTOR3 &rminplane,const VECTOR3 &minorvel, double inheritedvelocity);
public:
	virtual ~slingejectplan(){};
	virtual class plan *iclone(){return new slingejectplan();};
	virtual int getplanid(){return 3;};
	virtual void getlabel(char *buffer){strcpy(buffer,"Plan:Sling Direct");};
	virtual void getviewname(char *buffer){strcpy(buffer,"View:Sling Direct");};

};

class encounterplan:public plan
{
private:
	virtual bool init(class MFDvarhandler *vars,class basefunction *base);
	MFDvardiscrete m_drawbase;
	VECTOR3 baseposition;
	bool drawnbase;
public:
	virtual class plan *iclone(){ return new encounterplan();};
	virtual int getplanid(){return 5;};
	virtual void getplanorbit(OrbitElements *planorbit);
	virtual void calculate(class MFDvarhandler *vars,basefunction *base){};
	encounterplan(){drawnbase=false;};
	virtual void graphupdate(oapi::Sketchpad *sketchpad, Graph *graph, basefunction *base);
	virtual void wordupdate(oapi::Sketchpad *sketchpad, int width, int height, basefunction *base);
	virtual void getlabel(char *buffer){strcpy(buffer,"Plan:Encounter");};
	virtual void getviewname(char *buffer){strcpy(buffer,"View:Encounter");};
	virtual ~encounterplan(){};
};

class slingshot:public plan
{
public:
	slingshot(){periapsisguess=-1;inwardvelocity=0;ejectvelocity2=0;};
	virtual class plan *iclone(){return new slingshot();};
	virtual void getplanorbit(OrbitElements *tplanorbit);
	virtual void graphscale(Graph *graph);//Use to scale graph for later update
	virtual void graphupdate(oapi::Sketchpad *sketchpad, Graph *graph,basefunction *base);
	virtual void wordupdate(oapi::Sketchpad *sketchpad, int width, int height, basefunction *base);
	virtual void calculate(class MFDvarhandler *vars,basefunction *base);
	virtual bool maingraph(oapi::Sketchpad *sketchpad,Graph *graph,basefunction *base);//bool returns whether base should draw a graph
	virtual double geteventtime(){return eventtime;};
	virtual double getentryvelocity(){return inwardvelocity;};
	virtual double getratio2radius(){return ratiotoradius;};
	virtual int getplanid(){return 4;};
	virtual void getlabel(char *buffer){strcpy(buffer,"Plan:Slingshot");};
	virtual void getviewname(char *buffer){strcpy(buffer,"View:Slingshot");};
private:
	virtual bool init(class MFDvarhandler *vars, class basefunction *base);
	MFDvardiscrete m_selectorbit;
	double goodness;
	double inwardvelocity,ejectvelocity2;
	double eventtime;
	double ratiotoradius;//Ratio of periapsis distance to the planet radius
	double periapsisguess;//guess of periapsis of slingshot
	OrbitElements planorbit;
};

#endif
