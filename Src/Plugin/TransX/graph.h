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

#pragma once

#include "orbitelements.h"

class Graph{
//This class holds information on a display window for graphs, and vectors for the projection angle of these
//orbit graphs, and current scaling
friend class OrbitElements;
public:
	enum Shape
	{
		Circle,
		Rectangle
	};

	void drawvectorline(oapi::Sketchpad *sketchpad, const VECTOR3 &line);//Draws vector line at current projection & scaling
	void drawplanet(oapi::Sketchpad *sketchpad, OBJHANDLE body); //Draw circle representing planet
	void drawatmosphere(oapi::Sketchpad *sketchpad, OBJHANDLE body); //Draw circle representing atmosphere
	void setprojection(const OrbitElements &torbit);//View set from orbit's plane vector
	void setprojection(const VECTOR3 &projection);// View set from vector
	void setprojection(const VECTOR3 &txaxis, const VECTOR3 &tyaxis, const VECTOR3 &tzaxis);//Explicitly set axes
	void drawvector(oapi::Sketchpad *sketchpad,const VECTOR3 &line1);//Draw vector using current projection & scale
	void drawmarker(oapi::Sketchpad *sketchpad, const VECTOR3 & location, Shape shape);	// draws a marker at the specified location
	void drawtwovector(oapi::Sketchpad *sketchpad, const VECTOR3 &line1, const VECTOR3 &line2);//Draw two vectors using current projection & scaling
	void draworbit(const class OrbitElements &element, oapi::Sketchpad *sketchpad, bool drawradius);//Calls draworbit in orbitelements
	double vectorpointdisplay(oapi::Sketchpad *sketchpad, const VECTOR3 &target, MFD2 *mfd, VESSEL *vessel, bool isposition);//Nice little pointer utility
	void setviewscale(const class OrbitElements &orbit);//Set scale of picture using orbit size
	void setviewscale(double temp);//Set scale using a number
	void setviewscalesize(double temp);//Set scale using a distance size
	void setviewwindow(DWORD xstart, DWORD ystart, DWORD xend, DWORD yend);//Set a window within MFD
	void getviewwindow(DWORD *xstart, DWORD *ystart, DWORD *xend, DWORD *yend);//Get back above info
	double getviewscale();//Get viewscale number
private:
	void drawcircle(oapi::Sketchpad *sketchpad, double size); //Draw circle representing planet
	VECTOR3 xaxis, yaxis, zaxis; // projection vectors
	DWORD ixstart, iystart, ixend, iyend, windowsize;//window parameters
	double scale;//scaling factor for diagram
};
