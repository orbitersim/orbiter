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

/*****************************************

  This is a base class from which is derived
  the functions that go into TransX.

  */
#ifndef __MFDFUNCTION_H
#define __MFDFUNCTION_H

class MFDFunction
{
private:

	bool allowedtowrite;//Only one action outstanding allowed per instance - use this to keep control of queues!
	MFDFunction *next,*previous;//Next and previous in current chain
	static MFDFunction *lastfast,*firstfast,*lastslow,*firstslow;
protected:
	bool valid;
	virtual void dolowpriaction(); //Overloaded to carry out processing task in background.
	virtual void dohighpriaction(); //Overloaded to carry out processing task in background
	void dofastaction();//carries out preparation for fast action
	void doslowaction();//carries out preparation for slow action
public:
	MFDFunction();
	void delist();
	virtual ~MFDFunction();
	bool isvalid();

	static void donextaction(); //Executes the next action in the stack
	virtual bool addaction(int priority); //Adds a timestep action to the list
};

#endif