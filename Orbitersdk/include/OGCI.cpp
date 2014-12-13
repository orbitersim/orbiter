// =================================================================================================================================
// The MIT Lisence:
//
// Copyright (C) 2014 Jarmo Nikkanen
//
// Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation 
// files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, 
// modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software 
// is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
// OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
// LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR
// IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
// =================================================================================================================================

#ifndef __OGCI_CPP
#define __OGCI_CPP

#include <windows.h>
#include "OGCI.h"

HMODULE ogciHandle = NULL;

_ogciClientID ogciClientID = NULL;
_ogciTakeCamera ogciTakeCamera = NULL;
_ogciReleaseCameraTake ogciReleaseCameraTake = NULL;
_ogciDeleteCustomCamera ogciDeleteCustomCamera = NULL;
_ogciCustomCameraOnOff ogciCustomCameraOnOff = NULL;
_ogciSetupCustomCamera ogciSetupCustomCamera = NULL;
_ogciSketchBlt ogciSketchBlt = NULL;
_ogciSketchBltEx ogciSketchBltEx = NULL;
_ogciSketchRotateBlt ogciSketchRotateBlt = NULL;
_ogciSketchpadVersion ogciSketchpadVersion = NULL;

#ifdef OGCI_FOR_2010P1
_oapiCreateSurfaceEx oapiCreateSurfaceEx = NULL;
#endif


bool PostInit(HMODULE hClient)
{
	if (!hClient) return false;

	ogciClientID = (_ogciClientID)GetProcAddress(hClient, "ogciClientID");
	// -------------
	ogciTakeCamera = (_ogciTakeCamera)GetProcAddress(hClient, "ogciTakeCamera");
	ogciReleaseCameraTake = (_ogciReleaseCameraTake)GetProcAddress(hClient, "ogciReleaseCameraTake");
	// -------------
	ogciDeleteCustomCamera = (_ogciDeleteCustomCamera)GetProcAddress(hClient, "ogciDeleteCustomCamera");
	ogciCustomCameraOnOff = (_ogciCustomCameraOnOff)GetProcAddress(hClient, "ogciCustomCameraOnOff");
	ogciSetupCustomCamera = (_ogciSetupCustomCamera)GetProcAddress(hClient, "ogciSetupCustomCamera");
	// -------------
	ogciSketchBlt = (_ogciSketchBlt)GetProcAddress(hClient, "ogciSketchBlt");
	ogciSketchBltEx = (_ogciSketchBltEx)GetProcAddress(hClient, "ogciSketchBltEx");
	ogciSketchRotateBlt = (_ogciSketchRotateBlt)GetProcAddress(hClient, "ogciSketchRotateBlt");
	ogciSketchpadVersion = (_ogciSketchpadVersion)GetProcAddress(hClient, "ogciSketchpadVersion");
	
#ifdef OGCI_FOR_2010P1
	oapiCreateSurfaceEx = (_oapiCreateSurfaceEx)GetProcAddress(hClient, "ogciCreateSurfaceEx");
#endif

	return ogciEnabled();
}

bool ogciEnabled()
{
	if (ogciHandle && ogciClientID) return true;
	return false;
}

bool ogciInitialize()
{
	if (ogciHandle) return ogciEnabled(); // Already Initialized
	if (!ogciHandle) ogciHandle = GetModuleHandle("D3D9Client.dll");
	if (!ogciHandle) ogciHandle = GetModuleHandle("D3D9ClientDebug.dll");
	if (ogciHandle) return PostInit(ogciHandle);
	else oapiWriteLog("OGCI: [Graphics Client Not Found]");
	return false;
}

#endif
	