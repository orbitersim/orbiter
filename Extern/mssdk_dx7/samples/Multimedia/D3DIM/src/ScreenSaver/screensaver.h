//-----------------------------------------------------------------------------
// File: ScreenSaver.h
//
// Desc: Windows code for making Direct3D screen savers.
//
//       This code uses the Direct3D sample framework.
//
// Copyright (c) 1996-1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------




//-----------------------------------------------------------------------------
// Short name for this screen saver
//-----------------------------------------------------------------------------
TCHAR g_strScreenSaverName[] = TEXT("Bend");




//-----------------------------------------------------------------------------
// Name: struct ScreenSaverOptions
// Desc: Structure to hold the options for the screensaver. A custom D3D screen
//       saver should put all of its options in this struct. Note: the first
//       two variables are required as they are used in ScreenSaver.cpp.
//-----------------------------------------------------------------------------
struct ScreenSaverOptions
{
	// Required options
	BOOL  bUse640x480Mode;
	BOOL  bUseHardware;

	// Custom options
	FLOAT fSpeed;
};




//-----------------------------------------------------------------------------
// Global options structure. Set this structure with default options values.
//-----------------------------------------------------------------------------
ScreenSaverOptions g_CurrentOptions =
{
	FALSE, // bUse640x480Mode
	TRUE,  // bUseHardware
	1.0f,  // fSpeed
};




