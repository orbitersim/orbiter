//-----------------------------------------------------------------------------
// 
// Sample Name: DDOverlay Sample
// 
// Copyright (c) 1998-1999 Microsoft Corporation. All rights reserved.
// 
//-----------------------------------------------------------------------------


Description
===========
  This sample application demonstrates the use of overlays in a windowed 
  DirectDraw application.

Path
====
  Source: Mssdk\Samples\Multimedia\DDraw\Src\DDOverlay

  Executable: Mssdk\Samples\Multimedia\DDraw\Bin

User's Guide
============
  Your hardware must support overlays in order for the program to run.

  Try moving, resizing, and minimizing and restoring the window. Press ALT+F4 
  or click the Close button to quit.

Programming Notes
=================
  The program checks for overlay support, loads a bitmap into an overlay 
  surface, and updates the window from the overlay surface in response to 
  Windows messages.

