//-----------------------------------------------------------------------------
// 
// Sample Name: Multimon Sample
// 
// Copyright (c) 1999 Microsoft Corporation. All rights reserved.
// 
//-----------------------------------------------------------------------------


Description
===========
  Multimon demonstrates some of the techniques that can be used in writing
  an application that takes advantage of multiple monitors.

Path
====
  Source: Mssdk\Samples\Multimedia\DDraw\Src\Multimon

  Executable: Mssdk\Samples\Multimedia\DDraw\Bin

User's Guide
============
  Multimon requires no user input. Press the ESC key to quit the program.  Two
  "sprites" (moving surfaces) are created.  "Sprite 1" uses video memory 
  whenever possible.  "Sprite 2" always uses system memory.

Programming Notes
=================
  Multimon shows examples of a variety of topics:

  - To use the multimonitor APIs such as GetMonitorInfo and MonitorFromRect
    on Windows 95, you can include the file multimon.h.  In addition, in one
	of the C or C++ files, you need to #define COMPILE_MULTIMON_STUBS before 
	including multimon.h.  This allows the multimon APIs to return reasonable
	values when running on Windows 95.  If your program does not need to be
	compatible with Windows 95, or if you do not use any of the multimonitor
	APIs, you do not need to include multimon.h or #define 
	COMPILE_MULTIMON_STUBS.

  - When enumerating DirectDraw devices, you can use either DirectDrawEnumerate
    or DirectDrawEnumerateEx.  DirectDrawEnumerateEx is available on Windows 98
	systems with DX5 and later, and all other systems with DX6 or later.  
	Because not all systems can be assumed to have DirectDrawEnumerateEx, 
	DirectDraw was set up so programmers had to use LoadLibrary and 
	GetProcAddress to check for its presence.  In DX7, this restriction has 
	been removed, so you can call DirectDrawEnumerateEx directly in code, but 
	you should note that this will prevent your program from running on a system 
	which does not have at least DX7 installed.  This sample shows how to do the 
	LoadLibrary/GetProcAddress technique, and how to fall back on 
	DirectDrawEnumerate if DirectDrawEnumerateEx is not available.

  - Fullscreen, multimonitor apps need to deal with focus and device windows.
    The focus window receives user input messages, and the device windows are
	used to cover each screen.  This program shows how to call 
	SetCooperativeLevel to properly create and assign these windows.

  - Each screen gets its own DirectDraw interface, and DirectDrawSurfaces created
    on one DD interface cannot be used by any other DD interface.  So creating
	graphics that span multiple monitors takes some extra work.  This sample
	demonstrates two techniques.  For best performance, video memory surfaces
	should be used.  A separate video memory DirectDrawSurface must be created
	on each screen.  For the cases where a system memory surface is required or
	desired, one must still create separate DirectDrawSurfaces for each screen,
	but they can be configured to point to the same surface data memory.  The 
	SetSurfaceDesc API can be used to accomplish this.  Doing this has no 
	performance impact, but it avoids unnecessary consumption of system memory.

  - Blt calls usually fail when they would cause data to be written outside the
    borders of the destination surface.  This failure can be avoided by attaching
	clipper objects to the destinations.  This sample shows how to create a 
	clipper for each screen and attach it to the front and back buffers so that
	the sprite surfaces can be blitted without being manually clipped by the 
	application first.

