//-----------------------------------------------------------------------------
// 
// Sample Name: ClipMirror Sample
// 
// Copyright (c) 1998 Microsoft Corporation. All rights reserved.
// 
//-----------------------------------------------------------------------------


Description
===========
  The ClipMirror sample demonstrates the use of custom-defined clip planes.
  A 3D scene is rendered normally, and then again in a 2nd pass as if reflected
  in a planar mirror. Clip planes are used to clip the reflected scene to the
  edges of the mirror.

Path
====
  Source: Mssdk\Samples\Multimedia\D3dim\Src\ClipMirror

  Executable: Mssdk\Samples\Multimedia\D3dim\Bin

User's Guide
============
  Press F1 to see available commands.

Programming Notes
=================
  The main feature of this sample is the use of clip planes. A mirror has 4
  edges, so 4 clip planes are used. Each plane is defined by the eye point
  and two vertices of one edge of the mirror.

  This sample was built using the Direct3D sample framework.

