//-----------------------------------------------------------------------------
// 
// Sample Name: StencilDepth Sample
// 
// Copyright (c) 1999 Microsoft Corporation. All rights reserved.
// 
//-----------------------------------------------------------------------------


Description
===========
  The StencilDepth sample demonstrates how to use the stencil to visualize the
  depth complexity of a scene.

Path
====
  Source: Mssdk\Samples\Multimedia\D3dim\Src\StencilDepth

  Executable: Mssdk\Samples\Multimedia\D3dim\Bin

User's Guide
============
  Press F1 to see available commands.

Programming Notes
=================
  The depth complexity is recorded by increment the value in the stencil
  buffer for each pixel drawn. Then, three colored rectangles are drawn
  over the scene, masked by the bits in the stencil buffer. The result is a
  false-color image of the stencil buffers contents, which is the recorded
  depth complexity of the scene.

  This sample was built using the Direct3D sample framework. 

