//-----------------------------------------------------------------------------
// 
// Sample Name: Particle
// 
// Copyright (c) 1999 Microsoft Corporation. All rights reserved.
// 
//-----------------------------------------------------------------------------


Description
===========
  The Particle displays a particle system.  Each of the particles displayed is
  actually a billboarded face, with a texture map applied.  The particle system
  also lights up, and is reflected by the ground plane.
  

Path
====
  Source: Mssdk\Samples\Multimedia\D3dx\Src\Particle

  Executable: Mssdk\Samples\Multimedia\D3dx\Bin
  

Keyboard controls
=================
  (NUMLOCK must be on)

  Move                    W,S
  Slide                   Arrow Keys
  Turn                    Numpad 4,6
  Pitch                   Numpad 2,8

  Pause simulation        Pause
  Mobile emitter          Enter
  Change color            Space

  Toggle ground           G
  Toggle reflection       R
  Toggle framerate        F

  Exit                    Escape



Command-line arguments
======================

  Full screen             fs
  # of Backbuffers        <any number>     (only used with full screen)    

  Use HWLEVEL_REFERENCE   ref
  Use HWLEVEL_2D          rgb
  Use HWLEVEL_RASTER      hal
  Use HWLEVEL_TL          tnl


Programming Notes
=================
  This sample was built using the D3DX utility library.