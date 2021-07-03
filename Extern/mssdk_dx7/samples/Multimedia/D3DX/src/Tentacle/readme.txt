//-----------------------------------------------------------------------------
// 
// Sample Name: Tentacle
// 
// Copyright (c) 1999 Microsoft Corporation. All rights reserved.
// 
//-----------------------------------------------------------------------------


Description
===========
  This sample demonstrates the vertex blending and user clip plane
  features which are new to DX7.  


Path
====
  Source: Mssdk\Samples\Multimedia\D3dx\Src\Tentacle

  Executable: Mssdk\Samples\Multimedia\D3dx\Bin


Keyboard controls
=================
  (NUMLOCK must be on)

  Move                    W,S
  Slide                   Arrow Keys
  Turn                    Numpad 4,6
  Pitch                   Numpad 2,8
  Roll                    Numpad 7,9
  Exit                    Escape

  Toggle wireframe        F2
  Toggle ground           G
  Toggle reflection       R
  Toggle lens flare       L
  Toggle framerate        F


Mouse controls
==============

  Rotate left arm         LButton                     
  Rotate right arm        RButton                  
  Rotate base             LButton+RButton             

  Twist left arm          Shift+LButton               
  Twist right arm         Shift+RButton
  Twist base              Shift+LButton+RButton

  Stretch left finger     Ctrl+LButton
  Stretch right finger    Ctrl+RButton


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