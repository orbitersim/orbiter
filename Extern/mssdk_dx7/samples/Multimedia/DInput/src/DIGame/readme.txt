//-----------------------------------------------------------------------------
// 
// Sample Name: DIGame Sample
// 
// Copyright (c) 1998 Microsoft Corporation. All rights reserved.
// 
//-----------------------------------------------------------------------------


Description
===========
  DIGame is a simple game that uses DirectInput to read the keyboard, mouse,
  and joystick.

Path
====
  Source: Mssdk\Samples\Multimedia\DInput\Src\DIGame

  Executable: Mssdk\Samples\Multimedia\DInput\Bin

User's Guide
============
  The game is controlled by using the mouse, keyboard, or joystick. The active
  input device is chosen from the menu. Move left, right, up, and down, and
  shoot the advancing weapons and special items. The special items are temporary
  shields (marked as S), and an item that momentarily stops times (marked as T).

Programming Notes
=================
  This sample shows the way to use DirectInput in a manner intended for game
  input. Particular attention should go to the way input devices are acquired,
  sampled, and released.

