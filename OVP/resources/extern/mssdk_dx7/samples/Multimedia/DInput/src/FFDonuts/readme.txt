//-----------------------------------------------------------------------------
// 
// Sample Name: FFDonuts Sample
// 
// Copyright (c) 1998 Microsoft Corporation. All rights reserved.
// 
//-----------------------------------------------------------------------------


Description
===========
  This is a variation on the Space Donuts sample program that adds 
  force-feedback output if a force-feedback joystick is installed in your 
  computer.

Path
====
  Source: Mssdk\Samples\Multimedia\DInput\Src\FFdonuts

  Executable: Mssdk\Samples\Multimedia\DInput\Bin

User's Guide
============
  When the program is started, you see a dialog box that lets you set the 
  magnitude of the force feedback effects.

  When your ship appears, move the joystick forward to accelerate forward and 
  pull it back to decelerate or move backward. Moving the joystick left or 
  right rotates the ship. Press the trigger button to fire. Press the second 
  button to activate the shields. Observe the force feedback effects as you 
  fire, collide with objects, and bounce off the edge of the screen.

Programming Notes
=================
  The force feedback routines are in Input.c. The program illustrates the use 
  of constant and periodic effects, envelopes, and gain. Note that the 
  fire-button effect is played in response to an ordinary input event rather 
  than being associated with a trigger button in the DIEFFECT structure.

See Also
========
  Space Donuts

