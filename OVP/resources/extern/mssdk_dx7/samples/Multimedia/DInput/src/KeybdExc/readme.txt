//-----------------------------------------------------------------------------
// 
// Sample Name: KeybdExc Sample
// 
// Copyright (c) 1999 Microsoft Corporation. All rights reserved.
// 
//-----------------------------------------------------------------------------


Description
===========
  The KeybdEsc program obtains the keyboard as a foreground, exclusive
  device and displays keyboard data

Path
====
  Source: Mssdk\Samples\Multimedia\DInput\Src\KeybdExc

  Executable: Mssdk\Samples\Multimedia\DInput\Bin

User's Guide
============
  Hold down one or more keys and the index value of each key (see Keyboard 
  Device Constants) is shown.

Programming Notes
=================
  This sample illustrates how an application can use DirectInput to obtain 
  immediate keyboard data in exclusive mode. Approximately 12 times per second the
  application calls IDirectInputDevice::GetDeviceState and displays a string
  containing the values of all the keys that are down.

