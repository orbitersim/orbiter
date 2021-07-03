//-----------------------------------------------------------------------------
// 
// Sample Name: FullDuplexFilter Sample
// 
// Copyright (c) 1999 Microsoft Corporation. All rights reserved.
// 
//-----------------------------------------------------------------------------


Description
===========
  The FullDuplexFilter program demonstrates how to use DirectSound to 
  implement full duplex audio and a filter.

  A microphone or other audio input is required.

Path
====
  Source: \Mssdk\Samples\Multimedia\DSound\Src\FullDuplexFilter

  Executable: \Mssdk\Samples\Multimedia\DSound\Bin

User's Guide
============
  When you start the program, it presents you will next see lists of sampling 
  rates for the input and output buffers. Select an output format, then an 
  input format. 

  The program then shows a dialog box that enables or disable you to select 
  the filter. Click Record to begin capturing and playing sound.

Programming Notes
=================
  FullDuplexFilter is designed to be a simple example of how one might go about 
  implementing full-duplex audio. It is designed primarily to show application 
  developers the proper sequence of steps for dealing with the sound devices 
  installed on the system for using full-duplex audio with waveIn as an input 
  source and DirectSound as the output device.

