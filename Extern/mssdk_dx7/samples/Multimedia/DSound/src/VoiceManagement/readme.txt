//-----------------------------------------------------------------------------
// 
// Sample Name: VoiceManagement Sample
// 
// Copyright (c) 1999 Microsoft Corporation. All rights reserved.
// 
//-----------------------------------------------------------------------------


Description
===========
  VoiceManagement illustrates how to play a wave file using the voice allocation
  and voice management flags when a DirectSound buffer is played. 

Path
====
  Source: Mssdk\Samples\Multimedia\DSound\Src\VoiceManagement

  Executable: Mssdk\Samples\Multimedia\DSound\Bin

User's Guide
============
  Load sounds and check the flags for desired effect.  Note the "Expect Behavior" 
  field automatically changes as the options change.  Click on 'Play' to hear
  the sound with the desired flags.  

Programming Notes
=================
  The buffer must be created using DSBCAPS_LOCDEFER otherwise DirectSound will
  not be able to dynamically place the buffer in either hardware or software
  at runtime. 

