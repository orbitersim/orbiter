//-----------------------------------------------------------------------------
// 
// Sample Name: ChatConnect Sample
// 
// Copyright (c) 1999 Microsoft Corporation. All rights reserved.
// 
//-----------------------------------------------------------------------------


Description
===========
  ChatConnect illustrates how to network to other players on the 
  using DirectPlay.  After joining or creating a session, a simple chat sesion
  begins immediately.  Other players may join the chat in progress at any time.  
  
  The chat itself is very simple, sending a DirectPlay chat message to all connected
  players.

Path
====
  Source: Mssdk\Samples\Multimedia\DPlay\Src\SimpleConnect

  Executable: Mssdk\Samples\Multimedia\DPlay\Bin

User's Guide
============
  Enter the player's name, and choose a connection type.  Choose "Wait for Lobby
  Connection" for inside-out lobby launching or choose a network provider.  
  Use the Multiplayer Games dialog to either search for an active game to join, 
  or to start a new game.  After game has been joined or created, the chat begins 
  immediately.  Other players may join the chat at any time.  The host player may
  also leave at anytime since the DirectPlay automatically migrates the host player.

Programming Notes
=================
  This sample was intended to be very simple, showing the basics of using 
  the DirectPlay API for the purpose of a simple chat session.
