//-----------------------------------------------------------------------------
// 
// Sample Name: StagedConnect Sample
// 
// Copyright (c) 1999 Microsoft Corporation. All rights reserved.
// 
//-----------------------------------------------------------------------------


Description
===========
  StagedConnect illustrates how to network to other players using DirectPlay.  
  After joining or creating a session, the multiplayer stage begins.
  
  Here players may chat which each other, and new players may enter or leave the stage 
  at any time.  The host player has control of when the game begins and who is allowed 
  in the game itself.  The host player may also close off any of the 10 player slots 
  to limit the number of players allowed to join the game.  Each of the players must 
  also check the ready flag next their name in order for the game to begin.  
  
  When everyone is ready and the game begins then no other players are allowed to join
  the game in progress.  Players in the game however may choose to leave the game without
  ill effects the other players.  
  
  If the host player leaves the game during the stage, the game is canceled but if the
  host player leaves after the game has begun then DirectPlay will automatically 
  migrate the host player without ill effects.

  The game itself is very simple, passing a single DirectPlay message to all connected
  players when the "Wave To other players" button is pressed.

Path
====
  Source: Mssdk\Samples\Multimedia\DPlay\Src\StagedConnect

  Executable: Mssdk\Samples\Multimedia\DPlay\Bin

User's Guide
============
  Type in the player's name, and choose a connection type.  Choose "Wait for Lobby
  Connection" for inside-out lobby launching or choose a network provider.  
  Use the Multiplayer Games dialog to either search for an active game to join, 
  or to start a new game.  After a game has been joined or created, the multiplayer stage 
  begins.  Type inside the edit box to send a chat message to everyone.  Flag the checkbox
  next to your name when you are ready to start the game.  
  
  The host player has control over the game while the multipler stage is active.  He may
  reject any player by clicking on a player's name, or close any player slot that is 
  currently empty by clicking on that slot.  When everyone has check their ready flag, then
  the "Start Game" button is actived and the host player may begin the game.  

Programming Notes
=================
  This sample was intended to be very simple, showing the basics of using 
  the DirectPlay API using a multiplayer stage area.  See SimpleConnect for a 
  a similiar sample that excludes the stage. 
