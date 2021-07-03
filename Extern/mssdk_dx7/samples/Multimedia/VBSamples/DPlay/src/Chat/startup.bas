Attribute VB_Name = "Startup"
' DXVBChat
' Copyright (c) 1999 Microsoft Corporation
'
' This sample shows how to a DirectPlay application can be launched
' from a lobby.

' In order to test, do the following:

' 1. Run the Lserver.exe lobby server (included with the DirectX SDK)
'    on one machine. Start a normal session.
' 2. Run Bellhop.exe (also in the SDK) on each client machine, and join the
'    lobby session created by Lserver.
' 3. Create a staging area for DXVB Chat in Bellhop on one client.
'    (Right-click on the Chat door and choose CreateGroupInGroup.)
' 4. Enter the staging area on each client by double-clicking on the
'    triangle. Start the session on one client by right-clicking on the
'    triangle and choosing StartSession. This runs DXVBChat on all clients
'    in the staging area, or takes it out of wait mode.
'    Note that new players can join after the session has started by
'    entering the staging area and "starting" it again.

' You can run DXVBChat before starting the session, but you can't run it
' from the Visual Basic environment; you must execute DXVBChat.exe.

' You can test multiple instances of the app on a single machine
' by using TCP/IP. Choose different player names in each instance
' of Bellhop in order to see more clearly what is going on.

' You can also run the program by using the DPLaunch application.
' Note however that DPLaunch does not pass on the session name when
' joining an existing session, therefore the DXVBChat window caption
' will be incomplete.

' ----------------------------------------------------------------------

Option Explicit


Const AppGuid = "{EB5E7E20-0303-11d3-9AAB-00104BCC1EAA}"

        
' DirectPlay stuff
Public objDX As New DirectX7
Public objDPlay As DirectPlay4
Public objDPLobby As DirectPlayLobby3
Public objDPLConnection As DirectPlayLobbyConnection
Public objDPSessionData As DirectPlaySessionData

' Application message types
Public Enum MSGTYPES
    MSG_CHAT
    MSG_WHISPER
End Enum

Public gMyPlayerID As Long
Public gMyPlayerName As String
Public gGotConnection As Boolean
                      
Public Sub Main()
  If Not InitDPlay Then
      MsgBox "Did not initialize DXVBChat."
      End
  End If
  frmSession.Show
  frmSession.UpdateRoster
  Do While DoEvents
    GetDPMessage
  Loop
  
End Sub


Public Function InitDPlay() As Boolean
  
  InitDPlay = True
  
  On Local Error GoTo FAILED
  Set objDPLobby = objDX.DirectPlayLobbyCreate
  
  On Local Error Resume Next
  Set objDPLConnection = objDPLobby.GetConnectionSettings(0)
  
  ' The app was launched by a lobby.
  If Err.Number = 0 Then GoTo GOTCONNECTION
  
  If Err.Number <> DPERR_NOTLOBBIED Then GoTo FAILED
  
  ' The app was not launched by the lobby, therefore we have to
  ' wait till the lobby gives us the connection info.
  
  On Local Error GoTo FAILED
  Call objDPLobby.WaitForConnectionSettings(DPLWAIT_DEFAULT)
  frmWaiting.Show
  WaitForConnectionMessage
  If gGotConnection Then
    frmWaiting.Hide
    Set objDPLConnection = objDPLobby.GetConnectionSettings(0)
  Else
    GoTo FAILED  ' User cancelled
  End If
  
GOTCONNECTION:
  ' We got connection info, either by being launched by the lobby, or
  ' by being patient and waiting for it.
  On Local Error GoTo FAILED
  Set objDPlay = objDPLobby.Connect(0)
  Set objDPSessionData = objDPLConnection.GetSessionDesc
  gMyPlayerName = objDPLConnection.GetPlayerShortName
  gMyPlayerID = objDPlay.CreatePlayer(gMyPlayerName, "PLAYER", 0, DPPLAYER_DEFAULT)
  frmSession.Caption = gMyPlayerName & " in Room " _
      & objDPSessionData.GetSessionName
  Exit Function
FAILED:
  InitDPlay = False
  
End Function

Public Sub CloseDownDPlay()
  Call objDPlay.DestroyPlayer(gMyPlayerID)
    ' Generates message -- redundant since this happens when DPlay destroyed?
  Call objDPlay.Close
  Set objDPlay = Nothing
End Sub


Public Sub WaitForConnectionMessage()
  Dim msg As DirectPlayMessage
  Dim flags As Long
  Dim MsgType As Long
 
  On Local Error Resume Next
  gGotConnection = False
  Do While DoEvents
      Set msg = objDPLobby.ReceiveLobbyMessage(0, flags)
      If Not (msg Is Nothing) Then
        MsgType = msg.ReadLong
        If MsgType = DPLSYS_NEWCONNECTIONSETTINGS Then
          gGotConnection = True
          Exit Do
        End If
      End If
  Loop
  Exit Sub
End Sub


Public Sub GetDPMessage()

' Receive and process DirectPlay Messages

  Dim FromPlayerID As Long
  Dim FromPlayerName As String
  Dim ToPlayerID As Long
  Dim MsgType As Long
  Dim Junk As Long
  Static dpMsg As DirectPlayMessage
  Dim ChangedPlayerID As Long
  
  ' No DirectPlay connection, nothing to do.
  If objDPlay Is Nothing Then Exit Sub
  
  On Local Error GoTo MSGERROR
  
    
  If objDPlay.GetMessageCount(gMyPlayerID) = 0 Then Exit Sub

  Set dpMsg = Nothing
  Set dpMsg = objDPlay.Receive(FromPlayerID, ToPlayerID, DPRECEIVE_ALL)
  If dpMsg Is Nothing Then Exit Sub  ' No message pending
  MsgType = dpMsg.ReadLong()
' ---------------------------------------------------------------------------------------
 ' System messages
 
  If FromPlayerID = DPID_SYSMSG Then
 
    Select Case MsgType
   
      Case DPSYS_DESTROYPLAYERORGROUP, _
           DPSYS_CREATEPLAYERORGROUP

       ' Record change in log
      
       ' Throw away next Long (dwPlayerType)
        Junk = dpMsg.ReadLong
       ' Get ID of player who left or entered
        ChangedPlayerID = dpMsg.ReadLong
       
        If MsgType = DPSYS_CREATEPLAYERORGROUP Then
          Dim Name As String
          Name = objDPlay.GetPlayerFriendlyName(ChangedPlayerID)
          Call frmSession.addToLog(Name & " has entered.")
          
        Else  ' DPSYS_DESTROYPLAYERORGROUP
       
         ' We  get the name from the roster
          Dim i As Integer
          Dim strName As String
         
          For i = 0 To frmSession.lstRoster.ListCount - 1
            If frmSession.lstRoster.ItemData(i) = ChangedPlayerID Then
               strName = frmSession.lstRoster.List(i)
               Exit For
            End If
          Next i
        Call frmSession.addToLog(strName & " has left.")
        End If
             
       ' Update the player list
        frmSession.UpdateRoster
       
    End Select ' System messages
     
' ---------------------------------------------------------------------------------------
 
 ' Application-defined message
 
 Else
 
   ' Get friendly name of sending player
   FromPlayerName = objDPlay.GetPlayerFriendlyName(FromPlayerID)
 
   Select Case MsgType
  
     Case MSG_CHAT, MSG_WHISPER
       
       Dim ChatStr As String
       
       If MsgType = MSG_WHISPER Then
         ChatStr = FromPlayerName & ": (" & dpMsg.ReadString & ")"
       Else
         ChatStr = FromPlayerName & ": " & dpMsg.ReadString
       End If
    
       ' Log the message
       Call frmSession.addToLog(ChatStr)
       
    End Select

  End If  ' app-defined message
 
  
  Exit Sub
  
' Error handlers
MSGERROR:
  MsgBox ("Error reading message. " & Err.Number & " " & Err.Description)
  'CloseDownDPlay
  'End
End Sub

