Attribute VB_Name = "Globals"
' Network Memory
'
' This sample application demonstrates the use of DirectPlay in a multiplayer game.
'
' The program shows how to:
'
'  -  enumerate connections         (frmMultiplayer)
'  -  initialize a connection       (frmMultiplayer)
'  -  enumerate sessions            (frmSessions)
'  -  create a session              (frmCreateGame)
'  -  join a session                (frmSessions)
'  -  create a player               (frmCreateGame and frmSessions)
'  -  send messages                 (various modules)
'  -  poll for messages in Sub Main (Globals)
'
' The actual game logic is all contained in frmGameBoard.


Option Explicit

' Win32 declares
Public Declare Function GetUserName Lib "advapi32.dll" _
        Alias "GetUserNameA" (ByVal lpBuffer As String, nSize As Long) As Long

' Constants
Public Const NumCells = 36
Public Const MaxPlayers = 4
Public Const MaxChatString = 60
Public Const AppGuid = "{AC330441-9B71-11D2-9AAB-0020781461AC}"

' Application message types
Public Enum MSGTYPES
    MSG_SETUPBOARD
    MSG_SHOWPIECE
    MSG_HIDEPIECES
    MSG_TURNEND
    MSG_CHAT
    MSG_MATCHED
End Enum

' DirectPlay stuff
Public gObjDX As New DirectX7
Public gObjDPlay As DirectPlay4
Public gObjDPEnumSessions As DirectPlayEnumSessions
Public gObjEnumConnections As DirectPlayEnumConnections

Public gMyPlayerID As Long
Public gNumPlayers As Byte
Public gNumPlayersWaiting As Byte
Public gAmHost As Boolean
Public gPicArray(NumCells) As Byte
Public gMatchedCells(NumCells) As Boolean
Public gCurrentPlayer As Integer
Public gPlayerScores(MaxPlayers) As Byte
Public gPlayerIDs(MaxPlayers) As Long      ' Indexed by order of play
Public gMyTurn As Integer
Public gGameUnderway As Boolean

                      

' Get the DirectPlay and EnumConnections objects

Public Sub InitDPlay()
  
  On Error GoTo FAILED
  Set gObjDPlay = gObjDX.DirectPlayCreate("")
  Set gObjEnumConnections = gObjDPlay.GetDPEnumConnections("", DPCONNECTION_DIRECTPLAY)
  Exit Sub
  
FAILED:
  MsgBox ("Failed to initialize DirectPlay.")
  End
End Sub


Public Sub CloseDownDPlay()
  gAmHost = False
  gGameUnderway =False
  Set gObjEnumConnections = Nothing
  set gObjDPEnumSessions = Nothing
  Set gObjDPlay = Nothing
End Sub

' Assign pieces to cells and initialize the state. Done only by the host
' in the multiplayer game. In single-player, can be called to restart.

Public Sub SetupBoard()

  Dim X As Integer
  Dim Pic As Integer
  Dim PicInstance As Integer
  Dim RandCell As Integer

  ' Empty the image index array
  For X = 0 To NumCells - 1
    gPicArray(X) = 0
  Next X
  
  ' Assign pictures to cells
  
  Randomize
  
  ' For every picture except #0, find two empty cells. The two leftover cells
  ' have picture #0 by default.
  ' PicArray indexes the play cells into the image cells stored on the invisible form.
  
  For Pic = 1 To NumCells \ 2 - 1
    For PicInstance = 1 To 2
      Do
        RandCell = Fix(Rnd * NumCells)
      Loop Until gPicArray(RandCell) = 0
      gPicArray(RandCell) = Pic
     Next PicInstance
   Next Pic

End Sub

' Main procedure. This is where we poll for DirectPlay messages in idle time.

Public Sub Main()
  gNumPlayers = 1
  frmMainMenu.Show
  Do While DoEvents()  ' allow event processing while any windows open
    GetDPMessages
  Loop
  
End Sub

' Receive and process DirectPlay Messages

Public Sub GetDPMessages()
  Dim FromPlayerID As Long
  Dim ToPlayerID As Long
  Dim MsgSize As Long
  Dim MsgType As Long
  Dim dpMsg As DirectPlayMessage
  Dim QuitPlayerID As Long
  Dim strPlayer As String
  
  Dim MsgCount As Long
  Dim MsgData() As Byte
  Dim X As Integer
  Dim iCell As Integer
  Dim FromPlayerName As String
  Dim NumPlayersWaiting As Byte
  
  ' No DirectPlay connection, nothing to do. Either it has not been
  'initialized yet or this is a solitaire game.
  If gObjDPlay Is Nothing Then Exit Sub
  
  
  On Error GoTo NOMESSAGE
  ' If this call fails, presumably it's because there's no session or
  ' no player.
  MsgCount = gObjDPlay.GetMessageCount(gMyPlayerID)
  
  On Error GoTo MSGERROR
  
  
  Do While MsgCount > 0
  
    Set dpMsg = gObjDPlay.Receive(FromPlayerID, ToPlayerID, DPRECEIVE_ALL)
    MsgType = dpMsg.ReadLong()
   
   ' Rather than checking the current state of the queue on each pass through
   ' the loop, we'll just do the messages we counted, so DoEvents gets a chance,
   
    MsgCount = MsgCount - 1
      
' ---------------------------------------------------------------------------------------
    ' System messages
    
    If FromPlayerID = DPID_SYSMSG Then
    
      Select Case MsgType
      
        ' New player, update player list
        
        Case DPSYS_DESTROYPLAYERORGROUP, _
             DPSYS_CREATEPLAYERORGROUP

          ' Update the player list
          If frmWaiting.Visible Then frmWaiting.UpdateWaiting
          
          ' If someone has quit...
          If MsgType = DPSYS_DESTROYPLAYERORGROUP Then
            ' Get unwanted player type to advance read pointer
            Dim lPlayerType As Long
            lPlayerType = dpMsg.ReadLong
            ' Get ID of destroyed player
            QuitPlayerID = dpMsg.ReadLong
            
            ' If the game is underway...
            If gGameUnderway Then
            
              'decrement player count
              gNumPlayers = gNumPlayers - 1
              
              ' No opponents left, quit
              If gNumPlayers = 1 Then
                Call MsgBox("All opponents have resigned.")
                Call gObjDPlay.DestroyPlayer(gMyPlayerID)
                gGameUnderway = False
                CloseDownDPlay
                End
              End If
              
              ' If current player quit, advance to next
              If gPlayerIDs(gCurrentPlayer) = QuitPlayerID Then AdvanceTurn
              
              ' Else notify everyone about who quit.
              ' We could extract the player name from the message,
              ' but we'll just get it from our private array.
              For X = 0 To MaxPlayers - 1
                If QuitPlayerID = gPlayerIDs(X) Then
                  strPlayer = frmGameBoard.Frame1(X).Caption
                  ' Flag player's spot in play order as empty
                  gPlayerIDs(X) = 0
                End If
              Next X
              frmGameBoard.lblChat = strPlayer & " has resigned."
              
            End If  ' game underway
            
          End If  ' DESTROYPLAYERORGROUP
          
          ' The host quit, so a new one is being appointed by the system. This matters to us
          ' in the frmSessions form, because only the host can start the game.
          Case DPSYS_HOST
             gAmHost = True
             If frmWaiting.Visible Then
               MsgBox ("You are now the host.")
               frmWaiting.UpdateWaiting   ' make sure Start button is enabled
            End If
          
      End Select
    
' ---------------------------------------------------------------------------------------
    
    ' Application-defined message
    
    Else
    
      ' Get friendly name of sending player
      FromPlayerName = gObjDPlay.GetPlayerFriendlyName(FromPlayerID)
    
      Select Case MsgType
     
        Case MSG_SETUPBOARD
          
          ' Number of players
          gNumPlayers = dpMsg.ReadByte
          ' Play IDs, in play order. Unused players have ID of 0.
          For X = 0 To gNumPlayers - 1
            gPlayerIDs(X) = dpMsg.ReadLong
            If gPlayerIDs(X) = gMyPlayerID Then gMyTurn = X
          Next X
          ' Tile arrangment
          For X = 0 To NumCells - 1
            gPicArray(X) = dpMsg.ReadByte
          Next X
          ' Show the game board. The scoreboard is initialized in the Load method.
          frmWaiting.Hide
          gGameUnderway = True
          frmGameBoard.Show
         
        Case MSG_SHOWPIECE
          ' Show a tile that has been clicked
          iCell = dpMsg.ReadByte
          frmGameBoard.Image1(iCell).Picture = frmPics.Image1(gPicArray(iCell)).Picture
         
         Case MSG_HIDEPIECES
         ' Hide unmatched pieces because player has made the first pick.
           For X = 0 To NumCells - 1
             If Not gMatchedCells(X) Then
               frmGameBoard.Image1(X).Picture = Nothing
             End If
           Next X
           
         Case MSG_MATCHED
           ' Retrieve matched cells array
           For X = 0 To NumCells - 1
             gMatchedCells(X) = dpMsg.ReadByte
           Next X
           ' Retrieve player scores array
           For X = 0 To MaxPlayers - 1
             gPlayerScores(X) = dpMsg.ReadByte
           Next X
           ' Display current score
           frmGameBoard.UpdateScoreboard
         
         Case MSG_TURNEND
           AdvanceTurn
           
        Case MSG_CHAT
          ' Display chat message
          Dim ChatStr$
          ChatStr = FromPlayerName & " says: " _
                  & dpMsg.ReadString()
          frmGameBoard.lblChat.Caption = ChatStr
          
      End Select
   
    End If  ' app-defined message
    
  Loop
  Exit Sub
  
' Error handlers
MSGERROR:
  MsgBox ("Error reading message.")
  CloseDownDPlay
  End
NOMESSAGE:
  Exit Sub
End Sub

Public Sub AdvanceTurn()
  
  If frmGameBoard.Visible Then
    ' Remove highlight from scorebox for last player
    frmGameBoard.Frame1(gCurrentPlayer).ForeColor = vbButtonText
    frmGameBoard.LabelScore(gCurrentPlayer).ForeColor = vbButtonText
  End If
    
  ' Advance the current player. Try till we find one that exists.
  ' Players who resigned are now 0 in gPlayerIDs.
 
  Do
    gCurrentPlayer = gCurrentPlayer + 1
    If gCurrentPlayer = MaxPlayers Then gCurrentPlayer = 0
  Loop Until gPlayerIDs(gCurrentPlayer) <> 0
 
   
  If frmGameBoard.Visible Then
    ' Highlight scorebox for active player
    frmGameBoard.Frame1(gCurrentPlayer).ForeColor = vbHighlight
    frmGameBoard.LabelScore(gCurrentPlayer).ForeColor = vbHighlight
    Call frmGameBoard.UpdateScoreboard
  End If
    
  Exit Sub
  
End Sub

