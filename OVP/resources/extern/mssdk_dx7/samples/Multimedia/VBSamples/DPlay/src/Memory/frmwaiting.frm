VERSION 5.00
Begin VB.Form frmWaiting 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Waiting for Host to Start Game"
   ClientHeight    =   2400
   ClientLeft      =   45
   ClientTop       =   330
   ClientWidth     =   6450
   Icon            =   "frmWaiting.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   ScaleHeight     =   2400
   ScaleWidth      =   6450
   StartUpPosition =   2  'CenterScreen
   Begin VB.ListBox lstPlayers 
      BeginProperty Font 
         Name            =   "Verdana"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   1260
      ItemData        =   "frmWaiting.frx":0442
      Left            =   600
      List            =   "frmWaiting.frx":0444
      TabIndex        =   3
      Top             =   720
      Width           =   3495
   End
   Begin VB.CommandButton cmdStart 
      Caption         =   "&Start"
      Default         =   -1  'True
      BeginProperty Font 
         Name            =   "Verdana"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   615
      Left            =   4680
      TabIndex        =   2
      Top             =   1384
      Width           =   1575
   End
   Begin VB.CommandButton cmdCancel 
      Caption         =   "&Leave"
      BeginProperty Font 
         Name            =   "Verdana"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   615
      Left            =   4680
      TabIndex        =   1
      Top             =   402
      Width           =   1575
   End
   Begin VB.Label lbStatus 
      Alignment       =   2  'Center
      Caption         =   "lbStatus"
      BeginProperty Font 
         Name            =   "Verdana"
         Size            =   12
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      Left            =   240
      TabIndex        =   0
      Top             =   240
      Width           =   4215
   End
End
Attribute VB_Name = "frmWaiting"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private Sub cmdCancel_Click()
  
  Call gObjDPlay.DestroyPlayer(gMyPlayerID)
  Call gObjDPlay.Close

  CloseDownDPlay
  Unload frmSessions
  Hide
  frmMainMenu.Show

End Sub

' Handle cancel through window close

Private Sub Form_QueryUnload(Cancel As Integer, UnloadMode As Integer)
  cmdCancel_Click
End Sub


' Host starts the game. Set up pieces and notify other players of initial state.

Private Sub cmdStart_Click()
  
  Const MsgSize = 21 + NumCells
  
  Dim dpMsg As DirectPlayMessage
  Dim MsgType As Long
  'Dim PlayerInfo As DPPLAYERINFO
  Dim X As Byte
  Dim objDPEnumPlayers As DirectPlayEnumPlayers
  Dim SessionData As DirectPlaySessionData
  
  ' Disable joining, in case we start before maximum no. of players reached. We
  ' don't want anyone slipping in at the last moment.
  
  Set SessionData = gObjDPlay.CreateSessionData
  Call gObjDPlay.GetSessionDesc(SessionData)    ' necessary?
  Call SessionData.SetFlags(SessionData.GetFlags + DPSESSION_JOINDISABLED)
  Call gObjDPlay.SetSessionDesc(SessionData)
  
  ' Set global player count. This mustn't be done earlier, because someone might
  ' have dropped out or joined just as the host clicked Start.
  
  Set objDPEnumPlayers = gObjDPlay.GetDPEnumPlayers("", 0)
  gNumPlayers = CByte(objDPEnumPlayers.GetCount)
  
  ' Initialize game state
  SetupBoard
  gGameUnderway = True
  
  ' Set play order.
  ' There's no guarantee that players will be enumerated in any particular order by
  ' each instance of GetDPEnumPlayers, so we need to have the host establish play position
  ' and then notify everyone.
   
  Set dpMsg = gObjDPlay.CreateMessage
  
  ' Pack message type
  Call dpMsg.WriteLong(MSG_SETUPBOARD)
  
  ' Pack number of players
  Call dpMsg.WriteByte(gNumPlayers)
  
  ' Pack player IDs.
  Dim PlayerID As Long
  For X = 0 To gNumPlayers - 1
    PlayerID = objDPEnumPlayers.GetDPID(X + 1)
    dpMsg.WriteLong (PlayerID)
    ' Keep local copy of player IDs
    gPlayerIDs(X) = PlayerID
    ' Assign place in order to the host
    If PlayerID = gMyPlayerID Then gMyTurn = X
  Next X
   
  ' Pack tile arrangement
  For X = 0 To NumCells - 1
    dpMsg.WriteByte (gPicArray(X))
  Next X
  
  ' Send message
  Call gObjDPlay.Send(gMyPlayerID, DPID_ALLPLAYERS, DPSEND_GUARANTEED, dpMsg)
    
  ' Switch to the game screen
  Hide
  frmGameBoard.Show
  
End Sub

' Update status info while waiting for game start. Called in response to
' messages when players created or destroyed.

Public Sub UpdateWaiting()
  Dim StatusMsg As String
  Dim X As Integer
  'Dim PlayerInfo As DPPLAYERINFO
  Dim caps As DPCAPS
  Dim objDPEnumPlayers As DirectPlayEnumPlayers
  Dim SessionData As DirectPlaySessionData
  
  ' Enumerate players
  On Error GoTo ENUMERROR
  Set objDPEnumPlayers = gObjDPlay.GetDPEnumPlayers("", 0)
  gNumPlayersWaiting = objDPEnumPlayers.GetCount
  
  ' Update label
  Set SessionData = gObjDPlay.CreateSessionData
  Call gObjDPlay.GetSessionDesc(SessionData)
  StatusMsg = gNumPlayersWaiting & " of " & SessionData.GetMaxPlayers _
          & " players ready..."
  frmWaiting.lbStatus.Caption = StatusMsg
  
  ' Update listbox
  
  lstPlayers.Clear
  
  Dim PlayerName As String
  For X = 1 To gNumPlayersWaiting
    PlayerName = objDPEnumPlayers.GetShortName(X)
    Call lstPlayers.AddItem(PlayerName)
  Next X
    
  ' Disable Start button if not at least two players
  If lstPlayers.ListCount < 2 Then
    cmdStart.Enabled = False
 ' Else enable it if we are the host. Note the host could change if we
 ' enable migration.
  Else
  
    
    If gAmHost Then
        cmdStart.Enabled = True
    End If
  End If
  Set objDPEnumPlayers = Nothing  ' Necessary?
  Exit Sub
  
ENUMERROR:
  MsgBox ("Error in player enumeration.")
  Exit Sub
  
End Sub

