VERSION 5.00
Begin VB.Form frmSessions 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Join or Create Game"
   ClientHeight    =   4440
   ClientLeft      =   45
   ClientTop       =   330
   ClientWidth     =   8040
   BeginProperty Font 
      Name            =   "Verdana"
      Size            =   9.75
      Charset         =   0
      Weight          =   400
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   4440
   ScaleWidth      =   8040
   StartUpPosition =   2  'CenterScreen
   Begin VB.CommandButton cmdRefresh 
      Caption         =   "&Refresh"
      BeginProperty Font 
         Name            =   "Verdana"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   735
      Left            =   5880
      TabIndex        =   2
      Top             =   360
      Width           =   1695
   End
   Begin VB.CommandButton cmdCancel 
      Cancel          =   -1  'True
      Caption         =   "Cancel"
      BeginProperty Font 
         Name            =   "Verdana"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   735
      Left            =   5880
      TabIndex        =   5
      Top             =   3360
      Width           =   1695
   End
   Begin VB.CommandButton cmdCreate 
      Caption         =   "&New"
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
      Height          =   735
      Left            =   5880
      TabIndex        =   4
      Top             =   2360
      Width           =   1695
   End
   Begin VB.CommandButton cmdJoin 
      Caption         =   "&Join"
      BeginProperty Font 
         Name            =   "Verdana"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   735
      Left            =   5880
      TabIndex        =   3
      Top             =   1360
      Width           =   1695
   End
   Begin VB.ListBox lstSessions 
      Height          =   3180
      ItemData        =   "SessionForm.frx":0000
      Left            =   240
      List            =   "SessionForm.frx":0002
      TabIndex        =   1
      Top             =   600
      Width           =   5295
   End
   Begin VB.Label Label1 
      Caption         =   "&Available games:"
      BeginProperty Font 
         Name            =   "Verdana"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      Left            =   360
      TabIndex        =   0
      Top             =   120
      Width           =   3135
   End
End
Attribute VB_Name = "frmSessions"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private Sub cmdCancel_Click()
  Hide
  Unload frmMultiplayer  ' Force reset of DPlay
  frmMainMenu.Show
End Sub

Private Sub cmdRefresh_Click()
    Screen.MousePointer = vbHourglass
    UpdateSessionList
    Screen.MousePointer = vbNormal
End Sub

Private Sub Form_Unload(Cancel As Integer)
  cmdCancel_Click
End Sub

Private Sub cmdCreate_Click()
  Hide
  frmCreateGame.Show
    ' Put focus on name
  frmCreateGame.txtGameName.SetFocus

End Sub

' Join game

Private Sub cmdJoin_Click()

  Dim SessionData As DirectPlaySessionData
  
  ' Join selected session
  Set SessionData = gObjDPEnumSessions.GetItem(lstSessions.ListIndex + 1)
  On Error GoTo NOSESSION
  Call gObjDPlay.Open(SessionData, DPOPEN_JOIN)
  On Error GoTo 0
  
  ' Create player
  Dim PlayerHandle As String
  Dim PlayerName As String
  
  PlayerName = frmMultiplayer.txtYourName.Text
  PlayerHandle = "Player"    ' We don't use this
  
  ' Create the player
  On Error GoTo FAILEDCREATE
  gMyPlayerID = gObjDPlay.CreatePlayer(PlayerName, PlayerHandle, 0, 0)
  On Error GoTo 0
  
  ' Only host can start game, so disable Start button
  frmWaiting.cmdStart.Enabled = False
  
  ' Hide this and show status window
  Hide
  frmWaiting.Show
  frmWaiting.UpdateWaiting  ' show the player list
  Exit Sub

' Error handlers

NOSESSION:
  MsgBox ("Failed to join game.")
  UpdateSessionList
  Exit Sub
  
FAILEDCREATE:
  MsgBox ("Failed to create player.")
  Exit Sub
  
End Sub


' Update session listbox.

Public Function UpdateSessionList() As Boolean
  
  Dim SessionCount As Integer, X As Integer
  Dim SessionData As DirectPlaySessionData
  Dim Details As String
  
  ' Delete the old list
  lstSessions.Clear
  
  Set SessionData = gObjDPlay.CreateSessionData
  
  ' Enumerate the sessions in synchronous mode.
  Call SessionData.SetGuidApplication(AppGuid)
  Call SessionData.SetSessionPassword("")
  
  On Error GoTo USERCANCEL
  Set gObjDPEnumSessions = gObjDPlay.GetDPEnumSessions(SessionData, 0, _
          DPENUMSESSIONS_AVAILABLE)
  On Error GoTo 0
 
  ' List info for enumerated sessions: name, players, max. players
  
  On Error GoTo ENUM_ERROR
  SessionCount = gObjDPEnumSessions.GetCount
  For X = 1 To SessionCount
    Set SessionData = gObjDPEnumSessions.GetItem(X)
    Details = SessionData.GetSessionName & " (" & SessionData.GetCurrentPlayers _
            & "/" & SessionData.GetMaxPlayers & ")"
    lstSessions.AddItem (Details)
  Next X
  
  ' Update Join button
  If SessionCount = 0 Then
    cmdJoin.Enabled = False
  Else
    cmdJoin.Enabled = True
  End If
  
  ' Initialize selection
  If lstSessions.ListCount > 0 Then lstSessions.ListIndex = 0
  
  UpdateSessionList = True
  Exit Function
  
  ' Error handlers
  ' User cancelled out of service provider dialog, e.g. for modem connection.
  ' We can't enumerate sessions but the user can still host a game.
USERCANCEL:
  UpdateSessionList = False
  cmdJoin.Enabled = False
  Exit Function
  
ENUM_ERROR:
  UpdateSessionList = False
  MsgBox ("Error in enumeration functions.")
  Exit Function
  
End Function

