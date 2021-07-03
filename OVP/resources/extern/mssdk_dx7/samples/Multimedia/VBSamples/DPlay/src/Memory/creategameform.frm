VERSION 5.00
Begin VB.Form frmCreateGame 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Create Game"
   ClientHeight    =   3405
   ClientLeft      =   45
   ClientTop       =   330
   ClientWidth     =   5550
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
   ScaleHeight     =   3405
   ScaleWidth      =   5550
   ShowInTaskbar   =   1  'True
   StartUpPosition =   2  'CenterScreen
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
      Height          =   615
      Left            =   634
      TabIndex        =   6
      Top             =   2520
      Width           =   1575
   End
   Begin VB.CommandButton cmdCreate 
      Caption         =   "&Create"
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
      Left            =   3341
      TabIndex        =   7
      Top             =   2520
      Width           =   1575
   End
   Begin VB.TextBox txtGameName 
      Height          =   375
      Left            =   240
      TabIndex        =   5
      Text            =   "Memory game"
      Top             =   1920
      Width           =   4935
   End
   Begin VB.Frame frmNumber 
      Caption         =   "&Maximum Players"
      BeginProperty Font 
         Name            =   "Verdana"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   1095
      Left            =   240
      TabIndex        =   0
      Top             =   240
      Width           =   3975
      Begin VB.OptionButton Option3 
         Caption         =   "Four"
         Height          =   240
         Left            =   2760
         TabIndex        =   3
         Top             =   480
         Width           =   975
      End
      Begin VB.OptionButton Option2 
         Caption         =   "Three"
         Height          =   240
         Left            =   1680
         TabIndex        =   2
         Top             =   480
         Width           =   975
      End
      Begin VB.OptionButton Option1 
         Caption         =   "Two"
         Height          =   240
         Left            =   600
         TabIndex        =   1
         Top             =   480
         Width           =   975
      End
   End
   Begin VB.Label Label1 
      Caption         =   "&Name of Game"
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
      Left            =   240
      TabIndex        =   4
      Top             =   1560
      Width           =   1815
   End
End
Attribute VB_Name = "frmCreateGame"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit


Private Sub cmdCancel_Click()
  Hide
  frmMainMenu.Show
End Sub

' Create a session and the host player

Private Sub cmdCreate_Click()
  
  Dim SessionData As DirectPlaySessionData
  
  ' Error if no name supplied
  If txtGameName.Text = "" Then
    Call MsgBox("Name of Game cannot be left blank.", 0, "Error")
    Exit Sub
  End If
  
  ' Else create game
  
  Set SessionData = gObjDPlay.CreateSessionData
  
  ' Set number of players
  If Option1.Value = True Then
    Call SessionData.SetMaxPlayers(2)
  ElseIf Option2.Value = True Then
    Call SessionData.SetMaxPlayers(3)
  Else
    Call SessionData.SetMaxPlayers(4)
  End If
  
  ' Finish describing the session
  Call SessionData.SetSessionName(txtGameName.Text)
  Call SessionData.SetGuidApplication(AppGuid)
  Call SessionData.SetFlags(DPSESSION_MIGRATEHOST)
  
  ' Create (and join) the session.
  
  ' Failure can result from the user cancelling out of the service provider dialog.
  ' In the case of the modem, this is the "answer" dialog.
  On Error GoTo FAILEDOPEN
  Call gObjDPlay.Open(SessionData, DPOPEN_CREATE)
  
  ' Describe the host player
  Dim PlayerHandle As String
  Dim PlayerName As String
  
  PlayerName = frmMultiplayer.txtYourName.Text
  PlayerHandle = "Player 1 (Host)"
  
  ' Create the host player
  On Error GoTo FAILEDCREATEPLAYER
  gMyPlayerID = gObjDPlay.CreatePlayer(PlayerName, PlayerHandle, 0, 0)
  On Error GoTo 0
    
  gAmHost = True
  

  ' Close this form and open the session status dialog
  Hide
  frmWaiting.UpdateWaiting
  frmWaiting.Show
  Exit Sub
  
  ' Error handlers
  
FAILEDOPEN:
  MsgBox ("Failed to create game.")
  Exit Sub
FAILEDCREATEPLAYER:
  Call MsgBox("Failed to create player.")
  
  If Err.Number = DPERR_INVALIDPLAYER Then MsgBox ("INVALIDPLAYER")
  Exit Sub
  
End Sub

Private Sub Form_Load()
  ' Set Four Player option button by default
  Option3.Value = True
End Sub

