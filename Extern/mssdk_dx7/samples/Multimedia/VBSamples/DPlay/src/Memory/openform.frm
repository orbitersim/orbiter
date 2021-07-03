VERSION 5.00
Begin VB.Form frmMainMenu 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Memory"
   ClientHeight    =   4965
   ClientLeft      =   1080
   ClientTop       =   1530
   ClientWidth     =   8610
   Icon            =   "OpenForm.frx":0000
   LinkTopic       =   "Form2"
   MaxButton       =   0   'False
   ScaleHeight     =   4965
   ScaleWidth      =   8610
   StartUpPosition =   2  'CenterScreen
   Begin VB.CommandButton cmdExit 
      Cancel          =   -1  'True
      Caption         =   "E&xit"
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
      Left            =   1118
      TabIndex        =   3
      Top             =   3480
      Width           =   1575
   End
   Begin VB.CommandButton cmdMulti 
      Caption         =   "&Multiplayer"
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
      Height          =   1095
      Left            =   3518
      TabIndex        =   1
      Top             =   3480
      Width           =   1575
   End
   Begin VB.CommandButton cmdSolitaire 
      Caption         =   "&Solitaire"
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
      Left            =   5918
      TabIndex        =   2
      Top             =   3480
      Width           =   1575
   End
   Begin VB.Label Label1 
      Alignment       =   2  'Center
      Caption         =   "Network Memory"
      BeginProperty Font 
         Name            =   "Verdana"
         Size            =   48
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   -1  'True
         Strikethrough   =   0   'False
      EndProperty
      Height          =   2535
      Left            =   960
      TabIndex        =   0
      Top             =   480
      Width           =   6375
   End
End
Attribute VB_Name = "frmMainMenu"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private Sub cmdCreate_Click()
End Sub

Private Sub cmdExit_Click()
  CloseDownDPlay
  End
End Sub

Private Sub cmdJoin_Click()
End Sub

Private Sub cmdMulti_Click()
  Hide
  frmMultiplayer.Show
  frmMultiplayer.lstConnections.SetFocus
End Sub

Private Sub cmdSolitaire_Click()
  ' Initialize game state
  Set gObjDPlay = Nothing
  SetupBoard
  Hide
  gNumPlayers = 1
  frmGameBoard.Show
End Sub


' Called when window is closed by system button

Private Sub Form_Unload(Cancel As Integer)
  CloseDownDPlay
  End
End Sub
