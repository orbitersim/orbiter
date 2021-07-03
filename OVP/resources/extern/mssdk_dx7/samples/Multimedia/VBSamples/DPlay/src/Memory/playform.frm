VERSION 5.00
Begin VB.Form frmGameBoard 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Network Memory"
   ClientHeight    =   7200
   ClientLeft      =   3150
   ClientTop       =   2400
   ClientWidth     =   8850
   Icon            =   "PlayForm.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   ScaleHeight     =   7200
   ScaleWidth      =   8850
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
      Height          =   615
      Left            =   6720
      TabIndex        =   9
      Top             =   1740
      Visible         =   0   'False
      Width           =   1995
   End
   Begin VB.Frame Frame1 
      BeginProperty Font 
         Name            =   "Verdana"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   1455
      Index           =   1
      Left            =   6720
      TabIndex        =   3
      Top             =   1760
      Width           =   1935
      Begin VB.Label LabelScore 
         Alignment       =   2  'Center
         Caption         =   "0"
         BeginProperty Font 
            Name            =   "Verdana"
            Size            =   36
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   975
         Index           =   1
         Left            =   120
         TabIndex        =   5
         Top             =   360
         Width           =   1695
      End
   End
   Begin VB.Frame Frame1 
      BeginProperty Font 
         Name            =   "Verdana"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   1455
      Index           =   2
      Left            =   6720
      TabIndex        =   2
      Top             =   3400
      Width           =   1935
      Begin VB.Label LabelScore 
         Alignment       =   2  'Center
         Caption         =   "0"
         BeginProperty Font 
            Name            =   "Verdana"
            Size            =   36
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   975
         Index           =   2
         Left            =   120
         TabIndex        =   6
         Top             =   360
         Width           =   1695
      End
   End
   Begin VB.Frame Frame1 
      BeginProperty Font 
         Name            =   "Verdana"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   1455
      Index           =   3
      Left            =   6720
      TabIndex        =   1
      Top             =   5040
      Width           =   1935
      Begin VB.Label LabelScore 
         Alignment       =   2  'Center
         Caption         =   "0"
         BeginProperty Font 
            Name            =   "Verdana"
            Size            =   36
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   975
         Index           =   3
         Left            =   120
         TabIndex        =   7
         Top             =   360
         Width           =   1695
      End
   End
   Begin VB.Frame Frame1 
      Caption         =   "Turns"
      BeginProperty Font 
         Name            =   "Verdana"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   1455
      Index           =   0
      Left            =   6720
      TabIndex        =   0
      Top             =   120
      Width           =   1935
      Begin VB.Label LabelScore 
         Alignment       =   2  'Center
         Caption         =   "0"
         BeginProperty Font 
            Name            =   "Verdana"
            Size            =   36
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   975
         Index           =   0
         Left            =   120
         TabIndex        =   4
         Top             =   360
         Width           =   1695
      End
   End
   Begin VB.Label lblChat 
      Caption         =   "Press Enter to chat, Alt+F4 to resign."
      BeginProperty Font 
         Name            =   "Verdana"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      Left            =   240
      TabIndex        =   8
      Top             =   6720
      Width           =   8415
   End
   Begin VB.Image Image1 
      BorderStyle     =   1  'Fixed Single
      Height          =   1005
      Index           =   35
      Left            =   5520
      Stretch         =   -1  'True
      Top             =   5520
      Width           =   1005
   End
   Begin VB.Image Image1 
      BorderStyle     =   1  'Fixed Single
      Height          =   1005
      Index           =   34
      Left            =   4440
      Stretch         =   -1  'True
      Top             =   5520
      Width           =   1005
   End
   Begin VB.Image Image1 
      BorderStyle     =   1  'Fixed Single
      Height          =   1005
      Index           =   33
      Left            =   3360
      Stretch         =   -1  'True
      Top             =   5520
      Width           =   1005
   End
   Begin VB.Image Image1 
      BorderStyle     =   1  'Fixed Single
      Height          =   1005
      Index           =   32
      Left            =   2280
      Stretch         =   -1  'True
      Top             =   5520
      Width           =   1005
   End
   Begin VB.Image Image1 
      BorderStyle     =   1  'Fixed Single
      Height          =   1005
      Index           =   31
      Left            =   1200
      Stretch         =   -1  'True
      Top             =   5520
      Width           =   1005
   End
   Begin VB.Image Image1 
      BorderStyle     =   1  'Fixed Single
      Height          =   1005
      Index           =   30
      Left            =   120
      Stretch         =   -1  'True
      Top             =   5520
      Width           =   1005
   End
   Begin VB.Image Image1 
      BorderStyle     =   1  'Fixed Single
      Height          =   1005
      Index           =   29
      Left            =   5520
      Stretch         =   -1  'True
      Top             =   4440
      Width           =   1005
   End
   Begin VB.Image Image1 
      BorderStyle     =   1  'Fixed Single
      Height          =   1005
      Index           =   28
      Left            =   4440
      Stretch         =   -1  'True
      Top             =   4440
      Width           =   1005
   End
   Begin VB.Image Image1 
      BorderStyle     =   1  'Fixed Single
      Height          =   1005
      Index           =   27
      Left            =   3360
      Stretch         =   -1  'True
      Top             =   4440
      Width           =   1005
   End
   Begin VB.Image Image1 
      BorderStyle     =   1  'Fixed Single
      Height          =   1005
      Index           =   26
      Left            =   2280
      Stretch         =   -1  'True
      Top             =   4440
      Width           =   1005
   End
   Begin VB.Image Image1 
      BorderStyle     =   1  'Fixed Single
      Height          =   1005
      Index           =   25
      Left            =   1200
      Stretch         =   -1  'True
      Top             =   4440
      Width           =   1005
   End
   Begin VB.Image Image1 
      BorderStyle     =   1  'Fixed Single
      Height          =   1005
      Index           =   24
      Left            =   120
      Stretch         =   -1  'True
      Top             =   4440
      Width           =   1005
   End
   Begin VB.Image Image1 
      BorderStyle     =   1  'Fixed Single
      Height          =   1005
      Index           =   23
      Left            =   5520
      Stretch         =   -1  'True
      Top             =   3360
      Width           =   1005
   End
   Begin VB.Image Image1 
      BorderStyle     =   1  'Fixed Single
      Height          =   1005
      Index           =   22
      Left            =   4440
      Stretch         =   -1  'True
      Top             =   3360
      Width           =   1005
   End
   Begin VB.Image Image1 
      BorderStyle     =   1  'Fixed Single
      Height          =   1005
      Index           =   21
      Left            =   3360
      Stretch         =   -1  'True
      Top             =   3360
      Width           =   1005
   End
   Begin VB.Image Image1 
      BorderStyle     =   1  'Fixed Single
      Height          =   1005
      Index           =   20
      Left            =   2280
      Stretch         =   -1  'True
      Top             =   3360
      Width           =   1005
   End
   Begin VB.Image Image1 
      BorderStyle     =   1  'Fixed Single
      Height          =   1005
      Index           =   19
      Left            =   1200
      Stretch         =   -1  'True
      Top             =   3360
      Width           =   1005
   End
   Begin VB.Image Image1 
      BorderStyle     =   1  'Fixed Single
      Height          =   1005
      Index           =   18
      Left            =   120
      Stretch         =   -1  'True
      Top             =   3360
      Width           =   1005
   End
   Begin VB.Image Image1 
      BorderStyle     =   1  'Fixed Single
      Height          =   1005
      Index           =   17
      Left            =   5520
      Stretch         =   -1  'True
      Top             =   2280
      Width           =   1005
   End
   Begin VB.Image Image1 
      BorderStyle     =   1  'Fixed Single
      Height          =   1005
      Index           =   16
      Left            =   4440
      Stretch         =   -1  'True
      Top             =   2280
      Width           =   1005
   End
   Begin VB.Image Image1 
      BorderStyle     =   1  'Fixed Single
      Height          =   1005
      Index           =   15
      Left            =   3360
      Stretch         =   -1  'True
      Top             =   2280
      Width           =   1005
   End
   Begin VB.Image Image1 
      BorderStyle     =   1  'Fixed Single
      Height          =   1005
      Index           =   14
      Left            =   2280
      Stretch         =   -1  'True
      Top             =   2280
      Width           =   1005
   End
   Begin VB.Image Image1 
      BorderStyle     =   1  'Fixed Single
      Height          =   1005
      Index           =   13
      Left            =   1200
      Stretch         =   -1  'True
      Top             =   2280
      Width           =   1005
   End
   Begin VB.Image Image1 
      BorderStyle     =   1  'Fixed Single
      Height          =   1005
      Index           =   12
      Left            =   120
      Stretch         =   -1  'True
      Top             =   2280
      Width           =   1005
   End
   Begin VB.Image Image1 
      BorderStyle     =   1  'Fixed Single
      Height          =   1005
      Index           =   11
      Left            =   5520
      Stretch         =   -1  'True
      Top             =   1200
      Width           =   1005
   End
   Begin VB.Image Image1 
      BorderStyle     =   1  'Fixed Single
      Height          =   1005
      Index           =   10
      Left            =   4440
      Stretch         =   -1  'True
      Top             =   1200
      Width           =   1005
   End
   Begin VB.Image Image1 
      BorderStyle     =   1  'Fixed Single
      Height          =   1005
      Index           =   9
      Left            =   3360
      Stretch         =   -1  'True
      Top             =   1200
      Width           =   1005
   End
   Begin VB.Image Image1 
      BorderStyle     =   1  'Fixed Single
      Height          =   1005
      Index           =   8
      Left            =   2280
      Stretch         =   -1  'True
      Top             =   1200
      Width           =   1005
   End
   Begin VB.Image Image1 
      BorderStyle     =   1  'Fixed Single
      Height          =   1005
      Index           =   7
      Left            =   1200
      Stretch         =   -1  'True
      Top             =   1200
      Width           =   1005
   End
   Begin VB.Image Image1 
      BorderStyle     =   1  'Fixed Single
      Height          =   1005
      Index           =   6
      Left            =   120
      Stretch         =   -1  'True
      Top             =   1200
      Width           =   1005
   End
   Begin VB.Image Image1 
      BorderStyle     =   1  'Fixed Single
      Height          =   1005
      Index           =   5
      Left            =   5520
      Stretch         =   -1  'True
      Top             =   120
      Width           =   1005
   End
   Begin VB.Image Image1 
      BorderStyle     =   1  'Fixed Single
      Height          =   1005
      Index           =   4
      Left            =   4440
      Stretch         =   -1  'True
      Top             =   120
      Width           =   1005
   End
   Begin VB.Image Image1 
      BorderStyle     =   1  'Fixed Single
      Height          =   1005
      Index           =   3
      Left            =   3360
      Stretch         =   -1  'True
      Top             =   120
      Width           =   1005
   End
   Begin VB.Image Image1 
      BorderStyle     =   1  'Fixed Single
      Height          =   1005
      Index           =   2
      Left            =   2280
      Stretch         =   -1  'True
      Top             =   120
      Width           =   1005
   End
   Begin VB.Image Image1 
      BorderStyle     =   1  'Fixed Single
      Height          =   1005
      Index           =   1
      Left            =   1200
      Stretch         =   -1  'True
      Top             =   120
      Width           =   1005
   End
   Begin VB.Image Image1 
      BorderStyle     =   1  'Fixed Single
      Height          =   1005
      Index           =   0
      Left            =   120
      Stretch         =   -1  'True
      Top             =   120
      Width           =   1005
   End
End
Attribute VB_Name = "frmGameBoard"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Dim FirstPick As Boolean
Dim FirstCellPicked As Integer
Dim GameOver As Boolean
Dim TurnCount

Private Sub cmdExit_Click()
    Unload Me
End Sub

  ' Keystroke handler
  ' Enter: open Chat dialog
  
Private Sub Form_KeyDown(KeyCode As Integer, Shift As Integer)
  Dim result As Integer
  If (KeyCode = vbKeyReturn) And (gNumPlayers > 1) Then
    frmChat.Show vbModal, Me
  End If
End Sub

Private Sub Form_Load()

  ' Initialize scoreboard
  InitLocalGame
  ' Erase chat prompt if only one player.
  If gNumPlayers = 1 Then
    lblChat.Caption = ""
    cmdExit.Visible = True
  Else
  ' Put user name on caption bar to ease debugging of multiple sessions on one machine
    Caption = Caption & " - " & frmMultiplayer.txtYourName.Text
  End If
End Sub

' If player cancels out of game, terminate program.

Private Sub Form_QueryUnload(Cancel As Integer, UnloadMode As Integer)
  On Error GoTo NO_DPLAY
  Call gObjDPlay.DestroyPlayer(gMyPlayerID)  ' Generates message
  Call gObjDPlay.Close
  CloseDownDPlay
NO_DPLAY:
  End

End Sub


' This is where the action takes place. In each turn the player clicks on two empty squares,
' making their pictures visible. The two pictures revealed in the previous turn are hidden
' as soon as the first square is clicked, unless they are a match. The player can click on
' an unmatched picture to begin the turn, in which case it remains visible.
' A message is broadcast whenever a square is shown or hidden.

Private Sub Image1_MouseDown(Index As Integer, Button As Integer, Shift As Integer, X As Single, Y As Single)
  
  Dim dpMsg As DirectPlayMessage
  Dim GameOver As Boolean
  
  ' Not your turn, bub.
  If gCurrentPlayer <> gMyTurn Then Exit Sub
  
  If Button = vbLeftButton Then
    ' If picture already showing and this is second pick, ignore click.
    ' If picture showing and is already one of a match, ignore click.
    If Image1(Index).Picture <> 0 And ((Not FirstPick) Or gMatchedCells(Index)) Then
      Exit Sub
    End If
    
    If FirstPick Then
     ' Hide previous picks unless they were a match.
      For X = 0 To NumCells - 1
        If Not gMatchedCells(X) Then
          Set Image1(X).Picture = Nothing
        End If
      Next X
      ' Tell the other players to update the display. We don't specify which
      ' squares, but just tell them to hide unmatched squares.
      If gNumPlayers > 1 Then
        Set dpMsg = gObjDPlay.CreateMessage
        Call dpMsg.WriteLong(MSG_HIDEPIECES)
        Call gObjDPlay.Send(gMyPlayerID, DPID_ALLPLAYERS, DPSEND_GUARANTEED, _
                dpMsg)
      End If
    End If
    ' Show the picture you clicked on
    Image1(Index).Picture = frmPics.Image1(gPicArray(Index)).Picture
    ' Broadcast message to show picture
    If gNumPlayers > 1 Then
      Set dpMsg = gObjDPlay.CreateMessage
      Call dpMsg.WriteLong(MSG_SHOWPIECE)
      Call dpMsg.WriteByte(Index)
      Call gObjDPlay.Send(gMyPlayerID, DPID_ALLPLAYERS, DPSEND_GUARANTEED, _
              dpMsg)
      
    End If
    If FirstPick Then
      ' Remember this one
      FirstCellPicked = Index
      FirstPick = False
    Else
      ' Second pick
      FirstPick = True  ' Reset for next time
      ' In solitaire game, show number of turns as score
      If gNumPlayers = 1 Then
        TurnCount = TurnCount + 1
        frmGameBoard.LabelScore(gCurrentPlayer).Caption = TurnCount
      End If
      
      ' Check for match
      If gPicArray(FirstCellPicked) = gPicArray(Index) Then
      
      ' There was a match
      
        gMatchedCells(Index) = True
        gMatchedCells(FirstCellPicked) = True
        
        ' Check for win and increment score (# of matches)
        GameOver = IsGameOver
        ' Increment score display only in multiplayer.
        ' For solitaire, the score is the turn count.
        If gNumPlayers > 1 Then UpdateScoreboard
      
        ' Advise other players of the match
        If Not (gObjDPlay Is Nothing) Then
          Set dpMsg = gObjDPlay.CreateMessage
            
          dpMsg.WriteLong (MSG_MATCHED)
          ' Get array of matched cells into message
          For X = 0 To NumCells - 1
            dpMsg.WriteByte (gMatchedCells(X))
          Next X
            
          ' Get scores into message
          For X = 0 To MaxPlayers - 1
            dpMsg.WriteByte (gPlayerScores(X))
          Next X
            
          ' Send the message
          Call gObjDPlay.Send(gMyPlayerID, DPID_ALLPLAYERS, DPSEND_GUARANTEED, _
                    dpMsg)
        End If ' DirectPlay exists
        
      Else
      ' There was no match.
                  
          ' Broadcast turn-end message
        
        If Not (gObjDPlay Is Nothing) Then
          Set dpMsg = gObjDPlay.CreateMessage
            
          dpMsg.WriteLong (MSG_TURNEND)
            
          ' Send the message
          Call gObjDPlay.Send(gMyPlayerID, DPID_ALLPLAYERS, DPSEND_GUARANTEED, _
                    dpMsg)
                    
          ' Pass control to next player & advance scoreboard highlight
          AdvanceTurn
        End If  ' DirectPlay exists
         
      End If ' match or no match
      
      ' If solitaire win, offer choice to play again
      
      If GameOver And gNumPlayers = 1 Then
        If MsgBox("Play again?", vbYesNo, "Game Over") = vbNo Then End
        SetupBoard
        InitLocalGame
      End If  ' solitaire win
      
    End If ' second pick
    
  End If ' left button
  

End Sub


' Update scores and check for win

Public Function IsGameOver() As Boolean
    
  Dim X As Integer, Response As Integer
  Dim EndGame As Boolean
  
  gPlayerScores(gCurrentPlayer) = gPlayerScores(gCurrentPlayer) + 1
  
  ' If any cells are still blank, game is not over
  EndGame = True
  For X = 0 To NumCells - 1
    If Image1(X).Picture = 0 Then
      EndGame = False
    End If
  Next X
  IsGameOver = EndGame
    
End Function

' Game initialization for all players, including setting up the scoreboard for the
' current number and order of players. Global game initialization (setting up the pieces)
' is handled by the host through SetupBoard.

Public Sub InitLocalGame()

  Dim X As Integer
  
  FirstPick = True
  TurnCount = 0
  
  ' Highlight current player
  gCurrentPlayer = 0
  Frame1(gCurrentPlayer).ForeColor = vbHighlight
  LabelScore(gCurrentPlayer).ForeColor = vbHighlight
  
  ' Hide superfluous scoreboxes and initialize scores
  For X = 0 To MaxPlayers - 1
    gPlayerScores(X) = 0
    If X >= gNumPlayers Then
      Frame1(X).Visible = False
    Else
      Frame1(X).Visible = True
      LabelScore(X).Caption = 0
    End If
  Next X
   
   ' Get names of players and label scoreboxes. The correct order has been
   ' stored in the gPlayerIDs array, which is initialized by the host
   ' and passed to the other players.
   If gNumPlayers > 1 Then
     For X = 0 To gNumPlayers - 1
        Frame1(X).Caption = gObjDPlay.GetPlayerFriendlyName(gPlayerIDs(X))
     Next X
  End If
  
  ' Erase the pictures and matches
  For X = 0 To NumCells - 1
    Image1(X).Picture = Nothing
    gMatchedCells(X) = 0
  Next X
End Sub

Public Sub UpdateScoreboard()
Dim X As Integer

    For X = 0 To gNumPlayers - 1
      frmGameBoard.LabelScore(X).Caption = gPlayerScores(X)
    Next X

End Sub


