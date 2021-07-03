VERSION 5.00
Begin VB.Form frmSession 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Chat Room"
   ClientHeight    =   5265
   ClientLeft      =   45
   ClientTop       =   330
   ClientWidth     =   7020
   BeginProperty Font 
      Name            =   "Arial"
      Size            =   9.75
      Charset         =   0
      Weight          =   400
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   ForeColor       =   &H00000000&
   Icon            =   "frmSession.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   ScaleHeight     =   5265
   ScaleWidth      =   7020
   StartUpPosition =   2  'CenterScreen
   Begin VB.CommandButton cmdWhisper 
      Caption         =   "&Whisper"
      Enabled         =   0   'False
      Height          =   495
      Left            =   627
      TabIndex        =   1
      Top             =   4560
      Width           =   1455
   End
   Begin VB.ListBox lstRoster 
      Height          =   3420
      ItemData        =   "frmSession.frx":0442
      Left            =   120
      List            =   "frmSession.frx":0444
      TabIndex        =   4
      Top             =   240
      Width           =   2055
   End
   Begin VB.CommandButton cmdExit 
      Caption         =   "E&xit"
      Height          =   495
      Left            =   4939
      Picture         =   "frmSession.frx":0446
      TabIndex        =   3
      Top             =   4560
      Width           =   1455
   End
   Begin VB.TextBox txtLog 
      Height          =   3420
      Left            =   2400
      Locked          =   -1  'True
      MultiLine       =   -1  'True
      ScrollBars      =   2  'Vertical
      TabIndex        =   5
      TabStop         =   0   'False
      Top             =   240
      Width           =   4455
   End
   Begin VB.TextBox txtEntry 
      BeginProperty Font 
         Name            =   "Verdana"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   375
      Left            =   120
      MaxLength       =   255
      MultiLine       =   -1  'True
      TabIndex        =   0
      Top             =   3960
      Width           =   6735
   End
   Begin VB.CommandButton cmdSend 
      Cancel          =   -1  'True
      Caption         =   "Send to &All"
      Default         =   -1  'True
      DownPicture     =   "frmSession.frx":0888
      Height          =   495
      Left            =   2783
      Picture         =   "frmSession.frx":0CCA
      TabIndex        =   2
      Top             =   4560
      Width           =   1455
   End
End
Attribute VB_Name = "frmSession"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private Sub Form_Unload(Cancel As Integer)
  CloseDownDPlay
  End
End Sub

Private Sub cmdQuit_Click()
  Unload Me
End Sub

Private Sub cmdExit_Click()
  Unload Me
End Sub

Private Sub cmdSend_Click()

  ' Send a broadcast chat message
  
  If txtEntry = "" Then Exit Sub
    
  Dim msg As DirectPlayMessage
  Set msg = objDPlay.CreateMessage
  Call msg.WriteLong(MSG_CHAT)
  Call msg.WriteString(txtEntry)
  Call objDPlay.Send(gMyPlayerID, DPID_ALLPLAYERS, DPSEND_GUARANTEED, msg)
  
  ' Log the message locally
  addToLog gMyPlayerName & ": " & txtEntry
  
  ' Set up the input box for another message
  txtEntry = ""
  txtEntry.SetFocus
  
End Sub


Private Sub cmdWhisper_Click()

  ' Send a private chat message to the selected player
  
  If lstRoster.ListIndex < 0 Then Exit Sub
  If txtEntry = "" Or _
      lstRoster.ItemData(lstRoster.ListIndex) = gMyPlayerID _
      Then Exit Sub
      
  Dim msg As DirectPlayMessage
  Set msg = objDPlay.CreateMessage
  Call msg.WriteLong(MSG_WHISPER)
  Call msg.WriteString(txtEntry)
  Call objDPlay.Send(gMyPlayerID, _
          lstRoster.ItemData(lstRoster.ListIndex), _
          DPSEND_GUARANTEED, msg)
  
  ' Log the message locally
  Call addToLog(gMyPlayerName & " to " & _
          lstRoster.List(lstRoster.ListIndex) & ": " & txtEntry)
          
  ' Set up the input box for another message
  txtEntry = ""
  txtEntry.SetFocus
End Sub


Public Sub UpdateRoster()

  ' Update list of current players
  
  Dim EnumPlayers As DirectPlayEnumPlayers
  Dim i As Integer
  
  On Local Error GoTo FAILED
  lstRoster.Clear
  Set EnumPlayers = objDPlay.GetDPEnumPlayers("", DPENUMPLAYERS_ALL)
  For i = 1 To EnumPlayers.GetCount
    Call lstRoster.AddItem(EnumPlayers.GetShortName(i), i - 1)
    lstRoster.ItemData(i - 1) = EnumPlayers.GetDPID(i)
  Next i
  If lstRoster.ListIndex < 0 Then cmdWhisper.Enabled = False
  Exit Sub
FAILED:
  MsgBox "Failed in UpdateRoster."
  
End Sub

Public Sub addToLog(newText As String)

  ' Add text to log display
  
  txtLog.Text = txtLog.Text & newText & vbCrLf
  
  ' Discard old stuff
  If Len(txtLog.Text) > 10000 Then
     txtLog.Text = Right(txtLog.Text, 9000)
  End If
  txtLog.SelStart = Len(txtLog.Text)  ' to scroll down
  
End Sub

Private Sub lstRoster_Click()

  ' If valid listener selected, enable Whisper
  
  Dim i As Integer
  i = lstRoster.ListIndex
  If (i >= 0) And (lstRoster.ItemData(i) <> gMyPlayerID) Then
    cmdWhisper.Enabled = True
  Else
    cmdWhisper.Enabled = False
  End If
  
End Sub

Private Sub lstRoster_KeyPress(KeyAscii As Integer)
   lstRoster_Click
End Sub

