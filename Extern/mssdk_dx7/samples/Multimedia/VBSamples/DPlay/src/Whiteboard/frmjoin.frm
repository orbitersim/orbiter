VERSION 5.00
Begin VB.Form frmJoin 
   BackColor       =   &H00000000&
   BorderStyle     =   4  'Fixed ToolWindow
   Caption         =   "Join"
   ClientHeight    =   4005
   ClientLeft      =   45
   ClientTop       =   285
   ClientWidth     =   4440
   LinkTopic       =   "Form3"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   4005
   ScaleWidth      =   4440
   StartUpPosition =   3  'Windows Default
   Begin VB.TextBox txtIP 
      Appearance      =   0  'Flat
      BackColor       =   &H00404040&
      ForeColor       =   &H80000005&
      Height          =   285
      Left            =   1440
      TabIndex        =   10
      Top             =   2880
      Width           =   2655
   End
   Begin VB.CommandButton cmdExit 
      Caption         =   "E&xit"
      Height          =   255
      Left            =   2520
      TabIndex        =   9
      Top             =   3480
      Width           =   1575
   End
   Begin VB.CommandButton cmdDoneJoin 
      Caption         =   "Finished"
      Default         =   -1  'True
      Height          =   255
      Left            =   240
      TabIndex        =   8
      Top             =   3480
      Width           =   1695
   End
   Begin VB.TextBox txtPassword 
      Appearance      =   0  'Flat
      BackColor       =   &H00404040&
      ForeColor       =   &H80000005&
      Height          =   285
      IMEMode         =   3  'DISABLE
      Left            =   1440
      PasswordChar    =   "*"
      TabIndex        =   6
      Top             =   2520
      Width           =   2655
   End
   Begin VB.TextBox txtNickName 
      Appearance      =   0  'Flat
      BackColor       =   &H00404040&
      ForeColor       =   &H80000005&
      Height          =   285
      Left            =   1440
      TabIndex        =   4
      Top             =   2160
      Width           =   2655
   End
   Begin VB.TextBox txtRealName 
      Appearance      =   0  'Flat
      BackColor       =   &H00404040&
      ForeColor       =   &H80000005&
      Height          =   285
      Left            =   1440
      TabIndex        =   2
      Top             =   1800
      Width           =   2655
   End
   Begin VB.ListBox lstSessions 
      Appearance      =   0  'Flat
      BackColor       =   &H00404040&
      ForeColor       =   &H80000005&
      Height          =   1200
      Left            =   240
      TabIndex        =   0
      Top             =   480
      Width           =   3855
   End
   Begin VB.Label lblIP 
      BackStyle       =   0  'Transparent
      Caption         =   "IP: (blank to search)"
      ForeColor       =   &H0000FF00&
      Height          =   495
      Left            =   240
      TabIndex        =   11
      Top             =   2880
      Width           =   1095
   End
   Begin VB.Label lblPassword 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "Password:"
      ForeColor       =   &H0000FF00&
      Height          =   195
      Left            =   240
      TabIndex        =   7
      Top             =   2520
      Width           =   735
   End
   Begin VB.Label lblNickName 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "Nick Name:"
      ForeColor       =   &H0000FF00&
      Height          =   195
      Left            =   240
      TabIndex        =   5
      Top             =   2160
      Width           =   840
   End
   Begin VB.Label lblRealName 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "Real Name:"
      ForeColor       =   &H0000FF00&
      Height          =   195
      Left            =   240
      TabIndex        =   3
      Top             =   1800
      Width           =   840
   End
   Begin VB.Label lblSessions 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "Current Sessions:"
      ForeColor       =   &H0000FF00&
      Height          =   195
      Left            =   240
      TabIndex        =   1
      Top             =   240
      Width           =   1230
   End
End
Attribute VB_Name = "frmJoin"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private Clicked As Boolean

Private Sub cmdDoneJoin_Click()
    
    If Not Clicked Then
        MsgBox "Please click on the session you want to enter.", vbApplicationModal
        Exit Sub
    End If
    
    
    gIP = txtIP.Text
    
    If dp Is Nothing Then Call InitDplay
    If dpa Is Nothing Then Call InitAddress
    
    
    If sD.GetFlags And DPERR_NONEWPLAYERS Then
            MsgBox "No New Players", vbApplicationModal
            Exit Sub
    ElseIf sD.GetFlags And DPSESSION_JOINDISABLED Then
            MsgBox "Join diabled!", vbApplicationModal
            Exit Sub
    End If
    
    If (checkVals) Then
        sD.SetSessionPassword txtPassword.Text
        dp.Open sD, DPOPEN_JOIN
        createJoinPlayer
    Else
        Exit Sub
    End If
            
    
    
    
    Unload Me
    Call getPlayers(frmMain.lstPlayers)
    Call StartMessageEngine
    frmMain.mnuCommands.Enabled = False
End Sub

Private Sub createJoinPlayer()
    playerfname = txtRealName.Text
    PlayerName = txtNickName.Text
    
    If playerfname = PlayerName Then Exit Sub
    
    player_id = dp.CreatePlayer(PlayerName, playerfname, 0, DPPLAYER_DEFAULT)
End Sub

Private Function checkVals() As Boolean
    '''''''''''' check if all the infoamtion is here
    If txtRealName.Text <> "" Then
        If txtNickName.Text <> "" Then
            checkVals = True
        End If
    End If
    
End Function

Private Sub cmdExit_Click()
    Unload Me
End Sub

Private Sub Form_Load()
    Refresh
    DoEvents
    EnumSessions
    txtPassword.Enabled = False
    lblPassword.Enabled = False
End Sub


Private Sub EnumSessions()
    
    Dim tempSession As DirectPlaySessionData
    Dim i As Integer
    
    If dp Is Nothing Then Call InitDplay
    If dpa Is Nothing Then Call InitAddress
    
    Set tempSession = dp.CreateSessionData()
    tempSession.SetGuidApplication APP_GUID
    Set eS = dp.GetDPEnumSessions(tempSession, 0, DPENUMSESSIONS_ASYNC Or DPENUMSESSIONS_ALL Or DPENUMSESSIONS_PASSWORDREQUIRED)
    DoEvents
    Sleep 1000
    Set eS = dp.GetDPEnumSessions(tempSession, 0, DPENUMSESSIONS_STOPASYNC Or DPENUMSESSIONS_ALL Or DPENUMSESSIONS_PASSWORDREQUIRED)
    
    For i = 1 To eS.GetCount
        lstSessions.AddItem eS.GetItem(i).GetSessionName
    Next i
    
End Sub
    
Private Sub lstSessions_Click()
    '''''''''''' set the session data '''''''''''''
    If dp Is Nothing Then Call InitDplay
    If dpa Is Nothing Then Call InitAddress
    
    Set sD = dp.CreateSessionData()
    Set sD = eS.GetItem(lstSessions.ListIndex + 1)
    
    If sD.GetFlags And DPSESSION_PASSWORDREQUIRED Then
        txtPassword.Enabled = True
        lblPassword.Enabled = True
    Else
        txtPassword.Enabled = False
        lblPassword.Enabled = False
    End If
    
    Clicked = True
End Sub
