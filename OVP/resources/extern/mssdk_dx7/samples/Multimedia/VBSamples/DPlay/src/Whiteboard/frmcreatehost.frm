VERSION 5.00
Begin VB.Form frmCreateHost 
   BackColor       =   &H80000012&
   BorderStyle     =   4  'Fixed ToolWindow
   Caption         =   "Create Host"
   ClientHeight    =   3165
   ClientLeft      =   45
   ClientTop       =   285
   ClientWidth     =   4545
   LinkTopic       =   "Form1"
   LockControls    =   -1  'True
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   3165
   ScaleWidth      =   4545
   ShowInTaskbar   =   1  'True
   StartUpPosition =   3  'Windows Default
   Begin VB.TextBox txtIP 
      Appearance      =   0  'Flat
      BackColor       =   &H00404040&
      ForeColor       =   &H00FFFFFF&
      Height          =   285
      Left            =   2160
      TabIndex        =   1
      Top             =   480
      Width           =   2295
   End
   Begin VB.TextBox txtHostNick 
      Appearance      =   0  'Flat
      BackColor       =   &H00404040&
      ForeColor       =   &H00FFFFFF&
      Height          =   285
      Left            =   2160
      TabIndex        =   5
      Top             =   1920
      Width           =   2295
   End
   Begin VB.TextBox txtHostName 
      Appearance      =   0  'Flat
      BackColor       =   &H00404040&
      ForeColor       =   &H00FFFFFF&
      Height          =   285
      Left            =   2160
      TabIndex        =   4
      Top             =   1560
      Width           =   2295
   End
   Begin VB.TextBox txtMaxPlayers 
      Appearance      =   0  'Flat
      BackColor       =   &H00404040&
      ForeColor       =   &H00FFFFFF&
      Height          =   285
      Left            =   2160
      MaxLength       =   2
      TabIndex        =   2
      Top             =   840
      Width           =   2295
   End
   Begin VB.CommandButton cmdExitHost 
      Caption         =   "E&xit"
      Height          =   255
      Left            =   2520
      TabIndex        =   7
      Top             =   2760
      Width           =   1935
   End
   Begin VB.CommandButton cmdFinishedHost 
      Caption         =   "Finished"
      Default         =   -1  'True
      Height          =   255
      Left            =   120
      TabIndex        =   6
      Top             =   2760
      Width           =   1815
   End
   Begin VB.TextBox txtPassword 
      Appearance      =   0  'Flat
      BackColor       =   &H00404040&
      ForeColor       =   &H00FFFFFF&
      Height          =   285
      Left            =   2160
      TabIndex        =   3
      Top             =   1200
      Width           =   2295
   End
   Begin VB.TextBox txtSessionName 
      Appearance      =   0  'Flat
      BackColor       =   &H00404040&
      ForeColor       =   &H00FFFFFF&
      Height          =   285
      Left            =   2160
      TabIndex        =   0
      Top             =   120
      Width           =   2295
   End
   Begin VB.Label lblIP 
      BackStyle       =   0  'Transparent
      Caption         =   "IP: (leave blank to search)"
      ForeColor       =   &H0000FF00&
      Height          =   255
      Left            =   120
      TabIndex        =   13
      Top             =   480
      Width           =   1935
   End
   Begin VB.Label lblHostNick 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "Host Nick Name:"
      ForeColor       =   &H0000FF00&
      Height          =   195
      Left            =   120
      TabIndex        =   12
      Top             =   1920
      Width           =   1215
   End
   Begin VB.Label lblHostName 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "Host Name:"
      ForeColor       =   &H0000FF00&
      Height          =   195
      Left            =   120
      TabIndex        =   11
      Top             =   1560
      Width           =   840
   End
   Begin VB.Label lblPassword 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "Password:"
      ForeColor       =   &H0000FF00&
      Height          =   195
      Left            =   120
      TabIndex        =   10
      Top             =   1200
      Width           =   735
   End
   Begin VB.Label lblMaxPlayers 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "Max People:"
      ForeColor       =   &H0000FF00&
      Height          =   195
      Left            =   120
      TabIndex        =   9
      Top             =   840
      Width           =   885
   End
   Begin VB.Label lblSessionName 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "Session Name:"
      ForeColor       =   &H0000FF00&
      Height          =   195
      Left            =   120
      TabIndex        =   8
      Top             =   120
      Width           =   1065
   End
End
Attribute VB_Name = "frmCreateHost"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False

Private Sub cmdExitHost_Click()
    Unload Me
End Sub

Private Sub cmdFinishedHost_Click()
    
    
    If dp Is Nothing Then Call InitDplay
    If dpa Is Nothing Then Call InitAddress
    
    If (passCheck()) Then
        Set sD = dp.CreateSessionData()
        
        With sD
            .SetGuidApplication APP_GUID
            .SetMaxPlayers CLng(txtMaxPlayers.Text)
            .SetSessionName txtSessionName.Text
            .SetSessionPassword txtPassword.Text
        End With
    Else
        Exit Sub
    End If
    
    gIP = txtIP.Text
    dp.Open sD, DPOPEN_CREATE
    
    Call CreateHostPlayer

    Unload Me
    Call getPlayers(frmMain.lstPlayers)
    Call StartMessageEngine
    frmMain.mnuCommands.Enabled = False
End Sub
Private Sub CreateHostPlayer()
    PlayerName = txtHostNick.Text
    playerfname = txtHostName.Text
    
    player_id = dp.CreatePlayer(PlayerName, playerfname, 0, DPPLAYER_DEFAULT)
End Sub


Private Function passCheck() As Boolean
    ''''''''''''''' check all the fields
    If txtSessionName.Text <> "" Then
        If txtMaxPlayers.Text <> "" And IsNumeric(txtMaxPlayers.Text) And CLng(Val(txtMaxPlayers.Text)) >= 1 Then
            If txtHostName.Text <> "" Then
                If txtHostNick.Text <> "" Then
                    If txtHostName.Text <> txtHostNick.Text Then
                        passCheck = True
                    End If
                End If
            End If
        End If
    End If
End Function
