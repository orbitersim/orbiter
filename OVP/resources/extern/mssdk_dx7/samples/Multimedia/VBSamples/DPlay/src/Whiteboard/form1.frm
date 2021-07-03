VERSION 5.00
Begin VB.Form frmMain 
   BackColor       =   &H00000000&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "White Board"
   ClientHeight    =   5760
   ClientLeft      =   150
   ClientTop       =   720
   ClientWidth     =   12270
   Icon            =   "Form1.frx":0000
   LinkTopic       =   "Form1"
   LockControls    =   -1  'True
   MaxButton       =   0   'False
   ScaleHeight     =   384
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   818
   StartUpPosition =   3  'Windows Default
   Begin VB.ListBox lstPlayers 
      Appearance      =   0  'Flat
      BackColor       =   &H00404040&
      ForeColor       =   &H00FFFFFF&
      Height          =   5295
      Left            =   10200
      TabIndex        =   5
      Top             =   360
      Width           =   1935
   End
   Begin VB.CommandButton cmdPicClear 
      BackColor       =   &H00E0E0E0&
      Caption         =   "Clear"
      Height          =   255
      Left            =   8640
      TabIndex        =   4
      Top             =   5400
      Width           =   1455
   End
   Begin VB.PictureBox pic 
      Appearance      =   0  'Flat
      AutoRedraw      =   -1  'True
      BackColor       =   &H00404040&
      DrawWidth       =   2
      ForeColor       =   &H80000008&
      Height          =   4935
      Left            =   7200
      ScaleHeight     =   327
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   191
      TabIndex        =   3
      Top             =   360
      Width           =   2895
   End
   Begin VB.TextBox txtSend 
      Appearance      =   0  'Flat
      BackColor       =   &H00404040&
      ForeColor       =   &H00FFFFFF&
      Height          =   285
      Left            =   120
      TabIndex        =   2
      Top             =   5370
      Width           =   5775
   End
   Begin VB.Timer tmrMSG 
      Left            =   4920
      Top             =   5760
   End
   Begin VB.CommandButton cmdSend 
      BackColor       =   &H00E0E0E0&
      Caption         =   "Send"
      Default         =   -1  'True
      Height          =   255
      Left            =   6000
      TabIndex        =   1
      Top             =   5400
      Width           =   1095
   End
   Begin VB.TextBox txtMsg 
      Appearance      =   0  'Flat
      BackColor       =   &H00404040&
      ForeColor       =   &H00FFFFFF&
      Height          =   4935
      Left            =   120
      MultiLine       =   -1  'True
      ScrollBars      =   2  'Vertical
      TabIndex        =   0
      Text            =   "Form1.frx":0442
      Top             =   360
      Width           =   6975
   End
   Begin VB.Line Line1 
      X1              =   8
      X2              =   816
      Y1              =   0
      Y2              =   0
   End
   Begin VB.Label Label2 
      BackStyle       =   0  'Transparent
      Caption         =   "Players:"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H0000FF00&
      Height          =   255
      Left            =   10200
      TabIndex        =   8
      Top             =   120
      Width           =   1695
   End
   Begin VB.Label Label1 
      BackStyle       =   0  'Transparent
      Caption         =   "WhiteBoard:"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H0000FF00&
      Height          =   255
      Left            =   7200
      TabIndex        =   7
      Top             =   120
      Width           =   1455
   End
   Begin VB.Label lblLobby 
      BackStyle       =   0  'Transparent
      Caption         =   "Lobby:"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H0000FF00&
      Height          =   255
      Left            =   120
      TabIndex        =   6
      Top             =   120
      Width           =   2055
   End
   Begin VB.Menu mnuFile 
      Caption         =   "File"
      Begin VB.Menu mnuExit 
         Caption         =   "Exit"
      End
   End
   Begin VB.Menu mnuCommands 
      Caption         =   "Commands"
      Begin VB.Menu mnuCreateHost 
         Caption         =   "Create Host"
      End
      Begin VB.Menu mnuJoin 
         Caption         =   "Join"
      End
   End
End
Attribute VB_Name = "frmMain"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private Sub cmdPicClear_Click()
    pic.Cls
End Sub

Private Sub cmdSend_Click()
    
    If Len(txtMsg.Text) > 1000000 Then
        txtMsg = ""
    End If
    
    txtMsg.Text = txtMsg.Text & vbCrLf & PlayerName & ">" & txtSend.Text
    txtMsg.SelStart = Len(txtMsg.Text & vbCrLf & PlayerName & ">" & txtSend.Text)
    
    send_msg LOBBY_MSG, txtSend.Text
    txtSend.SetFocus
    txtSend.Text = ""
End Sub

Private Sub Form_Load()
    pic.ScaleWidth = 100
    pic.ScaleHeight = 100
    Show
    txtMsg.Text = ""
    txtMsg.Locked = True
    txtMsg.TabStop = False
    txtSend.SetFocus
End Sub

Private Sub Form_QueryUnload(Cancel As Integer, UnloadMode As Integer)
    Call CleanUp
    End
End Sub

Private Sub lstPlayers_DblClick()
    PRI_FROM = eP.GetDPID(lstPlayers.ListIndex + 1)
    If PRI_FROM <> player_id Then
        frmPrivateMessage.Show
    Else
        Exit Sub
    End If
End Sub

Private Sub mnuCreateHost_Click()
    frmCreateHost.Show 1
End Sub

Private Sub mnuExit_Click()
    Call CleanUp
    End
End Sub

Private Sub mnuJoin_Click()
    Call JoinChat
End Sub

Private Sub pic_MouseDown(Button As Integer, Shift As Integer, x As Single, y As Single)
    mD = True
End Sub

Private Sub pic_MouseMove(Button As Integer, Shift As Integer, x As Single, y As Single)
    If mD Then
    
        Static sX As Single, sY As Single
        Static lastX As Single
        Static lastY As Single
        
        If x > 100 Then x = 100
        If x < 0 Then x = 0
        If y > 100 Then y = 100
        If y < 0 Then y = 0
        
        lastX = sX: lastY = sY
        sX = x: sY = y
        If lastX = 0 Then lastX = sX: If lastY = 0 Then lastY = sY
        
        
        'pic.Line (lastX, lastY)-(sX, sY)
        pic.PSet (sX, sY), QBColor(10)
        Dim f(1) As Single
        f(0) = sX: f(1) = sY
        send_x MOUSE_MOVE, f()
    Else
        Exit Sub
    End If
End Sub

Private Sub pic_MouseUp(Button As Integer, Shift As Integer, x As Single, y As Single)
    mD = False
End Sub

Private Sub tmrMSG_Timer()
    ''''''' get messages
    Call get_msg
    
End Sub

