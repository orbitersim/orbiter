VERSION 5.00
Begin VB.Form frmPrivateMessage 
   BackColor       =   &H00000000&
   BorderStyle     =   4  'Fixed ToolWindow
   Caption         =   "Private Message"
   ClientHeight    =   3015
   ClientLeft      =   45
   ClientTop       =   285
   ClientWidth     =   4800
   LinkTopic       =   "Form2"
   LockControls    =   -1  'True
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   3015
   ScaleWidth      =   4800
   ShowInTaskbar   =   1  'True
   StartUpPosition =   3  'Windows Default
   Begin VB.CommandButton cmdPriSend 
      Caption         =   "Send"
      Default         =   -1  'True
      Height          =   255
      Left            =   3840
      TabIndex        =   2
      Top             =   2640
      Width           =   855
   End
   Begin VB.TextBox txtPriSend 
      Appearance      =   0  'Flat
      BackColor       =   &H00404040&
      ForeColor       =   &H80000005&
      Height          =   285
      Left            =   120
      TabIndex        =   1
      Text            =   "Text1"
      Top             =   2640
      Width           =   3615
   End
   Begin VB.TextBox txtPriMsg 
      Appearance      =   0  'Flat
      BackColor       =   &H00404040&
      ForeColor       =   &H80000005&
      Height          =   2415
      Left            =   120
      Locked          =   -1  'True
      MultiLine       =   -1  'True
      ScrollBars      =   2  'Vertical
      TabIndex        =   0
      TabStop         =   0   'False
      Text            =   "Form2.frx":0000
      Top             =   120
      Width           =   4575
   End
End
Attribute VB_Name = "frmPrivateMessage"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private Sub cmdPriSend_Click()
    
    txtPriMsg.Text = txtPriMsg.Text & vbCrLf & PlayerName & ">" & txtPriSend.Text
    txtPriMsg.SelStart = Len(txtPriMsg.Text & vbCrLf & PlayerName & ">" & txtPriSend.Text)
    send_msg PRIVATE_MSG, txtPriSend.Text
    txtPriSend.SetFocus
    txtPriSend.Text = ""
End Sub

Private Sub Form_Load()
    Dim HANDL As Long
    HANDL = SetWindowPos(hwnd, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOSIZE)

    txtPriMsg.Text = ""
    txtPriSend.Text = ""
    'txtPriSend.SetFocus
End Sub

Private Sub Form_QueryUnload(Cancel As Integer, UnloadMode As Integer)
    PRI_FROM = 0
End Sub
