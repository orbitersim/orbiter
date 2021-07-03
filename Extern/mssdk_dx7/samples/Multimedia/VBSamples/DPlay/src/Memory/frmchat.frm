VERSION 5.00
Begin VB.Form frmChat 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Memory Chat"
   ClientHeight    =   1785
   ClientLeft      =   2760
   ClientTop       =   3750
   ClientWidth     =   6030
   BeginProperty Font 
      Name            =   "Verdana"
      Size            =   12
      Charset         =   0
      Weight          =   400
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   1785
   ScaleWidth      =   6030
   ShowInTaskbar   =   1  'True
   StartUpPosition =   1  'CenterOwner
   Begin VB.TextBox txtChat 
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
      Left            =   240
      MaxLength       =   50
      TabIndex        =   0
      Top             =   240
      Width           =   5535
   End
   Begin VB.CommandButton CancelButton 
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
      Left            =   3000
      TabIndex        =   1
      Top             =   960
      Width           =   1215
   End
   Begin VB.CommandButton cmdSend 
      Caption         =   "&Send"
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
      Left            =   4560
      TabIndex        =   2
      Top             =   960
      Width           =   1215
   End
End
Attribute VB_Name = "frmChat"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False

Option Explicit

Private Sub CancelButton_Click()
' When the dialog is closed, unload it so that next time is is reloaded
' with defaults in place, i.e. empty textbox with the focus on it.
  Unload Me
End Sub

Private Sub cmdSend_Click()
  Const MsgLen = 5 + MaxChatString
  Dim MsgData(MsgLen) As Byte
  Dim X As Integer
  Dim ChatMsg As DirectPlayMessage
  
  Set ChatMsg = gObjDPlay.CreateMessage
  Call ChatMsg.WriteLong(MSG_CHAT)
  Call ChatMsg.WriteString(txtChat.Text)
  Call gObjDPlay.Send(gMyPlayerID, DPID_ALLPLAYERS, DPSEND_GUARANTEED, _
          ChatMsg)
  Unload Me  ' See remarks for CancelButtonClick()
End Sub

Private Sub Form_Load()
  ' Make sure the user can't enter more characters than can be accommodated in
  ' our message-handling routines.
  txtChat.MaxLength = MaxChatString
End Sub
