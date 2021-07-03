VERSION 5.00
Begin VB.Form Instructions 
   Caption         =   "DDBlend"
   ClientHeight    =   3150
   ClientLeft      =   60
   ClientTop       =   345
   ClientWidth     =   4680
   Icon            =   "Instructions.frx":0000
   LinkTopic       =   "Form1"
   ScaleHeight     =   3150
   ScaleWidth      =   4680
   StartUpPosition =   3  'Windows Default
   Begin VB.CommandButton Command1 
      Caption         =   "OK"
      Height          =   375
      Left            =   120
      TabIndex        =   0
      Top             =   2760
      Width           =   4455
   End
   Begin VB.TextBox Text1 
      Height          =   2535
      Left            =   120
      Locked          =   -1  'True
      MultiLine       =   -1  'True
      TabIndex        =   1
      Top             =   120
      Width           =   4455
   End
End
Attribute VB_Name = "Instructions"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private Sub Command1_Click()
    Start = True
    Unload Me
End Sub

Private Sub Form_Load()
    Text1.Text = "Instructions:" & vbCrLf & vbCrLf & _
        "Switch Screen Modes:   Insert/Delete" & vbCrLf & vbCrLf & _
        "Add/Remove Particles:  Home/End" & vbCrLf & vbCrLf & _
        "Modify Blur Factor:         PageUp/PageDown" & vbCrLf & vbCrLf & _
        "Switch Palettes:             Space" & vbCrLf & vbCrLf & _
        "Move the origin with the mouse."
End Sub

Private Sub Form_UnLoad(Cancel As Integer)
        If Not Start Then End
End Sub
