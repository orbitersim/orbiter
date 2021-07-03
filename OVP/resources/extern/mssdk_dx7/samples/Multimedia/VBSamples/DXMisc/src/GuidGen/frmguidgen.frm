VERSION 5.00
Begin VB.Form frmGUIDGen 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Create GUID"
   ClientHeight    =   2595
   ClientLeft      =   45
   ClientTop       =   330
   ClientWidth     =   5070
   Icon            =   "frmGUIDGen.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   2595
   ScaleWidth      =   5070
   StartUpPosition =   2  'CenterScreen
   Begin VB.Frame fraResult 
      Caption         =   "Result"
      Height          =   975
      Left            =   60
      TabIndex        =   4
      Top             =   1440
      Width           =   4935
      Begin VB.Label lblGUID 
         BackStyle       =   0  'Transparent
         Caption         =   "lblGUID"
         Height          =   255
         Left            =   120
         TabIndex        =   5
         Top             =   240
         Width           =   4695
      End
   End
   Begin VB.CommandButton cmdExit 
      Cancel          =   -1  'True
      Caption         =   "E&xit"
      Height          =   315
      Left            =   3840
      TabIndex        =   3
      Top             =   1020
      Width           =   1035
   End
   Begin VB.CommandButton cmdNew 
      Caption         =   "&New GUID"
      Height          =   315
      Left            =   3840
      TabIndex        =   2
      Top             =   600
      Width           =   1035
   End
   Begin VB.CommandButton cmdCopy 
      Caption         =   "&Copy"
      Default         =   -1  'True
      Height          =   315
      Left            =   3840
      TabIndex        =   1
      Top             =   180
      Width           =   1035
   End
   Begin VB.Label lblIntro 
      BackStyle       =   0  'Transparent
      Caption         =   $"frmGUIDGen.frx":0442
      Height          =   675
      Left            =   60
      TabIndex        =   0
      Top             =   180
      Width           =   3615
   End
End
Attribute VB_Name = "frmGUIDGen"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Private DX7 As DirectX7

Private Sub NewGuid()
    'Generate a new GUID and display it to the user
    lblGUID.Caption = DX7.CreateNewGuid
End Sub

Private Sub cmdCopy_Click()
    'Clear any previous contents of the clipboard and copy the current GUID there.
    Clipboard.Clear
    Clipboard.SetText lblGUID.Caption
End Sub

Private Sub cmdExit_Click()
    Unload Me
End Sub

Private Sub cmdNew_Click()
    NewGuid
End Sub

Private Sub Form_Load()
    Set DX7 = New DirectX7
    NewGuid
End Sub

Private Sub Form_Unload(Cancel As Integer)
    Set DX7 = Nothing
End Sub
