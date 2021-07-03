VERSION 5.00
Begin VB.Form frmAbout 
   BorderStyle     =   4  'Fixed ToolWindow
   Caption         =   "About"
   ClientHeight    =   2955
   ClientLeft      =   45
   ClientTop       =   285
   ClientWidth     =   3645
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   2955
   ScaleWidth      =   3645
   ShowInTaskbar   =   0   'False
   StartUpPosition =   3  'Windows Default
   Begin VB.Frame fraAbout 
      Height          =   2895
      Left            =   0
      TabIndex        =   0
      Top             =   0
      Width           =   3615
      Begin VB.CommandButton cmdOK 
         Caption         =   "&OK"
         Height          =   375
         Left            =   2280
         TabIndex        =   3
         Top             =   2400
         Width           =   1215
      End
      Begin VB.Image Image1 
         Height          =   480
         Left            =   1080
         Picture         =   "frmAbout.frx":0000
         Top             =   1440
         Width           =   480
      End
      Begin VB.Label lblCopy 
         Caption         =   "(c) 1999 Microsoft"
         Height          =   255
         Left            =   1080
         TabIndex        =   2
         Top             =   960
         Width           =   2055
      End
      Begin VB.Label lblFullDuplex 
         Caption         =   "FullDuplex Sample"
         Height          =   255
         Left            =   1080
         TabIndex        =   1
         Top             =   360
         Width           =   1455
      End
   End
End
Attribute VB_Name = "frmAbout"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private Sub cmdOK_Click()
Unload Me
End Sub
