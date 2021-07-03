VERSION 5.00
Object = "{08216199-47EA-11D3-9479-00AA006C473C}#2.0#0"; "RMCONTROL.OCX"
Begin VB.Form Form1 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Direct3DRM Quick Start"
   ClientHeight    =   3495
   ClientLeft      =   45
   ClientTop       =   330
   ClientWidth     =   4860
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   3495
   ScaleWidth      =   4860
   StartUpPosition =   3  'Windows Default
   Begin VB.Timer Timer1 
      Left            =   3000
      Top             =   2400
   End
   Begin RMControl7.RMCanvas RMCanvas1 
      Height          =   3510
      Left            =   0
      TabIndex        =   0
      Top             =   0
      Width           =   4875
      _ExtentX        =   8599
      _ExtentY        =   6191
   End
End
Attribute VB_Name = "Form1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Private Sub Form_Load()
          
          'All objects from the DX library must be
          'strongly typed
          Dim frame As Direct3DRMFrame3
          Dim mesh As Direct3DRMMeshBuilder3
          
          With RMCanvas1
            'Initialize our RMcontrol
            .StartWindowed
            
            'Create a Frame to contain our objects
            'by default its at (0,0,0)
            'Frames are containers for geometry that
            'help position and orient the geometry in a scene
            'we also parent the frame to the scene
            Set frame = .D3DRM.CreateFrame(.SceneFrame)
            
            'Create a MeshBuilder and load an x file into it
            'MeshBuilders hold the actual geometry.
            'please note the meshbuilders can not load
            'files with frame information in them
            'for such files use frame.load
            
            Set mesh = .D3DRM.CreateMeshBuilder()
            mesh.LoadFromFile App.Path + "\egg.x", 0, D3DRMLOAD_FROMFILE, Nothing, Nothing
            
            'here we add the MeshBuilder to the frame
            frame.AddVisual mesh
            
            'we set the frame spinning about the (1,1,1) axis
            'at .04 radians per second
            frame.SetRotation Nothing, 1, 1, 1, 0.04
          End With
          
          'We start a timer
          Timer1.Interval = 1
        End Sub


Private Sub Timer1_Timer()
          'update our simulation
          RMCanvas1.Update
End Sub
        
