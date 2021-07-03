VERSION 5.00
Object = "*\Armcontrol.vbp"
Begin VB.Form Form1 
   Caption         =   "Test Canvas"
   ClientHeight    =   4980
   ClientLeft      =   45
   ClientTop       =   270
   ClientWidth     =   6600
   Icon            =   "Demo.frx":0000
   LinkTopic       =   "Form1"
   ScaleHeight     =   332
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   440
   StartUpPosition =   3  'Windows Default
   Begin RMControl7Sample.RMCanvas RMCanvas1 
      Height          =   4095
      Left            =   0
      TabIndex        =   0
      Top             =   0
      Width           =   5415
      _ExtentX        =   9551
      _ExtentY        =   7223
   End
   Begin VB.Timer Timer1 
      Left            =   5496
      Top             =   4620
   End
End
Attribute VB_Name = "Form1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Dim framebox  As Direct3DRMFrame3
Dim framesheet  As Direct3DRMFrame3
Dim meshbox As Direct3DRMMeshBuilder3
Dim meshsheet As Direct3DRMMeshBuilder3
Dim texturesheet As Direct3DRMTexture3
Dim texturesurf As DirectDrawSurface4

Private Sub Form_Load()
    Dim b as boolean           
    Me.ScaleMode = 3 'pixel
    Me.Show
    With RMCanvas1
        .UseBackBuffer=TRUE
        b=.InitWindowed("","")
        if not b then 
	  MsgBox "Please run this sample in high color resolution"
          end
        end if
        
        'Create A rotating box
        Set framebox = .D3DRM.CreateFrame(.SceneFrame)
        Set meshbox = .CreateBoxMesh(4, 4, 4)
        meshbox.SetName "Color Box"
        framebox.SetPosition Nothing, -5, 0, 10
        framebox.SetRotation Nothing, 1, 1, 1, 0.05
        
        'Color its faces
        meshbox.GetFace(0).SetColor .dx.CreateColorRGB(1, 0, 0)
        meshbox.GetFace(1).SetColor .dx.CreateColorRGB(0, 1, 0)
        meshbox.GetFace(2).SetColor .dx.CreateColorRGB(0, 0, 1)
        meshbox.GetFace(3).SetColor .dx.CreateColorRGB(1, 1, 0)
        meshbox.GetFace(4).SetColor .dx.CreateColorRGB(0, 1, 1)
        meshbox.GetFace(5).SetColor .dx.CreateColorRGB(1, 1, 1)
        
        
        'Create A sheet with text as the texture
        Set framesheet = .D3DRM.CreateFrame(.SceneFrame)
        Set meshsheet = .CreateSheetMesh(2, 5, 10)
        meshsheet.SetName "Hello World"
        framebox.AddVisual meshbox
        framesheet.AddVisual meshsheet
        framesheet.SetPosition Nothing, 5, 0, 20
        
        'Create a Texture we can draw text to
        'note texture w and h should be power of 2
        
        Set texturesheet = .CreateUpdateableTexture(128, 32, "")
        Set texturesurf = texturesheet.GetSurface(0)
        
        'Draw to the texture
        Dim r(1) As RECT
        texturesurf.SetFillColor vbWhite
        Me.FontSize = 14
        texturesurf.SetFont Me.Font
        texturesurf.SetForeColor vbRed
        texturesurf.setDrawWidth 4
        texturesurf.DrawBox 0, 0, 128, 32
        texturesurf.DrawText 15, 2, "Hello World", False
        texturesheet.Changed D3DRMTEXTURE_CHANGEDPIXELS, 0, r()
                
        'Apply the texure to the sheet
        meshsheet.SetTexture texturesheet
        
        'turn lighting off for the sheet
        meshsheet.SetQuality D3DRMRENDER_UNLITFLAT
        
        'Enable autorotation UI
        Set .RotateFrame = framesheet
               
        
        .Device.SetTextureQuality D3DRMTEXTURE_LINEAR
        
    End With
    
    'Timers are slow but cooperative..
    'for beter performance use doevents loop
    Timer1.Interval = 1
    
    
End Sub


Private Sub Form_Resize()
    RMCanvas1.Width = Me.ScaleWidth
    RMCanvas1.Height = Me.ScaleHeight
End Sub

Private Sub Form_Unload(Cancel As Integer)
    End
End Sub



Private Sub RMCanvas1_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
    Dim meshB As Direct3DRMMeshBuilder3
    Set meshB = RMCanvas1.PickTopMesh(CLng(X), CLng(Y))
    If meshB Is Nothing Then
        Me.Caption = "Test Canvas"
        Exit Sub
    End If
    Form1.Caption = "Over " + meshB.GetName()
End Sub

Private Sub Timer1_Timer()
    RMCanvas1.Update
End Sub
