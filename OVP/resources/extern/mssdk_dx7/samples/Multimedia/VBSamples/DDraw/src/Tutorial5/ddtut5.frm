VERSION 5.00
Begin VB.Form DDDisplayCardInfo 
   Caption         =   "DD Display Card Information"
   ClientHeight    =   6570
   ClientLeft      =   60
   ClientTop       =   345
   ClientWidth     =   4785
   Icon            =   "ddtut5.frx":0000
   LinkTopic       =   "Form1"
   ScaleHeight     =   6570
   ScaleWidth      =   4785
   StartUpPosition =   3  'Windows Default
   Begin VB.ListBox OutList 
      Height          =   6495
      Left            =   0
      TabIndex        =   0
      Top             =   0
      Width           =   4815
   End
End
Attribute VB_Name = "DDDisplayCardInfo"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Dim m_dx As New DirectX7

Private Sub Form_Load()
    Me.Show
    GetDisplayCards
End Sub


Sub GetDisplayModes(sGuid As String)
    'If you want to switch display modes
    'to a certain resolution
    'this is how you figure out what resoultions
    'are supported.
    'note some cards will report zero for
    'the refresh rate.
    
    Dim DisplayModesEnum As DirectDrawEnumModes
    Dim ddsd2 As DDSURFACEDESC2
    Dim dd As DirectDraw7
    Set dd = m_dx.DirectDrawCreate(sGuid)
    dd.SetCooperativeLevel Me.hWnd, DDSCL_NORMAL
    Set DisplayModesEnum = dd.GetDisplayModesEnum(0, ddsd2)
    OutList.AddItem " Display Modes"
    For i = 1 To DisplayModesEnum.GetCount()
        DisplayModesEnum.GetItem i, ddsd2
        OutList.AddItem "  Index          " + Str(i)
        OutList.AddItem "  Width          " + Str(ddsd2.lWidth)
        OutList.AddItem "  Height         " + Str(ddsd2.lHeight)
        OutList.AddItem "  Bits Per Pixel" + Str(ddsd2.ddpfPixelFormat.lRGBBitCount)
        OutList.AddItem "  Refresh Rate   " + Str(ddsd2.lRefreshRate)
        OutList.AddItem ""
    Next
    
    Set dd = Nothing
    
End Sub

Sub GetDisplayCards()
    'Some systems will have multiple display cards
    'or have daughter cards for 3d support.
    'if you want to draw to more than just the
    'primary display or search for 3d hardware not
    'on the primary display card you can
    'use this code to search for such devices
    'Note that windows 98 supports multiple monitors.
    'Note the GUID is what identifies the device
    'and that on the primary display this will return
    'an empty string
    
    Dim ddEnum As DirectDrawEnum
    Dim strGuid As String
    
    Set ddEnum = m_dx.GetDDEnum()
    OutList.AddItem "Display Cards"
    For i = 1 To ddEnum.GetCount()

        
        OutList.AddItem " Index        " + Str(i)
        OutList.AddItem " Description  " + ddEnum.GetDescription(i)
        OutList.AddItem " Name         " + ddEnum.GetName(i)
        OutList.AddItem " GUID         " + ddEnum.GetGuid(i)
        OutList.AddItem ""
        
        strGuid = ddEnum.GetGuid(i)
        GetDDCaps strGuid
        GetD3DDevices strGuid
        GetDisplayModes strGuid
        
    Next
End Sub


Sub GetDDCaps(sGuid As String)
    Dim dd As DirectDraw7
    Dim hwCaps As DDCAPS   'HARDWARE
    Dim helCaps As DDCAPS  'SOFTWARE EMULATION
    
    Set dd = m_dx.DirectDrawCreate(sGuid)
    dd.SetCooperativeLevel Me.hWnd, DDSCL_NORMAL
    
    'Its always a good idea to figure out if the HW
    'supports a feature
    'may un supported features however are emulated via
    'software but are much slower
    'This code provide an example of querying the hw.
    'Note there is a seperate caps call for determining
    '3d capabilities
    
    dd.GetCaps hwCaps, helCaps
    
    'how much video memory is available
    OutList.AddItem "  HW CAPS"
    OutList.AddItem "   total video memory " + Str(hwCaps.lVidMemTotal)
    OutList.AddItem "   free video memory " + Str(hwCaps.lVidMemFree)
    
    
    'Palette Support
    'Most apps dont use palettes since
    'all cards support 16bpp
    'Some apps use 8bpp for speed
    
    lVal = hwCaps.lPalCaps
    If (lVal = 0) Then
        OutList.AddItem "   no hw palette support"
    End If
    If (lVal And DDPCAPS_1BIT) Then
        OutList.AddItem "   palette support 1bpp "
    End If
    If (lVal And DDPCAPS_2BIT) Then
        OutList.AddItem "   palette support 2bit "
    End If
    If (lVal And DDPCAPS_8BIT) Then
        OutList.AddItem "   palette support 8bit "
    End If
    If (lVal And DDPCAPS_8BITENTRIES) Then
        OutList.AddItem "   palette support 8bit entries "
    End If
    If (lVal And DDPCAPS_ALLOW256) Then
        OutList.AddItem "   palette support setting all 256 colors"
    End If
    
    'do we support the gamma ramp interface?
    lVal = hwCaps.ddsCaps.lCaps2
    If lVal And DDCAPS2_CANCALIBRATEGAMMA Then
        OutList.AddItem "   supports gamma correction"
    Else
        OutList.AddItem "   no support for gamma correction"
    End If
    
    
    
    

    Set dd = Nothing
End Sub

Sub GetD3DDevices(sGuid As String)
    Dim d3denum As Direct3DEnumDevices
    
    Dim helDesc As D3DDEVICEDESC7
    Dim hwDesc As D3DDEVICEDESC7
    Dim dd As DirectDraw7
    Dim ddSurf As DirectDrawSurface7
    Dim d3d As Direct3D7
    Set dd = m_dx.DirectDrawCreate(sGuid)
    Set d3d = dd.GetDirect3D()
        
    OutList.AddItem "   D3D devices"
    
    'NOTE its important not to get to bogged down
    'in understand the caps bits. particularly
    'if using the retained mode api.
    
    'things become more important if you want to run
    'use specialized features such as blending and
    'multiple texture stages
    
    'there are cards that dont have a zbuffer
    'that may need there triangles sorted..
    'most cards however have zbuffers
    
    'The color model is the most important aspect in
    'determining speed. software rasterizers provide
    'MONOchormatic lighting for more speed.
    'The sort flags are only important of IM applications
    'that need to work on HW that doesnt support z buffers
    
    Set d3denum = d3d.GetDevicesEnum()
    
    OutList.AddItem ""
    
    For i = 1 To d3denum.GetCount()
        
        d3denum.GetDesc i, hwDesc
        
        
        
        OutList.AddItem "     Guid        " + d3denum.GetGuid(i)
        OutList.AddItem "     Description " + d3denum.GetDescription(i)
        OutList.AddItem "     Name        " + d3denum.GetName(i)
        
        ' you can make
        
        OutList.AddItem "    Device "
        With hwDesc
            OutList.AddItem "     Max Texture Height  " + Str(.lMaxTextureHeight)
            OutList.AddItem "     Max Texture Width   " + Str(.lMaxTextureWidth)
            
                        
            If (.lDeviceRenderBitDepth And DDBD_8) Then
                OutList.AddItem "     Supports rendering to 8 bit"
            End If
            If (.lDeviceRenderBitDepth And DDBD_16) Then
                OutList.AddItem "     Supports rendering to 16 bit"
            End If
            If (.lDeviceRenderBitDepth And DDBD_24) Then
                OutList.AddItem "     Supports rendering to 24 bit"
            End If
            If (.lDeviceRenderBitDepth And DDBD_32) Then
                OutList.AddItem "     Supports rendering to 32 bit"
            End If
            
            
            If (.lDeviceZBufferBitDepth And DDBD_8) Then
                OutList.AddItem "     Supports  8 bit z buffer"
            End If
            If (.lDeviceZBufferBitDepth And DDBD_16) Then
                OutList.AddItem "     Supports  16 bit z buffer"
            End If
            If (.lDeviceZBufferBitDepth And DDBD_24) Then
                OutList.AddItem "     Supports 24 bit z buffer"
            End If
            If (.lDeviceZBufferBitDepth And DDBD_32) Then
                OutList.AddItem "     Supports 32 bit z buffer"
            End If
            If (.lDeviceZBufferBitDepth = 0) Then
                OutList.AddItem "     no z buffer support"
            End If
            

            If (.lDevCaps And D3DDEVCAPS_TEXTURENONLOCALVIDMEM) Then
                OutList.AddItem "     Supports AGP textures"
            End If
            If (.lDevCaps And D3DDEVCAPS_SORTDECREASINGZ) Then
                OutList.AddItem "     IM triangles must be sorted by decreasing depth"
            End If
            If (.lDevCaps And D3DDEVCAPS_SORTDECREASINGZ) Then
                OutList.AddItem "     IM triangles must be sorted exactly"
            End If
            If (.lDevCaps And D3DDEVCAPS_SORTINCREASINGZ) Then
                OutList.AddItem "     IM triangles must be sorted by increasing depth"
            End If
            
            If (.lDevCaps And D3DDEVCAPS_TEXTUREVIDEOMEMORY) Then
                OutList.AddItem "     IM can uses video memory to store textures"
            End If
            
        End With

        
        OutList.AddItem ""
    Next
        
    
    
    
End Sub




Private Sub Form_Resize()
    If Me.Width > 100 Then
        OutList.Width = Me.Width - 100
    End If
    If Me.Height > 400 Then
        OutList.Height = Me.Height - 400
    End If
End Sub

