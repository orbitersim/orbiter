VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
END
Attribute VB_Name = "IMTexture"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True

Dim m_ddsd As DDSURFACEDESC2
Dim m_dd As DirectDraw4
Dim m_d3d As Direct3D3
Dim m_d3ddev As Direct3DDevice3
Dim m_width As Integer
Dim m_height As Integer
Public SurfaceObject As DirectDrawSurface4
Public TextureObject As Direct3DTexture2


Const D3DTEXTR_TRANSPARENTBLACK = 1
Const D3DTEXTR_TRANSPARENTWHITE = 2


Function Init(dd As DirectDraw4, d3d As Direct3D3, d3ddev As Direct3DDevice3, sFile As String, extraFlags As Long, w As Long, h As Long)
    Dim bOK As Boolean
    Dim enumTex As Direct3DEnumPixelFormats
    
    Dim sLoadFile As String
    
    Dim i As Long
        
        
    Set m_d3ddev = d3ddev
    Set m_d3d = d3d
    Set m_dd = dd
        
    m_ddsd.lFlags = DDSD_CAPS Or DDSD_TEXTURESTAGE Or DDSD_PIXELFORMAT
    If ((h <> 0) And (w <> 0)) Then
        m_ddsd.lFlags = m_ddsd.lFlags Or DDSD_HEIGHT Or DDSD_WIDTH
        m_ddsd.lHeight = h
        m_ddsd.lWidth = w
    End If
    

     
    Set enumTex = m_d3ddev.GetTextureFormatsEnum()
    
   
    
    For i = 1 To enumTex.GetCount()
        bOK = True
        Call enumTex.GetItem(i, m_ddsd.ddpfPixelFormat)
            
        With m_ddsd.ddpfPixelFormat
            
            If .lRGBBitCount <> 16 Then bOK = False
            If .lFourCC <> 0 Then bOK = False
                        
            
            If ((D3DTEXTR_TRANSPARENTBLACK And extraFlags) _
                  Or (D3DTEXTR_TRANSPARENTWHITE And extraFlags)) Then
                  If (.lRGBAlphaBitMask = 0) Then bOK = False
                  'DDPF_ALPHAPIXELS
            Else
                  If (.lRGBAlphaBitMask <> 0) Then bOK = False
            End If
                  
            
        End With
        If bOK = True Then Exit For
    Next
    
    If bOK = False Then
        Debug.Print "Unable to find 16bit surface support on your hardware - exiting"
        Init = False
    End If
    
    If (((D3DTEXTR_TRANSPARENTBLACK And extraFlags) _
          Or (D3DTEXTR_TRANSPARENTWHITE And extraFlags))) Then
        m_ddsd.ddpfPixelFormat.lFlags = DDPF_ALPHAPIXELS Or DDPF_RGB
        m_ddsd.lFlags = m_ddsd.lFlags Or DDSD_PIXELFORMAT
        
    Else
        m_ddsd.ddpfPixelFormat.lFlags = DDPF_RGB
  
    End If
    
    m_ddsd.ddsCaps.lCaps = DDSCAPS_TEXTURE
    m_ddsd.ddsCaps.lCaps2 = DDSCAPS2_TEXTUREMANAGE
    m_ddsd.lTextureStage = 0
    
        

    
    Set SurfaceObject = m_dd.CreateSurfaceFromFile(sFile, m_ddsd)
    Set TextureObject = SurfaceObject.GetTexture()
    
    
    
    If Not (((D3DTEXTR_TRANSPARENTBLACK = extraFlags) _
        Or (D3DTEXTR_TRANSPARENTWHITE = extraFlags)) _
        ) Then Exit Function

        
        
    Dim ddsd3 As DDSURFACEDESC2
    Dim mem() As Integer
    Dim lAlphaMask As Long
    Dim lRGBMask As Long
    Dim lColorKey As Long
    Dim c As Long
    Dim x As Long
    Dim y As Long
    Dim r1 As RECT
    
    ReDim mem(m_ddsd.lHeight * m_ddsd.lPitch)
                        
    With m_ddsd.ddpfPixelFormat
        lAlphaMask = .lRGBAlphaBitMask
        lRGBMask = .lRBitMask Or .lGBitMask Or .lBBitMask
    End With
    If (extraFlags And D3DTEXTR_TRANSPARENTWHITE) Then
        lColorKey = lRGBMask     'color key on white
    End If
    If (extraFlags And D3DTEXTR_TRANSPARENTBLACK) Then
        lColorKey = 0                  'color key on black
    End If
    
    'pixel format should be 16 bit because thats what we selected
    r1.bottom = m_ddsd.lHeight
    r1.right = m_ddsd.lWidth
    SurfaceObject.Lock r1, ddsd3, DDLOCK_WAIT, 0
    
    ' Add an opaque alpha value to each non-colorkeyed pixel
    For y = 0 To ddsd3.lHeight - 1
        For x = 0 To ddsd3.lWidth - 1
            c = SurfaceObject.GetLockedPixel(x, y)
            If c And lRGBMask <> lColorKey Then
                SurfaceObject.SetLockedPixel x, y, c Or lAlphaMask
            End If
        Next
    Next

    SurfaceObject.Unlock r1
        
    
End Function









