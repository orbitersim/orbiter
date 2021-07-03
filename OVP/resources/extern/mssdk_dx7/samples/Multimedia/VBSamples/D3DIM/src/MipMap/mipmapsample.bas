Attribute VB_Name = "MipMapSample"
Option Explicit
Dim MipMap As DirectDrawSurface7
Dim TempMipMap As DirectDrawSurface7
Dim TempSurface As DirectDrawSurface7
Dim MipMapDesc As DDSURFACEDESC2
Dim tempdesc As DDSURFACEDESC2
Dim Mesh(3) As D3DVERTEX
Dim Rects(0) As D3DRECT
Dim Index As Double
Dim DirFoward As Boolean
Dim PixelFormat As DDPIXELFORMAT
Dim SourceRect As RECT
Dim DestRect As RECT
Dim d3ddevdesc As D3DDEVICEDESC7
Dim DDSC As DDSCAPS2
Dim lastT As Long
Dim thisT As Long
Dim delta As Single


Sub Main()

    InitDX
    InitDeviceObjects
    InitMipMap
    
    Dim fRestore As Boolean
    Do Until MustExit ' Main Loop
	on local error resume next                
        fRestore = False
        While MipMapForm.IMCanvas1.DirectDraw.TestCooperativeLevel <> DD_OK
            fRestore = True
            DoEvents
        Wend
        If fRestore Then
            MipMapForm.IMCanvas1.DirectDraw.RestoreAllSurfaces
            InitMipMap
        End If
        
        'compute how much time has passed
        thisT = DX.TickCount()
        If lastT <> 0 Then
           Index = Index + CDbl(thisT - lastT) / 400
        End If
        lastT = thisT
        
        'check for boundary conditions
        If Index < 2 Or Index > 18 Then
           Index = 2
           DirFoward = Not DirFoward
        End If
        
        'clear the backbuffer and zbuffer
        MipMapForm.IMCanvas1.ClearBackSurface
        
        'translate the billboards along the z axis
        If DirFoward Then
            TranslateMatrix MatWorld1, RVector(-1.1, 0, CSng(Index))
        Else
            TranslateMatrix MatWorld1, RVector(-1.1, 0, CSng(20 - Index))
        End If
        
            
       ' draw a bilinear filtered mesh
        With d3ddev
            .SetTransform D3DTRANSFORMSTATE_WORLD, MatWorld1
            .SetTextureStageState 0, D3DTSS_MIPFILTER, D3DTFP_NONE
            .BeginScene
            .DrawPrimitive D3DPT_TRIANGLESTRIP, D3DFVF_VERTEX, _
                Mesh(0), 4, D3DDP_DEFAULT
            .EndScene
            
        End With
        
        If DirFoward Then
            TranslateMatrix MatWorld1, RVector(1.1, 0, CSng(Index))
        Else
            TranslateMatrix MatWorld1, RVector(1.1, 0, 20 - Index)
        End If
        
        ' draw a bilinear filtered, mipmapped mesh
        With d3ddev
            .SetTransform D3DTRANSFORMSTATE_WORLD, MatWorld1
            .SetTextureStageState 0, D3DTSS_MIPFILTER, D3DTFP_LINEAR
                
            .BeginScene
            .DrawPrimitive D3DPT_TRIANGLESTRIP, D3DFVF_VERTEX, _
                Mesh(0), 4, D3DDP_DEFAULT
            .EndScene
        End With
        
        MipMapForm.IMCanvas1.Update
        DoEvents
            
        
    Loop
    
    IM7Terminate
    
End Sub

 ' FindTextureFormat will find and pass back a valid mipmapping pixel format
 ' back through the PixelFormat structure.
 Private Function FindTextureFormat(BitCount As Long, PixelFormat As DDPIXELFORMAT) As Boolean
    With d3ddev.GetTextureFormatsEnum
        'loop through all of our possible pixel formats
        For Index = 1 To .GetCount
            .GetItem Index, PixelFormat
            With PixelFormat
                If .lRGBBitCount = BitCount Then
                    If Not (.lFlags And (DDPF_LUMINANCE _
                      Or DDPF_ALPHAPIXELS)) Then
                        If Not (.lFlags And (DDPF_BUMPLUMINANCE _
                          Or DDPF_BUMPDUDV)) Then
                            If .lFourCC = 0 Then
                                FindTextureFormat = True
                                Exit Function
                            End If
                        End If
                    End If
                End If
            End With
        Next
    End With
End Function

Sub FindMediaDir(sFile As String)
    On Local Error Resume Next
    If Dir$(sFile) <> "" Then Exit Sub
    If Mid$(App.Path, 2, 1) = ":" Then
        ChDrive Mid$(App.Path, 1, 1)
    End If
    ChDir App.Path
    If Dir$(sFile) = "" Then
        ChDir "..\media"
    End If
    If Dir$(sFile) = "" Then
        ChDir "..\..\media"
    End If
End Sub

Sub InitDeviceObjects()
            Dim c As D3DCOLORVALUE
            Dim lProp As D3DLIGHT7
            Dim Index As Long
            Dim material1 As D3DMATERIAL7
            
            With MipMapForm.IMCanvas1
                    Set DDraw = .DirectDraw
                    Set d3ddev = .Direct3DDevice
            End With
    
            
            With DX
                .IdentityMatrix MatWorld1
                .IdentityMatrix matView1
                .IdentityMatrix matProj1
                .ViewMatrix matView1, RVector(0, 0, 0), RVector(0, 0, 100), RVector(0, 1, 0), 0
                .ProjectionMatrix matProj1, 0, 1, 1.57
            End With
    
            d3ddev.SetTransform D3DTRANSFORMSTATE_WORLD, MatWorld1
            d3ddev.SetTransform D3DTRANSFORMSTATE_VIEW, matView1
            d3ddev.SetTransform D3DTRANSFORMSTATE_PROJECTION, matProj1
    

            'Setup Lighting
            With c
                .a = 1
                .r = 1
                .g = 1
                .b = 1
            End With
            
            With lProp
                .dltType = D3DLIGHT_POINT
                .Ambient = c
                .diffuse = c
                .specular = c
                .position = RVector(0, 100, 0)
            End With
            d3ddev.SetLight 0, lProp
            d3ddev.LightEnable 0, True
                
            
            material1.Ambient = c
            material1.diffuse = c
            material1.emissive = c
            material1.power = 1
            d3ddev.SetMaterial material1


End Sub


Sub InitMipMap()
     
    ' obtain a valid a 16-bit pixel format
    If Not FindTextureFormat(16, PixelFormat) Then
    ' if that wasn't found, check for a 32-bit format
        If Not FindTextureFormat(32, PixelFormat) Then
            MsgBox "Couldn't find a valid pixel format."
            IM7Terminate
        End If
    End If
    
    ' setup the mipmap's description
    ' You can set the lMipMapcount to a lower value if you like.
    ' In this particular case, valid value range from 1 to 5.
    With MipMapDesc
        LSet .ddpfPixelFormat = PixelFormat
        .lFlags = DDSD_CAPS Or DDSD_PIXELFORMAT Or DDSD_MIPMAPCOUNT Or DDSD_WIDTH Or DDSD_HEIGHT
        .lMipMapCount = 5
        .ddsCaps.lCaps = DDSCAPS_TEXTURE Or DDSCAPS_MIPMAP Or DDSCAPS_COMPLEX
	.ddsCaps.lCaps2 = 0
        .lWidth = 128
        .lHeight = 128
    End With
    
    if Ucase(d3ddev.GetDeviceGuid)="IID_IDIRECT3DHALDEVICE" then
	MipMapDesc.ddsCaps.lCaps2=DDSCAPS2_TEXTUREMANAGE
    else
	MipMapDesc.ddsCaps.lCaps=MipMapDesc.ddsCaps.lCaps or DDSCAPS_SYSTEMMEMORY
    endif

    ' Create the base mipmap surface.  A chain of surfaces will be created, so
    ' we want to create a temporary surface which we'll use to access each
    ' surface in that chain.
    Set MipMap = DDraw.CreateSurface(MipMapDesc)
    Set TempMipMap = MipMap
    Index = 0
    
    
    Do ' loop until all the mipmaps have been created
        Dim newdesc As DDSURFACEDESC2
        TempMipMap.GetSurfaceDesc tempdesc
        newdesc.ddsCaps.lCaps = DDSCAPS_OFFSCREENPLAIN Or DDSCAPS_SYSTEMMEMORY
        newdesc.lFlags = DDSD_CAPS
        FindMediaDir "brick0.bmp"
        ' load a new image into the current mipmap's surface
        Set TempSurface = DDraw.CreateSurfaceFromFile _
            ("brick" & Trim$(Str$(Index)) & ".bmp", MipMapDesc)
             
        DestRect.right = tempdesc.lWidth
        DestRect.Bottom = tempdesc.lHeight
        SourceRect.right = newdesc.lWidth
        SourceRect.Bottom = newdesc.lHeight
        
        ' copy the temporary surface to the current mipmap's surface
        TempMipMap.Blt DestRect, TempSurface, SourceRect, DDBLT_WAIT
    
        DDSC.lCaps = DDSCAPS_MIPMAP Or DDSCAPS_TEXTURE
        
        On Local Error Resume Next
        Set TempMipMap = TempMipMap.GetAttachedSurface(DDSC)
        ' if there are no mipmap surfaces left in the chain, then exit the loop
        If Err.Number = DDERR_NOTFOUND Then Exit Do
        
        Index = Index + 1
         
    Loop
    On Error GoTo 0
    ' initialize the mesh used to render the mipmaps
    
    Mesh(0) = RVertex(RVector(-1, -1, 0), RVector(0, 0, 1), 0, 1)
    Mesh(1) = RVertex(RVector(-1, 1, 0), RVector(0, 0, 1), 0, 0)
    Mesh(2) = RVertex(RVector(1, -1, 0), RVector(0, 0, 1), 1, 1)
    Mesh(3) = RVertex(RVector(1, 1, 0), RVector(0, 0, 1), 1, 0)
            
    ' setup the texture in the device
    With d3ddev
        .SetTexture 0, MipMap
        .SetTextureStageState 0, D3DTSS_MINFILTER, D3DTFN_LINEAR
        .SetTextureStageState 0, D3DTSS_MAGFILTER, D3DTFG_LINEAR
        .SetRenderState D3DRENDERSTATE_DITHERENABLE, True
    End With
End Sub
