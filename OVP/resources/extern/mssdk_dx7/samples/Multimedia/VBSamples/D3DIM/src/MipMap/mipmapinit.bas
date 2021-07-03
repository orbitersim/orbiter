Attribute VB_Name = "MipMapInit"
' IM7 Initialization Module

Public DX As New DirectX7
Public DDraw As DirectDraw7

Public d3ddev As Direct3DDevice7
Public MatWorld1 As D3DMATRIX, matView1 As D3DMATRIX, matProj1 As D3DMATRIX
Public MustExit As Boolean
Public UseRGB As Boolean
Public binit As Boolean

Sub IM7Terminate()

    DDraw.SetCooperativeLevel MipMapForm.hWnd, DDSCL_NORMAL
    DDraw.RestoreDisplayMode
    
    CleanUp
    End
End Sub

Sub InitDX()
    Dim b As Boolean
    MipMapForm.Show
    DoEvents
        
        With MipMapForm.IMCanvas1
            If .DX.SystemBpp <= 8 Then
                MsgBox "This sample was designed to run in High Color (16 bit) displays"
                End
            End If
            
            b = .StartWindowed()
            If b = False Then
                MsgBox "unable to initialize 3d "
                End
            End If
            If CheckDeviceCaps(.Direct3DDevice) = False Then
                UseRGB = True
                MsgBox "This sample requires MIPLINEAR MipMapping. to run in Hardware"
                b = .InitWindowed("", "IID_IDirect3DRGBDevice")
                If b = False Then End
            End If
            
            binit = True
        End With
End Sub

Sub CleanUp()
    Set d3ddev = Nothing
    Set DDraw = Nothing
End Sub

Function CheckDeviceCaps(d3ddev As Direct3DDevice7) As Boolean
   ' check texture modes for MIPLINEAR MipMapping
    Dim d3ddevdesc As D3DDEVICEDESC7
    d3ddev.GetCaps d3ddevdesc
    
    If (d3ddevdesc.dpcTriCaps.lTextureFilterCaps And D3DPTFILTERCAPS_LINEARMIPLINEAR) <> D3DPTFILTERCAPS_LINEARMIPLINEAR Then
        Exit Function
    End If
    CheckDeviceCaps = True
End Function

