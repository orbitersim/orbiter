Attribute VB_Name = "FireWorksInit"
' IM7 Initialization Module

Public DX As New DirectX7
Public DDraw As DirectDraw7
Public D3D As Direct3D7
Public DDSFront As DirectDrawSurface7
Public DDSBack As DirectDrawSurface7
Public DDSFrontDesc As DDSURFACEDESC2
Public DDSBackDesc As DDSURFACEDESC2
Public DDSCaps As DDSCAPS2
Public d3ddev As Direct3DDevice7
Public BackRect As RECT
Public ViewPort1 As D3DVIEWPORT7
Public Light1 As D3DLIGHT7
Public Material1 As D3DMATERIAL7
Public RWDestRect As RECT, RWSourceRect As RECT
Public MatWorld1 As D3DMATRIX, matView1 As D3DMATRIX, matProj1 As D3DMATRIX
Public MustExit As Boolean
Public UseRGB As Boolean

Sub IM7Terminate()

    DDraw.SetCooperativeLevel FireWorksForm.hWnd, DDSCL_NORMAL
    DDraw.RestoreDisplayMode
    CleanUp
    Set DX = Nothing
    
    End
End Sub
Sub CleanUp()
    Set d3ddev = Nothing
    Set D3D = Nothing
    Set DDSBack = Nothing
    Set DDSFront = Nothing
    Set DDraw = Nothing

End Sub

Sub InitDX()
    Dim c As D3DCOLORVALUE
    Dim lProp As D3DLIGHT7
    Dim Index As Long
    Dim b As Boolean

        DoEvents
        
        With FireWorksForm.IMCanvas1
            If .DX.SystemBpp <= 8 Then
                MsgBox "This sample was designed to run in High Color (16 bit) displays"
                End
            End If
            b = .StartWindowed()
            If b = False Then
                MsgBox "could not initialize 3d device"
                End
            End If
            If False = CheckCaps(.Direct3DDevice) Then
                UseRGB = True
                b = .InitWindowed("", "IID_IDirect3DRGBDevice")
            End If
            If b = False Then
                MsgBox "could not initialize 3d device"
                End
            End If

        End With

        FireWorksForm.Show
End Sub

Function CheckCaps(d3ddev As Direct3DDevice7) As Boolean
    Dim desc As D3DDEVICEDESC7
    d3ddev.GetCaps desc
    
    If (desc.dpcTriCaps.lSrcBlendCaps And D3DPBLENDCAPS_ONE) <> D3DPBLENDCAPS_ONE Then Exit Function
    If (desc.dpcTriCaps.lDestBlendCaps And D3DPBLENDCAPS_ONE) <> D3DPBLENDCAPS_ONE Then Exit Function

    CheckCaps = True
End Function
