Attribute VB_Name = "basDD"
Option Explicit
'******
'This application uses conditional compilation.  To run this sample in the IDE, you
'must first go to Project Properties (Project Menu-> Properties).  Then on the Make tab
'change the RunInIDE=0 to RunInIDE=1.

Public dx As New DirectX7
Public DD As DirectDraw7
Public DDS As DirectDrawSurface7
Public dC As DirectDrawClipper
Public DDSD As DDSURFACEDESC2
Public DR As RECT


Public bB As DirectDrawSurface7
Public BD As DDSURFACEDESC2
Public BBR As RECT

Public ar() As Byte
Public AlphaRect As RECT

Dim lPixelDepth As Byte
Dim clr As Long
Dim cols As Long
Dim rows As Long
Dim col As Long
Dim row As Long

Public Sprites(9) As DirectDrawSurface7
Public SpriteD(9) As DDSURFACEDESC2
Public SpriteR(9) As RECT
Public key(9) As DDCOLORKEY

Dim spriteWidth As Integer
Dim spriteHeight As Integer
Dim currentframe As Integer
Dim slide(39) As RECT
Dim Pal(255) As PALETTEENTRY
Dim Palette As DirectDrawPalette

Public Fish(2) As DirectDrawSurface7
Public fishD(2) As DDSURFACEDESC2
Public fishR(2) As RECT
Public fishkey(2) As DDCOLORKEY

Public sMSG As String


Public x%, y%
Private tmpR As RECT

Private Type fis
    sR As RECT
    x As Long
    y As Single
End Type

Private fi(2) As fis

'Registry constants
Private Const KEY_READ = 131097
Private Const REG_SZ = 1
Private Const HKEY_LOCAL_MACHINE = &H80000002
'Registry API's
Private Declare Function RegConnectRegistry Lib "advapi32.dll" Alias "RegConnectRegistryA" (ByVal lpMachineName As String, ByVal hKey As Long, phkResult As Long) As Long
Private Declare Function RegCloseKey Lib "advapi32.dll" (ByVal hKey As Long) As Long
Private Declare Function RegQueryValueEx Lib "advapi32.dll" Alias "RegQueryValueExA" (ByVal hKey As Long, ByVal lpValueName As String, ByVal lpReserved As Long, lpType As Long, ByVal lpData As String, lpcbData As Long) As Long
Private Declare Function RegOpenKeyEx Lib "advapi32.dll" Alias "RegOpenKeyExA" (ByVal hKey As Long, ByVal lpSubKey As String, ByVal ulOptions As Long, ByVal samDesired As Long, phkResult As Long) As Long
'Sleep
Public Declare Sub Sleep Lib "kernel32" (ByVal dwMilliseconds As Long)



Public Sub InitDD(hWnd As Long, ClipperHwnd As PictureBox)
    Dim oPixelFormat As DDPIXELFORMAT
    On Local Error GoTo err_
    
    Set DD = dx.DirectDrawCreate(vbNullString)
    DD.SetCooperativeLevel hWnd, DDSCL_NORMAL
    
    
    DDSD.lFlags = DDSD_CAPS
    DDSD.ddsCaps.lCaps = DDSCAPS_PRIMARYSURFACE
    Set DDS = DD.CreateSurface(DDSD)
    
    Set dC = DD.CreateClipper(0)
    dC.SetHWnd ClipperHwnd.hWnd
    
    DDS.SetClipper dC
    
    DDS.GetPixelFormat oPixelFormat
    
    If oPixelFormat.lRGBBitCount < 8 Then
        If Not (DD Is Nothing) Then
            DD.SetCooperativeLevel frmMain.hWnd, DDSCL_NORMAL
            DoEvents
        End If
    
        MsgBox "Must run at 16bit color or higher.", vbApplicationModal
        End
    Else
        lPixelDepth = oPixelFormat.lRGBBitCount
    End If
    
    BD.lFlags = DDSD_HEIGHT Or DDSD_WIDTH Or DDSD_CAPS
    BD.ddsCaps.lCaps = DDSCAPS_OFFSCREENPLAIN Or DDSCAPS_SYSTEMMEMORY
    BD.lWidth = ClipperHwnd.ScaleWidth
    BD.lHeight = ClipperHwnd.ScaleHeight
    
    Set bB = DD.CreateSurface(BD)

    BBR.Bottom = ClipperHwnd.Height
    BBR.Right = ClipperHwnd.Width
    
    loadSprites

    AlphaRect.Right = BD.lWidth - 1
    AlphaRect.Bottom = BD.lHeight - 1
    Exit Sub


err_:
    
    If Not (DD Is Nothing) Then
        DD.SetCooperativeLevel frmMain.hWnd, DDSCL_NORMAL
        DoEvents
    End If

    MsgBox "Unable to initalize DirectDraw.", vbApplicationModal
    End

End Sub

Private Sub loadSprites()
    '0
    SpriteD(0).lFlags = DDSD_CAPS
    SpriteD(0).ddsCaps.lCaps = DDSCAPS_OFFSCREENPLAIN Or DDSCAPS_SYSTEMMEMORY

#If RunInIDE = 1 Then
    'ide
    Dim sMedia As String
    
    sMedia = FindMediaDir("base.bmp", "DMusic")
    If sMedia <> vbNullString Then 'Media is not in current folder
        If (Left$(sMedia, 2) <> Left$(CurDir, 2)) And (InStr(Left$(sMedia, 2), ":") > 0) Then ChDrive Left$(sMedia, 2)
        ChDir sMedia
    End If
    
    Set Sprites(0) = DD.CreateSurfaceFromFile("base.bmp", SpriteD(0))
#Else
    'exe
    Set Sprites(0) = DD.CreateSurfaceFromResource(App.Path & "\clubmet.exe", "BASE", SpriteD(0))
#End If

    SpriteR(0).Right = SpriteD(0).lWidth
    SpriteR(0).Bottom = SpriteD(0).lHeight

    
    '1
    SpriteD(1).lFlags = DDSD_CAPS
    SpriteD(1).ddsCaps.lCaps = DDSCAPS_OFFSCREENPLAIN Or DDSCAPS_SYSTEMMEMORY
    
#If RunInIDE = 1 Then
    'ide
    Set Sprites(1) = DD.CreateSurfaceFromFile("sax.bmp", SpriteD(1))
#Else
    'exe
    Set Sprites(1) = DD.CreateSurfaceFromResource(App.Path & "\clubmet.exe", "sax", SpriteD(1))
#End If

    SpriteR(1).Right = SpriteD(1).lWidth
    SpriteR(1).Bottom = SpriteD(1).lHeight
    
    'notes
    SpriteD(6).lFlags = DDSD_CAPS Or DDSD_HEIGHT Or DDSD_WIDTH
    SpriteD(6).ddsCaps.lCaps = DDSCAPS_OFFSCREENPLAIN Or DDSCAPS_SYSTEMMEMORY
    SpriteD(6).lWidth = 64: SpriteD(6).lHeight = 64
    
#If RunInIDE = 1 Then
    'ide
    Set Sprites(6) = DD.CreateSurfaceFromFile("notes.bmp", SpriteD(6))
#Else
    'exe
    Set Sprites(6) = DD.CreateSurfaceFromResource(App.Path & "\clubmet.exe", "NOTES", SpriteD(6))
#End If

    SpriteR(6).Right = SpriteD(6).lWidth
    SpriteR(6).Bottom = SpriteD(6).lHeight
    
    
    key(6).low = 0
    key(6).high = 0
    Sprites(6).SetColorKey DDCKEY_SRCBLT, key(6)
    
    
    
    '2
    SpriteD(2).lFlags = DDSD_CAPS
    SpriteD(2).ddsCaps.lCaps = DDSCAPS_OFFSCREENPLAIN Or DDSCAPS_SYSTEMMEMORY
    
#If RunInIDE = 1 Then
    'ide
    Set Sprites(2) = DD.CreateSurfaceFromFile("keys.bmp", SpriteD(2))
#Else
    Set Sprites(2) = DD.CreateSurfaceFromResource(App.Path & "\clubmet.exe", "KEYS", SpriteD(2))
#End If

    SpriteR(2).Right = SpriteD(2).lWidth
    SpriteR(2).Bottom = SpriteD(2).lHeight
    
    
    ''''''''''''''''''''''''''''''''''''''''''
    'loadFish
    ''''''''''''''''''''''''''''''''''''''''''
    fishD(0).lFlags = DDSD_CAPS Or DDSD_HEIGHT Or DDSD_WIDTH
    fishD(0).ddsCaps.lCaps = DDSCAPS_OFFSCREENPLAIN Or DDSCAPS_SYSTEMMEMORY
    fishD(0).lWidth = 64: fishD(0).lHeight = 64
    
    fishD(1).lFlags = DDSD_CAPS Or DDSD_HEIGHT Or DDSD_WIDTH
    fishD(1).ddsCaps.lCaps = DDSCAPS_OFFSCREENPLAIN Or DDSCAPS_SYSTEMMEMORY
    fishD(1).lWidth = 64: fishD(1).lHeight = 64
    
    fishD(2).lFlags = DDSD_CAPS Or DDSD_HEIGHT Or DDSD_WIDTH
    fishD(2).ddsCaps.lCaps = DDSCAPS_OFFSCREENPLAIN Or DDSCAPS_SYSTEMMEMORY
    fishD(2).lWidth = 64: fishD(2).lHeight = 64
    
#If RunInIDE = 1 Then
 'ide
    Set Fish(0) = DD.CreateSurfaceFromFile("f1.bmp", fishD(0))
    Set Fish(1) = DD.CreateSurfaceFromFile("f2.bmp", fishD(1))
    Set Fish(2) = DD.CreateSurfaceFromFile("f3.bmp", fishD(2))
#Else
'exe
    Set Fish(0) = DD.CreateSurfaceFromResource(App.Path & "\clubmet.exe", "F1", fishD(0))
    Set Fish(1) = DD.CreateSurfaceFromResource(App.Path & "\clubmet.exe", "F2", fishD(1))
    Set Fish(2) = DD.CreateSurfaceFromResource(App.Path & "\clubmet.exe", "F3", fishD(2))
#End If


    
    Dim i As Integer
    
    For i = 0 To UBound(Fish)
        fishR(i).Right = fishD(i).lWidth
        fishR(i).Bottom = fishD(i).lHeight
        fishkey(i).low = 0
        fishkey(i).high = 0
        Fish(i).SetColorKey DDCKEY_SRCBLT, fishkey(i)
    Next i
    
    
    'sprite(5) animated hand
    
    SpriteD(5).lFlags = DDSD_CAPS
    SpriteD(5).ddsCaps.lCaps = DDSCAPS_OFFSCREENPLAIN Or DDSCAPS_SYSTEMMEMORY
    
#If RunInIDE = 1 Then
    'ide
    Set Sprites(5) = DD.CreateSurfaceFromFile("handani.bmp", SpriteD(5))
#Else
    'exe
    Set Sprites(5) = DD.CreateSurfaceFromResource(App.Path & "\clubmet.exe", "handani", SpriteD(5))
#End If
    

    SpriteR(5).Right = SpriteD(5).lWidth
    SpriteR(5).Bottom = SpriteD(5).lHeight
    spriteWidth = 272
    spriteHeight = 177
    cols = SpriteD(5).lWidth / spriteWidth
    rows = SpriteD(5).lHeight / spriteHeight
    
    key(5).low = 0
    key(5).high = 0
    Sprites(5).SetColorKey DDCKEY_SRCBLT, key(5)
    
    
    '9
    SpriteD(9).lFlags = DDSD_CAPS
    SpriteD(9).ddsCaps.lCaps = DDSCAPS_OFFSCREENPLAIN Or DDSCAPS_SYSTEMMEMORY
    
#If RunInIDE = 1 Then
    'ide
    Set Sprites(9) = DD.CreateSurfaceFromFile("bknote.bmp", SpriteD(9))
#Else
    'exe
    Set Sprites(9) = DD.CreateSurfaceFromResource(App.Path & "\clubmet.exe", "bknote", SpriteD(9))
#End If
    

    SpriteR(9).Right = SpriteD(9).lWidth
    SpriteR(9).Bottom = SpriteD(9).lHeight
        
        
    StripVert slide(), SpriteR(9).Right, SpriteR(9).Bottom
    
    '8
    SpriteD(8).lFlags = DDSD_CAPS
    SpriteD(8).ddsCaps.lCaps = DDSCAPS_OFFSCREENPLAIN Or DDSCAPS_SYSTEMMEMORY
    
#If RunInIDE = 1 Then
    'ide
    Set Sprites(8) = DD.CreateSurfaceFromFile("dance.bmp", SpriteD(8))
#Else
    'exe
    Set Sprites(8) = DD.CreateSurfaceFromResource(App.Path & "\clubmet.exe", "dance", SpriteD(8))
#End If

    SpriteR(8).Right = SpriteD(8).lWidth
    SpriteR(8).Bottom = SpriteD(8).lHeight
        
    
    
    
    
End Sub

Public Sub MoveFrame(Index As Integer)

    On Local Error GoTo err_
    
    Select Case Index
        Case 0
            bB.Blt BBR, Sprites(Index), SpriteR(Index), DDBLT_WAIT
            
            
            bB.Lock AlphaRect, BD, DDLOCK_WAIT, 0
            bB.GetLockedArray ar()
                DoEvents
    
                clr = Rnd * 255
                
                For y = 0 To (AlphaRect.Bottom - 1)
                For x = 0 To (AlphaRect.Right - 1) * 2
        
                    If ar(x, y) <> 0 And ar(x, y) <> 64 And ar(x, y) <> 255 And ar(x, y) <> 127 Then
                            If ar(x, y) = 224 Then
                                ar(x, y) = clr
                            End If
                    End If
                Next
                DoEvents
                Next
                DoEvents
                bB.Unlock AlphaRect
             
            
                dx.GetWindowRect frmMain.mnCan.hWnd, DR
                DDS.Blt DR, bB, BBR, DDBLT_WAIT
            
        Case 1
            tmpR.Top = Rnd * 200
            tmpR.Left = Rnd * 50
            
            bB.Blt BBR, Sprites(Index), SpriteR(Index), DDBLT_WAIT
            bB.BltFast tmpR.Left, tmpR.Top, Sprites(6), SpriteR(6), DDBLTFAST_WAIT Or DDBLTFAST_SRCCOLORKEY
                        
            
            
            dx.GetWindowRect frmMain.mnCan.hWnd, DR
            DDS.Blt DR, bB, BBR, DDBLT_WAIT
            Sleep 50
            
            
        Case 2
            On Error Resume Next
            Dim nColor As Integer, tmp As Integer
            bB.Blt BBR, Sprites(Index), SpriteR(Index), DDBLT_WAIT
            dx.GetWindowRect frmMain.mnCan.hWnd, DR
            bB.Lock AlphaRect, BD, DDLOCK_WAIT, 0
            bB.GetLockedArray ar()
            DoEvents
            nColor = Rnd * 256
            If nColor = 0 Then nColor = 1
            
            For y = 0 To (AlphaRect.Bottom - 1)
            For x = 0 To (AlphaRect.Right - 1) * (lPixelDepth \ 8)
            If ar(x, y) <> 0 And ar(x, y) <> 124 Then
                    ar(x, y) = nColor
            End If
            Next
            DoEvents
            
            Next
            DoEvents
            bB.Unlock AlphaRect
            DDS.Blt DR, bB, BBR, DDBLT_WAIT
            On Error GoTo 0
        Case 3
              
              
            With fi(0)
                .x = .x + 1: If .x > frmMain.mnCan.ScaleWidth Then .x = 0
                .y = Sin(.x / 5) * 5 + (frmMain.mnCan.ScaleHeight \ 2)
            End With


            With fi(1)
                .x = .x + 2: If .x > frmMain.mnCan.ScaleWidth Then .x = 0
                .y = Sin(.x / 20) * 20 + (frmMain.mnCan.ScaleHeight \ 4)
            End With

            With fi(2)
                .x = .x - 2: If .x < frmMain.mnCan.ScaleLeft Then .x = frmMain.mnCan.ScaleWidth
                .y = Sin(.x / 40) * 40 + (frmMain.mnCan.ScaleHeight \ 3)
            End With
              
            Dim i As Integer
            
                bB.BltColorFill BBR, &H0
                
            For i = 0 To UBound(fi)
                bB.BltFast fi(i).x, fi(i).y, Fish(i), fishR(i), DDBLTFAST_WAIT Or DDBLTFAST_SRCCOLORKEY
            Next i
                        
            dx.GetWindowRect frmMain.mnCan.hWnd, DR
            DDS.Blt DR, bB, BBR, DDBLT_WAIT
            
            Sleep 50
        Case 5
        
            Dim rSprite As RECT
            
            currentframe = currentframe + 1
            If currentframe > rows * cols - 1 Then currentframe = 0
            
            col = currentframe Mod cols
            row = Int(currentframe / cols)
            rSprite.Left = col * spriteWidth
            rSprite.Top = row * spriteHeight
            rSprite.Right = rSprite.Left + spriteWidth
            rSprite.Bottom = rSprite.Top + spriteHeight
              
           
    
            bB.BltColorFill BBR, &H0
            Set bB = MoveBackRight(bB, Sprites(9), slide)
           
            DoEvents
            
            bB.BltFast 0, frmMain.mnCan.ScaleHeight \ 3, Sprites(Index), rSprite, DDBLTFAST_SRCCOLORKEY Or DDBLTFAST_WAIT
            
                        
            dx.GetWindowRect frmMain.mnCan.hWnd, DR
            DDS.Blt DR, bB, BBR, DDBLT_WAIT
            
            
            Sleep 80
        Case 6
           
           
            bB.Blt BBR, Sprites(8), SpriteR(8), DDBLT_WAIT
            
            
                bB.Lock AlphaRect, BD, DDLOCK_WAIT, 0
                bB.GetLockedArray ar()
                DoEvents
            
    
            For y = 0 To (AlphaRect.Bottom - 1)
            For x = 0 To (AlphaRect.Right - 1) * 2
    
            If ar(x, y) <> 0 And ar(x, y) <> 255 Then
                    ar(x, y) = Rnd * 255
            End If
            Next
            DoEvents
            
            Next
            DoEvents
            bB.Unlock AlphaRect
            DDS.Blt DR, bB, BBR, DDBLT_WAIT
           
        
                        
            dx.GetWindowRect frmMain.mnCan.hWnd, DR
            DDS.Blt DR, bB, BBR, DDBLT_WAIT
            
'            Sleep 20
    
    End Select
    
    Exit Sub
err_:
    
    If Not (DD Is Nothing) Then
        DD.SetCooperativeLevel frmMain.hWnd, DDSCL_NORMAL
        DoEvents
    End If
    MsgBox "There was an issue with playing the current frame." & vbCrLf & _
            Err.Number & vbCrLf & _
            Err.Description, vbApplicationModal
    End
    
    
End Sub


Public Sub StripVert(cChop() As RECT, wD As Long, hD As Long)
    Dim cntr As Integer
    Dim nN As Long
    Dim sZ As Long
    
    For cntr = 0 To UBound(cChop)
        sZ = wD / UBound(cChop)
        nN = nN + sZ
        
        cChop(cntr).Left = (nN - sZ)
        cChop(cntr).Right = nN
        cChop(cntr).Bottom = hD
    Next
    
    'StripVert = cChop
    
End Sub



Public Function MoveBackRight(ByVal bB As DirectDrawSurface7, ByVal backgrounds As DirectDrawSurface7, recArray() As RECT) As DirectDrawSurface7
    Dim tmpC As Integer
    Dim stp As Integer
    Static cntrFR As Integer
    
    For tmpC = 0 To UBound(recArray)
           If cntrFR >= (UBound(recArray) - 1) Then
               cntrFR = 0
           End If
            bB.BltFast recArray(cntrFR).Left, (frmMain.mnCan.ScaleHeight \ 3), backgrounds, recArray(tmpC), DDBLTFAST_WAIT
            cntrFR = cntrFR + 1
           DoEvents
    Next
    
    Set MoveBackRight = bB
    DoEvents
End Function



Public Function LoadMSg(Index As Integer) As String

    Dim tempMSG(7) As String
    
    'wed
    tempMSG(0) = "Big Band!" & vbCrLf & _
                 "Join us for a blast from the past!  Enjoy cutting the rug " & vbCrLf & _
                 "with the finest big band musicians from the area.  Whether " & vbCrLf & _
                 "you want to swing dance the night away, or just listen to " & vbCrLf & _
                 "the big sound from the stage, you're sure to have a great time." & vbCrLf & _
                 "Just don't forget the zoot suit at home!"
    'thur
    tempMSG(1) = "Jazz Night!" & vbCrLf & _
                 "Thursdays are a big night at Club Met, because our jazz music" & vbCrLf & _
                 "is always cool and fresh.  The musicians that join us are always" & vbCrLf & _
                 "Grade A, including local band Benny HaHa and the Blue Wave." & vbCrLf & _
                 "Jazz has never been better!"
    'fri
    tempMSG(2) = "Disco!" & vbCrLf & _
                 "If the 70's are making a comeback, you'll find Friday Night at Club Met" & vbCrLf & _
                 "to be the headquarters.  Always a blast, we bring in some of the original" & vbCrLf & _
                 "disco artists to play their most popular numbers.  It's even more fun to" & vbCrLf & _
                 "come dressed to the part.  Come once and it's sure to be your Friday Night" & vbCrLf & _
                 "hang out!"
    'sat
    tempMSG(3) = "Dance Night!" & vbCrLf & _
                 "A definite favorite of Generation X (and Y) is Dance night at Club Met." & vbCrLf & _
                 "Our own DJ, Flavor Mike, spins the latest dance hits all night long." & vbCrLf & _
                 "Once you start dancing, you can't slow down.  Plenty of new people to " & vbCrLf & _
                 "meet, and always a good time."
                 
    'sun
    tempMSG(4) = "Classical!" & vbCrLf & _
                 "If your tastes are more refined, we encourage you to join us on Sunday" & vbCrLf & _
                 "night at Club Met.  You'll enjoy intellectual conversation and delightful" & vbCrLf & _
                 "music from the Baroque, Classical, and Romantic eras, played by renowned" & vbCrLf & _
                 "local artists.  A great way to energize yourself for the week ahead."
                 
                 
    LoadMSg = tempMSG(Index)
                 

End Function

Public Function FindMediaDir(ByVal sFile As String, ByVal sSampleType As String) As String
    If Dir$(sFile, vbNormal) <> vbNullString Then 'This file is the current folder
        FindMediaDir = vbNullString
        Exit Function
    End If
    FindMediaDir = AddDirSep(GetDXSampleFolder) & "vbsamples\" & sSampleType & "\media\"
End Function

Private Function AddDirSep(ByVal sPath As String) As String
    AddDirSep = sPath
    If Right$(sPath, 1) <> "\" Then
        AddDirSep = sPath & "\"
    End If
End Function
Private Function GetDXSampleFolder() As String
    Dim lHandle As Long
    Dim lNewHandle As Long, sValue As String
    Dim lNewKey As Long
    
    RegConnectRegistry vbNullString, HKEY_LOCAL_MACHINE, lHandle
    RegOpenKeyEx lHandle, "SOFTWARE\Microsoft\DirectX", 0, KEY_READ, lNewHandle
    sValue = Space$(255)
    RegQueryValueEx lNewHandle, "DXSDK Samples Path", 0, REG_SZ, sValue, 255
    If sValue <> Space$(255) Then
        sValue = Left$(sValue, InStr(sValue, Chr$(0)) - 1)
    Else
        sValue = vbNullString
    End If
    RegCloseKey lNewHandle
    RegCloseKey lHandle
    GetDXSampleFolder = sValue
End Function

