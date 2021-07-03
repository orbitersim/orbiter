Attribute VB_Name = "Introduction"
Option Explicit

Public Sub ShowMapLocation(Optional OutlineLocation As Boolean)
    
    'This sub displays all levels, and displays where the player is located with a flashing orange box
    
    Dim DestRect As RECT                                'Destination rectangle
    Dim CurrentLevelRect As RECT                        'Rectangle for the current level
    Dim ddfxBack As DDBLTFX                             'Blitfx for the back buffer
    Dim intCount As Integer                             'Count variable
    Dim XOffset As Long                                 'Offset of the X line
    Dim YOffset As Long                                 'Offset of the Y line
    Dim XLocation(8) As Long                            'Location X lines
    Dim YLocation(8) As Long                            'Location Y lines
    
    With ddfxBack                                       'Describe the effects to be used with the bltfx
        .lROP = DDBLTFX_ARITHSTRETCHY                   'Stretch/shrink the bitmap (shrink in this case)
    End With
            
    YOffset = 380                                       'Beginning offset where the rectangles will be drawn
    
    For intCount = 0 To UBound(ddsBackgroundObject)     'loop through all background bitmaps
        If intCount Mod 2 = 0 Then                      'if this is an even numbered index
            XOffset = 50                                'this location's rectangle left is 50
            XLocation(intCount) = 110                   'this location's line X is 110
        Else
            XOffset = 510                               'this location's rectangle left is 510
            XLocation(intCount) = XOffset               'this location's line X is the same as the xoffset
        End If
        
        With DestRect                                   'set up this rectangle using the above coordinate values
            .Top = YOffset
            .Bottom = .Top + 60
            .Left = XOffset
            .Right = .Left + 60
        End With
                
        If intCount = (byteLevel - 1) Then CurrentLevelRect = DestRect
                                                        'if the level is equal to the count we are on, store this rectangle for use
        YLocation(intCount) = DestRect.Bottom - ((DestRect.Bottom - DestRect.Top) \ 2)
                                                        'calculate the line that will be drawn between the rectangles' Y position
        ddsBack.BltFx DestRect, ddsBackgroundObject(intCount), EmptyRect, DDBLT_WAIT, ddfxBack
                                                        'blit the background to the screen
        ddsBack.SetForeColor RGB(75, 75, 75)            'set the fore color to a gray color
        ddsBack.SetFillStyle 1                          'make sure the fill is transparent
        ddsBack.DrawBox DestRect.Left, DestRect.Top, DestRect.Right, DestRect.Bottom
                                                        'draw a box around the bitmap
        YOffset = YOffset - 45                          'decrement the Y offset
    Next
    
    If byteLevel > 1 Then                               'if the level is larger than level 1
        For intCount = 1 To (byteLevel - 1)             'loop until we reach the current level
            ddsBack.DrawLine XLocation(intCount - 1), YLocation(intCount - 1), XLocation(intCount), YLocation(intCount)
                                                        'draw a line connecting the last level's index with this level's index
        Next
    End If
        
    If OutlineLocation Then                             'if the sub is called with the OutlineLocation flag set then
        ddsBack.SetForeColor RGB(200, 50, 0)            'set the forecolor to an orange color
        ddsBack.DrawBox CurrentLevelRect.Left, CurrentLevelRect.Top, CurrentLevelRect.Right, CurrentLevelRect.Bottom
                                                        'draw the orange rectangle around the current level bitmap
    End If
    
End Sub

Public Sub StartIntro()
    
    'This subroutine displays the introductory text
    
    Dim strDialog(25) As String                                     'store 25 strings
    Dim lngCount As Long                                            'count variable
    Dim YPosition As Long                                           'y position for the string location
    Dim KeyboardState(0 To 255) As Byte                             'Byte array to hold the state of the keyboard
    Dim ddsSplash As DirectDrawSurface7                             'direct draw surface to hold the background bitmap
    Dim SrcRect As RECT                                             'source rectangle
    Dim ddfxBack As DDBLTFX                                         'blit effects structure

    'These lines store the text to be displayed
    
    strDialog(0) = "As you may know, the unknown alien species has been attacking the Earth for an"
    strDialog(1) = "untold number of years. You have been assigned to the only ship that humankind"
    strDialog(2) = "has been able to build capable of our defense. Reasoning with the aliens"
    strDialog(3) = "has met with silence on their part, and their assault has not stopped."
    strDialog(4) = "Now is the time for all inhabitants of the Earth to put their trust in you."
    strDialog(5) = "You must not let us down."
    strDialog(6) = ""
    strDialog(7) = "You will receive the opportunity of grabbing a power-up for every twenty-five"
    strDialog(8) = "alien ships you destroy. We don't have the time to fit your ship with them now,"
    strDialog(9) = " as our outer perimeter space probes have detected a large armada of alien"
    strDialog(10) = "warcraft on a course for our solar system. The weapons have been manufactured"
    strDialog(11) = "to automatically retrofit to your ship. However, the weapons themselves are very"
    strDialog(12) = "sensitive pieces of equipment, and cannot withstand any direct hits that your"
    strDialog(13) = "craft may encounter. A direct hit will result in the last upgrade made to the"
    strDialog(14) = "ship to fail. With that in mind, it is imperative that you avoid getting hit,"
    strDialog(15) = "as the enemy forces are large, and every upgrade you get will make this difficult"
    strDialog(16) = "mission more attainable."
    strDialog(17) = ""
    strDialog(18) = "We will warp you to the first entry point of the alien galaxy, and you will journey"
    strDialog(19) = "on a course that leads you through each part of their system, destroying as much"
    strDialog(20) = "of their weaponry and resources as possible along the way. At the end of each"
    strDialog(21) = "stage, we have set up warp-jumps that will transport you to the next critical sector."
    strDialog(22) = "Go now, soldier, and fight so that we may avert the annihilation of the human race."
    strDialog(23) = ""
    strDialog(24) = ""
    strDialog(25) = "(Press enter to continue)"
    
    ddsBack.BltColorFill EmptyRect, 0                           'fill the backbuffer with black
    YPosition = 50                                              'initialize the Y coordinate of the text to 50
    
    With SrcRect                                                'describe the location rectangle for the large background bitmap
        .Top = 0
        .Bottom = 479
        .Left = 0
        .Right = 639
    End With
    With ddfxBack                                               'setup the blit effects
        .lROP = DDBLTFX_ARITHSTRETCHY                           'use stretch effects to change the original size of the bitmap
    End With
    
    Set ddsSplash = CreateDDSFromBitmap(dd, App.Path & "\graphics\nebulae4.gif")
                                                                'create a surface
    ddsBack.BltFx SrcRect, ddsSplash, EmptyRect, DDBLTFAST_WAIT, ddfxBack
                                                                'blit the surface to the screen
    Do Until lngCount > UBound(strDialog)                       'loop through all string arrays
        DrawText FindStringCenter(strDialog(lngCount)), YPosition, strDialog(lngCount), RGB(151, 150, 150)
                                                                'draw the text to the screen
        YPosition = YPosition + 15                              'increment the Y position of the text
        lngCount = lngCount + 1                                 'increment the count
    Loop
        
    FadeScreen True                                             'fade the screen in
        
    Do Until True = False
        diKeyBoard.Acquire                                      'acquire the keyboard
        diKeyBoard.GetDeviceState 256, KeyboardState(0)         'get the state of all the keyboard keys
        If ((KeyboardState(DIK_RETURN) And &H80) <> 0) Or _
        ((KeyboardState(DIK_NUMPADENTER) And &H80) <> 0) Then Exit Do
                                                                'if the enter key is pressed, exit the loop
        DoEvents                                                'don't hog the processor
    Loop
    
    FadeScreen                                                  'fade the screen out
    Set ddsSplash = Nothing                                     'release all resources for the background bitmap
    
End Sub


