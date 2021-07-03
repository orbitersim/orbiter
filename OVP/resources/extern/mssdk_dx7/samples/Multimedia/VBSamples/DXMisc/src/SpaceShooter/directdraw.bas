Attribute VB_Name = "DirectDrawBas"
Option Explicit

Public Sub InitializeDD()

    'This sub initializes Direct Draw and loads up all the surfaces
    
    Dim ColorKey As DDCOLORKEY                                                          'dim a colorkey
    Dim ddsSplash As DirectDrawSurface7                                                 'dim a direct draw surface
    Dim EmptyRect As RECT                                                               'setup an empty rectangle
    Dim intCount As Integer                                                             'count variable
    
    On Local Error GoTo ErrorHandler                                                    'Handle any errors
    
    Set dd = dx.DirectDrawCreate("")                                                    'create an instance of a direct draw object
    dd.SetCooperativeLevel frmSpaceShooter.hWnd, DDSCL_FULLSCREEN Or DDSCL_EXCLUSIVE
                                                                                        'use frmSpaceShooters hWnd, and make the app exculsive and fullscreen
    dd.SetDisplayMode 640, 480, 8, 0, 0                                                 'change the display mode to 640x480, the rest of the params are not used
    With ddsdFront                                                                      'now it is time to describe the primary surface, which is the visible suface that the user sees
        .lFlags = DDSD_CAPS Or DDSD_BACKBUFFERCOUNT                                     'the front buffer will have the caps as a valid param, as well as the backbuffer param
        .ddsCaps.lCaps = DDSCAPS_PRIMARYSURFACE Or DDSCAPS_FLIP Or DDSCAPS_COMPLEX Or DDSCAPS_SYSTEMMEMORY
                                                                                        'this surface will be the primary, flipable surface, it's a complex surface, and it will be located in system memory
        .lBackBufferCount = 1                                                           'there is one back buffer attached to the front buffer
    End With
                                                    
    Set ddsFront = dd.CreateSurface(ddsdFront)                                          'Create front buffer
                                                                                       
    ddCaps.lCaps = DDSCAPS_BACKBUFFER                                                   'The only caps needed for the back buffer is to tell direct draw that it is a backbuffer
    Set ddsBack = ddsFront.GetAttachedSurface(ddCaps)                                   'create the back buffer
                                                                                           
    With ddfxBack                                                                       'used to fill the back buffer with black at the ddsStart of a new cycle in the subroutine BlitBackground
        .lFill = 0                                                                      'use black to fill the back buffer
    End With
    
    Set DDPalette = dd.LoadPaletteFromBitmap(App.path & "\graphics\laser.bmp")         'Create a direct draw palette from the laser bitmap
    ddsFront.SetPalette DDPalette                                                       'set the front buffers palette to our palette
    Set ddsSplash = CreateDDSFromBitmap(dd, App.path & "\graphics\splash.gif")          'create the splash screen surface
    ddsBack.BltFast 0, 0, ddsSplash, EmptyRect, DDBLTFAST_WAIT                          'blit the splash screen to the back buffer
    ddsFront.Flip Nothing, 0                                                            'flip the front buffer so the splash screen bitmap on the backbuffer is displayed
    Set ddsSplash = Nothing                                                             'release the splash screen, since we don't need it anymore
    PlayMidi "title.mid"                                                                'Start playing the title song
    
    With ColorKey                                                                       'set the color key used for blitting the rest of the bitmaps
        .high = TRANSPARENTINDEX                                                        'uses a pink color as the colorkey
        .low = TRANSPARENTINDEX
    End With
    
    Set ddsTitle = CreateDDSFromBitmap(dd, App.path & "\graphics\title.gif")            'Load the title screen bitmap and put in a direct draw surface
    ddsTitle.SetColorKey DDCKEY_SRCBLT, ColorKey                                        'Set the transparent color key for this direct draw surface
    Set ddsShip = CreateDDSFromBitmap(dd, App.path & "\graphics\ship.gif")              'Load the ship bitmap and make it into a direct draw surface
    ddsShip.SetColorKey DDCKEY_SRCBLT, ColorKey                                         'set the bitmaps color key to the one defined above
    Set ddsShieldIndicator = CreateDDSFromBitmap(dd, App.path & "\graphics\shields.gif")
                                                                                        'Load the shield indicator bitmap and put in a direct draw surface
    Set ddsPowerUp = CreateDDSFromBitmap(dd, App.path & "\graphics\PowerUps.gif")       'Load the shield indicator bitmap and put in a direct draw surface
    ddsPowerUp.SetColorKey DDCKEY_SRCBLT, ColorKey                                      'Set the transparent color key for this direct draw surface
    Set ddsExplosion(0) = CreateDDSFromBitmap(dd, App.path & "\graphics\explosion.gif") 'Load the first explosion bitmap
    ddsExplosion(0).SetColorKey DDCKEY_SRCBLT, ColorKey                                 'Color key for the explosion
    Set ddsExplosion(1) = CreateDDSFromBitmap(dd, App.path & "\graphics\explosion2.gif")
                                                                                        'Load the second explosion bitmap
    ddsExplosion(1).SetColorKey DDCKEY_SRCBLT, ColorKey                                 'Color key for the explosion
    Set ddsInvulnerable = CreateDDSFromBitmap(dd, App.path & "\graphics\invulnerable.gif")
                                                                                        'Load the invulnerable bitmap
    ddsInvulnerable.SetColorKey DDCKEY_SRCBLT, ColorKey                                 'Color key for the explosion
    
    'The rest of the sub just describes the various attributes of the enemies, obstacles, and background bitmaps, and
    'loads the neccessary objets into direct draw surfaces
        
    For intCount = 0 To UBound(ExplosionDesc)
        With ExplosionDesc(intCount)
            .NumFrames = 19
            .Width = 120
            .Height = 120
        End With
    Next
            
    Set ddsHit = CreateDDSFromBitmap(dd, App.path & "\graphics\hit.gif")
    ddsHit.SetColorKey DDCKEY_SRCBLT, ColorKey
    For intCount = 0 To UBound(HitDesc)
        With HitDesc(intCount)
            .NumFrames = 5
            .Height = 8
            .Width = 8
        End With
    Next
    
    Set ddsLaser = CreateDDSFromBitmap(dd, App.path & "\graphics\laser.bmp")
    ddsLaser.SetColorKey DDCKEY_SRCBLT, ColorKey
    For intCount = 0 To UBound(LaserDesc)
        LaserDesc(intCount).Exists = False
        LaserDesc(intCount).Width = LASER1WIDTH
        LaserDesc(intCount).Height = LASER1HEIGHT
    Next
    
    Set ddsLaser2R = CreateDDSFromBitmap(dd, App.path & "\graphics\laser2.gif")
    ddsLaser2R.SetColorKey DDCKEY_SRCBLT, ColorKey
    For intCount = 0 To UBound(Laser2RDesc)
        Laser2RDesc(intCount).Exists = False
        Laser2RDesc(intCount).Width = LASER2WIDTH
        Laser2RDesc(intCount).Height = LASER2HEIGHT
    Next
    
    Set ddsLaser2L = CreateDDSFromBitmap(dd, App.path & "\graphics\laser2.gif")
    ddsLaser2L.SetColorKey DDCKEY_SRCBLT, ColorKey
    For intCount = 0 To UBound(Laser2LDesc)
        Laser2LDesc(intCount).Exists = False
        Laser2LDesc(intCount).Width = LASER2WIDTH
        Laser2LDesc(intCount).Height = LASER2HEIGHT
    Next
        
    Set ddsLaser3 = CreateDDSFromBitmap(dd, App.path & "\graphics\laser3.gif")
    ddsLaser3.SetColorKey DDCKEY_SRCBLT, ColorKey
    For intCount = 0 To UBound(Laser3Desc)
        Laser3Desc(intCount).Exists = False
        Laser3Desc(intCount).Width = LASER3WIDTH
        Laser3Desc(intCount).Height = LASER3HEIGHT
    Next
    
    Set ddsEnemyFire = CreateDDSFromBitmap(dd, App.path & "\graphics\enemyfire1.gif")
    ddsEnemyFire.SetColorKey DDCKEY_SRCBLT, ColorKey
    
    Set ddsStar = CreateDDSFromBitmap(dd, App.path & "\graphics\stars.gif")
    
    Set ddsGuidedMissile = CreateDDSFromBitmap(dd, App.path & "\graphics\GuidedMissile.gif")
    ddsGuidedMissile.SetColorKey DDCKEY_SRCBLT, ColorKey
    
    Set ddsDisplayBomb = CreateDDSFromBitmap(dd, App.path & "\graphics\displaybomb.gif")
    ddsDisplayBomb.SetColorKey DDCKEY_SRCBLT, ColorKey
    
    Set ddsObstacle(40) = CreateDDSFromBitmap(dd, App.path & "\graphics\deadplate.gif")
    
    Exit Sub
       
ErrorHandler:                                                                           'handle errors here
    
    If Err.Number = DDERR_NOEXCLUSIVEMODE Then                                          'if the card has problems initializing DX
        EndGame                                                                         'release all objects
        MsgBox "Unable to create Direct Draw object. Ensure that your drivers are the latest available for your video card."
                                                                                        'display this error message
        End
    Else                                                                                'otherwise, it is an unknown error
        EndGame                                                                         'release all objects
        End                                                                             'end the game
    End If
    
End Sub

