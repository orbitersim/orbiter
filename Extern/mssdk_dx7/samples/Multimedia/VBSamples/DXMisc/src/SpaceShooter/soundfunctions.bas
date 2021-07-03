Attribute VB_Name = "DirectSoundBas"
Option Explicit
    
Public Sub InitializeDS()
    
    'This sub initializes all of the sound effects used by SS2k
    
    Dim dsBDesc As DSBUFFERDESC                                     'variable to hold the direct sound buffer description
    Dim bdesc As DSBUFFERDESC                                       'variable to hold another direct sound buffer description
    Dim intCount As Integer                                         'standard count variable
    
    On Local Error GoTo ErrorHandler                                'make sure to handle any errors
    
    Set DS = dx.DirectSoundCreate("")                               'create the direct sound object using the default sound device
    DS.SetCooperativeLevel frmSpaceShooter.hWnd, DSSCL_NORMAL       'set the cooperative level to the space shooter form, and use normal mode
    With bdesc
        .lFlags = DSBCAPS_PRIMARYBUFFER Or DSBCAPS_CTRLPAN          'this will be the primary buffer, and have panning capabilities
    End With
    
    Dim s As WAVEFORMATEX, w As WAVEFORMATEX                        'Dim two waveformatex structures
    Set dsPrimaryBuffer = DS.CreateSoundBuffer(bdesc, s)            'create a primary sound buffer using the buffer desc and wave format structure
    
    With dsBDesc
        .lFlags = DSBCAPS_CTRLPAN Or DSBCAPS_CTRLFREQUENCY          'allow for pan and frequency changes
    End With
    
    'The next lines load up all of the wave files using the default capabilites
    
    Set dsPowerUp = DS.CreateSoundBufferFromFile(App.path & "\sound\powerup.wav", dsBDesc, w)
    Set dsEnergize = DS.CreateSoundBufferFromFile(App.path & "\sound\energize.wav", dsBDesc, w)
    Set dsAlarm = DS.CreateSoundBufferFromFile(App.path & "\sound\alarm.wav", dsBDesc, w)
    Set dsLaser = DS.CreateSoundBufferFromFile(App.path & "\sound\laser.wav", dsBDesc, w)
    Set dsExplosion = DS.CreateSoundBufferFromFile(App.path & "\sound\explosion.wav", dsBDesc, w)
    Set dsMissile = DS.CreateSoundBufferFromFile(App.path & "\sound\missile.wav", dsBDesc, w)
    Set dsNoHit = DS.CreateSoundBufferFromFile(App.path & "\sound\nohit.wav", dsBDesc, w)
    Set dsEnemyFire = DS.CreateSoundBufferFromFile(App.path & "\sound\enemyfire.wav", dsBDesc, w)
    Set dsLaser2 = DS.CreateSoundBufferFromFile(App.path & "\sound\laser2.wav", dsBDesc, w)
    Set dsPulseCannon = DS.CreateSoundBufferFromFile(App.path & "\sound\pulse.wav", dsBDesc, w)
    Set dsPlayerDies = DS.CreateSoundBufferFromFile(App.path & "\sound\playerdies.wav", dsBDesc, w)
    Set dsInvulnerability = DS.CreateSoundBufferFromFile(App.path & "\sound\invulnerability.wav", dsBDesc, w)
    Set dsInvPowerDown = DS.CreateSoundBufferFromFile(App.path & "\sound\invpowerdown.wav", dsBDesc, w)
    Set dsExtraLife = DS.CreateSoundBufferFromFile(App.path & "\sound\extralife.wav", dsBDesc, w)
    
    'The next lines initialize duplicate sound buffers from the existing ones
    
    For intCount = 0 To UBound(dsLaser2Duplicate)
        Set dsLaser2Duplicate(intCount) = DS.DuplicateSoundBuffer(dsLaser2)
    Next
    For intCount = 0 To UBound(dsEnemyFireDuplicate)
        Set dsEnemyFireDuplicate(intCount) = DS.DuplicateSoundBuffer(dsEnemyFire)
    Next
    For intCount = 0 To UBound(dsNoHitDuplicate)
        Set dsNoHitDuplicate(intCount) = DS.DuplicateSoundBuffer(dsNoHit)
    Next
    For intCount = 0 To UBound(dsExplosionDuplicate)
        Set dsExplosionDuplicate(intCount) = DS.DuplicateSoundBuffer(dsExplosion)
    Next
    
    Exit Sub

ErrorHandler:                                                       'Handle any errors
    EndGame                                                         'release any objects
    MsgBox "Unable to create Direct Sound object." & vbCrLf & "Check to ensure that a sound card is installed and working properly."
                                                                    'display this message
    End                                                             'end the program
    
End Sub


