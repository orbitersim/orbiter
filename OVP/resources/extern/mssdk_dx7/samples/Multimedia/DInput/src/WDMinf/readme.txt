//-----------------------------------------------------------------------------
// File: Readme.txt
//
// Desc: Readme file for the WDM input sample INFs
//
//  THIS CODE AND INFORMATION IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY
//  KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
//  IMPLIED WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A PARTICULAR
//  PURPOSE.
//
//
// Copyright (c) 1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------

This directory contains sample INFs for WDM gameport and joystick drivers.


Overview

WDM is the driver model to be used for future input device drivers on both 
Windows 98 and Windows 2000.  It is closely related to but not the same as USB.  
To ease the transition from older devices and device drivers to more Plug and 
Play aware technologies, such as USB, support for WDM gameport drivers has been 
added to DirectInput which allows gameport devices to be exposed as present 
when they are configured from the Game Controllers CPL.

The DirectX 7 version of DirectInput adds this support however user systems are 
not upgraded to use WDM gameport drivers as drivers for gameport joysticks have 
not yet become sufficiently widely available.


WDMJOY.INF
    WDMJoy.inf is the English language Windows 2000 file of the same name with 
    the necessary modifications made to install the WDM driver in the 
    Windows 98 environment.  
    
    To install a WDM gameport joystick driver on Windows 98 do the following:
        If you do not have a WDM gameport installed, use the GAMEPORT.INF (see 
        below) to install one.
        Use the Game Controllers CPL to add a WDM joystick, if you do not have 
        one, use WDMJINIT.INF (see below) to add some first.
        When prompted supply the path of this INF as the location to search for 
        drivers.  (This step can be short-cut by copying this inf to the 
        %windir%\inf directory before starting.)
        When prompted for "DirectX 7 - WDM Gamport support disk", supply the 
        path of HIDGame.sys file from the redist (or debug) directory.


GAMEPORT.INF
    Gameport.inf is the English language Windows 2000 file of the same name 
    with the necessary modifications made to install the WDM driver in the 
    Windows 98 environment.  Gameports on Windows 98 must use the older 
    VxD/Drv drivers in addition to WDM drivers as they are required to support 
    legacy multimedia functionality and to provide compatibility with the VxD 
    joystick mini-drivers currently in use.
    
    
    To install a WDM gameport on Windows 98 do the following:
        Copy this INF and the GameEnum.sys file from the redist (or debug) 
        into a new directory.  
        Select your current gameport from the Device Manager and ask to update 
        the driver.  
        Specify the directory into which you copied this INF and GameEnum.sys 
        as the location to search for an updated driver.  
        You should be prompted for your original Win98 media to copy the 
        non-WDM drivers (pointing to your system directory should be fine as 
        these files should already be in place).
    
        If no driver can be found for your gameport, you may need specific WDM 
        drivers with the gameport you have.

WDMJINIT.INF
    WDMJInit.inf is provided to allow the registry setting for a set of analog 
    devices to be added by running the INF directly.  No drivers are installed 
    but the settings used by DirectInput to treat these analog devices as PNP 
    devices are created.

OEMSETUP.INF
    OEMSetup.inf is from the Windows 2000 DDK HIDGame sample with the 
    necessary modifications made to install the WDM driver in the Windows 98 
    environment.  It is important to note that since registry setting must be 
    present before DirectInput can start the process of creating fake PnP 
    events to expose a gameport device on a gameport, some initial set up must 
    be still be performed by a setup program or INF (such as WDMJINIT.INF).
