//-----------------------------------------------------------------------------
// File: diutil.cpp
//
// Desc: DirectInput support
//
// Copyright (C) 1995-1999 Microsoft Corporation. All Rights Reserved.
//-----------------------------------------------------------------------------
#define STRICT
#include "DIUtil.h"
#include "resource.h"


//-----------------------------------------------------------------------------
// Function prototypes
//-----------------------------------------------------------------------------
static BOOL CALLBACK EnumDeviceProc( DIDEVICEINSTANCE* pdidi, VOID* pv );
static HRESULT       CreateJoystick( HWND, LPDIRECTINPUTDEVICE2 pdidDevice );




//-----------------------------------------------------------------------------
// Global variables
//-----------------------------------------------------------------------------
#define MAX_INPUT_DEVICES 20

static DIDEVICEINSTANCE     g_didiDevices[MAX_INPUT_DEVICES+1];
static DWORD                g_dwNumDevices = 0;
static LPDIRECTINPUT        g_pDI          = NULL;    // DirectInput object




//-----------------------------------------------------------------------------
// Name: 
// Desc: 
//-----------------------------------------------------------------------------
HRESULT DIUtil_GetDevices( DIDEVICEINSTANCE** ppDevice, DWORD* pdwCount )
{
	if( NULL==ppDevice || NULL==pdwCount )
		return E_INVALIDARG;

	(*ppDevice) = g_didiDevices;
	(*pdwCount) = g_dwNumDevices;
	
	return S_OK;
}




//-----------------------------------------------------------------------------
// Name: 
// Desc: 
//-----------------------------------------------------------------------------
BOOL CALLBACK EnumDeviceProc( DIDEVICEINSTANCE* pdidi, VOID* )
{
	if( g_dwNumDevices >= MAX_INPUT_DEVICES )
		return DIENUM_STOP;

	memcpy( &g_didiDevices[g_dwNumDevices++], pdidi, sizeof(DIDEVICEINSTANCE) );

	return DIENUM_CONTINUE;
}




//-----------------------------------------------------------------------------
// Name: Initialize()
// Desc: Creates and initializes DirectInput objects
//-----------------------------------------------------------------------------
HRESULT DIUtil_Initialize( HWND hWnd )
{
    HRESULT hr;

    // Create the base DirectInput object
    hr = DirectInputCreate( (HINSTANCE)GetWindowLong( hWnd,GWL_HINSTANCE ),
		                    DIRECTINPUT_VERSION, &g_pDI, NULL );
    if( FAILED(hr) )
		return E_FAIL;

    // Enumerate all DirectInput devices
	hr = g_pDI->EnumDevices( 0, (LPDIENUMDEVICESCALLBACK)EnumDeviceProc, NULL,
                             DIEDFL_ATTACHEDONLY );
    if( FAILED(hr) )
        return E_FAIL;

	return S_OK;
}




LPDIRECTINPUTDEVICE2 DIUtil_CreateDevice( HWND hWnd, DIDEVICEINSTANCE* pdidi )
{
    LPDIRECTINPUTDEVICE  pdidTempDevice = NULL;
    LPDIRECTINPUTDEVICE2 pdidDevice     = NULL;
    HRESULT              hr;

	if( NULL == pdidi )
		return NULL;

    // Create a temporary "Device 1" object
    hr = g_pDI->CreateDevice( pdidi->guidInstance, &pdidTempDevice, NULL );
    if( FAILED(hr) )
		return NULL;

    // Get the Device2 interface
    hr = pdidTempDevice->QueryInterface( IID_IDirectInputDevice2,
                                         (VOID**)&pdidDevice );
	pdidTempDevice->Release();
    if( FAILED(hr) )
		return NULL;

	if( ( pdidi->dwDevType & 0xff ) == DIDEVTYPE_JOYSTICK )
		hr = CreateJoystick( hWnd, pdidDevice );

	if( FAILED(hr) )
		return NULL;

	return pdidDevice;
}




HRESULT CreateJoystick( HWND hWnd, LPDIRECTINPUTDEVICE2 pdidDevice )
{
    DIPROPRANGE dipr;
    DIPROPDWORD dipdw;
    HRESULT     hr;

	// Set the joystick's data format.
	hr = pdidDevice->SetDataFormat( &c_dfDIJoystick );
	if( FAILED(hr) )
		return E_FAIL;

	// ForceFeedback requires Exclusive access to the device.
	hr = pdidDevice->SetCooperativeLevel( hWnd, DISCL_EXCLUSIVE | DISCL_FOREGROUND );
	if( FAILED(hr) )
		return E_FAIL;

    // SetParameter() will fail if a device is currently acquired, we are
    // doing this here in case we get careless and forget to call this
    // function either before we call Acquire() or after we call Unacquire().
    pdidDevice->Unacquire();

    // Set the axis ranges for the device. Use the same range for X and Y axes.
	// Set them fairly low since we're only concerned with left, right, etc.
    dipr.diph.dwSize       = sizeof(DIPROPRANGE);
	dipr.diph.dwHeaderSize = sizeof(dipr.diph);
	dipr.diph.dwHow        = DIPH_BYOFFSET;
	dipr.lMin              = RANGE_MIN;  // negative to the left/top
	dipr.lMax              = RANGE_MAX;  // positive to the right/bottom
    dipr.diph.dwObj        = DIJOFS_X;
    
	// Set the x-axis range property
    hr = pdidDevice->SetProperty( DIPROP_RANGE, &dipr.diph );
    if( FAILED(hr) )
        return E_FAIL;

	// Set the y-axis range property
    dipr.diph.dwObj = DIJOFS_Y;
    hr = pdidDevice->SetProperty( DIPROP_RANGE, &dipr.diph );
    if( FAILED(hr) )
        return E_FAIL;

    // Set the deadzone for the device
	dipdw.diph.dwSize       = sizeof(DIPROPDWORD);
	dipdw.diph.dwHeaderSize = sizeof(dipdw.diph);
	dipdw.diph.dwHow        = DIPH_BYOFFSET;
	dipdw.dwData            = DEADZONE;

    // Set the x-axis deadzone property
    dipdw.diph.dwObj         = DIJOFS_X;
    hr = pdidDevice->SetProperty( DIPROP_DEADZONE, &dipdw.diph );
    if( FAILED(hr) )
        return E_FAIL;

    // Set the y-axis deadzone property
    dipdw.diph.dwObj = DIJOFS_Y;
    hr = pdidDevice->SetProperty( DIPROP_DEADZONE, &dipdw.diph );
    if( FAILED(hr) )
        return E_FAIL;

    // Acquire the device
    if( FAILED( pdidDevice->Acquire() ) )
		return E_FAIL;

    return S_OK;
}




BOOL DIUtil_IsForceFeedback( LPDIRECTINPUTDEVICE2 pdidDevice )
{
	// Check to see if the device is a force feedback device
    DIDEVCAPS didc;
	didc.dwSize = sizeof(DIDEVCAPS);
	
	if( SUCCEEDED( pdidDevice->GetCapabilities( &didc ) ) )
	{
		if( didc.dwFlags & DIDC_FORCEFEEDBACK )
			return TRUE;
	}

	return FALSE;
}




//-----------------------------------------------------------------------------
// Name: DIUtil_CleanupDirectInput()
// Desc: Cleans up DirectInput objects
//-----------------------------------------------------------------------------
VOID DIUtil_CleanupDirectInput()
{
    // Release() base object
    if( g_pDI )
        g_pDI->Release();
    g_pDI = NULL;
}




BOOL CALLBACK EnumEffectCallback( LPCDIEFFECTINFO pei, VOID* pv )
{
    GUID* pEffectGUID = (GUID*)pv;;

    // Report back the guid of the effect we enumerated
    if( pEffectGUID )
        *pEffectGUID = pei->guid;

	// Could continue and build a list of effects, but we'll stop after one.
    return DIENUM_STOP;
}




//-----------------------------------------------------------------------------
// Name:
// Desc:
//-----------------------------------------------------------------------------
HRESULT DIUtil_CreateEffect( LPDIRECTINPUTDEVICE2 pdidDevice,
							 FFEffectObject* pEffectObject )
{
	GUID    guidEffect;
	HRESULT hr;

	// Release the effect before recreating it
    if( pEffectObject->pDIEffect )
        pEffectObject->pDIEffect->Release();
    pEffectObject->pDIEffect = NULL;

    // Enumerate for a periodic effect
    hr = pdidDevice->EnumEffects( (LPDIENUMEFFECTSCALLBACK)EnumEffectCallback,
                                  &guidEffect, pEffectObject->dwEffectType );
    if( FAILED(hr) )
		return E_FAIL;

    // Create the effect
    hr = pdidDevice->CreateEffect( guidEffect, &pEffectObject->diEffect,
		                           &pEffectObject->pDIEffect, NULL );
    if( FAILED(hr) )
		return E_FAIL;

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name:
// Desc:
//-----------------------------------------------------------------------------
HRESULT DIUtil_SetupConstantForceEffect( FFEffectObject* pEffectObject,
										 LPDIRECTINPUTDEVICE2 pdidDevice,
										 BOOL bFudgeForBounce )
{
    HRESULT         hr;
    DWORD           rgdwAxes[2];
    LONG            rglDirections[2];
    DICONSTANTFORCE dicf;

    if( NULL == pEffectObject || NULL == pdidDevice )
        return E_FAIL;

    // These fields are the same for all effects we will be creating
    pEffectObject->diEffect.dwSamplePeriod          = 0; // use default sample period
    pEffectObject->diEffect.dwTriggerButton         = DIEB_NOTRIGGER;
    pEffectObject->diEffect.dwTriggerRepeatInterval = 0;
    pEffectObject->diEffect.rgdwAxes                = rgdwAxes;
    pEffectObject->diEffect.rglDirection            = rglDirections;

    // Prepare the DICONSTANTFORCE structure
    dicf.lMagnitude  = 10000;

	if( bFudgeForBounce ) 
	{
	    // Axes and directions to use
		rgdwAxes[0]      = DIJOFS_X;
		rgdwAxes[1]      = DIJOFS_Y;
		rglDirections[0] = 0;
		rglDirections[1] = 0;
	}
	else
	{
		// Axes and directions to use
		rgdwAxes[0]      = DIJOFS_Y;
		rglDirections[0] = 1;
	}

    // Prepare the DIEFFECT structure
	if( bFudgeForBounce )
	{
		pEffectObject->diEffect.dwFlags    = DIEFF_OBJECTOFFSETS | DIEFF_POLAR;
		pEffectObject->diEffect.dwDuration = 200000;
		pEffectObject->diEffect.cAxes      = 2;
	}
	else
	{
		pEffectObject->diEffect.dwFlags    = DIEFF_OBJECTOFFSETS | DIEFF_CARTESIAN;
        pEffectObject->diEffect.dwDuration = 20000;
        pEffectObject->diEffect.cAxes      = 1;
	}
    pEffectObject->diEffect.lpEnvelope            = NULL;
    pEffectObject->diEffect.cbTypeSpecificParams  = sizeof(DICONSTANTFORCE);
    pEffectObject->diEffect.lpvTypeSpecificParams = &dicf;

    pEffectObject->dwEffectType = DIEFT_CONSTANTFORCE;

	hr = DIUtil_CreateEffect( pdidDevice, pEffectObject );
	if( FAILED(hr) )
		return E_FAIL;

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name:
// Desc:
//-----------------------------------------------------------------------------
HRESULT DIUtil_SetupPeriodicEffect( FFEffectObject* pEffectObject,
								    LPDIRECTINPUTDEVICE2 pdidDevice )
{
    HRESULT         hr;
    DWORD           rgdwAxes[2];
    LONG            rglDirections[2];
    DIPERIODIC      dipf;

    // Make sure that we have a non-NULL device object
    if( NULL == pEffectObject || NULL == pdidDevice )
        return E_FAIL;

    // These fields are the same for all effects we will be creating
    pEffectObject->diEffect.dwSamplePeriod          = 0; // use default sample period
    pEffectObject->diEffect.dwTriggerButton         = DIEB_NOTRIGGER;
    pEffectObject->diEffect.dwTriggerRepeatInterval = 0;
    pEffectObject->diEffect.rgdwAxes                = rgdwAxes;
    pEffectObject->diEffect.rglDirection            = rglDirections;

    // Prepare the DIENVELOPE structure. Shape the explode effect so that
	// it starts at it's peak and then fades out
    pEffectObject->diEnvelope.dwAttackLevel = 0;
    pEffectObject->diEnvelope.dwAttackTime  = 0;
    pEffectObject->diEnvelope.dwFadeLevel   = 0;
    pEffectObject->diEnvelope.dwFadeTime    = 1000000;

    // Pepare the DIPERIODIC structure
    dipf.dwMagnitude = 10000;
    dipf.lOffset     = 0;
    dipf.dwPhase     = 0;
    dipf.dwPeriod    = 100000;

    // Axes and directions to use
    rgdwAxes[0]      = DIJOFS_X;
    rglDirections[0] = 0;

    // Prepare the DIEFFECT structure
    pEffectObject->diEffect.dwFlags               = DIEFF_OBJECTOFFSETS | DIEFF_CARTESIAN;
    pEffectObject->diEffect.dwDuration            = 1000000;
    pEffectObject->diEffect.cAxes                 = 1;
    pEffectObject->diEffect.lpEnvelope            = &pEffectObject->diEnvelope;
    pEffectObject->diEffect.cbTypeSpecificParams  = sizeof(DIPERIODIC);
    pEffectObject->diEffect.lpvTypeSpecificParams = &dipf;

    pEffectObject->dwEffectType = DIEFT_PERIODIC;

	hr = DIUtil_CreateEffect( pdidDevice, pEffectObject );
	if( FAILED(hr) )
		return E_FAIL;

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name:
// Desc:
//-----------------------------------------------------------------------------
HRESULT DIUtil_PlayDirectionalEffect( FFEffectObject* pEffectObject,
									  LONG lDirection )
{
	if( pEffectObject )
	{
		if( pEffectObject->pDIEffect )
		{
			// Set the direction. Since this is a polar coordinate effect, we will
			// pass the angle in as the direction relative to the x-axis, and will
			// leave 0 for the y-axis direction. Direction is passed in in degrees,
			// we convert to 100ths of a degree to make it easier for the caller.
			LONG rglDirections[2];
			rglDirections[0] = lDirection * 100;
			rglDirections[1] = NULL;
			
			// Initialize DIEFFECT structure
			DIEFFECT effect;
			ZeroMemory( &effect, sizeof(DIEFFECT) );
			effect.dwSize       = sizeof(DIEFFECT);
			effect.dwFlags      = DIEFF_OBJECTOFFSETS | DIEFF_POLAR;
			effect.cAxes        = 2;
			effect.rglDirection = rglDirections;

			pEffectObject->pDIEffect->SetParameters( &effect, DIEP_DIRECTION );

		    // Play the effect
			return pEffectObject->pDIEffect->Start( 1, 0 );
		}
	}

	return E_INVALIDARG;
}




//-----------------------------------------------------------------------------
// Name:
// Desc:
//-----------------------------------------------------------------------------
HRESULT DIUtil_PlayEffect( FFEffectObject* pEffectObject )
{
	if( pEffectObject )
	{
		if( pEffectObject->pDIEffect )
		{
		    // Play the effect
			return pEffectObject->pDIEffect->Start( 1, 0 );
		}
	}

	return E_INVALIDARG;
}




//-----------------------------------------------------------------------------
// Name:
// Desc:
//-----------------------------------------------------------------------------
FFEffectObject* DIUtil_CreateEffect()
{
	FFEffectObject* pEffectObject = new FFEffectObject;

    // Initialize DIEFFECT and DIENVELOPE structures
    ZeroMemory( &pEffectObject->diEffect, sizeof(DIEFFECT) );
    pEffectObject->diEffect.dwSize   = sizeof(DIEFFECT);
	pEffectObject->diEffect.dwGain   = 7500L;

    ZeroMemory( &pEffectObject->diEnvelope, sizeof(DIENVELOPE) );
    pEffectObject->diEnvelope.dwSize = sizeof(DIENVELOPE);

	pEffectObject->pDIEffect = NULL;
	
	return pEffectObject;
}




//-----------------------------------------------------------------------------
// Name:
// Desc:
//-----------------------------------------------------------------------------
VOID DIUtil_DeleteEffect( FFEffectObject* pEffectObject )
{
	if( pEffectObject )
	{
		if( pEffectObject->pDIEffect )
			pEffectObject->pDIEffect->Release();

		delete pEffectObject;
	}
}




