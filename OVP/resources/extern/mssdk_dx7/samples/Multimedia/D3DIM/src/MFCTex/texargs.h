//-----------------------------------------------------------------------------
// File: TexArgs.h
//
// Desc: Header file to declare texture stage state arguments used for the app.
//
// Copyright (c) 1997-1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------
#ifndef TEX_ARGS_H
#define TEX_ARGS_H




//-----------------------------------------------------------------------------
// String arrays for filling the form's combo boxes
//-----------------------------------------------------------------------------
CHAR* g_astrPresetEffects[] =
{
    "Modulate", "Modulate Alpha", "Add", "Decal Alpha", "Colored Light Map",
    "Inverse Colored Light Map", "Single Channel Light Map",
    "Modulate and Late Add",
    "Linear Blend using Texture Alpha", "Linear Blend using Diffuse Alpha",
    "Add with smooth saturation", "Multitexture Subtract",
    "Add Diffuse to light map then modulate", "Detail Modulate", "Detail Add",
    NULL
};

CHAR* g_astrTextureOps[] =
{
    "Disable", "Select Arg 1", "Select Arg 2",
    "Modulate", "Modulate 2x", "Modulate 4x", "Add",
    "Add Signed", "Add Signed 2x", "Subtract",
    "Add Smooth", "Blend Diffuse", "Blend Texture",
    "Blend Factor",
    NULL
};

CHAR* g_astrTextureArgs1[] =
{
    "Diffuse", "Current", "Texture",
    "Diffuse Inv", "Current Inv", "Texture Inv",
    "Tex w/Alpha", NULL
};

CHAR* g_astrTextureArgs2[] =
{
    "Diffuse", "Current", "Factor",
    "Diffuse Inv", "Current Inv", "Factor Inv",
    "Diffuse w/Alpha", "Current w/Alpha", "Factor w/Alpha",
    NULL
};




//-----------------------------------------------------------------------------
// D3DTOP values corresponding to the strings above for the dropdown lists
//-----------------------------------------------------------------------------
WORD aTexOps[] =
{
    NULL, D3DTOP_DISABLE, D3DTOP_SELECTARG1, D3DTOP_SELECTARG2,
    D3DTOP_MODULATE, D3DTOP_MODULATE2X, D3DTOP_MODULATE4X, D3DTOP_ADD,
    D3DTOP_ADDSIGNED, D3DTOP_ADDSIGNED2X, D3DTOP_SUBTRACT,
    D3DTOP_ADDSMOOTH, D3DTOP_BLENDDIFFUSEALPHA, D3DTOP_BLENDTEXTUREALPHA,
    D3DTOP_BLENDFACTORALPHA
};

WORD aTexArgs1[] =
{
    NULL, D3DTA_DIFFUSE, D3DTA_CURRENT, D3DTA_TEXTURE,
    D3DTA_DIFFUSE|D3DTA_COMPLEMENT, D3DTA_CURRENT|D3DTA_COMPLEMENT,
    D3DTA_TEXTURE|D3DTA_COMPLEMENT, D3DTA_TEXTURE|D3DTA_ALPHAREPLICATE
};

WORD aTexArgs2[] =
{
    NULL, D3DTA_DIFFUSE, D3DTA_CURRENT, D3DTA_TFACTOR,
    D3DTA_COMPLEMENT|D3DTA_DIFFUSE, D3DTA_COMPLEMENT|D3DTA_CURRENT,
        D3DTA_COMPLEMENT|D3DTA_TFACTOR,
    D3DTA_ALPHAREPLICATE|D3DTA_DIFFUSE, D3DTA_ALPHAREPLICATE|D3DTA_CURRENT,
        D3DTA_ALPHAREPLICATE|D3DTA_TFACTOR,
};





//-----------------------------------------------------------------------------
// Enumerated values to relate the combo box strings (listed above) with the
// combo box indices.
//-----------------------------------------------------------------------------
enum _TEXOPS { toDISABLE, toSELECTARG1, toSELECTARG2,
               toMODULATE, toMODULATE2X, toMODULATE4X, toADD,
               toADDSIGNED, toADDSIGNED2X, toSUBTRACT,
               toADDSMOOTH, toBLENDDIFFUSE, toBLENDTEXTURE,
               toBLENDFACTOR };

enum _TEXARGS1 { ta1DIFFUSE, ta1CURRENT, ta1TEXTURE, ta1INVDIFFUSE,
                ta1INVCURRENT, ta1INVTEXTURE, ta1ALPHATEXTURE };

enum _TEXARGS2 { ta2DIFFUSE, ta2CURRENT, ta2FACTOR,
                 ta2INVDIFFUSE, ta2INVCURRENT, ta2INVFACTOR,
                 ta2ALPHADIFFUSE, ta2ALPHACURRENT, ta2ALPHAFACTOR };




#endif

