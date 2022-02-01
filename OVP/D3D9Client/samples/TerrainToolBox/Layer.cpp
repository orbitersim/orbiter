// ==================================================================
// Copyright (c) 2021 Jarmo Nikkanen
// Licensed under the MIT License
// ==================================================================


#include <Windows.h>
#include <windowsx.h>
#include "OrbiterAPI.h"
#include "VesselAPI.h"
#include "ModuleAPI.h"
#include "DrawAPI.h"
#include "ToolBox.h"
#include "resource.h"
#include "gcPropertyTree.h"
#include "QTree.h"
#include <Commctrl.h>
#include <vector>
#include <list>

using namespace std;

extern ToolKit *g_pTK;


// ==================================================================================
//
Layer::Layer(gcPropertyTree* pProp, gcCore2* pCore, SURFHANDLE hSr, LayerType type, string name) : pProp(pProp), pCore(pCore), name(name), type(type), hSource(hSr)
{
	width = height = 0;
	fmt = LrFmt::UNDEF;
	lvl = 0.0;
	string lname = "";
	bAlpha = true;

	hIFil = hIWid = hIHei = hIFmt = hILog = hISec = hIRes = hIRef = hIWtr = NULL;
	hGLig = hGBri = hGGam = hBRed = hBGrn = hBBlu = NULL;

	switch (type) {
	case LayerType::TEXTURE: lname = "Surface texture"; break;
	case LayerType::NIGHT: lname = "Nightlights"; break;
	case LayerType::WATER: lname = "Water mask"; break;
	case LayerType::ELEVATION: lname = "Elevation"; break;
	}
	
	gcCore::SurfaceSpecs specs;

	if (hSource) {
		if (pCore->GetSurfaceSpecs(hSource, &specs, sizeof(gcCore::SurfaceSpecs)))
		{
			width = specs.Width;
			height = specs.Height;
		}
	}
	
	hISec = pProp->SubSection(lname);
	hIFil = pProp->AddEntry("Filename", hISec);
	hILog = pProp->AddEntry("Log Level", hISec);

	if (type == LayerType::ELEVATION) {
		hIRes = pProp->AddEditControl("Elev Scale", IDC_LAYER, hISec, "1.00", this);
		hIRef = pProp->AddEditControl("Elev Ref", IDC_LAYER, hISec, "0.00", this);
	}

	if (name.find(".raw") == string::npos) {
		fmt = LrFmt::DDS;
		hIWid = pProp->AddEntry("Width [px]", hISec);
		hIHei = pProp->AddEntry("Height [px]", hISec);
	}
	else {
		hIWid = pProp->AddEditControl("Width [px]", IDC_LAYER, hISec, "", this);
		hIHei = pProp->AddEditControl("Height [px]", IDC_LAYER, hISec, "", this);

		fmt = LrFmt::RAW_U16;
		hIFmt = pProp->AddComboBox("Format", IDC_LAYER, hISec, this);
		pProp->AddComboBoxItem(hIFmt, "UInt 16");
		pProp->AddComboBoxItem(hIFmt, "SInt 16");
		pProp->AddComboBoxItem(hIFmt, "Float 32");
	}

	if (type == LayerType::NIGHT) 
	{
		hIWtr = pProp->AddComboBox("Has water mask ?", IDC_LAYER, hISec, this);
		pProp->AddComboBoxItem(hIWtr, "No");
		pProp->AddComboBoxItem(hIWtr, "Yes");
		pProp->AddComboBoxItem(hIWtr, "No - All Land");
		pProp->SetComboBoxSelection(hIWtr, 1);
	}

	if (type == LayerType::TEXTURE || type == LayerType::NIGHT)
	{
		hITrs = pProp->AddComboBox("Enable Transparency", IDC_LAYER, hISec, this);
		pProp->AddComboBoxItem(hITrs, "No");
		pProp->AddComboBoxItem(hITrs, "Yes");
		pProp->SetComboBoxSelection(hITrs, 0);

		// ---------------------------------------------------
		hSecGfx = pProp->SubSection("Graphics Adjustments", hISec);
		// ---------------------------------------------------
		hGLig = pProp->AddSlider("Lightness", IDC_LIGHTNESS, hSecGfx);
		hGBri = pProp->AddSlider("Brightness", IDC_BRIGHTNESS, hSecGfx);
		hGGam = pProp->AddSlider("Gamma", IDC_GAMMA, hSecGfx);

		// ---------------------------------------------------
		hSecClr = pProp->SubSection("Color Balance", hSecGfx);
		// ---------------------------------------------------
		hBRed = pProp->AddSlider("Red", IDC_RED, hSecClr);
		hBGrn = pProp->AddSlider("Green", IDC_GREEN, hSecClr);
		hBBlu = pProp->AddSlider("Blue", IDC_BLUE, hSecClr);

		pProp->SetSliderScale(hGLig, -0.4, 0.4, gcPropertyTree::Scale::LINEAR);
		pProp->SetSliderScale(hGBri, 0.625, 1.6, gcPropertyTree::Scale::LOG);
		pProp->SetSliderScale(hGGam, 0.5, 2.0, gcPropertyTree::Scale::LOG);
		pProp->SetSliderScale(hBRed, 0.8333, 1.2, gcPropertyTree::Scale::LOG);
		pProp->SetSliderScale(hBGrn, 0.8333, 1.2, gcPropertyTree::Scale::LOG);
		pProp->SetSliderScale(hBBlu, 0.8333, 1.2, gcPropertyTree::Scale::LOG);

		pProp->SetSliderValue(hGLig, 0.0);
		pProp->SetSliderValue(hGBri, 1.0);
		pProp->SetSliderValue(hGGam, 1.0);
		pProp->SetSliderValue(hBRed, 1.0);
		pProp->SetSliderValue(hBGrn, 1.0);
		pProp->SetSliderValue(hBBlu, 1.0);

		pProp->OpenEntry(hSecGfx, false);
		pProp->OpenEntry(hSecClr, false);
	}
	pProp->OpenEntry(hISec, true);
	UpdateProps();
}


// ==================================================================================
//
Layer::~Layer()
{
	if (hSource) oapiReleaseTexture(hSource);
}


// ==================================================================================
//
FVECTOR4 Layer::GetColor()
{
	float bri = float(pProp->GetSliderValue(hGBri));
	float red = float(pProp->GetSliderValue(hBRed) * bri);
	float grn = float(pProp->GetSliderValue(hBGrn) * bri);
	float blu = float(pProp->GetSliderValue(hBBlu) * bri);
	return FVECTOR4(red, grn, blu, 1.0f);
}


// ==================================================================================
//
FVECTOR4 Layer::GetAdjustments()
{
	float lgh = float(pProp->GetSliderValue(hGLig));
	float gma = float(pProp->GetSliderValue(hGGam));
	return FVECTOR4(lgh, gma, 0.0f, 1.0f);
}


// ==================================================================================
//
bool Layer::GetTransparency()
{
	return (pProp->GetComboBoxSelection(hITrs) == 1);
}


// ==================================================================================
//
int	Layer::GetWaterMode()
{
	return pProp->GetComboBoxSelection(hIWtr);
}


// ==================================================================================
//
Layer::LrFmt Layer::GetFormat()
{
	if (name.find(".raw") == string::npos) return Layer::LrFmt::DDS;
	return LrFmt(2 + pProp->GetComboBoxSelection(hIFmt));
}

double Layer::Max(double a, double b, double c, double d)
{
	double q = max(a, b);
	double w = max(c, d);
	return max(q, w);
}

double Layer::Min(double a, double b, double c, double d)
{
	double q = min(a, b);
	double w = min(c, d);
	return min(q, w);
}

// ==================================================================================
//
void Layer::ComputeLevel(Position points[4])
{
	// Figure out the level the input texture is good for ------------------
	//
	double w1 = Max(points[0].lng, points[3].lng, points[1].lng, points[2].lng);
	double w2 = Min(points[0].lng, points[3].lng, points[1].lng, points[2].lng);
	double h1 = Max(points[0].lat, points[3].lat, points[1].lat, points[2].lat);
	double h2 = Min(points[0].lat, points[3].lat, points[1].lat, points[2].lat);

	//if (w1 > PI) w1 = PI2 - w1;
	//if (w2 > PI) w2 = PI2 - w2;

	// Resolution pixels/rad
	double wr = double(width) / abs(w1 - w2);
	double hr = double(height) / abs(h1 - h2);

	double logw = log(wr*PI/512.0) / log(2.0);
	double logh = log(hr*PI/512.0) / log(2.0);

	lvl = max(logw, logh) + 4;

	UpdateProps();
}


// ==================================================================================
//
void Layer::UpdateProps()
{
	if (hSource) {
		pProp->OpenEntry(hISec);
		pProp->SetValue(hIFil, name);
		pProp->SetValue(hILog, lvl, 1);
		if (name.find(".raw") == string::npos) {
			pProp->SetValue(hIWid, width);
			pProp->SetValue(hIHei, height);
		}
	}
	else pProp->OpenEntry(hISec, false);
	pProp->Update();
}
