// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ======================================================================
// Screen capture dialog
// ======================================================================

#include "DlgCapture.h"
#include "Orbiter.h"
#include "IconsFontAwesome6.h"

#define TRANSLATION_CONTEXT "Dialog Capture"
#include "I18NAPI.h"

extern Orbiter *g_pOrbiter;

// ======================================================================

#include "imgui.h"
#include "imgui_extras.h"

DlgCapture::DlgCapture() : ImGuiDialog(ICON_FA_VOICEMAIL, _("Orbiter: Capture frames"),{323,343}) {
	SetHelp("html/orbiter.chm", "/capture.htm");
}

void DlgCapture::OnDraw() {
	CFG_CAPTUREPRM &prm = g_pOrbiter->Cfg()->CfgCapturePrm;

	ImGui::SeparatorText(_("Save snapshots"));
	ImGui::RadioButton(_("To the clipboard"), &prm.ImageTgt, 0);
	ImGui::RadioButton(_("To file"), &prm.ImageTgt, 1);
	if(prm.ImageTgt == 1) {
		ImGui::SameLine();
		ImGui::InputText("##File", prm.ImageFile, 128);
	}
	if(ImGui::Button(_("Take snapshot"))) {
		oapi::GraphicsClient *gclient = g_pOrbiter->GetGraphicsClient();
		if (prm.ImageTgt) {
			oapi::ImageFileFormat fmt = (oapi::ImageFileFormat)prm.ImageFormat;
			float quality = (float)prm.ImageQuality/10.0f;
			gclient->clbkSaveSurfaceToImage (0, prm.ImageFile, fmt, quality);
			AutoIncrement (prm.ImageFile);
		} else {
			gclient->clbkSaveSurfaceToImage (0, NULL, oapi::IMAGE_BMP);
		}
	}

	ImGui::SeparatorText(_("Save frame sequence"));
	ImGui::InputText(_("To directory"), prm.SequenceDir, 128);
	ImGui::InputInt(_("Counter start"), &prm.SequenceStart);
	ImGui::InputInt(_("Skip frames"), &prm.SequenceSkip);

	if (g_pOrbiter->IsCapturingFrames()) {
		if(ImGui::Button(_("Stop recording")))
			g_pOrbiter->StopCaptureFrames();
	} else if (ImGui::Button(_("Start recording"))) {
		g_pOrbiter->StartCaptureFrames();
	}

	ImGui::SeparatorText(_("File settings"));
	const char* const formats[] = { "BMP", "PNG", "JPG", "TIF" };
	ImGui::Combo(_("File format"), &prm.ImageFormat, formats, IM_ARRAYSIZE(formats));
	ImGui::BeginDisabled(prm.ImageFormat != 2);
	ImGui::SliderInt(_("Image quality"), &prm.ImageQuality, 1, 10);
	ImGui::EndDisabled();

	if(ImGui::Button(_("Reset")))
		g_pOrbiter->Cfg()->SetDefaults_Capture();
}

void DlgCapture::AutoIncrement (char *cbuf)
{
	int i, count, len = strlen(cbuf);
	for (i = len; i > 0; i--)
		if (cbuf[i-1] == '\\') break;
	if (sscanf (cbuf+i, "%d", &count) == 1) {
		int w = len-i;
		sprintf (cbuf+i, "%0*d", w, count+1);
	}
}
