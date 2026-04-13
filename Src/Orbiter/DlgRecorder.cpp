// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ======================================================================
// Flight recorder dialog
// ======================================================================

#include "DlgRecorder.h"
#include "Orbiter.h"
#include "OrbiterAPI.h"
#include "imgui.h"
#include "imgui_extras.h"
#include "IconsFontAwesome6.h"

#define TRANSLATION_CONTEXT "Dialog Recorder"
#include "I18NAPI.h"

extern Vessel *g_focusobj;
extern Orbiter *g_pOrbiter;

DlgRecorder::DlgRecorder() : ImGuiDialog(ICON_FA_FILM, _("Orbiter: Flight recorder/player"), {329,354}) {
	SetHelp("html/orbiter.chm", "/recorder.htm");
    strcpy(m_ScenarioFile, "test_record");
}

void DlgRecorder::OnDraw() {
    int status = g_pOrbiter->RecorderStatus();
	// TRANSLATORS: Recording state
    static const char *statestr[3] = {_("Status: Normal"), _("Status: Recording"), _("Status: Playing")};
    ImGui::TextUnformatted(statestr[status]);

    switch(status) {
        case 0: DrawNormalRecording(false); break;
        case 1: DrawNormalRecording(true); break;
        case 2: DrawPlaying(); break;
    }
}

void DlgRecorder::DrawNormalRecording(bool recording) {
    if(recording) {
        if(ImGui::Button(_("STOP"))) {
            g_pOrbiter->ToggleRecorder ();
        }
    } else {
        if(ImGui::Button(_("REC"))) {
            if(!g_pOrbiter->ToggleRecorder ())
                ImGui::OpenPopup("Warning");
        }
    }

    bool unused_open = true;
    if (ImGui::BeginPopupModal("Warning", &unused_open))
    {
        ImGui::TextUnformatted(_("A flight record under this name already exists!"));
        if(ImGui::Button(_("Overwrite"))) {
            g_pOrbiter->ToggleRecorder (true);
            ImGui::CloseCurrentPopup();
        }
        ImGui::SameLine();
        if(ImGui::Button(_("Cancel"))) {
            ImGui::CloseCurrentPopup();
        }
        ImGui::EndPopup();
    }

    ImGui::Separator();
    ImGui::InputText(_("Scenario"), m_ScenarioFile, sizeof(m_ScenarioFile));
	// TRANSLATORS: Advanced options
    if(ImGui::BeginAnimatedCollapsingHeader(_("Advanced"))) {
        ImGui::Checkbox(_("Record Time Acceleration"), &g_pOrbiter->Cfg()->CfgRecPlayPrm.bRecordWarp);
        ImGui::Checkbox(_("Record Focus Events"), &g_pOrbiter->Cfg()->CfgRecPlayPrm.bRecordFocus);
        ImGui::BeginDisabled(recording);
        ImGui::Checkbox(_("Sampling in System Time Steps"), &g_pOrbiter->Cfg()->CfgRecPlayPrm.bSysInterval);
        //char buf[20]="";
        //ImGui::InputText("sec. sampling", buf, 20);
        ImGui::Separator();
        ImGui::TextUnformatted(_("Attitude Data"));
		// TRANSLATORS: keep the ###ef1 suffix (required by ImGui)
        ImGui::RadioButton(_("Ecliptic Frame###ef1"), &g_pOrbiter->Cfg()->CfgRecPlayPrm.RecordAttFrame, 0);
        ImGui::RadioButton(_("Local Horizon Frame"), &g_pOrbiter->Cfg()->CfgRecPlayPrm.RecordAttFrame, 1);

        ImGui::Separator();
        ImGui::TextUnformatted(_("Position / Velocity Data"));
		// TRANSLATORS: keep the ###ef2 suffix (required by ImGui)
        ImGui::RadioButton(_("Ecliptic Frame###ef2"), &g_pOrbiter->Cfg()->CfgRecPlayPrm.RecordPosFrame, 0);
        ImGui::RadioButton(_("Equatorial Frame"), &g_pOrbiter->Cfg()->CfgRecPlayPrm.RecordPosFrame, 1);
        ImGui::EndDisabled();
        ImGui::EndAnimatedCollapsingHeader();
    }
}

void DlgRecorder::DrawPlaying() {
    if(ImGui::Button(_("STOP"))) {
        g_pOrbiter->EndPlayback ();
    }
    ImGui::Separator();
    ImGui::TextUnformatted(_("Replay Options"));
    ImGui::Checkbox(_("Show Inflight Notes"), &g_pOrbiter->Cfg()->CfgRecPlayPrm.bShowNotes);
    if(ImGui::Checkbox(_("Play at Recording Speed"), &g_pOrbiter->Cfg()->CfgRecPlayPrm.bReplayWarp)) {
		if (g_pOrbiter->Cfg()->CfgRecPlayPrm.bReplayWarp) {
			extern double RecordingSpeed;
			g_pOrbiter->SetWarpFactor (RecordingSpeed, true);
		}
    }
    if(ImGui::Checkbox(_("Use Recorded Camera Settings"), &g_pOrbiter->Cfg()->CfgRecPlayPrm.bReplayCam)) {
		if (g_pOrbiter->Cfg()->CfgRecPlayPrm.bReplayCam) {
			// should set camera according to current playback status here!
		}
    }
    if(ImGui::Checkbox(_("Use Recorded Focus Events"), &g_pOrbiter->Cfg()->CfgRecPlayPrm.bReplayFocus)) {
		if (g_pOrbiter->Cfg()->CfgRecPlayPrm.bReplayFocus) {
			extern Vessel *vfocus;
			if (vfocus && vfocus != g_focusobj) g_pOrbiter->SetFocusObject (vfocus);
			// do we need to check if vfocus is valid?
		}
    }
    if(ImGui::Button(_("Editor"))) {
        g_pOrbiter->FRecorder_ToggleEditor();        
    }
}

void DlgRecorder::GetRecordName (char *str, int maxlen) const
{
	strncpy(str, m_ScenarioFile, maxlen);
}
