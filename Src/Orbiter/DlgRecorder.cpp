// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ======================================================================
// Flight recorder dialog
// ======================================================================

#include "DlgRecorder.h"
#include "Orbiter.h"
#include "OrbiterAPI.h"
#include "imgui.h"
#include "IconsFontAwesome6.h"

extern Vessel *g_focusobj;
extern Orbiter *g_pOrbiter;

DlgRecorder::DlgRecorder() : ImGuiDialog(ICON_FA_FILM " Flight recorder/player", {329,354}) {
    strcpy(m_ScenarioFile, "test_record");
}

void DlgRecorder::OnDraw() {
    int status = g_pOrbiter->RecorderStatus();
    static const char *statestr[3] = {"Status: Normal", "Status: Recording", "Status: Playing"};
    ImGui::TextUnformatted(statestr[status]);

    switch(status) {
        case 0: DrawNormalRecording(false); break;
        case 1: DrawNormalRecording(true); break;
        case 2: DrawPlaying(); break;
    }
}

void DlgRecorder::DrawNormalRecording(bool recording) {
    if(recording) {
        if(ImGui::Button("STOP")) {
            g_pOrbiter->ToggleRecorder ();
        }
    } else {
        if(ImGui::Button("REC")) {
            if(!g_pOrbiter->ToggleRecorder ())
                ImGui::OpenPopup("Warning");
        }
    }

    bool unused_open = true;
    if (ImGui::BeginPopupModal("Warning", &unused_open))
    {
        ImGui::TextUnformatted("A flight record under this name already exists!");
        if(ImGui::Button("Overwrite")) {
            g_pOrbiter->ToggleRecorder (true);
            ImGui::CloseCurrentPopup();
        }
        ImGui::SameLine();
        if(ImGui::Button("Cancel")) {
            ImGui::CloseCurrentPopup();
        }
        ImGui::EndPopup();
    }

    ImGui::Separator();
    ImGui::InputText("Scenario", m_ScenarioFile, sizeof(m_ScenarioFile));
    if(ImGui::CollapsingHeader("Advanced")) {
        ImGui::Checkbox("Record Time Acceleration", &g_pOrbiter->Cfg()->CfgRecPlayPrm.bRecordWarp);
        ImGui::Checkbox("Record Focus Events", &g_pOrbiter->Cfg()->CfgRecPlayPrm.bRecordFocus);
        ImGui::BeginDisabled(recording);
        ImGui::Checkbox("Sampling in System Time Steps", &g_pOrbiter->Cfg()->CfgRecPlayPrm.bSysInterval);
        //char buf[20]="";
        //ImGui::InputText("sec. sampling", buf, 20);
        ImGui::Separator();
        ImGui::TextUnformatted("Attitude Data");
        ImGui::RadioButton("Ecliptic Frame###ef1", &g_pOrbiter->Cfg()->CfgRecPlayPrm.RecordAttFrame, 0);
        ImGui::RadioButton("Local Horizon Frame", &g_pOrbiter->Cfg()->CfgRecPlayPrm.RecordAttFrame, 1);

        ImGui::Separator();
        ImGui::TextUnformatted("Position / Velocity Data");
        ImGui::RadioButton("Ecliptic Frame###ef2", &g_pOrbiter->Cfg()->CfgRecPlayPrm.RecordPosFrame, 0);
        ImGui::RadioButton("Equatorial Frame", &g_pOrbiter->Cfg()->CfgRecPlayPrm.RecordPosFrame, 1);
        ImGui::EndDisabled();
    }
}

void DlgRecorder::DrawPlaying() {
    if(ImGui::Button("STOP")) {
        g_pOrbiter->EndPlayback ();
    }
    ImGui::Separator();
    ImGui::TextUnformatted("Replay Options");
    ImGui::Checkbox("Show Inflight Notes", &g_pOrbiter->Cfg()->CfgRecPlayPrm.bShowNotes);
    if(ImGui::Checkbox("Play at Recording Speed", &g_pOrbiter->Cfg()->CfgRecPlayPrm.bReplayWarp)) {
		if (g_pOrbiter->Cfg()->CfgRecPlayPrm.bReplayWarp) {
			extern double RecordingSpeed;
			g_pOrbiter->SetWarpFactor (RecordingSpeed, true);
		}
    }
    if(ImGui::Checkbox("Use Recorded Camera Settings", &g_pOrbiter->Cfg()->CfgRecPlayPrm.bReplayCam)) {
		if (g_pOrbiter->Cfg()->CfgRecPlayPrm.bReplayCam) {
			// should set camera according to current playback status here!
		}
    }
    if(ImGui::Checkbox("Use Recorded Focus Events", &g_pOrbiter->Cfg()->CfgRecPlayPrm.bReplayFocus)) {
		if (g_pOrbiter->Cfg()->CfgRecPlayPrm.bReplayFocus) {
			extern Vessel *vfocus;
			if (vfocus && vfocus != g_focusobj) g_pOrbiter->SetFocusObject (vfocus);
			// do we need to check if vfocus is valid?
		}
    }
    if(ImGui::Button("Editor")) {
        g_pOrbiter->FRecorder_ToggleEditor();        
    }
}

void DlgRecorder::GetRecordName (char *str, int maxlen) const
{
	strncpy(str, m_ScenarioFile, maxlen);
}
