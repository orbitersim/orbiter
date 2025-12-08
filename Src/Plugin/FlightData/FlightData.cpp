// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//                 ORBITER MODULE: FlightData
//                  Part of the ORBITER SDK
//
// FlightData.cpp
// Dialog box for displaying vessel flight data during the
// simulation.
// ==============================================================

#define ORBITER_MODULE
#include <vector>
#include <functional>
#include <algorithm>
#include "orbitersdk.h"
#include "imgui.h"
#include "imgui_extras.h"
#include "implot.h"
#include "IconsFontAwesome6.h"

// ==============================================================
// The module interface class - singleton implementation

#define NDATA 256

namespace oapi {

	class DataStream {
	public:
		std::string m_name;
		std::vector<float> m_data;
		std::function<float(VESSEL *)> m_convert;
		std::string m_header;
		std::string m_format;
		int m_idx;
		ImAxis_ m_axis;

		DataStream(const char *name, ImAxis_ axis, std::function<float(VESSEL *)> convert, const char *header, const char *format) {
			m_idx = 0;
			m_data.resize(NDATA, NAN);
			m_name = name;
			m_axis = axis;
			m_convert = convert;
			m_header = header;
			m_format = format;
		}

		void AddPoint(VESSEL *vessel, FILE *f) {
			float data = m_convert(vessel);
			m_data[m_idx] = data;
			// Force the axis to include 0
			m_data[(m_idx + 1) % NDATA] = 0.0;
			m_data[(m_idx + 2) % NDATA] = NAN;
			m_idx = (m_idx + 1) % NDATA;

			if(f)
				fprintf(f, m_format.c_str(), data);
		}

		int GetOffset() { return m_idx; }
		float *GetData() { return m_data.data(); }
	};

	class StreamGraph {
	public:
		std::vector<DataStream> m_datastreams;
		std::string m_title;
		std::string m_y1legend;
		std::string m_y2legend;
		std::string m_y3legend;
		bool m_enabled;
		
		StreamGraph(const char *title, const char *y1, const char *y2, const char *y3) {
			m_title = title;
			m_y1legend = y1;
			m_y2legend = y2;
			m_y3legend = y3;
			m_enabled = true;
		}
		void AddDataStream(const char *name, ImAxis_ pos, std::function<float(VESSEL *)> &&convert, const char *header, const char *format) {
			m_datastreams.emplace_back(name, pos, convert, header, format);
		}
		void AddDataPoint(VESSEL *vessel, FILE *f) {
			for(auto &s: m_datastreams) {
				s.AddPoint(vessel, f);
			}
		}

		void ResetData() {
			for(auto &s: m_datastreams) {
				std::fill(s.m_data.begin(), s.m_data.end(), NAN);
			}
		}

		void Draw() {
			if(!m_enabled) return;

            if (ImPlot::BeginPlot(m_title.c_str())) {
				ImPlot::SetupAxis(ImAxis_X1, NULL, ImPlotAxisFlags_AutoFit | ImPlotAxisFlags_NoTickLabels);
				ImPlot::SetupAxisLimitsConstraints(ImAxis_X1, 0, NDATA);

				ImPlot::SetupAxis(ImAxis_Y1, m_y1legend.c_str(), ImPlotAxisFlags_AutoFit);
				if(!m_y2legend.empty())
					ImPlot::SetupAxis(ImAxis_Y2, m_y2legend.c_str(), ImPlotAxisFlags_AuxDefault | ImPlotAxisFlags_AutoFit);
				if(!m_y3legend.empty())
					ImPlot::SetupAxis(ImAxis_Y3, m_y3legend.c_str(), ImPlotAxisFlags_AuxDefault | ImPlotAxisFlags_AutoFit);

				for(auto &s: m_datastreams) {
					ImPlot::SetAxes(ImAxis_X1, s.m_axis);
					ImPlot::PlotLine(s.m_name.c_str(), s.GetData(), NDATA, 1.0, 0.0, ImPlotLineFlags_None, s.GetOffset());
				}
				ImPlot::EndPlot();
			}
		}
	};

	class FlightData : public Module, public ImGuiDialog {
		VESSEL* m_pVessel;       ///> current focus vessel
		double m_sysT;           ///> current system time
		float m_DT;              ///> sample interval
		bool m_bResetLog;        ///> Reset log file on next output
		bool m_bLogging;         ///> Logging active
		std::string m_sLogfile;  ///> Log file name
		FILE *m_LogFile;
		std::vector<StreamGraph> m_graphs;
		DWORD m_dwCmd;
		int m_dwMenuCmd;
		std::string m_vesselName;

		StreamGraph &CreateGraph(const char *title, const char *left, const char *right = "", const char *right2 = "") {
			return m_graphs.emplace_back(title, left, right, right2);
		}
	public:
		FlightData(HINSTANCE hDLL):Module(hDLL),ImGuiDialog("Flight Data Monitor") {
			m_pVessel = NULL;
			m_sysT = 0.0;
			m_DT = 0.1f;
			m_bResetLog = true;
			m_bLogging = false;
			m_sLogfile = "FlightData.log";
			m_LogFile = NULL;

			auto lalt  = [](VESSEL *v) {return (float)v->GetAltitude() / 1000.0f;};
			auto lvspd = [](VESSEL *v) {
				VECTOR3 vel;
				float vspeed = 0.0f;
				if (v->GetAirspeedVector(FRAME_HORIZON, vel))
					vspeed = vel.y;
				return vspeed * 0.001f;
			};
			auto laspd = [](VESSEL *v) {return (float)v->GetAirspeed() * 0.001f;};
			auto lmach = [](VESSEL *v) {return (float)v->GetMachNumber();};
			auto ltemp = [](VESSEL *v) {return (float)v->GetAtmTemperature();};
			auto lspre = [](VESSEL *v) {return (float)v->GetAtmPressure() * 0.001f;};
			auto ldpre = [](VESSEL *v) {return (float)v->GetDynPressure() * 0.001f;};
			auto llift = [](VESSEL *v) {return (float)v->GetLift() * 0.001f;};
			auto ldrag = [](VESSEL *v) {return (float)v->GetDrag() * 0.001f;};
			auto lld   = [](VESSEL *v) {return (float)v->GetLift() / (float)v->GetDrag();};
			auto lmass = [](VESSEL *v) {return (float)v->GetMass() * 0.001f;};
			auto lprop = [](VESSEL *v) {return (float)v->GetTotalPropellantMass() * 0.001f;};
			auto laoa  = [](VESSEL *v) {return (float)v->GetAOA() * (float)DEG;};
			auto lslip = [](VESSEL *v) {return (float)v->GetSlipAngle() * (float)DEG;};

			StreamGraph &alt = CreateGraph("Altitude", "Altitude (km)", "Speed (km/s)");
			alt.AddDataStream("Altitude",       ImAxis_Y1, lalt,  " _______ALT", " %10.4f");
			alt.AddDataStream("Vertical speed", ImAxis_Y2, lvspd, " ___VSPEED",  " %9.2f");

			StreamGraph &speed = CreateGraph("Speed", "Speed (km/s)", "Mach number");
			speed.AddDataStream("Airspeed", ImAxis_Y1, laspd, " _AIRSPEED", " %9.2f");
			speed.AddDataStream("Mach",     ImAxis_Y2, lmach, " __MACH",    " %6.2f");

			StreamGraph &atm = CreateGraph("Atmosphere", "Temperature (K)", "Pressure (kPa)");
			atm.AddDataStream("Temperature", ImAxis_Y1, ltemp, " ___TEMP",    " %7.1f");
			atm.AddDataStream("Static",      ImAxis_Y2, lspre, " _______STP", " %10.4f");
			atm.AddDataStream("Dynamic",     ImAxis_Y2, ldpre, " _______DNP", " %10.4f");

			StreamGraph &ld = CreateGraph("Lift & Drag", "Force (kN)", "L/D");
			ld.AddDataStream("Lift", ImAxis_Y1, llift, " _____LIFT", " %9.2f");
			ld.AddDataStream("Drag", ImAxis_Y1, ldrag, " _____DRAG", " %9.2f");
			ld.AddDataStream("L/D",  ImAxis_Y2, lld,   " _____L/D",  " %8.3f");

			StreamGraph &mass = CreateGraph("Mass", "Mass (Ton)");
			mass.AddDataStream("Total",      ImAxis_Y1, lmass, " _TOTMASS", " %8.0f");
			mass.AddDataStream("Propellant", ImAxis_Y1, lprop, " _PRPMASS", " %8.0f");

			StreamGraph &aoa = CreateGraph("AoA", u8"Angle (°)");
			aoa.AddDataStream("AOA",  ImAxis_Y1, laoa,  " ____AOA", " %7.1f");
			aoa.AddDataStream("Slip", ImAxis_Y1, lslip, " ___SLIP", " %7.1f");

			static char* desc = (char*)"Open a window to track flight parameters of a spacecraft.";
			m_dwCmd = oapiRegisterCustomCmd((char*)"Flight Data Monitor", desc, hookOpenDlg, this);
			m_dwMenuCmd = oapiRegisterCustomMenuCmd("Flight Data", "MenuInfoBar/FlightData.png", hookOpenDlg, this);

		}

		static void hookOpenDlg(void *ctx) {
			FlightData *self = (FlightData *)ctx;
			oapiOpenDialog(self);
		}

		~FlightData() {
			oapiUnregisterCustomMenuCmd(m_dwMenuCmd);
			oapiUnregisterCustomCmd(m_dwCmd);
		}

		void ResetData() {
			for(auto &graph: m_graphs) {
				graph.ResetData();
			}
		}

		void StartRecording() {
			m_LogFile = fopen(m_sLogfile.c_str(), m_bResetLog ? "wt" : "at");
			if(!m_LogFile) {
				std::string error = std::string("Cannot open file ") + m_sLogfile + " for writing";
				oapiAddNotification(OAPINOTIF_ERROR, "Flight data recording error", error.c_str());
				return;
			}
			if (m_bResetLog) { // write out header
				fprintf(m_LogFile, "Orbiter Flight Data Log Record\n");
				fprintf(m_LogFile, "==============================\n");
				fprintf(m_LogFile, "Columns:\n");
				fprintf(m_LogFile, "\tTIME:     simulation time (seconds)\n");
				fprintf(m_LogFile, "\tALT:      altitude (km)\n");
				fprintf(m_LogFile, "\tAIRSPEED: airspeed (km/s)\n");
				fprintf(m_LogFile, "\tVSPEED:   vertical speed (km/s)\n");
				fprintf(m_LogFile, "\tMACH:     Mach number\n");
				fprintf(m_LogFile, "\tTEMP:     freestream temperature (K)\n");
				fprintf(m_LogFile, "\tSTP:      static pressure (kPa)\n");
				fprintf(m_LogFile, "\tDNP:      dynamic pressure (kPa)\n");
				fprintf(m_LogFile, "\tAOA:      angle of attack (deg)\n");
				fprintf(m_LogFile, "\tSLIP:     horizontal slip angle (deg)\n");
				fprintf(m_LogFile, "\tLIFT:     total lift force (kN)\n");
				fprintf(m_LogFile, "\tDRAG:     total drag force (kN)\n");
				fprintf(m_LogFile, "\tL/D:      lift/drag ratio\n");
				fprintf(m_LogFile, "\tTOTMASS:  total vessel mass (ton)\n");
				fprintf(m_LogFile, "\tPRPMASS:  propellant mass (ton)\n\n");
				m_bResetLog = false;
			}
			fprintf(m_LogFile, "# Log started for %s\n", m_vesselName.c_str());
			fprintf(m_LogFile, "# ____TIME");
			for(auto &graph: m_graphs) {
				for(auto &ds: graph.m_datastreams) {
					fprintf(m_LogFile, ds.m_header.c_str());
				}
			}
			fprintf(m_LogFile, "\n");
			oapiAddNotification(OAPINOTIF_INFO, "Flight data recording enabled", m_vesselName.c_str());
		}

		void StopRecording() {
			if(m_LogFile) {
				fprintf(m_LogFile, "# Log stopped for %s\n", m_vesselName.c_str());
				fclose(m_LogFile);
				m_LogFile = NULL;
				oapiAddNotification(OAPINOTIF_INFO, "Flight data recording disabled", m_vesselName.c_str());
			}
		}

		void SetVessel(VESSEL *v) {
			m_pVessel = v;
			m_vesselName = v->GetName();
		}

		void OnDraw() override {
			// vessel
			ImGui::SetNextItemWidth(160.0f);
			if(ImGui::BeginAnimatedCombo("##Vessel", m_vesselName.c_str(), ImGuiComboFlags_HeightLargest)) {
				for (int i = 0; i < oapiGetVesselCount(); i++) {
					OBJHANDLE hVessel = oapiGetVesselByIndex(i);
					VESSEL *vessel = oapiGetVesselInterface(hVessel);
					if (vessel->GetEnableFocus()) {
						const bool is_selected = m_pVessel == vessel;
						if(ImGui::Selectable(vessel->GetName(), is_selected)) {
							if(m_pVessel != vessel) {
								if(m_LogFile) {
									StopRecording();
									SetVessel(vessel);
									StartRecording();
								} else {
									SetVessel(vessel);
								}
								ResetData();
							}
						}

					}
				}
				ImGui::EndAnimatedCombo();
			}
			// sampling period
			ImGui::SameLine();
			ImGui::SetNextItemWidth(80.0f);
			ImGui::SliderFloat("Sampling period", &m_DT, 0.1f, 100.0f, "%.1fs", ImGuiSliderFlags_Logarithmic);

			// reset
			ImGui::SameLine();
			if(ImGui::Button(ICON_FA_ERASER " Reset")) {
				ResetData();
			}

			ImGui::SameLine();
			ImGui::SetNextItemWidth(160.0f);
			if(ImGui::BeginAnimatedCombo("##Graphs", "Graphs", ImGuiComboFlags_HeightLargest)) {
				for(auto &graph: m_graphs) {
					ImGui::Checkbox(graph.m_title.c_str(), &graph.m_enabled);
				}

				ImGui::EndAnimatedCombo();
			}
			

			// start/stop
			bool recording = m_LogFile != NULL;
			if(ImGui::Checkbox("Save to file", &recording)) {
				if(recording) StartRecording();
				else StopRecording();
			}

			ImGui::SameLine();
			if(ImGui::Button(m_bLogging ? ICON_FA_STOP " Stop" : ICON_FA_PLAY " Start")) {
				m_bLogging = !m_bLogging;
			}
			
			int nGraph = std::count_if(m_graphs.begin(), m_graphs.end(), [](auto &item) {return item.m_enabled;});

			int nVGraph = 3;
			int nHgraph = (nGraph < 4) ? 1 : 2;

			// Draw graphs
			if (ImPlot::BeginSubplots("Flight data", nVGraph, nHgraph, ImVec2(-1,600), ImPlotSubplotFlags_NoTitle)) {
				for(auto &graph: m_graphs) {
					graph.Draw();
				}
				ImPlot::EndSubplots();
			}
		}

		void clbkPreStep(double simt, double simdt, double mjd) override {
			if(!m_bLogging) return;

			double syst = oapiGetSysTime(); // ignore time acceleration for graph updates
			if (syst >= m_sysT + m_DT) {
				if (m_LogFile) {
					fprintf(m_LogFile, "%10.2f", simt);
				}
				for(auto &graph: m_graphs) {
					graph.AddDataPoint(m_pVessel, m_LogFile);
				}
				if (m_LogFile) {
					fprintf(m_LogFile, "\n");
				}
				m_sysT = syst;
			}
		}

		void clbkDeleteVessel(OBJHANDLE hVessel) override {
			VESSEL* v = oapiGetVesselInterface(hVessel);
			if (v == m_pVessel) {
				m_pVessel = oapiGetFocusInterface();
				ResetData();
			}
		}
		void clbkSimulationStart (RenderMode mode) override {
			SetVessel(oapiGetFocusInterface());
			m_bResetLog = true;
			m_bLogging = false;
			m_sysT = 0.0;
		}
		void clbkSimulationEnd () override {
			ResetData();
			StopRecording();
			oapiCloseDialog(this);
		}
	};

};

// ==============================================================
// API interface
// ==============================================================


static oapi::FlightData *g_fData;
/// \brief Module entry point 
/// \param hDLL module handle
DLLCLBK void InitModule(HINSTANCE hDLL)
{
	// Create and register the module
	g_fData = new oapi::FlightData(hDLL);
	oapiRegisterModule(g_fData);
}

/// \brief Module exit point 
/// \param hDLL module handle
DLLCLBK void ExitModule(HINSTANCE hDLL)
{
	// Delete the module
	delete g_fData;
}
