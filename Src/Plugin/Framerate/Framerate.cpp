// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//                 ORBITER MODULE: Framerate
//                  Part of the ORBITER SDK
//
// Framerate.cpp
// Dialog box for displaying simulation frame rate.
// ==============================================================

#define ORBITER_MODULE
#include "Orbitersdk.h"
#include "imgui.h"
#include "implot.h"
#define NDATA 256

// ==============================================================
// The module interface class - singleton implementation

namespace oapi {

	/// \brief Plugin for graphically displaying frame rate and time step length.
	class Framerate : public Module, ImGuiDialog {
	public:
		/// \brief Entry point for open dialog callback
		static void hookOpenDlg(void* context);

		/// \brief Time step notification callback
		void clbkPreStep(double simt, double simdt, double mjd);

		void clbkSimulationStart (RenderMode mode);

		void OnDraw();

		Framerate(HINSTANCE hDLL);

		~Framerate();

		void InsertData(float fps, float dt);
	private:
		DWORD m_dwCmd;           ///> Handle for plugin entry in custom command list

		double m_sysT;           ///> current system time
		double m_simT;           ///> current simulation time
		double m_DT;             ///> sample interval
		DWORD m_fcount;          ///> frame counter

		std::vector<float> m_FPS;
		std::vector<float> m_DTPS;
		int m_idx;
	};

} // namespace oapi


// ==============================================================
// API interface
// ==============================================================

static oapi::Framerate *fr;

/// \brief Module entry point 
/// \param hDLL module handle
DLLCLBK void InitModule (HINSTANCE hDLL)
{
	// Create and register the module
	fr = new oapi::Framerate(hDLL);
	oapiRegisterModule(fr);
}

/// \brief Module exit point 
/// \param hDLL module handle
DLLCLBK void ExitModule (HINSTANCE hDLL)
{
	// Delete the module
	delete fr;
}

// --------------------------------------------------------------

oapi::Framerate::Framerate(HINSTANCE hDLL)
	: Module(hDLL), ImGuiDialog("Orbiter Performance Meter", {500,350})
{
	// Register the custom command for the plugin
	static char* desc = (char*)"Simulation frame rate / time step monitor";
	m_dwCmd = oapiRegisterCustomCmd((char*)"Performance Meter", desc, hookOpenDlg, this);

	m_sysT = 0.0;
	m_simT = 0.0;
	m_DT = 0.1;
	m_fcount = 0;

	m_DTPS.resize(NDATA, NAN);
	m_FPS.resize(NDATA, NAN);
	m_idx = 0;
}

void oapi::Framerate::InsertData(float fps, float dt)
{
	m_FPS[m_idx] = fps;
	m_DTPS[m_idx] = dt;
	// Force the axis to include 0
	m_FPS[(m_idx + 1) % NDATA] = 0.0;
	m_FPS[(m_idx + 2) % NDATA] = NAN;
	m_DTPS[(m_idx + 1) % NDATA] = 0.001;
	m_DTPS[(m_idx + 2) % NDATA] = NAN;
	m_idx = (m_idx + 1) % NDATA;
}

void oapi::Framerate::OnDraw()
{
    if (ImPlot::BeginPlot("Performance Meter", ImVec2(-1,0), ImPlotFlags_NoTitle|ImPlotFlags_NoMenus)) {
		ImPlot::SetupAxis(ImAxis_X1, NULL, ImPlotAxisFlags_AutoFit | ImPlotAxisFlags_NoTickLabels);
		ImPlot::SetupAxisLimitsConstraints(ImAxis_X1, 0, NDATA);

        ImPlot::SetupAxis(ImAxis_Y1, "F/s", ImPlotAxisFlags_AutoFit);
		ImPlot::SetupAxisLimitsConstraints(ImAxis_Y1, 0, 10000.0);
        ImPlot::SetupAxis(ImAxis_Y2, "dt/s", ImPlotAxisFlags_AuxDefault | ImPlotAxisFlags_AutoFit);
		ImPlot::SetupAxisScale(ImAxis_Y2, ImPlotScale_Log10);

		ImPlot::SetAxes(ImAxis_X1, ImAxis_Y1);
        ImPlot::PlotLine("FPS", m_FPS.data(), NDATA, 1.0, 0.0, ImPlotLineFlags_None, m_idx);
        ImPlot::SetAxes(ImAxis_X1, ImAxis_Y2);
        ImPlot::PlotLine("dt", m_DTPS.data(), NDATA, 1.0, 0.0, ImPlotLineFlags_None, m_idx);
        ImPlot::EndPlot();
    }
}


// --------------------------------------------------------------

oapi::Framerate::~Framerate()
{
	// Unregister the custom command for calling the plugin
	oapiUnregisterCustomCmd(m_dwCmd);
}

// --------------------------------------------------------------
// Per-frame update

void oapi::Framerate::clbkPreStep(double simt, double simdt, double mjd)
{
	double syst = oapiGetSysTime(); // ignore time acceleration for graph updates
	m_fcount++;

	if (syst >= m_sysT + m_DT) {
		float fps = (float)(m_fcount / (syst - m_sysT));
		float dt = (float)(simt - m_simT) / (float)m_fcount;
		InsertData(fps, dt);
		m_sysT = syst;
		m_simT = simt;
		m_fcount = 0;
	}
}

void oapi::Framerate::clbkSimulationStart (RenderMode mode)
{
	m_sysT = 0.0;
	m_fcount = 0;
	std::fill(m_FPS.begin(), m_FPS.end(), NAN);
	std::fill(m_DTPS.begin(), m_DTPS.end(), NAN);
}
// --------------------------------------------------------------

void oapi::Framerate::hookOpenDlg(void* context)
{
	Framerate *self = (Framerate *)context;
	oapiOpenDialog(self);
}
