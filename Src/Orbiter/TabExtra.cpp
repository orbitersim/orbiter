// Copyright (c) Martin Schweiger
// Licensed under the MIT License

//=============================================================================
// ExtraTab class
//=============================================================================

#define OAPI_IMPLEMENTATION

#include <windows.h>
#include "Launchpad.h"
#include "TabExtra.h"
#include "Orbiter.h"
#include "Rigidbody.h"
#include "Help.h"
#include "imgui.h"
#include <algorithm>

extern Orbiter *g_pOrbiter;
extern CFG_PHYSICSPRM CfgPhysicsPrm_default;
extern CFG_INSTRUMENTPRM CfgInstrumentPrm_default;
extern CFG_DEBUGPRM CfgDebugPrm_default;

namespace {

	const char *DEGSTR = "\xC2\xB0";

	//-------------------------------------------------------------------------
	// Base class for the settings dialogs of the built-in "Extra" items

	class ExtraDialog : public ImGuiDialog {
	public:
		ExtraDialog (const char *title, Config *cfg, float width, const char *help = nullptr, bool reset = true)
			: ImGuiDialog (title), m_cfg (cfg), m_width (width), m_help (help), m_reset (reset) {}

		void Open ()
		{
			if (!active) {
				m_error.clear ();
				Init ();
			}
			oapiOpenDialog (this);
		}

		void Display () override
		{
			const float em = ImGui::GetFontSize ();
			ImGuiViewport *vp = ImGui::GetMainViewport ();
			ImGui::SetNextWindowPos (vp->GetCenter (), ImGuiCond_Appearing, ImVec2 (0.5f, 0.5f));
			ImGui::SetNextWindowSizeConstraints (ImVec2 (m_width * em, 0.0f), ImVec2 (m_width * em, FLT_MAX));
			if (ImGui::Begin (name.c_str (), &active, ImGuiWindowFlags_AlwaysAutoResize | ImGuiWindowFlags_NoCollapse | ImGuiWindowFlags_NoSavedSettings)) {
				DrawContent ();
				if (!m_error.empty ()) {
					ImGui::Spacing ();
					ImGui::PushStyleColor (ImGuiCol_Text, ImVec4 (1.0f, 0.45f, 0.4f, 1.0f));
					ImGui::TextWrapped ("%s", m_error.c_str ());
					ImGui::PopStyleColor ();
				}
				ImGui::Spacing ();
				ImGui::Separator ();
				ImGui::Spacing ();
				float bw = em * 5.0f;
				if (ImGui::Button ("OK", ImVec2 (bw, 0))) {
					m_error.clear ();
					if (Store ()) active = false;
				}
				ImGui::SameLine ();
				if (ImGui::Button ("Cancel", ImVec2 (bw, 0)))
					active = false;
				if (m_reset) {
					ImGui::SameLine ();
					if (ImGui::Button ("Reset", ImVec2 (bw, 0))) {
						m_error.clear ();
						Reset ();
					}
				}
				if (m_help) {
					ImGui::SameLine (ImGui::GetContentRegionMax ().x - bw);
					if (ImGui::Button ("Help", ImVec2 (bw, 0)))
						OpenDefaultHelp (m_help);
				}
			}
			ImGui::End ();
			if (!active) OnClose ();
		}

	protected:
		void OnDraw () override {}
		virtual void Init () = 0;
		virtual void Reset () = 0;
		virtual bool Store () = 0;
		virtual void DrawContent () = 0;

		bool Error (const char *msg) { m_error = msg; return false; }

		void Note (const char *text)
		{
			ImGui::PushStyleColor (ImGuiCol_Text, ImGui::GetStyleColorVec4 (ImGuiCol_TextDisabled));
			ImGui::TextWrapped ("%s", text);
			ImGui::PopStyleColor ();
		}

		void Label (const char *text)
		{
			ImGui::AlignTextToFramePadding ();
			ImGui::TextWrapped ("%s", text);
		}

		Config *m_cfg;
		float m_width;
		const char *m_help;
		bool m_reset;
		std::string m_error;
	};

	//-------------------------------------------------------------------------
	// Dynamic state propagators

	class DynamicsDialog : public ExtraDialog {
	public:
		DynamicsDialog (Config *cfg) : ExtraDialog ("Dynamic state propagators", cfg, 44.0f, "extra_linprop") {}

	protected:
		static inline const int PropId[NPROP_METHOD] = {
			PROP_RK2, PROP_RK4, PROP_RK5, PROP_RK6, PROP_RK7, PROP_RK8,
			PROP_SY2, PROP_SY4, PROP_SY6, PROP_SY8
		};

		void Set (const CFG_PHYSICSPRM &prm)
		{
			const CFG_PHYSICSPRM &def = CfgPhysicsPrm_default;
			n = std::max (1, std::min ((int)MAX_PROP_LEVEL, prm.nLPropLevel));
			for (int i = 0; i < MAX_PROP_LEVEL; i++) {
				const CFG_PHYSICSPRM &src = (i < prm.nLPropLevel ? prm : def);
				mode[i] = 0;
				for (int j = 0; j < NPROP_METHOD; j++)
					if (src.PropMode[i] == PropId[j]) { mode[i] = j; break; }
				ttgt[i] = src.PropTTgt[i];
				atgt[i] = src.PropATgt[i] * DEG;
				tlim[i] = src.PropTLim[i];
				alim[i] = src.PropALim[i] * DEG;
				if (tlim[i] >= 1e9) tlim[i] = 5.0 * ttgt[i];
				if (alim[i] >= 1e9) alim[i] = 2.0 * atgt[i];
			}
			submax = prm.PropSubMax;
		}
		void Init () override { Set (m_cfg->CfgPhysicsPrm); }
		void Reset () override { Set (CfgPhysicsPrm_default); }

		bool Store () override
		{
			char cbuf[256];
			for (int i = 0; i < n; i++) {
				if (mode[i] < 0 || mode[i] >= NPROP_METHOD) {
					snprintf (cbuf, sizeof (cbuf), "Invalid propagator for integration stage %d.", i + 1);
					return Error (cbuf);
				}
				if (ttgt[i] <= 0) {
					snprintf (cbuf, sizeof (cbuf), "Invalid time step target for integration stage %d.", i + 1);
					return Error (cbuf);
				}
				if (atgt[i] <= 0) {
					snprintf (cbuf, sizeof (cbuf), "Invalid angle step target for integration stage %d.", i + 1);
					return Error (cbuf);
				}
				if (i < n - 1) {
					if (tlim[i] <= 0) {
						snprintf (cbuf, sizeof (cbuf), "Invalid time step limit for integration stage %d -> %d.", i + 1, i + 2);
						return Error (cbuf);
					}
					if (alim[i] <= 0) {
						snprintf (cbuf, sizeof (cbuf), "Invalid angle step limit for integration stage %d -> %d.", i + 1, i + 2);
						return Error (cbuf);
					}
					if (i > 0 && (tlim[i] <= tlim[i - 1] || alim[i] <= alim[i - 1]))
						return Error ("Step limits must be in ascending order");
				}
			}
			if (submax < 1)
				return Error ("Invalid value for max. subsamples (integer value >= 1 required).");

			CFG_PHYSICSPRM &prm = m_cfg->CfgPhysicsPrm;
			prm.nLPropLevel = n;
			for (int i = 0; i < n; i++) {
				prm.PropMode[i] = PropId[mode[i]];
				prm.PropTTgt[i] = ttgt[i];
				prm.PropATgt[i] = atgt[i] * RAD;
				prm.PropTLim[i] = (i < n - 1 ? tlim[i] : 1e10);
				prm.PropALim[i] = (i < n - 1 ? alim[i] * RAD : 1e10);
			}
			prm.PropSubMax = submax;
			return true;
		}

		void DrawContent () override
		{
			const float em = ImGui::GetFontSize ();
			char hdr[4][64];
			snprintf (hdr[0], 64, "Target time [s]");
			snprintf (hdr[1], 64, "Target rotation [%s]", DEGSTR);
			snprintf (hdr[2], 64, "Limit time [s]");
			snprintf (hdr[3], 64, "Limit rotation [%s]", DEGSTR);

			ImGui::SeparatorText ("Integration stages");
			if (ImGui::BeginTable ("##stages", 6, ImGuiTableFlags_SizingFixedFit)) {
				ImGui::TableSetupColumn ("Active");
				ImGui::TableSetupColumn ("Propagator", ImGuiTableColumnFlags_WidthStretch);
				for (int k = 0; k < 4; k++)
					ImGui::TableSetupColumn (hdr[k], ImGuiTableColumnFlags_WidthFixed, em * 6.5f);
				ImGui::TableHeadersRow ();
				for (int i = 0; i < MAX_PROP_LEVEL; i++) {
					ImGui::PushID (i);
					ImGui::TableNextRow ();
					ImGui::TableNextColumn ();
					bool act = i < n;
					ImGui::BeginDisabled (!((i == n - 1 && i > 0) || i == n));
					if (ImGui::Checkbox ("##act", &act))
						n = act ? i + 1 : i;
					ImGui::EndDisabled ();
					if (i < n) {
						ImGui::TableNextColumn ();
						ImGui::SetNextItemWidth (-FLT_MIN);
						if (ImGui::BeginCombo ("##prop", RigidBody::PropagatorStr (mode[i]))) {
							for (int j = 0; j < NPROP_METHOD; j++)
								if (ImGui::Selectable (RigidBody::PropagatorStr (j), mode[i] == j))
									mode[i] = j;
							ImGui::EndCombo ();
						}
						ImGui::TableNextColumn ();
						ImGui::SetNextItemWidth (-FLT_MIN);
						ImGui::InputDouble ("##ttgt", ttgt + i, 0.0, 0.0, "%0.2f");
						ImGui::TableNextColumn ();
						ImGui::SetNextItemWidth (-FLT_MIN);
						ImGui::InputDouble ("##atgt", atgt + i, 0.0, 0.0, "%0.1f");
						if (i < n - 1) {
							ImGui::TableNextColumn ();
							ImGui::SetNextItemWidth (-FLT_MIN);
							ImGui::InputDouble ("##tlim", tlim + i, 0.0, 0.0, "%0.2f");
							ImGui::TableNextColumn ();
							ImGui::SetNextItemWidth (-FLT_MIN);
							ImGui::InputDouble ("##alim", alim + i, 0.0, 0.0, "%0.1f");
						}
					}
					ImGui::PopID ();
				}
				ImGui::EndTable ();
			}

			ImGui::SeparatorText ("Parameters");
			ImGui::AlignTextToFramePadding ();
			ImGui::TextUnformatted ("Max. subsamples");
			ImGui::SameLine ();
			ImGui::SetNextItemWidth (em * 6.0f);
			ImGui::InputInt ("##submax", &submax, 0, 0);
		}

		int n;
		int mode[MAX_PROP_LEVEL];
		double ttgt[MAX_PROP_LEVEL], atgt[MAX_PROP_LEVEL], tlim[MAX_PROP_LEVEL], alim[MAX_PROP_LEVEL];
		int submax;
	};

	//-------------------------------------------------------------------------
	// Orbit stabilisation

	class StabilisationDialog : public ExtraDialog {
	public:
		StabilisationDialog (Config *cfg) : ExtraDialog ("Orbit stabilisation parameters", cfg, 30.0f, "extra_orbitstab") {}

	protected:
		void Set (const CFG_PHYSICSPRM &prm)
		{
			enable = prm.bOrbitStabilise;
			plimit = prm.Stabilise_PLimit * 100.0;
			slimit = prm.Stabilise_SLimit * 100.0;
			psub = prm.PPropSubLimit * 100.0;
			psubmax = prm.PPropSubMax;
			pstep = prm.PPropStepLimit * 100.0;
		}
		void Init () override { Set (m_cfg->CfgPhysicsPrm); }
		void Reset () override { Set (CfgPhysicsPrm_default); }

		bool Store () override
		{
			if (plimit < 0.0 || plimit > 100.0) return Error ("Invalid perturbation limit.");
			if (slimit < 0.0) return Error ("Invalid step limit.");
			if (psub < 0.0) return Error ("Invalid subsampling target step.");
			if (psubmax < 1) return Error ("Invalid subsampling steps.");
			if (pstep < 0.0) return Error ("Invalid perturbation limit value.");
			CFG_PHYSICSPRM &prm = m_cfg->CfgPhysicsPrm;
			prm.bOrbitStabilise = enable;
			prm.Stabilise_PLimit = plimit * 0.01;
			prm.Stabilise_SLimit = slimit * 0.01;
			prm.PPropSubLimit = psub * 0.01;
			prm.PPropSubMax = psubmax;
			prm.PPropStepLimit = pstep * 0.01;
			return true;
		}

		void Row (const char *label, double *v, const char *unit)
		{
			ImGui::TableNextRow ();
			ImGui::TableNextColumn (); Label (label);
			ImGui::TableNextColumn ();
			ImGui::SetNextItemWidth (-FLT_MIN);
			ImGui::PushID (label);
			ImGui::InputDouble ("##v", v, 0.0, 0.0, "%0.4g");
			ImGui::PopID ();
			ImGui::TableNextColumn ();
			ImGui::AlignTextToFramePadding ();
			ImGui::TextUnformatted (unit);
		}

		bool BeginRows (const char *id)
		{
			if (!ImGui::BeginTable (id, 3)) return false;
			const float em = ImGui::GetFontSize ();
			ImGui::TableSetupColumn ("##l", ImGuiTableColumnFlags_WidthStretch);
			ImGui::TableSetupColumn ("##v", ImGuiTableColumnFlags_WidthFixed, em * 5.0f);
			ImGui::TableSetupColumn ("##u", ImGuiTableColumnFlags_WidthFixed, em * 7.0f);
			return true;
		}

		void DrawContent () override
		{
			ImGui::Checkbox ("Enable orbit stabilisation", &enable);
			ImGui::BeginDisabled (!enable);
			ImGui::SeparatorText ("Orbit step limit");
			if (BeginRows ("##t1")) {
				Row ("Enable stabilisation for orbit steps larger than", &slimit, "% of full orbit");
				ImGui::EndTable ();
			}
			ImGui::SeparatorText ("Gravitational field perturbation limit");
			if (BeginRows ("##t2")) {
				Row ("Suppress stabilisation if gravity perturbations are greater than", &plimit, "% of total field");
				ImGui::EndTable ();
			}
			ImGui::SeparatorText ("Subsampling of perturbation integration");
			if (BeginRows ("##t3")) {
				Row ("Target orbit step", &psub, "% of full orbit");
				ImGui::TableNextRow ();
				ImGui::TableNextColumn (); Label ("Max. steps");
				ImGui::TableNextColumn ();
				ImGui::SetNextItemWidth (-FLT_MIN);
				ImGui::InputInt ("##psubmax", &psubmax, 0, 0);
				ImGui::EndTable ();
			}
			ImGui::SeparatorText ("Perturbation model");
			if (BeginRows ("##t4")) {
				Row ("Suppress nonspherical gravity perturbations for orbit steps greater than", &pstep, "% of full orbit");
				ImGui::EndTable ();
			}
			ImGui::EndDisabled ();
		}

		bool enable;
		double plimit, slimit, psub, pstep;
		int psubmax;
	};

	//-------------------------------------------------------------------------
	// MFD parameters

	class MfdConfigDialog : public ExtraDialog {
	public:
		MfdConfigDialog (Config *cfg) : ExtraDialog ("MFD parameters", cfg, 24.0f, "extra_mfdconfig") {}

	protected:
		void Set (const CFG_INSTRUMENTPRM &prm)
		{
			pow2 = prm.bMfdPow2;
			hires = prm.MfdHiresThreshold;
			vcsize = (prm.VCMFDSize == 256 ? 0 : prm.VCMFDSize == 512 ? 1 : 2);
		}
		void Init () override { Set (m_cfg->CfgInstrumentPrm); }
		void Reset () override { Set (CfgInstrumentPrm_default); }

		bool Store () override
		{
			if (hires < 8) return Error ("Invalid hires texture threshold (integer value >= 8 required).");
			CFG_INSTRUMENTPRM &prm = m_cfg->CfgInstrumentPrm;
			prm.MfdHiresThreshold = hires;
			prm.bMfdPow2 = pow2;
			prm.VCMFDSize = 256 << vcsize;
			return true;
		}

		void DrawContent () override
		{
			ImGui::SeparatorText ("MFD texture size: 2D panels");
			ImGui::TextUnformatted ("Enforce pow2 MFD display texture sizes");
			ImGui::RadioButton ("No", &pow2, 0); ImGui::SameLine ();
			ImGui::RadioButton ("Yes", &pow2, 1); ImGui::SameLine ();
			ImGui::RadioButton ("Auto", &pow2, 2);
			ImGui::AlignTextToFramePadding ();
			ImGui::TextUnformatted ("Switch to hires texture at MFD size");
			ImGui::SameLine ();
			ImGui::SetNextItemWidth (ImGui::GetFontSize () * 5.0f);
			ImGui::InputInt ("##hires", &hires, 0, 0);
			ImGui::SeparatorText ("MFD texture size: virtual cockpits");
			ImGui::RadioButton ("256 x 256", &vcsize, 0); ImGui::SameLine ();
			ImGui::RadioButton ("512 x 512", &vcsize, 1); ImGui::SameLine ();
			ImGui::RadioButton ("1024 x 1024", &vcsize, 2);
		}

		int pow2, hires, vcsize;
	};

	//-------------------------------------------------------------------------
	// Shutdown options

	class ShutdownDialog : public ExtraDialog {
	public:
		ShutdownDialog (Config *cfg) : ExtraDialog ("Shutdown options", cfg, 24.0f, "extra_shutdown") {}

	protected:
		void Init () override { mode = m_cfg->CfgDebugPrm.ShutdownMode; }
		void Reset () override { mode = CfgDebugPrm_default.ShutdownMode; }
		bool Store () override { m_cfg->CfgDebugPrm.ShutdownMode = mode; return true; }

		void DrawContent () override
		{
			ImGui::SeparatorText ("After closing simulation window");
			ImGui::RadioButton ("De-allocate memory and display launchpad dialog", &mode, 0);
			ImGui::RadioButton ("Respawn Orbiter process", &mode, 1);
			ImGui::RadioButton ("Terminate Orbiter process", &mode, 2);
		}

		int mode;
	};

	//-------------------------------------------------------------------------
	// Fixed time steps

	class FixedStepDialog : public ExtraDialog {
	public:
		FixedStepDialog (Config *cfg) : ExtraDialog ("Fixed time steps", cfg, 24.0f, "extra_fixedstep") {}

	protected:
		void Set (const CFG_DEBUGPRM &prm)
		{
			double cmd = m_cfg->CfgCmdlinePrm.FixedStep;
			locked = (cmd != 0.0);
			fixed = locked || prm.FixedStep != 0.0;
			step = locked ? cmd : (prm.FixedStep ? prm.FixedStep : 0.01);
		}
		void Init () override { Set (m_cfg->CfgDebugPrm); }
		void Reset () override { Set (CfgDebugPrm_default); }

		bool Store () override
		{
			if (locked) return true;
			if (!fixed) {
				m_cfg->CfgDebugPrm.FixedStep = 0.0;
				return true;
			}
			if (step <= 0.0) return Error ("Invalid frame interval length");
			m_cfg->CfgDebugPrm.FixedStep = step;
			return true;
		}

		void DrawContent () override
		{
			ImGui::BeginDisabled (locked);
			ImGui::Checkbox ("Enable fixed time step length", &fixed);
			ImGui::BeginDisabled (!fixed);
			ImGui::AlignTextToFramePadding ();
			ImGui::TextUnformatted ("Fixed time step length per frame [s]");
			ImGui::SameLine ();
			ImGui::SetNextItemWidth (ImGui::GetFontSize () * 5.0f);
			ImGui::InputDouble ("##step", &step, 0.0, 0.0, "%0.4g");
			ImGui::EndDisabled ();
			ImGui::EndDisabled ();
		}

		bool locked, fixed;
		double step;
	};

	//-------------------------------------------------------------------------
	// Rendering options

	class RenderingDialog : public ExtraDialog {
	public:
		RenderingDialog (Config *cfg) : ExtraDialog ("Debug rendering options", cfg, 20.0f) {}

	protected:
		void Set (const CFG_DEBUGPRM &prm) { wire = prm.bWireframeMode; norm = prm.bNormaliseNormals; }
		void Init () override { Set (m_cfg->CfgDebugPrm); }
		void Reset () override { Set (CfgDebugPrm_default); }
		bool Store () override
		{
			m_cfg->CfgDebugPrm.bWireframeMode = wire;
			m_cfg->CfgDebugPrm.bNormaliseNormals = norm;
			return true;
		}
		void DrawContent () override
		{
			ImGui::Checkbox ("Render objects as wire frames", &wire);
			ImGui::Checkbox ("Force normalisation of normals", &norm);
		}

		bool wire, norm;
	};

	//-------------------------------------------------------------------------
	// Performance settings

	class PerformanceDialog : public ExtraDialog {
	public:
		PerformanceDialog (Config *cfg) : ExtraDialog ("Performance settings", cfg, 24.0f, "extra_performance") {}

	protected:
		void Set (const CFG_DEBUGPRM &prm) { disable = prm.bDisableSmoothFont; force = prm.bForceReenableSmoothFont; }
		void Init () override { Set (m_cfg->CfgDebugPrm); }
		void Reset () override { Set (CfgDebugPrm_default); }
		bool Store () override
		{
			m_cfg->CfgDebugPrm.bDisableSmoothFont = disable;
			m_cfg->CfgDebugPrm.bForceReenableSmoothFont = force;
			return true;
		}
		void DrawContent () override
		{
			ImGui::SeparatorText ("Font smoothing");
			ImGui::Checkbox ("Disable font smoothing while Orbiter is running", &disable);
			Note ("Font smoothing provides better text display in MFD instruments, but can seriously degrade performance on some computers. Disable font smoothing if you are experiencing performance issues, such as periodic stutters.");
			ImGui::Spacing ();
			ImGui::Checkbox ("Enforce font smoothing on exit", &force);
			Note ("Orbiter usually restores the original font smoothing setting on exit. Tick this option to enforce font smoothing on exiting Orbiter even if it was not active at the start.");
		}

		bool disable, force;
	};

	//-------------------------------------------------------------------------
	// Launchpad options

	class LaunchpadOptionsDialog : public ExtraDialog {
	public:
		LaunchpadOptionsDialog (Config *cfg) : ExtraDialog ("Launchpad options", cfg, 24.0f) {}

	protected:
		void Set (const CFG_DEBUGPRM &prm) { html = prm.bHtmlScnDesc; screen = prm.bSaveExitScreen; }
		void Init () override { Set (m_cfg->CfgDebugPrm); }
		void Reset () override { Set (CfgDebugPrm_default); }
		bool Store () override
		{
			m_cfg->CfgDebugPrm.bHtmlScnDesc = html;
			m_cfg->CfgDebugPrm.bSaveExitScreen = screen;
			return true;
		}
		void DrawContent () override
		{
			ImGui::SeparatorText ("Scenario description format");
			ImGui::RadioButton ("Inline HTML", &html, 1);
			ImGui::RadioButton ("Simple text display", &html, 0);
			ImGui::RadioButton ("Auto detect", &html, 2);
			ImGui::SeparatorText ("Scenario screenshot");
			ImGui::Checkbox ("Save a screenshot on closing a simulation session", &screen);
			Note ("If activated, the screenshot is displayed in the description of the \"Current state\" scenario.");
		}

		int html;
		bool screen;
	};

	//-------------------------------------------------------------------------
	// Logfile options

	class LogfileDialog : public ExtraDialog {
	public:
		LogfileDialog (Config *cfg) : ExtraDialog ("Logfile options", cfg, 24.0f) {}

	protected:
		void Init () override { verbose = m_cfg->CfgDebugPrm.bVerboseLog; }
		void Reset () override { verbose = CfgDebugPrm_default.bVerboseLog; }
		bool Store () override { m_cfg->CfgDebugPrm.bVerboseLog = verbose; return true; }
		void DrawContent () override
		{
			ImGui::SeparatorText ("Output format");
			ImGui::Checkbox ("Verbose output", &verbose);
			Note ("Verbose output logs many events during the simulation session, such as texture loads. This can help identifying problems, but it may also slightly affect performance, and will create very large log files.");
		}

		bool verbose;
	};

}

//=============================================================================
// ExtraTab class

orbiter::ExtraTab::ExtraTab (LaunchpadDialog *lp): LaunchpadTab (lp, "Extra", "tab_extra")
{
	m_sel = nullptr;
	m_listWidth = 260.0f;

	ExtraItem *ht;
	ht = Register (new ExtraPropagation (this));
	Register (new ExtraDynamics (this), ht);
	Register (new ExtraStabilisation (this), ht);
	ht = Register (new ExtraInstruments (this));
	Register (new ExtraMfdConfig (this), ht);
	Register (new ExtraVesselConfig (this));
	Register (new ExtraPlanetConfig (this));
	ht = Register (new ExtraDebug (this));
	Register (new ExtraShutdown (this), ht);
	Register (new ExtraFixedStep (this), ht);
	Register (new ExtraRenderingOptions (this), ht);
	Register (new ExtraLaunchpadOptions (this), ht);
	Register (new ExtraLogfileOptions (this), ht);
	Register (new ExtraPerformanceSettings (this), ht);
}

//-----------------------------------------------------------------------------

orbiter::ExtraTab::~ExtraTab ()
{
	for (auto it = m_builtin.rbegin (); it != m_builtin.rend (); ++it) {
		pLp->UnregisterExtraParam (*it);
		delete *it;
	}
	m_builtin.clear ();
}

//-----------------------------------------------------------------------------

orbiter::ExtraItem *orbiter::ExtraTab::Register (BuiltinLaunchpadItem *item, ExtraItem *parent)
{
	m_builtin.push_back (item);
	ExtraItem *node = pLp->RegisterExtraParam (item, parent);
	item->hItem = (LAUNCHPADITEM_HANDLE)node;
	return node;
}

//-----------------------------------------------------------------------------

void orbiter::ExtraTab::GetConfig (const Config *cfg)
{
	int listw = cfg->CfgWindowPos.LaunchpadExtListWidth;
	m_listWidth = listw > 0 ? (float)listw : 260.0f;
}

//-----------------------------------------------------------------------------

void orbiter::ExtraTab::SetConfig (Config *cfg)
{
	cfg->CfgWindowPos.LaunchpadExtListWidth = (int)m_listWidth;
}

//-----------------------------------------------------------------------------

bool orbiter::ExtraTab::Exists (const LaunchpadItem *item, const std::list<std::unique_ptr<ExtraItem>> &list) const
{
	for (auto &n : list)
		if (n->item == item || Exists (item, n->children)) return true;
	return false;
}

//-----------------------------------------------------------------------------

void orbiter::ExtraTab::OpenItem (LaunchpadItem *item)
{
	if (item) item->clbkOpen ();
}

//-----------------------------------------------------------------------------

void orbiter::ExtraTab::DrawNode (const ExtraItem *node)
{
	const char *name = node->item->Name ();
	if (!name) return;
	bool leaf = true;
	for (auto &c : node->children)
		if (c->item->Name ()) { leaf = false; break; }

	ImGuiTreeNodeFlags flags = ImGuiTreeNodeFlags_SpanAvailWidth | ImGuiTreeNodeFlags_OpenOnArrow | ImGuiTreeNodeFlags_OpenOnDoubleClick | ImGuiTreeNodeFlags_NavLeftJumpsToParent;
	if (leaf) flags |= ImGuiTreeNodeFlags_Leaf | ImGuiTreeNodeFlags_NoTreePushOnOpen;
	if (node->item == m_sel) flags |= ImGuiTreeNodeFlags_Selected;

	ImGui::PushID (node);
	bool open = ImGui::TreeNodeEx ("##item", flags, "%s", name);
	if (ImGui::IsItemClicked () && !ImGui::IsItemToggledOpen ())
		m_sel = node->item;
	if (ImGui::IsItemHovered () && ImGui::IsMouseDoubleClicked (ImGuiMouseButton_Left))
		OpenItem (node->item);
	if (ImGui::IsItemFocused () && (ImGui::IsKeyPressed (ImGuiKey_Enter) || ImGui::IsKeyPressed (ImGuiKey_KeypadEnter)))
		OpenItem (node->item);
	if (!leaf && open) {
		for (auto &c : node->children)
			DrawNode (c.get ());
		ImGui::TreePop ();
	}
	ImGui::PopID ();
}

//-----------------------------------------------------------------------------

void orbiter::ExtraTab::Draw ()
{
	const float sc = pLp->UIScale ();
	const ImGuiStyle &style = ImGui::GetStyle ();
	const auto &items = pLp->ExtraItems ();
	if (m_sel && !Exists (m_sel, items))
		m_sel = nullptr;

	float footer = ImGui::GetFrameHeight () + style.ItemSpacing.y;
	float h = ImGui::GetContentRegionAvail ().y - footer;

	ImGui::BeginChild ("##extlist", ImVec2 (m_listWidth * sc, h), ImGuiChildFlags_Borders | ImGuiChildFlags_ResizeX);
	for (auto &n : items)
		DrawNode (n.get ());
	m_listWidth = ImGui::GetWindowWidth () / sc;
	ImGui::EndChild ();

	ImGui::SameLine ();
	ImGui::BeginChild ("##extdesc", ImVec2 (0, h), ImGuiChildFlags_Borders);
	if (m_sel) {
		const char *desc = m_sel->Description ();
		if (desc) {
			std::string d (desc);
			d.erase (std::remove (d.begin (), d.end (), '\r'), d.end ());
			ImGui::TextWrapped ("%s", d.c_str ());
		}
	}
	ImGui::EndChild ();

	ImGui::BeginDisabled (!m_sel);
	if (ImGui::Button ("Edit"))
		OpenItem (m_sel);
	ImGui::EndDisabled ();
}

//=============================================================================
// Built-in "Extra" items

BuiltinLaunchpadItem::BuiltinLaunchpadItem (orbiter::ExtraTab *tab): LaunchpadItem ()
{
	pTab = tab;
}

BuiltinLaunchpadItem::~BuiltinLaunchpadItem ()
{
	if (m_dlg)
		pTab->Launchpad ()->CloseDialog (m_dlg.get ());
}

Config *BuiltinLaunchpadItem::Cfg () const
{
	return pTab->Cfg ();
}

void BuiltinLaunchpadItem::Open (ImGuiDialog *dlg)
{
	if (!m_dlg) m_dlg.reset (dlg);
	else delete dlg;
	static_cast<ExtraDialog*> (m_dlg.get ())->Open ();
}

//-----------------------------------------------------------------------------
// Physics engine

char *ExtraPropagation::Name ()
{
	return (char*)"Time propagation";
}

char *ExtraPropagation::Description ()
{
	return (char*)"Select and configure the time propagation methods Orbiter uses to update vessel positions and velocities from one time frame to the next.";
}

char *ExtraDynamics::Name ()
{
	return (char*)"Dynamic state propagators";
}

char *ExtraDynamics::Description ()
{
	return (char*)"Select the numerical integration methods used for dynamic state updates.\r\n\r\nState propagators affect the accuracy and stability of spacecraft orbits and trajectory calculations.";
}

bool ExtraDynamics::clbkOpen ()
{
	Open (m_dlg ? nullptr : new DynamicsDialog (Cfg ()));
	return true;
}

char *ExtraStabilisation::Name ()
{
	return (char*)"Orbit stabilisation";
}

char *ExtraStabilisation::Description ()
{
	return (char*)"Select the parameters that determine the conditions when Orbiter switches between dynamic and stabilised state updates.";
}

bool ExtraStabilisation::clbkOpen ()
{
	Open (m_dlg ? nullptr : new StabilisationDialog (Cfg ()));
	return true;
}

//-----------------------------------------------------------------------------
// Instruments and panels

char *ExtraInstruments::Name ()
{
	return (char*)"Instruments and panels";
}

char *ExtraInstruments::Description ()
{
	return (char*)"Select general configuration parameters for spacecraft instruments, MFD displays and instrument panels.";
}

char *ExtraMfdConfig::Name ()
{
	return (char*)"MFD parameter configuration";
}

char *ExtraMfdConfig::Description ()
{
	return (char*)"Select display parameters for multifunctional displays (MFD).";
}

bool ExtraMfdConfig::clbkOpen ()
{
	Open (m_dlg ? nullptr : new MfdConfigDialog (Cfg ()));
	return true;
}

//-----------------------------------------------------------------------------
// Root items for vessel and planet configurations (sub-items added by modules)

char *ExtraVesselConfig::Name ()
{
	return (char*)"Vessel configuration";
}

char *ExtraVesselConfig::Description ()
{
	return (char*)"Configure spacecraft parameters";
}

char *ExtraPlanetConfig::Name ()
{
	return (char*)"Celestial body configuration";
}

char *ExtraPlanetConfig::Description ()
{
	return (char*)"Configure options for celestial objects";
}

//-----------------------------------------------------------------------------
// Debugging parameters

char *ExtraDebug::Name ()
{
	return (char*)"Debugging options";
}

char *ExtraDebug::Description ()
{
	return (char*)"Various options that are useful for debugging and special tasks. Not generally used for standard simulation sessions.";
}

char *ExtraShutdown::Name ()
{
	return (char*)"Orbiter shutdown options";
}

char *ExtraShutdown::Description ()
{
	return (char*)"Set the behaviour of Orbiter after closing the simulation window: return to Launchpad, respawn or terminate.";
}

bool ExtraShutdown::clbkOpen ()
{
	Open (m_dlg ? nullptr : new ShutdownDialog (Cfg ()));
	return true;
}

char *ExtraFixedStep::Name ()
{
	return (char*)"Fixed time steps";
}

char *ExtraFixedStep::Description ()
{
	return (char*)"This option assigns a fixed simulation time interval to each frame. Useful for debugging, and when numerical accuracy and stability of the dynamic propagators are important (for example, to generate trajectory data or when recording high-fidelity playbacks).\r\n\r\nWarning: Selecting this option leads to nonlinear time flow and a simulation that is no longer real-time.";
}

bool ExtraFixedStep::clbkOpen ()
{
	Open (m_dlg ? nullptr : new FixedStepDialog (Cfg ()));
	return true;
}

char *ExtraRenderingOptions::Name ()
{
	return (char*)"Rendering options";
}

char *ExtraRenderingOptions::Description ()
{
	return (char*)"Some rendering options that can be used for debugging problems.";
}

bool ExtraRenderingOptions::clbkOpen ()
{
	Open (m_dlg ? nullptr : new RenderingDialog (Cfg ()));
	return true;
}

char *ExtraPerformanceSettings::Name ()
{
	return (char*)"Performance options";
}

char *ExtraPerformanceSettings::Description ()
{
	return (char*)"This option can be used to modify environment parameters that can improve the simulator performance.";
}

bool ExtraPerformanceSettings::clbkOpen ()
{
	Open (m_dlg ? nullptr : new PerformanceDialog (Cfg ()));
	return true;
}

char *ExtraLaunchpadOptions::Name ()
{
	return (char*)"Launchpad options";
}

char *ExtraLaunchpadOptions::Description ()
{
	return (char*)"Configure the behaviour of the Orbiter Launchpad dialog.";
}

bool ExtraLaunchpadOptions::clbkOpen ()
{
	Open (m_dlg ? nullptr : new LaunchpadOptionsDialog (Cfg ()));
	return true;
}

char *ExtraLogfileOptions::Name ()
{
	return (char*)"Logfile options";
}

char *ExtraLogfileOptions::Description ()
{
	return (char*)"Configure options for log file output.";
}

bool ExtraLogfileOptions::clbkOpen ()
{
	Open (m_dlg ? nullptr : new LogfileDialog (Cfg ()));
	return true;
}

//=============================================================================
// class LaunchpadItem: addon-defined items for the "Extra" tab
// Interface in OrbiterAPI.h

LaunchpadItem::LaunchpadItem ()
{
	hItem = 0;
}

LaunchpadItem::~LaunchpadItem ()
{}

char *LaunchpadItem::Name ()
{
	return 0;
}

char *LaunchpadItem::Description ()
{
	return 0;
}

bool LaunchpadItem::clbkOpen ()
{
	return false;
}

int LaunchpadItem::clbkWriteConfig ()
{
	return 0;
}
