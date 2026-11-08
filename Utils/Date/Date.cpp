// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#include "Convert.h"
#include "UtilApp.h"
#include "imgui.h"
#include <cstring>

namespace {

double mjd;
struct tm date;
char bufDay[16], bufMonth[16], bufYear[16], bufHour[16], bufMin[16], bufSec[16];
char bufMJD[64], bufJD[64], bufJC[64], bufEpoch[64];
bool showAbout = false;

void UpdateUT()
{
	snprintf(bufDay, sizeof(bufDay), "%02d", date.tm_mday);
	snprintf(bufMonth, sizeof(bufMonth), "%02d", date.tm_mon);
	snprintf(bufYear, sizeof(bufYear), "%04d", date.tm_year + 1900);
	snprintf(bufHour, sizeof(bufHour), "%02d", date.tm_hour);
	snprintf(bufMin, sizeof(bufMin), "%02d", date.tm_min);
	snprintf(bufSec, sizeof(bufSec), "%02d", date.tm_sec);
}

void UpdateMJD() { snprintf(bufMJD, sizeof(bufMJD), "%0.6f", mjd); }
void UpdateJD() { snprintf(bufJD, sizeof(bufJD), "%0.6f", mjd + 2400000.5); }
void UpdateJC() { snprintf(bufJC, sizeof(bufJC), "%0.10f", MJD2JC(mjd)); }
void UpdateEpoch() { snprintf(bufEpoch, sizeof(bufEpoch), "%0.8f", MJD2Jepoch(mjd)); }

void SetMJD(double new_mjd, bool reset_mjd = false)
{
	mjd = new_mjd;
	memcpy(&date, mjddate(mjd), sizeof(date));
	UpdateUT();
	UpdateJD();
	UpdateJC();
	UpdateEpoch();
	if (reset_mjd) UpdateMJD();
}

void SetJD(double new_jd)
{
	mjd = new_jd - 2400000.5;
	memcpy(&date, mjddate(mjd), sizeof(date));
	UpdateUT();
	UpdateMJD();
	UpdateJC();
	UpdateEpoch();
}

void SetJC(double new_jc)
{
	mjd = JC2MJD(new_jc);
	memcpy(&date, mjddate(mjd), sizeof(date));
	UpdateUT();
	UpdateMJD();
	UpdateJD();
	UpdateEpoch();
}

void SetEpoch(double new_epoch)
{
	mjd = Jepoch2MJD(new_epoch);
	memcpy(&date, mjddate(mjd), sizeof(date));
	UpdateUT();
	UpdateMJD();
	UpdateJD();
	UpdateJC();
}

void SetUT(struct tm *new_date)
{
	mjd = date2mjd(new_date);
	UpdateMJD();
	UpdateJD();
	UpdateJC();
	UpdateEpoch();
}

void OnChangeMjd()
{
	double v;
	if (sscanf(bufMJD, "%lf", &v) == 1 && fabs(v - mjd) > 1e-6) SetMJD(v);
}

void OnChangeJd()
{
	double v;
	if (sscanf(bufJD, "%lf", &v) == 1) SetJD(v);
}

void OnChangeJc()
{
	double v;
	if (sscanf(bufJC, "%lf", &v) == 1) SetJC(v);
}

void OnChangeEpoch()
{
	double v;
	if (sscanf(bufEpoch, "%lf", &v) == 1) SetEpoch(v);
}

void OnChangeDay()
{
	int v;
	if (sscanf(bufDay, "%d", &v) == 1 && v != date.tm_mday && v >= 1 && v <= 31) {
		date.tm_mday = v;
		SetUT(&date);
	}
}

void OnChangeMonth()
{
	int v;
	if (sscanf(bufMonth, "%d", &v) == 1 && v != date.tm_mon && v >= 1 && v <= 12) {
		date.tm_mon = v;
		SetUT(&date);
	}
}

void OnChangeYear()
{
	int v;
	if (sscanf(bufYear, "%d", &v) == 1 && (v -= 1900) != date.tm_year) {
		date.tm_year = v;
		SetUT(&date);
	}
}

void OnChangeHour()
{
	int v;
	if (sscanf(bufHour, "%d", &v) == 1 && v != date.tm_hour) {
		date.tm_hour = v;
		SetUT(&date);
	}
}

void OnChangeMin()
{
	int v;
	if (sscanf(bufMin, "%d", &v) == 1 && v != date.tm_min) {
		date.tm_min = v;
		SetUT(&date);
	}
}

void OnChangeSec()
{
	int v;
	if (sscanf(bufSec, "%d", &v) == 1 && v != date.tm_sec) {
		date.tm_sec = v;
		SetUT(&date);
	}
}

int DigitsOnly(ImGuiInputTextCallbackData *data)
{
	return (data->EventChar >= '0' && data->EventChar <= '9') ? 0 : 1;
}

bool NumberField(const char *id, char *buf, size_t size, float width)
{
	ImGui::SetNextItemWidth(width);
	return ImGui::InputText(id, buf, size, ImGuiInputTextFlags_CallbackCharFilter, DigitsOnly);
}

bool ValueField(const char *id, char *buf, size_t size)
{
	ImGui::SetNextItemWidth(-FLT_MIN);
	return ImGui::InputText(id, buf, size, ImGuiInputTextFlags_CharsScientific);
}

void Unit(const char *label, float width)
{
	float w = ImGui::CalcTextSize(label).x;
	float x = ImGui::GetCursorPosX();
	ImGui::SetCursorPosX(x + (width - w) * 0.5f);
	ImGui::TextDisabled("%s", label);
}

void Frame()
{
	utilapp::BeginMainWindow(true);
	if (ImGui::BeginMenuBar()) {
		if (ImGui::BeginMenu("Help")) {
			if (ImGui::MenuItem("About Date")) showAbout = true;
			ImGui::EndMenu();
		}
		ImGui::EndMenuBar();
	}

	float sp = ImGui::GetStyle().ItemSpacing.x;
	float avail = ImGui::GetContentRegionAvail().x;
	float w2 = (avail - 2 * sp) * 0.28f;
	float w4 = avail - 2 * sp - 2 * w2;

	ImGui::SeparatorText("Ephemeris time (ET)");
	ImGui::BeginGroup();
	if (NumberField("##day", bufDay, sizeof(bufDay), w2)) OnChangeDay();
	Unit("DD", w2);
	ImGui::EndGroup();
	ImGui::SameLine();
	ImGui::BeginGroup();
	if (NumberField("##month", bufMonth, sizeof(bufMonth), w2)) OnChangeMonth();
	Unit("MM", w2);
	ImGui::EndGroup();
	ImGui::SameLine();
	ImGui::BeginGroup();
	if (NumberField("##year", bufYear, sizeof(bufYear), w4)) OnChangeYear();
	Unit("YYYY", w4);
	ImGui::EndGroup();

	ImGui::BeginGroup();
	if (NumberField("##hour", bufHour, sizeof(bufHour), w2)) OnChangeHour();
	Unit("h", w2);
	ImGui::EndGroup();
	ImGui::SameLine();
	ImGui::BeginGroup();
	if (NumberField("##min", bufMin, sizeof(bufMin), w2)) OnChangeMin();
	Unit("m", w2);
	ImGui::EndGroup();
	ImGui::SameLine();
	ImGui::BeginGroup();
	if (NumberField("##sec", bufSec, sizeof(bufSec), w4)) OnChangeSec();
	Unit("s", w4);
	ImGui::EndGroup();

	ImGui::SeparatorText("Modified Julian date (MJD)");
	if (ValueField("##mjd", bufMJD, 33)) OnChangeMjd();
	ImGui::SeparatorText("Julian date (JD)");
	if (ValueField("##jd", bufJD, sizeof(bufJD))) OnChangeJd();
	ImGui::SeparatorText("Julian century (JC2000)");
	if (ValueField("##jc", bufJC, sizeof(bufJC))) OnChangeJc();
	ImGui::SeparatorText("Julian epoch (J)");
	if (ValueField("##epoch", bufEpoch, sizeof(bufEpoch))) OnChangeEpoch();

	if (showAbout) {
		ImGui::OpenPopup("About Date");
		showAbout = false;
	}
	if (ImGui::BeginPopupModal("About Date", nullptr, ImGuiWindowFlags_AlwaysAutoResize)) {
		ImGui::TextUnformatted("Date version 1.0");
		ImGui::TextUnformatted("Copyright (C) 2005");
		if (ImGui::Button("OK")) ImGui::CloseCurrentPopup();
		ImGui::EndPopup();
	}

	utilapp::EndMainWindow();
}

}

int main()
{
	SetMJD(MJD(time(NULL)), true);
	utilapp::AppDesc desc;
	desc.title = "Orbiter date convert";
	desc.width = 260;
	desc.height = 420;
	desc.resizable = false;
	return utilapp::Run(desc, Frame);
}
