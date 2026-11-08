// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#import <Metal/Metal.h>
#import <QuartzCore/CAMetalLayer.h>
#include "UtilApp.h"
#include "OrbiterPlatform.h"
#include "imgui.h"
#include "imgui_impl_metal.h"
#include "imgui_impl_orbiter.h"
#include <filesystem>
#include <vector>

ImGuiContext *GImGui = nullptr;

namespace utilapp {

namespace {

bool g_quit = false;
std::vector<std::function<void()>> g_deferred;
float g_scale = 1.0f;

LRESULT WndProc(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
	if (msg == WM_CLOSE) {
		g_quit = true;
		return 0;
	}
	if (msg == WM_DESTROY) return 0;
	ImGui_ImplOrbiter_WndProcHandler(hWnd, msg, wParam, lParam);
	return 0;
}

}

void Defer(const std::function<void()> &action)
{
	g_deferred.push_back(action);
}

void Quit()
{
	g_quit = true;
}

float Scale()
{
	return g_scale;
}

void BeginMainWindow(bool menuBar)
{
	ImGuiViewport *vp = ImGui::GetMainViewport();
	ImGui::SetNextWindowPos(vp->Pos);
	ImGui::SetNextWindowSize(vp->Size);
	ImGuiWindowFlags flags = ImGuiWindowFlags_NoDecoration | ImGuiWindowFlags_NoMove | ImGuiWindowFlags_NoSavedSettings |
		ImGuiWindowFlags_NoBringToFrontOnFocus;
	if (menuBar) flags |= ImGuiWindowFlags_MenuBar;
	ImGui::Begin("##main", nullptr, flags);
}

void EndMainWindow()
{
	ImGui::End();
}

int Run(const AppDesc &desc, const std::function<void()> &frame, const std::function<bool()> &busy)
{
	oplat::Init();
	g_scale = oplat::GetBackingScale(NULL);

	oplat::WindowDesc wd;
	wd.title = desc.title;
	wd.width = (int)(desc.width * g_scale);
	wd.height = (int)(desc.height * g_scale);
	wd.resizable = desc.resizable;
	wd.metal = true;
	wd.proc = WndProc;
	HWND hWnd = oplat::OpenWindow(wd);
	if (!hWnd) return 1;
	g_scale = oplat::GetBackingScale(hWnd);

	IMGUI_CHECKVERSION();
	ImGui::CreateContext();
	ImGuiIO &io = ImGui::GetIO();
	io.ConfigFlags |= ImGuiConfigFlags_NavEnableKeyboard;
	io.IniFilename = nullptr;
	ImGui::StyleColorsDark();
	ImGui::GetStyle().ScaleAllSizes(g_scale);
	const char *sysfont = "/System/Library/Fonts/SFNS.ttf";
	if (std::filesystem::exists(sysfont)) io.Fonts->AddFontFromFileTTF(sysfont, 14.0f * g_scale);
	else {
		ImFontConfig cfg;
		cfg.SizePixels = 13.0f * g_scale;
		io.Fonts->AddFontDefault(&cfg);
	}

	ImGui_ImplOrbiter_Init(hWnd);

	CAMetalLayer *layer = (__bridge CAMetalLayer *)oplat::GetMetalLayer(hWnd);
	id<MTLDevice> device = layer.device ? layer.device : MTLCreateSystemDefaultDevice();
	layer.device = device;
	id<MTLCommandQueue> queue = [device newCommandQueue];
	MTLRenderPassDescriptor *pass = [MTLRenderPassDescriptor new];
	ImGui_ImplMetal_Init(device);

	int settle = 2;
	while (!g_quit) {
		bool wait = settle <= 0 && !(busy && busy());
		if (!oplat::PumpEvents(wait)) break;
		settle = wait ? 2 : settle - 1;
		if (g_quit) break;

		@autoreleasepool {
			id<CAMetalDrawable> drawable = [layer nextDrawable];
			if (!drawable) continue;
			MTLRenderPassColorAttachmentDescriptor *c = pass.colorAttachments[0];
			c.texture = drawable.texture;
			c.loadAction = MTLLoadActionClear;
			c.storeAction = MTLStoreActionStore;
			c.clearColor = MTLClearColorMake(0.06, 0.06, 0.06, 1.0);
			ImGui_ImplMetal_NewFrame(pass);
			ImGui_ImplOrbiter_NewFrame();
			ImGui::NewFrame();
			frame();
			ImGui::Render();
			id<MTLCommandBuffer> cmd = [queue commandBuffer];
			id<MTLRenderCommandEncoder> enc = [cmd renderCommandEncoderWithDescriptor:pass];
			ImGui_ImplMetal_RenderDrawData(ImGui::GetDrawData(), cmd, enc);
			[enc endEncoding];
			[cmd presentDrawable:drawable];
			[cmd commit];
		}
		std::vector<std::function<void()>> actions;
		actions.swap(g_deferred);
		for (auto &a : actions) a();
		if (!actions.empty()) settle = 2;
	}

	ImGui_ImplMetal_Shutdown();
	ImGui_ImplOrbiter_Shutdown();
	ImGui::DestroyContext();
	DestroyWindow(hWnd);
	return 0;
}

}
