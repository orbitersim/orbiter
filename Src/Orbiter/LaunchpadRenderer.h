#pragma once

#include <windows.h>

struct ImDrawData;

namespace orbiter {

class LaunchpadRenderer {
public:
	LaunchpadRenderer();
	~LaunchpadRenderer();

	bool Init(HWND hWnd);
	void Shutdown();
	bool BeginFrame();
	void EndFrame(ImDrawData *drawData);

	// Create a texture from 32-bit BGRA pixels for use with ImGui::Image
	void *CreateTexture(const unsigned char *bgra, int width, int height);
	void ReleaseTexture(void *texture);

private:
	struct Impl;
	Impl *impl;
};

}
