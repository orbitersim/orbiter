// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#pragma once

#include <functional>
#include <string>

namespace utilapp {

struct AppDesc {
	std::string title;
	int width = 400;
	int height = 300;
	bool resizable = true;
};

int Run(const AppDesc &desc, const std::function<void()> &frame, const std::function<bool()> &busy = {});
void Quit();
void Defer(const std::function<void()> &action);
float Scale();
void BeginMainWindow(bool menuBar = false);
void EndMainWindow();

}
