#pragma once
#include <stdint.h>

struct ImDrawData;
struct IDirect3DBaseTexture9;
struct IDirect3DSurface9;

bool ClientImGuiInit();
void ClientImGuiShutdown();
void ClientImGuiNewFrame();
void ClientImGuiRender(ImDrawData *data, IDirect3DSurface9 *target);
uint64_t ClientImGuiTexture(IDirect3DBaseTexture9 *tex);
