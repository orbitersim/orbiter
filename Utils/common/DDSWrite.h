// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#pragma once

#include <cstdint>
#include <vector>

namespace ddsw {

enum Format { DXT1, DXT3, DXT5 };

// Compress a top-down RGBA8 image (bytes r,g,b,a per pixel) into a DDS file image.
// DXT1 treats pixels with alpha < 128 as transparent if any are present.
std::vector<uint8_t> Encode(const uint8_t *rgba, int width, int height, Format fmt, bool mipmaps);

// Build a top-down RGBA8 image from a bottom-up 24-bit BMP-order (b,g,r) pixel
// buffer and an optional bottom-up 8-bit alpha buffer (opaque if null).
std::vector<uint8_t> FromBottomUpBGR(const uint8_t *bgr, const uint8_t *alpha, int width, int height);

}
