#pragma once
#include <stdint.h>

namespace m9 {

struct FontMetrics {
	int height, ascent, descent, internalLeading, externalLeading, aveCharWidth, maxCharWidth;
};

void *CreateFontRef(const char *face, int height, int width, int weight, bool italic);
void ReleaseFontRef(void *font);
void GetFontMetrics(void *font, FontMetrics *m);
float GlyphAdvance(void *font, uint32_t ch);
void DrawGlyph(void *font, uint32_t ch, uint8_t *gray, int pitch, int w, int h, float x, float baseline, bool antialias);
uint32_t AnsiToUnicode(uint8_t c);

}
