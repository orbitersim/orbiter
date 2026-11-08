#ifndef IMAGETOOLS_H
#define IMAGETOOLS_H

#include <cstdint>
#include <utility>
#include <vector>

struct Image {
	std::vector<uint32_t> data;
	uint32_t width;
	uint32_t height;

	Image() { width = height = 0; }
	Image &operator=(const Image &img);
	Image SubImage(const std::pair<uint32_t, uint32_t> &xrange, const std::pair<uint32_t, uint32_t> &yrange);
};

void match_histogram(Image &im1, const Image &im2);
void match_hue_sat(Image &im1, const Image &im2);

#endif // !IMAGETOOLS_H
