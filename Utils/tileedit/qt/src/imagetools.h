#ifndef IMAGETOOLS_H
#define IMAGETOOLS_H

#include <vector>
#include <windows.h>

struct Image {
	std::vector<DWORD> data;
	DWORD width;
	DWORD height;

	Image() { width = height = 0; }
	Image &operator=(const Image &img);
	Image SubImage(const std::pair<DWORD, DWORD> &xrange, const std::pair<DWORD, DWORD> &yrange);
};

void match_histogram(Image &im1, const Image &im2);
void match_hue_sat(Image &im1, const Image &im2);

#endif // !IMAGETOOLS_H
