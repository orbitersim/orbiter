#include "imagetools.h"
#include <QColor>

Image &Image::operator=(const Image &img)
{
	data = img.data;
	width = img.width;
	height = img.height;
	return *this;
}

Image Image::SubImage(const std::pair<DWORD, DWORD> &xrange, const std::pair<DWORD, DWORD> &yrange)
{
	Image sub;
	sub.width = xrange.second - xrange.first;
	sub.height = yrange.second - yrange.first;

	sub.data.resize(sub.width * sub.height);

	DWORD *imgptr = data.data();
	DWORD *subptr = sub.data.data();
	for (int y = 0; y < sub.height; y++) {
		memcpy(subptr + y*sub.width, imgptr + (y + yrange.first)*width + xrange.first, sub.width * sizeof(DWORD));
	}
	return sub;
}

void match_histogram(Image &im1, const Image &im2)
{
	int n = im1.width * im1.height;
	for (int ch = 0; ch < 3; ch++) {
		DWORD hist[256];
		memset(hist, 0, 256 * sizeof(DWORD));
		for (int i = 0; i < n; i++) { // construct histogram
			DWORD v = (im2.data[i] >> (ch * 8)) & 0xff;
			hist[v]++;
		}
		for (int i = 1; i < 256; i++) // cumulative
			hist[i] += hist[i - 1];
		for (int i = 0; i < 256; i++) // normalise
			hist[i] = (hist[i] * 255) / n;

		DWORD ihist[256]; // invert
		for (int i = 0; i < 255; i++) {
			DWORD v1 = hist[i];
			DWORD v2 = hist[i + 1];
			for (int j = v1; j < v2; j++)
				ihist[j] = i;
		}
		for (int j = 0; j < hist[0]; j++)
			ihist[j] = 0;
		for (int j = hist[255]; j < 256; j++)
			ihist[j] = 255;

		// apply histogram to im1
		DWORD mask = ~(0xff << (ch * 8));
		for (int i = 0; i < n; i++) {
			DWORD v = (im1.data[i] >> (ch * 8)) & 0xff;
			DWORD vn = ihist[v];
			im1.data[i] &= mask;
			im1.data[i] |= vn << (ch * 8);
		}
	}
}

void match_hue_sat(Image &im1, const Image &im2)
{
	int n = im1.width * im1.height;
	for (int i = 0; i < n; i++) {
		DWORD p1 = im1.data[i];
		DWORD p2 = im2.data[i];
		QColor c1((p1 >> 16) & 0xff, (p1 >> 8) & 0xff, p1 & 0xff);
		QColor c2((p2 >> 16) & 0xff, (p2 >> 8) & 0xff, p2 & 0xff);
		int h1, s1, v1, h2, s2, v2;
		c1.getHsv(&h1, &s1, &v1);
		c2.getHsv(&h2, &s2, &v2);
		c1.setHsv(h2, s2, v1);
		p1 = 0xff000000 | (c1.red() << 16) | (c1.green() << 8) | c1.blue();
		im1.data[i] = p1;
	}
}
