#include "imagetools.h"
#include <cstring>
#include <QColor>

Image &Image::operator=(const Image &img)
{
	data = img.data;
	width = img.width;
	height = img.height;
	return *this;
}

Image Image::SubImage(const std::pair<uint32_t, uint32_t> &xrange, const std::pair<uint32_t, uint32_t> &yrange)
{
	Image sub;
	sub.width = xrange.second - xrange.first;
	sub.height = yrange.second - yrange.first;

	sub.data.resize(sub.width * sub.height);

	uint32_t *imgptr = data.data();
	uint32_t *subptr = sub.data.data();
	for (int y = 0; y < sub.height; y++) {
		memcpy(subptr + y*sub.width, imgptr + (y + yrange.first)*width + xrange.first, sub.width * sizeof(uint32_t));
	}
	return sub;
}

void match_histogram(Image &im1, const Image &im2)
{
	int n = im1.width * im1.height;
	for (int ch = 0; ch < 3; ch++) {
		uint32_t hist[256];
		memset(hist, 0, 256 * sizeof(uint32_t));
		for (int i = 0; i < n; i++) { // construct histogram
			uint32_t v = (im2.data[i] >> (ch * 8)) & 0xff;
			hist[v]++;
		}
		for (int i = 1; i < 256; i++) // cumulative
			hist[i] += hist[i - 1];
		for (int i = 0; i < 256; i++) // normalise
			hist[i] = (hist[i] * 255) / n;

		uint32_t ihist[256]; // invert
		for (int i = 0; i < 255; i++) {
			uint32_t v1 = hist[i];
			uint32_t v2 = hist[i + 1];
			for (int j = v1; j < v2; j++)
				ihist[j] = i;
		}
		for (int j = 0; j < hist[0]; j++)
			ihist[j] = 0;
		for (int j = hist[255]; j < 256; j++)
			ihist[j] = 255;

		// apply histogram to im1
		uint32_t mask = ~(0xff << (ch * 8));
		for (int i = 0; i < n; i++) {
			uint32_t v = (im1.data[i] >> (ch * 8)) & 0xff;
			uint32_t vn = ihist[v];
			im1.data[i] &= mask;
			im1.data[i] |= vn << (ch * 8);
		}
	}
}

void match_hue_sat(Image &im1, const Image &im2)
{
	int n = im1.width * im1.height;
	for (int i = 0; i < n; i++) {
		uint32_t p1 = im1.data[i];
		uint32_t p2 = im2.data[i];
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
