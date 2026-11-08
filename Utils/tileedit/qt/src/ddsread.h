#ifndef DDSREAD_H
#define DDSREAD_H

#include "imagetools.h"

Image ddsread(const char *fname);
Image ddsscan(const uint8_t *data, int ndata);

#endif // !DDSREAD_H
