/* -*-c++-*- */
/* osgEarth - Geospatial SDK for OpenSceneGraph
* Copyright 2008-2013 Pelican Mapping
* http://osgearth.org
*
* osgEarth is free software; you can redistribute it and/or modify
* it under the terms of the GNU Lesser General Public License as published by
* the Free Software Foundation; either version 2 of the License, or
* (at your option) any later version.
*
* This program is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU Lesser General Public License for more details.
*
* You should have received a copy of the GNU Lesser General Public License
* along with this program.  If not, see <http://www.gnu.org/licenses/>
*/

#include <osg/Texture>
#include <osgDB/Registry>
#include <osg/Notify>
#include <osgEarth/ImageUtils>
#include <stdlib.h>
#include "libdxt.h"
#include <string.h>

class FastDXTProcessor : public osgDB::ImageProcessor
{
public:
    virtual void compress(osg::Image& image, osg::Texture::InternalFormatMode compressedFormat, bool generateMipMap, bool resizeToPowerOfTwo, CompressionMethod method, CompressionQuality quality)
    {
        //Resize the image to the nearest power of two
        if (!osgEarth::ImageUtils::isPowerOfTwo( &image ))
        {
            unsigned int s = osg::Image::computeNearestPowerOfTwo( image.s() );
            unsigned int t = osg::Image::computeNearestPowerOfTwo( image.t() );
            image.scaleImage(s, t, image.r());
        }

        osg::Image* sourceImage = &image;

        //FastDXT only works on RGBA imagery so we must convert it
        osg::ref_ptr< osg::Image > rgba;
        if (image.getPixelFormat() != GL_RGBA)
        {
            osg::Timer_t start = osg::Timer::instance()->tick();
            rgba = osgEarth::ImageUtils::convertToRGBA8( &image );
            osg::Timer_t end = osg::Timer::instance()->tick();
            OE_DEBUG << "conversion to rgba took" << osg::Timer::instance()->delta_m(start, end) << std::endl;
            sourceImage = rgba.get();
        }

        int format;
        GLint pixelFormat;
        switch (compressedFormat)
        {
        case osg::Texture::USE_S3TC_DXT1_COMPRESSION:
            format = FORMAT_DXT1;
            pixelFormat = GL_COMPRESSED_RGB_S3TC_DXT1_EXT;
            OE_DEBUG << "FastDXT using dxt1 format" << std::endl;
            break;
        case osg::Texture::USE_S3TC_DXT5_COMPRESSION:
            format = FORMAT_DXT5;
            pixelFormat = GL_COMPRESSED_RGBA_S3TC_DXT5_EXT;
            OE_DEBUG << "FastDXT dxt5 format" << std::endl;
            break;
        default:
            OSG_WARN << "Unhandled compressed format" << compressedFormat << std::endl;
            return;
            break;
        }

        //Copy over the source data to an array
        unsigned char *in = 0;
        in = (unsigned char*)memalign(16, sourceImage->getTotalSizeInBytes());
        memcpy(in, sourceImage->data(0,0), sourceImage->getTotalSizeInBytes());



        //Allocate memory for the output
        unsigned char* out = (unsigned char*)memalign(16, image.s()*image.t()*4);
        memset(out, 0, image.s()*image.t()*4);

        osg::Timer_t start = osg::Timer::instance()->tick();
        int outputBytes = CompressDXT(in, out, sourceImage->s(), sourceImage->t(), format);
        osg::Timer_t end = osg::Timer::instance()->tick();
        OE_DEBUG << "compression took" << osg::Timer::instance()->delta_m(start, end) << std::endl;

        //Allocate and copy over the output data to the correct size array.
        unsigned char* data = (unsigned char*)malloc(outputBytes);
        memcpy(data, out, outputBytes);
        memfree(out);
        memfree(in);
        image.setImage(image.s(), image.t(), image.r(), pixelFormat, pixelFormat, GL_UNSIGNED_BYTE, data, osg::Image::USE_MALLOC_FREE);
    }

    virtual void generateMipMap(osg::Image& image, bool resizeToPowerOfTwo, CompressionMethod method)
    {
        OSG_WARN << "FastDXT: generateMipMap not implemented" << std::endl;
    }
};

REGISTER_OSGIMAGEPROCESSOR(fastdxt, FastDXTProcessor)
