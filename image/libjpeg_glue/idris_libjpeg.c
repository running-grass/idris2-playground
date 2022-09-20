#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <turbojpeg.h>

#include <buffer.h>


#define THROW(action, message) { \
  printf("ERROR in line %d while %s:\n%s\n", __LINE__, action, message); \
  goto bailout; \
}

#define THROW_TJ(action)  THROW(action, tjGetErrorStr2(tjInstance))

#define THROW_UNIX(action)  THROW(action, strerror(errno))

#define DEFAULT_SUBSAMP  TJSAMP_444
#define DEFAULT_QUALITY  95


const char *subsampName[TJ_NUMSAMP] = {
  "4:4:4", "4:2:2", "4:2:0", "Grayscale", "4:4:0", "4:1:1"
};

const char *colorspaceName[TJ_NUMCS] = {
  "RGB", "YCbCr", "GRAY", "CMYK", "YCCK"
};


typedef struct {
  int image_width;
  int image_height;
  void* buffer;
} jpeg_decompress;

jpeg_decompress* read_JPEG_file(char *filename)
{

  int flags = 0;
  int width, height;
  FILE *jpegFile = NULL;
  unsigned char *imgBuf = NULL, *jpegBuf = NULL;
  int  pixelFormat = TJPF_UNKNOWN;
  tjhandle tjInstance = NULL;

  /* Input image is a JPEG image.  Decompress and/or transform it. */
  long size;
  int inSubsamp, inColorspace;
  unsigned long jpegSize;

  /* Read the JPEG file into memory. */
  if ((jpegFile = fopen(filename, "rb")) == NULL)
    THROW_UNIX("opening input file");
  if (fseek(jpegFile, 0, SEEK_END) < 0 || ((size = ftell(jpegFile)) < 0) ||
      fseek(jpegFile, 0, SEEK_SET) < 0)
    THROW_UNIX("determining input file size");
  if (size == 0)
    THROW("determining input file size", "Input file contains no data");
  jpegSize = (unsigned long)size;
  if ((jpegBuf = (unsigned char *)tjAlloc(jpegSize)) == NULL)
    THROW_UNIX("allocating JPEG buffer");
  if (fread(jpegBuf, jpegSize, 1, jpegFile) < 1)
    THROW_UNIX("reading input file");
  fclose(jpegFile);  jpegFile = NULL;


  if ((tjInstance = tjInitDecompress()) == NULL)
    THROW_TJ("initializing decompressor");


  if (tjDecompressHeader3(tjInstance, jpegBuf, jpegSize, &width, &height,
                          &inSubsamp, &inColorspace) < 0)
    THROW_TJ("reading JPEG header");

  pixelFormat = TJPF_RGB;
  if ((imgBuf = (unsigned char *)tjAlloc(width * height * tjPixelSize[pixelFormat])) == NULL)
    THROW_UNIX("allocating uncompressed image buffer");

  memset(imgBuf, 'c', sizeof(unsigned char) * width * height * tjPixelSize[pixelFormat]);//清0

  if (tjDecompress2(tjInstance, jpegBuf, jpegSize, imgBuf, width, 0, height,
                    pixelFormat, flags) < 0)
    THROW_TJ("decompressing JPEG image");

  tjFree(jpegBuf);  jpegBuf = NULL;
  tjDestroy(tjInstance);  tjInstance = NULL;

  jpeg_decompress* jpeg = malloc(sizeof(jpeg_decompress));

  jpeg->image_height = height;
  jpeg->image_width = width;
  jpeg->buffer = imgBuf;

  return jpeg;

  bailout:
    tjFree(imgBuf);
    if (tjInstance) tjDestroy(tjInstance);
    tjFree(jpegBuf);
    if (jpegFile) fclose(jpegFile);
    return NULL;
}

char* getString(void *p) {
    return (char*)p;
}
void setBufferString2(void* buffer, void* str, int len) {
    // Buffer* b = buffer;
    // FIXME 向后错了四个字节,这里应该是buffer->size的
    memcpy((buffer), str, len);
}