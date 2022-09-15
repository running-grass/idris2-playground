#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <turbojpeg.h>

#include <buffer.h>


#define THROW(action, message) { \
  printf("ERROR in line %d while %s:\n%s\n", __LINE__, action, message); \
  retval = -1;  goto bailout; \
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

tjscalingfactor *scalingFactors = NULL;
int numScalingFactors = 0;


/* DCT filter example.  This produces a negative of the image. */

static int customFilter(short *coeffs, tjregion arrayRegion,
                        tjregion planeRegion, int componentIndex,
                        int transformIndex, tjtransform *transform)
{
  int i;

  for (i = 0; i < arrayRegion.w * arrayRegion.h; i++)
    coeffs[i] = -coeffs[i];

  return 0;
}





typedef struct {
  int image_width;
  int image_height;
  void* buffer;
} jpeg_decompress;

jpeg_decompress* read_JPEG_file(char *filename)
{

  tjscalingfactor scalingFactor = { 1, 1 };
  int outSubsamp = -1, outQual = -1;
  tjtransform xform;
  int flags = 0;
  int width, height;
  char *inFormat, *outFormat;
  FILE *jpegFile = NULL;
  unsigned char *imgBuf = NULL, *jpegBuf = NULL;
  int retval = 0, i, pixelFormat = TJPF_UNKNOWN;
  tjhandle tjInstance = NULL;

  if ((scalingFactors = tjGetScalingFactors(&numScalingFactors)) == NULL)
    THROW_TJ("getting scaling factors");
  memset(&xform, 0, sizeof(tjtransform));


  /* Input image is a JPEG image.  Decompress and/or transform it. */
  long size;
  int inSubsamp, inColorspace;
  int doTransform = (xform.op != TJXOP_NONE || xform.options != 0 ||
                      xform.customFilter != NULL);
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

  /* Scaling and/or a non-JPEG output image format and/or compression options
      have been selected, so we need to decompress the input/transformed
      image. */
  // width = width;
  // height = height;
  if (outSubsamp < 0)
    outSubsamp = inSubsamp;

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
    memcpy((buffer), str, len);
}