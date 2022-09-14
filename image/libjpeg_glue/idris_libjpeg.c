#include <stdio.h>
#include <stdlib.h>
#include <setjmp.h>
#include <string.h>

#include "jpeglib.h"

JSAMPLE *image_buffer;   /* Points to large array of R,G,B-order data */
int image_height;        /* Number of rows in image */
int image_width;         /* Number of columns in image */


/*
 * Sample routine for JPEG compression.  We assume that the target file name
 * and a compression quality factor are passed in.
 */

GLOBAL(void)
write_JPEG_file(char *filename, int quality)
{

  struct jpeg_compress_struct cinfo;
  struct jpeg_error_mgr jerr;
  /* More stuff */
  FILE *outfile;                /* target file */
  JSAMPROW row_pointer[1];      /* pointer to JSAMPLE row[s] */
  int row_stride;               /* physical row width in image buffer */

  /* Step 1: allocate and initialize JPEG compression object */
  cinfo.err = jpeg_std_error(&jerr);
  /* Now we can initialize the JPEG compression object. */
  jpeg_create_compress(&cinfo);

  /* Step 2: specify data destination (eg, a file) */
  /* Note: steps 2 and 3 can be done in either order. */
  if ((outfile = fopen(filename, "wb")) == NULL) {
    fprintf(stderr, "can't open %s\n", filename);
    // exit(1);
  }
  jpeg_stdio_dest(&cinfo, outfile);

  /* Step 3: set parameters for compression */
  cinfo.image_width = image_width;      /* image width and height, in pixels */
  cinfo.image_height = image_height;
  cinfo.input_components = 3;           /* # of color components per pixel */
  cinfo.in_color_space = JCS_RGB;       /* colorspace of input image */
  /* Now use the library's routine to set default compression parameters.
   * (You must set at least cinfo.in_color_space before calling this,
   * since the defaults depend on the source color space.)
   */
  jpeg_set_defaults(&cinfo);
  /* Now you can set any non-default parameters you wish to.
   * Here we just illustrate the use of quality (quantization table) scaling:
   */
  jpeg_set_quality(&cinfo, quality, TRUE /* limit to baseline-JPEG values */);

  /* Step 4: Start compressor */
  jpeg_start_compress(&cinfo, TRUE);

  /* Step 5: while (scan lines remain to be written) */
  /*           jpeg_write_scanlines(...); */
  row_stride = image_width * 3; /* JSAMPLEs per row in image_buffer */

  while (cinfo.next_scanline < cinfo.image_height) {
    /* jpeg_write_scanlines expects an array of pointers to scanlines.
     * Here the array is only one element long, but you could pass
     * more than one scanline at a time if that's more convenient.
     */
    row_pointer[0] = &image_buffer[cinfo.next_scanline * row_stride];
    (void)jpeg_write_scanlines(&cinfo, row_pointer, 1);
  }

  /* Step 6: Finish compression */

  jpeg_finish_compress(&cinfo);
  /* After finish_compress, we can close the output file. */
  fclose(outfile);

  /* Step 7: release JPEG compression object */

  /* This is an important step since it will release a good deal of memory. */
  jpeg_destroy_compress(&cinfo);

  /* And we're done! */
}


struct my_error_mgr {
  struct jpeg_error_mgr pub;    /* "public" fields */

  jmp_buf setjmp_buffer;        /* for return to caller */
};

typedef struct my_error_mgr *my_error_ptr;

/*
 * Here's the routine that will replace the standard error_exit method:
 */

METHODDEF(void)
my_error_exit(j_common_ptr cinfo)
{
  /* cinfo->err really points to a my_error_mgr struct, so coerce pointer */
  my_error_ptr myerr = (my_error_ptr)cinfo->err;

  /* Always display the message. */
  /* We could postpone this until after returning, if we chose. */
  (*cinfo->err->output_message) (cinfo);

  /* Return control to the setjmp point */
  longjmp(myerr->setjmp_buffer, 1);
}



typedef struct {
  int image_width;
  int image_height;
  void* buffer;
} jpeg_decompress;

jpeg_decompress* do_read_JPEG_file(struct jpeg_decompress_struct *cinfo,
                                 char *filename);


jpeg_decompress* read_JPEG_file(char *filename)
{
  struct jpeg_decompress_struct cinfo;

  return do_read_JPEG_file(&cinfo, filename);
}

jpeg_decompress* do_read_JPEG_file(struct jpeg_decompress_struct *cinfo, char *filename)
{

  struct my_error_mgr jerr;
  /* More stuff */
  FILE *infile;                 /* source file */
  JSAMPARRAY buffer;            /* Output row buffer */
  int row_stride;               /* physical row width in output buffer */

  if ((infile = fopen(filename, "rb")) == NULL) {
    fprintf(stderr, "can't open %s\n", filename);
    return NULL;
  }

  /* Step 1: allocate and initialize JPEG decompression object */

  /* We set up the normal JPEG error routines, then override error_exit. */
  cinfo->err = jpeg_std_error(&jerr.pub);
  jerr.pub.error_exit = my_error_exit;
  /* Establish the setjmp return context for my_error_exit to use. */
  if (setjmp(jerr.setjmp_buffer)) {
    /* If we get here, the JPEG code has signaled an error.
     * We need to clean up the JPEG object, close the input file, and return.
     */
    jpeg_destroy_decompress(cinfo);
    fclose(infile);
    return NULL;
  }
  /* Now we can initialize the JPEG decompression object. */
  jpeg_create_decompress(cinfo);
  /* Step 2: specify data source (eg, a file) */
  jpeg_stdio_src(cinfo, infile);
  /* Step 3: read file parameters with jpeg_read_header() */

  (void)jpeg_read_header(cinfo, TRUE);
  /* Step 4: set parameters for decompression */

  /* Step 5: Start decompressor */

  (void)jpeg_start_decompress(cinfo);
  row_stride = cinfo->output_width * cinfo->output_components;

	unsigned long width = cinfo->output_width;//图像宽度
	unsigned long height = cinfo->output_height;//图像高度
	unsigned short depth = cinfo->output_components;//图像深度
  
  unsigned long imageBufferSize = width * height * depth;
  unsigned char *src_buff = malloc(sizeof(unsigned char) * imageBufferSize);//用于存取解码之后的位图数据(RGB格式)
  memset(src_buff, 5, sizeof(unsigned char) * imageBufferSize);//清0
	unsigned char *point = src_buff;

  buffer = (*cinfo->mem->alloc_sarray)
                ((j_common_ptr)cinfo, JPOOL_IMAGE, row_stride, 1);

  /* Step 6: while (scan lines remain to be read) */
  while (cinfo->output_scanline < cinfo->output_height) {
    (void)jpeg_read_scanlines(cinfo, buffer, 1);
		memcpy(point, *buffer, width*depth);  // 将buffer中的数据逐行给src_buff
		point += width * depth;      // 指针偏移一行
  }

  /* Step 7: Finish decompression */
  (void)jpeg_finish_decompress(cinfo);
  /* Step 8: Release JPEG decompression object */

  jpeg_destroy_decompress(cinfo);
  fclose(infile);

  jpeg_decompress* jpeg = malloc(sizeof(jpeg_decompress));

  jpeg->image_height = cinfo->image_height;
  jpeg->image_width = cinfo->image_width;
  jpeg->buffer = src_buff;

  return jpeg;
}


char* getString(void *p) {
    return (char*)p;
}