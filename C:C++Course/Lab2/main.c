#include "return_codes.h"
#include <math.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if defined(ZLIB)
	#include <zlib.h>
#elif defined(LIBDEFLATE)
	#include <libdeflate.h>
#elif defined(ISAL)
	#include <../include/igzip_lib.h>
#else
	#error "The library is not supported"
#endif

int convertHex(const unsigned char*, int);

const char* getType(FILE*);

int parseHeader(FILE*, char*);

int parse(FILE* inputImage, char* outName, int width, int height, int CT);

int getLength(FILE*);

int writeIn(char* outName, int width, int height, int CT, const int* filters, const unsigned char* b);

void sub(int i, int width, int** a);

void up(int, int, int**);

void average(int, int, int**);

void paeth(int i, int, int** a);

bool contains(const char*);

const unsigned char signatures[19][4] = {
	{ 'I', 'H', 'D', 'R' }, { 'I', 'D', 'A', 'T' }, { 'I', 'E', 'N', 'D' }, { 'P', 'L', 'T', 'E' },
	{ 'b', 'K', 'G', 'D' }, { 'c', 'H', 'R', 'M' }, { 'g', 'A', 'M', 'A' }, { 'h', 'I', 'S', 'T' },
	{ 'i', 'C', 'C', 'P' }, { 'i', 'T', 'X', 't' }, { 'p', 'H', 'Y', 's' }, { 's', 'B', 'I', 'T' },
	{ 's', 'P', 'L', 'T' }, { 's', 'R', 'G', 'B' }, { 's', 'T', 'E', 'R' }, { 't', 'E', 'X', 't' },
	{ 't', 'I', 'M', 'E' }, { 't', 'R', 'N', 'S' }, { 'z', 'T', 'X', 't' }
};
const char* stringSignatures[19] = {
	"header", "data", "end",  "plte", "bKGD", "cHRM", "gAMA", "hIST", "iCCP", "iTXt",
	"pHYs",	  "sBIT", "sPLT", "sRGB", "sTER", "tEXt", "tIME", "tRNS", "zTXt"
};

int main(int argc, char* argv[])
{

	unsigned char pngSignature[8] = { 137, 80, 78, 71, 13, 10, 26, 10 };
	FILE* inputImage;
	if (argc != 3)
	{
		fprintf(stderr, "Wrong arguments count\n");
		return ERROR_INVALID_PARAMETER;
	}
	inputImage = fopen(argv[1], "rb");
	if (inputImage == NULL)
	{
		fprintf(stderr, "Could not open the file\n");
		return ERROR_FILE_NOT_FOUND;
	}
	unsigned char signature[8];
	size_t br = fread(&signature, 1, 8, inputImage);
	if (br != 8)
	{
		fprintf(stderr, "Invalid file data\n");
		fclose(inputImage);
		return ERROR_INVALID_DATA;
	}
	for (int i = 0; i < 8; i++)
	{
		if (pngSignature[i] != signature[i])
		{
			fprintf(stderr, "Wrong file format\n");
			fclose(inputImage);
			return ERROR_INVALID_DATA;
		}
	}
	int codeError = parseHeader(inputImage, argv[2]);
	fclose(inputImage);
	switch (codeError)
	{
	case 1:
		fprintf(stderr, "Invalid IHDR chunk data\n");
		return ERROR_INVALID_DATA;
	case 2:
		fprintf(stderr, "Invalid data\n");
		return ERROR_INVALID_DATA;
	case 3:
		fprintf(stderr, "Failed to allocate memory\n");
		return ERROR_MEMORY;
	case 4:
		fprintf(stderr, "Failed to open write file\n");
		return ERROR_NOT_FOUND;
	case 5:
		fprintf(stderr, "Invalid filter type\n");
		return ERROR_INVALID_DATA;
	case 6:
		fprintf(stderr, "Invalid chunk type\n");
		return ERROR_INVALID_DATA;
	default:
		return ERROR_SUCCESS;
	}
}

int parseHeader(FILE* inputImage, char* outName)
{
	int imageWidth, imageHeight;
	int chunkLength = getLength(inputImage);
	if (chunkLength != 13)
	{
		return 1;
	}
	const char* chunkType = getType(inputImage);
	if (chunkType == NULL)
	{
		return 3;
	}
	if (strcmp(chunkType, "header") != 0)
	{
		return 1;
	}
	unsigned char width[4], height[4];
	unsigned char depth[1], colorType[1], deflateMethod[1], filterMethod[1], interlace[1];
	size_t br;

	br = fread(&width, 1, 4, inputImage);
	if (br != 4)
	{
		return 1;
	}
	br = fread(&height, 1, 4, inputImage);
	if (br != 4)
	{
		return 1;
	}
	br = fread(&depth, 1, 1, inputImage);
	if (br != 1)
	{
		return 1;
	}
	br = fread(&colorType, 1, 1, inputImage);
	if (br != 1)
	{
		return 1;
	}
	br = fread(&deflateMethod, 1, 1, inputImage);
	if (br != 1)
	{
		return 1;
	}
	br = fread(&filterMethod, 1, 1, inputImage);
	if (br != 1)
	{
		return 1;
	}
	br = fread(&interlace, 1, 1, inputImage);
	if (br != 1)
	{
		return 1;
	}

	imageWidth = convertHex(&width[0], 4);
	imageHeight = convertHex(&height[0], 4);
	int imageDepth = convertHex(&depth[0], 1);
	int imageColorType = convertHex(&colorType[0], 1);
	int imageDeflate = convertHex(&deflateMethod[0], 1);
	int imageFilter = convertHex(&filterMethod[0], 1);
	int imageInterlace = convertHex(&interlace[0], 1);

	if ((imageDepth != 8) || (imageColorType != 2 && imageColorType != 0) || (imageDeflate != 0) ||
		(imageFilter != 0) || (imageInterlace != 0))
	{
		return 1;
	}

	if (fseek(inputImage, 4, SEEK_CUR) != 0)
	{
		return 1;
	}

	int codeError = parse(inputImage, outName, imageWidth, imageHeight, imageColorType);
	switch (codeError)
	{
	case 1:
		return 2;
	case 2:
		return 3;
	case 3:
		return 4;
	case 4:
		return 5;
	case 5:
		return 6;
	default:
		return 0;
	}
}

int parse(FILE* inputImage, char* outName, int width, int height, const int CT)
{
	size_t size = 25, br;
	int pos = 0;
	unsigned char* data = malloc(sizeof(unsigned char) * size);
	if (data == NULL)
	{
		return 2;
	}
	size_t dataSize = 0;
	while (!feof(inputImage))
	{
		const int length = getLength(inputImage);
		const char* type = getType(inputImage);
		if (type == NULL)
		{
			return 2;
		}
		if (strcmp(type, "end") == 0)
		{
			unsigned char bin[5];
			br = fread(&bin, 1, length, inputImage);
			size_t t = fread(&bin, 1, 5, inputImage);
			if ((br != 0) || (t != 4))
			{
				free(data);
				return 1;
			}
			break;
		}
		else if (strcmp(type, "data") == 0)
		{
			dataSize += length;
			while (pos + length >= size)
			{
				size *= 2;
				unsigned char* p = realloc(data, size);
				if (p)
				{
					data = p;
				}
				else
				{
					free(data);
					free(p);
					return 2;
				}
			}
			for (int i = 0; i < length; i++)
			{
				if (feof(inputImage))
				{
					free(data);
					return 1;
				}
				else
				{
					fread(&data[pos++], 1, 1, inputImage);
				}
			}
			if (fseek(inputImage, 4, SEEK_CUR) != 0)
			{
				free(data);
				return 1;
			}
		}
		else
		{
			if ((!contains(type)) || (strcmp("-1", type) == 0))
			{
				free(data);
				return 5;
			}
			if (fseek(inputImage, length + 4, SEEK_CUR) != 0)
			{
				free(data);
				return 1;
			}
		}
	}
	size_t s = (width * height * (CT + 1) + height);
	unsigned char* decompressed = malloc(sizeof(unsigned char) * s);
	if (decompressed == NULL)
	{
		free(data);
		return 2;
	}
#if defined(ZLIB)
	int codeError = uncompress(decompressed, &s, data, size);
	if (codeError != Z_OK)
	{
		free(data);
		free(decompressed);
		return 1;
	}
#elif defined(LIBDEFLATE)
	int codeError = libdeflate_zlib_decompress(libdeflate_alloc_decompressor(), data, dataSize, decompressed, s, &s);
	if (codeError != LIBDEFLATE_SUCCESS)
	{
		free(data);
		free(decompressed);
		return 1;
	}
#elif defined(ISAL)
	struct inflate_state state;
	isal_inflate_init(&state);
	state.avail_in = dataSize;
	state.next_in = data;
	state.next_out = decompressed;
	state.avail_out = s;
	state.crc_flag = ISAL_ZLIB;
	int codeError = isal_inflate_stateless(&state);
	if (codeError != COMP_OK)
	{
		free(data);
		free(decompressed);
		return 1;
	}
#endif
	free(data);
	int* filters = malloc(sizeof(int) * height);
	if (filters == NULL)
	{
		return 2;
	}
	int pointer = 0;
	for (int i = 0; i < height; i++)
	{
		filters[i] = decompressed[pointer];
		pointer += (width * (CT + 1)) + 1;
	}
	codeError = writeIn(outName, width, height, CT, &filters[0], decompressed);
	free(decompressed);
	free(filters);
	switch (codeError)
	{
	case 1:
		return 4;
	case 2:
		return 2;
	case 3:
		return 3;
	default:
		return 0;
	}
}

void fMemory(int** a, size_t length)
{
	for (int i = 0; i < length; i++)
	{
		free(a[i]);
	}
	free(a);
}

int writeIn(char* outName, int width, int height, int CT, const int* filters, const unsigned char* b)
{
	if (CT == 0)
	{
		int** scale;
		scale = malloc(sizeof(int*) * height);
		if (scale == NULL)
		{
			return 2;
		}
		for (int i = 0; i < height; i++)
		{
			scale[i] = malloc(sizeof(int) * width);
			if (scale[i] == NULL)
			{
				fMemory(scale, i);
				return 2;
			}
		}
		for (int i = 0; i < height; i++)
		{
			for (int j = 0; j < width; j++)
			{
				scale[i][j] = b[i * (width + 1) + 1 + j];
			}
		}
		for (int i = 0; i < height; i++)
		{
			switch (filters[i])
			{
			case (0):
				break;
			case (1):
				sub(i, width, scale);
				break;
			case (2):
				up(i, width, scale);
				break;
			case (3):
				average(i, width, scale);
				break;
			case (4):
				paeth(i, width, scale);
				break;
			default:
				fMemory(scale, height);
				return 1;
			}
		}
		FILE* outputImage = fopen(outName, "wb");
		fprintf(outputImage, "P5\n%i %i\n255\n", width, height);
		if (outputImage == NULL)
		{
			return 3;
		}
		for (int i = 0; i < height; i++)
		{
			for (int j = 0; j < width; j++)
			{
				fwrite(&scale[i][j], 1, 1, outputImage);
			}
		}
		fMemory(scale, height);
		fclose(outputImage);
	}
	else if (CT == 2)
	{
		int **red, **green, **blue;
		red = malloc(sizeof(int*) * height);
		green = malloc(sizeof(int*) * height);
		blue = malloc(sizeof(int*) * height);
		if (red == NULL || green == NULL || blue == NULL)
		{
			return 2;
		}
		for (int i = 0; i < height; i++)
		{
			red[i] = malloc(sizeof(int) * width);
			green[i] = malloc(sizeof(int) * width);
			blue[i] = malloc(sizeof(int) * width);
			if (red[i] == NULL || green[i] == NULL || blue[i] == NULL)
			{
				if (red[i] == NULL)
				{
					fMemory(red, i);
				}
				if (green[i] == NULL)
				{
					fMemory(green, i);
				}
				if (blue[i] == NULL)
				{
					fMemory(blue, i);
				}
				return 2;
			}
		}
		for (int i = 0; i < height; i++)
		{
			for (int j = 0; j < width; j++)
			{
				red[i][j] = b[i * (width * 3 + 1) + 1 + (j * 3)];
				green[i][j] = b[i * (width * 3 + 1) + 1 + (j * 3) + 1];
				blue[i][j] = b[i * (width * 3 + 1) + 1 + (j * 3) + 2];
			}
		}
		for (int i = 0; i < height; i++)
		{
			switch (filters[i])
			{
			case (0):
				break;
			case (1):
				sub(i, width, red);
				sub(i, width, green);
				sub(i, width, blue);
				break;
			case (2):
				up(i, width, red);
				up(i, width, green);
				up(i, width, blue);
				break;
			case (3):
				average(i, width, red);
				average(i, width, green);
				average(i, width, blue);
				break;
			case (4):
				paeth(i, width, red);
				paeth(i, width, green);
				paeth(i, width, blue);
				break;
			default:
				fMemory(red, height);
				fMemory(green, height);
				fMemory(blue, height);
				return 1;
			}
		}
		FILE* outputImage = fopen(outName, "wb");
		fprintf(outputImage, "P6\n%i %i\n255\n", width, height);
		if (outputImage == NULL)
		{
			return 3;
		}
		for (int i = 0; i < height; i++)
		{
			for (int j = 0; j < width; j++)
			{
				fwrite(&red[i][j], 1, 1, outputImage);
				fwrite(&green[i][j], 1, 1, outputImage);
				fwrite(&blue[i][j], 1, 1, outputImage);
			}
		}
		fMemory(red, height);
		fMemory(green, height);
		fMemory(blue, height);
		fclose(outputImage);
	}
	return 0;
}

int getUpper(int** a, int i, int j)
{
	if (i > 0)
	{
		return a[i - 1][j];
	}
	else
	{
		return 0;
	}
}

int getUpperLeft(int** a, int i, int j)
{
	if (i > 0 && j > 0)
	{
		return a[i - 1][j - 1];
	}
	else
	{
		return 0;
	}
}

int getLeft(int** a, int i, int j)
{
	if (j > 0)
	{
		return a[i][j - 1];
	}
	else
	{
		return 0;
	}
}

int paethPredictor(int a, int b, int c)
{
	int res;
	int p = a + b - c;
	int pa = abs(p - a);
	int pb = abs(p - b);
	int pc = abs(p - c);
	if (pa <= pb && pa <= pc)
	{
		res = a;
	}
	else if (pb <= pc)
	{
		res = b;
	}
	else
	{
		res = c;
	}
	return res;
}

void sub(int i, int width, int** a)
{
	for (int j = 0; j < width; j++)
	{
		a[i][j] = (unsigned char)(a[i][j] + getLeft(a, i, j)) % 256;
	}
}

void up(int i, int width, int** a)
{
	for (int j = 0; j < width; j++)
	{
		a[i][j] = (unsigned char)(a[i][j] + getUpper(a, i, j)) % 256;
	}
}

void average(int i, int width, int** a)
{
	for (int j = 0; j < width; j++)
	{
		a[i][j] = (unsigned char)(a[i][j] + (int)floor((getLeft(a, i, j) + getUpper(a, i, j) + 0.0) / 2)) % 256;
	}
}

void paeth(int i, int width, int** a)
{
	for (int j = 0; j < width; j++)
	{
		a[i][j] = (unsigned char)(a[i][j] + paethPredictor(getLeft(a, i, j), getUpper(a, i, j), getUpperLeft(a, i, j))) % 256;
	}
}

int convertHex(const unsigned char* hex, int len)
{
	int sum = 0;
	for (int i = len - 1, c = 0; i >= 0; i--, c++)
	{
		sum += pow(16, c * 2) * (unsigned int)hex[i];
	}
	return sum;
}

bool contains(const char* target)
{
	for (int i = 0; i < 19; i++)
	{
		if (strcmp(target, stringSignatures[i]) == 0)
		{
			return true;
		}
	}
	return false;
}

int getLength(FILE* inputImage)
{
	unsigned char length[4];
	size_t br = fread(&length, 1, 4, inputImage);
	if (br != 4)
	{
		return -1;
	}
	return convertHex(&length[0], 4);
}

const char* getType(FILE* im)
{
	unsigned char type[4];
	size_t br = fread(&type, 1, 4, im);
	if (br != 4)
	{
		return "-1";
	}
	for (int i = 0; i < 19; i++)
	{
		bool flag = true;
		for (int j = 0; j < 4; j++)
		{
			if (type[j] != signatures[i][j])
			{
				flag = false;
				break;
			}
		}
		if (flag)
		{
			return stringSignatures[i];
		}
	}
	return "None";
}