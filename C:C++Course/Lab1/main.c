#include "return_codes.h"

#include <math.h>
#include <stdio.h>
#include <stdlib.h>

const float eps = 0.0001F;
float *matrix;
int row;
int col;

void triangularForm();

void swap(int, int);

int rank(int);

int checkoutManySolutions();

float roundFloat(float);

int main(int argc, char *argv[])
{
	if (argc != 3)
	{
		printf("Error: wrong arguments count");
		return ERROR_INVALID_PARAMETER;
	}
	FILE *rf, *wf;
	rf = fopen(argv[1], "r");
	if (rf == NULL)
	{
		printf("Error: cannot open read-file");
		return ERROR_NOT_FOUND;
	}
	fscanf(rf, "%i", &row);
	col = row + 1;
	matrix = (float *)malloc(sizeof(float) * row * col);
	if (matrix == NULL)
	{
		fclose(rf);
		printf("Error: failed to allocate memory");
		return ERROR_MEMORY;
	}
	int count = 0;
	for (int i = 0; i < row && !feof(rf); i++)
	{
		for (int j = 0; j < col && !feof(rf); j++)
		{
			fscanf(rf, "%f", &matrix[i * col + j]);
			count++;
		}
	}
	if (count != row * col)
	{
		free(matrix);
		fclose(rf);
		printf("Error: wrong data count");
		return ERROR_INVALID_DATA;
	}
	wf = fopen(argv[2], "w");
	if (wf == NULL)
	{
		fclose(rf);
		free(matrix);
		printf("Error: cannot open write-file");
		return ERROR_NOT_FOUND;
	}
	triangularForm();
	int rankA = rank(0), rankT = rank(1);
	if (rankA == rankT)
	{
		int status = checkoutManySolutions();
		if (status == 0)
		{
			for (int i = 0; i < row; i++)
			{
				fprintf(wf, "%g\n", roundFloat(matrix[i * col + row] / matrix[i * col + i]));
			}
		}
		else
		{
			fprintf(wf, "many solutions");
		}
	}
	else
	{
		fprintf(wf, "no solution");
	}
	fclose(rf);
	fclose(wf);
	free(matrix);
	return ERROR_SUCCESS;
}

void triangularForm()
{
	int maxRow = -1;
	float k;
	for (int i = 0; i < row; i++)
	{
		for (int j = i + 1; j < row; j++)
		{
			if (fabsf(matrix[j * col + i]) > fabsf(matrix[i * col + i]))
			{
				maxRow = j;
			}
		}
		if (maxRow != -1)
		{
			swap(maxRow, i);
			maxRow = -1;
		}
		if (fabsf(matrix[i * col + i]) < eps)
		{
			continue;
		}
		for (int j = 0; j < row; j++)
		{
			if (i != j)
			{
				k = matrix[j * col + i] / matrix[i * col + i];
				for (int r = 0; r < col; r++)
				{
					matrix[j * col + r] -= matrix[i * col + r] * k;
				}
			}
		}
	}
}

int checkoutManySolutions()
{
	for (int i = 0; i < row; i++)
	{
		if (fabsf(matrix[i * col + i]) < eps)
		{
			return 1;	 // many solutions
		}
	}
	return 0;
}

void swap(int i, int j)
{
	float temp;
	for (int k = 0; k < col; k++)
	{
		temp = matrix[i * col + k];
		matrix[i * col + k] = matrix[j * col + k];
		matrix[j * col + k] = temp;
	}
}

int rank(int diff)
{
	int r = 0;
	for (int i = 0; i < row; i++)
	{
		for (int j = 0; j < col - diff; j++)
		{
			if (fabsf(matrix[i * col + j]) >= eps)
			{
				r++;
				break;
			}
		}
	}
	return r;
}

float roundFloat(float a)
{
	char sign = a > 0 ? 1 : -1;
	a = fabsf(a);
	float epsilon = 0.00001F;
	int low = (int)a;
	int high = low + 1;
	float diff = a - (float)low;
	if (diff - 0.5F > 0)
	{
		if ((float)high - a < epsilon)
		{
			a = (float)high;
		}
	}
	else
	{
		if (a - (float)low < epsilon)
		{
			a = (float)low;
		}
	}
	return a * sign;
}
