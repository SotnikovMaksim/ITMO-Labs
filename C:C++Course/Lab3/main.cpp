#include "phonebook.h"
#include "quicksort.h"
#include "return_codes.h"
#include <cstdio>
#include <fstream>
#include <iostream>

int phonebookReadSortWrite(std::ifstream &, size_t &, bool, char *);

template< typename T >
int readSortWrite(std::ifstream &input, size_t length, bool descending, char *outputName)
{
	T *data = (T *)malloc(sizeof(T) * length);
	if (!data)
	{
		input.close();
		fprintf(stderr, "Failed tp allocate memory for data\n");
		return ERROR_NOT_ENOUGH_MEMORY;
	}
	for (int i = 0; i < length; i++)
	{
		input >> data[i];
	}

	std::string bin;
	if (input >> bin)
	{
		input.close();
        free(data);
		fprintf(stderr, "More data was received than was stated\n");
		return ERROR_INVALID_DATA;
	}
	input.close();

	if (descending)
	{
		quicksort< T, true >(0, length - 1, data);
	}
	else
	{
		quicksort< T, false >(0, length - 1, data);
	}
	std::ofstream output(outputName, std::ios::out);
	if (!output.is_open())
	{
        free(data);
		fprintf(stderr, "Failed to open output file.\n");
		return ERROR_NOT_FOUND;
	}
	for (int i = 0; i < length; i++)
	{
		output << data[i] << std::endl;
	}
	free(data);
	output.close();
	return ERROR_SUCCESS;
}

int main(int argc, char *argv[])
{
	if (argc != 3)
	{
		fprintf(stderr, "Wrong number of arguments was passed.\n");
		return ERROR_INVALID_PARAMETER;
	}
	std::ifstream input(argv[1]);
	if (!input.is_open())
	{
		fprintf(stderr, "Failed to open input file.\n");
		return ERROR_FILE_NOT_FOUND;
	}
	std::string type, order;
	size_t length;
	input >> type;
	input >> order;
	input >> length;
	bool descending;
	if (input.eof() && !(length == 0))
	{
		input.close();
		fprintf(stderr, "Input file has invalid data\n");
		return ERROR_INVALID_DATA;
	}
	if (length == 0)
	{
		FILE *out = fopen(argv[2], "w");
		fclose(out);
		return ERROR_SUCCESS;
	}
	descending = (bool)(order == "descending");
	if (type == "int")
	{
		return readSortWrite< int >(input, length, descending, argv[2]);
	}
	else if (type == "float")
	{
		return readSortWrite< float >(input, length, descending, argv[2]);
	}
	else if (type == "phonebook")
	{
		return phonebookReadSortWrite(input, length, descending, argv[2]);
	}
	else
	{
		input.close();
		fprintf(stderr, "THis type of data does not support");
		return ERROR_NOT_IMPLEMENTED;
	}
}

int phonebookReadSortWrite(std::ifstream &input, size_t &length, bool descending, char *outputName)
{
	auto *data = (phonebook *)malloc(sizeof(phonebook) * length);
	if (!data)
	{
		input.close();
		fprintf(stderr, "Failed tp allocate memory for data\n");
		return ERROR_NOT_ENOUGH_MEMORY;
	}
	for (int i = 0; i < length; i++)
	{
		input >> data[i].surname;
		input >> data[i].name;
		input >> data[i].patronymic;
		input >> data[i].phone;
	}

	std::string bin;
	if (input >> bin)
	{
		input.close();
		free(data);
		fprintf(stderr, "More data was received than was stated\n");
		return ERROR_INVALID_DATA;
	}
	input.close();

	if (descending)
	{
		quicksort< phonebook, true >(0, length - 1, data);
	}
	else
	{
		quicksort< phonebook, false >(0, length - 1, data);
	}

	std::ofstream output(outputName, std::ios::out);
	if (!output.is_open())
	{
		free(data);
		fprintf(stderr, "Failed to open output file.\n");
		return ERROR_NOT_FOUND;
	}

	for (int i = 0; i < length; i++)
	{
		output << data[i].surname << " " << data[i].name << " " << data[i].patronymic << " " << data[i].phone << std::endl;
	}
	free(data);
	output.close();
	return ERROR_SUCCESS;
}