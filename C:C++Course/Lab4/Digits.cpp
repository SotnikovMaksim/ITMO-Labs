#include "Digits.h"

#include <iostream>

void Digits::add(char s)
{
	if (pointer == size)
	{
		char* newData = new char[size * 2];
		for (size_t i = 0; i < pointer; i++)
		{
			newData[i] = data[i];
		}
		size *= 2;
		delete[] data;
		data = newData;
	}
	data[pointer++] = s;
}

Digits::Digits(const size_t length, char s) : Digits()
{
	for (size_t i = 0; i < length; i++)
	{
		add(s);
	}
}

char Digits::pop()
{
	// TODO: check for size reduction
	if (pointer > 0)
	{
		pointer--;
		if (pointer <= size / 4)
		{
			char* newData = new char[size / 2];
			for (size_t i = 0; i < pointer; i++)
			{
				newData[i] = data[i];
			}
			size /= 2;
			delete[] data;
			data = newData;
		}
		return data[pointer - 1];
	}

	return -1;
}

char Digits::get(size_t ind) const
{
	if (pointer > 0 && 0 <= ind && ind < pointer)
	{
		return data[ind];
	}
	return -1;
}

size_t Digits::getSize() const
{
	return size;
}

size_t Digits::getPointer() const
{
	return pointer;
}

std::ostream& operator<<(std::ostream& out, const Digits& e)
{
	for (size_t i = e.pointer - 1; i >= 0 && i < e.pointer; i--)
	{
		out << e.data[i];
	}
	return out;
}

Digits::Digits()
{
	data = new char[2];
	size = 2;
	pointer = 0;
}

Digits::Digits(const Digits& other)
{
	delete[] data;
	size = other.size;
	pointer = other.pointer;
	data = new char[size];
	for (size_t i = 0; i < pointer; i++)
	{
		data[i] = other.data[i];
	}
}

Digits& Digits::operator=(const Digits& other)
{
	if (this == &other)
	{
		return *this;
	}
	delete[] data;
	size = other.size;
	pointer = other.pointer;
	data = new char[size];
	for (size_t i = 0; i < pointer; i++)
	{
		data[i] = other.data[i];
	}
	return *this;
}
