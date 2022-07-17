#pragma once

#include <cstdio>
#include <iostream>

class Digits
{
	char* data;
	size_t pointer;
	size_t size;

  public:
	explicit Digits();

	Digits(const Digits& other);

	explicit Digits(size_t length, char s);

	~Digits() { delete[] data; }

	size_t getPointer() const;

	size_t getSize() const;

	void set(size_t ind, char s) { data[ind] = s; }

	void add(char s);

	char pop();

	char get(size_t ind) const;

	Digits& operator=(const Digits& other);

	friend std::ostream& operator<<(std::ostream& out, const Digits& point);
};