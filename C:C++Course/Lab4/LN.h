#pragma once
#include "Digits.h"
#include <string_view>

#include <iostream>
#include <string>

class LN
{
	mutable Digits number;
	mutable bool nan;
	mutable bool negate;

  public:
	LN(long long input_number = 0);

	LN(const char* input_number);

	LN(Digits number, bool negate, bool nan);

	LN(std::string_view input_number);

	LN(const LN& other);

	LN(LN&& other);

	void RLZ();

	size_t getPointer() const { return number.getPointer(); }

	char getDigit(size_t ind) const { return number.get(ind); }

	bool isZero() const { return (getPointer() == 1 && getDigit(0) == '0'); }

	LN& operator=(const LN& other);

	LN& operator=(LN&& other);

	LN operator+(const LN& other) const;

	LN operator-(const LN& other) const;

	LN operator*(const LN& other) const;

	LN operator/(const LN& other) const;

	LN operator%(const LN& other) const;

	LN operator-();

	LN operator~();

	explicit operator bool() const;

	explicit operator long long() const;

	void operator+=(const LN& other) { *this = *this + other; }

	void operator-=(const LN& other) { *this = *this - other; }

	void operator*=(const LN& other) { *this = *this * other; }

	void operator/=(const LN& other) { *this = *this / other; }

	bool operator<(const LN& other) const;

	bool operator^(const LN& other) const;

	bool operator==(const LN& other) const;

	friend std::ostream& operator<<(std::ostream& out, const LN& point);

	bool operator<=(const LN& other) const { return !nan && !other.nan && ((*this < other) || (*this == other)); }

	bool operator>=(const LN& other) const { return !nan && !other.nan && ((*this > other) || (*this == other)); }

	bool operator!=(const LN& other) const { return !(*this == other) || nan || other.nan; }

	bool operator>(const LN& other) const { return !nan && !other.nan && !(*this <= other); }
};

inline LN operator"" _ln(const char* input_number)
{
	return LN(input_number);
}
