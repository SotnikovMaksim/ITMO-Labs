#include "LN.h"

#include "Digits.h"
#include <string_view>

#include <cstring>
#include <iostream>
#include <stack>

#define MIN (-9223372036854775807)
#define MAX 9223372036854775807
#define NAN LN(Digits(), false, true)

LN::LN(long long input_number) : number(Digits()), nan(false), negate(input_number < 0)
{
	do
	{
		number.add((char)(input_number % 10 + '0'));
		input_number /= 10;
	} while (input_number != 0);
	RLZ();
}

LN::LN(const char* input_number) : number(Digits()), nan(false), negate(input_number[0] == '-')
{
	size_t s = std::strlen(input_number);
	for (size_t i = s - 1; i >= (negate ? 1 : 0) && i < s; i--)
	{
		int t = input_number[i] - '0';
		char charT = (char)(t + '0');
		number.add(charT);
	}
	RLZ();
}

LN::LN(const Digits number, bool negate, bool nan)
{
	this->number = number;
	this->negate = negate;
	this->nan = nan;
}

LN::LN(std::string_view input_number) : LN(input_number.data())
{
	RLZ();
}

LN::LN(const LN& other)
{
	number = other.number;
	negate = other.negate;
	nan = other.nan;
	RLZ();
}

LN::LN(LN&& other)
{
	number = other.number;
	negate = other.negate;
	nan = other.nan;
	other.nan = true;
	RLZ();
}

LN::operator long long() const
{
	if (LN(MIN) <= *this && *this <= LN(MAX))
	{
		long long ans = 0;
		long long step = 1;
		for (size_t i = 0; i < getPointer(); i++)
		{
			ans += step * getDigit(i) * (negate ? -1 : 1);
			step *= 10;
		}
		return ans;
	}
	throw std::overflow_error("Failed to cast LN to long long");
}

LN::operator bool() const
{
	return !isZero();
}

void LN::RLZ()
{
	size_t ind = getPointer() - 1;
	while (ind > 0 && getDigit(ind) == '0')
	{
		number.pop();
		ind--;
	}
}

LN& LN::operator=(const LN& other)
{
	if (this == &other)
	{
		return *this;
	}
	number = other.number;
	negate = other.negate;
	nan = other.nan;
	return *this;
}

LN& LN::operator=(LN&& other)
{
	if (this == &other)
	{
		return *this;
	}
	number = other.number;
	negate = other.negate;
	nan = other.nan;
	other.nan = true;
	return *this;
}

LN LN::operator+(const LN& other) const
{
	if (nan || other.nan)
	{
		return NAN;
	}
	if (negate)
	{
		if (other.negate)
		{
			negate = false;
			other.negate = false;
			LN temp = *this + other;
			temp.negate = true;
			return temp;
		}
		negate = false;
		return other - *this;
	}
	else if (other.negate)
	{
		other.negate = false;
		return *this - other;
	}

	size_t size = std::max(getPointer(), other.getPointer());
	int ost = 0;
	Digits ans = Digits();
	for (size_t i = 0; i < size; i++)
	{
		int sum = ost + (i < getPointer() ? getDigit(i) - '0' : 0) + (i < other.getPointer() ? other.getDigit(i) - '0' : 0);
		ans.add((char)(sum % 10 + '0'));
		ost = sum > 9;
	}
	if (ost != 0)
	{
		ans.add((char)(ost + '0'));
	}
	return LN(ans, false, false);
}

LN LN::operator-(const LN& other) const
{
	if (nan || other.nan)
	{
		return NAN;
	}
	if (other.negate)
	{
		other.negate = false;
		return *this + other;
	}
	if (negate)
	{
		negate = false;
		LN temp = *this + other;
		temp.negate = !temp.negate;
		return temp;
	}
	if (*this < other)
	{
		LN temp = other - *this;
		temp.negate = true;
		return temp;
	}
	int ost = 0;
	Digits ans = Digits();
	for (size_t i = 0; i < getPointer() || ost != 0; ++i)
	{
		int sub = (getDigit(i) - '0') - ost - (i < other.getPointer() ? other.getDigit(i) - '0' : 0);
		ost = sub < 0;
		if (ost)
		{
			sub += 10;
		}
		ans.add((char)(sub + '0'));
	}
	return LN(ans, false, false);
}

LN LN::operator*(const LN& other) const
{
	if (nan || other.nan)
	{
		return NAN;
	}
	if (isZero() || other.isZero())
	{
		return LN();
	}
	size_t size = getPointer() + other.getPointer();
	Digits res = Digits(size, '0');
	for (size_t i = 0; i < getPointer(); i++)
	{
		int ost = 0;
		for (size_t j = 0; j < other.getPointer() || ost != 0; j++)
		{
			int mul = (res.get(i + j) - '0') + (getDigit(i) - '0') * (j < other.getPointer() ? other.getDigit(j) - '0' : 0) + ost;
			res.set(i + j, (char)(mul % 10 + '0'));
			ost = mul / 10;
		}
	}
	LN final = LN(res, negate != other.negate, false);
	final.RLZ();
	return final;
}

LN LN::operator/(const LN& other) const
{
	if (other.isZero() || nan || other.nan)
	{
		return NAN;
	}
	if (*this ^ other)
	{
		return LN();
	}

	size_t size = getPointer() - other.getPointer() + 1;
	bool memThis = negate, memOther = other.negate;
	negate = false;
	other.negate = false;
	LN res = LN(Digits(size, '0'), false, false);
	for (size_t i = size - 1; i >= 0 && i < size; i--)
	{
		while (other * res <= *this)
		{
			res.number.set(i, (char)(((res.getDigit(i) - '0') + 1) + '0'));
		}
		res.number.set(i, (char)(((res.getDigit(i) - '0') - 1) + '0'));
	}
	negate = memThis;
	other.negate = memOther;
	res.negate = negate != other.negate;
	res.RLZ();
	return res;
}

LN LN::operator~()
{
	if (isZero())
	{
		return LN();
	}
	else if (nan || negate)
	{
		return NAN;
	}
	size_t size = getPointer();
	LN res = LN(Digits(size, '0'), false, false);
	for (size_t i = size - 1; i >= 0 && i < size; i--)
	{
		while (res * res <= *this)
		{
			res.number.set(i, (char)(((res.getDigit(i) - '0') + 1) + '0'));
		}
		res.number.set(i, (char)(((res.getDigit(i) - '0') - 1) + '0'));
	}
	res.RLZ();
	return res;
}

bool LN::operator<(const LN& other) const
{
	if (nan || other.nan)
	{
		return false;
	}
	if (getPointer() < other.getPointer())
	{
		if (other.negate)
		{
			return false;
		}
		return true;
	}
	if (getPointer() > other.getPointer())
	{
		if (negate)
		{
			return true;
		}
		return false;
	}
	if (negate != other.negate)
	{
		return negate;
	}
	for (size_t i = getPointer() - 1; i >= 0 && i < getPointer(); i--)
	{
		if (getDigit(i) < other.getDigit(i))
		{
			return !(negate && other.negate);
		}
		else if (getDigit(i) > other.getDigit(i))
		{
			return (negate && other.negate);
		}
	}
	return false;
}

LN LN::operator%(const LN& other) const
{
	LN div = *this / other;
	LN mul = div * other;
	return *this - mul;
}

bool LN::operator^(const LN& other) const
{
	if (nan || other.nan || (getPointer() < other.getPointer()))
	{
		return true;
	}
	else if (getPointer() > other.getPointer())
	{
		return false;
	}
	for (size_t i = getPointer() - 1; i >= 0 && i < getPointer(); i--)
	{
		if (getDigit(i) < other.getDigit(i))
		{
			return true;
		}
		else if (getDigit(i) > other.getDigit(i))
		{
			return false;
		}
	}
	return false;
}

LN LN::operator-()
{
	negate = !negate;
	return *this;
}

//auto compare(const LN& other) const
//{
//	return (-1) * (*this < other) + (*this > other);
//}

bool LN::operator==(const LN& other) const
{
	if (nan || other.nan || (getPointer() != other.getPointer()) || ((negate != other.negate) && !(isZero() && other.isZero())))
	{
		return false;
	}
	for (size_t i = 0; i < getPointer(); i++)
	{
		if (getDigit(i) != other.getDigit(i))
		{
			return false;
		}
	}
	return true;
}

std::ostream& operator<<(std::ostream& out, const LN& e)
{
	if (e.nan)
	{
		out << "NaN";
		return out;
	}
	if (e.negate && !e.isZero())
	{
		out << "-";
	}
	out << e.number;
	return out;
}
