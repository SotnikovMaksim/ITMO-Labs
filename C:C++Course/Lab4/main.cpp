#include "LN.h"
#include "return_codes.h"

#include <cstdio>
#include <fstream>
#include <functional>
#include <iostream>
#include <map>
#include <stack>
#include <string>

std::map< std::string, std::function< LN(LN&, LN&) > > binary_operations = {
	{ "+", std::plus() },	 { "-", std::minus() },			{ "*", std::multiplies() }, { "/", std::divides() },
	{ "<", std::less() },	 { "<=", std::less_equal() },	{ "==", std::equal_to() },	{ ">=", std::greater_equal() },
	{ ">", std::greater() }, { "!=", std::not_equal_to() }, { "%", std::modulus() }
};

std::map< std::string, std::function< LN(LN) > > unary_operations = {
	{ "_", std::negate() },
	{ "~", std::bit_not() },
};

int main(int argc, char* argv[])
{
	if (argc != 3)
	{
		fprintf(stderr, "Wrong number of arguments was passed!");
		return ERROR_INVALID_PARAMETER;
	}
	std::ifstream in(argv[1]);
	if (!in.is_open())
	{
		fprintf(stderr, "Failed to open input file.");
		return ERROR_FILE_NOT_FOUND;
	}
	std::string content;
	std::stack< LN > nums;
	try
	{
		while (in >> content)
		{
			if (isdigit(content[0]) || (content[0] == '-' && content.length() > 1))
			{
				nums.push(LN(content.c_str()));
			}
			else if (content == "NaN")
			{
				nums.push(LN(Digits(), false, true));
			}
			else if (binary_operations.count(content))
			{
				LN b = nums.top();
				nums.pop();
				LN a = nums.top();
				nums.pop();
				nums.push(binary_operations.at(content)(a, b));
			}
			else if (unary_operations.count(content))
			{
				LN a = nums.top();
				nums.pop();
				nums.push(unary_operations.at(content)(a));
			}
			else
			{
				LN b = nums.top();
				nums.pop();
				LN a = nums.top();
				nums.pop();
				if (content == "+=")
				{
					a += b;
					nums.push(a);
				}
				else if (content == "-=")
				{
					a -= b;
					nums.push(a);
				}
				else if (content == "*=")
				{
					a *= b;
					nums.push(a);
				}
				else if (content == "/=")
				{
					a /= b;
					nums.push(a);
				}
			}
		}
	} catch (std::bad_alloc& e)
	{
		in.close();
		fprintf(stderr, "Failed to allocate memory!");
		return ERROR_NOT_ENOUGH_MEMORY;
	} catch (std::overflow_error& e)
	{
		in.close();
		fprintf(stderr, "Failed to cast LN to long long");
		return ERROR_UNSUPPORTED;
	} catch (...)
	{
		in.close();
		return ERROR_UNKNOWN;
	}
	in.close();
	std::ofstream out(argv[2]);
	if (!out.is_open())
	{
		fprintf(stderr, "Failed to open output file.");
		return ERROR_FILE_NOT_FOUND;
	}
	while (!nums.empty())
	{
		LN temp = nums.top();
		nums.pop();
		out << temp << std::endl;
	}
	out.close();
	return ERROR_SUCCESS;
}