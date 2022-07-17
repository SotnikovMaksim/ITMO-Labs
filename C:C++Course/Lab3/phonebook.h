#include <string>

struct phonebook
{
	std::string name, surname, patronymic;
	long phone;
	bool operator<(const phonebook& v) const;
};