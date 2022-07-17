#include "phonebook.h"

bool phonebook::operator<(const phonebook& v) const
{
	if (surname == v.surname)
	{
		if (name == v.name)
		{
			if (patronymic == v.patronymic)
			{
				if (phone == v.phone)
				{
					return false;
				}
				else
				{
					return (phone < v.phone);
				}
			}
			else
			{
				return (patronymic < v.patronymic);
			}
		}
		else
		{
			return (name < v.name);
		}
	}
	else
	{
		return (surname < v.surname);
	}
}