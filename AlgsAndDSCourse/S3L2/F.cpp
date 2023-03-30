#include <cstdio>
#include <vector>
#include <algorithm>

int main()
{
	int n;

	scanf("%i", &n);

	std::vector< bool > a(n + 1, false);
	for (int i = 1; i < n + 1; i++)
	{
		if (i % 3 == 0)
		{
			a[i] = (!a[i - 1] || (i - 1 >= 0 ? !a[i - 1] : false));
		} else if (i % 3 == 1)
		{
			a[i] = (!a[i - 1] || (i - 3 >= 0 ? !a[i - 3] : false));
		} else
		{
			a[i] = (!a[i - 1] || (i - 2 >= 0 ? !a[i - 2] : false) || (i - 3 >= 0 ? !a[i - 3] : false));
		}
	}




	printf("%i", a[n] ? 1 : 2);
}
